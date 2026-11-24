;;; agent-dashboard.el --- Overview of AI agents and work clocked -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: agents, org, clock, dashboard

;;; Commentary:

;; A single buffer giving a live overview of:
;;
;; - All AI agent-shell sessions, grouped by status (waiting / working /
;;   ready / killed), with the agents that need a reply floated to the top.
;; - Time clocked and money earned today / this week / this month, plus a
;;   per-client breakdown and the currently running clocks.
;; - The open work-todo backlog grouped by client.
;;
;; Rendering uses the org-ui block system (agent-dashboard/org-ui/ui.el):
;; the buffer is org-mode, and every action is an org-ui clickable block
;; ({{ {:on-click HANDLER ...} label }}) activated with RET.  Because org-ui
;; attaches the action via a text-property keymap, RET works regardless of
;; evil state.
;;
;; Data is read defensively: each external dependency (agent-shell,
;; agent-shell-manager, org-clock-multi, org-clock-data) is probed with
;; `fboundp'/`ignore-errors', so the dashboard renders even when a dependency
;; is missing -- the corresponding section just shows a placeholder.
;;
;; Entry point: M-x `agent-dashboard'.
;;
;; Deferred to a later iteration (see task.md): event-driven instant refresh
;; via `agent-shell-subscribe-to', inline transcript-tail preview, restart/kill
;; and set-model/-mode actions, launching an agent straight from the backlog.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'map)

;;; Customization

(defgroup agent-dashboard nil
  "Overview of AI agents and work clocked."
  :group 'tools
  :prefix "agent-dashboard-")

(defcustom agent-dashboard-buffer-name "*Agent Dashboard*"
  "Name of the dashboard buffer."
  :type 'string)

(defcustom agent-dashboard-refresh-interval 2
  "Seconds between background refresh checks.
A refresh only redraws the buffer when the underlying state changed
\(see `agent-dashboard--signature'), so a short interval is cheap."
  :type 'number)

(defcustom agent-dashboard-workday-minutes 480
  "Length of a target workday in minutes, used for the today progress bar."
  :type 'integer)

(defcustom agent-dashboard-backlog-max 5
  "Maximum number of backlog todos to show per client."
  :type 'integer)

(defcustom agent-dashboard-width 78
  "Width of the horizontal rules drawn between sections."
  :type 'integer)

(defcustom agent-dashboard-net-threshold 7200
  "Gross-earnings threshold for the month net calculation.
Earnings up to this value are scaled linearly into the 0..`agent-dashboard-net-base'
range; earnings above it are added at 1/`agent-dashboard-net-excess-divisor'."
  :type 'number)

(defcustom agent-dashboard-net-base 4000
  "Net value the first `agent-dashboard-net-threshold' of gross earnings maps to."
  :type 'number)

(defcustom agent-dashboard-net-excess-divisor 2
  "Divisor applied to gross earnings above `agent-dashboard-net-threshold'."
  :type 'number)

;;; Faces

(defface agent-dashboard-header-face '((t :inherit bold :height 1.1))
  "Face for the dashboard title.")
(defface agent-dashboard-section-face '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section headers.")
(defface agent-dashboard-faded-face '((t :inherit shadow))
  "Face for secondary / dimmed text.")
(defface agent-dashboard-slug-face '((t :inherit default))
  "Face for ticket slugs and client names.")
(defface agent-dashboard-money-face '((t :inherit success))
  "Face for money amounts.")
(defface agent-dashboard-waiting-face '((t :inherit error :weight bold))
  "Face for agents waiting on the user.")
(defface agent-dashboard-working-face '((t :inherit warning))
  "Face for working agents and progress bars.")
(defface agent-dashboard-ready-face '((t :inherit success))
  "Face for ready agents.")
(defface agent-dashboard-idle-face '((t :inherit shadow))
  "Face for idle / unknown agents.")
(defface agent-dashboard-killed-face '((t :inherit shadow :strike-through t))
  "Face for killed agents.")

;;; State

(defvar agent-dashboard--timer nil
  "Background refresh timer, or nil.")

(defvar-local agent-dashboard--signature nil
  "Last rendered state signature for this buffer.")

(defvar agent-dashboard--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory this file lives in, used to locate org-ui.")

;;; org-ui loading

(defun agent-dashboard--ensure-ui ()
  "Load the bundled org-ui (ui.el) if it is not already available."
  (unless (fboundp 'ui/replace-button-syntax)
    (let ((f (expand-file-name "org-ui/ui.el" agent-dashboard--dir)))
      (if (file-exists-p f)
          (load f nil t)
        (error "Cannot find org-ui at %s" f)))))

;;; Small helpers

(defun agent-dashboard--p (s face)
  "Propertize S with FACE via `font-lock-face' so it survives refontification."
  (propertize s 'font-lock-face face))

(defun agent-dashboard--rule ()
  "Return a horizontal rule line."
  (agent-dashboard--p (make-string agent-dashboard-width ?─)
                      'agent-dashboard-faded-face))

(defun agent-dashboard--pad (s n)
  "Pad or truncate S to width N."
  (truncate-string-to-width (or s "") n 0 ?\s))

(defun agent-dashboard--fmt-mins (m)
  "Format M minutes as H:MM."
  (let ((m (or m 0)))
    (format "%d:%02d" (/ m 60) (% m 60))))

(defun agent-dashboard--money (e)
  "Format earnings E with the configured currency symbol."
  (format "%s%.2f"
          (if (boundp 'org-clock-data-currency-symbol)
              org-clock-data-currency-symbol "€")
          (or e 0)))

(defun agent-dashboard--month-net (earnings)
  "Return the net value of gross EARNINGS for the month.
The first `agent-dashboard-net-threshold' of gross is scaled linearly to
`agent-dashboard-net-base'; anything above the threshold is added at
1/`agent-dashboard-net-excess-divisor'."
  (let ((thr agent-dashboard-net-threshold)
        (base agent-dashboard-net-base)
        (div agent-dashboard-net-excess-divisor)
        (e (or earnings 0)))
    (if (<= e thr)
        (if (> thr 0) (* e (/ (float base) thr)) 0.0)
      (+ base (/ (- e thr) (float div))))))

(defun agent-dashboard--euro-format (n)
  "Format number N European-style: \".\" thousands separator, \",\" decimals.
E.g. 5740.75 -> \"5.740,75\"."
  (pcase-let* ((s (format "%.2f" (abs (or n 0))))
               (`(,int ,dec) (split-string s "\\."))
               (grouped "")
               (cnt 0))
    (dolist (ch (reverse (string-to-list int)))
      (when (= cnt 3)
        (setq grouped (concat "." grouped) cnt 0))
      (setq grouped (concat (char-to-string ch) grouped) cnt (1+ cnt)))
    (concat (if (and n (< n 0)) "-" "") grouped "," dec)))

(defun agent-dashboard--kfmt (n)
  "Format integer N compactly with k/m suffixes."
  (cond ((null n) "")
        ((>= n 1000000) (format "%.1fm" (/ n 1000000.0)))
        ((>= n 1000) (format "%dk" (round (/ n 1000.0))))
        (t (format "%d" n))))

(defun agent-dashboard--bar (val max width)
  "Return a [████░░] progress bar of WIDTH for VAL against MAX."
  (let* ((ratio (if (and max (> max 0)) (min 1.0 (/ (float (or val 0)) max)) 0))
         (filled (round (* width ratio)))
         (pct (round (* 100 ratio))))
    (concat "["
            (agent-dashboard--p (make-string filled ?█) 'agent-dashboard-working-face)
            (make-string (max 0 (- width filled)) ?░)
            "] " (format "%d%%" pct))))

(defun agent-dashboard--button (label handler &rest opts)
  "Return org-ui clickable-block text for LABEL calling HANDLER (a symbol).
OPTS is a plist of extra readable options (e.g. :key \"x\" :file \"y\")
stored in the block and available to HANDLER via the block plist.
LABEL must not contain a closing brace."
  (let ((opt-str (mapconcat (lambda (kv) (format "%s %S" (car kv) (cadr kv)))
                            (seq-partition opts 2) " ")))
    (format "{{ {:on-click %s%s} %s }}"
            handler
            (if (string-empty-p opt-str) "" (concat " " opt-str))
            label)))

(defun agent-dashboard--opt (block k)
  "Read option K from org-ui BLOCK plist."
  (plist-get (plist-get block :opts) k))

(defun agent-dashboard--block-buffer (block)
  "Resolve the agent buffer referenced by BLOCK's :key option."
  (let ((k (agent-dashboard--opt block :key)))
    (and k (get-buffer k))))

;;; Data: agents

(defun agent-dashboard--norm-status (s)
  "Normalize a raw agent status symbol S to one of the dashboard groups."
  (pcase s
    ((or 'waiting 'blocked) 'waiting)
    ((or 'working 'busy) 'working)
    ('ready 'ready)
    ('killed 'killed)
    (_ 'other)))

(defun agent-dashboard--agent-status (buf)
  "Return the raw status symbol for agent buffer BUF."
  (let ((s (and (fboundp 'agent-shell-manager--get-status)
                (ignore-errors (agent-shell-manager--get-status buf)))))
    (cond
     ((stringp s) (intern s))
     ((symbolp s) s)
     (t (or (ignore-errors
              (with-current-buffer buf
                (when (fboundp 'agent-shell-status) (agent-shell-status))))
            'unknown)))))

(defun agent-dashboard--agent-perms (buf)
  "Return the number of pending permission requests for agent buffer BUF."
  (let ((p (and (fboundp 'agent-shell-manager--count-pending-permissions)
                (ignore-errors
                  (agent-shell-manager--count-pending-permissions buf)))))
    (cond ((numberp p) p)
          ((stringp p) (or (ignore-errors
                             (string-to-number (substring-no-properties p)))
                           0))
          (t 0))))

(defun agent-dashboard--agent-info (buf)
  "Collect a render-ready plist of state for agent buffer BUF."
  (let* ((state (ignore-errors (buffer-local-value 'agent-shell--state buf)))
         (usage (and state (ignore-errors (map-elt state :usage))))
         (ticket (ignore-errors (buffer-local-value 'org-agent-shell--ticket-info buf)))
         (kind (or (and (fboundp 'agent-shell-manager--get-agent-kind)
                        (ignore-errors (agent-shell-manager--get-agent-kind buf)))
                   "agent"))
         (model (and (fboundp 'agent-shell-manager--get-model-id)
                     (ignore-errors (agent-shell-manager--get-model-id buf)))))
    (list :key (buffer-name buf)
          :buffer buf
          :status (agent-dashboard--agent-status buf)
          :perms (agent-dashboard--agent-perms buf)
          :kind kind
          :model (and model (not (string= model "-")) model)
          :cwd (or (ignore-errors (buffer-local-value 'default-directory buf)) "")
          :slug (and ticket (or (alist-get 'heading ticket)
                                (alist-get 'worktree ticket)))
          :branch (and ticket (alist-get 'branch ticket))
          :title (and state (ignore-errors (map-nested-elt state '(:session :title))))
          :cost (and usage (map-elt usage :cost-amount))
          :ctx-used (and usage (map-elt usage :context-used))
          :ctx-size (and usage (map-elt usage :context-size)))))

(defun agent-dashboard--agents ()
  "Return a list of agent info plists, or nil when agent-shell is absent."
  (when (fboundp 'agent-shell-buffers)
    (let (rows)
      (dolist (buf (ignore-errors (agent-shell-buffers)))
        (when (buffer-live-p buf)
          (push (agent-dashboard--agent-info buf) rows)))
      (nreverse rows))))

(defun agent-dashboard--status-counts (agents)
  "Return an alist of normalized-status -> count for AGENTS."
  (let (counts)
    (dolist (a agents)
      (let ((s (agent-dashboard--norm-status (plist-get a :status))))
        (setf (alist-get s counts 0) (1+ (alist-get s counts 0)))))
    counts))

(defun agent-dashboard--status-face (s)
  "Face for normalized status S."
  (pcase s
    ('waiting 'agent-dashboard-waiting-face)
    ('working 'agent-dashboard-working-face)
    ('ready 'agent-dashboard-ready-face)
    ('killed 'agent-dashboard-killed-face)
    (_ 'agent-dashboard-idle-face)))

(defun agent-dashboard--status-icon (s)
  "Glyph for normalized status S."
  (pcase s
    ('waiting "▲") ('working "●") ('ready "○") ('killed "✖") (_ "·")))

;;; Data: time / money

(defun agent-dashboard--fmt-date (greg)
  "Format Gregorian date GREG (MONTH DAY YEAR) as YYYY-MM-DD."
  (format "%04d-%02d-%02d" (nth 2 greg) (nth 0 greg) (nth 1 greg)))

(defun agent-dashboard--period-range (period)
  "Return (FROM . TO) date strings for PERIOD: `today', `week' or `month'."
  (pcase-let* ((`(,_ ,_ ,_ ,d ,m ,y . ,_) (decode-time))
               (abs (calendar-absolute-from-gregorian (list m d y))))
    (pcase period
      ('today (let ((s (agent-dashboard--fmt-date (list m d y)))) (cons s s)))
      ('week
       (let* ((dow (calendar-day-of-week (list m d y)))
              (off (mod (- dow calendar-week-start-day) 7))
              (start (- abs off)))
         (cons (agent-dashboard--fmt-date (calendar-gregorian-from-absolute start))
               (agent-dashboard--fmt-date (calendar-gregorian-from-absolute (+ start 6))))))
      ('month
       (cons (format "%04d-%02d-01" y m)
             (format "%04d-%02d-%02d" y m (calendar-last-day-of-month m y)))))))

(defun agent-dashboard--extract (period)
  "Return clock entries tagged work in PERIOD, or nil if org-clock-data absent."
  (when (fboundp 'org-clock-data-extract)
    (ignore-errors
      (pcase-let ((`(,from . ,to) (agent-dashboard--period-range period)))
        (org-clock-data-extract :tag "work" :from from :to to)))))

(defun agent-dashboard--period-totals (period)
  "Return (:minutes M :earnings E) clocked in PERIOD."
  (let ((by-date (when (fboundp 'org-clock-data-aggregate-by-date)
                   (ignore-errors
                     (org-clock-data-aggregate-by-date
                      (agent-dashboard--extract period))))))
    (list :minutes (apply #'+ 0 (mapcar (lambda (c) (or (plist-get (cdr c) :minutes) 0))
                                        by-date))
          :earnings (apply #'+ 0.0 (mapcar (lambda (c) (or (plist-get (cdr c) :earnings) 0.0))
                                           by-date)))))

(defun agent-dashboard--clients-today ()
  "Return per-client (CLIENT . (:minutes :earnings)) aggregation for today."
  (when (fboundp 'org-clock-data-aggregate-by-client)
    (ignore-errors
      (org-clock-data-aggregate-by-client (agent-dashboard--extract 'today)))))

;;; Data: active clocks

(defun agent-dashboard--heading-client ()
  "Return the client for the org heading at point."
  (or (seq-find (lambda (tg) (and (boundp 'org-clock-data-hourly-rates)
                                  (assoc tg org-clock-data-hourly-rates)))
                (org-get-tags))
      (org-entry-get nil "CLIENT" t)
      (and (buffer-file-name) (file-name-base (buffer-file-name)))
      "?"))

(defun agent-dashboard--with-clock-key (key fn)
  "Move point to the heading whose CLOCK_MULTI_ID is KEY and call FN there.
Scans `org-agenda-files'.  Returns non-nil if found."
  (catch 'done
    (dolist (file (ignore-errors (org-agenda-files)))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (when (re-search-forward
                  (format ":CLOCK_MULTI_ID:[ \t]+%s" (regexp-quote key)) nil t)
             (org-back-to-heading t)
             (funcall fn)
             (throw 'done t))))))
    nil))

(defun agent-dashboard--active-clocks ()
  "Return a list of plists describing currently running org-clock-multi clocks."
  (when (and (boundp 'org-clock-multi-clocks)
             org-clock-multi-clocks
             (fboundp 'org-clock-multi--current-minutes))
    (let ((now (ignore-errors (org-clock-multi--current-minutes)))
          rows)
      (dolist (c org-clock-multi-clocks)
        (let ((key (car c)) (start (cdr c)))
          (ignore-errors
            (agent-dashboard--with-clock-key
             key
             (lambda ()
               (push (list :key key
                           :heading (org-get-heading t t t t)
                           :client (agent-dashboard--heading-client)
                           :elapsed (and now (numberp start) (- now start)))
                     rows))))))
      (nreverse rows))))

;;; Data: backlog

(defun agent-dashboard--backlog ()
  "Return an alist of (CLIENT . ITEMS) for open work todos.
Each item is a plist with :state :heading :file."
  (let ((states '("TODO" "IN PROGRESS" "WAITING"))
        (table (make-hash-table :test 'equal)))
    (ignore-errors
      (dolist (file (org-agenda-files))
        (when (file-exists-p file)
          (with-current-buffer (find-file-noselect file)
            (org-with-wide-buffer
             (goto-char (point-min))
             (while (re-search-forward org-heading-regexp nil t)
               (when (and (member (org-get-todo-state) states)
                          (member "work" (org-get-tags)))
                 (let ((client (agent-dashboard--heading-client))
                       (item (list :state (org-get-todo-state)
                                   :heading (org-get-heading t t t t)
                                   :file file)))
                   (puthash client (cons item (gethash client table)) table)))))))))
    (let (result)
      (maphash (lambda (k v) (push (cons k (nreverse v)) result)) table)
      (sort result (lambda (a b) (string< (car a) (car b)))))))

;;; Rendering

(defun agent-dashboard--ctx-string (a)
  "Return a context-usage indicator string for agent plist A, or nil."
  (let ((u (plist-get a :ctx-used)) (s (plist-get a :ctx-size)))
    (when (and u s (> s 0))
      (agent-dashboard--p (format "ctx %s/%s" (agent-dashboard--kfmt u)
                                  (agent-dashboard--kfmt s))
                          'agent-dashboard-faded-face))))

(defun agent-dashboard--short-cwd (cwd)
  "Return the last path component of CWD."
  (if (and cwd (> (length cwd) 0))
      (file-name-nondirectory (directory-file-name cwd))
    "?"))

(defun agent-dashboard--format-agent (a)
  "Render agent plist A to a string with clickable action blocks."
  (let* ((status (agent-dashboard--norm-status (plist-get a :status)))
         (key (plist-get a :key))
         (face (agent-dashboard--status-face status))
         (slug (or (plist-get a :slug)
                   (agent-dashboard--short-cwd (plist-get a :cwd))))
         (branch (plist-get a :branch))
         (model (plist-get a :model))
         (cost (plist-get a :cost))
         (ctx (agent-dashboard--ctx-string a))
         (perms (plist-get a :perms))
         (title (plist-get a :title)))
    (concat
     "  " (agent-dashboard--p (concat (agent-dashboard--status-icon status) " "
                                      (plist-get a :kind)) face)
     "  " (agent-dashboard--p (agent-dashboard--pad slug 24) 'agent-dashboard-slug-face)
     (when branch (concat " " (agent-dashboard--p branch 'agent-dashboard-faded-face)))
     "\n      "
     (when model (concat (agent-dashboard--p model 'agent-dashboard-faded-face) "  "))
     (when ctx (concat ctx "  "))
     (when cost (agent-dashboard--p (format "$%.2f" cost) 'agent-dashboard-money-face))
     (when title (concat "  " (agent-dashboard--p
                               (agent-dashboard--pad (concat "“" title "”") 40)
                               'agent-dashboard-faded-face)))
     (when (and (eq status 'waiting) (> perms 0))
       (concat "  " (agent-dashboard--p (format "needs you (%d)" perms)
                                        'agent-dashboard-waiting-face)))
     "\n      "
     (agent-dashboard--button "open" 'agent-dashboard--act-open :key key) " "
     (when (eq status 'waiting)
       (concat (agent-dashboard--button "answer" 'agent-dashboard--act-open :key key) " "))
     (agent-dashboard--button "diff" 'agent-dashboard--act-diff :key key) " "
     (agent-dashboard--button "transcript" 'agent-dashboard--act-transcript :key key) " "
     (agent-dashboard--button "msg" 'agent-dashboard--act-msg :key key)
     "\n\n")))

(defun agent-dashboard--section-header ()
  "Render the title bar."
  (concat
   (agent-dashboard--rule) "\n"
   "  " (agent-dashboard--p "AGENT DASHBOARD" 'agent-dashboard-header-face)
   "      " (agent-dashboard--p (format-time-string "%a %Y-%m-%d %H:%M")
                                'agent-dashboard-faded-face)
   "      " (agent-dashboard--button "⟳ refresh" 'agent-dashboard--act-refresh)
   "  " (agent-dashboard--button "✕ quit" 'agent-dashboard--act-quit)
   "\n" (agent-dashboard--rule) "\n\n"))

(defun agent-dashboard--section-summary ()
  "Render the time / money summary, per-client breakdown and active clocks."
  (let* ((today (agent-dashboard--period-totals 'today))
         (week (agent-dashboard--period-totals 'week))
         (month (agent-dashboard--period-totals 'month))
         (tmin (plist-get today :minutes))
         (clients (agent-dashboard--clients-today))
         (active (agent-dashboard--active-clocks)))
    (concat
     "  " (agent-dashboard--p "TODAY " 'agent-dashboard-section-face)
     (agent-dashboard--fmt-mins tmin) " / "
     (agent-dashboard--fmt-mins agent-dashboard-workday-minutes) "  "
     (agent-dashboard--bar tmin agent-dashboard-workday-minutes 26) "  "
     (agent-dashboard--p (agent-dashboard--money (plist-get today :earnings))
                         'agent-dashboard-money-face)
     "\n  " (agent-dashboard--p "WEEK  " 'agent-dashboard-section-face)
     (agent-dashboard--pad (agent-dashboard--fmt-mins (plist-get week :minutes)) 8)
     "    " (agent-dashboard--p (agent-dashboard--money (plist-get week :earnings))
                                'agent-dashboard-money-face)
     "\n  " (agent-dashboard--p "MONTH " 'agent-dashboard-section-face)
     (agent-dashboard--pad (agent-dashboard--fmt-mins (plist-get month :minutes)) 8)
     "    " (agent-dashboard--p (agent-dashboard--money (plist-get month :earnings))
                                'agent-dashboard-money-face)
     " " (agent-dashboard--p
          (format "(%s)" (agent-dashboard--euro-format
                          (agent-dashboard--month-net (plist-get month :earnings))))
          'agent-dashboard-money-face)
     "\n"
     (when clients
       (concat "\n  " (agent-dashboard--p "By client (today)" 'agent-dashboard-faded-face) "\n"
               (mapconcat
                (lambda (c)
                  (concat "    • " (agent-dashboard--pad (car c) 18) "  "
                          (agent-dashboard--pad
                           (agent-dashboard--fmt-mins (plist-get (cdr c) :minutes)) 8)
                          "  " (agent-dashboard--p
                                (agent-dashboard--money (plist-get (cdr c) :earnings))
                                'agent-dashboard-money-face)))
                clients "\n")
               "\n"))
     (when active
       (concat "\n  " (agent-dashboard--p "Active clocks" 'agent-dashboard-faded-face) "\n"
               (mapconcat
                (lambda (c)
                  (concat "    " (agent-dashboard--p "●" 'agent-dashboard-working-face)
                          "  " (agent-dashboard--pad (or (plist-get c :heading) "?") 30)
                          "  " (agent-dashboard--pad
                                (if (plist-get c :elapsed)
                                    (agent-dashboard--fmt-mins (plist-get c :elapsed)) "") 8)
                          "  " (agent-dashboard--button "stop" 'agent-dashboard--act-clock-out
                                                        :clock (plist-get c :key))))
                active "\n")
               "\n"))
     "\n")))

(defun agent-dashboard--section-agents ()
  "Render the agents section, grouped by status."
  (let* ((agents (agent-dashboard--agents))
         (counts (agent-dashboard--status-counts agents))
         (groups '((waiting . "WAITING — needs you")
                   (working . "WORKING")
                   (ready . "READY / IDLE")
                   (other . "OTHER")
                   (killed . "KILLED / STOPPED"))))
    (concat
     (agent-dashboard--rule) "\n"
     "  " (agent-dashboard--p "AGENTS" 'agent-dashboard-section-face)
     "    " (agent-dashboard--p
             (format "working %d · waiting %d · ready %d · %d total"
                     (alist-get 'working counts 0)
                     (alist-get 'waiting counts 0)
                     (alist-get 'ready counts 0)
                     (length agents))
             'agent-dashboard-faded-face)
     "\n" (agent-dashboard--rule) "\n\n"
     (if (null agents)
         (concat "  " (agent-dashboard--p
                       (if (fboundp 'agent-shell-buffers)
                           "No agents running."
                         "agent-shell not loaded.")
                       'agent-dashboard-faded-face)
                 "\n\n")
       (mapconcat
        (lambda (g)
          (let ((items (seq-filter
                        (lambda (a) (eq (agent-dashboard--norm-status (plist-get a :status))
                                        (car g)))
                        agents)))
            (if (null items) ""
              (concat "  " (agent-dashboard--p (cdr g)
                                               (agent-dashboard--status-face (car g)))
                      "\n"
                      (mapconcat #'agent-dashboard--format-agent items "")))))
        groups "")))))

(defun agent-dashboard--section-backlog ()
  "Render the backlog section, grouped by client."
  (let ((bl (agent-dashboard--backlog)))
    (concat
     (agent-dashboard--rule) "\n"
     "  " (agent-dashboard--p "BACKLOG (work todos)" 'agent-dashboard-section-face)
     "\n" (agent-dashboard--rule) "\n\n"
     (if (null bl)
         (concat "  " (agent-dashboard--p "No open work todos." 'agent-dashboard-faded-face) "\n")
       (mapconcat
        (lambda (cg)
          (concat "  " (agent-dashboard--p (car cg) 'agent-dashboard-slug-face) "\n"
                  (mapconcat
                   (lambda (it)
                     (concat "    "
                             (agent-dashboard--p
                              (agent-dashboard--pad (plist-get it :state) 11)
                              'agent-dashboard-faded-face)
                             " " (agent-dashboard--pad (plist-get it :heading) 40) " "
                             (agent-dashboard--button "go" 'agent-dashboard--act-goto-org
                                                      :file (plist-get it :file)
                                                      :heading (plist-get it :heading))))
                   (seq-take (cdr cg) agent-dashboard-backlog-max) "\n")
                  "\n"))
        bl "\n")))))

(defun agent-dashboard--build ()
  "Return the full dashboard content string (with org-ui block syntax)."
  (concat
   (agent-dashboard--section-header)
   (agent-dashboard--section-summary)
   (agent-dashboard--section-agents)
   (agent-dashboard--section-backlog)))

;;; Refresh

(defun agent-dashboard--signature ()
  "Return a cheap value summarizing visible state, to skip no-op redraws."
  (list (format-time-string "%H:%M")
        (ignore-errors
          (and (fboundp 'kwrooijen/org-clock-clocked-today)
               (floor (kwrooijen/org-clock-clocked-today))))
        (length (and (boundp 'org-clock-multi-clocks) org-clock-multi-clocks))
        (mapcar (lambda (a)
                  (list (plist-get a :key)
                        (agent-dashboard--norm-status (plist-get a :status))
                        (plist-get a :perms)
                        (and (plist-get a :cost) (floor (* 100 (plist-get a :cost))))))
                (agent-dashboard--agents))))

(defun agent-dashboard-refresh ()
  "Redraw the dashboard buffer, preserving cursor line and scroll."
  (interactive)
  (let ((buf (get-buffer agent-dashboard-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (agent-dashboard--ensure-ui)
        (let* ((win (get-buffer-window buf))
               (line (line-number-at-pos))
               (wstart (and win (window-start win)))
               (inhibit-read-only t))
          (erase-buffer)
          (insert (agent-dashboard--build))
          (goto-char (point-min))
          (ignore-errors (ui/process-unique-ids))
          (ignore-errors (ui/replace-button-syntax))
          (goto-char (point-min))
          (forward-line (1- line))
          (when (and win (window-live-p win) wstart)
            (set-window-start win wstart)))
        (ignore-errors (font-lock-ensure))
        (setq agent-dashboard--signature (agent-dashboard--signature))))))

(defun agent-dashboard--tick ()
  "Background timer callback: refresh only when state changed."
  (let ((buf (get-buffer agent-dashboard-buffer-name)))
    (if (not (buffer-live-p buf))
        (agent-dashboard--cancel-timer)
      (when (and (get-buffer-window buf t)
                 (not (active-minibuffer-window)))
        (let ((sig (agent-dashboard--signature)))
          (unless (equal sig (buffer-local-value 'agent-dashboard--signature buf))
            (agent-dashboard-refresh)))))))

(defun agent-dashboard--start-timer ()
  "Start the background refresh timer."
  (agent-dashboard--cancel-timer)
  (setq agent-dashboard--timer
        (run-with-timer agent-dashboard-refresh-interval
                        agent-dashboard-refresh-interval
                        #'agent-dashboard--tick)))

(defun agent-dashboard--cancel-timer ()
  "Cancel the background refresh timer."
  (when agent-dashboard--timer
    (cancel-timer agent-dashboard--timer)
    (setq agent-dashboard--timer nil)))

;;; Actions

(defun agent-dashboard--act-refresh (&rest _)
  "Action: manual refresh."
  (agent-dashboard-refresh))

(defun agent-dashboard--act-quit (&rest _)
  "Action: quit the dashboard window."
  (quit-window))

(defun agent-dashboard--act-open (block)
  "Action: jump to the agent buffer referenced by BLOCK."
  (let ((buf (agent-dashboard--block-buffer block)))
    (if (buffer-live-p buf)
        (pop-to-buffer buf)
      (message "Agent buffer is gone; press g to refresh."))))

(defun agent-dashboard--act-diff (block)
  "Action: show the git diff for the agent's worktree referenced by BLOCK."
  (let* ((buf (agent-dashboard--block-buffer block))
         (ticket (and (buffer-live-p buf)
                      (ignore-errors (buffer-local-value 'org-agent-shell--ticket-info buf))))
         (dir (or (and ticket (alist-get 'worktree_path ticket))
                  (and (buffer-live-p buf)
                       (buffer-local-value 'default-directory buf)))))
    (if (not (and dir (file-directory-p dir)))
        (message "No worktree directory for this agent.")
      (let ((default-directory dir)
            (out (get-buffer-create "*agent-dashboard-diff*")))
        (with-current-buffer out
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (shell-command-to-string "git diff --stat; echo; git diff"))
            (goto-char (point-min))
            (when (fboundp 'diff-mode) (diff-mode))))
        (pop-to-buffer out)))))

(defun agent-dashboard--act-transcript (block)
  "Action: open the transcript file for the agent referenced by BLOCK."
  (let* ((buf (agent-dashboard--block-buffer block))
         (tf (and (buffer-live-p buf)
                  (ignore-errors (buffer-local-value 'agent-shell--transcript-file buf)))))
    (if (and tf (file-exists-p tf))
        (find-file-other-window tf)
      (message "No transcript file for this agent."))))

(defun agent-dashboard--act-msg (block)
  "Action: send a message to the agent referenced by BLOCK."
  (let ((buf (agent-dashboard--block-buffer block)))
    (if (not (buffer-live-p buf))
        (message "Agent buffer is gone.")
      (let ((text (read-string "Message to agent: ")))
        (when (and text (not (string-empty-p text)))
          (if (fboundp 'agent-shell-insert)
              (progn (agent-shell-insert :text text :submit t :shell-buffer buf)
                     (message "Sent."))
            (message "agent-shell-insert is unavailable.")))))))

(defun agent-dashboard--act-clock-out (block)
  "Action: clock out the running clock referenced by BLOCK's :clock option."
  (let ((key (agent-dashboard--opt block :clock)))
    (when key
      (if (agent-dashboard--with-clock-key
           key (lambda () (when (fboundp 'org-clock-multi-clock-out)
                            (org-clock-multi-clock-out))))
          (progn (message "Clocked out.") (agent-dashboard-refresh))
        (message "Could not find that clock.")))))

(defun agent-dashboard--act-goto-org (block)
  "Action: open the org file/heading referenced by BLOCK."
  (let ((file (agent-dashboard--opt block :file))
        (heading (agent-dashboard--opt block :heading)))
    (when (and file (file-exists-p file))
      (find-file-other-window file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (and heading
                  (re-search-forward
                   (concat "^\\*+.*" (regexp-quote heading)) nil t))
         (org-back-to-heading t)
         (when (fboundp 'org-fold-show-context) (org-fold-show-context 'agenda)))))))

;;; Mode and entry point

(defvar agent-dashboard-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") #'agent-dashboard-refresh)
    (define-key m (kbd "r") #'agent-dashboard-refresh)
    (define-key m (kbd "q") #'quit-window)
    m)
  "Keymap for `agent-dashboard-mode'.")

(define-minor-mode agent-dashboard-mode
  "Minor mode active in the agent dashboard buffer.
Provides refresh/quit keys and the background refresh timer.  The buffer
itself stays in `org-mode' so the org-ui block machinery works; per-row
actions are org-ui clickable blocks activated with RET."
  :lighter " AgentDash"
  :keymap agent-dashboard-mode-map
  (if agent-dashboard-mode
      (add-hook 'kill-buffer-hook #'agent-dashboard--cancel-timer nil t)
    (agent-dashboard--cancel-timer)))

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal agent-dashboard-mode-map
      "g" #'agent-dashboard-refresh
      "r" #'agent-dashboard-refresh
      "q" #'quit-window)))

;;;###autoload
(defun agent-dashboard ()
  "Open (or refresh) the agent dashboard."
  (interactive)
  (agent-dashboard--ensure-ui)
  (let ((buf (get-buffer-create agent-dashboard-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode) (org-mode))
      (setq-local org-startup-folded nil)
      (agent-dashboard-mode 1)
      (setq buffer-read-only t))
    (pop-to-buffer buf)
    (agent-dashboard-refresh)
    (agent-dashboard--start-timer)
    buf))

(provide 'agent-dashboard)
;;; agent-dashboard.el ends here
