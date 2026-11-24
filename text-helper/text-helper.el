;;; text-helper.el --- AI writing coach for practicing prose -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (acp "0.7.1") (agent-shell "0.16.2"))
;; Keywords: writing, tools, convenience

;;; Commentary:

;; A distraction-free scratchpad for practicing written communication in any
;; language (Dutch, English, ...).  You open a fresh entry, type a message, and
;; on a keystroke an AI coach *reviews* it -- pointing out grammar mistakes,
;; awkward phrasing, and better word choices, and explaining WHY -- so you learn
;; to write better.  It deliberately does NOT rewrite your message for you; it
;; teaches.  Because speed matters more than depth here, the review runs on the
;; fast Haiku model.
;;
;; Usage:
;;
;;   M-x text-helper-compose  (bound to "SPC l c")
;;       Create and open a new numbered entry under `text-helper-directory'
;;       (e.g. ~/.text-helper/entry-001.text-helper) in `text-helper-mode'.
;;       It is a normal editable buffer -- save with C-x C-s as usual.
;;
;;   C-c C-c  (in `text-helper-mode')
;;       Send the buffer text to Claude Code (Haiku) and show coaching feedback
;;       in a right-side window.  Never rewrites; teaches with targeted
;;       before/after examples, in the same language as your message.
;;
;; The review transport mirrors the ACP async flow used by agent-review
;; (initialize -> session/new -> [session/set_model haiku] -> session/prompt),
;; but with a writing-coach prompt and free-text output instead of a git diff
;; and a code-review parser.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)

;;; Customization

(defgroup text-helper nil
  "AI writing coach for practicing prose."
  :group 'tools
  :prefix "text-helper-")

(defcustom text-helper-directory "~/.text-helper/"
  "Directory where writing-practice entries are stored."
  :type 'directory)

(defcustom text-helper-file-extension ".text-helper"
  "File extension used for writing-practice entries."
  :type 'string)

(defcustom text-helper-model-id "sonnet"
  "ACP model slug used for reviews.
This is the short slug advertised by the agent in its session/new
response (e.g. \"haiku\", \"sonnet\", \"opus\", \"default\"), not a full
API model id.  If the running agent does not advertise this slug, the
review falls back to the agent's default model."
  :type 'string)

(defcustom text-helper-config-function
  #'agent-shell-anthropic-make-claude-code-config
  "Function returning the agent-shell config used for reviews.
Called with no arguments; must return an agent config alist containing
a `:client-maker' entry (as produced by
`agent-shell-anthropic-make-claude-code-config')."
  :type 'function)

(defcustom text-helper-buffer-name "*Text Helper Feedback*"
  "Name of the buffer in which coaching feedback is displayed."
  :type 'string)

(defcustom text-helper-instructions
  "You are a writing checker.  Reply in TWO sections and nothing else.

Silently detect the language of the message (Dutch, English, or other)
and write ALL feedback in that same language.

=== Section 1: Fixes ===
A prioritized list of concrete fixes the user can scan at a glance.
- One point per line.  Keep each point brief (a handful of words at most).
- Do NOT rewrite the whole message.  Fix only the fragment that is wrong.
- No praise and no chatter in this section.
- Order by priority, most urgent first.  Tag each point:
    [URGENT]  spelling errors and clear grammar errors.
    [FIX]     wrong word choice, wrong preposition, punctuation.
    [STYLE]   awkward or unclear phrasing that is not strictly wrong.
- Format every line exactly as:
    [PRIORITY] \"wrong fragment\" -> \"fix\" (brief why)
- If there is nothing to fix, write a single line: no issues.

=== Section 2: General ===
After the fixes, a short blank line, then a heading \"General:\" followed
by a few general styling observations about the message as a whole --
tone, structure, clarity, flow, word variety, conciseness.  These are
broader than a single fragment.  For each observation, say plainly
whether it works well or poorly and explain WHY, so the user understands
the reasoning.  Keep it to a few sentences or short bullets -- helpful,
not an essay."
  "System prompt sent to the writing coach.
The user's message is appended after these instructions."
  :type 'string)

;;; Buffer-local session state (in the hidden work buffer)

(defvar-local text-helper--response nil
  "Accumulated streamed response text for the current review.")
(defvar-local text-helper--session-client nil
  "ACP client for the current review session.")
(defvar-local text-helper--session-id nil
  "ACP session id for the current review session.")

;;; Prompt

(defun text-helper--make-prompt (text)
  "Build the full review prompt for TEXT."
  (concat text-helper-instructions
          "\n\n=== The message to review ===\n\n"
          text))

;;; Feedback display

(defvar text-helper-feedback-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") #'quit-window)
    m)
  "Keymap for `text-helper-feedback-mode'.")

(define-derived-mode text-helper-feedback-mode special-mode "TextHelper-Feedback"
  "Major mode for displaying writing-coach feedback.
The buffer is read-only; press \\[quit-window] to close it.")

(defun text-helper--render (content)
  "Display CONTENT in the feedback side window and return its buffer."
  (let ((buf (get-buffer-create text-helper-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'text-helper-feedback-mode)
        (text-helper-feedback-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min))))
    (display-buffer buf '((display-buffer-in-side-window)
                          (side . right)
                          (window-width . 0.42)))
    buf))

(defun text-helper--set-status (status)
  "Show a transient STATUS line in the feedback window."
  (text-helper--render (concat "⏳ " status)))

;;; ACP transport

(defun text-helper--cleanup-session (buffer)
  "Cancel and shut down the review session held in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and text-helper--session-id text-helper--session-client)
        (ignore-errors
          (acp-send-notification
           :client text-helper--session-client
           :notification (acp-make-session-cancel-notification
                          :session-id text-helper--session-id
                          :reason "Review complete"))))
      (when text-helper--session-client
        (ignore-errors (acp-shutdown :client text-helper--session-client)))
      (setq-local text-helper--session-client nil)
      (setq-local text-helper--session-id nil))))

(defun text-helper--request-review-async (text config on-complete)
  "Review TEXT using agent CONFIG, calling ON-COMPLETE when done.
ON-COMPLETE is called with (response error) where ERROR is nil on
success.  The review runs asynchronously so Emacs stays responsive."
  (let ((work-buffer (generate-new-buffer " *text-helper-work*")))
    (with-current-buffer work-buffer
      (setq-local text-helper--response "")
      (let ((client (funcall (alist-get :client-maker config) work-buffer)))
        (setq-local text-helper--session-client client)

        ;; Capture streamed agent output.
        (acp-subscribe-to-notifications
         :client client
         :buffer work-buffer
         :on-notification
         (lambda (notification)
           (when (buffer-live-p work-buffer)
             (with-current-buffer work-buffer
               (let-alist notification
                 (when (equal .method "session/update")
                   (let ((update (alist-get 'update .params)))
                     (when (equal (alist-get 'sessionUpdate update)
                                  "agent_message_chunk")
                       (let-alist update
                         (setq-local text-helper--response
                                     (concat text-helper--response
                                             (or .content.text "")))
                         (text-helper--set-status
                          "Agent is writing feedback…"))))))))))

        ;; Surface protocol errors.
        (acp-subscribe-to-errors
         :client client
         :buffer work-buffer
         :on-error
         (lambda (err)
           (when (buffer-live-p work-buffer)
             (text-helper--cleanup-session work-buffer)
             (kill-buffer work-buffer))
           (funcall on-complete nil (format "Agent error: %S" err))))

        ;; 1. Initialize.
        (text-helper--set-status "Handshaking with agent…")
        (acp-send-request
         :client client
         :sync nil
         :request (acp-make-initialize-request
                   :protocol-version 1
                   :read-text-file-capability nil
                   :write-text-file-capability nil)
         :on-success
         (lambda (_result)
           (when (buffer-live-p work-buffer)
             ;; 2. Create session.
             (text-helper--set-status "Creating session…")
             (acp-send-request
              :client client
              :sync nil
              :request (acp-make-session-new-request
                        :cwd default-directory
                        :mcp-servers [])
              :on-success
              (lambda (session-response)
                (when (buffer-live-p work-buffer)
                  (with-current-buffer work-buffer
                    (let* ((session-id (alist-get 'sessionId session-response))
                           (models (ignore-errors
                                     (map-nested-elt session-response
                                                     '(models availableModels))))
                           (ids (and models
                                     (mapcar (lambda (m) (map-elt m 'modelId))
                                             (append models nil))))
                           ;; 4. Send the prompt (shared by both set-model paths).
                           (send-prompt
                            (lambda ()
                              (when (buffer-live-p work-buffer)
                                (text-helper--set-status "Waiting for feedback…")
                                (acp-send-request
                                 :client client
                                 :sync nil
                                 :request (acp-make-session-prompt-request
                                           :session-id session-id
                                           :prompt (vector
                                                    (list (cons 'type "text")
                                                          (cons 'text (text-helper--make-prompt text)))))
                                 :on-success
                                 (lambda (_r)
                                   (when (buffer-live-p work-buffer)
                                     (let ((response (with-current-buffer work-buffer
                                                       text-helper--response)))
                                       (text-helper--cleanup-session work-buffer)
                                       (kill-buffer work-buffer)
                                       (funcall on-complete response nil))))
                                 :on-failure
                                 (lambda (err)
                                   (text-helper--cleanup-session work-buffer)
                                   (kill-buffer work-buffer)
                                   (funcall on-complete nil (format "Prompt request failed: %S" err))))))))
                      (setq-local text-helper--session-id session-id)
                      ;; 3. Pin the model, then prompt.  Never abort on set-model
                      ;;    trouble -- fall back to the agent's default model.
                      (if (and ids (member text-helper-model-id ids))
                          (acp-send-request
                           :client client
                           :sync nil
                           :request (acp-make-session-set-model-request
                                     :session-id session-id
                                     :model-id text-helper-model-id)
                           :on-success (lambda (_r) (funcall send-prompt))
                           :on-failure
                           (lambda (err)
                             (message "text-helper: set-model failed (%S); using default model" err)
                             (funcall send-prompt)))
                        (when (and ids text-helper-model-id)
                          (message "text-helper: model %S not advertised (have %S); using default model"
                                   text-helper-model-id ids))
                        (funcall send-prompt))))))
              :on-failure
              (lambda (err)
                (text-helper--cleanup-session work-buffer)
                (kill-buffer work-buffer)
                (funcall on-complete nil (format "Session creation failed: %S" err))))))
         :on-failure
         (lambda (err)
           (text-helper--cleanup-session work-buffer)
           (kill-buffer work-buffer)
           (funcall on-complete nil (format "Initialization failed: %S" err))))))))

;;; Commands

(defun text-helper--next-entry-path ()
  "Return the path of the next numbered entry file, creating the dir."
  (make-directory text-helper-directory t)
  (let ((re (concat "\\`entry-\\([0-9]+\\)"
                    (regexp-quote text-helper-file-extension) "\\'"))
        (max-n 0))
    (dolist (f (directory-files text-helper-directory nil nil t))
      (when (string-match re f)
        (setq max-n (max max-n (string-to-number (match-string 1 f))))))
    (expand-file-name (format "entry-%03d%s" (1+ max-n) text-helper-file-extension)
                      text-helper-directory)))

;;;###autoload
(defun text-helper-compose ()
  "Create and open a new writing-practice entry."
  (interactive)
  (find-file (text-helper--next-entry-path))
  (message "Write your message, then press C-c C-c for feedback."))

(defun text-helper-review ()
  "Send the current buffer to the writing coach and show feedback."
  (interactive)
  (require 'agent-shell)
  (require 'agent-shell-anthropic)
  (require 'acp)
  (let ((text (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p text)
      (user-error "Nothing to review -- write a message first"))
    (when (and buffer-file-name (buffer-modified-p))
      (save-buffer))
    (let ((config (funcall text-helper-config-function)))
      (text-helper--set-status
       (format "Reviewing with %s…" (capitalize text-helper-model-id)))
      (text-helper--request-review-async
       text config
       (lambda (response error-msg)
         (cond
          (error-msg
           (message "text-helper review failed: %s" error-msg)
           (text-helper--render (format "Review failed:\n\n%s" error-msg)))
          ((and response (not (string-empty-p (string-trim response))))
           (text-helper--render response)
           (message "text-helper: feedback ready"))
          (t
           (text-helper--render "No feedback was returned."))))))))

;;; Mode and key bindings

(defvar text-helper-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'text-helper-review)
    m)
  "Keymap for `text-helper-mode'.")

;;;###autoload
(define-derived-mode text-helper-mode text-mode "TextHelper"
  "Major mode for writing-practice entries reviewed by an AI coach.
\\<text-helper-mode-map>Press \\[text-helper-review] to get coaching feedback.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.text-helper\\'" . text-helper-mode))

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key)
    (evil-define-key '(normal insert) text-helper-mode-map
      (kbd "C-c C-c") #'text-helper-review))
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'text-helper-feedback-mode 'normal)))

(with-eval-after-load 'general
  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'override
   :prefix "SPC"
   "l c" '(text-helper-compose :which-key "Compose text")))

(provide 'text-helper)
;;; text-helper.el ends here
