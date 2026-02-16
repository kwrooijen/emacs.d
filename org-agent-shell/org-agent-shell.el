;;; org-agent-shell.el --- Manage agent-shell workspaces from org TODOs -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.0") (agent-shell "0.1.0") (magit "3.0"))
;; Keywords: org, agent, shell, worktree

;;; Commentary:

;; Turn org TODOs into managed agent-shell workspaces.
;;
;; Each org heading can have:
;;   :PROJECT:  - path to git repo root (inherited from parent)
;;   :BRANCH:   - feature branch name
;;   :WORKTREE: - auto-populated worktree name on first launch
;;
;; Commands:
;;   org-agent-shell-launch     - Create worktree + start agent-shell + send plan
;;   org-agent-shell-open-shell - Switch to the agent-shell buffer
;;   org-agent-shell-open-magit - Open magit in the worktree
;;   org-agent-shell-diff       - Show branch diff from base
;;   org-agent-shell-resend     - Resend org body to agent-shell
;;   org-agent-shell-review     - Run agent-review in the worktree
;;   org-agent-shell-reset      - Kill shell, reset branch, optionally relaunch

;;; Code:

(require 'org)
(require 'agent-shell)
(require 'agent-shell-worktree)
(require 'magit)

;;; Customization

(defgroup org-agent-shell nil
  "Manage agent-shell workspaces from org TODOs."
  :group 'org
  :prefix "org-agent-shell-")

(defcustom org-agent-shell-base-branch "staging"
  "Default base branch to create feature branches from."
  :type 'string
  :group 'org-agent-shell)

;;; Helpers

(defun org-agent-shell--get-property (property)
  "Get PROPERTY from current heading, with inheritance.
When in an edit-indirect buffer, look up the property in the parent buffer."
  (if (and (boundp 'edit-indirect--overlay) edit-indirect--overlay)
      (let ((parent (overlay-buffer edit-indirect--overlay))
            (pos (overlay-start edit-indirect--overlay)))
        (with-current-buffer parent
          (save-excursion
            (goto-char pos)
            (org-entry-get nil property t))))
    (org-entry-get nil property t)))

(defun org-agent-shell--worktree-path ()
  "Compute the worktree path for the current heading."
  (let ((project (org-agent-shell--get-property "PROJECT"))
        (worktree (org-entry-get nil "WORKTREE")))
    (unless project
      (user-error "No :PROJECT: property found"))
    (unless worktree
      (user-error "No :WORKTREE: property found"))
    (expand-file-name worktree
                      (file-name-concat (expand-file-name project)
                                        agent-shell-worktree--subdirectory))))

(defun org-agent-shell--heading-body ()
  "Get the body of the current org heading, stripping drawers and logbook."
  (save-excursion
    (org-back-to-heading t)
    (let ((content (org-get-entry)))
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        ;; Remove property drawers
        (while (re-search-forward
                "^[ \t]*:PROPERTIES:\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n?" nil t)
          (replace-match ""))
        (goto-char (point-min))
        ;; Remove logbook drawers
        (while (re-search-forward
                "^[ \t]*:LOGBOOK:\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n?" nil t)
          (replace-match ""))
        (goto-char (point-min))
        ;; Remove CLOSED/DEADLINE/SCHEDULED lines
        (while (re-search-forward
                "^[ \t]*\\(CLOSED\\|DEADLINE\\|SCHEDULED\\):.*\n?" nil t)
          (replace-match ""))
        (string-trim (buffer-string))))))

(defun org-agent-shell--find-shell-buffer (worktree-path)
  "Find the agent-shell buffer for WORKTREE-PATH."
  (let ((normalized (file-name-as-directory (expand-file-name worktree-path))))
    (cl-find-if
     (lambda (buf)
       (with-current-buffer buf
         (string= (file-name-as-directory (expand-file-name default-directory))
                  normalized)))
     (agent-shell-buffers))))

;;; Commands

(defun org-agent-shell-launch ()
  "Launch an agent-shell workspace for the current org heading.
Creates a git worktree and starts agent-shell with the heading body as input."
  (interactive)
  (org-back-to-heading t)
  (let ((project (org-agent-shell--get-property "PROJECT"))
        (branch (org-entry-get nil "BRANCH"))
        (worktree (org-entry-get nil "WORKTREE")))
    (unless project
      (user-error "No :PROJECT: property found"))
    (unless branch
      (user-error "No :BRANCH: property found"))
    (let* ((project-path (expand-file-name project))
           (worktree-name (or worktree
                              (let ((clean-branch (replace-regexp-in-string
                                                   "^\\(?:feature\\|bugfix\\|hotfix\\|fix\\|chore\\|release\\|support\\)/"
                                                   "" branch)))
                                (concat (file-name-nondirectory
                                         (directory-file-name project-path))
                                        "_" clean-branch))))
           (worktree-path (expand-file-name
                           worktree-name
                           (file-name-concat project-path
                                             agent-shell-worktree--subdirectory)))
           (body (org-agent-shell--heading-body))
           (reuse (and worktree (file-directory-p worktree-path))))
      ;; Set WORKTREE property if not already set
      (unless worktree
        (org-entry-put nil "WORKTREE" worktree-name)
        (save-buffer))
      ;; Create worktree if needed
      (unless reuse
        (make-directory (file-name-directory worktree-path) t)
        (let ((default-directory project-path))
          (let* ((branch-exists-p
                  (= 0 (call-process "git" nil nil nil "rev-parse" "--verify" branch)))
                 (output (shell-command-to-string
                          (if branch-exists-p
                              (format "git worktree add --force %s %s 2>&1"
                                      (shell-quote-argument worktree-path)
                                      (shell-quote-argument branch))
                            (format "git worktree add -b %s %s %s 2>&1"
                                    (shell-quote-argument branch)
                                    (shell-quote-argument worktree-path)
                                    (shell-quote-argument org-agent-shell-base-branch))))))
            (unless (file-exists-p worktree-path)
              (user-error "Failed to create worktree: %s" output)))))
      ;; Write ticket.org in worktree
      (when (and body (not (string-empty-p body)))
        (let ((ticket-file (expand-file-name "ticket.org" worktree-path)))
          (with-temp-file ticket-file
            (insert body))))
      ;; Start agent-shell in worktree, or switch to existing one
      (let ((existing (org-agent-shell--find-shell-buffer worktree-path))
            (display-buffer-overriding-action '((display-buffer-use-some-window))))
        (if existing
            (pop-to-buffer existing)
          (let ((default-directory worktree-path))
            (agent-shell '(4)))
          ;; Tell agent to read ticket.org
          (when (and body (not (string-empty-p body)))
            (run-with-timer 1.5 nil
                            (lambda (wpath)
                              (when-let* ((buf (org-agent-shell--find-shell-buffer wpath)))
                                (agent-shell-insert :text "Read ticket.org and enter plan mode"
                                                    :submit t
                                                    :shell-buffer buf)))
                            worktree-path)))))))

(defun org-agent-shell-open-shell ()
  "Switch to the agent-shell buffer for the current heading's worktree."
  (interactive)
  (let* ((worktree-path (org-agent-shell--worktree-path))
         (buf (org-agent-shell--find-shell-buffer worktree-path)))
    (if buf
        (let ((win (get-buffer-window buf)))
          (if (and win (window-live-p win))
              (select-window win)
            (pop-to-buffer buf '((display-buffer-use-some-window)))))
      (user-error "No agent-shell found for %s" worktree-path))))

(defun org-agent-shell-open-magit ()
  "Open magit-status in the worktree directory."
  (interactive)
  (let ((default-directory (org-agent-shell--worktree-path))
        (display-buffer-overriding-action '((display-buffer-use-some-window))))
    (magit-status)))

(defun org-agent-shell-diff ()
  "Show full branch diff from base branch."
  (interactive)
  (let* ((default-directory (org-agent-shell--worktree-path))
         (branch (org-agent-shell--get-property "BRANCH"))
         (display-buffer-overriding-action '((display-buffer-use-some-window)))
         (magit-section-initial-visibility-alist '((file . hide)))
         (range (format "%s...%s" org-agent-shell-base-branch branch))
         (stat (shell-command-to-string (format "git diff --stat %s" range))))
    (unless branch
      (user-error "No :BRANCH: property found"))
    (magit-diff-range range)
    (with-current-buffer (magit-get-mode-buffer 'magit-diff-mode)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (insert (propertize stat 'face 'magit-diff-file-heading)
                  "\n"))))))

(defun org-agent-shell-resend ()
  "Rewrite ticket.org and tell the agent-shell to re-read it."
  (interactive)
  (org-back-to-heading t)
  (let* ((worktree-path (org-agent-shell--worktree-path))
         (buf (org-agent-shell--find-shell-buffer worktree-path))
         (body (org-agent-shell--heading-body)))
    (unless buf
      (user-error "No agent-shell found for %s" worktree-path))
    (when (and body (not (string-empty-p body)))
      (let ((ticket-file (expand-file-name "ticket.org" worktree-path)))
        (with-temp-file ticket-file
          (insert body)))
      (agent-shell-insert :text "ticket.org has been updated. Read it again."
                          :submit t
                          :shell-buffer buf)
      (message "Updated ticket.org and notified agent-shell"))))

(defun org-agent-shell-review ()
  "Run agent-review in the worktree directory."
  (interactive)
  (let ((default-directory (org-agent-shell--worktree-path)))
    (kwrooijen/agent-review)))

(defun org-agent-shell-reset ()
  "Kill the agent-shell and reset the branch to base.
With prefix argument, also relaunch."
  (interactive)
  (let* ((worktree-path (org-agent-shell--worktree-path))
         (branch (org-entry-get nil "BRANCH"))
         (buf (org-agent-shell--find-shell-buffer worktree-path)))
    (unless branch
      (user-error "No :BRANCH: property found"))
    ;; Kill shell buffer
    (when buf
      (kill-buffer buf))
    ;; Reset branch
    (let ((default-directory worktree-path))
      (shell-command-to-string
       (format "git reset --hard %s"
               (shell-quote-argument org-agent-shell-base-branch))))
    (message "Reset %s to %s" branch org-agent-shell-base-branch)
    ;; Optionally relaunch
    (when (y-or-n-p "Relaunch agent-shell? ")
      (org-agent-shell-launch))))

(provide 'org-agent-shell)
;;; org-agent-shell.el ends here
