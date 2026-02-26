;;; mcp.el --- Elisp backend for the Emacs MCP server -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.0") (agent-shell "0.1.0") (org-agent-shell "0.1.0"))
;; Keywords: org, mcp, tools

;;; Commentary:

;; Elisp functions called by the MCP server (emacs-mcp-server.rb) via
;; emacsclient --eval.  Provides todo management and agent management
;; tool implementations.  All public functions return JSON strings.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'json)
(require 'org-agent-shell)
(require 'org-clock-multi)

;;; Helpers

(defun kwrooijen/mcp--get-own-properties ()
  "Return an alist of the current heading's own properties.
Excludes computed org properties to reduce noise."
  (let ((excluded '("CATEGORY" "ALLTAGS" "BLOCKED" "ITEM"
                     "PRIORITY" "FILE" "CLOCKSUM" "CLOCKSUM_T"
                     "TIMESTAMP" "TIMESTAMP_IA" "CLOSED"
                     "DEADLINE" "SCHEDULED"))
        result)
    (dolist (pair (org-entry-properties nil 'standard))
      (unless (member (car pair) excluded)
        (when (cdr pair)
          (push pair result))))
    (nreverse result)))

(defun kwrooijen/mcp--get-parent-properties ()
  "Return an alist of the parent heading's own properties.
Returns nil if already at top level."
  (save-excursion
    (condition-case nil
        (progn
          (outline-up-heading 1 t)
          (kwrooijen/mcp--get-own-properties))
      (error nil))))

(defun kwrooijen/mcp--find-heading-marker (file heading)
  "In FILE, return a marker at the heading matching HEADING text."
  (let ((buf (find-file-noselect (expand-file-name file))))
    (with-current-buffer buf
      (org-with-wide-buffer
       (goto-char (point-min))
       (catch 'found
         (while (re-search-forward org-heading-regexp nil t)
           (when (string= (org-get-heading t t t t) heading)
             (throw 'found (point-marker))))
         (error "Heading not found: %s in %s" heading file))))))

(defun kwrooijen/mcp--get-logbook ()
  "Return a list of clock entries from the current heading's LOGBOOK drawer.
Each entry is an alist with `start', `end', and `duration' keys.
Running clocks (no end time) have end and duration set to nil."
  (save-excursion
    (org-back-to-heading t)
    (let ((bound (save-excursion (org-end-of-subtree t t) (point)))
          entries)
      (when (re-search-forward ":LOGBOOK:" bound t)
        (let ((drawer-end (save-excursion
                            (re-search-forward ":END:" bound t)
                            (point))))
          (while (re-search-forward
                  "CLOCK: \\[\\([0-9]+-[0-9]+-[0-9]+ [A-Za-z]+ [0-9]+:[0-9]+\\)\\]\\(?:--\\[\\([0-9]+-[0-9]+-[0-9]+ [A-Za-z]+ [0-9]+:[0-9]+\\)\\] =>  *\\([0-9]+:[0-9]+\\)\\)?"
                  drawer-end t)
            (push `((start . ,(match-string 1))
                    (end . ,(match-string 2))
                    (duration . ,(match-string 3)))
                  entries))))
      (nreverse entries))))

(defun kwrooijen/mcp--get-heading-body ()
  "Return the body text of the current heading, excluding sub-headings.
Skips the property drawer, logbook, and planning lines."
  (save-excursion
    (org-back-to-heading t)
    (let* ((content-start (save-excursion
                            (org-end-of-meta-data t)
                            (point)))
           (subtree-end (save-excursion
                          (org-end-of-subtree t t)
                          (point)))
           (child-start (save-excursion
                          (goto-char content-start)
                          (if (re-search-forward org-heading-regexp subtree-end t)
                              (line-beginning-position)
                            subtree-end)))
           (body (buffer-substring-no-properties content-start child-start)))
      (string-trim body))))

(defun kwrooijen/mcp--get-subtasks ()
  "Return a list of alists for immediate child headings of current heading."
  (save-excursion
    (org-back-to-heading t)
    (let ((level (org-current-level))
          subtasks)
      (org-end-of-meta-data t)
      (while (re-search-forward org-heading-regexp
                                (save-excursion (org-end-of-subtree t t) (point))
                                t)
        (when (= (org-current-level) (1+ level))
          (push `((heading . ,(org-get-heading t t t t))
                  (state . ,(org-get-todo-state)))
                subtasks)))
      (nreverse subtasks))))

(defun kwrooijen/mcp--replace-heading-body (new-body)
  "Replace the body of the current heading with NEW-BODY."
  (save-excursion
    (org-back-to-heading t)
    (let* ((content-start (save-excursion
                            (org-end-of-meta-data t)
                            (point)))
           (subtree-end (save-excursion
                          (org-end-of-subtree t t)
                          (point)))
           (child-start (save-excursion
                          (goto-char content-start)
                          (if (re-search-forward org-heading-regexp subtree-end t)
                              (line-beginning-position)
                            subtree-end))))
      (delete-region content-start child-start)
      (goto-char content-start)
      (insert new-body "\n\n"))))

(defun kwrooijen/mcp--generate-branch-name (heading &optional asana-id)
  "Generate a concise branch name from HEADING text.
Strips common prefixes like [Feature], [Bug], etc.  Produces a short
kebab-case slug (max ~50 chars).  Prepends ASANA-ID when non-nil.
Returns a string like \"feature/1234-short-slug\"."
  (let* ((stripped (replace-regexp-in-string
                    "^\\[\\(?:Feature\\|Bug\\|Uitzoeken\\|Wijziging\\|Marketing\\|Uitbreiding\\)\\]\\s-*"
                    "" heading))
         (lower (downcase stripped))
         (kebab (replace-regexp-in-string "[^a-z0-9]+" "-" lower))
         (trimmed (replace-regexp-in-string "^-+\\|-+$" "" kebab))
         (short (if (> (length trimmed) 50)
                    (let ((cut (substring trimmed 0 50)))
                      (replace-regexp-in-string "-[^-]*$" "" cut))
                  trimmed))
         (slug (if (and asana-id (not (string-empty-p asana-id)))
                   (concat asana-id "-" short)
                 short)))
    (concat "feature/" slug)))

;;; Clock Management

(defun kwrooijen/mcp--clock-in-heading (file heading)
  "Clock in the heading identified by FILE and HEADING using org-clock-multi."
  (let ((marker (kwrooijen/mcp--find-heading-marker file heading)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (org-clock-multi-clock-in)))))

(defun kwrooijen/mcp--clock-out-heading (file heading)
  "Clock out the heading identified by FILE and HEADING using org-clock-multi."
  (let ((marker (kwrooijen/mcp--find-heading-marker file heading)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (org-clock-multi-clock-out)))))

(defun kwrooijen/mcp--clock-out-overige (file)
  "Clock out the OVERIGE heading in FILE if it is clocked in."
  (condition-case nil
      (let ((marker (kwrooijen/mcp--find-heading-marker file "OVERIGE")))
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char marker)
           (when (org-clock-multi-clocking-p)
             (org-clock-multi-clock-out)))))
    (error nil)))

;;; Todo Management

(cl-defun kwrooijen/mcp-get-work-todo-files ()
  "Return a JSON array of org file paths in the client todos directory."
  (let* ((dir (expand-file-name "~/Documents/org/todos/client"))
         (files (directory-files dir t "\\.org$")))
    (json-encode files)))

(cl-defun kwrooijen/mcp-get-work-todos ()
  "Return a JSON string of org TODO items tagged :work:.
Queries agenda files for entries in TODO, IN PROGRESS, or WAITING state.
Includes task properties and parent heading properties."
  (let ((states '("TODO" "IN PROGRESS" "WAITING"))
        (tag "work")
        entries)
    (dolist (file (org-agenda-files))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward org-heading-regexp nil t)
           (when (and (member (org-get-todo-state) states)
                      (member tag (org-get-tags)))
             (let ((props (kwrooijen/mcp--get-own-properties))
                   (parent-props (kwrooijen/mcp--get-parent-properties)))
               (push `((heading . ,(org-get-heading t t t t))
                       (state . ,(org-get-todo-state))
                       (tags . ,(org-get-tags))
                       (file . ,(buffer-file-name))
                       (properties . ,props)
                       (parent_properties . ,parent-props))
                     entries)))))))
    (json-encode (nreverse entries))))

(cl-defun kwrooijen/mcp-get-work-todo (file heading)
  "Return JSON with full details of a single TODO identified by FILE and HEADING."
  (let ((marker (kwrooijen/mcp--find-heading-marker file heading)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (let* ((state (org-get-todo-state))
              (tags (org-get-tags))
              (props (kwrooijen/mcp--get-own-properties))
              (parent-props (kwrooijen/mcp--get-parent-properties))
              (logbook (kwrooijen/mcp--get-logbook))
              (body (kwrooijen/mcp--get-heading-body))
              (subtasks (kwrooijen/mcp--get-subtasks)))
         (json-encode
          `((heading . ,heading)
            (state . ,state)
            (tags . ,tags)
            (file . ,file)
            (properties . ,props)
            (parent_properties . ,parent-props)
            (logbook . ,(or logbook []))
            (body . ,body)
            (subtasks . ,subtasks))))))))

(cl-defun kwrooijen/mcp-edit-work-todo (file heading &key state set-properties delete-properties body)
  "Edit a TODO identified by FILE and HEADING.
Optionally change STATE, SET-PROPERTIES (alist), DELETE-PROPERTIES (list), or BODY."
  (let ((marker (kwrooijen/mcp--find-heading-marker file heading)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (when state
         (org-todo state))
       (dolist (pair set-properties)
         (org-set-property (car pair) (cdr pair)))
       (dolist (prop delete-properties)
         (org-delete-property prop))
       (when body
         (kwrooijen/mcp--replace-heading-body body))
       (save-buffer)
       (json-encode
        `((success . t)
          (heading . ,(org-get-heading t t t t))
          (state . ,(org-get-todo-state))
          (properties . ,(kwrooijen/mcp--get-own-properties))))))))

;;; Agent Management

(cl-defun kwrooijen/mcp-agent-set-branch (file heading &optional branch)
  "Set :BRANCH: property on the TODO identified by FILE and HEADING.
If BRANCH is nil or empty, auto-generate from heading text and ASANA id."
  (let ((marker (kwrooijen/mcp--find-heading-marker file heading)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (let* ((asana-id (org-entry-get nil "ASANA"))
              (effective-branch
               (if (and branch (not (string-empty-p branch)))
                   branch
                 (kwrooijen/mcp--generate-branch-name heading asana-id))))
         (org-entry-put nil "BRANCH" effective-branch)
         (save-buffer)
         (json-encode
          `((success . t)
            (heading . ,heading)
            (branch . ,effective-branch))))))))

(cl-defun kwrooijen/mcp-agent-launch (file heading)
  "Launch an agent-shell workspace for the TODO identified by FILE and HEADING.
Creates a git worktree, writes ticket.org, and starts agent-shell.
Requires :PROJECT: (inherited) and :BRANCH: properties."
  (let ((marker (kwrooijen/mcp--find-heading-marker file heading)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (let ((project (org-agent-shell--get-property "PROJECT"))
             (branch (org-entry-get nil "BRANCH"))
             (worktree (org-entry-get nil "WORKTREE"))
             (body (org-agent-shell--heading-body))
             (base (org-agent-shell--base-branch))
             (org-file (buffer-file-name)))
         (unless project
           (error "No :PROJECT: property found on heading: %s" heading))
         (unless branch
           (error "No :BRANCH: property found on heading: %s. Use agent_set_branch first" heading))
         (let ((result (org-agent-shell--launch
                        :project project :branch branch :worktree worktree
                        :body body :heading heading :file org-file
                        :headless t :base base)))
           ;; Set WORKTREE property if newly generated
           (unless worktree
             (org-entry-put nil "WORKTREE" (alist-get 'worktree_name result))
             (save-buffer))
           ;; Clock out OVERIGE for this client, clock in this ticket
           (kwrooijen/mcp--clock-out-overige org-file)
           (kwrooijen/mcp--clock-in-heading org-file heading)
           ;; Return result
           (json-encode
            `((success . t)
              (heading . ,heading)
              (project . ,(expand-file-name project))
              (branch . ,branch)
              (worktree . ,(alist-get 'worktree_name result))
              (worktree_path . ,(alist-get 'worktree_path result))
              (reused . ,(if (alist-get 'reused result) t :json-false))))))))))

(cl-defun kwrooijen/mcp-agent-list ()
  "List all ticket-managed agent-shell workspaces.
Returns a JSON array of agents that were started via org-agent-shell.
Each entry includes heading, file, project, branch, worktree, status,
and transcript file path."
  (let (entries)
    (dolist (buf (agent-shell-buffers))
      (when-let* ((info (buffer-local-value 'org-agent-shell--ticket-info buf)))
        (let* ((status (substring-no-properties
                        (agent-shell-manager--get-combined-status buf)))
               (transcript (when (buffer-live-p buf)
                             (buffer-local-value 'agent-shell--transcript-file buf))))
          (push `((heading . ,(alist-get 'heading info))
                  (file . ,(alist-get 'file info))
                  (project . ,(alist-get 'project info))
                  (branch . ,(alist-get 'branch info))
                  (worktree . ,(alist-get 'worktree info))
                  (worktree_path . ,(alist-get 'worktree_path info))
                  (status . ,status)
                  (transcript_file . ,transcript))
                entries))))
    (json-encode (nreverse entries))))

(cl-defun kwrooijen/mcp-agent-get-prompt (file heading)
  "Read the ticket.org prompt for the agent identified by FILE and HEADING."
  (let ((marker (kwrooijen/mcp--find-heading-marker file heading)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (let* ((project (org-agent-shell--get-property "PROJECT"))
              (worktree (org-entry-get nil "WORKTREE")))
         (unless project
           (error "No :PROJECT: property found"))
         (unless worktree
           (error "No :WORKTREE: property found"))
         (let* ((worktree-path
                 (expand-file-name
                  worktree
                  (file-name-concat (expand-file-name project)
                                    agent-shell-worktree--subdirectory)))
                (ticket-file (expand-file-name "ticket.org" worktree-path)))
           (unless (file-exists-p ticket-file)
             (error "ticket.org not found at %s" ticket-file))
           (json-encode
            `((heading . ,heading)
              (ticket_file . ,ticket-file)
              (content . ,(with-temp-buffer
                            (insert-file-contents ticket-file)
                            (buffer-string)))))))))))

(cl-defun kwrooijen/mcp-agent-get-transcript (file heading)
  "Read the transcript for the agent identified by FILE and HEADING.
Tries the running shell buffer first, falls back to the most recent
transcript file in the worktree."
  (let ((marker (kwrooijen/mcp--find-heading-marker file heading)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (let* ((project (org-agent-shell--get-property "PROJECT"))
              (worktree (org-entry-get nil "WORKTREE")))
         (unless project
           (error "No :PROJECT: property found"))
         (unless worktree
           (error "No :WORKTREE: property found"))
         (let* ((worktree-path
                 (expand-file-name
                  worktree
                  (file-name-concat (expand-file-name project)
                                    agent-shell-worktree--subdirectory)))
                (shell-buf (org-agent-shell--find-shell-buffer worktree-path))
                (transcript-file
                 (or
                  ;; Try the buffer-local variable first
                  (when shell-buf
                    (buffer-local-value 'agent-shell--transcript-file shell-buf))
                  ;; Fallback: most recent transcript file on disk
                  (let* ((transcripts-dir
                          (expand-file-name ".agent-shell/transcripts"
                                            worktree-path))
                         (files (and (file-directory-p transcripts-dir)
                                     (directory-files transcripts-dir t "\\.md$"))))
                    (when files
                      (car (sort files #'string>)))))))
           (unless transcript-file
             (error "No transcript found for this agent"))
           (unless (file-exists-p transcript-file)
             (error "Transcript file does not exist: %s" transcript-file))
           (json-encode
            `((heading . ,heading)
              (transcript_file . ,transcript-file)
              (content . ,(with-temp-buffer
                            (insert-file-contents transcript-file)
                            (buffer-string)))))))))))

(cl-defun kwrooijen/mcp-agent-get-diff (file heading)
  "Get the git diff for the agent's worktree identified by FILE and HEADING."
  (let ((marker (kwrooijen/mcp--find-heading-marker file heading)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (let* ((project (org-agent-shell--get-property "PROJECT"))
              (worktree (org-entry-get nil "WORKTREE"))
              (branch (org-entry-get nil "BRANCH"))
              (base (org-agent-shell--base-branch)))
         (unless project
           (error "No :PROJECT: property found"))
         (unless worktree
           (error "No :WORKTREE: property found"))
         (unless branch
           (error "No :BRANCH: property found"))
         (let* ((worktree-path
                 (expand-file-name
                  worktree
                  (file-name-concat (expand-file-name project)
                                    agent-shell-worktree--subdirectory)))
                (default-directory worktree-path)
                (range (format "%s...%s" base branch))
                (stat (shell-command-to-string
                       (format "git diff --stat %s" range)))
                (diff (shell-command-to-string
                       (format "git diff %s" range))))
           (json-encode
            `((heading . ,heading)
              (base . ,base)
              (branch . ,branch)
              (worktree_path . ,worktree-path)
              (stat . ,stat)
              (diff . ,diff)))))))))

(provide 'mcp)
;;; mcp.el ends here
