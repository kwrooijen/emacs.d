;;; org-todo-dashboard.el --- Dashboard agenda view for multi-client workflows -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0") (org-clock-multi "0.1.0"))
;; Keywords: org, agenda, dashboard

;;; Commentary:

;; Provides a custom org-agenda dashboard view with two sections:
;; - Active clocks (from org-clock-multi)
;; - Work tasks grouped by client, sorted by priority, limited to N per group
;;
;; Clients are discovered dynamically from .org files in a configurable directory.

;;; Code:

(require 'org-agenda)
(require 'org-clock-multi)

;;; Customization

(defgroup org-todo-dashboard nil
  "Dashboard agenda view for multi-client workflows."
  :group 'org-agenda
  :prefix "org-todo-dashboard-")

(defcustom org-todo-dashboard-client-directory
  (expand-file-name "~/Documents/org/todos/client/")
  "Directory containing client org files."
  :type 'directory
  :group 'org-todo-dashboard)

(defcustom org-todo-dashboard-max-entries 3
  "Maximum number of tasks to show per client group."
  :type 'integer
  :group 'org-todo-dashboard)

(defcustom org-todo-dashboard-key "d"
  "Key for the dashboard in `org-agenda-custom-commands'."
  :type 'string
  :group 'org-todo-dashboard)

;;; Core Functions

(defun org-todo-dashboard--client-names ()
  "Return a list of client names from org files in the client directory."
  (when (file-directory-p org-todo-dashboard-client-directory)
    (mapcar #'file-name-base
            (directory-files org-todo-dashboard-client-directory nil "\\.org$"))))

(defun org-todo-dashboard--format-client-header (client)
  "Format CLIENT name as a readable header."
  (capitalize (replace-regexp-in-string "_" " " client)))

(defun org-todo-dashboard--separator ()
  "Return a separator line sized to the current window. Divide window-width
to account for the character width of the separator."
  (concat (make-string (floor (/ (window-width) 1.7)) ?┄)
          "\n"))

(defun org-todo-dashboard--client-blocks ()
  "Build a list of agenda blocks, one per client."
  (mapcar
   (lambda (client)
     `(tags-todo ,client
                 ((org-agenda-overriding-header
                   ,(lambda ()
                      (concat (org-todo-dashboard--separator)
                              "\nClient: "
                              (org-todo-dashboard--format-client-header client)
                              "\n")))
                  (org-agenda-max-entries ,org-todo-dashboard-max-entries)
                  (org-agenda-sorting-strategy '(priority-down)))))
   (org-todo-dashboard--client-names)))

(defun org-todo-dashboard-build-agenda-command ()
  "Build the agenda command entry for the dashboard."
  `(,org-todo-dashboard-key "Dashboard"
    ((alltodo ""
              ((org-agenda-overriding-header "Active Clocks")
               (org-agenda-block-separator "")
               (org-agenda-skip-function
                #'kwrooijen/org-clock-multi-skip-unless-clocked-or-paused)))
     ,@(org-todo-dashboard--client-blocks))
    ((org-agenda-block-separator ""))))

(defun org-todo-dashboard-install ()
  "Install the dashboard into `org-agenda-custom-commands'.
Replaces any existing entry with the same key."
  (setq org-agenda-custom-commands
        (cons (org-todo-dashboard-build-agenda-command)
              (cl-remove-if (lambda (cmd)
                              (equal (car cmd) org-todo-dashboard-key))
                            org-agenda-custom-commands))))

(defun org-todo-dashboard-open ()
  "Open the org-agenda dashboard view."
  (interactive)
  (org-todo-dashboard-install)
  (org-agenda nil org-todo-dashboard-key))

(provide 'org-todo-dashboard)
;;; org-todo-dashboard.el ends here
