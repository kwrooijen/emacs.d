;;; org-clock-multi.el --- Multiple simultaneous org clocks -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: org, clock, time-tracking
;; URL: https://github.com/kwrooijen/org-clock-multi

;;; Commentary:

;; This package enables multiple simultaneous org clocks by maintaining
;; a shadow clock system alongside org-clock.  It reuses org-clock internals
;; for LOGBOOK writing to ensure compatibility.
;;
;; NOTE: Headings are identified by CLOCK_MULTI_ID and located by searching
;; `org-agenda-files'.  Clocking into headings outside of agenda files is
;; currently not supported.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-clock)
(require 'org-id)
(require 'org-agenda)

;;; Customization

(defgroup org-clock-multi nil
  "Multiple simultaneous org clocks."
  :group 'org-clock
  :prefix "org-clock-multi-")

(defcustom org-clock-multi-persist-file
  (expand-file-name "org-clock-multi/org-clock-multi-state.el"
                    user-emacs-directory)
  "File to persist clock state across Emacs restarts."
  :type 'file
  :group 'org-clock-multi)

;;; State Management

(defvar org-clock-multi-clocks nil
  "List of active clocks.
Each entry is a cons cell (ID . START-MINUTES) where ID is a
CLOCK_MULTI_ID string and START-MINUTES is the clock-in time in
minutes since epoch.")

(defvar org-clock-multi-paused nil
  "List of paused clock IDs.
Each entry is a CLOCK_MULTI_ID string for a task that was clocked out
but paused.")

(defun org-clock-multi-save-state ()
  "Save clock state to `org-clock-multi-persist-file'."
  (let ((dir (file-name-directory org-clock-multi-persist-file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (with-temp-file org-clock-multi-persist-file
    (insert ";; org-clock-multi state file. Do not edit.\n")
    (prin1 org-clock-multi-clocks (current-buffer))
    (insert "\n")
    (prin1 org-clock-multi-paused (current-buffer))
    (insert "\n")))

(defun org-clock-multi-load-state ()
  "Load clock state from `org-clock-multi-persist-file'."
  (when (file-exists-p org-clock-multi-persist-file)
    (with-temp-buffer
      (insert-file-contents org-clock-multi-persist-file)
      (goto-char (point-min))
      (let ((clocks (read (current-buffer)))
            (paused (read (current-buffer))))
        (setq org-clock-multi-clocks clocks)
        (setq org-clock-multi-paused paused)))))

;;; Core Functions

(defun org-clock-multi--get-or-create-id ()
  "Get or create a CLOCK_MULTI_ID property for the heading at point.
Returns the ID string."
  (org-back-to-heading t)
  (let ((id (org-entry-get nil "CLOCK_MULTI_ID")))
    (unless id
      (setq id (org-id-new))
      (org-entry-put nil "CLOCK_MULTI_ID" id))
    id))

(defun org-clock-multi--heading-key ()
  "Return a unique key for the heading at point.
Returns the CLOCK_MULTI_ID string."
  (save-excursion
    (org-back-to-heading t)
    (org-clock-multi--get-or-create-id)))

(defun org-clock-multi--find-clock-by-key (key)
  "Find clock entry by KEY in `org-clock-multi-clocks'.
KEY is a CLOCK_MULTI_ID string. Returns the clock cons cell or nil."
  (cl-find-if (lambda (clock)
                (equal key (car clock)))
              org-clock-multi-clocks))

(defun org-clock-multi--find-clock-at-point ()
  "Find clock entry for heading at point.
Returns the clock cons cell or nil."
  (let ((key (org-clock-multi--heading-key)))
    (org-clock-multi--find-clock-by-key key)))

(defun org-clock-multi-clocking-p ()
  "Return non-nil if heading at point is clocked in."
  (org-clock-multi--find-clock-at-point))

(defun org-clock-multi-paused-p ()
  "Return non-nil if heading at point is paused."
  (let ((key (org-clock-multi--heading-key)))
    (member key org-clock-multi-paused)))

(defun org-clock-multi--current-minutes ()
  "Return current time as minutes since epoch."
  (floor (/ (float-time (current-time)) 60)))

(defun org-clock-multi--minutes-to-time (minutes)
  "Convert MINUTES since epoch to an Emacs time value."
  (seconds-to-time (* minutes 60)))

(defun org-clock-multi-clock-in ()
  "Clock in the current heading.
Adds to the list of active clocks without affecting other clocks.
If the task was paused, removes it from the paused list."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((key (org-clock-multi--heading-key))
          (start-minutes (org-clock-multi--current-minutes)))
      (if (org-clock-multi--find-clock-by-key key)
          (message "Already clocked in to this task")
        ;; Remove from paused list if present
        (setq org-clock-multi-paused
              (cl-remove-if (lambda (k) (equal key k)) org-clock-multi-paused))
        (push (cons key start-minutes) org-clock-multi-clocks)
        (org-clock-multi-save-state)
        (message "Clocked in: %s" (org-get-heading t t t t))))))

(defun org-clock-multi--format-timestamp (time)
  "Format TIME as an inactive org timestamp."
  (format-time-string "[%Y-%m-%d %a %H:%M]" time))

(defmacro org-clock-multi--with-heading-at-key (key &rest body)
  "Execute BODY with point at the heading identified by KEY.
KEY is a CLOCK_MULTI_ID string. Searches `org-agenda-files' to locate
the heading. BODY is only executed if the heading is found.
Returns nil if the heading cannot be located."
  (declare (indent 1) (debug t))
  (let ((id-sym (gensym "id-"))
        (file-sym (gensym "file-"))
        (buf-sym (gensym "buf-"))
        (found-sym (gensym "found-")))
    `(let ((,id-sym ,key)
           (,found-sym nil))
       (cl-block nil
         (dolist (,file-sym (org-agenda-files))
           (when (file-exists-p ,file-sym)
             (let ((,buf-sym (or (find-buffer-visiting ,file-sym)
                                 (find-file-noselect ,file-sym))))
               (with-current-buffer ,buf-sym
                 (org-with-wide-buffer
                  (goto-char (point-min))
                  (when (re-search-forward
                         (format "^[ \t]*:CLOCK_MULTI_ID:[ \t]+%s[ \t]*$"
                                 (regexp-quote ,id-sym))
                         nil t)
                    (org-back-to-heading t)
                    (setq ,found-sym (progn ,@body))
                    (cl-return ,found-sym)))))))
         ,found-sym))))

(defun org-clock-multi--write-clock-entry (key start-minutes)
  "Write a LOGBOOK clock entry for heading at KEY with START-MINUTES.
KEY is a CLOCK_MULTI_ID string. START-MINUTES is minutes since epoch.
Uses org-clock's format for compatibility."
  (let* ((end-minutes (org-clock-multi--current-minutes))
         (start-time (org-clock-multi--minutes-to-time start-minutes))
         (end-time (org-clock-multi--minutes-to-time end-minutes))
         (duration (- end-minutes start-minutes)))
    (org-clock-multi--with-heading-at-key key
      (let ((org-clock-out-when-done nil))
        (org-clock-find-position nil)
        (insert-before-markers
         "CLOCK: "
         (org-clock-multi--format-timestamp start-time)
         "--"
         (org-clock-multi--format-timestamp end-time)
         " =>  "
         (org-duration-from-minutes duration)
         "\n")))))

(defun org-clock-multi--get-heading-for-key (key)
  "Get the heading text for KEY."
  (org-clock-multi--with-heading-at-key key
    (org-get-heading t t t t)))

(defun org-clock-multi--clock-out-internal ()
  "Clock out the heading at point, writing a LOGBOOK entry.
Returns (key . heading) on success, nil if not clocked in."
  (let ((clock (org-clock-multi--find-clock-at-point)))
    (when clock
      (let ((key (car clock))
            (start-time (cdr clock))
            (heading (org-get-heading t t t t)))
        (org-clock-multi--write-clock-entry key start-time)
        (setq org-clock-multi-clocks (delq clock org-clock-multi-clocks))
        (cons key heading)))))

(defun org-clock-multi-clock-out ()
  "Clock out heading at point.
Writes a LOGBOOK entry and removes from active clocks."
  (interactive)
  (save-excursion
    (let ((result (org-clock-multi--clock-out-internal)))
      (if result
          (progn
            (org-clock-multi-save-state)
            (message "Clocked out: %s" (cdr result)))
        (message "Not clocked in to this task")))))

(defun org-clock-multi-clock-out-all ()
  "Clock out all active clocks."
  (interactive)
  (if (null org-clock-multi-clocks)
      (message "No active clocks")
    (let ((count (length org-clock-multi-clocks)))
      (dolist (clock (copy-sequence org-clock-multi-clocks))
        (let ((key (car clock))
              (start-time (cdr clock)))
          (org-clock-multi--write-clock-entry key start-time)))
      (setq org-clock-multi-clocks nil)
      (org-clock-multi-save-state)
      (message "Clocked out %d task(s)" count))))

(defun org-clock-multi-clock-pause ()
  "Pause clock for heading at point.
Clocks out the task and adds it to the paused list for easy resumption."
  (interactive)
  (save-excursion
    (let ((result (org-clock-multi--clock-out-internal)))
      (if result
          (let ((key (car result)))
            (unless (member key org-clock-multi-paused)
              (push key org-clock-multi-paused))
            (org-clock-multi-save-state)
            (message "Paused: %s" (cdr result)))
        (message "Not clocked in to this task")))))

(defalias 'org-clock-multi-clock-resume #'org-clock-multi-clock-in
  "Resume a paused clock at point.
Equivalent to clock-in but explicitly for paused tasks.")

(defun org-clock-multi-clock-cancel ()
  "Cancel clock for heading at point without writing LOGBOOK."
  (interactive)
  (save-excursion
    (let* ((clock (org-clock-multi--find-clock-at-point)))
      (if (not clock)
          (message "Not clocked in to this task")
        (let ((heading (org-get-heading t t t t)))
          (setq org-clock-multi-clocks (delq clock org-clock-multi-clocks))
          (org-clock-multi-save-state)
          (message "Cancelled clock: %s" heading))))))

(defun org-clock-multi-clock-cancel-all ()
  "Cancel all active clocks without writing LOGBOOK entries."
  (interactive)
  (if (null org-clock-multi-clocks)
      (message "No active clocks")
    (let ((count (length org-clock-multi-clocks)))
      (setq org-clock-multi-clocks nil)
      (org-clock-multi-save-state)
      (message "Cancelled %d clock(s)" count))))

(defun org-clock-multi--format-clock-candidate (clock)
  "Format CLOCK as a candidate for `completing-read'.
Returns (label . key) or nil if the heading cannot be found."
  (let* ((key (car clock))
         (start-minutes (cdr clock))
         (elapsed (- (org-clock-multi--current-minutes) start-minutes))
         (heading (org-clock-multi--get-heading-for-key key)))
    (when heading
      (cons (format "%s (%s)"
                    heading
                    (org-duration-from-minutes elapsed))
            key))))

(defun org-clock-multi-clock-goto ()
  "Jump to one of the active clocks."
  (interactive)
  (if (null org-clock-multi-clocks)
      (message "No active clocks")
    (let* ((candidates (delq nil (mapcar #'org-clock-multi--format-clock-candidate
                                         org-clock-multi-clocks)))
           (selected (completing-read "Clock: " candidates nil t)))
      (when selected
        (let ((key (cdr (assoc selected candidates))))
          (org-clock-multi--with-heading-at-key key
            (switch-to-buffer (current-buffer))
            (org-show-entry)
            (org-reveal)))))))

;;; Agenda Integration

(defmacro org-clock-multi--agenda-with-heading (&rest body)
  "Execute BODY at the agenda heading, then refresh the agenda.
Extracts the heading marker from the current agenda line."
  (declare (indent 0) (debug t))
  `(progn
     (org-agenda-check-no-diary)
     (let* ((marker (or (org-get-at-bol 'org-marker)
                        (org-agenda-error)))
            (hdmarker (or (org-get-at-bol 'org-hd-marker) marker)))
       (with-current-buffer (marker-buffer hdmarker)
         (save-excursion
           (goto-char hdmarker)
           ,@body)))
     (org-agenda-redo-all)))

(defun org-clock-multi-agenda-clock-in ()
  "Clock in the task at point in agenda."
  (interactive)
  (org-clock-multi--agenda-with-heading (org-clock-multi-clock-in)))

(defun org-clock-multi-agenda-clock-out ()
  "Clock out the task at point in agenda."
  (interactive)
  (org-clock-multi--agenda-with-heading (org-clock-multi-clock-out)))

(defun org-clock-multi-agenda-clock-out-all ()
  "Clock out all active clocks from agenda."
  (interactive)
  (org-clock-multi-clock-out-all)
  (org-agenda-redo-all))

(defun org-clock-multi-agenda-clock-pause ()
  "Pause clock for task at point in agenda."
  (interactive)
  (org-clock-multi--agenda-with-heading (org-clock-multi-clock-pause)))

(defun org-clock-multi-agenda-clock-cancel ()
  "Cancel clock for task at point in agenda."
  (interactive)
  (org-clock-multi--agenda-with-heading (org-clock-multi-clock-cancel)))

(defun org-clock-multi-agenda-clock-cancel-all ()
  "Cancel all active clocks from agenda."
  (interactive)
  (org-clock-multi-clock-cancel-all)
  (org-agenda-redo-all))

;;; Agenda Highlighting

(defun org-clock-multi--agenda-fontify-clocked ()
  "Apply `org-agenda-clocking' face to all multi-clocked entries in agenda."
  (when org-clock-multi-clocks
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((marker (or (org-get-at-bol 'org-hd-marker)
                          (org-get-at-bol 'org-marker))))
          (when marker
            (let ((id (with-current-buffer (marker-buffer marker)
                        (save-excursion
                          (goto-char marker)
                          (org-back-to-heading t)
                          (org-entry-get nil "CLOCK_MULTI_ID")))))
              (when (and id (org-clock-multi--find-clock-by-key id))
                (let ((bol (line-beginning-position))
                      (eol (line-end-position)))
                  (add-text-properties bol eol
                                       '(face org-agenda-clocking)))))))
        (forward-line 1)))))

(add-hook 'org-agenda-finalize-hook #'org-clock-multi--agenda-fontify-clocked)

;;; Initialization

(defun org-clock-multi-initialize ()
  "Initialize org-clock-multi, loading persisted state."
  (org-clock-multi-load-state))

;; Auto-initialize when loaded
(org-clock-multi-initialize)

(provide 'org-clock-multi)
;;; org-clock-multi.el ends here
