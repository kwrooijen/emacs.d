;;; org-clock-multi.el --- Multiple simultaneous org clocks -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0") (consult "0.35"))
;; Keywords: org, clock, time-tracking
;; URL: https://github.com/kwrooijen/org-clock-multi

;;; Commentary:

;; This package enables multiple simultaneous org clocks by maintaining
;; a shadow clock system alongside org-clock.  It reuses org-clock internals
;; for LOGBOOK writing to ensure compatibility.

;;; Code:

(require 'org)
(require 'org-clock)
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
Each entry is a cons cell (KEY . START-MINUTES) where KEY is (file . id)
and START-MINUTES is the clock-in time in minutes since epoch.")

(defun org-clock-multi-save-state ()
  "Save clock state to `org-clock-multi-persist-file'."
  (let ((dir (file-name-directory org-clock-multi-persist-file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  ;; Keys are already in persistent format (file . id)
  (with-temp-file org-clock-multi-persist-file
    (insert ";;; org-clock-multi state file -*- lexical-binding: t; -*-\n")
    (insert ";; This file is auto-generated. Do not edit.\n\n")
    (prin1 `(setq org-clock-multi--persistent-state ',org-clock-multi-clocks)
           (current-buffer))
    (insert "\n")))

(defun org-clock-multi-load-state ()
  "Load clock state from `org-clock-multi-persist-file'."
  (when (file-exists-p org-clock-multi-persist-file)
    (load org-clock-multi-persist-file nil t)
    (when (boundp 'org-clock-multi--persistent-state)
      ;; Filter out clocks for files that no longer exist
      (setq org-clock-multi-clocks
            (delq nil
                  (mapcar (lambda (clock)
                            (let ((file (car (car clock))))
                              (when (and file (file-exists-p file))
                                clock)))
                          org-clock-multi--persistent-state)))
      (makunbound 'org-clock-multi--persistent-state))))

;;; Core Functions

(defun org-clock-multi--generate-uuid ()
  "Generate a UUID string."
  (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (logior (logand (random 65536) 4095) 16384)
          (logior (logand (random 65536) 16383) 32768)
          (random 65536) (random 65536) (random 65536)))

(defun org-clock-multi--get-or-create-id ()
  "Get or create a CLOCK_MULTI_ID property for the heading at point.
Returns the ID string."
  (org-back-to-heading t)
  (let ((id (org-entry-get nil "CLOCK_MULTI_ID")))
    (unless id
      (setq id (org-clock-multi--generate-uuid))
      (org-entry-put nil "CLOCK_MULTI_ID" id))
    id))

(defun org-clock-multi--get-id-at-marker (marker)
  "Get the CLOCK_MULTI_ID for the heading at MARKER.
Returns nil if marker is invalid or no ID exists."
  (when (and marker (marker-buffer marker))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (org-back-to-heading t)
        (org-entry-get nil "CLOCK_MULTI_ID")))))

(defun org-clock-multi--heading-key ()
  "Return a unique key for the heading at point.
Returns (file . id) cons cell."
  (save-excursion
    (org-back-to-heading t)
    (cons (buffer-file-name) (org-clock-multi--get-or-create-id))))

(defun org-clock-multi--find-clock-by-key (key)
  "Find clock entry by KEY in `org-clock-multi-clocks'.
KEY is (file . id). Returns the clock cons cell or nil."
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

(defun org-clock-multi--current-minutes ()
  "Return current time as minutes since epoch."
  (floor (/ (float-time (current-time)) 60)))

(defun org-clock-multi--minutes-to-time (minutes)
  "Convert MINUTES since epoch to an Emacs time value."
  (seconds-to-time (* minutes 60)))

(defun org-clock-multi-clock-in ()
  "Clock in the current heading.
Adds to the list of active clocks without affecting other clocks."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((key (org-clock-multi--heading-key))
          (start-minutes (org-clock-multi--current-minutes)))
      (if (org-clock-multi--find-clock-by-key key)
          (message "Already clocked in to this task")
        (push (cons key start-minutes) org-clock-multi-clocks)
        (org-clock-multi-save-state)
        (message "Clocked in: %s" (org-get-heading t t t t))))))

(defun org-clock-multi--format-timestamp (time)
  "Format TIME as an inactive org timestamp."
  (format-time-string "[%Y-%m-%d %a %H:%M]" time))

(defun org-clock-multi--goto-key (key)
  "Go to the heading identified by KEY.
KEY is (file . id). Returns t if found, nil otherwise."
  (let ((file (car key))
        (id (cdr key)))
    (when (and file (file-exists-p file))
      (let ((buf (or (find-buffer-visiting file)
                     (find-file-noselect file))))
        (with-current-buffer buf
          (org-with-wide-buffer
           (goto-char (point-min))
           (when (re-search-forward
                  (format "^[ \t]*:CLOCK_MULTI_ID:[ \t]+%s[ \t]*$" (regexp-quote id))
                  nil t)
             (org-back-to-heading t)
             t)))))))

(defun org-clock-multi--write-clock-entry (key start-minutes)
  "Write a LOGBOOK clock entry for heading at KEY with START-MINUTES.
KEY is (file . id). START-MINUTES is minutes since epoch.
Uses org-clock's format for compatibility."
  (let* ((end-minutes (org-clock-multi--current-minutes))
         (start-time (org-clock-multi--minutes-to-time start-minutes))
         (end-time (org-clock-multi--minutes-to-time end-minutes))
         (duration (- end-minutes start-minutes))
         (file (car key))
         (id (cdr key)))
    (when (and file (file-exists-p file))
      (let ((buf (or (find-buffer-visiting file)
                     (find-file-noselect file))))
        (with-current-buffer buf
          (org-with-wide-buffer
           (goto-char (point-min))
           (when (re-search-forward
                  (format "^[ \t]*:CLOCK_MULTI_ID:[ \t]+%s[ \t]*$" (regexp-quote id))
                  nil t)
             (org-back-to-heading t)
             ;; Use org-clock's function to find the right position
             (let ((org-clock-out-when-done nil))
               ;; Find or create LOGBOOK drawer
               (org-clock-find-position nil)
               ;; Insert the clock line in org-clock's exact format
               ;; Note: org-clock uses two spaces after "=>"
               (insert-before-markers
                "CLOCK: "
                (org-clock-multi--format-timestamp start-time)
                "--"
                (org-clock-multi--format-timestamp end-time)
                " =>  "
                (org-duration-from-minutes duration)
                "\n")))))))))

(defun org-clock-multi--get-heading-for-key (key)
  "Get the heading text for KEY."
  (let ((file (car key))
        (id (cdr key)))
    (when (and file (file-exists-p file))
      (let ((buf (or (find-buffer-visiting file)
                     (find-file-noselect file))))
        (with-current-buffer buf
          (org-with-wide-buffer
           (goto-char (point-min))
           (when (re-search-forward
                  (format "^[ \t]*:CLOCK_MULTI_ID:[ \t]+%s[ \t]*$" (regexp-quote id))
                  nil t)
             (org-back-to-heading t)
             (org-get-heading t t t t))))))))

(defun org-clock-multi-clock-out ()
  "Clock out heading at point.
Writes a LOGBOOK entry and removes from active clocks."
  (interactive)
  (save-excursion
    (let* ((clock (org-clock-multi--find-clock-at-point)))
      (if (not clock)
          (message "Not clocked in to this task")
        (let ((key (car clock))
              (start-time (cdr clock))
              (heading (org-get-heading t t t t)))
          (org-clock-multi--write-clock-entry key start-time)
          (setq org-clock-multi-clocks (delq clock org-clock-multi-clocks))
          (org-clock-multi-save-state)
          (message "Clocked out: %s" heading))))))

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

(defun org-clock-multi-clock-goto ()
  "Jump to one of the active clocks."
  (interactive)
  (if (null org-clock-multi-clocks)
      (message "No active clocks")
    (let* ((candidates
            (delq nil
                  (mapcar (lambda (clock)
                            (let* ((key (car clock))
                                   (start-minutes (cdr clock))
                                   (elapsed (- (org-clock-multi--current-minutes) start-minutes))
                                   (heading (org-clock-multi--get-heading-for-key key))
                                   (file (car key)))
                              (when heading
                                (cons (format "%s (%s) [%s]"
                                              heading
                                              (org-duration-from-minutes elapsed)
                                              (file-name-nondirectory file))
                                      key))))
                          org-clock-multi-clocks)))
           (selected (completing-read "Clock: " candidates nil t)))
      (when selected
        (let* ((key (cdr (assoc selected candidates)))
               (file (car key))
               (id (cdr key)))
          (find-file file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (when (re-search-forward
                  (format "^[ \t]*:CLOCK_MULTI_ID:[ \t]+%s[ \t]*$" (regexp-quote id))
                  nil t)
             (org-back-to-heading t)
             (org-show-entry)
             (org-reveal))))))))

;;; Agenda Integration

(defun org-clock-multi-agenda-clock-in ()
  "Clock in the task at point in agenda."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (hdmarker (or (org-get-at-bol 'org-hd-marker) marker)))
    (with-current-buffer (marker-buffer hdmarker)
      (save-excursion
        (goto-char hdmarker)
        (org-clock-multi-clock-in))))
  (org-agenda-redo-all))

(defun org-clock-multi-agenda-clock-out ()
  "Clock out the task at point in agenda."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (hdmarker (or (org-get-at-bol 'org-hd-marker) marker)))
    (with-current-buffer (marker-buffer hdmarker)
      (save-excursion
        (goto-char hdmarker)
        (org-clock-multi-clock-out))))
  (org-agenda-redo-all))

(defun org-clock-multi-agenda-clock-out-all ()
  "Clock out all active clocks from agenda."
  (interactive)
  (org-clock-multi-clock-out-all)
  (org-agenda-redo-all))

(defun org-clock-multi-agenda-clock-cancel ()
  "Cancel clock for task at point in agenda."
  (interactive)
  (org-clock-multi--with-agenda-heading
   (org-clock-multi-clock-cancel))
  (org-agenda-redo-all))

(defun org-clock-multi-agenda-clock-cancel-all ()
  "Cancel all active clocks from agenda."
  (interactive)
  (org-clock-multi-clock-cancel-all)
  (org-agenda-redo-all))

(defun org-clock-multi-agenda-clock-goto ()
  "Jump to one of the active clocks from agenda using consult."
  (interactive)
  (org-clock-multi-clock-goto))

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
            (let ((key (with-current-buffer (marker-buffer marker)
                         (save-excursion
                           (goto-char marker)
                           (org-back-to-heading t)
                           (let ((id (org-entry-get nil "CLOCK_MULTI_ID")))
                             (when id
                               (cons (buffer-file-name) id)))))))
              (when (and key (org-clock-multi--find-clock-by-key key))
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
