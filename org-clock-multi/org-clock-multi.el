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
Each entry is a cons cell (MARKER . START-TIME) where MARKER points
to the heading and START-TIME is the clock-in time.")

(defun org-clock-multi--marker-to-persistent (marker)
  "Convert MARKER to a persistent format (file . position)."
  (when (and marker (marker-buffer marker))
    (cons (buffer-file-name (marker-buffer marker))
          (marker-position marker))))

(defun org-clock-multi--persistent-to-marker (persistent)
  "Convert PERSISTENT format (file . position) back to a marker."
  (when persistent
    (let ((file (car persistent))
          (pos (cdr persistent)))
      (when (and file (file-exists-p file))
        (let ((buf (or (find-buffer-visiting file)
                       (find-file-noselect file))))
          (with-current-buffer buf
            (save-excursion
              (goto-char pos)
              (point-marker))))))))

(defun org-clock-multi-save-state ()
  "Save clock state to `org-clock-multi-persist-file'."
  (let ((dir (file-name-directory org-clock-multi-persist-file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (let ((persistent-clocks
         (delq nil
               (mapcar (lambda (clock)
                         (let ((persistent-marker (org-clock-multi--marker-to-persistent (car clock))))
                           (when persistent-marker
                             (cons persistent-marker (cdr clock)))))
                       org-clock-multi-clocks))))
    (with-temp-file org-clock-multi-persist-file
      (insert ";;; org-clock-multi state file -*- lexical-binding: t; -*-\n")
      (insert ";; This file is auto-generated. Do not edit.\n\n")
      (prin1 `(setq org-clock-multi--persistent-state ',persistent-clocks)
             (current-buffer))
      (insert "\n"))))

(defun org-clock-multi-load-state ()
  "Load clock state from `org-clock-multi-persist-file'."
  (when (file-exists-p org-clock-multi-persist-file)
    (load org-clock-multi-persist-file nil t)
    (when (boundp 'org-clock-multi--persistent-state)
      (setq org-clock-multi-clocks
            (delq nil
                  (mapcar (lambda (persistent-clock)
                            (let ((marker (org-clock-multi--persistent-to-marker (car persistent-clock))))
                              (when marker
                                (cons marker (cdr persistent-clock)))))
                          org-clock-multi--persistent-state)))
      (makunbound 'org-clock-multi--persistent-state))))

;;; Core Functions

(defun org-clock-multi--find-clock (marker)
  "Find clock entry for MARKER in `org-clock-multi-clocks'.
Returns the clock cons cell or nil."
  (cl-find-if (lambda (clock)
                (and (marker-buffer (car clock))
                     (marker-buffer marker)
                     (eq (marker-buffer (car clock)) (marker-buffer marker))
                     (= (marker-position (car clock)) (marker-position marker))))
              org-clock-multi-clocks))

(defun org-clock-multi--heading-marker ()
  "Return a marker at the current heading."
  (save-excursion
    (org-back-to-heading t)
    (point-marker)))

(defun org-clock-multi-clocking-p (&optional marker)
  "Return non-nil if MARKER (or current heading) is clocked in.
If MARKER is nil, check the heading at point."
  (let ((m (or marker (org-clock-multi--heading-marker))))
    (org-clock-multi--find-clock m)))

(defun org-clock-multi-clock-in ()
  "Clock in the current heading.
Adds to the list of active clocks without affecting other clocks."
  (interactive)
  (org-back-to-heading t)
  (let ((marker (point-marker))
        (start-time (current-time)))
    (if (org-clock-multi--find-clock marker)
        (message "Already clocked in to this task")
      (push (cons marker start-time) org-clock-multi-clocks)
      (org-clock-multi-save-state)
      (message "Clocked in: %s" (org-get-heading t t t t)))))

(defun org-clock-multi--format-timestamp (time)
  "Format TIME as an inactive org timestamp."
  (format-time-string "[%Y-%m-%d %a %H:%M]" time))

(defun org-clock-multi--write-clock-entry (marker start-time)
  "Write a LOGBOOK clock entry for MARKER with START-TIME.
Uses org-clock's format for compatibility."
  (let ((end-time (current-time)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (org-back-to-heading t)
        ;; Use org-clock's function to find the right position
        (let ((org-clock-out-when-done nil))
          ;; Find or create LOGBOOK drawer
          (org-clock-find-position nil)
          ;; Insert the clock line in org-clock's exact format
          (insert-before-markers
           "CLOCK: "
           (org-clock-multi--format-timestamp start-time)
           "--"
           (org-clock-multi--format-timestamp end-time)
           " => "
           (org-duration-from-minutes
            (floor (/ (float-time (time-subtract end-time start-time)) 60)))
           "\n"))))))

(defun org-clock-multi-clock-out (&optional marker)
  "Clock out MARKER (or heading at point).
Writes a LOGBOOK entry and removes from active clocks."
  (interactive)
  (let* ((m (or marker (org-clock-multi--heading-marker)))
         (clock (org-clock-multi--find-clock m)))
    (if (not clock)
        (message "Not clocked in to this task")
      (let ((start-time (cdr clock))
            (heading (with-current-buffer (marker-buffer (car clock))
                       (save-excursion
                         (goto-char (car clock))
                         (org-get-heading t t t t)))))
        (org-clock-multi--write-clock-entry (car clock) start-time)
        (setq org-clock-multi-clocks (delq clock org-clock-multi-clocks))
        (org-clock-multi-save-state)
        (message "Clocked out: %s" heading)))))

(defun org-clock-multi-clock-out-all ()
  "Clock out all active clocks."
  (interactive)
  (if (null org-clock-multi-clocks)
      (message "No active clocks")
    (let ((count (length org-clock-multi-clocks)))
      (dolist (clock (copy-sequence org-clock-multi-clocks))
        (org-clock-multi-clock-out (car clock)))
      (message "Clocked out %d task(s)" count))))

(defun org-clock-multi-clock-cancel (&optional marker)
  "Cancel clock for MARKER (or heading at point) without writing LOGBOOK."
  (interactive)
  (let* ((m (or marker (org-clock-multi--heading-marker)))
         (clock (org-clock-multi--find-clock m)))
    (if (not clock)
        (message "Not clocked in to this task")
      (let ((heading (with-current-buffer (marker-buffer (car clock))
                       (save-excursion
                         (goto-char (car clock))
                         (org-get-heading t t t t)))))
        (setq org-clock-multi-clocks (delq clock org-clock-multi-clocks))
        (org-clock-multi-save-state)
        (message "Cancelled clock: %s" heading)))))

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
            (mapcar (lambda (clock)
                      (let* ((marker (car clock))
                             (start-time (cdr clock))
                             (elapsed (floor (/ (float-time
                                                 (time-subtract (current-time) start-time))
                                                60)))
                             (heading (with-current-buffer (marker-buffer marker)
                                        (save-excursion
                                          (goto-char marker)
                                          (org-get-heading t t t t))))
                             (file (buffer-file-name (marker-buffer marker))))
                        (cons (format "%s (%s) [%s]"
                                      heading
                                      (org-duration-from-minutes elapsed)
                                      (file-name-nondirectory file))
                              marker)))
                    org-clock-multi-clocks))
           (selected (completing-read "Clock: " candidates nil t)))
      (when selected
        (let ((marker (cdr (assoc selected candidates))))
          (switch-to-buffer (marker-buffer marker))
          (goto-char marker)
          (org-show-entry)
          (org-reveal))))))

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
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (hdmarker (or (org-get-at-bol 'org-hd-marker) marker)))
    (with-current-buffer (marker-buffer hdmarker)
      (save-excursion
        (goto-char hdmarker)
        (org-clock-multi-clock-cancel))))
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
            (let ((clocked (cl-find-if
                            (lambda (clock)
                              (and (marker-buffer (car clock))
                                   (marker-buffer marker)
                                   (eq (marker-buffer (car clock))
                                       (marker-buffer marker))
                                   (= (marker-position (car clock))
                                      (marker-position marker))))
                            org-clock-multi-clocks)))
              (when clocked
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
