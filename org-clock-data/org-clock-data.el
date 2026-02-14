;;; org-clock-data.el --- Extract and export org clock data -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: org, clock, data

;;; Commentary:

;; Extract clock data from org agenda files and export to various formats.
;;
;; Extraction returns a list of plists:
;;   (:date "2025-12-01" :tags ("work" "tool2match") :hours 1 :minutes 7)
;;
;; Export functions consume this data to produce CSV, bar charts, etc.

;;; Code:

(require 'org)
(require 'cl-lib)

;;; Extraction

(defconst org-clock-data--clock-re
  (concat "^[ \t]*CLOCK: "
          "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) [A-Za-z]\\{2,3\\} "
          "[0-9]\\{2\\}:[0-9]\\{2\\}\\]--"
          "\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{2,3\\} "
          "[0-9]\\{2\\}:[0-9]\\{2\\}\\] => [ ]*"
          "\\([0-9]+\\):\\([0-9]\\{2\\}\\)")
  "Regexp matching org CLOCK lines, capturing date, hours, and minutes.")

(cl-defun org-clock-data-extract (&key tag from to)
  "Extract clock entries from `org-agenda-files'.
Returns a list of plists with :date, :tags, :hours, :minutes.
When TAG is non-nil, only include entries under headings with that tag.
FROM and TO are date strings (YYYY-MM-DD) for filtering (inclusive).
Results are sorted by date ascending."
  (let (rows)
    (dolist (file (org-agenda-files))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward org-clock-data--clock-re nil t)
           (let ((date (match-string 1))
                 (hours (string-to-number (match-string 2)))
                 (minutes (string-to-number (match-string 3))))
             (when (and (or (null from) (org-string<= from date))
                        (or (null to) (org-string<= date to)))
               (save-excursion
                 (org-back-to-heading t)
                 (let ((tags (org-get-tags)))
                   (when (or (null tag)
                             (member tag tags))
                     (push (list :date date
                                 :tags tags
                                 :hours hours
                                 :minutes minutes)
                           rows))))))))))
    (sort rows (lambda (a b)
                 (string< (plist-get a :date) (plist-get b :date))))))

;;; Aggregation

(defun org-clock-data-aggregate-by-date (entries)
  "Aggregate ENTRIES by date, summing total minutes.
Returns an alist of (DATE . TOTAL-MINUTES) sorted by date."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let ((date (plist-get entry :date))
            (mins (+ (* (plist-get entry :hours) 60)
                     (plist-get entry :minutes))))
        (puthash date (+ (gethash date table 0) mins) table)))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) table)
      (sort result (lambda (a b) (string< (car a) (car b)))))))

;;; Bar Chart

(defcustom org-clock-data-bar-char ?█
  "Character used to draw bar chart bars."
  :type 'character
  :group 'org-clock-data)

(defcustom org-clock-data-bar-max-width 30
  "Maximum width of a bar in characters."
  :type 'integer
  :group 'org-clock-data)

(defcustom org-clock-data-bar-target-hours 8.0
  "Target hours per day. The bar is full at this value."
  :type 'float
  :group 'org-clock-data)

(defun org-clock-data--format-date-short (date-str)
  "Format DATE-STR (YYYY-MM-DD) as a short label like Mon 02/10."
  (let ((time (encode-time 0 0 0
                           (string-to-number (substring date-str 8 10))
                           (string-to-number (substring date-str 5 7))
                           (string-to-number (substring date-str 0 4)))))
    (format-time-string "%a %m/%d" time)))

(defun org-clock-data-bar-chart (aggregated)
  "Render AGGREGATED data (from `org-clock-data-aggregate-by-date') as a bar chart string."
  (if (null aggregated)
      "  No clock data for this period.\n"
    (let* ((target-mins (* org-clock-data-bar-target-hours 60))
           (lines
            (mapcar
             (lambda (pair)
               (let* ((date (car pair))
                      (mins (cdr pair))
                      (hours (/ mins 60))
                      (remaining-mins (% mins 60))
                      (ratio (min 1.0 (/ (float mins) target-mins)))
                      (filled (round (* org-clock-data-bar-max-width ratio)))
                      (label (org-clock-data--format-date-short date)))
                 (format "  %s  %s %dh%02dm"
                         label
                         (make-string filled org-clock-data-bar-char)
                         hours remaining-mins)))
             aggregated)))
      (concat (string-join lines "\n") "\n"))))

;;; Agenda Integration

(defvar org-clock-data-agenda-chart-mode nil
  "Non-nil when bar chart mode is active in the agenda.")

(defun org-clock-data--agenda-date-range ()
  "Return (FROM . TO) date strings for the current agenda view."
  (when (and (boundp 'org-starting-day) org-starting-day
             (boundp 'org-agenda-current-span) org-agenda-current-span)
    (let* ((start-day org-starting-day)
           (span org-agenda-current-span)
           (ndays (org-agenda-span-to-ndays span start-day))
           (end-day (+ start-day ndays -1))
           (start-time (calendar-gregorian-from-absolute start-day))
           (end-time (calendar-gregorian-from-absolute end-day))
           (from (format "%04d-%02d-%02d"
                         (nth 2 start-time) (nth 0 start-time) (nth 1 start-time)))
           (to (format "%04d-%02d-%02d"
                       (nth 2 end-time) (nth 0 end-time) (nth 1 end-time))))
      (cons from to))))

(defun org-clock-data-agenda-insert-chart ()
  "Insert a bar chart into the agenda buffer when chart mode is active."
  (when (and org-clock-data-agenda-chart-mode
             (derived-mode-p 'org-agenda-mode))
    (let ((range (org-clock-data--agenda-date-range)))
      (when range
        (let* ((from (car range))
               (to (cdr range))
               (entries (org-clock-data-extract :tag "work" :from from :to to))
               (aggregated (org-clock-data-aggregate-by-date entries))
               (chart (org-clock-data-bar-chart aggregated))
               (inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (insert "\n"
                    (propertize "Clock Chart" 'face 'org-agenda-structure)
                    "\n"
                    chart)))))))

(add-hook 'org-agenda-finalize-hook #'org-clock-data-agenda-insert-chart)

(defun org-clock-data-agenda-chart-mode ()
  "Toggle bar chart mode in the agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-clock-data-agenda-chart-mode (not org-clock-data-agenda-chart-mode))
  (org-agenda-redo)
  (message "Clock chart mode is %s"
           (if org-clock-data-agenda-chart-mode "on" "off")))

;;; CSV Export

(defun org-clock-data-to-csv (entries)
  "Convert ENTRIES (from `org-clock-data-extract') to a CSV string."
  (concat "time,tags,hours,minutes\n"
          (mapconcat
           (lambda (entry)
             (format "%s,%s,%d,%d"
                     (plist-get entry :date)
                     (string-join (plist-get entry :tags) ":")
                     (plist-get entry :hours)
                     (plist-get entry :minutes)))
           entries "\n")))

(defun org-clock-data-export-csv (&optional filter-tag)
  "Export clock data as CSV to a buffer.
FILTER-TAG defaults to \"work\"."
  (interactive (list "work"))
  (let* ((entries (org-clock-data-extract :tag filter-tag))
         (csv (org-clock-data-to-csv entries)))
    (with-current-buffer (get-buffer-create "*org-clock-csv*")
      (erase-buffer)
      (insert csv)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer))
      (message "Exported %d clock entries" (length entries)))))

(provide 'org-clock-data)
;;; org-clock-data.el ends here
