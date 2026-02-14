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
;; Export functions consume this data to produce CSV, etc.

;;; Code:

(require 'org)

;;; Extraction

(defconst org-clock-data--clock-re
  (concat "^[ \t]*CLOCK: "
          "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) [A-Za-z]\\{2,3\\} "
          "[0-9]\\{2\\}:[0-9]\\{2\\}\\]--"
          "\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{2,3\\} "
          "[0-9]\\{2\\}:[0-9]\\{2\\}\\] => [ ]*"
          "\\([0-9]+\\):\\([0-9]\\{2\\}\\)")
  "Regexp matching org CLOCK lines, capturing date, hours, and minutes.")

(cl-defun org-clock-data-extract (&key tag)
  "Extract clock entries from `org-agenda-files'.
Returns a list of plists with :date, :tags, :hours, :minutes.
When TAG is non-nil, only include entries under headings with that tag.
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
             (save-excursion
               (org-back-to-heading t)
               (let ((tags (org-get-tags)))
                 (when (or (null tag)
                           (member tag tags))
                   (push (list :date date
                               :tags tags
                               :hours hours
                               :minutes minutes)
                         rows)))))))))
    (sort rows (lambda (a b)
                 (string< (plist-get a :date) (plist-get b :date))))))

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
