;;; org-clock-data.el --- Extract and export org clock data -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: org, clock, data

;;; Commentary:

;; Extract clock data from org agenda files and export to various formats.
;;
;; Extraction returns a list of plists:
;;   (:date "2025-12-01" :tags ("work" "company1") :hours 1 :minutes 7)
;;
;; Export functions consume this data to produce CSV, bar charts, etc.

;;; Code:

(require 'org)
(require 'cl-lib)

;;; Extraction

(defconst org-clock-data--clock-re
  (concat "^[ \t]*CLOCK: "
          "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) [A-Za-z]\\{2,3\\} "
          "\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\]--"
          "\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{2,3\\} "
          "\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\] => [ ]*"
          "\\([0-9]+\\):\\([0-9]\\{2\\}\\)")
  "Regexp matching org CLOCK lines.
Capture groups: 1=date, 2=start-hour, 3=start-min,
4=end-hour, 5=end-min, 6=duration-hours, 7=duration-minutes.")

(cl-defun org-clock-data-extract (&key tag from to)
  "Extract clock entries from `org-agenda-files'.
Returns a list of plists with :date, :tags, :hours, :minutes,
:start-hh, :start-mm, :end-hh, :end-mm.
When TAG is non-nil, only include entries under headings with that tag.
FROM and TO are date strings (YYYY-MM-DD) for filtering (inclusive).
Includes currently running `org-clock-multi' clocks.
Results are sorted by date ascending."
  (let (rows)
    ;; Completed clock entries from LOGBOOK
    (dolist (file (org-agenda-files))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward org-clock-data--clock-re nil t)
           (let ((date (match-string 1))
                 (start-hh (string-to-number (match-string 2)))
                 (start-mm (string-to-number (match-string 3)))
                 (end-hh (string-to-number (match-string 4)))
                 (end-mm (string-to-number (match-string 5)))
                 (hours (string-to-number (match-string 6)))
                 (minutes (string-to-number (match-string 7))))
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
                                 :minutes minutes
                                 :start-hh start-hh
                                 :start-mm start-mm
                                 :end-hh end-hh
                                 :end-mm end-mm)
                           rows))))))))))
    ;; Running org-clock-multi clocks
    (when (bound-and-true-p org-clock-multi-clocks)
      (dolist (clock org-clock-multi-clocks)
        (let* ((key (car clock))
               (start-minutes (cdr clock))
               (now-minutes (org-clock-multi--current-minutes))
               (start-time (org-clock-multi--minutes-to-time start-minutes))
               (now-time (current-time))
               (date (format-time-string "%Y-%m-%d" start-time))
               (start-hh (string-to-number (format-time-string "%H" start-time)))
               (start-mm (string-to-number (format-time-string "%M" start-time)))
               (end-hh (string-to-number (format-time-string "%H" now-time)))
               (end-mm (string-to-number (format-time-string "%M" now-time)))
               (elapsed (- now-minutes start-minutes))
               (hours (/ elapsed 60))
               (minutes (% elapsed 60)))
          (when (and (or (null from) (org-string<= from date))
                     (or (null to) (org-string<= date to)))
            (org-clock-multi--with-heading-at-key key
              (let ((tags (org-get-tags)))
                (when (or (null tag)
                          (member tag tags))
                  (push (list :date date
                              :tags tags
                              :hours hours
                              :minutes minutes
                              :start-hh start-hh
                              :start-mm start-mm
                              :end-hh end-hh
                              :end-mm end-mm)
                        rows))))))))
    (sort rows (lambda (a b)
                 (string< (plist-get a :date) (plist-get b :date))))))

;;; Aggregation

(defun org-clock-data--entry-rate (entry)
  "Return the hourly rate for ENTRY based on its tags and `org-clock-data-hourly-rates'."
  (let ((tags (plist-get entry :tags))
        (rate 0))
    (dolist (tag tags)
      (when-let* ((r (cdr (assoc tag org-clock-data-hourly-rates))))
        (setq rate r)))
    rate))

(defun org-clock-data-aggregate-by-date (entries)
  "Aggregate ENTRIES by date, summing total minutes and earnings.
Returns an alist of (DATE . (:minutes M :earnings E)) sorted by date."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let* ((date (plist-get entry :date))
             (mins (+ (* (plist-get entry :hours) 60)
                      (plist-get entry :minutes)))
             (rate (org-clock-data--entry-rate entry))
             (earned (* (/ mins 60.0) rate))
             (existing (gethash date table (list :minutes 0 :earnings 0.0))))
        (puthash date
                 (list :minutes (+ (plist-get existing :minutes) mins)
                       :earnings (+ (plist-get existing :earnings) earned))
                 table)))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) table)
      (sort result (lambda (a b) (string< (car a) (car b)))))))

(defun org-clock-data-aggregate-by-hour (entries)
  "Aggregate ENTRIES by hour, splitting multi-hour entries proportionally.
A clock entry spanning 09:30--12:15 distributes as:
  hour 9 = 30min, hour 10 = 60min, hour 11 = 60min, hour 12 = 15min.
Overlapping clocks can cause a single hour to exceed 60 minutes.
Returns an alist of (HOUR . (:minutes M :earnings E)) sorted by hour."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let* ((start-hh (plist-get entry :start-hh))
             (start-mm (plist-get entry :start-mm))
             (end-hh (plist-get entry :end-hh))
             (end-mm (plist-get entry :end-mm))
             (total-mins (+ (* (plist-get entry :hours) 60)
                            (plist-get entry :minutes)))
             (rate (org-clock-data--entry-rate entry))
             (hour start-hh)
             (cursor-mm start-mm))
        (while (and (> total-mins 0) (<= hour 23))
          (let* ((mins-in-hour (if (= hour end-hh)
                                   (min total-mins (- end-mm cursor-mm))
                                 (min total-mins (- 60 cursor-mm))))
                 (mins-in-hour (max 0 mins-in-hour))
                 (earned (* (/ mins-in-hour 60.0) rate))
                 (existing (gethash hour table (list :minutes 0 :earnings 0.0))))
            (when (> mins-in-hour 0)
              (puthash hour
                       (list :minutes (+ (plist-get existing :minutes) mins-in-hour)
                             :earnings (+ (plist-get existing :earnings) earned))
                       table))
            (setq total-mins (- total-mins mins-in-hour))
            (setq hour (1+ hour))
            (setq cursor-mm 0)))))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) table)
      (sort result (lambda (a b) (< (car a) (car b)))))))

;;; Bar Chart

(defcustom org-clock-data-hourly-rates nil
  "Alist mapping tag names to hourly rates.
Example: ((\"company1\" . 100) (\"company2\" . 120))"
  :type '(alist :key-type string :value-type number)
  :group 'org-clock-data)

(defcustom org-clock-data-currency-symbol "€"
  "Currency symbol for earnings display."
  :type 'string
  :group 'org-clock-data)

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

(defcustom org-clock-data-bar-value 'time
  "What the bar length represents.
\\='time means bar length is based on minutes worked.
\\='earnings means bar length is based on money earned."
  :type '(choice (const :tag "Time" time)
                 (const :tag "Earnings" earnings))
  :group 'org-clock-data)

(defun org-clock-data--format-hour (hour)
  "Format HOUR (integer 0-23) as a label like 09:00 or 14:00."
  (format "%02d:00" hour))

(defun org-clock-data--format-date-short (date-str)
  "Format DATE-STR (YYYY-MM-DD) as a short label like Mon 02/10."
  (let ((time (encode-time 0 0 0
                           (string-to-number (substring date-str 8 10))
                           (string-to-number (substring date-str 5 7))
                           (string-to-number (substring date-str 0 4)))))
    (format-time-string "%a %m/%d" time)))

(defun org-clock-data-bar-chart (aggregated &optional mode)
  "Render AGGREGATED data as a bar chart string.
MODE is \\='daily (default) or \\='hourly.
In daily mode, keys are date strings and target is `org-clock-data-bar-target-hours'.
In hourly mode, keys are hour integers and target is 60 minutes.
Bar length is based on `org-clock-data-bar-value' (time or earnings)."
  (if (null aggregated)
      "  No clock data for this period.\n"
    (let* ((hourlyp (eq mode 'hourly))
           (target-mins (if hourlyp 60 (* org-clock-data-bar-target-hours 60)))
           (earn-bar-p (eq org-clock-data-bar-value 'earnings))
           (max-rate (if (and earn-bar-p org-clock-data-hourly-rates)
                         (apply #'max (mapcar #'cdr org-clock-data-hourly-rates))
                       0))
           (target-earnings (* (/ target-mins 60.0) max-rate))
           (total-mins 0)
           (total-earnings 0.0)
           (row-data
            (mapcar
             (lambda (pair)
               (let* ((key (car pair))
                      (data (cdr pair))
                      (mins (plist-get data :minutes))
                      (earnings (plist-get data :earnings))
                      (hours (/ mins 60))
                      (remaining-mins (% mins 60))
                      (bar-target (if earn-bar-p target-earnings target-mins))
                      (bar-val (if earn-bar-p earnings (float mins)))
                      (ratio (if (> bar-target 0)
                                 (min 1.0 (/ bar-val bar-target))
                               0.0))
                      (filled (round (* org-clock-data-bar-max-width ratio)))
                      (label (if hourlyp
                                 (org-clock-data--format-hour key)
                               (org-clock-data--format-date-short key)))
                      (time-str (format "%dh%02dm" hours remaining-mins))
                      (earn-str (when (and org-clock-data-hourly-rates (> earnings 0))
                                  (format "%s%.2f" org-clock-data-currency-symbol earnings))))
                 (setq total-mins (+ total-mins mins))
                 (setq total-earnings (+ total-earnings earnings))
                 (list label time-str earn-str filled)))
             aggregated))
           (has-earnings (cl-some (lambda (r) (nth 2 r)) row-data))
           (max-time-width (apply #'max (mapcar (lambda (r) (length (nth 1 r))) row-data)))
           (max-earn-width (if has-earnings
                               (apply #'max (mapcar (lambda (r) (length (or (nth 2 r) ""))) row-data))
                             0))
           (lines
            (mapcar
             (lambda (r)
               (let* ((time-str (nth 1 r))
                      (earn-str (nth 2 r))
                      (padded-time (format (format "%%-%ds" max-time-width) time-str))
                      (colored-info
                       (if earn-str
                           (let ((padded-earn (format (format "%%%ds" max-earn-width) earn-str)))
                             (concat (propertize "[" 'face `(:foreground ,(embellish-theme-color :info)))
                                     (propertize padded-time 'face `(:foreground ,(embellish-theme-color :info)))
                                     (propertize " | " 'face `(:foreground ,(embellish-theme-color :info)))
                                     (propertize padded-earn 'face `(:foreground ,(embellish-theme-color :success)))
                                     (propertize "]" 'face `(:foreground ,(embellish-theme-color :info)))))
                         (concat (propertize "[" 'face `(:foreground ,(embellish-theme-color :info)))
                                 (propertize padded-time 'face `(:foreground ,(embellish-theme-color :info)))
                                 (propertize "]" 'face `(:foreground ,(embellish-theme-color :info)))))))
                 (concat "  "
                         (propertize (nth 0 r) 'face `(:foreground ,(embellish-theme-color :salient)))
                         "  "
                         colored-info
                         " "
                         (propertize (make-string (nth 3 r) org-clock-data-bar-char)
                                     'face `(:foreground ,(embellish-theme-color :subtle))))))
             row-data))
           (total-hours (/ total-mins 60))
           (total-remaining (% total-mins 60))
           (total-time-str (format "%dh%02dm" total-hours total-remaining))
           (total-line
            (if (and org-clock-data-hourly-rates (> total-earnings 0))
                (concat (propertize "  Total: " 'face `(:foreground ,(embellish-theme-color :salient)))
                        (propertize total-time-str 'face `(:foreground ,(embellish-theme-color :info)))
                        (propertize " | " 'face `(:foreground ,(embellish-theme-color :info)))
                        (propertize (format "%s%.2f" org-clock-data-currency-symbol total-earnings)
                                    'face `(:foreground ,(embellish-theme-color :success))))
              (concat (propertize "  Total: " 'face `(:foreground ,(embellish-theme-color :salient)))
                      (propertize total-time-str 'face `(:foreground ,(embellish-theme-color :info)))))))
      (concat (string-join lines "\n") "\n"
              (propertize (make-string 40 ?─) 'face `(:foreground ,(embellish-theme-color :faded))) "\n"
              total-line "\n"))))

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
               (ndays (org-agenda-span-to-ndays org-agenda-current-span org-starting-day))
               (day-view-p (= ndays 1))
               (entries (org-clock-data-extract :tag "work" :from from :to to))
               (aggregated (if day-view-p
                               (org-clock-data-aggregate-by-hour entries)
                             (org-clock-data-aggregate-by-date entries)))
               (chart (org-clock-data-bar-chart aggregated (if day-view-p 'hourly 'daily)))
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
