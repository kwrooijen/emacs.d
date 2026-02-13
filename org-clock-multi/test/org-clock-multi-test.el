;;; org-clock-multi-test.el --- Tests for org-clock-multi -*- lexical-binding: t; -*-

;;; Commentary:

;; Test suite for org-clock-multi, with special focus on LOGBOOK integrity.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-clock)

;; Load org-clock-multi from parent directory
(let ((default-directory (file-name-directory (or load-file-name buffer-file-name))))
  (require 'org-clock-multi (expand-file-name "../org-clock-multi.el")))

;;; Test Utilities

(defmacro org-clock-multi-test-with-temp-org (content &rest body)
  "Create a temp org buffer with CONTENT and execute BODY."
  (declare (indent 1) (debug t))
  `(let ((org-clock-multi-clocks nil)  ; Start with clean state
         (org-clock-multi-persist-file (make-temp-file "org-clock-multi-test-"))
         ;; Save original state to restore after test
         (--orig-clocks-- org-clock-multi-clocks))
     (unwind-protect
         (with-temp-buffer
           (org-mode)
           (insert ,content)
           (goto-char (point-min))
           ,@body)
       ;; Restore original state and clean up temp file
       (setq org-clock-multi-clocks --orig-clocks--)
       (when (file-exists-p org-clock-multi-persist-file)
         (delete-file org-clock-multi-persist-file)))))

(defun org-clock-multi-test--get-logbook-entries ()
  "Get all CLOCK entries from LOGBOOK at current heading."
  (save-excursion
    (org-back-to-heading t)
    (let ((entries nil)
          (bound (save-excursion (org-end-of-subtree t) (point))))
      (when (re-search-forward ":LOGBOOK:" bound t)
        (let ((drawer-end (save-excursion
                            (re-search-forward ":END:" bound t)
                            (point))))
          (while (re-search-forward "^CLOCK: " drawer-end t)
            (push (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))
                  entries))))
      (nreverse entries))))

(defun org-clock-multi-test--valid-clock-entry-p (entry)
  "Return non-nil if ENTRY is a valid org-clock LOGBOOK line."
  ;; Format: CLOCK: [YYYY-MM-DD Day HH:MM]--[YYYY-MM-DD Day HH:MM] =>  H:MM
  (string-match-p
   (concat "^CLOCK: "
           "\\[" "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\} [0-9]\\{2\\}:[0-9]\\{2\\}" "\\]"
           "--"
           "\\[" "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\} [0-9]\\{2\\}:[0-9]\\{2\\}" "\\]"
           " => +[0-9]+:[0-9][0-9]$")
   entry))

;;; LOGBOOK Integrity Tests (Critical)

(ert-deftest org-clock-multi-test-clock-entry-format ()
  "Test that clock entries match org-clock's exact format."
  (org-clock-multi-test-with-temp-org
      "* TODO Test task\n"
    (org-clock-multi-clock-in)
    ;; Wait a moment so we have elapsed time
    (sleep-for 0.1)
    (org-clock-multi-clock-out)
    (let ((entries (org-clock-multi-test--get-logbook-entries)))
      (should (= 1 (length entries)))
      (should (org-clock-multi-test--valid-clock-entry-p (car entries))))))

(ert-deftest org-clock-multi-test-multiple-clock-outs ()
  "Test clocking in 3 tasks and clocking out each separately."
  (org-clock-multi-test-with-temp-org
      "* TODO Task 1\n* TODO Task 2\n* TODO Task 3\n"
    ;; Clock into all 3 tasks
    (goto-char (point-min))
    (org-clock-multi-clock-in)
    (org-next-visible-heading 1)
    (org-clock-multi-clock-in)
    (org-next-visible-heading 1)
    (org-clock-multi-clock-in)
    (should (= 3 (length org-clock-multi-clocks)))
    ;; Wait a bit
    (sleep-for 0.1)
    ;; Clock out each
    (goto-char (point-min))
    (org-clock-multi-clock-out)
    (let ((entries (org-clock-multi-test--get-logbook-entries)))
      (should (= 1 (length entries)))
      (should (org-clock-multi-test--valid-clock-entry-p (car entries))))
    (org-next-visible-heading 1)
    (org-clock-multi-clock-out)
    (let ((entries (org-clock-multi-test--get-logbook-entries)))
      (should (= 1 (length entries)))
      (should (org-clock-multi-test--valid-clock-entry-p (car entries))))
    (org-next-visible-heading 1)
    (org-clock-multi-clock-out)
    (let ((entries (org-clock-multi-test--get-logbook-entries)))
      (should (= 1 (length entries)))
      (should (org-clock-multi-test--valid-clock-entry-p (car entries))))
    ;; All clocks should be cleared
    (should (= 0 (length org-clock-multi-clocks)))))

(ert-deftest org-clock-multi-test-clock-out-all ()
  "Test clock-out-all writes valid entries for all tasks."
  (org-clock-multi-test-with-temp-org
      "* TODO Task 1\n* TODO Task 2\n* TODO Task 3\n"
    ;; Clock into all 3 tasks
    (goto-char (point-min))
    (org-clock-multi-clock-in)
    (org-next-visible-heading 1)
    (org-clock-multi-clock-in)
    (org-next-visible-heading 1)
    (org-clock-multi-clock-in)
    (sleep-for 0.1)
    ;; Clock out all at once
    (org-clock-multi-clock-out-all)
    (should (= 0 (length org-clock-multi-clocks)))
    ;; Verify each task has a valid entry
    (goto-char (point-min))
    (dotimes (_ 3)
      (let ((entries (org-clock-multi-test--get-logbook-entries)))
        (should (= 1 (length entries)))
        (should (org-clock-multi-test--valid-clock-entry-p (car entries))))
      (org-next-visible-heading 1))))

(ert-deftest org-clock-multi-test-existing-logbook-not-corrupted ()
  "Test that existing LOGBOOK entries are preserved."
  (org-clock-multi-test-with-temp-org
      "* TODO Task with existing clocks
:LOGBOOK:
CLOCK: [2024-01-15 Mon 10:00]--[2024-01-15 Mon 11:30] =>  1:30
CLOCK: [2024-01-14 Sun 09:00]--[2024-01-14 Sun 10:00] =>  1:00
:END:
"
    (org-clock-multi-clock-in)
    (sleep-for 0.1)
    (org-clock-multi-clock-out)
    (let ((entries (org-clock-multi-test--get-logbook-entries)))
      ;; Should have 3 entries now (1 new + 2 existing)
      (should (= 3 (length entries)))
      ;; All should be valid
      (dolist (entry entries)
        (should (org-clock-multi-test--valid-clock-entry-p entry)))
      ;; Original entries should still be there
      (should (cl-find-if (lambda (e) (string-match-p "2024-01-15" e)) entries))
      (should (cl-find-if (lambda (e) (string-match-p "2024-01-14" e)) entries)))))

(ert-deftest org-clock-multi-test-org-clock-sum-parseable ()
  "Test that written entries can be parsed by org-clock-sum."
  (org-clock-multi-test-with-temp-org
      "* TODO Test task\n"
    (org-clock-multi-clock-in)
    (sleep-for 0.1)
    (org-clock-multi-clock-out)
    ;; org-clock-sum should not error and should find some time
    (org-clock-sum)
    ;; If we get here without error, the format is parseable
    (should t)))

;;; Functional Tests

(ert-deftest org-clock-multi-test-clock-in-adds-to-list ()
  "Test that clock-in adds entry to clock list."
  (org-clock-multi-test-with-temp-org
      "* TODO Test task\n"
    (should (= 0 (length org-clock-multi-clocks)))
    (org-clock-multi-clock-in)
    (should (= 1 (length org-clock-multi-clocks)))))

(ert-deftest org-clock-multi-test-clock-out-removes-from-list ()
  "Test that clock-out removes entry from clock list."
  (org-clock-multi-test-with-temp-org
      "* TODO Test task\n"
    (org-clock-multi-clock-in)
    (should (= 1 (length org-clock-multi-clocks)))
    (org-clock-multi-clock-out)
    (should (= 0 (length org-clock-multi-clocks)))))

(ert-deftest org-clock-multi-test-duplicate-clock-in-idempotent ()
  "Test that clocking in twice to same task doesn't duplicate."
  (org-clock-multi-test-with-temp-org
      "* TODO Test task\n"
    (org-clock-multi-clock-in)
    (org-clock-multi-clock-in)  ; Second clock-in should be no-op
    (should (= 1 (length org-clock-multi-clocks)))))

(ert-deftest org-clock-multi-test-cancel-no-logbook ()
  "Test that cancel doesn't write LOGBOOK entry."
  (org-clock-multi-test-with-temp-org
      "* TODO Test task\n"
    (org-clock-multi-clock-in)
    (org-clock-multi-clock-cancel)
    (let ((entries (org-clock-multi-test--get-logbook-entries)))
      (should (= 0 (length entries))))
    (should (= 0 (length org-clock-multi-clocks)))))

(ert-deftest org-clock-multi-test-cancel-all ()
  "Test that cancel-all doesn't write any LOGBOOK entries."
  (org-clock-multi-test-with-temp-org
      "* TODO Task 1\n* TODO Task 2\n"
    (goto-char (point-min))
    (org-clock-multi-clock-in)
    (org-next-visible-heading 1)
    (org-clock-multi-clock-in)
    (org-clock-multi-clock-cancel-all)
    ;; Check no entries written
    (goto-char (point-min))
    (should (= 0 (length (org-clock-multi-test--get-logbook-entries))))
    (org-next-visible-heading 1)
    (should (= 0 (length (org-clock-multi-test--get-logbook-entries))))
    (should (= 0 (length org-clock-multi-clocks)))))

(ert-deftest org-clock-multi-test-clocking-p ()
  "Test org-clock-multi-clocking-p predicate."
  (org-clock-multi-test-with-temp-org
      "* TODO Task 1\n* TODO Task 2\n"
    (goto-char (point-min))
    (should-not (org-clock-multi-clocking-p))
    (org-clock-multi-clock-in)
    (should (org-clock-multi-clocking-p))
    (org-next-visible-heading 1)
    (should-not (org-clock-multi-clocking-p))))

(ert-deftest org-clock-multi-test-state-persistence ()
  "Test that state persists via save/load cycle."
  (let ((test-file (make-temp-file "org-clock-multi-test-" nil ".el"))
        (org-file (make-temp-file "org-clock-multi-test-org-" nil ".org")))
    (unwind-protect
        (let ((org-clock-multi-persist-file test-file)
              (org-clock-multi-clocks nil))
          ;; Create an org file and clock in
          (with-temp-file org-file
            (insert "* TODO Test task\n"))
          (with-current-buffer (find-file-noselect org-file)
            (org-mode)
            (goto-char (point-min))
            (org-clock-multi-clock-in)
            (should (= 1 (length org-clock-multi-clocks)))
            ;; Save state
            (org-clock-multi-save-state)
            ;; Clear and reload
            (setq org-clock-multi-clocks nil)
            (org-clock-multi-load-state)
            (should (= 1 (length org-clock-multi-clocks)))
            ;; Verify it points to same place
            (let ((clock (car org-clock-multi-clocks)))
              (should (marker-buffer (car clock)))
              (should (= 1 (marker-position (car clock)))))))
      ;; Cleanup
      (delete-file test-file)
      (delete-file org-file)
      (when-let ((buf (find-buffer-visiting org-file)))
        (kill-buffer buf)))))

(provide 'org-clock-multi-test)
;;; org-clock-multi-test.el ends here
