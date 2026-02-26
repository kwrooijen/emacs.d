;;; mcp-test.el --- Tests for MCP tool functions -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the MCP elisp tool functions.
;; All tests use temporary org files and mock external dependencies.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'json)

;; Provide stubs for packages unavailable in batch mode
;; (agent-shell has heavy deps: comint, magit, acp, display-buffer, etc.)
(unless (featurep 'request)
  (provide 'request))
(unless (featurep 'org-agent-shell)
  (provide 'org-agent-shell))

;; Load modules under test relative to this file
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (require 'org-clock-multi (expand-file-name "../../org-clock-multi/org-clock-multi.el" dir))
  (require 'org-asana (expand-file-name "../../org-asana/org-asana.el" dir))
  (require 'mcp (expand-file-name "../mcp.el" dir)))

;;; Test Utilities

(defmacro mcp-test-with-temp-org (content &rest body)
  "Create a temp org file with CONTENT, execute BODY, then clean up.
The variable `--org-file--' is bound to the temp file path."
  (declare (indent 1) (debug t))
  `(let ((--org-file-- (make-temp-file "mcp-test-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file --org-file--
             (insert ,content))
           (with-current-buffer (find-file-noselect --org-file--)
             (org-mode)
             (goto-char (point-min))
             ,@body))
       (when-let* ((buf (find-buffer-visiting --org-file--)))
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (when (file-exists-p --org-file--)
         (delete-file --org-file--)))))

(defun mcp-test--parse-json (json-string)
  "Parse JSON-STRING returned by MCP functions.
MCP functions return elisp-printed JSON strings, so we unwrap the outer quotes."
  (let ((unquoted (if (and (string-prefix-p "\"" json-string)
                           (string-suffix-p "\"" json-string))
                      (read json-string)
                    json-string)))
    (json-read-from-string unquoted)))

;;; create_work_todo tests

(ert-deftest mcp-test-create-work-todo-basic ()
  "Creating a todo inserts a level-2 heading under the parent."
  (mcp-test-with-temp-org
      "* Project\n:PROPERTIES:\n:ASANA_PROJECT: 123\n:END:\n"
    (kwrooijen/mcp-create-work-todo --org-file-- "Project" "My new ticket")
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\* TODO My new ticket$" nil t))
    (should (org-entry-get nil "ASANA_PROJECT" t))))

(ert-deftest mcp-test-create-work-todo-custom-state ()
  "Creating a todo with a custom state uses that state."
  (mcp-test-with-temp-org
      "* Project\n:PROPERTIES:\n:ASANA_PROJECT: 123\n:END:\n"
    (kwrooijen/mcp-create-work-todo --org-file-- "Project" "Blocked ticket"
                                    :state "WAITING")
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\* WAITING Blocked ticket$" nil t))))

(ert-deftest mcp-test-create-work-todo-with-body ()
  "Creating a todo with body includes the body text."
  (mcp-test-with-temp-org
      "* Project\n:PROPERTIES:\n:ASANA_PROJECT: 123\n:END:\n"
    (kwrooijen/mcp-create-work-todo --org-file-- "Project" "With body"
                                    :body "Some initial notes")
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\* TODO With body$" nil t))
    (should (re-search-forward "Some initial notes" nil t))))

(ert-deftest mcp-test-create-work-todo-returns-json ()
  "Creating a todo returns JSON with success, heading, state, file."
  (mcp-test-with-temp-org
      "* Project\n:PROPERTIES:\n:ASANA_PROJECT: 123\n:END:\n"
    (let* ((result (kwrooijen/mcp-create-work-todo
                    --org-file-- "Project" "JSON test"))
           (parsed (mcp-test--parse-json result)))
      (should (eq (alist-get 'success parsed) t))
      (should (equal (alist-get 'heading parsed) "JSON test"))
      (should (equal (alist-get 'state parsed) "TODO")))))

;;; edit_work_todo_ticket_description tests

(ert-deftest mcp-test-edit-work-todo-ticket-description-creates-quote ()
  "Adds a #+BEGIN_QUOTE ticket block when none exists."
  (mcp-test-with-temp-org
      "* Project\n** TODO My ticket\n:PROPERTIES:\n:END:\n"
    (kwrooijen/mcp-edit-work-todo-ticket-description --org-file-- "My ticket" "The description")
    (goto-char (point-min))
    (should (re-search-forward "^#\\+BEGIN_QUOTE ticket$" nil t))
    (should (re-search-forward "The description" nil t))
    (should (re-search-forward "^#\\+END_QUOTE$" nil t))))

(ert-deftest mcp-test-edit-work-todo-ticket-description-replaces-quote ()
  "Replaces an existing #+BEGIN_QUOTE ticket block."
  (mcp-test-with-temp-org
      (concat "* Project\n** TODO My ticket\n:PROPERTIES:\n:END:\n\n"
              "#+BEGIN_QUOTE ticket\nOld content\n#+END_QUOTE\n")
    (kwrooijen/mcp-edit-work-todo-ticket-description --org-file-- "My ticket" "New content")
    (goto-char (point-min))
    (should (re-search-forward "New content" nil t))
    (goto-char (point-min))
    (should-not (re-search-forward "Old content" nil t))))

(ert-deftest mcp-test-edit-work-todo-ticket-description-preserves-other-content ()
  "Preserves existing body text when adding a quote block."
  (mcp-test-with-temp-org
      "* Project\n** TODO My ticket\n:PROPERTIES:\n:END:\nMy notes here\n"
    (kwrooijen/mcp-edit-work-todo-ticket-description --org-file-- "My ticket" "Ticket desc")
    (goto-char (point-min))
    (should (re-search-forward "My notes here" nil t))
    (goto-char (point-min))
    (should (re-search-forward "^#\\+BEGIN_QUOTE ticket$" nil t))
    (should (re-search-forward "Ticket desc" nil t))))

;;; asana_push tests

(ert-deftest mcp-test-asana-push-errors-without-project ()
  "Errors when heading has no ASANA_PROJECT property."
  (mcp-test-with-temp-org
      "* Project\n** TODO No project ticket\n:PROPERTIES:\n:END:\n"
    (should-error
     (kwrooijen/mcp-asana-push --org-file-- "No project ticket")
     :type 'user-error)))

(ert-deftest mcp-test-asana-push-errors-if-already-synced ()
  "Errors when heading already has an ASANA property."
  (mcp-test-with-temp-org
      (concat "* Project\n:PROPERTIES:\n:ASANA_PROJECT: 999\n:END:\n"
              "** TODO Already synced\n:PROPERTIES:\n:ASANA: 12345\n:END:\n")
    (should-error
     (kwrooijen/mcp-asana-push --org-file-- "Already synced")
     :type 'user-error)))

(ert-deftest mcp-test-asana-push-sets-properties ()
  "Pushes to Asana and sets ASANA and ASANA_SECTION properties."
  (mcp-test-with-temp-org
      (concat "* Project\n:PROPERTIES:\n:ASANA_PROJECT: 999\n:END:\n"
              "** TODO Push me\n:PROPERTIES:\n:END:\n\n"
              "#+BEGIN_QUOTE ticket\nTicket body\n#+END_QUOTE\n")
    (cl-letf (((symbol-function 'org-asana--post)
               (lambda (endpoint data)
                 (cond
                  ((string-prefix-p "tasks" endpoint)
                   '((gid . "mock-gid-42")))
                  ((string-match "sections/.*/addTask" endpoint)
                   '((success . t))))))
              ((symbol-function 'org-asana--request-all-pages)
               (lambda (_endpoint &optional _params)
                 (vector '((gid . "sec-1") (name . "Backlog")))))
              ((symbol-function 'org-asana--token)
               (lambda () "fake-token")))
      (let* ((result (kwrooijen/mcp-asana-push --org-file-- "Push me"))
             (parsed (mcp-test--parse-json result)))
        (should (eq (alist-get 'success parsed) t))
        (should (equal (alist-get 'asana_gid parsed) "mock-gid-42"))
        (should (equal (alist-get 'asana_section parsed) "Backlog"))
        ;; Verify properties were set on the heading
        (goto-char (point-min))
        (re-search-forward "^\\*\\* .* Push me$")
        (should (equal (org-entry-get nil "ASANA") "mock-gid-42"))
        (should (equal (org-entry-get nil "ASANA_SECTION") "Backlog"))))))

;;; get_work_todo / edit_work_todo round-trip tests

(ert-deftest mcp-test-create-then-get-todo ()
  "Create a todo, then retrieve it with get_work_todo."
  (mcp-test-with-temp-org
      "* Project\n:PROPERTIES:\n:ASANA_PROJECT: 123\n:END:\n"
    (kwrooijen/mcp-create-work-todo --org-file-- "Project" "Roundtrip ticket")
    (let* ((result (kwrooijen/mcp-get-work-todo --org-file-- "Roundtrip ticket"))
           (parsed (mcp-test--parse-json result)))
      (should (equal (alist-get 'heading parsed) "Roundtrip ticket"))
      (should (equal (alist-get 'state parsed) "TODO")))))

(ert-deftest mcp-test-edit-work-todo-state ()
  "Edit a todo's state via edit_work_todo."
  (mcp-test-with-temp-org
      "* Project\n** TODO Change me\n:PROPERTIES:\n:END:\n"
    (kwrooijen/mcp-edit-work-todo --org-file-- "Change me" :state "DONE")
    (goto-char (point-min))
    (re-search-forward "Change me")
    (should (equal (org-get-todo-state) "DONE"))))

;;; clock_in / clock_out tests

(defmacro mcp-test-with-clock-env (&rest body)
  "Execute BODY with a clean org-clock-multi state and no disk writes."
  (declare (indent 0) (debug t))
  `(let ((org-clock-multi-clocks nil)
         (org-clock-multi-paused nil))
     (cl-letf (((symbol-function 'org-clock-multi-save-state) #'ignore))
       ,@body)))

(ert-deftest mcp-test-clock-in-returns-json ()
  "clock_in returns JSON with success, heading, and file."
  (mcp-test-with-temp-org
      "* Project\n** TODO My task\n:PROPERTIES:\n:END:\n"
    (mcp-test-with-clock-env
      (let* ((result (kwrooijen/mcp-clock-in --org-file-- "My task"))
             (parsed (mcp-test--parse-json result)))
        (should (eq (alist-get 'success parsed) t))
        (should (equal (alist-get 'heading parsed) "My task"))
        (should (equal (alist-get 'file parsed) --org-file--))))))

(ert-deftest mcp-test-clock-in-adds-to-active-clocks ()
  "clock_in adds the heading to org-clock-multi-clocks."
  (mcp-test-with-temp-org
      "* Project\n** TODO My task\n:PROPERTIES:\n:END:\n"
    (mcp-test-with-clock-env
      (should (null org-clock-multi-clocks))
      (kwrooijen/mcp-clock-in --org-file-- "My task")
      (should (= 1 (length org-clock-multi-clocks))))))

(ert-deftest mcp-test-clock-out-returns-json ()
  "clock_out returns JSON with success, heading, and file."
  (mcp-test-with-temp-org
      "* Project\n** TODO My task\n:PROPERTIES:\n:END:\n"
    (mcp-test-with-clock-env
      (kwrooijen/mcp-clock-in --org-file-- "My task")
      (let* ((result (kwrooijen/mcp-clock-out --org-file-- "My task"))
             (parsed (mcp-test--parse-json result)))
        (should (eq (alist-get 'success parsed) t))
        (should (equal (alist-get 'heading parsed) "My task"))))))

(ert-deftest mcp-test-clock-out-removes-from-active-clocks ()
  "clock_out removes the heading from org-clock-multi-clocks."
  (mcp-test-with-temp-org
      "* Project\n** TODO My task\n:PROPERTIES:\n:END:\n"
    (mcp-test-with-clock-env
      (kwrooijen/mcp-clock-in --org-file-- "My task")
      (should (= 1 (length org-clock-multi-clocks)))
      (kwrooijen/mcp-clock-out --org-file-- "My task")
      (should (null org-clock-multi-clocks)))))

(ert-deftest mcp-test-clock-in-errors-on-missing-heading ()
  "clock_in errors when the heading does not exist."
  (mcp-test-with-temp-org
      "* Project\n** TODO Real task\n:PROPERTIES:\n:END:\n"
    (mcp-test-with-clock-env
      (should-error
       (kwrooijen/mcp-clock-in --org-file-- "Nonexistent task")))))

(provide 'mcp-test)
;;; mcp-test.el ends here
