;;; org-asana.el --- Pull Asana tasks into org-mode files -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.0") (request "0.3.0"))
;; Keywords: org, asana, tasks

;;; Commentary:

;; Pull Asana board tasks into org-mode TODO files.
;;
;; Each org heading with an :ASANA_PROJECT: property maps to an Asana project.
;; Running `org-asana-pull' fetches tasks and inserts them as child headings.
;;
;; Features:
;;   - Automatic project discovery from org-agenda-files
;;   - JSON cache with configurable TTL (default 15 min)
;;   - Deduplication via :ASANA: property
;;   - :ASANA_SECTION: tracks board column (independent of org TODO state)
;;   - Removed ticket detection with user prompt

;;; Code:

(require 'org)
(require 'org-element)
(require 'json)
(require 'request)

;;; Customization

(defgroup org-asana nil
  "Pull Asana tasks into org-mode files."
  :group 'org
  :prefix "org-asana-")

(defcustom org-asana-token-env "ASANA_TOKEN"
  "Environment variable name containing the Asana Personal Access Token."
  :type 'string
  :group 'org-asana)

(defcustom org-asana-cache-directory
  (expand-file-name "tmp/org-asana/" user-emacs-directory)
  "Directory for cached Asana JSON responses."
  :type 'directory
  :group 'org-asana)

(defcustom org-asana-cache-ttl 900
  "Cache time-to-live in seconds.  Default is 900 (15 minutes)."
  :type 'integer
  :group 'org-asana)

(defcustom org-asana-pull-completed nil
  "When non-nil, also pull completed tasks from Asana."
  :type 'boolean
  :group 'org-asana)

(defcustom org-asana-default-state "TODO"
  "Default org TODO state for newly pulled tasks."
  :type 'string
  :group 'org-asana)

;;; Token

(defun org-asana--token ()
  "Return the Asana PAT from the environment."
  (or (getenv org-asana-token-env)
      (user-error "Environment variable %s is not set" org-asana-token-env)))

;;; HTTP

(defun org-asana--request-all-pages (endpoint &optional params)
  "Fetch all pages from Asana API ENDPOINT.
Returns accumulated results from all pages."
  (let ((all-data '())
        (page-params (append params '(("limit" . "100"))))
        (has-more t))
    (while has-more
      (let* ((url (concat "https://app.asana.com/api/1.0/" endpoint))
             (response (request url
                        :type "GET"
                        :headers `(("Authorization" . ,(concat "Bearer " (org-asana--token))))
                        :params page-params
                        :parser 'json-read
                        :sync t
                        :silent t))
             (body (request-response-data response))
             (data (alist-get 'data body))
             (next-page (alist-get 'next_page body)))
        (when (not (= 200 (request-response-status-code response)))
          (user-error "Asana API error %d: %s"
                      (request-response-status-code response)
                      body))
        (setq all-data (vconcat all-data data))
        (if (and next-page (alist-get 'offset next-page))
            (setq page-params
                  (append params
                          `(("limit" . "100")
                            ("offset" . ,(alist-get 'offset next-page)))))
          (setq has-more nil))))
    all-data))

;;; Cache

(defun org-asana--cache-file (project-gid)
  "Return the cache file path for PROJECT-GID."
  (expand-file-name (concat project-gid ".json") org-asana-cache-directory))

(defun org-asana--cache-fresh-p (project-gid)
  "Return non-nil if the cache for PROJECT-GID exists and is fresh."
  (let ((file (org-asana--cache-file project-gid)))
    (and (file-exists-p file)
         (< (float-time (time-subtract (current-time)
                                       (file-attribute-modification-time
                                        (file-attributes file))))
            org-asana-cache-ttl))))

(defun org-asana--write-cache (project-gid data)
  "Write DATA as JSON to the cache file for PROJECT-GID."
  (make-directory org-asana-cache-directory t)
  (let ((file (org-asana--cache-file project-gid)))
    (with-temp-file file
      (insert (json-encode data)))))

(defun org-asana--read-cache (project-gid)
  "Read and parse the cached JSON for PROJECT-GID."
  (let ((file (org-asana--cache-file project-gid)))
    (with-temp-buffer
      (insert-file-contents file)
      (json-read))))

;;; Discovery

(defun org-asana--discover-projects ()
  "Scan `org-agenda-files' for headings with :ASANA_PROJECT: property.
Returns a list of plists (:gid GID :file FILE :marker MARKER :level LEVEL)."
  (let (projects)
    (dolist (file (org-agenda-files))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward org-heading-regexp nil t)
           (let ((gid (org-entry-get nil "ASANA_PROJECT")))
             (when gid
               (push (list :gid gid
                           :file (buffer-file-name)
                           :marker (point-marker)
                           :level (org-current-level))
                     projects)))))))
    (nreverse projects)))

;;; Data Fetching

(defun org-asana--fetch-project (project-gid &optional force)
  "Fetch tasks for PROJECT-GID, using cache unless stale or FORCE is non-nil.
Returns a vector of task alists."
  (if (and (not force) (org-asana--cache-fresh-p project-gid))
      (progn
        (message "org-asana: Using cached data for %s" project-gid)
        (org-asana--read-cache project-gid))
    (message "org-asana: Fetching tasks for project %s..." project-gid)
    (let ((data (org-asana--request-all-pages
                 (format "projects/%s/tasks" project-gid)
                 `(("opt_fields" . "gid,name,notes,completed,memberships.section.name,memberships.project.gid")))))
      (org-asana--write-cache project-gid data)
      data)))

;;; Org File Operations

(defun org-asana--task-section (task project-gid)
  "Extract the section name for TASK within PROJECT-GID.
Looks through the memberships array for the matching project."
  (let ((memberships (alist-get 'memberships task))
        (section nil))
    (when memberships
      (seq-doseq (m memberships)
        (let* ((project (alist-get 'project m))
               (pgid (alist-get 'gid project)))
          (when (equal pgid project-gid)
            (let ((sec (alist-get 'section m)))
              (when sec
                (setq section (alist-get 'name sec))))))))
    section))

(defun org-asana--existing-asana-entries (marker)
  "Collect :ASANA: property values from child headings under MARKER.
Returns a hash-table mapping GID strings to markers."
  (let ((table (make-hash-table :test 'equal)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (let ((parent-level (org-current-level))
             (end (org-end-of-subtree t t)))
         (goto-char marker)
         (while (and (outline-next-heading)
                     (< (point) end))
           (when (= (org-current-level) (1+ parent-level))
             (let ((asana-id (org-entry-get nil "ASANA")))
               (when asana-id
                 (puthash asana-id (point-marker) table))))))))
    table))

(defun org-asana--format-entry (task level state)
  "Format TASK as an org heading string at LEVEL with TODO STATE."
  (let* ((gid (alist-get 'gid task))
         (name (alist-get 'name task))
         (notes (alist-get 'notes task))
         (section (alist-get '_section task))
         (stars (make-string level ?*))
         (props (concat ":PROPERTIES:\n"
                        ":ASANA: " gid "\n"
                        (when section
                          (concat ":ASANA_SECTION: " section "\n"))
                        ":END:"))
         (clean-notes (when notes
                       (replace-regexp-in-string
                        "[ \t]+$" ""
                        (replace-regexp-in-string "\r" "" (string-trim notes)))))
         (body (when (and clean-notes (not (string-empty-p clean-notes)))
                 (concat "\n#+BEGIN_QUOTE ticket\n"
                         clean-notes
                         "\n#+END_QUOTE"))))
    (concat stars " " state " " name "\n"
            props
            (or body "")
            "\n")))

(defun org-asana--insert-tasks (marker level tasks)
  "Insert TASKS as org headings at LEVEL under the heading at MARKER.
Returns the number of inserted tasks."
  (let ((count 0))
    (when (> (length tasks) 0)
      (with-current-buffer (marker-buffer marker)
        (org-with-wide-buffer
         (goto-char marker)
         ;; Move to end of this subtree
         (org-end-of-subtree t t)
         ;; Insert before any trailing blank lines
         (skip-chars-backward " \t\n")
         (end-of-line)
         (seq-doseq (task tasks)
           (insert "\n" (org-asana--format-entry task level org-asana-default-state))
           (cl-incf count)))))
    count))

(defun org-asana--update-existing (existing-table tasks project-gid)
  "Update :ASANA_SECTION: and ticket description for existing headings.
EXISTING-TABLE maps GID to marker.  TASKS is the fetched task vector.
Returns the number of updated entries."
  (let ((count 0))
    (seq-doseq (task tasks)
      (let* ((gid (alist-get 'gid task))
             (marker (gethash gid existing-table))
             (section (org-asana--task-section task project-gid))
             (notes (alist-get 'notes task))
             (updated nil))
        (when marker
          (with-current-buffer (marker-buffer marker)
            (org-with-wide-buffer
             (goto-char marker)
             ;; Update section
             (when section
               (let ((current (org-entry-get nil "ASANA_SECTION")))
                 (unless (equal current section)
                   (org-entry-put nil "ASANA_SECTION" section)
                   (setq updated t))))
             ;; Update ticket description
             (when notes
               (when (org-asana--update-ticket-quote marker notes)
                 (setq updated t))))))
        (when updated
          (cl-incf count))))
    count))

(defun org-asana--update-ticket-quote (marker notes)
  "Update the #+BEGIN_QUOTE ticket block under heading at MARKER with NOTES.
If no ticket quote block exists, do nothing.  Returns non-nil if changed."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char marker)
     (let ((end (save-excursion (org-end-of-subtree t t) (point)))
           (changed nil))
       (when (re-search-forward "^#\\+BEGIN_QUOTE ticket$" end t)
         (let ((quote-start (line-beginning-position))
               (quote-end (when (re-search-forward "^#\\+END_QUOTE$" end t)
                            (line-end-position))))
           (when quote-end
             (let* ((new-body (string-trim notes))
                    (old-body (string-trim
                               (buffer-substring-no-properties
                                (save-excursion (goto-char quote-start)
                                                (forward-line 1) (point))
                                (save-excursion (goto-char quote-end)
                                                (line-beginning-position))))))
               (setq new-body (replace-regexp-in-string
                               "[ \t]+$" ""
                               (replace-regexp-in-string "\r" "" new-body)))
               (unless (or (string-empty-p new-body)
                           (string= old-body new-body))
                 (delete-region quote-start quote-end)
                 (goto-char quote-start)
                 (insert "#+BEGIN_QUOTE ticket\n"
                         new-body "\n"
                         "#+END_QUOTE")
                 (setq changed t))))))
       changed))))

(defun org-asana--handle-removed (existing-table fetched-ids)
  "Handle tasks in EXISTING-TABLE whose GID is not in FETCHED-IDS.
Sets :ASANA_REMOVED: t and prompts the user for a TODO state.
Returns the number of removed entries."
  (let ((count 0))
    (maphash
     (lambda (gid marker)
       (unless (member gid fetched-ids)
         (with-current-buffer (marker-buffer marker)
           (org-with-wide-buffer
            (goto-char marker)
            (let ((already-removed (org-entry-get nil "ASANA_REMOVED")))
              (unless already-removed
                (let* ((heading (org-get-heading t t t t))
                       (state (completing-read
                               (format "Task removed from Asana: \"%s\". Set state: " heading)
                               '("CANCELLED" "DONE" "Skip")
                               nil t)))
                  (org-entry-put nil "ASANA_REMOVED" "t")
                  (unless (equal state "Skip")
                    (org-todo state))
                  (cl-incf count))))))))
     existing-table)
    count))

;;; Interactive Commands

(defun org-asana-pull (&optional force)
  "Pull Asana tasks into org files.
Discovers projects from :ASANA_PROJECT: properties in org-agenda-files.
Uses cached data when fresh (< 15 min).  With prefix arg, force re-fetch."
  (interactive "P")
  (org-asana--token) ;; validate early
  (let ((projects (org-asana--discover-projects))
        (total-new 0)
        (total-updated 0)
        (total-removed 0))
    (unless projects
      (user-error "No headings with :ASANA_PROJECT: found in org-agenda-files"))
    ;; Check for unsaved buffers before doing any work
    (dolist (proj projects)
      (let ((buf (find-buffer-visiting (plist-get proj :file))))
        (when (and buf (buffer-modified-p buf))
          (user-error "Buffer %s has unsaved changes — save it first"
                      (buffer-name buf)))))
    (dolist (proj projects)
      (let* ((gid (plist-get proj :gid))
             (file (plist-get proj :file))
             (marker (plist-get proj :marker))
             (level (plist-get proj :level))
             (child-level (1+ level))
             (tasks (org-asana--fetch-project gid force))
             (existing (org-asana--existing-asana-entries marker)))
        ;; Separate new tasks from existing
        (let ((new-tasks '())
              (fetched-ids '()))
          ;; Build list of fetched IDs and filter new tasks
          (seq-doseq (task tasks)
            (let ((task-gid (alist-get 'gid task))
                  (completed (eq (alist-get 'completed task) t)))
              (unless (and completed (not org-asana-pull-completed))
                (push task-gid fetched-ids)
                (if (gethash task-gid existing)
                    ;; Existing task - section will be updated below
                    nil
                  ;; New task - annotate with section name
                  (let ((section (org-asana--task-section task gid)))
                    (push `((gid . ,task-gid)
                            (name . ,(alist-get 'name task))
                            (notes . ,(alist-get 'notes task))
                            (_section . ,section))
                          new-tasks))))))
          ;; Insert new tasks
          (setq total-new (+ total-new
                             (org-asana--insert-tasks marker child-level
                                                      (nreverse new-tasks))))
          ;; Update sections and descriptions on existing tasks
          (setq total-updated (+ total-updated
                                 (org-asana--update-existing existing tasks gid)))
          ;; Handle removed tasks
          (setq total-removed (+ total-removed
                                 (org-asana--handle-removed existing fetched-ids))))
        ;; Save the buffer
        (with-current-buffer (find-file-noselect file)
          (save-buffer))))
    (message "org-asana: %d new, %d updated, %d removed"
             total-new total-updated total-removed)))

(defun org-asana-redownload ()
  "Clear the Asana cache and pull fresh data."
  (interactive)
  (when (file-directory-p org-asana-cache-directory)
    (dolist (file (directory-files org-asana-cache-directory t "\\.json$"))
      (delete-file file)))
  (org-asana-pull t))

(defun org-asana-set-states ()
  "Batch-set org TODO states based on :ASANA_SECTION: values.
For each Asana-tracked heading, prompts to set the TODO state."
  (interactive)
  (let ((projects (org-asana--discover-projects))
        (count 0))
    (dolist (proj projects)
      (let* ((marker (plist-get proj :marker))
             (existing (org-asana--existing-asana-entries marker)))
        (maphash
         (lambda (_gid task-marker)
           (with-current-buffer (marker-buffer task-marker)
             (org-with-wide-buffer
              (goto-char task-marker)
              (let* ((section (org-entry-get nil "ASANA_SECTION"))
                     (heading (org-get-heading t t t t))
                     (current-state (org-get-todo-state))
                     (new-state (completing-read
                                 (format "[%s] \"%s\" → state: "
                                         (or section "no section")
                                         heading)
                                 '("TODO" "IN PROGRESS" "WAITING" "DONE" "CANCELLED" "Skip")
                                 nil t)))
                (unless (or (equal new-state "Skip")
                            (equal new-state current-state))
                  (org-todo new-state)
                  (cl-incf count))))))
         existing)))
    (message "org-asana: Updated %d states" count)))

(provide 'org-asana)
;;; org-asana.el ends here
