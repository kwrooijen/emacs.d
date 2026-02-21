;;; embellish-org.el --- Visual enhancements for Org mode -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; URL: https://github.com/kwrooijen/embellish

;;; Commentary:

;; Purely visual enhancements for Org mode: prettify symbols, emphasis
;; markers, heading styling, bullet faces, and backtick emphasis.
;; Loaded automatically by `embellish-mode' when `embellish-org' is
;; non-nil and Org is available.

;;; Code:

(defcustom embellish-org-prettify-symbols t
  "When non-nil, replace block delimiters with symbols.
Source blocks use `embellish-org-src-begin-symbol' and
`embellish-org-src-end-symbol'.  Quote and example blocks use
curly quotes."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-org-src-begin-symbol ?\u00BB
  "Character to display for #+BEGIN_SRC."
  :type 'character
  :group 'embellish)

(defcustom embellish-org-src-end-symbol ?\u00AB
  "Character to display for #+END_SRC."
  :type 'character
  :group 'embellish)

(defcustom embellish-org-hide-emphasis-markers t
  "When non-nil, hide emphasis markers (*, /, _, etc.)."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-org-bold-headings t
  "When non-nil, make org heading levels 1-5 bold."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-org-style-meta-line t
  "When non-nil, style org-meta-line as italic fixed-pitch."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-org-clean-column-faces t
  "When non-nil, remove background from org-column faces."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-org-bullet-face t
  "When non-nil, add font-lock for list bullet characters."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-org-backtick-emphasis t
  "When non-nil, style backtick-quoted text with `font-lock-keyword-face'."
  :type 'boolean
  :group 'embellish)

(defun embellish-org--apply-prettify-symbols ()
  "Set up prettify-symbols for org block delimiters."
  (let ((begin (string embellish-org-src-begin-symbol))
        (end (string embellish-org-src-end-symbol)))
    (setq-local prettify-symbols-alist
                `(("#+BEGIN_SRC"     . ,begin)
                  ("#+END_SRC"       . ,end)
                  ("#+begin_src"     . ,begin)
                  ("#+end_src"       . ,end)
                  ("#+begin_quote"   . "\u201C")
                  ("#+end_quote"     . "\u201D")
                  ("#+BEGIN_QUOTE"   . "\u201C")
                  ("#+END_QUOTE"     . "\u201D")
                  ("#+begin_example" . "\u201C")
                  ("#+end_example"   . "\u201D")
                  ("#+BEGIN_EXAMPLE" . "\u201C")
                  ("#+END_EXAMPLE"   . "\u201D")))
    (prettify-symbols-mode 1)))

(defun embellish-org--apply-faces ()
  "Apply org face customizations."
  (when embellish-org-bold-headings
    (dolist (face '(org-level-1 org-level-2 org-level-3
                    org-level-4 org-level-5))
      (set-face-attribute face nil :bold t)))
  (when embellish-org-style-meta-line
    (set-face-attribute 'org-meta-line nil :italic t :inherit 'fixed-pitch))
  (when embellish-org-clean-column-faces
    (set-face-attribute 'org-column nil :background 'unspecified)
    (set-face-attribute 'org-column-title nil :background 'unspecified)))

(defun embellish-org--apply-font-lock ()
  "Add font-lock keywords for bullets and backtick emphasis."
  (when embellish-org-bullet-face
    (font-lock-add-keywords
     'org-mode
     '(("^\\([0-9]+[.)]\\|\\+\\|\\-\\) " 1 'org-bullet))
     'append))
  (when embellish-org-backtick-emphasis
    (add-to-list 'org-emphasis-alist '("`" (:inherit font-lock-keyword-face)))
    (font-lock-add-keywords
     'org-mode
     '(("\\(`\\([^`]+\\)`\\)" 1 'font-lock-keyword-face))
     'append)))

(defun embellish-org--setup-hook ()
  "Hook function for `org-mode-hook'."
  (when embellish-org-prettify-symbols
    (embellish-org--apply-prettify-symbols)))

(defun embellish-org--refresh-existing-buffers ()
  "Apply prettify-symbols and refontify all existing Org buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode)
        (embellish-org--setup-hook)
        (font-lock-flush)))))

;;;###autoload
(define-minor-mode embellish-org-mode
  "Global minor mode for Org visual enhancements."
  :global t
  :group 'embellish
  (if embellish-org-mode
      (progn
        (when embellish-org-hide-emphasis-markers
          (setq org-hide-emphasis-markers t))
        (embellish-org--apply-faces)
        (embellish-org--apply-font-lock)
        (add-hook 'org-mode-hook #'embellish-org--setup-hook)
        (embellish-org--refresh-existing-buffers))
    (remove-hook 'org-mode-hook #'embellish-org--setup-hook)))

(provide 'embellish-org)

;;; embellish-org.el ends here
