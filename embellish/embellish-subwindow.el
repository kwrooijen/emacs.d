;;; embellish-subwindow.el --- Subwindow styling for Emacs -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; URL: https://github.com/kwrooijen/embellish

;;; Commentary:

;; Gives special buffers (REPLs, Magit, Dired, shells) a distinct
;; background and padding.  Loaded automatically by `embellish-mode'
;; when `embellish-subwindow' is non-nil.

;;; Code:

(defcustom embellish-subwindow-fringe-width 24
  "Fringe width (pixels) for subwindow buffers."
  :type 'integer
  :group 'embellish)

(defcustom embellish-subwindow-header-height 190
  "Header line height for subwindow padding."
  :type 'integer
  :group 'embellish)

(defcustom embellish-subwindow-hooks
  '(cider-repl-mode-hook
    magit-mode-hook
    dired-mode-hook
    sql-interactive-mode-hook
    eshell-mode-hook
    inf-ruby-mode-hook
    messages-buffer-mode-hook
    special-mode-hook)
  "List of mode hooks that trigger subwindow styling."
  :type '(repeat symbol)
  :group 'embellish)

(defcustom embellish-subwindow-buffers
  '("\\*Messages\\*" "\\*Warnings\\*" "Claude Code Agent.*" "\\*Org Agenda\\*")
  "List of regexps matched against buffer names for subwindow styling.
Matched buffers are styled eagerly when `embellish-subwindow-mode' is
enabled, and continuously as new buffers become visible."
  :type '(repeat regexp)
  :group 'embellish)

(defcustom embellish-subwindow-style-transient t
  "When non-nil, also style transient popup buffers."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-subwindow-style-minibuffer t
  "When non-nil, style the minibuffer with the subwindow background."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-subwindow-minibuffer-margins 2
  "Left and right margin width for the minibuffer."
  :type 'integer
  :group 'embellish)

(defvar-local embellish-subwindow--needs-style nil
  "Buffer-local flag indicating this buffer needs subwindow styling.")

(defvar-local embellish-subwindow--styled nil
  "Buffer-local flag indicating subwindow styling has been applied.")

(defun embellish-subwindow--bg ()
  "Return the background color for subwindow styling, or nil.
Reads from the `:background' attribute of `embellish-subwindow-face'."
  (let ((bg (face-attribute 'embellish-subwindow-face :background nil t)))
    (unless (or (null bg) (eq bg 'unspecified))
      bg)))

(defun embellish-subwindow--style (win)
  "Apply subwindow styling to window WIN."
  (when-let* ((bg (and embellish-subwindow--needs-style
                       (not embellish-subwindow--styled)
                       (embellish-subwindow--bg))))
    (set-window-fringes win
                        embellish-subwindow-fringe-width
                        embellish-subwindow-fringe-width)
    (setq header-line-format " ")
    (face-remap-add-relative 'fringe
                              :foreground bg
                              :background bg)
    (face-remap-add-relative 'header-line
                              :background bg
                              :height embellish-subwindow-header-height)
    (face-remap-add-relative 'default
                              :background bg)
    (setq embellish-subwindow--styled t)))

(defun embellish-subwindow--setup ()
  "Mark the current buffer as needing subwindow styling."
  (setq embellish-subwindow--needs-style t))

(defun embellish-subwindow--setup-transient ()
  "Mark a transient buffer as needing subwindow styling."
  (when (and (boundp 'transient--buffer-name)
             (get-buffer (or transient--buffer-name "")))
    (with-current-buffer (get-buffer (or transient--buffer-name ""))
      (setq embellish-subwindow--needs-style t))))

(defun embellish-subwindow--buffer-match-p (name)
  "Return non-nil if buffer NAME matches any `embellish-subwindow-buffers' regexp."
  (cl-some (lambda (re) (string-match-p re name))
           embellish-subwindow-buffers))

(defun embellish-subwindow--restyle ()
  "Restyle all visible windows that need subwindow styling."
  (dolist (win (window-list))
    (with-current-buffer (window-buffer win)
      (when (and (not embellish-subwindow--needs-style)
                 (embellish-subwindow--buffer-match-p (buffer-name)))
        (setq embellish-subwindow--needs-style t))
      (embellish-subwindow--style win))))

(defun embellish-subwindow--minibuffer-hook ()
  "Apply subwindow styling to the minibuffer."
  (when-let* ((bg (embellish-subwindow--bg)))
    (face-remap-add-relative 'default :background bg)
    (face-remap-add-relative 'minibuffer-prompt :background bg))
  (set-window-margins (minibuffer-window)
                      embellish-subwindow-minibuffer-margins
                      embellish-subwindow-minibuffer-margins))

(defun embellish-subwindow--add-hooks ()
  "Register all subwindow hooks."
  (add-hook 'window-configuration-change-hook #'embellish-subwindow--restyle)
  (dolist (hook embellish-subwindow-hooks)
    (add-hook hook #'embellish-subwindow--setup))
  (when embellish-subwindow-style-transient
    (add-hook 'transient-setup-buffer-hook #'embellish-subwindow--setup-transient))
  (when embellish-subwindow-style-minibuffer
    (add-hook 'minibuffer-setup-hook #'embellish-subwindow--minibuffer-hook))
  (dolist (buf (buffer-list))
    (when (embellish-subwindow--buffer-match-p (buffer-name buf))
      (with-current-buffer buf
        (setq embellish-subwindow--needs-style t)))))

(defun embellish-subwindow--remove-hooks ()
  "Remove all subwindow hooks."
  (remove-hook 'window-configuration-change-hook #'embellish-subwindow--restyle)
  (dolist (hook embellish-subwindow-hooks)
    (remove-hook hook #'embellish-subwindow--setup))
  (remove-hook 'transient-setup-buffer-hook #'embellish-subwindow--setup-transient)
  (remove-hook 'minibuffer-setup-hook #'embellish-subwindow--minibuffer-hook))

;;;###autoload
(define-minor-mode embellish-subwindow-mode
  "Global minor mode for styling subwindow buffers.
Gives special buffers (REPLs, Magit, Dired, etc.) a distinct
background and padding.  The background color is read from
`embellish-subwindow-face'."
  :global t
  :group 'embellish
  (if embellish-subwindow-mode
      (embellish-subwindow--add-hooks)
    (embellish-subwindow--remove-hooks)))

(provide 'embellish-subwindow)

;;; embellish-subwindow.el ends here
