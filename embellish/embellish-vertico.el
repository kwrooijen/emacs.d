;;; embellish-vertico.el --- Vertico padding integration -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; URL: https://github.com/kwrooijen/embellish

;;; Commentary:

;; Adds top/bottom padding to the Vertico completion list and hides
;; the minibuffer truncation indicator.  Loaded automatically by
;; `embellish-mode' when Vertico is present.

;;; Code:

(defcustom embellish-vertico-top-padding t
  "When non-nil, add a blank line above the candidate list."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-vertico-bottom-padding t
  "When non-nil, add extra height below the candidate list."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-vertico-min-height t
  "When non-nil, enforce minimum minibuffer height from `vertico-count'."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-vertico-hide-truncation t
  "When non-nil, hide the truncation indicator in the minibuffer."
  :type 'boolean
  :group 'embellish)

(defun embellish-vertico--set-min-height ()
  "Enforce minimum minibuffer window height based on `vertico-count'."
  (when (bound-and-true-p vertico-count)
    (let ((win (active-minibuffer-window)))
      (when (window-live-p win)
        (with-selected-window win
          (let ((delta (- vertico-count (window-height))))
            (when (/= delta 0)
              (enlarge-window delta))))))))

(defun embellish-vertico--add-top-padding (orig-fun lines)
  "Add an empty visual line above the Vertico candidate list."
  (funcall
   orig-fun
   (if (or (null lines)
           (bound-and-true-p vertico-flat-mode)
           (bound-and-true-p vertico-buffer-mode)
           (bound-and-true-p vertico-grid-mode))
       lines
     (cons "\n" lines))))

(defun embellish-vertico--add-bottom-padding (orig-func height)
  "Add extra padding to Vertico completion window height.
Ensures the minimum height accounts for all padding lines, since
vertico clamps small heights to `vertico-count' internally which
would otherwise swallow the padding."
  (let ((top (if embellish-vertico-top-padding 1 0)))
    (funcall orig-func (max (+ height 1) (+ vertico-count top 1)))))

(defun embellish-vertico--hide-truncation ()
  "Hide the truncation indicator in the minibuffer.
Replaces the `$' character with a space styled to blend into the
background."
  (let ((table (or buffer-display-table (make-display-table))))
    (set-display-table-slot
     table 'truncation
     (make-glyph-code ?\  'embellish-subwindow-face))
    (setq-local buffer-display-table table)))

;;;###autoload
(define-minor-mode embellish-vertico-mode
  "Global minor mode for Vertico padding and truncation hiding."
  :global t
  :group 'embellish
  (if embellish-vertico-mode
      (progn
        (when embellish-vertico-top-padding
          (advice-add #'vertico--display-candidates :around #'embellish-vertico--add-top-padding))
        (when embellish-vertico-bottom-padding
          (advice-add #'vertico--resize-window :around #'embellish-vertico--add-bottom-padding))
        (when embellish-vertico-hide-truncation
          (add-hook 'minibuffer-setup-hook #'embellish-vertico--hide-truncation))
        (when embellish-vertico-min-height
          (add-hook 'minibuffer-setup-hook #'embellish-vertico--set-min-height)))
    (advice-remove #'vertico--display-candidates #'embellish-vertico--add-top-padding)
    (advice-remove #'vertico--resize-window #'embellish-vertico--add-bottom-padding)
    (remove-hook 'minibuffer-setup-hook #'embellish-vertico--hide-truncation)
    (remove-hook 'minibuffer-setup-hook #'embellish-vertico--set-min-height)))

(provide 'embellish-vertico)

;;; embellish-vertico.el ends here
