;;; embellish-theme.el --- Configurable Emacs theme builder -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, themes
;; URL: https://github.com/kwrooijen/embellish

;;; Commentary:

;; Embellish Theme generates complete Emacs themes from 13 semantic colors.
;; Theme files call `embellish-theme-generate' with a name and color palette,
;; producing a standard Emacs theme compatible with `load-theme'.
;;
;; The 13 semantic colors are:
;;   :strong :foreground :subtle :faded :salient :popout
;;   :error :warning :success :info :background :darken :highlight
;;
;; Usage in a theme file:
;;
;;   (require 'embellish-theme)
;;   (embellish-theme-generate 'embellish-winter-night
;;     '(:strong "#FFFFFF" :foreground "#ECEFF4" ...))

;;; Code:

(require 'color)
(require 'cl-lib)

;;;; Palette state

(defvar embellish-theme-palette nil
  "The active color palette plist.")

(defvar embellish-theme-face-registry '()
  "Registry of all generated theme faces.")

;;;; Color access API

(defun embellish-theme-color (name)
  "Get color NAME from the active palette.
NAME should be a keyword like :salient, :error, etc."
  (plist-get embellish-theme-palette name))

(defun embellish-theme-colors ()
  "Return list of all color names in the palette."
  (cl-loop for (key _val) on embellish-theme-palette by #'cddr
           collect key))

;;;; Color utilities

(defun embellish-theme-brighten (hex &optional percent)
  "Adjust brightness of HEX by PERCENT (default 10).
Positive values brighten, negative values darken.
Returns a #RRGGBB string."
  (let* ((p (/ (or percent 10) 100.0))
         (rgb (color-name-to-rgb hex))
         (adjusted
          (mapcar
           (lambda (c)
             (cond
              ((> p 0) (min 1.0 (+ c (* (- 1.0 c) p))))
              ((< p 0) (max 0.0 (* c (+ 1.0 p))))
              (t c)))
           rgb)))
    (apply #'color-rgb-to-hex (append adjusted (list 2)))))

(defun embellish-theme-saturate (hex &optional percent)
  "Adjust saturation of HEX by PERCENT (default 10).
Returns a #RRGGBB string."
  (let* ((p (/ (or percent 10) 100.0))
         (rgb (color-name-to-rgb hex))
         (hsl (apply #'color-rgb-to-hsl rgb))
         (h (nth 0 hsl))
         (s (nth 1 hsl))
         (l (nth 2 hsl))
         (new-s (min 1.0 (+ s (* (- 1.0 s) p))))
         (new-rgb (color-hsl-to-rgb h new-s l)))
    (apply #'color-rgb-to-hex (append new-rgb (list 2)))))

;;;; Face generation system

(defun embellish-theme-define-face (name color &optional bg-color)
  "Define faces for semantic NAME with COLOR.
Creates three faces:
  - embellish-theme-NAME-fg-face (foreground only)
  - embellish-theme-NAME-bg-face (background only)
  - embellish-theme-NAME-fb-face (foreground + background)
Optional BG-COLOR sets a different background for fb-face."
  (let ((fg-face (intern (format "embellish-theme-%s-fg-face" name)))
        (bg-face (intern (format "embellish-theme-%s-bg-face" name)))
        (fb-face (intern (format "embellish-theme-%s-fb-face" name))))
    (unless (facep fg-face)
      (eval `(defface ,fg-face '((t (:foreground ,color))) "")))
    (unless (facep bg-face)
      (eval `(defface ,bg-face '((t (:background ,color))) "")))
    (unless (facep fb-face)
      (eval `(defface ,fb-face '((t (:foreground ,color :background ,(or bg-color color)))) "")))
    (set-face-attribute fg-face nil :foreground color)
    (set-face-attribute bg-face nil :background color)
    (set-face-attribute fb-face nil
                        :foreground color
                        :background (or bg-color color))
    (cl-pushnew fg-face embellish-theme-face-registry)
    (cl-pushnew bg-face embellish-theme-face-registry)
    (cl-pushnew fb-face embellish-theme-face-registry)))

(defun embellish-theme-reset-face-attributes (&rest faces)
  "Reset all attributes of FACES to unspecified."
  (dolist (face faces)
    (set-face-attribute
     face nil
     :foreground 'unspecified
     :background 'unspecified
     :inherit 'unspecified
     :stipple 'unspecified
     :inverse-video 'unspecified
     :box 'unspecified
     :strike-through 'unspecified
     :overline 'unspecified
     :underline 'unspecified
     :slant 'unspecified
     :weight 'unspecified
     :height 'unspecified
     :width 'unspecified
     :foundry 'unspecified
     :family 'unspecified)))

(defun embellish-theme-reset-inherit-face! (&rest face-pairs)
  "Reset FACE-PAIRS and set inheritance.
FACE-PAIRS is a list of (face inherit-face) pairs."
  (dolist (face-pair (-partition 2 face-pairs))
    (let ((face (car face-pair))
          (inherit-face (cadr face-pair)))
      (embellish-theme-reset-face-attributes face)
      (set-face-attribute face nil :inherit inherit-face))))

;;;; Face generation from palette

(defun embellish-theme--generate-faces (palette)
  "Generate face triplets from PALETTE as side effects."
  (embellish-theme-define-face 'background  (plist-get palette :background))
  (embellish-theme-define-face 'foreground  (plist-get palette :foreground))
  (embellish-theme-define-face 'strong      (plist-get palette :strong))
  (embellish-theme-define-face 'faded       (plist-get palette :faded))
  (embellish-theme-define-face 'subtle      (plist-get palette :subtle))
  (embellish-theme-define-face 'salient     (plist-get palette :salient))
  (embellish-theme-define-face 'popout      (plist-get palette :popout))
  (embellish-theme-define-face 'error       (plist-get palette :error))
  (embellish-theme-define-face 'warning     (plist-get palette :warning))
  (embellish-theme-define-face 'success     (plist-get palette :success))
  (embellish-theme-define-face 'info        (plist-get palette :info))
  (embellish-theme-define-face 'darken      (plist-get palette :darken))
  (embellish-theme-define-face 'highlight   (plist-get palette :highlight))
  (embellish-theme-define-face 'faded-darken
                               (plist-get palette :faded)
                               (plist-get palette :darken)))

;;;; Theme face specs

(defun embellish-theme--face-specs (palette)
  "Build the full list of face specs from PALETTE for `custom-theme-set-faces'."
  (let ((strong     (plist-get palette :strong))
        (foreground (plist-get palette :foreground))
        (subtle     (plist-get palette :subtle))
        (faded      (plist-get palette :faded))
        (salient    (plist-get palette :salient))
        (popout     (plist-get palette :popout))
        (err        (plist-get palette :error))
        (warning_   (plist-get palette :warning))
        (success    (plist-get palette :success))
        (info       (plist-get palette :info))
        (background (plist-get palette :background))
        (darken     (plist-get palette :darken))
        (highlight  (plist-get palette :highlight)))
    `(
      ;; Core Emacs
      (default                          ((t (:foreground ,foreground :background ,background))))
      (bold                             ((t (:foreground ,strong :weight bold))))
      (secondary-selection              ((t (:background ,darken))))
      (region                           ((t (:background ,highlight))))
      (highlight                        ((t (:background ,highlight))))
      (success                          ((t (:foreground ,success))))
      (error                            ((t (:foreground ,err))))
      (warning                          ((t (:foreground ,warning_))))
      (button                           ((t (:foreground ,popout))))
      (fringe                           ((t (:background ,background))))
      (minibuffer-prompt                ((t (:foreground ,salient))))
      (header-line                      ((t (:foreground ,foreground))))
      (isearch                          ((t (:background ,faded))))
      (lazy-highlight                   ((t (:background ,subtle))))
      (match                            ((t (:inherit bold))))

      ;; Font lock
      (font-lock-comment-face           ((t (:foreground ,faded))))
      (font-lock-doc-face               ((t (:foreground ,faded))))
      (font-lock-string-face            ((t (:foreground ,popout))))
      (font-lock-constant-face          ((t (:foreground ,salient))))
      (font-lock-warning-face           ((t (:foreground ,popout))))
      (font-lock-function-name-face     ((t (:foreground ,strong))))
      (font-lock-variable-name-face     ((t (:foreground ,strong))))
      (font-lock-builtin-face           ((t (:foreground ,salient))))
      (font-lock-type-face              ((t (:foreground ,salient))))
      (font-lock-keyword-face           ((t (:foreground ,salient))))

      ;; Dired
      (dired-directory                  ((t (:foreground ,salient))))

      ;; HL Line
      (hl-line                          ((t (:background ,highlight))))

      ;; Mode line
      (mode-line                        ((t (:background ,darken))))
      (mode-line-active                 ((t (:background ,darken))))
      (mode-line-inactive               ((t (:background ,darken))))
      (mode-line-highlight              ((t nil)))

      ;; Paren
      (show-paren-match                 ((t (:foreground ,salient))))
      (show-paren-mismatch              ((t (:foreground ,err))))
      (show-paren-match-expression      ((t (:foreground ,popout))))

      ;; Org
      (org-block                        ((t (:background ,darken :extend t))))
      (org-quote                        ((t (:background ,darken :extend t))))
      (org-block-begin-line             ((t (:foreground ,faded :background ,darken :extend t))))
      (org-block-end-line               ((t (:foreground ,faded :background ,darken :extend t))))
      (org-code                         ((t (:foreground ,salient))))
      (org-link                         ((t (:foreground ,salient))))
      (org-table                        ((t (:foreground ,salient))))
      (org-todo                         ((t (:foreground ,salient))))
      (org-tag                          ((t (:foreground ,popout))))
      (org-verbatim                     ((t (:foreground ,popout))))
      (org-warning                      ((t (:foreground ,popout))))
      (org-priority                     ((t (:foreground ,err))))
      (org-agenda-current-time          ((t (:foreground ,strong))))
      (org-agenda-structure             ((t (:foreground ,strong))))
      (org-level-1                      ((t (:foreground ,foreground))))
      (org-level-2                      ((t (:foreground ,foreground))))
      (org-level-3                      ((t (:foreground ,foreground))))
      (org-level-4                      ((t (:foreground ,foreground))))
      (org-level-5                      ((t (:foreground ,foreground))))
      (org-level-6                      ((t (:foreground ,foreground))))
      (org-level-7                      ((t (:foreground ,foreground))))
      (org-level-8                      ((t (:foreground ,foreground))))
      (org-done                         ((t (:foreground ,foreground))))
      (org-agenda-calendar-event        ((t (:foreground ,foreground))))
      (org-upcoming-deadline            ((t (:foreground ,foreground))))

      ;; Org faded faces
      (org-agenda-clocking              ((t (:foreground ,faded))))
      (org-agenda-column-dateline       ((t (:foreground ,faded))))
      (org-agenda-date-weekend          ((t (:foreground ,faded))))
      (org-agenda-diary                 ((t (:foreground ,faded))))
      (org-agenda-dimmed-todo-face      ((t (:foreground ,faded))))
      (org-agenda-done                  ((t (:foreground ,faded))))
      (org-agenda-filter-category       ((t (:foreground ,faded))))
      (org-agenda-filter-effort         ((t (:foreground ,faded))))
      (org-agenda-filter-regexp         ((t (:foreground ,faded))))
      (org-agenda-filter-tags           ((t (:foreground ,faded))))
      (org-agenda-restriction-lock      ((t (:foreground ,faded))))
      (org-archived                     ((t (:foreground ,faded))))
      (org-checkbox                     ((t (:foreground ,faded))))
      (org-checkbox-statistics-done     ((t (:foreground ,faded))))
      (org-checkbox-statistics-todo     ((t (:foreground ,faded))))
      (org-clock-overlay                ((t (:foreground ,faded))))
      (org-column                       ((t (:foreground ,faded))))
      (org-column-title                 ((t (:foreground ,faded))))
      (org-date                         ((t (:foreground ,faded))))
      (org-date-selected                ((t (:foreground ,faded))))
      (org-default                      ((t (:foreground ,faded))))
      (org-document-info                ((t (:foreground ,faded))))
      (org-document-info-keyword        ((t (:foreground ,faded))))
      (org-document-title               ((t (:foreground ,faded))))
      (org-drawer                       ((t (:foreground ,faded))))
      (org-ellipsis                     ((t (:foreground ,faded))))
      (org-footnote                     ((t (:foreground ,faded))))
      (org-formula                      ((t (:foreground ,faded))))
      (org-headline-done                ((t (:foreground ,faded))))
      (org-latex-and-related            ((t (:foreground ,faded))))
      (org-list-dt                      ((t (:foreground ,faded))))
      (org-macro                        ((t (:foreground ,faded))))
      (org-meta-line                    ((t (:foreground ,faded))))
      (org-mode-line-clock              ((t (:foreground ,faded))))
      (org-mode-line-clock-overrun      ((t (:foreground ,faded))))
      (org-property-value               ((t (:foreground ,faded))))
      (org-scheduled                    ((t (:foreground ,faded))))
      (org-scheduled-previously         ((t (:foreground ,faded))))
      (org-sexp-date                    ((t (:foreground ,faded))))
      (org-special-keyword              ((t (:foreground ,faded))))
      (org-tag-group                    ((t (:foreground ,faded))))
      (org-target                       ((t (:foreground ,faded))))
      (org-time-grid                    ((t (:foreground ,faded))))
      (org-verse                        ((t (:foreground ,faded))))

      ;; Org Agenda
      (org-agenda-calendar-sexp         ((t (:foreground ,salient))))
      (org-agenda-date                  ((t (:foreground ,salient))))

      ;; Org Habit
      (org-habit-clear-future-face      ((t (:foreground ,salient :background ,salient))))
      (org-habit-alert-face             ((t (:foreground ,warning_ :background ,warning_))))
      (org-habit-overdue-future-face    ((t (:foreground ,err :background ,err))))

      ;; Magit
      (magit-branch                     ((t (:foreground ,info :weight bold))))
      (magit-diff-context-highlight     ((t (:background ,darken))))
      (magit-diff-file-header           ((t (:foreground ,info :box (:color ,info)))))
      (magit-diffstat-added             ((t (:foreground ,success))))
      (magit-diffstat-removed           ((t (:foreground ,err))))
      (magit-hash                       ((t (:foreground ,info))))
      (magit-hunk-heading               ((t (:foreground ,salient))))
      (magit-hunk-heading-highlight     ((t (:foreground ,salient :background ,subtle))))
      (magit-item-highlight             ((t (:foreground ,info :background ,subtle))))
      (magit-log-author                 ((t (:foreground ,info))))
      (magit-process-ng                 ((t (:foreground ,warning_ :weight bold))))
      (magit-process-ok                 ((t (:foreground ,success :weight bold))))
      (magit-section-heading            ((t (:foreground ,info :weight bold))))
      (magit-section-highlight          ((t (:background ,highlight))))
      (magit-diff-hunk-heading-highlight ((t (:background ,background))))
      (magit-diff-hunk-heading          ((t (:background ,background))))

      ;; Diredfl
      (diredfl-file-name                ((t (:foreground ,foreground))))
      (diredfl-file-suffix              ((t (:foreground ,foreground))))
      (diredfl-dir-name                 ((t (:foreground ,salient))))
      (diredfl-dir-heading              ((t (:foreground ,salient))))
      (diredfl-dir-priv                 ((t (:foreground ,subtle))))
      (diredfl-date-time                ((t (:foreground ,faded))))
      (diredfl-read-priv                ((t (:foreground ,subtle))))
      (diredfl-write-priv               ((t (:foreground ,subtle))))
      (diredfl-exec-priv                ((t (:foreground ,subtle))))
      (diredfl-number                   ((t (:foreground ,subtle))))
      (diredfl-no-priv                  ((t (:foreground ,subtle))))
      (diredfl-other-priv               ((t (:foreground ,subtle))))
      (diredfl-link-priv                ((t (:foreground ,subtle))))
      (diredfl-symlink                  ((t (:foreground ,faded))))
      (diredfl-compressed-file-name     ((t (:foreground ,faded))))
      (diredfl-compressed-file-suffix   ((t (:foreground ,faded))))
      (diredfl-ignored-file-name        ((t nil)))

      ;; Iedit
      (iedit-occurrence                 ((t (:foreground ,strong :background ,salient))))

      ;; Evil search
      (evil-search-highlight-persist-highlight-face ((t (:inherit highlight))))

      ;; Anzu
      (anzu-mode-line                   ((t (:foreground ,salient))))

      ;; Web mode
      (web-mode-string-face             ((t (:inherit font-lock-string-face))))
      (web-mode-html-attr-value-face    ((t (:inherit font-lock-string-face))))
      (web-mode-symbol-face             ((t (:inherit font-lock-keyword-face))))
      (web-mode-builtin-face            ((t (:foreground ,strong))))
      (web-mode-variable-name-face      ((t (:inherit font-lock-variable-name-face))))
      (web-mode-current-element-highlight-face ((t (:inherit underline))))

      ;; Ediff
      (ediff-even-diff-A                ((t (:inherit default))))
      (ediff-even-diff-B                ((t (:inherit default))))
      (ediff-odd-diff-A                 ((t (:inherit org-warning))))
      (ediff-odd-diff-B                 ((t (:inherit org-warning))))

      ;; Pulse
      (pulse-highlight-face             ((t (:background ,salient))))
      (pulse-highlight-start-face       ((t (:background ,salient))))

      ;; Shr
      (shr-text                         ((t (:weight light))))

      ;; Corfu
      (corfu-default                    ((t (:background ,darken))))
      (corfu-current                    ((t (:inherit hl-line))))

      ;; Consult
      (consult-file                     ((t (:foreground ,faded))))
      (consult-preview-line             ((t (:inherit hl-line))))
      (consult-highlight-match          ((t (:inherit bold))))
      (consult-preview-match            ((t (:inherit bold))))
      (consult-line-number              ((t (:inherit font-lock-warning-face))))
      (consult-line-number-prefix       ((t (:inherit font-lock-warning-face))))

      ;; Orderless
      (orderless-match-face-0           ((t (:inherit bold :foreground unspecified))))
      (orderless-match-face-1           ((t (:inherit bold :foreground unspecified))))
      (orderless-match-face-2           ((t (:inherit bold :foreground unspecified))))
      (orderless-match-face-3           ((t (:inherit bold :foreground unspecified))))

      ;; Wgrep
      (wgrep-face                       ((t (:inherit warning))))

      ;; Elfeed
      (elfeed-search-feed-face          ((t (:foreground ,salient))))
      (elfeed-search-tag-face           ((t (:foreground ,popout))))
      (elfeed-search-date-face          ((t (:foreground ,faded))))

      ;; Message
      (message-header-to                ((t (:foreground ,salient))))
      (message-header-subject           ((t (:foreground ,strong))))
      (message-header-other             ((t (:foreground ,faded))))
      (message-header-name              ((t (:foreground ,popout))))

      ;; Eshell
      (eshell-ls-directory              ((t (:inherit dired-directory))))
      (eshell-prompt                    ((t (:inherit minibuffer-prompt))))

      ;; Diff
      (diff-refine-added                ((t (:background "#276128"))))
      (diff-refine-removed              ((t (:background "#612828"))))

      ;; Embellish subwindow
      (embellish-subwindow-face         ((t (:background ,darken))))
      )))

;;;; Theme generator

;;;###autoload
(defun embellish-theme-generate (theme-name palette)
  "Generate an Emacs theme named THEME-NAME from PALETTE.
PALETTE is a plist of 13 semantic colors.  This function:
1. Stores the palette in `embellish-theme-palette'
2. Generates face triplets as side effects
3. Calls `deftheme' + `custom-theme-set-faces' + `provide-theme'
4. Updates the default face on the current frame."
  (setq embellish-theme-palette palette)

  ;; Generate face triplets (side effect — always reflects last loaded theme)
  (embellish-theme--generate-faces palette)

  ;; Define the theme
  (custom-declare-theme theme-name nil)

  ;; Apply all face specs
  (apply #'custom-theme-set-faces
         theme-name
         (embellish-theme--face-specs palette))

  ;; Provide it
  (provide-theme theme-name)

  ;; Update current frame
  (set-face-attribute 'default nil
                      :foreground (plist-get palette :foreground)
                      :background (plist-get palette :background))
  (set-frame-parameter nil 'background-color (plist-get palette :background)))

(provide 'embellish-theme)

;;; embellish-theme.el ends here
