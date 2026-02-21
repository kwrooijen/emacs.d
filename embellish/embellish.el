;;; embellish.el --- Embellish your Emacs frame and windows -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, frames
;; URL: https://github.com/kwrooijen/embellish

;;; Commentary:

;; Embellish provides a clean, minimal Emacs appearance by consolidating
;; frame chrome, spacing, scrolling, and subwindow styling into a single
;; configurable package.
;;
;; Enable `embellish-mode' for frame-level appearance (chrome, borders,
;; dividers, scrolling).  Enable `embellish-subwindow-mode' to give
;; special buffers (REPLs, Magit, Dired, etc.) a distinct background
;; and padding.
;;
;; The subwindow background is controlled by the `embellish-subwindow-face'
;; face.  When using `embellish-theme', this face is set automatically
;; by the theme.  Without a theme, set it manually:
;;
;;   (set-face-attribute 'embellish-subwindow-face nil :background "#1e2030")
;;
;; Each feature group can be disabled entirely via its group toggle
;; (e.g. `embellish-chrome', `embellish-spacing'), or individual
;; settings can be toggled within an enabled group.

;;; Code:

;;;; Customization group

(defgroup embellish nil
  "Embellish your Emacs frame and windows."
  :group 'faces
  :prefix "embellish-")

;;;; Subwindow face

(defface embellish-subwindow-face
  '((t nil))
  "Face used for subwindow background styling.
The `:background' attribute of this face is applied to subwindow
buffers (fringes, header lines, default background, minibuffer).
Set via a theme (e.g. embellish-winter-night sets it to the darken
color) or manually:

  (set-face-attribute \\='embellish-subwindow-face nil :background \"#1e2030\")"
  :group 'embellish)

;;;; Group toggles

(defcustom embellish-chrome t
  "When non-nil, apply frame chrome settings.
This controls the entire chrome group (toolbar, menubar, scrollbar,
cursor, fringes, titlebar, proxy icon).  Set to nil to skip all
chrome modifications.  Individual settings within the group can
still be toggled when the group is enabled."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-spacing t
  "When non-nil, apply spacing settings.
This controls the entire spacing group (internal border, window
divider).  Set to nil to skip all spacing modifications."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-scrolling t
  "When non-nil, apply scrolling settings.
This controls the entire scrolling group.  Set to nil to skip
all scrolling modifications."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-subwindow t
  "When non-nil, enable subwindow styling.
Loads `embellish-subwindow' and enables `embellish-subwindow-mode'."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-font t
  "When non-nil, apply font settings on enable.
Calls `embellish-font-apply' from `embellish-font'."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-vertico t
  "When non-nil, enable Vertico padding integration.
Automatically loads `embellish-vertico' and enables
`embellish-vertico-mode' when Vertico is available."
  :type 'boolean
  :group 'embellish)

;;;; Frame chrome

(defcustom embellish-hide-toolbar t
  "When non-nil, disable the tool bar."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-hide-menubar t
  "When non-nil, disable the menu bar."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-hide-scrollbar t
  "When non-nil, disable the scroll bar."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-hide-cursor-in-inactive-windows t
  "When non-nil, hide the cursor in non-selected windows."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-disable-cursor-blink t
  "When non-nil, disable cursor blinking."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-hide-fringes t
  "When non-nil, set all fringes to zero width."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-transparent-titlebar t
  "When non-nil, use a transparent titlebar on macOS."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-hide-title t
  "When non-nil, hide the buffer name from the frame title bar.
Uses a space/unicode toggle trick to prevent window size display."
  :type 'boolean
  :group 'embellish)

(defcustom embellish-hide-proxy-icon t
  "When non-nil, hide the macOS proxy icon in the title bar."
  :type 'boolean
  :group 'embellish)

;;;; Spacing

(defcustom embellish-internal-border-width 24
  "Internal border width (pixels) for frame margins."
  :type 'integer
  :group 'embellish)

(defcustom embellish-window-divider-width 24
  "Width (pixels) of the window divider."
  :type 'integer
  :group 'embellish)

(defcustom embellish-window-divider-places 'right-only
  "Where to show window dividers."
  :type '(choice (const right-only)
                 (const bottom-only)
                 (const t))
  :group 'embellish)

;;;; Scrolling

(defcustom embellish-scroll-margin 1
  "Number of lines of margin at top and bottom of a window."
  :type 'integer
  :group 'embellish)

(defcustom embellish-scroll-conservatively 10000
  "Scroll conservatively when point moves off-screen."
  :type 'integer
  :group 'embellish)

(defcustom embellish-scroll-step 1
  "Number of lines to scroll when point moves off-screen."
  :type 'integer
  :group 'embellish)

;;;; Visual line mode

(defcustom embellish-visual-line-mode t
  "When non-nil, enable `global-visual-line-mode'."
  :type 'boolean
  :group 'embellish)

;;;; Internal state

(defvar embellish--title-hook-fn nil
  "Stored function for the frame title toggle hook.")

(defvar embellish--saved-frame-title-format nil
  "Saved `frame-title-format' before embellish modified it.")

(defvar embellish--saved-scroll-margin nil)
(defvar embellish--saved-scroll-conservatively nil)
(defvar embellish--saved-scroll-step nil)
(defvar embellish--saved-auto-window-vscroll nil)

(defvar embellish--saved-scroll-error-top-bottom nil)
(defvar embellish--saved-scroll-preserve-screen-position nil)

;;;; Frame chrome implementation

(defun embellish--apply-chrome ()
  "Apply frame chrome settings."
  (when embellish-chrome
    (when embellish-hide-toolbar
      (tool-bar-mode -1))
    (when embellish-hide-menubar
      (menu-bar-mode -1))
    (when embellish-hide-scrollbar
      (scroll-bar-mode -1))
    (when embellish-disable-cursor-blink
      (blink-cursor-mode -1))
    (when embellish-hide-cursor-in-inactive-windows
      (setq-default cursor-in-non-selected-windows nil))
    (when embellish-hide-fringes
      (set-fringe-mode 0)
      (setq left-fringe-width 0)
      (setq right-fringe-width 0))
    (when embellish-transparent-titlebar
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))
    (when embellish-hide-title
      (setq embellish--saved-frame-title-format frame-title-format)
      (setq-default frame-title-format " ")
      (setq embellish--title-hook-fn
            (lambda (_frame)
              (if (equal frame-title-format " ")
                  (setq frame-title-format "\u3164")
                (setq frame-title-format " "))))
      (add-hook 'window-size-change-functions embellish--title-hook-fn))
    (when embellish-hide-proxy-icon
      (setq-default ns-use-proxy-icon nil))))

(defun embellish--revert-chrome ()
  "Revert frame chrome settings."
  (tool-bar-mode 1)
  (menu-bar-mode 1)
  (scroll-bar-mode 1)
  (blink-cursor-mode 1)
  (setq-default cursor-in-non-selected-windows t)
  (set-fringe-mode nil)
  (setq left-fringe-width nil)
  (setq right-fringe-width nil)
  (setq default-frame-alist
        (assq-delete-all 'ns-transparent-titlebar default-frame-alist))
  (when embellish--title-hook-fn
    (remove-hook 'window-size-change-functions embellish--title-hook-fn)
    (setq embellish--title-hook-fn nil))
  (when embellish--saved-frame-title-format
    (setq-default frame-title-format embellish--saved-frame-title-format))
  (setq-default ns-use-proxy-icon t))

;;;; Spacing implementation

(defun embellish--update-divider-colors ()
  "Update window divider colors to match the default face background."
  (let ((bg (face-attribute 'default :background)))
    (dolist (face '(window-divider
                    window-divider-first-pixel
                    window-divider-last-pixel
                    vertical-border))
      (set-face-attribute face nil
                          :foreground bg
                          :background bg))))

(defun embellish--on-theme-change (&rest _)
  "Hook run after a theme is loaded to update divider colors."
  (when (and embellish-mode embellish-spacing)
    (embellish--update-divider-colors)))

(defun embellish--apply-spacing ()
  "Apply spacing settings."
  (when embellish-spacing
    (push `(internal-border-width . ,embellish-internal-border-width)
          default-frame-alist)
    (set-frame-parameter nil 'internal-border-width embellish-internal-border-width)
    (embellish--update-divider-colors)
    (setq window-divider-default-right-width embellish-window-divider-width)
    (setq window-divider-default-places embellish-window-divider-places)
    (window-divider-mode 1)
    (add-hook 'enable-theme-functions #'embellish--on-theme-change)))

(defun embellish--revert-spacing ()
  "Revert spacing settings."
  (setq default-frame-alist
        (assq-delete-all 'internal-border-width default-frame-alist))
  (set-frame-parameter nil 'internal-border-width 0)
  (window-divider-mode -1)
  (remove-hook 'enable-theme-functions #'embellish--on-theme-change))

;;;; Scrolling implementation

(defun embellish--apply-scrolling ()
  "Apply scrolling settings."
  (when embellish-scrolling
    (setq embellish--saved-scroll-margin scroll-margin)
    (setq embellish--saved-scroll-conservatively scroll-conservatively)
    (setq embellish--saved-scroll-step scroll-step)
    (setq embellish--saved-auto-window-vscroll auto-window-vscroll)
    (setq embellish--saved-scroll-error-top-bottom scroll-error-top-bottom)
    (setq embellish--saved-scroll-preserve-screen-position scroll-preserve-screen-position)
    (setq scroll-margin embellish-scroll-margin
          scroll-conservatively embellish-scroll-conservatively
          scroll-step embellish-scroll-step
          auto-window-vscroll nil
          scroll-error-top-bottom t
          scroll-preserve-screen-position t)))

(defun embellish--revert-scrolling ()
  "Revert scrolling settings."
  (setq scroll-margin (or embellish--saved-scroll-margin 0))
  (setq scroll-conservatively (or embellish--saved-scroll-conservatively 0))
  (setq scroll-step (or embellish--saved-scroll-step 0))
  (setq auto-window-vscroll (or embellish--saved-auto-window-vscroll t))
  (setq scroll-error-top-bottom embellish--saved-scroll-error-top-bottom)
  (setq scroll-preserve-screen-position embellish--saved-scroll-preserve-screen-position))

;;;; Visual line mode implementation

(defun embellish--apply-visual-line ()
  "Apply visual line mode."
  (when embellish-visual-line-mode
    (global-visual-line-mode 1)))

(defun embellish--revert-visual-line ()
  "Revert visual line mode."
  (global-visual-line-mode -1))

;;;; Subwindow integration

(defun embellish--setup-subwindow ()
  "Enable subwindow styling."
  (when embellish-subwindow
    (require 'embellish-subwindow)
    (embellish-subwindow-mode 1)))

(defun embellish--teardown-subwindow ()
  "Disable subwindow styling."
  (when (fboundp 'embellish-subwindow-mode)
    (embellish-subwindow-mode -1)))

;;;; Font integration

(defun embellish--setup-font ()
  "Apply font settings."
  (when embellish-font
    (require 'embellish-font)
    (embellish-font-apply)))

(defun embellish--teardown-font ()
  "No-op; font settings persist until changed."
  nil)

;;;; Vertico integration

(defun embellish--enable-vertico ()
  "Enable `embellish-vertico-mode'."
  (require 'embellish-vertico)
  (embellish-vertico-mode 1))

(defun embellish--setup-vertico ()
  "Set up Vertico integration, now or when Vertico loads."
  (when embellish-vertico
    (if (featurep 'vertico)
        (embellish--enable-vertico)
      (with-eval-after-load 'vertico
        (when embellish-mode
          (embellish--enable-vertico))))))

(defun embellish--teardown-vertico ()
  "Tear down Vertico integration."
  (when (fboundp 'embellish-vertico-mode)
    (embellish-vertico-mode -1)))

;;;; embellish-mode

;;;###autoload
(define-minor-mode embellish-mode
  "Global minor mode for a clean, minimal Emacs appearance.
Applies frame chrome, spacing, scrolling, and visual line settings.
Each group can be disabled via `embellish-chrome', `embellish-spacing',
`embellish-scrolling', and `embellish-visual-line-mode'."
  :global t
  :group 'embellish
  (if embellish-mode
      (progn
        (embellish--apply-chrome)
        (embellish--apply-spacing)
        (embellish--apply-scrolling)
        (embellish--apply-visual-line)
        (embellish--setup-subwindow)
        (embellish--setup-font)
        (embellish--setup-vertico))
    (embellish--revert-chrome)
    (embellish--revert-spacing)
    (embellish--revert-scrolling)
    (embellish--revert-visual-line)
    (embellish--teardown-subwindow)
    (embellish--teardown-font)
    (embellish--teardown-vertico)))



(provide 'embellish)

;;; embellish.el ends here
