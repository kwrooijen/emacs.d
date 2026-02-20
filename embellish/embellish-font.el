;;; embellish-font.el --- Font management for Emacs -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; URL: https://github.com/kwrooijen/embellish

;;; Commentary:

;; Centralized font configuration: family, weight, size, and zoom
;; keybindings.  Call `embellish-font-apply' after loading to set the
;; default face.

;;; Code:

(defgroup embellish-font nil
  "Font configuration."
  :group 'embellish)

(defcustom embellish-font-family "Roboto Mono"
  "Default font family."
  :type 'string
  :group 'embellish-font)

(defcustom embellish-font-weight 'thin
  "Default font weight."
  :type 'symbol
  :group 'embellish-font)

(defcustom embellish-font-size 200
  "Default font size (in 1/10 pt)."
  :type 'integer
  :group 'embellish-font)

(defcustom embellish-font-step 20
  "Font size adjustment step."
  :type 'integer
  :group 'embellish-font)

(defun embellish-font-apply ()
  "Apply font family, weight, and size to the default face."
  (set-face-attribute 'default nil
                      :family embellish-font-family
                      :weight embellish-font-weight
                      :height embellish-font-size))

(defun embellish-font-size-set (size)
  "Set global font SIZE."
  (set-face-attribute 'default nil :height size))

(defun embellish-font-size-increase ()
  "Increase global font size by `embellish-font-step'."
  (interactive)
  (embellish-font-size-set
   (+ (face-attribute 'default :height) embellish-font-step)))

(defun embellish-font-size-decrease ()
  "Decrease global font size by `embellish-font-step'."
  (interactive)
  (embellish-font-size-set
   (- (face-attribute 'default :height) embellish-font-step)))

(defun embellish-font-size-reset ()
  "Reset global font size to `embellish-font-size'."
  (interactive)
  (embellish-font-size-set embellish-font-size))

(global-set-key (kbd "C-x C-=") #'embellish-font-size-increase)
(global-set-key (kbd "C-x C--") #'embellish-font-size-decrease)
(global-set-key (kbd "C-x C-0") #'embellish-font-size-reset)

(provide 'embellish-font)

;;; embellish-font.el ends here
