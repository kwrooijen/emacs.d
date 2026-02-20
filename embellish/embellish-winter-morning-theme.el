;;; embellish-winter-morning-theme.el --- Light theme built on embellish-theme -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (embellish-theme "0.1.0"))
;; Keywords: faces, themes

;;; Commentary:

;; A light theme with warm neutral tones, built on the embellish-theme engine.
;; Load with: (load-theme 'embellish-winter-morning t)

;;; Code:

(require 'embellish-theme)

(embellish-theme-generate 'embellish-winter-morning
  '(:strong     "#000000"
    :foreground "#101010"
    :subtle     "#9099AB"
    :faded      "#7A8599"
    :salient    "#3B6EA8"
    :popout     "#C35D2A"
    :error      "#C33C54"
    :warning    "#AC8300"
    :success    "#6A9A3A"
    :info       "#2C9A9A"
    :background "#FAFAFA"
    :darken     "#F0F0F0"
    :highlight  "#E5E5E5"))

;;; embellish-winter-morning-theme.el ends here
