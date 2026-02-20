;;; embellish-winter-night-theme.el --- Dark theme built on embellish-theme -*- lexical-binding: t; -*-

;; Author: Kevin van Rooijen
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (embellish-theme "0.1.0"))
;; Keywords: faces, themes

;;; Commentary:

;; A dark theme with cool blue tones, built on the embellish-theme engine.
;; Load with: (load-theme 'embellish-winter-night t)

;;; Code:

(require 'embellish-theme)

(embellish-theme-generate 'embellish-winter-night
  '(:strong     "#FFFFFF"
    :foreground "#ECEFF4"
    :subtle     "#434C5E"
    :faded      "#616E88"
    :salient    "#81A1C1"
    :popout     "#D08770"
    :error      "#BF616A"
    :warning    "#FFC16D"
    :success    "#A3BE8C"
    :info       "#88C0D0"
    :background "#222436"
    :darken     "#1e2030"
    :highlight  "#2f334d"))

;;; embellish-winter-night-theme.el ends here
