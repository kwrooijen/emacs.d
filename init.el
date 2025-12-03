(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)

(use-package wisdom
  :straight (wisdom :type git :host github :repo "kwrooijen/wisdom")
  :custom
  (wisdom-org-directory "~/.wisdom.d/org")
  (wisdom-output-directory "~/.emacs.d/wisdom")
  :config
  (wisdom-boot))





































;; For local development
;; (load-file (expand-file-name "~/Programming/Emacs/wisdom/wisdom.el"))
;; (setq wisdom-org-directory "~/.dotfiles/emacs/org")
;; (setq wisdom-output-directory "~/.emacs.d/wisdom")
;; (wisdom-boot)


;; (wisdom-aggregate-directory "~/.dotfiles/emacs/README.org")

;; Org sometimes freezes. This fixed it (but what exactly I don't know)
;; (setq org-element--cache-self-verify 'backtrace)
;; (setq org-element--cache-self-verify-frequency 1.0)
;; M-x org-element-cache-reset
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9cd784dfeea58d9d852d52be9126c1fba2b890ed368245624dec1df165a4f6fd"
     "ac233a81793936d663ed6489ae2faa03d454af609db2e75fa50f5d1364d0f297"
     "55e0df6f9a900f7273630b8ec140a525c1da4906f76628924f6ed78a7ad6d15b"
     default))
 '(helm-minibuffer-history-key "M-p")
 '(ignored-local-variable-values
   '((cider-repl-display-help-banner)
     (cider-figwheel-main-default-options . "dev")
     (cider-default-cljs-repl . figwheel-main)
     (cider-preferred-build-tool . clojure-cli)))
 '(warning-suppress-log-types '((org-roam))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bold ((t (:foreground "#ffffff" :weight bold)))))
