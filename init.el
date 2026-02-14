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

(add-to-list 'load-path (expand-file-name "~/.wisdom.d/org-clock-multi"))
(require 'org-clock-multi)

(add-to-list 'load-path (expand-file-name "~/.wisdom.d/org-todo-dashboard"))
(require 'org-todo-dashboard)


(use-package wisdom
  :straight (wisdom :type git :host github :repo "kwrooijen/wisdom")
  :custom
  (wisdom-org-directory "~/.wisdom.d/org")
  (wisdom-output-directory "~/.emacs.d/wisdom")
  :config
  (wisdom-boot))

