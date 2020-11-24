;; Commentary: Meu init.el caótico para um caramba
;; Utilizo do straight.el para management de meus pacotes

;; facilita o port das configs do use-package 
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(straight-use-package 'org)
(straight-use-package 'gnus)
;; org configs load
(org-babel-load-file (expand-file-name (concat (getenv "XDG_CONFIG_HOME") "/emacs/organizator.org")))
