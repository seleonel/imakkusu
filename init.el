;; Commentary: literal example of how to not config your emacs
;;  Uses straight.el + use-package by default to deal with all
;; packages listed in organizator.el

;; stops package from loading itself+some other packages
(setq package-enable-at-startup nil)
;; default org file to get loaded
(setq org-config-file (concat user-emacs-directory "organizator.org"))
;; gnus directory
(setq gnus-user-config (concat user-emacs-directory "gnus/gnus"))

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
;; for all those lsp-servers
(straight-use-package 'use-package-ensure-system-package)
;; keep with the latest org version
;; also force emacs to load straight's org
;; and not the native one
(straight-use-package 'org)
;; mail and rss
(straight-use-package 'gnus)
;; org configs load
(org-babel-load-file org-config-file)
