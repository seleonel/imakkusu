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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function 'browse-url-chromium)
 '(custom-safe-themes
   '("8885761700542f5d0ea63436874bf3f9e279211707d4b1ca9ed6f53522f21934" default))
 '(linum-format " %5i ")
 '(org-agenda-files '("~/.config/emacs/organizator.org"))
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.6 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-src-window-setup 'split-window-below)
 '(pyvenv-mode t)
 '(reb-re-syntax 'string)
 '(safe-local-variable-values '((org-download-image-dir . \.\./\.\./imgs/)))
 '(send-mail-function 'mailclient-send-it)
 '(shell-escape-mode "-shell-escape"))




