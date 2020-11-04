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
 '(org-agenda-files nil)
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.6 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(package-selected-packages
   '(peep-dired flycheck-pycheckers visual-regexp-steroids visual-regexp define-word dired-x org-roam-protocol undo-tree counsel org-ref org-roam org-superstar company-math company-quickhelp company-c-headers company-auctex latex-preview-pane ac-js2 plantuml-mode pyenv-mode magit mu4e-alert use-package-ensure-system-package mu4e mu skewer-mode simple-httpd js2-mode emacs-neotree neotree column-enforce-mode gnuplot sublime-themes gnuplot-mode emms auto-dictionary doom-modeline with-editor git-commit transpose-frame rainbow-delimiters projectile all-the-icons dashboard sudo-edit noctilux-theme cyberpunk-theme web-mode yasnippet-snippets yasnippet rainbow-delimiters-mode expand-region multiple-cursors ivy swiper browse-kill-ring dmenu jedi flycheck auto-complete company-anaconda pyvenv pyenv spaceline virtualenvwrapper company-jedi company switch-window smart-tabs-mode rainbow-mode avy smex ample-theme which-key use-package dash latex-extra))
 '(pyvenv-mode t)
 '(reb-re-syntax 'string)
 '(send-mail-function 'mailclient-send-it)
 '(shell-escape-mode "-shell-escape"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(all-the-icons-dired-dir-face ((t (:foreground "HotPink1"))))
 '(comint-highlight-prompt ((t (:inherit minibuffer-prompt :foreground "HotPink1"))))
 '(company-scrollbar-bg ((t (:background "maroon3"))))
 '(company-scrollbar-fg ((t (:background "orchid"))))
 '(company-template-field ((t (:background "maroon3" :foreground "black"))))
 '(company-tooltip ((t (:background "maroon3" :foreground "black"))))
 '(company-tooltip-common ((t (:foreground "white"))))
 '(company-tooltip-selection ((t (:background "LightPink1"))))
 '(custom-link ((t (:inherit link :foreground "maroon1"))))
 '(doom-modeline-bar ((t (:background "gray20"))))
 '(error ((t (:foreground "IndianRed1" :weight bold))))
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "VioletRed1" :weight bold))))
 '(highlight ((t (:background "dark orchid"))))
 '(hl-line ((t (:inherit highlight :background "HotPink4"))))
 '(ido-first-match ((t (:foreground "pink" :weight bold))))
 '(ido-subdir ((t (:foreground "deep pink"))))
 '(line-number ((t (:inherit (shadow default) :foreground "HotPink3"))))
 '(link ((t (:foreground "CadetBlue3" :underline t))))
 '(minibuffer-prompt ((t (:foreground "magenta"))))
 '(mu4e-header-key-face ((t (:inherit message-header-name :foreground "DeepPink1" :weight bold))))
 '(swiper-line-face ((t (:background "DeepPink4" :foreground "PeachPuff1")))))



