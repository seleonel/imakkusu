(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(require 'dbus)
;; *Use package*

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; Ajuda na instalação e compilação de pacotes do emacs. (Faz mais sentido estar aqui no início).


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
   '(visual-regexp-steroids visual-regexp define-word dired-x org-roam-protocol undo-tree counsel org-ref org-roam org-superstar company-math company-quickhelp company-c-headers company-auctex latex-preview-pane ac-js2 plantuml-mode pyenv-mode magit mu4e-alert use-package-ensure-system-package mu4e mu skewer-mode simple-httpd js2-mode emacs-neotree neotree column-enforce-mode gnuplot sublime-themes gnuplot-mode emms auto-dictionary doom-modeline with-editor git-commit transpose-frame rainbow-delimiters projectile all-the-icons dashboard sudo-edit noctilux-theme cyberpunk-theme web-mode yasnippet-snippets yasnippet rainbow-delimiters-mode expand-region multiple-cursors ivy swiper browse-kill-ring dmenu jedi flycheck auto-complete company-anaconda pyvenv pyenv spaceline virtualenvwrapper company-jedi company switch-window smart-tabs-mode rainbow-mode avy smex ample-theme which-key use-package dash latex-extra))
 '(pyvenv-mode t)
 '(send-mail-function 'mailclient-send-it)
 '(shell-escape-mode "-shell-escape"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(comint-highlight-prompt ((t (:inherit minibuffer-prompt :foreground "HotPink1"))))
 '(company-scrollbar-bg ((t (:background "maroon3"))))
 '(company-scrollbar-fg ((t (:background "orchid"))))
 '(company-template-field ((t (:background "maroon3" :foreground "black"))))
 '(company-tooltip ((t (:background "maroon3" :foreground "black"))))
 '(company-tooltip-common ((t (:foreground "white"))))
 '(company-tooltip-selection ((t (:background "LightPink1"))))
 '(custom-link ((t (:inherit link :foreground "maroon1"))))
 '(doom-modeline-bar ((t (:background "gray20"))))
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



