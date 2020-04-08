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

(org-babel-load-file (expand-file-name "~/.emacs.d/organizator.org"))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-chromium))
 '(custom-safe-themes
   (quote
    ("8885761700542f5d0ea63436874bf3f9e279211707d4b1ca9ed6f53522f21934" default)))
 '(org-agenda-files (quote ("~/Estudo/diario.org")))
 '(package-selected-packages
   (quote
    (use-package-ensure-system-package mu4e mu skewer-mode simple-httpd js2-mode emacs-neotree neotree column-enforce-mode gnuplot sublime-themes gnuplot-mode emms auto-dictionary doom-modeline with-editor git-commit transpose-frame rainbow-delimiters projectile all-the-icons dashboard sudo-edit noctilux-theme cyberpunk-theme web-mode yasnippet-snippets yasnippet rainbow-delimiters-mode expand-region multiple-cursors ivy swiper browse-kill-ring dmenu jedi flycheck auto-complete company-anaconda pyvenv pyenv spaceline virtualenvwrapper company-jedi company switch-window smart-tabs-mode rainbow-mode avy smex ample-theme which-key use-package dash latex-extra)))
 '(pyvenv-mode t)
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(comint-highlight-prompt ((t (:inherit minibuffer-prompt :foreground "HotPink1"))))
 '(custom-link ((t (:inherit link :foreground "maroon1"))))
 '(doom-modeline-bar ((t (:background "gray20"))))
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "VioletRed1" :weight bold))))
 '(hl-line ((t (:inherit highlight :background "HotPink4"))))
 '(ido-first-match ((t (:foreground "pink" :weight bold))))
 '(ido-subdir ((t (:foreground "deep pink"))))
 '(link ((t (:foreground "CadetBlue3" :underline t))))
 '(minibuffer-prompt ((t (:foreground "magenta"))))
 '(swiper-line-face ((t (:background "DeepPink4" :foreground "PeachPuff1")))))



