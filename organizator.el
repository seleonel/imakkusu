(defun my-buffeiro ()
  (let ((buffer (generate-new-buffer "intro")))
    (switch-to-buffer buffer)
    (center-line)
    (insert "BEM VINDO AO MARAVILHOSO IMAKKUSU\n")
    (insert-image (create-image "~/.emacs.d/img/kicchiri.png"))
    (insert "\n\n\n\n\n")
    buffer))
(setq initial-buffer-choice 'my-buffeiro)

(set-default-font "Hack 12")

(tool-bar-mode -1)

(menu-bar-mode -1)

(when window-system (global-hl-line-mode t))

(use-package ample-theme
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t)
               (enable-theme 'ample-flat))
  :defer t
  :ensure t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(defvar default-shell "/bin/zsh")
(defadvice ansi-term (before force-zsh)
  (interactive (list default-shell)))
(ad-activate 'ansi-term)

(global-set-key (kbd "s-t") 'ansi-term)

(defalias 'yes-or-no-p 'y-or-n-p)
