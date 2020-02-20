(use-package page-break-lines
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  :bind
  ("S-p" . projectile-command-mode))

;; ANTIGO BUFFER
;;(defun my-buffeiro ()
;;  (let ((buffer (generate-new-buffer "intro")))
;;    (switch-to-buffer buffer)
;;    (center-line)
;;    (insert "BEM VINDO AO MARAVILHOSO IMAKKUSU\n")
;;    (insert-image (create-image "~/.emacs.d/img/kicchiri.png"))
;;    (insert "\n\n\n\n\n")
;;    buffer))
;;(setq initial-buffer-choice 'my-buffeiro)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
   (setq dashboard-banner-logo-title "BEM VINDO AO MARAVILHOSO IMAKKUSU")
   (setq dashboard-startup-banner "~/.emacs.d/img/kicchiri.png")
   (setq dashboard-center-content t)
   (setq dashboard-show-shortcuts nil)
   (setq dashboard-items '((recents . 20)
			   (bookmarks . 5)
			   (projects . 10)))
   (setq dashboard-set-heading-icons t)
   (setq dashboard-set-file-icons t)
   (dashboard-modify-heading-icons '((recents . "ruby")
				     (bookmarks . "bookmark" )
				     (projects . "package" )))
   (setq dashboard-footer "emags :DDDDDDDD"))

(set-default-font "Hack 12")

(tool-bar-mode -1)

(menu-bar-mode -1)

(when window-system (global-hl-line-mode t))

(use-package noctilux-theme
   :ensure t
   :init
   (load-theme 'noctilux t))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator (quote slant))
  (spaceline-spacemacs-theme))

(defun mostrarLinhazitas ()
    (interactive)
    (display-line-numbers-mode))
(add-hook 'prog-mode-hook 'mostrarLinhazitas)

(scroll-bar-mode -1)

(use-package all-the-icons
  :ensure t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package avy
  :ensure t
  :bind
  ("M-." . avy-goto-char))

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind
  ("M-x" . smex))

(use-package rainbow-delimiters
    :ensure t
    :config
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package smart-tabs-mode
 :ensure t
 :init
 :config
 (smart-tabs-add-language-support latex latex-mode-hook
 ((latex-indent-line . 4)
 (latex-indent-region . 4)))
 (smart-tabs-insinuate 'c 'c++ 'java 'latex))

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  :bind
  ([remap other-window] .  switch-window))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup))

(use-package dmenu
  :ensure t
  :bind
  ("s-d" . 'dmenu))

(use-package browse-kill-ring
  :ensure t
  :bind
  ("M-y" . 'browse-kill-ring))

(use-package ivy
  :ensure t)
(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-c q" . 'mc/mark-next-like-this)
  ("C-c a" . 'mc/mark-all-like-this))

(use-package expand-region
  :ensure t
  :bind
  ("C-c e" . er/expand-region))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package sudo-edit
  :ensure t
  :bind
      ("C-c C-s" . sudo-edit))

(use-package transpose-frame
  :ensure t)

(defvar default-shell "/bin/zsh")
(defadvice ansi-term (before force-zsh)
  (interactive (list default-shell)))
(ad-activate 'ansi-term)

(global-set-key (kbd "s-t") 'ansi-term)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

(global-set-key (kbd "C-x b") 'ibuffer)

(global-set-key (kbd "s-s") 'ido-switch-buffer)

(setq org-src-window-setup 'current-window)

(defun matarPalavra ()
  (interactive)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c DEL") 'matarPalavra)

(line-number-mode 1)
(column-number-mode 1)

(global-set-key (kbd "C-c y") 'avy-copy-line)

(setq-default backup-directory-alist
 '(("." . "~/.cache/emacs/backups/")))
(add-to-list 'auto-save-file-name-transforms
 `(".*" "~/.cache/emacs/autosave/\\1" t) 'append)

(global-set-key (kbd "s-'") 'other-frame)
