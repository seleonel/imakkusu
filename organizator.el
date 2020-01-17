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

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator (quote slant))
  (spaceline-spacemacs-theme))

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

(use-package rainbow-mode
    :ensure t
    :init (rainbow-mode 1))

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

(global-set-key (kbd "S-s") 'ido-switch-buffer)

(setq org-src-window-setup 'current-window)

(defun matarPalavra ()
  (interactive)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c DEL") 'matarPalavra)

(line-number-mode 1)
(column-number-mode 1)

(global-set-key (kbd "C-c y") 'avy-copy-line)
