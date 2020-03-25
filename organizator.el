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
   (setq dashboard-footer "emags :DDDDDDDD")
   (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(set-default-font "Hack 12")

(tool-bar-mode -1)

(menu-bar-mode -1)

(when window-system (global-hl-line-mode t))

(use-package sublime-themes
   :ensure t
   :init
   (load-theme 'brin t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config 
  (setq doom-modeline-bar-width 1)
  (setq doom-modeline-icon 1))

(defun mostrarLinhazitas ()
    (interactive)
    (display-line-numbers-mode))
(add-hook 'prog-mode-hook 'mostrarLinhazitas)

(scroll-bar-mode -1)

(use-package all-the-icons
  :ensure t)

(use-package column-enforce-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'column-enforce-mode)
  (add-hook 'text-mode-hook 'column-enforce-mode))

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
  (ac-config-default)
  (ac-complete-yasnippet))

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

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t))

(use-package gnuplot-mode
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

;; configs do uncle dave
(use-package emms
  :ensure t
  :config
    (require 'emms-setup)
    (require 'emms-player-mpd)
    (emms-all) ; don't change this to values you see on stackoverflow questions if you expect emms to work
    (setq emms-seek-seconds 1)
    (setq emms-player-list '(emms-player-mpd))
    (setq emms-info-functions '(emms-info-mpd))
    (setq emms-player-mpd-server-name "localhost")
    (setq emms-player-mpd-server-port "6600")
  :bind
    ("s-m p" . emms)
    ("s-m b" . emms-smart-browse)
    ("s-m r" . emms-player-mpd-update-all-reset-cache)
    ("<C-XF86AudioPrev>" . emms-previous)
    ("<C-XF86AudioNext>" . emms-next)
    ("<C-XF86AudioPlay>" . emms-pause))

(setq mpc-host "localhost:6000")

(defun mpd/update-database ()
  "Updates the MPD database synchronously."
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "Database atualizado"))
(global-set-key (kbd "s-m u") 'mpd/update-database)

(defun mpd/start-music-daemon ()
  "Start MPD, connects to it and syncs the metadata cache."
  (interactive)
  (shell-command "mpd")
  (mpd/update-database)
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "MPD atualizederson"))
(global-set-key (kbd "s-m c") 'mpd/start-music-daemon)

(setq-default backup-directory-alist
 '(("." . "~/.cache/emacs/backups/")))
(add-to-list 'auto-save-file-name-transforms
 `(".*" "~/.cache/emacs/autosave/\\1" t) 'append)

(global-set-key (kbd "s-'") 'other-frame)

(global-subword-mode 1)

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

(setenv
 "DICPATH"
 "/usr/share/hunspell")

(setq ispell-program-name "hunspell")

(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
    (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
      (add-hook hook (lambda () (flyspell-mode -1))))
(setq flyspell-issue-message-flag nil)

(use-package auto-dictionary
    :ensure t
    :config
    (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))

(use-package gnuplot
  :ensure t)
(use-package gnuplot-mode
  :ensure t)

(local-set-key "M-C-g" 'org-plot/gnuplot)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )
