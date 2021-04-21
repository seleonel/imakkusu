;; Commentary: literal example of how to not config your emacs
;;  Uses straight.el + use-package by default to deal with all
;; packages listed in organizator.el

(defmacro plist-change (plist key new-item)
  "Changes the property of a plist's value given a key.
Adds in a new key with that value otherwise"
  `(setq ,plist (plist-put ,plist ,key ,new-item)))

;; stops package from loading itself+some other packages
(setq package-enable-at-startup nil)
;; default org file to get loaded
(setq org-config-file (concat user-emacs-directory "organizator.org"))

;; straight's bootstrap and bootstrap process in case it hasn't been eval'd 
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

;; emacs "native" packages
;; directory editor - standard file browser that I use daily
(use-package dired
  :straight nil
  :custom
  (dired-listing-switches "-alhg")
  (image-dired-thumb-margin 5)
  (dired-dwim-target t)
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package dired-x
  :straight nil
  :after dired
  :custom
  (dired-guess-shell-alist-user `((,(rx "." (or "mp4"
						"mp3"
						"mkv"
						"webm"
						"flac"
						"gif")) "mpv ? &")
				  (,(rx "." (or "docx" "doc" "xlsx")) "libreoffice ? &")
				  (,(rx ".html") "chromium ? &")
				  (,(rx ".exe") "wine ? &"))))

;; keeps track of the recent files, also built-in
(use-package recentf
  :init
  (recentf-mode)
  (run-at-time nil (* 30 60) 'recentf-save-list)
  :custom
  (recentf-max-saved-items 50))

;; built-in spelling checker
(use-package flyspell
  :straight nil
  :custom
  (ispell-program-name "aspell")
  (ispell-list-command "--list")
  (flyspell-issue-message-flag nil)
  :bind
  ("<f8>" . ispell-word)
  :hook
  (text-mode . flyspell-mode))

;; keep with the latest org version
;; also force emacs to load straight's org
;; and not the native one
(use-package org
  :config
  (org-babel-do-load-languages 
   'org-babel-load-languages 
   '((plantuml . t)
     (python . t)
     (dot . t)
     (lisp . t)
     (shell . t)))
  :config
  ;; image produced by the latex preview output is now bigger
  (plist-change org-format-latex-options :scale 1.4)
  :custom
  ;; listings package by default when exporting to tex
  (org-latex-listings t)
  ;; hiding some markdown stuff like the bold, italic, ttt delimiters
  (org-hide-emphasis-markers t)
  ;; keep the src indentation untouched
  (org-src-preserve-indentation t)
  ;; all headings must stay folded
  (org-startup-folded t)
  (org-display-inline-images t)
  (org-redisplay-inline-images t)
  (org-startup-with-latex-preview t) ; terrible for loading times, maybe I should comment it out
  (org-startup-with-inline-images t)
  (org-image-actual-width 600)
  (org-src-window-setup 'split-window-below)
  (org-refile-targets '((nil :maxlevel . 4)
			(nil :tag . "candidate")))
  :hook
  (org-mode . org-toggle-pretty-entities)
  (org-mode . (lambda ()
		(when org-inline-image-overlays
		  (org-redisplay-inline-images)))))

(use-package ox-latex
  :after org
  :defer t
  :straight nil
  :config
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color")))

;; load the rest of the configs
(org-babel-load-file org-config-file)
