
(require 'gnus-desktop-notify)
(gnus-demon-init)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(gnus-desktop-notify-mode)
(gnus-demon-add-scanmail)
(gnus-demon-add-handler 'gnus-demon-scan-news 10 t)
;; (gnus-demon-close-connections nil 60)
(defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
  "Timeout for Gnus."
  (with-timeout
      (120 (message "Gnus timed out."))
    ad-do-it))
(setq gnus-asyncronous t)
