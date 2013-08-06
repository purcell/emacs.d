(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5)
  )

;; gnus+davmail bug, so I have to use pop3 for davmail
;; http://permalink.gmane.org/gmane.emacs.gnus.general/83301
;; but delete all the mails on server is scary
(setq pop3-leave-mail-on-server t)

(add-hook 'gnus-group-mode-hook
          ;; list all the subscribed groups even they contain zero un-read messages
          (lambda () (local-set-key "o" 'my-gnus-group-list-subscribed-groups ))
          )

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials "~/.authinfo.gpg"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "homepc")

(provide 'init-gnus)
