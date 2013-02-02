(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5)
  )
 (add-hook 'gnus-group-mode-hook
           ;; list all the subscribed groups even they contain zero un-read messages
           (lambda () (local-set-key "o" 'my-gnus-group-list-subscribed-groups ))
           )

; Personal Information
(setq user-full-name "Chen Bin"
      user-mail-address "chenbin.sh@gmail.com"
      ;message-generate-headers-first t
      )

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials "~/.authinfo.gpg"
      ;smtpmail-auth-credentials '(("smtp.gmail.com" 587 "chenbin.sh@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "homepc")
(provide 'init-gnus)
