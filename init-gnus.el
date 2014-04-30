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

;; @see http://www.fnal.gov/docs/products/emacs/emacs/gnus_3.html#SEC60
;; QUOTED: If you are using an unthreaded display for some strange reason ...
;; Yes, when I search email in IMAP folder, emails are not threaded
(setq gnus-article-sort-functions
      '(
        (not gnus-article-sort-by-date)
        (not gnus-article-sort-by-number)
        ))

(defun message-select-forwarded-email-tags ()
  "select the <#mml-or-what-ever> tags in message-mode"
  (interactive)
  (let (start rlt)
    (when (search-forward "<#")
      (setq start (point))
      (push-mark (point) t t)
      (goto-char (point-max))
      (search-backward ">")
      (forward-char)
      (setq rlt t))
    rlt))

(defun message-copy-select-forwarded-email-tags ()
  "copy the <#mml-or-what-ever> tags in message-mode"
  (interactive)
  (save-excursion
    (cond
     ((message-select-forwarded-email-tags)
      (copy-region-as-kill (region-beginning) (region-end))
      (message "forwarded email tags copied!"))
     (t (message "NO forwarded email tags found!"))
     )
    ))

(provide 'init-gnus)
