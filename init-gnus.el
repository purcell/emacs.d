(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5)
  )
 (add-hook 'gnus-group-mode-hook
           ;; list all the subscribed groups even they contain zero un-read messages
           (lambda () (local-set-key "o" 'my-gnus-group-list-subscribed-groups ))
           )
(provide 'init-gnus)
