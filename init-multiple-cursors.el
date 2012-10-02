(global-set-key (kbd "C-M->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; From active region to multiple cursors:
;(global-set-key (kbd "C-M-c C-S-c") 'mc/edit-lines)
;(global-set-key (kbd "C-M-c C-e") 'mc/edit-ends-of-lines)
;(global-set-key (kbd "C-M-c C-a") 'mc/edit-beginnings-of-lines)

(provide 'init-multiple-cursors)
