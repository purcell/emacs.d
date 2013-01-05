;; multiple-cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
;(global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
;(global-set-key (kbd "C-c c c") 'mc/edit-lines)
;(global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
;(global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)

(provide 'init-multiple-cursors)
