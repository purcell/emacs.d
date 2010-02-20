(autoload 'iedit-mode "iedit" "Edit current search matches")
(global-set-key (kbd "C-;") 'iedit-mode)
(eval-after-load "iedit"
  '(define-key iedit-mode-map (kbd "C-g") 'iedit-mode))

(provide 'init-iedit)