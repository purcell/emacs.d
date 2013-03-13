(require-package 'haskell-mode)

(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook))
  (add-hook hook 'turn-on-haskell-doc-mode))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
     (define-key haskell-mode-map (kbd "RET") 'newline)))

(require-package 'ghci-completion)
(add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)

(require-package 'flymake-hlint)
(add-hook 'haskell-mode-hook #'flymake-hlint-load)

(provide 'init-haskell)
