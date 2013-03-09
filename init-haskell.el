(require-package 'haskell-mode)
(setq haskell-font-lock-symbols t)

(add-hook 'haskell-mode-hook
          (lambda ()
            (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
            (turn-on-haskell-doc-mode)
            (turn-on-haskell-indent)))

(require-package 'flymake-hlint)
(add-hook 'haskell-mode-hook #'flymake-hlint-init)

(provide 'init-haskell)
