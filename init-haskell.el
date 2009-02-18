(load-library "haskell-site-file")

(load-library "cabal-mode")

(require 'hoogle)

(setq haskell-program-name (executable-find "ghci"))
(setq haskell-font-lock-symbols t)

(add-hook 'haskell-mode-hook
          (lambda ()
            (define-key haskell-mode-map [?\C-c h] 'hoogle-lookup)
            (turn-on-haskell-doc-mode)
            (turn-on-haskell-indent)))


(provide 'init-haskell)
