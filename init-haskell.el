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

(require-package 'flymake-haskell-multi)
(add-hook 'haskell-mode-hook #'flymake-haskell-multi-load)

;; Make compilation-mode understand "at blah.hs:11:34-50" lines output by GHC
(eval-after-load 'compile
  '(progn
     (let ((alias 'ghc-at-regexp))
       (add-to-list
        'compilation-error-regexp-alist-alist
        (list alias " at \\(.*l?hs\\):\\([0-9]+\\):\\([0-9]+\\)-[0-9]+$" 1 2 3 0 1))
       (add-to-list
        'compilation-error-regexp-alist alias))))

(provide 'init-haskell)
