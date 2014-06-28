(require-package 'haskell-mode)

(when (> emacs-major-version 23)
  (require-package 'flycheck-hdevtools)
  (require-package 'flycheck-haskell))
(after-load 'flycheck
  (require 'flycheck-hdevtools))

(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook interactive-haskell-mode-hook))
  (add-hook hook 'turn-on-haskell-doc-mode))
(add-hook 'haskell-mode-hook 'inferior-haskell-mode)

(after-load 'haskell-interactive-mode
  (diminish 'interactive-haskell-mode " IntHS"))

(add-auto-mode 'haskell-mode "\\.ghci\\'")

(require-package 'hi2)
(add-hook 'haskell-mode-hook 'turn-on-hi2)

(add-hook 'haskell-mode-hook (lambda () (subword-mode +1)))
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

(setq-default haskell-stylish-on-save t)

(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  (define-key haskell-mode-map (kbd "C-o") 'open-line))

(when (eval-when-compile (>= emacs-major-version 24))
  (require-package 'ghci-completion)
  (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))

(eval-after-load 'page-break-lines
  '(push 'haskell-mode page-break-lines-modes))

;; Make compilation-mode understand "at blah.hs:11:34-50" lines output by GHC
(after-load 'compile
  (let ((alias 'ghc-at-regexp))
    (add-to-list
     'compilation-error-regexp-alist-alist
     (list alias " at \\(.*\\.\\(?:l?[gh]hs\\|hi\\)\\):\\([0-9]+\\):\\([0-9]+\\)-[0-9]+$" 1 2 3 0 1))
    (add-to-list
     'compilation-error-regexp-alist alias)))

(provide 'init-haskell)
