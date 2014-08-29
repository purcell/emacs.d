(when (eval-when-compile (>= emacs-major-version 24))
  (require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode))

;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.8)

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

(setq flycheck-disabled-checkers
      '(emacs-lisp-checkdoc haskell-hdevtools ledger))

(provide 'init-flycheck)
