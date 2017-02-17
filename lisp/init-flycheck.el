(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(require 'flycheck-tip)
(flycheck-tip-use-timer 'verbose)

(provide 'init-flycheck)
