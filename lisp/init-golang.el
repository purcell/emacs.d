(when (maybe-require-package 'go-mode)
  (maybe-require-package 'company-go)
  (add-hook 'go-mode-hook (lambda ()
                            (setq-default tab-width 4))))


(provide 'init-golang)
