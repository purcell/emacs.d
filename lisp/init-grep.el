(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when (executable-find "ag")
  (require-package 'ag)
  (require-package 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag-project))


(provide 'init-grep)
