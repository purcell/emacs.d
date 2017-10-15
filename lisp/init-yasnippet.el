(require-package 'yasnippet)
(after-load 'yasnippet
  (yas/reload-all)
  (setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt yas/no-prompt)))
(yas-global-mode t)
(provide 'init-yasnippet)