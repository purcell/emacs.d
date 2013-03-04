(require-package 'haml-mode)
(require-package 'flymake-haml)
(add-hook 'haml-mode-hook 'flymake-haml-load)

(provide 'init-haml)
