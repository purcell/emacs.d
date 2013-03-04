(require-package 'flymake-shell)
(add-hook 'sh-set-shell-hook 'flymake-shell-load)


(provide 'init-sh)
