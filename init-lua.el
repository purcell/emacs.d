(require-package 'lua-mode)
(require-package 'flymake-lua)


(add-hook 'lua-mode-hook 'flymake-lua-load)

(provide 'init-lua)
