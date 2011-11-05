(add-auto-mode 'haml-mode "\.haml$")
(add-auto-mode 'sass-mode "\.sass$" "\.scss$")

(add-hook 'haml-mode-hook 'flymake-haml-load)
(add-hook 'sass-mode-hook 'flymake-sass-load)


(provide 'init-haml)
