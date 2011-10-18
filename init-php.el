(add-auto-mode 'php-mode "\\.php[345]?\\'\\|\\.phtml\\." "\\.(inc|tpl)$" "\\.module$")
(add-hook 'php-mode-hook 'flymake-php-load)

(autoload 'smarty-mode "smarty-mode" "Smarty Mode" t)
(add-auto-mode 'smarty-mode "\\.tpl$")

(provide 'init-php)
