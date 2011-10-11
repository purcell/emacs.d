(add-auto-mode 'php-mode "\\.php[345]?\\'\\|\\.phtml\\." "\\.(inc|tpl)$" "\\.module$")
(add-hook 'php-mode-hook
          (lambda ()
            (require 'flymake-php)
            (flymake-mode t)))
(autoload 'smarty-mode "smarty-mode" "Smarty Mode" t)
(add-auto-mode 'smarty-mode "\\.tpl$")

(provide 'init-php)
