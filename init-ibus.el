;;set ibus-el
(add-to-list 'load-path "~/.emacs.d/site-lisp/ibus-el-0.3.1")
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)
(provide 'init-ibus)
