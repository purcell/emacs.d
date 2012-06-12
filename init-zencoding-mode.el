(require 'zencoding-mode)
; @see https://github.com/rooney/zencoding for tutorial
; C-j or C-return to expand the line
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'nxml-mode-hook 'zencoding-mode)
(provide 'init-zencoding-mode)
