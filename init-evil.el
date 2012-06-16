(evil-mode 1)
; @see https://github.com/timcharper/evil-surround
(require 'surround)
(global-surround-mode 1)
(evil-set-initial-state 'org-mode 'emacs)
(provide 'init-evil)
