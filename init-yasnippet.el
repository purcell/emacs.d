(require 'yasnippet)
(yas/initialize)
;; default TAB key is occupied by auto-complete
(global-set-key (kbd "C-c ; u") 'yas/expand)
;; default hotkey `C-c & C-s` is still valid
(global-set-key (kbd "C-c ; s") 'yas/insert-snippet)
;; give yas/dropdown-prompt in yas/prompt-functions a chance
(require 'dropdown-list)
(provide 'init-yasnippet)
