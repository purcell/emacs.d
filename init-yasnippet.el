(require 'yasnippet)
(if (fboundp 'yas/initialize)
  (yas/initialize)
  (yas-global-mode 1)
  )
;; default TAB key is occupied by auto-complete
(global-set-key (kbd "C-c k") 'yas/expand)
;; default hotkey `C-c & C-s` is still valid
(global-set-key (kbd "C-c l") 'yas/insert-snippet)
;; give yas/dropdown-prompt in yas/prompt-functions a chance
(require 'dropdown-list)
;; use yas/completing-prompt when ONLY when `M-x yas/insert-snippet'
;; thanks to capitaomorte for providing the trick.
(defadvice yas/insert-snippet (around use-completing-prompt activate)
     "Use `yas/completing-prompt' for `yas/prompt-functions' but only here..."
       (let ((yas/prompt-functions '(yas/completing-prompt)))
             ad-do-it))
(provide 'init-yasnippet)
