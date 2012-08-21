(eval-after-load "semantic"
  '(progn
     ;; any other config specific to sql
     (defun my-semantic-hook ()
       (semantic-add-system-include "/usr/include/wx-2.8" 'c++-mode)
       (semantic-add-system-include "/usr/include/wx-2.8/wx/gtk" 'c++-mode)
       )
     (add-hook 'semantic-init-hooks 'my-semantic-hook)
     ))

;Try completion with semantic-mode, it may slow the emacs,
;`M-x complete-symbol` (Hotkey: C-M-i) will trigger the completion
; Uncomment below code if you want semantic plus complete-symbol
;(semantic-mode)
;(add-to-list 'completion-at-point-functions 'semantic-completion-at-point-function)

(provide 'init-semantic)

