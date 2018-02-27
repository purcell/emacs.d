;;自动清除行位空格  
(add-hook 'before-save-hook 'delete-trailing-whitespace)  
(add-hook 'before-save-hook 'whitespace-cleanup)  
;;自动清除行之间的空白行  
(add-hook 'before-save-hook 'delete-blank-lines)  
;;显示空格  
(global-set-key [f1] 'whitespace-newline-mode)

(provide 'init-my-edit)
