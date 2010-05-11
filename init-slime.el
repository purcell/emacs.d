(require 'slime-autoloads)
(global-set-key [f4] 'slime-selector)


(eval-after-load "slime"
  '(progn
     (add-to-list 'load-path (concat (directory-of-library "slime") "/contrib"))
     (add-hook 'slime-mode-hook 'pretty-lambdas)
     (add-hook 'slime-mode-hook 'enable-paredit-mode)
     (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
     (slime-setup '(slime-repl slime-fuzzy slime-highlight-edits))
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

     (require 'ac-slime)
     (add-hook 'slime-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

     (require 'hippie-expand-slime)
     (add-hook 'slime-mode-hook 'set-up-slime-hippie-expand)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-hippie-expand)))



(provide 'init-slime)
