(require 'slime-autoloads)
(global-set-key [f4] 'slime-selector)


(eval-after-load "slime"
  '(progn
     (add-to-list 'load-path (concat (directory-of-library "slime") "/contrib"))
     (add-hook 'slime-mode-hook 'pretty-lambdas)
     (add-hook 'slime-mode-hook (lambda () (enable-paredit slime-mode-map)))
     (add-hook 'slime-repl-mode-hook (lambda () (enable-paredit slime-repl-mode-map)))
     (slime-setup '(slime-fancy slime-highlight-edits))
     (require 'slime-fuzzy)
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

     (require 'init-slime-completion)
     (add-hook 'slime-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-mode-hook 'set-up-slime-hippie-expand)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-hippie-expand)))



(provide 'init-slime)
