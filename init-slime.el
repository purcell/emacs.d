(require 'slime-autoloads)
(global-set-key [f4] 'slime-selector)

(eval-after-load "slime"
  '(progn
     (add-to-list 'load-path (concat (directory-of-library "slime") "/contrib"))
     (add-hook 'slime-mode-hook 'pretty-lambdas)
     (add-hook 'slime-mode-hook (lambda () (enable-paredit slime-mode-map)))
     (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
     (slime-setup '(slime-fancy slime-highlight-edits))
     (require 'slime-fuzzy)
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))



(provide 'init-slime)
