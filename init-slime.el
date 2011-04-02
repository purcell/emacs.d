(global-set-key [f4] 'slime-selector)

(autoload 'slime-fuzzy-init "slime-fuzzy" "" nil)
(eval-after-load "slime-fuzzy"
  '(require 'slime-repl))

(eval-after-load "slime"
  '(progn
     (add-to-list 'load-path (concat (directory-of-library "slime") "/contrib"))
     (setq slime-protocol-version 'ignore)
     (add-hook 'slime-mode-hook 'pretty-lambdas)
     (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
     (slime-setup '(slime-repl slime-fuzzy))
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

     (add-hook 'sldb-mode-hook #'(lambda () (setq autopair-dont-activate t)))

     ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
     (defun override-slime-repl-bindings-with-paredit ()
       (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil))
     (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

     (require 'hippie-expand-slime)
     (add-hook 'slime-mode-hook 'set-up-slime-hippie-expand)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-hippie-expand)

     (require 'ac-slime)
     (add-hook 'slime-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

     (eval-after-load "auto-complete"
       '(add-to-list 'ac-modes 'slime-repl-mode))))



(provide 'init-slime)
