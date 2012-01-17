(autoload 'slime-fuzzy-init "slime-fuzzy" "" nil)
(eval-after-load 'slime-fuzzy
  '(require 'slime-repl))

(eval-after-load 'slime
  '(progn
     (add-to-list 'load-path (concat (directory-of-library "slime") "/contrib"))
     (setq slime-protocol-version 'ignore)
     (setq slime-net-coding-system 'utf-8-unix)
     (add-hook 'slime-repl-mode-hook 'smp-lisp-setup)
     (slime-setup '(slime-repl slime-fuzzy))
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

     (add-hook 'sldb-mode-hook #'(lambda () (setq autopair-dont-activate t)))

     ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
     (defun override-slime-repl-bindings-with-paredit ()
       (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil))
     (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

     (add-hook 'slime-mode-hook 'set-up-slime-hippie-expand)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-hippie-expand)

     (add-hook 'slime-repl-mode-hook (lambda () (setq show-trailing-whitespace nil)))

     (add-hook 'slime-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

     (eval-after-load 'auto-complete
       '(add-to-list 'ac-modes 'slime-repl-mode))))



(provide 'init-slime)
