(require 'slime-autoloads)
(global-set-key [f4] 'slime-selector)


(eval-after-load "slime"
  '(progn
     (add-to-list 'load-path (concat (directory-of-library "slime") "/contrib"))
     (setq slime-protocol-version 'ignore)
     (add-hook 'slime-mode-hook 'pretty-lambdas)
     (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
     (slime-setup '(slime-repl slime-fuzzy))
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

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

     (add-hook 'slime-repl-mode-hook (lambda () (auto-complete-mode t)))
     (add-hook 'slime-connected-hook
               (lambda ()
                 (define-key slime-repl-mode-map (kbd "TAB") 'indent-or-expand-with-ac)))))



(provide 'init-slime)
