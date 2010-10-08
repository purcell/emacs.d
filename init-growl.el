(require 'todochiku) ;; growl notifications when compilation finishes
(add-hook 'compilation-mode-hook (lambda () (local-set-key [f6] 'recompile)))
(setq todochiku-icons-directory (expand-file-name "~/.emacs.d/site-lisp/todochiku-icons"))

(provide 'init-growl)
