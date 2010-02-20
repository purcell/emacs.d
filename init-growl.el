(require 'todochiku) ;; growl notifications when compilation finishes
(add-hook 'compilation-mode-hook (lambda () (local-set-key [f6] 'recompile)))


(provide 'init-growl)