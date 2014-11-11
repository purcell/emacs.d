(require-package 'ess)
(require 'ess-site)
(require-package 'ess-smart-underscore)
(require 'ess-smart-underscore)

(setq ess-use-auto-complete t)
(setq ess-tab-complete-in-script t)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'ess-mode))
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'inferior-ess-mode))

(defun my-ess-setup ()
  (interactive)
  (add-to-list 'ac-sources 'ac-source-R)
  (add-to-list 'ac-sources 'ac-source-R-objects)
  (add-to-list 'ac-sources 'ac-source-R-args)
  (add-to-list 'ac-sources 'ac-source-filename))
;(add-hook 'ess-mode-hook 'my-ess-setup)
(add-hook 'inferior-ess-mode-hook 'my-ess-setup)
(add-hook 'R-mode-hook 'my-ess-setup)

;(add-hook 'ess-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'inferior-ess-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'R-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'R-mode-hook #'(lambda () (autopair-mode)))
(add-hook 'inferior-ess-mode-hook #'(lambda () (autopair-mode)))
(provide 'init-ess)
