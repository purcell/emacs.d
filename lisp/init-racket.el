(require-package 'geiser)
(require 'geiser)


(require-package 'quack)
(require 'quack)

(require-package 'ac-geiser)
(require 'ac-geiser)
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-mode))

(add-hook 'geiser-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'geiser-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'geiser-repl-mode-hook 'paredit-mode)
(add-hook 'geiser-mode-hook 'paredit-mode)


(add-hook 'geiser-repl-mode-hook 'evil-paredit-mode)
(add-hook 'geiser-mode-hook 'evil-paredit-mode)

(require 'rainbow-delimiters)
(add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'geiser-mode-hook 'rainbow-delimiters-mode)

(provide 'init-racket)
