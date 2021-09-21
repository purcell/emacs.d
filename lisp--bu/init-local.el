
;; TODO: migrate config to minor mode
;; TODO: auto downlaod
;; Code:

(load-file "~/.emacs.d/emacs-stuff/fff-mode.el")
(require 'fff-mode)
(fff-mode 1)

(remove-hook 'sanityinc/lispy-modes-hook 'enable-paredit-mode)

(desktop-save-mode -1)
(symbol-overlay-mode -1)

(provide 'init-local)
