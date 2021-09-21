;; TODO: migrate config to minor mode
;; TODO: auto downlaod
;; Code:


(require 'evil)
(evil-mode 1)
(evil-collection-init)

(load-file "/home/ff/.config/emacs/emacs-stuff/fff-mode.el")
(require 'fff-mode)
(fff-mode 1)

(remove-hook 'sanityinc/lispy-modes-hook 'enable-paredit-mode)

(desktop-save-mode -1)
;; (symbol-overlay-mode -1)

(provide 'init-local)
