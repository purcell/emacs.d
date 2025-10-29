;;  -*- lexical-binding: t; -*-

(use-package evil
  :ensure t
  :demand t
  :init
  ;; These must be set before evil loads
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)

  :config
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Cursor styles
  (setq evil-normal-state-cursor '(box))
  (setq evil-insert-state-cursor '((hbar . 3)))

  ;; Blinking only in insert mode
  (add-hook 'evil-insert-state-entry-hook (lambda () (blink-cursor-mode 1)))
  (add-hook 'evil-insert-state-exit-hook (lambda () (blink-cursor-mode 0)))

  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint))

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

;; Load OS-specific configurations
(cond
 ((eq system-type 'windows-nt)
  (load (expand-file-name "lisp/init-local-windows.el" user-emacs-directory)))
 ((eq system-type 'darwin)
  (load (expand-file-name "lisp/init-local-macos.el" user-emacs-directory)))
 ((eq system-type 'gnu/linux)
  (load (expand-file-name "lisp/init-local-linux.el" user-emacs-directory))))

;; Load keybinding
(load (expand-file-name "lisp/init-local-keybinding.el" user-emacs-directory))

(provide 'init-local)
