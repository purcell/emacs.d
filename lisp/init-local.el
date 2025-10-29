;; evil-want-keybinding must be declared before Evil and Evil Collection
(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :demand t  ; Force immediate loading
  :init
  ;; These must be set before evil loads
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)

  ;; Ensure evil-core is loaded
  (defvar evil-mode-buffers nil
    "List of buffers with Evil mode enabled.")

  :config
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Block cursor in normal mode
  (setq evil-normal-state-cursor '(box))
  ;; Blinking underline in insert mode
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

