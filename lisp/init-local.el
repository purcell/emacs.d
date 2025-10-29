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

;; auto-save
(add-to-list 'load-path "~/github/auto-save/") ; add auto-save to your load-path
(require 'auto-save)
(auto-save-enable)

(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving

;;; custom predicates if you don't want auto save.
;;; disable auto save mode when current filetype is an gpg file.
(setq auto-save-disable-predicates
      '((lambda ()
          (string-suffix-p
           "gpg"
           (file-name-extension (buffer-name)) t))))

;; denote
(use-package zoxide
  :ensure t)

(defun dired-jump-with-zoxide (&optional other-window)
  (interactive "P")
  (zoxide-open-with nil (lambda (file) (dired-jump other-window file)) t))

;; Load OS-specific configurations
(cond
 ((eq system-type 'windows-nt)
  (load (expand-file-name "lisp/init-local-windows.el" user-emacs-directory)))
 ((eq system-type 'darwin)
  (load (expand-file-name "lisp/init-local-macos.el" user-emacs-directory)))
 ((eq system-type 'gnu/linux)
  (load (expand-file-name "lisp/init-local-linux.el" user-emacs-directory))))

;; Load org config
(load (expand-file-name "lisp/init-local-org.el" user-emacs-directory))

;; Load keybinding
(load (expand-file-name "lisp/init-local-keybinding.el" user-emacs-directory))

(provide 'init-local)
