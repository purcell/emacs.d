;;; Package --- macOS specific settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; macOS specific key bindings
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;; Install sis
(use-package sis
  :ensure t
  :config
  ;; Configure input sources based on platform
  (when *is-a-mac*
    (sis-ism-lazyman-config
     "com.apple.keylayout.ABC"
     "im.rime.inputmethod.Squirrel.Hans")

    ;; Set cursor colors for different input sources
    (setq sis-default-cursor-color "#b81e19") ; English input source
    (setq sis-other-cursor-color "#b81e19") ; Other input source (change as needed)

    ;; Enable global modes
    (sis-global-cursor-color-mode t)
    (sis-global-respect-mode t)
    (sis-global-context-mode t)

    ;; Auto-switch to other input source when entering Evil insert mode
    (add-hook 'evil-insert-state-entry-hook #'sis-set-other)
    (add-hook 'evil-insert-state-exit-hook #'sis-set-english)))

(provide 'init-local-macos)
;;; init-local-macos.el ends here
