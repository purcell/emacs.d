;;; init-scala.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Taken from https://scalameta.org/metals/docs/editors/emacs/
;;; Code:

;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Enable scala-mode for highlighting, indentation and motion commands
(require-package 'scala-mode)
(use-package scala-mode
  :interpreter ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Add metals backend for lsp-mode
(require-package 'lsp-metals)
(use-package lsp-metals)

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(require-package 'posframe)
(use-package posframe)

;; Use the Debug Adapter Protocol for running tests and debugging
(require-package 'dap-mode)
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(require 'company)
(use-package company
  :hook (scala-mode . company-mode)
  :config
  (setq lsp-completion-provider :capf))

(provide 'init-scala)
;;; init-scala.el ends here
