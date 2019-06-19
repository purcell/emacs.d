;;; init-local.el --- Local Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(set-default-font "monaco-16")

(require-package 'dracula-theme)
(load-theme 'dracula t)

(require-package 'web-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(require-package 'graphql-mode)
(require 'graphql-mode)

(provide 'init-local)
;;; init-local.el ends here
