;;; init-j.el --- Basic support for programming in J -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'j-mode)

(setq-default j-console-cmd "jconsole")
(add-hook 'inferior-j-mode-hook (lambda () (electric-pair-mode -1)))


(provide 'init-j)
;;; init-j.el ends here
