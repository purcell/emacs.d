;;; init-sly.el --- Sly support for Common Lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'sly)
(require-package 'sly-asdf)
(require-package 'sly-macrostep)
(require-package 'sly-repl-ansi-color)


;;; Lisp buffers

(with-eval-after-load 'sly
  (setq sly-protocol-version 'ignore)
  (setq sly-net-coding-system 'utf-8-unix)
  (let ((features '(sly-fancy)))
    ;; (when (require 'sly-company nil t)
    ;;   (push 'sly-company features))
    (sly-setup features)))


;;; REPL

(with-eval-after-load 'sly-repl
  ;; Stop SLY's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (with-eval-after-load 'paredit
    (define-key sly-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil))

  ;; Bind TAB to `indent-for-tab-command', as in regular Sly buffers.
  (define-key sly-repl-mode-map (kbd "TAB") 'indent-for-tab-command)

  (add-hook 'sly-repl-mode-hook 'sanityinc/lisp-setup))


(provide 'init-sly)
;;; init-sly.el ends here
