;;; init-slime.el --- Slime support for Common Lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'slime)

(when (maybe-require-package 'slime-company)
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space)
  (with-eval-after-load 'slime-company
    (add-to-list 'company-backends 'company-slime)))


;;; Lisp buffers

(with-eval-after-load 'slime
  (setq slime-protocol-version 'ignore)
  (setq slime-net-coding-system 'utf-8-unix)
  (let ((features '(slime-fancy slime-repl slime-fuzzy)))
    (when (require 'slime-company nil t)
      (push 'slime-company features))
    (slime-setup features)) )


;;; REPL

(defun sanityinc/slime-repl-setup ()
  "Mode setup function for slime REPL."
  (sanityinc/lisp-setup))

(with-eval-after-load 'slime-repl
  ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (with-eval-after-load 'paredit
    (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil))

  ;; Bind TAB to `indent-for-tab-command', as in regular Slime buffers.
  (define-key slime-repl-mode-map (kbd "TAB") 'indent-for-tab-command)

  (add-hook 'slime-repl-mode-hook 'sanityinc/slime-repl-setup))


(provide 'init-slime)
;;; init-slime.el ends here
