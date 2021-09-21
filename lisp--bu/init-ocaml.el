;;; init-ocaml.el --- Support the OCaml language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'eglot
  (put 'tuareg-mode 'eglot-language-id "ocaml")
  (add-to-list 'eglot-server-programs '((tuareg-mode) . ("ocamllsp")) t))

(when (maybe-require-package 'tuareg)
  (when (maybe-require-package 'merlin)
    (add-hook 'tuareg-mode-hook 'merlin-mode)

    (when (maybe-require-package 'merlin-company)
      (with-eval-after-load 'merlin
        (with-eval-after-load 'company
          (push 'merlin-company-backend company-backends))))

    (when (maybe-require-package 'merlin-eldoc)
      (with-eval-after-load 'merlin
        (autoload 'merlin-eldoc--gather-info "merlin-eldoc")
        (add-hook 'merlin-mode-hook
                  (lambda ()
                    (setq-local eldoc-documentation-function
                                #'merlin-eldoc--gather-info))))))

  (with-eval-after-load 'tuareg
    (defvar-local tuareg-previous-tuareg-buffer nil
      "Buffer from which we jumped to the REPL.")

    (defun sanityinc/tuareg-repl-switch ()
      (interactive)
      (let ((last-tuareg-buf (when (derived-mode-p 'tuareg-mode)
                               (current-buffer))))
        (tuareg-run-ocaml)
        (pop-to-buffer tuareg-interactive-buffer-name)
        (when last-tuareg-buf
          (setq-local tuareg-previous-tuareg-buffer last-tuareg-buf))))

    (defun sanityinc/tuareg-repl-switch-back ()
      (interactive)
      (when tuareg-previous-tuareg-buffer
        (pop-to-buffer tuareg-previous-tuareg-buffer)))

    (define-key tuareg-mode-map (kbd "C-c C-z") 'sanityinc/tuareg-repl-switch)
    (define-key tuareg-interactive-mode-map (kbd "C-c C-z") 'sanityinc/tuareg-repl-switch-back)))

(when (maybe-require-package 'dune)
  (maybe-require-package 'dune-format))

;; Add my own lightweight ocp-indent reformatter, instead of the clunky upstream package
(when (maybe-require-package 'reformatter)
  (defcustom ocp-indent-args nil
    "Arguments for \"ocp-indent\" invocation.")

  (reformatter-define ocp-indent
    :program "ocp-indent"
    :args ocp-indent-args
    :lighter " OCP"))


(provide 'init-ocaml)
;;; init-ocaml.el ends here
