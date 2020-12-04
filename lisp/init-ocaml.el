;;; init-ocaml.el --- Support the OCaml language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'tuareg)
  (when (maybe-require-package 'merlin)
    (autoload 'merlin-mode "merlin" "Merlin mode" t)
    (add-hook 'tuareg-mode-hook 'merlin-mode)

    (with-eval-after-load 'merlin
      (add-hook 'merlin-mode-hook
                (lambda ()
                  (if merlin-mode
                      (add-hook 'xref-backend-functions 'merlin-xref-backend nil t)
                    (remove-hook 'xref-backend-functions 'merlin-xref-backend t))))
      (with-eval-after-load 'company
        (push 'merlin-company-backend company-backends)))

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

(when (maybe-require-package 'reformatter)
  (defcustom ocp-indent-args nil
    "Arguments for \"ocp-indent\" invocation.")

  (reformatter-define ocp-indent
    :program "ocp-indent"
    :args ocp-indent-args
    :lighter " OCP"))


(provide 'init-ocaml)
;;; init-ocaml.el ends here
