;;; init-nix.el --- Support for the Nix package manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'nix-mode)
(maybe-require-package 'nix-sandbox)
(maybe-require-package 'nix-buffer)

(when (maybe-require-package 'nixos-options)
  (when (maybe-require-package 'company-nixos-options)
    (after-load 'company

      ;; Patch pending https://github.com/travisbhartwell/nix-emacs/pull/46
      (after-load 'company-nixos-options
        (defun company-nixos--in-nix-context-p ()
          (or (derived-mode-p 'nix-mode 'nix-repl-mode)
              (let ((file-name (buffer-file-name (current-buffer))))
                (and file-name (equal "nix" (file-name-extension file-name)))))))

      (add-to-list 'company-backends 'company-nixos-options))))



(provide 'init-nix)
;;; init-nix.el ends here
