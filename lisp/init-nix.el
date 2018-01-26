(maybe-require-package 'nix-mode)
(maybe-require-package 'nix-sandbox)
(maybe-require-package 'nix-buffer)

(when (maybe-require-package 'nixos-options)
  (when (maybe-require-package 'company-nixos-options)
    (after-load 'company
      (add-to-list 'company-backends 'company-nixos-options))))



(provide 'init-nix)
