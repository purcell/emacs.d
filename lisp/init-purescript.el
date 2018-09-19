(when (maybe-require-package 'purescript-mode)
  (when (maybe-require-package 'psc-ide)
    (add-hook 'purescript-mode-hook
              (lambda ()
                (psc-ide-mode)
                (company-mode)
                (flycheck-mode)
                (turn-on-purescript-indentation))))
  (when (maybe-require-package 'add-node-modules-path)
    (after-load 'purescript-mode
      (add-hook 'purescript-mode-hook 'add-node-modules-path))))

(provide 'init-purescript)
