(when (maybe-require-package 'purescript-mode)
  (when (maybe-require-package 'psc-ide)
    (add-hook 'purescript-mode-hook
              (lambda ()
                (psc-ide-mode)
                (company-mode)
                (flycheck-mode)
                (turn-on-purescript-indentation)))))

(provide 'init-purescript)
