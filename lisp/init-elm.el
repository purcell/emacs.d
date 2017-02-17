(when (maybe-require-package 'elm-mode)
  (after-load 'elm-mode
    (diminish 'elm-indent-mode)
    (when (executable-find "elm-oracle")
      (add-hook 'elm-mode-hook 'elm-oracle-setup-completion)))
  (when (maybe-require-package 'flycheck-elm)
    (after-load 'elm-mode
      (flycheck-elm-setup))))

(provide 'init-elm)
