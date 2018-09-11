(when (maybe-require-package 'elm-mode)
  (setq-default elm-format-on-save t)
  (after-load 'elm-mode
    (diminish 'elm-indent-mode)
    (add-hook 'elm-mode-hook
              (lambda () (sanityinc/local-push-company-backend 'company-elm)))
    (when (executable-find "elm-format")
      (setq-default elm-format-on-save t)))
  (maybe-require-package 'elm-test-runner)
  (when (maybe-require-package 'flycheck-elm)
    (after-load 'elm-mode
      (flycheck-elm-setup))))

(provide 'init-elm)
