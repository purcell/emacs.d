;;; init-elm.el --- Support for the Elm language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'elm-mode)
  (after-load 'elm-mode
    (diminish 'elm-indent-mode)
    (after-load 'company
      (push 'company-elm company-backends))
    (when (executable-find "elm-format")
      (setq-default elm-format-on-save t)))
  (maybe-require-package 'elm-test-runner)
  (when (maybe-require-package 'flycheck-elm)
    (after-load 'elm-mode
      (flycheck-elm-setup))))

(provide 'init-elm)
;;; init-elm.el ends here
