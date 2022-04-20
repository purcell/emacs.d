;;; init-elm.el --- Support for the Elm language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'elm-mode)
  (with-eval-after-load 'elm-mode
    (diminish 'elm-indent-mode)
    (when (executable-find "elm-format")
      (setq-default elm-format-on-save t)))
  (maybe-require-package 'elm-test-runner))

(provide 'init-elm)
;;; init-elm.el ends here
