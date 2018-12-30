;;; init-purescript.el --- Support the Purescript language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'purescript-mode)
  (when (maybe-require-package 'psc-ide)
    (add-hook 'purescript-mode-hook
              (lambda ()
                (psc-ide-mode)
                (company-mode)
                (flycheck-mode)
                (turn-on-purescript-indentation))))

  (when (maybe-require-package 'psci)
    (add-hook 'purescript-mode-hook 'inferior-psci-mode))

  (when (maybe-require-package 'add-node-modules-path)
    (after-load 'purescript-mode
      (add-hook 'purescript-mode-hook 'add-node-modules-path))
    (after-load 'psci
      (advice-add 'psci :around (lambda (oldfun &rest args)
                                  (let ((psci/purs-path (or (executable-find "purs")
                                                            psci/purs-path)))
                                    (apply oldfun args)))))))

(provide 'init-purescript)
;;; init-purescript.el ends here
