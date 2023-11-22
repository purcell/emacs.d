;;; init-purescript.el --- Support the Purescript language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'purescript-mode)
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)

  (add-hook 'purescript-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'purescript-sort-imports nil t)))

  (add-hook 'purescript-mode-hook (apply-partially 'prettify-symbols-mode -1))

  (with-eval-after-load 'purescript-mode
    (define-key purescript-mode-map (kbd "C-o") 'open-line))

  (when (maybe-require-package 'reformatter)
    (reformatter-define purty
      :program "purty" :lighter " purty"))

  (when (maybe-require-package 'psci)
    (add-hook 'purescript-mode-hook 'inferior-psci-mode))

  (when (maybe-require-package 'add-node-modules-path)
    (with-eval-after-load 'purescript-mode
      (add-hook 'purescript-mode-hook 'add-node-modules-path))
    (with-eval-after-load 'psci
      (advice-add 'psci :around (lambda (oldfun &rest args)
                                  (let ((psci/purs-path (or (executable-find "purs")
                                                            psci/purs-path)))
                                    (apply oldfun args)))))))

(provide 'init-purescript)
;;; init-purescript.el ends here
