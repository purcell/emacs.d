;;; init-purescript.el --- Support the Purescript language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'purescript-mode)
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)

  (add-hook 'purescript-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'purescript-sort-imports nil t)))

  (add-hook 'purescript-mode-hook (apply-partially 'prettify-symbols-mode -1))

  (after-load 'purescript-mode
    (define-key purescript-mode-map (kbd "C-o") 'open-line))

  (when (maybe-require-package 'reformatter)
    (reformatter-define purty
      :program "purty" :lighter " purty"))

  (when (maybe-require-package 'psc-ide)
    (add-hook 'purescript-mode-hook 'psc-ide-mode)
    (add-hook 'psc-ide-mode-hook
              (lambda ()
                (setq-local flycheck-check-syntax-automatically '(save mode-enabled))))

    (defun psc-ide-foreign-js-after-save-handler ()
      "Call `psc-ide-rebuild' in any neighbouring purescript file buffer, if `psc-ide-rebuild-on-save' is set.
This is a little magical because it only works if the
corresponding .purs file is open."
      (let ((js-path (buffer-file-name)))
        (when js-path
          (let* ((purs-path (concat (file-name-sans-extension js-path) ".purs"))
                 (purs-buf (get-file-buffer purs-path)))
            (when purs-buf
              (with-current-buffer purs-buf
                (when psc-ide-mode
                  (cond
                   (psc-ide-rebuild-on-save
                    (message "Triggering rebuild of %s" purs-path)
                    (psc-ide-rebuild))
                   (flycheck-mode
                    (message "Flychecking %s" purs-path)
                    (flycheck-buffer))))))))))

    (define-minor-mode psc-ide-foreign-js-mode
      "Rebuild corresponding purescript file."
      nil
      :lighter " PursJS"
      :global nil
      (if psc-ide-foreign-js-mode
          (add-hook 'after-save-hook 'psc-ide-foreign-js-after-save-handler nil t)
        (remove-hook 'after-save-hook 'psc-ide-foreign-js-after-save-handler t))))

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
