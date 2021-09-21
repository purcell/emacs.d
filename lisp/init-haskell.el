;;; init-haskell.el --- Support the Haskell language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'haskell-mode)
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'haskell-cabal-mode 'subword-mode)

  (when (maybe-require-package 'dante)
    (add-hook 'haskell-mode-hook 'dante-mode)
    (with-eval-after-load 'dante
      (flycheck-add-next-checker 'haskell-dante
                                 '(warning . haskell-hlint))))

  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  (add-auto-mode 'haskell-mode "\\.ghci\\'")

  ;; Indentation
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


  ;; Source code helpers

  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

  (when (maybe-require-package 'reformatter)
    (reformatter-define hindent
      :program "hindent"
      :lighter " Hin")

    (defalias 'hindent-mode 'hindent-on-save-mode)

    (reformatter-define ormolu
      :program "ormolu"
      :lighter " Orm"))

  (with-eval-after-load 'haskell-mode
    (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
    (define-key haskell-mode-map (kbd "C-o") 'open-line))


  (with-eval-after-load 'page-break-lines
    (add-to-list 'page-break-lines-modes 'haskell-mode)))



(define-minor-mode stack-exec-path-mode
  "If this is a stack project, set `exec-path' to the path \"stack exec\" would use."
  nil
  :lighter ""
  :global nil
  (if stack-exec-path-mode
      (when (and (executable-find "stack")
                 (locate-dominating-file default-directory "stack.yaml"))
        (let ((stack-path (replace-regexp-in-string
                           "[\r\n]+\\'" ""
                           (shell-command-to-string (concat "stack exec -- sh -c "
                                                            (shell-quote-argument "echo $PATH"))))))
          (setq-local exec-path (seq-uniq (parse-colon-path stack-path) 'string-equal))
          (make-local-variable 'process-environment)
          (setenv "PATH" (string-join exec-path path-separator))))
    (kill-local-variable 'exec-path)
    (kill-local-variable 'process-environment)))

(add-hook 'haskell-mode-hook 'stack-exec-path-mode)



(when (maybe-require-package 'dhall-mode)
  (add-hook 'dhall-mode-hook 'stack-exec-path-mode))




(provide 'init-haskell)
;;; init-haskell.el ends here
