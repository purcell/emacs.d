;;; init-haskell.el --- Support the Haskell language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'haskell-mode)


;; Use intero for completion and flycheck

(add-hook 'haskell-mode-hook 'subword-mode)
(add-hook 'haskell-cabal-mode 'subword-mode)

(when (maybe-require-package 'dante)
  (add-hook 'haskell-mode-hook 'dante-mode)
  (after-load 'dante
    (flycheck-add-next-checker 'haskell-dante
                               '(warning . haskell-hlint))))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(add-auto-mode 'haskell-mode "\\.ghci\\'")

;; Workaround for https://github.com/haskell/haskell-mode/issues/1577
(when (eq 25 emacs-major-version)
  (defun sanityinc/inhibit-bracket-inside-comment-or-default (ch)
    (or (nth 4 (syntax-ppss))
        (funcall #'electric-pair-default-inhibit ch)))
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq-local electric-pair-inhibit-predicate 'sanityinc/inhibit-bracket-inside-comment-or-default))))


;; Indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)



;; Source code helpers

(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

(when (maybe-require-package 'reformatter)
  (reformatter-define hindent
    :program "hindent"
    :lighter " Hin")

  (defalias 'hindent-mode 'hindent-on-save-mode))

(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  (define-key haskell-mode-map (kbd "C-o") 'open-line))


(after-load 'page-break-lines
  (push 'haskell-mode page-break-lines-modes))



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
