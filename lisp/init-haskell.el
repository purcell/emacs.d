;; TODO: https://wunki.org/posts/2014-05-17-haskell-packages-development.html
;; https://github.com/chrisdone/chrisdone-emacs/blob/master/config/haskell.el
;; TODO: ghci-ng
;; TODO: don't pop up *Warnings* if haskell-stylish-on-save fails
;; TODO: purescript-mode
(require-package 'haskell-mode)


;; Use intero for completion and flycheck

(when (maybe-require-package 'intero)
  (after-load 'haskell-mode
    (intero-global-mode)
    (add-hook 'haskell-mode-hook 'eldoc-mode))
  (after-load 'intero
    ;; Don't clobber counsel-ag binding
    (define-key intero-mode-map (kbd "M-?") nil)
    (after-load 'flycheck
      (flycheck-add-next-checker 'intero
                                 '(warning . haskell-hlint)))))


(add-auto-mode 'haskell-mode "\\.ghci\\'")


;; Indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)



;; Source code helpers

(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

(setq-default haskell-stylish-on-save t)

(when (maybe-require-package 'hindent)
  (add-hook 'haskell-mode-hook 'hindent-mode))

(maybe-require-package 'hayoo)
(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  (define-key haskell-mode-map (kbd "C-o") 'open-line))


(after-load 'page-break-lines
  (push 'haskell-mode page-break-lines-modes))


(after-load 'haskell
  (define-key interactive-haskell-mode-map (kbd "M-N") 'haskell-goto-next-error)
  (define-key interactive-haskell-mode-map (kbd "M-P") 'haskell-goto-prev-error))


(provide 'init-haskell)
