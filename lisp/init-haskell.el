(require-package 'haskell-mode)


;; Completion

;; Hook auto-complete into the completions provided by the inferior
;; haskell process, if any.

(require-package 'ac-haskell-process)

(add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)

(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c C-d") 'ac-haskell-process-popup-doc))

(after-load 'auto-complete
  (add-to-list 'ac-modes 'haskell-interactive-mode)
  (add-hook 'haskell-interactive-mode-hook 'set-auto-complete-as-completion-at-point-function))

(when (executable-find "ghci-ng")
  (setq-default haskell-process-args-cabal-repl
                '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng")))



;; Flycheck specifics

(when (and (maybe-require-package 'flycheck-haskell)
           (require-package 'flycheck-hdevtools))
  (after-load 'flycheck
    (add-hook 'haskell-mode-hook #'flycheck-haskell-setup)

    (defun sanityinc/flycheck-haskell-reconfigure ()
      "Reconfigure flycheck haskell settings, e.g. after changing cabal file."
      (interactive)
      (unless (eq major-mode 'haskell-mode)
        (error "Expected to be in haskell-mode"))
      (flycheck-haskell-clear-config-cache)
      (flycheck-haskell-configure)
      (flycheck-mode -1)
      (flycheck-mode))

    (after-load 'haskell-mode
      (require 'flycheck-hdevtools))))


;; Docs

(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook haskell-interactive-mode-hook))
  (add-hook hook (lambda () (subword-mode +1)))
  (add-hook hook (lambda () (eldoc-mode 1))))
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(add-hook 'haskell-interactive-mode-hook 'sanityinc/no-trailing-whitespace)


;; Interaction

(after-load 'haskell
  (diminish 'interactive-haskell-mode " IntHS"))

(add-auto-mode 'haskell-mode "\\.ghci\\'")

(when (maybe-require-package 'ghci-completion)
  (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))



;; Indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)



;; Source code helpers

(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

(setq-default haskell-stylish-on-save t)

(maybe-require-package 'hayoo)
(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  (define-key haskell-mode-map (kbd "C-o") 'open-line))


(after-load 'page-break-lines
  (push 'haskell-mode page-break-lines-modes))

;; Make compilation-mode understand "at blah.hs:11:34-50" lines output by GHC
(after-load 'compile
  (let ((alias 'ghc-at-regexp))
    (add-to-list
     'compilation-error-regexp-alist-alist
     (list alias " at \\(.*\\.\\(?:l?[gh]hs\\|hi\\)\\):\\([0-9]+\\):\\([0-9]+\\)-[0-9]+$" 1 2 3 0 1))
    (add-to-list
     'compilation-error-regexp-alist alias)))


;; Stop haskell-mode's compiler note navigation from clobbering highlight-symbol-nav-mode
(after-load 'haskell
  (define-key interactive-haskell-mode-map (kbd "M-n") nil)
  (define-key interactive-haskell-mode-map (kbd "M-p") nil)
  (define-key interactive-haskell-mode-map (kbd "M-N") 'haskell-goto-next-error)
  (define-key interactive-haskell-mode-map (kbd "M-P") 'haskell-goto-prev-error))


(provide 'init-haskell)
