(helm-mode 1)

;; helm-gtags ==begin
;; customize
(setq helm-c-gtags-path-style 'relative)
(setq helm-c-gtags-ignore-case t)
(setq helm-c-gtags-read-only t)

(add-hook 'c-mode-common-hook (lambda () (helm-gtags-mode)))

;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
              (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
              (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
              (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
              (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)
              (local-set-key (kbd "C-c C-f") 'helm-gtags-pop-stack)))
;; ==end

(if *emacs24*
    (progn
      (autoload 'helm-c-yas-complete "helm-c-yasnippet" nil t)
      (global-set-key (kbd "C-x C-o") 'helm-find-files)
      (global-set-key (kbd "C-c f") 'helm-for-files)
      (global-set-key (kbd "C-c y") 'helm-c-yas-complete)
      (global-set-key (kbd "C-c ;") 'helm-ls-git-ls)
      (global-set-key (kbd "C-c i") 'helm-imenu)
      )
  (global-set-key (kbd "C-x C-o") 'ffap)
  )
(provide 'init-helm)
