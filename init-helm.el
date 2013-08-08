(helm-mode 1)

(setq helm-completing-read-handlers-alist
      '((describe-function . ido)
        (describe-variable . ido)
        (debug-on-entry . helm-completing-read-symbols)
        (find-function . helm-completing-read-symbols)
        (find-tag . helm-completing-read-with-cands-in-buffer)
        (ffap-alternate-file . nil)
        (tmm-menubar . nil)
        (dired-do-copy . nil)
        (dired-do-rename . nil)
        (dired-create-directory . nil)
        (find-file . ido)
        (copy-file-and-rename-buffer . nil)
        (rename-file-and-buffer . nil)
        (w3m-goto-url . nil)
        (ido-find-file . nil)
        (ido-edit-input . nil)
        (read-file-name . ni)
        (yas/compile-directory . ido)
        (yas-compile-directory . ido)
        (execute-extended-command . ido)
        ))

;; {{make helm-ls-git-ls more UI friendly
(require 'helm-ls-git)
(helm-attrset 'header-name
                '(lambda (name) (concat name ", `C-]' to toggle full path"))
                helm-source-ls-git)
(define-key helm-map (kbd ",k") 'helm-keyboard-quit)
;; }}

;; {{helm-gtags
;; customize
(setq helm-c-gtags-path-style 'relative)
(setq helm-c-gtags-ignore-case t)
(setq helm-c-gtags-read-only t)
(add-hook 'c-mode-hook (lambda () (helm-gtags-mode)))
(add-hook 'c++-mode-hook (lambda () (helm-gtags-mode)))
;; }}



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
      (global-set-key (kbd "C-c C-g") 'helm-ls-git-ls)
      (global-set-key (kbd "C-c i") 'helm-imenu)
      )
  (global-set-key (kbd "C-x C-o") 'ffap)
  )
(provide 'init-helm)
