(require 'init-local nil t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'evil)
(evil-mode 1)

(load "auctex.el" nil t t)

(require 'neotree)
(global-set-key [f2] 'neotree-toggle)
(global-set-key [f8] 'sr-speedbar-toggle)

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
            (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
            (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
            (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
            (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))


(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;; Tab setting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset' tab-width)
(setq-default LaTeX-indent-level tab-width)
(setq-default LaTeX-item-indent 0)

;; syntax highlight
(global-font-lock-mode 1)
(transient-mark-mode t)

;; Fold
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; Company-clang
(setq company-backends (delete 'company-semantic company-backends))
;;(define-key c-mode-map  [(tab)] 'company-complete)
;;(define-key c++-mode-map  [(tab)] 'company-complete)

;; Switch window by number
(require 'window-numbering)
(window-numbering-mode 1)

;; YaSnippet
(require 'yasnippet)
(yas-global-mode 1)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(font-lock-add-keywords
 'c-mode
 '(("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face)))

(font-lock-add-keywords
 'c++-mode
 '(("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face)))

(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(add-hook 'haskell-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "C-,") 'intero-goto-definition)))

(setq haskell-indent-spaces 4)

(provide 'init-users)
