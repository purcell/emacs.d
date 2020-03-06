(require 'init-local nil t)

;; Evil
(require-package 'evil)
(require 'evil)
(evil-mode 1)

(require-package 'neotree)
(global-set-key [f2] 'neotree-toggle)

(defun sr-speedbar-toggle-fixed-size ()
  "Toggle sr-speedbar in fixed size."
  (interactive)
  (sr-speedbar-toggle)
  (with-current-buffer sr-speedbar-buffer-name
    (setq window-size-fixed 'width)))

(global-set-key [f8] 'sr-speedbar-toggle-fixed-size)



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
(setq js-indent-level 4)
(setq-default LaTeX-indent-level tab-width)
(setq-default LaTeX-item-indent 0)

;; syntax highlight
(global-font-lock-mode 1)
(transient-mark-mode t)

;; Fold
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Company
(require-package 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; Company-clang
(setq company-backends (delete 'company-semantic company-backends))
;;(define-key c-mode-map  [(tab)] 'company-complete)
;;(define-key c++-mode-map  [(tab)] 'company-complete)

;; Switch window by number
(require-package 'window-numbering)
(window-numbering-mode 1)

;; YaSnippet
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
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
            (define-key evil-normal-state-local-map (kbd "C-\\") 'intero-goto-definition)))


(require-package 'smooth-scrolling)
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 5)

(require-package 'flycheck-mypy)
(require 'flycheck-mypy)
(add-hook 'python-mode-hook 'flycheck-mode)

(custom-set-variables
 '(flycheck-python-pycompile-executable "python3"))
(custom-set-variables
 '(flycheck-python-flake8-executable "python3"))
(custom-set-variables
 '(flycheck-python-pylint-executable "python3"))

(flycheck-add-next-checker 'python-pycompile 'python-mypy)
(flycheck-add-next-checker 'python-flake8 'python-mypy)

;;(add-to-list 'flycheck-disabled-checkers 'python-flake8)
;;(add-to-list 'flycheck-disabled-checkers 'python-pylint)

;; Nameframe
(require-package 'nameframe-projectile)
(require 'nameframe-projectile)
(projectile-global-mode)
(nameframe-projectile-mode t)
;; If your OS can't switch between applications windows by default *cough* OS X *cough*
;; you can have a shortcut to switch between existing frames by name
(global-set-key (kbd "M-P") 'nameframe-switch-frame)

;; Python3 as default interpreter
(setq py-python-command "/usr/bin/python3")

;; Elpy
(require-package 'elpy)
(require 'elpy)
(setq elpy-rpc-python-command "python3")
(elpy-enable)

(add-hook 'python-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "C-\\") 'elpy-goto-definition)))

;; Solarized
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(setq solarized-italic nil)
(load-theme 'solarized t)


;; Tango 2
;;(require-package 'tango-2-theme)
;;(load-theme 'tango-2)

;; Powerline
(require 'powerline)
(powerline-default-theme)

(provide 'init-users)
