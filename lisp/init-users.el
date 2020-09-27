;;; init-users.el --- Provide custom configurations
;;; Commentary:
;;; Code:

(require 'init-local nil t)
(setq frame-resize-pixelwise t)

(require-package 'use-package)

;; Evil
(require-package 'evil)
(require 'evil)
(evil-mode 1)

(require-package 'neotree)
(setq neo-smart-open t)
(global-set-key [f2] 'neotree-toggle)
(setq projectile-switch-project-action 'neotree-projectile-action)
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


;; google-this
(require-package 'google-this)
(google-this-mode 1)

;; sr-speedbar
(require-package 'sr-speedbar)
(defun sr-speedbar-toggle-fixed-size ()
  "Toggle sr-speedbar in fixed size."
  (interactive)
  (sr-speedbar-toggle)
  (with-current-buffer sr-speedbar-buffer-name
    (setq window-size-fixed 'width)))
(global-set-key [f8] 'sr-speedbar-toggle-fixed-size)

;; Tab setting
(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (setq tab-width 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defun my-c-mode-hook ()
  (setq c-indent-level 4)
  (setq tab-width 4)
  (setq c-basic-offset 4))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(setq js-indent-level 4)
(setq-default LaTeX-indent-level 4)
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
(add-hook 'c-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "C-\\") 'evil-jump-to-tag)))


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

;;(require-package 'zenburn-theme)
;;(require 'zenburn-theme)
;;(load-theme 'zenburn t)

;; Tango 2
;;(require-package 'tango-2-theme)
;;(load-theme 'tango-2)

;; Powerline
(require-package 'powerline)
(require 'powerline)
(powerline-center-theme)

;; Org-mode
(setq org-todo-keywords '((sequence "Todo" "Doing" "Verify" "|" "Done" "Delegated")))
(setq org-todo-keyword-faces
      '(("Todo" . (:background "" :foreground "red"))
        ("Doing" . (:background "" :foreground "red"))
        ("Verify" . (:background "" :foreground "orange"))
        ("Done" . (:background "" :foreground "green"))))

;; optional: this is the evil state that evil-magit will use
;; (setq evil-magit-state 'normal)
;; optional: disable additional bindings for yanking text
;; (setq evil-magit-use-y-for-yank nil)
(require-package 'evil-magit)
(require 'evil-magit)

(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))

;; TypeScript
(require-package 'tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)


;; LSP
(require-package 'lsp-mode)
(require-package 'lsp-ui)
(require-package 'lsp-treemacs)
(require-package 'dap-mode)
(require-package 'which-key)

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))


;; Python LSP
(add-hook 'python-mode-hook #'lsp)
(setq-default lsp-pyls-configuration-sources ["flake8"])
(with-eval-after-load 'lsp-mode  ; try this or similar
  (lsp-register-custom-settings '(("pyls.plugins.pyls_mypy.enabled" t t))))

;; Typescript LSP
(add-hook 'typescript-mode-hook #'lsp)


;; C LSP
(add-hook 'c-mode-hook #'lsp)
(require-package 'ccls)

;; CPP LSP
(add-hook 'c++-mode-hook #'lsp)

;; Haskell LSP
(require-package 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(use-package lsp-haskell
  :ensure t
  :config
  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
  ;; Comment/uncomment this line to see interactions between lsp client/server.
  ;;(setq lsp-log-io t)
  )

(provide 'init-users)
;;; init-users.el ends here
