;;; init-users.el --- Provide custom configurations
;;; Commentary:
;;; Code:

(require 'init-local nil t)
(setq frame-resize-pixelwise t)

(setq warning-minimum-level :emergency)

(require-package 'use-package)
(require-package 'alarm-clock)

;; Solarized
(setq x-underline-at-descent-line t)

;; Evil
(require-package 'evil)
(require 'evil)
(evil-mode 1)

(require-package 'neotree)
(setq neo-smart-open t)
(global-set-key (kbd "C-x t") 'neotree-toggle)
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

;; Coq
(require-package 'proof-general)
(require-package 'company-coq)
(require-package 'coq-commenter)
(add-hook 'coq-mode-hook #'company-coq-mode)
(add-hook 'coq-mode-hook 'coq-commenter-mode)

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
(global-set-key (kbd "C-x s") 'sr-speedbar-toggle-fixed-size)

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

(defun my-go-mode-hook ()
  (setq tab-width 4)
  (setq indent-tabs-mode 1))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(setq-default LaTeX-indent-level 4)
(setq-default LaTeX-item-indent 0)

(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 4)))

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
(setq or
      '(("Todo" . (:background "" :foreground "red"))
        ("Doing" . (:background "" :foreground "red"))
        ("Verify" . (:background "" :foreground "orange"))
        ("Done" . (:background "" :foreground "green"))))

;; optional: this is the evil state that evil-magit will use
;; (setq evil-magit-state 'normal)
;; optional: disable additional bindings for yanking text
;; (setq evil-magit-use-y-for-yank nil)
;; (require-package 'evil-magit)
;; (require 'evil-magit)

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
(use-package lsp-ui
  :commands  lsp-ui-mode
  :defer t
  :config
  (setq lsp-ui-imenu-auto-refresh t))
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; This feature does not support terminal so disable
(setq dap-auto-configure-features (remove 'controls dap-auto-configure-features))

;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))


;; Python LSP
(add-hook 'python-mode-hook #'lsp)
(require 'dap-python)
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
(require 'lsp)
(require 'lsp-haskell)
;; Hooks so haskell and literate haskell major modes trigger LSP setup
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

;; Go LSP
(add-hook 'go-mode-hook #'lsp-deferred)
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


;; Typescript LSP
(add-hook 'typescript-mode-hook #'lsp)
;; Angular LSP
(setq lsp-clients-angular-language-server-command
      '("node"
        "/usr/local/lib/node_modules/@angular/language-server"
        "--ngProbeLocations"
        "/usr/local/lib/node_modules"
        "--tsProbeLocations"
        "/usr/local/lib/node_modules"
        "--stdio"))

;; Org mode

;; Org preview
(require-package 'org-preview-html)

(setq org-latex-pdf-process
      (let
          ((cmd (concat "pdflatex -shell-escape -interaction nonstopmode"
                        " --synctex=1"
                        " -output-directory %o %f")))
        (list cmd
              "cd %o; if test -r %b.idx; then makeindex %b.idx; fi"
              "cd %o; bibtex %b"
              cmd
              cmd)))

(require-package 'org-roam)
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Notes/Brain"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(require 'org-roam-ui)
(require 'org-roam-protocol)

(use-package diff-hl
  :ensure t
  :demand
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  (defun user/maybe-diff-hl-margin-mode()
    (diff-hl-margin-mode (if (window-system) -1 1)))
  (dolist (it '(post-command-hook before-hack-local-variables-hook))
    (add-hook it 'user/maybe-diff-hl-margin-mode nil 1)))

(provide 'init-users)
;;; init-users.el ends here
