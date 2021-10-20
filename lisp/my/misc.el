;;; misc.el --- Provide custom configurations
;;; Commentary:
;;; Code:

;; google-this
(require-package 'google-this)
(google-this-mode 1)

(require 'all-the-icons)

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

;; Nameframe
(require-package 'nameframe-projectile)
(require 'nameframe-projectile)
(projectile-global-mode)
(nameframe-projectile-mode t)
;; If your OS can't switch between applications windows by default *cough* OS X *cough*
;; you can have a shortcut to switch between existing frames by name
(global-set-key (kbd "M-P") 'nameframe-switch-frame)

;; Spaceline
(custom-set-faces
 '(mode-line ((t (:underline nil))))
 '(mode-line-inactive ((t (:underline nil)))))
(require-package 'doom-modeline)
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; Org-mode
(setq org-todo-keywords '((sequence "Todo" "Doing" "Verify" "|" "Done" "Delegated" "Canceled")))
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

;; Latex indent
(setq-default LaTeX-indent-level 4)
(setq-default LaTeX-item-indent 0)

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

;; indent highlights
(require-package 'highlight-indent-guides)
(require 'highlight-indent-guides)

;; magit-todo
(magit-todos-mode t)

;; awesome-tab

(provide 'misc)
;; End
