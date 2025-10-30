;;; init-local-majutsu.el --- Majutsu configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Majutsu Git porcelain configuration converted from Doom Emacs

;;; Code:

;; Ensure with-editor is loaded and configured
(use-package with-editor
  :ensure t
  :config
  ;; Keybindings for commit message buffers
  (define-key with-editor-mode-map (kbd "C-c C-c") #'with-editor-finish)
  (define-key with-editor-mode-map (kbd "C-c C-k") #'with-editor-cancel))

;; Transient is also required
(use-package transient
  :ensure t)

;; Declare autoloaded commands via use-package for lazy loading
(use-package majutsu
  :ensure t
  :vc (:url "https://github.com/0WD0/majutsu"
            :rev :newest)
  :commands (majutsu majutsu-log
                     majutsu-rebase-transient majutsu-bookmark-transient majutsu-git-transient
                     majutsu-commit majutsu-describe majutsu-diff majutsu-diffedit-emacs majutsu-diffedit-smerge
                     majutsu-log-refresh majutsu-mode-transient majutsu-squash-transient
                     majutsu-abandon majutsu-undo majutsu-new majutsu-enter-dwim majutsu-goto-current
                     majutsu-edit-changeset majutsu-redo majutsu-log-transient)

  :init
  ;; Keybindings: SPC j prefix (evil leader) - set before loading
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global
      (kbd "SPC j j") #'majutsu
      (kbd "SPC j c") #'majutsu-commit
      (kbd "SPC j d") #'majutsu-describe
      (kbd "SPC j D") #'majutsu-diff))

  :config
  ;; Per-mode evil bindings
  (with-eval-after-load 'evil
    ;; Disable evil-snipe in majutsu buffers if evil-snipe is loaded
    (when (fboundp 'turn-off-evil-snipe-mode)
      (add-hook 'majutsu-mode-hook #'turn-off-evil-snipe-mode))
    (when (fboundp 'turn-off-evil-snipe-override-mode)
      (add-hook 'majutsu-mode-hook #'turn-off-evil-snipe-override-mode))

    ;; Define keybindings for normal and visual states
    (evil-define-key '(normal visual) majutsu-mode-map
      (kbd ".")     #'majutsu-goto-current
      (kbd "R")     #'majutsu-log-refresh
      (kbd "g r")   #'majutsu-log-refresh
      (kbd "c")     #'majutsu-commit
      (kbd "e")     #'majutsu-edit-changeset
      (kbd "u")     #'majutsu-undo
      (kbd "C-r")   #'majutsu-redo
      (kbd "s")     #'majutsu-squash-transient
      (kbd "l")     #'majutsu-log-transient
      (kbd "d")     #'majutsu-describe
      (kbd "x")     #'majutsu-abandon
      (kbd "b")     #'majutsu-bookmark-transient
      (kbd "r")     #'majutsu-rebase-transient
      (kbd "D")     #'majutsu-diff
      (kbd "E")     #'majutsu-diffedit-emacs
      (kbd "M")     #'majutsu-diffedit-smerge
      (kbd "?")     #'majutsu-mode-transient
      (kbd "]")     #'magit-section-forward-sibling
      (kbd "[")     #'magit-section-backward-sibling)

    ;; RET binding for normal state only
    (evil-define-key 'normal majutsu-mode-map
      (kbd "RET")   #'majutsu-enter-dwim)))

(provide 'init-local-majutsu)
;;; init-local-majutsu.el ends here
