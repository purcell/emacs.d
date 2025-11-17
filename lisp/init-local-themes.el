;;; package --- init-local-themes  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; modus theme
(use-package modus-themes
  :ensure t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs t)

  ;; Configure the Modus Themes' appearance
  ;; frome https://systemcrafters.net/emacs-from-scratch/the-modus-themes/
  (setq modus-themes-mode-line '(accented borderless)
        modus-themes-fringes 'subtle
        modus-themes-tabs-accented t
        modus-themes-paren-match '(bold intense)
        modus-themes-prompts '(bold intense)
        modus-themes-completions
        '((matches . (extrabold underline))
          (selection . (extrabold underline)))
        modus-themes-org-blocks 'tinted-background
        modus-themes-scale-headings t
        modus-themes-region '(bg-only)
        modus-themes-headings
        '((1 . (rainbow overline background 1.4))
          (2 . (rainbow background 1.3))
          (3 . (rainbow bold 1.2))
          (t . (semilight 1.1))))

  ;; Apply more colorful foreground to some headings (headings 0-8).
  ;; Level 0 is for Org #+title and related.
  (setq modus-themes-common-palette-overrides
        '((fg-heading-1 blue-warmer)
          (bg-heading-1 bg-blue-nuanced)
          (overline-heading-1 blue)
          (fg-heading-2 yellow-cooler)
          (fg-heading-3 cyan-cooler)
          (prose-done fg-dim)

          ;; Make line numbers less intense
          (fg-line-number-inactive "gray50")
          (fg-line-number-active fg-main)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)))

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi-tinted :no-confirm)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; work with modus theme
(with-eval-after-load 'modus-themes
  (setq org-todo-keyword-faces
        `(("TODO" . (:foreground ,(modus-themes-get-color-value 'green-intense) :weight bold))
          ("NEXT" . (:foreground ,(modus-themes-get-color-value 'blue) :weight bold))
          ("DONE" . (:foreground ,(modus-themes-get-color-value 'fg-dim) :weight bold))
          ("WAITING" . (:foreground ,(modus-themes-get-color-value 'cyan) :weight bold))
          ("HOLD" . (:foreground ,(modus-themes-get-color-value 'magenta) :weight bold))
          ("CANCELLED" . (:foreground ,(modus-themes-get-color-value 'fg-dim) :weight bold))
          ("PHONE" . (:foreground ,(modus-themes-get-color-value 'rust) :weight bold))
          ("MEETING" . (:foreground ,(modus-themes-get-color-value 'rust) :weight bold)))))



(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(setq doom-modeline-height 18
      doom-modeline-buffer-file-name-style 'auto
      doom-modeline-buffer-modification-icon t
      doom-modeline-bar-width 4
      doom-modeline-hud t
      doom-modeline-hud-min-height 1)



(use-package spacious-padding
  :ensure t
  :init (spacious-padding-mode 1))



(use-package all-the-icons
  :ensure t
  :defer
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :ensure t
  :defer
  :hook (marginalia-mode . #'all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(provide 'init-local-themes)
;;; init-local-themes.el ends here
