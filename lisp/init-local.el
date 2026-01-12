;; init-local.el --- Local configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (eq system-type 'windows-nt))
;; (defconst ORG-PATH
;;   (cond (IS-MAC "~/Library/CloudStorage/Dropbox/org")
;;         (IS-LINUX "~/Dropbox/org")))
(defconst FALLBACK-FONTS '("Jigmo" "Jigmo2" "Jigmo3"))
(defconst FONT-SIZE (if IS-MAC 16 13))
(defconst DEFAULT-FONT
  (format "%s %d"
          (if IS-WINDOWS
              "Aporetic Sans M Nerd Font"
            "MonoLisa Nerd Font")
          FONT-SIZE))
(defconst ORG-FONT
  (format "%s %d"
          (if IS-WINDOWS
              "Aporetic Sans M Nerd Font"
            "MonoLisa Nerd Font")
          FONT-SIZE))
(defconst ZH-DEFAULT-FONT "Maple Mono NF CN")
(defconst NERD-ICONS-FONT "Symbols Nerd Font Mono")
(defconst EMOJI-FONTS '("Apple Color Emoji"
                        "Noto Color Emoji"
                        "Noto Emoji"
                        "Segoe UI Emoji"))
(defconst SYMBOL-FONT '("PragmataPro"
                        "Apple Symbols"
                        "Segoe UI Symbol"
                        "Symbola"
                        "Symbol"))
;; load module settings
(dolist (dir '("lisp" "lib" "site-lisp"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))



;; Load themes config
(require 'init-local-themes)



(use-package evil
  :ensure t
  :demand t
  :init
  ;; These must be set before evil loads
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)

  :config
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Cursor styles
  (setq evil-normal-state-cursor '(box))
  (setq evil-insert-state-cursor '((hbar . 3)))

  ;; Blinking only in insert mode
  (add-hook 'evil-insert-state-entry-hook (lambda () (blink-cursor-mode 1)))
  (add-hook 'evil-insert-state-exit-hook (lambda () (blink-cursor-mode 0)))

  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint))

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

;; auto-save
(add-to-list 'load-path "~/github/auto-save/") ; add auto-save to your load-path
(require 'auto-save)
(auto-save-enable)

(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving

;;; custom predicates if you don't want auto save.
;;; disable auto save mode when current filetype is an gpg file.
(setq auto-save-disable-predicates
      '((lambda ()
          (string-suffix-p
           "gpg"
           (file-name-extension (buffer-name)) t))))

;; zoxide
(use-package zoxide
  :ensure t)

(defun dired-jump-with-zoxide (&optional other-window)
  "Jump to a directory using zoxide and open it in Dired.
If OTHER-WINDOW is non-nil, open the directory in another window."
  (interactive "P")
  (zoxide-open-with nil (lambda (file) (dired-jump other-window file)) t))


;; Load OS-specific configurations
(cond
 ((eq system-type 'windows-nt)
  (require 'init-local-windows nil t)) ;; nil t = don't error if missing
 ((eq system-type 'darwin)
  (require 'init-local-macos nil t)
  (require 'init-local-ai nil t)
  (require 'init-local-majutsu nil t))
 ((eq system-type 'gnu/linux)
  (require 'init-local-linux nil t)))

;; Load org config
(require 'init-local-org)

(use-package vim-tab-bar
  :ensure t
  :commands vim-tab-bar-mode
  :init
  (defun my/setup-vim-tab-bar ()
    (vim-tab-bar-mode 1)
    (setq tab-bar-tab-hints t))
  :hook (after-init . my/setup-vim-tab-bar))

;; restart emacs command
(use-package restart-emacs
  :ensure t)

(setq dired-listing-switches "-Ahlt --time-style=long-iso")

(defun insert-timestamp ()
  "Insert current timestamp in format YYYYMMDDTHHMM."
  (interactive)
  (insert (format-time-string "%Y%m%dT%H%M")))

(rg-define-search rg-search-everything
  "Search files including hidden and ignore"
  :query ask
  :format literal
  :files "everything"
  :flags ("--hidden" "--no-ignore")
  :menu ("Search" "hn" "Everything"))

(require 'init-local-denote)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package substitute
  :ensure t)

;; If you want a message reporting the matches that changed in the
;; given context.  We don't do it by default.
(add-hook 'substitute-post-replace-functions #'substitute-report-operation)

(use-package ztree
  :ensure t)

(with-eval-after-load 'ztree-diff
  ;; Automatically show hidden files (like .config, .bashrc, etc.)
  (setq ztree-show-hidden-files t)

  ;; Optional: Set filter patterns for files you DO want to ignore
  (setq ztree-diff-filter-list
        '("^\\.git$"
          "^\\.jj$"
          "^node_modules$"
          "^__pycache__$"      ;; Added ^ for exact match
          "\\.pyc$"            ;; Matches any .pyc file
          "~$"                 ;; Backup files ending with ~
          "^\\.DS_Store$"      ;; Optional: macOS files
          "^target$"))         ;; Optional: Rust/Java build dirs

  (evil-define-key 'normal ztree-mode-map
    ;; Navigation
    (kbd "j") 'ztree-next-line
    (kbd "k") 'ztree-previous-line
    (kbd "g g") 'ztree-jump-to-first-line
    (kbd "G") 'ztree-jump-to-last-line

    ;; Open/close directories and diff
    (kbd "RET") 'ztree-perform-action
    (kbd "SPC") 'ztree-perform-soft-action

    ;; Directory navigation
    (kbd "<backspace>") 'ztree-move-up-in-tree
    (kbd "DEL") 'ztree-move-up-in-tree
    (kbd "x") 'ztree-toggle-expand-subtree

    ;; Panel switching
    (kbd "TAB") 'ztree-jump-side
    (kbd "<tab>") 'ztree-jump-side

    ;; Toggle visibility
    (kbd "h") 'ztree-diff-toggle-show-equal-files
    (kbd "H") 'ztree-diff-toggle-show-filtered-files

    ;; File operations
    (kbd "C") 'ztree-diff-copy
    (kbd "D") 'ztree-diff-delete-file
    (kbd "v") 'ztree-diff-view-file

    ;; Refresh
    (kbd "r") 'ztree-diff-partial-rescan
    (kbd "<f5>") 'ztree-diff-full-rescan
    (kbd "F5") 'ztree-diff-full-rescan

    ;; Quit
    (kbd "q") 'quit-window
    (kbd "Q") 'kill-this-buffer

    ;; Help
    (kbd "?") 'ztree-diff-toggle-help))

(use-package dash
  :ensure t)

(setq warning-minimum-level :error           ; Only pop up *Warnings* buffer for errors
      warning-minimum-log-level :warning)    ; Log warnings and above to *Messages*

;; just for looks
(use-package vertico-posframe
  :ensure t
  :hook (after-init . vertico-posframe-mode)
  :custom
  (vertico-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8))))

(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1)
  :custom
  ;; Exclude specific modes from golden-ratio
  (golden-ratio-exclude-modes '(ediff-mode))
  ;; Exclude specific buffer names
  (golden-ratio-exclude-buffer-names '("*NeoTree*" "*Treemacs*"))
  ;; Auto-scale windows
  (golden-ratio-auto-scale t))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


;; useage /sshx:host: or /-:host:
(setq tramp-default-method "sshx")
(use-package tramp-term
  :ensure t)

(use-package pulsar
  :ensure t

  :bind
  ( :map global-map
    ("C-x l" . pulsar-pulse-line) ; overrides `count-lines-page'
    ("C-x L" . pulsar-highlight-permanently-dwim)) ; or use `pulsar-highlight-temporarily-dwim'

  :init
  (pulsar-global-mode 1)

  :config
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 5)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-region-face 'pulsar-yellow)
  (setq pulsar-highlight-face 'pulsar-magenta)

  :hook
  ;; Minibuffer + next-error
  (minibuffer-setup-hook . pulsar-pulse-line)
  (next-error-hook       . pulsar-pulse-line)

  ;; consult integration
  (consult-after-jump-hook . pulsar-recenter-top)
  (consult-after-jump-hook . pulsar-reveal-entry)

  ;; imenu integration
  (imenu-after-jump-hook . pulsar-recenter-top)
  (imenu-after-jump-hook . pulsar-reveal-entry))



(require 'init-local-shell)

(require 'blinko)

(require 'init-local-program)



(defun ml-update-all-packages ()
  "Update all installed packages."
  (interactive)
  (package-refresh-contents)
  (package-upgrade-all)
  (package-vc-upgrade-all))

(defun ml-init-ediff-current-with-other-window ()
  "Ediff current window buffer with the next window buffer."
  (interactive)
  (if (< (count-windows) 2)
      (user-error "Need at least two windows")
    (ediff-buffers (window-buffer)
                   (window-buffer (next-window (selected-window) 0)))))



;; Load keybinding
(require 'init-local-keybinding)

(provide 'init-local)
;;; init-local.el ends here
