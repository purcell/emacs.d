(evil-mode 1)
; evil-surround
; @see https://github.com/timcharper/evil-surround
(require 'surround)
(require 'evil-numbers)
(global-surround-mode 0)
(evil-set-initial-state 'org-mode 'emacs)
(evil-set-initial-state 'Info-mode 'emacs)
(evil-set-initial-state 'inf-ruby-mode 'emacs)
(evil-set-initial-state 'yari-mode 'emacs)
(evil-set-initial-state 'erc-mode 'emacs)
(evil-set-initial-state 'gud-mode 'emacs)
(evil-set-initial-state 'help-mode 'emacs)
; I need copy words from eshell history
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'shell-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'message-mode 'emacs)

(define-key evil-normal-state-map "Y" (kbd "y$"))
(define-key evil-normal-state-map "+" 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map "-" 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map "go" 'goto-char)

; evil-leader config
(setq evil-leader/leader "," evil-leader/in-all-states t)
(require 'evil-leader)
(evil-leader/set-key
  "t" 'ctags-create-or-update-tags-table
  "w" 'save-buffer
  "c" 'qiang-comment-dwim-line
  "W" 'save-some-buffers
  "K" 'kill-buffer-and-window
  "p" 'previous-error
  "n" 'next-error
  "g" 'magit-status
  "." 'evil-ex
  )

(provide 'init-evil)
