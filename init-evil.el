(evil-mode 1)
; evil-surround
; @see https://github.com/timcharper/evil-surround
(require 'surround)
(global-surround-mode 1)
(evil-set-initial-state 'org-mode 'emacs)
(evil-set-initial-state 'Info-mode 'emacs)
(evil-set-initial-state 'inf-ruby-mode 'emacs)
(evil-set-initial-state 'yari-mode 'emacs)
(evil-set-initial-state 'erc-mode 'emacs)
(evil-set-initial-state 'gud-mode 'emacs)
(evil-set-initial-state 'help-mode 'emacs)
; I need copy words from eshell history
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'message-mode 'emacs)

(setq evil-leader/leader "," evil-leader/in-all-states t)
(require 'evil-leader)
(evil-leader/set-key
  "w" 'save-buffer
  "W" 'save-some-buffers
  "K" 'kill-buffer-and-window
  "p" 'previous-error
  "n" 'next-error
  "g" 'magit-status
  "." 'evil-ex
  )

(provide 'init-evil)
