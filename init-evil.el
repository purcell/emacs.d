(evil-mode 1)

; @see https://github.com/timcharper/evil-surround
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
(evil-set-initial-state 'shell-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'message-mode 'emacs)
; message buffer
(evil-set-initial-state 'fundamental-mode 'emacs)
(evil-set-initial-state 'gtags-select-mode 'emacs)
(evil-set-initial-state 'weibo-timeline-mode 'emacs)
(evil-set-initial-state 'weibo-post-mode 'emacs)

(define-key evil-normal-state-map "Y" (kbd "y$"))
(define-key evil-normal-state-map "+" 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map "-" 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map "go" 'goto-char)

; evil-leader config
(setq evil-leader/leader "," evil-leader/in-all-states t)
(require 'evil-leader)
(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cc" 'evilnc-comment-or-uncomment-to-the-line
  "ct" 'ctags-create-or-update-tags-table
  "t" 'ido-goto-symbol ;; same as my vim hotkey
  "ww" 'save-buffer
  "cl" 'compile
  "ud" '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer))))
  "W" 'save-some-buffers
  "K" 'kill-buffer-and-window
  "bm" 'pomodoro-start ; beat myself
  "." 'evil-ex
  ;; toggle overview,  @see http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
  "ov" '(lambda () (interactive) (set-selective-display (if selective-display nil 1)))
  "mq" '(lambda () (interactive) (man (concat "-k " (thing-at-point 'symbol))))
  "gg" '(lambda () (interactive) (w3m-search "g" (thing-at-point 'symbol)))
  "q" '(lambda () (interactive) (w3m-search "q" (thing-at-point 'symbol)))
  "s" '(lambda () (interactive) (require 'w3m) (browse-url-generic (concat "http://code.google.com/codesearch?q=" (w3m-url-encode-string (thing-at-point 'symbol)))))
  "d" 'gtags-find-tag-from-here
  "p" 'gtags-pop-stack
  "gr" 'gtags-find-rtag
  "gy" 'gtags-find-symbol
  "cg" 'djcb-gtags-create-or-update
  "bc" '(lambda () (interactive) (wxhelp-browse-class-or-api (thing-at-point 'symbol)))
  "bo" 'kill-other-buffers
  "bs" 'ido-switch-buffer
  "ma" 'mc/mark-all-like-this-in-defun
  "mw" 'mc/mark-all-words-like-this-in-defun
  "ms" 'mc/mark-all-symbols-like-this-in-defun
  ;; recommended in html
  "md" 'mc/mark-all-like-this-dwim
  "rw" 'rotate-windows
  "wc" 'wg-create-workgroup
  "ws" 'wgext-save
  "wl" 'wgext-load
  "wv" 'wg-switch-to-workgroup
  )

(provide 'init-evil)
