(evil-mode 1)

; @see https://github.com/timcharper/evil-surround
(global-surround-mode 1)

;; (evil-set-initial-state 'org-mode 'emacs)
;; Remap org-mode meta keys for convenience
(evil-declare-key 'normal org-mode-map
    "gh" 'outline-up-heading
    "gj" 'org-forward-same-level
    "gk" 'org-backward-same-level
    "gl" 'outline-next-visible-heading
    "H" 'org-beginning-of-line ; smarter behaviour on headlines etc.
    "L" 'org-end-of-line ; smarter behaviour on headlines etc.
    "t" 'org-todo ; mark a TODO item as DONE
    ",c" 'org-cycle
    ",e" 'org-export-dispatch
    ",n" 'outline-next-visible-heading
    ",p" 'outline-previous-visible-heading
    ",t" 'org-set-tags-command
    "$" 'org-end-of-line ; smarter behaviour on headlines etc.
    "^" 'org-beginning-of-line ; ditto
    "-" 'org-ctrl-c-minus ; change bullet style
    "<" 'org-metaleft ; out-dent
    ">" 'org-metaright ; indent
    )

(mapcar (lambda (state)
          (evil-declare-key state org-mode-map
                            (kbd "TAB") 'org-cycle
                            (kbd "M-l") 'org-metaright
                            (kbd "M-h") 'org-metaleft
                            (kbd "M-k") 'org-metaup
                            (kbd "M-j") 'org-metadown
                            (kbd "M-L") 'org-shiftmetaright
                            (kbd "M-H") 'org-shiftmetaleft
                            (kbd "M-K") 'org-shiftmetaup
                            (kbd "M-J") 'org-shiftmetadown))
        '(normal insert))

(evil-set-initial-state 'Info-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'log-edit-mode 'emacs)
(evil-set-initial-state 'inf-ruby-mode 'emacs)
(evil-set-initial-state 'yari-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'erc-mode 'emacs)
(evil-set-initial-state 'gud-mode 'emacs)
(evil-set-initial-state 'help-mode 'emacs)
; I need copy words from eshell history
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'shell-mode 'emacs)
(evil-set-initial-state 'message-mode 'emacs)
(evil-set-initial-state 'magit-log-edit-mode 'emacs)
; message buffer
(evil-set-initial-state 'fundamental-mode 'emacs)
(evil-set-initial-state 'gtags-select-mode 'emacs)
(evil-set-initial-state 'weibo-timeline-mode 'emacs)
(evil-set-initial-state 'weibo-post-mode 'emacs)
(evil-set-initial-state 'diff-mode 'emacs)
(evil-set-initial-state 'sr-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'compilation-mode 'emacs)
(evil-set-initial-state 'speedbar-mode 'emacs)

(define-key evil-normal-state-map "Y" (kbd "y$"))
(define-key evil-normal-state-map "+" 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map "-" 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map "go" 'goto-char)

;; @see http://stackoverflow.com/questions/10569165/how-to-map-jj-to-esc-in-emacs-evil-mode
;; @see http://zuttobenkyou.wordpress.com/2011/02/15/some-thoughts-on-emacs-and-vim/
(define-key evil-insert-state-map "k" #'cofi/maybe-exit)
(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "k")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?j))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))

(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)

(defun evilcvn-change-symbol-in-defun ()
  "mark the region in defun (definition of function) and use string replacing UI in evil-mode
to replace the symbol under cursor"
  (interactive)
  (let ((old (thing-at-point 'symbol)))
    (mark-defun)
    (unless (evil-visual-state-p)
      (evil-visual-state))
    (evil-ex (concat "'<,'>s/" (if (= 0 (length old)) "" "\\<\\(") old (if (= 0 (length old)) "" "\\)\\>/"))))
  )
(global-set-key (kbd "C-c ; s") 'evilcvn-change-symbol-in-defun)

; evil-leader config
(setq evil-leader/leader "," evil-leader/in-all-states t)
(require 'evil-leader)
(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-comment-or-uncomment-to-the-line
  "cc" 'copy-and-comment-region
  "ct" 'ctags-create-or-update-tags-table
  "cs" 'evilcvn-change-symbol-in-defun
  "t" 'ido-goto-symbol ;; same as my vim hotkey
  "w" 'save-buffer
  "cp" 'compile
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
  "s" 'w3mext-hacker-search
  "d" 'gtags-find-tag-from-here
  "p" 'gtags-pop-stack
  "gr" 'gtags-find-rtag
  "gy" 'gtags-find-symbol
  "cg" 'djcb-gtags-create-or-update
  "bc" '(lambda () (interactive) (wxhelp-browse-class-or-api (thing-at-point 'symbol)))
  "ma" 'mc/mark-all-like-this-in-defun
  "mw" 'mc/mark-all-words-like-this-in-defun
  "ms" 'mc/mark-all-symbols-like-this-in-defun
  ;; recommended in html
  "md" 'mc/mark-all-like-this-dwim
  "rw" 'rotate-windows
  "l" 'align-regexp
  "x" 'er/expand-region
  )

;; comment/uncomment lines
(evilnc-default-hotkeys)

(provide 'init-evil)
