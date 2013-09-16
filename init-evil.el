(evil-mode 1)

; @see https://github.com/timcharper/evil-surround
(global-surround-mode 1)
(defun toggle-org-or-message-mode ()
  (interactive)
  (if (eq major-mode 'message-mode)
    (org-mode)
    (if (eq major-mode 'org-mode) (message-mode))
    ))

;; (evil-set-initial-state 'org-mode 'emacs)
;; Remap org-mode meta keys for convenience
(evil-declare-key 'normal org-mode-map
    "gh" 'outline-up-heading
    "gl" 'outline-next-visible-heading
    "H" 'org-beginning-of-line ; smarter behaviour on headlines etc.
    "L" 'org-end-of-line ; smarter behaviour on headlines etc.
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
    (kbd "TAB") 'org-cycle
    )

(loop for (mode . state) in
      '(
        (Info-mode . emacs)
        (term-mode . emacs)
        (log-edit-mode . emacs)
        (inf-ruby-mode . emacs)
        (yari-mode . emacs)
        (erc-mode . emacs)
        (gud-mode . emacs)
        (help-mode . emacs)
        (eshell-mode . emacs)
        (shell-mode . emacs)
        ;;(message-mode . emacs)
        (magit-log-edit-mode . emacs)
        (fundamental-mode . emacs)
        (gtags-select-mode . emacs)
        (weibo-timeline-mode . emacs)
        (weibo-post-mode . emacs)
        (diff-mode . emacs)
        (sr-mode . emacs)
        (dired-mode . emacs)
        (compilation-mode . emacs)
        (speedbar-mode . emacs)
        (magit-commit-mode . normal)
        )
      do (evil-set-initial-state mode state))

(define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
(define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)

(define-key evil-normal-state-map "Y" (kbd "y$"))
(define-key evil-normal-state-map "+" 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map "-" 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map "go" 'goto-char)

;; {{ evil-matchit
(defun my-evil-jump-item-enhanced-for-html ()
  (interactive)
  (if (or (eq major-mode 'html-mode)
          (eq major-mode 'xml-mode)
          (eq major-mode 'nxml-mode)
          (eq major-mode 'web-mode)
          )
      (progn
        (if (not (my-sp-select-next-thing 1)) (exchange-point-and-mark))
        (deactivate-mark)
        )
    (progn
      (evil-jump-item)
      )
    )
  )
(define-key evil-normal-state-map "%" 'my-evil-jump-item-enhanced-for-html)
;; }}


(eval-after-load "evil" '(setq expand-region-contract-fast-key "z"))

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


(define-key evil-insert-state-map (kbd "M-a") 'move-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "M-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(global-set-key (kbd "M-k") 'keyboard-quit)
(define-key evil-insert-state-map (kbd "M-k") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "M-k") 'evil-exit-visual-state)
(define-key evil-visual-state-map (kbd ",k") 'evil-exit-visual-state)
(define-key minibuffer-local-map (kbd "M-k") 'abort-recursive-edit)
(define-key minibuffer-local-map (kbd ",k") 'abort-recursive-edit)
(define-key evil-insert-state-map (kbd "M-j") 'my-yas-expand)

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

;; {{ evil-leader config
(setq evil-leader/leader "," evil-leader/in-all-states t)
(require 'evil-leader)
(evil-leader/set-key
  "as" 'ack-same
  "ac" 'ack
  "aa" 'ack-find-same-file
  "af" 'ack-find-file
  "em" 'erase-message-buffer
  "eb" 'eval-buffer
  "ee" 'eval-expression
  "cx" 'copy-to-x-clipboard
  "cfn" 'copy-filename-of-current-buffer
  "cfp" 'copy-full-path-of-current-buffer
  "pp" 'evil-scroll-page-up
  "nn" 'evil-scroll-page-down
  "ff" 'toggle-full-window ;; I use WIN+F in i3
  "px" 'paste-from-x-clipboard
  ;; "ci" 'evilnc-comment-or-uncomment-lines
  ;; "cl" 'evilnc-comment-or-uncomment-to-the-line
  ;; "cc" 'evilnc-copy-and-comment-lines
  ;; "cp" 'evilnc-comment-or-uncomment-paragraphs
  "ct" 'ctags-create-or-update-tags-table
  "cs" 'evilcvn-change-symbol-in-defun
  "tt" 'ido-goto-symbol ;; same as my vim hotkey
  "cg" 'helm-ls-git-ls
  "ud" '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer))))
  "W" 'save-some-buffers
  "K" 'kill-buffer-and-window ;; "k" is preserved to replace "C-g"
  "it" 'issue-tracker-increment-issue-id-under-cursor
  "hh" 'highlight-symbol-at-point
  "hn" 'highlight-symbol-next
  "hp" 'highlight-symbol-prev
  "hq" 'highlight-symbol-query-replace
  "bm" 'pomodoro-start ;; beat myself
  "." 'evil-ex
  ;; toggle overview,  @see http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
  "oo" 'switch-window
  "ov" '(lambda () (interactive) (set-selective-display (if selective-display nil 1)))
  "or" 'open-readme-in-git-root-directory
  "mq" '(lambda () (interactive) (man (concat "-k " (thing-at-point 'symbol))))
  "gg" '(lambda () (interactive) (w3m-search "g" (thing-at-point 'symbol)))
  "qq" '(lambda () (interactive) (w3m-search "q" (thing-at-point 'symbol)))
  "hr" 'helm-recentf
  "s0" 'delete-window
  "s1" 'delete-other-windows
  "s2" 'split-window-below
  "s3" 'split-window-right
  "su" 'winner-undo
  "sp" '(lambda (&optional NUM)
          (interactive "p")
          ;; move cursor
          (if (or (eq major-mode 'html-mode)
                  (eq major-mode 'xml-mode)
                  (eq major-mode 'nxml-mode)
                  (eq major-mode 'web-mode)
                  )
              (my-sp-select-next-thing NUM)
            (sp-select-next-thing)
              )
          )
  "ls" 'package-list-packages
  "hs" 'w3mext-hacker-search
  "gf" 'gtags-find-tag-from-here
  "gp" 'gtags-pop-stack
  "gr" 'gtags-find-rtag
  "fb" 'flyspell-buffer
  "gy" 'gtags-find-symbol
  "dg" 'djcb-gtags-create-or-update
  "bc" '(lambda () (interactive) (wxhelp-browse-class-or-api (thing-at-point 'symbol)))
  "ma" 'mc/mark-all-like-this-in-defun
  "mw" 'mc/mark-all-words-like-this-in-defun
  "ms" 'mc/mark-all-symbols-like-this-in-defun
  ;; recommended in html
  "md" 'mc/mark-all-like-this-dwim
  "rw" 'rotate-windows
  "oc" 'occur
  "om" 'toggle-org-or-message-mode
  "ops" 'my-org2blog-post-subtree
  "al" 'align-regexp
  "ww" 'save-buffer
  "bk" 'buf-move-up
  "bj" 'buf-move-down
  "bh" 'buf-move-left
  "bl" 'buf-move-right
  "0" 'select-window-0
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "4" 'select-window-4
  "5" 'select-window-5
  "6" 'select-window-6
  "7" 'select-window-7
  "8" 'select-window-8
  "9" 'select-window-9
  "xm" 'smex
  "xx" 'er/expand-region
  "xf" 'ido-find-file
  "xb" 'ido-switch-buffer
  "xc" 'save-buffers-kill-terminal
  "xo" 'helm-find-files
  "vd" 'scroll-other-window
  "vu" '(lambda () (interactive) (scroll-other-window-down nil))
  "jj" 'w3mext-search-js-api-mdn
  "xk" 'ido-kill-buffer
  "xs" 'save-buffer
  "xz" 'suspend-frame
  "xvv" 'vc-next-action
  "xv=" 'vc-diff
  "xvl" 'vc-print-log
  "xvp" 'git-messenger:popup-message
  )
;; }}

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))

;; comment/uncomment lines
(evilnc-default-hotkeys)

(provide 'init-evil)
