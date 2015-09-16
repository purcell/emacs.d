;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)


(global-set-key (kbd "s-.") 'copy-from-above-command)

;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Use C-x C-m to do M-x per Steve Yegge's advice
;;(global-set-key (kbd "C-x C-m") 'smex)

;; Expand region (increases selected region by semantic units)
(global-set-key (kbd "C-'") 'er/expand-region)

;; Experimental multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)

;;;; Mark additional regions matching current region
;;(global-set-key (kbd "M-æ") 'mc/mark-all-dwim)
;;(global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
;;(global-set-key (kbd "C-æ") 'mc/mark-next-like-this)
;;(global-set-key (kbd "C-Æ") 'mc/mark-more-like-this-extended)
;;(global-set-key (kbd "M-å") 'mc/mark-all-in-region)
;;
;;;; Symbol and word specific mark-more
;;(global-set-key (kbd "s-æ") 'mc/mark-next-word-like-this)
;;(global-set-key (kbd "s-å") 'mc/mark-previous-word-like-this)
;;(global-set-key (kbd "M-s-æ") 'mc/mark-all-words-like-this)
;;(global-set-key (kbd "s-Æ") 'mc/mark-next-symbol-like-this)
;;(global-set-key (kbd "s-Å") 'mc/mark-previous-symbol-like-this)
;;(global-set-key (kbd "M-s-Æ") 'mc/mark-all-symbols-like-this)
;;
;; Extra multiple cursors stuff
(global-set-key (kbd "C-~") 'mc/reverse-regions)
(global-set-key (kbd "M-~") 'mc/sort-regions)
(global-set-key (kbd "H-~") 'mc/insert-numbers)

(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Set anchor to start rectangular-region-mode
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; Replace rectangle-text with inline-string-rectangle
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

;; Quickly jump in document with ace-jump-mode
(define-key global-map (kbd "C-ø") 'ace-jump-mode)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

;; M-i for back-to-indentation
(global-set-key (kbd "M-i") 'back-to-indentation)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

(global-set-key (kbd "M-h") 'kill-region-or-backward-word)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Interactive selective display
(global-set-key (kbd "C-x $") 'inc-selective-display)

;; Change next underscore with a camel case
(global-set-key (kbd "C-c C--") 'replace-next-underscore-with-camel)
(global-set-key (kbd "M-s M--") 'snakeify-current-word)

;; Change word separators
(global-unset-key (kbd "C-x +")) ;; used to be balance-windows
(global-set-key (kbd "C-x + -") (λ (replace-region-by 's-dashed-words)))
(global-set-key (kbd "C-x + _") (λ (replace-region-by 's-snake-case)))
(global-set-key (kbd "C-x + c") (λ (replace-region-by 's-lower-camel-case)))
(global-set-key (kbd "C-x + C") (λ (replace-region-by 's-upper-camel-case)))

;; Killing text
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)
(global-set-key (kbd "s-w") 'save-region-or-current-line)
(global-set-key (kbd "M-W") (λ (save-region-or-current-line 1)))

;; Make shell more convenient, and suspend-frame less
(global-set-key (kbd "C-z") 'shell)
(global-set-key (kbd "C-x M-z") 'suspend-frame)
(global-set-key (kbd "C-x C-m") 'toggle-frame-maximized) ;; already have C-x RET



;; Zap to char
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "s-z") (lambda (char) (interactive "cZap up to char backwards: ") (zap-up-to-char -1 char)))

(global-set-key (kbd "M-Z") (lambda (char) (interactive "cZap to char: ") (zap-to-char 1 char)))
(global-set-key (kbd "s-Z") (lambda (char) (interactive "cZap to char backwards: ") (zap-to-char -1 char)))

;; iy-go-to-char - like f in Vim
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)
(global-set-key (kbd "s-m") 'jump-char-backward)

;; vim's ci and co commands
(global-set-key (kbd "M-I") 'change-inner)
(global-set-key (kbd "M-O") 'change-outer)

(global-set-key (kbd "s-i") 'copy-inner)
(global-set-key (kbd "s-o") 'copy-outer)

;; Create new frame
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; Jump to a definition in the current file. (This is awesome)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-p") 'find-or-create-file-at-point)
(global-set-key (kbd "C-x M-p") 'find-or-create-file-at-point-other-window)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "s-b") 'quick-switch-buffer)

(global-set-key (kbd "s-y") 'bury-buffer)

;; Revert without any fuss
(global-set-key (kbd "M-<escape>") (λ (revert-buffer t t)))

;; Edit file with sudo
(global-set-key (kbd "M-s e") 'sudo-edit)

;; Copy file path to kill ring
(global-set-key (kbd "C-x M-w") 'copy-current-file-path)

;; Window switching
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)
(global-unset-key (kbd "C-x C-+")) ;; don't zoom like this

(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

;; Add region to *multifile*
(global-set-key (kbd "C-!") 'mf/mirror-region-in-multifile)

;; Indentation help
(global-set-key (kbd "M-j") (λ (join-line -1)))

;; Help should search more than just commands
(global-set-key (kbd "<f1> a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; Navigation bindings
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(global-set-key (kbd "<prior>") 'beginning-of-buffer)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<next>") 'end-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "M-<up>") 'smart-up)
(global-set-key (kbd "M-<down>") 'smart-down)
(global-set-key (kbd "M-<left>") 'smart-backward)
(global-set-key (kbd "M-<right>") 'smart-forward)

;; Webjump let's you quickly search google, wikipedia, emacs wiki
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x M-g") 'browse-url-at-point)

;; Completion at point
(global-set-key (kbd "C-<tab>") 'completion-at-point)

;; Like isearch, but adds region (if any) to history and deactivates mark
(global-set-key (kbd "C-s") 'isearch-forward-use-region)
(global-set-key (kbd "C-r") 'isearch-backward-use-region)

;; Like isearch-*-use-region, but doesn't fuck with the active region
(global-set-key (kbd "C-S-s") 'isearch-forward)
(global-set-key (kbd "C-S-r") 'isearch-backward)

;; Move more quickly
(global-set-key (kbd "C-S-n") (λ (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (λ (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (λ (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (λ (ignore-errors (backward-char 5))))

(global-set-key (kbd "H-*") 'beginning-of-buffer) ;; H-p
(global-set-key (kbd "H-n") 'end-of-buffer)

;; Convenience on ThinkPad Keyboard: Use back/forward as pg up/down
(global-set-key (kbd "<XF86Back>") 'scroll-down)
(global-set-key (kbd "<XF86Forward>") 'scroll-up)
(global-set-key (kbd "<XF86WakeUp>") 'beginning-of-buffer)

;; Query replace regex key binding
(global-set-key (kbd "M-&") 'query-replace-regexp)

;; Yank selection in isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-yank-selection)

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Eval buffer
(global-set-key (kbd "C-c C-k") 'eval-buffer)

;; Create scratch buffer
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;; Move windows, even in org-mode
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status)
(autoload 'magit-status "magit")

;; Clever newlines
(global-set-key (kbd "C-o") 'open-line-and-indent)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "<M-return>") 'new-line-dwim)

;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Line movement
(global-set-key (kbd "<C-S-down>") 'move-text-down)
(global-set-key (kbd "<C-S-up>") 'move-text-up)

;; Fold the active region
(global-set-key (kbd "C-c C-f") 'fold-this-all)
(global-set-key (kbd "C-c C-F") 'fold-this)
(global-set-key (kbd "C-c M-f") 'fold-this-unfold-all)

;; Yank and indent
(global-set-key (kbd "C-S-y") 'yank-unindented)

;; Toggle quotes
(global-set-key (kbd "C-\"") 'toggle-quotes)

;; Sorting
(global-set-key (kbd "M-s l") 'sort-lines)

;; Increase number at point (or other change based on prefix arg)
(global-set-key (kbd "C-+") 'change-number-at-point)
(global-set-key (kbd "C-?") 'subtract-number-at-point)
(eval-after-load 'undo-tree '(define-key undo-tree-map (kbd "C-?") nil))

;; Browse the kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;; Buffer file functions
(global-set-key (kbd "C-x t") 'touch-buffer-file)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; Jump from file to containing directory
(global-set-key (kbd "C-x C-j") 'dired-jump) (autoload 'dired-jump "dired")
(global-set-key (kbd "C-x M-j") '(λ (dired-jump 1)))

;; Easy-mode fullscreen rgrep
(global-set-key (kbd "M-s s") 'git-grep-fullscreen)
(global-set-key (kbd "M-s S") 'rgrep-fullscreen)

;; Multi-occur
(global-set-key (kbd "M-s m") 'multi-occur)
(global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-c o") 'occur)

;; Find files by name and display results in dired
(global-set-key (kbd "M-s f") 'find-name-dired)

;; Find file in project
;;(global-set-key (kbd "C-x o") 'find-file-in-project)

;;Find file in project, with specific patterns
;;(global-unset-key (kbd "C-x C-o")) ;; which used to be delete-blank-lines (also bound to C-c C-<return>)
;;(global-set-key (kbd "C-x C-o ja") (ffip-create-pattern-file-finder "*.java"))
;;(global-set-key (kbd "C-x C-o js") (ffip-create-pattern-file-finder "*.js"))
;;(global-set-key (kbd "C-x C-o ht") (ffip-create-pattern-file-finder "*.html"))
;;(global-set-key (kbd "C-x C-o jp") (ffip-create-pattern-file-finder "*.jsp"))
;;(global-set-key (kbd "C-x C-o cs") (ffip-create-pattern-file-finder "*.css"))
;;(global-set-key (kbd "C-x C-o ft") (ffip-create-pattern-file-finder "*.feature"))
;;(global-set-key (kbd "C-x C-o cl") (ffip-create-pattern-file-finder "*.clj"))
;;(global-set-key (kbd "C-x C-o el") (ffip-create-pattern-file-finder "*.el"))
;;(global-set-key (kbd "C-x C-o ed") (ffip-create-pattern-file-finder "*.edn"))
;;(global-set-key (kbd "C-x C-o md") (ffip-create-pattern-file-finder "*.md"))
;;(global-set-key (kbd "C-x C-o rb") (ffip-create-pattern-file-finder "*.rb"))
;;(global-set-key (kbd "C-x C-o or") (ffip-create-pattern-file-finder "*.org"))
;;(global-set-key (kbd "C-x C-o ph") (ffip-create-pattern-file-finder "*.php"))
;;(global-set-key (kbd "C-x C-o tx") (ffip-create-pattern-file-finder "*.txt"))
;;(global-set-key (kbd "C-x C-o vm") (ffip-create-pattern-file-finder "*.vm"))
;;(global-set-key (kbd "C-x C-o xm") (ffip-create-pattern-file-finder "*.xml"))
;;(global-set-key (kbd "C-x C-o in") (ffip-create-pattern-file-finder "*.ini"))
;;(global-set-key (kbd "C-x C-o pr") (ffip-create-pattern-file-finder "*.properties"))
;;(global-set-key (kbd "C-x C-o in") (ffip-create-pattern-file-finder "*.ini"))
;;(global-set-key (kbd "C-x C-o gr") (ffip-create-pattern-file-finder "*.groovy"))
;;(global-set-key (kbd "C-x C-o ga") (ffip-create-pattern-file-finder "*.gradle"))
;;(global-set-key (kbd "C-x C-o sc") (ffip-create-pattern-file-finder "*.scala"))
;;(global-set-key (kbd "C-x C-o co") (ffip-create-pattern-file-finder "*.conf"))
;;(global-set-key (kbd "C-x C-o !") (ffip-create-pattern-file-finder "*"))
;;
;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)



;; 回车缩进
;;(global-set-key "\C-m" 'reindent-then-newline-and-indent)
(global-set-key (kbd "C-<return>") 'reindent-then-newline-and-indent)
(global-set-key (kbd "C-S-i") 'indent-region)

;; Config the mark
(global-set-key (kbd "S-SPC") 'set-mark-command)

;; move text up or down
;;(global-set-key [S-C-up] 'move-text-up)
;;(global-set-key [S-C-down] 'move-text-down)

;; add multi cursors:
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; config for smart forward
(global-set-key (kbd "M-<up>") 'smart-up)
(global-set-key (kbd "M-<down>") 'smart-down)
(global-set-key (kbd "M-<left>") 'smart-backward)
(global-set-key (kbd "M-<right>") 'smart-forward)

;; Killing text
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)

;; M-i for back-to-indentation
(global-set-key (kbd "M-i") 'back-to-indentation)



;;key bindings for org
;;(global-set-key (kbd "<f12>") 'org-agenda) ;; configured blew
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "C-c c") 'org-capture)

;; add ~/notes/front-end-dev-plan.org into agenda
;; (setq org-agenda-files (list "~/notes/front-end-dev-plan.org"))
(global-set-key "\C-c a" 'org-agenda)


;; enlarge current window
(global-set-key (kbd "M-[") 'enlarge-window-horizontally)
(global-set-key (kbd "M-]") 'shrink-window-horizontally)

;; Using dired-view for allowing select file in Dired-Mode by file's fist name.
;;(define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-toggle)
(define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-dired-toggle)

;; Jump to current buffer's directory it is in.
(define-key global-map (kbd "C-x C-j") 'dired-jump)
(define-key global-map (kbd "C-x 4 C-j") 'dired-jump-other-window)
(define-key global-map (kbd "C-x C-h") 'dired-omit-mode)


(global-unset-key (kbd "C-SPC")) ;; close C-SPC for mark, using for toggle fcitx

;; helm-project
(global-set-key (kbd "C-c h") 'helm-projectile)

;; start tabbar-mode
(global-set-key (kbd "C-c t") 'tabbar-mode)
;; Tabbar, following is default key binding, no need to set again
;;(global-set-key (kbd "C-c <C-left>") 'tabbar-forward-tab)
;;(global-set-key (kbd "C-c <C-right>") 'tabbar-backward-tab)
;;(global-set-key (kbd "C-c <C-up>") 'tabbar-forward-group)
;;(global-set-key (kbd "C-c <C-down>") 'tabbar-backward-group)



;; config for sunrise commander
;;(global-set-key (kbd "C-c x") 'sunrise)
;;(global-set-key (kbd "C-c X") 'sunrise-cd)



(provide 'key-bindings)
