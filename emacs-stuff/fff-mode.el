;; fff-mode.el --- FFF's personal minor mode
;; TODOs:
;; TODO: change <f9> to <space> and make <space> my leader key in evil mode
;; TODO: see list in personal notes
;; TODO: make <leader> r a shortcut to universal search and replace
;;
;; Code:
;;
;; (package-initialize)

(setq package-list
      '(rainbow-mode windsize evil-numbers evil-visualstar
                     evil-surround evil zop-to-char zenburn-theme
                     which-key volatile-highlights use-package
                     undo-tree super-save smartrep smartparens
                     restart-emacs real-auto-save projectile
                     operate-on-number nlinum move-text magit
                     lorem-ipsum key-chord jump-char imenu-anywhere
                     hungry-delete company hl-todo haskell-mode guru-mode
                     gitignore-mode gitconfig-mode git-timemachine
                     gist flycheck expand-region exec-path-from-shell
                     editorconfig easy-kill discover-my-major diminish
                     diff-hl cython-mode crux counsel company-anaconda
                     centered-cursor-mode browse-kill-ring anzu ag
                     ace-window evil-collection terminal-here
                     simple-modeline emojify))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq inferior-lisp-program "/usr/bin/clisp")

;; (add-hook 'prog-mode-hook 'evil-mode)
(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load 'company-mode
  (define-key company-mode-map (kbd "<tab>") 'company-complete)
  )

(with-eval-after-load 'cc-mode
  (define-key company-mode-map (kbd "<tab>") 'company-complete)
  )

(add-to-list 'auto-mode-alist '("\\.hx\\'" . haxe-mode))

(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c c") 'fff-run-haskell))

(eval-after-load "cc-mode"
  '(define-key c-mode-map (kbd "C-c c") 'fff-run-c))

(eval-after-load "haxe-mode"
  '(define-key c-mode-map (kbd "C-c c") 'fff-run-haxe))

(eval-after-load "lua-mode"
  '(define-key c-mode-map (kbd "C-c c") 'fff-run-lua))

;; get help on this:

(defun fff-run-haxe ()
  (interactive)
  (setq dir-path (file-name-directory (buffer-file-name)))
  (shell-command (format "haxe --cwd \"%s\" --run Main" dir-path))
  )

(defun fff-run-lua ()
  (interactive)
  (shell-command (format "lua %s" buffer-file-name))
  )

(defun fff-run-haskell ()
  (interactive)
  (shell-command (format "runhaskell %s" buffer-file-name))
  )

(defun fff-run-java ()
  (interactive)
  (setq dir-path (file-name-directory (buffer-file-name)))
  (setq file-no-ext (substring (buffer-name) 0 -5))
  (shell-command (format "cd %s && javac %s && java %s" dir-path (buffer-name) file-no-ext))
  )

(defun fff-run-c ()
  (interactive)
  (setq dir-path (file-name-directory (buffer-file-name)))
  (setq file-no-ext (substring (buffer-name) 0 -2))
  (shell-command (format "cd %s && cc -w -o %s %s -lm && ./%s" dir-path file-no-ext (buffer-name) file-no-ext))
  )

(defun fff-set-daemon-stuff ()
  (interactive)
  (define-key input-decode-map [?\C-m] [C-m])
  (define-key input-decode-map [?\C-i] [C-i])
  )

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (fff-set-daemon-stuff)))))

(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\C-m] [C-m])

(custom-set-faces
 '(default ((t (:family "Liberation Mono" :foundry "1ASC" :slant normal :weight normal :height 139 :width normal)))))

(require 'windsize)
(require 'cl-lib)
(require 'restart-emacs)
(set-default 'truncate-lines t)
(show-paren-mode 1)
(setq isearch-wrap-function '(lambda nil))
(setq python-shell-interpreter "/usr/bin/python3")
(delete-selection-mode 1)
(setq ivy-initial-inputs-alist nil)
(define-key isearch-mode-map "\C-v" 'isearch-yank-kill)
(define-key isearch-mode-map "\M-y" 'isearch-yank-pop)
(setq-default cursor-type 'bar)
(set-cursor-color "red")
(setq-default cursor-in-non-selected-windows 'hbar)
(setq auto-save-no-message t)
(setq ccm-ignored-commands
      '(mouse-drag-region mouse-set-region mouse-set-point widget-button-click scroll-bar-toolkit-scroll evil-mouse-drag-region recenter-top-bottom))
(setq python-indent-offset 4)
(setq company-idle-delay nil)
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
(setq initial-scratch-message "")
(setq super-save-remote-files nil)


(counsel-mode +1)

;; stop active processes exist
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

(add-hook 'haskell-mode-hook
          (lambda () (progn
                       (local-set-key (kbd "C-c C-c") #'haskell-process-load-file)
                       (local-set-key (kbd "C-c C-p") #'haskell-interactive-switch)
                       (haskell-indent-mode -1)
                       )
            ))

;; (add-hook 'minibuffer-setup-hook
;;           (lambda () (local-set-key (kbd "<C-tab>") #'consult-buffer)))

(add-hook 'minibuffer-setup-hook
          (lambda () (local-set-key (kbd "C-f") #'ivy-next-line)))

(add-hook 'html-mode-hook
          (lambda () (progn
                       (local-set-key (kbd "<tab>") #'emmet-expand-line)
                       (local-set-key (kbd "<C-9>") #'emmet-wrap-with-markup)
                       )))

(define-key key-translation-map (kbd "<f9> c")  (kbd "C-c"))
(define-key key-translation-map (kbd "<f9> x <f9> b")  (kbd "C-x C-b"))
;; (define-key key-translation-map (kbd "<f9> n") (kbd "M--"))
(global-set-key (kbd "<f9> x") ctl-x-map)

;; (defun fff-cheat-sheet ()
;;   (interactive)
;;   (find-file "~/.emacs.d-fff-settings/prelude-cheat-sheet.md"))

(defun fff-access-config ()
  (interactive)
  (find-file "/home/ff/.config/emacs/emacs-stuff/fff-mode.el"))


(defun fff-access-hosts ()
  (interactive)
  (find-file "/etc/hosts"))

(defun fff-access-home-dir()
  (interactive)
  (find-file "~/"))

(defun fff-access-sched ()
  (interactive)
  (find-file "~/personal-notes.md"))

;; (defun fff-select-line ()
;;   (interactive)
;;   (if (region-active-p)
;;       (progn
;;         (forward-line 1)
;;         (end-of-line))
;;     (progn
;;       (end-of-line)
;;       (set-mark (line-beginning-position)))))

;; don't ask which buffer to kill, just kill current buffer
(defun fff-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;; delete word here

(defun fff-kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of thing."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun fff-kill-word-at-point ()
  "Kill the word at point."
  (interactive)
  (progn
    (fff-kill-thing-at-point 'word)
    (setq kill-ring (cdr kill-ring))))

;; (defun fff-clear-line ()
;;   "Clear line, (do not delete or cut)"
;;   (interactive)
;;   (progn
;;     (beginning-of-line)
;;     (kill-line)
;;     (setq kill-ring (cdr kill-ring))))

;; (defun fff-select-block ()
;;   (interactive)
;;   (if (region-active-p)
;;       (re-search-forward "\n[ \t]*\n" nil "move")
;;     (progn
;;       (skip-chars-forward " \n\t")
;;       (when (re-search-backward "\n[ \t]*\n" nil "move")
;;         (re-search-forward "\n[ \t]*\n"))
;;       (push-mark (point) t t)
;;       (re-search-forward "\n[ \t]*\n" nil "move"))))

(add-hook 'html-mode-hook (lambda () (company-mode -1)))

(defun fff-switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun fff-copy-file-path ()
  "Put the current file path on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(setq super-save-remote-files nil)
(setq tooltip-hide-delay 999)

(eval-after-load "evil"
  '(progn
     (define-key evil-insert-state-map (kbd "C-k") 'er/expand-region)
     (define-key evil-insert-state-map (kbd "C-S-k") 'er/contract-region)
     (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
     (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
     (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
     (define-key evil-insert-state-map (kbd "C-'") 'forward-char)
     (define-key evil-insert-state-map (kbd "C-;") 'backward-char)
     (define-key evil-insert-state-map (kbd "M-'") 'forward-word)
     (define-key evil-insert-state-map (kbd "M-;") 'backward-word)
     (define-key evil-insert-state-map (kbd "C-w") 'kill-region)
     (define-key evil-insert-state-map (kbd "M-w") 'easy-kill)
     (define-key evil-insert-state-map (kbd "C-y") 'yank)
     (define-key evil-insert-state-map (kbd "M-y") 'yank-pop)
     (define-key evil-insert-state-map (kbd "C-n") 'next-line)
     (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
     (define-key evil-insert-state-map (kbd "M-/") 'hippie-expand)

     (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)
     (define-key evil-normal-state-map (kbd "C-k") 'er/expand-region)
     (define-key evil-normal-state-map (kbd "C-S-k") 'er/contract-region)
     (define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
     (define-key evil-normal-state-map (kbd "C-a") 'beginning-of-line)
     (define-key evil-normal-state-map (kbd "C-<backspace>") 'fff-kill-word-at-point)
     (define-key evil-normal-state-map (kbd "C-d") 'mc/mark-all-dwim)
     (define-key evil-normal-state-map (kbd "-") 'text-scale-adjust)
     (define-key evil-normal-state-map (kbd "=") 'text-scale-adjust)
     (define-key evil-normal-state-map (kbd "q") 'fff-kill-this-buffer)
     ;; (define-key evil-normal-state-map (kbd "[") 'evil-backward-paragraph)
     ;; (define-key evil-normal-state-map (kbd "]") 'evil-forward-paragraph)

     (define-key evil-visual-state-map (kbd "<backspace>") 'delete-char)
     (define-key evil-visual-state-map (kbd "C-d") 'mc/mark-all-dwim)
     (define-key evil-visual-state-map (kbd "C-e") 'end-of-line)
     (define-key evil-visual-state-map (kbd "C-a") 'beginning-of-line)
     ))

;; sometimes I hit this by accident so let's disable it:
(global-unset-key (kbd "C-x C-c"))

(defvar fff-mode-map
  (let ((map (make-sparse-keymap)))

    ;; (define-key map (kbd "C-=") 'mc/mark-next-like-this)
    (define-key map (kbd "C-c p") 'projectile-command-map)
    (define-key map (kbd "C-c s") 'shell)

    ;; (define-key map (kbd "C-\\") 'Control-X-prefix)
    (define-key map (kbd "C-x k") 'fff-kill-this-buffer)
    (define-key map (kbd "C-/") 'comment-line)
    (define-key map (kbd "C-<right>") 'forward-char)
    (define-key map (kbd "C-<left>") 'backward-char)
    (define-key map (kbd "<right>") 'forward-char)
    (define-key map (kbd "<down>") 'next-line)
    (define-key map (kbd "<up>") 'previous-line)
    (define-key map (kbd "C-\\") 'other-window)
    ;; (define-key map (kbd "<C-m>") 'other-window)
    (define-key map (kbd "M-l") 'windsize-right)
    (define-key map (kbd "M-h") 'windsize-left)
    (define-key map (kbd "M-k") 'windsize-up)
    (define-key map (kbd "M-j") 'windsize-down)
    ;; (define-key map (kbd "<tab>") 'company-complete)

    (define-key map (kbd "<f9> t") 'terminal-here-launch)
    (define-key map (kbd "<f9> RET") 'terminal-here-launch)

    (define-key map (kbd "<f9> p") 'crux-open-with)
    (define-key map (kbd "<f9> h") 'mark-whole-buffer)
    (define-key map (kbd "<f9> <tab>") 'ivy-switch-buffer)
    (define-key map (kbd "<f9> q") 'kill-buffer-and-window)
    (define-key map (kbd "<f9> f q") 'fff-force-kill-this-buffer)


    (define-key map (kbd "<f9> <f9>") 'execute-extended-command)
    (define-key map (kbd "<f9> z") 'shell-command)
    (define-key map (kbd "<f9> e e") 'eval-last-sexp)
    (define-key map (kbd "<f9> e r") 'eval-region)
    (define-key map (kbd "<f9> e b") 'eval-buffer)
    ;; (define-key map (kbd "<f9> e p") 'crux-eval-and-replace)
    (define-key map (kbd "<f9> j" ) 'avy-goto-word-1)
    ;; (define-key map (kbd "<f9> f" ) 'jump-char-forward)
    (define-key map (kbd "<f9> b") 'comint-clear-buffer)
    ;; (define-key map (kbd "<f9> r r") 'anzu-query-replace-regexp)
    ;; (define-key map (kbd "<f9> r s") 'anzu-query-replace)
    (define-key map (kbd "<f9> s") 'save-buffer)
    (define-key map (kbd "<f9> o") 'counsel-find-file)
    (define-key map (kbd "<f9> 7") 'fff-access-config)
    (define-key map (kbd "<f9> 8") 'fff-access-hosts)
    (define-key map (kbd "<f9> g") 'keyboard-quit)
    (define-key map (kbd "<f9> <escape>") 'keyboard-escape-quit)
    ;; (define-key map (kbd "<f9> m s") 'magit-status)
    (define-key map (kbd "<f9> d b") 'delete-blank-lines)
    (define-key map (kbd "<f9> d f") 'fixup-whitespace)
    ;; (define-key map (kbd "<f9> d d") 'fff-clear-line)
    ;; (define-key map (kbd "<f9> d w") 'fff-kill-word-at-point)
    ;; (define-key map (kbd "<f9> 9") 'kmacro-start-macro)
    ;; (define-key map (kbd "<f9> 0") 'kmacro-end-macro)
    ;; (define-key map (kbd "<f9> =") 'kmacro-end-and-call-macro)
    (define-key map (kbd "<f9> i") 'fff-switch-to-scratch-buffer)
    (define-key map (kbd "<f9> u") 'fff-access-home-dir)
    ;; (define-key map (kbd "<f9> l s p d") 'lsp-ui-doc-glance)
    ;; (define-key map (kbd "<f9> w") 'swiper)
    (define-key map (kbd "<f9> y") 'fff-access-sched)
    (define-key map (kbd "<f9> x k") 'fff-kill-this-buffer)
    ;; (define-key map (kbd "<f9> k") 'fff-kill-this-buffer)



    (easy-menu-define fff-mode-menu map
      "FFF's menu."
      '("FFF"
        ["eval-last-sexp" eval-last-sexp]
        )
      )
    map)
  "FFF Key map.")

(define-minor-mode fff-mode
  "FF's personal minor mode.
\\{fff-mode-map}"
  :lighter " FFF"
  :keymap fff-mode-map
  :global t)

;; TODO: change the value of '<function-name> until I learn how to write a
;; mode specific hook for each language
;; (global-set-key (kbd "C-c c") 'fff-run-c)

;; (eval-after-load "haskell-mode"
;;   '(define-key haskell-mode-map (kbd "C-c c") 'haskell-compile))


(global-set-key (kbd "C-x w") 'write-file)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x s") 'save-buffer)

;; (global-set-key (kbd "<C-tab>") 'consult-buffer)
;; (add-hook 'minibuffer-setup-hook
;;           (lambda () (local-set-key (kbd "<C-tab>") #'consult-buffer)))


(setq ivy-on-del-error-function #'ignore)

(set-face-attribute 'region nil :background "#9c9c9c" :foreground "#ffffff")

(define-fringe-bitmap 'flycheck-fringe-bitmap-ball
  (vector #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00111000
          #b01111100
          #b11111110
          #b11111110
          #b01111100
          #b00111000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b00000000))

(flycheck-define-error-level 'error
  :severity 100
  :compilation-level 2
  :overlay-category 'flycheck-error-overlay
  :fringe-bitmap 'flycheck-fringe-bitmap-ball
  :fringe-face 'flycheck-fringe-error
  :error-list-face 'flycheck-error-list-error)

;; (evil-mode +1)
;; (evil-collection-init)
;; (setq evil-collection-company-use-tng nil)

(setq terminal-here-linux-terminal-command 'st)

(require 'terminal-here)

(scroll-bar-mode +1)
(global-hl-todo-mode +1)

(require 'simple-modeline)
(simple-modeline-mode)

(defun fff-space-makes-space ()
  "Reset the Keyboard Translation Table to nil, the space bar acts normally and sends a space character."
  (interactive)
  (setq keyboard-translate-table nil) )

(defun fff-space-makes-dash ()
  "(Re)Create the Keyboard Translation Table so that the space bar sends a dash character."
  (interactive)
  (setq keyboard-translate-table (vconcat (make-vector 32 nil) [?-])) )

(defun fff-space-makes-underscore ()
  "(Re)Create the Keyboard Translation Table so that the space bar sends an underscore character."
  (interactive)
  (setq keyboard-translate-table (vconcat (make-vector 32 nil) [?_])) )

;; (use-package evil-surround
;;   :ensure t
;;   :config
;;   (global-evil-surround-mode 1))

;; shell to open in new window
(setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-pop-up-window)))

;; (add-to-list 'backup-directory-alist
;;              (cons tramp-file-name-regexp nil))

;; get help on this:
;; (use-package lsp-python-ms
;;   :ensure t
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-python-ms)
;;                          (lsp))))  ; or lsp-deferred

(defun fff-open-this-file-in-firefox()
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url-firefox (concat "file://" filename))))

(defun fff-force-kill-this-buffer ()
  "Kill the current buffer - even if modified."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer)))

(global-flycheck-mode -1)
(flycheck-mode -1)

(require 'evil)
(evil-mode 1)
(evil-collection-init)

(provide 'fff-mode)

;;; fff-mode.el ends here
