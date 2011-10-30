(require 'pretty-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-pretty-mode)

(autoload 'enable-paredit-mode "paredit")
(defadvice enable-paredit-mode (before disable-autopair activate)
  (setq autopair-dont-activate t)
  (autopair-mode -1))

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'enable-paredit-mode))


(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)
(defun conditionally-enable-paredit-mode ()
  "Enable paredit-mode during eval-expression"
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(suspend-mode-during-cua-rect-selection 'paredit-mode)


(defun set-up-hippie-expand-for-elisp ()
  (make-variable-buffer-local 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))


;; ----------------------------------------------------------------------------
;; Auto-complete tweaks for emacs-lisp mode
;; ----------------------------------------------------------------------------

(defface ac-symbol-menu-face
  '((t (:background "lightgray" :foreground "darkgreen")))
  "Face for slime candidate menu."
  :group 'auto-complete)

(defface ac-symbol-selection-face
  '((t (:background "darkgreen" :foreground "white")))
  "Face for the slime selected candidate."
  :group 'auto-complete)

;; Modify ac-source-symbols to add colours
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-source-symbols '(candidate-face . ac-symbol-menu-face))
     (add-to-list 'ac-source-symbols '(selection-face . ac-symbol-selection-face))))

(defun set-up-ac-for-elisp ()
  (ac-emacs-lisp-mode-setup))

(defun maybe-map-paredit-newline ()
  (unless (or (eq major-mode 'inferior-emacs-lisp-mode) (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

(add-hook 'paredit-mode-hook 'maybe-map-paredit-newline)

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'set-up-hippie-expand-for-elisp)
  (add-hook hook 'set-up-ac-for-elisp)
  (add-hook hook 'turn-on-eldoc-mode))

(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(add-to-list 'auto-mode-alist '("\\.emacs-project$" . emacs-lisp-mode))

(defun warn-disabled-command ()
  (interactive)
  (message "Command disabled")
  (ding))

(eval-after-load "paredit"
  '(progn
     ;; These are handy everywhere, not just in lisp modes
     (global-set-key (kbd "M-(") 'paredit-wrap-round)
     (global-set-key (kbd "M-[") 'paredit-wrap-square)
     (global-set-key (kbd "M-{") 'paredit-wrap-curly)

     (global-set-key (kbd "M-)") 'paredit-close-round-and-newline)
     (global-set-key (kbd "M-]") 'paredit-close-square-and-newline)
     (global-set-key (kbd "M-}") 'paredit-close-curly-and-newline)

     (global-set-key (kbd "C-<right>") 'paredit-forward-slurp-sexp)
     (global-set-key (kbd "C-<left>") 'paredit-forward-barf-sexp)
     (global-set-key (kbd "C-M-<left>") 'paredit-backward-slurp-sexp)
     (global-set-key (kbd "C-M-<right>") 'paredit-backward-barf-sexp)

     ;; Disable kill-sentence, which is easily confused with the kill-sexp
     ;; binding, but doesn't preserve sexp structure
     (define-key paredit-mode-map (kbd "M-K") 'warn-disabled-command)
     (define-key paredit-mode-map (kbd "M-k") 'warn-disabled-command)))

;; When editing lisp code, highlight the current sexp
(add-hook 'paredit-mode-hook (lambda () (hl-sexp-mode t)))

;; Prevent flickery behaviour due to hl-sexp-mode unhighlighting before each command
(eval-after-load "hl-sexp"
  '(defadvice hl-sexp-mode (after unflicker (turn-on) activate)
     (when turn-on
       (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))


(require 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))


(provide 'init-lisp)
