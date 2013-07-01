(require-package 'smartparens)
(setq sp-base-key-bindings 'paredit)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

(require 'smartparens-config)

(defun sanityinc/smartparens-lisp-setup ()
  (local-set-key [remap delete-char] 'sp-delete-char)
  (local-set-key (kbd "<backspace>") 'sp-backward-delete-char))

;; Compatibility with other modes

(suspend-mode-during-cua-rect-selection 'smartparens-mode)

;; TODO: kill sexp
;; TODO: wrap round

(define-key sp-keymap [remap kill-sexp] 'sp-kill-sexp)

;; Use smartparens in the minibuffer too

(setq sp-ignore-modes-list
      (delete 'minibuffer-inactive-mode sp-ignore-modes-list))

;; TODO: break out into separate package
;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-lisp)

(defvar lispy-minibuffer-commands '(eval-expression
                                    pp-eval-expression
                                    eval-expression-with-eldoc
                                    ibuffer-do-eval
                                    ibuffer-do-view-and-eval)
  "Interactive commands in which lisp code is entered.")

(defun conditionally-enable-smartparens-lisp ()
  "Configure smartparens during lisp-related minibuffer commands."
  (when (memq this-command lispy-minibuffer-commands)
    (sanityinc/smartparens-lisp-setup)))

(provide 'init-smartparens)
