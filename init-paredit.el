(require-package 'paredit)
(autoload 'enable-paredit-mode "paredit")

(defun maybe-map-paredit-newline ()
  (unless (or (memq major-mode '(inferior-emacs-lisp-mode nrepl-mode))
              (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

(add-hook 'paredit-mode-hook 'maybe-map-paredit-newline)

(eval-after-load 'paredit
  '(progn
     (diminish 'paredit-mode " Par")
     (dolist (binding (list (kbd "C-<left>") (kbd "C-<right>")
                            (kbd "C-M-<left>") (kbd "C-M-<right>")))
       (define-key paredit-mode-map binding nil))

     ;; Disable kill-sentence, which is easily confused with the kill-sexp
     ;; binding, but doesn't preserve sexp structure
     (define-key paredit-mode-map [remap kill-sentence] nil)
     (define-key paredit-mode-map [remap backward-kill-sentence] nil)))


;; Compatibility with other modes

(suspend-mode-during-cua-rect-selection 'paredit-mode)


;; Use paredit in the minibuffer
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")

(defun conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))

;; ----------------------------------------------------------------------------
;; Enable some handy paredit functions in all prog modes
;; ----------------------------------------------------------------------------

(defvar sanityinc/paredit-mini-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-)") 'paredit-forward-slurp-sexp)
    (define-key m (kbd "C-}") 'paredit-forward-barf-sexp)
    (define-key m (kbd "M-(") 'paredit-wrap-round)
    (define-key m (kbd "M-[") 'paredit-wrap-square)
    (define-key m (kbd "M-{") 'paredit-wrap-curly)
    (define-key m (kbd "M-)") 'paredit-close-round-and-newline)
    (define-key m (kbd "M-]") 'paredit-close-square-and-newline)
    (define-key m (kbd "M-}") 'paredit-close-curly-and-newline)
    m)
  "Keymap for command `sanityinc/paredit-mini-mode'.")

(define-minor-mode sanityinc/paredit-mini-mode
  "A cut-down version of paredit which can be used in non-lisp buffers."
  nil
  " Par-"
  sanityinc/paredit-mini-mode-map
  (require 'paredit))

;; Enable certain paredit keys in all prog modes...
(add-hook 'prog-mode-hook 'sanityinc/paredit-mini-mode)
;; ... but disable when full paredit is enabled
(add-hook 'paredit-mode-hook
          (lambda () (sanityinc/paredit-mini-mode 0)))


(provide 'init-paredit)
