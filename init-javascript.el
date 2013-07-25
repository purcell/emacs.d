(defvar preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(autoload 'js2-mode "js2-mode" nil t)
(setq auto-mode-alist (cons '("\\.js\\(\\.erb\\|on\\)?\\'" . js2-mode) auto-mode-alist))

;; json
(setq auto-mode-alist (cons '("\\.json$" . json-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jason$" . json-mode) auto-mode-alist))

;; js2-mode
(add-hook 'js2-mode-hook '(lambda ()
                            (setq mode-name "JS2")
                            (require 'requirejs-mode)
                            (requirejs-mode)
                            (require 'js-doc)
                            (define-key js2-mode-map "\C-cd" 'js-doc-insert-function-doc)
                            (define-key js2-mode-map "@" 'js-doc-insert-tag)
                            ))

(setq js2-use-font-lock-faces t
      js2-mode-must-byte-compile nil
      js2-basic-offset preferred-javascript-indent-level
      js2-indent-on-enter-key t
      js2-auto-indent-p t
      js2-bounce-indent-p t)

(eval-after-load 'js2-mode
  '(progn
     (require 'js2-imenu-extras)
     (js2-imenu-extras-setup)))

;; js-mode
(setq js-indent-level preferred-javascript-indent-level)


;; standard javascript-mode
(setq javascript-indent-level preferred-javascript-indent-level)

(add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))


(eval-after-load 'coffee-mode
  `(setq coffee-js-mode 'js2-mode
         coffee-tab-width preferred-javascript-indent-level))

(add-hook 'coffee-mode-hook 'flymake-coffee-load)


;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; ---------------------------------------------------------------------------

(setq inferior-js-program-command "js")
(defun add-inferior-js-keys ()
  (local-set-key "\C-x\C-e" 'js-send-last-sexp)
  (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
  (local-set-key "\C-cb" 'js-send-buffer)
  (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
  (local-set-key "\C-cl" 'js-load-file-and-go))

(dolist (hook '(js2-mode-hook js-mode-hook))
  (add-hook hook 'add-inferior-js-keys))


(provide 'init-javascript)
