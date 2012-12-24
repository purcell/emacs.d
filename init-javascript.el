(defcustom preferred-javascript-mode 'js2-mode
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js3-mode js-mode))
(defvar preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(setq auto-mode-alist (cons `("\\.js\\(\\.erb\\|on\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))


;; On-the-fly syntax checking
(eval-after-load 'js
  '(add-hook 'js-mode-hook 'flymake-jslint-load))


;; js2-mode
(add-hook 'js2-mode-hook '(lambda () (setq mode-name "JS2")))
(setq js2-use-font-lock-faces t
      js2-mode-must-byte-compile nil
      js2-basic-offset preferred-javascript-indent-level
      js2-indent-on-enter-key t
      js2-auto-indent-p t
      js2-bounce-indent-p t)

;; js3-mode
(add-hook 'js3-mode-hook '(lambda () (setq mode-name "JS3")))
(setq js3-auto-indent-p t
      js3-enter-indents-newline t
      js3-indent-on-enter-key t
      js3-indent-level preferred-javascript-indent-level)

;; js-mode
(setq js-indent-level preferred-javascript-indent-level)


;; standard javascript-mode
(setq javascript-indent-level preferred-javascript-indent-level)

(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))


(eval-after-load 'coffee-mode
  `(setq coffee-js-mode preferred-javascript-mode
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

(dolist (hook '(js2-mode-hook js3-mode-hook js-mode-hook))
  (add-hook hook 'add-inferior-js-keys))


(provide 'init-javascript)
