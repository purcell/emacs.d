;; looks nodejs is more popular, if you prefer rhino, change to "js"
(setq inferior-js-program-command "node --interactive")

(require 'js-comint)
;; if use node.js, we need nice output
(setenv "NODE_NO_READLINE" "1")

;; may be in an arbitrary order
(eval-when-compile (require 'cl))

;; json
(setq auto-mode-alist (cons '("\\.json$" . json-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jason$" . json-mode) auto-mode-alist))

;; {{ js2-mode or javascript-mode
(setq js2-use-font-lock-faces t
      js2-mode-must-byte-compile nil
      js2-idle-timer-delay 0.5 ;; could not be too big for real time syntax check
      js2-indent-on-enter-key t
      js2-skip-preprocessor-directives t
      js2-auto-indent-p t
      js2-bounce-indent-p t)

;; js-mode imenu enhancement
;; @see http://stackoverflow.com/questions/20863386/idomenu-not-working-in-javascript-mode
(defun mo-js-imenu-make-index ()
  (save-excursion
    (imenu--generic-function '((nil "function\\s-+\\([^ ]+\\)(" 1)
                               (nil " \\([^ ]+\\)\\s-*=\\s-*function\\s-*(" 1)))))

(defun flymake-jshint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name)))
           (arglist (list local-file)))
      (list "jshint" arglist)))

(defun mo-js-mode-hook ()
  (setq imenu-create-index-function 'mo-js-imenu-make-index)
  (flymake-mode 1)
  (setq flymake-err-line-patterns
        (cons '(".*: line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.*\\)$"
                nil 1 2 3)
              flymake-err-line-patterns))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.js\\'" flymake-jshint-init)
               '("\\.json\\'" flymake-jshint-init))
  (flymake-mode 1)
  )
(add-hook 'js-mode-hook 'mo-js-mode-hook)

(cond
 ((and (>= emacs-major-version 24) (>= emacs-minor-version 1) (not *no-memory*))
  (setq auto-mode-alist (cons '("\\.js\\(\\.erb\\)?\\'" . js2-mode) auto-mode-alist))
  (autoload 'js2-mode "js2-mode" nil t)
  (add-hook 'js2-mode-hook '(lambda ()
                              (js2-imenu-extras-mode)
                              (setq mode-name "JS2")
                              (require 'js-doc)
                              (define-key js2-mode-map "\C-cd" 'js-doc-insert-function-doc)
                              (define-key js2-mode-map "@" 'js-doc-insert-tag)
                              ))
  (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
  )
 (t
  (setq auto-mode-alist (cons '("\\.js\\(\\.erb\\)?\\'" . js-mode) auto-mode-alist))
  ))
;; }}

(if *emacs24* (add-hook 'coffee-mode-hook 'flymake-coffee-load))

;; {{ js-beautify
(defun js-beautify ()
  "Beautify a region of javascript using the code from jsbeautify.org.
sudo pip install jsbeautifier"
  (interactive)
  (let ((orig-point (point)))
    (unless (mark)
      (mark-defun))
    (shell-command-on-region (point)
                             (mark)
                             (concat "js-beautify"
                                     " --stdin "
                                     " --jslint-happy --brace-style=end-expand --keep-array-indentation "
                                     (format " --indent-size=%d " js2-basic-offset))
                             nil t)
    (goto-char orig-point)))
;; }}

;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;; add any symbols to a buffer-local var of acceptable global vars
;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
;; you can;t have a symbol called "someName:false"
(add-hook 'js2-post-parse-callbacks
          (lambda ()
            (when (> (buffer-size) 0)
              (let ((btext (replace-regexp-in-string
                            ": *true" " "
                            (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
                (mapc (apply-partially 'add-to-list 'js2-additional-externs)
                      (split-string
                       (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                       " *, *" t))
                ))))

(provide 'init-javascript)
