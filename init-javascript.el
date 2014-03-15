;; looks nodejs is more popular, if you prefer rhino, change to "js"
(setq inferior-js-program-command "node --interactive")

(require 'js-comint)
;; if use node.js, we need nice output
(setenv "NODE_NO_READLINE" "1")
;; (setq inferior-js-mode-hook
;;       (lambda ()
;;         ;; We like nice colors
;;         (ansi-color-for-comint-mode-on)
;;         ;; Deal with some prompt nonsense
;;         (add-to-list
;;          'comint-preoutput-filter-functions
;;          (lambda (output)
;;            (replace-regexp-in-string "\033\\[[0-9A-Z][0-9A-Z]" "" output)))))

(defun add-inferior-js-keys ()
  (moz-minor-mode 1)
  (local-set-key "\C-x\C-e" 'js-send-last-sexp)
  (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
  (local-set-key "\C-cb" 'js-send-buffer)
  (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
  (local-set-key "\C-cl" 'js-load-file-and-go))

;; may be in an arbitrary order
(eval-when-compile (require 'cl))

;; json
(setq auto-mode-alist (cons '("\\.json$" . json-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jason$" . json-mode) auto-mode-alist))

;; {{ js2-mode or js-mode
(if (and (>= emacs-major-version 24) (>= emacs-minor-version 1))
    (progn
      (setq auto-mode-alist (cons '("\\.js\\(\\.erb\\)?\\'" . js2-mode) auto-mode-alist))
      (autoload 'js2-mode "js2-mode" nil t)
      (add-hook 'js2-mode-hook '(lambda ()
                                  (js2-imenu-extras-mode)
                                  (setq mode-name "JS2")
                                  (require 'requirejs-mode)
                                  (requirejs-mode)
                                  (require 'js-doc)
                                  (define-key js2-mode-map "\C-cd" 'js-doc-insert-function-doc)
                                  (define-key js2-mode-map "@" 'js-doc-insert-tag)
                                  (add-inferior-js-keys)
                                  ))

      (setq js2-use-font-lock-faces t
            js2-mode-must-byte-compile nil
            js2-indent-on-enter-key t
            js2-skip-preprocessor-directives t
            js2-auto-indent-p t
            js2-bounce-indent-p t)

      (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))

      (eval-after-load 'coffee-mode
        `(setq coffee-js-mode 'js2-mode
               coffee-tab-width 4))
      )
  (progn
    ;; js-mode
    (setq auto-mode-alist (cons '("\\.js\\(\\.erb\\)?\\'" . js-mode) auto-mode-alist))
    ;; Need to first remove from list if present, since elpa adds entries too
    (add-hook 'js-mode-hook 'add-inferior-js-keys)

    )
  )
;; }}

(add-hook 'coffee-mode-hook 'flymake-coffee-load)

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
