;; See http://www.emacswiki.org/cgi-bin/wiki/FlymakeJavaScript
;; and http://d.hatena.ne.jp/kazu-yamamoto/mobile?date=20071029
;;
;; Usage: (add-hook 'javascript-mode-hook '(lambda () (flymake-js-load)))

(defconst flymake-allowed-js-file-name-masks '(("\\.json$" flymake-js-init)
                                               ("\\.js$" flymake-js-init)))
(defcustom flymake-js-detect-trailing-comma t nil :type 'boolean)
(defvar flymake-js-err-line-patterns
  '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" nil 2 nil 3)
    ("^\\(.+\\)(\\([0-9]+\\)): \\(SyntaxError:.+\\)$" nil 2 nil 3)
    ("^\\(.+\\)(\\([0-9]+\\)): lint \\(warning:.+\\)$" nil 2 nil 3)))
(when flymake-js-detect-trailing-comma
  (add-to-list 'flymake-js-err-line-patterns
               '("^\\(.+\\)\:\\([0-9]+\\)\: strict \\(warning: trailing comma.+\\)\:$" nil 2 nil 3)
               t))

(defun flymake-js-create-temp-in-system-tempdir (filename prefix)
  (make-temp-file (or prefix "flymake-js")))

(defun flymake-js-init ()
  (list "jsl" (list "-process" (flymake-init-create-temp-buffer-copy
                                'flymake-js-create-temp-in-system-tempdir))))


(defun flymake-js-load ()
  (interactive)
  (require 'flymake)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-js-file-name-masks))
  (setq flymake-err-line-patterns flymake-js-err-line-patterns)
  (if (executable-find "jsl")
      (flymake-mode t)
    (message "Not enabling flymake: jsl command not found")))


(provide 'flymake-js)
