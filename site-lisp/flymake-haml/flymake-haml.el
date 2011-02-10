;;; A flymake handler for haml-mode and sass-mode files
;;;
;;; Author: Steve Purcell
;;; Homepage: http://github.com/purcell/emacs.d/blob/master/site-lisp/flymake-haml/flymake-haml.el
;;;
;;; Usage:
;;;   (require 'flymake-haml)
;;;   (add-hook 'haml-mode-hook 'flymake-haml-load)
;;;   (add-hook 'sass-mode-hook 'flymake-sass-load)


(defvar flymake-haml-err-line-patterns '(("^Syntax error on line \\([0-9]+\\): \\(.*\\)$" nil 1 nil 2)))
(defvar flymake-haml-allowed-file-name-masks '((".+\\.\\(haml\\)$" flymake-haml-init)
                                               (".+\\.\\(sass\\)$" flymake-sass-init)))

;; Not provided by flymake itself, curiously
(defun flymake-haml-create-temp-in-system-tempdir (filename prefix)
  (make-temp-file (or prefix "flymake-haml")))

;; Invoke utilities with '-c' to get syntax checking
(defun flymake-haml-init ()
  (list "haml" (list "-c" (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-in-system-tempdir))))
(defun flymake-sass-init ()
  (list "sass" (list "-c" (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-in-system-tempdir))))

(defun flymake-haml-load ()
  (interactive)
  (set (make-local-variable 'flymake-allowed-file-name-masks) flymake-haml-allowed-file-name-masks)
  (set (make-local-variable 'flymake-err-line-patterns) flymake-haml-err-line-patterns)
  (when (executable-find "haml")
    (flymake-mode t)))

(defalias 'flymake-sass-load 'flymake-haml-load)


(provide 'flymake-haml)
