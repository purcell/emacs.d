;;; ipretty.el --- Interactive Emacs Lisp pretty-printing

;; Copyright (C) 2014, Steckerhalter

;; Author: steckerhalter
;; URL: https://github.com/steckerhalter/ipretty
;; Package-Version: 20140406.2220
;; Keywords: pretty-print elisp buffer
;; License: GPLv3, see https://github.com/ipretty/LICENSE

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `ipretty.el' provides interactive functions to pretty-print the
;; result of an expression and a global mode `ipretty-mode' that
;; advices `eval-print-last-sexp' to pretty print.

;;; Code:

(require 'pp)

;;;###autoload
(defun ipretty-last-sexp (&optional truncate)
  "Pretty-print the last sexp into the current buffer.
When TRUNCATE is non-nil or with a prefix argument, long output
is truncated. See the documentation of `eval-print-last-sexp' for
more information on what affects truncation."
  (interactive "P")
  (let ((standard-output (current-buffer)))
    (terpri)
    (eval-last-sexp (if truncate t 0)))
  (backward-sexp 1)
  (indent-pp-sexp t))

;;;###autoload
(defun ipretty-last-sexp-other-buffer (&optional buffer-name)
  "Display the last sexp pretty-printed in other buffer.
If BUFFER-NAME (a string)is provided it will be used to name the
buffer, otherwise the default `*pp-display-expression*' is used."
  (interactive)
  (pp-display-expression
   (eval (preceding-sexp)) (or buffer-name "*pp-display-expression*")))

(defadvice eval-print-last-sexp (after eval-print-last-sexp-after-advice)
  "Advice `eval-print-last-sexp' to pretty-print the result."
    (backward-sexp 1)
    (indent-pp-sexp t))

;;;###autoload
(define-minor-mode ipretty-mode
  "Toggle ipretty mode globally.
   With no argument, this command toggles the mode.
   Non-null prefix argument turns on the mode.
   Null prefix argument turns off the mode."
  :global t

  (if ipretty-mode
      (ad-activate-regexp "eval-print-last-sexp-after-advice")
    (ad-deactivate-regexp "eval-print-last-sexp-after-advice")))

(provide 'ipretty)
;;; ipretty.el ends here
