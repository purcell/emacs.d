;;; ob-emacs-lisp.el --- Babel Functions for Emacs-lisp Code -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating emacs-lisp code

;;; Code:
(require 'ob)

(defconst org-babel-header-args:emacs-lisp '((lexical . :any))
  "Emacs-lisp specific header arguments.")

(defvar org-babel-default-header-args:emacs-lisp '((:lexical . "no"))
  "Default arguments for evaluating an emacs-lisp source block.

A value of \"yes\" or t causes src blocks to be eval'd using
lexical scoping.  It can also be an alist mapping symbols to
their value.  It is used as the optional LEXICAL argument to
`eval', which see.")

(defun org-babel-expand-body:emacs-lisp (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params))
	(print-level nil)
	(print-length nil))
    (if (null vars) (concat body "\n")
      (format "(let (%s)\n%s\n)"
	      (mapconcat
	       (lambda (var)
		 (format "%S" (print `(,(car var) ',(cdr var)))))
	       vars "\n      ")
	      body))))

(defun org-babel-execute:emacs-lisp (body params)
  "Execute a block of emacs-lisp code with Babel."
  (save-window-excursion
    (let* ((lexical (cdr (assq :lexical params)))
	   (result-params (cdr (assq :result-params params)))
	   (body (format (if (member "output" result-params)
			     "(with-output-to-string %s\n)"
			   "(progn %s\n)")
			 (org-babel-expand-body:emacs-lisp body params)))
	   (result (eval (read (if (or (member "code" result-params)
				       (member "pp" result-params))
				   (concat "(pp " body ")")
				 body))
			 (if (listp lexical)
			     lexical
			   (member lexical '("yes" "t"))))))
      (org-babel-result-cond result-params
	(let ((print-level nil)
              (print-length nil))
          (if (or (member "scalar" result-params)
                  (member "verbatim" result-params))
              (format "%S" result)
            (format "%s" result)))
	(org-babel-reassemble-table
	 result
         (org-babel-pick-name (cdr (assq :colname-names params))
                              (cdr (assq :colnames params)))
         (org-babel-pick-name (cdr (assq :rowname-names params))
                              (cdr (assq :rownames params))))))))

(org-babel-make-language-alias "elisp" "emacs-lisp")

(provide 'ob-emacs-lisp)



;;; ob-emacs-lisp.el ends here
