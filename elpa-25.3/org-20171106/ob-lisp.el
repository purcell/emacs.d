;;; ob-lisp.el --- Babel Functions for Common Lisp   -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Authors: Joel Boehland
;;	 Eric Schulte
;;	 David T. O'Toole <dto@gnu.org>
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

;;; Support for evaluating Common Lisp code, relies on SLY or SLIME
;;; for all eval.

;;; Requirements:

;; Requires SLY (Sylvester the Cat's Common Lisp IDE) or SLIME
;; (Superior Lisp Interaction Mode for Emacs).  See:
;; - https://github.com/capitaomorte/sly
;; - http://common-lisp.net/project/slime/

;;; Code:
(require 'ob)

(declare-function sly-eval "ext:sly" (sexp &optional package))
(declare-function slime-eval "ext:slime" (sexp &optional package))
(declare-function org-trim "org" (s &optional keep-lead))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("lisp" . "lisp"))

(defvar org-babel-default-header-args:lisp '())
(defvar org-babel-header-args:lisp '((package . :any)))

(defcustom org-babel-lisp-eval-fn #'slime-eval
  "The function to be called to evaluate code on the Lisp side.
Valid values include `slime-eval' and `sly-eval'."
  :group 'org-babel
  :version "26.1"
  :package-version '(Org . "9.0")
  :type 'function)

(defcustom org-babel-lisp-dir-fmt
  "(let ((*default-pathname-defaults* #P%S\n)) %%s\n)"
  "Format string used to wrap code bodies to set the current directory.
For example a value of \"(progn ;; %s\\n   %%s)\" would ignore the
current directory string."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defun org-babel-expand-body:lisp (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((vars (org-babel--get-vars params))
	 (result-params (cdr (assq :result-params params)))
	 (print-level nil) (print-length nil)
	 (body (if (null vars) (org-trim body)
		 (concat "(let ("
			 (mapconcat
			  (lambda (var)
			    (format "(%S (quote %S))" (car var) (cdr var)))
			  vars "\n      ")
			 ")\n" body ")"))))
    (if (or (member "code" result-params)
	    (member "pp" result-params))
	(format "(pprint %s)" body)
      body)))

(defun org-babel-execute:lisp (body params)
  "Execute a block of Common Lisp code with Babel.
BODY is the contents of the block, as a string.  PARAMS is
a property list containing the parameters of the block."
  (require (pcase org-babel-lisp-eval-fn
	     (`slime-eval 'slime)
	     (`sly-eval 'sly)))
  (org-babel-reassemble-table
   (let ((result
          (funcall (if (member "output" (cdr (assq :result-params params)))
                       #'car #'cadr)
                   (with-temp-buffer
                     (insert (org-babel-expand-body:lisp body params))
                     (funcall org-babel-lisp-eval-fn
                              `(swank:eval-and-grab-output
                                ,(let ((dir (if (assq :dir params)
                                                (cdr (assq :dir params))
                                              default-directory)))
                                   (format
                                    (if dir (format org-babel-lisp-dir-fmt dir)
                                      "(progn %s\n)")
                                    (buffer-substring-no-properties
                                     (point-min) (point-max)))))
                              (cdr (assq :package params)))))))
     (org-babel-result-cond (cdr (assq :result-params params))
       result
       (condition-case nil
           (read (org-babel-lisp-vector-to-list result))
         (error result))))
   (org-babel-pick-name (cdr (assq :colname-names params))
			(cdr (assq :colnames params)))
   (org-babel-pick-name (cdr (assq :rowname-names params))
			(cdr (assq :rownames params)))))

(defun org-babel-lisp-vector-to-list (results)
  ;; TODO: better would be to replace #(...) with [...]
  (replace-regexp-in-string "#(" "(" results))

(provide 'ob-lisp)



;;; ob-lisp.el ends here
