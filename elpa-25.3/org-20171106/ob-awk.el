;;; ob-awk.el --- Babel Functions for Awk            -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2017 Free Software Foundation, Inc.

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

;; Babel's awk can use special header argument:
;;
;; - :in-file takes a path to a file of data to be processed by awk
;;
;; - :stdin takes an Org data or code block reference, the value of
;;          which will be passed to the awk process through STDIN

;;; Code:
(require 'ob)
(require 'org-compat)

(declare-function org-babel-ref-resolve "ob-ref" (ref))
(declare-function orgtbl-to-generic "org-table" (table params))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("awk" . "awk"))

(defvar org-babel-awk-command "awk"
  "Name of the awk executable command.")

(defun org-babel-expand-body:awk (body _params)
  "Expand BODY according to PARAMS, return the expanded body."
  body)

(defun org-babel-execute:awk (body params)
  "Execute a block of Awk code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (message "executing Awk source code block")
  (let* ((result-params (cdr (assq :result-params params)))
         (cmd-line (cdr (assq :cmd-line params)))
         (in-file (cdr (assq :in-file params)))
	 (full-body (org-babel-expand-body:awk body params))
	 (code-file (let ((file (org-babel-temp-file "awk-")))
                      (with-temp-file file (insert full-body)) file))
	 (stdin (let ((stdin (cdr (assq :stdin params))))
		   (when stdin
		     (let ((tmp (org-babel-temp-file "awk-stdin-"))
			   (res (org-babel-ref-resolve stdin)))
		       (with-temp-file tmp
			 (insert (org-babel-awk-var-to-awk res)))
		       tmp))))
         (cmd (mapconcat #'identity
			 (append
			  (list org-babel-awk-command
				"-f" code-file cmd-line)
			  (mapcar (lambda (pair)
				    (format "-v %s='%s'"
					    (car pair)
					    (org-babel-awk-var-to-awk
					     (cdr pair))))
				  (org-babel--get-vars params))
			  (list in-file))
			 " ")))
    (org-babel-reassemble-table
     (let ((results
            (cond
             (stdin (with-temp-buffer
                      (call-process-shell-command cmd stdin (current-buffer))
                      (buffer-string)))
             (t (org-babel-eval cmd "")))))
       (when results
         (org-babel-result-cond result-params
	   results
	   (let ((tmp (org-babel-temp-file "awk-results-")))
	     (with-temp-file tmp (insert results))
	     (org-babel-import-elisp-from-file tmp)))))
     (org-babel-pick-name
      (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
     (org-babel-pick-name
      (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))

(defun org-babel-awk-var-to-awk (var &optional sep)
  "Return a printed value of VAR suitable for parsing with awk."
  (let ((echo-var (lambda (v) (if (stringp v) v (format "%S" v)))))
    (cond
     ((and (listp var) (listp (car var)))
      (orgtbl-to-generic var  (list :sep (or sep "\t") :fmt echo-var)))
     ((listp var)
      (mapconcat echo-var var "\n"))
     (t (funcall echo-var var)))))

(provide 'ob-awk)



;;; ob-awk.el ends here
