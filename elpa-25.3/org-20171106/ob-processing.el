;;; ob-processing.el --- Babel functions for processing -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; Author: Jarmo Hurri (adapted from ob-asymptote.el written by Eric Schulte)
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

;; Babel support for evaluating processing source code.
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in processing
;;
;; 2) results can only be exported as html; in this case, the
;;    processing code is embedded via a file into a javascript block
;;    using the processing.js module; the script then draws the
;;    resulting output when the web page is viewed in a browser; note
;;    that the user is responsible for making sure that processing.js
;;    is available on the website
;;
;; 3) it is possible to interactively view the sketch of the
;;    Processing code block via Processing 2.0 Emacs mode, using
;;    `org-babel-processing-view-sketch'.  You can bind this command
;;    to, e.g., C-c C-v C-k with
;;
;;      (define-key org-babel-map (kbd "C-k") 'org-babel-processing-view-sketch)


;;; Requirements:

;; - processing2-emacs mode :: https://github.com/ptrv/processing2-emacs
;; - Processing.js module :: http://processingjs.org/

;;; Code:
(require 'ob)
(require 'sha1)

(declare-function processing-sketch-run "ext:processing-mode" ())

(defvar org-babel-temporary-directory)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("processing" . "pde"))

;; Default header tags depend on whether exporting html or not; if not
;; exporting html, then no results are produced; otherwise results are
;; HTML.
(defvar org-babel-default-header-args:processing
  '((:results . "html") (:exports . "results"))
  "Default arguments when evaluating a Processing source block.")

(defvar org-babel-processing-processing-js-filename "processing.js"
  "Filename of the processing.js file.")

(defun org-babel-processing-view-sketch ()
  "Show the sketch of the Processing block under point in an external viewer."
  (interactive)
  (require 'processing-mode)
  (let ((info (org-babel-get-src-block-info)))
    (if (string= (nth 0 info) "processing")
	(let* ((body (nth 1 info))
	       (params (org-babel-process-params (nth 2 info)))
	       (sketch-code
		(org-babel-expand-body:generic
		 body
		 params
		 (org-babel-variable-assignments:processing params))))
	  ;; Note: sketch filename can not contain a hyphen, since it
	  ;; has to be a valid java class name; for this reason
	  ;; make-temp-file is repeated until no hyphen is in the
	  ;; name; also sketch dir name must be the same as the
	  ;; basename of the sketch file.
	  (let* ((temporary-file-directory org-babel-temporary-directory)
		 (sketch-dir
		  (let (sketch-dir-candidate)
		    (while
			(progn
			  (setq sketch-dir-candidate
				(make-temp-file "processing" t))
			  (when (string-match-p
				 "-"
				 (file-name-nondirectory sketch-dir-candidate))
			    (delete-directory sketch-dir-candidate)
			    t)))
		    sketch-dir-candidate))
		 (sketch-filename
		  (concat sketch-dir
			  "/"
			  (file-name-nondirectory sketch-dir)
			  ".pde")))
	    (with-temp-file sketch-filename (insert sketch-code))
	    (find-file sketch-filename)
	    (processing-sketch-run)
	    (kill-buffer)))
      (message "Not inside a Processing source block."))))

(defun org-babel-execute:processing (body params)
  "Execute a block of Processing code.
This function is called by `org-babel-execute-src-block'."
  (let ((sketch-code
	 (org-babel-expand-body:generic
	  body
	  params
	  (org-babel-variable-assignments:processing params))))
    ;; Results are HTML.
    (let ((sketch-canvas-id (concat "ob-" (sha1 sketch-code))))
      (concat "<script src=\""
	      org-babel-processing-processing-js-filename
	      "\"></script>\n <script type=\"text/processing\""
	      " data-processing-target=\""
	      sketch-canvas-id
	      "\">\n"
	      sketch-code
	      "\n</script> <canvas id=\""
	      sketch-canvas-id
	      "\"></canvas>"))))

(defun org-babel-prep-session:processing (_session _params)
  "Return an error if the :session header argument is set.
Processing does not support sessions"
  (error "Processing does not support sessions"))

(defun org-babel-variable-assignments:processing (params)
  "Return list of processing statements assigning the block's variables."
  (mapcar #'org-babel-processing-var-to-processing
	  (org-babel--get-vars params)))

(defun org-babel-processing-var-to-processing (pair)
  "Convert an elisp value into a Processing variable.
The elisp value PAIR is converted into Processing code specifying
a variable of the same value."
  (let ((var (car pair))
        (val (let ((v (cdr pair)))
	       (if (symbolp v) (symbol-name v) v))))
    (cond
     ((integerp val)
      (format "int %S=%S;" var val))
     ((floatp val)
      (format "float %S=%S;" var val))
     ((stringp val)
      (format "String %S=\"%s\";" var val))
     ((and (listp val) (not (listp (car val))))
      (let* ((type (org-babel-processing-define-type val))
	     (fmt (if (eq 'String type) "\"%s\"" "%s"))
	     (vect (mapconcat (lambda (e) (format fmt e)) val ", ")))
	(format "%s[] %S={%s};" type var vect)))
     ((listp val)
      (let* ((type (org-babel-processing-define-type val))
	     (fmt (if (eq 'String type) "\"%s\"" "%s"))
             (array (mapconcat (lambda (row)
				 (concat "{"
					 (mapconcat (lambda (e) (format fmt e))
						    row ", ")
					 "}"))
			       val ",")))
        (format "%S[][] %S={%s};" type var array))))))

(defun org-babel-processing-define-type (data)
  "Determine type of DATA.

DATA is a list.  Return type as a symbol.

The type is `String' if any element in DATA is a string.
Otherwise, it is either `float', if some elements are floats, or
`int'."
  (letrec ((type 'int)
	   (find-type
	    (lambda (row)
	      (dolist (e row type)
		(cond ((listp e) (setq type (funcall find-type e)))
		      ((stringp e) (throw 'exit 'String))
		      ((floatp e) (setq type 'float)))))))
    (catch 'exit (funcall find-type data))))

(provide 'ob-processing)

;;; ob-processing.el ends here
