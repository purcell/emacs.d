;;; ob-lob.el --- Functions Supporting the Library of Babel -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	 Dan Davison
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

;;; Code:
(require 'cl-lib)
(require 'ob-core)
(require 'ob-table)

(declare-function org-babel-ref-split-args "ob-ref" (arg-string))
(declare-function org-element-at-point "org-element" ())
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))

(defvar org-babel-library-of-babel nil
  "Library of source-code blocks.
This is an association list.  Populate the library by calling
`org-babel-lob-ingest' on files containing source blocks.")

(defvar org-babel-default-lob-header-args '((:exports . "results"))
  "Default header arguments to use when exporting Babel calls.
By default, a Babel call inherits its arguments from the source
block being called.  Header arguments defined in this variable
take precedence over these.  It is useful for properties that
should not be inherited from a source block.")

(defun org-babel-lob-ingest (&optional file)
  "Add all named source blocks defined in FILE to `org-babel-library-of-babel'."
  (interactive "fFile: ")
  (let ((lob-ingest-count 0))
    (org-babel-map-src-blocks file
      (let* ((info (org-babel-get-src-block-info 'light))
	     (source-name (nth 4 info)))
	(when source-name
	  (setf (nth 1 info)
		(if (org-babel-noweb-p (nth 2 info) :eval)
		    (org-babel-expand-noweb-references info)
		  (nth 1 info)))
	  (let ((source (intern source-name)))
	    (setq org-babel-library-of-babel
		  (cons (cons source info)
			(assq-delete-all source org-babel-library-of-babel))))
	  (cl-incf lob-ingest-count))))
    (message "%d src block%s added to Library of Babel"
	     lob-ingest-count (if (> lob-ingest-count 1) "s" ""))
    lob-ingest-count))

;; Functions for executing lob one-liners.

;;;###autoload
(defun org-babel-lob-execute-maybe ()
  "Execute a Library of Babel source block, if appropriate.
Detect if this is context for a Library Of Babel source block and
if so then run the appropriate source block from the Library."
  (interactive)
  (let ((info (org-babel-lob-get-info)))
    (when info
      (org-babel-execute-src-block nil info)
      t)))

(defun org-babel-lob--src-info (name)
  "Return internal representation for Babel data named NAME.
NAME is a string.  This function looks into the current document
for a Babel call or source block.  If none is found, it looks
after NAME in the Library of Babel.  Eventually, if that also
fails, it returns nil."
  ;; During export, look into the pristine copy of the document being
  ;; exported instead of the current one, which could miss some data.
  (with-current-buffer (or org-babel-exp-reference-buffer (current-buffer))
    (org-with-wide-buffer
     (goto-char (point-min))
     (catch :found
       (let ((case-fold-search t)
	     (regexp (org-babel-named-data-regexp-for-name name)))
	 (while (re-search-forward regexp nil t)
	   (let ((element (org-element-at-point)))
	     (when (equal name (org-element-property :name element))
	       (throw :found
		      (pcase (org-element-type element)
			(`src-block (org-babel-get-src-block-info t element))
			(`babel-call (org-babel-lob-get-info element))
			;; Non-executable data found.  Since names are
			;; supposed to be unique throughout a document,
			;; bail out.
			(_ nil))))))
	 ;; No element named NAME in buffer.  Try Library of Babel.
	 (cdr (assoc-string name org-babel-library-of-babel)))))))

;;;###autoload
(defun org-babel-lob-get-info (&optional datum)
  "Return internal representation for Library of Babel function call.
Consider DATUM, when provided, or element at point.  Return nil
when not on an appropriate location.  Otherwise return a list
compatible with `org-babel-get-src-block-info', which see."
  (let* ((context (or datum (org-element-context)))
	 (type (org-element-type context)))
    (when (memq type '(babel-call inline-babel-call))
      (pcase (org-babel-lob--src-info (org-element-property :call context))
	(`(,language ,body ,header ,_ ,_ ,_ ,coderef)
	 (let ((begin (org-element-property (if (eq type 'inline-babel-call)
						:begin
					      :post-affiliated)
					    context)))
	   (list language
		 body
		 (apply #'org-babel-merge-params
			header
			org-babel-default-lob-header-args
			(append
			 (org-with-wide-buffer
			  (goto-char begin)
			  (org-babel-params-from-properties language))
			 (list
			  (org-babel-parse-header-arguments
			   (org-element-property :inside-header context))
			  (let ((args (org-element-property :arguments context)))
			    (and args
				 (mapcar (lambda (ref) (cons :var ref))
					 (org-babel-ref-split-args args))))
			  (org-babel-parse-header-arguments
			   (org-element-property :end-header context)))))
		 nil
		 (org-element-property :name context)
		 begin
		 coderef)))
	(_ nil)))))

(provide 'ob-lob)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ob-lob.el ends here
