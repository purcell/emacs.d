;;; ob-exp.el --- Exportation of Babel Source Blocks -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	Dan Davison
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
(require 'ob-core)

(declare-function org-babel-lob-get-info "ob-lob" (&optional datum))
(declare-function org-element-at-point "org-element" ())
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-escape-code-in-string "org-src" (s))
(declare-function org-export-copy-buffer "ox" ())
(declare-function org-fill-template "org" (template alist))
(declare-function org-get-indentation "org" (&optional line))
(declare-function org-in-commented-heading-p "org" (&optional no-inheritance))

(defvar org-src-preserve-indentation)

(defcustom org-export-use-babel t
  "Switch controlling code evaluation and header processing during export.
When set to nil no code will be evaluated as part of the export
process and no header arguments will be obeyed.  Users who wish
to avoid evaluating code on export should use the header argument
`:eval never-export'."
  :group 'org-babel
  :version "24.1"
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t))
  :safe #'null)


(defmacro org-babel-exp--at-source (&rest body)
  "Evaluate BODY at the source of the Babel block at point.
Source is located in `org-babel-exp-reference-buffer'.  The value
returned is the value of the last form in BODY.  Assume that
point is at the beginning of the Babel block."
  (declare (indent 1) (debug body))
  `(let ((source (get-text-property (point) 'org-reference)))
     (with-current-buffer org-babel-exp-reference-buffer
       (org-with-wide-buffer
	(goto-char source)
	,@body))))

(defun org-babel-exp-src-block ()
  "Process source block for export.
Depending on the \":export\" header argument, replace the source
code block like this:

both ---- display the code and the results

code ---- the default, display the code inside the block but do
          not process

results - just like none only the block is run on export ensuring
          that its results are present in the Org mode buffer

none ---- do not display either code or results upon export

Assume point is at block opening line."
  (interactive)
  (save-excursion
    (let* ((info (org-babel-get-src-block-info 'light))
	   (lang (nth 0 info))
	   (raw-params (nth 2 info))
	   hash)
      ;; bail if we couldn't get any info from the block
      (unless noninteractive
	(message "org-babel-exp process %s at position %d..."
		 lang
		 (line-beginning-position)))
      (when info
	;; if we're actually going to need the parameters
	(when (member (cdr (assq :exports (nth 2 info))) '("both" "results"))
	  (let ((lang-headers (intern (concat "org-babel-default-header-args:"
					      lang))))
	    (org-babel-exp--at-source
		(setf (nth 2 info)
		      (org-babel-process-params
		       (apply #'org-babel-merge-params
			      org-babel-default-header-args
			      (and (boundp lang-headers)
				   (symbol-value lang-headers))
			      (append (org-babel-params-from-properties lang)
				      (list raw-params)))))))
	  (setf hash (org-babel-sha1-hash info)))
	(org-babel-exp-do-export info 'block hash)))))

(defcustom org-babel-exp-call-line-template
  ""
  "Template used to export call lines.
This template may be customized to include the call line name
with any export markup.  The template is filled out using
`org-fill-template', and the following %keys may be used.

 line --- call line

An example value would be \"\\n: call: %line\" to export the call line
wrapped in a verbatim environment.

Note: the results are inserted separately after the contents of
this template."
  :group 'org-babel
  :type 'string)

(defun org-babel-exp-process-buffer ()
  "Execute all Babel blocks in current buffer."
  (interactive)
  (when org-export-use-babel
    (save-window-excursion
      (let ((case-fold-search t)
	    (regexp "\\(call\\|src\\)_\\|^[ \t]*#\\+\\(BEGIN_SRC\\|CALL:\\)")
	    ;; Get a pristine copy of current buffer so Babel
	    ;; references are properly resolved and source block
	    ;; context is preserved.
	    (org-babel-exp-reference-buffer (org-export-copy-buffer)))
	(unwind-protect
	    (save-excursion
	      ;; First attach to every source block their original
	      ;; position, so that they can be retrieved within
	      ;; `org-babel-exp-reference-buffer', even after heavy
	      ;; modifications on current buffer.
	      ;;
	      ;; False positives are harmless, so we don't check if
	      ;; we're really at some Babel object.  Moreover,
	      ;; `line-end-position' ensures that we propertize
	      ;; a noticeable part of the object, without affecting
	      ;; multiple objects on the same line.
	      (goto-char (point-min))
	      (while (re-search-forward regexp nil t)
		(let ((s (match-beginning 0)))
		  (put-text-property s (line-end-position) 'org-reference s)))
	      ;; Evaluate from top to bottom every Babel block
	      ;; encountered.
	      (goto-char (point-min))
	      (while (re-search-forward regexp nil t)
		(unless (save-match-data (org-in-commented-heading-p))
		  (let* ((object? (match-end 1))
			 (element (save-match-data
				    (if object? (org-element-context)
				      ;; No deep inspection if we're
				      ;; just looking for an element.
				      (org-element-at-point))))
			 (type
			  (pcase (org-element-type element)
			    ;; Discard block elements if we're looking
			    ;; for inline objects.  False results
			    ;; happen when, e.g., "call_" syntax is
			    ;; located within affiliated keywords:
			    ;;
			    ;; #+name: call_src
			    ;; #+begin_src ...
			    ((and (or `babel-call `src-block) (guard object?))
			     nil)
			    (type type)))
			 (begin
			  (copy-marker (org-element-property :begin element)))
			 (end
			  (copy-marker
			   (save-excursion
			     (goto-char (org-element-property :end element))
			     (skip-chars-backward " \r\t\n")
			     (point)))))
		    (pcase type
		      (`inline-src-block
		       (let* ((info
			       (org-babel-get-src-block-info nil element))
			      (params (nth 2 info)))
			 (setf (nth 1 info)
			       (if (and (cdr (assq :noweb params))
					(string= "yes"
						 (cdr (assq :noweb params))))
				   (org-babel-expand-noweb-references
				    info org-babel-exp-reference-buffer)
				 (nth 1 info)))
			 (goto-char begin)
			 (let ((replacement
				(org-babel-exp-do-export info 'inline)))
			   (if (equal replacement "")
			       ;; Replacement code is empty: remove
			       ;; inline source block, including extra
			       ;; white space that might have been
			       ;; created when inserting results.
			       (delete-region begin
					      (progn (goto-char end)
						     (skip-chars-forward " \t")
						     (point)))
			     ;; Otherwise: remove inline src block but
			     ;; preserve following white spaces.  Then
			     ;; insert value.
			     (delete-region begin end)
			     (insert replacement)))))
		      ((or `babel-call `inline-babel-call)
		       (org-babel-exp-do-export (org-babel-lob-get-info element)
						'lob)
		       (let ((rep
			      (org-fill-template
			       org-babel-exp-call-line-template
			       `(("line"  .
				  ,(org-element-property :value element))))))
			 ;; If replacement is empty, completely remove
			 ;; the object/element, including any extra
			 ;; white space that might have been created
			 ;; when including results.
			 (if (equal rep "")
			     (delete-region
			      begin
			      (progn (goto-char end)
				     (if (not (eq type 'babel-call))
					 (progn (skip-chars-forward " \t")
						(point))
				       (skip-chars-forward " \r\t\n")
				       (line-beginning-position))))
			   ;; Otherwise, preserve trailing
			   ;; spaces/newlines and then, insert
			   ;; replacement string.
			   (goto-char begin)
			   (delete-region begin end)
			   (insert rep))))
		      (`src-block
		       (let ((match-start (copy-marker (match-beginning 0)))
			     (ind (org-get-indentation)))
			 ;; Take care of matched block: compute
			 ;; replacement string.  In particular, a nil
			 ;; REPLACEMENT means the block is left as-is
			 ;; while an empty string removes the block.
			 (let ((replacement
				(progn (goto-char match-start)
				       (org-babel-exp-src-block))))
			   (cond ((not replacement) (goto-char end))
				 ((equal replacement "")
				  (goto-char end)
				  (skip-chars-forward " \r\t\n")
				  (beginning-of-line)
				  (delete-region begin (point)))
				 (t
				  (goto-char match-start)
				  (delete-region (point)
						 (save-excursion
						   (goto-char end)
						   (line-end-position)))
				  (insert replacement)
				  (if (or org-src-preserve-indentation
					  (org-element-property
					   :preserve-indent element))
				      ;; Indent only code block
				      ;; markers.
				      (save-excursion
					(skip-chars-backward " \r\t\n")
					(indent-line-to ind)
					(goto-char match-start)
					(indent-line-to ind))
				    ;; Indent everything.
				    (indent-rigidly
				     match-start (point) ind)))))
			 (set-marker match-start nil))))
		    (set-marker begin nil)
		    (set-marker end nil)))))
	  (kill-buffer org-babel-exp-reference-buffer)
	  (remove-text-properties (point-min) (point-max) '(org-reference)))))))

(defun org-babel-exp-do-export (info type &optional hash)
  "Return a string with the exported content of a code block.
The function respects the value of the :exports header argument."
  (let ((silently (lambda () (let ((session (cdr (assq :session (nth 2 info)))))
			  (unless (equal "none" session)
			    (org-babel-exp-results info type 'silent)))))
	(clean (lambda () (if (eq type 'inline)
			 (org-babel-remove-inline-result)
		       (org-babel-remove-result info)))))
    (pcase (or (cdr (assq :exports (nth 2 info))) "code")
      ("none" (funcall silently) (funcall clean) "")
      ("code" (funcall silently) (funcall clean) (org-babel-exp-code info type))
      ("results" (org-babel-exp-results info type nil hash) "")
      ("both"
       (org-babel-exp-results info type nil hash)
       (org-babel-exp-code info type)))))

(defcustom org-babel-exp-code-template
  "#+BEGIN_SRC %lang%switches%flags\n%body\n#+END_SRC"
  "Template used to export the body of code blocks.
This template may be customized to include additional information
such as the code block name, or the values of particular header
arguments.  The template is filled out using `org-fill-template',
and the following %keys may be used.

 lang ------ the language of the code block
 name ------ the name of the code block
 body ------ the body of the code block
 switches -- the switches associated to the code block
 flags ----- the flags passed to the code block

In addition to the keys mentioned above, every header argument
defined for the code block may be used as a key and will be
replaced with its value."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-exp-inline-code-template
  "src_%lang[%switches%flags]{%body}"
  "Template used to export the body of inline code blocks.
This template may be customized to include additional information
such as the code block name, or the values of particular header
arguments.  The template is filled out using `org-fill-template',
and the following %keys may be used.

 lang ------ the language of the code block
 name ------ the name of the code block
 body ------ the body of the code block
 switches -- the switches associated to the code block
 flags ----- the flags passed to the code block

In addition to the keys mentioned above, every header argument
defined for the code block may be used as a key and will be
replaced with its value."
  :group 'org-babel
  :type 'string
  :version "26.1"
  :package-version '(Org . "8.3"))

(defun org-babel-exp-code (info type)
  "Return the original code block formatted for export."
  (setf (nth 1 info)
	(if (string= "strip-export" (cdr (assq :noweb (nth 2 info))))
	    (replace-regexp-in-string
	     (org-babel-noweb-wrap) "" (nth 1 info))
	  (if (org-babel-noweb-p (nth 2 info) :export)
	      (org-babel-expand-noweb-references
	       info org-babel-exp-reference-buffer)
	    (nth 1 info))))
  (org-fill-template
   (if (eq type 'inline)
       org-babel-exp-inline-code-template
       org-babel-exp-code-template)
   `(("lang"  . ,(nth 0 info))
     ("body"  . ,(org-escape-code-in-string (nth 1 info)))
     ("switches" . ,(let ((f (nth 3 info)))
		      (and (org-string-nw-p f) (concat " " f))))
     ("flags" . ,(let ((f (assq :flags (nth 2 info))))
		   (and f (concat " " (cdr f)))))
     ,@(mapcar (lambda (pair)
		 (cons (substring (symbol-name (car pair)) 1)
		       (format "%S" (cdr pair))))
	       (nth 2 info))
     ("name"  . ,(or (nth 4 info) "")))))

(defun org-babel-exp-results (info type &optional silent hash)
  "Evaluate and return the results of the current code block for export.
Results are prepared in a manner suitable for export by Org mode.
This function is called by `org-babel-exp-do-export'.  The code
block will be evaluated.  Optional argument SILENT can be used to
inhibit insertion of results into the buffer."
  (unless (and hash (equal hash (org-babel-current-result-hash)))
    (let ((lang (nth 0 info))
	  (body (if (org-babel-noweb-p (nth 2 info) :eval)
		    (org-babel-expand-noweb-references
		     info org-babel-exp-reference-buffer)
		  (nth 1 info)))
	  (info (copy-sequence info))
	  (org-babel-current-src-block-location (point-marker)))
      ;; Skip code blocks which we can't evaluate.
      (when (fboundp (intern (concat "org-babel-execute:" lang)))
	(org-babel-eval-wipe-error-buffer)
	(setf (nth 1 info) body)
	(setf (nth 2 info)
	      (org-babel-exp--at-source
		(org-babel-process-params
		 (org-babel-merge-params
		  (nth 2 info)
		  `((:results . ,(if silent "silent" "replace")))))))
	(pcase type
	  (`block (org-babel-execute-src-block nil info))
	  (`inline
	    ;; Position the point on the inline source block
	    ;; allowing `org-babel-insert-result' to check that the
	    ;; block is inline.
	    (goto-char (nth 5 info))
	    (org-babel-execute-src-block nil info))
	  (`lob
	   (save-excursion
	     (goto-char (nth 5 info))
	     (let (org-confirm-babel-evaluate)
	       (org-babel-execute-src-block nil info)))))))))


(provide 'ob-exp)

;;; ob-exp.el ends here
