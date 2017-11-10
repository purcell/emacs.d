;;; ob-ref.el --- Babel Functions for Referencing External Data -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Functions for referencing data from the header arguments of a
;; org-babel block.  The syntax of such a reference should be

;;   #+VAR: variable-name=file:resource-id

;; - variable-name :: the name of the variable to which the value
;;                    will be assigned

;; - file :: path to the file containing the resource, or omitted if
;;           resource is in the current file

;; - resource-id :: the id or name of the resource

;; So an example of a simple src block referencing table data in the
;; same file would be

;;  #+NAME: sandbox
;;  | 1 |         2 | 3 |
;;  | 4 | org-babel | 6 |
;;
;;  #+begin_src emacs-lisp :var table=sandbox
;;    (message table)
;;  #+end_src

;;; Code:
(require 'ob-core)
(require 'cl-lib)

(declare-function org-babel-lob-get-info "ob-lob" (&optional datum))
(declare-function org-element-at-point "org-element" ())
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-end-of-meta-data "org" (&optional full))
(declare-function org-find-property "org" (property &optional value))
(declare-function org-id-find-id-file "org-id" (id))
(declare-function org-id-find-id-in-file "org-id" (id file &optional markerp))
(declare-function org-in-commented-heading-p "org" (&optional no-inheritance))
(declare-function org-narrow-to-subtree "org" ())
(declare-function org-show-context "org" (&optional key))
(declare-function org-trim "org" (s &optional keep-lead))

(defvar org-babel-ref-split-regexp
  "[ \f\t\n\r\v]*\\(.+?\\)[ \f\t\n\r\v]*=[ \f\t\n\r\v]*\\(.+\\)[ \f\t\n\r\v]*")

(defvar org-babel-update-intermediate nil
  "Update the in-buffer results of code blocks executed to resolve references.")

(defun org-babel-ref-parse (assignment)
  "Parse a variable ASSIGNMENT in a header argument.
If the right hand side of the assignment has a literal value
return that value, otherwise interpret as a reference to an
external resource and find its value using
`org-babel-ref-resolve'.  Return a list with two elements.  The
first element of the list will be the name of the variable, and
the second will be an emacs-lisp representation of the value of
the variable."
  (when (string-match org-babel-ref-split-regexp assignment)
    (let ((var (match-string 1 assignment))
	  (ref (match-string 2 assignment)))
      (cons (intern var)
	    (let ((out (save-excursion
			 (when org-babel-current-src-block-location
			   (goto-char (if (markerp org-babel-current-src-block-location)
					  (marker-position org-babel-current-src-block-location)
					org-babel-current-src-block-location)))
			 (org-babel-read ref))))
	      (if (equal out ref)
		  (if (and (string-prefix-p "\"" ref)
			   (string-suffix-p "\"" ref))
		      (read ref)
		    (org-babel-ref-resolve ref))
		out))))))

(defun org-babel-ref-goto-headline-id (id)
  (or (let ((h (org-find-property "CUSTOM_ID" id)))
	(when h (goto-char h)))
      (let* ((file (org-id-find-id-file id))
	     (m (when file (org-id-find-id-in-file id file 'marker))))
	(when (and file m)
	  (message "file:%S" file)
	  (pop-to-buffer-same-window (marker-buffer m))
	  (goto-char m)
	  (move-marker m nil)
	  (org-show-context)
	  t))))

(defun org-babel-ref-headline-body ()
  (save-restriction
    (org-narrow-to-subtree)
    (buffer-substring
     (save-excursion (goto-char (point-min))
		     (org-end-of-meta-data)
		     (point))
     (point-max))))

(defvar org-babel-library-of-babel)
(defun org-babel-ref-resolve (ref)
  "Resolve the reference REF and return its value."
  (save-window-excursion
    (with-current-buffer (or org-babel-exp-reference-buffer (current-buffer))
      (save-excursion
	(let ((case-fold-search t)
	      args new-refere new-header-args new-referent split-file split-ref
	      index)
	  ;; if ref is indexed grab the indices -- beware nested indices
	  (when (and (string-match "\\[\\([^\\[]+\\)\\]$" ref)
		     (let ((str (substring ref 0 (match-beginning 0))))
		       (= (cl-count ?\( str) (cl-count ?\) str))))
	    (setq index (match-string 1 ref))
	    (setq ref (substring ref 0 (match-beginning 0))))
	  ;; assign any arguments to pass to source block
	  (when (string-match
		 "^\\(.+?\\)\\(\\[\\(.*\\)\\]\\|\\(\\)\\)(\\(.*\\))$" ref)
	    (setq new-refere      (match-string 1 ref))
	    (setq new-header-args (match-string 3 ref))
	    (setq new-referent    (match-string 5 ref))
	    (when (> (length new-refere) 0)
	      (when (> (length new-referent) 0)
		(setq args (mapcar (lambda (ref) (cons :var ref))
				   (org-babel-ref-split-args new-referent))))
	      (when (> (length new-header-args) 0)
		(setq args (append (org-babel-parse-header-arguments
				    new-header-args) args)))
	      (setq ref new-refere)))
	  (when (string-match "^\\(.+\\):\\(.+\\)$" ref)
	    (setq split-file (match-string 1 ref))
	    (setq split-ref (match-string 2 ref))
	    (find-file split-file)
	    (setq ref split-ref))
	  (org-with-wide-buffer
	   (goto-char (point-min))
	   (let* ((params (append args '((:results . "silent"))))
		  (regexp (org-babel-named-data-regexp-for-name ref))
		  (result
		   (catch :found
		     ;; Check for code blocks or named data.
		     (while (re-search-forward regexp nil t)
		       ;; Ignore COMMENTed headings and orphaned
		       ;; affiliated keywords.
		       (unless (org-in-commented-heading-p)
			 (let ((e (org-element-at-point)))
			   (when (equal (org-element-property :name e) ref)
			     (goto-char
			      (org-element-property :post-affiliated e))
			     (pcase (org-element-type e)
			       (`babel-call
				(throw :found
				       (org-babel-execute-src-block
					nil (org-babel-lob-get-info e) params)))
			       (`src-block
				(throw :found
				       (org-babel-execute-src-block
					nil nil
					(and
					 (not org-babel-update-intermediate)
					 params))))
			       ((and (let v (org-babel-read-element e))
				     (guard v))
				(throw :found v))
			       (_ (error "Reference not found")))))))
		     ;; Check for local or global headlines by ID.
		     (when (org-babel-ref-goto-headline-id ref)
		       (throw :found (org-babel-ref-headline-body)))
		     ;; Check the Library of Babel.
		     (let ((info (cdr (assq (intern ref)
					    org-babel-library-of-babel))))
		       (when info
			 (throw :found
				(org-babel-execute-src-block nil info params))))
		     (error "Reference `%s' not found in this buffer" ref))))
	     (cond
	      ((symbolp result) (format "%S" result))
	      ((and index (listp result))
	       (org-babel-ref-index-list index result))
	      (t result)))))))))

(defun org-babel-ref-index-list (index lis)
  "Return the subset of LIS indexed by INDEX.

Indices are 0 based and negative indices count from the end of
LIS, so 0 references the first element of LIS and -1 references
the last.  If INDEX is separated by \",\"s then each \"portion\"
is assumed to index into the next deepest nesting or dimension.

A valid \"portion\" can consist of either an integer index, two
integers separated by a \":\" in which case the entire range is
returned, or an empty string or \"*\" both of which are
interpreted to mean the entire range and as such are equivalent
to \"0:-1\"."
  (if (and (> (length index) 0) (string-match "^\\([^,]*\\),?" index))
      (let* ((ind-re "\\(\\([-[:digit:]]+\\):\\([-[:digit:]]+\\)\\|\\*\\)")
	     (lgth (length lis))
	     (portion (match-string 1 index))
	     (remainder (substring index (match-end 0)))
	     (wrap (lambda (num) (if (< num 0) (+ lgth num) num)))
	     (open (lambda (ls) (if (and (listp ls) (= (length ls) 1)) (car ls) ls))))
	(funcall
	 open
	 (mapcar
	  (lambda (sub-lis)
	    (if (listp sub-lis)
		(org-babel-ref-index-list remainder sub-lis)
	      sub-lis))
	  (if (or (= 0 (length portion)) (string-match ind-re portion))
	      (mapcar
	       (lambda (n) (nth n lis))
	       (apply 'org-number-sequence
		      (if (and (> (length portion) 0) (match-string 2 portion))
			  (list
			   (funcall wrap (string-to-number (match-string 2 portion)))
			   (funcall wrap (string-to-number (match-string 3 portion))))
			(list (funcall wrap 0) (funcall wrap -1)))))
	    (list (nth (funcall wrap (string-to-number portion)) lis))))))
    lis))

(defun org-babel-ref-split-args (arg-string)
  "Split ARG-STRING into top-level arguments of balanced parenthesis."
  (mapcar #'org-trim (org-babel-balanced-split arg-string 44)))


(provide 'ob-ref)

;;; ob-ref.el ends here
