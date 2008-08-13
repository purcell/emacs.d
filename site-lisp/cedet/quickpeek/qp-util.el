;;; qp-util.el --- utility functions for parsing languages

;;; Copyright (C) 1999, 2000 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tools
;; X-RCS: $Id: qp-util.el,v 1.2 2005/09/30 20:42:11 zappo Exp $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; These are utility functions to be used by language context determining
;; functions.

;; This is to make it easiser to set the colors to useful things.
(require 'font-lock)

;;; Customization:
(defcustom quickpeek-use-tags t
  "*Use tags table for languages that use them as a symbol database."
  :group 'quickpeek
  :type 'boolean)

;;; Code:
(defmacro quickpeek-with-writable (&rest forms)
  "Allow the buffer to be writable and evaluate FORMS."
  (list 'let '((inhibit-read-only t))
	(cons 'progn forms)))
(put 'quickpeek-with-writable 'lisp-indent-function 0)

(defmacro quickpeek-with-alternate-syntax-table (&rest forms)
  "Execute  FORMS with the alternate syntax table specified in `font-lock'."
  (let ((ostvar (make-symbol "old-syntax-table")))
    `(let ((,ostvar (syntax-table)))
       (unwind-protect
	   (progn
	     (if (and (boundp 'font-lock-syntax-table)
		      font-lock-syntax-table)
		 (set-syntax-table font-lock-syntax-table))
	     ,@forms)
	 (set-syntax-table ,ostvar)))))
(put 'quickpeek-with-alternate-syntax-table 'lisp-indent-function 0)

(defun quickpeek-do-completion (newstring)
  "Insert NEWSTRING in place of the current symbol.
Used as the callback element of a completion widget."
  (set-buffer (marker-buffer quickpeek-marker))
  (goto-char quickpeek-marker)
  (let ((b (quickpeek-thing-bounds)))
    (delete-region (car b) (cdr b))
    (insert newstring))
  (sit-for 0)
  (quickpeek-update-contents))


;;; Data collection core
;;
(defmacro quickpeek-facep (face)
  "Return non-nil if FACE is a valid face."
  (if (fboundp 'find-face)
      `(find-face ,face)
    `(facep ,face)))

;;; String insertion routines
;;	
(defun quickpeek-plain-string-insert (str)
  "Insert STR as a `quickpeek' string.
This string can be a string, or a list of insert specifiers.
An insert specifier will be of this form:
  (SPEC1 SPEC2 ...)
Where carriage returns are not allowed in any SPEC.  A spec can be
a another spec, or a cons cell of the form (TEXT . FACE) where FACE is
a valid or the symbol 'leave-faces, meaning to not strip faces off of
the text inserted."
  (cond ((stringp str)
	 (remove-text-properties 0 (length str) '(face) str)
	 (insert (quickpeek-cleanup-string str)))
	((listp str)
	 (cond ((and (stringp (car str)) (quickpeek-facep (cdr str)))
		(let ((p (point)))
		  (insert (quickpeek-cleanup-string (car str)))
		  (put-text-property p (point) 'face (cdr str))))
	       ((and (stringp (car str)) (eq 'leave-faces (cdr str)))
		(insert (quickpeek-cleanup-string (car str))))
	       (t
		(mapcar 'quickpeek-plain-string-insert str))))
	(t (error))))

(defun quickpeek-error-form (error-detail)
  "Display some error that occured.
Argument ERROR-DETAIL is the list returned in a `condition-case' variable."
  ;; Lot this puppy with message
  (message "%S" error-detail)
  ;; Stick it into the frame.
  (insert "An error occured in quickpeek while collecting data:\n"
	  (format "%S" error-detail)))

(defun quickpeek-simple-form (one two three)
  "Insert three lines of text into the `quickpeek' buffer.
Arguments ONE, TWO, and THREE are text lines to insert."
  (quickpeek-plain-string-insert one)
  (insert "\n")
  (quickpeek-plain-string-insert two)
  (insert "\n")
  (quickpeek-plain-string-insert three))

(defun quickpeek-insert-completions (completion-list)
  "Insert a COMPLETION-LIST on one line of a `quickpeek' buffer."
  (while completion-list
    (widget-create 'push-button
		   :value (car completion-list)
		   :notify (lambda (widget &rest ignore)
			     (quickpeek-do-completion
			      (widget-get widget :value)))
		   (car completion-list))
    (setq completion-list (cdr completion-list))
    (if completion-list
	(progn
	  (insert " ")
	  ;; 73 = 80 - (length " More") - 2 brackets.
	  (if (> (+ (current-column) (length (car completion-list))) 72)
	      (progn
		(apply 'widget-create 'menu-choice
		       :tag "More"
		       :format "%[[%t]%]"
		       :value (car completion-list)
		       :help-echo "Choose additional completions."
		       :notify (lambda (widget &rest ignore)
				 (quickpeek-do-completion
				  (widget-get widget :value)))
		       (mapcar (lambda (c)
				 (list 'choice-item c))
			       completion-list))
		(setq completion-list nil)))))))

(defun quickpeek-functional-form (top-level-sexp-summary
				  context-summary completion-list)
  "Insert details about a `quickpeek'ed buffer.
TOP-LEVEL-SEXP-SUMMARY is a summary for the top level sexp the
cursor is in (ie, the function name/params).
CONTEXT-SUMMARY is info about the current context.  (ei, definition of
the cursor variable or function).
COMPLETION-LIST is a list of possible completions for the current
context.  This would be completable function names or variables.  The
completion list is not due to be exhaustive, there are better methods
for that.  It is meant to be localized for local variables, methods,
or whatever is apropriate."
  (quickpeek-plain-string-insert top-level-sexp-summary)
  (insert "\n")
  (quickpeek-plain-string-insert context-summary)
  (insert "\n")
  (quickpeek-insert-completions completion-list))

(defun quickpeek-file-form (file completion-list)
  "Insert details about a `quickpeek'ed buffer.
FILE is the name of the file.
COMPLETION-LIST is a list of possible completions for the current
context.  This would be completable pragmas, or directives.  The
completion list is not due to be exhaustive, there are better methods
for that.  It is meant to be localized for local variables, methods,
or whatever is apropriate."
  (insert "File: " file "\n")
  (quickpeek-insert-completions completion-list))

;;; Handy utilities
;;
(eval-and-compile
  (condition-case nil
      (require 'thingatpt)
    (error (require 'thing))))

(defun quickpeek-thing-bounds (&optional type)
  "Return the bounds of the thing under point.
Optional TYPE can be something passed down to the thing functions
available under Emacs (thingatpt) or XEmacs (thing)."
  (if (not type) (setq type 'sexp))
  (if (featurep 'thingatpt)
      (bounds-of-thing-at-point type)
    ;; The xemacs version
    (let ((newsym (intern (concat "thing-" (symbol-name type)))))
      (funcall newsym (point)))))

(defun quickpeek-thing (&optional type)
  "Return the thing under point.
Optional TYPE can be something passed down to the thing functions
available under Emacs (thingatpt) or XEmacs (thing)."
  (let ((b (quickpeek-thing-bounds type)))
    (if b
	(buffer-substring-no-properties (car b) (cdr b))
      nil)))

(defun quickpeek-thing-beginning (&optional type)
  "Move to the beginning of the thing point is on.
Optional argument TYPE is the type of thing to find the beginning of."
  (let ((b (quickpeek-thing-bounds type)))
    (goto-char (car b))))

(defun quickpeek-cleanup-string (string)
  "Remove execess whitespace from STRING."
  (if (string-match "^\\s-+" string)
      (setq string (replace-match "" nil t string)))
  ;(if (string-match "\\s-+$" string)
  ;    (setq string (replace-match "" nil t string)))
  (while (string-match "\\s-\\s-+\\|\n" string)
    (setq string (replace-match " " nil t string)))
  string)

(defun quickpeek-beginning-of-defun ()
  "Move to the beginning of a defun using `up-list'."
  ;; This irritating combo works on multiple situations
  ;; and seems to be the best I can do at the moment.
  (beginning-of-line)
  (let ((p (point)))
    (condition-case nil
	(while t (up-list -1))
      (error))
    (if (= (point) p)
	;; In this case, we must assume beginning of buffer.
	(goto-char (point-min)))))

(defun quickpeek-in-non-code ()
  "Return t if the cursor is in non-code.  (ie; comment ot string)."
  (let* ((bod (save-excursion (quickpeek-beginning-of-defun) (point)))
	 (ps (parse-partial-sexp bod (point))))
    (or (nth 3 ps) (nth 4 ps) (nth 5 ps) (nth 7 ps))))

;;; Quick peek Tag handling
;;
(eval-when-compile (require 'etags))
(defun quickpeek-find-tag-stealthy (tag)
  "Return a dotted pair (BUFFER . POS) where TAG can be found.
Uses the tags table, but does not set the mark."
  (require 'etags)
  (if (string= tag "")
      (error "Empty reference tag in `quickpeek-find-tag-stealthy'!"))
  (if (not quickpeek-use-tags)
      nil
    (if (fboundp 'find-tag-internal)
	;; XEmacs has this convenient function
	(find-tag-internal tag)
      ;; the following code has been mostly stolen from etags.el
      ;; in order to support returning a tag value.
      (save-excursion
	(let ((order-preds '(tag-exact-file-name-match-p
			     tag-exact-match-p
			     tag-symbol-match-p
			     tag-word-match-p
			     tag-any-match-p))
	      (inhibit-quit nil)
	      (first t)
	      order file tag-info goto-func
	      buff line
	      ;; Override this variable so we can change it
	      ;; by loading an scanning alternate tags tables
	      ;; without changing the user selected table.
	      (tags-file-name tags-file-name)
	      tl
	      (ltf (concat default-directory "TAGS"))
	      (ret nil))
	  (if (and (file-exists-p ltf) (not (member ltf tl)))
	      (visit-tags-table ltf))
	  (setq tl tags-table-list)
	  (while (and (not ret) tl)
	    (visit-tags-table-buffer (car tl))
	    (catch 'qualified-match-found
	      (while (or first (visit-tags-table-buffer t))
		(if first (progn (goto-char (point-min))
				 (setq first nil)))
		(setq order order-preds)
		(while order
		  (while (search-forward tag nil t)
		    (and (funcall (car order) tag)
			 (throw 'qualified-match-found nil)))
		  (setq order (cdr order))
		  (goto-char (point-min))))
	      ;; We only get here if there was no match.  Yikes!
	      ;; Force caller of us to use condition case for now.
	      (error "No tag found"))
	    ;; Found a tag.  Extract it.
	    (beginning-of-line)
	    ;; Expand the filename, use the tags table buffer's default-directory.
	    (setq file (expand-file-name (file-of-tag))
		  tag-info (etags-snarf-tag))
	    ;; Get the local value in the tags table buffer before switching.
	    (setq goto-func goto-tag-location-function)
	    ;; Find the right line in the specified file.
	    (setq buff (set-buffer (find-file-noselect file)))
	    ;; go there.
	    (save-excursion (funcall goto-func tag-info)
			    (setq line (point)))
	    ;; Return this junk
	    (setq ret (cons buff line))
	    (setq tl (cdr tl)))
	  ret)))))

(defun quickpeek-tags-completion (word)
  "Perform completion of WORD using tags tables."
  (if (not quickpeek-use-tags)
      nil
    (require 'etags)
    (cond ((boundp 'tags-complete-tag)
	   ;; Expand this as necessary since this is still somewhat incomplete.
	   (tags-complete-tag word nil t))
	  ((boundp 'find-tag-tag)
	   ;; XEmacs has this function which contains within it a function
	   ;; resembling this:
	   (all-completions word tag-completion-table nil))
	  (t
	   ;; There appears to be no convenience function for XEmacs.  Shame!
	   nil))))

(provide 'qp-util)

;;; qp-util.el ends here
