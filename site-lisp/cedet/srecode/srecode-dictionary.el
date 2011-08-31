;;; srecode-dictionary.el --- Dictionary code for the semantic recoder.

;; Copyright (C) 2007, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-dictionary.el,v 1.9 2009/02/11 00:43:45 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Dictionaries contain lists of names and their assocaited values.
;; These dictionaries are used to fill in macros from recoder templates.

;;; Code:

;;; CLASSES
;;

;;;###autoload
(defclass srecode-dictionary ()
  ((namehash :initarg :namehash
	     :documentation
	     "Hash table containing the names of all the templates.")
   (buffer :initarg :buffer
	   :documentation
	   "The buffer this dictionary was initialized with.")
   (parent :initarg :parent
	   :type (or null srecode-dictionary)
	   :documentation
	   "The parent dictionary.
Symbols not appearing in this dictionary will be checked against the
parent dictionary.")
   )
  "Dictionary of symbols and what they mean.
Dictionaries are used to look up named symbols from
templates to decide what to do with those symbols.")

;;;###autoload
(defclass srecode-dictionary-compound-value ()
  ()
  "A compound dictionary value.
Values stored in a dictionary must be a STRING,
a dictionary for showing sections, or an instance of a subclass
of this class.

Compound dictionary values derive from this class, and must
provide a sequence of method implementations to convert into
a string."
  :abstract t)

;;;###autoload
(defclass srecode-dictionary-compound-variable 
  (srecode-dictionary-compound-value)
  ((value :initarg :value
	  :documentation
	  "The value of this template variable.
Variables in template files are usually a single string
which can be inserted into a dictionary directly.

Some variables may be more complex and involve dictionary
lookups, strings, concatenation, or the like.

The format of VALUE is determined by current template
formatting rules.")
   (compiled :initarg :compiled
	     :type list
	     :documentation
	     "The compiled version of VALUE.")
   )
  "A compound dictionary value for template file variables.
You can declare a variable in a template like this:

set NAME \"str\" macro \"OTHERNAME\"

with appending various parts together in a list.")

(defmethod initialize-instance ((this srecode-dictionary-compound-variable)
				&optional fields)
  "Initialize the compound variable THIS.
Makes sure that :value is compiled."
  (let ((newfields nil)
	(state nil))
    (while fields
      ;; Strip out :state
      (if (eq (car fields) :state)
	  (setq state (car (cdr fields)))
	(setq newfields (cons (car (cdr fields))
			      (cons (car fields) newfields))))
      (setq fields (cdr (cdr fields))))

    (when (not state)
      (error "Cannot create compound variable without :state"))
    
    (call-next-method this (nreverse newfields))
    (when (not (slot-boundp this 'compiled))
      (let ((val (oref this :value))
	    (comp nil))
	(while val
	  (let ((nval (car val))
		)
	    (cond ((stringp nval)
		   (setq comp (cons nval comp)))
		  ((and (listp nval)
			(equal (car nval) 'macro))
		   (setq comp (cons
			       (srecode-compile-parse-inserter 
				(cdr nval)
				state)
			       comp)))
		  (t
		   (error "Don't know how to handle variable value %S" nval)))
	    )
	  (setq val (cdr val)))
	(oset this :compiled (nreverse comp))))))

;;; DICTIONARY METHODS
;;

;;;###autoload
(defun srecode-create-dictionary (&optional buffer-or-parent)
  "Create a dictionary for BUFFER.
If BUFFER-OR-PARENT is not specified, assume a buffer, and
use the current buffer.
If BUFFER-OR-PARENT is another dictionary, then remember the
parent within the new dictionary, and assume that BUFFER
is the same as belongs to the parent dictionary.
The dictionary is initialized with variables setup for that
buffer's table.
If BUFFER-OR-PARENT is t, then this dictionary should not be
assocated with a buffer or parent."
  (save-excursion
    (let ((parent nil)
	  (buffer nil))
      (cond ((bufferp buffer-or-parent)
	     (set-buffer buffer-or-parent)
	     (setq buffer buffer-or-parent))
	    ((srecode-dictionary-child-p buffer-or-parent)
	     (setq parent buffer-or-parent
		   buffer (oref buffer-or-parent buffer))
	     (when buffer
	       (set-buffer buffer)))
	    ((eq buffer-or-parent t)
	     (setq buffer nil))
	    (t
	     (setq buffer (current-buffer)))
	    )
      (let ((dict (srecode-dictionary
		   major-mode
		   :buffer buffer
		   :parent parent
		   :namehash  (make-hash-table :test 'equal
					       :size 20))))
	;; Only set up the default variables if we don't have
	;; a buffer
	(when buffer
	  ;; Variables from the table we are inserting from.
	  ;; @todo - get a better tree of tables.
	  (let ((mt (srecode-get-mode-table major-mode))
		(def (srecode-get-mode-table 'default)))
	    ;; Each table has multiple template tables.
	    ;; Do DEF first so that MT can override any values.
	    (srecode-dictionary-add-template-table dict def)
	    (srecode-dictionary-add-template-table dict mt)
	    ))
	dict))))

(defmethod srecode-dictionary-add-template-table ((dict srecode-dictionary)
						  tpl)
  "Insert into DICT the variables found in table TPL.
TPL is an object representing a compiled template file."
  (when tpl
    (let ((tabs (oref tpl :tables)))
      (while tabs
	(let ((vars (oref (car tabs) variables)))
	  (while vars
	    (srecode-dictionary-set-value
	     dict (car (car vars)) (cdr (car vars)))
	    (setq vars (cdr vars))))
	(setq tabs (cdr tabs))))))


(defmethod srecode-dictionary-set-value ((dict srecode-dictionary)
					 name value)
  "In dictionary DICT, set NAME to have VALUE."
  ;; Validate inputs
  (if (not (stringp name))
      (signal 'wrong-type-argument (list name 'stringp)))
  ;; Add the value.
  (with-slots (namehash) dict
    (puthash name value namehash))
  )

(defmethod srecode-dictionary-add-section-dictionary ((dict srecode-dictionary)
						      name &optional show-only)
  "In dictionary DICT, add a section dictionary for section macro NAME.
Return the new dictionary.

You can add several dictionaries to the same section macro.
For each dictionary added to a macro, the block of codes in the
template will be repeated.

If optional argument SHOW-ONLY is non-nil, then don't add a new dictionarly
if there is already one in place.  Also, don't add FIRST/LAST entries.
These entries are not needed when we are just showing a section.

Each dictionary added will automatically get values for positional macros
which will enable SECTIONS to be enabled.

 * FIRST - The first entry in the table.
 * NOTFIRST - Not the first entry in the table.
 * LAST - The last entry in the table
 * NOTLAST - Not the last entry in the table.

Adding a new dictionary will alter these values in previously
inserted dictionaries."
  ;; Validate inputs
  (if (not (stringp name))
      (signal 'wrong-type-argument (list name 'stringp)))
  (let ((new (srecode-create-dictionary dict))
	(ov (srecode-dictionary-lookup-name dict name)))

    (when (not show-only)
      ;; Setup the FIRST/NOTFIRST and LAST/NOTLAST entries.
      (if (null ov)
	  (progn
	    (srecode-dictionary-show-section new "FIRST")
	    (srecode-dictionary-show-section new "LAST"))
	;; Not the very first one.  Lets clean up CAR.
	(let ((tail (car (last ov))))
	  (srecode-dictionary-hide-section tail "LAST")
	  (srecode-dictionary-show-section tail "NOTLAST")
	  )
	(srecode-dictionary-show-section new "NOTFIRST")
	(srecode-dictionary-show-section new "LAST"))
      )

    (when (or (not show-only) (null ov))
      (srecode-dictionary-set-value dict name (append ov (list new))))
    ;; Return the new sub-dictionary.
    new))

(defmethod srecode-dictionary-show-section ((dict srecode-dictionary) name)
  "In dictionary DICT, indicate that the section NAME should be exposed."
  ;; Validate inputs
  (if (not (stringp name))
      (signal 'wrong-type-argument (list name 'stringp)))
  ;; Showing a section is just like making a section dictionary, but
  ;; with no dictionary values to add.
  (srecode-dictionary-add-section-dictionary dict name t)
  nil)

(defmethod srecode-dictionary-hide-section ((dict srecode-dictionary) name)
  "In dictionary DICT, indicate that the section NAME should be hidden."
  ;; We need to find the has value, and then delete it.
  ;; Validate inputs
  (if (not (stringp name))
      (signal 'wrong-type-argument (list name 'stringp)))
  ;; Add the value.
  (with-slots (namehash) dict
    (remhash name namehash))
  nil)

(defmethod srecode-dictionary-merge ((dict srecode-dictionary) otherdict)
  "Merge into DICT the dictionary entries from OTHERDICT."
  (when otherdict
    (maphash
     (lambda (key entry)
       ;; Only merge in the new values if there was no old value.
       ;; This protects applications from being whacked, and basically
       ;; makes these new section dictionary entries act like
       ;; "defaults" instead of overrides.
       (when (not (srecode-dictionary-lookup-name dict key))
	 (cond ((and (listp entry) (srecode-dictionary-p (car entry)))
		;; A list of section dictionaries.
		;; We need to merge them in.
		(while entry
		  (let ((new-sub-dict
			 (srecode-dictionary-add-section-dictionary
			  dict key)))
		    (srecode-dictionary-merge new-sub-dict (car entry)))
		  (setq entry (cdr entry)))
		  )

	       (t
		(srecode-dictionary-set-value dict key entry)))
	       ))
     (oref otherdict namehash))))

(defmethod srecode-dictionary-lookup-name ((dict srecode-dictionary)
					   name)
  "Return information about the current DICT's value for NAME."
  (if (not (slot-boundp dict 'namehash))
      nil
    ;; Get the value of this name from the dictionary
    (or (with-slots (namehash) dict
	  (gethash name namehash))
	(and (not (member name '("FIRST" "LAST" "NOTFIRST" "NOTLAST")))
	     (oref dict parent)
	     (srecode-dictionary-lookup-name (oref dict parent) name))
	)))

(defmethod srecode-root-dictionary ((dict srecode-dictionary))
  "For dictionary DICT, return the root dictionary.
The root dictionary is usually for a current or active insertion."
  (let ((ans dict))
    (while (oref ans parent)
      (setq ans (oref ans parent)))
    ans))

;;; COMPOUND VALUE METHODS
;;
;; Compound values must provide at least the toStriong method
;; for use in converting the compound value into sometehing insertable.

(defmethod srecode-compound-toString ((cp srecode-dictionary-compound-value)
				      function
				      dictionary)
  "Convert the compound dictionary value CP to a string.
If FUNCTION is non-nil, then FUNCTION is somehow applied to an aspect
of the compound value.  The FUNCTION could be a fraction
of some function symbol with a logical prefix excluded.

If you subclass `srecode-dictionary-compound-value' then this
method could return nil, but if it does that, it must insert
the value itself using `princ', or by detecting if the current
standard out is a buffer, and using `insert'."
  (object-name cp))

(defmethod srecode-dump ((cp srecode-dictionary-compound-value)
			 &optional indent)
  "Display information about this compound value."
  (princ (object-name cp))
  )

(defmethod srecode-compound-toString ((cp srecode-dictionary-compound-variable)
				      function
				      dictionary)
  "Convert the compound dictionary variable value CP into a string.
FUNCTION and DICTIONARY are as for the baseclass."
  (srecode-insert-code-stream (oref cp compiled) dictionary))


(defmethod srecode-dump ((cp srecode-dictionary-compound-variable)
			 &optional indent)
  "Display information about this compound value."
  (princ "# Compound Variable #\n")
  (let ((indent (+ 4 (or indent 0)))
	(cmp (oref cp compiled))
	)
    (srecode-dump-code-list cmp (make-string indent ? ))
    ))


;;; Higher level dictionary functions
;;
(defun srecode-create-section-dicionary (sectiondicts STATE)
  "Create a dictionary with section entries for a template.
The format for SECTIONDICTS is what is emitted from the template parsers.
STATE is the current compiler state."
  (when sectiondicts
    (let ((new (srecode-create-dictionary t)))
      ;; Loop over each section.  The section is a macro w/in the
      ;; template.
      (while sectiondicts
	(let* ((sect (car (car sectiondicts)))
	       (entries (cdr (car sectiondicts)))
	       (subdict (srecode-dictionary-add-section-dictionary new sect))
	       )
	  ;; Loop over each entry.  This is one variable in the
	  ;; section dictionary.
	  (while entries
	    (let ((tname (semantic-tag-name (car entries)))
		  (val (semantic-tag-variable-default (car entries))))
	      (if (eq val t)
		  (srecode-dictionary-show-section subdict tname)
		(cond
		 ((and (stringp (car val))
		       (= (length val) 1))
		  (setq val (car val)))
		 (t
		  (setq val (srecode-dictionary-compound-variable
			     tname :value val :state STATE))))
		(srecode-dictionary-set-value
		 subdict tname val))
	      (setq entries (cdr entries))))
	  )
	(setq sectiondicts (cdr sectiondicts)))
      new)))

;;; DUMP DICTIONARY
;;
;; Make a dictionary, and dump it's contents.

;;;###autoload
(defun srecode-adebug-dictionary ()
  "Run data-debug on this mode's dictionary."
  (interactive)
  (require 'data-debug)
  (let* ((modesym major-mode)
	 (start (current-time))
	 (junk (or (progn (srecode-load-tables-for-mode modesym)
			  (srecode-get-mode-table modesym))
		   (error "No table found for mode %S" modesym)))
	 (dict (srecode-create-dictionary (current-buffer)))
	 (end (current-time))
	 )
    (message "Creating a dictionary took %.2f seconds."
	     (semantic-elapsed-time start end))
    (data-debug-new-buffer "*SRECUDE ADEBUG*")
    (data-debug-insert-object-slots dict "*")))

;;;###autoload
(defun srecode-dictionary-dump ()
  "Dump a typical fabricated dictionary."
  (interactive)
  (let ((modesym major-mode))
    ;; This load allows the dictionary access to inherited
    ;; and stacked dictionary entries.
    (srecode-load-tables-for-mode modesym)
    (let ((tmp (srecode-get-mode-table modesym))
	  )
      (if (not tmp)
	  (error "No table found for mode %S" modesym))
      ;; Now make the dictionary.
      (let ((dict (srecode-create-dictionary (current-buffer))))
	(with-output-to-temp-buffer "*SRECODE DUMP*"
	  (princ "DICTIONARY FOR ")
	  (princ major-mode)
	  (princ "\n--------------------------------------------\n")
	  (srecode-dump dict))
	))))

(defmethod srecode-dump ((dict srecode-dictionary) &optional indent)
  "Dump a dictionary."
  (if (not indent) (setq indent 0))
  (maphash (lambda (key entry)
	     (princ (make-string indent ? ))
	     (princ " ")
	     (princ key)
	     (princ " ")
	     (cond ((and (listp entry)
			 (srecode-dictionary-p (car entry)))
		    (let ((newindent (if indent
					 (+ indent 4)
				       4)))
		      (while entry
			(princ " --> SUBDICTIONARY ")
			(princ (object-name dict))
			(princ "\n")
			(srecode-dump (car entry) newindent)
			(setq entry (cdr entry))
			))
		    (princ "\n")
		    )
		   ((srecode-dictionary-compound-value-child-p entry)
		    (srecode-dump entry indent)
		    (princ "\n")
		    )
		   (t
		    (prin1 entry)
		    ;(princ "\n")
		    ))
	     (terpri)
	     )
	   (oref dict namehash))
  )

(provide 'srecode-dictionary)
;;; srecode-dictionary.el ends here

