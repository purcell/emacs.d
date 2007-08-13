;;; srecode-dictionary.el --- Dictionary code for the semantic recoder.

;; Copyright (C) 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-dictionary.el,v 1.1 2007/02/21 01:56:27 zappo Exp $

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
Values stored in a dictionary must be a STRING, or something
that can be cast into a string with (format \"%S\" ...),
or perhaps a dictionary for the case of sections.

Compound dictionary values derive from this class, and must
provide a sequence of method implementations to convert into
a string."
  :abstract t)

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
buffer's table."
  (save-excursion
    (let ((parent nil)
	  (buffer nil))
      (cond ((bufferp buffer-or-parent)
	     (set-buffer buffer-or-parent))
	    ((srecode-dictionary-child-p buffer-or-parent)
	     (setq parent buffer-or-parent
		   buffer (oref buffer-or-parent buffer))
	     (set-buffer buffer))
	    (t
	     (setq buffer (current-buffer)))
	    )
      (let ((dict (srecode-dictionary
		   "tmp"
		   :buffer buffer
		   :parent parent
		   :namehash  (make-hash-table :test 'equal
					       :size 20))))
	;; Only set up the default variables if we don't have
	;; a parent dictionary.
	(when (not parent)
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
  "Insert into DICT the variables found in TPL."
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
      (signal 'wrong-type-argument
	      (list name 'stringp 'srecode-template-inserter)))
  ;; Add the value.
  (with-slots (namehash) dict
    (puthash name value namehash))
  )

(defmethod srecode-dictionary-add-section-dictionary ((dict srecode-dictionary)
						      name)
  "In dictionary DICT, add a section dictionary for section delimeters NAME.
Return the new dictionary.

You can add several dictionaries to the same section variables.
For each dictionary added, the block of codes in the template will be
repeated."
  (let ((new (srecode-create-dictionary dict))
	(ov (srecode-dictionary-lookup-name dict name)))
    (srecode-dictionary-set-value dict name (append ov (list new)))
    ;; Return the new sub-dictionary.
    new))

(defmethod srecode-dictionary-show-section ((dict srecode-dictionary) name)
  "In dictionary DICT, indicate that the section NAME should be exposed."
  ;; Showing a section is just like making a section dictionary, but
  ;; with no dictionary values to add.
  (srecode-dictionary-add-section-dictionary dict name)
  nil)

(defmethod srecode-dictionary-lookup-name ((dict srecode-dictionary)
					   name)
  "Return information about the current templates value for NAME."
  (if (not (slot-boundp dict 'namehash))
      nil
    ;; Get the value of this name from the dictionary
    (or (with-slots (namehash) dict
	  (gethash name namehash))
	(and (oref dict parent)
	     (srecode-dictionary-lookup-name (oref dict parent) name))
	)))

;;; COMPOUND VALUE METHODS
;;
;; Compound values must provide at least the toStriong method
;; for use in converting the compound value into sometehing insertable.

(defmethod srecode-compound-toString((cp srecode-dictionary-compound-value)
				     dictionary)
  "Convert the compound dictionary value CP to a string."
  (object-name cp))

(defmethod srecode-dump ((cp srecode-dictionary-compound-value)
			 &optional indent)
  "Display information about this compound value."
  (object-name cp)
  )


;;; DUMP DICTIONARY
;;
;; Make a dictionary, and dump it's contents.
(defun srecode-dictionary-dump ()
  "Dump a typical fabricated dictionary."
  (interactive)
  (let ((dict (srecode-create-dictionary (current-buffer))))
    (with-output-to-temp-buffer "*SRECODE DUMP*"
      (srecode-dump dict))
    ))

(defmethod srecode-dump ((dict srecode-dictionary) &optional indent)
  "Dump a dictionary."
  (if indent
      (progn
	(princ (make-string " " indent))
	(princ "SUBDICTIONARY ")
	(princ (object-name dict)))
    (princ "DICTIONARY FOR ")
    (princ major-mode)
    (princ "\n--------------------------------------------\n"))
  (princ "\n")
  (maphash (lambda (key entry)
	     (princ " ")
	     (princ key)
	     (princ " ")
	     (cond ((srecode-dictionary-p entry)
		    (srecode-dump entry (if indent
					    (+ indent 4)
					  4)))
		   (t
		    (prin1 entry)))
	     (princ "\n")
	     )
	   (oref dict namehash))
  )

(provide 'srecode-dictionary)
;;; srecode-dictionary.el ends here

