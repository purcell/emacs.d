;;; srecode-insert --- Insert srecode templates to an output stream.

;;; Copyright (C) 2005, 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: srecode-insert.el,v 1.6 2007/03/19 02:37:47 zappo Exp $

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Define and implements specific inserter objects.
;;
;; Manage the insertion process for a template.
;;

(require 'srecode-compile)
(require 'srecode-find)
(require 'srecode-dictionary)

;;; Code:

;;; INSERTION COMMANDS
;;
;; User level commands for inserting stuff.
;;;###autoload
(defun srecode-insert (template-name)
  "Inesrt the template TEMPLATE-NAME into the current buffer at point."
  (interactive (list (srecode-read-template-name "Template Name: ")))
  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))
  (let ((newdict (srecode-create-dictionary))
	(temp (srecode-template-get-table (srecode-table) template-name)))
    (if (not temp)
	(error "No Template named %s" template-name))
    (srecode-resolve-arguments temp newdict)
    (srecode-insert-fcn temp newdict)
    ))

;;;###autoload
(defun srecode-insert-fcn (template dictionary &optional stream)
  "Insert TEMPLATE using DICTIONARY into STREAM."
  (let ((standard-output (or stream (current-buffer))))
    (srecode-insert-method template dictionary)
    ;; Handle specialization of the POINT inserter.
    (when (and (bufferp standard-output)
	       (slot-boundp 'srecode-template-inserter-point 'point))
      (set-buffer standard-output)
      (goto-char  (oref 'srecode-template-inserter-point point)))
    (oset-default 'srecode-template-inserter-point point eieio-unbound))
  )

;;; TEMPLATE ARGUMENTS
;;
;; Some templates have arguments.  Each argument is assocaited with
;; a function that can resolve the inputs needed.
(defun srecode-resolve-arguments (temp dict)
  "Resolve all the arguments needed by the template TEMP.
Apply anything learned to the dictionary DICT."
  (let ((args (oref temp args))
	(fcn nil)
	)
    (while args
      (setq fcn (intern-soft (concat "srecode-semantic-handle-"
				     (symbol-name (car args)))))
      (if (not fcn)
	  (error "Error resolving template argument %S" (car args)))
      (funcall fcn dict)
      (setq args (cdr args)))
    ))

;;; INSERTION METHODS
;;
;; Code managing the top-level insert command.  The insert function
;; 
(defmethod srecode-push ((st srecode-template))
  "Push the srecoder template ST onto the active stack."
  (oset st active (cons st (oref st active))))

(defmethod srecode-pop :STATIC ((st srecode-template))
  "Pop the srecoder template ST onto the active stack.
ST can be a class, or an object."
  (oset st active (cdr (oref st active))))

(defmethod srecode-peek :STATIC ((st srecode-template))
  "Fetch the topmost active template record.  ST can be a class."
  (car (oref st active)))

(defmethod srecode-insert-method ((st srecode-template) dictionary)
  "Insert the srecoder template ST."
  (unwind-protect
      (let ((c (oref st code)))
	(srecode-push st)
	(while c
	  (cond ((stringp (car c))
		 (princ (car c)))
		(t
		 (srecode-insert-method (car c) dictionary)))
	  (setq c (cdr c))))
    ;; Poping the stack is protected
    (srecode-pop st)))

;;; INSERTERS
;;
;; Specific srecode inserters.
;; The base class is from srecode-compile.
;;
;; Each inserter handles various macro codes from the temlate.
;; The `code' slot specifies a character used to identify which
;; inserter is to be created.
;;
(defclass srecode-template-inserter-newline (srecode-template-inserter)
  ((key :initform "\n"
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style."))
  "Insert a newline, and possibly do indenting.")

(defmethod srecode-insert-method ((sti srecode-template-inserter-newline)
				  dictionary)
  "Insert the STI inserter."
  ;; To be safe, indent the previous line since the template will
  ;; change what is there to indent
  (let ((i (srecode-dictionary-lookup-name dictionary "INDENT"))
	(pm (point-marker)))
    (when (eq i t)
      (indent-according-to-mode)
      (goto-char pm))
    (insert "\n")
    ;; Indent after the newline, particularly for numeric indents.
    (cond ((eq i t)
	   (indent-according-to-mode))
	  ((numberp i)
	   (insert (make-string i " ")))
	  ((stringp i)
	   (insert i)))))

(defclass srecode-template-inserter-variable (srecode-template-inserter)
  ((key :initform nil
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style."))
  "Insert the value of some variable with :object-name.")

(defvar srecode-inserter-variable-current-dictionary nil
  "The active dictionary when calling a variable filter.")

(defmethod srecode-insert-method ((sti srecode-template-inserter-variable)
				  dictionary)
  "Insert the STI inserter."
  ;; Convert the name into a name/fcn pair
  (let* ((name (oref sti :object-name))
	 (fcnpart (if (oref sti :secondname)
		      (read (oref sti :secondname))
		    nil))
	 (val (srecode-dictionary-lookup-name 
	       dictionary name))
	 (ans nil))
    ;; Alert if a macro wasn't found.
    (if (not val)
	(error "Macro %S was not found in the dictionary." name))
    ;; If there was a functional part, call that function.
    (cond ;; Strings
       ((stringp val)
	(if fcnpart
	    (let ((srecode-inserter-variable-current-dictionary dictionary))
	      (setq val (funcall fcnpart val)))))
       ;; Compound data value
       ((srecode-dictionary-compound-value-child-p val)
	;; Methods should accept a dictionary to look stuff up in.
	(if fcnpart
	    (progn
	      (if (not (fboundp fcnpart))
		  ;; May be a short-form name.  Prefix it.
		  (let ((nfcn (concat "srecode-compound-" (symbol-name fcnpart))))
		    (setq fcnpart (read nfcn))))
	      (setq val (funcall fcnpart val dictionary)))
	  ;; use always available toString...
	  (setq val (srecode-compound-toString val dictionary)))
	)
       ;; Dictionaries... not allowed in this style
       ((srecode-dictionary-child-p val)
	(error "Macro %s cannot insert a dictionary.  Use section macros instead."
	       name))
       ;; Other stuff... convert
       (t
	(if (and val (not (stringp val)))
	    (setq val (format "%S" val)))))
    ;; Output the dumb thing
    (princ val)))

(defclass srecode-template-inserter-comment (srecode-template-inserter-variable)
  ((key :initform ?!
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   )
  "Insert the value of some variable with :object-name.
If this object isn't in the dictionary, ask the user what it should be.")

(defmethod srecode-insert-method ((sti srecode-template-inserter-comment)
				  dictionary)
  "Don't insert anything for comment macros in STI."
  nil)


(defclass srecode-template-inserter-ask (srecode-template-inserter-variable)
  ((key :initform ??
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   (prompt :initarg :prompt
	   :initform nil
	   :documentation
	   "The prompt used to query for this dictionary value.")
   (defaultfcn :initarg :defaultfcn
	       :initform nil
	       :documentation
	       "The function which can calculate a default value.")
   )
  "Insert the value of some variable with :object-name.
If this object isn't in the dictionary, ask the user what it should be.")

(defmethod srecode-inserter-apply-state ((ins srecode-template-inserter-ask) STATE)
  "For the template inserter INS, apply information from STATE.
Loop over the prompts to see if we have a match."
  (let ((prompts (oref STATE prompts))
	(ans nil))
    (while prompts
      (when (string= (semantic-tag-name (car prompts))
		     (oref ins :object-name))
	(oset ins :prompt (semantic-tag-get-attribute (car prompts)
						      :text))
	(oset ins :defaultfcn (semantic-tag-get-attribute (car prompts)
							  :default))
	)
      (setq prompts (cdr prompts)))
    ))

(defmethod srecode-insert-method ((sti srecode-template-inserter-ask)
				  dictionary)
  "Insert the STI inserter."
  (let ((val (srecode-dictionary-lookup-name 
	      dictionary (oref sti :object-name))))
    (if val
	;; Does some extra work.  Oh well.
	(call-next-method)
      (let ((prompt (or (oref sti prompt)
			(format "Specify %s: "
				     (oref sti :object-name))))
	    ;; Add default value fcn here
	    )
	(setq val (read-string prompt)))
      ;; After asking, save in the dictionary so that
      ;; the user can use the same name again later.
      (srecode-dictionary-set-value 
       dictionary (oref sti :object-name) val)
      (call-next-method))))

(defmethod srecode-dump ((ins srecode-template-inserter-ask) indent)
  "Dump the state of the SRecode template inserter INS."
  (call-next-method)
  (princ " : \"")
  (princ (oref ins prompt))
  (princ "\"")
  )

(defclass srecode-template-inserter-point (srecode-template-inserter)
  ((key :initform ?^
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   (point :type (or null marker)
	  :allocation :class
	  :documentation
	  "Record the value of (point) in this class slot.
It is the responsibility of the inserter algorithm to clear this
after a successful insertion."))
  "Record the value of (point) when inserted.")

(defmethod srecode-insert-method ((sti srecode-template-inserter-point)
				  dictionary)
  "Insert the STI inserter.
Save point in the class allocated 'point' slot."
  (oset sti point (point-marker))
  )

(defclass srecode-template-inserter-subtemplate (srecode-template-inserter)
  ((template :initarg :template
	     :documentation
	     "A Template used to frame the codes from this inserter.")
   )
  "All template segments between the secion-start and section-end
are treated specially."
  :abstract t)

(defmethod srecode-insert-subtemplate ((sti srecode-template-inserter-subtemplate)
				       dict)
  "Insert a subtemplate for the inserter STI with dictionary DICT."
  ;; make sure that only dictionaries are used.
  (when (not (srecode-dictionary-child-p dict))
    (error "Only section dictionaries allowed for %s" 
	   (object-name-string sti)))
  ;; Output the code from the sub-template.
  (srecode-insert-method (oref sti template) dict)
  )

(defmethod srecode-insert-method ((sti srecode-template-inserter-subtemplate)
				  dictionary)
  "Insert the STI inserter.
Loops over the embedded CODE which was saved here during compilation."
  (let ((dicts (srecode-dictionary-lookup-name 
		dictionary (oref sti :object-name))))
    ;; If there is no section dictionary, then don't output anything
    ;; from this section.
    (while dicts
      (srecode-insert-subtemplate sti (car dicts))
      (setq dicts (cdr dicts)))))

(defclass srecode-template-inserter-section-start (srecode-template-inserter-subtemplate)
  ((key :initform ?#
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   )
  "All template segments between the secion-start and section-end
are treated specially.")

(defmethod srecode-parse-input ((ins srecode-template-inserter-section-start)
				tag input STATE)
  "For the section inserter INS, parse INPUT.
Shorten input until the END token is found.
Return the remains of INPUT."
  (let* ((escape_start (oref STATE escape_start))
	 (escape_end (oref STATE escape_end))
	 (out (srecode-compile-split-code tag input STATE
					  (oref ins :object-name))))
    (oset ins template (srecode-template 
			(object-name-string ins)
			:context nil
			:args nil
			:code (cdr out)))
    (car out)))

(defmethod srecode-dump ((ins srecode-template-inserter-section-start) indent)
  "Dump the state of the SRecode template inserter INS."
  (call-next-method)
  (princ "\n")
  (srecode-dump-code-list (oref (oref ins template) code)
			  (concat indent "    "))
  )

(defclass srecode-template-inserter-section-end (srecode-template-inserter)
  ((key :initform ?/
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   )
  "All template segments between the secion-start and section-end
are treated specially.")

(defmethod srecode-insert-method ((sti srecode-template-inserter-section-end)
				  dictionary)
  "Insert the STI inserter."
  )

(defmethod srecode-match-end ((ins srecode-template-inserter-section-end) name)
			      
  "For the template inserter INS, do I end a section called NAME?"
  (string= name (oref ins :object-name)))

(defclass srecode-template-inserter-include (srecode-template-inserter-subtemplate)
  ((key :initform ?>
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   )
  "This macro will bring in an exapansion of a different template.")

(defmethod srecode-insert-method ((sti srecode-template-inserter-include)
				  dictionary)
  "Insert the STI inserter.
Finds the template with this macro function part, and inserts it
with the dictionaries found in the dictinary."
  (let* ((templatenamepart (oref sti :secondname))
	 (ans nil))
    ;; If there was no template name, throw an error
    (if (not templatenamepart)
	(error "Include macro %s needs a template name." (oref sti :object-name)))
    ;; Find the template by name, and save it.
    (if (or (not (slot-boundp sti 'template))
	    (not (oref sti template)))
	(let ((tmpl (srecode-template-get-table (srecode-table)
						templatenamepart))
	      (active (oref srecode-template active))
	      ctxt)
	  ;; If it isn't just available, scan back through
	  ;; the active tempalte stack, searching for a matching
	  ;; context.
	  (while (and (not tmpl) active)
	    (setq ctxt (oref (car active) context))
	    (setq tmpl (srecode-template-get-table (srecode-table)
						   templatenamepart
						   ctxt))
	    (setq active (cdr active)))
	  (oset sti :template tmpl)))

    (if (not (oref sti template))
	(error "No template %s found for include %s"
	       templatenamepart (oref sti :object-name)))
    ;; Insert the template.
    ;; Our baseclass has a simple way to do this.
    (if (srecode-dictionary-lookup-name dictionary (oref sti :object-name))
	;; If we have a value, then call the next method
	(call-next-method)
      ;; If we don't have a special dictitonary, then just insert with the
      ;; current dictionary.
      (srecode-insert-subtemplate sti dictionary))
    ))

(provide 'srecode-insert)

;;; srecode-insert.el ends here
