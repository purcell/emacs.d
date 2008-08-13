;;; srecode-insert --- Insert srecode templates to an output stream.

;;; Copyright (C) 2005, 2007, 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: srecode-insert.el,v 1.20 2008/06/19 02:21:12 zappo Exp $

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
(defvar srecode-insertion-start-context nil
  "The context that was at point at the beginning of the template insertion.")

;;;###autoload
(defun srecode-insert-again ()
  "Insert the previously inserted template (by name) again."
  (interactive)
  (let ((prev (car srecode-read-template-name-history)))
    (if prev
	(srecode-insert prev)
      (call-interactively 'srecode-insert))))

;;;###autoload
(defun srecode-insert (template-name &rest dict-entries)
  "Inesrt the template TEMPLATE-NAME into the current buffer at point.
DICT-ENTRIES are additional dictionary values to add."
  (interactive (list (srecode-read-template-name "Template Name: ")))
  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))
  (let ((newdict (srecode-create-dictionary))
	(temp (srecode-template-get-table (srecode-table) template-name))
	(srecode-insertion-start-context (srecode-calculate-context))
	)
    (if (not temp)
	(error "No Template named %s" template-name))
    (while dict-entries
      (srecode-dictionary-set-value newdict
				    (car dict-entries)
				    (car (cdr dict-entries)))
      (setq dict-entries (cdr (cdr dict-entries))))
    ;;(srecode-resolve-arguments temp newdict)
    (srecode-insert-fcn temp newdict)
    ;; Don't put code here.  We need to return the end-mark
    ;; for this insertion step.
    ))

;;;###autoload
(defun srecode-insert-fcn (template dictionary &optional stream)
  "Insert TEMPLATE using DICTIONARY into STREAM."
  ;; Perform the insertion.
  (let ((standard-output (or stream (current-buffer)))
	(end-mark nil))
    ;; Make sure the semantic tags are up to date.
    (semantic-fetch-tags)
    ;; Resolve the arguments
    (srecode-resolve-arguments template dictionary)
    ;; Insert
    (srecode-insert-method template dictionary)
    ;; Handle specialization of the POINT inserter.
    (when (and (bufferp standard-output)
	       (slot-boundp 'srecode-template-inserter-point 'point)
	       )
      (set-buffer standard-output)
      (setq end-mark (point-marker))
      (goto-char  (oref srecode-template-inserter-point point)))
    (oset-default 'srecode-template-inserter-point point eieio-unbound)
    (or end-mark (point)))
  )

;;; TEMPLATE ARGUMENTS
;;
;; Some templates have arguments.  Each argument is assocaited with
;; a function that can resolve the inputs needed.
(defun srecode-resolve-arguments (temp dict)
  "Resolve all the arguments needed by the template TEMP.
Apply anything learned to the dictionary DICT."
  (srecode-resolve-argument-list (oref temp args) dict temp))

(defun srecode-resolve-argument-list (args dict &optional temp)
  "Resolve arguments in the argument list ARGS.
Apply values to DICT.
Optional argument TEMP is the template that is getting it's arguments resolved."
  (let ((fcn nil))
    (while args
      (setq fcn (intern-soft (concat "srecode-semantic-handle-"
				     (symbol-name (car args)))))
      (if (not fcn)
	  (error "Error resolving template argument %S" (car args)))
      (if temp
	  (condition-case nil
	      ;; Allow some to accept a 2nd argument optionally.
	      ;; They throw an error if not available, so try again.
	      (funcall fcn dict temp)
	    (error (funcall fcn dict)))
	(funcall fcn dict))
      (setq args (cdr args)))
    ))

;;; INSERTION STACK & METHOD
;;
;; Code managing the top-level insert method and the current
;; insertion stack.
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
  ;; Merge any template entries into the input dictionary.
  (when (slot-boundp st 'dictionary)
    (srecode-dictionary-merge dictionary (oref st dictionary)))
  ;; Do an insertion.
  (unwind-protect
      (let ((c (oref st code)))
	(srecode-push st)
	(srecode-insert-code-stream c dictionary))
    ;; Poping the stack is protected
    (srecode-pop st)))

(defun srecode-insert-code-stream (code dictionary)
  "Insert the CODE from a template into `standard-output'.
Use DICTIONARY to resolve any macros."
  (while code
    (cond ((stringp (car code))
	   (princ (car code)))
	  (t
	   (srecode-insert-method (car code) dictionary)))
    (setq code (cdr code))))

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
	"The character code used to identify inserters of this style.")
   (hard :initform nil
	 :initarg :hard
	 :documentation
	 "Is this a hard newline (always inserted) or optional?
Optional newlines don't insert themselves if they are on a blank line
by themselves.")
   )
  "Insert a newline, and possibly do indenting.")

(defmethod srecode-insert-method ((sti srecode-template-inserter-newline)
				  dictionary)
  "Insert the STI inserter."
  ;; To be safe, indent the previous line since the template will
  ;; change what is there to indent
  (let ((i (srecode-dictionary-lookup-name dictionary "INDENT"))
	(inbuff (bufferp standard-output))
	(doit t)
	(pm (point-marker)))
    (when (and inbuff (not (oref sti hard)))
      ;; If this is not a hard newline, we need do the calculation
      ;; and set "doit" to nil.
      (beginning-of-line)
      (save-restriction
	(narrow-to-region (point) pm)
	(when (looking-at "\\s-*$")
	  (setq doit nil)))
      (goto-char pm)
      )
    ;; Do indentation reguardless of the newline.
    (when (and (eq i t) inbuff)
      (indent-according-to-mode)
      (goto-char pm))

    (when doit
      (princ "\n")
      ;; Indent after the newline, particularly for numeric indents.
      (cond ((and (eq i t) (bufferp standard-output))
	     ;; WARNING - indent according to mode requires that standard-output
	     ;;           is a buffer!
	     ;; @todo - how to indent in a string???
	     (setq pm (point-marker))
	     (indent-according-to-mode)
	     (goto-char pm))
	    ((numberp i)
	     (princ (make-string i " ")))
	    ((stringp i)
	     (princ i))))))

(defmethod srecode-dump ((ins srecode-template-inserter-newline) indent)
  "Dump the state of the SRecode template inserter INS."
  (call-next-method)
  (when (oref ins hard)
    (princ " : hard")
    ))

(defclass srecode-template-inserter-blank (srecode-template-inserter)
   ((key :initform "\r"
	 :allocation :class
	 :documentation
	 "The character represeinting this inserter style.
Can't be blank, or it might be used by regular variable insertion.")
    (where :initform 'begin
	   :initarg :where
	   :documentation
	   "This should be 'begin or 'end, indicating where to insrt a CR.
When set to 'begin, it will insert a CR if we are not at 'bol'.
When set to 'end it will insert a CR if we are not at 'eol'")
    ;; @TODO - Add slot and control for the number of blank
    ;;         lines before and after point.
   )
   "Insert a newline before and after a template, and possibly do indenting.")

(defmethod srecode-insert-method ((sti srecode-template-inserter-blank)
				  dictionary)
  "Make sure there is no text before or after point."
  (let ((i (srecode-dictionary-lookup-name dictionary "INDENT"))
	(inbuff (bufferp standard-output))
	(pm (point-marker)))
    (when (and inbuff 
	       ;; Don't do this if we are not the active template.
	       (= (length (oref srecode-template active)) 1))

      (when (and (eq i t) inbuff (not (eq (oref sti where) 'begin)))
	(indent-according-to-mode)
	(goto-char pm))
      
      (cond ((and (eq (oref sti where) 'begin) (not (bolp)))
	     (princ "\n"))
	    ((and (eq (oref sti where) 'end) (not (eolp)))
	     (princ "\n"))
	    )
      (setq pm (point-marker))
      (when (and (eq i t) inbuff (not (eq (oref sti where) 'end)))
	(indent-according-to-mode)
	(goto-char pm))
      )))

(defclass srecode-template-inserter-comment (srecode-template-inserter)
  ((key :initform ?!
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   )
  "Allow comments within template coding.
This inserts nothing.")

(defmethod srecode-insert-method ((sti srecode-template-inserter-comment)
				  dictionary)
  "Don't insert anything for comment macros in STI."
  nil)


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
    (when (not val)
      (message "Warning: macro %S was not found in the dictionary." name)
      (setq val ""))
    ;; If there was a functional part, call that function.
    (cond ;; Strings
       ((stringp val)
	(if fcnpart
	    (let ((srecode-inserter-variable-current-dictionary dictionary))
	      (setq val (funcall fcnpart val)))))
       ;; Compound data value
       ((srecode-dictionary-compound-value-child-p val)
	(setq val (srecode-compound-toString val fcnpart dictionary))
	(if (not val) (setq val ""))
	)
       ;; Dictionaries... not allowed in this style
       ((srecode-dictionary-child-p val)
	(error "Macro %s cannot insert a dictionary.  Use section macros instead."
	       name))
       ;; Other stuff... convert
       (t
	(error "Macro %s cannot insert arbitrary data." name)
	;;(if (and val (not (stringp val)))
	;;    (setq val (format "%S" val))))
	))
    ;; Output the dumb thing
    (princ val)))

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
   (read-fcn :initarg :read-fcn
	     :initform 'read-string
	     :documentation
	     "The function used to read in the text for this prompt.")
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
	(oset ins :prompt 
	      (semantic-tag-get-attribute (car prompts) :text))
	(oset ins :defaultfcn
	      (semantic-tag-get-attribute (car prompts) :default))
	(oset ins :read-fcn
	      (or (semantic-tag-get-attribute (car prompts) :read)
		  'read-string))
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
      (let* ((prompt (oref sti prompt))
	     (defaultfcn (oref sti :defaultfcn))
	     (default (cond ((stringp defaultfcn)
			     defaultfcn)
			    ((functionp defaultfcn)
			     (funcall defaultfcn))
			    ((and (listp defaultfcn)
				  (eq (car defaultfcn) 'macro))
			      (srecode-dictionary-lookup-name
			       dictionary (cdr defaultfcn)))
			    ((null defaultfcn)
			     "")
			    (t
			     (error "Unknown default for prompt: %S"
				    defaultfcn))))
	     (reader (oref sti :read-fcn))
	     )
	(cond ((eq reader 'y-or-n-p)
	       (if (y-or-n-p (or prompt
				 (format "%s? "
					 (oref sti :object-name))))
		   (setq val default)
		 (setq val "")))
	      ((eq reader 'read-char)
	       (setq val (format
			  "%c"
			  (read-char (or prompt
					 (format "Char for %s: "
						 (oref sti :object-name))))))
	       )
	      (t
	       (save-excursion
		 (setq val (funcall reader
				    (or prompt
					(format "Specify %s: "
						(oref sti :object-name)))
				    default
				    )))))
	)
      ;; After asking, save in the dictionary so that
      ;; the user can use the same name again later.
      (srecode-dictionary-set-value 
       (srecode-root-dictionary dictionary)
       (oref sti :object-name) val)
      ;; Now that this value is safely stowed in the dictionary,
      ;; we can do what regular inserters do.
      (call-next-method))))

(defmethod srecode-dump ((ins srecode-template-inserter-ask) indent)
  "Dump the state of the SRecode template inserter INS."
  (call-next-method)
  (princ " : \"")
  (princ (oref ins prompt))
  (princ "\"")
  )

(defvar srecode-template-inserter-point-override nil
  "When non-nil, the point inserter will do this functin instead.")

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
Save point in the class allocated 'point' slot.
If `srecode-template-inserter-point-override' then this generalized
marker will do something else.  See `srecode-template-inserter-include-wrap'
as an example."
  (if srecode-template-inserter-point-override
      ;; Disable the old override while we do this.
      (let ((over srecode-template-inserter-point-override)
	    (srecode-template-inserter-point-override nil))
	(funcall over dictionary)
	)
    (oset sti point (point-marker))
    ))

(defclass srecode-template-inserter-subtemplate (srecode-template-inserter)
  ()
  "All template segments between the secion-start and section-end
are treated specially."
  :abstract t)

(defmethod srecode-insert-subtemplate ((sti srecode-template-inserter-subtemplate)
				       dict slot)
  "Insert a subtemplate for the inserter STI with dictionary DICT."
  ;; make sure that only dictionaries are used.
  (when (not (srecode-dictionary-child-p dict))
    (error "Only section dictionaries allowed for %s" 
	   (object-name-string sti)))
  ;; Output the code from the sub-template.
  (srecode-insert-method (slot-value sti slot) dict)
  )

(defmethod srecode-insert-method-helper ((sti srecode-template-inserter-subtemplate)
					 dictionary slot)
  "Do the work for inserting the STI inserter.
Loops over the embedded CODE which was saved here during compilation.
The template to insert is stored in SLOT."
  (let ((dicts (srecode-dictionary-lookup-name 
		dictionary (oref sti :object-name))))
    ;; If there is no section dictionary, then don't output anything
    ;; from this section.
    (while dicts
      (srecode-insert-subtemplate sti (car dicts) slot)
      (setq dicts (cdr dicts)))))

(defmethod srecode-insert-method ((sti srecode-template-inserter-subtemplate)
				  dictionary)
  "Insert the STI inserter.
Calls back to `srecode-insert-method-helper' for this class."
  (srecode-insert-method-helper sti dictionary 'template))


(defclass srecode-template-inserter-section-start (srecode-template-inserter-subtemplate)
  ((key :initform ?#
	:allocation :class
	:documentation
	"The character code used to identify inserters of this style.")
   (template :initarg :template
	     :documentation
	     "A Template used to frame the codes from this inserter.")
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
   (includedtemplate 
    :initarg :includedtemplate
    :documentation
    "The template included for this inserter."))
  "This macro will bring in an exapansion of a different template.")

(defmethod srecode-insert-include-lookup ((sti srecode-template-inserter-include)
					  dictionary)
  "For the template inserter STI, lookup the template to include.
Finds the template with this macro function part and stores it in
this template instance."
  (let* ((templatenamepart (oref sti :secondname))
	 (ans nil))
    ;; If there was no template name, throw an error
    (if (not templatenamepart)
	(error "Include macro %s needs a template name." (oref sti :object-name)))
    ;; Find the template by name, and save it.
    (if (or (not (slot-boundp sti 'includedtemplate))
	    (not (oref sti includedtemplate)))
	(let ((tmpl (srecode-template-get-table (srecode-table)
						templatenamepart))
	      (active (oref srecode-template active))
	      ctxt)
	  (when (not tmpl)
	    ;; If it isn't just available, scan back through
	    ;; the active template stack, searching for a matching
	    ;; context.
	    (while (and (not tmpl) active)
	      (setq ctxt (oref (car active) context))
	      (setq tmpl (srecode-template-get-table (srecode-table)
						     templatenamepart
						     ctxt))
	      (when (not tmpl)
		(when (slot-boundp (car active) 'table)
		  (let ((app (oref (oref (car active) table) application)))
		    (when app
		      (setq tmpl (srecode-template-get-table 
				  (srecode-table)
				  templatenamepart
				  ctxt app)))
		    )))
	      (setq active (cdr active)))
	    (when (not tmpl)
	      ;; If it wasn't in this context, look to see if it
	      ;; defines it's own context
	      (setq tmpl (srecode-template-get-table (srecode-table)
						     templatenamepart)))
	    )
	  (oset sti :includedtemplate tmpl)))

    (if (not (oref sti includedtemplate))
	;; @todo - Call into a debugger to help find the template in question.
	(error "No template \"%s\" found for include macro `%s'"
	       templatenamepart (oref sti :object-name)))
    ))

(defmethod srecode-insert-method ((sti srecode-template-inserter-include)
				  dictionary)
  "Insert the STI inserter.
Finds the template with this macro function part, and inserts it
with the dictionaries found in the dictinary."
  (srecode-insert-include-lookup sti dictionary)
  ;; Insert the template.
  ;; Our baseclass has a simple way to do this.
  (if (srecode-dictionary-lookup-name dictionary (oref sti :object-name))
      ;; If we have a value, then call the next method
      (srecode-insert-method-helper sti dictionary 'includedtemplate)
    ;; If we don't have a special dictitonary, then just insert with the
    ;; current dictionary.
    (srecode-insert-subtemplate sti dictionary 'includedtemplate))
  )

;;
;; This template combines the include template and the sectional template.
;; It will first insert the included template, then insert the embedded
;; template wherever the $^$ in the included template was.
;;
;; Since it uses dual inheretance, it will magically get the end-matching
;; behavior of #, with the including feature of >.
;;
(defclass srecode-template-inserter-include-wrap (srecode-template-inserter-include srecode-template-inserter-section-start)
   ((key :initform ?<
	 :allocation :class
	 :documentation
	 "The character code used to identify inserters of this style.")
    )
   "Class srecode-template-inserter-include-wrap ")

(defmethod srecode-insert-method ((sti srecode-template-inserter-include-wrap)
				  dictionary)
  "Insert the template STI.
This will first insert the include part via inheritance, then
insert the section it wraps into the location in the included
template where  a ^ inserter occurs."
  ;; Step 1: Look up the included inserter
  (srecode-insert-include-lookup sti dictionary)
  ;; Step 2: Temporarilly override the point inserter.
  (let* ((vaguely-unique-name sti)
	 (srecode-template-inserter-point-override
	  (lambda (dict2)
	    (if (srecode-dictionary-lookup-name 
		 dict2 (oref vaguely-unique-name :object-name))
		;; Insert our sectional part with looping.
		(srecode-insert-method-helper 
		 vaguely-unique-name dict2 'template)
	      ;; Insert our sectional part just once.
	      (srecode-insert-subtemplate vaguely-unique-name
					  dict2 'template))
	   )))
    ;; Do a regular insertion for an include, but with our override in
    ;; place.
    (call-next-method)
    ))

(provide 'srecode-insert)

;;; srecode-insert.el ends here
