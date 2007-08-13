;;; srecode-compile --- Compilation of srecode template files.

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
;; Compile a Semantic Recoder template file.
;;
;; Template files are parsed using a Semantic/Wisent parser into
;; a tag table.  The code therin is then further parsed down using
;; a regular expression parser.
;;
;; The output are a series of EIEIO objects which represent the
;; templates in a way that could be inserted later.


(require 'eieio)
(require 'eieio-base)
(require 'srecode-table)

;;; Code:

;;; Template Class
;;
;; Templatets describe a patter of text that can be inserted into a
;; buffer.
;;
(defclass srecode-template (eieio-named)
  ((context :initarg :context
	    :initform nil
	    :documentation
	    "Context this template belongs to.")
   (args :initarg :args
	 :documentation
	 "List of arguments that this template requires.")
   (code :initarg :code
	 :documentation
	 "Compiled text from the template.")
   (active :allocation :class
	   :initform nil
	   :documentation
	   "During template insertion, this is the stack of active templates.
The top-most template is the 'active' template.  Use the accessor methods
for push, pop, and peek for the active template.")
   (table :initarg :table
	  :documentation
	  "The table this template lives in.")
   )
  "Class defines storage for semantic recoder templates.")

;;; Inserters
;;
;; Each inserter object manages a different thing that
;; might be inserted into a template output stream.
;;
;; The 'srecode-insert-method' on each inserter does the actual
;; work, and the smaller, simple inserter object is saved in
;; the compiled templates.
;;
;; See srecode-insert.el for the specialized classes.
;;
(defclass srecode-template-inserter (eieio-named)
  ((secondname :initarg :secondname
	       :type (or null string)
	       :documentation
	       "If there is a colon in the inserter's name, it represents 
additional static argument data."))
  "This represents an item to be inserted via a template macro.
Plain text strings are not handled via this baseclass."
  :abstract t)

(defmethod srecode-parse-input ((ins srecode-template-inserter) 
				tag input STATE)
  "For the template inserter INS, parse INPUT.
Shorten input only by the amount needed.
Return the remains of INPUT.
STATE is the current compilation state."
  input)

(defmethod srecode-match-end ((ins srecode-template-inserter) name)
  "For the template inserter INS, do I end a section called NAME?"
  nil)

(defmethod srecode-inserter-apply-state ((ins srecode-template-inserter) STATE)
  "For the template inserter INS, apply information from STATE."
  nil)

;;; Compile State
(defclass srecode-compile-state ()
  ((context :initform "declaration"
	    :documentation "The active context.")
   (prompts :initform nil
	    :documentation "The active prompts.")
   (escape_start :initform "{{"
		 :documentation "The starting escape sequence.")
   (escape_end :initform "}}"
	       :documentation "The ending escape sequence.")
   )
  "Current state of the compile.")

(defmethod srecode-compile-add-prompt ((state srecode-compile-state) 
				       prompttag)
  "Add PROMPTTAG to the current list of prompts."
  (with-slots (prompts) state
      (let ((match (assoc (semantic-tag-name prompttag) prompts)))
	(when match
	  (let ((newprompts nil))
	    (while prompts
	      (when (not (string= (car (car prompts))
				  (car prompttag)))
		(setq newprompts (cons (car prompts)
				       newprompts)))
	      (setq prompts (cdr prompts)))))
	(setq prompts (cons prompttag prompts)))
      ))

;;;  TEMPLATE COMPILER
;;
;;;###autoload
(defun srecode-compile-file (fname)
  "Compile the templates from the file FNAME."
  (let ((peb (get-file-buffer fname)))
    (save-excursion
      ;; Make whatever it is local.
      (if (not peb)
	  (set-buffer (find-file-noselect fname))
	(set-buffer peb))
      ;; Do the compile.
      (srecode-compile-templates)
      ;; Trash the buffer if we had to read it in.
      (if (not peb)
	  (kill-buffer (current-buffer)))
      )))

;;;###autoload
(defun srecode-compile-templates ()
  "Compile a semantic recode template file into a mode-local variable."
  (interactive)
  (require 'srecode-insert)
  (message "Compiling template %s..."
	   (file-name-nondirectory (buffer-file-name)))
  (let ((tags (semantic-fetch-tags))
	(tag nil)
	(class nil)
	(table nil)
	(STATE (srecode-compile-state (file-name-nondirectory
				       (buffer-file-name))))
	(mode nil)
	(application nil)
	(priority nil)
	(vars nil)
	)

    ;;
    ;; COMPILE
    ;;
    (while tags
      (setq tag (car tags)
	    class (semantic-tag-class tag))
      ;; What type of item is it?
      (cond
       ;; CONTEXT tags specify the context all future tags
       ;; belong to.
       ((eq class 'context)
	(oset STATE context (semantic-tag-name tag))
	)

       ;; PROMPT tags specify prompts for dictionary ? inserters
       ;; which appear in the following templates
       ((eq class 'prompt)
	(srecode-compile-add-prompt STATE tag)
	)
	    
       ;; VARIABLE tags can specify operational control
       ((eq class 'variable)
	(let ((name (semantic-tag-name tag))
	      (value (read (semantic-tag-variable-default tag))))
	  (cond ((string= name "mode")
		 (setq mode (intern value)))
		((string= name "escape_start")
		 (oset STATE escape_start value)
		 )
		((string= name "escape_end")
		 (oset STATE escape_end value)
		 )
		((string= name "application")
		 (setq application (read value)))
		((string= name "priority")
		 (setq priority (read value)))
		(t
		 ;; Assign this into some table of variables.
		 (setq vars (cons (cons name value) vars))
		 )))
	)

       ;; FUNCTION tags are really templates.
       ((eq class 'function)
	(setq table (cons (srecode-compile-one-template-tag tag STATE)
			  table))
	)

       ;; Ooops
       (t (error "Unknown TAG class %s" class))
       )
      ;; Continue
      (setq tags (cdr tags)))

    ;; MSG - Before install since nreverse whacks our list.
    (message "%d templates compiled for %s"
	     (length table) mode)

    ;;
    ;; APPLY TO MODE
    ;;
    (if (not mode)
	(error "You must specify a MODE for your templates"))

    ;;
    ;; Calculate priority
    ;; 
    (if (not priority)
	(let ((d (file-name-directory (buffer-file-name)))
	      (sd (file-name-directory (locate-library "srecode")))
	      (defaultdelta (if (eq mode 'default) 20 0)))
	  (if (string= d sd)
	      (setq priority (+ 80 defaultdelta))
	    (setq priority (+ 30 defaultdelta)))
	  (message "Templates %s has estimated priority of %d"
		   (file-name-nondirectory (buffer-file-name))
		   priority))
      (message "Compiling templates %s priority %d... done!"
	       (file-name-nondirectory (buffer-file-name))
	       priority))

    ;; Save it up!
    (srecode-compile-template-table table mode priority application vars)
    )
)

(defun srecode-compile-one-template-tag (tag STATE)
  "Compile a template tag TAG into an srecode template class.
STATE is the current compile state as an `srecode-compile-state' object."
  (let* ((context (oref STATE context))
	 (prompts (oref STATE prompts))
	 (escape_start (oref STATE escape_start))
	 (escape_end (oref STATE escape_end))
	 (code  (srecode-compile-split-code
		 tag (semantic-tag-get-attribute tag :code)
		 STATE))
	 (args (semantic-tag-function-arguments tag))
	 (addargs nil))
    (message "Compiled %s to %d codes with %d args and %d prompts."
	     (semantic-tag-name tag)
	     (length code)
	     (length args)
	     (length prompts))
    (while args
      (setq addargs (cons (intern (car args)) addargs))
      (setq args (cdr args)))
    (srecode-template (semantic-tag-name tag)
		      :context context
		      :args (nreverse addargs)
		      :code (cdr code))
    ))

(defun srecode-compile-split-code (tag str STATE
				       &optional end-name)
  "Split the code for TAG into something templatable.
STR is the string of code from TAG to split.
STATE is the current compile state.
ESCAPE_START and ESCAPE_END are regexps that indicate the beginning
escape character, and end escape character pattern for expandable
macro names.
Optional argument END-NAME specifies the name of a token upon which
parsing should stop.
If END-NAME is specified, and the input string"
  (let* ((what str)
	 (end-token nil)
	 (comp nil)
	 (regex (concat "\n\\|" (oref STATE escape_start)))
	 (tmp nil)
	 )
    (while (and what (not end-token))
      (cond
       ((string-match regex what)
	(let* ((prefix (substring what 0 (match-beginning 0)))
	       (match (substring what
				 (match-beginning 0)
				 (match-end 0)))
	       (namestart (match-end 0))
	       (junk (string-match (oref STATE escape_end) what namestart))
	       end tail name key)
	  ;; Add string to compiled output
	  (setq comp (cons prefix comp))
	  (if (string= match "\n")
	      ;; Do newline thingy.
	      (let ((new-inserter (srecode-compile-inserter
				   "INDENT"
				   "\n"
				   STATE
				   :secondname nil)))
		;; Trim WHAT back.
		(setq what (substring what namestart))
		(when (> (length what) 0)
		  ;; make the new inserter, but only if we aren't last.
		  (setq comp (cons new-inserter comp))
		  ))
	    ;; Regular inserter thingy.
	    (setq end (if junk
			  (match-beginning 0)
			(error "Could not find end escape for %s"
			       (semantic-tag-name tag)))
		  tail (match-end 0))
	    (cond ((not end)
		   (error "No matching escape end for %s"
			  (semantic-tag-name tag)))
		  ((<= end namestart)
		   (error "Stray end escape for %s"
			  (semantic-tag-name tag)))
		  )
	    ;; Add string to compiled output
	    (setq key (aref what namestart))
	    (if (and (or (< key ?A) (> key ?Z))
		     (or (< key ?a) (> key ?z)) )
		(setq name (substring what (1+ namestart) end))
	      (setq name (substring what namestart end)
		    key nil))
	    ;; Trim WHAT back.
	    (setq what (substring what tail))
	    ;; Add new inserted to compiled output
	    (let* ((junk (string-match ":" name))
		   (namepart (if junk
				 (substring name 0 (match-beginning 0))
			       name))
		   (secondname (if junk
				   (substring name (match-end 0))
				 nil)))
	      (let ((new-inserter (srecode-compile-inserter
				   namepart key STATE
				   :secondname secondname
				   )))
		(if (srecode-match-end new-inserter end-name)
		    (setq end-token new-inserter))
		;; Add the inserter to our compilation stream.
		(setq comp (cons new-inserter comp))
		;; Allow the inserter an opportunity to modify
		;; the input stream.
		(setq what (srecode-parse-input new-inserter tag what
						STATE))
		))
	    )))
       (t
	(if end-name
	    (error "Unmatched section end %s" end-name))
	(setq comp (cons what comp)
	      what nil))))
    (cons what (nreverse comp))))

(defun srecode-compile-inserter (name key STATE &rest props)
  "Create an srecode inserter object for some macro NAME.
KEY indicates a single character key representing a type
of inserter to create.
STATE is the current compile state.
PROPS are additional properties that might need to be passed
to the inserter constructor."
  ;;(message "Compile: %s %S" name props)
  (if (not key)
      (apply 'srecode-template-inserter-variable name props)
    (let ((classes (class-children srecode-template-inserter))
	  (new nil))
      ;; Loop over the various subclasses and
      ;; create the correct inserter.
      (while (and (not new) classes)
	(setq classes (append classes (class-children (car classes))))
	;; Do we have a match?
	(when (and (not (class-abstract-p (car classes)))
		   (equal (oref (car classes) key) key))
	  ;; Create the new class, and apply state.
	  (setq new (apply (car classes) name props))
	  (srecode-inserter-apply-state new STATE)
	  )
	(setq classes (cdr classes)))
      (if (not new) (error "SRECODE: Unknown macro code %S" key))
      new)))

(defun srecode-compile-template-table (templates mode priority application vars)
  "Compile a list of TEMPLATES into an semantic recode table.
The table being compiled is for MODE, or the string \"default\".
PRIORITY is a numerical value that indicates this tables location
in an ordered search.
APPLICATION is the name of the application these templates belong to.
A list of defined variables VARS provides a variable table."
  (let ((namehash (make-hash-table :test 'equal
				   :size (length templates)))
	(contexthash (make-hash-table :test 'equal :size 10))
	(lp templates)
	(name nil))
    
    (while lp
      
      (let* ((objname (oref (car lp) :object-name))
	     (context (oref (car lp) :context))
	     (globalname (concat context ":" objname))
	     )

	;; Place this template object into the global name hash.
	(puthash globalname (car lp) namehash)

	;; Place this template into the specific context name hash.
	(let ((hs (gethash context contexthash)))
	  ;; Make a new context if none was available.
	  (when (not hs)
	    (setq hs (make-hash-table :test 'equal :size 20))
	    (puthash context hs contexthash))
	  ;; Put into that contenxt's hash.
	  (puthash objname (car lp) hs)
	  )

	(setq lp (cdr lp))))

    (let* ((table (srecode-mode-table-new mode (buffer-file-name)
		   :templates (nreverse templates)
		   :namehash namehash
		   :contexthash contexthash
		   :variables vars
		   :major-mode mode
		   :priority priority
		   :application application))
	   (tmpl (oref table templates)))
      ;; Loop over all the templates, and xref.
      (while tmpl
	(oset (car tmpl) :table table)
	(setq tmpl (cdr tmpl))))
    ))
    


;;; DEBUG
;;
;; Dump out information about the current srecoder compiled templates.
;;

(defmethod srecode-dump ((tmp srecode-template))
  "Dump the contents of the SRecode template tmp."
  (princ "== Template \"")
  (princ (object-name-string tmp))
  (princ "\" in context ")
  (princ (oref tmp context))
  (princ "\n   Input Arguments required: ")
  (prin1 (oref tmp args))
  (princ "\n   Compiled Codes:\n")
  (srecode-dump-code-list (oref tmp code) "    ")
  )

(defun srecode-dump-code-list (code indent)
  "Dump the CODE from a template code list to standard output.
Argument INDENT specifies the indentation level for the list."
  (let ((i 1))
    (while code
      (princ indent)
      (prin1 i)
      (princ ") ")
      (cond ((stringp (car code))
	     (prin1 (car code)))
	    ((srecode-template-inserter-child-p (car code))
	     (srecode-dump (car code) indent))
	    (t
	     (princ "Unknown Code: ")
	     (prin1 (car code))))
      (princ "\n")
      (setq code (cdr code)
	    i (1+ i))))
  )

(defmethod srecode-dump ((ins srecode-template-inserter) indent)
  "Dump the state of the SRecode template inserter INS."
  (princ "INS: \"")
  (princ (object-name-string ins))
  (when (oref ins :secondname)
    (princ "\" : \"")
    (princ (oref ins :secondname)))
  (princ "\" type \"")
  (let* ((oc (symbol-name (object-class ins)))
	 (junk (string-match "srecode-template-inserter-" oc))
	 (on (if junk
		 (substring oc (match-end 0))
	       oc)))
    (princ on))
  (princ "\"")
  )

(provide 'srecode-compile)

;;; srecode-compile.el ends here
