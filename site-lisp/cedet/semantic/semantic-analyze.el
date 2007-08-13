;;; semantic-analyze.el --- Analyze semantic tags against local context

;;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-analyze.el,v 1.53 2007/05/17 15:46:42 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
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
;; Semantic, as a tool, provides a nice list of searchable tags.
;; That information can provide some very accurate answers if the current
;; context of a position is known.
;;
;; Semantic-ctxt provides ways of analyzing, and manipulating the
;; semantic context of a language in code.
;;
;; This library provides routines for finding intelligent answers to
;; tough problems, such as if an argument to a function has the correct
;; return type, or all possible tags that fit in a given local context.
;;

;;; Vocabulary:
;;
;; Here are some words used to describe different things in the analyzer:
;;
;; tag - A single entity
;; prefix - The beginning of a symbol, usually used to look up something
;;       incomplete.
;; type - The name of a datatype in the langauge.
;; metatype - If a type is named in a declaration like:
;;       struct moose somevariable;
;;       that name "moose" can be turned into a concrete type.
;; tag sequence - In C code, a list of dereferences, such as:
;;       this.that.theother();
;; parent - For a datatype in an OO language, another datatype
;;       inherited from.  This excludes interfaces.
;; scope - A list of tags that can be dereferenced that cannot
;;       be found from the global namespace.
;; scopetypes - A list of tags which are datatype that contain
;;       the scope.  The scopetypes need to have the scope extracted
;;       in a way that honors the type of inheritance.
;; nest/nested - When one tag is contained entirely in another.
;; 
;; context - A semantic datatype representing a point in a buffer.
;;
;; constriant - If a context specifies a specific datatype is needed,
;;       that is a constraint.
;; constants - Some datatypes define elements of themselves as a
;;       constant.  These need to be returned as there would be no
;;       other possible completions.
;;
(require 'inversion)
(eval-and-compile
  (inversion-require 'eieio "0.18beta1"))
(require 'semantic-format)
(require 'semantic-ctxt)
(require 'semantic-sort)
(eval-when-compile (require 'semanticdb)
		   (require 'semanticdb-find))

;;; Code:

;;; Small Mode Specific Options
;;
;; These queries allow a major mode to help the analyzer make decisions.
;;
(define-overload semantic-analyze-tag-prototype-p (tag)
  "Non-nil if TAG is a prototype."
  )

(defun semantic-analyze-tag-prototype-p-default (tag)
  "Non-nil if TAG is a prototype."
  (let ((p (semantic-tag-get-attribute tag :prototype-flag)))
    (cond
     ;; Trust the parser author.
     (p p)
     ;; Empty types might be a prototype.
     ((eq (semantic-tag-class tag) 'type)
      (not (semantic-tag-type-members tag)))
     ;; No other heuristics.
     (t nil))
    ))

(define-overload semantic-analyze-split-name (name)
  "Split a tag NAME into a sequence.
Sometimes NAMES are gathered from the parser that are compounded,
such as in C++ where foo::bar means:
  \"The class BAR in the namespace FOO.\"
Return the string NAME for no change, or a list if it needs to be split.")

(defun semantic-analyze-split-name-default (name)
  "Don't split up NAME by default."
  name)

(define-overload semantic-analyze-dereference-metatype (type scope)
  "Return a concrete type tag based on input TYPE tag.
A concrete type is an actual declaration of a memory description,
such as a structure, or class.  A meta type is an alias,
or a typedef in C or C++.  If TYPE is concrete, it
is returned.  If it is a meta type, it will return the concrete
type defined by TYPE.
The default behavior always returns TYPE.
Override functions need not return a real semantic tag.
Just a name, or short tag will be ok.  It will be expanded here.
SCOPE is the additional scope in which to search for names."
  (catch 'default-behavior
    (let ((ans (:override
                ;; Nothing fancy, just return type be default.
                (throw 'default-behavior type))))
      ;; If ANS is a string, or if ANS is a short tag, we
      ;; need to do some more work to look it up.
      (cond ((stringp ans)
             (semantic-analyze-find-tag ans nil scope))
            ((and (semantic-tag-p ans)
                  (eq (semantic-tag-class ans) 'type)
                  (semantic-tag-type-members ans))
             ans)
            ((and (semantic-tag-p ans)
                  (eq (semantic-tag-class ans) 'type)
                  (not (semantic-tag-type-members ans)))
             (semantic-analyze-find-tag
              (semantic-tag-name ans) nil scope))
            (t nil)))))

;;; SELECTING/MERGING
;;
;; If you narrow things down to a list of tags that all mean
;; the same thing, how to you pick one?  Select or merge.
;;

(defun semantic-analyze-merge-namespaces (spaces)
  "Merge all the namespaces SPACES into a single super-tag.
TODO: consider some higher level find routine to do this."
  (if (not (string= (semantic-tag-type (car spaces))
		    "namespace"))
      (signal 'wrong-type-argument (list (car spaces) "namespace")))
  (let ((first (car spaces))
	(members nil))
    (while spaces
      (if (string= (semantic-tag-type (car spaces)) "namespace")
	  (setq members (append members
				(semantic-tag-type-members (car spaces))))
	;; Else ... how did we get here?
	(message "Non namespace?? : %s"
		 (semantic-format-tag-summarize (car spaces))))
      (setq spaces (cdr spaces)))

    ;; Create the new tag.
    (let ((nt (semantic-tag-new-type (semantic-tag-name first)
				     (semantic-tag-type first)
				     members
				     nil)))
      (semantic-tag-set-faux nt)
      nt)))

(defun semantic-analyze-select-best-tag (sequence &optional tagclass)
  "For a SEQUENCE of tags, pick the best one.
If SEQUENCE is made up of namespaces, merge the namespaces together.
If SEQUENCE has several prototypes, find the non-prototype.
If SEQUENCE has some items w/ no type information, find the one with a type.
If SEQUENCE is all prototypes, or has no prototypes, get the first one.
Optional TAGCLASS indicates to restrict the return to only
tags of TAGCLASS."
  ;; 1) If these are namespace, merge them together.
  (if (and (or (not tagclass) (eq tagclass 'type))
	   (semantic-tag-of-class-p (car sequence) 'type)
	   (string= (semantic-tag-type (car sequence)) "namespace"))
      (semantic-analyze-merge-namespaces sequence)
    ;; 2) Loop over them, select a non-prototype.
    (let ((best nil)
	  (proto nil)
	  (notypeinfo nil)
	  )
      (while (and (not best) sequence)
	
	(when (or (not tagclass)
		  (semantic-tag-of-class-p (car sequence) tagclass))
	  ;; Prototypes are second class tags
	  (if (semantic-analyze-tag-prototype-p (car sequence))
	      (setq proto (car sequence))
	    ;; Typeless symbols are third class tags
	    (if (not (semantic-tag-type (car sequence)))
		(setq notypeinfo (car sequence))

	      (setq best (car sequence))))
	  )
	
	(setq sequence (cdr sequence)))
      
      ;; Select the best, or at least the prototype.
      (or best proto notypeinfo))))

;;; Tag Finding
;;
;; Mechanism for lookup up tags by name.
;;
(defun semantic-analyze-find-tags-by-prefix (prefix)
  "Attempt to find a tag with PREFIX.
This is a wrapper on top of semanticdb, and semantic search functions.
Almost all searches use the same arguments."
  (if (and (fboundp 'semanticdb-minor-mode-p)
           (semanticdb-minor-mode-p))
      ;; Search the database & concatenate all matches together.
      (semanticdb-strip-find-results
       (semanticdb-find-tags-for-completion prefix)
       t)
    ;; Search just this file because there is no DB available.
    (semantic-find-tags-for-completion
     prefix (current-buffer))))
 
(defun semantic-analyze-find-tag (name &optional tagclass scope)
  "Return the first tag found with NAME or nil if not found.
Optional argument TAGCLASS specifies the class of tag to return, such
as 'function or 'variable.
Optional argument SCOPE specifies additional type tags which are in
SCOPE and do not need prefixing to find.
This is a wrapper on top of semanticdb, and semantic search functions.
Almost all searches use the same arguments."
  (let ((namelst (semantic-analyze-split-name name)))
    (cond
     ;; If the splitter gives us a list, use the sequence finder
     ;; to get the list.  Since this routine is expected to return
     ;; only one tag, return the LAST tag found from the sequence
     ;; which is supposedly the nexted reference.
     ;;
     ;; Of note, the SEQUENCE function below calls this function
     ;; (recursively now) so the names that we get from the above
     ;; fcn better not, in turn, be splittable.
     ((listp namelst)
      (let ((seq (semantic-analyze-find-tag-sequence
		  namelst nil scope)))
	(car (nreverse seq))))
     ;; If NAME is solo, then do our searches for it here.
     ((stringp namelst)
      (let ((retlist
	     (or (and scope (semantic-find-tags-by-name name scope))
		 (if (and (fboundp 'semanticdb-minor-mode-p)
			  (semanticdb-minor-mode-p))
		     ;; Search the database
		     (semanticdb-strip-find-results
		      (semanticdb-find-tags-by-name name)
		      ;; This T means to find files for matching symbols
		      t)
		   ;; Search just this file
		   (semantic-find-tags-by-name
		    name (current-buffer))))))

	(semantic-analyze-select-best-tag retlist tagclass))))))

;;; Finding Datatypes
;;
;; Finding a data type by name within a project.
;;
(defun semantic-analyze-tag-type-to-name (tag)
  "Get the name of TAG's type.
The TYPE field in a tag can be nil (return nil)
or a string, or a non-positional tag."
  (let ((tt (semantic-tag-type tag)))
    (cond ((semantic-tag-p tt)
	   (semantic-tag-name tt))
	  ((stringp tt)
	   tt)
	  ((listp tt)
	   (car tt))
	  (t nil))))

(defun semantic-analyze-tag-type (tag scope)
  "Return the semantic tag for a type within the type of TAG.
TAG can be a variable, function or other type of tag.
The type of tag (such as a class or struct) is a name.
Lookup this name in database, and return all slots/fields
within that types field.  Also handles anonymous types.
SCOPE represents a calculated scope in which the types might be found."
  (let ((ttype (semantic-tag-type tag))
	(name nil)
	(typetag nil)
	)

    ;; Is it an anonymous type?
    (if (and ttype
	     (semantic-tag-p ttype)
	     (eq (semantic-tag-class ttype) 'type)
	     (semantic-analyze-type-parts ttype)
	     ;(semantic-tag-children ttype)
	     )
	;; We have an anonymous type for TAG with children.
	;; Use this type directly.
	(semantic-analyze-dereference-metatype ttype scope)

      ;; Not an anonymous type.  Look up the name of this type
      ;; elsewhere, and report back.
      (setq name (semantic-analyze-tag-type-to-name tag))
      (if (and name (not (string= name "")))
	  ;; Find a type of that name in scope.
	  (setq typetag (semantic-analyze-find-tag name 'type scope))
	;; No name to look stuff up with.
	(error "Semantic tag %S has no type information"
	       (semantic-tag-name ttype)))

      ;; Handle lists of tags.
      (when (and (listp typetag) (semantic-tag-p (car typetag)))
	(setq typetag (semantic-analyze-select-best-tag typetag 'type))
	)

      ;; We now have a tag associated with the type.
      (semantic-analyze-dereference-metatype typetag scope))))

(defun semantic-analyze-type-parts (type &optional scope)
  "Return all parts of TYPE, a tag representing a TYPE declaration.
SCOPE include additional tags which are in scope.
This includes both the TYPE parts, and all functions found in all
databases which have this type as a property."
  (let (;; SLOTS are the slots directly a part of TYPE.
	(slots (semantic-tag-components type))
	;; EXTMETH are externally defined methods that are still
	;; a part of this class.
	(extmeth (semantic-tag-external-member-children type t))
	;; INHERITED are tags found in classes that our TYPE tag
	;; inherits from.
	(inherited (semantic-analyze-inherited-tags type scope))
	)
    ;; Flatten the database output.
    (append slots extmeth inherited)
    ))

;;; Tag Sequences
;;
;; A list of strings is a sequence.  Each string needs to be found,
;; and it's datatype determined so the next string can be identified.
;;
(defun semantic-analyze-find-tag-sequence (sequence &optional localvar
						    scope typereturn)
  "Attempt to find all tags in SEQUENCE.
Optional argument LOCALVAR is the list of local variables to use when
finding the details on the first element of SEQUENCE in case
it is not found in the global set of tables.
Optional argument SCOPE are additional terminals to search which are currently
scoped.  These are not local variables, but symbols available in a structure
which doesn't need to be dereferneced.
Optional argument TYPERETURN is a symbol in which the types of all found
will be stored.  If nil, that data is thrown away."
  (let ((s sequence)			;copy of the sequence
	(tmp nil)			;tmp find variable
	(nexttype nil)			;a tag for the type next in sequence
	(tag nil)			;tag return list
	(tagtype nil)			;tag types return list
	)
    ;; For the first entry, it better be a variable, but it might
    ;; be in the local context too.
    ;; NOTE: Don't forget c++ namespace foo::bar.
    (setq tmp (or
	       ;; This should be first, but bugs in the
	       ;; C parser will turn function calls into
	       ;; assumed int return function prototypes.  Yuck!
	       (semantic-find-tags-by-name
		(car s) localvar)
	       (semantic-find-tags-by-name
		(car s) (semantic-get-local-arguments))
	       (semantic-find-tags-by-name
		(car s) scope)
	       (semantic-analyze-find-tag (car s))
	       ))

    (if (and (listp tmp) (semantic-tag-p (car tmp)))
	(setq tmp (semantic-analyze-select-best-tag tmp)))
    (if (not (semantic-tag-p tmp))
	(error "Cannot find definition for \"%s\"" (car s)))
    (setq s (cdr s))
    (setq tag (cons tmp tag))

    ;; For the middle entries
    (while s
      ;; Using the tag found in TMP, lets find the tag
      ;; representing the full typeographic information of its
      ;; type, and use that to determine the search context for
      ;; (car s)
      (let ((tmptype
	     ;; In some cases the found TMP is a type,
	     ;; and we can use it directly.
	     (cond ((eq (semantic-tag-class tmp) 'type)
		    tmp)
		   (t
		    (semantic-analyze-tag-type tmp scope))))
	    (slots nil))
	
	;; Get the children
	(setq slots (semantic-analyze-type-parts tmptype))

	;; find (car s) in the list o slots
	(setq tmp (semantic-find-tags-by-name (car s) slots))

	;; If we have lots
	(if (and (listp tmp) (semantic-tag-p (car tmp)))
	    (setq tmp (semantic-analyze-select-best-tag tmp)))

	;; Make sure we have a tag.
	(if (not (semantic-tag-p tmp))
	    (if (cdr s)
		;; In the middle, we need to keep seeking our types out.
		(error "Cannot find definition for \"%s\"" (car s))
	      ;; Else, it's ok to end with a non-tag
	      (setq tmp (car s))))

	(setq tag (cons tmp tag))
	(setq tagtype (cons tmptype tagtype))
	)
      (setq s (cdr s)))

    (if typereturn (set typereturn (nreverse tagtype)))
    ;; Return the mess
    (nreverse tag)))

;;; Scope Determination
;;
;; A context is in a scope, which is a list of tags which are
;; visible to the current context, but are not "global" variables
;; or functions.
;;
(defun semantic-analyze-inherited-tags (type scope)
  "Return all tags that TYPE inherits from.
Argument SCOPE specify additional tags that are in scope
whose tags can be searched when needed.
For langauges with protection on specific methods or slots,
it should strip out those not accessable by methods of TYPE."
  (let (;; PARENTS specifies only the superclasses and not
	;; interfaces.  Inheriting from an interfaces implies
	;; you have a copy of all methods locally.  I think.
	(parents (semantic-tag-type-superclasses type))
	(p nil)
	(ret nil)
	)
    (while parents
      (setq p (car parents))
      ;; Get this parent
      (let ((oneparent
	     (semantic-analyze-find-tag
	      (cond ((stringp p) p)
		    ((semantic-tag-p p) (semantic-tag-name p))
		    ((and (listp p) (stringp (car p)))
		     (car p)))
	      'type scope)))
	(when oneparent
	  ;; Get tags from this parent.
	  (let* ((alltags (semantic-analyze-type-parts oneparent))
		 (accessabletags (append
				  ;; @todo: Is there a better way to ask
				  ;;        this question than two full
				  ;;        searches?
				  (semantic-find-tags-by-scope-protection
				   'public oneparent alltags)
				  (semantic-find-tags-by-scope-protection
				   'protected oneparent alltags))))
	    (setq ret (append ret accessabletags)))
	  ;; is this right?
	  (setq ret (append ret (semantic-analyze-inherited-tags
				 oneparent scope)))
	  ))
	;; Continue on
      (setq parents (cdr parents)))
    ret))

(defun semantic-analyze-scoped-tags (typelist)
  "Return a list of tags accessable when TYPELIST is in scope.
Tags returned are not in the global name space, but are instead
scoped inside a class or namespace.  Such items can be referenced
without use of \"object.function()\" style syntax due to an
implicit \"object\"."
  (let ((typelist2 nil)
	(currentscope nil))
    ;; Loop over typelist, and find and merge all namespaces matching
    ;; the names in typelist.
    (while typelist
      (if (string= (semantic-tag-type (car typelist)) "namespace")
	  (setq typelist2 (cons (semantic-analyze-find-tag
				 (semantic-tag-name (car typelist))
				 'type
				 typelist2)
				typelist2))
	;; No namespace, just append...
	(setq typelist2 (cons (car typelist) typelist2)))
      (setq typelist (cdr typelist)))

    ;; Loop over the types (which should be sorted by postion
    ;; adding to the scopelist as we go, and using the scopelist
    ;; for additional searching!
    (while typelist2
      (setq currentscope (append
			  currentscope
			  (semantic-analyze-type-parts (car typelist2)
						       currentscope)))
      (setq typelist2 (cdr typelist2)))
    currentscope))

(defun semantic-analyze-scope-nested-tags (&optional position scopetypes)
  "Return a list of types in order of nesting for the context of POSITION.
If POSITION is in a method with a named parent, find that parent, and
identify it's scope via overlay instead.
Optional SCOPETYPES are additional scoped entities in which our parent might
be found.
This only finds ONE immediate parent by name.  All other parents returned
are from nesting data types."
  (save-excursion
    (if position (goto-char position))
    (let* ((stack (reverse (semantic-find-tag-by-overlay (point))))
	   (tag (car stack))
	   (pparent (car (cdr stack)))
	   )
      ;; Only do this level of analysis for functions.
      (when (eq (semantic-tag-class tag) 'function)
	(if (and pparent (eq (semantic-tag-class pparent) 'type))
	    ;; We have a parent in our stack, so analyze this stack
	    ;; We are done.
	    nil
	  ;; No parent, we need to seek one out.
	  (let ((p (semantic-tag-function-parent tag)))
	    (when p
	      ;; We have a parent, search for it.
	      (let* ((searchname (cond ((stringp p) p)
				      ((semantic-tag-p p)
				       (semantic-tag-name p))
				      ((and (listp p) (stringp (car p)))
				       (car p))))
		     (scope (apply 'append
				   (mapcar 'semantic-tag-type-members scopetypes)))
		     (ptag (semantic-analyze-find-tag searchname
						      'type scope)))
		(setq pparent ptag)))
	    ))
	;; If we have a pparent tag, lets go there
	;; an analyze that stack of tags.
	(when (and pparent (semantic-tag-with-position-p pparent))
	  (semantic-go-to-tag pparent)
	  (setq stack (reverse (semantic-find-tag-by-overlay (point))))
	  (let ((returnlist nil))
	    ;; Add things to STACK until we cease finding tags of class type.
	    (while (and stack (eq (semantic-tag-class (car stack)) 'type))
	      (setq returnlist (cons (car stack) returnlist)
		    stack (cdr stack)))
	    (reverse returnlist))
	  )))))

(defun semantic-analyze-scoped-types (&optional position)
  "Return a list of types current in scope at POSITION.
This is based on what tags exist at POSITION, and any associated
types available."
  (save-excursion
    (if position (goto-char position))
    (let ((tag (semantic-current-tag))
	  (code-scoped-types nil)
	  (parents nil))
      ;; Lets ask if any types are currently scoped.  Scoped
      ;; classes and types provide their public methods and types
      ;; in source code, but are unrelated hierarchically.
      (let ((sp (semantic-ctxt-scoped-types)))
	(while sp
	  ;; Get this thing as a tag
	  (let ((tmp (cond ((stringp (car sp))
			    (semantic-analyze-find-tag (car sp) 'type))
			   ((semantic-tag-p (car sp))
			    (car sp))
			   (t nil))))
	    (when tmp
	      (setq code-scoped-types
		    (cons tmp code-scoped-types))))
	  (setq  sp (cdr sp))))
      (setq code-scoped-types (nreverse code-scoped-types))
      ;; Get the PARENTS including nesting scope for this location.
      (setq parents (semantic-analyze-scope-nested-tags
		     nil code-scoped-types))
      ;; We return a list in case a function can have multiple explicit
      ;; parents.
      (semantic-unique-tag-table
       (if parents
	   (append parents code-scoped-types)
	 code-scoped-types)))))

;;; Simple utility functions
;;
(defun semantic-analyze-calculate-bounds ()
  "At the current point, calculate the prefix and bounds.
Return (PREFIX ENDSYM BOUNDS)"
  (let* ((prefix (semantic-ctxt-current-symbol))
	 (endsym (car (reverse prefix)))
	 (bounds (save-excursion
		   (cond ((string= endsym "")
			  (cons (point) (point))
			  )
			 ((and prefix (looking-at endsym))
			  (cons (point) (progn
					  (condition-case nil
					      (forward-sexp 1)
					    (error nil))
					  (point))))
			 (prefix
			  (condition-case nil
			      (cons (progn (forward-sexp -1) (point))
				    (progn (forward-sexp 1) (point)))
			    (error nil)))
			 (t nil))))
	 )
    (list prefix endsym bounds)))

;;; Analysis Classes
;;
;; These classes represent what a context is.  Different types
;; of contexts provide differing amounts of information to help
;; provide completions.
;;
(defclass semantic-analyze-context ()
  ((bounds :initarg :bounds
	   :type list
	   :documentation "The bounds of this context.
Usually bound to the dimension of a single symbol or command.")
   (prefix :initarg :prefix
	   :type list
	   :documentation "List of tags defining local text.
This can be nil, or a list where the last element can be a string
representing text that may be incomplete.  Preceeding elements
must be semantic tags representing variables or functions
called in a dereference sequence.")
   (prefixclass :initarg :prefixclass
		:type list
		:documentation "Tag classes expected at this context.
These are clases for tags, such as 'function, or 'variable.")
   (prefixtypes :initarg :prefixtypes
	   :type list
	   :documentation "List of tags defining types for :prefix.
This list is one shorter than :prefix.  Each element is a semantic
tag representing a type matching the semantic tag in the same
position in PREFIX.")
   (scopetypes :initarg :scopetypes
	       :type list
	       :documentation "List of type tags in scope.
When in a function is called, it may have certain other types
in scope, such as classes in it's lineage.  This is a list
of all those classes.")
   (scope :initarg :scope
	  :type list
	  :documentation "List of tags available in scopetype.
See `semantic-analyze-scoped-tags' for details.")
   (localvariables :initarg :localvariables
		   :initform nil
		   :type list
		   :documentation "List of local variables.
Local variables are defined withing the code scope.")
   (buffer :initarg :buffer
	   :type buffer
	   :documentation "The buffer this context is derived from.")
   )
  "Base analysis data for a any context.")

(defclass semantic-analyze-context-assignment (semantic-analyze-context)
  ((assignee :initarg :assignee
	     :type list
	     :documentation "A sequence of tags for an assignee.
This is a variable into which some value is being placed.  The last
item in the list is the variable accepting the value.  Earlier
tags represent the variables being derefernece to get to the
assignee."))
  "Analysis class for a value in an assignment.")

(defclass semantic-analyze-context-functionarg (semantic-analyze-context)
  ((function :initarg :function
	     :type list
	     :documentation "A sequence of tags for a function.
This is a function being called.  The cursor will be in the position
of an argument.
The last tag in :function is the function being called.  Earlier
tags represent the variables being dereferenced to get to the
function.")
   (index :initarg :index
	  :type integer
	  :documentation "The index of the argument for this context.
If a function takes 4 arguments, this value should be bound to
the values 1 through 4.")
   (argument :initarg :argument
	     :type list
	     :documentation "A sequence of tags for the :index argument.
The argument can accept a value of some type, and this contains the
tag for that definition.  It should be a tag, but might
be just a string in some circumstances.")
   )
  "Analysis class for a value as a function argument.")

(defclass semantic-analyze-context-return (semantic-analyze-context)
  () ; No extra data.
  "Analysis class for return data.
Return data methods identify the requred type by the return value
of the parent function.")

;;; ANALYSIS
;;
;; Main Analysis function
;;
;;;###autoload
(define-overload semantic-analyze-current-context (&optional position)
  "Analyze the current context at optional POSITION.
If called interactively, display interesting information about POSITION
in a separate buffer.
Returns an object based on symbol `semantic-analyze-context'.

This function can be overriden with the symbol `analyze-context'.
When overriding this function, your override will be called while
cursor is at POSITION.  In addition, your function will not be called
if a cached copy of the return object is found."
  (interactive "d")
  (if (not position) (setq position (point)))
  (save-excursion
    (goto-char position)
    (let* ((answer (semantic-get-cache-data 'current-context)))
      (with-syntax-table semantic-lex-syntax-table
	(when (not answer)
	  (setq answer (:override))
	  (when (and answer (oref answer bounds))
	    (with-slots (bounds) answer
	      (semantic-cache-data-to-buffer (current-buffer)
					     (car bounds)
					     (cdr bounds)
					     answer
					     'current-context
					     'exit-cache-zone))
	    ;; Check for interactivity
	    (if (interactive-p)
		(semantic-analyze-pop-to-context answer))))
      
	answer))))

(defun semantic-analyze-current-context-default (position)
  "Analyze the current context at POSITION.
Returns an object based on symbol `semantic-analyze-context'."
  (let* ((context-return nil)
	 (startpoint (point))
	 (prefixandbounds (semantic-analyze-calculate-bounds))
	 (prefix (car prefixandbounds))
	 (endsym (nth 1 prefixandbounds))
	 (bounds (nth 2 prefixandbounds))
	 (prefixclass (semantic-ctxt-current-class-list))
	 (prefixtypes nil)
	 (scopetypes nil)
	 (scope nil)
	 (localvar nil)
	 (function nil)
	 (fntag nil)
	 arg fntagend argtag
	 )

    (unless (not bounds)

      ;; Don't do the work if there are no bounds.
      (setq scopetypes (semantic-analyze-scoped-types position)
	    scope (if scopetypes
		      (semantic-analyze-scoped-tags scopetypes))
	    localvar (semantic-get-local-variables)
	    function (semantic-ctxt-current-function))

      (condition-case nil
	  ;; If we are on lame stuff, it won't be found!
	  (setq prefix (semantic-analyze-find-tag-sequence
			prefix localvar scope 'prefixtypes))
	(error nil))

      (when function
	;; If we have a function, then we can get the argument
	(setq arg (semantic-ctxt-current-argument))

	(condition-case nil
	    (setq fntag
		  (semantic-analyze-find-tag-sequence
		   function localvar scope))
	  (error nil))

	(when fntag
	  (setq fntagend (car (reverse fntag))
		argtag
		(when (semantic-tag-p fntagend)
		  (nth (1- arg) (semantic-tag-function-arguments fntagend)))
		)))

      (if fntag
	  ;; If we found a tag for our function, we can go into
	  ;; functional context analysis mode, meaning we have a type
	  ;; for the argument.
	  (setq context-return
		(semantic-analyze-context-functionarg
		 "functionargument"
		 :buffer (current-buffer)
		 :function fntag
		 :index arg
		 :argument (list argtag)
		 :scope scope
		 :scopetypes scopetypes
		 :localvariables localvar
		 :prefix prefix
		 :prefixclass prefixclass
		 :bounds bounds
		 :prefixtypes prefixtypes))

	;; No function, try assignment
	(let ((assign (semantic-ctxt-current-assignment))
	      (asstag nil))
	  (if assign
	      ;; We have an assignment
	      (condition-case nil
		  (setq asstag (semantic-analyze-find-tag-sequence
				assign localvar scope))
		(error nil)))
	  
	  (if asstag
	      (setq context-return
		    (semantic-analyze-context-assignment
		     "assignment"
		     :buffer (current-buffer)
		     :assignee asstag
		     :scope scope
		     :scopetypes scopetypes
		     :localvariables localvar
		     :bounds bounds
		     :prefix prefix
		     :prefixclass prefixclass
		     :prefixtypes prefixtypes))
	  
	    ;; TODO: Identify return value condition.

	    ;; Nothing in particular
	    (setq context-return
		  (semantic-analyze-context
		   "context"
		   :buffer (current-buffer)
		   :scope scope
		   :scopetypes scopetypes
		   :localvariables localvar
		   :bounds bounds
		   :prefix prefix
		   :prefixclass prefixclass
		   :prefixtypes prefixtypes)))))

      ;; Return our context.
      context-return)))


;;; COMPLETION
;;
;; Context Analysis Completion
;;
(defmethod semantic-analyze-type-constraint
  ((context semantic-analyze-context) &optional desired-type)
  "Return a type constraint for completing :prefix in CONTEXT.
Optional argument DESIRED-TYPE may be a non-type tag to analyze."
  (when (semantic-tag-p desired-type)
    ;; Convert the desired type if needed.
    (if (not (eq (semantic-tag-class desired-type) 'type))
	(setq desired-type (semantic-tag-type desired-type)))
    ;; Protect against plain strings
    (cond ((stringp desired-type)
	   (setq desired-type (list desired-type 'type)))
	  ((and (stringp (car desired-type))
		(not (semantic-tag-p desired-type)))
	   (setq desired-type (list (car desired-type) 'type)))
	  ((semantic-tag-p desired-type)
	   ;; We have a tag of some sort.  Yay!
	   nil)
	  (t (setq desired-type nil))
	  )
    desired-type))

(defmethod semantic-analyze-type-constraint
  ((context semantic-analyze-context-functionarg))
  "Return a type constraint for completing :prefix in CONTEXT."
  (call-next-method context (car (oref context argument))))

(defmethod semantic-analyze-type-constraint
  ((context semantic-analyze-context-assignment))
  "Return a type constraint for completing :prefix in CONTEXT."
  (call-next-method context (car (reverse (oref context assignee)))))

(defmethod semantic-analyze-interesting-tag
  ((context semantic-analyze-context))
  "Return a tag from CONTEXT that would be most interesting to a user."
  (let ((prefix (oref context :prefix)))
    (cond ((semantic-tag-p (car prefix))
	   ;; If the prefix is a tag, that is interesting.
	   (car prefix))
	  ((and (stringp (car prefix))
		(semantic-tag-p (car (cdr prefix))))
	   ;; Well, if it is a string, the predecessor might be
	   ;; interesting.
	   (car (cdr prefix)))
	  (t
	   ;; Nope, nothing good.
	   nil))
    ))

(defmethod semantic-analyze-interesting-tag
  ((context semantic-analyze-context-functionarg))
  "Try the base, and if that fails, return what we are assigning into."
  (or (call-next-method) (car-safe (oref context :function))))

(defmethod semantic-analyze-interesting-tag
  ((context semantic-analyze-context-assignment))
  "Try the base, and if that fails, return what we are assigning into."
  (or (call-next-method) (car-safe (oref context :assignee))))

(define-overload semantic-analyze-type-constants (type)
  "For the tag TYPE, return any constant symbols of TYPE.
Used as options when completing."
  (let ((ans
         (:override-with-args
             ((semantic-analyze-find-tag (semantic-tag-name type)))
           ;; Be default, we don't know.
           nil))
        (out nil))
    (dolist (elt ans)
      (cond
       ((stringp elt)
        (push (semantic-tag-new-variable
               elt (semantic-tag-name type) nil)
              out))
       ((semantic-tag-p elt)
        (push elt out))
       (t nil)))
    (nreverse out)))

(defun semantic-analyze-tags-of-class-list (tags classlist)
  "Return the tags in TAGS that are of classes in CLASSLIST."
  (let ((origc tags))
    ;; Accept only tags that are of the datatype specified by
    ;; the desired classes.
    (setq tags (apply 'append
		      (mapcar (lambda (class)
				(semantic-find-tags-by-class class origc))
			      classlist)))
    tags))

;;;###autoload
(define-overload semantic-analyze-possible-completions (context)
  "Return a list of semantic tags which are possible completions.
CONTEXT is either a position (such as point), or a precalculated
context.  Passing in a context is useful if the caller also needs
to access parts of the analysis.
Completions run through the following filters:
  * Elements currently in scope
  * Constants currently in scope
  * Elements match the :prefix in the CONTEXT.
  * Type of the completion matches the type of the context.
Context type matching can identify the following:
  * No specific type
  * Assignment into a variable of some type.
  * Argument to a function with type constraints.
When called interactively, displays the list of possible completions
in a buffer."
  (interactive "d")
  (with-syntax-table semantic-lex-syntax-table
    (let* ((context (if (semantic-analyze-context-child-p context)
                        context
                      (semantic-analyze-current-context context)))
	   (ans (:override)))
      ;; If interactive, display them.
      (when (interactive-p)
	(with-output-to-temp-buffer "*Possible Completions*"
	  (semantic-analyze-princ-sequence ans "" (current-buffer)))
	(shrink-window-if-larger-than-buffer
	 (get-buffer-window "*Possible Completions*")))
      ans)))

(defun semantic-analyze-possible-completions-default (context)
  "Default method for producing smart completions.
Argument CONTEXT is an object specifying the locally derived context."
  (let* ((a context)
	 (fnargs (save-excursion
		   (semantic-get-local-arguments
		    (car (oref a bounds)))))
	 (desired-type (semantic-analyze-type-constraint a))
	 (desired-class (oref a prefixclass))
	 (prefix (oref a prefix))
	 (prefixtypes (oref a prefixtypes))
	 (completetext nil)
	 (completetexttype nil)
	 (c nil))

    ;; Calculate what our prefix string is so that we can
    ;; find all our matching text.
    (setq completetext (car (reverse prefix)))
    (if (semantic-tag-p completetext)
	(setq completetext (semantic-tag-name completetext)))

    (if (and (not completetext) (not desired-type))
	(error "Nothing to complete"))

    (if (not completetext) (setq completetext ""))

    ;; This better be a reasonable type, or we should fry it.
    ;; The prefixtypes should always be at least 1 less than
    ;; the prefix since the type is never looked up for the last
    ;; item when calculating a sequence.
    (setq completetexttype (car (reverse prefixtypes)))
    (if (or (not completetexttype)
	    (not (and (semantic-tag-p completetexttype)
		      (eq (semantic-tag-class completetexttype) 'type))))
	;; What should I do here?  I think this is an error condition.
	(setq completetexttype nil))

    ;; There are many places to get our completion stream for.
    ;; Here we go.
    (if completetexttype

	(setq c (semantic-find-tags-by-name-regexp
		 (concat "^" completetext)
		 (semantic-analyze-type-parts completetexttype
					      (oref a scope))
		 ))
	      
      (let ((expr (concat "^" completetext)))
	;; No type based on the completetext.  This is a free-range
	;; var or function.  We need to expand our search beyond this
	;; scope into semanticdb, etc.
	(setq c (append
		 ;; Argument list
		 (semantic-find-tags-by-name-regexp expr fnargs)
		 ;; Local variables
		 (semantic-find-tags-by-name-regexp expr
						    (oref a localvariables))
		 ;; The current scope
		 (semantic-find-tags-by-name-regexp expr (oref a scope))
		 ;; The world
		 (semantic-analyze-find-tags-by-prefix
		  completetext))
	      )
	))

    (let ((origc c)
	  (scope (oref a scope))
	  (dtname (semantic-tag-name desired-type)))
	
      ;; Reset c.
      (setq c nil)

      ;; Loop over all the found matches, and catagorize them
      ;; as being possible features.
      (while origc

	(cond
	 ;; Strip operators
	 ((semantic-tag-get-attribute (car origc) :operator-flag)
	  nil
	  )
	 
	 ;; If we are completing from within some prefix,
	 ;; then we want to exclude constructors and destructors
	 ((and completetexttype
	       (or (semantic-tag-get-attribute (car origc) :constructor-flag)
		   (semantic-tag-get-attribute (car origc) :destructor-flag)))
	  nil
	  )

	 ;; If there is a desired type, we need a pair of restrictions
	 (desired-type

	  (cond
	   ;; Ok, we now have a completion list based on the text we found
	   ;; we want to complete on.  Now filter that stream against the
	   ;; type we want to search for.
	   ((string= dtname (semantic-analyze-tag-type-to-name (car origc)))
	    (setq c (cons (car origc) c))
	    )

	   ;; Now anything that is a compound type which could contain
	   ;; additional things which are of the desired type
	   ((semantic-tag-type (car origc))
	    (let ((att (semantic-analyze-tag-type (car origc) scope))
		)
	      (if (and att (semantic-tag-type-members att))
		  (setq c (cons (car origc) c))))
	    )
	   
	   ) ; cond
	  ); desired type

	 ;; No desired type, no other restrictions.  Just add.
	 (t
	  (setq c (cons (car origc) c)))

	 ); cond

	(setq origc (cdr origc)))

      (when desired-type
      ;; Some types, like the enum in C, have special constant values that
      ;; we could complete with.  Thus, if the target is an enum, we can
      ;; find possible symbol values to fill in that value.
      (let ((constants
	     (semantic-analyze-type-constants desired-type)))
	(if constants
	    (progn
	      ;; Filter
	      (setq constants
		    (semantic-find-tags-by-name-regexp
		     (concat "^" completetext)
		     constants))
	      ;; Add to the list
	      (setq c (append c constants)))
	  )))
      )

    (when desired-class
      (setq c (semantic-analyze-tags-of-class-list c desired-class)))

    ;; Pull out trash.
    ;; NOTE TO SELF: Is this too slow?
    ;; OTHER NOTE: Do we not want to strip duplicates by name and
    ;; only by position?  When are duplicate by name but not by tag
    ;; useful?
    (setq c (semantic-unique-tag-table-by-name c))

    ;; All done!

    c))


;;; DEBUG OUTPUT 
;;
;; Friendly output of a context analysis.
;;
(defcustom semantic-analyze-summary-function 'semantic-format-tag-prototype
  "*Function to use when creating items in Imenu.
Some useful functions are found in `semantic-format-tag-functions'."
  :group 'semantic
  :type semantic-format-tag-custom-list)

(defun semantic-analyze-princ-sequence (sequence &optional prefix buff)
  "Send the tag SEQUENCE to standard out.
Use PREFIX as a label.
Use BUFF as a source of override methods."
  (while sequence
      (princ prefix)
      (cond
       ((semantic-tag-p (car sequence))
	(princ (funcall semantic-analyze-summary-function
			(car sequence))))
       ((stringp (car sequence))
	(princ "\"")
	(princ (semantic--format-colorize-text (car sequence) 'variable))
	(princ "\""))
       (t
	(princ (format "'%S" (car sequence)))))
      (princ "\n")
      (setq sequence (cdr sequence))
      (setq prefix (make-string (length prefix) ? ))
      ))

(defmethod semantic-analyze-show ((context semantic-analyze-context))
  "Insert CONTEXT into the current buffer in a nice way."
  (semantic-analyze-princ-sequence (oref context prefix) "Prefix: " )
  (semantic-analyze-princ-sequence (oref context prefixclass) "Prefix Classes: ")
  (semantic-analyze-princ-sequence (oref context prefixtypes) "Prefix Types: ")
  (princ "--------\n")
  (semantic-analyze-princ-sequence (oref context scopetypes) "Scope Types: ")
  (semantic-analyze-princ-sequence (oref context scope) "Scope: ")
  (semantic-analyze-princ-sequence (oref context localvariables) "LocalVars: ")
  )

(defmethod semantic-analyze-show ((context semantic-analyze-context-assignment))
  "Insert CONTEXT into the current buffer in a nice way."
  (semantic-analyze-princ-sequence (oref context assignee) "Assignee: ")
  (call-next-method))

(defmethod semantic-analyze-show ((context semantic-analyze-context-functionarg))
  "Insert CONTEXT into the current buffer in a nice way."
  (semantic-analyze-princ-sequence (oref context function) "Function: ")
  (princ "Argument Index: ")
  (princ (oref context index))
  (princ "\n")
  (semantic-analyze-princ-sequence (oref context argument) "Argument: ")
  (call-next-method))

(defun semantic-analyze-pop-to-context (context)
  "Display CONTEXT in a temporary buffer.
CONTEXT's content is described in `semantic-analyze-current-context'."
  (with-output-to-temp-buffer "*Semantic Context Analysis*"
    (princ "Context Type: ")
    (princ (object-name context))
    (princ "\n")
    (princ "Bounds: ")
    (princ (oref context bounds))
    (princ "\n")
    (semantic-analyze-show context)
    )
  (shrink-window-if-larger-than-buffer
   (get-buffer-window "*Semantic Context Analysis*"))
  )

(provide 'semantic-analyze)

;;; semantic-analyze.el ends here
