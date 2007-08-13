;;; eieio.el --- Enhanced Implementation of Emacs Interpreted Objects
;;               or maybe Eric's Implementation of Emacs Intrepreted Objects

;;;
;; Copyright (C) 95,96,98,99,2000,01,02,03,04,05,06,07 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; RCS: $Id: eieio.el,v 1.149 2007/03/18 17:20:41 zappo Exp $
;; Keywords: OO, lisp
(defvar eieio-version "1.0"
  "Current version of EIEIO.")
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.org

;;; Commentary:
;;
;; EIEIO is a series of Lisp routines which implements a subset of
;; CLOS, the Common Lisp Object System.  In addition, EIEIO also adds
;; a few new features which help it integrate more strongly with the
;; Emacs running environment.
;;
;; See eieio.texi for complete documentation on using this package.

;; There is funny stuff going on with typep and deftype.  This
;; is the only way I seem to be able to make this stuff load properly.
(require 'cl)
(load "cl-macs" nil t) ; No provide in this file.

;;; Code:
(defun eieio-version ()
  "Display the current version of EIEIO."
  (interactive)
  (message eieio-version))

(require 'inversion)

(defun eieio-require-version (major minor &optional beta)
  "Non-nil if this version of EIEIO does not satisfy a specific version.
Arguments can be:

  (MAJOR MINOR &optional BETA)

  Values MAJOR and MINOR must be integers.  BETA can be an integer, or
excluded if a released version is required.

It is assumed that if the current version is newer than that specified,
everything passes.  Exceptions occur when known incompatibilities are
introduced."
  (inversion-test 'eieio
		  (format "%s.%s%s" major minor
			  (if beta (format "beta%s" beta) ""))))

(eval-and-compile
;; Abount the above.  EIEIO must process it's own code when it compiles
;; itself, thus, by eval-and-compiling outselves, we solve the problem.

;; Compatibility
(if (fboundp 'compiled-function-arglist)

    ;; XEmacs can only access a compiled functions arglist like this:
    (defalias 'eieio-compiled-function-arglist 'compiled-function-arglist)

  ;; Emacs doesn't have this function, but since FUNC is a vector, we can just
  ;; grab the appropriate element.
  (defun eieio-compiled-function-arglist (func)
    "Return the argument list for the compiled function FUNC."
    (aref func 0))

  )


;;;
;; Variable declarations.
;;

(defvar eieio-hook nil
  "*This hook is executed, then cleared each time `defclass' is called.
The immediate effect is that I can safely keep track of common-lisp
`setf' definitions regardless of the order.  Users can add hooks to
this variable without worrying about weather this package has been
loaded or not.")

(defvar eieio-error-unsupported-class-tags nil
  "*Non nil to throw an error if an encountered tag us unsupported.
This may prevent classes from CLOS applications from being used with EIEIO
since EIEIO does not support all CLOS tags.")

(defvar eieio-skip-typecheck nil
  "*If non-nil, skip all slot typechecking.
Set this to t permanently if a program is functioning well to get a
small speed increase.  This variable is also used internally to handle
default setting for optimization purposes.")

;; State Variables
(defvar this nil
  "Inside a method, this variable is the object in question.
DO NOT SET THIS YOURSELF unless you are trying to simulate friendly fields.

Note: Embedded methods are no longer supported.  The variable THIS is
still set for CLOS methods for the sake of routines like
`call-next-method'")

(defvar scoped-class nil
  "This is set to a class when a method is running.
This is so we know we are allowed to check private parts or how to
execute a `call-next-method'.  DO NOT SET THIS YOURSELF!")

(defvar eieio-initializing-object  nil
  "Set to non-nil while initializing an object.")

(defconst eieio-unbound (make-symbol "unbound")
  "Uninterned symbol representing an unbound slot in an object.")

;; This is a bootstrap for eieio-default-superclass so it has a value
;; while it is being built itself.
(defvar eieio-default-superclass nil)

(defconst class-symbol 1 "Class's symbol (self-referencing.).")
(defconst class-parent 2 "Class parent field.")
(defconst class-children 3 "Class children class field.")
(defconst class-symbol-obarray 4 "Obarray permitting fast access to variable position indexes.")
(defconst class-public-a 5 "Class public attribute index.")
(defconst class-public-d 6 "Class public attribute defaults index.")
(defconst class-public-doc 7 "Class public documentation strings for attributes.")
(defconst class-public-type 8 "Class public type for a slot.")
(defconst class-public-custom 9 "Class public custom type for a slot.")
(defconst class-public-custom-label 10 "Class public custom group for a slot.")
(defconst class-public-custom-group 11 "Class public custom group for a slot.")
(defconst class-protection 12 "Class protection for a slot.")
(defconst class-initarg-tuples 13 "Class initarg tuples list.")
(defconst class-class-allocation-a 14 "Class allocated attributes.")
(defconst class-class-allocation-doc 15 "Class allocated documentation.")
(defconst class-class-allocation-type 16 "Class allocated value type.")
(defconst class-class-allocation-custom 17 "Class allocated custom descriptor.")
(defconst class-class-allocation-custom-label 18 "Class allocated custom descriptor.")
(defconst class-class-allocation-custom-group 19 "Class allocated custom group.")
(defconst class-class-allocation-protection 20 "Class allocated protection list.")
(defconst class-class-allocation-values 21 "Class allocated value vector.")
(defconst class-default-object-cache 22
  "Cache index of what a newly created object would look like.
This will speed up instantiation time as only a `copy-sequence' will
be needed, instead of looping over all the values and setting them
from the default.")
(defconst class-options 23
  "Storage location of tagged class options.
Stored outright without modifications or stripping.")

(defconst class-num-fields 24
  "Number of fields in the class definition object.")

(defconst object-class 1 "Index in an object vector where the class is stored.")
(defconst object-name 2 "Index in an object where the name is stored.")

(defconst method-static 0 "Index into :STATIC tag on a method.")
(defconst method-before 1 "Index into :BEFORE tag on a method.")
(defconst method-primary 2 "Index into :PRIMARY tag on a method.")
(defconst method-after 3 "Index into :AFTER tag on a method.")
(defconst method-num-lists 4 "Number of indexes into methods vector in which groups of functions are kept.")
(defconst method-generic-before 4 "Index into generic :BEFORE tag on a method.")
(defconst method-generic-primary 5 "Index into generic :PRIMARY tag on a method.")
(defconst method-generic-after 6 "Index into generic :AFTER tag on a method.")
(defconst method-num-fields 7 "Number of indexes into a method's vector.")

;; How to specialty compile stuff.
(autoload 'byte-compile-file-form-defmethod "eieio-comp"
  "This function is used to byte compile methods in a nice way.")
(put 'defmethod 'byte-hunk-handler 'byte-compile-file-form-defmethod)

(eval-when-compile (require 'eieio-comp))


;;; Important macros used in eieio.
;;
(defmacro class-v (class) "Internal: Return the class vector from the CLASS symbol."
  ;; No check: If eieio gets this far, it's probably been checked already.
  `(get ,class `eieio-class-definition))

(defmacro class-p (class)
  "Return t if CLASS is a valid class vector.
CLASS is a symbol.  Defclass will assign the class symbol to itself, so
the shortcut (class-p foo) will work.  The form (class-p 'foo) is more
robust."
  ;; this new method is faster since it doesn't waste time checking lots of
  ;; things.
  `(condition-case nil
       (eq (aref (class-v ,class) 0) 'defclass)
     (error nil)))

(defmacro object-p (obj) "Return t if OBJ is an object vector."
  `(condition-case nil
       (let ((tobj ,obj))
	 (and (eq (aref tobj 0) 'object)
	      (class-p (aref tobj object-class))))
     (error nil)))

(defmacro class-constructor (class)
  "Return the symbol representing the constructor of CLASS."
  `(aref (class-v ,class) class-symbol))

(defmacro generic-p (method)
  "Return t if symbol METHOD is a generic function.
Only methods have the symbol `eieio-method-obarray' as a property (which
contains a list of all bindings to that method type.)"
  `(and (fboundp ,method) (get ,method 'eieio-method-obarray)))

(defmacro class-option-assoc (list option)
  "Return from LIST the found OPTION.  Nil if it doesn't exist."
  `(car-safe (cdr (memq ,option ,list))))

(defmacro class-option (class option)
  "Return the value stored for CLASS' OPTION.
Return nil if that option doesn't exist."
  `(class-option-assoc (aref (class-v ,class) class-options) ',option))

(defmacro class-abstract-p (class)
  "Return non-nil if CLASS is abstract.
Abstract classes cannot be instantiated."
  `(class-option ,class :abstract))


;;; Defining a new class
;;
(defmacro defclass (name superclass fields &rest options-and-doc)
  "Define NAME as a new class derived from SUPERCLASS with FIELDS.
OPTIONS-AND-DOC is used as the class' options and base documentation.
SUPERCLASS is a list of superclasses to inherit from, with FIELDS
being the fields residing in that class definition.  NOTE: Currently
only one field may exist in SUPERCLASS as multiple inheritance is not
yet supported.  Supported tags are:

  :initform   - initializing form
  :initarg    - tag used during initialization
  :accessor   - tag used to create a function to access this field
  :allocation - specify where the value is stored.
                defaults to `:instance', but could also be `:class'
  :writer     - a function symbol which will `write' an object's slot
  :reader     - a function symbol which will `read' an object
  :type       - the type of data allowed in this slot (see `typep')
  :documentation
              - A string documenting use of this slot.

The following are extensions on CLOS:
  :protection - Specify protection for this slot.
                Defaults to `:public'.  Also use `:protected', or `:private'
  :custom     - When customizing an object, the custom :type.  Public only.
  :label      - A text string label used for a slot when customizing.
  :group      - Name of a customization group this slot belongs in.

A class can also have optional options.  These options happen in place
of documentation, (including a :documentation tag) in addition to
documentation, or not at all.  Supported options are:

  :documentation - The doc-string used for this class.

Options added to EIEIO:

  :allow-nil-initform - Non-nil to skip typechecking of initforms if nil.
  :custom-groups      - List of custom group names.  Organizes slots into
                        reasonable groups for customizations.
  :abstract           - Non-nil to prevent instances of this class.
                        If a string, use as an error string if someone does
                        try to make an instance.

Options in CLOS not supported in EIEIO:

  :metaclass - Class to use in place of `standard-class'
  :default-initargs - Initargs to use when initializing new objects of
                      this class.

Due to the way class options are set up, you can add any tags in you
wish, and reference them using the function `class-option'."
  ;; We must `eval-and-compile' this so that when we byte compile
  ;; an eieio program, there is no need to load it ahead of time.
  ;; It also provides lots of nice debugging errors at compile time.
  `(eval-and-compile
     (eieio-defclass ',name ',superclass ',fields ',options-and-doc)))

(defun eieio-defclass (cname superclasses fields options-and-doc)
  "See `defclass' for more information.
Define CNAME as a new subclass of SUPERCLASSES, with FIELDS being the
fields residing in that class definition, and with options or documentation
OPTIONS-AND-DOC as the toplevel documentation for this class."
  ;; Run our eieio-hook each time, and clear it when we are done.
  ;; This way people can add hooks safely if they want to modify eieio
  ;; or add definitions when eieio is loaded or something like that.
  (run-hooks 'eieio-hook)
  (setq eieio-hook nil)

  (if (not (symbolp cname)) (signal 'wrong-type-argument '(symbolp cname)))
  (if (not (listp superclasses)) (signal 'wrong-type-argument '(listp superclasses)))

  (let* ((pname (if superclasses superclasses nil))
	 (newc (make-vector class-num-fields nil))
	 (oldc (when (class-p cname) (class-v cname)))
	 (groups nil) ;; list of groups id'd from slots
	 (options nil)
	 (clearparent nil))

    (aset newc 0 'defclass)
    (aset newc class-symbol cname)

    ;; If this class already existed, and we are updating it's structure,
    ;; make sure we keep the old child list.  This can cause bugs, but
    ;; if no new slots are created, it also saves time, and prevents
    ;; method table breakage, particularly when the users is only
    ;; byte compiling an EIEIO file.
    (when oldc
      (aset newc class-children (aref oldc class-children)))

    (cond ((and (stringp (car options-and-doc))
		(/= 1 (% (length options-and-doc) 2)))
	   (error "Too many arguments to `defclass'"))
	  ((and (symbolp (car options-and-doc))
		(/= 0 (% (length options-and-doc) 2)))
	   (error "Too many arguments to `defclass'"))
	  )

    (setq options
	  (if (stringp (car options-and-doc))
	      (cons :documentation options-and-doc)
	    options-and-doc))

    (if pname
	(progn
	  (while pname
	    (if (and (car pname) (symbolp (car pname)))
		(if (not (class-p (car pname)))
		    ;; bad class
		    (error "Given parent class %s is not a class" (car pname))
		  ;; good parent class...
		  ;; save new child in parent
		  (if (not (member cname (aref (class-v (car pname)) class-children)))
		      (aset (class-v (car pname)) class-children
			    (cons cname (aref (class-v (car pname)) class-children))))
		  ;; Get custom groups, and store them into our local copy.
		  (mapcar (lambda (g) (add-to-list 'groups g))
			  (class-option (car pname) :custom-groups))
		  ;; save parent in child
		  (aset newc class-parent (cons (car pname) (aref newc class-parent))))
	      (error "Invalid parent class %s" pname))
	    (setq pname (cdr pname)))
	  ;; Reverse the list of our parents so that they are prioritized in
	  ;; the same order as specified in the code.
	  (aset newc class-parent (nreverse (aref newc class-parent))) )
      ;; If there is nothing to loop over, then inherit from the
      ;; default superclass.
      (unless (eq cname 'eieio-default-superclass)
	;; adopt the default parent here, but clear it later...
	(setq clearparent t)
	;; save new child in parent
	(if (not (member cname (aref (class-v 'eieio-default-superclass) class-children)))
	    (aset (class-v 'eieio-default-superclass) class-children
		  (cons cname (aref (class-v 'eieio-default-superclass) class-children))))
	;; save parent in child
	(aset newc class-parent (list eieio-default-superclass))))
    
    ;; turn this into a useable self-pointing symbol
    (set cname cname)

    ;; These two tests must be created right away so we can have self-
    ;; referencing classes.  ei, a class whose slot can contain only
    ;; pointers to itself.

    ;; Create the test function
    (let ((csym (intern (concat (symbol-name cname) "-p"))))
      (fset csym
	    (list 'lambda (list 'obj)
		  (format "Test OBJ to see if it an object of type %s" cname)
		  (list 'and '(object-p obj)
			(list 'same-class-p 'obj cname)))))

    ;; Create a handy child test too
    (let ((csym (intern (concat (symbol-name cname) "-child-p"))))
      (fset csym
	    `(lambda (obj)
	       ,(format
		  "Test OBJ to see if it an object is a child of type %s"
		  cname)
	       (and (object-p obj)
		    (object-of-class-p obj ,cname))))
    
      ;; When using typep, (typep OBJ 'myclass) returns t for objects which
      ;; are subclasses of myclass.  For our predicates, however, it is
      ;; important for EIEIO to be backwards compatible, where
      ;; myobject-p, and myobject-child-p are different.
      ;; "cl" uses this technique to specify symbols with specific typep
      ;; test, so we can let typep have the CLOS documented behavior
      ;; while keeping our above predicate clean.
      (eval `(deftype ,cname ()
	       '(satisfies
		 ,(intern (concat (symbol-name cname) "-child-p")))))

      )

    ;; before adding new fields, lets add all the methods and classes
    ;; in from the parent class
    (eieio-copy-parents-into-subclass newc superclasses)

    ;; Store the new class vector definition into the symbol.  We need to
    ;; do this first so that we can call defmethod for the accessor.
    ;; The vector will be updated by the following while loop and will not
    ;; need to be stored a second time.
    (put cname 'eieio-class-definition newc)

    ;; Query each field in the declaration list and mangle into the
    ;; class structure I have defined.
    (while fields
      (let* ((field1  (car fields))
	     (name    (car field1))
	     (field   (cdr field1))
	     (acces   (plist-get field ':accessor))
	     (init    (or (plist-get field ':initform)
			  (if (member ':initform field) nil
			    eieio-unbound)))
	     (initarg (plist-get field ':initarg))
	     (docstr  (plist-get field ':documentation))
	     (prot    (plist-get field ':protection))
	     (reader  (plist-get field ':reader))
	     (writer  (plist-get field ':writer))
	     (alloc   (plist-get field ':allocation))
	     (type    (plist-get field ':type))
	     (custom  (plist-get field ':custom))
	     (label   (plist-get field ':label))
	     (customg (plist-get field ':group))
	     
	     (skip-nil (class-option-assoc options :allow-nil-initform))
	     )

	(if eieio-error-unsupported-class-tags
	    (let ((tmp field))
	      (while tmp
		(if (not (member (car tmp) '(:accessor
					     :initform
					     :initarg
					     :documentation
					     :protection
					     :reader
					     :writer
					     :allocation
					     :type
					     :custom
					     :label
					     :group
					     :allow-nil-initform
					     :custom-groups)))
		    (signal 'invalid-slot-type (list (car tmp))))
		(setq tmp (cdr (cdr tmp))))))

	;; Clean up the meaning of protection.
	(cond ((or (eq prot 'public) (eq prot :public)) (setq prot nil))
	      ((or (eq prot 'protected) (eq prot :protected)) (setq prot 'protected))
	      ((or (eq prot 'private) (eq prot :private)) (setq prot 'private))
	      ((eq prot nil) nil)
	      (t (signal 'invalid-slot-type (list ':protection prot))))

	;; Make sure the :allocation parameter has a valid value.
	(if (not (or (not alloc) (eq alloc :class) (eq alloc :instance)))
	    (signal 'invalid-slot-type (list ':allocation alloc)))

	;; The default type specifier is supposed to be t, meaning anything.
	(if (not type) (setq type t))

	;; Label is nil, or a string
	(if (not (or (null label) (stringp label)))
	    (signal 'invalid-slot-type (list ':label label)))
	
	;; Is there an initarg, but allocation of class?
	(if (and initarg (eq alloc :class))
	    (message "Class allocated slots do not need :initarg"))

	;; intern the symbol so we can use it blankly
	(if initarg (set initarg initarg))

	;; The customgroup should be a list of symbols
	(cond ((null customg)
	       (setq customg '(default)))
	      ((not (listp customg))
	       (setq customg (list customg))))
	;; The customgroup better be a symbol, or list o symbols.
	(mapcar (lambda (cg)
		  (if (not (symbolp cg))
		      (signal 'invalid-slot-type (list ':group cg))))
		customg)

	;; First up, add this field into our new class.
	(eieio-add-new-field newc name init docstr type custom label customg
			     prot initarg alloc 'defaultoverride skip-nil)

	;; We need to id the group, and store them in a group list attribute.
	(mapcar (lambda (cg) (add-to-list 'groups cg)) customg)

	;; anyone can have an accessor function.  This creates a function
	;; of the specified name, and also performs a `defsetf' if applicable
	;; so that users can `setf' the space returned by this function
	(if acces
	    (progn
	      (eieio-defmethod acces
		(list (if (eq alloc :class) :STATIC :PRIMARY)
		      (list (list 'this cname))
		      (format
		       "Retrieves the slot `%s' from an object of class `%s'"
		       name cname)
		      (list 'eieio-oref 'this (list 'quote name))))
	      ;; Thanks Pascal Bourguignon <pjb@informatimago.com>
	      ;; For this complex macro.
	      (eval (macroexpand
		     (list  'defsetf acces '(widget) '(store)
			    (list 'list ''eieio-oset 'widget
				  (list 'quote (list 'quote name)) 'store))))
	      ;;`(defsetf ,acces (widget) (store) (eieio-oset widget ',cname store))
	      )
	  )
	;; If a writer is defined, then create a generic method of that
	;; name whose purpose is to set the value of the slot.
	(if writer
	    (progn
	      (eieio-defmethod writer
		(list (list (list 'this cname) 'value)
		      (format "Set the slot `%s' of an object of class `%s'"
			      name cname)
		      `(setf (slot-value this ',name) value)))
	      ))
	;; If a reader is defined, then create a generic method
	;; of that name whose purpose is to access this slot value.
	(if reader
	    (progn
	      (eieio-defmethod reader
		(list (list (list 'this cname))
		      (format "Access the slot `%s' from object of class `%s'"
			      name cname)
		      `(slot-value this ',name)))))
	)
      (setq fields (cdr fields)))

    ;; Now that everything has been loaded up, all our lists are backwards!  Fix that up now.
    (aset newc class-public-a (nreverse (aref newc class-public-a)))
    (aset newc class-public-d (nreverse (aref newc class-public-d)))
    (aset newc class-public-doc (nreverse (aref newc class-public-doc)))
    (aset newc class-public-type
	  (apply 'vector (nreverse (aref newc class-public-type))))
    (aset newc class-public-custom (nreverse (aref newc class-public-custom)))
    (aset newc class-public-custom-label (nreverse (aref newc class-public-custom-label)))
    (aset newc class-public-custom-group (nreverse (aref newc class-public-custom-group)))
    (aset newc class-protection (nreverse (aref newc class-protection)))
    (aset newc class-initarg-tuples (nreverse (aref newc class-initarg-tuples)))

    ;; The storage for class-class-allocation-type needs to be turned into
    ;; a vector now.
    (aset newc class-class-allocation-type
	  (apply 'vector (aref newc class-class-allocation-type)))

    ;; Also, take class allocated values, and vectorize them for speed.
    (aset newc class-class-allocation-values
	  (apply 'vector (aref newc class-class-allocation-values)))

    ;; Attach field symbols into an obarray, and store the index of
    ;; this field as the variable slot in this new symbol.  We need to
    ;; know about primes, because obarrays are best set in vectors of
    ;; prime number length, and we also need to make our vector small
    ;; to save space, and also optimal for the number of items we have.
    (let* ((cnt 0)
	   (pubsyms (aref newc class-public-a))
	   (prots (aref newc class-protection))
	   (l (length pubsyms))
	   (vl (let ((primes '( 3 5 7 11 13 17 19 23 29 31 37 41 43 47
				  53 59 61 67 71 73 79 83 89 97 101 )))
		 (while (and primes (< (car primes) l))
		   (setq primes (cdr primes)))
		 (car primes)))
	   (oa (make-vector vl 0))
	   (newsym))
      (while pubsyms
	(setq newsym (intern (symbol-name (car pubsyms)) oa))
	(set newsym cnt)
	(setq cnt (1+ cnt))
	(if (car prots) (put newsym 'protection (car prots)))
	(setq pubsyms (cdr pubsyms)
	      prots (cdr prots)))
      (aset newc class-symbol-obarray oa)
      )

    ;; Create the constructor function
    (if (class-option-assoc options :abstract)
	;; Abstract classes cannot be instantiated.  Say so.
	(let ((abs (class-option-assoc options :abstract)))
	  (if (not (stringp abs))
	      (setq abs (format "Class %s is abstract" cname)))
	  (fset cname
		`(lambda (&rest stuff)
		   ,(format "You cannot create a new object of type %s" cname)
		   (error ,abs))))

      ;; Non-abstract classes need a constructor.
      (fset cname
	    `(lambda (newname &rest fields)
	       ,(format "Create a new object with name NAME of class type %s" cname)
	       (apply 'constructor ,cname newname fields)))
      )

    ;; Set up a specialized doc string.
    ;; Use stored value since it is calculated in a non-trivial way
    (put cname 'variable-documentation
	 (class-option-assoc options :documentation))

    ;; We have a list of custom groups.  Store them into the options.
    (let ((g (class-option-assoc options :custom-groups)))
      (mapcar (lambda (cg) (add-to-list 'g cg)) groups)
      (if (memq :custom-groups options)
	  (setcar (cdr (memq :custom-groups options)) g)
	(setq options (cons :custom-groups (cons g options)))))

    ;; Set up the options we have collected.
    (aset newc class-options options)

    ;; if this is a superclass, clear out parent (which was set to the
    ;; default superclass eieio-default-superclass)
    (if clearparent (aset newc class-parent nil))

    ;; Create the cached default object.
    (let ((cache (make-vector (+ (length (aref newc class-public-a))
				 3) nil)))
      (aset cache 0 'object)
      (aset cache object-class cname)
      (aset cache object-name 'default-cache-object)
      (let ((eieio-skip-typecheck t))
	;; All type-checking has been done to our satisfaction
	;; before this call.  Don't waste our time in this call..
	(eieio-set-defaults cache t))
      (aset newc class-default-object-cache cache))

    ;; Return our new class object
    newc
    ))

(defun eieio-perform-slot-validation-for-default (field spec value skipnil)
  "For FIELD, signal if SPEC does not match VALUE.
If SKIPNIL is non-nil, then if VALUE is nil, return t."
  (let ((val (eieio-default-eval-maybe value)))
    (if (and (not eieio-skip-typecheck)
	     (not (and skipnil (null val)))
	     (not (eieio-perform-slot-validation spec val)))
	(signal 'invalid-slot-type (list field spec val)))))

(defun eieio-add-new-field (newc a d doc type cust label custg prot init alloc
				 &optional defaultoverride skipnil)
  "Add into NEWC attribute A.
If A already exists in NEWC, then do nothing.  If it doesn't exist,
then also add in D (defualt), DOC, TYPE, CUST, LABEL, CUSTG, PROT, and INIT arg.
Argument ALLOC specifies if the field is allocated per instance, or per class.
If optional DEFAULTOVERRIDE is non-nil, then if A exists in NEWC,
we must override it's value for a default.
Optional argument SKIPNIL indicates if type checking should be skipped
if default value is nil."
  ;; Make sure we duplicate those items that are sequences.
  (if (sequencep d) (setq d (copy-sequence d)))
  (if (sequencep type) (setq type (copy-sequence type)))
  (if (sequencep cust) (setq cust (copy-sequence cust)))
  (if (sequencep custg) (setq custg (copy-sequence custg)))

  ;; To prevent override information w/out specification of storage,
  ;; we need to do this little hack.
  (if (member a (aref newc class-class-allocation-a)) (setq alloc ':class))

  (if (or (not alloc) (and (symbolp alloc) (eq alloc ':instance)))
      ;; In this case, we modify the INSTANCE version of a given slot.
      ;; Only add this element if it is so-far unique
      (if (not (member a (aref newc class-public-a)))
	  (progn
	    (eieio-perform-slot-validation-for-default a type d skipnil)
	    (aset newc class-public-a (cons a (aref newc class-public-a)))
	    (aset newc class-public-d (cons d (aref newc class-public-d)))
	    (aset newc class-public-doc (cons doc (aref newc class-public-doc)))
	    (aset newc class-public-type (cons type (aref newc class-public-type)))
	    (aset newc class-public-custom (cons cust (aref newc class-public-custom)))
	    (aset newc class-public-custom-label (cons label (aref newc class-public-custom-label)))
	    (aset newc class-public-custom-group (cons custg (aref newc class-public-custom-group)))
	    (aset newc class-protection (cons prot (aref newc class-protection)))
	    (aset newc class-initarg-tuples (cons (cons init a) (aref newc class-initarg-tuples)))
	    )
	;; When defaultoverride is true, we are usually adding new local
	;; attributes which must override the default value of any field
	;; passed in by one of the parent classes.
	(if defaultoverride
	    (progn
	      ;; There is a match, and we must override the old value.
	      (let* ((ca (aref newc class-public-a))
		     (np (member a ca))
		     (num (- (length ca) (length np)))
		     (dp (if np (nthcdr num (aref newc class-public-d))
			   nil))
		     (tp (if np (nth num (aref newc class-public-type))))
		     )
		(if (not np)
		    (error "Eieio internal error overriding default value for %s"
			   a)
		  ;; If type is passed in, is it the same?
		  (if (not (eq type t))
		      (if (not (equal type tp))
			  (error
			   "Child slot type `%s' does not match inherited type `%s' for `%s'"
			   type tp a)))
		  ;; If we have a repeat, only update the initarg...
		  (eieio-perform-slot-validation-for-default a tp d skipnil)
		  (setcar dp d)
		  ;; If we have a new initarg, check for it.
		  (when init
		    (let* ((inits (aref newc class-initarg-tuples))
			   (inita (rassq a inits)))
		      ;; Replace the CAR of the associate INITA.
		      ;;(message "Initarg: %S replace %s" inita init)
		      (setcar inita init)
		      ))
		  ;; TODO:
		  ;;  For other slots (protection, etc) we should get the
		  ;;  original value, and make sure each is equal to the
		  ;;  last value and throw an error, or accept it.
		  )))))
    (let ((value (eieio-default-eval-maybe d)))
      (if (not (member a (aref newc class-class-allocation-a)))
	  (progn
	    (eieio-perform-slot-validation-for-default a type value skipnil)
	    ;; Here we have found a :class version of a slot.  This
	    ;; requires a very different aproach.
	    (aset newc class-class-allocation-a (cons a (aref newc class-class-allocation-a)))
	    (aset newc class-class-allocation-doc (cons doc (aref newc class-class-allocation-doc)))
	    (aset newc class-class-allocation-type (cons type (aref newc class-class-allocation-type)))
	    (aset newc class-class-allocation-custom (cons cust (aref newc class-class-allocation-custom)))
	    (aset newc class-class-allocation-custom-label (cons label (aref newc class-class-allocation-custom-label)))
	    (aset newc class-class-allocation-custom-group (cons custg (aref newc class-class-allocation-custom-group)))
	    (aset newc class-class-allocation-protection (cons prot (aref newc class-class-allocation-protection)))
  ;; Default value is stored in the 'values section, since new objects
	    ;; can't initialize from this element.
	    (aset newc class-class-allocation-values (cons value (aref newc class-class-allocation-values))))
	(if defaultoverride
	    (progn
	      ;; There is a match, and we must override the old value.
	      (let* ((ca (aref newc class-class-allocation-a))
		     (np (member a ca))
		     (num (- (length ca) (length np)))
		     (dp (if np
			     (nthcdr num
				     (aref newc class-class-allocation-values))
			   nil))
		     (tp (if np (nth num (aref newc class-class-allocation-type))
			   nil)))
		(if (not np)
		    (error "Eieio internal error overriding default value for %s"
			   a)
		  ;; If type is passed in, is it the same?
		  (if (not (eq type t))
		      (if (not (equal type tp))
			  (error
			   "Child slot type `%s' does not match inherited type `%s' for `%s'"
			   type tp a)))
		  ;; If we have a repeat, only update the vlaue...
		  (eieio-perform-slot-validation-for-default a tp value skipnil)
		  (setcar dp value))
		)))))
    ))

(defun eieio-copy-parents-into-subclass (newc parents)
  "Copy into NEWC the fields of PARENTS.
Follow the rules of not overwritting early parents when applying to
the new child class."
  (let ((ps (aref newc class-parent))
	(sn (class-option-assoc (aref newc class-options)
				':allow-nil-initform)))
    (while ps
      ;; First, duplicate all the fields of the parent.
      (let ((pcv (class-v (car ps))))
	(let ((pa (aref pcv class-public-a))
	      (pd (aref pcv class-public-d))
	      (pdoc (aref pcv class-public-doc))
	      (ptype (aref pcv class-public-type))
	      (pcust (aref pcv class-public-custom))
	      (plabel (aref pcv class-public-custom-label))
	      (pcustg (aref pcv class-public-custom-group))
	      (pprot (aref pcv class-protection))
	      (pinit (aref pcv class-initarg-tuples))
	      (i 0))
	  (while pa
	    (eieio-add-new-field newc
				 (car pa) (car pd) (car pdoc) (aref ptype i)
				 (car pcust) (car plabel) (car pcustg)
				 (car pprot) (car-safe (car pinit)) nil nil sn)
	    ;; Increment each value.
	    (setq pa (cdr pa)
		  pd (cdr pd)
		  pdoc (cdr pdoc)
		  i (1+ i)
		  pcust (cdr pcust)
		  plabel (cdr plabel)
		  pcustg (cdr pcustg)
		  pprot (cdr pprot)
		  pinit (cdr pinit))
	    )) ;; while/let
	;; Now duplicate all the class alloc fields.
	(let ((pa (aref pcv class-class-allocation-a))
	      (pdoc (aref pcv class-class-allocation-doc))
	      (ptype (aref pcv class-class-allocation-type))
	      (pcust (aref pcv class-class-allocation-custom))
	      (plabel (aref pcv class-class-allocation-custom-label))
	      (pcustg (aref pcv class-class-allocation-custom-group))
	      (pprot (aref pcv class-class-allocation-protection))
	      (pval (aref pcv class-class-allocation-values))
	      (i 0))
	  (while pa
	    (eieio-add-new-field newc
				 (car pa) (aref pval i) (car pdoc) (aref ptype i)
				 (car pcust) (car plabel) (car pcustg)
				 (car pprot) nil ':class sn)
	    ;; Increment each value.
	    (setq pa (cdr pa)
		  pdoc (cdr pdoc)
		  pcust (cdr pcust)
		  plabel (cdr plabel)
		  pcustg (cdr pcustg)
		  pprot (cdr pprot)
		  i (1+ i))
	    ))) ;; while/let
      ;; Loop over each parent class
      (setq ps (cdr ps)))
    ))

;;; CLOS style implementation of object creators.
;;
(defun make-instance (class &rest initargs)
  "Make a new instance of CLASS with NAME and initialization based on INITARGS.
The class' constructor requires a name for use when printing.
`make-instance' in CLOS doesn't use names the way Emacs does, so the
class is used as the name slot instead when INITARGS doesn't start with
a string.  The rest of INITARGS are label/value pairs.  The label's
are the symbols created with the :initarg tag from the `defclass' call.
The value is the value stored in that slot.
CLASS is a symbol.  Defclass will assign the class symbol to itself, so
the shortcut (make-instance foo) will work.  The form (make-instance 'foo)
is more robust."
  (if (and (car initargs) (stringp (car initargs)))
      (apply (class-constructor class) initargs)
    (apply  (class-constructor class)
	    (cond ((symbolp class) (symbol-name class))
		  (t (format "%S" class)))
	    initargs)))


;;; CLOS methods and generics
;;
(defmacro defgeneric (method args &optional doc-string)
  "Create a generic function METHOD.  ARGS is ignored.
DOC-STRING is the base documentation for this class.  A generic
function has no body, as it's purpose is to decide which method body
is appropriate to use.  Use `defmethod' to create methods, and it
calls defgeneric for you.  With this implementation the arguments are
currently ignored.  You can use `defgeneric' to apply specialized
top level documentation to a method."
  `(eieio-defgeneric (quote ,method) ,doc-string))

(defun eieio-defgeneric-form (method doc-string)
  "The lambda form that would be used as the function defined on METHOD.
All methods should call the same EIEIO function for dispatch.
DOC-STRING is the documentation attached to METHOD."
  `(lambda (&rest local-args)
     ,doc-string
     (eieio-generic-call (quote ,method) local-args)))

(defun eieio-defgeneric (method doc-string)
  "Engine part to `defgeneric' macro defining METHOD with DOC-STRING."
  (if (and (fboundp method) (not (generic-p method))
	   (or (byte-code-function-p (symbol-function method))
	       (not (eq 'autoload (car (symbol-function method)))))
	   )
      (error "You cannot create a generic/method over an existing symbol: %s"
	     method))
  ;; Don't do this over and over.
  (unless (fboundp 'method)
    ;; This defun tells emacs where the first definition of this
    ;; method is defined.
    `(defun ,method nil)
    ;; Apply the actual body of this function.
    (fset method (eieio-defgeneric-form method doc-string))
    ;; Make sure the method tables are installed.
    (eieiomt-install method)
    ;; Return the method
    'method))

(defun eieio-unbind-method-implementations (method)
  "Make the generic method METHOD have no implementations..
It will leave the original generic function in place, but remove
reference to all implementations of METHOD."
  (put method 'eieio-method-tree nil)
  (put method 'eieio-method-obarray nil))

(defmacro defmethod (method &rest args)
  "Create a new METHOD through `defgeneric' with ARGS.
ARGS lists any keys (such as :BEFORE, :PRIMARY, :AFTER, or :STATIC),
the arglst, and doc string, and eventually the body, such as:

 (defmethod mymethod [:BEFORE | :PRIMARY | :AFTER | :STATIC] (args)
    doc-string body)"
  `(eieio-defmethod (quote ,method) (quote ,args)))

(defun eieio-defmethod (method args)
  "Work part of the `defmethod' macro defining METHOD with ARGS."
  (let ((key nil) (body nil) (firstarg nil) (argfix nil) (argclass nil) loopa)
    ;; find optional keys
    (setq key
	  (cond ((eq ':BEFORE (car args))
		 (setq args (cdr args))
		 method-before)
		((eq ':AFTER (car args))
		 (setq args (cdr args))
		 method-after)
		((eq ':PRIMARY (car args))
		 (setq args (cdr args))
		 method-primary)
		((eq ':STATIC (car args))
		 (setq args (cdr args))
		 method-static)
		;; Primary key
		(t method-primary)))
    ;; get body, and fix contents of args to be the arguments of the fn.
    (setq body (cdr args)
	  args (car args))
    (setq loopa args)
    ;; Create a fixed version of the arguments
    (while loopa
      (setq argfix (cons (if (listp (car loopa)) (car (car loopa)) (car loopa))
			 argfix))
      (setq loopa (cdr loopa)))
    ;; make sure there is a generic
    (eieio-defgeneric
     method
     (if (stringp (car body))
	 (car body) (format "Generically created method `%s'" method)))
    ;; create symbol for property to bind to.  If the first arg is of
    ;; the form (varname vartype) and `vartype' is a class, then
    ;; that class will be the type symbol.  If not, then it will fall
    ;; under the type `primary' which is a non-specific calling of the
    ;; function.
    (setq firstarg (car args))
    (if (listp firstarg)
	(progn
	  (setq argclass  (nth 1 firstarg))
	  (if (not (class-p argclass))
	      (error "Unknown class type %s in method parameters"
		     (nth 1 firstarg))))
      (if (= key -1)
	  (signal 'wrong-type-argument (list :STATIC 'non-class-arg)))
      ;; generics are higher
      (setq key (+ key 3)))
    ;; Put this lambda into the symbol so we can find it
    (if (byte-code-function-p (car-safe body))
	(eieiomt-add method (car-safe body) key argclass)
      (eieiomt-add method (append (list 'lambda (reverse argfix)) body)
		   key argclass))
    )
  method)

;;; Slot type validation
;;
(defun eieio-perform-slot-validation (spec value)
  "Return non-nil if SPEC does not match VALUE."
  ;; typep is in cl-macs
  (or (eq spec t)			; t always passes
      (eq value eieio-unbound)		; unbound always passes
      (typep value spec)))

(defun eieio-validate-slot-value (class field-idx value field)
  "Make sure that for CLASS referencing FIELD-IDX, that VALUE is valid.
Checks the :type specifier.
FIELD is the field that is being checked, and is only used when throwing
and error."
  (if eieio-skip-typecheck
      nil
    ;; Trim off object IDX junk added in for the object index.
    (setq field-idx (- field-idx 3))
    (let ((st (aref (aref (class-v class) class-public-type) field-idx)))
      (if (not (eieio-perform-slot-validation st value))
	  (signal 'invalid-slot-type (list class field st value))))))

(defun eieio-validate-class-slot-value (class field-idx value field)
  "Make sure that for CLASS referencing FIELD-IDX, that VALUE is valid.
Checks the :type specifier.
FIELD is the field that is being checked, and is only used when throwing
and error."
  (if eieio-skip-typecheck
      nil
    (let ((st (aref (aref (class-v class) class-class-allocation-type)
		    field-idx)))
      (if (not (eieio-perform-slot-validation st value))
	  (signal 'invalid-slot-type (list class field st value))))))

(defun eieio-barf-if-slot-unbound (value instance slotname fn)
  "Throw a signal if VALUE is a representation of an UNBOUND slot.
INSTANCE is the object being referenced.  SLOTNAME is the offending
slot.  If the slot is ok, return VALUE.
Argument FN is the function calling this verifier."
  (if (and (eq value eieio-unbound) (not eieio-skip-typecheck))
      (slot-unbound instance (object-class instance) slotname fn)
    value))

;;; Missing types that are useful to me.
;;
(defun boolean-p (bool)
  "Return non-nil if BOOL is nil or t."
  (or (null bool) (eq bool t)))

;;; Get/Set slots in an object.
;;
(defmacro oref (obj field)
  "Retrieve the value stored in OBJ in the slot named by FIELD.
Field is the name of the slot when created by `defclass' or the label
created by the :initarg tag."
  `(eieio-oref ,obj (quote ,field)))

(defun eieio-oref (obj field)
  "Return the value in OBJ at FIELD in the object vector."
  (if (not (or (object-p obj) (class-p obj)))
      (signal 'wrong-type-argument (list '(or object-p class-p) obj)))
  (if (not (symbolp field))
      (signal 'wrong-type-argument (list 'symbolp field)))
  (let* ((class (if (class-p obj) obj (aref obj object-class)))
	 (c (eieio-field-name-index class obj field)))
    (if (not c)
	;; It might be missing because it is a :class allocated field.
	;; Lets check that info out.
	(if (setq c (eieio-class-field-name-index class field))
	    ;; Oref that slot.
	    (aref (aref (class-v class) class-class-allocation-values) c)
	  ;; The slot-missing method is a cool way of allowing an object author
	  ;; to intercept missing slot definitions.  Since it is also the LAST
	  ;; thing called in this fn, it's return value would be retrieved.
	  (slot-missing obj field 'oref)
	  ;;(signal 'invalid-slot-name (list (object-name obj) field))
	  )
      (if (not (object-p obj))
	  (signal 'wrong-type-argument (list 'object-p obj)))
      (eieio-barf-if-slot-unbound (aref obj c) obj field 'oref))))

(defalias 'slot-value 'eieio-oref)
(defalias 'set-slot-value 'eieio-oset)

;; This alias is needed so that functions can be written
;; for defaults, but still behave like lambdas.
(defmacro lambda-default (&rest cdr)
  "The same as `lambda' but is used as a default value in `defclass'.
As such, the form (lambda-default ARGS DOCSTRING INTERACTIVE BODY) is
self quoting.  This macro is meant for the sole purpose of quoting
lambda expressions into class defaults.  Any `lambda-default'
expression is automatically transformed into a `lambda' expression
when copied from the defaults into a new object.  The use of
`oref-default', however, will return a `lambda-default' expression.
CDR is function definition and body."
  ;; This definition is copied directly from subr.el for lambda
  (list 'function (cons 'lambda-default cdr)))

(put 'lambda-default 'lisp-indent-function 'defun)
(put 'lambda-default 'byte-compile 'byte-compile-lambda-form)

(defmacro oref-default (obj field)
  "Gets the default value of OBJ (maybe a class) for FIELD.
The default value is the value installed in a class with the :initform
tag.  FIELD can be the slot name, or the tag specified by the :initarg
tag in the `defclass' call."
  `(eieio-oref-default ,obj (quote ,field)))

(defun eieio-oref-default (obj field)
  "Does the work for the macro `oref-default' with similar parameters.
Fills in OBJ's FIELD with it's default value."
  (if (not (or (object-p obj) (class-p obj))) (signal 'wrong-type-argument (list 'object-p obj)))
  (if (not (symbolp field)) (signal 'wrong-type-argument (list 'symbolp field)))
  (let* ((cl (if (object-p obj) (aref obj object-class) obj))
	 (c (eieio-field-name-index cl obj field)))
    (if (not c)
	;; It might be missing because it is a :class allocated field.
	;; Lets check that info out.
	(if (setq c
		  (eieio-class-field-name-index cl field))
	    ;; Oref that slot.
	    (aref (aref (class-v cl) class-class-allocation-values)
		  c)
	  (slot-missing obj field 'oref-default)
	  ;;(signal 'invalid-slot-name (list (class-name cl) field))
	  )
      (eieio-barf-if-slot-unbound
       (let ((val (nth (- c 3) (aref (class-v cl) class-public-d))))
	 (eieio-default-eval-maybe val))
       obj cl 'oref-default))))

(defun eieio-default-eval-maybe (val)
  "Check VAL, and return what `oref-default' would provide."
  ;; check for functions to evaluate
  (if (and (listp val) (equal (car val) 'lambda))
      (funcall val)
    ;; check for quoted things, and unquote them
    (if (and (listp val) (eq (car val) 'quote))
	(car (cdr val))
      ;; return it verbatim
      (if (and (listp val) (eq (car val) 'lambda-default))
	  (let ((s (copy-sequence val)))
	    (setcar s 'lambda)
	    s)
	val))))

;;; Object Set macros
;;
(defmacro oset (obj field value)
  "Set the value in OBJ for slot FIELD to VALUE.
FIELD is the slot name as specified in `defclass' or the tag created
with in the :initarg slot.  VALUE can be any Lisp object."
  `(eieio-oset ,obj (quote ,field) ,value))

(defun eieio-oset (obj field value)
  "Does the work for the macro `oset'.
Fills in OBJ's FIELD with VALUE."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (if (not (symbolp field)) (signal 'wrong-type-argument (list 'symbolp field)))
  (let ((c (eieio-field-name-index (object-class-fast obj) obj field)))
    (if (not c)
	;; It might be missing because it is a :class allocated field.
	;; Lets check that info out.
	(if (setq c
		  (eieio-class-field-name-index (aref obj object-class) field))
	    ;; Oset that slot.
	    (progn
	      (eieio-validate-class-slot-value (object-class-fast obj) c value field)
	      (aset (aref (class-v (aref obj object-class))
			  class-class-allocation-values)
		    c value))
	  ;; See oref for comment on `slot-missing'
	  (slot-missing obj field 'oset value)
	  ;;(signal 'invalid-slot-name (list (object-name obj) field))
	  )
      (eieio-validate-slot-value (object-class-fast obj) c value field)
      (aset obj c value))))

(defmacro oset-default (class field value)
  "Set the default slot in CLASS for FIELD to VALUE.
The default value is usually set with the :initform tag during class
creation.  This allows users to change the default behavior of classes
after they are created."
  `(eieio-oset-default ,class (quote ,field) ,value))

(defun eieio-oset-default (class field value)
  "Does the work for the macro `oset-default'.
Fills in the default value in CLASS' in FIELD with VALUE."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (if (not (symbolp field)) (signal 'wrong-type-argument (list 'symbolp field)))
  (let* ((scoped-class class)
	 (c (eieio-field-name-index class nil field)))
    (if (not c)
	;; It might be missing because it is a :class allocated field.
	;; Lets check that info out.
	(if (setq c (eieio-class-field-name-index class field))
	    (progn
	      ;; Oref that slot.
	      (eieio-validate-class-slot-value class c value field)
	      (aset (aref (class-v class) class-class-allocation-values) c
		    value))
	  (signal 'invalid-slot-name (list (class-name class) field)))
      (eieio-validate-slot-value class c value field)
      ;; Set this into the storage for defaults.
      (setcar (nthcdr (- c 3) (aref (class-v class) class-public-d))
	      value)
      ;; Take the value, and put it into our cache object.
      (eieio-oset (aref (class-v class) class-default-object-cache)
		  field value)
      )))

;;; Handy CLOS macros
;;
(defmacro with-slots (spec-list object &rest body)
  "The macro with-slots establishes a lexical environment for
referring to the slots in the instance named by the given
slot-names as though they were variables. Within such a context
the value of the slot can be specified by using its slot name,
as if it were a lexically bound variable. Both setf and setq
can be used to set the value of the slot."
  ;; Transform the spec-list into a symbol-macrolet spec-list.
  (let ((mappings (mapcar (lambda (entry)
			    (let ((var  (if (listp entry) (car entry) entry))
				  (slot (if (listp entry) (cadr entry) entry)))
			      (list var `(slot-value ,object ',slot))))
			  spec-list)))
    (append (list 'symbol-macrolet mappings)
	    body)))
(put 'with-slots 'lisp-indent-function 2)


;;; Simple generators, and query functions.  None of these would do
;;  well embedded into an object.
;;
(defmacro object-class-fast (obj) "Return the class struct defining OBJ with no check."
  `(aref ,obj object-class))
  
(defun class-name (class) "Return a Lisp like symbol name for CLASS."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  ;; I think this is supposed to return a symbol, but to me CLASS is a symbol,
  ;; and I wanted a string.  Arg!
  (format "#<class %s>" (symbol-name class)))

(defun object-name (obj &optional extra)
  "Return a Lisp like symbol string for object OBJ.
If EXTRA, include that in the string returned to represent the symbol."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (format "#<%s %s%s>" (symbol-name (object-class-fast obj))
	  (aref obj object-name) (or extra "")))

(defun object-name-string (obj) "Return a string which is OBJ's name."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (aref obj object-name))

(defun object-set-name-string (obj name) "Set the string which is OBJ's NAME."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (if (not (stringp name)) (signal 'wrong-type-argument (list 'stringp name)))
  (aset obj object-name name))

(defun object-class (obj) "Return the class struct defining OBJ."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (object-class-fast obj))
(defalias 'class-of 'object-class)

(defun object-class-name (obj) "Return a Lisp like symbol name for OBJ's class."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (class-name (object-class-fast obj)))

(defmacro class-parents-fast (class) "Return parent classes to CLASS with no check."
  `(aref (class-v ,class) class-parent))

(defun class-parents (class) "Return parent classes to CLASS.  (overload of variable)."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (class-parents-fast class))

(defmacro class-children-fast (class) "Return child classes to CLASS with no check."
  `(aref (class-v ,class) class-children))

(defun class-children (class) "Return child classses to CLASS."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (class-children-fast class))

(defmacro class-parent-fast (class) "Return first parent class to CLASS with no check."
  `(car (class-parents-fast ,class)))

(defmacro class-parent (class) "Return first parent class to CLASS.  (overload of variable)."
  `(car (class-parents ,class)))

(defmacro same-class-fast-p (obj class) "Return t if OBJ is of class-type CLASS with no error checking."
  `(eq (aref ,obj object-class) ,class))

(defun same-class-p (obj class) "Return t if OBJ is of class-type CLASS."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (same-class-fast-p obj class))

(defun object-of-class-p (obj class)
  "Return non-nil if OBJ is an instance of CLASS or CLASS' subclasses."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  ;; class will be checked one layer down
  (child-of-class-p (aref obj object-class) class))
;; Backwards compatibility
(defalias 'obj-of-class-p 'object-of-class-p)

(defun child-of-class-p (child class)
  "If CHILD class is a subclass of CLASS."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (if (not (class-p child)) (signal 'wrong-type-argument (list 'class-p child)))
  (let ((p nil))
    (while (and child (not (eq child class)))
      (setq p (append p (aref (class-v child) class-parent))
	    child (car p)
	    p (cdr p)))
    (if child t)))

(defun object-slots (obj) "List of slots available in OBJ."
  (if (not (object-p obj)) (signal 'wrong-type-argument (list 'object-p obj)))
  (aref (class-v (object-class-fast obj)) class-public-a))

(defun class-slot-initarg (class slot) "Fetch from CLASS, SLOT's :initarg."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (let ((ia (aref (class-v class) class-initarg-tuples))
	(f nil))
    (while (and ia (not f))
      (if (eq (cdr (car ia)) slot)
	  (setq f (car (car ia))))
      (setq ia (cdr ia)))
    f))

;;; CLOS queries into classes and slots
;;
(defun slot-boundp (object slot)
  "Non-nil if OBJECT's SLOT is bound.
Setting a slot's value makes it bound.  Calling `slot-makeunbound' will
make a slot unbound.
OBJECT can be an instance or a class."
  ;; Skip typechecking while retrieving this value.
  (let ((eieio-skip-typecheck t))
    ;; Return nil if the magic symbol is in there.
    (if (object-p object)
	(if (eq (eieio-oref object slot) eieio-unbound) nil t)
      (if (class-p object)
	  (if (eq (eieio-oref-default object slot) eieio-unbound) nil t)
	(signal 'wrong-type-argument (list 'object-p object))))))

(defun slot-makeunbound (object slot)
  "In OBJECT, make SLOT unbound."
  (eieio-oset object slot eieio-unbound))

(defun slot-exists-p (object-or-class slot)
  "Non-nil if OBJECT-OR-CLASS has SLOT."
  (let ((cv (class-v (cond ((object-p object-or-class)
			    (object-class object-or-class))
			   ((class-p object-or-class)
			    object-or-class))
		     )))
    (or (memq slot (aref cv class-public-a))
	(memq slot (aref cv class-class-allocation-a)))
    ))

(defun find-class (symbol &optional errorp)
  "Return the class that SYMBOL represents. (CLOS function)
If there is no class, nil is returned if ERRORP is nil."
  (if (not (class-p symbol))
      (if errorp (signal 'wrong-type-argument (list 'class-p symbol))
	nil)
    (class-v symbol)))

;;; Slightly more complex utility functions for objects
;;
(defun object-assoc (key field list)
  "Return non-nil if KEY is `equal' to the FIELD of the car of objects in LIST.
The value is actually the element of LIST whose field equals KEY."
  (if (not (listp list)) (signal 'wrong-type-argument (list 'listp list)))
  (while (and list (not (condition-case nil
			    ;; This prevents errors for missing slots.
			    (equal key (eieio-oref (car list) field))
			  (error nil))))
    (setq list (cdr list)))
  (car list))

(defun object-assoc-list (field list)
  "Return an association list with the contents of FIELD as the key element.
LIST must be a list of objects with FIELD in it.
This is useful when you need to do completing read on an object group."
  (if (not (listp list)) (signal 'wrong-type-argument (list 'listp list)))
  (let ((assoclist nil))
    (while list
      (setq assoclist (cons (cons (eieio-oref (car list) field)
				  (car list))
			    assoclist))
      (setq list (cdr list)))
    (nreverse assoclist)))

(defun object-assoc-list-safe (field list)
  "Return an association list with the contents of FIELD as the key element.
LIST must be a list of objects, but those objects do not need to have
FIELD in it.  If it does not, then that element is left out of the association
list."
  (if (not (listp list)) (signal 'wrong-type-argument (list 'listp list)))
  (let ((assoclist nil))
    (while list
      (if (slot-exists-p (car list) field)
	  (setq assoclist (cons (cons (eieio-oref (car list) field)
				      (car list))
				assoclist)))
      (setq list (cdr list)))
    (nreverse assoclist)))

(defun object-add-to-list (object slot item &optional append)
  "In OBJECT's SLOT, add ITEM to the pre-existing list of elements.
Optional argument APPEND indicates we need to append to the list.
If ITEM already exists in the list in SLOT, then it is not added.
Comparison is done with `equal' through the `member' function call.
If SLOT is unbound, bind it to the list containing ITEM."
  (let (ov)
    ;; Find the originating list.
    (if (not (slot-boundp object slot))
	(setq ov (list item))
      (setq ov (eieio-oref object slot))
      ;; turn it into a list.
      (unless (listp ov)
	(setq ov (list ov)))
      ;; Do the combination
      (if (not (member item ov))
	  (setq ov
		(if append
		    (append ov (list item))
		  (cons item ov)))))
    ;; Set back into the slot.
    (eieio-oset object slot ov)))

(defun object-remove-from-list (object slot item)
  "In OBJECT's SLOT, remove occurrences ITEM.
If ITEM already exists in the list in SLOT, then it is not added.
Comparison is done with `equal' through the `delete' function call.
If SLOT is unbound, do nothing."
  (if (not (slot-boundp object slot))
      nil
    (eieio-oset object slot (delete item (eieio-oref object slot)))))

;;; EIEIO internal search functions
;;
(defun eieio-field-originating-class-p (start-class field)
  "Return Non-nil if START-CLASS is the first class to define FIELD.
This is for testing if `scoped-class' is the class that defines FIELD
so that we can protect private slots."
  (let ((par (class-parents start-class))
	(ret t))
    (if (not par)
	t
      (while (and par ret)
	(if (intern-soft (symbol-name field)
			 (aref (class-v (car par))
			       class-symbol-obarray))
	    (setq ret nil))
	(setq par (cdr par)))
      ret)))

(defun eieio-field-name-index (class obj field)
  "In CLASS for OBJ find the index of the named FIELD.
The field is a symbol which is installed in CLASS by the `defclass'
call.  OBJ can be nil, but if it is an object, and the slot in question
is protected, access will be allowed if obj is a child of the currently
`scoped-class'.
If FIELD is the value created with :initarg instead,
reverse-lookup that name, and recurse with the associated slot value."
  ;; Removed checks to outside this call
  (let* ((fsym (intern-soft (symbol-name field)
			    (aref (class-v class)
				  class-symbol-obarray)))
	 (fsi (if (symbolp fsym) (symbol-value fsym) nil)))
    (if (integerp fsi)
	(cond
	 ((not (get fsym 'protection))
	  (+ 3 fsi))
	 ((and (eq (get fsym 'protection) 'protected)
	       scoped-class
	       (or (child-of-class-p class scoped-class)
		   (and (object-p obj)
			(child-of-class-p class (object-class obj)))))
	  (+ 3 fsi))
	 ((and (eq (get fsym 'protection) 'private)
	       (or (and scoped-class
			(eieio-field-originating-class-p scoped-class field))
		   eieio-initializing-object))
	  (+ 3 fsi))
	 (t nil))
      (let ((fn (eieio-initarg-to-attribute class field)))
	(if fn (eieio-field-name-index class obj fn) nil)))))

(defun eieio-class-field-name-index (class field)
  "In CLASS find the index of the named FIELD.
The field is a symbol which is installed in CLASS by the `defclass'
call.  If FIELD is the value created with :initarg instead,
reverse-lookup that name, and recurse with the associated slot value."
  ;; This will happen less often, and with fewer slots.  Do this the
  ;; storage cheap way.
  (let* ((a (aref (class-v class) class-class-allocation-a))
	 (l1 (length a))
	 (af (memq field a))
	 (l2 (length af)))
    ;; Slot # is length of the total list, minus the remaining list of
    ;; the found slot.
    (if af (- l1 l2))))

;;; CLOS generics internal function handling
;;
(defvar eieio-generic-call-methodname nil
  "When using `call-next-method', provides a context on how to do it.")
(defvar eieio-generic-call-arglst nil
  "When using `call-next-method', provides a context for parameters.")
(defvar eieio-generic-call-key nil
  "When using `call-next-method', provides a context for the current key.
Keys are a number representing :BEFORE, :PRIMARY, and :AFTER methods.")
(defvar eieio-generic-call-next-method-list nil
  "When executing a PRIMARY or STATIC method, track the 'next-method'.
During executions, the list is first generated, then as each next method
is called, the next method is popped off the stack.")

(defun eieio-generic-call (method args)
  "Call METHOD with ARGS.
ARGS provides the context on which implementation to use.
This should only be called from a generic function."
  ;; We must expand our arguments first as they are always
  ;; passed in as quoted symbols
  (let ((newargs nil) (mclass nil)  (lambdas nil) (tlambdas nil) (keys nil)
	(eieio-generic-call-methodname method)
	(eieio-generic-call-arglst args)
	(firstarg nil)
	(primarymethodlist nil))
    ;; get a copy
    (setq newargs args
	  firstarg (car newargs))
    ;; Is the class passed in autoloaded?
    ;; Since class names are also constructors, they can be autoloaded
    ;; via the autoload command.  Check for this, and load them in.
    ;; It's ok if it doesn't turn out to be a class.  Probably want that
    ;; function loaded anyway.
    (if (and (symbolp firstarg)
	     (fboundp firstarg)
	     (listp (symbol-function firstarg))
	     (eq 'autoload (car (symbol-function firstarg))))
	(load (nth 1 (symbol-function firstarg))))
    ;; lookup the forms to use
    (cond ((object-p firstarg)
	   (setq mclass (object-class-fast firstarg)))
	  ((class-p firstarg)
	   (setq mclass firstarg
		 )))
    ;; Now create a list in reverse order of all the calls we have
    ;; make in order to successfully do this right.  Rules:
    ;; 1) Only call generics if scoped-class is not defined
    ;;    This prevents multiple calls in the case of recursion
    ;; 2) Only call static if this is a static method.
    ;; 3) Only call specifics if the definition allows for them.
    ;; 4) Call in order based on :BEFORE, :PRIMARY, and :AFTER
    (when (object-p firstarg)
      ;; Non-static calls do all this stuff.

      ;; :AFTER methods
      (setq tlambdas
	    (if mclass
		(eieiomt-method-list method method-after mclass)
	      (list (eieio-generic-form method method-after nil)))
	    ;;(or (and mclass (eieio-generic-form method method-after mclass))
	    ;;	(eieio-generic-form method method-after nil))
	    )
      (setq lambdas (append tlambdas lambdas)
	    keys (append (make-list (length tlambdas) method-after) keys))
      
      ;; :PRIMARY methods
      (setq tlambdas
	    (or (and mclass (eieio-generic-form method method-primary mclass))
		(eieio-generic-form method method-primary nil)))
      (when tlambdas
	(setq lambdas (cons tlambdas lambdas)
	      keys (cons method-primary keys)
	      primarymethodlist
	      (eieiomt-method-list method method-primary mclass)))

      ;; :BEFORE methods
      (setq tlambdas
	    (if mclass
		(eieiomt-method-list method method-before mclass)
	      (list (eieio-generic-form method method-before nil)))
	    ;;(or (and mclass (eieio-generic-form method method-before mclass))
	    ;;	(eieio-generic-form method method-before nil))
	    )
      (setq lambdas (append tlambdas lambdas)
	    keys (append (make-list (length tlambdas) method-before) keys))
      )

    ;; If there were no methods found, then there could be :STATIC methods.
    (when (not lambdas)
      (setq tlambdas
	    (eieio-generic-form method method-static mclass))
      (setq lambdas (cons tlambdas lambdas)
	    keys (cons method-static keys)
	    primarymethodlist  ;; Re-use even with bad name here
	    (eieiomt-method-list method method-static mclass)))

    ;; Now loop through all occurances forms which we must execute
    ;; (which are happily sorted now) and execute them all!
    (let ((rval nil) (lastval nil) (rvalever nil) (found nil))
      (while lambdas
	(if (car lambdas)
	    (let* ((scoped-class (cdr (car lambdas)))
		   (eieio-generic-call-key (car keys))
		   (has-return-val
		    (or (= eieio-generic-call-key method-primary)
			(= eieio-generic-call-key method-static)))
		   (eieio-generic-call-next-method-list
		    ;; Use the cdr, as the first element is the fcn
		    ;; we are calling right now.
		    (when has-return-val (cdr primarymethodlist)))
		   )
	      (setq found t)
	      ;;(setq rval (apply (car (car lambdas)) newargs))
	      (setq lastval (apply (car (car lambdas)) newargs))
	      (when has-return-val
	      	(setq rval lastval
	      	      rvalever t))
	      ))
	(setq lambdas (cdr lambdas)
	      keys (cdr keys)))
      (if (not found)
	  (if (object-p (car args))
	      (setq rval (no-applicable-method (car args) method)
		    rvalever t)
	    (signal
	     'no-method-definition
	     (list method args))))
      ;; Right Here... it could be that lastval is returned when
      ;; rvalever is nil.  Is that right?
      rval)))

(defun eieiomt-method-list (method key class)
  "Return an alist list of methods lambdas.
METHOD is the method name.
KEY represents either :BEFORE, or :AFTER methods.
CLASS is the starting class to search from in the method tree."
  (let ((lambdas nil)
	(mclass (list class)))
    (while mclass
      (when (car mclass)
	;; lookup the form to use for the PRIMARY object for the next level
	(let ((tmpl (eieio-generic-form method key (car mclass))))
	  (when (or (not lambdas) 
		    ;; This prevents duplicates coming out of the
		    ;; class method optimizer.  Perhaps we should
		    ;; just not optimize before/afters?
		    (not (eq (car tmpl) (car (car lambdas)))))
	    (setq lambdas (cons tmpl lambdas))
	    (if (null (car lambdas))
		(setq lambdas (cdr lambdas))))))
      ;; Add new classes to mclass
      (setq mclass (append (cdr mclass) (eieiomt-next (car mclass))))
      )
    (if (eq key method-after)
	lambdas
      (nreverse lambdas))))

(defun next-method-p ()
  "Return a list of lambdas which qualify as the `next-method'."
  eieio-generic-call-next-method-list)

(defun call-next-method (&rest replacement-args)
  "Call the next logical method from another method.
The next logical method is the method belong to the parent class of
the currently running method.  If REPLACEMENT-ARGS is non-nil, then
use them instead of `eieio-generic-call-arglst'.  The generic arg list
are the arguments passed in at the top level."
  (if (not scoped-class)
      (error "Call-next-method not called within a class specific method"))
  (if (and (/= eieio-generic-call-key method-primary)
	   (/= eieio-generic-call-key method-static))
      (error "Cannot `call-next-method' except in :PRIMARY or :STATIC methods")
    )
  (let ((newargs (or replacement-args eieio-generic-call-arglst))
	(next (car eieio-generic-call-next-method-list))
	(returnval nil)
	)
    (if (or (not next) (not (car next)))
	(no-next-method (car newargs))
      (let* ((eieio-generic-call-next-method-list
	      (cdr eieio-generic-call-next-method-list))
	     (scoped-class (cdr next))
	     (fcn (car next))
	     )
	(apply fcn newargs)
	))))

;;;
;; eieio-method-tree : eieiomt-
;;
;; Stored as eieio-method-tree in property list of a generic method
;;
;; (eieio-method-tree . [BEFORE PRIMARY AFTER
;;                       genericBEFORE genericPRIMARY genericAFTER])
;; and
;; (eieio-method-obarray . [BEFORE PRIMARY AFTER
;;                          genericBEFORE genericPRIMARY genericAFTER])
;;    where the association is a vector.
;;    (aref 0  -- all static methods.
;;    (aref 1  -- all methods classified as :BEFORE
;;    (aref 2  -- all methods classified as :PRIMARY
;;    (aref 3  -- all methods classified as :AFTER
;;    (aref 4  -- a generic classified as :BEFORE
;;    (aref 5  -- a generic classified as :PRIMARY
;;    (aref 6  -- a generic classified as :AFTER
;;
(defvar eieiomt-optimizing-obarray nil
  "While mapping atoms, this contain the obarray being optimized.")

(defun eieiomt-install (method-name)
  "Install the method tree, and obarray onto METHOD-NAME.
Do not do the work if they already exist."
  (let ((emtv (get method-name 'eieio-method-tree))
	(emto (get method-name 'eieio-method-obarray)))
    (if (or (not emtv) (not emto))
	(progn
	  (setq emtv (put method-name 'eieio-method-tree
			  (make-vector method-num-fields nil))
		emto (put method-name 'eieio-method-obarray
			  (make-vector method-num-fields nil)))
	  (aset emto 0 (make-vector 11 0))
	  (aset emto 1 (make-vector 11 0))
	  (aset emto 2 (make-vector 41 0))
	  (aset emto 3 (make-vector 11 0))
	  ))))

(defun eieiomt-add (method-name method key class)
  "Add to METHOD-NAME the forms METHOD in a call position KEY for CLASS.
METHOD-NAME is the name created by a call to `defgeneric'.
METHOD are the forms for a given implementation.
KEY is an integer (see comment in eieio.el near this function) which
is associated with the :STATIC :BEFORE :PRIMARY and :AFTER tags.
It also indicates if CLASS is defined or not.
CLASS is the class this method is associated with."
  (if (or (> key method-num-fields) (< key 0))
      (error "Eieiomt-add: method key error!"))
  (let ((emtv (get method-name 'eieio-method-tree))
	(emto (get method-name 'eieio-method-obarray)))
    ;; Make sure the method tables are available.
    (if (or (not emtv) (not emto))
	(error "Programmer error: eieiomt-add"))
    ;; only add new cells on if it doesn't already exist!
    (if (assq class (aref emtv key))
	(setcdr (assq class (aref emtv key)) method)
      (aset emtv key (cons (cons class method) (aref emtv key))))
    ;; Add function definition into newly created symbol, and store
    ;; said symbol in the correct obarray, otherwise use the
    ;; other array to keep this stuff
    (if (< key method-num-lists)
	(let ((nsym (intern (symbol-name class) (aref emto key))))
	  (fset nsym method)))
    ;; Now optimize the entire obarray
    (if (< key method-num-lists)
	(let ((eieiomt-optimizing-obarray (aref emto key)))
	  (mapatoms 'eieiomt-sym-optimize eieiomt-optimizing-obarray)))
    ))

(defun eieiomt-next (class)
  "Return the next parent class for CLASS.
If CLASS is a superclass, return variable `eieio-default-superclass'.  If CLASS
is variable `eieio-default-superclass' then return nil.  This is different from
function `class-parent' as class parent returns nil for superclasses.  This
function performs no type checking!"
  ;; No type-checking because all calls are made from functions which
  ;; are safe and do checking for us.
  (or (class-parents-fast class)
      (if (eq class 'eieio-default-superclass)
	  nil
	'(eieio-default-superclass))))

(defun eieiomt-sym-optimize (s)
  "Find the next class above S which has a function body for the optimizer."
  ;; (message "Optimizing %S" s)
  (let ((es (intern-soft (symbol-name s))) ;external symbol of class
	(ov nil)
	(cont t))
    ;; This converts ES from a single symbol to a list of parent classes.
    (setq es (eieiomt-next es))
    ;; Loop over ES, then it's children individually.
    ;; We can have multiple hits only at one level of the parent tree.
    (while (and es cont)
      (setq ov (intern-soft (symbol-name (car es)) eieiomt-optimizing-obarray))
      (if (fboundp ov)
	  (progn
	    (set s ov)			;store ov as our next symbol
	    (setq cont nil))
	(setq es (append (cdr es) (eieiomt-next (car es))))))
    ;; If there is no nearest call, then set our value to nil
    (if (not es) (set s nil))
    ))

(defun eieio-generic-form (method key class)
 "Return the lambda form belonging to METHOD using KEY based upon CLASS.
If CLASS is not a class then use `generic' instead.  If class has no
form, but has a parent class, then trace to that parent class.  The
first time a form is requested from a symbol, an optimized path is
memoized for future faster use."
 (let ((emto (aref (get method 'eieio-method-obarray)
		   (if class key (+ key 3)))))
   (if (class-p class)
       ;; 1) find our symbol
       (let ((cs (intern-soft (symbol-name class) emto)))
	 (if (not cs)
	     ;; 2) If there isn't one, then make one.
	     ;;    This can be slow since it only occurs once
	     (progn
	       (setq cs (intern (symbol-name class) emto))
	       ;; 2.1) Cache it's nearest neighbor with a quick optimize
	       ;;      which should only occur once for this call ever
	       (let ((eieiomt-optimizing-obarray emto))
		 (eieiomt-sym-optimize cs))))
	 ;; 3) If it's bound return this one.
	 (if (fboundp  cs)
	     (cons cs (aref (class-v class) class-symbol))
	   ;; 4) If it's not bound then this variable knows something
	   (if (symbol-value cs)
	       (progn
		 ;; 4.1) This symbol holds the next class in it's value
		 (setq class (symbol-value cs)
		       cs (intern-soft (symbol-name class) emto))
		 ;; 4.2) The optimizer should always have chosen a
		 ;;      function-symbol
		 ;;(if (fboundp cs)
		 (cons cs (aref (class-v (intern (symbol-name class)))
				class-symbol))
		   ;;(error "EIEIO optimizer: erratic data loss!"))
		 )
	       ;; There never will be a funcall...
	       nil)))
     ;; for a generic call, what is a list, is the function body we want.
     (let ((emtl (aref (get method 'eieio-method-tree)
		       (if class key (+ key 3)))))
       (if emtl
	   ;; The car of EMTL is supposed to be a class, which in this
	   ;; case is nil, so skip it.
	   (cons (cdr (car emtl)) nil)
	 nil)))))

;;;
;; Way to assign fields based on a list.  Used for constructors, or
;; even resetting an object at run-time
;;
(defun eieio-set-defaults (obj &optional set-all)
  "Take object OBJ, and reset all fields to their defaults.
If SET-ALL is non-nil, then when a default is nil, that value is
reset.  If SET-ALL is nil, the fields are only reset if the default is
not nil."
  (let ((scoped-class (aref obj object-class))
	(eieio-initializing-object t)
	(pub (aref (class-v (aref obj object-class)) class-public-a)))
    (while pub
      (let ((df (eieio-oref-default obj (car pub))))
	(if (and (listp df) (eq (car df) 'lambda-default))
	    (progn
	      (setq df (copy-sequence df))
	      (setcar df 'lambda)))
	(if (or df set-all)
	    (eieio-oset obj (car pub) df)))
      (setq pub (cdr pub)))))

(defun eieio-initarg-to-attribute (class initarg)
  "For CLASS, convert INITARG to the actual attribute name.
If there is no translation, pass it in directly (so we can cheat if
need be.. May remove that later...)"
  (let ((tuple (assoc initarg (aref (class-v class) class-initarg-tuples))))
    (if tuple
	(cdr tuple)
      nil)))

(defun eieio-attribute-to-initarg (class attribute)
  "In CLASS, convert the ATTRIBUTE into the corresponding init argument tag.
This is usually a symbol that starts with `:'."
  (let ((tuple (rassoc attribute (aref (class-v class) class-initarg-tuples))))
    (if tuple
	(car tuple)
      nil)))


;;; Here are some special types of errors
;;
(intern "no-method-definition")
(put 'no-method-definition 'error-conditions '(no-method-definition error))
(put 'no-method-definition 'error-message "No method definition")

(intern "no-next-method")
(put 'no-next-method 'error-conditions '(no-next-method error))
(put 'no-next-method 'error-message "No next method")

(intern "invalid-slot-name")
(put 'invalid-slot-name 'error-conditions '(invalid-slot-name error))
(put 'invalid-slot-name 'error-message "Invalid slot name")

(intern "invalid-slot-type")
(put 'invalid-slot-type 'error-conditions '(invalid-slot-type error nil))
(put 'invalid-slot-type 'error-message "Invalid slot type")

(intern "unbound-slot")
(put 'unbound-slot 'error-conditions '(unbound-slot error nil))
(put 'unbound-slot 'error-message "Unbound slot")

;;; Here are some CLOS items that need the CL package
;;

(defsetf slot-value (obj field) (store) (list 'eieio-oset obj field store))
(defsetf eieio-oref (obj field) (store) (list 'eieio-oset obj field store))

;; The below setf method was written by Arnd Kohrs <kohrs@acm.org>
(define-setf-method oref (obj field) 
  (let ((obj-temp (gensym)) 
	(field-temp (gensym)) 
	(store-temp (gensym))) 
    (list (list obj-temp field-temp) 
	  (list obj `(quote ,field)) 
	  (list store-temp) 
	  (list 'set-slot-value obj-temp field-temp
		store-temp)
	  (list 'slot-value obj-temp field-temp))))


;;;
;; We want all objects created by EIEIO to have some default set of
;; behaviours so we can create object utilities, and allow various
;; types of error checking.  To do this, create the default EIEIO
;; class, and when no parent class is specified, use this as the
;; default.  (But don't store it in the other classes as the default,
;; allowing for transparent support.)
;;

(defclass eieio-default-superclass nil
  nil
  "Default class used as parent class for superclasses.
Its fields are automatically adopted by such superclasses but not
stored in the `parent' field.  When searching for attributes or
methods, when the last parent is found, the search will recurse to
this class."
  :abstract t)

(defalias 'standard-class 'eieio-default-superclass)

(defmethod constructor :STATIC
  ((class eieio-default-superclass) newname &rest fields)
  "Default constructor for CLASS `eieio-defualt-superclass'.
NEWNAME is the name to be given to the constructed object.
FIELDS are the initialization fields used by `shared-initialize'.
This static method is called when an object is constructed.
It allocates the vector used to represent an EIEIO object, and then
calls `shared-initialize' on that object."
  (let* ((new-object (copy-sequence (aref (class-v class)
					  class-default-object-cache))))
    ;; Update the name for the newly created object.
    (aset new-object object-name newname)
    ;; Call the initialize method on the new object with the fields
    ;; that were passed down to us.
    (initialize-instance new-object fields)
    ;; Return the created object.
    new-object))

(defmethod shared-initialize ((obj eieio-default-superclass) fields)
  "Set fields of OBJ with FIELDS which is a list of name/value pairs.
Called from the constructor routine."
  (let ((scoped-class (aref obj object-class)))
    (while fields
      (let ((rn (eieio-initarg-to-attribute (object-class-fast obj)
					    (car fields))))
	(if (not rn)
	    (slot-missing obj (car fields) 'oset (car (cdr fields))))
	(eieio-oset obj rn (car (cdr fields))))
      (setq fields (cdr (cdr fields))))))

(defmethod initialize-instance ((this eieio-default-superclass)
				&optional fields)
    "Constructs the new object THIS based on FIELDS.
FIELDS is a tagged list where odd numbered elements are tags, and
even numbered elements are the values to store in the tagged slot.  If
you overload the `initialize-instance', there you will need to call
`shared-initialize' yourself, or you can call `call-next-method' to
have this constructor called automatically.  If these steps are not
taken, then new objects of your class will not have their values
dynamically set from FIELDS."
    ;; First, see if any of our defaults are `lambda', and
    ;; re-evaluate them and apply the value to our slots.
    (let* ((scoped-class (class-v (aref this object-class)))
	   (slot (aref scoped-class class-public-a))
	   (defaults (aref scoped-class class-public-d)))
      (while slot
	(if (and (listp (car defaults))
		 (eq 'lambda (car (car defaults))))
	    (eieio-oset this (car slot) (funcall (car defaults))))
	(setq slot (cdr slot)
	      defaults (cdr defaults))))
    ;; Shared initialize will parse our fields for us.
    (shared-initialize this fields))

(defmethod slot-missing ((object eieio-default-superclass) slot-name
			 operation &optional new-value)
  "Slot missing is invoked when an attempt to access a slot in OBJECT fails.
SLOT-NAME is the name of the failed slot, OPERATION is the type of access
that was requested, and optional NEW-VALUE is the value that was desired
to be set."
  (signal 'invalid-slot-name (list (object-name object)
				   slot-name)))

(defmethod slot-unbound ((object eieio-default-superclass)
			 class slot-name fn)
  "Slot unbound is invoked during an attempt to reference an unbound slot.
OBJECT is the instance of the object being reference.  CLASS is the
class of OBJECT, and SLOT-NAME is the offending slot.  This function
throws the signal `unbound-slot'.  You can overload this function and
return the value to use in place of the unbound value.
Argument FN is the function signaling this error.
Use `slot-boundp' to determine if a slot is bound or not."
  (signal 'unbound-slot (list (class-name class) (object-name object)
			      slot-name fn)))

(defmethod no-applicable-method ((object eieio-default-superclass)
				 method)
  "Called if there are no implementations for OBJECT in METHOD.
OBJECT is the object which has no method implementation."
  (signal 'no-method-definition (list method (object-name object)))
  )

(defmethod no-next-method ((object eieio-default-superclass)
			   &rest args)
  "Called from `call-next-method' when no additional methods are available.
OBJECT is othe object being called on `call-next-method'.
ARGS are the  arguments it is called by.
This method throws `no-next-method' by default.  Override this
method to not throw an error, and it's return value becomes the
return value of `call-next-method'."
  (signal 'no-next-method (list (object-name object) args))
)

(defmethod clone ((obj eieio-default-superclass) &rest params)
  "Make a deep copy of OBJ, and then apply PARAMS.
PARAMS is a parameter list of the same form as INITIALIZE-INSTANCE
which are applied to change the object.  When overloading `clone', be
sure to call `call-next-method' first and modify the returned object."
  (let ((nobj (copy-sequence obj))
	(nm (aref obj object-name))
	(passname (and params (stringp (car params))))
	(num 1))
    (if params (shared-initialize nobj (if passname (cdr params) params)))
    (if (not passname)
	(save-match-data
	  (if (string-match "-\\([0-9]+\\)" nm)
	      (setq num (1+ (string-to-number (match-string 1 nm)))
		    nm (substring nm 0 (match-beginning 0))))
	  (aset nobj object-name (concat nm "-" (int-to-string num))))
      (aset nobj object-name (car params)))
    nobj))

(defmethod destructor ((this eieio-default-superclass) &rest params)
  "Destructor for cleaning up any dynamic links to our object.
Argument THIS is the object being destroyed.  PARAMS are additional
ignored parameters."
  ;; No cleanup... yet.
  )

(defmethod object-print ((this eieio-default-superclass) &rest strings)
  "Pretty printer for object THIS.  Call function `object-name' with STRINGS.
The default method for printing object THIS is to use the
function `object-name'.  At times it could be useful to put a summary
of the object into the default #<notation> string.  Overload this
function to allow summaries of your objects to be used by eieio
browsing tools.  The optional parameter STRINGS is for additional
summary parts to put into the name string.  When passing in extra
strings from child classes, always remember to prepend a space."
  (object-name this (apply 'concat strings)))

(defvar eieio-print-depth 0
  "When printing, keep track of the current indentation depth.")

(defmethod object-write ((this eieio-default-superclass) &optional comment)
  "Write object THIS out to the current stream.
This writes out the vector version of this object.  Complex and recursive
object are discouraged from being written.
  If optional COMMENT is non-nil, include comments when outputting
this object."
  (when comment
    (princ ";; Object ")
    (princ (object-name-string this))
    (princ "\n")
    (princ comment)
    (princ "\n"))
  (let* ((cl (object-class this))
	 (cv (class-v cl)))
    ;; Now output readable lisp to recreate this object
    ;; It should look like this:
    ;; (<constructor> <name> <slot> <field> ... )
    ;; Each slot's field is writen using its :writer.
    (princ (make-string (* eieio-print-depth 2) ? ))
    (princ "(")
    (princ (symbol-name (class-constructor (object-class this))))
    (princ " \"")
    (princ (object-name-string this))
    (princ "\"\n")
    ;; Loop over all the public slots
    (let ((publa (aref cv class-public-a))
	  (publd (aref cv class-public-d))
	  (eieio-print-depth (1+ eieio-print-depth)))
      (while publa
	(when (slot-boundp this (car publa))
	  (let ((i (class-slot-initarg cl (car publa)))
		(v (eieio-oref this (car publa))))
	    (unless (or (not i) (equal v (car publd)))
	      (princ (make-string (* eieio-print-depth 2) ? ))
	      (princ (symbol-name i))
	      (princ " ")
	      (eieio-override-prin1 v)
	      (princ "\n"))))
	(setq publa (cdr publa) publd (cdr publd)))
      (princ (make-string (* eieio-print-depth 2) ? )))
    (princ ")\n")))

(defun eieio-override-prin1 (thing)
  "Perform a prin1 on THING taking advantage of object knowledge."
  (cond ((object-p thing)
	 (object-write thing))
	((listp thing)
	 (eieio-list-prin1 thing))
	((class-p thing)
	 (princ (class-name thing)))
	((symbolp thing)
	 (princ (concat "'" (symbol-name thing))))
	(t (prin1 thing))))

(defun eieio-list-prin1 (list)
  "Display LIST where list may contain objects."
  (if (not (object-p (car list)))
      (progn
	(princ "'")
	(prin1 list))
    (princ "(list ")
    (if (object-p (car list)) (princ "\n "))
    (while list
      (if (object-p (car list))
	  (object-write (car list))
	(princ "'")
	(prin1 (car list)))
      (princ " ")
      (setq list (cdr list)))
    (princ (make-string (* eieio-print-depth 2) ? ))
    (princ ")")))


;;; Unimplemented functions from CLOS
;;
(defun change-class (obj class)
  "Change the class of OBJ to type CLASS.
This may create or delete slots, but does not affect the return value
of `eq'."
  (error "Eieio: `change-class' is unimplemented"))

)


;;; Interfacing with edebug
;;
(defun eieio-edebug-prin1-to-string (object &optional noescape)
  "Display eieio OBJECT in fancy format.  Overrides the edebug default.
Optional argument NOESCAPE is passed to `prin1-to-string' when appropriate."
  (cond ((class-p object) (class-name object))
	((object-p object) (object-print object))
	((and (listp object) (or (class-p (car object))
				 (object-p (car object))))
	 (concat "(" (mapconcat 'eieio-edebug-prin1-to-string object " ") ")"))
	(t (prin1-to-string object noescape))))

(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec defmethod
	      (&define			; this means we are defining something
	       [&or name ("setf" :name setf name)]
	       ;; ^^ This is the methods symbol
	       [ &optional symbolp ]    ; this is key :BEFORE etc
	       list              ; arguments
	       [ &optional stringp ]    ; documentation string
	       def-body	                ; part to be debugged
	       ))
	    ;; The rest of the macros
	    (def-edebug-spec oref (form quote))
	    (def-edebug-spec oref-default (form quote))
	    (def-edebug-spec oset (form quote form))
	    (def-edebug-spec oset-default (form quote form))
	    (def-edebug-spec class-v form)
	    (def-edebug-spec class-p form)
	    (def-edebug-spec object-p form)
	    (def-edebug-spec class-constructor form)
	    (def-edebug-spec generic-p form)
	    (def-edebug-spec with-slots (list list def-body))
	    ;; I suspect this isn't the best way to do this, but when
	    ;; cust-print was used on my system all my objects
	    ;; appeared as "#1 =" which was not useful.  This allows
	    ;; edebug to print my objects in the nice way they were
	    ;; meant to with `object-print' and `class-name'
	    ;; (defalias 'edebug-prin1-to-string 'eieio-edebug-prin1-to-string)
	    )
	  )

(eval-after-load "cedet-edebug"
  '(progn
     (cedet-edebug-add-print-override '(class-p object) '(class-name object) )
     (cedet-edebug-add-print-override '(object-p object) '(object-print object) )
     (cedet-edebug-add-print-override '(and (listp object)
					    (or (class-p (car object)) (object-p (car object))))
				      '(cedet-edebug-prin1-recurse object) )
     ))

;;; Interfacing with imenu in emacs lisp mode
;;    (Only if the expression is defined)
;;
(if (eval-when-compile (boundp 'list-imenu-generic-expression))
(progn

(defun eieio-update-lisp-imenu-expression ()
  "Examine `lisp-imenu-generic-expression' and modify it to find `defmethod'."
  (let ((exp lisp-imenu-generic-expression))
    (while exp
      ;; it's of the form '( ( title expr indx ) ... )
      (let* ((subcar (cdr (car exp)))
	     (substr (car subcar)))
	(if (and (not (string-match "|method\\\\" substr))
		 (string-match "|advice\\\\" substr))
	    (setcar subcar
		    (replace-match "|advice\\|method\\" t t substr 0))))
      (setq exp (cdr exp)))))

(eieio-update-lisp-imenu-expression)

))

;;; Autoloading some external symbols, and hooking into the help system
;;

(autoload 'eieio-help-mode-augmentation-maybee "eieio-opt" "For buffers thrown into help mode, augment for eieio.")
(autoload 'eieio-browse "eieio-opt" "Create an object browser window" t)
(autoload 'eieio-describe-class "eieio-opt" "Describe CLASS defined by a string or symbol" t)
(autoload 'describe-class "eieio-opt" "Describe CLASS defined by a string or symbol" t)
(autoload 'eieio-describe-generic "eieio-opt" "Describe GENERIC defined by a string or symbol" t)
(autoload 'describe-generic "eieio-opt" "Describe GENERIC defined by a string or symbol" t)
(autoload 'eieiodoc-class "eieio-doc" "Create texinfo documentation about a class hierarchy." t)

(autoload 'customize-object "eieio-custom" "Create a custom buffer editing OBJ.")

;; make sure this shows up after the help mode hook.
(add-hook 'temp-buffer-show-hook 'eieio-help-mode-augmentation-maybee t)
(require 'advice)
(defadvice describe-variable (around eieio-describe activate)
  "Display the full documentation of FUNCTION (a symbol).
Returns the documentation as a string, also."
  (if (class-p (ad-get-arg 0))
      (eieio-describe-class (ad-get-arg 0))
    ad-do-it))

(defadvice describe-function (around eieio-describe activate)
  "Display the full documentation of VARIABLE (a symbol).
Returns the documentation as a string, also."
  (if (generic-p (ad-get-arg 0))
      (eieio-describe-generic (ad-get-arg 0))
    ad-do-it))

(provide 'eieio)
;;; eieio ends here
