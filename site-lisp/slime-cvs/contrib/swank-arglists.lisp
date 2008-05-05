;;; swank-arglists.lisp --- arglist related code ??
;;
;; Authors: Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;;          Tobias C. Rittweiler <tcr@freebits.de>
;;          and others 
;;
;; License: Public Domain
;;

(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank-require :swank-c-p-c))

(defun length= (seq n)
  "Test for whether SEQ contains N number of elements. I.e. it's equivalent
 to (= (LENGTH SEQ) N), but besides being more concise, it may also be more
 efficiently implemented."
  (etypecase seq 
    (list (do ((i n (1- i))
               (list seq (cdr list)))
              ((or (<= i 0) (null list))
               (and (zerop i) (null list)))))
    (sequence (= (length seq) n))))

(defun ensure-list (thing)
  (if (listp thing) thing (list thing)))

(defun recursively-empty-p (list)
  "Returns whether LIST consists only of arbitrarily nested empty lists."
  (cond ((not (listp list)) nil)
	((null list) t)
	(t (every #'recursively-empty-p list))))

(defun maybecall (bool fn &rest args)
  "Call FN with ARGS if BOOL is T. Otherwise return ARGS as multiple values."
  (if bool (apply fn args) (values-list args)))

(defun exactly-one-p (&rest values)
  "If exactly one value in VALUES is non-NIL, this value is returned.
Otherwise NIL is returned."
  (let ((found nil))
    (dolist (v values)
      (when v (if found
                  (return-from exactly-one-p nil)
                  (setq found v))))
    found))

(defun valid-operator-symbol-p (symbol)
  "Is SYMBOL the name of a function, a macro, or a special-operator?"
  (or (fboundp symbol)
      (macro-function symbol)
      (special-operator-p symbol)
      (eq symbol 'declare)))
  
(defun valid-operator-name-p (string)
  "Is STRING the name of a function, macro, or special-operator?"
  (let ((symbol (parse-symbol string)))
    (valid-operator-symbol-p symbol)))

(defun valid-function-name-p (form)
  (or (symbolp form)
      (and (consp form)
           (second form)
           (not (third form))
           (eq (first form) 'setf)
           (symbolp (second form)))))

(defslimefun arglist-for-echo-area (raw-specs &key arg-indices
                                                   print-right-margin print-lines)
  "Return the arglist for the first valid ``form spec'' in
RAW-SPECS. A ``form spec'' is a superset of functions, macros,
special-ops, declarations and type specifiers.

For more information about the format of ``raw form specs'' and
``form specs'', please see PARSE-FORM-SPEC."
  (handler-case 
      (with-buffer-syntax ()
        (multiple-value-bind (form-spec position newly-interned-symbols)
            (parse-first-valid-form-spec raw-specs #'read-conversatively-for-autodoc)
          (unwind-protect
               (when form-spec
                 (let ((arglist (arglist-from-form-spec form-spec :remove-args nil)))
                   (unless (eql arglist :not-available)
                     (multiple-value-bind (type operator arguments)
                         (split-form-spec form-spec)
                       (declare (ignore arguments))
                       (multiple-value-bind (stringified-arglist)
                           (decoded-arglist-to-string
                            arglist
                            :operator operator
                            :print-right-margin print-right-margin
                            :print-lines print-lines
                            :highlight (let ((index (nth position arg-indices)))
					 ;; don't highlight the operator
					 (and index (not (zerop index)) index)))
			 ;; Post formatting:
                         (case type
                           (:type-specifier (format nil "[Typespec] ~A" stringified-arglist))
                           (:declaration
			    (locally (declare (special *arglist-pprint-bindings*))
			      (with-bindings *arglist-pprint-bindings*
				(let ((op (%find-declaration-operator raw-specs position)))
				  (if op
				      (format nil "(~A ~A)" op stringified-arglist)
				      (format nil "[Declaration] ~A" stringified-arglist))))))
                           (t stringified-arglist)))))))
            (mapc #'unintern-in-home-package newly-interned-symbols))))
    (error (cond)
      (format nil "ARGLIST (error): ~A" cond))
    ))

(defun %find-declaration-operator (raw-specs position)
  (let ((op-rawspec (nth (1+ position) raw-specs)))
    (first (parse-form-spec op-rawspec #'read-conversatively-for-autodoc))))

;; This is a wrapper object around anything that came from Slime and
;; could not reliably be read. 
(defstruct (arglist-dummy
	     (:conc-name #:arglist-dummy.)
	     (:print-object (lambda (struct stream)
			      (with-struct (arglist-dummy. string-representation) struct
				(write-string string-representation stream)))))
  string-representation)

(defun read-conversatively-for-autodoc (string)
  "Tries to find the symbol that's represented by STRING. 

If it can't, this either means that STRING does not represent a
symbol, or that the symbol behind STRING would have to be freshly
interned. Because this function is supposed to be called from the
automatic arglist display stuff from Slime, interning freshly
symbols is a big no-no.

In such a case (that no symbol could be found), an object of type
ARGLIST-DUMMY is returned instead, which works as a placeholder
datum for subsequent logics to rely on."
  (let* ((string  (string-left-trim '(#\Space #\Tab #\Newline) string))
	 (quoted? (eql (aref string 0) #\')))
    (multiple-value-bind (symbol found?)
	(parse-symbol (if quoted? (subseq string 1) string))
      (if found?
	  (if quoted? `(quote ,symbol) symbol)
	  (make-arglist-dummy :string-representation string)))))


(defun parse-form-spec (raw-spec &optional reader)
  "Takes a raw (i.e. unparsed) form spec from SLIME and returns a
proper form spec for further processing within SWANK. Returns NIL
if RAW-SPEC could not be parsed. Symbols that had to be interned
in course of the conversion, are returned as secondary return value.

A ``raw form spec'' can be either: 

  i)   a list of strings representing a Common Lisp form

  ii)  a list of strings as of i), but which additionally 
       contains other raw form specs

  iii) one of:

     a)  (:declaration declspec) 

           where DECLSPEC is a raw form spec.

     b)  (:type-specifier typespec) 
       
           where TYPESPEC is a raw form spec.


A ``form spec'' is either

  1) a normal Common Lisp form

  2) a Common Lisp form with a list as its CAR specifying what namespace
     the operator is supposed to be interpreted in:

       a) ((:declaration decl-identifier) declarg1 declarg2 ...)

       b) ((:type-specifier typespec-op) typespec-arg1 typespec-arg2 ...)


Examples:

  (\"defmethod\")                               =>  (defmethod)
  (\"cl:defmethod\")                            =>  (cl:defmethod)
  (\"defmethod\" \"print-object\")              =>  (defmethod print-object)

  (\"foo\" (\"bar\" (\"quux\")) \"baz\"         =>  (foo (bar (quux)) baz)

  (:declaration \"optimize\" \"(optimize)\")    =>  ((:declaration optimize))
  (:declaration \"type\"     \"(type string)\") =>  ((:declaration type) string)
  (:type-specifier \"float\" \"(float)\")       =>  ((:type-specifier float))
  (:type-specifier \"float\" \"(float 0 100)\") =>  ((:type-specifier float) 0 100)
"
  (flet ((parse-extended-spec (raw-extension extension-flag)
           (when (and (stringp (first raw-extension)) ; (:DECLARATION (("a" "b" ("c")) "d"))
                      (nth-value 1 (parse-symbol (first raw-extension))))
	     (multiple-value-bind (extension introduced-symbols)
                 (read-form-spec raw-extension reader)
	       (unless (recursively-empty-p extension) ; (:DECLARATION (())) &c.
                 (destructuring-bind (identifier &rest args) extension
                   (values `((,extension-flag ,identifier) ,@args)
                           introduced-symbols)))))))
    (when (consp raw-spec)
      (destructure-case raw-spec
        ((:declaration raw-declspec)
         (parse-extended-spec raw-declspec :declaration))
        ((:type-specifier raw-typespec)
         (parse-extended-spec raw-typespec :type-specifier))
        (t
         (when (every #'(lambda (x) (or (stringp x) (consp x))) raw-spec)
           (destructuring-bind (raw-operator &rest raw-args) raw-spec
             (multiple-value-bind (operator found?) (parse-symbol raw-operator)
               (when (and found? (valid-operator-symbol-p operator))
                 (multiple-value-bind (parsed-args introduced-symbols)
                     (read-form-spec raw-args reader)
                   (values `(,operator ,@parsed-args) introduced-symbols)))))))))))


(defun split-form-spec (spec)
  "Returns all three relevant information a ``form spec''
contains: the operator type, the operator, and the operands."
  (destructuring-bind (operator-designator &rest arguments) spec
    (multiple-value-bind (type operator)
        (if (listp operator-designator)
            (values (first operator-designator) (second operator-designator))
            (values :function operator-designator)) ; functions, macros, special ops
      (values type operator arguments))))           ;  are all fbound.

(defun parse-first-valid-form-spec (raw-specs &optional reader)
  "Returns the first parsed form spec in RAW-SPECS that can
successfully be parsed. Additionally returns that spec's position
as secondary, and all newly interned symbols as tertiary return
value."
  (loop for raw-spec in raw-specs
	for pos upfrom 0
	do (multiple-value-bind (spec symbols) (parse-form-spec raw-spec reader)
	     (when spec (return (values spec pos symbols))))))

(defun read-form-spec (spec &optional reader)
  "Turns the ``raw form spec'' SPEC into a proper Common Lisp
form. As secondary return value, it returns all the symbols that
had to be newly interned during the conversion.

READER is a function that takes a string, and returns two values:
the Common Lisp datum that the string represents, a flag whether
the returned datum is a symbol and has been newly interned in
some package.

If READER is not explicitly given, the function 
READ-SOFTLY-FROM-STRING* is used instead."
  (when spec
    (with-buffer-syntax ()
      (call-with-ignored-reader-errors
       #'(lambda ()
           (let ((result) (newly-interned-symbols) (ok))
             (unwind-protect
                  (dolist (element spec (setq ok t))
                    (etypecase element
                      (string
                       (multiple-value-bind (sexp newly-interned?)
                           (funcall (or reader 'read-softly-from-string*) element)
                         (push sexp result)
                         (when newly-interned?
                           (push sexp newly-interned-symbols))))
                      (list
                       (multiple-value-bind (read-spec interned-symbols)
                           (read-form-spec element reader)
                         (push read-spec result)
                         (setf newly-interned-symbols
                               (append interned-symbols
                                       newly-interned-symbols))))))
               (unless ok
                 (mapc #'unintern-in-home-package newly-interned-symbols)))
             (values (nreverse result)
                     (nreverse newly-interned-symbols))))))))

(defun read-softly-from-string* (string)
  "Like READ-SOFTLY-FROM-STRING, but only returns the sexp and
the flag if a symbol had to be interned."
  (multiple-value-bind (sexp pos interned?)
      (read-softly-from-string string)
    ;; To make sure that we haven't got any junk from Emacs.
    (assert (= pos (length string)))
    (values sexp interned?)))

(defun read-softly-from-string (string)
  "Returns three values:

     1. the object resulting from READing STRING.

     2. The index of the first character in STRING that was not read.

     3. T if the object is a symbol that had to be newly interned
        in some package. (This does not work for symbols in
        compound forms like lists or vectors.)"
  (multiple-value-bind (symbol found? symbol-name package) (parse-symbol string)
    (if found?
        (values symbol (length string) nil)
        (multiple-value-bind (sexp pos) (read-from-string string)
          (values sexp pos
                  (when (symbolp sexp)
                    (prog1 t
                      ;; assert that PARSE-SYMBOL didn't parse incorrectly.
                      (assert (and (equal symbol-name (symbol-name sexp))
                                   (eq package (symbol-package sexp)))))))))))

(defun unintern-in-home-package (symbol)
  (unintern symbol (symbol-package symbol)))

(defstruct (arglist (:conc-name arglist.) (:predicate arglist-p))
  provided-args         ; list of the provided actual arguments
  required-args         ; list of the required arguments
  optional-args         ; list of the optional arguments
  key-p                 ; whether &key appeared
  keyword-args          ; list of the keywords
  rest                  ; name of the &rest or &body argument (if any)
  body-p                ; whether the rest argument is a &body
  allow-other-keys-p    ; whether &allow-other-keys appeared
  aux-args              ; list of &aux variables
  any-p                 ; whether &any appeared
  any-args              ; list of &any arguments  [*]
  known-junk            ; &whole, &environment
  unknown-junk)         ; unparsed stuff

;;;
;;; [*] The &ANY lambda keyword is an extension to ANSI Common Lisp,
;;;     and is only used to describe certain arglists that cannot be
;;;     described in another way. 
;;;
;;;     &ANY is very similiar to &KEY but while &KEY is based upon
;;;     the idea of a plist (key1 value1 key2 value2), &ANY is a
;;;     cross between &OPTIONAL, &KEY and *FEATURES* lists:
;;;
;;;        a) (&ANY :A :B :C) means that you can provide any (non-null)
;;;              set consisting of the keywords `:A', `:B', or `:C' in
;;;              the arglist. E.g. (:A) or (:C :B :A).
;;;
;;;        (This is not restricted to keywords only, but any self-evaluating
;;;         expression is allowed.)
;;;
;;;        b) (&ANY (key1 v1) (key2 v2) (key3 v3)) means that you can
;;;              provide any (non-null) set consisting of lists where
;;;              the CAR of the list is one of `key1', `key2', or `key3'.
;;;              E.g. ((key1 100) (key3 42)), or ((key3 66) (key2 23))
;;;
;;;
;;;     For example, a) let us describe the situations of EVAL-WHEN as
;;;
;;;       (EVAL-WHEN (&ANY :compile-toplevel :load-toplevel :execute) &BODY body)
;;;
;;;     and b) let us describe the optimization qualifiers that are valid
;;;     in the declaration specifier `OPTIMIZE':
;;;
;;;       (DECLARE (OPTIMIZE &ANY (compilation-speed 1) (safety 1) ...))
;;;

;; FIXME: This really ought to be rewritten.
(defun print-arglist (arglist &key operator highlight)
  (let ((index 0)
        (need-space nil))
    (labels ((print-arg (arg)
               (typecase arg
                 (arglist               ; destructuring pattern
                  (print-arglist arg))
                 (optional-arg
		  (let ((enc-arg (encode-optional-arg arg)))
		    (if (symbolp enc-arg)
			(princ enc-arg)
			(destructuring-bind (var &optional (initform nil initform-p)) enc-arg
			    (pprint-logical-block (nil nil :prefix "(" :suffix ")")
			      (format t "~A~:[~; ~S~]" var initform-p initform))))))
                 (keyword-arg
                  (let ((enc-arg (encode-keyword-arg arg)))
                    (etypecase enc-arg
                      (symbol (princ enc-arg))
                      ((cons symbol)
		       (destructuring-bind (keyarg initform) enc-arg
			 (pprint-logical-block (nil nil :prefix "(" :suffix ")")
			   (format t "~A ~S" keyarg initform))))
                      ((cons cons)
		       (destructuring-bind ((keyword-name var) &optional (initform nil initform-p))
			   enc-arg
			 (pprint-logical-block (nil nil :prefix "(" :suffix ")")
			   (pprint-logical-block (nil nil :prefix "(" :suffix ")")
			     (format t "~S ~A" keyword-name var))
			   (when initform-p
			     (format t " ~S" initform))))))))
                 (t           ; required formal or provided actual arg
                  (if (keywordp arg)
		      (prin1 arg)	; for &ANY args.
		      (princ arg)))))
             (print-space ()
               (ecase need-space
                 ((nil))
                 ((:miser)
                  (write-char #\space)
                  (pprint-newline :miser))
                 ((t)
                  (write-char #\space)
                  (pprint-newline :fill)))
               (setq need-space t))
             (print-with-space (obj)
               (print-space)
               (print-arg obj))
             (print-with-highlight (arg &optional (index-ok-p #'=))
               (print-space)
               (cond 
                 ((and highlight (funcall index-ok-p index highlight))
                  (princ "===> ")
                  (print-arg arg)
                  (princ " <==="))
                 (t
                  (print-arg arg)))
               (incf index)))
      (pprint-logical-block (nil nil :prefix "(" :suffix ")")
        (when operator
          (print-with-highlight operator)
          (setq need-space :miser))
	(mapc #'print-with-highlight
	      (arglist.provided-args arglist))
        (mapc #'print-with-highlight
              (arglist.required-args arglist))
        (when (arglist.optional-args arglist)
          (print-with-space '&optional)
          (mapc #'print-with-highlight 
                (arglist.optional-args arglist)))
        (when (arglist.key-p arglist)
          (print-with-space '&key)
          (mapc #'print-with-space
                (arglist.keyword-args arglist)))
        (when (arglist.allow-other-keys-p arglist)
          (print-with-space '&allow-other-keys))
        (when (arglist.any-args arglist)
          (print-with-space '&any)
          (mapc #'print-with-space
                (arglist.any-args arglist)))
        (cond ((not (arglist.rest arglist)))
              ((arglist.body-p arglist)
               (print-with-space '&body)
               (print-with-highlight (arglist.rest arglist) #'<=))
              (t
               (print-with-space '&rest)
               (print-with-highlight (arglist.rest arglist) #'<=)))
        (mapc #'print-with-space                 
              (arglist.unknown-junk arglist))))))  

(defvar *arglist-pprint-bindings*
  '((*print-case*     . :downcase)
    (*print-pretty*   . t)
    (*print-circle*   . nil)
    (*print-readably* . nil)
    (*print-level*    . 10)
    (*print-length*   . 20)
    (*print-escape*   . nil))) ; no package qualifiers.

(defun decoded-arglist-to-string (arglist
                                  &key operator highlight (package *package*)
                                  print-right-margin print-lines)
  "Print the decoded ARGLIST for display in the echo area.  The
argument name are printed without package qualifiers and pretty
printing of (function foo) as #'foo is suppressed.  If HIGHLIGHT is
non-nil, it must be the index of an argument; highlight this argument.
If OPERATOR is non-nil, put it in front of the arglist."
  (with-output-to-string (*standard-output*)
    (with-standard-io-syntax
      (with-bindings *arglist-pprint-bindings*
	(let ((*package* package)
	      (*print-right-margin* print-right-margin)
	      (*print-lines* print-lines))       
	  (print-arglist arglist :operator operator :highlight highlight))))))

(defslimefun variable-desc-for-echo-area (variable-name)
  "Return a short description of VARIABLE-NAME, or NIL."
  (with-buffer-syntax ()
    (let ((sym (parse-symbol variable-name)))
      (if (and sym (boundp sym))
          (let ((*print-pretty* nil) (*print-level* 4)
                (*print-length* 10) (*print-circle* t))
             (format nil "~A => ~A" sym (symbol-value sym)))))))

(defun decode-required-arg (arg)
  "ARG can be a symbol or a destructuring pattern."
  (etypecase arg
    (symbol arg)
    (list   (decode-arglist arg))))

(defun encode-required-arg (arg)
  (etypecase arg
    (symbol arg)
    (arglist (encode-arglist arg))))

(defstruct (keyword-arg 
            (:conc-name keyword-arg.)
            (:constructor make-keyword-arg (keyword arg-name default-arg)))
  keyword
  arg-name
  default-arg)

(defun decode-keyword-arg (arg)
  "Decode a keyword item of formal argument list.
Return three values: keyword, argument name, default arg."
  (cond ((symbolp arg)
         (make-keyword-arg (intern (symbol-name arg) keyword-package)
                           arg
                           nil))
        ((and (consp arg)
              (consp (car arg)))
         (make-keyword-arg (caar arg)
                           (decode-required-arg (cadar arg))
                           (cadr arg)))
        ((consp arg)
         (make-keyword-arg (intern (symbol-name (car arg)) keyword-package)
                           (car arg)
                           (cadr arg)))
        (t
         (error "Bad keyword item of formal argument list"))))

(defun encode-keyword-arg (arg)
  (cond
    ((arglist-p (keyword-arg.arg-name arg))
     ;; Destructuring pattern
     (let ((keyword/name (list (keyword-arg.keyword arg)
                               (encode-required-arg
                                (keyword-arg.arg-name arg)))))
       (if (keyword-arg.default-arg arg)
           (list keyword/name
                 (keyword-arg.default-arg arg))
           (list keyword/name))))
    ((eql (intern (symbol-name (keyword-arg.arg-name arg)) 
                  keyword-package)
          (keyword-arg.keyword arg))
     (if (keyword-arg.default-arg arg)
         (list (keyword-arg.arg-name arg)
               (keyword-arg.default-arg arg))
         (keyword-arg.arg-name arg)))
    (t
     (let ((keyword/name (list (keyword-arg.keyword arg)
                               (keyword-arg.arg-name arg))))
       (if (keyword-arg.default-arg arg)
           (list keyword/name
                 (keyword-arg.default-arg arg))
           (list keyword/name))))))

(progn
  (assert (equalp (decode-keyword-arg 'x) 
                  (make-keyword-arg :x 'x nil)))
  (assert (equalp (decode-keyword-arg '(x t)) 
                  (make-keyword-arg :x 'x t)))
  (assert (equalp (decode-keyword-arg '((:x y)))
                  (make-keyword-arg :x 'y nil)))
  (assert (equalp (decode-keyword-arg '((:x y) t))
                  (make-keyword-arg :x 'y t))))

(defstruct (optional-arg 
            (:conc-name optional-arg.)
            (:constructor make-optional-arg (arg-name default-arg)))
  arg-name
  default-arg)

(defun decode-optional-arg (arg)
  "Decode an optional item of a formal argument list.
Return an OPTIONAL-ARG structure."
  (etypecase arg
    (symbol (make-optional-arg arg nil))
    (list   (make-optional-arg (decode-required-arg (car arg)) 
                               (cadr arg)))))

(defun encode-optional-arg (optional-arg)
  (if (or (optional-arg.default-arg optional-arg)
          (arglist-p (optional-arg.arg-name optional-arg)))
      (list (encode-required-arg
             (optional-arg.arg-name optional-arg))
            (optional-arg.default-arg optional-arg))
      (optional-arg.arg-name optional-arg)))

(progn
  (assert (equalp (decode-optional-arg 'x)
                  (make-optional-arg 'x nil)))
  (assert (equalp (decode-optional-arg '(x t))
                  (make-optional-arg 'x t))))

(define-modify-macro nreversef () nreverse "Reverse the list in PLACE.")

(defun decode-arglist (arglist)
  "Parse the list ARGLIST and return an ARGLIST structure."
  (loop
    with mode = nil
    with result = (make-arglist)
    for arg = (if (consp arglist)
                (pop arglist)
                (progn
                  (setf mode '&rest)
                  arglist))
    do (cond
         ((eql mode '&unknown-junk)      
          ;; don't leave this mode -- we don't know how the arglist
          ;; after unknown lambda-list keywords is interpreted
          (push arg (arglist.unknown-junk result)))
         ((eql arg '&allow-other-keys)
          (setf (arglist.allow-other-keys-p result) t))
         ((eql arg '&key)
          (setf (arglist.key-p result) t
                mode arg))
         ((member arg '(&optional &rest &body &aux))
          (setq mode arg))
         ((member arg '(&whole &environment))
          (setq mode arg)
          (push arg (arglist.known-junk result)))
         ((and (symbolp arg)
               (string= (symbol-name arg) (string '#:&any))) ; may be interned
          (setf (arglist.any-p result) t)                    ;  in any *package*.
          (setq mode '&any))
         ((member arg lambda-list-keywords)
          (setq mode '&unknown-junk)
          (push arg (arglist.unknown-junk result)))
         (t
          (ecase mode
            (&key
               (push (decode-keyword-arg arg) 
                     (arglist.keyword-args result)))
            (&optional
               (push (decode-optional-arg arg) 
                     (arglist.optional-args result)))
            (&body
               (setf (arglist.body-p result) t
                     (arglist.rest result) arg))
            (&rest
               (setf (arglist.rest result) arg))
            (&aux
               (push (decode-optional-arg arg)
                     (arglist.aux-args result)))
            ((nil)
               (push (decode-required-arg arg)
                     (arglist.required-args result)))
            ((&whole &environment)
               (setf mode nil)
               (push arg (arglist.known-junk result)))
            (&any
               (push arg (arglist.any-args result))))))
    until (atom arglist)
    finally (nreversef (arglist.required-args result))
    finally (nreversef (arglist.optional-args result))
    finally (nreversef (arglist.keyword-args result))
    finally (nreversef (arglist.aux-args result))
    finally (nreversef (arglist.any-args result))
    finally (nreversef (arglist.known-junk result))
    finally (nreversef (arglist.unknown-junk result))
    finally (assert (or (and (not (arglist.key-p result)) (not (arglist.any-p result)))
                        (exactly-one-p (arglist.key-p result) (arglist.any-p result))))
    finally (return result)))

(defun encode-arglist (decoded-arglist)
  (append (mapcar #'encode-required-arg (arglist.required-args decoded-arglist))
          (when (arglist.optional-args decoded-arglist)
            '(&optional))
          (mapcar #'encode-optional-arg (arglist.optional-args decoded-arglist))
          (when (arglist.key-p decoded-arglist)
            '(&key))
          (mapcar #'encode-keyword-arg (arglist.keyword-args decoded-arglist))
          (when (arglist.allow-other-keys-p decoded-arglist)
            '(&allow-other-keys))
          (when (arglist.any-args decoded-arglist)
            `(&any ,@(arglist.any-args decoded-arglist)))
          (cond ((not (arglist.rest decoded-arglist)) 
                 '())
                ((arglist.body-p decoded-arglist)
                 `(&body ,(arglist.rest decoded-arglist)))
                (t
                 `(&rest ,(arglist.rest decoded-arglist))))
          (when (arglist.aux-args decoded-arglist)
            `(&aux ,(arglist.aux-args decoded-arglist)))
          (arglist.known-junk decoded-arglist)
          (arglist.unknown-junk decoded-arglist)))

(defun arglist-keywords (arglist)
  "Return the list of keywords in ARGLIST.
As a secondary value, return whether &allow-other-keys appears."
  (let ((decoded-arglist (decode-arglist arglist)))
    (values (arglist.keyword-args decoded-arglist)
            (arglist.allow-other-keys-p decoded-arglist))))
                                      
(defun methods-keywords (methods)
  "Collect all keywords in the arglists of METHODS.
As a secondary value, return whether &allow-other-keys appears somewhere."
  (let ((keywords '())
	(allow-other-keys nil))
    (dolist (method methods)
      (multiple-value-bind (kw aok)
	  (arglist-keywords
	   (swank-mop:method-lambda-list method))
	(setq keywords (remove-duplicates (append keywords kw)
                                          :key #'keyword-arg.keyword)
	      allow-other-keys (or allow-other-keys aok))))
    (values keywords allow-other-keys)))

(defun generic-function-keywords (generic-function)
  "Collect all keywords in the methods of GENERIC-FUNCTION.
As a secondary value, return whether &allow-other-keys appears somewhere."
  (methods-keywords 
   (swank-mop:generic-function-methods generic-function)))

(defun applicable-methods-keywords (generic-function arguments)
  "Collect all keywords in the methods of GENERIC-FUNCTION that are
applicable for argument of CLASSES.  As a secondary value, return
whether &allow-other-keys appears somewhere."
  (methods-keywords
   (multiple-value-bind (amuc okp)
       (swank-mop:compute-applicable-methods-using-classes
        generic-function (mapcar #'class-of arguments))
     (if okp
         amuc
         (compute-applicable-methods generic-function arguments)))))

(defun decoded-arglist-to-template-string (decoded-arglist package &key (prefix "(") (suffix ")"))
  (with-output-to-string (*standard-output*)
    (with-standard-io-syntax
      (let ((*package* package) (*print-case* :downcase)
            (*print-pretty* t) (*print-circle* nil) (*print-readably* nil)
            (*print-level* 10) (*print-length* 20))
        (print-decoded-arglist-as-template decoded-arglist 
                                           :prefix prefix 
                                           :suffix suffix)))))

(defun print-decoded-arglist-as-template (decoded-arglist &key
                                          (prefix "(") (suffix ")"))
  (pprint-logical-block (nil nil :prefix prefix :suffix suffix)  
    (let ((first-p t))
      (flet ((space ()
               (unless first-p
                 (write-char #\space)
                 (pprint-newline :fill))
               (setq first-p nil))
             (print-arg-or-pattern (arg)
               (etypecase arg
                 (symbol (if (keywordp arg) (prin1 arg) (princ arg)))
                 (string (princ arg))
                 (list   (princ arg))
                 (arglist (print-decoded-arglist-as-template arg)))))
        (dolist (arg (arglist.required-args decoded-arglist))
          (space)
          (print-arg-or-pattern arg))
        (dolist (arg (arglist.optional-args decoded-arglist))
          (space) 
          (princ "[")
          (print-arg-or-pattern (optional-arg.arg-name arg))
          (princ "]"))
        (dolist (keyword-arg (arglist.keyword-args decoded-arglist))
          (space)
          (let ((arg-name (keyword-arg.arg-name keyword-arg))
                (keyword (keyword-arg.keyword keyword-arg)))
            (format t "~W " 
                    (if (keywordp keyword) keyword `',keyword))
            (print-arg-or-pattern arg-name)))
        (dolist (any-arg (arglist.any-args decoded-arglist))
          (space)
          (print-arg-or-pattern any-arg))
        (when (and (arglist.rest decoded-arglist)
                   (or (not (arglist.keyword-args decoded-arglist))
                       (arglist.allow-other-keys-p decoded-arglist)))
          (if (arglist.body-p decoded-arglist)
              (pprint-newline :mandatory)
              (space))
          (format t "~A..." (arglist.rest decoded-arglist)))))
    (pprint-newline :fill)))


(defgeneric extra-keywords (operator &rest args)
   (:documentation "Return a list of extra keywords of OPERATOR (a
symbol) when applied to the (unevaluated) ARGS.  
As a secondary value, return whether other keys are allowed.  
As a tertiary value, return the initial sublist of ARGS that was needed 
to determine the extra keywords."))

(defun keywords-of-operator (operator)
  "Return a list of KEYWORD-ARGs that OPERATOR accepts.
This function is useful for writing EXTRA-KEYWORDS methods for
user-defined functions which are declared &ALLOW-OTHER-KEYS and which
forward keywords to OPERATOR."
  (let ((arglist (arglist-from-form-spec (ensure-list operator) 
                                         :remove-args nil)))
    (unless (eql arglist :not-available)
      (values 
       (arglist.keyword-args arglist)
       (arglist.allow-other-keys-p arglist)))))

(defmethod extra-keywords (operator &rest args)
  ;; default method
  (declare (ignore args))
  (let ((symbol-function (symbol-function operator)))
    (if (typep symbol-function 'generic-function)
        (generic-function-keywords symbol-function)
        nil)))

(defun class-from-class-name-form (class-name-form)
  (when (and (listp class-name-form)
             (= (length class-name-form) 2)
             (eq (car class-name-form) 'quote))
    (let* ((class-name (cadr class-name-form))
           (class (find-class class-name nil)))
      (when (and class
                 (not (swank-mop:class-finalized-p class)))
        ;; Try to finalize the class, which can fail if
        ;; superclasses are not defined yet
        (handler-case (swank-mop:finalize-inheritance class)
          (program-error (c)
            (declare (ignore c)))))
      class)))
    
(defun extra-keywords/slots (class)
  (multiple-value-bind (slots allow-other-keys-p)
      (if (swank-mop:class-finalized-p class)
          (values (swank-mop:class-slots class) nil)
          (values (swank-mop:class-direct-slots class) t))
    (let ((slot-init-keywords
           (loop for slot in slots append 
                 (mapcar (lambda (initarg)
                           (make-keyword-arg 
                            initarg
                            (swank-mop:slot-definition-name slot)
                            (swank-mop:slot-definition-initform slot)))
                         (swank-mop:slot-definition-initargs slot)))))
      (values slot-init-keywords allow-other-keys-p))))

(defun extra-keywords/make-instance (operator &rest args)
  (declare (ignore operator))
  (unless (null args)
    (let* ((class-name-form (car args))
           (class (class-from-class-name-form class-name-form)))
      (when class
        (multiple-value-bind (slot-init-keywords class-aokp)
            (extra-keywords/slots class)
          (multiple-value-bind (allocate-instance-keywords ai-aokp)
              (applicable-methods-keywords 
               #'allocate-instance (list class))
            (multiple-value-bind (initialize-instance-keywords ii-aokp)
                (applicable-methods-keywords 
                 #'initialize-instance (list (swank-mop:class-prototype class)))
              (multiple-value-bind (shared-initialize-keywords si-aokp)
                  (applicable-methods-keywords 
                   #'shared-initialize (list (swank-mop:class-prototype class) t))
                (values (append slot-init-keywords 
                                allocate-instance-keywords
                                initialize-instance-keywords
                                shared-initialize-keywords)
                        (or class-aokp ai-aokp ii-aokp si-aokp)
                        (list class-name-form))))))))))

(defun extra-keywords/change-class (operator &rest args)
  (declare (ignore operator))
  (unless (null args)
    (let* ((class-name-form (car args))
           (class (class-from-class-name-form class-name-form)))
      (when class
        (multiple-value-bind (slot-init-keywords class-aokp)
            (extra-keywords/slots class)
          (declare (ignore class-aokp))
          (multiple-value-bind (shared-initialize-keywords si-aokp)
              (applicable-methods-keywords
               #'shared-initialize (list (swank-mop:class-prototype class) t))
            ;; FIXME: much as it would be nice to include the
            ;; applicable keywords from
            ;; UPDATE-INSTANCE-FOR-DIFFERENT-CLASS, I don't really see
            ;; how to do it: so we punt, always declaring
            ;; &ALLOW-OTHER-KEYS.
            (declare (ignore si-aokp))
            (values (append slot-init-keywords shared-initialize-keywords)
                    t
                    (list class-name-form))))))))

(defmacro multiple-value-or (&rest forms)
  (if (null forms)
      nil
      (let ((first (first forms))
            (rest (rest forms)))
        `(let* ((values (multiple-value-list ,first))
                (primary-value (first values)))
          (if primary-value
              (values-list values)
              (multiple-value-or ,@rest))))))

(defmethod extra-keywords ((operator (eql 'make-instance))
                           &rest args)
  (multiple-value-or (apply #'extra-keywords/make-instance operator args)
                     (call-next-method)))

(defmethod extra-keywords ((operator (eql 'make-condition))
                           &rest args)
  (multiple-value-or (apply #'extra-keywords/make-instance operator args)
                     (call-next-method)))

(defmethod extra-keywords ((operator (eql 'error))
                           &rest args)
  (multiple-value-or (apply #'extra-keywords/make-instance operator args)
                     (call-next-method)))

(defmethod extra-keywords ((operator (eql 'signal))
                           &rest args)
  (multiple-value-or (apply #'extra-keywords/make-instance operator args)
                     (call-next-method)))

(defmethod extra-keywords ((operator (eql 'warn))
                           &rest args)
  (multiple-value-or (apply #'extra-keywords/make-instance operator args)
                     (call-next-method)))

(defmethod extra-keywords ((operator (eql 'cerror))
                           &rest args)
  (multiple-value-bind (keywords aok determiners)
      (apply #'extra-keywords/make-instance operator
             (cdr args))
    (if keywords
        (values keywords aok
                (cons (car args) determiners))
        (call-next-method))))

(defmethod extra-keywords ((operator (eql 'change-class)) 
                           &rest args)
  (multiple-value-bind (keywords aok determiners)
      (apply #'extra-keywords/change-class operator (cdr args))
    (if keywords
        (values keywords aok
                (cons (car args) determiners))
        (call-next-method))))

(defun enrich-decoded-arglist-with-keywords (decoded-arglist keywords allow-other-keys-p)
  "Modify DECODED-ARGLIST using KEYWORDS and ALLOW-OTHER-KEYS-P."
  (when keywords
    (setf (arglist.key-p decoded-arglist) t)
    (setf (arglist.keyword-args decoded-arglist)
          (remove-duplicates
           (append (arglist.keyword-args decoded-arglist)
                   keywords)
           :key #'keyword-arg.keyword)))
  (setf (arglist.allow-other-keys-p decoded-arglist)
        (or (arglist.allow-other-keys-p decoded-arglist) 
            allow-other-keys-p)))

(defun enrich-decoded-arglist-with-extra-keywords (decoded-arglist form)
  "Determine extra keywords from the function call FORM, and modify
DECODED-ARGLIST to include them.  As a secondary return value, return
the initial sublist of ARGS that was needed to determine the extra
keywords.  As a tertiary return value, return whether any enrichment
was done."
  (multiple-value-bind (extra-keywords extra-aok determining-args)
      (apply #'extra-keywords form)
    ;; enrich the list of keywords with the extra keywords
    (enrich-decoded-arglist-with-keywords decoded-arglist 
                                          extra-keywords extra-aok)
    (values decoded-arglist
            determining-args
            (or extra-keywords extra-aok))))

(defgeneric compute-enriched-decoded-arglist (operator-form argument-forms)
  (:documentation 
   "Return three values: DECODED-ARGLIST, DETERMINING-ARGS, and
ANY-ENRICHMENT, just like enrich-decoded-arglist-with-extra-keywords.
If the arglist is not available, return :NOT-AVAILABLE."))

(defmethod compute-enriched-decoded-arglist (operator-form argument-forms)
  (let ((arglist (arglist operator-form)))
    (etypecase arglist
      ((member :not-available)
       :not-available)
      (list
       (let ((decoded-arglist (decode-arglist arglist)))
         (enrich-decoded-arglist-with-extra-keywords decoded-arglist 
                                                     (cons operator-form 
                                                           argument-forms)))))))

(defmethod compute-enriched-decoded-arglist ((operator-form (eql 'with-open-file))
                                             argument-forms)
  (declare (ignore argument-forms))
  (multiple-value-bind (decoded-arglist determining-args)
      (call-next-method)
    (let ((first-arg (first (arglist.required-args decoded-arglist)))
          (open-arglist (compute-enriched-decoded-arglist 'open nil)))
      (when (and (arglist-p first-arg) (arglist-p open-arglist))
        (enrich-decoded-arglist-with-keywords 
         first-arg 
         (arglist.keyword-args open-arglist)
         nil)))
    (values decoded-arglist determining-args t)))

(defmethod compute-enriched-decoded-arglist ((operator-form (eql 'apply))
                                             argument-forms)
  (let ((function-name-form (car argument-forms)))
    (when (and (listp function-name-form)
               (length= function-name-form 2)
               (member (car function-name-form) '(quote function)))
      (let ((function-name (cadr function-name-form)))
        (when (valid-operator-symbol-p function-name)
          (let ((function-arglist 
                 (compute-enriched-decoded-arglist function-name 
                                                   (cdr argument-forms))))
            (return-from compute-enriched-decoded-arglist
              (values (make-arglist :required-args
                                    (list 'function)
                                    :optional-args 
                                    (append 
                                     (mapcar #'(lambda (arg)
                                                 (make-optional-arg arg nil))
                                             (arglist.required-args function-arglist))
                                     (arglist.optional-args function-arglist))
                                    :key-p 
                                    (arglist.key-p function-arglist)
                                    :keyword-args 
                                    (arglist.keyword-args function-arglist)
                                    :rest 
                                    'args
                                    :allow-other-keys-p 
                                    (arglist.allow-other-keys-p function-arglist))
                      (list function-name-form)
                      t)))))))
  (call-next-method))

(defvar *remove-keywords-alist*
  '((:test :test-not)
    (:test-not :test)))

(defun remove-actual-args (decoded-arglist actual-arglist)
  "Remove from DECODED-ARGLIST the arguments that have already been
provided in ACTUAL-ARGLIST."
  (assert (or (and (not (arglist.key-p decoded-arglist))
                   (not (arglist.any-p decoded-arglist)))
              (exactly-one-p (arglist.key-p decoded-arglist)
                             (arglist.any-p decoded-arglist))))
  (loop while (and actual-arglist
		   (arglist.required-args decoded-arglist))
     do (progn (pop actual-arglist)
	       (pop (arglist.required-args decoded-arglist))))
  (loop while (and actual-arglist
		   (arglist.optional-args decoded-arglist))
     do (progn (pop actual-arglist)
	       (pop (arglist.optional-args decoded-arglist))))
  (if (arglist.any-p decoded-arglist)
      (remove-&any-args decoded-arglist actual-arglist)
      (remove-&key-args decoded-arglist actual-arglist))
  decoded-arglist)

(defun remove-&key-args (decoded-arglist key-args)
  (loop for keyword in key-args by #'cddr
        for keywords-to-remove = (cdr (assoc keyword *remove-keywords-alist*))
        do (setf (arglist.keyword-args decoded-arglist)
                 (remove-if (lambda (kw)
                              (or (eql kw keyword)
                                  (member kw keywords-to-remove)))
                            (arglist.keyword-args decoded-arglist)
                            :key #'keyword-arg.keyword)))  )

(defun remove-&any-args (decoded-arglist any-args)
  (setf (arglist.any-args decoded-arglist)
        (remove-if #'(lambda (x) (member x any-args))
                   (arglist.any-args decoded-arglist)
                   :key #'(lambda (x) (first (ensure-list x))))))


(defun arglist-from-form-spec (form-spec &key (remove-args t))
  "Returns the decoded arglist that corresponds to FORM-SPEC. If
REMOVE-ARGS is T, the arguments that are contained in FORM-SPEC
are removed from the result arglist.

Examples:

  (arglist-from-form-spec '(defun)) 

      ~=> (name args &body body)

  (arglist-from-form-spec '(defun foo)) 

      ~=> (args &body body)

  (arglist-from-form-spec '(defun foo) :remove-args nil)) 

      ~=>  (name args &body body))

  (arglist-from-form-spec '((:type-specifier float) 42) :remove-args nil)

      ~=> (&optional lower-limit upper-limit)
"
  (if (null form-spec)
      :not-available
      (multiple-value-bind (type operator arguments)
          (split-form-spec form-spec)
        (arglist-dispatch type operator arguments :remove-args remove-args))))

(defmacro with-availability ((var) form &body body)
  `(let ((,var ,form))
     (if (eql ,var :not-available)
         :not-available
         (progn ,@body))))

(defgeneric arglist-dispatch (operator-type operator arguments &key remove-args))
  
(defmethod arglist-dispatch ((operator-type t) operator arguments &key (remove-args t))
  (when (and (symbolp operator)
             (valid-operator-symbol-p operator))
    (multiple-value-bind (decoded-arglist determining-args any-enrichment)
        (compute-enriched-decoded-arglist operator arguments)
      (etypecase decoded-arglist
	((member :not-available)
	 :not-available)
	(arglist
	 (cond 
	   (remove-args
	    ;; get rid of formal args already provided
	    (remove-actual-args decoded-arglist arguments))
	   (t
	    ;; replace some formal args by determining actual args
	    (remove-actual-args decoded-arglist determining-args)
	    (setf (arglist.provided-args decoded-arglist)
		  determining-args)))
         (return-from arglist-dispatch
           (values decoded-arglist any-enrichment))))))
  :not-available)

(defmethod arglist-dispatch ((operator-type (eql :function)) (operator (eql 'defmethod))
                             arguments &key (remove-args t))
  (when (and (listp arguments)
	     (not (null arguments)) ;have generic function name
	     (notany #'listp (rest arguments))) ;don't have arglist yet 
    (let* ((gf-name (first arguments))
	   (gf (and (valid-function-name-p gf-name)
		    (fboundp gf-name)
		    (fdefinition gf-name))))
      (when (typep gf 'generic-function)
        (with-availability (arglist) (arglist gf)
          (return-from arglist-dispatch
            (values (make-arglist :provided-args (if remove-args
                                                     nil
                                                     (list gf-name))
                                  :required-args (list arglist)
                                  :rest "body" :body-p t)
                    t))))))
  (call-next-method))

(defmethod arglist-dispatch ((operator-type (eql :function)) (operator (eql 'eval-when))
                             arguments &key (remove-args t))
  (let ((eval-when-args '(:compile-toplevel :load-toplevel :execute)))
    (make-arglist :required-args (list (maybecall remove-args #'remove-actual-args
						  (make-arglist :any-args eval-when-args)
						  arguments))
		  :rest '#:body :body-p t)))

(defmethod arglist-dispatch ((operator-type (eql :function)) (operator (eql 'declare))
                             arguments &key (remove-args t))
  ;; Catching 'DECLARE before SWANK-BACKEND:ARGLIST can barf.
  (declare (ignore remove-args arguments))
  (make-arglist :rest '#:decl-specifiers))

(defmethod arglist-dispatch ((operator-type (eql :declaration))
                             decl-identifier decl-args &key (remove-args t))
  (with-availability (arglist)
      (declaration-arglist decl-identifier)
    (maybecall remove-args #'remove-actual-args
               (decode-arglist arglist) decl-args))
  ;; We don't fall back to CALL-NEXT-METHOD because we're within a
  ;; different namespace! 
  )

(defmethod arglist-dispatch ((operator-type (eql :type-specifier))
                             type-specifier specifier-args &key (remove-args t))
  (with-availability (arglist)
      (type-specifier-arglist type-specifier)
    (maybecall remove-args #'remove-actual-args
               (decode-arglist arglist) specifier-args))
  ;; No CALL-NEXT-METHOD, see above.
  )


(defun read-incomplete-form-from-string (form-string)
  (with-buffer-syntax ()
    (call-with-ignored-reader-errors
      #'(lambda ()
          (read-from-string form-string)))))

(defun call-with-ignored-reader-errors (thunk)
  (declare (type (function () (values &rest t)) thunk))
  (declare (optimize (speed 3) (safety 1)))
  (handler-case (funcall thunk)
    (reader-error (c)
      (declare (ignore c))
      nil)
    (stream-error (c)
      (declare (ignore c))
      nil)))

(defslimefun complete-form (form-string)
  "Read FORM-STRING in the current buffer package, then complete it
by adding a template for the missing arguments."
  (multiple-value-bind (form newly-interned-symbols)
      (parse-form-spec form-string)
    (unwind-protect
         (when (consp form)
           (let ((form-completion (arglist-from-form-spec form)))
             (unless (eql form-completion :not-available)
               (return-from complete-form
                 (decoded-arglist-to-template-string form-completion
                                                     *buffer-package*
                                                     :prefix "")))))
      (mapc #'unintern-in-home-package newly-interned-symbols))
    :not-available))


(defun arglist-ref (decoded-arglist operator &rest indices)
  (cond
    ((null indices) decoded-arglist)
    ((not (arglist-p decoded-arglist)) nil)
    (t
     (let ((index (first indices))
           (args (append (and operator 
                              (list operator))
                         (arglist.required-args decoded-arglist)
                         (arglist.optional-args decoded-arglist))))
       (when (< index (length args))
         (let ((arg (elt args index)))
           (apply #'arglist-ref arg nil (rest indices))))))))

(defslimefun completions-for-keyword (raw-specs keyword-string arg-index-specs)
  (with-buffer-syntax ()
    (multiple-value-bind (form-spec position newly-interned-symbols)
        (parse-first-valid-form-spec raw-specs)
      (unwind-protect
	   (when form-spec
	     (let ((arglist (arglist-from-form-spec form-spec  :remove-args nil)))
	       (unless (eql arglist :not-available)
		 (let* ((operator (nth-value 1 (split-form-spec form-spec)))
			(indices  (reverse (rest (subseq arg-index-specs 0 (1+ position)))))
			(arglist  (apply #'arglist-ref arglist operator indices)))
		   (when (and arglist (arglist-p arglist))
		     ;; It would be possible to complete keywords only if we
		     ;; are in a keyword position, but it is not clear if we
		     ;; want that.
		     (let* ((keywords 
			     (append (mapcar #'keyword-arg.keyword
					     (arglist.keyword-args arglist))
				     (remove-if-not #'keywordp (arglist.any-args arglist))))
			    (keyword-name
			     (tokenize-symbol keyword-string))
			    (matching-keywords
			     (find-matching-symbols-in-list keyword-name keywords
							    #'compound-prefix-match))
			    (converter (completion-output-symbol-converter keyword-string))
			    (strings
			     (mapcar converter
				     (mapcar #'symbol-name matching-keywords)))
			    (completion-set
			     (format-completion-set strings nil "")))
		       (list completion-set
			     (longest-compound-prefix completion-set))))))))
        (mapc #'unintern-in-home-package newly-interned-symbols)))))
           

(defun arglist-to-string (arglist package &key print-right-margin highlight)
  (decoded-arglist-to-string (decode-arglist arglist)
                             :package package
                             :print-right-margin print-right-margin
                             :highlight highlight))

(defun test-print-arglist ()
  (flet ((test (list string)
           (let* ((p (find-package :swank))
                  (actual (arglist-to-string list p)))
             (unless (string= actual string)
               (warn "Test failed: ~S => ~S~%  Expected: ~S" 
                     list actual string)))))
    (test '(function cons) "(function cons)")
    (test '(quote cons) "(quote cons)")
    (test '(&key (function #'+)) "(&key (function #'+))")
    (test '(&whole x y z) "(y z)")
    (test '(x &aux y z) "(x)")
    (test '(x &environment env y) "(x y)")
    (test '(&key ((function f))) "(&key ((function f)))")
    (test '(eval-when (&any :compile-toplevel :load-toplevel :execute) &body body)
	  "(eval-when (&any :compile-toplevel :load-toplevel :execute) &body body)")
    (test '(declare (optimize &any (speed 1) (safety 1)))
	  "(declare (optimize &any (speed 1) (safety 1)))")
    ))

(test-print-arglist)

(provide :swank-arglists)
