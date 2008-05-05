;;; swank-fancy-inspector.lisp --- Fancy inspector for CLOS objects
;;
;; Author: Marco Baringer <mb@bese.it> and others
;; License: Public Domain
;;

(in-package :swank)

(defmethod emacs-inspect ((symbol symbol))
  (let ((package (symbol-package symbol)))
    (multiple-value-bind (_symbol status) 
	(and package (find-symbol (string symbol) package))
      (declare (ignore _symbol))
      (append
	(label-value-line "Its name is" (symbol-name symbol))
	;;
	;; Value 
	(cond ((boundp symbol)
               (label-value-line (if (constantp symbol)
                                     "It is a constant of value"
                                     "It is a global variable bound to")
                                 (symbol-value symbol)))
	      (t '("It is unbound." (:newline))))
	(docstring-ispec "Documentation" symbol 'variable)
	(multiple-value-bind (expansion definedp) (macroexpand symbol)
	  (if definedp 
	      (label-value-line "It is a symbol macro with expansion" 
				expansion)))
	;;
	;; Function
	(if (fboundp symbol)
	    (append (if (macro-function symbol)
			`("It a macro with macro-function: "
			  (:value ,(macro-function symbol)))
			`("It is a function: " 
			  (:value ,(symbol-function symbol))))
		    `(" " (:action "[make funbound]"
				   ,(lambda () (fmakunbound symbol))))
		    `((:newline)))
	    `("It has no function value." (:newline)))
	(docstring-ispec "Function Documentation" symbol 'function)
	(if (compiler-macro-function symbol)
	    (label-value-line "It also names the compiler macro"
			      (compiler-macro-function symbol)))
	(docstring-ispec "Compiler Macro Documentation" 
			 symbol 'compiler-macro)
	;;
	;; Package
        (if package
            `("It is " ,(string-downcase (string status)) 
                       " to the package: "
                       (:value ,package ,(package-name package))
                       ,@(if (eq :internal status) 
                             `(" "
                               (:action "[export it]"
                                        ,(lambda () (export symbol package)))))
                       " "
                       (:action "[unintern it]"
                                ,(lambda () (unintern symbol package)))
                       (:newline))
            '("It is a non-interned symbol." (:newline)))
	;;
	;; Plist
	(label-value-line "Property list" (symbol-plist symbol))
	;; 
	;; Class
	(if (find-class symbol nil)
	    `("It names the class " 
	      (:value ,(find-class symbol) ,(string symbol))
              " "
	      (:action "[remove]"
		       ,(lambda () (setf (find-class symbol) nil)))
	      (:newline)))
	;;
	;; More package
	(if (find-package symbol)
	    (label-value-line "It names the package" (find-package symbol)))
	))))

(defun docstring-ispec (label object kind)
  "Return a inspector spec if OBJECT has a docstring of of kind KIND."
  (let ((docstring (documentation object kind)))
    (cond ((not docstring) nil)
	  ((< (+ (length label) (length docstring))
	      75)
	   (list label ": " docstring '(:newline)))
	  (t 
	   (list label ": " '(:newline) "  " docstring '(:newline))))))

(unless (find-method #'emacs-inspect '() (list (find-class 'function)) nil)
  (defmethod emacs-inspect ((f function))
    (inspect-function f)))

(defun inspect-function (f)
  (append
   (label-value-line "Name" (function-name f))
   `("Its argument list is: " 
     ,(inspector-princ (arglist f)) (:newline))
   (docstring-ispec "Documentation" f t)
   (if (function-lambda-expression f)
       (label-value-line "Lambda Expression"
			 (function-lambda-expression f)))))

(defun method-specializers-for-inspect (method)
  "Return a \"pretty\" list of the method's specializers. Normal
  specializers are replaced by the name of the class, eql
  specializers are replaced by `(eql ,object)."
  (mapcar (lambda (spec)
            (typecase spec
              (swank-mop:eql-specializer
               `(eql ,(swank-mop:eql-specializer-object spec)))
              (t (swank-mop:class-name spec))))
          (swank-mop:method-specializers method)))

(defun method-for-inspect-value (method)
  "Returns a \"pretty\" list describing METHOD. The first element
  of the list is the name of generic-function method is
  specialiazed on, the second element is the method qualifiers,
  the rest of the list is the method's specialiazers (as per
  method-specializers-for-inspect)."
  (append (list (swank-mop:generic-function-name
		 (swank-mop:method-generic-function method)))
	  (swank-mop:method-qualifiers method)
	  (method-specializers-for-inspect method)))

(defmethod emacs-inspect ((object standard-object))
  (let ((class (class-of object)))
            `("Class: " (:value ,class) (:newline)
              ,@(all-slots-for-inspector object))))

(defvar *gf-method-getter* 'methods-by-applicability
  "This function is called to get the methods of a generic function.
The default returns the method sorted by applicability.
See `methods-by-applicability'.")

(defun specializer< (specializer1 specializer2)
  "Return true if SPECIALIZER1 is more specific than SPECIALIZER2."
  (let ((s1 specializer1) (s2 specializer2) )
    (cond ((typep s1 'swank-mop:eql-specializer)
	   (not (typep s2 'swank-mop:eql-specializer)))
	  (t
	   (flet ((cpl (class)
		    (and (swank-mop:class-finalized-p class)
			 (swank-mop:class-precedence-list class))))
	     (member s2 (cpl s1)))))))

(defun methods-by-applicability (gf)
  "Return methods ordered by most specific argument types.

`method-specializer<' is used for sorting."
  ;; FIXME: argument-precedence-order and qualifiers are ignored.  
  (labels ((method< (meth1 meth2)
             (loop for s1 in (swank-mop:method-specializers meth1)
                   for s2 in (swank-mop:method-specializers meth2)
                   do (cond ((specializer< s2 s1) (return nil))
                            ((specializer< s1 s2) (return t))))))
    (stable-sort (copy-seq (swank-mop:generic-function-methods gf)) #'method<)))

(defun abbrev-doc (doc &optional (maxlen 80))
  "Return the first sentence of DOC, but not more than MAXLAN characters."
  (subseq doc 0 (min (1+ (or (position #\. doc) (1- maxlen)))
		     maxlen
		     (length doc))))

(defgeneric inspect-slot-for-emacs (class object slot)
  (:method (class object slot)
           (let ((slot-name (swank-mop:slot-definition-name slot))
                 (boundp (swank-mop:slot-boundp-using-class class object slot)))
             `(,@(if boundp
                     `((:value ,(swank-mop:slot-value-using-class class object slot)))
                     `("#<unbound>"))
               " "
               (:action "[set value]"
                ,(lambda () (with-simple-restart
                                (abort "Abort setting slot ~S" slot-name)
                              (let ((value-string (eval-in-emacs
                                                   `(condition-case c
                                                     (slime-read-from-minibuffer
                                                      ,(format nil "Set slot ~S to (evaluated) : " slot-name))
                                                     (quit nil)))))
                                (when (and value-string
                                           (not (string= value-string "")))
                                  (setf (swank-mop:slot-value-using-class class object slot)
                                        (eval (read-from-string value-string))))))))
               ,@(when boundp
                   `(" " (:action "[make unbound]"
                          ,(lambda () (swank-mop:slot-makunbound-using-class class object slot)))))))))

(defgeneric all-slots-for-inspector (object)
  (:method ((object standard-object))
    (append '("--------------------" (:newline)
              "All Slots:" (:newline))
            (let* ((class (class-of object))
                   (direct-slots (swank-mop:class-direct-slots class))
                   (effective-slots (sort (copy-seq (swank-mop:class-slots class))
                                          #'string< :key #'swank-mop:slot-definition-name))
                   (slot-presentations (loop for effective-slot :in effective-slots
                                             collect (inspect-slot-for-emacs
                                                      class object effective-slot)))
                   (longest-slot-name-length
                    (loop for slot :in effective-slots
                          maximize (length (symbol-name
                                            (swank-mop:slot-definition-name slot))))))
              (loop
                  for effective-slot :in effective-slots
                  for slot-presentation :in slot-presentations
                  for direct-slot = (find (swank-mop:slot-definition-name effective-slot)
                                          direct-slots :key #'swank-mop:slot-definition-name)
                  for slot-name = (inspector-princ
                                   (swank-mop:slot-definition-name effective-slot))
                  for padding-length = (- longest-slot-name-length
                                          (length (symbol-name
                                                   (swank-mop:slot-definition-name
                                                    effective-slot))))
                  collect `(:value ,(if direct-slot
                                        (list direct-slot effective-slot)
                                        effective-slot)
                            ,slot-name)
                  collect (make-array padding-length
                                      :element-type 'character
                                      :initial-element #\Space)
                  collect " = "
                  append slot-presentation
                  collect '(:newline))))))

(defmethod emacs-inspect ((gf standard-generic-function)) 
  (flet ((lv (label value) (label-value-line label value)))
    (append 
      (lv "Name" (swank-mop:generic-function-name gf))
      (lv "Arguments" (swank-mop:generic-function-lambda-list gf))
      (docstring-ispec "Documentation" gf t)
      (lv "Method class" (swank-mop:generic-function-method-class gf))
      (lv "Method combination" 
	  (swank-mop:generic-function-method-combination gf))
      `("Methods: " (:newline))
      (loop for method in (funcall *gf-method-getter* gf) append
	    `((:value ,method ,(inspector-princ
			       ;; drop the name of the GF
			       (cdr (method-for-inspect-value method))))
              " "
	      (:action "[remove method]" 
                       ,(let ((m method)) ; LOOP reassigns method
                          (lambda () 
                            (remove-method gf m))))
	      (:newline)))
      `((:newline))
      (all-slots-for-inspector gf))))

(defmethod emacs-inspect ((method standard-method))
          `("Method defined on the generic function " 
	    (:value ,(swank-mop:method-generic-function method)
		    ,(inspector-princ
		      (swank-mop:generic-function-name
		       (swank-mop:method-generic-function method))))
            (:newline)
	    ,@(docstring-ispec "Documentation" method t)
            "Lambda List: " (:value ,(swank-mop:method-lambda-list method))
            (:newline)
            "Specializers: " (:value ,(swank-mop:method-specializers method)
                                     ,(inspector-princ (method-specializers-for-inspect method)))
            (:newline)
            "Qualifiers: " (:value ,(swank-mop:method-qualifiers method))
            (:newline)
            "Method function: " (:value ,(swank-mop:method-function method))
            (:newline)
            ,@(all-slots-for-inspector method)))

(defmethod emacs-inspect ((class standard-class))
          `("Name: " (:value ,(class-name class))
            (:newline)
            "Super classes: "
            ,@(common-seperated-spec (swank-mop:class-direct-superclasses class))
            (:newline)
            "Direct Slots: "
            ,@(common-seperated-spec
               (swank-mop:class-direct-slots class)
               (lambda (slot)
                 `(:value ,slot ,(inspector-princ (swank-mop:slot-definition-name slot)))))
            (:newline)
            "Effective Slots: "
            ,@(if (swank-mop:class-finalized-p class)
                  (common-seperated-spec
                   (swank-mop:class-slots class)
                   (lambda (slot)
                     `(:value ,slot ,(inspector-princ
                                      (swank-mop:slot-definition-name slot)))))
                  '("#<N/A (class not finalized)>"))
            (:newline)
            ,@(let ((doc (documentation class t)))
                (when doc
                  `("Documentation:" (:newline) ,(inspector-princ doc) (:newline))))
            "Sub classes: "
            ,@(common-seperated-spec (swank-mop:class-direct-subclasses class)
                                     (lambda (sub)
                                       `(:value ,sub ,(inspector-princ (class-name sub)))))
            (:newline)
            "Precedence List: "
            ,@(if (swank-mop:class-finalized-p class)
                  (common-seperated-spec (swank-mop:class-precedence-list class)
                                         (lambda (class)
                                           `(:value ,class ,(inspector-princ (class-name class)))))
                  '("#<N/A (class not finalized)>"))
            (:newline)
            ,@(when (swank-mop:specializer-direct-methods class)
               `("It is used as a direct specializer in the following methods:" (:newline)
                 ,@(loop
                      for method in (sort (copy-seq (swank-mop:specializer-direct-methods class))
                                          #'string< :key (lambda (x)
                                                           (symbol-name
                                                            (let ((name (swank-mop::generic-function-name
                                                                         (swank-mop::method-generic-function x))))
                                                              (if (symbolp name) name (second name))))))
                      collect "  "
                      collect `(:value ,method ,(inspector-princ (method-for-inspect-value method)))
                      collect '(:newline)
                      if (documentation method t)
                      collect "    Documentation: " and
                      collect (abbrev-doc (documentation method t)) and
                      collect '(:newline))))
            "Prototype: " ,(if (swank-mop:class-finalized-p class)
                               `(:value ,(swank-mop:class-prototype class))
                               '"#<N/A (class not finalized)>")
            (:newline)
            ,@(all-slots-for-inspector class)))

(defmethod emacs-inspect ((slot swank-mop:standard-slot-definition))
          `("Name: " (:value ,(swank-mop:slot-definition-name slot))
            (:newline)
            ,@(when (swank-mop:slot-definition-documentation slot)
                `("Documentation:"  (:newline)
                  (:value ,(swank-mop:slot-definition-documentation slot))
                  (:newline)))
            "Init args: " (:value ,(swank-mop:slot-definition-initargs slot)) (:newline)
            "Init form: "  ,(if (swank-mop:slot-definition-initfunction slot)
                             `(:value ,(swank-mop:slot-definition-initform slot))
                             "#<unspecified>") (:newline)
            "Init function: " (:value ,(swank-mop:slot-definition-initfunction slot))            
            (:newline)
            ,@(all-slots-for-inspector slot)))


;; Wrapper structure over the list of symbols of a package that should
;; be displayed with their respective classification flags. This is
;; because we need a unique type to dispatch on in EMACS-INSPECT.
;; Used by the Inspector for packages.
(defstruct (%package-symbols-container (:conc-name   %container.)
                                       (:constructor %%make-package-symbols-container))
  title          ;; A string; the title of the inspector page in Emacs.   
  description    ;; A list of renderable objects; used as description.
  symbols        ;; A list of symbols. Supposed to be sorted alphabetically.
  grouping-kind  ;; Either :SYMBOL or :CLASSIFICATION. Cf. MAKE-SYMBOLS-LISTING.
  )

(defun %make-package-symbols-container (&key title description symbols)
  (%%make-package-symbols-container :title title :description description
                                    :symbols symbols :grouping-kind :symbol))

(defgeneric make-symbols-listing (grouping-kind symbols))

(defmethod make-symbols-listing ((grouping-kind (eql :symbol)) symbols)
  "Returns an object renderable by Emacs' inspector side that
alphabetically lists all the symbols in SYMBOLS together with a
concise string representation of what each symbol
represents (cf. CLASSIFY-SYMBOL & Fuzzy Completion.)"
  (let ((max-length (loop for s in symbols maximizing (length (symbol-name s))))
        (distance 10)) ; empty distance between name and classification
    (flet ((string-representations (symbol)
             (let* ((name (symbol-name symbol))
                    (length (length name))
                    (padding (- max-length length))                    
                    (classification (classify-symbol symbol)))
               (values
                (concatenate 'string
                             name
                             (make-string (+ padding distance) :initial-element #\Space))
                (symbol-classification->string classification)))))
      `(""                           ; 8 is (length "Symbols:")
        "Symbols:" ,(make-string (+ -8 max-length distance) :initial-element #\Space) "Flags:"
        (:newline)
        ,(concatenate 'string        ; underlining dashes
                      (make-string (+ max-length distance -1) :initial-element #\-)
                      " "
                      (let* ((dummy (classify-symbol :foo))
                             (dummy (symbol-classification->string dummy))
                             (classification-length (length dummy)))
                        (make-string classification-length :initial-element #\-)))
        (:newline)          
        ,@(loop for symbol in symbols appending
               (multiple-value-bind (symbol-string classification-string)
                   (string-representations symbol)
                 `((:value ,symbol ,symbol-string) ,classification-string
                   (:newline)
                   )))))))

(defmethod make-symbols-listing ((grouping-kind (eql :classification)) symbols)
  "For each possible classification (cf. CLASSIFY-SYMBOL), group
all the symbols in SYMBOLS to all of their respective
classifications. (If a symbol is, for instance, boundp and a
generic-function, it'll appear both below the BOUNDP group and
the GENERIC-FUNCTION group.) As macros and special-operators are
specified to be FBOUNDP, there is no general FBOUNDP group,
instead there are the three explicit FUNCTION, MACRO and
SPECIAL-OPERATOR groups."
  (let ((table (make-hash-table :test #'eq))
	(+default-classification+ :misc))
    (flet ((normalize-classifications (classifications)
             (cond ((null classifications) `(,+default-classification+))
		   ;; Convert an :FBOUNDP in CLASSIFICATIONS to :FUNCTION if possible.
		   ((and (member :fboundp classifications)
			 (not (member :macro classifications))
			 (not (member :special-operator classifications)))
		      (substitute :function :fboundp classifications))
		   (t (remove :fboundp classifications)))))
      (loop for symbol in symbols do
            (loop for classification in (normalize-classifications (classify-symbol symbol))
                  ;; SYMBOLS are supposed to be sorted alphabetically;
                  ;; this property is preserved here except for reversing.
                  do (push symbol (gethash classification table)))))
    (let* ((classifications (loop for k being each hash-key in table collect k))
           (classifications (sort classifications
				  ;; Sort alphabetically, except +DEFAULT-CLASSIFICATION+
				  ;; which sort to the end.
				  #'(lambda (a b)
				      (cond ((eql a +default-classification+) nil)
					    ((eql b +default-classification+) t)
					    (t (string< a b)))))))
      (loop for classification in classifications
            for symbols = (gethash classification table)
            appending`(,(symbol-name classification)
                        (:newline)
                        ,(make-string 64 :initial-element #\-)
                        (:newline)
                        ,@(mapcan #'(lambda (symbol)
                                      (list `(:value ,symbol ,(symbol-name symbol)) '(:newline)))
                                  (nreverse symbols)) ; restore alphabetic orderness.
                        (:newline)
                        )))))

(defmethod emacs-inspect ((%container %package-symbols-container))
  (with-struct (%container. title description symbols grouping-kind) %container
            `(,title (:newline) 
	      ,@description
              (:newline)
              "  " ,(ecase grouping-kind
                           (:symbol
                            `(:action "[Group by classification]"
                                      ,(lambda () (setf grouping-kind :classification))
                                      :refreshp t))
                           (:classification
                            `(:action "[Group by symbol]"
                                      ,(lambda () (setf grouping-kind :symbol))
                                      :refreshp t)))
              (:newline) (:newline)
              ,@(make-symbols-listing grouping-kind symbols))))

(defmethod emacs-inspect ((package package))
  (let ((package-name         (package-name package))
        (package-nicknames    (package-nicknames package))
        (package-use-list     (package-use-list package))
        (package-used-by-list (package-used-by-list package))
        (shadowed-symbols     (package-shadowing-symbols package))
        (present-symbols      '()) (present-symbols-length  0)
        (internal-symbols     '()) (internal-symbols-length 0)
        (external-symbols     '()) (external-symbols-length 0))

    (do-symbols* (sym package)
      (let ((status (symbol-status sym package)))
        (when (not (eq status :inherited))
          (push sym present-symbols) (incf present-symbols-length)
          (if (eq status :internal)
              (progn (push sym internal-symbols) (incf internal-symbols-length))                
              (progn (push sym external-symbols) (incf external-symbols-length))))))
    
    (setf package-nicknames    (sort (copy-list package-nicknames)    #'string<)
          package-use-list     (sort (copy-list package-use-list)     #'string< :key #'package-name)
          package-used-by-list (sort (copy-list package-used-by-list) #'string< :key #'package-name)
          shadowed-symbols     (sort (copy-list shadowed-symbols)     #'string<))
    
    (setf present-symbols      (sort present-symbols  #'string<)  ; SORT + STRING-LESSP
          internal-symbols     (sort internal-symbols #'string<)  ; conses on at least
          external-symbols     (sort external-symbols #'string<)) ; SBCL 0.9.18.

    
     `(""                               ; dummy to preserve indentation.
       "Name: " (:value ,package-name) (:newline)
                       
       "Nick names: " ,@(common-seperated-spec package-nicknames) (:newline)
              
       ,@(when (documentation package t)
               `("Documentation:" (:newline) ,(documentation package t) (:newline)))
              
       "Use list: " ,@(common-seperated-spec
                       package-use-list
                       (lambda (package)
                         `(:value ,package ,(package-name package))))
       (:newline)
              
       "Used by list: " ,@(common-seperated-spec
                           package-used-by-list
                           (lambda (package)
                             `(:value ,package ,(package-name package))))
       (:newline)

       ,@     ; ,@(flet ((...)) ...) would break indentation in Emacs.
       (flet ((display-link (type symbols length &key title description)
                (if (null symbols)
                    (format nil "0 ~A symbols." type)
                    `(:value ,(%make-package-symbols-container :title title
                                                               :description description
                                                               :symbols symbols)
                             ,(format nil "~D ~A symbol~P." length type length)))))
         
         `(,(display-link "present" present-symbols  present-symbols-length
                          :title (format nil "All present symbols of package \"~A\"" package-name)
                          :description
                          '("A symbol is considered present in a package if it's" (:newline)
                            "\"accessible in that package directly, rather than"  (:newline)
                            "being inherited from another package.\""             (:newline)
                            "(CLHS glossary entry for `present')"                 (:newline)))
            
            (:newline)
            ,(display-link "external" external-symbols external-symbols-length
                           :title (format nil "All external symbols of package \"~A\"" package-name)
                           :description
                           '("A symbol is considered external of a package if it's"  (:newline)
                             "\"part of the `external interface' to the package and" (:newline)
                             "[is] inherited by any other package that uses the"     (:newline)
                             "package.\" (CLHS glossary entry of `external')"        (:newline)))
            (:newline)
            ,(display-link "internal" internal-symbols internal-symbols-length
                           :title (format nil "All internal symbols of package \"~A\"" package-name)
                           :description
                           '("A symbol is considered internal of a package if it's"   (:newline)
                             "present and not external---that is if the package is"   (:newline)
                             "the home package of the symbol, or if the symbol has"   (:newline)
                             "been explicitly imported into the package."             (:newline)
                             (:newline)
                             "Notice that inherited symbols will thus not be listed," (:newline)
                             "which deliberately deviates from the CLHS glossary"     (:newline)
                             "entry of `internal' because it's assumed to be more"    (:newline)
                             "useful this way."                                       (:newline)))
            (:newline)
            ,(display-link "shadowed" shadowed-symbols (length shadowed-symbols)
                           :title (format nil "All shadowed symbols of package \"~A\"" package-name)
                           :description nil))))))


(defmethod emacs-inspect ((pathname pathname))
  `(,(if (wild-pathname-p pathname)
	 "A wild pathname."
	 "A pathname.")
     (:newline)
     ,@(label-value-line*
	("Namestring" (namestring pathname))
	("Host"       (pathname-host pathname))
	("Device"     (pathname-device pathname))
	("Directory"  (pathname-directory pathname))
	("Name"       (pathname-name pathname))
	("Type"       (pathname-type pathname))
	("Version"    (pathname-version pathname)))
     ,@ (unless (or (wild-pathname-p pathname)
		    (not (probe-file pathname)))
	  (label-value-line "Truename" (truename pathname)))))

(defmethod emacs-inspect ((pathname logical-pathname))
          (append 
           (label-value-line*
            ("Namestring" (namestring pathname))
            ("Physical pathname: " (translate-logical-pathname pathname)))
           `("Host: " 
             ,(pathname-host pathname)
             " (" (:value ,(logical-pathname-translations
                            (pathname-host pathname))) 
             "other translations)"
             (:newline))
           (label-value-line*
            ("Directory" (pathname-directory pathname))
            ("Name" (pathname-name pathname))
            ("Type" (pathname-type pathname))
            ("Version" (pathname-version pathname))
            ("Truename" (if (not (wild-pathname-p pathname))
                            (probe-file pathname))))))

(defmethod emacs-inspect ((n number))
  `("Value: " ,(princ-to-string n)))

(defun format-iso8601-time (time-value &optional include-timezone-p)
    "Formats a universal time TIME-VALUE in ISO 8601 format, with
    the time zone included if INCLUDE-TIMEZONE-P is non-NIL"    
    ;; Taken from http://www.pvv.ntnu.no/~nsaa/ISO8601.html
    ;; Thanks, Nikolai Sandved and Thomas Russ!
    (flet ((format-iso8601-timezone (zone)
             (if (zerop zone)
                 "Z"
                 (multiple-value-bind (h m) (truncate (abs zone) 1.0)
                   ;; Tricky.  Sign of time zone is reversed in ISO 8601
                   ;; relative to Common Lisp convention!
                   (format nil "~:[+~;-~]~2,'0D:~2,'0D"
                           (> zone 0) h (round (* 60 m)))))))
    (multiple-value-bind (second minute hour day month year dow dst zone)
      (decode-universal-time time-value)
      (declare (ignore dow dst))
      (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~:[~*~;~A~]"
              year month day hour minute second
              include-timezone-p (format-iso8601-timezone zone)))))

(defmethod emacs-inspect ((i integer))
          (append
           `(,(format nil "Value: ~D = #x~8,'0X = #o~O = #b~,,' ,8:B~@[ = ~E~]"
                      i i i i (ignore-errors (coerce i 'float)))
              (:newline))
           (when (< -1 i char-code-limit)
             (label-value-line "Code-char" (code-char i)))
           (label-value-line "Integer-length" (integer-length i))           
           (ignore-errors
             (label-value-line "Universal-time" (format-iso8601-time i t)))))

(defmethod emacs-inspect ((c complex))
          (label-value-line* 
           ("Real part" (realpart c))
           ("Imaginary part" (imagpart c))))

(defmethod emacs-inspect ((r ratio))
          (label-value-line*
           ("Numerator" (numerator r))
           ("Denominator" (denominator r))
           ("As float" (float r))))

(defmethod emacs-inspect ((f float))
          (cond
            ((> f most-positive-long-float)
             (list "Positive infinity."))
            ((< f most-negative-long-float)
             (list "Negative infinity."))
            ((not (= f f))
             (list "Not a Number."))
            (t
             (multiple-value-bind (significand exponent sign) (decode-float f)
               (append 
                `("Scientific: " ,(format nil "~E" f) (:newline)
                                 "Decoded: " 
                                 (:value ,sign) " * " 
                                 (:value ,significand) " * " 
                                 (:value ,(float-radix f)) "^" (:value ,exponent) (:newline))
                (label-value-line "Digits" (float-digits f))
                (label-value-line "Precision" (float-precision f)))))))

(defmethod emacs-inspect ((stream file-stream))
  (multiple-value-bind (content)
      (call-next-method)
            (append
             `("Pathname: "
               (:value ,(pathname stream))
               (:newline) "  "
               (:action "[visit file and show current position]"
                        ,(let ((pathname (pathname stream))
                               (position (file-position stream)))
                           (lambda ()
                             (ed-in-emacs `(,pathname :charpos ,position))))
                        :refreshp nil)
               (:newline))
             content)))

(defmethod emacs-inspect ((condition stream-error))
  (multiple-value-bind (content)
      (call-next-method)
    (let ((stream (stream-error-stream condition)))
      (if (typep stream 'file-stream)
                  (append
                   `("Pathname: "
                     (:value ,(pathname stream))
                     (:newline) "  "
                     (:action "[visit file and show current position]"
                              ,(let ((pathname (pathname stream))
                                     (position (file-position stream)))
                                    (lambda ()
                                      (ed-in-emacs `(,pathname :charpos ,position))))
                              :refreshp nil)
                     (:newline))
                   content)
          content))))

(defun common-seperated-spec (list &optional (callback (lambda (v) 
							 `(:value ,v))))
  (butlast
   (loop
      for i in list
      collect (funcall callback i)
      collect ", ")))

(defun inspector-princ (list)
  "Like princ-to-string, but don't rewrite (function foo) as #'foo. 
Do NOT pass circular lists to this function."
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch '(cons (member function)) nil)
    (princ-to-string list)))

(provide :swank-fancy-inspector)
