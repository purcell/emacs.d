;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; openmcl-swank.lisp --- SLIME backend for OpenMCL.
;;;
;;; Copyright (C) 2003, James Bielman  <jamesjb@jamesjb.com>
;;;
;;; This program is licensed under the terms of the Lisp Lesser GNU
;;; Public License, known as the LLGPL, and distributed with OpenMCL
;;; as the file "LICENSE".  The LLGPL consists of a preamble and the
;;; LGPL, which is distributed with OpenMCL as the file "LGPL".  Where
;;; these conflict, the preamble takes precedence.
;;;
;;; The LLGPL is also available online at
;;; http://opensource.franz.com/preamble.html

;;;
;;; This is the beginning of a Slime backend for OpenMCL.  It has been
;;; tested only with OpenMCL version 0.14-030901 on Darwin --- I would
;;; be interested in hearing the results with other versions.
;;;
;;; Additionally, reporting the positions of warnings accurately requires
;;; a small patch to the OpenMCL file compiler, which may be found at:
;;;
;;;   http://www.jamesjb.com/slime/openmcl-warning-position.diff
;;;
;;; Things that work:
;;;
;;; * Evaluation of forms with C-M-x.
;;; * Compilation of defuns with C-c C-c.
;;; * File compilation with C-c C-k.
;;; * Most of the debugger functionality, except EVAL-IN-FRAME,
;;;   FRAME-SOURCE-LOCATION, and FRAME-CATCH-TAGS.
;;; * Macroexpanding with C-c RET.
;;; * Disassembling the symbol at point with C-c M-d.
;;; * Describing symbol at point with C-c C-d.
;;; * Compiler warnings are trapped and sent to Emacs using the buffer
;;;   position of the offending top level form.
;;; * Symbol completion and apropos.
;;;
;;; Things that sort of work:
;;;
;;; * WHO-CALLS is implemented but is only able to return the file a
;;;   caller is defined in---source location information is not
;;;   available.
;;;
;;; Things that aren't done yet:
;;;
;;; * Cross-referencing.
;;; * Due to unimplementation functionality the test suite does not
;;;   run correctly (it hangs upon entering the debugger).
;;;

(in-package :swank-backend)

(import-from :ccl *gray-stream-symbols* :swank-backend)

(require 'xref)

;;; swank-mop

(import-to-swank-mop
 '( ;; classes
   cl:standard-generic-function
   ccl::standard-slot-definition
   cl:method
   cl:standard-class
   ccl::eql-specializer
   openmcl-mop:finalize-inheritance
   ;; standard-class readers
   openmcl-mop:class-default-initargs
   openmcl-mop:class-direct-default-initargs
   openmcl-mop:class-direct-slots
   openmcl-mop:class-direct-subclasses
   openmcl-mop:class-direct-superclasses
   openmcl-mop:class-finalized-p
   cl:class-name
   openmcl-mop:class-precedence-list
   openmcl-mop:class-prototype
   openmcl-mop:class-slots
   openmcl-mop:specializer-direct-methods
   ;; eql-specializer accessors
   openmcl-mop:eql-specializer-object
   ;; generic function readers
   openmcl-mop:generic-function-argument-precedence-order
   openmcl-mop:generic-function-declarations
   openmcl-mop:generic-function-lambda-list
   openmcl-mop:generic-function-methods
   openmcl-mop:generic-function-method-class
   openmcl-mop:generic-function-method-combination
   openmcl-mop:generic-function-name
   ;; method readers
   openmcl-mop:method-generic-function
   openmcl-mop:method-function
   openmcl-mop:method-lambda-list
   openmcl-mop:method-specializers
   openmcl-mop:method-qualifiers
   ;; slot readers
   openmcl-mop:slot-definition-allocation
   ccl::slot-definition-documentation
   openmcl-mop:slot-value-using-class
   openmcl-mop:slot-definition-initargs
   openmcl-mop:slot-definition-initform
   openmcl-mop:slot-definition-initfunction
   openmcl-mop:slot-definition-name
   openmcl-mop:slot-definition-type
   openmcl-mop:slot-definition-readers
   openmcl-mop:slot-definition-writers
   openmcl-mop:slot-boundp-using-class))

(defun specializer-name (spec)
  (etypecase spec
    (cons spec)
    (class (class-name spec))
    (ccl::eql-specializer `(eql ,(ccl::eql-specializer-object spec)))))

(defun swank-mop:compute-applicable-methods-using-classes (gf args)
  (let* ((methods (ccl::%gf-methods gf))
         (args-length (length args))
         (bits (ccl::inner-lfun-bits gf))
         arg-count res)
    (when methods
      (setq arg-count (length (ccl::%method-specializers (car methods))))
      (unless (<= arg-count args-length)
        (error "Too few args to ~s" gf))
      (unless (or (logbitp ccl::$lfbits-rest-bit bits)
                  (logbitp ccl::$lfbits-restv-bit bits)
                  (logbitp ccl::$lfbits-keys-bit bits)
                  (<= args-length 
                      (+ (ldb ccl::$lfbits-numreq bits) (ldb ccl::$lfbits-numopt bits))))
        (error "Too many args to ~s" gf))
      (let ((cpls (make-list arg-count)))
        (declare (dynamic-extent cpls))
        (do* ((args-tail args (cdr args-tail))
              (cpls-tail cpls (cdr cpls-tail)))
             ((null cpls-tail))
          (setf (car cpls-tail)
                (ccl::%class-precedence-list (car args-tail))))
        (flet ((%method-applicable-p (method args cpls)
                 (do* ((specs (ccl::%method-specializers method) (ccl::%cdr specs))
                       (args args (ccl::%cdr args))
                       (cpls cpls (ccl::%cdr cpls)))
                      ((null specs) t)
                   (let ((spec (ccl::%car specs)))
                     (if (typep spec 'ccl::eql-specializer)
                         (unless (subtypep (ccl::%car args) (class-of (ccl::eql-specializer-object spec)))
                           (return nil))
                         (unless (ccl:memq spec (ccl::%car cpls))
                           (return nil)))))))
          (dolist (m methods)
            (if (%method-applicable-p m args cpls)
                (push m res))))
        (ccl::sort-methods res cpls (ccl::%gf-precedence-list gf))))))

;;; TCP Server

(defimplementation preferred-communication-style ()
  :spawn)

(defimplementation create-socket (host port)
  (ccl:make-socket :connect :passive :local-port port 
                   :local-host host :reuse-address t))

(defimplementation local-port (socket)
  (ccl:local-port socket))

(defimplementation close-socket (socket)
  (close socket))

(defimplementation accept-connection (socket
                                      &key external-format buffering timeout)
  (declare (ignore buffering timeout external-format))
  (ccl:accept-connection socket :wait t))

(defimplementation emacs-connected ()
  (setq ccl::*interactive-abort-process* ccl::*current-process*))

(defimplementation make-stream-interactive (stream)
  (typecase stream
    (ccl:fundamental-output-stream 
     (push stream ccl::*auto-flush-streams*))))

;;; Unix signals

(defimplementation call-without-interrupts (fn)
  (ccl:without-interrupts (funcall fn)))

(defimplementation getpid ()
  (ccl::getpid))

(defimplementation lisp-implementation-type-name ()
  "openmcl")

(defvar *break-in-sldb* t)

(let ((ccl::*warn-if-redefine-kernel* nil))
  (ccl::advise 
   cl::break 
   (if (and *break-in-sldb* 
            (find ccl::*current-process* (symbol-value (intern "*CONNECTIONS*" 'swank))
                  :key (intern "CONNECTION.REPL-THREAD" 'swank)))
       (apply 'break-in-sldb ccl::arglist)
       (:do-it)) :when :around :name sldb-break))

(defun break-in-sldb (&optional string &rest args)
  (let ((c (make-condition 'simple-condition
                           :format-control (or string "Break")
                           :format-arguments args)))
    (let ((previous-f nil)
          (previous-f2 nil))
      (block find-frame
        (map-backtrace  
         #'(lambda(frame-number p context lfun pc)
             (declare (ignore frame-number context pc))
             (when (eq  previous-f2 'break-in-sldb) 
               (record-stack-top p)
               (return-from find-frame))
             (setq previous-f2 previous-f)
             (setq previous-f (ccl::lfun-name lfun)))))
      (restart-case (invoke-debugger c)
        (continue () :report (lambda (stream) (write-string "Resume interrupted evaluation" stream)) t))
      )))

; In previous version the code that recorded the function that had an
; error or which was interrupted was not thread safe. This code repairs that by
; associating the frame pointer with a process via the *process-to-stack-top* hash.

(defvar *process-to-stack-top* (make-hash-table :test 'eql))

(defun record-stack-top (frame)
  (setf (gethash (ccl::process-serial-number ccl::*current-process*) *process-to-stack-top* )
        frame))
          
(defun grab-stack-top ()
  (let ((psn (ccl::process-serial-number ccl::*current-process*)))
    (ccl::without-interrupts
      (prog1
          (gethash  psn *process-to-stack-top*)
        (setf (gethash psn *process-to-stack-top*) nil)))))

(defmethod ccl::application-error :before (application condition error-pointer)
  (declare (ignore application condition))
  (record-stack-top error-pointer)
  nil)

;;; Evaluation

(defimplementation arglist (fname)
  (arglist% fname))

(defmethod arglist% ((f symbol))
  (ccl:arglist f))

(defmethod arglist% ((f function))
  (ccl:arglist (ccl:function-name f)))

(defimplementation function-name (function)
  (ccl:function-name function))

;;; Compilation

(defvar *buffer-offset* nil)
(defvar *buffer-name* nil)

(defun condition-source-position (condition)
  "Return the position in the source file of a compiler condition."
  (+ 1
     (or *buffer-offset* 0)
     ;; alanr sometimes returned stream position nil.
     (or (ccl::compiler-warning-stream-position condition) 0))) 


(defun handle-compiler-warning (condition)
  "Construct a compiler note for Emacs from a compiler warning
condition."
  (signal (make-condition
           'compiler-condition
           :original-condition condition
           :message (format nil "~A" condition)
           :severity :warning
           :location
           (let ((position (condition-source-position condition)))
             (if *buffer-name*
                 (make-location
                  (list :buffer *buffer-name*)
                  (list :position position t))
                 (if (ccl::compiler-warning-file-name condition)
                     (make-location
                      (list :file (namestring (truename (ccl::compiler-warning-file-name condition))))
                      (list :position position t))))))))

(defun temp-file-name ()
  "Return a temporary file name to compile strings into."
  (ccl:%get-cstring (#_tmpnam (ccl:%null-ptr))))

(defimplementation call-with-compilation-hooks (function)
  (handler-bind ((ccl::compiler-warning 'handle-compiler-warning))
    (funcall function)))

(defimplementation swank-compile-file (filename load-p external-format)
  (declare (ignore external-format))
  (with-compilation-hooks ()
    (let ((*buffer-name* nil)
          (*buffer-offset* nil))
      (compile-file filename :load load-p))))

(defimplementation frame-var-value (frame var)
  (block frame-var-value
    (map-backtrace  
     #'(lambda(frame-number p context lfun pc)
         (when (= frame frame-number)
           (return-from frame-var-value 
             (multiple-value-bind (total vsp parent-vsp)
                 (ccl::count-values-in-frame p context)
               (loop for count below total
                     with varcount = -1
                     for (value nil name) = (multiple-value-list (ccl::nth-value-in-frame p count context lfun pc vsp parent-vsp))
                     when name do (incf varcount)
                     until (= varcount var)
                     finally (return value))
               )))))))

(defun xref-locations (relation name &optional (inverse nil))
  (loop for xref in (if inverse 
                        (ccl::get-relation  relation name :wild :exhaustive t)
                        (ccl::get-relation  relation :wild name :exhaustive t))
        for function = (ccl::xref-entry-name xref)
        collect `((function ,function) ,(function-source-location (ccl::xref-entry-name xref)))))

(defimplementation who-binds (name)
  (xref-locations :binds name))

(defimplementation who-macroexpands (name)
  (xref-locations :macro-calls name t))
  
(defimplementation who-references (name)
  (remove-duplicates
   (append (xref-locations :references name)
           (xref-locations :sets name)
           (xref-locations :binds name))
   :test 'equal))

(defimplementation who-sets (name)
  (xref-locations :sets name))

(defimplementation who-calls (name)
  (remove-duplicates
   (append
    (xref-locations :direct-calls name)
    (xref-locations :indirect-calls name)
    (xref-locations :macro-calls name t))
   :test 'equal))

(defimplementation list-callees (name)
  (remove-duplicates
   (append
   (xref-locations :direct-calls name t)
   (xref-locations :macro-calls name nil))
   :test 'equal))

(defimplementation who-specializes (class)
  (if (symbolp class) (setq class (find-class class)))
  (remove-duplicates
   (append (mapcar (lambda(m)
                     (let ((location (function-source-location (ccl::method-function m))))
                       (if (eq (car location) :error)
                           (setq location nil ))
                       `((method ,(ccl::method-name m)
                                 ,(mapcar #'specializer-name (ccl::method-specializers m))
                                 ,@(ccl::method-qualifiers m))
                         ,location)))
                   (ccl::%class.direct-methods class))
           (mapcan 'who-specializes (ccl::%class-direct-subclasses class)))
   :test 'equal))

(defimplementation swank-compile-string (string &key buffer position directory)
  (declare (ignore directory))
  (with-compilation-hooks ()
    (let ((*buffer-name* buffer)
          (*buffer-offset* position)
          (filename (temp-file-name)))
      (unwind-protect
           (with-open-file (s filename :direction :output :if-exists :error)
             (write-string string s))
        (let ((binary-filename (compile-file filename :load t)))
          (delete-file binary-filename)))
      (delete-file filename))))

;;; Profiling (alanr: lifted from swank-clisp)

(defimplementation profile (fname)
  (eval `(mon:monitor ,fname)))		;monitor is a macro

(defimplementation profiled-functions ()
  mon:*monitored-functions*)

(defimplementation unprofile (fname)
  (eval `(mon:unmonitor ,fname)))	;unmonitor is a macro

(defimplementation unprofile-all ()
  (mon:unmonitor))

(defimplementation profile-report ()
  (mon:report-monitoring))

(defimplementation profile-reset ()
  (mon:reset-all-monitoring))

(defimplementation profile-package (package callers-p methods)
  (declare (ignore callers-p methods))
  (mon:monitor-all package))

;;; Debugging

(defun openmcl-set-debug-switches ()
  (setq ccl::*fasl-save-definitions* nil)
  (setq ccl::*fasl-save-doc-strings* t)
  (setq ccl::*fasl-save-local-symbols* t)
  (setq ccl::*ppc2-compiler-register-save-label* t) 
  (setq ccl::*save-arglist-info* t)
  (setq ccl::*save-definitions* nil)
  (setq ccl::*save-doc-strings* t)
  (setq ccl::*save-local-symbols* t)
  (ccl::start-xref))

(defvar *sldb-stack-top* nil)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let* ((*debugger-hook* nil)
         (*sldb-stack-top* (grab-stack-top))
         (ccl::*signal-printing-errors* nil)) ; don't let error while printing error take us down
    (funcall debugger-loop-fn)))

(defun backtrace-context ()
  (if (and (= ccl::*openmcl-major-version* 0)
           (<= ccl::*openmcl-minor-version* 14)
           (< ccl::*openmcl-revision* 2))
      (ccl::%current-tcr)
      nil))

(defun map-backtrace (function &optional
                      (start-frame-number 0)
                      (end-frame-number most-positive-fixnum))
  "Call FUNCTION passing information about each stack frame
 from frames START-FRAME-NUMBER to END-FRAME-NUMBER."
  (let ((context (backtrace-context))
        (frame-number 0)
        (top-stack-frame (or *sldb-stack-top*
                             (ccl::%get-frame-ptr))))
    (do* ((p top-stack-frame (ccl::parent-frame p context))
          (q (ccl::last-frame-ptr context)))
         ((or (null p) (eq p q) (ccl::%stack< q p context))
          (values))
      (multiple-value-bind (lfun pc) (ccl::cfp-lfun p)
        (when lfun
          (if (and (>= frame-number start-frame-number)
                   (< frame-number end-frame-number))
              (funcall function frame-number p context lfun pc))
          (incf frame-number))))))

;; May 13, 2004 alanr: use prin1 instead of princ so I see " around strings. Write ' in front of symbol names and lists.
;; Sept  6, 2004 alanr: use builtin ccl::frame-supplied-args

(defun frame-arguments (p context lfun pc)
  "Returns a string representing the arguments of a frame."
  (multiple-value-bind (args types names count nclosed)
      (ccl::frame-supplied-args p lfun pc nil context)
    (declare (ignore count nclosed))
    (let ((result nil))
      (loop named loop
         for var = (cond
                     ((null args)
                      (return-from loop))
                     ((atom args)
                      (prog1
                          args
                        (setf args nil)))
                     (t (pop args)))
          for type in types
          for name in names
          do
          (when (or (symbolp var) (listp var)) (setq var (list 'quote var)))
          (cond ((equal type "keyword")
                 (push (format nil "~S ~A" 
                               (intern (symbol-name name) "KEYWORD")
                               (prin1-to-string var))
                       result))
                (t   (push (prin1-to-string var) result))))
      (format nil "~{ ~A~}" (nreverse result)))))


;; XXX should return something less stringy
;; alanr May 13, 2004: put #<> around anonymous functions in the backtrace.

(defimplementation compute-backtrace (start-frame-number end-frame-number)
  (let (result)
    (map-backtrace (lambda (frame-number p  context lfun pc)
		     (declare (ignore  frame-number))
                     (push (with-output-to-string (s)
                             (format s "(~A~A)"
                                     (if (ccl::function-name lfun)
					 (ccl::%lfun-name-string lfun)
					 lfun)
                                     (frame-arguments p context lfun pc)))
                           result))
                   start-frame-number end-frame-number)
    (nreverse result)))

(defimplementation print-frame (frame stream)
  (princ frame stream))

(defimplementation frame-locals (index)
  (block frame-locals
    (map-backtrace 
     (lambda (frame-number p context lfun pc)
       (when (= frame-number index)
         (multiple-value-bind (count vsp parent-vsp)
             (ccl::count-values-in-frame p context)
           (let (result)
             (dotimes (i count)
               (multiple-value-bind (var type name)
                   (ccl::nth-value-in-frame p i context lfun pc vsp parent-vsp)
                 (declare (ignore type))
                 (when name
                   (push (list 
                          :name name
                          :id 0
                          :value var)
                         result))))
             (return-from frame-locals (nreverse result)))))))))

(defimplementation frame-catch-tags (index &aux my-frame)
  (block frame-catch-tags
    (map-backtrace 
     (lambda (frame-number p context lfun pc)
       (declare (ignore pc lfun))
       (if (= frame-number index) 
           (setq my-frame p)
           (when my-frame
             (return-from frame-catch-tags
               (loop for catch = (ccl::%catch-top (ccl::%current-tcr)) then (ccl::next-catch catch)
                     while catch
                     for csp = (ccl::uvref catch 3) ; ppc32::catch-frame.csp-cell) defined in arch.lisp
                     for tag = (ccl::uvref catch 0) ; ppc32::catch-frame.catch-tag-cell)
                     until (ccl::%stack< p csp context)
                     when (ccl::%stack< my-frame csp context)
                     collect (cond 
                               ((symbolp tag)
                                tag)
                               ((and (listp tag)
                                     (typep (car tag) 'restart))
                                `(:restart ,(restart-name (car tag)))))))))))))

(defimplementation disassemble-frame (the-frame-number)
  (let ((function-to-disassemble nil))
    (block find-frame
      (map-backtrace
       (lambda(frame-number p context lfun pc)
         (declare (ignore p context pc))
         (when (= frame-number the-frame-number)
           (setq function-to-disassemble lfun)
           (return-from find-frame)))))
    (ccl::print-ppc-instructions 
     *standard-output* 
     (ccl::function-to-dll-header function-to-disassemble) nil)))

;;;

(defun canonicalize-location (file symbol)
  (etypecase file
    ((or string pathname)
     (multiple-value-bind (truename c) (ignore-errors (namestring (truename file)))
       (cond (c (list :error (princ-to-string c)))
             (t (make-location (list :file (remove-filename-quoting truename))
                               (list :function-name (princ-to-string symbol)))))))))

(defun remove-filename-quoting (string)
  (if (search "\\" string)
      (read-from-string (format nil "\"~a\"" string))
      string))

(defun maybe-method-location (type)
  (when (typep type 'ccl::method)
    `((method ,(ccl::method-name type)
              ,(mapcar #'specializer-name (ccl::method-specializers type))
              ,@(ccl::method-qualifiers type))
      ,(function-source-location (ccl::method-function type)))))

(defimplementation find-definitions (symbol)
  (let* ((info (ccl::get-source-files-with-types&classes symbol)))
    (loop for (type . file) in info
          when (not (equal "l1-boot-3" (pathname-name file))) ; alanr: This is a bug - there's nothing in there
          collect (or (maybe-method-location type)
                      (list (list type symbol) 
                            (canonicalize-location file symbol))))))


(defun function-source-location (function)
  (multiple-value-bind (info name) (ccl::edit-definition-p function)
    (cond ((not info) (list :error (format nil "No source info available for ~A" function)))
          ((typep (caar info) 'ccl::method)
           `(:location 
             (:file ,(remove-filename-quoting (namestring (translate-logical-pathname (cdr (car info))) )))
             (:method  ,(princ-to-string (ccl::method-name (caar info)))
               ,(mapcar 'princ-to-string
                        (mapcar #'specializer-name
                                (ccl::method-specializers (caar info))))
               ,@(mapcar 'princ-to-string (ccl::method-qualifiers (caar info))))
             nil))
          (t (canonicalize-location (cdr (first info)) name)))))

(defimplementation frame-source-location-for-emacs (index)
  "Return to Emacs the location of the source code for the
function in a debugger frame.  In OpenMCL, we are not able to
find the precise position of the frame, but we do attempt to give
at least the filename containing it."
  (block frame-source-location-for-emacs
    (map-backtrace
     (lambda (frame-number p context lfun pc)
       (declare (ignore p context pc))
       (when (and (= frame-number index) lfun)
         (return-from frame-source-location-for-emacs
           (function-source-location lfun)))))))

(defimplementation eval-in-frame (form index)
  (block eval-in-frame
    (map-backtrace
     (lambda (frame-number p context lfun pc)
       (when (= frame-number index)
         (multiple-value-bind (count vsp parent-vsp)
             (ccl::count-values-in-frame p context)
           (let ((bindings nil))
             (dotimes (i count)
               (multiple-value-bind (var type name)
                   (ccl::nth-value-in-frame p i context lfun pc vsp parent-vsp)
                 (declare (ignore type))
                 (when name
                   (push (list name `',var) bindings))
                 ))
             (return-from eval-in-frame
               (eval `(let ,bindings
                        (declare (ignorable ,@(mapcar 'car bindings)))
                        ,form)))
             )))))))

(defimplementation return-from-frame (index form)
  (let ((values (multiple-value-list (eval-in-frame form index))))
    (map-backtrace
     (lambda (frame-number p context lfun pc)
       (declare (ignore context lfun pc))
       (when (= frame-number index)
         (ccl::apply-in-frame p #'values values))))))
 
(defimplementation restart-frame (index)
  (map-backtrace
   (lambda (frame-number p context lfun pc)
     (when (= frame-number index)
       (ccl::apply-in-frame p lfun 
                            (ccl::frame-supplied-args p lfun pc nil context))))))

;;; Utilities

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (flet ((doc (kind &optional (sym symbol))
             (or (documentation sym kind) :not-documented))
           (maybe-push (property value)
             (when value
               (setf result (list* property value result)))))
      (maybe-push
       :variable (when (boundp symbol)
                   (doc 'variable)))
      (maybe-push
       :function (if (fboundp symbol)
                     (doc 'function)))
      (maybe-push
       :setf (let ((setf-function-name (ccl::setf-function-spec-name 
                                        `(setf ,symbol))))
               (when (fboundp setf-function-name)
                 (doc 'function setf-function-name))))
      result)))

(defimplementation describe-definition (symbol namespace)
  (ecase namespace
    (:variable 
     (describe symbol))
    ((:function :generic-function)
     (describe (symbol-function symbol)))
    (:setf
     (describe (ccl::setf-function-spec-name `(setf ,symbol))))
    (:class
     (describe (find-class symbol)))))

(defimplementation toggle-trace (spec)
  "We currently ignore just about everything."
  (ecase (car spec)
    (setf
     (ccl::%trace spec))
    (:defmethod
     (ccl::%trace (second spec)))
    (:defgeneric
     (ccl::%trace (second spec)))
    (:call
     (toggle-trace (third spec)))
    ;; mb: FIXME: shouldn't we warn that we're not doing anything for
    ;; these two?
    (:labels nil)
    (:flet nil))
  t)

;;; XREF

(defimplementation list-callers (symbol)
  (loop for caller in (ccl::callers symbol)
        append (multiple-value-bind (info name type specializers modifiers)
                   (ccl::edit-definition-p caller)
                 (loop for (nil . file) in info
                       collect (list (if (eq t type)
                                         name
                                         `(,type ,name ,specializers
                                           ,@modifiers))
                                     (canonicalize-location file name))))))
;;; Macroexpansion

(defvar *value2tag* (make-hash-table))

(do-symbols (s (find-package 'arch))
  (if (and (> (length (symbol-name s)) 7)
	   (string= (symbol-name s) "SUBTAG-" :end1 7)
	   (boundp s)
	   (numberp (symbol-value s))
	   (< (symbol-value s) 255))
      (setf (gethash (symbol-value s) *value2tag*) s)))

;;;; Inspection

(defclass openmcl-inspector (inspector)
  ())

(defimplementation make-default-inspector ()
  (make-instance 'openmcl-inspector))

(defimplementation describe-primitive-type (thing)
  (let ((typecode (ccl::typecode thing)))
    (if (gethash typecode *value2tag*)
	(string (gethash typecode *value2tag*))
	(string (nth typecode '(tag-fixnum tag-list tag-misc tag-imm))))))

(defmethod inspect-for-emacs ((o t) (inspector openmcl-inspector))
  (declare (ignore inspector))
  (let* ((i (inspector::make-inspector o))
	 (count (inspector::compute-line-count i))
	 (lines 
          (loop
             for l below count
             for (value label) = (multiple-value-list 
                                  (inspector::line-n i l))
             collect `(:value ,label ,(string-capitalize (format nil "~a" label)))
             collect " = "
             collect `(:value ,value)
             collect '(:newline))))
    (values (with-output-to-string (s)
              (let ((*print-lines* 1)
                    (*print-right-margin* 80))
                (pprint o s)))
            lines)))

(defmethod inspect-for-emacs :around ((o t) (inspector openmcl-inspector))
  (if (or (uvector-inspector-p o)
          (not (ccl:uvectorp o)))
      (call-next-method)
      (multiple-value-bind (title content)
          (call-next-method)
        (values
         title
         (append content
                 `((:newline)
                   (:value ,(make-instance 'uvector-inspector :object o)
                           "Underlying UVECTOR")))))))

(defclass uvector-inspector ()
  ((object :initarg :object)))

(defgeneric uvector-inspector-p (object)
  (:method ((object t)) nil)
  (:method ((object uvector-inspector)) t))

(defmethod inspect-for-emacs ((uv uvector-inspector) (inspector openmcl-inspector))
  (with-slots (object)
      uv
    (values (format nil "The UVECTOR for ~S." object)
            (loop
               for index below (ccl::uvsize object)
               collect (format nil "~D: " index)
               collect `(:value ,(ccl::uvref object index))
               collect `(:newline)))))

(defun closure-closed-over-values (closure)
  (let ((howmany (nth-value 8 (ccl::function-args (ccl::closure-function closure)))))
    (loop for n below howmany
	 collect
	 (let* ((value (ccl::%svref closure (+ 1 (- howmany n))))
		(map (car (ccl::function-symbol-map (ccl::closure-function closure))))
		(label (or (and map (svref map n)) n))
		(cellp (ccl::closed-over-value-p value)))
	   (list label (if cellp (ccl::closed-over-value value) value))))))

(defmethod inspect-for-emacs ((c ccl::compiled-lexical-closure) (inspector t))
  (declare (ignore inspector))
  (values
   (format nil "A closure: ~a" c)
   `(,@(if (arglist c)
	   (list "Its argument list is: " 
		 (funcall (intern "INSPECTOR-PRINC" 'swank) (arglist c))) 
           ;; FIXME inspector-princ should load earlier
	   (list "A function of no arguments"))
     (:newline)
     ,@(when (documentation c t)
	 `("Documentation:" (:newline) ,(documentation c t) (:newline)))
     ,(format nil "Closed over ~a values"  (length (closure-closed-over-values c)))
     (:newline)
     ,@(loop for (name value) in (closure-closed-over-values c)
	    for count from 1
	  append
	  (label-value-line* ((format nil "~2,' d) ~a" count (string name)) value))))))




;;; Multiprocessing

(defvar *known-processes* '()         ; FIXME: leakage. -luke
  "Alist (ID . PROCESS MAILBOX) list of processes that we have handed
out IDs for.")

(defvar *known-processes-lock* (ccl:make-lock "*known-processes-lock*"))

(defstruct (mailbox (:conc-name mailbox.)) 
  (mutex (ccl:make-lock "thread mailbox"))
  (semaphore (ccl:make-semaphore))
  (queue '() :type list))

(defimplementation spawn (fn &key name)
  (ccl:process-run-function (or name "Anonymous (Swank)") fn))

(defimplementation thread-id (thread)
  (ccl::process-serial-number thread))

(defimplementation find-thread (id)
  (find id (ccl:all-processes) :key #'ccl::process-serial-number))

(defimplementation thread-name (thread)
  (ccl::process-name thread))

(defimplementation thread-status (thread)
  (format nil "~A" (ccl:process-whostate thread)))

(defimplementation make-lock (&key name)
  (ccl:make-lock name))

(defimplementation call-with-lock-held (lock function)
  (ccl:with-lock-grabbed (lock)
    (funcall function)))

(defimplementation current-thread ()
  ccl:*current-process*)

(defimplementation all-threads ()
  (ccl:all-processes))

(defimplementation kill-thread (thread)
  (ccl:process-kill thread))

;; September  5, 2004 alanr. record the frame interrupted
(defimplementation interrupt-thread (thread fn)
  (ccl:process-interrupt 
   thread 
   (lambda(&rest args)
     (let ((previous-f nil))
       (block find-frame
         (map-backtrace  
          #'(lambda(frame-number p context lfun pc)
              (declare (ignore frame-number context pc))
              (when (eq  previous-f 'ccl::%pascal-functions%) 
                (record-stack-top p)
                (return-from find-frame))
              (setq previous-f (ccl::lfun-name lfun)))))
       (apply fn args)))))


(defun mailbox (thread)
  (ccl:with-lock-grabbed (*known-processes-lock*)
    (let ((probe (rassoc thread *known-processes* :key #'car)))
      (cond (probe (second (cdr probe)))
            (t (let ((mailbox (make-mailbox)))
                 (setq *known-processes*
                       (acons (ccl::process-serial-number thread) 
                              (list thread mailbox)
                              (remove-if 
                               (lambda(entry) 
                                 (string= (ccl::process-whostate (second entry)) "Exhausted")) 
                               *known-processes*)
                              ))
                 mailbox))))))
          
(defimplementation send (thread message)
  (assert message)
  (let* ((mbox (mailbox thread))
         (mutex (mailbox.mutex mbox)))
    (ccl:with-lock-grabbed (mutex)
      (setf (mailbox.queue mbox)
            (nconc (mailbox.queue mbox) (list message)))
      (ccl:signal-semaphore (mailbox.semaphore mbox)))))

(defimplementation receive ()
  (let* ((mbox (mailbox ccl:*current-process*))
         (mutex (mailbox.mutex mbox)))
    (ccl:wait-on-semaphore (mailbox.semaphore mbox))
    (ccl:with-lock-grabbed (mutex)
      (assert (mailbox.queue mbox))
      (pop (mailbox.queue mbox)))))

(defimplementation quit-lisp ()
  (ccl::quit))

;;; Weak datastructures

(defimplementation make-weak-key-hash-table (&rest args)
  (apply #'make-hash-table :weak :key args))

(defimplementation make-weak-value-hash-table (&rest args)
  (apply #'make-hash-table :weak :value args))

(defimplementation hash-table-weakness (hashtable)
  (ccl::hash-table-weak-p hashtable))
