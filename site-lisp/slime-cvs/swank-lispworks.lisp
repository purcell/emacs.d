;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-lispworks.lisp --- LispWorks specific code for SLIME. 
;;;
;;; Created 2003, Helmut Eller
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(in-package :swank-backend)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm")
  (import-from :stream *gray-stream-symbols* :swank-backend))

(import-swank-mop-symbols :clos '(:slot-definition-documentation
                                  :eql-specializer
                                  :eql-specializer-object
                                  :compute-applicable-methods-using-classes))

(defun swank-mop:slot-definition-documentation (slot)
  (documentation slot t))

(defun swank-mop:compute-applicable-methods-using-classes (gf classes)
  (clos::compute-applicable-methods-from-classes gf classes))

;; lispworks doesn't have the eql-specializer class, it represents
;; them as a list of `(EQL ,OBJECT)
(deftype swank-mop:eql-specializer () 'cons)

(defun swank-mop:eql-specializer-object (eql-spec)
  (second eql-spec))

(when (fboundp 'dspec::define-dspec-alias)
  (dspec::define-dspec-alias defimplementation (name args &rest body)
    `(defmethod ,name ,args ,@body)))

;;; TCP server

(defimplementation preferred-communication-style ()
  :spawn)

(defun socket-fd (socket)
  (etypecase socket
    (fixnum socket)
    (comm:socket-stream (comm:socket-stream-socket socket))))

(defimplementation create-socket (host port)
  (multiple-value-bind (socket where errno)
      #-(or lispworks4.1 (and macosx lispworks4.3))
      (comm::create-tcp-socket-for-service port :address host)
      #+(or lispworks4.1 (and macosx lispworks4.3))
      (comm::create-tcp-socket-for-service port)
    (cond (socket socket)
          (t (error 'network-error 
              :format-control "~A failed: ~A (~D)"
              :format-arguments (list where 
                                      (list #+unix (lw:get-unix-error errno))
                                      errno))))))

(defimplementation local-port (socket)
  (nth-value 1 (comm:get-socket-address (socket-fd socket))))

(defimplementation close-socket (socket)
  (comm::close-socket (socket-fd socket)))

(defimplementation accept-connection (socket 
                                      &key external-format buffering timeout)
  (declare (ignore buffering timeout external-format))
  (let* ((fd (comm::get-fd-from-socket socket)))
    (assert (/= fd -1))
    (make-instance 'comm:socket-stream :socket fd :direction :io 
                   :element-type 'base-char)))

(defun set-sigint-handler ()
  ;; Set SIGINT handler on Swank request handler thread.
  #-win32
  (sys::set-signal-handler +sigint+ 
                           (make-sigint-handler mp:*current-process*)))

;;; Coding Systems

(defvar *external-format-to-coding-system*
  '(((:latin-1 :eol-style :lf) 
     "latin-1-unix" "iso-latin-1-unix" "iso-8859-1-unix")
    ((:latin-1) 
     "latin-1" "iso-latin-1" "iso-8859-1")
    ((:utf-8) "utf-8")
    ((:utf-8 :eol-style :lf) "utf-8-unix")
    ((:euc-jp) "euc-jp")
    ((:euc-jp :eol-style :lf) "euc-jp-unix")
    ((:ascii) "us-ascii")
    ((:ascii :eol-style :lf) "us-ascii-unix")))

(defimplementation find-external-format (coding-system)
  (car (rassoc-if (lambda (x) (member coding-system x :test #'equal))
                  *external-format-to-coding-system*)))

;;; Unix signals

(defun sigint-handler ()
  (with-simple-restart  (continue "Continue from SIGINT handler.")
    (invoke-debugger "SIGINT")))

(defun make-sigint-handler (process)
  (lambda (&rest args)
    (declare (ignore args))
    (mp:process-interrupt process #'sigint-handler)))

(defimplementation call-without-interrupts (fn)
  (lw:without-interrupts (funcall fn)))
  
(defimplementation getpid ()
  #+win32 (win32:get-current-process-id)
  #-win32 (system::getpid))

(defimplementation lisp-implementation-type-name ()
  "lispworks")

(defimplementation set-default-directory (directory)
  (namestring (hcl:change-directory directory)))

;;;; Documentation

(defimplementation arglist (symbol-or-function)
  (let ((arglist (lw:function-lambda-list symbol-or-function)))
    (etypecase arglist
      ((member :dont-know) 
       :not-available)
      (list
       arglist))))

(defimplementation function-name (function)
  (nth-value 2 (function-lambda-expression function)))

(defimplementation macroexpand-all (form)
  (walker:walk-form form))

(defun generic-function-p (object)
  (typep object 'generic-function))

(defimplementation describe-symbol-for-emacs (symbol)
  "Return a plist describing SYMBOL.
Return NIL if the symbol is unbound."
  (let ((result '()))
    (labels ((first-line (string) 
               (let ((pos (position #\newline string)))
                 (if (null pos) string (subseq string 0 pos))))
             (doc (kind &optional (sym symbol))
               (let ((string (documentation sym kind)))
                 (if string 
                     (first-line string)
                     :not-documented)))
             (maybe-push (property value)
               (when value
                 (setf result (list* property value result)))))
      (maybe-push
       :variable (when (boundp symbol)
                   (doc 'variable)))
      (maybe-push
       :generic-function (if (and (fboundp symbol)
                                  (generic-function-p (fdefinition symbol)))
                             (doc 'function)))
      (maybe-push
       :function (if (and (fboundp symbol)
                          (not (generic-function-p (fdefinition symbol))))
                     (doc 'function)))
      (maybe-push
       :setf (let ((setf-name (sys:underlying-setf-name `(setf ,symbol))))
               (if (fboundp setf-name)
                   (doc 'setf))))
      (maybe-push
       :class (if (find-class symbol nil) 
                  (doc 'class)))
      result)))

(defimplementation describe-definition (symbol type)
  (ecase type
    (:variable (describe-symbol symbol))
    (:class (describe (find-class symbol)))
    ((:function :generic-function) (describe-function symbol))
    (:setf (describe-function (sys:underlying-setf-name `(setf ,symbol))))))

(defun describe-function (symbol)
  (cond ((fboundp symbol)
         (format t "~%(~A~{ ~A~})~%~%~:[(not documented)~;~:*~A~]~%"
                 (string-downcase symbol)
                 (mapcar #'string-upcase 
                         (lispworks:function-lambda-list symbol))
                 (documentation symbol 'function))
         (describe (fdefinition symbol)))
        (t (format t "~S is not fbound" symbol))))

(defun describe-symbol (sym)
  (format t "~A is a symbol in package ~A." sym (symbol-package sym))
  (when (boundp sym)
    (format t "~%~%Value: ~A" (symbol-value sym)))
  (let ((doc (documentation sym 'variable)))
    (when doc 
      (format t "~%~%Variable documentation:~%~A"  doc)))
  (when (fboundp sym)
    (describe-function sym)))

;;; Debugging

(defclass slime-env (env:environment) 
  ((debugger-hook :initarg :debugger-hoook)))

(defun slime-env (hook io-bindings) 
  (make-instance 'slime-env :name "SLIME Environment" 
                 :io-bindings io-bindings
                 :debugger-hoook hook))

(defmethod env-internals:environment-display-notifier
    ((env slime-env) &key restarts condition)
  (declare (ignore restarts))
  (funcall (slot-value env 'debugger-hook) condition *debugger-hook*))

(defmethod env-internals:environment-display-debugger ((env slime-env))
  *debug-io*)

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook))
    (env:with-environment ((slime-env hook '()))
      (funcall fun))))

(defvar *sldb-top-frame*)

(defun interesting-frame-p (frame)
  (cond ((or (dbg::call-frame-p frame)
             (dbg::derived-call-frame-p frame)
             (dbg::foreign-frame-p frame)
             (dbg::interpreted-call-frame-p frame))
         t)
        ((dbg::catch-frame-p frame) dbg:*print-catch-frames*)
        ((dbg::binding-frame-p frame) dbg:*print-binding-frames*)
        ((dbg::handler-frame-p frame) dbg:*print-handler-frames*)
        ((dbg::restart-frame-p frame) dbg:*print-restart-frames*)
        ((dbg::open-frame-p frame) dbg:*print-open-frames*)
        (t nil)))

(defun nth-next-frame (frame n)
  "Unwind FRAME N times."
  (do ((frame frame (dbg::frame-next frame))
       (i n (if (interesting-frame-p frame) (1- i) i)))
      ((or (not frame)
           (and (interesting-frame-p frame) (zerop i)))
       frame)))

(defun nth-frame (index)
  (nth-next-frame *sldb-top-frame* index))
           
(defun find-top-frame ()
  "Return the most suitable top-frame for the debugger."
  (or (do ((frame (dbg::debugger-stack-current-frame dbg::*debugger-stack*)
                  (nth-next-frame frame 1)))
          ((or (null frame)             ; no frame found!
               (and (dbg::call-frame-p frame)
                    (eq (dbg::call-frame-function-name frame) 
                        'invoke-debugger)))
           (nth-next-frame frame 1)))
      ;; if we can't find a invoke-debugger frame, take any old frame at the top
      (dbg::debugger-stack-current-frame dbg::*debugger-stack*)))
  
(defimplementation call-with-debugging-environment (fn)
  (dbg::with-debugger-stack ()
    (let ((*sldb-top-frame* (find-top-frame)))
      (funcall fn))))

(defimplementation compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum))
	(backtrace '()))
    (do ((frame (nth-frame start) (dbg::frame-next frame))
	 (i start))
	((or (not frame) (= i end)) (nreverse backtrace))
      (when (interesting-frame-p frame)
	(incf i)
	(push frame backtrace)))))

(defun frame-actual-args (frame)
  (let ((*break-on-signals* nil))
    (mapcar (lambda (arg)
              (case arg
                ((&rest &optional &key) arg)
                (t
                 (handler-case (dbg::dbg-eval arg frame)
                   (error (e) (format nil "<~A>" arg))))))
            (dbg::call-frame-arglist frame))))

(defimplementation print-frame (frame stream)
  (cond ((dbg::call-frame-p frame)
         (format stream "~S ~S"
                 (dbg::call-frame-function-name frame)
                 (frame-actual-args frame)))
        (t (princ frame stream))))

(defun frame-vars (frame)
  (first (dbg::frame-locals-format-list frame #'list 75 0)))

(defimplementation frame-locals (n)
  (let ((frame (nth-frame n)))
    (if (dbg::call-frame-p frame)
        (mapcar (lambda (var)
                  (destructuring-bind (name value symbol location) var
                    (declare (ignore name location))
                    (list :name symbol :id 0
                          :value value)))
                (frame-vars frame)))))

(defimplementation frame-var-value (frame var)
  (let ((frame (nth-frame frame)))
    (destructuring-bind (_n value _s _l) (nth var (frame-vars frame))
      (declare (ignore _n _s _l))
      value)))

(defimplementation frame-catch-tags (index)
  (declare (ignore index))
  nil)

(defimplementation frame-source-location-for-emacs (frame)
  (let ((frame (nth-frame frame))
        (callee (if (plusp frame) (nth-frame (1- frame)))))
    (if (dbg::call-frame-p frame)
	(let ((dspec (dbg::call-frame-function-name frame))
              (cname (and (dbg::call-frame-p callee)
                          (dbg::call-frame-function-name callee))))
	  (if dspec
              (frame-location dspec cname))))))

(defimplementation eval-in-frame (form frame-number)
  (let ((frame (nth-frame frame-number)))
    (dbg::dbg-eval form frame)))

(defimplementation return-from-frame (frame-number form)
  (let* ((frame (nth-frame frame-number))
         (return-frame (dbg::find-frame-for-return frame)))
    (dbg::dbg-return-from-call-frame frame form return-frame 
                                     dbg::*debugger-stack*)))

(defimplementation restart-frame (frame-number)
  (let ((frame (nth-frame frame-number)))
    (dbg::restart-frame frame :same-args t)))

;;; Definition finding

(defun frame-location (dspec callee-name)
  (let ((infos (dspec:find-dspec-locations dspec)))
    (cond (infos 
           (destructuring-bind ((rdspec location) &rest _) infos
             (declare (ignore _))
             (let ((name (and callee-name (symbolp callee-name)
                              (string callee-name))))
               (make-dspec-location rdspec location 
                                    `(:call-site ,name)))))
          (t 
           (list :error (format nil "Source location not available for: ~S" 
                                dspec))))))

(defimplementation find-definitions (name)
  (let ((locations (dspec:find-name-locations dspec:*dspec-classes* name)))
    (loop for (dspec location) in locations
          collect (list dspec (make-dspec-location dspec location)))))


;;; Compilation 

(defmacro with-swank-compilation-unit ((location &rest options) &body body)
  (lw:rebinding (location)
    `(let ((compiler::*error-database* '()))
       (with-compilation-unit ,options
         ,@body
         (signal-error-data-base compiler::*error-database* ,location)
         (signal-undefined-functions compiler::*unknown-functions* ,location)))))

(defimplementation swank-compile-file (filename load-p external-format)
  (with-swank-compilation-unit (filename)
    (compile-file filename :load load-p :external-format external-format)))

(defvar *within-call-with-compilation-hooks* nil
  "Whether COMPILE-FILE was called from within CALL-WITH-COMPILATION-HOOKS.")

(defvar *undefined-functions-hash* nil
  "Hash table to map info about undefined functions to pathnames.")

(lw:defadvice (compile-file compile-file-and-collect-notes :around)
    (pathname &rest rest)
  (multiple-value-prog1 (apply #'lw:call-next-advice pathname rest)
    (when *within-call-with-compilation-hooks*
      (maphash (lambda (unfun dspecs)
                 (dolist (dspec dspecs)
                   (let ((unfun-info (list unfun dspec)))
                     (unless (gethash unfun-info *undefined-functions-hash*)
                       (setf (gethash unfun-info *undefined-functions-hash*)
                               pathname)))))
               compiler::*unknown-functions*))))

(defimplementation call-with-compilation-hooks (function)
  (let ((compiler::*error-database* '())
        (*undefined-functions-hash* (make-hash-table :test 'equal))
        (*within-call-with-compilation-hooks* t))
    (with-compilation-unit ()
      (prog1 (funcall function)
        (signal-error-data-base compiler::*error-database*)
        (signal-undefined-functions compiler::*unknown-functions*)))))

(defun map-error-database (database fn)
  (loop for (filename . defs) in database do
	(loop for (dspec . conditions) in defs do
	      (dolist (c conditions) 
		(funcall fn filename dspec (if (consp c) (car c) c))))))

(defun lispworks-severity (condition)
  (cond ((not condition) :warning)
	(t (etypecase condition
	     (error :error)
	     (style-warning :warning)
	     (warning :warning)))))

(defun signal-compiler-condition (message location condition)
  (check-type message string)
  (signal 
   (make-instance 'compiler-condition :message message 
		  :severity (lispworks-severity condition) 
		  :location location
		  :original-condition condition)))

(defun compile-from-temp-file (string filename)
  (unwind-protect
       (progn
	 (with-open-file (s filename :direction :output :if-exists :supersede)
	   (write-string string s)
	   (finish-output s))
	 (let ((binary-filename (compile-file filename :load t)))
           (when binary-filename
             (delete-file binary-filename))))
    (delete-file filename)))

(defun dspec-buffer-position (dspec offset)
  (etypecase dspec
    (cons (let ((name (dspec:dspec-primary-name dspec)))
            (typecase name
              ((or symbol string) 
               (list :function-name (string name)))
              (t (list :position offset)))))
    (null (list :position offset))
    (symbol (list :function-name (string dspec)))))

(defmacro with-fairly-standard-io-syntax (&body body)
  "Like WITH-STANDARD-IO-SYNTAX but preserve *PACKAGE* and *READTABLE*."
  (let ((package (gensym))
        (readtable (gensym)))
    `(let ((,package *package*)
           (,readtable *readtable*))
      (with-standard-io-syntax
        (let ((*package* ,package)
              (*readtable* ,readtable))
          ,@body)))))

#-(or lispworks4.1 lispworks4.2) ; no dspec:parse-form-dspec prior to 4.3
(defun dspec-stream-position (stream dspec)
  (with-fairly-standard-io-syntax
    (loop (let* ((pos (file-position stream))
                 (form (read stream nil '#1=#:eof)))
            (when (eq form '#1#)
              (return nil))
            (labels ((check-dspec (form)
                       (when (consp form)
                         (let ((operator (car form)))
                           (case operator
                             ((progn)
                              (mapcar #'check-dspec
                                      (cdr form)))
                             ((eval-when locally macrolet symbol-macrolet)
                              (mapcar #'check-dspec
                                      (cddr form)))
                             ((in-package)
                              (let ((package (find-package (second form))))
                                (when package
                                  (setq *package* package))))
                             (otherwise
                              (let ((form-dspec (dspec:parse-form-dspec form)))
                                (when (dspec:dspec-equal dspec form-dspec)
                                  (return pos)))))))))
              (check-dspec form))))))

(defun dspec-file-position (file dspec)
  (let* ((*compile-file-pathname* (pathname file))
         (*compile-file-truename* (truename *compile-file-pathname*))
         (*load-pathname* *compile-file-pathname*)
         (*load-truename* *compile-file-truename*))
    (with-open-file (stream file)
      (let ((pos 
             #-(or lispworks4.1 lispworks4.2)
             (dspec-stream-position stream dspec)))
        (if pos
            (list :position (1+ pos) t)
            (dspec-buffer-position dspec 1))))))

(defun emacs-buffer-location-p (location)
  (and (consp location)
       (eq (car location) :emacs-buffer)))

(defun make-dspec-location (dspec location &optional hints)
  (etypecase location
    ((or pathname string)
     (multiple-value-bind (file err) 
         (ignore-errors (namestring (truename location)))
       (if err
           (list :error (princ-to-string err))
           (make-location `(:file ,file)
                          (dspec-file-position file dspec)
                          hints))))
    (symbol 
     `(:error ,(format nil "Cannot resolve location: ~S" location)))
    ((satisfies emacs-buffer-location-p)
     (destructuring-bind (_ buffer offset string) location
       (declare (ignore _ string))
       (make-location `(:buffer ,buffer)
                      (dspec-buffer-position dspec offset)
                      hints)))))

(defun make-dspec-progenitor-location (dspec location)
  (let ((canon-dspec (dspec:canonicalize-dspec dspec)))
    (make-dspec-location
     (if canon-dspec
         (if (dspec:local-dspec-p canon-dspec)
             (dspec:dspec-progenitor canon-dspec)
           canon-dspec)
       nil)
     location)))

(defun signal-error-data-base (database &optional location)
  (map-error-database 
   database
   (lambda (filename dspec condition)
     (signal-compiler-condition
      (format nil "~A" condition)
      (make-dspec-progenitor-location dspec (or location filename))
      condition))))

(defun unmangle-unfun (symbol)
  "Converts symbols like 'SETF::|\"CL-USER\" \"GET\"| to
function names like \(SETF GET)."
  (cond ((sys::setf-symbol-p symbol)
         (sys::setf-pair-from-underlying-name symbol))
        (t symbol)))
                    
(defun signal-undefined-functions (htab &optional filename)
  (maphash (lambda (unfun dspecs)
	     (dolist (dspec dspecs)
	       (signal-compiler-condition 
		(format nil "Undefined function ~A" (unmangle-unfun unfun))
		(make-dspec-progenitor-location dspec
                                                (or filename
                                                    (gethash (list unfun dspec)
                                                             *undefined-functions-hash*)))
		nil)))
	   htab))

(defimplementation swank-compile-string (string &key buffer position directory
                                                debug)
  (declare (ignore directory debug))
  (assert buffer)
  (assert position)
  (let* ((location (list :emacs-buffer buffer position string))
         (tmpname (hcl:make-temp-file nil "lisp")))
    (with-swank-compilation-unit (location)
      (compile-from-temp-file 
       (with-output-to-string (s)
         (let ((*print-radix* t))
           (print `(eval-when (:compile-toplevel)
                     (setq dspec::*location* (list ,@location)))
                  s))
         (write-string string s))
       tmpname))))

;;; xref

(defmacro defxref (name function)
  `(defimplementation ,name (name)
    (xref-results (,function name))))

(defxref who-calls      hcl:who-calls)
(defxref who-macroexpands hcl:who-calls) ; macros are in the calls table too
(defxref calls-who      hcl:calls-who)
(defxref list-callers   list-callers-internal)
;; (defxref list-callees   list-callees-internal)

(defun list-callers-internal (name)
  (let ((callers (make-array 100
                             :fill-pointer 0
                             :adjustable t)))
    (hcl:sweep-all-objects
     #'(lambda (object)
         (when (and #+Harlequin-PC-Lisp (low:compiled-code-p object)
                    #-Harlequin-PC-Lisp (sys::callablep object)
                    (system::find-constant$funcallable name object))
           (vector-push-extend object callers))))
    ;; Delay dspec:object-dspec until after sweep-all-objects
    ;; to reduce allocation problems.
    (loop for object across callers
          collect (if (symbolp object)
		      (list 'function object)
                      (or (dspec:object-dspec object) object)))))

;; only for lispworks 4.2 and above
#-lispworks4.1
(progn
  (defxref who-references hcl:who-references)
  (defxref who-binds      hcl:who-binds)
  (defxref who-sets       hcl:who-sets))

(defimplementation who-specializes (classname)
  (let ((methods (clos:class-direct-methods (find-class classname))))
    (xref-results (mapcar #'dspec:object-dspec methods))))

(defun xref-results (dspecs)
  (flet ((frob-locs (dspec locs)
           (cond (locs
                  (loop for (name loc) in locs
                        collect (list name (make-dspec-location name loc))))
                 (t `((,dspec (:error "Source location not available")))))))
    (loop for dspec in dspecs
          append (frob-locs dspec (dspec:dspec-definition-locations dspec)))))

;;; Inspector

(defmethod emacs-inspect ((o t))
  (lispworks-inspect o))

(defmethod emacs-inspect ((o function))
  (lispworks-inspect o))

;; FIXME: slot-boundp-using-class in LW works with names so we can't
;; use our method in swank.lisp.
(defmethod emacs-inspect ((o standard-object))
  (lispworks-inspect o))

(defun lispworks-inspect (o)
  (multiple-value-bind (names values _getter _setter type)
      (lw:get-inspector-values o nil)
    (declare (ignore _getter _setter))
            (append 
             (label-value-line "Type" type)
             (loop for name in names
                   for value in values
                   append (label-value-line name value)))))

;;; Miscellaneous

(defimplementation quit-lisp ()
  (lispworks:quit))

;;; Tracing

(defun parse-fspec (fspec)
  "Return a dspec for FSPEC."
  (ecase (car fspec)
    ((:defmethod) `(method ,(cdr fspec)))))

(defun tracedp (dspec) 
  (member dspec (eval '(trace)) :test #'equal))

(defun toggle-trace-aux (dspec)
  (cond ((tracedp dspec)
         (eval `(untrace ,dspec))
         (format nil "~S is now untraced." dspec))
        (t
         (eval `(trace (,dspec)))
         (format nil "~S is now traced." dspec))))

(defimplementation toggle-trace (fspec)
  (toggle-trace-aux (parse-fspec fspec)))

;;; Multithreading

(defimplementation initialize-multiprocessing (continuation)
  (cond ((not mp::*multiprocessing*)
         (push (list "Initialize SLIME" '() continuation) 
               mp:*initial-processes*)
         (mp:initialize-multiprocessing))
        (t (funcall continuation))))

(defimplementation spawn (fn &key name)
  (let ((mp:*process-initial-bindings* 
         (remove (find-package :cl) 
                 mp:*process-initial-bindings*
                 :key (lambda (x) (symbol-package (car x))))))
    (mp:process-run-function name () fn)))

(defvar *id-lock* (mp:make-lock))
(defvar *thread-id-counter* 0)

(defimplementation thread-id (thread)
  (mp:with-lock (*id-lock*)
    (or (getf (mp:process-plist thread) 'id)
        (setf (getf (mp:process-plist thread) 'id)
              (incf *thread-id-counter*)))))

(defimplementation find-thread (id)
  (find id (mp:list-all-processes) 
        :key (lambda (p) (getf (mp:process-plist p) 'id))))

(defimplementation thread-name (thread)
  (mp:process-name thread))

(defimplementation thread-status (thread)
  (format nil "~A ~D" 
          (mp:process-whostate thread)
          (mp:process-priority thread)))

(defimplementation make-lock (&key name)
  (mp:make-lock :name name))

(defimplementation call-with-lock-held (lock function)
  (mp:with-lock (lock) (funcall function)))

(defimplementation current-thread ()
  mp:*current-process*)

(defimplementation all-threads ()
  (mp:list-all-processes))

(defimplementation interrupt-thread (thread fn)
  (mp:process-interrupt thread fn))

(defimplementation kill-thread (thread)
  (mp:process-kill thread))

(defimplementation thread-alive-p (thread)
  (mp:process-alive-p thread))

(defvar *mailbox-lock* (mp:make-lock))

(defun mailbox (thread)
  (mp:with-lock (*mailbox-lock*)
    (or (getf (mp:process-plist thread) 'mailbox)
        (setf (getf (mp:process-plist thread) 'mailbox)
              (mp:make-mailbox)))))

(defimplementation receive ()
  (mp:mailbox-read (mailbox mp:*current-process*)))

(defimplementation send (thread object)
  (mp:mailbox-send (mailbox thread) object))

;;; Some intergration with the lispworks environment

(defun swank-sym (name) (find-symbol (string name) :swank))

(defimplementation emacs-connected ()
  (when (eq (eval (swank-sym :*communication-style*))
            nil)
    (set-sigint-handler))
  ;; pop up the slime debugger by default
  (let ((lw:*handle-warn-on-redefinition* :warn))
    (defmethod env-internals:environment-display-notifier 
        (env &key restarts condition)
      (declare (ignore restarts))
      (funcall (swank-sym :swank-debugger-hook) condition *debugger-hook*))
    (defmethod env-internals:environment-display-debugger (env)
      *debug-io*)))

(defimplementation make-stream-interactive (stream)
  (unless (find-method #'stream:stream-soft-force-output nil `((eql ,stream))
                       nil)
    (let ((lw:*handle-warn-on-redefinition* :warn))
      (defmethod stream:stream-soft-force-output  ((o (eql stream)))
        (force-output o)))))

(defmethod env-internals:confirm-p ((e slime-env) &optional msg &rest args)
  (apply (swank-sym :y-or-n-p-in-emacs) msg args))
      

;;;; Weak hashtables

(defimplementation make-weak-key-hash-table (&rest args)
  (apply #'make-hash-table :weak-kind :key args))

(defimplementation make-weak-value-hash-table (&rest args)
  (apply #'make-hash-table :weak-kind :value args))
