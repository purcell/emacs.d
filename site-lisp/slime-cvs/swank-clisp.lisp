;;;; SWANK support for CLISP.

;;;; Copyright (C) 2003, 2004 W. Jenkner, V. Sedach

;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation; either version 2 of
;;;; the License, or (at your option) any later version.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public
;;;; License along with this program; if not, write to the Free
;;;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;;; MA 02111-1307, USA.

;;; This is work in progress, but it's already usable.  Many things
;;; are adapted from other swank-*.lisp, in particular from
;;; swank-allegro (I don't use allegro at all, but it's the shortest
;;; one and I found Helmut Eller's code there enlightening).

;;; This code will work better with recent versions of CLISP (say, the
;;; last release or CVS HEAD) while it may not work at all with older
;;; versions.  It is reasonable to expect it to work on platforms with
;;; a "SOCKET" package, in particular on GNU/Linux or Unix-like
;;; systems, but also on Win32.  This backend uses the portable xref
;;; from the CMU AI repository and metering.lisp from CLOCC [1], which
;;; are conveniently included in SLIME.

;;; [1] http://cvs.sourceforge.net/viewcvs.py/clocc/clocc/src/tools/metering/

(in-package :swank-backend)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;(use-package "SOCKET")
  (use-package "GRAY"))

;;;; if this lisp has the complete CLOS then we use it, otherwise we
;;;; build up a "fake" swank-mop and then override the methods in the
;;;; inspector.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *have-mop*
    (and (find-package :clos)
         (eql :external
              (nth-value 1 (find-symbol (string ':standard-slot-definition)
					:clos))))
    "True in those CLISP images which have a complete MOP implementation."))

#+#.(cl:if swank-backend::*have-mop* '(cl:and) '(cl:or))
(progn
  (import-swank-mop-symbols :clos '(:slot-definition-documentation))
  
  (defun swank-mop:slot-definition-documentation (slot)
    (clos::slot-definition-documentation slot)))

#-#.(cl:if swank-backend::*have-mop* '(and) '(or))
(defclass swank-mop:standard-slot-definition ()
  ()
  (:documentation 
   "Dummy class created so that swank.lisp will compile and load."))

;; #+#.(cl:if (cl:find-package "LINUX") '(and) '(or))
;; (progn
;;   (defmacro with-blocked-signals ((&rest signals) &body body)
;;     (ext:with-gensyms ("SIGPROCMASK" ret mask)
;;       `(multiple-value-bind (,ret ,mask)
;; 	   (linux:sigprocmask-set-n-save
;; 	    ,linux:SIG_BLOCK
;; 	    ,(do ((sigset (linux:sigset-empty)
;; 			  (linux:sigset-add sigset (the fixnum (pop signals)))))
;; 		 ((null signals) sigset)))
;; 	 (linux:check-res ,ret 'linux:sigprocmask-set-n-save)
;; 	 (unwind-protect
;; 	      (progn ,@body)
;; 	   (linux:sigprocmask-set ,linux:SIG_SETMASK ,mask nil)))))

;;   (defimplementation call-without-interrupts (fn)
;;     (with-blocked-signals (#.linux:SIGINT) (funcall fn))))

;; #+#.(cl:if (cl:find-package "LINUX") '(or) '(and))
(defimplementation call-without-interrupts (fn)
  (funcall fn))

(let ((getpid (or (find-symbol "PROCESS-ID" :system)
		  ;; old name prior to 2005-03-01, clisp <= 2.33.2
		  (find-symbol "PROGRAM-ID" :system)
		  #+win32 ; integrated into the above since 2005-02-24
		  (and (find-package :win32) ; optional modules/win32
		       (find-symbol "GetCurrentProcessId" :win32)))))
  (defimplementation getpid () ; a required interface
    (cond
      (getpid (funcall getpid))
      #+win32 ((ext:getenv "PID")) ; where does that come from?
      (t -1))))

(defimplementation lisp-implementation-type-name ()
  "clisp")

(defimplementation set-default-directory (directory)
  (setf (ext:default-directory) directory)
  (namestring (setf *default-pathname-defaults* (ext:default-directory))))


;;; TCP Server

(defimplementation create-socket (host port)
  (declare (ignore host))
  (socket:socket-server port))

(defimplementation local-port (socket)
  (socket:socket-server-port socket))

(defimplementation close-socket (socket)
  (socket:socket-server-close socket))
  
(defimplementation accept-connection (socket
				      &key external-format buffering timeout)
  (declare (ignore buffering timeout))
  (socket:socket-accept socket
			:buffered nil ;; XXX should be t
			:element-type 'character
			:external-format external-format))

;;; Coding systems

(defvar *external-format-to-coding-system*
  '(((:charset "iso-8859-1" :line-terminator :unix) 
     "latin-1-unix" "iso-latin-1-unix" "iso-8859-1-unix")
    ((:charset "iso-8859-1":latin-1) 
     "latin-1" "iso-latin-1" "iso-8859-1")
    ((:charset "utf-8") "utf-8")
    ((:charset "utf-8" :line-terminator :unix) "utf-8-unix")
    ((:charset "euc-jp") "euc-jp")
    ((:charset "euc-jp" :line-terminator :unix) "euc-jp-unix")
    ((:charset "us-ascii") "us-ascii")
    ((:charset "us-ascii" :line-terminator :unix) "us-ascii-unix")))

(defimplementation find-external-format (coding-system)
  (let ((args (car (rassoc-if (lambda (x) 
				(member coding-system x :test #'equal))
			      *external-format-to-coding-system*))))
    (and args (apply #'ext:make-encoding args))))


;;; Swank functions

(defimplementation arglist (fname)
  (block nil
    (or (ignore-errors
	  (let ((exp (function-lambda-expression fname)))
	    (and exp (return (second exp)))))
	(ignore-errors 
	  (return (ext:arglist fname)))
	:not-available)))

(defimplementation macroexpand-all (form)
  (ext:expand-form form))

(defimplementation describe-symbol-for-emacs (symbol)
  "Return a plist describing SYMBOL.
Return NIL if the symbol is unbound."
  (let ((result ()))
    (flet ((doc (kind)
	     (or (documentation symbol kind) :not-documented))
	   (maybe-push (property value)
	     (when value
	       (setf result (list* property value result)))))
      (maybe-push :variable (when (boundp symbol) (doc 'variable)))
      (when (fboundp symbol)
	(maybe-push
	 ;; Report WHEN etc. as macros, even though they may be
	 ;; implemented as special operators.
	 (if (macro-function symbol) :macro
	     (typecase (fdefinition symbol)
	       (generic-function :generic-function)
	       (function         :function)
	       ;; (type-of 'progn) -> ext:special-operator
	       (t                :special-operator)))
	 (doc 'function)))
      (when (or (get symbol 'system::setf-function) ; e.g. #'(setf elt)
		(get symbol 'system::setf-expander)); defsetf
	(maybe-push :setf (doc 'setf)))
      (when (or (get symbol 'system::type-symbol); cf. clisp/src/describe.lisp
		(get symbol 'system::defstruct-description)
		(get symbol 'system::deftype-expander))
	(maybe-push :type (doc 'type))) ; even for 'structure
      (when (find-class symbol nil)
	(maybe-push :class (doc 'type)))
      ;; Let this code work compiled in images without FFI
      (let ((types (load-time-value
		    (and (find-package "FFI")
			 (symbol-value 
			  (find-symbol "*C-TYPE-TABLE*" "FFI"))))))
	;; Use ffi::*c-type-table* so as not to suffer the overhead of
	;; (ignore-errors (ffi:parse-c-type symbol)) for 99.9% of symbols
	;; which are not FFI type names.
	(when (and types (nth-value 1 (gethash symbol types)))
	  ;; Maybe use (case (head (ffi:deparse-c-type)))
	  ;; to distinguish struct and union types?
	  (maybe-push :alien-type :not-documented)))
      result)))

(defimplementation describe-definition (symbol namespace)
  (ecase namespace
    (:variable (describe symbol))
    (:macro (describe (macro-function symbol)))
    (:function (describe (symbol-function symbol)))
    (:class (describe (find-class symbol)))))

(defun fspec-pathname (symbol)
  (let ((path (documentation symbol 'sys::file))
	lines)
    (when (consp path)
      (psetq path (car path)
	     lines (cdr path)))
    (when (and path
	       (member (pathname-type path)
		       custom:*compiled-file-types* :test #'equal))
      (setq path
	    (loop for suffix in custom:*source-file-types*
	       thereis (probe-file (make-pathname :defaults path
						  :type suffix)))))
    (values path lines)))

(defun fspec-location (fspec)
  (multiple-value-bind (file lines)
      (fspec-pathname fspec)
    (cond (file
	   (multiple-value-bind (truename c) (ignore-errors (truename file))
	     (cond (truename
		    (make-location (list :file (namestring truename))
				   (if (consp lines)
				       (list* :line lines)
				       (list :function-name (string fspec)))))
		   (t (list :error (princ-to-string c))))))
	  (t (list :error (format nil "No source information available for: ~S"
				  fspec))))))

(defimplementation find-definitions (name)
  (list (list name (fspec-location name))))

(defun trim-whitespace (string)
  (string-trim #(#\newline #\space #\tab) string))

(defvar *sldb-backtrace*)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let* (;;(sys::*break-count* (1+ sys::*break-count*))
	 ;;(sys::*driver* debugger-loop-fn)
	 ;;(sys::*fasoutput-stream* nil)
	 (*sldb-backtrace* (nthcdr 5 (sldb-backtrace))))
    (funcall debugger-loop-fn)))

(defun nth-frame (index) 
  (nth index *sldb-backtrace*))

;; This is the old backtrace implementation.  Not sure yet wheter the
;; new is much better.
;;
;;(defimplementation compute-backtrace (start end)
;;  (let ((end (or end most-positive-fixnum)))
;;    (loop for last = nil then frame
;;	    for frame = (nth-frame start) then (frame-up frame)
;;	    for i from start below end
;;	    until (or (eq frame last) (not frame))
;;	    collect frame)))
;; 
;;(defimplementation print-frame (frame stream)
;;  (write-string (trim-whitespace
;;		   (with-output-to-string (stream)
;;		     (sys::describe-frame stream frame)))
;;		  stream))
;;
;;(defimplementation frame-locals (frame-number)
;;  (let* ((frame (nth-frame frame-number))
;;	   (frame-env (sys::eval-at frame '(sys::the-environment))))
;;    (append
;;     (frame-do-venv frame (svref frame-env 0))
;;     (frame-do-fenv frame (svref frame-env 1))
;;     (frame-do-benv frame (svref frame-env 2))
;;     (frame-do-genv frame (svref frame-env 3))
;;     (frame-do-denv frame (svref frame-env 4)))))
;;
;;(defimplementation frame-var-value (frame var)
;;  (getf (nth var (frame-locals frame)) :value))

(defun format-frame (frame)
  (trim-whitespace 
   (with-output-to-string (s) 
     (sys::describe-frame s frame))))

(defun function-frame-p (frame)
  ;; We are interested in frames which like look "<5> foo ...".  
  ;; Ugly, I know.
  (char= #\< (aref (format-frame frame) 0)))

(defun sldb-backtrace ()
  "Return a list ((ADDRESS . DESCRIPTION) ...) of frames."
  (do ((fframes '())
       (last nil frame)
       (frame (sys::the-frame) (sys::frame-up-1 frame 1)))
      ((eq frame last) (nreverse fframes))
    (when (function-frame-p frame)
      (push (cons frame (format-frame frame)) fframes))))

(defimplementation compute-backtrace (start end)
  (let* ((bt *sldb-backtrace*)
	 (len (length bt)))
    (subseq bt start (min (or end len) len))))

(defimplementation print-frame (frame stream)
  (let ((desc (cdr frame)))
    (write-string (subseq (cdr frame)
			  (+ (position #\> desc) 2)
			  (position #\newline desc))
		  stream)))

(defimplementation format-sldb-condition (condition)
  (trim-whitespace (princ-to-string condition)))

(defimplementation eval-in-frame (form frame-number)
  (sys::eval-at (car (nth-frame frame-number)) form))

;; Don't know how to access locals.  Return some strings instead.
;; Maybe we should search some frame nearby with a 'sys::the-environment?
(defimplementation frame-locals (frame-number) 
  (let ((desc (cdr (nth-frame frame-number))))
    (list (list :name :|| :id 0
		:value (trim-whitespace 
			(subseq desc (position #\newline desc)))))))

(defimplementation frame-var-value (frame var) nil)

;; Interpreter-Variablen-Environment has the shape
;; NIL or #(v1 val1 ... vn valn NEXT-ENV).

(defun frame-do-venv (frame venv)
  (loop for i from 1 below (length venv) by 2
	as symbol = (svref venv (1- i))
	and value = (svref venv i)
	collect (list :name symbol :id 0
		      :value (if (eq sys::specdecl value)
                                 ;; special variable
                                 (sys::eval-at frame symbol)
                                 ;; lexical variable or symbol macro
                                 value))))

(defun frame-do-fenv (frame fenv)
  (declare (ignore frame fenv))
  nil)

(defun frame-do-benv (frame benv)
  (declare (ignore frame benv))
  nil)

(defun frame-do-genv (frame genv)
  (declare (ignore frame genv))
  nil)

(defun frame-do-denv (frame denv)
  (declare (ignore frame denv))
  nil)

(defimplementation frame-catch-tags (index)
  (declare (ignore index))
  nil)

(defimplementation return-from-frame (index form)
  (sys::return-from-eval-frame (car (nth-frame index)) form))

(defimplementation restart-frame (index)
  (sys::redo-eval-frame (car (nth-frame index))))

(defimplementation frame-source-location-for-emacs (index)
  `(:error 
    ,(format nil "frame-source-location not implemented. (frame: ~A)" 
	     (car (nth-frame index)))))

;;; Profiling

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

;;; Handle compiler conditions (find out location of error etc.)

(defmacro compile-file-frobbing-notes ((&rest args) &body body)
  "Pass ARGS to COMPILE-FILE, send the compiler notes to
*STANDARD-INPUT* and frob them in BODY."
  `(let ((*error-output* (make-string-output-stream))
	 (*compile-verbose* t))
     (multiple-value-prog1
      (compile-file ,@args)
      (handler-case  
       (with-input-from-string
	(*standard-input* (get-output-stream-string *error-output*))
	,@body)
       (sys::simple-end-of-file () nil)))))

(defvar *orig-c-warn* (symbol-function 'system::c-warn))
(defvar *orig-c-style-warn* (symbol-function 'system::c-style-warn))
(defvar *orig-c-error* (symbol-function 'system::c-error))
(defvar *orig-c-report-problems* (symbol-function 'system::c-report-problems))

(defmacro dynamic-flet (names-functions &body body)
  "(dynamic-flet ((NAME FUNCTION) ...) BODY ...)
Execute BODY with NAME's function slot set to FUNCTION."
  `(ext:letf* ,(loop for (name function) in names-functions
		     collect `((symbol-function ',name) ,function))
    ,@body))

(defvar *buffer-name* nil)
(defvar *buffer-offset*)

(defun compiler-note-location ()
  "Return the current compiler location."
  (let ((lineno1 sys::*compile-file-lineno1*)
	(lineno2 sys::*compile-file-lineno2*)
	(file sys::*compile-file-truename*))
    (cond ((and file lineno1 lineno2)
	   (make-location (list ':file (namestring file))
			  (list ':line lineno1)))
	  (*buffer-name*
	   (make-location (list ':buffer *buffer-name*) 
			  (list ':position *buffer-offset*)))
	  (t
	   (list :error "No error location available")))))

(defun signal-compiler-warning (cstring args severity orig-fn)
  (signal (make-condition 'compiler-condition
			  :severity severity
			  :message (apply #'format nil cstring args)
			  :location (compiler-note-location)))
  (apply orig-fn cstring args))

(defun c-warn (cstring &rest args)
  (signal-compiler-warning cstring args :warning *orig-c-warn*))

(defun c-style-warn (cstring &rest args)
  (dynamic-flet ((sys::c-warn *orig-c-warn*))
    (signal-compiler-warning cstring args :style-warning *orig-c-style-warn*)))

(defun c-error (cstring &rest args)
  (signal-compiler-warning cstring args :error *orig-c-error*))

(defimplementation call-with-compilation-hooks (function)
  (handler-bind ((warning #'handle-notification-condition))
    (dynamic-flet ((system::c-warn #'c-warn)
		   (system::c-style-warn #'c-style-warn)
		   (system::c-error #'c-error))
      (funcall function))))

(defun handle-notification-condition (condition)
  "Handle a condition caused by a compiler warning."
  (signal (make-condition 'compiler-condition
			  :original-condition condition
			  :severity :warning
			  :message (princ-to-string condition)
			  :location (compiler-note-location))))

(defimplementation swank-compile-file (filename load-p external-format)
  (with-compilation-hooks ()
    (with-compilation-unit ()
      (let ((fasl-file (compile-file filename 
				     :external-format external-format)))
	(when (and load-p fasl-file)
	  (load fasl-file))
	nil))))

(defimplementation swank-compile-string (string &key buffer position directory)
  (declare (ignore directory))
  (with-compilation-hooks ()
    (let ((*buffer-name* buffer)
	  (*buffer-offset* position))
      (funcall (compile nil (read-from-string
                             (format nil "(~S () ~A)" 'lambda string)))))))

;;; Portable XREF from the CMU AI repository.

(setq pxref::*handle-package-forms* '(cl:in-package))

(defmacro defxref (name function)
  `(defimplementation ,name (name)
    (xref-results (,function name))))

(defxref who-calls      pxref:list-callers)
(defxref who-references pxref:list-readers)
(defxref who-binds      pxref:list-setters)
(defxref who-sets       pxref:list-setters)
(defxref list-callers   pxref:list-callers)
(defxref list-callees   pxref:list-callees)

(defun xref-results (symbols)
  (let ((xrefs '()))
    (dolist (symbol symbols)
      (push (list symbol (fspec-location symbol)) xrefs))
    xrefs))

(when (find-package :swank-loader)
  (setf (symbol-function (intern "USER-INIT-FILE" :swank-loader))
	(lambda ()
	  (let ((home (user-homedir-pathname)))
	    (and (ext:probe-directory home)
		 (probe-file (format nil "~A/.swank.lisp"
				     (namestring (truename home)))))))))

;; Don't set *debugger-hook* to nil on break.
(ext:without-package-lock () 
 (defun break (&optional (format-string "Break") &rest args)
   (if (not sys::*use-clcs*)
       (progn
	 (terpri *error-output*)
	 (apply #'format *error-output*
		(concatenate 'string "*** - " format-string)
		args)
	 (funcall ext:*break-driver* t))
       (let ((condition
	      (make-condition 'simple-condition
			      :format-control format-string
			      :format-arguments args))
	     ;;(*debugger-hook* nil)
	     ;; Issue 91
	     )				
	 (ext:with-restarts
	     ((continue
	       :report (lambda (stream)
			 (format stream (sys::text "Return from ~S loop")
				 'break))
	       ()))
	   (with-condition-restarts condition (list (find-restart 'continue))
				    (invoke-debugger condition)))))
   nil))

;;; Inspecting

(defclass clisp-inspector (inspector)
  ())

(defimplementation make-default-inspector ()
  (make-instance 'clisp-inspector))

(defmethod inspect-for-emacs ((o t) (inspector clisp-inspector))
  (declare (ignore inspector))
  (let* ((*print-array* nil) (*print-pretty* t)
	 (*print-circle* t) (*print-escape* t)
	 (*print-lines* custom:*inspect-print-lines*)
	 (*print-level* custom:*inspect-print-level*)
	 (*print-length* custom:*inspect-print-length*)
	 (sys::*inspect-all* (make-array 10 :fill-pointer 0 :adjustable t))
	 (tmp-pack (make-package (gensym "INSPECT-TMP-PACKAGE-")))
	 (*package* tmp-pack)
	 (sys::*inspect-unbound-value* (intern "#<unbound>" tmp-pack)))
    (let ((inspection (sys::inspect-backend o)))
      (values (format nil "~S~% ~A~{~%~A~}" o 
		      (sys::insp-title inspection)
		      (sys::insp-blurb inspection))
              (loop with count = (sys::insp-num-slots inspection)
                    for i upto count
		    for (value name) = (multiple-value-list 
					(funcall (sys::insp-nth-slot 
						  inspection) i))
		    collect `((:value ,name) " = " (:value ,value) 
			      (:newline)))))))

(defimplementation quit-lisp ()
  #+lisp=cl (ext:quit)
  #-lisp=cl (lisp:quit))

;;; Local Variables:
;;; eval: (put 'compile-file-frobbing-notes 'lisp-indent-function 1)
;;; eval: (put 'dynamic-flet 'common-lisp-indent-function 1)
;;; End:
