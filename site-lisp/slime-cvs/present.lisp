(in-package :swank)

;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.

;; A mechanism for printing to the slime repl so that the printed
;; result remembers what object it is associated with. Depends on the
;; ilisp bridge code being installed and ready to intercept messages
;; in the printed stream. We encode the information with a message
;; saying that we are starting to print an object corresponding to a
;; given id and another when we are done. The process filter notices these
;; and adds the necessary text properties to the output.

;; We only do this if we know we are printing to a slime stream,
;; checked with the method slime-stream-p. Initially this checks for
;; the knows slime streams looking at *connections*. In cmucl and
;; openmcl it also checks if it is a pretty-printing stream which
;; ultimately prints to a slime stream.

;; Control
(defvar *enable-presenting-readable-objects* t
  "set this to enable automatically printing presentations for some
subset of readable objects, such as pathnames."  )

;; doing it

(defmacro presenting-object (object stream &body body)
  "What you use in your code. Wrap this around some printing and that text will
be sensitive and remember what object it is in the repl"
  `(presenting-object-1 ,object ,stream #'(lambda () ,@body)))

(defmacro presenting-object-if (predicate object stream &body body)
  "What you use in your code. Wrap this around some printing and that text will
be sensitive and remember what object it is in the repl if predicate is true"
  (let ((continue (gensym)))
  `(let ((,continue #'(lambda () ,@body)))
    (if ,predicate
	(presenting-object-1 ,object ,stream ,continue)
	(funcall ,continue)))))

;;; Get pretty printer patches for SBCL
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-bind ((simple-error 
		  (lambda (c) 
		    (declare (ignore c))
		    (let ((clobber-it (find-restart 'sb-kernel::clobber-it)))
		      (when clobber-it (invoke-restart clobber-it))))))
    (sb-ext:without-package-locks
      (swank-backend::with-debootstrapping
	(load (make-pathname 
	       :name "sbcl-pprint-patch"
	       :type "lisp"
	       :directory (pathname-directory swank-loader:*source-directory*)))))))

(let ((last-stream nil)
      (last-answer nil))
  (defun slime-stream-p (stream)
    "Check if stream is one of the slime streams, since if it isn't we
don't want to present anything"
    (if (eq last-stream stream)
	last-answer
	(progn
	  (setq last-stream stream)
	  (if (eq stream t) 
	      (setq stream *standard-output*))
	  (setq last-answer 
		(or #+openmcl 
		    (and (typep stream 'ccl::xp-stream) 
					;(slime-stream-p (ccl::xp-base-stream (slot-value stream 'ccl::xp-structure)))
			 (slime-stream-p (ccl::%svref (slot-value stream 'ccl::xp-structure) 1)))
		    #+cmu
		    (or (and (typep stream 'lisp::indenting-stream)
			     (slime-stream-p (lisp::indenting-stream-stream stream)))
			(and (typep stream 'pretty-print::pretty-stream)
			     (fboundp 'pretty-print::enqueue-annotation)
			     (not *use-dedicated-output-stream*)
			     ;; Printing through CMUCL pretty streams
			     ;; is only cleanly possible if we are
			     ;; using the bridge-less protocol with
			     ;; annotations, because the bridge escape
			     ;; sequences disturb the pretty printer
			     ;; layout.
			     (slime-stream-p (pretty-print::pretty-stream-target  stream))))
		    #+sbcl
		    (or (and (typep stream 'sb-impl::indenting-stream)
			     (slime-stream-p (sb-impl::indenting-stream-stream stream)))
			(and (typep stream 'sb-pretty::pretty-stream)
			     (fboundp 'sb-pretty::enqueue-annotation)
			     (not *use-dedicated-output-stream*)
			     (slime-stream-p (sb-pretty::pretty-stream-target  stream))))
		    #+allegro
		    (and (typep stream 'excl:xp-simple-stream)
			 (slime-stream-p (excl::stream-output-handle stream)))
		    (loop for connection in *connections*
			  thereis (or (eq stream (connection.dedicated-output connection))
				      (eq stream (connection.socket-io connection))
				      (eq stream (connection.user-output connection))
				      (eq stream (connection.user-io connection))))))))))

(defun can-present-readable-objects (&optional stream)
  (declare (ignore stream))
  *enable-presenting-readable-objects*)

;; If we are printing to an XP (pretty printing) stream, printing the
;; escape sequences directly would mess up the layout because column
;; counting is disturbed.  Use "annotations" instead.
#+allegro
(defun write-annotation (stream function arg)
  (if (typep stream 'excl:xp-simple-stream)
      (excl::schedule-annotation stream function arg)
      (funcall function arg stream nil)))
#+cmu
(defun write-annotation (stream function arg)
  (if (and (typep stream 'pp:pretty-stream)
	   (fboundp 'pp::enqueue-annotation))
      (pp::enqueue-annotation stream function arg)
      (funcall function arg stream nil)))
#+sbcl
(defun write-annotation (stream function arg)
  (if (typep stream 'sb-pretty::pretty-stream)
      (sb-pretty::enqueue-annotation stream function arg)
      (funcall function arg stream nil)))
#-(or allegro cmu sbcl)
(defun write-annotation (stream function arg)
  (funcall function arg stream nil))

(defstruct presentation-record 
  (id)
  (printed-p))

(defun presentation-start (record stream truncatep) 
  (unless truncatep
    ;; Don't start new presentations when nothing is going to be
    ;; printed due to *print-lines*.
    (let ((pid (presentation-record-id record)))
      (cond (*use-dedicated-output-stream* 
	     (write-string "<" stream)
	     (prin1 pid stream)
	     (write-string "" stream))
	    (t
	     (finish-output stream)
	     (send-to-emacs `(:presentation-start ,pid)))))
    (setf (presentation-record-printed-p record) t)))
	   
(defun presentation-end (record stream truncatep)
  (declare (ignore truncatep))
  ;; Always end old presentations that were started.
  (when (presentation-record-printed-p record)
    (let ((pid (presentation-record-id record)))
      (cond (*use-dedicated-output-stream* 
	     (write-string ">" stream)
	     (prin1 pid stream)
	     (write-string "" stream))
	    (t
	     (finish-output stream)
	     (send-to-emacs `(:presentation-end ,pid)))))))

(defun presenting-object-1 (object stream continue)
  "Uses the bridge mechanism with two messages >id and <id. The first one
says that I am starting to print an object with this id. The second says I am finished"
  (if (and *record-repl-results* (slime-stream-p stream))
      (let* ((pid (swank::save-presented-object object))
	     (record (make-presentation-record :id pid :printed-p nil)))
	(write-annotation stream #'presentation-start record)
	(multiple-value-prog1
	    (funcall continue)
	  (write-annotation stream #'presentation-end record)))
      (funcall continue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example: Tell openmcl and cmucl to always present unreadable objects. try (describe 'class) 
#+openmcl
(in-package :ccl)
#+openmcl
(let ((*warn-if-redefine-kernel* nil)
      (*warn-if-redefine* nil))
  (defun %print-unreadable-object (object stream type id thunk)
    (cond ((null stream) (setq stream *standard-output*))
	  ((eq stream t) (setq stream *terminal-io*)))
    (swank::presenting-object object stream
      (write-unreadable-start object stream)
      (when type
	(princ (type-of object) stream)
	(stream-write-char stream #\space))
      (when thunk 
	(funcall thunk))
      (if id
	  (%write-address object stream #\>)
	  (pp-end-block stream ">"))
      nil))
  (defmethod print-object :around ((pathname pathname) stream)
    (swank::presenting-object-if
	(swank::can-present-readable-objects stream)
	pathname stream (call-next-method))))

#+openmcl
(ccl::def-load-pointers clear-presentations ()
  (swank::clear-presentation-tables))

(in-package :swank)

#+cmu
(progn
  (fwrappers:define-fwrapper presenting-unreadable-wrapper (object stream type identity body)
    (presenting-object object stream
      (fwrappers:call-next-function)))

  (fwrappers:define-fwrapper presenting-pathname-wrapper (pathname stream depth)
    (presenting-object-if (can-present-readable-objects stream) pathname stream
      (fwrappers:call-next-function)))

  (fwrappers::fwrap 'lisp::%print-pathname  #'presenting-pathname-wrapper)
  (fwrappers::fwrap 'lisp::%print-unreadable-object  #'presenting-unreadable-wrapper)
  )

#+sbcl
(progn 
  (defvar *saved-%print-unreadable-object*
    (fdefinition 'sb-impl::%print-unreadable-object))
  (sb-ext:without-package-locks 
    (setf (fdefinition 'sb-impl::%print-unreadable-object)
	  (lambda (object stream type identity body)
	    (presenting-object object stream
	      (funcall *saved-%print-unreadable-object* 
		       object stream type identity body))))
    (defmethod print-object :around ((object pathname) stream)
      (presenting-object object stream
	(call-next-method)))))

#+allegro
(progn
  (excl:def-fwrapper presenting-unreadable-wrapper (object stream type identity continuation) 
    (swank::presenting-object object stream (excl:call-next-fwrapper)))
  (excl:def-fwrapper presenting-pathname-wrapper (pathname stream depth)
    (presenting-object-if (can-present-readable-objects stream) pathname stream
      (excl:call-next-fwrapper)))
  (excl:fwrap 'excl::print-unreadable-object-1 
	      'print-unreadable-present 'presenting-unreadable-wrapper)
  (excl:fwrap 'excl::pathname-printer 
	      'print-pathname-present 'presenting-pathname-wrapper))
