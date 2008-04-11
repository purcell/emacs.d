;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-ecl.lisp --- SLIME backend for ECL.
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

;;; Administrivia

(in-package :swank-backend)

(if (find-package :gray)
  (import-from :gray *gray-stream-symbols* :swank-backend)
  (import-from :ext *gray-stream-symbols* :swank-backend))

(swank-backend::import-swank-mop-symbols :clos
 '(:eql-specializer
   :eql-specializer-object
   :generic-function-declarations
   :specializer-direct-methods
   :compute-applicable-methods-using-classes))


;;;; TCP Server

(require 'sockets)

(defun resolve-hostname (name)
  (car (sb-bsd-sockets:host-ent-addresses
        (sb-bsd-sockets:get-host-by-name name))))

(defimplementation create-socket (host port)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket (resolve-hostname host) port)
    (sb-bsd-sockets:socket-listen socket 5)
    socket))

(defimplementation local-port (socket)
  (nth-value 1 (sb-bsd-sockets:socket-name socket)))

(defimplementation close-socket (socket)
  (sb-bsd-sockets:socket-close socket))

(defimplementation accept-connection (socket
                                      &key external-format
                                      buffering timeout)
  (declare (ignore buffering timeout external-format))
  (make-socket-io-stream (accept socket)))

(defun make-socket-io-stream (socket)
  (sb-bsd-sockets:socket-make-stream socket
                                     :output t
                                     :input t
                                     :element-type 'base-char))

(defun accept (socket)
  "Like socket-accept, but retry on EAGAIN."
  (loop (handler-case
            (return (sb-bsd-sockets:socket-accept socket))
          (sb-bsd-sockets:interrupted-error ()))))

(defimplementation preferred-communication-style ()
  (values nil))


;;;; Unix signals

(defimplementation getpid ()
  (si:getpid))

#+nil
(defimplementation set-default-directory (directory)
  (ext::chdir (namestring directory))
  ;; Setting *default-pathname-defaults* to an absolute directory
  ;; makes the behavior of MERGE-PATHNAMES a bit more intuitive.
  (setf *default-pathname-defaults* (ext::getcwd))
  (default-directory))

#+nil
(defimplementation default-directory ()
  (namestring (ext:getcwd)))

(defimplementation quit-lisp ()
  (ext:quit))


;;;; Compilation

(defvar *buffer-name* nil)
(defvar *buffer-start-position*)
(defvar *buffer-string*)
(defvar *compile-filename*)

(defun signal-compiler-condition (&rest args)
  (signal (apply #'make-condition 'compiler-condition args)))

(defun handle-compiler-warning (condition)
  (signal-compiler-condition
   :original-condition condition
   :message (format nil "~A" condition)
   :severity :warning
   :location
   (if *buffer-name*
       (make-location (list :buffer *buffer-name*)
                      (list :position *buffer-start-position*))
       ;; ;; compiler::*current-form*
       ;; (if compiler::*current-function*
       ;;     (make-location (list :file *compile-filename*)
       ;;                    (list :function-name   
       ;;                          (symbol-name
       ;;                           (slot-value compiler::*current-function*
       ;;                                       'compiler::name))))
       (list :error "No location found.")
           ;; )
       )))

(defimplementation call-with-compilation-hooks (function)
  (handler-bind ((warning #'handle-compiler-warning))
    (funcall function)))

(defimplementation swank-compile-file (*compile-filename* load-p
                                       external-format)
  (declare (ignore external-format))
  (with-compilation-hooks ()
    (let ((*buffer-name* nil))
      (multiple-value-bind (fn warn fail) 
          (compile-file *compile-filename*)
        (when load-p (unless fail (load fn)))))))

(defimplementation swank-compile-string (string &key buffer position directory)
  (declare (ignore directory))
  (with-compilation-hooks ()
    (let ((*buffer-name* buffer)
          (*buffer-start-position* position)
          (*buffer-string* string))
      (with-input-from-string (s string)
        (compile-from-stream s :load t)))))

(defun compile-from-stream (stream &rest args)
  (let ((file (si::mkstemp "TMP:ECLXXXXXX")))
    (with-open-file (s file :direction :output :if-exists :overwrite)
      (do ((line (read-line stream nil) (read-line stream nil)))
	  ((not line))
	(write-line line s)))
    (unwind-protect
         (apply #'compile-file file args)
      (delete-file file))))


;;;; Documentation

(defimplementation arglist (name)
  (or (functionp name) (setf name (symbol-function name)))
  (if (functionp name)
      (typecase name 
        (generic-function
         (clos::generic-function-lambda-list name))
        (compiled-function
         ; most of the compiled functions have an Args: line in their docs
         (with-input-from-string (s (or
                                     (si::get-documentation
                                      (si:compiled-function-name name) 'function)
                                     ""))
           (do ((line (read-line s nil) (read-line s nil)))
               ((not line) :not-available)
             (ignore-errors
               (if (string= (subseq line 0 6) "Args: ")
                   (return-from nil
                     (read-from-string (subseq line 6))))))))
         ;
        (function
         (let ((fle (function-lambda-expression name)))
           (case (car fle)
             (si:lambda-block (caddr fle))
             (t               :not-available)))))
      :not-available))

(defimplementation function-name (f)
  (si:compiled-function-name f))

(defimplementation macroexpand-all (form)
  ;;; FIXME! This is not the same as a recursive macroexpansion!
  (macroexpand form))

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (dolist (type '(:VARIABLE :FUNCTION :CLASS))
      (let ((doc (describe-definition symbol type)))
        (when doc
          (setf result (list* type doc result)))))
    result))

(defimplementation describe-definition (name type)
  (case type
    (:variable (documentation name 'variable))
    (:function (documentation name 'function))
    (:class (documentation name 'class))
    (t nil)))

;;; Debugging

(import
 '(si::*ihs-top*
   si::*ihs-current*
   si::*ihs-base*
   si::*frs-base*
   si::*frs-top*
   si::*tpl-commands*
   si::*tpl-level*
   si::frs-top
   si::ihs-top
   si::sch-frs-base
   si::set-break-env
   si::set-current-ihs
   si::tpl-commands))

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (declare (type function debugger-loop-fn))
  (let* ((*tpl-commands* si::tpl-commands)
         (*ihs-top* (ihs-top 'call-with-debugging-environment))
	 (*ihs-current* *ihs-top*)
	 (*frs-base* (or (sch-frs-base *frs-top* *ihs-base*) (1+ (frs-top))))
	 (*frs-top* (frs-top))
	 (*read-suppress* nil)
	 (*tpl-level* (1+ *tpl-level*)))
    (set-break-env)
    (set-current-ihs)
    (funcall debugger-loop-fn)))

;; (defimplementation call-with-debugger-hook (hook fun)
;;   (let ((*debugger-hook* hook))
;;     (funcall fun)))

(defun nth-frame (n)
  (cond ((>= n *ihs-top* ) nil)
        (t (- *ihs-top*  n))))
                                               
(defimplementation compute-backtrace (start end)
  (loop for i from start below end
        for f = (nth-frame i)     
        while f
        collect f))

(defimplementation print-frame (frame stream)
  (format stream "~A" (si::ihs-fname frame)))

;;;; Inspector

(defmethod emacs-inspect ((o t))
  ; ecl clos support leaves some to be desired
  (cond
    ((streamp o)
     (list*
      (format nil "~S is an ordinary stream~%" o)
      (append
       (list
        "Open for "
        (cond
          ((ignore-errors (interactive-stream-p o)) "Interactive")
          ((and (input-stream-p o) (output-stream-p o)) "Input and output")
          ((input-stream-p o) "Input")
          ((output-stream-p o) "Output"))
        `(:newline) `(:newline))
       (label-value-line*
        ("Element type" (stream-element-type o))
        ("External format" (stream-external-format o)))
       (ignore-errors (label-value-line*
                       ("Broadcast streams" (broadcast-stream-streams o))))
       (ignore-errors (label-value-line*
                       ("Concatenated streams" (concatenated-stream-streams o))))
       (ignore-errors (label-value-line*
                       ("Echo input stream" (echo-stream-input-stream o))))
       (ignore-errors (label-value-line*
                       ("Echo output stream" (echo-stream-output-stream o))))
       (ignore-errors (label-value-line*
                       ("Output String" (get-output-stream-string o))))
       (ignore-errors (label-value-line*
                       ("Synonym symbol" (synonym-stream-symbol o))))
       (ignore-errors (label-value-line*
                       ("Input stream" (two-way-stream-input-stream o))))
       (ignore-errors (label-value-line*
                       ("Output stream" (two-way-stream-output-stream o)))))))
    (t
     (let* ((cl (si:instance-class o))
            (slots (clos:class-slots cl)))
       (list* (format nil "~S is an instance of class ~A~%"
                       o (clos::class-name cl))
               (loop for x in slots append
                    (let* ((name (clos:slot-definition-name x))
                           (value (clos::slot-value o name)))
                      (list
                       (format nil "~S: " name)
                       `(:value ,value)
                       `(:newline)))))))))

;;;; Definitions

(defimplementation find-definitions (name) nil)

;;;; Threads

#+threads
(progn
  (defvar *thread-id-counter* 0)

  (defvar *thread-id-counter-lock*
    (mp:make-lock :name "thread id counter lock"))

  (defun next-thread-id ()
    (mp:with-lock (*thread-id-counter-lock*)
      (incf *thread-id-counter*)))

  (defparameter *thread-id-map* (make-hash-table))

  (defvar *thread-id-map-lock*
    (mp:make-lock :name "thread id map lock"))

  ; ecl doesn't have weak pointers
  (defimplementation spawn (fn &key name)
    (let ((thread (mp:make-process :name name))
	  (id (next-thread-id)))
      (mp:process-preset
	thread
	#'(lambda ()
	    (unwind-protect
	      (mp:with-lock (*thread-id-map-lock*)
	        (setf (gethash id *thread-id-map*) thread))
	      (funcall fn)
	      (mp:with-lock (*thread-id-map-lock*)
                (remhash id *thread-id-map*)))))
      (mp:process-enable thread)))

  (defimplementation thread-id (thread)
    (block thread-id
      (mp:with-lock (*thread-id-map-lock*)
        (loop for id being the hash-key in *thread-id-map*
              using (hash-value thread-pointer)
              do (if (eq thread thread-pointer)
		   (return-from thread-id id))))))

  (defimplementation find-thread (id)
    (mp:with-lock (*thread-id-map-lock*)
      (gethash id *thread-id-map*)))

  (defimplementation thread-name (thread)
    (mp:process-name thread))

  (defimplementation thread-status (thread)
    (if (mp:process-active-p thread)
        "RUNNING"
        "STOPPED"))

  (defimplementation make-lock (&key name)
    (mp:make-lock :name name))

  (defimplementation call-with-lock-held (lock function)
    (declare (type function function))
    (mp:with-lock (lock) (funcall function)))

  (defimplementation make-recursive-lock (&key name)
    (mp:make-lock :name name))

  (defimplementation call-with-recursive-lock-held (lock function)
    (declare (type function function))
    (mp:with-lock (lock) (funcall function)))

  (defimplementation current-thread ()
    mp:*current-process*)

  (defimplementation all-threads ()
    (mp:all-processes))

  (defimplementation interrupt-thread (thread fn)
    (mp:interrupt-process thread fn))

  (defimplementation kill-thread (thread)
    (mp:process-kill thread))

  (defimplementation thread-alive-p (thread)
    (mp:process-active-p thread))

  (defvar *mailbox-lock* (mp:make-lock :name "mailbox lock"))

  (defstruct (mailbox (:conc-name mailbox.))
    (mutex (mp:make-lock :name "process mailbox"))
    (queue '() :type list))

  (defun mailbox (thread)
    "Return THREAD's mailbox."
    (mp:with-lock (*mailbox-lock*)
      (or (find thread *mailboxes* :key #'mailbox.thread)
          (let ((mb (make-mailbox :thread thread)))
            (push mb *mailboxes*)
            mb))))

  (defimplementation send (thread message)
    (let* ((mbox (mailbox thread))
           (mutex (mailbox.mutex mbox)))
      (mp:interrupt-process
	thread
	(lambda ()
	  (mp:with-lock (mutex)
            (setf (mailbox.queue mbox)
              (nconc (mailbox.queue mbox) (list message))))))))

  (defimplementation receive ()
    (block got-mail
      (let* ((mbox (mailbox mp:*current-process*))
             (mutex (mailbox.mutex mbox)))
        (loop
	  (mp:with-lock (mutex)
            (if (mailbox.queue mbox)
	      (return-from got-mail (pop (mailbox.queue mbox)))))
          ;interrupt-process will halt this if it takes longer than 1sec
          (sleep 1)))))

  ;; Auto-flush streams
  (defvar *auto-flush-interval* 0.15
    "How often to flush interactive streams. This valu is passed
    directly to cl:sleep.")

  (defvar *auto-flush-lock* (make-recursive-lock :name "auto flush"))

  (defvar *auto-flush-thread* nil)

  (defvar *auto-flush-streams* '())

  (defimplementation make-stream-interactive (stream)
    (call-with-recursive-lock-held
     *auto-flush-lock*
     (lambda ()
       (pushnew stream *auto-flush-streams*)
       (unless *auto-flush-thread*
         (setq *auto-flush-thread*
               (spawn #'flush-streams
		      :name "auto-flush-thread"))))))

  (defmethod stream-finish-output ((stream stream))
    (finish-output stream))

  (defun flush-streams ()
    (loop
     (call-with-recursive-lock-held
      *auto-flush-lock*
      (lambda ()
        (setq *auto-flush-streams*
              (remove-if (lambda (x)
                           (not (and (open-stream-p x)
                                     (output-stream-p x))))
                         *auto-flush-streams*))
        (mapc #'stream-finish-output *auto-flush-streams*)))
     (sleep *auto-flush-interval*)))

  )

