;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-ecl.lisp --- SLIME backend for ECL.
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

;;; Administrivia

(in-package :swank-backend)

(import-from :ext *gray-stream-symbols* :swank-backend)

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
	  (line)
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

(defclass ecl-inspector (inspector)
  ())

(defimplementation make-default-inspector ()
  (make-instance 'ecl-inspector))

;;;; Definitions

(defimplementation find-definitions (name) nil)
