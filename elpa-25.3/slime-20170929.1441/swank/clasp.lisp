;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-clasp.lisp --- SLIME backend for CLASP.
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

;;; Administrivia

(defpackage swank/clasp
  (:use cl swank/backend))

(in-package swank/clasp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq swank::*log-output* (open "/tmp/slime.log" :direction :output))
  (setq swank:*log-events* t))

(defmacro slime-dbg (fmt &rest args)
  `(swank::log-event "slime-dbg ~a ~a~%" mp:*current-process* (apply #'format nil ,fmt ,args)))

;; Hard dependencies.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sockets))

;; Soft dependencies.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (probe-file "sys:profile.fas")
    (require :profile)
    (pushnew :profile *features*))
  (when (probe-file "sys:serve-event")
    (require :serve-event)
    (pushnew :serve-event *features*)))

(declaim (optimize (debug 3)))

;;; Swank-mop

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import-swank-mop-symbols
   :clos
   nil
   #+(or)`(:eql-specializer
           :eql-specializer-object
           :generic-function-declarations
           :specializer-direct-methods
           ,@(unless (fboundp 'clos:compute-applicable-methods-using-classes)
               '(:compute-applicable-methods-using-classes)))))

(defimplementation gray-package-name ()
  "GRAY")


;;;; TCP Server

(defimplementation preferred-communication-style ()
  ;; As of March 2017 CLASP provides threads.
  ;; But it's experimental.
  ;; ECLs swank implementation says that CLOS is not thread safe and
  ;; I use ECLs CLOS implementation - this is a worry for the future.
  ;; nil or  :spawn
;;  nil
  :spawn
#|  #+threads :spawn
  #-threads nil
|#
  )

(defun resolve-hostname (name)
  (car (sb-bsd-sockets:host-ent-addresses
        (sb-bsd-sockets:get-host-by-name name))))

(defimplementation create-socket (host port &key backlog)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket (resolve-hostname host) port)
    (sb-bsd-sockets:socket-listen socket (or backlog 5))
    socket))

(defimplementation local-port (socket)
  (nth-value 1 (sb-bsd-sockets:socket-name socket)))

(defimplementation close-socket (socket)
  (sb-bsd-sockets:socket-close socket))

(defimplementation accept-connection (socket
                                      &key external-format
                                      buffering timeout)
  (declare (ignore timeout))
  (sb-bsd-sockets:socket-make-stream (accept socket)
                                     :output t
                                     :input t
                                     :buffering (ecase buffering
                                                  ((t) :full)
                                                  ((nil) :none)
                                                  (:line :line))
                                     :element-type (if external-format
                                                       'character 
                                                       '(unsigned-byte 8))
                                     :external-format external-format))
(defun accept (socket)
  "Like socket-accept, but retry on EAGAIN."
  (loop (handler-case
            (return (sb-bsd-sockets:socket-accept socket))
          (sb-bsd-sockets:interrupted-error ()))))

(defimplementation socket-fd (socket)
  (etypecase socket
    (fixnum socket)
    (two-way-stream (socket-fd (two-way-stream-input-stream socket)))
    (sb-bsd-sockets:socket (sb-bsd-sockets:socket-file-descriptor socket))
    (file-stream (si:file-stream-fd socket))))

(defvar *external-format-to-coding-system*
  '((:latin-1
     "latin-1" "latin-1-unix" "iso-latin-1-unix" 
     "iso-8859-1" "iso-8859-1-unix")
    (:utf-8 "utf-8" "utf-8-unix")))

(defun external-format (coding-system)
  (or (car (rassoc-if (lambda (x) (member coding-system x :test #'equal))
                      *external-format-to-coding-system*))
      (find coding-system (ext:all-encodings) :test #'string-equal)))

(defimplementation find-external-format (coding-system)
  #+unicode (external-format coding-system)
  ;; Without unicode support, CLASP uses the one-byte encoding of the
  ;; underlying OS, and will barf on anything except :DEFAULT.  We
  ;; return NIL here for known multibyte encodings, so
  ;; SWANK:CREATE-SERVER will barf.
  #-unicode (let ((xf (external-format coding-system)))
              (if (member xf '(:utf-8))
                  nil
                  :default)))


;;;; Unix Integration

;;; If CLASP is built with thread support, it'll spawn a helper thread
;;; executing the SIGINT handler. We do not want to BREAK into that
;;; helper but into the main thread, though. This is coupled with the
;;; current choice of NIL as communication-style in so far as CLASP's
;;; main-thread is also the Slime's REPL thread.

#+clasp-working
(defimplementation call-with-user-break-handler (real-handler function)
  (let ((old-handler #'si:terminal-interrupt))
    (setf (symbol-function 'si:terminal-interrupt)
          (make-interrupt-handler real-handler))
    (unwind-protect (funcall function)
      (setf (symbol-function 'si:terminal-interrupt) old-handler))))

#+threads
(defun make-interrupt-handler (real-handler)
  (let ((main-thread (find 'si:top-level (mp:all-processes)
                           :key #'mp:process-name)))
    #'(lambda (&rest args)
        (declare (ignore args))
        (mp:interrupt-process main-thread real-handler))))

#-threads
(defun make-interrupt-handler (real-handler)
  #'(lambda (&rest args)
      (declare (ignore args))
      (funcall real-handler)))


(defimplementation getpid ()
  (si:getpid))

(defimplementation set-default-directory (directory)
  (ext:chdir (namestring directory))  ; adapts *DEFAULT-PATHNAME-DEFAULTS*.
  (default-directory))

(defimplementation default-directory ()
  (namestring (ext:getcwd)))

(defimplementation quit-lisp ()
  (core:quit))



;;; Instead of busy waiting with communication-style NIL, use select()
;;; on the sockets' streams.
#+serve-event
(progn
  (defun poll-streams (streams timeout)
    (let* ((serve-event::*descriptor-handlers*
            (copy-list serve-event::*descriptor-handlers*))
           (active-fds '())
           (fd-stream-alist
            (loop for s in streams
                  for fd = (socket-fd s)
                  collect (cons fd s)
                  do (serve-event:add-fd-handler fd :input
                                                 #'(lambda (fd)
                                                     (push fd active-fds))))))
      (serve-event:serve-event timeout)
      (loop for fd in active-fds collect (cdr (assoc fd fd-stream-alist)))))

  (defimplementation wait-for-input (streams &optional timeout)
    (assert (member timeout '(nil t)))
    (loop
       (cond ((check-slime-interrupts) (return :interrupt))
             (timeout (return (poll-streams streams 0)))
             (t
              (when-let (ready (poll-streams streams 0.2))
                        (return ready))))))  

) ; #+serve-event (progn ...

#-serve-event
(defimplementation wait-for-input (streams &optional timeout)
  (assert (member timeout '(nil t)))
  (loop
   (cond ((check-slime-interrupts) (return :interrupt))
         (timeout (return (remove-if-not #'listen streams)))
         (t
          (let ((ready (remove-if-not #'listen streams)))
            (if ready (return ready))
            (sleep 0.1))))))


;;;; Compilation

(defvar *buffer-name* nil)
(defvar *buffer-start-position*)

(defun signal-compiler-condition (&rest args)
  (apply #'signal 'compiler-condition args))

#-clasp-bytecmp
(defun handle-compiler-message (condition)
  ;; CLASP emits lots of noise in compiler-notes, like "Invoking
  ;; external command".
  (unless (typep condition 'c::compiler-note)
    (signal-compiler-condition
     :original-condition condition
     :message (princ-to-string condition)
     :severity (etypecase condition
                 (cmp:compiler-fatal-error :error)
                 (cmp:compiler-error       :error)
                 (error                  :error)
                 (style-warning          :style-warning)
                 (warning                :warning))
     :location (condition-location condition))))

#-clasp-bytecmp
(defun condition-location (condition)
  (let ((file     (cmp:compiler-message-file condition))
        (position (cmp:compiler-message-file-position condition)))
    (if (and position (not (minusp position)))
        (if *buffer-name*
            (make-buffer-location *buffer-name*
                                  *buffer-start-position*
                                  position)
            (make-file-location file position))
        (make-error-location "No location found."))))

(defimplementation call-with-compilation-hooks (function)
  (funcall function))
#||  #-clasp-bytecmp
  (handler-bind ((c:compiler-message #'handle-compiler-message))
    (funcall function)))
||#


(defimplementation swank-compile-file (input-file output-file
                                       load-p external-format
                                       &key policy)
  (declare (ignore policy))
  (format t "Compiling file input-file = ~a   output-file = ~a~%" input-file output-file)
  ;; Ignore the output-file and generate our own
  (let ((tmp-output-file (compile-file-pathname (si:mkstemp "TMP:clasp-swank-compile-file-"))))
    (format t "Using tmp-output-file: ~a~%" tmp-output-file)
    (multiple-value-bind (fasl warnings-p failure-p)
        (with-compilation-hooks ()
          (compile-file input-file :output-file tmp-output-file
                        :external-format external-format))
      (values fasl warnings-p
              (or failure-p
                  (when load-p
                    (not (load fasl))))))))

(defvar *tmpfile-map* (make-hash-table :test #'equal))

(defun note-buffer-tmpfile (tmp-file buffer-name)
  ;; EXT:COMPILED-FUNCTION-FILE below will return a namestring.
  (let ((tmp-namestring (namestring (truename tmp-file))))
    (setf (gethash tmp-namestring *tmpfile-map*) buffer-name)
    tmp-namestring))

(defun tmpfile-to-buffer (tmp-file)
  (gethash tmp-file *tmpfile-map*))

(defimplementation swank-compile-string (string &key buffer position filename policy)
  (declare (ignore policy))
  (with-compilation-hooks ()
    (let ((*buffer-name* buffer)        ; for compilation hooks
          (*buffer-start-position* position))
      (let ((tmp-file (si:mkstemp "TMP:clasp-swank-tmpfile-"))
            (fasl-file)
            (warnings-p)
            (failure-p))
        (unwind-protect
             (with-open-file (tmp-stream tmp-file :direction :output
                                                  :if-exists :supersede)
               (write-string string tmp-stream)
               (finish-output tmp-stream)
               (multiple-value-setq (fasl-file warnings-p failure-p)
                 (let ((truename (or filename (note-buffer-tmpfile tmp-file buffer))))
                   (compile-file tmp-file
                                 :source-debug-namestring truename
                                 :source-debug-offset (1- position)))))
          (when fasl-file (load fasl-file))
          (when (probe-file tmp-file)
            (delete-file tmp-file))
          (when fasl-file
            (delete-file fasl-file)))
        (not failure-p)))))

;;;; Documentation

(defimplementation arglist (name)
  (multiple-value-bind (arglist foundp)
      (core:function-lambda-list name)     ;; Uses bc-split
    (if foundp arglist :not-available)))

(defimplementation function-name (f)
  (typecase f
    (generic-function (clos::generic-function-name f))
    (function (ext:compiled-function-name f))))

;; FIXME
(defimplementation macroexpand-all (form &optional env)
  (declare (ignore env))
  (macroexpand form))

;;; modified from sbcl.lisp
(defimplementation collect-macro-forms (form &optional environment)
  (let ((macro-forms '())
        (compiler-macro-forms '())
        (function-quoted-forms '()))
    (format t "In collect-macro-forms~%")
    (cmp:code-walk
     form environment
     :code-walker-function
     (lambda (form environment)
       (when (and (consp form)
                  (symbolp (car form)))
         (cond ((eq (car form) 'function)
                (push (cadr form) function-quoted-forms))
               ((member form function-quoted-forms)
                nil)
               ((macro-function (car form) environment)
                (push form macro-forms))
               ((not (eq form (core:compiler-macroexpand-1 form environment)))
                (push form compiler-macro-forms))))
       form))
    (values macro-forms compiler-macro-forms)))





(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (flet ((frob (type boundp)
             (when (funcall boundp symbol)
               (let ((doc (describe-definition symbol type)))
                 (setf result (list* type doc result))))))
      (frob :VARIABLE #'boundp)
      (frob :FUNCTION #'fboundp)
      (frob :CLASS (lambda (x) (find-class x nil))))
    result))

(defimplementation describe-definition (name type)
  (case type
    (:variable (documentation name 'variable))
    (:function (documentation name 'function))
    (:class (documentation name 'class))
    (t nil)))

(defimplementation type-specifier-p (symbol)
  (or (subtypep nil symbol)
      (not (eq (type-specifier-arglist symbol) :not-available))))


;;; Debugging

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(si::*break-env*
     si::*ihs-top*
     si::*ihs-current*
     si::*ihs-base*
#+frs     si::*frs-base*
#+frs     si::*frs-top*
     si::*tpl-commands*
     si::*tpl-level*
#+frs     si::frs-top
     si::ihs-top
     si::ihs-fun
     si::ihs-env
#+frs     si::sch-frs-base
     si::set-break-env
     si::set-current-ihs
     si::tpl-commands)))

(defun make-invoke-debugger-hook (hook)
  (when hook
    #'(lambda (condition old-hook)
        ;; Regard *debugger-hook* if set by user.
        (if *debugger-hook*
            nil         ; decline, *DEBUGGER-HOOK* will be tried next.
            (funcall hook condition old-hook)))))

(defimplementation install-debugger-globally (function)
  (setq *debugger-hook* function)
  (setq ext:*invoke-debugger-hook* (make-invoke-debugger-hook function))
  )

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook)
        (ext:*invoke-debugger-hook* (make-invoke-debugger-hook hook)))
    (funcall fun))
  )

(defvar *backtrace* '())

;;; Commented out; it's not clear this is a good way of doing it. In
;;; particular because it makes errors stemming from this file harder
;;; to debug, and given the "young" age of CLASP's swank backend, that's
;;; a bad idea.

;; (defun in-swank-package-p (x)
;;   (and
;;    (symbolp x)
;;    (member (symbol-package x)
;;            (list #.(find-package :swank)
;;                  #.(find-package :swank/backend)
;;                  #.(ignore-errors (find-package :swank-mop))
;;                  #.(ignore-errors (find-package :swank-loader))))
;;    t))

;; (defun is-swank-source-p (name)
;;   (setf name (pathname name))
;;   (pathname-match-p
;;    name
;;    (make-pathname :defaults swank-loader::*source-directory*
;;                   :name (pathname-name name)
;;                   :type (pathname-type name)
;;                   :version (pathname-version name))))

;; (defun is-ignorable-fun-p (x)
;;   (or
;;    (in-swank-package-p (frame-name x))
;;    (multiple-value-bind (file position)
;;        (ignore-errors (si::bc-file (car x)))
;;      (declare (ignore position))
;;      (if file (is-swank-source-p file)))))

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (declare (type function debugger-loop-fn))
  (let* ((*ihs-top* 0)
         (*ihs-current* *ihs-top*)
#+frs         (*frs-base* (or (sch-frs-base *frs-top* *ihs-base*) (1+ (frs-top))))
#+frs         (*frs-top* (frs-top))
         (*tpl-level* (1+ *tpl-level*))
         (*backtrace* (core::common-lisp-backtrace-frames)))
    (declare (special *ihs-current*))
#+frs    (loop for f from *frs-base* until *frs-top*
          do (let ((i (- (si::frs-ihs f) *ihs-base* 1)))
               (when (plusp i)
                 (let* ((x (elt *backtrace* i))
                        (name (si::frs-tag f)))
                   (unless (si::fixnump name)
                     (push name (third x)))))))
    (set-break-env)
    (set-current-ihs)
    (let ((*ihs-base* *ihs-top*))
      (funcall debugger-loop-fn))))

(defimplementation compute-backtrace (start end)
  (subseq *backtrace* start
          (and (numberp end)
               (min end (length *backtrace*)))))

(defun frame-name (frame)
  (let ((x (core::backtrace-frame-function-name frame)))
    (if (symbolp x)
      x
      (function-name x))))

(defun frame-function (frame-number)
  (let ((x (core::backtrace-frame-function-name (elt *backtrace* frame-number))))
    (etypecase x
      (symbol
       (and (fboundp x)
            (fdefinition x)))
      (function
       x))))

(defimplementation print-frame (frame stream)
  (if (core::backtrace-frame-arguments frame)
      (format stream "(~a~{ ~s~})" (core::backtrace-frame-print-name frame)
              (coerce (core::backtrace-frame-arguments frame) 'list))
      (format stream "~a" (core::backtrace-frame-print-name frame))))

(defimplementation frame-source-location (frame-number)
  (source-location (frame-function frame-number)))

#+clasp-working
(defimplementation frame-catch-tags (frame-number)
  (third (elt *backtrace* frame-number)))

(defun ihs-frame-id (frame-number)
  (- (core:ihs-top) frame-number))

(defimplementation frame-locals (frame-number)
  (let* ((frame (elt *backtrace* frame-number))
         (env nil) ; no env yet
         (locals (loop for x = env then (core:get-parent-environment x)
                       while x
                       nconc (loop for name across (core:environment-debug-names x)
                                   for value across (core:environment-debug-values x)
                                   collect (list :name name :id 0 :value value)))))
    (nconc
     (loop for arg across (core::backtrace-frame-arguments frame)
           for i from 0
           collect (list :name (intern (format nil "ARG~d" i) #.*package*)
                         :id 0
                         :value arg))
     locals)))

(defimplementation frame-var-value (frame-number var-number)
  (let* ((frame (elt *backtrace* frame-number))
         (env nil)
         (args (core::backtrace-frame-arguments frame)))
    (if (< var-number (length args))
        (svref args var-number)
        (elt (frame-locals frame-number) var-number))))

(defimplementation disassemble-frame (frame-number)
  (let ((fun (frame-function frame-number)))
    (disassemble fun)))

(defimplementation eval-in-frame (form frame-number)
  (let ((env nil)) ; (second (elt *backtrace* frame-number))))
    (core:compile-form-and-eval-with-env form env)))

#+clasp-working
(defimplementation gdb-initial-commands ()
  ;; These signals are used by the GC.
  #+linux '("handle SIGPWR  noprint nostop"
            "handle SIGXCPU noprint nostop"))

#+clasp-working
(defimplementation command-line-args ()
  (loop for n from 0 below (si:argc) collect (si:argv n)))


;;;; Inspector

;;; FIXME: Would be nice if it was possible to inspect objects
;;; implemented in C.


;;;; Definitions

(defun make-file-location (file file-position)
  ;; File positions in CL start at 0, but Emacs' buffer positions
  ;; start at 1. We specify (:ALIGN T) because the positions comming
  ;; from CLASP point at right after the toplevel form appearing before
  ;; the actual target toplevel form; (:ALIGN T) will DTRT in that case.
  (make-location `(:file ,(namestring (translate-logical-pathname file)))
                 `(:position ,(1+ file-position))
                 `(:align t)))

(defun make-buffer-location (buffer-name start-position &optional (offset 0))
  (make-location `(:buffer ,buffer-name)
                 `(:offset ,start-position ,offset)
                 `(:align t)))

(defun translate-location (location)
  (make-location (list :file (namestring (ext:source-location-pathname location)))
                 (list :position (ext:source-location-offset location))
                 '(:align t)))

(defimplementation find-definitions (name)
  (loop for kind in ext:*source-location-kinds*
        for locations = (ext:source-location name kind)
        when locations
        nconc (loop for location in locations
                    collect (list kind (translate-location location)))))

(defun source-location (object)
  (let ((location (ext:source-location object t)))
    (when location
      (translate-location (car location)))))

(defimplementation find-source-location (object)
  (or (source-location object)
      (make-error-location "Source definition of ~S not found." object)))


;;;; Profiling

#+profile
(progn

(defimplementation profile (fname)
  (when fname (eval `(profile:profile ,fname))))

(defimplementation unprofile (fname)
  (when fname (eval `(profile:unprofile ,fname))))

(defimplementation unprofile-all ()
  (profile:unprofile-all)
  "All functions unprofiled.")

(defimplementation profile-report ()
  (profile:report))

(defimplementation profile-reset ()
  (profile:reset)
  "Reset profiling counters.")

(defimplementation profiled-functions ()
  (profile:profile))

(defimplementation profile-package (package callers methods)
  (declare (ignore callers methods))
  (eval `(profile:profile ,(package-name (find-package package)))))
) ; #+profile (progn ...


;;;; Threads

#+threads
(progn
  (defvar *thread-id-counter* 0)

  (defparameter *thread-id-map* (make-hash-table))

  (defvar *thread-id-map-lock*
    (mp:make-lock :name "thread id map lock"))

  (defimplementation spawn (fn &key name)
    (mp:process-run-function name fn))

  (defimplementation thread-id (target-thread)
    (block thread-id
      (mp:with-lock (*thread-id-map-lock*)
        ;; Does TARGET-THREAD have an id already?
        (maphash (lambda (id thread-pointer)
                   (let ((thread (si:weak-pointer-value thread-pointer)))
                     (cond ((not thread)
                            (remhash id *thread-id-map*))
                           ((eq thread target-thread)
                            (return-from thread-id id)))))
                 *thread-id-map*)
        ;; TARGET-THREAD not found in *THREAD-ID-MAP*
        (let ((id (incf *thread-id-counter*))
              (thread-pointer (si:make-weak-pointer target-thread)))
          (setf (gethash id *thread-id-map*) thread-pointer)
          id))))

  (defimplementation find-thread (id)
    (mp:with-lock (*thread-id-map-lock*)
      (let* ((thread-ptr (gethash id *thread-id-map*))
             (thread (and thread-ptr (si:weak-pointer-value thread-ptr))))
        (unless thread
          (remhash id *thread-id-map*))
        thread)))

  (defimplementation thread-name (thread)
    (mp:process-name thread))

  (defimplementation thread-status (thread)
    (if (mp:process-active-p thread)
        "RUNNING"
        "STOPPED"))

  (defimplementation make-lock (&key name)
    (mp:make-lock :name name :recursive t))

  (defimplementation call-with-lock-held (lock function)
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
  (defvar *mailboxes* (list))
  (declaim (type list *mailboxes*))

  (defstruct (mailbox (:conc-name mailbox.))
    thread
    (mutex (mp:make-lock))
    (cvar  (mp:make-condition-variable))
    (queue '() :type list))

  (defun mailbox (thread)
    "Return THREAD's mailbox."
    (mp:with-lock (*mailbox-lock*)
      (or (find thread *mailboxes* :key #'mailbox.thread)
          (let ((mb (make-mailbox :thread thread)))
            (push mb *mailboxes*)
            mb))))

  (defimplementation wake-thread (thread)
    (let* ((mbox (mailbox thread))
           (mutex (mailbox.mutex mbox)))
      (format t "About to with-lock in wake-thread~%")
      (mp:with-lock (mutex)
        (format t "In wake-thread~%")
        (mp:condition-variable-broadcast (mailbox.cvar mbox)))))
  
  (defimplementation send (thread message)
    (let* ((mbox (mailbox thread))
           (mutex (mailbox.mutex mbox)))
      (swank::log-event "clasp.lisp: send message ~a    mutex: ~a~%" message mutex)
      (swank::log-event "clasp.lisp:    (lock-owner mutex) -> ~a~%" (mp:lock-owner mutex))
      (swank::log-event "clasp.lisp:    (lock-count mutex) -> ~a~%" (mp:lock-count mutex))
      (mp:with-lock (mutex)
        (swank::log-event "clasp.lisp:  in with-lock   (lock-owner mutex) -> ~a~%" (mp:lock-owner mutex))
        (swank::log-event "clasp.lisp:  in with-lock   (lock-count mutex) -> ~a~%" (mp:lock-count mutex))
        (setf (mailbox.queue mbox)
              (nconc (mailbox.queue mbox) (list message)))
        (swank::log-event "clasp.lisp: send about to broadcast~%")
        (mp:condition-variable-broadcast (mailbox.cvar mbox)))))

  
  (defimplementation receive-if (test &optional timeout)
    (slime-dbg "Entered receive-if")
    (let* ((mbox (mailbox (current-thread)))
           (mutex (mailbox.mutex mbox)))
      (slime-dbg "receive-if assert")
      (assert (or (not timeout) (eq timeout t)))
      (loop
         (slime-dbg "receive-if check-slime-interrupts")
         (check-slime-interrupts)
         (slime-dbg "receive-if with-lock")
         (mp:with-lock (mutex)
           (let* ((q (mailbox.queue mbox))
                  (tail (member-if test q)))
             (when tail
               (setf (mailbox.queue mbox) (nconc (ldiff q tail) (cdr tail)))
               (return (car tail))))
           (slime-dbg "receive-if when (eq")
           (when (eq timeout t) (return (values nil t))) 
           (slime-dbg "receive-if condition-variable-timedwait")
           (mp:condition-variable-wait (mailbox.cvar mbox) mutex) ; timedwait 0.2
           (slime-dbg "came out of condition-variable-timedwait")
           (core:check-pending-interrupts)))))

  ) ; #+threads (progn ...


(defmethod emacs-inspect ((object core:cxx-object))
  (let ((encoded (core:encode object)))
    (loop for (key . value) in encoded
       append (list (string key) ": " (list :value value) (list :newline)))))
