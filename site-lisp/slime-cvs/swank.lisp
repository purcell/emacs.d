;;; -*- outline-regexp:";;;;;*" indent-tabs-mode:nil coding:latin-1-unix -*-
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;;; swank.lisp
;;;
;;; This file defines the "Swank" TCP server for Emacs to talk to. The
;;; code in this file is purely portable Common Lisp. We do require a
;;; smattering of non-portable functions in order to write the server,
;;; so we have defined them in `swank-backend.lisp' and implemented
;;; them separately for each Lisp implementation. These extensions are
;;; available to us here via the `SWANK-BACKEND' package.

(defpackage :swank
  (:use :common-lisp :swank-backend)
  (:export #:startup-multiprocessing
           #:start-server 
           #:create-server
           #:ed-in-emacs
           #:inspect-in-emacs
           #:print-indentation-lossage
           #:swank-debugger-hook
           #:run-after-init-hook
           #:inspect-for-emacs
           #:inspect-slot-for-emacs
           ;; These are user-configurable variables:
           #:*communication-style*
           #:*dont-close*
           #:*log-events*
           #:*log-output*
           #:*use-dedicated-output-stream*
           #:*dedicated-output-stream-port*
           #:*configure-emacs-indentation*
           #:*readtable-alist*
           #:*globally-redirect-io*
           #:*global-debugger*
           #:*sldb-printer-bindings*
           #:*swank-pprint-bindings*
           #:*default-worker-thread-bindings*
           #:*macroexpand-printer-bindings*
           #:*record-repl-results*
           #:*inspector-dwim-lookup-hooks*
           ;; These are re-exported directly from the backend:
           #:buffer-first-change
           #:frame-source-location-for-emacs
           #:restart-frame
           #:sldb-step
           #:sldb-break
           #:sldb-break-on-return
           #:profiled-functions
           #:profile-report
           #:profile-reset
           #:unprofile-all
           #:profile-package
           #:default-directory
           #:set-default-directory
           #:quit-lisp))

(in-package :swank)


;;;; Top-level variables, constants, macros

(defconstant cl-package (find-package :cl)
  "The COMMON-LISP package.")

(defconstant keyword-package (find-package :keyword)
  "The KEYWORD package.")

(defvar *canonical-package-nicknames*
  `((:common-lisp-user . :cl-user))
  "Canonical package names to use instead of shortest name/nickname.")

(defvar *auto-abbreviate-dotted-packages* t
  "Abbreviate dotted package names to their last component if T.")

(defvar *swank-io-package*
  (let ((package (make-package :swank-io-package :use '())))
    (import '(nil t quote) package)
    package))

(defconstant default-server-port 4005
  "The default TCP port for the server (when started manually).")

(defvar *swank-debug-p* t
  "When true, print extra debugging information.")

(defvar *redirect-io* t
  "When non-nil redirect Lisp standard I/O to Emacs.
Redirection is done while Lisp is processing a request for Emacs.")

(defvar *sldb-printer-bindings*
  `((*print-pretty*           . t)
    (*print-level*            . 4)
    (*print-length*           . 10)
    (*print-circle*           . t)
    (*print-readably*         . nil)
    (*print-pprint-dispatch*  . ,(copy-pprint-dispatch nil))
    (*print-gensym*           . t)
    (*print-base*             . 10)
    (*print-radix*            . nil)
    (*print-array*            . t)
    (*print-lines*            . 10)
    (*print-escape*           . t)
    (*print-right-margin*     . 70))
  "A set of printer variables used in the debugger.")

(defvar *default-worker-thread-bindings* '()
  "An alist to initialize dynamic variables in worker threads.  
The list has the form ((VAR . VALUE) ...).  Each variable VAR will be
bound to the corresponding VALUE.")

(defun call-with-bindings (alist fun)
  "Call FUN with variables bound according to ALIST.
ALIST is a list of the form ((VAR . VAL) ...)."
  (let* ((rlist (reverse alist))
         (vars (mapcar #'car rlist))
         (vals (mapcar #'cdr rlist)))
    (progv vars vals
      (funcall fun))))

(defmacro with-bindings (alist &body body)
  "See `call-with-bindings'."
  `(call-with-bindings ,alist (lambda () ,@body)))

;;; The `DEFSLIMEFUN' macro defines a function that Emacs can call via
;;; RPC.

(defmacro defslimefun (name arglist &body rest)
  "A DEFUN for functions that Emacs can call by RPC."
  `(progn
     (defun ,name ,arglist ,@rest)
     ;; see <http://www.franz.com/support/documentation/6.2/doc/pages/variables/compiler/s_cltl1-compile-file-toplevel-compatibility-p_s.htm>
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name :swank))))

(defun missing-arg ()
  "A function that the compiler knows will never to return a value.
You can use (MISSING-ARG) as the initform for defstruct slots that
must always be supplied. This way the :TYPE slot option need not
include some arbitrary initial value like NIL."
  (error "A required &KEY or &OPTIONAL argument was not supplied."))


;;;; Hooks
;;;
;;; We use Emacs-like `add-hook' and `run-hook' utilities to support
;;; simple indirection. The interface is more CLish than the Emacs
;;; Lisp one.

(defmacro add-hook (place function)
  "Add FUNCTION to the list of values on PLACE."
  `(pushnew ,function ,place))

(defun run-hook (functions &rest arguments)
  "Call each of FUNCTIONS with ARGUMENTS."
  (dolist (function functions)
    (apply function arguments)))

(defvar *new-connection-hook* '()
  "This hook is run each time a connection is established.
The connection structure is given as the argument.
Backend code should treat the connection structure as opaque.")

(defvar *connection-closed-hook* '()
  "This hook is run when a connection is closed.
The connection as passed as an argument.
Backend code should treat the connection structure as opaque.")

(defvar *pre-reply-hook* '()
  "Hook run (without arguments) immediately before replying to an RPC.")

(defvar *after-init-hook* '()
  "Hook run after user init files are loaded.")

(defun run-after-init-hook ()
  (run-hook *after-init-hook*))


;;;; Connections
;;;
;;; Connection structures represent the network connections between
;;; Emacs and Lisp. Each has a socket stream, a set of user I/O
;;; streams that redirect to Emacs, and optionally a second socket
;;; used solely to pipe user-output to Emacs (an optimization).
;;;

(defstruct (connection
             (:conc-name connection.)
             (:print-function print-connection))
  ;; Raw I/O stream of socket connection.
  (socket-io        (missing-arg) :type stream :read-only t)
  ;; Optional dedicated output socket (backending `user-output' slot).
  ;; Has a slot so that it can be closed with the connection.
  (dedicated-output nil :type (or stream null))
  ;; Streams that can be used for user interaction, with requests
  ;; redirected to Emacs.
  (user-input       nil :type (or stream null))
  (user-output      nil :type (or stream null))
  (user-io          nil :type (or stream null))
  ;; A stream where we send REPL results.
  (repl-results     nil :type (or stream null))
  ;; In multithreaded systems we delegate certain tasks to specific
  ;; threads. The `reader-thread' is responsible for reading network
  ;; requests from Emacs and sending them to the `control-thread'; the
  ;; `control-thread' is responsible for dispatching requests to the
  ;; threads that should handle them; the `repl-thread' is the one
  ;; that evaluates REPL expressions. The control thread dispatches
  ;; all REPL evaluations to the REPL thread and for other requests it
  ;; spawns new threads.
  reader-thread
  control-thread
  repl-thread
  ;; Callback functions:
  ;; (SERVE-REQUESTS <this-connection>) serves all pending requests
  ;; from Emacs.
  (serve-requests   (missing-arg) :type function)
  ;; (READ) is called to read and return one message from Emacs.
  (read             (missing-arg) :type function)
  ;; (SEND OBJECT) is called to send one message to Emacs.
  (send             (missing-arg) :type function)
  ;; (CLEANUP <this-connection>) is called when the connection is
  ;; closed.
  (cleanup          nil :type (or null function))
  ;; Cache of macro-indentation information that has been sent to Emacs.
  ;; This is used for preparing deltas to update Emacs's knowledge.
  ;; Maps: symbol -> indentation-specification
  (indentation-cache (make-hash-table :test 'eq) :type hash-table)
  ;; The list of packages represented in the cache:
  (indentation-cache-packages '())
  ;; The communication style used.
  (communication-style nil :type (member nil :spawn :sigio :fd-handler))
  ;; The coding system for network streams.
  (coding-system ))

(defun print-connection (conn stream depth)
  (declare (ignore depth))
  (print-unreadable-object (conn stream :type t :identity t)))

(defvar *connections* '()
  "List of all active connections, with the most recent at the front.")

(defvar *emacs-connection* nil
  "The connection to Emacs currently in use.")

(defvar *swank-state-stack* '()
  "A list of symbols describing the current state.  Used for debugging
and to detect situations where interrupts can be ignored.")

(defun default-connection ()
  "Return the 'default' Emacs connection.
This connection can be used to talk with Emacs when no specific
connection is in use, i.e. *EMACS-CONNECTION* is NIL.

The default connection is defined (quite arbitrarily) as the most
recently established one."
  (first *connections*))

(defslimefun state-stack ()
  "Return the value of *SWANK-STATE-STACK*."
  *swank-state-stack*)

;; A conditions to include backtrace information
(define-condition swank-error (error) 
  ((condition :initarg :condition :reader swank-error.condition)
   (backtrace :initarg :backtrace :reader swank-error.backtrace))
  (:report (lambda (condition stream)
             (princ (swank-error.condition condition) stream))))

(defun make-swank-error (condition)
  (let ((bt (ignore-errors 
              (call-with-debugging-environment 
               (lambda () (backtrace 0 nil))))))
    (make-condition 'swank-error :condition condition :backtrace bt)))

(add-hook *new-connection-hook* 'notify-backend-of-connection)
(defun notify-backend-of-connection (connection)
  (declare (ignore connection))
  (emacs-connected))


;;;; Helper macros

(defmacro with-io-redirection ((connection) &body body)
  "Execute BODY I/O redirection to CONNECTION.
If *REDIRECT-IO* is true then all standard I/O streams are redirected."
  `(maybe-call-with-io-redirection ,connection (lambda () ,@body)))

(defun maybe-call-with-io-redirection (connection fun)
  (if *redirect-io*
      (call-with-redirected-io connection fun)
      (funcall fun)))
      
(defmacro with-connection ((connection) &body body)
  "Execute BODY in the context of CONNECTION."
  `(call-with-connection ,connection (lambda () ,@body)))

(defun call-with-connection (connection fun)
  (let ((*emacs-connection* connection))
    (with-io-redirection (*emacs-connection*)
      (call-with-debugger-hook #'swank-debugger-hook fun))))

(defmacro without-interrupts (&body body)
  `(call-without-interrupts (lambda () ,@body)))

(defmacro destructure-case (value &rest patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
         ,@(loop for (pattern . body) in patterns collect 
                   (if (eq pattern t)
                       `(t ,@body)
                       (destructuring-bind (op &rest rands) pattern
                         `(,op (destructuring-bind ,rands ,operands 
                                 ,@body)))))
         ,@(if (eq (caar (last patterns)) t)
               '()
               `((t (error "destructure-case failed: ~S" ,tmp))))))))

(defmacro with-temp-package (var &body body)
  "Execute BODY with VAR bound to a temporary package.
The package is deleted before returning."
  `(let ((,var (make-package (gensym "TEMP-PACKAGE-"))))
     (unwind-protect (progn ,@body)
       (delete-package ,var))))

(defvar *log-events* nil)
(defvar *log-output* *error-output*)
(defvar *event-history* (make-array 40 :initial-element nil)
  "A ring buffer to record events for better error messages.")
(defvar *event-history-index* 0)
(defvar *enable-event-history* t)

(defun log-event (format-string &rest args)
  "Write a message to *terminal-io* when *log-events* is non-nil.
Useful for low level debugging."
  (when *enable-event-history*
    (setf (aref *event-history* *event-history-index*) 
          (format nil "~?" format-string args))
    (setf *event-history-index* 
          (mod (1+ *event-history-index*) (length *event-history*))))
  (when *log-events*
    (apply #'format *log-output* format-string args)
    (force-output *log-output*)))

(defun event-history-to-list ()
  "Return the list of events (older events first)."
  (let ((arr *event-history*)
        (idx *event-history-index*))
    (concatenate 'list (subseq arr idx) (subseq arr 0 idx))))

(defun dump-event-history (stream)
  (dolist (e (event-history-to-list))
    (dump-event e stream)))

(defun dump-event (event stream)
  (cond ((stringp event)
         (write-string (escape-non-ascii event) stream))
        ((null event))
        (t (format stream "Unexpected event: ~A~%" event))))

(defun escape-non-ascii (string)
  "Return a string like STRING but with non-ascii chars escaped."
  (cond ((ascii-string-p string) string)
        (t (with-output-to-string (out)
             (loop for c across string do
               (cond ((ascii-char-p c) (write-char c out))
                     (t (format out "\\x~4,'0X" (char-code c)))))))))

(defun ascii-string-p (o)
  (and (stringp o)
       (every #'ascii-char-p o)))

(defun ascii-char-p (c) 
  (<= (char-code c) 127))

(defmacro do-symbols* ((var &optional (package '*package*) result-form) &body body)
  "Just like do-symbols, but makes sure a symbol is visited only once."
  (let ((seen-ht (gensym "SEEN-HT")))
    `(let ((,seen-ht (make-hash-table :test #'eq)))
      (do-symbols (,var ,package ,result-form)
        (unless (gethash ,var ,seen-ht)
          (setf (gethash ,var ,seen-ht) t)
          ,@body)))))


;;;; TCP Server

(defvar *use-dedicated-output-stream* nil
  "When T swank will attempt to create a second connection to
  Emacs which is used just to send output.")

(defvar *dedicated-output-stream-port* 0
  "Which port we should use for the dedicated output stream.")

(defvar *communication-style* (preferred-communication-style))

(defvar *dont-close* nil
  "Default value of :dont-close argument to start-server and
  create-server.")

(defvar *dedicated-output-stream-buffering* 
  (if (eq *communication-style* :spawn) :full :none)
  "The buffering scheme that should be used for the output stream.
Valid values are :none, :line, and :full.")

(defvar *coding-system* "iso-latin-1-unix")

(defun start-server (port-file &key (style *communication-style*)
                                    (dont-close *dont-close*)
                                    (coding-system *coding-system*))
  "Start the server and write the listen port number to PORT-FILE.
This is the entry point for Emacs."
  (flet ((start-server-aux ()
           (setup-server 0 (lambda (port) 
                             (announce-server-port port-file port))
                         style dont-close 
                         (find-external-format-or-lose coding-system))))
    (if (eq style :spawn)
        (initialize-multiprocessing #'start-server-aux)
        (start-server-aux))))

(defun create-server (&key (port default-server-port)
                      (style *communication-style*)
                      (dont-close *dont-close*) 
                      (coding-system *coding-system*))
  "Start a SWANK server on PORT running in STYLE.
If DONT-CLOSE is true then the listen socket will accept multiple
connections, otherwise it will be closed after the first."
  (setup-server port #'simple-announce-function style dont-close 
                (find-external-format-or-lose coding-system)))

(defun find-external-format-or-lose (coding-system)
  (or (find-external-format coding-system)
      (error "Unsupported coding system: ~s" coding-system)))

(defparameter *loopback-interface* "127.0.0.1")

(defun setup-server (port announce-fn style dont-close external-format)
  (declare (type function announce-fn))
  (let* ((socket (create-socket *loopback-interface* port))
         (port (local-port socket)))
    (funcall announce-fn port)
    (flet ((serve ()
             (serve-connection socket style dont-close external-format)))
      (ecase style
        (:spawn
         (spawn (lambda () (loop do (ignore-errors (serve)) while dont-close))
                :name "Swank"))
        ((:fd-handler :sigio)
         (add-fd-handler socket (lambda () (serve))))
        ((nil) (loop do (serve) while dont-close)))
      port)))

(defun serve-connection (socket style dont-close external-format)
  (let ((closed-socket-p nil))
    (unwind-protect
         (let ((client (accept-authenticated-connection
                        socket :external-format external-format)))
           (unless dont-close
             (close-socket socket)
             (setf closed-socket-p t))
           (let ((connection (create-connection client style)))
             (run-hook *new-connection-hook* connection)
             (push connection *connections*)
             (serve-requests connection)))
      (unless (or dont-close closed-socket-p)
        (close-socket socket)))))

(defun accept-authenticated-connection (&rest args)
  (let ((new (apply #'accept-connection args))
        (success nil))
    (unwind-protect
         (let ((secret (slime-secret)))
           (when secret
             (set-stream-timeout new 20)
             (let ((first-val (decode-message new)))
               (unless (and (stringp first-val) (string= first-val secret))
                 (error "Incoming connection doesn't know the password."))))
           (set-stream-timeout new nil)
           (setf success t))
      (unless success
        (close new :abort t)))
    new))

(defun slime-secret ()
  "Finds the magic secret from the user's home directory.  Returns nil
if the file doesn't exist; otherwise the first line of the file."
  (with-open-file (in
                   (merge-pathnames (user-homedir-pathname) #p".slime-secret")
                   :if-does-not-exist nil)
    (and in (read-line in nil ""))))

(defun serve-requests (connection)
  "Read and process all requests on connections."
  (funcall (connection.serve-requests connection) connection))

(defun announce-server-port (file port)
  (with-open-file (s file
                     :direction :output
                     :if-exists :error
                     :if-does-not-exist :create)
    (format s "~S~%" port))
  (simple-announce-function port))

(defun simple-announce-function (port)
  (when *swank-debug-p*
    (format *debug-io* "~&;; Swank started at port: ~D.~%" port)
    (force-output *debug-io*)))

(defun open-streams (connection)
  "Return the 5 streams for IO redirection:
DEDICATED-OUTPUT INPUT OUTPUT IO REPL-RESULTS"
  (multiple-value-bind (output-fn dedicated-output) 
      (make-output-function connection)
    (let ((input-fn
           (lambda () 
             (with-connection (connection)
               (with-simple-restart (abort-read
                                     "Abort reading input from Emacs.")
                 (read-user-input-from-emacs))))))
      (multiple-value-bind (in out) (make-fn-streams input-fn output-fn)
        (let ((out (or dedicated-output out)))
          (let ((io (make-two-way-stream in out)))
            (mapc #'make-stream-interactive (list in out io))
            (let* ((repl-results-fn
                    (make-output-function-for-target connection :repl-result))
                   (repl-results
                    (nth-value 1 (make-fn-streams 
                                  (lambda ()
                                    (error "Should never be called"))
                                  repl-results-fn))))
              (values dedicated-output in out io repl-results))))))))

(defun make-output-function (connection)
  "Create function to send user output to Emacs.
This function may open a dedicated socket to send output. It
returns two values: the output function, and the dedicated
stream (or NIL if none was created)."
  (if *use-dedicated-output-stream*
      (let ((stream (open-dedicated-output-stream 
                     (connection.socket-io connection))))
        (values (lambda (string)
                  (write-string string stream)
                  (force-output stream))
                stream))
      (values (lambda (string) 
                (with-connection (connection)
                  (with-simple-restart
                      (abort "Abort sending output to Emacs.")
                    (send-to-emacs `(:write-string ,string)))))
              nil)))

(defun make-output-function-for-target (connection target)
  "Create a function to send user output to a specific TARGET in Emacs."
  (lambda (string) 
    (with-connection (connection)
      (with-simple-restart
          (abort "Abort sending output to Emacs.")
        (send-to-emacs `(:write-string ,string nil ,target))))))

(defun open-dedicated-output-stream (socket-io)
  "Open a dedicated output connection to the Emacs on SOCKET-IO.
Return an output stream suitable for writing program output.

This is an optimized way for Lisp to deliver output to Emacs."
  (let ((socket (create-socket *loopback-interface* 
                               *dedicated-output-stream-port*)))
    (unwind-protect
         (let ((port (local-port socket)))
           (encode-message `(:open-dedicated-output-stream ,port) socket-io)
           (let ((dedicated (accept-authenticated-connection 
                             socket 
                             :external-format 
                             (or (ignore-errors
                                   (stream-external-format socket-io))
                                 :default)
                             :buffering *dedicated-output-stream-buffering*
                             :timeout 30)))
             (close-socket socket)
             (setf socket nil)
             dedicated))
      (when socket
        (close-socket socket)))))

(defvar *sldb-quit-restart* 'abort
  "What restart should swank attempt to invoke when the user sldb-quits.")

(defun handle-request (connection)
  "Read and process one request.  The processing is done in the extent
of the toplevel restart."
  (assert (null *swank-state-stack*))
  (let ((*swank-state-stack* '(:handle-request)))
    (with-connection (connection)
      (with-simple-restart (abort "Return to SLIME's top level.")
        (let ((*sldb-quit-restart* (find-restart 'abort)))
          (read-from-emacs))))))

(defun current-socket-io ()
  (connection.socket-io *emacs-connection*))

(defun close-connection (c &optional condition backtrace)
  (format *debug-io* "~&;; swank:close-connection: ~A~%" condition)
  (let ((cleanup (connection.cleanup c)))
    (when cleanup
      (funcall cleanup c)))
  (close (connection.socket-io c))
  (when (connection.dedicated-output c)
    (close (connection.dedicated-output c)))
  (setf *connections* (remove c *connections*))
  (run-hook *connection-closed-hook* c)
  (when (and condition (not (typep condition 'end-of-file)))
    (finish-output *debug-io*)
    (format *debug-io* "~&;; Event history start:~%")
    (dump-event-history *debug-io*)
    (format *debug-io* ";; Event history end.~%~
                        ;; Backtrace:~%~{~A~%~}~
                        ;; Connection to Emacs lost. [~%~
                        ;;  condition: ~A~%~
                        ;;  type: ~S~%~
                        ;;  encoding: ~A style: ~S dedicated: ~S]~%"
            backtrace
            (escape-non-ascii (safe-condition-message condition) )
            (type-of condition)
            (ignore-errors (stream-external-format (connection.socket-io c)))
            (connection.communication-style c)
            *use-dedicated-output-stream*)
    (finish-output *debug-io*)))

(defmacro with-reader-error-handler ((connection) &body body)
  (let ((con (gensym)))
    `(let ((,con ,connection))
       (handler-case 
           (progn ,@body)
         (swank-error (e)
           (close-connection ,con 
                             (swank-error.condition e)
                             (swank-error.backtrace e)))))))

(defslimefun simple-break ()
  (with-simple-restart  (continue "Continue from interrupt.")
    (call-with-debugger-hook
     #'swank-debugger-hook
     (lambda ()
       (invoke-debugger 
        (make-condition 'simple-error 
                        :format-control "Interrupt from Emacs")))))
  nil)

;;;;;; Thread based communication

(defvar *active-threads* '())

(defun read-loop (control-thread input-stream connection)
  (with-reader-error-handler (connection)
    (loop (send control-thread (decode-message input-stream)))))

(defun dispatch-loop (socket-io connection)
  (let ((*emacs-connection* connection))
    (handler-case
        (loop (dispatch-event (receive) socket-io))
      (error (e)
        (close-connection connection e)))))

(defun repl-thread (connection)
  (let ((thread (connection.repl-thread connection)))
    (when (not thread)
      (log-event "ERROR: repl-thread is nil"))
    (assert thread)
    (cond ((thread-alive-p thread)
           thread)
          (t
           (setf (connection.repl-thread connection)
                 (spawn-repl-thread connection "new-repl-thread"))))))

(defun find-worker-thread (id)
  (etypecase id
    ((member t)
     (car *active-threads*))
    ((member :repl-thread) 
     (repl-thread *emacs-connection*))
    (fixnum 
     (find-thread id))))

(defun interrupt-worker-thread (id)
  (let ((thread (or (find-worker-thread id)
                    (repl-thread *emacs-connection*))))
    (interrupt-thread thread #'simple-break)))

(defun thread-for-evaluation (id)
  "Find or create a thread to evaluate the next request."
  (let ((c *emacs-connection*))
    (etypecase id
      ((member t)
       (spawn-worker-thread c))
      ((member :repl-thread)
       (repl-thread c))
      (fixnum
       (find-thread id)))))

(defun spawn-worker-thread (connection)
  (spawn (lambda () 
           (with-bindings *default-worker-thread-bindings*
             (handle-request connection)))
         :name "worker"))

(defun spawn-repl-thread (connection name)
  (spawn (lambda () 
           (with-bindings *default-worker-thread-bindings*
             (repl-loop connection)))
         :name name))

(defun dispatch-event (event socket-io)
  "Handle an event triggered either by Emacs or within Lisp."
  (log-event "DISPATCHING: ~S~%" event)
  (destructure-case event
    ((:emacs-rex form package thread-id id)
     (let ((thread (thread-for-evaluation thread-id)))
       (push thread *active-threads*)
       (send thread `(eval-for-emacs ,form ,package ,id))))
    ((:return thread &rest args)
     (let ((tail (member thread *active-threads*)))
       (setq *active-threads* (nconc (ldiff *active-threads* tail)
                                     (cdr tail))))
     (encode-message `(:return ,@args) socket-io))
    ((:emacs-interrupt thread-id)
     (interrupt-worker-thread thread-id))
    (((:debug :debug-condition :debug-activate :debug-return)
      thread &rest args)
     (encode-message `(,(car event) ,(thread-id thread) ,@args) socket-io))
    ((:read-string thread &rest args)
     (encode-message `(:read-string ,(thread-id thread) ,@args) socket-io))
    ((:y-or-n-p thread &rest args)
     (encode-message `(:y-or-n-p ,(thread-id thread) ,@args) socket-io))
    ((:read-aborted thread &rest args)
     (encode-message `(:read-aborted ,(thread-id thread) ,@args) socket-io))
    ((:emacs-return-string thread-id tag string)
     (send (find-thread thread-id) `(take-input ,tag ,string)))
    ((:eval thread &rest args)
     (encode-message `(:eval ,(thread-id thread) ,@args) socket-io))
    ((:emacs-return thread-id tag value)
     (send (find-thread thread-id) `(take-input ,tag ,value)))
    (((:write-string :presentation-start :presentation-end
                     :new-package :new-features :ed :%apply :indentation-update
                     :eval-no-wait :background-message :inspect)
      &rest _)
     (declare (ignore _))
     (encode-message event socket-io))))

(defun spawn-threads-for-connection (connection)
  (macrolet ((without-debugger-hook (&body body) 
               `(call-with-debugger-hook nil (lambda () ,@body))))
    (let* ((socket-io (connection.socket-io connection))
           (control-thread (spawn (lambda ()
                                    (without-debugger-hook
                                     (dispatch-loop socket-io connection)))
                                  :name "control-thread")))
      (setf (connection.control-thread connection) control-thread)
      (let ((reader-thread (spawn (lambda () 
                                    (let ((go (receive)))
                                      (assert (eq go 'accept-input)))
                                    (without-debugger-hook
                                     (read-loop control-thread socket-io
                                                connection)))
                                  :name "reader-thread"))
            (repl-thread (spawn-repl-thread connection "repl-thread")))
        (setf (connection.repl-thread connection) repl-thread)
        (setf (connection.reader-thread connection) reader-thread)
        (send reader-thread 'accept-input)
        connection))))

(defun cleanup-connection-threads (connection)
  (let ((threads (list (connection.repl-thread connection)
                       (connection.reader-thread connection)
                       (connection.control-thread connection))))
    (dolist (thread threads)
      (when (and thread 
                 (thread-alive-p thread)
                 (not (equal (current-thread) thread)))
        (kill-thread thread)))))

(defun repl-loop (connection)
  (loop (handle-request connection)))

(defun process-available-input (stream fn)
  (loop while (input-available-p stream)
        do (funcall fn)))

(defun input-available-p (stream)
  ;; return true iff we can read from STREAM without waiting or if we
  ;; hit EOF
  (let ((c (read-char-no-hang stream nil :eof)))
    (cond ((not c) nil)
          ((eq c :eof) t)
          (t 
           (unread-char c stream)
           t))))

;;;;;; Signal driven IO

(defun install-sigio-handler (connection)
  (let ((client (connection.socket-io connection)))
    (flet ((handler ()
	     (cond ((null *swank-state-stack*)
		    (with-reader-error-handler (connection)
		      (process-available-input 
		       client (lambda () (handle-request connection)))))
		   ((eq (car *swank-state-stack*) :read-next-form))
		   (t (process-available-input client #'read-from-emacs)))))
      (add-sigio-handler client #'handler)
      (handler))))

(defun deinstall-sigio-handler (connection)
  (remove-sigio-handlers (connection.socket-io connection)))

;;;;;; SERVE-EVENT based IO

(defun install-fd-handler (connection)
  (let ((client (connection.socket-io connection)))
    (flet ((handler ()   
	     (cond ((null *swank-state-stack*)
		    (with-reader-error-handler (connection)
		      (process-available-input 
		       client (lambda () (handle-request connection)))))
		   ((eq (car *swank-state-stack*) :read-next-form))
		   (t 
		    (process-available-input client #'read-from-emacs)))))
      ;;;; handle sigint
      ;;(install-debugger-globally
      ;; (lambda (c h)
      ;;   (with-reader-error-handler (connection)
      ;;     (block debugger
      ;;       (with-connection (connection)
      ;;	 (swank-debugger-hook c h)
      ;;	 (return-from debugger))
      ;;       (abort)))))
      (add-fd-handler client #'handler)
      (handler))))

(defun deinstall-fd-handler (connection)
  (remove-fd-handlers (connection.socket-io connection)))

;;;;;; Simple sequential IO

(defun simple-serve-requests (connection)
  (unwind-protect 
       (with-simple-restart (close-connection "Close SLIME connection")
         (with-reader-error-handler (connection)
           (loop
            (handle-request connection))))
    (close-connection connection)))

(defun read-from-socket-io ()
  (let ((event (decode-message (current-socket-io))))
    (log-event "DISPATCHING: ~S~%" event)
    (destructure-case event
      ((:emacs-rex form package thread id)
       (declare (ignore thread))
       `(eval-for-emacs ,form ,package ,id))
      ((:emacs-interrupt thread)
       (declare (ignore thread))
       '(simple-break))
      ((:emacs-return-string thread tag string)
       (declare (ignore thread))
       `(take-input ,tag ,string))
      ((:emacs-return thread tag value)
       (declare (ignore thread))
       `(take-input ,tag ,value)))))

(defun send-to-socket-io (event) 
  (log-event "DISPATCHING: ~S~%" event)
  (flet ((send (o) 
           (without-interrupts 
             (encode-message o (current-socket-io)))))
    (destructure-case event
      (((:debug-activate :debug :debug-return :read-string :read-aborted 
                         :y-or-n-p :eval)
        thread &rest args)
       (declare (ignore thread))
       (send `(,(car event) 0 ,@args)))
      ((:return thread &rest args)
       (declare (ignore thread))
       (send `(:return ,@args)))
      (((:write-string :new-package :new-features :debug-condition
                       :presentation-start :presentation-end
                       :indentation-update :ed :%apply :eval-no-wait
                       :background-message :inspect)
        &rest _)
       (declare (ignore _))
       (send event)))))

(defun initialize-streams-for-connection (connection)
  (multiple-value-bind (dedicated in out io repl-results) 
      (open-streams connection)
    (setf (connection.dedicated-output connection) dedicated
          (connection.user-io connection)          io
          (connection.user-output connection)      out
          (connection.user-input connection)       in
          (connection.repl-results connection)     repl-results)
    connection))

(defun create-connection (socket-io style)
  (let ((success nil))
    (unwind-protect
         (let ((c (ecase style
                    (:spawn
                     (make-connection :socket-io socket-io
                                      :read #'read-from-control-thread
                                      :send #'send-to-control-thread
                                      :serve-requests #'spawn-threads-for-connection
                                      :cleanup #'cleanup-connection-threads))
                    (:sigio
                     (make-connection :socket-io socket-io
                                      :read #'read-from-socket-io
                                      :send #'send-to-socket-io
                                      :serve-requests #'install-sigio-handler
                                      :cleanup #'deinstall-sigio-handler))
                    (:fd-handler
                     (make-connection :socket-io socket-io
                                      :read #'read-from-socket-io
                                      :send #'send-to-socket-io
                                      :serve-requests #'install-fd-handler
                                      :cleanup #'deinstall-fd-handler))
                    ((nil)
                     (make-connection :socket-io socket-io
                                      :read #'read-from-socket-io
                                      :send #'send-to-socket-io
                                      :serve-requests #'simple-serve-requests)))))
           (setf (connection.communication-style c) style)
           (initialize-streams-for-connection c)
           (setf success t)
           c)
      (unless success
        (close socket-io :abort t)))))


;;;; IO to Emacs
;;;
;;; This code handles redirection of the standard I/O streams
;;; (`*standard-output*', etc) into Emacs. The `connection' structure
;;; contains the appropriate streams, so all we have to do is make the
;;; right bindings.

;;;;; Global I/O redirection framework
;;;
;;; Optionally, the top-level global bindings of the standard streams
;;; can be assigned to be redirected to Emacs. When Emacs connects we
;;; redirect the streams into the connection, and they keep going into
;;; that connection even if more are established. If the connection
;;; handling the streams closes then another is chosen, or if there
;;; are no connections then we revert to the original (real) streams.
;;;
;;; It is slightly tricky to assign the global values of standard
;;; streams because they are often shadowed by dynamic bindings. We
;;; solve this problem by introducing an extra indirection via synonym
;;; streams, so that *STANDARD-INPUT* is a synonym stream to
;;; *CURRENT-STANDARD-INPUT*, etc. We never shadow the "current"
;;; variables, so they can always be assigned to affect a global
;;; change.

(defvar *globally-redirect-io* nil
  "When non-nil globally redirect all standard streams to Emacs.")

;;;;; Global redirection setup

(defvar *saved-global-streams* '()
  "A plist to save and restore redirected stream objects.
E.g. the value for '*standard-output* holds the stream object
for *standard-output* before we install our redirection.")

(defun setup-stream-indirection (stream-var &optional stream)
  "Setup redirection scaffolding for a global stream variable.
Supposing (for example) STREAM-VAR is *STANDARD-INPUT*, this macro:

1. Saves the value of *STANDARD-INPUT* in `*SAVED-GLOBAL-STREAMS*'.

2. Creates *CURRENT-STANDARD-INPUT*, initially with the same value as
*STANDARD-INPUT*.

3. Assigns *STANDARD-INPUT* to a synonym stream pointing to
*CURRENT-STANDARD-INPUT*.

This has the effect of making *CURRENT-STANDARD-INPUT* contain the
effective global value for *STANDARD-INPUT*. This way we can assign
the effective global value even when *STANDARD-INPUT* is shadowed by a
dynamic binding."
  (let ((current-stream-var (prefixed-var '#:current stream-var))
        (stream (or stream (symbol-value stream-var))))
    ;; Save the real stream value for the future.
    (setf (getf *saved-global-streams* stream-var) stream)
    ;; Define a new variable for the effective stream.
    ;; This can be reassigned.
    (proclaim `(special ,current-stream-var))
    (set current-stream-var stream)
    ;; Assign the real binding as a synonym for the current one.
    (set stream-var (make-synonym-stream current-stream-var))))

(defun prefixed-var (prefix variable-symbol)
  "(PREFIXED-VAR \"FOO\" '*BAR*) => SWANK::*FOO-BAR*"
  (let ((basename (subseq (symbol-name variable-symbol) 1)))
    (intern (format nil "*~A-~A" (string prefix) basename) :swank)))

(defvar *standard-output-streams*
  '(*standard-output* *error-output* *trace-output*)
  "The symbols naming standard output streams.")

(defvar *standard-input-streams*
  '(*standard-input*)
  "The symbols naming standard input streams.")

(defvar *standard-io-streams*
  '(*debug-io* *query-io* *terminal-io*)
  "The symbols naming standard io streams.")

(defun init-global-stream-redirection ()
  (when *globally-redirect-io*
    (mapc #'setup-stream-indirection 
          (append *standard-output-streams*
                  *standard-input-streams*
                  *standard-io-streams*))))

(add-hook *after-init-hook* 'init-global-stream-redirection)

(defun globally-redirect-io-to-connection (connection)
  "Set the standard I/O streams to redirect to CONNECTION.
Assigns *CURRENT-<STREAM>* for all standard streams."
  (dolist (o *standard-output-streams*)
    (set (prefixed-var '#:current o)
         (connection.user-output connection)))
  ;; FIXME: If we redirect standard input to Emacs then we get the
  ;; regular Lisp top-level trying to read from our REPL.
  ;;
  ;; Perhaps the ideal would be for the real top-level to run in a
  ;; thread with local bindings for all the standard streams. Failing
  ;; that we probably would like to inhibit it from reading while
  ;; Emacs is connected.
  ;;
  ;; Meanwhile we just leave *standard-input* alone.
  #+NIL
  (dolist (i *standard-input-streams*)
    (set (prefixed-var '#:current i)
         (connection.user-input connection)))
  (dolist (io *standard-io-streams*)
    (set (prefixed-var '#:current io)
         (connection.user-io connection))))

(defun revert-global-io-redirection ()
  "Set *CURRENT-<STREAM>* to *REAL-<STREAM>* for all standard streams."
  (dolist (stream-var (append *standard-output-streams*
                              *standard-input-streams*
                              *standard-io-streams*))
    (set (prefixed-var '#:current stream-var)
         (getf *saved-global-streams* stream-var))))

;;;;; Global redirection hooks

(defvar *global-stdio-connection* nil
  "The connection to which standard I/O streams are globally redirected.
NIL if streams are not globally redirected.")

(defun maybe-redirect-global-io (connection)
  "Consider globally redirecting to a newly-established CONNECTION."
  (when (and *globally-redirect-io* (null *global-stdio-connection*))
    (setq *global-stdio-connection* connection)
    (globally-redirect-io-to-connection connection)))

(defun update-redirection-after-close (closed-connection)
  "Update redirection after a connection closes."
  (when (eq *global-stdio-connection* closed-connection)
    (if (and (default-connection) *globally-redirect-io*)
        ;; Redirect to another connection.
        (globally-redirect-io-to-connection (default-connection))
        ;; No more connections, revert to the real streams.
        (progn (revert-global-io-redirection)
               (setq *global-stdio-connection* nil)))))

(add-hook *new-connection-hook*    'maybe-redirect-global-io)
(add-hook *connection-closed-hook* 'update-redirection-after-close)

;;;;; Redirection during requests
;;;
;;; We always redirect the standard streams to Emacs while evaluating
;;; an RPC. This is done with simple dynamic bindings.

(defun call-with-redirected-io (connection function)
  "Call FUNCTION with I/O streams redirected via CONNECTION."
  (declare (type function function))
  (let* ((io  (connection.user-io connection))
         (in  (connection.user-input connection))
         (out (connection.user-output connection))
         (*standard-output* out)
         (*error-output* out)
         (*trace-output* out)
         (*debug-io* io)
         (*query-io* io)
         (*standard-input* in)
         (*terminal-io* io))
    (funcall function)))

(defun read-from-emacs ()
  "Read and process a request from Emacs."
  (apply #'funcall (funcall (connection.read *emacs-connection*))))

(defun read-from-control-thread ()
  (receive))

(defun decode-message (stream)
  "Read an S-expression from STREAM using the SLIME protocol."
  (let ((*swank-state-stack* (cons :read-next-form *swank-state-stack*)))
    (handler-bind ((error (lambda (c) (error (make-swank-error c)))))
      (let* ((length (decode-message-length stream))
             (string (make-string length))
             (pos (read-sequence string stream)))
        (assert (= pos length) ()
                "Short read: length=~D  pos=~D" length pos)
        (log-event "READ: ~S~%" string)
        (read-form string)))))

(defun decode-message-length (stream)
  (let ((buffer (make-string 6)))
    (dotimes (i 6)
      (setf (aref buffer i) (read-char stream)))
    (parse-integer buffer :radix #x10)))

(defun read-form (string)
  (with-standard-io-syntax
    (let ((*package* *swank-io-package*))
      (read-from-string string))))

(defvar *slime-features* nil
  "The feature list that has been sent to Emacs.")

(defun send-to-emacs (object)
  "Send OBJECT to Emacs."
  (funcall (connection.send *emacs-connection*) object))

(defun send-oob-to-emacs (object)
  (send-to-emacs object))

(defun send-to-control-thread (object)
  (send (connection.control-thread *emacs-connection*) object))

(defun encode-message (message stream)
  (let* ((string (prin1-to-string-for-emacs message))
         (length (length string)))
    (log-event "WRITE: ~A~%" string)
    (let ((*print-pretty* nil))
      (format stream "~6,'0x" length))
    (write-string string stream)
    ;;(terpri stream)
    (finish-output stream)))

(defun prin1-to-string-for-emacs (object)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-readably* nil)
          (*print-pretty* nil)
          (*package* *swank-io-package*))
      (prin1-to-string object))))

(defun force-user-output ()
  (force-output (connection.user-io *emacs-connection*))
  (finish-output (connection.user-output *emacs-connection*)))

(defun clear-user-input  ()
  (clear-input (connection.user-input *emacs-connection*)))

(defvar *read-input-catch-tag* 0)

(defun intern-catch-tag (tag)
  ;; fixnums aren't eq in ABCL, so we use intern to create tags
  (intern (format nil "~D" tag) :swank))

(defun read-user-input-from-emacs ()
  (let ((tag (incf *read-input-catch-tag*)))
    (force-output)
    (send-to-emacs `(:read-string ,(current-thread) ,tag))
    (let ((ok nil))
      (unwind-protect
           (prog1 (catch (intern-catch-tag tag)
                    (loop (read-from-emacs)))
             (setq ok t))
        (unless ok 
          (send-to-emacs `(:read-aborted ,(current-thread) ,tag)))))))

(defun y-or-n-p-in-emacs (format-string &rest arguments)
  "Like y-or-n-p, but ask in the Emacs minibuffer."
  (let ((tag (incf *read-input-catch-tag*))
        (question (apply #'format nil format-string arguments)))
    (force-output)
    (send-to-emacs `(:y-or-n-p ,(current-thread) ,tag ,question))
    (catch (intern-catch-tag tag)
      (loop (read-from-emacs)))))

(defslimefun take-input (tag input)
  "Return the string INPUT to the continuation TAG."
  (throw (intern-catch-tag tag) input))

(defun process-form-for-emacs (form)
  "Returns a string which emacs will read as equivalent to
FORM. FORM can contain lists, strings, characters, symbols and
numbers.

Characters are converted emacs' ?<char> notaion, strings are left
as they are (except for espacing any nested \" chars, numbers are
printed in base 10 and symbols are printed as their symbol-name
converted to lower case."
  (etypecase form
    (string (format nil "~S" form))
    (cons (format nil "(~A . ~A)"
                  (process-form-for-emacs (car form))
                  (process-form-for-emacs (cdr form))))
    (character (format nil "?~C" form))
    (symbol (concatenate 'string (when (eq (symbol-package form)
                                           #.(find-package "KEYWORD"))
                                   ":")
                         (string-downcase (symbol-name form))))
    (number (let ((*print-base* 10))
              (princ-to-string form)))))

(defun eval-in-emacs (form &optional nowait)
  "Eval FORM in Emacs."
  (cond (nowait 
         (send-to-emacs `(:eval-no-wait ,(process-form-for-emacs form))))
        (t
         (force-output)
         (let* ((tag (incf *read-input-catch-tag*))
                (value (catch (intern-catch-tag tag)
                         (send-to-emacs 
                          `(:eval ,(current-thread) ,tag 
                            ,(process-form-for-emacs form)))
                         (loop (read-from-emacs)))))
           (destructure-case value
             ((:ok value) value)
             ((:abort) (abort)))))))

(defun present-in-emacs (value-or-values &key (separated-by " "))
  "Present VALUE in the Emacs repl buffer of the current thread."
  (unless (consp value-or-values)
    (setf value-or-values (list value-or-values)))
  (flet ((present (value)
           (if (stringp value)
               (send-to-emacs `(:write-string ,value))
               (let ((id (save-presented-object value)))
                 (send-to-emacs `(:write-string ,(prin1-to-string value) ,id))))))
    (map nil (let ((first-time-p t))
               (lambda (value)
                 (when (and (not first-time-p)
                            separated-by)
                   (present separated-by))
                 (present value)
                 (setf first-time-p nil)))
         value-or-values))
  (values))

(defvar *swank-wire-protocol-version* nil
  "The version of the swank/slime communication protocol.")

(defslimefun connection-info ()
  "Return a key-value list of the form: 
\(&key PID STYLE LISP-IMPLEMENTATION MACHINE FEATURES PACKAGE VERSION)
PID: is the process-id of Lisp process (or nil, depending on the STYLE)
STYLE: the communication style
LISP-IMPLEMENTATION: a list (&key TYPE NAME VERSION)
FEATURES: a list of keywords
PACKAGE: a list (&key NAME PROMPT)
VERSION: the protocol version"
  (setq *slime-features* *features*)
  `(:pid ,(getpid) :style ,(connection.communication-style *emacs-connection*)
    :lisp-implementation (:type ,(lisp-implementation-type)
                          :name ,(lisp-implementation-type-name)
                          :version ,(lisp-implementation-version))
    :machine (:instance ,(machine-instance)
              :type ,(machine-type)
              :version ,(machine-version))
    :features ,(features-for-emacs)
    :package (:name ,(package-name *package*)
              :prompt ,(package-string-for-prompt *package*))
    :version ,*swank-wire-protocol-version*))

(defslimefun io-speed-test (&optional (n 5000) (m 1))
  (let* ((s *standard-output*)
         (*trace-output* (make-broadcast-stream s *log-output*)))
    (time (progn
            (dotimes (i n)
              (format s "~D abcdefghijklm~%" i)
              (when (zerop (mod n m))
                (force-output s)))
            (finish-output s)
            (when *emacs-connection*
              (eval-in-emacs '(message "done.")))))
    (terpri *trace-output*)
    (finish-output *trace-output*)
    nil))


;;;; Reading and printing

(defmacro define-special (name doc)
  "Define a special variable NAME with doc string DOC.
This is like defvar, but NAME will not be initialized."
  `(progn
    (defvar ,name)
    (setf (documentation ',name 'variable) ,doc)))

(define-special *buffer-package*     
    "Package corresponding to slime-buffer-package.  

EVAL-FOR-EMACS binds *buffer-package*.  Strings originating from a slime
buffer are best read in this package.  See also FROM-STRING and TO-STRING.")

(define-special *buffer-readtable*
    "Readtable associated with the current buffer")

(defmacro with-buffer-syntax ((&rest _) &body body)
  "Execute BODY with appropriate *package* and *readtable* bindings.

This should be used for code that is conceptionally executed in an
Emacs buffer."
  (destructuring-bind () _
    `(call-with-buffer-syntax (lambda () ,@body))))

(defun call-with-buffer-syntax (fun)
  (let ((*package* *buffer-package*))
    ;; Don't shadow *readtable* unnecessarily because that prevents
    ;; the user from assigning to it.
    (if (eq *readtable* *buffer-readtable*)
        (call-with-syntax-hooks fun)
        (let ((*readtable* *buffer-readtable*))
          (call-with-syntax-hooks fun)))))

(defun to-string (object)
  "Write OBJECT in the *BUFFER-PACKAGE*.
The result may not be readable. Handles problems with PRINT-OBJECT methods
gracefully."
  (with-buffer-syntax ()
    (let ((*print-readably* nil))
      (handler-case
          (prin1-to-string object)
        (error ()
          (with-output-to-string (s)
            (print-unreadable-object (object s :type t :identity t)
              (princ "<<error printing object>>" s))))))))

(defun from-string (string)
  "Read string in the *BUFFER-PACKAGE*"
  (with-buffer-syntax ()
    (let ((*read-suppress* nil))
      (read-from-string string))))

;; FIXME: deal with #\| etc.  hard to do portably.
(defun tokenize-symbol (string)
  (let ((package (let ((pos (position #\: string)))
                   (if pos (subseq string 0 pos) nil)))
        (symbol (let ((pos (position #\: string :from-end t)))
                  (if pos (subseq string (1+ pos)) string)))
        (internp (search "::" string)))
    (values symbol package internp)))

(defun tokenize-symbol-thoroughly (string)
  "This version of tokenize-symbol handles escape characters."
  (let ((package nil)
        (token (make-array (length string) :element-type 'character
                           :fill-pointer 0))
        (backslash nil)
        (vertical nil)
        (internp nil))
    (loop for char across string
       do (cond
            (backslash
             (vector-push-extend char token)
             (setq backslash nil))
            ((char= char #\\) ; Quotes next character, even within |...|
             (setq backslash t))
            ((char= char #\|)
             (setq vertical t))
            (vertical
             (vector-push-extend char token))
            ((char= char #\:)
             (if package
                 (setq internp t)
                 (setq package token
                       token (make-array (length string)
                                         :element-type 'character
                                         :fill-pointer 0))))
            (t
             (vector-push-extend (casify-char char) token))))
    (values token package internp)))

(defun casify-char (char)
  "Convert CHAR accoring to readtable-case."
  (ecase (readtable-case *readtable*)
    (:preserve char)
    (:upcase   (char-upcase char))
    (:downcase (char-downcase char))
    (:invert (if (upper-case-p char)
                 (char-downcase char)
                 (char-upcase char)))))

(defun parse-symbol (string &optional (package *package*))
  "Find the symbol named STRING.
Return the symbol and a flag indicating whether the symbols was found."
  (multiple-value-bind (sname pname) (tokenize-symbol-thoroughly string)
    (let ((package (cond ((string= pname "") keyword-package)
                         (pname              (find-package pname))
                         (t                  package))))
      (if package
          (find-symbol sname package)
          (values nil nil)))))

(defun parse-symbol-or-lose (string &optional (package *package*))
  (multiple-value-bind (symbol status) (parse-symbol string package)
    (if status
        (values symbol status)
        (error "Unknown symbol: ~A [in ~A]" string package))))

;; FIXME: interns the name
(defun parse-package (string)
  "Find the package named STRING.
Return the package or nil."
  (multiple-value-bind (name pos) 
      (if (zerop (length string))
          (values :|| 0)
          (let ((*package* *swank-io-package*))
            (ignore-errors (read-from-string string))))
    (and name
         (or (symbolp name) 
             (stringp name))
         (= (length string) pos)
         (find-package name))))

(defun unparse-name (string)
  "Print the name STRING according to the current printer settings."
  ;; this is intended for package or symbol names
  (subseq (prin1-to-string (make-symbol string)) 2))

(defun guess-package (string)
  "Guess which package corresponds to STRING.
Return nil if no package matches."
  (or (find-package string)
      (parse-package string)
      (if (find #\! string) ; for SBCL
          (guess-package (substitute #\- #\! string)))))

(defvar *readtable-alist* (default-readtable-alist)
  "An alist mapping package names to readtables.")

(defun guess-buffer-readtable (package-name)
  (let ((package (guess-package package-name)))
    (or (and package 
             (cdr (assoc (package-name package) *readtable-alist* 
                         :test #'string=)))
        *readtable*)))

(defun valid-operator-symbol-p (symbol)
  "Is SYMBOL the name of a function, a macro, or a special-operator?"
  (or (fboundp symbol)
      (macro-function symbol)
      (special-operator-p symbol)))
  
(defun valid-operator-name-p (string)
  "Is STRING the name of a function, macro, or special-operator?"
  (let ((symbol (parse-symbol string)))
    (valid-operator-symbol-p symbol)))


;;;; Arglists

(defun find-valid-operator-name (names)
  "As a secondary result, returns its index."
  (let ((index 
         (position-if (lambda (name)
                        (or (consp name)
                            (valid-operator-name-p name)))
                      names)))
    (if index
        (values (elt names index) index)
        (values nil nil))))

(defslimefun arglist-for-echo-area (names &key print-right-margin
                                          print-lines arg-indices)
  "Return the arglist for the first function, macro, or special-op in NAMES."
  (handler-case
      (with-buffer-syntax ()
        (multiple-value-bind (name which)
            (find-valid-operator-name names)
          (when which
            (let ((arg-index (and arg-indices (elt arg-indices which))))
              (multiple-value-bind (form operator-name)
                  (operator-designator-to-form name)
                (let ((*print-right-margin* print-right-margin))
                  (format-arglist-for-echo-area
                   form operator-name
                   :print-right-margin print-right-margin
                   :print-lines print-lines
                   :highlight (and arg-index
                                   (not (zerop arg-index))
                                   ;; don't highlight the operator
                                   arg-index))))))))
    (error (cond)
      (format nil "ARGLIST: ~A" cond))))

(defun operator-designator-to-form (name)
  (etypecase name
    (cons
     (destructure-case name
       ((:make-instance class-name operator-name &rest args)
        (let ((parsed-operator-name (parse-symbol operator-name)))
          (values `(,parsed-operator-name ,@args ',(parse-symbol class-name))
                  operator-name)))
       ((:defmethod generic-name)
        (values `(defmethod ,(parse-symbol generic-name))
                'defmethod))))
    (string
     (values `(,(parse-symbol name))
             name))))

(defun clean-arglist (arglist)
  "Remove &whole, &enviroment, and &aux elements from ARGLIST."
  (cond ((null arglist) '())
        ((member (car arglist) '(&whole &environment))
         (clean-arglist (cddr arglist)))
        ((eq (car arglist) '&aux)
         '())
        (t (cons (car arglist) (clean-arglist (cdr arglist))))))

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
  known-junk            ; &whole, &environment
  unknown-junk)         ; unparsed stuff

(defun print-arglist (arglist &key operator highlight)
  (let ((index 0)
        (need-space nil))
    (labels ((print-arg (arg)
               (typecase arg
                 (arglist               ; destructuring pattern
                  (print-arglist arg))
                 (optional-arg 
                  (princ (encode-optional-arg arg)))
                 (keyword-arg
                  (let ((enc-arg (encode-keyword-arg arg)))
                    (etypecase enc-arg
                      (symbol (princ enc-arg))
                      ((cons symbol) 
                       (pprint-logical-block (nil nil :prefix "(" :suffix ")")
                         (princ (car enc-arg))
                         (write-char #\space)
                         (pprint-fill *standard-output* (cdr enc-arg) nil)))
                      ((cons cons)
                       (pprint-logical-block (nil nil :prefix "(" :suffix ")")
                         (pprint-logical-block (nil nil :prefix "(" :suffix ")")
                           (prin1 (caar enc-arg))
                           (write-char #\space)
                           (print-arg (keyword-arg.arg-name arg)))
                         (unless (null (cdr enc-arg))
                           (write-char #\space))
                         (pprint-fill *standard-output* (cdr enc-arg) nil))))))
                 (t           ; required formal or provided actual arg
                  (princ arg))))
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
        (cond ((not (arglist.rest arglist)))
              ((arglist.body-p arglist)
               (print-with-space '&body)
               (print-with-highlight (arglist.rest arglist) #'<=))
              (t
               (print-with-space '&rest)
               (print-with-highlight (arglist.rest arglist) #'<=)))
        (mapc #'print-with-space                 
              (arglist.unknown-junk arglist))))))  

(defun decoded-arglist-to-string (arglist package 
                                  &key operator print-right-margin 
                                  print-lines highlight)
  "Print the decoded ARGLIST for display in the echo area.  The
argument name are printed without package qualifiers and pretty
printing of (function foo) as #'foo is suppressed.  If HIGHLIGHT is
non-nil, it must be the index of an argument; highlight this argument.
If OPERATOR is non-nil, put it in front of the arglist."
  (with-output-to-string (*standard-output*)
    (with-standard-io-syntax
      (let ((*package* package) (*print-case* :downcase)
            (*print-pretty* t) (*print-circle* nil) (*print-readably* nil)
            (*print-level* 10) (*print-length* 20)
            (*print-right-margin* print-right-margin)
            (*print-lines* print-lines))
        (print-arglist arglist :operator operator :highlight highlight)))))

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
         (abort-request "Bad keyword item of formal argument list"))))

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
  (let ((mode nil)
        (result (make-arglist)))
    (dolist (arg arglist)
      (cond
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
            (push arg (arglist.known-junk result)))))))
    (nreversef (arglist.required-args result))
    (nreversef (arglist.optional-args result))
    (nreversef (arglist.keyword-args result))
    (nreversef (arglist.aux-args result))
    (nreversef (arglist.known-junk result))
    (nreversef (arglist.unknown-junk result))
    result))

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
                 (symbol (princ arg))
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
               (= (length function-name-form) 2)
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

(defslimefun arglist-for-insertion (name)
  (with-buffer-syntax ()
    (let ((symbol (parse-symbol name)))
      (cond 
        ((and symbol 
              (valid-operator-name-p name))
         (let ((decoded-arglist
                (compute-enriched-decoded-arglist symbol nil)))
           (if (eql decoded-arglist :not-available)
               :not-available
               (decoded-arglist-to-template-string decoded-arglist 
                                                   *buffer-package*))))
        (t
         :not-available)))))

(defvar *remove-keywords-alist*
  '((:test :test-not)
    (:test-not :test)))

(defun remove-actual-args (decoded-arglist actual-arglist)
  "Remove from DECODED-ARGLIST the arguments that have already been
provided in ACTUAL-ARGLIST."
  (loop while (and actual-arglist
		   (arglist.required-args decoded-arglist))
     do (progn (pop actual-arglist)
	       (pop (arglist.required-args decoded-arglist))))
  (loop while (and actual-arglist
		   (arglist.optional-args decoded-arglist))
     do (progn (pop actual-arglist)
	       (pop (arglist.optional-args decoded-arglist))))
  (loop for keyword in actual-arglist by #'cddr
     for keywords-to-remove = (cdr (assoc keyword *remove-keywords-alist*))
     do (setf (arglist.keyword-args decoded-arglist)
	      (remove-if (lambda (kw)
                           (or (eql kw keyword)
                               (member kw keywords-to-remove)))
                         (arglist.keyword-args decoded-arglist)
                         :key #'keyword-arg.keyword))))

(defgeneric form-completion (operator-form argument-forms &key remove-args))
  
(defmethod form-completion (operator-form argument-forms &key (remove-args t))
  (when (and (symbolp operator-form)
	     (valid-operator-symbol-p operator-form))
    (multiple-value-bind (decoded-arglist determining-args any-enrichment)
        (compute-enriched-decoded-arglist operator-form argument-forms)
      (etypecase decoded-arglist
	((member :not-available)
	 :not-available)
	(arglist
	 (cond 
	   (remove-args
	    ;; get rid of formal args already provided
	    (remove-actual-args decoded-arglist argument-forms))
	   (t
	    ;; replace some formal args by determining actual args
	    (remove-actual-args decoded-arglist determining-args)
	    (setf (arglist.provided-args decoded-arglist)
		  determining-args)))
         (return-from form-completion 
           (values decoded-arglist any-enrichment))))))
  :not-available)

(defmethod form-completion ((operator-form (eql 'defmethod))
			    argument-forms &key (remove-args t))
  (when (and (listp argument-forms)
	     (not (null argument-forms)) ;have generic function name
	     (notany #'listp (rest argument-forms))) ;don't have arglist yet
    (let* ((gf-name (first argument-forms))
	   (gf (and (or (symbolp gf-name)
			(and (listp gf-name)
			     (eql (first gf-name) 'setf)))
		    (fboundp gf-name)
		    (fdefinition gf-name))))
      (when (typep gf 'generic-function)
	(let ((arglist (arglist gf)))
	  (etypecase arglist
	    ((member :not-available))
	    (list
	     (return-from form-completion
               (values (make-arglist :provided-args (if remove-args
                                                        nil
                                                        (list gf-name))
                                     :required-args (list arglist)
                                     :rest "body" :body-p t)
                       t))))))))
  (call-next-method))

(defun read-incomplete-form-from-string (form-string)
  (with-buffer-syntax ()
    (handler-case
        (read-from-string form-string)
      (reader-error (c)
	(declare (ignore c))
	nil)
      (stream-error (c)
        (declare (ignore c))
        nil))))

(defslimefun complete-form (form-string)
  "Read FORM-STRING in the current buffer package, then complete it
by adding a template for the missing arguments."
  (let ((form (read-incomplete-form-from-string form-string)))
    (when (consp form)
      (let ((operator-form (first form))
            (argument-forms (rest form)))
        (let ((form-completion
               (form-completion operator-form argument-forms)))
          (unless (eql form-completion :not-available)
            (return-from complete-form
              (decoded-arglist-to-template-string form-completion
                                                  *buffer-package*
                                                  :prefix ""))))))
    :not-available))

(defun format-arglist-for-echo-area (form operator-name
                                     &key print-right-margin print-lines
                                     highlight)
  "Return the arglist for FORM as a string."
  (when (consp form)
    (destructuring-bind (operator-form &rest argument-forms)
        form
      (let ((form-completion 
             (form-completion operator-form argument-forms
                              :remove-args nil)))
        (unless (eql form-completion :not-available)
          (return-from format-arglist-for-echo-area
            (decoded-arglist-to-string 
             form-completion
             *package*
             :operator operator-name
             :print-right-margin print-right-margin
             :print-lines print-lines
             :highlight highlight))))))
  nil)

(defun keywords-of-operator (operator)
  "Return a list of KEYWORD-ARGs that OPERATOR accepts.
This function is useful for writing EXTRA-KEYWORDS methods for
user-defined functions which are declared &ALLOW-OTHER-KEYS and which
forward keywords to OPERATOR."
  (let ((arglist (form-completion operator nil 
                                  :remove-args nil)))
    (unless (eql arglist :not-available)
      (values 
       (arglist.keyword-args arglist)
       (arglist.allow-other-keys-p arglist)))))

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

(defslimefun completions-for-keyword (names keyword-string arg-indices)
  (with-buffer-syntax ()
    (multiple-value-bind (name index)
        (find-valid-operator-name names)
      (when name
        (let* ((form (operator-designator-to-form name))
               (operator-form (first form))
               (argument-forms (rest form))
               (arglist
                (form-completion operator-form argument-forms
                                 :remove-args nil)))
          (unless (eql arglist :not-available)
            (let* ((indices (butlast (reverse (last arg-indices (1+ index)))))
                   (arglist (apply #'arglist-ref arglist operator-form indices)))
              (when (and arglist (arglist-p arglist))
                ;; It would be possible to complete keywords only if we
                ;; are in a keyword position, but it is not clear if we
                ;; want that.
                (let* ((keywords 
                        (mapcar #'keyword-arg.keyword
                                (arglist.keyword-args arglist)))
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
                        (longest-completion completion-set)))))))))))
           

(defun arglist-to-string (arglist package &key print-right-margin highlight)
  (decoded-arglist-to-string (decode-arglist arglist)
                             package
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
    (test '(&key ((function f))) "(&key ((function f)))")))

(test-print-arglist)


;;;; Recording and accessing results of computations

(defvar *record-repl-results* t
  "Non-nil means that REPL results are saved for later lookup.")

(defvar *object-to-presentation-id* 
  (make-weak-key-hash-table :test 'eq)
  "Store the mapping of objects to numeric identifiers")

(defvar *presentation-id-to-object* 
  (make-weak-value-hash-table :test 'eql)
  "Store the mapping of numeric identifiers to objects")

(defun clear-presentation-tables ()
  (clrhash *object-to-presentation-id*)
  (clrhash *presentation-id-to-object*))

(defvar *presentation-counter* 0 "identifier counter")

(defvar *nil-surrogate* (make-symbol "nil-surrogate"))

;; XXX thread safety? [2006-09-13] mb: not in the slightest (fwiw the
;; rest of slime isn't thread safe either), do we really care?
(defun save-presented-object (object)
  "Save OBJECT and return the assigned id.
If OBJECT was saved previously return the old id."
  (let ((object (if (null object) *nil-surrogate* object)))
    ;; We store *nil-surrogate* instead of nil, to distinguish it from
    ;; an object that was garbage collected.
    (or (gethash object *object-to-presentation-id*)
        (let ((id (incf *presentation-counter*)))
          (setf (gethash id *presentation-id-to-object*) object)
          (setf (gethash object *object-to-presentation-id*) id)
          id))))

(defun lookup-presented-object (id)
  "Retrieve the object corresponding to ID.
The secondary value indicates the absence of an entry."
  (etypecase id
    (integer 
     ;; 
     (multiple-value-bind (object foundp)
         (gethash id *presentation-id-to-object*)
       (cond
         ((eql object *nil-surrogate*)
          ;; A stored nil object
          (values nil t))
         ((null object)
          ;; Object that was replaced by nil in the weak hash table
          ;; when the object was garbage collected.
          (values nil nil))
         (t 
          (values object foundp)))))
    (cons
     (destructure-case id
       ((:frame-var thread-id frame index)
        (declare (ignore thread-id)) ; later 
        (handler-case 
            (frame-var-value frame index)
          (t (condition)
            (declare (ignore condition))
            (values nil nil))
          (:no-error (value)
            (values value t))))
       ((:inspected-part part-index)
        (declare (special *inspectee-parts*))
        (if (< part-index (length *inspectee-parts*))
            (values (inspector-nth-part part-index) t)
            (values nil nil)))))))

(defslimefun get-repl-result (id)
  "Get the result of the previous REPL evaluation with ID."
  (multiple-value-bind (object foundp) (lookup-presented-object id)
    (cond (foundp object)
          (t (abort-request "Attempt to access unrecorded object (id ~D)." id)))))

(defslimefun clear-repl-results ()
  "Forget the results of all previous REPL evaluations."
  (clear-presentation-tables)
  t)


;;;; Evaluation

(defvar *pending-continuations* '()
  "List of continuations for Emacs. (thread local)")

(defun guess-buffer-package (string)
  "Return a package for STRING. 
Fall back to the the current if no such package exists."
  (or (and string (guess-package string))
      *package*))

(defun eval-for-emacs (form buffer-package id)
  "Bind *BUFFER-PACKAGE* BUFFER-PACKAGE and evaluate FORM.
Return the result to the continuation ID.
Errors are trapped and invoke our debugger."
  (call-with-debugger-hook
   #'swank-debugger-hook
   (lambda ()
     (let (ok result reason)
       (unwind-protect
            (let ((*buffer-package* (guess-buffer-package buffer-package))
                  (*buffer-readtable* (guess-buffer-readtable buffer-package))
                  (*pending-continuations* (cons id *pending-continuations*)))
              (check-type *buffer-package* package)
              (check-type *buffer-readtable* readtable)
              ;; APPLY would be cleaner than EVAL. 
              ;;(setq result (apply (car form) (cdr form)))
              (handler-case
                  (progn
                    (setq result (eval form))
                    (run-hook *pre-reply-hook*)
                    (finish-output)
                    (setq ok t))
                (request-abort (c)
                  (setf ok nil
                        reason (list (slot-value c 'swank-backend::reason))))))
         (force-user-output)
         (send-to-emacs `(:return ,(current-thread)
                                  ,(if ok
                                       `(:ok ,result)
                                       `(:abort ,@reason)) 
                                  ,id)))))))

(defvar *echo-area-prefix* "=> "
  "A prefix that `format-values-for-echo-area' should use.")

(defun format-values-for-echo-area (values)
  (with-buffer-syntax ()
    (let ((*print-readably* nil))
      (cond ((null values) "; No value")
            ((and (null (cdr values)) (integerp (car values)))
             (let ((i (car values)))
               (format nil "~A~D (#x~X, #o~O, #b~B)" 
                       *echo-area-prefix* i i i i)))
            (t (with-output-to-string (s)
                 (pprint-logical-block (s () :prefix *echo-area-prefix*)
                   (format s "~{~S~^, ~}" values))))))))

(defslimefun interactive-eval (string)
  (with-buffer-syntax ()
    (let ((values (multiple-value-list (eval (from-string string)))))
      (fresh-line)
      (finish-output)
      (format-values-for-echo-area values))))

(defslimefun eval-and-grab-output (string)
  (with-buffer-syntax ()
    (let* ((s (make-string-output-stream))
           (*standard-output* s)
           (values (multiple-value-list (eval (from-string string)))))
      (list (get-output-stream-string s) 
            (format nil "~{~S~^~%~}" values)))))

;;; XXX do we need this stuff?  What is it good for?
(defvar *slime-repl-advance-history* nil 
  "In the dynamic scope of a single form typed at the repl, is set to nil to 
   prevent the repl from advancing the history - * ** *** etc.")

(defvar *slime-repl-suppress-output* nil
  "In the dynamic scope of a single form typed at the repl, is set to nil to
   prevent the repl from printing the result of the evalation.")
  
(defvar *slime-repl-eval-hook-pass* (gensym "PASS")
  "Token to indicate that a repl hook declines to evaluate the form")

(defvar *slime-repl-eval-hooks* nil
  "A list of functions. When the repl is about to eval a form, first try running each of
   these hooks. The first hook which returns a value which is not *slime-repl-eval-hook-pass*
   is considered a replacement for calling eval. If there are no hooks, or all
   pass, then eval is used.")

(defslimefun repl-eval-hook-pass ()
  "call when repl hook declines to evaluate the form"
  (throw *slime-repl-eval-hook-pass* *slime-repl-eval-hook-pass*))

(defslimefun repl-suppress-output ()
  "In the dynamic scope of a single form typed at the repl, call to
   prevent the repl from printing the result of the evalation."
  (setq *slime-repl-suppress-output* t))

(defslimefun repl-suppress-advance-history ()
  "In the dynamic scope of a single form typed at the repl, call to 
   prevent the repl from advancing the history - * ** *** etc."
  (setq *slime-repl-advance-history* nil))

(defun eval-region (string &optional package-update-p)
  "Evaluate STRING and return the result.
If PACKAGE-UPDATE-P is non-nil, and evaluation causes a package
change, then send Emacs an update."
  (unwind-protect
       (with-input-from-string (stream string)
         (let (- values)
           (loop
            (let ((form (read stream nil stream)))
              (when (eq form stream)
                (fresh-line)
                (finish-output)
                (return (values values -)))
              (setq - form)
	      (if *slime-repl-eval-hooks* 
                  (setq values (run-repl-eval-hooks form))
                  (setq values (multiple-value-list (eval form))))
              (finish-output)))))
    (when (and package-update-p (not (eq *package* *buffer-package*)))
      (send-to-emacs 
       (list :new-package (package-name *package*)
             (package-string-for-prompt *package*))))))

(defun run-repl-eval-hooks (form)
  (loop for hook in *slime-repl-eval-hooks* 
     for res =  (catch *slime-repl-eval-hook-pass* 
                  (multiple-value-list (funcall hook form)))
     until (not (eq res *slime-repl-eval-hook-pass*))
     finally (return 
               (if (eq res *slime-repl-eval-hook-pass*)
                   (multiple-value-list (eval form))
                   res))))

(defun package-string-for-prompt (package)
  "Return the shortest nickname (or canonical name) of PACKAGE."
  (unparse-name
   (or (canonical-package-nickname package)
       (auto-abbreviated-package-name package)
       (shortest-package-nickname package))))

(defun canonical-package-nickname (package)
  "Return the canonical package nickname, if any, of PACKAGE."
  (let ((name (cdr (assoc (package-name package) *canonical-package-nicknames* 
                          :test #'string=))))
    (and name (string name))))

(defun auto-abbreviated-package-name (package)
  "Return an abbreviated 'name' for PACKAGE. 

N.B. this is not an actual package name or nickname."
  (when *auto-abbreviate-dotted-packages*
    (let ((last-dot (position #\. (package-name package) :from-end t)))
      (when last-dot (subseq (package-name package) (1+ last-dot))))))

(defun shortest-package-nickname (package)
  "Return the shortest nickname (or canonical name) of PACKAGE."
  (loop for name in (cons (package-name package) (package-nicknames package))
        for shortest = name then (if (< (length name) (length shortest))
                                   name
                                   shortest)
              finally (return shortest)))

(defslimefun interactive-eval-region (string)
  (with-buffer-syntax ()
    (format-values-for-echo-area (eval-region string))))

(defslimefun re-evaluate-defvar (form)
  (with-buffer-syntax ()
    (let ((form (read-from-string form)))
      (destructuring-bind (dv name &optional value doc) form
	(declare (ignore value doc))
	(assert (eq dv 'defvar))
	(makunbound name)
	(prin1-to-string (eval form))))))

(defvar *swank-pprint-bindings*
  `((*print-pretty*   . t) 
    (*print-level*    . nil)
    (*print-length*   . nil)
    (*print-circle*   . t)
    (*print-gensym*   . t)
    (*print-readably* . nil))
  "A list of variables bindings during pretty printing.
Used by pprint-eval.")

(defun swank-pprint (list)
  "Bind some printer variables and pretty print each object in LIST."
  (with-buffer-syntax ()
    (with-bindings *swank-pprint-bindings*
      (cond ((null list) "; No value")
            (t (with-output-to-string (*standard-output*)
                 (dolist (o list)
                   (pprint o)
                   (terpri))))))))
  
(defslimefun pprint-eval (string)
  (with-buffer-syntax ()
    (swank-pprint (multiple-value-list (eval (read-from-string string))))))

(defslimefun set-package (name)
  "Set *package* to the package named NAME.
Return the full package-name and the string to use in the prompt."
  (let ((p (guess-package name)))
    (assert (packagep p))
    (setq *package* p)
    (list (package-name p) (package-string-for-prompt p))))

(defun send-repl-results-to-emacs (values)
  (flet ((send (value)
           (let ((id (and *record-repl-results*
                          (save-presented-object value))))
             (send-to-emacs `(:write-string ,(prin1-to-string value)
                              ,id :repl-result))
             (send-to-emacs `(:write-string ,(string #\Newline) 
                              nil :repl-result)))))
    (if (null values)
        (send-to-emacs `(:write-string "; No value" nil :repl-result))
        (mapc #'send values))))

(defslimefun listener-eval (string)
  (clear-user-input)
  (with-buffer-syntax ()
    (let ((*slime-repl-suppress-output* :unset)
	  (*slime-repl-advance-history* :unset))
      (multiple-value-bind (values last-form) (eval-region string t)
	(unless (or (and (eq values nil) (eq last-form nil))
		    (eq *slime-repl-advance-history* nil))
	  (setq *** **  ** *  * (car values)
		/// //  // /  / values))
	(setq +++ ++  ++ +  + last-form)
        (unless (eq *slime-repl-suppress-output* t)
          (send-repl-results-to-emacs values)))))
  nil)

(defslimefun ed-in-emacs (&optional what)
  "Edit WHAT in Emacs.

WHAT can be:
  A pathname or a string,
  A list (PATHNAME-OR-STRING LINE [COLUMN]),
  A function name (symbol or cons),
  NIL.

Returns true if it actually called emacs, or NIL if not."
  (flet ((pathname-or-string-p (thing)
           (or (pathnamep thing) (typep thing 'string))))
    (let ((target
           (cond ((and (listp what) (pathname-or-string-p (first what)))
                  (cons (canonicalize-filename (car what)) (cdr what)))
                 ((pathname-or-string-p what)
                  (canonicalize-filename what))
                 ((symbolp what) what)
                 ((consp what) what)
                 (t (return-from ed-in-emacs nil)))))
      (cond
        (*emacs-connection* (send-oob-to-emacs `(:ed ,target)))
        ((default-connection)
         (with-connection ((default-connection))
           (send-oob-to-emacs `(:ed ,target))))
        (t nil)))))

(defslimefun inspect-in-emacs (what)
  "Inspect WHAT in Emacs."
  (flet ((send-it ()
           (with-buffer-syntax ()
             (reset-inspector)
             (send-oob-to-emacs `(:inspect ,(inspect-object what))))))
    (cond 
      (*emacs-connection*
       (send-it))
      ((default-connection)
       (with-connection ((default-connection))
         (send-it))))
    what))

(defslimefun value-for-editing (form)
  "Return a readable value of FORM for editing in Emacs.
FORM is expected, but not required, to be SETF'able."
  ;; FIXME: Can we check FORM for setfability? -luke (12/Mar/2005)
  (with-buffer-syntax ()
    (prin1-to-string (eval (read-from-string form)))))

(defslimefun commit-edited-value (form value)
  "Set the value of a setf'able FORM to VALUE.
FORM and VALUE are both strings from Emacs."
  (with-buffer-syntax ()
    (eval `(setf ,(read-from-string form) 
            ,(read-from-string (concatenate 'string "`" value))))
    t))

(defun background-message  (format-string &rest args)
  "Display a message in Emacs' echo area.

Use this function for informative messages only.  The message may even
be dropped, if we are too busy with other things."
  (when *emacs-connection*
    (send-to-emacs `(:background-message 
                     ,(apply #'format nil format-string args)))))


;;;; Debugger

(defun swank-debugger-hook (condition hook)
  "Debugger function for binding *DEBUGGER-HOOK*.
Sends a message to Emacs declaring that the debugger has been entered,
then waits to handle further requests from Emacs. Eventually returns
after Emacs causes a restart to be invoked."
  (declare (ignore hook))
  (cond (*emacs-connection*
         (debug-in-emacs condition))
        ((default-connection)
         (with-connection ((default-connection))
           (debug-in-emacs condition)))))

(defvar *global-debugger* t
  "Non-nil means the Swank debugger hook will be installed globally.")

(add-hook *new-connection-hook* 'install-debugger)
(defun install-debugger (connection)
  (declare (ignore connection))
  (when *global-debugger*
    (install-debugger-globally #'swank-debugger-hook)))

;;;;; Debugger loop
;;;
;;; These variables are dynamically bound during debugging.
;;;
(defvar *swank-debugger-condition* nil
  "The condition being debugged.")

(defvar *sldb-level* 0
  "The current level of recursive debugging.")

(defvar *sldb-initial-frames* 20
  "The initial number of backtrace frames to send to Emacs.")

(defvar *sldb-restarts* nil
  "The list of currenlty active restarts.")

(defvar *sldb-stepping-p* nil
  "True during execution of a step command.")

(defun debug-in-emacs (condition)
  (let ((*swank-debugger-condition* condition)
        (*sldb-restarts* (compute-restarts condition))
        (*package* (or (and (boundp '*buffer-package*)
                            (symbol-value '*buffer-package*))
                       *package*))
        (*sldb-level* (1+ *sldb-level*))
        (*sldb-stepping-p* nil)
        (*swank-state-stack* (cons :swank-debugger-hook *swank-state-stack*)))
    (force-user-output)
    (call-with-debugging-environment
     (lambda () 
       (with-bindings *sldb-printer-bindings*
         (sldb-loop *sldb-level*))))))

(defun sldb-loop (level)
  (unwind-protect
       (catch 'sldb-enter-default-debugger
         (send-to-emacs 
          (list* :debug (current-thread) level
                 (debugger-info-for-emacs 0 *sldb-initial-frames*)))
         (loop (catch 'sldb-loop-catcher
                 (with-simple-restart (abort "Return to sldb level ~D." level)
                   (send-to-emacs (list :debug-activate (current-thread)
                                        level))
                   (handler-bind ((sldb-condition #'handle-sldb-condition))
                     (read-from-emacs))))))
    (send-to-emacs `(:debug-return
                     ,(current-thread) ,level ,*sldb-stepping-p*))))

(defun handle-sldb-condition (condition)
  "Handle an internal debugger condition.
Rather than recursively debug the debugger (a dangerous idea!), these
conditions are simply reported."
  (let ((real-condition (original-condition condition)))
    (send-to-emacs `(:debug-condition ,(current-thread)
                                      ,(princ-to-string real-condition))))
  (throw 'sldb-loop-catcher nil))

(defun safe-condition-message (condition)
  "Safely print condition to a string, handling any errors during
printing."
  (let ((*print-pretty* t))
    (handler-case
        (format-sldb-condition condition)
      (error (cond)
        ;; Beware of recursive errors in printing, so only use the condition
        ;; if it is printable itself:
        (format nil "Unable to display error condition~@[: ~A~]"
                (ignore-errors (princ-to-string cond)))))))

(defun debugger-condition-for-emacs ()
  (list (safe-condition-message *swank-debugger-condition*)
        (format nil "   [Condition of type ~S]"
                (type-of *swank-debugger-condition*))
        (condition-references *swank-debugger-condition*)
        (condition-extras *swank-debugger-condition*)))

(defun format-restarts-for-emacs ()
  "Return a list of restarts for *swank-debugger-condition* in a
format suitable for Emacs."
  (let ((*print-right-margin* most-positive-fixnum))
    (loop for restart in *sldb-restarts*
          collect (list (princ-to-string (restart-name restart))
                        (princ-to-string restart)))))


;;;;; SLDB entry points

(defslimefun sldb-break-with-default-debugger ()
  "Invoke the default debugger by returning from our debugger-loop."
  (throw 'sldb-enter-default-debugger nil))

(defslimefun backtrace (start end)
  "Return a list ((I FRAME) ...) of frames from START to END.
I is an integer describing and FRAME a string."
  (loop for frame in (compute-backtrace start end)
        for i from start
        collect (list i (with-output-to-string (stream)
                          (print-frame frame stream)))))

(defslimefun debugger-info-for-emacs (start end)
  "Return debugger state, with stack frames from START to END.
The result is a list:
  (condition ({restart}*) ({stack-frame}*) (cont*))
where
  condition   ::= (description type [extra])
  restart     ::= (name description)
  stack-frame ::= (number description)
  extra       ::= (:references and other random things)
  cont        ::= continutation
condition---a pair of strings: message, and type.  If show-source is
not nil it is a frame number for which the source should be displayed.

restart---a pair of strings: restart name, and description.

stack-frame---a number from zero (the top), and a printed
representation of the frame's call.

continutation---the id of a pending Emacs continuation.

Below is an example return value. In this case the condition was a
division by zero (multi-line description), and only one frame is being
fetched (start=0, end=1).

 ((\"Arithmetic error DIVISION-BY-ZERO signalled.
Operation was KERNEL::DIVISION, operands (1 0).\"
   \"[Condition of type DIVISION-BY-ZERO]\")
  ((\"ABORT\" \"Return to Slime toplevel.\")
   (\"ABORT\" \"Return to Top-Level.\"))
  ((0 \"(KERNEL::INTEGER-/-INTEGER 1 0)\"))
  (4))"
  (list (debugger-condition-for-emacs)
        (format-restarts-for-emacs)
        (backtrace start end)
        *pending-continuations*))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

(defslimefun invoke-nth-restart (index)
  (invoke-restart-interactively (nth-restart index)))

(defslimefun sldb-abort ()
  (invoke-restart (find 'abort *sldb-restarts* :key #'restart-name)))

(defslimefun sldb-continue ()
  (continue))

(defslimefun throw-to-toplevel ()
  "Invoke the ABORT-REQUEST restart abort an RPC from Emacs.
If we are not evaluating an RPC then ABORT instead."
  (let ((restart (find-restart *sldb-quit-restart*)))
    (cond (restart (invoke-restart restart))
          (t (format nil
                     "Restart not found: ~a"
                     *sldb-quit-restart*)))))

(defslimefun invoke-nth-restart-for-emacs (sldb-level n)
  "Invoke the Nth available restart.
SLDB-LEVEL is the debug level when the request was made. If this
has changed, ignore the request."
  (when (= sldb-level *sldb-level*)
    (invoke-nth-restart n)))

(defun wrap-sldb-vars (form)
  `(let ((*sldb-level* ,*sldb-level*))
     ,form))

(defslimefun eval-string-in-frame (string index)
  (to-string (eval-in-frame (wrap-sldb-vars (from-string string))
                            index)))

(defslimefun pprint-eval-string-in-frame (string index)
  (swank-pprint
   (multiple-value-list 
    (eval-in-frame (wrap-sldb-vars (from-string string)) index))))

(defslimefun frame-locals-for-emacs (index)
  "Return a property list ((&key NAME ID VALUE) ...) describing
the local variables in the frame INDEX."
  (mapcar (lambda (frame-locals)
            (destructuring-bind (&key name id value) frame-locals
              (list :name (prin1-to-string name) :id id
                    :value (to-string value))))
          (frame-locals index)))

(defslimefun frame-catch-tags-for-emacs (frame-index)
  (mapcar #'to-string (frame-catch-tags frame-index)))

(defslimefun sldb-disassemble (index)
  (with-output-to-string (*standard-output*)
    (disassemble-frame index)))

(defslimefun sldb-return-from-frame (index string)
  (let ((form (from-string string)))
    (to-string (multiple-value-list (return-from-frame index form)))))

(defslimefun sldb-break (name)
  (with-buffer-syntax ()
    (sldb-break-at-start (read-from-string name))))

(defmacro define-stepper-function (name backend-function-name)
  `(defslimefun ,name (frame)
     (cond ((sldb-stepper-condition-p *swank-debugger-condition*)
            (setq *sldb-stepping-p* t)
            (,backend-function-name))
           ((find-restart 'continue)
         (activate-stepping frame)
         (setq *sldb-stepping-p* t)
         (continue))
        (t
            (error "Not currently single-stepping, and no continue restart available.")))))

(define-stepper-function sldb-step sldb-step-into)
(define-stepper-function sldb-next sldb-step-next)
(define-stepper-function sldb-out  sldb-step-out)


;;;; Compilation Commands.

(defvar *compiler-notes* '()
  "List of compiler notes for the last compilation unit.")

(defun clear-compiler-notes ()  
  (setf *compiler-notes* '()))

(defun canonicalize-filename (filename)
  (namestring (truename filename)))

(defslimefun compiler-notes-for-emacs ()
  "Return the list of compiler notes for the last compilation unit."
  (reverse *compiler-notes*))

(defun measure-time-interval (fn)
  "Call FN and return the first return value and the elapsed time.
The time is measured in microseconds."
  (declare (type function fn))
  (let ((before (get-internal-real-time)))
    (values
     (funcall fn)
     (* (- (get-internal-real-time) before)
        (/ 1000000 internal-time-units-per-second)))))

(defun record-note-for-condition (condition)
  "Record a note for a compiler-condition."
  (push (make-compiler-note condition) *compiler-notes*))

(defun make-compiler-note (condition)
  "Make a compiler note data structure from a compiler-condition."
  (declare (type compiler-condition condition))
  (list* :message (message condition)
         :severity (severity condition)
         :location (location condition)
         :references (references condition)
         (let ((s (short-message condition)))
           (if s (list :short-message s)))))

(defun swank-compiler (function)
  (clear-compiler-notes)
  (with-simple-restart (abort "Abort SLIME compilation.")
    (multiple-value-bind (result usecs)
        (handler-bind ((compiler-condition #'record-note-for-condition))
          (measure-time-interval function))
      (list (to-string result)
            (format nil "~,2F" (/ usecs 1000000.0))))))

(defslimefun compile-file-for-emacs (filename load-p)
  "Compile FILENAME and, when LOAD-P, load the result.
Record compiler notes signalled as `compiler-condition's."
  (with-buffer-syntax ()
    (let ((*compile-print* nil))
      (swank-compiler 
       (lambda ()
         (swank-compile-file filename load-p
                             (or (guess-external-format filename)
                                 :default)))))))

(defslimefun compile-string-for-emacs (string buffer position directory)
  "Compile STRING (exerpted from BUFFER at POSITION).
Record compiler notes signalled as `compiler-condition's."
  (with-buffer-syntax ()
    (swank-compiler
     (lambda () 
       (let ((*compile-print* nil) (*compile-verbose* t))
         (swank-compile-string string :buffer buffer :position position 
                               :directory directory))))))

(defslimefun operate-on-system-for-emacs (system-name operation &rest keywords)
  "Compile and load SYSTEM using ASDF.
Record compiler notes signalled as `compiler-condition's."
  (swank-compiler 
   (lambda ()
     (apply #'operate-on-system system-name operation keywords))))

(defun asdf-central-registry ()
  (when (find-package :asdf)
    (symbol-value (find-symbol (string :*central-registry*) :asdf))))

(defslimefun list-all-systems-in-central-registry ()
  "Returns a list of all systems in ASDF's central registry."
  (mapcar #'pathname-name
          (delete-duplicates
           (loop for dir in (asdf-central-registry)
                 for defaults = (eval dir)
                 when defaults
                   nconc (mapcar #'file-namestring
                                   (directory
                                     (make-pathname :defaults defaults
                                          :version :newest
                                          :type "asd"
                                          :name :wild
                                          :case :local))))
           :test #'string=)))

(defslimefun list-all-systems-known-to-asdf ()
  "Returns a list of all systems ASDF knows already."
  (unless (find-package :asdf)
    (error "ASDF not loaded"))
  ;; ugh, yeah, it's unexported - but do we really expect this to
  ;; change anytime soon?
  (loop for name being the hash-keys of (read-from-string 
                                         "#.asdf::*defined-systems*")
        collect name))

(defslimefun list-asdf-systems ()
  "Returns the systems in ASDF's central registry and those which ASDF
already knows."
  (nunion (list-all-systems-known-to-asdf)
          (list-all-systems-in-central-registry)
          :test #'string=))
  
(defun file-newer-p (new-file old-file)
  "Returns true if NEW-FILE is newer than OLD-FILE."
  (> (file-write-date new-file) (file-write-date old-file)))

(defun requires-compile-p (source-file)
  (let ((fasl-file (probe-file (compile-file-pathname source-file))))
    (or (not fasl-file)
        (file-newer-p source-file fasl-file))))

(defslimefun compile-file-if-needed (filename loadp)
  (cond ((requires-compile-p filename)
         (compile-file-for-emacs filename loadp))
        (loadp
         (load (compile-file-pathname filename))
         nil)))


;;;; Loading

(defslimefun load-file (filename)
  (to-string (load filename)))

(defslimefun load-file-set-package (filename &optional package)
  (load-file filename)
  (if package
      (set-package package)))


;;;; Macroexpansion

(defvar *macroexpand-printer-bindings*
  '((*print-circle* . nil)
    (*print-pretty* . t)
    (*print-escape* . t)
    (*print-lines* . nil)
    (*print-level* . nil)
    (*print-length* . nil)))

(defun apply-macro-expander (expander string)
  (with-buffer-syntax ()
    (with-bindings *macroexpand-printer-bindings*
      (prin1-to-string (funcall expander (from-string string))))))

(defslimefun swank-macroexpand-1 (string)
  (apply-macro-expander #'macroexpand-1 string))

(defslimefun swank-macroexpand (string)
  (apply-macro-expander #'macroexpand string))

(defslimefun swank-macroexpand-all (string)
  (apply-macro-expander #'macroexpand-all string))

(defslimefun swank-compiler-macroexpand-1 (string)
  (apply-macro-expander #'compiler-macroexpand-1 string))

(defslimefun swank-compiler-macroexpand (string)
  (apply-macro-expander #'compiler-macroexpand string))

(defslimefun disassemble-symbol (name)
  (with-buffer-syntax ()
    (with-output-to-string (*standard-output*)
      (let ((*print-readably* nil))
        (disassemble (fdefinition (from-string name)))))))


;;;; Basic completion

(defslimefun completions (string default-package-name)
  "Return a list of completions for a symbol designator STRING.  

The result is the list (COMPLETION-SET
COMPLETED-PREFIX). COMPLETION-SET is the list of all matching
completions, and COMPLETED-PREFIX is the best (partial)
completion of the input string.

If STRING is package qualified the result list will also be
qualified.  If string is non-qualified the result strings are
also not qualified and are considered relative to
DEFAULT-PACKAGE-NAME.

The way symbols are matched depends on the symbol designator's
format. The cases are as follows:
  FOO      - Symbols with matching prefix and accessible in the buffer package.
  PKG:FOO  - Symbols with matching prefix and external in package PKG.
  PKG::FOO - Symbols with matching prefix and accessible in package PKG."
  (let ((completion-set (completion-set string default-package-name 
                                        #'compound-prefix-match)))
    (list completion-set (longest-completion completion-set))))

(defslimefun simple-completions (string default-package-name)
  "Return a list of completions for a symbol designator STRING."
  (let ((completion-set (completion-set string default-package-name 
                                        #'prefix-match-p)))
    (list completion-set (longest-common-prefix completion-set))))

;;;;; Find completion set

(defun completion-set (string default-package-name matchp)
  "Return the set of completion-candidates as strings."
  (multiple-value-bind (name package-name package internal-p)
      (parse-completion-arguments string default-package-name)
    (let* ((symbols (mapcar (completion-output-symbol-converter name)
                            (and package
                                 (mapcar #'symbol-name
                                         (find-matching-symbols name
                                                                package
                                                                (and (not internal-p)
                                                                     package-name)
                                                                matchp)))))
           (packs (mapcar (completion-output-package-converter name)
                          (and (not package-name)
                               (find-matching-packages name matchp)))))
      (format-completion-set (nconc symbols packs) internal-p package-name))))

(defun find-matching-symbols (string package external test)
  "Return a list of symbols in PACKAGE matching STRING.
TEST is called with two strings.  If EXTERNAL is true, only external
symbols are returned."
  (let ((completions '())
        (converter (completion-output-symbol-converter string)))
    (flet ((symbol-matches-p (symbol)
             (and (or (not external)
                      (symbol-external-p symbol package))
                  (funcall test string
                           (funcall converter (symbol-name symbol))))))
      (do-symbols* (symbol package) 
        (when (symbol-matches-p symbol)
          (push symbol completions))))
    completions))

(defun find-matching-symbols-in-list (string list test)
  "Return a list of symbols in LIST matching STRING.
TEST is called with two strings."
  (let ((completions '())
        (converter (completion-output-symbol-converter string)))
    (flet ((symbol-matches-p (symbol)
             (funcall test string
                      (funcall converter (symbol-name symbol)))))
      (dolist (symbol list) 
        (when (symbol-matches-p symbol)
          (push symbol completions))))
    (remove-duplicates completions)))

(defun symbol-external-p (symbol &optional (package (symbol-package symbol)))
  "True if SYMBOL is external in PACKAGE.
If PACKAGE is not specified, the home package of SYMBOL is used."
  (and package
       (eq (nth-value 1 (find-symbol (symbol-name symbol) package))
           :external)))

(defun find-matching-packages (name matcher)
  "Return a list of package names matching NAME with MATCHER.
MATCHER is a two-argument predicate."
  (let ((to-match (string-upcase name)))
    (remove-if-not (lambda (x) (funcall matcher to-match x))
                   (mapcar (lambda (pkgname)
                             (concatenate 'string pkgname ":"))
                           (loop for package in (list-all-packages)
                                 collect (package-name package)
                                 append (package-nicknames package))))))

(defun parse-completion-arguments (string default-package-name)
  "Parse STRING as a symbol designator.
Return these values:
 SYMBOL-NAME
 PACKAGE-NAME, or nil if the designator does not include an explicit package.
 PACKAGE, the package to complete in
 INTERNAL-P, if the symbol is qualified with `::'."
  (multiple-value-bind (name package-name internal-p)
      (tokenize-symbol string)
    (let ((package (carefully-find-package package-name default-package-name)))
      (values name package-name package internal-p))))

(defun carefully-find-package (name default-package-name)
  "Find the package with name NAME, or DEFAULT-PACKAGE-NAME, or the
*buffer-package*.  NAME and DEFAULT-PACKAGE-NAME can be nil."
  (let ((string (cond ((equal name "") "KEYWORD")
                      (t (or name default-package-name)))))
    (or (and string (guess-package string))
        *buffer-package*)))

;;;;; Format completion results
;;;
;;; We try to format results in the case as inputs. If you complete
;;; `FOO' then your result should include `FOOBAR' rather than
;;; `foobar'.

(defun format-completion-set (strings internal-p package-name)
  "Format a set of completion strings.
Returns a list of completions with package qualifiers if needed."
  (mapcar (lambda (string)
            (format-completion-result string internal-p package-name))
          (sort strings #'string<)))

(defun format-completion-result (string internal-p package-name)
  (let ((prefix (cond (internal-p (format nil "~A::" package-name))
                      (package-name (format nil "~A:" package-name))
                      (t ""))))
    (values (concatenate 'string prefix string)
            (length prefix))))

(defun completion-output-case-converter (input &optional with-escaping-p)
  "Return a function to convert strings for the completion output.
INPUT is used to guess the preferred case."
  (ecase (readtable-case *readtable*)
    (:upcase (cond ((or with-escaping-p
                        (not (some #'lower-case-p input)))
                    #'identity)
                   (t #'string-downcase)))
    (:invert (lambda (output)
               (multiple-value-bind (lower upper) (determine-case output)
                 (cond ((and lower upper) output)
                       (lower (string-upcase output))
                       (upper (string-downcase output))
                       (t output)))))
    (:downcase (cond ((or with-escaping-p
                          (not (some #'upper-case-p input)))
                      #'identity)
                     (t #'string-upcase)))
    (:preserve #'identity)))

(defun completion-output-package-converter (input)
  "Return a function to convert strings for the completion output.
INPUT is used to guess the preferred case."
  (completion-output-case-converter input))

(defun completion-output-symbol-converter (input)
  "Return a function to convert strings for the completion output.
INPUT is used to guess the preferred case. Escape symbols when needed."
  (let ((case-converter (completion-output-case-converter input))
        (case-converter-with-escaping (completion-output-case-converter input t)))
    (lambda (str)
      (if (or (multiple-value-bind (lowercase uppercase)
                  (determine-case str)
                ;; In these readtable cases, symbols with letters from
                ;; the wrong case need escaping
                (case (readtable-case *readtable*)
                  (:upcase   lowercase)
                  (:downcase uppercase)
                  (t         nil)))
              (some (lambda (el)
                      (or (member el '(#\: #\Space #\Newline #\Tab))
                          (multiple-value-bind (macrofun nonterminating)
                              (get-macro-character el)
                            (and macrofun
                                 (not nonterminating)))))
                    str))
          (concatenate 'string "|" (funcall case-converter-with-escaping str) "|")
          (funcall case-converter str)))))


(defun determine-case (string)
  "Return two booleans LOWER and UPPER indicating whether STRING
contains lower or upper case characters."
  (values (some #'lower-case-p string)
          (some #'upper-case-p string)))


;;;;; Compound-prefix matching

(defun compound-prefix-match (prefix target)
  "Return true if PREFIX is a compound-prefix of TARGET.
Viewing each of PREFIX and TARGET as a series of substrings delimited
by hyphens, if each substring of PREFIX is a prefix of the
corresponding substring in TARGET then we call PREFIX a
compound-prefix of TARGET.

Examples:
\(compound-prefix-match \"foo\" \"foobar\") => t
\(compound-prefix-match \"m--b\" \"multiple-value-bind\") => t
\(compound-prefix-match \"m-v-c\" \"multiple-value-bind\") => NIL"
  (declare (type simple-string prefix target))
  (loop for ch across prefix
        with tpos = 0
        always (and (< tpos (length target))
                    (if (char= ch #\-)
                        (setf tpos (position #\- target :start tpos))
                        (char= ch (aref target tpos))))
        do (incf tpos)))

(defun prefix-match-p (prefix string)
  "Return true if PREFIX is a prefix of STRING."
  (not (mismatch prefix string :end2 (min (length string) (length prefix)))))


;;;;; Extending the input string by completion

(defun longest-completion (completions)
  "Return the longest prefix for all COMPLETIONS.
COMPLETIONS is a list of strings."
  (untokenize-completion
   (mapcar #'longest-common-prefix
           (transpose-lists (mapcar #'tokenize-completion completions)))))

(defun tokenize-completion (string)
  "Return all substrings of STRING delimited by #\-."
  (loop with end
        for start = 0 then (1+ end)
        until (> start (length string))
        do (setq end (or (position #\- string :start start) (length string)))
        collect (subseq string start end)))

(defun untokenize-completion (tokens)
  (format nil "~{~A~^-~}" tokens))

(defun longest-common-prefix (strings)
  "Return the longest string that is a common prefix of STRINGS."
  (if (null strings)
      ""
      (flet ((common-prefix (s1 s2)
               (let ((diff-pos (mismatch s1 s2)))
                 (if diff-pos (subseq s1 0 diff-pos) s1))))
        (reduce #'common-prefix strings))))

(defun transpose-lists (lists)
  "Turn a list-of-lists on its side.
If the rows are of unequal length, truncate uniformly to the shortest.

For example:
\(transpose-lists '((ONE TWO THREE) (1 2)))
  => ((ONE 1) (TWO 2))"
  (cond ((null lists) '())
        ((some #'null lists) '())
        (t (cons (mapcar #'car lists)
                 (transpose-lists (mapcar #'cdr lists))))))


;;;;; Completion Tests

(defpackage :swank-completion-test
  (:use))

(let ((*readtable* (copy-readtable *readtable*))
      (p (find-package :swank-completion-test)))
  (intern "foo" p)
  (intern "Foo" p)
  (intern "FOO" p)
  (setf (readtable-case *readtable*) :invert)
  (flet ((names (prefix) 
           (sort (mapcar #'symbol-name
                         (find-matching-symbols prefix p nil #'prefix-match-p))
                 #'string<)))
    (assert (equal '("FOO") (names "f")))
    (assert (equal '("Foo" "foo") (names "F")))
    (assert (equal '("Foo") (names "Fo")))
    (assert (equal '("foo") (names "FO")))))
           
;;;; Fuzzy completion

(defslimefun fuzzy-completions (string default-package-name &key limit time-limit-in-msec)
  "Return an (optionally limited to LIMIT best results) list of
fuzzy completions for a symbol designator STRING.  The list will
be sorted by score, most likely match first.

The result is a list of completion objects, where a completion
object is:
    (COMPLETED-STRING SCORE (&rest CHUNKS) FLAGS)
where a CHUNK is a description of a matched string of characters:
    (OFFSET STRING)
and FLAGS is a list of keywords describing properties of the symbol.
For example, the top result for completing \"mvb\" in a package
that uses COMMON-LISP would be something like:
    (\"multiple-value-bind\" 42.391666 ((0 \"mul\") (9 \"v\") (15 \"b\"))
     (:FBOUNDP :MACRO))

If STRING is package qualified the result list will also be
qualified.  If string is non-qualified the result strings are
also not qualified and are considered relative to
DEFAULT-PACKAGE-NAME.

Which symbols are candidates for matching depends on the symbol
designator's format. The cases are as follows:
  FOO      - Symbols accessible in the buffer package.
  PKG:FOO  - Symbols external in package PKG.
  PKG::FOO - Symbols accessible in package PKG."
  ;; We may send this as elisp [] arrays to spare a coerce here,
  ;; but then the network serialization were slower by handling arrays.
  ;; Instead we limit the number of completions that is transferred
  ;; (the limit is set from emacs).
  (coerce (fuzzy-completion-set string default-package-name
                                :limit limit :time-limit-in-msec time-limit-in-msec)
          'list))

(defun convert-fuzzy-completion-result (result converter
                                        internal-p package-name)
  "Converts a result from the fuzzy completion core into
something that emacs is expecting.  Converts symbols to strings,
fixes case issues, and adds information describing if the symbol
is :bound, :fbound, a :class, a :macro, a :generic-function,
a :special-operator, or a :package."
  (destructuring-bind (symbol-or-name score chunks) result
    (multiple-value-bind (name added-length)
        (format-completion-result
         (if converter
             (funcall converter 
                      (if (symbolp symbol-or-name)
                          (symbol-name symbol-or-name)
                          symbol-or-name))
             symbol-or-name)
         internal-p package-name)
      (list name score
            (mapcar
             #'(lambda (chunk)
                 ;; fix up chunk positions to account for possible
                 ;; added package identifier
                 (list (+ added-length (first chunk))
                       (second chunk))) 
             chunks)
            (loop for flag in '(:boundp :fboundp :generic-function 
                                :class :macro :special-operator
                                :package)
                  if (if (symbolp symbol-or-name)
                         (case flag
                           (:boundp (boundp symbol-or-name))
                           (:fboundp (fboundp symbol-or-name))
                           (:class (find-class symbol-or-name nil))
                           (:macro (macro-function symbol-or-name))
                           (:special-operator
                            (special-operator-p symbol-or-name))
                           (:generic-function
                            (typep (ignore-errors (fdefinition symbol-or-name))
                                   'generic-function)))
                         (case flag
                           (:package (stringp symbol-or-name)
                                     ;; KLUDGE: depends on internal
                                     ;; knowledge that packages are
                                     ;; brought up from the bowels of
                                     ;; the completion algorithm as
                                     ;; strings!
                                     )))
                  collect flag)))))

(defun fuzzy-completion-set (string default-package-name &key limit time-limit-in-msec)
  "Prepares list of completion obajects, sorted by SCORE, of fuzzy
completions of STRING in DEFAULT-PACKAGE-NAME.  If LIMIT is set,
only the top LIMIT results will be returned."
  (declare (type (or null (integer 0 #.(1- most-positive-fixnum))) limit time-limit-in-msec))
  (multiple-value-bind (name package-name package internal-p)
      (parse-completion-arguments string default-package-name)
    (flet ((convert (vector &optional converter)
             (when vector
               (loop for idx below (length vector)
                     for el = (aref vector idx)
                     do (setf (aref vector idx) (convert-fuzzy-completion-result
                                                 el converter internal-p package-name))))))
      (let* ((symbols (and package
                           (fuzzy-find-matching-symbols name
                                                        package
                                                        (and (not internal-p)
                                                             package-name)
                                                        :time-limit-in-msec time-limit-in-msec
                                                        :return-converted-p nil)))
             (packs (and (not package-name)
                         (fuzzy-find-matching-packages name)))
             (results))
        (convert symbols (completion-output-symbol-converter string))
        (convert packs)
        (setf results (sort (concatenate 'vector symbols packs) #'> :key #'second))
        (when (and limit
                   (> limit 0)
                   (< limit (length results)))
          (if (array-has-fill-pointer-p results)
              (setf (fill-pointer results) limit)
              (setf results (make-array limit :displaced-to results))))
        results))))

(defun fuzzy-find-matching-symbols (string package external &key time-limit-in-msec return-converted-p)
  "Return a list of symbols in PACKAGE matching STRING using the
fuzzy completion algorithm.  If EXTERNAL is true, only external
symbols are returned."
  (let ((completions (make-array 256 :adjustable t :fill-pointer 0))
        (time-limit (if time-limit-in-msec
                        (ceiling (/ time-limit-in-msec 1000))
                        0))
        (utime-at-start (get-universal-time))
        (count 0)
        (converter (completion-output-symbol-converter string)))
    (declare (type (integer 0 #.(1- most-positive-fixnum)) count time-limit)
             (type function converter))
    (flet ((symbol-match (symbol converted)
             (and (or (not external)
                      (symbol-external-p symbol package))
                  (compute-highest-scoring-completion
                   string converted))))
      (block loop
        (do-symbols* (symbol package)
          (incf count)
          (when (and (not (zerop time-limit))
                     (zerop (mod count 256))  ; ease up on calling get-universal-time like crazy
                     (>= (- (get-universal-time) utime-at-start) time-limit))
            (return-from loop))
          (let* ((converted (funcall converter (symbol-name symbol)))
                 (result (if return-converted-p converted symbol)))
            (if (string= "" string)
                (when (or (and external (symbol-external-p symbol package))
                          (not external))
                  (vector-push-extend (list result 0.0 (list (list 0 ""))) completions))
                (multiple-value-bind (match-result score) (symbol-match symbol converted)
                  (when match-result
                    (vector-push-extend (list result score match-result) completions)))))))
      completions)))

(defun fuzzy-find-matching-packages (name)
  "Return a list of package names matching NAME using the fuzzy
completion algorithm."
  (let ((converter (completion-output-package-converter name))
        (completions (make-array 32 :adjustable t :fill-pointer 0)))
    (declare ;;(optimize (speed 3))
             (type function converter))  
    (loop for package in (list-all-packages)
          for package-name = (concatenate 'string 
                                          (funcall converter
                                                   (package-name package)) 
                                          ":")
          for (result score) = (multiple-value-list
                                   (compute-highest-scoring-completion
                                    name package-name))
          when result do
          (vector-push-extend (list package-name score result) completions))
    completions))

(defslimefun fuzzy-completion-selected (original-string completion)
  "This function is called by Slime when a fuzzy completion is
selected by the user.  It is for future expansion to make
testing, say, a machine learning algorithm for completion scoring
easier.

ORIGINAL-STRING is the string the user completed from, and
COMPLETION is the completion object (see docstring for
SWANK:FUZZY-COMPLETIONS) corresponding to the completion that the
user selected."
  (declare (ignore original-string completion))
  nil)

;;;;; Fuzzy completion core

(defparameter *fuzzy-recursion-soft-limit* 30
  "This is a soft limit for recursion in
RECURSIVELY-COMPUTE-MOST-COMPLETIONS.  Without this limit,
completing a string such as \"ZZZZZZ\" with a symbol named
\"ZZZZZZZZZZZZZZZZZZZZZZZ\" will result in explosive recursion to
find all the ways it can match.

Most natural language searches and symbols do not have this
problem -- this is only here as a safeguard.")
(declaim (fixnum *fuzzy-recursion-soft-limit*))

(defun compute-highest-scoring-completion (short full)
  "Finds the highest scoring way to complete the abbreviation
SHORT onto the string FULL, using CHAR= as a equality function for
letters.  Returns two values:  The first being the completion
chunks of the high scorer, and the second being the score."
  (let* ((scored-results
          (mapcar #'(lambda (result)
                      (cons (score-completion result short full) result))
                  (compute-most-completions short full)))
         (winner (first (sort scored-results #'> :key #'first))))
    (values (rest winner) (first winner))))

(defun compute-most-completions (short full)
  "Finds most possible ways to complete FULL with the letters in SHORT.
Calls RECURSIVELY-COMPUTE-MOST-COMPLETIONS recursively.  Returns
a list of (&rest CHUNKS), where each CHUNKS is a description of
how a completion matches."
  (let ((*all-chunks* nil))
    (declare (special *all-chunks*))
    (recursively-compute-most-completions short full 0 0 nil nil nil t)
    *all-chunks*))

(defun recursively-compute-most-completions 
    (short full 
     short-index initial-full-index 
     chunks current-chunk current-chunk-pos 
     recurse-p)
  "Recursively (if RECURSE-P is true) find /most/ possible ways
to fuzzily map the letters in SHORT onto FULL, using CHAR= to
determine if two letters match.

A chunk is a list of elements that have matched consecutively.
When consecutive matches stop, it is coerced into a string,
paired with the starting position of the chunk, and pushed onto
CHUNKS.

Whenever a letter matches, if RECURSE-P is true,
RECURSIVELY-COMPUTE-MOST-COMPLETIONS calls itself with a position
one index ahead, to find other possibly higher scoring
possibilities.  If there are less than
*FUZZY-RECURSION-SOFT-LIMIT* results in *ALL-CHUNKS* currently,
this call will also recurse.

Once a word has been completely matched, the chunks are pushed
onto the special variable *ALL-CHUNKS* and the function returns."
  (declare ;(optimize speed)
           (fixnum short-index initial-full-index)
           (simple-string short full)
           (special *all-chunks*))
  (flet ((short-cur () 
           "Returns the next letter from the abbreviation, or NIL
            if all have been used."
           (if (= short-index (length short))
               nil
               (aref short short-index)))
         (add-to-chunk (char pos)
           "Adds the CHAR at POS in FULL to the current chunk,
            marking the start position if it is empty."
           (unless current-chunk
             (setf current-chunk-pos pos))
           (push char current-chunk))
         (collect-chunk ()
           "Collects the current chunk to CHUNKS and prepares for
            a new chunk."
           (when current-chunk
             (push (list current-chunk-pos
                         (coerce (reverse current-chunk) 'string)) chunks)
             (setf current-chunk nil
                   current-chunk-pos nil))))
    ;; If there's an outstanding chunk coming in collect it.  Since
    ;; we're recursively called on skipping an input character, the
    ;; chunk can't possibly continue on.
    (when current-chunk (collect-chunk))
    (do ((pos initial-full-index (1+ pos)))
        ((= pos (length full)))
      (let ((cur-char (aref full pos)))
        (if (and (short-cur) 
                 (char= cur-char (short-cur)))
            (progn
              (when recurse-p
                ;; Try other possibilities, limiting insanely deep
                ;; recursion somewhat.
                (recursively-compute-most-completions 
                 short full short-index (1+ pos) 
                 chunks current-chunk current-chunk-pos
                 (not (> (length *all-chunks*) 
                         *fuzzy-recursion-soft-limit*))))
              (incf short-index)
              (add-to-chunk cur-char pos))
            (collect-chunk))))
    (collect-chunk)
    ;; If we've exhausted the short characters we have a match.
    (if (short-cur)
        nil
        (let ((rev-chunks (reverse chunks)))
          (push rev-chunks *all-chunks*)
          rev-chunks))))

;;;;; Fuzzy completion scoring

(defparameter *fuzzy-completion-symbol-prefixes* "*+-%&?<"
  "Letters that are likely to be at the beginning of a symbol.
Letters found after one of these prefixes will be scored as if
they were at the beginning of ths symbol.")
(defparameter *fuzzy-completion-symbol-suffixes* "*+->"
  "Letters that are likely to be at the end of a symbol.
Letters found before one of these suffixes will be scored as if
they were at the end of the symbol.")
(defparameter *fuzzy-completion-word-separators* "-/."
  "Letters that separate different words in symbols.  Letters
after one of these symbols will be scores more highly than other
letters.")

(defun score-completion (completion short full)
  "Scores the completion chunks COMPLETION as a completion from
the abbreviation SHORT to the full string FULL.  COMPLETION is a
list like:
    ((0 \"mul\") (9 \"v\") (15 \"b\"))
Which, if SHORT were \"mulvb\" and full were \"multiple-value-bind\", 
would indicate that it completed as such (completed letters
capitalized):
    MULtiple-Value-Bind

Letters are given scores based on their position in the string.
Letters at the beginning of a string or after a prefix letter at
the beginning of a string are scored highest.  Letters after a
word separator such as #\- are scored next highest.  Letters at
the end of a string or before a suffix letter at the end of a
string are scored medium, and letters anywhere else are scored
low.

If a letter is directly after another matched letter, and its
intrinsic value in that position is less than a percentage of the
previous letter's value, it will use that percentage instead.

Finally, a small scaling factor is applied to favor shorter
matches, all other things being equal."
  (labels ((at-beginning-p (pos) 
             (= pos 0))
           (after-prefix-p (pos) 
             (and (= pos 1) 
                  (find (aref full 0) *fuzzy-completion-symbol-prefixes*)))
           (word-separator-p (pos)
             (find (aref full pos) *fuzzy-completion-word-separators*))
           (after-word-separator-p (pos)
             (find (aref full (1- pos)) *fuzzy-completion-word-separators*))
           (at-end-p (pos)
             (= pos (1- (length full))))
           (before-suffix-p (pos)
             (and (= pos (- (length full) 2))
                  (find (aref full (1- (length full)))
                        *fuzzy-completion-symbol-suffixes*)))
           (score-or-percentage-of-previous (base-score pos chunk-pos)
             (if (zerop chunk-pos) 
                 base-score 
                 (max base-score 
                      (+ (* (score-char (1- pos) (1- chunk-pos)) 0.85)
                         (expt 1.2 chunk-pos)))))
           (score-char (pos chunk-pos)
             (score-or-percentage-of-previous
              (cond ((at-beginning-p pos)         10)
                    ((after-prefix-p pos)         10)
                    ((word-separator-p pos)       1)
                    ((after-word-separator-p pos) 8)
                    ((at-end-p pos)               6)
                    ((before-suffix-p pos)        6)
                    (t                            1))
              pos chunk-pos))
           (score-chunk (chunk)
             (loop for chunk-pos below (length (second chunk))
                   for pos from (first chunk) 
                   summing (score-char pos chunk-pos))))
    (let* ((chunk-scores (mapcar #'score-chunk completion))
           (length-score (/ 10.0 (1+ (- (length full) (length short))))))
      (values
       (+ (reduce #'+ chunk-scores) length-score)
       (list (mapcar #'list chunk-scores completion) length-score)))))

(defun highlight-completion (completion full)
  "Given a chunk definition COMPLETION and the string FULL,
HIGHLIGHT-COMPLETION will create a string that demonstrates where
the completion matched in the string.  Matches will be
capitalized, while the rest of the string will be lower-case."
  (let ((highlit (nstring-downcase (copy-seq full))))
    (dolist (chunk completion)
      (setf highlit (nstring-upcase highlit 
                                    :start (first chunk)
                                    :end (+ (first chunk) 
                                            (length (second chunk))))))
    highlit))

(defun format-fuzzy-completions (winners)
  "Given a list of completion objects such as on returned by
FUZZY-COMPLETIONS, format the list into user-readable output."
  (let ((max-len 
         (loop for winner in winners maximizing (length (first winner)))))
    (loop for (sym score result) in winners do
          (format t "~&~VA  score ~8,2F  ~A"
                  max-len (highlight-completion result sym) score result))))


;;;; Completion for character names

(defslimefun completions-for-character (prefix)
  (let ((completion-set 
         (sort 
          (character-completion-set prefix 
                                    #'compound-prefix-match/ci/underscores)
          #'string<)))
    (list completion-set (longest-completion/underscores completion-set))))

(defun compound-prefix-match/ci/underscores (prefix target)
  "Like compound-prefix-match, but case-insensitive, and using the underscore, 
not the hyphen, as a delimiter." 
  (declare (type simple-string prefix target))
  (loop for ch across prefix
        with tpos = 0
        always (and (< tpos (length target))
                    (if (char= ch #\_)
                        (setf tpos (position #\_ target :start tpos))
                        (char-equal ch (aref target tpos))))
        do (incf tpos)))

(defun longest-completion/underscores (completions)
  "Return the longest prefix for all COMPLETIONS.
COMPLETIONS is a list of strings."
  (untokenize-completion/underscores
   (mapcar #'longest-common-prefix
           (transpose-lists (mapcar #'tokenize-completion/underscores 
                                    completions)))))

(defun tokenize-completion/underscores (string)
  "Return all substrings of STRING delimited by #\_."
  (loop with end
        for start = 0 then (1+ end)
        until (> start (length string))
        do (setq end (or (position #\_ string :start start) (length string)))
        collect (subseq string start end)))

(defun untokenize-completion/underscores (tokens)
  (format nil "~{~A~^_~}" tokens))


;;;; Documentation

(defslimefun apropos-list-for-emacs  (name &optional external-only 
                                           case-sensitive package)
  "Make an apropos search for Emacs.
The result is a list of property lists."
  (let ((package (if package
                     (or (parse-package package)
                         (error "No such package: ~S" package)))))
    (mapcan (listify #'briefly-describe-symbol-for-emacs)
            (sort (remove-duplicates
                   (apropos-symbols name external-only case-sensitive package))
                  #'present-symbol-before-p))))

(defun briefly-describe-symbol-for-emacs (symbol)
  "Return a property list describing SYMBOL.
Like `describe-symbol-for-emacs' but with at most one line per item."
  (flet ((first-line (string)
           (let ((pos (position #\newline string)))
             (if (null pos) string (subseq string 0 pos)))))
    (let ((desc (map-if #'stringp #'first-line 
                        (describe-symbol-for-emacs symbol))))
      (if desc 
          (list* :designator (to-string symbol) desc)))))

(defun map-if (test fn &rest lists)
  "Like (mapcar FN . LISTS) but only call FN on objects satisfying TEST.
Example:
\(map-if #'oddp #'- '(1 2 3 4 5)) => (-1 2 -3 4 -5)"
  (apply #'mapcar
         (lambda (x) (if (funcall test x) (funcall fn x) x))
         lists))

(defun listify (f)
  "Return a function like F, but which returns any non-null value
wrapped in a list."
  (lambda (x)
    (let ((y (funcall f x)))
      (and y (list y)))))

(defun present-symbol-before-p (x y)
  "Return true if X belongs before Y in a printed summary of symbols.
Sorted alphabetically by package name and then symbol name, except
that symbols accessible in the current package go first."
  (declare (type symbol x y))
  (flet ((accessible (s)
           ;; Test breaks on NIL for package that does not inherit it
           (eq (find-symbol (symbol-name s) *buffer-package*) s)))
    (let ((ax (accessible x)) (ay (accessible y)))
      (cond ((and ax ay) (string< (symbol-name x) (symbol-name y)))
            (ax t)
            (ay nil)
            (t (let ((px (symbol-package x)) (py (symbol-package y)))
                 (if (eq px py)
                     (string< (symbol-name x) (symbol-name y))
                     (string< (package-name px) (package-name py)))))))))

(let ((regex-hash (make-hash-table :test #'equal)))
  (defun compiled-regex (regex-string)
    (or (gethash regex-string regex-hash)
        (setf (gethash regex-string regex-hash)
              (if (zerop (length regex-string))
                  (lambda (s) (check-type s string) t)
                  (compile nil (slime-nregex:regex-compile regex-string)))))))

(defun apropos-matcher (string case-sensitive package external-only)
  (let* ((case-modifier (if case-sensitive #'string #'string-upcase))
         (regex (compiled-regex (funcall case-modifier string))))
    (lambda (symbol)
      (and (not (keywordp symbol))
           (if package (eq (symbol-package symbol) package) t)
           (if external-only (symbol-external-p symbol) t)
           (funcall regex (funcall case-modifier symbol))))))

(defun apropos-symbols (string external-only case-sensitive package)
  (let ((result '())
        (matchp (apropos-matcher string case-sensitive package external-only)))
    (with-package-iterator (next (or package (list-all-packages))
                                 :external :internal)
      (loop
       (multiple-value-bind (morep symbol) (next)
         (cond ((not morep)
                (return))
               ((funcall matchp symbol)
                (push symbol result))))))
    result))

(defun call-with-describe-settings (fn)
  (let ((*print-readably* nil))
    (funcall fn)))

(defmacro with-describe-settings ((&rest _) &body body)
  (declare (ignore _))
  `(call-with-describe-settings (lambda () ,@body)))
    
(defun describe-to-string (object)
  (with-describe-settings ()
    (with-output-to-string (*standard-output*)
      (describe object))))

(defslimefun describe-symbol (symbol-name)
  (with-buffer-syntax ()
    (describe-to-string (parse-symbol-or-lose symbol-name))))

(defslimefun describe-function (name)
  (with-buffer-syntax ()
    (let ((symbol (parse-symbol-or-lose name)))
      (describe-to-string (or (macro-function symbol)
                              (symbol-function symbol))))))

(defslimefun describe-definition-for-emacs (name kind)
  (with-buffer-syntax ()
    (with-describe-settings ()
      (with-output-to-string (*standard-output*)
        (describe-definition (parse-symbol-or-lose name) kind)))))

(defslimefun documentation-symbol (symbol-name &optional default)
  (with-buffer-syntax ()
    (multiple-value-bind (sym foundp) (parse-symbol symbol-name)
      (if foundp
          (let ((vdoc (documentation sym 'variable))
                (fdoc (documentation sym 'function)))
            (or (and (or vdoc fdoc)
                     (concatenate 'string
                                  fdoc
                                  (and vdoc fdoc '(#\Newline #\Newline))
                                  vdoc))
                default))
          default))))


;;;; Package Commands

(defslimefun list-all-package-names (&optional nicknames)
  "Return a list of all package names.
Include the nicknames if NICKNAMES is true."
  (mapcar #'unparse-name
          (loop for package in (list-all-packages)
                collect (package-name package)
                when nicknames append (package-nicknames package))))


;;;; Tracing

;; Use eval for the sake of portability... 
(defun tracedp (fspec)
  (member fspec (eval '(trace))))

(defslimefun swank-toggle-trace (spec-string)
  (let ((spec (from-string spec-string)))
    (cond ((consp spec) ; handle complicated cases in the backend
           (toggle-trace spec))
          ((tracedp spec)
	   (eval `(untrace ,spec))
	   (format nil "~S is now untraced." spec))
	  (t
           (eval `(trace ,spec))
	   (format nil "~S is now traced." spec)))))

(defslimefun untrace-all ()
  (untrace))


;;;; Undefing

(defslimefun undefine-function (fname-string)
  (let ((fname (from-string fname-string)))
    (format nil "~S" (fmakunbound fname))))


;;;; Profiling

(defun profiledp (fspec)
  (member fspec (profiled-functions)))

(defslimefun toggle-profile-fdefinition (fname-string)
  (let ((fname (from-string fname-string)))
    (cond ((profiledp fname)
	   (unprofile fname)
	   (format nil "~S is now unprofiled." fname))
	  (t
           (profile fname)
	   (format nil "~S is now profiled." fname)))))


;;;; Source Locations

(defslimefun find-definitions-for-emacs (name)
  "Return a list ((DSPEC LOCATION) ...) of definitions for NAME.
DSPEC is a string and LOCATION a source location. NAME is a string."
  (multiple-value-bind (sexp error)
      (ignore-errors (values (from-string name)))
    (unless error
      (loop for (dspec loc) in (find-definitions sexp)
         collect (list (to-string dspec) loc)))))

(defun alistify (list key test)
  "Partition the elements of LIST into an alist.  KEY extracts the key
from an element and TEST is used to compare keys."
  (declare (type function key))
  (let ((alist '()))
    (dolist (e list)
      (let* ((k (funcall key e))
	     (probe (assoc k alist :test test)))
	(if probe
	    (push e (cdr probe))
            (push (cons k (list e)) alist))))
    alist))

(defun location-position< (pos1 pos2)
  (cond ((and (position-p pos1) (position-p pos2))
         (< (position-pos pos1)
            (position-pos pos2)))
        (t nil)))

(defun partition (list test key)
  (declare (type function test key))
  (loop for e in list
	if (funcall test (funcall key e)) collect e into yes
	else collect e into no
	finally (return (values yes no))))

(defstruct (xref (:conc-name xref.)
                 (:type list))
  dspec location)

(defun location-valid-p (location)
  (eq (car location) :location))

(defun xref-buffer (xref)
  (location-buffer (xref.location xref)))

(defun xref-position (xref)
  (location-buffer (xref.location xref)))

(defun group-xrefs (xrefs)
  "Group XREFS, a list of the form ((DSPEC LOCATION) ...) by location.
The result is a list of the form ((LOCATION . ((DSPEC . LOCATION) ...)) ...)."
  (multiple-value-bind (resolved errors)
      (partition xrefs #'location-valid-p #'xref.location)
    (let ((alist (alistify resolved #'xref-buffer #'equal)))
      (append
       (loop for (buffer . list) in alist
             collect (cons (second buffer)
                           (mapcar (lambda (xref)
                                     (cons (to-string (xref.dspec xref))
                                           (xref.location xref)))
                                   (sort list #'location-position<
                                         :key #'xref-position))))
       (if errors
           (list (cons "Unresolved"
                       (mapcar (lambda (xref)
                                 (cons (to-string (xref.dspec xref))
                                       (xref.location xref)))
                               errors))))))))

(defslimefun xref (type symbol-name)
  (let ((symbol (parse-symbol-or-lose symbol-name *buffer-package*)))
    (group-xrefs
     (ecase type
       (:calls (who-calls symbol))
       (:calls-who (calls-who symbol))
       (:references (who-references symbol))
       (:binds (who-binds symbol))
       (:sets (who-sets symbol))
       (:macroexpands (who-macroexpands symbol))
       (:specializes (who-specializes symbol))
       (:callers (list-callers symbol))
       (:callees (list-callees symbol))))))


;;;; Inspecting

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

(defmethod inspect-for-emacs ((object cons) inspector)
  (declare (ignore inspector))
  (if (consp (cdr object))
      (inspect-for-emacs-list object)
      (inspect-for-emacs-simple-cons object)))

(defun inspect-for-emacs-simple-cons (cons)
  (values "A cons cell."
          (label-value-line* 
           ('car (car cons))
           ('cdr (cdr cons)))))

(defun inspect-for-emacs-list (list)
  (let ((maxlen 40))
    (multiple-value-bind (length tail) (safe-length list)
      (flet ((frob (title list)
               (let ((lines 
                      (do ((i 0 (1+ i))
                           (l list (cdr l))
                           (a '() (cons (label-value-line i (car l)) a)))
                          ((not (consp l))
                           (let ((a (if (null l)
                                        a
                                        (cons (label-value-line :tail l) a))))
                             (reduce #'append (reverse a) :from-end t))))))
                 (values title (append '("Elements:" (:newline)) lines)))))
                               
        (cond ((not length)             ; circular
               (frob "A circular list."
                     (cons (car list)
                           (ldiff (cdr list) list))))
              ((and (<= length maxlen) (not tail))
               (frob "A proper list." list))
              (tail
               (frob "An improper list." list))
              (t
               (frob "A proper list." list)))))))

;; (inspect-for-emacs-list '#1=(a #1# . #1# ))

(defun safe-length (list)
  "Similar to `list-length', but avoid errors on improper lists.
Return two values: the length of the list and the last cdr.
NIL is returned if the list is circular."
  (do ((n 0 (+ n 2))                    ;Counter.
       (fast list (cddr fast))          ;Fast pointer: leaps by 2.
       (slow list (cdr slow)))          ;Slow pointer: leaps by 1.
      (nil)
    (cond ((null fast) (return (values n nil)))
          ((not (consp fast)) (return (values n fast)))
          ((null (cdr fast)) (return (values (1+ n) (cdr fast))))
          ((and (eq fast slow) (> n 0)) (return nil))
          ((not (consp (cdr fast))) (return (values (1+ n) (cdr fast)))))))

(defvar *slime-inspect-contents-limit* nil "How many elements of
 a hash table or array to show by default. If table has more than
 this then offer actions to view more. Set to nil for no limit." )

(defmethod inspect-for-emacs ((ht hash-table) inspector)
  (declare (ignore inspector))
  (values (prin1-to-string ht)
          (append
           (label-value-line*
            ("Count" (hash-table-count ht))
            ("Size" (hash-table-size ht))
            ("Test" (hash-table-test ht))
            ("Rehash size" (hash-table-rehash-size ht))
            ("Rehash threshold" (hash-table-rehash-threshold ht)))
           (let ((weakness (hash-table-weakness ht)))
             (when weakness
               `("Weakness: " (:value ,weakness) (:newline))))
           (unless (zerop (hash-table-count ht))
             `((:action "[clear hashtable]" ,(lambda () (clrhash ht))) (:newline)
               "Contents: " (:newline)))
	   (if (and *slime-inspect-contents-limit*
		    (>= (hash-table-count ht) *slime-inspect-contents-limit*))
	       (inspect-bigger-piece-actions ht (hash-table-count ht))
	       nil)
           (loop for key being the hash-keys of ht
                 for value being the hash-values of ht
                 repeat (or *slime-inspect-contents-limit* most-positive-fixnum)
                 append `((:value ,key) " = " (:value ,value)
                          " " (:action "[remove entry]"
                               ,(let ((key key))
                                  (lambda () (remhash key ht))))
                          (:newline))))))

(defmethod inspect-bigger-piece-actions (thing size)
  (append 
   (if (> size *slime-inspect-contents-limit*)
       (list (inspect-show-more-action thing)
	     '(:newline))
       nil)
   (list (inspect-whole-thing-action thing  size)
	 '(:newline))))

(defmethod inspect-whole-thing-action (thing size)
  `(:action ,(format nil "Inspect all ~a elements." 
		      size)
	    ,(lambda() 
	       (let ((*slime-inspect-contents-limit* nil))
		 (values
		  (swank::inspect-object thing)
		  :replace)))))

(defmethod inspect-show-more-action (thing)
  `(:action ,(format nil "~a elements shown. Prompt for how many to inspect..." 
		     *slime-inspect-contents-limit* )
	    ,(lambda() 
	       (let ((*slime-inspect-contents-limit* 
		      (progn (format t "How many elements should be shown? ") (read))))
		 (values
		  (swank::inspect-object thing)
		  :replace)))))

(defmethod inspect-for-emacs ((array array) inspector)
  (declare (ignore inspector))
  (values "An array."
          (append
           (label-value-line*
            ("Dimensions" (array-dimensions array))
            ("Its element type is" (array-element-type array))
            ("Total size" (array-total-size array))
            ("Adjustable" (adjustable-array-p array)))
           (when (array-has-fill-pointer-p array)
             (label-value-line "Fill pointer" (fill-pointer array)))
           '("Contents:" (:newline))
           (if (and *slime-inspect-contents-limit*
		    (>= (array-total-size array) *slime-inspect-contents-limit*))
	       (inspect-bigger-piece-actions array  (length array))
	       nil)
           (loop for i below (or *slime-inspect-contents-limit* (array-total-size array))
                 append (label-value-line i (row-major-aref array i))))))

(defmethod inspect-for-emacs ((char character) inspector)
  (declare (ignore inspector))
  (values "A character."
          (append 
           (label-value-line*
            ("Char code" (char-code char))
            ("Lower cased" (char-downcase char))
            ("Upper cased" (char-upcase char)))
           (if (get-macro-character char)
               `("In the current readtable (" 
                 (:value ,*readtable*) ") it is a macro character: "
                 (:value ,(get-macro-character char)))))))

(defun docstring-ispec (label object kind)
  "Return a inspector spec if OBJECT has a docstring of of kind KIND."
  (let ((docstring (documentation object kind)))
    (cond ((not docstring) nil)
	  ((< (+ (length label) (length docstring))
	      75)
	   (list label ": " docstring '(:newline)))
	  (t 
	   (list label ": " '(:newline) "  " docstring '(:newline))))))

(defmethod inspect-for-emacs ((symbol symbol) inspector)
  (declare (ignore inspector))
  (let ((package (symbol-package symbol)))
    (multiple-value-bind (_symbol status) 
	(and package (find-symbol (string symbol) package))
      (declare (ignore _symbol))
      (values 
       "A symbol."
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
	)))))

(defmethod inspect-for-emacs ((f function) inspector)
  (declare (ignore inspector))
  (values "A function."
	  (append 
	   (label-value-line "Name" (function-name f))
	   `("Its argument list is: " 
	     ,(inspector-princ (arglist f)) (:newline))
	   (docstring-ispec "Documentation" f t)
	   (if (function-lambda-expression f)
	       (label-value-line "Lambda Expression"
				 (function-lambda-expression f))))))

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

(defmethod inspect-for-emacs ((object standard-object) inspector)
  (let ((class (class-of object)))
    (values "An object."
            `("Class: " (:value ,class) (:newline)
              ,@(all-slots-for-inspector object inspector)))))

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
                                                     (slime-read-object
                                                      ,(format nil "Set slot ~S to (evaluated) : " slot-name))
                                                     (quit nil)))))
                                (when (and value-string
                                           (not (string= value-string "")))
                                  (setf (swank-mop:slot-value-using-class class object slot)
                                        (eval (read-from-string value-string))))))))
               ,@(when boundp
                   `(" " (:action "[make unbound]"
                          ,(lambda () (swank-mop:slot-makunbound-using-class class object slot)))))))))

(defgeneric all-slots-for-inspector (object inspector)
  (:method ((object standard-object) inspector)
    (declare (ignore inspector))
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

(defmethod inspect-for-emacs ((gf standard-generic-function) inspector)
  (flet ((lv (label value) (label-value-line label value)))
    (values 
     "A generic function."
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
      (all-slots-for-inspector gf inspector)))))

(defmethod inspect-for-emacs ((method standard-method) inspector)
  (values "A method." 
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
            ,@(all-slots-for-inspector method inspector))))

(defmethod inspect-for-emacs ((class standard-class) inspector)
  (values "A class."
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
            ,@(all-slots-for-inspector class inspector))))

(defmethod inspect-for-emacs ((slot swank-mop:standard-slot-definition) inspector)
  (values "A slot."
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
            ,@(all-slots-for-inspector slot inspector))))

(defmethod inspect-for-emacs ((package package) inspector)
  (declare (ignore inspector))
  (let ((internal-symbols '())
        (external-symbols '()))
    (do-symbols (sym package)
      (when (eq package (symbol-package sym))
        (push sym internal-symbols)
        (multiple-value-bind (symbol status)
            (find-symbol (symbol-name sym) package)
          (declare (ignore symbol))
          (when (eql :external status)
            (push sym external-symbols)))))
    (setf internal-symbols (sort internal-symbols #'string-lessp)
          external-symbols (sort external-symbols #'string-lessp))
    (values "A package."
            `("Name: " (:value ,(package-name package))
              (:newline)
              "Nick names: " ,@(common-seperated-spec (sort (copy-seq (package-nicknames package))
                                                            #'string-lessp))
              (:newline)
              ,@(when (documentation package t)
                  `("Documentation:" (:newline)
                    ,(documentation package t) (:newline)))
              "Use list: " ,@(common-seperated-spec (sort (copy-seq (package-use-list package)) #'string-lessp :key #'package-name)
                                                    (lambda (pack)
                                                      `(:value ,pack ,(inspector-princ (package-name pack)))))
              (:newline)
              "Used by list: " ,@(common-seperated-spec (sort (copy-seq (package-used-by-list package)) #'string-lessp :key #'package-name)
                                                        (lambda (pack)
                                                          `(:value ,pack ,(inspector-princ (package-name pack)))))
              (:newline)
              ,(if (null external-symbols)
                   "0 external symbols."
                   `(:value ,external-symbols ,(format nil "~D external symbol~:P." (length external-symbols))))
              (:newline)
              ,(if (null internal-symbols)
                   "0 internal symbols."
                   `(:value ,internal-symbols ,(format nil "~D internal symbol~:P." (length internal-symbols))))
              (:newline)
              ,(if (null (package-shadowing-symbols package))
                   "0 shadowed symbols."
                   `(:value ,(package-shadowing-symbols package)
                            ,(format nil "~D shadowed symbol~:P." (length (package-shadowing-symbols package)))))))))

(defmethod inspect-for-emacs ((pathname pathname) inspector)
  (declare (ignore inspector))
  (values (if (wild-pathname-p pathname)
              "A wild pathname."
              "A pathname.")
          (append (label-value-line*
                   ("Namestring" (namestring pathname))
                   ("Host"       (pathname-host pathname))
                   ("Device"     (pathname-device pathname))
                   ("Directory"  (pathname-directory pathname))
                   ("Name"       (pathname-name pathname))
                   ("Type"       (pathname-type pathname))
                   ("Version"    (pathname-version pathname)))
                  (unless (or (wild-pathname-p pathname)
                              (not (probe-file pathname)))
                    (label-value-line "Truename" (truename pathname))))))

(defmethod inspect-for-emacs ((pathname logical-pathname) inspector)
  (declare (ignore inspector))
  (values "A logical pathname."
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
                            (probe-file pathname)))))))

(defmethod inspect-for-emacs ((n number) inspector)
  (declare (ignore inspector))
  (values "A number." `("Value: " ,(princ-to-string n))))

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

(defmethod inspect-for-emacs ((i integer) inspector)
  (declare (ignore inspector))
  (values "A number."
          (append
           `(,(format nil "Value: ~D = #x~8,'0X = #o~O = #b~,,' ,8:B~@[ = ~E~]"
                      i i i i (ignore-errors (coerce i 'float)))
              (:newline))
           (when (< -1 i char-code-limit)
             (label-value-line "Code-char" (code-char i)))
           (label-value-line "Integer-length" (integer-length i))           
           (ignore-errors
             (label-value-line "Universal-time" (format-iso8601-time i t))))))

(defmethod inspect-for-emacs ((c complex) inspector)
  (declare (ignore inspector))
  (values "A complex number."
          (label-value-line* 
           ("Real part" (realpart c))
           ("Imaginary part" (imagpart c)))))

(defmethod inspect-for-emacs ((r ratio) inspector)
  (declare (ignore inspector))
  (values "A non-integer ratio."
          (label-value-line*
           ("Numerator" (numerator r))
           ("Denominator" (denominator r))
           ("As float" (float r)))))

(defmethod inspect-for-emacs ((f float) inspector)
  (declare (ignore inspector))
  (values "A floating point number."
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
                (label-value-line "Precision" (float-precision f))))))))

(defmethod inspect-for-emacs ((stream file-stream) inspector)
  (declare (ignore inspector))
  (multiple-value-bind (title content)
      (call-next-method)
    (declare (ignore title))
    (values "A file stream."
            (append
             `("Pathname: "
               (:value ,(pathname stream))
               (:newline) "  "
               (:action "[visit file and show current position]"
                        ,(let ((pathname (pathname stream))
                               (position (file-position stream)))
                           (lambda ()
                             (ed-in-emacs `(,pathname :charpos ,position)))))
               (:newline))
             content))))

(defmethod inspect-for-emacs ((condition stream-error) inspector)
  (declare (ignore inspector))
  (multiple-value-bind (title content)
      (call-next-method)
    (let ((stream (stream-error-stream condition)))
      (if (typep stream 'file-stream)
          (values "A stream error."
                  (append
                   `("Pathname: "
                     (:value ,(pathname stream))
                     (:newline) "  "
                     (:action "[visit file and show current position]"
                              ,(let ((pathname (pathname stream))
                                     (position (file-position stream)))
                                    (lambda ()
                                      (ed-in-emacs `(,pathname :charpos ,position)))))
                     (:newline))
                   content))
          (values title content)))))

(defvar *inspectee*)
(defvar *inspectee-parts*) 
(defvar *inspectee-actions*)
(defvar *inspector-stack* '())
(defvar *inspector-history* (make-array 10 :adjustable t :fill-pointer 0))
(declaim (type vector *inspector-history*))
(defvar *inspect-length* 30)

(defun reset-inspector ()
  (setq *inspectee* nil
        *inspector-stack* nil
        *inspectee-parts* (make-array 10 :adjustable t :fill-pointer 0)
        *inspectee-actions* (make-array 10 :adjustable t :fill-pointer 0)
        *inspector-history* (make-array 10 :adjustable t :fill-pointer 0)))

(defun valid-function-name-p (form)
  (or (and (not (null form))
           (not (eq form t))
           (symbolp form))
      (and (consp form)
           (second form)
           (not (third form))
           (eq (first form) 'setf))))

(defvar *inspector-dwim-lookup-hooks* '(default-dwim-inspector-lookup-hook)
  "A list of funcallables with one argument. It can be used to register user hooks that look up various things when inspecting in dwim mode.")

(defun default-dwim-inspector-lookup-hook (form)
  (let ((result '()))
    (when (and (symbolp form)
               (boundp form))
      (push (symbol-value form) result))
    (when (and (valid-function-name-p form)
               (fboundp form))
      (push (fdefinition form) result))
    (when (and (symbolp form)
               (find-class form nil))
      (push (find-class form) result))
    (when (and (consp form)
               (valid-function-name-p (first form))
               (fboundp (first form)))
      (push (eval form) result))
    (values result (not (null result)))))

(defslimefun init-inspector (string &key (reset t) (eval t) (dwim-mode nil))
  (with-buffer-syntax ()
    (when reset
      (reset-inspector))
    (let* ((form (block reading
                   (handler-bind
                       ((error (lambda (e)
                                 (declare (ignore e))
                                 (when dwim-mode
                                   (return-from reading 'nothing)))))
                     (read-from-string string nil 'nothing))))
           (value))
      (unless (eq form 'nothing)
        (setf value (cond
                      (dwim-mode
                       (let ((things (loop for hook :in *inspector-dwim-lookup-hooks*
                                           for (result foundp) = (multiple-value-list
                                                                     (funcall hook form))
                                           when foundp
                                           append (if (consp result)
                                                      result
                                                      (list result)))))
                         (if (rest things)
                             things
                             (first things))))
                      (eval (eval form))
                      (t form)))
        (when (and dwim-mode
                   form
                   value)
          ;; push the form to the inspector stack, so you can go back to it
          ;; with slime-inspector-pop if dwim missed the intention
          (push form *inspector-stack*))
        (inspect-object (if dwim-mode
                            (or value form)
                            value))))))

(defun print-part-to-string (value)
  (let ((string (to-string value))
        (pos (position value *inspector-history*)))
    (if pos
        (format nil "#~D=~A" pos string)
        string)))

(defun inspector-content-for-emacs (specs)
  (loop for part in specs collect 
        (etypecase part
          (null ; XXX encourages sloppy programming
           nil)
          (string part)
          (cons (destructure-case part
                  ((:newline) 
                   (string #\newline))
                  ((:value obj &optional str) 
                   (value-part-for-emacs obj str))
                  ((:action label lambda) 
                   (action-part-for-emacs label lambda)))))))

(defun assign-index (object vector)
  (let ((index (fill-pointer vector)))
    (vector-push-extend object vector)
    index))

(defun value-part-for-emacs (object string)
  (list :value 
        (or string (print-part-to-string object))
        (assign-index object *inspectee-parts*)))

(defun action-part-for-emacs (label lambda)
  (list :action label (assign-index lambda *inspectee-actions*)))
  
(defun inspect-object (object &optional (inspector (make-default-inspector)))
  (push (setq *inspectee* object) *inspector-stack*)
  (unless (find object *inspector-history*)
    (vector-push-extend object *inspector-history*))
  (let ((*print-pretty* nil)            ; print everything in the same line
        (*print-circle* t)
        (*print-readably* nil))
    (multiple-value-bind (title content)
        (inspect-for-emacs object inspector)
      (list :title title
            :type (to-string (type-for-emacs object))
            :content (inspector-content-for-emacs content)
            :id (assign-index object *inspectee-parts*)))))

(defslimefun inspector-nth-part (index)
  (aref *inspectee-parts* index))

(defslimefun inspect-nth-part (index)
  (with-buffer-syntax ()
    (inspect-object (inspector-nth-part index))))

(defslimefun inspector-call-nth-action (index &rest args)
  (multiple-value-bind (value replace) (apply (aref *inspectee-actions* index) args)
      (if (eq replace :replace)
	  value
	  (inspect-object (pop *inspector-stack*)))))

(defslimefun inspector-pop ()
  "Drop the inspector stack and inspect the second element.  Return
nil if there's no second element."
  (with-buffer-syntax ()
    (cond ((cdr *inspector-stack*)
           (pop *inspector-stack*)
           (inspect-object (pop *inspector-stack*)))
          (t nil))))

(defslimefun inspector-next ()
  "Inspect the next element in the *inspector-history*."
  (with-buffer-syntax ()
    (let ((position (position *inspectee* *inspector-history*)))
      (cond ((= (1+ position) (length *inspector-history*))
             nil)
            (t (inspect-object (aref *inspector-history* (1+ position))))))))

(defslimefun inspector-reinspect ()
  (inspect-object *inspectee*))

(defslimefun quit-inspector ()
  (reset-inspector)
  nil)

(defslimefun describe-inspectee ()
  "Describe the currently inspected object."
  (with-buffer-syntax ()
    (describe-to-string *inspectee*)))

(defslimefun pprint-inspector-part (index)
  "Pretty-print the currently inspected object."
  (with-buffer-syntax ()
    (swank-pprint (list (inspector-nth-part index)))))

(defslimefun inspect-in-frame (string index)
  (with-buffer-syntax ()
    (reset-inspector)
    (inspect-object (eval-in-frame (from-string string) index))))

(defslimefun inspect-current-condition ()
  (with-buffer-syntax ()
    (reset-inspector)
    (inspect-object *swank-debugger-condition*)))

(defslimefun inspect-frame-var (frame var)
  (with-buffer-syntax ()
    (reset-inspector)
    (inspect-object (frame-var-value frame var))))


;;;; Thread listing

(defvar *thread-list* ()
  "List of threads displayed in Emacs.  We don't care a about
synchronization issues (yet).  There can only be one thread listing at
a time.")

(defslimefun list-threads ()
  "Return a list ((NAME DESCRIPTION) ...) of all threads."
  (setq *thread-list* (all-threads))
  (loop for thread in  *thread-list* 
       for name = (thread-name thread)
        collect (list (if (symbolp name) (symbol-name name) name)
                      (thread-status thread)
                      (thread-id thread))))

(defslimefun quit-thread-browser ()
  (setq *thread-list* nil))

(defun nth-thread (index)
  (nth index *thread-list*))

(defslimefun debug-nth-thread (index)
  (let ((connection *emacs-connection*))
    (interrupt-thread (nth-thread index)
                      (lambda ()
			(with-connection (connection)
			  (simple-break))))))

(defslimefun kill-nth-thread (index)
  (kill-thread (nth-thread index)))

(defslimefun start-swank-server-in-thread (index port-file-name)
  "Interrupt the INDEXth thread and make it start a swank server.
The server port is written to PORT-FILE-NAME."
  (interrupt-thread (nth-thread index)
                    (lambda ()
                      (start-server port-file-name :style nil))))

;;;; Class browser

(defun mop-helper (class-name fn)
  (let ((class (find-class class-name nil)))
    (if class
        (mapcar (lambda (x) (to-string (class-name x)))
                (funcall fn class)))))

(defslimefun mop (type symbol-name)
  "Return info about classes using mop.

    When type is:
     :subclasses - return the list of subclasses of class.
     :superclasses - return the list of superclasses of class."
  (let ((symbol (parse-symbol symbol-name *buffer-package*)))
    (ecase type
      (:subclasses
       (mop-helper symbol #'swank-mop:class-direct-subclasses))
      (:superclasses 
       (mop-helper symbol #'swank-mop:class-direct-superclasses)))))


;;;; Automatically synchronized state
;;;
;;; Here we add hooks to push updates of relevant information to
;;; Emacs.

;;;;; *FEATURES*

(defun sync-features-to-emacs ()
  "Update Emacs if any relevant Lisp state has changed."
  ;; FIXME: *slime-features* should be connection-local
  (unless (eq *slime-features* *features*)
    (setq *slime-features* *features*)
    (send-to-emacs (list :new-features (features-for-emacs)))))

(defun features-for-emacs ()
  "Return `*slime-features*' in a format suitable to send it to Emacs."
  *slime-features*)

(add-hook *pre-reply-hook* 'sync-features-to-emacs)


;;;;; Indentation of macros
;;;
;;; This code decides how macros should be indented (based on their
;;; arglists) and tells Emacs. A per-connection cache is used to avoid
;;; sending redundant information to Emacs -- we just say what's
;;; changed since last time.
;;;
;;; The strategy is to scan all symbols, pick out the macros, and look
;;; for &body-arguments.

(defvar *configure-emacs-indentation* t
  "When true, automatically send indentation information to Emacs
after each command.")

(defslimefun update-indentation-information ()
  (perform-indentation-update *emacs-connection* t)
  nil)

;; This function is for *PRE-REPLY-HOOK*.
(defun sync-indentation-to-emacs ()
  "Send any indentation updates to Emacs via CONNECTION."
  (when *configure-emacs-indentation*
    (let ((fullp (need-full-indentation-update-p *emacs-connection*)))
      (perform-indentation-update *emacs-connection* fullp))))

(defun need-full-indentation-update-p (connection)
  "Return true if the whole indentation cache should be updated.
This is a heuristic to avoid scanning all symbols all the time:
instead, we only do a full scan if the set of packages has changed."
  (set-difference (list-all-packages)
                  (connection.indentation-cache-packages connection)))

(defun perform-indentation-update (connection force)
  "Update the indentation cache in CONNECTION and update Emacs.
If FORCE is true then start again without considering the old cache."
  (let ((cache (connection.indentation-cache connection)))
    (when force (clrhash cache))
    (let ((delta (update-indentation/delta-for-emacs cache force)))
      (setf (connection.indentation-cache-packages connection)
            (list-all-packages))
      (unless (null delta)
        (send-to-emacs (list :indentation-update delta))))))

(defun update-indentation/delta-for-emacs (cache &optional force)
  "Update the cache and return the changes in a (SYMBOL . INDENT) list.
If FORCE is true then check all symbols, otherwise only check symbols
belonging to the buffer package."
  (let ((alist '()))
      (flet ((consider (symbol)
             (let ((indent (symbol-indentation symbol)))
               (when indent
                 (unless (equal (gethash symbol cache) indent)
                   (setf (gethash symbol cache) indent)
                   (push (cons (string-downcase symbol) indent) alist))))))
      (if force
          (do-all-symbols (symbol)
            (consider symbol))
          (do-symbols (symbol *buffer-package*)
            (when (eq (symbol-package symbol) *buffer-package*)
              (consider symbol)))))
    alist))

(defun package-names (package)
  "Return the name and all nicknames of PACKAGE in a list."
  (cons (package-name package) (package-nicknames package)))

(defun cl-symbol-p (symbol)
  "Is SYMBOL a symbol in the COMMON-LISP package?"
  (eq (symbol-package symbol) cl-package))

(defun known-to-emacs-p (symbol)
  "Return true if Emacs has special rules for indenting SYMBOL."
  (cl-symbol-p symbol))

(defun symbol-indentation (symbol)
  "Return a form describing the indentation of SYMBOL.
The form is to be used as the `common-lisp-indent-function' property
in Emacs."
  (if (and (macro-function symbol)
           (not (known-to-emacs-p symbol)))
      (let ((arglist (arglist symbol)))
        (etypecase arglist
          ((member :not-available)
           nil)
          (list
           (macro-indentation arglist))))
      nil))

(defun macro-indentation (arglist)
  (if (well-formed-list-p arglist)
      (position '&body (remove '&optional (clean-arglist arglist)))
      nil))

(defun well-formed-list-p (list)
  "Is LIST a proper list terminated by NIL?"
  (typecase list
    (null t)
    (cons (well-formed-list-p (cdr list)))
    (t    nil)))

(defun print-indentation-lossage (&optional (stream *standard-output*))
  "Return the list of symbols whose indentation styles collide incompatibly.
Collisions are caused because package information is ignored."
  (let ((table (make-hash-table :test 'equal)))
    (flet ((name (s) (string-downcase (symbol-name s))))
      (do-all-symbols (s)
        (setf (gethash (name s) table)
              (cons s (symbol-indentation s))))
      (let ((collisions '()))
        (do-all-symbols (s)
          (let* ((entry (gethash (name s) table))
                 (owner (car entry))
                 (indent (cdr entry)))
            (unless (or (eq s owner)
                        (equal (symbol-indentation s) indent)
                        (and (not (fboundp s))
                             (null (macro-function s))))
              (pushnew owner collisions)
              (pushnew s collisions))))
        (if (null collisions)
            (format stream "~&No worries!~%")
            (format stream "~&Symbols with collisions:~%~{  ~S~%~}"
                    collisions))))))

(add-hook *pre-reply-hook* 'sync-indentation-to-emacs)


;;;; Presentation menu protocol
;;
;; To define a menu for a type of object, define a method
;; menu-choices-for-presentation on that object type.  This function
;; should return a list of two element lists where the first element is
;; the name of the menu action and the second is a function that will be
;; called if the menu is chosen. The function will be called with 3
;; arguments:
;;
;; choice: The string naming the action from above
;;
;; object: The object 
;;
;; id: The presentation id of the object
;;
;; You might want append (when (next-method-p) (call-next-method)) to
;; pick up the Menu actions of superclasses.
;;

(defvar *presentation-active-menu* nil)

(defun menu-choices-for-presentation-id (id)
  (multiple-value-bind (ob presentp) (lookup-presented-object id)
    (cond ((not presentp) 'not-present)
	  (t
	   (let ((menu-and-actions (menu-choices-for-presentation ob)))
	     (setq *presentation-active-menu* (cons id menu-and-actions))
	     (mapcar 'car menu-and-actions))))))

(defun swank-ioify (thing)
  (cond ((keywordp thing) thing)
	((and (symbolp thing)(not (find #\: (symbol-name thing))))
	 (intern (symbol-name thing) 'swank-io-package))
	((consp thing) (cons (swank-ioify (car thing)) (swank-ioify (cdr thing))))
	(t thing)))

(defun execute-menu-choice-for-presentation-id (id count item)
  (let ((ob (lookup-presented-object id)))
    (assert (equal id (car *presentation-active-menu*)) () 
	    "Bug: Execute menu call for id ~a  but menu has id ~a"
	    id (car *presentation-active-menu*))
    (let ((action (second (nth (1- count) (cdr *presentation-active-menu*)))))
      (swank-ioify (funcall action item ob id)))))

;; Default method
(defmethod menu-choices-for-presentation (ob)
  (declare (ignore ob))
  nil)

;; Pathname
(defmethod menu-choices-for-presentation ((ob pathname))
  (let* ((file-exists (ignore-errors (probe-file ob)))
	 (lisp-type (make-pathname :type "lisp"))
	 (source-file (and (not (member (pathname-type ob) '("lisp" "cl") :test 'equal))
			   (let ((source (merge-pathnames lisp-type ob)))
			     (and (ignore-errors (probe-file source))
				  source))))
	 (fasl-file (and file-exists 
			 (equal (ignore-errors
				  (namestring
				   (truename
				    (compile-file-pathname
				     (merge-pathnames lisp-type ob)))))
				(namestring (truename ob))))))
    (remove nil 
	    (list*
	     (and (and file-exists (not fasl-file))
		  (list "Edit this file" 
			(lambda(choice object id) 
			  (declare (ignore choice id))
			  (ed-in-emacs (namestring (truename object)))
			  nil)))
	     (and file-exists
		  (list "Dired containing directory"
			(lambda (choice object id)
			  (declare (ignore choice id))
			  (ed-in-emacs (namestring 
					(truename
					 (merge-pathnames
					  (make-pathname :name "" :type "") object))))
			  nil)))
	     (and fasl-file
		  (list "Load this fasl file"
			(lambda (choice object id)
			  (declare (ignore choice id object)) 
			  (load ob)
			  nil)))
	     (and fasl-file
		  (list "Delete this fasl file"
			(lambda (choice object id)
			  (declare (ignore choice id object)) 
			  (let ((nt (namestring (truename ob))))
			    (when (y-or-n-p-in-emacs "Delete ~a? " nt)
			      (delete-file nt)))
			  nil)))
	     (and source-file 
		  (list "Edit lisp source file" 
			(lambda (choice object id) 
			  (declare (ignore choice id object)) 
			  (ed-in-emacs (namestring (truename source-file)))
			  nil)))
	     (and source-file 
		  (list "Load lisp source file" 
			(lambda(choice object id) 
			  (declare (ignore choice id object)) 
			  (load source-file)
			  nil)))
	     (and (next-method-p) (call-next-method))))))

(defmethod menu-choices-for-presentation ((ob function))
  (list (list "Disassemble"
              (lambda (choice object id) 
                (declare (ignore choice id)) 
                (disassemble object)))))

;;; swank.lisp ends here
