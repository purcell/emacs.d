;;; deferred.el --- Simple asynchronous functions for emacs lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2016 SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Version: 0.5.1
;; Package-Version: 20170901.630
;; Keywords: deferred, async
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/kiwanami/emacs-deferred

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 'deferred.el' is a simple library for asynchronous tasks.
;; [https://github.com/kiwanami/emacs-deferred]

;; The API is almost the same as JSDeferred written by cho45. See the
;; JSDeferred and Mochikit.Async web sites for further documentations.
;; [https://github.com/cho45/jsdeferred]
;; [http://mochikit.com/doc/html/MochiKit/Async.html]

;; A good introduction document (JavaScript)
;; [http://cho45.stfuawsc.com/jsdeferred/doc/intro.en.html]

;;; Samples:

;; ** HTTP Access

;; (require 'url)
;; (deferred:$
;;   (deferred:url-retrieve "http://www.gnu.org")
;;   (deferred:nextc it
;;     (lambda (buf)
;;       (insert  (with-current-buffer buf (buffer-string)))
;;       (kill-buffer buf))))

;; ** Invoking command tasks

;; (deferred:$
;;   (deferred:process "wget" "-O" "a.jpg" "http://www.gnu.org/software/emacs/tour/images/splash.png")
;;   (deferred:nextc it
;;     (lambda (x) (deferred:process "convert" "a.jpg" "-resize" "100x100" "jpg:b.jpg")))
;;   (deferred:nextc it
;;     (lambda (x)
;;       (insert-image (create-image (expand-file-name "b.jpg") 'jpeg nil)))))

;; See the readme for further API documentation.

;; ** Applications

;; *Inertial scrolling for Emacs
;; [https://github.com/kiwanami/emacs-inertial-scroll]

;; This program makes simple multi-thread function, using
;; deferred.el.

(require 'cl-lib)
(require 'subr-x)

(declare-function pp-display-expression 'pp)

(defvar deferred:version nil "deferred.el version")
(setq deferred:version "0.5.0")

;;; Code:

(defmacro deferred:aand (test &rest rest)
  "[internal] Anaphoric AND."
  (declare (debug ("test" form &rest form)))
  `(let ((it ,test))
     (if it ,(if rest `(deferred:aand ,@rest) 'it))))

(defmacro deferred:$ (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form)))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

(defmacro deferred:lambda (args &rest body)
  "Anaphoric lambda macro for self recursion."
  (declare (debug ("args" form &rest form)))
  (let ((argsyms (cl-loop repeat (length args) collect (cl-gensym))))
  `(lambda (,@argsyms)
     (let (self)
       (setq self (lambda( ,@args ) ,@body))
       (funcall self ,@argsyms)))))

(cl-defmacro deferred:try (d &key catch finally)
  "Try-catch-finally macro. This macro simulates the
try-catch-finally block asynchronously. CATCH and FINALLY can be
nil. Because of asynchrony, this macro does not ensure that the
task FINALLY should be called."
  (let ((chain
         (if catch `((deferred:error it ,catch)))))
    (when finally
      (setq chain (append chain `((deferred:watch it ,finally)))))
    `(deferred:$ ,d ,@chain)))

(defun deferred:setTimeout (f msec)
  "[internal] Timer function that emulates the `setTimeout' function in JS."
  (run-at-time (/ msec 1000.0) nil f))

(defun deferred:cancelTimeout (id)
  "[internal] Timer cancellation function that emulates the `cancelTimeout' function in JS."
  (cancel-timer id))

(defun deferred:run-with-idle-timer (sec f)
  "[internal] Wrapper function for run-with-idle-timer."
  (run-with-idle-timer sec nil f))

(defun deferred:call-lambda (f &optional arg)
  "[internal] Call a function with one or zero argument safely.
The lambda function can define with zero and one argument."
  (condition-case err
      (funcall f arg)
    ('wrong-number-of-arguments
     (display-warning 'deferred "\
Callback that takes no argument may be specified.
Passing callback with no argument is deprecated.
Callback must take one argument.
Or, this error is coming from somewhere inside of the callback: %S" err)
     (condition-case nil
         (funcall f)
       ('wrong-number-of-arguments
        (signal 'wrong-number-of-arguments (cdr err))))))) ; return the first error

;; debug

(eval-and-compile
  (defvar deferred:debug nil "Debug output switch."))
(defvar deferred:debug-count 0 "[internal] Debug output counter.")

(defmacro deferred:message (&rest args)
  "[internal] Debug log function."
  (when deferred:debug
    `(progn
       (with-current-buffer (get-buffer-create "*deferred:debug*")
         (save-excursion
           (goto-char (point-max))
           (insert (format "%5i %s\n" deferred:debug-count (format ,@args)))))
       (cl-incf deferred:debug-count))))

(defun deferred:message-mark ()
  "[internal] Debug log function."
  (interactive)
  (deferred:message "==================== mark ==== %s"
    (format-time-string "%H:%M:%S" (current-time))))

(defun deferred:pp (d)
  (require 'pp)
  (deferred:$
    (deferred:nextc d
      (lambda (x)
        (pp-display-expression x "*deferred:pp*")))
    (deferred:error it
      (lambda (e)
        (pp-display-expression e "*deferred:pp*")))
    (deferred:nextc it
      (lambda (_x) (pop-to-buffer "*deferred:pp*")))))

(defvar deferred:debug-on-signal nil
"If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(defmacro deferred:condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`deferred:debug-on-signal'."
  (declare (debug condition-case)
           (indent 2))
  `(let ((debug-on-signal
          (or debug-on-signal deferred:debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar deferred:tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar deferred:queue nil
  "[internal] The execution queue of deferred objects.
See the functions `deferred:post-task' and `deferred:worker'.")

(defmacro deferred:pack (a b c)
  `(cons ,a (cons ,b ,c)))

(defun deferred:schedule-worker ()
  "[internal] Schedule consuming a deferred task in the execution queue."
  (run-at-time deferred:tick-time nil 'deferred:worker))

(defun deferred:post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`deferred:queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (push (deferred:pack d which arg) deferred:queue)
  (deferred:message "QUEUE-POST [%s]: %s"
    (length deferred:queue) (deferred:pack d which arg))
  (deferred:schedule-worker)
  d)

(defun deferred:clear-queue ()
  "Clear the execution queue. For test and debugging."
  (interactive)
  (deferred:message "QUEUE-CLEAR [%s -> 0]" (length deferred:queue))
  (setq deferred:queue nil))

(defun deferred:worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when deferred:queue
    (let* ((pack (car (last deferred:queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq deferred:queue (nbutlast deferred:queue))
      (condition-case err
          (setq value (deferred:exec-task d which arg))
        (error
         (deferred:message "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

(defun deferred:flush-queue! ()
  "Call all deferred tasks synchronously. For test and debugging."
  (let (value)
    (while deferred:queue
      (setq value (deferred:worker)))
    value))

(defun deferred:sync! (d)
  "Wait for the given deferred task. For test and debugging.
Error is raised if it is not processed within deferred chain D."
  (progn
    (let ((last-value 'deferred:undefined*)
          uncaught-error)
      (deferred:try
        (deferred:nextc d
          (lambda (x) (setq last-value x)))
        :catch
        (lambda (err) (setq uncaught-error err)))
      (while (and (eq 'deferred:undefined* last-value)
                  (not uncaught-error))
        (sit-for 0.05)
        (sleep-for 0.05))
      (when uncaught-error
        (deferred:resignal uncaught-error))
      last-value)))



;; Struct: deferred
;;
;; callback    : a callback function (default `deferred:default-callback')
;; errorback   : an errorback function (default `deferred:default-errorback')
;; cancel      : a canceling function (default `deferred:default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct deferred
  (callback 'deferred:default-callback)
  (errorback 'deferred:default-errorback)
  (cancel 'deferred:default-cancel)
  next status value)

(defun deferred:default-callback (i)
  "[internal] Default callback function."
  (identity i))

(defun deferred:default-errorback (err)
  "[internal] Default errorback function."
  (deferred:resignal err))

(defun deferred:resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun deferred:default-cancel (d)
  "[internal] Default canceling function."
  (deferred:message "CANCEL : %s" d)
  (setf (deferred-callback d) 'deferred:default-callback)
  (setf (deferred-errorback d) 'deferred:default-errorback)
  (setf (deferred-next d) nil)
  d)

(defvar deferred:onerror nil
  "Default error handler. This value is nil or a function that
  have one argument for the error message.")

(defun deferred:exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (deferred:message "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "deferred:exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (deferred-callback d)
                    (deferred-errorback d)))
        (next-deferred (deferred-next d)))
    (cond
     (callback
      (deferred:condition-case err
          (let ((value (deferred:call-lambda callback arg)))
            (cond
             ((deferred-p value)
              (deferred:message "WAIT NEST : %s" value)
              (if next-deferred
                  (deferred:set-next value next-deferred)
                value))
             (t
              (if next-deferred
                  (deferred:post-task next-deferred 'ok value)
                (setf (deferred-status d) 'ok)
                (setf (deferred-value d) value)
                value))))
        (error
         (cond
          (next-deferred
           (deferred:post-task next-deferred 'ng err))
          (deferred:onerror
            (deferred:call-lambda deferred:onerror err))
          (t
           (deferred:message "ERROR : %S" err)
           (message "deferred error : %S" err)
           (setf (deferred-status d) 'ng)
           (setf (deferred-value d) err)
           err)))))
     (t ; <= (null callback)
      (cond
       (next-deferred
        (deferred:exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                             ; (eq which 'ng)
        (deferred:resignal arg)))))))

(defun deferred:set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (deferred-next prev) next)
  (cond
   ((eq 'ok (deferred-status prev))
    (setf (deferred-status prev) nil)
    (let ((ret (deferred:exec-task
                 next 'ok (deferred-value prev))))
      (if (deferred-p ret) ret
        next)))
   ((eq 'ng (deferred-status prev))
    (setf (deferred-status prev) nil)
    (let ((ret (deferred:exec-task next 'ng (deferred-value prev))))
      (if (deferred-p ret) ret
        next)))
   (t
    next)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic functions for deferred objects

(defun deferred:new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-deferred :callback callback)
    (make-deferred)))

(defun deferred:callback (d &optional arg)
  "Start deferred chain with a callback message."
  (deferred:exec-task d 'ok arg))

(defun deferred:errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (deferred:exec-task d 'ng arg))

(defun deferred:callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (deferred:post-task d 'ok arg))

(defun deferred:errorback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (deferred:post-task d 'ng arg))

(defun deferred:cancel (d)
  "Cancel all callbacks and deferred chain in the deferred object."
  (deferred:message "CANCEL : %s" d)
  (funcall (deferred-cancel d) d)
  d)

(defun deferred:status (d)
  "Return a current status of the deferred object. The returned value means following:
`ok': the callback was called and waiting for next deferred.
`ng': the errorback was called and waiting for next deferred.
 nil: The neither callback nor errorback was not called."
  (deferred-status d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic utility functions

(defun deferred:succeed (&optional arg)
  "Create a synchronous deferred object."
  (let ((d (deferred:new)))
    (deferred:exec-task d 'ok arg)
    d))

(defun deferred:fail (&optional arg)
  "Create a synchronous deferred object."
  (let ((d (deferred:new)))
    (deferred:exec-task d 'ng arg)
    d))

(defun deferred:next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (deferred:callback-post (deferred:new callback))."
  (let ((d (if callback
               (make-deferred :callback callback)
             (make-deferred))))
    (deferred:callback-post d arg)
    d))

(defun deferred:nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (let ((nd (make-deferred :callback callback)))
    (deferred:set-next d nd)))

(defun deferred:error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (let ((nd (make-deferred :errorback callback)))
    (deferred:set-next d nd)))

(defun deferred:watch (d callback)
  "Create a deferred object with watch task and connect it to the given deferred object.
The watch task CALLBACK can not affect deferred chains with
return values. This function is used in following purposes,
simulation of try-finally block in asynchronous tasks, progress
monitoring of tasks."
  (let* ((callback callback)
         (normal (lambda (x) (ignore-errors (deferred:call-lambda callback x)) x))
         (err    (lambda (e)
                   (ignore-errors (deferred:call-lambda callback e))
                   (deferred:resignal e))))
    (let ((nd (make-deferred :callback normal :errorback err)))
      (deferred:set-next d nd))))

(defun deferred:wait (msec)
  "Return a deferred object scheduled at MSEC millisecond later."
  (let ((d (deferred:new)) (start-time (float-time)) timer)
    (deferred:message "WAIT : %s" msec)
    (setq timer (deferred:setTimeout
                  (lambda ()
                    (deferred:exec-task d 'ok
                      (* 1000.0 (- (float-time) start-time)))
                    nil) msec))
    (setf (deferred-cancel d)
          (lambda (x)
            (deferred:cancelTimeout timer)
            (deferred:default-cancel x)))
    d))

(defun deferred:wait-idle (msec)
  "Return a deferred object which will run when Emacs has been
idle for MSEC millisecond."
  (let ((d (deferred:new)) (start-time (float-time)) timer)
    (deferred:message "WAIT-IDLE : %s" msec)
    (setq timer
          (deferred:run-with-idle-timer
            (/ msec 1000.0)
            (lambda ()
              (deferred:exec-task d 'ok
                (* 1000.0 (- (float-time) start-time)))
              nil)))
    (setf (deferred-cancel d)
          (lambda (x)
            (deferred:cancelTimeout timer)
            (deferred:default-cancel x)))
    d))

(defun deferred:call (f &rest args)
  "Call the given function asynchronously."
  (deferred:next
    (lambda (_x)
      (apply f args))))

(defun deferred:apply (f &optional args)
  "Call the given function asynchronously."
  (deferred:next
    (lambda (_x)
      (apply f args))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defun deferred:empty-p (times-or-seq)
  "[internal] Return non-nil if TIMES-OR-SEQ is the number zero or nil."
  (or (and (numberp times-or-seq) (<= times-or-seq 0))
      (and (sequencep times-or-seq) (= (length times-or-seq) 0))))

(defun deferred:loop (times-or-seq func)
  "Return a iteration deferred object."
  (deferred:message "LOOP : %s" times-or-seq)
  (if (deferred:empty-p times-or-seq) (deferred:next)
    (let* (items (rd
                  (cond
                   ((numberp times-or-seq)
                    (cl-loop for i from 0 below times-or-seq
                             with ld = (deferred:next)
                             do
                             (push ld items)
                             (setq ld
                                   (let ((i i))
                                     (deferred:nextc ld
                                       (lambda (_x) (deferred:call-lambda func i)))))
                             finally return ld))
                   ((sequencep times-or-seq)
                    (cl-loop for i in (append times-or-seq nil) ; seq->list
                             with ld = (deferred:next)
                             do
                             (push ld items)
                             (setq ld
                                   (let ((i i))
                                     (deferred:nextc ld
                                       (lambda (_x) (deferred:call-lambda func i)))))
                             finally return ld)))))
      (setf (deferred-cancel rd)
            (lambda (x) (deferred:default-cancel x)
              (cl-loop for i in items
                       do (deferred:cancel i))))
      rd)))

(defun deferred:trans-multi-args (args self-func list-func main-func)
  "[internal] Check the argument values and dispatch to methods."
  (cond
   ((and (= 1 (length args)) (consp (car args)) (not (functionp (car args))))
    (let ((lst (car args)))
      (cond
       ((or (null lst) (null (car lst)))
        (deferred:next))
       ((deferred:aand lst (car it) (or (functionp it) (deferred-p it)))
        ;; a list of deferred objects
        (funcall list-func lst))
       ((deferred:aand lst (consp it))
        ;; an alist of deferred objects
        (funcall main-func lst))
       (t (error "Wrong argument type. %s" args)))))
   (t (funcall self-func args))))

(defun deferred:parallel-array-to-alist (lst)
  "[internal] Translation array to alist."
  (cl-loop for d in lst
           for i from 0 below (length lst)
           collect (cons i d)))

(defun deferred:parallel-alist-to-array (alst)
  "[internal] Translation alist to array."
  (cl-loop for pair in
           (sort alst (lambda (x y)
                        (< (car x) (car y))))
           collect (cdr pair)))

(defun deferred:parallel-func-to-deferred (alst)
  "[internal] Normalization for parallel and earlier arguments."
  (cl-loop for pair in alst
           for d = (cdr pair)
           collect
           (progn
             (unless (deferred-p d)
               (setf (cdr pair) (deferred:next d)))
             pair)))

(defun deferred:parallel-main (alst)
  "[internal] Deferred alist implementation for `deferred:parallel'. "
  (deferred:message "PARALLEL<KEY . VALUE>" )
  (let ((nd (deferred:new))
        (len (length alst))
        values)
    (cl-loop for pair in
             (deferred:parallel-func-to-deferred alst)
             with cd ; current child deferred
             do
             (let ((name (car pair)))
               (setq cd
                     (deferred:nextc (cdr pair)
                       (lambda (x)
                         (push (cons name x) values)
                         (deferred:message "PARALLEL VALUE [%s/%s] %s"
                           (length values) len (cons name x))
                         (when (= len (length values))
                           (deferred:message "PARALLEL COLLECTED")
                           (deferred:post-task nd 'ok (nreverse values)))
                         nil)))
               (deferred:error cd
                 (lambda (e)
                   (push (cons name e) values)
                   (deferred:message "PARALLEL ERROR [%s/%s] %s"
                     (length values) len (cons name e))
                   (when (= (length values) len)
                     (deferred:message "PARALLEL COLLECTED")
                     (deferred:post-task nd 'ok (nreverse values)))
                   nil))))
    nd))

(defun deferred:parallel-list (lst)
  "[internal] Deferred list implementation for `deferred:parallel'. "
  (deferred:message "PARALLEL<LIST>" )
  (let* ((pd (deferred:parallel-main (deferred:parallel-array-to-alist lst)))
         (rd (deferred:nextc pd 'deferred:parallel-alist-to-array)))
    (setf (deferred-cancel rd)
          (lambda (x) (deferred:default-cancel x)
            (deferred:cancel pd)))
    rd))

(defun deferred:parallel (&rest args)
  "Return a deferred object that calls given deferred objects or
functions in parallel and wait for all callbacks. The following
deferred task will be called with an array of the return
values. ARGS can be a list or an alist of deferred objects or
functions."
  (deferred:message "PARALLEL : %s" args)
  (deferred:trans-multi-args args
    'deferred:parallel 'deferred:parallel-list 'deferred:parallel-main))

(defun deferred:earlier-main (alst)
  "[internal] Deferred alist implementation for `deferred:earlier'. "
  (deferred:message "EARLIER<KEY . VALUE>" )
  (let ((nd (deferred:new))
        (len (length alst))
        value results)
    (cl-loop for pair in
             (deferred:parallel-func-to-deferred alst)
             with cd ; current child deferred
             do
             (let ((name (car pair)))
               (setq cd
                     (deferred:nextc (cdr pair)
                       (lambda (x)
                         (push (cons name x) results)
                         (cond
                          ((null value)
                           (setq value (cons name x))
                           (deferred:message "EARLIER VALUE %s" (cons name value))
                           (deferred:post-task nd 'ok value))
                          (t
                           (deferred:message "EARLIER MISS [%s/%s] %s" (length results) len (cons name value))
                           (when (eql (length results) len)
                             (deferred:message "EARLIER COLLECTED"))))
                         nil)))
               (deferred:error cd
                 (lambda (e)
                   (push (cons name e) results)
                   (deferred:message "EARLIER ERROR [%s/%s] %s" (length results) len (cons name e))
                   (when (and (eql (length results) len) (null value))
                     (deferred:message "EARLIER FAILED")
                     (deferred:post-task nd 'ok nil))
                   nil))))
    nd))

(defun deferred:earlier-list (lst)
  "[internal] Deferred list implementation for `deferred:earlier'. "
  (deferred:message "EARLIER<LIST>" )
  (let* ((pd (deferred:earlier-main (deferred:parallel-array-to-alist lst)))
         (rd (deferred:nextc pd (lambda (x) (cdr x)))))
    (setf (deferred-cancel rd)
          (lambda (x) (deferred:default-cancel x)
            (deferred:cancel pd)))
    rd))


(defun deferred:earlier (&rest args)
  "Return a deferred object that calls given deferred objects or
functions in parallel and wait for the first callback. The
following deferred task will be called with the first return
value. ARGS can be a list or an alist of deferred objects or
functions."
  (deferred:message "EARLIER : %s" args)
  (deferred:trans-multi-args args
    'deferred:earlier 'deferred:earlier-list 'deferred:earlier-main))

(defmacro deferred:timeout (timeout-msec timeout-form d)
  "Time out macro on a deferred task D.  If the deferred task D
does not complete within TIMEOUT-MSEC, this macro cancels the
deferred task and return the TIMEOUT-FORM."
  `(deferred:earlier
     (deferred:nextc (deferred:wait ,timeout-msec)
       (lambda (x) ,timeout-form))
     ,d))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application functions

(defvar deferred:uid 0 "[internal] Sequence number for some utilities. See the function `deferred:uid'.")

(defun deferred:uid ()
  "[internal] Generate a sequence number."
  (cl-incf deferred:uid))

(defun deferred:buffer-string (strformat buf)
  "[internal] Return a string in the buffer with the given format."
  (format strformat
          (with-current-buffer buf (buffer-string))))

(defun deferred:process (command &rest args)
  "A deferred wrapper of `start-process'. Return a deferred
object. The process name and buffer name of the argument of the
`start-process' are generated by this function automatically.
The next deferred object receives stdout and stderr string from
the command process."
  (deferred:process-gen 'start-process command args))

(defun deferred:process-shell (command &rest args)
  "A deferred wrapper of `start-process-shell-command'. Return a deferred
object. The process name and buffer name of the argument of the
`start-process-shell-command' are generated by this function automatically.
The next deferred object receives stdout and stderr string from
the command process."
  (deferred:process-gen 'start-process-shell-command command args))

(defun deferred:process-buffer (command &rest args)
  "A deferred wrapper of `start-process'. Return a deferred
object. The process name and buffer name of the argument of the
`start-process' are generated by this function automatically.
The next deferred object receives stdout and stderr buffer from
the command process."
  (deferred:process-buffer-gen 'start-process command args))

(defun deferred:process-shell-buffer (command &rest args)
  "A deferred wrapper of `start-process-shell-command'. Return a deferred
object. The process name and buffer name of the argument of the
`start-process-shell-command' are generated by this function automatically.
The next deferred object receives stdout and stderr buffer from
the command process."
  (deferred:process-buffer-gen 'start-process-shell-command command args))

(defun deferred:process-gen (f command args)
  "[internal]"
  (let ((pd (deferred:process-buffer-gen f command args)) d)
    (setq d (deferred:nextc pd
              (lambda (buf)
                (prog1
                    (with-current-buffer buf (buffer-string))
                  (kill-buffer buf)))))
    (setf (deferred-cancel d)
          (lambda (_x)
            (deferred:default-cancel d)
            (deferred:default-cancel pd)))
    d))

(defun deferred:process-buffer-gen (f command args)
  "[internal]"
  (let ((d (deferred:next)) (uid (deferred:uid)))
    (let ((proc-name (format "*deferred:*%s*:%s" command uid))
          (buf-name (format " *deferred:*%s*:%s" command uid))
          (pwd default-directory)
          (env process-environment)
          (con-type process-connection-type)
          (nd (deferred:new)) proc-buf proc)
      (deferred:nextc d
        (lambda (_x)
          (setq proc-buf (get-buffer-create buf-name))
          (condition-case err
              (let ((default-directory pwd)
                    (process-environment env)
                    (process-connection-type con-type))
                (setq proc
                      (if (null (car args))
                          (apply f proc-name buf-name command nil)
                        (apply f proc-name buf-name command args)))
                (set-process-sentinel
                 proc
                 (lambda (proc event)
		   (unless (process-live-p proc)
		     (if (zerop (process-exit-status proc))
			 (deferred:post-task nd 'ok proc-buf)
		       (let ((msg (format "Deferred process exited abnormally:\n  command: %s\n  exit status: %s %s\n  event: %s\n  buffer contents: %S"
					  command
					  (process-status proc)
					  (process-exit-status proc)
					  (string-trim-right event)
					  (if (buffer-live-p proc-buf)
					      (with-current-buffer proc-buf
						(buffer-string))
					    "(unavailable)"))))
			 (kill-buffer proc-buf)
			 (deferred:post-task nd 'ng msg))))))
		(setf (deferred-cancel nd)
		      (lambda (x) (deferred:default-cancel x)
			(when proc
			  (kill-process proc)
			  (kill-buffer proc-buf)))))
	    (error (deferred:post-task nd 'ng err)))
	  nil))
      nd)))

(defmacro deferred:processc (d command &rest args)
  "Process chain of `deferred:process'."
  `(deferred:nextc ,d
    (lambda (,(cl-gensym)) (deferred:process ,command ,@args))))

(defmacro deferred:process-bufferc (d command &rest args)
  "Process chain of `deferred:process-buffer'."
  `(deferred:nextc ,d
     (lambda (,(cl-gensym)) (deferred:process-buffer ,command ,@args))))

(defmacro deferred:process-shellc (d command &rest args)
  "Process chain of `deferred:process'."
  `(deferred:nextc ,d
    (lambda (,(cl-gensym)) (deferred:process-shell ,command ,@args))))

(defmacro deferred:process-shell-bufferc (d command &rest args)
  "Process chain of `deferred:process-buffer'."
  `(deferred:nextc ,d
     (lambda (,(cl-gensym)) (deferred:process-shell-buffer ,command ,@args))))

;; Special variables defined in url-vars.el.
(defvar url-request-data)
(defvar url-request-method)
(defvar url-request-extra-headers)

(declare-function url-http-symbol-value-in-buffer "url-http"
                  (symbol buffer &optional unbound-value))

(declare-function deferred:url-param-serialize "request" (params))

(declare-function deferred:url-escape "request" (val))

(eval-after-load "url"
  ;; for url package
  ;; TODO: proxy, charaset
  ;; List of gloabl variables to preserve and restore before url-retrieve call
  '(let ((url-global-variables '(url-request-data
                                 url-request-method
                                 url-request-extra-headers)))

     (defun deferred:url-retrieve (url &optional cbargs silent inhibit-cookies)
       "A wrapper function for url-retrieve. The next deferred
object receives the buffer object that URL will load
into. Values of dynamically bound 'url-request-data', 'url-request-method' and
'url-request-extra-headers' are passed to url-retrieve call."
       (let ((nd (deferred:new))
             buf
             (local-values (mapcar (lambda (symbol) (symbol-value symbol)) url-global-variables)))
         (deferred:next
           (lambda (_x)
             (cl-progv url-global-variables local-values
               (condition-case err
                   (setq buf
                         (url-retrieve
                          url (lambda (_xx) (deferred:post-task nd 'ok buf))
                          cbargs silent inhibit-cookies))
                 (error (deferred:post-task nd 'ng err)))
             nil)))
         (setf (deferred-cancel nd)
               (lambda (_x)
                 (when (buffer-live-p buf)
                   (kill-buffer buf))))
         nd))

     (defun deferred:url-delete-header (buf)
       (with-current-buffer buf
         (let ((pos (url-http-symbol-value-in-buffer
                     'url-http-end-of-headers buf)))
           (when pos
             (delete-region (point-min) (1+ pos)))))
       buf)

     (defun deferred:url-delete-buffer (buf)
       (when (and buf (buffer-live-p buf))
         (kill-buffer buf))
       nil)

     (defun deferred:url-get (url &optional params &rest args)
       "Perform a HTTP GET method with `url-retrieve'. PARAMS is
a parameter list of (key . value) or key. ARGS will be appended
to deferred:url-retrieve args list. The next deferred
object receives the buffer object that URL will load into."
       (when params
         (setq url
               (concat url "?" (deferred:url-param-serialize params))))
       (let ((d (deferred:$
                  (apply 'deferred:url-retrieve url args)
                  (deferred:nextc it 'deferred:url-delete-header))))
         (deferred:set-next
           d (deferred:new 'deferred:url-delete-buffer))
         d))

     (defun deferred:url-post (url &optional params &rest args)
       "Perform a HTTP POST method with `url-retrieve'. PARAMS is
a parameter list of (key . value) or key. ARGS will be appended
to deferred:url-retrieve args list. The next deferred
object receives the buffer object that URL will load into."
       (let ((url-request-method "POST")
             (url-request-extra-headers
              (append url-request-extra-headers
                      '(("Content-Type" . "application/x-www-form-urlencoded"))))
             (url-request-data (deferred:url-param-serialize params)))
         (let ((d (deferred:$
                    (apply 'deferred:url-retrieve url args)
                    (deferred:nextc it 'deferred:url-delete-header))))
           (deferred:set-next
             d (deferred:new 'deferred:url-delete-buffer))
           d)))

     (defun deferred:url-escape (val)
       "[internal] Return a new string that is VAL URI-encoded."
       (unless (stringp val)
         (setq val (format "%s" val)))
       (url-hexify-string
        (encode-coding-string val 'utf-8)))

     (defun deferred:url-param-serialize (params)
       "[internal] Serialize a list of (key . value) cons cells
into a query string."
       (when params
         (mapconcat
          'identity
          (cl-loop for p in params
                   collect
                   (cond
                    ((consp p)
                     (concat
                      (deferred:url-escape (car p)) "="
                      (deferred:url-escape (cdr p))))
                    (t
                     (deferred:url-escape p))))
          "&")))
     ))


(provide 'deferred)
;;; deferred.el ends here
