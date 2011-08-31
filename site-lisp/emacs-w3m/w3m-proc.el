;;; w3m-proc.el --- Functions and macros to control sub-processes

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          Satoru Takabayashi <satoru-t@is.aist-nara.ac.jp>,
;;          Hideyuki SHIRAI    <shirai@meadowy.org>,
;;          Keisuke Nishida    <kxn30@po.cwru.edu>,
;;          Yuuichi Teranishi  <teranisi@gohome.org>,
;;          Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
;;          Katsumi Yamaoka    <yamaoka@jpl.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This module is a part of emacs-w3m which provides functions and
;; macros to control sub-processes.  Visit
;; <URL:http://emacs-w3m.namazu.org/> for more details of emacs-w3m.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'w3m-util)

(eval-when-compile
  ;; Variable(s) which are used in the following inline functions.
  ;; They should be defined in the other module at run-time.
  (defvar w3m-current-url)
  (defvar w3m-current-buffer)
  (defvar w3m-current-process)
  (defvar w3m-profile-directory)
  (defvar w3m-terminal-coding-system)
  (defvar w3m-command)
  (defvar w3m-command-arguments)
  (defvar w3m-command-environment)
  (defvar w3m-async-exec)
  (defvar w3m-process-connection-type)
  (defvar w3m-process-modeline-format)
  (defvar w3m-work-buffer-list)
  (autoload 'w3m-idle-images-show-unqueue "w3m"))

;; Silence the Emacs' byte-compiler that says ``might not be defined''.
(eval-when-compile
  (defun w3m-decode-coding-string-with-priority (str coding)
    ()))

(defvar w3m-process-inhibit-quit t
  "`w3m-process-sentinel' binds `inhibit-quit' according to this variable.")
(defvar w3m-process-timeout 300
  "Number of seconds idle time waiting for processes to terminate.")
(defvar w3m-process-kill-surely (featurep 'meadow)
  "If non-nil, kill the process surely.")

(defconst w3m-process-max 5 "The maximum limit of the working processes.")
(defvar w3m-process-queue nil "Queue of processes.")

(defvar w3m-process-exit-status nil "The last exit status of a process.")
(defvar w3m-process-authinfo-alist nil)
(defvar w3m-process-accept-alist nil)

(defvar w3m-process-user nil)
(defvar w3m-process-passwd nil)
(defvar w3m-process-realm nil)
(defvar w3m-process-object nil)
(make-variable-buffer-local 'w3m-process-user)
(make-variable-buffer-local 'w3m-process-passwd)
(make-variable-buffer-local 'w3m-process-realm)
(make-variable-buffer-local 'w3m-process-object)

(defvar w3m-process-modeline-string nil
  "Modeline string to show status of retrieving process.")
(make-variable-buffer-local 'w3m-process-modeline-string)

(defvar w3m-process-proxy-user nil "User name of the proxy server.")
(defvar w3m-process-proxy-passwd nil "Password of the proxy server.")
(defvar w3m-process-ssl-passphrase nil
  "Passphrase for the client certificate.")

(defmacro w3m-process-with-coding-system (&rest body)
  "Set coding systems for `w3m-command', and evaluate BODY."
  `(let ((coding-system-for-read 'binary)
	 (coding-system-for-write w3m-terminal-coding-system)
	 (default-process-coding-system
	   (cons 'binary w3m-terminal-coding-system))
	 (process-connection-type w3m-process-connection-type))
     ,@body))
(put 'w3m-process-with-coding-system 'lisp-indent-function 0)
(put 'w3m-process-with-coding-system 'edebug-form-spec '(body))

(defmacro w3m-process-with-environment (alist &rest body)
  "Set the environment variables according to ALIST, and evaluate BODY."
  `(let ((process-environment (copy-sequence process-environment))
	 (temporary-file-directory
	  (if (file-directory-p w3m-profile-directory)
	      (file-name-as-directory w3m-profile-directory)
	    ,(if (featurep 'xemacs)
		 ;; Though `temporary-file-directory' exists even in XEmacs,
		 ;; that's only an imitation provided by APEL.
		 '(temp-directory)
	       'temporary-file-directory)))
	 (default-directory
	   (cond ((file-directory-p w3m-profile-directory)
		  (file-name-as-directory w3m-profile-directory))
		 ((file-directory-p (expand-file-name "~/"))
		  (expand-file-name "~/"))
		 (t temporary-file-directory))))
     ;; XEmacs obtains tmp-dir from the `temp-directory' function of which
     ;; return value can only be modified by the following env vars.
     ,@(if (featurep 'xemacs)
	   '((setenv "TEMP" temporary-file-directory) ;; Windoze
	     (setenv "TMPDIR" temporary-file-directory))) ;; Un|x
     (dolist (pair ,alist)
       (setenv (car pair) (cdr pair)))
     ,@body))
(put 'w3m-process-with-environment 'lisp-indent-function 1)
(put 'w3m-process-with-environment 'edebug-form-spec '(form body))

(defun w3m-process-p (object)
  "Return t if OBJECT is a `w3m-process' object."
  (and (consp object)
       (vectorp (cdr object))
       (eq 'w3m-process-object (aref (cdr object) 0))))

(put 'w3m-process-new 'edebug-form-spec '(form form form &optional form form))
(defmacro w3m-process-new (command arguments buffer &optional process handlers)
  "Return a new `w3m-process' object."
  `(cons (cons ,command ,arguments)
	 (vector 'w3m-process-object
		 ,buffer
		 ,process
		 ,handlers)))

(defmacro w3m-process-command (object)
  `(car (car ,object)))
(defmacro w3m-process-arguments (object)
  `(cdr (car ,object)))
(defmacro w3m-process-buffer (object)
  `(aref (cdr ,object) 1))
(defmacro w3m-process-process (object)
  `(aref (cdr ,object) 2))
(defmacro w3m-process-handlers (object)
  `(aref (cdr ,object) 3))

(put 'w3m-process-handler-new 'edebug-form-spec '(form form form))
(defmacro w3m-process-handler-new (buffer parent-buffer functions)
  `(vector ,buffer ,parent-buffer ,functions nil))
(defmacro w3m-process-handler-buffer (handler)
  `(aref ,handler 0))
(defmacro w3m-process-handler-parent-buffer (handler)
  `(aref ,handler 1))
(defmacro w3m-process-handler-functions (handler)
  `(aref ,handler 2))
(defmacro w3m-process-handler-result (handler)
  `(aref ,handler 3))

(defun w3m-process-push (handler command arguments)
  "Generate a new `w3m-process' object which is provided by HANDLER,
ARGUMENTS and this buffer, regist it to `w3m-process-queue', and
return it."
  (let ((x (assoc (cons command arguments) w3m-process-queue)))
    (unless x
      (setq x (w3m-process-new command arguments (current-buffer)))
      (push x w3m-process-queue))
    (push (w3m-process-handler-new (current-buffer) w3m-current-buffer handler)
	  (w3m-process-handlers x))
    (with-current-buffer (w3m-process-buffer x)
      (setq w3m-process-object x))))

(defun w3m-process-kill-process (process)
  "Kill process PROCESS safely."
  (when (processp process)
    (set-process-filter process 'ignore)
    (set-process-sentinel process 'ignore)
    (when (memq (process-status process) '(run stop))
      (kill-process process)
      (when w3m-process-kill-surely
	(while (memq (process-status process) '(run stop))
	  (sit-for 0.1))))))

(defun w3m-process-start-process (object &optional no-sentinel)
  "Start a process specified by the OBJECT, return always nil.
When NO-SENTINEL is not equal to nil, all status changes of the
generated asynchronous process is ignored.  Otherwise,
`w3m-process-sentinel' is given to the process as the sentinel."
  (if (w3m-process-process object)
      (when no-sentinel
	(set-process-sentinel (w3m-process-process object) 'ignore))
    (with-current-buffer (w3m-process-buffer object)
      (w3m-process-with-coding-system
	(w3m-process-with-environment w3m-command-environment
	  (let* ((command (w3m-process-command object))
		 (proc (apply 'start-process command
			      (current-buffer) command
			      (w3m-process-arguments object)))
		 (authinfo (when w3m-current-url
			     (w3m-url-authinfo w3m-current-url)))
		 (set-process-query-on-exit-flag
		  (if (fboundp 'set-process-query-on-exit-flag)
		      'set-process-query-on-exit-flag
		    'process-kill-without-query)))
	    (setq w3m-process-user (car authinfo)
		  w3m-process-passwd (cdr authinfo)
		  w3m-process-realm nil)
	    (setf (w3m-process-process object) proc)
	    (set-process-filter proc 'w3m-process-filter)
	    (set-process-sentinel proc (if no-sentinel
					   'ignore
					 'w3m-process-sentinel))
	    (funcall set-process-query-on-exit-flag proc nil))))))
  nil)	;; The return value of `w3m-process-start-process'.

(defun w3m-process-kill-stray-processes ()
  "Kill stray processes."
  (dolist (obj w3m-process-queue)
    (unless (buffer-name (w3m-process-buffer obj))
      (setq w3m-process-queue (delq obj w3m-process-queue))
      (when (w3m-process-process obj)
	(w3m-process-kill-process (w3m-process-process obj))))))

(defun w3m-process-start-queued-processes ()
  "Start a process which is registerd in `w3m-process-queue' if the
number of current working processes is less than `w3m-process-max'."
  (w3m-process-kill-stray-processes)
  (let ((num 0))
    (catch 'last
      (dolist (obj (reverse w3m-process-queue))
	(when (buffer-name (w3m-process-buffer obj))
	  (if (> (incf num) w3m-process-max)
	      (throw 'last nil)
	    (w3m-process-start-process obj)))))))

(defun w3m-process-stop (buffer)
  "Remove handlers related to the buffer BUFFER, and stop processes
which have no handler."
  (interactive (list (current-buffer)))
  (w3m-cancel-refresh-timer buffer)
  (setq w3m-process-queue
	(delq nil
	      (mapcar
	       (lambda (obj)
		 (let ((handlers
			;; List up handlers related to other buffer
			;; than the buffer BUFFER.
			(delq nil
			      (mapcar
			       (lambda (handler)
				 (unless (eq buffer
					     (w3m-process-handler-parent-buffer
					      handler))
				   handler))
			       (w3m-process-handlers obj)))))
		   (if handlers
		       (w3m-process-new
			(w3m-process-command obj)
			(w3m-process-arguments obj)
			(w3m-process-buffer obj)
			(w3m-process-process obj)
			(if (memq (w3m-process-buffer obj)
				  (mapcar (lambda (x)
					    (w3m-process-handler-buffer x))
					  handlers))
			    handlers
			  (cons
			   ;; Dummy handler to remove buffer.
			   (w3m-process-handler-new
			    (w3m-process-buffer obj)
			    (w3m-process-handler-parent-buffer (car handlers))
			    (lambda (x) (w3m-kill-buffer (current-buffer))))
			   handlers)))
		     (when (w3m-process-process obj)
		       (w3m-process-kill-process (w3m-process-process obj)))
		     (dolist (handler (w3m-process-handlers obj))
		       (w3m-kill-buffer (w3m-process-handler-buffer handler)))
		     nil)))
	       w3m-process-queue)))
  (when (buffer-name buffer)
    (with-current-buffer buffer
      (setq w3m-current-process nil)))
  (w3m-process-start-queued-processes)
  (w3m-force-window-update-later buffer))

(defun w3m-process-shutdown ()
  (let ((list w3m-process-queue))
    (setq w3m-process-queue nil
	  w3m-process-authinfo-alist nil
	  w3m-process-accept-alist nil)
    (dolist (obj list)
      (when (buffer-name (w3m-process-buffer obj))
	(when (w3m-process-process obj)
	  (w3m-process-kill-process (w3m-process-process obj))))
      (w3m-kill-buffer (w3m-process-buffer obj)))))

(defmacro w3m-process-with-null-handler (&rest body)
  "Generate the null handler, and evaluate BODY.
When BODY is evaluated, the local variable `handler' keeps the null
handler."
  (let ((var (gensym "--tempvar--")))
    `(let ((,var (let (handler) ,@body)))
       (when (w3m-process-p ,var)
	 (w3m-process-start-process ,var))
       ,var)))
(put 'w3m-process-with-null-handler 'lisp-indent-function 0)
(put 'w3m-process-with-null-handler 'edebug-form-spec '(body))

;; Error symbol:
(put 'w3m-process-timeout 'error-conditions '(error w3m-process-timeout))
(put 'w3m-process-timeout 'error-message "Time out")

(defun w3m-process-error-handler (error-data process)
  (setq w3m-process-queue (delq process w3m-process-queue))
  (w3m-process-kill-process (w3m-process-process process))
  (signal (car error-data) (cdr error-data)))

(defvar w3m-process-waited nil
  "Non-nil means that `w3m-process-with-wait-handler' is being evaluated.")

(defun w3m-process-wait-process (process seconds)
  "Wait for SECONDS seconds or until PROCESS will exit.
Returns the exit status of the PROCESS when it exit normally,
otherwise returns nil."
  (catch 'timeout
    (let ((start (current-time)))
      (while (or (and (prog2
			  (discard-input)
			  (not (save-current-buffer (sit-for 0.1)))
			(discard-input))
		      ;; Some input is detected but it may be a key
		      ;; press event which should be ignored when the
		      ;; process is not running.
		      (memq (process-status process) '(open run)))
		 (memq (process-status process) '(open run stop)))
	(and seconds
	     (< seconds (w3m-time-lapse-seconds start (current-time)))
	     (throw 'timeout nil)))
      (process-exit-status process))))

(defmacro w3m-process-with-wait-handler (&rest body)
  "Generate the waiting handler, and evaluate BODY.
When BODY is evaluated, the local variable `handler' keeps the handler
which will wait for the end of the evaluation."
  (let ((result (gensym "--result--"))
	(wait-function (gensym "--wait-function--")))
    `(let ((w3m-process-waited t)
	   (,result)
	   (,wait-function (make-symbol "wait-function")))
       (fset ,wait-function 'identity)
       (setq ,result (let ((handler (list ,wait-function))) ,@body))
       (while (w3m-process-p ,result)
	 (condition-case error
	     (let (w3m-process-inhibit-quit inhibit-quit)
	       ;; No sentinel function is registered and the process
	       ;; sentinel function is called from this macro, in
	       ;; order to avoid the dead-locking which occurs when
	       ;; this macro is called in the environment that
	       ;; `w3m-process-sentinel' is evaluated.
	       (w3m-process-start-process ,result t)
	       (unless (w3m-process-wait-process (w3m-process-process ,result)
						 w3m-process-timeout)
		 (w3m-process-error-handler (cons 'w3m-process-timeout nil)
					    ,result)))
	   (quit (w3m-process-error-handler error ,result)))
	 (w3m-process-sentinel (w3m-process-process ,result) "finished\n" t)
	 (setq ,result
	       (catch 'result
		 (dolist (handler (w3m-process-handlers ,result))
		   (when (memq ,wait-function
			       (w3m-process-handler-functions handler))
		     (throw 'result (w3m-process-handler-result handler))))
		 (w3m-process-error-handler (cons 'error
						  "Can't find wait handler")
					    ,result))))
       ,result)))
(put 'w3m-process-with-wait-handler 'lisp-indent-function 0)
(put 'w3m-process-with-wait-handler 'edebug-form-spec '(body))

;;; Explanation of w3m-process-do in Japanese:
;;
;; w3m-process-do は、非同期処理を簡単に書くためのマクロである。例えば、
;;
;;    (w3m-process-do
;;        (var (async-form...))
;;      post-body...)
;;
;; というように書くと、以下の順序で処理が行われる。
;;
;;   (1) async-form を評価
;;       --> async-form 内で非同期プロセスが生成された場合は、その非同
;;           期プロセス終了後に post-body が評価されるように、ハンドラ
;;           に追加
;;       --> 非同期プロセスが生成されなかった場合は、単に次のステップ
;;           に進む(= post-body を評価する)。
;;   (2) post-body を評価
;;
;; なお、async-form / post-body が評価される時、その内部で非同期プロセ
;; スが生成された場合に、その返り値を処理するためのハンドラが、変数
;; handler に設定されている。非同期な処理を行う関数を呼び出す場合には、
;; その関数の引数として必ず handler を渡さなければならない。
;;
;; また、w3m-process-do は、現在のハンドラの内容を調べるため、そのマク
;; ロが呼び出されている環境の変数 handler を参照する。例えば、
;;
;;    (let (handler) (w3m-process-do ...))
;;
;; と変数 handler を nil に束縛しておくと、「現時点のハンドラは空であ
;; る = 非同期プロセス実行後に必要な処理は存在しない」という意味になり、
;; w3m-process-do() は、非同期プロセスが生成された場合には単に nil を
;; 返し、それ以外の場合は post-body の値を返す。
;;
(defmacro w3m-process-do (spec &rest body)
  "(w3m-process-do (VAR FORM) BODY...): Eval the body BODY asynchronously.
If an asynchronous process is generated in the evaluation of the form
FORM, this macro returns its object immdiately, and the body BODY will
be evaluated after the end of the process with the variable VAR which
is set to the result of the form FORM.  Otherwise, the body BODY is
evaluated at the same time, and this macro returns the result of the
body BODY."
  (let ((var (or (car spec) (gensym "--tempvar--")))
	(form (cdr spec))
	(post-function (gensym "--post-function--")))
    `(let ((,post-function (lambda (,var) ,@body)))
       (let ((,var (let ((handler (cons ,post-function handler)))
		     ,@form)))
	 (if (w3m-process-p ,var)
	     (if handler
		 ,var
	       (w3m-process-start-process ,var))
	   (if (w3m-process-p (setq ,var (funcall ,post-function ,var)))
	       (if handler
		   ,var
		 (w3m-process-start-process ,var))
	     ,var))))))
(put 'w3m-process-do 'lisp-indent-function 1)
(put 'w3m-process-do 'edebug-form-spec '((symbolp form) def-body))

(defmacro w3m-process-do-with-temp-buffer (spec &rest body)
  "(w3m-process-do-with-temp-buffer (VAR FORM) BODY...):
Like `w3m-process-do', but the form FORM and the body BODY are
evaluated in a temporary buffer."
  (let ((var (or (car spec) (gensym "--tempvar--")))
	(form (cdr spec))
	(post-body (gensym "--post-body--"))
	(post-handler (gensym "--post-handler--"))
	(temp-buffer (gensym "--temp-buffer--"))
	(current-buffer (gensym "--current-buffer--")))
    `(lexical-let ((,temp-buffer
		    (w3m-get-buffer-create
		     (generate-new-buffer-name w3m-work-buffer-name)))
		   (,current-buffer (current-buffer)))
       (labels ((,post-body (,var)
			    (when (buffer-name ,temp-buffer)
			      (set-buffer ,temp-buffer))
			    ,@body)
		(,post-handler (,var)
			       (w3m-kill-buffer ,temp-buffer)
			       (when (buffer-name ,current-buffer)
				 (set-buffer ,current-buffer))
			       ,var))
	 (let ((,var (let ((handler
			    (cons ',post-body (cons ',post-handler handler))))
		       (with-current-buffer ,temp-buffer ,@form))))
	   (if (w3m-process-p ,var)
	       (if handler
		   ,var
		 (w3m-process-start-process ,var))
	     (if (w3m-process-p
		  (setq ,var (save-current-buffer
			       (let ((handler (cons ',post-handler handler)))
				 (,post-body ,var)))))
		 (if handler
		     ,var
		   (w3m-process-start-process ,var))
	       (,post-handler ,var))))))))
(put 'w3m-process-do-with-temp-buffer 'lisp-indent-function 1)
(put 'w3m-process-do-with-temp-buffer 'edebug-form-spec
     '((symbolp form) def-body))


(defun w3m-process-start (handler command arguments)
  "Run COMMAND with ARGUMENTS, and eval HANDLER asynchronously."
  (if w3m-async-exec
      (w3m-process-do
	  (exit-status (w3m-process-push handler command arguments))
	(w3m-process-start-after exit-status))
    (w3m-process-start-after
     (w3m-process-with-coding-system
       (w3m-process-with-environment w3m-command-environment
	 (apply 'call-process command nil t nil arguments))))))

(defun w3m-process-start-after (exit-status)
  (when w3m-current-buffer
    (with-current-buffer w3m-current-buffer
      (setq w3m-process-modeline-string nil)))
  (cond
   ((numberp exit-status)
    (zerop (setq w3m-process-exit-status exit-status)))
   ((not exit-status)
    (setq w3m-process-exit-status nil))
   (t
    (setq w3m-process-exit-status
	  (string-as-multibyte (format "%s" exit-status)))
    nil)))

(defvar w3m-process-background nil
  "Non-nil means that an after handler is being evaluated.")

(defun w3m-process-sentinel (process event &optional ignore-queue)
  ;; Ensure that this function will be never called repeatedly.
  (set-process-sentinel process 'ignore)
  (let ((inhibit-quit w3m-process-inhibit-quit)
	(w3m-process-background t))
    (unwind-protect
	(if (buffer-name (process-buffer process))
	    (with-current-buffer (process-buffer process)
	      (w3m-static-unless (featurep 'xemacs)
		(accept-process-output process 1))
	      (setq w3m-process-queue
		    (delq w3m-process-object w3m-process-queue))
	      (let ((exit-status (process-exit-status process))
		    (buffer (current-buffer))
		    (realm  w3m-process-realm)
		    (user   w3m-process-user)
		    (passwd w3m-process-passwd)
		    (obj    w3m-process-object))
		(setq w3m-process-object nil)
		(dolist (x (w3m-process-handlers obj))
		  (when (and
			 (buffer-name (w3m-process-handler-buffer x))
			 (buffer-name (w3m-process-handler-parent-buffer x)))
		    (set-buffer (w3m-process-handler-buffer x))
		    (unless (eq buffer (current-buffer))
		      (insert-buffer-substring buffer))))
		(dolist (x (w3m-process-handlers obj))
		  (when (and
			 (buffer-name (w3m-process-handler-buffer x))
			 (buffer-name (w3m-process-handler-parent-buffer x)))
		    (set-buffer (w3m-process-handler-buffer x))
		    (let ((w3m-process-exit-status)
			  (w3m-current-buffer
			   (w3m-process-handler-parent-buffer x))
			  (handler
			   (w3m-process-handler-functions x))
			  (exit-status exit-status))
		      (when realm
			(w3m-process-set-authinfo w3m-current-url
						  realm user passwd))
		      (while (and handler
				  (not (w3m-process-p
					(setq exit-status
					      (funcall (pop handler)
						       exit-status))))))
		      (setf (w3m-process-handler-result x) exit-status))))))
	  ;; Something wrong has been occured.
	  (catch 'last
	    (dolist (obj w3m-process-queue)
	      (when (eq process (w3m-process-process obj))
		(setq w3m-process-queue (delq obj w3m-process-queue))
		(throw 'last nil)))))
      (delete-process process)
      (unless ignore-queue
	(w3m-process-start-queued-processes)))))

(defun w3m-process-filter (process string)
  (when (buffer-name (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((buffer-read-only nil)
	    (case-fold-search nil))
	(goto-char (process-mark process))
	(insert string)
	(set-marker (process-mark process) (point))
	(unless (string= "" string)
	  (goto-char (point-min))
	  (cond
	   ((and (looking-at
		  "\\(?:Accept [^\n]+\n\\)*\\([^\n]+: accept\\? \\)(y/n)")
		 (= (match-end 0) (point-max)))
	    ;; SSL certificate
	    (message "")
	    (let ((yn (w3m-process-y-or-n-p w3m-current-url (match-string 1))))
	      (ignore-errors
		(process-send-string process (if yn "y\n" "n\n"))
		(delete-region (point-min) (point-max)))))
	   ((and (looking-at "\n?Accept unsecure SSL session:.*\n")
		 (= (match-end 0) (point-max)))
	    (delete-region (point-min) (point-max)))
	   ((and (looking-at "\\(\n?Wrong username or password\n\\)?\
Proxy Username for \\(?:.*\\): Proxy Password: ")
		 (= (match-end 0) (point-max)))
	    (when (or (match-beginning 1)
		      (not (stringp w3m-process-proxy-passwd)))
	      (setq w3m-process-proxy-passwd
		    (read-passwd "Proxy Password: ")))
	    (ignore-errors
	      (process-send-string process
				   (concat w3m-process-proxy-passwd "\n"))
	      (delete-region (point-min) (point-max))))
	   ((and (looking-at "\\(\n?Wrong username or password\n\\)?\
Proxy Username for \\(.*\\): ")
		 (= (match-end 0) (point-max)))
	    (when (or (match-beginning 1)
		      (not (stringp w3m-process-proxy-user)))
	      (setq w3m-process-proxy-user
		    (read-from-minibuffer (concat
					   "Proxy Username for "
					   (match-string 2) ": "))))
	    (ignore-errors
	      (process-send-string process
				   (concat w3m-process-proxy-user "\n"))))
	   ((and (looking-at "\\(\n?Wrong username or password\n\\)?\
Username for [^\n]*\n?: Password: ")
		 (= (match-end 0) (point-max)))
	    (when (or (match-beginning 1)
		      (not (stringp w3m-process-passwd)))
	      (setq w3m-process-passwd
		    (w3m-process-read-passwd w3m-current-url
					     w3m-process-realm
					     w3m-process-user
					     (match-beginning 1))))
	    (ignore-errors
	      (process-send-string process
				   (concat w3m-process-passwd "\n"))
	      (delete-region (point-min) (point-max))))
	   ((and (looking-at "\\(\n?Wrong username or password\n\\)?\
Username for \\(.*\\)\n?: ")
		 (= (match-end 0) (point-max)))
	    (setq w3m-process-realm (w3m-decode-coding-string-with-priority
				     (match-string 2) nil))
	    (when (or (match-beginning 1)
		      (not (stringp w3m-process-user)))
	      (setq w3m-process-user
		    (w3m-process-read-user w3m-current-url
					   w3m-process-realm
					   (match-beginning 1))))
	    (ignore-errors
	      (process-send-string process
				   (concat w3m-process-user "\n"))))
	   ((and (looking-at "Enter PEM pass phrase:")
		 (= (match-end 0) (point-max)))
	    (unless (stringp w3m-process-ssl-passphrase)
	      (setq w3m-process-ssl-passphrase
		    (read-passwd "PEM pass phrase: ")))
	    (ignore-errors
	      (process-send-string process
				   (concat w3m-process-ssl-passphrase "\n"))
	      (delete-region (point-min) (point-max))))
	   ((progn
	      (or (search-forward "\nW3m-current-url:" nil t)
		  (goto-char (process-mark process)))
	      (re-search-backward
	       "^W3m-\\(?:in-\\)?progress: \\([.0-9]+/[.0-9]+[a-zA-Z]?b\\)$"
	       nil t))
	    (let ((str (w3m-process-modeline-format (match-string 1)))
		  (buf))
	      (save-current-buffer
		(dolist (handler (w3m-process-handlers w3m-process-object))
		  (when (setq buf (w3m-process-handler-parent-buffer handler))
		    (if (buffer-name buf)
			(progn
			  (set-buffer buf)
			  (setq w3m-process-modeline-string str))
		      (w3m-process-kill-stray-processes)))))))))))))

(defun w3m-process-modeline-format (str)
  (ignore-errors
    (cond
     ((stringp w3m-process-modeline-format)
      (format w3m-process-modeline-format
	      (if (string-match "/0\\([a-zA-Z]?b\\)\\'" str)
		  (replace-match "\\1" t nil str)
		str)))
     ((functionp w3m-process-modeline-format)
      (funcall w3m-process-modeline-format str)))))

;; w3m-process-authinfo-alist has an association list as below format.
;; (("root1" ("realm11" ("user11" . "pass11")
;;                      ("user12" . "pass12"))
;;           ("realm12" ("user13" . "pass13")))
;;  ("root2" ("realm21" ("user21" . "pass21"))))
(defun w3m-process-set-authinfo (url realm username password)
  (let (x y z (root (w3m-get-server-hostname url)))
    (if (setq x (assoc root w3m-process-authinfo-alist))
	(if (setq y (assoc realm x))
	    (if (setq z (assoc username y))
		;; Change a password only.
		(setcdr z password)
	      ;; Add a pair of a username and a password.
	      (setcdr y (cons (cons username password) (cdr y))))
	  ;; Add a 3-tuple of a realm, a username and a password.
	  (setcdr x (cons (cons realm (list (cons username password)))
			  (cdr x))))
      ;; Add a 4-tuple of a server root, a realm, a username and a password.
      (push (cons root (list (cons realm (list (cons username password)))))
	    w3m-process-authinfo-alist))))

(defun w3m-process-read-user (url &optional realm ignore-history)
  "Read a user name for URL and REALM."
  (let* ((root (when (stringp url) (w3m-get-server-hostname url)))
	 (ident (or realm root))
	 (alist))
    (if (and (not ignore-history)
	     (setq alist
		   (cdr (assoc realm
			       (cdr (assoc root
					   w3m-process-authinfo-alist))))))
	(if (= 1 (length alist))
	    (caar alist)
	  (completing-read (if ident
			       (format "Select username for %s: " ident)
			     "Select username: ")
			   (mapcar (lambda (x) (cons (car x) (car x))) alist)
			   nil t))
      (read-from-minibuffer (if ident
				(format "Username for %s: " ident)
			      "Username: ")))))

(defun w3m-process-read-passwd (url &optional realm username ignore-history)
  "Read a password for URL, REALM, and USERNAME."
  (let* ((root (when (stringp url) (w3m-get-server-hostname url)))
	 (ident (or realm root))
	 (pass (cdr (assoc username
			   (cdr (assoc realm
				       (cdr (assoc root
						   w3m-process-authinfo-alist))))))))
    (if (and pass (not ignore-history))
	pass
      (read-passwd (format (if ident
			       (format "Password for %s%%s: " ident)
			     "Password%s: ")
			   (if (and (stringp pass)
				    (> (length pass) 0)
				    (not (featurep 'xemacs)))
			       (concat " (default "
				       (make-string (length pass) ?\*)
				       ")")
			     ""))
		   nil pass))))

(defun w3m-process-y-or-n-p (url prompt)
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".
NOTE: This function is designed to avoid annoying questions.  So when
the same questions is reasked, its previous answer is reused without
prompt."
  (let ((root (w3m-get-server-hostname url))
	(map (copy-keymap query-replace-map))
	elem answer)
    ;; ignore [space] to avoid answering y without intention.
    (define-key map " " 'ignore)
    (let ((query-replace-map map))
      (if (setq elem (assoc root w3m-process-accept-alist))
	  (if (member prompt (cdr elem))
	      ;; When the same question has been asked, the previous
	      ;; answer is reused.
	      (setq answer t)
	    ;; When any question for the same server has been asked,
	    ;; regist the pair of this question and its answer to
	    ;; `w3m-process-accept-alist'.
	    (when (setq answer (y-or-n-p prompt))
	      (setcdr elem (cons prompt (cdr elem)))))
	;; When no question for the same server has been asked, regist
	;; the 3-tuple of the server, the question and its answer to
	;; `w3m-process-accept-alist'.
	(when (setq answer (y-or-n-p prompt))
	  (push (cons root (list prompt)) w3m-process-accept-alist)))
      answer)))

;; Silence the byte compiler complaining against `gensym' like:
;; "Warning: the function `gensym' might not be defined at runtime."
(eval-when-compile
  (and (boundp 'byte-compile-unresolved-functions)
       (fboundp 'gensym)
       (symbol-file 'gensym)
       (string-match "/cl-macs\\.el[^/]*\\'" (symbol-file 'gensym))
       (condition-case nil
	   (setq byte-compile-unresolved-functions
		 (delq (assq 'gensym byte-compile-unresolved-functions)
		       byte-compile-unresolved-functions))
	 (error))))

(provide 'w3m-proc)

;;; w3m-proc.el ends here
