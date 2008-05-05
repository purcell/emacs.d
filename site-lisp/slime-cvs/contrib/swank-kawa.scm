;;;; swank-kawa.scm --- Swank server for Kawa
;;;
;;; Copyright (C) 2007  Helmut Eller
;;;
;;; This file is licensed under the terms of the GNU General Public
;;; License as distributed with Emacs (press C-h C-c for details).

;;;; Installation 
;;
;; 1. You need Kawa (SVN version) 
;;    and a Sun JVM with debugger support.
;; 2. Compile this file with:
;;      kawa -e '(compile-file "swank-kawa.scm" "swank-kawa")'
;; 3. Add something like this to your .emacs:
#|
;; Kawa and the debugger classes (tools.jar) must be in the classpath.
;; You also need to start the debug agent.
(setq slime-lisp-implementations
      '((kawa ("java"
	       "-cp" "/opt/kawa/kawa-svn:/opt/java/jdk1.6.0/lib/tools.jar"
	       "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"
	       "kawa.repl")
              :init kawa-slime-init)))

(defun kawa-slime-init (file _)
  (setq slime-protocol-version 'ignore)
  (let ((zip ".../slime/contrib/swank-kawa.zip")) ; <-- insert the right path
    (format "%S\n"
            `(begin (load ,(expand-file-name zip)) (start-swank ,file)))))
|#
;; 4. Start everything with  M-- M-x slime kawa
;;
;;

;;;; Module declaration

(module-export start-swank create-swank-server swank-java-source-path)

(module-static #t)

(module-compile-options
 :warn-invoke-unknown-method #t
 :warn-undefined-variable #t
 )

(require 'hash-table)


;;;; Macros ()

(define-syntax df
  (syntax-rules (=>)
    ((df name (args ... => return-type) body ...)
     (define (name args ...) :: return-type body ...))
    ((df name (args ...) body ...)
     (define (name args ...) body ...))))

(define-syntax fun
  (syntax-rules ()
    ((fun (args ...) body ...)
     (lambda (args ...) body ...))))

(define-syntax fin
  (syntax-rules ()
    ((fin body handler ...)
     (try-finally body (seq handler ...)))))

(define-syntax seq
  (syntax-rules ()
    ((seq body ...)
     (begin body ...))))

(define-syntax esc
  (syntax-rules ()
    ((esc abort body ...)
     (let* ((key (<symbol>))
            (abort (lambda (val) (throw key val))))
       (catch key 
              (lambda () body ...)
              (lambda (key val) val))))))

(define-syntax !
  (syntax-rules ()
    ((! name obj args ...)
     (invoke obj 'name args ...))))

(define-syntax !!
  (syntax-rules ()
    ((!! name1 name2 obj args ...)
     (! name1 (! name2 obj args ...)))))

(define-syntax @
  (syntax-rules ()
    ((@ name obj)
     (field obj 'name))))

(define-syntax while
  (syntax-rules ()
    ((while exp body ...)
     (do () ((not exp)) body ...))))

(define-syntax dotimes 
  (syntax-rules ()
    ((dotimes (i n result) body ...)
     (let ((max :: <int> n))
       (do ((i :: <int> 0 (as <int> (+ i 1))))
           ((= i max) result)
           body ...)))
    ((dotimes (i n) body ...)
     (dotimes (i n #f) body ...))))

(define-syntax dolist 
  (syntax-rules ()
    ((dolist (e list) body ... )
     (for ((e list)) body ...))))

(define-syntax for
  (syntax-rules ()
    ((for ((var iterable)) body ...)
     (let ((iter (! iterator iterable)))
       (while (! has-next iter)
         ((lambda (var) body ...)
          (! next iter)))))))

(define-syntax packing
  (syntax-rules ()
    ((packing (var) body ...)
     (let ((var :: <list> '()))
       (let ((var (lambda (v) (set! var (cons v var)))))
         body ...)
       (reverse! var)))))

;;(define-syntax loop
;;  (syntax-rules (for = then collect until)
;;    ((loop for var = init then step until test collect exp)
;;     (packing (pack) 
;;       (do ((var init step))
;;           (test)
;;         (pack exp))))
;;    ((loop while test collect exp)
;;     (packing (pack) (while test (pack exp))))))

(define-syntax with
  (syntax-rules ()
    ((with (vars ... (f args ...)) body ...)
     (f args ... (lambda (vars ...) body ...)))))

(define-syntax pushf 
  (syntax-rules ()
    ((pushf value var)
     (set! var (cons value var)))))

(define-syntax ==
  (syntax-rules ()
    ((== x y)
     (eq? x y))))

(define-syntax set
  (syntax-rules ()
    ((set x y)
     (let ((tmp y))
       (set! x tmp)
       tmp))
    ((set x y more ...)
     (begin (set! x y) (set more ...)))))

(define-syntax assert
  (syntax-rules ()
    ((assert test)
     (seq
       (when (not test)
         (error "Assertion failed" 'test))
       'ok))
    ((assert test fstring args ...)
     (seq
       (when (not test)
         (error "Assertion failed" 'test (format #f fstring args ...)))
       'ok))))

(define-syntax mif
  (syntax-rules (unquote quote _ ::)
    ((mif ('x value) then else)
     (if (equal? 'x value) then else))
    ((mif (,x value) then else)
     (if (eq? x value) then else))
    ((mif (() value) then else)
     (if (eq? value '()) then else))
    ((mif ((p . ps) value) then else)
     (let ((tmp value)
           (fail? :: <int> 0)
           (result #!null))
       (if (instance? tmp <pair>)
           (let ((tmp :: <pair> tmp))
             (mif (p tmp:car)
                (mif (ps tmp:cdr)
                     (set! result then)
                     (set! fail? -1))
                (set! fail? -1)))
           (set! fail? -1))
       (if (= fail? 0) result else)))
    ((mif (_ value) then else)
     then)
    ((mif (var value) then else)
     (let ((var value)) then))
    ((mif (pattern value) then)
     (mif (pattern value) then (values)))))

(define-syntax mcase
  (syntax-rules ()
    ((mcase exp (pattern body ...) more ...)
     (let ((tmp exp))
       (mif (pattern tmp)
            (begin body ...)
            (mcase tmp more ...))))
    ((mcase exp) (ferror "mcase failed ~s\n~a" 'exp (pprint-to-string exp)))))

(define-syntax mlet
  (syntax-rules ()
    ((mlet (pattern value) body ...)
     (let ((tmp value))
       (mif (pattern tmp)
            (begin body ...)
            (error "mlet failed" tmp))))))

(define-syntax mlet* 
  (syntax-rules ()
    ((mlet* () body ...) (begin body ...))
    ((mlet* ((pattern value) ms ...) body ...)
     (mlet (pattern value) (mlet* (ms ...) body ...)))))

(define-syntax typecase 
  (syntax-rules (::)
    ((typecase var (type body ...) ...)
     (cond ((instance? var type) 
            (let ((var :: type var))
              body ...))
           ...
           (else (error "typecase failed" var 
                        (! getClass (as <object> var))))))))

(define-syntax ignore-errors
  (syntax-rules ()
    ((ignore-errors body ...)
     (try-catch (begin body ...)
                (v <java.lang.Exception> #f)))))

;;(define-syntax dc
;;  (syntax-rules ()
;;    ((dc name () %% (props ...) prop more ...)
;;     (dc name () %% (props ... (prop <object>)) more ...))
;;    ;;((dc name () %% (props ...) (prop type) more ...)
;;    ;; (dc name () %% (props ... (prop type)) more ...))
;;    ((dc name () %% ((prop type) ...))
;;     (define-simple-class name () 
;;                          ((*init* (prop :: type) ...)
;;                           (set (field (this) 'prop) prop) ...)
;;                          (prop :type type) ...))
;;    ((dc name () props ...)
;;     (dc name () %% () props ...))))


;;;; Aliases

(define-alias <server-socket> <java.net.ServerSocket>)
(define-alias <socket> <java.net.Socket>)
(define-alias <in> <java.io.InputStreamReader>)
(define-alias <out> <java.io.OutputStreamWriter>)
(define-alias <file> <java.io.File>)
(define-alias <str> <java.lang.String>)
(define-alias <builder> <java.lang.StringBuilder>)
(define-alias <throwable> <java.lang.Throwable>)
(define-alias <source-error> <gnu.text.SourceError>)
(define-alias <module-info> <gnu.expr.ModuleInfo>)
(define-alias <iterable> <java.lang.Iterable>)
(define-alias <thread> <java.lang.Thread>)
(define-alias <queue> <java.util.concurrent.LinkedBlockingQueue>)
(define-alias <exchanger> <java.util.concurrent.Exchanger>)
(define-alias <timeunit> <java.util.concurrent.TimeUnit>)
(define-alias <vm> <com.sun.jdi.VirtualMachine>)
(define-alias <mirror> <com.sun.jdi.Mirror>)
(define-alias <value> <com.sun.jdi.Value>)
(define-alias <thread-ref> <com.sun.jdi.ThreadReference>)
(define-alias <obj-ref> <com.sun.jdi.ObjectReference>)
(define-alias <array-ref> <com.sun.jdi.ArrayReference>)
(define-alias <str-ref> <com.sun.jdi.StringReference>)
(define-alias <meth-ref> <com.sun.jdi.Method>)
(define-alias <class-ref> <com.sun.jdi.ClassType>)
(define-alias <frame> <com.sun.jdi.StackFrame>)
(define-alias <field> <com.sun.jdi.Field>)
(define-alias <local-var> <com.sun.jdi.LocalVariable>)
(define-alias <location> <com.sun.jdi.Location>)
(define-alias <absent-exc> <com.sun.jdi.AbsentInformationException>)
(define-alias <ref-type> <com.sun.jdi.ReferenceType>)
(define-alias <event> <com.sun.jdi.event.Event>)
(define-alias <exception-event> <com.sun.jdi.event.ExceptionEvent>)
(define-alias <step-event> <com.sun.jdi.event.StepEvent>)
(define-alias <env> <gnu.mapping.Environment>)

(define-simple-class <chan> ()
  (owner :: <thread> :init (java.lang.Thread:currentThread))
  (peer :: <chan>)
  (queue :: <queue> :init (<queue>))
  (lock :init (<object>)))


;;;; Entry Points

(df create-swank-server (port-number) 
  (setup-server port-number announce-port))

(df start-swank (port-file)
  (let ((announce (fun ((socket <server-socket>))
                    (with (f (call-with-output-file port-file))
                      (format f "~d\n" (! get-local-port socket))))))
    (spawn (fun ()
             (setup-server 0 announce)))))

(df setup-server ((port-number <int>) announce)
  (! set-name (current-thread) "swank")
  (let ((s (<server-socket> port-number)))
    (announce s)
    (let ((c (! accept s)))
      (! close s)
      (log "connection: ~s\n"  c)
      (fin (dispatch-events c)
        (log "closing socket: ~a\n" s)
        (! close c)))))

(df announce-port ((socket <server-socket>))
  (log "Listening on port: ~d\n" (! get-local-port socket)))


;;;; Event dispatcher

(define-variable *the-vm* #f)
(define-variable *last-exception* #f)
(define-variable *last-stacktrace* #f)

(df dispatch-events ((s <socket>))
  (mlet* ((charset "iso-8859-1")
          (ins (<in> (! getInputStream s) charset))
          (outs (<out> (! getOutputStream s) charset))
          ((in . _) (spawn/chan/catch (fun (c) (reader ins c))))
          ((out . _) (spawn/chan/catch (fun (c) (writer outs c))))
          ((dbg . _) (spawn/chan/catch vm-monitor))
          (user-env 
           (<gnu.mapping.InheritingEnvironment>
            "user" (interaction-environment))
           ;;(interaction-environment)
           )
          (x (seq 
               (! set-flag user-env #t #|<env>:THREAD_SAFE|# 8)
               (! set-flag user-env #f #|<env>:DIRECT_INHERITED_ON_SET|# 16)))
          ((listener . _)
           (spawn/chan (fun (c) (listener c user-env))))
          (inspector #f)
          (threads '())
          (repl-thread #f)
          (extra '())
          (vm (let ((vm #f)) (fun () (or vm (rpc dbg `(get-vm)))))))
    (while #t
      (mlet ((c . event) (recv* (append (list in out dbg listener)
                                        (if inspector (list inspector) '())
                                        (map car threads)
                                        extra)))
        ;;(log "event: ~s\n" event)
        (mcase (list c event)
          ((_ (':emacs-rex ('|swank:debugger-info-for-emacs| from to)
                           pkg thread id))
           (send dbg `(debug-info ,thread ,from ,to ,id)))
          ((_ (':emacs-rex ('|swank:throw-to-toplevel|) pkg thread id))
           (send dbg `(throw-to-toplevel ,thread ,id)))
          ((_ (':emacs-rex ('|swank:sldb-continue|) pkg thread id))
           (send dbg `(thread-continue ,thread ,id)))
          ((_ (':emacs-rex ('|swank:frame-source-location-for-emacs| frame)
                           pkg thread id))
           (send dbg `(frame-src-loc ,thread ,frame ,id)))
          ((_ (':emacs-rex ('|swank:frame-locals-for-emacs| frame)
                           pkg thread id))
           (send dbg `(frame-locals ,thread ,frame ,id)))
          ((_ (':emacs-rex ('|swank:frame-catch-tags-for-emacs| frame)
                           pkg thread id))
           (send dbg `(frame-catchers ,thread ,frame ,id)))
          ((_ (':emacs-rex ('|swank:sldb-disassemble| frame)
                           pkg thread id))
           (send dbg `(disassemble-frame ,thread ,frame ,id)))
          ((_ (':emacs-rex ('|swank:backtrace| from to) pkg thread id))
           (send dbg `(thread-frames ,thread ,from ,to ,id)))
          ((_ (':emacs-rex ('|swank:list-threads|) pkg thread id))
           (send dbg `(list-threads ,id)))
          ((_ (':emacs-rex ('|swank:debug-nth-thread| n) _  _ _))
           (send dbg `(debug-nth-thread ,n)))
          ((_ (':emacs-rex ('|swank:quit-thread-browser|) _  _ id))
           (send dbg `(quit-thread-browser ,id)))
          ((_ (':emacs-rex ('|swank:init-inspector| str . _) pkg _ id))
           (set inspector (make-inspector user-env (vm)))
           (send inspector `(init ,str ,id)))
          ((_ (':emacs-rex ('|swank:inspect-frame-var| frame var) 
                           pkg thread id))
           (mlet ((im . ex) (chan))
             (set inspector (make-inspector user-env (vm)))
             (send dbg `(get-local ,ex ,thread ,frame ,var))
             (send inspector `(init-mirror ,im ,id))))
          ((_ (':emacs-rex ('|swank:inspect-current-condition|) pkg thread id))
           (mlet ((im . ex) (chan))
             (set inspector (make-inspector user-env (vm)))
             (send dbg `(get-exception ,ex ,thread))
             (send inspector `(init-mirror ,im ,id))))
          ((_ (':emacs-rex ('|swank:inspect-nth-part| n) pkg _ id))
           (send inspector `(inspect-part ,n ,id)))
          ((_ (':emacs-rex ('|swank:inspector-pop|) pkg _ id))
           (send inspector `(pop ,id)))
          ((_ (':emacs-rex ('|swank:quit-inspector|) pkg _ id))
           (send inspector `(quit ,id)))
          ((_ (':emacs-interrupt id))
           (let* ((vm (vm))
                  (t (find-thread id (map cdr threads) repl-thread vm)))
             (send dbg `(debug-thread ,t))))
          ((_ (':emacs-rex form _ _ id))
           (send listener `(,form ,id)))
          ((_ ('get-vm c))
           (send dbg `(get-vm ,c)))
          ((_ ('get-channel c))
           (mlet ((im . ex) (chan))
             (pushf im extra)
             (send c ex)))
          ((_ ('forward x))
           (send out x))
          ((_ ('set-listener x))
           (set repl-thread x))
          ((_ ('publish-vm vm))
           (set *the-vm* vm))
          )))))

(df find-thread (id threads listener (vm <vm>))
  (cond ((== id :repl-thread) listener)
        ((== id 't) listener
         ;;(if (null? threads) 
         ;;    listener 
         ;;    (vm-mirror vm (car threads)))
         )
        (#t 
         (let ((f (find-if threads 
                      (fun (t :: <thread>)
                        (= id (! uniqueID 
                                 (as <thread-ref> (vm-mirror vm t)))))
                      #f)))
           (cond (f (vm-mirror vm f))
                 (#t listener))))))


;;;; Reader thread

(df reader ((in <in>) (c <chan>))
  (! set-name (current-thread) "swank-net-reader")
  (let ((rt (gnu.kawa.lispexpr.ReadTable:createInitial))) ; ':' not special
    (while #t
      (send c (decode-message in rt)))))

(df decode-message ((in <in>) (rt  <gnu.kawa.lispexpr.ReadTable>) => <list>)
  (let* ((header (read-chunk in 6))
         (len (java.lang.Integer:parseInt header 16)))
    (call-with-input-string (read-chunk in len) 
                            (fun ((port <input-port>))
                              (%read port rt)))))

(df read-chunk ((in <in>) (len <int>) => <str>)
  (let ((chars (<char[]> :length len)))
    (let loop ((offset :: <int> 0))
      (cond ((= offset len) (<str> chars))
            (#t (let ((count (! read in chars offset (- len offset))))
                  (assert (not (= count -1)) "partial packet")
                  (loop (+ offset count))))))))

;;; FIXME: not thread safe
(df %read ((port <gnu.mapping.InPort>) (table <gnu.kawa.lispexpr.ReadTable>))
  ;;  (parameterize ((current-readtable table))
  ;;                (read)))
  (let ((old (gnu.kawa.lispexpr.ReadTable:getCurrent)))
    (try-finally
     (seq (gnu.kawa.lispexpr.ReadTable:setCurrent table)
          (read port))
     (gnu.kawa.lispexpr.ReadTable:setCurrent old))))


;;;; Writer thread

(df writer ((out <out>) (c <chan>))
  (! set-name (current-thread) "swank-net-writer")
  (while #t
    (encode-message out (recv c))))

(df encode-message ((out <out>) (message <list>))
  (let ((builder (<builder> (as <int> 512))))
    (print-for-emacs message builder)
    (! write out (! toString (format "~6,'0x" (! length builder))))
    (! write out builder)
    (! flush out)))

(df print-for-emacs (obj (out <builder>))
  (let ((pr (fun (o) (! append out (! toString (format "~s" o)))))
        (++ (fun ((s <string>)) (! append out (! toString s)))))
    (cond ((null? obj) (++ "nil"))
          ((string? obj) (pr obj))
          ((number? obj) (pr obj))
          ((keyword? obj) (++ ":") (! append out (to-str obj)))
          ((symbol? obj) (pr obj))
          ((pair? obj)
           (++ "(")
           (let loop ((obj obj))
             (print-for-emacs (car obj) out)
             (let ((cdr (cdr obj)))
               (cond ((null? cdr) (++ ")"))
                     ((pair? cdr) (++ " ") (loop cdr))
                     (#t (++ " . ") (print-for-emacs cdr out) (++ ")"))))))
          (#t (error "Unprintable object" obj)))))

;;;; SLIME-EVAL

(df eval-for-emacs ((form <list>) env (id <int>) (c <chan>))
  ;;(! set-uncaught-exception-handler (current-thread)
  ;;   (<ucex-handler> (fun (t e) (reply-abort c id))))
  (reply c (%eval form env) id))

(define-variable *slime-funs*)
(set *slime-funs* (tab))

(df %eval (form env)
  (apply (lookup-slimefun (car form) *slime-funs*) env (cdr form)))

(df lookup-slimefun ((name <symbol>) tab)
  ;; name looks like '|swank:connection-info|
  (let* ((str (symbol->string name))
         (sub (substring str 6 (string-length str))))
    (or (get tab (string->symbol sub) #f)
        (ferror "~a not implemented" sub))))
                         
(define-syntax defslimefun 
  (syntax-rules ()
    ((defslimefun name (args ...) body ...)
     (seq
       (df name (args ...) body ...)
       (put *slime-funs* 'name name)))))

(defslimefun connection-info ((env <env>))
  (let ((prop java.lang.System:getProperty))
  `(:pid 
    0 
    :style :spawn
    :lisp-implementation (:type "Kawa" :name "kawa" 
                                :version ,(scheme-implementation-version))
    :machine (:instance ,(prop "java.vm.name") :type ,(prop "os.name")
                        :version ,(prop "java.runtime.version"))
    :features ()
    :package (:name "??" :prompt ,(! getName env)))))

 
;;;; Listener

(df listener ((c <chan>) (env <env>))
  (! set-name (current-thread) "swank-listener")
  (log "listener: ~s ~s ~s ~s\n" 
       (current-thread) ((current-thread):hashCode) c env)
  (let ((out (make-swank-outport (rpc c `(get-channel)))))
    (set (current-output-port) out)
    (let ((vm (as <vm> (rpc c `(get-vm)))))
      (send c `(set-listener ,(vm-mirror vm (current-thread))))
      (enable-uncaught-exception-events vm))
    (rpc c `(get-vm))
    (listener-loop c env out)))

(df listener-loop ((c <chan>) (env <env>) port)
  (while (not (nul? c))
    ;;(log "listener-loop: ~s ~s\n" (current-thread) c)
    (mlet ((form id) (recv c))
      (let ((restart (fun ()
                       (close-output-port port)
                       (reply-abort c id)
                       (send (car (spawn/chan
                                   (fun (cc) 
                                     (listener (recv cc) env)))) 
                             c)
                       (set c #!null))))
        (! set-uncaught-exception-handler (current-thread)
           (<ucex-handler> (fun (t e) (restart))))
        (try-catch
         (let* ((val (%eval form env)))
           (force-output)
           (reply c val id))
         (ex <listener-abort>
             (let ((flag (java.lang.Thread:interrupted)))
               (log "listener-abort: ~s ~a\n" ex flag))
             (restart)))))))

(defslimefun interactive-eval (env str)
  (values-for-echo-area (eval (read-from-string str) env)))

(defslimefun interactive-eval-region (env (s <string>))
  (with (port (call-with-input-string s))
    (values-for-echo-area
     (let next ((result (values)))
       (let ((form (read port)))
         (cond ((== form #!eof) result)
               (#t (next (eval form env)))))))))

(defslimefun listener-eval (env string)
  (let* ((form (read-from-string string))
         (list (values-to-list (eval form env))))
  `(:values ,@(map pprint-to-string list))))

(defslimefun pprint-eval (env string)
  (let* ((form (read-from-string string))
         (l (values-to-list (eval form env))))
    (apply cat (map pprint-to-string l))))

(df call-with-abort (f)
  (try-catch (f) (ex <throwable> (exception-message ex))))

(df exception-message ((ex <throwable>))
  (typecase ex
    (<kawa.lang.NamedException> (! to-string ex))
    (<throwable> (format "~a: ~a"
                         (class-name-sans-package ex)
                         (! getMessage ex)))))

(df values-for-echo-area (values)
  (let ((values (values-to-list values)))
    (format "~:[=> ~{~s~^, ~}~;; No values~]" (null? values) values)))

;;;; Compilation

(define-constant compilation-messages (<gnu.text.SourceMessages>))

(defslimefun compile-file-for-emacs (env (filename <str>) load?)
  (let ((zip (cat (path-sans-extension (filepath filename)) ".zip")))
    (wrap-compilation 
     (fun () (kawa.lang.CompileFile:read filename compilation-messages))
     zip (if (lisp-bool load?) env #f) #f)))

(df wrap-compilation (f zip env delete?)
  (! clear compilation-messages)
  (let ((start-time (current-time)))
    (try-catch
     (let ((c (as <gnu.expr.Compilation> (f))))
       (! compile-to-archive c (! get-module c) zip))
     (ex <throwable>
         (log "error during compilation: ~a\n" ex)
         (! error compilation-messages (as <char> #\f)
            (to-str (exception-message ex)) #!null)))
    (log "compilation done.\n")
    (when (and env
               (zero? (! get-error-count compilation-messages)))
      (log "loading ...\n")
      (eval `(load ,zip) env)
      (log "loading ... done.\n"))
    (when delete?
      (ignore-errors (delete-file zip)))
    (let ((end-time (current-time)))
      (list 'nil (format "~3f" (/ (- end-time start-time) 1000))))))

(defslimefun compile-string-for-emacs (env string buffer offset dir)
  (wrap-compilation
   (fun ()
     (let ((c (as <gnu.expr.Compilation>
                  (call-with-input-string 
                   string
                   (fun ((p <gnu.mapping.InPort>))
                     (! set-path p 
                        (format "~s" 
                                `(buffer ,buffer offset ,offset str ,string)))
                     (kawa.lang.CompileFile:read p compilation-messages))))))
       (let ((o (@ currentOptions c)))
         (! set o "warn-invoke-unknown-method" #t)
         (! set o "warn-undefined-variable" #t))
       (let ((m (! getModule c)))
         (! set-name m (format "<emacs>:~a/~a" buffer (current-time))))
       c))
   "/tmp/kawa-tmp.zip" env #t))

(defslimefun compiler-notes-for-emacs (env) 
  (packing (pack)
    (do ((e (! get-errors compilation-messages) (@ next e)))
        ((nul? e))
      (pack (source-error>elisp e)))))

(df source-error>elisp ((e <source-error>) => <list>)
  (list :message (to-string (@ message e))
        :severity (case (integer->char (@ severity e))
                    ((#\e #\f) :error)
                    ((#\w) :warning)
                    (else :note))
        :location (error-loc>elisp e)))

(df error-loc>elisp ((e <source-error>))
  (cond ((nul? (@ filename e)) `(:error "No source location"))
        ((! starts-with (@ filename e) "(buffer ")
         (mlet (('buffer b 'offset o 'str s) (read-from-string (@ filename e)))
           `(:location (:buffer ,b)
                       (:position ,(+ o (line>offset (1- (@ line e)) s)
                                      (1- (@ column e))))
                       nil)))
        (#t
         `(:location (:file ,(to-string (@ filename e)))
                     (:line ,(@ line e) ,(1- (@ column e)))
                     nil))))

(df line>offset ((line <int>) (s <str>) => <int>)
  (let ((offset :: <int> 0))
    (dotimes (i line)
      (set offset (! index-of s (as <char> #\newline) offset))
      (assert (>= offset 0))
      (set offset (as <int> (+ offset 1))))
    (log "line=~a offset=~a\n" line offset)
    offset))

(defslimefun load-file (env filename)
  (format "Loaded: ~a => ~s" filename (eval `(load ,filename) env)))

;;;; Completion

(defslimefun simple-completions (env (pattern <str>) _)
  (let* ((env (as <gnu.mapping.InheritingEnvironment> env))
         (matches (packing (pack)
                    (let ((iter (! enumerate-all-locations env)))
                      (while (! has-next iter)
                        (let ((l (! next-location iter)))
                          (typecase l
                            (<gnu.mapping.NamedLocation>
                             (let ((name (!! get-name get-key-symbol l)))
                               (when (! starts-with name pattern)
                                 (pack name)))))))))))
    `(,matches ,(cond ((null? matches) pattern)
                      (#t (fold+ common-prefix matches))))))

(df common-prefix ((s1 <str>) (s2 <str>) => <str>)
  (let ((limit (min (! length s1) (! length s2))))
    (let loop ((i 0))
      (cond ((or (= i limit)
                 (not (== (! char-at s1 i)
                          (! char-at s2 i))))
             (! substring s1 0 i))
            (#t (loop (1+ i)))))))

(df fold+ (f list)
  (let loop ((s (car list))
             (l (cdr list)))
    (cond ((null? l) s)
          (#t (loop (f s (car l)) (cdr l))))))

;;; Quit

(defslimefun quit-lisp (env)
  (exit))

;;;; Dummy defs

(defslimefun operator-arglist (#!rest y) '())
(defslimefun buffer-first-change (#!rest y) '())

;;;; M-.

(defslimefun find-definitions-for-emacs (env name)
  (mcase (try-catch `(ok ,(eval (read-from-string name) env))
                    (ex <throwable> `(error ,(exception-message ex))))
    (('ok obj) (mapi (all-definitions obj)
                     (fun (d)
                       `(,(format "~a" d) ,(src-loc>elisp (src-loc d))))))
    (('error msg) `((,name (:error ,msg))))))

(define-simple-class <swank-location> (<location>)
  (file :init #f)
  (line :init #f)
  ((*init* file name) 
   (set (@ file (this)) file)
   (set (@ line (this)) line))
  ((lineNumber) :: <int> (or line (absent)))
  ((lineNumber (s <str>)) :: int (! lineNumber (this)))
  ((method) :: <meth-ref> (absent))
  ((sourcePath) :: <str> (or file (absent)))
  ((sourcePath (s <str>)) :: <str> (! sourcePath (this)))
  ((sourceName) :: <str> (absent))
  ((sourceName (s <str>)) :: <str> (! sourceName (this)))
  ((declaringType) :: <ref-type> (absent))
  ((codeIndex) :: <long> -1)
  ((virtualMachine) :: <vm> *the-vm*)
  ((compareTo o) :: <int>
   (typecase o
     (<location> (- (! codeIndex (this)) (! codeIndex o))))))

(df absent () (primitive-throw (<absent-exc>)))

(df all-definitions (o)
  (typecase o
    (<gnu.expr.ModuleMethod> (list o))
    (<gnu.expr.GenericProc> (append (mappend all-definitions (gf-methods o))
                                    (let ((s (! get-setter o)))
                                      (if s (all-definitions s) '()))))
    (<java.lang.Class> (list o))
    (<gnu.mapping.Procedure> (all-definitions (! get-class o)))
    (<kawa.lang.Macro> (list o))))

(df gf-methods ((f <gnu.expr.GenericProc>))
  (let* ((o :: <obj-ref> (vm-mirror *the-vm* f))
         (f (! field-by-name (! reference-type o) "methods"))
         (ms (vm-demirror *the-vm* (! get-value o f))))
    (filter (array-to-list ms) (fun (x) (not (nul? x))))))

(df src-loc (o => <location>)
  (typecase o
    (<gnu.expr.ModuleMethod> (module-method>src-loc o))
    (<gnu.expr.GenericProc> (<swank-location> #f #f))
    (<java.lang.Class> (class>src-loc o))
    (<kawa.lang.Macro> (<swank-location> #f #f))))

(df module-method>src-loc ((f <gnu.expr.ModuleMethod>))
  (! location (module-method>meth-ref f)))

(df module-method>meth-ref ((f <gnu.expr.ModuleMethod>) => <meth-ref>)
  (let ((module (! reference-type
                   (as <obj-ref> (vm-mirror *the-vm* (@ module f)))))
	(name (mangled-name f)))
    (as <meth-ref> (1st (! methods-by-name module name)))))

(df mangled-name ((f <gnu.expr.ModuleMethod>))
  (let ((name (gnu.expr.Compilation:mangleName (! get-name f))))
    (if (= (! maxArgs f) -1)
        (cat name "$V")
        name)))

(df class>src-loc ((c <java.lang.Class>) => <location>)
  (let* ((type (! reflectedType (as <com.sun.jdi.ClassObjectReference>
                                    (vm-mirror *the-vm* c))))
         (locs (! all-line-locations type)))
    (cond ((not (! isEmpty locs)) (1st locs))
          (#t (<swank-location> (1st (! source-paths type #!null))
                                #f)))))

(df src-loc>elisp ((l <location>))
  (df src-loc>list ((l <location>))
    (list (ignore-errors (! source-name l))
          (ignore-errors (! source-path l))
          (ignore-errors (! line-number l))))
  (mcase (src-loc>list l)
    ((name path line)
     (cond ((not path) 
            `(:error ,(call-with-abort (fun () (! source-path l)))))
           ((! starts-with (as <str> path) "(buffer ")
            (mlet (('buffer b 'offset o 'str s) (read-from-string path))
              `(:location (:buffer ,b)
                          (:position ,(+ o (line>offset line s)))
                          nil)))
           (#t
            `(:location ,(or (find-file-in-path name (source-path))
                             (find-file-in-path path (source-path))
                             (ferror "Can't find source-path: ~s ~s ~a" 
                                     path name (source-path)))
                        (:line ,(or line -1)) ()))))))


(df src-loc>str ((l <location>))
  (cond ((nul? l) "<null-location>")
        (#t (format "~a ~a ~a" 
                    (or (ignore-errors (! source-path l))
                        (ignore-errors (! source-name l))
                        (ignore-errors (!! name declaring-type l)))
                    (ignore-errors (!! name method l))
                    (ignore-errors (! lineNumber l))))))

(df ferror (fstring #!rest args)
  (primitive-throw (<java.lang.Error> (to-str (apply format fstring args)))))

;;;;;; class-path hacking

(df find-file-in-path ((filename <str>) (path <list>))
  (let ((f (<file> filename)))
    (cond ((! isAbsolute f) `(:file ,filename))
          (#t (let ((result #f))
                (find-if path (fun (dir) 
                                (let ((x (find-file-in-dir f dir)))
                                  (set result x)))
                         #f)
                result)))))

(df find-file-in-dir ((file <file>) (dir <str>))
  (let ((filename (! getPath file)))
    (or (let ((child (<file> (<file> dir) filename)))
          (and (! exists child)
               `(:file ,(! getPath child))))
        (try-catch 
         (and (not (nul? (! getEntry (<java.util.zip.ZipFile> dir) filename)))
              `(:zip ,dir ,filename))
         (ex <throwable> #f)))))

(define swank-java-source-path
        (let ((jre-home (<java.lang.System>:getProperty "java.home")))
          (list (<file> (<file> jre-home):parent "src.zip"):path)))

(df source-path ()
  (mlet ((base) (search-path-prop "user.dir"))
    (append 
     (list base)
     (map (fun ((s <str>))
             (let ((f (<file> s)))
               (cond ((! isAbsolute f) s)
                     (#t (<file> (as <str> base) s):path))))
          (class-path))
     swank-java-source-path)))

(df class-path ()
  (append (search-path-prop "java.class.path")
          (search-path-prop "sun.boot.class.path")))

(df search-path-prop ((name <str>))
  (array-to-list (! split (java.lang.System:getProperty name)
                    <file>:pathSeparator)))

;;;; Disassemble 

(defslimefun disassemble-symbol (env name)
  (let ((f (eval (read-from-string name) env)))
    (typecase f
      (<gnu.expr.ModuleMethod> 
       (disassemble (module-method>meth-ref f))))))

(df disassemble ((mr <meth-ref>) => <str>)
  (with-sink #f (fun (out) (disassemble-meth-ref mr out))))

(df disassemble-meth-ref ((mr <meth-ref>) (out <java.io.PrintWriter>))
  (let* ((t (! declaring-type mr)))
    (disas-header mr out)
    (disas-code (! constant-pool t)
                (! constant-pool-count t)
                (! bytecodes mr)
                out)))

(df disas-header ((mr <meth-ref>) (out <java.io.PrintWriter>))
  (let* ((++ (fun ((str <str>)) (! write out str)))
         (? (fun (flag str) (if flag (++ str)))))
    (? (! is-static mr) "static ") 
    (? (! is-final mr) "final ")
    (? (! is-private mr) "private ") 
    (? (! is-protected mr) "protected ")
    (? (! is-public mr) "public ")
    (++ (! name mr)) (++ (! signature mr)) (++ "\n")))

(df disas-code ((cpool <byte[]>) (cpoolcount <int>) (bytecode <byte[]>) 
                (out <java.io.PrintWriter>))
  (let* ((ct (<gnu.bytecode.ClassType> "foo"))
	 (met (! addMethod ct "bar" 0))
	 (ca (<gnu.bytecode.CodeAttr> met))
         (constants (let* ((bs (<java.io.ByteArrayOutputStream>))
                           (s (<java.io.DataOutputStream> bs)))
                      (! write-short s cpoolcount)
                      (! write s cpool)
                      (! flush s)
                      (! toByteArray bs))))
    (vm-set-slot *the-vm* ct 'constants 
                 (<gnu.bytecode.ConstantPool>
                  (<java.io.DataInputStream>
                   (<java.io.ByteArrayInputStream>
                    constants))))
    (! setCode ca bytecode)
    (let ((w (<gnu.bytecode.ClassTypeWriter> ct out 0)))
      (! print ca w)
      (! flush w))))

(df with-sink (sink (f <function>))
  (cond ((instance? sink <java.io.PrintWriter>) (f sink))
        ((== sink #t) (f (as <java.io.PrintWriter> (current-output-port))))
        ((== sink #f)
         (let* ((buffer (<java.io.StringWriter>))
                (out (<java.io.PrintWriter> buffer)))
           (f out)
           (! flush out)
           (! toString buffer)))
        (#t (ferror "Invalid sink designator: ~s" sink))))

(df test-disas ((c <str>) (m <str>))
  (let* ((vm (as <vm> *the-vm*))
         (c (as <ref-type> (1st (! classes-by-name vm c))))
         (m (as <meth-ref> (1st (! methods-by-name c m)))))
    (with-sink #f (fun (out) (disassemble-meth-ref m out)))))

;; (test-disas "java.lang.Class" "toString")


;;;; Macroexpansion

(defslimefun swank-macroexpand-1 (env s) (%swank-macroexpand s))
(defslimefun swank-macroexpand (env s) (%swank-macroexpand s))
(defslimefun swank-macroexpand-all (env s) (%swank-macroexpand s))

(df %swank-macroexpand (string)
  (pprint-to-string (%macroexpand (read-from-string string))))

(df %macroexpand (sexp)
  (let* ((lang (<gnu.expr.Language>:getDefaultLanguage))
	 (msgs (<gnu.text.SourceMessages>))
	 (tr (<kawa.lang.Translator> lang msgs)))
    (! pushNewModule tr (as <str> #!null))
    (! parse tr `(lambda () ,sexp))))


;;;; Inspector

(define-simple-class <inspector-state> () 
  (object :init #!null) 
  (parts :: <java.util.ArrayList> :init (<java.util.ArrayList>) )
  (stack :: <list> :init '())
  (content :: <list> :init '()))

(df make-inspector (env (vm <vm>) => <chan>)
  (car (spawn/chan (fun (c) (inspector c env vm)))))

(df inspector ((c <chan>) env (vm <vm>))
  (! set-name (current-thread) "inspector")
  (let ((state :: <inspector-state> (<inspector-state>))
        (open #t))
    (while open
      (mcase (recv c)
        (('init str id)
         (set state (<inspector-state>))
         (let ((obj (try-catch (eval (read-from-string str) env)
                               (ex <throwable> ex))))
           (reply c (inspect-object obj state vm) id)))
        (('init-mirror cc id)
         (set state (<inspector-state>))
         (let* ((mirror (recv cc))
                (obj (vm-demirror vm mirror)))
           (reply c (inspect-object obj state vm) id)))
        (('inspect-part n id)
         (let ((part (! get (@ parts state) n)))
           (reply c (inspect-object part state vm) id)))
        (('pop id)
         (reply c (inspector-pop state vm) id))
        (('quit id)
         (reply c 'nil id)
         (set open #f))))))

(df inspect-object (obj (state <inspector-state>) (vm <vm>))
  (set (@ object state) obj)
  (set (@ parts state) (<java.util.ArrayList>))
  (pushf obj (@ stack state))
  (set (@ content state) (inspector-content 
                          `("class: " (:value ,(! getClass obj)) "\n" 
                            ,@(inspect obj vm))
                          state))
  (cond ((nul? obj) (list :title "#!null" :id 0 :content `()))
        (#t
         (list :title (pprint-to-string obj) 
               :id (assign-index obj state)
               :content (let ((c (@ content state)))
                          (content-range  c 0 (len c)))))))

(df inspect (obj vm)
  (let* ((obj (as <obj-ref> (vm-mirror vm obj))))
    (packing (pack)
      (typecase obj
        (<array-ref>
         (let ((i 0))
           (iter (! getValues obj)
                 (fun ((v <value>))
                   (pack (format "~d: " i))
                   (set i (1+ i))
                   (pack `(:value ,(vm-demirror vm v)))
                   (pack "\n")))))
        (<obj-ref>
         (let* ((type (! referenceType obj))
                (fields (! allFields type))
                (values (! getValues obj fields)))
           (iter fields 
                 (fun ((f <field>))
                   (let ((val (as <value> (! get values f))))
                     (when (! is-static f)
                       (pack "static "))
                     (pack (! name f)) (pack ": ") 
                     (pack `(:value ,(vm-demirror vm val)))
                     (pack "\n"))))))))))

(df inspector-content (content (state <inspector-state>))
  (map (fun (part)
         (mcase part
           ((':value val)
            `(:value ,(pprint-to-string val) ,(assign-index val state)))
           (x (to-string x))))
       content))

(df assign-index (obj (state <inspector-state>) => <int>)
  (! add (@ parts state) obj)
  (1- (! size  (@ parts state))))

(df content-range (l start end)
  (let* ((len (length l)) (end (min len end)))
    (list (subseq l start end) len start end)))

(df inspector-pop ((state <inspector-state>) vm)
  (cond ((<= 2 (len (@ stack state)))
         (let ((obj (cadr (@ stack state))))
           (set (@ stack state) (cddr (@ stack state)))
           (inspect-object obj state vm)))
        (#t 'nil)))

;;;; IO redirection

(define-simple-class <swank-writer> (<java.io.Writer>)
  (q :: <queue> :init (<queue> (as <int> 100)))
  ((*init*) (invoke-special <java.io.Writer> (this) '*init*))
  ((write (buffer <char[]>) (from <int>) (to <int>)) :: <void>
   (synchronized (this)
     (assert (not (== q #!null)))
     (! put q `(write ,(<str> buffer from to)))))
  ((close) :: <void>
   (synchronized (this)
     (! put q 'close)
     (set! q #!null)))
  ((flush) :: <void>
   (synchronized (this)
     (assert (not (== q #!null)))
     (let ((ex (<exchanger>)))
       (! put q `(flush ,ex))
       (! exchange ex #!null)))))

(df swank-writer ((in <chan>) (q <queue>))
  (! set-name (current-thread) "swank-redirect-thread")
  (let* ((out (as <chan> (recv in)))
         (builder (<builder>))
         (flush (fun ()
                  (unless (zero? (! length builder))
                    (send out `(forward (:write-string ,(<str> builder))))
                    (! setLength builder 0))))
         (closed #f))
    (while (not closed)
      (mcase (! poll q 200 <timeunit>:MILLISECONDS)
        ('#!null (flush))
        (('write s)
         (! append builder (as <str> s))
         (when (> (! length builder) 4000)
           (flush)))
        (('flush ex)
         (flush)
         (! exchange (as <exchanger> ex) #!null))
        ('close        
         (set closed #t)
         (flush))))))

(df make-swank-outport ((out <chan>))
  (let ((w (<swank-writer>)))
    (mlet ((in . _) (spawn/chan (fun (c) (swank-writer c (@ q w)))))
      (send in out))
    (<gnu.mapping.OutPort> w  #t #t)))


;;;; Monitor

(df vm-monitor ((c <chan>))
  (! set-name (current-thread) "swank-vm-monitor")
  (let ((vm (vm-attach)))
    ;;(enable-uncaught-exception-events vm)
    (mlet* (((ev . _) (spawn/chan/catch 
                       (fun (c) 
                         (let ((q (! eventQueue vm)))
                           (while #t
                             (send c `(vm-event ,(to-list (! remove q)))))))))
            (to-string (vm-to-string vm))
            (state (tab)))
      (send c `(publish-vm ,vm))
      (while #t
        (mcase (recv* (list c ev))
          ((_ . ('get-vm cc))
           (send cc vm))
          ((,c . ('debug-info thread from to id))
           (reply c (debug-info thread from to state) id))
          ((,c . ('throw-to-toplevel thread id))
           (set state (throw-to-toplevel thread id c state)))
          ((,c . ('thread-continue thread id))
           (set state (thread-continue thread id c state)))
          ((,c . ('frame-src-loc thread frame id))
           (reply c (frame-src-loc thread frame state) id))
          ((,c . ('frame-locals thread frame id))
           (reply c (frame-locals thread frame state) id))
          ((,c . ('frame-catchers thread frame id))
           (reply c (frame-catchers thread frame state) id))
          ((,c . ('disassemble-frame thread frame id))
           (reply c (disassemble-frame thread frame state) id))
          ((,c . ('thread-frames thread from to id))
           (reply c (thread-frames thread from to state) id))
          ((,c . ('list-threads id))
           (reply c (list-threads vm state) id))
          ((,c . ('debug-thread ref))
           (set state (debug-thread ref state c)))
          ((,c . ('debug-nth-thread n))
           (let ((t (nth (get state 'all-threads #f) n)))
             ;;(log "thread ~d : ~a\n" n t)
             (set state (debug-thread t state c))))
          ((,c . ('quit-thread-browser id))
           (reply c 't id)
           (set state (del state 'all-threads)))
          ((,ev . ('vm-event es))
           ;;(log "vm-events: len=~a\n" (len es))
           (for (((e <event>) (as <list> es)))
             (set state (process-vm-event e c state))))
          ((_ . ('get-exception from tid))
           (mlet ((_ _ es) (get state tid #f))
             (send from (let ((e (car es)))
                          (typecase e 
                            (<exception-event> (! exception e))
                            (<event> e))))))
          ((_ . ('get-local rc tid frame var))
           (send rc (frame-local-var tid frame var state)))
          )))))

(df reply ((c <chan>) value id)
  (send c `(forward (:return (:ok ,value) ,id))))

(df reply-abort ((c <chan>) id)
  (send c `(forward (:return (:abort) ,id))))

(df process-vm-event ((e <event>) (c <chan>) state)
  (log "vm-event: ~s\n" e)
  (typecase e
    (<exception-event>
     (log "exception-location: ~s\n" (src-loc>str (! location e)))
     (log "exception-catch-location: ~s\n" (src-loc>str (! catch-location e)))
     (let ((l (! catch-location e)))
       (cond ((or (nul? l)
                  ;; (member (! source-path l) '("gnu/expr/ModuleExp.java")
                  )
              (process-exception e c state))
             (#t
              (let* ((t (! thread e))
                     (r (! request e))
                     (ex (! exception e)))
                (unless (eq? *last-exception* ex)
                  (set *last-exception* ex)
                  (set *last-stacktrace*  (copy-stack t)))
                (! resume t))
              state))))
    (<step-event>
     (let* ((r (! request e))
            (k (! get-property r 'continuation)))
       (! disable r)
       (log "k: ~s\n" k)
       (k e))
     state)))

(df process-exception ((e <exception-event>) (c <chan>) state)
    (let* ((tref (! thread e))
           (tid (! uniqueID tref))
           (s (get state tid #f)))
      (mcase s
        ('#f
         ;; XXX redundant in debug-thread
         (let* ((level 1)
                (state (put state tid (list tref level (list e)))))
           (send c `(forward (:debug ,tid ,level 
                                     ,@(debug-info tid 0 15 state))))
           (send c `(forward (:debug-activate ,tid ,level)))
           state))
        ((_ level exs)
         (send c `(forward (:debug-activate ,(! uniqueID tref) ,level)))
         (put state tid (list tref (1+ level) (cons e exs)))))))

(define-simple-class <faked-frame> ()
  (loc :: <location>)
  (args)
  (names)
  (values :: <java.util.Map>)
  (self)
  ((*init* (loc <location>) args names (values <java.util.Map>) self)
   (set (@ loc (this)) loc)
   (set (@ args (this)) args)
   (set (@ names (this)) names)
   (set (@ values (this)) values))
  ((toString) :: <str>
   (format "#<ff ~a>" (src-loc>str loc))))

(df copy-stack ((t <thread-ref>))
  (packing (pack)
    (iter (! frames t)
          (fun ((f <frame>))
            (let ((vars (ignore-errors (! visibleVariables f))))
              (pack (<faked-frame> 
                     (ignore-errors (! location f))
                     (ignore-errors (! getArgumentValues f))
                     (or vars #!null)
                     (or (and vars (ignore-errors (! get-values f vars)))
                         #!null)
                     (ignore-errors (! thisObject f)))))))))

(define-simple-class <listener-abort> (<java.lang.Throwable>)
  ((abort) :: void
   (primitive-throw (this))
   #!void))

(define-simple-class <break-event> (<com.sun.jdi.event.Event>)
  (thread :: <thread-ref>)
  ((*init* (thread :: <thread-ref>)) (set (@ thread (this)) thread))
  ((request) :: <com.sun.jdi.request.EventRequest> #!null)
  ((virtualMachine) :: <vm> (! virtualMachine thread)))

;;;;; Debugger

(df debug-thread ((tref <thread-ref>) state (c <chan>))
  (! suspend tref)
  (let* ((ev (<break-event> tref))
         (id (! uniqueID tref))
         (level 1)
         (state (put state id (list tref level (list ev)))))
    (send c `(forward (:debug ,id ,level ,@(debug-info id 0 10 state))))
    (send c `(forward (:debug-activate ,id ,level)))
    state))

(df debug-info ((tid <int>) (from <int>) to state)
  (mlet ((thread-ref level evs) (get state tid #f))
    (let* ((tref (as <thread-ref> thread-ref))
           (vm (! virtualMachine tref))
           (ev (as <event> (car evs)))
           (ex (typecase ev
                 (<exception-event> (! exception ev))
                 (<break-event> (<java.lang.Exception> "Interrupt"))))
           (desc (typecase ex
                   (<obj-ref> 
                    ;;(log "ex: ~a ~a\n" ex (vm-demirror vm ex))
                    (! toString (vm-demirror vm ex)))
                   (<java.lang.Exception> (! toString ex))))
           (type (format "  [type ~a]" 
                         (typecase ex
                           (<obj-ref> (! name (! referenceType ex)))
                           (<object> (!! getName getClass ex)))))
           (bt (thread-frames tid from to state)))
      `((,desc ,type nil) () ,bt ()))))

(df thread-frames ((tid <int>) (from <int>) to state)
  (mlet ((thread level evs) (get state tid #f))
    (let* ((thread (as <thread-ref> thread))
           (fcount (! frameCount thread))
           (stacktrace (event-stacktrace (car evs)))
           (missing (cond ((zero? (len stacktrace)) 0)
                          (#t (- (len stacktrace) fcount))))
           (fstart (max (- from missing) 0))
           (flen (max (- to from missing) 0))
           (frames (! frames thread fstart (min flen (- fcount fstart)))))
      (packing (pack)
        (let ((i from))
          (dotimes (_ (max (- missing from) 0))
            (pack (list i (format "~a" (stacktrace i))))
            (set i (1+ i)))
          (iter frames (fun ((f <frame>))
                         (let ((s (frame-to-string f)))
                           (pack (list i s))
                           (set i (1+ i))))))))))

(df event-stacktrace ((ev <event>))
  (typecase ev
    (<exception-event>
     (let ((r (! request ev))
           (vm (! virtualMachine ev)))
       (cond ((== (vm-demirror vm (! exception ev))
                  (ignore-errors (vm-demirror vm *last-exception*)))
              *last-stacktrace*)
             (#t
              (! getStackTrace 
                 (as <throwable> (vm-demirror vm (! exception ev))))))))
    (<event> (<java.lang.StackTraceElement[]>))))

(df frame-to-string ((f <frame>))
  (let ((loc (! location f))
        (vm (! virtualMachine f)))
    (format "~a (~a)" (!! name method loc)
            (call-with-abort 
             (fun () (format "~{~a~^ ~}"
                             (mapi (! getArgumentValues f) 
                                   (fun (arg) 
                                     (pprint-to-string
                                      (vm-demirror vm arg))))))))))

(df frame-src-loc ((tid <int>) (n <int>) state)
  (try-catch 
   (mlet* (((frame vm) (nth-frame tid n state))
           (vm (as <vm> vm)))
     (src-loc>elisp 
      (typecase frame
        (<frame> (! location frame))
        (<faked-frame> (@ loc frame))
        (<java.lang.StackTraceElement>
         (let* ((classname (! getClassName frame))
                (classes (! classesByName vm classname))
                (t (as <ref-type> (1st classes))))
           (1st (! locationsOfLine t (! getLineNumber frame))))))))
   (ex <throwable> 
       (let ((msg (! getMessage ex)))
         `(:error ,(if (== msg #!null) 
                       (! toString ex)
                       msg))))))

(df nth-frame ((tid <int>) (n <int>) state)
  (mlet ((tref level evs) (get state tid #f))
    (let* ((thread (as <thread-ref> tref))
           (fcount (! frameCount thread))
           (stacktrace (event-stacktrace (car evs)))
           (missing (cond ((zero? (len stacktrace)) 0)
                          (#t (- (len stacktrace) fcount))))
           (vm (! virtualMachine thread))
           (frame (cond ((< n missing)
                         (stacktrace n))
                        (#t (! frame thread (- n missing))))))
      (list frame vm))))

;;;;; Locals

(df frame-locals ((tid <int>) (n <int>) state)
  (mlet ((thread _ _) (get state tid #f))
    (let* ((thread (as <thread-ref> thread))
           (vm (! virtualMachine thread))
           (p (fun (x) (pprint-to-string 
                        (call-with-abort (fun () (vm-demirror vm x)))))))
      (map (fun (x)
             (mlet ((name value) x)
               (list :name name :value (p value) :id 0)))
           (%frame-locals tid n state)))))

(df frame-local-var ((tid <int>) (frame <int>) (var <int>) state => <mirror>)
  (cadr (nth (%frame-locals tid frame state) var)))

(df %frame-locals ((tid <int>) (n <int>) state)
  (mlet ((frame _) (nth-frame tid n state))
    (typecase frame
      (<frame>
       (let* ((visible (try-catch (! visibleVariables frame)
                                  (ex <com.sun.jdi.AbsentInformationException>
                                      '())))
              (map (! getValues frame visible))
              (p (fun (x) x)))
         (packing (pack)
           (let ((self (ignore-errors (! thisObject frame))))
             (when self
               (pack (list "this" (p self)))))
           (iter (! entrySet map)
                 (fun ((e <java.util.Map$Entry>))
                   (let ((var (as <local-var> (! getKey e)))
                         (val (as <value> (! getValue e))))
                     (pack (list (! name var) (p val)))))))))
      (<faked-frame> 
       (packing (pack)
         (when (@ self frame) 
           (pack (list "this" (@ self frame))))
         (iter (! entrySet (@ values frame))
               (fun ((e <java.util.Map$Entry>))
                 (let ((var (as <local-var> (! getKey e)))
                       (val (as <value> (! getValue e))))
                   (pack (list (! name var) val)))))))
      (<java.lang.StackTraceElement> '()))))

(df frame-catchers ((tid <int>) (frame <int>) state)
  '())

(df disassemble-frame ((tid <int>) (frame <int>) state)
  (mlet ((frame _) (nth-frame tid frame state))
    (typecase frame
      (<java.lang.StackTraceElement> "<??>")
      (<frame> 
       (let* ((l (! location frame))
              (m (! method l))
              (c (! declaringType l)))
          (disassemble m))))))

;;;;; Restarts

(df throw-to-toplevel ((tid <int>) (id <int>) (c <chan>) state)
  (mlet ((tref level exc) (get state tid #f))
    (let* ((t (as <thread-ref> tref))
           (ev (car exc))) 
      (typecase ev
        (<exception-event>
         (! resume t)
         (reply-abort c id)
         (do ((level level (1- level))
              (exc exc (cdr exc)))
             ((null? exc))
           (send c `(forward (:debug-return ,tid ,level nil))))
         (del state tid))
        (<break-event>
         ;; XXX race condition? 
         (let ((vm (! virtualMachine t)))
           (reply-abort c id)
           (! stop t (vm-mirror vm (<listener-abort>)))
           (! interrupt t)
           (! resume t)
           (! interrupt t)
           (do ((level level (1- level))
                (exc exc (cdr exc)))
               ((null? exc))
             (send c `(forward (:debug-return ,tid ,level nil))))
           (del state tid)))))))

(df thread-continue ((tid <int>) (id <int>) (c <chan>) state)
  (mlet ((tref level exc) (get state tid #f))
    (let* ((t (as <thread-ref> tref)))
       (! resume t))
    (reply-abort c id)
    (do ((level level (1- level))
         (exc exc (cdr exc)))
        ((null? exc))
      (send c `(forward (:debug-return ,tid ,level nil))))
    (del state tid)))

(df thread-step ((t <thread-ref>) k)
  (let* ((vm (! virtual-machine t))
         (erm (! eventRequestManager vm))
         (<sr> <com.sun.jdi.request.StepRequest>)
         (req (! createStepRequest erm t <sr>:STEP_MIN <sr>:STEP_OVER)))
    (! setSuspendPolicy req (@ SUSPEND_EVENT_THREAD req))
    (! addCountFilter req 1)
    (! put-property req 'continuation k)
    (! enable req)))

(df eval-in-thread ((t <thread-ref>) sexp 
		    #!optional (env :: <env> (<env>:current)))
  (let* ((vm (! virtualMachine t))
	 (sc :: <class-ref>
	     (1st (! classes-by-name vm "kawa.standard.Scheme")))
	 (ev :: <meth-ref>
	     (1st (! methods-by-name sc "eval" 
		     (cat "(Ljava/lang/Object;Lgnu/mapping/Environment;)"
			  "Ljava/lang/Object;")))))
    (! invokeMethod sc t ev (list sexp env) sc:INVOKE_SINGLE_THREADED)))

;;;;; Threads 

(df list-threads (vm :: <vm> state)
  (let* ((threads (! allThreads vm)))
    (put state 'all-threads threads)
    (packing (pack)
      (iter threads (fun ((t <thread-ref>))
                      (pack (list (! name t)
                                  (let ((s (thread-status t)))
                                    (if (! is-suspended t)
                                        (cat "SUSPENDED/" s)
                                        s))
                                  (! uniqueID t))))))))

(df thread-status (t :: <thread-ref>)
  (let ((s (! status t)))
    (cond ((= s t:THREAD_STATUS_UNKNOWN) "UNKNOWN")
          ((= s t:THREAD_STATUS_ZOMBIE) "ZOMBIE")
          ((= s t:THREAD_STATUS_RUNNING) "RUNNING")
          ((= s t:THREAD_STATUS_SLEEPING) "SLEEPING")
          ((= s t:THREAD_STATUS_MONITOR) "MONITOR")
          ((= s t:THREAD_STATUS_WAIT) "WAIT")
          ((= s t:THREAD_STATUS_NOT_STARTED) "NOT_STARTED")
          (#t "<bug>"))))

;;;;; Bootstrap

(df vm-attach (=> <vm>)
  (attach (getpid) 20))

(df attach (pid timeout)
  (log "attaching: ~a ~a\n" pid timeout)
  (let* ((<ac> <com.sun.jdi.connect.AttachingConnector>)
         (<arg> <com.sun.jdi.connect.Connector$Argument>)
         (vmm (com.sun.jdi.Bootstrap:virtualMachineManager))
         (pa (as <ac>
                 (or 
                  (find-if (! attaching-connectors vmm)
                           (fun (x :: <ac>) 
                             (! equals (! name x) "com.sun.jdi.ProcessAttach"))
                           #f)
                  (error "ProcessAttach connector not found"))))
         (args (! default-arguments pa)))
    (! set-value (as <arg> (! get args (to-str "pid"))) pid)
    (when timeout
      (! set-value (as <arg> (! get args (to-str "timeout"))) timeout))
    (log "attaching2: ~a ~a\n" pa args)
    (! attach pa args)))

(df getpid ()
  (let ((p (make-process (command-parse "echo -n $PPID") #!null)))
    (! waitFor p)
    (! read-line (<java.io.BufferedReader> (<in> (! get-input-stream p))))))

(df enable-uncaught-exception-events ((vm <vm>))
  (let* ((erm (! eventRequestManager vm))
         (req (! createExceptionRequest erm #!null #f #t)))
    (! setSuspendPolicy req (@ SUSPEND_EVENT_THREAD req))
    (! addThreadFilter req (vm-mirror vm (current-thread)))
    (! enable req))
  (let* ((erm (! eventRequestManager vm))
         (req (! createExceptionRequest erm #!null #t #f)))
    (! setSuspendPolicy req (@ SUSPEND_EVENT_THREAD req))
    (! addThreadFilter req (vm-mirror vm (current-thread)))
    (! addClassExclusionFilter req "java.lang.ClassLoader")
    (! addClassExclusionFilter req "java.net.URLClassLoader")
    (! addClassExclusionFilter req "java.net.URLClassLoader$1")
    (! enable req))
  #!void
  )

(df set-stacktrace-recording ((vm <vm>) (flag <boolean>))
  (for (((e <com.sun.jdi.request.ExceptionRequest>) 
         (!! exceptionRequests eventRequestManager vm)))
    (when (! notify-caught e)
      (! setEnabled e flag))))

;; (set-stacktrace-recording *the-vm* #f)

(df vm-to-string ((vm <vm>))
  (let* ((obj (as <ref-type> (1st (! classesByName vm "java.lang.Object"))))
         (met (as <meth-ref> (1st (! methodsByName obj "toString")))))
    (fun ((o <obj-ref>) (t <thread-ref>)) 
      (! value
         (as <str-ref>
             (! invokeMethod o t met '() o:INVOKE_SINGLE_THREADED))))))

(define-simple-class <swank-global-variable> ()
  (var :allocation 'static))

(define-variable *global-get-mirror* #!null)
(define-variable *global-set-mirror* #!null)
(define-variable *global-get-raw* #!null)
(define-variable *global-set-raw* #!null)

(df init-global-field ((vm <vm>))
  (when (nul? *global-get-mirror*)
    (set <swank-global-variable>:var #!null) ; prepare class
    (let* ((c (as <com.sun.jdi.ClassType>
                  (1st (! classes-by-name vm "swank$Mnglobal$Mnvariable"))))
           (f (! fieldByName c "var")))
      (set *global-get-mirror* (fun () (! getValue c f)))
      (set *global-set-mirror* (fun ((v <obj-ref>)) (! setValue c f v))))
    (set *global-get-raw* (fun () <swank-global-variable>:var))
    (set *global-set-raw* (fun (x) (set <swank-global-variable>:var x)))))

(df vm-mirror ((vm <vm>) obj)
  (synchronized vm
    (init-global-field vm)
    (*global-set-raw* obj)
    (*global-get-mirror*)))

(df vm-demirror ((vm <vm>) (v <value>))
  (synchronized vm
    (if (== v #!null) 
      #!null
      (typecase v
        (<obj-ref> (init-global-field vm)
                   (*global-set-mirror* v)
                   (*global-get-raw*))
        (<com.sun.jdi.IntegerValue> (! value v))
        (<com.sun.jdi.LongValue> (! value v))
        (<com.sun.jdi.CharValue> (! value v))
        (<com.sun.jdi.ByteValue> (! value v))
        (<com.sun.jdi.BooleanValue> (! value v))
        (<com.sun.jdi.ShortValue> (! value v))
        (<com.sun.jdi.FloatValue> (! value v))
        (<com.sun.jdi.DoubleValue> (! value v))))))

(df vm-set-slot ((vm <vm>) (o <object>) (name <str>) value)
  (let* ((o (as <obj-ref> (vm-mirror vm o)))
         (t (! reference-type o))
	 (f (! field-by-name t name)))
    (! set-value o f (vm-mirror vm value))))

(define-simple-class <ucex-handler>
    (<java.lang.Thread$UncaughtExceptionHandler>)
  (f :: <gnu.mapping.Procedure>)
  ((*init* (f :: <gnu.mapping.Procedure>)) (set (@ f (this)) f))
  ((uncaughtException (t <thread>) (e <throwable>))
   :: <void>
   ;;(! println (java.lang.System:.err) (to-str "uhexc:::"))
   (! apply2 f t e)
    #!void))

;;;; Channels

(df spawn (f)
  (let ((thread (<thread> (%%runnable f))))
    (! start thread)
    thread))

(df %%runnable (f => <java.lang.Runnable>) 
  (<runnable> f)
  ;;(<gnu.mapping.RunnableClosure> f)
  )

(df %runnable (f => <java.lang.Runnable>)
  (<runnable>
   (fun ()
     (try-catch (f)
                (ex <throwable> 
                    (log "exception in thread ~s: ~s" (current-thread)
                          ex)
                    (! printStackTrace ex))))))

(df chan () 
  (let ((lock (<object>))
        (im (<chan>))
        (ex (<chan>)))
    (set (@ lock im) lock)
    (set (@ lock ex) lock)
    (set (@ peer im) ex)
    (set (@ peer ex) im)
    (cons im ex)))

(df immutable? (obj)
  (or (== obj #!null)
      (symbol? obj)
      (number? obj)
      (char? obj)
      (instance? obj <str>)
      (null? obj)))

(df send ((c <chan>) value => <void>)
  (df pass (obj)
    (cond ((immutable? obj) obj)
          ((string? obj) (! to-string obj))
          ((pair? obj)
           (let loop ((r (list (pass (car obj))))
                      (o (cdr obj)))
             (cond ((null? o) (reverse! r))
                   ((pair? o) (loop (cons (pass (car o)) r) (cdr o)))
                   (#t (append (reverse! r) (pass o))))))
          ((instance? obj <chan>)
           (let ((o :: <chan> obj))
             (assert (== (@ owner o) (current-thread)))
             (synchronized (@ lock c)
               (set (@ owner o) (@ owner (@ peer c))))
             o))
          ((or (instance? obj <env>)
               (instance? obj <mirror>))
           ;; those can be shared, for pragmatic reasons
           obj
           )
          (#t (error "can't send" obj (class-name-sans-package obj)))))
  ;;(log "send: ~s ~s -> ~s\n" value (@ owner c) (@ owner (@ peer c)))
  (assert (== (@ owner c) (current-thread)))
  ;;(log "lock: ~s send\n" (@ owner (@ peer c)))
  (synchronized (@ owner (@ peer c))
    (! put (@ queue (@ peer c)) (pass value))
    (! notify (@ owner (@ peer c))))
  ;;(log "unlock: ~s send\n" (@ owner (@ peer c)))
  )

(df recv ((c <chan>))
  (cdr (recv/timeout (list c) 0)))

(df recv* ((cs <iterable>))
  (recv/timeout cs 0))

(df recv/timeout ((cs <iterable>) (timeout <long>))
  (let ((self (current-thread))
        (end (if (zero? timeout) 
                 0 
                 (+ (current-time) timeout))))
    ;;(log "lock: ~s recv\n" self)
    (synchronized self
      (let loop ()
        ;;(log "receive-loop: ~s\n" self)
        (let ((ready (find-if cs 
                              (fun ((c <chan>))
                                (not (! is-empty (@ queue c))))
                              #f)))
          (cond (ready
                 ;;(log "unlock: ~s recv\n" self)
                 (cons ready (! take (@ queue (as <chan> ready)))))
                ((zero? timeout)
                 ;;(log "wait: ~s recv\n" self)
                 (! wait self) (loop))
                (#t 
                 (let ((now (current-time)))
                   (cond ((<= end now)
                          'timeout)
                         (#t
                          ;;(log "wait: ~s recv\n" self)
                          (! wait self (- end now))
                          (loop)))))))))))

(df rpc ((c <chan>) msg)
  (mlet* (((im . ex) (chan))
          ((op . args) msg))
    (send c `(,op ,ex . ,args))
    (recv im)))

(df spawn/chan (f)
  (mlet ((im . ex) (chan))
    (let ((thread (<thread> (%%runnable (fun () (f ex))))))
      (set (@ owner ex) thread)
      (! start thread)
      (cons im thread))))

(df spawn/chan/catch (f)
  (spawn/chan 
   (fun (c)
     (try-catch 
      (f c)
      (ex <throwable>
          (send c `(error ,(! toString ex)
                          ,(class-name-sans-package ex)
                          ,(map (fun (e) (! to-string e))
                                (array-to-list (! get-stack-trace ex))))))))))

(define-simple-class <runnable> (<gnu.mapping.RunnableClosure>)
  (f :: <gnu.mapping.Procedure>)
  ((*init* (f <gnu.mapping.Procedure>))
   (invoke-special <gnu.mapping.RunnableClosure> (this) '*init* f)
   (set (@ f (this)) f))
  ((run) :: void
   (! set-environment-raw (<gnu.mapping.CallContext>:getInstance)
      (@ environment (this)))
   (! apply0 f)))

;;;; Logging

(define swank-log-port (current-error-port))
(df log (fstr #!rest args)
  (synchronized swank-log-port
    (apply format swank-log-port fstr args)
    (force-output swank-log-port))
  #!void)

;;;; Random helpers

(df 1+ (x) (+ x 1))
(df 1- (x) (- x 1))

(df len (x => <int>)
  (typecase x
    (<list> (length x))
    (<str> (! length x))
    (<string> (string-length x))
    (<vector> (vector-length x))
    (<java.util.List> (! size x))
    (<object[]> (@ length x))))

(df put (tab key value) (hash-table-set! tab key value) tab)
(df get (tab key default) (hash-table-ref/default tab key default))
(df del (tab key) (hash-table-delete! tab key) tab)
(df tab () (make-hash-table))

(df equal (x y => <boolean>) (equal? x y))

(df current-thread (=> <thread>) (java.lang.Thread:currentThread))
(df current-time (=> <long>) (java.lang.System:currentTimeMillis))

(df nul? (x) (== x #!null))

(df read-from-string (str)
  (call-with-input-string str read))

;;(df print-to-string (obj) (call-with-output-string (fun (p) (write obj p))))
   
(df pprint-to-string (obj)
  (let* ((w (<java.io.StringWriter>))
         (p (<gnu.mapping.OutPort> w #t #f)))
    (try-catch (write obj p)
               (ex <throwable> 
                   (format p "#<error while printing ~a ~a>" 
                           ex (class-name-sans-package ex))))
    (! flush p)
    (to-string (! getBuffer w))))

(define cat string-append)

(df values-to-list (values)
  (typecase values
    (<gnu.mapping.Values> (array-to-list (! getValues values)))
    (<object> (list values))))

;; (to-list (as-list (values 1 2 2)))

(df array-to-list ((array <object[]>) => <list>)
  (packing (pack)
    (dotimes (i (@ length array))
      (pack (array i)))))

(df lisp-bool (obj)
  (cond ((== obj 'nil) #f)
        ((== obj 't) #t)
        (#t (error "Can't map lisp boolean" obj))))

(df path-sans-extension ((p path) => <string>)
  (let ((ex (! get-extension p))
        (str (! to-string p)))
    (to-string (cond ((not ex) str)
                     (#t (! substring str 0 (- (len str) (len ex) 1)))))))

(df class-name-sans-package ((obj <object>))
  (cond ((nul? obj) "<#!null>")
        (#t
         (let* ((c (! get-class obj)) (n (! get-simple-name c)))
           (cond ((equal n "") (! get-name c))
                 (#t n))))))

(df list-env (#!optional (env :: <env> (<env>:current)))
  (let ((enum (! enumerateAllLocations env)))
    (packing (pack)
      (while (! hasMoreElements enum)
        (pack (! nextLocation enum))))))

(df list-file (filename)
  (with (port (call-with-input-file filename))
    (let* ((lang (gnu.expr.Language:getDefaultLanguage))
           (messages (<gnu.text.SourceMessages>))
           (comp (! parse lang port messages 0)))
      (! get-module comp))))

(df list-decls (file)
  (let* ((module (as <gnu.expr.ModuleExp> (list-file file))))
    (do ((decl :: <gnu.expr.Declaration>
               (! firstDecl module) (! nextDecl decl)))
        ((nul? decl))
      (format #t "~a ~a:~d:~d\n" decl
              (! getFileName decl)
              (! getLineNumber decl)
              (! getColumnNumber decl)
              ))))

(df %time (f)
  (define-alias <mf> <java.lang.management.ManagementFactory>)
  (define-alias <gc> <java.lang.management.GarbageCollectorMXBean>)
  (let* ((gcs (<mf>:getGarbageCollectorMXBeans))
         (mem (<mf>:getMemoryMXBean))
         (jit (<mf>:getCompilationMXBean))
         (oldjit (! getTotalCompilationTime jit))
         (oldgc (packing (pack)
                  (iter gcs (fun ((gc <gc>))
                              (pack (cons gc 
                                          (list (! getCollectionCount gc)
                                                (! getCollectionTime gc))))))))
         (heap (!! getUsed getHeapMemoryUsage mem))
         (nonheap (!! getUsed getNonHeapMemoryUsage mem))
         (start (java.lang.System:nanoTime))
         (values (f))
         (end (java.lang.System:nanoTime))
         (newheap (!! getUsed getHeapMemoryUsage mem))
         (newnonheap (!! getUsed getNonHeapMemoryUsage mem)))
    (format #t "~&")
    (let ((njit (! getTotalCompilationTime jit)))
      (format #t "; JIT compilation: ~:d ms (~:d)\n" (- njit oldjit) njit))
    (iter gcs (fun ((gc <gc>))
                (mlet ((_ count time) (assoc gc oldgc))
                  (format #t "; GC ~a: ~:d ms (~d)\n"
                          (! getName gc)
                          (- (! getCollectionTime gc) time)
                          (- (! getCollectionCount gc) count)))))
    (format #t "; Heap: ~@:d (~:d)\n" (- newheap heap) newheap)
    (format #t "; Non-Heap: ~@:d (~:d)\n" (- newnonheap nonheap) newnonheap)
    (format #t "; Elapsed time: ~:d us\n" (/ (- end start) 1000))
    values))

(define-syntax time
  (syntax-rules ()
    ((time form)
     (%time (lambda () form)))))

(df gc ()
  (let* ((mem (java.lang.management.ManagementFactory:getMemoryMXBean))
         (oheap (!! getUsed getHeapMemoryUsage mem))
         (onheap (!! getUsed getNonHeapMemoryUsage mem))
         (_ (! gc mem))
         (heap (!! getUsed  getHeapMemoryUsage mem))
         (nheap (!! getUsed getNonHeapMemoryUsage mem)))
    (format #t "; heap: ~@:d (~:d) non-heap: ~@:d (~:d)\n"
             (- heap oheap) heap (- onheap nheap) nheap)))

(df room ()
  (let* ((pools (java.lang.management.ManagementFactory:getMemoryPoolMXBeans))
         (mem (java.lang.management.ManagementFactory:getMemoryMXBean))
         (heap (!! getUsed  getHeapMemoryUsage mem))
         (nheap (!! getUsed getNonHeapMemoryUsage mem)))
    (iter pools (fun ((p <java.lang.management.MemoryPoolMXBean>))
                  (format #t "~&; ~a~1,16t: ~10:d\n"
                          (! getName p)
                          (!! getUsed getUsage p))))
    (format #t "; Heap~1,16t: ~10:d\n" heap)
    (format #t "; Non-Heap~1,16t: ~10:d\n" nheap)))

(df javap (class #!key method signature)
  (let* ((<is> <java.io.ByteArrayInputStream>)
         (bytes
          (typecase class
            (<string> (read-bytes (<java.io.FileInputStream> (to-str class))))
            (<byte[]> class)
            (<symbol> (read-class-file class))))
         (cdata (<sun.tools.javap.ClassData> (<is> bytes)))
         (p (<sun.tools.javap.JavapPrinter> 
	     (<is> bytes)
             (current-output-port)
             (<sun.tools.javap.JavapEnvironment>))))
    (cond (method
           (dolist ((m <sun.tools.javap.MethodData>)
                    (array-to-list (! getMethods cdata)))
             (when (and (equal (to-str method) (! getName m))
                        (or (not signature) 
                            (equal signature (! getInternalSig m))))
               (! printMethodSignature p m (! getAccess m))
               (! printExceptions p m)
               (newline)
               (! printVerboseHeader p m)
               (! printcodeSequence p m))))
          (#t (p:print)))
    (values)))

(df read-bytes ((is <java.io.InputStream>) => <byte[]>)
  (let ((os (<java.io.ByteArrayOutputStream>)))
    (let loop ()
      (let ((c (! read is)))
        (cond ((= c -1))
              (#t (! write os c) (loop)))))
    (! to-byte-array os)))

(df read-class-file ((name <symbol>) => <byte[]>)
  (let ((f (cat (! replace (to-str name) (as <char> #\.) (as <char> #\/)) 
                ".class")))
    (mcase (find-file-in-path f (class-path))
      ('#f (ferror "Can't find classfile for ~s" name))
      ((:zip zipfile entry)
       (let* ((z (<java.util.zip.ZipFile> (as <str> zipfile)))
              (e (z:getEntry (as <str> entry))))
         (read-bytes (z:getInputStream e))))
      ((:file s) (read-bytes (<java.io.FileInputStream> (as <str> s)))))))

(df all-instances ((vm <vm>) (classname <str>))
  (mappend (fun ((c <class-ref>)) (to-list (! instances c 9999)))
	   (%all-subclasses vm classname)))

(df %all-subclasses ((vm <vm>) (classname <str>))
  (mappend (fun ((c <class-ref>)) (cons c (to-list (! subclasses c))))
           (to-list (! classes-by-name vm classname))))

(df with-output-to-string (thunk => <str>)
  (call-with-output-string 
   (fun (s) (parameterize ((current-output-port s)) (thunk)))))

(df find-if ((i <iterable>) test default)
  (let ((iter (! iterator i))
        (found #f))
    (while (and (not found) (! has-next iter))
      (let ((e (! next iter)))
        (when (test e)
          (set found #t)
          (set default e))))
    default))

(df filter ((i <iterable>) test => <list>)
  (packing (pack)
    (for ((e i))
      (when (test e)
        (pack e)))))

(df iter ((i <iterable>) f)
  (for ((e i)) (f e)))

(df mapi ((i <iterable>) f => <list>)
  (packing (pack) (for ((e i)) (pack (f e)))))

(df nth ((i <iterable>) (n <int>))
  (let ((iter (! iterator i)))
    (dotimes (i n)
      (! next iter))
    (! next iter)))

(df 1st ((i <iterable>)) (!! next iterator i))

(df to-list ((i <iterable>) => <list>)
  (packing (pack) (for ((e i)) (pack e))))

(df as-list ((o <java.lang.Object[]>) => <java.util.List>)
  (java.util.Arrays:asList o))

(df mappend (f list)
  (apply append (map f list)))

(df subseq (s from to)
  (typecase s
    (<list> (apply list (! sub-list s from to)))
    (<vector> (apply vector (! sub-list s from to)))
    (<str> (! substring s from to))
    (<byte[]> (let* ((len (as <int> (- to from)))
                     (t (<byte[]> :length len)))
                (java.lang.System:arraycopy s from t 0 len)
                t))))

(df to-string (obj => <string>)
  (cond ((instance? obj <str>) (<gnu.lists.FString> (as <str> obj)))
        ((string? obj) obj)
        ((symbol? obj) (symbol->string obj))
        ((instance? obj <java.lang.StringBuffer>)
         (<gnu.lists.FString> (as <java.lang.StringBuffer> obj)))
        ((instance? obj <java.lang.StringBuilder>)
         (<gnu.lists.FString> (as <java.lang.StringBuilder> obj)))
        (#t (error "Not a string designator" obj 
                   (class-name-sans-package obj)))))

(df to-str (obj => <str>)
  (cond ((instance? obj <str>) obj)
        ((string? obj) (! toString obj))
        ((symbol? obj) (! getName (as <gnu.mapping.Symbol> obj)))
        (#t (error "Not a string designator" obj
                   (class-name-sans-package obj)))))

;; Local Variables:
;; mode: goo 
;; compile-command:"kawa -e '(compile-file \"swank-kawa.scm\"\"swank-kawa\")'" 
;; End: