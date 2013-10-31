;;; swank-jolt.k --- Swank server for Jolt                         -*- goo -*-
;;
;; Copyright (C) 2008  Helmut Eller
;;
;; This file is licensed under the terms of the GNU General Public
;; License as distributed with Emacs (press C-h C-c for details).

;;; Commentary:
;;
;; Jolt/Coke is a Lisp-like language wich operates at the semantic level of
;; C, i.e. most objects are machine words and memory pointers.  The
;; standard boot files define an interface to Id Smalltalk.  So we can
;; also pretend to do OOP, but we must be careful to pass properly
;; tagged pointers to Smalltalk.
;;
;; This file only implements a minimum of SLIME's functionality.  We
;; install a handler with atexit(3) to invoke the debugger.  This way
;; we can stop Jolt from terminating the process on every error.
;; Unfortunately, the backtrace doesn't contain much information and
;; we also have no error message (other than the exit code).  Jolt
;; usually prints some message to stdout before calling exit, so you
;; have to look in the *inferior-lisp* buffer for hints.  We do
;; nothing (yet) to recover from SIGSEGV.

;;; Installation
;;
;; 1. Download and build cola.  See <http://piumarta.com/software/cola/>.  
;;    I used the svn version:
;;       svn co http://piumarta.com/svn2/idst/trunk idst
;; 2. Add something like this to your .emacs:
;;    
;;  (add-to-list 'slime-lisp-implementations
;;               '(jolt (".../idst/function/jolt-burg/main" 
;;                       "boot.k" ".../swank-jolt.k" "-") ; note the "-"
;;                      :init jolt-slime-init
;;                      :init-function slime-redirect-inferior-output)
;;  (defun jolt-slime-init (file _) (format "%S\n" `(start-swank ,file)))
;;  (defun jolt () (interactive) (slime 'jolt))
;;
;; 3. Use `M-x jolt' to start it.
;;

;;; Code

;; In this file I use 2-3 letters for often used names, like DF or
;; VEC, even if those names are abbreviations.  I think that after a
;; little getting used to, this style is just as readable as the more
;; traditional DEFUN and VECTOR.  Shorter names make it easier to
;; write terse code, in particular 1-line definitions.

;; `df' is like `defun' in a traditional lisp
(syntax df 
  (lambda (form compiler)
    (printf "df %s ...\n" [[[form second] asString] _stringValue])
    `(define ,[form second] (lambda ,@[form copyFrom: '2]))))

;; (! args ...) is the same as [args ...] but easier to edit.
(syntax !
  (lambda (form compiler)
    (cond ((== [form size] '3)
           (if [[form third] isSymbol]
               `(send ',[form third] ,[form second])
               [compiler errorSyntax: [form third]]))
          ((and [[form size] > '3]
                (== [[form size] \\ '2] '0))
           (let ((args [OrderedCollection new])
                 (keys [OrderedCollection new])
                 (i '2) (len [form size]))
             (while (< i len)
               (let ((key [form at: i]))
                 (if (or [key isKeyword]
                         (and (== i '2) [key isSymbol])) ; for [X + Y]
                     [keys addLast: [key asString]]
                     [compiler errorSyntax: key]))
               [args addLast: [form at: [i + '1]]]
               (set i [i + '2]))
             `(send ',[[keys concatenated] asSymbol] ,[form second] ,@args)))
          (1 [compiler errorArgumentCount: form]))))

(define Integer (import "Integer"))
(define Symbol (import "Symbol")) ;; aka. _selector
(define StaticBlockClosure (import "StaticBlockClosure"))
(define BlockClosure (import "BlockClosure"))
(define SequenceableCollection (import "SequenceableCollection"))
(define _vtable (import "_vtable"))
(define ByteArray (import "ByteArray"))
(define CodeGenerator (import "CodeGenerator"))
(define TheGlobalEnvironment (import "TheGlobalEnvironment"))

(df error (msg) (! Object error: msg))
(df print-to-string (obj)
  (let ((len '200)
        (stream (! WriteStream on: (! String new: len))))
    (! stream print: obj)
    (! stream contents)))
(df assertion-failed (exp)
  (error (! '"Assertion failed: " , (print-to-string exp))))

(syntax assert 
  (lambda (form) 
    `(if (not ,(! form second))
         (assertion-failed ',(! form second)))))

(df isa? (obj type) (! obj isKindOf: type))
(df equal (o1 o2) (! o1 = o2))

(define nil 0)
(define false 0)
(define true (! Object notNil))
(df bool? (obj) (or (== obj false) (== obj true)))
(df int? (obj) (isa? obj Integer))

;; In this file the convention X>Y is used for operations that convert
;; X-to-Y.  And _ means "machine word".  So _>int is the operator that
;; converts a machine word to an Integer.

(df _>int (word) (! Integer value_: word))
(df int>_ (i) (! i _integerValue))

;; Fixnum operators.  Manual tagging/untagging would probably be more
;; efficent than invoking methods.

(df fix? (obj) (& obj 1))
(df _>fix (n) (! SmallInteger value_: n))
(df fix>_ (i) (! i _integerValue))
(df fx+ (fx1 fx2) (! fx1 + fx2))
(df fx* (fx1 fx2) (! fx1 * fx2))
(df fx1+ (fx) (! fx + '1))
(df fx1- (fx) (! fx - '1))

(df str? (obj) (isa? obj String))
(df >str (o) (! o asString))
(df str>_ (s) (! s _stringValue))
(df _>str (s) (! String value_: s))
(df sym? (obj) (isa? obj Symbol))
(df seq? (obj) (isa? obj SequenceableCollection))
(df array? (obj) (isa? obj Array))
(df len (obj) (! obj size))
(df len_ (obj) (! (! obj size) _integerValue))
(df ref (obj idx) (! obj at: idx))
(df set-ref (obj idx elt) (! obj at: idx put: elt))
(df first (obj) (! obj first))
(df second (obj) (! obj second))

(df puts (string stream) (! stream nextPutAll: string))

(define _GC_base (dlsym "GC_base"))

;; Is ADDR a pointer to a heap allocated object?  The Boehm GC nows
;; such things.  This is useful for debugging, because we can quite
;; safely (i.e. without provoking SIGSEGV) access such addresses.
(df valid-pointer? (addr) 
  (let ((ptr (& addr (~ 1))))
    (and (_GC_base ptr)
         (_GC_base (long@ ptr -1)))))

;; Print OBJ as a Lisp printer would do.
(df prin1 (obj stream)
  (cond ((fix? obj) (! stream print: obj))
        ((== obj nil) (puts '"nil" stream))
        ((== obj false) (puts '"#f" stream))
        ((== obj true) (puts '"#t" stream))
        ((not (valid-pointer? obj)) 
         (begin (puts '"#<w " stream)
                (prin1 (_>int obj) stream)
                (puts '">" stream)))
        ((int? obj) (! stream print: obj))
        ((sym? obj) (puts (>str obj) stream))
        ((isa? obj StaticBlockClosure)
         (begin (puts '"#<fun /" stream)
                (! stream print: (! obj arity))
                (puts '"#>" stream)))
        ((and (str? obj) (len obj))
         (! obj printEscapedOn: stream delimited: (ref '"\"" '0)))
        ((and (array? obj) (len obj))
         (begin (puts '"(" stream)
                (let ((max (- (len_ obj) 1)))
                  (for (i 0 1 max)
                    (prin1 (ref obj (_>fix i)) stream)
                    (if (!= i max)
                        (puts '" " stream))))
                (puts '")" stream)))
        ((and (isa? obj OrderedCollection) (len obj))
         (begin (puts '"#[" stream)
                (let ((max (- (len_ obj) 1)))
                  (for (i 0 1 max)
                    (prin1 (ref obj (_>fix i)) stream)
                    (if (!= i max)
                        (puts '" " stream))))
                (puts '"]" stream)))
        (true 
         (begin (puts '"#<" stream)
                (puts (! obj debugName) stream)
                (puts '">" stream))))
  obj)

(df print (obj)
  (prin1 obj StdOut)
  (puts '"\n" StdOut))

(df prin1-to-string (obj)
  (let ((len '100)
        (stream (! WriteStream on: (! String new: len))))
    (prin1 obj stream)
    (! stream contents)))

;;(df %vable-tally (_vtable) (long@ _vtable))
(df cr () (printf "\n"))
(df print-object-selectors (obj)
  (let ((vtable (! obj _vtable))
        (tally (long@ vtable 0))
        (bindings (long@ vtable 1)))
    (for (i 1 1 tally)
      (print (long@ (long@ bindings i)))
      (cr))))

(df print-object-slots (obj)
  (let ((size (! obj _sizeof))
        (end (+ obj size)))
    (while (< obj end)
      (print (long@ obj))
      (cr)
      (incr obj 4))))

(df intern (string) (! Symbol intern: string))

;; Jolt doesn't seem to have an equivalent for gensym, but it's damn
;; hard to write macros without it.  So here we adopt the conventions
;; that symbols which look like ".[0-9]+" are reserved for gensym and
;; shouldn't be used for "user visible variables".
(define gensym-counter 0)
(df gensym ()
  (set gensym-counter (+ gensym-counter 1))
  (intern (! '"." , (>str (_>fix gensym-counter)))))

;; Surprisingly, SequenceableCollection doesn't have a indexOf method.
;; So we even need to implement such mundane things.
(df index-of (seq elt)
  (let ((max (len seq))
        (i '0))
    (while (! i < max)
      (if (equal (ref seq i) elt)
          (return i)
          (set i (! i + '1))))
    nil))

(df find-dot (array) (index-of array '.))

;; What followes is the implementation of the pattern matching macro MIF.
;; The syntax is (mif (PATTERN EXP) THEN ELSE).
;; The THEN-branch is executed if PATTERN matches the value produced by EXP.
;; ELSE gets only executed if the match failes.
;; A pattern can be
;;  1) a symbol, which matches all values, but also binds the variable to the
;;     value
;;  2) (quote LITERAL), matches if the value is `equal' to LITERAL.
;;  3) (PS ...) matches sequences, if the elements match PS.
;;  4) (P1 ... Pn . Ptail) matches if P1 ... Pn match the respective elements
;;                         at indices 1..n and if Ptail matches the rest
;;                         of the sequence
;; Examples:
;;   (mif (x 10) x 'else) => 10
;;   (mif ('a 'a) 'then 'else) => then
;;   (mif ('a 'b) 'then 'else) => else
;;   (mif ((a b) '(1 2)) b 'else) => 2
;;   (mif ((a . b) '(1 2)) b 'else) => '(2)
;;   (mif ((. x) '(1 2)) x 'else) => '(1 2)

(define mif% 0) ;; defer
(df mif%array (compiler pattern i value then fail)
  ;;(print `(mif%array ,pattern ,i ,value))
  (cond ((== i (len_ pattern)) then)
        ((== (ref pattern (_>fix i)) '.)
         (begin
          (if (!= (- (len_ pattern) 2) i)
              (begin 
               (print pattern)
               (! compiler error: (! '"dot in strange position: "
                                     , (>str (_>fix i))))))
          (mif% compiler 
                (ref pattern (_>fix (+ i 1)))
                `(! ,value copyFrom: ',(_>fix i))
                then fail)))
        (true 
         (mif% compiler
               (ref pattern (_>fix i))
               `(ref ,value ',(_>fix i))
               (mif%array compiler pattern (+ i 1) value then fail)
               fail))))

(df mif% (compiler pattern value then fail)
  ;;(print `(mif% ,pattern ,value ,then))
  (cond ((== pattern '_) then)
        ((== pattern '.) (! compiler errorSyntax: pattern))
        ((sym? pattern) 
         `(let ((,pattern ,value)) ,then))
        ((seq? pattern)
         (cond ((== (len_ pattern) 0)
                `(if (== (len_ ,value) 0) ,then (goto ,fail)))
               ((== (first pattern) 'quote)
                (begin
                 (if (not (== (len_ pattern) 2))
                     (! compiler errorSyntax: pattern))
                 `(if (equal ,value ,pattern) ,then (goto ,fail))))
               (true 
                (let ((tmp (gensym)) (tmp2 (gensym))
                      (pos (find-dot pattern)))
                  `(let ((,tmp2 ,value)
                         (,tmp ,tmp2))
                     (if (and (seq? ,tmp)
                              ,(if (find-dot pattern)
                                   `(>= (len ,tmp) 
                                        ',(_>fix (- (len_ pattern) 2)))
                                   `(== (len ,tmp) ',(len pattern))))
                         ,(mif%array compiler pattern 0 tmp then fail)
                         (goto ,fail)))))))
        (true (! compiler errorSyntax: pattern))))

(syntax mif
  (lambda (node compiler)
    ;;(print `(mif ,node))
    (if (not (or (== (len_ node) 4)
                 (== (len_ node) 3)))
        (! compiler errorArgumentCount: node))
    (if (not (and (array? (ref node '1))
                  (== (len_ (ref node '1)) 2)))
        (! compiler errorSyntax: (ref node '1)))
    (let ((pattern (first (ref node '1)))
          (value (second (ref node '1)))
          (then (ref node '2))
          (else (if (== (len_ node) 4)
                    (ref node '3)
                    `(error "mif failed")))
          (destination (gensym))
          (fail (! compiler newLabel))
          (success (! compiler newLabel)))
      `(let ((,destination 0))
         ,(mif% compiler pattern value 
                `(begin (set ,destination ,then)
                        (goto ,success))
                fail)
         (label ,fail)
         (set ,destination ,else)
         (label ,success)
         ,destination))))

;; (define *catch-stack* nil)
;; 
(df bar (o) (mif ('a o) 'yes 'no))
(assert (== (bar 'a) 'yes))
(assert (== (bar 'b) 'no))
(df foo (o) (mif (('a) o) 'yes 'no))
(assert (== (foo '(a)) 'yes))
(assert (== (foo '(b)) 'no))
(df baz (o) (mif (('a 'b) o) 'yes 'no))
(assert (== (baz '(a b)) 'yes))
(assert (== (baz '(a c)) 'no))
(assert (== (baz '(b c)) 'no))
(assert (== (baz 'a) 'no))
(df mifvar (o) (mif (y o) y 'no))
(assert (== (mifvar 'foo) 'foo))
(df mifvec (o) (mif ((y) o) y 'no))
(assert (== (mifvec '(a)) 'a))
(assert (== (mifvec 'x) 'no))
(df mifvec2 (o) (mif (('a y) o) y 'no))
(assert (== (mifvec2 '(a b)) 'b))
(assert (== (mifvec2 '(b c)) 'no))
(assert (== (mif ((x) '(a)) x 'no) 'a))
(assert (== (mif ((x . y) '(a b)) x 'no) 'a))
(assert (== (mif ((x y . z) '(a b)) y 'no) 'b))
(assert (equal (mif ((x . y) '(a b)) y 'no) '(b)))
(assert (equal (mif ((. x) '(a b)) x 'no) '(a b)))
(assert (equal (mif (((. x)) '((a b))) x 'no) '(a b)))
(assert (equal (mif (((. x) . y) '((a b) c)) y 'no) '(c)))
(assert (== (mif (() '()) 'yes 'no) 'yes))
(assert (== (mif (() '(a)) 'yes 'no) 'no))

;; Now that we have a somewhat convenient pattern matcher we can write
;; a more convenient macro defining macro:
(syntax defmacro
  (lambda (node compiler)
    (mif (('defmacro name (. args) . body) node)
         (begin 
          (printf "defmacro %s ...\n" (str>_ (>str name)))
          `(syntax ,name
             (lambda (node compiler)
               (mif ((',name ,@args) node)
                    (begin ,@body)
                    (! compiler errorSyntax: node)))))
         (! compiler errorSyntax: node))))

;; and an even more convenient pattern matcher:
(defmacro mcase (value . clauses)
  (let ((tmp (gensym)))
    `(let ((,tmp ,value))
       ,(mif (() clauses) 
             `(begin (print ,tmp) 
                     (error "mcase failed"))
             (mif (((pattern . body) . more) clauses)
                  `(mif (,pattern ,tmp) 
                        (begin ,@(mif (() body) '(0) body))
                        (mcase ,tmp ,@more))
                  (! compiler errorSyntax: clauses))))))

;; and some traditional macros
(defmacro when (test . body) `(if ,test (begin ,@body)))
(defmacro unless (test . body) `(if ,test 0 (begin ,@body)))
(defmacro or (. args)  ; the built in OR returns 1 on success.
  (mcase args
    (() 0)
    ((e) e)
    ((e1 . more)
     (let ((tmp (gensym)))
       `(let ((,tmp ,e1))
          (if ,tmp ,tmp (or ,@more)))))))

(defmacro dotimes_ ((var n) . body)
  (let ((tmp (gensym)))
    `(let ((,tmp ,n)
	   (,var 0))
       (while (< ,var ,tmp)
	 ,@body
	 (set ,var (+ ,var 1))))))

(defmacro dotimes ((var n) . body)
  (let ((tmp (gensym)))
    `(let ((,tmp ,n)
	   (,var '0))
       (while (< ,var ,tmp)
	 ,@body
	 (set ,var (fx1+ ,var))))))

;; DOVEC is like the traditional DOLIST but works on "vectors"
;; i.e. sequences which can be indexed efficently.
(defmacro dovec ((var seq) . body)
  (let ((i (gensym))
	(max (gensym))
	(tmp (gensym)))
    `(let ((,i 0)
	   (,tmp ,seq)
	   (,max (len_ ,tmp)))
       (while (< ,i ,max)
	 (let ((,var (! ,tmp at: (_>fix ,i))))
	   ,@body
	   (set ,i (+ ,i 1)))))))

;; "Packing" is what Lispers usually call "collecting".
;; The Lisp idiom  (let ((result '())) .. (push x result) .. (nreverse result))
;; translates to   (packing (result) .. (pack x result))
(defmacro packing ((var) . body)
  `(let ((,var (! OrderedCollection new)))
     ,@body
     (! ,var asArray)))

(df pack (elt packer) (! packer addLast: elt))

(assert (equal (packing (p) (dotimes_ (i 2) (pack (_>fix i) p)))
               '(0 1)))

(assert (equal (packing (p) (dovec (e '(2 3)) (pack e p)))
               '(2 3)))

(assert (equal (packing (p)
                 (let ((a '(2 3)))
                   (dotimes (i (len a))
                     (pack (ref a i) p))))
               '(2 3)))

;; MAPCAR (more or less)
(df map (fun col)
  (packing (r) 
    (dovec (e col) 
      (pack (fun e) r))))

;; VEC allocates and initializes a new array.
;; The macro translates (vec x y z) to `(,x ,y ,z).
(defmacro vec (. args)
  `(quasiquote
    (,@(map (lambda (arg) `(,'unquote ,arg))
            args))))

(assert (equal (vec '0 '1) '(0 1)))
(assert (equal (vec) '()))
(assert (== (len (vec 0 1 2 3 4)) '5))

;; Concatenate.
(defmacro cat (. args) `(! (vec '"" ,@args) concatenated))

(assert (equal (cat '"a" '"b" '"c") '"abc"))

;; Take a vector of bytes and copy the bytes to a continuous
;; block of memory
(df assemble_ (col) (! (! ByteArray withAll: col) _bytes))

;; Jolt doesn't seem to have catch/throw or something equivalent.
;; Here I use a pair of assembly routines as substitue.
;; (catch% FUN) calls FUN with the current stack pointer.
;; (throw% VALUE K) unwinds the stack to K and then returns VALUE.
;; catch% is a bit like call/cc.
;;
;; [Would setjmp/longjmp work from Jolt? or does setjmp require
;;  C-compiler magic?]
;; [I figure Smalltalk has a way to do non-local-exits but, I don't know
;;  how to use that in Jolt.]
;;
(define catch%
  (assemble_
   '(0x55                               ; push   %ebp
     0x89 0xe5                          ; mov    %esp,%ebp
     0x54                               ; push   %esp
     0x8b 0x45 0x08                     ; mov    0x8(%ebp),%eax
     0xff 0xd0                          ; call   *%eax
     0xc9                               ; leave  
     0xc3                               ; ret    
     )))

(define throw%
  (assemble_
   `(,@'()
     0x8b 0x44 0x24 0x04                ; mov    0x4(%esp),%eax
     0x8b 0x6c 0x24 0x08                ; mov    0x8(%esp),%ebp
     0xc9                               ; leave  	
     0xc3                               ; ret    
     )))

(df bar (i  k)
  (if (== i 0)
      (throw% 100 k)
      (begin
       (printf "bar %d\n" i)
       (bar (- i 1) k))))
(df foo (k)
  (printf "foo.1\n")
  (printf "foo.2 %d\n" (bar 10 k)))

;; Our way to produce closures: we compile a new little function which
;; hardcodes the addresses of the code resp. the data-vector.  The
;; nice thing is that such closures can be used called C function
;; pointers.  It's probably slow to invoke the compiler for such
;; things, so use with care.
(df make-closure (addr state)
  (int>_
   (! `(lambda (a b c d)
         (,(_>int addr) ,(_>int state) a b c d))
      eval)))

;; Return a closure which calls FUN with ARGS and the arguments
;; that the closure was called with.  
;; Example: ((curry printf "%d\n") 10)
(defmacro curry (fun . args)
  `(make-closure
    (lambda (state a b c d)
      ((ref state '0)
       ,@(packing (sv)
	   (dotimes (i (len args))
	     (pack `(ref state ',(fx1+ i)) sv)))
       a b c d))
    (vec ,fun ,@args)))

(df parse-closure-arglist (vars)
  (let ((pos (or (index-of vars '|)
                 (return nil)))
        (cvars (! vars copyFrom: '0 to: (fx1- pos)))
        (lvars (! vars copyFrom: (fx1+ pos))))
    (vec cvars lvars)))

;; Create a closure, to-be-closed-over variables must enumerated
;; explicitly.  
;; Example: ((let ((x 1)) (closure (x | y) (+ x y))) 3) => 4.
;; The variables before the "|" are captured by the closure.
(defmacro closure ((. vars) . body)
  (mif ((cvars lvars) (parse-closure-arglist vars))
       `(curry (lambda (,@cvars ,@lvars) ,@body)
               ,@cvars)
       (! compiler errorSyntax: vars)))

;; The analog for Smalltalkish "blocks".
(defmacro block ((. vars) . body)
  (mif ((cvars lvars) (parse-closure-arglist vars))
       `(! StaticBlockClosure 
           function_: (curry (lambda (,@cvars _closure _self ,@lvars) ,@body)
                             ,@cvars)
           arity_: ,(len lvars))
       (! compiler errorSyntax: vars)))

(define %mkstemp (dlsym "mkstemp"))
(df make-temp-file ()
  (let ((name (! '"/tmp/jolt-tmp.XXXXXX" copy))
        (fd (%mkstemp (! name _stringValue))))
    (if (== fd -1)
        (error "mkstemp failed"))
    `(,fd ,name)))
(define %unlink (dlsym "unlink"))
(df unlink (filename) (%unlink (! filename _stringValue)))

(define write (dlsym "write"))
(df write-bytes (addr count fd)
  (let ((written (write fd addr count)))
    (if (!= written count)
        (begin
         (printf "write failed %p %d %d => %d" addr count fd written)
         (error '"write failed")))))

(define system (dlsym "system"))
(define main (dlsym "main"))

;; Starting at address ADDR, disassemble COUNT bytes.
;; This is implemented by writing the memory region to a file
;; and call ndisasm on it.
(df disas (addr count)
  (let ((fd+name (make-temp-file)))
    (write-bytes addr count (first fd+name))
    (let ((cmd (str>_ (cat '"ndisasm -u -o " 
                           (>str (_>fix addr))
                           '" " (second fd+name)))))
      (printf "Running: %s\n" cmd)
      (system cmd))
    (unlink (second fd+name))))

(df rep ()
  (let ((result (! (! CokeScanner read: StdIn) eval)))
    (puts '"=> " StdOut)
    (print result)
    (puts '"\n" StdOut)))

;; Perhaps we could use setcontext/getcontext to return from signal
;; handlers (or not).
(define +ucontext-size+ 350)
(define _getcontext (dlsym "getcontext"))
(define _setcontext (dlsym "setcontext"))
(df getcontext ()
  (let ((context (malloc 350)))
    (_getcontext context)
    context))

(define on_exit (dlsym "on_exit")) ; "atexit" doesn't work. why?

(define *top-level-restart* 0)
(define *top-level-context* 0)
(define *debugger-hook* 0)

;; Jolt's error handling strategy is charmingly simple: call exit.
;; We invoke the SLIME debugger from an exit handler. 
;; (The handler is registered with atexit, that's a libc function.)

(df exit-handler (reason arg)
  (printf "exit-handler 0x%x\n" reason)
  ;;(backtrace)
  (on_exit exit-handler nil)
  (when *debugger-hook*
    (*debugger-hook* `(exit ,reason)))
  (cond (*top-level-context*
         (_setcontext *top-level-context*))
        (*top-level-restart*
         (throw% reason *top-level-restart*))))

(df repl ()
  (set *top-level-context* (getcontext))
  (while (not (! (! StdIn readStream) atEnd))
    (printf "top-level\n")
    (catch%
     (lambda (k)
       (set *top-level-restart* k)
       (printf "repl\n")
       (while 1
         (rep)))))
  (printf "EOF\n"))

;; (repl)


;;; Socket code. (How boring. Duh, should have used netcat instead.)

(define strerror (dlsym "strerror"))

(df check-os-code (value)
  (if (== value -1)
      (error (_>str (strerror (fix>_ (! OS errno)))))
      value))

;; For now just hard-code constants which usually reside in header
;; files (just like a Forth guy would do).
(define PF_INET 2)
(define SOCK_STREAM 1)
(define SOL_SOCKET 1)
(define SO_REUSEADDR 2)
(define socket (dlsym "socket"))
(define setsockopt (dlsym "setsockopt"))

(df set-reuse-address (sock value)
  (let ((word-size 4)
        (val (! Object _balloc: (_>fix word-size))))
    (set-int@ val value)
    (check-os-code
     (setsockopt sock SOL_SOCKET SO_REUSEADDR val word-size))))

(define sockaddr_in/size 16)
(define sockaddr_in/sin_family 0)
(define sockaddr_in/sin_port 2)
(define sockaddr_in/sin_addr 4)
(define INADDR_ANY 0)
(define AF_INET 2)
(define htons (dlsym "htons"))
(define bind (dlsym "bind"))

(df bind-socket (sock port)
  (let ((addr (! OS _balloc: (_>fix sockaddr_in/size))))
    (set-short@ (+ addr sockaddr_in/sin_family) AF_INET)
    (set-short@ (+ addr sockaddr_in/sin_port) (htons port))
    (set-int@ (+ addr sockaddr_in/sin_addr) INADDR_ANY)
    (check-os-code 
     (bind sock addr sockaddr_in/size))))

(define listen (dlsym "listen"))

(df create-socket (port)
  (let ((sock (check-os-code (socket PF_INET SOCK_STREAM 0))))
    (set-reuse-address sock 1)
    (bind-socket sock port)
    (check-os-code (listen sock 1))
    sock))

(define accept% (dlsym "accept"))
(df accept (sock)
  (let ((addr (! OS _balloc: (_>fix sockaddr_in/size)))
        (len (! OS _balloc: 4)))
    (set-int@ len sockaddr_in/size)
    (check-os-code (accept% sock addr len))))

(define getsockname (dlsym "getsockname"))
(define ntohs (dlsym "ntohs"))
(df local-port (sock)
  (let ((addr (! OS _balloc: (_>fix sockaddr_in/size)))
        (len (! OS _balloc: 4)))
    (set-int@ len sockaddr_in/size)
    (check-os-code
     (getsockname sock addr len))
    (ntohs (short@ (+ addr sockaddr_in/sin_port)))))

(define close (dlsym "close"))
(define _read (dlsym "read"))

;; Now, after 2/3 of the file we can begin with the actual Swank
;; server.

(df read-string (fd count)
  (let ((buffer (! String new: count))
        (buffer_ (str>_ buffer))
        (count_ (int>_ count))
        (start 0))
    (while (> (- count_ start) 0)
      (let ((rcount (check-os-code (_read fd 
                                          (+ buffer_ start) 
                                          (- count_ start)))))
        (set start (+ start rcount))))
    buffer))

;; Read and parse a message from the wire.
(df read-packet (fd)
  (let ((header (read-string fd '6))
        (length (! Integer fromString: header base: '16))
        (payload (read-string fd length)))
    (! CokeScanner read: payload)))

;; Print a messag to the wire.
(df send-to-emacs (event fd)
  (let ((stream (! WriteStream on: (! String new: '100))))
    (! stream position: '6)
    (prin1 event stream)
    (let ((len (! stream position)))
      (! stream position: '0)
      (! (fx+ len '-6) printOn: stream base: '16 width: '6)
      (write-bytes (str>_ (! stream collection)) (int>_ len) fd))))

(df add-quotes (form)
  (mcase form
    ((fun . args)
     `(,fun ,@(packing (s)
                (dovec (e args) 
                  (pack `(quote ,e) s)))))))

(define sldb 0) ;defer

(df eval-for-emacs (form id fd abort)
  (let ((old-hook *debugger-hook*))
    (mcase (catch%
            (closure (form fd | k)
              (set *debugger-hook* (curry sldb fd k))
              `(ok ,(int>_ (! (add-quotes form) eval)))))
      (('ok value) 
       (set *debugger-hook* old-hook)
       (send-to-emacs `(:return (:ok ,value) ,id) fd)
       'ok)
      (arg
       (set *debugger-hook* old-hook)
       (send-to-emacs `(:return (:abort) ,id) fd)
       (throw% arg abort)))))

(df process-events (fd)
  (on_exit exit-handler nil)
  (let ((done nil))
    (while (not done)
      (mcase (read-packet fd)
        ((':emacs-rex form package thread id)
         (mcase (catch% (closure (form id fd | abort)
                          (eval-for-emacs form id fd abort)))
           ('ok)
           ;;('abort nil)
           ('top-level)
           (other 
            ;;(return other) ; compiler breaks with return
            (set done 1))))))))

(df next-frame (fp)
  (let ((next (get-caller-fp fp)))
    (if (and (!= next fp) 
             (<= next %top-level-fp))
        next
        nil)))

(df nth-frame (n top)
  (let ((fp top)
        (i 0))
    (while fp
      (if (== i n) (return fp))
      (set fp (next-frame fp))
      (set i (+ i 1)))
    nil))

(define Dl_info/size 16)
(define Dl_info/dli_fname 0)
(define Dl_info/dli_sname 8)

(df get-dl-sym-name (addr)
  (let ((info (! OS _balloc: (_>fix Dl_info/size))))
    (when (== (dladdr addr info) 0)
      (return nil))
    (let ((sname (long@ (+ info Dl_info/dli_sname)) )
          (fname (long@ (+ info Dl_info/dli_fname))))
      (cond ((and sname fname)
             (cat (_>str sname) '" in " (_>str fname)))
            (sname (_>str fname))
            (fname (cat '"<??> " (_>str fname)))
            (true nil)))))

;;(get-dl-sym-name printf)

(df guess-function-name (ip)
  (let ((fname (get-function-name ip)))
    (if fname
        (_>str fname) 
        (get-dl-sym-name ip))))

(df backtrace>el (top_ from_ to_)
  (let ((fp (nth-frame from_ top_))
        (i from_))
    (packing (bt)
      (while (and fp (< i to_))
        (let ((ip (get-frame-ip fp)))
          (pack (vec (_>int i)
                     (cat (or (guess-function-name ip) '"(no-name)")
                          '" " ;;(>str (_>int ip))
                          ))
                bt))
        (set i (+ i 1))
        (set fp (next-frame fp))))))
 
(df debugger-info (fp msg)
  (vec `(,(prin1-to-string msg) " [type ...]" ())
       '(("quit" "Return to top level"))
       (backtrace>el fp 0 20)
       '()))
       
(define *top-frame* 0)
(define *sldb-quit* 0)

(df debugger-loop (fd args abort)
  (let ((fp (get-current-fp)))
    (set *top-frame* fp)
    (send-to-emacs `(:debug 0 1 ,@(debugger-info fp args)) fd)
    (while 1
      (mcase (read-packet fd)
        ((':emacs-rex form package thread id)
         (mcase (catch% (closure (form id fd | k)
                          (set *sldb-quit* k)
                          (eval-for-emacs form id fd k)
                          'ok))
           ('ok nil)
           (other
            (send-to-emacs `(:return (:abort) ,id) fd)
            (throw% other abort))))))))

(df sldb (fd abort args)
  (let ((old-top-frame *top-frame*)
        (old-sldb-quit *sldb-quit*))
    (mcase (catch% (curry debugger-loop fd args))
      (value
       (set *top-frame* old-top-frame)
       (set *sldb-quit* old-sldb-quit)
       (send-to-emacs `(:debug-return 0 1 nil) fd)
       (throw% value abort)))))

(df swank:backtrace (start end)
  (backtrace>el *top-frame* (int>_ start) (int>_ end)))
 
(df sldb-quit ()
  (assert *sldb-quit*)
  (throw% 'top-level *sldb-quit*))

(df swank:invoke-nth-restart-for-emacs (...) (sldb-quit))
(df swank:throw-to-toplevel (...) (sldb-quit))

(df setup-server (port announce)
  (let ((sock (create-socket port)))
    (announce sock)
    (let ((client (accept sock)))
      (process-events client)
      (close client))
    (printf "Closing socket: %d %d\n" sock (local-port sock))
    (close sock)))

(df announce-port (sock)
  (printf "Listening on port: %d\n" (local-port sock)))

(df create-server (port) (setup-server port announce-port))

(df write-port-file (filename sock) 
  (let ((f (! File create: filename)))
    (! f write: (print-to-string (_>int (local-port sock))))
    (! f close)))

(df start-swank (port-file)
  (setup-server 0 (curry write-port-file (_>str port-file))))

(define getpid (dlsym "getpid"))
(df swank:connection-info ()
  `(,@'()
    :pid ,(_>int (getpid))
    :style nil
    :lisp-implementation (,@'()
                          :type "Coke" 
                          :name "jolt" 
                          :version ,(! CodeGenerator versionString))
    :machine (:instance "" :type ,(! OS architecture) :version "")
    :features ()
    :package (:name "jolt" :prompt "jolt")))

(df swank:listener-eval (string)
  (let ((result (! (! CokeScanner read: string) eval)))
    `(:values ,(prin1-to-string (if (or (fix? result) 
					(and (valid-pointer? result)
					     (int? result)))
				    (int>_ result)
				    result))
	      ,(prin1-to-string result))))

(df swank:interactive-eval (string)
  (let ((result (! (! CokeScanner read: string) eval)))
    (cat '"=> " (prin1-to-string (if (or (fix? result)
                                         (and (valid-pointer? result)
                                              (int? result)))
                                     (int>_ result)
                                     result))
         '", " (prin1-to-string result))))

(df swank:operator-arglist () nil)
(df swank:buffer-first-change () nil)
(df swank:create-repl (_) '("jolt" "jolt"))

(df min (x y) (if (<= x y) x y))

(df common-prefix2 (e1 e2)
  (let ((i '0)
        (max (min (len e1) (len e2))))
    (while (and (< i max)
                (== (ref e1 i) (ref e2 i)))
      (set i (fx1+ i)))
    (! e1 copyFrom: '0 to: (fx1- i))))

(df common-prefix (seq)
  (mcase seq
    (() nil)
    (_
     (let ((prefix (ref seq '0)))
       (dovec (e seq)
         (set prefix (common-prefix2 prefix e)))
       prefix))))

(df swank:simple-completions (prefix _package)
  (let ((matches (packing (s)
                   (dovec (e (! TheGlobalEnvironment keys))
                     (let ((name (>str e)))
                       (when (! name beginsWith: prefix)
                         (pack name s)))))))
    (vec matches (or (common-prefix matches) prefix))))


;; swank-jolt.k ends here
