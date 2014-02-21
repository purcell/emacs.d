;;; workgroups-utils.el --- Utilities used by Workgroups
;;
;; Copyright (C) 2010, 2011 tlh
;;
;; Author: tlh <thunkout at gmail dot com>
;; Keywords: session management window-configuration persistence
;; Homepage: https://github.com/tlh/workgroups.el
;; Version   1.0.0

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; A bunch of general purpose-ish utilities used by Workgroups.
;;
;;; Code:

;;; utils used in macros

(require 'cl-lib)

(defmacro wg-with-gensyms (syms &rest body)
  "Bind all symbols in SYMS to `gensym's, and eval BODY."
  (declare (indent 1))
  `(let (,@(mapcar (lambda (sym) `(,sym (cl-gensym))) syms)) ,@body))

(defmacro wg-dbind (args expr &rest body)
  "Bind the variables in ARGS to the result of EXPR and execute BODY.
Abbreviation of `destructuring-bind'."
  (declare (indent 2))
  `(cl-destructuring-bind ,args ,expr ,@body))

(defun wg-partition (list &optional n step)
  "Return list of N-length sublists of LIST, offset by STEP.
N defaults to 2, and STEP defaults to N.
Iterative to prevent stack overflow."
  (let* ((n (or n 2)) (step (or step n)) acc)
    (while list
      (push (wg-take list n) acc)
      (setq list (nthcdr step list)))
    (nreverse acc)))



;;; bindings

(defmacro wg-if-let (cond-form then &rest else)
  "Bind VAR to the return value of COND.  If VAR is non-nil, do THEN.
Else do ELSE...

\(fn ((VAR COND) THEN ELSE...)"
  (declare (indent 2))
  `(let (,cond-form)
     (if ,(car cond-form) ,then ,@else)))

(defmacro wg-when-let (binds &rest body)
  "Like `let*' but when all BINDS are non-nil - eval BODY."
  (declare (indent 1))
  (wg-dbind (bind . binds) binds
    (when (consp bind)
      `(let (,bind)
         (when ,(car bind)
           ,(if (not binds) `(progn ,@body)
              `(wg-when-let ,binds ,@body)))))))

(defmacro wg-when-boundp (symbols &rest body)
  "When all SYMBOLS are bound, `eval' BODY."
  (declare (indent 1))
  `(when (and ,@(mapcar (lambda (sym) `(boundp ',sym)) symbols))
     ,@body))



;;; do-style wrappers

(defmacro wg-docar (spec &rest body)
  "do-style wrapper for `mapcar'.

\(fn (VAR LIST) BODY...)"
  (declare (indent 1))
  `(mapcar (lambda (,(car spec)) ,@body) ,(cadr spec)))

(defmacro wg-dohash (spec &rest body)
  "do-style wrapper for `maphash'.

\(fn (KEY VALUE TABLE [RESULT]) BODY...)"
  (declare (indent 1))
  (wg-dbind (key val table &optional result) spec
    `(progn (maphash (lambda (,key ,val) ,@body) ,table) ,result)))

(defmacro wg-doconcat (spec &rest body)
  "do-style wrapper for `mapconcat'.

\(fn (VAR SEQ [SEPARATOR]) BODY...)"
  (declare (indent 1))
  (wg-dbind (elt seq &optional sep) spec
    `(mapconcat (lambda (,elt) ,@body) ,seq (or ,sep ""))))



;;; anaphora

(defmacro wg-aif (test then &rest else)
  "Anaphoric `if'."
  (declare (indent 2))
  `(let ((it ,test)) (if it ,then ,@else)))

(defmacro wg-awhen (test &rest body)
  "Anaphoric `when'."
  (declare (indent 1))
  `(wg-aif ,test (progn ,@body)))

(defmacro wg-acond (&rest clauses)
  "Anaphoric `cond'."
  (when clauses
    (wg-dbind ((condition . body) . rest) clauses
      `(wg-aif ,condition (progn ,@body)
         (wg-acond ,@rest)))))

(defmacro wg-aand (&rest args)
  "Anaphoric `and'."
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(wg-aif ,(car args) (aand ,@(cdr args))))))

(defmacro wg-asetf (&rest places-and-values)
  "Anaphoric `setf'."
  `(progn ,@(mapcar (lambda (pv) `(let ((it ,(car pv))) (setf ,@pv)))
                    (wg-partition places-and-values 2))))



;;; other control structures

(defmacro wg-until (test &rest body)
  "`while' not."
  (declare (indent 1))
  `(while (not ,test) ,@body))

(defmacro wg-destructuring-dolist (spec &rest body)
  "Loop over a list.
Evaluate BODY, destructuring LIST into SPEC, then evaluate RESULT
to get a return value, defaulting to nil.  The only hitch is that
spec must end in dotted style, collecting the rest of the list
into a var, like so: (a (b c) . rest)

\(fn (SPEC LIST [RESULT]) BODY...)"
  (declare (indent 1))
  (wg-dbind (loopspec list &optional result) spec
    (let ((rest (cdr (last loopspec))))
      (wg-with-gensyms (list-sym)
        `(let ((,list-sym ,list))
           (while ,list-sym
             (wg-dbind ,loopspec ,list-sym
               ,@body
               (setq ,list-sym ,rest)))
           ,result)))))



;;; boolean operators

(defmacro wg-eager-or (&rest conditions)
  "Evaluate all CONDITIONS.  Return the first non-nil return value."
  (let ((syms (mapcar (lambda (x) (cl-gensym)) conditions)))
    `(let ,(cl-mapcar 'list syms conditions)
       (or ,@syms))))

(defmacro wg-eager-and (&rest conditions)
  "Evaluate all conditions.  If any return nil, return nil.
Otherwise return the return value of the last condition."
  (let ((syms (mapcar (lambda (x) (cl-gensym)) conditions)))
    `(let ,(cl-mapcar 'list syms conditions)
       (and ,@syms))))



;;; numbers

(defun wg-step-to (n m step)
  "Increment or decrement N toward M by STEP.
Return M when the difference between N and M is less than STEP."
  (cond ((= n m) n)
        ((< n m) (min (+ n step) m))
        ((> n m) (max (- n step) m))))

(defun wg-within (num lo hi &optional hi-inclusive)
  "Return t when NUM is within bounds LO and HI.
HI-INCLUSIVE non-nil means the HI bound is inclusive."
  (and (>= num lo) (if hi-inclusive (<= num hi) (< num hi))))

(defun wg-int-to-b36-one-digit (i)
  "Return a character in 0..9 or A..Z from I, and integer 0<=I<32.
Cribbed from `org-id-int-to-b36-one-digit'."
  (cond ((not (wg-within i 0 36))
         (error "%s out of range" i))
        ((< i 10) (+ ?0 i))
        ((< i 36) (+ ?A i -10))))

(defun wg-b36-to-int-one-digit (i)
  "Turn a character 0..9, A..Z, a..z into a number 0..61.
The input I may be a character, or a single-letter string.
Cribbed from `org-id-b36-to-int-one-digit'."
  (and (stringp i) (setq i (string-to-char i)))
  (cond ((and (>= i ?0) (<= i ?9)) (- i ?0))
        ((and (>= i ?A) (<= i ?Z)) (+ (- i ?A) 10))
        (t (error "Invalid b36 character"))))

(defun wg-int-to-b36 (i &optional length)
  "Return a base 36 string from I."
  (let ((base 36) b36)
    (cl-labels ((add-digit () (push (wg-int-to-b36-one-digit (mod i base)) b36)
                         (setq i (/ i base))))
      (add-digit)
      (while (> i 0) (add-digit))
      (setq b36 (cl-map 'string 'identity b36))
      (if (not length) b36
        (concat (make-string (max 0 (- length (length b36))) ?0) b36)))))

(defun wg-b36-to-int (str)
  "Convert STR, a base-36 string, into the corresponding integer.
Cribbed from `org-id-b36-to-int'."
  (let ((result 0))
    (mapc (lambda (i)
            (setq result (+ (* result 36)
                            (wg-b36-to-int-one-digit i))))
          str)
    result))



;;; lists

(defmacro wg-removef-p (item seq-place &rest keys)
  "If ITEM is a `member*' of SEQ-PLACE, remove it from SEQ-PLACE and return t.
Otherwise return nil.  KEYS can be any keywords accepted by `remove*'."
  `(> (length ,seq-place)
      (length (setf ,seq-place (cl-remove ,item ,seq-place ,@keys)))))

(defmacro wg-pushnew-p (item seq-place &rest keys)
  "If ITEM is not a `member' of SEQ-PLACE, push it to SEQ-PLACE and return t.
Otherwise return nil.  KEYS can be any keyword args accepted by `pushnew'."
  `(< (length ,seq-place)
      (length (cl-pushnew ,item ,seq-place ,@keys))))

(defun wg-last1 (list)
  "Return the last element of LIST."
  (car (last list)))

(defun wg-take (list n)
  "Return a list of the first N elts in LIST."
  (butlast list (- (length list) n)))

(defun wg-leave (list n)
  "Return a list of the last N elts in LIST."
  (nthcdr (- (length list) n) list))

(defun wg-rnth (n list)
  "Return the Nth element of LIST, counting from the end."
  (nth (- (length list) n 1) list))

(defun wg-take-until-fail (pred list)
  "Take elements from LIST up to the first element on which PRED fails."
  (let (taken)
    (catch 'result
      (dolist (elt list (nreverse taken))
        (if (funcall pred elt) (push elt taken)
          (throw 'result (nreverse taken)))))))

(defun wg-range (start end)
  "Return a list of integers from START up to but not including END."
  (let (accum)
    (dotimes (i (- end start) (nreverse accum))
      (push (+ start i) accum))))

(defun wg-rotate-list (list &optional offset)
  "Rotate LIST by OFFSET.  Positive OFFSET rotates left, negative right."
  (when list
    (let ((split (mod (or offset 1) (length list))))
      (append (nthcdr split list) (wg-take list split)))))

(defun wg-center-rotate-list (list)
  "Rotate LIST so it's first elt is in the center.  When LIST's
length is even, the first elt is left nearer the front."
  (wg-rotate-list list (- (/ (1- (length list)) 2))))

(defun wg-insert-after (elt list index)
  "Insert ELT into LIST after INDEX."
  (let ((new-list (cl-copy-list list)))
    (push elt (cdr (nthcdr index new-list)))
    new-list))

(defun wg-insert-before (elt list index)
  "Insert ELT into LIST before INDEX."
  (if (zerop index) (cons elt list)
    (wg-insert-after elt list (1- index))))

(defun wg-move-elt (elt list index &rest keys)
  "Move ELT before INDEX in LIST.
KEYS is passed to `remove*'."
  (wg-insert-before elt (apply 'cl-remove elt list keys) index))

(defun wg-cyclic-nth (list n)
  "Return the Nth element of LIST, modded by the length of list."
  (nth (mod n (length list)) list))

(defun wg-cyclic-offset-elt (elt list n)
  "Cyclically offset ELT's position in LIST by N."
  (wg-when-let ((pos (cl-position elt list)))
    (wg-move-elt elt list (mod (+ n pos) (length list)))))

(defun wg-cyclic-nth-from-elt (elt list n &rest keys)
  "Return the elt in LIST N places cyclically from ELT.
If ELT is not present is LIST, return nil.
KEYS is passed to `position'."
  (wg-when-let ((pos (apply 'cl-position elt list keys)))
    (wg-cyclic-nth list (+ pos n))))

(defun wg-util-swap (elt1 elt2 list)
  "Return a copy of LIST with ELT1 and ELT2 swapped.
Return nil when ELT1 and ELT2 aren't both present."
  (wg-when-let ((p1 (cl-position elt1 list))
                (p2 (cl-position elt2 list)))
    (wg-move-elt elt1 (wg-move-elt elt2 list p1) p2)))

(defun wg-dups-p (list &rest keys)
  "Return non-nil when LIST contains duplicate elements.

Keywords supported: :test :key

\(fn LIST [KEYWORD VALUE]...)"
  (let ((test (or (plist-get keys :test) 'eq))
        (key (or (plist-get keys :key) 'identity)))
    (cl-loop for (elt . rest) on list
             for elt = (funcall key elt)
             when (cl-find elt rest :test test :key key) return elt)))

(defun wg-string-list-union (&optional list1 list2)
  "Return the `union' of LIST1 and LIST2, using `string=' as the test.
This only exists to get rid of duplicate lambdas in a few reductions."
  (cl-union list1 list2 :test 'string=))



;;; alists

(defun wg-make-alist (&rest kvps)
  "Return a new alist from KVPS."
  (let (alist)
    (while kvps
      (push (cons (car kvps) (cadr kvps)) alist)
      (setq kvps (cddr kvps)))
    (nreverse alist)))

(defun wg-aget (alist key &optional default)
  "Return the value of KEY in ALIST. Uses `assq'.
If PARAM is not found, return DEFAULT which defaults to nil."
  (wg-aif (assq key alist) (cdr it) default))

(defun wg-acopy (alist)
  "Return a copy of ALIST's toplevel list structure."
  (mapcar (lambda (kvp) (cons (car kvp) (cdr kvp))) alist))

(defun wg-aput (alist key value)
  "Return a new alist from ALIST with KEY's value set to VALUE."
  (let* ((found nil)
         (new (wg-docar (kvp alist)
                (if (not (eq key (car kvp))) kvp
                  (setq found (cons key value))))))
    (if found new (cons (cons key value) new))))

(defun wg-aremove (alist key)
  "`remove' KEY's key-value-pair from ALIST."
  (remove (assoc key alist) alist))

(defmacro wg-abind (alist binds &rest body)
  "Bind values in ALIST to symbols in BINDS, then eval BODY.
If an elt of BINDS is a symbol, use it as both the bound variable
and the key in ALIST.  If it is a cons, use the car as the bound
variable, and the cadr as the key."
  (declare (indent 2))
  (wg-with-gensyms (asym)
    `(let* ((,asym ,alist)
            ,@(wg-docar (bind binds)
                (let ((c (consp bind)))
                  `(,(if c (car bind) bind)
                    (wg-aget ,asym ',(if c (cadr bind) bind))))))
       ,@body)))



;;; hash-tables

(defun wg-fill-hash-table (table &rest key-value-pairs)
  "Fill TABLE with KEY-VALUE-PAIRS and return TABLE."
  (while key-value-pairs
    (puthash (car key-value-pairs) (cadr key-value-pairs) table)
    (setq key-value-pairs (cddr key-value-pairs)))
  table)



;;; symbols and strings

(defun wg-toggle (symbol)
  "Toggle SYMBOL's truthiness."
  (set symbol (not (symbol-value symbol))))

(defun wg-symcat (&rest symbols-and-strings)
  "Return a new interned symbol by concatenating SYMBOLS-AND-STRINGS."
  (intern (mapconcat (lambda (obj) (if (symbolp obj) (symbol-name obj) obj))
                     symbols-and-strings "")))

(defun wg-make-string (times string &optional separator)
  "Like `make-string', but includes a separator."
  (mapconcat 'identity (make-list times string) (or separator "")))



;;; buffers

(defun wg-get-buffer (buffer-or-name)
  "Return BUFFER-OR-NAME's buffer, or error."
  (or (get-buffer buffer-or-name)
      (error "%S does not identify a buffer" buffer-or-name)))

(defun wg-buffer-name (buffer-or-name)
  "Return BUFFER-OR-NAME's `buffer-name', or error."
  (buffer-name (wg-get-buffer buffer-or-name)))

(defun wg-buffer-file-name (buffer-or-name)
  "Return BUFFER-OR-NAME's `buffer-file-name', or error."
  (buffer-file-name (wg-get-buffer buffer-or-name)))

(defun wg-buffer-major-mode (buffer-or-name)
  "Return BUFFER's major-mode."
  (with-current-buffer buffer-or-name major-mode))

(defun wg-current-buffer-p (buffer-or-name)
  "Return t if BUFFER-OR-NAME is the current buffer, nil otherwise."
  (eq (wg-get-buffer buffer-or-name) (current-buffer)))

(defmacro wg-buffer-local-setq (buffer var value)
  "`setq' VAR to VALUE while BUFFER is current.
Note that this won't make VAR buffer-local if it isn't already."
  `(with-current-buffer ,buffer (setq ,var ,value)))

(defun wg-interesting-buffers ()
  "Return a list of only the interesting buffers in `buffer-list'."
  (cl-remove-if (lambda (bname) (string-match "^ " bname))
                (buffer-list) :key 'buffer-name))

(defun wg-get-first-buffer-matching-regexp (regexp &optional buffer-list)
  "Return the first buffer in BUFFER-LIST with a name matching REGEXP.
BUFFER-LIST should contain buffer objects and/or buffer names."
  (cl-find regexp (or buffer-list (buffer-list))
           :test 'string-match :key 'wg-buffer-name))



;;; files

(defun wg-write-sexp-to-file (sexp file)
  "Write the printable representation of SEXP to FILE."
  (with-temp-buffer
    (let ((print-level nil)  (print-length nil))
      (insert (format "%S" sexp)))
    (write-file file)))

(defun wg-read-sexp-from-file (file)
  "Return a Lisp object from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))
(defalias 'wg-lisp-object-from-file 'wg-read-sexp-from-file)

(defun wg-file-under-root-path-p (root-path file-path)
  "Return t when FILE-PATH is under ROOT-PATH, nil otherwise."
  (string-match (concat "^" (regexp-quote (expand-file-name root-path)))
                (expand-file-name file-path)))



;;; frames

(defun wg-cyclic-nth-from-frame (&optional n frame)
  "Return the frame N places away from FRAME in `frame-list' cyclically.
N defaults to 1, and FRAME defaults to `selected-frame'."
  (wg-cyclic-nth-from-elt
   (or frame (selected-frame)) (frame-list) (or n 1)))



;;; namespace-prefixed defstruct

(defmacro wg-defstruct (prefix name-form &rest slot-defs)
  "`defstruct' wrapper that namespace-prefixes all generated functions.
Note: this doesn't yet work with :conc-name, and possibly other
options."
  (declare (indent 2))
  (let* ((name (if (consp name-form) (car name-form) name-form))
         (prefixed-name (wg-symcat prefix "-" name)))
    (cl-labels ((rebind (opstr)
                      (let ((oldfnsym (wg-symcat opstr "-" prefix "-" name)))
                        `((fset ',(wg-symcat prefix "-" opstr "-" name)
                                (symbol-function ',oldfnsym))
                          (fmakunbound ',oldfnsym)))))
      ;; `eval-and-compile' gets rid of byte-comp warnings ("function `foo' not
      ;; known to be defined").  We can accomplish this with `declare-function'
      ;; too, but it annoyingly requires inclusion of the function's arglist,
      ;; which gets ugly.
      `(eval-and-compile
         (cl-defstruct ,(if (symbolp name-form) prefixed-name
                       `(,prefixed-name ,@(cdr name-form)))
           ,@slot-defs)
         ,@(rebind "make")
         ,@(rebind "copy")
         ',prefixed-name))))

(defmacro wg-with-slots (obj slot-bindings &rest body)
  "Bind OBJ's slot values to symbols in BINDS, then eval BODY.
The car of each element of SLOT-BINDINGS is the bound symbol, and
the cadr as the accessor function."
  (declare (indent 2))
  (wg-with-gensyms (objsym)
    `(let* ((,objsym ,obj)
            ,@(wg-docar (slot slot-bindings)
                `(,(car slot) (,(cadr slot) ,objsym))))
       ,@body)))



;;; misc

(defun wg-minibuffer-inactive-p ()
  "Return t when `minibuffer-depth' returns zero, nil otherwise."
  (zerop (minibuffer-depth)))

(defun wg-minibuffer-active-p ()
  "Return t when `minibuffer-depth' does not return zero, nil otherwise."
  (not (wg-minibuffer-inactive-p)))

(defun wg-fill-keymap (keymap &rest binds)
  "Return KEYMAP after defining in it all keybindings in BINDS."
  (while binds
    (define-key keymap (car binds) (cadr binds))
    (setq binds (cddr binds)))
  keymap)

(defun wg-add-or-remove-hooks (remove &rest pairs)
  "Add FUNCTION to or remove it from HOOK, depending on REMOVE."
  (dolist (pair (wg-partition pairs 2))
    (funcall (if remove 'remove-hook 'add-hook)
             (car pair) (cadr pair))))


(defun wg-read-object (prompt test warning &optional initial-contents keymap
                              read hist default-value inherit-input-method)
  "PROMPT for an object that satisfies TEST, WARNING if necessary.
ARGS are `read-from-minibuffer's args, after PROMPT."
  (cl-labels ((read () (read-from-minibuffer
                      prompt initial-contents keymap read hist
                      default-value inherit-input-method)))
    (let ((obj (read)))
      (when (and (equal obj "") default-value) (setq obj default-value))
      (while (not (funcall test obj))
        (message warning)
        (sit-for wg-minibuffer-message-timeout)
        (setq obj (read)))
      obj)))

(defvar wg-readable-types
  '(integer float cons symbol vector string char-table bool-vector)
  "List of types with readable printed representations.")

(defun wg-is-readable-p (obj)
  "Return non-nil if OBJ's printed representation is readable."
  (memq (type-of obj) wg-readable-types))

(defun wg-take-until-unreadable (list)
  "Return a new list of elements of LIST up to the first unreadable element."
  (wg-take-until-fail 'wg-is-readable-p list))

(defun wg-add-face (facekey string)
  "Return a copy of STRING fontified according to FACEKEY.
FACEKEY must be a key in `wg-face-abbrevs'."
  (let ((face (wg-aget wg-face-abbrevs facekey))
        (string  (copy-sequence string)))
    (unless face (error "No face with key %s" facekey))
    (if (not wg-use-faces) string
      (put-text-property 0 (length string) 'face face string)
      string)))

(defmacro wg-fontify (&rest specs)
  "A small fontification DSL.
The results of all SPECS are `concat'd together.
If a spec is a cons with a keyword car, apply `wg-add-face' to it.
Other conses get evaluated, and should produce a strings.
If a spec is a string it is used unmodified.
Anything else is formatted with %s to produce a string."
  (declare (indent defun))
  `(concat
    ,@(wg-docar (spec specs)
        (cond ((and (consp spec)
                    (keywordp (car spec)))
               `(wg-add-face ,@spec))
              ;;((listp spec) (cdr (eval spec)))
              ;;((listp spec)
              ;; ;;(prin1-to-string (nth 1 (eval spec)))
              ;; )
              ((consp spec) spec)
              ((stringp spec) spec)
              (t `(format "%s" ,spec))))))

(defun wg-barf-on-active-minibuffer ()
  "Throw an error when the minibuffer is active."
  (when (wg-minibuffer-active-p)
    (error "Workgroup operations aren't permitted while the \
minibuffer is active")))

(defmacro wg-set-parameter (place parameter value)
  "Set PARAMETER to VALUE at PLACE.
This needs to be a macro to allow specification of a setf'able place."
  (wg-with-gensyms (p v)
    `(let ((,p ,parameter) (,v ,value))
       (wg-pickelable-or-error ,p)
       (wg-pickelable-or-error ,v)
       (setf ,place (wg-aput ,place ,p ,v))
       ,v)))



;;; uid utils

(defun wg-time-to-b36 (&optional time)
  "Convert `current-time' into a b36 string."
  (apply 'concat (wg-docar (time (or time (current-time)))
                   (wg-int-to-b36 time 4))))

(defun wg-b36-to-time (b36)
  "Parse the time from UID."
  (cl-loop for i from 0 to 8 by 4
           collect (wg-b36-to-int (cl-subseq b36 i (+ i 4)))))


(defalias 'wg-uid-to-time 'wg-b36-to-time)

(defun wg-generate-uid (&optional prefix)
  "Return a new uid, optionally prefixed by PREFIX."
  (concat prefix
          (wg-time-to-b36)
          "-"
          (wg-int-to-b36 string-chars-consed)))

(defun wg-uid-to-seconds (uid)
  "Return the `float-time' parsed from UID with `wg-uid-to-time'."
  (float-time (wg-uid-to-time uid)))


(provide 'workgroups-utils-basic)
;;; workgroups-utils-basic.el ends here
