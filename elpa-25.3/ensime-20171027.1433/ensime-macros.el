;;; ensime-macros.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.

(eval-when-compile (require 'cl))

(require 'cl-lib)

(defmacro ensime-plist-bind (args expr &rest body)
  ;; http://emacs.stackexchange.com/questions/22542
  "`destructuring-bind' without the boilerplate for plists."
  `(cl-destructuring-bind
       (&key ,@args &allow-other-keys)
       ,expr
     ,@body))

(defmacro ensime-with-conn-interactive (conn-sym &rest body)
  "Surround body forms with a check to see if we're connected.
If not, message the user."
  `(let* ((,conn-sym (or (ensime-connection-or-nil)
			 (ensime-prompt-for-connection))))
     (if conn
	 (progn ,@body)
       (message
	"This command requires a connection to an ENSIME server."))))

(defmacro* ensime-with-popup-buffer ((name &optional connection select major-mode-fn)
				     &body body)
  "Similar to `with-output-to-temp-buffer'.
Bind standard-output and initialize some buffer-local variables.
Restore window configuration when closed.

NAME is the name of the buffer to be created.
CONNECTION is the value for `ensime-buffer-connection'.
If nil, no explicit connection is associated with
the buffer.  If t, the current connection is taken.
SELECT determines whether the new window is selected.
MAJOR-MODE-FN, if non-nil, is executed immediately after the new
buffer is created, for example to set the major mode.
"
  `(let* ((vars% (list ,(if (eq connection t) '(ensime-connection) connection)))
	  (standard-output (ensime-make-popup-buffer ,name vars% ,major-mode-fn)))
     (with-current-buffer standard-output
       (prog1
	   (progn
	     ,@body)
	 (assert (eq (current-buffer) standard-output))
	 (setq buffer-read-only t)
	 (set-window-point (ensime-display-popup-buffer ,(or select 'nil))
			   (point))))))

(defmacro ensime-assert-buffer-saved-interactive (&rest body)
  "Offer to save buffer if buffer is modified. Execute body only if
buffer is saved."
  `(if (buffer-modified-p)
       (if (y-or-n-p "Buffer must be saved to continue. Save now? ")
	   (progn
	     (ensime-save-buffer-no-hooks)
	     ,@body))
     (progn
       ,@body)))

(defmacro* ensime-with-connection-buffer ((&optional process) &rest body)
  "Execute BODY in the process-buffer of PROCESS.
If PROCESS is not specified, `ensime-connection' is used.

\(fn (&optional PROCESS) &body BODY))"
  `(with-current-buffer
       (process-buffer (or ,process (ensime-connection)
			   (error "No connection")))
     ,@body))

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
	 ,@(mapcar (lambda (clause)
		     (if (eq (car clause) t)
			 `(t ,@(cdr clause))
		       (destructuring-bind ((op &rest rands) &rest body) clause
			 `(,op (destructuring-bind ,rands ,operands
				 . ,body)))))
		   patterns)
	 ,@(if (eq (caar (last patterns)) t)
	       '()
	     `((t (error "Elisp destructure-case failed: %S" ,tmp))))))))


(defmacro ensime-define-keys (keymap &rest key-command)
  "Define keys in KEYMAP. Each KEY-COMMAND is a list of (KEY COMMAND)."
  `(progn . ,(mapcar (lambda (k-c) `(define-key ,keymap . ,k-c))
		     key-command)))


(defmacro* with-struct ((conc-name &rest slots) struct &body body)
  "Like with-slots but works only for structs.
\(fn (CONC-NAME &rest SLOTS) STRUCT &body BODY)"
  (cl-flet ((reader (slot) (intern (concat (symbol-name conc-name)
					(symbol-name slot)))))
    (let ((struct-var (gensym "struct")))
      `(let ((,struct-var ,struct))
	 (symbol-macrolet
	     ,(mapcar (lambda (slot)
			(etypecase slot
			  (symbol `(,slot (,(reader slot) ,struct-var)))
			  (cons `(,(first slot) (,(reader (second slot))
						 ,struct-var)))))
		      slots)
	   . ,body)))))


(defvar ensime-qualified-type-regexp
  "^\\(?:object \\)?\\(\\(?:[a-z0-9_]+\\.\\)*\\)\\(?:\\([^\\.]+?\\)\\$\\)?\\(\\$\\$anon\\|[^\\.$]+\\$?\\)$"
  "Match strings of form pack.pack1.pack2.Types$Type or pack.pack1.pack2.Type")
(defmacro* ensime-with-name-parts (str (path outer-type-name name) &rest body)
  "Evaluate BODY with path bound to the dot-separated path of
 this type-name, and name bound to the final type name."
  (let ((tmp (gensym)))
    `(let ((matchedp (integerp (string-match
				ensime-qualified-type-regexp
				,str))))
       (let* ((,tmp (if matchedp (match-string 1 ,str) nil))
	      (,path (if (> (length ,tmp) 0)
			 (substring ,tmp 0 (- (length ,tmp) 1)) ,tmp))
	      (,outer-type-name (if matchedp (match-string 2 ,str) nil))
	      (,name (if matchedp (match-string 3 ,str) ,str)))
	 ,@body))))

(defvar ensime-qualified-path-and-name-regexp
  "^\\(\\(?:[a-z0-9_]+\\.\\)*\\)\\([^\\.]*\\)$")
(defmacro* ensime-with-path-and-name (str (path name) &rest body)
  "Evaluate body with path bound to all sections up to the
 last, concatenated, and name bound to the last section."
  (let ((tmp (gensym)))
    `(let ((matchedp (integerp (string-match
				ensime-qualified-path-and-name-regexp
				,str))))
       (let* ((,tmp (if matchedp (match-string 1 ,str) nil))
	      (,path (if (> (length ,tmp) 0)
			 (substring ,tmp 0 (- (length ,tmp) 1)) ,tmp))
	      (,name (if matchedp (match-string 2 ,str) nil)))
	 ,@body))))


(defmacro* ensime-with-buffer-written-to-tmp ((file-sym) &rest body)
  "Write latest buffer state to a temp file, bind the temp filename
 to file-sym, and eval body. The idea is to not disturb the original
 file's state."
  `(let ((,file-sym (ensime-temp-file-name
		     (concat ".tmp_" (file-name-nondirectory
				      (buffer-file-name-with-indirect))))))
     (ensime-write-buffer ,file-sym)
     ,@body))

(defmacro ensime-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (let ((start (gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))

(defmacro ensime-with-rigid-indentation (level &rest body)
  "Execute BODY and then rigidly indent its text insertions.
Assumes all insertions are made at point."
  (let ((start (gensym)) (l (gensym)))
    `(let ((,start (point)) (,l ,(or level '(current-column))))
       (prog1 (progn ,@body)
	 (ensime-indent-rigidly ,start (point) ,l)))))

(put 'ensime-with-rigid-indentation 'lisp-indent-function 1)

(defmacro ensime-def-connection-var (varname &rest initial-value-and-doc)
  "Define a connection-local variable.
The value of the variable can be read by calling the function of the
same name (it must not be accessed directly). The accessor function is
setf-able.

The actual variable bindings are stored buffer-local in the
process-buffers of connections. The accessor function refers to
the binding for `ensime-connection'."
  (let ((real-var (intern (format "%s:connlocal" varname)))
        (store-var (gensym)))
    `(progn
       ;; Variable
       (make-variable-buffer-local
       (defvar ,real-var ,@initial-value-and-doc))
       ;; Accessor
       (defun ,varname (&optional process)
         (ensime-with-connection-buffer (process) ,real-var))
       ;; Setf
       (defsetf ,varname (&optional process) (store)
         `(let ((,',store-var ,store))
            (ensime-with-connection-buffer (,process)
              (setq ,',real-var ,',store-var)
              ,',store-var)))
       '(\, varname))))

(put 'ensime-def-connection-var 'lisp-indent-function 2)
(put 'ensime-indulge-pretty-colors 'ensime-def-connection-var t)

;;; `ensime-rex' is the RPC primitive which is used to implement both
;;; `ensime-eval' and `ensime-eval-async'. You can use it directly if
;;; you need to, but the others are usually more convenient.

(defmacro* ensime-rex ((&rest saved-vars)
                       sexp
                       &rest continuations)
  "(ensime-rex (VAR ...) SEXP CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Lisp.

CLAUSES is a list of patterns with same syntax as
`destructure-case'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:abort REASON).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (let ((result (gensym)))
    `(lexical-let ,(loop for var in saved-vars
                         collect (etypecase var
                                   (symbol (list var var))
                                   (cons var)))
       (ensime-dispatch-event
        (list :swank-rpc ,sexp
              (lambda (,result)
                (destructure-case ,result
                                  ,@continuations)))))))

(put 'ensime-rex 'lisp-indent-function 2)

(defmacro ensime-set-key (conf key val)
  `(setq ,conf (plist-put ,conf ,key ,val)))

(defmacro* ensime-db-with-active-thread ((tid-sym) &rest body)
  `(if ensime-db-active-thread-id
       (let ((,tid-sym ensime-db-active-thread-id))
         ,@body)
     (message "No active debug thread.")))

(defmacro ensime--propertize-inserted-text (prop-list &rest body)
  `(let ((start-props-point (point)))
     ,@body
     (add-text-properties start-props-point (point) (list ,@prop-list))))

(provide 'ensime-macros)

;; Local Variables:
;; End:
