;;; ob-core.el --- Working with Code Blocks          -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	Dan Davison
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'cl-lib)
(require 'ob-eval)
(require 'org-macs)
(require 'org-compat)

(defconst org-babel-exeext
  (if (memq system-type '(windows-nt cygwin))
      ".exe"
    nil))

(defvar org-babel-library-of-babel)
(defvar org-edit-src-content-indentation)
(defvar org-src-lang-modes)
(defvar org-src-preserve-indentation)

(declare-function org-at-item-p "org-list" ())
(declare-function org-at-table-p "org" (&optional table-type))
(declare-function org-babel-lob-execute-maybe "ob-lob" ())
(declare-function org-babel-ref-goto-headline-id "ob-ref" (id))
(declare-function org-babel-ref-headline-body "ob-ref" ())
(declare-function org-babel-ref-parse "ob-ref" (assignment))
(declare-function org-babel-ref-resolve "ob-ref" (ref))
(declare-function org-babel-ref-split-args "ob-ref" (arg-string))
(declare-function org-babel-tangle-comment-links "ob-tangle" (&optional info))
(declare-function org-completing-read "org" (&rest args))
(declare-function org-current-level "org" ())
(declare-function org-cycle "org" (&optional arg))
(declare-function org-do-remove-indentation "org" (&optional n))
(declare-function org-edit-src-code "org-src" (&optional code edit-buffer-name))
(declare-function org-edit-src-exit "org-src"  ())
(declare-function org-element-at-point "org-element" ())
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-normalize-string "org-element" (s))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-escape-code-in-region "org-src" (beg end))
(declare-function org-get-indentation "org" (&optional line))
(declare-function org-get-indentation "org" (&optional line))
(declare-function org-in-regexp "org" (regexp &optional nlines visually))
(declare-function org-indent-line "org" ())
(declare-function org-list-get-list-end "org-list" (item struct prevs))
(declare-function org-list-prevs-alist "org-list" (struct))
(declare-function org-list-struct "org-list" ())
(declare-function org-list-to-generic "org-list" (LIST PARAMS))
(declare-function org-list-to-lisp "org-list" (&optional delete))
(declare-function org-macro-escape-arguments "org-macro" (&rest args))
(declare-function org-make-options-regexp "org" (kwds &optional extra))
(declare-function org-mark-ring-push "org" (&optional pos buffer))
(declare-function org-narrow-to-subtree "org" ())
(declare-function org-next-block "org" (arg &optional backward block-regexp))
(declare-function org-number-sequence "org-compat" (from &optional to inc))
(declare-function org-open-at-point "org" (&optional in-emacs reference-buffer))
(declare-function org-outline-overlay-data "org" (&optional use-markers))
(declare-function org-previous-block "org" (arg &optional block-regexp))
(declare-function org-remove-indentation "org" (code &optional n))
(declare-function org-reverse-string "org" (string))
(declare-function org-set-outline-overlay-data "org" (data))
(declare-function org-show-context "org" (&optional key))
(declare-function org-src-coderef-format "org-src" (element))
(declare-function org-src-coderef-regexp "org-src" (fmt &optional label))
(declare-function org-table-align "org-table" ())
(declare-function org-table-end "org-table" (&optional table-type))
(declare-function org-table-import "org-table" (file arg))
(declare-function org-table-to-lisp "org-table" (&optional txt))
(declare-function org-trim "org" (s &optional keep-lead))
(declare-function org-unescape-code-in-string "org-src" (s))
(declare-function org-uniquify "org" (list))
(declare-function orgtbl-to-generic "org-table" (table params))
(declare-function orgtbl-to-orgtbl "org-table" (table params))
(declare-function outline-show-all "outline" ())
(declare-function tramp-compat-make-temp-file "tramp-compat" (filename &optional dir-flag))

(defgroup org-babel nil
  "Code block evaluation and management in `org-mode' documents."
  :tag "Babel"
  :group 'org)

(defcustom org-confirm-babel-evaluate t
  "Confirm before evaluation.
\\<org-mode-map>\
Require confirmation before interactively evaluating code
blocks in Org buffers.  The default value of this variable is t,
meaning confirmation is required for any code block evaluation.
This variable can be set to nil to inhibit any future
confirmation requests.  This variable can also be set to a
function which takes two arguments the language of the code block
and the body of the code block.  Such a function should then
return a non-nil value if the user should be prompted for
execution or nil if no prompt is required.

Warning: Disabling confirmation may result in accidental
evaluation of potentially harmful code.  It may be advisable
remove code block execution from `\\[org-ctrl-c-ctrl-c]' \
as further protection
against accidental code block evaluation.  The
`org-babel-no-eval-on-ctrl-c-ctrl-c' variable can be used to
remove code block execution from the `\\[org-ctrl-c-ctrl-c]' keybinding."
  :group 'org-babel
  :version "24.1"
  :type '(choice boolean function))
;; don't allow this variable to be changed through file settings
(put 'org-confirm-babel-evaluate 'safe-local-variable (lambda (x) (eq x t)))

(defcustom org-babel-no-eval-on-ctrl-c-ctrl-c nil
  "\\<org-mode-map>\
Remove code block evaluation from the `\\[org-ctrl-c-ctrl-c]' key binding."
  :group 'org-babel
  :version "24.1"
  :type 'boolean)

(defcustom org-babel-results-keyword "RESULTS"
  "Keyword used to name results generated by code blocks.
It should be \"RESULTS\".  However any capitalization may be
used."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string
  :safe (lambda (v)
	  (and (stringp v)
	       (eq (compare-strings "RESULTS" nil nil v nil nil t)
		   t))))

(defcustom org-babel-noweb-wrap-start "<<"
  "String used to begin a noweb reference in a code block.
See also `org-babel-noweb-wrap-end'."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-noweb-wrap-end ">>"
  "String used to end a noweb reference in a code block.
See also `org-babel-noweb-wrap-start'."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-inline-result-wrap "=%s="
  "Format string used to wrap inline results.
This string must include a \"%s\" which will be replaced by the results."
  :group 'org-babel
  :type 'string)
(put 'org-babel-inline-result-wrap
     'safe-local-variable
     (lambda (value)
       (and (stringp value)
	    (string-match-p "%s" value))))

(defcustom org-babel-hash-show-time nil
  "Non-nil means show the time the code block was evaluated in the result hash."
  :group 'org-babel
  :type 'boolean
  :version "26.1"
  :package-version '(Org . "9.0")
  :safe #'booleanp)

(defcustom org-babel-uppercase-example-markers nil
  "When non-nil, begin/end example markers will be inserted in upper case."
  :group 'org-babel
  :type 'boolean
  :version "26.1"
  :package-version '(Org . "9.1")
  :safe #'booleanp)

(defun org-babel-noweb-wrap (&optional regexp)
  (concat org-babel-noweb-wrap-start
	  (or regexp "\\([^ \t\n].+?[^ \t]\\|[^ \t\n]\\)")
	  org-babel-noweb-wrap-end))

(defvar org-babel-src-name-regexp
  "^[ \t]*#\\+name:[ \t]*"
  "Regular expression used to match a source name line.")

(defvar org-babel-multi-line-header-regexp
  "^[ \t]*#\\+headers?:[ \t]*\\([^\n]*\\)$"
  "Regular expression used to match multi-line header arguments.")

(defvar org-babel-src-block-regexp
  (concat
   ;; (1) indentation                 (2) lang
   "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
   ;; (3) switches
   "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
   ;; (4) header arguments
   "\\([^\n]*\\)\n"
   ;; (5) body
   "\\([^\000]*?\n\\)??[ \t]*#\\+end_src")
  "Regexp used to identify code blocks.")

(defun org-babel--get-vars (params)
  "Return the babel variable assignments in PARAMS.

PARAMS is a quasi-alist of header args, which may contain
multiple entries for the key `:var'.  This function returns a
list of the cdr of all the `:var' entries."
  (mapcar #'cdr
	  (cl-remove-if-not (lambda (x) (eq (car x) :var)) params)))

(defvar org-babel-exp-reference-buffer nil
  "Buffer containing original contents of the exported buffer.
This is used by Babel to resolve references in source blocks.
Its value is dynamically bound during export.")

(defun org-babel-check-confirm-evaluate (info)
  "Check whether INFO allows code block evaluation.

Returns nil if evaluation is disallowed, t if it is
unconditionally allowed, and the symbol `query' if the user
should be asked whether to allow evaluation."
  (let* ((headers (nth 2 info))
	 (eval (or (cdr  (assq :eval headers))
		   (when (assq :noeval headers) "no")))
	 (eval-no (member eval '("no" "never")))
	 (export org-babel-exp-reference-buffer)
	 (eval-no-export (and export (member eval '("no-export" "never-export"))))
	 (noeval (or eval-no eval-no-export))
	 (query (or (equal eval "query")
		    (and export (equal eval "query-export"))
		    (if (functionp org-confirm-babel-evaluate)
			(funcall org-confirm-babel-evaluate
				 ;; Language, code block body.
				 (nth 0 info) (nth 1 info))
		      org-confirm-babel-evaluate))))
    (cond
     (noeval nil)
     (query 'query)
     (t t))))

(defun org-babel-check-evaluate (info)
  "Check if code block INFO should be evaluated.
Do not query the user, but do display an informative message if
evaluation is blocked.  Returns non-nil if evaluation is not blocked."
  (let ((confirmed (org-babel-check-confirm-evaluate info)))
    (unless confirmed
      (message "Evaluation of this %s code block%sis disabled."
	       (nth 0 info)
	       (let ((name (nth 4 info)))
		 (if name (format " (%s) " name) " "))))
    confirmed))

;; Dynamically scoped for asynchronous export.
(defvar org-babel-confirm-evaluate-answer-no)

(defun org-babel-confirm-evaluate (info)
  "Confirm evaluation of the code block INFO.

This query can also be suppressed by setting the value of
`org-confirm-babel-evaluate' to nil, in which case all future
interactive code block evaluations will proceed without any
confirmation from the user.

Note disabling confirmation may result in accidental evaluation
of potentially harmful code.

The variable `org-babel-confirm-evaluate-answer-no' is used by
the async export process, which requires a non-interactive
environment, to override this check."
  (let* ((evalp (org-babel-check-confirm-evaluate info))
	 (lang (nth 0 info))
	 (name (nth 4 info))
	 (name-string (if name (format " (%s) " name) " ")))
    (pcase evalp
      (`nil nil)
      (`t t)
      (`query (or
	       (and (not (bound-and-true-p
			  org-babel-confirm-evaluate-answer-no))
		    (yes-or-no-p
		     (format "Evaluate this %s code block%son your system? "
			     lang name-string)))
	       (progn
		(message "Evaluation of this %s code block%sis aborted."
			 lang name-string)
		nil)))
      (x (error "Unexpected value `%s' from `org-babel-check-confirm-evaluate'" x)))))

;;;###autoload
(defun org-babel-execute-safely-maybe ()
  (unless org-babel-no-eval-on-ctrl-c-ctrl-c
    (org-babel-execute-maybe)))

;;;###autoload
(defun org-babel-execute-maybe ()
  (interactive)
  (or (org-babel-execute-src-block-maybe)
      (org-babel-lob-execute-maybe)))

(defmacro org-babel-when-in-src-block (&rest body)
  "Execute BODY if point is in a source block and return t.

Otherwise do nothing and return nil."
  `(if (memq (org-element-type (org-element-context))
	     '(inline-src-block src-block))
       (progn
	 ,@body
	 t)
     nil))

(defun org-babel-execute-src-block-maybe ()
  "Conditionally execute a source block.
Detect if this is context for a Babel src-block and if so
then run `org-babel-execute-src-block'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-eval-wipe-error-buffer)
   (org-babel-execute-src-block current-prefix-arg)))

;;;###autoload
(defun org-babel-view-src-block-info ()
  "Display information on the current source block.
This includes header arguments, language and name, and is largely
a window into the `org-babel-get-src-block-info' function."
  (interactive)
  (let ((info (org-babel-get-src-block-info 'light))
	(full (lambda (it) (> (length it) 0)))
	(printf (lambda (fmt &rest args) (princ (apply #'format fmt args)))))
    (when info
      (with-help-window (help-buffer)
	(let ((name        (nth 4 info))
	      (lang        (nth 0 info))
	      (switches    (nth 3 info))
	      (header-args (nth 2 info)))
	  (when name            (funcall printf "Name: %s\n"     name))
	  (when lang            (funcall printf "Lang: %s\n"     lang))
	  (funcall printf "Properties:\n")
	  (funcall printf "\t:header-args \t%s\n" (org-entry-get (point) "header-args" t))
	  (funcall printf "\t:header-args:%s \t%s\n" lang (org-entry-get (point) (concat "header-args:" lang) t))

	  (when (funcall full switches) (funcall printf "Switches: %s\n" switches))
	  (funcall printf "Header Arguments:\n")
	  (dolist (pair (sort header-args
			      (lambda (a b) (string< (symbol-name (car a))
						     (symbol-name (car b))))))
	    (when (funcall full (format "%s" (cdr pair)))
	      (funcall printf "\t%S%s\t%s\n"
		       (car pair)
		       (if (> (length (format "%S" (car pair))) 7) "" "\t")
		       (cdr pair)))))))))

;;;###autoload
(defun org-babel-expand-src-block-maybe ()
  "Conditionally expand a source block.
Detect if this is context for a org-babel src-block and if so
then run `org-babel-expand-src-block'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-expand-src-block current-prefix-arg)))

;;;###autoload
(defun org-babel-load-in-session-maybe ()
  "Conditionally load a source block in a session.
Detect if this is context for a org-babel src-block and if so
then run `org-babel-load-in-session'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-load-in-session current-prefix-arg)))

(add-hook 'org-metaup-hook 'org-babel-load-in-session-maybe)

;;;###autoload
(defun org-babel-pop-to-session-maybe ()
  "Conditionally pop to a session.
Detect if this is context for a org-babel src-block and if so
then run `org-babel-switch-to-session'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-switch-to-session current-prefix-arg)))

(add-hook 'org-metadown-hook 'org-babel-pop-to-session-maybe)

(defconst org-babel-common-header-args-w-values
  '((cache	. ((no yes)))
    (cmdline	. :any)
    (colnames	. ((nil no yes)))
    (comments	. ((no link yes org both noweb)))
    (dir	. :any)
    (eval	. ((yes no no-export strip-export never-export eval never
			query)))
    (exports	. ((code results both none)))
    (epilogue   . :any)
    (file	. :any)
    (file-desc  . :any)
    (file-ext   . :any)
    (hlines	. ((no yes)))
    (mkdirp	. ((yes no)))
    (no-expand)
    (noeval)
    (noweb	. ((yes no tangle no-export strip-export)))
    (noweb-ref	. :any)
    (noweb-sep  . :any)
    (output-dir . :any)
    (padline	. ((yes no)))
    (post       . :any)
    (prologue   . :any)
    (results	. ((file list vector table scalar verbatim)
		   (raw html latex org code pp drawer)
		   (replace silent none append prepend)
		   (output value)))
    (rownames	. ((no yes)))
    (sep	. :any)
    (session	. :any)
    (shebang	. :any)
    (tangle	. ((tangle yes no :any)))
    (tangle-mode . ((#o755 #o555 #o444 :any)))
    (var	. :any)
    (wrap       . :any)))

(defconst org-babel-header-arg-names
  (mapcar #'car org-babel-common-header-args-w-values)
  "Common header arguments used by org-babel.
Note that individual languages may define their own language
specific header arguments as well.")

(defconst org-babel-safe-header-args
  '(:cache :colnames :comments :exports :epilogue :hlines :noeval
	   :noweb :noweb-ref :noweb-sep :padline :prologue :rownames
	   :sep :session :tangle :wrap
	   (:eval . ("never" "query"))
	   (:results . (lambda (str) (not (string-match "file" str)))))
  "A list of safe header arguments for babel source blocks.

The list can have entries of the following forms:
- :ARG                     -> :ARG is always a safe header arg
- (:ARG . (VAL1 VAL2 ...)) -> :ARG is safe as a header arg if it is
                              `equal' to one of the VALs.
- (:ARG . FN)              -> :ARG is safe as a header arg if the function FN
                              returns non-nil.  FN is passed one
                              argument, the value of the header arg
                              (as a string).")

(defmacro org-babel-header-args-safe-fn (safe-list)
  "Return a function that determines whether a list of header args are safe.

Intended usage is:
\(put \\='org-babel-default-header-args \\='safe-local-variable
 (org-babel-header-args-safe-p org-babel-safe-header-args)

This allows org-babel languages to extend the list of safe values for
their `org-babel-default-header-args:foo' variable.

For the format of SAFE-LIST, see `org-babel-safe-header-args'."
  `(lambda (value)
     (and (listp value)
	  (cl-every
	   (lambda (pair)
	     (and (consp pair)
		  (org-babel-one-header-arg-safe-p pair ,safe-list)))
	   value))))

(defvar org-babel-default-header-args
  '((:session . "none") (:results . "replace") (:exports . "code")
    (:cache . "no") (:noweb . "no") (:hlines . "no") (:tangle . "no"))
  "Default arguments to use when evaluating a source block.")
(put 'org-babel-default-header-args 'safe-local-variable
     (org-babel-header-args-safe-fn org-babel-safe-header-args))

(defvar org-babel-default-inline-header-args
  '((:session . "none") (:results . "replace")
    (:exports . "results") (:hlines . "yes"))
  "Default arguments to use when evaluating an inline source block.")
(put 'org-babel-default-inline-header-args 'safe-local-variable
     (org-babel-header-args-safe-fn org-babel-safe-header-args))

(defconst org-babel-name-regexp
  (format "^[ \t]*#\\+%s:[ \t]*"
	  ;; FIXME: TBLNAME is for backward compatibility.
	  (regexp-opt '("NAME" "TBLNAME")))
  "Regexp matching a NAME keyword.")

(defconst org-babel-result-regexp
  (format "^[ \t]*#\\+%s\\(?:\\[\\(?:%s \\)?\\([[:alnum:]]+\\)\\]\\)?:[ \t]*"
	  org-babel-results-keyword
	  ;; <%Y-%m-%d %H:%M:%S>
	  "<\\(?:[0-9]\\{4\\}-[0-1][0-9]-[0-3][0-9] \
[0-2][0-9]\\(?::[0-5][0-9]\\)\\{2\\}\\)>")
  "Regular expression used to match result lines.
If the results are associated with a hash key then the hash will
be saved in match group 1.")

(defconst org-babel-result-w-name-regexp
  (concat org-babel-result-regexp "\\(?9:[^ \t\n\r\v\f]+\\)")
  "Regexp matching a RESULTS keyword with a name.
Name is saved in match group 9.")

(defvar org-babel-min-lines-for-block-output 10
  "The minimum number of lines for block output.
If number of lines of output is equal to or exceeds this
value, the output is placed in a #+begin_example...#+end_example
block.  Otherwise the output is marked as literal by inserting
colons at the starts of the lines.  This variable only takes
effect if the :results output option is in effect.")

(defvar org-babel-noweb-error-all-langs nil
  "Raise errors when noweb references don't resolve.
Also see `org-babel-noweb-error-langs' to control noweb errors on
a language by language bases.")

(defvar org-babel-noweb-error-langs nil
  "Languages for which Babel will raise literate programming errors.
List of languages for which errors should be raised when the
source code block satisfying a noweb reference in this language
can not be resolved.  Also see `org-babel-noweb-error-all-langs'
to raise errors for all languages.")

(defvar org-babel-hash-show 4
  "Number of initial characters to show of a hidden results hash.")

(defvar org-babel-after-execute-hook nil
  "Hook for functions to be called after `org-babel-execute-src-block'")

(defun org-babel-named-src-block-regexp-for-name (&optional name)
  "This generates a regexp used to match a src block named NAME.
If NAME is nil, match any name.  Matched name is then put in
match group 9.  Other match groups are defined in
`org-babel-src-block-regexp'."
  (concat org-babel-src-name-regexp
	  (concat (if name (regexp-quote name) "\\(?9:.*?\\)") "[ \t]*" )
	  "\\(?:\n[ \t]*#\\+\\S-+:.*\\)*?"
	  "\n"
	  (substring org-babel-src-block-regexp 1)))

(defun org-babel-named-data-regexp-for-name (name)
  "This generates a regexp used to match data named NAME."
  (concat org-babel-name-regexp (regexp-quote name) "[ \t]*$"))

(defun org-babel--normalize-body (datum)
  "Normalize body for element or object DATUM.
DATUM is a source block element or an inline source block object.
Remove final newline character and spurious indentation."
  (let* ((value (org-element-property :value datum))
	 (body (if (string-suffix-p "\n" value)
		   (substring value 0 -1)
		 value)))
    (cond ((eq (org-element-type datum) 'inline-src-block)
	   ;; Newline characters and indentation in an inline
	   ;; src-block are not meaningful, since they could come from
	   ;; some paragraph filling.  Treat them as a white space.
	   (replace-regexp-in-string "\n[ \t]*" " " body))
	  ((or org-src-preserve-indentation
	       (org-element-property :preserve-indent datum))
	   body)
	  (t (org-remove-indentation body)))))

;;; functions
(defvar org-babel-current-src-block-location nil
  "Marker pointing to the src block currently being executed.
This may also point to a call line or an inline code block.  If
multiple blocks are being executed (e.g., in chained execution
through use of the :var header argument) this marker points to
the outer-most code block.")

(defvar *this*)

(defun org-babel-get-src-block-info (&optional light datum)
  "Extract information from a source block or inline source block.

Optional argument LIGHT does not resolve remote variable
references; a process which could likely result in the execution
of other code blocks.

By default, consider the block at point.  However, when optional
argument DATUM is provided, extract information from that parsed
object instead.

Return nil if point is not on a source block.  Otherwise, return
a list with the following pattern:

  (language body arguments switches name start coderef)"
  (let* ((datum (or datum (org-element-context)))
	 (type (org-element-type datum))
	 (inline (eq type 'inline-src-block)))
    (when (memq type '(inline-src-block src-block))
      (let* ((lang (org-element-property :language datum))
	     (lang-headers (intern
			    (concat "org-babel-default-header-args:" lang)))
	     (name (org-element-property :name datum))
	     (info
	      (list
	       lang
	       (org-babel--normalize-body datum)
	       (apply #'org-babel-merge-params
		      (if inline org-babel-default-inline-header-args
			org-babel-default-header-args)
		      (and (boundp lang-headers) (eval lang-headers t))
		      (append
		       ;; If DATUM is provided, make sure we get node
		       ;; properties applicable to its location within
		       ;; the document.
		       (org-with-point-at (org-element-property :begin datum)
			 (org-babel-params-from-properties lang))
		       (mapcar #'org-babel-parse-header-arguments
			       (cons (org-element-property :parameters datum)
				     (org-element-property :header datum)))))
	       (or (org-element-property :switches datum) "")
	       name
	       (org-element-property (if inline :begin :post-affiliated)
				     datum)
	       (and (not inline) (org-src-coderef-format datum)))))
	(unless light
	  (setf (nth 2 info) (org-babel-process-params (nth 2 info))))
	(setf (nth 2 info) (org-babel-generate-file-param name (nth 2 info)))
	info))))

;;;###autoload
(defun org-babel-execute-src-block (&optional arg info params)
  "Execute the current source code block.
Insert the results of execution into the buffer.  Source code
execution and the collection and formatting of results can be
controlled through a variety of header arguments.

With prefix argument ARG, force re-execution even if an existing
result cached in the buffer would otherwise have been returned.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block."
  (interactive)
  (let* ((org-babel-current-src-block-location
	  (or org-babel-current-src-block-location
	      (nth 5 info)
	      (org-babel-where-is-src-block-head)))
	 (info (if info (copy-tree info) (org-babel-get-src-block-info))))
    ;; Merge PARAMS with INFO before considering source block
    ;; evaluation since both could disagree.
    (cl-callf org-babel-merge-params (nth 2 info) params)
    (when (org-babel-check-evaluate info)
      (cl-callf org-babel-process-params (nth 2 info))
      (let* ((params (nth 2 info))
	     (cache (let ((c (cdr (assq :cache params))))
		      (and (not arg) c (string= "yes" c))))
	     (new-hash (and cache (org-babel-sha1-hash info)))
	     (old-hash (and cache (org-babel-current-result-hash)))
	     (current-cache (and new-hash (equal new-hash old-hash))))
	(cond
	 (current-cache
	  (save-excursion		;Return cached result.
	    (goto-char (org-babel-where-is-src-block-result nil info))
	    (forward-line)
	    (skip-chars-forward " \t")
	    (let ((result (org-babel-read-result)))
	      (message (replace-regexp-in-string "%" "%%" (format "%S" result)))
	      result)))
	 ((org-babel-confirm-evaluate info)
	  (let* ((lang (nth 0 info))
		 (result-params (cdr (assq :result-params params)))
		 ;; Expand noweb references in BODY and remove any
		 ;; coderef.
		 (body
		  (let ((coderef (nth 6 info))
			(expand
			 (if (org-babel-noweb-p params :eval)
			     (org-babel-expand-noweb-references info)
			   (nth 1 info))))
		    (if (not coderef) expand
		      (replace-regexp-in-string
		       (org-src-coderef-regexp coderef) "" expand nil nil 1))))
		 (dir (cdr (assq :dir params)))
		 (default-directory
		   (or (and dir (file-name-as-directory (expand-file-name dir)))
		       default-directory))
		 (cmd (intern (concat "org-babel-execute:" lang)))
		 result)
	    (unless (fboundp cmd)
	      (error "No org-babel-execute function for %s!" lang))
	    (message "executing %s code block%s..."
		     (capitalize lang)
		     (let ((name (nth 4 info)))
		       (if name (format " (%s)" name) "")))
	    (if (member "none" result-params)
		(progn (funcall cmd body params)
		       (message "result silenced"))
	      (setq result
		    (let ((r (funcall cmd body params)))
		      (if (and (eq (cdr (assq :result-type params)) 'value)
			       (or (member "vector" result-params)
				   (member "table" result-params))
			       (not (listp r)))
			  (list (list r))
			r)))
	      (let ((file (cdr (assq :file params))))
		;; If non-empty result and :file then write to :file.
		(when file
		  (when result
		    (with-temp-file file
		      (insert (org-babel-format-result
			       result (cdr (assq :sep params))))))
		  (setq result file))
		;; Possibly perform post process provided its
		;; appropriate.  Dynamically bind "*this*" to the
		;; actual results of the block.
		(let ((post (cdr (assq :post params))))
		  (when post
		    (let ((*this* (if (not file) result
				    (org-babel-result-to-file
				     file
				     (let ((desc (assq :file-desc params)))
				       (and desc (or (cdr desc) result)))))))
		      (setq result (org-babel-ref-resolve post))
		      (when file
			(setq result-params (remove "file" result-params))))))
		(org-babel-insert-result
		 result result-params info new-hash lang)))
	    (run-hooks 'org-babel-after-execute-hook)
	    result)))))))

(defun org-babel-expand-body:generic (body params &optional var-lines)
  "Expand BODY with PARAMS.
Expand a block of code with org-babel according to its header
arguments.  This generic implementation of body expansion is
called for languages which have not defined their own specific
org-babel-expand-body:lang function."
  (let ((pro (cdr (assq :prologue params)))
	(epi (cdr (assq :epilogue params))))
    (mapconcat #'identity
	       (append (when pro (list pro))
		       var-lines
		       (list body)
		       (when epi (list epi)))
	       "\n")))

;;;###autoload
(defun org-babel-expand-src-block (&optional _arg info params)
  "Expand the current source code block.
Expand according to the source code block's header
arguments and pop open the results in a preview buffer."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (nth 0 info))
	 (params (setf (nth 2 info)
                       (sort (org-babel-merge-params (nth 2 info) params)
                             (lambda (el1 el2) (string< (symbol-name (car el1))
							(symbol-name (car el2)))))))
         (body (setf (nth 1 info)
		     (if (org-babel-noweb-p params :eval)
			 (org-babel-expand-noweb-references info) (nth 1 info))))
         (expand-cmd (intern (concat "org-babel-expand-body:" lang)))
	 (assignments-cmd (intern (concat "org-babel-variable-assignments:"
					  lang)))
         (expanded
	  (if (fboundp expand-cmd) (funcall expand-cmd body params)
	    (org-babel-expand-body:generic
	     body params (and (fboundp assignments-cmd)
			      (funcall assignments-cmd params))))))
    (if (called-interactively-p 'any)
	(org-edit-src-code
	 expanded (concat "*Org-Babel Preview " (buffer-name) "[ " lang " ]*"))
      expanded)))

(defun org-babel-edit-distance (s1 s2)
  "Return the edit (levenshtein) distance between strings S1 S2."
  (let* ((l1 (length s1))
	 (l2 (length s2))
	 (dist (vconcat (mapcar (lambda (_) (make-vector (1+ l2) nil))
				(number-sequence 1 (1+ l1)))))
	 (in (lambda (i j) (aref (aref dist i) j))))
    (setf (aref (aref dist 0) 0) 0)
    (dolist (j (number-sequence 1 l2))
      (setf (aref (aref dist 0) j) j))
    (dolist (i (number-sequence 1 l1))
      (setf (aref (aref dist i) 0) i)
      (dolist (j (number-sequence 1 l2))
	(setf (aref (aref dist i) j)
	      (min
	       (1+ (funcall in (1- i) j))
	       (1+ (funcall in i (1- j)))
	       (+ (if (equal (aref s1 (1- i)) (aref s2 (1- j))) 0 1)
		  (funcall in (1- i) (1- j)))))))
    (funcall in l1 l2)))

(defun org-babel-combine-header-arg-lists (original &rest others)
  "Combine a number of lists of header argument names and arguments."
  (let ((results (copy-sequence original)))
    (dolist (new-list others)
      (dolist (arg-pair new-list)
	(let ((header (car arg-pair)))
	  (setq results
		(cons arg-pair (cl-remove-if
				(lambda (pair) (equal header (car pair)))
				results))))))
    results))

;;;###autoload
(defun org-babel-check-src-block ()
  "Check for misspelled header arguments in the current code block."
  (interactive)
  ;; TODO: report malformed code block
  ;; TODO: report incompatible combinations of header arguments
  ;; TODO: report uninitialized variables
  (let ((too-close 2) ;; <- control closeness to report potential match
	(names (mapcar #'symbol-name org-babel-header-arg-names)))
    (dolist (header (mapcar (lambda (arg) (substring (symbol-name (car arg)) 1))
			    (and (org-babel-where-is-src-block-head)
				 (org-babel-parse-header-arguments
				  (org-no-properties
				   (match-string 4))))))
      (dolist (name names)
	(when (and (not (string= header name))
		   (<= (org-babel-edit-distance header name) too-close)
		   (not (member header names)))
	  (error "Supplied header \"%S\" is suspiciously close to \"%S\""
		 header name))))
    (message "No suspicious header arguments found.")))

;;;###autoload
(defun org-babel-insert-header-arg (&optional header-arg value)
  "Insert a header argument selecting from lists of common args and values."
  (interactive)
  (let* ((info (org-babel-get-src-block-info 'light))
	 (lang (car info))
	 (begin (nth 5 info))
	 (lang-headers (intern (concat "org-babel-header-args:" lang)))
	 (headers (org-babel-combine-header-arg-lists
		   org-babel-common-header-args-w-values
		   (when (boundp lang-headers) (eval lang-headers t))))
	 (header-arg (or header-arg
			 (completing-read
			  "Header Arg: "
			  (mapcar
			   (lambda (header-spec) (symbol-name (car header-spec)))
			   headers))))
	 (vals (cdr (assoc (intern header-arg) headers)))
	 (value (or value
		    (cond
		     ((eq vals :any)
		      (read-from-minibuffer "value: "))
		     ((listp vals)
		      (mapconcat
		       (lambda (group)
			 (let ((arg (completing-read
				     "Value: "
				     (cons "default"
					   (mapcar #'symbol-name group)))))
			   (if (and arg (not (string= "default" arg)))
			       (concat arg " ")
			     "")))
		       vals ""))))))
    (save-excursion
      (goto-char begin)
      (goto-char (point-at-eol))
      (unless (= (char-before (point)) ?\ ) (insert " "))
      (insert ":" header-arg) (when value (insert " " value)))))

;; Add support for completing-read insertion of header arguments after ":"
(defun org-babel-header-arg-expand ()
  "Call `org-babel-enter-header-arg-w-completion' in appropriate contexts."
  (when (and (equal (char-before) ?\:) (org-babel-where-is-src-block-head))
    (org-babel-enter-header-arg-w-completion (match-string 2))))

(defun org-babel-enter-header-arg-w-completion (&optional lang)
  "Insert header argument appropriate for LANG with completion."
  (let* ((lang-headers-var (intern (concat "org-babel-header-args:" lang)))
         (lang-headers (when (boundp lang-headers-var) (eval lang-headers-var t)))
	 (headers-w-values (org-babel-combine-header-arg-lists
			    org-babel-common-header-args-w-values lang-headers))
         (headers (mapcar #'symbol-name (mapcar #'car headers-w-values)))
         (header (org-completing-read "Header Arg: " headers))
         (args (cdr (assoc (intern header) headers-w-values)))
         (arg (when (and args (listp args))
                (org-completing-read
                 (format "%s: " header)
                 (mapcar #'symbol-name (apply #'append args))))))
    (insert (concat header " " (or arg "")))
    (cons header arg)))

(add-hook 'org-tab-first-hook 'org-babel-header-arg-expand)

;;;###autoload
(defun org-babel-load-in-session (&optional _arg info)
  "Load the body of the current source-code block.
Evaluate the header arguments for the source block before
entering the session.  After loading the body this pops open the
session."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (nth 0 info))
         (params (nth 2 info))
         (body (if (not info)
		   (user-error "No src code block at point")
		 (setf (nth 1 info)
		       (if (org-babel-noweb-p params :eval)
			   (org-babel-expand-noweb-references info)
			 (nth 1 info)))))
         (session (cdr (assq :session params)))
	 (dir (cdr (assq :dir params)))
	 (default-directory
	   (or (and dir (file-name-as-directory dir)) default-directory))
	 (cmd (intern (concat "org-babel-load-session:" lang))))
    (unless (fboundp cmd)
      (error "No org-babel-load-session function for %s!" lang))
    (pop-to-buffer (funcall cmd session body params))
    (end-of-line 1)))

;;;###autoload
(defun org-babel-initiate-session (&optional arg info)
  "Initiate session for current code block.
If called with a prefix argument then resolve any variable
references in the header arguments and assign these variables in
the session.  Copy the body of the code block to the kill ring."
  (interactive "P")
  (let* ((info (or info (org-babel-get-src-block-info (not arg))))
         (lang (nth 0 info))
         (body (nth 1 info))
         (params (nth 2 info))
         (session (cdr (assq :session params)))
	 (dir (cdr (assq :dir params)))
	 (default-directory
	   (or (and dir (file-name-as-directory dir)) default-directory))
	 (init-cmd (intern (format "org-babel-%s-initiate-session" lang)))
	 (prep-cmd (intern (concat "org-babel-prep-session:" lang))))
    (when (and (stringp session) (string= session "none"))
      (error "This block is not using a session!"))
    (unless (fboundp init-cmd)
      (error "No org-babel-initiate-session function for %s!" lang))
    (with-temp-buffer (insert (org-trim body))
                      (copy-region-as-kill (point-min) (point-max)))
    (when arg
      (unless (fboundp prep-cmd)
	(error "No org-babel-prep-session function for %s!" lang))
      (funcall prep-cmd session params))
    (funcall init-cmd session params)))

;;;###autoload
(defun org-babel-switch-to-session (&optional arg info)
  "Switch to the session of the current code block.
Uses `org-babel-initiate-session' to start the session.  If called
with a prefix argument then this is passed on to
`org-babel-initiate-session'."
  (interactive "P")
  (pop-to-buffer (org-babel-initiate-session arg info))
  (end-of-line 1))

(defalias 'org-babel-pop-to-session 'org-babel-switch-to-session)

(defvar org-src-window-setup)

;;;###autoload
(defun org-babel-switch-to-session-with-code (&optional arg _info)
  "Switch to code buffer and display session."
  (interactive "P")
  (let ((swap-windows
	 (lambda ()
	   (let ((other-window-buffer (window-buffer (next-window))))
	     (set-window-buffer (next-window) (current-buffer))
	     (set-window-buffer (selected-window) other-window-buffer))
	   (other-window 1)))
	(info (org-babel-get-src-block-info))
	(org-src-window-setup 'reorganize-frame))
    (save-excursion
      (org-babel-switch-to-session arg info))
    (org-edit-src-code)
    (funcall swap-windows)))

;;;###autoload
(defmacro org-babel-do-in-edit-buffer (&rest body)
  "Evaluate BODY in edit buffer if there is a code block at point.
Return t if a code block was found at point, nil otherwise."
  `(let ((org-src-window-setup 'switch-invisibly))
     (when (and (org-babel-where-is-src-block-head)
		(org-edit-src-code))
       (unwind-protect (progn ,@body)
	 (org-edit-src-exit))
       t)))
(def-edebug-spec org-babel-do-in-edit-buffer (body))

(defun org-babel-do-key-sequence-in-edit-buffer (key)
  "Read key sequence and execute the command in edit buffer.
Enter a key sequence to be executed in the language major-mode
edit buffer.  For example, TAB will alter the contents of the
Org code block according to the effect of TAB in the language
major mode buffer.  For languages that support interactive
sessions, this can be used to send code from the Org buffer
to the session for evaluation using the native major mode
evaluation mechanisms."
  (interactive "kEnter key-sequence to execute in edit buffer: ")
  (org-babel-do-in-edit-buffer
   (call-interactively
    (key-binding (or key (read-key-sequence nil))))))

(defvar org-bracket-link-regexp)

(defun org-babel-active-location-p ()
  (memq (org-element-type (save-match-data (org-element-context)))
	'(babel-call inline-babel-call inline-src-block src-block)))

;;;###autoload
(defun org-babel-open-src-block-result (&optional re-run)
  "If `point' is on a src block then open the results of the
source code block, otherwise return nil.  With optional prefix
argument RE-RUN the source-code block is evaluated even if
results already exist."
  (interactive "P")
  (let ((info (org-babel-get-src-block-info 'light)))
    (when info
      (save-excursion
	;; go to the results, if there aren't any then run the block
	(goto-char (or (and (not re-run) (org-babel-where-is-src-block-result))
		       (progn (org-babel-execute-src-block)
			      (org-babel-where-is-src-block-result))))
	(end-of-line 1)
	(while (looking-at "[\n\r\t\f ]") (forward-char 1))
	;; open the results
	(if (looking-at org-bracket-link-regexp)
	    ;; file results
	    (org-open-at-point)
	  (let ((r (org-babel-format-result
		    (org-babel-read-result) (cdr (assq :sep (nth 2 info))))))
	    (pop-to-buffer (get-buffer-create "*Org-Babel Results*"))
	    (delete-region (point-min) (point-max))
	    (insert r)))
	t))))

;;;###autoload
(defmacro org-babel-map-src-blocks (file &rest body)
  "Evaluate BODY forms on each source-block in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer.  During evaluation of BODY the following local variables
are set relative to the currently matched code block.

full-block ------- string holding the entirety of the code block
beg-block -------- point at the beginning of the code block
end-block -------- point at the end of the matched code block
lang ------------- string holding the language of the code block
beg-lang --------- point at the beginning of the lang
end-lang --------- point at the end of the lang
switches --------- string holding the switches
beg-switches ----- point at the beginning of the switches
end-switches ----- point at the end of the switches
header-args ------ string holding the header-args
beg-header-args -- point at the beginning of the header-args
end-header-args -- point at the end of the header-args
body ------------- string holding the body of the code block
beg-body --------- point at the beginning of the body
end-body --------- point at the end of the body"
  (declare (indent 1))
  (let ((tempvar (make-symbol "file")))
    `(let* ((case-fold-search t)
	    (,tempvar ,file)
	    (visited-p (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (point (point)) to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward org-babel-src-block-regexp nil t)
	   (when (org-babel-active-location-p)
	     (goto-char (match-beginning 0))
	     (let ((full-block (match-string 0))
		   (beg-block (match-beginning 0))
		   (end-block (match-end 0))
		   (lang (match-string 2))
		   (beg-lang (match-beginning 2))
		   (end-lang (match-end 2))
		   (switches (match-string 3))
		   (beg-switches (match-beginning 3))
		   (end-switches (match-end 3))
		   (header-args (match-string 4))
		   (beg-header-args (match-beginning 4))
		   (end-header-args (match-end 4))
		   (body (match-string 5))
		   (beg-body (match-beginning 5))
		   (end-body (match-end 5)))
               ;; Silence byte-compiler in case `body' doesn't use all
               ;; those variables.
               (ignore full-block beg-block end-block lang
                       beg-lang end-lang switches beg-switches
                       end-switches header-args beg-header-args
                       end-header-args body beg-body end-body)
               ,@body
	       (goto-char end-block)))))
       (unless visited-p (kill-buffer to-be-removed))
       (goto-char point))))
(def-edebug-spec org-babel-map-src-blocks (form body))

;;;###autoload
(defmacro org-babel-map-inline-src-blocks (file &rest body)
  "Evaluate BODY forms on each inline source block in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer."
  (declare (indent 1) (debug (form body)))
  (org-with-gensyms (datum end point tempvar to-be-removed visitedp)
    `(let* ((case-fold-search t)
	    (,tempvar ,file)
	    (,visitedp (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (,point (point))
	    ,to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq ,to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward "src_\\S-" nil t)
	   (let ((,datum (save-match-data (org-element-context))))
	     (when (eq (org-element-type ,datum) 'inline-src-block)
	       (goto-char (match-beginning 0))
	       (let ((,end (copy-marker (org-element-property :end ,datum))))
		 ,@body
		 (goto-char ,end)
		 (set-marker ,end nil))))))
       (unless ,visitedp (kill-buffer ,to-be-removed))
       (goto-char ,point))))

;;;###autoload
(defmacro org-babel-map-call-lines (file &rest body)
  "Evaluate BODY forms on each call line in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer."
  (declare (indent 1) (debug (form body)))
  (org-with-gensyms (datum end point tempvar to-be-removed visitedp)
    `(let* ((case-fold-search t)
	    (,tempvar ,file)
	    (,visitedp (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (,point (point))
	    ,to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq ,to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward "call_\\S-\\|^[ \t]*#\\+CALL:" nil t)
	   (let ((,datum (save-match-data (org-element-context))))
	     (when (memq (org-element-type ,datum)
			 '(babel-call inline-babel-call))
	       (goto-char (match-beginning 0))
	       (let ((,end (copy-marker (org-element-property :end ,datum))))
		 ,@body
		 (goto-char ,end)
		 (set-marker ,end nil))))))
       (unless ,visitedp (kill-buffer ,to-be-removed))
       (goto-char ,point))))

;;;###autoload
(defmacro org-babel-map-executables (file &rest body)
  "Evaluate BODY forms on each active Babel code in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer."
  (declare (indent 1) (debug (form body)))
  (org-with-gensyms (datum end point tempvar to-be-removed visitedp)
    `(let* ((case-fold-search t)
	    (,tempvar ,file)
	    (,visitedp (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (,point (point))
	    ,to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq ,to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward
		 "\\(call\\|src\\)_\\|^[ \t]*#\\+\\(BEGIN_SRC\\|CALL:\\)" nil t)
	   (let ((,datum (save-match-data (org-element-context))))
	     (when (memq (org-element-type ,datum)
			 '(babel-call inline-babel-call inline-src-block
				      src-block))
	       (goto-char (match-beginning 0))
	       (let ((,end (copy-marker (org-element-property :end ,datum))))
		 ,@body
		 (goto-char ,end)
		 (set-marker ,end nil))))))
       (unless ,visitedp (kill-buffer ,to-be-removed))
       (goto-char ,point))))

;;;###autoload
(defun org-babel-execute-buffer (&optional arg)
  "Execute source code blocks in a buffer.
Call `org-babel-execute-src-block' on every source block in
the current buffer."
  (interactive "P")
  (org-babel-eval-wipe-error-buffer)
  (org-save-outline-visibility t
    (org-babel-map-executables nil
      (if (memq (org-element-type (org-element-context))
		'(babel-call inline-babel-call))
          (org-babel-lob-execute-maybe)
        (org-babel-execute-src-block arg)))))

;;;###autoload
(defun org-babel-execute-subtree (&optional arg)
  "Execute source code blocks in a subtree.
Call `org-babel-execute-src-block' on every source block in
the current subtree."
  (interactive "P")
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (org-babel-execute-buffer arg)
      (widen))))

;;;###autoload
(defun org-babel-sha1-hash (&optional info)
  "Generate an sha1 hash based on the value of info."
  (interactive)
  (let ((print-level nil)
	(info (or info (org-babel-get-src-block-info))))
    (setf (nth 2 info)
	  (sort (copy-sequence (nth 2 info))
		(lambda (a b) (string< (car a) (car b)))))
    (let* ((rm (lambda (lst)
		 (dolist (p '("replace" "silent" "none"
			      "append" "prepend"))
		   (setq lst (remove p lst)))
		 lst))
	   (norm (lambda (arg)
		   (let ((v (if (and (listp (cdr arg)) (null (cddr arg)))
				(copy-sequence (cdr arg))
			      (cdr arg))))
		     (when (and v (not (and (sequencep v)
					    (not (consp v))
					    (= (length v) 0))))
		       (cond
			((and (listp v) ; lists are sorted
			      (member (car arg) '(:result-params)))
			 (sort (funcall rm v) #'string<))
			((and (stringp v) ; strings are sorted
			      (member (car arg) '(:results :exports)))
			 (mapconcat #'identity (sort (funcall rm (split-string v))
						     #'string<) " "))
			(t v))))))
	   ;; expanded body
	   (lang (nth 0 info))
	   (params (nth 2 info))
	   (body (if (org-babel-noweb-p params :eval)
			   (org-babel-expand-noweb-references info) (nth 1 info)))
	   (expand-cmd (intern (concat "org-babel-expand-body:" lang)))
	   (assignments-cmd (intern (concat "org-babel-variable-assignments:"
					    lang)))
	   (expanded
	    (if (fboundp expand-cmd) (funcall expand-cmd body params)
	      (org-babel-expand-body:generic
	       body params (and (fboundp assignments-cmd)
				(funcall assignments-cmd params))))))
      (let* ((it (format "%s-%s"
                         (mapconcat
                          #'identity
                          (delq nil (mapcar (lambda (arg)
                                              (let ((normalized (funcall norm arg)))
                                                (when normalized
                                                  (format "%S" normalized))))
                                            (nth 2 info))) ":")
                         expanded))
             (hash (sha1 it)))
        (when (called-interactively-p 'interactive) (message hash))
        hash))))

(defun org-babel-current-result-hash (&optional info)
  "Return the current in-buffer hash."
  (let ((result (org-babel-where-is-src-block-result nil info)))
    (when result
      (org-with-wide-buffer
       (goto-char result)
       (looking-at org-babel-result-regexp)
       (match-string-no-properties 1)))))

(defun org-babel-set-current-result-hash (hash info)
  "Set the current in-buffer hash to HASH."
  (org-with-wide-buffer
   (goto-char (org-babel-where-is-src-block-result nil info))
   (looking-at org-babel-result-regexp)
   (goto-char (match-beginning 1))
   (mapc #'delete-overlay (overlays-at (point)))
   (forward-char org-babel-hash-show)
   (mapc #'delete-overlay (overlays-at (point)))
   (replace-match hash nil nil nil 1)
   (beginning-of-line)
   (org-babel-hide-hash)))

(defun org-babel-hide-hash ()
  "Hide the hash in the current results line.
Only the initial `org-babel-hash-show' characters of the hash
will remain visible."
  (add-to-invisibility-spec '(org-babel-hide-hash . t))
  (save-excursion
    (when (and (re-search-forward org-babel-result-regexp nil t)
               (match-string 1))
      (let* ((start (match-beginning 1))
             (hide-start (+ org-babel-hash-show start))
             (end (match-end 1))
             (hash (match-string 1))
             ov1 ov2)
        (setq ov1 (make-overlay start hide-start))
        (setq ov2 (make-overlay hide-start end))
        (overlay-put ov2 'invisible 'org-babel-hide-hash)
        (overlay-put ov1 'babel-hash hash)))))

(defun org-babel-hide-all-hashes ()
  "Hide the hash in the current buffer.
Only the initial `org-babel-hash-show' characters of each hash
will remain visible.  This function should be called as part of
the `org-mode-hook'."
  (save-excursion
    (while (and (not org-babel-hash-show-time)
		(re-search-forward org-babel-result-regexp nil t))
      (goto-char (match-beginning 0))
      (org-babel-hide-hash)
      (goto-char (match-end 0)))))
(add-hook 'org-mode-hook 'org-babel-hide-all-hashes)

(defun org-babel-hash-at-point (&optional point)
  "Return the value of the hash at POINT.
\\<org-mode-map>\
The hash is also added as the last element of the kill ring.
This can be called with `\\[org-ctrl-c-ctrl-c]'."
  (interactive)
  (let ((hash (car (delq nil (mapcar
			      (lambda (ol) (overlay-get ol 'babel-hash))
                              (overlays-at (or point (point))))))))
    (when hash (kill-new hash) (message hash))))

(defun org-babel-result-hide-spec ()
  "Hide portions of results lines.
Add `org-babel-hide-result' as an invisibility spec for hiding
portions of results lines."
  (add-to-invisibility-spec '(org-babel-hide-result . t)))
(add-hook 'org-mode-hook 'org-babel-result-hide-spec)

(defvar org-babel-hide-result-overlays nil
  "Overlays hiding results.")

(defun org-babel-result-hide-all ()
  "Fold all results in the current buffer."
  (interactive)
  (org-babel-show-result-all)
  (save-excursion
    (while (re-search-forward org-babel-result-regexp nil t)
      (save-excursion (goto-char (match-beginning 0))
                      (org-babel-hide-result-toggle-maybe)))))

(defun org-babel-show-result-all ()
  "Unfold all results in the current buffer."
  (mapc 'delete-overlay org-babel-hide-result-overlays)
  (setq org-babel-hide-result-overlays nil))

;;;###autoload
(defun org-babel-hide-result-toggle-maybe ()
  "Toggle visibility of result at point."
  (interactive)
  (let ((case-fold-search t))
    (if (save-excursion
          (beginning-of-line 1)
          (looking-at org-babel-result-regexp))
        (progn (org-babel-hide-result-toggle)
               t) ;; to signal that we took action
      nil))) ;; to signal that we did not

(defun org-babel-hide-result-toggle (&optional force)
  "Toggle the visibility of the current result."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward org-babel-result-regexp nil t)
        (let ((start (progn (beginning-of-line 2) (- (point) 1)))
	      (end (progn
		     (while (looking-at org-babel-multi-line-header-regexp)
		       (forward-line 1))
		     (goto-char (- (org-babel-result-end) 1)) (point)))
	      ov)
          (if (memq t (mapcar (lambda (overlay)
                                (eq (overlay-get overlay 'invisible)
				    'org-babel-hide-result))
                              (overlays-at start)))
              (when (or (not force) (eq force 'off))
		(mapc (lambda (ov)
			(when (member ov org-babel-hide-result-overlays)
			  (setq org-babel-hide-result-overlays
				(delq ov org-babel-hide-result-overlays)))
			(when (eq (overlay-get ov 'invisible)
				  'org-babel-hide-result)
			  (delete-overlay ov)))
		      (overlays-at start)))
            (setq ov (make-overlay start end))
            (overlay-put ov 'invisible 'org-babel-hide-result)
            ;; make the block accessible to isearch
            (overlay-put
             ov 'isearch-open-invisible
             (lambda (ov)
               (when (member ov org-babel-hide-result-overlays)
                 (setq org-babel-hide-result-overlays
                       (delq ov org-babel-hide-result-overlays)))
               (when (eq (overlay-get ov 'invisible)
                         'org-babel-hide-result)
                 (delete-overlay ov))))
            (push ov org-babel-hide-result-overlays)))
      (error "Not looking at a result line"))))

;; org-tab-after-check-for-cycling-hook
(add-hook 'org-tab-first-hook 'org-babel-hide-result-toggle-maybe)
;; Remove overlays when changing major mode
(add-hook 'org-mode-hook
	  (lambda () (add-hook 'change-major-mode-hook
			  'org-babel-show-result-all 'append 'local)))

(defvar org-file-properties)
(defun org-babel-params-from-properties (&optional lang)
  "Retrieve parameters specified as properties.
Return a list of association lists of source block params
specified in the properties of the current outline entry."
  (save-match-data
    (list
     ;; header arguments specified with the header-args property at
     ;; point of call.
     (org-babel-parse-header-arguments
      (org-entry-get org-babel-current-src-block-location
		     "header-args"
		     'inherit))
     (and lang	 ; language-specific header arguments at point of call
	  (org-babel-parse-header-arguments
	   (org-entry-get org-babel-current-src-block-location
			  (concat "header-args:" lang)
			  'inherit))))))

(defun org-babel-balanced-split (string alts)
  "Split STRING on instances of ALTS.
ALTS is a character, or cons of two character options where each
option may be either the numeric code of a single character or
a list of character alternatives.  For example, to split on
balanced instances of \"[ \t]:\", set ALTS to ((32 9) . 58)."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((splitp (lambda (past next)
		    ;; Non-nil when there should be a split after NEXT
		    ;; character. PAST is the character before NEXT.
		    (pcase alts
		      (`(,(and first (pred consp)) . ,(and second (pred consp)))
		       (and (memq past first) (memq next second)))
		      (`(,first . ,(and second (pred consp)))
		       (and (eq past first) (memq next second)))
		      (`(,(and first (pred consp)) . ,second)
		       (and (memq past first) (eq next second)))
		      (`(,first . ,second)
		       (and (eq past first) (eq next second)))
		      ((pred (eq next)) t)
		      (_ nil))))
	  (partial nil)
	  (result nil))
      (while (not (eobp))
        (cond
	 ((funcall splitp (char-before) (char-after))
	  ;; There is a split after point.  If ALTS is two-folds,
	  ;; remove last parsed character as it belongs to ALTS.
	  (when (consp alts) (pop partial))
	  ;; Include elements parsed so far in RESULTS and flush
	  ;; partial parsing.
	  (when partial
	    (push (apply #'string (nreverse partial)) result)
	    (setq partial nil))
	  (forward-char))
	 ((memq (char-after) '(?\( ?\[))
	  ;; Include everything between balanced brackets.
	  (let* ((origin (point))
		 (after (char-after))
		 (openings (list after)))
	    (forward-char)
	    (while (and openings (re-search-forward "[]()]" nil t))
	      (pcase (char-before)
		((and match (or ?\[ ?\()) (push match openings))
		(?\] (when (eq ?\[ (car openings)) (pop openings)))
		(_ (when (eq ?\( (car openings)) (pop openings)))))
	    (if (null openings)
		(setq partial
		      (nconc (nreverse (string-to-list
					(buffer-substring origin (point))))
			     partial))
	      ;; Un-balanced bracket.  Backtrack.
	      (push after partial)
	      (goto-char (1+ origin)))))
	 ((and (eq ?\" (char-after)) (not (eq ?\\ (char-before))))
	  ;; Include everything from current double quote to next
	  ;; non-escaped double quote.
	  (let ((origin (point)))
	    (if (re-search-forward "[^\\]\"" nil t)
		(setq partial
		      (nconc (nreverse (string-to-list
					(buffer-substring origin (point))))
			     partial))
	      ;; No closing double quote.  Backtrack.
	      (push ?\" partial)
	      (forward-char))))
	 (t (push (char-after) partial)
	    (forward-char))))
      ;; Add pending parsing and return result.
      (when partial (push (apply #'string (nreverse partial)) result))
      (nreverse result))))

(defun org-babel-join-splits-near-ch (ch list)
  "Join splits where \"=\" is on either end of the split."
  (let ((last= (lambda (str) (= ch (aref str (1- (length str))))))
	(first= (lambda (str) (= ch (aref str 0)))))
    (reverse
     (cl-reduce (lambda (acc el)
		   (let ((head (car acc)))
		     (if (and head (or (funcall last= head) (funcall first= el)))
			 (cons (concat head el) (cdr acc))
		       (cons el acc))))
		 list :initial-value nil))))

(defun org-babel-parse-header-arguments (arg-string)
  "Parse a string of header arguments returning an alist."
  (when (> (length arg-string) 0)
    (org-babel-parse-multiple-vars
     (delq nil
	   (mapcar
	    (lambda (arg)
	      (if (string-match
		   "\\([^ \f\t\n\r\v]+\\)[ \f\t\n\r\v]+\\([^ \f\t\n\r\v]+.*\\)"
		   arg)
		  (cons (intern (match-string 1 arg))
			(org-babel-read (org-babel-chomp (match-string 2 arg))))
		(cons (intern (org-babel-chomp arg)) nil)))
	    (let ((raw (org-babel-balanced-split arg-string '((32 9) . 58))))
              (cons (car raw) (mapcar (lambda (r) (concat ":" r)) (cdr raw)))))))))

(defun org-babel-parse-multiple-vars (header-arguments)
  "Expand multiple variable assignments behind a single :var keyword.

This allows expression of multiple variables with one :var as
shown below.

#+PROPERTY: var foo=1, bar=2"
  (let (results)
    (mapc (lambda (pair)
	    (if (eq (car pair) :var)
		(mapcar (lambda (v) (push (cons :var (org-trim v)) results))
			(org-babel-join-splits-near-ch
			 61 (org-babel-balanced-split (cdr pair) 32)))
	      (push pair results)))
	  header-arguments)
    (nreverse results)))

(defun org-babel-process-params (params)
  "Expand variables in PARAMS and add summary parameters."
  (let* ((processed-vars (mapcar (lambda (el)
				   (if (consp el)
				       el
				     (org-babel-ref-parse el)))
				 (org-babel--get-vars params)))
	 (vars-and-names (if (and (assq :colname-names params)
				  (assq :rowname-names params))
			     (list processed-vars)
			   (org-babel-disassemble-tables
			    processed-vars
			    (cdr (assq :hlines params))
			    (cdr (assq :colnames params))
			    (cdr (assq :rownames params)))))
	 (raw-result (or (cdr (assq :results params)) ""))
	 (result-params (delete-dups
			 (append
			  (split-string (if (stringp raw-result)
					    raw-result
					  (eval raw-result t)))
			  (cdr (assq :result-params params))))))
    (append
     (mapcar (lambda (var) (cons :var var)) (car vars-and-names))
     (list
      (cons :colname-names (or (cdr (assq :colname-names params))
			       (cadr  vars-and-names)))
      (cons :rowname-names (or (cdr (assq :rowname-names params))
			       (cl-caddr vars-and-names)))
      (cons :result-params result-params)
      (cons :result-type  (cond ((member "output" result-params) 'output)
				((member "value" result-params) 'value)
				(t 'value))))
     (cl-remove-if
      (lambda (x) (memq (car x) '(:colname-names :rowname-names :result-params
					    :result-type :var)))
      params))))

;; row and column names
(defun org-babel-del-hlines (table)
  "Remove all `hline's from TABLE."
  (remq 'hline table))

(defun org-babel-get-colnames (table)
  "Return the column names of TABLE.
Return a cons cell, the `car' of which contains the TABLE less
colnames, and the `cdr' of which contains a list of the column
names."
  (if (eq 'hline (nth 1 table))
      (cons (cddr table) (car table))
    (cons (cdr table) (car table))))

(defun org-babel-get-rownames (table)
  "Return the row names of TABLE.
Return a cons cell, the `car' of which contains the TABLE less
rownames, and the `cdr' of which contains a list of the rownames.
Note: this function removes any hlines in TABLE."
  (let* ((table (org-babel-del-hlines table))
	 (rownames (funcall (lambda ()
			      (let ((tp table))
				(mapcar
				 (lambda (_row)
				   (prog1
				       (pop (car tp))
				     (setq tp (cdr tp))))
				 table))))))
    (cons table rownames)))

(defun org-babel-put-colnames (table colnames)
  "Add COLNAMES to TABLE if they exist."
  (if colnames (apply 'list colnames 'hline table) table))

(defun org-babel-put-rownames (table rownames)
  "Add ROWNAMES to TABLE if they exist."
  (if rownames
      (mapcar (lambda (row)
                (if (listp row)
                    (cons (or (pop rownames) "") row)
                  row)) table)
    table))

(defun org-babel-pick-name (names selector)
  "Select one out of an alist of row or column names.
SELECTOR can be either a list of names in which case those names
will be returned directly, or an index into the list NAMES in
which case the indexed names will be return."
  (if (listp selector)
      selector
    (when names
      (if (and selector (symbolp selector) (not (equal t selector)))
	  (cdr (assoc selector names))
	(if (integerp selector)
	    (nth (- selector 1) names)
	  (cdr (car (last names))))))))

(defun org-babel-disassemble-tables (vars hlines colnames rownames)
  "Parse tables for further processing.
Process the variables in VARS according to the HLINES,
ROWNAMES and COLNAMES header arguments.  Return a list consisting
of the vars, cnames and rnames."
  (let (cnames rnames)
    (list
     (mapcar
      (lambda (var)
        (when (listp (cdr var))
          (when (and (not (equal colnames "no"))
                     (or colnames (and (eq (nth 1 (cdr var)) 'hline)
                                       (not (member 'hline (cddr (cdr var)))))))
            (let ((both (org-babel-get-colnames (cdr var))))
              (setq cnames (cons (cons (car var) (cdr both))
                                 cnames))
              (setq var (cons (car var) (car both)))))
          (when (and rownames (not (equal rownames "no")))
            (let ((both (org-babel-get-rownames (cdr var))))
              (setq rnames (cons (cons (car var) (cdr both))
                                 rnames))
              (setq var (cons (car var) (car both)))))
          (when (and hlines (not (equal hlines "yes")))
            (setq var (cons (car var) (org-babel-del-hlines (cdr var))))))
        var)
      vars)
     (reverse cnames) (reverse rnames))))

(defun org-babel-reassemble-table (table colnames rownames)
  "Add column and row names to a table.
Given a TABLE and set of COLNAMES and ROWNAMES add the names
to the table for reinsertion to org-mode."
  (if (listp table)
      (let ((table (if (and rownames (= (length table) (length rownames)))
                       (org-babel-put-rownames table rownames) table)))
        (if (and colnames (listp (car table)) (= (length (car table))
                                                 (length colnames)))
            (org-babel-put-colnames table colnames) table))
    table))

(defun org-babel-where-is-src-block-head (&optional src-block)
  "Find where the current source block begins.

If optional argument SRC-BLOCK is `src-block' type element, find
its current beginning instead.

Return the point at the beginning of the current source block.
Specifically at the beginning of the #+BEGIN_SRC line.  Also set
match-data relatively to `org-babel-src-block-regexp', which see.
If the point is not on a source block then return nil."
  (let ((element (or src-block (org-element-at-point))))
    (when (eq (org-element-type element) 'src-block)
      (let ((end (org-element-property :end element)))
	(org-with-wide-buffer
	 ;; Ensure point is not on a blank line after the block.
	 (beginning-of-line)
	 (skip-chars-forward " \r\t\n" end)
	 (when (< (point) end)
	   (prog1 (goto-char (org-element-property :post-affiliated element))
	     (looking-at org-babel-src-block-regexp))))))))

;;;###autoload
(defun org-babel-goto-src-block-head ()
  "Go to the beginning of the current code block."
  (interactive)
  (let ((head (org-babel-where-is-src-block-head)))
     (if head (goto-char head) (error "Not currently in a code block"))))

;;;###autoload
(defun org-babel-goto-named-src-block (name)
  "Go to a named source-code block."
  (interactive
   (let ((completion-ignore-case t)
	 (case-fold-search t)
	 (all-block-names (org-babel-src-block-names)))
     (list (completing-read
	    "source-block name: " all-block-names nil t
	    (let* ((context (org-element-context))
		   (type (org-element-type context))
		   (noweb-ref
		    (and (memq type '(inline-src-block src-block))
			 (org-in-regexp (org-babel-noweb-wrap)))))
	      (cond
	       (noweb-ref
		(buffer-substring
		 (+ (car noweb-ref) (length org-babel-noweb-wrap-start))
		 (- (cdr noweb-ref) (length org-babel-noweb-wrap-end))))
	       ((memq type '(babel-call inline-babel-call)) ;#+CALL:
		(org-element-property :call context))
	       ((car (org-element-property :results context))) ;#+RESULTS:
	       ((let ((symbol (thing-at-point 'symbol))) ;Symbol.
		  (and symbol
		       (member-ignore-case symbol all-block-names)
		       symbol)))
	       (t "")))))))
  (let ((point (org-babel-find-named-block name)))
    (if point
        ;; Taken from `org-open-at-point'.
        (progn (org-mark-ring-push) (goto-char point) (org-show-context))
      (message "source-code block `%s' not found in this buffer" name))))

(defun org-babel-find-named-block (name)
  "Find a named source-code block.
Return the location of the source block identified by source
NAME, or nil if no such block exists.  Set match data according
to `org-babel-named-src-block-regexp'."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (org-babel-named-src-block-regexp-for-name name)))
      (or (and (looking-at regexp)
	       (progn (goto-char (match-beginning 1))
		      (line-beginning-position)))
	  (ignore-errors (org-next-block 1 nil regexp))))))

(defun org-babel-src-block-names (&optional file)
  "Returns the names of source blocks in FILE or the current buffer."
  (when file (find-file file))
  (save-excursion
    (goto-char (point-min))
    (let* ((re (org-babel-named-src-block-regexp-for-name))
	   (names (and (looking-at re)
		       (list (match-string-no-properties 9)))))
      (while (ignore-errors (org-next-block 1 nil re))
	(push (match-string-no-properties 9) names))
      names)))

;;;###autoload
(defun org-babel-goto-named-result (name)
  "Go to a named result."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Source-block name: "
			    (org-babel-result-names) nil t))))
  (let ((point (org-babel-find-named-result name)))
    (if point
        ;; taken from `org-open-at-point'
        (progn (goto-char point) (org-show-context))
      (message "result `%s' not found in this buffer" name))))

(defun org-babel-find-named-result (name)
  "Find a named result.
Return the location of the result named NAME in the current
buffer or nil if no such result exists."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (re (format "^[ \t]*#\\+%s.*?:[ \t]*%s[ \t]*$"
		      org-babel-results-keyword
		      (regexp-quote name))))
      (catch :found
	(while (re-search-forward re nil t)
	  (let ((element (org-element-at-point)))
	    (when (or (eq (org-element-type element) 'keyword)
		      (< (point)
			 (org-element-property :post-affiliated element)))
	      (throw :found (line-beginning-position)))))))))

(defun org-babel-result-names (&optional file)
  "Returns the names of results in FILE or the current buffer."
  (save-excursion
    (when file (find-file file)) (goto-char (point-min))
    (let ((case-fold-search t) names)
      (while (re-search-forward org-babel-result-w-name-regexp nil t)
	(setq names (cons (match-string-no-properties 9) names)))
      names)))

;;;###autoload
(defun org-babel-next-src-block (&optional arg)
  "Jump to the next source block.
With optional prefix argument ARG, jump forward ARG many source blocks."
  (interactive "p")
  (org-next-block arg nil org-babel-src-block-regexp))

;;;###autoload
(defun org-babel-previous-src-block (&optional arg)
  "Jump to the previous source block.
With optional prefix argument ARG, jump backward ARG many source blocks."
  (interactive "p")
  (org-previous-block arg org-babel-src-block-regexp))

(defvar org-babel-load-languages)

;;;###autoload
(defun org-babel-mark-block ()
  "Mark current src block."
  (interactive)
  (let ((head (org-babel-where-is-src-block-head)))
    (when head
      (save-excursion
        (goto-char head)
        (looking-at org-babel-src-block-regexp))
      (push-mark (match-end 5) nil t)
      (goto-char (match-beginning 5)))))

(defun org-babel-demarcate-block (&optional arg)
  "Wrap or split the code in the region or on the point.
When called from inside of a code block the current block is
split.  When called from outside of a code block a new code block
is created.  In both cases if the region is demarcated and if the
region is not active then the point is demarcated."
  (interactive "P")
  (let* ((info (org-babel-get-src-block-info 'light))
	 (start (org-babel-where-is-src-block-head))
	 (block (and start (match-string 0)))
	 (headers (and start (match-string 4)))
	 (stars (concat (make-string (or (org-current-level) 1) ?*) " "))
	 (lower-case-p (and block
			    (let (case-fold-search)
			      (string-match-p "#\\+begin_src" block)))))
    (if info
        (mapc
         (lambda (place)
           (save-excursion
             (goto-char place)
             (let ((lang (nth 0 info))
                   (indent (make-string (org-get-indentation) ?\s)))
	       (when (string-match "^[[:space:]]*$"
				   (buffer-substring (point-at-bol)
						     (point-at-eol)))
		 (delete-region (point-at-bol) (point-at-eol)))
               (insert (concat
			(if (looking-at "^") "" "\n")
			indent (funcall (if lower-case-p 'downcase 'upcase) "#+end_src\n")
			(if arg stars indent) "\n"
			indent (funcall (if lower-case-p 'downcase 'upcase) "#+begin_src ")
			lang
			(if (> (length headers) 1)
			    (concat " " headers) headers)
			(if (looking-at "[\n\r]")
			    ""
			  (concat "\n" (make-string (current-column) ? )))))))
	   (move-end-of-line 2))
         (sort (if (org-region-active-p) (list (mark) (point)) (list (point))) #'>))
      (let ((start (point))
	    (lang (completing-read
		   "Lang: "
		   (mapcar #'symbol-name
			   (delete-dups
			    (append (mapcar #'car org-babel-load-languages)
				    (mapcar (lambda (el) (intern (car el)))
					    org-src-lang-modes))))))
	    (body (delete-and-extract-region
		   (if (org-region-active-p) (mark) (point)) (point))))
	(insert (concat (if (looking-at "^") "" "\n")
			(if arg (concat stars "\n") "")
			(funcall (if lower-case-p 'downcase 'upcase) "#+begin_src ")
			lang "\n"
			body
			(if (or (= (length body) 0)
				(string-suffix-p "\r" body)
				(string-suffix-p "\n" body)) "" "\n")
			(funcall (if lower-case-p 'downcase 'upcase) "#+end_src\n")))
	(goto-char start) (move-end-of-line 1)))))

(defun org-babel--insert-results-keyword (name hash)
  "Insert RESULTS keyword with NAME value at point.
If NAME is nil, results are anonymous.  HASH is a string used as
the results hash, or nil.  Leave point before the keyword."
  (save-excursion (insert "\n"))	;open line to indent.
  (org-indent-line)
  (delete-char 1)
  (insert (concat "#+" org-babel-results-keyword
		  (cond ((not hash) nil)
			(org-babel-hash-show-time
			 (format "[%s %s]"
				 (format-time-string "<%F %T>")
				 hash))
			(t (format "[%s]" hash)))
		  ":"
		  (when name (concat " " name))
		  "\n"))
  ;; Make sure results are going to be followed by at least one blank
  ;; line so they do not get merged with the next element, e.g.,
  ;;
  ;;   #+results:
  ;;   : 1
  ;;
  ;;   : fixed-width area, unrelated to the above.
  (unless (looking-at "^[ \t]*$") (save-excursion (insert "\n")))
  (beginning-of-line 0)
  (when hash (org-babel-hide-hash)))

(defun org-babel--clear-results-maybe (hash)
  "Clear results when hash doesn't match HASH.

When results hash does not match HASH, remove RESULTS keyword at
point, along with related contents.  Do nothing if HASH is nil.

Return a non-nil value if results were cleared.  In this case,
leave point where new results should be inserted."
  (when hash
    (looking-at org-babel-result-regexp)
    (unless (string= (match-string 1) hash)
      (let* ((e (org-element-at-point))
	     (post (copy-marker (org-element-property :post-affiliated e))))
	;; Delete contents.
	(delete-region post
		       (save-excursion
			 (goto-char (org-element-property :end e))
			 (skip-chars-backward " \t\n")
			 (line-beginning-position 2)))
	;; Delete RESULT keyword.  However, if RESULTS keyword is
	;; orphaned, ignore this part.  The deletion above already
	;; took care of it.
	(unless (= (point) post)
	  (delete-region (line-beginning-position)
			 (line-beginning-position 2)))
	(goto-char post)
	(set-marker post nil)
	t))))

(defun org-babel-where-is-src-block-result (&optional insert _info hash)
  "Find where the current source block results begin.

Return the point at the beginning of the result of the current
source block, specifically at the beginning of the results line.

If no result exists for this block return nil, unless optional
argument INSERT is non-nil.  In this case, create a results line
following the source block and return the position at its
beginning.  In the case of inline code, remove the results part
instead.

If optional argument HASH is a string, remove contents related to
RESULTS keyword if its hash is different.  Then update the latter
to HASH."
  (let ((context (org-element-context)))
    (catch :found
      (org-with-wide-buffer
       (pcase (org-element-type context)
	 ((or `inline-babel-call `inline-src-block)
	  ;; Results for inline objects are located right after them.
	  ;; There is no RESULTS line to insert either.
	  (let ((limit (org-element-property
			:contents-end (org-element-property :parent context))))
	    (goto-char (org-element-property :end context))
	    (skip-chars-forward " \t\n" limit)
	    (throw :found
		   (and
		    (< (point) limit)
		    (let ((result (org-element-context)))
		      (and (eq (org-element-type result) 'macro)
			   (string= (org-element-property :key result)
				    "results")
			   (if (not insert) (point)
			     (delete-region
			      (point)
			      (progn
				(goto-char (org-element-property :end result))
				(skip-chars-backward " \t")
				(point)))
			     (point))))))))
	 ((or `babel-call `src-block)
	  (let* ((name (org-element-property :name context))
		 (named-results (and name (org-babel-find-named-result name))))
	    (goto-char (or named-results (org-element-property :end context)))
	    (cond
	     ;; Existing results named after the current source.
	     (named-results
	      (when (org-babel--clear-results-maybe hash)
		(org-babel--insert-results-keyword name hash))
	      (throw :found (point)))
	     ;; Named results expect but none to be found.
	     (name)
	     ;; No possible anonymous results at the very end of
	     ;; buffer or outside CONTEXT parent.
	     ((eq (point)
		  (or (org-element-property
		       :contents-end (org-element-property :parent context))
		      (point-max))))
	     ;; Check if next element is an anonymous result below
	     ;; the current block.
	     ((let* ((next (org-element-at-point))
		     (end (save-excursion
			    (goto-char
			     (org-element-property :post-affiliated next))
			    (line-end-position)))
		     (empty-result-re (concat org-babel-result-regexp "$"))
		     (case-fold-search t))
		(re-search-forward empty-result-re end t))
	      (beginning-of-line)
	      (when (org-babel--clear-results-maybe hash)
		(org-babel--insert-results-keyword nil hash))
	      (throw :found (point))))))
	 ;; Ignore other elements.
	 (_ (throw :found nil))))
      ;; No result found.  Insert a RESULTS keyword below element, if
      ;; appropriate.  In this case, ensure there is an empty line
      ;; after the previous element.
      (when insert
	(save-excursion
	  (goto-char (min (org-element-property :end context) (point-max)))
	  (skip-chars-backward " \t\n")
	  (forward-line)
	  (unless (bolp) (insert "\n"))
	  (insert "\n")
	  (org-babel--insert-results-keyword
	   (org-element-property :name context) hash)
	  (point))))))

(defun org-babel-read-element (element)
  "Read ELEMENT into emacs-lisp.
Return nil if ELEMENT cannot be read."
  (org-with-wide-buffer
   (goto-char (org-element-property :post-affiliated element))
   (pcase (org-element-type element)
     (`fixed-width
      (let ((v (org-trim (org-element-property :value element))))
	(or (org-babel--string-to-number v) v)))
     (`table (org-babel-read-table))
     (`plain-list (org-babel-read-list))
     (`example-block
      (let ((v (org-element-property :value element)))
	(if (or org-src-preserve-indentation
		(org-element-property :preserve-indent element))
	    v
	  (org-remove-indentation v))))
     (`export-block
      (org-remove-indentation (org-element-property :value element)))
     (`paragraph
      ;; Treat paragraphs containing a single link specially.
      (skip-chars-forward " \t")
      (if (and (looking-at org-bracket-link-regexp)
	       (save-excursion
		 (goto-char (match-end 0))
		 (skip-chars-forward " \r\t\n")
		 (<= (org-element-property :end element)
		     (point))))
	  (org-babel-read-link)
	(buffer-substring-no-properties
	 (org-element-property :contents-begin element)
	 (org-element-property :contents-end element))))
     ((or `center-block `quote-block `verse-block `special-block)
      (org-remove-indentation
       (buffer-substring-no-properties
	(org-element-property :contents-begin element)
	(org-element-property :contents-end element))))
     (_ nil))))

(defun org-babel-read-result ()
  "Read the result at point into emacs-lisp."
  (and (not (save-excursion
	      (beginning-of-line)
	      (looking-at-p "[ \t]*$")))
       (org-babel-read-element (org-element-at-point))))

(defun org-babel-read-table ()
  "Read the table at point into emacs-lisp."
  (mapcar (lambda (row)
            (if (and (symbolp row) (equal row 'hline)) row
              (mapcar (lambda (el) (org-babel-read el 'inhibit-lisp-eval)) row)))
          (org-table-to-lisp)))

(defun org-babel-read-list ()
  "Read the list at point into emacs-lisp."
  (mapcar (lambda (el) (org-babel-read el 'inhibit-lisp-eval))
	  (cdr (org-list-to-lisp))))

(defvar org-link-types-re)
(defun org-babel-read-link ()
  "Read the link at point into emacs-lisp.
If the path of the link is a file path it is expanded using
`expand-file-name'."
  (let* ((case-fold-search t)
         (raw (and (looking-at org-bracket-link-regexp)
                   (org-no-properties (match-string 1))))
         (type (and (string-match org-link-types-re raw)
                    (match-string 1 raw))))
    (cond
     ((not type) (expand-file-name raw))
     ((string= type "file")
      (and (string-match "file\\(.*\\):\\(.+\\)" raw)
           (expand-file-name (match-string 2 raw))))
     (t raw))))

(defun org-babel-format-result (result &optional sep)
  "Format RESULT for writing to file."
  (let ((echo-res (lambda (r) (if (stringp r) r (format "%S" r)))))
    (if (listp result)
	;; table result
	(orgtbl-to-generic
	 result (list :sep (or sep "\t") :fmt echo-res))
      ;; scalar result
      (funcall echo-res result))))

(defun org-babel-insert-result (result &optional result-params info hash lang)
  "Insert RESULT into the current buffer.

By default RESULT is inserted after the end of the current source
block.  The RESULT of an inline source block usually will be
wrapped inside a `results' macro and placed on the same line as
the inline source block.  The macro is stripped upon export.
Multiline and non-scalar RESULTS from inline source blocks are
not allowed.  With optional argument RESULT-PARAMS controls
insertion of results in the Org mode file.  RESULT-PARAMS can
take the following values:

replace - (default option) insert results after the source block
          or inline source block replacing any previously
          inserted results.

silent -- no results are inserted into the Org buffer but
          the results are echoed to the minibuffer and are
          ingested by Emacs (a potentially time consuming
          process).

file ---- the results are interpreted as a file path, and are
          inserted into the buffer using the Org file syntax.

list ---- the results are interpreted as an Org list.

raw ----- results are added directly to the Org file.  This is
          a good option if you code block will output Org
          formatted text.

drawer -- results are added directly to the Org file as with
          \"raw\", but are wrapped in a RESULTS drawer or results
          macro, allowing them to later be replaced or removed
          automatically.

org ----- results are added inside of a \"src_org{}\" or \"#+BEGIN_SRC
          org\" block depending on whether the current source block is
          inline or not.  They are not comma-escaped when inserted,
          but Org syntax here will be discarded when exporting the
          file.

html ---- results are added inside of a #+BEGIN_EXPORT HTML block
          or html export snippet depending on whether the current
          source block is inline or not.  This is a good option
          if your code block will output html formatted text.

latex --- results are added inside of a #+BEGIN_EXPORT LATEX
          block or latex export snippet depending on whether the
          current source block is inline or not.  This is a good
          option if your code block will output latex formatted
          text.

code ---- the results are extracted in the syntax of the source
          code of the language being evaluated and are added
          inside of a source block with the source-code language
          set appropriately.  Also, source block inlining is
          preserved in this case.  Note this relies on the
          optional LANG argument.

list ---- the results are rendered as a list.  This option not
          allowed for inline src blocks.

table --- the results are rendered as a table.  This option not
          allowed for inline src blocks.

INFO may provide the values of these header arguments (in the
`header-arguments-alist' see the docstring for
`org-babel-get-src-block-info'):

:file --- the name of the file to which output should be written.

:wrap --- the effect is similar to `latex' in RESULT-PARAMS but
          using the argument supplied to specify the export block
          or snippet type."
  (cond ((stringp result)
	 (setq result (org-no-properties result))
	 (when (member "file" result-params)
	   (setq result (org-babel-result-to-file
			 result (when (assq :file-desc (nth 2 info))
				  (or (cdr (assq :file-desc (nth 2 info)))
				      result))))))
	((listp result))
	(t (setq result (format "%S" result))))
  (if (and result-params (member "silent" result-params))
      (progn (message (replace-regexp-in-string "%" "%%" (format "%S" result)))
	     result)
    (let ((inline (let ((context (org-element-context)))
		    (and (memq (org-element-type context)
			       '(inline-babel-call inline-src-block))
			 context))))
      (when inline
	(let ((warning
	       (or (and (member "table" result-params) "`:results table'")
		   (and (listp result) "list result")
		   (and (string-match-p "\n." result) "multiline result")
		   (and (member "list" result-params) "`:results list'"))))
	  (when warning
	    (user-error "Inline error: %s cannot be used" warning))))
      (save-excursion
	(let* ((visible-beg (point-min-marker))
	       (visible-end (copy-marker (point-max) t))
	       (inline (let ((context (org-element-context)))
			 (and (memq (org-element-type context)
				    '(inline-babel-call inline-src-block))
			      context)))
	       (existing-result (org-babel-where-is-src-block-result t nil hash))
	       (results-switches (cdr (assq :results_switches (nth 2 info))))
	       ;; When results exist outside of the current visible
	       ;; region of the buffer, be sure to widen buffer to
	       ;; update them.
	       (outside-scope (and existing-result
				   (buffer-narrowed-p)
				   (or (> visible-beg existing-result)
				       (<= visible-end existing-result))))
	       beg end indent)
	  ;; Ensure non-inline results end in a newline.
	  (when (and (org-string-nw-p result)
		     (not inline)
		     (not (string-equal (substring result -1) "\n")))
	    (setq result (concat result "\n")))
	  (unwind-protect
	      (progn
		(when outside-scope (widen))
		(if existing-result (goto-char existing-result)
		  (goto-char (org-element-property :end inline))
		  (skip-chars-backward " \t"))
		(unless inline
		  (setq indent (org-get-indentation))
		  (forward-line 1))
		(setq beg (point))
		(cond
		 (inline
		   ;; Make sure new results are separated from the
		   ;; source code by one space.
		   (unless existing-result
		     (insert " ")
		     (setq beg (point))))
		 ((member "replace" result-params)
		  (delete-region (point) (org-babel-result-end)))
		 ((member "append" result-params)
		  (goto-char (org-babel-result-end)) (setq beg (point-marker)))
		 ((member "prepend" result-params))) ; already there
		(setq results-switches
		      (if results-switches (concat " " results-switches) ""))
		(let ((wrap
		       (lambda (start finish &optional no-escape no-newlines
				 inline-start inline-finish)
			 (when inline
			   (setq start inline-start)
			   (setq finish inline-finish)
			   (setq no-newlines t))
			 (let ((before-finish (marker-position end)))
			   (goto-char end)
			   (insert (concat finish (unless no-newlines "\n")))
			   (goto-char beg)
			   (insert (concat start (unless no-newlines "\n")))
			   (unless no-escape
			     (org-escape-code-in-region
			      (min (point) before-finish) before-finish))
			   (goto-char end))))
		      (tabulablep
		       (lambda (r)
			 ;; Non-nil when result R can be turned into
			 ;; a table.
			 (and (listp r)
			      (null (cdr (last r)))
			      (cl-every
			       (lambda (e) (or (atom e) (null (cdr (last e)))))
			       result)))))
		  ;; insert results based on type
		  (cond
		   ;; Do nothing for an empty result.
		   ((null result))
		   ;; Insert a list if preferred.
		   ((member "list" result-params)
		    (insert
		     (org-trim
		      (org-list-to-generic
		       (cons 'unordered
			     (mapcar
			      (lambda (e)
				(list (if (stringp e) e (format "%S" e))))
			      (if (listp result) result
				(split-string result "\n" t))))
		       '(:splicep nil :istart "- " :iend "\n")))
		     "\n"))
		   ;; Try hard to print RESULT as a table.  Give up if
		   ;; it contains an improper list.
		   ((funcall tabulablep result)
		    (goto-char beg)
		    (insert (concat (orgtbl-to-orgtbl
				     (if (cl-every
					  (lambda (e)
					    (or (eq e 'hline) (listp e)))
					  result)
					 result
				       (list result))
				     nil)
				    "\n"))
		    (goto-char beg)
		    (when (org-at-table-p) (org-table-align))
		    (goto-char (org-table-end)))
		   ;; Print verbatim a list that cannot be turned into
		   ;; a table.
		   ((listp result) (insert (format "%s\n" result)))
		   ((member "file" result-params)
		    (when inline
		      (setq result (org-macro-escape-arguments result)))
		    (insert result))
		   ((and inline (not (member "raw" result-params)))
		    (insert (org-macro-escape-arguments
			     (org-babel-chomp result "\n"))))
		   (t (goto-char beg) (insert result)))
		  (setq end (copy-marker (point) t))
		  ;; possibly wrap result
		  (cond
		   ((assq :wrap (nth 2 info))
		    (let ((name (or (cdr (assq :wrap (nth 2 info))) "RESULTS")))
		      (funcall wrap (concat "#+BEGIN_" name)
			       (concat "#+END_" (car (split-string name)))
			       nil nil (concat "{{{results(@@" name ":") "@@)}}}")))
		   ((member "html" result-params)
		    (funcall wrap "#+BEGIN_EXPORT html" "#+END_EXPORT" nil nil
			     "{{{results(@@html:" "@@)}}}"))
		   ((member "latex" result-params)
		    (funcall wrap "#+BEGIN_EXPORT latex" "#+END_EXPORT" nil nil
			     "{{{results(@@latex:" "@@)}}}"))
		   ((member "org" result-params)
		    (goto-char beg) (when (org-at-table-p) (org-cycle))
		    (funcall wrap "#+BEGIN_SRC org" "#+END_SRC" nil nil
			     "{{{results(src_org{" "})}}}"))
		   ((member "code" result-params)
		    (let ((lang (or lang "none")))
		      (funcall wrap (format "#+BEGIN_SRC %s%s" lang results-switches)
			       "#+END_SRC" nil nil
			       (format "{{{results(src_%s[%s]{" lang results-switches)
			       "})}}}")))
		   ((member "raw" result-params)
		    (goto-char beg) (when (org-at-table-p) (org-cycle)))
		   ((or (member "drawer" result-params)
			;; Stay backward compatible with <7.9.2
			(member "wrap" result-params))
		    (goto-char beg) (when (org-at-table-p) (org-cycle))
		    (funcall wrap ":RESULTS:" ":END:" 'no-escape nil
			     "{{{results(" ")}}}"))
		   ((and inline (member "file" result-params))
		    (funcall wrap nil nil nil nil "{{{results(" ")}}}"))
		   ((and (not (funcall tabulablep result))
			 (not (member "file" result-params)))
		    (let ((org-babel-inline-result-wrap
			   ;; Hard code {{{results(...)}}} on top of
			   ;; customization.
			   (format "{{{results(%s)}}}"
				   org-babel-inline-result-wrap)))
		      (org-babel-examplify-region
		       beg end results-switches inline)))))
		;; Possibly indent results in par with #+results line.
		(when (and (not inline) (numberp indent) (> indent 0)
			   ;; In this case `table-align' does the work
			   ;; for us.
			   (not (and (listp result)
				     (member "append" result-params))))
		  (indent-rigidly beg end indent))
		(if (null result)
		    (if (member "value" result-params)
			(message "Code block returned no value.")
		      (message "Code block produced no output."))
		  (message "Code block evaluation complete.")))
	    (set-marker end nil)
	    (when outside-scope (narrow-to-region visible-beg visible-end))
	    (set-marker visible-beg nil)
	    (set-marker visible-end nil)))))))

(defun org-babel-remove-result (&optional info keep-keyword)
  "Remove the result of the current source block."
  (interactive)
  (let ((location (org-babel-where-is-src-block-result nil info)))
    (when location
      (save-excursion
        (goto-char location)
	(when (looking-at (concat org-babel-result-regexp ".*$"))
	  (delete-region
	   (if keep-keyword (1+ (match-end 0)) (1- (match-beginning 0)))
	   (progn (forward-line 1) (org-babel-result-end))))))))

(defun org-babel-remove-inline-result (&optional datum)
  "Remove the result of the current inline-src-block or babel call.
The result must be wrapped in a `results' macro to be removed.
Leading white space is trimmed."
  (interactive)
  (let* ((el (or datum (org-element-context))))
    (when (memq (org-element-type el) '(inline-src-block inline-babel-call))
      (org-with-wide-buffer
       (goto-char (org-element-property :end el))
       (skip-chars-backward " \t")
       (let ((result (save-excursion
		       (skip-chars-forward
			" \t\n"
			(org-element-property
			 :contents-end (org-element-property :parent el)))
		       (org-element-context))))
	 (when (and (eq (org-element-type result) 'macro)
		    (string= (org-element-property :key result) "results"))
	   (delete-region		; And leading whitespace.
	    (point)
	    (progn (goto-char (org-element-property :end result))
		   (skip-chars-backward " \t\n")
		   (point)))))))))

(defun org-babel-remove-result-one-or-many (x)
  "Remove the result of the current source block.
If called with a prefix argument, remove all result blocks
in the buffer."
  (interactive "P")
  (if x
      (org-babel-map-src-blocks nil (org-babel-remove-result))
    (org-babel-remove-result)))

(defun org-babel-result-end ()
  "Return the point at the end of the current set of results."
  (save-excursion
    (cond
     ((org-at-table-p) (progn (goto-char (org-table-end)) (point)))
     ((org-at-item-p) (let* ((struct (org-list-struct))
			     (prvs (org-list-prevs-alist struct)))
			(org-list-get-list-end (point-at-bol) struct prvs)))
     ((let ((case-fold-search t)) (looking-at "^\\([ \t]*\\):results:"))
      (progn (re-search-forward (concat "^" (match-string 1) ":END:"))
	     (forward-char 1) (point)))
     (t
      (let ((case-fold-search t))
	(if (looking-at (concat "[ \t]*#\\+begin_\\([^ \t\n\r]+\\)"))
	    (progn (re-search-forward (concat "[ \t]*#\\+end_" (match-string 1))
				      nil t)
		   (forward-char 1))
	  (while (looking-at "[ \t]*\\(: \\|:$\\|\\[\\[\\)")
	    (forward-line 1))))
      (point)))))

(defun org-babel-result-to-file (result &optional description)
  "Convert RESULT into an `org-mode' link with optional DESCRIPTION.
If the `default-directory' is different from the containing
file's directory then expand relative links."
  (when (stringp result)
    (format "[[file:%s]%s]"
	    (if (and default-directory
		     buffer-file-name
		     (not (string= (expand-file-name default-directory)
				   (expand-file-name
				    (file-name-directory buffer-file-name)))))
		(expand-file-name result default-directory)
	      result)
	    (if description (concat "[" description "]") ""))))

(defun org-babel-examplify-region (beg end &optional results-switches inline)
  "Comment out region using the inline `==' or `: ' org example quote."
  (interactive "*r")
  (let ((maybe-cap
	 (lambda (str)
	   (if org-babel-uppercase-example-markers (upcase str) str))))
    (if inline
	(save-excursion
	  (goto-char beg)
	  (insert (format org-babel-inline-result-wrap
			  (delete-and-extract-region beg end))))
      (let ((size (count-lines beg end)))
	(save-excursion
	  (cond ((= size 0))	      ; do nothing for an empty result
		((< size org-babel-min-lines-for-block-output)
		 (goto-char beg)
		 (dotimes (_ size)
		   (beginning-of-line 1) (insert ": ") (forward-line 1)))
		(t
		 (goto-char beg)
		 (insert (if results-switches
			     (format "%s%s\n"
				     (funcall maybe-cap "#+begin_example")
				     results-switches)
			   (funcall maybe-cap "#+begin_example\n")))
		 (let ((p (point)))
		   (if (markerp end) (goto-char end) (forward-char (- end beg)))
		   (org-escape-code-in-region p (point)))
		 (insert (funcall maybe-cap "#+end_example\n")))))))))

(defun org-babel-update-block-body (new-body)
  "Update the body of the current code block to NEW-BODY."
  (let ((element (org-element-at-point)))
    (unless (eq (org-element-type element) 'src-block)
      (error "Not in a source block"))
    (goto-char (org-babel-where-is-src-block-head element))
    (let* ((ind (org-get-indentation))
	   (body-start (line-beginning-position 2))
	   (body (org-element-normalize-string
		  (if (or org-src-preserve-indentation
			  (org-element-property :preserve-indent element))
		      new-body
		    (with-temp-buffer
		      (insert (org-remove-indentation new-body))
		      (indent-rigidly
		       (point-min)
		       (point-max)
		       (+ ind org-edit-src-content-indentation))
		      (buffer-string))))))
      (delete-region body-start
		     (org-with-wide-buffer
		      (goto-char (org-element-property :end element))
		      (skip-chars-backward " \t\n")
		      (line-beginning-position)))
      (goto-char body-start)
      (insert body))))

(defun org-babel-merge-params (&rest plists)
  "Combine all parameter association lists in PLISTS.
Later elements of PLISTS override the values of previous elements.
This takes into account some special considerations for certain
parameters when merging lists."
  (let* ((results-exclusive-groups
	  (mapcar (lambda (group) (mapcar #'symbol-name group))
		  (cdr (assq 'results org-babel-common-header-args-w-values))))
	 (exports-exclusive-groups
	  (mapcar (lambda (group) (mapcar #'symbol-name group))
		  (cdr (assq 'exports org-babel-common-header-args-w-values))))
	 (merge
	  (lambda (exclusive-groups &rest result-params)
	    ;; Maintain exclusivity of mutually exclusive parameters,
	    ;; as defined in EXCLUSIVE-GROUPS while merging lists in
	    ;; RESULT-PARAMS.
	    (let (output)
	      (dolist (new-params result-params (delete-dups output))
		(dolist (new-param new-params)
		  (dolist (exclusive-group exclusive-groups)
		    (when (member new-param exclusive-group)
		      (setq output (cl-remove-if
				    (lambda (o) (member o exclusive-group))
				    output))))
		  (push new-param output))))))
	 (variable-index 0)		;Handle positional arguments.
	 clearnames
	 params				;Final parameters list.
	 ;; Some keywords accept multiple values.  We need to treat
	 ;; them specially.
	 vars results exports)
    (dolist (plist plists)
      (dolist (pair plist)
	(pcase pair
	  (`(:var . ,value)
	   (let ((name (cond
			((listp value) (car value))
			((string-match "^\\([^= \f\t\n\r\v]+\\)[ \t]*=" value)
			 (intern (match-string 1 value)))
			(t nil))))
	     (cond
	      (name
	       (setq vars
		     (append (if (not (assoc name vars)) vars
			       (push name clearnames)
			       (cl-remove-if (lambda (p) (equal name (car p)))
					     vars))
			     (list (cons name pair)))))
	      ((and vars (nth variable-index vars))
	       ;; If no name is given and we already have named
	       ;; variables then assign to named variables in order.
	       (let ((name (car (nth variable-index vars))))
		 ;; Clear out colnames and rownames for replace vars.
		 (push name clearnames)
		 (setf (cddr (nth variable-index vars))
		       (concat (symbol-name name) "=" value))
		 (cl-incf variable-index)))
	      (t (error "Variable \"%s\" must be assigned a default value"
			(cdr pair))))))
	  (`(:results . ,value)
	   (setq results (funcall merge
				  results-exclusive-groups
				  results
				  (split-string
				   (if (stringp value) value (eval value t))))))
	  (`(,(or :file :file-ext) . ,value)
	   ;; `:file' and `:file-ext' are regular keywords but they
	   ;; imply a "file" `:results' and a "results" `:exports'.
	   (when value
	     (setq results
		   (funcall merge results-exclusive-groups results '("file")))
	     (unless (or (member "both" exports)
			 (member "none" exports)
			 (member "code" exports))
	       (setq exports
		     (funcall merge
			      exports-exclusive-groups exports '("results"))))
	     (push pair params)))
	  (`(:exports . ,value)
	   (setq exports (funcall merge
				  exports-exclusive-groups
				  exports
				  (split-string (or value "")))))
	  ;; Regular keywords: any value overwrites the previous one.
	  (_ (setq params (cons pair (assq-delete-all (car pair) params)))))))
    ;; Handle `:var' and clear out colnames and rownames for replaced
    ;; variables.
    (setq params (nconc (mapcar (lambda (v) (cons :var (cddr v))) vars)
			params))
    (dolist (name clearnames)
      (dolist (param '(:colname-names :rowname-names))
	(when (assq param params)
	  (setf (cdr (assq param params))
		(cl-remove-if (lambda (pair) (equal name (car pair)))
			      (cdr (assq param params))))
	  (setq params
		(cl-remove-if (lambda (pair) (and (equal (car pair) param)
					     (null (cdr pair))))
			      params)))))
    ;; Handle other special keywords, which accept multiple values.
    (setq params (nconc (list (cons :results (mapconcat #'identity results " "))
			      (cons :exports (mapconcat #'identity exports " ")))
			params))
    ;; Return merged params.
    params))

(defvar org-babel-use-quick-and-dirty-noweb-expansion nil
  "Set to true to use regular expressions to expand noweb references.
This results in much faster noweb reference expansion but does
not properly allow code blocks to inherit the \":noweb-ref\"
header argument from buffer or subtree wide properties.")

(defun org-babel-noweb-p (params context)
  "Check if PARAMS require expansion in CONTEXT.
CONTEXT may be one of :tangle, :export or :eval."
  (let ((allowed-values (cl-case context
			  (:tangle '("yes" "tangle" "no-export" "strip-export"))
			  (:eval   '("yes" "no-export" "strip-export" "eval"))
			  (:export '("yes")))))
    (cl-some (lambda (v) (member v allowed-values))
	     (split-string (or (cdr (assq :noweb params)) "")))))

(defun org-babel-expand-noweb-references (&optional info parent-buffer)
  "Expand Noweb references in the body of the current source code block.

For example the following reference would be replaced with the
body of the source-code block named `example-block'.

<<example-block>>

Note that any text preceding the <<foo>> construct on a line will
be interposed between the lines of the replacement text.  So for
example if <<foo>> is placed behind a comment, then the entire
replacement text will also be commented.

This function must be called from inside of the buffer containing
the source-code block which holds BODY.

In addition the following syntax can be used to insert the
results of evaluating the source-code block named `example-block'.

<<example-block()>>

Any optional arguments can be passed to example-block by placing
the arguments inside the parenthesis following the convention
defined by `org-babel-lob'.  For example

<<example-block(a=9)>>

would set the value of argument \"a\" equal to \"9\".  Note that
these arguments are not evaluated in the current source-code
block but are passed literally to the \"example-block\"."
  (let* ((parent-buffer (or parent-buffer (current-buffer)))
         (info (or info (org-babel-get-src-block-info 'light)))
         (lang (nth 0 info))
         (body (nth 1 info))
	 (ob-nww-start org-babel-noweb-wrap-start)
	 (ob-nww-end org-babel-noweb-wrap-end)
	 (comment (string= "noweb" (cdr (assq :comments (nth 2 info)))))
	 (rx-prefix (concat "\\(" org-babel-src-name-regexp "\\|"
			    ":noweb-ref[ \t]+" "\\)"))
         (new-body "")
	 (nb-add (lambda (text) (setq new-body (concat new-body text))))
	 (c-wrap (lambda (text)
		   (with-temp-buffer
		     (funcall (intern (concat lang "-mode")))
		     (comment-region (point) (progn (insert text) (point)))
		     (org-trim (buffer-string)))))
	 index source-name evaluate prefix)
    (with-temp-buffer
      (setq-local org-babel-noweb-wrap-start ob-nww-start)
      (setq-local org-babel-noweb-wrap-end ob-nww-end)
      (insert body) (goto-char (point-min))
      (setq index (point))
      (while (and (re-search-forward (org-babel-noweb-wrap) nil t))
	(save-match-data (setf source-name (match-string 1)))
	(save-match-data (setq evaluate (string-match "(.*)" source-name)))
	(save-match-data
	  (setq prefix
		(buffer-substring (match-beginning 0)
				  (save-excursion
				    (beginning-of-line 1) (point)))))
	;; add interval to new-body (removing noweb reference)
	(goto-char (match-beginning 0))
	(funcall nb-add (buffer-substring index (point)))
	(goto-char (match-end 0))
	(setq index (point))
	(funcall
         nb-add
         (with-current-buffer parent-buffer
           (save-restriction
             (widen)
             (mapconcat ;; Interpose PREFIX between every line.
              #'identity
              (split-string
               (if evaluate
                   (let ((raw (org-babel-ref-resolve source-name)))
                     (if (stringp raw) raw (format "%S" raw)))
                 (or
                  ;; Retrieve from the library of babel.
                  (nth 2 (assoc (intern source-name)
                                org-babel-library-of-babel))
                  ;; Return the contents of headlines literally.
                  (save-excursion
                    (when (org-babel-ref-goto-headline-id source-name)
			      (org-babel-ref-headline-body)))
                  ;; Find the expansion of reference in this buffer.
                  (let ((rx (concat rx-prefix source-name "[ \t\n]"))
                        expansion)
                    (save-excursion
                      (goto-char (point-min))
                      (if org-babel-use-quick-and-dirty-noweb-expansion
                          (while (re-search-forward rx nil t)
                            (let* ((i (org-babel-get-src-block-info 'light))
                                   (body (org-babel-expand-noweb-references i))
                                   (sep (or (cdr (assq :noweb-sep (nth 2 i)))
                                            "\n"))
                                   (full (if comment
                                             (let ((cs (org-babel-tangle-comment-links i)))
                                                (concat (funcall c-wrap (car cs)) "\n"
                                                        body "\n"
                                                        (funcall c-wrap (cadr cs))))
                                           body)))
                              (setq expansion (cons sep (cons full expansion)))))
                        (org-babel-map-src-blocks nil
			  (let ((i (org-babel-get-src-block-info 'light)))
                            (when (equal (or (cdr (assq :noweb-ref (nth 2 i)))
                                             (nth 4 i))
                                         source-name)
                              (let* ((body (org-babel-expand-noweb-references i))
                                     (sep (or (cdr (assq :noweb-sep (nth 2 i)))
                                              "\n"))
                                     (full (if comment
                                               (let ((cs (org-babel-tangle-comment-links i)))
                                                  (concat (funcall c-wrap (car cs)) "\n"
                                                          body "\n"
                                                          (funcall c-wrap (cadr cs))))
                                             body)))
                                (setq expansion
                                      (cons sep (cons full expansion)))))))))
                    (and expansion
                         (mapconcat #'identity (nreverse (cdr expansion)) "")))
                  ;; Possibly raise an error if named block doesn't exist.
                  (if (or org-babel-noweb-error-all-langs
			  (member lang org-babel-noweb-error-langs))
                      (error "%s" (concat
                                   (org-babel-noweb-wrap source-name)
                                   "could not be resolved (see "
                                   "`org-babel-noweb-error-langs')"))
                    "")))
               "[\n\r]") (concat "\n" prefix))))))
      (funcall nb-add (buffer-substring index (point-max))))
    new-body))

(defun org-babel--script-escape-inner (str)
  (let (in-single in-double backslash out)
    (mapc
     (lambda (ch)
       (setq
	out
	(if backslash
	    (progn
	      (setq backslash nil)
	      (cond
	       ((and in-single (eq ch ?'))
		;; Escaped single quote inside single quoted string:
		;; emit just a single quote, since we've changed the
		;; outer quotes to double.
		(cons ch out))
	       ((eq ch ?\")
		;; Escaped double quote
		(if in-single
		    ;; This should be interpreted as backslash+quote,
		    ;; not an escape.  Emit a three backslashes
		    ;; followed by a quote (because one layer of
		    ;; quoting will be stripped by `org-babel-read').
		    (append (list ch ?\\ ?\\ ?\\) out)
		  ;; Otherwise we are in a double-quoted string.  Emit
		  ;; a single escaped quote
		  (append (list ch ?\\) out)))
	       ((eq ch ?\\)
		;; Escaped backslash: emit a single escaped backslash
		(append (list ?\\ ?\\) out))
	       ;; Other: emit a quoted backslash followed by whatever
	       ;; the character was (because one layer of quoting will
	       ;; be stripped by `org-babel-read').
	       (t (append (list ch ?\\ ?\\) out))))
	  (cl-case ch
	    (?\[ (if (or in-double in-single)
		     (cons ?\[ out)
		   (cons ?\( out)))
	    (?\] (if (or in-double in-single)
		     (cons ?\] out)
		   (cons ?\) out)))
	    (?\{ (if (or in-double in-single)
		     (cons ?\{ out)
		   (cons ?\( out)))
	    (?\} (if (or in-double in-single)
		     (cons ?\} out)
		   (cons ?\) out)))
	    (?, (if (or in-double in-single)
		    (cons ?, out) (cons ?\s out)))
	    (?\' (if in-double
		     (cons ?\' out)
		   (setq in-single (not in-single)) (cons ?\" out)))
	    (?\" (if in-single
		     (append (list ?\" ?\\) out)
		   (setq in-double (not in-double)) (cons ?\" out)))
	    (?\\ (unless (or in-single in-double)
		   (error "Can't handle backslash outside string in `org-babel-script-escape'"))
		 (setq backslash t)
		 out)
	    (t  (cons ch out))))))
     (string-to-list str))
    (when (or in-single in-double)
      (error "Unterminated string in `org-babel-script-escape'"))
    (apply #'string (reverse out))))

(defun org-babel-script-escape (str &optional force)
  "Safely convert tables into elisp lists."
  (unless (stringp str)
    (error "`org-babel-script-escape' expects a string"))
  (let ((escaped
	 (cond
	  ((and (> (length str) 2)
		(or (and (string-equal "[" (substring str 0 1))
			 (string-equal "]" (substring str -1)))
		    (and (string-equal "{" (substring str 0 1))
			 (string-equal "}" (substring str -1)))
		    (and (string-equal "(" (substring str 0 1))
			 (string-equal ")" (substring str -1)))))

	   (concat "'" (org-babel--script-escape-inner str)))
	  ((or force
	       (and (> (length str) 2)
		    (or (and (string-equal "'" (substring str 0 1))
			     (string-equal "'" (substring str -1)))
			;; We need to pass double-quoted strings
			;; through the backslash-twiddling bits, even
			;; though we don't need to change their
			;; delimiters.
			(and (string-equal "\"" (substring str 0 1))
			     (string-equal "\"" (substring str -1))))))
	   (org-babel--script-escape-inner str))
	  (t str))))
    (condition-case nil (org-babel-read escaped) (error escaped))))

(defun org-babel-read (cell &optional inhibit-lisp-eval)
  "Convert the string value of CELL to a number if appropriate.
Otherwise if CELL looks like lisp (meaning it starts with a
\"(\", \"\\='\", \"\\=`\" or a \"[\") then read and evaluate it as
lisp, otherwise return it unmodified as a string.  Optional
argument INHIBIT-LISP-EVAL inhibits lisp evaluation for
situations in which is it not appropriate."
  (cond ((not (org-string-nw-p cell)) cell)
	((org-babel--string-to-number cell))
	((and (not inhibit-lisp-eval)
	      (or (memq (string-to-char cell) '(?\( ?' ?` ?\[))
		  (string= cell "*this*")))
	 (eval (read cell) t))
	((eq (string-to-char cell) ?\") (read cell))
	(t (org-no-properties cell))))

(defun org-babel--string-to-number (string)
  "If STRING represents a number return its value.
Otherwise return nil."
  (and (string-match-p "\\`-?[0-9]*\\.?[0-9]*\\'" string)
       (string-to-number string)))

(defun org-babel-import-elisp-from-file (file-name &optional separator)
  "Read the results located at FILE-NAME into an elisp table.
If the table is trivial, then return it as a scalar."
  (let (result)
    (save-window-excursion
      (with-temp-buffer
	(condition-case err
	    (progn
	      (org-table-import file-name separator)
	      (delete-file file-name)
	      (setq result (mapcar (lambda (row)
				     (mapcar #'org-babel-string-read row))
				   (org-table-to-lisp))))
	  (error (message "Error reading results: %s" err) nil)))
      (if (null (cdr result)) ;; if result is trivial vector, then scalarize it
	  (if (consp (car result))
	      (if (null (cdr (car result)))
		  (caar result)
		result)
	    (car result))
	result))))

(defun org-babel-string-read (cell)
  "Strip nested \"s from around strings."
  (org-babel-read (or (and (stringp cell)
                           (string-match "\\\"\\(.+\\)\\\"" cell)
                           (match-string 1 cell))
                      cell) t))

(defun org-babel-chomp (string &optional regexp)
  "Strip a trailing space or carriage return from STRING.
The default regexp used is \"[ \\f\\t\\n\\r\\v]\" but another one
can be specified as the REGEXP argument."
  (let ((regexp (or regexp "[ \f\t\n\r\v]")))
    (while (and (> (length string) 0)
                (string-match regexp (substring string -1)))
      (setq string (substring string 0 -1)))
    string))

(defun org-babel-process-file-name (name &optional no-quote-p)
  "Prepare NAME to be used in an external process.
If NAME specifies a remote location, the remote portion of the
name is removed, since in that case the process will be executing
remotely.  The file name is then processed by `expand-file-name'.
Unless second argument NO-QUOTE-P is non-nil, the file name is
additionally processed by `shell-quote-argument'"
  (let ((f (org-babel-local-file-name (expand-file-name name))))
    (if no-quote-p f (shell-quote-argument f))))

(defvar org-babel-temporary-directory)
(unless (or noninteractive (boundp 'org-babel-temporary-directory))
  (defvar org-babel-temporary-directory
    (or (and (boundp 'org-babel-temporary-directory)
	     (file-exists-p org-babel-temporary-directory)
	     org-babel-temporary-directory)
	(make-temp-file "babel-" t))
    "Directory to hold temporary files created to execute code blocks.
Used by `org-babel-temp-file'.  This directory will be removed on
Emacs shutdown."))

(defcustom org-babel-remote-temporary-directory "/tmp/"
  "Directory to hold temporary files on remote hosts."
  :group 'org-babel
  :type 'string)

(defmacro org-babel-result-cond (result-params scalar-form &rest table-forms)
  "Call the code to parse raw string results according to RESULT-PARAMS."
  (declare (indent 1)
	   (debug (form form &rest form)))
  (org-with-gensyms (params)
    `(let ((,params ,result-params))
       (unless (member "none" ,params)
	 (if (or (member "scalar" ,params)
		 (member "verbatim" ,params)
		 (member "html" ,params)
		 (member "code" ,params)
		 (member "pp" ,params)
		 (member "file" ,params)
		 (and (or (member "output" ,params)
			  (member "raw"    ,params)
			  (member "org"    ,params)
			  (member "drawer" ,params))
		      (not (member "table" ,params))))
	     ,scalar-form
	   ,@table-forms)))))
(def-edebug-spec org-babel-result-cond (form form body))

(defun org-babel-temp-file (prefix &optional suffix)
  "Create a temporary file in the `org-babel-temporary-directory'.
Passes PREFIX and SUFFIX directly to `make-temp-file' with the
value of `temporary-file-directory' temporarily set to the value
of `org-babel-temporary-directory'."
  (if (file-remote-p default-directory)
      (let ((prefix
             (concat (file-remote-p default-directory)
                     (expand-file-name
		      prefix org-babel-remote-temporary-directory))))
        (make-temp-file prefix nil suffix))
    (let ((temporary-file-directory
	   (or (and (boundp 'org-babel-temporary-directory)
		    (file-exists-p org-babel-temporary-directory)
		    org-babel-temporary-directory)
	       temporary-file-directory)))
      (make-temp-file prefix nil suffix))))

(defun org-babel-remove-temporary-directory ()
  "Remove `org-babel-temporary-directory' on Emacs shutdown."
  (when (and (boundp 'org-babel-temporary-directory)
	     (file-exists-p org-babel-temporary-directory))
    ;; taken from `delete-directory' in files.el
    (condition-case nil
	(progn
	  (mapc (lambda (file)
		  ;; This test is equivalent to
		  ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
		  ;; but more efficient
		  (if (eq t (car (file-attributes file)))
		      (delete-directory file)
		    (delete-file file)))
		;; We do not want to delete "." and "..".
		(directory-files org-babel-temporary-directory 'full
				 "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"))
	  (delete-directory org-babel-temporary-directory))
      (error
       (message "Failed to remove temporary Org-babel directory %s"
		(if (boundp 'org-babel-temporary-directory)
		    org-babel-temporary-directory
		  "[directory not defined]"))))))

(add-hook 'kill-emacs-hook 'org-babel-remove-temporary-directory)

(defun org-babel-one-header-arg-safe-p (pair safe-list)
  "Determine if the PAIR is a safe babel header arg according to SAFE-LIST.

For the format of SAFE-LIST, see `org-babel-safe-header-args'."
  (and (consp pair)
       (keywordp (car pair))
       (stringp (cdr pair))
       (or
	(memq (car pair) safe-list)
	(let ((entry (assq (car pair) safe-list)))
	  (and entry
	       (consp entry)
	       (cond ((functionp (cdr entry))
		       (funcall (cdr entry) (cdr pair)))
		     ((listp (cdr entry))
		      (member (cdr pair) (cdr entry)))
		     (t nil)))))))

(defun org-babel-generate-file-param (src-name params)
  "Calculate the filename for source block results.

The directory is calculated from the :output-dir property of the
source block; if not specified, use the current directory.

If the source block has a #+NAME and the :file parameter does not
contain any period characters, then the :file parameter is
treated as an extension, and the output file name is the
concatenation of the directory (as calculated above), the block
name, a period, and the parameter value as a file extension.
Otherwise, the :file parameter is treated as a full file name,
and the output file name is the directory (as calculated above)
plus the parameter value."
  (let* ((file-cons (assq :file params))
	   (file-ext-cons (assq :file-ext params))
	   (file-ext (cdr-safe file-ext-cons))
	   (dir (cdr-safe (assq :output-dir params)))
	   fname)
    ;; create the output-dir if it does not exist
    (when dir
      (make-directory dir t))
    (if file-cons
	;; :file given; add :output-dir if given
	(when dir
	  (setcdr file-cons (concat (file-name-as-directory dir) (cdr file-cons))))
      ;; :file not given; compute from name and :file-ext if possible
      (when (and src-name file-ext)
	(if dir
	    (setq fname (concat (file-name-as-directory (or dir ""))
				src-name "." file-ext))
	  (setq fname (concat src-name "." file-ext)))
	(setq params (cons (cons :file fname) params))))
    params))

(defun org-babel-graphical-output-file (params)
  "File where a babel block should send graphical output, per PARAMS.
Return nil if no graphical output is expected.  Raise an error if
the output file is ill-defined."
  (let ((file (cdr (assq :file params))))
    (cond (file (and (member "graphics" (cdr (assq :result-params params)))
		     file))
	  ((assq :file-ext params)
	   (user-error ":file-ext given but no :file generated; did you forget \
to name a block?"))
	  (t (user-error "No :file header argument given; cannot create \
graphical result")))))

(defun org-babel-make-language-alias (new old)
  "Make source blocks of type NEW aliases for those of type OLD.

NEW and OLD should be strings.  This function should be called
after the babel API for OLD-type source blocks is fully defined.

Callers of this function will probably want to add an entry to
`org-src-lang-modes' as well."
  (dolist (fn '("execute" "expand-body" "prep-session"
		"variable-assignments" "load-session"))
    (let ((sym (intern-soft (concat "org-babel-" fn ":" old))))
      (when (and sym (fboundp sym))
	(defalias (intern (concat "org-babel-" fn ":" new)) sym))))
  ;; Technically we don't need a `dolist' for just one variable, but
  ;; we keep it for symmetry/ease of future expansion.
  (dolist (var '("default-header-args"))
    (let ((sym (intern-soft (concat "org-babel-" var ":" old))))
      (when (and sym (boundp sym))
	(defvaralias (intern (concat "org-babel-" var ":" new)) sym)))))

(defun org-babel-strip-quotes (string)
  "Strip \\\"s from around a string, if applicable."
  (org-unbracket-string "\"" "\"" string))

(provide 'ob-core)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ob-core.el ends here
