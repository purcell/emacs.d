;;; erc-compat.el --- ERC compatibility code for XEmacs

;; Copyright (C) 2002, 2003, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/ERC

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This mostly defines stuff that cannot be worked around easily.

;;; Code:

;; erc-define-minor-mode: the easy-mmode-define-minor-mode available
;; in XEmacs' easy-mmode.el does not have the BODY argument.  This
;; code has to work, even if somebody has defaliased
;; easy-mmode-define-minor-mode to define-minor-mode.  The code runs a
;; test first, and if define-minor-mode works, it uninterns all the
;; symbols created, so nothing should be left behind.

;;;###autoload (autoload 'erc-define-minor-mode "erc-compat")
(condition-case nil
    (progn
      (define-minor-mode erc-compat-test "Testing `define-minor-mode'." nil nil nil (ignore))
      (mapc 'unintern (apropos-internal "^erc-compat-test"))
      (defalias 'erc-define-minor-mode 'define-minor-mode)
      (put 'erc-define-minor-mode 'edebug-form-spec 'define-minor-mode))
  (error
   (defmacro erc-define-minor-mode (mode doc &optional init-value lighter
					 keymap &rest body)
     "Define a minor mode like in Emacs."
     ;; Deal with at least /some/ keywords.
     ;; the rest don't seem to be as important.
     (let (keyw globalp group)
       (while (keywordp (setq keyw (car body)))
	 (setq body (cdr body))
	 (case keyw
	   (:global (setq globalp (pop body)))
	   (:group (setq group (pop body)))
	   (t (pop body))))
       `(progn
	  (if ,group
	      (defcustom ,mode ,init-value
		"Non-nil if the corresponding mode is enabled."
		:group ,group
		:type 'boolean)
	    (defvar ,mode ,init-value
	      "Non-nil if the corresponding mode is enabled."))
	  (unless ,globalp
	    (make-variable-buffer-local ',mode))
	  (defun ,mode (&optional arg)
	    ,doc
	    (interactive)
	    (setq ,mode (if arg
			    (> (prefix-numeric-value arg) 0)
			  (not ,mode)))
	    ,@body
	    ,mode)
	  (add-minor-mode ,mode ,lighter ,keymap))))
   (put 'erc-define-minor-mode 'edebug-form-spec
	'(&define name stringp
		  [&optional sexp sexp &or consp symbolp]
		  [&rest
		   [keywordp sexp]]
		  def-body))
   ))

;; MULE: decode-coding-string and encode-coding-string -- note that
;; XEmacs' functions do not have the NOCOPY argument.

;; latin-1 is only available as iso-8859-1 on XEmacs.  Since that
;; works for both, we will use that.

(condition-case nil
    ;; Try 3 arguments
    (progn
      (decode-coding-string "a" 'iso-8859-1 t)
      (defun erc-decode-coding-string (s coding-system)
	"Decode S using CODING-SYSTEM."
	(decode-coding-string s coding-system t)))
  (error
   (condition-case nil
       ;; Try 2 arguments
       (progn
	 (decode-coding-string "a" 'iso-8859-1)
	 (defun erc-decode-coding-string (s coding-system)
	   "Decode S using CODING-SYSTEM."
	   (decode-coding-string s coding-system)))
     (error
      ;; Default
      (defun erc-decode-coding-string (s &rest ignore)
	"Return S."
	s)))))

(condition-case nil
    ;; Try 3 arguments
    (progn
      (encode-coding-string "a" 'iso-8859-1 t)
      (defun erc-encode-coding-string (s coding-system)
	"Encode S using CODING-SYSTEM.
Return the same string, if the encoding operation is trivial.
See `erc-encoding-coding-alist'."
	(encode-coding-string s coding-system t)))
  (error
   (condition-case nil
       ;; Try 2 arguments
       (progn
	 (encode-coding-string "a" 'iso-8859-1)
	 (defun erc-encode-coding-string (s coding-system)
	   "Encode S using CODING-SYSTEM.
See `erc-encoding-coding-alist'."
	   (encode-coding-string s coding-system)))
     (error
      ;; Default
      (defun erc-encode-coding-string (s &rest ignore)
	"Return S unchanged."
	s)))))

(if (not (fboundp 'propertize))
    (defun erc-propertize (string &rest props)
      (let ((string (copy-sequence string)))
	(while props
	  (put-text-property 0 (length string)
			     (nth 0 props) (nth 1 props) string)
	  (setq props (cddr props)))
	string))
  (defalias 'erc-propertize 'propertize))

;;; XEmacs does not have `view-mode-enter', but its `view-mode' has a
;;; similar argument list.  And we need this in erc-match.el.

;; Emacs view-mode-enter: (view-mode-enter &optional RETURN-TO
;; EXIT-ACTION)

;; XEmacs view-mode: (view-mode &optional PREV-BUFFER EXIT-ACTION
;; CLEAN-BS)

;; But note Emacs view-mode: (view-mode &optional ARG)

(defun erc-view-mode-enter (&optional return-to exit-action)
  "Enter View mode.
See either `view-mode-enter' (if using Emacs)
or `view-mode' (if using XEmacs)
to determine what the arguments accomplish.

If we cannot find a suitable way of passing the arguments, we
default to just entering View mode."
  (cond ((fboundp 'view-mode-enter)
	 (view-mode-enter return-to exit-action))
	((featurep 'xemacs)
	 (condition-case nil
	     (view-mode return-to exit-action)
	   (view-mode 1)))
	(t nil)))

;; if we're in emacs21 CVS, we use help-function-arglist which is more
;; sophisticated and can handle subrs, etc
(if (fboundp 'help-function-arglist)
    (defalias 'erc-function-arglist 'help-function-arglist)
  (defun erc-function-arglist (fun)
    "Returns the arglist signature of FUN"
    (let ((def (symbol-function fun)))
      (ignore-errors
	;; load an autoloaded function first
	(when (equal 'autoload (car-safe def))
	  (load (second def))
	  (setq def (symbol-function fun)))
	(if (listp def)
	    (second def)
	  (format "[Arglist not available, try %s instead]"
		  (substitute-command-keys "\\[describe-function]")))))))

;; XEmacs doesn't have `delete-dups'.  Taken from subr.el.
(if (fboundp 'delete-dups)
    (defalias 'erc-delete-dups 'delete-dups)
  (defun erc-delete-dups (list)
    "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept."
    (let ((tail list))
      (while tail
	(setcdr tail (delete (car tail) (cdr tail)))
	(setq tail (cdr tail))))
    list))

;;; XEmacs has `replace-in-string', Emacs has `replace-regexp-in-string':

(cond ((fboundp 'replace-regexp-in-string)
       (defalias 'erc-replace-regexp-in-string 'replace-regexp-in-string))
      ((fboundp 'replace-in-string)
       (defun erc-replace-regexp-in-string (regexp rep string &optional fixedcase literal)
         (replace-in-string string regexp rep literal))))

;;; Several different Emacsen have different variables for setting
;;; write hooks.
(cond ((boundp 'write-file-functions)
       (defun erc-set-write-file-functions (new-val)
	 (set (make-local-variable 'write-file-functions) new-val)))
      ((boundp 'local-write-file-hooks)
       (defun erc-set-write-file-functions (new-val)
	 (setq local-write-file-hooks new-val)))
      (t
       (defun erc-set-write-file-functions (new-val)
	 (set (make-local-variable 'write-file-hooks) new-val))))

;;; Done!

;; XEmacs has a string representation of the build time.  It's
;; possible for date-to-time to throw an "invalid date" error, so
;; we'll just use a string instead of a time.
(defvar erc-emacs-build-time
  (if (stringp emacs-build-time)
      emacs-build-time
    (format-time-string "%Y-%m-%d" emacs-build-time))
  "Time at which Emacs was dumped out.")

;; Emacs 21 and XEmacs do not have user-emacs-directory, but XEmacs
;; has user-init-directory.
(defvar erc-user-emacs-directory
  (cond ((boundp 'user-emacs-directory)
	 user-emacs-directory)
	((boundp 'user-init-directory)
	 user-init-directory)
	(t "~/.emacs.d/"))
  "Directory beneath which additional per-user Emacs-specific files
are placed.
Note that this should end with a directory separator.")

;; XEmacs' `replace-match' does not replace matching subexpressions in strings.
(defun erc-replace-match-subexpression-in-string
  (newtext string match subexp start &optional fixedcase literal)
  "Replace the subexpression SUBEXP of the last match in STRING with NEWTEXT.
MATCH is the text which matched the subexpression (see `match-string').
START is the beginning position of the last match (see `match-beginning').
See `replace-match' for explanations of FIXEDCASE and LITERAL."
  (cond ((featurep 'xemacs)
	 (string-match match string start)
	 (replace-match newtext fixedcase literal string))
	(t (replace-match newtext fixedcase literal string subexp))))

;; If a version of Emacs or XEmacs does not have gnus or tramp, they
;; will not have the format-spec library.  We deal with this by
;; providing copies of its functions if the library is not available.
(condition-case nil
    (require 'format-spec)
  (error
   (defun format-spec (format specification)
     "Return a string based on FORMAT and SPECIFICATION.
FORMAT is a string containing `format'-like specs like \"bash %u %k\",
while SPECIFICATION is an alist mapping from format spec characters
to values."
     (with-temp-buffer
       (insert format)
       (goto-char (point-min))
       (while (search-forward "%" nil t)
	 (cond
	  ;; Quoted percent sign.
	  ((eq (char-after) ?%)
	   (delete-char 1))
	  ;; Valid format spec.
	  ((looking-at "\\([-0-9.]*\\)\\([a-zA-Z]\\)")
	   (let* ((num (match-string 1))
		  (spec (string-to-char (match-string 2)))
		  (val (cdr (assq spec specification))))
	     (delete-region (1- (match-beginning 0)) (match-end 0))
	     (unless val
	       (error "Invalid format character: %s" spec))
	     (insert (format (concat "%" num "s") val))))
	  ;; Signal an error on bogus format strings.
	  (t
	   (error "Invalid format string"))))
       (buffer-string)))

   (defun format-spec-make (&rest pairs)
     "Return an alist suitable for use in `format-spec' based on PAIRS.
PAIRS is a list where every other element is a character and a value,
starting with a character."
     (let (alist)
       (while pairs
	 (unless (cdr pairs)
	   (error "Invalid list of pairs"))
	 (push (cons (car pairs) (cadr pairs)) alist)
	 (setq pairs (cddr pairs)))
       (nreverse alist)))))

;; Emacs21 does not have `with-selected-window', but Emacs22 and
;; XEmacs do.
(if (or (fboundp 'with-selected-window)
	(condition-case nil
	    (progn
	      (require 'window)
	      (fboundp 'with-selected-window))
	  (error nil)))
    (defmacro erc-with-selected-window (window &rest body)
      "Execute the forms in BODY with WINDOW as the selected window.
The value returned is the value of the last form in BODY."
      (cons 'with-selected-window (cons window body)))
  ;; ripped from subr.el in Emacs 22
  (defmacro erc-with-selected-window (window &rest body)
    "Execute the forms in BODY with WINDOW as the selected window.
The value returned is the value of the last form in BODY."
    `(let ((save-selected-window-window (selected-window))
	   (save-selected-window-alist
	    (mapcar (lambda (frame) (list frame (frame-selected-window frame)))
		    (frame-list))))
       (save-current-buffer
	 (unwind-protect
	     (progn (select-window ,window 'norecord)
		    ,@body)
	   (dolist (elt save-selected-window-alist)
	     (and (frame-live-p (car elt))
		  (window-live-p (cadr elt))
		  (set-frame-selected-window (car elt) (cadr elt))))
	   (if (window-live-p save-selected-window-window)
	       (select-window save-selected-window-window 'norecord)))))))

(put 'erc-with-selected-window 'lisp-indent-function 1)
(put 'erc-with-selected-window 'edebug-form-spec '(form body))

;; Emacs has `cancel-timer', but XEmacs uses `delete-itimer'.
(defun erc-cancel-timer (timer)
  (cond ((fboundp 'cancel-timer)
	 (cancel-timer timer))
	((fboundp 'delete-itimer)
	 (delete-itimer timer))
	(t
	 (error "Cannot find `cancel-timer' variant"))))

;; Emacs accepts three arguments to `make-obsolete', `make-obsolete-variable'
;; XEmacs only takes two arguments
(defun erc-make-obsolete (old-name new-name when)
  "Make the byte-compiler warn that OLD-NAME is obsolete.
The warning will say that NEW-NAME should be used instead.
WHEN should be a string indicating when the function was
first made obsolete, either the file's revision number or an
ERC release version number."
  (condition-case nil
      (make-obsolete old-name new-name when)
    (wrong-number-of-arguments (make-obsolete old-name new-name))))

(defun erc-make-obsolete-variable (old-name new-name when)
  "Make the byte-compiler warn that OLD-NAME is obsolete.
The warning will say that NEW-NAME should be used instead.
WHEN should be a string indicating when the variable was
first made obsolete, either the file's revision number or an
ERC release version number."
  (condition-case nil
      (make-obsolete-variable old-name new-name when)
    (wrong-number-of-arguments (make-obsolete-variable old-name new-name))))

;; Provide a simpler replacement for `member-if'
(defun erc-member-if (predicate list)
  "Find the first item satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches."
  (let ((ptr list))
    (catch 'found
      (while ptr
	(when (funcall predicate (car ptr))
	  (throw 'found ptr))
	(setq ptr (cdr ptr))))))

;; Provide a simpler replacement for `delete-if'
(defun erc-delete-if (predicate seq)
  "Remove all items satisfying PREDICATE in SEQ.
This is a destructive function: it reuses the storage of SEQ
whenever possible."
  ;; remove from car
  (while (when (funcall predicate (car seq))
	   (setq seq (cdr seq))))
  ;; remove from cdr
  (let ((ptr seq)
	(next (cdr seq)))
    (while next
      (when (funcall predicate (car next))
	(setcdr ptr (if (consp next)
			(cdr next)
		      nil)))
      (setq ptr (cdr ptr))
      (setq next (cdr ptr))))
  seq)

;; Provide a simpler replacement for `remove-if-not'
(defun erc-remove-if-not (predicate seq)
  "Remove all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ to
avoid corrupting the original SEQ."
  (let (newseq)
    (dolist (el seq)
      (when (funcall predicate el)
	(setq newseq (cons el newseq))))
    (nreverse newseq)))

;; Copied from cl-extra.el
(defun erc-subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (if (stringp seq) (substring seq start end)
    (let (len)
      (and end (< end 0) (setq end (+ end (setq len (length seq)))))
      (if (< start 0) (setq start (+ start (or len (setq len (length seq))))))
      (cond ((listp seq)
	     (if (> start 0) (setq seq (nthcdr start seq)))
	     (if end
		 (let ((res nil))
		   (while (>= (setq end (1- end)) start)
		     (push (pop seq) res))
		   (nreverse res))
	       (copy-sequence seq)))
	    (t
	     (or end (setq end (or len (length seq))))
	     (let ((res (make-vector (max (- end start) 0) nil))
		   (i 0))
	       (while (< start end)
		 (aset res i (aref seq start))
		 (setq i (1+ i) start (1+ start)))
	       res))))))

(provide 'erc-compat)

;;; erc-compat.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

;; arch-tag: 8948ffe0-aff8-4ad8-a196-368ebbfd58ff
