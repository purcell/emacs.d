;;; info-look.el --- major-mode-sensitive Info index lookup facility.
;; An older version of this was known as libc.el.

;; Copyright (C) 1995, 1996, 1997, 1998 Free Software Foundation, Inc.

;; Author: Ralph Schleicher <rs@purple.UL.BaWue.DE>
;; Keywords: help languages

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'info)
(eval-and-compile
  (condition-case nil
      (require 'custom)
    (error
     (defmacro defgroup (&rest arg)
       nil)
     (defmacro defcustom (symbol value doc &rest arg)
       `(defvar ,symbol ,value ,doc ,@arg)))))

(defgroup info-lookup nil
  "Major mode sensitive help agent."
  :group 'help :group 'languages)

(defvar info-lookup-mode nil
  "Symbol of the current buffer's help mode.
Help is provided according to the buffer's major mode if value is nil.
Automatically becomes buffer local when set in any fashion.")
(make-variable-buffer-local 'info-lookup-mode)

(defcustom info-lookup-other-window-flag t
  "Non-nil means pop up the Info buffer in another window."
  :group 'info-lookup :type 'boolean)

(defcustom info-lookup-highlight-face 'highlight
  "Face for highlighting looked up help items.
Setting this variable to nil disables highlighting."
  :group 'info-lookup :type 'face)

(defvar info-lookup-highlight-overlay nil
  "Overlay object used for highlighting.")

(defcustom info-lookup-file-name-alist
  '(("\\`configure\\.in\\'" . autoconf-mode)
    ("\\`aclocal\\.m4\\'" . autoconf-mode)
    ("\\`acsite\\.m4\\'" . autoconf-mode)
    ("\\`acinclude\\.m4\\'" . autoconf-mode))
  "Alist of file names handled specially.
List elements are cons cells of the form

    (REGEXP . MODE)

If a file name matches REGEXP, then use help mode MODE instead of the
buffer's major mode."
  :group 'info-lookup :type '(repeat (cons (string :tag "Regexp")
					   (symbol :tag "Mode"))))

(defvar info-lookup-history nil
  "History of previous input lines.")

(defvar info-lookup-alist nil
  "Alist of known help topics.
Cons cells are of the form

    (HELP-TOPIC . HELP-DATA)

HELP-TOPIC is the symbol of a help topic.
HELP-DATA is a HELP-TOPIC's public data set.
 Value is an alist with elements of the form

    (HELP-MODE REGEXP IGNORE-CASE DOC-SPEC PARSE-RULE OTHER-MODES)

HELP-MODE is a mode's symbol.
REGEXP is a regular expression matching those help items whose
 documentation can be looked up via DOC-SPEC.
IGNORE-CASE is non-nil if help items are case insensitive.
DOC-SPEC is a list of documentation specifications of the form

    (INFO-NODE TRANS-FUNC PREFIX SUFFIX)

INFO-NODE is the name (including file name part) of an Info index.
TRANS-FUNC is a function translating index entries into help items;
 nil means add only those index entries matching REGEXP, a string
 means prepend string to the first word of all index entries.
PREFIX and SUFFIX are parts of a regular expression.  If one of
 them is non-nil then search the help item's Info node for the
 first occurrence of the regular expression `PREFIX ITEM SUFFIX'.
 ITEM will be highlighted with `info-lookup-highlight-face' if this
 variable is not nil.
PARSE-RULE is either the symbol name of a function or a regular
 expression for guessing the default help item at point.  Fuzzy
 regular expressions like \"[_a-zA-Z0-9]+\" do a better job if
 there are no clear delimiters; do not try to write too complex
 expressions.  PARSE-RULE defaults to REGEXP.
OTHER-MODES is a list of cross references to other help modes.")

(defsubst info-lookup->topic-value (topic)
  (cdr (assoc topic info-lookup-alist)))

(defsubst info-lookup->mode-value (topic mode)
  (assoc mode (info-lookup->topic-value topic)))

(defsubst info-lookup->regexp (topic mode)
  (nth 1 (info-lookup->mode-value topic mode)))

(defsubst info-lookup->ignore-case (topic mode)
  (nth 2 (info-lookup->mode-value topic mode)))

(defsubst info-lookup->doc-spec (topic mode)
  (nth 3 (info-lookup->mode-value topic mode)))

(defsubst info-lookup->parse-rule (topic mode)
  (nth 4 (info-lookup->mode-value topic mode)))

(defsubst info-lookup->other-modes (topic mode)
  (nth 5 (info-lookup->mode-value topic mode)))

(eval-and-compile
  (mapcar (lambda (keyword)
	    (or (boundp keyword)
		(set keyword keyword)))
	  '(:topic :mode :regexp :ignore-case
	    :doc-spec :parse-rule :other-modes)))

(defun info-lookup-add-help (&rest arg)
  "Add or update a help specification.
Function arguments are one or more options of the form

    KEYWORD ARGUMENT

KEYWORD is either `:topic', `:mode', `:regexp', `:ignore-case',
 `:doc-spec', `:parse-rule', or `:other-modes'.
ARGUMENT has a value as explained in the documentation of the
 variable `info-lookup-alist'.

If no topic or mode option has been specified, then the help topic defaults
to `symbol', and the help mode defaults to the current major mode."
  (apply 'info-lookup-add-help* nil arg))

(defun info-lookup-maybe-add-help (&rest arg)
  "Add a help specification iff no one is defined.
See the documentation of the function `info-lookup-add-help'
for more details."
  (apply 'info-lookup-add-help* t arg))

(defun info-lookup-add-help* (maybe &rest arg)
  (let (topic mode regexp ignore-case doc-spec
	      parse-rule other-modes keyword value)
    (setq topic 'symbol
	  mode major-mode
	  regexp "\\w+")
    (while arg
      (setq keyword (car arg))
      (or (symbolp keyword)
	  (error "Junk in argument list \"%S\"" arg))
      (setq arg (cdr arg))
      (and (null arg)
	   (error "Keyword \"%S\" is missing an argument" keyword))
      (setq value (car arg)
	    arg (cdr arg))
      (cond ((eq keyword :topic)
	     (setq topic value))
	    ((eq keyword :mode)
	     (setq mode value))
	    ((eq keyword :regexp)
	     (setq regexp value))
	    ((eq keyword :ignore-case)
	     (setq ignore-case value))
	    ((eq keyword :doc-spec)
	     (setq doc-spec value))
	    ((eq keyword :parse-rule)
	     (setq parse-rule value))
	    ((eq keyword :other-modes)
	     (setq other-modes value))
	    (t
	     (error "Unknown keyword \"%S\"" keyword))))
    (or (and maybe (info-lookup->mode-value topic mode))
	(let* ((data (list regexp ignore-case doc-spec parse-rule other-modes))
	       (topic-cell (or (assoc topic info-lookup-alist)
			       (car (setq info-lookup-alist
					  (cons (cons topic nil)
						info-lookup-alist)))))
	       (mode-cell (assoc mode topic-cell)))
	  (if (null mode-cell)
	      (setcdr topic-cell (cons (cons mode data) (cdr topic-cell)))
	    (setcdr mode-cell data))))
    nil))

(defvar info-lookup-cache nil
  "Cache storing data maintained automatically by the program.
Value is an alist with cons cell of the form

    (HELP-TOPIC . ((HELP-MODE INITIALIZED COMPLETIONS REFER-MODES) ...))

HELP-TOPIC is the symbol of a help topic.
HELP-MODE is a mode's symbol.
INITIALIZED is nil if HELP-MODE is uninitialized, t if
 HELP-MODE is initialized, and `0' means HELP-MODE is
 initialized but void.
COMPLETIONS is an alist of documented help items.
REFER-MODES is a list of other help modes to use.")

(defsubst info-lookup->cache (topic)
  (or (assoc topic info-lookup-cache)
      (car (setq info-lookup-cache
		 (cons (cons topic nil)
		       info-lookup-cache)))))

(defun info-lookup->topic-cache (topic)
  (cdr (info-lookup->cache topic)))

(defun info-lookup->mode-cache (topic mode)
  (assoc mode (info-lookup->topic-cache topic)))

(defun info-lookup->initialized (topic mode)
  (nth 1 (info-lookup->mode-cache topic mode)))

(defun info-lookup->completions (topic mode)
  (or (info-lookup->initialized topic mode)
      (info-lookup-setup-mode topic mode))
  (nth 2 (info-lookup->mode-cache topic mode)))

(defun info-lookup->refer-modes (topic mode)
  (or (info-lookup->initialized topic mode)
      (info-lookup-setup-mode topic mode))
  (nth 3 (info-lookup->mode-cache topic mode)))

(defun info-lookup->all-modes (topic mode)
  (cons mode (info-lookup->refer-modes topic mode)))

(defun info-lookup-quick-all-modes (topic mode)
  (cons mode (info-lookup->other-modes topic mode)))

;;;###autoload
(defun info-lookup-reset ()
  "Throw away all cached data.
This command is useful if the user wants to start at the beginning without
quitting Emacs, for example, after some Info documents were updated on the
system."
  (interactive)
  (setq info-lookup-cache nil))

;;;###autoload
(defun info-lookup-symbol (symbol &optional mode)
  "Display the documentation of a symbol.
If called interactively, SYMBOL will be read from the mini-buffer.
Prefix argument means unconditionally insert the default symbol name
into the mini-buffer so that it can be edited.
The default symbol is the one found at point."
  (interactive
   (info-lookup-interactive-arguments 'symbol))
  (info-lookup 'symbol symbol mode))

;;;###autoload
(defun info-lookup-file (file &optional mode)
  "Display the documentation of a file.
If called interactively, FILE will be read from the mini-buffer.
Prefix argument means unconditionally insert the default file name
into the mini-buffer so that it can be edited.
The default file name is the one found at point."
  (interactive
   (info-lookup-interactive-arguments 'file))
  (info-lookup 'file file mode))

(defun info-lookup-interactive-arguments (topic)
  "Return default value and help mode for help topic TOPIC."
  (let* ((mode (if (info-lookup->mode-value topic (info-lookup-select-mode))
		   info-lookup-mode
		 (info-lookup-change-mode topic)))
	 (completions (info-lookup->completions topic mode))
	 (default (info-lookup-guess-default topic mode))
	 (input (if (or current-prefix-arg (not (assoc default completions)))
		    default))
	 (completion-ignore-case (info-lookup->ignore-case topic mode))
	 (enable-recursive-minibuffers t)
	 (value (completing-read
		 (if (and default (not input))
		     (format "Describe %s (default %s): " topic default)
		   (format "Describe %s: " topic))
		 completions nil nil input 'info-lookup-history)))
    (list (if (equal value "") default value) mode)))

(defun info-lookup-select-mode ()
  (when (and (not info-lookup-mode) (buffer-file-name))
    (let ((file-name (file-name-nondirectory (buffer-file-name)))
	  (file-name-alist info-lookup-file-name-alist))
      (while (and (not info-lookup-mode) file-name-alist)
	(when (string-match (caar file-name-alist) file-name)
	  (setq info-lookup-mode (cdar file-name-alist)))
	(setq file-name-alist (cdr file-name-alist)))))
  (or info-lookup-mode (setq info-lookup-mode major-mode)))

(defun info-lookup-change-mode (topic)
  (let* ((completions (mapcar (lambda (arg)
				(cons (symbol-name (car arg)) (car arg)))
			      (info-lookup->topic-value topic)))
	 (mode (completing-read
		(format "Use %s help mode: " topic)
		completions nil t nil 'info-lookup-history)))
    (or (setq mode (cdr (assoc mode completions)))
	(error "No %s help available" topic))
    (or (info-lookup->mode-value topic mode)
	(error "No %s help available for `%s'" topic mode))
    (setq info-lookup-mode mode)))

(defun info-lookup (topic item mode)
  "Display the documentation of a help item."
  (or mode (setq mode (info-lookup-select-mode)))
  (or (info-lookup->mode-value topic mode)
      (error "No %s help available for `%s'" topic mode))
  (let ((entry (or (assoc (if (info-lookup->ignore-case topic mode)
			      (downcase item) item)
			  (info-lookup->completions topic mode))
		   (error "Not documented as a %s: %s" topic (or item ""))))
	(modes (info-lookup->all-modes topic mode))
	(window (selected-window))
	found doc-spec node prefix suffix doc-found)
    (if (not info-lookup-other-window-flag)
	(info)
      (save-window-excursion (info))
      (switch-to-buffer-other-window "*info*"))
    (while (and (not found) modes)
      (setq doc-spec (info-lookup->doc-spec topic (car modes)))
      (while (and (not found) doc-spec)
	(setq node (nth 0 (car doc-spec))
	      prefix (nth 2 (car doc-spec))
	      suffix (nth 3 (car doc-spec)))
	(when (condition-case error-data
		  (progn 
		    (Info-goto-node node)
		    (setq doc-found t))
		(error 
		 (message "Cannot access Info node %s" node)
		 (sit-for 1)
		 nil))
	  (condition-case nil
	      (progn
		(Info-menu (or (cdr entry) item))
		(setq found t)
		(if (or prefix suffix)
		    (let ((case-fold-search
			   (info-lookup->ignore-case topic (car modes)))
			  (buffer-read-only nil))
		      (goto-char (point-min))
		      (re-search-forward
		       (concat prefix (regexp-quote item) suffix))
		      (goto-char (match-beginning 0))
		      (and window-system info-lookup-highlight-face
			   ;; Search again for ITEM so that the first
			   ;; occurence of ITEM will be highlighted.
			   (re-search-forward (regexp-quote item))
			   (let ((start (match-beginning 0))
				 (end (match-end 0)))
			     (if (overlayp info-lookup-highlight-overlay)
				 (move-overlay info-lookup-highlight-overlay
					       start end (current-buffer))
			       (setq info-lookup-highlight-overlay
				     (make-overlay start end))))
			   (overlay-put info-lookup-highlight-overlay
					'face info-lookup-highlight-face)))))
	    (error nil)))
	(setq doc-spec (cdr doc-spec)))
      (setq modes (cdr modes)))
    (or doc-found
	(error "Info documentation for lookup was not found"))
    ;; Don't leave the Info buffer if the help item couldn't be looked up.
    (if (and info-lookup-other-window-flag found)
	(select-window window))))

(defun info-lookup-setup-mode (topic mode)
  "Initialize the internal data structure."
  (or (info-lookup->initialized topic mode)
      (let (cell data (initialized 0) completions refer-modes)
	(if (not (info-lookup->mode-value topic mode))
	    (message "No %s help available for `%s'" topic mode)
	  ;; Recursively setup cross references.
	  ;; But refer only to non-void modes.
	  (mapcar (lambda (arg)
		    (or (info-lookup->initialized topic arg)
			(info-lookup-setup-mode topic arg))
		    (and (eq (info-lookup->initialized topic arg) t)
			 (setq refer-modes (cons arg refer-modes))))
		  (info-lookup->other-modes topic mode))
	  (setq refer-modes (nreverse refer-modes))
	  ;; Build the full completion alist.
	  (setq completions
		(nconc (info-lookup-make-completions topic mode)
		       (apply 'append
			      (mapcar (lambda (arg)
					(info-lookup->completions topic arg))
				      refer-modes))))
	  (setq initialized t))
	;; Update `info-lookup-cache'.
	(setq cell (info-lookup->mode-cache topic mode)
	      data (list initialized completions refer-modes))
	(if (not cell)
	    (setcdr (info-lookup->cache topic)
		    (cons (cons mode data) (info-lookup->topic-cache topic)))
	  (setcdr cell data))
	initialized)))

(defun info-lookup-make-completions (topic mode)
  "Create a unique alist from all index entries."
  (let ((doc-spec (info-lookup->doc-spec topic mode))
	(regexp (concat "^\\(" (info-lookup->regexp topic mode)
			"\\)\\([ \t].*\\)?$"))
	node trans entry item prefix result doc-found
	(buffer (get-buffer-create " temp-info-look")))
    (with-current-buffer buffer
      (Info-mode))
    (while doc-spec
      (setq node (nth 0 (car doc-spec))
	    trans (cond ((eq (nth 1 (car doc-spec)) nil)
			 (lambda (arg)
			   (if (string-match regexp arg)
			       (match-string 1 arg))))
			((stringp (nth 1 (car doc-spec)))
			 (setq prefix (nth 1 (car doc-spec)))
			 (lambda (arg)
			   (if (string-match "^\\([^: \t\n]+\\)" arg)
			       (concat prefix (match-string 1 arg)))))
			(t (nth 1 (car doc-spec)))))
      (with-current-buffer buffer
	(message "Processing Info node `%s'..." node)
	(when (condition-case error-data
		  (progn 
		    (Info-goto-node node)
		    (setq doc-found t))
		(error 
		 (message "Cannot access Info node `%s'" node)
		 (sit-for 1)
		 nil))
	  (condition-case nil
	      (progn
		(goto-char (point-min))
		(and (search-forward "\n* Menu:" nil t)
		     (while (re-search-forward "\n\\* \\([^:\t\n]*\\):" nil t)
		       (setq entry (match-string 1)
			     item (funcall trans entry))
		       (and (info-lookup->ignore-case topic mode)
			    (setq item (downcase item)))
		       (and (string-equal entry item)
			    (setq entry nil))
		       (or (assoc item result)
			   (setq result (cons (cons item entry) result))))))
	    (error nil))))
      (message "Processing Info node `%s'...done" node)
      (setq doc-spec (cdr doc-spec)))
    (or doc-found
	(error "Info documentation for lookup was not found"))
    result))

(defun info-lookup-guess-default (topic mode)
  "Pick up default item at point (with favor to look back).
Return nil if there is nothing appropriate."
  (let ((modes (info-lookup->all-modes topic mode))
	(start (point)) guess whitespace)
    (while (and (not guess) modes)
      (setq guess (info-lookup-guess-default* topic (car modes))
	    modes (cdr modes))
      (goto-char start))
    ;; Collapse whitespace characters.
    (and guess (concat (delete nil (mapcar (lambda (ch)
					     (if (or (char-equal ch ? )
						     (char-equal ch ?\t)
						     (char-equal ch ?\n))
						 (if (not whitespace)
						     (setq whitespace ? ))
					       (setq whitespace nil) ch))
					   guess))))))

(defun info-lookup-guess-default* (topic mode)
  (let ((case-fold-search (info-lookup->ignore-case topic mode))
	(rule (or (info-lookup->parse-rule topic mode)
		  (info-lookup->regexp topic mode)))
	(start (point)) end regexp subexp result)
    (if (symbolp rule)
	(setq result (funcall rule))
      (if (consp rule)
	  (setq regexp (car rule)
		subexp (cdr rule))
	(setq regexp rule
	      subexp 0))
      (skip-chars-backward " \t\n") (setq end (point))
      (while (and (re-search-backward regexp nil t)
		  (looking-at regexp)
		  (>= (match-end 0) end))
	(setq result (match-string subexp)))
      (if (not result)
	  (progn
	    (goto-char start)
	    (skip-chars-forward " \t\n")
	    (and (looking-at regexp)
		 (setq result (match-string subexp))))))
    result))

(defun info-lookup-guess-c-symbol ()
  "Get the C symbol at point."
  (condition-case nil
      (progn
	(backward-sexp)
	(let ((start (point)) prefix name)
	  ;; Test for a leading `struct', `union', or `enum' keyword
	  ;; but ignore names like `foo_struct'.
	  (setq prefix (and (< (skip-chars-backward " \t\n") 0)
			    (< (skip-chars-backward "_a-zA-Z0-9") 0)
			    (looking-at "\\(struct\\|union\\|enum\\)\\s ")
			    (concat (match-string 1) " ")))
	  (goto-char start)
	  (and (looking-at "[_a-zA-Z][_a-zA-Z0-9]*")
	       (setq name (match-string 0)))
	  ;; Caveat!  Look forward if point is at `struct' etc.
	  (and (not prefix)
	       (or (string-equal name "struct")
		   (string-equal name "union")
		   (string-equal name "enum"))
	       (looking-at "[a-z]+\\s +\\([_a-zA-Z][_a-zA-Z0-9]*\\)")
	       (setq prefix (concat name " ")
		     name (match-string 1)))
	  (and (or prefix name)
	       (concat prefix name))))
    (error nil)))

;;;###autoload
(defun info-complete-symbol (&optional mode)
  "Perform completion on symbol preceding point."
  (interactive)
  (info-complete 'symbol
		 (or mode
		     (if (info-lookup->mode-value
			  'symbol (info-lookup-select-mode))
			 info-lookup-mode
		       (info-lookup-change-mode 'symbol)))))

;;;###autoload
(defun info-complete-file (&optional mode)
  "Perform completion on file preceding point."
  (interactive)
  (info-complete 'file
		 (or mode
		     (if (info-lookup->mode-value
			  'file (info-lookup-select-mode))
			 info-lookup-mode
		       (info-lookup-change-mode 'file)))))

(defun info-complete (topic mode)
  "Try to complete a help item."
  (barf-if-buffer-read-only)
  (or mode (setq mode (info-lookup-select-mode)))
  (or (info-lookup->mode-value topic mode)
      (error "No %s completion available for `%s'" topic mode))
  (let ((modes (info-lookup-quick-all-modes topic mode))
	(start (point))
	try)
    (while (and (not try) modes)
      (setq mode (car modes)
	    modes (cdr modes)
	    try (info-lookup-guess-default* topic mode))
      (goto-char start))
    (and (not try)
	 (error "Found no %S to complete" topic))
    (let ((completions (info-lookup->completions topic mode))
	  (completion-ignore-case (info-lookup->ignore-case topic mode))
	  completion)
      (setq completion (try-completion try completions))
      (cond ((not completion)
	     (ding)
	     (message "No match"))
	    ((stringp completion)
	     (or (assoc completion completions)
		 (setq completion (completing-read
				   (format "Complete %S: " topic)
				   completions nil t completion
				   info-lookup-history)))
	     (delete-region (- start (length try)) start)
	     (insert completion))
	    (t
	     (message "%s is complete"
		      (capitalize (prin1-to-string topic))))))))


;;; Initialize some common modes.

(info-lookup-maybe-add-help
 :mode 'c-mode :topic 'symbol
 :regexp "\\(struct \\|union \\|enum \\)?[_a-zA-Z][_a-zA-Z0-9]*"
 :doc-spec '(("(libc)Function Index" nil
	      "^[ \t]+- \\(Function\\|Macro\\): .*\\<" "\\>")
	     ("(libc)Variable Index" nil
	      "^[ \t]+- \\(Variable\\|Macro\\): .*\\<" "\\>")
	     ("(libc)Type Index" nil
	      "^[ \t]+- Data Type: \\<" "\\>")
	     ("(termcap)Var Index" nil
	      "^[ \t]*`" "'"))
 :parse-rule 'info-lookup-guess-c-symbol)

(info-lookup-maybe-add-help
 :mode 'c-mode :topic 'file
 :regexp "[_a-zA-Z0-9./+-]+"
 :doc-spec '(("(libc)File Index")))

(info-lookup-maybe-add-help
 :mode 'bison-mode
 :regexp "[:;|]\\|%\\([%{}]\\|[_a-z]+\\)\\|YY[_A-Z]+\\|yy[_a-z]+"
 :doc-spec '(("(bison)Index" nil
	      "`" "'"))
 :parse-rule "[:;|]\\|%\\([%{}]\\|[_a-zA-Z][_a-zA-Z0-9]*\\)"
 :other-modes '(c-mode))

(info-lookup-maybe-add-help
 :mode 'makefile-mode
 :regexp "\\$[^({]\\|\\.[_A-Z]*\\|[_a-zA-Z][_a-zA-Z0-9-]*"
 :doc-spec '(("(make)Name Index" nil
	      "^[ \t]*`" "'"))
 :parse-rule "\\$[^({]\\|\\.[_A-Z]*\\|[_a-zA-Z0-9-]+")

(info-lookup-maybe-add-help
 :mode 'texinfo-mode
 :regexp "@\\([a-zA-Z]+\\|[^a-zA-Z]\\)"
 :doc-spec '(("(texinfo)Command and Variable Index"
	      ;; Ignore Emacs commands and prepend a `@'.
	      (lambda (item)
		(if (string-match "^\\([a-zA-Z]+\\|[^a-zA-Z]\\)\\( .*\\)?$" item)
		    (concat "@" (match-string 1 item))))
	      "`" "'")))

(info-lookup-maybe-add-help
 :mode 'm4-mode
 :regexp "[_a-zA-Z][_a-zA-Z0-9]*"
 :doc-spec '(("(m4)Macro index"))
 :parse-rule "[_a-zA-Z0-9]+")

(info-lookup-maybe-add-help
 :mode 'autoconf-mode
 :regexp "A[CM]_[_A-Z0-9]+"
 :doc-spec '(("(autoconf)Macro Index" "AC_"
	      "^[ \t]+- \\(Macro\\|Variable\\): .*\\<" "\\>")
	     ("(automake)Index" nil
	      "^[ \t]*`" "'"))
 ;; Autoconf symbols are M4 macros.  Thus use M4's parser.
 :parse-rule 'ignore
 :other-modes '(m4-mode))

(info-lookup-maybe-add-help
 :mode 'awk-mode
 :regexp "[_a-zA-Z]+"
 :doc-spec '(("(gawk)Index"
	      (lambda (item)
		(let ((case-fold-search nil))
		  (cond
		   ;; `BEGIN' and `END'.
		   ((string-match "^\\([A-Z]+\\) special pattern\\b" item)
		    (match-string 1 item))
		   ;; `if', `while', `do', ...
		   ((string-match "^\\([a-z]+\\) statement\\b" item)
		    (if (not (string-equal (match-string 1 item) "control"))
			(match-string 1 item)))
		   ;; `NR', `NF', ...
		   ((string-match "^[A-Z]+$" item)
		    item)
		   ;; Built-in functions (matches to many entries).
		   ((string-match "^[a-z]+$" item)
		    item))))
	      "`" "\\([ \t]*([^)]*)\\)?'")))

(info-lookup-maybe-add-help
 :mode 'perl-mode
 :regexp "[$@%][^a-zA-Z]\\|\\$\\^[A-Z]\\|[$@%]?[a-zA-Z][_a-zA-Z0-9]*"
 :doc-spec '(("(perl5)Function Index"
	      (lambda (item)
		(if (string-match "^\\([a-zA-Z0-9]+\\)" item)
		    (match-string 1 item)))
	      "^" "\\b")
	     ("(perl5)Variable Index"
	      (lambda (item)
		;; Work around bad formatted array variables.
		(let ((sym (cond ((or (string-match "^\\$\\(.\\|@@\\)$" item)
				      (string-match "^\\$\\^[A-Z]$" item))
				  item)
				 ((string-match
				   "^\\([$%@]\\|@@\\)?[_a-zA-Z0-9]+" item)
				  (match-string 0 item))
				 (t ""))))
		  (if (string-match "@@" sym)
		      (setq sym (concat (substring sym 0 (match-beginning 0))
					(substring sym (1- (match-end 0))))))
		  (if (string-equal sym "") nil sym)))
	      "^" "\\b"))
 :parse-rule "[$@%]?\\([_a-zA-Z0-9]+\\|[^a-zA-Z]\\)")

(info-lookup-maybe-add-help
 :mode 'latex-mode
 :regexp "\\\\\\([a-zA-Z]+\\|[^a-zA-Z]\\)"
 :doc-spec '(("(latex2e)Command Index" nil
	      "`" "\\({[^}]*}\\)?'")))

(info-lookup-maybe-add-help
 :mode 'scheme-mode
 :regexp ;; "\\(\\sw\\|\\s_\\)+"
 "[^()' \t\n]+"
 :ignore-case t
 ;; Aubrey Jaffer's rendition from <URL:ftp://ftp-swiss.ai.mit.edu/pub/scm>
 :doc-spec '(("(r5rs)Index")))

(info-lookup-maybe-add-help
 :mode 'emacs-lisp-mode
 :regexp "[^()' \t\n]+"
 :doc-spec '(("(emacs)Command Index")
	     ("(emacs)Variable Index")
	     ("(elisp)Index"
	      (lambda (item)
		(let ((sym (intern-soft item)))
		  (cond ((null sym)
			 (if (string-equal item "nil") item))
			((or (boundp sym) (fboundp sym))
			 item))))
	      "^[ \t]+- [^:]+:[ \t]*" "\\b")))

(info-lookup-maybe-add-help
 :mode 'lisp-interaction-mode
 :regexp "[^()' \t\n]+"
 :parse-rule 'ignore
 :other-modes '(emacs-lisp-mode))

(info-lookup-maybe-add-help
 :mode 'lisp-mode
 :regexp "[^()' \t\n]+"
 :parse-rule 'ignore
 :other-modes '(emacs-lisp-mode))

(info-lookup-maybe-add-help
 :mode 'scheme-mode
 :regexp "[^()' \t\n]+"
 :ignore-case t
 :doc-spec '(("(r5rs)Index" nil
	      "^[ \t]+- [^:]+:[ \t]*" "\\b")))


(provide 'info-look)

;;; info-look.el ends here
