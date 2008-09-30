;;; info-look.el --- major-mode-sensitive Info index lookup facility.
;; An older version of this was known as libc.el.

;; Copyright (C) 1995, 1996, 1997 Ralph Schleicher.

;; Author: Ralph Schleicher <rs@purple.UL.BaWue.DE>
;; Keywords: help languages

;; This file is not part of GNU Emacs. (but is slightly modified from
;; a file that is a part of GNU Emacs -- see below)

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

;; Bruce Ravel <ravel@phys.washington.edu> made two chanegs to this
;; file:
;;  1. Added a check for XEmacs
;;  2. Added (format "%s" (match-string 1)) in function
;;     `info-lookup-make-completions' so that text properties are not
;;     grabbed.

;;; Code:

(require 'info)
;; next two lines added by Bruce Ravel <ravel@phys.washington.edu> to
;; make this file compile properly under XEmacs.
(eval-and-compile
  (if (string-match "XEmacs" emacs-version)
      (require 'overlay)))

(defvar info-lookup-mode nil
  "*Symbol of the current buffer's help mode.
Provide help according to the buffer's major mode if value is nil.
Automatically becomes buffer local when set in any fashion.")
(make-variable-buffer-local 'info-lookup-mode)

(defvar info-lookup-other-window-flag t
  "*Non-nil means pop up the Info buffer in another window.")

(defvar info-lookup-highlight-face 'highlight
  "*Face for highlighting looked up help items.
Setting this variable to nil disables highlighting.")

(defvar info-lookup-highlight-overlay nil
  "Overlay object used for highlighting.")

(defvar info-lookup-history nil
  "History of previous input lines.")

(defvar info-lookup-alist '((symbol . info-lookup-symbol-alist)
			    (file . info-lookup-file-alist))
  "*Alist of known help topics.
Cons cells are of the form

    (HELP-TOPIC . VARIABLE)

HELP-TOPIC is the symbol of a help topic.
VARIABLE is a variable storing HELP-TOPIC's public data.
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
  (symbol-value (cdr (assoc topic info-lookup-alist))))

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

(defsubst info-lookup->topic-cache (topic)
  (cdr (info-lookup->cache topic)))

(defsubst info-lookup->mode-cache (topic mode)
  (assoc mode (info-lookup->topic-cache topic)))

(defsubst info-lookup->initialized (topic mode)
  (nth 1 (info-lookup->mode-cache topic mode)))

(defsubst info-lookup->completions (topic mode)
  (or (info-lookup->initialized topic mode)
      (info-lookup-setup-mode topic mode))
  (nth 2 (info-lookup->mode-cache topic mode)))

(defsubst info-lookup->refer-modes (topic mode)
  (or (info-lookup->initialized topic mode)
      (info-lookup-setup-mode topic mode))
  (nth 3 (info-lookup->mode-cache topic mode)))

(defsubst info-lookup->all-modes (topic mode)
  (cons mode (info-lookup->refer-modes topic mode)))

(defvar info-lookup-symbol-alist
  '((autoconf-mode
     "A[CM]_[_A-Z0-9]+" nil
     (("(autoconf)Macro Index" "AC_"
       "^[ \t]+- \\(Macro\\|Variable\\): .*\\<" "\\>")
      ("(automake)Index" nil
       "^[ \t]*`" "'"))
     ;; Autoconf symbols are M4 macros.  Thus use M4's parser.
     ignore
     (m4-mode))
    (bison-mode
     "[:;|]\\|%\\([%{}]\\|[_a-z]+\\)\\|YY[_A-Z]+\\|yy[_a-z]+" nil
     (("(bison)Index" nil
       "`" "'"))
     "[:;|]\\|%\\([%{}]\\|[_a-zA-Z][_a-zA-Z0-9]*\\)"
     (c-mode))
    (c-mode
     "\\(struct \\|union \\|enum \\)?[_a-zA-Z][_a-zA-Z0-9]*" nil
     (("(libc)Function Index" nil
       "^[ \t]+- \\(Function\\|Macro\\): .*\\<" "\\>")
      ("(libc)Variable Index" nil
       "^[ \t]+- \\(Variable\\|Macro\\): .*\\<" "\\>")
      ("(libc)Type Index" nil
       "^[ \t]+- Data Type: \\<" "\\>")
      ("(termcap)Var Index" nil
       "^[ \t]*`" "'"))
     info-lookup-guess-c-symbol)
    (m4-mode
     "[_a-zA-Z][_a-zA-Z0-9]*" nil
     (("(m4)Macro index"))
     "[_a-zA-Z0-9]+")
    (makefile-mode
     "\\$[^({]\\|\\.[_A-Z]*\\|[_a-zA-Z][_a-zA-Z0-9-]*" nil
     (("(make)Name Index" nil
       "^[ \t]*`" "'"))
     "\\$[^({]\\|\\.[_A-Z]*\\|[_a-zA-Z0-9-]+")
    (texinfo-mode
     "@\\([a-zA-Z]+\\|[^a-zA-Z]\\)" nil
     (("(texinfo)Command and Variable Index"
       ;; Ignore Emacs commands and prepend a `@'.
       (lambda (item)
	 (if (string-match "^\\([a-zA-Z]+\\|[^a-zA-Z]\\)\\( .*\\)?$" item)
	     (concat "@" (match-string 1 item))))
       "`" "'"))))
  "*Alist of help specifications for symbol names.
See the documentation of the variable `info-lookup-alist' for more details.")

(defvar info-lookup-file-alist
  '((c-mode
     "[_a-zA-Z0-9./+-]+" nil
     (("(libc)File Index"))))
  "*Alist of help specifications for file names.
See the documentation of the variable `info-lookup-alist' for more details.")

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
  (let* ((mode (if (info-lookup->mode-value
		    topic (or info-lookup-mode major-mode))
		   (or info-lookup-mode major-mode)
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
  (if (not mode)
      (setq mode (or info-lookup-mode major-mode)))
  (or (info-lookup->mode-value topic mode)
      (error "No %s help available for `%s'" topic mode))
  (let ((entry (or (assoc (if (info-lookup->ignore-case topic mode)
			      (downcase item) item)
			  (info-lookup->completions topic mode))
		   (error "Not documented as a %s: %s" topic (or item ""))))
	(modes (info-lookup->all-modes topic mode))
	(window (selected-window))
	found doc-spec node prefix suffix)
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
	(condition-case nil
	    (progn
	      (Info-goto-node node)
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
	  (error nil))
	(setq doc-spec (cdr doc-spec)))
      (setq modes (cdr modes)))
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
  (condition-case nil
      (let ((doc-spec (info-lookup->doc-spec topic mode))
	    (regexp (concat "^\\(" (info-lookup->regexp topic mode)
			    "\\)\\([ \t].*\\)?$"))
	    node trans entry item prefix result)
	(save-window-excursion
	  (info)
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
	    (message "Processing Info node \"%s\"..." node)
	    (Info-goto-node node)
	    (goto-char (point-min))
	    (and (search-forward "\n* Menu:" nil t)
		 (while (re-search-forward "\n\\* \\([^:\t\n]*\\):" nil t)
		   ;; Bruce Ravel added format
		   ;; w/o format, this grabs text properties
		   (setq entry (format "%s" (match-string 1))
			 item (funcall trans entry))
		   (and (info-lookup->ignore-case topic mode)
			(setq item (downcase item)))
		   (and (string-equal entry item)
			(setq entry nil))
		   (or (assoc item result)
		       (setq result (cons (cons item entry) result)))))
	    (message "Processing Info node \"%s\"... done" node)
	    (setq doc-spec (cdr doc-spec)))
	  (Info-directory))
	result)
    (error nil)))

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
			  'symbol (or info-lookup-mode major-mode))
			 (or info-lookup-mode major-mode)
		       (info-lookup-change-mode 'symbol)))))

;;;###autoload
(defun info-complete-file (&optional mode)
  "Perform completion on file preceding point."
  (interactive
   (list (if (info-lookup->mode-value
	      'file (or info-lookup-mode major-mode))
	     (or info-lookup-mode major-mode)
	   (info-lookup-change-mode 'file))))
  (info-complete 'file mode))

(defun info-complete (topic mode)
  "Try to complete a help item."
  (barf-if-buffer-read-only)
  (if (not mode)
      (setq mode (or info-lookup-mode major-mode)))
  (or (info-lookup->mode-value topic mode)
      (error "No %s completion available for `%s'" topic mode))
  (let ((modes (info-lookup->all-modes topic mode))
	(start (point)) try completion)
    (while (and (not try) modes)
      (setq mode (car modes)
	    modes (cdr modes)
	    try (info-lookup-guess-default* topic mode))
      (goto-char start))
    (and (not try)
	 (error "Found no %s to complete" topic))
    (setq completion (try-completion
		      try (info-lookup->completions topic mode)))
    (cond ((not completion)
	   (ding))
	  ((stringp completion)
	   (delete-region (- start (length try)) start)
	   (insert completion)))))

(provide 'info-look)

;;; info-look.el ends here
