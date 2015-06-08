;;; Adaptive fill
;;; Copyright (C) 1989, 1995-1998 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to kyle_jones@wonderworks.com

;; LCD Archive Entry: 
;; filladapt|Kyle Jones|kyle_jones@wonderworks.com| 
;; Minor mode to adaptively set fill-prefix and overload filling functions|
;; 28-February-1998|2.12|~/packages/filladapt.el| 

;; These functions enhance the default behavior of Emacs' Auto Fill
;; mode and the commands fill-paragraph, lisp-fill-paragraph,
;; fill-region-as-paragraph and fill-region.
;;
;; The chief improvement is that the beginning of a line to be
;; filled is examined and, based on information gathered, an
;; appropriate value for fill-prefix is constructed.  Also the
;; boundaries of the current paragraph are located.  This occurs
;; only if the fill prefix is not already non-nil.
;;
;; The net result of this is that blurbs of text that are offset
;; from left margin by asterisks, dashes, and/or spaces, numbered
;; examples, included text from USENET news articles, etc. are
;; generally filled correctly with no fuss.
;;
;; Since this package replaces existing Emacs functions, it cannot
;; be autoloaded.  Save this in a file named filladapt.el in a
;; Lisp directory that Emacs knows about, byte-compile it and put
;;    (require 'filladapt)
;; in your .emacs file.
;;
;; Note that in this release Filladapt mode is a minor mode and it is
;; _off_ by default.  If you want it to be on by default, use
;;   (setq-default filladapt-mode t)
;;
;; M-x filladapt-mode toggles Filladapt mode on/off in the current
;; buffer.
;;
;; Use
;;     (add-hook 'text-mode-hook 'turn-on-filladapt-mode)
;; to have Filladapt always enabled in Text mode.
;;
;; Use
;;     (add-hook 'c-mode-hook 'turn-off-filladapt-mode)
;; to have Filladapt always disabled in C mode.
;;
;; In many cases, you can extend Filladapt by adding appropriate
;; entries to the following three `defvar's.  See `postscript-comment'
;; or `texinfo-comment' as a sample of what needs to be done.
;;
;;     filladapt-token-table
;;     filladapt-token-match-table
;;     filladapt-token-conversion-table

(and (featurep 'filladapt)
     (error "filladapt cannot be loaded twice in the same Emacs session."))

(provide 'filladapt)

(defvar filladapt-version "2.12"
  "Version string for filladapt.")

;; BLOB to make custom stuff work even without customize
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args) 
      (` (defvar (, var) (, value) (, doc))))))

(defgroup filladapt nil
  "Enhanced filling"
  :group 'fill)

(defvar filladapt-mode nil
  "Non-nil means that Filladapt minor mode is enabled.
Use the filladapt-mode command to toggle the mode on/off.")
(make-variable-buffer-local 'filladapt-mode)

(defcustom filladapt-mode-line-string " Filladapt"
  "*String to display in the modeline when Filladapt mode is active.
Set this to nil if you don't want a modeline indicator for Filladapt."
  :type 'string
  :group 'filladapt)

(defcustom filladapt-fill-column-tolerance nil
  "*Tolerate filled paragraph lines ending this far from the fill column.
If any lines other than the last paragraph line end at a column
less than fill-column - filladapt-fill-column-tolerance, fill-column will
be adjusted using the filladapt-fill-column-*-fuzz variables and
the paragraph will be re-filled until the tolerance is achieved
or filladapt runs out of fuzz values to try.

A nil value means behave normally, that is, don't try refilling
paragraphs to make filled line lengths fit within any particular
range."
  :type '(choice (const nil)
		 integer)
  :group 'filladapt)

(defcustom filladapt-fill-column-forward-fuzz 5
  "*Try values from fill-column to fill-column plus this variable
when trying to make filled paragraph lines fall with the tolerance
range specified by filladapt-fill-column-tolerance."
  :type 'integer
  :group 'filladapt)

(defcustom filladapt-fill-column-backward-fuzz 5
  "*Try values from fill-column to fill-column minus this variable
when trying to make filled paragraph lines fall with the tolerance
range specified by filladapt-fill-column-tolerance."
  :type 'integer
  :group 'filladapt)

;; install on minor-mode-alist
(or (assq 'filladapt-mode minor-mode-alist)
    (setq minor-mode-alist (cons (list 'filladapt-mode
				       'filladapt-mode-line-string)
				 minor-mode-alist)))

(defcustom filladapt-token-table
  '(
    ;; this must be first
    ("^" beginning-of-line)
    ;; Included text in news or mail replies
    (">+" citation->)
    ;; Included text generated by SUPERCITE.  We can't hope to match all
    ;; the possible variations, your mileage may vary.
    ("\\(\\w\\|[0-9]\\)[^'`\"< \t\n]*>[ \t]*" supercite-citation)
    ;; Lisp comments
    (";+" lisp-comment)
    ;; UNIX shell comments
    ("#+" sh-comment)
    ;; Postscript comments
    ("%+" postscript-comment)
    ;; C++ comments
    ("///*" c++-comment)
    ;; Texinfo comments
    ("@c[ \t]" texinfo-comment)
    ("@comment[ \t]" texinfo-comment)
    ;; Bullet types.
    ;;
    ;; LaTex \item
    ;;
    ("\\\\item[ \t]" bullet)
    ;;
    ;; 1. xxxxx
    ;;    xxxxx
    ;;
    ("[0-9]+\\.[ \t]" bullet)
    ;;
    ;; 2.1.3  xxxxx xx x xx x
    ;;        xxx
    ;;
    ("[0-9]+\\(\\.[0-9]+\\)+[ \t]" bullet)
    ;;
    ;; a. xxxxxx xx
    ;;    xxx xxx
    ;;
    ("[A-Za-z]\\.[ \t]" bullet)
    ;;
    ;; 1) xxxx x xx x xx   or   (1) xx xx x x xx xx
    ;;    xx xx xxxx                xxx xx x x xx x
    ;;
    ("(?[0-9]+)[ \t]" bullet)
    ;;
    ;; a) xxxx x xx x xx   or   (a) xx xx x x xx xx
    ;;    xx xx xxxx                xxx xx x x xx x
    ;;
    ("(?[A-Za-z])[ \t]" bullet)
    ;;
    ;; 2a. xx x xxx x x xxx
    ;;     xxx xx x xx x
    ;;
    ("[0-9]+[A-Za-z]\\.[ \t]" bullet)
    ;;
    ;; 1a) xxxx x xx x xx   or   (1a) xx xx x x xx xx
    ;;     xx xx xxxx                 xxx xx x x xx x
    ;;
    ("(?[0-9]+[A-Za-z])[ \t]" bullet)
    ;;
    ;; -  xx xxx xxxx   or   *  xx xx x xxx xxx
    ;;    xxx xx xx             x xxx x xx x x x
    ;;
    ("[-~*+]+[ \t]" bullet)
    ;;
    ;; o  xx xxx xxxx xx x xx xxx x xxx xx x xxx
    ;;    xxx xx xx 
    ;;
    ("o[ \t]" bullet)
    ;; don't touch
    ("[ \t]+" space)
    ("$" end-of-line)
   )
  "Table of tokens filladapt knows about.
Format is

   ((REGEXP SYM) ...)

filladapt uses this table to build a tokenized representation of
the beginning of the current line.  Each REGEXP is matched
against the beginning of the line until a match is found.
Matching is done case-sensitively.  The corresponding SYM is
added to the list, point is moved to (match-end 0) and the
process is repeated.  The process ends when there is no REGEXP in
the table that matches what is at point."
  :type '(repeat (list regexp symbol))
  :group 'filladapt)

(defcustom filladapt-not-token-table
  '(
    "[Ee]\\.g\\.[ \t,]"
    "[Ii]\\.e\\.[ \t,]"
    ;; end-of-line isn't a token if whole line is empty
    "^$"
   )
  "List of regexps that can never be a token.
Before trying the regular expressions in filladapt-token-table,
the regexps in this list are tried.  If any regexp in this list
matches what is at point then the token generator gives up and
doesn't try any of the regexps in filladapt-token-table.

Regexp matching is done case-sensitively."
  :type '(repeat regexp)
  :group 'filladapt)

(defcustom filladapt-token-match-table
  '(
    (citation-> citation->)
    (supercite-citation supercite-citation)
    (lisp-comment lisp-comment)
    (sh-comment sh-comment)
    (postscript-comment postscript-comment)
    (c++-comment c++-comment)
    (texinfo-comment texinfo-comment)
    (bullet)
    (space bullet space)
    (beginning-of-line beginning-of-line)
   )
  "Table describing what tokens a certain token will match.

To decide whether a line belongs in the current paragraph,
filladapt creates a token list for the fill prefix of both lines.
Tokens and the columns where tokens end are compared.  This table
specifies what a certain token will match.

Table format is

   (SYM [SYM1 [SYM2 ...]])

The first symbol SYM is the token, subsequent symbols are the
tokens that SYM will match."
  :type '(repeat (repeat symbol))
  :group 'filladapt)

(defcustom filladapt-token-match-many-table
  '(
    space
   )
  "List of tokens that can match multiple tokens.
If one of these tokens appears in a token list, it will eat all
matching tokens in a token list being matched against it until it
encounters a token that doesn't match or a token that ends on
a greater column number."
  :type '(repeat symbol)
  :group 'filladapt)

(defcustom filladapt-token-paragraph-start-table
  '(
    bullet
   )
  "List of tokens that indicate the start of a paragraph.
If parsing a line generates a token list containing one of
these tokens, then the line is considered to be the start of a
paragraph."
  :type '(repeat symbol)
  :group 'filladapt)

(defcustom filladapt-token-conversion-table
  '(
    (citation-> . exact)
    (supercite-citation . exact)
    (lisp-comment . exact)
    (sh-comment . exact)
    (postscript-comment . exact)
    (c++-comment . exact)
    (texinfo-comment . exact)
    (bullet . spaces)
    (space . exact)
    (end-of-line . exact)
   )
  "Table that specifies how to convert a token into a fill prefix.
Table format is

   ((SYM . HOWTO) ...)

SYM is the symbol naming the token to be converted.
HOWTO specifies how to do the conversion.
  `exact' means copy the token's string directly into the fill prefix.
  `spaces' means convert all characters in the token string that are
      not a TAB or a space into spaces and copy the resulting string into 
      the fill prefix."
  :type '(repeat (cons symbol (choice (const exact)
				      (const spaces))))
  :group 'filladapt)

(defvar filladapt-function-table
  (let ((assoc-list
	 (list (cons 'fill-paragraph (symbol-function 'fill-paragraph))
	       (cons 'fill-region (symbol-function 'fill-region))
	       (cons 'fill-region-as-paragraph
		     (symbol-function 'fill-region-as-paragraph))
	       (cons 'do-auto-fill (symbol-function 'do-auto-fill)))))
    ;; v18 Emacs doesn't have lisp-fill-paragraph
    (if (fboundp 'lisp-fill-paragraph)
	(nconc assoc-list
	       (list (cons 'lisp-fill-paragraph
			   (symbol-function 'lisp-fill-paragraph)))))
    assoc-list )
  "Table containing the old function definitions that filladapt usurps.")

(defcustom filladapt-fill-paragraph-post-hook nil
  "Hooks run after filladapt runs fill-paragraph."
  :type 'hook
  :group 'filladapt)

(defvar filladapt-inside-filladapt nil
  "Non-nil if the filladapt version of a fill function executing.
Currently this is only checked by the filladapt version of
fill-region-as-paragraph to avoid this infinite recursion:

  fill-region-as-paragraph -> fill-paragraph -> fill-region-as-paragraph ...")

(defcustom filladapt-debug nil
  "Non-nil means filladapt debugging is enabled.
Use the filladapt-debug command to turn on debugging.

With debugging enabled, filladapt will

    a. display the proposed indentation with the tokens highlighted
       using filladapt-debug-indentation-face-1 and
       filladapt-debug-indentation-face-2.
    b. display the current paragraph using the face specified by
       filladapt-debug-paragraph-face."
  :type 'boolean
  :group 'filladapt)

(if filladapt-debug
    (add-hook 'post-command-hook 'filladapt-display-debug-info-maybe))

(defvar filladapt-debug-indentation-face-1 'highlight
  "Face used to display the indentation when debugging is enabled.")

(defvar filladapt-debug-indentation-face-2 'secondary-selection
  "Another face used to display the indentation when debugging is enabled.")

(defvar filladapt-debug-paragraph-face 'bold
  "Face used to display the current paragraph when debugging is enabled.")

(defvar filladapt-debug-indentation-extents nil)
(make-variable-buffer-local 'filladapt-debug-indentation-extents)
(defvar filladapt-debug-paragraph-extent nil)
(make-variable-buffer-local 'filladapt-debug-paragraph-extent)

;; kludge city, see references in code.
(defvar filladapt-old-line-prefix)

(defun do-auto-fill ()
  (catch 'done
    (if (and filladapt-mode (null fill-prefix))
	(save-restriction
	  (let ((paragraph-ignore-fill-prefix nil)
		;; if the user wanted this stuff, they probably
		;; wouldn't be using filladapt-mode.
		(adaptive-fill-mode nil)
		(adaptive-fill-regexp nil)
		;; need this or Emacs 19 ignores fill-prefix when
		;; inside a comment.
		(comment-multi-line t)
		(filladapt-inside-filladapt t)
		fill-prefix retval)
	    (if (filladapt-adapt nil nil)
		(progn
		  (setq retval (filladapt-funcall 'do-auto-fill))
		  (throw 'done retval))))))
    (filladapt-funcall 'do-auto-fill)))

(defun filladapt-fill-paragraph (function arg)
  (catch 'done
    (if (and filladapt-mode (null fill-prefix))
	(save-restriction
	  (let ((paragraph-ignore-fill-prefix nil)
		;; if the user wanted this stuff, they probably
		;; wouldn't be using filladapt-mode.
		(adaptive-fill-mode nil)
		(adaptive-fill-regexp nil)
		;; need this or Emacs 19 ignores fill-prefix when
		;; inside a comment.
		(comment-multi-line t)
		fill-prefix retval)
	    (if (filladapt-adapt t nil)
		(progn
		  (if filladapt-fill-column-tolerance
		      (let* ((low (- fill-column
				     filladapt-fill-column-backward-fuzz))
			     (high (+ fill-column
				      filladapt-fill-column-forward-fuzz))
			     (old-fill-column fill-column)
			     (fill-column fill-column)
			     (lim (- high low))
			     (done nil)
			     (sign 1)
			     (delta 0))
			(while (not done)
			  (setq retval (filladapt-funcall function arg))
			  (if (filladapt-paragraph-within-fill-tolerance)
			      (setq done 'success)
			    (setq delta (1+ delta)
				  sign (* sign -1)
				  fill-column (+ fill-column (* delta sign)))
			    (while (and (<= delta lim)
					(or (< fill-column low)
					    (> fill-column high)))
			      (setq delta (1+ delta)
				    sign (* sign -1)
				    fill-column (+ fill-column
						   (* delta sign))))
			    (setq done (> delta lim))))
			;; if the paragraph lines never fell
			;; within the tolerances, refill using
			;; the old fill-column.
			(if (not (eq done 'success))
			    (let ((fill-column old-fill-column))
			      (setq retval (filladapt-funcall function arg)))))
		    (setq retval (filladapt-funcall function arg)))
		  (run-hooks 'filladapt-fill-paragraph-post-hook)
		  (throw 'done retval))))))
    ;; filladapt-adapt failed, so do fill-paragraph normally.
    (filladapt-funcall function arg)))

(defun fill-paragraph (arg)
  "Fill paragraph at or after point.  Prefix arg means justify as well.

(This function has been overloaded with the `filladapt' version.)

If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there.

If `fill-paragraph-function' is non-nil, we call it (passing our
argument to it), and if it returns non-nil, we simply return its value."
  (interactive "*P")
  (let ((filladapt-inside-filladapt t))
    (filladapt-fill-paragraph 'fill-paragraph arg)))

(defun lisp-fill-paragraph (&optional arg)
  "Like \\[fill-paragraph], but handle Emacs Lisp comments.

(This function has been overloaded with the `filladapt' version.)

If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial semicolons."
  (interactive "*P")
  (let ((filladapt-inside-filladapt t))
    (filladapt-fill-paragraph 'lisp-fill-paragraph arg)))

(defun fill-region-as-paragraph (beg end &optional justify
				 nosqueeze squeeze-after)
  "Fill the region as one paragraph.

(This function has been overloaded with the `filladapt' version.)

It removes any paragraph breaks in the region and extra newlines at the end,
indents and fills lines between the margins given by the
`current-left-margin' and `current-fill-column' functions.
It leaves point at the beginning of the line following the paragraph.

Normally performs justification according to the `current-justification'
function, but with a prefix arg, does full justification instead.

From a program, optional third arg JUSTIFY can specify any type of
justification.  Fourth arg NOSQUEEZE non-nil means not to make spaces
between words canonical before filling.  Fifth arg SQUEEZE-AFTER, if non-nil,
means don't canonicalize spaces before that position.

If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there."
  (interactive "*r\nP")
  (if (and filladapt-mode (not filladapt-inside-filladapt))
      (save-restriction
	(narrow-to-region beg end)
	(let ((filladapt-inside-filladapt t)
	      line-start last-token)
	  (goto-char beg)
	  (while (equal (char-after (point)) ?\n)
	    (delete-char 1))
	  (end-of-line)
	  (while (zerop (forward-line))
	    (if (setq last-token
		      (car (filladapt-tail (filladapt-parse-prefixes))))
		(progn
		  (setq line-start (point))
		  (move-to-column (nth 1 last-token))
		  (delete-region line-start (point))))
	    ;; Dance...
	    ;;
	    ;; Do this instead of (delete-char -1) to keep
	    ;; markers on the correct side of the whitespace.
	    (goto-char (1- (point)))
	    (insert " ")
	    (delete-char 1)

	    (end-of-line))
	  (goto-char beg)
	  (fill-paragraph justify))
	;; In XEmacs 19.12 and Emacs 18.59 fill-region relies on
	;; fill-region-as-paragraph to do this.  If we don't do
	;; it, fill-region will spin in an endless loop.
	(goto-char (point-max)))
    (condition-case nil
	;; five args for Emacs 19.31
	(filladapt-funcall 'fill-region-as-paragraph beg end
			   justify nosqueeze squeeze-after)
      (wrong-number-of-arguments
       (condition-case nil
	   ;; four args for Emacs 19.29
	   (filladapt-funcall 'fill-region-as-paragraph beg end
			      justify nosqueeze)
	 ;; three args for the rest of the world.
	 (wrong-number-of-arguments
	  (filladapt-funcall 'fill-region-as-paragraph beg end justify)))))))

(defun fill-region (beg end &optional justify nosqueeze to-eop)
  "Fill each of the paragraphs in the region.

(This function has been overloaded with the `filladapt' version.)

Prefix arg (non-nil third arg, if called from program) means justify as well.

Noninteractively, fourth arg NOSQUEEZE non-nil means to leave
whitespace other than line breaks untouched, and fifth arg TO-EOP
non-nil means to keep filling to the end of the paragraph (or next
hard newline, if `use-hard-newlines' is on).

If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there."
  (interactive "*r\nP")
  (if (and filladapt-mode (not filladapt-inside-filladapt))
      (save-restriction
	(narrow-to-region beg end)
	(let ((filladapt-inside-filladapt t)
	      start)
	  (goto-char beg)
	  (while (not (eobp))
	    (setq start (point))
	    (while (and (not (eobp)) (not (filladapt-parse-prefixes)))
	      (forward-line 1))
	    (if (not (equal start (point)))
		(progn
		  (save-restriction
		    (narrow-to-region start (point))
		    (fill-region start (point) justify nosqueeze to-eop)
		    (goto-char (point-max)))
		  (if (and (not (bolp)) (not (eobp)))
		      (forward-line 1))))
	    (if (filladapt-parse-prefixes)
		(progn
		  (save-restriction
		    ;; for the clipping region
		    (filladapt-adapt t t)
		    (fill-paragraph justify)
		    (goto-char (point-max)))
		  (if (and (not (bolp)) (not (eobp)))
		      (forward-line 1)))))))
    (condition-case nil
	(filladapt-funcall 'fill-region beg end justify nosqueeze to-eop)
      (wrong-number-of-arguments
       (condition-case nil
	   (filladapt-funcall 'fill-region beg end justify nosqueeze)
	 (wrong-number-of-arguments
	  (filladapt-funcall 'fill-region beg end justify)))))))

(defvar zmacs-region-stays) ; for XEmacs

(defun filladapt-mode (&optional arg)
  "Toggle Filladapt minor mode.
With arg, turn Filladapt mode on iff arg is positive.  When
Filladapt mode is enabled, auto-fill-mode and the fill-paragraph
command are both smarter about guessing a proper fill-prefix and
finding paragraph boundaries when bulleted and indented lines and
paragraphs are used."
  (interactive "P")
  ;; don't deactivate the region.
  (setq zmacs-region-stays t)
  (setq filladapt-mode (or (and arg (> (prefix-numeric-value arg) 0))
			   (and (null arg) (null filladapt-mode))))
  (if (fboundp 'force-mode-line-update)
      (force-mode-line-update)
    (set-buffer-modified-p (buffer-modified-p))))

(defun turn-on-filladapt-mode ()
  "Unconditionally turn on Filladapt mode in the current buffer."
  (filladapt-mode 1))

(defun turn-off-filladapt-mode ()
  "Unconditionally turn off Filladapt mode in the current buffer."
  (filladapt-mode -1))

(defun filladapt-funcall (function &rest args)
  "Call the old definition of a function that filladapt has usurped."
  (apply (cdr (assoc function filladapt-function-table)) args))

(defun filladapt-paragraph-start (list)
  "Returns non-nil if LIST contains a paragraph starting token.
LIST should be a token list as returned by filladapt-parse-prefixes."
  (catch 'done
    (while list
      (if (memq (car (car list)) filladapt-token-paragraph-start-table)
	  (throw 'done t))
      (setq list (cdr list)))))

(defun filladapt-parse-prefixes ()
  "Parse all the tokens after point and return a list of them.
The tokens regular expressions are specified in
filladapt-token-table.  The list returned is of this form

  ((SYM COL STRING) ...)

SYM is a token symbol as found in filladapt-token-table.
COL is the column at which the token ended.
STRING is the token's text."
  (save-excursion
    (let ((token-list nil)
	  (done nil)
	  (old-point (point))
	  (case-fold-search nil)
	  token-table not-token-table moved)
      (catch 'done
	(while (not done)
	  (setq not-token-table filladapt-not-token-table)
	  (while not-token-table
	    (if (looking-at (car not-token-table))
		(throw 'done t))
	    (setq not-token-table (cdr not-token-table)))
	  (setq token-table filladapt-token-table
		done t)
	  (while token-table
	    (if (null (looking-at (car (car token-table))))
		(setq token-table (cdr token-table))
	      (goto-char (match-end 0))
	      (setq token-list (cons (list (nth 1 (car token-table))
					   (current-column)
					   (buffer-substring
					    (match-beginning 0)
					    (match-end 0)))
				     token-list)
		    moved (not (eq (point) old-point))
		    token-table (if moved nil (cdr token-table))
		    done (not moved)
		    old-point (point))))))
      (nreverse token-list))))

(defun filladapt-tokens-match-p (list1 list2)
  "Compare two token lists and return non-nil if they match, nil otherwise.
The lists are walked through in lockstep, comparing tokens.

When two tokens A and B are compared, they are considered to
match if

    1. A appears in B's list of matching tokens or
       B appears in A's list of matching tokens
and
    2. A and B both end at the same column
         or
       A can match multiple tokens and ends at a column > than B
         or
       B can match multiple tokens and ends at a column > than A

In the case where the end columns differ the list pointer for the
token with the greater end column is not moved forward, which
allows its current token to be matched against the next token in
the other list in the next iteration of the matching loop.

All tokens must be matched in order for the lists to be considered
matching."
  (let ((matched t)
	(done nil))
    (while (and (not done) list1 list2)
      (let* ((token1 (car (car list1)))
	     (token1-matches-many-p
	         (memq token1 filladapt-token-match-many-table))
	     (token1-matches (cdr (assq token1 filladapt-token-match-table)))
	     (token1-endcol (nth 1 (car list1)))
	     (token2 (car (car list2)))
	     (token2-matches-many-p
	         (memq token2 filladapt-token-match-many-table))
	     (token2-matches (cdr (assq token2 filladapt-token-match-table)))
	     (token2-endcol (nth 1 (car list2)))
	     (tokens-match (or (memq token1 token2-matches)
			       (memq token2 token1-matches))))
	(cond ((not tokens-match)
	       (setq matched nil
		     done t))
	      ((and token1-matches-many-p token2-matches-many-p)
	       (cond ((= token1-endcol token2-endcol)
		      (setq list1 (cdr list1)
			    list2 (cdr list2)))
		     ((< token1-endcol token2-endcol)
		      (setq list1 (cdr list1)))
		     (t
		      (setq list2 (cdr list2)))))
	      (token1-matches-many-p
	       (cond ((= token1-endcol token2-endcol)
		      (setq list1 (cdr list1)
			    list2 (cdr list2)))
		     ((< token1-endcol token2-endcol)
		      (setq matched nil
			    done t))
		     (t
		      (setq list2 (cdr list2)))))
	      (token2-matches-many-p
	       (cond ((= token1-endcol token2-endcol)
		      (setq list1 (cdr list1)
			    list2 (cdr list2)))
		     ((< token2-endcol token1-endcol)
		      (setq matched nil
			    done t))
		     (t
		      (setq list1 (cdr list1)))))
	      ((= token1-endcol token2-endcol)
	       (setq list1 (cdr list1)
		     list2 (cdr list2)))
	      (t
	       (setq matched nil
		     done t)))))
    (and matched (null list1) (null list2)) ))

(defun filladapt-make-fill-prefix (list)
  "Build a fill-prefix for a token LIST.
filladapt-token-conversion-table specifies how this is done."
  (let ((prefix-list nil)
	(conversion-spec nil))
    (while list
      (setq conversion-spec (cdr (assq (car (car list))
				       filladapt-token-conversion-table)))
      (cond ((eq conversion-spec 'spaces)
	     (setq prefix-list
		   (cons
		    (filladapt-convert-to-spaces (nth 2 (car list)))
		    prefix-list)))
	    ((eq conversion-spec 'exact)
	     (setq prefix-list
		   (cons
		    (nth 2 (car list))
		    prefix-list))))
      (setq list (cdr list)))
    (apply (function concat) (nreverse prefix-list)) ))

(defun filladapt-paragraph-within-fill-tolerance ()
  (catch 'done
    (save-excursion
      (let ((low (- fill-column filladapt-fill-column-tolerance))
	    (shortline nil))
	(goto-char (point-min))
	(while (not (eobp))
	  (if shortline
	      (throw 'done nil)
	    (end-of-line)
	    (setq shortline (< (current-column) low))
	    (forward-line 1)))
	t ))))

(defun filladapt-convert-to-spaces (string)
  "Return a copy of STRING, with all non-tabs and non-space changed to spaces."
  (let ((i 0)
	(space-list '(?\  ?\t))
	(space ?\ )
	(lim (length string)))
    (setq string (copy-sequence string))
    (while (< i lim)
      (if (not (memq (aref string i) space-list))
	  (aset string i space))
      (setq i (1+ i)))
    string ))

(defun filladapt-adapt (paragraph debugging)
  "Set fill-prefix based on the contents of the current line.

If the first arg PARAGRAPH is non-nil, also set a clipping region
around the current paragraph.

If the second arg DEBUGGING is non-nil, don't do the kludge that's
necessary to make certain paragraph fills work properly."
  (save-excursion
    (beginning-of-line)
    (let ((token-list (filladapt-parse-prefixes))
	  curr-list done)
      (if (null token-list)
	  nil
	(setq fill-prefix (filladapt-make-fill-prefix token-list))
	(if paragraph
	    (let (beg end)
	      (if (filladapt-paragraph-start token-list)
		  (setq beg (point))
		(save-excursion
		  (setq done nil)
		  (while (not done)
		    (cond ((not (= 0 (forward-line -1)))
			   (setq done t
				 beg (point)))
			  ((not (filladapt-tokens-match-p
				 token-list
				 (setq curr-list (filladapt-parse-prefixes))))
			   (forward-line 1)
			   (setq done t
				 beg (point)))
			  ((filladapt-paragraph-start curr-list)
			   (setq done t
				 beg (point)))))))
	      (save-excursion
		(setq done nil)
		(while (not done)
		  (cond ((not (= 0 (progn (end-of-line) (forward-line 1))))
			 (setq done t
			       end (point)))
			((not (filladapt-tokens-match-p
			       token-list
			       (setq curr-list (filladapt-parse-prefixes))))
			 (setq done t
			       end (point)))
			((filladapt-paragraph-start curr-list)
			 (setq done t
			       end (point))))))
	      (narrow-to-region beg end)
	      ;; Multiple spaces after the bullet at the start of
	      ;; a hanging list paragraph get squashed by
	      ;; fill-paragraph.  We kludge around this by
	      ;; replacing the line prefix with the fill-prefix
	      ;; used by the rest of the lines in the paragraph.
	      ;; fill-paragraph will not alter the fill prefix so
	      ;; we win.  The post hook restores the old line prefix
	      ;; after fill-paragraph has been called.
	      (if (and paragraph (not debugging))
		  (let (col)
		    (setq col (nth 1 (car (filladapt-tail token-list))))
		    (goto-char (point-min))
		    (move-to-column col)
		    (setq filladapt-old-line-prefix
			  (buffer-substring (point-min) (point)))
		    (delete-region (point-min) (point))
		    (insert fill-prefix)
		    (add-hook 'filladapt-fill-paragraph-post-hook
			      'filladapt-cleanup-kludge-at-point-min)))))
	t ))))

(defun filladapt-cleanup-kludge-at-point-min ()
  "Cleanup the paragraph fill kludge.
See filladapt-adapt."
  (save-excursion
    (goto-char (point-min))
    (insert filladapt-old-line-prefix)
    (delete-char (length fill-prefix))
    (remove-hook 'filladapt-fill-paragraph-post-hook
		 'filladapt-cleanup-kludge-at-point-min)))

(defun filladapt-tail (list)
  "Returns the last cons in LIST."
  (if (null list)
      nil
    (while (consp (cdr list))
      (setq list (cdr list)))
    list ))

(defun filladapt-delete-extent (e)
  (if (fboundp 'delete-extent)
      (delete-extent e)
    (delete-overlay e)))

(defun filladapt-make-extent (beg end)
  (if (fboundp 'make-extent)
      (make-extent beg end)
    (make-overlay beg end)))

(defun filladapt-set-extent-endpoints (e beg end)
  (if (fboundp 'set-extent-endpoints)
      (set-extent-endpoints e beg end)
    (move-overlay e beg end)))

(defun filladapt-set-extent-property (e prop val)
  (if (fboundp 'set-extent-property)
      (set-extent-property e prop val)
    (overlay-put e prop val)))

(defun filladapt-debug ()
  "Toggle filladapt debugging on/off in the current buffer."
;;  (interactive)
  (make-local-variable 'filladapt-debug)
  (setq filladapt-debug (not filladapt-debug))
  (if (null filladapt-debug)
      (progn
	(mapcar (function (lambda (e) (filladapt-set-extent-endpoints e 1 1)))
		filladapt-debug-indentation-extents)
	(if filladapt-debug-paragraph-extent
	    (progn
	      (filladapt-delete-extent filladapt-debug-paragraph-extent)
	      (setq filladapt-debug-paragraph-extent nil)))))
  (add-hook 'post-command-hook 'filladapt-display-debug-info-maybe))

(defun filladapt-display-debug-info-maybe ()
  (cond ((null filladapt-debug) nil)
	(fill-prefix nil)
	(t
	 (if (null filladapt-debug-paragraph-extent)
	     (let ((e (filladapt-make-extent 1 1)))
	       (filladapt-set-extent-property e 'detachable nil)
	       (filladapt-set-extent-property e 'evaporate nil)
	       (filladapt-set-extent-property e 'face
					      filladapt-debug-paragraph-face)
	       (setq filladapt-debug-paragraph-extent e)))
	 (save-excursion
	   (save-restriction
	     (let ((ei-list filladapt-debug-indentation-extents)
		   (ep filladapt-debug-paragraph-extent)
		   (face filladapt-debug-indentation-face-1)
		   fill-prefix token-list)
	       (if (null (filladapt-adapt t t))
		   (progn
		     (filladapt-set-extent-endpoints ep 1 1)
		     (while ei-list
		       (filladapt-set-extent-endpoints (car ei-list) 1 1)
		       (setq ei-list (cdr ei-list))))
		 (filladapt-set-extent-endpoints ep (point-min) (point-max))
		 (beginning-of-line)
		 (setq token-list (filladapt-parse-prefixes))
		 (message "(%s)" (mapconcat (function
					   (lambda (q) (symbol-name (car q))))
					  token-list
					  " "))
		 (while token-list
		   (if ei-list
		       (setq e (car ei-list)
			     ei-list (cdr ei-list))
		     (setq e (filladapt-make-extent 1 1))
		     (filladapt-set-extent-property e 'detachable nil)
		     (filladapt-set-extent-property e 'evaporate nil)
		     (setq filladapt-debug-indentation-extents
			   (cons e filladapt-debug-indentation-extents)))
		   (filladapt-set-extent-property e 'face face)
		   (filladapt-set-extent-endpoints e (point)
						   (progn
						     (move-to-column
						      (nth 1
							   (car token-list)))
						     (point)))
		   (if (eq face filladapt-debug-indentation-face-1)
		       (setq face filladapt-debug-indentation-face-2)
		     (setq face filladapt-debug-indentation-face-1))
		   (setq token-list (cdr token-list)))
		 (while ei-list
		   (filladapt-set-extent-endpoints (car ei-list) 1 1)
		   (setq ei-list (cdr ei-list))))))))))
