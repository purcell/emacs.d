;;; Sformat --- souped up format
;;
;; Author: Eric Ludlam (zappo@gnu.org)
;; Version: 1.4
;; Keywords: extensions
;;
;; Copyright (C) 1994, 1996, 1998, 1999, 2000, 2002 Free Software Foundation
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;

;;; Commentary:
;;
;; In some applications configuration strings have % options in them
;; which permit special strings to be inserted.  There are many other
;; programs which would also benefit from such configuration, but do
;; not have it due to the time required to build such an extension.
;; Sformat fills that need making the creation of such functions
;; quite simple.

;;; v 1.3
;; * If no args are passed, then don't attempt to post-format the string.
;; * Format reversing functions `sformat-just-before-token-regexp' and
;;   `sformat-just-after-token-regexp'

;;; v 1.2
;; Sformat has been sped up by using string commands (string-match,
;; and substring) to quickly scan over plain text, and then a slower
;; character by character scan to handle tokens.

;;; $Id: sformat.el,v 1.1 2003/09/24 08:03:37 ponced Exp $
;;
;; History
;;
;; eml: 1998/09/11
;; XEmacs no longer treats a ?c character as an integer.  Change this
;; check to look at it as a char-or-string.
;; eml 8/17/94
;; Added positional data, and ability to handle lambda functions in
;; options list for more general use.
;; eml 5/3/95
;; Added speed up mentioned above
;; eml 9/8/96
;; Fixed error where if sizing number = string length, nothing came out
;; Added ability to pad a string w/ 0s.  Don't use w/ neg argument. ;)
;; Added speedup to a list searches

;;; Code:
(eval-when-compile
  ;; Silence the byte-compiler
  (defvar newstr nil))

(defvar Sformat-default-format 'Sformat-default-format-method
  "Function used when the input param is a string and not a function.
This function must conform to the following parameters:
1 - string to be formatted
2 - A1
3 - A2
4 - A1 default or fn
5 - A2 default or fn")

(defvar Sformat-formatting nil
  "This flags when Sformat is currently formatting a string.")

(defun Sformat-point ()
  "Return the current offset in the string being formated.
Called from % token lambda expressions when needed."
  (length newstr))

(defun Sformat-column ()
  "Return the current column inside a string being formatted.
Used from % token lambda expressions."
  (let ((ts newstr))
    (while (string-match "\\(\n\\)" ts)
      (setq ts (substring ts (match-end 1))))
    (length ts)))

(defun Sformat (extensions fmt &rest args)
  "Provide a simple means of formatting strings with special % options.
This will use EXTENSIONS to fill out FMT, and then pass the
result to #<subr format> with ARGS.  EXTENSIONS is of the form:
      '( (?F local-filename A1default A2default)
         (?U local-username) )

where F is the character after a %, and 'local-filename is a variable
or function.  If it is a function, it must be able to take 2 numeric
arguments.  The args can be used for whatever purpose you desire for
the function.  A string or variable holding a string will have it's
value inserted just as `Sformat-string-word-format' would cut it up.
This action can be modified by changing what the variable
`Sformat-default-format' points to.  A1default and A2default can be
either default values for A1 or A2, or symbols to be used when cutting
this specific string into little pieces.  Numbers are formatted as per
%d with A#defaults being used (numeric only).  Lambda functions passed
in directly as lists will be evaled with no parameters.  Anything else
will be inserted as %S would with A#defaults being used (numeric
only).

Viable formats would be:
   %-10v         - 10 chars, pad left
   %.1v %:1v     - first word
   %10.2v %10:2v - 10 chars, pad right for first 2 words
   %03v          - at least 3 chars, padded w/ zeros at beginning

   where v is some format character.  Note that .  and : are interchangeable

      (Sformat extensions fmt &rest args)"

  ;; verify arguments
  (if (not (listp extensions))
      (signal 'wrong-type-argument (list 'listp extensions)))
  (if (not (stringp fmt))
      (signal 'wrong-type-argument (list 'stringp fmt)))

  (let ((Sformat-formatting t)		;Yes, we are formatting something
	(cnt 0)				;position in string
	(tl nil)			;temp list of extensions
	(ln (length fmt))		;length of fmt string
	(tc nil)			;temp char
	(newstr "")			;the new string
	(pcnt nil)			;% symbol flag
	(dot nil)			;. symbol flag
	(neg1 nil)			;- symbol flag on arg1
	(neg2 nil)			;- symbol flag on arg2
	(zpad nil)			;numeric starts /w 0
	(A1 nil)			;arg 1
	(A2 nil))			;arg 2
    (while (/= (length fmt) 0)
      (if (string-match "\\(%\\)" fmt)
	  (progn
	    (setq newstr (concat newstr (substring fmt 0 (match-beginning 1))))
	    (setq fmt (substring fmt (match-end 1)))
	    (setq pcnt t))
	(setq newstr (concat newstr fmt))
	(setq fmt ""))
      (setq cnt 0)
      (while pcnt
	(setq tc (aref fmt cnt))
	(if (not pcnt)
	    (if (= tc ?%)
		(setq pcnt t)
	      (setq newstr (concat newstr (char-to-string tc))))
	  (cond
	   ((or (= tc ?.) (= tc ?:))	;. such as %1.2F
	    (if dot
		(error "Too many .  or : in %% formatter!")
	      (setq dot t)))
	   ((= tc ?-)			;- such as %-1F
	    (if dot
		(if A2 (error "Cannot use '-' in middle of numeric arg")
		  (setq neg2 t))
	      (if A1 (error "Cannot use '-' in middle of numeric arg")
		(setq neg1 t))))
	   ((and (<= tc ?9) (>= tc ?0))	;number arg
	    (if dot
		(progn
		  (if (not A2) (setq A2 0))
		  (setq A2 (+ (* A2 10) (- tc ?0))))
	      (if (not A1) (progn
			     ;; check for 0 padding
			     (if (= tc ?0) (setq zpad t))
			     (setq A1 0)))
	      (setq A1 (+ (* A1 10) (- tc ?0)))))
	   (t				;the F in %F
	    (setq tl (assoc tc extensions))
	    ;; negafy A1 and A2 if need be.
	    (if (and neg1 A1) (setq A1 (- A1)))
	    (if (and neg2 A2) (setq A2 (- A2)))
	    ;; if we don't find it, pass through verbatim
	    (if (not tl)
		(let ((tmpstr (concat "%"
				      (if A1 (format "%d" A1))
				      (if A2 (format ".%d" A2))
				      (char-to-string tc))))
		  (setq newstr (concat newstr tmpstr)))
	      (if (not (char-or-string-p (car tl)))
		  (error "Invalid extensions list passed to Sformat"))
	      
	      (if (and (not A1) (numberp (car (cdr (cdr tl)))))
		  (setq A1 (car (cdr (cdr tl)))))
	      (if (and (not A2) (numberp (car (cdr (cdr (cdr tl))))))
		  (setq A2 (car (cdr (cdr (cdr tl))))))
	      
	      (let* ((v (car (cdr tl)))
		     (sym (if (symbolp v) (eval v) v))
		     (tmpstr (cond
			      ((and (symbolp sym) (fboundp sym))
			       (funcall sym A1 A2))
			      ((and (listp sym) (equal (car sym) 'lambda))
			       (funcall sym))
			      ((byte-code-function-p sym)
			       (funcall sym))
			      ((stringp sym)
			       (let ((m1 (car (cdr (cdr tl)))))
				 (if zpad
				     (if m1 (setq m1 (intern
						      (symbol-name m1)
						      "-0"))
				       (setq m1 'both-0)))
				 (funcall Sformat-default-format
					  sym A1 A2 m1
					  (car (cdr (cdr (cdr tl)))))))
			      ((numberp sym)
			       (setq zpad (if zpad "0" ""))
			       (format (concat "%"
					       (if A1 (format
						       (concat zpad"%d")
						       A1))
					       (if A2 (format ".%d" A2))
					       "d")
				       sym))
			      (t
			       (format (concat "%"
					       (if A1 (format "%d" A1))
					       (if A2 (format ".%d" A2))
					       "S")
				       sym)))))
		(setq newstr (concat newstr tmpstr))))
	    (setq A1 nil A2 nil neg1 nil neg2 nil zpad nil dot nil pcnt nil)
	    )
	   )
	  )
	(setq cnt (1+ cnt))
	)
      (setq fmt (substring fmt cnt))
      )
    (if args (funcall 'format newstr args) newstr)
    ))

(defun Sformat-default-format-method (str A1 A2 A1def A2def)
  "Format routine used when the format method is a string.
STR is the text to be formated.  A1 and A2 represent the passed in
format adjustors.  (Of the form %A1.A2C) where C is a code, and A1
and A2 are numbers.  A1DEF and A2DEF are default values."
  ;; check for numbers in defaults, and nil them if need be
  (if (numberp A1def) (setq A1def nil))
  (if (numberp A2def) (setq A2def nil))
  (Sformat-string-word-format str A1 A2 A1def A2def)
  )

;;; The next few routines are for support to make writing your own
;; formating routines much easier.

(defun Sformat-string-word-format (str A1 A2 method1 method2)
  "Support routine which will adjust STR by the given restrictions.
A1 and A2 are dimension bounds for the string.  METHOD1 and METHOD2 define
how those dimensions are used.

A1 represents character limits, and A2 is based on words where a word is
terminated by METHOD2 regexp.  A1 formatting always overrides
A2 for length.  If A1 is negative, pad right, else pad left to fill to
A1 length.

   Values of METHOD1 are:
   'fill-only    - If (length STR) < A1, pad (left or right), but do
                  not shorten
   'fill-only-0  - As above, pad with 0
   'shorten-only - If (length STR) > A1, cut back, but do not pad to
                  make STR A1 characters
   'shorten-only-0 - A convenience
   nil, 'both    - If STR is too short, pad, if too long, shorten.
   'both-0       - As above, padding with 0

   Values of METHOD2 are:
   nil, \"[a-zA-Z0-9_]*\"  - cut by word, where a word includes numbers
                             and '_'
   string (regexp)         - trim out given white space replacing with
                             one space, with A2 words in string
   'preceeding-space       - if A2, the add space to beginning of str

   Other notes:

   The word trimmer automatically always leaves white-space in front
of each word, thus choochoo.ultranet.com => choochoo.ultranet.com,
not choochoo ultranet com."

  (if (not method1) (setq method1 'both))
  (if (not method2) (setq method2 "[a-zA-Z0-9_]*"))

   (let* ((pad nil)
	  (newstr nil)
	  (rstr nil)
	  (zpad (string-match "-0" (symbol-name method1)))
	  (A1fl (and A1 (< A1 0)))
	 )
     (if (and A1 (numberp A1))
	 (setq A1 (abs A1)))

     ;; first, cut by A2, if A2 exists.
     (if (or (not A2) (not (stringp method2)))
	 (setq newstr str)
       (let ((notrim (progn
		       (string-match "\\(\\[\\)" method2)
		       (concat
			(substring method2 0 (match-end 1))
			"^"
			(substring method2 (match-end 1)))
		       )))
	 (while (and (< 0 A2) ( string-match (concat notrim
						     "\\("
						     method2
						     "\\)")
					     str))
	   (if newstr
	       (setq newstr (concat newstr
				    (substring str 0 (match-end 1))))
	     (setq newstr (substring str (match-beginning 1)
				     (match-end 1))))
	   (setq str (substring str (match-end 1)))
	   (setq A2 (1- A2)))))
     ;; Now, cut up newstr by A1 specs!
     (cond
      ((stringp method2)
       (if (not A1)
	   (setq rstr newstr)
	 (if (and (< (length newstr) A1)
		  (member method1 '(both both-0 fill-only fill-only-0)))
	     (progn
	       ;; fill specifications
	       (setq pad (make-string (- A1 (length newstr)) (if zpad ?0 ? )))
	       (if A1fl
		   (setq rstr (concat newstr pad))
		 (setq rstr (concat pad newstr)))))
	 ;; cut specifications
	 (if (and (>= (length newstr) A1)
		  (member method1 '(both both-0 shorten-only shorten-only-0)))
	     (setq rstr (substring newstr 0 A1)))))
      ((and (eq (eval method2) 'preceeding-space)
	    (integerp A2)
	    (not (eq A2 0))
	    (> (length newstr) 0))
       (setq rstr (concat " " newstr)))
      (t
       (setq rstr newstr)))
     
     rstr)
   )


;;; Sformat string managers
;;
;; These two routines find the string between different % tokens, and
;; returns them as regular expressions vie regexp-quote.  The result
;; will allow a program to find text surrounding major parts within a
;; format string.
;;
;; This is useful if you want to examine text inserted with sformat
;; and extract data stuck in originally.

(defun sformat-just-before-token-regexp (token format)
  "Return a search expression for text before TOKEN in FORMAT.
This search string can be used to find the text residing in TOKEN
if it were inserted with FORMAT in the past."
  (let ((rs nil) (case-fold-search nil))
    (if (string-match (concat "\\(%" (char-to-string token) "\\)") format)
	(progn
	  (setq rs (substring format 0 (match-beginning 1)))
	  ;; scan for previous tokens and shorten
	  (while (string-match "\\(\\`\\|[^%]\\)\\(%\\)\\(\\'\\|[^%]\\)" rs)
	    (setq rs (substring rs (+ (match-end 2) 1))))
	  (regexp-quote rs))
      nil)))

(defun sformat-just-after-token-regexp (token format)
  "Return a search expression for text after TOKEN in FORMAT.
This search string can be used to find the text residing in TOKEN
if it were inserted with FORMAT in the past."
  (let ((rs nil) (case-fold-search nil))
    (if (string-match (concat "\\(%" (char-to-string token) "\\)") format)
	(progn
	  (setq rs (substring format (match-end 1)))
	  (if (string-match "\\(\\`\\|[^%]\\)\\(%\\)\\(\\'\\|[^%]\\)" rs)
	      (setq rs (substring rs 0 (match-beginning 2))))
	  (regexp-quote rs))
      nil)))

(provide 'sformat)
;;; sformat ends here
