;;; csv-mode.el --- major mode for editing comma-separated value files

;; Copyright (C) 2003, 2004 Francis J. Wright

;; Author: Francis J. Wright <F.J.Wright at qmul.ac.uk>
;; Time-stamp: <23 August 2004>
;; URL: http://centaur.maths.qmul.ac.uk/Emacs/
;; Version: $Id: csv-mode.el,v 1.50 2004/08/23 17:51:26 fjw Exp $
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package is intended for use with GNU Emacs 21 (only) and
;; implements the following commands to process records of CSV
;; (comma-separated value) type: `csv-sort-fields' and
;; `csv-sort-numeric-fields' sort respectively lexicographically and
;; numerically on a specified field or column; `csv-reverse-region'
;; reverses the order.  They are based closely on, and use, code in
;; `sort.el'.  `csv-kill-fields' and `csv-yank-fields' respectively
;; kill and yank fields or columns, although they do not use the
;; normal kill ring.  `csv-kill-fields' can kill more than one field
;; at once, but multiple killed fields can be yanked only as a fixed
;; group equivalent to a single field.  `csv-align-fields' aligns
;; fields into columns; `csv-unalign-fields' undoes such alignment;
;; separators can be hidden within aligned records.  `csv-transpose'
;; interchanges rows and columns.  For details, see the documentation
;; for the individual commands.

;; CSV mode supports a generalised comma-separated values format
;; (character-separated values) in which the fields can be separated
;; by any of several single characters, specified by the value of the
;; customizable user option `csv-separators'.  CSV data fields can be
;; delimited by quote characters (and must if they contain separator
;; characters).  This implementation supports quoted fields, where the
;; quote characters allowed are specified by the value of the
;; customizable user option `csv-field-quotes'.  By default, the only
;; separator is a comma and the only field quote is a double quote.
;; These user options can be changed ONLY by CUSTOMIZING them,
;; e.g. via the command `customize-variable'.

;; CSV mode commands ignore blank lines and comment lines beginning
;; with the value of the buffer local variable `csv-comment-start',
;; which by default is #.  The user interface is similar to that of
;; the standard commands `sort-fields' and `sort-numeric-fields', but
;; see the major mode documentation below.

;; The global minor mode `csv-field-index-mode' provides display of
;; the current field index in the mode line, cf. `line-number-mode'
;; and `column-number-mode'.  It is on by default.

;;; Installation:

;; Put this file somewhere that Emacs can find it (i.e. in one of the
;; directories in your `load-path' such as `site-lisp'), optionally
;; byte-compile it (recommended), and put this in your .emacs file:
;;
;; (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
;; (autoload 'csv-mode "csv-mode"
;;   "Major mode for editing comma-separated value files." t)

;;; History:

;; Begun on 15 November 2003 to provide lexicographic sorting of
;; simple CSV data by field and released as csv.el.  Facilities to
;; kill multiple fields and customize separator added on 9 April 2004.
;; Converted to a major mode and renamed csv-mode.el on 10 April 2004,
;; partly at the suggestion of Stefan Monnier <monnier at
;; IRO.UMontreal.CA> to avoid conflict with csv.el by Ulf Jasper.
;; Field alignment, comment support and CSV mode customization group
;; added on 1 May 2004.  Support for index ranges added on 6 June
;; 2004.  Multiple field separators added on 12 June 2004.
;; Transposition added on 22 June 2004.  Separator invisibility added
;; on 23 June 2004.

;;; See also:

;; the standard GNU Emacs 21 packages align.el, which will align
;; columns within a region, and delim-col.el, which helps to prettify
;; columns in a text region or rectangle;

;; csv.el by Ulf Jasper <ulf.jasper at web.de>, which provides
;; functions for reading/parsing comma-separated value files and is
;; available at http://de.geocities.com/ulf_jasper/emacs.html (and in
;; the gnu.emacs.sources archives).

;;; To do (maybe):

;; Make separators and quotes buffer-local and locally settable.
;; Support (La)TeX tables: set separator and comment; support record
;; end string.
;; Convert comma-separated to space- or tab-separated.

;;; Code:

(defgroup CSV nil
  "Major mode for editing files of comma-separated value type."
  :group 'convenience)

(defvar csv-separator-chars nil
  "Field separators as a list of character.
Set by customizing `csv-separators' -- do not set directly!")

(defvar csv-separator-regexp nil
  "Regexp to match a field separator.
Set by customizing `csv-separators' -- do not set directly!")

(defvar csv-skip-regexp nil
  "Regexp used by `skip-chars-forward' etc. to skip fields.
Set by customizing `csv-separators' -- do not set directly!")

(defvar csv-font-lock-keywords nil
  "Font lock keywords to highlight the field separators in CSV mode.
Set by customizing `csv-separators' -- do not set directly!")

(defcustom csv-separators '(",")
  "Field separators: a list of *single-character* strings.
For example: (\",\"), the default, or (\",\" \";\" \":\").
Neighbouring fields may be separated by any one of these characters.
The first is used when inserting a field separator into the buffer.
All must be different from the field quote characters, `csv-field-quotes'."
  ;; Suggested by Eckhard Neber <neber@mwt.e-technik.uni-ulm.de>
  :group 'CSV
  :type '(repeat string)
  ;; Character would be better, but in Emacs 21.3 does not display
  ;; correctly in a customization buffer.
  :set (lambda (variable value)
	 (mapc (lambda (x)
		 (if (or (/= (length x) 1)
			 (and (boundp 'csv-field-quotes)
			      (member x csv-field-quotes)))
		     (error)))
	       value)
	 (custom-set-default variable value)
	 (setq csv-separator-chars (mapcar 'string-to-char value)
	       csv-skip-regexp (apply 'concat "^\n" csv-separators)
	       csv-separator-regexp (apply 'concat `("[" ,@value "]"))
	       csv-font-lock-keywords
	       ;; NB: csv-separator-face variable evaluates to itself.
	       `((,csv-separator-regexp . csv-separator-face)))))

(defcustom csv-field-quotes '("\"")
  "Field quotes: a list of *single-character* strings.
For example: (\"\\\"\"), the default, or (\"\\\"\" \"'\" \"`\").
A field can be delimited by a pair of any of these characters.
All must be different from the field separators, `csv-separators'."
  :group 'CSV
  :type '(repeat string)
  ;; Character would be better, but in Emacs 21 does not display
  ;; correctly in a customization buffer.
  :set (lambda (variable value)
	 (mapc (lambda (x)
		 (if (or (/= (length x) 1)
			 (member x csv-separators))
		     (error)))
	       value)
	 (when (boundp 'csv-mode-syntax-table)
	   ;; FIRST remove old quote syntax:
	   (with-syntax-table text-mode-syntax-table
	     (mapc (lambda (x)
		     (modify-syntax-entry
		      (string-to-char x)
		      (string (char-syntax (string-to-char x)))
		      ;; symbol-value to avoid compiler warning:
		      (symbol-value 'csv-mode-syntax-table)))
		   csv-field-quotes))
	   ;; THEN set new quote syntax:
	   (csv-set-quote-syntax value))
	 ;; BEFORE setting new value of `csv-field-quotes':
	 (custom-set-default variable value)))

(defun csv-set-quote-syntax (field-quotes)
  "Set syntax for field quote characters FIELD-QUOTES to be \"string\".
FIELD-QUOTES should be a list of single-character strings."
  (mapc (lambda (x)
	  (modify-syntax-entry
	   (string-to-char x) "\""
	   ;; symbol-value to avoid compiler warning:
	   (symbol-value 'csv-mode-syntax-table)))
	field-quotes))

(defvar csv-comment-start nil
  "String that starts a comment line, or nil if no comment syntax.
Such comment lines are ignored by CSV mode commands.
This variable is buffer local\; its default value is that of
`csv-comment-start-default'.  It is set by the function
`csv-set-comment-start' -- do not set it directly!")

(make-variable-buffer-local 'csv-comment-start)

(defcustom csv-comment-start-default "#"
  "String that starts a comment line, or nil if no comment syntax.
Such comment lines are ignored by CSV mode commands.
Default value of buffer-local variable `csv-comment-start'.
Changing this variable does not affect any existing CSV mode buffer."
  :group 'CSV
  :type '(choice (const :tag "None" nil) string)
  :set (lambda (variable value)
	 (custom-set-default variable value)
	 (set-default 'csv-comment-start value)))

(defcustom csv-align-style 'left
  "Aligned field style: one of 'left, 'centre, 'right or 'auto.
Alignment style used by `csv-align-fields'.
Auto-alignment means left align text and right align numbers."
  :group 'CSV
  :type '(choice (const left) (const centre)
		 (const right) (const auto)))

(defcustom csv-align-padding 1
  "Aligned field spacing: must be a positive integer.
Number of spaces used by `csv-align-fields' after separators."
  :group 'CSV
  :type 'integer)

(defcustom csv-header-lines 0
  "Header lines to skip when setting region automatically."
  :group 'CSV
  :type 'integer)

(defcustom csv-invisibility-default nil
  "If non-nil, make separators in aligned records invisible."
  :group 'CSV
  :type 'boolean)

(defface csv-separator-face
  '((((class color)) (:foreground "red"))
    (t (:weight bold)))
  "CSV mode face used to highlight separators."
  :group 'CSV)

;; This mechanism seems to keep XEmacs happy:
(defvar csv-separator-face 'csv-separator-face
  "Face name to use to highlight separators.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Mode definition, key bindings and menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst csv-mode-line-help-echo
  ;; See bindings.el for details of `mode-line-format' construction.
  nil
  "Primary default mode line help echo text.")

(defconst csv-mode-line-format
  ;; See bindings.el for details of `mode-line-format' construction.
  (append (butlast default-mode-line-format 2)
	  (cons `(csv-field-index-string
		  ("" csv-field-index-string
		   ,(propertize "--" 'help-echo csv-mode-line-help-echo)))
		(last default-mode-line-format 2)))
  "Mode line format string for CSV mode.")

(define-derived-mode csv-mode text-mode "CSV"
  "Major mode for editing files of comma-separated value type.

CSV mode is derived from `text-mode', and runs `text-mode-hook' before
running `csv-mode-hook'.  It turns `auto-fill-mode' off by default.
CSV mode can be customized by user options in the CSV customization
group.  The separators are specified by the value of `csv-separators'.

CSV mode commands ignore blank lines and comment lines beginning with
the value of `csv-comment-start', which delimit \"paragraphs\".
\"Sexp\" is re-interpreted to mean \"field\", so that `forward-sexp'
\(\\[forward-sexp]), `kill-sexp' (\\[kill-sexp]), etc. all apply to fields.
Standard comment commands apply, such as `comment-dwim' (\\[comment-dwim]).

If `font-lock-mode' is enabled then separators, quoted values and
comment lines are highlighted using respectively `csv-separator-face',
`font-lock-string-face' and `font-lock-comment-face'.

The user interface (UI) for CSV mode commands is similar to that of
the standard commands `sort-fields' and `sort-numeric-fields', except
that if there is no prefix argument then the UI prompts for the field
index or indices.  In `transient-mark-mode' only: if the region is not
set then the UI attempts to set it to include all consecutive CSV
records around point, and prompts for confirmation; if there is no
prefix argument then the UI prompts for it, offering as a default the
index of the field containing point if the region was not set
explicitly.  The region set automatically is delimited by blank lines
and comment lines, and the number of header lines at the beginning of
the region given by the value of `csv-header-lines' are skipped.

Sort order is controlled by `csv-descending'.

CSV mode provides the following specific keyboard key bindings:

\\{csv-mode-map}"
  (turn-off-auto-fill)
  ;; Set syntax for field quotes:
  (csv-set-quote-syntax csv-field-quotes)
  ;; Make sexp functions apply to fields:
  (set (make-local-variable 'forward-sexp-function) 'csv-forward-field)
  ;; Paragraph means a group of contiguous records:
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  ;; Comment support:
  (make-local-variable 'comment-start)
  (csv-set-comment-start csv-comment-start)
  (setq
   ;; Font locking -- separator plus syntactic:
   font-lock-defaults '(csv-font-lock-keywords)
   buffer-invisibility-spec csv-invisibility-default
   ;; Mode line to support `csv-field-index-mode':
   mode-line-format csv-mode-line-format)
  ;; Enable or disable `csv-field-index-mode' (could probably do this
  ;; a bit more efficiently):
  (csv-field-index-mode (symbol-value 'csv-field-index-mode)))

(defun csv-set-comment-start (string)
  "Set comment start for this CSV mode buffer to STRING.
It must be either a string or nil."
  (interactive
   (list (edit-and-eval-command
	  "Comment start (string or nil): " csv-comment-start)))
  (setq csv-comment-start string
	paragraph-separate "[:space:]*$" ; white space
	paragraph-start "\n")		; must include \n explicitly!
  (if string
      (progn
	(setq paragraph-separate (concat paragraph-separate "\\|" string)
	      paragraph-start (concat paragraph-start "\\|" string)
	      comment-start string)
	(modify-syntax-entry
	 (string-to-char string) "<" csv-mode-syntax-table)
	(modify-syntax-entry ?\n ">" csv-mode-syntax-table))
    (with-syntax-table text-mode-syntax-table
      (modify-syntax-entry (string-to-char string)
			   (string (char-syntax (string-to-char string)))
			   csv-mode-syntax-table)
      (modify-syntax-entry ?\n
			   (string (char-syntax ?\n))
			   csv-mode-syntax-table))))

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(define-key csv-mode-map [(control ?c) (control ?v)] 'csv-toggle-invisibility)
(define-key csv-mode-map [(control ?c) (control ?t)] 'csv-transpose)
(define-key csv-mode-map [(control ?c) (control ?c)] 'csv-set-comment-start)
(define-key csv-mode-map [(control ?c) (control ?u)] 'csv-unalign-fields)
(define-key csv-mode-map [(control ?c) (control ?a)] 'csv-align-fields)
(define-key csv-mode-map [(control ?c) (control ?z)] 'csv-yank-as-new-table)
(define-key csv-mode-map [(control ?c) (control ?y)] 'csv-yank-fields)
(define-key csv-mode-map [(control ?c) (control ?k)] 'csv-kill-fields)
(define-key csv-mode-map [(control ?c) (control ?d)] 'csv-toggle-descending)
(define-key csv-mode-map [(control ?c) (control ?r)] 'csv-reverse-region)
(define-key csv-mode-map [(control ?c) (control ?n)] 'csv-sort-numeric-fields)
(define-key csv-mode-map [(control ?c) (control ?s)] 'csv-sort-fields)

(defvar csv-descending nil
  "If non-nil, CSV mode sort functions sort in order of descending sort key.
Usually they sort in order of ascending sort key.")

(defun csv-toggle-descending ()
  "Toggle `csv-descending'."
  (interactive)
  (setq csv-descending (not csv-descending))
  (message "Sort order is %sscending" (if csv-descending "de" "a")))

(defun csv-toggle-invisibility ()
  "Toggle `buffer-invisibility-spec'."
  (interactive)
  (setq buffer-invisibility-spec (not buffer-invisibility-spec))
  (message "Separators in aligned records will be %svisible \
\(after re-aligning if soft\)"
	   (if buffer-invisibility-spec "in" ""))
  (redraw-frame (selected-frame)))

(easy-menu-define
  csv-menu
  csv-mode-map
  "CSV major mode menu keymap"
  '("CSV"
    ["Sort By Field Lexicographically" csv-sort-fields :active t
     :help "Sort lines in region lexicographically by the specified field"]
    ["Sort By Field Numerically" csv-sort-numeric-fields :active t
     :help "Sort lines in region numerically by the specified field"]
    ["Reverse Order of Lines" csv-reverse-region :active t
     :help "Reverse the order of the lines in the region"]
    ["Use Descending Sort Order" csv-toggle-descending :active t
     :style toggle :selected csv-descending
     :help "If selected, use descending order when sorting"]
    "--"
    ["Kill Fields (Columns)" csv-kill-fields :active t
     :help "Kill specified fields of each line in the region"]
    ["Yank Fields (Columns)" csv-yank-fields :active t
     :help "Yank killed fields as specified field of each line in region"]
    ["Yank As New Table" csv-yank-as-new-table :active t
     :help "Yank killed fields as a new table at point"]
    ["Align Fields into Columns" csv-align-fields :active t
     :help "Align the start of every field of each line in the region"]
    ["Unalign Columns into Fields" csv-unalign-fields :active t
     :help "Undo soft alignment and optionally remove redundant white space"]
    ["Transpose Rows and Columns" csv-transpose :active t
     :help "Rewrite rows (which may have different lengths) as columns"]
    "--"
    ["Forward Field" forward-sexp :active t
     :help "Move forward across one field\; with ARG, do it that many times"]
    ["Backward Field" backward-sexp :active t
     :help "Move backward across one field\; with ARG, do it that many times"]
    ["Kill Field Forward" kill-sexp :active t
     :help "Kill field following cursor\; with ARG, do it that many times"]
    ["Kill Field Backward" backward-kill-sexp :active t
     :help "Kill field preceding cursor\; with ARG, do it that many times"]
    "--"
    ("Alignment Style"
     ["Left" (setq csv-align-style 'left) :active t
      :style radio :selected (eq csv-align-style 'left)
      :help "If selected, `csv-align-fields' left aligns fields"]
     ["Centre" (setq csv-align-style 'centre) :active t
      :style radio :selected (eq csv-align-style 'centre)
      :help "If selected, `csv-align-fields' centres fields"]
     ["Right" (setq csv-align-style 'right) :active t
      :style radio :selected (eq csv-align-style 'right)
      :help "If selected, `csv-align-fields' right aligns fields"]
     ["Auto" (setq csv-align-style 'auto) :active t
      :style radio :selected (eq csv-align-style 'auto)
      :help "\
If selected, `csv-align-fields' left aligns text and right aligns numbers"]
     )
    ["Show Current Field Index" csv-field-index-mode :active t
     :style toggle :selected csv-field-index-mode
     :help "If selected, display current field index in mode line"]
    ["Make Separators Invisible" csv-toggle-invisibility :active t
     :style toggle :selected buffer-invisibility-spec
     :help "If selected, separators in aligned records are invisible"]
    ["Set Buffer's Comment Start" csv-set-comment-start :active t
     :help "Set comment start string for this buffer"]
    ["Customize CSV Mode" (customize-group 'CSV) :active t
     :help "Open a customization buffer to change CSV mode options"]
    ))

(require 'sort)

(defsubst csv-not-looking-at-record ()
  "Return t if looking at blank or comment line, nil otherwise.
Assumes point is at beginning of line."
  (looking-at paragraph-separate))

(defun csv-interactive-args (&optional type)
  "Get arg or field(s) and region interactively, offering sensible defaults.
Signal an error if the buffer is read-only.
If TYPE is noarg then return a list `(beg end)'.
Otherwise, return a list `(arg beg end)', where arg is:
  the raw prefix argument by default\;
  a single field index if TYPE is single\;
  a list of field indices or index ranges if TYPE is multiple.
Field defaults to the current prefix arg\; if not set, prompt user.

A field index list consists of positive or negative integers or ranges,
separated by any non-integer characters.  A range has the form m-n,
where m and n are positive or negative integers, m < n, and n defaults
to the last field index if omitted.

In transient mark mode, if the mark is not active then automatically
select and highlight CSV records around point, and query user.
The default field when read interactively is the current field."
  ;; Must be run interactively to activate mark!
  (let* ((arg current-prefix-arg) (default-field 1)
	 (region
	  (if (and transient-mark-mode (not mark-active))
	      ;; Set region automatically:
	      (save-excursion
		(let (startline lbp)
		  (if arg
		      (beginning-of-line)
		    (setq lbp (line-beginning-position))
		    (while (re-search-backward csv-separator-regexp lbp 1)
		      ;; Move as far as possible, i.e. to beginning of line.
		      (setq default-field (1+ default-field))))
		  (if (csv-not-looking-at-record)
		      (error "Point may not be within CSV records"))
		  (setq startline (point))
		  ;; Set mark at beginning of region:
		  (while (not (or (bobp) (csv-not-looking-at-record)))
		    (forward-line -1))
		  (if (csv-not-looking-at-record) (forward-line 1))
		  ;; Skip header lines:
		  (forward-line csv-header-lines)
		  (set-mark (point))	; OK since in save-excursion
		  ;; Move point to end of region:
		  (goto-char startline)
		  (beginning-of-line)
		  (while (not (or (eobp) (csv-not-looking-at-record)))
		    (forward-line 1))
		  ;; Show mark briefly if necessary:
		  (unless (and (pos-visible-in-window-p)
			       (pos-visible-in-window-p (mark)))
		    (exchange-point-and-mark)
		    (sit-for 1)
		    (exchange-point-and-mark))
		  (or (y-or-n-p "Region OK? ")
		      (error "Action aborted by user"))
		  (message nil)		; clear y-or-n-p message
		  (list (region-beginning) (region-end))))
	    ;; Use region set by user:
	    (list (region-beginning) (region-end)))))
    (setq default-field (number-to-string default-field))
    (cond
     ((eq type 'multiple)
      (if arg
	  ;; Ensure that field is a list:
	  (or (consp arg)
	      (setq arg (list (prefix-numeric-value arg))))
	;; Read field interactively, ignoring non-integers:
	(setq arg
	      (mapcar
	       (lambda (x)
		 (if (string-match "-" x 1) ; not first character
		     ;; Return a range as a pair - the cdr may be nil:
		     (let ((m (substring x 0 (match-beginning 0)))
			   (n (substring x (match-end 0))))
		       (cons (car (read-from-string m))
			     (and (not (string= n ""))
				  (car (read-from-string n)))))
		   ;; Return a number as a number:
		   (car (read-from-string x))))
	       (split-string
		(read-string
		 "Fields (sequence of integers or ranges): " default-field)
		"[^-+0-9]+")))))
     ((eq type 'single)
      (if arg
	  (setq arg (prefix-numeric-value arg))
	(while (not (integerp arg))
	  (setq arg (eval-minibuffer "Field (integer): " default-field))))))
    (if (eq type 'noarg) region (cons arg region))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Sorting by field
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csv-nextrecfun ()
  "Called by `csv-sort-fields-1' with point at end of previous record.
It moves point to the start of the next record.
It should move point to the end of the buffer if there are no more records."
  (forward-line)
  (while (and (not (eobp)) (csv-not-looking-at-record))
    (forward-line)))

(defun csv-sort-fields-1 (field beg end startkeyfun endkeyfun)
  "Modified version of `sort-fields-1' that skips blank or comment lines.

FIELD is a single field index, and BEG and END specify the region to
sort.

STARTKEYFUN moves from the start of the record to the start of the key.
It may return either a non-nil value to be used as the key, or
else the key is the substring between the values of point after
STARTKEYFUN and ENDKEYFUN are called.  If STARTKEYFUN is nil, the key
starts at the beginning of the record.

ENDKEYFUN moves from the start of the sort key to the end of the sort key.
ENDKEYFUN may be nil if STARTKEYFUN returns a value or if it would be the
same as ENDRECFUN."
  (let ((tbl (syntax-table)))
    (if (zerop field) (setq field 1))
    (unwind-protect
	(save-excursion
	  (save-restriction
	    (narrow-to-region beg end)
	    (goto-char (point-min))
	    (set-syntax-table sort-fields-syntax-table)
	    (sort-subr csv-descending
		       'csv-nextrecfun 'end-of-line
		       startkeyfun endkeyfun)))
      (set-syntax-table tbl))))

(defun csv-sort-fields (field beg end)
  "Sort lines in region lexicographically by the ARGth field of each line.
If not set, the region defaults to the CSV records around point.
Fields are separated by `csv-separators' and null fields are allowed anywhere.
Field indices increase from 1 on the left or decrease from -1 on the right.
A prefix argument specifies a single field, otherwise prompt for field index.
Ignore blank and comment lines.  The variable `sort-fold-case'
determines whether alphabetic case affects the sort order.
When called non-interactively, FIELD is a single field index\;
BEG and END specify the region to sort."
  ;; (interactive "*P\nr")
  (interactive (csv-interactive-args 'single))
  (barf-if-buffer-read-only)
  (csv-sort-fields-1 field beg end
		     (lambda () (csv-sort-skip-fields field) nil)
		     (lambda () (skip-chars-forward csv-skip-regexp))))

(defun csv-sort-numeric-fields (field beg end)
  "Sort lines in region numerically by the ARGth field of each line.
If not set, the region defaults to the CSV records around point.
Fields are separated by `csv-separators'.
Null fields are allowed anywhere and sort as zeros.
Field indices increase from 1 on the left or decrease from -1 on the right.
A prefix argument specifies a single field, otherwise prompt for field index.
Specified non-null field must contain a number in each line of the region,
which may begin with \"0x\" or \"0\" for hexadecimal and octal values.
Otherwise, the number is interpreted according to sort-numeric-base.
Ignore blank and comment lines.
When called non-interactively, FIELD is a single field index\;
BEG and END specify the region to sort."
  ;; (interactive "*P\nr")
  (interactive (csv-interactive-args 'single))
  (barf-if-buffer-read-only)
  (csv-sort-fields-1 field beg end
		 (lambda ()
		   (csv-sort-skip-fields field)
		   (let* ((case-fold-search t)
			  (base
			   (if (looking-at "\\(0x\\)[0-9a-f]\\|\\(0\\)[0-7]")
			       (cond ((match-beginning 1)
				      (goto-char (match-end 1))
				      16)
				     ((match-beginning 2)
				      (goto-char (match-end 2))
				      8)
				     (t nil)))))
		     (string-to-number (buffer-substring (point)
							 (save-excursion
							   (forward-sexp 1)
							   (point)))
				       (or base sort-numeric-base))))
		 nil))

(defun csv-reverse-region (beg end)
  "Reverse the order of the lines in the region.
This is just a CSV-mode style interface to `reverse-region', which is
the function that should be used non-interactively.  It takes two
point or marker arguments, BEG and END, delimiting the region."
  ;; (interactive "*P\nr")
  (interactive (csv-interactive-args 'noarg))
  (barf-if-buffer-read-only)
  (reverse-region beg end))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Moving by field
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst csv-end-of-field ()
  "Skip forward over one field."
  (skip-syntax-forward " ")
  (if (eq (char-syntax (following-char)) ?\")
      (goto-char (scan-sexps (point) 1)))
  (skip-chars-forward csv-skip-regexp))

(defsubst csv-beginning-of-field ()
  "Skip backward over one field."
  (skip-syntax-backward " ")
  (if (eq (char-syntax (preceding-char)) ?\")
      (goto-char (scan-sexps (point) -1)))
  (skip-chars-backward csv-skip-regexp))

(defun csv-forward-field (arg)
  "Move forward across one field, cf. `forward-sexp'.
With ARG, do it that many times.  Negative arg -N means
move backward across N fields."
  (interactive "p")
  (if (< arg 0)
      (csv-backward-field (- arg))
    (while (>= (setq arg (1- arg)) 0)
      (if (or (bolp)
	      (when (and (not (eobp)) (eolp)) (forward-char) t))
	  (while (and (not (eobp)) (csv-not-looking-at-record))
	    (forward-line 1)))
      (if (memq (following-char) csv-separator-chars) (forward-char))
      (csv-end-of-field))))

(defun csv-backward-field (arg)
  "Move backward across one field, cf. `backward-sexp'.
With ARG, do it that many times.  Negative arg -N means
move forward across N fields."
  (interactive "p")
  (if (< arg 0)
      (csv-forward-field (- arg))
    (while (>= (setq arg (1- arg)) 0)
      (when (or (eolp)
		(when (and (not (bobp)) (bolp)) (backward-char) t))
	(while (progn
		 (beginning-of-line)
		 (csv-not-looking-at-record))
	  (backward-char))
	(end-of-line))
      (if (memq (preceding-char) csv-separator-chars) (backward-char))
      (csv-beginning-of-field))))

(defun csv-sort-skip-fields (n &optional yank)
  "Position point at the beginning of field N on the current line.
Fields are separated by `csv-separators'\; null terminal field allowed.
Assumes point is initially at the beginning of the line.
YANK non-nil allows N to be greater than the number of fields, in
which case extend the record as necessary."
  (if (> n 0)
      ;; Skip across N - 1 fields.
      (let ((i (1- n)))
	(while (> i 0)
	  (csv-end-of-field)
	  (if (eolp)
	      (if yank
		  (if (> i 1) (insert (car csv-separators)))
		(error "Line has too few fields: %s"
		       (buffer-substring
			(save-excursion (beginning-of-line) (point))
			(save-excursion (end-of-line) (point)))))
	    (forward-char))		; skip separator
	  (setq i (1- i))))
    (end-of-line)
    ;; Skip back across -N - 1 fields.
    (let ((i (1- (- n))))
      (while (> i 0)
	(csv-beginning-of-field)
	(if (bolp)
	    (error "Line has too few fields: %s"
		   (buffer-substring
		    (save-excursion (beginning-of-line) (point))
		    (save-excursion (end-of-line) (point)))))
	(backward-char)			; skip separator
	(setq i (1- i)))
      ;; Position at the front of the field
      ;; even if moving backwards.
      (csv-beginning-of-field))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Field index mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Based partly on paren.el

(defcustom csv-field-index-delay 0.125
  "Time in seconds to delay before updating field index display."
  :group 'CSV
  :type '(number :tag "seconds"))

(defvar csv-field-index-idle-timer nil)

(defvar csv-field-index-string nil)
(make-variable-buffer-local 'csv-field-index-string)

(defvar csv-field-index-old nil)
(make-variable-buffer-local 'csv-field-index-old)

(define-minor-mode csv-field-index-mode
  "Toggle CSV-Field-Index mode.
With prefix ARG, turn CSV-Field-Index mode on if and only if ARG is positive.
Returns the new status of CSV-Field-Index mode (non-nil means on).
When CSV-Field-Index mode is enabled, the current field index appears in
the mode line after `csv-field-index-delay' seconds of Emacs idle time."
  :group 'CSV
  :global t
  :init-value t		       ; for documentation, since default is t
  ;; This macro generates a function that first sets the mode
  ;; variable, then runs the following code, runs the mode hooks,
  ;; displays a message if interactive, updates the mode line and
  ;; finally returns the variable value.

  ;; First, always disable the mechanism (to avoid having two timers):
  (when csv-field-index-idle-timer
    (cancel-timer csv-field-index-idle-timer)
    (setq csv-field-index-idle-timer nil))
  ;; Now, if the mode is on and any buffer is in CSV mode then
  ;; re-initialize and enable the mechanism by setting up a new timer:
  (if csv-field-index-mode
      (if (memq t (mapcar (lambda (buffer)
			    (with-current-buffer buffer
			      (when (eq major-mode 'csv-mode)
				(setq csv-field-index-string nil
				      csv-field-index-old nil)
				t)))
			  (buffer-list)))
	  (setq csv-field-index-idle-timer
		(run-with-idle-timer csv-field-index-delay t
				     'csv-field-index)))
    ;; but if the mode is off then remove the display from the mode
    ;; lines of all CSV buffers:
    (mapc (lambda (buffer)
	    (with-current-buffer buffer
	      (when (eq major-mode 'csv-mode)
		(setq csv-field-index-string nil
		      csv-field-index-old nil)
		(force-mode-line-update))))
	    (buffer-list))))

(defun csv-field-index ()
  "Construct `csv-field-index-string' to display in mode line.
Called by `csv-field-index-idle-timer'."
  (if (eq major-mode 'csv-mode)
      (save-excursion
	(let ((lbp (line-beginning-position)) (field 1))
	  (while (re-search-backward csv-separator-regexp lbp 1)
	    ;; Move as far as possible, i.e. to beginning of line.
	    (setq field (1+ field)))
	  (if (csv-not-looking-at-record) (setq field nil))
	  (when (not (eq field csv-field-index-old))
	    (setq csv-field-index-old field
		  csv-field-index-string
		  (and field (propertize (format "F%d" field)
					 'help-echo csv-mode-line-help-echo)))
	    (force-mode-line-update))))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Killing and yanking fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar csv-killed-fields nil
  "A list of the fields or sub-records last killed by `csv-kill-fields'.")

(defun csv-kill-fields (fields beg end)
  "Kill specified fields of each line in the region.
If not set, the region defaults to the CSV records around point.
Fields are separated by `csv-separators' and null fields are allowed anywhere.
Field indices increase from 1 on the left or decrease from -1 on the right.
The fields are stored for use by `csv-yank-fields'.  Fields can be
specified in any order but are saved in increasing index order.
Ignore blank and comment lines.

When called interactively, a prefix argument specifies a single field,
otherwise prompt for a field list, which may include ranges in the form
m-n, where m < n and n defaults to the last field index if omitted.

When called non-interactively, FIELDS is a single field index or a
list of field indices, with ranges specified as (m.n) or (m), and BEG
and END specify the region to process."
  ;; (interactive "*P\nr")
  (interactive (csv-interactive-args 'multiple))
  (barf-if-buffer-read-only)
  ;; Kill the field(s):
  (setq csv-killed-fields nil)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (if (or (cdr fields) (consp (car fields)))
	  (csv-kill-many-columns fields)
	(csv-kill-one-column (car fields)))))
  (setq csv-killed-fields (nreverse csv-killed-fields)))

(defmacro csv-kill-one-field (field killed-fields)
  "Kill field with index FIELD in current line.
Save killed field by `push'ing onto KILLED-FIELDS.
Assumes point is at beginning of line.
Called by `csv-kill-one-column' and `csv-kill-many-columns'."
  `(progn
     ;; Move to start of field to kill:
     (csv-sort-skip-fields ,field)
     ;; Kill to end of field (cf. `kill-region'):
     (push (delete-and-extract-region
	    (point)
	    (progn (csv-end-of-field) (point)))
	   ,killed-fields)
     (if (eolp) (delete-char -1)    ; delete trailing separator at eol
       (delete-char 1))))	    ; or following separator otherwise

(defun csv-kill-one-column (field)
  "Kill field with index FIELD in all lines in (narrowed) buffer.
Save killed fields in `csv-killed-fields'.
Assumes point is at `point-min'.  Called by `csv-kill-fields'.
Ignore blank and comment lines."
  (while (not (eobp))
    (or (csv-not-looking-at-record)
	(csv-kill-one-field field csv-killed-fields))
    (forward-line)))

(defun csv-kill-many-columns (fields)
  "Kill several fields in all lines in (narrowed) buffer.
FIELDS is an unordered list of field indices.
Save killed fields in increasing index order in `csv-killed-fields'.
Assumes point is at `point-min'.  Called by `csv-kill-fields'.
Ignore blank and comment lines."
  (if (eolp) (error "First record is empty"))
  ;; Convert non-positive to positive field numbers:
  (let ((last 1) (f fields))
    (csv-end-of-field)
    (while (not (eolp))
      (forward-char)			; skip separator
      (csv-end-of-field)
      (setq last (1+ last)))	     ; last = # fields in first record
    (while f
      (cond ((consp (car f))
	     ;; Expand a field range: (m.n) -> m m+1 ... n-1 n.
	     ;; If n is nil then it defaults to the number of fields.
	     (let* ((range (car f)) (cdrf (cdr f))
		    (m (car range)) (n (cdr range)))
	       (if (< m 0) (setq m (+ m last 1)))
	       (if n
		   (if (< n 0) (setq n (+ n last 1)))
		 (setq n last))
	       (setq range (list n))
	       (while (> n m) (push (setq n (1- n)) range))
	       (setcar f (car range))
	       (setcdr f (cdr range))
	       (setcdr (setq f (last range)) cdrf)))
	    ((zerop (car f)) (setcar f 1))
	    ((< (car f) 0) (setcar f (+ f last 1))))
      (setq f (cdr f))))
  (goto-char (point-min))
  ;; Kill from right to avoid miscounting:
  (setq fields (sort fields '>))
  (while (not (eobp))
    (or (csv-not-looking-at-record)
	(let ((fields fields) killed-fields field)
	  (while fields
	    (setq field (car fields)
		  fields (cdr fields))
	    (beginning-of-line)
	    (csv-kill-one-field field killed-fields))
	  (push (mapconcat 'identity killed-fields (car csv-separators))
		csv-killed-fields)))
    (forward-line)))

(defun csv-yank-fields (field beg end)
  "Yank fields as the ARGth field of each line in the region.
ARG may be arbitrarily large and records are extended as necessary.
If not set, the region defaults to the CSV records around point\;
if point is not in a CSV record then offer to yank as a new table.
The fields yanked are those last killed by `csv-kill-fields'.
Fields are separated by `csv-separators' and null fields are allowed anywhere.
Field indices increase from 1 on the left or decrease from -1 on the right.
A prefix argument specifies a single field, otherwise prompt for field index.
Ignore blank and comment lines.  When called non-interactively, FIELD
is a single field index\; BEG and END specify the region to process."
  ;; (interactive "*P\nr")
  (interactive (condition-case err
		   (csv-interactive-args 'single)
		 (error (list nil nil err))))
  (barf-if-buffer-read-only)
  (if (null beg)
      (if (y-or-n-p (concat (error-message-string end)
			    ".  Yank as a new table? "))
	  (csv-yank-as-new-table)
	(error (error-message-string end)))
    (if (<= field 0) (setq field (1+ field)))
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	(let ((fields csv-killed-fields))
	  (while (not (eobp))
	    (unless (csv-not-looking-at-record)
	      ;; Yank at start of specified field if possible,
	      ;; otherwise yank at end of record:
	      (if (zerop field)
		  (end-of-line)
		(csv-sort-skip-fields field 'yank))
	      (and (eolp) (insert (car csv-separators)))
	      (when fields
		(insert (car fields))
		(setq fields (cdr fields)))
	      (or (eolp) (insert (car csv-separators))))
	    (forward-line)))))))

(defun csv-yank-as-new-table ()
  "Yank fields as a new table starting at point.
The fields yanked are those last killed by `csv-kill-fields'."
  (interactive "*")
  (let ((fields csv-killed-fields))
    (while fields
      (insert (car fields) ?\n)
      (setq fields (cdr fields)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Aligning fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csv-align-fields (hard beg end)
  "Align all the fields in the region to form columns.
The alignment style is specified by `csv-align-style'.  The number of
spaces specified by `csv-align-fields' appears after each separator.
Use soft alignment done by displaying virtual white space after the
separators unless invoked with an argument, in which case insert real
space characters into the buffer after the separators.
Unalign first (see `csv-unalign-fields').  Ignore blank and comment lines.

In hard-aligned records, separators become invisible whenever
`buffer-invisibility-spec' is non-nil.  In soft-aligned records, make
separators invisible if and only if `buffer-invisibility-spec' is
non-nil when the records are aligned\; this can be changed only by
re-aligning.  \(Unaligning always makes separators visible.)

When called non-interactively, use hard alignment if HARD is non-nil\;
BEG and END specify the region to align."
  (interactive (csv-interactive-args))
  (setq end (set-marker (make-marker) end))
  (csv-unalign-fields hard beg end) ; if hard then barfs if buffer read only
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (set-marker end nil)
      (goto-char (point-min))
      (let (widths)
	;; Construct list of column widths:
	(while (not (eobp))		; for each record...
	  (or (csv-not-looking-at-record)
	      (let ((w widths) x)
		(setq beg (point))	; beginning of current field
		(while (not (eolp))
		  (csv-end-of-field)
		  (setq x (- (point) beg)) ; field width
		  (if w
		      (if (> x (car w)) (setcar w x))
		    (setq w (list x)
			  widths (nconc widths w)))
		  (or (eolp) (forward-char)) ; skip separator
		  (setq w (cdr w)
			beg (point)))))
	  (forward-line))

	;; Align fields:
	(goto-char (point-min))
	(while (not (eobp))		; for each record...
	  (or (csv-not-looking-at-record)
	      (let ((w widths) (padding 0) x)
		(setq beg (point))	; beginning of current field
		(while (and w (not (eolp)))
		  (let ((left-padding 0) (right-padding 0) overlay)
		    (csv-end-of-field)
		    (set-marker end (point)) ; end of current field
		    (setq x (- (point) beg) ; field width
			  x (- (car w) x)) ; required padding

		    ;; beg = beginning of current field
		    ;; end = (point) = end of current field

		    ;; Compute required padding:
		    (cond
		     ((eq csv-align-style 'left)
		      ;; Left align -- pad on the right:
		      (setq left-padding csv-align-padding
			    right-padding x))
		     ((eq csv-align-style 'right)
		      ;; Right align -- pad on the left:
		      (setq left-padding (+ csv-align-padding x)))
		     ((eq csv-align-style 'auto)
		      ;; Auto align -- left align text, right align numbers:
		      (if (string-match "\\`[-+.[:digit:]]+\\'"
					(buffer-substring beg (point)))
			  ;; Right align -- pad on the left:
			  (setq left-padding (+ csv-align-padding x))
			;; Left align -- pad on the right:
			(setq left-padding csv-align-padding
			      right-padding x)))
		     ((eq csv-align-style 'centre)
		      ;; Centre -- pad on both left and right:
		      (let ((y (/ x 2))) ; truncated integer quotient
			(setq left-padding (+ csv-align-padding y)
			      right-padding (- x y)))))

		    (if hard
			;; Hard alignment...
			(progn
			  (when (> left-padding 0) ; pad on the left
			    ;; Insert spaces before field:
			    (if (= beg end) ; null field
				(insert (make-string left-padding ?\ ))
			      (goto-char beg) ; beginning of current field
			      (insert (make-string left-padding ?\ ))
			      (goto-char end))) ; end of current field
			  (unless (eolp)
			    (if (> right-padding 0) ; pad on the right
				;; Insert spaces after field:
				(insert (make-string right-padding ?\ )))
			    ;; Make separator (potentially) invisible;
			    ;; in Emacs 21.3, neighbouring overlays
			    ;; conflict, so use the following only
			    ;; with hard alignment:
			    (overlay-put (make-overlay (point) (1+ (point)))
					 ;; 'face 'secondary-selection) ; test
					 'invisible t)
			    (forward-char))) ; skip separator

		      ;; Soft alignment...

		      (if buffer-invisibility-spec ; csv-hide-separators

			  ;; Hide separators...
			  (progn
			    ;; Merge right-padding from previous field
			    ;; with left-padding from this field:
			    (setq padding (+ padding left-padding))
			    (when (> padding 0)
			      (goto-char beg) ; beginning of current field
			      (if (bolp)
				  ;; Display spaces before first field
				  ;; by overlaying first character:
				  (overlay-put
				   (make-overlay (point) (1+ (point)))
				   'before-string
				   (make-string padding ?\ ))
				;; Display separator as spaces:
				(overlay-put
				   (make-overlay (1- (point)) (point))
				   ;; 'face 'secondary-selection)) ; test
				   ;; 'display (make-string padding ?\ )))
				   ;; Above 'display mangles buffer
				   ;; horribly if any string is empty!
				   'display `(space :width ,padding)))
			      (goto-char end)) ; end of current field
			    (unless (eolp)
			      (setq padding right-padding)
			      (forward-char))) ; skip separator

			;; Do not hide separators...
			(when (> left-padding 0) ; pad on the left
			  ;; Display spaces before field:
			  (setq overlay (make-overlay beg (point)))
			  (overlay-put overlay 'before-string
				       (make-string left-padding ?\ )))
			(unless (eolp)
			  (if (> right-padding 0) ; pad on the right
			      ;; Display spaces after field:
			      (overlay-put
			       (or overlay
				   (make-overlay beg (point)))
			       'after-string (make-string right-padding ?\ )))
			  (forward-char))) ; skip separator

		      ))

		  (setq w (cdr w)
			beg (point)))))
	  (forward-line)))))
  (set-marker end nil))

(defun csv-unalign-fields (hard beg end)
  "Undo soft alignment and optionally remove redundant white space.
Undo soft alignment introduced by `csv-align-fields'.  If invoked with
an argument then also remove all spaces and tabs around separators.
Also make all invisible separators visible again.
Ignore blank and comment lines.  When called non-interactively, remove
spaces and tabs if HARD non-nil\; BEG and END specify region to unalign."
  (interactive (csv-interactive-args))
  ;; Remove any soft alignment:
  (mapc 'delete-overlay	(overlays-in beg end))
  (when hard
    (barf-if-buffer-read-only)
    ;; Remove any white-space padding around separators:
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	(while (not (eobp))
	  (or (csv-not-looking-at-record)
	      (while (not (eolp))
		;; Delete horizontal white space forward:
		;; (delete-horizontal-space)
		;; This relies on left-to-right argument evaluation;
		;; see info node (elisp) Function Forms.
		(delete-region (point)
			       (+ (point) (skip-chars-forward " \t")))
		(csv-end-of-field)
		;; Delete horizontal white space backward:
		;; (delete-horizontal-space t)
		(delete-region (point)
			       (+ (point) (skip-chars-backward " \t")))
		(or (eolp) (forward-char))))
	  (forward-line))))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Transposing rows and columns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csv-transpose (beg end)
  "Rewrite rows (which may have different lengths) as columns.
Null fields are introduced as necessary within records but are
stripped from the ends of records.  Preserve soft alignment.
This function is its own inverse.  Ignore blank and comment lines.
When called non-interactively, BEG and END specify region to process."
  ;; (interactive "*P\nr")
  (interactive (csv-interactive-args 'noarg))
  (barf-if-buffer-read-only)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; Delete rows and collect them as a reversed list of lists of
      ;; fields, skipping comment and blank lines:
      (let ((sep (car csv-separators))
	    (align (overlays-in beg end))
	    rows columns)
	;; Remove soft alignment if necessary:
	(when align
	  (mapc 'delete-overlay	align)
	  (setq align t))
	(while (not (eobp))
	  (if (csv-not-looking-at-record)
	      ;; Skip blank and comment lines:
	      (forward-line)
	    (let ((lep (line-end-position)))
	      (push
	       (csv-split-string
		(buffer-substring-no-properties (point) lep)
		csv-separator-regexp nil t)
	       rows)
	      (delete-region (point) lep)
	      (or (eobp) (delete-char 1)))))
	;; Rows must have monotonic decreasing lengths to be
	;; transposable, so ensure this by padding with null fields.
	;; rows is currently a reversed list of field lists, which
	;; must therefore have monotonic increasing lengths.
	(let ((oldlen (length (car rows))) newlen
	      (r (cdr rows)))
	  (while r
	    (setq newlen (length (car r)))
	    (if (< newlen oldlen)
		(nconc (car r) (make-list (- oldlen newlen) nil))
	      (setq oldlen newlen))
	    (setq r (cdr r))))
	;; Collect columns as a reversed list of lists of fields:
	(while rows
	  (let (column (r rows) row)
	    (while r
	      (setq row (car r))
	      ;; Provided it would not be a trailing null field, push
	      ;; field onto column:
	      (if (or column (string< "" (car row)))
		  (push (car row) column))
	      ;; Pop field off row:
	      (setcar r (cdr row))
	      ;; If row is now empty then remove it:
	      (or (car r) (setq rows (cdr rows)))
	      (setq r (cdr r)))
	    (push column columns)))
	;; Insert columns into buffer as rows:
	(setq columns (nreverse columns))
	(while columns
	  (insert (mapconcat 'identity (car columns) sep) ?\n)
	  (setq columns (cdr columns)))
	;; Re-do soft alignment if necessary:
	(if align (csv-align-fields nil (point-min) (point-max)))))))

;; The following generalised version of `split-string' is taken from
;; the development version of WoMan and should probably replace the
;; standard version in subr.el.  However, CSV mode (currently) needs
;; only the `allowbeg' option.

(defun csv-split-string
  (string &optional separators subexp allowbeg allowend)
  "Splits STRING into substrings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The substrings between the splitting points are made into a list
which is returned.
If SEPARATORS is absent, it defaults to \"[ \\f\\t\\n\\r\\v]+\".
SUBEXP specifies a subexpression of SEPARATORS to be the splitting
point\; it defaults to 0.

If there is a match for SEPARATORS at the beginning of STRING, we do
not include a null substring for that, unless ALLOWBEG is non-nil.
Likewise, if there is a match at the end of STRING, we do not include
a null substring for that, unless ALLOWEND is non-nil.

Modifies the match data; use `save-match-data' if necessary."
  (or subexp (setq subexp 0))
  (let ((rexp (or separators "[ \f\t\n\r\v]+"))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning subexp))
				       (< start (length string)))
				  (1+ start) start))
		(< (match-beginning subexp) (length string)))
      (setq notfirst t)
      (or (and (not allowbeg) (eq (match-beginning subexp) 0))
	  (and (eq (match-beginning subexp) (match-end subexp))
	       (eq (match-beginning subexp) start))
	  (push (substring string start (match-beginning subexp)) list))
      (setq start (match-end subexp)))
    (or (and (not allowend) (eq start (length string)))
	(push (substring string start) list))
    (nreverse list)))

(provide 'csv-mode)

;;; csv-mode.el ends here
