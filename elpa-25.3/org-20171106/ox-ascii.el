;;; ox-ascii.el --- ASCII Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2017 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp

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

;;; Commentary:
;;
;; This library implements an ASCII back-end for Org generic exporter.
;; See Org manual for more information.

;;; Code:

(require 'ox)
(require 'ox-publish)
(require 'cl-lib)

(declare-function aa2u "ext:ascii-art-to-unicode" ())

;;; Define Back-End
;;
;; The following setting won't allow modifying preferred charset
;; through a buffer keyword or an option item, but, since the property
;; will appear in communication channel nonetheless, it allows
;; overriding `org-ascii-charset' variable on the fly by the ext-plist
;; mechanism.
;;
;; We also install a filter for headlines and sections, in order to
;; control blank lines separating them in output string.

(org-export-define-backend 'ascii
  '((bold . org-ascii-bold)
    (center-block . org-ascii-center-block)
    (clock . org-ascii-clock)
    (code . org-ascii-code)
    (drawer . org-ascii-drawer)
    (dynamic-block . org-ascii-dynamic-block)
    (entity . org-ascii-entity)
    (example-block . org-ascii-example-block)
    (export-block . org-ascii-export-block)
    (export-snippet . org-ascii-export-snippet)
    (fixed-width . org-ascii-fixed-width)
    (footnote-reference . org-ascii-footnote-reference)
    (headline . org-ascii-headline)
    (horizontal-rule . org-ascii-horizontal-rule)
    (inline-src-block . org-ascii-inline-src-block)
    (inlinetask . org-ascii-inlinetask)
    (inner-template . org-ascii-inner-template)
    (italic . org-ascii-italic)
    (item . org-ascii-item)
    (keyword . org-ascii-keyword)
    (latex-environment . org-ascii-latex-environment)
    (latex-fragment . org-ascii-latex-fragment)
    (line-break . org-ascii-line-break)
    (link . org-ascii-link)
    (node-property . org-ascii-node-property)
    (paragraph . org-ascii-paragraph)
    (plain-list . org-ascii-plain-list)
    (plain-text . org-ascii-plain-text)
    (planning . org-ascii-planning)
    (property-drawer . org-ascii-property-drawer)
    (quote-block . org-ascii-quote-block)
    (radio-target . org-ascii-radio-target)
    (section . org-ascii-section)
    (special-block . org-ascii-special-block)
    (src-block . org-ascii-src-block)
    (statistics-cookie . org-ascii-statistics-cookie)
    (strike-through . org-ascii-strike-through)
    (subscript . org-ascii-subscript)
    (superscript . org-ascii-superscript)
    (table . org-ascii-table)
    (table-cell . org-ascii-table-cell)
    (table-row . org-ascii-table-row)
    (target . org-ascii-target)
    (template . org-ascii-template)
    (timestamp . org-ascii-timestamp)
    (underline . org-ascii-underline)
    (verbatim . org-ascii-verbatim)
    (verse-block . org-ascii-verse-block))
  :menu-entry
  '(?t "Export to Plain Text"
       ((?A "As ASCII buffer"
	    (lambda (a s v b)
	      (org-ascii-export-as-ascii a s v b '(:ascii-charset ascii))))
	(?a "As ASCII file"
	    (lambda (a s v b)
	      (org-ascii-export-to-ascii a s v b '(:ascii-charset ascii))))
	(?L "As Latin1 buffer"
	    (lambda (a s v b)
	      (org-ascii-export-as-ascii a s v b '(:ascii-charset latin1))))
	(?l "As Latin1 file"
	    (lambda (a s v b)
	      (org-ascii-export-to-ascii a s v b '(:ascii-charset latin1))))
	(?U "As UTF-8 buffer"
	    (lambda (a s v b)
	      (org-ascii-export-as-ascii a s v b '(:ascii-charset utf-8))))
	(?u "As UTF-8 file"
	    (lambda (a s v b)
	      (org-ascii-export-to-ascii a s v b '(:ascii-charset utf-8))))))
  :filters-alist '((:filter-headline . org-ascii-filter-headline-blank-lines)
		   (:filter-parse-tree org-ascii-filter-paragraph-spacing
				       org-ascii-filter-comment-spacing)
		   (:filter-section . org-ascii-filter-headline-blank-lines))
  :options-alist
  '((:subtitle "SUBTITLE" nil nil parse)
    (:ascii-bullets nil nil org-ascii-bullets)
    (:ascii-caption-above nil nil org-ascii-caption-above)
    (:ascii-charset nil nil org-ascii-charset)
    (:ascii-global-margin nil nil org-ascii-global-margin)
    (:ascii-format-drawer-function nil nil org-ascii-format-drawer-function)
    (:ascii-format-inlinetask-function
     nil nil org-ascii-format-inlinetask-function)
    (:ascii-headline-spacing nil nil org-ascii-headline-spacing)
    (:ascii-indented-line-width nil nil org-ascii-indented-line-width)
    (:ascii-inlinetask-width nil nil org-ascii-inlinetask-width)
    (:ascii-inner-margin nil nil org-ascii-inner-margin)
    (:ascii-links-to-notes nil nil org-ascii-links-to-notes)
    (:ascii-list-margin nil nil org-ascii-list-margin)
    (:ascii-paragraph-spacing nil nil org-ascii-paragraph-spacing)
    (:ascii-quote-margin nil nil org-ascii-quote-margin)
    (:ascii-table-keep-all-vertical-lines
     nil nil org-ascii-table-keep-all-vertical-lines)
    (:ascii-table-use-ascii-art nil nil org-ascii-table-use-ascii-art)
    (:ascii-table-widen-columns nil nil org-ascii-table-widen-columns)
    (:ascii-text-width nil nil org-ascii-text-width)
    (:ascii-underline nil nil org-ascii-underline)
    (:ascii-verbatim-format nil nil org-ascii-verbatim-format)))



;;; User Configurable Variables

(defgroup org-export-ascii nil
  "Options for exporting Org mode files to ASCII."
  :tag "Org Export ASCII"
  :group 'org-export)

(defcustom org-ascii-text-width 72
  "Maximum width of exported text.
This number includes margin size, as set in
`org-ascii-global-margin'."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)

(defcustom org-ascii-global-margin 0
  "Width of the left margin, in number of characters."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)

(defcustom org-ascii-inner-margin 2
  "Width of the inner margin, in number of characters.
Inner margin is applied between each headline."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)

(defcustom org-ascii-quote-margin 6
  "Width of margin used for quoting text, in characters.
This margin is applied on both sides of the text."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)

(defcustom org-ascii-list-margin 0
  "Width of margin used for plain lists, in characters.
This margin applies to top level list only, not to its
sub-lists."
  :group 'org-export-ascii
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'integer)

(defcustom org-ascii-inlinetask-width 30
  "Width of inline tasks, in number of characters.
This number ignores any margin."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)

(defcustom org-ascii-headline-spacing '(1 . 2)
  "Number of blank lines inserted around headlines.

This variable can be set to a cons cell.  In that case, its car
represents the number of blank lines present before headline
contents whereas its cdr reflects the number of blank lines after
contents.

A nil value replicates the number of blank lines found in the
original Org buffer at the same place."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Replicate original spacing" nil)
	  (cons :tag "Set a uniform spacing"
		(integer :tag "Number of blank lines before contents")
		(integer :tag "Number of blank lines after contents"))))

(defcustom org-ascii-indented-line-width 'auto
  "Additional indentation width for the first line in a paragraph.
If the value is an integer, indent the first line of each
paragraph by this width, unless it is located at the beginning of
a section, in which case indentation is removed from that line.
If it is the symbol `auto' preserve indentation from original
document."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (integer :tag "Number of white spaces characters")
	  (const :tag "Preserve original width" auto)))

(defcustom org-ascii-paragraph-spacing 'auto
  "Number of white lines between paragraphs.
If the value is an integer, add this number of blank lines
between contiguous paragraphs.  If is it the symbol `auto', keep
the same number of blank lines as in the original document."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (integer :tag "Number of blank lines")
	  (const :tag "Preserve original spacing" auto)))

(defcustom org-ascii-charset 'ascii
  "The charset allowed to represent various elements and objects.
Possible values are:
`ascii'    Only use plain ASCII characters
`latin1'   Include Latin-1 characters
`utf-8'    Use all UTF-8 characters"
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "ASCII" ascii)
	  (const :tag "Latin-1" latin1)
	  (const :tag "UTF-8" utf-8)))

(defcustom org-ascii-underline '((ascii ?= ?~ ?-)
				 (latin1 ?= ?~ ?-)
				 (utf-8 ?═ ?─ ?╌ ?┄ ?┈))
  "Characters for underlining headings in ASCII export.

Alist whose key is a symbol among `ascii', `latin1' and `utf-8'
and whose value is a list of characters.

For each supported charset, this variable associates a sequence
of underline characters.  In a sequence, the characters will be
used in order for headlines level 1, 2, ...  If no character is
available for a given level, the headline won't be underlined."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(list
	  (cons :tag "Underline characters sequence"
		(const :tag "ASCII charset" ascii)
		(repeat character))
	  (cons :tag "Underline characters sequence"
		(const :tag "Latin-1 charset" latin1)
		(repeat character))
	  (cons :tag "Underline characters sequence"
		(const :tag "UTF-8 charset" utf-8)
		(repeat character))))

(defcustom org-ascii-bullets '((ascii ?* ?+ ?-)
			       (latin1 ?§ ?¶)
			       (utf-8 ?◊))
  "Bullet characters for headlines converted to lists in ASCII export.

Alist whose key is a symbol among `ascii', `latin1' and `utf-8'
and whose value is a list of characters.

The first character is used for the first level considered as low
level, and so on.  If there are more levels than characters given
here, the list will be repeated.

Note that this variable doesn't affect plain lists
representation."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(list
	  (cons :tag "Bullet characters for low level headlines"
		(const :tag "ASCII charset" ascii)
		(repeat character))
	  (cons :tag "Bullet characters for low level headlines"
		(const :tag "Latin-1 charset" latin1)
		(repeat character))
	  (cons :tag "Bullet characters for low level headlines"
		(const :tag "UTF-8 charset" utf-8)
		(repeat character))))

(defcustom org-ascii-links-to-notes t
  "Non-nil means convert links to notes before the next headline.
When nil, the link will be exported in place.  If the line
becomes long in this way, it will be wrapped."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-ascii-table-keep-all-vertical-lines nil
  "Non-nil means keep all vertical lines in ASCII tables.
When nil, vertical lines will be removed except for those needed
for column grouping."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-ascii-table-widen-columns t
  "Non-nil means widen narrowed columns for export.
When nil, narrowed columns will look in ASCII export just like in
Org mode, i.e. with \"=>\" as ellipsis."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-ascii-table-use-ascii-art nil
  "Non-nil means \"table.el\" tables are turned into ASCII art.
It only makes sense when export charset is `utf-8'.  It is nil by
default since it requires \"ascii-art-to-unicode.el\" package,
available through, e.g., GNU ELPA."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-ascii-caption-above nil
  "When non-nil, place caption string before the element.
Otherwise, place it right after it."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-ascii-verbatim-format "`%s'"
  "Format string used for verbatim text and inline code."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-ascii-format-drawer-function
  (lambda (_name contents _width) contents)
  "Function called to format a drawer in ASCII.

The function must accept three parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.
  WIDTH     the text width within the drawer.

The function should return either the string to be exported or
nil to ignore the drawer.

The default value simply returns the value of CONTENTS."
  :group 'org-export-ascii
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)

(defcustom org-ascii-format-inlinetask-function
  'org-ascii-format-inlinetask-default
  "Function called to format an inlinetask in ASCII.

The function must accept nine parameters:
  TODO       the todo keyword, as a string
  TODO-TYPE  the todo type, a symbol among `todo', `done' and nil.
  PRIORITY   the inlinetask priority, as a string
  NAME       the inlinetask name, as a string.
  TAGS       the inlinetask tags, as a list of strings.
  CONTENTS   the contents of the inlinetask, as a string.
  WIDTH      the width of the inlinetask, as a number.
  INLINETASK the inlinetask itself.
  INFO       the info channel.

The function should return either the string to be exported or
nil to ignore the inline task."
  :group 'org-export-ascii
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'function)



;;; Internal Functions

;; Internal functions fall into three categories.

;; The first one is about text formatting.  The core functions are
;; `org-ascii--current-text-width' and
;; `org-ascii--current-justification', which determine, respectively,
;; the current text width allowed to a given element and its expected
;; justification.  Once this information is known,
;; `org-ascii--fill-string', `org-ascii--justify-lines',
;; `org-ascii--justify-element' `org-ascii--box-string' and
;; `org-ascii--indent-string' can operate on a given output string.
;; In particular, justification happens at the regular (i.e.,
;; non-greater) element level, which means that when the exporting
;; process reaches a container (e.g., a center block) content are
;; already justified.

;; The second category contains functions handling elements listings,
;; triggered by "#+TOC:" keyword.  As such, `org-ascii--build-toc'
;; returns a complete table of contents, `org-ascii--list-listings'
;; returns a list of referenceable src-block elements, and
;; `org-ascii--list-tables' does the same for table elements.

;; The third category includes general helper functions.
;; `org-ascii--build-title' creates the title for a given headline
;; or inlinetask element.  `org-ascii--build-caption' returns the
;; caption string associated to a table or a src-block.
;; `org-ascii--describe-links' creates notes about links for
;; insertion at the end of a section.  It uses
;; `org-ascii--unique-links' to get the list of links to describe.
;; Eventually, `org-ascii--translate' translates a string according
;; to language and charset specification.


(defun org-ascii--fill-string (s text-width info &optional justify)
  "Fill a string with specified text-width and return it.

S is the string being filled.  TEXT-WIDTH is an integer
specifying maximum length of a line.  INFO is the plist used as
a communication channel.

Optional argument JUSTIFY can specify any type of justification
among `left', `center', `right' or `full'.  A nil value is
equivalent to `left'.  For a justification that doesn't also fill
string, see `org-ascii--justify-lines' and
`org-ascii--justify-block'.

Return nil if S isn't a string."
  (when (stringp s)
    (let ((double-space-p sentence-end-double-space))
      (with-temp-buffer
	(let ((fill-column text-width)
	      (use-hard-newlines t)
	      (sentence-end-double-space double-space-p))
	  (insert (if (plist-get info :preserve-breaks)
		      (replace-regexp-in-string "\n" hard-newline s)
		    s))
	  (fill-region (point-min) (point-max) justify))
	(buffer-string)))))

(defun org-ascii--justify-lines (s text-width how)
  "Justify all lines in string S.
TEXT-WIDTH is an integer specifying maximum length of a line.
HOW determines the type of justification: it can be `left',
`right', `full' or `center'."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (let ((fill-column text-width)
	  ;; Disable `adaptive-fill-mode' so it doesn't prevent
	  ;; filling lines matching `adaptive-fill-regexp'.
	  (adaptive-fill-mode nil))
      (while (< (point) (point-max))
	(justify-current-line how)
	(forward-line)))
    (buffer-string)))

(defun org-ascii--justify-element (contents element info)
  "Justify CONTENTS of ELEMENT.
INFO is a plist used as a communication channel.  Justification
is done according to the type of element.  More accurately,
paragraphs are filled and other elements are justified as blocks,
that is according to the widest non blank line in CONTENTS."
  (if (not (org-string-nw-p contents)) contents
    (let ((text-width (org-ascii--current-text-width element info))
	  (how (org-ascii--current-justification element)))
      (cond
       ((eq (org-element-type element) 'paragraph)
	;; Paragraphs are treated specially as they need to be filled.
	(org-ascii--fill-string contents text-width info how))
       ((eq how 'left) contents)
       (t (with-temp-buffer
	    (insert contents)
	    (goto-char (point-min))
	    (catch 'exit
	      (let ((max-width 0))
		;; Compute maximum width.  Bail out if it is greater
		;; than page width, since no justification is
		;; possible.
		(save-excursion
		  (while (not (eobp))
		    (unless (looking-at-p "[ \t]*$")
		      (end-of-line)
		      (let ((column (current-column)))
			(cond
			 ((>= column text-width) (throw 'exit contents))
			 ((> column max-width) (setq max-width column)))))
		    (forward-line)))
		;; Justify every line according to TEXT-WIDTH and
		;; MAX-WIDTH.
		(let ((offset (/ (- text-width max-width)
				 (if (eq how 'right) 1 2))))
		  (if (zerop offset) (throw 'exit contents)
		    (while (not (eobp))
		      (unless (looking-at-p "[ \t]*$")
			(indent-to-column offset))
		      (forward-line)))))
	      (buffer-string))))))))

(defun org-ascii--indent-string (s width)
  "Indent string S by WIDTH white spaces.
Empty lines are not indented."
  (when (stringp s)
    (replace-regexp-in-string
     "\\(^\\)[ \t]*\\S-" (make-string width ?\s) s nil nil 1)))

(defun org-ascii--box-string (s info)
  "Return string S with a partial box to its left.
INFO is a plist used as a communication channel."
  (let ((utf8p (eq (plist-get info :ascii-charset) 'utf-8)))
    (format (if utf8p "┌────\n%s\n└────" ",----\n%s\n`----")
	    (replace-regexp-in-string
	     "^" (if utf8p "│ " "| ")
	     ;; Remove last newline character.
	     (replace-regexp-in-string "\n[ \t]*\\'" "" s)))))

(defun org-ascii--current-text-width (element info)
  "Return maximum text width for ELEMENT's contents.
INFO is a plist used as a communication channel."
  (pcase (org-element-type element)
    ;; Elements with an absolute width: `headline' and `inlinetask'.
    (`inlinetask (plist-get info :ascii-inlinetask-width))
    (`headline
     (- (plist-get info :ascii-text-width)
	(let ((low-level-rank (org-export-low-level-p element info)))
	  (if low-level-rank (* low-level-rank 2)
	    (plist-get info :ascii-global-margin)))))
    ;; Elements with a relative width: store maximum text width in
    ;; TOTAL-WIDTH.
    (_
     (let* ((genealogy (org-element-lineage element nil t))
	    ;; Total width is determined by the presence, or not, of an
	    ;; inline task among ELEMENT parents.
	    (total-width
	     (if (cl-some (lambda (parent)
			    (eq (org-element-type parent) 'inlinetask))
			  genealogy)
		 (plist-get info :ascii-inlinetask-width)
	       ;; No inlinetask: Remove global margin from text width.
	       (- (plist-get info :ascii-text-width)
		  (plist-get info :ascii-global-margin)
		  (let ((parent (org-export-get-parent-headline element)))
		    ;; Inner margin doesn't apply to text before first
		    ;; headline.
		    (if (not parent) 0
		      (let ((low-level-rank
			     (org-export-low-level-p parent info)))
			;; Inner margin doesn't apply to contents of
			;; low level headlines, since they've got their
			;; own indentation mechanism.
			(if low-level-rank (* low-level-rank 2)
			  (plist-get info :ascii-inner-margin)))))))))
       (- total-width
	  ;; Each `quote-block' and `verse-block' above narrows text
	  ;; width by twice the standard margin size.
	  (+ (* (cl-count-if (lambda (parent)
			       (memq (org-element-type parent)
				     '(quote-block verse-block)))
			     genealogy)
		2
		(plist-get info :ascii-quote-margin))
	     ;; Apply list margin once per "top-level" plain-list
	     ;; containing current line
	     (* (cl-count-if
		 (lambda (e)
		   (and (eq (org-element-type e) 'plain-list)
			(not (eq (org-element-type (org-export-get-parent e))
				 'item))))
		 genealogy)
		(plist-get info :ascii-list-margin))
	     ;; Text width within a plain-list is restricted by
	     ;; indentation of current item.  If that's the case,
	     ;; compute it with the help of `:structure' property from
	     ;; parent item, if any.
	     (let ((item
		    (if (eq (org-element-type element) 'item) element
		      (cl-find-if (lambda (parent)
				    (eq (org-element-type parent) 'item))
				  genealogy))))
	       (if (not item) 0
		 ;; Compute indentation offset of the current item,
		 ;; that is the sum of the difference between its
		 ;; indentation and the indentation of the top item in
		 ;; the list and current item bullet's length.  Also
		 ;; remove checkbox length, and tag length (for
		 ;; description lists) or bullet length.
		 (let ((struct (org-element-property :structure item))
		       (beg-item (org-element-property :begin item)))
		   (+ (- (org-list-get-ind beg-item struct)
			 (org-list-get-ind
			  (org-list-get-top-point struct) struct))
		      (string-width (or (org-ascii--checkbox item info)
					""))
		      (string-width
		       (let ((tag (org-element-property :tag item)))
			 (if tag (org-export-data tag info)
			   (org-element-property :bullet item))))))))))))))

(defun org-ascii--current-justification (element)
  "Return expected justification for ELEMENT's contents.
Return value is a symbol among `left', `center', `right' and
`full'."
  (let (justification)
    (while (and (not justification)
		(setq element (org-element-property :parent element)))
      (pcase (org-element-type element)
	(`center-block (setq justification 'center))
	(`special-block
	 (let ((name (org-element-property :type element)))
	   (cond ((string= name "JUSTIFYRIGHT") (setq justification 'right))
		 ((string= name "JUSTIFYLEFT") (setq justification 'left)))))))
    (or justification 'left)))

(defun org-ascii--build-title
  (element info text-width &optional underline notags toc)
  "Format ELEMENT title and return it.

ELEMENT is either an `headline' or `inlinetask' element.  INFO is
a plist used as a communication channel.  TEXT-WIDTH is an
integer representing the maximum length of a line.

When optional argument UNDERLINE is non-nil, underline title,
without the tags, according to `org-ascii-underline'
specifications.

If optional argument NOTAGS is non-nil, no tags will be added to
the title.

When optional argument TOC is non-nil, use optional title if
possible.  It doesn't apply to `inlinetask' elements."
  (let* ((headlinep (eq (org-element-type element) 'headline))
	 (numbers
	  ;; Numbering is specific to headlines.
	  (and headlinep (org-export-numbered-headline-p element info)
	       ;; All tests passed: build numbering string.
	       (concat
		(mapconcat
		 'number-to-string
		 (org-export-get-headline-number element info) ".")
		" ")))
	 (text
	  (org-trim
	   (org-export-data
	    (if (and toc headlinep) (org-export-get-alt-title element info)
	      (org-element-property :title element))
	    info)))
	 (todo
	  (and (plist-get info :with-todo-keywords)
	       (let ((todo (org-element-property :todo-keyword element)))
		 (and todo (concat (org-export-data todo info) " ")))))
	 (tags (and (not notags)
		    (plist-get info :with-tags)
		    (let ((tag-list (org-export-get-tags element info)))
		      (and tag-list
			   (format ":%s:"
				   (mapconcat 'identity tag-list ":"))))))
	 (priority
	  (and (plist-get info :with-priority)
	       (let ((char (org-element-property :priority element)))
		 (and char (format "(#%c) " char)))))
	 (first-part (concat numbers todo priority text)))
    (concat
     first-part
     ;; Align tags, if any.
     (when tags
       (format
	(format " %%%ds"
		(max (- text-width  (1+ (string-width first-part)))
		     (string-width tags)))
	tags))
     ;; Maybe underline text, if ELEMENT type is `headline' and an
     ;; underline character has been defined.
     (when (and underline headlinep)
       (let ((under-char
	      (nth (1- (org-export-get-relative-level element info))
		   (cdr (assq (plist-get info :ascii-charset)
			      (plist-get info :ascii-underline))))))
	 (and under-char
	      (concat "\n"
		      (make-string (/ (string-width first-part)
				      (char-width under-char))
				   under-char))))))))

(defun org-ascii--has-caption-p (element _info)
  "Non-nil when ELEMENT has a caption affiliated keyword.
INFO is a plist used as a communication channel.  This function
is meant to be used as a predicate for `org-export-get-ordinal'."
  (org-element-property :caption element))

(defun org-ascii--build-caption (element info)
  "Return caption string for ELEMENT, if applicable.

INFO is a plist used as a communication channel.

The caption string contains the sequence number of ELEMENT along
with its real caption.  Return nil when ELEMENT has no affiliated
caption keyword."
  (let ((caption (org-export-get-caption element)))
    (when caption
      ;; Get sequence number of current src-block among every
      ;; src-block with a caption.
      (let ((reference
	     (org-export-get-ordinal
	      element info nil 'org-ascii--has-caption-p))
	    (title-fmt (org-ascii--translate
			(pcase (org-element-type element)
			  (`table "Table %d:")
			  (`src-block "Listing %d:"))
			info)))
	(org-ascii--fill-string
	 (concat (format title-fmt reference)
		 " "
		 (org-export-data caption info))
	 (org-ascii--current-text-width element info) info)))))

(defun org-ascii--build-toc (info &optional n keyword local)
  "Return a table of contents.

INFO is a plist used as a communication channel.

Optional argument N, when non-nil, is an integer specifying the
depth of the table.

Optional argument KEYWORD specifies the TOC keyword, if any, from
which the table of contents generation has been initiated.

When optional argument LOCAL is non-nil, build a table of
contents according to the current headline."
  (concat
   (unless local
     (let ((title (org-ascii--translate "Table of Contents" info)))
       (concat title "\n"
	       (make-string
		(string-width title)
		(if (eq (plist-get info :ascii-charset) 'utf-8) ?─ ?_))
	       "\n\n")))
   (let ((text-width
	  (if keyword (org-ascii--current-text-width keyword info)
	    (- (plist-get info :ascii-text-width)
	       (plist-get info :ascii-global-margin)))))
     (mapconcat
      (lambda (headline)
	(let* ((level (org-export-get-relative-level headline info))
	       (indent (* (1- level) 3)))
	  (concat
	   (unless (zerop indent) (concat (make-string (1- indent) ?.) " "))
	   (org-ascii--build-title
	    headline info (- text-width indent) nil
	    (or (not (plist-get info :with-tags))
		(eq (plist-get info :with-tags) 'not-in-toc))
	    'toc))))
      (org-export-collect-headlines info n (and local keyword)) "\n"))))

(defun org-ascii--list-listings (keyword info)
  "Return a list of listings.

KEYWORD is the keyword that initiated the list of listings
generation.  INFO is a plist used as a communication channel."
  (let ((title (org-ascii--translate "List of Listings" info)))
    (concat
     title "\n"
     (make-string (string-width title)
		  (if (eq (plist-get info :ascii-charset) 'utf-8) ?─ ?_))
     "\n\n"
     (let ((text-width
	    (if keyword (org-ascii--current-text-width keyword info)
	      (- (plist-get info :ascii-text-width)
		 (plist-get info :ascii-global-margin))))
	   ;; Use a counter instead of retrieving ordinal of each
	   ;; src-block.
	   (count 0))
       (mapconcat
	(lambda (src-block)
	  ;; Store initial text so its length can be computed.  This is
	  ;; used to properly align caption right to it in case of
	  ;; filling (like contents of a description list item).
	  (let* ((initial-text
		  (format (org-ascii--translate "Listing %d:" info)
			  (cl-incf count)))
		 (initial-width (string-width initial-text)))
	    (concat
	     initial-text " "
	     (org-trim
	      (org-ascii--indent-string
	       (org-ascii--fill-string
		;; Use short name in priority, if available.
		(let ((caption (or (org-export-get-caption src-block t)
				   (org-export-get-caption src-block))))
		  (org-export-data caption info))
		(- text-width initial-width) info)
	       initial-width)))))
	(org-export-collect-listings info) "\n")))))

(defun org-ascii--list-tables (keyword info)
  "Return a list of tables.

KEYWORD is the keyword that initiated the list of tables
generation.  INFO is a plist used as a communication channel."
  (let ((title (org-ascii--translate "List of Tables" info)))
    (concat
     title "\n"
     (make-string (string-width title)
		  (if (eq (plist-get info :ascii-charset) 'utf-8) ?─ ?_))
     "\n\n"
     (let ((text-width
	    (if keyword (org-ascii--current-text-width keyword info)
	      (- (plist-get info :ascii-text-width)
		 (plist-get info :ascii-global-margin))))
	   ;; Use a counter instead of retrieving ordinal of each
	   ;; src-block.
	   (count 0))
       (mapconcat
	(lambda (table)
	  ;; Store initial text so its length can be computed.  This is
	  ;; used to properly align caption right to it in case of
	  ;; filling (like contents of a description list item).
	  (let* ((initial-text
		  (format (org-ascii--translate "Table %d:" info)
			  (cl-incf count)))
		 (initial-width (string-width initial-text)))
	    (concat
	     initial-text " "
	     (org-trim
	      (org-ascii--indent-string
	       (org-ascii--fill-string
		;; Use short name in priority, if available.
		(let ((caption (or (org-export-get-caption table t)
				   (org-export-get-caption table))))
		  (org-export-data caption info))
		(- text-width initial-width) info)
	       initial-width)))))
	(org-export-collect-tables info) "\n")))))

(defun org-ascii--unique-links (element info)
  "Return a list of unique link references in ELEMENT.
ELEMENT is either a headline element or a section element.  INFO
is a plist used as a communication channel."
  (let* (seen
	 (unique-link-p
	  ;; Return LINK if it wasn't referenced so far, or nil.
	  ;; Update SEEN links along the way.
	  (lambda (link)
	    (let ((footprint
		   ;; Normalize description in footprints.
		   (cons (org-element-property :raw-link link)
			 (let ((contents (org-element-contents link)))
			   (and contents
				(replace-regexp-in-string
				 "[ \r\t\n]+" " "
				 (org-trim
				  (org-element-interpret-data contents))))))))
	      ;; Ignore LINK if it hasn't been translated already.  It
	      ;; can happen if it is located in an affiliated keyword
	      ;; that was ignored.
	      (when (and (org-string-nw-p
			  (gethash link (plist-get info :exported-data)))
			 (not (member footprint seen)))
		(push footprint seen) link)))))
    (org-element-map (if (eq (org-element-type element) 'section)
			 element
		       ;; In a headline, only retrieve links in title
		       ;; and relative section, not in children.
		       (list (org-element-property :title element)
			     (car (org-element-contents element))))
	'link unique-link-p info nil 'headline t)))

(defun org-ascii--describe-datum (datum info)
  "Describe DATUM object or element.
If DATUM is a string, consider it to be a file name, per
`org-export-resolve-id-link'.  INFO is the communication channel,
as a plist."
  (pcase (org-element-type datum)
    (`plain-text (format "See file %s" datum)) ;External file
    (`headline
     (format (org-ascii--translate "See section %s" info)
	     (if (org-export-numbered-headline-p datum info)
		 (mapconcat #'number-to-string
			    (org-export-get-headline-number datum info)
			    ".")
	       (org-export-data (org-element-property :title datum) info))))
    (_
     (let ((number (org-export-get-ordinal
		    datum info nil #'org-ascii--has-caption-p))
	   ;; If destination is a target, make sure we can name the
	   ;; container it refers to.
	   (enumerable
	    (org-element-lineage datum
				 '(headline paragraph src-block table) t)))
       (pcase (org-element-type enumerable)
	 (`headline
	  (format (org-ascii--translate "See section %s" info)
		  (if (org-export-numbered-headline-p enumerable info)
		      (mapconcat #'number-to-string number ".")
		    (org-export-data
		     (org-element-property :title enumerable) info))))
	 ((guard (not number))
	  (org-ascii--translate "Unknown reference" info))
	 (`paragraph
	  (format (org-ascii--translate "See figure %s" info) number))
	 (`src-block
	  (format (org-ascii--translate "See listing %s" info) number))
	 (`table
	  (format (org-ascii--translate "See table %s" info) number))
	 (_ (org-ascii--translate "Unknown reference" info)))))))

(defun org-ascii--describe-links (links width info)
  "Return a string describing a list of links.
LINKS is a list of link type objects, as returned by
`org-ascii--unique-links'.  WIDTH is the text width allowed for
the output string.  INFO is a plist used as a communication
channel."
  (mapconcat
   (lambda (link)
     (let* ((type (org-element-property :type link))
	    (description (org-element-contents link))
	    (anchor (org-export-data
		     (or description (org-element-property :raw-link link))
		     info)))
       (cond
	((member type '("coderef" "radio")) nil)
	((member type '("custom-id" "fuzzy" "id"))
	 ;; Only links with a description need an entry.  Other are
	 ;; already handled in `org-ascii-link'.
	 (when description
	   (let ((dest (if (equal type "fuzzy")
			   (org-export-resolve-fuzzy-link link info)
			 (org-export-resolve-id-link link info))))
	     (concat
	      (org-ascii--fill-string
	       (format "[%s] %s" anchor (org-ascii--describe-datum dest info))
	       width info)
	      "\n\n"))))
	;; Do not add a link that cannot be resolved and doesn't have
	;; any description: destination is already visible in the
	;; paragraph.
	((not (org-element-contents link)) nil)
	;; Do not add a link already handled by custom export
	;; functions.
	((org-export-custom-protocol-maybe link anchor 'ascii) nil)
	(t
	 (concat
	  (org-ascii--fill-string
	   (format "[%s] %s" anchor (org-element-property :raw-link link))
	   width info)
	  "\n\n")))))
   links ""))

(defun org-ascii--checkbox (item info)
  "Return checkbox string for ITEM or nil.
INFO is a plist used as a communication channel."
  (let ((utf8p (eq (plist-get info :ascii-charset) 'utf-8)))
    (pcase (org-element-property :checkbox item)
      (`on (if utf8p "☑ " "[X] "))
      (`off (if utf8p "☐ " "[ ] "))
      (`trans (if utf8p "☒ " "[-] ")))))



;;; Template

(defun org-ascii-template--document-title (info)
  "Return document title, as a string.
INFO is a plist used as a communication channel."
  (let* ((text-width (plist-get info :ascii-text-width))
	 ;; Links in the title will not be resolved later, so we make
	 ;; sure their path is located right after them.
	 (info (org-combine-plists info '(:ascii-links-to-notes nil)))
	 (with-title (plist-get info :with-title))
	 (title (org-export-data
		 (when with-title (plist-get info :title)) info))
	 (subtitle (org-export-data
		    (when with-title (plist-get info :subtitle)) info))
	 (author (and (plist-get info :with-author)
		      (let ((auth (plist-get info :author)))
			(and auth (org-export-data auth info)))))
	 (email (and (plist-get info :with-email)
		     (org-export-data (plist-get info :email) info)))
	 (date (and (plist-get info :with-date)
		    (org-export-data (org-export-get-date info) info))))
    ;; There are two types of title blocks depending on the presence
    ;; of a title to display.
    (if (string= title "")
	;; Title block without a title.  DATE is positioned at the top
	;; right of the document, AUTHOR to the top left and EMAIL
	;; just below.
	(cond
	 ((and (org-string-nw-p date) (org-string-nw-p author))
	  (concat
	   author
	   (make-string (- text-width (string-width date) (string-width author))
			?\s)
	   date
	   (when (org-string-nw-p email) (concat "\n" email))
	   "\n\n\n"))
	 ((and (org-string-nw-p date) (org-string-nw-p email))
	  (concat
	   email
	   (make-string (- text-width (string-width date) (string-width email))
			?\s)
	   date "\n\n\n"))
	 ((org-string-nw-p date)
	  (concat
	   (org-ascii--justify-lines date text-width 'right)
	   "\n\n\n"))
	 ((and (org-string-nw-p author) (org-string-nw-p email))
	  (concat author "\n" email "\n\n\n"))
	 ((org-string-nw-p author) (concat author "\n\n\n"))
	 ((org-string-nw-p email) (concat email "\n\n\n")))
      ;; Title block with a title.  Document's TITLE, along with the
      ;; AUTHOR and its EMAIL are both overlined and an underlined,
      ;; centered.  Date is just below, also centered.
      (let* ((utf8p (eq (plist-get info :ascii-charset) 'utf-8))
	     ;; Format TITLE.  It may be filled if it is too wide,
	     ;; that is wider than the two thirds of the total width.
	     (title-len (min (apply #'max
				    (mapcar #'length
					    (org-split-string
					     (concat title "\n" subtitle) "\n")))
			     (/ (* 2 text-width) 3)))
	     (formatted-title (org-ascii--fill-string title title-len info))
	     (formatted-subtitle (when (org-string-nw-p subtitle)
				   (org-ascii--fill-string subtitle title-len info)))
	     (line
	      (make-string
	       (min (+ (max title-len
			    (string-width (or author ""))
			    (string-width (or email "")))
		       2)
		    text-width) (if utf8p ?━ ?_))))
	(org-ascii--justify-lines
	 (concat line "\n"
		 (unless utf8p "\n")
		 (upcase formatted-title)
		 (and formatted-subtitle (concat "\n" formatted-subtitle))
		 (cond
		  ((and (org-string-nw-p author) (org-string-nw-p email))
		   (concat "\n\n" author "\n" email))
		  ((org-string-nw-p author) (concat "\n\n" author))
		  ((org-string-nw-p email) (concat "\n\n" email)))
		 "\n" line
		 (when (org-string-nw-p date) (concat "\n\n\n" date))
		 "\n\n\n") text-width 'center)))))

(defun org-ascii-inner-template (contents info)
  "Return complete document string after ASCII conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (org-element-normalize-string
   (let ((global-margin (plist-get info :ascii-global-margin)))
     (org-ascii--indent-string
      (concat
       ;; 1. Document's body.
       contents
       ;; 2. Footnote definitions.
       (let ((definitions (org-export-collect-footnote-definitions info))
	     ;; Insert full links right inside the footnote definition
	     ;; as they have no chance to be inserted later.
	     (info (org-combine-plists info '(:ascii-links-to-notes nil))))
	 (when definitions
	   (concat
	    "\n\n\n"
	    (let ((title (org-ascii--translate "Footnotes" info)))
	      (concat
	       title "\n"
	       (make-string
		(string-width title)
		(if (eq (plist-get info :ascii-charset) 'utf-8) ?─ ?_))))
	    "\n\n"
	    (let ((text-width (- (plist-get info :ascii-text-width)
				 global-margin)))
	      (mapconcat
	       (lambda (ref)
		 (let ((id (format "[%s] " (car ref))))
		   ;; Distinguish between inline definitions and
		   ;; full-fledged definitions.
		   (org-trim
		    (let ((def (nth 2 ref)))
		      (if (org-element-map def org-element-all-elements
			    #'identity info 'first-match)
			  ;; Full-fledged definition: footnote ID is
			  ;; inserted inside the first parsed
			  ;; paragraph (FIRST), if any, to be sure
			  ;; filling will take it into consideration.
			  (let ((first (car (org-element-contents def))))
			    (if (not (eq (org-element-type first) 'paragraph))
				(concat id "\n" (org-export-data def info))
			      (push id (nthcdr 2 first))
			      (org-export-data def info)))
			;; Fill paragraph once footnote ID is inserted
			;; in order to have a correct length for first
			;; line.
			(org-ascii--fill-string
			 (concat id (org-export-data def info))
			 text-width info))))))
	       definitions "\n\n"))))))
      global-margin))))

(defun org-ascii-template (contents info)
  "Return complete document string after ASCII conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((global-margin (plist-get info :ascii-global-margin)))
    (concat
     ;; Build title block.
     (org-ascii--indent-string
      (concat (org-ascii-template--document-title info)
	      ;; 2. Table of contents.
	      (let ((depth (plist-get info :with-toc)))
		(when depth
		  (concat
		   (org-ascii--build-toc info (and (wholenump depth) depth))
		   "\n\n\n"))))
      global-margin)
     ;; Document's body.
     contents
     ;; Creator.  Justify it to the bottom right.
     (and (plist-get info :with-creator)
	  (org-ascii--indent-string
	   (let ((text-width
		  (- (plist-get info :ascii-text-width) global-margin)))
	     (concat
	      "\n\n\n"
	      (org-ascii--fill-string
	       (plist-get info :creator) text-width info 'right)))
	   global-margin)))))

(defun org-ascii--translate (s info)
  "Translate string S according to specified language and charset.
INFO is a plist used as a communication channel."
  (let ((charset (intern (format ":%s" (plist-get info :ascii-charset)))))
    (org-export-translate s charset info)))



;;; Transcode Functions

;;;; Bold

(defun org-ascii-bold (_bold contents _info)
  "Transcode BOLD from Org to ASCII.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "*%s*" contents))


;;;; Center Block

(defun org-ascii-center-block (_center-block contents _info)
  "Transcode a CENTER-BLOCK element from Org to ASCII.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  ;; Center has already been taken care of at a lower level, so
  ;; there's nothing left to do.
  contents)


;;;; Clock

(defun org-ascii-clock (clock _contents info)
  "Transcode a CLOCK object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-ascii--justify-element
   (concat org-clock-string " "
	   (org-timestamp-translate (org-element-property :value clock))
	   (let ((time (org-element-property :duration clock)))
	     (and time
		  (concat " => "
			  (apply 'format
				 "%2s:%02s"
				 (org-split-string time ":"))))))
   clock info))


;;;; Code

(defun org-ascii-code (code _contents info)
  "Return a CODE object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format (plist-get info :ascii-verbatim-format)
	  (org-element-property :value code)))


;;;; Drawer

(defun org-ascii-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to ASCII.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((name (org-element-property :drawer-name drawer))
	(width (org-ascii--current-text-width drawer info)))
    (funcall (plist-get info :ascii-format-drawer-function)
	     name contents width)))


;;;; Dynamic Block

(defun org-ascii-dynamic-block (_dynamic-block contents _info)
  "Transcode a DYNAMIC-BLOCK element from Org to ASCII.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  contents)


;;;; Entity

(defun org-ascii-entity (entity _contents info)
  "Transcode an ENTITY object from Org to ASCII.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-property
   (intern (concat ":" (symbol-name (plist-get info :ascii-charset))))
   entity))


;;;; Example Block

(defun org-ascii-example-block (example-block _contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-ascii--justify-element
   (org-ascii--box-string
    (org-export-format-code-default example-block info) info)
   example-block info))


;;;; Export Snippet

(defun org-ascii-export-snippet (export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'ascii)
    (org-element-property :value export-snippet)))


;;;; Export Block

(defun org-ascii-export-block (export-block _contents info)
  "Transcode a EXPORT-BLOCK element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "ASCII")
    (org-ascii--justify-element
     (org-element-property :value export-block) export-block info)))


;;;; Fixed Width

(defun org-ascii-fixed-width (fixed-width _contents info)
  "Transcode a FIXED-WIDTH element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-ascii--justify-element
   (org-ascii--box-string
    (org-remove-indentation
     (org-element-property :value fixed-width)) info)
   fixed-width info))


;;;; Footnote Definition

;; Footnote Definitions are ignored.  They are compiled at the end of
;; the document, by `org-ascii-inner-template'.


;;;; Footnote Reference

(defun org-ascii-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "[%s]" (org-export-get-footnote-number footnote-reference info)))


;;;; Headline

(defun org-ascii-headline (headline contents info)
  "Transcode a HEADLINE element from Org to ASCII.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  ;; Don't export footnote section, which will be handled at the end
  ;; of the template.
  (unless (org-element-property :footnote-section-p headline)
    (let* ((low-level (org-export-low-level-p headline info))
	   (width (org-ascii--current-text-width headline info))
	   ;; Export title early so that any link in it can be
	   ;; exported and seen in `org-ascii--unique-links'.
	   (title (org-ascii--build-title headline info width (not low-level)))
	   ;; Blank lines between headline and its contents.
	   ;; `org-ascii-headline-spacing', when set, overwrites
	   ;; original buffer's spacing.
	   (pre-blanks
	    (make-string (or (car (plist-get info :ascii-headline-spacing))
			     (org-element-property :pre-blank headline)
			     0)
			 ?\n))
	   (links (and (plist-get info :ascii-links-to-notes)
		       (org-ascii--describe-links
			(org-ascii--unique-links headline info) width info)))
	   ;; Re-build contents, inserting section links at the right
	   ;; place.  The cost is low since build results are cached.
	   (body
	    (if (not (org-string-nw-p links)) contents
	      (let* ((contents (org-element-contents headline))
		     (section (let ((first (car contents)))
				(and (eq (org-element-type first) 'section)
				     first))))
		(concat (and section
			     (concat (org-element-normalize-string
				      (org-export-data section info))
				     "\n\n"))
			links
			(mapconcat (lambda (e) (org-export-data e info))
				   (if section (cdr contents) contents)
				   ""))))))
      ;; Deep subtree: export it as a list item.
      (if low-level
	  (let* ((bullets (cdr (assq (plist-get info :ascii-charset)
				     (plist-get info :ascii-bullets))))
		 (bullet
		  (format "%c "
			  (nth (mod (1- low-level) (length bullets)) bullets))))
	    (concat bullet title "\n" pre-blanks
		    ;; Contents, indented by length of bullet.
		    (org-ascii--indent-string body (length bullet))))
	;; Else: Standard headline.
	(concat title "\n" pre-blanks body)))))


;;;; Horizontal Rule

(defun org-ascii-horizontal-rule (horizontal-rule _contents info)
  "Transcode an HORIZONTAL-RULE object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((text-width (org-ascii--current-text-width horizontal-rule info))
	(spec-width
	 (org-export-read-attribute :attr_ascii horizontal-rule :width)))
    (org-ascii--justify-lines
     (make-string (if (and spec-width (string-match "^[0-9]+$" spec-width))
		      (string-to-number spec-width)
		    text-width)
		  (if (eq (plist-get info :ascii-charset) 'utf-8) ?― ?-))
     text-width 'center)))


;;;; Inline Src Block

(defun org-ascii-inline-src-block (inline-src-block _contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to ASCII.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format (plist-get info :ascii-verbatim-format)
	  (org-element-property :value inline-src-block)))


;;;; Inlinetask

(defun org-ascii-format-inlinetask-default
  (_todo _type _priority _name _tags contents width inlinetask info)
  "Format an inline task element for ASCII export.
See `org-ascii-format-inlinetask-function' for a description
of the parameters."
  (let* ((utf8p (eq (plist-get info :ascii-charset) 'utf-8))
	 (width (or width (plist-get info :ascii-inlinetask-width))))
    (org-ascii--indent-string
     (concat
      ;; Top line, with an additional blank line if not in UTF-8.
      (make-string width (if utf8p ?━ ?_)) "\n"
      (unless utf8p (concat (make-string width ? ) "\n"))
      ;; Add title.  Fill it if wider than inlinetask.
      (let ((title (org-ascii--build-title inlinetask info width)))
	(if (<= (string-width title) width) title
	  (org-ascii--fill-string title width info)))
      "\n"
      ;; If CONTENTS is not empty, insert it along with
      ;; a separator.
      (when (org-string-nw-p contents)
        (concat (make-string width (if utf8p ?─ ?-)) "\n" contents))
      ;; Bottom line.
      (make-string width (if utf8p ?━ ?_)))
     ;; Flush the inlinetask to the right.
     (- (plist-get info :ascii-text-width) (plist-get info :ascii-global-margin)
	(if (not (org-export-get-parent-headline inlinetask)) 0
	  (plist-get info :ascii-inner-margin))
	(org-ascii--current-text-width inlinetask info)))))

(defun org-ascii-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to ASCII.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((width (org-ascii--current-text-width inlinetask info)))
    (funcall (plist-get info :ascii-format-inlinetask-function)
	     ;; todo.
	     (and (plist-get info :with-todo-keywords)
		  (let ((todo (org-element-property
			       :todo-keyword inlinetask)))
		    (and todo (org-export-data todo info))))
	     ;; todo-type
	     (org-element-property :todo-type inlinetask)
	     ;; priority
	     (and (plist-get info :with-priority)
		  (org-element-property :priority inlinetask))
	     ;; title
	     (org-export-data (org-element-property :title inlinetask) info)
	     ;; tags
	     (and (plist-get info :with-tags)
		  (org-element-property :tags inlinetask))
	     ;; contents and width
	     contents width inlinetask info)))


;;;; Italic

(defun org-ascii-italic (_italic contents _info)
  "Transcode italic from Org to ASCII.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "/%s/" contents))


;;;; Item

(defun org-ascii-item (item contents info)
  "Transcode an ITEM element from Org to ASCII.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((utf8p (eq (plist-get info :ascii-charset) 'utf-8))
	 (checkbox (org-ascii--checkbox item info))
	 (list-type (org-element-property :type (org-export-get-parent item)))
	 (bullet
	  ;; First parent of ITEM is always the plain-list.  Get
	  ;; `:type' property from it.
	  (org-list-bullet-string
	   (pcase list-type
	     (`descriptive
	      (concat checkbox
		      (org-export-data (org-element-property :tag item) info)
		      ": "))
	     (`ordered
	      ;; Return correct number for ITEM, paying attention to
	      ;; counters.
	      (let* ((struct (org-element-property :structure item))
		     (bul (org-element-property :bullet item))
		     (num (number-to-string
			   (car (last (org-list-get-item-number
				       (org-element-property :begin item)
				       struct
				       (org-list-prevs-alist struct)
				       (org-list-parents-alist struct)))))))
		(replace-regexp-in-string "[0-9]+" num bul)))
	     (_ (let ((bul (org-element-property :bullet item)))
		  ;; Change bullets into more visible form if UTF-8 is active.
		  (if (not utf8p) bul
		    (replace-regexp-in-string
		     "-" "•"
		     (replace-regexp-in-string
		      "+" "⁃"
		      (replace-regexp-in-string "*" "‣" bul))))))))))
    (concat
     bullet
     (unless (eq list-type 'descriptive) checkbox)
     ;; Contents: Pay attention to indentation.  Note: check-boxes are
     ;; already taken care of at the paragraph level so they don't
     ;; interfere with indentation.
     (let ((contents (org-ascii--indent-string contents (string-width bullet))))
       (if (eq (org-element-type (car (org-element-contents item))) 'paragraph)
	   (org-trim contents)
	 (concat "\n" contents))))))


;;;; Keyword

(defun org-ascii-keyword (keyword _contents info)
  "Transcode a KEYWORD element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "ASCII") (org-ascii--justify-element value keyword info))
     ((string= key "TOC")
      (org-ascii--justify-element
       (let ((case-fold-search t))
	 (cond
	  ((string-match-p "\\<headlines\\>" value)
	   (let ((depth (and (string-match "\\<[0-9]+\\>" value)
			     (string-to-number (match-string 0 value))))
		 (localp (string-match-p "\\<local\\>" value)))
	     (org-ascii--build-toc info depth keyword localp)))
	  ((string-match-p "\\<tables\\>" value)
	   (org-ascii--list-tables keyword info))
	  ((string-match-p "\\<listings\\>" value)
	   (org-ascii--list-listings keyword info))))
       keyword info)))))


;;;; Latex Environment

(defun org-ascii-latex-environment (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (when (plist-get info :with-latex)
    (org-ascii--justify-element
     (org-remove-indentation (org-element-property :value latex-environment))
     latex-environment info)))


;;;; Latex Fragment

(defun org-ascii-latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (when (plist-get info :with-latex)
    (org-element-property :value latex-fragment)))


;;;; Line Break

(defun org-ascii-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
  information."  hard-newline)


;;;; Link

(defun org-ascii-link (link desc info)
  "Transcode a LINK object from Org to ASCII.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information."
  (let ((type (org-element-property :type link)))
    (cond
     ((org-export-custom-protocol-maybe link desc 'ascii))
     ((string= type "coderef")
      (let ((ref (org-element-property :path link)))
	(format (org-export-get-coderef-format ref desc)
		(org-export-resolve-coderef ref info))))
     ;; Do not apply a special syntax on radio links.  Though, use
     ;; transcoded target's contents as output.
     ((string= type "radio") desc)
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(pcase (org-element-type destination)
	  ((guard desc)
	   (if (plist-get info :ascii-links-to-notes)
	       (format "[%s]" desc)
	     (concat desc
		     (format " (%s)"
			     (org-ascii--describe-datum destination info)))))
	  ;; External file.
	  (`plain-text destination)
	  (`headline
	   (if (org-export-numbered-headline-p destination info)
	       (mapconcat #'number-to-string
			  (org-export-get-headline-number destination info)
			  ".")
	     (org-export-data (org-element-property :title destination) info)))
	  ;; Handle enumerable elements and targets within them.
	  ((and (let number (org-export-get-ordinal
			     destination info nil #'org-ascii--has-caption-p))
		(guard number))
	   (if (atom number) (number-to-string number)
	     (mapconcat #'number-to-string number ".")))
	  ;; Don't know what to do.  Signal it.
	  (_ "???"))))
     (t
      (let ((raw-link (org-element-property :raw-link link)))
	(if (not (org-string-nw-p desc)) (format "[%s]" raw-link)
	  (concat (format "[%s]" desc)
		  (and (not (plist-get info :ascii-links-to-notes))
		       (format " (%s)" raw-link)))))))))


;;;; Node Properties

(defun org-ascii-node-property (node-property _contents _info)
  "Transcode a NODE-PROPERTY element from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "%s:%s"
          (org-element-property :key node-property)
          (let ((value (org-element-property :value node-property)))
            (if value (concat " " value) ""))))


;;;; Paragraph

(defun org-ascii-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to ASCII.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (org-ascii--justify-element
   (let ((indented-line-width (plist-get info :ascii-indented-line-width)))
     (if (not (wholenump indented-line-width)) contents
       (concat
	;; Do not indent first paragraph in a section.
	(unless (and (not (org-export-get-previous-element paragraph info))
		     (eq (org-element-type (org-export-get-parent paragraph))
			 'section))
	  (make-string indented-line-width ?\s))
	(replace-regexp-in-string "\\`[ \t]+" "" contents))))
   paragraph info))


;;;; Plain List

(defun org-ascii-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to ASCII.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let ((margin (plist-get info :ascii-list-margin)))
    (if (or (< margin 1)
	    (eq (org-element-type (org-export-get-parent plain-list)) 'item))
	contents
      (org-ascii--indent-string contents margin))))


;;;; Plain Text

(defun org-ascii-plain-text (text info)
  "Transcode a TEXT string from Org to ASCII.
INFO is a plist used as a communication channel."
  (let ((utf8p (eq (plist-get info :ascii-charset) 'utf-8)))
    (when (and utf8p (plist-get info :with-smart-quotes))
      (setq text (org-export-activate-smart-quotes text :utf-8 info)))
    (if (not (plist-get info :with-special-strings)) text
      (setq text (replace-regexp-in-string "\\\\-" "" text))
      (if (not utf8p) text
	;; Usual replacements in utf-8 with proper option set.
	(replace-regexp-in-string
	 "\\.\\.\\." "…"
	 (replace-regexp-in-string
	  "--" "–"
	  (replace-regexp-in-string "---" "—" text)))))))


;;;; Planning

(defun org-ascii-planning (planning _contents info)
  "Transcode a PLANNING element from Org to ASCII.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-ascii--justify-element
   (mapconcat
    #'identity
    (delq nil
	  (list (let ((closed (org-element-property :closed planning)))
		  (when closed
		    (concat org-closed-string " "
			    (org-timestamp-translate closed))))
		(let ((deadline (org-element-property :deadline planning)))
		  (when deadline
		    (concat org-deadline-string " "
			    (org-timestamp-translate deadline))))
		(let ((scheduled (org-element-property :scheduled planning)))
		  (when scheduled
		    (concat org-scheduled-string " "
			    (org-timestamp-translate scheduled))))))
    " ")
   planning info))


;;;; Property Drawer

(defun org-ascii-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to ASCII.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
       (org-ascii--justify-element contents property-drawer info)))


;;;; Quote Block

(defun org-ascii-quote-block (_quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to ASCII.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-ascii--indent-string contents (plist-get info :ascii-quote-margin)))


;;;; Radio Target

(defun org-ascii-radio-target (_radio-target contents _info)
  "Transcode a RADIO-TARGET object from Org to ASCII.
CONTENTS is the contents of the target.  INFO is a plist holding
contextual information."
  contents)


;;;; Section

(defun org-ascii-section (section contents info)
  "Transcode a SECTION element from Org to ASCII.
CONTENTS is the contents of the section.  INFO is a plist holding
contextual information."
  (let ((links
	 (and (plist-get info :ascii-links-to-notes)
	      ;; Take care of links in first section of the document.
	      (not (org-element-lineage section '(headline)))
	      (org-ascii--describe-links
	       (org-ascii--unique-links section info)
	       (org-ascii--current-text-width section info)
	       info))))
    (org-ascii--indent-string
     (if (not (org-string-nw-p links)) contents
       (concat (org-element-normalize-string contents) "\n\n" links))
     ;; Do not apply inner margin if parent headline is low level.
     (let ((headline (org-export-get-parent-headline section)))
       (if (or (not headline) (org-export-low-level-p headline info)) 0
	 (plist-get info :ascii-inner-margin))))))


;;;; Special Block

(defun org-ascii-special-block (_special-block contents _info)
  "Transcode a SPECIAL-BLOCK element from Org to ASCII.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  ;; "JUSTIFYLEFT" and "JUSTIFYRIGHT" have already been taken care of
  ;; at a lower level.  There is no other special block type to
  ;; handle.
  contents)


;;;; Src Block

(defun org-ascii-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to ASCII.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((caption (org-ascii--build-caption src-block info))
	(caption-above-p (plist-get info :ascii-caption-above))
	(code (org-export-format-code-default src-block info)))
    (if (equal code "") ""
      (org-ascii--justify-element
       (concat
	(and caption caption-above-p (concat caption "\n"))
	(org-ascii--box-string code info)
	(and caption (not caption-above-p) (concat "\n" caption)))
       src-block info))))


;;;; Statistics Cookie

(defun org-ascii-statistics-cookie (statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))


;;;; Subscript

(defun org-ascii-subscript (subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to ASCII.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (if (org-element-property :use-brackets-p subscript)
      (format "_{%s}" contents)
    (format "_%s" contents)))


;;;; Superscript

(defun org-ascii-superscript (superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to ASCII.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (if (org-element-property :use-brackets-p superscript)
      (format "^{%s}" contents)
    (format "^%s" contents)))


;;;; Strike-through

(defun org-ascii-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH from Org to ASCII.
CONTENTS is text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "+%s+" contents))


;;;; Table

(defun org-ascii-table (table contents info)
  "Transcode a TABLE element from Org to ASCII.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (let ((caption (org-ascii--build-caption table info))
	(caption-above-p (plist-get info :ascii-caption-above)))
    (org-ascii--justify-element
     (concat
      ;; Possibly add a caption string above.
      (and caption caption-above-p (concat caption "\n"))
      ;; Insert table.  Note: "table.el" tables are left unmodified.
      (cond ((eq (org-element-property :type table) 'org) contents)
	    ((and (plist-get info :ascii-table-use-ascii-art)
		  (eq (plist-get info :ascii-charset) 'utf-8)
		  (require 'ascii-art-to-unicode nil t))
	     (with-temp-buffer
	       (insert (org-remove-indentation
			(org-element-property :value table)))
	       (goto-char (point-min))
	       (aa2u)
	       (goto-char (point-max))
	       (skip-chars-backward " \r\t\n")
	       (buffer-substring (point-min) (point))))
	    (t (org-remove-indentation (org-element-property :value table))))
      ;; Possible add a caption string below.
      (and (not caption-above-p) caption))
     table info)))


;;;; Table Cell

(defun org-ascii--table-cell-width (table-cell info)
  "Return width of TABLE-CELL.

INFO is a plist used as a communication channel.

Width of a cell is determined either by a width cookie in the
same column as the cell, or by the maximum cell's length in that
column.

When `org-ascii-table-widen-columns' is non-nil, width cookies
are ignored."
  (let* ((row (org-export-get-parent table-cell))
	 (table (org-export-get-parent row))
	 (col (let ((cells (org-element-contents row)))
		(- (length cells) (length (memq table-cell cells)))))
	 (cache
	  (or (plist-get info :ascii-table-cell-width-cache)
	      (plist-get (setq info
			       (plist-put info :ascii-table-cell-width-cache
					  (make-hash-table :test 'equal)))
			 :ascii-table-cell-width-cache)))
	 (key (cons table col))
	 (widenp (plist-get info :ascii-table-widen-columns)))
    (or (gethash key cache)
	(puthash
	 key
	 (let ((cookie-width (org-export-table-cell-width table-cell info)))
	   (or (and (not widenp) cookie-width)
	       (let ((contents-width
		      (let ((max-width 0))
			(org-element-map table 'table-row
			  (lambda (row)
			    (setq max-width
				  (max (string-width
					(org-export-data
					 (org-element-contents
					  (elt (org-element-contents row) col))
					 info))
				       max-width)))
			  info)
			max-width)))
		 (cond ((not cookie-width) contents-width)
		       (widenp (max cookie-width contents-width))
		       (t cookie-width)))))
	 cache))))

(defun org-ascii-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL object from Org to ASCII.
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  ;; Determine column width.  When `org-ascii-table-widen-columns'
  ;; is nil and some width cookie has set it, use that value.
  ;; Otherwise, compute the maximum width among transcoded data of
  ;; each cell in the column.
  (let ((width (org-ascii--table-cell-width table-cell info)))
    ;; When contents are too large, truncate them.
    (unless (or (plist-get info :ascii-table-widen-columns)
		(<= (string-width (or contents "")) width))
      (setq contents (concat (substring contents 0 (- width 2)) "=>")))
    ;; Align contents correctly within the cell.
    (let* ((indent-tabs-mode nil)
	   (data
	    (when contents
	      (org-ascii--justify-lines
	       contents width
	       (org-export-table-cell-alignment table-cell info)))))
      (setq contents
	    (concat data
		    (make-string (- width (string-width (or data ""))) ?\s))))
    ;; Return cell.
    (concat (format " %s " contents)
	    (when (memq 'right (org-export-table-cell-borders table-cell info))
	      (if (eq (plist-get info :ascii-charset) 'utf-8) "│" "|")))))


;;;; Table Row

(defun org-ascii-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to ASCII.
CONTENTS is the row contents.  INFO is a plist used as
a communication channel."
  (when (eq (org-element-property :type table-row) 'standard)
    (let ((build-hline
	   (function
	    (lambda (lcorner horiz vert rcorner)
	      (concat
	       (apply
		'concat
		(org-element-map table-row 'table-cell
		  (lambda (cell)
		    (let ((width (org-ascii--table-cell-width cell info))
			  (borders (org-export-table-cell-borders cell info)))
		      (concat
		       ;; In order to know if CELL starts the row, do
		       ;; not compare it with the first cell in the
		       ;; row as there might be a special column.
		       ;; Instead, compare it with first exportable
		       ;; cell, obtained with `org-element-map'.
		       (when (and (memq 'left borders)
				  (eq (org-element-map table-row 'table-cell
					'identity info t)
				      cell))
			 lcorner)
		       (make-string (+ 2 width) (string-to-char horiz))
		       (cond
			((not (memq 'right borders)) nil)
			((eq (car (last (org-element-contents table-row))) cell)
			 rcorner)
			(t vert)))))
		  info)) "\n"))))
	  (utf8p (eq (plist-get info :ascii-charset) 'utf-8))
	  (borders (org-export-table-cell-borders
		    (org-element-map table-row 'table-cell 'identity info t)
		    info)))
      (concat (cond
	       ((and (memq 'top borders) (or utf8p (memq 'above borders)))
		(if utf8p (funcall build-hline "┍" "━" "┯" "┑")
		  (funcall build-hline "+" "-" "+" "+")))
	       ((memq 'above borders)
		(if utf8p (funcall build-hline "├" "─" "┼" "┤")
		  (funcall build-hline "+" "-" "+" "+"))))
	      (when (memq 'left borders) (if utf8p "│" "|"))
	      contents "\n"
	      (when (and (memq 'bottom borders) (or utf8p (memq 'below borders)))
		(if utf8p (funcall build-hline "┕" "━" "┷" "┙")
		  (funcall build-hline "+" "-" "+" "+")))))))


;;;; Timestamp

(defun org-ascii-timestamp (timestamp _contents info)
  "Transcode a TIMESTAMP object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-ascii-plain-text (org-timestamp-translate timestamp) info))


;;;; Underline

(defun org-ascii-underline (_underline contents _info)
  "Transcode UNDERLINE from Org to ASCII.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "_%s_" contents))


;;;; Verbatim

(defun org-ascii-verbatim (verbatim _contents info)
  "Return a VERBATIM object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format (plist-get info :ascii-verbatim-format)
	  (org-element-property :value verbatim)))


;;;; Verse Block

(defun org-ascii-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to ASCII.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (org-ascii--indent-string
   (org-ascii--justify-element contents verse-block info)
   (plist-get info :ascii-quote-margin)))



;;; Filters

(defun org-ascii-filter-headline-blank-lines (headline _backend info)
  "Filter controlling number of blank lines after a headline.

HEADLINE is a string representing a transcoded headline.  BACKEND
is symbol specifying back-end used for export.  INFO is plist
containing the communication channel.

This function only applies to `ascii' back-end.  See
`org-ascii-headline-spacing' for information."
  (let ((headline-spacing (plist-get info :ascii-headline-spacing)))
    (if (not headline-spacing) headline
      (let ((blanks (make-string (1+ (cdr headline-spacing)) ?\n)))
	(replace-regexp-in-string "\n\\(?:\n[ \t]*\\)*\\'" blanks headline)))))

(defun org-ascii-filter-paragraph-spacing (tree _backend info)
  "Filter controlling number of blank lines between paragraphs.

TREE is the parse tree.  BACKEND is the symbol specifying
back-end used for export.  INFO is a plist used as
a communication channel.

See `org-ascii-paragraph-spacing' for information."
  (let ((paragraph-spacing (plist-get info :ascii-paragraph-spacing)))
    (when (wholenump paragraph-spacing)
      (org-element-map tree 'paragraph
	(lambda (p)
	  (when (eq (org-element-type (org-export-get-next-element p info))
		    'paragraph)
	    (org-element-put-property p :post-blank paragraph-spacing))))))
  tree)

(defun org-ascii-filter-comment-spacing (tree _backend info)
  "Filter removing blank lines between comments.
TREE is the parse tree.  BACKEND is the symbol specifying
back-end used for export.  INFO is a plist used as
a communication channel."
  (org-element-map tree '(comment comment-block)
    (lambda (c)
      (when (memq (org-element-type (org-export-get-next-element c info))
		  '(comment comment-block))
	(org-element-put-property c :post-blank 0))))
  tree)



;;; End-user functions

;;;###autoload
(defun org-ascii-export-as-ascii
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title and
table of contents from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org ASCII Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'ascii "*Org ASCII Export*"
    async subtreep visible-only body-only ext-plist (lambda () (text-mode))))

;;;###autoload
(defun org-ascii-export-to-ascii
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title and
table of contents from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".txt" subtreep)))
    (org-export-to-file 'ascii file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-ascii-publish-to-ascii (plist filename pub-dir)
  "Publish an Org file to ASCII.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to
   'ascii filename ".txt" `(:ascii-charset ascii ,@plist) pub-dir))

;;;###autoload
(defun org-ascii-publish-to-latin1 (plist filename pub-dir)
  "Publish an Org file to Latin-1.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to
   'ascii filename ".txt" `(:ascii-charset latin1 ,@plist) pub-dir))

;;;###autoload
(defun org-ascii-publish-to-utf8 (plist filename pub-dir)
  "Publish an org file to UTF-8.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to
   'ascii filename ".txt" `(:ascii-charset utf-8 ,@plist) pub-dir))


(provide 'ox-ascii)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; coding: utf-8
;; End:

;;; ox-ascii.el ends here
