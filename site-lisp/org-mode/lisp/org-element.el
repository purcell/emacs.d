;;; org-element.el --- Parser And Applications for Org syntax

;; Copyright (C) 2012-2014 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Org syntax can be divided into three categories: "Greater
;; elements", "Elements" and "Objects".
;;
;; Elements are related to the structure of the document.  Indeed, all
;; elements are a cover for the document: each position within belongs
;; to at least one element.
;;
;; An element always starts and ends at the beginning of a line.  With
;; a few exceptions (`clock', `headline', `inlinetask', `item',
;; `planning', `node-property', `quote-section' `section' and
;; `table-row' types), it can also accept a fixed set of keywords as
;; attributes.  Those are called "affiliated keywords" to distinguish
;; them from other keywords, which are full-fledged elements.  Almost
;; all affiliated keywords are referenced in
;; `org-element-affiliated-keywords'; the others are export attributes
;; and start with "ATTR_" prefix.
;;
;; Element containing other elements (and only elements) are called
;; greater elements.  Concerned types are: `center-block', `drawer',
;; `dynamic-block', `footnote-definition', `headline', `inlinetask',
;; `item', `plain-list', `property-drawer', `quote-block', `section'
;; and `special-block'.
;;
;; Other element types are: `babel-call', `clock', `comment',
;; `comment-block', `diary-sexp', `example-block', `export-block',
;; `fixed-width', `horizontal-rule', `keyword', `latex-environment',
;; `node-property', `paragraph', `planning', `quote-section',
;; `src-block', `table', `table-row' and `verse-block'.  Among them,
;; `paragraph' and `verse-block' types can contain Org objects and
;; plain text.
;;
;; Objects are related to document's contents.  Some of them are
;; recursive.  Associated types are of the following: `bold', `code',
;; `entity', `export-snippet', `footnote-reference',
;; `inline-babel-call', `inline-src-block', `italic',
;; `latex-fragment', `line-break', `link', `macro', `radio-target',
;; `statistics-cookie', `strike-through', `subscript', `superscript',
;; `table-cell', `target', `timestamp', `underline' and `verbatim'.
;;
;; Some elements also have special properties whose value can hold
;; objects themselves (e.g. an item tag or a headline name).  Such
;; values are called "secondary strings".  Any object belongs to
;; either an element or a secondary string.
;;
;; Notwithstanding affiliated keywords, each greater element, element
;; and object has a fixed set of properties attached to it.  Among
;; them, four are shared by all types: `:begin' and `:end', which
;; refer to the beginning and ending buffer positions of the
;; considered element or object, `:post-blank', which holds the number
;; of blank lines, or white spaces, at its end and `:parent' which
;; refers to the element or object containing it.  Greater elements,
;; elements and objects containing objects will also have
;; `:contents-begin' and `:contents-end' properties to delimit
;; contents.  Eventually, greater elements and elements accepting
;; affiliated keywords will have a `:post-affiliated' property,
;; referring to the buffer position after all such keywords.
;;
;; At the lowest level, a `:parent' property is also attached to any
;; string, as a text property.
;;
;; Lisp-wise, an element or an object can be represented as a list.
;; It follows the pattern (TYPE PROPERTIES CONTENTS), where:
;;   TYPE is a symbol describing the Org element or object.
;;   PROPERTIES is the property list attached to it.  See docstring of
;;              appropriate parsing function to get an exhaustive
;;              list.
;;   CONTENTS is a list of elements, objects or raw strings contained
;;            in the current element or object, when applicable.
;;
;; An Org buffer is a nested list of such elements and objects, whose
;; type is `org-data' and properties is nil.
;;
;; The first part of this file defines Org syntax, while the second
;; one provide accessors and setters functions.
;;
;; The next part implements a parser and an interpreter for each
;; element and object type in Org syntax.
;;
;; The following part creates a fully recursive buffer parser.  It
;; also provides a tool to map a function to elements or objects
;; matching some criteria in the parse tree.  Functions of interest
;; are `org-element-parse-buffer', `org-element-map' and, to a lesser
;; extent, `org-element-parse-secondary-string'.
;;
;; The penultimate part is the cradle of an interpreter for the
;; obtained parse tree: `org-element-interpret-data'.
;;
;; The library ends by furnishing `org-element-at-point' function, and
;; a way to give information about document structure around point
;; with `org-element-context'.


;;; Code:

(eval-when-compile (require 'cl))
(require 'org)



;;; Definitions And Rules
;;
;; Define elements, greater elements and specify recursive objects,
;; along with the affiliated keywords recognized.  Also set up
;; restrictions on recursive objects combinations.
;;
;; These variables really act as a control center for the parsing
;; process.

(defconst org-element-paragraph-separate
  (concat "^\\(?:"
          ;; Headlines, inlinetasks.
          org-outline-regexp "\\|"
          ;; Footnote definitions.
	  "\\[\\(?:[0-9]+\\|fn:[-_[:word:]]+\\)\\]" "\\|"
	  ;; Diary sexps.
	  "%%(" "\\|"
          "[ \t]*\\(?:"
          ;; Empty lines.
          "$" "\\|"
	  ;; Tables (any type).
	  "\\(?:|\\|\\+-[-+]\\)" "\\|"
          ;; Blocks (any type), Babel calls and keywords.  Note: this
	  ;; is only an indication and need some thorough check.
          "#\\(?:[+ ]\\|$\\)" "\\|"
	  ;; Drawers (any type) and fixed-width areas.  This is also
	  ;; only an indication.
	  ":" "\\|"
          ;; Horizontal rules.
          "-\\{5,\\}[ \t]*$" "\\|"
          ;; LaTeX environments.
          "\\\\begin{\\([A-Za-z0-9]+\\*?\\)}" "\\|"
          ;; Planning and Clock lines.
          (regexp-opt (list org-scheduled-string
                            org-deadline-string
                            org-closed-string
                            org-clock-string))
          "\\|"
          ;; Lists.
          (let ((term (case org-plain-list-ordered-item-terminator
                        (?\) ")") (?. "\\.") (otherwise "[.)]")))
                (alpha (and org-list-allow-alphabetical "\\|[A-Za-z]")))
            (concat "\\(?:[-+*]\\|\\(?:[0-9]+" alpha "\\)" term "\\)"
                    "\\(?:[ \t]\\|$\\)"))
          "\\)\\)")
  "Regexp to separate paragraphs in an Org buffer.
In the case of lines starting with \"#\" and \":\", this regexp
is not sufficient to know if point is at a paragraph ending.  See
`org-element-paragraph-parser' for more information.")

(defconst org-element-all-elements
  '(babel-call center-block clock comment comment-block diary-sexp drawer
	       dynamic-block example-block export-block fixed-width
	       footnote-definition headline horizontal-rule inlinetask item
	       keyword latex-environment node-property paragraph plain-list
	       planning property-drawer quote-block quote-section section
	       special-block src-block table table-row verse-block)
  "Complete list of element types.")

(defconst org-element-greater-elements
  '(center-block drawer dynamic-block footnote-definition headline inlinetask
		 item plain-list property-drawer quote-block section
		 special-block table)
  "List of recursive element types aka Greater Elements.")

(defconst org-element-all-successors
  '(link export-snippet footnote-reference inline-babel-call
	 inline-src-block latex-or-entity line-break macro plain-link
	 radio-target statistics-cookie sub/superscript table-cell target
	 text-markup timestamp)
  "Complete list of successors.")

(defconst org-element-object-successor-alist
  '((subscript . sub/superscript) (superscript . sub/superscript)
    (bold . text-markup) (code . text-markup) (italic . text-markup)
    (strike-through . text-markup) (underline . text-markup)
    (verbatim . text-markup) (entity . latex-or-entity)
    (latex-fragment . latex-or-entity))
  "Alist of translations between object type and successor name.
Sharing the same successor comes handy when, for example, the
regexp matching one object can also match the other object.")

(defconst org-element-all-objects
  '(bold code entity export-snippet footnote-reference inline-babel-call
	 inline-src-block italic line-break latex-fragment link macro
	 radio-target statistics-cookie strike-through subscript superscript
	 table-cell target timestamp underline verbatim)
  "Complete list of object types.")

(defconst org-element-recursive-objects
  '(bold italic link subscript radio-target strike-through superscript
	 table-cell underline)
  "List of recursive object types.")

(defvar org-element-block-name-alist
  '(("CENTER" . org-element-center-block-parser)
    ("COMMENT" . org-element-comment-block-parser)
    ("EXAMPLE" . org-element-example-block-parser)
    ("QUOTE" . org-element-quote-block-parser)
    ("SRC" . org-element-src-block-parser)
    ("VERSE" . org-element-verse-block-parser))
  "Alist between block names and the associated parsing function.
Names must be uppercase.  Any block whose name has no association
is parsed with `org-element-special-block-parser'.")

(defconst org-element-link-type-is-file
  '("file" "file+emacs" "file+sys" "docview")
  "List of link types equivalent to \"file\".
Only these types can accept search options and an explicit
application to open them.")

(defconst org-element-affiliated-keywords
  '("CAPTION" "DATA" "HEADER" "HEADERS" "LABEL" "NAME" "PLOT" "RESNAME" "RESULT"
    "RESULTS" "SOURCE" "SRCNAME" "TBLNAME")
  "List of affiliated keywords as strings.
By default, all keywords setting attributes (e.g., \"ATTR_LATEX\")
are affiliated keywords and need not to be in this list.")

(defconst org-element-keyword-translation-alist
  '(("DATA" . "NAME")  ("LABEL" . "NAME") ("RESNAME" . "NAME")
    ("SOURCE" . "NAME") ("SRCNAME" . "NAME") ("TBLNAME" . "NAME")
    ("RESULT" . "RESULTS") ("HEADERS" . "HEADER"))
  "Alist of usual translations for keywords.
The key is the old name and the value the new one.  The property
holding their value will be named after the translated name.")

(defconst org-element-multiple-keywords '("CAPTION" "HEADER")
  "List of affiliated keywords that can occur more than once in an element.

Their value will be consed into a list of strings, which will be
returned as the value of the property.

This list is checked after translations have been applied.  See
`org-element-keyword-translation-alist'.

By default, all keywords setting attributes (e.g., \"ATTR_LATEX\")
allow multiple occurrences and need not to be in this list.")

(defconst org-element-parsed-keywords '("CAPTION")
  "List of affiliated keywords whose value can be parsed.

Their value will be stored as a secondary string: a list of
strings and objects.

This list is checked after translations have been applied.  See
`org-element-keyword-translation-alist'.")

(defconst org-element-dual-keywords '("CAPTION" "RESULTS")
  "List of affiliated keywords which can have a secondary value.

In Org syntax, they can be written with optional square brackets
before the colons.  For example, RESULTS keyword can be
associated to a hash value with the following:

  #+RESULTS[hash-string]: some-source

This list is checked after translations have been applied.  See
`org-element-keyword-translation-alist'.")

(defconst org-element-document-properties '("AUTHOR" "DATE" "TITLE")
  "List of properties associated to the whole document.
Any keyword in this list will have its value parsed and stored as
a secondary string.")

(defconst org-element--affiliated-re
  (format "[ \t]*#\\+\\(?:%s\\):\\(?: \\|$\\)"
	  (concat
	   ;; Dual affiliated keywords.
	   (format "\\(?1:%s\\)\\(?:\\[\\(.*\\)\\]\\)?"
		   (regexp-opt org-element-dual-keywords))
	   "\\|"
	   ;; Regular affiliated keywords.
	   (format "\\(?1:%s\\)"
		   (regexp-opt
		    (org-remove-if
		     #'(lambda (keyword)
			 (member keyword org-element-dual-keywords))
		     org-element-affiliated-keywords)))
	   "\\|"
	   ;; Export attributes.
	   "\\(?1:ATTR_[-_A-Za-z0-9]+\\)"))
  "Regexp matching any affiliated keyword.

Keyword name is put in match group 1.  Moreover, if keyword
belongs to `org-element-dual-keywords', put the dual value in
match group 2.

Don't modify it, set `org-element-affiliated-keywords' instead.")

(defconst org-element-object-restrictions
  (let* ((standard-set
	  (remq 'plain-link (remq 'table-cell org-element-all-successors)))
	 (standard-set-no-line-break (remq 'line-break standard-set)))
    `((bold ,@standard-set)
      (footnote-reference ,@standard-set)
      (headline ,@standard-set-no-line-break)
      (inlinetask ,@standard-set-no-line-break)
      (italic ,@standard-set)
      (item ,@standard-set-no-line-break)
      (keyword ,@standard-set)
      ;; Ignore all links excepted plain links in a link description.
      ;; Also ignore radio-targets and line breaks.
      (link export-snippet inline-babel-call inline-src-block latex-or-entity
	    macro plain-link statistics-cookie sub/superscript text-markup)
      (paragraph ,@standard-set)
      ;; Remove any variable object from radio target as it would
      ;; prevent it from being properly recognized.
      (radio-target latex-or-entity sub/superscript text-markup)
      (strike-through ,@standard-set)
      (subscript ,@standard-set)
      (superscript ,@standard-set)
      ;; Ignore inline babel call and inline src block as formulas are
      ;; possible.  Also ignore line breaks and statistics cookies.
      (table-cell link export-snippet footnote-reference latex-or-entity macro
		  radio-target sub/superscript target text-markup timestamp)
      (table-row table-cell)
      (underline ,@standard-set)
      (verse-block ,@standard-set)))
  "Alist of objects restrictions.

CAR is an element or object type containing objects and CDR is
a list of successors that will be called within an element or
object of such type.

For example, in a `radio-target' object, one can only find
entities, latex-fragments, subscript, superscript and text
markup.

This alist also applies to secondary string.  For example, an
`headline' type element doesn't directly contain objects, but
still has an entry since one of its properties (`:title') does.")

(defconst org-element-secondary-value-alist
  '((headline . :title)
    (inlinetask . :title)
    (item . :tag)
    (footnote-reference . :inline-definition))
  "Alist between element types and location of secondary value.")

(defconst org-element-object-variables '(org-link-abbrev-alist-local)
  "List of buffer-local variables used when parsing objects.
These variables are copied to the temporary buffer created by
`org-export-secondary-string'.")



;;; Accessors and Setters
;;
;; Provide four accessors: `org-element-type', `org-element-property'
;; `org-element-contents' and `org-element-restriction'.
;;
;; Setter functions allow to modify elements by side effect.  There is
;; `org-element-put-property', `org-element-set-contents',
;; `org-element-set-element' and `org-element-adopt-element'.  Note
;; that `org-element-set-element' and `org-element-adopt-elements' are
;; higher level functions since also update `:parent' property.

(defsubst org-element-type (element)
  "Return type of ELEMENT.

The function returns the type of the element or object provided.
It can also return the following special value:
  `plain-text'       for a string
  `org-data'         for a complete document
  nil                in any other case."
  (cond
   ((not (consp element)) (and (stringp element) 'plain-text))
   ((symbolp (car element)) (car element))))

(defsubst org-element-property (property element)
  "Extract the value from the PROPERTY of an ELEMENT."
  (if (stringp element) (get-text-property 0 property element)
    (plist-get (nth 1 element) property)))

(defsubst org-element-contents (element)
  "Extract contents from an ELEMENT."
  (cond ((not (consp element)) nil)
	((symbolp (car element)) (nthcdr 2 element))
	(t element)))

(defsubst org-element-restriction (element)
  "Return restriction associated to ELEMENT.
ELEMENT can be an element, an object or a symbol representing an
element or object type."
  (cdr (assq (if (symbolp element) element (org-element-type element))
	     org-element-object-restrictions)))

(defsubst org-element-put-property (element property value)
  "In ELEMENT set PROPERTY to VALUE.
Return modified element."
  (if (stringp element) (org-add-props element nil property value)
    (setcar (cdr element) (plist-put (nth 1 element) property value))
    element))

(defsubst org-element-set-contents (element &rest contents)
  "Set ELEMENT contents to CONTENTS.
Return modified element."
  (cond ((not element) (list contents))
	((not (symbolp (car element))) contents)
	((cdr element) (setcdr (cdr element) contents))
	(t (nconc element contents))))

(defsubst org-element-set-element (old new)
  "Replace element or object OLD with element or object NEW.
The function takes care of setting `:parent' property for NEW."
  ;; Since OLD is going to be changed into NEW by side-effect, first
  ;; make sure that every element or object within NEW has OLD as
  ;; parent.
  (mapc (lambda (blob) (org-element-put-property blob :parent old))
	(org-element-contents new))
  ;; Transfer contents.
  (apply 'org-element-set-contents old (org-element-contents new))
  ;; Ensure NEW has same parent as OLD, then overwrite OLD properties
  ;; with NEW's.
  (org-element-put-property new :parent (org-element-property :parent old))
  (setcar (cdr old) (nth 1 new))
  ;; Transfer type.
  (setcar old (car new)))

(defsubst org-element-adopt-elements (parent &rest children)
  "Append elements to the contents of another element.

PARENT is an element or object.  CHILDREN can be elements,
objects, or a strings.

The function takes care of setting `:parent' property for CHILD.
Return parent element."
  ;; Link every child to PARENT. If PARENT is nil, it is a secondary
  ;; string: parent is the list itself.
  (mapc (lambda (child)
	  (org-element-put-property child :parent (or parent children)))
	children)
  ;; Add CHILDREN at the end of PARENT contents.
  (when parent
    (apply 'org-element-set-contents
	   parent
	   (nconc (org-element-contents parent) children)))
  ;; Return modified PARENT element.
  (or parent children))



;;; Greater elements
;;
;; For each greater element type, we define a parser and an
;; interpreter.
;;
;; A parser returns the element or object as the list described above.
;; Most of them accepts no argument.  Though, exceptions exist.  Hence
;; every element containing a secondary string (see
;; `org-element-secondary-value-alist') will accept an optional
;; argument to toggle parsing of that secondary string.  Moreover,
;; `item' parser requires current list's structure as its first
;; element.
;;
;; An interpreter accepts two arguments: the list representation of
;; the element or object, and its contents.  The latter may be nil,
;; depending on the element or object considered.  It returns the
;; appropriate Org syntax, as a string.
;;
;; Parsing functions must follow the naming convention:
;; org-element-TYPE-parser, where TYPE is greater element's type, as
;; defined in `org-element-greater-elements'.
;;
;; Similarly, interpreting functions must follow the naming
;; convention: org-element-TYPE-interpreter.
;;
;; With the exception of `headline' and `item' types, greater elements
;; cannot contain other greater elements of their own type.
;;
;; Beside implementing a parser and an interpreter, adding a new
;; greater element requires to tweak `org-element--current-element'.
;; Moreover, the newly defined type must be added to both
;; `org-element-all-elements' and `org-element-greater-elements'.


;;;; Center Block

(defun org-element-center-block-parser (limit affiliated)
  "Parse a center block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `center-block' and CDR is a plist
containing `:begin', `:end', `:hiddenp', `:contents-begin',
`:contents-end', `:post-blank' and `:post-affiliated' keywords.

Assume point is at the beginning of the block."
  (let ((case-fold-search t))
    (if (not (save-excursion
	       (re-search-forward "^[ \t]*#\\+END_CENTER[ \t]*$" limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((block-end-line (match-beginning 0)))
	(let* ((begin (car affiliated))
	       (post-affiliated (point))
	       ;; Empty blocks have no contents.
	       (contents-begin (progn (forward-line)
				      (and (< (point) block-end-line)
					   (point))))
	       (contents-end (and contents-begin block-end-line))
	       (hidden (org-invisible-p2))
	       (pos-before-blank (progn (goto-char block-end-line)
					(forward-line)
					(point)))
	       (end (save-excursion
		      (skip-chars-forward " \r\t\n" limit)
		      (if (eobp) (point) (line-beginning-position)))))
	  (list 'center-block
		(nconc
		 (list :begin begin
		       :end end
		       :hiddenp hidden
		       :contents-begin contents-begin
		       :contents-end contents-end
		       :post-blank (count-lines pos-before-blank end)
		       :post-affiliated post-affiliated)
		 (cdr affiliated))))))))

(defun org-element-center-block-interpreter (center-block contents)
  "Interpret CENTER-BLOCK element as Org syntax.
CONTENTS is the contents of the element."
  (format "#+BEGIN_CENTER\n%s#+END_CENTER" contents))


;;;; Drawer

(defun org-element-drawer-parser (limit affiliated)
  "Parse a drawer.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `drawer' and CDR is a plist containing
`:drawer-name', `:begin', `:end', `:hiddenp', `:contents-begin',
`:contents-end', `:post-blank' and `:post-affiliated' keywords.

Assume point is at beginning of drawer."
  (let ((case-fold-search t))
    (if (not (save-excursion (re-search-forward "^[ \t]*:END:[ \t]*$" limit t)))
	;; Incomplete drawer: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (save-excursion
	(let* ((drawer-end-line (match-beginning 0))
	       (name (progn (looking-at org-drawer-regexp)
			    (org-match-string-no-properties 1)))
	       (begin (car affiliated))
	       (post-affiliated (point))
	       ;; Empty drawers have no contents.
	       (contents-begin (progn (forward-line)
				      (and (< (point) drawer-end-line)
					   (point))))
	       (contents-end (and contents-begin drawer-end-line))
	       (hidden (org-invisible-p2))
	       (pos-before-blank (progn (goto-char drawer-end-line)
					(forward-line)
					(point)))
	       (end (progn (skip-chars-forward " \r\t\n" limit)
			   (if (eobp) (point) (line-beginning-position)))))
	  (list 'drawer
		(nconc
		 (list :begin begin
		       :end end
		       :drawer-name name
		       :hiddenp hidden
		       :contents-begin contents-begin
		       :contents-end contents-end
		       :post-blank (count-lines pos-before-blank end)
		       :post-affiliated post-affiliated)
		 (cdr affiliated))))))))

(defun org-element-drawer-interpreter (drawer contents)
  "Interpret DRAWER element as Org syntax.
CONTENTS is the contents of the element."
  (format ":%s:\n%s:END:"
	  (org-element-property :drawer-name drawer)
	  contents))


;;;; Dynamic Block

(defun org-element-dynamic-block-parser (limit affiliated)
  "Parse a dynamic block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `dynamic-block' and CDR is a plist
containing `:block-name', `:begin', `:end', `:hiddenp',
`:contents-begin', `:contents-end', `:arguments', `:post-blank'
and `:post-affiliated' keywords.

Assume point is at beginning of dynamic block."
  (let ((case-fold-search t))
    (if (not (save-excursion
	       (re-search-forward "^[ \t]*#\\+END:?[ \t]*$" limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((block-end-line (match-beginning 0)))
	(save-excursion
	  (let* ((name (progn (looking-at org-dblock-start-re)
			      (org-match-string-no-properties 1)))
		 (arguments (org-match-string-no-properties 3))
		 (begin (car affiliated))
		 (post-affiliated (point))
		 ;; Empty blocks have no contents.
		 (contents-begin (progn (forward-line)
					(and (< (point) block-end-line)
					     (point))))
		 (contents-end (and contents-begin block-end-line))
		 (hidden (org-invisible-p2))
		 (pos-before-blank (progn (goto-char block-end-line)
					  (forward-line)
					  (point)))
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position)))))
	    (list 'dynamic-block
		  (nconc
		   (list :begin begin
			 :end end
			 :block-name name
			 :arguments arguments
			 :hiddenp hidden
			 :contents-begin contents-begin
			 :contents-end contents-end
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-dynamic-block-interpreter (dynamic-block contents)
  "Interpret DYNAMIC-BLOCK element as Org syntax.
CONTENTS is the contents of the element."
  (format "#+BEGIN: %s%s\n%s#+END:"
	  (org-element-property :block-name dynamic-block)
	  (let ((args (org-element-property :arguments dynamic-block)))
	    (and args (concat " " args)))
	  contents))


;;;; Footnote Definition

(defun org-element-footnote-definition-parser (limit affiliated)
  "Parse a footnote definition.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `footnote-definition' and CDR is
a plist containing `:label', `:begin' `:end', `:contents-begin',
`:contents-end', `:post-blank' and `:post-affiliated' keywords.

Assume point is at the beginning of the footnote definition."
  (save-excursion
    (let* ((label (progn (looking-at org-footnote-definition-re)
			 (org-match-string-no-properties 1)))
	   (begin (car affiliated))
	   (post-affiliated (point))
	   (ending (save-excursion
		     (if (progn
			   (end-of-line)
			   (re-search-forward
			    (concat org-outline-regexp-bol "\\|"
				    org-footnote-definition-re "\\|"
				    "^\\([ \t]*\n\\)\\{2,\\}") limit 'move))
			 (match-beginning 0)
		       (point))))
	   (contents-begin (progn
			     (search-forward "]")
			     (skip-chars-forward " \r\t\n" ending)
			     (cond ((= (point) ending) nil)
				   ((= (line-beginning-position) begin) (point))
				   (t (line-beginning-position)))))
	   (contents-end (and contents-begin ending))
	   (end (progn (goto-char ending)
		       (skip-chars-forward " \r\t\n" limit)
		       (if (eobp) (point) (line-beginning-position)))))
      (list 'footnote-definition
	    (nconc
	     (list :label label
		   :begin begin
		   :end end
		   :contents-begin contents-begin
		   :contents-end contents-end
		   :post-blank (count-lines ending end)
		   :post-affiliated post-affiliated)
	     (cdr affiliated))))))

(defun org-element-footnote-definition-interpreter (footnote-definition contents)
  "Interpret FOOTNOTE-DEFINITION element as Org syntax.
CONTENTS is the contents of the footnote-definition."
  (concat (format "[%s]" (org-element-property :label footnote-definition))
	  " "
	  contents))


;;;; Headline

(defun org-element-headline-parser (limit &optional raw-secondary-p)
  "Parse a headline.

Return a list whose CAR is `headline' and CDR is a plist
containing `:raw-value', `:title', `:alt-title', `:begin',
`:end', `:pre-blank', `:hiddenp', `:contents-begin' and
`:contents-end', `:level', `:priority', `:tags',
`:todo-keyword',`:todo-type', `:scheduled', `:deadline',
`:closed', `:quotedp', `:archivedp', `:commentedp' and
`:footnote-section-p' keywords.

The plist also contains any property set in the property drawer,
with its name in upper cases and colons added at the
beginning (e.g., `:CUSTOM_ID').

LIMIT is a buffer position bounding the search.

When RAW-SECONDARY-P is non-nil, headline's title will not be
parsed as a secondary string, but as a plain string instead.

Assume point is at beginning of the headline."
  (save-excursion
    (let* ((components (org-heading-components))
	   (level (nth 1 components))
	   (todo (nth 2 components))
	   (todo-type
	    (and todo (if (member todo org-done-keywords) 'done 'todo)))
	   (tags (let ((raw-tags (nth 5 components)))
		   (and raw-tags (org-split-string raw-tags ":"))))
	   (raw-value (or (nth 4 components) ""))
	   (quotedp
	    (let ((case-fold-search nil))
	      (string-match (format "^%s\\( \\|$\\)" org-quote-string)
			    raw-value)))
	   (commentedp
	    (let ((case-fold-search nil))
	      (string-match (format "^%s\\( \\|$\\)" org-comment-string)
			    raw-value)))
	   (archivedp (member org-archive-tag tags))
	   (footnote-section-p (and org-footnote-section
				    (string= org-footnote-section raw-value)))
	   ;; Upcase property names.  It avoids confusion between
	   ;; properties obtained through property drawer and default
	   ;; properties from the parser (e.g. `:end' and :END:)
	   (standard-props
	    (let (plist)
	      (mapc
	       (lambda (p)
		 (setq plist
		       (plist-put plist
				  (intern (concat ":" (upcase (car p))))
				  (cdr p))))
	       (org-entry-properties nil 'standard))
	      plist))
	   (time-props
	    ;; Read time properties on the line below the headline.
	    (save-excursion
	      (when (progn (forward-line)
			   (looking-at org-planning-or-clock-line-re))
		(let ((end (line-end-position)) plist)
		  (while (re-search-forward
			  org-keyword-time-not-clock-regexp end t)
		    (goto-char (match-end 1))
		    (skip-chars-forward " \t")
		    (let ((keyword (match-string 1))
			  (time (org-element-timestamp-parser)))
		      (cond ((equal keyword org-scheduled-string)
			     (setq plist (plist-put plist :scheduled time)))
			    ((equal keyword org-deadline-string)
			     (setq plist (plist-put plist :deadline time)))
			    (t (setq plist (plist-put plist :closed time))))))
		  plist))))
	   (begin (point))
	   (end (min (save-excursion (org-end-of-subtree t t)) limit))
	   (pos-after-head (progn (forward-line) (point)))
	   (contents-begin (save-excursion
			     (skip-chars-forward " \r\t\n" end)
			     (and (/= (point) end) (line-beginning-position))))
	   (hidden (org-invisible-p2))
	   (contents-end (and contents-begin
			      (progn (goto-char end)
				     (skip-chars-backward " \r\t\n")
				     (forward-line)
				     (point)))))
      ;; Clean RAW-VALUE from any quote or comment string.
      (when (or quotedp commentedp)
	(let ((case-fold-search nil))
	  (setq raw-value
		(replace-regexp-in-string
		 (concat
		  (regexp-opt (list org-quote-string org-comment-string))
		  "\\(?: \\|$\\)")
		 ""
		 raw-value))))
      ;; Clean TAGS from archive tag, if any.
      (when archivedp (setq tags (delete org-archive-tag tags)))
      (let ((headline
	     (list 'headline
		   (nconc
		    (list :raw-value raw-value
			  :begin begin
			  :end end
			  :pre-blank
			  (if (not contents-begin) 0
			    (count-lines pos-after-head contents-begin))
			  :hiddenp hidden
			  :contents-begin contents-begin
			  :contents-end contents-end
			  :level level
			  :priority (nth 3 components)
			  :tags tags
			  :todo-keyword todo
			  :todo-type todo-type
			  :post-blank (count-lines
				       (or contents-end pos-after-head)
				       end)
			  :footnote-section-p footnote-section-p
			  :archivedp archivedp
			  :commentedp commentedp
			  :quotedp quotedp)
		    time-props
		    standard-props))))
	(let ((alt-title (org-element-property :ALT_TITLE headline)))
	  (when alt-title
	    (org-element-put-property
	     headline :alt-title
	     (if raw-secondary-p alt-title
	       (org-element-parse-secondary-string
		alt-title (org-element-restriction 'headline) headline)))))
	(org-element-put-property
	 headline :title
	 (if raw-secondary-p raw-value
	   (org-element-parse-secondary-string
	    raw-value (org-element-restriction 'headline) headline)))))))

(defun org-element-headline-interpreter (headline contents)
  "Interpret HEADLINE element as Org syntax.
CONTENTS is the contents of the element."
  (let* ((level (org-element-property :level headline))
	 (todo (org-element-property :todo-keyword headline))
	 (priority (org-element-property :priority headline))
	 (title (org-element-interpret-data
		 (org-element-property :title headline)))
	 (tags (let ((tag-list (if (org-element-property :archivedp headline)
				   (cons org-archive-tag
					 (org-element-property :tags headline))
				 (org-element-property :tags headline))))
		 (and tag-list
		      (format ":%s:" (mapconcat 'identity tag-list ":")))))
	 (commentedp (org-element-property :commentedp headline))
	 (quotedp (org-element-property :quotedp headline))
	 (pre-blank (or (org-element-property :pre-blank headline) 0))
	 (heading (concat (make-string (org-reduced-level level) ?*)
			  (and todo (concat " " todo))
			  (and quotedp (concat " " org-quote-string))
			  (and commentedp (concat " " org-comment-string))
			  (and priority
			       (format " [#%s]" (char-to-string priority)))
			  (cond ((and org-footnote-section
				      (org-element-property
				       :footnote-section-p headline))
				 (concat " " org-footnote-section))
				(title (concat " " title))))))
    (concat heading
	    ;; Align tags.
	    (when tags
	      (cond
	       ((zerop org-tags-column) (format " %s" tags))
	       ((< org-tags-column 0)
		(concat
		 (make-string
		  (max (- (+ org-tags-column (length heading) (length tags))) 1)
		  ? )
		 tags))
	       (t
		(concat
		 (make-string (max (- org-tags-column (length heading)) 1) ? )
		 tags))))
	    (make-string (1+ pre-blank) 10)
	    contents)))


;;;; Inlinetask

(defun org-element-inlinetask-parser (limit &optional raw-secondary-p)
  "Parse an inline task.

Return a list whose CAR is `inlinetask' and CDR is a plist
containing `:title', `:begin', `:end', `:hiddenp',
`:contents-begin' and `:contents-end', `:level', `:priority',
`:raw-value', `:tags', `:todo-keyword', `:todo-type',
`:scheduled', `:deadline', `:closed' and `:post-blank' keywords.

The plist also contains any property set in the property drawer,
with its name in upper cases and colons added at the
beginning (e.g., `:CUSTOM_ID').

When optional argument RAW-SECONDARY-P is non-nil, inline-task's
title will not be parsed as a secondary string, but as a plain
string instead.

Assume point is at beginning of the inline task."
  (save-excursion
    (let* ((begin (point))
	   (components (org-heading-components))
	   (todo (nth 2 components))
	   (todo-type (and todo
			   (if (member todo org-done-keywords) 'done 'todo)))
	   (tags (let ((raw-tags (nth 5 components)))
		   (and raw-tags (org-split-string raw-tags ":"))))
	   (raw-value (or (nth 4 components) ""))
	   ;; Upcase property names.  It avoids confusion between
	   ;; properties obtained through property drawer and default
	   ;; properties from the parser (e.g. `:end' and :END:)
	   (standard-props
	    (let (plist)
	      (mapc
	       (lambda (p)
		 (setq plist
		       (plist-put plist
				  (intern (concat ":" (upcase (car p))))
				  (cdr p))))
	       (org-entry-properties nil 'standard))
	      plist))
	   (time-props
	    ;; Read time properties on the line below the inlinetask
	    ;; opening string.
	    (save-excursion
	      (when (progn (forward-line)
			   (looking-at org-planning-or-clock-line-re))
		(let ((end (line-end-position)) plist)
		  (while (re-search-forward
			  org-keyword-time-not-clock-regexp end t)
		    (goto-char (match-end 1))
		    (skip-chars-forward " \t")
		    (let ((keyword (match-string 1))
			  (time (org-element-timestamp-parser)))
		      (cond ((equal keyword org-scheduled-string)
			     (setq plist (plist-put plist :scheduled time)))
			    ((equal keyword org-deadline-string)
			     (setq plist (plist-put plist :deadline time)))
			    (t (setq plist (plist-put plist :closed time))))))
		  plist))))
	   (task-end (save-excursion
		       (end-of-line)
		       (and (re-search-forward org-outline-regexp-bol limit t)
			    (org-looking-at-p "END[ \t]*$")
			    (line-beginning-position))))
	   (contents-begin (progn (forward-line)
				  (and task-end (< (point) task-end) (point))))
	   (hidden (and contents-begin (org-invisible-p2)))
	   (contents-end (and contents-begin task-end))
	   (before-blank (if (not task-end) (point)
			   (goto-char task-end)
			   (forward-line)
			   (point)))
	   (end (progn (skip-chars-forward " \r\t\n" limit)
		       (if (eobp) (point) (line-beginning-position))))
	   (inlinetask
	    (list 'inlinetask
		  (nconc
		   (list :raw-value raw-value
			 :begin begin
			 :end end
			 :hiddenp hidden
			 :contents-begin contents-begin
			 :contents-end contents-end
			 :level (nth 1 components)
			 :priority (nth 3 components)
			 :tags tags
			 :todo-keyword todo
			 :todo-type todo-type
			 :post-blank (count-lines before-blank end))
		   time-props
		   standard-props))))
      (org-element-put-property
       inlinetask :title
       (if raw-secondary-p raw-value
	 (org-element-parse-secondary-string
	  raw-value
	  (org-element-restriction 'inlinetask)
	  inlinetask))))))

(defun org-element-inlinetask-interpreter (inlinetask contents)
  "Interpret INLINETASK element as Org syntax.
CONTENTS is the contents of inlinetask."
  (let* ((level (org-element-property :level inlinetask))
	 (todo (org-element-property :todo-keyword inlinetask))
	 (priority (org-element-property :priority inlinetask))
	 (title (org-element-interpret-data
		 (org-element-property :title inlinetask)))
	 (tags (let ((tag-list (org-element-property :tags inlinetask)))
		 (and tag-list
		      (format ":%s:" (mapconcat 'identity tag-list ":")))))
	 (task (concat (make-string level ?*)
		       (and todo (concat " " todo))
		       (and priority
			    (format " [#%s]" (char-to-string priority)))
		       (and title (concat " " title)))))
    (concat task
	    ;; Align tags.
	    (when tags
	      (cond
	       ((zerop org-tags-column) (format " %s" tags))
	       ((< org-tags-column 0)
		(concat
		 (make-string
		  (max (- (+ org-tags-column (length task) (length tags))) 1)
		  ? )
		 tags))
	       (t
		(concat
		 (make-string (max (- org-tags-column (length task)) 1) ? )
		 tags))))
	    ;; Prefer degenerate inlinetasks when there are no
	    ;; contents.
	    (when contents
	      (concat "\n"
		      contents
		      (make-string level ?*) " END")))))


;;;; Item

(defun org-element-item-parser (limit struct &optional raw-secondary-p)
  "Parse an item.

STRUCT is the structure of the plain list.

Return a list whose CAR is `item' and CDR is a plist containing
`:bullet', `:begin', `:end', `:contents-begin', `:contents-end',
`:checkbox', `:counter', `:tag', `:structure', `:hiddenp' and
`:post-blank' keywords.

When optional argument RAW-SECONDARY-P is non-nil, item's tag, if
any, will not be parsed as a secondary string, but as a plain
string instead.

Assume point is at the beginning of the item."
  (save-excursion
    (beginning-of-line)
    (looking-at org-list-full-item-re)
    (let* ((begin (point))
	   (bullet (org-match-string-no-properties 1))
	   (checkbox (let ((box (org-match-string-no-properties 3)))
		       (cond ((equal "[ ]" box) 'off)
			     ((equal "[X]" box) 'on)
			     ((equal "[-]" box) 'trans))))
	   (counter (let ((c (org-match-string-no-properties 2)))
		      (save-match-data
			(cond
			 ((not c) nil)
			 ((string-match "[A-Za-z]" c)
			  (- (string-to-char (upcase (match-string 0 c)))
			     64))
			 ((string-match "[0-9]+" c)
			  (string-to-number (match-string 0 c)))))))
	   (end (save-excursion (goto-char (org-list-get-item-end begin struct))
				(unless (bolp) (forward-line))
				(point)))
	   (contents-begin
	    (progn (goto-char
		    ;; Ignore tags in un-ordered lists: they are just
		    ;; a part of item's body.
		    (if (and (match-beginning 4)
			     (save-match-data (string-match "[.)]" bullet)))
			(match-beginning 4)
		      (match-end 0)))
		   (skip-chars-forward " \r\t\n" limit)
		   ;; If first line isn't empty, contents really start
		   ;; at the text after item's meta-data.
		   (if (= (point-at-bol) begin) (point) (point-at-bol))))
	   (hidden (progn (forward-line)
			  (and (not (= (point) end)) (org-invisible-p2))))
	   (contents-end (progn (goto-char end)
				(skip-chars-backward " \r\t\n")
				(forward-line)
				(point)))
	   (item
	    (list 'item
		  (list :bullet bullet
			:begin begin
			:end end
			;; CONTENTS-BEGIN and CONTENTS-END may be
			;; mixed up in the case of an empty item
			;; separated from the next by a blank line.
			;; Thus ensure the former is always the
			;; smallest.
			:contents-begin (min contents-begin contents-end)
			:contents-end (max contents-begin contents-end)
			:checkbox checkbox
			:counter counter
			:hiddenp hidden
			:structure struct
			:post-blank (count-lines contents-end end)))))
      (org-element-put-property
       item :tag
       (let ((raw-tag (org-list-get-tag begin struct)))
	 (and raw-tag
	      (if raw-secondary-p raw-tag
		(org-element-parse-secondary-string
		 raw-tag (org-element-restriction 'item) item))))))))

(defun org-element-item-interpreter (item contents)
  "Interpret ITEM element as Org syntax.
CONTENTS is the contents of the element."
  (let* ((bullet (let ((bullet (org-element-property :bullet item)))
		   (org-list-bullet-string
		    (cond ((not (string-match "[0-9a-zA-Z]" bullet)) "- ")
			  ((eq org-plain-list-ordered-item-terminator ?\)) "1)")
			  (t "1.")))))
	 (checkbox (org-element-property :checkbox item))
	 (counter (org-element-property :counter item))
	 (tag (let ((tag (org-element-property :tag item)))
		(and tag (org-element-interpret-data tag))))
	 ;; Compute indentation.
	 (ind (make-string (length bullet) 32))
	 (item-starts-with-par-p
	  (eq (org-element-type (car (org-element-contents item)))
	      'paragraph)))
    ;; Indent contents.
    (concat
     bullet
     (and counter (format "[@%d] " counter))
     (case checkbox
       (on "[X] ")
       (off "[ ] ")
       (trans "[-] "))
     (and tag (format "%s :: " tag))
     (when contents
       (let ((contents (replace-regexp-in-string
			"\\(^\\)[ \t]*\\S-" ind contents nil nil 1)))
	 (if item-starts-with-par-p (org-trim contents)
	   (concat "\n" contents)))))))


;;;; Plain List

(defun org-element--list-struct (limit)
  ;; Return structure of list at point.  Internal function.  See
  ;; `org-list-struct' for details.
  (let ((case-fold-search t)
	(top-ind limit)
	(item-re (org-item-re))
	(drawers-re (concat ":\\("
			    (mapconcat 'regexp-quote org-drawers "\\|")
			    "\\):[ \t]*$"))
	(inlinetask-re (and (featurep 'org-inlinetask) "^\\*+ "))
	items struct)
    (save-excursion
      (catch 'exit
	(while t
	  (cond
	   ;; At limit: end all items.
	   ((>= (point) limit)
	    (throw 'exit
		   (let ((end (progn (skip-chars-backward " \r\t\n")
				     (forward-line)
				     (point))))
		     (dolist (item items (sort (nconc items struct)
					       'car-less-than-car))
		       (setcar (nthcdr 6 item) end)))))
	   ;; At list end: end all items.
	   ((looking-at org-list-end-re)
	    (throw 'exit (dolist (item items (sort (nconc items struct)
						   'car-less-than-car))
			   (setcar (nthcdr 6 item) (point)))))
	   ;; At a new item: end previous sibling.
	   ((looking-at item-re)
	    (let ((ind (save-excursion (skip-chars-forward " \t")
				       (current-column))))
	      (setq top-ind (min top-ind ind))
	      (while (and items (<= ind (nth 1 (car items))))
		(let ((item (pop items)))
		  (setcar (nthcdr 6 item) (point))
		  (push item struct)))
	      (push (progn (looking-at org-list-full-item-re)
			   (let ((bullet (match-string-no-properties 1)))
			     (list (point)
				   ind
				   bullet
				   (match-string-no-properties 2) ; counter
				   (match-string-no-properties 3) ; checkbox
				   ;; Description tag.
				   (and (save-match-data
					  (string-match "[-+*]" bullet))
					(match-string-no-properties 4))
				   ;; Ending position, unknown so far.
				   nil)))
		    items))
	    (forward-line 1))
	   ;; Skip empty lines.
	   ((looking-at "^[ \t]*$") (forward-line))
	   ;; Skip inline tasks and blank lines along the way.
	   ((and inlinetask-re (looking-at inlinetask-re))
	    (forward-line)
	    (let ((origin (point)))
	      (when (re-search-forward inlinetask-re limit t)
		(if (org-looking-at-p "END[ \t]*$") (forward-line)
		  (goto-char origin)))))
	   ;; At some text line.  Check if it ends any previous item.
	   (t
	    (let ((ind (progn (skip-chars-forward " \t") (current-column))))
	      (when (<= ind top-ind)
		(skip-chars-backward " \r\t\n")
		(forward-line))
	      (while (<= ind (nth 1 (car items)))
		(let ((item (pop items)))
		  (setcar (nthcdr 6 item) (line-beginning-position))
		  (push item struct)
		  (unless items
		    (throw 'exit (sort struct 'car-less-than-car))))))
	    ;; Skip blocks (any type) and drawers contents.
	    (cond
	     ((and (looking-at "#\\+BEGIN\\(:\\|_\\S-+\\)")
		   (re-search-forward
		    (format "^[ \t]*#\\+END%s[ \t]*$"
			    (org-match-string-no-properties 1))
		    limit t)))
	     ((and (looking-at drawers-re)
		   (re-search-forward "^[ \t]*:END:[ \t]*$" limit t))))
	    (forward-line))))))))

(defun org-element-plain-list-parser (limit affiliated structure)
  "Parse a plain list.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.  STRUCTURE is the structure of the plain list being
parsed.

Return a list whose CAR is `plain-list' and CDR is a plist
containing `:type', `:begin', `:end', `:contents-begin' and
`:contents-end', `:structure', `:post-blank' and
`:post-affiliated' keywords.

Assume point is at the beginning of the list."
  (save-excursion
    (let* ((struct (or structure (org-element--list-struct limit)))
	   (prevs (org-list-prevs-alist struct))
	   (type (org-list-get-list-type (point) struct prevs))
	   (contents-begin (point))
	   (begin (car affiliated))
	   (contents-end
	    (progn (goto-char (org-list-get-list-end (point) struct prevs))
		   (unless (bolp) (forward-line))
		   (point)))
	   (end (progn (skip-chars-forward " \r\t\n" limit)
		       (if (= (point) limit) limit (line-beginning-position)))))
      ;; Return value.
      (list 'plain-list
	    (nconc
	     (list :type type
		   :begin begin
		   :end end
		   :contents-begin contents-begin
		   :contents-end contents-end
		   :structure struct
		   :post-blank (count-lines contents-end end)
		   :post-affiliated contents-begin)
	     (cdr affiliated))))))

(defun org-element-plain-list-interpreter (plain-list contents)
  "Interpret PLAIN-LIST element as Org syntax.
CONTENTS is the contents of the element."
  (with-temp-buffer
    (insert contents)
    (goto-char (point-min))
    (org-list-repair)
    (buffer-string)))


;;;; Property Drawer

(defun org-element-property-drawer-parser (limit affiliated)
  "Parse a property drawer.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `property-drawer' and CDR is a plist
containing `:begin', `:end', `:hiddenp', `:contents-begin',
`:contents-end', `:post-blank' and `:post-affiliated' keywords.

Assume point is at the beginning of the property drawer."
  (save-excursion
    (let ((case-fold-search t))
      (if (not (save-excursion
		 (re-search-forward "^[ \t]*:END:[ \t]*$" limit t)))
	  ;; Incomplete drawer: parse it as a paragraph.
	  (org-element-paragraph-parser limit affiliated)
	(save-excursion
	  (let* ((drawer-end-line (match-beginning 0))
		 (begin (car affiliated))
		 (post-affiliated (point))
		 (contents-begin (progn (forward-line)
					(and (< (point) drawer-end-line)
					     (point))))
		 (contents-end (and contents-begin drawer-end-line))
		 (hidden (org-invisible-p2))
		 (pos-before-blank (progn (goto-char drawer-end-line)
					  (forward-line)
					  (point)))
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position)))))
	    (list 'property-drawer
		  (nconc
		   (list :begin begin
			 :end end
			 :hiddenp hidden
			 :contents-begin contents-begin
			 :contents-end contents-end
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-property-drawer-interpreter (property-drawer contents)
  "Interpret PROPERTY-DRAWER element as Org syntax.
CONTENTS is the properties within the drawer."
  (format ":PROPERTIES:\n%s:END:" contents))


;;;; Quote Block

(defun org-element-quote-block-parser (limit affiliated)
  "Parse a quote block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `quote-block' and CDR is a plist
containing `:begin', `:end', `:hiddenp', `:contents-begin',
`:contents-end', `:post-blank' and `:post-affiliated' keywords.

Assume point is at the beginning of the block."
  (let ((case-fold-search t))
    (if (not (save-excursion
	       (re-search-forward "^[ \t]*#\\+END_QUOTE[ \t]*$" limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((block-end-line (match-beginning 0)))
	(save-excursion
	  (let* ((begin (car affiliated))
		 (post-affiliated (point))
		 ;; Empty blocks have no contents.
		 (contents-begin (progn (forward-line)
					(and (< (point) block-end-line)
					     (point))))
		 (contents-end (and contents-begin block-end-line))
		 (hidden (org-invisible-p2))
		 (pos-before-blank (progn (goto-char block-end-line)
					  (forward-line)
					  (point)))
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position)))))
	    (list 'quote-block
		  (nconc
		   (list :begin begin
			 :end end
			 :hiddenp hidden
			 :contents-begin contents-begin
			 :contents-end contents-end
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-quote-block-interpreter (quote-block contents)
  "Interpret QUOTE-BLOCK element as Org syntax.
CONTENTS is the contents of the element."
  (format "#+BEGIN_QUOTE\n%s#+END_QUOTE" contents))


;;;; Section

(defun org-element-section-parser (limit)
  "Parse a section.

LIMIT bounds the search.

Return a list whose CAR is `section' and CDR is a plist
containing `:begin', `:end', `:contents-begin', `contents-end'
and `:post-blank' keywords."
  (save-excursion
    ;; Beginning of section is the beginning of the first non-blank
    ;; line after previous headline.
    (let ((begin (point))
	  (end (progn (org-with-limited-levels (outline-next-heading))
		      (point)))
	  (pos-before-blank (progn (skip-chars-backward " \r\t\n")
				   (forward-line)
				   (point))))
      (list 'section
	    (list :begin begin
		  :end end
		  :contents-begin begin
		  :contents-end pos-before-blank
		  :post-blank (count-lines pos-before-blank end))))))

(defun org-element-section-interpreter (section contents)
  "Interpret SECTION element as Org syntax.
CONTENTS is the contents of the element."
  contents)


;;;; Special Block

(defun org-element-special-block-parser (limit affiliated)
  "Parse a special block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `special-block' and CDR is a plist
containing `:type', `:begin', `:end', `:hiddenp',
`:contents-begin', `:contents-end', `:post-blank' and
`:post-affiliated' keywords.

Assume point is at the beginning of the block."
  (let* ((case-fold-search t)
	 (type (progn (looking-at "[ \t]*#\\+BEGIN_\\(\\S-+\\)")
		      (upcase (match-string-no-properties 1)))))
    (if (not (save-excursion
	       (re-search-forward
		(format "^[ \t]*#\\+END_%s[ \t]*$" (regexp-quote type))
		limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((block-end-line (match-beginning 0)))
	(save-excursion
	  (let* ((begin (car affiliated))
		 (post-affiliated (point))
		 ;; Empty blocks have no contents.
		 (contents-begin (progn (forward-line)
					(and (< (point) block-end-line)
					     (point))))
		 (contents-end (and contents-begin block-end-line))
		 (hidden (org-invisible-p2))
		 (pos-before-blank (progn (goto-char block-end-line)
					  (forward-line)
					  (point)))
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position)))))
	    (list 'special-block
		  (nconc
		   (list :type type
			 :begin begin
			 :end end
			 :hiddenp hidden
			 :contents-begin contents-begin
			 :contents-end contents-end
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-special-block-interpreter (special-block contents)
  "Interpret SPECIAL-BLOCK element as Org syntax.
CONTENTS is the contents of the element."
  (let ((block-type (org-element-property :type special-block)))
    (format "#+BEGIN_%s\n%s#+END_%s" block-type contents block-type)))



;;; Elements
;;
;; For each element, a parser and an interpreter are also defined.
;; Both follow the same naming convention used for greater elements.
;;
;; Also, as for greater elements, adding a new element type is done
;; through the following steps: implement a parser and an interpreter,
;; tweak `org-element--current-element' so that it recognizes the new
;; type and add that new type to `org-element-all-elements'.
;;
;; As a special case, when the newly defined type is a block type,
;; `org-element-block-name-alist' has to be modified accordingly.


;;;; Babel Call

(defun org-element-babel-call-parser (limit affiliated)
  "Parse a babel call.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `babel-call' and CDR is a plist
containing `:begin', `:end', `:info', `:post-blank' and
`:post-affiliated' as keywords."
  (save-excursion
    (let ((case-fold-search t)
	  (info (progn (looking-at org-babel-block-lob-one-liner-regexp)
		       (org-babel-lob-get-info)))
	  (begin (car affiliated))
	  (post-affiliated (point))
	  (pos-before-blank (progn (forward-line) (point)))
	  (end (progn (skip-chars-forward " \r\t\n" limit)
		      (if (eobp) (point) (line-beginning-position)))))
      (list 'babel-call
	    (nconc
	     (list :begin begin
		   :end end
		   :info info
		   :post-blank (count-lines pos-before-blank end)
		   :post-affiliated post-affiliated)
	     (cdr affiliated))))))

(defun org-element-babel-call-interpreter (babel-call contents)
  "Interpret BABEL-CALL element as Org syntax.
CONTENTS is nil."
  (let* ((babel-info (org-element-property :info babel-call))
	 (main (car babel-info))
	 (post-options (nth 1 babel-info)))
    (concat "#+CALL: "
	    (if (not (string-match "\\[\\(\\[.*?\\]\\)\\]" main)) main
	      ;; Remove redundant square brackets.
	      (replace-match (match-string 1 main) nil nil main))
	    (and post-options (format "[%s]" post-options)))))


;;;; Clock

(defun org-element-clock-parser (limit)
  "Parse a clock.

LIMIT bounds the search.

Return a list whose CAR is `clock' and CDR is a plist containing
`:status', `:value', `:time', `:begin', `:end' and `:post-blank'
as keywords."
  (save-excursion
    (let* ((case-fold-search nil)
	   (begin (point))
	   (value (progn (search-forward org-clock-string (line-end-position) t)
			 (skip-chars-forward " \t")
			 (org-element-timestamp-parser)))
	   (duration (and (search-forward " => " (line-end-position) t)
			  (progn (skip-chars-forward " \t")
				 (looking-at "\\(\\S-+\\)[ \t]*$"))
			  (org-match-string-no-properties 1)))
	   (status (if duration 'closed 'running))
	   (post-blank (let ((before-blank (progn (forward-line) (point))))
			 (skip-chars-forward " \r\t\n" limit)
			 (skip-chars-backward " \t")
			 (unless (bolp) (end-of-line))
			 (count-lines before-blank (point))))
	   (end (point)))
      (list 'clock
	    (list :status status
		  :value value
		  :duration duration
		  :begin begin
		  :end end
		  :post-blank post-blank)))))

(defun org-element-clock-interpreter (clock contents)
  "Interpret CLOCK element as Org syntax.
CONTENTS is nil."
  (concat org-clock-string " "
	  (org-element-timestamp-interpreter
	   (org-element-property :value clock) nil)
	  (let ((duration (org-element-property :duration clock)))
	    (and duration
		 (concat " => "
			 (apply 'format
				"%2s:%02s"
				(org-split-string duration ":")))))))


;;;; Comment

(defun org-element-comment-parser (limit affiliated)
  "Parse a comment.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `comment' and CDR is a plist
containing `:begin', `:end', `:value', `:post-blank',
`:post-affiliated' keywords.

Assume point is at comment beginning."
  (save-excursion
    (let* ((begin (car affiliated))
	   (post-affiliated (point))
	   (value (prog2 (looking-at "[ \t]*# ?")
		      (buffer-substring-no-properties
		       (match-end 0) (line-end-position))
		    (forward-line)))
	   (com-end
	    ;; Get comments ending.
	    (progn
	      (while (and (< (point) limit) (looking-at "[ \t]*#\\( \\|$\\)"))
		;; Accumulate lines without leading hash and first
		;; whitespace.
		(setq value
		      (concat value
			      "\n"
			      (buffer-substring-no-properties
			       (match-end 0) (line-end-position))))
		(forward-line))
	      (point)))
	   (end (progn (goto-char com-end)
		       (skip-chars-forward " \r\t\n" limit)
		       (if (eobp) (point) (line-beginning-position)))))
      (list 'comment
	    (nconc
	     (list :begin begin
		   :end end
		   :value value
		   :post-blank (count-lines com-end end)
		   :post-affiliated post-affiliated)
	     (cdr affiliated))))))

(defun org-element-comment-interpreter (comment contents)
  "Interpret COMMENT element as Org syntax.
CONTENTS is nil."
  (replace-regexp-in-string "^" "# " (org-element-property :value comment)))


;;;; Comment Block

(defun org-element-comment-block-parser (limit affiliated)
  "Parse an export block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `comment-block' and CDR is a plist
containing `:begin', `:end', `:hiddenp', `:value', `:post-blank'
and `:post-affiliated' keywords.

Assume point is at comment block beginning."
  (let ((case-fold-search t))
    (if (not (save-excursion
	       (re-search-forward "^[ \t]*#\\+END_COMMENT[ \t]*$" limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((contents-end (match-beginning 0)))
	(save-excursion
	  (let* ((begin (car affiliated))
		 (post-affiliated (point))
		 (contents-begin (progn (forward-line) (point)))
		 (hidden (org-invisible-p2))
		 (pos-before-blank (progn (goto-char contents-end)
					  (forward-line)
					  (point)))
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position))))
		 (value (buffer-substring-no-properties
			 contents-begin contents-end)))
	    (list 'comment-block
		  (nconc
		   (list :begin begin
			 :end end
			 :value value
			 :hiddenp hidden
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-comment-block-interpreter (comment-block contents)
  "Interpret COMMENT-BLOCK element as Org syntax.
CONTENTS is nil."
  (format "#+BEGIN_COMMENT\n%s#+END_COMMENT"
	  (org-remove-indentation (org-element-property :value comment-block))))


;;;; Diary Sexp

(defun org-element-diary-sexp-parser (limit affiliated)
  "Parse a diary sexp.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `diary-sexp' and CDR is a plist
containing `:begin', `:end', `:value', `:post-blank' and
`:post-affiliated' keywords."
  (save-excursion
    (let ((begin (car affiliated))
	  (post-affiliated (point))
	  (value (progn (looking-at "\\(%%(.*\\)[ \t]*$")
			(org-match-string-no-properties 1)))
	  (pos-before-blank (progn (forward-line) (point)))
	  (end (progn (skip-chars-forward " \r\t\n" limit)
		      (if (eobp) (point) (line-beginning-position)))))
      (list 'diary-sexp
	    (nconc
	     (list :value value
		   :begin begin
		   :end end
		   :post-blank (count-lines pos-before-blank end)
		   :post-affiliated post-affiliated)
	     (cdr affiliated))))))

(defun org-element-diary-sexp-interpreter (diary-sexp contents)
  "Interpret DIARY-SEXP as Org syntax.
CONTENTS is nil."
  (org-element-property :value diary-sexp))


;;;; Example Block

(defun org-element--remove-indentation (s &optional n)
  "Remove maximum common indentation in string S and return it.
When optional argument N is a positive integer, remove exactly
that much characters from indentation, if possible, or return
S as-is otherwise.  Unlike to `org-remove-indentation', this
function doesn't call `untabify' on S."
  (catch 'exit
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      ;; Find maximum common indentation, if not specified.
      (setq n (or n
                  (let ((min-ind (point-max)))
		    (save-excursion
		      (while (re-search-forward "^[ \t]*\\S-" nil t)
			(let ((ind (1- (current-column))))
			  (if (zerop ind) (throw 'exit s)
			    (setq min-ind (min min-ind ind))))))
		    min-ind)))
      (if (zerop n) s
	;; Remove exactly N indentation, but give up if not possible.
	(while (not (eobp))
	  (let ((ind (progn (skip-chars-forward " \t") (current-column))))
	    (cond ((eolp) (delete-region (line-beginning-position) (point)))
		  ((< ind n) (throw 'exit s))
		  (t (org-indent-line-to (- ind n))))
	    (forward-line)))
	(buffer-string)))))

(defun org-element-example-block-parser (limit affiliated)
  "Parse an example block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `example-block' and CDR is a plist
containing `:begin', `:end', `:number-lines', `:preserve-indent',
`:retain-labels', `:use-labels', `:label-fmt', `:hiddenp',
`:switches', `:value', `:post-blank' and `:post-affiliated'
keywords."
  (let ((case-fold-search t))
    (if (not (save-excursion
	       (re-search-forward "^[ \t]*#\\+END_EXAMPLE[ \t]*$" limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((contents-end (match-beginning 0)))
	(save-excursion
	  (let* ((switches
		  (progn
		    (looking-at "^[ \t]*#\\+BEGIN_EXAMPLE\\(?: +\\(.*\\)\\)?")
		    (org-match-string-no-properties 1)))
		 ;; Switches analysis
		 (number-lines
		  (cond ((not switches) nil)
			((string-match "-n\\>" switches) 'new)
			((string-match "+n\\>" switches) 'continued)))
		 (preserve-indent
		  (or org-src-preserve-indentation
		      (and switches (string-match "-i\\>" switches))))
		 ;; Should labels be retained in (or stripped from) example
		 ;; blocks?
		 (retain-labels
		  (or (not switches)
		      (not (string-match "-r\\>" switches))
		      (and number-lines (string-match "-k\\>" switches))))
		 ;; What should code-references use - labels or
		 ;; line-numbers?
		 (use-labels
		  (or (not switches)
		      (and retain-labels
			   (not (string-match "-k\\>" switches)))))
		 (label-fmt
		  (and switches
		       (string-match "-l +\"\\([^\"\n]+\\)\"" switches)
		       (match-string 1 switches)))
		 ;; Standard block parsing.
		 (begin (car affiliated))
		 (post-affiliated (point))
		 (block-ind (progn (skip-chars-forward " \t") (current-column)))
		 (contents-begin (progn (forward-line) (point)))
		 (hidden (org-invisible-p2))
		 (value (org-element--remove-indentation
			 (org-unescape-code-in-string
			  (buffer-substring-no-properties
			   contents-begin contents-end))
			 (and preserve-indent block-ind)))
		 (pos-before-blank (progn (goto-char contents-end)
					  (forward-line)
					  (point)))
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position)))))
	    (list 'example-block
		  (nconc
		   (list :begin begin
			 :end end
			 :value value
			 :switches switches
			 :number-lines number-lines
			 :preserve-indent preserve-indent
			 :retain-labels retain-labels
			 :use-labels use-labels
			 :label-fmt label-fmt
			 :hiddenp hidden
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-example-block-interpreter (example-block contents)
  "Interpret EXAMPLE-BLOCK element as Org syntax.
CONTENTS is nil."
  (let ((switches (org-element-property :switches example-block)))
    (concat "#+BEGIN_EXAMPLE" (and switches (concat " " switches)) "\n"
	    (org-escape-code-in-string
	     (org-element-property :value example-block))
	    "#+END_EXAMPLE")))


;;;; Export Block

(defun org-element-export-block-parser (limit affiliated)
  "Parse an export block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `export-block' and CDR is a plist
containing `:begin', `:end', `:type', `:hiddenp', `:value',
`:post-blank' and `:post-affiliated' keywords.

Assume point is at export-block beginning."
  (let* ((case-fold-search t)
	 (type (progn (looking-at "[ \t]*#\\+BEGIN_\\(\\S-+\\)")
		      (upcase (org-match-string-no-properties 1)))))
    (if (not (save-excursion
	       (re-search-forward
		(format "^[ \t]*#\\+END_%s[ \t]*$" type) limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((contents-end (match-beginning 0)))
	(save-excursion
	  (let* ((begin (car affiliated))
		 (post-affiliated (point))
		 (contents-begin (progn (forward-line) (point)))
		 (hidden (org-invisible-p2))
		 (pos-before-blank (progn (goto-char contents-end)
					  (forward-line)
					  (point)))
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position))))
		 (value (buffer-substring-no-properties contents-begin
							contents-end)))
	    (list 'export-block
		  (nconc
		   (list :begin begin
			 :end end
			 :type type
			 :value value
			 :hiddenp hidden
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-export-block-interpreter (export-block contents)
  "Interpret EXPORT-BLOCK element as Org syntax.
CONTENTS is nil."
  (let ((type (org-element-property :type export-block)))
    (concat (format "#+BEGIN_%s\n" type)
	    (org-element-property :value export-block)
	    (format "#+END_%s" type))))


;;;; Fixed-width

(defun org-element-fixed-width-parser (limit affiliated)
  "Parse a fixed-width section.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `fixed-width' and CDR is a plist
containing `:begin', `:end', `:value', `:post-blank' and
`:post-affiliated' keywords.

Assume point is at the beginning of the fixed-width area."
  (save-excursion
    (let* ((begin (car affiliated))
	   (post-affiliated (point))
	   value
	   (end-area
	    (progn
	      (while (and (< (point) limit)
			  (looking-at "[ \t]*:\\( \\|$\\)"))
		;; Accumulate text without starting colons.
		(setq value
		      (concat value
			      (buffer-substring-no-properties
			       (match-end 0) (point-at-eol))
			      "\n"))
		(forward-line))
	      (point)))
	   (end (progn (skip-chars-forward " \r\t\n" limit)
		       (if (eobp) (point) (line-beginning-position)))))
      (list 'fixed-width
	    (nconc
	     (list :begin begin
		   :end end
		   :value value
		   :post-blank (count-lines end-area end)
		   :post-affiliated post-affiliated)
	     (cdr affiliated))))))

(defun org-element-fixed-width-interpreter (fixed-width contents)
  "Interpret FIXED-WIDTH element as Org syntax.
CONTENTS is nil."
  (let ((value (org-element-property :value fixed-width)))
    (and value
	 (replace-regexp-in-string
	  "^" ": "
	  (if (string-match "\n\\'" value) (substring value 0 -1) value)))))


;;;; Horizontal Rule

(defun org-element-horizontal-rule-parser (limit affiliated)
  "Parse an horizontal rule.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `horizontal-rule' and CDR is a plist
containing `:begin', `:end', `:post-blank' and `:post-affiliated'
keywords."
  (save-excursion
    (let ((begin (car affiliated))
	  (post-affiliated (point))
	  (post-hr (progn (forward-line) (point)))
	  (end (progn (skip-chars-forward " \r\t\n" limit)
		      (if (eobp) (point) (line-beginning-position)))))
      (list 'horizontal-rule
	    (nconc
	     (list :begin begin
		   :end end
		   :post-blank (count-lines post-hr end)
		   :post-affiliated post-affiliated)
	     (cdr affiliated))))))

(defun org-element-horizontal-rule-interpreter (horizontal-rule contents)
  "Interpret HORIZONTAL-RULE element as Org syntax.
CONTENTS is nil."
  "-----")


;;;; Keyword

(defun org-element-keyword-parser (limit affiliated)
  "Parse a keyword at point.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `keyword' and CDR is a plist
containing `:key', `:value', `:begin', `:end', `:post-blank' and
`:post-affiliated' keywords."
  (save-excursion
    (let ((begin (car affiliated))
	  (post-affiliated (point))
	  (key (progn (looking-at "[ \t]*#\\+\\(\\S-+*\\):")
		      (upcase (org-match-string-no-properties 1))))
	  (value (org-trim (buffer-substring-no-properties
			    (match-end 0) (point-at-eol))))
	  (pos-before-blank (progn (forward-line) (point)))
	  (end (progn (skip-chars-forward " \r\t\n" limit)
		      (if (eobp) (point) (line-beginning-position)))))
      (list 'keyword
	    (nconc
	     (list :key key
		   :value value
		   :begin begin
		   :end end
		   :post-blank (count-lines pos-before-blank end)
		   :post-affiliated post-affiliated)
	     (cdr affiliated))))))

(defun org-element-keyword-interpreter (keyword contents)
  "Interpret KEYWORD element as Org syntax.
CONTENTS is nil."
  (format "#+%s: %s"
	  (org-element-property :key keyword)
	  (org-element-property :value keyword)))


;;;; Latex Environment

(defun org-element-latex-environment-parser (limit affiliated)
  "Parse a LaTeX environment.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `latex-environment' and CDR is a plist
containing `:begin', `:end', `:value', `:post-blank' and
`:post-affiliated' keywords.

Assume point is at the beginning of the latex environment."
  (save-excursion
    (let ((case-fold-search t)
	  (code-begin (point)))
      (looking-at "[ \t]*\\\\begin{\\([A-Za-z0-9]+\\*?\\)}")
      (if (not (re-search-forward (format "^[ \t]*\\\\end{%s}[ \t]*$"
					  (regexp-quote (match-string 1)))
				  limit t))
	  ;; Incomplete latex environment: parse it as a paragraph.
	  (org-element-paragraph-parser limit affiliated)
	(let* ((code-end (progn (forward-line) (point)))
	       (begin (car affiliated))
	       (value (buffer-substring-no-properties code-begin code-end))
	       (end (progn (skip-chars-forward " \r\t\n" limit)
			   (if (eobp) (point) (line-beginning-position)))))
	  (list 'latex-environment
		(nconc
		 (list :begin begin
		       :end end
		       :value value
		       :post-blank (count-lines code-end end)
		       :post-affiliated code-begin)
		 (cdr affiliated))))))))

(defun org-element-latex-environment-interpreter (latex-environment contents)
  "Interpret LATEX-ENVIRONMENT element as Org syntax.
CONTENTS is nil."
  (org-element-property :value latex-environment))


;;;; Node Property

(defun org-element-node-property-parser (limit)
  "Parse a node-property at point.

LIMIT bounds the search.

Return a list whose CAR is `node-property' and CDR is a plist
containing `:key', `:value', `:begin', `:end' and `:post-blank'
keywords."
  (save-excursion
    (looking-at org-property-re)
    (let ((case-fold-search t)
	  (begin (point))
	  (key   (org-match-string-no-properties 2))
	  (value (org-match-string-no-properties 3))
	  (pos-before-blank (progn (forward-line) (point)))
	  (end (progn (skip-chars-forward " \r\t\n" limit)
		      (if (eobp) (point) (point-at-bol)))))
      (list 'node-property
	    (list :key key
		  :value value
		  :begin begin
		  :end end
		  :post-blank (count-lines pos-before-blank end))))))

(defun org-element-node-property-interpreter (node-property contents)
  "Interpret NODE-PROPERTY element as Org syntax.
CONTENTS is nil."
  (format org-property-format
	  (format ":%s:" (org-element-property :key node-property))
	  (org-element-property :value node-property)))


;;;; Paragraph

(defun org-element-paragraph-parser (limit affiliated)
  "Parse a paragraph.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `paragraph' and CDR is a plist
containing `:begin', `:end', `:contents-begin' and
`:contents-end', `:post-blank' and `:post-affiliated' keywords.

Assume point is at the beginning of the paragraph."
  (save-excursion
    (let* ((begin (car affiliated))
	   (contents-begin (point))
	   (before-blank
	    (let ((case-fold-search t))
	      (end-of-line)
	      (if (not (re-search-forward
			org-element-paragraph-separate limit 'm))
		  limit
		;; A matching `org-element-paragraph-separate' is not
		;; necessarily the end of the paragraph.  In
		;; particular, lines starting with # or : as a first
		;; non-space character are ambiguous.  We have to
		;; check if they are valid Org syntax (e.g., not an
		;; incomplete keyword).
		(beginning-of-line)
		(while (not
			(or
			 ;; There's no ambiguity for other symbols or
			 ;; empty lines: stop here.
			 (looking-at "[ \t]*\\(?:[^:#]\\|$\\)")
			 ;; Stop at valid fixed-width areas.
			 (looking-at "[ \t]*:\\(?: \\|$\\)")
			 ;; Stop at drawers.
			 (and (looking-at org-drawer-regexp)
			      (save-excursion
				(re-search-forward
				 "^[ \t]*:END:[ \t]*$" limit t)))
			 ;; Stop at valid comments.
			 (looking-at "[ \t]*#\\(?: \\|$\\)")
			 ;; Stop at valid dynamic blocks.
			 (and (looking-at org-dblock-start-re)
			      (save-excursion
				(re-search-forward
				 "^[ \t]*#\\+END:?[ \t]*$" limit t)))
			 ;; Stop at valid blocks.
			 (and (looking-at "[ \t]*#\\+BEGIN_\\(\\S-+\\)")
			      (save-excursion
				(re-search-forward
				 (format "^[ \t]*#\\+END_%s[ \t]*$"
					 (regexp-quote
					  (org-match-string-no-properties 1)))
				 limit t)))
			 ;; Stop at valid latex environments.
			 (and (looking-at
			       "[ \t]*\\\\begin{\\([A-Za-z0-9]+\\*?\\)}")
			      (save-excursion
				(re-search-forward
				 (format "^[ \t]*\\\\end{%s}[ \t]*$"
					 (regexp-quote
					  (org-match-string-no-properties 1)))
				 limit t)))
			 ;; Stop at valid keywords.
			 (looking-at "[ \t]*#\\+\\S-+:")
			 ;; Skip everything else.
			 (not
			  (progn
			    (end-of-line)
			    (re-search-forward org-element-paragraph-separate
					       limit 'm)))))
		  (beginning-of-line)))
	      (if (= (point) limit) limit
		(goto-char (line-beginning-position)))))
	   (contents-end (progn (skip-chars-backward " \r\t\n" contents-begin)
				(forward-line)
				(point)))
	   (end (progn (skip-chars-forward " \r\t\n" limit)
		       (if (eobp) (point) (line-beginning-position)))))
      (list 'paragraph
	    (nconc
	     (list :begin begin
		   :end end
		   :contents-begin contents-begin
		   :contents-end contents-end
		   :post-blank (count-lines before-blank end)
		   :post-affiliated contents-begin)
	     (cdr affiliated))))))

(defun org-element-paragraph-interpreter (paragraph contents)
  "Interpret PARAGRAPH element as Org syntax.
CONTENTS is the contents of the element."
  contents)


;;;; Planning

(defun org-element-planning-parser (limit)
  "Parse a planning.

LIMIT bounds the search.

Return a list whose CAR is `planning' and CDR is a plist
containing `:closed', `:deadline', `:scheduled', `:begin', `:end'
and `:post-blank' keywords."
  (save-excursion
    (let* ((case-fold-search nil)
	   (begin (point))
	   (post-blank (let ((before-blank (progn (forward-line) (point))))
			 (skip-chars-forward " \r\t\n" limit)
			 (skip-chars-backward " \t")
			 (unless (bolp) (end-of-line))
			 (count-lines before-blank (point))))
	   (end (point))
	   closed deadline scheduled)
      (goto-char begin)
      (while (re-search-forward org-keyword-time-not-clock-regexp end t)
	(goto-char (match-end 1))
	(skip-chars-forward " \t" end)
	(let ((keyword (match-string 1))
	      (time (org-element-timestamp-parser)))
	  (cond ((equal keyword org-closed-string) (setq closed time))
		((equal keyword org-deadline-string) (setq deadline time))
		(t (setq scheduled time)))))
      (list 'planning
	    (list :closed closed
		  :deadline deadline
		  :scheduled scheduled
		  :begin begin
		  :end end
		  :post-blank post-blank)))))

(defun org-element-planning-interpreter (planning contents)
  "Interpret PLANNING element as Org syntax.
CONTENTS is nil."
  (mapconcat
   'identity
   (delq nil
	 (list (let ((deadline (org-element-property :deadline planning)))
		 (when deadline
		   (concat org-deadline-string " "
			   (org-element-timestamp-interpreter deadline nil))))
	       (let ((scheduled (org-element-property :scheduled planning)))
		 (when scheduled
		   (concat org-scheduled-string " "
			   (org-element-timestamp-interpreter scheduled nil))))
	       (let ((closed (org-element-property :closed planning)))
		 (when closed
		   (concat org-closed-string " "
			   (org-element-timestamp-interpreter closed nil))))))
   " "))


;;;; Quote Section

(defun org-element-quote-section-parser (limit)
  "Parse a quote section.

LIMIT bounds the search.

Return a list whose CAR is `quote-section' and CDR is a plist
containing `:begin', `:end', `:value' and `:post-blank' keywords.

Assume point is at beginning of the section."
  (save-excursion
    (let* ((begin (point))
	   (end (progn (org-with-limited-levels (outline-next-heading))
		       (point)))
	   (pos-before-blank (progn (skip-chars-backward " \r\t\n")
				    (forward-line)
				    (point)))
	   (value (buffer-substring-no-properties begin pos-before-blank)))
      (list 'quote-section
	    (list :begin begin
		  :end end
		  :value value
		  :post-blank (count-lines pos-before-blank end))))))

(defun org-element-quote-section-interpreter (quote-section contents)
  "Interpret QUOTE-SECTION element as Org syntax.
CONTENTS is nil."
  (org-element-property :value quote-section))


;;;; Src Block

(defun org-element-src-block-parser (limit affiliated)
  "Parse a src block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `src-block' and CDR is a plist
containing `:language', `:switches', `:parameters', `:begin',
`:end', `:hiddenp', `:number-lines', `:retain-labels',
`:use-labels', `:label-fmt', `:preserve-indent', `:value',
`:post-blank' and `:post-affiliated' keywords.

Assume point is at the beginning of the block."
  (let ((case-fold-search t))
    (if (not (save-excursion (re-search-forward "^[ \t]*#\\+END_SRC[ \t]*$"
						limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((contents-end (match-beginning 0)))
	(save-excursion
	  (let* ((begin (car affiliated))
		 (post-affiliated (point))
		 ;; Get language as a string.
		 (language
		  (progn
		    (looking-at
		     (concat "^[ \t]*#\\+BEGIN_SRC"
			     "\\(?: +\\(\\S-+\\)\\)?"
			     "\\(\\(?: +\\(?:-l \".*?\"\\|[-+][A-Za-z]\\)\\)+\\)?"
			     "\\(.*\\)[ \t]*$"))
		    (org-match-string-no-properties 1)))
		 ;; Get switches.
		 (switches (org-match-string-no-properties 2))
		 ;; Get parameters.
		 (parameters (org-match-string-no-properties 3))
		 ;; Switches analysis
		 (number-lines
		  (cond ((not switches) nil)
			((string-match "-n\\>" switches) 'new)
			((string-match "+n\\>" switches) 'continued)))
		 (preserve-indent (or org-src-preserve-indentation
				      (and switches
					   (string-match "-i\\>" switches))))
		 (label-fmt
		  (and switches
		       (string-match "-l +\"\\([^\"\n]+\\)\"" switches)
		       (match-string 1 switches)))
		 ;; Should labels be retained in (or stripped from)
		 ;; src blocks?
		 (retain-labels
		  (or (not switches)
		      (not (string-match "-r\\>" switches))
		      (and number-lines (string-match "-k\\>" switches))))
		 ;; What should code-references use - labels or
		 ;; line-numbers?
		 (use-labels
		  (or (not switches)
		      (and retain-labels
			   (not (string-match "-k\\>" switches)))))
		 ;; Indentation.
		 (block-ind (progn (skip-chars-forward " \t") (current-column)))
		 ;; Get visibility status.
		 (hidden (progn (forward-line) (org-invisible-p2)))
		 ;; Retrieve code.
		 (value (org-element--remove-indentation
			 (org-unescape-code-in-string
			  (buffer-substring-no-properties
			   (point) contents-end))
			 (and preserve-indent block-ind)))
		 (pos-before-blank (progn (goto-char contents-end)
					  (forward-line)
					  (point)))
		 ;; Get position after ending blank lines.
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position)))))
	    (list 'src-block
		  (nconc
		   (list :language language
			 :switches (and (org-string-nw-p switches)
					(org-trim switches))
			 :parameters (and (org-string-nw-p parameters)
					  (org-trim parameters))
			 :begin begin
			 :end end
			 :number-lines number-lines
			 :preserve-indent preserve-indent
			 :retain-labels retain-labels
			 :use-labels use-labels
			 :label-fmt label-fmt
			 :hiddenp hidden
			 :value value
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-src-block-interpreter (src-block contents)
  "Interpret SRC-BLOCK element as Org syntax.
CONTENTS is nil."
  (let ((lang (org-element-property :language src-block))
	(switches (org-element-property :switches src-block))
	(params (org-element-property :parameters src-block))
	(value (let ((val (org-element-property :value src-block)))
		 (cond
		  ((org-element-property :preserve-indent src-block) val)
		  ((zerop org-edit-src-content-indentation) val)
		  (t
		   (let ((ind (make-string
			       org-edit-src-content-indentation 32)))
		     (replace-regexp-in-string
		      "\\(^\\)[ \t]*\\S-" ind val nil nil 1)))))))
    (concat (format "#+BEGIN_SRC%s\n"
		    (concat (and lang (concat " " lang))
			    (and switches (concat " " switches))
			    (and params (concat " " params))))
	    (org-escape-code-in-string value)
	    "#+END_SRC")))


;;;; Table

(defun org-element-table-parser (limit affiliated)
  "Parse a table at point.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `table' and CDR is a plist containing
`:begin', `:end', `:tblfm', `:type', `:contents-begin',
`:contents-end', `:value', `:post-blank' and `:post-affiliated'
keywords.

Assume point is at the beginning of the table."
  (save-excursion
    (let* ((case-fold-search t)
	   (table-begin (point))
	   (type (if (org-at-table.el-p) 'table.el 'org))
	   (begin (car affiliated))
	   (table-end
	    (if (re-search-forward org-table-any-border-regexp limit 'm)
		(goto-char (match-beginning 0))
	      (point)))
	   (tblfm (let (acc)
		    (while (looking-at "[ \t]*#\\+TBLFM: +\\(.*\\)[ \t]*$")
		      (push (org-match-string-no-properties 1) acc)
		      (forward-line))
		    acc))
	   (pos-before-blank (point))
	   (end (progn (skip-chars-forward " \r\t\n" limit)
		       (if (eobp) (point) (line-beginning-position)))))
      (list 'table
	    (nconc
	     (list :begin begin
		   :end end
		   :type type
		   :tblfm tblfm
		   ;; Only `org' tables have contents.  `table.el' tables
		   ;; use a `:value' property to store raw table as
		   ;; a string.
		   :contents-begin (and (eq type 'org) table-begin)
		   :contents-end (and (eq type 'org) table-end)
		   :value (and (eq type 'table.el)
			       (buffer-substring-no-properties
				table-begin table-end))
		   :post-blank (count-lines pos-before-blank end)
		   :post-affiliated table-begin)
	     (cdr affiliated))))))

(defun org-element-table-interpreter (table contents)
  "Interpret TABLE element as Org syntax.
CONTENTS is nil."
  (if (eq (org-element-property :type table) 'table.el)
      (org-remove-indentation (org-element-property :value table))
    (concat (with-temp-buffer (insert contents)
			      (org-table-align)
			      (buffer-string))
	    (mapconcat (lambda (fm) (concat "#+TBLFM: " fm))
		       (reverse (org-element-property :tblfm table))
		       "\n"))))


;;;; Table Row

(defun org-element-table-row-parser (limit)
  "Parse table row at point.

LIMIT bounds the search.

Return a list whose CAR is `table-row' and CDR is a plist
containing `:begin', `:end', `:contents-begin', `:contents-end',
`:type' and `:post-blank' keywords."
  (save-excursion
    (let* ((type (if (looking-at "^[ \t]*|-") 'rule 'standard))
	   (begin (point))
	   ;; A table rule has no contents.  In that case, ensure
	   ;; CONTENTS-BEGIN matches CONTENTS-END.
	   (contents-begin (and (eq type 'standard)
				(search-forward "|")
				(point)))
	   (contents-end (and (eq type 'standard)
			      (progn
				(end-of-line)
				(skip-chars-backward " \t")
				(point))))
	   (end (progn (forward-line) (point))))
      (list 'table-row
	    (list :type type
		  :begin begin
		  :end end
		  :contents-begin contents-begin
		  :contents-end contents-end
		  :post-blank 0)))))

(defun org-element-table-row-interpreter (table-row contents)
  "Interpret TABLE-ROW element as Org syntax.
CONTENTS is the contents of the table row."
  (if (eq (org-element-property :type table-row) 'rule) "|-"
    (concat "| " contents)))


;;;; Verse Block

(defun org-element-verse-block-parser (limit affiliated)
  "Parse a verse block.

LIMIT bounds the search.  AFFILIATED is a list of which CAR is
the buffer position at the beginning of the first affiliated
keyword and CDR is a plist of affiliated keywords along with
their value.

Return a list whose CAR is `verse-block' and CDR is a plist
containing `:begin', `:end', `:contents-begin', `:contents-end',
`:hiddenp', `:post-blank' and `:post-affiliated' keywords.

Assume point is at beginning of the block."
  (let ((case-fold-search t))
    (if (not (save-excursion
	       (re-search-forward "^[ \t]*#\\+END_VERSE[ \t]*$" limit t)))
	;; Incomplete block: parse it as a paragraph.
	(org-element-paragraph-parser limit affiliated)
      (let ((contents-end (match-beginning 0)))
	(save-excursion
	  (let* ((begin (car affiliated))
		 (post-affiliated (point))
		 (hidden (progn (forward-line) (org-invisible-p2)))
		 (contents-begin (point))
		 (pos-before-blank (progn (goto-char contents-end)
					  (forward-line)
					  (point)))
		 (end (progn (skip-chars-forward " \r\t\n" limit)
			     (if (eobp) (point) (line-beginning-position)))))
	    (list 'verse-block
		  (nconc
		   (list :begin begin
			 :end end
			 :contents-begin contents-begin
			 :contents-end contents-end
			 :hiddenp hidden
			 :post-blank (count-lines pos-before-blank end)
			 :post-affiliated post-affiliated)
		   (cdr affiliated)))))))))

(defun org-element-verse-block-interpreter (verse-block contents)
  "Interpret VERSE-BLOCK element as Org syntax.
CONTENTS is verse block contents."
  (format "#+BEGIN_VERSE\n%s#+END_VERSE" contents))



;;; Objects
;;
;; Unlike to elements, interstices can be found between objects.
;; That's why, along with the parser, successor functions are provided
;; for each object.  Some objects share the same successor (e.g.,
;; `code' and `verbatim' objects).
;;
;; A successor must accept a single argument bounding the search.  It
;; will return either a cons cell whose CAR is the object's type, as
;; a symbol, and CDR the position of its next occurrence, or nil.
;;
;; Successors follow the naming convention:
;; org-element-NAME-successor, where NAME is the name of the
;; successor, as defined in `org-element-all-successors'.
;;
;; Some object types (e.g., `italic') are recursive.  Restrictions on
;; object types they can contain will be specified in
;; `org-element-object-restrictions'.
;;
;; Adding a new type of object is simple.  Implement a successor,
;; a parser, and an interpreter for it, all following the naming
;; convention.  Register type in `org-element-all-objects' and
;; successor in `org-element-all-successors'.  Maybe tweak
;; restrictions about it, and that's it.


;;;; Bold

(defun org-element-bold-parser ()
  "Parse bold object at point.

Return a list whose CAR is `bold' and CDR is a plist with
`:begin', `:end', `:contents-begin' and `:contents-end' and
`:post-blank' keywords.

Assume point is at the first star marker."
  (save-excursion
    (unless (bolp) (backward-char 1))
    (looking-at org-emph-re)
    (let ((begin (match-beginning 2))
	  (contents-begin (match-beginning 4))
	  (contents-end (match-end 4))
	  (post-blank (progn (goto-char (match-end 2))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'bold
	    (list :begin begin
		  :end end
		  :contents-begin contents-begin
		  :contents-end contents-end
		  :post-blank post-blank)))))

(defun org-element-bold-interpreter (bold contents)
  "Interpret BOLD object as Org syntax.
CONTENTS is the contents of the object."
  (format "*%s*" contents))

(defun org-element-text-markup-successor ()
  "Search for the next text-markup object.

Return value is a cons cell whose CAR is a symbol among `bold',
`italic', `underline', `strike-through', `code' and `verbatim'
and CDR is beginning position."
  (save-excursion
    (unless (bolp) (backward-char))
    (when (re-search-forward org-emph-re nil t)
      (let ((marker (match-string 3)))
	(cons (cond
	       ((equal marker "*") 'bold)
	       ((equal marker "/") 'italic)
	       ((equal marker "_") 'underline)
	       ((equal marker "+") 'strike-through)
	       ((equal marker "~") 'code)
	       ((equal marker "=") 'verbatim)
	       (t (error "Unknown marker at %d" (match-beginning 3))))
	      (match-beginning 2))))))


;;;; Code

(defun org-element-code-parser ()
  "Parse code object at point.

Return a list whose CAR is `code' and CDR is a plist with
`:value', `:begin', `:end' and `:post-blank' keywords.

Assume point is at the first tilde marker."
  (save-excursion
    (unless (bolp) (backward-char 1))
    (looking-at org-emph-re)
    (let ((begin (match-beginning 2))
	  (value (org-match-string-no-properties 4))
	  (post-blank (progn (goto-char (match-end 2))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'code
	    (list :value value
		  :begin begin
		  :end end
		  :post-blank post-blank)))))

(defun org-element-code-interpreter (code contents)
  "Interpret CODE object as Org syntax.
CONTENTS is nil."
  (format "~%s~" (org-element-property :value code)))


;;;; Entity

(defun org-element-entity-parser ()
  "Parse entity at point.

Return a list whose CAR is `entity' and CDR a plist with
`:begin', `:end', `:latex', `:latex-math-p', `:html', `:latin1',
`:utf-8', `:ascii', `:use-brackets-p' and `:post-blank' as
keywords.

Assume point is at the beginning of the entity."
  (save-excursion
    (looking-at "\\\\\\(there4\\|sup[123]\\|frac[13][24]\\|[a-zA-Z]+\\)\\($\\|{}\\|[^[:alpha:]]\\)")
    (let* ((value (org-entity-get (match-string 1)))
	   (begin (match-beginning 0))
	   (bracketsp (string= (match-string 2) "{}"))
	   (post-blank (progn (goto-char (match-end 1))
			      (when bracketsp (forward-char 2))
			      (skip-chars-forward " \t")))
	   (end (point)))
      (list 'entity
	    (list :name (car value)
		  :latex (nth 1 value)
		  :latex-math-p (nth 2 value)
		  :html (nth 3 value)
		  :ascii (nth 4 value)
		  :latin1 (nth 5 value)
		  :utf-8 (nth 6 value)
		  :begin begin
		  :end end
		  :use-brackets-p bracketsp
		  :post-blank post-blank)))))

(defun org-element-entity-interpreter (entity contents)
  "Interpret ENTITY object as Org syntax.
CONTENTS is nil."
  (concat "\\"
	  (org-element-property :name entity)
	  (when (org-element-property :use-brackets-p entity) "{}")))

(defun org-element-latex-or-entity-successor ()
  "Search for the next latex-fragment or entity object.

Return value is a cons cell whose CAR is `entity' or
`latex-fragment' and CDR is beginning position."
  (save-excursion
    (unless (bolp) (backward-char))
    (let ((matchers (cdr org-latex-regexps))
	  ;; ENTITY-RE matches both LaTeX commands and Org entities.
	  (entity-re
	   "\\\\\\(there4\\|sup[123]\\|frac[13][24]\\|[a-zA-Z]+\\)\\($\\|{}\\|[^[:alpha:]]\\)"))
      (when (re-search-forward
	     (concat (mapconcat #'cadr matchers "\\|") "\\|" entity-re) nil t)
	(goto-char (match-beginning 0))
	(if (looking-at entity-re)
	    ;; Determine if it's a real entity or a LaTeX command.
	    (cons (if (org-entity-get (match-string 1)) 'entity 'latex-fragment)
		  (match-beginning 0))
	  ;; No entity nor command: point is at a LaTeX fragment.
	  ;; Determine its type to get the correct beginning position.
	  (cons 'latex-fragment
		(catch 'return
		  (dolist (e matchers)
		    (when (looking-at (nth 1 e))
		      (throw 'return (match-beginning (nth 2 e)))))
		  (point))))))))


;;;; Export Snippet

(defun org-element-export-snippet-parser ()
  "Parse export snippet at point.

Return a list whose CAR is `export-snippet' and CDR a plist with
`:begin', `:end', `:back-end', `:value' and `:post-blank' as
keywords.

Assume point is at the beginning of the snippet."
  (save-excursion
    (re-search-forward "@@\\([-A-Za-z0-9]+\\):" nil t)
    (let* ((begin (match-beginning 0))
	   (back-end (org-match-string-no-properties 1))
	   (value (buffer-substring-no-properties
		   (point)
		   (progn (re-search-forward "@@" nil t) (match-beginning 0))))
	   (post-blank (skip-chars-forward " \t"))
	   (end (point)))
      (list 'export-snippet
	    (list :back-end back-end
		  :value value
		  :begin begin
		  :end end
		  :post-blank post-blank)))))

(defun org-element-export-snippet-interpreter (export-snippet contents)
  "Interpret EXPORT-SNIPPET object as Org syntax.
CONTENTS is nil."
  (format "@@%s:%s@@"
	  (org-element-property :back-end export-snippet)
	  (org-element-property :value export-snippet)))

(defun org-element-export-snippet-successor ()
  "Search for the next export-snippet object.

Return value is a cons cell whose CAR is `export-snippet' and CDR
its beginning position."
  (save-excursion
    (let (beg)
      (when (and (re-search-forward "@@[-A-Za-z0-9]+:" nil t)
		 (setq beg (match-beginning 0))
		 (search-forward "@@" nil t))
	(cons 'export-snippet beg)))))


;;;; Footnote Reference

(defun org-element-footnote-reference-parser ()
  "Parse footnote reference at point.

Return a list whose CAR is `footnote-reference' and CDR a plist
with `:label', `:type', `:inline-definition', `:begin', `:end'
and `:post-blank' as keywords."
  (save-excursion
    (looking-at org-footnote-re)
    (let* ((begin (point))
	   (label (or (org-match-string-no-properties 2)
		      (org-match-string-no-properties 3)
		      (and (match-string 1)
			   (concat "fn:" (org-match-string-no-properties 1)))))
	   (type (if (or (not label) (match-string 1)) 'inline 'standard))
	   (inner-begin (match-end 0))
	   (inner-end
	    (let ((count 1))
	      (forward-char)
	      (while (and (> count 0) (re-search-forward "[][]" nil t))
		(if (equal (match-string 0) "[") (incf count) (decf count)))
	      (1- (point))))
	   (post-blank (progn (goto-char (1+ inner-end))
			      (skip-chars-forward " \t")))
	   (end (point))
	   (footnote-reference
	    (list 'footnote-reference
		  (list :label label
			:type type
			:begin begin
			:end end
			:post-blank post-blank))))
      (org-element-put-property
       footnote-reference :inline-definition
       (and (eq type 'inline)
	    (org-element-parse-secondary-string
	     (buffer-substring inner-begin inner-end)
	     (org-element-restriction 'footnote-reference)
	     footnote-reference))))))

(defun org-element-footnote-reference-interpreter (footnote-reference contents)
  "Interpret FOOTNOTE-REFERENCE object as Org syntax.
CONTENTS is nil."
  (let ((label (or (org-element-property :label footnote-reference) "fn:"))
	(def
	 (let ((inline-def
		(org-element-property :inline-definition footnote-reference)))
	   (if (not inline-def) ""
	     (concat ":" (org-element-interpret-data inline-def))))))
    (format "[%s]" (concat label def))))

(defun org-element-footnote-reference-successor ()
  "Search for the next footnote-reference object.

Return value is a cons cell whose CAR is `footnote-reference' and
CDR is beginning position."
  (save-excursion
    (catch 'exit
      (while (re-search-forward org-footnote-re nil t)
	(save-excursion
	  (let ((beg (match-beginning 0))
		(count 1))
	    (backward-char)
	    (while (re-search-forward "[][]" nil t)
	      (if (equal (match-string 0) "[") (incf count) (decf count))
	      (when (zerop count)
		(throw 'exit (cons 'footnote-reference beg))))))))))


;;;; Inline Babel Call

(defun org-element-inline-babel-call-parser ()
  "Parse inline babel call at point.

Return a list whose CAR is `inline-babel-call' and CDR a plist
with `:begin', `:end', `:info' and `:post-blank' as keywords.

Assume point is at the beginning of the babel call."
  (save-excursion
    (unless (bolp) (backward-char))
    (looking-at org-babel-inline-lob-one-liner-regexp)
    (let ((info (save-match-data (org-babel-lob-get-info)))
	  (begin (match-end 1))
	  (post-blank (progn (goto-char (match-end 0))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'inline-babel-call
	    (list :begin begin
		  :end end
		  :info info
		  :post-blank post-blank)))))

(defun org-element-inline-babel-call-interpreter (inline-babel-call contents)
  "Interpret INLINE-BABEL-CALL object as Org syntax.
CONTENTS is nil."
  (let* ((babel-info (org-element-property :info inline-babel-call))
	 (main-source (car babel-info))
	 (post-options (nth 1 babel-info)))
    (concat "call_"
	    (if (string-match "\\[\\(\\[.*?\\]\\)\\]" main-source)
		;; Remove redundant square brackets.
		(replace-match
		 (match-string 1 main-source) nil nil main-source)
	      main-source)
	    (and post-options (format "[%s]" post-options)))))

(defun org-element-inline-babel-call-successor ()
  "Search for the next inline-babel-call object.

Return value is a cons cell whose CAR is `inline-babel-call' and
CDR is beginning position."
  (save-excursion
    (when (re-search-forward org-babel-inline-lob-one-liner-regexp nil t)
      (cons 'inline-babel-call (match-end 1)))))


;;;; Inline Src Block

(defun org-element-inline-src-block-parser ()
  "Parse inline source block at point.

Return a list whose CAR is `inline-src-block' and CDR a plist
with `:begin', `:end', `:language', `:value', `:parameters' and
`:post-blank' as keywords.

Assume point is at the beginning of the inline src block."
  (save-excursion
    (unless (bolp) (backward-char))
    (looking-at org-babel-inline-src-block-regexp)
    (let ((begin (match-beginning 1))
	  (language (org-match-string-no-properties 2))
	  (parameters (org-match-string-no-properties 4))
	  (value (org-match-string-no-properties 5))
	  (post-blank (progn (goto-char (match-end 0))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'inline-src-block
	    (list :language language
		  :value value
		  :parameters parameters
		  :begin begin
		  :end end
		  :post-blank post-blank)))))

(defun org-element-inline-src-block-interpreter (inline-src-block contents)
  "Interpret INLINE-SRC-BLOCK object as Org syntax.
CONTENTS is nil."
  (let ((language (org-element-property :language inline-src-block))
	(arguments (org-element-property :parameters inline-src-block))
	(body (org-element-property :value inline-src-block)))
    (format "src_%s%s{%s}"
	    language
	    (if arguments (format "[%s]" arguments) "")
	    body)))

(defun org-element-inline-src-block-successor ()
  "Search for the next inline-babel-call element.

Return value is a cons cell whose CAR is `inline-babel-call' and
CDR is beginning position."
  (save-excursion
    (unless (bolp) (backward-char))
    (when (re-search-forward org-babel-inline-src-block-regexp nil t)
      (cons 'inline-src-block (match-beginning 1)))))

;;;; Italic

(defun org-element-italic-parser ()
  "Parse italic object at point.

Return a list whose CAR is `italic' and CDR is a plist with
`:begin', `:end', `:contents-begin' and `:contents-end' and
`:post-blank' keywords.

Assume point is at the first slash marker."
  (save-excursion
    (unless (bolp) (backward-char 1))
    (looking-at org-emph-re)
    (let ((begin (match-beginning 2))
	  (contents-begin (match-beginning 4))
	  (contents-end (match-end 4))
	  (post-blank (progn (goto-char (match-end 2))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'italic
	    (list :begin begin
		  :end end
		  :contents-begin contents-begin
		  :contents-end contents-end
		  :post-blank post-blank)))))

(defun org-element-italic-interpreter (italic contents)
  "Interpret ITALIC object as Org syntax.
CONTENTS is the contents of the object."
  (format "/%s/" contents))


;;;; Latex Fragment

(defun org-element-latex-fragment-parser ()
  "Parse LaTeX fragment at point.

Return a list whose CAR is `latex-fragment' and CDR a plist with
`:value', `:begin', `:end', and `:post-blank' as keywords.

Assume point is at the beginning of the LaTeX fragment."
  (save-excursion
    (let* ((begin (point))
	   (substring-match
	    (catch 'exit
	      (dolist (e (cdr org-latex-regexps))
		(let ((latex-regexp (nth 1 e)))
		  (when (or (looking-at latex-regexp)
			    (and (not (bobp))
				 (save-excursion
				   (backward-char)
				   (looking-at latex-regexp))))
		    (throw 'exit (nth 2 e)))))
	      ;; None found: it's a macro.
	      (looking-at "\\\\[a-zA-Z]+\\*?\\(\\(\\[[^][\n{}]*\\]\\)\\|\\({[^{}\n]*}\\)\\)*")
	      0))
	   (value (org-match-string-no-properties substring-match))
	   (post-blank (progn (goto-char (match-end substring-match))
			      (skip-chars-forward " \t")))
	   (end (point)))
      (list 'latex-fragment
	    (list :value value
		  :begin begin
		  :end end
		  :post-blank post-blank)))))

(defun org-element-latex-fragment-interpreter (latex-fragment contents)
  "Interpret LATEX-FRAGMENT object as Org syntax.
CONTENTS is nil."
  (org-element-property :value latex-fragment))

;;;; Line Break

(defun org-element-line-break-parser ()
  "Parse line break at point.

Return a list whose CAR is `line-break', and CDR a plist with
`:begin', `:end' and `:post-blank' keywords.

Assume point is at the beginning of the line break."
  (list 'line-break
	(list :begin (point)
	      :end (progn (forward-line) (point))
	      :post-blank 0)))

(defun org-element-line-break-interpreter (line-break contents)
  "Interpret LINE-BREAK object as Org syntax.
CONTENTS is nil."
  "\\\\\n")

(defun org-element-line-break-successor ()
  "Search for the next line-break object.

Return value is a cons cell whose CAR is `line-break' and CDR is
beginning position."
  (save-excursion
    (let ((beg (and (re-search-forward "[^\\\\]\\(\\\\\\\\\\)[ \t]*$" nil t)
		    (goto-char (match-beginning 1)))))
      ;; A line break can only happen on a non-empty line.
      (when (and beg (re-search-backward "\\S-" (point-at-bol) t))
	(cons 'line-break beg)))))


;;;; Link

(defun org-element-link-parser ()
  "Parse link at point.

Return a list whose CAR is `link' and CDR a plist with `:type',
`:path', `:raw-link', `:application', `:search-option', `:begin',
`:end', `:contents-begin', `:contents-end' and `:post-blank' as
keywords.

Assume point is at the beginning of the link."
  (save-excursion
    (let ((begin (point))
	  end contents-begin contents-end link-end post-blank path type
	  raw-link link search-option application)
      (cond
       ;; Type 1: Text targeted from a radio target.
       ((and org-target-link-regexp (looking-at org-target-link-regexp))
	(setq type "radio"
	      link-end (match-end 0)
	      path (org-match-string-no-properties 0)
	      contents-begin (match-beginning 0)
	      contents-end (match-end 0)))
       ;; Type 2: Standard link, i.e. [[http://orgmode.org][homepage]]
       ((looking-at org-bracket-link-regexp)
	(setq contents-begin (match-beginning 3)
	      contents-end (match-end 3)
	      link-end (match-end 0)
	      ;; RAW-LINK is the original link.  Expand any
	      ;; abbreviation in it.
	      raw-link (org-translate-link
			(org-link-expand-abbrev
			 (org-match-string-no-properties 1))))
	;; Determine TYPE of link and set PATH accordingly.
	(cond
	 ;; File type.
	 ((or (file-name-absolute-p raw-link)
	      (string-match "^\\.\\.?/" raw-link))
	  (setq type "file" path raw-link))
	 ;; Explicit type (http, irc, bbdb...).  See `org-link-types'.
	 ((string-match org-link-re-with-space3 raw-link)
	  (setq type (match-string 1 raw-link) path (match-string 2 raw-link)))
	 ;; Id type: PATH is the id.
	 ((string-match "^id:\\([-a-f0-9]+\\)" raw-link)
	  (setq type "id" path (match-string 1 raw-link)))
	 ;; Code-ref type: PATH is the name of the reference.
	 ((string-match "^(\\(.*\\))$" raw-link)
	  (setq type "coderef" path (match-string 1 raw-link)))
	 ;; Custom-id type: PATH is the name of the custom id.
	 ((= (aref raw-link 0) ?#)
	  (setq type "custom-id" path (substring raw-link 1)))
	 ;; Fuzzy type: Internal link either matches a target, an
	 ;; headline name or nothing.  PATH is the target or
	 ;; headline's name.
	 (t (setq type "fuzzy" path raw-link))))
       ;; Type 3: Plain link, e.g., http://orgmode.org
       ((looking-at org-plain-link-re)
	(setq raw-link (org-match-string-no-properties 0)
	      type (org-match-string-no-properties 1)
	      link-end (match-end 0)
	      path (org-match-string-no-properties 2)))
       ;; Type 4: Angular link, e.g., <http://orgmode.org>
       ((looking-at org-angle-link-re)
	(setq raw-link (buffer-substring-no-properties
			(match-beginning 1) (match-end 2))
	      type (org-match-string-no-properties 1)
	      link-end (match-end 0)
	      path (org-match-string-no-properties 2))))
      ;; In any case, deduce end point after trailing white space from
      ;; LINK-END variable.
      (setq post-blank (progn (goto-char link-end) (skip-chars-forward " \t"))
	    end (point))
      ;; Special "file" type link processing.
      (when (member type org-element-link-type-is-file)
	;; Extract opening application and search option.
	(cond ((string-match "^file\\+\\(.*\\)$" type)
	       (setq application (match-string 1 type)))
	      ((not (string-match "^file" type))
	       (setq application type)))
	(when (string-match "::\\(.*\\)\\'" path)
	  (setq search-option (match-string 1 path)
		path (replace-match "" nil nil path)))
	;; Normalize URI.
	(when (and (not (org-string-match-p "\\`//" path))
		   (file-name-absolute-p path))
	  (setq path (concat "//" (expand-file-name path))))
	;; Make sure TYPE always reports "file".
	(setq type "file"))
      (list 'link
	    (list :type type
		  :path path
		  :raw-link (or raw-link path)
		  :application application
		  :search-option search-option
		  :begin begin
		  :end end
		  :contents-begin contents-begin
		  :contents-end contents-end
		  :post-blank post-blank)))))

(defun org-element-link-interpreter (link contents)
  "Interpret LINK object as Org syntax.
CONTENTS is the contents of the object, or nil."
  (let ((type (org-element-property :type link))
	(raw-link (org-element-property :raw-link link)))
    (if (string= type "radio") raw-link
      (format "[[%s]%s]"
	      raw-link
	      (if contents (format "[%s]" contents) "")))))

(defun org-element-link-successor ()
  "Search for the next link object.

Return value is a cons cell whose CAR is `link' and CDR is
beginning position."
  (save-excursion
    (let ((link-regexp
	   (if (not org-target-link-regexp) org-any-link-re
	     (concat org-any-link-re "\\|" org-target-link-regexp))))
      (when (re-search-forward link-regexp nil t)
	(cons 'link (match-beginning 0))))))

(defun org-element-plain-link-successor ()
  "Search for the next plain link object.

Return value is a cons cell whose CAR is `link' and CDR is
beginning position."
  (and (save-excursion (re-search-forward org-plain-link-re nil t))
       (cons 'link (match-beginning 0))))


;;;; Macro

(defun org-element-macro-parser ()
  "Parse macro at point.

Return a list whose CAR is `macro' and CDR a plist with `:key',
`:args', `:begin', `:end', `:value' and `:post-blank' as
keywords.

Assume point is at the macro."
  (save-excursion
    (looking-at "{{{\\([a-zA-Z][-a-zA-Z0-9_]*\\)\\(([ \t\n]*\\([^\000]*?\\))\\)?}}}")
    (let ((begin (point))
	  (key (downcase (org-match-string-no-properties 1)))
	  (value (org-match-string-no-properties 0))
	  (post-blank (progn (goto-char (match-end 0))
			     (skip-chars-forward " \t")))
	  (end (point))
	  (args (let ((args (org-match-string-no-properties 3)))
		  (when args
		    ;; Do not use `org-split-string' since empty
		    ;; strings are meaningful here.
		    (split-string
		     (replace-regexp-in-string
		      "\\(\\\\*\\)\\(,\\)"
		      (lambda (str)
			(let ((len (length (match-string 1 str))))
			  (concat (make-string (/ len 2) ?\\)
				  (if (zerop (mod len 2)) "\000" ","))))
		      args nil t)
		     "\000")))))
      (list 'macro
	    (list :key key
		  :value value
		  :args args
		  :begin begin
		  :end end
		  :post-blank post-blank)))))

(defun org-element-macro-interpreter (macro contents)
  "Interpret MACRO object as Org syntax.
CONTENTS is nil."
  (org-element-property :value macro))

(defun org-element-macro-successor ()
  "Search for the next macro object.

Return value is cons cell whose CAR is `macro' and CDR is
beginning position."
  (save-excursion
    (when (re-search-forward
	   "{{{\\([a-zA-Z][-a-zA-Z0-9_]*\\)\\(([ \t\n]*\\([^\000]*?\\))\\)?}}}"
	   nil t)
      (cons 'macro (match-beginning 0)))))


;;;; Radio-target

(defun org-element-radio-target-parser ()
  "Parse radio target at point.

Return a list whose CAR is `radio-target' and CDR a plist with
`:begin', `:end', `:contents-begin', `:contents-end', `:value'
and `:post-blank' as keywords.

Assume point is at the radio target."
  (save-excursion
    (looking-at org-radio-target-regexp)
    (let ((begin (point))
	  (contents-begin (match-beginning 1))
	  (contents-end (match-end 1))
	  (value (org-match-string-no-properties 1))
	  (post-blank (progn (goto-char (match-end 0))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'radio-target
	    (list :begin begin
		  :end end
		  :contents-begin contents-begin
		  :contents-end contents-end
		  :post-blank post-blank
		  :value value)))))

(defun org-element-radio-target-interpreter (target contents)
  "Interpret TARGET object as Org syntax.
CONTENTS is the contents of the object."
  (concat "<<<" contents ">>>"))

(defun org-element-radio-target-successor ()
  "Search for the next radio-target object.

Return value is a cons cell whose CAR is `radio-target' and CDR
is beginning position."
  (save-excursion
    (when (re-search-forward org-radio-target-regexp nil t)
      (cons 'radio-target (match-beginning 0)))))


;;;; Statistics Cookie

(defun org-element-statistics-cookie-parser ()
  "Parse statistics cookie at point.

Return a list whose CAR is `statistics-cookie', and CDR a plist
with `:begin', `:end', `:value' and `:post-blank' keywords.

Assume point is at the beginning of the statistics-cookie."
  (save-excursion
    (looking-at "\\[[0-9]*\\(%\\|/[0-9]*\\)\\]")
    (let* ((begin (point))
	   (value (buffer-substring-no-properties
		   (match-beginning 0) (match-end 0)))
	   (post-blank (progn (goto-char (match-end 0))
			      (skip-chars-forward " \t")))
	   (end (point)))
      (list 'statistics-cookie
	    (list :begin begin
		  :end end
		  :value value
		  :post-blank post-blank)))))

(defun org-element-statistics-cookie-interpreter (statistics-cookie contents)
  "Interpret STATISTICS-COOKIE object as Org syntax.
CONTENTS is nil."
  (org-element-property :value statistics-cookie))

(defun org-element-statistics-cookie-successor ()
  "Search for the next statistics cookie object.

Return value is a cons cell whose CAR is `statistics-cookie' and
CDR is beginning position."
  (save-excursion
    (when (re-search-forward "\\[[0-9]*\\(%\\|/[0-9]*\\)\\]" nil t)
      (cons 'statistics-cookie (match-beginning 0)))))


;;;; Strike-Through

(defun org-element-strike-through-parser ()
  "Parse strike-through object at point.

Return a list whose CAR is `strike-through' and CDR is a plist
with `:begin', `:end', `:contents-begin' and `:contents-end' and
`:post-blank' keywords.

Assume point is at the first plus sign marker."
  (save-excursion
    (unless (bolp) (backward-char 1))
    (looking-at org-emph-re)
    (let ((begin (match-beginning 2))
	  (contents-begin (match-beginning 4))
	  (contents-end (match-end 4))
	  (post-blank (progn (goto-char (match-end 2))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'strike-through
	    (list :begin begin
		  :end end
		  :contents-begin contents-begin
		  :contents-end contents-end
		  :post-blank post-blank)))))

(defun org-element-strike-through-interpreter (strike-through contents)
  "Interpret STRIKE-THROUGH object as Org syntax.
CONTENTS is the contents of the object."
  (format "+%s+" contents))


;;;; Subscript

(defun org-element-subscript-parser ()
  "Parse subscript at point.

Return a list whose CAR is `subscript' and CDR a plist with
`:begin', `:end', `:contents-begin', `:contents-end',
`:use-brackets-p' and `:post-blank' as keywords.

Assume point is at the underscore."
  (save-excursion
    (unless (bolp) (backward-char))
    (let ((bracketsp (if (looking-at org-match-substring-with-braces-regexp)
			 t
		       (not (looking-at org-match-substring-regexp))))
	  (begin (match-beginning 2))
	  (contents-begin (or (match-beginning 5)
			      (match-beginning 3)))
	  (contents-end (or (match-end 5) (match-end 3)))
	  (post-blank (progn (goto-char (match-end 0))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'subscript
	    (list :begin begin
		  :end end
		  :use-brackets-p bracketsp
		  :contents-begin contents-begin
		  :contents-end contents-end
		  :post-blank post-blank)))))

(defun org-element-subscript-interpreter (subscript contents)
  "Interpret SUBSCRIPT object as Org syntax.
CONTENTS is the contents of the object."
  (format
   (if (org-element-property :use-brackets-p subscript) "_{%s}" "_%s")
   contents))

(defun org-element-sub/superscript-successor ()
  "Search for the next sub/superscript object.

Return value is a cons cell whose CAR is either `subscript' or
`superscript' and CDR is beginning position."
  (save-excursion
    (unless (bolp) (backward-char))
    (when (re-search-forward org-match-substring-regexp nil t)
      (cons (if (string= (match-string 2) "_") 'subscript 'superscript)
	    (match-beginning 2)))))


;;;; Superscript

(defun org-element-superscript-parser ()
  "Parse superscript at point.

Return a list whose CAR is `superscript' and CDR a plist with
`:begin', `:end', `:contents-begin', `:contents-end',
`:use-brackets-p' and `:post-blank' as keywords.

Assume point is at the caret."
  (save-excursion
    (unless (bolp) (backward-char))
    (let ((bracketsp (if (looking-at org-match-substring-with-braces-regexp) t
		       (not (looking-at org-match-substring-regexp))))
	  (begin (match-beginning 2))
	  (contents-begin (or (match-beginning 5)
			      (match-beginning 3)))
	  (contents-end (or (match-end 5) (match-end 3)))
	  (post-blank (progn (goto-char (match-end 0))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'superscript
	    (list :begin begin
		  :end end
		  :use-brackets-p bracketsp
		  :contents-begin contents-begin
		  :contents-end contents-end
		  :post-blank post-blank)))))

(defun org-element-superscript-interpreter (superscript contents)
  "Interpret SUPERSCRIPT object as Org syntax.
CONTENTS is the contents of the object."
  (format
   (if (org-element-property :use-brackets-p superscript) "^{%s}" "^%s")
   contents))


;;;; Table Cell

(defun org-element-table-cell-parser ()
  "Parse table cell at point.

Return a list whose CAR is `table-cell' and CDR is a plist
containing `:begin', `:end', `:contents-begin', `:contents-end'
and `:post-blank' keywords."
  (looking-at "[ \t]*\\(.*?\\)[ \t]*\\(?:|\\|$\\)")
  (let* ((begin (match-beginning 0))
	 (end (match-end 0))
	 (contents-begin (match-beginning 1))
	 (contents-end (match-end 1)))
    (list 'table-cell
	  (list :begin begin
		:end end
		:contents-begin contents-begin
		:contents-end contents-end
		:post-blank 0))))

(defun org-element-table-cell-interpreter (table-cell contents)
  "Interpret TABLE-CELL element as Org syntax.
CONTENTS is the contents of the cell, or nil."
  (concat  " " contents " |"))

(defun org-element-table-cell-successor ()
  "Search for the next table-cell object.

Return value is a cons cell whose CAR is `table-cell' and CDR is
beginning position."
  (when (looking-at "[ \t]*.*?[ \t]*\\(|\\|$\\)") (cons 'table-cell (point))))


;;;; Target

(defun org-element-target-parser ()
  "Parse target at point.

Return a list whose CAR is `target' and CDR a plist with
`:begin', `:end', `:value' and `:post-blank' as keywords.

Assume point is at the target."
  (save-excursion
    (looking-at org-target-regexp)
    (let ((begin (point))
	  (value (org-match-string-no-properties 1))
	  (post-blank (progn (goto-char (match-end 0))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'target
	    (list :begin begin
		  :end end
		  :value value
		  :post-blank post-blank)))))

(defun org-element-target-interpreter (target contents)
  "Interpret TARGET object as Org syntax.
CONTENTS is nil."
  (format "<<%s>>" (org-element-property :value target)))

(defun org-element-target-successor ()
  "Search for the next target object.

Return value is a cons cell whose CAR is `target' and CDR is
beginning position."
  (save-excursion
    (when (re-search-forward org-target-regexp nil t)
      (cons 'target (match-beginning 0)))))


;;;; Timestamp

(defun org-element-timestamp-parser ()
  "Parse time stamp at point.

Return a list whose CAR is `timestamp', and CDR a plist with
`:type', `:raw-value', `:year-start', `:month-start',
`:day-start', `:hour-start', `:minute-start', `:year-end',
`:month-end', `:day-end', `:hour-end', `:minute-end',
`:repeater-type', `:repeater-value', `:repeater-unit',
`:warning-type', `:warning-value', `:warning-unit', `:begin',
`:end', `:value' and `:post-blank' keywords.

Assume point is at the beginning of the timestamp."
  (save-excursion
    (let* ((begin (point))
	   (activep (eq (char-after) ?<))
	   (raw-value
	    (progn
	      (looking-at "\\([<[]\\(%%\\)?.*?\\)[]>]\\(?:--\\([<[].*?[]>]\\)\\)?")
	      (match-string-no-properties 0)))
	   (date-start (match-string-no-properties 1))
	   (date-end (match-string 3))
	   (diaryp (match-beginning 2))
	   (post-blank (progn (goto-char (match-end 0))
			      (skip-chars-forward " \t")))
	   (end (point))
	   (time-range
	    (and (not diaryp)
		 (string-match
		  "[012]?[0-9]:[0-5][0-9]\\(-\\([012]?[0-9]\\):\\([0-5][0-9]\\)\\)"
		  date-start)
		 (cons (string-to-number (match-string 2 date-start))
		       (string-to-number (match-string 3 date-start)))))
	   (type (cond (diaryp 'diary)
		       ((and activep (or date-end time-range)) 'active-range)
		       (activep 'active)
		       ((or date-end time-range) 'inactive-range)
		       (t 'inactive)))
	   (repeater-props
	    (and (not diaryp)
		 (string-match "\\([.+]?\\+\\)\\([0-9]+\\)\\([hdwmy]\\)"
			       raw-value)
		 (list
		  :repeater-type
		  (let ((type (match-string 1 raw-value)))
		    (cond ((equal "++" type) 'catch-up)
			  ((equal ".+" type) 'restart)
			  (t 'cumulate)))
		  :repeater-value (string-to-number (match-string 2 raw-value))
		  :repeater-unit
		  (case (string-to-char (match-string 3 raw-value))
		    (?h 'hour) (?d 'day) (?w 'week) (?m 'month) (t 'year)))))
	   (warning-props
	    (and (not diaryp)
		 (string-match "\\(-\\)?-\\([0-9]+\\)\\([hdwmy]\\)" raw-value)
		 (list
		  :warning-type (if (match-string 1 raw-value) 'first 'all)
		  :warning-value (string-to-number (match-string 2 raw-value))
		  :warning-unit
		  (case (string-to-char (match-string 3 raw-value))
		    (?h 'hour) (?d 'day) (?w 'week) (?m 'month) (t 'year)))))
	   year-start month-start day-start hour-start minute-start year-end
	   month-end day-end hour-end minute-end)
      ;; Parse date-start.
      (unless diaryp
	(let ((date (org-parse-time-string date-start t)))
	  (setq year-start (nth 5 date)
		month-start (nth 4 date)
		day-start (nth 3 date)
		hour-start (nth 2 date)
		minute-start (nth 1 date))))
      ;; Compute date-end.  It can be provided directly in time-stamp,
      ;; or extracted from time range.  Otherwise, it defaults to the
      ;; same values as date-start.
      (unless diaryp
	(let ((date (and date-end (org-parse-time-string date-end t))))
	  (setq year-end (or (nth 5 date) year-start)
		month-end (or (nth 4 date) month-start)
		day-end (or (nth 3 date) day-start)
		hour-end (or (nth 2 date) (car time-range) hour-start)
		minute-end (or (nth 1 date) (cdr time-range) minute-start))))
      (list 'timestamp
	    (nconc (list :type type
			 :raw-value raw-value
			 :year-start year-start
			 :month-start month-start
			 :day-start day-start
			 :hour-start hour-start
			 :minute-start minute-start
			 :year-end year-end
			 :month-end month-end
			 :day-end day-end
			 :hour-end hour-end
			 :minute-end minute-end
			 :begin begin
			 :end end
			 :post-blank post-blank)
		   repeater-props
		   warning-props)))))

(defun org-element-timestamp-interpreter (timestamp contents)
  "Interpret TIMESTAMP object as Org syntax.
CONTENTS is nil."
  ;; Use `:raw-value' if specified.
  (or (org-element-property :raw-value timestamp)
      ;; Otherwise, build timestamp string.
      (let* ((repeat-string
	      (concat
	       (case (org-element-property :repeater-type timestamp)
		 (cumulate "+") (catch-up "++") (restart ".+"))
	       (let ((val (org-element-property :repeater-value timestamp)))
		 (and val (number-to-string val)))
	       (case (org-element-property :repeater-unit timestamp)
		 (hour "h") (day "d") (week "w") (month "m") (year "y"))))
	     (warning-string
	      (concat
	       (case (org-element-property :warning-type timestamp)
		 (first "--")
		 (all "-"))
	       (let ((val (org-element-property :warning-value timestamp)))
		 (and val (number-to-string val)))
	       (case (org-element-property :warning-unit timestamp)
		 (hour "h") (day "d") (week "w") (month "m") (year "y"))))
	     (build-ts-string
	      ;; Build an Org timestamp string from TIME.  ACTIVEP is
	      ;; non-nil when time stamp is active.  If WITH-TIME-P is
	      ;; non-nil, add a time part.  HOUR-END and MINUTE-END
	      ;; specify a time range in the timestamp.  REPEAT-STRING
	      ;; is the repeater string, if any.
	      (lambda (time activep &optional with-time-p hour-end minute-end)
		(let ((ts (format-time-string
			   (funcall (if with-time-p 'cdr 'car)
				    org-time-stamp-formats)
			   time)))
		  (when (and hour-end minute-end)
		    (string-match "[012]?[0-9]:[0-5][0-9]" ts)
		    (setq ts
			  (replace-match
			   (format "\\&-%02d:%02d" hour-end minute-end)
			   nil nil ts)))
		  (unless activep (setq ts (format "[%s]" (substring ts 1 -1))))
		  (dolist (s (list repeat-string warning-string))
		    (when (org-string-nw-p s)
		      (setq ts (concat (substring ts 0 -1)
				       " "
				       s
				       (substring ts -1)))))
		  ;; Return value.
		  ts)))
	     (type (org-element-property :type timestamp)))
	(case type
	  ((active inactive)
	   (let* ((minute-start (org-element-property :minute-start timestamp))
		  (minute-end (org-element-property :minute-end timestamp))
		  (hour-start (org-element-property :hour-start timestamp))
		  (hour-end (org-element-property :hour-end timestamp))
		  (time-range-p (and hour-start hour-end minute-start minute-end
				     (or (/= hour-start hour-end)
					 (/= minute-start minute-end)))))
	     (funcall
	      build-ts-string
	      (encode-time 0
			   (or minute-start 0)
			   (or hour-start 0)
			   (org-element-property :day-start timestamp)
			   (org-element-property :month-start timestamp)
			   (org-element-property :year-start timestamp))
	      (eq type 'active)
	      (and hour-start minute-start)
	      (and time-range-p hour-end)
	      (and time-range-p minute-end))))
	  ((active-range inactive-range)
	   (let ((minute-start (org-element-property :minute-start timestamp))
		 (minute-end (org-element-property :minute-end timestamp))
		 (hour-start (org-element-property :hour-start timestamp))
		 (hour-end (org-element-property :hour-end timestamp)))
	     (concat
	      (funcall
	       build-ts-string (encode-time
				0
				(or minute-start 0)
				(or hour-start 0)
				(org-element-property :day-start timestamp)
				(org-element-property :month-start timestamp)
				(org-element-property :year-start timestamp))
	       (eq type 'active-range)
	       (and hour-start minute-start))
	      "--"
	      (funcall build-ts-string
		       (encode-time 0
				    (or minute-end 0)
				    (or hour-end 0)
				    (org-element-property :day-end timestamp)
				    (org-element-property :month-end timestamp)
				    (org-element-property :year-end timestamp))
		       (eq type 'active-range)
		       (and hour-end minute-end)))))))))

(defun org-element-timestamp-successor ()
  "Search for the next timestamp object.

Return value is a cons cell whose CAR is `timestamp' and CDR is
beginning position."
  (save-excursion
    (when (re-search-forward
	   (concat org-ts-regexp-both
		   "\\|"
		   "\\(?:<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+[dwmy]>\\)"
		   "\\|"
		   "\\(?:<%%\\(?:([^>\n]+)\\)>\\)")
	   nil t)
      (cons 'timestamp (match-beginning 0)))))


;;;; Underline

(defun org-element-underline-parser ()
  "Parse underline object at point.

Return a list whose CAR is `underline' and CDR is a plist with
`:begin', `:end', `:contents-begin' and `:contents-end' and
`:post-blank' keywords.

Assume point is at the first underscore marker."
  (save-excursion
    (unless (bolp) (backward-char 1))
    (looking-at org-emph-re)
    (let ((begin (match-beginning 2))
	  (contents-begin (match-beginning 4))
	  (contents-end (match-end 4))
	  (post-blank (progn (goto-char (match-end 2))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'underline
	    (list :begin begin
		  :end end
		  :contents-begin contents-begin
		  :contents-end contents-end
		  :post-blank post-blank)))))

(defun org-element-underline-interpreter (underline contents)
  "Interpret UNDERLINE object as Org syntax.
CONTENTS is the contents of the object."
  (format "_%s_" contents))


;;;; Verbatim

(defun org-element-verbatim-parser ()
  "Parse verbatim object at point.

Return a list whose CAR is `verbatim' and CDR is a plist with
`:value', `:begin', `:end' and `:post-blank' keywords.

Assume point is at the first equal sign marker."
  (save-excursion
    (unless (bolp) (backward-char 1))
    (looking-at org-emph-re)
    (let ((begin (match-beginning 2))
	  (value (org-match-string-no-properties 4))
	  (post-blank (progn (goto-char (match-end 2))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'verbatim
	    (list :value value
		  :begin begin
		  :end end
		  :post-blank post-blank)))))

(defun org-element-verbatim-interpreter (verbatim contents)
  "Interpret VERBATIM object as Org syntax.
CONTENTS is nil."
  (format "=%s=" (org-element-property :value verbatim)))



;;; Parsing Element Starting At Point
;;
;; `org-element--current-element' is the core function of this section.
;; It returns the Lisp representation of the element starting at
;; point.
;;
;; `org-element--current-element' makes use of special modes.  They
;; are activated for fixed element chaining (e.g., `plain-list' >
;; `item') or fixed conditional element chaining (e.g., `headline' >
;; `section').  Special modes are: `first-section', `item',
;; `node-property', `quote-section', `section' and `table-row'.

(defun org-element--current-element
  (limit &optional granularity special structure)
  "Parse the element starting at point.

Return value is a list like (TYPE PROPS) where TYPE is the type
of the element and PROPS a plist of properties associated to the
element.

Possible types are defined in `org-element-all-elements'.

LIMIT bounds the search.

Optional argument GRANULARITY determines the depth of the
recursion.  Allowed values are `headline', `greater-element',
`element', `object' or nil.  When it is broader than `object' (or
nil), secondary values will not be parsed, since they only
contain objects.

Optional argument SPECIAL, when non-nil, can be either
`first-section', `item', `node-property', `quote-section',
`section', and `table-row'.

If STRUCTURE isn't provided but SPECIAL is set to `item', it will
be computed.

This function assumes point is always at the beginning of the
element it has to parse."
  (save-excursion
    (let ((case-fold-search t)
	  ;; Determine if parsing depth allows for secondary strings
	  ;; parsing.  It only applies to elements referenced in
	  ;; `org-element-secondary-value-alist'.
	  (raw-secondary-p (and granularity (not (eq granularity 'object)))))
      (cond
       ;; Item.
       ((eq special 'item)
	(org-element-item-parser limit structure raw-secondary-p))
       ;; Table Row.
       ((eq special 'table-row) (org-element-table-row-parser limit))
       ;; Node Property.
       ((eq special 'node-property) (org-element-node-property-parser limit))
       ;; Headline.
       ((org-with-limited-levels (org-at-heading-p))
        (org-element-headline-parser limit raw-secondary-p))
       ;; Sections (must be checked after headline).
       ((eq special 'section) (org-element-section-parser limit))
       ((eq special 'quote-section) (org-element-quote-section-parser limit))
       ((eq special 'first-section)
	(org-element-section-parser
	 (or (save-excursion (org-with-limited-levels (outline-next-heading)))
	     limit)))
       ;; When not at bol, point is at the beginning of an item or
       ;; a footnote definition: next item is always a paragraph.
       ((not (bolp)) (org-element-paragraph-parser limit (list (point))))
       ;; Planning and Clock.
       ((looking-at org-planning-or-clock-line-re)
	(if (equal (match-string 1) org-clock-string)
	    (org-element-clock-parser limit)
	  (org-element-planning-parser limit)))
       ;; Inlinetask.
       ((org-at-heading-p)
	(org-element-inlinetask-parser limit raw-secondary-p))
       ;; From there, elements can have affiliated keywords.
       (t (let ((affiliated (org-element--collect-affiliated-keywords limit)))
	    (cond
	     ;; Jumping over affiliated keywords put point off-limits.
	     ;; Parse them as regular keywords.
	     ((and (cdr affiliated) (>= (point) limit))
	      (goto-char (car affiliated))
	      (org-element-keyword-parser limit nil))
	     ;; LaTeX Environment.
	     ((looking-at
	       "[ \t]*\\\\begin{[A-Za-z0-9*]+}\\(\\[.*?\\]\\|{.*?}\\)*[ \t]*$")
	      (org-element-latex-environment-parser limit affiliated))
	     ;; Drawer and Property Drawer.
	     ((looking-at org-drawer-regexp)
	      (if (equal (match-string 1) "PROPERTIES")
		  (org-element-property-drawer-parser limit affiliated)
		(org-element-drawer-parser limit affiliated)))
	     ;; Fixed Width
	     ((looking-at "[ \t]*:\\( \\|$\\)")
	      (org-element-fixed-width-parser limit affiliated))
	     ;; Inline Comments, Blocks, Babel Calls, Dynamic Blocks and
	     ;; Keywords.
	     ((looking-at "[ \t]*#")
	      (goto-char (match-end 0))
	      (cond ((looking-at "\\(?: \\|$\\)")
		     (beginning-of-line)
		     (org-element-comment-parser limit affiliated))
		    ((looking-at "\\+BEGIN_\\(\\S-+\\)")
		     (beginning-of-line)
		     (let ((parser (assoc (upcase (match-string 1))
					  org-element-block-name-alist)))
		       (if parser (funcall (cdr parser) limit affiliated)
			 (org-element-special-block-parser limit affiliated))))
		    ((looking-at "\\+CALL:")
		     (beginning-of-line)
		     (org-element-babel-call-parser limit affiliated))
		    ((looking-at "\\+BEGIN:? ")
		     (beginning-of-line)
		     (org-element-dynamic-block-parser limit affiliated))
		    ((looking-at "\\+\\S-+:")
		     (beginning-of-line)
		     (org-element-keyword-parser limit affiliated))
		    (t
		     (beginning-of-line)
		     (org-element-paragraph-parser limit affiliated))))
	     ;; Footnote Definition.
	     ((looking-at org-footnote-definition-re)
	      (org-element-footnote-definition-parser limit affiliated))
	     ;; Horizontal Rule.
	     ((looking-at "[ \t]*-\\{5,\\}[ \t]*$")
	      (org-element-horizontal-rule-parser limit affiliated))
	     ;; Diary Sexp.
	     ((looking-at "%%(")
	      (org-element-diary-sexp-parser limit affiliated))
	     ;; Table.
	     ((org-at-table-p t) (org-element-table-parser limit affiliated))
	     ;; List.
	     ((looking-at (org-item-re))
	      (org-element-plain-list-parser
	       limit affiliated
	       (or structure (org-element--list-struct limit))))
	     ;; Default element: Paragraph.
	     (t (org-element-paragraph-parser limit affiliated)))))))))


;; Most elements can have affiliated keywords.  When looking for an
;; element beginning, we want to move before them, as they belong to
;; that element, and, in the meantime, collect information they give
;; into appropriate properties.  Hence the following function.

(defun org-element--collect-affiliated-keywords (limit)
  "Collect affiliated keywords from point down to LIMIT.

Return a list whose CAR is the position at the first of them and
CDR a plist of keywords and values and move point to the
beginning of the first line after them.

As a special case, if element doesn't start at the beginning of
the line (e.g., a paragraph starting an item), CAR is current
position of point and CDR is nil."
  (if (not (bolp)) (list (point))
    (let ((case-fold-search t)
	  (origin (point))
	  ;; RESTRICT is the list of objects allowed in parsed
	  ;; keywords value.
	  (restrict (org-element-restriction 'keyword))
	  output)
      (while (and (< (point) limit) (looking-at org-element--affiliated-re))
	(let* ((raw-kwd (upcase (match-string 1)))
	       ;; Apply translation to RAW-KWD.  From there, KWD is
	       ;; the official keyword.
	       (kwd (or (cdr (assoc raw-kwd
				    org-element-keyword-translation-alist))
			raw-kwd))
	       ;; Find main value for any keyword.
	       (value
		(save-match-data
		  (org-trim
		   (buffer-substring-no-properties
		    (match-end 0) (point-at-eol)))))
	       ;; PARSEDP is non-nil when keyword should have its
	       ;; value parsed.
	       (parsedp (member kwd org-element-parsed-keywords))
	       ;; If KWD is a dual keyword, find its secondary
	       ;; value.  Maybe parse it.
	       (dualp (member kwd org-element-dual-keywords))
	       (dual-value
		(and dualp
		     (let ((sec (org-match-string-no-properties 2)))
		       (if (or (not sec) (not parsedp)) sec
			 (org-element-parse-secondary-string sec restrict)))))
	       ;; Attribute a property name to KWD.
	       (kwd-sym (and kwd (intern (concat ":" (downcase kwd))))))
	  ;; Now set final shape for VALUE.
	  (when parsedp
	    (setq value (org-element-parse-secondary-string value restrict)))
	  (when dualp
	    (setq value (and (or value dual-value) (cons value dual-value))))
	  (when (or (member kwd org-element-multiple-keywords)
		    ;; Attributes can always appear on multiple lines.
		    (string-match "^ATTR_" kwd))
	    (setq value (cons value (plist-get output kwd-sym))))
	  ;; Eventually store the new value in OUTPUT.
	  (setq output (plist-put output kwd-sym value))
	  ;; Move to next keyword.
	  (forward-line)))
      ;; If affiliated keywords are orphaned: move back to first one.
      ;; They will be parsed as a paragraph.
      (when (looking-at "[ \t]*$") (goto-char origin) (setq output nil))
      ;; Return value.
      (cons origin output))))



;;; The Org Parser
;;
;; The two major functions here are `org-element-parse-buffer', which
;; parses Org syntax inside the current buffer, taking into account
;; region, narrowing, or even visibility if specified, and
;; `org-element-parse-secondary-string', which parses objects within
;; a given string.
;;
;; The (almost) almighty `org-element-map' allows to apply a function
;; on elements or objects matching some type, and accumulate the
;; resulting values.  In an export situation, it also skips unneeded
;; parts of the parse tree.

(defun org-element-parse-buffer (&optional granularity visible-only)
  "Recursively parse the buffer and return structure.
If narrowing is in effect, only parse the visible part of the
buffer.

Optional argument GRANULARITY determines the depth of the
recursion.  It can be set to the following symbols:

`headline'          Only parse headlines.
`greater-element'   Don't recurse into greater elements excepted
		    headlines and sections.  Thus, elements
		    parsed are the top-level ones.
`element'           Parse everything but objects and plain text.
`object'            Parse the complete buffer (default).

When VISIBLE-ONLY is non-nil, don't parse contents of hidden
elements.

An element or an objects is represented as a list with the
pattern (TYPE PROPERTIES CONTENTS), where :

  TYPE is a symbol describing the element or object.  See
  `org-element-all-elements' and `org-element-all-objects' for an
  exhaustive list of such symbols.  One can retrieve it with
  `org-element-type' function.

  PROPERTIES is the list of attributes attached to the element or
  object, as a plist.  Although most of them are specific to the
  element or object type, all types share `:begin', `:end',
  `:post-blank' and `:parent' properties, which respectively
  refer to buffer position where the element or object starts,
  ends, the number of white spaces or blank lines after it, and
  the element or object containing it.  Properties values can be
  obtained by using `org-element-property' function.

  CONTENTS is a list of elements, objects or raw strings
  contained in the current element or object, when applicable.
  One can access them with `org-element-contents' function.

The Org buffer has `org-data' as type and nil as properties.
`org-element-map' function can be used to find specific elements
or objects within the parse tree.

This function assumes that current major mode is `org-mode'."
  (save-excursion
    (goto-char (point-min))
    (org-skip-whitespace)
    (org-element--parse-elements
     (point-at-bol) (point-max)
     ;; Start in `first-section' mode so text before the first
     ;; headline belongs to a section.
     'first-section nil granularity visible-only (list 'org-data nil))))

(defun org-element-parse-secondary-string (string restriction &optional parent)
  "Recursively parse objects in STRING and return structure.

RESTRICTION is a symbol limiting the object types that will be
looked after.

Optional argument PARENT, when non-nil, is the element or object
containing the secondary string.  It is used to set correctly
`:parent' property within the string."
  ;; Copy buffer-local variables listed in
  ;; `org-element-object-variables' into temporary buffer.  This is
  ;; required since object parsing is dependent on these variables.
  (let ((pairs (delq nil (mapcar (lambda (var)
				   (when (boundp var)
				     (cons var (symbol-value var))))
				 org-element-object-variables))))
    (with-temp-buffer
      (mapc (lambda (pair) (org-set-local (car pair) (cdr pair))) pairs)
      (insert string)
      (let ((secondary (org-element--parse-objects
			(point-min) (point-max) nil restriction)))
	(when parent
	  (mapc (lambda (obj) (org-element-put-property obj :parent parent))
		secondary))
	secondary))))

(defun org-element-map
  (data types fun &optional info first-match no-recursion with-affiliated)
  "Map a function on selected elements or objects.

DATA is a parse tree, an element, an object, a string, or a list
of such constructs.  TYPES is a symbol or list of symbols of
elements or objects types (see `org-element-all-elements' and
`org-element-all-objects' for a complete list of types).  FUN is
the function called on the matching element or object.  It has to
accept one argument: the element or object itself.

When optional argument INFO is non-nil, it should be a plist
holding export options.  In that case, parts of the parse tree
not exportable according to that property list will be skipped.

When optional argument FIRST-MATCH is non-nil, stop at the first
match for which FUN doesn't return nil, and return that value.

Optional argument NO-RECURSION is a symbol or a list of symbols
representing elements or objects types.  `org-element-map' won't
enter any recursive element or object whose type belongs to that
list.  Though, FUN can still be applied on them.

When optional argument WITH-AFFILIATED is non-nil, FUN will also
apply to matching objects within parsed affiliated keywords (see
`org-element-parsed-keywords').

Nil values returned from FUN do not appear in the results.


Examples:
---------

Assuming TREE is a variable containing an Org buffer parse tree,
the following example will return a flat list of all `src-block'
and `example-block' elements in it:

  \(org-element-map tree '(example-block src-block) 'identity)

The following snippet will find the first headline with a level
of 1 and a \"phone\" tag, and will return its beginning position:

  \(org-element-map tree 'headline
   \(lambda (hl)
     \(and (= (org-element-property :level hl) 1)
          \(member \"phone\" (org-element-property :tags hl))
          \(org-element-property :begin hl)))
   nil t)

The next example will return a flat list of all `plain-list' type
elements in TREE that are not a sub-list themselves:

  \(org-element-map tree 'plain-list 'identity nil nil 'plain-list)

Eventually, this example will return a flat list of all `bold'
type objects containing a `latex-snippet' type object, even
looking into captions:

  \(org-element-map tree 'bold
   \(lambda (b)
     \(and (org-element-map b 'latex-snippet 'identity nil t) b))
   nil nil nil t)"
  ;; Ensure TYPES and NO-RECURSION are a list, even of one element.
  (unless (listp types) (setq types (list types)))
  (unless (listp no-recursion) (setq no-recursion (list no-recursion)))
  ;; Recursion depth is determined by --CATEGORY.
  (let* ((--category
	  (catch 'found
	    (let ((category 'greater-elements))
	      (mapc (lambda (type)
		      (cond ((or (memq type org-element-all-objects)
				 (eq type 'plain-text))
			     ;; If one object is found, the function
			     ;; has to recurse into every object.
			     (throw 'found 'objects))
			    ((not (memq type org-element-greater-elements))
			     ;; If one regular element is found, the
			     ;; function has to recurse, at least,
			     ;; into every element it encounters.
			     (and (not (eq category 'elements))
				  (setq category 'elements)))))
		    types)
	      category)))
	 ;; Compute properties for affiliated keywords if necessary.
	 (--affiliated-alist
	  (and with-affiliated
	       (mapcar (lambda (kwd)
			 (cons kwd (intern (concat ":" (downcase kwd)))))
		       org-element-affiliated-keywords)))
	 --acc
	 --walk-tree
	 (--walk-tree
	  (function
	   (lambda (--data)
	     ;; Recursively walk DATA.  INFO, if non-nil, is a plist
	     ;; holding contextual information.
	     (let ((--type (org-element-type --data)))
	       (cond
		((not --data))
		;; Ignored element in an export context.
		((and info (memq --data (plist-get info :ignore-list))))
		;; List of elements or objects.
		((not --type) (mapc --walk-tree --data))
		;; Unconditionally enter parse trees.
		((eq --type 'org-data)
		 (mapc --walk-tree (org-element-contents --data)))
		(t
		 ;; Check if TYPE is matching among TYPES.  If so,
		 ;; apply FUN to --DATA and accumulate return value
		 ;; into --ACC (or exit if FIRST-MATCH is non-nil).
		 (when (memq --type types)
		   (let ((result (funcall fun --data)))
		     (cond ((not result))
			   (first-match (throw '--map-first-match result))
			   (t (push result --acc)))))
		 ;; If --DATA has a secondary string that can contain
		 ;; objects with their type among TYPES, look into it.
		 (when (and (eq --category 'objects) (not (stringp --data)))
		   (let ((sec-prop
			  (assq --type org-element-secondary-value-alist)))
		     (when sec-prop
		       (funcall --walk-tree
				(org-element-property (cdr sec-prop) --data)))))
		 ;; If --DATA has any affiliated keywords and
		 ;; WITH-AFFILIATED is non-nil, look for objects in
		 ;; them.
		 (when (and with-affiliated
			    (eq --category 'objects)
			    (memq --type org-element-all-elements))
		   (mapc (lambda (kwd-pair)
			   (let ((kwd (car kwd-pair))
				 (value (org-element-property
					 (cdr kwd-pair) --data)))
			     ;; Pay attention to the type of value.
			     ;; Preserve order for multiple keywords.
			     (cond
			      ((not value))
			      ((and (member kwd org-element-multiple-keywords)
				    (member kwd org-element-dual-keywords))
			       (mapc (lambda (line)
				       (funcall --walk-tree (cdr line))
				       (funcall --walk-tree (car line)))
				     (reverse value)))
			      ((member kwd org-element-multiple-keywords)
			       (mapc (lambda (line) (funcall --walk-tree line))
				     (reverse value)))
			      ((member kwd org-element-dual-keywords)
			       (funcall --walk-tree (cdr value))
			       (funcall --walk-tree (car value)))
			      (t (funcall --walk-tree value)))))
			 --affiliated-alist))
		 ;; Determine if a recursion into --DATA is possible.
		 (cond
		  ;; --TYPE is explicitly removed from recursion.
		  ((memq --type no-recursion))
		  ;; --DATA has no contents.
		  ((not (org-element-contents --data)))
		  ;; Looking for greater elements but --DATA is simply
		  ;; an element or an object.
		  ((and (eq --category 'greater-elements)
			(not (memq --type org-element-greater-elements))))
		  ;; Looking for elements but --DATA is an object.
		  ((and (eq --category 'elements)
			(memq --type org-element-all-objects)))
		  ;; In any other case, map contents.
		  (t (mapc --walk-tree (org-element-contents --data)))))))))))
    (catch '--map-first-match
      (funcall --walk-tree data)
      ;; Return value in a proper order.
      (nreverse --acc))))
(put 'org-element-map 'lisp-indent-function 2)

;; The following functions are internal parts of the parser.
;;
;; The first one, `org-element--parse-elements' acts at the element's
;; level.
;;
;; The second one, `org-element--parse-objects' applies on all objects
;; of a paragraph or a secondary string.  It uses
;; `org-element--get-next-object-candidates' to optimize the search of
;; the next object in the buffer.
;;
;; More precisely, that function looks for every allowed object type
;; first.  Then, it discards failed searches, keeps further matches,
;; and searches again types matched behind point, for subsequent
;; calls.  Thus, searching for a given type fails only once, and every
;; object is searched only once at top level (but sometimes more for
;; nested types).

(defun org-element--parse-elements
  (beg end special structure granularity visible-only acc)
  "Parse elements between BEG and END positions.

SPECIAL prioritize some elements over the others.  It can be set
to `first-section', `quote-section', `section' `item' or
`table-row'.

When value is `item', STRUCTURE will be used as the current list
structure.

GRANULARITY determines the depth of the recursion.  See
`org-element-parse-buffer' for more information.

When VISIBLE-ONLY is non-nil, don't parse contents of hidden
elements.

Elements are accumulated into ACC."
  (save-excursion
    (goto-char beg)
    ;; Visible only: skip invisible parts at the beginning of the
    ;; element.
    (when (and visible-only (org-invisible-p2))
      (goto-char (min (1+ (org-find-visible)) end)))
    ;; When parsing only headlines, skip any text before first one.
    (when (and (eq granularity 'headline) (not (org-at-heading-p)))
      (org-with-limited-levels (outline-next-heading)))
    ;; Main loop start.
    (while (< (point) end)
      ;; Find current element's type and parse it accordingly to
      ;; its category.
      (let* ((element (org-element--current-element
		       end granularity special structure))
	     (type (org-element-type element))
	     (cbeg (org-element-property :contents-begin element)))
	(goto-char (org-element-property :end element))
	;; Visible only: skip invisible parts between siblings.
	(when (and visible-only (org-invisible-p2))
	  (goto-char (min (1+ (org-find-visible)) end)))
	;; Fill ELEMENT contents by side-effect.
	(cond
	 ;; If element has no contents, don't modify it.
	 ((not cbeg))
	 ;; Greater element: parse it between `contents-begin' and
	 ;; `contents-end'.  Make sure GRANULARITY allows the
	 ;; recursion, or ELEMENT is a headline, in which case going
	 ;; inside is mandatory, in order to get sub-level headings.
	 ((and (memq type org-element-greater-elements)
	       (or (memq granularity '(element object nil))
		   (and (eq granularity 'greater-element)
			(eq type 'section))
		   (eq type 'headline)))
	  (org-element--parse-elements
	   cbeg (org-element-property :contents-end element)
	   ;; Possibly switch to a special mode.
	   (case type
	     (headline
	      (if (org-element-property :quotedp element) 'quote-section
		'section))
	     (plain-list 'item)
	     (property-drawer 'node-property)
	     (table 'table-row))
	   (and (memq type '(item plain-list))
		(org-element-property :structure element))
	   granularity visible-only element))
	 ;; ELEMENT has contents.  Parse objects inside, if
	 ;; GRANULARITY allows it.
	 ((memq granularity '(object nil))
	  (org-element--parse-objects
	   cbeg (org-element-property :contents-end element) element
	   (org-element-restriction type))))
	(org-element-adopt-elements acc element)))
    ;; Return result.
    acc))

(defun org-element--parse-objects (beg end acc restriction)
  "Parse objects between BEG and END and return recursive structure.

Objects are accumulated in ACC.

RESTRICTION is a list of object successors which are allowed in
the current object."
  (let ((candidates 'initial))
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	(while (and (not (eobp))
		    (setq candidates
			  (org-element--get-next-object-candidates
			   restriction candidates)))
	  (let ((next-object
		 (let ((pos (apply 'min (mapcar 'cdr candidates))))
		   (save-excursion
		     (goto-char pos)
		     (funcall (intern (format "org-element-%s-parser"
					      (car (rassq pos candidates)))))))))
	    ;; 1. Text before any object.  Untabify it.
	    (let ((obj-beg (org-element-property :begin next-object)))
	      (unless (= (point) obj-beg)
		(setq acc
		      (org-element-adopt-elements
		       acc
		       (replace-regexp-in-string
			"\t" (make-string tab-width ? )
			(buffer-substring-no-properties (point) obj-beg))))))
	    ;; 2. Object...
	    (let ((obj-end (org-element-property :end next-object))
		  (cont-beg (org-element-property :contents-begin next-object)))
	      ;; Fill contents of NEXT-OBJECT by side-effect, if it has
	      ;; a recursive type.
	      (when (and cont-beg
			 (memq (car next-object) org-element-recursive-objects))
		(org-element--parse-objects
		 cont-beg (org-element-property :contents-end next-object)
		 next-object (org-element-restriction next-object)))
	      (setq acc (org-element-adopt-elements acc next-object))
	      (goto-char obj-end))))
	;; 3. Text after last object.  Untabify it.
	(unless (eobp)
	  (setq acc
		(org-element-adopt-elements
		 acc
		 (replace-regexp-in-string
		  "\t" (make-string tab-width ? )
		  (buffer-substring-no-properties (point) end)))))
	;; Result.
	acc))))

(defun org-element--get-next-object-candidates (restriction objects)
  "Return an alist of candidates for the next object.

RESTRICTION is a list of object types, as symbols.  Only
candidates with such types are looked after.

OBJECTS is the previous candidates alist.  If it is set to
`initial', no search has been done before, and all symbols in
RESTRICTION should be looked after.

Return value is an alist whose CAR is the object type and CDR its
beginning position."
  (delq
   nil
   (if (eq objects 'initial)
       ;; When searching for the first time, look for every successor
       ;; allowed in RESTRICTION.
       (mapcar
	(lambda (res)
	  (funcall (intern (format "org-element-%s-successor" res))))
	restriction)
     ;; Focus on objects returned during last search.  Keep those
     ;; still after point.  Search again objects before it.
     (mapcar
      (lambda (obj)
	(if (>= (cdr obj) (point)) obj
	  (let* ((type (car obj))
		 (succ (or (cdr (assq type org-element-object-successor-alist))
			   type)))
	    (and succ
		 (funcall (intern (format "org-element-%s-successor" succ)))))))
      objects))))



;;; Towards A Bijective Process
;;
;; The parse tree obtained with `org-element-parse-buffer' is really
;; a snapshot of the corresponding Org buffer.  Therefore, it can be
;; interpreted and expanded into a string with canonical Org syntax.
;; Hence `org-element-interpret-data'.
;;
;; The function relies internally on
;; `org-element--interpret-affiliated-keywords'.

;;;###autoload
(defun org-element-interpret-data (data &optional parent)
  "Interpret DATA as Org syntax.

DATA is a parse tree, an element, an object or a secondary string
to interpret.

Optional argument PARENT is used for recursive calls.  It contains
the element or object containing data, or nil.

Return Org syntax as a string."
  (let* ((type (org-element-type data))
	 (results
	  (cond
	   ;; Secondary string.
	   ((not type)
	    (mapconcat
	     (lambda (obj) (org-element-interpret-data obj parent))
	     data ""))
	   ;; Full Org document.
	   ((eq type 'org-data)
	    (mapconcat
	     (lambda (obj) (org-element-interpret-data obj parent))
	     (org-element-contents data) ""))
	   ;; Plain text: return it.
	   ((stringp data) data)
	   ;; Element/Object without contents.
	   ((not (org-element-contents data))
	    (funcall (intern (format "org-element-%s-interpreter" type))
		     data nil))
	   ;; Element/Object with contents.
	   (t
	    (let* ((greaterp (memq type org-element-greater-elements))
		   (objectp (and (not greaterp)
				 (memq type org-element-recursive-objects)))
		   (contents
		    (mapconcat
		     (lambda (obj) (org-element-interpret-data obj data))
		     (org-element-contents
		      (if (or greaterp objectp) data
			;; Elements directly containing objects must
			;; have their indentation normalized first.
			(org-element-normalize-contents
			 data
			 ;; When normalizing first paragraph of an
			 ;; item or a footnote-definition, ignore
			 ;; first line's indentation.
			 (and (eq type 'paragraph)
			      (equal data (car (org-element-contents parent)))
			      (memq (org-element-type parent)
				    '(footnote-definition item))))))
		     "")))
	      (funcall (intern (format "org-element-%s-interpreter" type))
		       data
		       (if greaterp (org-element-normalize-contents contents)
			 contents)))))))
    (if (memq type '(org-data plain-text nil)) results
      ;; Build white spaces.  If no `:post-blank' property is
      ;; specified, assume its value is 0.
      (let ((post-blank (or (org-element-property :post-blank data) 0)))
	(if (memq type org-element-all-objects)
	    (concat results (make-string post-blank 32))
	  (concat
	   (org-element--interpret-affiliated-keywords data)
	   (org-element-normalize-string results)
	   (make-string post-blank 10)))))))

(defun org-element--interpret-affiliated-keywords (element)
  "Return ELEMENT's affiliated keywords as Org syntax.
If there is no affiliated keyword, return the empty string."
  (let ((keyword-to-org
	 (function
	  (lambda (key value)
	    (let (dual)
	      (when (member key org-element-dual-keywords)
		(setq dual (cdr value) value (car value)))
	      (concat "#+" key
		      (and dual
			   (format "[%s]" (org-element-interpret-data dual)))
		      ": "
		      (if (member key org-element-parsed-keywords)
			  (org-element-interpret-data value)
			value)
		      "\n"))))))
    (mapconcat
     (lambda (prop)
       (let ((value (org-element-property prop element))
	     (keyword (upcase (substring (symbol-name prop) 1))))
	 (when value
	   (if (or (member keyword org-element-multiple-keywords)
		   ;; All attribute keywords can have multiple lines.
		   (string-match "^ATTR_" keyword))
	       (mapconcat (lambda (line) (funcall keyword-to-org keyword line))
			  (reverse value)
			  "")
	     (funcall keyword-to-org keyword value)))))
     ;; List all ELEMENT's properties matching an attribute line or an
     ;; affiliated keyword, but ignore translated keywords since they
     ;; cannot belong to the property list.
     (loop for prop in (nth 1 element) by 'cddr
	   when (let ((keyword (upcase (substring (symbol-name prop) 1))))
		  (or (string-match "^ATTR_" keyword)
		      (and
		       (member keyword org-element-affiliated-keywords)
		       (not (assoc keyword
				   org-element-keyword-translation-alist)))))
	   collect prop)
     "")))

;; Because interpretation of the parse tree must return the same
;; number of blank lines between elements and the same number of white
;; space after objects, some special care must be given to white
;; spaces.
;;
;; The first function, `org-element-normalize-string', ensures any
;; string different from the empty string will end with a single
;; newline character.
;;
;; The second function, `org-element-normalize-contents', removes
;; global indentation from the contents of the current element.

(defun org-element-normalize-string (s)
  "Ensure string S ends with a single newline character.

If S isn't a string return it unchanged.  If S is the empty
string, return it.  Otherwise, return a new string with a single
newline character at its end."
  (cond
   ((not (stringp s)) s)
   ((string= "" s) "")
   (t (and (string-match "\\(\n[ \t]*\\)*\\'" s)
	   (replace-match "\n" nil nil s)))))

(defun org-element-normalize-contents (element &optional ignore-first)
  "Normalize plain text in ELEMENT's contents.

ELEMENT must only contain plain text and objects.

If optional argument IGNORE-FIRST is non-nil, ignore first line's
indentation to compute maximal common indentation.

Return the normalized element that is element with global
indentation removed from its contents.  The function assumes that
indentation is not done with TAB characters."
  (let* ((min-ind most-positive-fixnum)
	 find-min-ind			; For byte-compiler.
	 (find-min-ind
	  (function
	   ;; Return minimal common indentation within BLOB.  This is
	   ;; done by walking recursively BLOB and updating MIN-IND
	   ;; along the way.  FIRST-FLAG is non-nil when the first
	   ;; string hasn't been seen yet.  It is required as this
	   ;; string is the only one whose indentation doesn't happen
	   ;; after a newline character.
	   (lambda (blob first-flag)
	     (dolist (object (org-element-contents blob))
	       (when (and first-flag (stringp object))
		 (setq first-flag nil)
		 (string-match "\\`\\( *\\)" object)
		 (let ((len (length (match-string 1 object))))
		   ;; An indentation of zero means no string will be
		   ;; modified.  Quit the process.
		   (if (zerop len) (throw 'zero (setq min-ind 0))
		     (setq min-ind (min len min-ind)))))
	       (cond
		((stringp object)
		 (dolist (line (delq "" (cdr (org-split-string object " *\n"))))
		   (setq min-ind (min (org-get-indentation line) min-ind))))
		((memq (org-element-type object) org-element-recursive-objects)
		 (funcall find-min-ind object first-flag))))))))
    ;; Find minimal indentation in ELEMENT.
    (catch 'zero (funcall find-min-ind element (not ignore-first)))
    (if (or (zerop min-ind) (= min-ind most-positive-fixnum)) element
      ;; Build ELEMENT back, replacing each string with the same
      ;; string minus common indentation.
      (let* (build			; For byte compiler.
	     (build
	      (function
	       (lambda (blob first-flag)
		 ;; Return BLOB with all its strings indentation
		 ;; shortened from MIN-IND white spaces.  FIRST-FLAG
		 ;; is non-nil when the first string hasn't been seen
		 ;; yet.
		 (setcdr (cdr blob)
			 (mapcar
			  #'(lambda (object)
			      (when (and first-flag (stringp object))
				(setq first-flag nil)
				(setq object
				      (replace-regexp-in-string
				       (format "\\` \\{%d\\}" min-ind)
				       "" object)))
			      (cond
			       ((stringp object)
				(replace-regexp-in-string
				 (format "\n \\{%d\\}" min-ind) "\n" object))
			       ((memq (org-element-type object)
				      org-element-recursive-objects)
				(funcall build object first-flag))
			       (t object)))
			  (org-element-contents blob)))
		 blob))))
	(funcall build element (not ignore-first))))))



;;; The Toolbox
;;
;; The first move is to implement a way to obtain the smallest element
;; containing point.  This is the job of `org-element-at-point'.  It
;; basically jumps back to the beginning of section containing point
;; and moves, element after element, with
;; `org-element--current-element' until the container is found.  Note:
;; When using `org-element-at-point', secondary values are never
;; parsed since the function focuses on elements, not on objects.
;;
;; At a deeper level, `org-element-context' lists all elements and
;; objects containing point.
;;
;; `org-element-nested-p' and `org-element-swap-A-B' may be used
;; internally by navigation and manipulation tools.

;;;###autoload
(defun org-element-at-point (&optional keep-trail)
  "Determine closest element around point.

Return value is a list like (TYPE PROPS) where TYPE is the type
of the element and PROPS a plist of properties associated to the
element.

Possible types are defined in `org-element-all-elements'.
Properties depend on element or object type, but always include
`:begin', `:end', `:parent' and `:post-blank' properties.

As a special case, if point is at the very beginning of a list or
sub-list, returned element will be that list instead of the first
item.  In the same way, if point is at the beginning of the first
row of a table, returned element will be the table instead of the
first row.

If optional argument KEEP-TRAIL is non-nil, the function returns
a list of elements leading to element at point.  The list's CAR
is always the element at point.  The following positions contain
element's siblings, then parents, siblings of parents, until the
first element of current section."
  (org-with-wide-buffer
   ;; If at a headline, parse it.  It is the sole element that
   ;; doesn't require to know about context.  Be sure to disallow
   ;; secondary string parsing, though.
   (if (org-with-limited-levels (org-at-heading-p))
       (progn
	 (beginning-of-line)
	 (if (not keep-trail) (org-element-headline-parser (point-max) t)
	   (list (org-element-headline-parser (point-max) t))))
     ;; Otherwise move at the beginning of the section containing
     ;; point.
     (catch 'exit
       (let ((origin (point))
	     (end (save-excursion
		    (org-with-limited-levels (outline-next-heading)) (point)))
	     element type special-flag trail struct prevs parent)
	 (org-with-limited-levels
	  (if (org-before-first-heading-p)
	      ;; In empty lines at buffer's beginning, return nil.
	      (progn (goto-char (point-min))
		     (org-skip-whitespace)
		     (when (or (eobp) (> (line-beginning-position) origin))
		       (throw 'exit nil)))
	    (org-back-to-heading)
	    (forward-line)
	    (org-skip-whitespace)
	    (when (or (eobp) (> (line-beginning-position) origin))
	      ;; In blank lines just after the headline, point still
	      ;; belongs to the headline.
	      (throw 'exit
		     (progn (skip-chars-backward " \r\t\n")
			    (beginning-of-line)
			    (if (not keep-trail)
				(org-element-headline-parser (point-max) t)
			      (list (org-element-headline-parser
				     (point-max) t))))))))
	 (beginning-of-line)
	 ;; Parse successively each element, skipping those ending
	 ;; before original position.
	 (while t
	   (setq element
		 (org-element--current-element end 'element special-flag struct)
		 type (car element))
	   (org-element-put-property element :parent parent)
	   (when keep-trail (push element trail))
	   (cond
	    ;; 1. Skip any element ending before point.  Also skip
	    ;;    element ending at point when we're sure that another
	    ;;    element has started.
	    ((let ((elem-end (org-element-property :end element)))
	       (when (or (< elem-end origin)
			 (and (= elem-end origin) (/= elem-end end)))
		 (goto-char elem-end))))
	    ;; 2. An element containing point is always the element at
	    ;;    point.
	    ((not (memq type org-element-greater-elements))
	     (throw 'exit (if keep-trail trail element)))
	    ;; 3. At any other greater element type, if point is
	    ;;    within contents, move into it.
	    (t
	     (let ((cbeg (org-element-property :contents-begin element))
		   (cend (org-element-property :contents-end element)))
	       (if (or (not cbeg) (not cend) (> cbeg origin) (< cend origin)
		       ;; Create an anchor for tables and plain lists:
		       ;; when point is at the very beginning of these
		       ;; elements, ignoring affiliated keywords,
		       ;; target them instead of their contents.
		       (and (= cbeg origin) (memq type '(plain-list table)))
		       ;; When point is at contents end, do not move
		       ;; into elements with an explicit ending, but
		       ;; return that element instead.
		       (and (= cend origin)
			    (or (memq type
				      '(center-block
					drawer dynamic-block inlinetask
					property-drawer quote-block
					special-block))
				;; Corner case: if a list ends at the
				;; end of a buffer without a final new
				;; line, return last element in last
				;; item instead.
				(and (memq type '(item plain-list))
				     (progn (goto-char cend)
					    (or (bolp) (not (eobp))))))))
		   (throw 'exit (if keep-trail trail element))
		 (setq parent element)
		 (case type
		   (plain-list
		    (setq special-flag 'item
			  struct (org-element-property :structure element)))
		   (item (setq special-flag nil))
		   (property-drawer
		    (setq special-flag 'node-property struct nil))
		   (table (setq special-flag 'table-row struct nil))
		   (otherwise (setq special-flag nil struct nil)))
		 (setq end cend)
		 (goto-char cbeg)))))))))))

;;;###autoload
(defun org-element-context (&optional element)
  "Return closest element or object around point.

Return value is a list like (TYPE PROPS) where TYPE is the type
of the element or object and PROPS a plist of properties
associated to it.

Possible types are defined in `org-element-all-elements' and
`org-element-all-objects'.  Properties depend on element or
object type, but always include `:begin', `:end', `:parent' and
`:post-blank'.

Optional argument ELEMENT, when non-nil, is the closest element
containing point, as returned by `org-element-at-point'.
Providing it allows for quicker computation."
  (catch 'objects-forbidden
    (org-with-wide-buffer
     (let* ((origin (point))
            (element (or element (org-element-at-point)))
            (type (org-element-type element))
            context)
       ;; Check if point is inside an element containing objects or at
       ;; a secondary string.  In that case, narrow buffer to the
       ;; containing area.  Otherwise, return ELEMENT.
       (cond
	;; At a parsed affiliated keyword, check if we're inside main
	;; or dual value.
	((let ((post (org-element-property :post-affiliated element)))
	   (and post (< origin post)))
	 (beginning-of-line)
	 (let ((case-fold-search t)) (looking-at org-element--affiliated-re))
	 (cond
	  ((not (member-ignore-case (match-string 1)
				    org-element-parsed-keywords))
	   (throw 'objects-forbidden element))
	  ((< (match-end 0) origin)
	   (narrow-to-region (match-end 0) (line-end-position)))
	  ((and (match-beginning 2)
		(>= origin (match-beginning 2))
		(< origin (match-end 2)))
	   (narrow-to-region (match-beginning 2) (match-end 2)))
	  (t (throw 'objects-forbidden element)))
	 ;; Also change type to retrieve correct restrictions.
	 (setq type 'keyword))
	;; At an item, objects can only be located within tag, if any.
	((eq type 'item)
	 (let ((tag (org-element-property :tag element)))
	   (if (not tag) (throw 'objects-forbidden element)
	     (beginning-of-line)
	     (search-forward tag (line-end-position))
	     (goto-char (match-beginning 0))
	     (if (and (>= origin (point)) (< origin (match-end 0)))
		 (narrow-to-region (point) (match-end 0))
	       (throw 'objects-forbidden element)))))
	;; At an headline or inlinetask, objects are located within
	;; their title.
	((memq type '(headline inlinetask))
	 (goto-char (org-element-property :begin element))
	 (skip-chars-forward "*")
	 (if (and (> origin (point)) (< origin (line-end-position)))
	     (narrow-to-region (point) (line-end-position))
	   (throw 'objects-forbidden element)))
	;; At a paragraph, a table-row or a verse block, objects are
	;; located within their contents.
	((memq type '(paragraph table-row verse-block))
	 (let ((cbeg (org-element-property :contents-begin element))
	       (cend (org-element-property :contents-end element)))
	   ;; CBEG is nil for table rules.
	   (if (and cbeg cend (>= origin cbeg) (< origin cend))
	       (narrow-to-region cbeg cend)
	     (throw 'objects-forbidden element))))
	;; At a parsed keyword, objects are located within value.
	((eq type 'keyword)
	 (if (not (member (org-element-property :key element)
			  org-element-document-properties))
	     (throw 'objects-forbidden element)
	   (beginning-of-line)
	   (search-forward ":")
	   (if (and (>= origin (point)) (< origin (line-end-position)))
	       (narrow-to-region (point) (line-end-position))
	     (throw 'objects-forbidden element))))
	;; At a planning line, if point is at a timestamp, return it,
	;; otherwise, return element.
	((eq type 'planning)
	 (dolist (p '(:closed :deadline :scheduled))
	   (let ((timestamp (org-element-property p element)))
	     (when (and timestamp
			(<= (org-element-property :begin timestamp) origin)
			(> (org-element-property :end timestamp) origin))
	       (throw 'objects-forbidden timestamp))))
	 (throw 'objects-forbidden element))
	(t (throw 'objects-forbidden element)))
       (goto-char (point-min))
       (let ((restriction (org-element-restriction type))
             (parent element)
             (candidates 'initial))
         (catch 'exit
           (while (setq candidates
			(org-element--get-next-object-candidates
			 restriction candidates))
             (let ((closest-cand (rassq (apply 'min (mapcar 'cdr candidates))
                                        candidates)))
               ;; If ORIGIN is before next object in element, there's
               ;; no point in looking further.
               (if (> (cdr closest-cand) origin) (throw 'exit parent)
                 (let* ((object
                         (progn (goto-char (cdr closest-cand))
                                (funcall (intern (format "org-element-%s-parser"
                                                         (car closest-cand))))))
                        (cbeg (org-element-property :contents-begin object))
                        (cend (org-element-property :contents-end object))
                        (obj-end (org-element-property :end object)))
                   (cond
                    ;; ORIGIN is after OBJECT, so skip it.
                    ((<= obj-end origin) (goto-char obj-end))
                    ;; ORIGIN is within a non-recursive object or at
                    ;; an object boundaries: Return that object.
                    ((or (not cbeg) (< origin cbeg) (>= origin cend))
                     (throw 'exit
                            (org-element-put-property object :parent parent)))
                    ;; Otherwise, move within current object and
                    ;; restrict search to the end of its contents.
                    (t (goto-char cbeg)
                       (narrow-to-region (point) cend)
                       (org-element-put-property object :parent parent)
                       (setq parent object
                             restriction (org-element-restriction object)
                             candidates 'initial)))))))
           parent))))))

(defun org-element-nested-p (elem-A elem-B)
  "Non-nil when elements ELEM-A and ELEM-B are nested."
  (let ((beg-A (org-element-property :begin elem-A))
	(beg-B (org-element-property :begin elem-B))
	(end-A (org-element-property :end elem-A))
	(end-B (org-element-property :end elem-B)))
    (or (and (>= beg-A beg-B) (<= end-A end-B))
	(and (>= beg-B beg-A) (<= end-B end-A)))))

(defun org-element-swap-A-B (elem-A elem-B)
  "Swap elements ELEM-A and ELEM-B.
Assume ELEM-B is after ELEM-A in the buffer.  Leave point at the
end of ELEM-A."
  (goto-char (org-element-property :begin elem-A))
  ;; There are two special cases when an element doesn't start at bol:
  ;; the first paragraph in an item or in a footnote definition.
  (let ((specialp (not (bolp))))
    ;; Only a paragraph without any affiliated keyword can be moved at
    ;; ELEM-A position in such a situation.  Note that the case of
    ;; a footnote definition is impossible: it cannot contain two
    ;; paragraphs in a row because it cannot contain a blank line.
    (if (and specialp
	     (or (not (eq (org-element-type elem-B) 'paragraph))
		 (/= (org-element-property :begin elem-B)
		     (org-element-property :contents-begin elem-B))))
	(error "Cannot swap elements"))
    ;; In a special situation, ELEM-A will have no indentation.  We'll
    ;; give it ELEM-B's (which will in, in turn, have no indentation).
    (let* ((ind-B (when specialp
		    (goto-char (org-element-property :begin elem-B))
		    (org-get-indentation)))
	   (beg-A (org-element-property :begin elem-A))
	   (end-A (save-excursion
		    (goto-char (org-element-property :end elem-A))
		    (skip-chars-backward " \r\t\n")
		    (point-at-eol)))
	   (beg-B (org-element-property :begin elem-B))
	   (end-B (save-excursion
		    (goto-char (org-element-property :end elem-B))
		    (skip-chars-backward " \r\t\n")
		    (point-at-eol)))
	   ;; Store overlays responsible for visibility status.  We
	   ;; also need to store their boundaries as they will be
	   ;; removed from buffer.
	   (overlays
	    (cons
	     (mapcar (lambda (ov) (list ov (overlay-start ov) (overlay-end ov)))
		     (overlays-in beg-A end-A))
	     (mapcar (lambda (ov) (list ov (overlay-start ov) (overlay-end ov)))
		     (overlays-in beg-B end-B))))
	   ;; Get contents.
	   (body-A (buffer-substring beg-A end-A))
	   (body-B (delete-and-extract-region beg-B end-B)))
      (goto-char beg-B)
      (when specialp
	(setq body-B (replace-regexp-in-string "\\`[ \t]*" "" body-B))
	(org-indent-to-column ind-B))
      (insert body-A)
      ;; Restore ex ELEM-A overlays.
      (let ((offset (- beg-B beg-A)))
	(mapc (lambda (ov)
		(move-overlay
		 (car ov) (+ (nth 1 ov) offset) (+ (nth 2 ov) offset)))
	      (car overlays))
	(goto-char beg-A)
	(delete-region beg-A end-A)
	(insert body-B)
	;; Restore ex ELEM-B overlays.
	(mapc (lambda (ov)
		(move-overlay
		 (car ov) (- (nth 1 ov) offset) (- (nth 2 ov) offset)))
	      (cdr overlays)))
      (goto-char (org-element-property :end elem-B)))))

(provide 'org-element)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-element.el ends here
