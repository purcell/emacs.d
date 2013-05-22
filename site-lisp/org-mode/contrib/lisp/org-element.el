;;; org-element.el --- Parser And Applications for Org syntax

;; Copyright (C) 2011, 2012  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; This file is not part of GNU Emacs.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org syntax can be divided into three categories: "Greater
;; elements", "Elements" and "Objects".

;; An object can be defined anywhere on a line.  It may span over more
;; than a line but never contains a blank one.  Objects belong to the
;; following types: `emphasis', `entity', `export-snippet',
;; `footnote-reference', `inline-babel-call', `inline-src-block',
;; `latex-fragment', `line-break', `link', `macro', `radio-target',
;; `statistics-cookie', `subscript', `superscript', `target',
;; `time-stamp' and `verbatim'.

;; An element always starts and ends at the beginning of a line.  The
;; only element's type containing objects is called a `paragraph'.
;; Other types are: `comment', `comment-block', `example-block',
;; `export-block', `fixed-width', `horizontal-rule', `keyword',
;; `latex-environment', `babel-call', `property-drawer',
;; `quote-section', `src-block', `table' and `verse-block'.

;; Elements containing paragraphs are called greater elements.
;; Concerned types are: `center-block', `drawer', `dynamic-block',
;; `footnote-definition', `headline', `inlinetask', `item',
;; `plain-list', `quote-block' and `special-block'.

;; Greater elements (excepted `headline' and `item' types) and
;; elements (excepted `keyword', `babel-call', and `property-drawer'
;; types) can have a fixed set of keywords as attributes.  Those are
;; called "affiliated keywords", to distinguish them from others
;; keywords, which are full-fledged elements.  In particular, the
;; "name" affiliated keyword allows to label almost any element in an
;; Org buffer.

;; Notwithstanding affiliated keywords, each greater element, element
;; and object has a fixed set of properties attached to it.  Among
;; them, three are shared by all types: `:begin' and `:end', which
;; refer to the beginning and ending buffer positions of the
;; considered element or object, and `:post-blank', which holds the
;; number of blank lines, or white spaces, at its end.

;; Some elements also have special properties whose value can hold
;; objects themselves (i.e. an item tag, an headline name, a table
;; cell).  Such values are called "secondary strings".

;; Lisp-wise, an element or an object can be represented as a list.
;; It follows the pattern (TYPE PROPERTIES CONTENTS), where:
;;   TYPE is a symbol describing the Org element or object.
;;   PROPERTIES is the property list attached to it.  See docstring of
;;              appropriate parsing function to get an exhaustive
;;              list.
;;   CONTENTS is a list of elements, objects or raw strings contained
;;            in the current element or object, when applicable.

;; An Org buffer is a nested list of such elements and objects, whose
;; type is `org-data' and properties is nil.

;; The first part of this file implements a parser and an interpreter
;; for each type of Org syntax.

;; The next two parts introduce two accessors and a function
;; retrieving the smallest element containing point (respectively
;; `org-element-get-property', `org-element-get-contents' and
;; `org-element-at-point').

;; The following part creates a fully recursive buffer parser.  It
;; also provides a tool to map a function to elements or objects
;; matching some criteria in the parse tree.  Functions of interest
;; are `org-element-parse-buffer', `org-element-map' and, to a lesser
;; extent, `org-element-parse-secondary-string'.

;; The penultimate part is the cradle of an interpreter for the
;; obtained parse tree: `org-element-interpret-data' (and its
;; relative, `org-element-interpret-secondary').

;; The library ends by furnishing a set of interactive tools for
;; element's navigation and manipulation.


;;; Code:

(eval-when-compile (require 'cl))
(require 'org)
(declare-function org-inlinetask-goto-end "org-inlinetask" ())


;;; Greater elements

;; For each greater element type, we define a parser and an
;; interpreter.

;; A parser (`item''s excepted) accepts no argument and represents the
;; element or object as the list described above.  An interpreter
;; accepts two arguments: the list representation of the element or
;; object, and its contents.  The latter may be nil, depending on the
;; element or object considered.  It returns the appropriate Org
;; syntax, as a string.

;; Parsing functions must follow the naming convention:
;; org-element-TYPE-parser, where TYPE is greater element's type, as
;; defined in `org-element-greater-elements'.
;;
;; Similarly, interpreting functions must follow the naming
;; convention: org-element-TYPE-interpreter.

;; With the exception of `headline' and `item' types, greater elements
;; cannot contain other greater elements of their own type.

;; Beside implementing a parser and an interpreter, adding a new
;; greater element requires to tweak `org-element-guess-type'.
;; Moreover, the newly defined type must be added to both
;; `org-element-all-elements' and `org-element-greater-elements'.


;;;; Center Block
(defun org-element-center-block-parser ()
  "Parse a center block.

Return a list whose car is `center-block' and cdr is a plist
containing `:begin', `:end', `:hiddenp', `:contents-begin',
`:contents-end' and `:post-blank' keywords.

Assume point is at beginning or end of the block."
  (save-excursion
    (let* ((case-fold-search t)
	   (keywords (progn
		       (end-of-line)
		       (re-search-backward
			(concat "^[ \t]*#\\+begin_center") nil t)
		       (org-element-collect-affiliated-keywords)))
	   (begin (car keywords))
	   (contents-begin (progn (forward-line) (point)))
	   (hidden (org-truely-invisible-p))
	   (contents-end (progn (re-search-forward
				 (concat "^[ \t]*#\\+end_center") nil t)
				(point-at-bol)))
	   (pos-before-blank (progn (forward-line) (point)))
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol)))))
      (list 'center-block
	    `(:begin ,begin
		     :end ,end
		     :hiddenp ,hidden
		     :contents-begin ,contents-begin
		     :contents-end ,contents-end
		     :post-blank ,(count-lines pos-before-blank end)
		     ,@(cadr keywords))))))

(defun org-element-center-block-interpreter (center-block contents)
  "Interpret CENTER-BLOCK element as Org syntax.
CONTENTS is the contents of the element."
  (format "#+begin_center\n%s#+end_center" contents))

;;;; Drawer
(defun org-element-drawer-parser ()
  "Parse a drawer.

Return a list whose car is `drawer' and cdr is a plist containing
`:drawer-name', `:begin', `:end', `:hiddenp', `:contents-begin',
`:contents-end' and `:post-blank' keywords.

Assume point is at beginning of drawer."
  (save-excursion
    (let* ((case-fold-search t)
	   (name (progn (looking-at org-drawer-regexp)
			(org-match-string-no-properties 1)))
	   (keywords (org-element-collect-affiliated-keywords))
	   (begin (car keywords))
	   (contents-begin (progn (forward-line) (point)))
	   (hidden (org-truely-invisible-p))
	   (contents-end (progn (re-search-forward "^[ \t]*:END:" nil t)
				(point-at-bol)))
	   (pos-before-blank (progn (forward-line) (point)))
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol)))))
      (list 'drawer
	    `(:begin ,begin
		     :end ,end
		     :drawer-name ,name
		     :hiddenp ,hidden
		     :contents-begin ,contents-begin
		     :contents-end ,contents-end
		     :post-blank ,(count-lines pos-before-blank end)
		     ,@(cadr keywords))))))

(defun org-element-drawer-interpreter (drawer contents)
  "Interpret DRAWER element as Org syntax.
CONTENTS is the contents of the element."
  (format ":%s:\n%s:END:"
	  (org-element-get-property :drawer-name drawer)
	  contents))

;;;; Dynamic Block
(defun org-element-dynamic-block-parser ()
  "Parse a dynamic block.

Return a list whose car is `dynamic-block' and cdr is a plist
containing `:block-name', `:begin', `:end', `:hiddenp',
`:contents-begin', `:contents-end', `:arguments' and
`:post-blank' keywords.

Assume point is at beginning of dynamic block."
  (save-excursion
    (let* ((case-fold-search t)
	   (name (progn (looking-at org-dblock-start-re)
			(org-match-string-no-properties 1)))
	   (arguments (org-match-string-no-properties 3))
	   (keywords (org-element-collect-affiliated-keywords))
	   (begin (car keywords))
	   (contents-begin (progn (forward-line) (point)))
	   (hidden (org-truely-invisible-p))
	   (contents-end (progn (re-search-forward org-dblock-end-re nil t)
				(point-at-bol)))
	   (pos-before-blank (progn (forward-line) (point)))
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol)))))
      (list 'dynamic-block
	    `(:begin ,begin
		     :end ,end
		     :block-name ,name
		     :arguments ,arguments
		     :hiddenp ,hidden
		     :contents-begin ,contents-begin
		     :contents-end ,contents-end
		     :post-blank ,(count-lines pos-before-blank end)
		     ,@(cadr keywords))))))

(defun org-element-dynamic-block-interpreter (dynamic-block contents)
  "Interpret DYNAMIC-BLOCK element as Org syntax.
CONTENTS is the contents of the element."
  (format "#+BEGIN: %s%s\n%s#+END:"
	  (org-element-get-property :block-name dynamic-block)
	  (let ((args (org-element-get-property :arguments dynamic-block)))
	    (and arg (concat " " args)))
	  contents))

;;;; Footnote Definition

(defun org-element-footnote-definition-parser ()
  "Parse a footnote definition.

Return a list whose car is `footnote-definition' and cdr is
a plist containing `:label', `:begin' `:end', `:contents-begin',
`:contents-end' and `:post-blank' keywords."
  (save-excursion
    (let* ((f-def (org-footnote-at-definition-p))
	   (label (car f-def))
	   (keywords (progn (goto-char (nth 1 f-def))
			    (org-element-collect-affiliated-keywords)))
	   (begin (car keywords))
	   (contents-begin (progn (looking-at (concat "\\[" label "\\]"))
				  (goto-char (match-end 0))
				  (org-skip-whitespace)
				  (point)))
	   (end (goto-char (nth 2 f-def)))
	   (contents-end (progn (skip-chars-backward " \r\t\n")
				(forward-line)
				(point))))
      (list 'footnote-definition
	    `(:label ,label
		     :begin ,begin
		     :end ,end
		     :contents-begin ,contents-begin
		     :contents-end ,contents-end
		     :post-blank ,(count-lines contents-end end)
		     ,@(cadr keywords))))))

(defun org-element-footnote-definition-interpreter (footnote-definition contents)
  "Interpret FOOTNOTE-DEFINITION element as Org syntax.
CONTENTS is the contents of the footnote-definition."
  (concat (format "[%s]" (org-element-get-property :label footnote-definition))
	  " "
	  contents))


;;;; Headline
(defun org-element-headline-parser ()
  "Parse an headline.

Return a list whose car is `headline' and cdr is a plist
containing `:raw-value', `:title', `:begin', `:end',
`:pre-blank', `:hiddenp', `:contents-begin' and `:contents-end',
`:level', `:priority', `:tags', `:todo-keyword',`:todo-type',
`:scheduled', `:deadline', `:timestamp', `:clock', `:category',
`:quotedp', `:archivedp', `:commentedp' and `:footnote-section-p'
keywords.

The plist also contains any property set in the property drawer,
with its name in lowercase, the underscores replaced with hyphens
and colons at the beginning (i.e. `:custom-id').

Assume point is at beginning of the headline."
  (save-excursion
    (let* ((components (org-heading-components))
	   (level (nth 1 components))
	   (todo (nth 2 components))
	   (todo-type (and todo
			   (if (member todo org-done-keywords) 'done 'todo)))
	   (tags (nth 5 components))
	   (raw-value (nth 4 components))
	   (quotedp (string-match (format "^%s +" org-quote-string) raw-value))
	   (commentedp (string-match
			(format "^%s +" org-comment-string) raw-value))
	   (archivedp (and tags
			   (string-match (format ":%s:" org-archive-tag) tags)))
	   (footnote-section-p (and org-footnote-section
				    (string= org-footnote-section raw-value)))
	   (standard-props (let (plist)
			     (mapc
			      (lambda (p)
				(let ((p-name (downcase (car p))))
				  (while (string-match "_" p-name)
				    (setq p-name
					  (replace-match "-" nil nil p-name)))
				  (setq p-name (intern (concat ":" p-name)))
				  (setq plist
					(plist-put plist p-name (cdr p)))))
			      (org-entry-properties nil 'standard))
			     plist))
	   (time-props (org-entry-properties nil 'special "CLOCK"))
	   (scheduled (cdr (assoc "SCHEDULED" time-props)))
	   (deadline (cdr (assoc "DEADLINE" time-props)))
	   (clock (cdr (assoc "CLOCK" time-props)))
	   (timestamp (cdr (assoc "TIMESTAMP" time-props)))
	   (begin (point))
	   (pos-after-head (save-excursion (forward-line) (point)))
	   (contents-begin (save-excursion (forward-line)
					   (org-skip-whitespace)
					   (if (eobp) (point) (point-at-bol))))
	   (hidden (save-excursion (forward-line) (org-truely-invisible-p)))
	   (end (progn (goto-char (org-end-of-subtree t t))))
	   (contents-end (progn (skip-chars-backward " \r\t\n")
				(forward-line)
				(point)))
	   title)
      ;; Clean RAW-VALUE from any quote or comment string.
      (when (or quotedp commentedp)
	(setq raw-value
	      (replace-regexp-in-string
	       (concat "\\(" org-quote-string "\\|" org-comment-string "\\) +")
	       ""
	       raw-value)))
      ;; Clean TAGS from archive tag, if any.
      (when archivedp
	(setq tags
	      (and (not (string= tags (format ":%s:" org-archive-tag)))
		   (replace-regexp-in-string
		    (concat org-archive-tag ":") "" tags)))
	(when (string= tags ":") (setq tags nil)))
      ;; Then get TITLE.
      (setq title (org-element-parse-secondary-string
		   raw-value
		   (cdr (assq 'headline org-element-string-restrictions))))
      (list 'headline
	    `(:raw-value ,raw-value
			 :title ,title
			 :begin ,begin
			 :end ,end
			 :pre-blank ,(count-lines pos-after-head contents-begin)
			 :hiddenp ,hidden
			 :contents-begin ,contents-begin
			 :contents-end ,contents-end
			 :level ,level
			 :priority ,(nth 3 components)
			 :tags ,tags
			 :todo-keyword ,todo
			 :todo-type ,todo-type
			 :scheduled ,scheduled
			 :deadline ,deadline
			 :timestamp ,timestamp
			 :clock ,clock
			 :post-blank ,(count-lines contents-end end)
			 :footnote-section-p ,footnote-section-p
			 :archivedp ,archivedp
			 :commentedp ,commentedp
			 :quotedp ,quotedp
			 ,@standard-props)))))

(defun org-element-headline-interpreter (headline contents)
  "Interpret HEADLINE element as Org syntax.
CONTENTS is the contents of the element."
  (let* ((level (org-element-get-property :level headline))
	 (todo (org-element-get-property :todo-keyword headline))
	 (priority (org-element-get-property :priority headline))
	 (title (org-element-get-property :raw-value headline))
	 (tags (let ((tag-string (org-element-get-property :tags headline))
		     (archivedp (org-element-get-property :archivedp headline)))
		 (cond
		  ((and (not tag-string) archivedp)
		   (format ":%s:" org-archive-tag))
		  (archivedp (concat ":" org-archive-tag tag-string))
		  (t tag-string))))
	 (commentedp (org-element-get-property :commentedp headline))
	 (quotedp (org-element-get-property :quotedp headline))
	 (pre-blank (org-element-get-property :pre-blank headline))
	 (heading (concat (make-string level ?*)
			  (and todo (concat " " todo))
			  (and quotedp (concat " " org-quote-string))
			  (and commentedp (concat " " org-comment-string))
			  (and priority (concat " " priority))
			  (cond ((and org-footnote-section
				      (org-element-get-property
				       :footnote-section-p headline))
				 (concat " " org-footnote-section))
				(title (concat " " title)))))
	 ;; Align tags.
	 (tags-fmt (when tags
		     (let ((tags-len (length tags)))
		       (format "%% %ds"
			       (cond
				((zerop org-tags-column) (1+ tags-len))
				((< org-tags-column 0)
				 (max (- (+ org-tags-column (length heading)))
				      (1+ tags-len)))
				(t (max (+ (- org-tags-column (length heading))
					   tags-len)
					(1+ tags-len)))))))))
    (concat heading (and tags (format tags-fmt tags))
	    (make-string (1+ pre-blank) 10)
	    contents)))

;;;; Inlinetask
(defun org-element-inlinetask-parser ()
  "Parse an inline task.

Return a list whose car is `inlinetask' and cdr is a plist
containing `:raw-value', `:title', `:begin', `:end', `:hiddenp',
`:contents-begin' and `:contents-end', `:level', `:priority',
`:raw-value', `:tags', `:todo-keyword', `:todo-type',
`:scheduled', `:deadline', `:timestamp', `:clock' and
`:post-blank' keywords.

The plist also contains any property set in the property drawer,
with its name in lowercase, the underscores replaced with hyphens
and colons at the beginning (i.e. `:custom-id').

Assume point is at beginning of the inline task."
  (save-excursion
    (let* ((keywords (org-element-collect-affiliated-keywords))
	   (begin (car keywords))
	   (components (org-heading-components))
	   (todo (nth 2 components))
	   (todo-type (and todo
			   (if (member todo org-done-keywords) 'done 'todo)))
	   (raw-value (nth 4 components))
	   (standard-props (let (plist)
			     (mapc
			      (lambda (p)
				(let ((p-name (downcase (car p))))
				  (while (string-match "_" p-name)
				    (setq p-name
					  (replace-match "-" nil nil p-name)))
				  (setq p-name (intern (concat ":" p-name)))
				  (setq plist
					(plist-put plist p-name (cdr p)))))
			      (org-entry-properties nil 'standard))
			     plist))
	   (time-props (org-entry-properties nil 'special "CLOCK"))
	   (scheduled (cdr (assoc "SCHEDULED" time-props)))
	   (deadline (cdr (assoc "DEADLINE" time-props)))
	   (clock (cdr (assoc "CLOCK" time-props)))
	   (timestamp (cdr (assoc "TIMESTAMP" time-props)))
	   (title (org-element-parse-secondary-string
		   raw-value
		   (cdr (assq 'inlinetask org-element-string-restrictions))))
	   (contents-begin (save-excursion (forward-line) (point)))
	   (hidden (org-truely-invisible-p))
	   (pos-before-blank (org-inlinetask-goto-end))
	   ;; In the case of a single line task, CONTENTS-BEGIN and
	   ;; CONTENTS-END might overlap.
	   (contents-end (max contents-begin
			      (save-excursion (forward-line -1) (point))))
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol)))))
      (list 'inlinetask
	    `(:raw-value ,raw-value
			 :title ,title
			 :begin ,begin
			 :end ,end
			 :hiddenp ,(and (> contents-end contents-begin) hidden)
			 :contents-begin ,contents-begin
			 :contents-end ,contents-end
			 :level ,(nth 1 components)
			 :priority ,(nth 3 components)
			 :tags ,(nth 5 components)
			 :todo-keyword ,todo
			 :todo-type ,todo-type
			 :scheduled ,scheduled
			 :deadline ,deadline
			 :timestamp ,timestamp
			 :clock ,clock
			 :post-blank ,(count-lines pos-before-blank end)
			 ,@standard-props
			 ,@(cadr keywords))))))

(defun org-element-inlinetask-interpreter (inlinetask contents)
  "Interpret INLINETASK element as Org syntax.
CONTENTS is the contents of inlinetask."
  (let* ((level (org-element-get-property :level inlinetask))
	 (todo (org-element-get-property :todo-keyword inlinetask))
	 (priority (org-element-get-property :priority inlinetask))
	 (title (org-element-get-property :raw-value inlinetask))
	 (tags (org-element-get-property :tags inlinetask))
	 (task (concat (make-string level ?*)
		       (and todo (concat " " todo))
		       (and priority (concat " " priority))
		       (and title (concat " " title))))
	 ;; Align tags.
	 (tags-fmt (when tags
		     (format "%% %ds"
			     (cond
			      ((zerop org-tags-column) 1)
			      ((< 0 org-tags-column)
			       (max (+ org-tags-column
				       (length inlinetask)
				       (length tags))
				    1))
			      (t (max (- org-tags-column (length inlinetask))
				      1)))))))
    (concat inlinetask (and tags (format tags-fmt tags) "\n" contents))))

;;;; Item
(defun org-element-item-parser (struct)
  "Parse an item.

STRUCT is the structure of the plain list.

Return a list whose car is `item' and cdr is a plist containing
`:bullet', `:begin', `:end', `:contents-begin', `:contents-end',
`:checkbox', `:counter', `:tag', `:raw-tag', `:structure',
`:hiddenp' and `:post-blank' keywords.

Assume point is at the beginning of the item."
  (save-excursion
    (beginning-of-line)
    (let* ((begin (point))
	   (bullet (org-list-get-bullet (point) struct))
	   (checkbox (let ((box (org-list-get-checkbox begin struct)))
		       (cond ((equal "[ ]" box) 'off)
			     ((equal "[X]" box) 'on)
			     ((equal "[-]" box) 'trans))))
	   (counter (let ((c (org-list-get-counter begin struct)))
		      (cond
		       ((not c) nil)
		       ((string-match "[A-Za-z]" c)
			(- (string-to-char (upcase (match-string 0 c)))
			   64))
		       ((string-match "[0-9]+" c)
			(string-to-number (match-string 0 c))))))
	   (raw-tag (org-list-get-tag begin struct))
	   (tag (and raw-tag
		     (org-element-parse-secondary-string
		      raw-tag
		      (cdr (assq 'item org-element-string-restrictions)))))
	   (end (org-list-get-item-end begin struct))
	   (contents-begin (progn (looking-at org-list-full-item-re)
				  (goto-char (match-end 0))
				  (org-skip-whitespace)
				  (if (>= (point) end)
				      (point-at-bol)
				    (point))))
	   (hidden (progn (forward-line)
			  (and (not (= (point) end))
			       (org-truely-invisible-p))))
	   (contents-end (progn (goto-char end)
				(skip-chars-backward " \r\t\n")
				(forward-line)
				(point))))
      (list 'item
	    `(:bullet ,bullet
		      :begin ,begin
		      :end ,end
		      ;; CONTENTS-BEGIN and CONTENTS-END may be mixed
		      ;; up in the case of an empty item separated
		      ;; from the next by a blank line.  Thus, ensure
		      ;; the former is always the smallest of two.
		      :contents-begin ,(min contents-begin contents-end)
		      :contents-end ,(max contents-begin contents-end)
		      :checkbox ,checkbox
		      :counter ,counter
		      :raw-tag ,raw-tag
		      :tag ,tag
		      :hiddenp ,hidden
		      :structure ,struct
		      :post-blank ,(count-lines contents-end end))))))

(defun org-element-item-interpreter (item contents)
  "Interpret ITEM element as Org syntax.
CONTENTS is the contents of the element."
  (let* ((bullet (org-element-get-property :bullet item))
	 (checkbox (org-element-get-property :checkbox item))
	 (counter (org-element-get-property :counter item))
	 (tag (org-element-get-property :raw-tag item))
	 ;; Compute indentation.
	 (ind (make-string (length bullet) 32)))
    ;; Indent contents.
    (concat
     bullet
     (when (and org-list-two-spaces-after-bullet-regexp
		(string-match org-list-two-spaces-after-bullet-regexp bullet))
       " ")
     (and counter (format "[@%d] " counter))
     (cond
      ((eq checkbox 'on) "[X] ")
      ((eq checkbox 'off) "[ ] ")
      ((eq checkbox 'trans) "[-] "))
     (and tag (format "%s :: " tag))
     (org-trim
      (replace-regexp-in-string
       "\\(^\\)[ \t]*\\S-" ind contents nil nil 1)))))

;;;; Plain List
(defun org-element-plain-list-parser (&optional structure)
  "Parse a plain list.

Return a list whose car is `plain-list' and cdr is a plist
containing `:type', `:begin', `:end', `:contents-begin' and
`:contents-end', `:level', `:structure' and `:post-blank'
keywords.

Assume point is at one of the list items."
  (save-excursion
    (let* ((struct (or structure (org-list-struct)))
	   (prevs (org-list-prevs-alist struct))
	   (parents (org-list-parents-alist struct))
	   (type (org-list-get-list-type (point) struct prevs))
	   (contents-begin (goto-char
			    (org-list-get-list-begin (point) struct prevs)))
	   (keywords (org-element-collect-affiliated-keywords))
	   (begin (car keywords))
	   (contents-end (goto-char
			  (org-list-get-list-end (point) struct prevs)))
	   (end (save-excursion (org-skip-whitespace)
				(if (eobp) (point) (point-at-bol))))
	   (level 0))
      ;; Get list level.
      (let ((item contents-begin))
	(while (setq item
		     (org-list-get-parent
		      (org-list-get-list-begin item struct prevs)
		      struct parents))
	  (incf level)))
      ;; Blank lines below list belong to the top-level list only.
      (when (> level 0)
	(setq end (min (org-list-get-bottom-point struct)
		       (progn (org-skip-whitespace)
			      (if (eobp) (point) (point-at-bol))))))
      ;; Return value.
      (list 'plain-list
	    `(:type ,type
		    :begin ,begin
		    :end ,end
		    :contents-begin ,contents-begin
		    :contents-end ,contents-end
		    :level ,level
		    :structure ,struct
		    :post-blank ,(count-lines contents-end end)
		    ,@(cadr keywords))))))

(defun org-element-plain-list-interpreter (plain-list contents)
  "Interpret PLAIN-LIST element as Org syntax.
CONTENTS is the contents of the element."
  contents)

;;;; Quote Block
(defun org-element-quote-block-parser ()
  "Parse a quote block.

Return a list whose car is `quote-block' and cdr is a plist
containing `:begin', `:end', `:hiddenp', `:contents-begin',
`:contents-end' and `:post-blank' keywords.

Assume point is at beginning or end of the block."
  (save-excursion
    (let* ((case-fold-search t)
	   (keywords (progn
		       (end-of-line)
		       (re-search-backward
			(concat "^[ \t]*#\\+begin_quote") nil t)
		       (org-element-collect-affiliated-keywords)))
	   (begin (car keywords))
	   (contents-begin (progn (forward-line) (point)))
	   (hidden (org-truely-invisible-p))
	   (contents-end (progn (re-search-forward
				 (concat "^[ \t]*#\\+end_quote") nil t)
				(point-at-bol)))
	   (pos-before-blank (progn (forward-line) (point)))
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol)))))
      (list 'quote-block
	    `(:begin ,begin
		     :end ,end
		     :hiddenp ,hidden
		     :contents-begin ,contents-begin
		     :contents-end ,contents-end
		     :post-blank ,(count-lines pos-before-blank end)
		     ,@(cadr keywords))))))


(defun org-element-quote-block-interpreter (quote-block contents)
  "Interpret QUOTE-BLOCK element as Org syntax.
CONTENTS is the contents of the element."
  (format "#+begin_quote\n%s#+end_quote" contents))

;;;; Special Block
(defun org-element-special-block-parser ()
  "Parse a special block.

Return a list whose car is `special-block' and cdr is a plist
containing `:type', `:begin', `:end', `:hiddenp',
`:contents-begin', `:contents-end' and `:post-blank' keywords.

Assume point is at beginning or end of the block."
  (save-excursion
    (let* ((case-fold-search t)
	   (type (progn (looking-at
			 "[ \t]*#\\+\\(?:begin\\|end\\)_\\([-A-Za-z0-9]+\\)")
			(org-match-string-no-properties 1)))
	   (keywords (progn
		       (end-of-line)
		       (re-search-backward
			(concat "^[ \t]*#\\+begin_" type) nil t)
		       (org-element-collect-affiliated-keywords)))
	   (begin (car keywords))
	   (contents-begin (progn (forward-line) (point)))
	   (hidden (org-truely-invisible-p))
	   (contents-end (progn (re-search-forward
				 (concat "^[ \t]*#\\+end_" type) nil t)
				(point-at-bol)))
	   (pos-before-blank (progn (forward-line) (point)))
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol)))))
      (list 'special-block
	    `(:type ,type
		    :begin ,begin
		    :end ,end
		    :hiddenp ,hidden
		    :contents-begin ,contents-begin
		    :contents-end ,contents-end
		    :post-blank ,(count-lines pos-before-blank end)
		    ,@(cadr keywords))))))

(defun org-element-special-block-interpreter (special-block contents)
  "Interpret SPECIAL-BLOCK element as Org syntax.
CONTENTS is the contents of the element."
  (let ((block-type (org-element-get-property :type special-block)))
    (format "#+begin_%s\n%s#+end_%s" block-type contents block-type)))



;;; Elements

;; For each element, a parser and an interpreter are also defined.
;; Both follow the same naming convention used for greater elements.

;; Also, as for greater elements, adding a new element type is done
;; through the following steps: implement a parser and an interpreter,
;; tweak `org-element-guess-type' so that it recognizes the new type
;; and add that new type to `org-element-all-elements'.

;; As a special case, when the newly defined type is a block type,
;; `org-element-non-recursive-block-alist' has to be modified
;; accordingly.


;;;; Babel Call
(defun org-element-babel-call-parser ()
  "Parse a babel call.

Return a list whose car is `babel-call' and cdr is a plist
containing `:begin', `:end', `:info' and `:post-blank' as
keywords."
  (save-excursion
    (let ((info (progn (looking-at org-babel-block-lob-one-liner-regexp)
		       (org-babel-lob-get-info)))
	  (beg (point-at-bol))
	  (pos-before-blank (progn (forward-line) (point)))
	  (end (progn (org-skip-whitespace)
		      (if (eobp) (point) (point-at-bol)))))
      (list 'babel-call
	    `(:beg ,beg
		   :end ,end
		   :info ,info
		   :post-blank ,(count-lines pos-before-blank end))))))

(defun org-element-babel-call-interpreter (inline-babel-call contents)
  "Interpret INLINE-BABEL-CALL object as Org syntax.
CONTENTS is nil."
  (let* ((babel-info (org-element-get-property :info inline-babel-call))
	 (main-source (car babel-info))
	 (post-options (nth 1 babel-info)))
    (concat "#+call: "
	    (if (string-match "\\[\\(\\[.*?\\]\\)\\]" main-source)
		;; Remove redundant square brackets.
		(replace-match
		 (match-string 1 main-source) nil nil main-source)
	      main-source)
	    (and post-options (format "[%s]" post-options)))))

;;;; Comment
(defun org-element-comment-parser ()
  "Parse a comment.

Return a list whose car is `comment' and cdr is a plist
containing `:begin', `:end', `:value' and `:post-blank'
keywords."
  (let ((comment-re "\\(#\\|[ \t]*#\\+\\( \\|$\\)\\)")
	beg-coms begin end value pos-before-blank keywords)
    (save-excursion
      ;; Move to the beginning of comments.
      (unless (bobp)
	(while (and (not (bobp)) (looking-at comment-re))
	  (forward-line -1))
	(unless (looking-at comment-re) (forward-line 1)))
      (setq beg-coms (point))
      ;; Get affiliated keywords, if any.
      (setq keywords (org-element-collect-affiliated-keywords))
      ;; Store true beginning of element.
      (setq begin (car keywords))
      ;; Get ending of comments.  If point is in a list, ensure to not
      ;; get outside of it.
      (let* ((itemp (org-in-item-p))
	     (max-pos (if itemp
			  (org-list-get-bottom-point
			   (save-excursion (goto-char itemp) (org-list-struct)))
			(point-max))))
	(while (and (looking-at comment-re) (< (point) max-pos))
	  (forward-line)))
      (setq pos-before-blank (point))
      ;; Find position after blank.
      (org-skip-whitespace)
      (setq end (if (eobp) (point) (point-at-bol)))
      ;; Extract value.
      (setq value (buffer-substring-no-properties beg-coms pos-before-blank)))
    (list 'comment
	  `(:begin ,begin
		   :end ,end
		   :value ,value
		   :post-blank ,(count-lines pos-before-blank end)
		   ,@(cadr keywords)))))

(defun org-element-comment-interpreter (comment contents)
  "Interpret COMMENT element as Org syntax.
CONTENTS is nil."
  (org-element-get-property :value comment))

;;;; Comment Block
(defun org-element-comment-block-parser ()
  "Parse an export block.

Return a list whose car is `comment-block' and cdr is a plist
containing `:begin', `:end', `:hiddenp', `:value' and
`:post-blank' keywords."
  (save-excursion
    (end-of-line)
    (let* ((case-fold-search t)
	   (keywords (progn
		       (re-search-backward "^[ \t]*#\\+begin_comment" nil t)
		       (org-element-collect-affiliated-keywords)))
	   (begin (car keywords))
	   (contents-begin (progn (forward-line) (point)))
	   (hidden (org-truely-invisible-p))
	   (contents-end (progn (re-search-forward
				 "^[ \t]*#\\+end_comment" nil t)
				(point-at-bol)))
	   (pos-before-blank (progn (forward-line) (point)))
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol))))
	   (value (buffer-substring-no-properties contents-begin contents-end)))
      (list 'comment-block
	    `(:begin ,begin
		     :end ,end
		     :value ,value
		     :hiddenp ,hidden
		     :post-blank ,(count-lines pos-before-blank end)
		     ,@(cadr keywords))))))

(defun org-element-comment-block-interpreter (comment-block contents)
  "Interpret COMMENT-BLOCK element as Org syntax.
CONTENTS is nil."
  (concat "#+begin_comment\n"
	  (org-remove-indentation
	   (org-element-get-property :value comment-block))
	  "#+begin_comment"))

;;;; Example Block
(defun org-element-example-block-parser ()
  "Parse an example block.

Return a list whose car is `example' and cdr is a plist
containing `:begin', `:end', `:options', `:hiddenp', `:value' and
`:post-blank' keywords."
  (save-excursion
    (end-of-line)
    (let* ((case-fold-search t)
	   (options (progn
		      (re-search-backward
		       "^[ \t]*#\\+begin_example\\(?: +\\(.*\\)\\)?" nil t)
		      (org-match-string-no-properties 1)))
	   (keywords (org-element-collect-affiliated-keywords))
	   (begin (car keywords))
	   (contents-begin (progn (forward-line) (point)))
	   (hidden (org-truely-invisible-p))
	   (contents-end (progn
			   (re-search-forward "^[ \t]*#\\+end_example" nil t)
			   (point-at-bol)))
	   (value (buffer-substring-no-properties contents-begin contents-end))
	   (pos-before-blank (progn (forward-line) (point)))
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol)))))
      (list 'example-block
	    `(:begin ,begin
		     :end ,end
		     :value ,value
		     :options ,options
		     :hiddenp ,hidden
		     :post-blank ,(count-lines pos-before-blank end)
		     ,@(cadr keywords))))))

(defun org-element-example-block-interpreter (example-block contents)
  "Interpret EXAMPLE-BLOCK element as Org syntax.
CONTENTS is nil."
  (let ((options (org-element-get-property :options example-block)))
    (concat "#+begin_example" (and options (concat " " options)) "\n"
	    (org-remove-indentation
	     (org-element-get-property :value example-block))
	    "#+end_example")))

;;;; Export Block
(defun org-element-export-block-parser ()
  "Parse an export block.

Return a list whose car is `export-block' and cdr is a plist
containing `:begin', `:end', `:type', `:hiddenp', `:value' and
`:post-blank' keywords."
  (save-excursion
    (end-of-line)
    (let* ((case-fold-search t)
	   (contents)
	   (type (progn (re-search-backward
			 (concat "[ \t]*#\\+begin_"
				 (org-re "\\([[:alnum:]]+\\)")))
			(downcase (org-match-string-no-properties 1))))
	   (keywords (org-element-collect-affiliated-keywords))
	   (begin (car keywords))
	   (contents-begin (progn (forward-line) (point)))
	   (hidden (org-truely-invisible-p))
	   (contents-end (progn (re-search-forward
				 (concat "^[ \t]*#\\+end_" type) nil t)
				(point-at-bol)))
	   (pos-before-blank (progn (forward-line) (point)))
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol))))
	   (value (buffer-substring-no-properties contents-begin contents-end)))
      (list 'export-block
	    `(:begin ,begin
		     :end ,end
		     :type ,type
		     :value ,value
		     :hiddenp ,hidden
		     :post-blank ,(count-lines pos-before-blank end)
		     ,@(cadr keywords))))))

(defun org-element-export-block-interpreter (export-block contents)
  "Interpret EXPORT-BLOCK element as Org syntax.
CONTENTS is nil."
  (let ((type (org-element-get-property :type export-block)))
    (concat (format "#+begin_%s\n" type)
	    (org-element-get-property :value export-block)
	    (format "#+end_%s" type))))

;;;; Fixed-width
(defun org-element-fixed-width-parser ()
  "Parse a fixed-width section.

Return a list whose car is `fixed-width' and cdr is a plist
containing `:begin', `:end', `:value' and `:post-blank'
keywords."
  (let ((fixed-re "[ \t]*:\\( \\|$\\)")
	beg-area begin end value pos-before-blank keywords)
    (save-excursion
      ;; Move to the beginning of the fixed-width area.
      (unless (bobp)
	(while (and (not (bobp)) (looking-at fixed-re))
	  (forward-line -1))
	(unless (looking-at fixed-re) (forward-line 1)))
      (setq beg-area (point))
      ;; Get affiliated keywords, if any.
      (setq keywords (org-element-collect-affiliated-keywords))
      ;; Store true beginning of element.
      (setq begin (car keywords))
      ;; Get ending of fixed-width area.  If point is in a list,
      ;; ensure to not get outside of it.
      (let* ((itemp (org-in-item-p))
	     (max-pos (if itemp
			  (org-list-get-bottom-point
			   (save-excursion (goto-char itemp) (org-list-struct)))
			(point-max))))
	(while (and (looking-at fixed-re) (< (point) max-pos))
	  (forward-line)))
      (setq pos-before-blank (point))
      ;; Find position after blank
      (org-skip-whitespace)
      (setq end (if (eobp) (point) (point-at-bol)))
      ;; Extract value.
      (setq value (buffer-substring-no-properties beg-area pos-before-blank)))
    (list 'fixed-width
	  `(:begin ,begin
		   :end ,end
		   :value ,value
		   :post-blank ,(count-lines pos-before-blank end)
		   ,@(cadr keywords)))))

(defun org-element-fixed-width-interpreter (fixed-width contents)
  "Interpret FIXED-WIDTH element as Org syntax.
CONTENTS is nil."
  (org-remove-indentation (org-element-get-property :value fixed-width)))

;;;; Horizontal Rule
(defun org-element-horizontal-rule-parser ()
  "Parse an horizontal rule.

   Return a list whose car is `horizontal-rule' and cdr is
   a plist containing `:begin', `:end' and `:post-blank'
   keywords."
  (save-excursion
    (let* ((keywords (org-element-collect-affiliated-keywords))
	   (begin (car keywords))
	   (post-hr (progn (forward-line) (point)))
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol)))))
      (list 'horizontal-rule
	    `(:begin ,begin
		     :end ,end
		     :post-blank ,(count-lines post-hr end)
		     ,@(cadr keywords))))))

(defun org-element-horizontal-rule-interpreter (horizontal-rule contents)
  "Interpret HORIZONTAL-RULE element as Org syntax.
CONTENTS is nil."
  "-----")

;;;; Keyword
(defun org-element-keyword-parser ()
  "Parse a keyword at point.

Return a list whose car is `keyword' and cdr is a plist
containing `:key', `:value', `:begin', `:end' and `:post-blank'
keywords."
  (save-excursion
    (let* ((begin (point))
	   (key (progn (looking-at
			"[ \t]*#\\+\\(\\(?:[a-z]+\\)\\(?:_[a-z]+\\)*\\):")
		       (org-match-string-no-properties 1)))
	   (value (org-trim (buffer-substring-no-properties
			     (match-end 0) (point-at-eol))))
	   (pos-before-blank (progn (forward-line) (point)))
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol)))))
      (list 'keyword
	    `(:key ,key
		   :value ,value
		   :begin ,begin
		   :end ,end
		   :post-blank ,(count-lines pos-before-blank end))))))

(defun org-element-keyword-interpreter (keyword contents)
  "Interpret KEYWORD element as Org syntax.
CONTENTS is nil."
  (format "#+%s: %s"
	  (org-element-get-property :key keyword)
	  (org-element-get-property :value keyword)))

;;;; Latex Environment
(defun org-element-latex-environment-parser ()
  "Parse a LaTeX environment.

Return a list whose car is `latex-environment' and cdr is a plist
containing `:begin', `:end', `:value' and `:post-blank' keywords."
  (save-excursion
    (end-of-line)
    (let* ((case-fold-search t)
	   (contents-begin (re-search-backward "^[ \t]*\\\\begin" nil t))
	   (keywords (org-element-collect-affiliated-keywords))
	   (begin (car keywords))
	   (contents-end (progn (re-search-forward "^[ \t]*\\\\end")
				(forward-line)
				(point)))
	   (value (buffer-substring-no-properties contents-begin contents-end))
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol)))))
      (list 'latex-environment
	    `(:begin ,begin
		     :end ,end
		     :value ,value
		     :post-blank ,(count-lines contents-end end)
		     ,@(cadr keywords))))))

(defun org-element-latex-environment-interpreter (latex-environment contents)
  "Interpret LATEX-ENVIRONMENT element as Org syntax.
CONTENTS is nil."
  (org-element-get-property :value latex-environment))

;;;; Paragraph
(defun org-element-paragraph-parser ()
  "Parse a paragraph.

Return a list whose car is `paragraph' and cdr is a plist
containing `:begin', `:end', `:contents-begin' and
`:contents-end' and `:post-blank' keywords.

Assume point is at the beginning of the paragraph."
  (save-excursion
    (let* ((contents-begin (point))
	   (keywords (org-element-collect-affiliated-keywords))
	   (begin (car keywords))
	   (contents-end (progn
			   (end-of-line)
			   (if (re-search-forward
				org-element-paragraph-separate nil 'm)
			       (progn (forward-line -1) (end-of-line) (point))
			     (point))))
	   (pos-before-blank (progn (forward-line) (point)))
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol)))))
      (list 'paragraph
	    `(:begin ,begin
		     :end ,end
		     :contents-begin ,contents-begin
		     :contents-end ,contents-end
		     :post-blank ,(count-lines pos-before-blank end)
		     ,@(cadr keywords))))))

(defun org-element-paragraph-interpreter (paragraph contents)
  "Interpret PARAGRAPH element as Org syntax.
CONTENTS is the contents of the element."
  contents)

;;;; Property Drawer
(defun org-element-property-drawer-parser ()
  "Parse a property drawer.

Return a list whose car is `property-drawer' and cdr is a plist
containing `:begin', `:end', `:hiddenp', `:contents-begin',
`:contents-end', `:properties' and `:post-blank' keywords."
  (save-excursion
    (let ((case-fold-search t)
	  (begin (progn (end-of-line)
			(re-search-backward org-property-start-re)
			(match-beginning 0)))
	  (contents-begin (progn (forward-line) (point)))
	  (hidden (org-truely-invisible-p))
	  (properties (let (val)
			(while (not (looking-at "^[ \t]*:END:"))
			  (when (looking-at
				 (org-re
				  "[ \t]*:\\([[:alpha:]][[:alnum:]_-]*\\):"))
			    (push (cons (match-string 1)
					(org-trim
					 (buffer-substring
					  (match-end 0) (point-at-eol))))
				  val))
			  (forward-line))
			val))
	  (contents-end (progn (re-search-forward "^[ \t]*:END:" nil t)
			       (point-at-bol)))
	  (pos-before-blank (progn (forward-line) (point)))
	  (end (progn (org-skip-whitespace)
		      (if (eobp) (point) (point-at-bol)))))
      (list 'property-drawer
	    `(:begin ,begin
		     :end ,end
		     :hiddenp ,hidden
		     :properties ,properties
		     :post-blank ,(count-lines pos-before-blank end))))))

(defun org-element-property-drawer-interpreter (property-drawer contents)
  "Interpret PROPERTY-DRAWER element as Org syntax.
CONTENTS is nil."
  (let ((props (org-element-get-property :properties property-drawer)))
    (concat
     ":PROPERTIES:\n"
     (mapconcat (lambda (p)
		  (format org-property-format (format ":%s:" (car p)) (cdr p)))
		(nreverse props) "\n")
     "\n:END:")))

;;;; Quote Section
(defun org-element-quote-section-parser ()
  "Parse a quote section.

Return a list whose car is `quote-section' and cdr is a plist
containing `:begin', `:end', `:value' and `:post-blank'
keywords."
  (save-excursion
    (let* ((begin (progn (org-back-to-heading t)
			 (forward-line)
			 (org-skip-whitespace)
			 (point-at-bol)))
	   (end (progn (org-with-limited-levels (outline-next-heading))
		       (point)))
	   (pos-before-blank (progn (skip-chars-backward " \r\t\n")
				    (forward-line)
				    (point)))
	   (value (unless (= begin end)
		    (buffer-substring-no-properties begin pos-before-blank))))
      (list 'quote-section
	    `(:begin ,begin
		     :end ,end
		     :value ,value
		     :post-blank ,(if value
				      (count-lines pos-before-blank end)
				    0))))))

(defun org-element-quote-section-interpreter (quote-section contents)
  "Interpret QUOTE-SECTION element as Org syntax.
CONTENTS is nil."
  (org-element-get-property :value quote-section))

;;;; Src Block
(defun org-element-src-block-parser ()
  "Parse a src block.

Return a list whose car is `src-block' and cdr is a plist
containing `:language', `:switches', `:parameters', `:begin',
`:end', `:hiddenp', `:contents-begin', `:contents-end', `:value'
and `:post-blank' keywords."
  (save-excursion
    (end-of-line)
    (let* ((case-fold-search t)
	   ;; Get position at beginning of block.
	   (contents-begin
	    (re-search-backward
	     (concat "^[ \t]*#\\+begin_src"
		     "\\(?: +\\(\\S-+\\)\\)?"	     ; language
		     "\\(\\(?: +[-+][A-Za-z]\\)*\\)" ; switches
		     "\\(.*\\)[ \t]*$")		     ; arguments
	     nil t))
	   ;; Get language as a string.
	   (language (org-match-string-no-properties 1))
	   ;; Get switches.
	   (switches (org-match-string-no-properties 2))
	   ;; Get parameters.
	   (parameters (org-trim (org-match-string-no-properties 3)))
	   ;; Get affiliated keywords.
	   (keywords (org-element-collect-affiliated-keywords))
	   ;; Get beginning position.
	   (begin (car keywords))
	   ;; Get position at end of block.
	   (contents-end (progn (re-search-forward "^[ \t]*#\\+end_src" nil t)
				(forward-line)
				(point)))
	   ;; Retrieve code.
	   (value (buffer-substring-no-properties
		   (save-excursion (goto-char contents-begin)
				   (forward-line)
				   (point))
		   (match-beginning 0)))
	   ;; Get position after ending blank lines.
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol))))
	   ;; Get visibility status.
	   (hidden (progn (goto-char contents-begin)
			     (forward-line)
			     (org-truely-invisible-p))))
      (list 'src-block
	    `(:language ,language
			:switches ,switches
			:parameters ,parameters
			:begin ,begin
			:end ,end
			:hiddenp ,hidden
			:value ,value
			:post-blank ,(count-lines contents-end end)
			,@(cadr keywords))))))

(defun org-element-src-block-interpreter (src-block contents)
  "Interpret SRC-BLOCK element as Org syntax.
CONTENTS is nil."
  (let ((lang (org-element-get-property :language src-block))
	(switches (org-element-get-property :switches src-block))
	(params (org-element-get-property :parameters src-block))
	(value (let ((val (org-element-get-property :value src-block)))
		 (cond
		  (org-src-preserve-indentation val)
		  ((zerop org-edit-src-content-indentation)
		   (org-remove-indentation val))
		  (t
		   (let ((ind (make-string
			       org-edit-src-content-indentation 32)))
		     (replace-regexp-in-string
		      "\\(^\\)[ \t]*\\S-" ind
		      (org-remove-indentation val) nil nil 1)))))))
    (concat (format "#+begin_src%s\n"
		    (concat (and lang (concat " " lang))
			    (and switches (concat " " switches))
			    (and params (concat " " params))))
	    value
	    "#+end_src")))

;;;; Table
(defun org-element-table-parser ()
  "Parse a table at point.

Return a list whose car is `table' and cdr is a plist containing
`:begin', `:end', `:contents-begin', `:contents-end', `:tblfm',
`:type', `:raw-table'  and `:post-blank' keywords."
  (save-excursion
    (let* ((table-begin (goto-char (org-table-begin t)))
	   (type (if (org-at-table.el-p) 'table.el 'org))
	   (keywords (org-element-collect-affiliated-keywords))
	   (begin (car keywords))
	   (table-end (goto-char (marker-position (org-table-end t))))
	   (tblfm (when (looking-at "[ \t]*#\\+tblfm: +\\(.*\\)[ \t]*")
		    (prog1 (org-match-string-no-properties 1)
		      (forward-line))))
	   (pos-before-blank (point))
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol))))
	   (raw-table (org-remove-indentation
		       (buffer-substring-no-properties table-begin table-end))))
      (list 'table
	    `(:begin ,begin
		     :end ,end
		     :type ,type
		     :raw-table ,raw-table
		     :tblfm ,tblfm
		     :post-blank ,(count-lines pos-before-blank end)
		     ,@(cadr keywords))))))

(defun org-element-table-interpreter (table contents)
  "Interpret TABLE element as Org syntax.
CONTENTS is nil."
  (org-element-get-property :raw-table table))

;;;; Verse Block
(defun org-element-verse-block-parser ()
  "Parse a verse block.

Return a list whose car is `verse-block' and cdr is a plist
containing `:begin', `:end', `:hiddenp', `:raw-value', `:value'
and `:post-blank' keywords.

Assume point is at beginning or end of the block."
  (save-excursion
    (let* ((case-fold-search t)
	   (keywords (progn
		       (end-of-line)
		       (re-search-backward
			(concat "^[ \t]*#\\+begin_verse") nil t)
		       (org-element-collect-affiliated-keywords)))
	   (begin (car keywords))
	   (hidden (progn (forward-line) (org-truely-invisible-p)))
	   (raw-val (buffer-substring-no-properties
		     (point)
		     (progn
		       (re-search-forward (concat "^[ \t]*#\\+end_verse") nil t)
		       (point-at-bol))))
	   (pos-before-blank (progn (forward-line) (point)))
	   (end (progn (org-skip-whitespace)
		       (if (eobp) (point) (point-at-bol))))
	   (value (org-element-parse-secondary-string
		   (org-remove-indentation raw-val)
		   (cdr (assq 'verse org-element-string-restrictions)))))
      (list 'verse-block
	    `(:begin ,begin
		     :end ,end
		     :hiddenp ,hidden
		     :raw-value ,raw-val
		     :value ,value
		     :post-blank ,(count-lines pos-before-blank end)
		     ,@(cadr keywords))))))


(defun org-element-verse-block-interpreter (verse-block contents)
  "Interpret VERSE-BLOCK element as Org syntax.
CONTENTS is nil."
  (format "#+begin_verse\n%s#+end_verse"
  (org-remove-indentation
  (org-element-get-property :raw-value verse-block))))



;;; Objects

;; Unlike to elements, interstices can be found between objects.
;; That's why, along with the parser, successor functions are provided
;; for each object.  Some objects share the same successor
;; (i.e. `emphasis' and `verbatim' objects).

;; A successor must accept a single argument bounding the search.  It
;; will return either a cons cell whose car is the object's type, as
;; a symbol, and cdr the position of its next occurrence, or nil.

;; Successors follow the naming convention:
;; org-element-NAME-successor, where NAME is the name of the
;; successor, as defined in `org-element-all-successors'.

;; Some object types (i.e `emphasis') are recursive.  Restrictions on
;; object types they can contain will be specified in
;; `org-element-object-restrictions'.

;; Adding a new type of object is simple.  Implement a successor,
;; a parser, and an interpreter for it, all following the naming
;; convention.  Register successor in `org-element-all-successors',
;; maybe tweak restrictions about it, and that's it.

;;;; Emphasis
(defun org-element-emphasis-parser ()
  "Parse text markup object at point.

Return a list whose car is `emphasis' and cdr is a plist with
`:marker', `:begin', `:end', `:contents-begin' and
`:contents-end' and `:post-blank' keywords.

Assume point is at the first emphasis marker."
  (save-excursion
    (unless (bolp) (backward-char 1))
    (looking-at org-emph-re)
    (let ((begin (match-beginning 2))
	  (marker (org-match-string-no-properties 3))
	  (contents-begin (match-beginning 4))
	  (contents-end (match-end 4))
	  (post-blank (progn (goto-char (match-end 2))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'emphasis
	    `(:marker ,marker
		      :begin ,begin
		      :end ,end
		      :contents-begin ,contents-begin
		      :contents-end ,contents-end
		      :post-blank ,post-blank)))))

(defun org-element-emphasis-interpreter (emphasis contents)
  "Interpret EMPHASIS object as Org syntax.
CONTENTS is the contents of the object."
  (let ((marker (org-element-get-property :marker emphasis)))
    (concat marker contents marker)))

(defun org-element-text-markup-successor (limit)
  "Search for the next emphasis or verbatim and return position.

LIMIT bounds the search.

Return value is a cons cell whose car is `emphasis' or
`verbatim' and cdr is beginning position."
  (save-excursion
    (unless (bolp) (backward-char))
    (when (re-search-forward org-emph-re limit t)
      (cons (if (nth 4 (assoc (match-string 3) org-emphasis-alist))
		'verbatim
	      'emphasis)
	    (match-beginning 2)))))

;;;; Entity
(defun org-element-entity-parser ()
  "Parse entity at point.

Return a list whose car is `entity' and cdr a plist with
`:begin', `:end', `:latex', `:latex-math-p', `:html', `:latin1',
`:utf-8', `:ascii', `:use-brackets-p' and `:post-blank' as
keywords.

Assume point is at the beginning of the entity."
  (save-excursion
    (looking-at "\\\\\\(frac[13][24]\\|[a-zA-Z]+\\)\\($\\|{}\\|[^[:alpha:]]\\)")
    (let* ((value (org-entity-get (match-string 1)))
	   (begin (match-beginning 0))
	   (bracketsp (string= (match-string 2) "{}"))
	   (post-blank (progn (goto-char (match-end 1))
			      (when bracketsp (forward-char 2))
			      (skip-chars-forward " \t")))
	   (end (point)))
      (list 'entity
	    `(:name ,(car value)
		    :latex ,(nth 1 value)
		    :latex-math-p ,(nth 2 value)
		    :html ,(nth 3 value)
		    :ascii ,(nth 4 value)
		    :latin1 ,(nth 5 value)
		    :utf-8 ,(nth 6 value)
		    :begin ,begin
		    :end ,end
		    :use-brackets-p ,bracketsp
		    :post-blank ,post-blank)))))

(defun org-element-entity-interpreter (entity contents)
  "Interpret ENTITY object as Org syntax.
CONTENTS is nil."
  (concat "\\"
	  (org-element-get-property :name entity)
	  (when (org-element-get-property :use-brackets-p entity) "{}")))

(defun org-element-latex-or-entity-successor (limit)
  "Search for the next latex-fragment or entity object.

LIMIT bounds the search.

Return value is a cons cell whose car is `entity' or
`latex-fragment' and cdr is beginning position."
  (save-excursion
    (let ((matchers (plist-get org-format-latex-options :matchers))
	  ;; ENTITY-RE matches both LaTeX commands and Org entities.
	  (entity-re
	   "\\\\\\(frac[13][24]\\|[a-zA-Z]+\\)\\($\\|[^[:alpha:]\n]\\)"))
      (when (re-search-forward
	     (concat (mapconcat (lambda (e) (nth 1 (assoc e org-latex-regexps)))
				matchers "\\|")
		     "\\|" entity-re)
	     limit t)
	(goto-char (match-beginning 0))
	(if (looking-at entity-re)
	    ;; Determine if it's a real entity or a LaTeX command.
	    (cons (if (org-entity-get (match-string 1)) 'entity 'latex-fragment)
		  (match-beginning 0))
	  ;; No entity nor command: point is at a LaTeX fragment.
	  ;; Determine its type to get the correct beginning position.
	  (cons 'latex-fragment
		(catch 'return
		  (mapc (lambda (e)
			  (when (looking-at (nth 1 (assoc e org-latex-regexps)))
			    (throw 'return
				   (match-beginning
				    (nth 2 (assoc e org-latex-regexps))))))
			matchers)
		  (point))))))))

;;;; Export Snippet
(defun org-element-export-snippet-parser ()
  "Parse export snippet at point.

Return a list whose car is `export-snippet' and cdr a plist with
`:begin', `:end', `:back-end', `:value' and `:post-blank' as
keywords.

Assume point is at the beginning of the snippet."
  (save-excursion
    (looking-at "@\\([-A-Za-z0-9]+\\){")
    (let* ((begin (point))
	   (back-end (org-match-string-no-properties 1))
	   (before-blank (progn (goto-char (scan-sexps (1- (match-end 0)) 1))))
	   (value (buffer-substring-no-properties
		   (match-end 0) (1- before-blank)))
	   (post-blank (skip-chars-forward " \t"))
	   (end (point)))
      (list 'export-snippet
	    `(:back-end ,back-end
			:value ,value
			:begin ,begin
			:end ,end
			:post-blank ,post-blank)))))

(defun org-element-export-snippet-interpreter (export-snippet contents)
  "Interpret EXPORT-SNIPPET object as Org syntax.
CONTENTS is nil."
  (format "@%s{%s}"
	  (org-element-get-property :back-end export-snippet)
	  (org-element-get-property :value export-snippet)))

(defun org-element-export-snippet-successor (limit)
  "Search for the next export-snippet object.

LIMIT bounds the search.

Return value is a cons cell whose car is `export-snippet' cdr is
its beginning position."
  (save-excursion
    (catch 'exit
      (while (re-search-forward "@[-A-Za-z0-9]+{" limit t)
	(when (let ((end (ignore-errors (scan-sexps (1- (point)) 1))))
		(and end (eq (char-before end) ?})))
	  (throw 'exit (cons 'export-snippet (match-beginning 0))))))))

;;;; Footnote Reference

(defun org-element-footnote-reference-parser ()
  "Parse footnote reference at point.

Return a list whose car is `footnote-reference' and cdr a plist
with `:label', `:type', `:definition', `:begin', `:end' and
`:post-blank' as keywords."
  (save-excursion
    (let* ((ref (org-footnote-at-reference-p))
	   (label (car ref))
	   (raw-def (nth 3 ref))
	   (inline-def (and raw-def
			    (org-element-parse-secondary-string raw-def nil)))
	   (type (if (nth 3 ref) 'inline 'standard))
	   (begin (nth 1 ref))
	   (post-blank (progn (goto-char (nth 2 ref))
			      (skip-chars-forward " \t")))
	   (end (point)))
      (list 'footnote-reference
	    `(:label ,label
		     :type ,type
		     :inline-definition ,inline-def
		     :begin ,begin
		     :end ,end
		     :post-blank ,post-blank
		     :raw-definition ,raw-def)))))

(defun org-element-footnote-reference-interpreter (footnote-reference contents)
  "Interpret FOOTNOTE-REFERENCE object as Org syntax.
CONTENTS is nil."
  (let ((label (or (org-element-get-property :label footnote-reference)
		   "fn:"))
	(def (let ((raw (org-element-get-property
			 :raw-definition footnote-reference)))
	       (if raw (concat ":" raw) ""))))
    (format "[%s]" (concat label def))))

(defun org-element-footnote-reference-successor (limit)
  "Search for the next footnote-reference and return beginning
  position.

LIMIT bounds the search.

Return value is a cons cell whose car is `footnote-reference' and
cdr is beginning position."
  (let (fn-ref)
     (when (setq fn-ref (org-footnote-get-next-reference nil nil limit))
       (cons 'footnote-reference (nth 1 fn-ref)))))


;;;; Inline Babel Call
(defun org-element-inline-babel-call-parser ()
  "Parse inline babel call at point.

Return a list whose car is `inline-babel-call' and cdr a plist with
`:begin', `:end', `:info' and `:post-blank' as keywords.

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
	    `(:begin ,begin
		     :end ,end
		     :info ,info
		     :post-blank ,post-blank)))))

(defun org-element-inline-babel-call-interpreter (inline-babel-call contents)
  "Interpret INLINE-BABEL-CALL object as Org syntax.
CONTENTS is nil."
  (let* ((babel-info (org-element-get-property :info inline-babel-call))
	 (main-source (car babel-info))
	 (post-options (nth 1 babel-info)))
    (concat "call_"
	    (if (string-match "\\[\\(\\[.*?\\]\\)\\]" main-source)
		;; Remove redundant square brackets.
		(replace-match
		 (match-string 1 main-source) nil nil main-source)
	      main-source)
	    (and post-options (format "[%s]" post-options)))))

(defun org-element-inline-babel-call-successor (limit)
  "Search for the next inline-babel-call and return beginning
  position.

LIMIT bounds the search.

Return value is a cons cell whose car is `inline-babel-call' and
cdr is beginning position."
  (save-excursion
    ;; Use a simplified version of
    ;; org-babel-inline-lob-one-liner-regexp as regexp for more speed.
    (when (re-search-forward
	   "\\(?:babel\\|call\\)_\\([^()\n]+?\\)\\(\\[\\(.*\\)\\]\\|\\(\\)\\)(\\([^\n]*\\))\\(\\[\\(.*?\\)\\]\\)?"
	   limit t)
      (cons 'inline-babel-call (match-beginning 0)))))

;;;; Inline Src Block
(defun org-element-inline-src-block-parser ()
  "Parse inline source block at point.

Return a list whose car is `inline-src-block' and cdr a plist
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
	    `(:language ,language
			:value ,value
			:parameters ,parameters
			:begin ,begin
			:end ,end
			:post-blank ,post-blank)))))



(defun org-element-inline-src-block-successor (limit)
  "Search for the next inline-babel-call and return beginning position.

LIMIT bounds the search.

Return value is a cons cell whose car is `inline-babel-call' and
cdr is beginning position."
  (save-excursion
    (when (re-search-forward org-babel-inline-src-block-regexp limit t)
      (cons 'inline-src-block (match-beginning 1)))))

;;;; Latex Fragment
(defun org-element-latex-fragment-parser ()
  "Parse latex fragment at point.

Return a list whose car is `latex-fragment' and cdr a plist with
`:value', `:begin', `:end', and `:post-blank' as keywords.

Assume point is at the beginning of the latex fragment."
  (save-excursion
    (let* ((begin (point))
	   (substring-match
	    (catch 'exit
	      (mapc (lambda (e)
		      (let ((latex-regexp (nth 1 (assoc e org-latex-regexps))))
			(when (or (looking-at latex-regexp)
				  (and (not (bobp))
				       (save-excursion
					 (backward-char)
					 (looking-at latex-regexp))))
			  (throw 'exit (nth 2 (assoc e org-latex-regexps))))))
		    (plist-get org-format-latex-options :matchers))
	      ;; None found: it's a macro.
	      (looking-at "\\\\[a-zA-Z]+\\*?\\(\\(\\[[^][\n{}]*\\]\\)\\|\\({[^{}\n]*}\\)\\)*")
	      0))
	   (value (match-string-no-properties substring-match))
	   (post-blank (progn (goto-char (match-end substring-match))
			      (skip-chars-forward " \t")))
	   (end (point)))
      (list 'latex-fragment
	    `(:value ,value
		     :begin ,begin
		     :end ,end
		     :post-blank ,post-blank)))))

(defun org-element-latex-fragment-interpreter (latex-fragment contents)
  "Interpret LATEX-FRAGMENT object as Org syntax.
CONTENTS is nil."
  (org-element-get-property :value latex-fragment))

;;;; Line Break
(defun org-element-line-break-parser ()
  "Parse line break at point.

Return a list whose car is `line-break', and cdr a plist with
`:begin', `:end' and `:post-blank' keywords.

Assume point is at the beginning of the line break."
  (save-excursion
    (let* ((begin (point))
	   (end (progn (end-of-line) (point)))
	   (post-blank (- (skip-chars-backward " \t")))
	   (end (point)))
      (list 'line-break
	    `(:begin ,begin
		     :end ,end
		     :post-blank ,post-blank)))))

(defun org-element-line-break-interpreter (line-break contents)
  "Interpret LINE-BREAK object as Org syntax.
CONTENTS is nil."
  (org-element-get-property :value line-break))

(defun org-element-line-break-successor (limit)
  "Search for the next statistics cookie and return position.

LIMIT bounds the search.

Return value is a cons cell whose car is `line-break' and cdr is
beginning position."
  (save-excursion
    (let ((beg (and (re-search-forward "[^\\\\]\\(\\\\\\\\\\)[ \t]*$" limit t)
		    (goto-char (match-beginning 1)))))
      ;; A line break can only happen on a non-empty line.
      (when (and beg (re-search-backward "\\S-" (point-at-bol) t))
	(cons 'line-break beg)))))

;;;; Link
(defun org-element-link-parser ()
  "Parse link at point.

Return a list whose car is `link' and cdr a plist with `:type',
`:path', `:raw-link', `:begin', `:end', `:contents-begin',
`:contents-end' and `:post-blank' as keywords.

Assume point is at the beginning of the link."
  (save-excursion
    (let ((begin (point))
	  end contents-begin contents-end link-end post-blank path type
	  raw-link link)
      (cond
       ;; Type 1: text targeted from a radio target.
       ((and org-target-link-regexp (looking-at org-target-link-regexp))
	(setq type "radio"
	      path (org-match-string-no-properties 0)
	      contents-begin (match-beginning 0)
	      contents-end (match-end 0)
	      link-end (match-end 0)))
       ;; Type 2: Standard link, i.e. [[http://orgmode.org][homepage]]
       ((looking-at org-bracket-link-regexp)
	(setq contents-begin (match-beginning 3)
	      contents-end (match-end 3)
	      link-end (match-end 0)
	      ;; RAW-LINK is the original link.
	      raw-link (org-match-string-no-properties 1)
	      link (org-link-expand-abbrev
		    (replace-regexp-in-string
		     " *\n *" " " (org-link-unescape raw-link) t t)))
	;; Determine TYPE of link and set PATH accordingly.
	(cond
	 ;; File type.
	 ((or (file-name-absolute-p link) (string-match "^\\.\\.?/" link))
	  (setq type "file" path link))
	 ;; Explicit type (http, irc, bbdb...).  See `org-link-types'.
	 ((string-match org-link-re-with-space3 link)
	  (setq type (match-string 1 link) path (match-string 2 link)))
	 ;; Id type: PATH is the id.
	 ((string-match "^id:\\([-a-f0-9]+\\)" link)
	  (setq type "id" path (match-string 1 link)))
	 ;; Code-ref type: PATH is the name of the reference.
	 ((string-match "^(\\(.*\\))$" link)
	  (setq type "coderef" path (match-string 1 link)))
	 ;; Custom-id type: PATH is the name of the custom id.
	 ((= (aref link 0) ?#)
	  (setq type "custom-id" path (substring link 1)))
	 ;; Fuzzy type: Internal link either matches a target, an
	 ;; headline name or nothing. PATH is the target or headline's
	 ;; name.
	 (t (setq type "fuzzy" path link))))
       ;; Type 3: Plain link, i.e. http://orgmode.org
       ((looking-at org-plain-link-re)
	(setq raw-link (org-match-string-no-properties 0)
	      type (org-match-string-no-properties 1)
	      path (org-match-string-no-properties 2)
	      link-end (match-end 0)))
       ;; Type 4: Angular link, i.e. <http://orgmode.org>
       ((looking-at org-angle-link-re)
	(setq raw-link (buffer-substring-no-properties
			(match-beginning 1) (match-end 2))
	      type (org-match-string-no-properties 1)
	      path (org-match-string-no-properties 2)
	      link-end (match-end 0))))
      ;; In any case, deduce end point after trailing white space from
      ;; LINK-END variable.
      (setq post-blank (progn (goto-char link-end) (skip-chars-forward " \t"))
	    end (point))
      (list 'link
	    `(:type ,type
		    :path ,path
		    :raw-link ,(or raw-link path)
		    :begin ,begin
		    :end ,end
		    :contents-begin ,contents-begin
		    :contents-end ,contents-end
		    :post-blank ,post-blank)))))

(defun org-element-link-interpreter (link contents)
  "Interpret LINK object as Org syntax.
CONTENTS is the contents of the object."
  (let ((type (org-element-get-property :type link))
	(raw-link (org-element-get-property :raw-link link)))
    (cond
     ((string= type "radio") raw-link)
     (t (format "[[%s]%s]"
		raw-link
		(if (string= contents "") "" (format "[%s]" contents)))))))

(defun org-element-link-successor (limit)
  "Search for the next link and return position.

LIMIT bounds the search.

Return value is a cons cell whose car is `link' and cdr is
beginning position."
  (save-excursion
    (let ((link-regexp
	   (if org-target-link-regexp
	       (concat org-any-link-re "\\|" org-target-link-regexp)
	     org-any-link-re)))
      (when (re-search-forward link-regexp limit t)
	(cons 'link (match-beginning 0))))))

;;;; Macro
(defun org-element-macro-parser ()
  "Parse macro at point.

Return a list whose car is `macro' and cdr a plist with `:key',
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
	  (args (let ((args (org-match-string-no-properties 3)) args2)
		  (when args
		    (setq args (org-split-string args ","))
		    (while args
		      (while (string-match "\\\\\\'" (car args))
			;; Repair bad splits.
			(setcar (cdr args) (concat (substring (car args) 0 -1)
						   "," (nth 1 args)))
			(pop args))
		      (push (pop args) args2))
		    (mapcar 'org-trim (nreverse args2))))))
      (list 'macro
	    `(:key ,key
		   :value ,value
		   :args ,args
		   :begin ,begin
		   :end ,end
		   :post-blank ,post-blank)))))

(defun org-element-macro-interpreter (macro contents)
  "Interpret MACRO object as Org syntax.
CONTENTS is nil."
  (org-element-get-property :value macro))

(defun org-element-macro-successor (limit)
  "Search for the next macro and return position.

LIMIT bounds the search.

Return value is cons cell whose car is `macro' and cdr is
beginning position."
  (save-excursion
    (when (re-search-forward
	   "{{{\\([a-zA-Z][-a-zA-Z0-9_]*\\)\\(([ \t\n]*\\([^\000]*?\\))\\)?}}}"
	   limit t)
      (cons 'macro (match-beginning 0)))))

;;;; Radio-target
(defun org-element-radio-target-parser ()
  "Parse radio target at point.

Return a list whose car is `radio-target' and cdr a plist with
`:begin', `:end', `:contents-begin', `:contents-end', `raw-value'
and `:post-blank' as keywords.

Assume point is at the radio target."
  (save-excursion
    (looking-at org-radio-target-regexp)
    (let ((begin (point))
	  (contents-begin (match-beginning 1))
	  (contents-end (match-end 1))
	  (raw-value (org-match-string-no-properties 1))
	  (post-blank (progn (goto-char (match-end 0))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'radio-target
	    `(:begin ,begin
		     :end ,end
		     :contents-begin ,contents-begin
		     :contents-end ,contents-end
		     :raw-value ,raw-value
		     :post-blank ,post-blank)))))

(defun org-element-radio-target-interpreter (target contents)
  "Interpret TARGET object as Org syntax.
CONTENTS is the contents of the object."
  (concat ">"))

(defun org-element-radio-target-successor (limit)
  "Search for the next radio-target and return position.

LIMIT bounds the search.

Return value is a cons cell whose car is `radio-target' and cdr
is beginning position."
  (save-excursion
     (when (re-search-forward org-radio-target-regexp limit t)
       (cons 'radio-target (match-beginning 0)))))

;;;; Statistics Cookie
(defun org-element-statistics-cookie-parser ()
  "Parse statistics cookie at point.

Return a list whose car is `statistics-cookie', and cdr a plist
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
	    `(:begin ,begin
		     :end ,end
		     :value ,value
		     :post-blank ,post-blank)))))

(defun org-element-statistics-cookie-interpreter (statistics-cookie contents)
  "Interpret STATISTICS-COOKIE object as Org syntax.
CONTENTS is nil."
  (org-element-get-property :value statistics-cookie))

(defun org-element-statistics-cookie-successor (limit)
  "Search for the next statistics cookie and return position.

LIMIT bounds the search.

Return value is a cons cell whose car is `statistics-cookie' and
cdr is beginning position."
  (save-excursion
    (when (re-search-forward "\\[[0-9]*\\(%\\|/[0-9]*\\)\\]" limit t)
      (cons 'statistics-cookie (match-beginning 0)))))

;;;; Subscript
(defun org-element-subscript-parser ()
  "Parse subscript at point.

Return a list whose car is `subscript' and cdr a plist with
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
	    `(:begin ,begin
		     :end ,end
		     :use-brackets-p ,bracketsp
		     :contents-begin ,contents-begin
		     :contents-end ,contents-end
		     :post-blank ,post-blank)))))

(defun org-element-subscript-interpreter (subscript contents)
  "Interpret SUBSCRIPT object as Org syntax.
CONTENTS is the contents of the object."
  (format
   (if (org-element-get-property :use-brackets-p subscript) "_{%s}" "_%s")
   contents))

(defun org-element-sub/superscript-successor  (limit)
  "Search for the next sub/superscript and return beginning
position.

LIMIT bounds the search.

Return value is a cons cell whose car is either `subscript' or
`superscript' and cdr is beginning position."
  (save-excursion
    (when (re-search-forward org-match-substring-regexp limit t)
      (cons (if (string= (match-string 2) "_") 'subscript 'superscript)
	    (match-beginning 2)))))

;;;; Superscript
(defun org-element-superscript-parser ()
  "Parse superscript at point.

Return a list whose car is `superscript' and cdr a plist with
`:begin', `:end', `:contents-begin', `:contents-end',
`:use-brackets-p' and `:post-blank' as keywords.

Assume point is at the caret."
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
      (list 'superscript
	    `(:begin ,begin
		     :end ,end
		     :use-brackets-p ,bracketsp
		     :contents-begin ,contents-begin
		     :contents-end ,contents-end
		     :post-blank ,post-blank)))))

(defun org-element-superscript-interpreter (superscript contents)
  "Interpret SUPERSCRIPT object as Org syntax.
CONTENTS is the contents of the object."
  (format
   (if (org-element-get-property :use-brackets-p superscript) "^{%s}" "^%s")
   contents))

;;;; Target
(defun org-element-target-parser ()
  "Parse target at point.

Return a list whose car is `target' and cdr a plist with
`:begin', `:end', `:contents-begin', `:contents-end', `raw-value'
and `:post-blank' as keywords.

Assume point is at the target."
  (save-excursion
    (looking-at org-target-regexp)
    (let ((begin (point))
	  (contents-begin (match-beginning 1))
	  (contents-end (match-end 1))
	  (raw-value (org-match-string-no-properties 1))
	  (post-blank (progn (goto-char (match-end 0))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'target
	    `(:begin ,begin
		     :end ,end
		     :contents-begin ,contents-begin
		     :contents-end ,contents-end
		     :raw-value ,raw-value
		     :post-blank ,post-blank)))))

(defun org-element-target-interpreter (target contents)
  "Interpret TARGET object as Org syntax.
CONTENTS is the contents of target."
  (concat ""))

(defun org-element-target-successor (limit)
  "Search for the next target and return position.

LIMIT bounds the search.

Return value is a cons cell whose car is `target' and cdr is
beginning position."
  (save-excursion
     (when (re-search-forward org-target-regexp limit t)
       (cons 'target (match-beginning 0)))))

;;;; Time-stamp
(defun org-element-time-stamp-parser ()
  "Parse time stamp at point.

Return a list whose car is `time-stamp', and cdr a plist with
`:appt-type', `:type', `:begin', `:end', `:value' and
`:post-blank' keywords.

Assume point is at the beginning of the time-stamp."
  (save-excursion
    (let* ((appt-type (cond
		       ((looking-at (concat org-deadline-string " +"))
			(goto-char (match-end 0))
			'deadline)
		       ((looking-at (concat org-scheduled-string " +"))
			(goto-char (match-end 0))
			'scheduled)
		       ((looking-at (concat org-closed-string " +"))
			(goto-char (match-end 0))
			'closed)))
	   (begin (and appt-type (match-beginning 0)))
	   (type (cond
		  ((looking-at org-tsr-regexp)
		   (if (match-string 2) 'active-range 'active))
		  ((looking-at org-tsr-regexp-both)
		   (if (match-string 2) 'inactive-range 'inactive))
		  ((looking-at (concat
				"\\(<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+[dwmy]>\\)"
				"\\|"
				"\\(<%%\\(([^>\n]+)\\)>\\)"))
		   'diary)))
	   (begin (or begin (match-beginning 0)))
	   (value (buffer-substring-no-properties
		   (match-beginning 0) (match-end 0)))
	   (post-blank (progn (goto-char (match-end 0))
			      (skip-chars-forward " \t")))
	   (end (point)))
      (list 'time-stamp
	    `(:appt-type ,appt-type
			 :type ,type
			 :value ,value
			 :begin ,begin
			 :end ,end
			 :post-blank ,post-blank)))))

(defun org-element-time-stamp-interpreter (time-stamp contents)
  "Interpret TIME-STAMP object as Org syntax.
CONTENTS is nil."
  (concat
   (case (org-element-get-property :appt-type time-stamp)
     (closed (concat org-closed-string " "))
     (deadline (concat org-deadline-string " "))
     (scheduled (concat org-scheduled-string " ")))
   (org-element-get-property :value time-stamp)))

(defun org-element-time-stamp-successor (limit)
  "Search for the next time-stamp and return position.

LIMIT bounds the search.

Return value is a cons cell whose car is `time-stamp' and cdr is
beginning position."
  (save-excursion
    (when (re-search-forward
	   (concat "\\(?:" org-scheduled-string " +\\|"
		   org-deadline-string " +\\|" org-closed-string " +\\)?"
		   org-ts-regexp-both
		   "\\|"
		   "\\(?:<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+[dwmy]>\\)"
		   "\\|"
		   "\\(?:<%%\\(?:([^>\n]+)\\)>\\)")
	   limit t)
      (cons 'time-stamp (match-beginning 0)))))

;;;; Verbatim
(defun org-element-verbatim-parser ()
  "Parse verbatim object at point.

Return a list whose car is `verbatim' and cdr is a plist with
`:marker', `:begin', `:end' and `:post-blank' keywords.

Assume point is at the first verbatim marker."
  (save-excursion
    (unless (bolp) (backward-char 1))
    (looking-at org-emph-re)
    (let ((begin (match-beginning 2))
	  (marker (org-match-string-no-properties 3))
	  (value (org-match-string-no-properties 4))
	  (post-blank (progn (goto-char (match-end 2))
			     (skip-chars-forward " \t")))
	  (end (point)))
      (list 'verbatim
	    `(:marker ,marker
		      :begin ,begin
		      :end ,end
		      :value ,value
		      :post-blank ,post-blank)))))

(defun org-element-verbatim-interpreter (verbatim contents)
  "Interpret VERBATIM object as Org syntax.
CONTENTS is nil."
  (let ((marker (org-element-get-property :marker verbatim))
	(value (org-element-get-property :value verbatim)))
    (concat marker value marker)))



;;; Definitions And Rules

;; Define elements, greater elements and specify recursive objects,
;; along with the affiliated keywords recognized.  Also set up
;; restrictions on recursive objects combinations.

;; These variables really act as a control center for the parsing
;; process.
(defconst org-element-paragraph-separate
  (concat "\f" "\\|" "^[ \t]*$" "\\|"
	  ;; Headlines and inlinetasks.
	  org-outline-regexp-bol "\\|"
	  ;; Comments, blocks (any type), keywords and babel calls.
	  "^[ \t]*#\\+" "\\|" "^#\\( \\|$\\)" "\\|"
	  ;; Lists.
	  (org-item-beginning-re) "\\|"
	  ;; Fixed-width, drawers (any type) and tables.
	  "^[ \t]*[:|]" "\\|"
	  ;; Footnote definitions.
	  org-footnote-definition-re "\\|"
	  ;; Horizontal rules.
	  "^[ \t]*-\\{5,\\}[ \t]*$" "\\|"
	  ;; LaTeX environments.
	  "^[ \t]*\\\\\\(begin\\|end\\)")
  "Regexp to separate paragraphs in an Org buffer.")

(defconst org-element-all-elements
  '(center-block comment comment-block drawer dynamic-block example-block
		 export-block fixed-width footnote-definition headline
		 horizontal-rule inlinetask item keyword latex-environment
		 babel-call paragraph plain-list property-drawer quote-block
		 quote-section special-block src-block table verse-block)
  "Complete list of elements.")

(defconst org-element-greater-elements
  '(center-block drawer dynamic-block footnote-definition headline inlinetask
		 item plain-list quote-block special-block)
  "List of recursive element types aka Greater Elements.")

(defconst org-element-all-successors
  '(export-snippet footnote-reference inline-babel-call inline-src-block
		   latex-or-entity line-break link macro radio-target
		   statistics-cookie sub/superscript target text-markup
		   time-stamp)
  "Complete list of successors.")

(defconst org-element-object-successor-alist
  '((subscript . sub/superscript) (superscript . sub/superscript)
    (emphasis . text-markup) (verbatim . text-markup)
    (entity . latex-or-entity) (latex-fragment . latex-or-entity))
  "Alist of translations between object type and successor name.

Sharing the same successor comes handy when, for example, the
regexp matching one object can also match the other object.")

(defconst org-element-recursive-objects
  '(emphasis link subscript superscript target radio-target)
  "List of recursive object types.")

(defconst org-element-non-recursive-block-alist
  '(("ascii" . export-block)
    ("comment" . comment-block)
    ("docbook" . export-block)
    ("example" . example-block)
    ("html" . export-block)
    ("latex" . latex-block)
    ("odt" . export-block)
    ("src" . src-block)
    ("verse" . verse-block))
  "Alist between non-recursive block name and their element type.")

(defconst org-element-affiliated-keywords
  '("attr_ascii" "attr_docbook" "attr_html" "attr_latex" "attr_odt" "caption"
    "data" "header" "headers" "label" "name" "plot" "resname" "result" "results"
    "source" "srcname" "tblname")
  "List of affiliated keywords as strings.")

(defconst org-element-keyword-translation-alist
  '(("data" . "name")  ("label" . "name") ("resname" . "name")
    ("source" . "name") ("srcname" . "name") ("tblname" . "name")
    ("result" . "results") ("headers" . "header"))
  "Alist of usual translations for keywords.
The key is the old name and the value the new one.  The property
holding their value will be named after the translated name.")

(defconst org-element-multiple-keywords
  '("attr_ascii" "attr_docbook" "attr_html" "attr_latex" "attr_odt" "header")
  "List of affiliated keywords that can occur more that once in an element.

Their value will be consed into a list of strings, which will be
returned as the value of the property.

This list is checked after translations have been applied.  See
`org-element-keyword-translation-alist'.")

(defconst org-element-parsed-keywords '("author" "caption" "title")
  "List of keywords whose value can be parsed.

Their value will be stored as a secondary string: a list of
strings and objects.

This list is checked after translations have been applied.  See
`org-element-keyword-translation-alist'.")

(defconst org-element-dual-keywords '("results")
  "List of keywords which can have a secondary value.

In Org syntax, they can be written with optional square brackets
before the colons.  For example, results keyword can be
associated to a hash value with the following:

  #+results[hash-string]: some-source

This list is checked after translations have been applied.  See
`org-element-keyword-translation-alist'.")

(defconst org-element-object-restrictions
  '((emphasis entity export-snippet inline-babel-call inline-src-block
	      radio-target sub/superscript target text-markup time-stamp)
    (link entity export-snippet inline-babel-call inline-src-block
	  latex-fragment sub/superscript text-markup)
    (radio-target entity export-snippet latex-fragment sub/superscript)
    (subscript entity export-snippet inline-babel-call inline-src-block
	       latex-fragment sub/superscript text-markup)
    (superscript entity export-snippet inline-babel-call inline-src-block
		 latex-fragment sub/superscript text-markup)
    (target entity export-snippet latex-fragment sub/superscript text-markup))
  "Alist of recursive objects restrictions.

Car is a recursive object type and cdr is a list of successors
that will be called within an object of such type.

For example, in a `radio-target' object, one can only find
entities, export snippets, latex-fragments, subscript and
superscript.")

(defconst org-element-string-restrictions
  '((headline entity inline-babel-call latex-fragment link macro radio-target
	      statistics-cookie sub/superscript text-markup time-stamp)
    (inlinetask entity inline-babel-call latex-fragment link macro radio-target
		sub/superscript text-markup time-stamp)
    (item entity inline-babel-call latex-fragment macro radio-target
	  sub/superscript target verbatim)
    (keyword entity latex-fragment macro sub/superscript text-markup)
    (table entity latex-fragment macro text-markup)
    (verse entity footnote-reference inline-babel-call inline-src-block
	   latex-fragment line-break link macro radio-target sub/superscript
	   target text-markup time-stamp))
  "Alist of secondary strings restrictions.

When parsed, some elements have a secondary string which could
contain various objects (i.e. headline's name, or table's cells).
For association, the car is the element type, and the cdr a list
of successors that will be called in that secondary string.

Note: `keyword' secondary string type only applies to keywords
matching `org-element-parsed-keywords'.")



;;; Accessors
;;
;; Provide two accessors: `org-element-get-property' and
;; `org-element-get-contents'.
(defun org-element-get-property (property element)
  "Extract the value from the PROPERTY of an ELEMENT."
  (plist-get (nth 1 element) property))

(defun org-element-get-contents (element)
  "Extract contents from an ELEMENT."
  (nthcdr 2 element))



;; Obtaining The Smallest Element Containing Point

;; `org-element-at-point' is the core function of this section.  It
;; returns the Lisp representation of the element at point.  It uses
;; `org-element-guess-type' and `org-element-skip-keywords' as helper
;; functions.

;; When point is at an item, there is no automatic way to determine if
;; the function should return the `plain-list' element, or the
;; corresponding `item' element.  By default, `org-element-at-point'
;; works at the `plain-list' level.  But, by providing an optional
;; argument, one can make it switch to the `item' level.
(defconst org-element--affiliated-re
  (format "[ \t]*#\\+\\(%s\\):"
	  (mapconcat
	   (lambda (keyword)
	     (if (member keyword org-element-dual-keywords)
		 (format "\\(%s\\)\\(?:\\[\\(.*?\\)\\]\\)?"
			 (regexp-quote keyword))
	       (regexp-quote keyword)))
	   org-element-affiliated-keywords "\\|"))
  "Regexp matching any affiliated keyword.

Keyword name is put in match group 1.  Moreover, if keyword
belongs to `org-element-dual-keywords', put the dual value in
match group 2.

Don't modify it, set `org-element--affiliated-keywords' instead.")

(defun org-element-at-point (&optional toggle-item structure)
  "Determine closest element around point.

Return value is a list \(TYPE PROPS\) where TYPE is the type of
the element and PROPS a plist of properties associated to the
element.

Possible types are defined in `org-element-all-elements'.

If optional argument TOGGLE-ITEM is non-nil, parse item wise
instead of plain-list wise, using STRUCTURE as the current list
structure.

If STRUCTURE isn't provided but TOGGLE-ITEM is non-nil, it will
be computed."
  (save-excursion
    (beginning-of-line)
    ;; Move before any blank line.
    (when (looking-at "[ \t]*$")
      (skip-chars-backward " \r\t\n")
      (beginning-of-line))
    (let ((case-fold-search t))
      ;; Check if point is at an affiliated keyword.  In that case,
      ;; try moving to the beginning of the associated element.  If
      ;; the keyword is orphaned, treat it as plain text.
      (when (looking-at org-element--affiliated-re)
	(let ((opoint (point)))
	  (while (looking-at org-element--affiliated-re) (forward-line))
	  (when (looking-at "[ \t]*$") (goto-char opoint))))
      (let ((type (org-element-guess-type)))
	(cond
	 ;; Guessing element type on the current line is impossible:
	 ;; try to find the beginning of the current element to get
	 ;; more information.
	 ((not type)
	  (let ((search-origin (point))
		(opoint-in-item-p (org-in-item-p))
		(par-found-p
		 (progn
		   (end-of-line)
		   (re-search-backward org-element-paragraph-separate nil 'm))))
	    (cond
	     ;; Unable to find a paragraph delimiter above: we're at
	     ;; bob and looking at a paragraph.
	     ((not par-found-p) (org-element-paragraph-parser))
	     ;; Trying to find element's beginning set point back to
	     ;; its original position.  There's something peculiar on
	     ;; this line that prevents parsing, probably an
	     ;; ill-formed keyword or an undefined drawer name.  Parse
	     ;; it as plain text anyway.
	     ((< search-origin (point-at-eol)) (org-element-paragraph-parser))
	     ;; Original point wasn't in a list but previous paragraph
	     ;; is.  It means that either point was inside some block,
	     ;; or current list was ended without using a blank line.
	     ;; In the last case, paragraph really starts at list end.
	     ((let (item)
		(and (not opoint-in-item-p)
		     (not (looking-at "[ \t]*#\\+begin"))
		     (setq item (org-in-item-p))
		     (let ((struct (save-excursion (goto-char item)
						   (org-list-struct))))
		       (goto-char (org-list-get-bottom-point struct))
		       (org-skip-whitespace)
		       (beginning-of-line)
		       (org-element-paragraph-parser)))))
	     ((org-footnote-at-definition-p)
	      (org-element-footnote-definition-parser))
	     ((and opoint-in-item-p (org-at-item-p) (= opoint-in-item-p (point)))
	      (if toggle-item
		  (org-element-item-parser (or structure (org-list-struct)))
		(org-element-plain-list-parser (or structure (org-list-struct)))))
	     ;; In any other case, the paragraph started the line
	     ;; below.
	     (t (forward-line) (org-element-paragraph-parser)))))
	 ((eq type 'plain-list)
	  (if toggle-item
	      (org-element-item-parser (or structure (org-list-struct)))
	    (org-element-plain-list-parser (or structure (org-list-struct)))))
	 ;; Straightforward case: call the appropriate parser.
	 (t (funcall (intern (format "org-element-%s-parser" type)))))))))


;; It is obvious to tell if point is in most elements, either by
;; looking for a specific regexp in the current line, or by using
;; already implemented functions.  This is the goal of
;; `org-element-guess-type'.
(defconst org-element--element-block-types
  (mapcar 'car org-element-non-recursive-block-alist)
  "List of non-recursive block types, as strings.
Used internally by `org-element-guess-type'.  Do not modify it
directly, set `org-element-non-recursive-block-alist' instead.")

(defun org-element-guess-type ()
  "Return the type of element at point, or nil if undetermined.
This function may move point to an appropriate position for
parsing.  Used internally by `org-element-at-point'."
  ;; Beware: Order matters for some cases in that function.
  (beginning-of-line)
  (let ((case-fold-search t))
    (cond
     ((org-with-limited-levels (org-at-heading-p)) 'headline)
     ((let ((headline (ignore-errors (nth 4 (org-heading-components)))))
	(and headline
	     (let (case-fold-search)
	       (string-match (format "^%s\\(?: \\|$\\)" org-quote-string)
			     headline))))
      'quote-section)
     ;; Non-recursive block.
     ((let ((type (org-in-block-p org-element--element-block-types)))
	(and type (cdr (assoc type org-element-non-recursive-block-alist)))))
     ((org-at-heading-p) 'inlinetask)
     ((org-between-regexps-p
       "^[ \t]*\\\\begin{" "^[ \t]*\\\\end{[^}]*}[ \t]*") 'latex-environment)
     ;; Property drawer.  Almost `org-at-property-p', but allow drawer
     ;; boundaries.
     ((org-with-wide-buffer
       (and (not (org-before-first-heading-p))
	    (let ((pblock (org-get-property-block)))
	      (and pblock
		   (<= (point) (cdr pblock))
		   (>= (point-at-eol) (1- (car pblock)))))))
      'property-drawer)
     ;; Recursive block. If the block isn't complete, parse the
     ;; current part as a paragraph.
     ((looking-at "[ \t]*#\\+\\(begin\\|end\\)_\\([-A-Za-z0-9]+\\)\\(?:$\\|\\s-\\)")
      (let ((type (downcase (match-string 2))))
	(cond
	 ((not (org-in-block-p (list type))) 'paragraph)
	 ((string= type "center") 'center-block)
	 ((string= type "quote") 'quote-block)
	 (t 'special-block))))
     ;; Regular drawers must be tested after property drawer as both
     ;; elements share the same ending regexp.
     ((or (looking-at org-drawer-regexp) (looking-at "[ \t]*:END:[ \t]*$"))
      (let ((completep (org-between-regexps-p
			org-drawer-regexp "^[ \t]*:END:[ \t]*$")))
	(if (not completep)
	    'paragraph
	  (goto-char (car completep)) 'drawer)))
     ((looking-at "[ \t]*:\\( \\|$\\)") 'fixed-width)
     ;; Babel calls must be tested before general keywords as they are
     ;; a subset of them.
     ((looking-at org-babel-block-lob-one-liner-regexp) 'babel-call)
     ((looking-at org-footnote-definition-re) 'footnote-definition)
     ((looking-at "[ \t]*#\\+\\([a-z]+\\(:?_[a-z]+\\)*\\):")
      (if (member (downcase (match-string 1)) org-element-affiliated-keywords)
	  'paragraph
	'keyword))
     ;; Dynamic block: simplify regexp used for match. If it isn't
     ;; complete, parse the current part as a paragraph.
     ((looking-at "[ \t]*#\\+\\(begin\\end\\):\\(?:\\s-\\|$\\)")
      (let ((completep (org-between-regexps-p
			"^[ \t]*#\\+begin:\\(?:\\s-\\|$\\)"
			"^[ \t]*#\\+end:\\(?:\\s-\\|$\\)")))
	(if (not completep)
	    'paragraph
	  (goto-char (car completep)) 'dynamic-block)))
     ((looking-at "\\(#\\|[ \t]*#\\+\\( \\|$\\)\\)") 'comment)
     ((looking-at "[ \t]*-\\{5,\\}[ \t]*$") 'horizontal-rule)
     ((org-at-table-p t) 'table)
     ((looking-at "[ \t]*#\\+tblfm:")
      (forward-line -1)
      ;; A TBLFM line separated from any table is just plain text.
      (if (org-at-table-p)
	  'table
	(forward-line) 'paragraph))
     ((looking-at (org-item-re)) 'plain-list))))

;; Most elements can have affiliated keywords.  When looking for an
;; element beginning, we want to move before them, as they belong to
;; that element, and, in the meantime, collect information they give
;; into appropriate properties.  Hence the following function.

;; Usage of optional arguments may not be obvious at first glance:

;; - TRANS-LIST is used to polish keywords names that have evolved
;;   during Org history.  In example, even though =result= and
;;   =results= coexist, we want to have them under the same =result=
;;   property.  It's also true for "srcname" and "name", where the
;;   latter seems to be preferred nowadays (thus the "name" property).

;; - CONSED allows to regroup multi-lines keywords under the same
;;   property, while preserving their own identity.  This is mostly
;;   used for "attr_latex" and al.

;; - PARSED prepares a keyword value for export.  This is useful for
;;   "caption".  Objects restrictions for such keywords are defined in
;;   `org-element-string-restrictions'.

;; - DUALS is used to take care of keywords accepting a main and an
;;   optional secondary values.  For example "results" has its
;;   source's name as the main value, and may have an hash string in
;;   optional square brackets as the secondary one.

;; A keyword may belong to more than one category.
(defun org-element-collect-affiliated-keywords (&optional key-re trans-list
							  consed parsed duals)
  "Collect affiliated keywords before point.

Optional argument KEY-RE is a regexp matching keywords, which
puts matched keyword in group 1.  It defaults to
`org-element--affiliated-re'.

TRANS-LIST is an alist where key is the keyword and value the
property name it should be translated to, without the colons.  It
defaults to `org-element-keyword-translation-alist'.

CONSED is a list of strings.  Any keyword belonging to that list
will have its value consed.  The check is done after keyword
translation.  It defaults to `org-element-multiple-keywords'.

PARSED is a list of strings.  Any keyword member of this list
will have its value parsed.  The check is done after keyword
translation.  If a keyword is a member of both CONSED and PARSED,
it's value will be a list of parsed strings.  It defaults to
`org-element-parsed-keywords'.

DUALS is a list of strings.  Any keyword member of this list can
have two parts: one mandatory and one optional.  Its value is
a cons cell whose car is the former, and the cdr the latter.  If
a keyword is a member of both PARSED and DUALS, only the primary
part will be parsed.  It defaults to `org-element-dual-keywords'.

Return a list whose car is the position at the first of them and
cdr a plist of keywords and values."
  (save-excursion
    (let ((case-fold-search t)
	  (key-re (or key-re org-element--affiliated-re))
	  (trans-list (or trans-list org-element-keyword-translation-alist))
	  (consed (or consed org-element-multiple-keywords))
	  (parsed (or parsed org-element-parsed-keywords))
	  (duals (or duals org-element-dual-keywords))
	  output)
      (unless (bobp)
	(while (and (not (bobp))
		    (progn (forward-line -1) (looking-at key-re)))
	  (let* ((raw-kwd (downcase (or (match-string 2) (match-string 1))))
		 ;; Apply translation to RAW-KWD.  From there, KWD is
		 ;; the official keyword.
		 (kwd (or (cdr (assoc raw-kwd trans-list)) raw-kwd))
		 ;; If KWD is a dual keyword, find it secondary value.
		 (dual-value (and (member kwd duals)
				  (org-match-string-no-properties 3)))
		 ;; Find main value for any keyword.
		 (value (org-trim (buffer-substring-no-properties
				   (match-end 0) (point-at-eol))))
		 ;; Attribute a property name to KWD.
		 (kwd-sym (and kwd (intern (concat ":" kwd)))))
	    ;; Now set final shape for VALUE.
	    (when (member kwd parsed)
	      (setq value
		    (org-element-parse-secondary-string
		     value
		     (cdr (assq 'keyword org-element-string-restrictions)))))
	    (when (member kwd duals) (setq value (cons value dual-value)))
	    (when (member kwd consed)
	      (setq value (cons value (plist-get output kwd-sym))))
	    ;; Eventually store the new value in OUTPUT.
	    (setq output (plist-put output kwd-sym value))))
	(unless (looking-at key-re) (forward-line 1)))
      (list (point) output))))



;;; The Org Parser

;; The two major functions here are `org-element-parse-buffer', which
;; parses Org syntax inside the current buffer, taking into account
;; region, narrowing, or even visibility if specified, and
;; `org-element-parse-secondary-string', which parses objects within
;; a given string.

;; The (almost) almighty `org-element-map' allows to apply a function
;; on elements or objects matching some type, and accumulate the
;; resulting values.  In an export situation, it also skips unneeded
;; parts of the parse tree, transparently walks into included files,
;; and maintain a list of local properties (i.e. those inherited from
;; parent headlines) for function's consumption.
(defun org-element-parse-buffer (&optional granularity visible-only)
  "Recursively parse the buffer and return structure.
If narrowing is in effect, only parse the visible part of the
buffer.

Optional argument GRANULARITY determines the depth of the
recursion.  It can be set to the following symbols:

`headline'          Only parse headlines.
`greater-element'   Don't recurse into greater elements.  Thus,
		    elements parsed are the top-level ones.
`element'           Parse everything but objects and plain text.
`object'            Parse the complete buffer (default).

When VISIBLE-ONLY is non-nil, don't parse contents of hidden
elements.

Assume buffer is in Org mode."
  (save-excursion
    (goto-char (point-min))
    (org-skip-whitespace)
    (nconc (list 'org-data nil)
	   (org-element-parse-elements
	    (point-at-bol) (point-max)
	    nil nil granularity visible-only nil))))

(defun org-element-parse-secondary-string (string restriction &optional buffer)
  "Recursively parse objects in STRING and return structure.

RESTRICTION, when non-nil, is a symbol limiting the object types
that will be looked after.

Optional argument BUFFER indicates the buffer from where the
secondary string was extracted.  It is used to determine where to
get extraneous information for an object \(i.e. when resolving
a link or looking for a footnote definition\).  It defaults to
the current buffer."
  (with-temp-buffer
    (insert string)
    (org-element-parse-objects (point-min) (point-max) nil restriction)))

(defun org-element-map (data types fun &optional info first-match)
  "Map a function on selected elements or objects.

DATA is the parsed tree, as returned by, i.e,
`org-element-parse-buffer'.  TYPES is a symbol or list of symbols
of elements or objects types.  FUN is the function called on the
matching element or object.  It must accept two arguments: the
element or object itself and a plist holding contextual
information.

When optional argument INFO is non-nil, it should be a plist
holding export options.  In that case, parts of the parse tree
not exportable according to that property list will be skipped
and files included through a keyword will be visited.

When optional argument FIRST-MATCH is non-nil, stop at the first
match for which FUN doesn't return nil, and return that value.

Nil values returned from FUN are ignored in the result."
  ;; Ensure TYPES is a list, even of one element.
  (unless (listp types) (setq types (list types)))
  ;; Recursion depth is determined by --CATEGORY.
  (let* ((--category
	  (cond
	   ((loop for type in types
		  always (memq type org-element-greater-elements))
	    'greater-elements)
	   ((loop for type in types
		  always (memq type org-element-all-elements))
	    'elements)
	   (t 'objects)))
	 walk-tree			; For byte-compiler
	 --acc
	 (accumulate-maybe
	  (function
	   (lambda (--type types fun --blob --local)
	     ;; Check if TYPE is matching among TYPES.  If so, apply
	     ;; FUN to --BLOB and accumulate return value
	     ;; into --ACC.  --LOCAL is the communication channel.
	     (when (memq --type types)
	       (let ((result (funcall fun --blob --local)))
		 (cond ((not result))
		       (first-match (throw 'first-match result))
		       (t (push result --acc))))))))
	 (walk-tree
	  (function
	   (lambda (--data --local)
	     ;; Recursively walk DATA.  --LOCAL, if non-nil, is
	     ;; a plist holding contextual information.
	     (mapc
	      (lambda (--blob)
		(let ((--type (if (stringp --blob) 'plain-text (car --blob))))
		  ;; Determine if a recursion into --BLOB is
		  ;; possible and allowed.
		  (cond
		   ;; Element or object not exportable.
		   ((and info (org-export-skip-p --blob info)))
		   ;; Archived headline: Maybe apply fun on it, but
		   ;; skip contents.
		   ((and info
			 (eq --type 'headline)
			 (eq (plist-get info :with-archived-trees) 'headline)
			 (org-element-get-property :archivedp --blob))
		    (funcall accumulate-maybe --type types fun --blob --local))
		   ;; At an include keyword: apply mapping to its
		   ;; contents.
		   ((and --local
			 (eq --type 'keyword)
			 (string=
			  (downcase (org-element-get-property :key --blob))
			  "include"))
		    (funcall accumulate-maybe --type types fun --blob --local)
		    (let* ((--data
			    (org-export-parse-included-file --blob --local))
			   (--value (org-element-get-property :value --blob))
			   (--file
			    (and (string-match "^\"\\(\\S-+\\)\"" --value)
				 (match-string 1 --value))))
		      (funcall
		       walk-tree --data
		       (org-combine-plists
			--local
			;; Store full path of already included files
			;; to avoid recursive file inclusion.
			`(:included-files
			  ,(cons (expand-file-name --file)
				 (plist-get --local :included-files))
			  ;; Ensure that a top-level headline in the
			  ;; included file becomes a direct child of
			  ;; the current headline in the buffer.
			  :headline-offset
			  ,(- (+ (plist-get
				  (plist-get --local :inherited-properties)
				  :level)
				 (or (plist-get --local :headline-offset) 0))
			      (1- (org-export-get-min-level
				   --data --local))))))))
		   ;; Limiting recursion to greater elements, and --BLOB
		   ;; isn't one.
		   ((and (eq --category 'greater-elements)
			 (not (memq --type org-element-greater-elements)))
		    (funcall accumulate-maybe --type types fun --blob --local))
		   ;; Limiting recursion to elements, and --BLOB only
		   ;; contains objects.
		   ((and (eq --category 'elements) (eq --type 'paragraph)))
		   ;; No limitation on recursion, but --BLOB hasn't
		   ;; got a recursive type.
		   ((and (eq --category 'objects)
			 (not (or (eq --type 'paragraph)
				  (memq --type org-element-greater-elements)
				  (memq --type org-element-recursive-objects))))
		    (funcall accumulate-maybe --type types fun --blob --local))
		   ;; Recursion is possible and allowed: Update local
		   ;; information and move into --BLOB.
		   (t (funcall accumulate-maybe --type types fun --blob --local)
		      (funcall
		       walk-tree --blob
		       (and info (org-export-update-info --blob --local t)))))))
	      (org-element-get-contents --data))))))
    (catch 'first-match
      (funcall walk-tree data info)
      ;; Return value in a proper order.
      (reverse --acc))))

;; The following functions are internal parts of the parser.  The
;; first one, `org-element-parse-elements' acts at the element's
;; level.  The second one, `org-element-parse-objects' applies on all
;; objects of a paragraph or a secondary string.  It uses
;; `org-element-get-candidates' to optimize the search of the next
;; object in the buffer.
;;
;; More precisely, that function looks for every allowed object type
;; first.  Then, it discards failed searches, keeps further matches,
;; and searches again types matched behind point, for subsequent
;; calls.  Thus, searching for a given type fails only once, and every
;; object is searched only once at top level (but sometimes more for
;; nested types).
(defun org-element-parse-elements (beg end item structure granularity visible-only acc)
  "Parse ELEMENT with point at its beginning.

If ITEM is non-nil, parse item wise instead of plain-list wise,
using STRUCTURE as the current list structure.

GRANULARITY determines the depth of the recursion.  It can be set
to the following symbols:

`headline'          Only parse headlines.
`greater-element'   Don't recurse into greater elements.  Thus,
		    elements parsed are the top-level ones.
`element'           Parse everything but objects and plain text.
`object' or nil     Parse the complete buffer.

When VISIBLE-ONLY is non-nil, don't parse contents of hidden
greater elements.

Elements are accumulated into ACC."
  (save-excursion
    (goto-char beg)
    ;; Shortcut when parsing only headlines.
    (when (and (eq granularity 'headline) (not (org-at-heading-p)))
      (org-with-limited-levels (outline-next-heading)))
    ;; Main loop start.
    (while (and (< (point) end) (not (eobp)))
      (push
       ;; 1. If ITEM is toggled, point is at an item.  Knowing that,
       ;; there's no need to go through `org-element-at-point'.
       (if item
	   (let* ((element (org-element-item-parser structure))
		  (cbeg (org-element-get-property :contents-begin element))
		  (cend (org-element-get-property :contents-end element)))
	     (goto-char (org-element-get-property :end element))
	     ;; Narrow region to contents, so that item bullet don't
	     ;; interfere with paragraph parsing.
	     (save-restriction
	       (narrow-to-region cbeg cend)
	       (org-element-parse-elements
		cbeg cend nil structure granularity visible-only
		(reverse element))))
	 ;; 2. When ITEM is nil, find current element's type and parse
	 ;;    it accordingly to its category.
	 (let ((element (org-element-at-point nil structure)))
	   (goto-char (org-element-get-property :end element))
	   (cond
	    ;; Case 1: ELEMENT is a footnote-definition.  If
	    ;; GRANURALITY allows parsing, use narrowing so that
	    ;; footnote label don't interfere with paragraph
	    ;; recognition.
	    ((and (eq (car element) 'footnote-definition)
		  (not (memq granularity '(headline greater-element))))
	     (let ((cbeg (org-element-get-property :contents-begin element))
		   (cend (org-element-get-property :contents-end element)))
	       (save-restriction
		 (narrow-to-region cbeg cend)
		 (org-element-parse-elements
		  cbeg cend nil structure granularity visible-only
		  (reverse element)))))
	    ;; Case 1: ELEMENT is a paragraph.  Parse objects inside,
	    ;;         if GRANULARITY allows it.
	    ((and (eq (car element) 'paragraph)
		  (or (not granularity) (eq granularity 'object)))
	     (org-element-parse-objects
	      (org-element-get-property :contents-begin element)
	      (org-element-get-property :contents-end element)
	      (reverse element)
	      nil))
	    ;; Case 2: ELEMENT is recursive: parse it between
	    ;;         `contents-begin' and `contents-end'.  If it's
	    ;;         a plain list, also switch to item mode.  Make
	    ;;         sure GRANULARITY allows the recursion, or
	    ;;         ELEMENT is an headline, in which case going
	    ;;         inside is mandatory, in order to get sub-level
	    ;;         headings.  If VISIBLE-ONLY is true and element
	    ;;         is hidden, do not recurse into it.
	    ((and (memq (car element) org-element-greater-elements)
		  (or (not granularity)
		      (memq granularity '(element object))
		      (eq (car element) 'headline))
		  (not (and visible-only
			    (org-element-get-property :hiddenp element))))
	     (org-element-parse-elements
	      (org-element-get-property :contents-begin element)
	      (org-element-get-property :contents-end element)
	      (eq (car element) 'plain-list)
	      (org-element-get-property :structure element)
	      granularity
	      visible-only
	      (reverse element)))
	    ;; Case 3: Else, just accumulate ELEMENT, unless
	    ;;         GRANULARITY is set to `headline'.
	    ((not (eq granularity 'headline)) element))))
       acc)
      (org-skip-whitespace))
    ;; Return result.
    (nreverse acc)))

(defun org-element-parse-objects (beg end acc restriction)
  "Parse objects between BEG and END and return recursive structure.

Objects are accumulated in ACC.

RESTRICTION, when non-nil, is a list of object types which are
allowed in the current object."
  (let ((get-next-object
	 (function
	  (lambda (cand)
	    ;; Return the parsing function associated to the nearest
	    ;; object among list of candidates CAND.
	    (let ((pos (apply #'min (mapcar #'cdr cand))))
	      (save-excursion
		(goto-char pos)
		(funcall
		 (intern
		  (format "org-element-%s-parser" (car (rassq pos cand))))))))))
	next-object candidates)
    (save-excursion
      (goto-char beg)
      (while (setq candidates (org-element-get-next-object-candidates
			       end restriction candidates))
	(setq next-object (funcall get-next-object candidates))
	;; 1. Text before any object.
	(let ((obj-beg (org-element-get-property :begin next-object)))
	  (unless (= beg obj-beg)
	    (push (buffer-substring-no-properties (point) obj-beg) acc)))
	;; 2. Object...
	(let ((obj-end (org-element-get-property :end next-object))
	      (cont-beg (org-element-get-property :contents-begin next-object)))
	  (push (if (and (memq (car next-object) org-element-recursive-objects)
			 cont-beg)
		    ;; ... recursive.  The CONT-BEG check is for
		    ;; links, as some of them might not be recursive
		    ;; (i.e. plain links).
		    (save-restriction
		      (narrow-to-region
		       cont-beg
		       (org-element-get-property :contents-end next-object))
		      (org-element-parse-objects
		       (point-min) (point-max) (reverse next-object)
		       ;; Restrict allowed objects.  This is the
		       ;; intersection of current restriction and next
		       ;; object's restriction.
		       (let ((new-restr
			      (cdr (assq (car next-object)
					 org-element-object-restrictions))))
			 (if (not restriction)
			     new-restr
			   (delq nil
				 (mapcar (lambda (e)
					   (and (memq e restriction) e))
					 new-restr))))))
		  ;; ... not recursive.
		  next-object)
		acc)
	  (goto-char obj-end)))
      ;; 3. Text after last object.
      (unless (= (point) end)
	(push (buffer-substring-no-properties (point) end) acc))
      ;; Result.
      (nreverse acc))))

(defun org-element-get-next-object-candidates (limit restriction objects)
  "Return an alist of candidates for the next object.

LIMIT bounds the search, and RESTRICTION, when non-nil, bounds
the possible object types.

Return value is an alist whose car is position and cdr the object
type, as a string.  There is an association for the closest
object of each type within RESTRICTION when non-nil, or for every
type otherwise.

OBJECTS is the previous candidates alist."
  (let ((restriction (or restriction org-element-all-successors))
	next-candidates types-to-search)
    ;; If no previous result, search every object type in RESTRICTION.
    ;; Otherwise, keep potential candidates (old objects located after
    ;; point) and ask to search again those which had matched before.
    (if objects
	(mapc (lambda (obj)
		(if (< (cdr obj) (point))
		    (push (car obj) types-to-search)
		  (push obj next-candidates)))
	      objects)
      (setq types-to-search restriction))
    ;; Call the appropriate "get-next" function for each type to
    ;; search and accumulate matches.
    (mapc
     (lambda (type)
       (let* ((successor-fun
	       (intern
		(format "org-element-%s-successor"
			(or (cdr (assq type org-element-object-successor-alist))
			    type))))
	      (obj (funcall successor-fun limit)))
	 (and obj (push obj next-candidates))))
     types-to-search)
    ;; Return alist.
    next-candidates))



;;; Towards A Bijective Process

;; The parse tree obtained with `org-element-parse-buffer' is really
;; a snapshot of the corresponding Org buffer.  Therefore, it can be
;; interpreted and expanded into a string with canonical Org
;; syntax. Hence `org-element-interpret-data'.
;;
;; Data parsed from secondary strings, whose shape is slightly
;; different than the standard parse tree, is expanded with the
;; equivalent function `org-element-interpret-secondary'.
;;
;; Both functions rely internally on
;; `org-element-interpret--affiliated-keywords'.
(defun org-element-interpret-data (data &optional genealogy previous)
  "Interpret a parse tree representing Org data.

DATA is the parse tree to interpret.

Optional arguments GENEALOGY and PREVIOUS are used for recursive
calls:
GENEALOGY is the list of its parents types.
PREVIOUS is the type of the element or object at the same level
interpreted before.

Return Org syntax as a string."
  (mapconcat
   (lambda (blob)
     ;; BLOB can be an element, an object, a string, or nil.
     (cond
      ((not blob) nil)
      ((equal blob "") nil)
      ((stringp blob) blob)
      (t
       (let* ((type (car blob))
	      (interpreter
	       (if (eq type 'org-data)
		   'identity
		 (intern (format "org-element-%s-interpreter" type))))
	      (contents
	       (cond
		;; Full Org document.
		((eq type 'org-data)
		 (org-element-interpret-data blob genealogy previous))
		;; Recursive objects.
		((memq type org-element-recursive-objects)
		 (org-element-interpret-data
		  blob (cons type genealogy) nil))
		;; Recursive elements.
		((memq type org-element-greater-elements)
		 (org-element-normalize-string
		  (org-element-interpret-data
		   blob (cons type genealogy) nil)))
		;; Paragraphs.
		((eq type 'paragraph)
		 (let ((paragraph
			(org-element-normalize-contents
			 blob
			 ;; When normalizing contents of an item,
			 ;; ignore first line's indentation.
			 (and (not previous)
			      (memq (car genealogy)
				    '(footnote-definiton item))))))
		   (org-element-interpret-data
		    paragraph (cons type genealogy) nil)))))
	      (results (funcall interpreter blob contents)))
	 ;; Update PREVIOUS.
	 (setq previous type)
	 ;; Build white spaces.
	 (cond
	  ((eq type 'org-data) results)
	  ((memq type org-element-all-elements)
	   (concat
	    (org-element-interpret--affiliated-keywords blob)
	    (org-element-normalize-string results)
	    (make-string (org-element-get-property :post-blank blob) 10)))
	  (t (concat
	      results
	      (make-string
	       (org-element-get-property :post-blank blob) 32))))))))
   (org-element-get-contents data) ""))

(defun org-element-interpret-secondary (secondary)
  "Interpret SECONDARY string as Org syntax.

SECONDARY-STRING is a nested list as returned by
`org-element-parse-secondary-string'.

Return interpreted string."
  ;; Make SECONDARY acceptable for `org-element-interpret-data'.
  (let ((s (if (listp secondary) secondary (list secondary))))
    (org-element-interpret-data `(org-data nil ,@s) nil nil)))

;; Both functions internally use `org-element--affiliated-keywords'.

(defun org-element-interpret--affiliated-keywords (element)
  "Return ELEMENT's affiliated keywords as Org syntax.
If there is no affiliated keyword, return the empty string."
  (let ((keyword-to-org
	 (function
	  (lambda (key value)
	    (let (dual)
	      (when (member key org-element-dual-keywords)
		(setq dual (cdr value) value (car value)))
	      (concat "#+" key (and dual (format "[%s]" dual)) ": "
		      (if (member key org-element-parsed-keywords)
			  (org-element-interpret-secondary value)
			value)
		      "\n"))))))
    (mapconcat
     (lambda (key)
       (let ((value (org-element-get-property (intern (concat ":" key)) element)))
	 (when value
	   (if (member key org-element-multiple-keywords)
	       (mapconcat (lambda (line)
			    (funcall keyword-to-org key line))
			  value "")
	     (funcall keyword-to-org key value)))))
     ;; Remove translated keywords.
     (delq nil
	   (mapcar
	    (lambda (key)
	      (and (not (assoc key org-element-keyword-translation-alist)) key))
	    org-element-affiliated-keywords))
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

The following changes are applied to plain text:
  - Remove global indentation, preserving relative one.
  - Untabify it.

If optional argument IGNORE-FIRST is non-nil, ignore first line's
indentation to compute maximal common indentation.

Return the normalized element."
  (nconc
   (list (car element) (nth 1 element))
   (let ((contents (org-element-get-contents element)))
     (cond
      ((and (not ignore-first) (not (stringp (car contents)))) contents)
      (t
       (catch 'exit
	 ;; 1. Remove tabs from each string in CONTENTS.  Get maximal
	 ;;    common indentation (MCI) along the way.
	 (let* ((ind-list (unless ignore-first
			    (list (org-get-string-indentation (car contents)))))
		(contents
		 (mapcar (lambda (object)
			   (if (not (stringp object))
			       object
			     (let ((start 0)
				   (object (org-remove-tabs object)))
			       (while (string-match "\n\\( *\\)" object start)
				 (setq start (match-end 0))
				 (push (length (match-string 1 object))
				       ind-list))
			       object)))
			 contents))
		(mci (if ind-list
			 (apply 'min ind-list)
		       (throw 'exit contents))))
	   ;; 2. Remove that indentation from CONTENTS.  First string
	   ;;    must be treated differently because it's the only one
	   ;;    whose indentation doesn't happen after a newline
	   ;;    character.
	   (let ((first-obj (car contents)))
	     (unless (or (not (stringp first-obj)) ignore-first)
	       (setq contents
		     (cons (replace-regexp-in-string
			    (format "\\` \\{%d\\}" mci) "" first-obj)
			   (cdr contents)))))
	   (mapcar (lambda (object)
		     (if (not (stringp object))
			 object
		       (replace-regexp-in-string
			(format "\n \\{%d\\}" mci) "\n" object)))
		   contents))))))))



;;; The Toolbox

;; Once the structure of an Org file is well understood, it's easy to
;; implement some replacements for `forward-paragraph'
;; `backward-paragraph', namely `org-element-forward' and
;; `org-element-backward'.

;; Also, `org-transpose-elements' mimics the behaviour of
;; `transpose-words', at the element's level, whereas
;; `org-element-drag-forward', `org-element-drag-backward', and
;; `org-element-up' generalize, respectively, functions
;; `org-subtree-down', `org-subtree-up' and `outline-up-heading'.

;; `org-element-unindent-buffer' will, as its name almost suggests,
;; smartly remove global indentation from buffer, making it possible
;; to use Org indent mode on a file created with hard indentation.

;; `org-element-nested-p' and `org-element-swap-A-B' are used
;; internally by some of the previously cited tools.
(defsubst org-element-nested-p (elem-A elem-B)
  "Non-nil when elements ELEM-A and ELEM-B are nested."
  (let ((beg-A (org-element-get-property :begin elem-A))
	(beg-B (org-element-get-property :begin elem-B))
	(end-A (org-element-get-property :end elem-A))
	(end-B (org-element-get-property :end elem-B)))
    (or (and (>= beg-A beg-B) (<= end-A end-B))
	(and (>= beg-B beg-A) (<= end-B end-A)))))

(defun org-element-swap-A-B (elem-A elem-B)
  "Swap elements ELEM-A and ELEM-B.

Leave point at the end of ELEM-A.

Assume ELEM-A is before ELEM-B and that they are not nested."
  (goto-char (org-element-get-property :begin elem-A))
  (let* ((beg-B (org-element-get-property :begin elem-B))
	 (end-B-no-blank (save-excursion
			     (goto-char (org-element-get-property :end elem-B))
			     (skip-chars-backward " \r\t\n")
			     (forward-line)
			     (point)))
	 (beg-A (org-element-get-property :begin elem-A))
	 (end-A-no-blank (save-excursion
			   (goto-char (org-element-get-property :end elem-A))
			   (skip-chars-backward " \r\t\n")
			   (forward-line)
			   (point)))
	 (body-A (buffer-substring beg-A end-A-no-blank))
	 (body-B (buffer-substring beg-B end-B-no-blank))
	 (between-A-B (buffer-substring end-A-no-blank beg-B)))
    (delete-region beg-A end-B-no-blank)
    (insert body-B between-A-B body-A)
    (goto-char (org-element-get-property :end elem-B))))

(defun org-element-backward ()
  "Move backward by one element."
  (interactive)
  (let* ((opoint (point))
	 (element (org-element-at-point))
	 (start-el-beg (org-element-get-property :begin element)))
    ;; At an headline. The previous element is the previous sibling,
    ;; or the parent if any.
    (cond
     ;; Already at the beginning of the current element: move to the
     ;; beginning of the previous one.
     ((= opoint start-el-beg)
      (forward-line -1)
      (skip-chars-backward " \r\t\n")
      (let* ((prev-element (org-element-at-point))
	     (itemp (org-in-item-p))
	     (struct (and itemp
			  (save-excursion (goto-char itemp)
					  (org-list-struct)))))
	;; When moving into a new list, go directly at the
	;; beginning of the top list structure.
	(if (and itemp (<= (org-list-get-bottom-point struct) opoint))
	    (progn
	      (goto-char (org-list-get-top-point struct))
	      (goto-char (org-element-get-property
			  :begin (org-element-at-point))))
	  (goto-char (org-element-get-property :begin prev-element))))
      (while (org-truely-invisible-p) (org-element-up)))
     ;; Else, move at the element beginning. One exception: if point
     ;; was in the blank lines after the end of a list, move directly
     ;; to the top item.
     (t
      (let (struct itemp)
	(if (and (setq itemp (org-in-item-p))
		 (<= (org-list-get-bottom-point
		      (save-excursion (goto-char itemp)
				      (setq struct (org-list-struct))))
		     opoint))
	    (progn (goto-char (org-list-get-top-point struct))
		   (goto-char (org-element-get-property
			       :begin (org-element-at-point))))
	  (goto-char start-el-beg)))))))

(defun org-element-drag-backward ()
  "Drag backward element at point."
  (interactive)
  (let* ((pos (point))
	 (elem (org-element-at-point)))
    (when (= (progn (goto-char (point-min))
		    (org-skip-whitespace)
		    (point-at-bol))
	     (org-element-get-property :end elem))
      (error "Cannot drag element backward"))
    (goto-char (org-element-get-property :begin elem))
    (org-element-backward)
    (let ((prev-elem (org-element-at-point)))
      (when (or (org-element-nested-p elem prev-elem)
		(and (eq (car elem) 'headline)
		     (not (eq (car prev-elem) 'headline))))
	(goto-char pos)
	(error "Cannot drag element backward"))
      ;; Compute new position of point: it's shifted by PREV-ELEM
      ;; body's length.
      (let ((size-prev (- (org-element-get-property :end prev-elem)
			  (org-element-get-property :begin prev-elem))))
	(org-element-swap-A-B prev-elem elem)
	(goto-char (- pos size-prev))))))

(defun org-element-drag-forward ()
  "Move forward element at point."
  (interactive)
  (let* ((pos (point))
	 (elem (org-element-at-point)))
    (when (= (point-max) (org-element-get-property :end elem))
      (error "Cannot drag element forward"))
    (goto-char (org-element-get-property :end elem))
    (let ((next-elem (org-element-at-point)))
      (when (or (org-element-nested-p elem next-elem)
		(and (eq (car next-elem) 'headline)
		     (not (eq (car elem) 'headline))))
	(goto-char pos)
	(error "Cannot drag element forward"))
      ;; Compute new position of point: it's shifted by NEXT-ELEM
      ;; body's length (without final blanks) and by the length of
      ;; blanks between ELEM and NEXT-ELEM.
      (let ((size-next (- (save-excursion
			    (goto-char (org-element-get-property :end next-elem))
			    (skip-chars-backward " \r\t\n")
			    (forward-line)
			    (point))
			  (org-element-get-property :begin next-elem)))
	    (size-blank (- (org-element-get-property :end elem)
			   (save-excursion
			     (goto-char (org-element-get-property :end elem))
			     (skip-chars-backward " \r\t\n")
			     (forward-line)
			     (point)))))
	(org-element-swap-A-B elem next-elem)
	(goto-char (+ pos size-next size-blank))))))

(defun org-element-forward ()
  "Move forward by one element."
  (interactive)
  (beginning-of-line)
  (cond ((eobp) (error "Cannot move further down"))
	((looking-at "[ \t]*$")
	 (org-skip-whitespace)
	 (goto-char (if (eobp) (point) (point-at-bol))))
	(t
	 (let ((element (org-element-at-point t))
	       (origin (point)))
	   (cond
	    ;; At an item: Either move to the next element inside, or
	    ;; to its end if it's hidden.
	    ((eq (car element) 'item)
	     (if (org-element-get-property :hiddenp element)
		 (goto-char (org-element-get-property :end element))
	       (end-of-line)
	       (re-search-forward org-element-paragraph-separate nil t)
	       (org-skip-whitespace)
	       (beginning-of-line)))
	    ;; At a recursive element: Either move inside, or if it's
	    ;; hidden, move to its end.
	    ((memq (car element) org-element-greater-elements)
	     (let ((cbeg (org-element-get-property :contents-begin element)))
	       (goto-char
		(if (or (org-element-get-property :hiddenp element)
			(> origin cbeg))
		    (org-element-get-property :end element)
		  cbeg))))
	    ;; Else: move to the current element's end.
	    (t (goto-char (org-element-get-property :end element))))))))

(defun org-element-mark-element ()
  "Put point at beginning of this element, mark at end.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next element after the
ones already marked."
  (interactive)
  (let (deactivate-mark)
    (if (or (and (eq last-command this-command) (mark t))
	    (and transient-mark-mode mark-active))
	(set-mark
	 (save-excursion
	   (goto-char (mark))
	   (goto-char (org-element-get-property :end (org-element-at-point)))))
      (let ((element (org-element-at-point)))
	(end-of-line)
	(push-mark (org-element-get-property :end element) t t)
	(goto-char (org-element-get-property :begin element))))))

(defun org-narrow-to-element ()
  "Narrow buffer to current element."
  (interactive)
  (let ((elem (org-element-at-point)))
    (cond
     ((eq (car elem) 'headline)
      (narrow-to-region
       (org-element-get-property :begin elem)
       (org-element-get-property :end elem)))
     ((memq (car elem) org-element-greater-elements)
      (narrow-to-region
       (org-element-get-property :contents-begin elem)
       (org-element-get-property :contents-end elem)))
     (t
      (narrow-to-region
       (org-element-get-property :begin elem)
       (org-element-get-property :end elem))))))

(defun org-transpose-elements ()
  "Transpose current and previous elements, keeping blank lines between.
Point is moved after both elements."
  (interactive)
  (org-skip-whitespace)
  (let ((pos (point))
	(cur (org-element-at-point)))
    (when (= (save-excursion (goto-char (point-min))
			     (org-skip-whitespace)
			     (point-at-bol))
	     (org-element-get-property :begin cur))
      (error "No previous element"))
    (goto-char (org-element-get-property :begin cur))
    (forward-line -1)
    (let ((prev (org-element-at-point)))
      (when (org-element-nested-p cur prev)
	(goto-char pos)
	(error "Cannot transpose nested elements"))
      (org-element-swap-A-B prev cur))))

(defun org-element-unindent-buffer ()
  "Un-indent the visible part of the buffer.
Relative indentation \(between items, inside blocks, etc.\) isn't
modified."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Cannot un-indent a buffer not in Org mode"))
  (let* ((parse-tree (org-element-parse-buffer 'greater-element))
	 unindent-tree                   ; For byte-compiler.
	 (unindent-tree
	  (function
	   (lambda (contents)
	     (mapc (lambda (element)
		     (if (eq (car element) 'headline)
			 (funcall unindent-tree
				  (org-element-get-contents element))
		       (save-excursion
			 (save-restriction
			   (narrow-to-region
			    (org-element-get-property :begin element)
			    (org-element-get-property :end element))
			   (org-do-remove-indentation)))))
		   (reverse contents))))))
    (funcall unindent-tree (org-element-get-contents parse-tree))))

(defun org-element-up ()
  "Move to upper element.
Return position at the beginning of the upper element."
  (interactive)
  (let ((opoint (point)) elem)
    (cond
     ((bobp) (error "No surrounding element"))
     ((org-with-limited-levels (org-at-heading-p))
      (or (org-up-heading-safe) (error "No surronding element")))
     ((and (org-at-item-p)
	   (setq elem (org-element-at-point))
	   (let* ((top-list-p (zerop (org-element-get-property :level elem))))
	     (unless top-list-p
	       ;; If parent is bound to be in the same list as the
	       ;; original point, move to that parent.
	       (let ((struct (org-element-get-property :structure elem)))
		 (goto-char
		  (org-list-get-parent
		   (point-at-bol) struct (org-list-parents-alist struct))))))))
     (t
      (let* ((elem (or elem (org-element-at-point)))
	     (end (save-excursion
		    (goto-char (org-element-get-property :end elem))
		    (skip-chars-backward " \r\t\n")
		    (forward-line)
		    (point)))
	     prev-elem)
	(goto-char (org-element-get-property :begin elem))
	(forward-line -1)
	(while (and (< (org-element-get-property
			:end (setq prev-elem (org-element-at-point)))
		       end)
		    (not (bobp)))
	  (goto-char (org-element-get-property :begin prev-elem))
	  (forward-line -1))
	(if (and (bobp) (< (org-element-get-property :end prev-elem) end))
	    (progn (goto-char opoint)
		   (error "No surrounding element"))
	  (goto-char (org-element-get-property :begin prev-elem))))))))


(provide 'org-element)
;;; org-element.el ends here
