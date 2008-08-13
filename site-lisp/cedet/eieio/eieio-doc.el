;;; eieio-doc.el --- create texinfo documentation for an eieio class

;;; Copyright (C) 1996, 1998, 1999, 2000, 2001, 2004, 2005 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; RCS: $Id: eieio-doc.el,v 1.20 2005/09/30 20:18:01 zappo Exp $
;; Keywords: OO, lisp, docs
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.org

;;; Commentary:
;;
;;  Outputs into the current buffer documentation in texinfo format

(require 'eieio-opt)

;;  for a class, all it's children, and all it's slots.

;;; Code:
(defvar eieiodoc-currently-in-node nil
  "String representing the node we go BACK to.")

(defvar eieiodoc-current-section-level nil
  "String represending what type of section header to use.")

(defvar eieiodoc-prev-class nil
  "Non-nil when while `eieiodoc-recurse' is running.
Can be referenced from the recursed function.")

(defvar eieiodoc-next-class nil
  "Non-nil when `eieiodoc-recurse' is running.
Can be referenced from the recursed function.")

(defun eieiodoc-class-nuke (root-class indexstring &optional skiplist)
  "Call `eieiodoc-class' after nuking everything from POINT on.
ROOT-CLASS, INDEXSTRING, and SKIPLIST are the same as `eieiodoc-class'."
  (delete-region (point) (point-max))
  (sit-for 0)
  (eieiodoc-class root-class indexstring skiplist))

(defun eieiodoc-class (root-class indexstring &optional skiplist)
  "Create documentation starting with ROOT-CLASS.
The first job is to create an indented menu of all the classes
starting with `root-class' and including all it's children.  Once this
is done, @nodes are created for all the subclasses.  Each node is then
documented with a description of the class, a brief inheritance tree
\(with xrefs) and a list of all slots in a big table.  Where each slot
is inherited from is also documented.  In addition, each class is
documented in the index referenced by INDEXSTRING, a two letter code
described in the texinfo manual.

The optional third argument SKIPLIST is a list of object not to put
into any menus, nodes or lists."
  (interactive
   (list (intern-soft
	  (completing-read "Class: " (eieio-build-class-alist) nil t))
	 (read-string "Index name (2 chars): ")))
  (if (looking-at "[ \t\n]+@end ignore")
      (goto-char (match-end 0)))
  (save-excursion
    (setq eieiodoc-currently-in-node
	  (if (re-search-backward "@node \\([^,]+\\)" nil t)
	      (buffer-substring (match-beginning 1) (match-end 1))
	    "Top")
	  eieiodoc-current-section-level
	  (if (re-search-forward "@\\(chapter\\|\\(sub\\)*section\\)"
				 (+ (point) 500) t)
	      (progn
		(goto-char (match-beginning 0))
		(cond ((looking-at "@chapter") "section")
		      ((looking-at "@section") "subsection")
		      ((looking-at "@\\(sub\\)+section") "subsubsection")
		      (t "subsubsection")))
	    "subsubsection")))
  (save-excursion
    (eieiodoc-main-menu root-class skiplist)
    (insert "\n")
    (eieiodoc-recurse root-class 'eieiodoc-one-node nil skiplist)))
  
(defun eieiodoc-main-menu (class skiplist)
  "Create a menu of all classes under CLASS indented the correct amount.
SKIPLIST is a list of objects to skip"
  (end-of-line)
  (insert "\n@menu\n")
  (eieiodoc-recurse class (lambda (class level)
			(insert "* " (make-string level ? )
				(symbol-name class) " ::\n"))
		nil skiplist)
  (insert "@end menu\n"))

(defun eieiodoc-one-node (class level)
  "Create a node for CLASS, and for all subclasses of CLASS in order.
This function should only be called by `eieiodoc-class'
Argument LEVEL is the current level of recursion we have hit."
  (message "Building node for %s" class)
  (insert "\n@node " (symbol-name class) ", "
	  (if eieiodoc-next-class (symbol-name eieiodoc-next-class) " ") ", "
	  (if eieiodoc-prev-class (symbol-name eieiodoc-prev-class) " ") ", "
	  eieiodoc-currently-in-node "\n"
	  "@comment  node-name,  next,  previous,  up\n"
	  "@" eieiodoc-current-section-level " " (symbol-name class) "\n"
	  ;; indexstring is grabbed from parent calling function
	  "@" indexstring "index " (symbol-name class) "\n\n")
  ;; Now lets create a nifty little inheritance tree
  (let ((cl class)
	(revlist nil)
	(depth 0))
    (while cl
      (setq revlist (cons cl revlist)
	    cl (class-parent cl)))
    (insert "@table @asis\n@item Inheritance Tree:\n")
    (while revlist
      ;; root-class is dragged in from the top-level function
      (insert "@table @code\n@item "
	      (if (and (child-of-class-p (car revlist) root-class)
		       (not (eq class (car revlist))))
		  (concat "@w{@xref{" (symbol-name (car revlist)) "}.}")
		(symbol-name (car revlist)))
	      "\n")
      (setq revlist (cdr revlist)
	    depth (1+ depth)))
    ;; the value of rclass is brought in from caller
    (let ((clist (reverse (aref (class-v rclass) class-children))))
      (if (not clist)
	  (insert "No children")
	(insert "@table @asis\n@item Children:\n")
	(while clist
	  (insert "@w{@xref{" (symbol-name (car clist)) "}")
	  (if (cdr clist) (insert ",") (insert "."))
	  (insert "} ")
	  (setq clist (cdr clist)))
	(insert "\n@end table\n")
	))
    (while (> depth 0)
      (insert "\n@end table\n")
      (setq depth (1- depth)))
    (insert "@end table\n\n  "))
  ;; Now lets build some documentation by extracting information from
  ;; the class description vector
  (let* ((cv (class-v class))
	 (docs (aref cv class-public-doc))
	 (names (aref cv class-public-a))
	 (deflt (aref cv class-public-d))
	 (prot (aref cv class-protection))
	 (typev (aref cv class-public-type))
	 (i 0)
	 (set-one nil)
	 (anchor nil)
	 )
    ;; doc of the class itself
    (insert (eieiodoc-texify-docstring (documentation class) class)
	    "\n\n@table @asis\n")
    (if names
	(progn
	  (setq anchor (point))
	  (insert "@item Slots:\n\n@table @code\n")
	  (while names
	    (if (eieiodoc-one-attribute class (car names) (car docs)
					(car prot) (car deflt) (aref typev i))
		(setq set-one t))
	    (setq names (cdr names)
		  docs (cdr docs)
		  prot (cdr prot)
		  deflt (cdr deflt)
		  i (1+ i)))
	  (insert "@end table\n\n")
	  (if (not set-one) (delete-region (point) anchor))
	  ))
    (insert "@end table\n")
    ;; Finally, document all the methods associated with this class.
    (let ((methods (eieio-all-generic-functions class))
	  (doc nil))
      (if (not methods) nil
	(if (string= eieiodoc-current-section-level "subsubsection")
	    (insert "@" eieiodoc-current-section-level)
	  (insert "@sub" eieiodoc-current-section-level))
	(insert " Specialized Methods\n\n")
	(while methods
	  (setq doc (eieio-method-documentation (car methods) class))
	  (insert "@deffn Method " (symbol-name (car methods)))
	  (if (not doc)
	      (insert "\n  Undocumented")
	    (if (car doc)
		(progn
		  (insert " :BEFORE ")
		  (eieiodoc-output-deffn-args (car (car doc)))
		  (insert "\n")
		  (eieiodoc-insert-and-massage-docstring-with-args
		   (cdr (car doc)) (car (car doc)) class)))
	    (setq doc (cdr doc))
	    (if (car doc)
		(progn
		  (insert " :PRIMARY ")
		  (eieiodoc-output-deffn-args (car (car doc)))
		  (insert "\n")
		  (eieiodoc-insert-and-massage-docstring-with-args
		   (cdr (car doc)) (car (car doc)) class)))
	    (setq doc (cdr doc))
	    (if (car doc)
		(progn
		  (insert " :AFTER ")
		  (eieiodoc-output-deffn-args (car (car doc)))
		  (insert "\n")
		  (eieiodoc-insert-and-massage-docstring-with-args
		   (cdr (car doc)) (car (car doc)) class)))
	    (insert "\n@end deffn\n\n"))
	  (setq methods (cdr methods)))))
    ))

(defun eieiodoc-insert-and-massage-docstring-with-args (doc arglst class)
  "Update DOC with texinfo strings using ARGLST with @var.
Argument CLASS is the class passed to `eieiodoc-texify-docstring'."
  (let ((start (point))
	(end nil)
	(case-fold-search nil))
    ;; Insert the text
    (insert (eieiodoc-texify-docstring doc class))
    (setq end (point))
    (save-restriction
      (narrow-to-region start end)
      (save-excursion
	;; Now find arguments
	(while arglst
	  (goto-char (point-min))
	  (while (re-search-forward (upcase (symbol-name (car arglst))) nil t)
	    (replace-match "@var{\\&}" t))
	  (setq arglst (cdr arglst)))))))

(defun eieiodoc-output-deffn-args (arglst)
  "Output ARGLST for a deffn."
  (while arglst
    (insert (symbol-name (car arglst)) " ")
    (setq arglst (cdr arglst))))

(defun eieiodoc-one-attribute (class attribute doc priv deflt type)
  "Create documentation of CLASS for a single ATTRIBUTE.
Assume this attribute is inside a table, so it is initiated with the
@item indicator.  If this attribute is not inserted (because it is
contained in the parent) then return nil, else return t.
DOC is the documentation to use, PRIV is non-nil if it is a private slot,
and DEFLT is the default value.  TYPE is the symbol describing what type
validation is done on that slot."
  (let ((pv (eieiodoc-parent-diff class attribute))
	(ia (eieio-attribute-to-initarg class attribute))
	(set-me nil))
    (if (or (eq pv t) (not ia))
	nil  ;; same in parent or no init arg
      (setq set-me t)
      (insert "@item " (if priv "Private: " "")
	      (symbol-name ia))
      (if (and type (not (eq type t)))
	  (insert "\nType: @code{" (format "%S" type) "}"))
      (if (not (eq deflt eieio-unbound))
	  (insert " @*\nDefault Value: @code{"(format "%S" deflt) "}"))
      (insert "\n\n")
      (if (eq pv 'default)
	  ;; default differs only, xref the parent
	  ;; This should be upgraded to actually search for the last
	  ;; differing default (or the original.)
	  (insert "@xref{" (symbol-name (class-parent class)) "}.\n")
	(insert (if doc (eieiodoc-texify-docstring doc class) "Not Documented")
		"\n@refill\n\n")))
    set-me))
;;;
;; Utilities
;;
(defun eieiodoc-recurse (rclass func &optional level skiplist)
  "Recurse down all children of RCLASS, calling FUNC on each one.
LEVEL indicates the current depth below the first call we are.  The
function FUNC will be called with RCLASS and LEVEL.  This will then
recursivly call itself once for each child class of RCLASS.  The
optional fourth argument SKIPLIST is a list of objects to ignore while
recursing."

  (if (not level) (setq level 0))

  ;; we reverse the children so they appear in the same order as it
  ;; does in the code that creates them.
  (let* ((children (reverse (aref (class-v rclass) class-children)))
	 (ocnc eieiodoc-next-class)
	 (eieiodoc-next-class (or (car children) ocnc))
	 (eieiodoc-prev-class eieiodoc-prev-class))

    (if (not (member rclass skiplist))
	(progn
	  (apply func (list rclass level))

	  (setq eieiodoc-prev-class rclass)))

    (while children
      (setq eieiodoc-next-class (or (car (cdr children)) ocnc))
      (setq eieiodoc-prev-class (eieiodoc-recurse (car children) func (1+ level)))
      (setq children (cdr children)))
    ;; return the previous class so that the prev/next node gets it right
    eieiodoc-prev-class))

(defun eieiodoc-parent-diff (class slot)
  "Return nil if the parent of CLASS does not have slot SLOT.
Return t if it does, and return 'default if the default has changed."
  (let ((df nil) (err t)
	(scoped-class (class-parent class))
	(eieio-skip-typecheck))
    (condition-case nil
	(setq df (eieio-oref-default (class-parent class) slot)
	      err nil)
      (invalid-slot-name (setq df nil))
      (error (setq df nil)))
    (if err
	nil
      (if (equal df (eieio-oref-default class slot))
	  t
	'default))))

(defun eieiodoc-texify-docstring (string class)
  "Take STRING, (a normal doc string), and convert it into a texinfo string.
For instances where CLASS is the class being referenced, do not Xref
that class.

 `function' => @dfn{function}
 `variable' => @code{variable}
 `class'    => @code{class} @xref{class}
 `unknown'  => @code{unknonwn}
 'quoteme   => @code{quoteme}
 non-nil    => non-@code{nil}
 t          => @code{t}
 :tag       => @code{:tag}
 [ stuff ]  => @code{[ stuff ]}
 Key        => @kbd{Key}"
  (while (string-match "`\\([-a-zA-Z0-9]+\\)'" string)
    (let* ((vs (substring string (match-beginning 1) (match-end 1)))
	   (v (intern-soft vs)))
      (setq string
	    (concat
	     (replace-match (concat
			     (if (and (not (class-p v))(fboundp v))
				 "@dfn{" "@code{")
			     vs "}"
			     (if (and (class-p v) (not (eq v class)))
				 (concat " @xref{" vs "}.")))
			    nil t string)))))
  (while (string-match "\\( \\|^\\|-\\)\\(nil\\|t\\|'[-a-zA-Z0-9]+\\|:[-a-zA-Z0-9]+\\)\\([ ,]\\|$\\)" string)
    (setq string (replace-match "@code{\\2}" t nil string 2)))
  (while (string-match "\\( \\|^\\)\\(\\[[^]]+\\]\\)\\( \\|$\\)" string)
    (setq string (replace-match "@code{\\2}" t nil string 2)))
  (while (string-match "\\( \\|^\\)\\(\\(\\(C-\\|M-\\|S-\\)+\\([^ \t\n]\\|RET\\|SPC\\|TAB\\)\\)\\|\\(RET\\|SPC\\|TAB\\)\\)\\( \\|$\\)" string)
    (setq string (replace-match "@kbd{\\2}" t nil string 2)))
  string)

(provide 'eieio-doc)

;;; eieio-doc.el ends here
