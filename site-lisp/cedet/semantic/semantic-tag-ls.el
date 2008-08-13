;;; semantic-tag-ls.el --- Language Specific override functions for tags

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2006, 2007, 2008 Eric M. Ludlam

;; X-CVS: $Id: semantic-tag-ls.el,v 1.15 2008/06/10 00:43:39 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; There are some features of tags that are too langauge dependent to
;; put in the core `semantic-tag' functionality.  For instance, the
;; protection of a tag (as specified by UML) could be almost anything.
;; In Java, it is a type specifier.  In C, there is a label.  This
;; informatin can be derived, and thus should not be stored in the tag
;; itself.  These are the functions that languages can use to derive
;; the information.

(require 'semantic-tag)

;;; Code:

;;; UML features:
;;
;; UML can represent several types of features of a tag
;; such as the `protection' of a symbol, or if it is abstract,
;; leaf, etc.  Learn about UML to catch onto the lingo.

;;;###autoload
(define-overloadable-function semantic-tag-calculate-parent (tag)
  "Attempt to calculate the parent of TAG.
The default behavior (if not overriden with `tag-calculate-parent')
is to search a buffer found with TAG, and if externally defined,
search locally, then semanticdb for that tag (when enabled.)")

(defun semantic-tag-calculate-parent-default (tag)
  "Attempt to calculate the parent of TAG."
  (when (semantic-tag-in-buffer-p tag)
    (save-excursion
      (set-buffer (semantic-tag-buffer tag))
      (save-excursion
	(goto-char (semantic-tag-start tag))
	(semantic-current-tag-parent))
      )))

;;;###autoload
(define-overloadable-function semantic-tag-protection (tag &optional parent)
  "Return protection information about TAG with optional PARENT.
This function returns on of the following symbols:
   nil        - No special protection.  Language dependent.
   'public    - Anyone can access this TAG.
   'private   - Only methods in the local scope can access TAG.
   'protected - Like private for outside scopes, like public for child
                classes.
Some languages may choose to provide additional return symbols specific
to themselves.  Use of this function should allow for this.

The default behavior (if not overridden with `tag-protection'
is to return a symbol based on type modifiers."
  (and (not parent)
       (semantic-tag-overlay tag)
       (semantic-tag-in-buffer-p tag)
       (setq parent (semantic-tag-calculate-parent tag)))
  (:override))

(make-obsolete-overload 'semantic-nonterminal-protection
                        'semantic-tag-protection)

(defun semantic-tag-protection-default (tag &optional parent)
  "Return the protection of TAG as a child of PARENT default action.
See `semantic-tag-protection'."
  (let ((mods (semantic-tag-modifiers tag))
	(prot nil))
    (while (and (not prot) mods)
      (if (stringp (car mods))
	  (let ((s (car mods)))
	    (setq prot
		  ;; A few silly defaults to get things started.
		  (cond ((or (string= s "public")
			     (string= s "extern")
			     (string= s "export"))
			 'public)
			((string= s "private")
			 'private)
			((string= s "protected")
			 'protected)))))
      (setq mods (cdr mods)))
    prot))

;;;###autoload
(defun semantic-tag-protected-p (tag protection &optional parent)
  "Non-nil if TAG is is protected.
PROTECTION is a symbol which can be returned by the method
`semantic-tag-protection'.
PARENT is the parent data type which contains TAG.

For these PROTECTIONs, true is returned if TAG is:
@table @asis
@item nil
  Always true
@item  private
  True if nil.
@item protected
  True if private or nil.
@item public
  True if private, protected, or nil.
@end table"
  (if (null protection)
      t
    (let ((tagpro (semantic-tag-protection tag parent)))
      (or (and (eq protection 'private)
	       (null tagpro))
	  (and (eq protection 'protected)
	       (or (null tagpro)
		   (eq tagpro 'private)))
	  (and (eq protection 'public)
	       (not (eq tagpro 'public)))))
    ))

;;;###autoload
(define-overloadable-function semantic-tag-abstract-p (tag &optional parent)
  "Return non nil if TAG is abstract.
Optional PARENT is the parent tag of TAG.
In UML, abstract methods and classes have special meaning and behavior
in how methods are overridden.  In UML, abstract methods are italicized.

The default behavior (if not overridden with `tag-abstract-p'
is to return true if `abstract' is in the type modifiers.")

(make-obsolete-overload 'semantic-nonterminal-abstract
                        'semantic-tag-abstract-p)

(defun semantic-tag-abstract-p-default (tag &optional parent)
  "Return non-nil if TAG is abstract as a child of PARENT default action.
See `semantic-tag-abstract-p'."
  (let ((mods (semantic-tag-modifiers tag))
	(abs nil))
    (while (and (not abs) mods)
      (if (stringp (car mods))
	  (setq abs (or (string= (car mods) "abstract")
			(string= (car mods) "virtual"))))
      (setq mods (cdr mods)))
    abs))

;;;###autoload
(define-overloadable-function semantic-tag-leaf-p (tag &optional parent)
  "Return non nil if TAG is leaf.
Optional PARENT is the parent tag of TAG.
In UML, leaf methods and classes have special meaning and behavior.

The default behavior (if not overridden with `tag-leaf-p'
is to return true if `leaf' is in the type modifiers.")

(make-obsolete-overload 'semantic-nonterminal-leaf
                        'semantic-tag-leaf-p)

(defun semantic-tag-leaf-p-default (tag &optional parent)
  "Return non-nil if TAG is leaf as a child of PARENT default action.
See `semantic-tag-leaf-p'."
  (let ((mods (semantic-tag-modifiers tag))
	(leaf nil))
    (while (and (not leaf) mods)
      (if (stringp (car mods))
	  ;; Use java FINAL as example default.  There is none
	  ;; for C/C++
	  (setq leaf (string= (car mods) "final")))
      (setq mods (cdr mods)))
    leaf))

;;;###autoload
(define-overloadable-function semantic-tag-static-p (tag &optional parent)
  "Return non nil if TAG is static.
Optional PARENT is the parent tag of TAG.
In UML, static methods and attributes mean that they are allocated
in the parent class, and are not instance specific.
UML notation specifies that STATIC entries are underlined.")

(defun semantic-tag-static-p-default (tag &optional parent)
  "Return non-nil if TAG is static as a child of PARENT default action.
See `semantic-tag-static-p'."
  (let ((mods (semantic-tag-modifiers tag))
	(static nil))
    (while (and (not static) mods)
      (if (stringp (car mods))
	  (setq static (string= (car mods) "static")))
      (setq mods (cdr mods)))
    static))

;;;###autoload
(define-overloadable-function semantic-tag-prototype-p (tag)
  "Return non nil if TAG is a prototype.
For some laguages, such as C, a prototype is a declaration of
something without an implementation."
  )

(defun semantic-tag-prototype-p-default (tag)
  "Non-nil if TAG is a prototype."
  (let ((p (semantic-tag-get-attribute tag :prototype-flag)))
    (cond
     ;; Trust the parser author.
     (p p)
     ;; Empty types might be a prototype.
     ;; @todo - make this better.
     ((eq (semantic-tag-class tag) 'type)
      (not (semantic-tag-type-members tag)))
     ;; No other heuristics.
     (t nil))
    ))

;;; FULL NAMES
;;
;; For programmer convenience, a full name is not specified in source
;; code.  Instead some abbreviation is made, and the local environment
;; will contain the info needed to determine the full name.

;;;###autoload
(define-overloadable-function semantic-tag-full-name (tag &optional stream-or-buffer)
  "Return the fully qualified name of TAG in the package hierarchy.
STREAM-OR-BUFFER can be anything convertable by `semantic-something-to-stream',
but must be a toplevel semantic tag stream that contains TAG.
A Package Hierarchy is defined in UML by the way classes and methods
are organized on disk.  Some language use this concept such that a
class can be accessed via it's fully qualified name, (such as Java.)
Other languages qualify names within a Namespace (such as C++) which
result in a different package like structure.  Languages which do not
override this function with `tag-full-name' will use
`semantic-tag-name'.  Override functions only need to handle
STREAM-OR-BUFFER with a tag stream value, or nil."
  (let ((stream (semantic-something-to-tag-table
                 (or stream-or-buffer tag))))
    (:override-with-args (tag stream))))

(make-obsolete-overload 'semantic-nonterminal-full-name
                        'semantic-tag-full-name)

(defun semantic-tag-full-name-default (tag stream)
  "Default method for `semantic-tag-full-name'.
Return the name of TAG found in the toplevel STREAM."
  (semantic-tag-name tag))

;;; Compatibility aliases.
;;
(semantic-alias-obsolete 'semantic-nonterminal-protection
			 'semantic-tag-protection)
(semantic-alias-obsolete 'semantic-nonterminal-protection-default
			 'semantic-tag-protection-default)
(semantic-alias-obsolete 'semantic-nonterminal-abstract
			 'semantic-tag-abstract-p)
(semantic-alias-obsolete 'semantic-nonterminal-abstract-default
			 'semantic-tag-abstract-p-default)
(semantic-alias-obsolete 'semantic-nonterminal-leaf
			 'semantic-tag-leaf-p)
(semantic-alias-obsolete 'semantic-nonterminal-leaf-default
			 'semantic-tag-leaf-p-default)
(semantic-alias-obsolete 'semantic-nonterminal-static-default
			 'semantic-tag-static-p-default)
(semantic-alias-obsolete 'semantic-nonterminal-full-name
			 'semantic-tag-full-name)
(semantic-alias-obsolete 'semantic-nonterminal-full-name-default
			 'semantic-tag-full-name-default)

;; TEMPORARY within betas of CEDET 1.0
(semantic-alias-obsolete 'semantic-tag-static 'semantic-tag-static-p)
(semantic-alias-obsolete 'semantic-tag-leaf 'semantic-tag-leaf-p)
(semantic-alias-obsolete 'semantic-tag-abstract 'semantic-tag-abstract-p)


(provide 'semantic-tag-ls)

;;; semantic-tag-ls.el ends here
