;;; semantic-analyze-complete.el --- Smart Completions

;; Copyright (C) 2007, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-analyze-complete.el,v 1.12 2009/01/31 18:28:21 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Caclulate smart completions.
;;
;; Uses the analyzer context routine to determine the best possible
;; list of completions.
;;
;;; History:
;;
;; Code was moved here from semantic-analyze.el

(require 'semantic-analyze)

;;; Code:

;;; Helper Fcns
;;
;;
(define-overloadable-function semantic-analyze-type-constants (type)
  "For the tag TYPE, return any constant symbols of TYPE.
Used as options when completing."
  (let ((ans
         (:override-with-args
             ((semantic-analyze-find-tag (semantic-tag-name type)))
           ;; Be default, we don't know.
           nil))
        (out nil))
    (dolist (elt ans)
      (cond
       ((stringp elt)
        (push (semantic-tag-new-variable
               elt (semantic-tag-name type) nil)
              out))
       ((semantic-tag-p elt)
        (push elt out))
       (t nil)))
    (nreverse out)))

;;;###autoload
(defun semantic-analyze-tags-of-class-list (tags classlist)
  "Return the tags in TAGS that are of classes in CLASSLIST."
  (let ((origc tags))
    ;; Accept only tags that are of the datatype specified by
    ;; the desired classes.
    (setq tags (apply 'nconc ;; All input lists are permutable.
		      (mapcar (lambda (class)
				(semantic-find-tags-by-class class origc))
			      classlist)))
    tags))

;;; MAIN completion calculator
;;
;;
;;;###autoload
(define-overloadable-function semantic-analyze-possible-completions (context)
  "Return a list of semantic tags which are possible completions.
CONTEXT is either a position (such as point), or a precalculated
context.  Passing in a context is useful if the caller also needs
to access parts of the analysis.
Completions run through the following filters:
  * Elements currently in scope
  * Constants currently in scope
  * Elements match the :prefix in the CONTEXT.
  * Type of the completion matches the type of the context.
Context type matching can identify the following:
  * No specific type
  * Assignment into a variable of some type.
  * Argument to a function with type constraints.
When called interactively, displays the list of possible completions
in a buffer."
  (interactive "d")
  ;; In theory, we don't need the below since the context will
  ;; do it for us.
  ;;(semantic-refresh-tags-safe)
  (with-syntax-table semantic-lex-syntax-table
    (let* ((context (if (semantic-analyze-context-child-p context)
                        context
                      (semantic-analyze-current-context context)))
	   (ans (if (not context)
		    (error "Nothing to Complete.")
		  (:override))))
      ;; If interactive, display them.
      (when (interactive-p)
	(with-output-to-temp-buffer "*Possible Completions*"
	  (semantic-analyze-princ-sequence ans "" (current-buffer)))
	(shrink-window-if-larger-than-buffer
	 (get-buffer-window "*Possible Completions*")))
      ans)))

(defun semantic-analyze-possible-completions-default (context)
  "Default method for producing smart completions.
Argument CONTEXT is an object specifying the locally derived context."
  (let* ((a context)
	 (desired-type (semantic-analyze-type-constraint a))
	 (desired-class (oref a prefixclass))
	 (prefix (oref a prefix))
	 (prefixtypes (oref a prefixtypes))
	 (completetext nil)
	 (completetexttype nil)
	 (scope (oref a scope))
	 (localvar (oref scope localvar))
	 (c nil))

    ;; Calculate what our prefix string is so that we can
    ;; find all our matching text.
    (setq completetext (car (reverse prefix)))
    (if (semantic-tag-p completetext)
	(setq completetext (semantic-tag-name completetext)))

    (if (and (not completetext) (not desired-type))
	(error "Nothing to complete"))

    (if (not completetext) (setq completetext ""))

    ;; This better be a reasonable type, or we should fry it.
    ;; The prefixtypes should always be at least 1 less than
    ;; the prefix since the type is never looked up for the last
    ;; item when calculating a sequence.
    (setq completetexttype (car (reverse prefixtypes)))
    (when (or (not completetexttype)
	      (not (and (semantic-tag-p completetexttype)
			(eq (semantic-tag-class completetexttype) 'type))))
      ;; What should I do here?  I think this is an error condition.
      (setq completetexttype nil)
      ;; If we had something that was a completetexttype but it wasn't
      ;; valid, then express our dismay!
      (when (> (length prefix) 1)
	(let* ((errprefix (car (cdr (reverse prefix)))))
	  (error "Cannot find types for `%s'"
		 (cond ((semantic-tag-p errprefix)
			(semantic-format-tag-prototype errprefix))
		       (t
			(format "%S" errprefix)))))
	))

    ;; There are many places to get our completion stream for.
    ;; Here we go.
    (if completetexttype

	(setq c (semantic-find-tags-for-completion
		 completetext
		 (semantic-analyze-scoped-type-parts completetexttype scope)
		 ))
	      
      ;; No type based on the completetext.  This is a free-range
      ;; var or function.  We need to expand our search beyond this
      ;; scope into semanticdb, etc.
      (setq c (nconc
	       ;; Argument list and local variables
	       (semantic-find-tags-for-completion completetext localvar)
	       ;; The current scope
	       (semantic-find-tags-for-completion completetext (oref scope fullscope))
	       ;; The world
	       (semantic-analyze-find-tags-by-prefix completetext))
	    )
      )

    (let ((origc c)
	  (dtname (semantic-tag-name desired-type)))
	
      ;; Reset c.
      (setq c nil)

      ;; Loop over all the found matches, and catagorize them
      ;; as being possible features.
      (while origc

	(cond
	 ;; Strip operators
	 ((semantic-tag-get-attribute (car origc) :operator-flag)
	  nil
	  )
	 
	 ;; If we are completing from within some prefix,
	 ;; then we want to exclude constructors and destructors
	 ((and completetexttype
	       (or (semantic-tag-get-attribute (car origc) :constructor-flag)
		   (semantic-tag-get-attribute (car origc) :destructor-flag)))
	  nil
	  )

	 ;; If there is a desired type, we need a pair of restrictions
	 (desired-type

	  (cond
	   ;; Ok, we now have a completion list based on the text we found
	   ;; we want to complete on.  Now filter that stream against the
	   ;; type we want to search for.
	   ((string= dtname (semantic-analyze-tag-type-to-name (car origc)))
	    (setq c (cons (car origc) c))
	    )

	   ;; Now anything that is a compound type which could contain
	   ;; additional things which are of the desired type
	   ((semantic-tag-type (car origc))
	    (let ((att (semantic-analyze-tag-type (car origc) scope))
		)
	      (if (and att (semantic-tag-type-members att))
		  (setq c (cons (car origc) c))))
	    )
	   
	   ) ; cond
	  ); desired type

	 ;; No desired type, no other restrictions.  Just add.
	 (t
	  (setq c (cons (car origc) c)))

	 ); cond

	(setq origc (cdr origc)))

      (when desired-type
	;; Some types, like the enum in C, have special constant values that
	;; we could complete with.  Thus, if the target is an enum, we can
	;; find possible symbol values to fill in that value.
	(let ((constants
	       (semantic-analyze-type-constants desired-type)))
	  (if constants
	      (progn
		;; Filter
		(setq constants
		      (semantic-find-tags-for-completion
		       completetext constants))
		;; Add to the list
		(setq c (nconc c constants)))
	    )))
      )

    (when desired-class
      (setq c (semantic-analyze-tags-of-class-list c desired-class)))

    ;; Pull out trash.
    ;; NOTE TO SELF: Is this too slow?
    ;; OTHER NOTE: Do we not want to strip duplicates by name and
    ;; only by position?  When are duplicate by name but not by tag
    ;; useful?
    (setq c (semantic-unique-tag-table-by-name c))

    ;; All done!

    c))



(provide 'semantic-analyze-complete)
;;; semantic-analyze-complete.el ends here
