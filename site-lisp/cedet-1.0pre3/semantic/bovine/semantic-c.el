;;; semantic-c.el --- Semantic details for C

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-c.el,v 1.37 2005/04/15 16:55:19 zappo Exp $

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;

;;; History:
;; 

(require 'semantic)
(require 'semantic-c-by)
(require 'backquote)

(eval-when-compile
  (require 'semantic-ctxt)
  (require 'semantic-imenu)
  (require 'semantic-tag-ls)
  (require 'document)
  (require 'senator)
  (require 'cc-mode))

;;; Code:
(define-lex-regex-analyzer semantic-lex-c-if-0
  "Block out code matched in an #if 0 condition."
  "^\\s-*#if\\s-*0$"
  (beginning-of-line)
  (c-forward-conditional 1)
  (setq semantic-lex-end-point (point))
  nil)

;;; Compatibility
;;
(if (fboundp 'c-end-of-macro)
    (defalias 'semantic-c-end-of-macro 'c-end-of-macro)
  ;; From cc-mode 5.30
  (defun semantic-c-end-of-macro ()
    "Go to the end of a preprocessor directive.
More accurately, move point to the end of the closest following line
that doesn't end with a line continuation backslash.

This function does not do any hidden buffer changes."
    (while (progn
             (end-of-line)
             (when (and (eq (char-before) ?\\)
                        (not (eobp)))
               (forward-char)
               t))))
  )

(define-lex-regex-analyzer semantic-lex-c-if
  "Ignore various forms of #if/#else/#endif conditionals."
  "^#\\(if\\(def\\)?\\|el\\(if\\|se\\)\\|endif\\)"
  (semantic-c-end-of-macro)
  (setq semantic-lex-end-point (point))
  nil)

(define-lex-analyzer semantic-lex-c-include-system
  "Identify system include strings, and return special tokens."
  (and (looking-at "<[^\n>]+>")
       (save-excursion
	 (beginning-of-line)
	 (looking-at "\\s-*#\\s-*include\\s-+<"))
       (= (match-end 0) (1+ (point))))
  ;; We found a system include.
  (let ((start (point)))
    ;; This should always pass
    (re-search-forward ">")
    ;; We have the whole thing.
    (semantic-lex-push-token
     (semantic-lex-token 'system-include start (point)))
    )
  )

(define-lex-regex-analyzer semantic-lex-c-ignore-ending-backslash
  "Skip backslash ending a line.
Go to the next line."
  "\\\\\\s-*\n"
  (setq semantic-lex-end-point (match-end 0)))

(define-lex-regex-analyzer semantic-lex-c-string
  "Detect and create a C string token."
  "L?\\(\\s\"\\)"
  ;; Zing to the end of this string.
  (semantic-lex-push-token
   (semantic-lex-token
    'string (point)
    (save-excursion
      ;; Skip L prefix if present.
      (goto-char (match-beginning 1))
      (semantic-lex-unterminated-syntax-protection 'string
	(forward-sexp 1)
	(point))
      ))))

(define-lex semantic-c-lexer
  "Lexical Analyzer for C code."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-c-if-0
  semantic-lex-c-if
  semantic-lex-c-include-system
  semantic-lex-c-ignore-ending-backslash
  semantic-lex-number
  ;; Must detect C strings before symbols because of possible L prefix!
  semantic-lex-c-string
  semantic-lex-symbol-or-keyword
  semantic-lex-charquote
  semantic-lex-paren-or-list
  semantic-lex-close-paren
  semantic-lex-ignore-comments
  semantic-lex-punctuation
  semantic-lex-default-action)

(defun semantic-expand-c-tag (tag)
  "Expand TAG into a list of equivalent tags, or nil."
  (cond ((eq (semantic-tag-class tag) 'extern)
	 ;; We have hit an exter "C" command with a list after it.
	 (let* ((mb (semantic-tag-get-attribute tag :members))
		(ret mb))
	   (while mb
	     (let ((mods (semantic-tag-get-attribute (car mb) :typemodifiers)))
	       (setq mods (cons "extern" (cons "\"C\"" mods)))
	       (semantic-tag-put-attribute (car mb) :typemodifiers mods))
	     (setq mb (cdr mb)))
	   ret))
	((listp (car tag))
	 (cond ((eq (semantic-tag-class tag) 'variable)
		;; The name part comes back in the form of:
		;; ( NAME NUMSTARS BITS ARRAY ASSIGN )
		(let ((vl nil)
		      (basety (semantic-tag-type tag))
		      (ty "")
		      (mods (semantic-tag-get-attribute tag :typemodifiers))
		      (suffix "")
		      (lst (semantic-tag-name tag))
		      (default nil)
		      (cur nil))
		  (while lst
		    (setq suffix "" ty "")
		    (setq cur (car lst))
		    (if (nth 2 cur)
			(setq suffix (concat ":" (nth 2 cur))))
		    (if (= (length basety) 1)
			(setq ty (car basety))
		      (setq ty basety))
		    (setq default (nth 4 cur))
		    (setq vl (cons
			      (semantic-tag-new-variable
			       (car cur) ;name
			       ty	;type
			       (if default
				   (buffer-substring-no-properties
				    (car default) (car (cdr default))))
			       :constant-flag (semantic-tag-variable-constant-p tag)
			       :suffix suffix
			       :typemodifiers mods
			       :dereference (length (nth 3 cur))
			       :pointer (nth 1 cur)
			       :documentation (semantic-tag-docstring tag) ;doc
			       )
			      vl))
		    (semantic--tag-copy-properties tag (car vl))
		    (semantic--tag-set-overlay (car vl)
					       (semantic-tag-overlay tag))
		    (setq lst (cdr lst)))
		  vl))
	       ((eq (semantic-tag-class tag) 'type)
		;; We may someday want to add an extra check for a type
		;; of type "typedef".
		;; Each elt of NAME is ( STARS NAME )
		(let ((vl nil)
		      (names (semantic-tag-name tag)))
		  (while names
		    (setq vl (cons (semantic-tag-new-type
				    (nth 1 (car names)) ; name
				    "typedef"
				    (semantic-tag-type-members tag)
				    ;; parent is just tbe name of what
				    ;; is passed down as a tag.
				    (list
				     (semantic-tag-name
				      (semantic-tag-type-superclasses tag)))
				    :pointer
				    (let ((stars (car (car (car names)))))
				      (if (= stars 0) nil stars))
				    ;; This specifies what the typedef
				    ;; is expanded out as.  Just the
				    ;; name shows up as a parent of this
				    ;; typedef.
				    :typedef
				    (semantic-token-type-parent tag)
				    :documentation
				    (semantic-tag-docstring tag))
				   vl))
		    (semantic--tag-copy-properties tag (car vl))
		    (semantic--tag-set-overlay (car vl)
					       (semantic-tag-overlay tag))
		    (setq names (cdr names)))
		  vl))
	       ((and (listp (car tag))
		     (eq (semantic-tag-class (car tag)) 'variable))
		;; Argument lists come in this way.  Append all the expansions!
		(let ((vl nil))
		  (while tag
		    (setq vl (append (semantic-tag-components (car vl))
				     vl)
			  tag (cdr tag)))
		  vl))
	       (t nil)))
	(t nil)))

(defvar-mode-local c-mode semantic-tag-expand-function 'semantic-expand-c-tag
  "Function used to expand tags generated in the C bovine parser.")

(defvar semantic-c-classname nil
  "At parse time, assign a class or struct name text here.
It is picked up by `semantic-c-reconstitute-token' to determine
if something is a constructor.  Value should be:
  ( TYPENAME .  TYPEOFTYPE)
where typename is the name of the type, and typeoftype is \"class\"
or \"struct\".")

(defun semantic-c-reconstitute-token (tokenpart declmods typedecl)
  "Reconstitute a token TOKENPART with DECLMODS and TYPEDECL.
This is so we don't have to match the same starting text several times.
Optional argument STAR and REF indicate the number of * and & in the typedef."
  (when (and (listp typedecl)
	     (= 1 (length typedecl))
	     (stringp (car typedecl)))
    (setq typedecl (car typedecl)))
  (cond ((eq (nth 1 tokenpart) 'variable)
	 (semantic-tag-new-variable
	  (car tokenpart)
	  (or typedecl "int")	;type
	  nil			;default value (filled with expand)
	  :constant-flag (if (member "const" declmods) t nil)
	  :typemodifiers (delete "const" declmods)
	  )
	 )
	((eq (nth 1 tokenpart) 'function)
	 ;; We should look at part 4 (the arglist) here, and throw an
	 ;; error of some sort if it contains parser errors so that we
	 ;; don't parser function calls, but that is a little beyond what
	 ;; is available for data here.
	 (let* ((constructor
		 (and (or (and semantic-c-classname
			       (string= (car semantic-c-classname)
					(car tokenpart)))
			  (and (stringp (car (nth 2 tokenpart)))
			       (string= (car (nth 2 tokenpart)) (car tokenpart)))
			  )
		      (not (car (nth 3 tokenpart)))))
		(fcnpointer (string-match "^\\*" (car tokenpart)))
		(fnname (if fcnpointer
			    (substring (car tokenpart) 1)
			  (car tokenpart))))
	   (if fcnpointer
	       ;; Function pointers are really variables.
	       (semantic-tag-new-variable
		fnname
		typedecl
		nil
		;; It is a function pointer
		:functionpointer-flag t
		)
	     ;; The function
	     (semantic-tag-new-function
	      fnname
	      (or typedecl		;type
		  (cond ((car (nth 3 tokenpart) )
			 "void")	; Destructors have no return?
			(constructor
			 ;; Constructors return an object.			  ;; in our
			 (list (or (car semantic-c-classname)
				   (car (nth 2 tokenpart)))
			       'type
			       (or (cdr semantic-c-classname)
				   "class")))
			(t "int")))
	      (nth 4 tokenpart)		;arglist
	      :constant-flag (if (member "const" declmods) t nil)
	      :typemodifiers (delete "const" declmods)
	      :parent (car (nth 2 tokenpart))
	      :destructor-flag (if (car (nth 3 tokenpart) ) t)
	      :constructor-flag (if constructor t)
	      :pointer (nth 7 tokenpart)
	      ;; Even though it is "throw" in C++, we use
	      ;; `throws' as a common name for things that toss
	      ;; exceptions about.
	      :throws (nth 5 tokenpart)
	      ;; Reemtrant is a C++ thingy.  Add it here
	      :reentrant-flag (if (member "reentrant" (nth 6 tokenpart)) t)
	      ;; A function post-const is funky.  Try stuff
	      :methodconst-flag (if (member "const" (nth 6 tokenpart)) t)
	      ;; prototypes are functions w/ no body
	      :prototype-flag (if (nth 8 tokenpart) t)
	      ;; Pure virtual
	      :pure-virtual-flag (if (eq (nth 8 tokenpart) :pure-virtual-flag) t)
	      )))
	 )
	))

(defun semantic-c-reconstitute-template (tag specifier)
  "Reconstitute the token TAG with the template SPECIFIER."
  (semantic-tag-put-attribute tag :template (or specifier ""))
  tag)

;;; Override methods & Variables
;;
(defcustom semantic-default-c-path '("/usr/include" "/usr/dt/include"
					 "/usr/X11R6/include")
  "Default set of include paths for C code.
Used by `semantic-inc' to define an include path.  This should
probably do some sort of search to see what is actually on the local
machine."
  :group 'c
  :group 'semantic
  :type '(repeat (string :tag "Path")))

(defvar-mode-local c-mode semantic-dependency-include-path 
  semantic-default-c-path
  "System path to search for include files.")

(define-mode-local-override semantic-format-tag-name
  c-mode (tag &optional parent color)
  "Convert TAG to a string that is the print name for TAG.
Optional PARENT and COLOR are ignored."
  (let ((name (semantic-format-tag-name-default tag parent color))
	(fnptr (semantic-tag-get-attribute tag :functionpointer-flag))
	)
    (if (not fnptr)
	name
      (concat "(*" name ")"))
    ))

(define-mode-local-override semantic-tag-protection
  c-mode (token &optional parent)
  "Return the protection of TOKEN in PARENT.
Override function for `semantic-tag-protection'."
  (let ((mods (semantic-tag-modifiers token))
	(prot nil))
    ;; Check the modifiers for protection if we are not a child
    ;; of some class type.
    (when (or (not parent) (not (eq (semantic-tag-class parent) 'type)))
      (while (and (not prot) mods)
	(if (stringp (car mods))
	    (let ((s (car mods)))
	      ;; A few silly defaults to get things started.
	      (cond ((or (string= s "extern")
			 (string= s "export"))
		     'public)
		    ((string= s "static")
		     'private))))
	(setq mods (cdr mods))))
    ;; If we have a typed parent, look for :public style labels.
    (when (and parent (eq (semantic-tag-class parent) 'type))
      (let ((pp (semantic-tag-type-members parent)))
	(while (and pp (not (semantic-equivalent-tag-p (car pp) token)))
	  (when (eq (semantic-tag-class (car pp)) 'label)
	    (setq prot
		  (cond ((string= (semantic-tag-name (car pp)) "public")
			 'public)
			((string= (semantic-tag-name (car pp)) "private")
			 'private)
			((string= (semantic-tag-name (car pp)) "protected")
			 'protected)))
	    )
	  (setq pp (cdr pp)))))
    (when (and (not prot) (eq (semantic-tag-class parent) 'type))
      (setq prot
	    (cond ((string= (semantic-tag-type parent) "class") 'private)
		  ((string= (semantic-tag-type parent) "struct") 'public)
		  (t 'unknown))))
    (or prot
	(if (and parent (semantic-tag-of-class-p parent 'type))
	    'public
	  nil))))

(define-mode-local-override semantic-tag-components c-mode (tag)
  "Return components for TAG."
  (if (and (eq (semantic-tag-class tag) 'type)
	   (string= (semantic-tag-type tag) "typedef"))
      ;; A typedef can contain a parent who has positional children,
      ;; but that parent will not have a position.  Do this funny hack
      ;; to make sure we can apply overlays properly.
      (semantic-tag-components (semantic-tag-type-superclasses tag))
    (semantic-tag-components-default tag)))

(define-mode-local-override semantic-format-tag-type c-mode (tag color)
  "Convert the data type of TAG to a string usable in tag formatting.
Adds pointer and reference symbols to the default.
Argument COLOR adds color to the text."
  (let* ((type (semantic-format-tag-type-default tag color))
	 (point (semantic-tag-get-attribute tag :pointer))
	 (ref (semantic-tag-get-attribute tag :reference))
	 )
    (if ref (setq ref "&"))
    (if point (setq point (make-string point ?*)) "")
    (when type
      (concat type ref point))
    ))

(defun semantic-c-tag-template (tag)
  "Return the template specification for TAG, or nil."
  (semantic-tag-get-attribute tag :template))

(defun semantic-c-tag-template-specifier (tag)
  "Return the template specifier specification for TAG, or nil."
  (semantic-tag-get-attribute tag :template-specifier))

(defun semantic-c-template-string-body (templatespec)
  "Convert TEMPLATESPEC into a string.
This might be a string, or a list of tokens."
  (cond ((stringp templatespec)
	 templatespec)
	((semantic-tag-p templatespec)
	 (semantic-format-tag-abbreviate templatespec))
	((listp templatespec)
	 (mapconcat 'semantic-format-tag-abbreviate templatespec ", "))))

(defun semantic-c-template-string (token &optional parent color)
  "Return a string representing the TEMPLATE attribute of TOKEN.
This string is prefixed with a space, or is the empty string.
Argument PARENT specifies a parent type.
Argument COLOR specifies that the string should be colorized."
  (let ((t2 (semantic-c-tag-template-specifier token))
	(t1 (semantic-c-tag-template token))
	(pt1 (if parent (semantic-c-tag-template parent)))
	(pt2 (if parent (semantic-c-tag-template-specifier parent)))
	)
    (cond (t2 ;; we have a template with specifier
	   (concat " <"
		   ;; Fill in the parts here
		   (semantic-c-template-string-body t2)
		   ">"))
	  (t1 ;; we have a template without specifier
	   " <>")
	  (t
	   ""))))

(define-mode-local-override semantic-format-tag-concise-prototype
  c-mode (token &optional parent color)
  "Return an abbreviated string describing TOKEN for C and C++.
Optional PARENT and COLOR as specified with
`semantic-format-tag-abbreviate-default'."
  ;; If we have special template things, append.
  (concat  (semantic-format-tag-concise-prototype-default token parent color)
	   (semantic-c-template-string token parent color)))

(define-mode-local-override semantic-format-tag-uml-prototype
  c-mode (token &optional parent color)
  "Return an uml string describing TOKEN for C and C++.
Optional PARENT and COLOR as specified with
`semantic-abbreviate-tag-default'."
  ;; If we have special template things, append.
  (concat  (semantic-format-tag-uml-prototype-default token parent color)
	   (semantic-c-template-string token parent color)))

(define-mode-local-override semantic-tag-abstract-p
  c-mode (tag &optional parent)
  "Return non-nil if TAG is considered abstract.
PARENT is tag's parent.
In C, a method is abstract if it is `virtual', which is already
handled.  A class is abstract iff it's destructor is virtual."
  (cond
   ((eq (semantic-tag-class tag) 'type)
    (or (semantic-brute-find-tag-by-attribute :pure-virtual-flag
					      (semantic-tag-components tag)
					      )
	(let* ((ds (semantic-brute-find-tag-by-attribute
		    :destructor-flag
		    (semantic-tag-components tag)
		    ))
	       (cs (semantic-brute-find-tag-by-attribute
		    :constructor-flag
		    (semantic-tag-components tag)
		    )))
	  (and ds (member "virtual" (semantic-tag-modifiers (car ds)))
	       cs (eq 'protected (semantic-tag-protection (car cs) tag))
	       )
	  )))
   ((eq (semantic-tag-class tag) 'function)
    (or (semantic-tag-get-attribute tag :pure-virtual-flag)
        (member "virtual" (semantic-tag-modifiers tag))))
   (t (semantic-tag-abstract-p-default tag parent))))

(define-mode-local-override semantic-analyze-dereference-metatype
  c-mode (type)
  "Dereference TYPE as described in `semantic-analyze-dereference-metatype'.
If TYPE is a typedef, get TYPE's type by name or tag, and return."
  (if (and (eq (semantic-tag-class type) 'type)
	   (string= (semantic-tag-type type) "typedef"))
      (semantic-tag-get-attribute type :typedef)
    type))

(define-mode-local-override semantic-analyze-type-constants c-mode (type)
  "When TYPE is a tag for an enum, return it's parts.
These are constants which are of type TYPE."
  (if (and (eq (semantic-tag-class type) 'type)
	   (string= (semantic-tag-type type) "enum"))
      (semantic-tag-type-members type)))

(define-mode-local-override semantic-ctxt-scoped-types c-mode (&optional point)
  "Return a list of tags of CLASS type based on POINT.
DO NOT return the list of tags encompassing point."
  (when point (goto-char (point)))
  (let ((tagreturn nil)
	(tmp nil))
    ;; In C++, we want to find all the namespaces declared
    ;; locally and add them to the list.
    (setq tmp (semantic-find-tags-by-class 'type (current-buffer)))
    (setq tmp (semantic-find-tags-by-type "namespace" tmp))
    (setq tagreturn tmp)
    ;; We should also find all "using" type statements and
    ;; accept those entities in as well.

    ;; Return the stuff
    tagreturn
    ))

(defvar-mode-local c-mode semantic-orphaned-member-metaparent-type "struct"
  "When lost memberes are found in the class hierarchy generator, use a struct.")

(defvar-mode-local c-mode semantic-symbol->name-assoc-list
  '((type     . "Types")
    (variable . "Variables")
    (function . "Functions")
    (include  . "Includes")
    )
  "List of tag classes, and strings to describe them.")

(defvar-mode-local c-mode semantic-symbol->name-assoc-list-for-type-parts
  '((type     . "Types")
    (variable . "Attributes")
    (function . "Methods")
    (label    . "Labels")
    )
  "List of tag classes in a datatype decl, and strings to describe them.")

(defvar-mode-local c-mode imenu-create-index-function 'semantic-create-imenu-index
  "Imenu index function for C.")

(defvar-mode-local c-mode semantic-type-relation-separator-character '("." "->")
  "Separator characters between something of a give type, and a field.")

(defvar-mode-local c-mode semantic-command-separation-character ";"
  "Commen separation character for C")

(defvar-mode-local c-mode document-comment-start "/*"
  "Comment start string.")

(defvar-mode-local c-mode document-comment-line-prefix " *"
  "Tween line comment decoration character.")

(defvar-mode-local c-mode document-comment-end " */"
  "Comment termination string.")

(defvar-mode-local c-mode senator-step-at-tag-classes '(function variable)
  "Tag classes where senator will stop at the end.")

;;;###autoload
(defun semantic-default-c-setup ()
  "Set up a buffer for semantic parsing of the C language."
  (semantic-c-by--install-parser)
  (setq semantic-lex-syntax-modifications '((?> ".")
                                            (?< ".")
                                            )
        )
  
  (setq semantic-lex-analyzer #'semantic-c-lexer))

;;;###autoload
(add-hook 'c-mode-hook 'semantic-default-c-setup)
;;;###autoload
(add-hook 'c++-mode-hook 'semantic-default-c-setup)

(define-child-mode c++-mode c-mode
  "`c++-mode' uses the same parser as `c-mode'.")

(provide 'semantic-c)

;;; semantic-c.el ends here
