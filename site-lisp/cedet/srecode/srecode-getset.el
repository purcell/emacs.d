;;; srecode-getset.el --- 

;; Copyright (C) 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; SRecoder application for inserting new get/set methods into a class.

(require 'semantic)
(require 'semantic-analyze)
(require 'srecode-insert)
(require 'srecode-dictionary)

;;; Code:
(defcustom srecode-getset-template-file-alist
  '( ( c++-mode . "srecode-getset-cpp.srt" )
     )
  ;; @todo - Make this variable auto-generated from the Makefile.
  "List of template files for getsest associated with a given major mode."
  :group 'srecode
  :type '(repeat (cons (sexp :tag "Mode")
		       (sexp :tag "Filename"))
		 ))

;;;###autoload
(defun srecode-insert-getset ()
  "Insert get/set methods for the current class."
  (interactive)

  (srecode-load-tables-for-mode major-mode)
  (srecode-load-tables-for-mode major-mode srecode-getset-template-file-alist)

  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))

  ;; Step 1: Try to derive the tag for the class we will use
  (let* ((class (srecode-auto-choose-class (point)))
	 (inclass (eq (semantic-current-tag-of-class 'type) class))
	 (field nil)
	 )
    (when (not class)
      (error "Move point to a class and try again"))

    ;; Step 2: Select a name for the field we will use.
    (when inclass
      (setq field (srecode-auto-choose-field (point))))

    (when (not field)
      (setq field (srecode-query-for-field class)))

    ;; Step 3: Insert a new field if needed
    (when (stringp field)

      ;; Ack.. auto-create is hard.  Do it later.
      (error "You must select a pre-existing field")

      (srecode-position-new-field class inclass)

      ;; Step 3.5: Insert an initializer if needed.
      ;; ...
      )

    (if (not (semantic-tag-p field))
	(error "Must specify field for get/set.  (parts may not be impl'd yet.)"))

    ;; Set 4: Position for insertion of methods
    (srecode-position-new-methods class field)

    ;; Step 5: Insert the get/set methods
    (if (not (eq (semantic-current-tag) class))
	;; We are positioned on top of something else.
	;; insert a /n
	(insert "\n"))

    (let* ((dict (srecode-create-dictionary))
	   (srecode-semantic-selected-tag field)
	   (temp (srecode-template-get-table (srecode-table)
					     "getset-in-class"
					     "declaration"
					     'getset))
	   )
      (if (not temp)
	  (error "Getset templates for %s not loaded!" major-mode))
      (srecode-resolve-arguments temp dict)
      (srecode-dictionary-set-value dict "GROUPNAME"
				    (concat (semantic-tag-name field)
					    " Accessors"))
      (srecode-dictionary-set-value dict "NICENAME"
				    (srecode-strip-fieldname
				     (semantic-tag-name field)))
      (srecode-insert-fcn temp dict)
      )))

(defun srecode-strip-fieldname (name)
  "Strip the fieldname NAME of polish notation things."
  (cond ((string-match "[a-z]\\([A-Z]\\w+\\)" name)
	 (substring name (match-beginning 1)))
	;; Add more rules here.
	(t
	 name)))

(defun srecode-position-new-methods (class field)
  "Position the cursor in CLASS where new getset methods should go.
FIELD is the field for the get sets.
INCLASS specifies if the cursor is already in CLASS or not."
  (semantic-go-to-tag field)

  (let ((prev (semantic-find-tag-by-overlay-prev))
	(next (semantic-find-tag-by-overlay-next))
	(setname nil)
	(aftertag nil)
	)
    (cond
     ((and prev (semantic-tag-of-class-p prev 'variable))
      (setq setname
	    (concat "set"
		    (srecode-strip-fieldname (semantic-tag-name prev))))
      )
     ((and next (semantic-tag-of-class-p next 'variable))
      (setq setname
	    (concat "set"
		    (srecode-strip-fieldname (semantic-tag-name prev)))))
     (t nil))

    (setq aftertag (semantic-find-first-tag-by-name
		    setname (semantic-tag-type-members class)))

    (if (not aftertag)
	(setq aftertag (car-safe
			(semantic--find-tags-by-macro
			 (semantic-tag-get-attribute (car tags) :destructor-flag)
			 (semantic-tag-type-members class))))
      )

    (if (not aftertag)
	(setq aftertag (car-safe
			(semantic--find-tags-by-macro
			 (semantic-tag-get-attribute (car tags) :constructor-flag)
			 (semantic-tag-type-members class))))
      )

    (if (not aftertag)
	(setq aftertag (semantic-find-first-tag-by-name
			"public" (semantic-tag-type-members class))))

    (if (not aftertag)
	(setq aftertag (car (semantic-tag-type-members class))))

    (if aftertag
	(goto-char (semantic-tag-end aftertag))
      ;; At the beginning.
      (goto-char (semantic-tag-end class))
      (forward-sexp -1)
      (forward-char 1))

    (end-of-line)
    (forward-char 1)
    ))

(defun srecode-position-new-field (class inclass)
  "Select a position for a new field for CLASS.
If INCLASS is non-nil, then the cursor is already in the class
and should not be moved during point selection."
  
  ;; If we aren't in the class, get the cursor there, pronto!
  (when (not inclass)

    (let ((kids (semantic-find-tags-by-class
		 'variable (semantic-tag-type-members class))))
      (cond (kids
	     (semantic-go-to-tag (car kids) class))
	    (t
	     (semantic-go-to-tag class)))
      )

    (switch-to-buffer (current-buffer)))

  
  
  ;; Once the cursor is in our class, ask the user to position
  ;; the cursor to keep going.
  

  )



(defun srecode-auto-choose-field (point)
  "Choose a field for the get/set methods.
Base selection on the field related to POINT."
  (save-excursion
    (when point
      (goto-char point))

    (let ((field (semantic-current-tag-of-class 'variable)))
      
      ;; If we get a field, make sure the user gets a chance to choose.
      (when field
	(when (not (y-or-n-p
		    (format "Use field %s? " (semantic-tag-name field))))
	  (setq field nil))

      field))))

(defun srecode-query-for-field (class)
  "Query for a field in CLASS."
  (let* ((kids (semantic-find-tags-by-class
		'variable (semantic-tag-type-members class)))
	 (sel (completing-read "Use Field: " kids))
	 )

    (or (semantic-find-tags-by-name sel kids)
	sel)
    ))

(defun srecode-auto-choose-class (point)
  "Choose a class based on locatin of POINT."
  (save-excursion
    (when point
      (goto-char point))
    
    (let ((tag (semantic-current-tag-of-class 'type)))

      (when (or (not tag)
		(not (string= (semantic-tag-type tag) "class")))
	;; The current tag is not a class.  Are we in a fcn
	;; that is a method?
	(setq tag (semantic-current-tag-of-class 'function))

	(when (and tag
		   (semantic-tag-function-parent tag))
	  (let ((p (semantic-tag-function-parent tag)))
	    ;; @TODO : Copied below out of semantic-analyze
	    ;;         Turn into a routine.

	    (let* ((searchname (cond ((stringp p) p)
				     ((semantic-tag-p p)
				      (semantic-tag-name p))
				     ((and (listp p) (stringp (car p)))
				      (car p))))
		   (ptag (semantic-analyze-find-tag searchname
						    'type nil)))
	      (when ptag (setq tag ptag ))
	      ))))

      (when (or (not tag)
		(not (semantic-tag-of-class-p tag 'type))
		(not (string= (semantic-tag-type tag) "class")))
	;; We are not in a class that needs a get/set method.
	;; Analyze the current context, and derive a class name.
	(let* ((ctxt (semantic-analyze-current-context))
	       (pfix nil)
	       (ans nil))
	  (when ctxt
	    (setq pfix (reverse (oref ctxt prefix)))
	    (while (and (not ans) pfix)
	      ;; Start at the end and back up to the first class.
	      (when (and (semantic-tag-p (car pfix))
			 (semantic-tag-of-class-p (car pfix) 'type)
			 (string= (semantic-tag-type (car pfix))
				  "class"))
		(setq ans (car pfix)))
	      (setq pfix (cdr pfix))))
	  (setq tag ans)))

      tag)))

(provide 'srecode-getset)

;;; srecode-getset.el ends here

