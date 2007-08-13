;;; srecode-semantic.el --- Semantic specific extensions to SRecode.

;; Copyright (C) 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-semantic.el,v 1.3 2007/03/18 16:47:42 zappo Exp $

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
;; Semantic specific extensions to the Semantic Recoder.
;;
;; I realize it is the "Semantic Recoder", but most of srecode
;; is a template library and set of user interfaces unrelated to
;; semantic in the specific.
;;
;; This file defines the following:
;;   - :tag argument handling.
;;   - <more goes here>

;;; Code:
(require 'srecode-insert)
(require 'srecode-dictionary)
(require 'semantic-tag)
(require 'senator)

;;; Managing the `current' tag
;;
(defclass srecode-semantic-tag (srecode-dictionary-compound-value)
  ((prime :initarg :prime
	  :type semantic-tag
	  :documentation
	  "This is the primary insertion tag.")
   )
  "Wrap up a collection of semantic tag information.
This class will be used to derive dictionary values.")

(defvar srecode-semantic-selected-tag nil
  "The tag selected by a :tag template argument.
If this is nil, then `senator-tag-ring' is used.")

(defun srecode-semantic-tag-from-kill-ring ()
  "Create an `srecode-semantic-tag' from the senator kill ring."
  (if (ring-empty-p senator-tag-ring)
      (error "You must use `senator-copy-tag' to provide a tag to this template"))
  (let ((tag (ring-ref senator-tag-ring 0))
	)
    (srecode-semantic-tag (semantic-tag-name tag)
			  :prime tag)
    ))

;;; :tag ARGUMENT HANDLING
;;
;; When a :tag argument is required, identify the current :tag,
;; and apply it's parts into the dictionary.
;;;###autoload
(defun srecode-semantic-handle-:tag (dict)
  "Add macroes into the dictionary DICT based on the current :tag."
  ;; We have a tag, start adding "stuff" into the dictionary.
  (srecode-semantic-apply-tag-to-dict
   (or
    (when srecode-semantic-selected-tag
      (srecode-semantic-tag (semantic-tag-name srecode-semantic-selected-tag)
			    :prime srecode-semantic-selected-tag))
    (srecode-semantic-tag-from-kill-ring))
   dict))

;;; :tagtype ARGUMENT HANDLING
;;
;; When a :tagtype argument is required, identify the current tag, of
;; cf class 'type.  Apply those parameters to the dictionary.

;;;###autoload
(defun srecode-semantic-handle-:tagtype (dict)
  "Add macroes into the dictionary DICT based on a tag of class type at point.
Assumes the cursor is in a tag of class type.  If not, throw an error."
  (let ((typetag (or srecode-semantic-selected-tag
		     (semantic-current-tag-of-class 'type))))
    (when (not typetag)
      (error "Cursor is not in a TAG of class 'type"))
    (srecode-semantic-apply-tag-to-dict
     typetag
     dict)))

(defun srecode-semantic-apply-tag-to-dict (tagobj dict)
  "Insert features of TAGOBJ into dictionary DICT."
  ;; Store the sst into the dictionary.
  (srecode-dictionary-set-value dict "TAG" tagobj)

  ;; Pull out the tag for the individual pieces.
  (let ((tag (oref tagobj :prime)))

    (srecode-dictionary-set-value dict "NAME" (semantic-tag-name tag))
    (srecode-dictionary-set-value dict "TYPE" (semantic-format-tag-type tag nil))
  
    (cond ((eq (semantic-tag-class tag) 'function)
	   (let ((args (semantic-tag-function-arguments tag)))
	     (while args
	       (let ((larg (car args))
		     (subdict (srecode-dictionary-add-section-dictionary
			       dict "ARGS")))
		 ;; Clean up elements in the arg list.
		 (if (stringp larg)
		     (setq larg (semantic-tag-new-variable
				 larg nil nil)))
		 ;; Apply the sub-argument to the subdictionary.
		 (srecode-semantic-apply-tag-to-dict
		  larg subdict)
		 ;; Is this the first argument?
		 (if (eq args (semantic-tag-function-arguments tag))
		     (srecode-dictionary-show-section subdict "FIRST")
		   (srecode-dictionary-show-section subdict "NOTFIRST"))
		 ;; Is this argument last?
		 (if (not (cdr args))
		     (srecode-dictionary-show-section subdict "LAST")
		   (srecode-dictionary-show-section subdict "NOTLAST"))
		 )
	       ;; Next!
	       (setq args (cdr args)))))
	  ((eq (semantic-tag-class tag) 'variable)
	   )
	  )
    ))


(provide 'srecode-semantic)

;;; srecode-semantic.el ends here

