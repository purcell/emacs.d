;;; srecode-ctxt.el --- Derive a context from the source buffer.

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
;; Manage context calculations for Semantic Recoder.
;;
;; SRecode templates are always bound to a context.  By calculating
;; the current context, we can narrow down the selection of possible 
;; templates to something reasonable.
;;
;; Alternately, code here will find a context for templates that
;; require different pieces of code placed in multiple areas.

(require 'semantic)

;;; Code:
;;;###autoload
(define-overload srecode-calculate-context ()
  "Calculate the context at the current point.
The returned context is a list, with the top-most context first.
Each returned context is a string that that would show up in a `context'
statement in an `.srt' file.

Some useful context values used by the provided srecode templates are:
  \"file\" - Templates that for a file (such as an empty file.)
     \"empty\" - The file is empty
  \"declaration\" - Top-level declarations in a file.
     \"include\" - In or near include statements
     \"package\" - In or near provide statements
     \"function\" - In or near function statements
         \"NAME\" - Near functions within NAME namespace or class
     \"variable\" - In or near variable statements.
  \"classdecl\" - Declarations within a class/struct/etc.
     \"public\", \"protected\", \"private\" -
                  In or near a section of public/pritected/private entries.
     \"method\" - In or near methods
        \"virtual\" - Nearby items are virtual
           \"pure\" - and those virtual items are pure virtual
     \"field\" - In or near fields
  \"code\" - In a block of code.
    ... More later."
  )

(defun srecode-calculate-nearby-things ()
  ;; NOTE: May need to add bounes to this FCN
  "Calculate the CONTEXT type items nearby the current point.
Assume that what we want to insert next is based on what is just
before point.  If there is nothing, then assume it is whatever is
after point."
  (let ((near (semantic-find-tag-by-overlay-prev)))
    (if (not near)
	(setq near (semantic-find-tag-by-overlay-next)))
    (if near
	(symbol-name (semantic-tag-class near)))
    ))

(defun srecode-calculate-context-default ()
  "Generic method for calculating a context for srecode.
Use Semantic language-agnostic functions to attempt a general solution
for most languages."
  (if (= (point-min) (point-max))
      (list "file" "empty")

    (let ((ct (semantic-find-tag-by-overlay))
	  )
      (cond ((or (not ct)
		 ;; Ok, below is a bit C specific.
		 (and (eq (semantic-tag-class (car ct)) 'type)
		      (string= (semantic-tag-type (car ct)) "namespace")))
	     (list "declaration"
		   (srecode-calculate-nearby-things))
	     )
	    ((eq (semantic-tag-class (car ct)) 'function)
	     (list "code")
	     )
	    ((eq (semantic-tag-class (car ct)) 'type) ; We know not namespace
	     (list "classdecl"
		   (srecode-calculate-nearby-things))
	     )
	    ((and (car (cdr ct))
		  (eq (semantic-tag-class (car (cdr ct))) 'type))
	     (list "classdecl"
		   (symbol-name (semantic-tag-class (car ct))))
	     )
	    )
      )))

(provide 'srecode-ctxt)

;;; srecode-ctxt.el ends here

