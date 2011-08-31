;;; srecode-expandproto.el --- Expanding prototypes.

;; Copyright (C) 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-expandproto.el,v 1.1 2007/03/18 16:47:16 zappo Exp $

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
;; Methods for expanding a prototype into an implementation.

(require 'semantic)
(require 'semantic-analyze)
(require 'srecode-insert)
(require 'srecode-dictionary)

;;; Code:
(defcustom srecode-expandproto-template-file-alist
  '( ( c++-mode . "srecode-expandproto-cpp.srt" )
     )
  ;; @todo - Make this variable auto-generated from the Makefile.
  "Associate template files for expanding prototypes to a major mode."
  :group 'srecode
  :type '(repeat (cons (sexp :tag "Mode")
		       (sexp :tag "Filename"))
		 ))

;;;###autoload
(defun srecode-insert-prototype-expansion ()
  "Insert get/set methods for the current class."
  (interactive)

  (srecode-load-tables-for-mode major-mode)
  (srecode-load-tables-for-mode major-mode
				srecode-expandproto-template-file-alist)

  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))

  (let ((proto
	 ;; Step 1: Find the prototype, or prototype list to expand.
	 (srecode-find-prototype-for-expansion)))

    (if (not proto)
	(error "Could not find prototype to expand"))

    ;; Step 2: Insert implementations of the prototypes.


    ))

(defun srecode-find-prototype-for-expansion ()
  "Find a prototype to use for expanding into an implementation."
  ;; We may find a prototype tag in one of several places.
  ;; Search in order of logical priority.
  (let ((proto nil)
	)

    ;; 1) A class full of prototypes under point.
    (let ((tag (semantic-current-tag)))
      (when tag
	(when (not (semantic-tag-of-class-p tag 'type))
	  (setq tag (semantic-current-tag-parent))))
      (when (and tag (semantic-tag-of-class-p tag 'type))
	;; If the current class has prototype members, then
	;; we will do the whole class!
	(if (semantic-brute-find-tag-by-attribute-value
	     :prototype t
	     (semantic-tag-type-members tag))
	    (setq proto tag)))
      )

    ;; 2) A prototype under point.
    (when (not proto)
      (let ((tag (semantic-current-tag)))
	(when (and tag
		   (and
		    (semantic-tag-of-class-p tag 'function)
		    (semantic-tag-get-attribute tag :prototype)))
	  (setq proto tag))))

    ;; 3) A tag in the kill ring that is a prototype
    (when (not proto)
      (if (ring-empty-p senator-tag-ring)
	  nil  ;; Not for us.
	(let ((tag (ring-ref senator-tag-ring 0))
	      )
	  (when
	      (and tag
		   (or
		    (and
		     (semantic-tag-of-class-p tag 'function)
		     (semantic-tag-get-attribute tag :prototype))
		    (and
		     (semantic-tag-of-class-p tag 'type)
		     (semantic-brute-find-tag-by-attribute-value
		      :prototype t
		      (semantic-tag-type-members tag))))
		   )
	    (setq proto tag))
	  )))

    proto))

(provide 'srecode-expandproto)

;;; srecode-expandproto.el ends here
