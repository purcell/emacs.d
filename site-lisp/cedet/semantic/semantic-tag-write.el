;;; semantic-tag-write.el --- Write tags to a text stream

;; Copyright (C) 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-tag-write.el,v 1.4 2009/01/10 00:09:51 zappo Exp $

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
;; Routine for writing out a list of tags to a text stream.
;;
;; These routines will be used by semanticdb to output a tag list into
;; a text stream to be saved to a file.  Ideally, you could use tag streams
;; to share tags between processes as well.
;;
;; As a bonus, these routines will also validate the tag structure, and make sure
;; that they conform to good semantic tag hygene.
;;

(require 'semantic-tag)

;;; Code:
(defun semantic-tag-write-one-tag (tag &optional indent)
  "Write a single tag TAG to standard out.
INDENT is the amount of indentation to use for this tag."
  (when (not (semantic-tag-p tag))
    (signal 'wrong-type-argument (list tag 'semantic-tag-p)))
  (when (not indent) (setq indent 0))
  ;(princ (make-string indent ? ))
  (princ "(\"")
  ;; Base parts
  (let ((name (semantic-tag-name tag))
	(class (semantic-tag-class tag)))
    (princ name)
    (princ "\" ")
    (princ (symbol-name class))
    )
  (let ((attr (semantic-tag-attributes tag))
	)
    ;; Attributes
    (cond ((not attr)
	   (princ " nil"))
	  
	  ((= (length attr) 2) ;; One item
	   (princ " (")
	   (semantic-tag-write-one-attribute attr indent)
	   (princ ")")
	   )
	  (t
	   ;; More than one tag.
	   (princ "\n")
	   (princ (make-string (+ indent 3) ? ))
	   (princ "(")
	   (while attr
	     (semantic-tag-write-one-attribute attr (+ indent 4))
	     (setq attr (cdr (cdr attr)))
	     (when attr
	       (princ "\n")
	       (princ (make-string (+ indent 4) ? )))
	     )
	   (princ ")\n")
	   (princ (make-string (+ indent 3) ? ))
	   ))
    ;; Properties - for now, always nil.
    (let ((rs (semantic--tag-get-property tag 'reparse-symbol)))
      (if (not rs)
	  (princ " nil")
	;; Else, put in the property list.
	(princ " (reparse-symbol ")
	(princ (symbol-name rs))
	(princ ")"))
      ))
  ;; Overlay
  (if (semantic-tag-with-position-p tag)
      (let ((bounds (semantic-tag-bounds tag)))
	(princ " ")
	(prin1 (apply 'vector bounds))
	)
    (princ " nil"))
  ;; End it.
  (princ ")")
  )

;;;###autoload
(defun semantic-tag-write-tag-list (tlist &optional indent dontaddnewline)
  "Write the tag list TLIST to the current stream.
INDENT indicates the current indentation level.
If optional DONTADDNEWLINE is non-nil, then don't add a newline."
  (if (not indent)
      (setq indent 0)
    (unless dontaddnewline
      ;; Assume cursor at end of current line.  Add a CR, and make the list.
      (princ "\n")
      (princ (make-string indent ? ))))
  (princ "( ")
  (while tlist
    (if (semantic-tag-p (car tlist))
	(semantic-tag-write-one-tag (car tlist) (+ indent 2))
      ;; If we don't have a tag in the tag list, use the below hack, and hope
      ;; it doesn't contain anything bad.  If we find something bad, go back here
      ;; and start extending what's expected here.
      (princ (format "%S" (car tlist))))
    (setq tlist (cdr tlist))
    (when tlist
      (princ "\n")
      (princ (make-string (+ indent 2) ? )))
    )
  (princ ")")
  (princ (make-string indent ? ))
  )
  

;; Writing out random stuff.
(defun semantic-tag-write-one-attribute (attrs indent)
  "Write out one attribute from the head of the list of attributes ATTRS.
INDENT is the current amount of indentation."
  (when (not attrs) (signal 'wrong-type-argument (list 'listp attrs)))
  (when (not (symbolp (car attrs))) (error "Bad Attribute List in tag"))

  (princ (symbol-name (car attrs)))
  (princ " ")
  (semantic-tag-write-one-value (car (cdr attrs)) indent)
  )

(defun semantic-tag-write-one-value (value indent)
  "Write out a VALUE for something in a tag.
INDENT is the current tag indentation.
Items that are long lists of tags may need their own line."
  (cond
   ;; Another tag.
   ((semantic-tag-p value)
    (semantic-tag-write-one-tag value (+ indent 2)))
   ;; A list of more tags
   ((and (listp value) (semantic-tag-p (car value)))
    (semantic-tag-write-tag-list value (+ indent 2))
    )
   ;; Some arbitrary data.
   (t
    (let ((str (format "%S" value)))
      ;; Protect against odd data types in tags.
      (if (= (aref str 0) ?#)
	  (progn
	    (princ "nil")
	    (message "Warning: Value %s not writable in tag." str))
	(princ str)))))
  )
;;; EIEIO USAGE
;;;###autoload
(defun semantic-tag-write-list-slot-value (value)
  "Write out the VALUE of a slot for EIEIO.
The VALUE is a list of tags."
  (if (not value)
      (princ "nil")
    (princ "\n        '")
    (semantic-tag-write-tag-list value 10 t)
    ))

;;; TESTING.

(defun semantic-tag-write-test ()
  "Test the semantic tag writer against the tag under point."
  (interactive)
  (with-output-to-temp-buffer "*Tag Write Test*"
    (semantic-tag-write-one-tag (semantic-current-tag))))

(defun semantic-tag-write-list-test ()
  "Test the semantic tag writer against the tag under point."
  (interactive)
  (with-output-to-temp-buffer "*Tag Write Test*"
    (semantic-tag-write-tag-list (semantic-fetch-tags))))


(provide 'semantic-tag-write)
;;; semantic-tag-write.el ends here
