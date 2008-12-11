;;; eieio-xml.el -- import and export EIEIO objects as XML.

;;;
;; Copyright (C) 2005, 2006, 2008 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; CVS: $Id: eieio-xml.el,v 1.3 2008/09/29 00:21:05 zappo Exp $
;; Keywords: oop, lisp, tools
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
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.org

;;; Commentary:
;;  
;; The EIEIO built-in save file (via object-write creates Lisp code
;; that can be read in.)  XML import/export allows a method to save
;; EIEIO objects in an XML form that would make it easier to have
;; EIEIO objects to interact with other tools.

;;; Code:

;;; WRITER
;;
(defun eieio-xml-override-prin1 (thing)
  "Perform a prin1 on THING taking advantage of object knowledge."
  (cond ((eieio-object-p thing)
	 (princ "\n")
	 (object-write-xml thing)
	 (princ "\n"))
	((listp thing)
	 (eieio-list-prin1 thing))
	(t (eieio-override-prin1 thing))))

(defun eieio-xml-list-prin1 (list)
  "Display LIST where list may contain objects."
  (if (not (eieio-object-p (car list)))
      (eieio-list-prin1 list)
    (princ "\n")
    (while list
      (eieio-xml-override-prin1 thing)
      (setq list (cdr list)))
    ))

;;;###autoload
(defmethod object-write-xml ((this eieio-default-superclass) &optional comment)
  "Write object THIS out to the current stream as XML.
  If optional COMMENT is non-nil, include comments when outputting
this object.
@todo - support arbitrary schema output"
  (when comment
    (princ "<!-- Object ")
    (princ (object-name-string this))
    (princ " -->\n<!-- ")
    (princ comment)
    (princ " -->\n"))
  (let* ((cl (object-class this))
	 (cv (class-v cl)))
    ;; Now output XML, with values of readable lisp to recreate this object
    ;; It should look like this:
    ;; <object>
    ;;    <name>name</name>
    ;;    <class>someclass</class>
    ;;    <slot>
    ;;      <name>slot1</name>
    ;;      <value>attrvalue</value>
    ;;    </slot>
    ;; </object>
    ;; Each slot's slot is writen using its :writer.
    (princ (make-string (* eieio-print-depth 2) ? ))
    (princ "<object>\n")
    (let ((eieio-print-depth (+ eieio-print-depth 1)))
      (princ (make-string (* eieio-print-depth 2) ? ))
      (princ "<name>")
      (princ (object-name-string this))
      (princ "</name>\n")
      (princ (make-string (* eieio-print-depth 2) ? ))
      (princ "<class>")
      (princ (symbol-name (class-constructor (object-class this))))
      (princ "</class>\n")
      ;; Loop over all the public slots
      (let ((publa (aref cv class-public-a))
	    (publd (aref cv class-public-d)))
	(while publa
	  (when (slot-boundp this (car publa))
	    (let ((i (class-slot-initarg cl (car publa)))
		  (v (eieio-oref this (car publa))))
	      (unless (or (not i) (equal v (car publd)))
		(princ (make-string (* eieio-print-depth 2) ? ))
		(princ "<slot>\n")
		(princ (make-string (+ (* eieio-print-depth 2) 2) ? ))
		(princ "<name>")
		(princ (symbol-name i))
		(princ "</name>\n")
		(princ (make-string (+ (* eieio-print-depth 2) 2) ? ))
		(princ "<value>")
		(let ((eieio-print-depth (+ eieio-print-depth 2))
		      (o (eieio-oref this (car publa))))
		  (eieio-xml-override-prin1 o))
		(princ "</value>\n")
		(princ (make-string (* eieio-print-depth 2) ? ))
		(princ "</slot>\n"))))
	  (setq publa (cdr publa) publd (cdr publd)))))
    (princ (make-string (* eieio-print-depth 2) ? ))
    (princ "</object>\n"))
  )

;;; READER
;;
(eval-when-compile
  (require 'xml))

;;;#autoload
(defun eieio-read-xml (file)
  "Read in the file FILE.  Return a list of EIEIO objects.
The XML file would have been created previously from `object-write-xml'
or compatible program.
@todo - Write this."
  (require 'xml)
  
  )

;;; TEST!!
(defclass Axml ()
  ((slot1 :initarg :slot1
	  :initform 10)
   (slot2 :initarg :slot2
	  :initform "moose")
   (slot3 :initarg :slot3
	  :initform 'emu)
   (slot4 :initarg :slot4
	  :initform nil)
   )
  "Test class")

(defun eieio-text-write-xml ()
  "Test the write XML functions."
  (interactive)
  (let ((o1 (Axml "test" :slot2 "cow" :slot3 'moose))
	(o2 (Axml "test2" :slot2 "pickle")))
    (oset o1 slot4 o2)
    (with-output-to-temp-buffer "*Test*"
      (object-write-xml o1 "Testing."))))


(provide 'eieio-xml)

;;; eieio-xml.el ends here
