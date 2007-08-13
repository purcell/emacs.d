;;; srecode-table.el --- Tables of Semantic Recoders

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
;; Semantic Recoder tables manage lists of templates and the major
;; modes they are associated with.
;;

(require 'eieio)
(require 'eieio-base)

;;; Code:

;;; TEMPLATE TABLE
;;
(defclass srecode-template-table ()
  ((file :initarg :file
	 :type string
	 :documentation
	 "The name of the file this table was built from.")
   (priority :initarg :priority
	     :type number
	     :documentation
	     "For file of this Major Mode, what is the priority of this file.
When there are multiple template files with similar names, templates with
the highest priority are scanned last, allowing them to override values in
previous template files.")
   (templates :initarg :templates
	      :type list
	      :documentation
	      "The list of templates compiled into this table.")
   (namehash :initarg :namehash
	     :documentation
	     "Hash table containing the names of all the templates.")
   (contexthash :initarg :contexthash
		:documentation
		"")
   (variables :initarg :variables
	      :documentation
	      "AList of variables.
These variables are used to initialize dictionaries.")
   (major-mode :initarg :major-mode
	       :documentation
	       "The major mode this table of templates is associated with.")
   (application :initarg :application
		:type symbol
		:documentation
		"Tracks the name of the application these templates belong to.
If this is nil, then this template table belongs to a set of generic
templates that can be used with no additional dictionary values.
When it is non-nil, it is assumed the template macros need specialized
Emacs Lisp code to fill in the dictoinary.")
   )
  "Semantic recoder template table.
A Table contains all templates from a single .srt file.
Tracks various lookup hash tables.")

;;; MODE TABLE
;;
(defvar srecode-mode-table-list nil
  "List of all the SRecode mode table classes that have been built.")

(defclass srecode-mode-table (eieio-instance-tracker)
   ((tracking-symbol :initform 'srecode-mode-table-list)
    (major-mode :initarg :major-mode
		:documentation
		"Table of template tables for this major-mode.")
    (tables :initarg :tables
	    :documentation
	    "All the tables that have been defined for this major mode.")
    )
   "Track template tables for a particular major mode.
Tracks all the template-tables for a specific major mode.")

;;;###autoload
(defun srecode-get-mode-table (mode)
  "Get the SRecoder mode table for the major mode MODE.
Optional argument SOFT indicates to not make a new one if a table
was not found."
  (eieio-instance-tracker-find
   mode 'major-mode 'srecode-mode-table-list))

(defun srecode-make-mode-table (mode)
  "Get the SRecoder mode table for the major mode MODE."
  (let ((old (eieio-instance-tracker-find
	      mode 'major-mode 'srecode-mode-table-list)))
    (if old
	old
      (let* ((ms (if (stringp mode) mode (symbol-name mode)))
	     (new (srecode-mode-table ms
				      :major-mode mode
				      :tables nil)))
	;; Save this new mode table in that mode's variable.
	(eval `(setq-mode-local ,mode srecode-table ,new))
	
	new))))

(defmethod srecode-mode-table-find ((mt srecode-mode-table) file)
  "Look in the mode table MT for a template table from FILE.
Return nil if there was none."
  (object-assoc file 'file (oref mt tables)))

;;;###autoload
(defun srecode-mode-table-new (mode file &rest init)
  "Create a new template table for MODE in FILE.
INIT are the initialization parametrs for the new template table."
  (let* ((mt (srecode-make-mode-table mode))
	 (old (srecode-mode-table-find mt file))
	 (new (apply 'srecode-template-table
		     (file-name-nondirectory file)
		     :file file
		     :major-mode mode
		     init
		     )))
    ;; Whack the old table.
    (when old (object-remove-from-list mt 'tables old))
    ;; Add the new table
    (object-add-to-list mt 'tables new)
    ;; Sort the list in reverse order.  When other routines
    ;; go front-to-back, the highest priority items are put
    ;; into the search table first, allowing lower priority items
    ;; to be the items found in the search table.
    (object-sort-list mt 'tables (lambda (a b)
				   (> (oref a :priority)
				      (oref b :priority))))
    ;; Return it.
    new))

(defun object-sort-list (object slot predicate)
  "Sort the items in OBJECT's SLOT.
Use PREDICATE is the same as for the `sort' function."
  (when (slot-boundp object slot)
    (when (listp (eieio-oref object slot))
      (eieio-oset object slot (sort (eieio-oref object slot) predicate)))))

;;; DEBUG
;;
;; Dump out information about the current srecoder compiled templates.
;;
;;;###autoload
(defun srecode-dump-templates (mode)
  "Dump a list of the current templates for MODE."
  (interactive "sMode: ")
  (srecode-load-tables-for-mode (intern-soft mode))
  (let ((tmp (srecode-get-mode-table
	      (cond ((or (not mode) (string= mode ""))
		     major-mode)
		    (t (intern-soft mode)))))
	)
    (if (not tmp)
	(error "No table found for mode %s" mode))
    (with-output-to-temp-buffer "*SRECODE DUMP*"
      (srecode-dump tmp))
    ))

(defmethod srecode-dump ((tab srecode-mode-table))
  "Dump the contents of the SRecode mode table TAB."
  (princ "MODE TABLE FOR ")
  (princ (oref tab :major-mode))
  (princ "\n--------------------------------------------\n\nNumber of tables: ")
  (let ((subtab (oref tab :tables)))
    (princ (length subtab))
    (princ "\n\n")
    (while subtab
      (srecode-dump (car subtab))
      (setq subtab (cdr subtab)))
    ))

(defmethod srecode-dump ((tab srecode-template-table))
  "Dump the contents of the SRecode template table TAB."
  (princ "Template Table for ")
  (princ (object-name-string tab))
  (princ "\nPriority: ")
  (prin1 (oref tab :priority))
  (princ "\n\nVariables:\n")
  (let ((vars (oref tab variables)))
    (while vars
      (princ (car (car vars)))
      (princ "\t")
      (if (< (length (car (car vars))) 9)
	  (princ "\t"))
      (prin1 (cdr (car vars)))
      (princ "\n")
      (setq vars (cdr vars))))
  (princ "\n\nTemplates:\n")
  (let ((temp (oref tab templates)))
    (while temp
      (srecode-dump (car temp))
      (setq temp (cdr temp))))
  )  

(provide 'srecode-table)

;;; srecode-table.el ends here

