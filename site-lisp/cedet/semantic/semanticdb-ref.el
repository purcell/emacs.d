;;; semanticdb-ref.el --- Handle cross-db file references

;; Copyright (C) 2007, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semanticdb-ref.el,v 1.11 2009/01/20 02:31:00 zappo Exp $

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
;; Handle cross-database file references.
;;
;; Any given database may be referred to by some other database.  For
;; example, if a .cpp file has a #include in a header, then that
;; header file should have a reference to the .cpp file that included
;; it.
;;
;; This is critical for purposes where a file (such as a .cpp file)
;; needs to have its caches flushed because of changes in the
;; header.  Changing a header may cause a referring file to be
;; reparsed due to account for changes in defined macros, or perhaps
;; a change to files the header includes.


;;; Code:
;;;###autoload
(defmethod semanticdb-add-reference ((dbt semanticdb-abstract-table)
				     include-tag)
  "Add a reference for the database table DBT based on INCLUDE-TAG.
DBT is the database table that owns the INCLUDE-TAG.  The reference
will be added to the database that INCLUDE-TAG refers to."
  ;; NOTE: I should add a check to make sure include-tag is in DB.
  ;;       but I'm too lazy.
  (let* ((semanticdb-find-default-throttle
	       (if (featurep 'semanticdb-find)
		   (remq 'unloaded semanticdb-find-default-throttle)
		 nil))
	 (refdbt (semanticdb-find-table-for-include include-tag dbt))
	 ;;(fullfile (semanticdb-full-filename dbt))
	 )
    (when refdbt
      ;; Add our filename (full path)
      ;; (object-add-to-list refdbt 'file-refs fullfile)

      ;; Add our database.
      (object-add-to-list refdbt 'db-refs dbt)
      t)))

(defmethod semanticdb-check-references ((dbt semanticdb-abstract-table))
  "Check and cleanup references in the database DBT.
Abstract tables would be difficult to reference."
  ;; Not sure how an abstract table can have references.
  nil)

(defmethod semanticdb-includes-in-table ((dbt semanticdb-abstract-table))
  "Return a list of direct includes in table DBT."
  (semantic-find-tags-by-class 'include (semanticdb-get-tags dbt)))


(defmethod semanticdb-check-references ((dbt semanticdb-table))
  "Check and cleanup references in the database DBT.
Any reference to a file that cannot be found, or whos file no longer
refers to DBT will be removed."
  (let ((refs (oref dbt db-refs))
	(myexpr (concat "\\<" (oref dbt file)))
	)
    (while refs
      (let* ((ok t)
	     (db (car refs))
	     (f (when (semanticdb-table-child-p db)
		  (semanticdb-full-filename db)))
	     )

	;; The file was deleted
	(when (and f (not (file-exists-p f)))
	  (setq ok nil))

	;; The reference no longer includes the textual reference?
	(let* ((refs (semanticdb-includes-in-table db))
	       (inc (semantic-find-tags-by-name-regexp
		     myexpr refs)))
	  (when (not inc)
	    (setq ok nil)))

	;; Remove not-ok databases from the list.
	(when (not ok)
	  (object-remove-from-list dbt 'db-refs db)
	  ))
      (setq refs (cdr refs)))))

(defmethod semanticdb-refresh-references ((dbt semanticdb-abstract-table))
  "Refresh references to DBT in other files."
  ;; alternate tables can't be edited, so can't be changed.
  nil
  )

(defmethod semanticdb-refresh-references ((dbt semanticdb-table))
  "Refresh references to DBT in other files."
  (let ((refs (semanticdb-includes-in-table dbt))
	)
    (while refs
      (if (semanticdb-add-reference dbt (car refs))
	  nil
	;; If we succeeded, then do... nothing?
	nil
	)
      (setq refs (cdr refs)))
    ))

(defmethod semanticdb-notify-references ((dbt semanticdb-table)
					 method)
  "Notify all references of the table DBT using method.
METHOD takes two arguments.
  (METHOD TABLE-TO-NOTIFY DBT)
TABLE-TO-NOTIFY is a semanticdb-table which is being notified.
DBT, the second argument is DBT."
  (mapc (lambda (R) (funcall method R dbt))
	  (oref dbt db-refs)))

;;; DEBUG
;;
(defclass semanticdb-ref-adebug ()
  ((i-depend-on :initarg :i-depend-on)
   (local-table :initarg :local-table)
   (i-include :initarg :i-include))
  "Simple class to allow ADEBUG to show a nice list.")

(defun semanticdb-ref-test (refresh)
  "Dump out the list of references for the current buffer.
If REFRESH is non-nil, cause the current table to have it's references
refreshed before dumping the result."
  (interactive "p")
  ;; If we need to refresh... then do so.
  (when refresh
    (semanticdb-refresh-references semanticdb-current-table))
  ;; Do the debug system
  (let* ((tab semanticdb-current-table)
	 (myrefs (oref tab db-refs))
	 (myinc (semanticdb-includes-in-table tab))
	 (adbc (semanticdb-ref-adebug "DEBUG"
				      :i-depend-on myrefs
				      :local-table tab
				      :i-include myinc)))
    (data-debug-new-buffer "*References ADEBUG*")
    (data-debug-insert-object-slots adbc "!"))
  )

(provide 'semanticdb-ref)
;;; semanticdb-ref.el ends here
