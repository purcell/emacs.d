;;; dbif-edit.el --- Enable editing of database fields
;;
;; Copyright (C) 1996, 1998 Eric M. Ludlam
;;
;; Author: <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; RCS: $Id: dbif-edit.el,v 1.1 1998/10/27 18:12:18 zappo Exp $
;; Keywords: OO database
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
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;
;; Updates can be found at:
;;    ftp://ftp.ultranet.com/pub/zappo

;;; Commentary:
;; A Program that enables the editing of any database fields that understands
;; the SQL language.

(require 'dbif)

;;; Code:
(defvar dbif-edit-database-list nil
  "Set this after the first collection of databases is loaded.")

(defvar dbif-edit-class nil
  "The type of database used by this users.
This is queried the first time `dbif-edit' is used, and then it is
stored here for future use.")

(defvar dbif-edit-db-history nil
  "History for the DB selector prompt.")

(defun dbif-edit (&rest args)
  "Start editing some table.
Optional argument ARGS Arguments to pass to the open database object."
  (interactive)
  ;; Make sure that we have a database type loaded in.
  (if (not dbif-edit-class)
      (let ((class (read-string "Database class: " "psql" nil "psql")))
	(if (locate-library class)
	    (setq dbif-edit-class class)
	  (error "Unknown class object: %s" class))))
  ;; Now get a list of tall the databases available
  (require (read dbif-edit-class))
  (if (not dbif-edit-database-list)
      (setq dbif-edit-database-list
	    (funcall (read (concat dbif-edit-class "-database-list")))))
  ;; Find out which database the user wants to in
  (let ((db (completing-read "Database: "
			     (mapcar 'list dbif-edit-database-list)
			     nil t nil 'dbif-edit-db-history))
	(dbobj nil)
	(query nil))
    (setq dbobj (funcall (read (concat dbif-edit-class "-set-db")) db))
    (switch-to-buffer
     (get-buffer-create (format "*%s %s*" dbif-edit-class db)))
    (dbif-edit-mode dbobj)
    ;; Lets load up some data.
    (dbif-edit-new-query)
    )
  )

(defvar dbif-edit-connection nil
  "Buffer local connection to a database.")

(defvar dbif-edit-data nil
  "The current data list.")

(defvar dbif-edit-index 0
  "The current data list.")

(defvar dbif-edit-field 0
  "The current data list.")

(defvar dbif-edit-keymap nil
  "Keymap used when editing databases.")

(if dbif-edit-keymap
    nil
  (setq dbif-edit-keymap (make-keymap "DBIF"))
  (define-key dbif-edit-keymap "n" 'dbif-edit-next)
  (define-key dbif-edit-keymap "p" 'dbif-edit-prev)
  (define-key dbif-edit-keymap "\C-i" 'dbif-edit-next-field)
  (define-key dbif-edit-keymap "\M-\C-i" 'dbif-edit-prev-field)
  (define-key dbif-edit-keymap "Q" 'dbif-edit-new-query)
  (define-key dbif-edit-keymap "\C-l" 'dbif-edit-refresh)
  )

(defun dbif-edit-mode (connect-object)
  "Major mode for editing information from a Database.
Argument CONNECT-OBJECT is the object that represents the connection
to the database."
  (kill-all-local-variables)
  (setq major-mode 'dbif-edit-mode
	mode-name "DBIF-EDIT")
  (use-local-map dbif-edit-keymap)
  (make-local-variable 'dbif-edit-connection)
  (setq dbif-edit-connection connect-object)
  (make-local-variable 'dbif-edit-data)
  (setq dbif-edit-data nil)
  (make-local-variable 'dbif-edit-index)
  (setq dbif-edit-index 0)
  (make-local-variable 'dbif-edit-field)
  (setq dbif-edit-field 0)
  )

(defun dbif-edit-next (arg)
  "Move to the next record in the database.
Argument ARG specifies how many fields to move."
  (interactive "p")
  (if (not arg) (setq arg 1))
  (let ((max (length (oref dbif-edit-data :values))))
    (if (< (+ dbif-edit-index arg) 0)
	(setq dbif-edit-index (+ max (% (+ dbif-edit-index arg) max)))
      (setq dbif-edit-index (% (+ dbif-edit-index arg) max))))
  (dbif-edit-refresh)
  (dbif-edit-reposition))

(defun dbif-edit-prev (arg)
  "Move to the previous field in the database.
Argument ARG specifies the number of fields to move."
  (interactive "p")
  (dbif-edit-next (if arg (- arg) -1)))

(defun dbif-edit-next-field (arg)
  "Move to the next field in the database.
Argument ARG specifies the number of fields to move."
  (interactive "p")
  (if (not arg) (setq arg 1))
  (let ((max (length (oref dbif-edit-data :headers))))
    (if (< (+ dbif-edit-field arg) 0)
	(setq dbif-edit-field (+ max (% (+ dbif-edit-field arg) max)))
      (setq dbif-edit-field (% (+ dbif-edit-field arg) max))))
  (dbif-edit-reposition))

(defun dbif-edit-prev-field (arg)
  "Move to the previous field in the database.
Argument ARG specifies the number of fields to move."
  (interactive "p")
  (dbif-edit-next-field (if arg (- arg) -1)))

(defvar dbif-edit-query-history nil
  "History for setting queries.")

(defun dbif-edit-new-query ()
  "Enter a new query with data to edit."
  (interactive)
  (let ((q (read-string "Query: " nil 'dbif-edit-query-history)))
    (setq dbif-edit-data (dbif-exec dbif-edit-connection q))
    (setq dbif-edit-index 0)
    (setq dbif-edit-field 0)
    (dbif-edit-refresh)))

(defun dbif-edit-refresh ()
  "Refresh the current display."
  (interactive)
  (erase-buffer)
  (let ((data (nth dbif-edit-index (oref dbif-edit-data :values)))
	(fields (oref dbif-edit-data :headers))
	(len (1+ (dbif-longest dbif-edit-data))))
    (while fields
      (insert (car fields) ":"
	      (make-string (- len (length (car fields))) ? )
	      (car data) "\n")
      (setq fields (cdr fields)
	    data (cdr data))))
  (dbif-edit-reposition))

(defun dbif-longest (tuple)
  "Return the longest field name in TUPLE."
  (let ((names (oref tuple :headers))
	(len 0))
    (while names
      (if (< len (length (car names)))
	  (setq len (length (car names))))
      (setq names (cdr names)))
    len))

(defun dbif-edit-reposition ()
  "Position the cursor based on numbers."
  (goto-char (point-min))
  (re-search-forward "[A-Za-z0-9_]+: +" nil nil (1+ dbif-edit-field)))

(provide 'dbif-edit)

;;; dbif-edit.el ends here


