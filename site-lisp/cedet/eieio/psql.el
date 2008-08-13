;;; psql --- PostgresSQL 'psql' tool interface
;;
;; Copyright (C) 1996, 1998, 1999, 2000, 2001 Eric M. Ludlam
;;
;; Author: <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; RCS: $Id: psql.el,v 1.6 2001/01/11 18:50:35 zappo Exp $
;; Keywords: OO postgres95 database
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
;;   This tool uses dbif to inherit a connection type, and uses this
;; to interface with a postgres95 database using the psql command
;; line tool which is run inside a comint buffer.  The output from
;; the program is parsed, and the apropriate dbif-tuple type is then
;; generated.

;;;
;; PSQL backend
;;

;;; History:
;; 

(require 'dbif)
(require 'pg)				;Eric Marsden's postgres library

;;; Code:

(defclass psql-connection (dbif-connection)
  ((pgconnection :initarg :pgconnection
		 :initform nil)
   (host :initarg :host
	 :initarg nil)
   (port :initarg :port
	 :initarg nil)
   (database :initarg :database
	     :initarg nil))
  "Postgressql connection type inheriting from dbif package.")

(defclass psql-tuple (dbif-tuple)
  nil
  "The PSQL version of the typle object.")

(defun psql-database-list ()
  "Fetch a listing of all the available databases."
  (prog1
      (condition-case nil
	  (save-excursion
	    (set-buffer (get-buffer-create "*PSQL DB LIST*"))
	    (shell-command "psql -F \C-? -l" (current-buffer))
	    (goto-char (point-min))
	    (let* ((tuple (psql-parse-table))
		   (databases nil)
		   (ret nil))
	      (setq databases (oref tuple :values))
	      ;;The following line, if run in emacs 20.2 while in edb
	      ;; will seg-fault emacs.  Hmmm.
	      ;;(setq databases (oref tuple :value))
	      (while databases
		(setq ret (cons (car (car databases)) ret)
		      databases (cdr databases)))
	      (nreverse ret)))
	;(error nil)
	)
    (kill-buffer "*PSQL DB LIST*")
    ))

(defun psql-set-db (database &optional host port)
  "Create a new connection to DATABASE in postgres.
The server is on HOST via PORT."
  (interactive "sDatabase: \nsHost: \nsPort: ")
  (if (string= database "") (setq database nil))
  (if (string= host "") (setq host "localhost"))
  (if (string= port "") (setq port nil))
  (let ((nb (pg:connect database (user-login-name)))); host port)))
    (make-instance psql-connection
		   :pgconnection nb
		   :host host
		   :port port
		   :database database)))

(defmethod dbif-get-table-info ((dbbuff psql-connection) tablename)
  "Return a psql-tuple object with information about tables in this database.
Argument DBBUFF specifies the current connection.
Argument TABLENAME is the name of the table to query."
  (save-excursion
    (dbif-tuple "tmp-tuple"
		:headers (reverse namelst)
		:maxwidths sizelst
		:values (reverse datalst)))
  (dbif-convert-tuple (pg:columns (oref dbbuff :pgconnection) tablename)))

(defmethod dbif-get-table-list ((dbbuff psql-connection))
  "Get a list of available tables from the database specified in DBBUFF."
  (pg:exec (oref dbbuff :pgconnection)
	   "select relname"
	   ;; INCOMPLETE HERE
	   ))

(defmethod dbif-exec ((dbbuff psql-connection) command)
  "Execute the SQL or PSQL command and grab its output.
The output is checked, and if tabular data results, a psql-tuple object
is returned.
DBBUFF is the current connection.
COMMAND should be a string which will execute the PSQL command.  ie,
SQL should end in a semi-colon, \ commands don't.  A carriage return
is supplied by `comint-mode'"
  (let ((T (pg:exec (oref dbbuff :pgconnection) command)))
    (if (member (pg:result T :status) '("ERROR"))
	nil
      (psql-tuple :value T))))

;;; Tuple Accessors
;;
(defmethod dbif-tuple-num-fields ((tuple psql-tuple))
  "Returns the number of fields in TUPLE"
  (length (pg:result (oref tuple :value) :attributes)))

(defmethod dbif-tuple-field-index ((tuple psql-tuple) field)
  "Returns the index (usable by command nth) of the field list.  This
is equivalent to a column number."
  (let* ((f (pg:result (oref tuple :value) :attributes))
	 (l1 (length f)))
    (while (not (string= field (car (car f))))
      (setq f (cdr f)))
    (- l1 (length f))))

(defmethod dbif-tuple-value ((tuple psql-tuple) field index)
  "Extracts from TUPLE the FIELD value in the INDEXED column"
  (nth (dbiff-tuple-field-index tuple field)
       (pg:result (oref tuple :value) :tuple index))
  )

(provide 'psql)

;;; psql.el ends here
