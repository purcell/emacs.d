;;; dbif.e --  Generic Database superclass for handling unique databases
;;
;; Copyright (C) 1996, 1998, 2000 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; Version: 0.1
;; RCS: $Id: dbif.el,v 1.3 2001/01/11 18:48:26 zappo Exp $
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
;; Please send bug reports, etc. to zappo@gnu.org
;;
;; Updates can be found at:
;;    ftp://ftp.ultranet.com/pub/zappo

;;; Commentary:
;;   All sql database have similar behaviours, but some aspects are
;; different.  They all use ansi SQL, they all can provide a list of
;; available tables, and a schema for those tables.  To access some
;; of this information is different.
;;   dbif tries to abstract that which is common from the specifics,
;; so any database type can be created, and overload the methods used
;; to get at this information.  This will provide the ability to
;; create super-database-accessor programs which simply invoke
;; different backends.
;;   Because this code is so short, a summary isn't provided.  See
;; dbif-browse for details on how to create and use different
;; database accessors.
;;
;; General assumptions:
;; * A connection to a database will use a special buffer w/ a process
;;   or tcp connection
;; * Will have special ways of getting a list of tables
;; * Will have a special way of getting a tables schema
;;
;; Other types of databases could be used, and simply ignore these
;; fields if that happens to be the case (like maybe EDB?)

(require 'eieio)
(require 'comint)

;;; Code:
(defvar dbif-table-list nil
  "Process id of the dbif command running as a sub-process.")

(defvar dbif-max-parse nil
  "Sets the maximum number of fields to parse before stopping.
If nil, find everything.")

(defclass dbif-tuple ()
  ((values :initarg :values
	   :initform nil))
  "Definition for a DBIF tuple.
Contains data about fields such as names and dimentions.")

(defclass dbif-connection ()
  ((buffer :initarg :buffer
	   :initform nil)
   )
  "Definition for a DBIF connection.  Represents basic things such as
the buffer output goes.")

(defmethod dbif-tuple-num-fields ((tuple dbif-tuple))
  "Returns the number of fields in TUPLE"
  (error "Unimplemented method dbif-get-table-info for %s"
	 (object-name dbbuff)))
;  (length (oref tuple headers)))

(defmethod dbif-tuple-field-index ((tuple dbif-tuple) field)
  "Returns the index (usable by command nth) of the field list.  This
is equivalent to a column number."
  (error "Unimplemented method dbif-get-table-info for %s"
	 (object-name dbbuff)))
;;  (let ((ix 0)
;;	(sl (oref tuple headers)))
;;    (while sl
;;      (if (string= (car sl) field)
;;	  (setq sl nil)
;;	(setq sl (cdr sl))
;;	(setq ix (1+ ix))))
;;    ix))

(defmethod dbif-tuple-value ((tuple dbif-tuple) field index)
  "Extracts from TUPLE the FIELD value in the INDEXED column"
  (error "Unimplemented method dbif-get-table-info for %s"
	 (object-name dbbuff)))
;  (nth (dbif-tuple-field-index tuple field) (nth index (oref tuple values))))

(defmethod dbif-get-table-info ((dbbuff dbif-connection) tablename)
  "Returns a dbif-tuple object containing information about the tables
in this database."
  (error "Unimplemented method dbif-get-table-info for %s"
	 (object-name dbbuff))
  nil)

(defmethod dbif-get-table-list ((dbbuff dbif-connection))
  "Get a list of available tables from the database specified in dbbuff"
  (error "Unimplemented method dbif-get-table-list for %s"
	 (object-name dbbuff))
  nil)

(defmethod dbif-exec ((dbbuff dbif-connection) command)
  "Execute the DBIF COMMAND and gret its output.  The output is
checked, and if tabular data results, a dbif-tuple object is returned.
COMMAND should be a string which will execute the DBIF command."
  (error "Unimplemented method dbif-exec for %s" (object-name dbbuff))
  nil)

(provide 'dbif)

;;; dbif.el ends here
