;;; semanticdb-java.el --- Semantic database extensions for Java

;;; Copyright (C) 2003 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-java.el,v 1.3 2005/09/30 20:19:23 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semanticdb is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;; 
;;; Commentary:
;;
;; A majority of Java's useful functionality is inside class files.
;; Often these class files do not have parsable source available.  In
;; order to get full advantage of speedbar extensions, access to
;; these class files is needed.
;;
;; The `semantic-project-database-java' class inherits from the
;; the database base class.  It uses the JDEE BeanShell installation to
;; query the class files, and create token compatible with Semantic.
;;

(require 'semanticdb-search)

;;; Code:

;;; Classes:
(defclass semanticdb-table-java (semanticdb-search-results-table)
  ((major-mode :initform java-mode)
   )
  "A table for returning search results from Emacs.")

(defclass semanticdb-project-database-java
  (semanticdb-project-database eieio-singleton)
  ((new-table-class :initform semanticdb-table-java
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   )
  "Database representing Java pre-compiled class libraries.")

;; Create the database, and add it to searchable databases for Emacs Lisp mode.
(defvar-mode-local java-mode semanticdb-project-system-databases
  (list
   ;; Create a global instance for all Java source files.
   (semanticdb-project-database-java "Java"))
  "Search Java Class files for for symbols.")

;;; Filename based methods
;;
(defmethod semanticdb-file-table ((obj semanticdb-project-database-java) filename)
  "From OBJ, return FILENAME's associated table object."
  ;; Creates one table for all of the compiled java environment.
  ;; Query the environment each time?
  ;; Perhaps it should instead keep a group of tables for each file
  ;; actually queried and cache the results.
  (if (slot-boundp obj 'tables)
      (car (oref obj tables))
    (let ((newtable (semanticdb-table-java "table")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)
      newtable)))

(defmethod semanticdb-get-tags ((table semanticdb-table-java ))
  "Return the list of tags belonging to TABLE."
  ;; specialty table ?  Probably derive tags at request time?
  nil)

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-java) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by by `semantic-equivalent-major-modes'
local variable."
  (save-excursion
    (set-buffer buffer)
    (or (eq major-mode 'java-mode)
	(eq major-mode 'jde-mode))))

(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-java) name)
  "Find all tags name NAME in TABLE.
Return a list of tags."
  )

(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-java) regex)
  "Find all tags with name matching REGEX in TABLE.
Return a list of tags."
  )

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-java) prefix)
  "In TABLE, find all occurances of tags matching PREFIX.
Returns a table of all matching tags."
  )

;;; Advanced Searches
;;
(defmethod semanticdb-find-tags-external-children-of-type-method
  ((table semanticdb-table-java) type)
  "Find all nonterminals which are child elements of TYPE
Return a list of tags."
  )

;;; Bean Shell Queries
;;
(defun semanticdb-beanshell-search (querytext)
  "Query the beanshell with QUERYTEXT.
Return a list of semantic compatible tokens."

  )

(provide 'semanticdb-java)
;;; semanticdb-java.el ends here
