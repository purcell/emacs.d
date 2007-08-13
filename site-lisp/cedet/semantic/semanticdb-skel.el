;;; semanticdb-skel.el --- Semantic database extensions for SKEL

;;; Copyright (C) 2002, 2003, 2004, 2005, 2006 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-skel.el,v 1.3 2006/07/29 15:02:49 zappo Exp $

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
;; If you want to make an omniscient semanticdb database, start with
;; this skeleton.  Replace `SKEL' with your language.  Implement those
;; areas described by comments.
;;

(require 'semanticdb-search)
(eval-when-compile
  ;; For generic function searching.
  (require 'eieio)
  (require 'eieio-opt)
  )
;;; Code:

;;; Classes:
(defclass semanticdb-table-SKEL (semanticdb-search-results-table)
  ((major-mode :initform SKEL-mode)
   )
  "A table for returning search results from SKEL.")

(defclass semanticdb-project-database-SKEL
  (semanticdb-project-database
   ;; Use SINGLETON if there should be only one copy of this database.
   ;; Do not use this if you need a different copy for different projects.
   ;; eieio-singleton
   )
  ((new-table-class :initform semanticdb-table-SKEL
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   )
  "Database representing SKEL.")

;; Create the database, and add it to searchable databases for SKEL mode.
(defvar-mode-local YOUR-MAJOR-mode semanticdb-project-system-databases
  (list 
   (semanticdb-project-database-SKEL "SKEL"))
  "Search SKEL for symbols.")

;; NOTE: Be sure to modify this to the best advantage of your
;;       language.
(defvar-mode-local YOUR-MAJOR-mode semanticdb-find-default-throttle
  '(project omniscience)
  "Search project files, then search this omniscience database.
It is not necessary to to system or recursive searching because of
the omniscience database.")

;;; Filename based methods
;;
(defmethod semanticdb-get-database-tables ((obj semanticdb-project-database-SKEL))
  "For a SKEL database, there are no explicit tables.
Create one of our special tables that can act as an intermediary."
  ;; NOTE: This method overrides an accessor for the `tables' slot in 
  ;;       a database.  You can either construct your own (like tmp here
  ;;       or you can manage any number of tables.

  ;; We need to return something since there is always the "master table"
  ;; The table can then answer file name type questions.
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (semanticdb-table-emacs-lisp "tmp")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)
      ))
  (call-next-method))

(defmethod semanticdb-file-table ((obj semanticdb-project-database-SKEL) filename)
  "From OBJ, return FILENAME's associated table object."
  ;; NOTE: See not for `semanticdb-get-database-tables'.
  (car (semanticdb-get-database-tables obj))
  )

(defmethod semanticdb-get-tags ((table semanticdb-table-SKEL ))
  "Return the list of tags belonging to TABLE."
  ;; NOTE: Omniscient databases probably don't want to keep large tabes
  ;;       lolly-gagging about.  Keep internal Emacs tables empty and
  ;;       refer to alternate databases when you need something.
  nil)

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-SKEL) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by by `semantic-equivalent-major-modes'
local variable."
  (save-excursion
    (set-buffer buffer)
    (eq (or mode-local-active-mode major-mode) 'SKEL-mode)))

;;; Usage
;;
;; Unlike other tables, an omniscent database does not need to
;; be associated with a path.  Use this routine to always add ourselves
;; to a search list.
(define-mode-local-override semanticdb-find-translate-path SKEL-mode
  (path brutish)
  "Return a list of semanticdb tables asociated with PATH.
If brutish, do the default action.
If not brutish, do the default action, and append the system
database (if available.)"
  (let ((default
	  ;; When we recurse, disable searching of system databases
	  ;; so that our ELisp database only shows up once when
	  ;; we append it in this iteration.
	  (let ((semanticdb-search-system-databases nil)
		)
	    (semanticdb-find-translate-path-default path brutish))))
    ;; Don't add anything if BRUTISH is on (it will be added in that fcn)
    ;; or if we aren't supposed to search the system.
    (if (or brutish (not semanticdb-search-system-databases))
	default
      (let ((tables (apply #'append
			   (mapcar
			    (lambda (db) (semanticdb-get-database-tables db))
			    semanticdb-project-system-databases))))
	(append default tables)))))

;;; Search Overrides
;;
;; NOTE WHEN IMPLEMENTING: Be sure to add doc-string updates explaining
;; how your new search routines are implemented.
;;
(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-SKEL) name &optional tags)
  "Find all tags named NAME in TABLE.
Return a list of tags."
  (if tags
      ;; If TAGS are passed in, then we don't need to do work here.
      (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    ))

(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-SKEL) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE    
    ))

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-SKEL) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    ))

(defmethod semanticdb-find-tags-by-class-method
  ((table semanticdb-table-SKEL) class &optional tags)
  "In TABLE, find all occurances of tags of CLASS.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    ;;
    ;; Note: This search method could be considered optional in an
    ;;       omniscient database.  It may be unwise to return all tags
    ;;       that exist for a language that are a variable or function.
    ;;
    ;; If it is optional, you can just delete this method.
    nil))

;;; Deep Searches
;;
;; If your language does not have a `deep' concept, these can be left
;; alone, otherwise replace with implementations similar to those
;; above. 
;;
(defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-SKEL) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags t
Like `semanticdb-find-tags-by-name-method' for SKEL."
  (semanticdb-find-tags-by-name-method table name tags))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-SKEL) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for SKEL."
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-SKEL) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for SKEL."
  (semanticdb-find-tags-for-completion-method table prefix tags))

;;; Advanced Searches
;;
(defmethod semanticdb-find-tags-external-children-of-type-method
  ((table semanticdb-table-SKEL) type &optional tags)
  "Find all nonterminals which are child elements of TYPE
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    ;;
    ;; OPTIONAL: This could be considered an optional function.  It is
    ;;       used for `semantic-adopt-external-members' and may not
    ;;       be possible to do in your language.
    ;;
    ;; If it is optional, you can just delete this method.
    ))

(provide 'semanticdb-el)

;;; semanticdb-el.el ends here
