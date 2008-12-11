;;; semanticdb-java.el --- Semantic database extensions for Java

;;; Copyright (C) 2003, 2007, 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-java.el,v 1.4 2008/10/06 16:51:39 zappo Exp $

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
  "A table for returning search results from Beanshell.")

(defclass semanticdb-project-database-java
  (semanticdb-project-database eieio-singleton)
  ((new-table-class :initform semanticdb-table-java
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   )
  "Database representing Java pre-compiled class libraries.")

;; Create the database, and add it to searchable databases for Java mode.
(defvar-mode-local java-mode semanticdb-project-system-databases
  (list
   ;; Create a global instance for all Java source files.
   (semanticdb-project-database-java "Java"))
  "Search Java Class files for for symbols.")


;; NOTE: Be sure to modify this to the best advantage of your
;;       language.

(defvar-mode-local java-mode semanticdb-find-default-throttle 
  '(project system  omniscience)
  "Search project files, then search this omniscience database.
It is not necessary to do system or recursive searching because of
the omniscience database.")


;;; just a hack...
;;(define-mode-local-override semanticdb-find-translate-path java-mode (path brutish)
;;  "brutish hack" 
;;  (progn 
;;    (message "semanticdb-find-translate-path java-mode %s " path)
;;    (semanticdb-find-translate-path-default path t); always do brutish search. its a hack.
;;    )
;;  )



;;; Filename based methods
;;
;; (defmethod semanticdb-file-table ((obj semanticdb-project-database-java) filename)
;;   "From OBJ, return FILENAME's associated table object."
;;   ;; Creates one table for all of the compiled java environment.
;;   ;; Query the environment each time?
;;   ;; Perhaps it should instead keep a group of tables for each file
;;   ;; actually queried and cache the results.
;;   (message "semanticdb-file-table")
;;   (if (slot-boundp obj 'tables)
;;       (car (oref obj tables))
;;     (let ((newtable (semanticdb-table-java "javatable")))
;;       (oset obj tables (list newtable))
;;       (oset newtable parent-db obj)
;;       (oset newtable tags nil)
;;       newtable)))



; try this stuff from -el implementation instead

(defmethod semanticdb-get-database-tables ((obj semanticdb-project-database-java))
  "For an Emacs Lisp database, there are no explicit tables.
Create one of our special tables that can act as an intermediary."
  ;; We need to return something since there is always the "master table"
  ;; The table can then answer file name type questions.
  (message "semanticdb-get-database-tables java")
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (semanticdb-table-java "javatmp")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)
      ))
  (call-next-method))

(defmethod semanticdb-file-table ((obj semanticdb-project-database-java) filename)
  "From OBJ, return FILENAME's associated table object.
For Emacs Lisp, creates a specialized table."
  (car (semanticdb-get-database-tables obj))
  )






(defmethod semanticdb-get-tags ((table semanticdb-table-java ))
  "Return the list of tags belonging to TABLE."
  ;; specialty table ?  Probably derive tags at request time?
  (message "semanticdb-get-tags")
  )

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-java) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by by `semantic-equivalent-major-modes'
local variable."
  (save-excursion
    (set-buffer buffer)
    (or (eq (or mode-local-active-mode major-mode) 'java-mode)
	(eq (or mode-local-active-mode major-mode) 'jde-mode))))
;    (or (eq major-mode 'java-mode)
;	(eq major-mode 'jde-mode))))

    
(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-java) name  &optional tags)
  "Find all tags name NAME in TABLE.
Return a list of tags."
  (message "semanticdb-find-tags-by-name-method %s" name)
  
  (list (semantic-tag name 'type))
  )

  ; this should only return a type, and then find-external should return a list of members
  ;(semantic-find-tags-by-name name (or tags (semanticdb-get-tags table)))  )

(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-java) regex  &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Return a list of tags."
  (message " semanticdb-find-tags-by-name-regexp-method %s" regex)
  (semantic-find-tags-by-name-regexp regexp (or tags (semanticdb-get-tags table)))
  )

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-java) prefix  &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Returns a table of all matching tags."
  (message "semanticdb-find-tags-for-completion-method %s" prefix)
  (semantic-find-tags-for-completion prefix (or tags (semanticdb-get-tags table)))
  )


;;; Deep Searches
;;
;; If your language does not have a `deep' concept, these can be left
;; alone, otherwise replace with implementations similar to those
;; above. 
;;
(defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-java) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags t
Like `semanticdb-find-tags-by-name-method' for java."
  (message "semanticdb-deep-find-tags-by-name-method")
  (semanticdb-find-tags-by-name-method table name tags))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-java) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for java."
  (message " semanticdb-deep-find-tags-by-name-regexp-method")
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-java) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for java."
  (message " semanticdb-deep-find-tags-for-completion-method")
  (semanticdb-find-tags-for-completion-method table prefix tags))

;;; Advanced Searches
;;
(defmethod semanticdb-find-tags-external-children-of-type-method
  ((table semanticdb-table-java) type &optional tags)
  "Find all nonterminals which are child elements of TYPE
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (message "semanticdb-find-tags-external-children-of-type-method 2 %s %s" type tags)
  (if tags (call-next-method)
    ;; YOUR IMPLEMENTATION HERE
    ;;
    ;; OPTIONAL: This could be considered an optional function.  It is
    ;;       used for `semantic-adopt-external-members' and may not
    ;;       be possible to do in your language.
    ;;
    ;; If it is optional, you can just delete this method.

    ; use the jde to call the bsh to ge classinfo
    ; see also jde-complete-get-classinfo
    (let* ((classinfo (nth 2 (car  (jde-complete-invoke-get-class-info type jde-complete-public)))) ; all method names and argument types, last is exception type:  ("wait" "void" "long" "int"  ("java.lang.InterruptedException"))
           (classinfo2 (mapcar
                        (lambda (el) (semantic-tag-new-function (car el) ;method name name
                                                                nil; (cadr (nreverse el)) ;returntype; seems to confuse completion

                                                                (mapcar
                                                                 (lambda (argtype) (semantic-tag-new-variable argtype argtype  ""))
                                                                 (cdr (nreverse (cdr (nreverse el)))) ;skip 1st and last element, these are the argument types for the function
                                                                 )


                                                                ));arglist
                        classinfo))
           )
      classinfo2
;TODO convert this: ("replace(int, int, java.lang.String) : java.lang.StringBuffer" . "replace(int, int, java.lang.String)")
;to a proper semantic tag
;also consider vars, like:  ("count : int" . "count")      
    )))



;;; Bean Shell Queries
;;



(provide 'semanticdb-java)
;;; semanticdb-java.el ends here
