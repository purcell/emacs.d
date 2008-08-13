;;; semanticdb.el --- Semantic tag database manager

;;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb.el,v 1.116 2008/07/15 01:23:30 zappo Exp $

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
;; Maintain a database of tags for a group of files and enable
;; queries into the database.
;;
;; By default, assume one database per directory.
;;

(require 'inversion)
(eval-and-compile
  (inversion-require 'eieio "1.0"))
(require 'eieio-base)
(require 'semantic)
(eval-when-compile
  (require 'semantic-lex-spp))

;;; Variables:
(defgroup semanticdb nil
  "Parser Generator Persistent Database interface."
  :group 'semantic
  )

;;;###autoload
(defcustom semanticdb-global-mode nil
  "*If non-nil enable the use of `semanticdb-minor-mode'."
  :group 'semantic
  :type 'boolean
  :require 'semanticdb
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semanticdb-minor-mode (if val 1 -1))
         (custom-set-default sym val)))

(defcustom semanticdb-mode-hooks nil
  "*Hooks run whenever `global-semanticdb-minor-mode' is run.
Use `semanticdb-minor-mode-p' to determine if the mode has been turned
on or off."
  :group 'semanticdb
  :type 'hook)

(defvar semanticdb-database-list nil
  "List of all active databases.")

;;;###autoload
(defvar semanticdb-current-database nil
  "For a given buffer, this is the currently active database.")
(make-variable-buffer-local 'semanticdb-current-database)

(defvar semanticdb-current-table nil
  "For a given buffer, this is the currently active database table.")
(make-variable-buffer-local 'semanticdb-current-table)

(defvar semanticdb-new-database-class 'semanticdb-project-database-file
  "The default type of database created for new files.
This can be changed on a per file basis, so that some directories
are saved using one mechanism, and some directories via a different
mechanism.")
(make-variable-buffer-local 'semanticdb-new-database-class)

(defvar semanticdb-default-find-index-class 'semanticdb-find-search-index
  "The default type of search index to use for a `semanticdb-table's.
This can be changed to try out new types of search indicies.")
(make-variable-buffer-local 'semanticdb-default-find=index-class)


;;; ABSTRACT CLASSES
;;
(defclass semanticdb-abstract-table ()
  ((parent-db ;; :initarg :parent-db
    ;; Do not set an initarg, or you get circular writes to disk.
	      :documentation "Database Object containing this table.")
   (major-mode :initarg :major-mode
	       :initform nil
	       :documentation "Major mode this table belongs to.
Sometimes it is important for a program to know if a given table has the
same major mode as the current buffer.")
   (tags :initarg :tags
	 :accessor semanticdb-get-tags
	 :printer semantic-tag-write-list-slot-value
	 :documentation "The tags belonging to this table.")
   (index :type semanticdb-abstract-search-index
	  :documentation "The search index.
Used by semanticdb-find to store additional information about
this table for searching purposes.

Note: This index will not be saved in a persistent file.")
   (cache :type list
	  :initform nil
	  :documentation "List of cache information for tools.
Any particular tool can cache data to a database at runtime
with `semanticdb-cache-get'.

Using a semanticdb cache does not save any information to a file,
so your cache will need to be recalculated at runtime.  Caches can be
referenced even when the file is not in a buffer.

Note: This index will not be saved in a persistent file.")
   )
  "A simple table for semantic tags.
This table is the root of tables, and contains the minimum needed
for a new table not associated with a buffer."
  :abstract t)

(defmethod semanticdb-in-buffer-p ((obj semanticdb-abstract-table))
  "Return a nil, meaning abstract table OBJ is not in a buffer."
  nil)

(defmethod semanticdb-get-buffer ((obj semanticdb-abstract-table))
  "Return a buffer associated with OBJ.
If the buffer is not in memory, load it with `find-file-noselect'."
  nil)

(defmethod semanticdb-set-dirty ((obj semanticdb-abstract-table))
  "Mark the abstract table OBJ dirty.
Abstract tables can not be marked dirty, as there is nothing
for them to synchronize against."
  ;; The abstract table can not be dirty.
  )

;;; Index Cache
;;
(defclass semanticdb-abstract-search-index ()
  ((table :initarg :table
	  :type semanticdb-abstract-table
	  :documentation "XRef to the table this belongs to.")
   )
  "A place where semanticdb-find can store search index information.
The search index will store data about which other tables might be
needed, or perhaps create hash or index tables for the current buffer."
  :abstract t)

(defmethod semanticdb-get-table-index ((obj semanticdb-abstract-table))
  "Return the search index for the table OBJ.
If one doesn't exist, create it."
  (if (slot-boundp obj 'index)
      (oref obj index)
    (let ((idx nil))
      (setq idx (funcall semanticdb-default-find-index-class
			 (concat (object-name obj) " index")
			 ;; Fill in the defaults
		         :table obj
			 ))
      (oset obj index idx)
      idx)))

(defmethod semanticdb-synchronize ((idx semanticdb-abstract-search-index)
				   new-tags)
  "Synchronize the search index IDX with some NEW-TAGS."
  ;; The abstract class will do... NOTHING!
  )

(defmethod semanticdb-partial-synchronize ((idx semanticdb-abstract-search-index)
					   new-tags)
  "Synchronize the search index IDX with some changed NEW-TAGS."
  ;; The abstract class will do... NOTHING!
  )


;;; CONCRETE CLASSES
;;
(defclass semanticdb-table (semanticdb-abstract-table)
  ((file :initarg :file
	 :documentation "File name relative to the parent database.
This is for the file whose tags are stored in this TABLE object.")
   (dirty :initform nil
	  :documentation
	  "Non nil if this table needs to be `Saved'.")
   (db-refs :initform nil
	    :documentation
	    "List of `semanticdb-table' objects refering to this one.
These aren't saved, but are instead recalculated after load.
See the file semanticdb-ref.el for how this slot is used.")
   (pointmax :initarg :pointmax
	     :initform nil
	     :documentation "Size of buffer when written to disk.
Checked on retrieval to make sure the file is the same.")
   (fsize :initarg :fsize
	  :initform nil
	  :documentation "Size of the file when it was last referenced.
Checked when deciding if a loaded table needs updating from changes
outside of Semantic's control.")
   (lastmodtime :initarg :lastmodtime
		:initform nil
		:documentation "Last modification time of the file referenced.
Checked when deciding if a loaded table needs updating from changes outside of
Semantic's control.")
   ;; @todo - need to add `last parsed time', so we can also have
   ;; refresh checks if spp tables or the parser gets rebuilt.
   (unmatched-syntax :initarg :unmatched-syntax
		     :documentation
		     "List of vectors specifying unmatched syntax.")

   (lexical-table :initarg :lexical-table
		  :initform nil
		  :printer semantic-lex-spp-table-write-slot-value
		  :documentation
		  "Table that might be needed by the lexical analyzer.
For C/C++, the C preprocessor macros can be saved here.")
   )
  "A single table of tags derived from file.")

(defmethod semanticdb-set-dirty ((obj semanticdb-table))
  "Mark the abstract table OBJ dirty."
  (oset obj dirty t)
  )

(defmethod object-print ((obj semanticdb-table) &rest strings)
  "Pretty printer extension for `semanticdb-table'.
Adds the number of tags in this file to the object print name."
  (apply 'call-next-method obj
	 (cons (format " (%d tags%s)"
		       (length (semanticdb-get-tags obj))
		       (if (oref obj dirty)
			   " DIRTY" "")
		       )
	       strings)))

;;; DATABASE

(defclass semanticdb-project-database (eieio-instance-tracker)
  ((tracking-symbol :initform semanticdb-database-list)
   (reference-directory :type string
			:documentation "Directory this database refers to.
When a cache directory is specified, then this refers to the directory
this database contains symbols for.")
   (new-table-class :initform semanticdb-table
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   (cache :type list
	  :initform nil
	  :documentation "List of cache information for tools.
Any particular tool can cache data to a database at runtime
with `semanticdb-cache-get'.

Using a semanticdb cache does not save any information to a file,
so your cache will need to be recalculated at runtime.

Note: This index will not be saved in a persistent file.")
   (tables :initarg :tables
	   :type list
	   ;; Need this protection so apps don't try to access
	   ;; the tables without using the accessor.
	   :accessor semanticdb-get-database-tables
	   :protection :protected
	   :documentation "List of `semantic-db-table' objects."))
  "Database of file tables.")

;;; Code:
(defmethod object-print ((obj semanticdb-project-database) &rest strings)
  "Pretty printer extension for `semanticdb-project-database'.
Adds the number of tables in this file to the object print name."
  (apply 'call-next-method obj
	 (cons (format " (%d tables%s)"
		       (length (semanticdb-get-database-tables obj))
		       (if (semanticdb-dirty-p obj)
			   " DIRTY" "")
		       )
	       strings)))

(defmethod semanticdb-create-database :STATIC ((dbc semanticdb-project-database) directory)
  "Create a new semantic database of class DBC for DIRECTORY and return it.
If a database for DIRECTORY has already been created, return it.
If DIRECTORY doesn't exist, create a new one."
  (let ((db (semanticdb-directory-loaded-p directory)))
    (unless db
      (setq db (semanticdb-project-database
		(file-name-nondirectory directory)
		:tables nil))
      ;; Set this up here.   We can't put it in the constructor because it
      ;; would be saved, and we want DB files to be portable.
      (oset db reference-directory (file-truename directory)))
    db))

(defmethod semanticdb-flush-database-tables ((db semanticdb-project-database))
  "Reset the tables in DB to be empty."
  (oset db tables nil))

(defmethod semanticdb-create-table ((db semanticdb-project-database) file)
  "Create a new table in DB for FILE and return it.
The class of DB contains the class name for the type of table to create.
If the table for FILE exists, return it.
If the table for FILE does not exist, create one."
  (let ((newtab (semanticdb-file-table db file)))
    (unless newtab
      ;; This implementation will satisfy autoloaded classes
      ;; for tables.
      (setq newtab (funcall (oref db new-table-class)
			    (file-name-nondirectory file)
			    :file (file-name-nondirectory file)
			    ))
      (oset newtab parent-db db)
      (object-add-to-list db 'tables newtab t))
    newtab))

(defun semanticdb-get-database (filename)
  "Get a database for FILENAME.
If one isn't found, create one."
  (semanticdb-create-database semanticdb-new-database-class (file-truename filename)))

(defun semanticdb-directory-loaded-p (path)
  "Return the project belonging to PATH if it was already loaded."
  (eieio-instance-tracker-find path 'reference-directory 'semanticdb-database-list))

(defmethod semanticdb-file-table ((obj semanticdb-project-database) filename)
  "From OBJ, return FILENAME's associated table object."
  (object-assoc (file-relative-name (file-truename filename)
  				    (oref obj reference-directory))
		'file (oref obj tables)))

(defmethod semanticdb-in-buffer-p ((obj semanticdb-table))
  "Return a buffer associated with OBJ.
If the buffer is in memory, return that buffer."
  ;; Extract the buffer from the tag structure.
  ;; This is faster than lots of find-files and other things
  ;; that mess with the file-system.
  (let ((tag1 (car (semanticdb-get-tags obj))))
    (and (semantic-tag-p tag1)
	 (semantic-tag-in-buffer-p tag1))))

(defmethod semanticdb-get-buffer ((obj semanticdb-table))
  "Return a buffer associated with OBJ.
If the buffer is in memory, return that buffer.
If the buffer is not in memory, load it with `find-file-noselect'."
  (or (semanticdb-in-buffer-p obj)
      (find-file-noselect (semanticdb-full-filename obj) t)))

(defmethod semanticdb-set-buffer ((obj semanticdb-table))
  "Set the current buffer to be a buffer owned by OBJ.
If OBJ's file is not loaded, read it in first."
  (set-buffer (semanticdb-get-buffer obj)))

(defmethod semanticdb-normalize-tags ((obj semanticdb-abstract-table) tags)
  "For the table OBJ, convert a list of TAGS, into standardized form.
The default is to return TAGS.
Some databases may default to searching and providing simplified tags
based on whichever technique used.  This method provides a hook for
them to convert TAG into a more complete form."
  tags)

;;; Cache Cache.
;;
(defclass semanticdb-abstract-cache ()
  ((table :initarg :table
	  :type semanticdb-abstract-table
	  :documentation
	  "Cross reference to the table this belongs to.")
   )
  "Abstract baseclass for tools to use to cache information in semanticdb.
Tools needing a per-file cache must subclass this, and then get one as
needed.  Cache objects are identified in semanticdb by subclass.
In order to keep your cache up to date, be sure to implement
`semanticdb-synchronize', and `semanticdb-partial-synchronize'.
See the file semantic-scope.el for an example."
  :abstract t)

(defmethod semanticdb-cache-get ((table semanticdb-abstract-table)
				 desired-class)
  "Get a cache object on TABLE of class DESIRED-CLASS.
This method will create one if none exists with no init arguments
other than :table."
  (assert (child-of-class-p desired-class 'semanticdb-abstract-cache))
  (let ((cache (oref table cache))
	(obj nil))
    (while (and (not obj) cache)
      (if (eq (object-class-fast (car cache)) desired-class)
	  (setq obj (car cache)))
      (setq cache (cdr cache)))
    (if obj
	obj ;; Just return it.
      ;; No object, lets create a new one and return that.
      (setq obj (funcall desired-class "Cache" :table table))
      (object-add-to-list table 'cache obj)
      obj)))

(defmethod semanticdb-cache-remove ((table semanticdb-abstract-table)
				    cache)
  "Remove from TABLE the cache object CACHE."
  (object-remove-from-list table 'cache cache))

(defmethod semanticdb-synchronize ((cache semanticdb-abstract-cache)
				   new-tags)
  "Synchronize a CACHE with some NEW-TAGS."
  ;; The abstract class will do... NOTHING!
  )

(defmethod semanticdb-partial-synchronize ((cache semanticdb-abstract-cache)
					   new-tags)
  "Synchronize a CACHE with some changed NEW-TAGS."
  ;; The abstract class will do... NOTHING!
  )

(defclass semanticdb-abstract-db-cache ()
  ((db :initarg :db
       :type semanticdb-project-database
       :documentation
       "Cross reference to the database this belongs to.")
   )
  "Abstract baseclass for tools to use to cache information in semanticdb.
Tools needing a database cache must subclass this, and then get one as
needed.  Cache objects are identified in semanticdb by subclass.
In order to keep your cache up to date, be sure to implement
`semanticdb-synchronize', and `semanticdb-partial-synchronize'.
See the file semantic-scope.el for an example."
  :abstract t)

(defmethod semanticdb-cache-get ((db semanticdb-project-database)
				 desired-class)
  "Get a cache object on DB of class DESIRED-CLASS.
This method will create one if none exists with no init arguments
other than :table."
  (assert (child-of-class-p desired-class 'semanticdb-abstract-db-cache))
  (let ((cache (oref db cache))
	(obj nil))
    (while (and (not obj) cache)
      (if (eq (object-class-fast (car cache)) desired-class)
	  (setq obj (car cache)))
      (setq cache (cdr cache)))
    (if obj
	obj ;; Just return it.
      ;; No object, lets create a new one and return that.
      (setq obj (funcall desired-class "Cache" :db db))
      (object-add-to-list db 'cache obj)
      obj)))

(defmethod semanticdb-cache-remove ((db semanticdb-project-database)
				    cache)
  "Remove from TABLE the cache object CACHE."
  (object-remove-from-list db 'cache cache))


(defmethod semanticdb-synchronize ((cache semanticdb-abstract-db-cache)
				   new-tags)
  "Synchronize a CACHE with some NEW-TAGS."
  ;; The abstract class will do... NOTHING!
  )

(defmethod semanticdb-partial-synchronize ((cache semanticdb-abstract-db-cache)
					   new-tags)
  "Synchronize a CACHE with some changed NEW-TAGS."
  ;; The abstract class will do... NOTHING!
  )

;;; REFRESH

(defmethod semanticdb-refresh-table ((obj semanticdb-table))
  "If the tag list associated with OBJ is loaded, refresh it.
This will call `semantic-fetch-tags' if that file is in memory."
  (let ((ff (semanticdb-full-filename obj)))
    (if (find-buffer-visiting ff)
	(save-excursion
	  (semanticdb-set-buffer obj)
	  (semantic-fetch-tags)))))

(defmethod semanticdb-needs-refresh-p ((obj semanticdb-table))
  "Return non-nil of OBJ's tag list is out of date.
The file associated with OBJ does not need to be in a buffer."
  (let* ((ff (semanticdb-full-filename obj))
	 (buff (find-buffer-visiting ff))
	 )
    (if buff
	(save-excursion
	  (set-buffer buff)
	  ;; Use semantic's magic tracker to determine of the buffer is up
	  ;; to date or not.
	  (not (semantic-parse-tree-up-to-date-p))
	  ;; We assume that semanticdb is keeping itself up to date.
	  ;; via all the clever hooks
	  )
      ;; Buffer isn't loaded.  The only clue we have is if the file
      ;; is somehow different from our mark in the semanticdb table.
      (let* ((stats (file-attributes ff 'integer))
	     (actualsize (nth 7 stats))
	     (actualmod (nth 5 stats))
	     )

	(or (not (slot-boundp obj 'tags))
	    ;; (not (oref obj tags)) -->  not needed anymore?
	    (/= (or (oref obj fsize) 0) actualsize)
	    (not (equal (oref obj lastmodtime) actualmod))
	    )
	))))

(defmethod semanticdb-dirty-p ((obj semanticdb-abstract-table))
  "Return non-nil if OBJ is 'dirty'."
  nil)

(defmethod semanticdb-dirty-p ((obj semanticdb-table))
  "Return non-nil if OBJ is 'dirty'."
  (oref obj dirty))

(defmethod semanticdb-dirty-p ((DB semanticdb-project-database))
  "Return non-nil if DB is 'dirty'.
A database is dirty if the state of the database changed in a way
where it may need to resynchronize with some persistent storage."
  (let ((dirty nil)
	(tabs (oref DB tables)))
    (while (and (not dirty) tabs)
      (setq dirty (semanticdb-dirty-p (car tabs)))
      (setq tabs (cdr tabs)))
    dirty))

(defmethod semanticdb-save-db ((DB semanticdb-project-database)
			       &optional supress-questions)
  "Cause a database to save itself.
The database base class does not save itself persistently.
Subclasses could save themselves to a file, or to a database, or other
form."
  nil)

(defun semanticdb-save-current-db ()
  "Save the current tag database."
  (interactive)
  (message "Saving current tag summaries...")
  (semanticdb-save-db semanticdb-current-database)
  (message "Saving current tag summaries...done"))

(defun semanticdb-save-all-db ()
  "Save all semantic tag databases."
  (interactive)
  (message "Saving tag summaries...")
  (mapc 'semanticdb-save-db semanticdb-database-list)
  (message "Saving tag summaries...done"))

(defun semanticdb-save-all-db-idle ()
  "Save all semantic tag databases from idle time.
Exit the save between databases if there is user input."
  (semantic-safe "Auto-DB Save: %S"
    (semantic-exit-on-input 'semanticdb-idle-save
      (mapc (lambda (db)
	      (semantic-throw-on-input 'semanticdb-idle-save)
	      (semanticdb-save-db db t))
	    semanticdb-database-list))
    ))

;;; Directory Project support
;;
(defvar semanticdb-project-predicate-functions nil
  "List of predicates to try that indicate a directory belongs to a project.
This list is used when `semanticdb-persistent-path' contains the value
'project.  If the predicate list is nil, then presume all paths are valid.

Project Management software (such as EDE and JDE) should add their own
predicates with `add-hook' to this variable, and semanticdb will save tag
caches in directories controlled by them.")

(defmethod semanticdb-write-directory-p ((obj semanticdb-project-database))
  "Return non-nil if OBJ should be written to disk.
Uses `semanticdb-persistent-path' to determine the return value."
  nil)

;;; Utilities
;;
;; What is the current database, are two tables of an equivalent mode,
;; and what databases are a part of the same project.
;;;###autoload
(defun semanticdb-current-database ()
  "Return the currently active database."
  (or semanticdb-current-database
      (and default-directory
	   (semanticdb-create-database semanticdb-new-database-class
				       default-directory)
	   )
      nil))

(defvar semanticdb-match-any-mode nil
  "Non-nil to temporarilly search any major mode for a tag.
If a particular major mode wants to search any mode, put the
`semantic-match-any-mode' symbol onto the symbol of that major mode.
Do not set the value of this variable permanently.")

(defmacro semanticdb-with-match-any-mode (&rest body)
  "A Semanticdb search occuring withing BODY will search tags in all modes.
This temporarilly sets `semanticdb-match-any-mode' while executing BODY."
  `(let ((semanticdb-match-any-mode t))
     ,@body))
(put 'semanticdb-with-match-any-mode 'lisp-indent-function 0)

(defmethod semanticdb-equivalent-mode-for-search (table &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
See `semanticdb-equivalent-mode' for details.
This version is used during searches.  Major-modes that opt
to set the `semantic-match-any-mode' property will be able to search
all files of any type."
  (or (get major-mode 'semantic-match-any-mode)
      semanticdb-match-any-mode
      (semanticdb-equivalent-mode table buffer))
  )

(defmethod semanticdb-equivalent-mode ((table semanticdb-abstract-table) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by by `semantic-equivalent-major-modes'
local variable."
  nil)

(defmethod semanticdb-equivalent-mode ((table semanticdb-table) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by by `semantic-equivalent-major-modes'
local variable."
  (save-excursion
    (if buffer (set-buffer buffer))
    (or
     ;; nil major mode in table means we don't know yet.  Assume yes for now?
     (null (oref table major-mode))
     ;; nil means the same as major-mode
     (and (not semantic-equivalent-major-modes)
	  (eq major-mode (oref table major-mode)))
     (and semantic-equivalent-major-modes
	  (member (oref table major-mode) semantic-equivalent-major-modes))
     )
    ))


(defmethod semanticdb-printable-name ((table semanticdb-table))
  "Return a string which is a short and logical printable name for TABLE.
Use this instead of getting the :file slot of the table, which can
sometimes be unbound."
  ;; I know I said that the above is sometimes unbound.
  ;; Not that if this line throws an error, you should go to
  ;; the subclass, and override this method.
  (file-name-nondirectory (file-name-sans-extension (oref table file)))
  )

;;; Associations
;;
;; These routines determine associations between a file, and multiple
;; associated databases.

(defcustom semanticdb-project-roots nil
  "*List of directories, where each directory is the root of some project.
All subdirectories of a root project are considered a part of one project.
Values in this string can be overriden by project management programs
via the `semanticdb-project-root-functions' variable."
  :group 'semanticdb
  :type '(repeat string))

(defvar semanticdb-project-root-functions nil
  "List of functions used to determine a given directories project root.
Functions in this variable can override `semanticdb-project-roots'.
Functions set in the variable are given one argument (a directory) and
must return a string, (the root directory) or a list of strings (multiple
root directories in a more complex system).  This variable should be used
by project management programs like EDE or JDE.")

(defvar semanticdb-project-system-databases nil
  "List of databases containing system library information.
Mode authors can create their own system databases which know
detailed information about the system libraries for querying purposes.
Put those into this variable as a buffer-local, or mode-local
value.")
(make-variable-buffer-local 'semanticdb-project-system-databases)

(defvar semanticdb-search-system-databases t
  "Non nil if search routines are to include a system database.")

(defun semanticdb-current-database-list (&optional dir)
  "Return a list of databases associated with the current buffer.
If optional argument DIR is non-nil, then use DIR as the starting directory.
If this buffer has a database, but doesn't have a project associated
with it, return nil.
First, it checks `semanticdb-project-root-functions', and if that
has no results, it checks `semanticdb-project-roots'.  If that fails,
it returns the results of function `semanticdb-current-database'.
Always append `semanticdb-project-system-databases' if
`semanticdb-search-system' is non-nil."
  (let ((root nil)			; found root directory
	(dbs nil)			; collected databases
	(roots semanticdb-project-roots) ;all user roots
	(dir (file-truename (or dir default-directory)))
	)
    ;; Find the root based on project functions.
    (setq root (run-hook-with-args-until-success
		'semanticdb-project-root-functions
		dir))
    ;; Find roots based on strings
    (while (and roots (not root))
      (let ((r (file-truename (car roots))))
	(if (string-match (concat "^" (regexp-quote r)) dir)
	    (setq root r)))
      (setq roots (cdr roots)))

    ;; If no roots are found, use this directory.
    (unless root (setq root dir))

    ;; Find databases based on the root directory.
    (when root
      ;; The rootlist allows the root functions to possibly
      ;; return several roots which are in different areas but
      ;; all apart of the same system.
      (let ((regexp (concat "^" (regexp-quote root)))
	    (adb semanticdb-database-list) ; all databases
	    )
	(while adb
	  ;; I don't like this part, but close enough.
	  (if (and (slot-boundp (car adb) 'reference-directory)
		   (string-match regexp (oref (car adb) reference-directory)))
	      (setq dbs (cons (car adb) dbs)))
	  (setq adb (cdr adb))))
      )
    ;; Add in system databases
    (when semanticdb-search-system-databases
      (setq dbs (nconc dbs semanticdb-project-system-databases)))
    ;; Return
    dbs))


;;; Hooks:
;;
(defun semanticdb-semantic-init-hook-fcn ()
  "Function saved in `semantic-init-db-hooks'.
Sets up the semanticdb environment."
  ;; Only initialize semanticdb if we have a file name.
  ;; There is no reason to cache a tag table if there is no
  ;; way to load it back in later.
  (when (buffer-file-name)
    (let* ((ans (semanticdb-create-table-for-file (buffer-file-name)))
	   (cdb (car ans))
	   (ctbl (cdr ans))
	   )
      ;; Get the current DB for this directory
      (setq semanticdb-current-database cdb)
      ;; We set the major mode because we know what it is.
      (oset ctbl major-mode major-mode)
      ;; Local state
      (setq semanticdb-current-table ctbl)
      ;; Try to swap in saved tags
      (if (or (not (slot-boundp ctbl 'tags)) (not (oref ctbl tags))
	      (/= (or (oref ctbl pointmax) 0) (point-max))
	      )
	  (semantic-clear-toplevel-cache)
	;; Unmatched syntax
	(condition-case nil
	    (semantic-set-unmatched-syntax-cache
	     (oref ctbl unmatched-syntax))
	  (unbound-slot
	   ;; Old version of the semanticdb table can miss the unmatched
	   ;; syntax slot.  If so, just clear the unmatched syntax cache.
	   (semantic-clear-unmatched-syntax-cache)
	   ;; Make sure it has a value.
	   (oset ctbl unmatched-syntax nil)
	   ))
	;; Keep lexical tables up to date.  Don't load
	;; semantic-spp if it isn't needed.
	(let ((lt (oref ctbl lexical-table)))
	  (when lt
	    (require 'semantic-lex-spp)
	    (semantic-lex-spp-set-dynamic-table lt)))
	;; Set the main tag cache.
	;; This must happen after setting up buffer local variables
	;; since this will turn around and re-save those variables.
	(semantic--set-buffer-cache (oref ctbl tags))
	;; Don't need it to be dirty.  Set dirty due to hooks from above.
	(oset ctbl dirty nil) ;; Special case here.
	;; Bind into the buffer.
	(semantic--tag-link-cache-to-buffer)
	)
      )))

(defun semanticdb-create-table-for-file (filename)
  "Initialize a database table for FILENAME, and return it.
If FILENAME exists in the database already, return that.
If there is no database for the table to live in, create one."
  (let ((cdb nil)
	(dd (file-name-directory filename))
	)
    ;; Allow a database override function
    (setq cdb (semanticdb-create-database semanticdb-new-database-class
					  dd))
    ;; Get a table for this file.
    (let ((tbl (semanticdb-create-table cdb filename)))
      (cons cdb tbl))
    ))

(defmethod semanticdb-synchronize ((table semanticdb-abstract-table)
				   new-tags)
  "Synchronize the table TABLE with some NEW-TAGS."
  (oset table tags new-tags)
  (oset table pointmax (point-max))
  (let ((fattr (file-attributes
		(semanticdb-full-filename table)
		'integer)))
    (oset table fsize (nth 7 fattr))
    (oset table lastmodtime (nth 5 fattr))
    )
  ;; Assume it is now up to date.
  (oset table unmatched-syntax semantic-unmatched-syntax-cache)
  ;; The lexical table should be good too.
  (when (featurep 'semantic-lex-spp)
    (oset table lexical-table (semantic-lex-spp-save-table)))
  ;; this implies dirtyness
  (semanticdb-set-dirty table)

  ;; Synchronize the index
  (when (slot-boundp table 'index)
    (let ((idx (oref table index)))
      (when idx (semanticdb-synchronize idx new-tags))))

  ;; Synchronize application caches.
  (let ((caches (copy-list (oref table cache))))
    (while caches
      (semanticdb-synchronize (car caches) new-tags)
      (setq caches (cdr caches))))

  ;; Update cross references
  ;; (semanticdb-refresh-references table)
  )

(defmethod semanticdb-partial-synchronize ((table semanticdb-abstract-table)
					   new-tags)
  "Synchronize the table TABLE where some NEW-TAGS changed."
  ;; You might think we need to reset the tags, but since the partial
  ;; parser splices the lists, we don't need to do anything
  ;;(oset table tags new-tags)
  ;; We do need to mark ourselves dirty.
  (semanticdb-set-dirty table)

  ;; Incremental parser doesn't mokey around with this.
  (oset table unmatched-syntax semantic-unmatched-syntax-cache)

  ;; Synchronize the index
  (when (slot-boundp table 'index)
    (let ((idx (oref table index)))
      (when idx (semanticdb-partial-synchronize idx new-tags))))

  ;; Synchronize application caches.
  (let ((caches (oref table cache)))
    (while caches
      (semanticdb-synchronize (car caches) new-tags)
      (setq caches (cdr caches))))

  ;; Update cross references
  ;;(when (semantic-find-tags-by-class 'include new-tags)
  ;;  (semanticdb-refresh-references table))
  )

(defun semanticdb-synchronize-table (new-table)
  "Function run after parsing.
Argument NEW-TABLE is the new table of tags."
  (when semanticdb-current-table
    (semanticdb-synchronize semanticdb-current-table new-table)))

(defun semanticdb-partial-synchronize-table (new-table)
  "Function run after parsing.
Argument NEW-TABLE is the new table of tags."
  (when semanticdb-current-table
    (semanticdb-partial-synchronize semanticdb-current-table new-table)))

(defun semanticdb-revert-hook ()
  "Hook run before a revert buffer.
We can't track incremental changes due to a revert, so just clear the cache.
This will prevent the next batch of hooks from wasting time parsing things
that don't need to be parsed."
  (if (and (semantic-active-p)
	   semantic--buffer-cache
	   semanticdb-current-table)
      (semantic-clear-toplevel-cache)))

(defun semanticdb-kill-hook ()
  "Function run when a buffer is killed.
If there is a semantic cache, slurp out the overlays, and store
it in our database.  If that buffer has no cache, ignore it, we'll
handle it later if need be."
  (when (and (semantic-active-p)
	     semantic--buffer-cache
	     semanticdb-current-table)
      
    ;; Try to get a fast update.
    (semantic-fetch-tags-fast)

    ;; If the buffer is in a bad state, don't save anything...
    (if (semantic-parse-tree-needs-rebuild-p)
	;; If this is the case, don't save anything.
	(progn
	  (semantic-clear-toplevel-cache)
	  (oset semanticdb-current-table pointmax 0)
	  (oset semanticdb-current-table fsize 0)
	  (oset semanticdb-current-table lastmodtime nil)
	  )
      ;; We have a clean buffer, save it off.
      (condition-case nil
	  (progn
	    (semantic--tag-unlink-cache-from-buffer)
	    ;; Set pointmax only if we had some success in the unlink.
	    (oset semanticdb-current-table pointmax (point-max))
	    (let ((fattr (file-attributes
			  (semanticdb-full-filename
			   semanticdb-current-table)
			  'integer)))
	      (oset semanticdb-current-table fsize (nth 7 fattr))
	      (oset semanticdb-current-table lastmodtime (nth 5 fattr))
	      ))
	;; If this messes up, just clear the system
	(error
	 (semantic-clear-toplevel-cache)
	 (message "semanticdb: Failed to deoverlay tag cache.")))
      )
    ))

(defun semanticdb-kill-emacs-hook ()
  "Function called when Emacs is killed.
Save all the databases."
  (semanticdb-save-all-db))

;;; Start/Stop database use
;;
(defvar semanticdb-hooks
  '((semanticdb-semantic-init-hook-fcn semantic-init-db-hooks)
    (semanticdb-synchronize-table semantic-after-toplevel-cache-change-hook)
    (semanticdb-partial-synchronize-table semantic-after-partial-cache-change-hook)
    (semanticdb-revert-hook before-revert-hook)
    (semanticdb-kill-hook kill-buffer-hook)
    (semanticdb-kill-hook change-major-mode-hook) ;; Not really a kill, but we need the same effect.
    (semanticdb-kill-emacs-hook kill-emacs-hook)
    (semanticdb-save-all-db-idle auto-save-hook)
    )
  "List of hooks and values to add/remove when configuring semanticdb.")

;;;###autoload
(defun semanticdb-minor-mode-p ()
  "Return non-nil if `semanticdb-minor-mode' is active."
  (member (car (car semanticdb-hooks))
	  (symbol-value (car (cdr (car semanticdb-hooks))))))

;;;###autoload
(defun global-semanticdb-minor-mode (&optional arg)
  "Toggle the use of `semanticdb-minor-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (if (not arg)
      (if (semanticdb-minor-mode-p)
	  (setq arg -1)
	(setq arg 1)))
  (let ((fn 'add-hook)
	(h semanticdb-hooks)
	(changed nil))
    (if (< arg 0)
	(setq changed semanticdb-global-mode
	      semanticdb-global-mode nil
              fn 'remove-hook)
      (setq changed (not semanticdb-global-mode)
	    semanticdb-global-mode t))
    ;(message "ARG = %d" arg)
    (when changed
      (while h
	(funcall fn (car (cdr (car h))) (car (car h)))
	(setq h (cdr h)))
      ;; Call a hook
      (run-hooks 'semanticdb-mode-hooks))
    ))

(defun semanticdb-toggle-global-mode ()
  "Toggle use of the Semantic Database feature.
Update the environment of Semantic enabled buffers accordingly."
  (interactive)
  (if (semanticdb-minor-mode-p)
      ;; Save databases before disabling semanticdb.
      (semanticdb-save-all-db))
  ;; Toggle semanticdb minor mode.
  (global-semanticdb-minor-mode))


;;; Generic Accessor Routines
;;
;; These routines can be used to get at tags in files w/out
;; having to know a lot about semanticDB.

;;;###autoload
(defun semanticdb-file-table-object (file &optional dontload)
  "Return a semanticdb table belonging to FILE, make it up to date.
If file has database tags available in the database, return it.
If file does not have tags available, and DONTLOAD is nil,
then load the tags for FILE, and create a new table object for it.
DONTLOAD does not affect the creation of new database objects."
  (setq file (file-truename file))
  ;; (message "Object Translate: %s" file)
  (when (file-exists-p file)
    (let* ((default-directory (file-name-directory file))
	   (db (or
		;; This line will pick up system databases.
		(semanticdb-directory-loaded-p default-directory)
		;; this line will make a new one if needed.
		(semanticdb-get-database default-directory)))
	   (tab (semanticdb-file-table db file))
	   )
      (cond
       ((and tab
	     ;; Is this in a buffer?
	     (find-buffer-visiting (semanticdb-full-filename tab))
	     )
	(save-excursion
	  (set-buffer (find-buffer-visiting (semanticdb-full-filename tab)))
	  (semantic-fetch-tags)
	  ;; Return the table.
	  tab))
       ((and tab dontload)
	;; If we have table, and we don't want to load it, just return it.
	tab)
       ((and tab
	     ;; Is table fully loaded, or just a proxy?
	     (number-or-marker-p (oref tab pointmax))
	     ;; Is this table up to date with the file?
	     (not (semanticdb-needs-refresh-p tab)))
	;; A-ok!
	tab)
       ((find-buffer-visiting file)
	;; If FILE is being visited, but none of the above state is
	;; true (meaning, there is no table object associated with it)
	;; then it is a file not supported by Semantic, and can be safely
	;; ignored.
	nil)
       ((not dontload) ;; We must load the file.
	(save-excursion
	  (let* ( ;; This is a brave statement.  Don't waste time loading in
		 ;; lots of modes.  Especially decoration mode can waste a lot
		 ;; of time for a buffer we intend to kill.
		 (semantic-init-hooks nil)
		 ;; This disables the part of EDE that asks questions
		 (ede-auto-add-method 'never)
		 ;; Ask font-lock to not colorize these buffers, nor to
		 ;; whine about it either.
		 (font-lock-maximum-size 0)
		 (font-lock-verbose nil)
		 ;; Remember the buffer to kill
		 (kill-buffer-flag (find-buffer-visiting file))
		 (buffer-to-kill (or kill-buffer-flag
				     (find-file-noselect file t))))

	    ;; Debug some issue here?
	    (when kill-buffer-flag
	      (debug))

	    (set-buffer buffer-to-kill)
	    ;; Find file should automatically do this for us.
	    ;; Sometimes the DB table doesn't contains tags and needs
	    ;; a refresh.  For example, when the file is loaded for
	    ;; the first time, and the idle scheduler didn't get a
	    ;; chance to trigger a parse before the file buffer is
	    ;; killed.
	    (when semanticdb-current-table
	      (semantic-fetch-tags))
	    (prog1
		semanticdb-current-table
	      (when (not kill-buffer-flag)
		;; If we had to find the file, then we should kill it
		;; to keep the master buffer list clean.
		(kill-buffer buffer-to-kill)
		))))
	)
       (t
	;; We were asked not to load the file in and parse it.
	;; Instead just create a database table with no tags
	;; and a claim of being empty.
	;;
	;; This will give us a starting point for storing
	;; database cross-references so when it is loaded,
	;; the cross-references will fire and caches will
	;; be cleaned.
	(let ((ans (semanticdb-create-table-for-file file)))
	  (cdr ans))
	)
       )
      )))

;;;###autoload
(defun semanticdb-file-stream (file)
  "Return a list of tags belonging to FILE.
If file has database tags available in the database, return them.
If file does not have tags available, then load the file, and create them."
  (let ((table (semanticdb-file-table-object file)))
    (when table
      (semanticdb-get-tags table))))

(provide 'semanticdb)

;;; semanticdb.el ends here
