;;; semanticdb-system.el --- Build a file DB for some system files.

;;; Copyright (C) 2002, 2003, 2004, 2005 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-system.el,v 1.10 2007/03/17 21:22:33 zappo Exp $

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
;; A system database is a file based DB which contains tags from a
;; system.  These files are ONLY ever loaded in, and can only be written
;; using a set of user initiated scripts.
;;

(require 'semanticdb-file)
(require 'eieio-opt)

;;;###autoload
(defcustom semanticdb-default-system-save-directory
  ;; Leave this obvious for now.  Maybe hide it later.
  (expand-file-name "~/.semanticdb")
  "*Directory name where semantic cache files for system headers are stored.
System files cannot have caches stored near them because users rarely have
write permission to such paths."
  :group 'semanticdb
  :type '(choice :tag "System-Save-Directory"
                 :menu-tag "System-Save-Directory"
                 (const :tag "Use current directory" :value nil)
                 (directory)))

(defcustom semanticdb-system-database-warn-level 50
  "*Number of files to be added to a system DB that causes us to warn.
If this number is exceeded, warn the users that it could take a while."
  :group 'semanticdb
  :type 'boolean)

(defvar semanticdb-system-force-save nil
  "When non-nil, the system DB will save itself.
Do not set this to non-nil unless you building a system table for
the first time.")

(defvar semanticdb-database-being-created nil
  "Database currently being created.")

;;; Code:
(defclass semanticdb-project-database-system (semanticdb-project-database-file)
  ((file-header-line :initform
		     ";; SEMANTICDB Tags save file for system libraries")
   (file-match-regex :allocation :class
		     :type string
		     :documentation
		     "Regular expression used to match files names for this database.
When building new databases, only matching files will be included.")
   (major-modes :allocation :class
		:type list
		:documentation
		"List of major modes this database is useful to.
Thus, C header files are useful to `c-mode', and to `c++-mode'.")
   ;; Provide an init arg for this item so that it will
   ;; be saved in the file.
   (reference-directory :initarg :reference-directory)
   )
  "Database of file tables for system libraries saved to disk."
  :abstract t)

(defmethod semanticdb-create-database :STATIC ((dbc semanticdb-project-database-system)
					       directory)
  "Create a new semantic database for DIRECTORY and return it.
If a database for DIRECTORY has already been loaded, return it.
If a database for DIRECTORY exists, then load that database, and return it.
If DIRECTORY doesn't exist, create a new one."
  ;; System databases span directories.  Be smart about creation.
  (or semanticdb-database-being-created
      (call-next-method)))

(defmethod semanticdb-write-directory-p
  ((obj semanticdb-project-database-system))
  "Return non-nil if OBJ should be written to disk.
Uses `semanticdb-persistent-path' to determine the return value."
  semanticdb-system-force-save)

(defmethod semanticdb-cache-filename :STATIC
  ((dbclass semanticdb-project-database-system) path)
  "For DBCLASS, return a file to a cache file belonging to PATH.
This could be a cache file in the current directory, or an encoded file
name in a secondary directory."
  ;; This variable hack re-uses logic for file based databases.
  ;; Not too purty, but ok for now.
  (let ((semanticdb-default-save-directory
	 semanticdb-default-system-save-directory))
    (call-next-method)))

;;; User initialization
;;
(defvar semanticdb-system-database-query-history nil
  "History variable when asking for a type of system database.")

;;;###autoload
(defun semanticdb-create-system-database (path &optional class)
  "Create a system database starting at PATH.
PATH should be a top level directory for a series of files containing
declarations for SYSTEM files.  In C, this would be header filaes.
CLASS is the class for the database to create.  Only child classes
of symbol `semanticdb-project-database-system' are accepted."
  (interactive "DPath to system files: ")
  ;; Make sure there is a / at the end.
  (setq path (semanticdb-fix-pathname path))
  ;; Make sure storage is available
  (if (not semanticdb-default-system-save-directory)
      (if (y-or-n-p "Specify value for system database storage now? ")
	  (customize-variable 'semanticdb-default-system-save-directory)
	(error ""))
    ;; Get a dir if needed
    (if (not (file-exists-p semanticdb-default-system-save-directory))
	(if (y-or-n-p
	     (format "Create %s now? " semanticdb-default-system-save-directory))
	    (make-directory semanticdb-default-system-save-directory)
	  (error "")))
    ;; All set with that path.  Ask about system type.
    (if (not class)
	(if (interactive-p)
	    (setq class
		  (eieio-read-subclass "System Type: "
				       semanticdb-project-database-system
				       'semanticdb-system-database-query-history
				       t))
	  (signal 'wrong-type-argument '(class nil))))
    (if (not (child-of-class-p class semanticdb-project-database-system))
	(signal 'wrong-type-argument (list 'class class)))
    ;; Ok!  Just do it!
    (semanticdb-load-system-database class path)
    ))

;;;###autoload
(defun semanticdb-load-system-caches ()
  "Load all system databases that were previously saved."
  (interactive)
  (let ((f (directory-files semanticdb-default-system-save-directory
			    t (concat semanticdb-default-file-name "$") t)))
    (while f
      ;; Emacs makes backup files if we save out the systemDB too often.
      ;; prevent loading backup files which are icky.
      (when (string-match (concat semanticdb-default-file-name "$")
			  (car f))
	(semanticdb-load-database (car f)))
      ;; NOTE FOR THE FUTURE: Verify the system was not expanded for
      ;; each.  This may be slow.
      (setq f (cdr f)))
    ))

(defvar semanticdb-system-db-directory-search-recursed nil
  "Track if we recursed for directory files.")

(defmethod semanticdb-load-system-database :STATIC
  ((dbclass semanticdb-project-database-system) path)
  "Load a system database of type DBCLASS starting at PATH.
PATH should be a top level directory for a series of files containing
declarations for SYSTEM files.  In C, this would be header files.
Only files in PATH matching DBCLASS' regular expression  will be loaded
and parsed. After the database is created, save it, and return the DB."
  ;; For each file do the following:
  ;; 1) If already in database, skip
  ;; 2) Setup semanticdb files to make sure new table shows up
  ;;    in the system database
  ;; 3) Load the file.  Allow normal semantic initialization.
  ;; 4) Force a reparse.
  ;; 5) Kill file if it wasn't already in a buffer.
  (let* ((semanticdb-system-db-directory-search-recursed nil)
	 (files (semanticdb-collect-matching-filenames
		 path (oref-default dbclass file-match-regex)))
	 (sysdb (semanticdb-create-database dbclass path))
	 ;; 2) Set up to use this database when loading.
	 (semanticdb-new-database-class dbclass)
	 )
    (if (and (> (length files) semanticdb-system-database-warn-level)
	     semanticdb-system-db-directory-search-recursed
	     (y-or-n-p
	      (format
	       "%d files found.  Try again without scanning subdirectories? "
	       (length files))))
	(setq files (semanticdb-collect-matching-filenames
		     path (oref-default dbclass file-match-regex) t)))
    (when (> (length files) semanticdb-system-database-warn-level)
      (if (y-or-n-p
	   (format
	    "There are %d files which could a long time to parse.  Proceed? "
	    (length files)))
	  nil ;; Okie dokie
	(error "")))
    (oset sysdb reference-directory path)
    (while files
      (let ((table (semanticdb-file-table sysdb (car files)))
	    )
	;; 1) Skip if loaded
	(unless (and table (oref table tags))
	  ;; 3) load the file.
	  (let ((b (get-file-buffer (car files))))
	    (save-excursion
	      (set-buffer (find-file-noselect (car files)))
	      ;; 4) Force a reparse
	      (semantic-fetch-tags)
	      ;; At this point, standard semantic actions
	      ;; have occured.
	      ;; 5) Kill the buffer
	      (if (not b) (kill-buffer (current-buffer)))))
	  ))
      (setq files (cdr files)))

    ;; All tables are in.  Save this database
    (let ((semanticdb-system-force-save t))
      (semanticdb-save-db sysdb))

    ;; Add it to the search path for major modes.
    (let ((m (oref-default sysdb major-modes)))
      (while m
	(let ((v (mode-local-value (car m) 'semanticdb-project-system-databases))
	      )
	  (setq v (cons sysdb v))
	  ;; NOTE TO SELF; get a set version of `setq-mode-local'.
	  (eval `(setq-mode-local ,(car m)
				  semanticdb-project-system-databases v)))
	(setq m (cdr m))))

    ;; Return it.
    sysdb))

(defun semanticdb-collect-matching-filenames (path regexp &optional not-recursive)
  "Collect a list of all filenames starting at PATH matching REGEXP.
Optional argument NOT-RECURSIVE suggests that this function will not recurse."
  (let ((returnfiles nil)
	(dirs (list path))
	(files nil)
	(useregexp (concat "^[^.].*" regexp)))
    (while dirs

      ;; First, look for more subdirectories.
      (when (not not-recursive)
	(setq files (directory-files (car dirs) t "^[^.]" t))
	(while files
	  (let ((attr (file-attributes (car files))))
	    ;; String in (car attr) is a symlink.
	    (if (and attr (car attr) (not (stringp (car attr))))
		(setq dirs (append dirs (list (car files)))
		      semanticdb-system-db-directory-search-recursed t))
	    (setq files (cdr files)))))

      ;; Now get the list of files we are returning.
      (setq returnfiles
	    (append returnfiles (directory-files (car dirs) t useregexp nil)))

      (setq dirs (cdr dirs)))
    returnfiles))

;;; Here are a a couple implementations
;;
(defclass semanticdb-project-database-system-c (semanticdb-project-database-system)
  ((file-header-line :initform
		     ";; SEMANTICDB Tags save file for system libraries")
   ;; Scan for C header files.
   (file-match-regex :initform "\\.\\(h\\(h\\|xx\\|pp\\|\\+\\+\\)?\\|H\\)?$")
   ;; C modes
   (major-modes :initform '(c-mode c++-mode))
   )
  "Database of file tables for system libraries saved to disk.")


(provide 'semanticdb-system)

;;; semanticdb-system.el ends here
