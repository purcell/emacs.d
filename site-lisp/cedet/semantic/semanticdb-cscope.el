;;; semanticdb-cscope.el --- Use CSCOPE databases w/ Semantic

;; Copyright (C) 2007, 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semanticdb-cscope.el,v 1.3 2008/10/10 21:41:39 zappo Exp $

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
;; cscope is a powerful way to analyze C code and create databases.
;; This provides a way to use cscope in semantic to lookup tags.

;;; TODO -
;;
;; * The actual arguments to pass to cscope  needs to be written
;; * Parsing of cscope output
;; * listing of files in a cscope database
;; * various search routines.
;;
;; To create a DB:
;;   M-x semanticdb-create-cscope-database RET path-to-source RET
;;
;;   OR
;;
;;   M-x semanticdb-cscope-use-database RET path-to-cscope-file RET
;;
;; Once created, a semantic native databases will be used in it's stead.
;;
;; Use:
;;
;; M-x semanticdb-cscope-dump RET
;;
;;  to look at the contents of your created database.


(require 'semanticdb-search)
(eval-when-compile
  ;; For generic function searching.
  (require 'eieio)
  (require 'eieio-opt)
  )

;;; Code:
(defcustom semanticdb-cscope-program "cscope.sh"
  "*The cscope program to execute."
  :group 'semanticdb
  :type 'string)

(defun semanticdb-cscope-call (searchp &rest args)
  "Call the cscope program.
If SEARCHP is t, then the output is parsed as though for a search command.
A tag list is returned in this case.
If SEARCHP is nil, then the output is returned a string.
ARGS are arguments passed to the cscope script."
  (let ((outbuff (get-buffer-create " *SCcope*")))
    (save-excursion
      (set-buffer outbuff)
      (erase-buffer))
    (apply 'call-process semanticdb-cscope-program
	   nil outbuff nil
	   args)
    (save-excursion
      (set-buffer outbuff)
      (if (not searchp)
	  (buffer-string)
	;; Parse the search output of cscope, and return it.
	(let ((tags nil))



	  tags)))))
  

(defvar semanticdb-cscope-default-file-name "cscope.out"
  "The SCOPE file name used for system caches.")

;;;###autoload
(defun semanticdb-create-cscope-database (dir)
  "Create an cscope database for directory DIR.
The database file is stored in ~/.semanticdb, or whichever directory
is specified by `semanticdb-default-system-save-directory'."
  (interactive "DDirectory: ")
  (let* ((savein (semanticdb-cscope-file-for-directory dir))
	 (cscopefile (expand-file-name semanticdb-cscope-default-file-name
				       dir ))
	 )
    ;; @TODO - create the cscope file
    (semanticdb-cscope-call nil   );; ARGS HERE
    ;; Create a  DB save file for it
    (semanticdb-cscope-use-database cscopefile)
    ;; Load it.
    (semanticdb-cscope-load-helper dir)
    ))

(defun semanticdb-cscope-use-database (scopefile)
  "Create a cscope database for preexisting cscope file SCOPEFILE.
The database file reference is stored in ~/.semanticdb, or whichever
directory is specified by `semanticdb-default-system-save-directory'."
  (interactive "FCScope File: ")
  ;; Create the loader
  (let ((loadfile (semanticdb-cscope-create-loader-file scopefile)))
    ;; Load it
    (load loadfile))
  )

(defun semanticdb-cscope-create-loader-file (cscopefile)
  "Create a load file for the cscope databases file.
Argument CSCOPEFILE is the source of the cscope data."
  (let ((savein (semanticdb-cscope-file-for-directory
		 (file-name-directory cscopefile)))
	)
    ;; Create a short LOADER program for loading in this database.
    (let* ((lfn (concat savein "-load.el"))
	   (lf (find-file-noselect lfn)))
      (save-excursion
	(set-buffer lf)
	(erase-buffer)
	(insert "(semanticdb-cscope-load-helper \""
		cscopefile
		"\")\n")
	(save-buffer)
	(kill-buffer (current-buffer)))
      ;; Return the file we saved stuff into.
      lfn)))

;;;###autoload
(defun semanticdb-load-cscope-caches ()
  "Load all semanticdb controlled cscope caches."
  (interactive)
  (let ((f (directory-files semanticdb-default-system-save-directory
			    t (concat semanticdb-cscope-default-file-name "-load.el$") t)))
    (while f
      (load (car f) nil t)
      (setq f (cdr f)))
    ))

;;;###autoload
(defun semanticdb-cscope-load-helper (scopefile)
  "Create the semanticdb database object via cscope for scope DB.
If SCOPEFILE is found to be defunct, it won't load the DB, and will
warn instead."
  (if (file-exists-p scopefile)
      (semanticdb-create-database semanticdb-project-database-cscope
				  (file-name-directory scopefile))
    ;; Else, the source directory doesn't exist anymore
    (let* ((BF (semanticdb-cscope-file-for-directory scopefile))
	   (BFL (concat BF "-load.el"))
	   (BFLB (concat BF "-load.el~")))
      (save-window-excursion
	(with-output-to-temp-buffer "*FILES TO DELETE*"
	  (princ "The following cscope files are obsolete.\n\n")
	  (princ BF)
	  (princ "\n")
	  (princ BFL)
	  (princ "\n")
	  (when (file-exists-p BFLB)
	    (princ BFLB)
	    (princ "\n"))
	  )
	(when (y-or-n-p (format
			 "Warning: Obsolete cscope file for: %s\nDelete? "
			 scopefile))
	  (delete-file BF)
	  (when(file-exists-p BFL)
	      (delete-file BFL))
	  (when (file-exists-p BFLB)
	    (delete-file BFLB))
	  )))))

;;; SEMANTIC Database related Code
;;; Classes:
(defclass semanticdb-table-cscope (semanticdb-table)
  ((major-mode :initform c-mode)
   )
  "A table for returning search results from ebrowse.")

(defclass semanticdb-project-database-cscope (semanticdb-project-database)
  ((new-table-class :initform semanticdb-table-cscope
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   (system-include-p :initform nil
		     :initarg :system-include
		     :documentation
		     "Flag indicating this database represents a system include directory.")
   )
  "Semantic Database deriving tags using the cscope tool.
cscope is a C/C++/Java parser for use with `cscope' command line program.")

(defmethod semanticdb-needs-refresh-p ((table semanticdb-table-cscope))
  "CScope database do not need to be refreshed.

Our database should probably remember the timestamp/checksum of
the cscope file in question.

@TODO - they probably do need update.  How to do it?"
  nil
)

;;; CScope code
;;
;; These routines deal with part of the cscope interface.
(defun semanticdb-cscope-file-for-directory (dir)
  "Return the file name for DIR where the cscope.files file is.
This file should reside in `semanticdb-default-system-save-directory'."
  (let* ((semanticdb-default-save-directory
	  semanticdb-default-system-save-directory)
	 (CS (semanticdb-file-name-directory
	      'semanticdb-project-database-file
	      (concat (expand-file-name dir)
		      semanticdb-cscope-default-file-name)))
	 )
    CS))

;;; Methods for creating a database or tables
;;
(defmethod semanticdb-create-database :STATIC ((dbeC semanticdb-project-database-cscope)
					       directory)
  "Create a new semantic database for DIRECTORY based on cscope."
  ;; MAKE SURE THAT THE FILE LOADED DOESN'T ALREADY EXIST.
  (let ((dbs semanticdb-database-list)
	(found nil))
    (while (and (not found) dbs)
      (when (semanticdb-project-database-cscope-p (car dbs))
	(when (string= (oref (car dbs) reference-directory) directory)
	  (setq found (car dbs))))
      (setq dbs (cdr dbs)))
    ;;STATIC means DBE cant be used as object, only as a class
    (let* ((db nil)
	   (default-directory directory)
	   )
      (if found
	  (setq db found)
	(setq db (make-instance
		  dbeC
		  directory
		  ))
	(oset db reference-directory directory))

      ;; Once we recycle or make a new DB, refresh the
      ;; contents from the BROWSE file.
      (oset db tables nil)
      ;; Create one table that knows about all files in some dir or project?
      ;; @TODO
      (oset db tables (list (semanticdb-table-cscope "Base Table")))

      ;; Once our database is loaded, if we are a system DB, we
      ;; add ourselves to the include list for C.
      ;; @TODO - how to know if this is an include directory or not?
      (semantic-add-system-include directory 'c-mode)
      
      db)))

;;;
;;
(defmethod semanticdb-file-table ((obj semanticdb-project-database-cscope) filename)
  "From OBJ, return FILENAME's associated table object."
  (let ((tab (oref obj tables)))
    ;; @TODO - I think I can just return the car.
    ;;     BUT, we may need some optimization or something.
    (car tab)
    ))


;;;
;; Overload for converting the simple faux tag into something better.
;;
(defmethod semanticdb-normalize-tags ((obj semanticdb-table-cscope) tags)
  "Convert in cscope database OBJ convert TAGS into a complete tags."
  (let ((tagret nil))
    ;; SemanticDB will automatically create a regular database
    ;; on top of the file just loaded by ebrowse during the set
    ;; buffer.  Fetch that table, and use it's tag list to look
    ;; up the tag we just got, and thus turn it into a full semantic
    ;; tag.
    (while tags
      (let ((tag (car tags)))
	(save-excursion
	  (semanticdb-set-buffer obj)
	  (let ((ans nil))


	    ;;; TODO - recopy from ebrowse.

	    ;; Below copied from ebrowse.  Does cscope lie?

	    ;; Gee, it would be nice to do this, fbut ebrowse LIES.  Oi.
	    (if (semantic-tag-with-position-p tag)
		(progn
		  (goto-char (semantic-tag-start tag))
		  (let ((foundtag (semantic-current-tag)))
		    ;; Make sure the discovered tag is the same as what we started with.
		    (if (string= (semantic-tag-name tag)
				 (semantic-tag-name foundtag))
			;; We have a winner!
			(setq ans foundtag)))))
	    ;; Sometimes cscope lies.  Do a generic search
	    ;; to find it within this file.
	    (when (not ans)
	      ;; We might find multiple hits for this tag, and we have no way
	      ;; of knowing which one the user wanted.  Return the first one.
	      (setq ans (semantic-deep-find-tags-by-name
			 (semantic-tag-name tag)
			 (semantic-fetch-tags))))
	    (if (semantic-tag-p ans)
		(setq tagret (cons ans tagret))
	      (setq tagret (append ans tagret)))
	    ))
	(setq tags (cdr tags))))
    tagret))

;;; Search Overrides
;;
;; NOTE WHEN IMPLEMENTING: Be sure to add doc-string updates explaining
;; how your new search routines are implemented.
;;
(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-cscope) name &optional tags)
  "Find all tags named NAME in TABLE.
Return a list of tags."
  ;;(message "semanticdb-find-tags-by-name-method name -- %s" name)
  (if tags
      ;; If TAGS are passed in, then we don't need to do work here.
      (call-next-method)
    ;; Call cscope in a subprocess.
    ;; @TODO
    (semanticdb-cscope-call t   );; ARGS HERE
    )
  )

(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-cscope) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags
      (call-next-method)
    ;; Call cscope in a subprocess.
    ;; @TODO
    (semanticdb-cscope-call t   );; ARGS HERE
    ))

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-cscope) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags
      (call-next-method)
    ;; Call cscope in a subprocess.
    ;; @TODO
    (semanticdb-cscope-call t   );; ARGS HERE
    ))

(defmethod semanticdb-find-tags-by-class-method
  ((table semanticdb-table-cscope) class &optional tags)
  "In TABLE, find all occurances of tags of CLASS.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    ;; Call cscope in a subprocess.
    ;; @TODO
    (semanticdb-cscope-call t   );; ARGS HERE
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Deep Searches
;;
;; If your language does not have a `deep' concept, these can be left
;; alone, otherwise replace with implementations similar to those
;; above.
;;

(defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-cscope) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags t
Like `semanticdb-find-tags-by-name-method' for cscope."
  ;;(semanticdb-find-tags-by-name-method table name tags)
  (if tags (call-next-method)
    ;; Call cscope in a subprocess.
    ;; @TODO
    (semanticdb-cscope-call t   );; ARGS HERE
    ))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-cscope) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for cscope."
  ;;(semanticdb-find-tags-by-name-regexp-method table regex tags)
  (if tags (call-next-method)
    ;; Call cscope in a subprocess.
    ;; @TODO
    (semanticdb-cscope-call t   );; ARGS HERE
    ))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-cscope) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for cscope."
  ;;(semanticdb-find-tags-for-completion-method table prefix tags)
  (if tags (call-next-method)
    ;; Call cscope in a subprocess.
    ;; @TODO
    (semanticdb-cscope-call t   );; ARGS HERE
    ))

;;; Advanced Searches
;;
(defmethod semanticdb-find-tags-external-children-of-type-method
  ((table semanticdb-table-cscope) type &optional tags)
  "Find all nonterminals which are child elements of TYPE
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    ;; Call cscope in a subprocess.
    ;; @TODO
    (semanticdb-cscope-call t   );; ARGS HERE
    ))

;;; TESTING
;;
(defun semanticdb-cscope-dump ()
  "Find the first loaded cscope database, and dump out the contents."
  (interactive)
  (let ((db semanticdb-database-list)
	(ab nil))
    (while db
      (when (semanticdb-project-database-cscope-p (car db))
	(setq ab (data-debug-new-buffer "*CScope Database*"))
	(data-debug-insert-thing (car db) "]" "")
	(setq db nil))
      (setq db (cdr db)))))


(provide 'semanticdb-cscope)

;;; semanticdb-cscope.el ends here
