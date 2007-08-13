;;; semanticdb-find.el --- Searching through semantic databases.

;;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-find.el,v 1.40 2007/06/04 00:54:25 zappo Exp $

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
;; Databases of various forms can all be searched.
;; There are a few types of searches that can be done:
;;
;;   Basic Name Search:
;;    These searches scan a database table  collection for tags based
;;    on name.
;;
;;   Basic Attribute Search:
;;    These searches allow searching on specific attributes of tags,
;;    such as name, type, or other attribute.
;;
;;   Advanced Search:
;;    These are searches that were needed to accomplish some
;;    specialized tasks as discovered in utilities.  Advanced searches
;;    include matching methods defined outside some parent class.
;;
;;    The reason for advanced searches are so that external
;;    repositories such as the Emacs obarray, or java .class files can
;;    quickly answer these needed questions without dumping the entire
;;    symbol list into Emacs for additional refinement searches via
;;    regular semanticdb search.
;;
;; How databases are decided upon is another important aspect of a
;; database search.  When it comes to searching for a name, there are
;; these types of searches:
;;
;;   Basic Search:
;;    Basic search means that tags looking for a given name start
;;    with a specific search path.  Names are sought on that path
;;    until it is empty or items on the path can no longer be found.
;;    Use `semanticdb-dump-all-table-summary' to test this list.
;;    Use `semanticdb-find-throttle-custom-list' to refine this list.
;;
;;   Deep Search:
;;    A deep search will search more than just the global namespace.
;;    It will recurse into tags that contain more tags, and search
;;    those too.
;;
;;   Brute Search:
;;    Brute search means that all tables in all databases in a given
;;    project are searched.  Brute searches are the search style as
;;    written for semantic version 1.x.
;;
;; How does the search path work?
;;
;;  A basic search starts with three parameters:
;;
;;     (FINDME &optional PATH FIND-FILE-MATCH)
;;
;;  FINDME is key to be searched for dependent on the type of search.
;;  PATH is an indicator of which tables are to be searched.
;;  FIND-FILE-MATCH indicates that any time a match is found, the
;;  file associated with the tag should be read into a file.
;;
;;  The PATH argument is then the most interesting argument.  It can
;;  have these values:
;;
;;    nil - Take the current buffer, and use it's include list
;;    buffer - Use that buffer's include list.
;;    filename - Use that file's include list.  If the file is not
;;        in a buffer, see of there is a semanticdb table for it.  If
;;        not, read that file into a buffer.
;;    tag - Get that tag's buffer of file file.  See above.
;;    table - Search that table, and it's include list.
;;
;; Search Results:
;;
;;   Semanticdb returns the results in a specific format.  There are a
;;   series of routines for using those results, and results can be
;;   passed in as a search-path for refinement searches with
;;   semanticdb.  Apropos for semanticdb.*find-result for more.
;;
;; Application:
;;
;; Here are applications where different searches are needed which
;; exist as of semantic 1.4.x
;;
;; eldoc - popup help
;;   => Requires basic search using default path.  (Header files ok)
;; tag jump - jump to a named tag
;;   => Requires a brute search useing whole project.  (Source files only)
;; completion - Completing symbol names in a smart way
;;   => Basic search (headers ok)
;; type analysis - finding type definitions for variables & fcns
;;   => Basic search (headers ok)
;; Class browser - organize types into some structure
;;   => Brute search, or custom navigation.

;; TODO:
;;  During a search, load any unloaded DB files based on paths in the
;;  current project.

(require 'semanticdb)
(eval-when-compile
  (require 'eieio)
  )

;;; Code:
;;;###autoload
(defvar semanticdb-find-throttle-custom-list
  '(repeat (radio (const 'local)
		  (const 'project)
		  (const 'unloaded)
		  (const 'system)
		  (const 'recursive)
		  (const 'omniscience)))
  "Customization values for semanticdb find throttle.
See `semanticdb-find-throttle' for details.")

;;;###autoload
(defcustom semanticdb-find-default-throttle '(project unloaded system recursive)
  "The default throttle for `semanticdb-find' routines.
The throttle controls how detailed the list of database
tables is for a symbol lookup.  The value is a list with
the following keys:
  `file'       - The file the search is being performed from.
                 This option is here for completeness only, and
                 is assumed to always be on.
  `local'      - Tables from the same local directory are included.
                 This includes files directly referenced by a file name
                 which might be in a different directory.
  `project'    - Tables from the same local project are included
                 If `project' is specified, then `local' is assumed.
  `unloaded'   - If a table is not in memory, load it.  If it is not cached
                 on disk either, get the source, parse it, and create
                 the table.
  `system'     - Tables from system databases.  These are specifically
                 tables from system header files, or language equivalent.
  `recursive'  - For include based searches, includes tables referenced
                 by included files.
  `omniscience' - Included system databases which are omniscience, or
                 somehow know everything.  Omniscience databases are found
                 in `semanticdb-project-system-databases'.
                 The Emacs Lisp system DB is an omniscience database."
  :group 'semanticdb
  :type semanticdb-find-throttle-custom-list)

(defun semanticdb-find-throttle-active-p (access-type)
  "Non-nil if ACCESS-TYPE is an active throttle type."
  (or (memq access-type semanticdb-find-default-throttle)
      (eq access-type 'file)
      (and (eq access-type 'local)
	   (memq 'project semanticdb-find-default-throttle))
      ))

;;; Path Translations
;;
;;; OVERLOAD Functions
;;
;; These routines needed to be overloaded by specific language modes.
;; They are needed for translating an INCLUDE tag into a semanticdb
;; TABLE object.
(define-overload semanticdb-find-translate-path (path brutish)
  "Translate PATH into a list of semantic tables.
Path translation involves identifying the PATH input argument
in one of the following ways:
  nil - Take the current buffer, and use it's include list
  buffer - Use that buffer's include list.
  filename - Use that file's include list.  If the file is not
      in a buffer, see of there is a semanticdb table for it.  If
      not, read that file into a buffer.
  tag - Get that tag's buffer of file file.  See above.
  table - Search that table, and it's include list.
  find result - Search the results of a previous find.

In addition, once the base path is found, there is the possibility of
each added table adding yet more tables to the path, so this routine
can return a lengthy list.

If argument BRUTISH is non-nil, then instead of using the include
list, use all tables found in the parent project of the table
identified by translating PATH.  Such searches use brute force to
scan every available table.

The return value is a list of objects of type `semanticdb-table' or
it's children.  In the case of passing in a find result, the result
is returned unchanged.

This routine uses `semanticdb-find-table-for-include' to translate
specific include tags into a semanticdb table."
  )

;;;###autoload
(defun semanticdb-find-translate-path-default (path brutish)
  "Translate PATH into a list of semantic tables.
If BRUTISH is non nil, return all tables associated with PATH.
Default action as described in `semanticdb-find-translate-path'."
  (if (semanticdb-find-results-p path)
      ;; Perform the search over these results.
      nil
    (if brutish
	(semanticdb-find-translate-path-brutish-default path)
      (semanticdb-find-translate-path-includes-default path))))

(defun semanticdb-find-translate-path-brutish-default (path)
  "Translate PATH into a list of semantic tables.
Default action as described in `semanticdb-find-translate-path'."
  (let ((basedb
	 (cond ((null path) semanticdb-current-database)
	       ((semanticdb-table-p path) (oref path parent-db))
	       (t (let ((tt (semantic-something-to-tag-table path)))
		    (save-excursion
		      (set-buffer (semantic-tag-buffer (car tt)))
		      semanticdb-current-database))))))
    (apply
     #'append
     (mapcar
      (lambda (db)
	(let ((tabs (semanticdb-get-database-tables db))
	      (ret nil))
	  ;; Only return tables of the same language (major-mode)
	  ;; as the current search environment.
	  (while tabs
	    (if (semanticdb-equivalent-mode-for-search (car tabs)
						       (current-buffer))
		(setq ret (cons (car tabs) ret)))
	    (setq tabs (cdr tabs)))
	  ret))
      ;; FIXME:
      ;; This should scan the current project directory list for all
      ;; semanticdb files, perhaps handling proxies for them.
      (semanticdb-current-database-list
       (if basedb (oref basedb reference-directory)
	 default-directory))))
    ))

(defun semanticdb-find-translate-path-includes-default (path)
  "Translate PATH into a list of semantic tables.
Default action as described in `semanticdb-find-translate-path'."
  (let ((includetags
	 (cond ((null path)
		(semantic-find-tags-included (current-buffer)))
	       ((semanticdb-table-p path)
		(semantic-find-tags-included (semanticdb-get-tags path)))
	       (t (semantic-find-tags-included path))))
	(matchedtables (list semanticdb-current-table))
	nexttable)
    ;; Loop over all include tags adding to matchedtables
    (while includetags
      (semantic-throw-on-input 'semantic-find-translate-path-includes-default)
      (setq nexttable (semanticdb-find-table-for-include (car includetags)))
      ;; (message "Scanning %s" (semantic-tag-name (car includetags)))
      (when (and nexttable
		 (not (memq nexttable matchedtables))
		 (semanticdb-equivalent-mode-for-search nexttable
							(current-buffer))
		 )
	;; Add to list of tables
	(push nexttable matchedtables)
	;; Queue new includes to list
	(if (semanticdb-find-throttle-active-p 'recursive)
	    (let ((newtags
		   (cond
		    ((semanticdb-table-p nexttable)
		     ;; Use the method directly, or we will recurse
		     ;; into ourselves here.
		     (semanticdb-find-tags-by-class-method
		      nexttable 'include))
		    (t
		     (semantic-find-tags-included
		      (semanticdb-get-tags nexttable))))))
	      (setq includetags (append includetags newtags)))))
      (setq includetags (cdr includetags)))
    ;; Find all the omniscient databases for this major mode, and
    ;; add them if needed
    (when (and (semanticdb-find-throttle-active-p 'omniscience)
	       semanticdb-search-system-databases)
      ;; We can append any mode-specific omniscience databases into
      ;; our search list here.
      (let ((systemdb semanticdb-project-system-databases)
	    (ans nil))
	(while systemdb
	  (setq ans (semanticdb-file-table
		     (car systemdb)
		     ;; I would expect most omniscient to return the same
		     ;; thing reguardless of filename, but we may have
		     ;; one that can return a table of all things the
		     ;; current file needs.
		     (buffer-file-name (current-buffer))))
	  (when (not (memq ans matchedtables))
	    (setq matchedtables (cons ans matchedtables)))
	  (setq systemdb (cdr systemdb))))
      )
    (nreverse matchedtables)))

(define-overload semanticdb-find-load-unloaded (filename)
  "Create a database table for FILENAME if it hasn't been parsed yet.
Assumes that FILENAME exists as a source file.
Assumes that a preexisting table does not exist, even if it
isn't in memory yet."
  (when (semanticdb-find-throttle-active-p 'unloaded)
    (:override)))

(defun semanticdb-find-load-unloaded-default (filename)
  "Load an unloaded file in FILENAME using the default semanticdb loader."
  (semanticdb-file-table-object filename))

;;;###autoload
(define-overload semanticdb-find-table-for-include (includetag &optional table)
  "For a single INCLUDETAG found in TABLE, find a `semanticdb-table' object
INCLUDETAG is a semantic TAG of class 'include.
TABLE as defined by `semantic-something-to-tag-table' to identify
where the tag came from.  TABLE is optional if INCLUDETAG has an
overlay of :filename attribute."
  )

(defun semanticdb-find-table-for-include-default (includetag &optional table)
  "Default implementation of `semanticdb-find-table-for-include'.
Uses `semanticdb-current-database-list' as the search path.
INCLUDETAG and TABLE are documented in `semanticdb-find-table-for-include'.
Included databases are filtered based on `semanticdb-find-default-throttle'."
  (if (not (eq (semantic-tag-class includetag) 'include))
      (signal 'wrong-type-argument (list includetag 'include)))

  ;; Note, some languages (like Emacs or Java) use include tag names
  ;; that don't represent files!  We want to have file names.
  (let ((name (semantic-tag-include-filename includetag))
	(roots (semanticdb-current-database-list))
	(tmp nil)
	(ans nil))
    (cond
     ;; Relative path name
     ;;
     ((and (file-exists-p (expand-file-name name))
	   (semanticdb-find-throttle-active-p 'local))

      (setq ans (semanticdb-file-table-object
		 name
		 (not (semanticdb-find-throttle-active-p 'unloaded))))
      )
     ;; On the path somewhere
     ;; NOTES: Separate system includes from local includes.
     ;;        Use only system databases for system includes.
     ((and (setq tmp (semantic-dependency-tag-file includetag))
	   (semanticdb-find-throttle-active-p 'system))
      (let ((db (semanticdb-directory-loaded-p (file-name-directory tmp))))
	(if db
	    ;; We have a database, but perhaps not a table?
	    (setq ans (semanticdb-file-table db tmp))
	  ;; ELSE: we could load a cache if it isn't already loaded
	  ;; based on another throttle value.
	  )
	(if ans
	    ;; We are A-ok!
	    nil
	  ;; The file is not in memory!
	  ;; Should we force it to be loaded in?
	  (setq ans (semanticdb-find-load-unloaded tmp))
	  )))

     ;; Somewhere in our project hierarchy
     ;; Remember: Roots includes system databases which can create
     ;; specialized tables we can search.
     ((semanticdb-find-throttle-active-p 'project)

      ;; @TODO - This needs the same treatment as 'local'
      ;;         above with the various phases.  We should also
      ;;         not use the existing DBs, but instead recurse
      ;;         through our current project.

      (while (and (not ans) roots)
	(let* ((ref (if (slot-boundp (car roots) 'reference-directory)
			(oref (car roots) reference-directory)))
	       (fname (cond ((null ref) nil)
			    ((file-exists-p (expand-file-name name ref))
			     (expand-file-name name ref))
			    ((file-exists-p (expand-file-name (file-name-nondirectory name) ref))
			     (expand-file-name (file-name-nondirectory name) ref)))))
	  (when (and ref fname)
	    ;; There is an actual file.  Grab it.
	    (setq ans (semanticdb-file-table-object fname)))

	  ;; ELSE
	  ;;
	  ;; NOTE: We used to look up omniscient databases here, but that
	  ;; is now handled one layer up.
	  ;;
	  ;; Missing: a database that knows where missing files are.  Hmm.
	  ;; perhaps I need an override function for that?

	  )

	(setq roots (cdr roots))))
     )
    ans))


;;; Perform interactive tests on the path/search mechanisms.
;;
;;;###autoload
(defun semanticdb-find-test-translate-path (&optional arg)
  "Call and output results of `semanticdb-find-translate-path'.
With ARG non-nil, specify a BRUTISH translation.
See `semanticdb-find-default-throttle' and `semanticdb-project-roots'
for details on how this list is derived."
  (interactive "P")
  (require 'semantic-adebug)
  (let ((start (current-time))
	(p (semanticdb-find-translate-path nil arg))
	(end (current-time))
	(ab (semantic-adebug-new-buffer "*SEMANTICDB FTP ADEBUG*"))
	)
    (message "Search of tags took %.2f seconds."
	     (semantic-elapsed-time start end))
    
    (semantic-adebug-insert-stuff-list p "*")))

;;    ;; Output the result
;;    (message "%d paths found." (length p))
;;    (with-output-to-temp-buffer "*Translated Path*"
;;      (while p
;;	(condition-case nil
;;	    (progn
;;	      (princ (semanticdb-full-filename (car p)))
;;	      (princ ": ")
;;	      (prin1 (condition-case nil
;;			 (length (oref (car p) tags))
;;		       (error "--")))
;;	      (princ " tags")
;;	      (let ((parent (oref (car p) parent-db)))
;;		(when parent
;;		  (princ " : ")
;;		  (princ (object-name parent))))
;;	      )
;;	  (no-method-definition
;;	   (princ (semanticdb-printable-name (car p)))))
;;	(princ "\n")
;;	(setq p (cdr p)))
;;      )
;;    ))


;;; FIND results and edebug
;;
(eval-after-load "cedet-edebug"
  '(progn
     (cedet-edebug-add-print-override
      '(semanticdb-find-results-p object)
      '(semanticdb-find-result-prin1-to-string object) )
     ))



;;; API Functions
;;
;; Once you have a search result, use these routines to operate
;; on the search results at a higher level

;;;###autoload
(defun semanticdb-strip-find-results (results &optional find-file-match)
  "Strip a semanticdb search RESULTS to exclude objects.
This makes it appear more like the results of a `semantic-find-' call.
Optional FIND-FILE-MATCH loads all files associated with RESULTS
into buffers.  This has the side effect of enabling `semantic-tag-buffer' to
return a value."
  (if find-file-match
      ;; Load all files associated with RESULTS.
      (let ((tmp results)
	    (output nil))
	(while tmp
	  (let ((tab (car (car tmp)))
		(tags (cdr (car tmp))))
	    (semanticdb-get-buffer tab)
	    (setq output (append output
				 (semanticdb-normalize-tags tab tags))))
	  (setq tmp (cdr tmp)))
	output)
    (apply #'append (mapcar #'cdr results))))

;;;###autoload
(defun semanticdb-find-results-p (resultp)
  "Non-nil if RESULTP is in the form of a semanticdb search result.
This query only really tests the first entry in the list that is RESULTP,
but should be good enough for debugging assertions."
  (and (listp resultp)
       (listp (car resultp))
       (semanticdb-abstract-table-child-p (car (car resultp)))
       (or (semantic-tag-p (car (cdr (car resultp))))
	   (null (car (cdr (car resultp)))))))

(defun semanticdb-find-result-prin1-to-string (result)
  "Presuming RESULT satisfies `semanticdb-find-results-p', provide a short PRIN1 output."
  (concat "#<FIND RESULT "
	  (mapconcat (lambda (a)
		       (concat "(" (object-name (car a) ) " . "
			       "#<TAG LIST " (number-to-string (length (cdr a))) ">)"))
		     result
		     " ")
	  ">"))

;;;###autoload
(defun semanticdb-find-result-with-nil-p (resultp)
  "Non-nil of RESULTP is in the form of a semanticdb search result.
nil is a valid value where a TABLE usually is, but only if the TAG
results include overlays.
This query only really tests the first entry in the list that is RESULTP,
but should be good enough for debugging assertions."
  (and (listp resultp)
       (listp (car resultp))
       (let ((tag-to-test (car-safe (cdr (car resultp)))))
	 (or (and (semanticdb-abstract-table-child-p (car (car resultp)))
		  (or (semantic-tag-p tag-to-test)
		      (null tag-to-test)))
	     (and (null (car (car resultp)))
		  (or (semantic-tag-with-position-p tag-to-test)
		      (null tag-to-test))))
	 )))

(defun semanticdb-find-result-length (result)
  "Number of tags found in RESULT."
  (let ((count 0))
    (mapc (lambda (onetable)
	    (setq count (+ count (1- (length onetable)))))
	  result)
    count))

;;;###autoload
(defun semanticdb-find-result-nth (result n)
  "In RESULT, return the Nth search result.
This is a 0 based search result, with the first match being element 0.

The returned value is a cons cell: (TAG . TABLE) where TAG
is the tag at the Nth position.  TABLE is the semanticdb table where
the TAG was found.  Sometimes TABLE can be nil."
  (let ((ans nil)
	(anstable nil))
    ;; Loop over each single table hit.
    (while (and (not ans) result)
      ;; For each table result, get local length, and modify
      ;; N to be that much less.
      (let ((ll (length (cdr (car result))))) ;; local length
	(if (> ll n)
	    ;; We have a local match.
	    (setq ans (nth n (cdr (car result)))
		  anstable (car (car result)))
	  ;; More to go.  Decrement N.
	  (setq n (- n ll))))
      ;; Keep moving.
      (setq result (cdr result)))
    (cons ans anstable)))

(defun semanticdb-find-result-test (result)
  "Test RESULT by accessing all the tags in the list."
  (if (not (semanticdb-find-results-p result))
      (error "Does not pass `semanticdb-find-results-p.\n"))
  (let ((len (semanticdb-find-result-length result))
	(i 0))
    (while (< i len)
      (let ((tag (semanticdb-find-result-nth result i)))
	(if (not (semantic-tag-p (car tag)))
	    (error "%d entry is not a tag" i)))
      (setq i (1+ i)))))

;;;###autoload
(defun semanticdb-find-result-nth-in-buffer (result n)
  "In RESULT, return the Nth search result.
Like `semanticdb-find-result-nth', except that only the TAG
is returned, and the buffer it is found it will be made current.
If the result tag has no position information, the originating buffer
is still made current."
  (let* ((ret (semanticdb-find-result-nth result n))
	 (ans (car ret))
	 (anstable (cdr ret)))
    ;; If we have a hit, double-check the find-file
    ;; entry.  If the file must be loaded, then gat that table's
    ;; source file into a buffer.
    (if anstable (semanticdb-set-buffer anstable))
    ;; Return the tag.
    ans))

;;; Search Logging
;;
;; Basic logging to see what the search routines are doing.
(defvar semanticdb-find-log-flag nil
  "Non-nil means log the process of searches.")

(defvar semanticdb-find-log-buffer-name "*SemanticDB Find Log*"
  "The name of the logging buffer.")

(defun semanticdb-find-toggle-logging ()
  "Toggle sematnicdb logging."
  (interactive)
  (setq semanticdb-find-log-flag (null semanticdb-find-log-flag))
  (message "Semanticdb find logging is %sabled"
	   (if semanticdb-find-log-flag "en" "dis")))

(defun semanticdb-reset-log ()
  "Reset the log buffer."
  (interactive)
  (when semanticdb-find-log-flag
    (save-excursion
      (set-buffer (get-buffer-create semanticdb-find-log-buffer-name))
      (erase-buffer)
      )))

(defun semanticdb-find-log-move-to-end ()
  "Move to the end of the semantic log."
  (let ((cb (current-buffer))
	(cw (selected-window)))
    (unwind-protect
	(progn
	  (set-buffer semanticdb-find-log-buffer-name)
	  (if (get-buffer-window (current-buffer) 'visible)
	      (select-window (get-buffer-window (current-buffer) 'visible)))
	  (goto-char (point-max)))
      (if cw (select-window cw))
      (set-buffer cb))))

(defun semanticdb-find-log-new-search (forwhat)
  "Start a new search FORWHAT."
  (when semanticdb-find-log-flag
    (save-excursion
      (set-buffer (get-buffer-create semanticdb-find-log-buffer-name))
      (insert (format "New Search: %S\n" forwhat))
      )
    (semanticdb-find-log-move-to-end)))

(defun semanticdb-find-log-activity (table result)
  "Log that TABLE has been searched and RESULT was found."
  (when semanticdb-find-log-flag
    (save-excursion
      (set-buffer semanticdb-find-log-buffer-name)
      (insert "Table: " (object-print table)
	      " Result: " (int-to-string (length result)) " tags"
	      "\n")
      )
    (semanticdb-find-log-move-to-end)))

;;; Semanticdb find API functions
;;
;; These are the routines actually used to perform searches.
;;
;;;###autoload
(defun semanticdb-find-tags-collector (function &optional path find-file-match
						brutish)
  "Search for all tags returned by FUNCTION over PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer.
If optional argument BRUTISH is non-nil, then ignore include statements,
and search all tables in this project tree."
  (let (found match)
    (save-excursion
      ;; If path is a buffer, set ourselves up in that buffer
      ;; so that the override methods work correctly.
      (when (bufferp path) (set-buffer path))
      (if (semanticdb-find-results-p path)
	  ;; When we get find results, loop over that.
	  (dolist (tableandtags path)
	    (semantic-throw-on-input 'semantic-find-translate-path)
	    ;; If FIND-FILE-MATCH is non-nil, skip tables of class
	    ;; `semanticdb-search-results-table', since those are system
	    ;; databases and not associated with a file.
	    (unless (and find-file-match
			 (obj-of-class-p
			  (car tableandtags) semanticdb-search-results-table))
	      (when (setq match (funcall function
					 (car tableandtags) (cdr tableandtags)))
		(when find-file-match
		  (save-excursion (semanticdb-set-buffer (car tableandtags))))
		(push (cons (car tableandtags) match) found)))
    	    )
	;; Only log searches across data bases.
	(semanticdb-find-log-new-search nil)
	;; If we get something else, scan the list of tables resulting
	;; from translating it into a list of objects.
	(dolist (table (semanticdb-find-translate-path path brutish))
	  (semantic-throw-on-input 'semantic-find-translate-path)
	  ;; If FIND-FILE-MATCH is non-nil, skip tables of class
	  ;; `semanticdb-search-results-table', since those are system
	  ;; databases and not associated with a file.
	  (unless (and find-file-match
		       (obj-of-class-p table semanticdb-search-results-table))
	    (when (and table (setq match (funcall function table nil)))
	      (semanticdb-find-log-activity table match)
	      (when find-file-match
		(save-excursion (semanticdb-set-buffer table)))
	      (push (cons table match) found))))))
    ;; At this point, FOUND has had items pushed onto it.
    ;; This means items are being returned in REVERSE order
    ;; of the tables searched, so if you just get th CAR, then
    ;; too-bad, you may have some system-tag that has no
    ;; buffer associated with it.

    ;; It must be reversed.
    (nreverse found)))

;;;###autoload
(defun semanticdb-find-tags-by-name (name &optional path find-file-match)
  "Search for all tags matching NAME on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-by-name-method table name tags))
   path find-file-match))

;;;###autoload
(defun semanticdb-find-tags-by-name-regexp (regexp &optional path find-file-match)
  "Search for all tags matching REGEXP on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-by-name-regexp-method table regexp tags))
   path find-file-match))

;;;###autoload
(defun semanticdb-find-tags-for-completion (prefix &optional path find-file-match)
  "Search for all tags matching PREFIX on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-for-completion-method table prefix tags))
   path find-file-match))

;;;###autoload
(defun semanticdb-find-tags-by-class (class &optional path find-file-match)
  "Search for all tags of CLASS on PATH.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-by-class-method table class tags))
   path find-file-match))

;;; Deep Searches
;;
;;;###autoload
(defun semanticdb-deep-find-tags-by-name (name &optional path find-file-match)
  "Search for all tags matching NAME on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-find-tags-by-name-method table name tags))
   path find-file-match))

;;;###autoload
(defun semanticdb-deep-find-tags-by-name-regexp (regexp &optional path find-file-match)
  "Search for all tags matching REGEXP on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-find-tags-by-name-regexp-method table regexp tags))
   path find-file-match))

;;;###autoload
(defun semanticdb-deep-find-tags-for-completion (prefix &optional path find-file-match)
  "Search for all tags matching PREFIX on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-find-tags-for-completion-method table prefix tags))
   path find-file-match))

;;; Brutish Search Routines
;;
;;;###autoload
(defun semanticdb-brute-deep-find-tags-by-name (name &optional path find-file-match)
  "Search for all tags matching NAME on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a matchi is found, the file
associated wit that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-find-tags-by-name-method table name tags))
   path find-file-match t))

;;;###autoload
(defun semanticdb-brute-deep-find-tags-for-completion (prefix &optional path find-file-match)
  "Search for all tags matching PREFIX on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a matchi is found, the file
associated wit that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-deep-find-tags-for-completion-method table prefix tags))
   path find-file-match t))

;;;###autoload
(defun semanticdb-brute-find-tags-by-class (class &optional path find-file-match)
  "Search for all tags of CLASS on PATH.
See `semanticdb-find-translate-path' for details on PATH.
The argument BRUTISH will be set so that searching includes all tables
in the current project.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-by-class-method table class tags))
   path find-file-match t))

;;; Specialty Search Routines
;;
;;;###autoload
(defun semanticdb-find-tags-external-children-of-type
  (type &optional path find-file-match)
  "Search for all tags defined outside of TYPE w/ TYPE as a parent.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (semanticdb-find-tags-external-children-of-type-method table type tags))
   path find-file-match))

;;; METHODS
;;
;; Default methods for semanticdb database and table objects.
;; Override these with system databases to as new types of back ends.

;;; Top level Searches
(defmethod semanticdb-find-tags-by-name-method ((table semanticdb-table) name &optional tags)
  "In TABLE, find all occurances of tags with NAME.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (semantic-find-tags-by-name name (or tags (semanticdb-get-tags table))))

(defmethod semanticdb-find-tags-by-name-regexp-method ((table semanticdb-table) regexp &optional tags)
  "In TABLE, find all occurances of tags matching REGEXP.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (semantic-find-tags-by-name-regexp regexp (or tags (semanticdb-get-tags table))))

(defmethod semanticdb-find-tags-for-completion-method ((table semanticdb-table) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (semantic-find-tags-for-completion prefix (or tags (semanticdb-get-tags table))))

(defmethod semanticdb-find-tags-by-class-method ((table semanticdb-table) class &optional tags)
  "In TABLE, find all occurances of tags of CLASS.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (semantic-find-tags-by-class class (or tags (semanticdb-get-tags table))))

(defmethod semanticdb-find-tags-external-children-of-type-method ((table semanticdb-table) parent &optional tags)
   "In TABLE, find all occurances of tags whose TYPE is PARENT.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
   (semantic-find-tags-external-children-of-type parent (or tags (semanticdb-get-tags table))))

;;; Deep Searches
(defmethod semanticdb-deep-find-tags-by-name-method ((table semanticdb-table) name &optional tags)
  "In TABLE, find all occurances of tags with NAME.
Search in all tags in TABLE, and all components of top level tags in
TABLE.
Optional argument TAGS is a list of tags to search.
Return a table of all matching tags."
  (semantic-find-tags-by-name name (semantic-flatten-tags-table (or tags (semanticdb-get-tags table)))))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method ((table semanticdb-table) regexp &optional tags)
  "In TABLE, find all occurances of tags matching REGEXP.
Search in all tags in TABLE, and all components of top level tags in
TABLE.
Optional argument TAGS is a list of tags to search.
Return a table of all matching tags."
  (semantic-find-tags-by-name-regexp regexp (semantic-flatten-tags-table (or tags (semanticdb-get-tags table)))))

(defmethod semanticdb-deep-find-tags-for-completion-method ((table semanticdb-table) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Search in all tags in TABLE, and all components of top level tags in
TABLE.
Optional argument TAGS is a list of tags to search.
Return a table of all matching tags."
  (semantic-find-tags-for-completion prefix (semantic-flatten-tags-table (or tags (semanticdb-get-tags table)))))

(provide 'semanticdb-find)

;;; semanticdb-find.el ends here
