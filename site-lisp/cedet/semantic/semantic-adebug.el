;;; semantic-adebug.el --- Semantic Application Debugger

;; Copyright (C) 2007, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-adebug.el,v 1.26 2009/01/20 02:27:13 zappo Exp $

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
;; Semantic datastructure debugger for semantic applications.
;; Uses data-debug for core implementation.
;;
;; Goals:
;;
;; Inspect all known details of a TAG in a buffer.
;; 
;; Analyze the list of active semantic databases, and the tags therin.
;;
;; Allow interactive navigation of the analysis process, tags, etc.

(require 'data-debug)
(require 'eieio-datadebug)
(require 'semantic-analyze)

;;; Code:

;;; SEMANTIC TAG STUFF
;;
(defun data-debug-insert-tag-parts (tag prefix &optional parent)
  "Insert all the parts of TAG.
PREFIX specifies what to insert at the start of each line.
PARENT specifires any parent tag."
  (data-debug-insert-thing (semantic-tag-name tag)
			   prefix
			   "Name: "
			   parent)
  (insert prefix "Class: '" (format "%S" (semantic-tag-class tag)) "\n")
  (when (semantic-tag-with-position-p tag)
    (let ((ol (semantic-tag-overlay tag))
	  (file (semantic-tag-file-name tag))
	  (start (semantic-tag-start tag))
	  (end (semantic-tag-end tag))
	  )
      (insert prefix "Position: "
	      (if (and (numberp start) (numberp end))
		  (format "%d -> %d in " start end)
		"")
	      (if file (file-name-nondirectory file) "unknown-file")
	      (if (semantic-overlay-p ol)
		  " <live tag>"
		"")
	      "\n")
      (data-debug-insert-thing ol prefix
			       "Position Data: "
			       parent)
      ))
  (let ((attrprefix (concat (make-string (length prefix) ? ) "# ")))
    (insert prefix "Attributes:\n")
    (data-debug-insert-property-list
     (semantic-tag-attributes tag) attrprefix tag)
    (insert prefix "Properties:\n")
    (data-debug-insert-property-list
     (semantic-tag-properties tag) attrprefix tag)
    )

  )

;;;###autoload
(defun data-debug-insert-tag-parts-from-point (point)
  "Call `data-debug-insert-tag-parts' based on text properties at POINT."
  (let ((tag (get-text-property point 'ddebug))
	(parent (get-text-property point 'ddebug-parent))
	(indent (get-text-property point 'ddebug-indent))
	start
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-tag-parts tag
				 (concat (make-string indent ? )
					 "| ")
				 parent)
    (goto-char start)
    ))

;;;###autoload
(defun data-debug-insert-tag (tag prefix prebuttontext &optional parent)
  "Insert TAG into the current buffer at the current point.
PREFIX specifies text to insert in front of TAG.
PREBUTTONTEXT is text appearing btewen the prefix and TAG.
Optional PARENT is the parent tag containing TAG.
Add text properties needed to allow tag expansion later."
  (let ((start (point))
	(end nil)
	(str (semantic-format-tag-uml-abbreviate tag parent t))
	(tip (semantic-format-tag-prototype tag parent t))
	)
    (insert prefix prebuttontext str "\n")
    (setq end (point))
    (put-text-property start end 'ddebug tag)
    (put-text-property start end 'ddebug-parent parent)
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-tag-parts-from-point)
    
    ))

;;; TAG LISTS
;;
;;;###autoload
(defun data-debug-insert-tag-list (taglist prefix &optional parent)
  "Insert the tag list TAGLIST with PREFIX.
Optional argument PARENT specifies the part of TAGLIST."
  (condition-case nil
      (while taglist
	(cond ((and (consp taglist) (semantic-tag-p (car taglist)))
	       (data-debug-insert-tag (car taglist) prefix "" parent))
	      ((consp taglist)
	       (data-debug-insert-thing (car taglist) prefix "" parent))
	      (t (data-debug-insert-thing taglist prefix "" parent)))
	(setq taglist (cdr taglist)))
    (error nil)))

(defun data-debug-insert-taglist-from-point (point)
  "Insert the taglist found at the taglist button at POINT."
  (let ((taglist (get-text-property point 'ddebug))
	(parent (get-text-property point 'ddebug-parent))
	(indent (get-text-property point 'ddebug-indent))
	start
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-tag-list taglist
				(concat (make-string indent ? )
					"* ")
				parent)
    (goto-char start)

    ))

;;;###autoload
(defun data-debug-insert-tag-list-button (taglist prefix prebuttontext &optional parent)
  "Insert a single summary of a TAGLIST.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between PREFIX and the taglist button.
PARENT is the tag that represents the parent of all the tags."
  (let ((start (point))
	(end nil)
	(str (format "#<TAG LIST: %d entries>" (safe-length taglist)))
	(tip nil))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-function-name-face)
    (put-text-property start end 'ddebug taglist)
    (put-text-property start end 'ddebug-parent parent)
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-taglist-from-point)
    (insert "\n")
    ))

;;; SEMANTICDB FIND RESULTS
;;
;;;###autoload
(defun data-debug-insert-find-results (findres prefix)
  "Insert the find results FINDRES with PREFIX."
  ;; ( (DBOBJ TAG TAG TAG) (DBOBJ TAG TAG TAG) ... )
  (let ((cnt 1))
    (while findres
      (let* ((dbhit (car findres))
	     (db (car dbhit))
	     (tags (cdr dbhit)))
	(data-debug-insert-thing db prefix (format "DB %d: " cnt))
	(data-debug-insert-thing tags prefix (format "HITS %d: " cnt))
	)
      (setq findres (cdr findres)
	    cnt (1+ cnt)))))

(defun data-debug-insert-find-results-from-point (point)
  "Insert the find results found at the find results button at POINT."
  (let ((findres (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-find-results findres
				    (concat (make-string indent ? )
					    "!* ")
				    )
    (goto-char start)
    ))

;;;###autoload
(defun data-debug-insert-find-results-button (findres prefix prebuttontext)
  "Insert a single summary of a find results FINDRES.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the find results button."
  (let ((start (point))
	(end nil)
	(str (semanticdb-find-result-prin1-to-string findres))
	(tip nil))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-function-name-face)
    (put-text-property start end 'ddebug findres)
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-find-results-from-point)
    (insert "\n")
    ))

;;;###autoload
(defun data-debug-insert-db-and-tag-button (dbtag prefix prebuttontext)
  "Insert a single summary of short list DBTAG of format (DB . TAG).
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the find results button."
  (let ((start (point))
	(end nil)
	(str (concat "(#<db/tag "
		     (object-name-string (car dbtag))
		     " / "
		     (semantic-format-tag-name (cdr dbtag) nil t)
		     ")"))
	(tip nil))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-function-name-face)
    (put-text-property start end 'ddebug dbtag)
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-db-and-tag-from-point)
    (insert "\n")
    ))

(defun data-debug-insert-db-and-tag-from-point (point)
  "Insert the find results found at the find results button at POINT."
  (let ((dbtag (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-thing (car dbtag) (make-string indent ? )
			     "| DB ")
    (data-debug-insert-tag (cdr dbtag) (concat (make-string indent ? )
					       "| ")
			   "TAG ")
    (goto-char start)
    ))

;;; DEBUG COMMANDS
;;
;; Various commands to output aspects of the current semantic environment.
;;;###autoload
(defun semantic-adebug-bovinate ()
  "The same as `bovinate'.  Display the results in a debug buffer."
  (interactive)
  (let* ((start (current-time))
	 (out (semantic-fetch-tags))
	 (end (current-time)))

    (message "Retrieving tags took %.2f seconds."
	     (semantic-elapsed-time start end))
    
    (data-debug-new-buffer (concat "*" (buffer-name) " ADEBUG*"))
    (data-debug-insert-tag-list out "* "))
  )

;;;###autoload  
(defun semantic-adebug-searchdb (regex)
  "Search the semanticdb for REGEX for the current buffer.
Display the results as a debug list."
  (interactive "sSymbol Regex: ")
  (let ((start (current-time))
	(fr (semanticdb-find-tags-by-name-regexp regex))
	(end (current-time)))

    (data-debug-new-buffer (concat "*SEMANTICDB SEARCH: "
				   regex
				   " ADEBUG*"))
    (message "Search of tags took %.2f seconds."
	     (semantic-elapsed-time start end))
    
    (data-debug-insert-find-results fr "*")))

;;;###autoload
(defun semantic-adebug-analyze (&optional ctxt)
  "Perform `semantic-analyze-current-context'.
Display the results as a debug list.
Optional argument CTXT is the context to show."
  (interactive)
  (let ((start (current-time))
	(ctxt (or ctxt (semantic-analyze-current-context)))
	(end (current-time)))
    (if (not ctxt)
	(message "No Analyzer Results")
      (message "Analysis  took %.2f seconds."
	       (semantic-elapsed-time start end))
      (semantic-analyze-pulse ctxt)
      (if ctxt
	  (progn
	    (data-debug-new-buffer "*Analyzer ADEBUG*")
	    (data-debug-insert-object-slots ctxt "]"))
	(message "No Context to analyze here.")))))

;;;###autoload
(defun semantic-adebug-edebug-expr (expr)
  "Dump out the contets of some expression EXPR in edebug with adebug."
  (interactive "sExpression: ")
  (let ((v (eval (read expr))))
    (if (not v)
	(message "Expression %s is nil." expr)
      (data-debug-new-buffer "*expression ADEBUG*")
      (data-debug-insert-thing v "?" "")
      )))

;;;###autoload
(defun semanticdb-debug-file-tag-check (startfile)
  "Report debug info for checking STARTFILE for up-to-date tags."
  (interactive "FFile to Check (default = current-buffer): ")
  (let* ((file (file-truename startfile))
	 (default-directory (file-name-directory file))
	 (db (or
	      ;; This line will pick up system databases.
	      (semanticdb-directory-loaded-p default-directory)
	      ;; this line will make a new one if needed.
	      (semanticdb-get-database default-directory)))
	 (tab (semanticdb-file-table db file))
	 )
    (with-output-to-temp-buffer "*DEBUG STUFF*"
      (princ "Starting file is: ")
      (princ startfile)
      (princ "\nTrueName is: ")
      (princ file)
      (when (not (file-exists-p file))
	(princ "\nFile does not exist!"))
      (princ "\nDirectory Part is: ")
      (princ default-directory)
      (princ "\nFound Database is: ")
      (princ (object-print db))
      (princ "\nFound Table is: ")
      (if tab (princ (object-print tab)) (princ "nil"))
      (princ "\n\nAction Summary: ")
      (cond
       ((and tab
	     ;; Is this in a buffer?
	     (find-buffer-visiting (semanticdb-full-filename tab))
	     )
	(princ "Found Buffer: ")
	(prin1 (find-buffer-visiting (semanticdb-full-filename tab)))
	)
       ((and tab
	     ;; Is table fully loaded, or just a proxy?
	     (number-or-marker-p (oref tab pointmax))
	     ;; Is this table up to date with the file?
	     (not (semanticdb-needs-refresh-p tab)))
	(princ "Found table, no refresh needed.\n   Pointmax is: ")
	(prin1 (oref tab pointmax))
	)
       (t
	(princ "Found table that needs refresh.")
	(if (not tab)
	    (princ "\n   No Saved Point.")
	  (princ "\n  Saved pointmax: ")
	  (prin1 (oref tab pointmax))
	  (princ "  Needs Refresh: ")
	  (prin1 (semanticdb-needs-refresh-p tab))
	  )
	))
      ;; Buffer isn't loaded.  The only clue we have is if the file
      ;; is somehow different from our mark in the semanticdb table.
      (let* ((stats (file-attributes file))
	     (actualsize (nth 7 stats))
	     (actualmod (nth 5 stats))
	     )

	(if (or  (not tab)
		 (not (slot-boundp tab 'tags))
		 (not (oref tab tags)))
	    (princ "\n   No tags in table.")
	  (princ "\n   Number of known tags: ")
	  (prin1 (length (oref tab tags))))

	(princ "\n   File Size is: ")
	(prin1 actualsize)
	(princ "\n   File Mod Time is: ")
	(princ (format-time-string "%Y-%m-%d %T" actualmod))
	(when tab
	  (princ "\n   Saved file size is: ")
	  (prin1 (oref tab fsize))
	  (princ "\n   Saved Mod time is: ")
	  (princ (format-time-string "%Y-%m-%d %T"
				     (oref tab lastmodtime)))
	  )
	)
      )
    ;; Force load
    (semanticdb-file-table-object file)
    nil
    ))

;; (semanticdb-debug-file-tag-check "/usr/lib/gcc/i486-linux-gnu/4.2/include/stddef.h")
;; (semanticdb-debug-file-tag-check "/usr/include/stdlib.h")

  

(provide 'semantic-adebug)

;;; semantic-adebug.el ends here
