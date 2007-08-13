;;; semantic-adebug.el --- Semantic Application Debugger

;; Copyright (C) 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-adebug.el,v 1.6 2007/05/19 00:48:04 zappo Exp $

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
;;
;; Goals:
;;
;; Inspect all known details of a TAG in a buffer.
;; 
;; Analyze the list of active semantic databases, and the tags therin.
;;
;; Allow interactive navigation of the analysis process, tags, etc.
;;
;; Navigate to the correct function for debugging.

(require 'font-lock)
(require 'semantic-analyze)

;;; Code:

;;; GENERIC STUFF
;;
;;;###autoload
(defun semantic-adebug-insert-property-list (proplist prefix &optional parent)
  "Insert the property list PROPLIST.
Each line starts with PREFIX.
The attributes belong to the tag PARENT."
  (while proplist
    (let ((pretext (concat (symbol-name (car proplist)) " : ")))
      (semantic-adebug-insert-thing (car (cdr proplist))
				    prefix
				    pretext
				    parent))
    (setq proplist (cdr (cdr proplist)))))

;;; TAG STUFF
;;
(defun semantic-adebug-insert-tag-parts (tag prefix &optional parent)
  "Insert all the parts of TAG.
PREFIX specifies what to insert at the start of each line.
PARENT specifires any parent tag."
  (semantic-adebug-insert-thing (semantic-tag-name tag)
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
      (semantic-adebug-insert-thing ol prefix
				    "Position Data: "
				    parent)
      ))
  (let ((attrprefix (concat (make-string (length prefix) ? ) "# ")))
    (insert prefix "Attributes:\n")
    (semantic-adebug-insert-property-list
     (semantic-tag-attributes tag) attrprefix tag)
    (insert prefix "Properties:\n")
    (semantic-adebug-insert-property-list
     (semantic-tag-properties tag) attrprefix tag)
    )

  )

(defun semantic-adebug-insert-tag-parts-from-point (point)
  "Call `semantic-adebug-insert-tag-parts' based on text properties at POINT."
  (let ((tag (get-text-property point 'adebug))
	(parent (get-text-property point 'adebug-parent))
	(indent (get-text-property point 'adebug-indent))
	start end
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (semantic-adebug-insert-tag-parts tag
				      (concat (make-string indent ? )
					      "| ")
				      parent)
    (setq end (point))
    (goto-char start)
    ))

(defun semantic-adebug-insert-tag (tag prefix prebuttontext &optional parent)
  "Insert TAG into the current buffer at the current point.
PREFIX specifies text to insert in front of TAG.
Optional PARENT is the parent tag containing TAG.
Add text properties needed to allow tag expansion later."
  (let ((start (point))
	(end nil)
	(str (semantic-format-tag-uml-abbreviate tag parent t))
	(tip (semantic-format-tag-prototype tag parent t))
	)
    (insert prefix prebuttontext str "\n")
    (setq end (point))
    (put-text-property start end 'adebug tag)
    (put-text-property start end 'adebug-parent parent)
    (put-text-property start end 'adebug-indent(length prefix))
    (put-text-property start end 'adebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'adebug-function
		       'semantic-adebug-insert-tag-parts-from-point)
    
    ))

;;; TAG LISTS
;;
(defun semantic-adebug-insert-tag-list (taglist prefix &optional parent)
  "Insert the tag list TAGLIST with PREFIX.
Optional argument PARENT specifies the part of TAGLIST."
  (while taglist
    (if (semantic-tag-p (car taglist))
	(semantic-adebug-insert-tag (car taglist) prefix "" parent)
      (semantic-adebug-insert-thing (car taglist) prefix "" parent))
    (setq taglist (cdr taglist))))

(defun semantic-adebug-insert-taglist-from-point (point)
  "Insert the taglist found at the taglist button at POINT."
  (let ((taglist (get-text-property point 'adebug))
	(parent (get-text-property point 'adebug-parent))
	(indent (get-text-property point 'adebug-indent))
	start end
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (semantic-adebug-insert-tag-list taglist
				     (concat (make-string indent ? )
					     "* ")
				     parent)
    (setq end (point))
    (goto-char start)

  ))

(defun semantic-adebug-insert-tag-list-button (taglist prefix prebuttontext &optional parent)
  "Insert a single summary of a TAGLIST.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between PREFIX and the taglist button.
PARENT is the tag that represents the parent of all the tags."
  (let ((start (point))
	(end nil)
	(str (format "#<TAG LIST: %d entries>" (length taglist)))
	(tip nil))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-function-name-face)
    (put-text-property start end 'adebug taglist)
    (put-text-property start end 'adebug-parent parent)
    (put-text-property start end 'adebug-indent(length prefix))
    (put-text-property start end 'adebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'adebug-function
		       'semantic-adebug-insert-taglist-from-point)
    (insert "\n")
    ))

;;; SEMANTICDB FIND RESULTS
;;
(defun semantic-adebug-insert-find-results (findres prefix)
  "Insert the find results FINDRES with PREFIX."
  ;; ( (DBOBJ TAG TAG TAG) (DBOBJ TAG TAG TAG) ... )
  (let ((cnt 1))
    (while findres
      (let* ((dbhit (car findres))
	     (db (car dbhit))
	     (tags (cdr dbhit)))
	(semantic-adebug-insert-thing db prefix (format "DB %d: " cnt))
	(semantic-adebug-insert-thing tags prefix (format "HITS %d: " cnt))
	)
      (setq findres (cdr findres)
	    cnt (1+ cnt)))))

(defun semantic-adebug-insert-find-results-from-point (point)
  "Insert the find results found at the find results button at POINT."
  (let ((findres (get-text-property point 'adebug))
	(indent (get-text-property point 'adebug-indent))
	start end
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (semantic-adebug-insert-find-results findres
					 (concat (make-string indent ? )
						 "!* ")
					 )
    (setq end (point))
    (goto-char start)
  ))

(defun semantic-adebug-insert-find-results-button (findres prefix prebuttontext)
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
    (put-text-property start end 'adebug findres)
    (put-text-property start end 'adebug-indent(length prefix))
    (put-text-property start end 'adebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'adebug-function
		       'semantic-adebug-insert-taglist-from-point)
    (insert "\n")
    ))

;;; overlays
;;
(defun semantic-adebug-insert-overlay-props (overlay prefix)
  "Insert all the parts of OVERLAY.
PREFIX specifies what to insert at the start of each line."
  (let ((attrprefix (concat (make-string (length prefix) ? ) "# "))
	(proplist (semantic-overlay-properties overlay)))
    (semantic-adebug-insert-property-list
     proplist attrprefix)
    )
  )

(defun semantic-adebug-insert-overlay-from-point (point)
  "Insert the overlay found at the overlay button at POINT."
  (let ((overlay (get-text-property point 'adebug))
	(indent (get-text-property point 'adebug-indent))
	start end
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (semantic-adebug-insert-overlay-props overlay
					  (concat (make-string indent ? )
						  "| "))
    (setq end (point))
    (goto-char start)
    ))

(defun semantic-adebug-insert-overlay-button (overlay prefix prebuttontext)
  "Insert a button representing OVERLAY.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the overlay button."
  (let ((start (point))
	(end nil)
	(str (format "%s" overlay))
	(tip nil))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-comment-face)
    (put-text-property start end 'adebug overlay)
    (put-text-property start end 'adebug-indent(length prefix))
    (put-text-property start end 'adebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'adebug-function
		       'semantic-adebug-insert-overlay-from-point)
    (insert "\n")
    )
  )

;;; overlay list
;;
(defun semantic-adebug-insert-overlay-list (overlaylist prefix)
  "Insert all the parts of OVERLAYLIST.
PREFIX specifies what to insert at the start of each line."
  (while overlaylist
    (semantic-adebug-insert-overlay-button (car overlaylist)
					   prefix
					   "")
    (setq overlaylist (cdr overlaylist))))

(defun semantic-adebug-insert-overlay-list-from-point (point)
  "Insert the overlay found at the overlay list button at POINT."
  (let ((overlaylist (get-text-property point 'adebug))
	(indent (get-text-property point 'adebug-indent))
	start end
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (semantic-adebug-insert-overlay-list overlaylist
					  (concat (make-string indent ? )
						  "* "))
    (setq end (point))
    (goto-char start)
    ))

(defun semantic-adebug-insert-overlay-list-button (overlaylist
						   prefix
						   prebuttontext)
  "Insert a button representing OVERLAYLIST.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the overlay list button."
  (let ((start (point))
	(end nil)
	(str (format "#<overlay list: %d entries>" (length overlaylist)))
	(tip nil))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-comment-face)
    (put-text-property start end 'adebug overlaylist)
    (put-text-property start end 'adebug-indent(length prefix))
    (put-text-property start end 'adebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'adebug-function
		       'semantic-adebug-insert-overlay-list-from-point)
    (insert "\n")
    )
  )

;;; Rings
;;
;; A ring (like kill-ring, or whatever.)
(defun semantic-adebug-insert-ring-contents (ring prefix)
  "Insert all the parts of RING.
PREFIX specifies what to insert at the start of each line."
  (let ((elts (ring-elements ring))
	)
    (while elts
      (semantic-adebug-insert-thing (car elts) prefix "")
      (setq elts (cdr elts)))))

(defun semantic-adebug-insert-ring-items-from-point (point)
  "Insert the ring found at the ring button at POINT."
  (let ((ring (get-text-property point 'adebug))
	(indent (get-text-property point 'adebug-indent))
	start end
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (semantic-adebug-insert-ring-contents ring
					  (concat (make-string indent ? )
						  "} "))
    (setq end (point))
    (goto-char start)
    ))

(defun semantic-adebug-insert-ring-button (ring
					   prefix
					   prebuttontext)
  "Insert a button representing RING.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the stuff list button."
  (let* ((start (point))
	 (end nil)
	 (str (format "#<RING: %d>" (ring-size ring)))
	 (ringthing (ring-ref ring 0))
	 (tip (format "Ring max-size %d, length %d.  Full of: %S"
		      (ring-size ring)
		      (ring-length ring)
		      (cond ((stringp ringthing)
			     "strings")
			    ((semantic-tag-p ringthing)
			     "tags")
			    ((object-p ringthing)
			     "eieio objects")
			    ((listp ringthing)
			     "List of somethin'")
			    (t "stuff"))))
	 )
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-type-face)
    (put-text-property start end 'adebug ring)
    (put-text-property start end 'adebug-indent(length prefix))
    (put-text-property start end 'adebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'adebug-function
		       'semantic-adebug-insert-ring-items-from-point)
    (insert "\n")
    )
  )

;;; list of stuff
;;
;; just a list.  random stuff inside.
;;;###autoload
(defun semantic-adebug-insert-stuff-list (stufflist prefix)
  "Insert all the parts of STUFFLIST.
PREFIX specifies what to insert at the start of each line."
  (while stufflist
    (semantic-adebug-insert-thing
     ;; Some lists may put a value in the CDR
     (if (listp stufflist) (car stufflist) stufflist)
     prefix
     "")
    (setq stufflist
	  (if (listp stufflist)
	      (cdr stufflist)
	    nil))))

(defun semantic-adebug-insert-stuff-list-from-point (point)
  "Insert the stuff found at the stuff list button at POINT."
  (let ((stufflist (get-text-property point 'adebug))
	(indent (get-text-property point 'adebug-indent))
	start end
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (semantic-adebug-insert-stuff-list stufflist
				       (concat (make-string indent ? )
					       "> "))
    (setq end (point))
    (goto-char start)
    ))

(defun semantic-adebug-insert-stuff-list-button (stufflist
						 prefix
						 prebuttontext)
  "Insert a button representing STUFFLIST.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the stuff list button."
  (let ((start (point))
	(end nil)
	(str
	 (condition-case nil
	     (format "#<list o' stuff: %d entries>" (length stufflist))
	   (error "#<list o' stuff>")))
	(tip (format "%s" stufflist)))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-variable-name-face)
    (put-text-property start end 'adebug stufflist)
    (put-text-property start end 'adebug-indent(length prefix))
    (put-text-property start end 'adebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'adebug-function
		       'semantic-adebug-insert-stuff-list-from-point)
    (insert "\n")
    )
  )

;;; simple thing
(defun semantic-adebug-insert-simple-thing (thing prefix prebuttontext face)
  "Insert one simple THING with a face.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the thing.
FACE is the face to use."
  (insert prefix prebuttontext)
  (let ((start (point))
	(end nil))
    (insert (format "%s" thing))
    (setq end (point))
    (insert "\n" )
    (put-text-property start end 'face face)
    ))

;; uber insert method
(defun semantic-adebug-insert-thing (thing prefix prebuttontext &optional parent)
  "Insert THING with PREFIX.
PREBUTTONTEXT is some text to insert between prefix and the thing
that is not included in the indentation calculation of any children.
If PARENT is non-nil, it is somehow related as a parent to thing."
  (cond
   ;; eieio object
   ((object-p thing)
    (semantic-adebug-insert-object-button
     thing prefix prebuttontext))

   ;; tag
   ((semantic-tag-p thing)
    (semantic-adebug-insert-tag
     thing prefix prebuttontext parent))

   ;; taglist
   ((and (listp thing) (semantic-tag-p (car thing)))
    (semantic-adebug-insert-tag-list-button
     thing prefix prebuttontext parent))

   ;; find results
   ((semanticdb-find-results-p thing)
    (semantic-adebug-insert-find-results-button
     thing prefix prebuttontext))
   
   ;; Overlay
   ((semantic-overlay-p thing)
    (semantic-adebug-insert-overlay-button thing prefix prebuttontext)
    )
   ((and (listp thing) (semantic-overlay-p (car thing)))
    (semantic-adebug-insert-overlay-list-button thing prefix prebuttontext)
    )

   ;; String
   ((stringp thing)
    (semantic-adebug-insert-simple-thing thing prefix prebuttontext
					 'font-lock-string-face)
    )

   ;; Symbol
   ((symbolp thing)
    (cond ((fboundp thing)
	   (semantic-adebug-insert-simple-thing
	    thing prefix (concat prebuttontext "#'")
	    'font-lock-function-name-face)
	   )
	  ((boundp thing)
	   (semantic-adebug-insert-simple-thing
	    thing prefix (concat prebuttontext "'")
	    'font-lock-variable-name-face))
	  (t
	   (semantic-adebug-insert-simple-thing
	    thing prefix (concat prebuttontext "'")
	    nil)
	   )
	  ))

   ;; Ring
   ((ring-p thing)
    (semantic-adebug-insert-ring-button thing prefix prebuttontext))

   ;; List of stuff
   ((listp thing)
    (semantic-adebug-insert-stuff-list-button thing prefix prebuttontext))

   (t
    (insert prefix prebuttontext (format "%S" thing) "\n" ))
   )
  )

;;; MAJOR MODE
;;
;; The Adebug major mode provides an interactive space to explore
;; the current state of semantic's parsing and analysis
;;
(defgroup semantic-adebug nil
  "semantic-adebug group."
  :group 'langauges)

(defvar semantic-adebug-mode-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?\; ". 12"  table) ;; SEMI, Comment start ;;
    (modify-syntax-entry ?\n ">"     table) ;; Comment end
    (modify-syntax-entry ?\" "\""    table) ;; String
    (modify-syntax-entry ?\- "_"     table) ;; Symbol
    (modify-syntax-entry ?\\ "\\"    table) ;; Quote
    (modify-syntax-entry ?\` "'"     table) ;; Prefix ` (backquote)
    (modify-syntax-entry ?\' "'"     table) ;; Prefix ' (quote)
    (modify-syntax-entry ?\, "'"     table) ;; Prefix , (comma)
    
    table)
  "Syntax table used in semantic-adebug macro buffers.")

(defvar semantic-adebug-map
  (let ((km (make-sparse-keymap)))
    (define-key km [mouse-2] 'semantic-adebug-expand-or-contract-mouse)
    (define-key km " " 'semantic-adebug-expand-or-contract)
    (define-key km "n" 'semantic-adebug-next)
    (define-key km "p" 'semantic-adebug-prev)
    (define-key km "N" 'semantic-adebug-next-expando)
    (define-key km "P" 'semantic-adebug-prev-expando)
    km)
  "Keymap used in semantic-adebug.")

(defcustom semantic-adebug-mode-hook nil
  "*Hook run when semantic-adebug starts."
  :group 'semantic-adebug
  :type 'hook)

;;;###autoload
(defun semantic-adebug-mode ()
  "Major-mode for the Analyzer debugger.

\\{semantic-adebug-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'semantic-adebug-mode
        mode-name "SEMANTIC-ADEBUG"
	comment-start ";;"
	comment-end "")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (set-syntax-table semantic-adebug-mode-syntax-table)
  (use-local-map semantic-adebug-map)
  (run-hooks 'semantic-adebug-hook)
  )

;;;###autoload
(defun semantic-adebug-new-buffer (name)
  "Create a new adebug buffer with NAME."
  (let ((b (get-buffer-create name)))
    (switch-to-buffer b)
    (set-buffer b)
    (erase-buffer)
    (semantic-adebug-mode)
    b))

;;; Adebug mode commands
;;
(defun semantic-adebug-next ()
  "Go to the next line in the ADebug buffer."
  (interactive)
  (forward-line 1)
  (beginning-of-line)
  (skip-chars-forward " *-><[]" (point-at-eol)))

(defun semantic-adebug-prev ()
  "Go to the next line in the ADebug buffer."
  (interactive)
  (forward-line -1)
  (beginning-of-line)
  (skip-chars-forward " *-><[]" (point-at-eol)))

(defun semantic-adebug-next-expando ()
  "Go to the next line in the ADebug buffer.
Contract the current line (if open) and expand the line
we move to."
  (interactive)
  (semantic-adebug-contract-current-line)
  (semantic-adebug-next)
  (semantic-adebug-expand-current-line)
  )

(defun semantic-adebug-prev-expando ()
  "Go to the previous line in the ADebug buffer.
Contract the current line (if open) and expand the line
we move to."
  (interactive)
  (semantic-adebug-contract-current-line)
  (semantic-adebug-prev)
  (semantic-adebug-expand-current-line)
  )

(defun semantic-adebug-current-line-expanded-p ()
  "Return non-nil if the current line is expanded."
  (let ((ti (current-indentation))
	(ni (condition-case nil
		(save-excursion
		  (end-of-line)
		  (forward-char 1)
		  (current-indentation))
	      (error 0))))
    (> ni ti)))

(defun semantic-adebug-expand-current-line ()
  "Expand the current line (if possible).
Do nothing if already expanded."
  (when (not (semantic-adebug-current-line-expanded-p))
    ;; If the next line is the same or less indentation, expand.
    (let ((fcn (get-text-property (point) 'adebug-function)))
      (when fcn
	(funcall fcn (point))
	(beginning-of-line)
	))))

(defun semantic-adebug-contract-current-line ()
  "Contract the current line (if possible).
Do nothing if already expanded."
  (when (and (semantic-adebug-current-line-expanded-p)
	     ;; Don't contract if the current line is not expandable.
	     (get-text-property (point) 'adebug-function))
    (let ((ti (current-indentation))
	  )
      ;; If next indentation is larger, collapse.
      (end-of-line)
      (forward-char 1)
      (let ((start (point))
	    (end nil))
	(condition-case nil
	    (progn
	      ;; Keep checking indentation
	      (while (or (> (current-indentation) ti)
			 (looking-at "^\\s-*$"))
		(end-of-line)
		(forward-char 1))
	      (setq end (point))
	      )
	  (error (setq end (point-max))))
	(delete-region start end)
	(forward-char -1)
	(beginning-of-line)))))

(defun semantic-adebug-expand-or-contract ()
  "Expand or contract anything at the current point."
  (interactive)
  (if (semantic-adebug-current-line-expanded-p)
      (semantic-adebug-contract-current-line)
    (semantic-adebug-expand-current-line))
  (skip-chars-forward " *-><[]" (point-at-eol)))

(defun semantic-adebug-expand-or-contract-mouse (e)
  "Expand or contract anything at event E."
  (interactive "e")
  (goto-char (posn-point (event-start e)))
  (semantic-adebug-expand-or-contract)
  )

;;; DEBUG COMMANDS
;;
;; Various commands to output aspects of the current semantic environment.
;;;###autoload
(defun semantic-adebug-bovinate ()
  "The same as `bovinate'. Display the results in a debug buffer."
  (interactive)
  (let* ((start (current-time))
	 (out (semantic-fetch-tags))
	 (end (current-time))
	 (ab (semantic-adebug-new-buffer (concat "*"
						 (buffer-name)
						 " ADEBUG*")))
	 )
    (message "Retrieving tags took %.2f seconds."
	     (semantic-elapsed-time start end))

    (semantic-adebug-insert-tag-list out "* "))
  )

;;;###autoload  
(defun semantic-adebug-searchdb (regex)
  "Search the semanticdb for REGEX for the current buffer.
Display the results as a debug list."
  (interactive "sSymbol Regex: ")
  (let ((start (current-time))
	(fr (semanticdb-find-tags-by-name-regexp regex))
	(end (current-time))
	(ab (semantic-adebug-new-buffer (concat "*SEMANTICDB SEARCH: "
						regex
						" ADEBUG*"))))
    (message "Search of tags took %.2f seconds."
	     (semantic-elapsed-time start end))
	     
    (semantic-adebug-insert-find-results fr "*")))

;;;###autoload
(defun semantic-adebug-analyze ()
  "Perform `semantic-analyze-current-context'.
Display the results as a debug list."
  (interactive)
  (let ((start (current-time))
	(ctxt (semantic-analyze-current-context))
	(end (current-time))
	(ab nil))
    (message "Analysis  took %.2f seconds."
	     (semantic-elapsed-time start end))
    (if ctxt
	(progn
	  (setq ab (semantic-adebug-new-buffer "*Analyzer ADEBUG*"))
	  (semantic-adebug-insert-object-fields ctxt "]"))
      (message "No Context to analyze here."))))

;;;###autoload
(defun semantic-adebug-edebug-expr (expr)
  "Dump out the contets of some expression EXPR in edebug with adebug."
  (interactive "sExpression: ")
  (let ((v (eval (read expr)))
	(ab nil))
    (if (not v)
	(message "Expression %s is nil." expr)
      (setq ab (semantic-adebug-new-buffer "*expression ADEBUG*"))
      (semantic-adebug-insert-thing v "?" "")
      )))
  

(provide 'semantic-adebug)

;;; semantic-adebug.el ends here
