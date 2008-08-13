;;; data-debug.el --- Datastructure Debugger

;; Copyright (C) 2007, 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: data-debug.el,v 1.7 2008/06/19 01:37:49 zappo Exp $

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
;; Provide a simple way to investigate particularly large and complex
;; data structures.
;;

(require 'font-lock)
;;; Code:

;;; Compatibility
;;
(if (featurep 'xemacs)
    (eval-and-compile
      (defalias 'data-debug-overlay-properties 'extent-properties)
      (defalias 'data-debug-overlay-p 'extentp)
      )
  ;; Regular Emacs
  (eval-and-compile
    (defalias 'data-debug-overlay-properties 'overlay-properties)
    (defalias 'data-debug-overlay-p 'overlayp)
    )
  )

;;; GENERIC STUFF
;;
;;;###autoload
(defun data-debug-insert-property-list (proplist prefix &optional parent)
  "Insert the property list PROPLIST.
Each line starts with PREFIX.
The attributes belong to the tag PARENT."
  (while proplist
    (let ((pretext (concat (symbol-name (car proplist)) " : ")))
      (data-debug-insert-thing (car (cdr proplist))
			       prefix
			       pretext
			       parent))
    (setq proplist (cdr (cdr proplist)))))

;;; overlays
;;
(defun data-debug-insert-overlay-props (overlay prefix)
  "Insert all the parts of OVERLAY.
PREFIX specifies what to insert at the start of each line."
  (let ((attrprefix (concat (make-string (length prefix) ? ) "# "))
	(proplist (data-debug-overlay-properties overlay)))
    (data-debug-insert-property-list
     proplist attrprefix)
    )
  )

(defun data-debug-insert-overlay-from-point (point)
  "Insert the overlay found at the overlay button at POINT."
  (let ((overlay (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start end
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-overlay-props overlay
				     (concat (make-string indent ? )
					     "| "))
    (setq end (point))
    (goto-char start)
    ))

(defun data-debug-insert-overlay-button (overlay prefix prebuttontext)
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
    (put-text-property start end 'ddebug overlay)
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-overlay-from-point)
    (insert "\n")
    )
  )

;;; overlay list
;;
(defun data-debug-insert-overlay-list (overlaylist prefix)
  "Insert all the parts of OVERLAYLIST.
PREFIX specifies what to insert at the start of each line."
  (while overlaylist
    (data-debug-insert-overlay-button (car overlaylist)
				      prefix
				      "")
    (setq overlaylist (cdr overlaylist))))

(defun data-debug-insert-overlay-list-from-point (point)
  "Insert the overlay found at the overlay list button at POINT."
  (let ((overlaylist (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start end
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-overlay-list overlaylist
				    (concat (make-string indent ? )
					    "* "))
    (setq end (point))
    (goto-char start)
    ))

(defun data-debug-insert-overlay-list-button (overlaylist
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
    (put-text-property start end 'ddebug overlaylist)
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-overlay-list-from-point)
    (insert "\n")
    )
  )

;;; processes
;;
(defun data-debug-insert-process-props (process prefix)
  "Insert all the parts of PROCESS.
PREFIX specifies what to insert at the start of each line."
  (let ((attrprefix (concat (make-string (length prefix) ? ) "# "))
	(id (process-id process))
	(tty (process-tty-name process))
	(pcontact (process-contact process t))
	(proplist (process-plist process)))
    (data-debug-insert-property-list
     (append
      (if id (list 'id id))
      (if tty (list 'tty tty))
      (if pcontact pcontact)
      proplist)
     attrprefix)
    )
  )

(defun data-debug-insert-process-from-point (point)
  "Insert the process found at the process button at POINT."
  (let ((process (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start end
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-process-props process
				     (concat (make-string indent ? )
					     "| "))
    (setq end (point))
    (goto-char start)
    ))

(defun data-debug-insert-process-button (process prefix prebuttontext)
  "Insert a button representing PROCESS.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the process button."
  (let ((start (point))
	(end nil)
	(str (format "%S : %s" process (process-status process)))
	(tip nil))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-comment-face)
    (put-text-property start end 'ddebug process)
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-process-from-point)
    (insert "\n")
    )
  )

;;; Rings
;;
;; A ring (like kill-ring, or whatever.)
(defun data-debug-insert-ring-contents (ring prefix)
  "Insert all the parts of RING.
PREFIX specifies what to insert at the start of each line."
  (let ((len (ring-length ring))
	(idx 0)
	)
    (while (< idx len)
      (data-debug-insert-thing (ring-ref ring idx) prefix "")
      (setq idx (1+ idx))
      )))

(defun data-debug-insert-ring-items-from-point (point)
  "Insert the ring found at the ring button at POINT."
  (let ((ring (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start end
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-ring-contents ring
				     (concat (make-string indent ? )
					     "} "))
    (setq end (point))
    (goto-char start)
    ))

(defun data-debug-insert-ring-button (ring
				      prefix
				      prebuttontext)
  "Insert a button representing RING.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the stuff list button."
  (let* ((start (point))
	 (end nil)
	 (str (format "#<RING: %d, %d max>"
		      (ring-length ring)
		      (ring-size ring)))
	 (ringthing
	  (if (= (ring-length ring) 0) nil (ring-ref ring 0)))
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
    (put-text-property start end 'ddebug ring)
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-ring-items-from-point)
    (insert "\n")
    )
  )

;;; list of stuff
;;
;; just a list.  random stuff inside.
;;;###autoload
(defun data-debug-insert-stuff-list (stufflist prefix)
  "Insert all the parts of STUFFLIST.
PREFIX specifies what to insert at the start of each line."
  (while stufflist
    (data-debug-insert-thing
     ;; Some lists may put a value in the CDR
     (if (listp stufflist) (car stufflist) stufflist)
     prefix
     "")
    (setq stufflist
	  (if (listp stufflist)
	      (cdr-safe stufflist)
	    nil))))

(defun data-debug-insert-stuff-list-from-point (point)
  "Insert the stuff found at the stuff list button at POINT."
  (let ((stufflist (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start end
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-stuff-list stufflist
				  (concat (make-string indent ? )
					  "> "))
    (setq end (point))
    (goto-char start)
    ))

(defun data-debug-insert-stuff-list-button (stufflist
					    prefix
					    prebuttontext)
  "Insert a button representing STUFFLIST.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the stuff list button."
  (let ((start (point))
	(end nil)
	(str
	 (condition-case nil
	     (format "#<list o' stuff: %d entries>" (safe-length stufflist))
	   (error "#<list o' stuff>")))
	(tip (if (or (listp (car stufflist))
		     (vectorp (car stufflist)))
		 ""
	       (format "%s" stufflist))))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-variable-name-face)
    (put-text-property start end 'ddebug stufflist)
    (put-text-property start end 'ddebug-indent (length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-stuff-list-from-point)
    (insert "\n")
    )
  )

;;; String
(defun data-debug-insert-string (thing prefix prebuttontext)
  "Insert one symbol THING.
A Symbol is a simple thing, but this provides some face and prefix rules.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the thing."
  (insert prefix prebuttontext)
  (let ((start (point))
	(end nil))
    (insert (format "\"%s\"" thing))
    (setq end (point))
    (insert "\n" )
    (put-text-property start end 'face font-lock-string-face)
    ))

;;; String
(defun data-debug-insert-number (thing prefix prebuttontext)
  "Insert one symbol THING.
A Symbol is a simple thing, but this provides some face and prefix rules.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the thing."
  (insert prefix prebuttontext)
  (let ((start (point))
	(end nil))
    (insert (format "%S" thing))
    (setq end (point))
    (insert "\n" )
    (put-text-property start end 'face font-lock-string-face)
    ))

;;; Symbol
(defun data-debug-insert-symbol (thing prefix prebuttontext)
  "Insert one symbol THING.
A Symbol is a simple thing, but this provides some face and prefix rules.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the thing."
  (cond ((fboundp thing)
	 (data-debug-insert-simple-thing
	  thing prefix (concat prebuttontext "#'")
	  'font-lock-function-name-face)
	 )
	((boundp thing)
	 (data-debug-insert-simple-thing
	  thing prefix (concat prebuttontext "'")
	  'font-lock-variable-name-face))
	(t
	 (data-debug-insert-simple-thing
	  thing prefix (concat prebuttontext "'")
	  nil)
	 )
	)
  )

;;; simple thing
(defun data-debug-insert-simple-thing (thing prefix prebuttontext face)
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

;;; simple thing
(defun data-debug-insert-custom (thingstring prefix prebuttontext face)
  "Insert one simple THINGSTRING with a face.
Use for simple items that need a custom insert.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between prefix and the thing.
FACE is the face to use."
  (insert prefix prebuttontext)
  (let ((start (point))
	(end nil))
    (insert thingstring)
    (setq end (point))
    (insert "\n" )
    (put-text-property start end 'face face)
    ))


(defvar data-debug-thing-alist
  '(
    ;; eieio object
    ((lambda (thing) (object-p thing)) . data-debug-insert-object-button)

    ;; tag
    (semantic-tag-p . data-debug-insert-tag)

    ;; taglist
    ((lambda (thing) (and (listp thing) (semantic-tag-p (car thing)))) .
     data-debug-insert-tag-list-button)

    ;; find results
    (semanticdb-find-results-p . data-debug-insert-find-results-button)
   
    ;; Overlay
    (data-debug-overlay-p . data-debug-insert-overlay-button)

    ;; overlay list
    ((lambda (thing) (and (listp thing) (semantic-overlay-p (car thing)))) .
     data-debug-insert-overlay-list-button)

    ;; process
    (processp . data-debug-insert-process-button)

    ;; String
    (stringp . data-debug-insert-string)

    ;; Numbers
    (numberp . data-debug-insert-number)

    ;; Symbol
    (symbolp . data-debug-insert-symbol)

    ;; Ring
    (ring-p . data-debug-insert-ring-button)

    ;; List of stuff
    (listp . data-debug-insert-stuff-list-button)
    )
  "Alist of methods used to insert things into an Ddebug buffer.")

;; uber insert method
;;;###autoload
(defun data-debug-insert-thing (thing prefix prebuttontext &optional parent)
  "Insert THING with PREFIX.
PREBUTTONTEXT is some text to insert between prefix and the thing
that is not included in the indentation calculation of any children.
If PARENT is non-nil, it is somehow related as a parent to thing."
  (when (catch 'done
	  (dolist (test data-debug-thing-alist)
	    (when (funcall (car test) thing)
	      (condition-case nil
		  (funcall (cdr test) thing prefix prebuttontext parent)
		(error
		 (funcall (cdr test) thing prefix prebuttontext)))
	      (throw 'done nil))
	    )
	  nil)
    (data-debug-insert-simple-thing (format "%S" thing)
				    prefix
				    prebuttontext
				    'bold)))

;;; MAJOR MODE
;;
;; The Ddebug major mode provides an interactive space to explore
;; the current state of semantic's parsing and analysis
;;
(defgroup data-debug nil
  "data-debug group."
  :group 'langauges)

(defvar data-debug-mode-syntax-table
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
  "Syntax table used in data-debug macro buffers.")

(defvar data-debug-map
  (let ((km (make-sparse-keymap)))
    (define-key km [mouse-2] 'data-debug-expand-or-contract-mouse)
    (define-key km " " 'data-debug-expand-or-contract)
    (define-key km "\C-m" 'data-debug-expand-or-contract)
    (define-key km "n" 'data-debug-next)
    (define-key km "p" 'data-debug-prev)
    (define-key km "N" 'data-debug-next-expando)
    (define-key km "P" 'data-debug-prev-expando)
    km)
  "Keymap used in data-debug.")

(defcustom data-debug-mode-hook nil
  "*Hook run when data-debug starts."
  :group 'data-debug
  :type 'hook)

;;;###autoload
(defun data-debug-mode ()
  "Major-mode for the Analyzer debugger.

\\{data-debug-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'data-debug-mode
        mode-name "DATA-DEBUG"
	comment-start ";;"
	comment-end "")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (set-syntax-table data-debug-mode-syntax-table)
  (use-local-map data-debug-map)
  (run-hooks 'data-debug-hook)
  (font-lock-mode -1)
  )

;;;###autoload
(defun data-debug-new-buffer (name)
  "Create a new ddebug buffer with NAME."
  (let ((b (get-buffer-create name)))
    (pop-to-buffer b)
    (set-buffer b)
    (erase-buffer)
    (data-debug-mode)
    b))

;;; Ddebug mode commands
;;
(defun data-debug-next ()
  "Go to the next line in the Ddebug buffer."
  (interactive)
  (forward-line 1)
  (beginning-of-line)
  (skip-chars-forward " *-><[]" (point-at-eol)))

(defun data-debug-prev ()
  "Go to the next line in the Ddebug buffer."
  (interactive)
  (forward-line -1)
  (beginning-of-line)
  (skip-chars-forward " *-><[]" (point-at-eol)))

(defun data-debug-next-expando ()
  "Go to the next line in the Ddebug buffer.
Contract the current line (if open) and expand the line
we move to."
  (interactive)
  (data-debug-contract-current-line)
  (data-debug-next)
  (data-debug-expand-current-line)
  )

(defun data-debug-prev-expando ()
  "Go to the previous line in the Ddebug buffer.
Contract the current line (if open) and expand the line
we move to."
  (interactive)
  (data-debug-contract-current-line)
  (data-debug-prev)
  (data-debug-expand-current-line)
  )

(defun data-debug-current-line-expanded-p ()
  "Return non-nil if the current line is expanded."
  (let ((ti (current-indentation))
	(ni (condition-case nil
		(save-excursion
		  (end-of-line)
		  (forward-char 1)
		  (current-indentation))
	      (error 0))))
    (> ni ti)))

(defun data-debug-line-expandable-p ()
  "Return non-nil if the current line is expandable.
Lines that are not expandable are assumed to not be contractable."
  (not (get-text-property (point) 'ddebug-noexpand)))

(defun data-debug-expand-current-line ()
  "Expand the current line (if possible).
Do nothing if already expanded."
  (when (or (not (data-debug-line-expandable-p))
	    (not (data-debug-current-line-expanded-p)))
    ;; If the next line is the same or less indentation, expand.
    (let ((fcn (get-text-property (point) 'ddebug-function)))
      (when fcn
	(funcall fcn (point))
	(beginning-of-line)
	))))

(defun data-debug-contract-current-line ()
  "Contract the current line (if possible).
Do nothing if already expanded."
  (when (and (data-debug-current-line-expanded-p)
	     ;; Don't contract if the current line is not expandable.
	     (get-text-property (point) 'ddebug-function))
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

(defun data-debug-expand-or-contract ()
  "Expand or contract anything at the current point."
  (interactive)
  (if (and (data-debug-line-expandable-p)
	   (data-debug-current-line-expanded-p))
      (data-debug-contract-current-line)
    (data-debug-expand-current-line))
  (skip-chars-forward " *-><[]" (point-at-eol)))

(defun data-debug-expand-or-contract-mouse (event)
  "Expand or contract anything at event EVENT."
  (interactive "e")
  (let* ((startwin (selected-window))
	 (win (car (car (cdr event))))
	 (eb (window-buffer win))
	 )
    (select-window win t)
    (save-excursion
      ;(goto-char (window-start win))
      (mouse-set-point event)
      (data-debug-expand-or-contract))
    ))

;;; DEBUG COMMANDS
;;
;; Various commands to output aspects of the current semantic environment.

;;;###autoload
(defun data-debug-edebug-expr (expr)
  "Dump out the contets of some expression EXPR in edebug with ddebug."
  (interactive "sExpression: ")
  (let ((v (eval (read expr)))
	(ab nil))
    (if (not v)
	(message "Expression %s is nil." expr)
      (setq ab (data-debug-new-buffer "*expression DDEBUG*"))
      (data-debug-insert-thing v "?" "")
      )))
  

(provide 'data-debug)

;;; data-debug.el ends here
