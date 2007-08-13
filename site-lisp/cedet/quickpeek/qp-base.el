;;; qp-base.el --- Base classes for the quickpeek display system

;;; Copyright (C) 2000, 2001 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: qp-base.el,v 1.3 2005/09/30 20:42:01 zappo Exp $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; The QuickPeek base classes form the base line for displaying, and
;; managing custom completion systems for any language.  To support an
;; arbitrary file, inherit from `quickpeek-default'.  To support a
;; language, inherit from `quickpeek-language', and override those
;; elements needed.  If you language supports the Semantic Bovinator,
;; inherit instead from `quickpeek-semantic', which in turn inherits
;; from `quickpeek-language.'

(eval-and-compile
  (condition-case nil
      (require 'thingatpt)
    (error (require 'thing))))

(require 'tempo)

;;; Code:
(defclass quickpeek-default ()
  (
   )
  "The default quickpeek class used for a given buffer.")

(defclass quickpeek-language (quickpeek-default)
  ((keywords :initform :keywords
	     :initform nil
	     :documentation "Keywords available in the current buffer.
This is a list of the form:
 (\"KEYWORD\" \"Formatted description string.\" ([TEMPO-TEMPLATES]))
where KEYWORD is the keyword followed by a nice description.
TEMPO-TEMPLATES is an optional list of templates that can be used to
auto-insert a code structure that uses this keyword.")
   (builtintype :initform :builtintype
		:initform nil
		:documentation "Built in types available for current buffer.
This is a list of the form:
 (\"TYPE\" \"Formatted description string.\" ([TEMPO-TEMPLATES]))
where KEYWORD is the keyword followed by a nice description.
TEMPO-TEMPLATES is an optional list of templates that can be used to
auto-insert a code structure that uses this keyword.")
   (current-fn :initform nil
	       :documentation "Store the current function here.")
   (current-thing :initform nil
		  :documentation "Store the current thing here.
The thing is the text under the cursor.")
   )
  "Base class for language completion displays.
Uses TAGS files to look up symbols and create completion lists, It
requires language specific parsing routines to flesh out the generation of ")

(defclass quickpeek-semantic (quickpeek-language)
  ((local-context :initform nil
		  :documentation "Store the local context here.
The local context are variables that are in a local context, and won't
show up in a top level parse, but must be searched anyway.")
   )
  "Base class for semantic-supported language completion displays.")

;;; Methods:
(defmethod quickpeek-thing-bounds ((obj quickpeek-default) &optional thing)
  "For OBJ, get the bounds of the THING at/near point.
Uses `thing-at-point' library, version safe."
  (if (not thing) (setq thing 'sexp))
  (if (featurep 'thingatpt)
      (bounds-of-thing-at-point thing)
    ;; The xemacs version
    (let ((newsym (intern (concat "thing-" (symbol-name thing)))))
      (funcall newsym (point)))))

(defmethod quickpeek-thing ((obj quickpeek-default) &optional type)
  "For OBJ, get the THING at the current point."
  ;; Note: current-word is another good function
  (let ((b (quickpeek-thing-bounds obj type)))
    (if b
	(buffer-substring-no-properties (car b) (cdr b))
      nil)))

(defmethod quickpeek-thing-beginning ((obj quickpeek-default) &optional type)
  "For OBJ, move to the beginning of the THING point is on."
  (let ((b (quickpeek-thing-bounds obj type)))
    (goto-char (car b))))

(defmethod quickpeek-collect ((obj quickpeek-default))
  "Collect information from the current point.
Store it in OBJ for use during display."
  
  )

(defmethod quickpeek-display ((obj quickpeek-default))
  "Display information from the current context.
The information is stored in OBJ, and must be output and formatted
in the current buffer.
The default object has no purpose, so display YOW information."
  (yow 1)
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (while (re-search-forward "\\<\\([A-Z]+\\)\\>" nil t)
      (put-text-property (match-beginning 1) (match-end 1) 'face 'bold))))

;;; Language type
(defmethod quickpeek-beginning-of-defun ((obj quickpeek-language))
  "Go to the beginning of a defun.
Is safe against bad formatting."
  ;; This irritating combo works on multiple situations
  ;; and seems to be the best I can do at the moment.
  (beginning-of-line)
  (let ((p (point)))
    (condition-case nil
	(while t (up-list -1))
      (error))
    (if (= (point) p)
	;; In this case, we must assume beginning of buffer.
	(goto-char (point-min))))
  )

(defmethod quickpeek-current-function ((obj quickpeek-language))
  "Get the current function signature, and store it in OBJ."
  (save-excursion
    (quickpeek-beginning-of-defun obj)
    ;; Now parse the function out of this line:
    (beginning-of-line)
    (buffer-substring (point) (progn (end-of-line) (point)))
    ))

(defmethod quickpeek-save-current-thing ((obj quickpeek-language))
  "Get the current thing under/near point, and store it in OBJ."
  (quickpeek-thing obj)
  )

(defmethod quickpeek-collect ((obj quickpeek-language))
  "Collect information from the current point for OBJ."
  (oset obj current-thing (quickpeek-save-current-thing obj))
  (oset obj current-fn (quickpeek-current-function obj))
  )

(defmethod quickpeek-insert-context ((obj quickpeek-language))
  "Insert a description of our current context."
  (quickpeek-plain-string-insert (oref obj current-fn)))

(defmethod quickpeek-insert-description ((obj quickpeek-language))
  "Insert a description of OBJ's current thing."
  (quickpeek-plain-string-insert (list '("Thing: " bold)
				       (oref obj current-thing))))

(defmethod quickpeek-insert-completions ((obj quickpeek-language))
  "Insert a collection of completions for the OBJ's current thing."
  (let ((c (quickpeek-tags-completion (oref obj current-thing))))
    (quickpeek-insert-completions c)))

(defmethod quickpeek-display ((obj quickpeek-default))
  "Display information in a quickpeek buffer.
Assume the buffer is current."
  ;; Current context/function
  (quickpeek-insert-context (oref obj current-fn))
  (insert "\n")
  ;; Description of thing
  (quickpeek-insert-description obj)
  (insert "\n")
  ;; Completion for thing
  (quickpeek-insert-completions obj))


;;; Semantic support
(defmethod quickpeek-beginning-of-defun ((obj quickpeek-semantic))
  "Go to the beginning of defun."
  (let ((o (semantic-current-nonterminal)))
    (goto-char (semantic-token-start o))))

(defmethod quickpeek-current-function ((obj quickpeek-semantic))
  "Return information about the current function."
  ;; Optional force refresh here??
  (semantic-current-nonterminal))

(defmethod quickpeek-current-context ((obj quickpeek-semantic))
  "Return a list of all current local variables in semantic token form."
  nil
  )

(defmethod quickpeek-collect ((obj quickpeek-semantic))
  "Get the current context, in addition to language items."
  (call-next-method)
  (oset obj local-context (quickpeek-current-context obj)))

(defmethod quickpeek-insert-token ((obj quickpeek-semantic) token)
  "For OBJ, insert TOKEN into the quickpeek buffer.
Uses default token knowledge to perform colorization."
  )

(defmethod quickpeek-insert-context ((obj quickpeek-semantic))
  "Insert a description of our current defun."
  (quickpeek-insert-token obj (oref obj current-thing)))

(defmethod quickpeek-insert-description ((obj quickpeek-semantic))
  "Insert a description of OBJ's stored thing."
  (let ((lc (oref obj local-context)))
    (cond ((stringp lc) (insert lc))
	  ((semantic-token-p lc) (quickpeek-insert-token lc))
	  (t (insert (format "%S" lc)))))
  )

(defmethod quickpeek-calc-context-completions ((obj quickpeek-semantic))
  "Calculate special completions for OBJ.
Special completions exist if we are completing symbols in structs,
classes, or something of that nature."
  nil
  )

(defmethod quickpeek-insert-completions ((obj quickpeek-semantic))
  "Insert completions for OBJ's stored thing.
If OBJ has structure or type information for trimming back use that
to provide useful details.  If not, call the old method."
  (let ((c (quickpeek-calc-context-completions obj)))
    (if c
	(quickpeek-insert-completions c)
      (call-next-method))))

(provide 'qp-base)

;;; qp-base.el ends here
