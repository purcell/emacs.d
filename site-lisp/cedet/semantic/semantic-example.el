;;; semanto-example.el --- Examples using the semantic API

;;; Copyright (C) 2002, 2003, 2004, 2005 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-example.el,v 1.6 2005/09/30 20:20:07 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
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

;;; Commentary:
;;
;; Simplified example programs that use the semantic API.

;;; Code:
(defun se-jump (name)
  "Jump to the token with NAME.
Shows how to:
 * Interactivly finding a token name from a stream.
 * Getting symbols under point.
 * Getting the main token list
 * Finding a token by name.
 * Highlighting a token"
  (interactive
   (list
    ;; Helpful way of interactivly querying for a token name.
    (semantic-read-symbol
     "Jump to: "
     ;; The default can be the text under the cursor.
     ;; It returns a list such that:
     ;;  this.that.th
     ;; is
     ;; ( "this" "that" "th" )
     (car (reverse (semantic-ctxt-current-symbol)))
     ;; The stream is from this buffer
     (semantic-fetch-tags)
     ;; We don't need a filter.
     nil )))
  (let (tok pos)
    ;; Search for the token
    (setq tok (semantic-find-first-tag-by-name
	       name
	       ;; Get the stream.  May be a buffer too.
	       (semantic-fetch-tags)
	       ;; Look inside structures
	       t
	       ;; Don't search include files. Perhaps a later
	       ;; revision?
	       nil))
    (if tok
	(progn
	  ;; Get the position of the token.
	  (setq pos (semantic-tag-start tok))
	  ;; Jump.
	  ;; Because we only searched this buffer, we can assume the token is
	  ;; also in this buffer.
	  (goto-char pos)
	  ;; Fancy stuff: Hightlight the token we jumped to.
	  ;; This will automatically unhighlight the token later.
	  (semantic-momentary-highlight-token tok)
	  )
      (error "No tag %s found" name)
      )))

(defcustom se-summary-function 'semantic-format-tag-uml-prototype
  "*Function to use when showing info about a token for Examples.
Shows how to:
  * Create a customization variable."
  :group 'semantic
  :type semantic-format-tag-custom-list)

(defun se-show (name)
  "Show important information about some token with NAME.
Shows how to:
 * Create a variable with `semantic-format-tag-functions'.
 * Create the colorized text."
  (interactive (list (semantic-read-symbol "Symbol: ")))
  (let ((tok (semantic-find-first-tag-by-name name (current-buffer) t))
	parent
	msg)
    (if tok
	(save-excursion
	  ;; Token text functions can take a PARENT argument.  To find this
	  ;; really quickly, jump to TOK, then use the overlay mechanism to
	  ;; Find it.
	  (goto-char (semantic-tag-start tok))
	  (setq parent (semantic-current-tag-parent))
	  ;; Create a message
	  (setq msg (funcall se-summary-function tok parent t))
	  ;; Display it
	  (message msg)))))

(defun se-db-apropos-show (name)
  "Show information about several tokens matching NAME using semanticdb.
Shows how to:
  * Query the database.
  * Decode the databases' output."
  (interactive
   (list (read-string "Symbol: "
		      ;; Current symbol is the default
		      (car (reverse (semantic-ctxt-current-symbol))))))
  (let ((tok (semanticdb-deep-find-tags-by-name-regexp
	      name
	      ;; The current list of active databases for this file.
	      ;; This is the same as passing `nil'
	      (semanticdb-current-database-list)
	      t   ;; When a token is found, make sure it is
	          ;; an active buffer.
	      )))
    ;; Each entry in the database is a list
    ;; starting with the database a token was found in,
    ;; followed by the tokens found.
    (message
     (mapconcat
      ;; Outer lambda: Decode the database search format.
      (lambda (db-list)
	(mapconcat
	 ;; Inner lambda: Convert found tokens into text.
	 (lambda (tok)
	   (funcall se-summary-function
		    tok
		    nil
		    t))
	 ;; The first element is the database.  The second is a token.
	 (cdr db-list)
	 "\n"))
      tok
      "\n"))
    ))


(provide 'semantic-example)

;;; semantic-example.el ends here
