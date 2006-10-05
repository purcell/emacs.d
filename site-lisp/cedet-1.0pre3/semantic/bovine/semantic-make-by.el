;;; semantic-make-by.el --- Generated parser support file

;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004 Eric M. Ludlam

;; Author: Steve Purcell <steve@Monaco.local>
;; Created: 2006-06-22 14:50:23+0200
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file make.by.

;;; History:
;;

;;; Code:

;;; Prologue
;;

;;; Declarations
;;
(defconst semantic-make-by--keyword-table
  (semantic-lex-make-keyword-table
   '(("if" . IF)
     ("ifdef" . IFDEF)
     ("ifndef" . IFNDEF)
     ("ifeq" . IFEQ)
     ("ifneq" . IFNEQ)
     ("else" . ELSE)
     ("endif" . ENDIF)
     ("include" . INCLUDE))
   '(("include" summary "Macro: include filename1 filename2 ...")
     ("ifneq" summary "Conditional: ifneq (expression) ... else ... endif")
     ("ifeq" summary "Conditional: ifeq (expression) ... else ... endif")
     ("ifndef" summary "Conditional: ifndef (expression) ... else ... endif")
     ("ifdef" summary "Conditional: ifdef (expression) ... else ... endif")
     ("endif" summary "Conditional: if (expression) ... else ... endif")
     ("else" summary "Conditional: if (expression) ... else ... endif")
     ("if" summary "Conditional: if (expression) ... else ... endif")))
  "Table of language keywords.")

(defconst semantic-make-by--token-table
  (semantic-lex-make-type-table
   '(("punctuation"
      (BACKSLASH . "\\`[\\]\\'")
      (DOLLAR . "\\`[$]\\'")
      (EQUAL . "\\`[=]\\'")
      (PLUS . "\\`[+]\\'")
      (COLON . "\\`[:]\\'")))
   'nil)
  "Table of lexical tokens.")

(defconst semantic-make-by--parse-table
  `(
    (bovine-toplevel 
     (Makefile)
     ) ;; end bovine-toplevel

    (Makefile
     (variable)
     (rule)
     (conditional)
     (include)
     (whitespace
      ,(semantic-lambda
	(list nil))
      )
     (newline
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end Makefile

    (variable
     (symbol
      opt-whitespace
      equals
      opt-whitespace
      element-list
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (nth 0 vals) nil
	 (nth 4 vals)))
      )
     ) ;; end variable

    (rule
     (targets
      opt-whitespace
      colons
      opt-whitespace
      element-list
      commands
      ,(semantic-lambda
	(semantic-tag-new-function
	 (nth 0 vals) nil
	 (nth 4 vals)))
      )
     ) ;; end rule

    (targets
     (target
      opt-whitespace
      targets
      ,(semantic-lambda
	(list
	 (car
	  (nth 0 vals))
	 (car
	  (nth 2 vals))))
      )
     (target
      ,(semantic-lambda
	(list
	 (car
	  (nth 0 vals))))
      )
     ) ;; end targets

    (target
     (sub-target
      target
      ,(semantic-lambda
	(list
	 (concat
	  (car
	   (nth 0 vals))
	  (car
	   (nth 2 vals)))))
      )
     (sub-target
      ,(semantic-lambda
	(list
	 (car
	  (nth 0 vals))))
      )
     ) ;; end target

    (sub-target
     (symbol)
     (string)
     (varref)
     ) ;; end sub-target

    (conditional
     (IF
      whitespace
      symbol
      newline
      ,(semantic-lambda
	(list nil))
      )
     (IFDEF
      whitespace
      symbol
      newline
      ,(semantic-lambda
	(list nil))
      )
     (IFNDEF
      whitespace
      symbol
      newline
      ,(semantic-lambda
	(list nil))
      )
     (IFEQ
      whitespace
      expression
      newline
      ,(semantic-lambda
	(list nil))
      )
     (IFNEQ
      whitespace
      expression
      newline
      ,(semantic-lambda
	(list nil))
      )
     (ELSE
      newline
      ,(semantic-lambda
	(list nil))
      )
     (ENDIF
      newline
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end conditional

    (expression
     (semantic-list)
     ) ;; end expression

    (include
     (INCLUDE
      whitespace
      element-list
      ,(semantic-lambda
	(semantic-tag-new-include
	 (nth 2 vals) nil))
      )
     ) ;; end include

    (equals
     (punctuation
      "\\`[:]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda)
      )
     (punctuation
      "\\`[+]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda)
      )
     (punctuation
      "\\`[=]\\'"
      ,(semantic-lambda)
      )
     ) ;; end equals

    (colons
     (punctuation
      "\\`[:]\\'"
      punctuation
      "\\`[:]\\'"
      ,(semantic-lambda)
      )
     (punctuation
      "\\`[:]\\'"
      ,(semantic-lambda)
      )
     ) ;; end colons

    (element-list
     (elements
      newline
      ,(semantic-lambda
	(nth 0 vals))
      )
     ) ;; end element-list

    (elements
     (element
      whitespace
      elements
      ,(semantic-lambda
	(nth 0 vals)
	(nth 2 vals))
      )
     (element
      ,(semantic-lambda
	(nth 0 vals))
      )
     ( ;;EMPTY
      )
     ) ;; end elements

    (element
     (sub-element
      element
      ,(semantic-lambda
	(list
	 (concat
	  (car
	   (nth 0 vals))
	  (car
	   (nth 1 vals)))))
      )
     ( ;;EMPTY
      )
     ) ;; end element

    (sub-element
     (symbol)
     (string)
     (punctuation)
     (semantic-list
      ,(semantic-lambda
	(list
	 (buffer-substring-no-properties
	  (identity start)
	  (identity end))))
      )
     ) ;; end sub-element

    (varref
     (punctuation
      "\\`[$]\\'"
      semantic-list
      ,(semantic-lambda
	(list
	 (buffer-substring-no-properties
	  (identity start)
	  (identity end))))
      )
     ) ;; end varref

    (commands
     (shell-command
      newline
      commands
      ,(semantic-lambda
	(list
	 (nth 0 vals))
	(nth 1 vals))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end commands

    (opt-whitespace
     (whitespace
      ,(semantic-lambda
	(list nil))
      )
     ( ;;EMPTY
      )
     ) ;; end opt-whitespace
    )
  "Parser table.")

(defun semantic-make-by--install-parser ()
  "Setup the Semantic Parser."
  (setq semantic--parse-table semantic-make-by--parse-table
	semantic-debug-parser-source "make.by"
	semantic-debug-parser-class 'semantic-bovine-debug-parser
	semantic-flex-keywords-obarray semantic-make-by--keyword-table
	))


;;; Analyzers
;;
(require 'semantic-lex)


;;; Epilogue
;;

(provide 'semantic-make-by)

;;; semantic-make-by.el ends here
