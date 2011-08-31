;;; semantic-scm-by.el --- Generated parser support file

;; Copyright (C) 2001, 2003 Eric M. Ludlam

;; Author: Mona <whisky@ubuntu.ubuntu-domain>
;; Created: 2011-05-20 16:21:00-0400
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file scheme.by.

;;; History:
;;

;;; Code:

;;; Prologue
;;

;;; Declarations
;;
(defconst semantic-scm-by--keyword-table
  (semantic-lex-make-keyword-table
   '(("define" . DEFINE)
     ("define-module" . DEFINE-MODULE)
     ("load" . LOAD))
   '(("load" summary "Function: (load \"filename\")")
     ("define-module" summary "Function: (define-module (name arg1 ...)) ")
     ("define" summary "Function: (define symbol expression)")))
  "Table of language keywords.")

(defconst semantic-scm-by--token-table
  (semantic-lex-make-type-table
   '(("close-paren"
      (CLOSEPAREN . ")"))
     ("open-paren"
      (OPENPAREN . "(")))
   'nil)
  "Table of lexical tokens.")

(defconst semantic-scm-by--parse-table
  `(
    (bovine-toplevel 
     (scheme)
     ) ;; end bovine-toplevel

    (scheme
     (semantic-list
      ,(lambda (vals start end)
	 (semantic-bovinate-from-nonterminal
	  (car
	   (nth 0 vals))
	  (cdr
	   (nth 0 vals))
	  'scheme-list))
      )
     ) ;; end scheme

    (scheme-list
     (open-paren
      "("
      scheme-in-list
      close-paren
      ")"
      ,(semantic-lambda
	(nth 1 vals))
      )
     ) ;; end scheme-list

    (scheme-in-list
     (DEFINE
       symbol
       expression
       ,(semantic-lambda
	 (semantic-tag-new-variable
	  (nth 1 vals) nil
	  (nth 2 vals)))
       )
     (DEFINE
       name-args
       opt-doc
       sequence
       ,(semantic-lambda
	 (semantic-tag-new-function
	  (car
	   (nth 1 vals)) nil
	  (cdr
	   (nth 1 vals))))
       )
     (DEFINE-MODULE
       name-args
       ,(semantic-lambda
	 (semantic-tag-new-package
	  (nth
	   (length
	    (nth 1 vals))
	   (nth 1 vals)) nil))
       )
     (LOAD
      string
      ,(semantic-lambda
	(semantic-tag-new-include
	 (file-name-nondirectory
	  (read
	   (nth 1 vals)))
	 (read
	  (nth 1 vals))))
      )
     (symbol
      ,(semantic-lambda
	(semantic-tag-new-code
	 (nth 0 vals) nil))
      )
     ) ;; end scheme-in-list

    (name-args
     (semantic-list
      ,(lambda (vals start end)
	 (semantic-bovinate-from-nonterminal
	  (car
	   (nth 0 vals))
	  (cdr
	   (nth 0 vals))
	  'name-arg-expand))
      )
     ) ;; end name-args

    (name-arg-expand
     (open-paren
      name-arg-expand
      ,(semantic-lambda
	(nth 1 vals))
      )
     (symbol
      name-arg-expand
      ,(semantic-lambda
	(cons
	 (nth 0 vals)
	 (nth 1 vals)))
      )
     ( ;;EMPTY
      ,(semantic-lambda)
      )
     ) ;; end name-arg-expand

    (opt-doc
     (string)
     ( ;;EMPTY
      )
     ) ;; end opt-doc

    (sequence
     (expression
      sequence)
     (expression)
     ) ;; end sequence

    (expression
     (symbol)
     (semantic-list)
     (string)
     ) ;; end expression
    )
  "Parser table.")

(defun semantic-scm-by--install-parser ()
  "Setup the Semantic Parser."
  (setq semantic--parse-table semantic-scm-by--parse-table
	semantic-debug-parser-source "scheme.by"
	semantic-debug-parser-class 'semantic-bovine-debug-parser
	semantic-flex-keywords-obarray semantic-scm-by--keyword-table
	))


;;; Analyzers
;;
(require 'semantic-lex)


;;; Epilogue
;;

(provide 'semantic-scm-by)

;;; semantic-scm-by.el ends here
