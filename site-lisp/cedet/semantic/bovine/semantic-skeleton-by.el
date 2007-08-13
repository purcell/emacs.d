;;; semantic-skeleton-by.el --- Generated parser support file

;; Copyright (C) 2003 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Created: 2003-08-01 15:51:49+0200
;; Keywords: syntax
;; X-RCS: $Id: semantic-skeleton-by.el,v 1.2 2005/09/30 20:22:37 zappo Exp $
;;
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
;; generated from the grammar file skeleton.by.

;;; History:
;;

;;; Code:

;;; Prologue
;;

;;; Declarations
;;
(defconst semantic-skeleton-by--keyword-table
  (semantic-lex-make-keyword-table
   '(("something" . SOMETHING))
   '(("something" summary "Describe something")))
  "Table of language keywords.")

(defconst semantic-skeleton-by--token-table
  (semantic-lex-make-type-table
   '(("punctuation"
      (OPERATORS . "[-+*/%^|&]")
      (PERIOD . "\\b[.]\\b")))
   'nil)
  "Table of lexical tokens.")

(defconst semantic-skeleton-by--parse-table
  `(
    (bovine-toplevel ;;starting_rule
     (something)
     ) ;; end starting_rule

    (bovine-inner-scope ;;start_scope_rule
     (something_else)
     ) ;; end start_scope_rule

    (something
     (SOMETHING)
     ) ;; end something

    (opt-expression
     (expression)
     ( ;;EMPTY
      )
     ) ;; end opt-expression

    (expression
     (number
      ,(semantic-lambda)
      )
     (symbol
      ,(semantic-lambda)
      )
     (string
      ,(semantic-lambda)
      )
     (semantic-list
      ,(semantic-lambda)
      )
     (punctuation
      "[-+*/%^|&]"
      expression)
     ) ;; end expression
    )
  "Parser table.")

(defun semantic-skeleton-by--install-parser ()
  "Setup the Semantic Parser."
  (setq semantic-toplevel-bovine-table semantic-skeleton-by--parse-table
        semantic-debug-parser-source "skeleton.by"
        semantic-debug-parser-class 'semantic-bovine-debug-parser
        semantic-flex-keywords-obarray semantic-skeleton-by--keyword-table
        ))


;;; Epilogue
;;

(provide 'semantic-skeleton-by)

;;; semantic-skeleton-by.el ends here
