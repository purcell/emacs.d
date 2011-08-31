;;; semantic-erlang-by.el --- Generated parser support file

;; Copyright (C) 2002, 2003 Vladimir G. Sekissov

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
;; generated from the grammar file erlang.by.

;;; History:
;;

;;; Code:

;;; Prologue
;;

;;; Declarations
;;
(defconst semantic-erlang-by--keyword-table
  (semantic-lex-make-keyword-table
   '(("begin" . BEGIN)
     ("end" . END)
     ("case" . CASE)
     ("of" . OF)
     ("if" . IF)
     ("when" . WHEN)
     ("true" . TRUE)
     ("receive" . RECEIVE)
     ("after" . AFTER)
     ("or" . OR)
     ("orelse" . ORELSE)
     ("xor" . XOR)
     ("bor" . BOR)
     ("bxor" . BXOR)
     ("bsl" . BSL)
     ("bsr" . BSR)
     ("div" . DIV)
     ("rem" . REM)
     ("band" . BAND)
     ("and" . AND)
     ("andalso" . ANDALSO)
     ("bnot" . BNOT)
     ("not" . NOT)
     ("catch" . CATCH)
     ("fun" . FUN)
     ("query" . QUERY)
     ("let" . LET)
     ("module" . MODULE)
     ("include" . INCLUDE)
     ("include_lib" . INCLUDE_LIB)
     ("behaviour" . BEHAVIOUR)
     ("define" . DEFINE)
     ("undef" . UNDEF)
     ("ifdef" . IFDEF)
     ("ifndef" . IFNDEF)
     ("else" . ELSE)
     ("endif" . ENDIF)
     ("export" . EXPORT)
     ("import" . IMPORT)
     ("record" . RECORD)
     ("signed" . SIGNED)
     ("unsigned" . UNSIGNED))
   'nil)
  "Table of language keywords.")

(defconst semantic-erlang-by--token-table
  (semantic-lex-make-type-table
   '(("close-paren"
      (RBRACE . "}")
      (RPAREN . ")"))
     ("open-paren"
      (LBRACE . "{")
      (LPAREN . "("))
     ("semantic-list"
      (BRACE_BLCK . "^{")
      (BRACK_BLCK . "^\\[")
      (PAREN_BLCK . "^("))
     ("symbol"
      (UNIT . "unit")
      (NATIVE . "native")
      (LITTLE . "little")
      (BIG . "big")
      (BINARY . "binary")
      (FLOAT . "float")
      (INTEGER . "integer")
      (FILE . "file")
      (USCORE . "_")
      (VAR2 . "^[A-Z][_a-zA-Z0-9]*")
      (VAR1 . "^_[a-zA-Z0-9]+")
      (INT2 . "[0-9]\\{1,2\\}")
      (INT1 . "[0-9a-eA-E]+")
      (EE . "[eE]")
      (DIGITS . "[0-9]+")
      (ATOM2 . "'.+'")
      (ATOM1 . "[a-z][a-zA-Z0-9_@]*"))
     ("punctuation"
      (SIGN . "[-+]")
      (VDEL . "\\`[|]\\'")
      (COMA . "\\`[,]\\'")
      (GREATER . "\\`[>]\\'")
      (LESS . "\\`[<]\\'")
      (EQUAL . "\\`[=]\\'")
      (WHY . "\\`[?]\\'")
      (BANG . "\\`[!]\\'")
      (MINUS . "\\`[-]\\'")
      (PLUS . "\\`[+]\\'")
      (DIVIDE . "\\`[/]\\'")
      (AMPERSAND . "\\`[&]\\'")
      (STAR . "\\`[*]\\'")
      (SEMICOLON . "\\`[;]\\'")
      (COLON . "\\`[:]\\'")
      (PERIOD . "\\`[.]\\'")
      (HASH . "\\`[#]\\'")))
   'nil)
  "Table of lexical tokens.")

(defconst semantic-erlang-by--parse-table
  `(
    (bovine-toplevel 
     (module-decl)
     ) ;; end bovine-toplevel

    (add-op
     (punctuation
      "\\`[+]\\'")
     (punctuation
      "\\`[-]\\'")
     (BOR)
     (BXOR)
     (BSL)
     (BSR)
     ) ;; end add-op

    (list-conc-op
     (punctuation
      "\\`[+]\\'"
      punctuation
      "\\`[+]\\'")
     (punctuation
      "\\`[-]\\'"
      punctuation
      "\\`[-]\\'")
     ) ;; end list-conc-op

    (comp-op
     (punctuation
      "\\`[=]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "=="))
      )
     (punctuation
      "\\`[=]\\'"
      punctuation
      "\\`[:]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "=:="))
      )
     (punctuation
      "\\`[=]\\'"
      punctuation
      "\\`[/]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "=/="))
      )
     (punctuation
      "\\`[/]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 "/="))
      )
     (punctuation
      "\\`[=]\\'"
      punctuation
      "\\`[<]\\'"
      ,(semantic-lambda
	(list
	 "=<"))
      )
     (punctuation
      "\\`[>]\\'"
      punctuation
      "\\`[=]\\'"
      ,(semantic-lambda
	(list
	 ">="))
      )
     (punctuation
      "\\`[<]\\'")
     (punctuation
      "\\`[>]\\'")
     (OR)
     (ORELSE)
     (XOR)
     (AND)
     (ANDALSO)
     ) ;; end comp-op

    (multi-op
     (punctuation
      "\\`[*]\\'")
     (punctuation
      "\\`[/]\\'")
     (DIV)
     (REM)
     (BAND)
     ) ;; end multi-op

    (prefix-op
     (punctuation
      "\\`[+]\\'")
     (punctuation
      "\\`[-]\\'")
     (BNOT)
     (NOT)
     ) ;; end prefix-op

    (basic-type
     (float-literal)
     (integer-literal)
     (char-literal)
     (atom)
     (var)
     (string)
     (TRUE)
     ) ;; end basic-type

    (atom
     (symbol
      "[a-z][a-zA-Z0-9_@]*"
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     (symbol
      "'.+'"
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end atom

    (float-literal
     (symbol
      "[0-9]+"
      punctuation
      "\\`[.]\\'"
      symbol
      "[0-9]+"
      exp-part)
     (punctuation
      "\\`[.]\\'"
      symbol
      "[0-9]+"
      exp-part)
     ) ;; end float-literal

    (exp-part
     (symbol
      "[eE]"
      punctuation
      "[-+]"
      symbol
      "[0-9]+")
     (symbol
      "[eE]"
      symbol
      "[0-9]+")
     ( ;;EMPTY
      )
     ) ;; end exp-part

    (integer-literal
     (symbol
      "[0-9a-eA-E]+")
     (symbol
      "[0-9]\\{1,2\\}"
      punctuation
      "\\`[#]\\'"
      symbol
      "[0-9a-eA-E]+")
     ) ;; end integer-literal

    (char-literal
     (CHAR)
     ) ;; end char-literal

    (var
     (symbol
      "^_[a-zA-Z0-9]+"
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (nth 0 vals) nil nil))
      )
     (symbol
      "^[A-Z][_a-zA-Z0-9]*"
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (nth 0 vals) nil nil))
      )
     ) ;; end var

    (uni-pattern
     (symbol
      "_")
     ) ;; end uni-pattern

    (binary
     (punctuation
      "\\`[<]\\'"
      punctuation
      "\\`[<]\\'"
      punctuation
      "\\`[>]\\'"
      punctuation
      "\\`[>]\\'"
      ,(semantic-lambda
	(semantic-tag
	 "<<>>"
	 'binary))
      )
     (punctuation
      "\\`[<]\\'"
      punctuation
      "\\`[<]\\'"
      binary-segments
      punctuation
      "\\`[>]\\'"
      punctuation
      "\\`[>]\\'"
      ,(semantic-lambda
	(semantic-tag
	 "<<Binary>>"
	 'binary))
      )
     ) ;; end binary

    (binary-segments
     (binary-segment
      binary-segments-rest)
     ) ;; end binary-segments

    (binary-segments-rest
     (punctuation
      "\\`[,]\\'"
      binary-segments
      ,(semantic-lambda
	(nth 1 vals))
      )
     ( ;;EMPTY
      )
     ) ;; end binary-segments-rest

    (binary-segment
     (basic-type
      binary-segment-rest)
     (clause-pattern
      binary-segment-rest)
     ) ;; end binary-segment

    (binary-segment-rest
     (punctuation
      "\\`[:]\\'"
      basic-type
      punctuation
      "\\`[/]\\'"
      binary-type-spec-list)
     (punctuation
      "\\`[:]\\'"
      basic-type)
     (punctuation
      "\\`[/]\\'"
      binary-type-spec-list)
     ( ;;EMPTY
      )
     ) ;; end binary-segment-rest

    (binary-type
     (symbol
      "integer")
     (symbol
      "float")
     (symbol
      "binary")
     ) ;; end binary-type

    (binary-signed
     (SIGNED)
     (UNSIGNED)
     ) ;; end binary-signed

    (binary-endian
     (symbol
      "big")
     (symbol
      "little")
     (symbol
      "native")
     ) ;; end binary-endian

    (binary-unit
     (symbol
      "unit"
      punctuation
      "\\`[:]\\'"
      basic-type)
     ) ;; end binary-unit

    (binary-type-spec
     (binary-type)
     (binary-signed)
     (binary-endian)
     (binary-unit)
     ) ;; end binary-type-spec

    (binary-type-spec-list
     (binary-type-spec
      punctuation
      "\\`[-]\\'"
      binary-type-spec-list)
     (binary-type-spec)
     ) ;; end binary-type-spec-list

    (module-decl
     (module-attr)
     (function-decl)
     (header-form)
     (directive)
     (file-attr)
     ) ;; end module-decl

    (module-attr
     (punctuation
      "\\`[-]\\'"
      MODULE
      semantic-list
      "^("
      full-stop
      ,(semantic-lambda
	(semantic-tag-new-package
	 (car
	  (semantic-bovinate-from-nonterminal
	   (car
	    (nth 2 vals))
	   (cdr
	    (nth 2 vals))
	   'module-attr-name)) nil))
      )
     ) ;; end module-attr

    (module-attr-name
     (open-paren
      "("
      module-name
      close-paren
      ")"
      ,(semantic-lambda
	(list
	 (car
	  (nth 1 vals))))
      )
     ) ;; end module-attr-name

    (module-name
     (atom
      module-name-rest
      ,(semantic-lambda
	(list
	 (concat
	  (car
	   (nth 0 vals))
	  (car
	   (nth 1 vals)))))
      )
     (module-name-rest
      ,(semantic-lambda
	(list
	 (car
	  (nth 0 vals))))
      )
     ) ;; end module-name

    (module-name-rest
     (punctuation
      "\\`[.]\\'"
      atom
      module-name-rest
      ,(semantic-lambda
	(list
	 (concat
	  (nth 0 vals)
	  (car
	   (nth 1 vals))
	  (car
	   (nth 2 vals)))))
      )
     ( ;;EMPTY
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end module-name-rest

    (header-form
     (header-attr)
     (anywhere-attr)
     ) ;; end header-form

    (header-attr
     (export-attr)
     (import-attr)
     (compile-attr)
     (whild-attr)
     ) ;; end header-attr

    (anywhere-attr
     (file-attr)
     (macro-def)
     (record-decl)
     ) ;; end anywhere-attr

    (export-attr
     (punctuation
      "\\`[-]\\'"
      EXPORT
      semantic-list
      "^("
      full-stop)
     ) ;; end export-attr

    (export-name-list
     (open-paren
      semantic-list
      "^\\["
      close-paren)
     ) ;; end export-name-list

    (import-attr
     (punctuation
      "\\`[-]\\'"
      IMPORT
      semantic-list
      "^("
      full-stop
      ,(semantic-lambda
	(semantic-tag
	 (car
	  (semantic-bovinate-from-nonterminal
	   (car
	    (nth 2 vals))
	   (cdr
	    (nth 2 vals))
	   'import-name-list))
	 'import))
      )
     ) ;; end import-attr

    (import-name-list
     (open-paren
      module-name
      punctuation
      "\\`[,]\\'"
      semantic-list
      "^\\["
      close-paren
      ,(semantic-lambda
	(nth 1 vals)
	(list
	 (semantic-bovinate-from-nonterminal
	  (car
	   (nth 3 vals))
	  (cdr
	   (nth 3 vals))
	  'function-name-list)))
      )
     (open-paren
      module-name
      close-paren
      ,(semantic-lambda
	(nth 1 vals))
      )
     ) ;; end import-name-list

    (function-name-list
     (open-paren
      close-paren
      ,(semantic-lambda
	(list nil))
      )
     (open-paren
      function-names
      close-paren
      ,(semantic-lambda
	(list
	 (nth 1 vals)))
      )
     ) ;; end function-name-list

    (function-names
     (function-arity
      punctuation
      "\\`[,]\\'"
      function-names
      ,(semantic-lambda
	(list
	 (nth 0 vals)
	 (nth 2 vals)))
      )
     (function-arity
      ,(semantic-lambda
	(list
	 (nth 0 vals)))
      )
     ) ;; end function-names

    (function-arity
     (atom
      punctuation
      "\\`[/]\\'"
      integer-literal
      ,(semantic-lambda
	(nth 0 vals)
	(list
	 (nth 1 vals))
	(nth 2 vals))
      )
     ) ;; end function-arity

    (compile-attr
     (punctuation
      "\\`[-]\\'"
      COMPILE
      semantic-list
      "^("
      full-stop)
     ) ;; end compile-attr

    (file-attr
     (punctuation
      "\\`[-]\\'"
      symbol
      "file"
      semantic-list
      "^("
      full-stop)
     ) ;; end file-attr

    (file-attr-list
     (open-paren
      string
      punctuation
      "\\`[,]\\'"
      integer-literal
      close-paren
      ,(semantic-lambda
	(list
	 (cons
	  (read
	   (nth 1 vals))
	  (nth 3 vals))))
      )
     ) ;; end file-attr-list

    (whild-attr
     (punctuation
      "\\`[-]\\'"
      atom
      semantic-list
      "^("
      full-stop)
     ) ;; end whild-attr

    (function-decl
     (function-clauses
      full-stop
      ,(semantic-lambda
	(nth 0 vals))
      )
     ) ;; end function-decl

    (function-clauses
     (function-clause
      function-clauses-rest
      ,(semantic-lambda
	(nth 0 vals))
      )
     ) ;; end function-clauses

    (function-clauses-rest
     (punctuation
      "\\`[;]\\'"
      function-clauses)
     ( ;;EMPTY
      )
     ) ;; end function-clauses-rest

    (function-clause
     (atom
      fun-clause
      ,(semantic-lambda
	(semantic-tag-new-function
	 (concat
	  (car
	   (nth 0 vals))
	  "/"
	  (number-to-string
	   (length
	    (car
	     (nth 1 vals))))) nil
	 (nth 1 vals)))
      )
     ) ;; end function-clause

    (record-decl
     (punctuation
      "\\`[-]\\'"
      RECORD
      semantic-list
      "^("
      full-stop
      ,(semantic-lambda
	(car
	 (semantic-parse-region
	  (car
	   (nth 2 vals))
	  (cdr
	   (nth 2 vals))
	  'record-def
	  1)))
      )
     ) ;; end record-decl

    (record-def
     (open-paren
      atom
      punctuation
      "\\`[,]\\'"
      semantic-list
      "^{"
      close-paren
      ,(semantic-lambda
	(semantic-tag-new-type
	 (nth 1 vals)
	 "record"
	 (semantic-parse-region
	  (car
	   (nth 3 vals))
	  (cdr
	   (nth 3 vals))
	  'record-field-decl
	  1) nil))
      )
     ) ;; end record-def

    (record-decl-tuple
     (open-paren
      record-field-decls
      close-paren
      ,(semantic-lambda
	(list
	 (nth 1 vals)))
      )
     (open-paren
      close-paren
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end record-decl-tuple

    (record-field-decl
     (atom
      record-field-value
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (nth 0 vals) nil
	 ""))
      )
     (atom
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (nth 0 vals) nil
	 ""))
      )
     (open-paren
      "{"
      ,(semantic-lambda
	(list nil))
      )
     (close-paren
      "}"
      ,(semantic-lambda
	(list nil))
      )
     (punctuation
      "\\`[,]\\'"
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end record-field-decl

    (pattern
     (pattern-expr
      ,(semantic-lambda
	(nth 0 vals))
      )
     (uni-pattern)
     (binary)
     (basic-type)
     (semantic-list
      "^\\["
      ,(semantic-lambda
	(semantic-tag
	 "List"
	 'list))
      )
     (semantic-list
      "^{"
      ,(semantic-lambda
	(semantic-tag
	 "Tuple"
	 'list))
      )
     (record-pattern
      ,(semantic-lambda
	(nth 0 vals))
      )
     ) ;; end pattern

    (pattern-expr
     (pattern-conc-expr
      pattern-conc-expr-rest
      ,(semantic-lambda
	(nth 0 vals))
      )
     ) ;; end pattern-expr

    (pattern-conc-expr
     (string-literal)
     (var)
     ) ;; end pattern-conc-expr

    (pattern-conc-expr-rest
     (list-conc-op
      pattern-expr
      ,(semantic-lambda)
      )
     ( ;;EMPTY
      )
     ) ;; end pattern-conc-expr-rest

    (tuple-pattern
     (open-paren
      patterns
      close-paren)
     (open-paren
      close-paren
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end tuple-pattern

    (list-pattern
     (open-paren
      patterns
      list-pattern-tail
      close-paren)
     (open-paren
      close-paren
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end list-pattern

    (list-pattern-tail
     (punctuation
      "\\`[|]\\'"
      pattern)
     ( ;;EMPTY
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end list-pattern-tail

    (patterns
     (pattern
      patterns-rest
      ,(semantic-lambda
	(list
	 (cons
	  (car
	   (nth 0 vals))
	  (car
	   (nth 1 vals)))))
      )
     ) ;; end patterns

    (patterns-rest
     (punctuation
      "\\`[,]\\'"
      patterns
      ,(semantic-lambda
	(nth 1 vals))
      )
     ( ;;EMPTY
      )
     ) ;; end patterns-rest

    (record-pattern
     (punctuation
      "\\`[#]\\'"
      atom
      semantic-list
      "^{"
      ,(semantic-lambda
	(list
	 (car
	  (nth 1 vals))))
      )
     ) ;; end record-pattern

    (record-pattern-tuple
     (open-paren
      record-field-patterns
      close-paren
      ,(semantic-lambda
	(list
	 (nth 1 vals)))
      )
     (open-paren
      close-paren
      ,(semantic-lambda
	(list nil))
      )
     ) ;; end record-pattern-tuple

    (record-field-patterns
     (record-field-patterns
      punctuation
      "\\`[,]\\'"
      record-field-pattern)
     (record-field-pattern)
     ) ;; end record-field-patterns

    (record-field-pattern
     (atom
      punctuation
      "\\`[=]\\'"
      pattern)
     ) ;; end record-field-pattern

    (body
     (exprs)
     ) ;; end body

    (exprs
     (expr
      exprs-rest)
     ) ;; end exprs

    (exprs-rest
     (punctuation
      "\\`[,]\\'"
      exprs)
     ( ;;EMPTY
      )
     ) ;; end exprs-rest

    (expr
     (CATCH
      expr)
     (match-expr)
     ) ;; end expr

    (match-expr
     (pattern
      punctuation
      "\\`[=]\\'"
      match-expr)
     (send-expr)
     ) ;; end match-expr

    (send-expr
     (compare-expr
      send-expr-rest)
     ) ;; end send-expr

    (send-expr-rest
     (punctuation
      "\\`[!]\\'"
      send-expr)
     ( ;;EMPTY
      )
     ) ;; end send-expr-rest

    (compare-expr
     (list-conc-expr
      compare-expr-rest)
     ) ;; end compare-expr

    (compare-expr-rest
     (comp-op
      list-conc-expr)
     ( ;;EMPTY
      )
     ) ;; end compare-expr-rest

    (list-conc-expr
     (add-expr
      list-conc-expr-rest)
     ) ;; end list-conc-expr

    (list-conc-expr-rest
     (list-conc-op
      list-conc-expr)
     ( ;;EMPTY
      )
     ) ;; end list-conc-expr-rest

    (add-expr
     (multi-expr
      add-expr-rest)
     ) ;; end add-expr

    (add-expr-rest
     (add-op
      add-expr)
     ( ;;EMPTY
      )
     ) ;; end add-expr-rest

    (multi-expr
     (prefix-expr
      multi-expr-rest)
     ) ;; end multi-expr

    (multi-expr-rest
     (multi-op
      multi-expr)
     ( ;;EMPTY
      )
     ) ;; end multi-expr-rest

    (prefix-expr
     (prefix-op
      record-expr)
     (record-expr)
     ) ;; end prefix-expr

    (record-expr
     (punctuation
      "\\`[#]\\'"
      record-expr-field)
     (application-expr
      record-expr-rest)
     ) ;; end record-expr

    (record-expr-rest
     (punctuation
      "\\`[#]\\'"
      record-expr-field)
     ( ;;EMPTY
      )
     ) ;; end record-expr-rest

    (record-expr-field
     (atom
      punctuation
      "\\`[.]\\'"
      atom)
     (atom
      semantic-list
      "^{")
     (record-expr)
     ) ;; end record-expr-field

    (record-update-tuple
     (open-paren
      close-paren)
     (open-paren
      record-field-updates
      close-paren)
     ) ;; end record-update-tuple

    (record-field-updates
     (record-field-update
      record-field-updates-rest)
     ) ;; end record-field-updates

    (record-field-updates-rest
     (punctuation
      "\\`[,]\\'"
      record-field-updates)
     ( ;;EMPTY
      )
     ) ;; end record-field-updates-rest

    (record-field-update
     (atom
      record-field-value)
     ) ;; end record-field-update

    (record-field-value
     (punctuation
      "\\`[=]\\'"
      expr)
     ( ;;EMPTY
      )
     ) ;; end record-field-value

    (application-expr
     (module-name
      punctuation
      "\\`[:]\\'"
      primary-expr
      semantic-list
      "^(")
     (primary-expr
      application-expr-rest)
     ) ;; end application-expr

    (application-expr-rest
     (semantic-list
      "^(")
     (punctuation
      "\\`[:]\\'"
      primary-expr
      semantic-list
      "^(")
     ( ;;EMPTY
      )
     ) ;; end application-expr-rest

    (application-expr-list
     (open-paren
      close-paren)
     (open-paren
      exprs
      close-paren)
     ) ;; end application-expr-list

    (primary-expr
     (binary)
     (string-literal)
     (basic-type)
     (semantic-list
      "^{")
     (semantic-list
      "^\\[")
     (semantic-list
      "^\\[")
     (block-expr)
     (if-expr)
     (case-expr)
     (receive-expr)
     (fun-expr)
     (query-expr)
     (paren-expr)
     ) ;; end primary-expr

    (string-literal
     (string)
     (macro-app)
     (string
      string-literal)
     ) ;; end string-literal

    (tuple-skel
     (open-paren
      close-paren)
     (open-paren
      exprs
      close-paren)
     ) ;; end tuple-skel

    (list-skel
     (open-paren
      close-paren)
     (open-paren
      exprs
      list-skel-tail
      close-paren)
     ) ;; end list-skel

    (list-skel-tail
     (punctuation
      "\\`[|]\\'"
      expr)
     ( ;;EMPTY
      )
     ) ;; end list-skel-tail

    (list-compr
     (open-paren
      expr
      punctuation
      "\\`[|]\\'"
      punctuation
      "\\`[|]\\'"
      list-compr-exprs
      close-paren)
     ) ;; end list-compr

    (list-compr-exprs
     (list-compr-expr
      list-compr-exprs-rest)
     ) ;; end list-compr-exprs

    (list-compr-exprs-rest
     (punctuation
      "\\`[,]\\'"
      list-compr-exprs)
     ( ;;EMPTY
      )
     ) ;; end list-compr-exprs-rest

    (list-compr-expr
     (generator)
     (filter)
     ) ;; end list-compr-expr

    (generator
     (pattern
      punctuation
      "\\`[<]\\'"
      punctuation
      "\\`[-]\\'"
      expr)
     ) ;; end generator

    (filter
     (expr)
     ) ;; end filter

    (block-expr
     (BEGIN
      body
      END)
     ) ;; end block-expr

    (if-expr
     (IF
      if-clauses
      END)
     ) ;; end if-expr

    (if-clauses
     (if-clause
      punctuation
      "\\`[;]\\'"
      if-clauses)
     (if-clause)
     ) ;; end if-clauses

    (if-clause
     (guard
      clause-body)
     (expr
      clause-body)
     ) ;; end if-clause

    (clause-body
     (punctuation
      "\\`[-]\\'"
      punctuation
      "\\`[>]\\'"
      body)
     ) ;; end clause-body

    (case-expr
     (CASE
      expr
      OF
      cr-clauses
      END)
     ) ;; end case-expr

    (cr-clauses
     (cr-clause
      punctuation
      "\\`[;]\\'"
      cr-clauses)
     (cr-clause)
     ) ;; end cr-clauses

    (cr-clause
     (clause-pattern
      clause-guard
      clause-body)
     ) ;; end cr-clause

    (clause-guard
     (WHEN
      guard)
     ( ;;EMPTY
      )
     ) ;; end clause-guard

    (receive-expr
     (RECEIVE
      cr-clauses
      receive-after
      END)
     (RECEIVE
      receive-after
      END)
     (RECEIVE
      cr-clauses
      AFTER
      expr
      clause-body
      END)
     ) ;; end receive-expr

    (receive-after
     (AFTER
      expr
      clause-body)
     ( ;;EMPTY
      )
     ) ;; end receive-after

    (fun-expr
     (FUN
      function-arity)
     (FUN
      fun-clauses
      END)
     ) ;; end fun-expr

    (fun-clauses
     (fun-clause
      fun-clauses-rest)
     ) ;; end fun-clauses

    (fun-clauses-rest
     (punctuation
      "\\`[;]\\'"
      fun-clauses)
     ( ;;EMPTY
      )
     ) ;; end fun-clauses-rest

    (fun-clause
     (semantic-list
      "^("
      clause-guard
      clause-body
      ,(semantic-lambda
	(car
	 (semantic-bovinate-from-nonterminal
	  (car
	   (nth 0 vals))
	  (cdr
	   (nth 0 vals))
	  'clause-pattern-list)))
      )
     ) ;; end fun-clause

    (clause-pattern-list
     (open-paren
      clause-patterns
      close-paren
      ,(semantic-lambda
	(list
	 (nth 1 vals)))
      )
     ) ;; end clause-pattern-list

    (clause-patterns
     (clause-pattern
      clause-patterns-rest
      ,(semantic-lambda
	(list
	 (cons
	  (car
	   (nth 0 vals))
	  (car
	   (nth 1 vals)))))
      )
     ) ;; end clause-patterns

    (clause-patterns-rest
     (punctuation
      "\\`[,]\\'"
      clause-patterns
      ,(semantic-lambda
	(nth 1 vals))
      )
     ( ;;EMPTY
      )
     ) ;; end clause-patterns-rest

    (clause-pattern
     (match-pattern)
     (pattern)
     ) ;; end clause-pattern

    (match-pattern
     (var
      punctuation
      "\\`[=]\\'"
      pattern
      ,(semantic-lambda
	(nth 0 vals))
      )
     (pattern
      punctuation
      "\\`[=]\\'"
      var
      ,(semantic-lambda
	(nth 0 vals))
      )
     ) ;; end match-pattern

    (query-expr
     (QUERY
      semantic-list
      "^\\["
      END)
     ) ;; end query-expr

    (paren-expr
     (semantic-list
      "^(")
     ) ;; end paren-expr

    (paren-expr-list
     (open-paren
      expr
      close-paren)
     ) ;; end paren-expr-list

    (guard
     (guard-test
      guard-rest)
     ) ;; end guard

    (guard-rest
     (punctuation
      "\\`[,]\\'"
      guard)
     (punctuation
      "\\`[;]\\'"
      guard)
     ( ;;EMPTY
      )
     ) ;; end guard-rest

    (guard-test
     (TRUE)
     (guard-record-test)
     (guard-term-cmp)
     (guard-recognizer)
     (semantic-list
      "^(")
     ) ;; end guard-test

    (guard-record-test
     (RECORD
      semantic-list
      "^(")
     (open-paren
      guard-expr
      punctuation
      "\\`[,]\\'"
      symbol
      close-paren)
     ) ;; end guard-record-test

    (guard-recognizer
     (symbol
      semantic-list
      "^(")
     ) ;; end guard-recognizer

    (guard-term-cmp
     (guard-expr
      guard-term-op
      guard-expr)
     ) ;; end guard-term-cmp

    (guard-term-op
     (comp-op)
     (punctuation
      "\\`[=]\\'")
     ) ;; end guard-term-op

    (paren-guard-test
     (open-paren
      guard-test
      close-paren)
     ) ;; end paren-guard-test

    (guard-expr
     (guard-add-expr)
     ) ;; end guard-expr

    (guard-add-expr
     (guard-multi-expr
      guard-add-expr-rest)
     ) ;; end guard-add-expr

    (guard-add-expr-rest
     (add-op
      guard-add-expr)
     ( ;;EMPTY
      )
     ) ;; end guard-add-expr-rest

    (guard-multi-expr
     (guard-prefix-expr
      guard-multi-expr-rest)
     ) ;; end guard-multi-expr

    (guard-multi-expr-rest
     (multi-op
      guard-multi-expr)
     ( ;;EMPTY
      )
     ) ;; end guard-multi-expr-rest

    (guard-prefix-expr
     (prefix-op
      guard-application-expr)
     (guard-application-expr)
     ) ;; end guard-prefix-expr

    (guard-application-expr
     (atom
      semantic-list
      "^(")
     (guard-record-expr)
     (guard-primary-expr)
     ) ;; end guard-application-expr

    (guard-exprs-list
     (open-paren
      close-paren)
     (open-paren
      guard-exprs
      close-paren)
     ) ;; end guard-exprs-list

    (guard-exprs
     (guard-expr
      guard-exprs-rest)
     ) ;; end guard-exprs

    (guard-exprs-rest
     (punctuation
      "\\`[,]\\'"
      guard-exprs)
     (punctuation
      "\\`[;]\\'"
      guard-exprs)
     ( ;;EMPTY
      )
     ) ;; end guard-exprs-rest

    (guard-record-expr
     (punctuation
      "\\`[#]\\'"
      atom
      punctuation
      "\\`[.]\\'"
      atom)
     (guard-primary-expr
      punctuation
      "\\`[#]\\'"
      atom
      punctuation
      "\\`[.]\\'"
      atom)
     ) ;; end guard-record-expr

    (guard-primary-expr
     (basic-type)
     (macro-app)
     (semantic-list
      "^{")
     (semantic-list
      "^\\[")
     (semantic-list
      "^(")
     ) ;; end guard-primary-expr

    (guard-tuple-skel
     (open-paren
      close-paren)
     (open-paren
      guard-exprs
      close-paren)
     ) ;; end guard-tuple-skel

    (guard-list-skel
     (open-paren
      close-paren)
     (open-paren
      guard-exprs
      guard-list-skel-tail
      close-paren)
     ) ;; end guard-list-skel

    (guard-list-skel-tail
     (punctuation
      "\\`[|]\\'"
      guard-expr)
     ( ;;EMPTY
      )
     ) ;; end guard-list-skel-tail

    (guard-paren-expr
     (open-paren
      guard-expr
      close-paren)
     ) ;; end guard-paren-expr

    (directive
     (macro-def)
     (macro-undef)
     (include-dir)
     (include-lib-dir)
     (ifdef-dir)
     (ifndef-dir)
     (else-dir)
     (endif-dir)
     ) ;; end directive

    (macro-def
     (punctuation
      "\\`[-]\\'"
      DEFINE
      semantic-list
      "^("
      full-stop
      ,(semantic-lambda
	(semantic-bovinate-from-nonterminal
	 (car
	  (nth 2 vals))
	 (cdr
	  (nth 2 vals))
	 'macro-def-list))
      )
     ) ;; end macro-def

    (macro-def-list
     (open-paren
      symbol
      macro-def-opt
      punctuation
      "\\`[,]\\'"
      macro-def-opt
      close-paren
      ,(semantic-lambda
	(semantic-tag-new-variable
	 (nth 1 vals) nil
	 (nth 4 vals) :constant-flag t))
      )
     ) ;; end macro-def-list

    (macro-def-opt
     (semantic-list)
     (expr)
     ( ;;EMPTY
      )
     ) ;; end macro-def-opt

    (macro-undef
     (punctuation
      "\\`[-]\\'"
      UNDEF
      semantic-list
      "^("
      full-stop)
     ) ;; end macro-undef

    (macro-app
     (punctuation
      "\\`[?]\\'"
      symbol
      semantic-list
      "^(")
     (punctuation
      "\\`[?]\\'"
      symbol)
     ) ;; end macro-app

    (include-dir
     (punctuation
      "\\`[-]\\'"
      INCLUDE
      semantic-list
      "^("
      full-stop
      ,(semantic-lambda
	(semantic-tag-new-include
	 (car
	  (semantic-bovinate-from-nonterminal
	   (car
	    (nth 2 vals))
	   (cdr
	    (nth 2 vals))
	   'include-file-name)) nil))
      )
     ) ;; end include-dir

    (include-lib-dir
     (punctuation
      "\\`[-]\\'"
      INCLUDE_LIB
      semantic-list
      "^("
      full-stop
      ,(semantic-lambda
	(semantic-tag-new-include
	 (car
	  (semantic-bovinate-from-nonterminal
	   (car
	    (nth 2 vals))
	   (cdr
	    (nth 2 vals))
	   'include-file-name)) nil))
      )
     ) ;; end include-lib-dir

    (include-file-name
     (open-paren
      string
      close-paren
      ,(semantic-lambda
	(list
	 (read
	  (nth 1 vals))))
      )
     ) ;; end include-file-name

    (ifdef-dir
     (punctuation
      "\\`[-]\\'"
      IFDEF
      semantic-list
      "^("
      full-stop)
     ) ;; end ifdef-dir

    (ifndef-dir
     (punctuation
      "\\`[-]\\'"
      IFNDEF
      semantic-list
      "^("
      full-stop)
     ) ;; end ifndef-dir

    (else-dir
     (punctuation
      "\\`[-]\\'"
      ELSE
      full-stop)
     ) ;; end else-dir

    (endif-dir
     (punctuation
      "\\`[-]\\'"
      ENDIF
      full-stop)
     ) ;; end endif-dir

    (full-stop
     (punctuation
      "\\`[.]\\'")
     ) ;; end full-stop
    )
  "Parser table.")

(defun semantic-erlang-by--install-parser ()
  "Setup the Semantic Parser."
  (setq semantic--parse-table semantic-erlang-by--parse-table
	semantic-debug-parser-source "erlang.by"
	semantic-debug-parser-class 'semantic-bovine-debug-parser
	semantic-flex-keywords-obarray semantic-erlang-by--keyword-table
	))


;;; Analyzers
;;
(require 'semantic-lex)


;;; Epilogue
;;

(provide 'semantic-erlang-by)

;;; semantic-erlang-by.el ends here
