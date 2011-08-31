;;; wisent-calc-wy.el --- Generated parser support file

;; Copyright (C) 2002, 2003 David Ponce

;; Author: Mona <whisky@ubuntu.ubuntu-domain>
;; Created: 2011-05-20 16:20:50-0400
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
;; generated from the grammar file wisent-calc.wy.

;;; History:
;;

;;; Code:

;;; Prologue
;;

;;; Declarations
;;
(defconst wisent-calc-wy--keyword-table
  (semantic-lex-make-keyword-table 'nil 'nil)
  "Table of language keywords.")

(defconst wisent-calc-wy--token-table
  (semantic-lex-make-type-table
   '(("number"
      (NUM)))
   'nil)
  "Table of lexical tokens.")

(defconst wisent-calc-wy--parse-table
  (progn
    (eval-when-compile
      (require 'wisent-comp))
    (wisent-compile-grammar
     '((NUM)
       ((nonassoc 61)
	(left 45 43)
	(left 42 47)
	(left NEG)
	(right 94))
       (input
	((line))
	((input line)
	 (format "%s %s" $1 $2)))
       (line
	((59)
	 (progn ";"))
	((exp 59)
	 (format "%s;" $1))
	((error 59)
	 (progn "Error;")))
       (exp
	((NUM)
	 (string-to-number $1))
	((exp 61 exp)
	 (= $1 $3))
	((exp 43 exp)
	 (+ $1 $3))
	((exp 45 exp)
	 (- $1 $3))
	((exp 42 exp)
	 (* $1 $3))
	((exp 47 exp)
	 (/ $1 $3))
	((45 exp)
	 [NEG]
	 (- $2))
	((exp 94 exp)
	 (expt $1 $3))
	((40 exp 41)
	 (progn $2))))
     'nil))
  "Parser table.")

(defun wisent-calc-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table wisent-calc-wy--parse-table
	semantic-debug-parser-source "wisent-calc.wy"
	semantic-flex-keywords-obarray wisent-calc-wy--keyword-table
	semantic-lex-types-obarray wisent-calc-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
	    'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(require 'semantic-lex)


;;; Epilogue
;;
(defun wisent-calc-setup-parser ()
  "Setup buffer for parse."
  (wisent-calc-wy--install-parser)
  (setq semantic-number-expression
        (concat "\\([0-9]+\\([.][0-9]*\\)?\\([eE][-+]?[0-9]+\\)?"
                "\\|[.][0-9]+\\([eE][-+]?[0-9]+\\)?\\)")
        semantic-lex-analyzer 'wisent-calc-lexer
        semantic-lex-depth nil
        semantic-lex-syntax-modifications
        '((?\; ".") (?\= ".") (?\+ ".")
          (?\- ".") (?\* ".") (?\/ ".")
          (?\^ ".") (?\( ".") (?\) ".")
          )
        )
  )

(provide 'wisent-calc-wy)

;;; wisent-calc-wy.el ends here
