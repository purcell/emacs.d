;;; wisent-awk-wy.el --- Generated parser support file

;; Copyright (C) 2002 David Ponce

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
;; generated from the grammar file wisent-awk.wy.

;;; History:
;;

;;; Code:

;;; Prologue
;;

;;; Declarations
;;
(defconst wisent-awk-wy--keyword-table
  (semantic-lex-make-keyword-table 'nil 'nil)
  "Table of language keywords.")

(defconst wisent-awk-wy--token-table
  (semantic-lex-make-type-table
   '(("<no-type>"
      (LEX_LENGTH)
      (LEX_BUILTIN)
      (DECREMENT)
      (INCREMENT)
      (LEX_OR)
      (LEX_AND)
      (LEX_IN)
      (LEX_NEXTFILE)
      (LEX_GETLINE)
      (LEX_FUNCTION)
      (LEX_EXIT)
      (LEX_NEXT)
      (LEX_PRINTF)
      (LEX_PRINT)
      (LEX_CONTINUE)
      (LEX_BREAK)
      (LEX_FOR)
      (LEX_DO)
      (LEX_WHILE)
      (LEX_DELETE)
      (LEX_RETURN)
      (LEX_ELSE)
      (LEX_IF)
      (LEX_END)
      (LEX_BEGIN)
      (CONCAT_OP)
      (NEWLINE)
      (MATCHOP)
      (ASSIGNOP)
      (APPEND_OP)
      (RELOP)
      (YSTRING)
      (YNUMBER)
      (ERROR)
      (REGEXP)
      (NAME)
      (FUNC_CALL)))
   'nil)
  "Table of lexical tokens.")

(defconst wisent-awk-wy--parse-table
  (progn
    (eval-when-compile
      (require 'wisent-comp))
    (wisent-compile-grammar
     '((FUNC_CALL NAME REGEXP ERROR YNUMBER YSTRING RELOP APPEND_OP ASSIGNOP MATCHOP NEWLINE CONCAT_OP LEX_BEGIN LEX_END LEX_IF LEX_ELSE LEX_RETURN LEX_DELETE LEX_WHILE LEX_DO LEX_FOR LEX_BREAK LEX_CONTINUE LEX_PRINT LEX_PRINTF LEX_NEXT LEX_EXIT LEX_FUNCTION LEX_GETLINE LEX_NEXTFILE LEX_IN LEX_AND LEX_OR INCREMENT DECREMENT LEX_BUILTIN LEX_LENGTH)
       ((right ASSIGNOP)
	(right 63 58)
	(left LEX_OR)
	(left LEX_AND)
	(left LEX_GETLINE)
	(nonassoc LEX_IN)
	(left FUNC_CALL LEX_BUILTIN LEX_LENGTH)
	(nonassoc 44)
	(nonassoc MATCHOP)
	(nonassoc RELOP 60 62 124 APPEND_OP TWOWAYIO)
	(left CONCAT_OP)
	(left YSTRING YNUMBER)
	(left 43 45)
	(left 42 47 37)
	(right 33 UNARY)
	(right 94)
	(left INCREMENT DECREMENT)
	(left 36)
	(left 40 41))
       (start_
	((opt_nls program opt_nls)))
       (program
	((rule))
	((program rule))
	((error))
	((program error))
	(nil))
       (rule
	((LEX_BEGIN
	  (progn)
	  action))
	((LEX_END
	  (progn)
	  action))
	((LEX_BEGIN statement_term))
	((LEX_END statement_term))
	((pattern action))
	((action))
	((pattern statement_term))
	((function_prologue function_body)))
       (func_name
	((NAME))
	((FUNC_CALL))
	((lex_builtin)))
       (lex_builtin
	((LEX_BUILTIN))
	((LEX_LENGTH)))
       (function_prologue
	((LEX_FUNCTION
	  (progn)
	  func_name 40 opt_param_list r_paren opt_nls)))
       (function_body
	((l_brace statements r_brace opt_semi opt_nls))
	((l_brace r_brace opt_semi opt_nls)))
       (pattern
	((exp))
	((exp 44 exp)))
       (regexp
	((47
	  (progn)
	  REGEXP 47)))
       (action
	((l_brace statements r_brace opt_semi opt_nls))
	((l_brace r_brace opt_semi opt_nls)))
       (statements
	((statement))
	((statements statement))
	((error))
	((statements error)))
       (statement_term
	((nls))
	((semi opt_nls)))
       (statement
	((semi opt_nls))
	((l_brace r_brace))
	((l_brace statements r_brace))
	((if_statement))
	((LEX_WHILE 40 exp r_paren opt_nls statement))
	((LEX_DO opt_nls statement LEX_WHILE 40 exp r_paren opt_nls))
	((LEX_FOR 40 NAME LEX_IN NAME r_paren opt_nls statement))
	((LEX_FOR 40 opt_exp semi opt_nls exp semi opt_nls opt_exp r_paren opt_nls statement))
	((LEX_FOR 40 opt_exp semi opt_nls semi opt_nls opt_exp r_paren opt_nls statement))
	((LEX_BREAK statement_term))
	((LEX_CONTINUE statement_term))
	((print 40 expression_list r_paren output_redir statement_term))
	((print opt_rexpression_list output_redir statement_term))
	((LEX_NEXT statement_term))
	((LEX_NEXTFILE statement_term))
	((LEX_EXIT opt_exp statement_term))
	((LEX_RETURN
	  (progn)
	  opt_exp statement_term))
	((LEX_DELETE NAME 91 expression_list 93 statement_term))
	((LEX_DELETE NAME statement_term))
	((exp statement_term)))
       (print
	((LEX_PRINT))
	((LEX_PRINTF)))
       (if_statement
	((LEX_IF 40 exp r_paren opt_nls statement))
	((LEX_IF 40 exp r_paren opt_nls statement LEX_ELSE opt_nls statement)))
       (nls
	((NEWLINE))
	((nls NEWLINE)))
       (opt_nls
	(nil)
	((nls)))
       (input_redir
	(nil)
	((60 simp_exp)))
       (output_redir
	(nil)
	((62 exp))
	((APPEND_OP exp))
	((124 exp))
	((TWOWAYIO exp)))
       (opt_param_list
	(nil)
	((param_list)))
       (param_list
	((NAME))
	((param_list comma NAME))
	((error))
	((param_list error))
	((param_list comma error)))
       (opt_exp
	(nil)
	((exp)))
       (opt_rexpression_list
	(nil)
	((rexpression_list)))
       (rexpression_list
	((rexp))
	((rexpression_list comma rexp))
	((error))
	((rexpression_list error))
	((rexpression_list error rexp))
	((rexpression_list comma error)))
       (opt_expression_list
	(nil)
	((expression_list)))
       (expression_list
	((exp))
	((expression_list comma exp))
	((error))
	((expression_list error))
	((expression_list error exp))
	((expression_list comma error)))
       (exp
	((variable ASSIGNOP
		   (progn)
		   exp))
	((40 expression_list r_paren LEX_IN NAME))
	((exp 124 LEX_GETLINE opt_variable))
	((exp TWOWAYIO LEX_GETLINE opt_variable))
	((LEX_GETLINE opt_variable input_redir))
	((exp LEX_AND exp))
	((exp LEX_OR exp))
	((exp MATCHOP exp))
	((regexp))
	((33 regexp)
	 [UNARY])
	((exp LEX_IN NAME))
	((exp RELOP exp))
	((exp 60 exp))
	((exp 62 exp))
	((exp 63 exp 58 exp))
	((simp_exp))
	((exp simp_exp)
	 [CONCAT_OP]))
       (rexp
	((variable ASSIGNOP
		   (progn)
		   rexp))
	((rexp LEX_AND rexp))
	((rexp LEX_OR rexp))
	((LEX_GETLINE opt_variable input_redir))
	((regexp))
	((33 regexp)
	 [UNARY])
	((rexp MATCHOP rexp))
	((rexp LEX_IN NAME))
	((rexp RELOP rexp))
	((rexp 63 rexp 58 rexp))
	((simp_exp))
	((rexp simp_exp)
	 [CONCAT_OP]))
       (simp_exp
	((non_post_simp_exp))
	((simp_exp 94 simp_exp))
	((simp_exp 42 simp_exp))
	((simp_exp 47 simp_exp))
	((simp_exp 37 simp_exp))
	((simp_exp 43 simp_exp))
	((simp_exp 45 simp_exp))
	((variable INCREMENT))
	((variable DECREMENT)))
       (non_post_simp_exp
	((33 simp_exp)
	 [UNARY])
	((40 exp r_paren))
	((LEX_BUILTIN 40 opt_expression_list r_paren))
	((LEX_LENGTH 40 opt_expression_list r_paren))
	((LEX_LENGTH))
	((FUNC_CALL 40 opt_expression_list r_paren))
	((variable))
	((INCREMENT variable))
	((DECREMENT variable))
	((YNUMBER))
	((YSTRING))
	((45 simp_exp)
	 [UNARY])
	((43 simp_exp)
	 [UNARY]))
       (opt_variable
	(nil)
	((variable)))
       (variable
	((NAME))
	((NAME 91 expression_list 93))
	((36 non_post_simp_exp)))
       (l_brace
	((123 opt_nls)))
       (r_brace
	((125 opt_nls)))
       (r_paren
	((41)))
       (opt_semi
	(nil)
	((semi)))
       (semi
	((59)))
       (comma
	((44 opt_nls))))
     'nil))
  "Parser table.")

(defun wisent-awk-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table wisent-awk-wy--parse-table
	semantic-debug-parser-source "wisent-awk.wy"
	semantic-flex-keywords-obarray wisent-awk-wy--keyword-table
	semantic-lex-types-obarray wisent-awk-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
	    'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(require 'semantic-lex)


;;; Epilogue
;;




(provide 'wisent-awk-wy)

;;; wisent-awk-wy.el ends here
