;;; wisent-ruby-wy.el --- Generated parser support file

;;; Copyright (C) 2007 Daniel Debertin (debertin@gmail.com)

;; Author: Mona <whisky@ubuntu.ubuntu-domain>
;; Created: 2011-05-20 16:21:12-0400
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
;; generated from the grammar file wisent-ruby.wy.

;;; History:
;;

;;; Code:

;;; Prologue
;;

;;; Declarations
;;
(defconst wisent-ruby-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("__FILE__" . FILE)
     ("__LINE__" . LINE)
     ("BEGIN" . BEGIN_BLOCK)
     ("END" . END_BLOCK)
     ("alias" . ALIAS)
     ("and" . AND)
     ("begin" . BEGIN)
     ("break" . BREAK)
     ("case" . CASE)
     ("class" . CLASS)
     ("def" . DEF)
     ("defined?" . DEFINED_P)
     ("do" . DO)
     ("else" . ELSE)
     ("elsif" . ELSIF)
     ("end" . END)
     ("ensure" . ENSURE)
     ("false" . FALSE)
     ("for" . FOR)
     ("if" . IF)
     ("in" . IN)
     ("module" . MODULE)
     ("next" . NEXT)
     ("nil" . NIL)
     ("not" . NOT)
     ("or" . OR)
     ("redo" . REDO)
     ("rescue" . RESCUE)
     ("retry" . RETRY)
     ("return" . RETURN)
     ("self" . SELF)
     ("super" . SUPER)
     ("then" . THEN)
     ("true" . TRUE)
     ("undef" . UNDEF)
     ("unless" . UNLESS)
     ("until" . UNTIL)
     ("when" . WHEN)
     ("while" . WHILE)
     ("yield" . YIELD)
     ("require" . REQUIRE)
     ("attr_reader" . ATTR_READER)
     ("attr_writer" . ATTR_WRITER)
     ("attr_accessor" . ATTR_ACCESSOR)
     ("include" . INCLUDE)
     ("extend" . EXTEND)
     ("public" . PROT_PUBLIC)
     ("protected" . PROT_PROTECTED)
     ("private" . PROT_PRIVATE)
     ("loop" . LOOP)
     ("raise" . RAISE)
     ("catch" . CATCH)
     ("throw" . THROW)
     ("proc" . PROC)
     ("lambda" . LAMBDA))
   '(("lambda" summary "lambda { [<|args|>] ... }")
     ("proc" summary "proc { [<|args|>] ... }")
     ("throw" summary "THROW <anchor>[, <retval>]")
     ("catch" summary "CATCH <tag> do ...[throw <tag>] ... end")
     ("raise" summary "RAISE [<exception_type>] [<message>]")
     ("loop" summary "loop .... end")
     ("private" summary "private: instance-level protection")
     ("protected" summary "protected: class-level protection")
     ("public" summary "public: public protection")
     ("extend" summary "EXTEND <module_name>")
     ("include" summary "INCLUDE <module_name>")
     ("attr_accessor" summary "attr_accessor <symbol_list>")
     ("attr_writer" summary "attr_writer <symbol_list>")
     ("attr_reader" summary "attr_reader <symbol_list>")
     ("require" summary "require <file>")
     ("yield" summary "yield: return control to the caller until called again")
     ("while" summary "WHILE(<expression>) ... end")
     ("when" summary "case <lhs> WHEN <rhs> ... end")
     ("until" summary "UNTIL(<expression>) ... end")
     ("unless" summary "UNLESS(<expression>) ... end")
     ("undef" summary "I have no idea what this does")
     ("true" summary "true: true value")
     ("then" summary "if(<expression>) THEN ... end")
     ("super" summary "super: the superclass method")
     ("self" summary "self: the current instance")
     ("return" summary "return: return control to the caller permanently")
     ("retry" summary "begin ... rescue [exception] ... RETRY ... end")
     ("rescue" summary "begin ... RESCUE [exception_name] <body> end")
     ("redo" summary "redo: try current iteration again")
     ("or" summary "or: logical OR")
     ("not" summary "not: logical NOT")
     ("nil" summary "nil: no value")
     ("next" summary "next: continue to next block iteration")
     ("module" summary "MODULE <module_name> <module_body> end")
     ("in" summary "for x IN y ... end")
     ("if" summary "IF(<expression>) ... end")
     ("for" summary "FOR x in y ... end")
     ("false" summary "false: false value")
     ("ensure" summary "begin ... ENSURE ... end")
     ("end" summary "end: block end")
     ("elsif" summary "if(<expression>) ... ELSIF(<expression>) ... end")
     ("else" summary "if(<expression>) ... ELSE ... end")
     ("do" summary "DO <body> end")
     ("defined?" summary "defined?(<value>)")
     ("def" summary "DEF <method_name> ([method_args]) <method_body> end")
     ("class" summary "CLASS <classname> [< parentclass] ... end")
     ("case" summary "CASE <lhs> when <rhs> ... when <rhs> ... end")
     ("break" summary "break: terminate current block")
     ("begin" summary "BEGIN ... [rescue,ensure] ... end")
     ("and" summary "and: logical AND")
     ("alias" summary "ALIAS method1 method2")
     ("END" summary "END { ... }")
     ("BEGIN" summary "BEGIN { ... }")
     ("__LINE__" summary "__LINE__: The current line")
     ("__FILE__" summary "__FILE__: The current file")))
  "Table of language keywords.")

(defconst wisent-ruby-wy--token-table
  (semantic-lex-make-type-table
   '(("punctuation"
      (HASH_REF . "=>")
      (OBJ_EQ . "===")
      (NAMESPACE_DEREF . "::")
      (LOGOR_EQ . "||=")
      (LOGAND_EQ . "&&=")
      (LOG_OR . "||")
      (LOG_AND . "&&")
      (SEMICOLON . ";")
      (DIV_EQ . "/=")
      (DIV . "/")
      (BACKQUOTE . "`")
      (HEREDOC . "<<-")
      (GTGT_EQ . ">>=")
      (LTLT_EQ . "<<=")
      (GTGT . ">>")
      (LTLT . "<<")
      (GTEQ . ">=")
      (LTEQ . "<=")
      (GT . ">")
      (LT . "<")
      (TERNARY_COND . "?")
      (COLON . ":")
      (REGEX_EQ . "=~")
      (COMPLEMENT . "~")
      (MINUS_EQ . "-=")
      (MINUS . "-")
      (EXP_EQ . "**=")
      (EXP . "**")
      (MULT_EQ . "*=")
      (MULT . "*")
      (BITAND_EQ . "&=")
      (BITAND . "&")
      (BITOR_EQ . "|=")
      (BITOR . "|")
      (BITXOR_EQ . "^=")
      (BITXOR . "^")
      (MOD_EQ . "%=")
      (MODULUS . "%")
      (BANG . "!")
      (COMMA . ",")
      (PLUS_EQ . "+=")
      (PLUS . "+")
      (NEQUAL . "!=")
      (EQUAL . "==")
      (PERIOD . ".")
      (EQUALS . "="))
     ("close-paren"
      (RPAREN . ")")
      (RBRACK . "]"))
     ("open-paren"
      (RBRACE . "}")
      (LBRACE . "{")
      (LPAREN . "(")
      (LBRACK . "["))
     ("newline"
      (NEWLINE))
     ("rubyblock"
      (RUBY_BLOCK))
     ("braceblock"
      (BRACE_BLOCK))
     ("parenblock"
      (PAREN_BLOCK))
     ("rubylvar"
      (VAR_LOCAL))
     ("rubyconst"
      (CONSTANT))
     ("rubygvar"
      (VAR_GLOBAL))
     ("rubycvar"
      (VAR_CLASS))
     ("rubyivar"
      (VAR_INSTANCE))
     ("rubysymbol"
      (SYMBOL_LITERAL))
     ("rubyhash"
      (HASH_LITERAL))
     ("rubyrange"
      (RANGE_LITERAL))
     ("rubynumber"
      (INTEGER_LITERAL))
     ("rubycmd"
      (SHELL_COMMAND_LITERAL))
     ("rubyregexp"
      (REGEXP_LITERAL))
     ("rubyarray"
      (ARRAY_LITERAL))
     ("rubystring"
      (STRING_LITERAL))
     ("identifier"
      (IDENTIFIER)))
   'nil)
  "Table of lexical tokens.")

(defconst wisent-ruby-wy--parse-table
  (progn
    (eval-when-compile
      (require 'wisent-comp))
    (wisent-compile-grammar
     '((IDENTIFIER STRING_LITERAL ARRAY_LITERAL REGEXP_LITERAL SHELL_COMMAND_LITERAL INTEGER_LITERAL RANGE_LITERAL HASH_LITERAL SYMBOL_LITERAL VAR_INSTANCE VAR_CLASS VAR_GLOBAL CONSTANT VAR_LOCAL PAREN_BLOCK BRACE_BLOCK RUBY_BLOCK NEWLINE FILE LINE BEGIN_BLOCK END_BLOCK ALIAS AND BEGIN BREAK CASE CLASS DEF DEFINED_P DO ELSE ELSIF END ENSURE FALSE FOR IF IN MODULE NEXT NIL NOT OR REDO RESCUE RETRY RETURN SELF SUPER THEN TRUE UNDEF UNLESS UNTIL WHEN WHILE YIELD REQUIRE ATTR_READER ATTR_WRITER ATTR_ACCESSOR INCLUDE EXTEND PROT_PUBLIC PROT_PROTECTED PROT_PRIVATE LOOP RAISE CATCH THROW PROC LAMBDA LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE EQUALS PERIOD EQUAL NEQUAL PLUS PLUS_EQ COMMA BANG MODULUS MOD_EQ BITXOR BITXOR_EQ BITOR BITOR_EQ BITAND BITAND_EQ MULT MULT_EQ EXP EXP_EQ MINUS MINUS_EQ COMPLEMENT REGEX_EQ COLON TERNARY_COND LT GT LTEQ GTEQ LTLT GTGT LTLT_EQ GTGT_EQ HEREDOC BACKQUOTE DIV DIV_EQ SEMICOLON LOG_AND LOG_OR LOGAND_EQ LOGOR_EQ NAMESPACE_DEREF OBJ_EQ HASH_REF)
       nil
       (stmts
	(nil)
	((stmt))
	((stmt separators stmts)))
       (stmt
	((expression))
	((definition))
	((simple_stmt)))
       (opt_cond
	(nil)
	((conditional_exp)))
       (separator
	((NEWLINE))
	((SEMICOLON)))
       (separators
	((separator))
	((separator separators)))
       (definition
	 ((method_def))
	 ((class_def))
	 ((module_def)))
       (simple_stmt
	((require))
	((super))
	((alias))
	((attr_decl))
	((mixin))
	((access_decl))
	((loop_control))
	((return))
	((exception)))
       (require
	((REQUIRE STRING_LITERAL)
	 (wisent-raw-tag
	  (semantic-tag-new-include $2 nil))))
       (super
	((SUPER opt_args)
	 (wisent-raw-tag
	  (semantic-tag-new-code "super" nil))))
       (opt_args
	(nil)
	((PAREN_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'parameters 1)))
       (parameters
	(nil)
	((LPAREN))
	((RPAREN))
	((parameter COMMA)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $1 nil nil)))
	((parameter RPAREN)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $1 nil nil))))
       (parameter
	((expression_term))
	((expression_term EQUALS literal)))
       (alias
	((ALIAS IDENTIFIER IDENTIFIER)
	 (wisent-raw-tag
	  (semantic-tag-new-function $2 "alias" nil nil))))
       (attr_decl
	((ATTR_READER symbol_list)
	 (wisent-raw-tag
	  (semantic-tag "reader" 'attribute :symbols $2)))
	((ATTR_WRITER symbol_list)
	 (wisent-raw-tag
	  (semantic-tag "writer" 'attribute :symbols $2)))
	((ATTR_ACCESSOR symbol_list)
	 (wisent-raw-tag
	  (semantic-tag "accessor" 'attribute :symbols $2))))
       (symbol_list
	((SYMBOL_LITERAL)
	 (list $1))
	((SYMBOL_LITERAL COMMA symbol_list)
	 (append
	  (list $1)
	  $3)))
       (mixin
	((INCLUDE CONSTANT)
	 (wisent-raw-tag
	  (semantic-tag-new-code "mixin" nil)))
	((EXTEND CONSTANT)
	 (wisent-raw-tag
	  (semantic-tag-new-code "mixin" nil))))
       (protection
	((PROT_PUBLIC))
	((PROT_PROTECTED))
	((PROT_PRIVATE)))
       (access_decl
	((protection)
	 (set-current-protection-context $1))
	((protection symbol_list)
	 (dolist
	     (sym $2)
	   (set-tag-protection sym $1))))
       (loop_control
	((BREAK opt_exp)
	 (wisent-raw-tag
	  (semantic-tag-new-code "loop_control" 'break :value $2)))
	((NEXT opt_exp)
	 (wisent-raw-tag
	  (semantic-tag-new-code "loop_control" 'next :value $2)))
	((REDO)
	 (wisent-raw-tag
	  (semantic-tag-new-code "loop_control" 'redo)))
	((RETRY)
	 (wisent-raw-tag
	  (semantic-tag-new-code "loop_control" 'retry)))
	((THROW SYMBOL_LITERAL opt_exp)
	 (wisent-raw-tag
	  (semantic-tag-new-code "throw" $2 :value $3))))
       (opt_exp
	(nil)
	((expression)))
       (return
	((RETURN opt_exp)
	 (wisent-raw-tag
	  (semantic-tag-new-code "return" nil))))
       (expression_term
	((literal))
	((varref))
	((methodcall)))
       (literal
	((STRING_LITERAL))
	((ARRAY_LITERAL))
	((REGEXP_LITERAL))
	((SHELL_COMMAND_LITERAL))
	((INTEGER_LITERAL))
	((HASH_LITERAL))
	((SYMBOL_LITERAL))
	((boolean_literal))
	((NIL)))
       (boolean_literal
	((TRUE))
	((FALSE)))
       (varref
	((VAR_INSTANCE))
	((VAR_CLASS))
	((VAR_GLOBAL))
	((CONSTANT))
	((VAR_LOCAL)))
       (method_head
	((receiver IDENTIFIER))
	((IDENTIFIER)))
       (methodcall
	((method_head))
	((method_head PAREN_BLOCK))
	((method_head code_block))
	((method_head PAREN_BLOCK code_block)))
       (receiver
	((CONSTANT PERIOD))
	((IDENTIFIER PERIOD))
	((literal PERIOD)))
       (code_block
	((BRACE_BLOCK))
	((DO RUBY_BLOCK)))
       (expression
	((operator_exp)
	 (wisent-raw-tag
	  (semantic-tag-new-code "expression" nil)))
	((assignment_exp)
	 (progn
	   (process-assignment-variables $1)
	   (wisent-raw-tag
	    (semantic-tag-new-code "assignment" nil))))
	((block_exp)
	 (wisent-raw-tag
	  (semantic-tag-new-code "block_expression" nil)))
	((conditional_exp)
	 (wisent-raw-tag
	  (semantic-tag-new-code "cond_exp" nil)))
	((case_exp)
	 (wisent-raw-tag
	  (semantic-tag-new-code "case" nil)))
	((iteration_exp)
	 (wisent-raw-tag
	  (semantic-tag-new-code "iteration" nil)))
	((expression_term)
	 (if
	     (member $1 semantic-lex-ruby-assigned-vars)
	     (wisent-raw-tag
	      (semantic-tag-new-code "expression_term" nil))
	   (wisent-raw-tag
	    (semantic-tag-new-code "methodcall" nil)))))
       (expressions
	((expression))
	((expression bool_op expressions)))
       (operator_exp
	((expression_term cmp_op expression_term))
	((expression_term cmp_op operator_exp)))
       (assignment_exp
	((expression_term assn_op expression_term)
	 (list $1))
	((expression_term assn_op assignment_exp)
	 (append
	  (list $1)
	  $3)))
       (block_exp
	((BEGIN inner_block END)))
       (inner_block
	((RUBY_BLOCK))
	((RUBY_BLOCK rescues))
	((RUBY_BLOCK ENSURE RUBY_BLOCK))
	((RUBY_BLOCK rescues ENSURE RUBY_BLOCK)))
       (rescues
	((RESCUE RUBY_BLOCK))
	((RESCUE exception_catch RUBY_BLOCK))
	((RESCUE exception_catch RUBY_BLOCK rescues)))
       (exception_catch
	((CONSTANT))
	((CONSTANT HASH_REF IDENTIFIER)))
       (conditional_exp_arg
	((expression))
	((PAREN_BLOCK)))
       (aux_cond
	((ELSIF conditional_exp_arg opt_then RUBY_BLOCK))
	((ELSIF conditional_exp_arg opt_then RUBY_BLOCK aux_cond))
	((ELSE RUBY_BLOCK END)))
       (conditional_exp
	((cond_kw conditional_exp_arg opt_then RUBY_BLOCK END))
	((cond_kw conditional_exp_arg opt_then RUBY_BLOCK aux_cond END)))
       (cond_kw
	((IF))
	((UNLESS)))
       (opt_then
	(nil)
	((THEN)))
       (ternary_exp
	((expression TERNARY_COND expression_term COLON expression_term)))
       (case_exp
	((CASE IDENTIFIER when_blocks END)))
       (when_blocks
	((WHEN IDENTIFIER RUBY_BLOCK))
	((WHEN IDENTIFIER RUBY_BLOCK when_blocks))
	((ELSE RUBY_BLOCK)))
       (iteration_exp
	((cond_iter_kw conditional_exp_arg RUBY_BLOCK END))
	((FOR RUBY_BLOCK END))
	((LOOP DO RUBY_BLOCK END))
	((LOOP BRACE_BLOCK)))
       (cond_iter_kw
	((WHILE))
	((UNTIL)))
       (method_def
	((DEF IDENTIFIER opt_args RUBY_BLOCK END)
	 (wisent-raw-tag
	  (semantic-tag-new-function $2 "method"
				     (if $3
					 (semantic-parse-region
					  (car $region3)
					  (cdr $region3)
					  'parameters 1)
				       nil)))))
       (class_def
	((CLASS CONSTANT RUBY_BLOCK END)
	 (wisent-raw-tag
	  (semantic-tag-new-type $2 $1
				 (semantic-parse-region
				  (car $region3)
				  (cdr $region3)
				  'stmts 1)
				 nil)))
	((CLASS CONSTANT LT CONSTANT RUBY_BLOCK END)
	 (wisent-raw-tag
	  (semantic-tag-new-type $2 $1
				 (semantic-parse-region
				  (car $region5)
				  (cdr $region5)
				  'stmts 1)
				 (cons $4 nil)))))
       (module_def
	((MODULE CONSTANT RUBY_BLOCK END)
	 (wisent-raw-tag
	  (semantic-tag-new-type $2 "module"
				 (semantic-parse-region
				  (car $region3)
				  (cdr $region3)
				  'stmts 1)
				 nil))))
       (exception
	((RAISE))
	((RAISE CONSTANT))
	((RAISE CONSTANT STRING_LITERAL))
	((RAISE STRING_LITERAL)))
       (cmp_op
	((EQUAL))
	((NEQUAL))
	((PLUS))
	((MODULUS))
	((BITXOR))
	((BITOR))
	((BITAND))
	((MULT))
	((EXP))
	((MINUS))
	((COMPLEMENT))
	((REGEX_EQ))
	((LT))
	((GT))
	((LTEQ))
	((GTEQ))
	((LTLT))
	((GTGT))
	((DIV))
	((OBJ_EQ)))
       (bool_op
	((AND))
	((OR))
	((LOG_AND))
	((LOG_OR)))
       (assn_op
	((EQUALS))
	((PLUS_EQ))
	((MOD_EQ))
	((BITXOR_EQ))
	((BITOR_EQ))
	((BITAND_EQ))
	((MULT_EQ))
	((EXP_EQ))
	((MINUS_EQ))
	((LTLT_EQ))
	((GTGT_EQ))
	((DIV_EQ))
	((LOGAND_EQ))
	((LOGOR_EQ))))
     '(stmts parameters)))
  "Parser table.")

(defun wisent-ruby-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table wisent-ruby-wy--parse-table
	semantic-debug-parser-source "wisent-ruby.wy"
	semantic-flex-keywords-obarray wisent-ruby-wy--keyword-table
	semantic-lex-types-obarray wisent-ruby-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
	    'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(require 'semantic-lex)


;;; Epilogue
;;

(provide 'wisent-ruby-wy)

;;; wisent-ruby-wy.el ends here
