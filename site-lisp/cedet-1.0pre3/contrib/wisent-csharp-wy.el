;;; wisent-csharp-wy.el --- Generated parser support file

;; Copyright (C) 2003 David Shilvock

;; Author: Steve Purcell <steve@Monaco.local>
;; Created: 2006-06-22 14:50:27+0200
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
;; generated from the grammar file wisent-csharp.wy.

;;; History:
;;

;;; Code:

;;; Prologue
;;

;;; Declarations
;;
(defconst wisent-csharp-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("abstract" . ABSTRACT)
     ("add" . ADD)
     ("as" . AS)
     ("base" . BASE)
     ("bool" . BOOL)
     ("break" . BREAK)
     ("byte" . BYTE)
     ("case" . CASE)
     ("catch" . CATCH)
     ("char" . CHAR)
     ("checked" . CHECKED)
     ("class" . CLASS)
     ("const" . CONST)
     ("continue" . CONTINUE)
     ("decimal" . DECIMAL)
     ("default" . DEFAULT)
     ("delegate" . DELEGATE)
     ("do" . DO)
     ("double" . DOUBLE)
     ("else" . ELSE)
     ("enum" . ENUM)
     ("event" . EVENT)
     ("explicit" . EXPLICIT)
     ("extern" . EXTERN)
     ("finally" . FINALLY)
     ("fixed" . FIXED)
     ("float" . FLOAT)
     ("for" . FOR)
     ("foreach" . FOREACH)
     ("get" . GET)
     ("goto" . GOTO)
     ("if" . IF)
     ("implicit" . IMPLICIT)
     ("in" . IN)
     ("int" . INT)
     ("interface" . INTERFACE)
     ("internal" . INTERNAL)
     ("is" . IS)
     ("lock" . LOCK)
     ("long" . LONG)
     ("namespace" . NAMESPACE)
     ("new" . NEW)
     ("object" . OBJECT)
     ("operator" . OPERATOR)
     ("out" . OUT)
     ("override" . OVERRIDE)
     ("params" . PARAMS)
     ("private" . PRIVATE)
     ("protected" . PROTECTED)
     ("public" . PUBLIC)
     ("readonly" . READONLY)
     ("ref" . REF)
     ("remove" . REMOVE)
     ("return" . RETURN)
     ("sbyte" . SBYTE)
     ("sealed" . SEALED)
     ("set" . SET)
     ("short" . SHORT)
     ("sizeof" . SIZEOF)
     ("stackalloc" . STACKALLOC)
     ("static" . STATIC)
     ("string" . STRING)
     ("struct" . STRUCT)
     ("switch" . SWITCH)
     ("this" . THIS)
     ("throw" . THROW)
     ("try" . TRY)
     ("typeof" . TYPEOF)
     ("unit" . UINT)
     ("ulong" . ULONG)
     ("unchecked" . UNCHECKED)
     ("unsafe" . UNSAFE)
     ("ushort" . USHORT)
     ("using" . USING)
     ("virtual" . VIRTUAL)
     ("void" . VOID)
     ("volatile" . VOLATILE)
     ("while" . WHILE))
   '(("while" summary "while (<expr>) <stmt> | do <stmt> while (<expr>);")
     ("volatile" summary "Field declaration modifier: volatile <type> <name> ...")
     ("void" summary "Method return type: void <name> ...")
     ("virtual" summary "")
     ("using" summary "Namespace import: using <name> | using <identifier> = <name>;")
     ("ushort" summary "")
     ("unsafe" summary "")
     ("unchecked" summary "")
     ("ulong" summary "")
     ("unit" summary "")
     ("typeof" summary "")
     ("try" summary "try {<stmts>} [catch(<parm>) {<stmts>} ...] [finally {<stmts>}]")
     ("throw" summary "throw <expr> ;")
     ("switch" summary "switch(<expr>) {[case <const-expr>: <stmts> ...] [default: <stmts>]}")
     ("struct" summary "")
     ("string" summary "")
     ("static" summary "Declaration modifier: static {class|interface|<type>} <name> ...")
     ("stackalloc" summary "")
     ("sizeof" summary "")
     ("short" summary "Integral primitive type (-32768 to 32767)")
     ("set" summary "")
     ("sealed" summary "")
     ("sbyte" summary "")
     ("return" summary "return [<expr>] ;")
     ("remove" summary "")
     ("ref" summary "")
     ("readonly" summary "")
     ("public" summary "Access level modifier: public {class|interface|<type>} <name> ...")
     ("protected" summary "Access level modifier: protected {class|interface|<type>} <name> ...")
     ("private" summary "Access level modifier: private {class|interface|<type>} <name> ...")
     ("params" summary "")
     ("override" summary "")
     ("out" summary "")
     ("operator" summary "")
     ("object" summary "")
     ("namespace" summary "")
     ("long" summary "Integral primitive type (-9223372036854775808 to 9223372036854775807)")
     ("lock" summary "")
     ("is" summary "")
     ("internal" summary "")
     ("interface" summary "Interface declaration: interface <name>")
     ("int" summary "Integral primitive type (-2147483648 to 2147483647)")
     ("in" summary "")
     ("implicit" summary "")
     ("if" summary "if (<expr>) <stmt> [else <stmt>]")
     ("goto" summary "Unused reserved word")
     ("get" summary "")
     ("foreach" summary "")
     ("for" summary "for ([<init-expr>]; [<expr>]; [<update-expr>]) <stmt>")
     ("float" summary "Primitive floating-point type (single-precision 32-bit IEEE 754)")
     ("fixed" summary "")
     ("finally" summary "try {<stmts>} ... finally {<stmts>}")
     ("extern" summary "")
     ("explicit" summary "")
     ("event" summary "")
     ("enum" summary "")
     ("else" summary "if (<expr>) <stmt> else <stmt>")
     ("double" summary "Primitive floating-point type (double-precision 64-bit IEEE 754)")
     ("do" summary "do <stmt> while (<expr>);")
     ("delegate" summary "")
     ("default" summary "switch(<expr>) { ... default: <stmts>}")
     ("decimal" summary "")
     ("continue" summary "continue [<label>] ;")
     ("const" summary "Unused reserved word")
     ("class" summary "Class declaration: class <name>")
     ("checked" summary "")
     ("char" summary "Integral primitive type ('u0000' to 'uffff') (0 to 65535)")
     ("catch" summary "try {<stmts>} catch(<parm>) {<stmts>} ... ")
     ("case" summary "switch(<expr>) {case <const-expr>: <stmts> ... }")
     ("byte" summary "Integral primitive type (-128 to 127)")
     ("break" summary "break [<label>] ;")
     ("bool" summary "Primitive logical quantity type (true or false)")
     ("base" summary "")
     ("as" summary "")
     ("add" summary "")
     ("abstract" summary "Class|Method declaration modifier: abstract {class|<type>} <name> ...")))
  "Table of language keywords.")

(defconst wisent-csharp-wy--token-table
  (semantic-lex-make-type-table
   '(("number"
      (NUMBER_LITERAL))
     ("string"
      (STRING_LITERAL))
     ("symbol"
      (IDENTIFIER)
      (BOOLEAN_LITERAL . "true")
      (BOOLEAN_LITERAL . "false")
      (NULL_LITERAL . "null"))
     ("punctuation"
      (COMP . "~")
      (OROR . "||")
      (OREQ . "|=")
      (OR . "|")
      (XOREQ . "^=")
      (XOR . "^")
      (QUESTION . "?")
      (RSHIFTEQ . ">>=")
      (RSHIFT . ">>")
      (GTEQ . ">=")
      (GT . ">")
      (EQEQ . "==")
      (EQ . "=")
      (LTEQ . "<=")
      (LSHIFTEQ . "<<=")
      (LSHIFT . "<<")
      (LT . "<")
      (DEREF . "->")
      (SEMICOLON . ";")
      (COLON . ":")
      (DIVEQ . "/=")
      (DIV . "/")
      (DOT . ".")
      (MINUSEQ . "-=")
      (MINUSMINUS . "--")
      (MINUS . "-")
      (COMMA . ",")
      (PLUSEQ . "+=")
      (PLUSPLUS . "++")
      (PLUS . "+")
      (MULTEQ . "*=")
      (MULT . "*")
      (ANDEQ . "&=")
      (ANDAND . "&&")
      (AND . "&")
      (MODEQ . "%=")
      (MOD . "%")
      (NOTEQ . "!=")
      (NOT . "!"))
     ("semantic-list"
      (BRACK_BLOCK . "^\\[")
      (BRACE_BLOCK . "^{")
      (PAREN_BLOCK . "^("))
     ("close-paren"
      (RBRACK . "]")
      (RBRACE . "}")
      (RPAREN . ")"))
     ("open-paren"
      (LBRACK . "[")
      (LBRACE . "{")
      (LPAREN . "(")))
   'nil)
  "Table of lexical tokens.")

(defconst wisent-csharp-wy--parse-table
  (progn
    (eval-when-compile
      (require 'wisent-comp))
    (wisent-compile-grammar
     '((LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK PAREN_BLOCK BRACE_BLOCK BRACK_BLOCK NOT NOTEQ MOD MODEQ AND ANDAND ANDEQ MULT MULTEQ PLUS PLUSPLUS PLUSEQ COMMA MINUS MINUSMINUS MINUSEQ DOT DIV DIVEQ COLON SEMICOLON DEREF LT LSHIFT LSHIFTEQ LTEQ EQ EQEQ GT GTEQ RSHIFT RSHIFTEQ QUESTION XOR XOREQ OR OREQ OROR COMP NULL_LITERAL BOOLEAN_LITERAL IDENTIFIER STRING_LITERAL NUMBER_LITERAL ABSTRACT ADD AS BASE BOOL BREAK BYTE CASE CATCH CHAR CHECKED CLASS CONST CONTINUE DECIMAL DEFAULT DELEGATE DO DOUBLE ELSE ENUM EVENT EXPLICIT EXTERN FINALLY FIXED FLOAT FOR FOREACH GET GOTO IF IMPLICIT IN INT INTERFACE INTERNAL IS LOCK LONG NAMESPACE NEW OBJECT OPERATOR OUT OVERRIDE PARAMS PRIVATE PROTECTED PUBLIC READONLY REF REMOVE RETURN SBYTE SEALED SET SHORT SIZEOF STACKALLOC STATIC STRING STRUCT SWITCH THIS THROW TRY TYPEOF UINT ULONG UNCHECKED UNSAFE USHORT USING VIRTUAL VOID VOLATILE WHILE)
       nil
       (literal
	((NULL_LITERAL))
	((BOOLEAN_LITERAL))
	((STRING_LITERAL))
	((NUMBER_LITERAL)))
       (integral_type
	((SBYTE))
	((BYTE))
	((SHORT))
	((USHORT))
	((INT))
	((UINT))
	((LONG))
	((ULONG))
	((CHAR)))
       (predefined_type
	((BOOL))
	((BYTE))
	((CHAR))
	((DECIMAL))
	((DOUBLE))
	((FLOAT))
	((INT))
	((LONG))
	((OBJECT))
	((SBYTE))
	((SHORT))
	((STRING))
	((UINT))
	((ULONG))
	((USHORT)))
       (type
	((qualified_name rank_specifiers_opt)
	 (concat $1 $2))
	((predefined_type rank_specifiers_opt)
	 (concat $1 $2)))
       (array_type
	((qualified_name rank_specifiers)
	 (concat $1 $2))
	((predefined_type rank_specifiers)
	 (concat $1 $2)))
       (rank_specifiers_opt
	(nil
	 (identity ""))
	((rank_specifiers)))
       (rank_specifiers
	((rank_specifiers BRACK_BLOCK)
	 (concat $1 "[]"))
	((BRACK_BLOCK)
	 (identity "[]")))
       (qualified_name
	((qualified_name DOT IDENTIFIER)
	 (concat $1 "." $3))
	((IDENTIFIER)))
       (qualified_name_list
	((qualified_name_list COMMA qualified_name)
	 (cons $3 $1))
	((qualified_name)
	 (list $1)))
       (argument_list_opt
	(nil)
	((argument_list)))
       (argument_list
	((argument_list COMMA argument))
	((argument)))
       (argument
	((REF expression))
	((OUT expression))
	((expression)))
       (expression
	((expression term))
	((term)))
       (constant_expression
	((expression)))
       (term
	((literal))
	((operator))
	((predefined_type))
	((IDENTIFIER))
	((BRACK_BLOCK))
	((PAREN_BLOCK))
	((BRACE_BLOCK))
	((NEW))
	((CLASS))
	((THIS))
	((BASE)))
       (operator
	((NOT))
	((PLUS))
	((PLUSPLUS))
	((MINUS))
	((MINUSMINUS))
	((NOTEQ))
	((MOD))
	((MODEQ))
	((AND))
	((ANDAND))
	((ANDEQ))
	((MULT))
	((MULTEQ))
	((PLUSEQ))
	((MINUSEQ))
	((DOT))
	((DIV))
	((DIVEQ))
	((COLON))
	((LT))
	((LSHIFT))
	((LSHIFTEQ))
	((LTEQ))
	((EQ))
	((EQEQ))
	((GT))
	((GTEQ))
	((RSHIFT))
	((RSHIFTEQ))
	((QUESTION))
	((XOR))
	((XOREQ))
	((OR))
	((OREQ))
	((OROR))
	((COMP))
	((IS))
	((AS)))
       (compilation_unit
	((using_directive))
	((namespace_declaration))
	((type_declaration)))
       (using_directive
	((USING IDENTIFIER EQ qualified_name SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-include $4 nil 'alias $2)))
	((USING qualified_name SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-include $2 nil))))
       (namespace_declaration
	((NAMESPACE qualified_name namespace_body)
	 (wisent-raw-tag
	  (semantic-tag-new-type $2 $1 $3 nil))))
       (namespace_body
	((BRACE_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'namespace_member_declaration 1)))
       (namespace_member_declaration
	((LBRACE)
	 nil)
	((RBRACE)
	 nil)
	((block)
	 nil)
	((using_directive))
	((namespace_declaration))
	((type_declaration)))
       (type_declaration
	((SEMICOLON)
	 nil)
	((class_declaration))
	((struct_declaration))
	((interface_declaration))
	((enum_declaration))
	((delegate_declaration)))
       (modifiers_opt
	(nil)
	((modifiers)
	 (nreverse $1)))
       (modifiers
	((modifiers modifier)
	 (cons $2 $1))
	((modifier)
	 (list $1)))
       (modifier
	((NEW))
	((PUBLIC))
	((PROTECTED))
	((INTERNAL))
	((PRIVATE))
	((ABSTRACT))
	((SEALED))
	((STATIC))
	((READONLY))
	((VOLATILE))
	((VIRTUAL))
	((OVERRIDE))
	((EXTERN))
	((IMPLICIT))
	((EXPLICIT)))
       (class_bases_opt
	(nil)
	((COLON qualified_name)
	 (cons $2 nil))
	((COLON qualified_name COMMA qualified_name_list)
	 (cons $2
	       (nreverse $4))))
       (interfaces_opt
	(nil)
	((COLON qualified_name_list)
	 (nreverse $2)))
       (class_declaration
	((attrs_opt modifiers_opt CLASS IDENTIFIER class_bases_opt class_body)
	 (wisent-raw-tag
	  (semantic-tag-new-type $4 $3 $6 $5 'typemodifiers $2))))
       (class_body
	((BRACE_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'class_member_declaration 1)))
       (class_member_declaration
	((LBRACE)
	 nil)
	((RBRACE)
	 nil)
	((block)
	 nil)
	((const_declaration))
	((field_declaration))
	((method_declaration))
	((property_declaration))
	((event_declaration))
	((indexer_declaration))
	((operator_declaration))
	((constructor_declaration))
	((static_constructor_declaration))
	((type_declaration)))
       (const_declaration
	((attrs_opt modifiers_opt CONST type const_declarators SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $5 $4 nil 'const t 'typemodifiers $2))))
       (const_declarators
	((const_declarators COMMA const_declarator)
	 (cons $3 $1))
	((const_declarator)
	 (list $1)))
       (const_declarator
	((IDENTIFIER EQ constant_expression)
	 (list $1 $3 $region)))
       (field_declaration
	((attrs_opt modifiers_opt type variable_declarators SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $4 $3 nil 'typemodifiers $2))))
       (variable_declarators
	((variable_declarators COMMA variable_declarator)
	 (cons $3 $1))
	((variable_declarator)
	 (list $1)))
       (variable_declarator
	((IDENTIFIER EQ variable_initializer)
	 (list $1 nil $region))
	((IDENTIFIER)
	 (list $1 nil $region)))
       (variable_initializer
	((expression)))
       (method_declaration
	((attrs_opt modifiers_opt VOID method_declarator method_body)
	 (wisent-raw-tag
	  (semantic-tag-new-function
	   (car $4)
	   $3
	   (cdr $4)
	   'typemodifiers $2)))
	((attrs_opt modifiers_opt type method_declarator method_body)
	 (wisent-raw-tag
	  (semantic-tag-new-function
	   (car $4)
	   $3
	   (cdr $4)
	   'typemodifiers $2))))
       (method_declarator
	((qualified_name formal_parameter_list)
	 (cons $1 $2)))
       (method_body
	((SEMICOLON))
	((block)))
       (formal_parameter_list
	((PAREN_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'formal_parameters 1)))
       (formal_parameters
	((LPAREN)
	 nil)
	((RPAREN)
	 nil)
	((fixed_parameter COMMA))
	((fixed_parameter RPAREN))
	((parameter_array RPAREN)))
       (parameter_modifier_opt
	(nil)
	((REF))
	((OUT)))
       (fixed_parameter
	((attrs_opt parameter_modifier_opt type IDENTIFIER)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $4 $3 nil 'typemodifiers
				     (if $2
					 (list $2))))))
       (parameter_array
	((attrs_opt PARAMS array_type IDENTIFIER)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $4 $3 nil 'typemodifiers
				     (if $2
					 (list $2))))))
       (property_declaration
	((attrs_opt modifiers_opt type qualified_name accessor_list)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $4 $3 nil 'typemodifiers $2 'property t 'accessors $5))))
       (accessor_list
	((BRACE_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'accessor_declaration 1)))
       (accessor_declaration
	((LBRACE)
	 nil)
	((RBRACE)
	 nil)
	((get_accessor_declaration))
	((set_accessor_declaration)))
       (get_accessor_declaration
	((attrs_opt GET block)
	 (wisent-raw-tag
	  (semantic-tag-new-function $2 nil nil)))
	((attrs_opt GET SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-function $2 nil nil 'typemodifiers
				     (list "abstract")))))
       (set_accessor_declaration
	((attrs_opt SET block)
	 (wisent-raw-tag
	  (semantic-tag-new-function $2 nil nil)))
	((attrs_opt SET SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-function $2 nil nil 'typemodifiers
				     (list "abstract")))))
       (event_declaration
	((attrs_opt modifiers_opt EVENT type variable_declarators SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $5 $4 nil 'typemodifiers
				     (cons "event" $2))))
	((attrs_opt modifiers_opt EVENT type qualified_name event_accessor_list)
	 (wisent-raw-tag
	  (semantic-tag-new-function $5 $4 nil 'typemodifiers
				     (cons "event" $2)))))
       (event_accessor_list
	((BRACE_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'event_accessor_declaration 1)))
       (event_accessor_declaration
	((LBRACE)
	 nil)
	((RBRACE)
	 nil)
	((add_accessor_declaration))
	((remove_accessor_declaration)))
       (add_accessor_declaration
	((attrs_opt ADD block)
	 (identity $2))
	((attrs_opt ADD SEMICOLON)
	 (identity $2)))
       (remove_accessor_declaration
	((attrs_opt REMOVE block)
	 (identity $2))
	((attrs_opt REMOVE SEMICOLON)
	 (identity $2)))
       (indexer_declaration
	((attrs_opt modifiers_opt type indexer_declarator accessor_list)
	 (wisent-raw-tag
	  (semantic-tag-new-function
	   (car $4)
	   $3
	   (cdr $4)
	   'typemodifiers $2 'accessors $5))))
       (indexer_declarator
	((THIS indexer_parameter_list)
	 (cons "this[]" $2))
	((qualified_name DOT THIS indexer_parameter_list)
	 (cons
	  (concat $1 ".this[]")
	  $4)))
       (indexer_parameter_list
	((BRACK_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'indexer_parameters 1)))
       (indexer_parameters
	((LBRACK)
	 nil)
	((RBRACK)
	 nil)
	((fixed_parameter COMMA))
	((fixed_parameter RBRACK))
	((parameter_array RBRACK)))
       (overloadable_operator
	((PLUS))
	((MINUS))
	((COMP))
	((PLUSPLUS))
	((MINUSMINUS))
	((BOOLEAN_LITERAL))
	((MULT))
	((DIV))
	((MOD))
	((AND))
	((OR))
	((XOR))
	((LSHIFT))
	((RSHIFT))
	((EQEQ))
	((NOTEQ))
	((GT))
	((LT))
	((GTEQ))
	((LTEQ)))
       (operator_declaration
	((attrs_opt modifiers_opt operator_declarator block)
	 (wisent-raw-tag
	  (semantic-tag-new-function
	   (concat "op"
		   (nth 0 $3))
	   (nth 1 $3)
	   (nth 2 $3)
	   'typemodifiers $2))))
       (operator_declarator
	((type OPERATOR overloadable_operator formal_parameter_list)
	 (list $3 $1 $4))
	((OPERATOR type formal_parameter_list)
	 (list
	  (concat "(" $2 ")")
	  $2 $3)))
       (constructor_declaration
	((attrs_opt modifiers_opt constructor_declarator block)
	 (wisent-raw-tag
	  (semantic-tag-new-function
	   (car $3)
	   nil
	   (cdr $3)
	   'typemodifiers $2))))
       (constructor_declarator
	((IDENTIFIER formal_parameter_list constructor_initializer_opt)
	 (cons $1 $2)))
       (constructor_initializer_opt
	(nil)
	((COLON BASE LPAREN argument_list_opt RPAREN))
	((COLON THIS LPAREN argument_list_opt RPAREN)))
       (static_constructor_declaration
	((attrs_opt modifiers_opt IDENTIFIER LPAREN RPAREN block)
	 (wisent-raw-tag
	  (semantic-tag-new-function $3 nil nil 'typemodifiers $2))))
       (extern_opt
	(nil)
	((EXTERN)))
       (destructor_declaration
	((attrs_opt extern_opt COMP IDENTIFIER LPAREN RPAREN block)
	 (wisent-raw-tag
	  (semantic-tag-new-function
	   (concat $3 $4)
	   nil nil 'typemodifiers $2))))
       (struct_declaration
	((attrs_opt modifiers_opt STRUCT IDENTIFIER interfaces_opt struct_body)
	 (wisent-raw-tag
	  (semantic-tag-new-type $4 $3 $6
				 (if $5
				     (cons nil $5))
				 'typemodifiers $2))))
       (struct_body
	((BRACE_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'struct_member_declaration 1)))
       (struct_member_declaration
	((LBRACE)
	 nil)
	((RBRACE)
	 nil)
	((block)
	 nil)
	((const_declaration))
	((field_declaration))
	((method_declaration))
	((property_declaration))
	((event_declaration))
	((indexer_declaration))
	((operator_declaration))
	((constructor_declaration))
	((static_constructor_declaration))
	((type_declaration)))
       (interface_declaration
	((attrs_opt modifiers_opt INTERFACE IDENTIFIER interfaces_opt interface_body)
	 (wisent-raw-tag
	  (semantic-tag-new-type $4 $3 $6
				 (if $5
				     (cons nil $5))
				 'typemodifiers $2))))
       (interface_body
	((BRACE_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'interface_member_declaration 1)))
       (interface_member_declaration
	((LBRACE)
	 nil)
	((RBRACE)
	 nil)
	((block)
	 nil)
	((interface_method_declaration))
	((interface_property_declaration))
	((interface_event_declaration))
	((interface_indexer_declaration)))
       (new_opt
	(nil)
	((NEW)))
       (interface_method_declaration
	((attrs_opt new_opt type IDENTIFIER formal_parameter_list SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-function $4 $3 $5 'typemodifiers $2)))
	((attrs_opt new_opt VOID IDENTIFIER formal_parameter_list SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-function $4 $3 $5 'typemodifiers $2))))
       (interface_property_declaration
	((attrs_opt new_opt type IDENTIFIER accessor_list)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $4 $3 nil 'typemodifiers
				     (if $2
					 (list $2))
				     'property t 'accessors $5))))
       (interface_event_declaration
	((attrs_opt new_opt EVENT type IDENTIFIER SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-function $5 $4 nil 'typemodifiers
				     (if $2
					 (list $2 $3)
				       (list $3))))))
       (interface_indexer_declaration
	((attrs_opt new_opt type THIS indexer_parameter_list accessor_list)
	 (wisent-raw-tag
	  (semantic-tag-new-function "this[]" $3 $5 'typemodifiers $2 'accessors $6))))
       (enum_declaration
	((attrs_opt modifiers_opt ENUM IDENTIFIER COLON integral_type enum_body)
	 (wisent-raw-tag
	  (semantic-tag-new-type $4 $3 $7
				 (cons $6 nil)
				 'typemodifiers $2)))
	((attrs_opt modifiers_opt ENUM IDENTIFIER enum_body)
	 (wisent-raw-tag
	  (semantic-tag-new-type $4 $3 $5 nil 'typemodifiers $2))))
       (enum_body
	((BRACE_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'enum_members 1)))
       (enum_members
	((LBRACE)
	 nil)
	((RBRACE)
	 nil)
	((block)
	 nil)
	((enum_member_declaration COMMA))
	((enum_member_declaration RBRACE)))
       (enum_member_declaration
	((attrs_opt IDENTIFIER)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $2 nil nil 'const t)))
	((attrs_opt IDENTIFIER EQ constant_expression)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $2 nil $4 'const t))))
       (delegate_declaration
	((attrs_opt modifiers_opt DELEGATE type IDENTIFIER formal_parameter_list SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-type $5 $3 nil nil 'typemodifiers $2)))
	((attrs_opt modifiers_opt DELEGATE VOID IDENTIFIER formal_parameter_list SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-type $5 $3 nil nil 'typemodifiers $2))))
       (attrs_opt
	(nil)
	((attr_sections)
	 (apply 'nconc
		(nreverse $1))))
       (attr_sections
	((attr_sections attr_section)
	 (cons $2 $1))
	((attr_section)
	 (list $1)))
       (attr_section
	((BRACK_BLOCK)
	 nil))
       (local_variable_declaration
	((type variable_declarators SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $2 $1 nil)))
	((CONST type const_declarators SEMICOLON)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $3 $2 nil 'const t)))
	((FOREACH PAREN_BLOCK)
	 (semantic-parse-region
	  (car $region2)
	  (cdr $region2)
	  'local_foreach_declaration 1)))
       (local_foreach_declaration
	((LPAREN type IDENTIFIER IN expression RPAREN)
	 (wisent-raw-tag
	  (semantic-tag-new-variable $3 $2 nil))))
       (block
	   ((BRACE_BLOCK))))
     '(compilation_unit using_directive namespace_declaration class_declaration const_declaration field_declaration constructor_declaration method_declaration property_declaration indexer_declaration event_declaration fixed_parameter interface_declaration delegate_declaration namespace_member_declaration class_member_declaration struct_member_declaration interface_member_declaration accessor_declaration event_accessor_declaration formal_parameters indexer_parameters enum_members local_variable_declaration local_foreach_declaration)))
  "Parser table.")

(defun wisent-csharp-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table wisent-csharp-wy--parse-table
	semantic-debug-parser-source "wisent-csharp.wy"
	semantic-flex-keywords-obarray wisent-csharp-wy--keyword-table
	semantic-lex-types-obarray wisent-csharp-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
	    'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(require 'semantic-lex)


;;; Epilogue
;;

(provide 'wisent-csharp-wy)

;;; wisent-csharp-wy.el ends here
