;;; wisent-java-wy.el --- Generated parser support file

;; Copyright (C) 2002, 2003, 2004 David Ponce

;; Author: Steve Purcell <steve@Monaco.local>
;; Created: 2006-06-22 14:50:16+0200
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
;; generated from the grammar file wisent-java.wy.

;;; History:
;;

;;; Code:

;;; Prologue
;;
;; Stack of enum names in scope.
  (defvar wisent-java-wy--enums nil)
  ;; Stack of anonymous class declarations found in an expression.
  (defvar wisent-java-wy--anons nil)

;;; Declarations
;;
(defconst wisent-java-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("abstract" . ABSTRACT)
     ("assert" . ASSERT)
     ("boolean" . BOOLEAN)
     ("break" . BREAK)
     ("byte" . BYTE)
     ("case" . CASE)
     ("catch" . CATCH)
     ("char" . CHAR)
     ("class" . CLASS)
     ("const" . CONST)
     ("continue" . CONTINUE)
     ("default" . DEFAULT)
     ("do" . DO)
     ("double" . DOUBLE)
     ("else" . ELSE)
     ("enum" . ENUM)
     ("extends" . EXTENDS)
     ("final" . FINAL)
     ("finally" . FINALLY)
     ("float" . FLOAT)
     ("for" . FOR)
     ("goto" . GOTO)
     ("if" . IF)
     ("implements" . IMPLEMENTS)
     ("import" . IMPORT)
     ("instanceof" . INSTANCEOF)
     ("int" . INT)
     ("interface" . INTERFACE)
     ("long" . LONG)
     ("native" . NATIVE)
     ("new" . NEW)
     ("package" . PACKAGE)
     ("private" . PRIVATE)
     ("protected" . PROTECTED)
     ("public" . PUBLIC)
     ("return" . RETURN)
     ("short" . SHORT)
     ("static" . STATIC)
     ("strictfp" . STRICTFP)
     ("super" . SUPER)
     ("switch" . SWITCH)
     ("synchronized" . SYNCHRONIZED)
     ("this" . THIS)
     ("throw" . THROW)
     ("throws" . THROWS)
     ("transient" . TRANSIENT)
     ("try" . TRY)
     ("void" . VOID)
     ("volatile" . VOLATILE)
     ("while" . WHILE)
     ("@author" . _AUTHOR)
     ("@version" . _VERSION)
     ("@param" . _PARAM)
     ("@return" . _RETURN)
     ("@exception" . _EXCEPTION)
     ("@throws" . _THROWS)
     ("@see" . _SEE)
     ("@since" . _SINCE)
     ("@serial" . _SERIAL)
     ("@serialData" . _SERIALDATA)
     ("@serialField" . _SERIALFIELD)
     ("@deprecated" . _DEPRECATED))
   '(("@deprecated" javadoc
      (seq 12 usage
	   (type function variable)
	   opt t))
     ("@serialField" javadoc
      (seq 11 usage
	   (variable)
	   opt t))
     ("@serialData" javadoc
      (seq 10 usage
	   (function)
	   opt t))
     ("@serial" javadoc
      (seq 9 usage
	   (variable)
	   opt t))
     ("@since" javadoc
      (seq 8 usage
	   (type function variable)
	   opt t))
     ("@see" javadoc
      (seq 7 usage
	   (type function variable)
	   opt t with-ref t))
     ("@throws" javadoc
      (seq 6 usage
	   (function)
	   with-name t))
     ("@exception" javadoc
      (seq 5 usage
	   (function)
	   with-name t))
     ("@return" javadoc
      (seq 4 usage
	   (function)))
     ("@param" javadoc
      (seq 3 usage
	   (function)
	   with-name t))
     ("@version" javadoc
      (seq 2 usage
	   (type)))
     ("@author" javadoc
      (seq 1 usage
	   (type)))
     ("while" summary "while (<expr>) <stmt> | do <stmt> while (<expr>);")
     ("volatile" summary "Field declaration modifier: volatile <type> <name> ...")
     ("void" summary "Method return type: void <name> ...")
     ("try" summary "try {<stmts>} [catch(<parm>) {<stmts>} ...] [finally {<stmts>}]")
     ("transient" summary "Field declaration modifier: transient <type> <name> ...")
     ("throws" summary "Method|Constructor declaration: throws <classType>, ...")
     ("throw" summary "throw <expr> ;")
     ("synchronized" summary "synchronized (<expr>) ... | Method decl. modifier: synchronized <type> <name> ...")
     ("switch" summary "switch(<expr>) {[case <const-expr>: <stmts> ...] [default: <stmts>]}")
     ("strictfp" summary "Declaration modifier: strictfp {class|interface|<type>} <name> ...")
     ("static" summary "Declaration modifier: static {class|interface|<type>} <name> ...")
     ("short" summary "Integral primitive type (-32768 to 32767)")
     ("return" summary "return [<expr>] ;")
     ("public" summary "Access level modifier: public {class|interface|<type>} <name> ...")
     ("protected" summary "Access level modifier: protected {class|interface|<type>} <name> ...")
     ("private" summary "Access level modifier: private {class|interface|<type>} <name> ...")
     ("package" summary "Package declaration: package <name>")
     ("native" summary "Method declaration modifier: native <type> <name> ...")
     ("long" summary "Integral primitive type (-9223372036854775808 to 9223372036854775807)")
     ("interface" summary "Interface declaration: interface <name>")
     ("int" summary "Integral primitive type (-2147483648 to 2147483647)")
     ("import" summary "Import package declarations: import <package>")
     ("implements" summary "Class SuperInterfaces declaration: implements <name> [, ...]")
     ("if" summary "if (<expr>) <stmt> [else <stmt>]")
     ("goto" summary "Unused reserved word")
     ("for" summary "for ([<init-expr>]; [<expr>]; [<update-expr>]) <stmt>")
     ("float" summary "Primitive floating-point type (single-precision 32-bit IEEE 754)")
     ("finally" summary "try {<stmts>} ... finally {<stmts>}")
     ("final" summary "Class|Member declaration modifier: final {class|<type>} <name> ...")
     ("extends" summary "SuperClass|SuperInterfaces declaration: extends <name> [, ...]")
     ("enum" summary "Enum declaration")
     ("else" summary "if (<expr>) <stmt> else <stmt>")
     ("double" summary "Primitive floating-point type (double-precision 64-bit IEEE 754)")
     ("do" summary "do <stmt> while (<expr>);")
     ("default" summary "switch(<expr>) { ... default: <stmts>}")
     ("continue" summary "continue [<label>] ;")
     ("const" summary "Unused reserved word")
     ("class" summary "Class declaration: class <name>")
     ("char" summary "Integral primitive type ('u0000' to 'uffff') (0 to 65535)")
     ("catch" summary "try {<stmts>} catch(<parm>) {<stmts>} ... ")
     ("case" summary "switch(<expr>) {case <const-expr>: <stmts> ... }")
     ("byte" summary "Integral primitive type (-128 to 127)")
     ("break" summary "break [<label>] ;")
     ("boolean" summary "Primitive logical quantity type (true or false)")
     ("assert" summary "Assertion statement: assert <expr> [,<expr> ...];")
     ("abstract" summary "Class|Method declaration modifier: abstract {class|<type>} <name> ...")))
  "Table of language keywords.")

(defconst wisent-java-wy--token-table
  (semantic-lex-make-type-table
   '(("number"
      (NUMBER_LITERAL))
     ("string"
      (STRING_LITERAL))
     ("symbol"
      (IDENTIFIER)
      (BOOLEAN_LITERAL . "\\`true\\'")
      (BOOLEAN_LITERAL . "\\`false\\'")
      (NULL_LITERAL . "\\`null\\'"))
     ("punctuation"
      (AT . "@")
      (ELLIPSIS . "...")
      (COMP . "~")
      (OROR . "||")
      (OREQ . "|=")
      (OR . "|")
      (XOREQ . "^=")
      (XOR . "^")
      (QUESTION . "?")
      (URSHIFTEQ . ">>>=")
      (URSHIFT . ">>>")
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
     ("close-paren"
      (RBRACK . "]")
      (RBRACE . "}")
      (RPAREN . ")"))
     ("open-paren"
      (LBRACK . "[")
      (LBRACE . "{")
      (LPAREN . "("))
     ("block"
      (BRACK_BLOCK . "(LBRACK RBRACK)")
      (BRACE_BLOCK . "(LBRACE RBRACE)")
      (PAREN_BLOCK . "(LPAREN RPAREN)")))
   '(("keyword" :declared t)
     ("number" :declared t)
     ("string" :declared t)
     ("symbol" :declared t)
     ("punctuation" :declared t)
     ("block" :declared t)))
  "Table of lexical tokens.")

(defconst wisent-java-wy--parse-table
  (progn
    (eval-when-compile
      (require 'wisent-comp))
    (wisent-compile-grammar
     '((PAREN_BLOCK BRACE_BLOCK BRACK_BLOCK LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK NOT NOTEQ MOD MODEQ AND ANDAND ANDEQ MULT MULTEQ PLUS PLUSPLUS PLUSEQ COMMA MINUS MINUSMINUS MINUSEQ DOT DIV DIVEQ COLON SEMICOLON LT LSHIFT LSHIFTEQ LTEQ EQ EQEQ GT GTEQ RSHIFT RSHIFTEQ URSHIFT URSHIFTEQ QUESTION XOR XOREQ OR OREQ OROR COMP ELLIPSIS AT NULL_LITERAL BOOLEAN_LITERAL IDENTIFIER STRING_LITERAL NUMBER_LITERAL ABSTRACT ASSERT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONST CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTENDS FINAL FINALLY FLOAT FOR GOTO IF IMPLEMENTS IMPORT INSTANCEOF INT INTERFACE LONG NATIVE NEW PACKAGE PRIVATE PROTECTED PUBLIC RETURN SHORT STATIC STRICTFP SUPER SWITCH SYNCHRONIZED THIS THROW THROWS TRANSIENT TRY VOID VOLATILE WHILE _AUTHOR _VERSION _PARAM _RETURN _EXCEPTION _THROWS _SEE _SINCE _SERIAL _SERIALDATA _SERIALFIELD _DEPRECATED)
       nil
       (goal
	((compilation_unit)))
       (literal
	((NUMBER_LITERAL))
	((BOOLEAN_LITERAL))
	((STRING_LITERAL))
	((NULL_LITERAL)))
       (type
	((primitive_type))
	((reference_type)))
       (primitive_type
	((numeric_type))
	((BOOLEAN)))
       (numeric_type
	((integral_type))
	((floating_point_type)))
       (integral_type
	((BYTE))
	((SHORT))
	((INT))
	((LONG))
	((CHAR)))
       (floating_point_type
	((FLOAT))
	((DOUBLE)))
       (reference_type
	((class_or_interface_type))
	((array_type)))
       (type_variable
	((IDENTIFIER)))
       (class_or_interface
	((name))
	((class_or_interface LT type_argument_list_1 DOT name)
	 (concat $1 $2 $3 $4 $5)))
       (class_or_interface_type
	((class_or_interface))
	((class_or_interface LT type_argument_list_1)
	 (concat $1 $2 $3)))
       (class_type
	((class_or_interface_type)))
       (interface_type
	((class_or_interface_type)))
       (array_type
	((primitive_type dims)
	 (concat $1 $2))
	((name dims)
	 (concat $1 $2))
	((class_or_interface LT type_argument_list_1 DOT name dims)
	 (concat $1 $2 $3 $4 $5 $6))
	((class_or_interface LT type_argument_list_1 dims)
	 (concat $1 $2 $3 $4)))
       (type_arguments_opt
	((type_arguments))
	(nil
	 (progn "")))
       (type_arguments
	((LT type_argument_list_1)
	 (concat $1 $2)))
       (wildcard
	((QUESTION))
	((QUESTION EXTENDS reference_type)
	 (concat $1 $2 $3))
	((QUESTION SUPER reference_type)
	 (concat $1 $2 $3)))
       (wildcard_1
	((QUESTION GT)
	 (concat $1 $2))
	((QUESTION EXTENDS reference_type_1)
	 (concat $1 $2 $3))
	((QUESTION SUPER reference_type_1)
	 (concat $1 $2 $3)))
       (wildcard_2
	((QUESTION RSHIFT)
	 (concat $1 $2))
	((QUESTION EXTENDS reference_type_2)
	 (concat $1 $2 $3))
	((QUESTION SUPER reference_type_2)
	 (concat $1 $2 $3)))
       (wildcard_3
	((QUESTION URSHIFT)
	 (concat $1 $2))
	((QUESTION EXTENDS reference_type_3)
	 (concat $1 $2 $3))
	((QUESTION SUPER reference_type_3)
	 (concat $1 $2 $3)))
       (reference_type_1
	((reference_type GT)
	 (concat $1 $2))
	((class_or_interface LT type_argument_list_2)
	 (concat $1 $2 $3)))
       (reference_type_2
	((reference_type RSHIFT)
	 (concat $1 $2))
	((class_or_interface LT type_argument_list_3)
	 (concat $1 $2 $3)))
       (reference_type_3
	((reference_type URSHIFT)
	 (concat $1 $2)))
       (type_argument_list
	((type_argument))
	((type_argument_list COMMA type_argument)
	 (concat $1 $2 $3)))
       (type_argument_list_1
	((type_argument_1))
	((type_argument_list COMMA type_argument_1)
	 (concat $1 $2 $3)))
       (type_argument_list_2
	((type_argument_2))
	((type_argument_list COMMA type_argument_2)
	 (concat $1 $2 $3)))
       (type_argument_list_3
	((type_argument_3))
	((type_argument_list COMMA type_argument_3)
	 (concat $1 $2 $3)))
       (type_argument
	((reference_type))
	((wildcard)))
       (type_argument_1
	((reference_type_1))
	((wildcard_1)))
       (type_argument_2
	((reference_type_2))
	((wildcard_2)))
       (type_argument_3
	((reference_type_3))
	((wildcard_3)))
       (name
	((simple_name))
	((qualified_name)))
       (simple_name
	((IDENTIFIER)))
       (qualified_name
	((name DOT IDENTIFIER)
	 (concat $1 $2 $3)))
       (compilation_unit
	((package_declaration))
	((import_declaration))
	((type_declaration)))
       (package_declaration
	((modifiers PACKAGE name SEMICOLON)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-package $3 nil))))
	((PACKAGE name SEMICOLON)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-package $2 nil)))))
       (import_declaration
	((IMPORT name SEMICOLON)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-include $2 nil))))
	((IMPORT name DOT MULT SEMICOLON)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-include
	    (concat $2 $3 $4)
	    nil))))
	((IMPORT STATIC name SEMICOLON)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-include $3 nil :static-flag t))))
	((IMPORT STATIC name DOT MULT SEMICOLON)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-include
	    (concat $3 $4 $5)
	    nil :static-flag t)))))
       (type_declaration
	((class_declaration))
	((interface_declaration))
	((enum_declaration))
	((annotation_type_declaration)
	 nil)
	((SEMICOLON)
	 nil))
       (modifiers_opt
	(nil)
	((modifiers)))
       (modifiers
	((modifier)
	 (list $1))
	((modifiers modifier)
	 (append $1
		 (list $2))))
       (modifier
	((PUBLIC))
	((PROTECTED))
	((PRIVATE))
	((STATIC))
	((ABSTRACT))
	((FINAL))
	((NATIVE))
	((SYNCHRONIZED))
	((TRANSIENT))
	((VOLATILE))
	((STRICTFP))
	((annotation)))
       (class_declaration
	((modifiers_opt CLASS IDENTIFIER type_parameters_opt super_opt interfaces_opt class_body)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-type $3 $2 $7
				  (if
				      (or $5 $6)
				      (cons $5 $6))
				  :typemodifiers $1 :template-specifier $4)))))
       (super
	((EXTENDS class_type)
	 (progn $2)))
       (super_opt
	(nil)
	((super)))
       (interfaces
	((IMPLEMENTS interface_type_list)
	 (progn $2)))
       (interfaces_opt
	(nil)
	((interfaces)
	 (nreverse $1)))
       (interface_type_list
	((interface_type)
	 (list $1))
	((interface_type_list COMMA interface_type)
	 (cons $3 $1)))
       (class_body
	((LBRACE class_body_declarations_opt RBRACE)
	 (progn $2)))
       (class_body_opt
	(nil)
	((class_body)))
       (class_body_declarations_opt
	(nil)
	((class_body_declarations)
	 (apply 'nconc
		(nreverse $1))))
       (class_body_declarations
	((class_body_declaration)
	 (list $1))
	((class_body_declarations class_body_declaration)
	 (cons $2 $1)))
       (class_body_declaration
	((class_member_declaration))
	((static_initializer))
	((constructor_declaration))
	((block))
	((error)
	 (wisent-skip-token)))
       (class_member_declaration
	((field_declaration))
	((method_declaration))
	((class_declaration))
	((enum_declaration))
	((interface_declaration))
	((SEMICOLON)
	 nil))
       (enum_declaration
	((modifiers_opt ENUM IDENTIFIER interfaces_opt
			(push $3 wisent-java-wy--enums)
			enum_body)
	 (progn
	   (pop wisent-java-wy--enums)
	   (wisent-cook-tag
	    (wisent-raw-tag
	     (semantic-tag-new-type $3 $2 $6
				    (if $4
					(cons nil $4))
				    :typemodifiers $1))))))
       (enum_body
	((LBRACE enum_constants_opt enum_body_declarations_opt RBRACE)
	 (nconc $2 $3))
	((LBRACE error)
	 (wisent-skip-block)))
       (enum_constants_opt
	(nil)
	((enum_constants)
	 (apply 'nconc
		(nreverse $1))))
       (enum_constants
	((enum_constant)
	 (list $1))
	((enum_constants COMMA enum_constant)
	 (cons $3 $1)))
       (enum_constant
	((IDENTIFIER enum_arguments_opt)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-type $1
				  (car wisent-java-wy--enums)
				  nil nil :enum-constant-flag t))))
	((IDENTIFIER enum_arguments_opt class_body)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-type $1
				  (car wisent-java-wy--enums)
				  $3 nil :enum-constant-flag t)))))
       (enum_arguments_opt
	(nil)
	((LPAREN argument_list_opt RPAREN))
	((LPAREN error)
	 (wisent-skip-block)))
       (enum_body_declarations_opt
	(nil)
	((SEMICOLON class_body_declarations_opt)
	 (progn $2)))
       (field_declaration
	((modifiers_opt type variable_declarators SEMICOLON)
	 (let*
	     ((decls
	       (mapcar 'car $3))
	      (anons
	       (apply 'nconc
		      (nreverse
		       (mapcar 'cdr $3)))))
	   (wisent-cook-tag
	    (wisent-raw-tag
	     (semantic-tag-new-variable decls $2 nil :typemodifiers $1 :members anons))))))
       (variable_declarators
	((variable_declarator)
	 (list $1))
	((variable_declarators COMMA variable_declarator)
	 (cons $3 $1)))
       (variable_declarator
	((variable_declarator_id)
	 (cons
	  (cons $1 $region)
	  nil))
	((variable_declarator_id
	  (setq wisent-java-wy--anons nil)
	  EQ variable_initializer)
	 (cons
	  (cons $1 $region)
	  wisent-java-wy--anons)))
       (variable_declarator_id
	((IDENTIFIER))
	((variable_declarator_id LBRACK RBRACK)
	 (concat $1 $2 $3)))
       (variable_initializer
	((expression))
	((array_initializer)))
       (method_declaration
	((method_header method_body)
	 (let
	     ((tag
	       (eval $1)))
	   (semantic-tag-put-attribute tag :body $2)
	   (wisent-cook-tag tag))))
       (method_header
	((modifiers_opt type method_declarator throws_opt)
	 `(wisent-raw-tag
	   (semantic-tag-new-function ',(car $3)
				      ',$2 ',(cdr $3)
				      :typemodifiers ',$1 :throws ',$4)))
	((modifiers_opt LT type_parameter_list_1 type method_declarator throws_opt)
	 `(wisent-raw-tag
	   (semantic-tag-new-function ',(car $5)
				      ',$4 ',(cdr $5)
				      :typemodifiers ',$1 :template-specifier ',$3 :throws ',$6)))
	((modifiers_opt VOID method_declarator throws_opt)
	 `(wisent-raw-tag
	   (semantic-tag-new-function ',(car $3)
				      ',$2 ',(cdr $3)
				      :typemodifiers ',$1 :throws ',$4)))
	((modifiers_opt LT type_parameter_list_1 VOID method_declarator throws_opt)
	 `(wisent-raw-tag
	   (semantic-tag-new-function ',(car $5)
				      ',$4 ',(cdr $5)
				      :typemodifiers ',$1 :template-specifier ',$3 :throws ',$6))))
       (method_declarator
	((IDENTIFIER LPAREN formal_parameter_list_opt RPAREN)
	 (cons $1 $3))
	((IDENTIFIER LPAREN error)
	 (wisent-skip-block))
	((method_declarator LBRACK RBRACK)
	 (cons
	  (concat
	   (car $1)
	   $2 $3)
	  (cdr $1))))
       (formal_parameter_list_opt
	(nil)
	((formal_parameter_list)
	 (apply 'nconc
		(nreverse $1))))
       (formal_parameter_list
	((formal_parameter)
	 (list $1))
	((formal_parameter_list COMMA formal_parameter)
	 (cons $3 $1)))
       (formal_parameter
	((type variable_declarator_id)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-variable $2 $1 nil))))
	((FINAL type variable_declarator_id)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-variable $3 $2 nil :typemodifiers
				      (list $1)))))
	((type ELLIPSIS IDENTIFIER)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-variable $3 $1 nil :vararg-flag t))))
	((FINAL type ELLIPSIS IDENTIFIER)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-variable $4 $2 nil :typemodifiers
				      (list $1)
				      :vararg-flag t)))))
       (throws_opt
	(nil)
	((throws)))
       (throws
	((THROWS class_type_list)
	 (nreverse $2)))
       (class_type_list
	((class_type)
	 (list $1))
	((class_type_list COMMA class_type)
	 (cons $3 $1)))
       (method_body
	((block))
	((SEMICOLON)
	 nil))
       (static_initializer
	((STATIC block)
	 (when $2
	   (semantic-tag-put-attribute $2 :static-flag t)
	   $2)))
       (constructor_declaration
	((modifiers_opt constructor_declarator throws_opt constructor_body)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-function
	    (car $2)
	    nil
	    (cdr $2)
	    :typemodifiers $1 :throws $3 :constructor-flag t :body $4))))
	((modifiers_opt LT type_parameter_list_1 constructor_declarator throws_opt constructor_body)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-function
	    (car $4)
	    nil
	    (cdr $4)
	    :typemodifiers $1 :template-specifier $3 :throws $5 :constructor-flag t :body $6)))))
       (constructor_declarator
	((simple_name LPAREN formal_parameter_list_opt RPAREN)
	 (cons $1 $3))
	((simple_name LPAREN error)
	 (wisent-skip-block)))
       (constructor_body
	((LBRACE explicit_constructor_invocation block_statements RBRACE)
	 (progn $3))
	((LBRACE explicit_constructor_invocation RBRACE)
	 nil)
	((LBRACE block_statements RBRACE)
	 (progn $2))
	((LBRACE RBRACE)
	 nil)
	((LBRACE error)
	 (wisent-skip-block)))
       (explicit_constructor_invocation
	((THIS LPAREN argument_list_opt RPAREN SEMICOLON))
	((type_arguments THIS LPAREN argument_list_opt RPAREN SEMICOLON))
	((SUPER LPAREN argument_list_opt RPAREN SEMICOLON))
	((type_arguments SUPER LPAREN argument_list_opt RPAREN SEMICOLON))
	((primary DOT SUPER LPAREN argument_list_opt RPAREN SEMICOLON))
	((primary DOT type_arguments SUPER LPAREN argument_list_opt RPAREN SEMICOLON))
	((name DOT SUPER LPAREN argument_list_opt RPAREN SEMICOLON))
	((name DOT type_arguments SUPER LPAREN argument_list_opt RPAREN SEMICOLON)))
       (interface_declaration
	((modifiers_opt INTERFACE IDENTIFIER type_parameters_opt extends_interfaces_opt interface_body)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-type $3 $2 $6
				  (if $5
				      (cons nil $5))
				  :template-specifier $4 :typemodifiers $1)))))
       (extends_interfaces_opt
	(nil)
	((extends_interfaces)
	 (nreverse $1)))
       (extends_interfaces
	((EXTENDS interface_type)
	 (list $2))
	((extends_interfaces COMMA interface_type)
	 (cons $3 $1)))
       (interface_body
	((LBRACE interface_member_declarations_opt RBRACE)
	 (progn $2)))
       (interface_member_declarations_opt
	(nil)
	((interface_member_declarations)
	 (apply 'nconc
		(nreverse $1))))
       (interface_member_declarations
	((interface_member_declaration)
	 (list $1))
	((interface_member_declarations interface_member_declaration)
	 (cons $2 $1)))
       (interface_member_declaration
	((constant_declaration))
	((abstract_method_declaration))
	((class_declaration))
	((enum_declaration))
	((interface_declaration))
	((SEMICOLON)
	 nil)
	((error)
	 (wisent-skip-token)))
       (constant_declaration
	((field_declaration)))
       (abstract_method_declaration
	((method_header SEMICOLON)
	 (let
	     ((tag
	       (eval $1)))
	   (wisent-cook-tag tag))))
       (array_initializer
	((LBRACE variable_initializers COMMA RBRACE))
	((LBRACE variable_initializers RBRACE))
	((LBRACE COMMA RBRACE))
	((LBRACE RBRACE))
	((LBRACE error)
	 (wisent-skip-block)))
       (variable_initializers
	((variable_initializer))
	((variable_initializers COMMA variable_initializer)))
       (block
	   ((LBRACE block_statements_opt RBRACE)
	    (progn $2))
	 ((LBRACE error)
	  (wisent-skip-block)))
       (block_statements_opt
	(nil)
	((block_statements)))
       (block_statements
	((block_statements_reverse)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag "block" 'block :value
			 (apply 'nconc
				(nreverse $1)))))))
       (block_statements_reverse
	((block_statement)
	 (list $1))
	((block_statements_reverse block_statement)
	 (cons $2 $1)))
       (block_statement
	((local_variable_declaration_statement))
	((statement)
	 nil)
	((class_declaration))
	((enum_declaration))
	((interface_declaration)))
       (local_variable_declaration_statement
	((local_variable_declaration SEMICOLON)))
       (local_variable_declaration
	((type variable_declarators)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-variable $2 $1 nil))))
	((FINAL type variable_declarators)
	 (wisent-cook-tag
	  (wisent-raw-tag
	   (semantic-tag-new-variable $3 $2 nil :typemodifiers
				      (list $1))))))
       (statement
	((statement_without_trailing_substatement))
	((labeled_statement))
	((if_then_statement))
	((if_then_else_statement))
	((while_statement))
	((for_statement))
	((foreach_statement)))
       (statement_no_short_if
	((statement_without_trailing_substatement))
	((labeled_statement_no_short_if))
	((if_then_else_statement_no_short_if))
	((while_statement_no_short_if))
	((for_statement_no_short_if))
	((foreach_statement_no_short_if)))
       (statement_without_trailing_substatement
	((block))
	((empty_statement))
	((expression_statement))
	((switch_statement))
	((do_statement))
	((break_statement))
	((continue_statement))
	((return_statement))
	((synchronized_statement))
	((throw_statement))
	((try_statement))
	((assert_statement)))
       (empty_statement
	((SEMICOLON)))
       (labeled_statement
	((IDENTIFIER COLON statement)))
       (labeled_statement_no_short_if
	((IDENTIFIER COLON statement_no_short_if)))
       (expression_statement
	((statement_expression SEMICOLON)))
       (statement_expression
	((assignment))
	((preincrement_expression))
	((predecrement_expression))
	((postincrement_expression))
	((postdecrement_expression))
	((method_invocation))
	((class_instance_creation_expression)))
       (if_then_statement
	((IF LPAREN expression RPAREN statement)))
       (if_then_else_statement
	((IF LPAREN expression RPAREN statement_no_short_if ELSE statement)))
       (if_then_else_statement_no_short_if
	((IF LPAREN expression RPAREN statement_no_short_if ELSE statement_no_short_if)))
       (switch_statement
	((SWITCH LPAREN expression RPAREN switch_block)))
       (switch_block
	((LBRACE switch_block_statement_groups switch_labels RBRACE))
	((LBRACE switch_block_statement_groups RBRACE))
	((LBRACE switch_labels RBRACE))
	((LBRACE RBRACE))
	((LBRACE error)
	 (wisent-skip-block)))
       (switch_block_statement_groups
	((switch_block_statement_group))
	((switch_block_statement_groups switch_block_statement_group)))
       (switch_block_statement_group
	((switch_labels block_statements)))
       (switch_labels
	((switch_label))
	((switch_labels switch_label)))
       (switch_label
	((CASE constant_expression COLON))
	((DEFAULT COLON)))
       (while_statement
	((WHILE LPAREN expression RPAREN statement)))
       (while_statement_no_short_if
	((WHILE LPAREN expression RPAREN statement_no_short_if)))
       (do_statement
	((DO statement WHILE LPAREN expression RPAREN SEMICOLON)))
       (foreach_statement
	((FOR LPAREN type variable_declarator_id COLON expression RPAREN statement)
	 nil)
	((FOR IDENTIFIER LPAREN type variable_declarator_id IDENTIFIER expression RPAREN statement)
	 nil))
       (foreach_statement_no_short_if
	((FOR LPAREN type variable_declarator_id COLON expression RPAREN statement_no_short_if)
	 nil)
	((FOR IDENTIFIER LPAREN type variable_declarator_id IDENTIFIER expression RPAREN statement_no_short_if)
	 nil))
       (for_statement
	((FOR LPAREN for_init_opt SEMICOLON expression_opt SEMICOLON for_update_opt RPAREN statement)
	 (progn $3)))
       (for_statement_no_short_if
	((FOR LPAREN for_init_opt SEMICOLON expression_opt SEMICOLON for_update_opt RPAREN statement_no_short_if)
	 (progn $3)))
       (for_init_opt
	(nil)
	((for_init)))
       (for_init
	((statement_expression_list)
	 nil)
	((local_variable_declaration)))
       (for_update_opt
	(nil)
	((for_update)))
       (for_update
	((statement_expression_list)))
       (statement_expression_list
	((statement_expression))
	((statement_expression_list COMMA statement_expression)))
       (identifier_opt
	(nil)
	((IDENTIFIER)))
       (break_statement
	((BREAK identifier_opt SEMICOLON)))
       (continue_statement
	((CONTINUE identifier_opt SEMICOLON)))
       (return_statement
	((RETURN expression_opt SEMICOLON)))
       (throw_statement
	((THROW expression SEMICOLON)))
       (synchronized_statement
	((SYNCHRONIZED LPAREN expression RPAREN block)))
       (try_statement
	((TRY block catches))
	((TRY block catches_opt finally)))
       (catches_opt
	(nil)
	((catches)))
       (catches
	((catch_clause))
	((catches catch_clause)))
       (catch_clause
	((CATCH LPAREN formal_parameter RPAREN block)))
       (finally
	((FINALLY block)))
       (assert_statement
	((ASSERT expression SEMICOLON))
	((ASSERT expression COLON expression SEMICOLON)))
       (primary
	((primary_no_new_array))
	((array_creation_init))
	((array_creation_uninit)))
       (primary_no_new_array
	((literal))
	((THIS))
	((LPAREN name RPAREN))
	((LPAREN expression_no_name RPAREN))
	((class_instance_creation_expression))
	((field_access))
	((method_invocation))
	((array_access))
	((name DOT THIS))
	((VOID DOT CLASS))
	((primitive_type DOT CLASS))
	((primitive_type dims DOT CLASS))
	((name DOT CLASS))
	((name dims DOT CLASS)))
       (class_instance_creation_expression
	((NEW class_or_interface_type LPAREN argument_list_opt RPAREN class_body_opt)
	 (when $6
	   (setq wisent-java-wy--anons
		 (nconc wisent-java-wy--anons
			(wisent-cook-tag
			 (wisent-raw-tag
			  (semantic-tag-new-type
			   (concat $2 "$")
			   "class" $6 nil)))))))
	((NEW type_arguments class_or_interface_type LPAREN argument_list_opt RPAREN class_body_opt)
	 (when $7
	   (setq wisent-java-wy--anons
		 (nconc wisent-java-wy--anons
			(wisent-cook-tag
			 (wisent-raw-tag
			  (semantic-tag-new-type
			   (concat $3 "$")
			   "class" $7 nil)))))))
	((primary DOT NEW type_arguments_opt IDENTIFIER type_arguments_opt LPAREN argument_list_opt RPAREN class_body_opt)
	 (when $10
	   (setq wisent-java-wy--anons
		 (nconc wisent-java-wy--anons
			(wisent-cook-tag
			 (wisent-raw-tag
			  (semantic-tag-new-type
			   (concat $5 "$")
			   "class" $10 nil)))))))
	((name DOT NEW type_arguments_opt IDENTIFIER type_arguments_opt LPAREN argument_list_opt RPAREN class_body_opt)
	 (when $10
	   (setq wisent-java-wy--anons
		 (nconc wisent-java-wy--anons
			(wisent-cook-tag
			 (wisent-raw-tag
			  (semantic-tag-new-type
			   (concat $5 "$")
			   "class" $10 nil))))))))
       (argument_list_opt
	(nil)
	((argument_list)))
       (argument_list
	((expression))
	((argument_list COMMA expression)))
       (array_creation_uninit
	((NEW primitive_type dim_exprs dims_opt))
	((NEW class_or_interface_type dim_exprs dims_opt)))
       (array_creation_init
	((NEW primitive_type dims array_initializer))
	((NEW class_or_interface_type dims array_initializer)))
       (dim_exprs
	((dim_expr))
	((dim_exprs dim_expr)))
       (dim_expr
	((LBRACK expression RBRACK))
	((LBRACK error)
	 (wisent-skip-block)))
       (dims_opt
	(nil
	 (progn ""))
	((dims)))
       (dims
	((LBRACK RBRACK)
	 (concat $1 $2))
	((dims LBRACK RBRACK)
	 (concat $1 $2 $3)))
       (field_access
	((primary DOT IDENTIFIER))
	((SUPER DOT IDENTIFIER))
	((name DOT SUPER DOT IDENTIFIER)))
       (method_invocation
	((name LPAREN argument_list_opt RPAREN))
	((primary DOT IDENTIFIER LPAREN argument_list_opt RPAREN))
	((primary DOT type_arguments IDENTIFIER LPAREN argument_list_opt RPAREN))
	((name DOT type_arguments IDENTIFIER LPAREN argument_list_opt RPAREN))
	((SUPER DOT IDENTIFIER LPAREN argument_list_opt RPAREN))
	((SUPER DOT type_arguments IDENTIFIER LPAREN argument_list_opt RPAREN))
	((name DOT SUPER DOT IDENTIFIER LPAREN argument_list_opt RPAREN))
	((name DOT SUPER DOT type_arguments IDENTIFIER LPAREN argument_list_opt RPAREN)))
       (array_access
	((name LBRACK expression RBRACK))
	((primary_no_new_array LBRACK expression RBRACK))
	((array_creation_init LBRACK expression RBRACK)))
       (postfix_expression
	((primary))
	((name))
	((postincrement_expression))
	((postdecrement_expression)))
       (postincrement_expression
	((postfix_expression PLUSPLUS)))
       (postdecrement_expression
	((postfix_expression MINUSMINUS)))
       (unary_expression
	((preincrement_expression))
	((predecrement_expression))
	((PLUS unary_expression))
	((MINUS unary_expression))
	((unary_expression_not_plus_minus)))
       (preincrement_expression
	((PLUSPLUS unary_expression)))
       (predecrement_expression
	((MINUSMINUS unary_expression)))
       (unary_expression_not_plus_minus
	((postfix_expression))
	((COMP unary_expression))
	((NOT unary_expression))
	((cast_expression)))
       (cast_expression
	((LPAREN primitive_type dims_opt RPAREN unary_expression))
	((LPAREN name RPAREN unary_expression_not_plus_minus))
	((LPAREN name dims RPAREN unary_expression_not_plus_minus))
	((LPAREN name LT type_argument_list_1 dims_opt RPAREN unary_expression_not_plus_minus))
	((LPAREN name LT type_argument_list_1 DOT class_or_interface_type dims_opt RPAREN unary_expression_not_plus_minus)))
       (multiplicative_expression
	((unary_expression))
	((multiplicative_expression MULT unary_expression))
	((multiplicative_expression DIV unary_expression))
	((multiplicative_expression MOD unary_expression)))
       (additive_expression
	((multiplicative_expression))
	((additive_expression PLUS multiplicative_expression))
	((additive_expression MINUS multiplicative_expression)))
       (shift_expression
	((additive_expression))
	((shift_expression LSHIFT additive_expression))
	((shift_expression RSHIFT additive_expression))
	((shift_expression URSHIFT additive_expression)))
       (relational_expression
	((shift_expression))
	((relational_expression LT shift_expression))
	((relational_expression GT shift_expression))
	((relational_expression LTEQ shift_expression))
	((relational_expression GTEQ shift_expression)))
       (instanceof_expression
	((relational_expression))
	((instanceof_expression INSTANCEOF reference_type)))
       (equality_expression
	((instanceof_expression))
	((equality_expression EQEQ instanceof_expression))
	((equality_expression NOTEQ instanceof_expression)))
       (and_expression
	((equality_expression))
	((and_expression AND equality_expression)))
       (exclusive_or_expression
	((and_expression))
	((exclusive_or_expression XOR and_expression)))
       (inclusive_or_expression
	((exclusive_or_expression))
	((inclusive_or_expression OR exclusive_or_expression)))
       (conditional_and_expression
	((inclusive_or_expression))
	((conditional_and_expression ANDAND inclusive_or_expression)))
       (conditional_or_expression
	((conditional_and_expression))
	((conditional_or_expression OROR conditional_and_expression)))
       (conditional_expression
	((conditional_or_expression))
	((conditional_or_expression QUESTION expression COLON conditional_expression)))
       (assignment_expression
	((conditional_expression))
	((assignment)))
       (assignment
	((postfix_expression assignment_operator assignment_expression)))
       (assignment_operator
	((EQ))
	((MULTEQ))
	((DIVEQ))
	((MODEQ))
	((PLUSEQ))
	((MINUSEQ))
	((LSHIFTEQ))
	((RSHIFTEQ))
	((URSHIFTEQ))
	((ANDEQ))
	((XOREQ))
	((OREQ)))
       (expression_opt
	(nil)
	((expression)))
       (expression
	((assignment_expression)))
       (constant_expression
	((expression)))
       (type_parameters_opt
	((type_parameters))
	(nil))
       (type_parameters
	((LT type_parameter_list_1)
	 (progn $2)))
       (type_parameter_list
	((type_parameter_list COMMA type_parameter)
	 (cons $3 $1))
	((type_parameter)
	 (list $1)))
       (type_parameter_list_1
	((type_parameter_1)
	 (list $1))
	((type_parameter_list COMMA type_parameter_1)
	 (cons $3 $1)))
       (type_parameter
	((type_variable type_bound_opt)))
       (type_parameter_1
	((type_variable GT))
	((type_variable type_bound_1)))
       (type_bound_opt
	((type_bound))
	(nil))
       (type_bound
	((EXTENDS reference_type additional_bound_list_opt)))
       (type_bound_1
	((EXTENDS reference_type_1))
	((EXTENDS reference_type additional_bound_list_1)))
       (additional_bound_list_opt
	((additional_bound_list))
	(nil))
       (additional_bound_list
	((additional_bound additional_bound_list))
	((additional_bound)))
       (additional_bound_list_1
	((additional_bound additional_bound_list_1))
	((additional_bound_1)))
       (additional_bound
	((AND interface_type)))
       (additional_bound_1
	((AND reference_type_1)))
       (postfix_expression_no_name
	((primary))
	((postincrement_expression))
	((postdecrement_expression)))
       (unary_expression_no_name
	((preincrement_expression))
	((predecrement_expression))
	((PLUS unary_expression))
	((MINUS unary_expression))
	((unary_expression_not_plus_minus_no_name)))
       (unary_expression_not_plus_minus_no_name
	((postfix_expression_no_name))
	((COMP unary_expression))
	((NOT unary_expression))
	((cast_expression)))
       (multiplicative_expression_no_name
	((unary_expression_no_name))
	((name MULT unary_expression))
	((multiplicative_expression_no_name MULT unary_expression))
	((name DIV unary_expression))
	((multiplicative_expression_no_name DIV unary_expression))
	((name MOD unary_expression))
	((multiplicative_expression_no_name MOD unary_expression)))
       (additive_expression_no_name
	((multiplicative_expression_no_name))
	((name PLUS multiplicative_expression))
	((additive_expression_no_name PLUS multiplicative_expression))
	((name MINUS multiplicative_expression))
	((additive_expression_no_name MINUS multiplicative_expression)))
       (shift_expression_no_name
	((additive_expression_no_name))
	((name LSHIFT additive_expression))
	((shift_expression_no_name LSHIFT additive_expression))
	((name RSHIFT additive_expression))
	((shift_expression_no_name RSHIFT additive_expression))
	((name URSHIFT additive_expression))
	((shift_expression_no_name URSHIFT additive_expression)))
       (relational_expression_no_name
	((shift_expression_no_name))
	((name LT shift_expression))
	((shift_expression_no_name LT shift_expression))
	((name GT shift_expression))
	((shift_expression_no_name GT shift_expression))
	((name LTEQ shift_expression))
	((relational_expression_no_name LTEQ shift_expression))
	((name GTEQ shift_expression))
	((relational_expression_no_name GTEQ shift_expression)))
       (instanceof_expression_no_name
	((relational_expression_no_name))
	((name INSTANCEOF reference_type))
	((instanceof_expression_no_name INSTANCEOF reference_type)))
       (equality_expression_no_name
	((instanceof_expression_no_name))
	((name EQEQ instanceof_expression))
	((equality_expression_no_name EQEQ instanceof_expression))
	((name NOTEQ instanceof_expression))
	((equality_expression_no_name NOTEQ instanceof_expression)))
       (and_expression_no_name
	((equality_expression_no_name))
	((name AND equality_expression))
	((and_expression_no_name AND equality_expression)))
       (exclusive_or_expression_no_name
	((and_expression_no_name))
	((name XOR and_expression))
	((exclusive_or_expression_no_name XOR and_expression)))
       (inclusive_or_expression_no_name
	((exclusive_or_expression_no_name))
	((name OR exclusive_or_expression))
	((inclusive_or_expression_no_name OR exclusive_or_expression)))
       (conditional_and_expression_no_name
	((inclusive_or_expression_no_name))
	((name ANDAND inclusive_or_expression))
	((conditional_and_expression_no_name ANDAND inclusive_or_expression)))
       (conditional_or_expression_no_name
	((conditional_and_expression_no_name))
	((name OROR conditional_and_expression))
	((conditional_or_expression_no_name OROR conditional_and_expression)))
       (conditional_expression_no_name
	((conditional_or_expression_no_name))
	((name QUESTION expression COLON conditional_expression))
	((conditional_or_expression_no_name QUESTION expression COLON conditional_expression)))
       (assignment_expression_no_name
	((conditional_expression_no_name))
	((assignment)))
       (expression_no_name
	((assignment_expression_no_name)))
       (annotation_type_declaration
	((modifiers AT INTERFACE IDENTIFIER annotation_type_body))
	((AT INTERFACE IDENTIFIER annotation_type_body)))
       (annotation_type_body
	((LBRACE annotation_type_member_declarations_opt RBRACE))
	((LBRACE error)
	 (wisent-skip-block)))
       (annotation_type_member_declarations_opt
	(nil)
	((annotation_type_member_declarations)))
       (annotation_type_member_declarations
	((annotation_type_member_declaration))
	((annotation_type_member_declarations annotation_type_member_declaration)))
       (annotation_type_member_declaration
	((modifiers_opt type IDENTIFIER LPAREN RPAREN default_value_opt SEMICOLON))
	((constant_declaration))
	((class_declaration))
	((interface_declaration))
	((enum_declaration))
	((annotation_type_declaration))
	((SEMICOLON)))
       (default_value_opt
	 (nil)
	 ((default_value)))
       (default_value
	 ((DEFAULT member_value)))
       (annotation
	((AT name))
	((AT name LPAREN member_value RPAREN))
	((AT name LPAREN member_value_pairs RPAREN))
	((AT name LPAREN RPAREN))
	((AT name LPAREN error)
	 (wisent-skip-block)))
       (member_value_pairs
	((member_value_pair))
	((member_value_pairs COMMA member_value_pair)))
       (member_value_pair
	((simple_name EQ member_value)))
       (member_value
	((conditional_expression))
	((annotation))
	((member_value_array_initializer)))
       (member_value_array_initializer
	((LBRACE member_values COMMA RBRACE))
	((LBRACE member_values RBRACE))
	((LBRACE COMMA RBRACE))
	((LBRACE RBRACE))
	((LBRACE error)
	 (wisent-skip-block)))
       (member_values
	((member_value))
	((member_values COMMA member_value))))
     '(goal package_declaration import_declaration class_declaration enum_declaration enum_constant field_declaration method_declaration formal_parameter constructor_declaration interface_declaration abstract_method_declaration block_statements local_variable_declaration block_statement)))
  "Parser table.")

(defun wisent-java-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table wisent-java-wy--parse-table
	semantic-debug-parser-source "wisent-java.wy"
	semantic-flex-keywords-obarray wisent-java-wy--keyword-table
	semantic-lex-types-obarray wisent-java-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
	    'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(require 'semantic-lex)

(define-lex-keyword-type-analyzer wisent-java-wy--<keyword>-keyword-analyzer
  "keyword analyzer for <keyword> tokens."
  "\\(\\sw\\|\\s_\\)+")

(define-lex-block-type-analyzer wisent-java-wy--<block>-block-analyzer
  "block analyzer for <block> tokens."
  "\\s(\\|\\s)"
  '((("(" LPAREN PAREN_BLOCK)
     ("{" LBRACE BRACE_BLOCK)
     ("[" LBRACK BRACK_BLOCK))
    (")" RPAREN)
    ("}" RBRACE)
    ("]" RBRACK))
  )

(define-lex-regex-type-analyzer wisent-java-wy--<symbol>-regexp-analyzer
  "regexp analyzer for <symbol> tokens."
  "\\(\\sw\\|\\s_\\)+"
  '((BOOLEAN_LITERAL . "\\`true\\'")
    (BOOLEAN_LITERAL . "\\`false\\'")
    (NULL_LITERAL . "\\`null\\'"))
  'IDENTIFIER)

(define-lex-sexp-type-analyzer wisent-java-wy--<string>-sexp-analyzer
  "sexp analyzer for <string> tokens."
  "\\s\""
  'STRING_LITERAL)

(define-lex-regex-type-analyzer wisent-java-wy--<number>-regexp-analyzer
  "regexp analyzer for <number> tokens."
  semantic-lex-number-expression
  nil
  'NUMBER_LITERAL)

(define-lex-string-type-analyzer wisent-java-wy--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)+"
  '((AT . "@")
    (ELLIPSIS . "...")
    (COMP . "~")
    (OROR . "||")
    (OREQ . "|=")
    (OR . "|")
    (XOREQ . "^=")
    (XOR . "^")
    (QUESTION . "?")
    (URSHIFTEQ . ">>>=")
    (URSHIFT . ">>>")
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
  'punctuation)


;;; Epilogue
;;
;; Define the lexer for this grammar
(define-lex wisent-java-lexer
  "Lexical analyzer that handles Java buffers.
It ignores whitespaces, newlines and comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments
  ;;;; Auto-generated analyzers.
  wisent-java-wy--<number>-regexp-analyzer
  wisent-java-wy--<string>-sexp-analyzer
  ;; Must detect keywords before other symbols
  wisent-java-wy--<keyword>-keyword-analyzer
  wisent-java-wy--<symbol>-regexp-analyzer
  wisent-java-wy--<punctuation>-string-analyzer
  wisent-java-wy--<block>-block-analyzer
  ;;;;
  semantic-lex-default-action)

(provide 'wisent-java-wy)

;;; wisent-java-wy.el ends here
