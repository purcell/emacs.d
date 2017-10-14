;; Copyright (C) 2015  David Arroyo Menéndez

;; Author: David Arroyo Menéndez <davidam@gnu.org>
;; Maintainer: David Arroyo Menéndez <davidam@gnu.org>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
;; Boston, MA 02110-1301 USA,

;; Variable handling functions
;; http://php.net/manual/en/ref.var.php
;; file:///usr/share/doc/php-doc/html/ref.var.html

(define-skeleton php-boolval
  "Insert a boolval statement"
  ""
  '(setq variable (skeleton-read "Variable? "))
  "boolval(" variable ");"
)
 
(define-skeleton php-debug_zval_dump
  "Insert a debug_zval_dump"
  ""
  '(setq variable (skeleton-read "Variable: "))
  "debug_zval_dump(" variable ");" \n
)

(define-skeleton php-empty
  "Insert an empty statement"
  ""
  '(setq variable (skeleton-read "Variable? "))
  "empty(" variable ");"
)

(define-skeleton php-floatval
  "Insert a floatval statement. Gets the float value of a variable"
  ""
  '(setq variable (skeleton-read "Variable: "))
  > "floatval(" variable ");" \n
)

(define-skeleton php-get_defined_vars
  "Insert a get_defined_vars statement. Returns an array of all defined variables"
  > "get_defined_vars();" \n
)

(define-skeleton php-get_resource_type
  "Insert a get_resource_type statement. Returns the resource type"
  ""
  '(setq variable (skeleton-read "Variable: "))
  > "get_resource_type(" variable ");" \n
)

(define-skeleton php-gettype
  "Insert a gettype statement. Returns the type of variable"
  ""
  '(setq variable (skeleton-read "Variable: "))
  > "gettype(" variable ");" \n
)

(define-skeleton php-import_request_variables 
  "Insert an import_request_variables statement. Import GET/POST/Cookie variables into the global scope"
  ""
  '(setq types (skeleton-read "Types: "))
  '(setq prefix (skeleton-read "Prefix: "))
  > "import_request_variables(" types ", " prefix ");" \n
)

(define-skeleton php-intval
  "Insert an intval statement."
  ""
  '(setq variable (skeleton-read "Variable: "))
  '(setq base (skeleton-read "Base: "))
  > "intval(" variable ", " base ");" \n
)

(define-skeleton php-is_array
  "Insert an is_array statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "is_array(" variable ");" \n
)

(define-skeleton php-is_bool
  "Insert an is_bool statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "is_bool(" variable ");" \n
)

(define-skeleton php-is_callable
  "Insert an is_callable statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "is_callable(" variable ");" \n
)

(define-skeleton php-is_double
  "Insert an is_double statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "is_double(" variable ");" \n
)

(define-skeleton php-is_float
  "Insert an is_float statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "is_float(" variable ");" \n
)

(define-skeleton php-is_int
  "Insert an is_int statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "is_int(" variable ");" \n
)

(define-skeleton php-is_integer
  "Insert an is_integer statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "is_integer(" variable ");" \n
)

(define-skeleton php-is_long
  "Insert an is_long statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "is_long(" variable ");" \n
)

(define-skeleton php-is_null
  "Insert an is_null statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "is_null(" variable ");" \n
)

(define-skeleton php-is_numeric
  "Insert an is_numeric statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "is_numeric(" variable ");" \n
)

(define-skeleton php-is_object
  "Insert an is_object statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "is_object(" variable ");" \n
)

(define-skeleton php-is_real
  "Insert an is_real statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "is_real(" variable ");" \n
)

(define-skeleton php-is_resource
  "Insert an is_resource statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "is_resource(" variable ");" \n
)

(define-skeleton php-is_scalar
  "Insert an is_scalar statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "is_scalar(" variable ");" \n
)

(define-skeleton php-is_string
  "Insert an is_string statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "is_string(" variable ");" \n
)

(define-skeleton php-isset
  "Insert an isset statement"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "isset(" variable ");" \n
)

(define-skeleton php-print_r
  "Insert a print_r statement"
  ""
  '(setq expression (skeleton-read "Expression: "))
  '(setq return (skeleton-read "Return (true | false): "))
  > "print_r(" expression ", " return ");"
)

(define-skeleton php-serialize
  "Insert an serialize statement. Generates a storable representation of a value
"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "serialize(" variable ");" \n
)

(define-skeleton php-settype
  "Insert a settype statement. Set the type of a variable" 
  ""
  '(setq variable (skeleton-read "Variable: "))
  '(setq type (skeleton-read "Type: "))
  > "settype(" variable ", " type ");" \n
)

(define-skeleton php-strval
  "Insert a strval statement. Get string value of a variable"
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "strval(" variable ");" \n
)  

(define-skeleton php-unserialize
  "Insert an unserialize statement. Creates a php value from a stored representation" 
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "unserialize(" variable ");" \n
)

(define-skeleton php-unset
  "Insert an unset statement. Unset a given variable." 
  ""
  '(setq variable (skeleton-read "Variable: "))  
  > "unset(" variable ");" \n
)  

(define-skeleton php-var_dump
  "Insert a var_dump statement"
  ""
  '(setq variable (skeleton-read "Variable? "))
  > "var_dump(" variable 
  ( "Other variable? %s: "
    > ", " str )
  > ");"
)

(define-skeleton php-var_export
  "Insert a var_export statement"
  ""
  '(setq variable (skeleton-read "Variable? "))
  '(setq return (skeleton-read "Return? "))
  > "var_export(" variable ", " return ");" \n 
)
 
 
