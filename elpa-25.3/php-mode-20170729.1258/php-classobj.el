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

;; To install php-ext.el:
;; You can add (load "path/php-ext/php-ext.el") to your .emacs

;; Classes Functions
;; http://php.net/manual/en/ref.classobj.php
;; file:///usr/share/doc/php-doc/html/ref.classobj.html

(define-skeleton php-class
  "Insert a class statement."
  ""
  '(setq class (skeleton-read "Class name? ")) \n
  > "class " class \n
  > "{" \n
  ( "Variable? %s: "
    > "var " str ";" \n )
  ( "Function? %s: "
    > "function " str "(" ( "Parameter? %s:" str ", " ) ")" \n
    > "{ " \n
    _
    > "} " \n
    )
  > "}" \n
)

(define-skeleton php__autoload 
  "Insert a __autoload statement. Attempt to load undefined class"
  ""
  '(setq class (skeleton-read "Class: "))
  > "__autoload(" class ");" \n
)

(define-skeleton php-class_alias
  "Insert a class_alias statement. Creates an alias for a class"
  ""
  '(setq original (skeleton-read "Original: "))
  '(setq alias (skeleton-read "Alias: "))
  '(setq autoload (skeleton-read "Autoload: "))
  > "class_alias(" original ", " alias ", " autoload ");" \n
)

(define-skeleton php-class_exists
  "Insert a class_exists statement. Checks if the class has been defined"
  ""
  '(setq class_name (skeleton-read "Class name: "))
  '(setq autoload (skeleton-read "Autoload: "))
  > "class_exists(" class_name ", " autoload ");" \n
)

(define-skeleton php-get_called_class
  "Insert a get_called_class statement. Gets the name of the class the static method is called in."
  > "get_called_class();" \n
)

(define-skeleton php-get_class_methods
  "Insert a get_class_methods statement."
  ""
  '(setq str (skeleton-read "Class? "))
  > "get_class_methods('" str "');" \n
)

(define-skeleton php-get_class_vars
  "Insert a get_class_vars statement."
  ""
  '(setq str (skeleton-read "Class? "))
  > "get_class_vars('" str "');" \n
)

(define-skeleton php-get_class
  "Insert a get_class statement."
  ""
  '(setq str (skeleton-read "Class? "))
  > "get_class('" str "');" \n
)

(define-skeleton php-get_declared_classes
  "Insert a get_declared_classes statement."
  > "get_declared_classes();" \n
)

(define-skeleton php-get_declared_interfaces
  "Insert a get_declared_interfaces statement."
  > "get_declared_interfaces();" \n
)

(define-skeleton php-get_declared_traits
  "Insert a get_declared_traits statement."
  > "get_declared_traits();" \n
)

(define-skeleton php-get_object_vars
  "Insert a get_object_var statement."
  ""
  '(setq var (skeleton-read "Var? "))
  > "get_object_vars(" var ");" \n
)

(define-skeleton php-get_parent_class
  "Insert a get_parent_class statement."
  ""
  '(setq obj (skeleton-read "Object? "))
  > "get_parent_class(" obj ");" \n
)

(define-skeleton php-interface_exists
  "Insert a interface_exists statement."
  ""
  '(setq interface_name (skeleton-read "Interface Name: "))
  '(setq autoload (skeleton-read "Autoload: "))
  > "interface_exists(" interface_name ", " autoload ");" \n
)

(define-skeleton php-is_a
  "Insert an is_a statement. Checks if the object is of this class or has this class as one of its parents"
  ""
  '(setq object (skeleton-read "Object: "))
  '(setq class_name (skeleton-read "Class name: "))
  '(setq allow_string (skeleton-read "Allow string: "))
  > "is_a(" object ", " class_name ", " allow_string ");" \n
)

(define-skeleton php-is_subclass_of
  "Insert an is_subclass_of statement."
  ""
  '(setq obj (skeleton-read "Object: "))
  '(setq class (skeleton-read "Class Name: "))
  '(setq allow_string (skeleton-read "Allow string: "))
  > "is_subclass_of(" obj ", " class ", " allow_string ");" \n
)

(define-skeleton php-method_exists
  "Insert a method_exists statement."
  ""
  '(setq obj (skeleton-read "Object: "))
  '(setq method (skeleton-read "Method: "))
  > "method_exists(" object ", " method ");" \n
)

(define-skeleton php-property_exists
  "Insert a property_exists statement."
  ""
  '(setq class (skeleton-read "Class: "))
  '(setq property (skeleton-read "Property: "))
  > "property_exists(" class ", " property ");" \n
)
  
(define-skeleton php-trait_exists
  "Insert a trait_exists statement."
  ""
  '(setq trait (skeleton-read "Trait: "))
  '(setq autoload (skeleton-read "Autoload (TRUE | FALSE): "))
  > "trait_exists(" trait ", " autoload ");" \n
)
