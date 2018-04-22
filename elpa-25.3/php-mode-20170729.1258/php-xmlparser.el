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

;; XML Parser
;; More see file:///usr/share/doc/php-doc/html/function.xml-parse-into-struct.html
;; http://php.net/manual/en/book.xml.php

(define-skeleton php-utf8_decode
  "Insert a utf8_decode statement"
  ""
  > "utf8_decode(" (skeleton-read "An utf-8 string ") ");" \n
)

(define-skeleton php-utf8_encode
  "Insert a utf8_encode statement"
  ""
  > "utf8_encode(" (skeleton-read "An iso-8859-1 string ") ");" \n
)

(define-skeleton php-xml_error_string
  "Insert a xml_error_string statement"
  ""
  > "xml_error_string(" (skeleton-read "Code? ") ");" \n
)

(define-skeleton php-xml_get_current_byte_index
  "Insert a xml_get_current_byte_index"
  ""
  > "xml_get_current_byte_index(" (skeleton-read "Parser? ") ");" \n
)

(define-skeleton php-xml-get_current_column_number
  "Insert a xml_get_current_column_number"
  ""
  > "xml_get_current_column_number(" (skeleton-read "Parser? ") ");" \n
)

(define-skeleton php-xml-get_current_line_number
  "Insert a xml_get_current_line_number"
  ""
  > "xml_get_current_line_number(" (skeleton-read "Parser? ") ");" \n
)

(define-skeleton php-xml-get_error_code
  "Insert a xml_get_error_code"
  ""
  > "xml_get_error_code(" (skeleton-read "Parser? ") ");" \n
)

(define-skeleton php-xml_parse
  "Insert a xml_parse"
  ""
  > "xml_parse(" (skeleton-read "Parser? ") ", " 
  > (skeleton-read "Data? ") 
  > ("Is final? " ", " str) _ ");")  

(define-skeleton php-xml_parser_create_ns
  "Insert a xml_parser_create_ns statement. Create an XML parser with namespace support."
  ""
  '(setq encoding (skeleton-read "encoding: "))
  '(setq separator (skeleton-read "separator: "))
  > "xml_parser_create_ns(" encoding ", " separator ");" \n
)

(define-skeleton php-xml_parser_create
  "Insert a xml_parser_create statement."
  ""
  '(setq encoding (skeleton-read "encoding: "))
  > "xml_parser_create(" encoding ");" \n
)

(define-skeleton php-xml_parser_free
  "Insert a xml_parser_free statement."
  ""
  '(setq parser (skeleton-read "parser: "))
  > "xml_parser_free(" parser ");" \n
)

(define-skeleton php-xml_parser_get_option
  "Insert a xml_parser_get_option statement."
  ""
  '(setq parser (skeleton-read "parser: "))
  '(setq option (skeleton-read "option: "))
  > "xml_parser_get_option(" parser ", " option ");" \n
)

(define-skeleton php-xml_parser_set_option
  "Insert a xml_parser_set_option statement."
  ""
  '(setq parser (skeleton-read "parser: "))
  '(setq option (skeleton-read "option: "))
  '(setq value (skeleton-read "value: "))
  > "xml_parser_set_option(" parser ", " option ", " value ");" \n
)

(define-skeleton php-xml_set_character_data_handler
  "Insert a xml_set_character_data_handler statement."
  ""
  '(setq parser (skeleton-read "parser: "))
  '(setq handler (skeleton-read "handler: "))
  > "xml_set_character_data_handler(" parser ", " handler ");" \n
)

(define-skeleton php-xml_set_default_handler
  "Insert a xml_set_default_handler statement."
  ""
  '(setq parser (skeleton-read "parser: "))
  '(setq handler (skeleton-read "handler: "))  
  > "xml_set_default_handler(" parser ", " handler ");" \n
)

(define-skeleton php-xml_set_element_handler
  "Insert a xml_set_element_handler statement."
  ""
  '(setq parser (skeleton-read "parser: "))
  '(setq start_element_handler (skeleton-read "start_element_handler: "))
  '(setq end_element_handler (skeleton-read "end_element_handler: "))
  > "xml_set_element_handler(" parser ", " start_element_handler ", " end_element_handler ");" \n
)

(define-skeleton php-xml_set_end_namespace_decl_handler
  "Insert a xml_set_end_namespace_decl_handler statement. Set up end namespace declaration handler "
  ""
  '(setq parser (skeleton-read "parser: "))
  '(setq handler (skeleton-read "handler: "))
  > "xml_set_end_namespace_decl_handler(" parser ", " handler ");" \n
)


(define-skeleton php-xml_set_external_entity_ref_handler
  "Insert a xml_set_external_entity_ref_handler statement."
  ""
  '(setq parser (skeleton-read "parser: "))
  '(setq handler (skeleton-read "handler: "))  
  > "xml_set_external_entity_ref_handler(" parser ", " handler ");" \n
)

(define-skeleton php-xml_set_notation_decl_handler
  "Insert a xml_set_notation_decl_handler statement."
  ""
  '(setq parser (skeleton-read "parser: "))
  '(setq handler (skeleton-read "handler: "))  
  > "xml_set_notation_decl_handler(" parser ", " handler ");" \n
)


(define-skeleton php-xml_set_object
  "Insert a xml_set_object statement. Use XML Parser within an object"
  ""
  '(setq parser (skeleton-read "parser: "))
  '(setq object (skeleton-read "object: "))
  > "xml_set_object(" parser ", " object ");" \n
)

(define-skeleton php-xml_set_processing_instruction_handler
  "Insert a xml_set_processing_instruction_handler statement. Set up processing instruction (PI) handler  "
  ""
  '(setq parser (skeleton-read "parser: "))
  '(setq handler (skeleton-read "handler: "))
  > "xml_set_processing_instruction_handler(" parser ", " handler ");" \n
)

(define-skeleton php-xml_set_start_namespace_decl_handler
  "Insert a xml_set_start_namespace_decl_handler statement.  Set up start namespace declaration handler "
  ""
  '(setq parser (skeleton-read "parser: "))
  '(setq handler (skeleton-read "handler: "))  
  > "xml_set_start_namespace_decl_handler(" parser ", " handler ");" \n
)

(define-skeleton php-xml_set_unparsed_entity_decl_handler
  "Insert a xml_set_unparsed_entity_decl_handler statement. Set up unparsed entity declaration handler  "
  ""
  '(setq parser (skeleton-read "parser: "))
  '(setq handler (skeleton-read "handler: "))    
  > "xml_set_unparsed_entity_decl_handler(" parser ", " handler ");" \n
)
