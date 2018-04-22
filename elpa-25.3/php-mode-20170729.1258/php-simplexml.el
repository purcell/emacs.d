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

;; Simple XML Functions
;; file:///usr/share/doc/php-doc/html/ref.simplexml.html
;; http://php.net/manual/en/ref.simplexml.php

(define-skeleton php-simplexml_import_dom
  "Insert a simplexml_import_dom statement."
  ""
  '(setq domnode (skeleton-read "domnode: "))
  '(setq class_name (skeleton-read "class_name: "))
  > "simplexml_import_dom(" domnode ", " class_name ");" \n
)


(define-skeleton php-simplexml_load_file
  "Insert a simplexml_load_file statement."
  ""
  '(setq filename (skeleton-read "filename: ")) 
  '(setq class_name (skeleton-read "class_name: ")) 
  '(setq options (skeleton-read "options: ")) 
  '(setq ns (skeleton-read "ns: ")) 
  '(setq is_prefix (skeleton-read "is_prefix: ")) 
  > "simplexml_load_file(" filename ", " class_name ", " options ", " ns ", " is_prefix ");" \n
)

(define-skeleton php-simplexml_load_string
  "Insert a simplexml_load_string statement."
  ""
  '(setq data (skeleton-read "data: "))
  '(setq class_name (skeleton-read "class_name: "))
  '(setq options (skeleton-read "options: "))
  '(setq ns (skeleton-read "ns: "))
  '(setq is_prefix (skeleton-read "is_prefix: "))
  > "simplexml_load_string(" data ", " class_name ", " options ", " ns ", " is_prefix ");" \n
  )
