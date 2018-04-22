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

;; Description:
;; Php ext is some skeleton templates for extend php-mode
;; DOM is to manage dom objects
;; http://php.net/manual/en/book.dom.php
;; More see file:///usr/share/doc/php-doc/html/book.dom.html


(define-skeleton php-domdocument
  "Insert a new domdocument object"
  ""
  > "$" (skeleton-read "Var? ") " = DOMDocument('1.0');" \n
)

(define-skeleton php-dom-appendChild
  "Insert a new appendChild dom method"
  ""
  >  (skeleton-read "Dom variable? ") "->appendChild(" (skeleton-read "Child? ") ");" \n
)

(define-skeleton php-dom-createElement
  "Insert a new appendChild dom method"
  ""
  >  (skeleton-read "Dom variable? ") "->createElement(" (skeleton-read "Element? ") ");" \n
)

(define-skeleton php-dom-createTextNode
  "Insert a new appendChild dom method"
  ""
  >  (skeleton-read "Dom variable? ") "->createTextNode(" (skeleton-read "Text Node? ") ");" \n
)

(define-skeleton php-dom-setAttribute
  "Insert a new appendChild dom method"
  ""
  >  (skeleton-read "Dom variable? ") "->setAttribute(" (skeleton-read "Attribute? ") ", " (skeleton-read "Value? ") ");" \n
)

(define-skeleton php-dom-saveXML
  "Insert a new appendChild dom method"
  ""
  >  (skeleton-read "Dom variable? ") "->saveXML();" \n
)
