;;; php-ext.el --- PHP skeleton templates

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

;; (require 'php-ext)

;; Description:
;; Php ext is some skeleton templates for extend php-mode

;; Math functions

(defvar php-ext-path
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(load (concat php-ext-path "php-math.el"))

;; Control Structures

(load (concat php-ext-path "php-control-structures.el"))

;; Classes Functions

(load (concat php-ext-path "php-classobj.el"))

;; Exceptions

(load (concat php-ext-path "php-exceptions.el"))

;; Handling strings

(load (concat php-ext-path "php-strings.el"))

;; Regular expression

(load (concat php-ext-path "php-regex.el"))
(load (concat php-ext-path "php-pcre.el"))

;; Handling Variables
;; http://php.net/manual/en/ref.var.php
;; file:///usr/share/doc/php-doc/html/ref.var.html

(load (concat php-ext-path "php-var.el"))

;; DOM
;; More see file:///usr/share/doc/php-doc/html/book.dom.html

(load (concat php-ext-path "php-dom.el"))

;; XML Parser
;; More see file:///usr/share/doc/php-doc/html/function.xml-parse-into-struct.html

(load (concat php-ext-path "php-xmlparser.el"))

;; XML Reader
;; More see file:///usr/share/doc/php-doc/html/book.xmlreader.html

(load (concat php-ext-path "php-xmlreader.el"))

;; Crack Functions

(load (concat php-ext-path "php-crack.el"))

;; Dio Functions

(load (concat php-ext-path "php-dio.el"))

;; Filesystems functions
;; file:///usr/share/doc/php-doc/html/ref.filesystem.html

(load (concat php-ext-path "php-filesystem.el"))

;; Graphic functions

;; GD functions
;; file:///usr/share/doc/php-doc/html/ref.image.html
;; http://php.net/manual/en/ref.image.php

(load (concat php-ext-path "php-gd.el"))

;; Exif functions
;; http://php.net/manual/en/ref.exif.php
;; file:///usr/share/doc/php-doc/html/ref.exif.html

(load (concat php-ext-path "php-exif.el"))

;; Another functions

(define-skeleton php-function
  "Insert a function statement."
  ""
  '(setq function (skeleton-read "Function name? ")) \n
  '(setq argument (skeleton-read "Argument? ")) \n
  > "function " function "(" argument
  ( "Another argument? %s: "
    > ", " str )
  > ") {" \n
  _ \n
  > "}"
)

(define-skeleton php-define
  "Insert a define statement"
  ""
  '(setq variable (skeleton-read "Variable? "))
  '(setq value (skeleton-read "Value? "))
  "define(\"" variable "\",\"" value "\");")

(provide 'php-ext)

;;; php-ext.el ends here
