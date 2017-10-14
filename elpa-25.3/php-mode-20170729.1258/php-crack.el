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
;; Functions defined in 
;; http://php.net/manual/en/ref.crack.php
;; file:///usr/share/doc/php-doc/html/ref.crack.html

(define-skeleton php-crack-check
  "Insert a crack_check statement"
  ""
  '(setq dictionary (skeleton-read "Dictionary? "))
  '(setq pass (skeleton-read "Password? "))
  > "crack_check(" dictionary ", " pass ");" \n
)


(define-skeleton php-crack-closedict
  "Insert a crack_closedict statement"
  ""
  '(setq dictionary (skeleton-read "Dictionary? "))
  > "crack_closedict(" dictionary ");" \n
)

(define-skeleton php-crack-getlastmessage
  "Insert a crack_getlastmessage statement"
  ""
  > "crack_getlastmessage();" \n
)

(define-skeleton php-crack-opendict
  "Insert a crack_opendict statement"
  ""
  '(setq dictionary (skeleton-read "Dictionary? "))
  > "crack_opendict(" dictionary ");" \n
)
