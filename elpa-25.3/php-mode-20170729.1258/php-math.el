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

;; Math functions
;; http://php.net/manual/en/ref.math.php
;; file:///usr/share/doc/php-doc/html/ref.math.html

(define-skeleton php-abs
  "Insert an abs statement"
  ""
  > "abs(" (skeleton-read "Number to round? ") ");" \n
  )

(define-skeleton php-acos
  "Insert an acos statement"
  ""
  > "acos(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-acosh
  "Insert an acosh statement"
  ""
  > "acosh(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-asin
  "Insert an asin statement"
  ""
  > "asin(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-asinh
  "Insert an asinh statement"
  ""
  > "asinh(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-atan
  "Insert an atan statement"
  ""
  > "atan(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-atan2
  "Insert an atan2 statement"
  ""
  > "atan2(" (skeleton-read "Number? ") ", " (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-atanh
  "Insert an atanh statement"
  ""
  > "atanh(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-base_convert
  "Insert a base_convert statement"
  ""
  > "base_convert(" (skeleton-read "Number? ") ", " (skeleton-read "Base number? ") ", " (skeleton-read "Base number? ") ");" \n
)

(define-skeleton php-bcadd
  "Insert a bcadd statement"
  ""
  > "bcadd(" (skeleton-read "Number? ") ", " (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-gmp_add
  "Insert a bcadd statement"
  ""
  > "gmp_add(" (skeleton-read "Number? ") ", " (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-bindec
  "Insert a bindec statement"
  ""
  > "bindec(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-ceil
  "Insert a ceil statement"
  ""
  > "ceil(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-cos
  "Insert a cos statement"
  ""
  > "cos(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-cosh
  "Insert a cosh statement"
  ""
  > "cosh(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-decbin
  "Insert a decbin statement"
  ""
  > "decbin(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-dechex
  "Insert a dechex statement"
  ""
  > "dechex(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-decoct
  "Insert a dechex statement"
  ""
  > "decoct(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-deg2rad
  "Insert a dechex statement"
  ""
  > "deg2rad(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-exp
  "Insert an exp statement"
  ""
  > "exp(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-expm1
  "Insert an expm1 statement"
  ""
  > "expm1(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-floor
  "Insert a floor statement"
  ""
  > "floor(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-fmod
  "Insert a fmod statement"
  ""
  > "fmod(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-getrandmax
  "Insert an getrandmax statement"
  ""
  > "getrandmax(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-mt_rand
  "Insert an mt_rand statement"
  ""
  > "mt_rand(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-mt_srand
  "Insert an mt_rand statement"
  ""
  > "mt_srand(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-hexdec
  "Insert a hexdec statement"
  ""
  > "hexdec(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-hypot
  "Insert a hypot statement"
  ""
  > "hypot(" (skeleton-read "Number? ") ", " (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-is_finite
  "Insert a is_finite statement"
  ""
  > "is_finite(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-is_infinite
  "Insert a is_infinite statement"
  ""
  > "is_infinite(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-is_nan
  "Insert a is_nan statement"
  ""
  > "is_nan(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-lcg_value
  "Insert a lcg_value statement"
  ""
  > "lcg_value(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-log10
  "Insert a log10 statement"
  ""
  > "log10(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-log1p
  "Insert a log1p statement"
  ""
  > "log1p(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-log
  "Insert a log statement"
  ""
  > "log(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-max
  "Insert a max statement"
  ""
  > "max(" (skeleton-read "Number? ") ", " (skeleton-read "Number? ") \n
   ( "Another number %s: "
   > ", " str)
  > ");" \n
)

(define-skeleton php-min
  "Insert a min statement"
  ""
  > "min(" (skeleton-read "Number? ") ", " (skeleton-read "Number? ") \n
   ( "Another number %s: "
   > ", " str)
  > ");" \n
)

(define-skeleton php-mt_getrandmax
  "Insert a mt_getrandmax statement"
  ""
  > "mt_getrandmax(); " \n
)

(define-skeleton php-number_format
  "Insert a number_format statement"
  ""
  '(setq number (skeleton-read "Number? "))
  > "number_format(" number ", " 
  ( "Another argument %s: "
   > ", " str)
)

(define-skeleton php-pi
  "Insert a pi statement"
  ""
  > "pi();" \n
)

(define-skeleton php-pow
  "Insert a pow statement"
  ""
  '(setq base (skeleton-read "Base? "))
  '(setq exp (skeleton-read "Exponent? "))
  > "pow(" base ", " exp ");" \n
)

(define-skeleton php-rad2deg
  "Insert a pow statement"
  ""
  '(setq base (skeleton-read "Radians? "))
  '(setq exp (skeleton-read "Degrees? "))
  > "rad2deg(" base  ");" \n
)

(define-skeleton php-rand_weighted
  "Insert an rand_weighted statement"
  ""
  > "rand_weighted(" (skeleton-read "Number? ") ");" \n
)

(define-skeleton php-round
  "Insert a round statement"
  ""
  '(setq number (skeleton-read "Number to round? "))
  > "round(" number ");" \n
)

(define-skeleton php-sin
  "Insert a sin statement"
  ""
  '(setq number (skeleton-read "Number? "))
  > "sin(" number ");" \n
)

(define-skeleton php-sinh
  "Insert a sin statement"
  ""
  '(setq number (skeleton-read "Number? "))
  > "sinh(" number ");" \n
)

(define-skeleton php-sqrt
  "Insert a sqrt statement"
  ""
  '(setq number (skeleton-read "Number? "))
  > "sqrt(" number ");" \n
)

(define-skeleton php-srand
  "Insert a sqrt statement"
  ""
  '(setq number (skeleton-read "Number? "))
  > "srand(" number ");" \n
)

(define-skeleton php-tan
  "Insert a tan statement"
  ""
  '(setq number (skeleton-read "Number? "))
  > "tan(" number ");" \n
)

(define-skeleton php-tanh
  "Insert a tan statement"
  ""
  '(setq number (skeleton-read "Number? "))
  > "tanh(" number ");" \n
)
