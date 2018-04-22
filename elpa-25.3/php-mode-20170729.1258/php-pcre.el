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

;; file:///usr/share/doc/php-doc/html/ref.pcre.html

(define-skeleton php-preg_filter
  "Insert a preg_filter statement. preg_filter is identical to preg_replace except it only returns the (possibly transformed) subjects where there was a match."
  ""
  '(setq pattern (skeleton-read "Pattern: "))
  '(setq replacement (skeleton-read "Replacement: "))
  '(setq subject (skeleton-read "Subject: "))
  > "preg_filter(" pattern ", " replacement ", " subject ");" \n)

(define-skeleton php-preg_grep
  "Insert a preg_grep statement. preg_grep return array entries that match the pattern."
  ""
  '(setq pattern (skeleton-read "Pattern: "))
  '(setq input (skeleton-read "Input: "))
  > "preg_grep(" pattern ", " replacement ");" \n)

(define-skeleton php-preg_last_error
  "Insert a preg_last_error statement. Returns the error code of the last PCRE regex execution"
  ""
  > "preg_last_error();" \n)


(define-skeleton php-preg_match_all
  "Insert a preg_match_all statement. Perform a global regular expression match"
  ""
  '(setq pattern (skeleton-read "Pattern: "))
  '(setq subject (skeleton-read "Input string: "))
  '(setq matches (skeleton-read "The array of all matches: "))
  '(setq flags (skeleton-read "Flags (PREG_PATTERN_ORDER | PREG_SET_ORDER | PREG_OFFSET_CAPTURE)"))
  '(setq offset (skeleton-read "Offset: "))
  > "preg_match_all(" pattern ", " subject ", " matches ", " flags ", " offset ");" \n
)

(define-skeleton php-preg_match
  "Insert a preg_match statement. Perform a regular expression match"
  '(setq pattern (skeleton-read "Pattern: "))
  '(setq subject (skeleton-read "Input string: "))
  '(setq matches (skeleton-read "The array of all matches: "))
  '(setq flags (skeleton-read "Flags (PREG_PATTERN_ORDER | PREG_SET_ORDER | PREG_OFFSET_CAPTURE)"))
  '(setq offset (skeleton-read "Offset: "))
  > "preg_match(" pattern ", " subject ", " matches ", " flags ", " offset ");" \n
)

(define-skeleton php-preg_quote
  "Insert a preg_quote statement. Quote regular expression characters"
  ""
  '(setq str (skeleton-read "String: "))
  '(setq delimiter (skeleton-read "Delimiter: "))
  > "preg_quote(" str ", " delimiter ");" \n
)

(define-skeleton php-preg_replace_callback
  "Insert a preg_replace_callback statement."
  ""
  '(setq pattern (skeleton-read "Pattern: "))
  '(setq callback (skeleton-read "Callback: "))
  '(setq subject (skeleton-read "Subject: "))
  '(setq limit (skeleton-read "Limit: "))
  '(setq count (skeleton-read "Count: "))
  > "preg_replace_callback(" pattern ", " callback ", " subject ", " limit ", " count ");" \n
)

(define-skeleton php-preg_replace
  "Insert a preg_replace statement. Perform a regular expression search and replace"
  ""
  '(setq pattern (skeleton-read "Pattern: "))
  '(setq replacement (skeleton-read "Replacement: "))
  '(setq subject (skeleton-read "Subject: "))
  '(setq limit (skeleton-read "Limit: "))
  '(setq count (skeleton-read "Count: "))
  > "preg_replace_callback(" pattern ", " replacement ", " subject ", " limit ", " count ");" \n
)

(define-skeleton php-preg_split
  "Insert a preg_split statement. Split string by a regular expression"
  ""
  '(setq pattern (skeleton-read "Pattern: "))
  '(setq subject (skeleton-read "Subject: "))
  '(setq limit (skeleton-read "Limit: "))
  '(setq flags (skeleton-read "Flags: "))
  > "preg_split(" pattern ", " subject ", " limit ", " flags ");" \n
)
