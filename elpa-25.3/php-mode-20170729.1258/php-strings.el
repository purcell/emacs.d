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

;; http://php.net/manual/en/ref.strings.php
;; file:///usr/share/doc/php-doc/html/ref.strings.html

(define-skeleton php-addcslashes 
  "Insert an addcslashes statement"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq charlist (skeleton-read "Charlist? "))
  > "addclslashes(" str ", " charlist ");" \n)

(define-skeleton php-addslashes 
  "Insert an addcslashes statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "addclslashes(" str ");" \n)

(define-skeleton php-bin2hex
  "Insert a bin2hex statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "bin2hex(" str ");" \n)

(define-skeleton php-chop
  "Insert a chop statement. Alias of rtrim"
  ""
  '(setq str (skeleton-read "String: "))
  > "chop(" str ");" \n)

(define-skeleton php-chr
  "Insert a chr statement. Returns a specific character from ascii code"
  ""
  '(setq ascii (skeleton-read "String: "))
  > "chr(" ascii ");" \n)

(define-skeleton php-chunk_split
  "Insert a chunk_split statement. Split a a string into smaller chunks"
  ""
  '(setq body (skeleton-read "String: "))
  '(setq chunklen (skeleton-read "Chunk length: "))
  '(setq end (skeleton-read "End: "))
  > "chunk_split(" body ", " chunklen ", " end ");" \n
)

(define-skeleton php-convert_cyr_string
  "Insert a convert_cyr_string statement"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq from (skeleton-read "From? "))
  '(setq to (skeleton-read "To? "))
  > "convert_cyr_string(" str ", " from ", " to ");" \n)

(define-skeleton php-convert_uudecode
  "Insert a bin2hex statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "convert_uudecode(" str ");" \n)

(define-skeleton php-convert_uuencode
  "Insert a bin2hex statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "convert_uuencode(" str ");" \n)

(define-skeleton php-count_chars
  "Insert a count_chars statement. Return information about characters used in a string"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq mode (skeleton-read "Mode? "))
  > "count_chars(" str ", " mode ");" \n
)

(define-skeleton php-crc32
  "Insert a crc32 statement. Calculates the crc32 polynomial of a string"
  ""
  '(setq str (skeleton-read "String: "))
  > "crc32(" str ");" \n
)

(define-skeleton php-crypt
  "Insert a crypt statement. One-way string hashing"
  ""
  '(setq str (skeleton-read "String: "))
  '(setq salt (skeleton-read "Salt: "))
  > "crypt(" str ", " salt ");" \n
)

(define-skeleton php-echo
  "Insert an echo statement"
  ""
  '(setq str (skeleton-read "String: "))
  > "echo '" str "';" \n)

(define-skeleton php-explode
  "Insert an explode statement"
  ""
  '(setq separator (skeleton-read "Explode separator? "))
  '(setq var (skeleton-read "Explode variable? "))
  > "explode('" separator "', " var ");" \n)

(define-skeleton php-fprintf 
  "Insert a fprintf statement"
  ""
  '(setq handle (skeleton-read "Handle: "))
  '(setq format (skeleton-read "Format: "))
  '(setq args (skeleton-read "Args: "))
  > "fprintf(" handle ", " format ", " args ");" \n
)

(define-skeleton php-get_html_translation_table
  "Insert a get_html_translation_table statement. Returns the translation table used by htmlspecialchars and htmlentities"
  ""
  '(setq table (skeleton-read "Table: "))
  '(setq flags (skeleton-read "Flags: "))
  '(setq encoding (skeleton-read "Encoding: "))
  > "get_html_translation_table(" table ", " flags ", " encoding ");" \n
)


(define-skeleton php-hebrev
  "Insert a hebrev statement. Convert logical Hebrew text to visual text with newline conversion"
  ""
  '(setq hebrew_text (skeleton-read "Hebrew text: "))
  '(setq max_chars_per_line (skeleton-read "Maximum number of characters per line: "))
  > "hebrev(" hebrew_text ", " max_chars_per_line ");" \n
)

(define-skeleton php-hebrevc
  "Insert a hebrevc statement. Convert logical Hebrew text to visual text with newline conversion"
  ""
  '(setq hebrew_text (skeleton-read "Hebrew text: "))
  '(setq max_chars_per_line (skeleton-read "Maximum number of characters per line: "))
  > "hebrevc(" hebrew_text ", " max_chars_per_line ");" \n
)

(define-skeleton php-hex2bin
  "Insert a hex2bin statement. Decodes a hexadecimally encoded binary string."
  ""
  '(setq data (skeleton-read "Data: "))
  > "hex2bin(" data ");" \n
)

(define-skeleton php-html_entity_decode
  "Insert a html_entity_decode statement. Convert special characters to HTML entities"
  ""
  '(setq string (skeleton-read "String: "))
  '(setq flags (skeleton-read "Flags: "))
  '(setq encoding (skeleton-read "Encoding: "))
  > "html_entity_decode(" string ", " flags ", " encoding ");" \n
)

(define-skeleton php-htmlentities
  "Insert a htmlentities statement. Convert all applicable characters to HTML entities"
  ""
  '(setq string (skeleton-read "String: "))
  '(setq flags (skeleton-read "Flags: "))
  '(setq encoding (skeleton-read "Encoding: "))
  > "htmlentities(" string ", " flags ", " encoding ");" \n
)

(define-skeleton php-htmlspecialchars_decode
  "Insert a htmlspecialchars_decode statement. Convert special HTML entities back to characters"
  ""
  '(setq string (skeleton-read "String: "))
  '(setq flags (skeleton-read "Flags: "))
  > "htmlspecialchars_decode(" string ", " flags ");" \n
)

(define-skeleton php-htmlspecialchars
  "Insert a htmlspecialchars_decode statement. Convert special HTML entities back to characters"
  ""
  '(setq string (skeleton-read "String: "))
  '(setq flags (skeleton-read "Flags: "))
  '(setq encoding (skeleton-read "Encoding: "))
  '(setq double_encode (skeleton-read "Double encode (true | false): "))
  > "htmlspecialchars_decode(" string ", " flags ");" \n
)

(define-skeleton php-implode
  "Insert an implode statement"
  ""
  '(setq separator (skeleton-read "Implode separator? "))
  '(setq var (skeleton-read "Implode variable? "))
  > "implode('" separator "', " var 
  (skeleton-read 
   > ", " str )
  > ");"
  )

(define-skeleton php-join
  "Insert an join statement"
  ""
  '(setq separator (skeleton-read "Join separator? "))
  '(setq var (skeleton-read "Join variable? "))
  > "join('" separator "', " var 
  (skeleton-read 
   > ", " str )
  > ");"
  )

(define-skeleton php-lcfirst
  "Insert a lcfirst statement. Make a string's first character lowercase"
  ""
  '(setq str (skeleton-read "String? "))
  > "lcfirst(" str ");" \n
)

(define-skeleton php-levenshtein
  "Insert a levenshtein statement. Calculate Levenshtein distance between two strings"
  ""
  '(setq str1 (skeleton-read "String? "))
  '(setq str2 (skeleton-read "String? "))
  > "levenshtein(" str1 ", " str2 ");" \n
)

(define-skeleton php-localeconv
  "Insert a localeconv statement."
  > "localeconv();" \n
)

(define-skeleton php-ltrim
  "Insert a ltrim statement"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq charlist (skeleton-read "Char list? "))
  > "ltrim(" str ", " charlist ");" \n)

(define-skeleton php-md5_file
  "Insert a md5_file statement"
  ""
  '(setq filename (skeleton-read "Filename? "))
  '(setq rawoutput (skeleton-read "Raw output? "))
  > "ltrim(" filename ", " rawoutput ");" \n)

(define-skeleton php-md5
  "Insert a md5 statement"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq rawoutput (skeleton-read "Raw output? "))
  > "md5(" str ", " rawoutput ");" \n)

(define-skeleton php-metaphone
  "Insert a metaphone statament"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq phonemes (skeleton-read "Phonemes? "))
  > "metaphone(" str ", " phonemes ");" \n)

(define-skeleton php-money_format
  "Insert a money_format statement"
  ""
  '(setq str (skeleton-read "Format? "))
  '(setq number (skeleton-read "Number? "))
  > "money_format(" str ", " number ");" \n)

(define-skeleton php-nl_langinfo
  "Insert a nl_langinfo statement"
  ""
  '(setq str (skeleton-read "Item? "))
  > "nl_langinfo(" str ");" \n)

(define-skeleton php-nl2br
  "Insert a nl2br statement"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq is_xhtml (skeleton-read "Is xhtml? "))
  > "nl2br(" str ", " is_xhtml ");" \n)

(define-skeleton php-number_format
  "Insert a number_format statement"
  ""
  '(setq float (skeleton-read "Float? "))
  '(setq number (skeleton-read "Number? "))
  > "number_format(" float ", " number ");" \n)

(define-skeleton php-ord
  "Insert an ord statement"
  ""
  '(setq char (skeleton-read "Character? "))
  > "ord(" str ");" \n)

(define-skeleton php-parse_str
  "Insert a parse_str statement"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq arr (skeleton-read "Array? "))
  > "parse_str(" str ", " arr ");" \n)

(define-skeleton php-print
  "Insert a print statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "print(" str ");" \n)

(define-skeleton php-printf
  "Insert a printf statement. Output a formatted string"
  ""
  '(setq str (skeleton-read "String? "))
  > "printf(" str ");" \n)

(define-skeleton php-quoted_printable_decode
  "Insert a quoted_printable_decode statement. Convert a quoted-printable string to an 8 bit string"
  ""
  '(setq str (skeleton-read "String? "))
  > "quoted_printable_decode(" str ");" \n)


(define-skeleton php-quoted_printable_encode
  "Insert a quoted_printable_encode statement. Convert a 8 bit string to a quoted-printable string"
  ""
  '(setq str (skeleton-read "String? "))
  > "quoted_printable_encode(" str ");" \n)

(define-skeleton php-quotemeta
  "Insert a quotemeta statement. Quote meta characters"
  ""
  '(setq str (skeleton-read "String? "))
  > "quotemeta(" str ");" \n)

(define-skeleton php-rtrim
  "Insert a rtrim statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "rtrim(" str ");" \n)

(define-skeleton php-setlocale
  "Insert a setlocale statement"
  '(setq category (skeleton-read "Category? "))
  '(setq locale (skeleton-read "Locale? "))
  > "setlocale(" category ", " locale ");" \n
)

(define-skeleton php-sha1_file
  "Insert a sha1_file statement. Calculate the sha1 of a file"
  ""
  '(setq filename (skeleton-read "Filename? "))
  '(setq rawoutuput (skeleton-read "Rawoutuput? "))
  > "sha1_file(" filename ", " rawoutuput ");" \n
)

(define-skeleton php-sha1
  "Insert a sha1 statement. Calculate the sha1 of a string"
  ""
  '(setq string (skeleton-read "String? "))
  '(setq rawoutuput (skeleton-read "Rawoutuput? "))
  > "sha1_file(" string ", " rawoutuput ");" \n
)

(define-skeleton php-similar_text
  "Insert a similar_text statement. Calculate the similarity between two words"
  ""
  '(setq first (skeleton-read "First string? "))
  '(setq second (skeleton-read "Second string? "))
  '(setq percentage (skeleton-read "Percentage? "))
  > "similar_text(" first ", " second ", " percentage ");" \n
)

(define-skeleton php-soundex
  "Insert a soundex statement. Calculates the soundex key of a string"
  ""
  '(setq str (skeleton-read "String? "))
  > "soundex(" str ");" \n
)

(define-skeleton php-sprintf
  "Insert a sprintf statement. Return a formatted string"
  ""
  '(setq str (skeleton-read "String? "))
  > "sprintf(" str ");" \n
)
  
(define-skeleton php-sscanf
  "Insert a sscanf statement. Parses input from a string according to a format"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq format (skeleton-read "Format? "))
  > "sprintf(" str ", " format ");" \n
)
  
(define-skeleton php-str_getcsv
  "Insert a str_getcsv statement. Parse a CSV string into an array"
  ""
  '(setq input (skeleton-read "Input? "))
  '(setq delimiter (skeleton-read "Delimiter? "))
  '(setq enclosure (skeleton-read "Enclosure? "))
  '(setq escape (skeleton-read "Escape? "))
  > "str_getcsv(" input ", " delimiter ", " enclosure ", " escape ");" \n
)

(define-skeleton php-str_ireplace
  "Insert a str_ireplace statement. Case-insensitive version of str_replace."
  ""
  '(setq search (skeleton-read "Search? "))
  '(setq replace (skeleton-read "Replace? "))
  '(setq subject (skeleton-read "Subject? "))
  '(setq count (skeleton-read "Count? "))
  > "str_ireplace(" search ", " replace ", " subject ", " count ");" \n
)

(define-skeleton php-str_pad
  "Insert a str_pad statement. Pad a string to a certain length with another string"
  ""
  '(setq input (skeleton-read "Input? "))
  '(setq pad_length (skeleton-read "Pad length? "))
  '(setq pad_string (skeleton-read "Pad string? "))
  '(setq pad_type (skeleton-read "Pad type? "))
  > "str_pad(" input ", " pad_length ", " pad_string ", " pad_type ");" \n
)

(define-skeleton php-str_repeat
  "Insert a str_repeat statement. Repeat a string"
  ""
  '(setq input (skeleton-read "Input? "))
  '(setq multiplier (skeleton-read "Multiplier? "))
  > "str_repeat(" input ", " multiplier ");" \n
)


(define-skeleton php-str_rot13
  "Insert a str_rot13 statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "str_rot13(" str ");" \n
)

(define-skeleton php-str_shuffle
  "Insert a str_shuffle statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "str_shuffle(" str ");" \n
)

(define-skeleton php-str_split
  "Insert a str_shuffle statement"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq split_length (skeleton-read "Split length? "))
  > "str_split(" str ", " split_length ");" \n
)

(define-skeleton php-str_word_count
  "Insert a str_word_count statement. Return information about words used in a string."
  ""
  '(setq str (skeleton-read "String? "))
  '(setq format (skeleton-read "Format? "))
  '(setq charlist (skeleton-read "Charlist? "))
  > "str_word_count(" str ", " format ", " charlist ");" \n
)

(define-skeleton php-strcasecmp
  "Insert a strcasecmp statement. Binary safe case-insensitive string comparison"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq str2 (skeleton-read "String? "))
  > "strcasecmp(" str ", " str2 ");" \n
)

(define-skeleton php-strcmp
  "Insert a strcmp statement. Binary safe string comparison"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq str2 (skeleton-read "String? "))
  > "strcmp(" str ", " str2 ");" \n
)

(define-skeleton php-strcoll
  "Insert a strcoll statement. Locale based string comparison"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq str2 (skeleton-read "String? "))
  > "strcoll(" str ", " str2 ");" \n
)

(define-skeleton php-strcspn
  "Insert a strcspn statement. Find length of initial segment not matching mask"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq str2 (skeleton-read "String? "))
  '(setq start (skeleton-read "The start position of the string? "))
  '(setq length (skeleton-read "The length of the string? "))
  > "strcspn(" str ", " str2 ");" \n
)

(define-skeleton php-strip_tags
  "Insert a strip_tags statement. Strip HTML and PHP tags from a string"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq allowable_tags (skeleton-read "Allowable tags? "))
  > "strcoll(" str ", " allowable_tags ");" \n
)

(define-skeleton php-stripslashes
  "Insert a stripcslashes statement."
  ""
  '(setq str (skeleton-read "String? "))
  > "stripcslashes(" str ");" \n
)

(define-skeleton php-stripcslashes
  "Insert a stripcslashes statement. Find the numeric position of the first occurrence of needle in the haystack string. "
  ""
  '(setq haystack (skeleton-read "String haystack? "))
  '(setq needle (skeleton-read "String needle? "))
  '(setq offset (skeleton-read "Offset?"))
  > "stripcslashes(" haystack ", " needle ", " offset ");" \n
)

(define-skeleton php-stristr
  "Insert a stristr statement. Case-insensitive strstr "
  ""
  '(setq haystack (skeleton-read "String haystack? "))
  '(setq needle (skeleton-read "String needle? "))
  '(setq before_needle (skeleton-read "Before needle?"))
  > "stripcslashes(" haystack ", " needle ", " before_needle ");" \n
)

(define-skeleton php-strlen
  "Insert a strlen statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "strlen(" str ");" \n)

(define-skeleton php-strnatcasecmp
  "Insert a strnatcasecmp statement"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq str2 (skeleton-read "String? "))
  > "strnatcasecmp(" str "," str2 ");" \n)

(define-skeleton php-strnatcmp
  "Insert a strnatcasecmp statement. String comparisons using a natural order algorithm"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq str2 (skeleton-read "String? "))
  > "strnatcmp(" str "," str2 ");" \n)

(define-skeleton php-strncasecmp
  "Insert a strncasecmp statement. Binary safe case-insensitive string comparison of the first n characters"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq str2 (skeleton-read "String? "))
  '(setq len (skeleton-read "Length? "))
  > "strncasecmp(" str "," str2 ", " len ");" \n)

(define-skeleton php-strncmp
  "Insert a strncmp statement." 
  ""
  '(setq str (skeleton-read "String? "))
  '(setq str2 (skeleton-read "String? "))
  '(setq len (skeleton-read "Length? "))
  > "strncmp(" str "," str2 ", " len ");" \n)

(define-skeleton php-strpbrk
  "Insert a strpbrk statement"
  ""
  '(setq haystack (skeleton-read "String? "))
  '(setq charlist (skeleton-read "Char list? "))
  > "strlen(" str ", " charlist ");" \n)

(define-skeleton php-strpos
  "Insert a strpos statement."
  ""
  '(setq haystack (skeleton-read "String haystack? "))
  '(setq needle (skeleton-read "String needle? "))
  '(setq offset (skeleton-read "Offset?"))
  > "strpos(" haystack ", " needle ", " offset ");" \n
)

(define-skeleton php-strrchr
  "Insert a strrchr statement. Find the last occurrence of a character in a string"
  ""
  '(setq haystack (skeleton-read "String haystack? "))
  '(setq needle (skeleton-read "String needle? "))
  > "strrchr(" haystack ", " needle ");" \n
)

(define-skeleton php-strrev
  "Insert a strrev statement. Reverse a string"
  ""
  '(setq string (skeleton-read "String? "))
  > "strrev(" string ");" \n
)

(define-skeleton php-strripos
  "Insert a strripos statement. Find the position of the last occurrence of a case-insensitive substring in a string"
  ""
  '(setq haystack (skeleton-read "String haystack? "))
  '(setq needle (skeleton-read "String needle? "))
  '(setq offset (skeleton-read "Offset?"))
  > "strripos(" string ", " needle ", " offset ");" \n
)

(define-skeleton php-strrpos
  "Insert a strripos statement. Find the position of the last occurrence of a substring in a string"
  ""
  '(setq haystack (skeleton-read "String haystack? "))
  '(setq needle (skeleton-read "String needle? "))
  '(setq offset (skeleton-read "Offset?"))
  > "strrpos(" string ", " needle ", " offset ");" \n
)

(define-skeleton php-strspn
  "Insert a strspn statement."
  ""
  '(setq subject (skeleton-read "Subject? "))
  '(setq mask (skeleton-read "Mask? "))
  '(setq start (skeleton-read "Start? "))
  '(setq length (skeleton-read "Length? "))
  > "strspn(" subject ", " mask ", " start ", " length ");" \n
)

(define-skeleton php-strstr
  "Insert a strstr statement. Find the first occurrence of a stringFind the first occurrence of a string"
  ""
  '(setq haystack (skeleton-read "String haystack? "))
  '(setq needle (skeleton-read "String needle? "))
  '(setq before_needle (skeleton-read "Before needle?"))
  > "strstr(" string ", " needle ", " before_needle ");" \n
)

(define-skeleton php-strtok
  "Insert a strlower statement"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq token (skeleton-read "Token? "))
  > "strtolower(" str ");" \n)


(define-skeleton php-strtolower
  "Insert a strlower statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "strtolower(" str ");" \n)

(define-skeleton php-strtotime
  "Insert a strtotime statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "strtotime(" str ");" \n)

(define-skeleton php-strtoupper
  "Insert a strtoupper statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "strtoupper(" str ");" \n)


(define-skeleton php-strtr 
  "Insert a strtr statement"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq from (skeleton-read "From? "))
  '(setq to (skeleton-read "To? "))
  > "strtr(" str ", " from ", " to ");" \n
)

(define-skeleton php-substr_compare
  "Insert a substr_compare"
  ""
  '(setq main_str (skeleton-read "The main string being compared? "))
  '(setq str (skeleton-read "The secondary string being compared? "))
  '(setq offset (skeleton-read "The start position for the comparison? "))
  '(setq length (skeleton-read "The length of the comparison? "))
  > "substr_compare(" main_str ", " str ", " offset ", " length ");" \n
)

(define-skeleton php-substr_count
  "Insert a substr_count"
  ""
  '(setq haystack (skeleton-read "String haystack? "))
  '(setq needle (skeleton-read "String needle? "))
  '(setq offset (skeleton-read "Offset?"))
  '(setq length (skeleton-read "Length? "))
  > "substr_count(" haystack ", " needle ", " offset ", " length ");" \n
)

(define-skeleton php-substr_replace
  "Insert a substr_replace"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq replacement (skeleton-read "Replacement? "))
  '(setq start (skeleton-read "Start? "))
  '(setq length (skeleton-read "Length? "))
  > "substr_replace(" str ", " replacement ", " start ", " length ");" \n
)

(define-skeleton php-substr
  "Insert a substr_replace statement"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq start (skeleton-read "Start? "))
  '(setq length (skeleton-read "Length? "))
  > "substr(" str ", " start ", " length ");" \n
)

(define-skeleton php-trim
  "Insert a trim statement"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq charlist (skeleton-read "Charlist? "))
  > "trim(" str ", " charlist ");" \n
)

(define-skeleton php-ucfirst
  "Insert a ucfirst statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "ucfirst('" str "');" \n)

(define-skeleton php-ucwords
  "Insert a ucfirst statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "ucwords('" str "');" \n)

(define-skeleton php-vfprintf 
  "Insert a vfprintf statement"
  ""
  '(setq handle (skeleton-read "Handle? "))
  '(setq format (skeleton-read "Format? "))
  '(setq args (skeleton-read "Args? "))
  > "vfprintf(" handle ", " format ", " args ");" \n
)

(define-skeleton php-vprintf 
  "Insert a vprintf statement"
  ""
  '(setq format (skeleton-read "Format? "))
  '(setq args (skeleton-read "Args? "))
  > "vprintf(" format ", " args ");" \n
)

(define-skeleton php-vsprintf 
  "Insert a vsprintf statement"
  ""
  '(setq format (skeleton-read "Format? "))
  '(setq args (skeleton-read "Args? "))
  > "vsprintf(" format ", " args ");" \n
)

(define-skeleton php-wordwrap
  "Insert a wordwrap statement. Wraps a string to a given number of characters" 
  ""
  '(setq str (skeleton-read "String? "))
  '(setq width (skeleton-read "Width? "))
  '(setq break (skeleton-read "Break? "))
  '(setq cut (skeleton-read "Cut? (TRUE | FALSE) "))
  > "wordwrap(" str ", " width ", " break ", " cut ");" \n
)
