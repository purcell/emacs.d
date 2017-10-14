;;; haskell-lexeme.el --- haskell lexical tokens   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2015 Gracjan Polak

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'rx)

(unless (category-docstring ?P)
  (define-category ?P "Haskell symbol constituent characters")
  (map-char-table
   #'(lambda (key val)
       (if (or
            (and (consp key) (> (car key) 128))
            (and (numberp key) (> key 128)))
           (if (member val '(Pc Pd Po Sm Sc Sk So))
               (modify-category-entry key ?P))))
   unicode-category-table)

  (dolist (key (string-to-list "!#$%&*+./<=>?@^|~\\-:"))
    (modify-category-entry key ?P)))

(defconst haskell-lexeme-modid
  "[[:upper:]][[:alnum:]'_]*"
  "Regexp matching a valid Haskell module identifier.

Note that GHC accepts Unicode category UppercaseLetter as a first
character. Following letters are from Unicode categories
UppercaseLetter, LowercaseLetter, OtherLetter, TitlecaseLetter,
ModifierLetter, DecimalNumber, OtherNumber, backslash or
underscore.")

(defconst haskell-lexeme-id
  "[[:alpha:]_][[:alnum:]'_]*"
  "Regexp matching a valid Haskell identifier.

GHC accepts a string starting with any alphabetic character or
underscore followed by any alphanumeric character or underscore
or apostrophe.")

(defconst haskell-lexeme-sym
  "\\cP+"
  "Regexp matching a valid Haskell variable or constructor symbol.

GHC accepts a string of chars from the set
[:!#$%&*+./<=>?@^|~\\-] or Unicode category Symbol for chars with
codes larger than 128 only.")

(defconst haskell-lexeme-idsym-first-char
  "\\(?:[[:alpha:]_]\\|\\cP\\)"
  "Regexp matching first character of a qualified or unqualified
identifier or symbol.

Useful for `re-search-forward'.")

(defconst haskell-lexeme-modid-opt-prefix
  (concat "\\(?:" haskell-lexeme-modid "\\.\\)*")
  "Regexp matching a valid Haskell module prefix, potentially empty.

Module path prefix is separated by dots and finishes with a
dot. For path component syntax see `haskell-lexeme-modid'.")

(defconst haskell-lexeme-qid-or-qsym
  (rx-to-string `(: (regexp ,haskell-lexeme-modid-opt-prefix)
                    (group (| (regexp ,haskell-lexeme-id) (regexp ,haskell-lexeme-sym)
                              ))))
  "Regexp matching a valid qualified identifier or symbol.

Note that (match-string 1) returns the unqualified part.")

(defun haskell-lexeme-looking-at-qidsym ()
  "Non-nil when point is just in front of an optionally qualified
identifier or symbol.

Using this function is more efficient than matching against the
regexp `haskell-lexeme-qid-or-qsym'.

Returns:
  'qid - if matched a qualified id: 'Data.Map' or 'Map'
  'qsym - if matched a qualified id: 'Monad.>>=' or '>>='
  'qprefix - if matched only modid prefix: 'Data.'

After successful 'qid or 'qsym match (match-string 1) will return
the unqualified part (if any)."
  (let ((begin (point))
        (match-data-old (match-data)))
    (save-excursion
      (while (looking-at (concat haskell-lexeme-modid "\\."))
        (goto-char (match-end 0)))
      (cond
       ((looking-at haskell-lexeme-id)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))

          ;; check is MagicHash is present at the end of the token
          (goto-char end)
          (when (looking-at "#+")
            (setq end (match-end 0)))

          (set-match-data
           (list begin end
                 beg end)))
        'qid)
       ((looking-at haskell-lexeme-sym)
        (set-match-data
          (list begin (match-end 0)
                (match-beginning 0) (match-end 0)))
        'qsym)
       ((equal begin (point))
        (set-match-data match-data-old)
        nil)
       (t
        (set-match-data
         (list begin (point)
               nil nil))
        'qprefix)))))

(defun haskell-lexeme-looking-at-backtick ()
  "Non-nil when point is just in front of an identifier quoted with backticks.

When match is successful, match-data will contain:
  (match-text 1) - opening backtick
  (match-text 2) - whole qualified identifier
  (match-text 3) - unqualified part of identifier
  (match-text 4) - closing backtick"
  (let ((match-data-old (match-data))
        first-backtick-start
        last-backtick-start
        qid-start
        id-start
        id-end
        result)
    (save-excursion
      (when (looking-at "`")
        (setq first-backtick-start (match-beginning 0))
        (goto-char (match-end 0))
        (forward-comment (buffer-size))
        (when (haskell-lexeme-looking-at-qidsym)
          (setq qid-start (match-beginning 0))
          (setq id-start (match-beginning 1))
          (setq id-end (match-end 1))
          (goto-char (match-end 0))
          (forward-comment (buffer-size))
          (when (looking-at "`")
            (setq last-backtick-start (match-beginning 0))
            (set-match-data
             (mapcar
              (lambda (p)
                (set-marker (make-marker) p))
              (list
               first-backtick-start (1+ last-backtick-start)
               first-backtick-start (1+ first-backtick-start)
               qid-start id-end
               id-start id-end
               last-backtick-start (1+ last-backtick-start))))
            (setq result t)))))
    (unless result
      (set-match-data match-data-old))
    result))

(defconst haskell-lexeme-qid
  (rx-to-string `(: (regexp "'*")
                    (regexp ,haskell-lexeme-modid-opt-prefix)
                    (group (regexp ,haskell-lexeme-id))))
  "Regexp matching a valid qualified identifier.

Note that (match-string 1) returns the unqualified part.")

(defconst haskell-lexeme-qsym
  (rx-to-string `(: (regexp "'*")
                    (regexp ,haskell-lexeme-modid-opt-prefix)
                    (group (regexp ,haskell-lexeme-id))))
  "Regexp matching a valid qualified symbol.

Note that (match-string 1) returns the unqualified part.")

(defconst haskell-lexeme-number
  (rx (| (: (regexp "[0-9]+\\.[0-9]+") (opt (regexp "[eE][-+]?[0-9]+")))
         (regexp "[0-9]+[eE][-+]?[0-9]+")
         (regexp "0[xX][0-9a-fA-F]+")
         (regexp "0[oO][0-7]+")
         (regexp "[0-9]+")))
  "Regexp matching a floating point, decimal, octal or hexadecimal number.

Note that negative sign char is not part of a number.")

(defconst haskell-lexeme-char-literal-inside
  (rx (| (not (any "\n'\\"))
         (: "\\"
            (| "a" "b" "f" "n" "r" "t" "v" "\\" "\"" "'"
               "NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK"
               "BEL" "BS" "HT" "LF" "VT" "FF" "CR" "SO" "SI" "DLE"
               "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN"
               "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "SP" "DEL"
               (regexp "[0-9]+")
               (: "x" (regexp "[0-9a-fA-F]+"))
               (: "o" (regexp "[0-7]+"))
               (: "^" (regexp "[]A-Z@^_\\[]"))))))
  "Regexp matching an inside of a character literal.

Note that `haskell-lexeme-char-literal-inside' matches strictly
only escape sequences defined in Haskell Report.")

(defconst haskell-lexeme--char-literal-rx
  (rx-to-string `(: (group "'")
                    (| (: (group (regexp "[[:alpha:]_([]")) (group "'")) ; exactly one char
                       (: (group (| (regexp "\\\\[^\n][^'\n]*") ; allow quote just after first backslash
                                    (regexp "[^[:alpha:]_(['\n][^'\n]*")))
                          (| (group "'") "\n" (regexp "\\'"))))))
  "Regexp matching a character literal lookalike.

Note that `haskell-lexeme--char-literal-rx' matches more than
Haskell Report specifies because we want to support also code
under edit.

Character literals end with a quote or a newline or end of
buffer.

Regexp has subgroup expressions:
 (match-text 1) matches the opening quote.
 (match-text 2) matches the inside of the character literal.
 (match-text 3) matches the closing quote or an empty string
                at the end of line or the end buffer.")

(defun haskell-lexeme-looking-at-char-literal ()
  "Non-nil when point is at a char literal lookalike.

Note that this function matches more than Haskell Report
specifies because we want to support also code under edit.

Char literals end with a quote or an unescaped newline or end
of buffer.

After successful match:
 (match-text 1) matches the opening quote.
 (match-text 2) matches the inside of the char literla.
 (match-text 3) matches the closing quote, or a closing
                newline or is nil when at the end of the buffer."
  (when (looking-at haskell-lexeme--char-literal-rx)
    (set-match-data
     (list (match-beginning 0) (match-end 0)
           (match-beginning 1) (match-end 1)
           (or (match-beginning 2) (match-beginning 4)) (or (match-end 2) (match-end 4))
           (or (match-beginning 3) (match-beginning 5)) (or (match-end 3) (match-end 5))))
    t))

(defconst haskell-lexeme-string-literal-inside-item
  (rx (| (not (any "\n\"\\"))
         (: "\\"
            (| "a" "b" "f" "n" "r" "t" "v" "\\" "\"" "'" "&"
               "NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK"
               "BEL" "BS" "HT" "LF" "VT" "FF" "CR" "SO" "SI" "DLE"
               "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN"
               "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "SP" "DEL"
               (regexp "[0-9]+")
               (: "x" (regexp "[0-9a-fA-F]+"))
               (: "o" (regexp "[0-7]+"))
               (: "^" (regexp "[]A-Z@^_\\[]"))
               (regexp "[ \t\n\r\v\f]*\\\\")))))
  "Regexp matching an item that is a single character or a single
escape sequence inside of a string literal.

Note that `haskell-lexeme-string-literal-inside-item' matches
strictly only escape sequences defined in Haskell Report.")

(defconst haskell-lexeme-string-literal
  (rx (: (group "\"")
         (group (* (| (regexp "\\\\[ \t\n\r\v\f]*\\\\")
                      (regexp "\\\\[ \t\n\r\v\f]+")
                      (regexp "\\\\[^ \t\n\r\v\f]")
                      (* (regexp "[^\"\n\\]")))))
         (group (| "\"" (regexp "$") (regexp "\\\\?\\'")
                   ))))
  "Regexp matching a string literal lookalike.

Note that `haskell-lexeme-string-literal' matches more than
Haskell Report specifies because we want to support also code
under edit.

String literals end with double quote or unescaped newline or end
of buffer.

Regexp has subgroup expressions:
 (match-text 1) matches the opening double quote.
 (match-text 2) matches the inside of the string.
 (match-text 3) matches the closing double quote or an empty string
                at the end of line or the end buffer.")

(defun haskell-lexeme-looking-at-string-literal ()
  "Non-nil when point is at a string literal lookalike.

Note that this function matches more than Haskell Report
specifies because we want to support also code under edit.

String literals end with double quote or unescaped newline or end
of buffer.

After successful match:
 (match-text 1) matches the opening doublequote.
 (match-text 2) matches the inside of the string.
 (match-text 3) matches the closing quote, or a closing
                newline or is nil when at the end of the buffer."
  (when (looking-at "\"")
    (save-excursion
      (let ((begin (point)))
        (goto-char (match-end 0))
        (let (finish)
          (while (and (not finish)
                      (re-search-forward "[\"\n\\]" nil 'goto-eob))
            (cond
             ((equal (match-string 0) "\\")
              (if (looking-at "[ \t\n\r\v\f]+\\\\?")
                  (goto-char (match-end 0))
                (goto-char (1+ (point)))))

             ((equal (match-string 0) "\"")
              (set-match-data
               (list begin (match-end 0)
                     begin (1+ begin)
                     (1+ begin) (match-beginning 0)
                     (match-beginning 0) (match-end 0)))
              (setq finish t))

             ((equal (match-string 0) "\n")
              (set-match-data
               (list begin (match-beginning 0)
                     begin (1+ begin)
                     (1+ begin) (match-beginning 0)
                     nil nil))
              (setq finish t))))
          (unless finish
            ;; string closed by end of buffer
            (set-match-data
             (list begin (point)
                   begin (1+ begin)
                   (1+ begin) (point)
                   nil nil))))))
    ;; there was a match
    t))

(defun haskell-lexeme-looking-at-quasi-quote-literal ()
  "Non-nil when point is just in front of Template Haskell
quaisquote literal.

Quasi quotes start with '[xxx|' or '[$xxx|' sequence and end with
  '|]'. The 'xxx' is a quoter name. There is no escaping mechanism
provided for the ending sequence.

Regexp has subgroup expressions:
 (match-text 1) matches the quoter name (without $ sign if present).
 (match-text 2) matches the opening vertical bar.
 (match-text 3) matches the inside of the quoted string.
 (match-text 4) matches the closing vertical bar
                or nil if at the end of the buffer.

Note that this function excludes 'e', 't', 'd', 'p' as quoter
names according to Template Haskell specification."
  (let ((match-data-old (match-data)))
    (if (and
         (looking-at (rx-to-string `(: "[" (optional "$")
                                       (group (regexp ,haskell-lexeme-id))
                                       (group "|"))))
         (equal (haskell-lexeme-classify-by-first-char (char-after (match-beginning 1)))
                'varid)
         (not (member (match-string 1) '("e" "t" "d" "p"))))
      (save-excursion
        ;; note that quasi quote syntax does not have any escaping
        ;; mechanism and if not closed it will span til lthe end of buffer
        (goto-char (match-end 0))
        (let ((match-data (match-data))
              (match-data-2 (and (re-search-forward "|]" nil t)
                                 (match-data))))
          (if match-data-2
              (set-match-data
               (list
                (nth 0 match-data) (nth 1 match-data-2)          ;; whole match
                (nth 2 match-data) (nth 3 match-data)            ;; quoter name
                (nth 4 match-data) (nth 5 match-data)            ;; opening bar
                (nth 5 match-data) (nth 0 match-data-2)          ;; inner string
                (nth 0 match-data-2) (1+ (nth 0 match-data-2)))) ;; closing bar

            (set-match-data
             (list
              (nth 0 match-data) (point-max)                   ;; whole match
              (nth 2 match-data) (nth 3 match-data)            ;; quoter name
              (nth 4 match-data) (nth 5 match-data)            ;; opening bar
              (nth 5 match-data) (point-max)                   ;; inner string
              nil nil))                                        ;; closing bar
            ))
        t)
      ;; restore old match data if not matched
      (set-match-data match-data-old)
      nil)))

(defun haskell-lexeme-classify-by-first-char (char)
  "Classify token by CHAR.

CHAR is a chararacter that is assumed to be the first character
of a token."
  (let ((category (get-char-code-property (or char ?\ ) 'general-category)))

    (cond
     ((or (member char '(?! ?# ?$ ?% ?& ?* ?+ ?. ?/ ?< ?= ?> ?? ?@ ?^ ?| ?~ ?\\ ?-))
          (and (> char 127)
               (member category '(Pc Pd Po Sm Sc Sk So))))
      'varsym)
     ((equal char ?:)
      'consym)
     ((equal char ?\')
      'char)
     ((equal char ?\")
      'string)
     ((member category '(Lu Lt))
      'conid)
     ((or (equal char ?_)
          (member category '(Ll Lo)))
      'varid)
     ((and (>= char ?0) (<= char ?9))
      'number)
     ((member char '(?\] ?\[ ?\( ?\) ?\{ ?\} ?\` ?\, ?\;))
      'special))))

(defun haskell-lexeme-looking-at-token (&rest flags)
  "Like `looking-at' but understands Haskell lexemes.

Moves point forward over whitespace.  Returns a symbol describing
type of Haskell token recognized.  Use `match-string',
`match-beginning' and `match-end' with argument 0 to query match
result.

Possible results are:
- 'special: for chars [](){}`,;
- 'comment: for single line comments
- 'nested-comment: for multiline comments
- 'qsymid: for qualified identifiers or symbols
- 'string: for strings literals
- 'char: for char literals
- 'number: for decimal, float, hexadecimal and octal number literals
- 'template-haskell-quote: for a string of apostrophes for template haskell
- 'template-haskell-quasi-quote: for a string of apostrophes for template haskell

Note that for qualified symbols (match-string 1) returns the
unqualified identifier or symbol.  Further qualification for
symbol or identifier can be done with:

   (haskell-lexeme-classify-by-first-char (char-after (match-beginning 1)))

See `haskell-lexeme-classify-by-first-char' for details."
  (while
      ;; Due to how unterminated strings terminate at newline, some
      ;; newlines have syntax set to generic string delimeter. We want
      ;; those to be treated as whitespace anyway
      (or
       (> (skip-syntax-forward "-") 0)
       (and (not (member 'newline flags))
            (> (skip-chars-forward "\n") 0))))
  (let
      ((case-fold-search nil)
       (point (point-marker)))
    (or
     (and
      (equal (string-to-syntax "<")
             (get-char-property (point) 'syntax-table))
      (progn
        (set-match-data (list point (set-marker (make-marker) (line-end-position))))
        'literate-comment))
     (and (looking-at "\n")
          'newline)
     (and (looking-at "{-")
          (save-excursion
            (forward-comment 1)
            (set-match-data (list point (point-marker)))
            'nested-comment))
     (and (haskell-lexeme-looking-at-char-literal)
          'char)
     (and (haskell-lexeme-looking-at-string-literal)
          'string)
     (and (looking-at "[][(){}`,;]")
          (if (haskell-lexeme-looking-at-quasi-quote-literal)
              'template-haskell-quasi-quote
            'special))
     (and (haskell-lexeme-looking-at-qidsym)
          (if (save-match-data
                (string-match "\\`---*\\'" (match-string-no-properties 0)))
              (progn
                (set-match-data (list point (set-marker (make-marker) (line-end-position))))
                'comment)
            'qsymid))
     (and (looking-at haskell-lexeme-number)
          'number)
     (and (looking-at "'+")
          'template-haskell-quote)
     (and (looking-at ".")
          'illegal))))

(provide 'haskell-lexeme)

;;; haskell-lexeme.el ends here
