;;; haskell-string.el --- Haskell related string utilities -*- lexical-binding: t -*-

;; Copyright (C) 2013  Herbert Valerio Riedel

;; Author: Herbert Valerio Riedel <hvr@gnu.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Todo:

;; - write ERT tests

;;; Code:

(require 'cl-lib)

(defun haskell-string-trim (string)
  "Remove whitespace around STRING.

A Whitespace character is defined in the Haskell Report as follows

  whitechar -> newline | vertab | space | tab | uniWhite
  newline   -> return linefeed | return | linefeed | formfeed
  uniWhite  -> any Unicode character defined as whitespace

Note: The implementation currently only supports ASCII
      white-space characters, i.e. the implemention doesn't
      consider uniWhite."

  (let ((s1 (if (string-match "[\t\n\v\f\r ]+\\'" string) (replace-match "" t t string) string)))
    (if (string-match "\\`[\t\n\v\f\r ]+" s1) (replace-match "" t t s1) s1)))

(defun haskell-string-only-spaces-p (string)
  "Return t if STRING contains only whitespace (or is empty)."
  (string= "" (haskell-string-trim string)))

(defun haskell-string-take (string n)
  "Return (up to) N character length prefix of STRING."
  (substring string 0 (min (length string) n)))

(defconst haskell-string-literal-encode-ascii-array
  [ "\\NUL" "\\SOH" "\\STX" "\\ETX" "\\EOT" "\\ENQ" "\\ACK" "\\a" "\\b" "\\t" "\\n" "\\v" "\\f" "\\r" "\\SO" "\\SI" "\\DLE" "\\DC1" "\\DC2" "\\DC3" "\\DC4" "\\NAK" "\\SYN" "\\ETB" "\\CAN" "\\EM" "\\SUB" "\\ESC" "\\FS" "\\GS" "\\RS" "\\US" " " "!" "\\\"" "#" "$" "%" "&" "'" "(" ")" "*" "+" "," "-" "." "/" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" ":" ";" "<" "=" ">" "?" "@" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "[" "\\\\" "]" "^" "_" "`" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "{" "|" "}" "~" "\\DEL" ]
  "Array of encodings for 7-bit ASCII character points indexed by ASCII value.")

(defun haskell-string-literal-encode (str &optional no-quotes)
  "Encode STR according Haskell escape rules using 7-bit ASCII representation.

The serialization has been implemented to closely match the
behaviour of GHC's Show instance for Strings.

If NO-QUOTES is non-nil, omit wrapping result in quotes.

This is the dual operation to `haskell-string-literal-decode'."

  (let ((lastc -1))
    (let ((encode (lambda (c)
                    (let ((lc lastc))
                      (setq lastc c)
                      (if (>= c 128) ;; if non-ASCII code point
                          (format "\\%d" c)
                        ;; else, for ASCII code points
                        (if (or (and (= lc 14) (= c ?H)) ;; "\SO\&H"
                                (and (>= lc 128) (>= c ?0) (<= c ?9))) ;; "\123\&4"
                            (concat "\\&" (aref haskell-string-literal-encode-ascii-array c))
                          (aref haskell-string-literal-encode-ascii-array c)
                          ))))))

      (if no-quotes
          (mapconcat encode str "")
        (concat "\"" (mapconcat encode str "") "\"")))))

(defconst haskell-string-literal-escapes-regexp
  (concat "[\\]\\(?:"
          (regexp-opt (append
                       (mapcar (lambda (c) (format "%c" c))
                               "abfnrtv\\\"'&") ;; "charesc" escape sequences
                       (mapcar (lambda (c) (format "^%c" c))
                               "ABCDEFGHIJKLMNOPQRSTUVWXYZ@[\\]^_") ;; "cntrl" escape sequences
                       (mapcar (lambda (s) (format "%s" s))
                               (split-string "NUL SOH STX ETX EOT ENQ ACK BEL BS HT LF VT FF CR
                                              SO SI DLE DC1 DC2 DC3 DC4 NAK SYN ETB CAN EM SUB ESC
                                              FS GS RS US SP DEL")))) ;; "ascii" (w\o "cntrl") escape sequences
          "\\|" "[\t\n\v\f\r ]+[\\]"  ;; whitespace gaps
          "\\|" "[0-9]+"              ;; decimal escape sequence
          "\\|" "o[0-7]+"             ;; octal escape sequence
          "\\|" "x[0-9a-f]+"          ;; hex escape sequence
          "\\)?") ;; everything else is an invalid escape sequence
  "Regexp for matching escape codes in string literals.
See Haskell Report Sect 2.6,
URL `http://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6',
for more details.")

(defconst haskell-string-literal-decode1-table
  (let ((h (make-hash-table :test 'equal)))
    (mapc (lambda (c) (puthash (concat "\\" (car c)) (cdr c) h))
          '(;; ascii-escapes
            ("NUL" . "\x00") ("SOH" . "\x01") ("STX" . "\x02") ("ETX" . "\x03") ("EOT" . "\x04") ("ENQ" . "\x05")
            ("ACK" . "\x06") ("BEL" . "\x07") ("BS"  . "\x08") ("HT"  . "\x09") ("LF"  . "\x0a") ("VT"  . "\x0b")
            ("FF"  . "\x0c") ("CR"  . "\x0d") ("SO"  . "\x0e") ("SI"  . "\x0f") ("DLE" . "\x10") ("DC1" . "\x11")
            ("DC2" . "\x12") ("DC3" . "\x13") ("DC4" . "\x14") ("NAK" . "\x15") ("SYN" . "\x16") ("ETB" . "\x17")
            ("CAN" . "\x18") ("EM"  . "\x19") ("SUB" . "\x1a") ("ESC" . "\x1b") ("FS"  . "\x1c") ("GS"  . "\x1d")
            ("RS"  . "\x1e") ("US"  . "\x1f") ("SP"  . "\x20")                                   ("DEL" . "\x7f" )
            ;; C-compatible single-char escape sequences
            ("a" . "\x07") ("b" . "\x08") ("f" . "\x0c") ("n" . "\x0a") ("r" . "\x0d") ("t" . "\x09") ("v" . "\x0b")
            ;; trivial escapes
            ("\\" . "\\") ("\"" . "\"") ("'" . "'")
            ;; "empty" escape
            ("&" . "")))
    h)
  "Hash table containing irregular escape sequences and their decoded strings.
Used by `haskell-string-literal-decode1'.")

(defun haskell-string-literal-decode1 (l)
  "Decode a single string literal escape sequence.
L must contain exactly one escape sequence.
This is an internal function used by `haskell-string-literal-decode'."
  (let ((case-fold-search nil))
    (cond
     ((gethash l haskell-string-literal-decode1-table))
     ((string-match "\\`[\\][0-9]+\\'" l)         (char-to-string (string-to-number (substring l 1) 10)))
     ((string-match "\\`[\\]x[[:xdigit:]]+\\'" l) (char-to-string (string-to-number (substring l 2) 16)))
     ((string-match "\\`[\\]o[0-7]+\\'" l)        (char-to-string (string-to-number (substring l 2) 8)))
     ((string-match "\\`[\\]\\^[@-_]\\'" l)       (char-to-string (- (aref l 2) ?@))) ;; "cntrl" escapes
     ((string-match "\\`[\\][\t\n\v\f\r ]+[\\]\\'" l) "") ;; whitespace gap
     (t (error "Invalid escape sequence")))))

(defun haskell-string-literal-decode (estr &optional no-quotes)
  "Decode a Haskell string-literal.
If NO-QUOTES is nil, ESTR must be surrounded by quotes.

This is the dual operation to `haskell-string-literal-encode'."
  (if (and (not no-quotes)
           (string-match-p "\\`\"[^\\\"[:cntrl:]]*\"\\'" estr))
      (substring estr 1 -1) ;; optimized fast-path for trivial strings
    (let ((s (if no-quotes ;; else: do general decoding
                 estr
               (if (string-match-p "\\`\".*\"\\'" estr)
                   (substring estr 1 -1)
                 (error "String literal must be delimited by quotes"))))
          (case-fold-search nil))
      (replace-regexp-in-string haskell-string-literal-escapes-regexp #'haskell-string-literal-decode1 s t t))))

(defun haskell-string-ellipsize (string n)
  "Return STRING truncated to (at most) N characters.
If truncation occured, last character in string is replaced by `…'.
See also `haskell-string-take'."
  (cond
   ((<= (length string) n) string) ;; no truncation needed
   ((< n 1) "")
   (t (concat (substring string 0 (1- n)) "…"))))

(defun haskell-string-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun haskell-string-split-to-lines (str)
  "Split STR to lines and return a list of strings with preceeding and
succeding space removed."
  (when (stringp str)
    (cl-mapcar #'haskell-string-chomp (split-string str "\n"))))

(defun haskell-string-trim-prefix (prefix str)
  "If PREFIX is prefix of STR, the string is trimmed."
  (when (and (stringp prefix)
             (stringp str))
    (if (string-prefix-p prefix str)
        (substring str (length prefix)))))

(defun haskell-string-trim-suffix (suffix str)
  "If SUFFIX is suffix of STR, the string is trimmed."
  (when (and (stringp suffix)
             (stringp str))
    (if (string-suffix-p suffix str)
        (substring str 0 (* -1 (length suffix))))))

(defun haskell-string-drop-qualifier (ident)
  "Drop qualifier from given identifier IDENT.

If the identifier is not qualified return it unchanged."
  (or (and (string-match "^\\([^.]*\\.\\)*\\(?1:[^.]+\\)$" ident)
           (match-string 1 ident))
      ident))

(defun haskell-mode-message-line (str)
  "Echo STR in mini-buffer.
Given string is shrinken to single line, multiple lines just
disturbs the programmer."
  (message "%s" (haskell-mode-one-line str (frame-width))))

(defun haskell-mode-one-line (str &optional width)
  "Try to fit STR as much as possible on one line according to given WIDTH."
  (unless width
    (setq width (length str)))
  (let* ((long-line (replace-regexp-in-string "\n" " " str))
         (condensed  (replace-regexp-in-string
                      " +" " " (haskell-string-trim long-line))))
    (truncate-string-to-width condensed width nil nil "…")))

(provide 'haskell-string)

;;; haskell-string.el ends here
