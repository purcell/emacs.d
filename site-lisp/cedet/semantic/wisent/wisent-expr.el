;;; wisent-expr.el --- Infix to prefix expression converter

;; Copyright (C) 2001 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 19 June 2001
;; Keywords: syntax
;; X-RCS: $Id: wisent-expr.el,v 1.4 2005/09/30 20:23:24 zappo Exp $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Sample program using the elisp LALR parser Wisent. It just converts
;; expressions from infix (C like) to prefix notation (Lisp like).

;;; History:
;; 

;;; Code:

(require 'wisent)

(defconst wisent-expr-parser-tables
  (eval-when-compile
    (wisent-compile-grammar
     '(
       ;; terminals
       (NUMBER MINUS PLUS DIV MULT LPAREN RPAREN SEMI)
       ;; no operator precedence
       nil
       ;; non terminals
       (grammar ((grammar expr)
                 (format "%s %s" $1 $2))
                ((expr)
                 (format "%s" $1))
                )
       (expr    ((add SEMI)
                 (format "%s%s" $1 $2))
                ((SEMI)
                 ";")
                ((error SEMI) ;; on parse error skip tokens until
                 "\"error\";") ;; next semicolon and return "error";
                )
       (add     ((add MINUS mult)
                 (list '- $1 $3))
                ((add PLUS mult)
                 (list '+ $1 $3))
                ((mult))
                )
       (mult    ((mult DIV final)
                 (list '/ $1 $3))
                ((mult MULT final)
                 (list '* $1 $3))
                ((final))
                )
       (final   ((LPAREN add RPAREN)
                 $2)
                ((NUMBER))
                )
       )))
  "Expression converter parser tables.")

(defconst wisent-expr-operators
  '((?\; . SEMI)
    (?\( . LPAREN)
    (?\) . RPAREN)
    (?\+ . PLUS)
    (?\- . MINUS)
    (?\* . MULT)
    (?\/ . DIV))
  "Expression converter operator terminals.")

(defconst wisent-expr-number-regexp
  (eval-when-compile
    (concat "^\\("
            "[0-9]+\\([.][0-9]*\\)?\\([eE][-+]?[0-9]+\\)?"
            "\\|"
            "[.][0-9]+\\([eE][-+]?[0-9]+\\)?"
            "\\)"
            ))
  "Regexp to match the expression converter number terminals.")

(defvar wisent-expr-lexer-input-stream nil
  "The expression converter lexer input stream.")

(defun wisent-expr-lexer ()
  "The expression converter lexer."
  (let* ((is  (or wisent-expr-lexer-input-stream ""))
         (k   (string-match "\\S-" is)) ;; skip spaces
         (lex (list wisent-eoi-term)))
    (if (not k)
        nil
      (setq is (substring is k))
      (cond
       ;; Number
       ((string-match wisent-expr-number-regexp is)
        (setq lex (list 'NUMBER (read (match-string 0 is)))
              is  (substring is (match-end 0))))
       ;; Operator
       ((setq k (assq (aref is 0) wisent-expr-operators))
        (setq lex (list (cdr k) (string (aref is 0)))
              is  (substring is 1)))
       ;; Invalid input
       (t
        (error "Invalid input character '%c'" (aref is 0))))
      (setq wisent-expr-lexer-input-stream is))
    lex))

(defun wisent-expr (input)
  "Infix to prefix expression converter.
Parse INPUT string and output the result of computation."
  (interactive "sexpr: ")
  (or (string-match ";\\s-*$" input)
      (setq input (concat input ";")))
  (let ((wisent-expr-lexer-input-stream input))
    (message "%s -> %s"
             input
             (wisent-parse wisent-expr-parser-tables
                           #'wisent-expr-lexer
                           #'message))))

(provide 'wisent-expr)

;;; wisent-expr.el ends here
