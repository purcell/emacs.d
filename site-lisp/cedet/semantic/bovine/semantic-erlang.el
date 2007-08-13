;;; semantic-erlang.el --- Semantic details for Erlang

;; Copyright (C) 2001, 2002, 2003 Vladimir G. Sekissov
;; Copyright (C) 2003 David Ponce

;; Author: Vladimir G. Sekissov <svg@surnet.ru>
;;         David Ponce <david@dponce.com>
;; Keywords: syntax
;; X-RCS: $Id: semantic-erlang.el,v 1.4 2005/09/30 20:22:18 zappo Exp $

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;

;;; History:
;;

;;; Code:

(require 'semantic)
(require 'backquote)
(require 'semantic-erlang-by)
(require 'erlang-edoc)

;; Depending on what elements you include specialized support for
(eval-when-compile
  (require 'semantic-ctxt)
  (require 'semantic-imenu)
  (require 'document)
  (require 'senator))

(define-lex-simple-regex-analyzer semantic-erlang-lex-char
  "Detect and create Erlang CHAR tokens."
  "\\s/\\(\\(\\\\[0-9]\\{3\\}\\)\\|[^\\]\\|\\\\\\\\\\)"
  'CHAR)

(define-lex semantic-erlang-lexer
  "Lexical Analyzer for Erlang code."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  ;;semantic-lex-number
  semantic-lex-symbol-or-keyword
  semantic-erlang-lex-char
  ;;semantic-lex-charquote
  semantic-lex-paren-or-list
  semantic-lex-close-paren
  semantic-lex-ignore-comments
  semantic-lex-punctuation
  semantic-lex-default-action)

(defun semantic-erlang-default-setup ()
  "Set up a buffer for semantic parsing of the Erlang language."
  (semantic-erlang-by--install-parser)
  (setq
   ;; Lexical analysis
   semantic-lex-number-expression nil
   semantic-lex-syntax-modifications '((?' "_")
                                       (?$ "/")
                                       )
   semantic-lex-analyzer 'semantic-erlang-lexer
   ;; Parsing
   ;; Environment
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-type-relation-separator-character '(".")
   semantic-command-separation-character ","
   document-comment-start "%%**"
   document-comment-line-prefix "%%"
   document-comment-end "%%*"
   ;; speedbar and imenu buckets name
   semantic-symbol->name-assoc-list-for-type-parts
   ;; in type parts
   '((variable . "Definitions")
     (type     . "Records")
     (function . "Functions"))
   semantic-symbol->name-assoc-list
   ;; everywhere
   '((variable . "Definitions")
     (type     . "Records")
     (function . "Functions")
     (include  . "Includes")
     (package  . "Module"))
   ))

(add-hook 'erlang-mode-hook 'semantic-erlang-default-setup)

(provide 'semantic-erlang)

;;; semantic-erlang.el ends here
