;; NOTE TO USERS OF THIS SKELETON:
;;
;; You may want to use the wisent parser generator instead.  It
;; will generate more efficient parsers.


;;; semantic-skel.el --- Semantic details for skel

;;; Copyright (C) 2001, 2003, 2004, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-skel.el,v 1.8 2009/01/24 03:53:52 zappo Exp $

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;

;;; History:
;; 

(require 'semantic)
(require 'semantic-skeleton-by)
(require 'backquote)

;; Depending on what elements you include specialized support for
(eval-when-compile
  (require 'semantic-ctxt)
  (require 'semantic-imenu)
  (require 'senator))

;;; Code:

;; Create a lexical analyzer for your language.  You can use
;; both the provided analyzers, and your own custom analyzers
;; that let you take short-cuts in your language.

;; One analyzer
(define-lex-regex-analyzer semantic-lex-skel-if-0
  "Block out code matched in an #if 0 condition."
  "^\\s-*#if\\s-*0$"
  (beginning-of-line)
  (c-forward-conditional 1)
  (setq semantic-lex-end-point (point))
  nil)

;; Define the lexial analyzer
(define-lex semantic-skeleton-lexer
  "Lexical Analyzer for SKELETON code."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-skel-if-0
  semantic-lex-number
  semantic-lex-symbol-or-keyword
  semantic-lex-charquote
  semantic-lex-paren-or-list
  semantic-lex-close-paren
  semantic-lex-ignore-comments
  semantic-lex-punctuation
  semantic-lex-default-action)

;; You do not need to use this function unless you have compound
;; definitions.  For example, in C, the following is expanded:
;;  int x, y;
(defun semantic-skeleton-expand-tag (nonterm)
  "Expand NONTERM into a list of equivalent nonterminals, or nil."
  nil)

;;; Override methods & Variables
;;
(defcustom-mode-local-semantic-dependency-system-include-path
  skel-mode semantic-makefile-dependency-system-include-path
  nil
  "The system include path used by skel langauge.")

;; Add methods to the override table here.  See
;; `semantic-install-function-overrides' for more details.


;;; Setup function
;;
;;;###autoload
(defun semantic-default-skel-setup ()
  "Set up a buffer for semantic parsing of the skeleton language."
  (semantic-skeleton-by--install-parser)
  ;; Commented out lines below are generally considered optional
  ;; See the Emacs Doc for the symbols used below
  (setq semantic-symbol->name-assoc-list '( (variable . "Variables")
                                            (type     . "Types")
                                            (function . "Functions")
                                            (include  . "Includes")
                                            (package  . "Exports"))
        ;;semantic-tag-expand-function 'semantic-skeleton-expand-tag
        ;;semantic-lex-extensions semantic-lex-skeleton-extensions
        ;;semantic-dependency-include-path semantic-default-skeleton-path
        imenu-create-index-function 'semantic-create-imenu-index
        semantic-type-relation-separator-character '(".")
        semantic-command-separation-character ";"
        ;; Semantic navigation inside 'type children
        senator-step-at-tag-classes '(function variable)
        )
  )

;; Loading this file will install the parser.  Add this line
;; to a .emacs file, or other setup file along with an autoload
;; for the setup function to dynamically install the parser
;; when a file of that type is read into Emacs.
(add-hook 'skeleton-mode-hook 'semantic-default-skeleton-setup)

(provide 'semantic-skel)

;;; semantic-skel.el ends here
