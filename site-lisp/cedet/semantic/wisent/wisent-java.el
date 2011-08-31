;;; wisent-java.el --- Java LALR parser for Emacs

;; Copyright (C) 2009 Eric M. Ludlam
;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 19 June 2001
;; Keywords: syntax
;; X-RCS: $Id: wisent-java.el,v 1.50 2009/01/24 03:47:19 zappo Exp $

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

;;; History:
;; 

;;; Code:

(require 'semantic-wisent)
(require 'wisent-java-wy)
(require 'semantic-java)
(eval-when-compile
  (require 'semantic-util)
  (require 'semantic-ctxt)
  (require 'semantic-imenu)
  (require 'senator))

;;; Enable Semantic in `java-mode'.
;;
(defun wisent-java-init-parser-context ()
  "Initialize context of the LR parser engine.
Used as a local `wisent-pre-parse-hook' to cleanup the stack of enum
names in scope."
  (setq wisent-java-wy--enums nil))

(defun wisent-java-default-setup ()
  "Hook run to setup Semantic in `java-mode'."
  ;; Use the Wisent LALR(1) parser to analyze Java sources.
  (wisent-java-wy--install-parser)
  (semantic-make-local-hook 'wisent-pre-parse-hook)
  (add-hook 'wisent-pre-parse-hook
            'wisent-java-init-parser-context nil t)
  (setq
   ;; Lexical analysis
   semantic-lex-number-expression semantic-java-number-regexp
   semantic-lex-depth nil
   semantic-lex-analyzer 'wisent-java-lexer
   ;; Parsing
   semantic-tag-expand-function 'semantic-java-expand-tag
   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-prototype
   semantic-imenu-expandable-tag-classes '(type variable)
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-type-relation-separator-character '(".")
   semantic-command-separation-character ";"
   ;; speedbar and imenu buckets name
   semantic-symbol->name-assoc-list-for-type-parts
   ;; in type parts
   '((type     . "Classes")
     (variable . "Variables")
     (function . "Methods"))
   semantic-symbol->name-assoc-list
   ;; everywhere
   (append semantic-symbol->name-assoc-list-for-type-parts
           '((include  . "Imports")
             (package  . "Package")))
   ;; navigation inside 'type children
   senator-step-at-tag-classes '(function variable)
   )
  ;; Setup javadoc stuff
  (semantic-java-doc-setup))

(add-hook 'java-mode-hook 'wisent-java-default-setup)

;;; Overridden Semantic API.
;;
(define-mode-local-override semantic-tag-components java-mode (tag)
  "Return a list of components for TAG."
  (if (semantic-tag-of-class-p tag 'function)
      (semantic-tag-function-arguments tag)
    ;; Simply return the value of the :members attribute.
    (semantic-tag-get-attribute tag :members)))

(define-mode-local-override semantic-get-local-variables
  java-mode ()
  "Get local variable declarations from the current context."
  (let (result
        ;; Ignore funny syntax while doing this.
        semantic-unmatched-syntax-hook)
    (while (not (semantic-up-context (point) 'function))
      (save-excursion
        (forward-char 1)
        (push (semantic-parse-region
               (point)
               (save-excursion (semantic-end-of-context) (point))
               ;; See this production in wisent-java.wy.
               'block_statement
               nil t)
              result)))
    (apply 'append result)))

(provide 'wisent-java)

;;; wisent-java.el ends here
