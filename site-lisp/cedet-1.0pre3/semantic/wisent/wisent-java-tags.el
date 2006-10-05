;;; wisent-java-tags.el --- Java LALR parser for Emacs

;; Copyright (C) 2001, 2002 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 15 Dec 2001
;; Keywords: syntax
;; X-RCS: $Id: wisent-java-tags.el,v 1.30 2004/04/29 10:10:53 ponced Exp $

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;

;;; History:
;; 

;;; Code:

(require 'semantic-wisent)
(require 'wisent-java-tags-wy)
(require 'semantic-java)
(eval-when-compile
  (require 'semantic-util)
  (require 'semantic-ctxt)
  (require 'semantic-imenu)
  (require 'senator)
  (require 'document))

;;;;
;;;; Simple parser error reporting function
;;;;

(defun wisent-java-parse-error (msg)
  "Error reporting function called when a parse error occurs.
MSG is the message string to report."
;;   (let ((error-start (nth 2 wisent-input)))
;;     (if (number-or-marker-p error-start)
;;         (goto-char error-start)))
  (message msg)
  ;;(debug)
  )

;;;;
;;;; Local context
;;;;

(define-mode-local-override semantic-get-local-variables
  java-mode ()
  "Get local values from a specific context.
Parse the current context for `field_declaration' nonterminals to
collect tags, such as local variables or prototypes.
This function override `get-local-variables'."
  (let ((vars nil)
        ;; We want nothing to do with funny syntaxing while doing this.
        (semantic-unmatched-syntax-hook nil))
    (while (not (semantic-up-context (point) 'function))
      (save-excursion
        (forward-char 1)
        (setq vars
              (append (semantic-parse-region
                       (point)
                       (save-excursion (semantic-end-of-context) (point))
                       'field_declaration
                       0 t)
                      vars))))
    vars))

;;;;
;;;; Semantic integration of the Java LALR parser
;;;;

;;;###autoload
(defun wisent-java-default-setup ()
  "Hook run to setup Semantic in `java-mode'.
Use the alternate LALR(1) parser."
  (wisent-java-tags-wy--install-parser)
  (setq
   ;; Lexical analysis
   semantic-lex-number-expression semantic-java-number-regexp
   semantic-lex-analyzer 'wisent-java-tags-lexer
   ;; Parsing
   semantic-tag-expand-function 'wisent-java-expand-tag
   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-prototype
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-type-relation-separator-character '(".")
   semantic-command-separation-character ";"
   document-comment-start "/**"
   document-comment-line-prefix " *"
   document-comment-end " */"
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

(defun wisent-java-expand-tag (tag)
  "Expand TAG into a list of equivalent tags, or nil.
Expand multiple variable declarations in the same statement, that is
tags of class `variable' whose name is equal to a list of elements of
the form (NAME START . END).  NAME is a variable name.  START and END
are the bounds in the declaration, related to this variable NAME."
  (let (elts elt clone start end xpand)
    (when (and (eq 'variable (semantic-tag-class tag))
               (consp (setq elts (semantic-tag-name tag))))
      ;; There are multiple names in the same variable declaration.
      (while elts
        ;; For each name element, clone the initial tag and give it
        ;; the name of the element.
        (setq elt   (car elts)
              elts  (cdr elts)
              clone (semantic-tag-clone tag (car elt))
              start (if elts  (cadr elt) (semantic-tag-start tag))
              end   (if xpand (cddr elt) (semantic-tag-end   tag))
              xpand (cons clone xpand))
        ;; Set the bounds of the cloned tag with those of the name
        ;; element.
        (semantic-tag-set-bounds clone start end))
      xpand)))

;;;###autoload
(add-hook 'java-mode-hook #'wisent-java-default-setup)

(provide 'wisent-java-tags)

;;; wisent-java-tags.el ends here
