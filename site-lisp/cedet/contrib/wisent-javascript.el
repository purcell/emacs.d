;;; wisent-javascript.el --- javascript parser support

;; Copyright (C) 2005 Eric M. Ludlam

;; Author: Eric Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: wisent-javascript.el,v 1.2 2005/09/30 20:16:07 zappo Exp $

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
;; Parser support for javascript language.


;;; Code:
(require 'semantic-java)
(require 'semantic-wisent)
(require 'wisent-javascript-jv-wy)

(defun wisent-javascript-jv-expand-tag (tag)
  "Expand TAG into a list of equivalent tags, or nil.
Expand multiple variable declarations in the same statement, that is
tags of class `variable' whose name is equal to a list of elements of
the form (NAME VALUE START . END).  NAME is a variable name.  VALUE is
an initializer START and END are the bounds in the declaration, related
to this variable NAME."
  (let (elts elt value clone start end xpand)
    (when (and (eq 'variable (semantic-tag-class tag))
               (consp (setq elts (semantic-tag-name tag))))
      ;; There are multiple names in the same variable declaration.
      (while elts
        ;; For each name element, clone the initial tag and give it
        ;; the name of the element.
        (setq elt   (car elts)
              elts  (cdr elts)
              clone (semantic-tag-clone tag (car elt))
	      value (car (cdr elt))
              start (if elts  (caddr elt) (semantic-tag-start tag))
              end   (if xpand (cdddr elt) (semantic-tag-end   tag))
              xpand (cons clone xpand))
	;; Set the definition of the cloned tag
	(semantic-tag-put-attribute clone :default-value value)
        ;; Set the bounds of the cloned tag with those of the name
        ;; element.
        (semantic-tag-set-bounds clone start end))
      xpand)))

;;; Override Methods
;;
;; These methods override aspects of how semantic-tools can access
;; the tags created by the javascript parser.
;; Local context
(define-mode-overload-implementation semantic-get-local-variables
  javascript-mode ()
  "Get local values from a specific context.
This function overrides `get-local-variables'."
  ;; Does javascript have identifiable local variables?
  nil)


;;; Setup Function
;;
;; This sets up the javascript parser

;;;###autoload
(defun wisent-javascript-setup-parser ()
  "Setup buffer for parse."
  (wisent-javascript-jv-wy--install-parser)
  (setq 
   ;; Lexical Analysis
   semantic-lex-analyzer 'javascript-lexer-jv
   semantic-lex-number-expression semantic-java-number-regexp
   ;; semantic-lex-depth nil ;; Full lexical analysis
   ;; Parsing
   semantic-tag-expand-function 'wisent-javascript-jv-expand-tag
   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-name
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-command-separation-character ";"
   ))

;;;###autoload
(add-hook 'javascript-mode-hook 'wisent-javascript-setup-parser)
;;;###autoload
(add-hook 'ecmascript-mode-hook 'wisent-javascript-setup-parser)

(provide 'wisent-javascript-jv)

;;; wisent-javascript-jv.el ends here
