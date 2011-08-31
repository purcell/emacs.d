;;; semantic-ast.el --- Simple Abstract Syntax Trees

;; Copyright (C) 2003 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 25 June 2003
;; Keywords: syntax
;; X-RCS: $Id: semantic-ast.el,v 1.2 2005/09/30 20:18:47 zappo Exp $

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
;; This is a simple implementation of Abstract Syntax Trees based on
;; property lists.  Each AST node is defined by a property and its
;; value.
;;

;;; History:
;;

;;; Code:
(defsubst semantic-ast-get (ast node)
  "From the abstract syntax tree AST, return NODE value.
A node value is always a list or nil."
  (plist-get ast node))

(defsubst semantic-ast-get1 (ast node)
  "From the abstract syntax tree AST, return NODE first value.
A node value is always a list or nil."
  (car (semantic-ast-get ast node)))

(defsubst semantic-ast-get-string (ast node)
  "From the abstract syntax tree AST, return NODE value as a string.
Return concatenation of the strings in NODE value separated by a
space."
  (mapconcat #'(lambda (o) (format "%s" o))
             (semantic-ast-get ast node)
             " "))

(defun semantic-ast-add (ast &rest nodes)
  "Update the abstract syntax tree AST with NODES.
NODES must be a sequence of NODE VALUE ..., where NODE is a symbol
that identifies the node, and VALUE is a Lisp object.
Add to the current value of NODE the associated new VALUE.
A node value is always a list or nil.
Return the updated abstract syntax tree."
  (let (node value)
    (while nodes
      (setq node  (car nodes)
            nodes (cdr nodes)
            value (car nodes)
            nodes (cdr nodes)
            ast (plist-put ast node
                           (cons value (plist-get ast node)))))
    ast))

(defun semantic-ast-put (ast &rest nodes)
  "Update the abstract syntax tree AST with NODES.
NODES must be a sequence of NODE VALUE ..., where NODE is a symbol
that identifies a node, and VALUE is a Lisp object.
Replace the current value of each NODE with the associated new VALUE.
A node value is always a list or nil.
Return the updated abstract syntax tree."
  (let (node value)
    (while nodes
      (setq node  (car nodes)
            nodes (cdr nodes)
            value (car nodes)
            nodes (cdr nodes)
            ast (plist-put ast node (list value))))
    ast))

(defun semantic-ast-merge (ast1 ast2)
  "Merge the abstract syntax trees AST1 and AST2.
Return the new merged abstract syntax tree."
  (let ((ast (copy-sequence ast1))
        node)
    (while ast2
      (setq node (car ast2)
            ast2 (cdr ast2)
            ast  (plist-put ast node
                            (append (plist-get ast node) (car ast2)))
            ast2 (cdr ast2)))
    ast))

(provide 'semantic-ast)

;;; semantic-ast.el ends here
