;;; wisent-grammar-macros.el --- Semantic macros for LALR grammars
;;
;; Copyright (C) 2003 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 02 Aug 2003
;; Keywords: syntax
;; X-RCS: $Id: wisent-grammar-macros.el,v 1.6 2006/05/31 12:46:17 ponced Exp $
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library defines the default set of Semantic grammar macros
;; used in wisent (.wy) grammars.

;;; History:
;;

;;; Code:

(defsubst wisent-grammar-region-placeholder (symb)
  "Given a $N placeholder symbol in SYMB, return a $regionN symbol.
Return nil if $N is not a valid placeholder symbol."
  (let ((n (symbol-name symb)))
    (if (string-match "^[$]\\([1-9][0-9]*\\)$" n)
        (intern (concat "$region" (match-string 1 n))))))

(defun wisent-grammar-EXPAND (symb nonterm)
  "Expand call to EXPAND grammar macro.
Return the form to parse from within a nonterminal.
SYMB is a $I placeholder symbol that gives the bounds of the area to
parse.
NONTERM is the nonterminal symbol to start with."
  (unless (member nonterm (semantic-grammar-start))
    (error "EXPANDFULL macro called with %s, but not used with %%start"
           nonterm))
  (let (($ri (wisent-grammar-region-placeholder symb)))
    (if $ri
        `(semantic-bovinate-from-nonterminal
          (car ,$ri) (cdr ,$ri) ',nonterm)
      (error "Invalid form (EXPAND %s %s)" symb nonterm))))

(defun wisent-grammar-EXPANDFULL (symb nonterm)
  "Expand call to EXPANDFULL grammar macro.
Return the form to recursively parse an area.
SYMB is a $I placeholder symbol that gives the bounds of the area.
NONTERM is the nonterminal symbol to start with."
  (unless (member nonterm (semantic-grammar-start))
    (error "EXPANDFULL macro called with %s, but not used with %%start"
           nonterm))
  (let (($ri (wisent-grammar-region-placeholder symb)))
    (if $ri
        `(semantic-parse-region
          (car ,$ri) (cdr ,$ri) ',nonterm 1)
      (error "Invalid form (EXPANDFULL %s %s)" symb nonterm))))

(defun wisent-grammar-TAG (name class &rest attributes)
  "Expand call to TAG grammar macro.
Return the form to create a generic semantic tag.
See the function `semantic-tag' for the meaning of arguments NAME,
CLASS and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag ,name ,class ,@attributes)))

(defun wisent-grammar-VARIABLE-TAG (name type default-value &rest attributes)
  "Expand call to VARIABLE-TAG grammar macro.
Return the form to create a semantic tag of class variable.
See the function `semantic-tag-new-variable' for the meaning of
arguments NAME, TYPE, DEFAULT-VALUE and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag-new-variable ,name ,type ,default-value ,@attributes)))

(defun wisent-grammar-FUNCTION-TAG (name type arg-list &rest attributes)
  "Expand call to FUNCTION-TAG grammar macro.
Return the form to create a semantic tag of class function.
See the function `semantic-tag-new-function' for the meaning of
arguments NAME, TYPE, ARG-LIST and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag-new-function ,name ,type ,arg-list ,@attributes)))

(defun wisent-grammar-TYPE-TAG (name type members parents &rest attributes)
  "Expand call to TYPE-TAG grammar macro.
Return the form to create a semantic tag of class type.
See the function `semantic-tag-new-type' for the meaning of arguments
NAME, TYPE, MEMBERS, PARENTS and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag-new-type ,name ,type ,members ,parents ,@attributes)))

(defun wisent-grammar-INCLUDE-TAG (name system-flag &rest attributes)
  "Expand call to INCLUDE-TAG grammar macro.
Return the form to create a semantic tag of class include.
See the function `semantic-tag-new-include' for the meaning of
arguments NAME, SYSTEM-FLAG and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag-new-include ,name ,system-flag ,@attributes)))

(defun wisent-grammar-PACKAGE-TAG (name detail &rest attributes)
  "Expand call to PACKAGE-TAG grammar macro.
Return the form to create a semantic tag of class package.
See the function `semantic-tag-new-package' for the meaning of
arguments NAME, DETAIL and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag-new-package ,name ,detail ,@attributes)))

(defun wisent-grammar-CODE-TAG (name detail &rest attributes)
  "Expand call to CODE-TAG grammar macro.
Return the form to create a semantic tag of class code.
See the function `semantic-tag-new-code' for the meaning of arguments
NAME, DETAIL and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag-new-code ,name ,detail ,@attributes)))

(defun wisent-grammar-ALIAS-TAG (name aliasclass definition &rest attributes)
  "Expand call to ALIAS-TAG grammar macro.
Return the form to create a semantic tag of class alias.
See the function `semantic-tag-new-alias' for the meaning of arguments
NAME, ALIASCLASS, DEFINITION and ATTRIBUTES."
  `(wisent-raw-tag
    (semantic-tag-new-alias ,name ,aliasclass ,definition ,@attributes)))

(defun wisent-grammar-EXPANDTAG (raw-tag)
  "Expand call to EXPANDTAG grammar macro.
Return the form to produce a list of cooked tags from raw form of
Semantic tag RAW-TAG."
  `(wisent-cook-tag ,raw-tag))

(defun wisent-grammar-AST-ADD (ast &rest nodes)
  "Expand call to AST-ADD grammar macro.
Return the form to update the abstract syntax tree AST with NODES.
See also the function `semantic-ast-add'."
  `(semantic-ast-add ,ast ,@nodes))

(defun wisent-grammar-AST-PUT (ast &rest nodes)
  "Expand call to AST-PUT grammar macro.
Return the form to update the abstract syntax tree AST with NODES.
See also the function `semantic-ast-put'."
  `(semantic-ast-put ,ast ,@nodes))

(defun wisent-grammar-AST-GET (ast node)
  "Expand call to AST-GET grammar macro.
Return the form to get, from the abstract syntax tree AST, the value
of NODE.
See also the function `semantic-ast-get'."
  `(semantic-ast-get ,ast ,node))

(defun wisent-grammar-AST-GET1 (ast node)
  "Expand call to AST-GET1 grammar macro.
Return the form to get, from the abstract syntax tree AST, the first
value of NODE.
See also the function `semantic-ast-get1'."
  `(semantic-ast-get1 ,ast ,node))

(defun wisent-grammar-AST-GET-STRING (ast node)
  "Expand call to AST-GET-STRING grammar macro.
Return the form to get, from the abstract syntax tree AST, the value
of NODE as a string.
See also the function `semantic-ast-get-string'."
  `(semantic-ast-get-string ,ast ,node))

(defun wisent-grammar-AST-MERGE (ast1 ast2)
  "Expand call to AST-MERGE grammar macro.
Return the form to merge the abstract syntax trees AST1 and AST2.
See also the function `semantic-ast-merge'."
  `(semantic-ast-merge ,ast1 ,ast2))

(defun wisent-grammar-SKIP-BLOCK (&optional symb)
  "Expand call to SKIP-BLOCK grammar macro.
Return the form to skip a parenthesized block.
Optional argument SYMB is a $I placeholder symbol that gives the
bounds of the block to skip.  By default, skip the block at `$1'.
See also the function `wisent-skip-block'."
  (let ($ri)
    (when symb
      (unless (setq $ri (wisent-grammar-region-placeholder symb))
        (error "Invalid form (SKIP-BLOCK %s)" symb)))
    `(wisent-skip-block ,$ri)))

(defun wisent-grammar-SKIP-TOKEN ()
  "Expand call to SKIP-TOKEN grammar macro.
Return the form to skip the lookahead token.
See also the function `wisent-skip-token'."
  `(wisent-skip-token))

(defvar-mode-local wisent-grammar-mode semantic-grammar-macros
  '(
    (ASSOC          . semantic-grammar-ASSOC)
    (EXPAND         . wisent-grammar-EXPAND)
    (EXPANDFULL     . wisent-grammar-EXPANDFULL)
    (TAG            . wisent-grammar-TAG)
    (VARIABLE-TAG   . wisent-grammar-VARIABLE-TAG)
    (FUNCTION-TAG   . wisent-grammar-FUNCTION-TAG)
    (TYPE-TAG       . wisent-grammar-TYPE-TAG)
    (INCLUDE-TAG    . wisent-grammar-INCLUDE-TAG)
    (PACKAGE-TAG    . wisent-grammar-PACKAGE-TAG)
    (EXPANDTAG      . wisent-grammar-EXPANDTAG)
    (CODE-TAG       . wisent-grammar-CODE-TAG)
    (ALIAS-TAG      . wisent-grammar-ALIAS-TAG)
    (AST-ADD        . wisent-grammar-AST-ADD)
    (AST-PUT        . wisent-grammar-AST-PUT)
    (AST-GET        . wisent-grammar-AST-GET)
    (AST-GET1       . wisent-grammar-AST-GET1)
    (AST-GET-STRING . wisent-grammar-AST-GET-STRING)
    (AST-MERGE      . wisent-grammar-AST-MERGE)
    (SKIP-BLOCK     . wisent-grammar-SKIP-BLOCK)
    (SKIP-TOKEN     . wisent-grammar-SKIP-TOKEN)
    )
  "Semantic grammar macros used in wisent grammars.")

(provide 'wisent-grammar-macros)

;;; wisent-grammar-macros.el ends here
