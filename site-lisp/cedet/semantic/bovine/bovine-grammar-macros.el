;;; bovine-grammar-macros.el --- Semantic macros for LL grammars
;;
;; Copyright (C) 2003, 2005 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 02 Aug 2003
;; Keywords: syntax
;; X-RCS: $Id: bovine-grammar-macros.el,v 1.4 2005/09/30 20:21:46 zappo Exp $
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
;; used in bovine (.by) grammars.

;;; History:
;;

;;; Code:

(defun bovine-grammar-EXPAND (bounds nonterm)
  "Expand call to EXPAND grammar macro.
Return the form to parse from within a nonterminal between BOUNDS.
NONTERM is the nonterminal symbol to start with."
  `(semantic-bovinate-from-nonterminal
    (car ,bounds) (cdr ,bounds) ',nonterm))

(defun bovine-grammar-EXPANDFULL (bounds nonterm)
  "Expand call to EXPANDFULL grammar macro.
Return the form to recursively parse the area between BOUNDS.
NONTERM is the nonterminal symbol to start with."
  `(semantic-parse-region
    (car ,bounds) (cdr ,bounds) ',nonterm 1))

(defun bovine-grammar-TAG (name class &rest attributes)
  "Expand call to TAG grammar macro.
Return the form to create a generic semantic tag.
See the function `semantic-tag' for the meaning of arguments NAME,
CLASS and ATTRIBUTES."
  `(semantic-tag ,name ,class ,@attributes))

(defun bovine-grammar-VARIABLE-TAG (name type default-value &rest attributes)
  "Expand call to VARIABLE-TAG grammar macro.
Return the form to create a semantic tag of class variable.
See the function `semantic-tag-new-variable' for the meaning of
arguments NAME, TYPE, DEFAULT-VALUE and ATTRIBUTES."
  `(semantic-tag-new-variable ,name ,type ,default-value ,@attributes))

(defun bovine-grammar-FUNCTION-TAG (name type arg-list &rest attributes)
  "Expand call to FUNCTION-TAG grammar macro.
Return the form to create a semantic tag of class function.
See the function `semantic-tag-new-function' for the meaning of
arguments NAME, TYPE, ARG-LIST and ATTRIBUTES."
  `(semantic-tag-new-function ,name ,type ,arg-list ,@attributes))

(defun bovine-grammar-TYPE-TAG (name type members parents &rest attributes)
  "Expand call to TYPE-TAG grammar macro.
Return the form to create a semantic tag of class type.
See the function `semantic-tag-new-type' for the meaning of arguments
NAME, TYPE, MEMBERS, PARENTS and ATTRIBUTES."
  `(semantic-tag-new-type ,name ,type ,members ,parents ,@attributes))

(defun bovine-grammar-INCLUDE-TAG (name system-flag &rest attributes)
  "Expand call to INCLUDE-TAG grammar macro.
Return the form to create a semantic tag of class include.
See the function `semantic-tag-new-include' for the meaning of
arguments NAME, SYSTEM-FLAG and ATTRIBUTES."
  `(semantic-tag-new-include ,name ,system-flag ,@attributes))

(defun bovine-grammar-PACKAGE-TAG (name detail &rest attributes)
  "Expand call to PACKAGE-TAG grammar macro.
Return the form to create a semantic tag of class package.
See the function `semantic-tag-new-package' for the meaning of
arguments NAME, DETAIL and ATTRIBUTES."
  `(semantic-tag-new-package ,name ,detail ,@attributes))

(defun bovine-grammar-CODE-TAG (name detail &rest attributes)
  "Expand call to CODE-TAG grammar macro.
Return the form to create a semantic tag of class code.
See the function `semantic-tag-new-code' for the meaning of arguments
NAME, DETAIL and ATTRIBUTES."
  `(semantic-tag-new-code ,name ,detail ,@attributes))

(defun bovine-grammar-ALIAS-TAG (name aliasclass definition &rest attributes)
  "Expand call to ALIAS-TAG grammar macro.
Return the form to create a semantic tag of class alias.
See the function `semantic-tag-new-alias' for the meaning of arguments
NAME, ALIASCLASS, DEFINITION and ATTRIBUTES."
  `(semantic-tag-new-alias ,name ,aliasclass ,definition ,@attributes))

(defvar-mode-local bovine-grammar-mode semantic-grammar-macros
  '(
    (ASSOC          . semantic-grammar-ASSOC)
    (EXPAND         . bovine-grammar-EXPAND)
    (EXPANDFULL     . bovine-grammar-EXPANDFULL)
    (TAG            . bovine-grammar-TAG)
    (VARIABLE-TAG   . bovine-grammar-VARIABLE-TAG)
    (FUNCTION-TAG   . bovine-grammar-FUNCTION-TAG)
    (TYPE-TAG       . bovine-grammar-TYPE-TAG)
    (INCLUDE-TAG    . bovine-grammar-INCLUDE-TAG)
    (PACKAGE-TAG    . bovine-grammar-PACKAGE-TAG)
    (CODE-TAG       . bovine-grammar-CODE-TAG)
    (ALIAS-TAG      . bovine-grammar-ALIAS-TAG)
    )
  "Semantic grammar macros used in bovine grammars.")

(provide 'bovine-grammar-macros)

;;; bovine-grammar-macros.el ends here
