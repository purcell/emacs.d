;;; wisent-dot.el --- GraphViz DOT parser

;; Copyright (C) 2003, 2004 Eric M. Ludlam

;; Author: Eric Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: wisent-dot.el,v 1.9 2005/09/30 20:07:07 zappo Exp $

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
;; Parser for GraphViz DOT language.
;; The language is declaritive and the whole thing is parsed.
;; The result could be used as a data structure representing a graph.

;; This depends on graphics dot mode by
;;   Pieter E.J. Pareit <pieter.pareit@planetinternet.be>
;;   http://users.skynet.be/ppareit/graphviz-dot-mode.el
;;   with the following patch:
;;
;;
;; *** graphviz-dot-mode.el	2003/03/23 17:14:22	1.1
;; --- graphviz-dot-mode.el	2003/03/26 03:39:21
;; ***************
;; *** 98,103 ****
;; --- 98,109 ----
;;       (modify-syntax-entry ?/ ". 124b" st)
;;       (modify-syntax-entry ?* ". 23" st)
;;       (modify-syntax-entry ?\n "> b" st)
;; +     (modify-syntax-entry ?= "." st)
;; +     (modify-syntax-entry ?, "." st)
;; +     (modify-syntax-entry ?\; "." st)
;; +     (modify-syntax-entry ?- "." st)
;; +     (modify-syntax-entry ?> "." st)
;; +     (modify-syntax-entry ?< "." st)
;;       st)
;;     "Syntax table for `graphviz-dot-mode'.")
;;   


;;; Code:
(require 'semantic-wisent)
(require 'semantic)
(require 'wisent-dot-wy)

(define-mode-overload-implementation semantic-tag-components
  graphviz-dot-mode (tag)
  "Return the children of tag TAG."
  (cond
   ((memq (semantic-tag-class tag)
         '(generic-node graph-attributes node link))
    (semantic-tag-get-attribute tag :attributes)
    )
   ((memq (semantic-tag-class tag)
         '(digraph graph))
    (semantic-tag-get-attribute tag :members)
    )))

;;;###autoload
(defun wisent-dot-setup-parser ()
  "Setup buffer for parse."
  (wisent-dot-wy--install-parser)

  (setq 
   ;; Lexical Analysis
   semantic-lex-analyzer 'wisent-dot-lexer
   ;; Parsing
   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-name
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-command-separation-character ";"
   ;; Speedbar
   semantic-symbol->name-assoc-list
   '((graph . "Graph")
     (digraph . "Directed Graph")
     (node . "Node")
     )
   ;; Navigation
   senator-step-at-tag-classes '(graph digraph)
   ))

;;;###autoload
(add-hook 'graphviz-dot-mode-hook 'wisent-dot-setup-parser)

(provide 'wisent-dot)

;;; wisent-dot.el ends here
