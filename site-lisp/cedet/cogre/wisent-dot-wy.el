;;; wisent-dot-wy.el --- Generated parser support file

;; Copyright (C) 2003, 2004 Eric M. Ludlam

;; Author: Mona <whisky@ubuntu.ubuntu-domain>
;; Created: 2011-05-20 16:21:08-0400
;; Keywords: syntax
;; X-RCS: $Id$

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
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file wisent-dot.wy.

;;; History:
;;

;;; Code:

;;; Prologue
;;

;;; Declarations
;;
(defconst wisent-dot-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("digraph" . DIGRAPH)
     ("graph" . GRAPH)
     ("subgraph" . SUBGRAPH)
     ("node" . NODE)
     ("shape" . SHAPE)
     ("label" . LABEL)
     ("color" . COLOR)
     ("style" . STYLE)
     ("len" . LEN)
     ("fontname" . FONTNAME)
     ("fontsize" . FONTSIZE)
     ("width" . WIDTH)
     ("height" . HEIGHT)
     ("splines" . SPLINES)
     ("overlap" . OVERLAP))
   '(("fontsize" summary "fontsize=<font-size-number>")
     ("fontname" summary "fontname=<font-spec>")
     ("len" summary "len=<value>")
     ("style" summary "style=<style-spec>")
     ("color" summary "color=<color-spec>")
     ("label" summary "label=\"string\"")
     ("shape" summary "shape=<shape-type>")
     ("node" summary "node [<attribute>...];")
     ("subgraph" summary "subgraph <name> { <graph elements> ... }")
     ("graph" summary "graph <name> { <graph elements> ... }")
     ("digraph" summary "digraph <name> { <graph elements> ... }")))
  "Table of language keywords.")

(defconst wisent-dot-wy--token-table
  (semantic-lex-make-type-table
   '(("number"
      (number))
     ("string"
      (string))
     ("symbol"
      (symbol))
     ("close-paren"
      (RPAREN . ")")
      (RBRACKET . "]")
      (RBRACE . "}"))
     ("open-paren"
      (LPAREN . "(")
      (LBRACKET . "[")
      (LBRACE . "{"))
     ("block"
      (PAREN_BLOCK . "(LPAREN RPAREN)")
      (BRACE_BLOCK . "(LBRACE RBRACE)")
      (BRACKET_BLOCK . "(LBRACKET RBRACKET)"))
     ("punctuation"
      (COMMA . ",")
      (SEMI . ";")
      (EQUAL . "=")
      (LINK . "--")
      (DILINK . "->")))
   '(("number" :declared t)
     ("string" :declared t)
     ("symbol" :declared t)
     ("block" :declared t)
     ("punctuation" syntax "\\s.+")
     ("punctuation" :declared t)
     ("keyword" :declared t)))
  "Table of lexical tokens.")

(defconst wisent-dot-wy--parse-table
  (progn
    (eval-when-compile
      (require 'wisent-comp))
    (wisent-compile-grammar
     '((DIGRAPH GRAPH SUBGRAPH NODE SHAPE LABEL COLOR STYLE LEN FONTNAME FONTSIZE WIDTH HEIGHT SPLINES OVERLAP DILINK LINK EQUAL SEMI COMMA BRACKET_BLOCK BRACE_BLOCK PAREN_BLOCK LBRACE RBRACE LBRACKET RBRACKET LPAREN RPAREN symbol string number)
       nil
       (dot_file
	((digraph))
	((graph)))
       (digraph
	((DIGRAPH symbol BRACE_BLOCK)
	 (wisent-raw-tag
	  (semantic-tag $2 'digraph :members
			(semantic-parse-region
			 (car $region3)
			 (cdr $region3)
			 'graph-contents 1)))))
       (graph
	((GRAPH symbol BRACE_BLOCK)
	 (wisent-raw-tag
	  (semantic-tag $2 'graph :members
			(semantic-parse-region
			 (car $region3)
			 (cdr $region3)
			 'graph-contents 1)))))
       (graph-contents
	((LBRACE)
	 nil)
	((RBRACE)
	 nil)
	((label))
	((style))
	((graph-attributes))
	((subgraph))
	((node))
	((named-node))
	((links)))
       (label
	((LABEL EQUAL string SEMI)
	 (wisent-raw-tag
	  (semantic-tag $3 'label))))
       (style
	((STYLE EQUAL symbol SEMI)
	 (wisent-raw-tag
	  (semantic-tag $3 'style))))
       (subgraph
	((SUBGRAPH symbol BRACE_BLOCK)
	 (wisent-raw-tag
	  (semantic-tag $2 'graph :members
			(semantic-parse-region
			 (car $region3)
			 (cdr $region3)
			 'graph-contents 1)))))
       (node
	((NODE BRACKET_BLOCK SEMI)
	 (wisent-raw-tag
	  (semantic-tag "NODE" 'generic-node :attributes
			(semantic-parse-region
			 (car $region2)
			 (cdr $region2)
			 'node-description 1)))))
       (graph-attributes
	((GRAPH BRACKET_BLOCK SEMI)
	 (wisent-raw-tag
	  (semantic-tag "GRAPH" 'graph-attributes :attributes
			(semantic-parse-region
			 (car $region2)
			 (cdr $region2)
			 'node-description 1)))))
       (named-node
	((symbol BRACKET_BLOCK SEMI)
	 (wisent-raw-tag
	  (semantic-tag $1 'node :attributes
			(semantic-parse-region
			 (car $region2)
			 (cdr $region2)
			 'node-description 1)))))
       (node-description
	((LBRACKET)
	 nil)
	((RBRACKET)
	 nil)
	((COMMA)
	 nil)
	((SHAPE EQUAL symbol)
	 (wisent-raw-tag
	  (semantic-tag $1 'attribute :value $3)))
	((LABEL EQUAL string)
	 (wisent-raw-tag
	  (semantic-tag $1 'attribute :value $3)))
	((FONTNAME EQUAL string)
	 (wisent-raw-tag
	  (semantic-tag $1 'attribute :value $3)))
	((FONTSIZE EQUAL number)
	 (wisent-raw-tag
	  (semantic-tag $1 'attribute :value $3)))
	((symbol EQUAL symbol)
	 (wisent-raw-tag
	  (semantic-tag $1 'attribute :value $3))))
       (links
	((symbol DILINK symbol opt-link-attributes opt-semi)
	 (wisent-raw-tag
	  (semantic-tag $1 'link :to $3 :attributes $4)))
	((BRACE_BLOCK)))
       (opt-semi
	((SEMI)
	 nil)
	(nil))
       (opt-link-attributes
	((BRACKET_BLOCK)
	 (semantic-parse-region
	  (car $region1)
	  (cdr $region1)
	  'node-description 1))
	(nil)))
     '(dot_file graph-contents node-description)))
  "Parser table.")

(defun wisent-dot-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
	semantic--parse-table wisent-dot-wy--parse-table
	semantic-debug-parser-source "wisent-dot.wy"
	semantic-flex-keywords-obarray wisent-dot-wy--keyword-table
	semantic-lex-types-obarray wisent-dot-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
	    'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(require 'semantic-lex)

(define-lex-keyword-type-analyzer wisent-dot-wy--<keyword>-keyword-analyzer
  "keyword analyzer for <keyword> tokens."
  "\\(\\sw\\|\\s_\\)+")

(define-lex-block-type-analyzer wisent-dot-wy--<block>-block-analyzer
  "block analyzer for <block> tokens."
  "\\s(\\|\\s)"
  '((("[" LBRACKET BRACKET_BLOCK)
     ("{" LBRACE BRACE_BLOCK)
     ("(" LPAREN PAREN_BLOCK))
    ("]" RBRACKET)
    ("}" RBRACE)
    (")" RPAREN))
  )

(define-lex-regex-type-analyzer wisent-dot-wy--<symbol>-regexp-analyzer
  "regexp analyzer for <symbol> tokens."
  "\\(\\sw\\|\\s_\\)+"
  nil
  'symbol)

(define-lex-sexp-type-analyzer wisent-dot-wy--<string>-sexp-analyzer
  "sexp analyzer for <string> tokens."
  "\\s\""
  'string)

(define-lex-regex-type-analyzer wisent-dot-wy--<number>-regexp-analyzer
  "regexp analyzer for <number> tokens."
  semantic-lex-number-expression
  nil
  'number)

(define-lex-string-type-analyzer wisent-dot-wy--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\s.+"
  '((COMMA . ",")
    (SEMI . ";")
    (EQUAL . "=")
    (LINK . "--")
    (DILINK . "->"))
  'punctuation)


;;; Epilogue
;;
(define-lex wisent-dot-lexer
  "Lexical analyzer that handles DOT buffers.
It ignores whitespace, newlines and comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments
  wisent-dot-wy--<keyword>-keyword-analyzer
  wisent-dot-wy--<symbol>-regexp-analyzer
  wisent-dot-wy--<block>-block-analyzer
  ;; ?? semantic-lex-close-paren
  wisent-dot-wy--<number>-regexp-analyzer
  wisent-dot-wy--<string>-sexp-analyzer
  wisent-dot-wy--<punctuation>-string-analyzer
  semantic-lex-default-action
  )

(provide 'wisent-dot-wy)

;;; wisent-dot-wy.el ends here
