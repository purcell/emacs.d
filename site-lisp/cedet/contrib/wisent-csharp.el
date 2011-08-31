;;; wisent-csharp.el --- LALR grammar for C#
;;
;; Copyright (C) 2003, 2007 David Shilvock
;; Some Changes Copyright (C) 2006 Eric M. Ludlam

;; Time-stamp: <2003-12-08 19:11:48 dave>
;;
;; Author: David Shilvock <davels@telus.net>
;; Maintainer: David Shilvock <davels@telus.net>
;; Created: November 2003
;; Keywords: syntax
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
;; This file contains the csharp parser created from the grammar
;; specified in wisent-csharp.wy file.  It also has some support code.
;; A bunch of this is ripped from wisent-java-tags.el (David Ponce)
;;
;;; Code:

(require 'semantic-wisent)
(require 'semantic-format)
(require 'semantic-ctxt)
(require 'wisent-csharp-wy)


;;;----------------------------------------------------------------------
;;; * Lexer/Parser Support Code
;;;----------------------------------------------------------------------

(defconst wysent-csharp-number-re
  (eval-when-compile
    (concat "\\("
            "\\<[0-9]+[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[.][eE][-+]?[0-9]+[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[.][fFdD]\\>"
            "\\|"
            "\\<[0-9]+[.]"
            "\\|"
            "[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[eE][-+]?[0-9]+[fFdD]?\\>"
            "\\|"
            "\\<0[xX][0-9a-fA-F]+[lL]?\\>"
            "\\|"
            "\\<[0-9]+[lLfFdD]?\\>"
            "\\)"
            ))
  "Lexer regexp to match Java number terminals.
Following is the specification of Java number literals.

DECIMAL_LITERAL:
    [1-9][0-9]*
  ;
HEX_LITERAL:
    0[xX][0-9a-fA-F]+
  ;
OCTAL_LITERAL:
    0[0-7]*
  ;
INTEGER_LITERAL:
    <DECIMAL_LITERAL>[lL]?
  | <HEX_LITERAL>[lL]?
  | <OCTAL_LITERAL>[lL]?
  ;
EXPONENT:
    [eE][+-]?[09]+
  ;
FLOATING_POINT_LITERAL:
    [0-9]+[.][0-9]*<EXPONENT>?[fFdD]?
  | [.][0-9]+<EXPONENT>?[fFdD]?
  | [0-9]+<EXPONENT>[fFdD]?
  | [0-9]+<EXPONENT>?[fFdD]
  ;")

(defconst wisent-csharp-string-re "@?['\"]"
  "Regexp matching beginning of a csharp string.")
  
(defun wisent-csharp-expand-tag (tag)
  "Expand TAG into a list of equivalent tags, or nil.
Expand multiple variable declarations in the same statement, that is
tags of class `variable' whose name is equal to a list of elements of
the form (NAME DEFAULT START . END).  NAME is a variable name.  DEFAULT is the
variable default value.  START and END are the bounds in the declaration,
related to this variable NAME."
  (let (elts elt clone def start end xpand)
    (when (and (eq 'variable (semantic-tag-class tag))
               (consp (setq elts (semantic-tag-name tag))))
      ;; There are multiple names in the same variable declaration.
      (while elts
        ;; For each name element, clone the initial tag and give it
        ;; the name of the element.
        (setq elt   (car elts)
              elts  (cdr elts)
              clone (semantic-tag-clone tag (car elt))
              def   (if elt  (nth 1 elt)  nil)
              start (if elts  (car (nth 2 elt)) (semantic-tag-start tag))
              end   (if xpand (cdr (nth 2 elt)) (semantic-tag-end   tag))
              xpand (cons clone xpand))
        ;; Set the bounds of the cloned tag with those of the name
        ;; element.
        (semantic-tag-set-bounds clone start end)
        ;; i don't think this is doing the right thing
        (if def
            (semantic-tag-put-attribute clone :default-value def))
        )
      xpand)))

;;;----------------------------------------------------------------------
;;; * Semantic Support Code
;;;----------------------------------------------------------------------

;; types - special formatting for ref/out parameters
(define-mode-overload-implementation semantic-format-tag-type
  csharp-mode (tag color)
  ""
  (let ((text (semantic-format-tag-type-default tag color))
        (mods (semantic-tag-get-attribute tag 'typemodifiers)))
    (concat text (or (and (member "ref" mods) "&")
                     (and (member "out" mods) "*")))))

;; special formatting for certain csharp tags
(define-mode-overload-implementation semantic-format-tag-uml-prototype
  csharp-mode (tag &optional parent color)
  ""
  (let ((property-p (semantic-tag-get-attribute tag 'property))
        (accessors (semantic-tag-get-attribute tag 'accessors)))
    (cond
     ;; properties: protection propname {get,set}: type
     ((and property-p accessors)
      (let ((name (semantic-format-tag-name tag parent color))
            (type (semantic--format-tag-uml-type tag color))
            (prot (semantic-format-tag-uml-protection tag parent color))
            (accesstext nil)
            (text nil))
        (setq accesstext
              (concat " {"
                      (mapconcat
                       #'(lambda (tag)
                           (semantic--format-colorize-text
                            (semantic-tag-name tag) 'function))
                       accessors ",")
                      "}"))
        (setq text (concat prot name accesstext type))
        (if color
            (setq text (semantic--format-uml-post-colorize text tag parent)))
        text))
     ;; rest - default format
     (t
      (semantic-format-tag-uml-prototype-default tag parent color)))))

;; symbols modifiers that equate to abstract
(define-mode-overload-implementation semantic-tag-abstract
  csharp-mode (tag &optional parent)
  "Return non nil if TAG is abstract."
  (let ((mods (semantic-tag-modifiers tag))
        (abs nil))
    (while (and (not abs) mods)
      (if (stringp (car mods))
          (setq abs (or (string= (car mods) "abstract")
                        (string= (car mods) "virtual")
                        (string= (car mods) "override"))))
      (setq mods (cdr mods)))
    abs))

(defvar wisent-csharp-internal-is-protected-p t
  "Says whether to show tags with internal protection as protected.
If non-nil any tags marked internal will be displayed as if they were
protected.")

;; map tag protection string to symbol
(define-mode-overload-implementation semantic-tag-protection
  csharp-mode (tag &optional parent)
  ""
  (let ((mods (semantic-tag-modifiers tag))
        (prot nil))
    (while (and (not prot) mods)
      (if (stringp (car mods))
          (let ((s (car mods)))
            (setq prot
                  ;; A few silly defaults to get things started.
                  (cond ((string= s "public")
                         'public)
                        ((string= s "private")
                         'private)
                        ((string= s "protected")
                         'protected)
                        ((string= s "internal")
                         (if wisent-csharp-internal-is-protected-p
                             'protected 'internal))))))
      (setq mods (cdr mods)))
    prot))


;; override semantic-format-uml-protection-to-string to return "%" for 'internal

;; add to semantic-format-tag-protection-image-alist for 'internal


;; Local context
(define-mode-overload-implementation semantic-get-local-variables
  csharp-mode ()
  "Get local values from a specific context.
Parse the current context for `local_variable_declaration' nonterminals to
collect tags, such as local variables or prototypes.
This function overrides `get-local-variables'."
  (let ((vars nil)
        ;; We want nothing to do with funny syntaxing while doing this.
        (semantic-unmatched-syntax-hook nil)
        (origp (point))
        start end)
    (save-excursion
      (while (not (semantic-up-context (point) 'function))
        (save-excursion
          (forward-char 1)
          (setq start (point)
                end (min (progn (semantic-end-of-context) (point)) origp))
          (setq vars
                (append (semantic-parse-region
                         start end
                         ;;'field_declaration
                         'local_variable_declaration
                         0 t)
                        vars))))
    vars)))


;;;----------------------------------------------------------------------
;;; * Lexer
;;;----------------------------------------------------------------------

(define-lex-regex-analyzer wisent-csharp-lex-ignore-region
  "Ignore # type macros for C sharp."
  "^\\s-*#region\\>"
  (goto-char (match-end 0))
  (forward-word 1)
  (setq semantic-lex-end-point (point))
  nil)

(define-lex-regex-analyzer wisent-csharp-lex-ignore-endregion
  "Ignore # type macros for C sharp."
  "^\\s-*#endregion\\>"
  (setq semantic-lex-end-point (match-end 0))
  nil)

(define-lex-analyzer wisent-csharp-lex-string
  "Detect and create a string token for csharp strings."
  (looking-at wisent-csharp-string-re)
  (semantic-lex-push-token
   (semantic-lex-token
    'STRING_LITERAL (point)
    (save-excursion
      (semantic-lex-unterminated-syntax-protection 'STRING_LITERAL
        ;; skip over "@" character if any
        (goto-char (1- (match-end 0)))
        (forward-sexp 1)
        (point))))))

(define-lex-simple-regex-analyzer wisent-csharp-lex-number
  "Detect and create number tokens."
  semantic-lex-number-expression 'NUMBER_LITERAL)

(define-lex-regex-analyzer wisent-csharp-lex-symbol
  "Detect and create identifier or keyword tokens."
  "\\(\\sw\\|\\s_\\)+"
  (semantic-lex-push-token
   (semantic-lex-token
    (or (semantic-lex-keyword-p (match-string 0))
        'IDENTIFIER)
    (match-beginning 0)
    (match-end 0))))

(define-lex-block-analyzer wisent-csharp-lex-blocks
  "Detect and create a open, close or block token."
  (PAREN_BLOCK ("(" LPAREN) (")" RPAREN))
  (BRACE_BLOCK ("{" LBRACE) ("}" RBRACE))
  (BRACK_BLOCK ("[" LBRACK) ("]" RBRACK))
  )

(define-lex wisent-csharp-lexer
  "Lexical analyzer for csharp code.
It ignores whitespaces, newlines and comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments
  wisent-csharp-lex-ignore-region
  wisent-csharp-lex-ignore-endregion
  wisent-csharp-lex-number
  wisent-csharp-lex-string
  wisent-csharp-lex-symbol
  semantic-lex-punctuation-type
  wisent-csharp-lex-blocks
  semantic-lex-default-action)

  
;;;----------------------------------------------------------------------
;;; * Parser
;;;----------------------------------------------------------------------

;;;###autoload   
(defun wisent-csharp-default-setup ()
  (wisent-csharp-wy--install-parser)
  (setq
   ;; Lexical analysis
   semantic-lex-number-expression wysent-csharp-number-re
   semantic-lex-analyzer #'wisent-csharp-lexer
   ;; Parsing
   semantic-tag-expand-function 'wisent-csharp-expand-tag
   ;; Environment
   semantic-type-relation-separator-character '(".")
   semantic-command-separation-character ";"
   ;; Imenu setup
   semantic-imenu-summary-function 'semantic-format-tag-uml-prototype
   imenu-create-index-function 'semantic-create-imenu-index
   ;; speedbar and imenu bucket names
   ;; .. in type parts
   semantic-symbol->name-assoc-list-for-type-parts
   '((type     . "Types")
     (variable . "Variables")
     (function . "Methods"))
   ;; .. everywhere
   semantic-symbol->name-assoc-list
   (append semantic-symbol->name-assoc-list-for-type-parts
           '((include  . "Using")))
   ;; navigation inside 'type children
   senator-step-at-tag-classes '(function variable)
   ))

;;;###autoload   
(add-hook 'csharp-mode-hook #'wisent-csharp-default-setup)


;;;----------------------------------------------------------------------
;;; * Test
;;;----------------------------------------------------------------------

(defun wisent-csharp-lex-buffer (&optional arg)
  "Run `wisent-csharp-lexer' on current buffer."
  (interactive "P")
  (semantic-lex-init)
  (setq semantic-lex-analyzer 'wisent-csharp-lexer)
  (let ((token-stream
         (semantic-lex (point-min) (point-max)
                       (if arg (prefix-numeric-value arg)))))
    (with-current-buffer
        (get-buffer-create "*wisent-csharp-lexer*")
      (erase-buffer)
      (pp token-stream (current-buffer))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))


(provide 'wisent-csharp)

;;; wisent-csharp.el ends here
