;;; wisent-c.el -- LALR parser that produces Semantic tags for C
;;
;; Copyright (C) 2003 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 13 Jun 2003
;; Keywords: syntax
;; X-RCS: $Id: wisent-c.el,v 1.6 2005/09/30 20:23:04 zappo Exp $
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

;;; History:
;;

;;; Code:
(require 'semantic-wisent)
(require 'wisent-c-wy)

;;; Compatibility
;;
(if (fboundp 'c-end-of-macro)
    (defalias 'wisent-c-end-of-macro 'c-end-of-macro)
  ;; From cc-mode 5.30
  (defun wisent-c-end-of-macro ()
    "Go to the end of a preprocessor directive.
More accurately, move point to the end of the closest following line
that doesn't end with a line continuation backslash.

This function does not do any hidden buffer changes."
    (while (progn
             (end-of-line)
             (when (and (eq (char-before) ?\\)
                        (not (eobp)))
               (forward-char)
               t))))
  )

;;; Analyzers
;;
(define-lex-regex-analyzer wisent-c-lex-symbol
  "Detect and create identifier or keyword tokens."
  "\\(\\sw\\|\\s_\\)+"
  (semantic-lex-push-token
   (semantic-lex-token
    (or (semantic-lex-keyword-p (match-string 0))
        'IDENTIFIER)
    (match-beginning 0)
    (match-end 0))))

(define-lex-simple-regex-analyzer wisent-c-lex-number
  "Detect and create number tokens."
  semantic-lex-number-expression 'NUMBER)

(define-lex-regex-analyzer wisent-c-lex-string
  "Detect and create character or string tokens."
  "L?\\(\\s\"\\)"
  ;; Zing to the end of this string.
  (semantic-lex-push-token
   (semantic-lex-token
    'CHAR_OR_STRING (point)
    (save-excursion
      ;; Skip L prefix if present.
      (goto-char (match-beginning 1))
      (semantic-lex-unterminated-syntax-protection 'CHAR_OR_STRING
        (forward-sexp 1)
        (point))))))

(define-lex-block-analyzer wisent-c-lex-blocks
  "Detect and create open, close or block tokens."
  (PAREN_BLOCK ("(" LPAREN) (")" RPAREN))
  (BRACE_BLOCK ("{" LBRACE) ("}" RBRACE))
  (BRACK_BLOCK ("[" LBRACK) ("]" RBRACK))
  )

;; C Preprocessor
(define-lex-simple-regex-analyzer wisent-c-lex-pp-include
  "Detect and create preprocessor include tokens."
  "^\\s-*#\\s-*include\\>" 'PP_INCLUDE)

(define-lex-regex-analyzer wisent-c-lex-pp-skip-if-0
  "Block out code matched in an #if 0 condition."
  "^\\s-*#\\s-*if\\s-*0\\s-*$"
  (beginning-of-line)
  (c-forward-conditional 1)
  (setq semantic-lex-end-point (point))
  nil)

(define-lex-regex-analyzer wisent-c-lex-pp-skip-unused
  "Skip unused preprocessor directives."
  (concat "^\\s-*#\\s-*"
          (regexp-opt '(
                        "define"
                        "undef"
                        "if"
                        "ifdef"
                        "ifndef"
                        "elif"
                        "else"
                        "endif"
                        "line"
                        "error"
                        "pragma"
                        )
                      t)
          "\\>")
  (wisent-c-end-of-macro)
  (setq semantic-lex-end-point (point))
  nil)

(define-lex-regex-analyzer wisent-c-lex-pp-skip-newline
  "Skip backslash ending a preprocessor line.
Go to the next line."
  "\\\\\\s-*\n"
  (setq semantic-lex-end-point (match-end 0)))

;;; Lexers
;;
(define-lex wisent-c-lexer
  "Lexical analyzer that handles C buffers.
It ignores whitespaces, newlines and comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments
  wisent-c-lex-pp-skip-if-0
  wisent-c-lex-pp-skip-unused
  wisent-c-lex-pp-skip-newline
  wisent-c-lex-pp-include
  wisent-c-lex-number
  ;; Must detect C strings before symbols because of possible L prefix!
  wisent-c-lex-string
  wisent-c-lex-symbol
  semantic-lex-punctuation-type
  wisent-c-lex-blocks
  semantic-lex-default-action)

(defvar wisent-c-identifier  nil)
(defvar wisent-c-paren-stack nil)

(defun wisent-c-init-parser-context ()
  "Setup a context for the LR parser engine."
  (setq wisent-c-identifier  nil
        wisent-c-paren-stack nil))

(defun wisent-c-lex ()
  "Return the next available lexical token in Wisent's form.
Change class of IDENTIFIER tokens that are typedef to TYPEDEFNAME."
  (let* ((token (wisent-lex))
         (class (semantic-lex-token-class token))
         )
    (cond
     ((memq class '(TYPEDEF VOID CHAR SHORT INT LONG FLOAT DOUBLE
                            SIGNED UNSIGNED BOOL COMPLEX IMAGINARY
                            TYPEDEFNAME RBRACE))
      (setq wisent-c-identifier t)
      )
     ((memq class '(STRUCT UNION ENUM RBRACK SEMIC))
      (setq wisent-c-identifier nil)
      )
     ((eq class 'IDENTIFIER)
      (unless wisent-c-identifier
        (setcar token 'TYPEDEFNAME))
      (setq wisent-c-identifier (not wisent-c-identifier))
      )
     ((eq class 'COMMA)
      (setq wisent-c-identifier
            (not (eq 'LPAREN (car wisent-c-paren-stack))))
      )
     ((eq class 'LPAREN)
      (when wisent-c-identifier
        (setcar token 'LPAREN-DECL))
      (push (car token) wisent-c-paren-stack)
      )
     ((eq class 'RPAREN)
      (when (eq 'LPAREN-DECL (pop wisent-c-paren-stack))
        (setcar token 'RPAREN-DECL))
      (setq wisent-c-identifier nil)
      )
;;      ((eq class 'LBRACE)
;;       (push wisent-c-identifier wisent-c-paren-stack)
;;       (setq wisent-c-identifier nil)
;;       )
;;      ((eq class 'RBRACE)
;;       (setq wisent-c-identifier (pop wisent-c-paren-stack))
;;       )
     )
    token))

;; (defun wisent-c-lex ()
;;   "Return the next available lexical token in Wisent's form.
;; Change class of IDENTIFIER tokens that are typedef to TYPEDEFNAME."
;;   (let* ((token (wisent-lex))
;;          (class (semantic-lex-token-class token))
;;          )
;;     (if (eq t (car wisent-c-paren-stack))
;;         (cond ((eq class 'RBRACE)
;;                (pop wisent-c-paren-stack))
;;               ((eq class 'RBRACK)
;;                (pop wisent-c-paren-stack)
;;                (setq wisent-c-identifier nil))
;;               )
;;       (cond
;;        ((memq class '(TYPEDEF VOID CHAR SHORT INT LONG FLOAT DOUBLE
;;                               SIGNED UNSIGNED BOOL COMPLEX IMAGINARY
;;                               TYPEDEFNAME))
;;         (setq wisent-c-identifier t)
;;         )
;;        ((eq class 'IDENTIFIER)
;;         (unless wisent-c-identifier
;;           (setcar token 'TYPEDEFNAME))
;;         (setq wisent-c-identifier (not wisent-c-identifier))
;;         )
;;        ((memq class '(STRUCT UNION ENUM SEMIC))
;;         (setq wisent-c-identifier nil)
;;         )
;;        ((eq class 'COMMA)
;;         (setq wisent-c-identifier
;;               (not (eq 'LPAREN (pop wisent-c-paren-stack))))
;;         )
;;        ((eq class 'LPAREN)
;;         (when wisent-c-identifier
;;           (setcar token 'LPAREN-DECL))
;;         (push (car token) wisent-c-paren-stack)
;;         )
;;        ((eq class 'RPAREN)
;;         (when (eq 'LPAREN-DECL (pop wisent-c-paren-stack))
;;           (setcar token 'RPAREN-DECL))
;;         (setq wisent-c-identifier nil)
;;         )
;;        ((memq class '(LBRACE LBRACK))
;;         (push t wisent-c-paren-stack)
;;         )
;;        ))
;;     token))

(defun wisent-c-lex-buffer ()
  "Run `wisent-c-lexer' on current buffer."
  (interactive)
  (semantic-lex-init)
  (setq semantic-flex-keywords-obarray wisent-c-keyword-table
        semantic-lex-types-obarray wisent-c-token-table
        semantic-lex-depth nil
        semantic-lex-analyzer 'wisent-c-lexer)
  (let ((token-stream
         (semantic-lex (point-min) (point-max))))
    (with-current-buffer
        (get-buffer-create "*wisent-c-lexer*")
      (erase-buffer)
      (pp token-stream (current-buffer))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;; Parser
;;

;;;###autoload
(defun wisent-c-default-setup ()
  "Setup buffer for parse."
  (wisent-c-wy--install-parser)
  (semantic-make-local-hook 'wisent-pre-parse-hook)
  (add-hook 'wisent-pre-parse-hook
            'wisent-c-init-parser-context nil t)
  (setq
   ;; Lexical analysis
   ;;semantic-lex-number-expression wisent-c-number-regexp
   semantic-lex-depth nil
   semantic-lex-analyzer 'wisent-c-lexer
   wisent-lexer-function 'wisent-c-lex
   ;; Parsing
   semantic-tag-expand-function 'wisent-c-expand-tag
   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-prototype
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-type-relation-separator-character '("." "->")
   semantic-command-separation-character ";"
   document-comment-start "/*"
   document-comment-line-prefix " *"
   document-comment-end " */"
   ;; speedbar and imenu buckets name
   ;; everywhere
   semantic-symbol->name-assoc-list
   '((type     . "Types")
     (variable . "Variables")
     (function . "Functions")
     (include  . "Includes")
     )
   ;; in type parts
   semantic-symbol->name-assoc-list-for-type-parts
   '((type     . "Types")
     (variable . "Attributes")
     (function . "Methods")
     (label    . "Labels")
     )
   ;; navigation inside 'type children
   senator-step-at-tag-classes '(function variable)
   ))

(add-hook 'c-mode-hook 'wisent-c-default-setup)

(defun wisent-c-expand-tag (tag)
  "Expand TAG into a list of derived tags, or nil.

Expand special tags of class 'goal.  Each 'goal tag has an attribute
`:tree' whose value is a list of already expanded tags.

Expand tags issued from compound definitions, that is variable tags
whose name is a list of abstract syntax trees that contain the
following nodes:

  :id         - a C identifier.
  :specifiers - type qualifiers like const, restrict and volatile.
  :parms      - list of arguments for a compound function prototype.
  :location   - the compound declaration bounds."
  (cond
   ;; Expand a goal tag.
   ((semantic-tag-of-class-p tag 'goal)
    (semantic-tag-get-attribute tag :tree)
    )
   ;; Expand compound declarations.
   ((consp (semantic-tag-name tag))
    (let ((items (semantic-tag-name tag))
          (tmods (semantic-tag-get-attribute tag :typemodifiers))
          ast name args spec bnds start end clone xpand)
      (while items
        ;; For each compound declaration, derive TAG to a new variable
        ;; or function tag (when an arglist exists).  Give it the name
        ;; of the compound item, and merge the attributes of TAG with
        ;; those of the compound item.  Finally, set the bounds of the
        ;; derived tag to those of the compound declaration.
        (setq ast   (car items)
              items (cdr items)
              name  (semantic-ast-get1 ast :id)
              args  (semantic-ast-get  ast :parms)
              spec  (semantic-ast-get  ast :specifiers)
              bnds  (semantic-ast-get1 ast :location)
              start (if items (car bnds) (semantic-tag-start tag))
              end   (if xpand (cdr bnds) (semantic-tag-end   tag))
              clone (if args
                        (semantic-tag-new-function name type (car args))
                      (semantic-tag-clone tag name))
              xpand (cons clone xpand))
        ;; Merge the attributes.
        (semantic-tag-put-attribute
         clone :typemodifiers (append tmods spec))
        ;; Set the bounds.
        (semantic-tag-set-bounds clone start end))
      (nreverse xpand))
    )))

(defun wisent-c-skip (&optional range)
  "Safely skip the given RANGE and try to continue parsing.
The optional argument RANGE can be the symbol `statement' to skip the
current C statement, or the symbol `line' to skip the current line.
By default, skip the current token.
Used in error recovery semantic actions."
  (let* ((start (nth 2 wisent-input))
         (end (save-excursion
                (goto-char start)
                (cond
                 ((eq range 'line)
                  (end-of-line))
                 ((eq range 'statement)
                  (c-end-of-statement))
                 (t
                  (goto-char (nth 2 wisent-input))))
                (point))))
    (wisent-error (format "Skipping invalid '%s' from %s to %s"
                          $nterm start end))
    ;; Read input until end is reached or EOI.
    (while (and (not (eq (car wisent-input) wisent-eoi-term))
                (<= (nth 2 wisent-input) end))
      (run-hook-with-args 'wisent-discarding-token-functions
                          wisent-input)
      (setq wisent-input (wisent-lexer)))
    (unless (eq wisent-eoi-term (car wisent-input))
      (wisent-errok))
    (wisent-set-region start (1+ end))
    nil))

(provide 'wisent-c)

;;; wisent-c.el ends here
