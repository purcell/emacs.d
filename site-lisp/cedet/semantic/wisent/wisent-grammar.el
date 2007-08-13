;;; wisent-grammar.el --- Wisent's input grammar mode
;;
;; Copyright (C) 2002, 2003, 2004 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 26 Aug 2002
;; Keywords: syntax
;; X-RCS: $Id: wisent-grammar.el,v 1.19 2005/09/30 20:23:27 zappo Exp $
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
;; Major mode for editing Wisent's input grammar (.wy) files.

;;; History:
;;

;;; Code:
(require 'semantic-grammar)
(require 'wisent-grammar-macros)
(eval-when-compile
  (require 'semantic-find))

(defun wisent-grammar-assocs ()
  "Return associativity and precedence level definitions."
  (mapcar
   #'(lambda (tag)
       (cons (intern (semantic-tag-name tag))
             (mapcar #'semantic-grammar-item-value
                     (semantic-tag-get-attribute tag :value))))
   (semantic-find-tags-by-class 'assoc (current-buffer))))

(defun wisent-grammar-terminals ()
  "Return the list of terminal symbols.
Keep order of declaration in the WY file without duplicates."
  (let (terms)
    (mapcar
     #'(lambda (tag)
         (mapcar #'(lambda (name)
                     (add-to-list 'terms (intern name)))
                 (cons (semantic-tag-name tag)
                       (semantic-tag-get-attribute tag :rest))))
     (semantic--find-tags-by-function
      #'(lambda (tag)
          (memq (semantic-tag-class tag) '(token keyword)))
      (current-buffer)))
    (nreverse terms)))

;; Cache of macro definitions currently in use.
(defvar wisent--grammar-macros nil)

(defun wisent-grammar-expand-macros (expr)
  "Expand expression EXPR into a form without grammar macros.
Return the expanded expression."
  (if (or (atom expr) (semantic-grammar-quote-p (car expr)))
      expr ;; Just return atom or quoted expression.
    (let* ((expr  (mapcar 'wisent-grammar-expand-macros expr))
           (macro (assq (car expr) wisent--grammar-macros)))
      (if macro ;; Expand Semantic built-in.
          (apply (cdr macro) (cdr expr))
        expr))))

(defun wisent-grammar-nonterminals ()
  "Return the list form of nonterminal definitions."
  (let ((nttags (semantic-find-tags-by-class
                 'nonterminal (current-buffer)))
        ;; Setup the cache of macro definitions.
        (wisent--grammar-macros (semantic-grammar-macros))
        rltags nterms rules rule elems elem actn sexp prec)
    (while nttags
      (setq rltags (semantic-tag-components (car nttags))
            rules  nil)
      (while rltags
        (setq elems (semantic-tag-get-attribute (car rltags) :value)
              prec  (semantic-tag-get-attribute (car rltags) :prec)
              actn  (semantic-tag-get-attribute (car rltags) :expr)
              rule  nil)
        (when elems ;; not an EMPTY rule
          (while elems
            (setq elem  (car elems)
                  elems (cdr elems))
            (setq elem (if (consp elem) ;; mid-rule action
                           (wisent-grammar-expand-macros (read (car elem)))
                         (semantic-grammar-item-value elem)) ;; item
                  rule (cons elem rule)))
          (setq rule (nreverse rule)))
        (if prec
            (setq prec (vector (semantic-grammar-item-value prec))))
        (if actn
            (setq sexp (wisent-grammar-expand-macros (read actn))))
        (setq rule (if actn
                       (if prec
                           (list rule prec sexp)
                         (list rule sexp))
                     (if prec
                         (list rule prec)
                       (list rule))))
        (setq rules (cons rule rules)
              rltags (cdr rltags)))
      (setq nterms (cons (cons (intern (semantic-tag-name (car nttags)))
                               (nreverse rules))
                         nterms)
            nttags (cdr nttags)))
    (nreverse nterms)))

(defun wisent-grammar-grammar ()
  "Return Elisp form of the grammar."
  (let* ((terminals    (wisent-grammar-terminals))
         (nonterminals (wisent-grammar-nonterminals))
         (assocs       (wisent-grammar-assocs)))
    (cons terminals (cons assocs nonterminals))))

(defun wisent-grammar-parsetable-builder ()
  "Return the value of the parser table."
  `(progn
     ;; Ensure that the grammar [byte-]compiler is available.
     (eval-when-compile (require 'wisent-comp))
     (wisent-compile-grammar
      ',(wisent-grammar-grammar)
      ',(semantic-grammar-start))))

(defun wisent-grammar-setupcode-builder ()
  "Return the parser setup code."
  (format
   "(semantic-install-function-overrides\n\
      '((parse-stream . wisent-parse-stream)))\n\
    (setq semantic-parser-name \"LALR\"\n\
          semantic--parse-table %s\n\
          semantic-debug-parser-source %S\n\
          semantic-flex-keywords-obarray %s\n\
          semantic-lex-types-obarray %s)\n\
    ;; Collect unmatched syntax lexical tokens\n\
    (semantic-make-local-hook 'wisent-discarding-token-functions)\n\
    (add-hook 'wisent-discarding-token-functions\n\
              'wisent-collect-unmatched-syntax nil t)"
   (semantic-grammar-parsetable)
   (buffer-name)
   (semantic-grammar-keywordtable)
   (semantic-grammar-tokentable)))

(defvar wisent-grammar-menu
  '("WY Grammar"
    ["LALR Compiler Verbose" wisent-toggle-verbose-flag
     :style toggle :active (boundp 'wisent-verbose-flag)
     :selected (and (boundp 'wisent-verbose-flag)
                    wisent-verbose-flag)]
    )
  "WY mode specific grammar menu.
Menu items are appended to the common grammar menu.")

;;;###autoload
(define-derived-mode wisent-grammar-mode semantic-grammar-mode "WY"
  "Major mode for editing Wisent grammars."
  (semantic-grammar-setup-menu wisent-grammar-menu)
  (semantic-install-function-overrides
   '((grammar-parsetable-builder . wisent-grammar-parsetable-builder)
     (grammar-setupcode-builder  . wisent-grammar-setupcode-builder)
     )))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wy$" . wisent-grammar-mode))

;;;###autoload
(eval-after-load "speedbar"
  '(speedbar-add-supported-extension ".wy"))

(provide 'wisent-grammar)

;;; wisent-grammar.el ends here
