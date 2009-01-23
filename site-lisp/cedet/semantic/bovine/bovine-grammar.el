;;; bovine-grammar.el --- Bovine's input grammar mode
;;
;; Copyright (C) 2002, 2003, 2004, 2007, 2009 David Ponce
;;
;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 26 Aug 2002
;; Keywords: syntax
;; X-RCS: $Id: bovine-grammar.el,v 1.21 2009/01/10 00:16:26 zappo Exp $
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
;; Major mode for editing Bovine's input grammar (.by) files.

;;; History:
;;

;;; Code:
(require 'semantic-grammar)
(require 'bovine-grammar-macros)

;; Cache of macro definitions currently in use.
(defvar bovine--grammar-macros nil)

(defun bovine-grammar-expand-form (form quotemode &optional inplace)
  "Expand FORM into a new one suitable to the bovine parser.
FORM is a list in which we are substituting.
Argument QUOTEMODE is non-nil if we are in backquote mode.
When non-nil, optional argument INPLACE indicates that FORM is being
expanded from elsewhere."
  (when (eq (car form) 'quote)
    (setq form (cdr form))
    (cond
     ((and (= (length form) 1) (listp (car form)))
      (insert "\n(append")
      (bovine-grammar-expand-form (car form) quotemode nil)
      (insert ")")
      (setq form nil inplace nil)
      )
     ((and (= (length form) 1) (symbolp (car form)))
      (insert "\n'" (symbol-name (car form)))
      (setq form nil inplace nil)
      )
     (t
      (insert "\n(list")
      (setq inplace t)
      )))
  (let ((macro (assq (car form) bovine--grammar-macros))
        inlist first n q x)
    (if macro
        (bovine-grammar-expand-form
         (apply (cdr macro) (cdr form))
         quotemode t)
      (if inplace (insert "\n("))
      (while form
        (setq first (car form)
              form  (cdr form))
        (cond
         ((eq first nil)
          (when (and (not inlist) (not inplace))
            (insert "\n(list")
            (setq inlist t))
          (insert " nil")
          )
         ((listp first)
          ;;(let ((fn (and (symbolp (caar form)) (fboundp (caar form)))))
          (when (and (not inlist) (not inplace))
            (insert "\n(list")
            (setq inlist t))
          ;;(if (and inplace (not fn) (not (eq (caar form) 'EXPAND)))
          ;;    (insert " (append"))
          (bovine-grammar-expand-form
           first quotemode t) ;;(and fn (not (eq fn 'quote))))
          ;;(if (and inplace (not fn) (not (eq (caar form) 'EXPAND)))
          ;;    (insert  ")"))
          ;;)
          )
         ((symbolp first)
          (setq n (symbol-name first)   ;the name
                q quotemode             ;implied quote flag
                x nil)                  ;expand flag
          (if (eq (aref n 0) ?,)
              (if quotemode
                  ;; backquote mode needs the @
                  (if (eq (aref n 1) ?@)
                      (setq n (substring n 2)
                            q nil
                            x t)
                    ;; non backquote mode behaves normally.
                    (setq n (substring n 1)
                          q nil))
                (setq n (substring n 1)
                      x t)))
          (if (string= n "")
              (progn
                ;; We expand only the next item in place (a list?)
                ;; A regular inline-list...
                (bovine-grammar-expand-form (car form) quotemode t)
                (setq form (cdr form)))
            (if (and (eq (aref n 0) ?$)
                     ;; Don't expand $ tokens in implied quote mode.
                     ;; This acts like quoting in other symbols.
                     (not q))
                (progn
                  (cond
                   ((and (not x) (not inlist) (not inplace))
                    (insert "\n(list"))
                   ((and x inlist (not inplace))
                    (insert ")")
                    (setq inlist nil)))
                  (insert "\n(nth " (int-to-string
                                     (1- (string-to-number
                                          (substring n 1))))
                          " vals)")
                  (and (not x) (not inplace)
                       (setq inlist t)))
              
              (when (and (not inlist) (not inplace))
                (insert "\n(list")
                (setq inlist t))
              (or (char-equal (char-before) ?\()
                  (insert " "))
              (insert (if (or inplace (eq first t))
                          "" "'")
                      n))) ;; " "
          )
         (t
          (when (and (not inlist) (not inplace))
            (insert "\n(list")
            (setq inlist t))
          (insert (format "\n%S" first))
          )
         ))
      (if inlist (insert ")"))
      (if inplace (insert ")")))
    ))

(defun bovine-grammar-expand-action (textform quotemode)
  "Expand semantic action string TEXTFORM into Lisp code.
QUOTEMODE is the mode in which quoted symbols are slurred."
  (if (string= "" textform)
      nil
    (let ((sexp (read textform)))
      ;; We converted the lambda string into a list.  Now write it
      ;; out as the bovine lambda expression, and do macro-like
      ;; conversion upon it.
      (insert "\n")
      (cond
       ((eq (car sexp) 'EXPAND)
        (insert ",(lambda (vals start end)")
        ;; The EXPAND macro definition is mandatory
        (bovine-grammar-expand-form
         (apply (cdr (assq 'EXPAND bovine--grammar-macros)) (cdr sexp))
         quotemode t)
        )
       ((and (listp (car sexp)) (eq (caar sexp) 'EVAL))
        ;; The user wants to evaluate the following args.
        ;; Use a simpler expander
        )
       (t
        (insert ",(semantic-lambda")
        (bovine-grammar-expand-form sexp quotemode)
        ))
      (insert ")\n"))))

(defun bovine-grammar-parsetable-builder ()
  "Return the parser table expression as a string value.
The format of a bovine parser table is:

 ( ( NONTERMINAL-SYMBOL1 MATCH-LIST1 )
   ( NONTERMINAL-SYMBOL2 MATCH-LIST2 )
   ...
   ( NONTERMINAL-SYMBOLn MATCH-LISTn )
 
Where each NONTERMINAL-SYMBOL is an artificial symbol which can appear
in any child state.  As a starting place, one of the NONTERMINAL-SYMBOLS
must be `bovine-toplevel'.

A MATCH-LIST is a list of possible matches of the form:

 ( STATE-LIST1
   STATE-LIST2
   ...
   STATE-LISTN )

where STATE-LIST is of the form:
  ( TYPE1 [ \"VALUE1\" ] TYPE2 [ \"VALUE2\" ] ... LAMBDA )

where TYPE is one of the returned types of the token stream.
VALUE is a value, or range of values to match against.  For
example, a SYMBOL might need to match \"foo\".  Some TYPES will not
have matching criteria.

LAMBDA is a lambda expression which is evaled with the text of the
type when it is found.  It is passed the list of all buffer text
elements found since the last lambda expression.  It should return a
semantic element (see below.)

For consistency between languages, try to use common return values
from your parser.  Please reference the chapter \"Writing Parsers\" in
the \"Language Support Developer's Guide -\" in the semantic texinfo
manual."
  (let* ((start      (semantic-grammar-start))
         (scopestart (semantic-grammar-scopestart))
         (quotemode  (semantic-grammar-quotemode))
         (tags       (semantic-find-tags-by-class
                      'token (current-buffer)))
         (nterms     (semantic-find-tags-by-class
                      'nonterminal (current-buffer)))
         ;; Setup the cache of macro definitions.
         (bovine--grammar-macros (semantic-grammar-macros))
         nterm rules items item actn prec tag type regex)

    ;; Check some trivial things
    (cond
     ((null nterms)
      (error "Bad input grammar"))
     (start
      (if (cdr start)
          (message "Extra start symbols %S ignored" (cdr start)))
      (setq start (symbol-name (car start)))
      (unless (semantic-find-first-tag-by-name start nterms)
        (error "start symbol `%s' has no rule" start)))
     (t
      ;; Default to the first grammar rule.
      (setq start (semantic-tag-name (car nterms)))))
    (when scopestart
      (setq scopestart (symbol-name scopestart))
      (unless (semantic-find-first-tag-by-name scopestart nterms)
        (error "scopestart symbol `%s' has no rule" scopestart)))

    ;; Generate the grammar Lisp form.
    (with-temp-buffer
      (erase-buffer)
      (insert "`(")
      ;; Insert the start/scopestart rules
      (insert "\n(bovine-toplevel \n("
              start
              ")\n) ;; end bovine-toplevel\n")
      (when scopestart
        (insert "\n(bovine-inner-scope \n("
                scopestart
                ")\n) ;; end bovine-inner-scope\n"))
      ;; Process each nonterminal
      (while nterms
        (setq nterm  (car nterms)
              ;; We can't use the override form because the current buffer
              ;; is not the originator of the tag.
              rules  (semantic-tag-components-semantic-grammar-mode nterm)
              nterm  (semantic-tag-name nterm)
              nterms (cdr nterms))
        (when (member nterm '("bovine-toplevel" "bovine-inner-scope"))
          (error "`%s' is a reserved internal name" nterm))
        (insert "\n(" nterm)
        ;; Process each rule
        (while rules
          (setq items (semantic-tag-get-attribute (car rules) :value)
                prec  (semantic-tag-get-attribute (car rules) :prec)
                actn  (semantic-tag-get-attribute (car rules) :expr)
                rules (cdr rules))
          ;; Process each item
          (insert "\n(")
          (if (null items)
              ;; EMPTY rule
              (insert ";;EMPTY" (if actn "" "\n"))
            ;; Expand items
            (while items
              (setq item  (car items)
                    items (cdr items))
              (if (consp item) ;; mid-rule action
                  (message "Mid-rule action %S ignored" item)
                (or (char-equal (char-before) ?\()
                    (insert "\n"))
                (cond
                 ((member item '("bovine-toplevel" "bovine-inner-scope"))
                  (error "`%s' is a reserved internal name" item))
                 ;; Replace ITEM by its %token definition.
                 ;; If a '%token TYPE ITEM [REGEX]' definition exists
                 ;; in the grammar, ITEM is replaced by TYPE [REGEX].
                 ((setq tag (semantic-find-first-tag-by-name
                             item tags)
                        type  (semantic-tag-get-attribute tag :type))
                  (insert type)
                  (if (setq regex (semantic-tag-get-attribute tag :value))
                      (insert (format "\n%S" regex))))
                 ;; Don't change ITEM
                 (t
                  (insert (semantic-grammar-item-text item)))
                 ))))
          (if prec
              (message "%%prec %S ignored" prec))
          (if actn
              (bovine-grammar-expand-action actn quotemode))
          (insert ")"))
        (insert "\n) ;; end " nterm "\n"))
      (insert ")\n")
      (buffer-string))))

(defun bovine-grammar-setupcode-builder ()
  "Return the text of the setup code."
  (format
   "(setq semantic--parse-table %s\n\
          semantic-debug-parser-source %S\n\
          semantic-debug-parser-class 'semantic-bovine-debug-parser
          semantic-flex-keywords-obarray %s\n\
          %s)"
   (semantic-grammar-parsetable)
   (buffer-name)
   (semantic-grammar-keywordtable)
   (let ((mode (semantic-grammar-languagemode)))
     ;; Is there more than one major mode?
     (if (and (listp mode) (> (length mode) 1))
         (format "semantic-equivalent-major-modes '%S\n" mode)
       ""))))

(defvar bovine-grammar-menu
  '("BY Grammar"
    )
  "BY mode specific grammar menu.
Menu items are appended to the common grammar menu.")

;;;###autoload
(define-derived-mode bovine-grammar-mode semantic-grammar-mode "BY"
  "Major mode for editing Bovine grammars."
  (semantic-grammar-setup-menu bovine-grammar-menu)
  (semantic-install-function-overrides
   '((grammar-parsetable-builder . bovine-grammar-parsetable-builder)
     (grammar-setupcode-builder  . bovine-grammar-setupcode-builder)
     )))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.by$" . bovine-grammar-mode))

;;;###autoload
(eval-after-load "speedbar"
  '(speedbar-add-supported-extension ".by"))

(provide 'bovine-grammar)

;;; bovine-grammar.el ends here
