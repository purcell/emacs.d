;;; ledger-regex.el --- Helper code for use with the "ledger" command-line tool

;; Copyright (C) 2003-2016 John Wiegley (johnw AT gnu DOT org)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.

(require 'rx)
(require 'cl-lib)

(defconst ledger-amount-regex
  (concat "\\(  \\|\t\\| \t\\)[ \t]*-?"
          "\\([A-Z$€£₹_(]+ *\\)?"
          ;; We either match just a number after the commodity with no
          ;; decimal or thousand separators or a number with thousand
          ;; separators.  If we have a decimal part starting with `,'
          ;; or `.', because the match is non-greedy, it must leave at
          ;; least one of those symbols for the following capture
          ;; group, which then finishes the decimal part.
          "\\(-?\\(?:[0-9]+\\|[0-9,.]+?\\)\\)"
          "\\([,.][0-9)]+\\)?"
          "\\( *[[:word:]€£₹_\"]+\\)?"
          "\\([ \t]*[@={]@?[^\n;]+?\\)?"
          "\\([ \t]+;.+?\\|[ \t]*\\)?$"))

(defconst ledger-amount-decimal-comma-regex
  "-?[1-9][0-9.]*[,]?[0-9]*")

(defconst ledger-amount-decimal-period-regex
  "-?[1-9][0-9,]*[.]?[0-9]*")

(defconst ledger-other-entries-regex
  "\\(^[~=A-Za-z].+\\)+")

(defconst ledger-comment-regex
  "^[;#|\\*%].*\\|[ \t]+;.*")

(defconst ledger-multiline-comment-start-regex
  "^!comment$")
(defconst ledger-multiline-comment-end-regex
  "^!end_comment$")
(defconst ledger-multiline-comment-regex
  "^!comment\n\\(.*\n\\)*?!end_comment$")

(defconst ledger-payee-any-status-regex
  "^[0-9]+[-/][-/.=0-9]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+\\(.+?\\)\\s-*\\(;\\|$\\)")

(defconst ledger-payee-pending-regex
  "^[0-9]+[-/][-/.=0-9]+\\s-\\!\\s-+\\(([^)]+)\\s-+\\)?\\([^*].+?\\)\\s-*\\(;\\|$\\)")

(defconst ledger-payee-cleared-regex
  "^[0-9]+[-/][-/.=0-9]+\\s-\\*\\s-+\\(([^)]+)\\s-+\\)?\\([^*].+?\\)\\s-*\\(;\\|$\\)")

(defconst ledger-payee-uncleared-regex
  "^[0-9]+[-/][-/.=0-9]+\\s-+\\(([^)]+)\\s-+\\)?\\([^*].+?\\)\\s-*\\(;\\|$\\)")

(defconst ledger-init-string-regex
  "^--.+?\\($\\|[ ]\\)")

(defconst ledger-account-any-status-regex
  "^[ \t]+\\([*!]\\s-+\\)?\\([[(]?.+?\\)\\(\t\\|\n\\| [ \t]\\)")

(defun ledger-account-any-status-with-seed-regex (seed)
  (concat "^[ \t]+\\([*!]\\s-+\\)?\\([[(]?" seed ".+?\\)\\(\t\\|\n\\| [ \t]\\)"))

(defconst ledger-account-pending-regex
  "\\(^[ \t]+\\)\\(!\\s-*.*?\\)\\(  \\|\t\\|$\\)")

(defconst ledger-account-cleared-regex
  "\\(^[ \t]+\\)\\(*\\s-*.*?\\)\\(  \\|\t\\|$\\)")


(defmacro ledger-define-regexp (name regex docs &rest args)
  "Simplify the creation of a Ledger regex and helper functions."
  (let ((defs
          (list
           `(defconst
              ,(intern (concat "ledger-" (symbol-name name) "-regexp"))
              ,(eval regex))))
        (addend 0) last-group)
    (if (null args)
        (progn
          (nconc
           defs
           (list
            `(defconst
               ,(intern
                 (concat "ledger-regex-" (symbol-name name) "-group"))
               1)))
          (nconc
           defs
           (list
            `(defconst
               ,(intern (concat "ledger-regex-" (symbol-name name)
                                "-group--count"))
               1)))
          (nconc
           defs
           (list
            `(defmacro
                 ,(intern (concat "ledger-regex-" (symbol-name name)))
                 (&optional string)
               ,(format "Return the match string for the %s" name)
               (match-string
                ,(intern (concat "ledger-regex-" (symbol-name name)
                                 "-group"))
                string)))))

      (dolist (arg args)
        (let (var grouping target)
          (if (symbolp arg)
              (setq var arg target arg)
            (cl-assert (listp arg))
            (if (= 2 (length arg))
                (setq var (car arg)
                      target (cadr arg))
              (setq var (car arg)
                    grouping (cadr arg)
                    target (cl-caddr arg))))

          (if (and last-group
                   (not (eq last-group (or grouping target))))
              (cl-incf addend
                       (symbol-value
                        (intern-soft (concat "ledger-regex-"
                                             (symbol-name last-group)
                                             "-group--count")))))
          (nconc
           defs
           (list
            `(defconst
               ,(intern (concat "ledger-regex-" (symbol-name name)
                                "-group-" (symbol-name var)))
               ,(+ addend
                   (symbol-value
                    (intern-soft
                     (if grouping
                         (concat "ledger-regex-" (symbol-name grouping)
                                 "-group-" (symbol-name target))
                       (concat "ledger-regex-" (symbol-name target)
                               "-group"))))))))
          (nconc
           defs
           (list
            `(defmacro
                 ,(intern (concat "ledger-regex-" (symbol-name name)
                                  "-" (symbol-name var)))
                 (&optional string)
               ,(format "Return the sub-group match for the %s %s."
                        name var)
               (match-string
                ,(intern (concat "ledger-regex-" (symbol-name name)
                                 "-group-" (symbol-name var)))
                string))))

          (setq last-group (or grouping target))))

      (nconc defs
             (list
              `(defconst ,(intern (concat "ledger-regex-" (symbol-name name)
                                          "-group--count"))
                 ,(length args)))))

    (cons 'progn defs)))

(put 'ledger-define-regexp 'lisp-indent-function 1)

(ledger-define-regexp iso-date
                      ( let ((sep '(or ?-  ?/)))
                        (rx (group
                             (and (group (= 4 num))
                                  (eval sep)
                                  (group (and num (? num)))
                                  (eval sep)
                                  (group (and num (? num)))))))
                      "Match a single date, in its 'written' form.")

(ledger-define-regexp full-date
                      (macroexpand
                       `(rx (and (regexp ,ledger-iso-date-regexp)
                                 (? (and ?= (regexp ,ledger-iso-date-regexp))))))
                      "Match a compound date, of the form ACTUAL=EFFECTIVE"
                      (actual iso-date)
                      (effective iso-date))

(ledger-define-regexp state
                      (rx (group (any ?! ?*)))
                      "Match a transaction or posting's \"state\" character.")

(ledger-define-regexp code
                      (rx (and ?\( (group (+? (not (any ?\))))) ?\)))
                      "Match the transaction code.")

(ledger-define-regexp long-space
                      (rx (and (*? blank)
                               (or (and ?  (or ?  ?\t)) ?\t)))
                      "Match a \"long space\".")

(ledger-define-regexp note
                      (rx (group (+ nonl)))
                      "")

(ledger-define-regexp end-note
                      (macroexpand
                       `(rx (and (regexp ,ledger-long-space-regexp) ?\;
                                 (regexp ,ledger-note-regexp))))
                      "")

(ledger-define-regexp full-note
                      (macroexpand
                       `(rx (and line-start (+ blank)
                                 ?\; (regexp ,ledger-note-regexp))))
                      "")

(ledger-define-regexp xact-line
                      (macroexpand
                       `(rx (and line-start
                                 (regexp ,ledger-full-date-regexp)
                                 (? (and (+ blank) (regexp ,ledger-state-regexp)))
                                 (? (and (+ blank) (regexp ,ledger-code-regexp)))
                                 (+ blank) (+? nonl)
                                 (? (regexp ,ledger-end-note-regexp))
                                 line-end)))
                      "Match a transaction's first line (and optional notes)."
                      (actual-date full-date actual)
                      (effective-date full-date effective)
                      state
                      code
                      (note end-note))

(ledger-define-regexp recurring-line
                      (macroexpand
                       `(rx (and line-start
                                 (regexp "\\[.+/.+/.+\\]")
                                 (? (and (+ blank) (regexp ,ledger-state-regexp)))
                                 (? (and (+ blank) (regexp ,ledger-code-regexp)))
                                 (+ blank) (+? nonl)
                                 (? (regexp ,ledger-end-note-regexp))
                                 line-end)))
                      "Match a transaction's first line (and optional notes)."
                      (actual-date full-date actual)
                      (effective-date full-date effective)
                      state
                      code
                      (note end-note))

(ledger-define-regexp account
                      (rx (group (and (not (any blank ?\[ ?\( ?: ?\;)) (*? nonl))))
                      "")

(ledger-define-regexp account-kind
                      (rx (group (? (any ?\[ ?\())))
                      "")

(ledger-define-regexp full-account
                      (macroexpand
                       `(rx (and (regexp ,ledger-account-kind-regexp)
                                 (regexp ,ledger-account-regexp)
                                 (? (any ?\] ?\))))))
                      ""
                      (kind account-kind)
                      (name account))

(ledger-define-regexp commodity
                      (rx (group
                           (or (and ?\" (+ (not (any ?\"))) ?\")
                               (not (any blank ?\n
                                         digit
                                         ?- ?\[ ?\]
                                         ?. ?, ?\; ?+ ?* ?/ ?^ ?? ?: ?& ?| ?! ?=
                                         ?\< ?\> ?\{ ?\} ?\( ?\) ?@)))))
                      "")

(ledger-define-regexp amount
                      (rx (group
                           (and (? ?-)
                                (and (+ digit)
                                     (*? (and (any ?. ?,) (+ digit))))
                                (? (and (any ?. ?,) (+ digit))))))
                      "")

(ledger-define-regexp commoditized-amount
                      (macroexpand
                       `(rx (group
                             (or (and (regexp ,ledger-commodity-regexp)
                                      (*? blank)
                                      (regexp ,ledger-amount-regexp))
                                 (and (regexp ,ledger-amount-regexp)
                                      (*? blank)
                                      (regexp ,ledger-commodity-regexp))))))
                      "")

(ledger-define-regexp commodity-annotations
                      (macroexpand
                       `(rx (* (+ blank)
                               (or (and ?\{ (regexp ,ledger-commoditized-amount-regexp) ?\})
                                   (and ?\[ (regexp ,ledger-iso-date-regexp) ?\])
                                   (and ?\( (not (any ?\))) ?\))))))
                      "")

(ledger-define-regexp cost
                      (macroexpand
                       `(rx (and (or "@" "@@") (+ blank)
                                 (regexp ,ledger-commoditized-amount-regexp))))
                      "")

(ledger-define-regexp balance-assertion
                      (macroexpand
                       `(rx (and ?= (+ blank)
                                 (regexp ,ledger-commoditized-amount-regexp))))
                      "")

(ledger-define-regexp full-amount
                      (macroexpand `(rx (group (+? (not (any ?\;))))))
                      "")

(ledger-define-regexp post-line
                      (macroexpand
                       `(rx (and line-start (+ blank)
                                 (? (and (regexp ,ledger-state-regexp) (* blank)))
                                 (regexp ,ledger-full-account-regexp)
                                 (? (and (regexp ,ledger-long-space-regexp)
                                         (regexp ,ledger-full-amount-regexp)))
                                 (? (regexp ,ledger-end-note-regexp))
                                 line-end)))
                      ""
                      state
                      (account-kind full-account kind)
                      (account full-account name)
                      (amount full-amount)
                      (note end-note))

(defconst ledger-iterate-regex
  (concat "\\(\\(?:Y\\|year\\)\\s-+\\([0-9]+\\)\\|"  ;; Catches a Y/year directive
          ledger-iso-date-regexp
          "\\([ *!]+\\)"  ;; mark
          "\\((.*)\\)?"  ;; code
          "\\([[:word:] ]+\\)"   ;; desc
          "\\)"))

(defconst ledger-incomplete-date-regexp
  "\\(?:\\([0-9]\\{1,2\\}\\)[-/]\\)?\\([0-9]\\{1,2\\}\\)")

(defconst ledger-xact-start-regex
  (concat "^" ledger-iso-date-regexp  ;; subexp 1
          "\\(=" ledger-iso-date-regexp "\\)?"
          ))

(defconst ledger-xact-after-date-regex
  (concat "\\([ \t]+[*!]\\)?"  ;; mark, subexp 1
          "\\([ \t]+(.*?)\\)?"  ;; code, subexp 2
          "\\([ \t]+[^;\n]+\\)"   ;; desc, subexp 3
          "\\(;[^\n]*\\)?" ;; comment, subexp 4
          ))

(defconst ledger-posting-regex
  (concat "^[ \t]+ ?"  ;; initial white space
          "\\([*!]\\)? ?" ;; state, subexpr 1
          "\\([[:print:]]+\\([ \t][ \t]\\)\\)"  ;; account, subexpr 2
          "\\([^;\n]*\\)"  ;; amount, subexpr 4
          "\\(.*\\)" ;; comment, subexpr 5
          ))



(defconst ledger-directive-start-regex
  "[=~;#%|\\*[A-Za-z]")


(provide 'ledger-regex)
