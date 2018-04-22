;;; haskell-completions.el --- Haskell Completion package -*- lexical-binding: t -*-

;; Copyright © 2015-2016 Athur Fayzrakhmanov. All rights reserved.

;; This file is part of haskell-mode package.
;; You can contact with authors using GitHub issue tracker:
;; https://github.com/haskell/haskell-mode/issues

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides completions related functionality for
;; Haskell Mode such grab completion prefix at point, and etc..

;; Some description
;; ================
;;
;; For major use function `haskell-completions-grab-prefix' is supposed, and
;; other prefix grabbing functions are used internally by it.  So, only this
;; funciton have prefix minimal length functionality and invokes predicate
;; function `haskell-completions-can-grab-prefix'.

;;; Code:

(require 'haskell-mode)
(require 'haskell-process)
(require 'haskell-interactive-mode)

;;;###autoload
(defgroup haskell-completions nil
  "Settings for completions provided by `haskell-mode'"
  :link '(custom-manual "(haskell-mode)Completion support")
  :group 'haskell)

(defcustom haskell-completions-complete-operators
  t
  "Should `haskell-completions-sync-repl-completion-at-point' complete operators.

Note: GHCi prior to version 8.0.1 have bug in `:complete`
 command: when completing operators it returns a list of all
 imported identifiers (see Track ticket URL
 `https://ghc.haskell.org/trac/ghc/ticket/10576'). This leads to
 significant Emacs slowdown. To aviod slowdown you should set
 this variable to `nil'."
  :group 'haskell-completions
  :type 'boolean)

(defvar haskell-completions--pragma-names
  (list "DEPRECATED"
        "INCLUDE"
        "INCOHERENT"
        "INLINABLE"
        "INLINE"
        "LANGUAGE"
        "LINE"
        "MINIMAL"
        "NOINLINE"
        "NOUNPACK"
        "OPTIONS"
        "OPTIONS_GHC"
        "OVERLAPPABLE"
        "OVERLAPPING"
        "OVERLAPS"
        "RULES"
        "SOURCE"
        "SPECIALIZE"
        "UNPACK"
        "WARNING")
  "A list of supported pragmas.
This list comes from GHC documentation (URL
`https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/pragmas.html'.")

(defvar haskell-completions--keywords
  (list
   "as"
   "case"
   "class"
   "data family"
   "data instance"
   "data"
   "default"
   "deriving instance"
   "deriving"
   "do"
   "else"
   "family"
   "forall"
   "foreign import"
   "foreign"
   "hiding"
   "if"
   "import qualified"
   "import"
   "in"
   "infix"
   "infixl"
   "infixr"
   "instance"
   "let"
   "mdo"
   "module"
   "newtype"
   "of"
   "proc"
   "qualified"
   "rec"
   "then"
   "type family"
   "type instance"
   "type"
   "where")
  "A list of Haskell's keywords (URL `https://wiki.haskell.org/Keywords').
Single char keywords and operator like keywords are not included
in this list.")


(defun haskell-completions-can-grab-prefix ()
  "Check if the case is appropriate for grabbing completion prefix.
Returns t if point is either at whitespace character, or at
punctuation, or at line end and preceeding character is not a
whitespace or new line, otherwise returns nil.

  Returns nil in presence of active region."
  (when (not (region-active-p))
    (when (looking-at-p (rx (| space line-end punct)))
      (when (not (bobp))
        (save-excursion
          (backward-char)
          (not (looking-at-p (rx (| space line-end)))))))))

(defun haskell-completions-grab-pragma-prefix ()
  "Grab completion prefix for pragma completions.
Returns a list of form '(prefix-start-position
prefix-end-position prefix-value prefix-type) for pramga names
such as WARNING, DEPRECATED, LANGUAGE etc.  Also returns
completion prefixes for options in case OPTIONS_GHC pragma, or
language extensions in case of LANGUAGE pragma.  Obsolete OPTIONS
pragma is supported also."
  (when (nth 4 (syntax-ppss))
    ;; We're inside comment
    (let ((p (point))
          (comment-start (nth 8 (syntax-ppss)))
          (case-fold-search nil)
          prefix-start
          prefix-end
          prefix-type
          prefix-value)
      (save-excursion
        (goto-char comment-start)
        (when (looking-at (rx "{-#" (1+ (| space "\n"))))
          (let ((pragma-start (match-end 0)))
            (when (> p pragma-start)
              ;; point stands after `{-#`
              (goto-char pragma-start)
              (when (looking-at (rx (1+ (| upper "_"))))
                ;; found suitable sequence for pragma name
                (let ((pragma-end (match-end 0))
                      (pragma-value (match-string-no-properties 0)))
                  (if (eq p pragma-end)
                      ;; point is at the end of (in)complete pragma name
                      ;; prepare resulting values
                      (progn
                        (setq prefix-start pragma-start)
                        (setq prefix-end pragma-end)
                        (setq prefix-value pragma-value)
                        (setq prefix-type
                              'haskell-completions-pragma-name-prefix))
                    (when (and (> p pragma-end)
                               (or (equal "OPTIONS_GHC" pragma-value)
                                   (equal "OPTIONS" pragma-value)
                                   (equal "LANGUAGE" pragma-value)))
                      ;; point is after pragma name, so we need to check
                      ;; special cases of `OPTIONS_GHC` and `LANGUAGE` pragmas
                      ;; and provide a completion prefix for possible ghc
                      ;; option or language extension.
                      (goto-char pragma-end)
                      (when (re-search-forward
                             (rx (* anything)
                                 (1+ (regexp "\\S-")))
                             p
                             t)
                        (let* ((str (match-string-no-properties 0))
                               (split (split-string str (rx (| space "\n")) t))
                               (val (car (last split)))
                               (end (point)))
                          (when (and (equal p end)
                                     (not (string-match-p "#" val)))
                            (setq prefix-value val)
                            (backward-char (length val))
                            (setq prefix-start (point))
                            (setq prefix-end end)
                            (setq
                             prefix-type
                             (if (not (equal "LANGUAGE" pragma-value))
                                 'haskell-completions-ghc-option-prefix
                               'haskell-completions-language-extension-prefix
                               )))))))))))))
      (when prefix-value
        (list prefix-start prefix-end prefix-value prefix-type)))))

(defun haskell-completions-grab-identifier-prefix ()
  "Grab completion prefix for identifier at point.
Returns a list of form '(prefix-start-position
prefix-end-position prefix-value prefix-type) for haskell
identifier at point depending on result of function
`haskell-ident-pos-at-point'."
  (let ((pos-at-point (haskell-ident-pos-at-point))
        (p (point)))
    (when pos-at-point
      (let* ((start (car pos-at-point))
             (end (cdr pos-at-point))
             (type 'haskell-completions-identifier-prefix)
             (case-fold-search nil)
             value)
        ;; we need end position of result, becase of
        ;; `haskell-ident-pos-at-point' ignores trailing whitespace, e.g. the
        ;; result will be same for `map|` and `map  |` invocations.
        (when (<= p end)
          (setq end p)
          (setq value (buffer-substring-no-properties start end))
          (when (string-match-p (rx bos upper) value)
            ;; we need to check if found identifier is a module name
            (save-excursion
              (goto-char (line-beginning-position))
              (when (re-search-forward
                     (rx "import"
                         (? (1+ space) "qualified")
                         (1+ space)
                         upper
                         (1+ (| alnum ".")))
                     p    ;; bound
                     t)   ;; no-error
                (if (equal p (point))
                    (setq type 'haskell-completions-module-name-prefix)
                  (when (re-search-forward
                         (rx (| " as " "("))
                         start
                         t)
                    ;; but uppercase ident could occur after `as` keyword, or in
                    ;; module imports after opening parenthesis, in this case
                    ;; restore identifier type again, it's neccessary to
                    ;; distinguish the means of completions retrieval
                    (setq type 'haskell-completions-identifier-prefix))))))
          (when (nth 8 (syntax-ppss))
            ;; eighth element of syntax-ppss result is string or comment start,
            ;; so when it's not nil word at point is inside string or comment,
            ;; return special literal prefix type
            (setq type 'haskell-completions-general-prefix))
          ;; finally take in account minlen if given and return the result
          (when value (list start end value type)))))))

(defun haskell-completions-grab-prefix (&optional minlen)
   "Grab prefix at point for possible completion.
Returns a list of form '(prefix-start-position
prefix-end-position prefix-value prefix-type) depending on
situation, e.g. is it needed to complete pragma, module name,
arbitrary identifier, etc.  Returns nil in case it is
impossible to grab prefix.

Possible prefix types are:

* haskell-completions-pragma-name-prefix
* haskell-completions-ghc-option-prefix
* haskell-completions-language-extension-prefix
* haskell-completions-module-name-prefix
* haskell-completions-identifier-prefix
* haskell-completions-general-prefix

the last type is used in cases when completing things inside comments.

If provided optional MINLEN parameter this function will return
result only if prefix length is not less than MINLEN."
   (when (haskell-completions-can-grab-prefix)
     (let ((prefix (cond
                    ((haskell-completions-grab-pragma-prefix))
                    ((haskell-completions-grab-identifier-prefix)))))
       (cond ((and minlen prefix)
              (when (>= (length (nth 2 prefix)) minlen)
                prefix))
             (prefix prefix)))))

(defun haskell-completions--simple-completions (prefix)
  "Provide a list of completion candidates for given PREFIX.
This function is used internally in
`haskell-completions-completion-at-point' and
`haskell-completions-sync-repl-completion-at-point'.

It provides completions for haskell keywords, language pragmas,
GHC's options, and language extensions.

PREFIX should be a list such one returned by
`haskell-completions-grab-identifier-prefix'."
  (cl-destructuring-bind (beg end _pfx typ) prefix
    (when (not (eql typ 'haskell-completions-general-prefix))
      (let ((candidates
             (cl-case typ
               ('haskell-completions-pragma-name-prefix
                haskell-completions--pragma-names)
               ('haskell-completions-ghc-option-prefix
                haskell-ghc-supported-options)
               ('haskell-completions-language-extension-prefix
                haskell-ghc-supported-extensions)
               (otherwise
                (append (when (bound-and-true-p haskell-tags-on-save)
                          tags-completion-table)
                        haskell-completions--keywords)))))
        (list beg end candidates)))))

;;;###autoload
(defun haskell-completions-completion-at-point ()
  "Provide completion list for thing at point.
This function is used in non-interactive `haskell-mode'.  It
provides completions for haskell keywords, language pragmas,
GHC's options, and language extensions, but not identifiers."
  (let ((prefix (haskell-completions-grab-prefix)))
    (when prefix
      (haskell-completions--simple-completions prefix))))

(defun haskell-completions-sync-repl-completion-at-point ()
  "A completion function used in `interactive-haskell-mode'.
Completion candidates are provided quering current haskell
process, that is sending `:complete repl' command.

Completes all possible things: everything that can be completed
with non-interactive function
`haskell-completions-completion-at-point' plus identifier
completions.

Returns nil if no completions available."
  (let ((prefix-data (haskell-completions-grab-prefix)))
    (when prefix-data
      (cl-destructuring-bind (beg end pfx typ) prefix-data
        (when (and (not (eql typ 'haskell-completions-general-prefix))
                   (or haskell-completions-complete-operators
                       (not (save-excursion
                              (goto-char (1- end))
                              (haskell-mode--looking-at-varsym)))))
          ;; do not complete things in comments
          (if (cl-member
               typ
               '(haskell-completions-pragma-name-prefix
                 haskell-completions-ghc-option-prefix
                 haskell-completions-language-extension-prefix))
              ;; provide simple completions
              (haskell-completions--simple-completions prefix-data)
            ;; only two cases left: haskell-completions-module-name-prefix
            ;; and haskell-completions-identifier-prefix
            (let* ((is-import (eql typ 'haskell-completions-module-name-prefix))
                   (candidates
                    (when (and (haskell-session-maybe)
                               (not (haskell-process-cmd
                                     (haskell-interactive-process)))
                               ;; few possible extra checks would be:
                               ;; (haskell-process-get 'is-restarting)
                               ;; (haskell-process-get 'evaluating)
                               )
                      ;; if REPL is available and not busy try to query it for
                      ;; completions list in case of module name or identifier
                      ;; prefixes
                      (haskell-completions-sync-complete-repl pfx is-import))))
              ;; append candidates with keywords
              (list beg end (append
                             candidates
                             haskell-completions--keywords)))))))))

(defun haskell-completions-sync-complete-repl (prefix &optional import)
  "Return completion list for given PREFIX querying REPL synchronously.
When optional IMPORT argument is non-nil complete PREFIX
prepending \"import \" keyword (useful for module names).  This
function is supposed for internal use."
  (haskell-process-get-repl-completions
   (haskell-interactive-process)
   (if import
       (concat "import " prefix)
     prefix)))

(provide 'haskell-completions)
;;; haskell-completions.el ends here
