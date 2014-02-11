;;; dflet.el --- dynamically-scoped flet

;; Copyright (C) 2012  Yann Hodique
;; Copyright (C) 1993, 2001-2012  Free Software Foundation, Inc.

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is bringing back the historical definition of `flet', in all its global
;; and dynamic splendor.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'macroexp)
  (require 'subr-compat))

;;; silence byte-compiler
(eval-when-compile
  (if (version< emacs-version "24.3")
      ;; sure it doesn't exist, but it won't be called anyway...
      (autoload 'cl--compiling-file "cl")
    (declare-function cl--compiling-file "cl" t t)))

(if (version< emacs-version "24.3")
    ;; before that version, flet was not marked as obsolete, so use it
    (defalias 'dflet 'flet)

  ;; This should really have some way to shadow 'byte-compile properties, etc.
  (defmacro dflet (bindings &rest body)
    "Make temporary overriding function definitions.
This is an analogue of a dynamically scoped `let' that operates on the function
cell of FUNCs rather than their value cell.
If you want the Common-Lisp style of `flet', you should use `cl-flet'.
The FORMs are evaluated with the specified function definitions in place,
then the definitions are undone (the FUNCs go back to their previous
definitions, or lack thereof).

\(fn ((FUNC ARGLIST BODY...) ...) FORM...)"
    (declare (indent 1) (debug cl-flet))
    `(letf ,(mapcar
             (lambda (x)
               (if (or (and (fboundp (car x))
                            (eq (car-safe (symbol-function (car x))) 'macro))
                       (cdr (assq (car x) macroexpand-all-environment)))
                   (error "Use `labels', not `dflet', to rebind macro names"))
               (let ((func `(cl-function
                             (lambda ,(cadr x)
                               (cl-block ,(car x) ,@(cddr x))))))
                 (when (cl--compiling-file)
                   ;; Bug#411.  It would be nice to fix this.
                   (and (get (car x) 'byte-compile)
                        (error "Byte-compiling a redefinition of `%s' \
will not work - use `labels' instead" (symbol-name (car x))))
                   ;; FIXME This affects the rest of the file, when it
                   ;; should be restricted to the flet body.
                   (and (boundp 'byte-compile-function-environment)
                        (push (cons (car x) (eval func))
                              byte-compile-function-environment)))
                 (list `(symbol-function ',(car x)) func)))
             bindings)
       ,@body)))

;;;###autoload
(autoload 'dflet "dflet")

(provide 'dflet)
;;; dflet.el ends here
