;;; cl-lib.el --- Properly prefixed CL functions and macros  -*- coding: utf-8 -*-

;; Copyright (C) 2012, 2013, 2014  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; vcomment: Emacs-24.3's version is 1.0 so this has to stay below.
;; Version: 0.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a forward compatibility package, which provides (a subset of) the
;; features of the cl-lib package introduced in Emacs-24.3, for use on
;; previous emacsen.

;; Make sure this is installed *late* in your `load-path`, i.e. after Emacs's
;; built-in .../lisp/emacs-lisp directory, so that if/when you upgrade to
;; Emacs-24.3, the built-in version of the file will take precedence, otherwise
;; you could get into trouble (although we try to hack our way around the
;; problem in case it happens).

;; This code is largely copied from Emacs-24.3's cl.el, with the alias bindings
;; simply reversed.

;;; Code:

;; We need to handle the situation where this package is used with an Emacs
;; that comes with a real cl-lib (i.e. ≥24.3).

;; First line of defense: try to make sure the built-in cl-lib comes earlier in
;; load-path so we never get loaded:
;;;###autoload (let ((d (file-name-directory #$)))
;;;###autoload   (when (member d load-path)
;;;###autoload     (setq load-path (append (remove d load-path) (list d)))))

(when (functionp 'macroexp--compiler-macro)
  ;; `macroexp--compiler-macro' was introduced as part of the big CL
  ;; reorganization which moved/reimplemented some of CL into core (mostly the
  ;; setf and compiler-macro support), so its presence indicates we're running
  ;; in an Emacs that comes with the new cl-lib.el, where this file should
  ;; never be loaded!
  (message "Real cl-lib shadowed by compatibility cl-lib? (%s)" load-file-name)
  (when load-file-name
    ;; (message "Let's try to patch things up")
    (let ((loaddir (file-name-directory load-file-name))
          load-path-dir)
      ;; Find the problematic directory from load-path.
      (dolist (dir load-path)
        (if (equal loaddir (expand-file-name (file-name-as-directory dir)))
            (setq load-path-dir dir)))
      (when load-path-dir
        ;; (message "Let's move the offending dir to the end")
        (setq load-path (append (remove load-path-dir load-path)
                                (list load-path-dir)))
        ;; Here we could manually load cl-lib and then return immediately.
        ;; But Emacs currently doesn't provide any way for a file to "return
        ;; immediately", so instead we make sure the rest of the file does not
        ;; throw away any pre-existing definition.
        ))))

(require 'cl)

;; Some of Emacs-24.3's cl.el definition are not just aliases, because either
;; the feature was dropped from cl-lib.el or because the cl-lib version is
;; not fully compatible.
;; Let's just not include them here, since it is very important that if code
;; works with this cl-lib.el it should also work with Emacs-24.3's cl-lib.el,
;; whereas the reverse is much less important.

(dolist (var '(
               ;; loop-result-var
               ;; loop-result
               ;; loop-initially
               ;; loop-finally
               ;; loop-bindings
               ;; loop-args
               ;; bind-inits
               ;; bind-block
               ;; lambda-list-keywords
               float-negative-epsilon
               float-epsilon
               least-negative-normalized-float
               least-positive-normalized-float
               least-negative-float
               least-positive-float
               most-negative-float
               most-positive-float
               ;; custom-print-functions
               ))
  (let ((new (intern (format "cl-%s" var))))
    (unless (boundp new) (defvaralias new var))))

;; The following cl-lib functions were already defined in the old cl.el,
;; with a different meaning:
;; - cl-position and cl-delete-duplicates
;;   the two meanings are clearly different, but we can distinguish which was
;;   meant by looking at the arguments.
;; - cl-member
;;   the old meaning hasn't been used for a long time and is a subset of the
;;   new, so we can simply override it.
;; - cl-adjoin
;;   the old meaning is actually the same as the new except for optimizations.

(dolist (fun '(
               (get* . cl-get)
               (random* . cl-random)
               (rem* . cl-rem)
               (mod* . cl-mod)
               (round* . cl-round)
               (truncate* . cl-truncate)
               (ceiling* . cl-ceiling)
               (floor* . cl-floor)
               (rassoc* . cl-rassoc)
               (assoc* . cl-assoc)
               ;; (member* . cl-member) ;Handle specially below.
               (delete* . cl-delete)
               (remove* . cl-remove)
               (defsubst* . cl-defsubst)
               (sort* . cl-sort)
               (function* . cl-function)
               (defmacro* . cl-defmacro)
               (defun* . cl-defun)
               (mapcar* . cl-mapcar)

               remprop
               getf
               tailp
               list-length
               nreconc
               revappend
               concatenate
               subseq
               random-state-p
               make-random-state
               signum
               isqrt
               lcm
               gcd
               notevery
               notany
               every
               some
               mapcon
               mapcan
               mapl
               maplist
               map
               equalp
               coerce
               tree-equal
               nsublis
               sublis
               nsubst-if-not
               nsubst-if
               nsubst
               subst-if-not
               subst-if
               subsetp
               nset-exclusive-or
               set-exclusive-or
               nset-difference
               set-difference
               nintersection
               intersection
               nunion
               union
               rassoc-if-not
               rassoc-if
               assoc-if-not
               assoc-if
               member-if-not
               member-if
               merge
               stable-sort
               search
               mismatch
               count-if-not
               count-if
               count
               position-if-not
               position-if
               ;; position ;Handle specially via defadvice below.
               find-if-not
               find-if
               find
               nsubstitute-if-not
               nsubstitute-if
               nsubstitute
               substitute-if-not
               substitute-if
               substitute
               ;; delete-duplicates ;Handle specially via defadvice below.
               remove-duplicates
               delete-if-not
               delete-if
               remove-if-not
               remove-if
               replace
               fill
               reduce
               compiler-macroexpand
               define-compiler-macro
               assert
               check-type
               typep
               deftype
               defstruct
               callf2
               callf
               letf*
               letf
               rotatef
               shiftf
               remf
               psetf
               declare
               the
               locally
               multiple-value-setq
               multiple-value-bind
               symbol-macrolet
               macrolet
               progv
               psetq
               do-all-symbols
               do-symbols
               dotimes
               dolist
               do*
               do
               loop
               return-from
               return
               block
               etypecase
               typecase
               ecase
               case
               load-time-value
               eval-when
               destructuring-bind
               gentemp
               gensym
               pairlis
               acons
               subst
               ;; adjoin ;It's already defined.
               copy-list
               ldiff
               list*
               cddddr
               cdddar
               cddadr
               cddaar
               cdaddr
               cdadar
               cdaadr
               cdaaar
               cadddr
               caddar
               cadadr
               cadaar
               caaddr
               caadar
               caaadr
               caaaar
               cdddr
               cddar
               cdadr
               cdaar
               caddr
               cadar
               caadr
               caaar
               tenth
               ninth
               eighth
               seventh
               sixth
               fifth
               fourth
               third
               endp
               rest
               second
               first
               svref
               copy-seq
               evenp
               oddp
               minusp
               plusp
               floatp-safe
               declaim
               proclaim
               nth-value
               multiple-value-call
               multiple-value-apply
               multiple-value-list
               values-list
               values
               pushnew
               decf
               incf

               dolist
               dotimes
               ))
  (let ((new (if (consp fun) (prog1 (cdr fun) (setq fun (car fun)))
               (intern (format "cl-%s" fun)))))
    (if (fboundp new)
        (unless (or (eq (symbol-function new) fun)
                    (eq new (and (symbolp fun) (fboundp fun)
                                 (symbol-function fun))))
          (message "%S already defined, not rebinding" new))
      (defalias new fun))))

(unless (symbolp (symbol-function 'position))
  (autoload 'cl-position "cl-seq")
  (defadvice cl-position (around cl-lib (cl-item cl-seq &rest cl-keys) activate)
  (let ((argk (ad-get-args 2)))
    (if (or (null argk) (keywordp (car argk)))
        ;; This is a call to cl-lib's `cl-position'.
        (setq ad-return-value
              (apply #'position (ad-get-arg 0) (ad-get-arg 1) argk))
      ;; Must be a call to cl's old `cl-position'.
      ad-do-it))))

(unless (symbolp (symbol-function 'delete-duplicates))
  (autoload 'cl-delete-duplicates "cl-seq")
  (defadvice cl-delete-duplicates (around cl-lib (cl-seq &rest cl-keys) activate)
  (let ((argk (ad-get-args 1)))
    (if (or (null argk) (keywordp (car argk)))
        ;; This is a call to cl-lib's `cl-delete-duplicates'.
        (setq ad-return-value
              (apply #'delete-duplicates (ad-get-arg 0) argk))
      ;; Must be a call to cl's old `cl-delete-duplicates'.
      ad-do-it))))

(when (or (not (fboundp 'cl-member))
          (eq (symbol-function 'cl-member) #'memq))
  (defalias 'cl-member #'member*))

;; `cl-labels' is not 100% compatible with `labels' when using dynamic scoping
;; (mostly because it does not turn lambdas that refer to those functions into
;; closures).  OTOH it is compatible when using lexical scoping.

(unless (fboundp 'cl-labels)
  (defmacro cl-labels (&rest args)
    (unless (and (boundp 'lexical-binding) lexical-binding)
      ;; We used to signal an error rather than a message, but in many uses of
      ;; cl-labels, the value of lexical-binding doesn't actually matter.
      ;; More importantly, the value of `lexical-binding' here is unreliable
      ;; (it does not necessarily reflect faithfully whether the output of this
      ;; macro will be interpreted as lexically bound code or not).
      (message "This `cl-labels' requires `lexical-binding' to be non-nil"))
    `(labels ,@args)))

;;;; ChangeLog:

;; 2014-02-25  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Fixes: debbugs:16671
;; 
;; 	* cl-lib.el (cl-position, cl-delete-duplicate): Don't advise if >=24.3.
;; 	(load-path): Try to make sure we're at the end.
;; 
;; 2014-01-25  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* cl-lib.el: Resolve conflicts with old internal definitions
;; 	(bug#16353).
;; 	(dolist fun): Don't skip definitions silently.
;; 	(define-setf-expander): Remove, not in cl-lib.
;; 	(cl-position, cl-delete-duplicates): Add advice to distinguish the use
;; 	case.
;; 	(cl-member): Override old definition.
;; 
;; 2013-05-22  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* cl-lib.el (cl-labels): Demote error to message and improve it.
;; 
;; 2012-11-30  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* cl-lib.el: Try and patch things up in case we're hiding the real
;; 	cl-lib.
;; 
;; 2012-11-22  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Add cl-letf and cl-labels.
;; 
;; 2012-11-16  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/cl-lib: New package.
;; 


(provide 'cl-lib)
;;; cl-lib.el ends here
