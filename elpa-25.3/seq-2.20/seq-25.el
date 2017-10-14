;;; seq-25.el --- seq.el implementation for Emacs 25.x -*- lexical-binding: t -*-

;; Copyright (C) 2014-2016 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: sequences

;; Maintainer: emacs-devel@gnu.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Sequence-manipulation functions that complement basic functions
;; provided by subr.el.
;;
;; All functions are prefixed with "seq-".
;;
;; All provided functions work on lists, strings and vectors.
;;
;; Functions taking a predicate or iterating over a sequence using a
;; function as argument take the function as their first argument and
;; the sequence as their second argument.  All other functions take
;; the sequence as their first argument.
;;
;; seq.el can be extended to support new type of sequences.  Here are
;; the generic functions that must be implemented by new seq types:
;; - `seq-elt'
;; - `seq-length'
;; - `seq-do'
;; - `seqp'
;; - `seq-subseq'
;; - `seq-into-sequence'
;; - `seq-copy'
;; - `seq-into'

;;; Code:

;; When loading seq.el in Emacs 24.x, this file gets byte-compiled, even if
;; never used.  This takes care of byte-compilation warnings is emitted, by
;; emitting nil in the macro expansion in Emacs 24.x.
(defmacro seq--when-emacs-25-p (&rest body)
  "Execute BODY if in Emacs>=25.x."
  (declare (indent (lambda (&rest x) 0)) (debug t))
  (when (version<= "25" emacs-version)
    `(progn ,@body)))

(seq--when-emacs-25-p

(require 'cl-generic)
(require 'cl-lib) ;; for cl-subseq

(defmacro seq-doseq (spec &rest body)
  "Loop over a sequence.
Evaluate BODY with VAR bound to each element of SEQUENCE, in turn.

Similar to `dolist' but can be applied to lists, strings, and vectors.

\(fn (VAR SEQUENCE) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  `(seq-do (lambda (,(car spec))
             ,@body)
           ,(cadr spec)))

(pcase-defmacro seq (&rest patterns)
  "Build a `pcase' pattern that matches elements of SEQUENCE.

The `pcase' pattern will match each element of PATTERNS against the
corresponding element of SEQUENCE.

Extra elements of the sequence are ignored if fewer PATTERNS are
given, and the match does not fail."
  `(and (pred seqp)
        ,@(seq--make-pcase-bindings patterns)))

(defmacro seq-let (args sequence &rest body)
  "Bind the variables in ARGS to the elements of SEQUENCE, then evaluate BODY.

ARGS can also include the `&rest' marker followed by a variable
name to be bound to the rest of SEQUENCE."
  (declare (indent 2) (debug (sexp form body)))
  `(pcase-let ((,(seq--make-pcase-patterns args) ,sequence))
     ,@body))


;;; Basic seq functions that have to be implemented by new sequence types
(cl-defgeneric seq-elt (sequence n)
  "Return Nth element of SEQUENCE."
  (elt sequence n))

;; Default gv setters for `seq-elt'.
;; It can be a good idea for new sequence implementations to provide a
;; "gv-setter" for `seq-elt'.
(cl-defmethod (setf seq-elt) (store (sequence array) n)
  (aset sequence n store))

(cl-defmethod (setf seq-elt) (store (sequence cons) n)
  (setcar (nthcdr n sequence) store))

(cl-defgeneric seq-length (sequence)
  "Return the number of elements of SEQUENCE."
  (length sequence))

(cl-defgeneric seq-do (function sequence)
  "Apply FUNCTION to each element of SEQUENCE, presumably for side effects.
Return SEQUENCE."
  (mapc function sequence))

(defalias 'seq-each #'seq-do)

(cl-defgeneric seqp (sequence)
  "Return non-nil if SEQUENCE is a sequence, nil otherwise."
  (sequencep sequence))

(cl-defgeneric seq-copy (sequence)
  "Return a shallow copy of SEQUENCE."
  (copy-sequence sequence))

(cl-defgeneric seq-subseq (sequence start &optional end)
  "Return the sequence of elements of SEQUENCE from START to END.
END is exclusive.

If END is omitted, it defaults to the length of the sequence.  If
START or END is negative, it counts from the end.  Signal an
error if START or END are outside of the sequence (i.e too large
if positive or too small if negative)."
  (cl-subseq sequence start end))


(cl-defgeneric seq-map (function sequence)
  "Return the result of applying FUNCTION to each element of SEQUENCE."
  (let (result)
    (seq-do (lambda (elt)
              (push (funcall function elt) result))
            sequence)
    (nreverse result)))

(defun seq-map-indexed (function sequence)
  "Return the result of applying FUNCTION to each element of SEQUENCE.
Unlike `seq-map', FUNCTION takes two arguments: the element of
the sequence, and its index within the sequence."
  (let ((index 0))
    (seq-map (lambda (elt)
               (prog1
                   (funcall function elt index)
                 (setq index (1+ index))))
             sequence)))

;; faster implementation for sequences (sequencep)
(cl-defmethod seq-map (function (sequence sequence))
  (mapcar function sequence))

(cl-defgeneric seq-mapn (function sequence &rest sequences)
  "Like `seq-map' but FUNCTION is mapped over all SEQUENCES.
The arity of FUNCTION must match the number of SEQUENCES, and the
mapping stops on the shortest sequence.
Return a list of the results.

\(fn FUNCTION SEQUENCES...)"
  (let ((result nil)
        (sequences (seq-map (lambda (s)
                              (seq-into s 'list))
                            (cons sequence sequences))))
    (while (not (memq nil sequences))
      (push (apply function (seq-map #'car sequences)) result)
      (setq sequences (seq-map #'cdr sequences)))
    (nreverse result)))

(cl-defgeneric seq-drop (sequence n)
  "Remove the first N elements of SEQUENCE and return the result.
The result is a sequence of the same type as SEQUENCE.

If N is a negative integer or zero, SEQUENCE is returned."
  (if (<= n 0)
      sequence
    (let ((length (seq-length sequence)))
      (seq-subseq sequence (min n length) length))))

(cl-defgeneric seq-take (sequence n)
  "Take the first N elements of SEQUENCE and return the result.
The result is a sequence of the same type as SEQUENCE.

If N is a negative integer or zero, an empty sequence is
returned."
  (seq-subseq sequence 0 (min (max n 0) (seq-length sequence))))

(cl-defgeneric seq-drop-while (pred sequence)
  "Remove the successive elements of SEQUENCE for which PRED returns non-nil.
PRED is a function of one argument.  The result is a sequence of
the same type as SEQUENCE."
  (seq-drop sequence (seq--count-successive pred sequence)))

(cl-defgeneric seq-take-while (pred sequence)
  "Take the successive elements of SEQUENCE for which PRED returns non-nil.
PRED is a function of one argument.  The result is a sequence of
the same type as SEQUENCE."
  (seq-take sequence (seq--count-successive pred sequence)))

(cl-defgeneric seq-empty-p (sequence)
  "Return non-nil if the SEQUENCE is empty, nil otherwise."
  (= 0 (seq-length sequence)))

(cl-defgeneric seq-sort (pred sequence)
  "Sort SEQUENCE using PRED as comparison function.
The result is a sequence of the same type as SEQUENCE."
  (let ((result (seq-sort pred (append sequence nil))))
    (seq-into result (type-of sequence))))

(defun seq-sort-by (function pred sequence)
  "Sort SEQUENCE using PRED as a comparison function.
Elements of SEQUENCE are transformed by FUNCTION before being
sorted.  FUNCTION must be a function of one argument."
  (seq-sort (lambda (a b)
              (funcall pred
                       (funcall function a)
                       (funcall function b)))
            sequence))

(cl-defmethod seq-sort (pred (list list))
  (sort (seq-copy list) pred))

(cl-defgeneric seq-reverse (sequence)
  "Return a sequence with elements of SEQUENCE in reverse order."
  (let ((result '()))
    (seq-map (lambda (elt)
               (push elt result))
             sequence)
    (seq-into result (type-of sequence))))

;; faster implementation for sequences (sequencep)
(cl-defmethod seq-reverse ((sequence sequence))
  (reverse sequence))

(cl-defgeneric seq-concatenate (type &rest sequences)
  "Concatenate SEQUENCES into a single sequence of type TYPE.
TYPE must be one of following symbols: vector, string or list.

\n(fn TYPE SEQUENCE...)"
  (apply #'cl-concatenate type (seq-map #'seq-into-sequence sequences)))

(cl-defgeneric seq-into-sequence (sequence)
  "Convert SEQUENCE into a sequence.

The default implementation is to signal an error if SEQUENCE is not a
sequence, specific functions should be implemented for new types
of sequence."
  (unless (sequencep sequence)
    (error "Cannot convert %S into a sequence" sequence))
  sequence)

(cl-defgeneric seq-into (sequence type)
  "Concatenate the elements of SEQUENCE into a sequence of type TYPE.
TYPE can be one of the following symbols: vector, string or
list."
  (pcase type
    (`vector (seq--into-vector sequence))
    (`string (seq--into-string sequence))
    (`list (seq--into-list sequence))
    (_ (error "Not a sequence type name: %S" type))))

(cl-defgeneric seq-filter (pred sequence)
  "Return a list of all the elements for which (PRED element) is non-nil in SEQUENCE."
  (let ((exclude (make-symbol "exclude")))
    (delq exclude (seq-map (lambda (elt)
                             (if (funcall pred elt)
                                 elt
                               exclude))
                           sequence))))

(cl-defgeneric seq-remove (pred sequence)
  "Return a list of all the elements for which (PRED element) is nil in SEQUENCE."
  (seq-filter (lambda (elt) (not (funcall pred elt)))
              sequence))

(cl-defgeneric seq-reduce (function sequence initial-value)
  "Reduce the function FUNCTION across SEQUENCE, starting with INITIAL-VALUE.

Return the result of calling FUNCTION with INITIAL-VALUE and the
first element of SEQUENCE, then calling FUNCTION with that result and
the second element of SEQUENCE, then with that result and the third
element of SEQUENCE, etc.

If SEQUENCE is empty, return INITIAL-VALUE and FUNCTION is not called."
  (if (seq-empty-p sequence)
      initial-value
    (let ((acc initial-value))
      (seq-doseq (elt sequence)
        (setq acc (funcall function acc elt)))
      acc)))

(cl-defgeneric seq-every-p (pred sequence)
  "Return non-nil if (PRED element) is non-nil for all elements of SEQUENCE."
  (catch 'seq--break
    (seq-doseq (elt sequence)
      (or (funcall pred elt)
          (throw 'seq--break nil)))
    t))

(cl-defgeneric seq-some (pred sequence)
  "Return the first value for which if (PRED element) is non-nil for in SEQUENCE."
  (catch 'seq--break
    (seq-doseq (elt sequence)
      (let ((result (funcall pred elt)))
        (when result
          (throw 'seq--break result))))
    nil))

(cl-defgeneric seq-find (pred sequence &optional default)
  "Return the first element for which (PRED element) is non-nil in SEQUENCE.
If no element is found, return DEFAULT.

Note that `seq-find' has an ambiguity if the found element is
identical to DEFAULT, as it cannot be known if an element was
found or not."
  (catch 'seq--break
    (seq-doseq (elt sequence)
      (when (funcall pred elt)
        (throw 'seq--break elt)))
    default))

(cl-defgeneric seq-count (pred sequence)
  "Return the number of elements for which (PRED element) is non-nil in SEQUENCE."
  (let ((count 0))
    (seq-doseq (elt sequence)
      (when (funcall pred elt)
        (setq count (+ 1 count))))
    count))

(cl-defgeneric seq-contains (sequence elt &optional testfn)
  "Return the first element in SEQUENCE that is equal to ELT.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-some (lambda (e)
              (funcall (or testfn #'equal) elt e))
            sequence))

(cl-defgeneric seq-set-equal-p (sequence1 sequence2 &optional testfn)
  "Return non-nil if SEQUENCE1 and SEQUENCE2 contain the same elements, regardless of order.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (and (seq-every-p (lambda (item1) (seq-contains sequence2 item1 testfn)) sequence1)
       (seq-every-p (lambda (item2) (seq-contains sequence1 item2 testfn)) sequence2)))

(cl-defgeneric seq-position (sequence elt &optional testfn)
  "Return the index of the first element in SEQUENCE that is equal to ELT.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (let ((index 0))
    (catch 'seq--break
      (seq-doseq (e sequence)
        (when (funcall (or testfn #'equal) e elt)
          (throw 'seq--break index))
        (setq index (1+ index)))
      nil)))

(cl-defgeneric seq-uniq (sequence &optional testfn)
  "Return a list of the elements of SEQUENCE with duplicates removed.
TESTFN is used to compare elements, or `equal' if TESTFN is nil."
  (let ((result '()))
    (seq-doseq (elt sequence)
      (unless (seq-contains result elt testfn)
        (setq result (cons elt result))))
    (nreverse result)))

(cl-defgeneric seq-mapcat (function sequence &optional type)
  "Concatenate the result of applying FUNCTION to each element of SEQUENCE.
The result is a sequence of type TYPE, or a list if TYPE is nil."
  (apply #'seq-concatenate (or type 'list)
         (seq-map function sequence)))

(cl-defgeneric seq-partition (sequence n)
  "Return a list of the elements of SEQUENCE grouped into sub-sequences of length N.
The last sequence may contain less than N elements.  If N is a
negative integer or 0, nil is returned."
  (unless (< n 1)
    (let ((result '()))
      (while (not (seq-empty-p sequence))
        (push (seq-take sequence n) result)
        (setq sequence (seq-drop sequence n)))
      (nreverse result))))

(cl-defgeneric seq-intersection (sequence1 sequence2 &optional testfn)
  "Return a list of the elements that appear in both SEQUENCE1 and SEQUENCE2.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-reduce (lambda (acc elt)
                (if (seq-contains sequence2 elt testfn)
                    (cons elt acc)
                  acc))
              (seq-reverse sequence1)
              '()))

(cl-defgeneric seq-difference (sequence1 sequence2 &optional testfn)
  "Return a list of the elements that appear in SEQUENCE1 but not in SEQUENCE2.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-reduce (lambda (acc elt)
                (if (not (seq-contains sequence2 elt testfn))
                    (cons elt acc)
                  acc))
              (seq-reverse sequence1)
              '()))

(cl-defgeneric seq-group-by (function sequence)
  "Apply FUNCTION to each element of SEQUENCE.
Separate the elements of SEQUENCE into an alist using the results as
keys.  Keys are compared using `equal'."
  (seq-reduce
   (lambda (acc elt)
     (let* ((key (funcall function elt))
            (cell (assoc key acc)))
       (if cell
           (setcdr cell (push elt (cdr cell)))
         (push (list key elt) acc))
       acc))
   (seq-reverse sequence)
   nil))

(cl-defgeneric seq-min (sequence)
  "Return the smallest element of SEQUENCE.
SEQUENCE must be a sequence of numbers or markers."
  (apply #'min (seq-into sequence 'list)))

(cl-defgeneric seq-max (sequence)
  "Return the largest element of SEQUENCE.
SEQUENCE must be a sequence of numbers or markers."
  (apply #'max (seq-into sequence 'list)))

(defun seq--count-successive (pred sequence)
  "Return the number of successive elements for which (PRED element) is non-nil in SEQUENCE."
  (let ((n 0)
        (len (seq-length sequence)))
    (while (and (< n len)
                (funcall pred (seq-elt sequence n)))
      (setq n (+ 1 n)))
    n))

;;; Optimized implementations for lists

(cl-defmethod seq-drop ((list list) n)
  "Optimized implementation of `seq-drop' for lists."
  (nthcdr n list))

(cl-defmethod seq-take ((list list) n)
  "Optimized implementation of `seq-take' for lists."
  (let ((result '()))
    (while (and list (> n 0))
      (setq n (1- n))
      (push (pop list) result))
    (nreverse result)))

(cl-defmethod seq-drop-while (pred (list list))
  "Optimized implementation of `seq-drop-while' for lists."
  (while (and list (funcall pred (car list)))
    (setq list (cdr list)))
  list)

(cl-defmethod seq-empty-p ((list list))
  "Optimized implementation of `seq-empty-p' for lists."
  (null list))


(defun seq--into-list (sequence)
  "Concatenate the elements of SEQUENCE into a list."
  (if (listp sequence)
      sequence
    (append sequence nil)))

(defun seq--into-vector (sequence)
  "Concatenate the elements of SEQUENCE into a vector."
  (if (vectorp sequence)
      sequence
    (vconcat sequence)))

(defun seq--into-string (sequence)
  "Concatenate the elements of SEQUENCE into a string."
  (if (stringp sequence)
      sequence
    (concat sequence)))

(defun seq--make-pcase-bindings (args)
  "Return a list of bindings of the variables in ARGS to the elements of a sequence."
  (let ((bindings '())
        (index 0)
        (rest-marker nil))
    (seq-doseq (name args)
      (unless rest-marker
        (pcase name
          (`&rest
           (progn (push `(app (pcase--flip seq-drop ,index)
                              ,(seq--elt-safe args (1+ index)))
                        bindings)
                  (setq rest-marker t)))
          (_
           (push `(app (pcase--flip seq--elt-safe ,index) ,name) bindings))))
      (setq index (1+ index)))
    bindings))

(defun seq--make-pcase-patterns (args)
  "Return a list of `(seq ...)' pcase patterns from the argument list ARGS."
  (cons 'seq
        (seq-map (lambda (elt)
                   (if (seqp elt)
                       (seq--make-pcase-patterns elt)
                     elt))
                 args)))

;; TODO: make public?
(defun seq--elt-safe (sequence n)
  "Return element of SEQUENCE at the index N.
If no element is found, return nil."
  (ignore-errors (seq-elt sequence n))))

(cl-defgeneric seq-random-elt (sequence)
  "Return a random element from SEQUENCE.
Signal an error if SEQUENCE is empty."
  (if (seq-empty-p sequence)
      (error "Sequence cannot be empty")
    (seq-elt sequence (random (seq-length sequence)))))

(provide 'seq-25)
;;; seq-25.el ends here
