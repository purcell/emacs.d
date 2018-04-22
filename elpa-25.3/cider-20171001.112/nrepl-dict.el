;;; nrepl-dict.el --- Dictionary functions for Clojure nREPL -*- lexical-binding: t -*-

;; Copyright © 2012-2013 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2017 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Provides functions to interact with and create `nrepl-dict's.  These are
;; simply plists with an extra element at the head.

;;; Code:
(require 'cl-lib)


(defun nrepl-dict (&rest key-vals)
  "Create nREPL dict from KEY-VALS."
  (cons 'dict key-vals))

(defun nrepl-dict-p (object)
  "Return t if OBJECT is an nREPL dict."
  (and (listp object)
       (eq (car object) 'dict)))

(defun nrepl-dict-empty-p (dict)
  "Return t if nREPL dict DICT is empty."
  (null (cdr dict)))

(defun nrepl-dict-contains (dict key)
  "Return nil if nREPL dict DICT doesn't contain KEY.
If DICT does contain KEY, then a non-nil value is returned.  Due to the
current implementation, this return value is the tail of DICT's key-list
whose car is KEY.  Comparison is done with `equal'."
  (member key (nrepl-dict-keys dict)))

(defun nrepl-dict-get (dict key &optional default)
  "Get from DICT value associated with KEY, optional DEFAULT if KEY not in DICT.
If dict is nil, return nil.  If DEFAULT not provided, and KEY not in DICT,
return nil.  If DICT is not an nREPL dict object, an error is thrown."
  (when dict
    (if (nrepl-dict-p dict)
        (if (nrepl-dict-contains dict key)
            (lax-plist-get (cdr dict) key)
          default)
      (error "Not an nREPL dict object: %s" dict))))

(defun nrepl-dict-put (dict key value)
  "Associate in DICT, KEY to VALUE.
Return new dict.  Dict is modified by side effects."
  (if (null dict)
      `(dict ,key ,value)
    (if (not (nrepl-dict-p dict))
        (error "Not an nREPL dict object: %s" dict)
      (setcdr dict (lax-plist-put (cdr dict) key value))
      dict)))

(defun nrepl-dict-keys (dict)
  "Return all the keys in the nREPL DICT."
  (if (nrepl-dict-p dict)
      (cl-loop for l on (cdr dict) by #'cddr
               collect (car l))
    (error "Not an nREPL dict")))

(defun nrepl-dict-vals (dict)
  "Return all the values in the nREPL DICT."
  (if (nrepl-dict-p dict)
      (cl-loop for l on (cdr dict) by #'cddr
               collect (cadr l))
    (error "Not an nREPL dict")))

(defun nrepl-dict-map (fn dict)
  "Map FN on nREPL DICT.
FN must accept two arguments key and value."
  (if (nrepl-dict-p dict)
      (cl-loop for l on (cdr dict) by #'cddr
               collect (funcall fn (car l) (cadr l)))
    (error "Not an nREPL dict")))

(defun nrepl-dict-merge (dict1 dict2)
  "Destructively merge DICT2 into DICT1.
Keys in DICT2 override those in DICT1."
  (let ((base (or dict1 '(dict))))
    (nrepl-dict-map (lambda (k v)
                      (nrepl-dict-put base k v))
                    (or dict2 '(dict)))
    base))

(defun nrepl-dict-get-in (dict keys)
  "Return the value in a nested DICT.
KEYS is a list of keys.  Return nil if any of the keys is not present or if
any of the values is nil."
  (let ((out dict))
    (while (and keys out)
      (setq out (nrepl-dict-get out (pop keys))))
    out))

(defun nrepl-dict-flat-map (function dict)
  "Map FUNCTION over DICT and flatten the result.
FUNCTION follows the same restrictions as in `nrepl-dict-map', and it must
also alway return a sequence (since the result will be flattened)."
  (when dict
    (apply #'append (nrepl-dict-map function dict))))


;;; More specific functions
(defun nrepl--cons (car list-or-dict)
  "Generic cons of CAR to LIST-OR-DICT."
  (if (eq (car list-or-dict) 'dict)
      (cons 'dict (cons car (cdr list-or-dict)))
    (cons car list-or-dict)))

(defun nrepl--nreverse (list-or-dict)
  "Generic `nreverse' which works on LIST-OR-DICT."
  (if (eq (car list-or-dict) 'dict)
      (cons 'dict (nreverse (cdr list-or-dict)))
    (nreverse list-or-dict)))

(defun nrepl--push (obj stack)
  "Cons OBJ to the top element of the STACK."
  ;; stack is assumed to be a list
  (if (eq (caar stack) 'dict)
      (cons (cons 'dict (cons obj (cdar stack)))
            (cdr stack))
    (cons (if (null stack)
              obj
            (cons obj (car stack)))
          (cdr stack))))

(defun nrepl--merge (dict1 dict2 &optional no-join)
  "Join nREPL dicts DICT1 and DICT2 in a meaningful way.
String values for non \"id\" and \"session\" keys are concatenated. Lists
are appended. nREPL dicts merged recursively. All other objects are
accumulated into a list. DICT1 is modified destructively and
then returned.
If NO-JOIN is given, return the first non nil dict."
  (if no-join
      (or dict1 dict2)
    (cond ((null dict1) dict2)
          ((null dict2) dict1)
          ((stringp dict1) (concat dict1 dict2))
          ((nrepl-dict-p dict1)
           (nrepl-dict-map
            (lambda (k2 v2)
              (nrepl-dict-put dict1 k2
                              (nrepl--merge (nrepl-dict-get dict1 k2) v2
                                            (member k2 '("id" "session")))))
            dict2)
           dict1)
          ((and (listp dict2) (listp dict1)) (append dict1 dict2))
          ((listp dict1) (append dict1 (list dict2)))
          (t `(,dict1 ,dict2)))))


;;; Dbind
(defmacro nrepl-dbind-response (response keys &rest body)
  "Destructure an nREPL RESPONSE dict.
Bind the value of the provided KEYS and execute BODY."
  (declare (debug (form (&rest symbolp) body)))
  `(let ,(cl-loop for key in keys
                  collect `(,key (nrepl-dict-get ,response ,(format "%s" key))))
     ,@body))
(put 'nrepl-dbind-response 'lisp-indent-function 2)

(provide 'nrepl-dict)

;;; nrepl-dict.el ends here
