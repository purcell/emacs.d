;;; eieio-testsinvoke.el -- eieio tests for method invokation

;;;
;; Copyright (C) 2005, 2008 Eric M. Ludlam
;;
;; Author: <zappo@gnu.org>
;; RCS: $Id: eieio-test-methodinvoke.el,v 1.8 2008/09/15 00:21:45 zappo Exp $
;; Keywords: oop, lisp, tools
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.org
;;

;;; Commentary:
;;  
;; Test method invocation order.  From the common lisp reference
;; manual:
;;
;; QUOTE:
;; - All the :before methods are called, in most-specific-first
;;   order.  Their values are ignored.  An error is signaled if
;;   call-next-method is used in a :before method.
;;
;; - The most specific primary method is called. Inside the body of a
;;   primary method, call-next-method may be used to call the next
;;   most specific primary method. When that method returns, the
;;   previous primary method can execute more code, perhaps based on
;;   the returned value or values. The generic function no-next-method
;;   is invoked if call-next-method is used and there are no more
;;   applicable primary methods. The function next-method-p may be
;;   used to determine whether a next method exists. If
;;   call-next-method is not used, only the most specific primary
;;   method is called.
;;   
;; - All the :after methods are called, in most-specific-last order.
;;   Their values are ignored.  An error is signaled if
;;   call-next-method is used in a :after method.
;;

(defvar eieio-test-method-order-list nil
  "List of symbols stored during method invocation.")

(defun eieio-test-method-store ()
  "Store current invocation class symbol in the invocation order list."
  (let* ((keysym (aref [ :STATIC :BEFORE :PRIMARY :AFTER ]
		       (or eieio-generic-call-key 0)))
	 (c (list eieio-generic-call-methodname keysym scoped-class)))
    (setq eieio-test-method-order-list
	  (cons c eieio-test-method-order-list))))

(defun eieio-test-match (rightanswer)
  "Do a test match."
  (if (equal rightanswer eieio-test-method-order-list)
      t
    (error "eieio-test-methodinvoke.el: Test Failed!")))

;;; This Example was submitted by darkman:
;;
;; drkm <darkman_spam@yahoo.fr>
(defclass A () ())
(defclass AA (A) ())
(defclass AAA (AA) ())

(defmethod F :BEFORE ((p A))
  (eieio-test-method-store))
(defmethod F :BEFORE ((p AA))
  (eieio-test-method-store))
(defmethod F :BEFORE ((p AAA))
  (eieio-test-method-store))

(defmethod F ((p A))
  (eieio-test-method-store))
(defmethod F ((p AA))
  (eieio-test-method-store))

(defmethod F :AFTER ((p A))
  (eieio-test-method-store))
(defmethod F :AFTER ((p AA))
  (eieio-test-method-store))
(defmethod F :AFTER ((p AAA))
  (eieio-test-method-store))

(let ((eieio-test-method-order-list nil)
      (ans '(
	     (F :BEFORE AAA)
	     (F :BEFORE AA)
	     (F :BEFORE A)
	     ;; Not primary A method
	     (F :PRIMARY AA)
	     ;; No call-next-method in AA to get to A.
	     (F :AFTER A)
	     (F :AFTER AA)
	     (F :AFTER AAA)
	     )))
  (F (AAA nil))
  (setq eieio-test-method-order-list (nreverse eieio-test-method-order-list))
  (eieio-test-match ans))

(defmethod G :BEFORE ((p A))
  (eieio-test-method-store))
(defmethod G :BEFORE ((p AAA))
  (eieio-test-method-store))

(defmethod G ((p A))
  (eieio-test-method-store))

(defmethod G :AFTER ((p A))
  (eieio-test-method-store))
(defmethod G :AFTER ((p AAA))
  (eieio-test-method-store))


(let ((eieio-test-method-order-list nil)
      (ans '(
	     (G :BEFORE AAA)
	     (G :BEFORE A)
	     ;; Not primary A method
	     (G :PRIMARY A)
	     ;; No call-next-method in AA to get to A.
	     (G :AFTER A)
	     (G :AFTER AAA)
	     )))
  (G (AAA nil))
  (setq eieio-test-method-order-list (nreverse eieio-test-method-order-list))
  (eieio-test-match ans))

;;; Test Multiple Inheritance.
;;
(defclass B-base1 () ())
(defclass B-base2 () ())
(defclass B (B-base1 B-base2) ())

(defmethod F :BEFORE ((p B-base1))
  (eieio-test-method-store))

(defmethod F :BEFORE ((p B-base2))
  (eieio-test-method-store))

(defmethod F :BEFORE ((p B))
  (eieio-test-method-store))

(defmethod F ((p B))
  (eieio-test-method-store)
  (call-next-method))

(defmethod F ((p B-base1))
  (eieio-test-method-store)
  (call-next-method))

(defmethod F ((p B-base2))
  (eieio-test-method-store)
  (when (next-method-p)
    (call-next-method))
  )

(defmethod F :AFTER ((p B-base1))
  (eieio-test-method-store))

(defmethod F :AFTER ((p B-base2))
  (eieio-test-method-store))

(defmethod F :AFTER ((p B))
  (eieio-test-method-store))

(let ((eieio-test-method-order-list nil)
      (ans '(
	     (F :BEFORE B)
	     (F :BEFORE B-base1)
	     (F :BEFORE B-base2)

	     (F :PRIMARY B)
	     (F :PRIMARY B-base1)
	     (F :PRIMARY B-base2)

	     (F :AFTER B-base2)
	     (F :AFTER B-base1)
	     (F :AFTER B)
	     )))
  (F (B nil))
  (setq eieio-test-method-order-list (nreverse eieio-test-method-order-list))
  ;;(message "%S" eieio-test-method-order-list)
  (eieio-test-match ans)
  )

;;; Test static invokation
;;
(defmethod H :STATIC ((class A))
  "No need to do work in here."
  'moose)

;; Both of these situations should succeed.
(H A)
(H (A nil))

;;; Return value from :PRIMARY
;;
(defmethod I :BEFORE ((a A))
  (eieio-test-method-store)
  ":before")

(defmethod I :PRIMARY ((a A))
  (eieio-test-method-store)
  ":primary")

(defmethod I :AFTER ((a A))
  (eieio-test-method-store)
  ":after")

(let ((eieio-test-method-order-list nil)
      (ans  (I (A nil))))
  (unless (string= ans ":primary")
    (error "Value %S erroneously provided in method call."
	   ans)))

;;; Multiple inheritance and the 'constructor' method.
;;
;; Constructor is a static method, so this is really testing
;; static method invocation and multiple inheritance.
;;
(defclass C-base1 () ())
(defclass C-base2 () ())
(defclass C (C-base1 C-base2) ())

(defmethod constructor :STATIC ((p C-base1) &rest args)
  (eieio-test-method-store)
  (if (next-method-p) (call-next-method))
  )

(defmethod constructor :STATIC ((p C-base2) &rest args)
  (eieio-test-method-store)
  (if (next-method-p) (call-next-method))
  )

(defmethod constructor :STATIC ((p C) &rest args)
  (eieio-test-method-store)
  (call-next-method)
  )

(let ((eieio-test-method-order-list nil)
      (ans '(
	     (constructor :STATIC C)
	     (constructor :STATIC C-base1)
	     (constructor :STATIC C-base2)
	     )))
  (C nil)
  (setq eieio-test-method-order-list (nreverse eieio-test-method-order-list))
  (eieio-test-match ans)
  )

;;; Diamond Test
;; 
;; For a diamond shaped inheritance structure, (call-next-method) can break.
;; As such, there are two possible orders.

(defclass D-base0 () () :method-invocation-order :depth-first)
(defclass D-base1 (D-base0) () :method-invocation-order :depth-first)
(defclass D-base2 (D-base0) () :method-invocation-order :depth-first)
(defclass D (D-base1 D-base2) () :method-invocation-order :depth-first)

(defmethod F ((p D))
  "D"
  (eieio-test-method-store)
  (call-next-method))

(defmethod F ((p D-base0))
  "D-base0"
  (eieio-test-method-store)
  ;; This should have no next
  ;; (when (next-method-p) (call-next-method))
  )

(defmethod F ((p D-base1))
  "D-base1"
  (eieio-test-method-store)
  (call-next-method))

(defmethod F ((p D-base2))
  "D-base2"
  (eieio-test-method-store)
  (when (next-method-p)
    (call-next-method))
  )

(let ((eieio-test-method-order-list nil)
      (ans '(
	     (F :PRIMARY D)
	     (F :PRIMARY D-base1)
	     ;; (F :PRIMARY D-base2)
	     (F :PRIMARY D-base0)
	     )))
  (F (D nil))
  (setq eieio-test-method-order-list (nreverse eieio-test-method-order-list))
  (message "%S" eieio-test-method-order-list)
  (eieio-test-match ans)
  )

;;; Other invocation order

(defclass E-base0 () () :method-invocation-order :breadth-first)
(defclass E-base1 (E-base0) () :method-invocation-order :breadth-first)
(defclass E-base2 (E-base0) () :method-invocation-order :breadth-first)
(defclass E (E-base1 E-base2) () :method-invocation-order :breadth-first)

(defmethod F ((p E))
  (eieio-test-method-store)
  (call-next-method))

(defmethod F ((p E-base0))
  (eieio-test-method-store)
  ;; This should have no next
  ;; (when (next-method-p) (call-next-method))
  )

(defmethod F ((p E-base1))
  (eieio-test-method-store)
  (call-next-method))

(defmethod F ((p E-base2))
  (eieio-test-method-store)
  (when (next-method-p)
    (call-next-method))
  )

(let ((eieio-test-method-order-list nil)
      (ans '(
	     (F :PRIMARY E)
	     (F :PRIMARY E-base1)
	     (F :PRIMARY E-base2)
	     (F :PRIMARY E-base0)
	     )))
  (F (E nil))
  (setq eieio-test-method-order-list (nreverse eieio-test-method-order-list))
  (message "%S" eieio-test-method-order-list)
  (eieio-test-match ans)
  )
