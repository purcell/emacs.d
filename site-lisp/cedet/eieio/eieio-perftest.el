;;; eieio-perftest.el --- Performance tests

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: eieio-perftest.el,v 1.3 2008/12/15 00:55:08 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Miscelaneous performance tests

;;; Code:

(defclass eieio-perftest-1 ()
  ((slot1 :initarg :slot1)
   (slottype :initarg :slottype
	     :type number)
   )
  "Baseclass for some performance tests.")

(defclass eieio-perftest-2 (eieio-perftest-1)
  ((slot3 :initarg :slot3)
   )
  "Subclass for some performance tests.")

(defmethod eieio-perftest-meth-1 ((this eieio-perftest-1))
  "Performance test method."
  (+ 1 1))

(defmethod eieio-perftest-meth-1 ((this eieio-perftest-2))
  "Performance test method."
  (+ 1 (call-next-method)))

(defmethod eieio-perftest-meth-2 ((this eieio-perftest-1))
  "Performance test method."
  (+ 1 1))

;;; Timing Functions
;;
;;;###autoload
(defun eieio-perftest-methodcall ()
  "Test and time performance of method invocation."
  (interactive)
  (let ((end nil) (start nil)
	(idx 0)
	(one (eieio-perftest-2 "test"))
	(gen nil)
	(prim nil)
	)
    (eieio-defgeneric-reset-generic-form 'eieio-perftest-meth-1)
    (setq start (current-time))
    (while (> 10000 idx)
      (assert (= (eieio-perftest-meth-1 one) 3))
      (setq idx (1+ idx)))
    (setq end (current-time))
    (setq gen (semantic-elapsed-time start end))

    (eieio-defgeneric-reset-generic-form-primary-only 'eieio-perftest-meth-1)
    (setq start (current-time))
    (setq idx 0)
    (while (> 10000 idx)
      (assert (= (eieio-perftest-meth-1 one) 3))
      (setq idx (1+ idx)))
    (setq end (current-time))
    (setq prim (semantic-elapsed-time start end))

    (let ((pcentf (* 100.0 (- 1 (/ prim gen))))
	  (pcents (* 100.0 (- 1 (/ gen prim))))
	  )

      (message "Generic: %1.4f  Primaryonly: %1.4f sec is %1.2f%% %s"
	       gen prim
	       (if (> gen prim) pcentf pcents)
	       (if (> gen prim) "faster" "slower")))
    
    ))

;;;###autoload
(defun eieio-perftest-onemethodcall ()
  "Test and time performance of method invocation."
  (interactive)
  (let ((end nil) (start nil)
	(idx 0)
	(two (eieio-perftest-2 "test"))
	(gen nil)
	(prim nil)
	(one nil)
	)
    (eieio-defgeneric-reset-generic-form 'eieio-perftest-meth-2)
    (setq start (current-time))
    (while (> 20000 idx)
      (assert (= (eieio-perftest-meth-2 two) 2))
      (setq idx (1+ idx)))
    (setq end (current-time))
    (setq gen (semantic-elapsed-time start end))

    (eieio-defgeneric-reset-generic-form-primary-only 'eieio-perftest-meth-2)
    (setq start (current-time))
    (setq idx 0)
    (while (> 20000 idx)
      (assert (= (eieio-perftest-meth-2 two) 2))
      (setq idx (1+ idx)))
    (setq end (current-time))
    (setq prim (semantic-elapsed-time start end))

    (eieio-defgeneric-reset-generic-form-primary-only-one 'eieio-perftest-meth-2)
    (setq start (current-time))
    (setq idx 0)
    (while (> 20000 idx)
      (assert (= (eieio-perftest-meth-2 two) 2))
      (setq idx (1+ idx)))
    (setq end (current-time))
    (setq one (semantic-elapsed-time start end))

    (let ((pcentf (* 100.0 (- 1 (/ prim gen))))
	  (pcents (* 100.0 (- 1 (/ gen prim))))
	  (1centf (* 100.0 (- 1 (/ one gen))))
	  (1cents (* 100.0 (- 1 (/ gen one))))
	  )

      (message "Gen: %1.4f  Prim: %1.4f is %1.2f%% %s One: %1.4f is %1.2f%% %s"
	       gen prim
	       (if (> gen prim) pcentf pcents)
	       (if (> gen prim) "faster" "slower")
	       one
	       (if (> gen one) 1centf 1cents)
	       (if (> gen one) "faster" "slower")
	       ))
    
    ))

(provide 'eieio-perftest)
;;; eieio-perftest.el ends here
