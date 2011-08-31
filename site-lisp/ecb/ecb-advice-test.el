;;; ecb-advice-test.el --- test-lib for the advice backbone of ECB

;; Copyright (C) 2000 - 2009 Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author:  Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2009

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: ecb-advice-test.el,v 1.2 2009/05/15 15:19:53 berndl Exp $

;;; Commentary:
;;
;; Contains a test-suite for the advice-backbone of ECB.
;;
;; This file is part of the ECB package which can be found at:
;; http://ecb.sourceforge.net

;;; Usage
;;
;; 1. Load this library into Emacs
;; 2. Call M-x ecb-test-with-original-advice-set
;; 3. Compare the Output in the message-buffer with the expected output at the
;;    end of this file
;; 4. If there are differences then send this output to
;;    the ecb-mailing-list (see Info-manual)

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-util)
(require 'ecb-common-browser)

(defecb-advice-set ecb-advice-test-set
  "An advice-set only for testing the advice-mechanism of ECB")

(defun ecb-advice-test-defun-1 ()
  (message "I'm the ORIGINAL function ecb-advice-test-defun-1"))

(defecb-advice ecb-advice-test-defun-1 around ecb-advice-test-set
  "An advice"
  (message "I'm the AROUND advice of ecb-advice-test-defun-1"))

(defun ecb-advice-test-defun-2 ()
  (message "I'm the ORIGINAL function ecb-advice-test-defun-2"))

(defecb-advice ecb-advice-test-defun-2 before ecb-advice-test-set
  "An advice"
  (message "I'm the BEFORE advice of ecb-advice-test-defun-2"))

(defecb-advice ecb-advice-test-defun-2 after ecb-advice-test-set
  "An advice"
  (message "I'm the AFTER advice of ecb-advice-test-defun-2"))

(defun ecb-advice-test-defun-3 ()
  (message "I'm the ORIGINAL function ecb-advice-test-defun-3"))

(defecb-advice ecb-advice-test-defun-3 around ecb-always-disabled-advices
  "An always disabled advice"
  (message "I'm the AROUND advice of (the always disabled) ecb-advice-test-defun-3"))

(defecb-advice ecb-advice-test-defun-3 after ecb-always-disabled-advices
  "An always disabled advice"
  (message "I'm the AFTER advice of (the always disabled) ecb-advice-test-defun-3"))

(defun ecb-advice-test-defun-4 ()
  (message "I'm the ORIGINAL function ecb-advice-test-defun-4"))

(defecb-advice ecb-advice-test-defun-4 around ecb-always-disabled-advices
  "An always disabled advice"
  (message "I'm the AROUND advice of (the always disabled) ecb-advice-test-defun-4"))

(defun ecb-test-with-original-advice-set ()
  (interactive)
  (let ((ecb-advices-debug-error t))
    (unwind-protect
        (progn
          (message "!!! BEGIN ecb-test-with-original-advice-set !!!!")
          (ecb-enable-advices 'ecb-advice-test-set)
          (ecb-disable-advices 'ecb-always-disabled-advices)
          (ecb-advice-test-defun-1)
          (ecb-advice-test-defun-2)
          (ecb-advice-test-defun-3)
          (ecb-advice-test-defun-4)
          (ecb-with-original-adviced-function-set 'ecb-advice-test-set
            (ecb-advice-test-defun-1)
            (message "LOC-0.1 ecb-test-with-original-advice-set")
            (ecb-with-ecb-advice 'ecb-advice-test-defun-3 'around
              (ecb-advice-test-defun-1)
              (ecb-advice-test-defun-2)
              (ecb-advice-test-defun-3)
              (ecb-advice-test-defun-4)
              (ecb-with-ecb-advice 'ecb-advice-test-defun-3 'around
                (ecb-advice-test-defun-3)
                (ecb-advice-test-defun-4)
                (ecb-with-ecb-advice 'ecb-advice-test-defun-3 'after
                  (ecb-advice-test-defun-3)
                  (ecb-advice-test-defun-4)
                  (ecb-with-ecb-advice 'ecb-advice-test-defun-4 'around
                    (ecb-advice-test-defun-3)
                    (ecb-advice-test-defun-4))
                  (message "LOC-0.2 ecb-test-with-original-advice-set")
                  (ecb-advice-test-defun-3)
                  (ecb-advice-test-defun-4))
                (message "LOC-0.3 ecb-test-with-original-advice-set")
                (ecb-advice-test-defun-3)
                (ecb-advice-test-defun-4))
              (message "LOC-0.4 ecb-test-with-original-advice-set")
              (ecb-advice-test-defun-3)
              (ecb-advice-test-defun-4))
            (ecb-advice-test-defun-2)
            (ecb-advice-test-defun-3)
            (ecb-advice-test-defun-4)
            (message "LOC-1 ecb-test-with-original-advice-set")
            (ecb-with-original-adviced-function-set 'ecb-advice-test-set
              (ecb-advice-test-defun-1)
              (ecb-advice-test-defun-2))
            (message "LOC-2 ecb-test-with-original-advice-set")
            (ecb-advice-test-defun-1)
            (ecb-advice-test-defun-2)            
            (message "LOC-3 ecb-test-with-original-advice-set")
            (ecb-with-original-adviced-function-set 'ecb-advice-test-set
              (ecb-advice-test-defun-1)
              (ecb-advice-test-defun-2)
              (message "LOC-4 ecb-test-with-original-advice-set")
              (ecb-with-original-adviced-function-set 'ecb-advice-test-set
                (ecb-advice-test-defun-1)
                (ecb-advice-test-defun-2)))            
            (message "LOC-5 ecb-test-with-original-advice-set")
            )
          (ecb-advice-test-defun-1)
          (ecb-advice-test-defun-2)
          (message "LOC-6 ecb-test-with-original-advice-set"))
      (ecb-disable-advices 'ecb-advice-test-set)
      (ecb-advice-test-defun-1)
      (ecb-advice-test-defun-2)
      (ecb-advice-test-defun-3)
      (ecb-advice-test-defun-4)
      (message "!!! END ecb-test-with-original-advice-set !!!!"))))


;; expected output:

;;  !!! BEGIN ecb-test-with-original-advice-set !!!!
;;  ECB 2.33: debug enabling the advice-set: ecb-advice-test-set
;;  ECB 2.33: debug enabling of 'after' advice ecb-advice-test-defun-2 
;;  ECB 2.33: debug enabling of 'before' advice ecb-advice-test-defun-2 
;;  ECB 2.33: debug enabling of 'around' advice ecb-advice-test-defun-1 
;;  ECB 2.33: debug disabling the advice-set: ecb-always-disabled-advices
;;  ECB 2.33: debug disabling of 'around' advice ecb-advice-test-defun-4 
;;  ECB 2.33: debug disabling of 'after' advice ecb-advice-test-defun-3 
;;  ECB 2.33: debug disabling of 'around' advice ecb-advice-test-defun-3 
;;  ECB 2.33: debug disabling of 'around' advice count-windows 
;;  ECB 2.33: debug disabling of 'around' advice one-window-p 
;;  ECB 2.33: debug disabling of 'around' advice walk-windows 
;;  ECB 2.33: debug calling of 'around' advice ecb-advice-test-defun-1 
;;  I'm the AROUND advice of ecb-advice-test-defun-1
;;  ECB 2.33: debug calling of 'before' advice ecb-advice-test-defun-2 
;;  I'm the BEFORE advice of ecb-advice-test-defun-2
;;  I'm the ORIGINAL function ecb-advice-test-defun-2
;;  ECB 2.33: debug calling of 'after' advice ecb-advice-test-defun-2 
;;  I'm the AFTER advice of ecb-advice-test-defun-2
;;  I'm the ORIGINAL function ecb-advice-test-defun-3
;;  I'm the ORIGINAL function ecb-advice-test-defun-4
;;  ECB 2.33: debug with original advice-set: ecb-advice-test-set - ENTRY
;;  ECB 2.33: debug disabling the advice-set: ecb-advice-test-set
;;  ECB 2.33: debug disabling of 'after' advice ecb-advice-test-defun-2 
;;  ECB 2.33: debug disabling of 'before' advice ecb-advice-test-defun-2 
;;  ECB 2.33: debug disabling of 'around' advice ecb-advice-test-defun-1 
;;  I'm the ORIGINAL function ecb-advice-test-defun-1
;;  LOC-0.1 ecb-test-with-original-advice-set
;;  ECB 2.33: debug with always disabled ecb-advice: around ecb-advice-test-defun-3 - ENTRY
;;  ECB 2.33: debug enabling of 'around' advice ecb-advice-test-defun-3 
;;  I'm the ORIGINAL function ecb-advice-test-defun-1
;;  I'm the ORIGINAL function ecb-advice-test-defun-2
;;  ECB 2.33: debug calling of 'around' advice ecb-advice-test-defun-3 
;;  I'm the AROUND advice of (the always disabled) ecb-advice-test-defun-3
;;  I'm the ORIGINAL function ecb-advice-test-defun-4
;;  ECB 2.33: debug with always disabled ecb-advice: around ecb-advice-test-defun-3 - ENTRY
;;  ECB 2.33: debug calling of 'around' advice ecb-advice-test-defun-3 
;;  I'm the AROUND advice of (the always disabled) ecb-advice-test-defun-3
;;  I'm the ORIGINAL function ecb-advice-test-defun-4
;;  ECB 2.33: debug with always disabled ecb-advice: after ecb-advice-test-defun-3 - ENTRY
;;  ECB 2.33: debug enabling of 'after' advice ecb-advice-test-defun-3 
;;  ECB 2.33: debug calling of 'around' advice ecb-advice-test-defun-3 
;;  I'm the AROUND advice of (the always disabled) ecb-advice-test-defun-3
;;  ECB 2.33: debug calling of 'after' advice ecb-advice-test-defun-3 
;;  I'm the AFTER advice of (the always disabled) ecb-advice-test-defun-3
;;  I'm the ORIGINAL function ecb-advice-test-defun-4
;;  ECB 2.33: debug with always disabled ecb-advice: around ecb-advice-test-defun-4 - ENTRY
;;  ECB 2.33: debug enabling of 'around' advice ecb-advice-test-defun-4 
;;  ECB 2.33: debug calling of 'around' advice ecb-advice-test-defun-3 
;;  I'm the AROUND advice of (the always disabled) ecb-advice-test-defun-3
;;  ECB 2.33: debug calling of 'after' advice ecb-advice-test-defun-3 
;;  I'm the AFTER advice of (the always disabled) ecb-advice-test-defun-3
;;  ECB 2.33: debug calling of 'around' advice ecb-advice-test-defun-4 
;;  I'm the AROUND advice of (the always disabled) ecb-advice-test-defun-4
;;  ECB 2.33: debug disabling of 'around' advice ecb-advice-test-defun-4 
;;  ECB 2.33: debug with always disabled ecb-advice: around ecb-advice-test-defun-4 - EXIT
;;  LOC-0.2 ecb-test-with-original-advice-set
;;  ECB 2.33: debug calling of 'around' advice ecb-advice-test-defun-3 
;;  I'm the AROUND advice of (the always disabled) ecb-advice-test-defun-3
;;  ECB 2.33: debug calling of 'after' advice ecb-advice-test-defun-3 
;;  I'm the AFTER advice of (the always disabled) ecb-advice-test-defun-3
;;  I'm the ORIGINAL function ecb-advice-test-defun-4
;;  ECB 2.33: debug disabling of 'after' advice ecb-advice-test-defun-3 
;;  ECB 2.33: debug with always disabled ecb-advice: after ecb-advice-test-defun-3 - EXIT
;;  LOC-0.3 ecb-test-with-original-advice-set
;;  ECB 2.33: debug calling of 'around' advice ecb-advice-test-defun-3 
;;  I'm the AROUND advice of (the always disabled) ecb-advice-test-defun-3
;;  I'm the ORIGINAL function ecb-advice-test-defun-4
;;  ECB 2.33: debug with always disabled ecb-advice: around ecb-advice-test-defun-3 - EXIT
;;  LOC-0.4 ecb-test-with-original-advice-set
;;  ECB 2.33: debug calling of 'around' advice ecb-advice-test-defun-3 
;;  I'm the AROUND advice of (the always disabled) ecb-advice-test-defun-3
;;  I'm the ORIGINAL function ecb-advice-test-defun-4
;;  ECB 2.33: debug disabling of 'around' advice ecb-advice-test-defun-3 
;;  ECB 2.33: debug with always disabled ecb-advice: around ecb-advice-test-defun-3 - EXIT
;;  I'm the ORIGINAL function ecb-advice-test-defun-2
;;  I'm the ORIGINAL function ecb-advice-test-defun-3
;;  I'm the ORIGINAL function ecb-advice-test-defun-4
;;  LOC-1 ecb-test-with-original-advice-set
;;  ECB 2.33: debug with original advice-set: ecb-advice-test-set - ENTRY
;;  I'm the ORIGINAL function ecb-advice-test-defun-1
;;  I'm the ORIGINAL function ecb-advice-test-defun-2
;;  ECB 2.33: debug with original advice-set: ecb-advice-test-set - EXIT
;;  LOC-2 ecb-test-with-original-advice-set
;;  I'm the ORIGINAL function ecb-advice-test-defun-1
;;  I'm the ORIGINAL function ecb-advice-test-defun-2
;;  LOC-3 ecb-test-with-original-advice-set
;;  ECB 2.33: debug with original advice-set: ecb-advice-test-set - ENTRY
;;  I'm the ORIGINAL function ecb-advice-test-defun-1
;;  I'm the ORIGINAL function ecb-advice-test-defun-2
;;  LOC-4 ecb-test-with-original-advice-set
;;  ECB 2.33: debug with original advice-set: ecb-advice-test-set - ENTRY
;;  I'm the ORIGINAL function ecb-advice-test-defun-1
;;  I'm the ORIGINAL function ecb-advice-test-defun-2
;;  ECB 2.33: debug with original advice-set: ecb-advice-test-set - EXIT
;;  ECB 2.33: debug with original advice-set: ecb-advice-test-set - EXIT
;;  LOC-5 ecb-test-with-original-advice-set
;;  ECB 2.33: debug enabling the advice-set: ecb-advice-test-set
;;  ECB 2.33: debug enabling of 'after' advice ecb-advice-test-defun-2 
;;  ECB 2.33: debug enabling of 'before' advice ecb-advice-test-defun-2 
;;  ECB 2.33: debug enabling of 'around' advice ecb-advice-test-defun-1 
;;  ECB 2.33: debug with original advice-set: ecb-advice-test-set - EXIT
;;  ECB 2.33: debug calling of 'around' advice ecb-advice-test-defun-1 
;;  I'm the AROUND advice of ecb-advice-test-defun-1
;;  ECB 2.33: debug calling of 'before' advice ecb-advice-test-defun-2 
;;  I'm the BEFORE advice of ecb-advice-test-defun-2
;;  I'm the ORIGINAL function ecb-advice-test-defun-2
;;  ECB 2.33: debug calling of 'after' advice ecb-advice-test-defun-2 
;;  I'm the AFTER advice of ecb-advice-test-defun-2
;;  LOC-6 ecb-test-with-original-advice-set
;;  ECB 2.33: debug disabling the advice-set: ecb-advice-test-set
;;  ECB 2.33: debug disabling of 'after' advice ecb-advice-test-defun-2 
;;  ECB 2.33: debug disabling of 'before' advice ecb-advice-test-defun-2 
;;  ECB 2.33: debug disabling of 'around' advice ecb-advice-test-defun-1 
;;  I'm the ORIGINAL function ecb-advice-test-defun-1
;;  I'm the ORIGINAL function ecb-advice-test-defun-2
;;  I'm the ORIGINAL function ecb-advice-test-defun-3
;;  I'm the ORIGINAL function ecb-advice-test-defun-4
;;  !!! END ecb-test-with-original-advice-set !!!!

(silentcomp-provide 'ecb-advice-test)

;;; ecb-advice-test.el ends here

