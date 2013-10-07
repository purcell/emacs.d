;;; later-do.el --- execute lisp code ... later

;; Copyright (C) 2004, 2005, 2006, 2007, 2008,
;;   2009 Free Software Foundation, Inc.

;; Author: Jorgen Schaefer <forcer@forcix.cx>

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 3
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;;; 02110-1301 USA

;;; Commentary

;; This file will execute lisp code ``later on''.  This way it is
;; possible to work while elisp does some longer calculations, if you
;; can convert those calculations into a sequence of function calls.

;;; Code:

(defvar later-do-version "0.2emms2 (2005-09-20)"
  "Version string of later-do.")

(defgroup later-do nil
  "*Running functions ... later!"
  :prefix "later-do-"
  :group 'development)

(defcustom later-do-interval 0.5
  "How many seconds to wait between running events."
  :group 'later-do
  :type 'number)

(defvar later-do-list nil
  "A list of functions to be called later on.")

(defvar later-do-timer nil
  "The timer that later-do uses.")

(defun later-do (function &rest args)
  "Apply FUNCTION to ARGS later on.  This is an unspecified
amount of time after this call, and definitely not while lisp is
still executing.  Code added using `later-do' is guaranteed to be
executed in the sequence it was added."
  (setq later-do-list (nconc later-do-list
                              (list (cons function args))))
  (unless later-do-timer
    (setq later-do-timer
          (run-with-timer later-do-interval nil 'later-do-timer))))

(defun later-do-timer ()
  "Run the next element in `later-do-list', or do nothing if it's
empty."
  (if (null later-do-list)
      (setq later-do-timer nil)
    (let ((fun (caar later-do-list))
          (args (cdar later-do-list)))
      (setq later-do-list (cdr later-do-list))
      (unwind-protect
          (apply fun args)
        (setq later-do-timer (run-with-timer later-do-interval
                                             nil
                                             'later-do-timer))))))

(provide 'later-do)
;;; later-do.el ends here
