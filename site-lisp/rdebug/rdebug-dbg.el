;;; rdebug-dbg.el --- Ruby debugger frames buffer

;; Copyright (C) 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2008 Anders Lindgren

;; $Id: rdebug-dbg.el 702 2008-02-17 22:00:36Z rockyb $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; See the manual and the file `rdebug.el' for more information.

;; This file contains internal debug trace support.

;;; Code:

(require 'rdebug-vars)

(defun rdebug-debug-message (&rest args)
  (if rdebug-debug-active
      (let ((buf (get-buffer-create "*Xrdebug*")))
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-max))
            ;; 32 = space.
            (insert (make-string (* 4 rdebug-debug-depth) 32))
            (insert (apply #'format args))
            (insert "\n"))))))


(defmacro rdebug-debug-enter (str &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (rdebug-debug-message "--> %s" ,str)
     (setq rdebug-debug-depth (+ rdebug-debug-depth 1))
     (unwind-protect
         (progn
           ,@body)
       (setq rdebug-debug-depth (max 0 (- rdebug-debug-depth 1)))
       (rdebug-debug-message "<-- %s" ,str))))

(provide 'rdebug-dbg)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-dbg.el ends here
