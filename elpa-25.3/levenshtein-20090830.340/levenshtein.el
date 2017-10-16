;;; levenshtein.el --- Edit distance between two strings.

;; Copyright (C) 2003, 2005  Aaron S. Hawley, Art Taylor

;; Author: Aaron S. Hawley <ashawley at uvm dot edu>,
;;         Art Taylor
;; Keywords: lisp
;; Package-Version: 20090830.340

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

;; See: http://wikipedia.org/wiki/Levenshtein_distance

;;; History:

;; Written by Art Taylor on 15 March 2003 in Washington, DC, USA and
;; released under the zlib license.  Rewritten by Aaron Hawley on 12
;; July 2005 in Burlington, VT, USA and released under the GNU GPL.
;; Posted to the EmacsWiki on 12 July 2005.

;;; Code:


(defun levenshtein-distance (str1 str2)
  "Return the edit distance between strings STR1 and STR2."
  (if (not (stringp str1))
      (error "Argument was not a string: %s" str1))
  (if (not (stringp str2))
      (error "Argument was not a string: %s" str2))
  (let* ((make-table ;; Multi-dimensional array object.
          (function
           (lambda (columns rows init)
             (make-vector rows (make-vector columns init)))))
         (tref ;; Table access method.
          (function
           (lambda (table x y)
            (aref (aref table y) x))))
         (tset ;; Table write method.
          (function
           (lambda
             (table x y object)
             (let ((row (copy-sequence (aref table y))))
               (aset row x object)
               (aset table y row)
               object))))
         ;; End table code.
         (length-str1 (length str1))
         (length-str2 (length str2))
         ;; d is a table with lenStr2+1 rows and lenStr2+1 columns
         (d (funcall make-table (1+ length-str1) (1+ length-str2)
                                0))) ;; Initialize to zero.
    ;; i and j are used to iterate over str1 and str2
    (let ((i 0)
          (j 0))
      (while (<= i length-str1) ;; for i from 0 to lenStr1
        (funcall tset d i 0 i) ;; d[i, 0] := i
        (setq i (1+ i))) ;; i++
      (while (<= j length-str2) ;; for j from 0 to lenStr2
        (funcall tset d 0 j j) ;; d[0, j] := j
        (setq j (1+ j)))) ;; j++
    (let ((i 1))
      (while (<= i length-str1) ;; for i from 1 to lenStr1
        (let ((j 1))
          (while (<= j length-str2) ;; for j from 1 to lenStr2
            (let* ((cost
                    ;; if str[i] = str[j] then cost:= 0 else cost := 1
                    (if (equal (aref str1 (1- i)) (aref str2 (1- j)))
                        0
                      1))
                   ;; d[i-1, j] + 1     // deletion
                   (deletion (1+ (funcall tref d (1- i) j)))
                   ;; d[i, j-1] + 1     // insertion
                   (insertion (1+ (funcall tref d i (1- j))))
                   ;; d[i-j,j-1] + cost // substitution
                   (substitution
                    (+ (funcall tref d (1- i) (1- j)) cost)))
              (funcall tset d i j ;; d[i,j] := minimum(
                    (min insertion deletion substitution)))
            (setq j (1+ j)))) ;; j++
        (setq i (1+ i)))) ;; i++
    ;; return d[lenStr1, lenStr2]
    (funcall tref d length-str1 length-str2)))

(provide 'levenshtein)
;;; levenshtein.el ends here
