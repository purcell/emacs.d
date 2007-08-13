;;; cedet-compat.el --- Compatibility across (X)Emacs versions

;; Copyright (C) 2003, 2007 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Keywords: compatibility
;; X-RCS: $Id: cedet-compat.el,v 1.3 2007/05/20 15:53:01 zappo Exp $

;; This file is not part of Emacs

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
;; This library provides functions to allow running CEDET packages on
;; a variety of [X]Emacs versions.

;;; Code:

(when (not (fboundp 'compare-strings))

;; XEmacs does not have the `compare-strings' function.  Here is an
;; implementation in Emacs Lisp, derived from the C implementation
;; found in src/fns.c, in GNU Emacs 21.3.1 sources.
;;;###autoload
(defun compare-strings (str1 start1 end1 str2 start2 end2 &optional ignore-case)
  "Compare the contents of two strings.
In string STR1, skip the first START1 characters and stop at END1.
In string STR2, skip the first START2 characters and stop at END2.
END1 and END2 default to the full lengths of the respective strings.

Case is significant in this comparison if IGNORE-CASE is nil.

The value is t if the strings (or specified portions) match.
If string STR1 is less, the value is a negative number N;
  - 1 - N is the number of characters that match at the beginning.
If string STR1 is greater, the value is a positive number N;
  N - 1 is the number of characters that match at the beginning."
  (or start1 (setq start1 0))
  (or start2 (setq start2 0))
  (setq end1 (if end1
                 (min end1 (length str1))
               (length str1)))
  (setq end2 (if end2
                 (min end2 (length str2))
               (length str2)))
  (let ((i1 start1)
        (i2 start2)
        result c1 c2)
    (while (and (not result) (< i1 end1) (< i2 end2))
      (setq c1 (aref str1 i1)
            c2 (aref str2 i2)
            i1 (1+ i1)
            i2 (1+ i2))
      (if ignore-case
          (setq c1 (upcase c1)
                c2 (upcase c2)))
      (setq result (cond ((< c1 c2) (- i1))
                         ((> c1 c2) i1))))
    (or result
        (cond ((< i1 end1) (1+ (- i1 start1)))
              ((< i2 end2) (1- (- start1 i1)))
              (t)))
    ))

)

;; subst-char-in-string is not found on the XEmacs <= 21.4.  Provide
;; here for compatibility.
(if (not (fboundp 'subst-char-in-string))

;;;###autoload    
(defun subst-char-in-string (fromchar tochar string &optional inplace)
  ;; From Emacs 21.3/lisp/subr.el
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
  (let ((i (length string))
	(newstr (if inplace string (copy-sequence string))))
    (while (> i 0)
      (setq i (1- i))
      (if (eq (aref newstr i) fromchar)
	  (aset newstr i tochar)))
    newstr))

)


(provide 'cedet-compat)

;;; cedet-compat.el ends here
