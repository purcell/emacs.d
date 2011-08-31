;;; w3m-ccl.el --- CCL programs to process Unicode and internal characters.

;; Copyright (C) 2001, 2003, 2004, 2005, 2006, 2007
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          ARISAWA Akihiro <ari@mbf.sphere.ne.jp>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains CCL programs to process Unicode and internal
;; characters of w3m.  For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/

;;; MEMO:

;; It is possible to support multi scripts without Mule-UCS.  For more
;; detail, see [emacs-w3m:01950]

;;; Code:

(eval-and-compile
  (cond
   ((featurep 'xemacs)
    (require 'pccl))
   (t
    (require 'ccl))))

;;; CCL programs:

(eval-when-compile
  (when (and (not (fboundp 'charset-id))
	     (fboundp 'charset-id-internal))
    (defmacro charset-id (charset)
      "Return charset identification number of CHARSET."
      `(charset-id-internal ,charset))))

(eval-and-compile
  (defconst w3m-internal-characters-alist
    '((?\x90 . ? )			; ANSP (use for empty anchor)
      (?\x91 . ? )			; IMSP (blank around image)
      (?\xa0 . ? ))			; NBSP (non breakble space)
    "Alist of internal characters v.s. ASCII characters.")

  (defun w3m-ccl-write-repeat (charset &optional r0 r1)
    (unless r0
      (setq r0 'r0))
    (unless r1
      (setq r1 (if (eq r0 'r1) 'r0 'r1)))
    (let ((unibyte (memq charset '(latin-iso8859-1 katakana-jisx0201))))
      (if (fboundp 'ccl-compile-write-multibyte-character)
	  `((,r1 &= ?\x7f)
	    ,@(unless unibyte
		`((,r1 |= ((,r0 & ?\x7f) << 7))))
	    (,r0 = ,(charset-id charset))
	    (write-multibyte-character ,r0 ,r1)
	    (repeat))
	`((write ,(charset-id charset))
	  ,@(unless unibyte
	      `((write ,r0)))
	  (write-repeat ,r1)))))

  (defconst w3m-ccl-write-euc-japan-character
    (when (fboundp 'ccl-compile-read-multibyte-character)
      `((read-multibyte-character r1 r0)
	(if (r1 == ,(charset-id 'ascii))
	    ;; (1) ASCII characters
	    (write-repeat r0))
	(if (r1 == ,(charset-id 'latin-jisx0201))
	    ;; (2) Latin Part of Japanese JISX0201.1976
	    ;;     Convert to ASCII
	    (write-repeat r0))
	(r2 = (r1 == ,(charset-id 'japanese-jisx0208-1978)))
	(if ((r1 == ,(charset-id 'japanese-jisx0208)) | r2)
	    ;; (3) Characters of Japanese JISX0208.
	    ((r1 = ((r0 & 127) | 128))
	     (r0 = ((r0 >> 7) | 128))
	     (write r0)
	     (write-repeat r1)))
	(if (r1 == ,(charset-id 'katakana-jisx0201))
	    ;; (4) Katakana Part of Japanese JISX0201.1976
	    ((r0 |= 128)
	     (write ?\x8e)
	     (write-repeat r0)))))
    "CCL program to write characters represented in `euc-japan'.")

  (defconst w3m-ccl-write-iso-latin-1-character
    (when (fboundp 'ccl-compile-read-multibyte-character)
      `((read-multibyte-character r1 r0)
	(if (r1 == ,(charset-id 'ascii))
	    ;; (1) ASCII characters
	    (write-repeat r0))
	(if (r1 == ,(charset-id 'latin-jisx0201))
	    ;; (2) Latin Part of Japanese JISX0201.1976
	    ;;     Convert to ASCII
	    (write-repeat r0))
	(if (r1 == ,(charset-id 'latin-iso8859-1))
	    ;; (3) Latin-1 characters
	    ((r0 |= ?\x80)
	     (write-repeat r0)))))
    "CCL program to write characters represented in `iso-latin-1'.")

  (defconst w3m-ccl-generate-ncr
    `((r1 = 0)
      (r2 = 0)
      (loop
       (r1 = (r1 << 4))
       (r1 |= (r0 & 15))
       (r0 = (r0 >> 4))
       (if (r0 == 0)
	   (break)
	 ((r2 += 1)
	  (repeat))))
      (write "&#x")
      (loop
       (branch (r1 & 15)
	       ,@(mapcar
		  (lambda (i)
		    (list 'write (string-to-char (format "%x" i))))
		  '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
       (r1 = (r1 >> 4))
       (if (r2 == 0)
	   ((write ?\;)
	    (break))
	 ((r2 -= 1)
	  (repeat))))
      (repeat))
    "CCL program to generate a string which represents a UCS codepoint
in NCR (Numeric Character References)."))

(define-ccl-program w3m-euc-japan-decoder
  `(2
    (loop
     (read r0)
     ;; Process normal EUC characters.
     (if (r0 < ?\x80)
	 (write-repeat r0))
     (if (r0 > ?\xa0)
	 ((read r1)
	  ,@(w3m-ccl-write-repeat 'japanese-jisx0208)))
     (if (r0 == ?\x8e)
	 ((read r1)
	  ,@(w3m-ccl-write-repeat 'katakana-jisx0201)))
     (if (r0 == ?\x8f)
	 ((read r0)
	  (read r1)
	  ,@(w3m-ccl-write-repeat 'japanese-jisx0212)))
     ;; Process internal characters used in w3m.
     ,@(mapcar (lambda (pair)
		 `(if (r0 == ,(car pair))
		      (write-repeat ,(cdr pair))))
	       w3m-internal-characters-alist)
     (write-repeat r0))))

(unless (get 'w3m-euc-japan-encoder 'ccl-program-idx)
  (define-ccl-program w3m-euc-japan-encoder
    `(1 (loop (read r0) (write-repeat r0)))))

(define-ccl-program w3m-iso-latin-1-decoder
  `(2
    (loop
     (read r0)
     ;; Process ASCII characters.
     (if (r0 < ?\x80)
	 (write-repeat r0))
     ;; Process Latin-1 characters.
     (if (r0 > ?\xa0)
	 (,@(w3m-ccl-write-repeat 'latin-iso8859-1 'r1)))
     ;; Process internal characters used in w3m.
     ,@(mapcar (lambda (pair)
		 `(if (r0 == ,(car pair))
		      (write-repeat ,(cdr pair))))
	       w3m-internal-characters-alist)
     (write-repeat r0))))

(unless (get 'w3m-iso-latin-1-encoder 'ccl-program-idx)
  (define-ccl-program w3m-iso-latin-1-encoder
    `(1 (loop (read r0) (write-repeat r0)))))


(provide 'w3m-ccl)

;;; w3m-ccl.el ends here
