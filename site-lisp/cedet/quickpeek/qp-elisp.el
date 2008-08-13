;;; qp-elisp.el --- Emacs Lisp scanning for `quickpeek'

;;; Copyright (C) 1999 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tools
;; X-RCS: $Id: qp-elisp.el,v 1.2 2005/09/30 20:42:08 zappo Exp $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; 
;;  This program handles Emacs Lisp quick peeking.  It depends on
;;  'eldoc for documentation strings of functions and variables.

(require 'eldoc)

;;; Code:
(defun lisp-quickpeek-collect-data ()
  "Collect data about the current position in an Emacs Lisp file."
  (list 'quickpeek-functional-form
	(save-excursion
	  (quickpeek-beginning-of-defun)
	  (if (looking-at "(def\\([a-z]+\\)\\s-+\\(\\w+\\)\\>")
	      (let* ((fun (match-string 1))
		     (name (match-string 2))
		     (strface
		      (cond ((string= fun "un")
			     "Function")
			    ((string= fun "var")
			     "Variable")
			    (t (capitalize fun)))))
		(list (cons (concat strface ": ") 'bold)
		      (cons name 'leave-faces)))
	    (cond ((looking-at "^\\s-*$")
		   (cons "No Context" 'bold))
		  ((quickpeek-in-non-code)
		   (cons "" nil))
		  ((looking-at "\\s-*;+")
		   (cons "Comment" font-lock-comment-face))
		  (t
		   (list (cons "Top Level Form: " 'bold)
			 (cons (buffer-substring (point)
						 (progn (end-of-line)
							(point)))
			       'leave-faces))))))
	(let* ((sym (function-called-at-point))
	       (desc
		(if sym
		    (condition-case nil
			(or (eldoc-function-argstring-from-docstring sym)
			    (eldoc-docstring-first-line (documentation sym t)))
		      (error ""))
		  "")))
	  (if sym
	      (concat
	       (symbol-name sym)
	       (if (and desc (string-match "(" desc))
		   (concat " " desc)
		 (concat ": " desc)))
	    ""))
	;; This pile o goo for completions was ripped from lisp.el
	(let* ((pattern (quickpeek-thing))
	       (predicate (function (lambda (sym)
				      (or (boundp sym) (fboundp sym)
					  (symbol-plist sym)))))
	       (lst (if (and pattern (> (length pattern) 2))
			(all-completions pattern obarray predicate)
		      nil)))
	  (if (and (= (length lst) 1)
		   (string= (symbol-name (function-called-at-point))
			    (car lst)))
	      nil
	    lst))))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq quickpeek-info-function 'lisp-quickpeek-collect-data)))


(provide 'qp-elisp)

;;; qp-elisp.el ends here
