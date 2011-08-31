;;; w3m-perldoc.el --- The add-on program to view Perl documents.

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2007
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: w3m, perldoc

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

;; w3m-perldoc.el is the add-on program of emacs-w3m to view Perl
;; documents.  For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/

;;; Code:
(require 'w3m)

(defgroup w3m-perldoc nil
  "Perldoc front-end for emacs-w3m."
  :group 'w3m
  :prefix "w3m-perldoc-")

(defcustom w3m-perldoc-command "perldoc"
  "*Name of the executable file of perldoc."
  :group 'w3m-perldoc
  :type '(string :size 0))

(defcustom w3m-perldoc-pod2html-command "pod2html"
  "*Name of the executable file of pod2html."
  :group 'w3m-perldoc
  :type '(string :size 0))

(defcustom w3m-perldoc-pod2html-arguments
  '("--noindex")
  "*Arguments of pod2html."
  :group 'w3m-perldoc
  :type '(repeat (string :format "Argument: %v\n" :size 0))
  :get (lambda (symbol)
	 (delq nil (delete "" (mapcar (lambda (x) (if (stringp x) x))
				      (default-value symbol)))))
  :set (lambda (symbol value)
	 (custom-set-default
	  symbol
	  (delq nil (delete "" (mapcar (lambda (x) (if (stringp x) x))
				       value))))))

(defcustom w3m-perldoc-input-coding-system
  (if (string= "Japanese" w3m-language)
      'euc-japan
    (if (w3m-find-coding-system 'utf-8)
	'utf-8
      'iso-latin-1))
  "*Coding system used when writing to `w3m-perldoc-command'."
  :group 'w3m-perldoc
  :type '(coding-system :size 0))

(defcustom w3m-perldoc-output-coding-system
  'undecided
  "*Coding system used when reading from `w3m-perldoc-command'."
  :group 'w3m-perldoc
  :type '(coding-system :size 0))

;;;###autoload
(defun w3m-about-perldoc (url &optional no-decode no-cache &rest args)
  (when (string-match "\\`about://perldoc/" url)
    (let ((docname (if (= (length url) (match-end 0))
		       "perl"
		     (w3m-url-decode-string (substring url (match-end 0)))))
	  (default-directory w3m-profile-directory)
	  (process-environment (copy-sequence process-environment)))
      ;; To specify the place in which pod2html generates its cache files.
      (setenv "HOME" (expand-file-name w3m-profile-directory))
      (and (let ((coding-system-for-read w3m-perldoc-output-coding-system))
	     (zerop (call-process w3m-perldoc-command
				  nil t nil "-u" docname)))
	   (let ((coding-system-for-write w3m-perldoc-input-coding-system)
		 (coding-system-for-read w3m-perldoc-input-coding-system))
	     (zerop (apply (function call-process-region)
			   (point-min) (point-max)
			   w3m-perldoc-pod2html-command
			   t '(t nil) nil
			   (append w3m-perldoc-pod2html-arguments
				   '("--htmlroot=about://perldoc")))))
	   (let ((case-fold-search t))
	     (goto-char (point-min))
	     (while (re-search-forward
		     "<a href=\"about://perldoc/\\([^\"]*\\)\\(\\.html\\)\">" nil t)
	       (delete-region (match-beginning 2) (match-end 2))
	       (save-restriction
		 (narrow-to-region (match-beginning 1) (match-end 1))
		 (while (search-backward "/" nil t)
		   (delete-char 1)
		   (insert "::"))
		 (goto-char (point-max))))
	     "text/html")))))

;;;###autoload
(defun w3m-perldoc (docname)
  "View Perl documents."
  (interactive "sDocument: ")
  (w3m-goto-url (concat "about://perldoc/" (w3m-url-encode-string docname))))

(provide 'w3m-perldoc)

;;; w3m-perldoc.el ends here.
