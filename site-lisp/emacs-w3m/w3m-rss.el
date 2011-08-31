;;; w3m-rss.el --- RSS functions

;; Copyright (C) 2004, 2005 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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

;; w3m-rss.el provides RSS-related functions for emacs-w3m.  For more
;; detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; Acknowledgment:

;; I refered functions in `sb-rss.el' to implement this module.
;; Thanks to Koichiro Ohba and NAKAJIMA Mikio.


;;; Code:

(eval-when-compile (require 'cl))
(autoload 'xml-parse-region "xml")

(eval-and-compile
  (autoload 'timezone-parse-date "timezone")
  (autoload 'timezone-parse-time "timezone"))

(eval-when-compile
  ;; Avoid warning for Emacs 19 and XEmacs.
  (unless (fboundp 'match-string-no-properties)
    (autoload 'match-string-no-properties "poe"))
  ;; Avoid warning for Emacs 19.
  (unless (fboundp 'split-string)
    (autoload 'split-string "poe")))

(defun w3m-rss-parse-date-string (date)
  "Decode DATE string written in the ISO 8601 format or the RFC822 style.
Return a list of numbers which conforms to the Emacs internal format.
Valid types in the ISO 8601 format include:

    Year:
       YYYY (eg 1997)
    Year and month:
       YYYY-MM (eg 1997-07)
    Complete date:
       YYYY-MM-DD (eg 1997-07-16)
    Complete date plus hours and minutes:
       YYYY-MM-DDThh:mmTZD (eg 1997-07-16T19:20+01:00)
    Complete date plus hours, minutes and seconds:
       YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)
    Complete date plus hours, minutes, seconds and a decimal fraction
    of a second
       YYYY-MM-DDThh:mm:ss.sTZD (eg 1997-07-16T19:20:30.45+01:00)

where:
  YYYY = four-digit year
  MM   = two-digit month (01=January, etc.)
  DD   = two-digit day of month (01 through 31)
  hh   = two digits of hour (00 through 23) (am/pm NOT allowed)
  mm   = two digits of minute (00 through 59)
  ss   = two digits of second (00 through 59)
  s    = one or more digits representing a decimal fraction of a second
  TZD  = time zone designator (Z or +hh:mm or -hh:mm)

For more detail about ISO 8601 date format, see
<URL:http://www.w3.org/TR/NOTE-datetime>.

In addition to the above, it also supports the date format in the
RFC822 style which RSS 2.0 allows.  Valid types are the same as ones
which are supported by the `timezone-parse-date' function (which see)."
  (cond ((not date) nil)
	((string-match " [0-9]+ " date)
	 (let* ((vector (timezone-parse-date date))
		(year (string-to-number (aref vector 0)))
		time)
	   (when (>= year 1970)
	     (setq time (timezone-parse-time (aref vector 3)))
	     (encode-time
	      (string-to-number (aref time 2))
	      (string-to-number (aref time 1))
	      (string-to-number (aref time 0))
	      (string-to-number (aref vector 2))
	      (string-to-number (aref vector 1))
	      year
	      (aref vector 4)))))
	((string-match "\
\\([0-9][0-9][0-9][0-9]\\)\\(?:-\\([0-9][0-9]\\)\\)?\\(?:-\\([0-9][0-9]\\)\\)?\
T?\\(?:\\([0-9][0-9]\\):\\([0-9][0-9]\\)\\(?::\\([.0-9]+\\)\\)?\\)?\
\\(?:\\([-+]\\)\\([0-9][0-9]\\):?\\([0-9][0-9]\\)\\|Z\\)?"
		       date)
	 (labels ((substr (n default)
			  (if (match-beginning n)
			      (string-to-number
			       (match-string-no-properties n date))
			    default)))
	   (encode-time
	    (substr 6 0) ;; seconds
	    (substr 5 0) ;; minitue
	    (substr 4 0) ;; hour
	    (substr 3 1) ;; day
	    (substr 2 1) ;; month
	    (substr 1 0) ;; year
	    (if (match-beginning 7)
		(funcall (intern (match-string-no-properties 7 date))
			 0
			 (* 3600 (substr 8 0))
			 (* 60 (substr 9 0)))
	      0))))))

(defun w3m-rss-find-el (tag data)
  "Find the all matching elements in the data.  Careful with this on
large documents!"
  (let (found)
    (when (listp data)
      (dolist (bit data)
	(when (car-safe bit)
	  (when (equal tag (car bit))
	    (setq found (nconc found (list bit))))
	  (setq found
		(nconc found
		       (w3m-rss-find-el
			tag
			(if (and (listp (car-safe (caddr bit)))
				 (not (stringp (caddr bit))))
			    (caddr bit)
			  (cddr bit))))))))
    found))

(defun w3m-rss-get-namespace-prefix (el uri)
  "Given EL (containing a parsed element) and URI (containing a string
that gives the URI for which you want to retrieve the namespace
prefix), return the prefix.
See http://feeds.archive.org/validator/docs/howto/declare_namespaces.html
for more RSS namespaces."
  (let* ((prefix (car (rassoc uri (cadar el))))
	 (nslist (when prefix
		   (split-string (symbol-name prefix) ":")))
	 (ns (cond ((eq (length nslist) 1) ; no prefix given
		    "")
		   ((eq (length nslist) 2) ; extract prefix
		    (cadr nslist)))))
    (if (and ns (not (equal ns "")))
	(concat ns ":")
      ns)))

(provide 'w3m-rss)

;;; w3m-rss.el ends here
