;;; sb-plucker.el --- shimbun backend for Plucker mailing lists.

;; Copyright (C) 2003, 2005 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news

;; This file is a part of shimbun.

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

;;; Code:

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-plucker (shimbun-mhonarc) ())

(defconst shimbun-plucker-group-path-alist
  '(("announce" . "/plucker-announce@rubberchicken.org/maillist.html")
    ("list" . "/plucker-list@rubberchicken.org/maillist.html")
    ("dev" . "/plucker-dev@rubberchicken.org/maillist.html")))
(defvar shimbun-plucker-url "http://www.mail-archive.com")
(defvar shimbun-plucker-groups (mapcar 'car shimbun-plucker-group-path-alist))
(defvar shimbun-plucker-reverse-flag t)
(defvar shimbun-plucker-litemplate-regexp
  "<li>[\t\r\n ]*<span[^>]*>\\([0-9][0-9][0-9][0-9]/[0-9]?[0-9]/[0-9]?[0-9]\\)</span>[\t\r\n ]*\
<span[^>]*><a name=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+\\.html\\)\">\\([^<]+\\)</a></span>[\t\r\n ]*\
<span[^>]*>\\([^<]+\\)</span>")

(defun shimbun-plucker-extract-header-values (shimbun url headers aux)
  (let ((id (format "<%s%s%%%s>"
		    (or aux "")
		    (match-string 2)
		    (shimbun-current-group-internal shimbun)))
	(url (shimbun-expand-url (match-string 3) url))
	(subject (shimbun-mhonarc-replace-newline-to-space (match-string 4)))
	(from (shimbun-mhonarc-replace-newline-to-space (match-string 5)))
	(date (match-string 1)))
    (if (shimbun-search-id shimbun id)
	(throw 'stop headers)
      (setq date (shimbun-make-date-string
		  (string-to-number (substring date 0 4))
		  (string-to-number (substring date 5 8))
		  (string-to-number (substring date 8 10))
		  nil "-0700"))
      (push (shimbun-make-header
	     0 (shimbun-mime-encode-string subject)
	     (shimbun-mime-encode-string from)
	     date id "" 0 0 url)
	    headers)
      headers)))

(defun shimbun-plucker-get-headers (shimbun url &optional aux)
  (let (headers case-fold-search)
    (if (shimbun-mhonarc-reverse-flag-internal shimbun)
	(progn
	  (goto-char (point-min))
	  (while (re-search-forward shimbun-plucker-litemplate-regexp nil t)
	    (setq headers (shimbun-plucker-extract-header-values
			   shimbun url headers aux))
	    (forward-line 1)))
      (goto-char (point-max))
      (while (re-search-backward shimbun-plucker-litemplate-regexp nil t)
	(setq headers (shimbun-plucker-extract-header-values
		       shimbun url headers aux))
	(forward-line 0)))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-mhonarc)
					 &optional range)
  (catch 'stop
    (shimbun-plucker-get-headers shimbun (shimbun-index-url shimbun))))

(luna-define-method shimbun-index-url ((shimbun shimbun-plucker))
  (concat (shimbun-url-internal shimbun)
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-plucker-group-path-alist))))

(provide 'sb-plucker)

;;; sb-plucker.el ends here
