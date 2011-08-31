;;; sb-aljazeera.el --- Al Jazeera shimbun backend

;; Copyright (C) 2007, 2008 David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Keywords: news

;; This file is a part of shimbun.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-aljazeera (shimbun-rss) ())

(defvar shimbun-aljazeera-url "http://english.aljazeera.net/Services/Rss/?PostingId=")

(defvar shimbun-aljazeera-path-alist
  '(("news" . "2007731105943979989")
    ("africa" . "2007721151816881407")
    ("america" . "200772115196613309")
    ("asia-pacific" . "2007722144444234906")
    ("central-asia" . "2007721155716791636")
    ("europe" . "2007721152443657412")
    ("middle-east" . "200861163157760548")
    ("focus" . "200861611391795581")
    ("business" . "20078615030782166")
    ("sport" . "200772215035764169")
    ("programmes" . "200772215116371443")))

(defvar shimbun-aljazeera-from-address  "press.int@aljazeera.net")

(defvar shimbun-aljazeera-content-start "<td[^>]*DetaildTitle[^>]*>")

;; FIXME: The value is currently useless.  See `sb-aljazeera-wash-article'.
(defvar shimbun-aljazeera-content-end "<TD id=\"tdRightColumn\"")

(defvar shimbun-aljazeera-groups (mapcar 'car shimbun-aljazeera-path-alist))

(defvar shimbun-aljazeera-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABgAAAAgAgMAAAB1MFCrAAAADFBMVEXIiyP27tr9/v7ixH8
 SFb1+AAAAzUlEQVQI12NYtWrlq1WrGFatWp0FppZAqBVQQSsIxQemwg+DqJU8Aa+A1HIGESsgtca
 Ylw1ILWFmPgik5jDLfMhiWPnhw3OZWwzLec+8NLdimHCTIfY4G4NztHHtgXMMZ698/MpQyGAp45f
 BcJnBNN49xeYNg0GgT21ADgODocPBw2sYDtsdrmdcwODMxzvHvoDBwdbmw4XDDPV/TZjjGRn2zpc
 /wMDHMDnA4ACDFMMahstnvwLtO1zMGwW0ffpn1lsgBx5gywI5twDoeABy7mBlftzySgAAAABJRU5
 ErkJggg==")))

(luna-define-method shimbun-index-url ((shimbun shimbun-aljazeera))
  (concat shimbun-aljazeera-url
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-aljazeera-path-alist))))

(defconst shimbun-aljazeera-date-re
  (eval-when-compile
    (concat "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) " ; M/D/Y
	    "\\([0-9]+\\):\\([0-9]+:[0-9]+\\) "       ; h:m:s
	    "\\(AM\\|PM\\)")))

(defun sb-aljazeera-wash-article ()
  ;; html coded by drunken monkeys
  (goto-char (point-min))
  (while (re-search-forward "<input[^>]*value=\"Remove Format\"[^>]*>" nil t)
    (delete-region (match-beginning 0) (match-end 0)))
  (goto-char (point-min))
  (while (re-search-forward "<[^>]*\\(align=\"right\"\\)" nil t)
    (delete-region (match-beginning 1) (match-end 1)))
  (goto-char (point-min))
  (while (re-search-forward "<tr[^>]*id=\"trMainImages" nil t)
    (let ((beg (match-beginning 0)))
      (when (search-forward "</tr>" nil t)
	(delete-region beg (point)))))
  ;; Look for content-start and content-end again and remove garbage.
  ;; (It's failed if either one has not been found.)
  (goto-char (point-min))
  (when (and (re-search-forward shimbun-aljazeera-content-start nil t)
	     (progn
	       (delete-region (point-min) (match-beginning 0))
	       (re-search-forward "<td\\(?:[\t\n ]+[^>]*\\)?>[\t\n ]*\
&nbsp;Source:</td>[^<]*<td>[^<]*</td>\\(?:[^<]*</[^>]+>\\)*" nil t)))
    (delete-region (match-end 0) (point-max))
    (insert "\n")))

(luna-define-method shimbun-make-contents
  :before ((shimbun shimbun-aljazeera) header)
  (sb-aljazeera-wash-article))

(provide 'sb-aljazeera)

;;; sb-aljazeera.el ends here
