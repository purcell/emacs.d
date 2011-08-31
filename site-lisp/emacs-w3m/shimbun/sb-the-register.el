;;; sb-the-register.el --- The Register shimbun backend

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 David Hansen

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

(luna-define-class shimbun-the-register (shimbun-rss) ())

(defvar shimbun-the-register-url "http://www.theregister.co.uk/")
(defvar shimbun-the-register-from-address  "invalid@theregister.co.uk")
(defvar shimbun-the-register-content-start "<h2>")
(defvar shimbun-the-register-content-end
  "<p class=\"Furniture\">\\|<p id=\"Copyright\">")
(defvar shimbun-the-register-x-face-alist
  '(("default" . "X-Face: 'r-3ZQiX|_[TrM[|LF34{X#MX`MHFuL$_2w4Cs\"ET_jx9/JsL)k\
xvY~i(,cv8ho2=\\L!Tz# @=+.N^%}G<@JRS<ZeD90JN/,oDx.o:\\-kBeyKN%DzZ)s|Ck69P6WY6^\
IPf~GT+xfvp:1-BRTK7'f&\"\"mr'CflD?Q2R%IkV>")))

(defvar shimbun-the-register-path-alist
  '(("news" . "headlines.rss")
    ("enterprise" . "enterprise/headlines.rss")
    ("software" . "software/headlines.rss")
    ("personal" . "personal/headlines.rss")
    ("internet" . "internet/headlines.rss")
    ("mobile" . "mobile/headlines.rss")
    ("security" . "security/headlines.rss")
    ("management" . "management/headlines.rss")
    ("channel" . "channel/headlines.rss")
    ("odds" . "odds/headlines.rss")))

(defvar shimbun-the-register-groups
  (mapcar 'car shimbun-the-register-path-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-the-register))
  (concat shimbun-the-register-url
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-the-register-path-alist))))

(luna-define-method shimbun-get-headers :around
  ((shimbun shimbun-the-register) &optional range)
  (mapcar
   (lambda (header)
     (shimbun-header-set-xref
      header (concat (shimbun-header-xref header) "print.html"))
     header)
   (luna-call-next-method)))

(luna-define-method shimbun-make-contents
  :before ((shimbun shimbun-the-register) header)
  (save-excursion
    ;; remove annoying stuff
    (dolist (junk '(("(?<span class=\"URL\">" . "</span>)?")
                    ("<div \\(class\\|id\\)=\"[^\"]*Ad\"" . "</div>")
                    ("<a href=\"http://ad\\." . "</a>")))
      (goto-char (point-min))
      (message "%s" (car junk))
      (while (re-search-forward (car junk) nil t)
        (let ((beg (match-beginning 0)))
          (when (re-search-forward (cdr junk) nil t)
            (delete-region beg (point))))))))

(provide 'sb-the-register)

;;; sb-the-register.el ends here
