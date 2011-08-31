;;; sb-kde.el --- shimbun backend for www.KDE.gr.jp

;; Copyright (C) 2002, 2003 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Authors: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
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

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-kde (shimbun-mhonarc) ())

(defvar shimbun-kde-url "http://www.KDE.gr.jp/ml/")
(defvar shimbun-kde-groups '("Kdeveloper" "Kuser"))
(defvar shimbun-kde-coding-system 'euc-jp)
(defvar shimbun-kde-reverse-flag t)
(defvar shimbun-kde-litemplate-regexp
  "<STRONG><A NAME=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+.html\\)\">\\([^<]+\\)</A></STRONG> <EM>\\([^<]+\\)</EM>")

(defvar shimbun-kde-x-face-alist
  '(("default" . "X-Face: $k<l7p@7F^!3Gz8>q]+,^4o0}[`AQ*4!ml,:9v\
:\\1JvC:xf^dG6rsim7uO\\sF<sb\\`jotT8\n x)%/mOn~<RBUKORnGwUHtsz$}\
&5COS0|'pT/1_A6$`o%2k`i/D(ntjnjFo9HKdpUcmQ|zW[yzHh+l<\n (NUfntXz\
V{p:G4A}<vq\"[#f;XPl\\Ea|B5yrA4-}Q};cWbLr9hfDhCzxs]z-bRkQ<Rc`m!")))

(luna-define-method shimbun-index-url ((shimbun shimbun-kde))
  (concat (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun) "/"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-kde)
					 &optional range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	(regexp "\
\\[Prev Page\\]\\[<a href=\"\\(.+\\.html\\)\">Next Page</a>\\]")
	(months '("index.html"))
	headers)
    (if (shimbun-mhonarc-reverse-flag-internal shimbun)
	(progn
	  (goto-char (point-min))
	  (while (and (or (not pages)
			  (>= (decf pages) 0))
		      (re-search-forward regexp nil t))
	    (push (match-string 1) months)))
      (goto-char (point-max))
      (while (and (or (not pages)
		      (>= (decf pages) 0))
		  (re-search-backward regexp nil t))
	(push (match-string 1) months)))
    (setq months (nreverse months))
    (catch 'stop
      (dolist (month months)
	(let ((url (concat (shimbun-index-url shimbun) month)))
	  (erase-buffer)
	  (shimbun-retrieve-url url t)
	  (shimbun-mhonarc-get-headers shimbun url headers month))))
    headers))

(provide 'sb-kde)

;;; sb-kde.el ends here
