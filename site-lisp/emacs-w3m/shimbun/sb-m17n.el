;;; sb-m17n.el --- shimbun backend for m17n.org

;; Copyright (C) 2001, 2002, 2003, 2006, 2009
;; Akihiro Arisawa <ari@mbf.sphere.ne.jp>

;; Author: Akihiro Arisawa <ari@mbf.sphere.ne.jp>
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

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-m17n (shimbun-mhonarc) ())

(defvar shimbun-m17n-url "http://www.m17n.org/mlarchive/")

(defconst shimbun-m17n-group-path-alist
  '(("mule-ja" "mule-ja/" "mule-ja@m17n.org")
    ("mule" "mule/" "mule@m17n.org")))

(defvar shimbun-m17n-groups (mapcar 'car shimbun-m17n-group-path-alist))

(defvar shimbun-m17n-x-face-alist
  '(("default" . "X-Face: '>=krMO{M-21~VC2Y-{q4s}ckFG89D`j^w\
:FI[z%_o+'mV,NKiQlN5v9i+<6EuQs\\I!3UB_\n ZY>/Nhd6A?6:B'+zCXV\
tcW.jD{Y&/'K6$ls7r8!M%HnBCV[j;?Vr!3l|dWt%.%Wx8nvzk1+w5Uw6Zb\n\
 |?bUC;wOQxjd46p\\MP[x{+0i|lA\\fZ)@4_|!uG;=M\\Y}I&D3dP00O")))

(luna-define-method shimbun-index-url ((shimbun shimbun-m17n))
  (shimbun-expand-url (nth 1 (assoc (shimbun-current-group-internal shimbun)
				    shimbun-m17n-group-path-alist))
		      (shimbun-url-internal shimbun)))

(luna-define-method shimbun-reply-to ((shimbun shimbun-m17n))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-m17n-group-path-alist)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-m17n)
					 &optional range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers months)
    (goto-char (point-min))
    (catch 'stop
      (while (and (if pages (<= (incf count) pages) t)
		  (re-search-forward
		   "<A HREF=\"\\([0-9]+\\)/maillist\\.html\">"
		   nil t)
		  (push (match-string 1) months)))
      (setq months (nreverse months))
      (dolist (month months)
	(let ((url (shimbun-expand-url (concat month "/maillist.html")
				       (shimbun-index-url shimbun))))
	  (erase-buffer)
	  (shimbun-retrieve-url url t)
	  (goto-char (point-max))
	  (while (re-search-backward "\
<a name=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+.html\\)\">\
\\([^<]+\\)</a></strong>\n\
<ul><li><em>From</em>: \\([^<]+\\)"
				     nil t)
	    (let ((id (format "<%s%s%%%s>" month (match-string 1)
			      (shimbun-current-group-internal shimbun)))
		  (xref (shimbun-expand-url (match-string 2) url))
		  (subject (shimbun-mhonarc-replace-newline-to-space
			    (match-string 3)))
		  (from (shimbun-mhonarc-replace-newline-to-space
			 (match-string 4))))
	      (if (shimbun-search-id shimbun id)
		  (throw 'stop headers)
		(push (shimbun-make-header 0
					   (shimbun-mime-encode-string subject)
					   (shimbun-mime-encode-string from)
					   nil id "" 0 0 xref)
		      headers)))))))
    headers))

(provide 'sb-m17n)

;;; sb-m17n.el ends here
