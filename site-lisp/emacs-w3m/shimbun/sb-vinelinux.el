;;; sb-vinelinux.el --- shimbun backend class for vinelinux web site. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2006, 2009
;; NAKAJIMA Mikio <minakaji@namazu.org>

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

(luna-define-class shimbun-vinelinux (shimbun) ())

(defconst shimbun-vinelinux-url "http://www.vinelinux.org")
(defconst shimbun-vinelinux-group-path-alist
  '(("errata.4x.i386" . "errata/4x/i386.html")
    ("errata.4x.ppc" . "errata/4x/ppc.html")
    ("errata.3x.i386" . "errata/3x/i386.html")
    ("errata.3x.ppc" . "errata/3x/ppc.html")
    ("errata.3x.alpha" . "errata/3x/alpha.html")
    ("errata.25x.i386" . "errata/25x/i386.html")
    ("errata.25x.ppc" . "errata/25x/ppc.html")
    ("errata.25x.sparc" . "errata/25x/sparc.html")
    ("errata.25x.alpha" . "errata/25x/alpha.html")
    ("errata.2x.i386" . "errata/2x/i386.html")
    ("errata.2x.ppc" . "errata/2x/ppc.html")
    ("errata.2x.sparc" . "errata/2x/sparc.html")
    ("errata.2x.alpha" . "errata/2x/alpha.html")
    ("errata.1x" . "errata/1x/index.html")))

(defconst shimbun-vinelinux-groups
  (mapcar 'car shimbun-vinelinux-group-path-alist))
(defconst shimbun-vinelinux-from-address "webmaster@www.vinelinux.org")

(defun shimbun-vinelinux-parse-time (str)
  (shimbun-make-date-string
   (string-to-number (substring str 0 4))
   (string-to-number (substring str 4 6))
   (string-to-number (substring str 6))))

(luna-define-method shimbun-index-url ((shimbun shimbun-vinelinux))
  (concat (shimbun-url-internal shimbun)
	  "/"
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-vinelinux-group-path-alist))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-vinelinux)
					 &optional range)
  (let ((case-fold-search t)
	start end headers url id date subject)
    (subst-char-in-region (point-min) (point-max) ?\t ?  t)
    (setq start (progn
		  (search-forward "</font>の更新/障害情報です</h4>" nil t nil)
		  (search-forward "<table>" nil t nil)
		  (point))
	  end (progn (search-forward "</table>" nil t nil)
		     (point)))
    (goto-char start)
    ;; Use entire archive.
    (catch 'stop
      (while (re-search-forward
	      ;; <td><a href="20010501.html">quota の更新</a></td>
	      ;; <td><a href="20010501-3.html">Vine Linux 2.1.5 にアップグレードした時の xdvi や tgif 等の日本語表示の不具合</a></td>
	      "^<td><a href=\"\\(\\([0-9]+\\)\\(-[0-9]+\\)*\\.html\\)\">\\(.+\\)</a></td>"
	      end t)
	(setq url (shimbun-expand-url (match-string-no-properties 1)
				      (shimbun-index-url shimbun))
	      id (format "<%s%05d%%%s>"
			 (match-string-no-properties 1)
			 (string-to-number (match-string-no-properties 2))
			 (shimbun-current-group-internal shimbun))
	      date (shimbun-vinelinux-parse-time (match-string-no-properties 2))
	      subject (match-string 4))
	(if (shimbun-search-id shimbun id)
	    (throw 'stop nil))
	(forward-line 1)
	(push (shimbun-make-header 0
				   (shimbun-mime-encode-string subject)
				   (shimbun-from-address shimbun)
				   date id "" 0 0 url)
	      headers)))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun-vinelinux) header)
  ;;<h4>●2002,1,22(2002,1,24 更新)● ppxp パッケージの修正</h4>
  ;; <h3>[ 2003,01,29 ] telnet にバグ</h4> ;; start with h3 and end with h4...f(^^;;;
  (if (not (re-search-forward
	    "^<h4>●[,0-9]+.*●.*</h4>\\|^<h[34]>\\[ [,0-9]+ \\] .+</h[34]>"
	    nil t nil))
      nil
    (delete-region (progn (forward-line 1) (point)) (point-min))
    (shimbun-header-insert shimbun header)
    (insert
     "Content-Type: text/html; charset=ISO-2022-JP\nMIME-Version: 1.0\n\n"
     "<html><head><base href=\""
     (shimbun-header-xref header)
     "\"></head><body>")
    (goto-char (point-min))
    (while (re-search-forward "</*blockquote>\n" nil t nil)
      (delete-region (match-beginning 0) (point)))
    (goto-char (point-max))
    (insert "</body></html>")
    (encode-coding-string (buffer-string)
			  (mime-charset-to-coding-system "ISO-2022-JP"))))

(provide 'sb-vinelinux)

;;; sb-vinelinux.el ends here
