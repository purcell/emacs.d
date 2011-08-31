;;; sb-netbsd.el --- shimbun backend for netbsd.org

;; Copyright (C) 2001, 2002, 2003 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>
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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-netbsd (shimbun-mhonarc) ())

(defvar shimbun-netbsd-url "http://www.jp.netbsd.org/ja/JP/ml/")
(defvar shimbun-netbsd-groups '("announce-ja" "junk-ja" "tech-misc-ja"
				"tech-pkg-ja" "port-arm32-ja" "port-hpcmips-ja"
				"port-mac68k-ja" "port-mips-ja"
				"port-powerpc-ja" "hpcmips-changes-ja"
				"members-ja" "admin-ja" "www-changes-ja"))

(luna-define-method shimbun-index-url ((shimbun shimbun-netbsd))
  (format "%s%s/index.html" (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-netbsd)
					 &optional range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers months)
    (goto-char (point-min))
    (while (re-search-forward
	    "<A HREF=\"\\([0-9]+\\)/\\(threads.html\\)?\">" nil t)
      (push (match-string 1) months))
    (setq months (nreverse months))
    (nreverse
     (catch 'stop
       (dolist (month months)
	 (unless (if pages (<= (incf count) pages) t)
	   (throw 'stop headers))
	 (erase-buffer)
	 (shimbun-retrieve-url
	  (format "%s%s/%s/maillist.html"
		  (shimbun-url-internal shimbun)
		  (shimbun-current-group-internal shimbun) month)
	  t)
	 (let (id url subject)
	   (while (re-search-forward
		   "<A[^>]*HREF=\"\\(msg\\([0-9]+\\)\\.html\\)\">\\([^<]+\\)</A>"
		   nil t)
	     (setq url (format "%s%s/%s/%s"
			       (shimbun-url-internal shimbun)
			       (shimbun-current-group-internal shimbun)
			       month
			       (match-string 1))
		   id (format "<%s%05d%%%s>"
			      month
			      (string-to-number (match-string 2))
			      (shimbun-current-group-internal shimbun))
		   subject (match-string 3))
	     (when (shimbun-search-id shimbun id)
	       (throw 'stop headers))
	     (push (shimbun-make-header
		    0
		    (shimbun-mime-encode-string subject)
		    (if (looking-at "</STRONG> *<EM>\\([^<]+\\)<")
			(shimbun-mime-encode-string (match-string 1))
		      "")
		    "" id "" 0 0 url)
		   headers))))
       headers))))

(provide 'sb-netbsd)

;;; sb-netbsd.el ends here
