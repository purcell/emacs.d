;;; sb-pocketgames.el --- shimbun backend class for www.pocketgames.jp. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2004, 2005, 2008 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news
;; Version: $Id: sb-pocketgames.el,v 1.18 2008-01-17 23:38:36 yamaoka Exp $
;; Last Modified: $Date: 2008-01-17 23:38:36 $

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

(require 'shimbun)

(eval-and-compile
  (luna-define-class shimbun-pocketgames (shimbun) (content-hash))
  (luna-define-internal-accessors 'shimbun-pocketgames))

(defvar shimbun-pocketgames-url "http://www.pocketgames.jp")
(defvar shimbun-pocketgames-groups '("news"))
(defvar shimbun-pocketgames-coding-system 'shift_jis)
(defvar shimbun-pocketgames-content-start
  "<a class=\"pn-normal\" href=\"modules.php\\?op=modload\&amp;name=Search\\&amp;file=index\\&amp;action=search\\&amp;overview=1\\&amp;active_stories=[0-9]+\\&amp;stories_topics\[[0-9]+\]=\"><b>[^<]+</b></a>")
(defvar shimbun-pocketgames-content-end
  "</body>")

(luna-define-method shimbun-reply-to ((shimbun shimbun-pocketgames))
  "Return the mailing list address."
  "info@pocketgames.jp")

(defvar shimbun-pocketgames-expiration-days 14)

(luna-define-method shimbun-get-headers ((shimbun shimbun-pocketgames)
					 &optional range)
  (let ((regexp "<a class=\"pn-title\" href=\"\\(modules.php\\?op=modload\\&amp;name=News\\&amp;file=article\\&amp;sid=[0-9]+\\&amp;mode=thread\\&amp;order=0\\)\">\\([^<]+\\)</a></font><br>")
	url from year month day time date subject id start end headers)
    (catch 'quit
      (while (re-search-forward regexp nil t nil)
	(setq url (match-string-no-properties 1)
	      subject (match-string-no-properties 2)
	      start (point)
	      end (set-marker
		   (make-marker)
		   (or (and (re-search-forward regexp nil t nil)
			    (match-beginning 0))
		       (point-max))))
	(goto-char start)
	(unless
	    (re-search-forward
	     "Posted by: \\(.+\\) on \\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) (\\(月\\|火\\|水\\|木\\|金\\|土\\|日\\))  - \\([0-9][0-9]:[0-9][0-9]\\) JST <\/font>"
	     end t nil)
	  (throw 'quit nil))
	(setq from (shimbun-mime-encode-string (match-string 1))
	      year (string-to-number (match-string 2))
	      month (string-to-number (match-string 3))
	      day (string-to-number (match-string 4))
	      time (match-string 6)
	      date (shimbun-make-date-string year month day time)
	      id (format "<%04d%02d%02d%s%%news@pocketgames>"
			 year month day
			 (apply (lambda (x y) (format "%02d%02d" x y))
                                (mapcar 'string-to-number (split-string time ":")))))
	(when (shimbun-search-id shimbun id)
	  (throw 'quit nil))
	(with-temp-buffer
	  (insert subject)
	  (shimbun-remove-markup)
	  (setq subject (buffer-string)))
	(setq url (shimbun-expand-url
		   (shimbun-decode-anchor-string url)
		   (concat (shimbun-index-url shimbun) "/")))
	(push (shimbun-make-header
	       0 (shimbun-mime-encode-string subject)
	       from date id "" 0 0 url)
	      headers)))
    headers))

(provide 'sb-pocketgames)

;;; sb-pocketgames.el ends here
