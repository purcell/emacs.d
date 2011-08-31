;;; sb-haiku-os.el --- shimbun backend for haiku-os.org

;; Copyright (C) 2004, 2006 Yoichi NAKAYAMA <yoichi@geiin.org>

;; Author: Yoichi NAKAYAMA <yoichi@geiin.org>
;; Keywords: news
;; Created: Jul 30, 2004

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
(require 'sb-rss)

(luna-define-class shimbun-haiku-os (shimbun-rss) ())

(defvar shimbun-haiku-os-group-alist
  '(("news" "http://haiku-os.org/rss.php?channel=news"
     "<td id=\"newsbody\">" "</td>")
    ("forums" "http://haiku-os.org/rss.php?channel=forums"
     "<td width=\"100%\"><span class=\"postbody\">" "</span></td>")
    ("newsletters" "http://haiku-os.org/rss.php?channel=newsletters"
     "<!-- Main Content -->" "<!-- / Main Content -->"))
  "Alist of readable groups and URLs of their RSSs.")

(luna-define-method shimbun-groups ((shimbun shimbun-haiku-os))
  (mapcar 'car shimbun-haiku-os-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-haiku-os))
  (nth 1 (assoc (shimbun-current-group shimbun) shimbun-haiku-os-group-alist)))

(luna-define-method shimbun-rss-process-date ((shimbun shimbun-haiku-os) date)
  date)

(luna-define-method shimbun-make-contents :around ((shimbun shimbun-haiku-os)
						   &optional header)
  (let ((entry (assoc (shimbun-current-group shimbun)
		      shimbun-haiku-os-group-alist)))
    (shimbun-set-content-start-internal shimbun (nth 2 entry))
    (shimbun-set-content-end-internal shimbun (nth 3 entry))
    (luna-call-next-method)))

(provide 'sb-haiku-os)

;;; sb-haiku-os.el ends here
