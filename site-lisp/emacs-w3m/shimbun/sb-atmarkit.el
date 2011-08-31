;;; sb-atmarkit.el --- shimbun backend for atmarkit -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2004, 2005, 2006 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news
;; Created: Jun 15, 2003

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
(require 'sb-multi)

(luna-define-class shimbun-atmarkit (shimbun-multi shimbun-rss) ())

(defvar shimbun-atmarkit-coding-system 'euc-japan)
(defvar shimbun-atmarkit-content-start "<body[^>]*>")
(defvar shimbun-atmarkit-content-end "</body[^>]*>")

(defvar shimbun-atmarkit-group-path-alist
  '( ;; ニュース系
    ;; NewsInsight
    ("news". "http://www.atmarkit.co.jp/rss/news/rss2dc.xml")
    ;; フォーラム系
    ;; Windows Server Insiderフォーラム
    ("fwin2k" . "http://www.atmarkit.co.jp/rss/fwin2k/rss2dc.xml")
    ;; Insider.NETフォーラム
    ("fdotnet" . "http://www.atmarkit.co.jp/rss/fdotnet/rss2dc.xml")
    ;; System Insiderフォーラム
    ("fsys" . "http://www.atmarkit.co.jp/rss/fsys/rss2dc.xml")
    ;; XML & Web Servicesフォーラム
    ("fxml" . "http://www.atmarkit.co.jp/rss/fxml/rss2dc.xml")
    ;; Database Expertフォーラム
    ("fdb". "http://www.atmarkit.co.jp/rss/fdb/rss2dc.xml")
    ;; Linux Squareフォーラム
    ("flinux" . "http://www.atmarkit.co.jp/rss/flinux/rss2dc.xml")
    ;; Master of IP Networkフォーラム
    ("fnetwork" . "http://www.atmarkit.co.jp/rss/fnetwork/rss2dc.xml")
    ;; Java Solutionフォーラム
    ("fjava" . "http://www.atmarkit.co.jp/rss/fjava/rss2dc.xml")
    ;; Security&Trustフォーラム
    ("fsecurity". "http://www.atmarkit.co.jp/rss/fsecurity/rss2dc.xml")
    ;; Web Client & Reportフォーラム
    ("fwcr" . "http://www.atmarkit.co.jp/rss/fwcr/rss2dc.xml")
    ;; IT Architectフォーラム
    ("farc" . "http://www.atmarkit.co.jp/rss/farc/rss2dc.xml")

    ;; obsolete フォーラム系
    ;; Business Computingフォーラム
    ("fbiz"  . "http://www.atmarkit.co.jp/rss/fbiz/rss2dc.xml")
    ;; ＠IT自分戦略研究所
    ("jibun" . "http://jibun.atmarkit.co.jp/rss/rss2dc.xml")
    ))

(luna-define-method shimbun-groups ((shimbun shimbun-atmarkit))
  (mapcar 'car shimbun-atmarkit-group-path-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-atmarkit))
  (cdr (assoc (shimbun-current-group-internal shimbun)
	      shimbun-atmarkit-group-path-alist)))

(luna-define-method shimbun-article-url ((shimbun shimbun-atmarkit)
					 header)
  "http://www.atmarkit.co.jp/club/print/print.php")

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-atmarkit)
						 &optional range)
  (mapcar
   (lambda (header)
     (shimbun-header-set-xref header
			      (shimbun-real-url (shimbun-header-xref header)))
     header)
   (luna-call-next-method)))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-atmarkit)
					    header url)
  (goto-char (point-max))
  (when (re-search-backward
	 "<a href=\"\\([^\"]+\\)\"><img src=\"[^\"]*/images/next\\.gif\"" nil t)
    (shimbun-expand-url (match-string 1) url)))

(luna-define-method shimbun-multi-clear-contents ((shimbun shimbun-atmarkit)
						  header
						  has-previous-page
						  has-next-page)
  (when (shimbun-clear-contents shimbun header)
    (when has-next-page
      ;; Remove footer.
      (goto-char (point-min))
      (when (re-search-forward
	     "<img src=\"[^\"]*/images/\\(prev\\|next\\)\\.gif\"" nil t)
	(delete-region (line-beginning-position) (point-max))
	(insert "</tr></table>")))
    t))

(luna-define-method shimbun-clear-contents :before ((shimbun shimbun-atmarkit)
						    header)
  (shimbun-remove-tags "<script" "</script *>")
  (shimbun-remove-tags "<noscript" "</noscript *>")
  (shimbun-remove-tags "<form" "</form *>"))

(luna-define-method shimbun-article :before ((shimbun shimbun-atmarkit)
					     header &optional outbuf)
  (let ((xref (shimbun-header-xref header))
	end)
    (when (setq end (string-match "\\?" xref))
      (setq xref (substring xref 0 end)))
    (shimbun-header-set-xref header xref)))

(provide 'sb-atmarkit)

;;; sb-atmarkit.el ends here
