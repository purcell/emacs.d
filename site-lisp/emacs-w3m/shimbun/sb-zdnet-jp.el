;;; sb-zdnet-jp.el --- shimbun backend for ZDNet Japan -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2005, 2006 Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>

;; Author: Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>
;; Keywords: news
;; Created: Jun 14, 2003

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

;; This code is based on sb-cnet-jp.el@ 2005-04-07.

;; Thanks.
;;  NAKAJIMA Mikio     <minakaji@namazu.org>,
;;  TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;  Katsumi Yamaoka    <yamaoka@jpl.org>

;;; Code:

(require 'shimbun)
(require 'sb-multi)
(require 'sb-rss)

(luna-define-class shimbun-zdnet-jp (shimbun-japanese-newspaper
				     shimbun-multi shimbun-rss) ())

(defvar shimbun-zdnet-jp-group-alist
  '( ;; news
    ("news"	     . "http://japan.zdnet.com/rss/news/index.rdf")
    ("news.network"  . "http://japan.zdnet.com/rss/news/nw/index.rdf")
    ("news.hardware" . "http://japan.zdnet.com/rss/news/hardware/index.rdf")
    ("news.software" . "http://japan.zdnet.com/rss/news/software/index.rdf")
    ("news.manage"   . "http://japan.zdnet.com/rss/news/itm/index.rdf")
    ("news.security" . "http://japan.zdnet.com/rss/news/sec/index.rdf")
    ("news.internet" . "http://japan.zdnet.com/rss/news/internet/index.rdf")
    ("news.os"       . "http://japan.zdnet.com/rss/news/os/index.rdf")
    ("news.db"       . "http://japan.zdnet.com/rss/news/db/index.rdf")
    ("news.system"   . "http://japan.zdnet.com/rss/news/devsys/index.rdf")

    ;; column
    ("column"       . "http://japan.zdnet.com/rss/column/index.rdf")
    ("column.sp1" . "http://japan.zdnet.com/rss/column/sp1/index.rdf")
    ("column.netsecurity1"
     . "http://japan.zdnet.com/rss/column/netsecurity1/index.rdf")
    ("column.ea1" . "http://japan.zdnet.com/rss/column/ea1/index.rdf")
    ("column.btl" . "http://japan.zdnet.com/rss/column/btl/index.rdf")
    ("column.solutionIT"
     . "http://japan.zdnet.com/rss/column/solutionIT/index.rdf")

    ;; channel
    ("channel.security" . "http://japan.zdnet.com/rss/channel/sec/index.rdf")
    ("channel.ilm" . "http://japan.zdnet.com/rss/channel/ilm/index.rdf")

    ;; blog
    ("blog.iida" . "http://blog.japan.zdnet.com/iida/index.rdf")
    ("blog.mhatta" . "http://blog.japan.zdnet.com/mhatta/index.rdf")
    ("blog.kurei" . "http://blog.japan.zdnet.com/kurei/index.rdf")
    ("blog.opensource" . "http://blog.japan.zdnet.com/opensource/index.rdf")
    ("blog.soa"  . "http://blog.japan.zdnet.com/soa/index.rdf")
    ("blog.dp" . "http://blog.japan.zdnet.com/dp/index.rdf")))

(defvar shimbun-zdnet-jp-x-face-alist
  '(("default" . "X-Face: 0p7.+XId>z%:!$ahe?x%+AEm37Abvn]n\
*GGh+>v=;[3`a{1lqO[$,~3C3xU_ri>[JwJ!9l0\n ~Y`b*eXAQ:*q=bBI\
_=ro*?]4:|n>]ZiLZ2LEo^2nr('C<+`lO~/!R[lH'N'4X&%\\I}8T!wt")))

(luna-define-method shimbun-groups ((shimbun shimbun-zdnet-jp))
  (mapcar 'car shimbun-zdnet-jp-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-zdnet-jp))
  (cdr (assoc (shimbun-current-group shimbun) shimbun-zdnet-jp-group-alist)))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-zdnet-jp)
					    header url)
  (goto-char (point-min))
  (when (re-search-forward
	 "<a href=\"\\([^\"]+\\)\" class=\"article_leaf_paging_next\"" nil t)
    (shimbun-expand-url (match-string 1) url)))

(luna-define-method shimbun-multi-clear-contents ((shimbun shimbun-zdnet-jp)
						  header
						  has-previous-page
						  has-next-page)
  (let (start end)
    (if (or has-previous-page has-next-page)
	(setq start "<div class=\"leaf_body\">"
	      end "<div class=\"article_leaf_paging\">")
      (setq start "<div class=\"article_body\">"
	    end "</div><!--/article_body-->"))
    (goto-char (point-min))
    (when (and (search-forward start nil t)
	       (setq start (point))
	       (re-search-forward end nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start)
      t)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-zdnet-jp)
						   header)
  (goto-char (point-min))
  (when (re-search-forward "<div class=\"property cblack\">\\([^\n]+\\)</div>"
			   nil t)
    (let ((from (match-string 1)))
      (setq from (shimbun-replace-in-string from "文：" ""))
      (setq from (shimbun-replace-in-string from "翻訳校正：*" ""))
      (setq from (shimbun-replace-in-string from " *<br */?> *" ", "))
      (setq from (shimbun-replace-in-string from "、" ", "))
      (shimbun-header-set-from header from))))

(luna-define-method shimbun-footer :around ((shimbun shimbun-zdnet-jp)
					    header &optional html)
  (if (string-match "news" (shimbun-current-group shimbun))
      (luna-call-next-method)
    ""))

(provide 'sb-zdnet-jp)

;;; sb-zdnet-jp.el ends here
