;;; sb-cnet-jp.el --- shimbun backend for CNET Japan -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2003, 2004, 2005, 2006, 2007
;; NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio     <minakaji@namazu.org>,
;;         TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Katsumi Yamaoka    <yamaoka@jpl.org>,
;;         Tsuyoshi CHO       <tsuyoshi_cho@ybb.ne.jp>
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

;;; Code:

(require 'shimbun)
(require 'sb-multi)
(require 'sb-rss)

(luna-define-class shimbun-cnet-jp (shimbun-multi shimbun-rss) ())

(defvar shimbun-cnet-jp-group-alist
  '(("general" . "http://feed.japan.cnet.com/rss/index.rdf")
    ("news" . "http://feed.japan.cnet.com/rss/news/index.rdf")
    ;;("release" . "http://release.japan.cnet.com/rss/index.rdf")
    ("special" . "http://feed.japan.cnet.com/rss/sp/index.rdf")
    ("opinion" . "http://feed.japan.cnet.com/rss/opinion/index.rdf")
    ;;("whitepaper" . "http://paper.japan.cnet.com/rss/index.rdf")
    ;;("review" . "http://review.japan.cnet.com/rss/index.rdf")
    ("blog.geetstate" . "http://blog.japan.cnet.com/geetstate/index.rdf")
    ("blog.kenn" . "http://blog.japan.cnet.com/kenn/index.rdf")
    ("blog.lessig" . "http://blog.japan.cnet.com/lessig/index.rdf")
    ("blog.matsumura" . "http://blog.japan.cnet.com/matsumura/index.rdf")
    ("blog.nakajima" . "http://blog.japan.cnet.com/nakajima/index.rdf")
    ("blog.saeki" . "http://blog.japan.cnet.com/saeki/index.rdf")
    ("blog.sakamoto" . "http://blog.japan.cnet.com/sakamoto/index.rdf")
    ("blog.sasaki" . "http://blog.japan.cnet.com/sasaki/index.rdf")
    ("blog.sentan" . "http://blog.japan.cnet.com/sentan/index.rdf")
    ("blog.staff" . "http://blog.japan.cnet.com/staff/index.rdf")
    ("blog.takawata" . "http://blog.japan.cnet.com/takawata/index.rdf")
    ("blog.watanabe" . "http://blog.japan.cnet.com/watanabe/index.rdf")))

(defvar shimbun-cnet-jp-orphaned-group-list
  '("blog.inoue"
    "blog.mori"
    "blog.umeda"
    "blog.editors"))

(defvar shimbun-cnet-jp-server-name "CNET Networks,Inc.")
(defvar shimbun-cnet-jp-content-start
  "<div class=\"\\(?:leaf\\|article\\)_body\\(?: \\(?:article\\|leaf\\)_body\\)?\">")
(defvar shimbun-cnet-jp-content-end
  "\\(<div \\(class=\"article_footer\"\\|id=\"bubble_tooltip\"\\)>\\|\
<!--h3>トラックバック一覧</h3-->\\)")
(defvar shimbun-cnet-jp-x-face-alist
  '(("default" . "X-Face: 0p7.+XId>z%:!$ahe?x%+AEm37Abvn]n\
*GGh+>v=;[3`a{1lqO[$,~3C3xU_ri>[JwJ!9l0\n ~Y`b*eXAQ:*q=bBI\
_=ro*?]4:|n>]ZiLZ2LEo^2nr('C<+`lO~/!R[lH'N'4X&%\\I}8T!wt")))

(luna-define-method shimbun-groups ((shimbun shimbun-cnet-jp))
  (nconc (mapcar 'car shimbun-cnet-jp-group-alist)
	 shimbun-cnet-jp-orphaned-group-list))

(luna-define-method shimbun-headers :around ((shimbun shimbun-cnet-jp)
					     &optional range)
  (unless (member (shimbun-current-group shimbun)
		  shimbun-cnet-jp-orphaned-group-list)
    (luna-call-next-method)))

(luna-define-method shimbun-index-url ((shimbun shimbun-cnet-jp))
  (cdr (assoc (shimbun-current-group shimbun) shimbun-cnet-jp-group-alist)))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-cnet-jp)
					    header url)
  (goto-char (point-min))
  (when (re-search-forward "<li class=\"next\"><a href=\"\\([^\"]+\\)\"" nil t)
    (shimbun-expand-url (match-string 1) url)))

(luna-define-method shimbun-clear-contents :before ((shimbun shimbun-cnet-jp)
						    header)
  (shimbun-strip-cr)
  (shimbun-remove-tags "<script" "</script>")
  (shimbun-remove-tags "<noscript" "</noscript>")
  (shimbun-remove-tags "<div class=\"photor_thumb_wrap\"" "</div>")
  (shimbun-remove-tags "<div class=\"block_infocnet_stb\">" "</div>")
  (shimbun-remove-tags "<div class=\"block_ad_print\">"
		       "<!-- block_ad_print END -->")
  (shimbun-remove-tags "<!--AD_ART_S-->" "<!--AD_ART_S END-->"))

(luna-define-method shimbun-multi-clear-contents ((shimbun shimbun-cnet-jp)
						  header
						  has-previous-page
						  has-next-page)
  (when (shimbun-clear-contents shimbun header)
    (cond
     (has-next-page
      (goto-char (point-max))
      (when (search-backward "<div class=\"navi_paging_alt\">" nil t)
	(delete-region (point) (point-max))))
     (has-previous-page
      (shimbun-remove-tags "<div class=\"navi_paging_alt\">"
			   "<!-- navi_paging_alt END -->")))
    t))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-cnet-jp)
						   header)
  (goto-char (point-min))
  (when (re-search-forward "\\(s.prop2=\"\\([^\"]+\\)\"\\|\
<dt +class=\"author\">\\([^\n]+\\)</dt>\\)" nil t)
    (let ((from (or (match-string 2) (match-string 3))))
      (setq from (shimbun-replace-in-string from "文：" ""))
      (setq from (shimbun-replace-in-string from "翻訳校正：*" ""))
      (setq from (shimbun-replace-in-string from " *<br */?> *" ", "))
      (setq from (shimbun-replace-in-string from "、" ", "))
      (shimbun-header-set-from header from))))

(provide 'sb-cnet-jp)

;;; sb-cnet-jp.el ends here
