;;; sb-yahoo.el --- shimbun backend for news.yahoo.co.jp -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001, 2002, 2003, 2005, 2006, 2007, 2009 Kazuyoshi KOREEDA

;; Author: Kazuyoshi KOREEDA <Koreeda.Kazuyoshi@jp.panasonic.com>,
;;         Katsumi Yamaoka <yamaoka@jpl.org>
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

;; Original code was sb-asahi.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-yahoo (shimbun) ())

(defvar shimbun-yahoo-prefer-text-plain nil
  "*Non-nil means prefer text/plain articles rather than html articles.")

(defvar shimbun-yahoo-url "http://headlines.yahoo.co.jp/")

(defvar shimbun-yahoo-groups-table
  (let* ((s0 "[\t\n ]*")
	 (s1 "[\t\n ]+")
	 (default (list
		   (concat
		    "<a" s1 "href=\"\\(?:[^\"]+\\)?"
		    ;; 1. url
		    "\\(http://headlines\\.yahoo\\.co\\.jp/hl\\?a="
		    ;; 2. serial number
		    "\\("
		    ;; 3. year
		    "\\(20[0-9][0-9]\\)"
		    ;; 4. month
		    "\\([01][0-9]\\)"
		    ;; 5. day
		    "\\([0-3][0-9]\\)"
		    "[^\"]*\\)\\)"
		    "\"" s0 ">" s0 "\\(?:<strong>" s0 "\\)?"
		    ;; 6. subject
		    "\\([^<]+\\)"
		    "\\(?:" s0 "</strong>\\)?"
		    s0 "</a>\\(?:\\(?:[^\n<（]*\\|[\t\n ]*\\)<[^>]+>\\)*" s0
		    "\\(?:（" s0 "\\(?:<a" s1 "[^>]+>" s0 "\\)?"
		    ;; 7. source
		    "\\([^<）]+\\)"
		    s0 "\\(?:</a>" s0 "\\)?"
		    s0 "）"
		    "\\(?:" s0 "-\\(?:[^<]+\)\\)?\
\\|" s0 "\\(?:<[^>]+>" s0 "\\)?\
\\(?:[01]?[0-9]月\\)?[0-3]?[0-9]日\\(?:([日月火水木金土])\\)?\\)?\
\\|[01]?[0-9]月[0-3]?[0-9]日\\(?:([日月火水木金土])\\)?\\)"
		    s0
		    ;; 8. hour
		    "\\([012]?[0-9]\\)"
		    s0 "時" s0
		    ;; 9. minute
		    "\\([0-5]?[0-9]\\)"
		    s0 "分"
		    "\\(?:\\(?:" s0 "</[^>]+>\\)?[^<]+<a" s1
		    "href=\"[^\">]+\">" s0
		    ;; 10. source
		    "\\([^<）]+\\)"
		    s0 "</a>\\)?")
		   1 2 3 4 5 6 7 8 9 10))
	 (topnews (list
		   (concat
		    "<a" s1 "href=\""
		    ;; 1. url
		    "\\(http://headlines\\.yahoo\\.co\\.jp/hl\\?a="
		    ;; 2. serial number
		    "\\("
		    ;; 3. year
		    "\\(20[0-9][0-9]\\)"
		    ;; 4. month
		    "\\([01][0-9]\\)"
		    ;; 5. day
		    "\\([0-3][0-9]\\)"
		    "[^\"]*\\)\\)"
		    "\"" s0 ">" s0
		    ;; 6. subject
		    "\\([^<]+\\)"
		    s0 "</a>\\(?:" s0 "<[^>]+>\\)*[^<]*)" s0
		    ;; 7. hour
		    "\\([012]?[0-9]\\)"
		    s0 "時" s0
		    ;; 8. minute
		    "\\([0-5]?[0-9]\\)"
		    s0 "分" "[^<]*\\(?:<a" s1 "[^>]+>" s0 "\\)?"
		    ;; 9. source
		    "\\([^<）]+\\)")
		   1 2 3 4 5 6 9 7 8)))
    `(("topnews" "トップ" "topnews" ,@topnews)
      ("news" "ニュース" news ,@default)
      ("politics" "政治" "pol" ,@default)
      ("society" "社会" "soci" ,@default)
      ("people" "人" "peo" ,@default)
      ("business-all" "経済総合" "bus_all" ,@default)
      ("market" "市況" "brf" ,@default)
      ("stock" "株式" "biz" ,@default)
      ("industry" "産業" "ind" ,@default)
      ("international" "海外" "int" ,@default)
      ("entertainment" "エンターテインメント" "ent" ,@default)
      ("sports" "スポーツ" "spo" ,@default)
      ("computer" "コンピュータ" "sci" ,@default)
      ("zenkoku" "全国" "loc" ,@default)
      ("hokkaido" "北海道" "hok" ,@default)
      ("aomori" "青森" "l02" ,@default) ;; not "102" but "l02" ;-)
      ("iwate" "岩手" "l03" ,@default)
      ("miyagi" "宮城" "l04" ,@default)
      ("akita" "秋田" "l05" ,@default)
      ("yamagata" "山形" "l06" ,@default)
      ("fukushima" "福島" "l07" ,@default)
      ("tokyo" "東京" "l13" ,@default)
      ("kanagawa" "神奈川" "l14" ,@default)
      ("chiba" "千葉" "l12" ,@default)
      ("saitama" "埼玉" "l11" ,@default)
      ("ibaraki" "茨城" "l08" ,@default)
      ("tochigi" "栃木" "l09" ,@default)
      ("gunma" "群馬" "l10" ,@default)
      ("yamanashi" "山梨" "l19" ,@default)
      ("nagano" "長野" "l20" ,@default)
      ("niigata" "新潟" "l15" ,@default)
      ("toyama" "富山" "l16" ,@default)
      ("ishikawa" "石川" "l17" ,@default)
      ("fukui" "福井" "l18" ,@default)
      ("aichi" "愛知" "l23" ,@default)
      ("gifu" "岐阜" "l21" ,@default)
      ("shizuoka" "静岡" "l22" ,@default)
      ("mie" "三重" "l24" ,@default)
      ("osaka" "大阪" "l27" ,@default)
      ("hyogo" "兵庫" "l28" ,@default)
      ("kyoto" "京都" "l26" ,@default)
      ("shiga" "滋賀" "l25" ,@default)
      ("nara" "奈良" "l29" ,@default)
      ("wakayama" "和歌山" "l30" ,@default)
      ("tottori" "鳥取" "l31" ,@default)
      ("shimane" "島根" "l32" ,@default)
      ("okayama" "岡山" "l33" ,@default)
      ("hiroshima" "広島" "l34" ,@default)
      ("yamaguchi" "山口" "l35" ,@default)
      ("tokushima" "徳島" "l36" ,@default)
      ("kagawa" "香川" "l37" ,@default)
      ("ehime" "愛媛" "l38" ,@default)
      ("kochi" "高知" "l39" ,@default)
      ("fukuoka" "福岡" "l40" ,@default)
      ("saga" "佐賀" "l41" ,@default)
      ("nagasaki" "長崎" "l42" ,@default)
      ("kumamoto" "熊本" "l43" ,@default)
      ("oita" "大分" "l44" ,@default)
      ("miyazaki" "宮崎" "l45" ,@default)
      ("kagoshima" "鹿児島" "l46" ,@default)
      ("okinawa" "沖縄" "oki" ,@default)))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.  Where numbers point to the search result in order
of [0]url, [1]serial number, [2]year, [3]month, [4]day, [5]subject,
\[6]news source, [7]hour, [8]minute, and [9]news source (the last one
may not be presented).")

(defvar shimbun-yahoo-groups
  (mapcar 'car shimbun-yahoo-groups-table))

(defvar shimbun-yahoo-from-address "nobody@example.com")
(defvar shimbun-yahoo-content-start ">[\t\n ]*[01]*[0-9]月[0-3]?[0-9]日\
\[012]?[0-9]時[0-5]?[0-9]分配信[\t\n ]*\\(?:&nbsp\;[\t\n ]*\\)?<a[\t\n ]+\
href=\"[^\">]+\">[^<]+</a>\\(?:[\t\n ]*</[^>]+>\\)*[\t\n ]*")

(defvar shimbun-yahoo-content-end "[\t\n 　]*\
\\(?:【関連\
\\(?:キーワード\\|サイト\\|ニュース\\|・\\|作品\\|情報\\|記事\\|：\\)[^】]*\
】\
\\|【関連】\
\\|\\(?:<br>[\t\n ]*\\)*■関連記事<br>\
\\|<!-+[\t\n ]*interest_match_relevant_zone_end[\t\n ]*-+>\\)")

(defvar shimbun-yahoo-x-face-alist
  '(("default" . "X-Face: \"Qj}=TahP*`:b#4o_o63:I=\"~wbql=kpF1a>Sp62\
fpAsVY`saZV[b*GqI!u|i|xKPjNh&P=\n R?n}rh38mkp_:')h=Bh:Rk>0pYF\\I?f\\\
PvPs3>/KG:03n47U?FC[?DNAR4QAQxE3L;m!L10OM$-]kF\n YD\\]-^qzd#'{(o2cu,\
\(}CMi|3b9JDQ(^D\\:@DE}d2+0S2G{VS@E*1Og7Vj#35[77\"z9XBq9$1uF$+W\n u")))
(defvar shimbun-yahoo-expiration-days 7)

(luna-define-method shimbun-index-url ((shimbun shimbun-yahoo))
;;;<DEBUG>
;;  (shimbun-yahoo-index-url shimbun))
;;
;;(defun shimbun-yahoo-index-url (shimbun)
;;;</DEBUG>
  (let ((group (shimbun-current-group-internal shimbun))
	(url (shimbun-url-internal shimbun)))
    (if (string-equal group "news")
	(concat url "hl")
      (format "%shl?c=%s&t=l"
	      url
	      (nth 2 (assoc group shimbun-yahoo-groups-table))))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yahoo)
					 &optional range)
;;;<DEBUG>
;;  (shimbun-yahoo-get-headers shimbun range))
;;
;;(defun shimbun-yahoo-get-headers (shimbun range)
;;;</DEBUG>
  (let* ((case-fold-search t)
	 (from "Yahoo!ニュース")
	 (group (shimbun-current-group-internal shimbun))
	 (numbers (cdr (assoc group shimbun-yahoo-groups-table)))
	 (jname (pop numbers))
	 (regexp (progn (setq numbers (cdr numbers)) (pop numbers)))
	 (pages (shimbun-header-index-pages range))
	 (count 0)
	 (index (shimbun-index-url shimbun))
	 id headers start)
    (catch 'stop
      (while t
	(if (string-equal group "news")
	    (progn
	      (when (and (re-search-forward
			  "<!-+[\t\n ]*main[\t\n ]+start[\t\n ]*-+>" nil t)
			 (progn
			   (setq start (match-end 0))
			   (re-search-forward
			    "<!-+[\t\n ]*main[\t\n ]+end[\t\n ]*-+>" nil t)))
		(delete-region (match-beginning 0) (point-max))
		(delete-region (point-min) start)
		(goto-char (point-min)))
	      (when (and (re-search-forward ">[\t\n ]*写真ニュース[\t\n ]*</a>\
\\(?:[\t\n ]*</[^>]+>\\)+[\y\n ]*\\(<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
class=\"ymuiContainer\"\\)" nil t)
			 (shimbun-end-of-tag "div"))
		(delete-region (match-beginning 0) (match-end 0))))
	  (shimbun-remove-tags "<!-+[\t\n ]*アクセスランキング[\t\n ]*-+>"
			       "<!-+[\t\n ]*/アクセスランキング[\t\n ]*-+>"))
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (setq id (concat "<"
			   (save-match-data
			     (shimbun-replace-in-string
			      (match-string (nth 1 numbers))
			      "-" "."))
			   "%" group ".headlines.yahoo.co.jp>"))
	  (unless (and (shimbun-search-id shimbun id)
		       (if (and (>= count 1) ;; We're in the next page.
				;; Stop fetching iff range is not specified.
				(not pages))
			   (throw 'stop nil)
			 t))
	    (if (save-match-data
		  (string-match "記事全文[\t\n ]*\\'"
				(match-string (nth 5 numbers))))
		(goto-char (match-end (nth 5 numbers)))
	      (push (shimbun-create-header
		     0
		     (match-string (nth 5 numbers))
		     (concat from " (" jname "/"
			     (or (match-string (nth 6 numbers))
				 (match-string (nth 9 numbers)))
			     ")")
		     (shimbun-make-date-string
		      (string-to-number (match-string (nth 2 numbers)))
		      (string-to-number (match-string (nth 3 numbers)))
		      (string-to-number (match-string (nth 4 numbers)))
		      (format
		       "%02d:%02d"
		       (string-to-number (match-string (nth 7 numbers)))
		       (string-to-number (match-string (nth 8 numbers)))))
		     id "" 0 0
		     (match-string (nth 0 numbers)))
		    headers))))
	(setq count (1+ count))
	(goto-char (point-min))
	(cond ((and pages (>= count pages))
	       (throw 'stop nil))
	      ((string-equal group "news")
	       (if (>= count 2)
		   (throw 'stop nil)
		 (erase-buffer)
		 (shimbun-retrieve-url
		  "http://headlines.yahoo.co.jp/hl?c=flash"
		  t)))
	      ((re-search-forward "<a href=\"\\([^\"]+\\)\">次のページ</a>"
				  nil t)
	       (shimbun-retrieve-url (prog1
					 (match-string 1)
				       (erase-buffer))
				     t))
	      ((and (re-search-forward "<!-+[\t\n ]*過去記事[\t\n ]*-+>"
				       nil t)
		    (progn
		      (setq start (match-end 0))
		      (re-search-forward "<!-+[\t\n ]*/過去記事[\t\n ]*-+>"
					 nil t))
		    (progn
		      (narrow-to-region start (match-beginning 0))
		      (goto-char start)
		      (or (re-search-forward "<option[\t\n ]+value=\"\
20[0-9][0-9][01][0-9][0-3][0-9]\"[\t\n ]+selected[\t\n ]*>"
					     nil t)
			  (re-search-forward "<option[\t\n ]+value=\"\
20[0-9][0-9][01][0-9][0-3][0-9]\"[\t\n ]*>"
					     nil t)))
		    (re-search-forward "<option[\t\n ]+value=\"\
\\(20[0-9][0-9][01][0-9][0-3][0-9]\\)\"[\t\n ]*>"
				       nil t))
	       (shimbun-retrieve-url (prog1
					 (concat index "&d=" (match-string 1))
				       (erase-buffer))
				     t))
	      (t
	       (throw 'stop nil)))))
    (shimbun-sort-headers headers)))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-yahoo)
						    header)
;;;<DEBUG>
;;  (shimbun-yahoo-clear-contents shimbun header))
;;
;;(defun shimbun-yahoo-clear-contents (shimbun header)
;;;</DEBUG>
  (when (luna-call-next-method)
    ;; Remove garbage.
    (when (re-search-forward "\
\[\t\n ]*<tr>[\t\n ]*<td\\(?:[\t\n ]+[^>]+\\)*[\t\n ]+class=\
\"[^\"]+[\t\n ]+yjSt\"[^>]*>[\t\n ]*<a[\t\n ]+[^>]+>\
\[\t\n ]*拡大写真[\t\n ]*</a>[\t\n ]*</td>[\t\n ]*</tr>[\t\n ]*"
			     nil t)
      (replace-match "\n")
      (goto-char (point-min)))
    (when (re-search-forward "\
\[\t\n ]*<tr>[\t\n ]*<td\\(?:[\t\n ]+[^>]+\\)*[\t\n ]+class=\
\"[^\"]*Caption[^\"]*[\t\n ]+yjSt\"[^>]*>[^<]+</td>[\t\n ]*</tr>[\t\n ]*"
			     nil t)
      (replace-match "\n"))
    t))

(provide 'sb-yahoo)

;;; sb-yahoo.el ends here
