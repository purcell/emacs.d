;;; sb-asahi-mytown.el --- mytown.asahi.com -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2004, 2005, 2006, 2007 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
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

(luna-define-class shimbun-asahi-mytown
		   (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-asahi-mytown-group-table
  '(("hokkaido" "北海道" "0100000")
    ("aomori" "青森" "0200000")
    ("iwate" "岩手" "0300000")
    ("miyagi" "宮城" "0400000")
    ("akita" "秋田" "0500000")
    ("yamagata" "山形" "0600000")
    ("fukushima" "福島" "0700000")
    ("ibaraki" "茨城" "0800000")
    ("tochigi" "栃木" "0900000")
    ("gunma" "群馬" "1000000")
    ("saitama" "埼玉" "1100000")
    ("chiba" "千葉" "1200000")
    ("tokyo" "東京" "1300000")
    ("tama" "多摩" "1400000")
    ("kanagawa" "神奈川" "1500000")
    ("niigata" "新潟" "1600000")
    ("toyama" "富山" "1700000")
    ("ishikawa" "石川" "1800000")
    ("fukui" "福井" "1900000")
    ("yamanashi" "山梨" "2000000")
    ("nagano" "長野" "2100000")
    ("gifu" "岐阜" "2200000")
    ("shizuoka" "静岡" "2300000")
    ("aichi" "愛知" "2400000")
    ("mie" "三重" "2500000")
    ("shiga" "滋賀" "2600000")
    ("kyoto" "京都" "2700000")
    ("osaka" "大阪" "2800000")
    ("hyogo" "兵庫" "2900000")
    ("nara" "奈良" "3000000")
    ("wakayama" "和歌山" "3100000")
    ("tottori" "鳥取" "3200000")
    ("shimane" "島根" "3300000")
    ("okayama" "岡山" "3400000")
    ("hiroshima" "広島" "3500000")
    ("yamaguchi" "山口" "3600000")
    ("tokushima" "徳島" "3700000")
    ("kagawa" "香川" "3800000")
    ("ehime" "愛媛" "3900000")
    ("kochi" "高知" "4000000")
    ("fukuoka" "福岡・北九州" "4100000")
    ("saga" "佐賀" "4200000")
    ("nagasaki" "長崎" "4300000")
    ("kumamoto" "熊本" "4400000")
    ("oita" "大分" "4500000")
    ("miyazaki" "宮崎" "4600000")
    ("kagoshima" "鹿児島" "4700000")
    ("okinawa" "沖縄" "4800000"))
  "Alist of group names, their Japanese translations and ids.")

(defvar shimbun-asahi-mytown-server-name "朝日新聞")

(defvar shimbun-asahi-mytown-top-level-domain "mytown.asahi.com"
  "Name of the top level domain for the Mytown Asahi Shimbun.")

(defvar shimbun-asahi-mytown-url
  (concat "http://" shimbun-asahi-mytown-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-asahi-mytown-expiration-days 6)

(defvar shimbun-asahi-mytown-content-start
  "<!--[\t\n ]*Start of photo[\t\n ]*-->\\|<!--★★写真ここから★★-->\
\\|<!--[\t\n ]*Start of Kiji[\t\n ]*-->\\|<!--★★本文ここから★★-->")

(defvar shimbun-asahi-mytown-content-end
  "<!--★★本文ここまで★★-->\\|<!--[\t\n ]*End of Kiji[\t\n ]*-->")

(defvar shimbun-asahi-mytown-x-face-alist
  '(("default" . "X-Face: +Oh!C!EFfmR$+Zw{dwWW]1e_>S0rnNCA*CX|\
bIy3rr^<Q#lf&~ADU:X!t5t>gW5)Q]N{Mmn\n L]suPpL|gFjV{S|]a-:)\\FR\
7GRf9uL:ue5_=;h{V%@()={uTd@l?eXBppF%`6W%;h`#]2q+f*81n$B\n h|t")))

(luna-define-method shimbun-groups ((shimbun shimbun-asahi-mytown))
  (mapcar 'car shimbun-asahi-mytown-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-asahi-mytown))
  (concat "朝日マイタウン ("
	  (nth 1 (assoc (shimbun-current-group-internal shimbun)
			shimbun-asahi-mytown-group-table))
	  ")"))

(luna-define-method shimbun-index-url ((shimbun shimbun-asahi-mytown))
  (let ((group (shimbun-current-group-internal shimbun)))
    (concat shimbun-asahi-mytown-url
	    group
	    "/newslist.php?d_id="
	    (nth 2 (assoc group shimbun-asahi-mytown-group-table)))))

(defun shimbun-asahi-mytown-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (shimbun-current-group-name shimbun))
	(case-fold-search t)
	cyear cmonth url id subject month day year headers)
    (setq cyear (shimbun-decode-time nil 32400)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear))
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n 　]*")
		    (s1 "[\t\n ]+")
		    (no-nl "[^\n<>]+"))
		(concat
		 "<a" s1 "href=\""
		 ;; 1. url
		 "\\(news\\.php\\?k_id="
		 ;; 2. id
		 "\\([0-9]+\\)"
		 "\\)"
		 "\">" s0
		 ;; 3. subject
		 "\\(" no-nl "\\)"
		 s0 "</a>" s0 "\\(?:<[^>]+>" s0 "\\)(" s0
		 ;; 4. month
		 "\\([01][0-9]\\)"
		 s0 "/" s0
		 ;; 5. day
		 "\\([0-3][0-9]\\)"
		 s0 ")")))
	    nil t)
      (setq url (shimbun-expand-url (concat group "/" (match-string 1))
				    shimbun-asahi-mytown-url)
	    id (match-string 2)
	    subject (match-string 3)
	    month (string-to-number (match-string 4))
	    day (string-to-number (match-string 5))
	    year (cond ((>= (- month cmonth) 2)
			(1- cyear))
		       ((and (= 1 month) (= 12 cmonth))
			(1+ cyear))
		       (t
			cyear)))
      (push (shimbun-create-header
	     ;; number
	     0
	     ;; subject
	     subject
	     ;; from
	     from
	     ;; date
	     (shimbun-make-date-string year month day)
	     ;; id
	     (format "<%d%02d%02d.%s%%%s.%s>"
		     year month day id group
		     shimbun-asahi-mytown-top-level-domain)
	     ;; references, chars, lines
	     "" 0 0
	     ;; xref
	     url)
	    headers))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-asahi-mytown)
					 &optional range)
  (shimbun-asahi-mytown-get-headers shimbun))

(defun shimbun-asahi-mytown-prepare-article ()
  "Remove trailing empty lines."
  (let ((case-fold-search t)
	end start)
    (when (and (re-search-forward shimbun-asahi-mytown-content-start nil t)
	       (re-search-forward shimbun-asahi-mytown-content-end nil t))
      (setq end (goto-char (match-beginning 0))
	    start end)
      (while (and (re-search-backward "[\t\n\r ]*\\(?:<[^>]+>[\t\n\r ]*\\)"
				      nil t)
		  (if (= (match-end 0) start)
		      (setq start (match-beginning 0))
		    (delete-region (goto-char start) end)
		    (insert "\n")
		    nil))))))

(luna-define-method shimbun-make-contents
  :before ((shimbun shimbun-asahi-mytown) header)
  (shimbun-asahi-mytown-prepare-article))

(provide 'sb-asahi-mytown)

;;; sb-asahi-mytown.el ends here
