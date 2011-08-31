;;; sb-nikkansports.el --- shimbun backend for www.nikkansports.com -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2007, 2008
;; MIYOSHI Masanori <miyoshi@meadowy.org>

;; Author: MIYOSHI Masanori <miyoshi@meadowy.org>
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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-nikkansports
		   (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-nikkansports-url "http://www.nikkansports.com/")

(defvar shimbun-nikkansports-server-name "日刊スポーツ")

(defvar shimbun-nikkansports-group-table
  '(("flash" "最新ニュース" "flash/flash-news.html")
    ("baseball" "野球" "baseball/news/backnumber-baseball.html")
    ("baseball.highschool" "高校野球"
     "baseball/highschool/news/backnumber-highschool.html")
    ("baseball.amateur" "大学・社会人野球"
     "baseball/amateur/news/backnumber-amateur.html")
    ("baseball.mlb" "ＭＬＢ" "baseball/mlb/news/backnumber-mlb.html")
    ("soccer" "サッカー" "soccer/news/backnumber-soccer.html")
    ("soccer.japan" "サッカー日本代表"
     "soccer/japan/news/backnumber-japan.html")
    ("soccer.world" "海外サッカー" "soccer/world/news/backnumber-world.html")
    ("sports" "スポーツ" "sports/news/backnumber-sports.html")
    ("sumo" "大相撲" "sports/sumo/news/backnumber-sumo.html")
    ("nba" "ＮＢＡ" "sports/nba/news/backnumber-nba.html")
    ("nfl" "ＮＦＬ" "sports/nfl/news/backnumber-nfl.html")
    ("nhl" "ＮＨＬ" "sports/nhl/news/backnumber-nhl.html")
    ("rugby" "ラグビー" "sports/rugby/news/backnumber-rugby.html")
    ("golf" "ゴルフ" "sports/golf/news/backnumber-golf.html")
    ("motor" "モータースポーツ" "sports/motor/news/backnumber-motor.html")
    ("battle" "格闘技" "battle/news/backnumber-battle.html")
    ("race" "競馬" "race/news/backnumber-race.html")
    ("race.kka" "競輪・競艇・オート" "race/kka/news/backnumber-kka.html")
    ("entertainment" "芸能" "entertainment/news/backnumber-entertainment.html")
    ("cinema" "シネマ" "entertainment/cinema/news/backnumber-cinema.html")
    ("general" "社会" "general/news/backnumber-general.html")))

(defvar shimbun-nikkansports-content-start
  "<[\t\n ]*![\t\n ]*-+[\t\n ]*\\++[\t\n ]*\
ニュース本文[\t\n ]*\\++[\t\n ]*-+[\t\n ]*>[\t\n ]*\
\\(?:\\(?:<[\t\n ]*/?[\t\n ]*[ads][^>]+>\
\\|<[\t\n ]*h[0-9]+[\t\n ]*>[^<]+<[\t\n ]*/[\t\n ]*h[0-9]+[\t\n ]*>\\)\
\[\t\n ]*\\)*")

(defvar shimbun-nikkansports-content-end
  "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\
<[\t\n ]*![\t\n ]*-+[\t\n ]*/[\t\n ]*\\++[\t\n ]*\
ニュース本文[\t\n ]*\\++[\t\n ]*-+[\t\n ]*>")

(defvar shimbun-nikkansports-expiration-days 17)

(defvar shimbun-nikkansports-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABAAAAAQAgMAAABinRfyAAAADFBMVEUDKpy11PIeeNv///+
 PA1z5AAAAP0lEQVQI12NgAAMOJgYG/n/2Fxj4a+0+MPDH2i4AEvYPgAQ3iLvvB5DYCyL0LzCsAgI
 kYm3u+yqGte9Td4G5AJiKHahMk6/LAAAAAElFTkSuQmCC")))

(luna-define-method shimbun-groups ((shimbun shimbun-nikkansports))
  (mapcar 'car shimbun-nikkansports-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-nikkansports))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-nikkansports-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-nikkansports))
  (shimbun-expand-url (nth 2 (assoc (shimbun-current-group-internal shimbun)
				    shimbun-nikkansports-group-table))
		      shimbun-nikkansports-url))

(luna-define-method shimbun-get-headers ((shimbun shimbun-nikkansports)
					 &optional range)
  (shimbun-nikkansports-get-headers shimbun range))

(defun shimbun-nikkansports-get-headers (shimbun range)
  (let ((regexp
	 (eval-when-compile
	   (concat
	    "<a[\t ]+href=\""
	    ;; 1. url
	    "\\([^\"]+/"
	    ;; 2. serial number
	    "\\([^/]+"
	    ;; 3. year
	    "\\(20[0-9][0-9]\\)"
	    ;; 4. month
	    "\\([01][0-9]\\)"
	    ;; 5. day
	    "\\([0-3][0-9]\\)"
	    "[^.]+\\)\\.html\\)\">[\t ]*"
	    ;; 6. subject
	    "\\([^<]+\\)"
	    "[\t ]*</a>[^\n]+\\[[\t ]*[0-9]+日[\t ]*"
	    ;; 7. time
	    "\\([012][0-9]:[0-5][0-9]\\)"
	    "[\t ]*\\]")))
	(group (shimbun-current-group-internal shimbun))
	(from (concat shimbun-nikkansports-server-name " ("
		      (shimbun-current-group-name shimbun) ")"))
	(case-fold-search t)
	headers)
    (setq group (mapconcat 'identity
			   (nreverse (split-string group "\\."))
			   "."))
    (while (re-search-forward regexp nil t)
      (push (shimbun-create-header
	     0 (match-string 6) from
	     (shimbun-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 5))
	      (match-string 7))
	     (concat "<"
		     (mapconcat 'identity
				(save-match-data
				  (split-string (match-string 2) "-"))
				".")
		     "%" group ".nikkansports.com>")
	     "" 0 0 (match-string 1))
	    headers))
    (shimbun-sort-headers headers)))

(luna-define-method shimbun-clear-contents :around ((shimbun
						     shimbun-nikkansports)
						    header)
  (shimbun-strip-cr)
  (goto-char (point-min))
  (when (luna-call-next-method)
    (unless (shimbun-prefer-text-plain-internal shimbun)
      (shimbun-break-long-japanese-lines))
    t))

(provide 'sb-nikkansports)

;;; sb-nikkansports.el ends here
