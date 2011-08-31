;;; sb-itmedia.el --- shimbun backend for ITmedia -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
;; Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>
;;         ARISAWA Akihiro    <ari@mbf.sphere.ne.jp>
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
(require 'sb-rss)
(require 'sb-multi)

(luna-define-class shimbun-itmedia (shimbun-multi shimbun-rss) ())

(defvar shimbun-itmedia-group-alist
  `(,@(mapcar
       (lambda (group)
	 (list (concat "news." group) (concat "news_" group)))
       '("bursts" "domestic" "foreign" "products" "technology" "web20"
	 "nettopics" "society" "security" "industry" "research" "sp_amd"))
    ("anchordesk" "anchordesk")
    ("bizid" "bizid")
    ("enterprise" "enterprise")
    ,@(mapcar
       (lambda (group)
	 (list (concat "+D." group) group))
       '("plusd" "mobile" "pcupdate" "lifestyle" "games" "docomo" "au_kddi"
	 "vodafone" "shopping"))
    ,@(mapcar
       (lambda (def)
	 (nconc (list (concat "+D.lifestyle.column." (car def)) nil) def))
       '(("asakura" "麻倉怜士"
	  "http://www.itmedia.co.jp/keywords/emma.html")
	 ("honda" "本田雅一")
	 ("kobayashi" "こばやしゆたか")
	 ("kodera" "小寺信良")
	 ("nishi" "西正")
	 ("ogikubo" "荻窪圭"
	  "http://plusd.itmedia.co.jp/lifestyle/features/satuei/")
	 ("tachibana" "橘十徳"
	  "http://plusd.itmedia.co.jp/lifestyle/features/jibara/")
	 ("takemura" "竹村譲")
	 ("unakami" "海上忍"
	  "http://plusd.itmedia.co.jp/lifestyle/features/keyword/")))))

(defvar shimbun-itmedia-x-face-alist
  '(("\\+D" . "X-Face: #Ur~tK`JhZFFHPEVGKEi`MS{55^~&^0KUuZ;]-@WQ[8\
@,Ex'EeAAE}6xF<K#]pULF@5r24J
 |8/oP)(lCAzF0}.C@@}!k8!Qiz6b{]V")
    ("default" . "X-Face: %JzFW&0lP]xKGl{Bk3\\`yC0zZp|!;\\KT9'rqE2AIk\
R[TQ[*i0d##D=I3|g`2yr@sc<pK1SB
 j`}1YEnKc;U0:^#LQB*})Q}y=45<lIE4q<gZ88e2qS8a@Tys6S")))

(defvar shimbun-itmedia-content-start
  "<div class=\"body-rap\">\\|<div id=\"wrIcon\">")
(defvar shimbun-itmedia-content-end "<div class=\"credit-rap\">")

(defvar shimbun-itmedia-retry-fetching 1)
(defvar shimbun-itmedia-ignored-subject "^PR:")

(luna-define-method initialize-instance :after ((shimbun shimbun-itmedia)
						&rest init-args)
  (shimbun-rss-initialize-ignored-subject shimbun))

(luna-define-method shimbun-groups ((shimbun shimbun-itmedia))
  (mapcar 'car shimbun-itmedia-group-alist))

(luna-define-method shimbun-from-address ((shimbun shimbun-itmedia))
  (let ((group (shimbun-current-group-internal shimbun)))
    (format "ITmedia (%s)"
	    (or (nth 3 (assoc group shimbun-itmedia-group-alist)) group))))

(luna-define-method shimbun-index-url ((shimbun shimbun-itmedia))
  (let ((def (assoc (shimbun-current-group-internal shimbun)
		    shimbun-itmedia-group-alist)))
    (or (nth 4 def)
	(if (nth 1 def)
	    (format "http://rss.itmedia.co.jp/rss/2.0/%s.xml" (nth 1 def))
	  (format "http://plusd.itmedia.co.jp/lifestyle/column/%s.html"
		  (nth 2 def))))))

(luna-define-method shimbun-headers :around ((shimbun shimbun-itmedia)
					     &optional range)
  (if (string-match "\\.xml\\'" (shimbun-index-url shimbun))
      ;; Use the function defined in sb-rss.el.
      (luna-call-next-method)
    ;; Use the default function defined in shimbun.el.
    (funcall (intern "shimbun-headers"
		     (luna-class-obarray (luna-find-class 'shimbun)))
	     shimbun range)))

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-itmedia)
						 &optional range)
  (if (string-match "\\.xml\\'" (shimbun-index-url shimbun))
      (luna-call-next-method)
    (let ((case-fold-search t)
	  (group (nth 2 (assoc (shimbun-current-group-internal shimbun)
			       shimbun-itmedia-group-alist)))
	  (from (shimbun-from-address shimbun))
	  headers)
      (goto-char (point-min))
      (let ((regexp "\
<!-+[\t\n ]*cms[\t\n /]+index\\(?:[\t\n ]+[^\t\n >-]+\\)?[\t\n ]*-+>[\t\n ]*"))
	(when (re-search-forward regexp nil t)
	  (delete-region (point-min) (match-end 0)))
	(goto-char (point-max))
	(when (re-search-backward regexp nil t)
	  (delete-region (match-beginning 0) (point-max))))
      (goto-char (point-min))
      (while (re-search-forward "<a[\t\n ]+href=\"\
\\(?:[^\"]+\\)?\\(/\\(?:lifestyle\\|pcupdate\\)/articles/\
\\([0-9][0-9]\\)\\([01][0-9]\\)/\\([0-3][0-9]\\)/news\\([0-9]+\\)\\.html\\)\
\"[\t\n ]*>\\(?:[\t\n ]*\\|[\t\n ]*<strong>[\t\n ]*\\)\\([^<]+\\)"
				nil t)
	(push (shimbun-create-header
	       0 (match-string 6) from
	       (shimbun-make-date-string
		(+ (string-to-number (match-string 2)) 2000)
		(string-to-number (match-string 3))
		(string-to-number (match-string 4)))
	       (concat
		"<20" (match-string 2) (match-string 3) (match-string 4)
		"." (match-string 5) "." group
		".column.lifestyle@itmedia.shimbun.namazu.org>")
	       "" 0 0
	       (concat "http://plusd.itmedia.co.jp" (match-string 1)))
	      headers))
      headers)))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-itmedia)
					    header url)
  (goto-char (point-min))
  (when (re-search-forward
	 "<b><a href=\"\\([^\"]+\\)\">次のページ</a></b>\
\\|<span id=\"next\"><a href=\"\\([^\"]+\\)\">次のページへ</a></span>" nil t)
    (let ((next (or (match-string 1) (match-string 2))))
      (prog1
	  (shimbun-expand-url next url)
	;; Remove navigation button.
	(goto-char (point-min))
	(when (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*id=\"notice\""
				      nil t)
		   (shimbun-end-of-tag "div" t)
		   (save-match-data
		     (re-search-backward (concat "[\t\n ]href=\""
						 (regexp-quote next)
						 "\"")
					 (match-beginning 0) t)))
	  (replace-match "\n"))))))

(luna-define-method shimbun-multi-clear-contents ((shimbun shimbun-itmedia)
						  header
						  has-previous-page
						  has-next-page)
  (let (credit)
    (when (and (not has-previous-page)
	       (progn
		 (goto-char (point-min))
		 (re-search-forward "<!--■クレジット-->[\t\n ]*" nil t))
	       (looking-at "<p\\( [^\n>]+>[^\n]+</\\)p>"))
      (setq credit (match-string 1))
      (when (string-match "<b>\\[ITmedia\\]</b>" credit)
	(setq credit nil)))
    (when (shimbun-clear-contents shimbun header)
      (goto-char (point-min))
      (when credit
	(insert "<div" credit "div>\n"))
      ;; Remove navigation buttons.
      (while (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"ctrl\""
				     nil t)
		  (shimbun-end-of-tag "div" t)
		  (save-match-data
		    (re-search-backward "[次前]のページへ"
					(match-beginning 0) t)))
	(replace-match "\n"))
      (goto-char (point-min))
      (when has-previous-page
	(insert "&#012;") ;; ^L
	;; Remove tags that likely cause a newline preceding a page.
	(when (and (looking-at "[\t\n ]*<\\(h[0-9]+\\|p\\)[\t\n >]")
		   (shimbun-end-of-tag (match-string 1) t))
	  (replace-match "\n\\3\n")))
      t)))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-itmedia)
						    header)
  (or (luna-call-next-method)
      (prog1
	  (let ((case-fold-search t)
		icon start)
	    (goto-char (point-min))
	    (when (and (re-search-forward "<div\\(?:[\t\n ]+[^\t\n >]+\\)*\
\[\t\n ]+class=\"article-icon\""
					  nil t)
		       (shimbun-end-of-tag "div"))
	      (setq icon (match-string 0)))
	    (goto-char (point-min))
	    (when (and (search-forward "<!--BODY-->" nil t)
		       (progn
			 (setq start (match-end 0))
			 (when (and (re-search-backward "<h[0-9]>[^<]+</h[0-9]>"
							nil t)
				    (progn
				      (goto-char (match-end 0))
				      (not (re-search-forward "<h[0-9]>" start t))))
			   (delete-region (match-end 0) start)
			   (setq start (match-beginning 0)))
			 (re-search-forward "<!--BODY ?END-->" nil t)))
	      (delete-region (match-beginning 0) (point-max))
	      (delete-region (point-min) start)
	      ;; Remove anchors to both the next page and the previous page.
	      ;; These anchors are inserted into the head and the tail of the
	      ;; article body.
	      (skip-chars-backward " \t\r\f\n")
	      (forward-line 0)
	      (when (looking-at "<P ALIGN=\"CENTER\"><[AB]")
		(delete-region (point) (point-max)))
	      (goto-char (point-min))
	      (skip-chars-forward " \t\r\f\n")
	      (when (looking-at "<P ALIGN=\"CENTER\"><[AB]")
		(delete-region (point-min) (point-at-eol)))
	      (when icon
		(goto-char (point-min))
		(insert icon "\n"))
	      t))
	(shimbun-remove-tags "<!-- AD START -->" "<!-- AD END -->")
	(shimbun-remove-tags "\
<IMG [^>]*SRC=\"http:/[^\"]*/\\(ad\\.itmedia\\.co\\.jp\\|\
a1100\\.g\\.akamai\\.net\\)/[^>]+>")
	(shimbun-remove-tags "\
<A [^>]*HREF=\"http:/[^\"]*/\\(ad\\.itmedia\\.co\\.jp\\|\
a1100\\.g\\.akamai\\.net\\)/[^>]+>[^<]*</A>")

	;; Insert line-break after images.
	(goto-char (point-min))
	(let (start md)
	  (while (re-search-forward
		  "\\(<img[\t\n ]+[^>]+>\\(?:[\t\n ]*<[^>]+>\\)*\\)[\t\n ]*"
		  nil t)
	    (when (or
		   ;; Look forward for </a>.
		   (progn
		     (setq start (point)
			   md (match-data))
		     (and (re-search-forward "<a[\t\n ]+\\|\\(</a>\\)[\t\n ]*"
					     nil t)
			  (or (match-beginning 1)
			      (progn
				(goto-char start)
				(set-match-data md)
				nil))))
		   ;; Look backward for </foo>.
		   (re-search-backward "\\(</[^>]+>\\)[\t\n ]*"
				       (match-beginning 1) t))
	      (goto-char (match-end 0)))
	    (unless
		;; Check if there's a tag that is likely to cause the line-break.
		(looking-at "\\(?:<![^>]+>[\t\n ]*\\)*\
<\\(?:br\\|div\\|h[0-9]+\\|p\\)\\(?:[\t\n ]*>\\|[\t\n ]\\)")
	      (replace-match "\\1<br>\n"))))

	(let ((hankaku (shimbun-japanese-hankaku shimbun)))
	  (when (and hankaku (not (memq hankaku '(header subject))))
	    (shimbun-japanese-hankaku-buffer t))))))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-itmedia)
						   header)
  (when (re-search-forward "\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日 \
\\([0-9]+\\)時\\([0-9]+\\)分 更新" nil t)
    (shimbun-header-set-date
     header
     (shimbun-make-date-string
      (string-to-number (match-string 1))
      (string-to-number (match-string 2))
      (string-to-number (match-string 3))
      (concat (match-string 4) ":" (match-string 5))))))

(luna-define-method shimbun-footer :around ((shimbun shimbun-itmedia)
					    header &optional html)
  (if html
      (concat "<div align=\"left\">\n--&nbsp;<br>\n\
この記事の諸権利は&nbsp;ITmedia&nbsp;または情報の提供元に帰属します。<br>
原物は<a href=\""
	      (shimbun-article-base-url shimbun header)
	      "\"><u>ここ</u></a>で公開されています。\n</div>\n")
    (concat "-- \n\
この記事の諸権利は ITmedia または情報の提供元に帰属します。\n\
原物は以下の場所で公開されています:\n"
	    (shimbun-article-base-url shimbun header) "\n")))

(provide 'sb-itmedia)

;;; sb-itmedia.el ends here
