;;; sb-kantei.el --- shimbun backend for kantei mail magazine backnumber -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
;; Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
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

(luna-define-class shimbun-kantei (shimbun) ())

(defvar shimbun-kantei-url "http://www.mmz.kantei.go.jp/")

(defvar shimbun-kantei-groups '("m-magazine-en"
				"m-magazine-ja"
				"m-magazine-cn.hatoyama"
				"m-magazine-kr.hatoyama"
				"m-magazine-en.hatoyama"
				"m-magazine-ja.hatoyama"
				"m-magazine-en.aso"
				"m-magazine-ja.aso"
				"m-magazine-en.fukuda"
				"m-magazine-ja.fukuda"
				"m-magazine-en.abe"
				"m-magazine-ja.abe"
				"m-magazine-en.koizumi"
				"m-magazine-ja.koizumi"
				;; Backward compatibility.
				"m-magazine"
				"m-magazine-cn"
				"m-magazine-kr")
  "List of the groups subscribing to the email magazin of Japan's Cabinet.
Note that the `m-magazine-ja.koizumi' is the same as `m-magazine'
which is for the backward compatibility; so are `m-magazine-cn' and
`m-magazine-kr' for those of Hatoyama.")

(defvar shimbun-kantei-x-face-alist
  ;; Don't change the order of the faces.  See the method function that
  ;; is applied to `shimbun-make-contents'.
  '(("default" . "X-Face: %irfZ0Z2=ufp].[Z8oJn?QOR6(SRBRW6:IDVMSO25/v\
SE]crv)dR/UBKa,6}+f9+2X$<v(+\n g&&t<oIo8|TUGOzHIQ!LzA@y7g)@)Fv,3Q'KXs\
n:a\"{Y(S#41h+0B\"w}n?QdR}5-@[q5exee!SQ'Fj\n HD@V`O1~7H$b%)F`_9{|rqKa\
F4(\\M2EW?")
    ("\\.hatoyama\\'" . "X-Face: Bhu:2dJ9#&[pX@hMRh=$pF|<M}p@,Fe{2SAS\
)tupW4jk^RavhwxRqDm>O>-,*d\"V+@u\"gB5\n ]}Yxh$n#S1BM<uz\\n|sXtBh\"1TH\
|g@:n,M4A7Cr8,MO$L-KmDmX&~)G+W:6gN0?c:5&o=JAJF6b7%_\n A{A`1-=;*q;RtW>\
o,8|XYsIrL4grl\\|6JV<A.,@%,RI\"v^EIY_[<>6fq3!B`28KP2,M/.Tsh")
    ("\\.aso\\'" . "X-Face: #(b'i|jCr9M1k*o`B1YbD.C*%\\T3~.mUK@q?}o4.\
TC*~*~fPaHg\\]V+Q2$3wu$=:[<k^Y<s\n X{VB1rZN[(X$(Cej@QV?FaoslWKi,fxp\"\
m\\<Cb#4vo!!hDZI@9I8gAMMp6>HZ'C/&9e15i*4e>OV4`\n pdAVvpz`w<$QCu9'~:}|\
h`SEZv\\U]f']V(QbE5'%u$InJltT4~|Ru\\vs~g!;y;1uY#8v<8|eGbb*i=\n a/RM<?\
`*?5`AL1#G(9F50D}`>Y:'\"^)0:;L!2x8j|br~q/E=j.s!FBI-6xr")
    ("\\.fukuda\\'" . "X-Face: R![ems6?kedF&(},`\";7nbUIT6Uyt2A9jSQ'\\\
$=;,n.9<lIS+DFBTdMEJ$nh0[t)XU!.D*p\n kd~cuh0/nvCm;1~B;Ib!g^TC*OHm5(<Z\
%=A2H_,kDt0E*HaI&^C%Wzb/EC_PF1f!jk7VHf=s*mqe91\n `H.J(Bq9(S'71?$O\\+=\
Kp\"yNww;MOGO&N+tm<MbYT}Mlh4<hahJgCV`P/<&9Fm|FRmb>vM+PFYQB}O\n <Se")
    ("\\.abe\\'" . "X-Face: 2lqMN=orK#d]Xl-K5P`=ApJHMB3[faCtca;G(i=qL\
^3qh<kEoLHF\"L\"x/a:|xD>x=IKEqN%\n 3EL93@D{*BW-{GE88b7{d^m-%v9}=-7=^M\
#$?zJm$]Yy07J^}:#V?9t_<{fhavZVZQ1^1=SLQf3X=<\n z|Af_njD},U!m}4V}$]L_7\
a!b>X!RW$['xZs$r=G?o|=M^O)IJoOurt|UKUu[UuQFT/r&vygySYUmf\n <G6B:zwx0@\
$xHbD#Hr3`J,C!5rN5t7oI)ng/'e40?>Jm;kjj")
    ("\\.koizumi\\'\\|\\`m-magazine\\'" . "X-Face: .bsmj'!8A`wI\\o+KF\
!)#0.a0,f1MA~PH/5T0fu$Mg+)_5G~NSk4.0t]&|f@^c3l8-Fuz8'|\n kr;td_Jn7|Gw\
REbDs'H9$Iy#yM#*J2c'L},(m8K:8?$vTPC%D}YJ[bV#7xw|{\"DJ:_?`V1m_4^+;7+\n\
 JOf6v&x6?mU-q=0}mTK5@\"-bFGuD}2Y/(lR/V#'?HRc2Jh2UrR,oIR~NL!})|^%kw")))

(luna-define-method shimbun-index-url ((shimbun shimbun-kantei))
  (let ((group (shimbun-current-group-internal shimbun)))
    (concat (shimbun-url-internal shimbun)
	    (cond ((string-equal group "m-magazine-en")
		   "foreign/m-magazine/backnumber/hatoyama.html")
		  ((string-equal group "m-magazine-cn.hatoyama")
		   "foreign/m-magazine/backnumber_ch/hatoyama_index.html")
		  ((string-equal group "m-magazine-kr.hatoyama")
		   "foreign/m-magazine/backnumber_ko/hatoyama_index.html")
		  ((string-equal group "m-magazine-en.hatoyama")
		   "foreign/m-magazine/backnumber/hatoyama.html")
		  ((string-equal group "m-magazine-ja.hatoyama")
		   "jp/m-magazine/backnumber/hatoyama.html")
		  ((string-equal group "m-magazine-en.aso")
		   "foreign/m-magazine/backnumber/aso.html")
		  ((string-equal group "m-magazine-ja.aso")
		   "jp/m-magazine/backnumber/aso.html")
		  ((string-equal group "m-magazine-en.fukuda")
		   "foreign/m-magazine/backnumber/fukuda.html")
		  ((string-equal group "m-magazine-ja.fukuda")
		   "jp/m-magazine/backnumber/hukuda.html")
		  ((string-equal group "m-magazine-en.abe")
		   "foreign/m-magazine/backnumber/abe.html")
		  ((string-equal group "m-magazine-ja.abe")
		   "jp/m-magazine/backnumber/abe.html")
		  ((string-equal group "m-magazine-en.koizumi")
		   "foreign/m-magazine/backnumber/koizumi.html")
		  ((string-equal group "m-magazine-ja.koizumi")
		   "jp/m-magazine/backnumber/koizumi.html")
		  ;; Backward compatibility.
		  ((string-equal group "m-magazine")
		   "jp/m-magazine/backnumber/koizumi.html")
		  ((string-equal group "m-magazine-cn")
		   "foreign/m-magazine/backnumber_ch/hatoyama_index.html")
		  ((string-equal group "m-magazine-kr")
		   "foreign/m-magazine/backnumber_ko/hatoyama_index.html")
		  ;; Default.
		  (t
		   "jp/m-magazine/backnumber/")))))

(luna-define-method shimbun-from-address ((shimbun shimbun-kantei))
  (let ((group (shimbun-current-group-internal shimbun)))
    (cond ((string-equal group "m-magazine-en")
	   "Naoto Kan")
	  ((string-equal group "m-magazine-cn.hatoyama")
	   "鸠山由纪夫")
	  ((string-equal group "m-magazine-kr.hatoyama")
	   "하토야마 유키오")
	  ((string-equal group "m-magazine-en.hatoyama")
	   "Yukio Hatoyama")
	  ((string-equal group "m-magazine-ja.hatoyama")
	   "鳩山由紀夫")
	  ((string-equal group "m-magazine-en.aso")
	   "Taro Aso")
	  ((string-equal group "m-magazine-ja.aso")
	   "麻生太郎")
	  ((string-equal group "m-magazine-en.fukuda")
	   "Yasuo Fukuda")
	  ((string-equal group "m-magazine-ja.fukuda")
	   "福田康夫")
	  ((string-equal group "m-magazine-en.abe")
	   "Shinzo Abe")
	  ((string-equal group "m-magazine-ja.abe")
	   "安倍晋三")
	  ((string-equal group "m-magazine-en.koizumi")
	   "Junichiro Koizumi")
	  ((string-equal group "m-magazine-ja.koizumi")
	   "小泉純一郎")
	  ((string-equal group "m-magazine") ;; Backward compatibility.
	   "小泉純一郎")
	  (t
	   "菅直人"))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-kantei)
					 &optional range)
  (let* ((group (shimbun-current-group-internal shimbun))
	 (enp (string-match "\\`m-magazine-en" group))
	 (cnp (string-match "\\`m-magazine-cn" group))
	 (krp (string-match "\\`m-magazine-kr" group))
	 (regexp
	  (cond
	   (enp
	    (eval-when-compile
	      (concat "<A[\t\n ]+HREF=\""
		      ;; 1. url
		      "\\(\\(?:[a-z]+/\\)?"
		      ;; 2. year
		      "\\(20[0-9][0-9]\\)"
		      "/"
		      ;; 3. month
		      "\\([01][0-9]\\)"
		      ;; 4. day of month
		      "\\([0-3][0-9]\\)"
		      "\\.html\\)\"[^>]*>[\t\n ]*"
		      ;; 5. subject
		      "\\(\\(?:[^\t\n <]+[\t\n ]+\\)*[^\t\n <]+\\)"
		      "[\t\n ]*</A>[\t\n ]*</TD>[\t\n ]*</TR>")))
	   (cnp
	    (eval-when-compile
	      (concat "<a[\t\n ]+href=\""
		      ;; 1. url
		      "\\(\\(?:/foreign/m-magazine/backnumber_ch/\\)?"
		      ;; 2. year
		      "\\(20[0-9][0-9]\\)"
		      "/"
		      ;; 3. month
		      "\\([01][0-9]\\)"
		      ;; 4. day of month
		      "\\([0-3][0-9]\\)"
		      "\\.html\\)\"[^>]*>[\t\n ]*"
		      ;; 5. subject
		      "\\(\\(?:[^\t\n <]+[\t\n ]+\\)*[^\t\n <]+\\)"
		      "[\t\n ]*</a>[\t\n ]*</td>[\t\n ]*</tr>")))
	   (krp
	    (eval-when-compile
	      (concat "<a[\t\n ]+href=\""
		      ;; 1. url
		      "\\(\\(?:/foreign/m-magazine/backnumber_ko/\\)?"
		      ;; 2. year
		      "\\(20[0-9][0-9]\\)"
		      "/"
		      ;; 3. month
		      "\\([01][0-9]\\)"
		      ;; 4. day of month
		      "\\([0-3][0-9]\\)"
		      "\\.html\\)\"[^>]*>[\t\n ]*"
		      ;; 5. subject
		      "\\(\\(?:[^\t\n <]+[\t\n ]+\\)*[^\t\n <]+\\)"
		      "[\t\n ]*</a>[\t\n ]*</td>[\t\n ]*</tr>")))
	   (t
	    (eval-when-compile
	      (concat "<A[\t\n ]+HREF=\""
		      ;; 1. url
		      "\\(\\(?:/jp/m-magazine/backnumber/\\)?"
		      ;; 2. year
		      "\\(20[0-9][0-9]\\)"
		      "/"
		      ;; 3. month
		      "\\([01][0-9]\\)"
		      ;; 4. day of month
		      "\\([0-3][0-9]\\)"
		      ;; 5. revision e.g., 2005/0602b.html
		      "\\([^.]+\\)?"
		      "\\.html\\)"
		      "\"[^>]*>[\t\n ]*【[^】]+】[\t\n ]*"
		      ;; 6. subject
		      "\\([^<]+\\)")))))
	 (parent (shimbun-index-url shimbun))
	 (from (shimbun-from-address shimbun))
	 year month mday url subject id headers)
    ;; Remove commented areas.
    (while (re-search-forward "<!-+" nil t)
      (when (shimbun-end-of-tag nil t)
	(replace-match "\n")))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (if (or enp cnp krp)
	  (setq year (string-to-number (match-string 2))
		month (string-to-number (match-string 3))
		mday (string-to-number (match-string 4))
		url (match-string 1)
		subject (match-string 5)
		id (format "<%d%02d%02d.%s%%kantei.go.jp>"
			   year month mday group))
	(setq year (string-to-number (match-string 2))
	      month (string-to-number (match-string 3))
	      mday (string-to-number (match-string 4))
	      url (match-string 1)
	      subject (shimbun-replace-in-string (match-string 6)
						 "[\t\n 　]+" " ")
	      id (format "<%d%02d%02d%s.%s%%kantei.go.jp>"
			 year month mday
			 (or (match-string 5) "")
			 group)))
      (push (shimbun-create-header
	     0 subject from
	     (shimbun-make-date-string year month mday)
	     id "" 0 0
	     (if (string-match "\\`http:" url)
		 url
	       (shimbun-expand-url url parent)))
	    headers))
    headers))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-kantei)
						    header)
  (let ((case-fold-search t)
	start end section)
    (if (and (search-forward "<pre>" nil t)
	     (progn
	       (setq start (match-beginning 0))
	       (goto-char (point-max))
	       (search-backward "</pre>" nil t)))
	(progn
	  (delete-region (match-end 0) (point-max))
	  (insert "\n")
	  (delete-region (point-min) start)
	  t)
      (if (re-search-forward "<!-+[\t\n ]*content[\t\n ]*-+>[\t\n ]*\
\\(?:<[^>]+>[\t\n ]*\\)*"
			     nil t)
	  (progn
	    (setq start (match-end 0))
	    (re-search-forward "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\
\\(?:<a[\t\n ]+href=\"#[^>]+>[\t\n ]*\
go[\t\n ]+to[\t\n ]+top[\t\n ]+of[\t\n ]+the[\t\n ]+page[\t\n ]*</a>\
\\|<a[\t\n ]*href=\"[^>]+>[\t\n ]*subscription[\t\n ]*</a>\
\\|<!-+[\t\n ]*/content[\t\n ]*-+>\\)"
			       nil t)
	    (delete-region (match-beginning 0) (point-max))
	    (insert "\n")
	    (delete-region (point-min) start)))
      (goto-char (point-min))
      (if (and (re-search-forward "<!--\\(\\cj+\\)-->[\t\n ]*" nil t)
	       (progn
		 (setq section (regexp-quote (match-string 1))
		       start (match-end 0))
		 (re-search-forward (concat "\[\t\n ]*<!--/" section
					    "\\(?:から\\)?-->")
				    nil t)))
	  (progn
	    (setq end (match-beginning 0))
	    (while (when (re-search-forward "<!--\\(\\cj+\\)-->[\t\n ]*" nil t)
		     (setq section (regexp-quote (match-string 1)))
		     (delete-region end (match-end 0))
		     (insert "\n&#012;\n")
		     (and (re-search-forward (concat "\[\t\n ]*<!--/" section
						     "\\(?:から\\)?-->")
					     nil t)
			  (setq end (match-beginning 0)))))
	    (if (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*align=\"center\"" nil t)
		     (shimbun-end-of-tag "div" t))
		(progn
		  (delete-region (match-end 1) (point-max))
		  (insert "\n")
		  (goto-char end)
		  (delete-region end (match-beginning 3))
		  (insert "\n<div align=\"left\">\n--&nbsp;<br>\n"))
	      (delete-region end (point-max))
	      (insert "\n"))
	    (delete-region (point-min) start))
	;; Remove style sheet.
	(goto-char (point-min))
	(when (and (re-search-forward "<style[\t\n ]+" nil t)
		   (shimbun-end-of-tag "style" t))
	  (replace-match "\n"))
	;; Remove navigation button.
	(goto-char (point-min))
	(when (and (re-search-forward "<\\(td\\|span\\)\
\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"breadcrumbs\""
				      nil t)
		   (shimbun-end-of-tag (match-string 1) t))
	  (replace-match "\n")))
      ;; Remove useless tags.
      (goto-char (point-min))
      (while (re-search-forward "[\t\n ]*</tr>[\t\n ]*" nil t)
	(replace-match "<br>\n"))
      (goto-char (point-min))
      (while (re-search-forward "\
\[\t\n ]*</?\\(?:hr\\|span\\|table\\|td\\|tr\\)\\(?:[\t\n ]+[^>]+\\)?>[\t\n ]*"
				nil t)
	(replace-match "\n"))
      (goto-char (point-min))
      (while (re-search-forward "[\t\n ]*<p[\t\n ]+[^>]+>[\t\n ]*" nil t)
	(replace-match "\n<p>"))
      (goto-char (point-min))
      (while (re-search-forward "[\t\n ]*<![^>]+>[\t\n ]*" nil t)
	(replace-match "\n"))
      (goto-char (point-min))
      (while (re-search-forward "^[\t 　]+\n" nil t)
	(delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward "\\([\t\n ]*<br\\(?:[\t\n ]+[^>]*\\)?>\\)\
\\(?:[\t\n ]*<br\\(?:[\t\n ]+[^>]*\\)?>\\)+[\t\n ]*\\(<br[\t\n >]\\)" nil t)
	(replace-match "\\1\\2"))
      (goto-char (point-min))
      (skip-chars-forward "\t\n ")
      (delete-region (point-min) (point))
      (while (re-search-forward "[\t\n ]+\n" nil t)
	(replace-match "\n"))
      ;; Insert newlines around images.
      (goto-char (point-min))
      (while (re-search-forward "[\t\n ]*\\(\\(?:<[^/][>]+>[\t\n ]*\\)*\
<img[\t\n ]+[^>]+>\\(?:[\t\n ]*<[^/][>]+>\\)*\\)[\t\n ]*" nil t)
	(replace-match "<br>\n\\1<br>\n"))
      ;; Shrink boundary lines.
      (let ((limit (w3m-static-if (featurep 'xemacs)
		       (when (device-on-window-system-p)
			 (font-width (face-font 'default)))
		     (when window-system
		       (frame-char-width)))))
	(when limit
	  (setq limit (* limit (1- (window-width))))
	  (goto-char (point-min))
	  (while (re-search-forward
		  "<img\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+height=\"1\""
		  nil t)
	    (when (shimbun-end-of-tag)
	      (goto-char (match-beginning 0))
	      (if (re-search-forward "width=\"\\([0-9]+\\)\"" (match-end 0) t)
		  (when (> (string-to-number (match-string 1)) limit)
		    (replace-match (concat "width=\"" (number-to-string limit)
					   "\"")))
		(goto-char (match-end 0)))))))
      ;; Zenkaku ASCII -> Hankaku
      (unless (memq (shimbun-japanese-hankaku shimbun) '(header subject nil))
	(shimbun-japanese-hankaku-buffer t)))))

(luna-define-method shimbun-make-contents :around ((shimbun shimbun-kantei)
						   header)
  (if (string-match "\\`m-magazine-\\(?:cn\\|en\\|ja\\|kr\\)\\'"
		    (shimbun-current-group-internal shimbun))
      ;; Choose a face according to the author.
      (let ((shimbun-x-face-database-function
	     (or shimbun-x-face-database-function
		 (let ((from (shimbun-header-from header t)))
		   `(lambda (ignore)
		      ,(cdr (nth
			     (cond ((member from '("Yukio Hatoyama"
						   "鳩山由紀夫"
						   "鸠山由纪夫"
						   "하토야마 유키오"))
				    1)
				   ((member from '("Taro Aso"
						   "麻生太郎"))
				    2)
				   ((member from '("Yasuo Fukuda"
						   "福田康夫"))
				    3)
				   ((member from '("Shinzo Abe"
						   "安倍晋三"))
				    4)
				   ((member from '("Junichiro Koizumi"
						   "小泉純一郎"))
				    5)
				   (t
				    0))
			     shimbun-kantei-x-face-alist)))))))
	(luna-call-next-method))
    (luna-call-next-method)))

(provide 'sb-kantei)

;;; sb-kantei.el ends here
