;;; w3m-weather.el --- Look weather forecast -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2005
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

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

;; w3m-weather.el is the add-on program of emacs-w3m to look weather
;; foracast.  For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; How to install:

;; Please put this file to appropriate directory, and if you want
;; byte-compile it.  And add following lisp expressions to your
;; ~/.emacs.
;;
;;     (autoload 'w3m-weather "w3m-weather" "Display weather report." t)


;;; Code:

(eval-when-compile (require 'cl))
(require 'w3m)

(defconst w3m-weather-completion-table
  (eval-when-compile
    (let* ((format "http://weather.yahoo.co.jp/weather/jp/%s.html")
	   (alist
	    '(;; URLの一部, 漢字表記, ローマ字表記, 別名
	      ;; (ローマ字表記では長音を省略しないこと)
	      ("1a/1100" "道北・宗谷" "douhokusouya" "souya")
	      ("1a/1200" "道北・上川" "douhokukamikawa" "kamikawa")
	      ("1a/1300" "道北・留萌" "douhokurumoi" "rumoi")
	      ("1c/1710" "道東・網走" "doutouabashiri" "abashiri")
	      ("1c/1720" "道東・北見" "doutoukitami" "kitami")
	      ("1c/1730" "道東・紋別" "doutoumonbetsu" "monbetsu")
	      ("1c/1800" "道東・根室" "doutounemuro" "nemuro")
	      ("1c/1900" "道東・釧路" "doutoukushiro" "kushiro")
	      ("1c/2000" "道東・十勝" "doutoutokachi" "tokachi")
	      ("1b/1400" "道央・石狩" "dououishikari" "ishikari")
	      ("1b/1500" "道央・空知" "douousorachi" "sorachi")
	      ("1b/1600" "道央・後志" "dououshiribeshi" "shiribeshi")
	      ("1d/2400" "道南・桧山" "dounanhiyama" "hiyama")
	      ("1d/2100" "道南・胆振" "dounaniburi" "iburi")
	      ("1d/2200" "道南・日高" "dounanhidaka" "hidaka")
	      ("1d/2300" "道南・渡島" "dounanoshima" "oshima")
	      ("1d/2400" "道南・檜山" "dounanhiyama" "hiyama")
	      ("2/3110" "青森県・津軽" "aomorikentsugaru" "tsugaru")
	      ("2/3120" "青森県・下北" "aomorikenshimokita" "shimokita")
	      ("2/3130" "青森県・三八上北"
	       "aomorikensanpachikamikita" "sanpachikamikita")
	      ("3/3310" "岩手県・内陸部" "iwatekennairikubu")
	      ("3/3320" "岩手県・沿岸北部" "iwatekenenganhokubu")
	      ("3/3330" "岩手県・沿岸南部" "iwatekenengannanbu")
	      ("5/3210" "秋田県・沿岸部" "akitakenenganbu")
	      ("5/3220" "秋田県・内陸部" "akitakennairikubu")
	      ("4/3410" "宮城県・東部" "miyagikentoubu")
	      ("4/3420" "宮城県・西部" "miyagikenseibu")
	      ("6/3510" "山形県・村山" "yamagatakenmurayama" "murayama")
	      ("6/3520" "山形県・置賜" "yamagatakenokitama" "okitama")
	      ("6/3530" "山形県・庄内" "yamagatakenshonai" "shounai")
	      ("6/3540" "山形県・最上" "yamagatakenmogami" "mogami")
	      ("7/3610" "福島県・中通り" "hukushimakennakadoori" "nakadoori")
	      ("7/3620" "福島県・浜通り" "hukushimakenhamadoori" "hamadoori")
	      ("7/3630" "福島県・会津" "hukushimakenaidu" "aidu")
	      ("8/4010" "茨城県・北部" "ibaragikenhokubu")
	      ("8/4020" "茨城県・南部" "ibaragikennanbu")
	      ("9/4110" "栃木県・南部" "tochigikennanbu")
	      ("9/4120" "栃木県・北部" "tochigikenhokubu")
	      ("10/4210" "群馬県・南部" "gunmakennanbu")
	      ("10/4220" "群馬県・北部" "gunmakenhokubu")
	      ("11/4310" "埼玉県・南部" "saitamakennanbu")
	      ("11/4320" "埼玉県・北部" "saitamakenhokubu")
	      ("11/4330" "埼玉県・秩父" "saitamakenchichibu")
	      ("12/4510" "千葉県・北西部" "chibakenhokuseibu")
	      ("12/4520" "千葉県・北東部" "chibakenhokutoubu")
	      ("12/4530" "千葉県・南部" "chibakennanbu")
	      ("13/4410" "東京都・東京" "toukyoutotoukyou" "toukyou")
	      ("13/4420" "東京都・伊豆諸島北部"
	       "toukyoutoizushotouhokubu" "izushotouhokubu")
	      ("13/100" "東京都・伊豆諸島南部"
	       "toukyoutoizushotounanbu" "izushotounanbu")
	      ("13/9600" "東京都・小笠原諸島"
	       "toukyoutoogasawarashotou" "ogasawarashotou")
	      ("14/4610" "神奈川県・東部" "kanagawakentoubu")
	      ("14/4620" "神奈川県・西部" "kanagawakenseibu")
	      ("15/5410" "新潟県・下越" "niigatakenkaetsu" "kaetsu")
	      ("15/5420" "新潟県・中越" "niigatakenchuuetsu" "chuuetsu")
	      ("15/5430" "新潟県・上越" "niigatakenjouetsu" "jouetsu")
	      ("15/5440" "新潟県・佐渡" "niigatakensado" "sado")
	      ("16/5510" "富山県・東部" "toyamakentoubu")
	      ("16/5520" "富山県・西部" "toyamakenseibu")
	      ("17/5610" "石川県・加賀" "ishikawakenkaga" "kaga")
	      ("17/5620" "石川県・能登" "ishikawakennoto" "noto")
	      ("18/5710" "福井県・嶺北" "hukuikenreihoku" "reihoku")
	      ("18/5720" "福井県・嶺南" "hukuikenreinan" "reinan")
	      ("19/4910" "山梨県・中西部" "yamanashikenchuuseibu")
	      ("19/4920" "山梨県・富士五湖" "yamanashikenhujigoko" "hujigoko")
	      ("20/4810" "長野県・北部" "naganokenhokubu")
	      ("20/4820" "長野県・中部" "naganokenchuubu")
	      ("20/4830" "長野県・南部" "naganokennanbu")
	      ("21/5210" "岐阜県・美濃" "gihukenmino" "mino")
	      ("21/5220" "岐阜県・飛騨" "gihukenhida" "hida")
	      ("22/5010" "静岡県・中部" "shizuokakenchuubu")
	      ("22/5020" "静岡県・伊豆" "shizuokakenizu" "izu")
	      ("22/5030" "静岡県・東部" "shizuokakentoubu")
	      ("22/5040" "静岡県・西部" "shizuokakenseibu")
	      ("23/5110" "愛知県・西部" "aichikenseibu")
	      ("23/5120" "愛知県・東部" "aichikentoubu")
	      ("24/5310" "三重県・北中部" "miekenhokuchuubu")
	      ("24/5320" "三重県・南部" "miekennanbu")
	      ("25/6010" "滋賀県・南部" "shigakennanbu")
	      ("25/6020" "滋賀県・北部" "shigakenhokubu")
	      ("26/400" "京都府・北部" "kyoutohuhokubu")
	      ("26/6100" "京都府・南部" "kyoutohunanbu")
	      ("27/6200" "大阪府" "oosakahu" "oosaka")
	      ("28/500" "兵庫県・北部" "hyougokenhokubu")
	      ("28/6300" "兵庫県・南部" "hyougokennanbu")
	      ("29/6410" "奈良県・北部" "narakenhokubu")
	      ("29/6420" "奈良県・南部" "narakennanbu")
	      ("30/6510" "和歌山県・北部" "wakayamakenhokubu")
	      ("30/6520" "和歌山県・南部" "wakayamakennanbu")
	      ("31/6910" "鳥取県・東部" "tottorikentoubu")
	      ("31/6920" "鳥取県・西部" "tottorikenseibu")
	      ("32/600" "島根県・隠岐" "shimanekenoki" "oki")
	      ("32/6810" "島根県・東部" "shimanekentoubu")
	      ("32/6820" "島根県・西部" "shimanekenseibu")
	      ("33/6610" "岡山県・南部" "okayamakennanbu")
	      ("33/6620" "岡山県・北部" "okayamakenhokubu")
	      ("34/6710" "広島県・南部" "hiroshimakennanbu")
	      ("34/6720" "広島県・北部" "hiroshimakenhokubu")
	      ("35/8110" "山口県・西部" "yamaguchikenseibu")
	      ("35/8120" "山口県・中部" "yamaguchikenchuubu")
	      ("35/8140" "山口県・北部" "yamaguchikenhokubu")
	      ("35/8130" "山口県・東部" "yamaguchikentoubu")
	      ("36/7110" "徳島県・北部" "tokushimakenhokubu")
	      ("36/7120" "徳島県・南部" "tokushimakennanbu")
	      ("37/7200" "香川県" "kagawaken" "kagawa")
	      ("38/7320" "愛媛県・東予" "ehimekentouyo" "touyo")
	      ("38/7330" "愛媛県・南予" "ehimekennanyo" "nanyo")
	      ("38/7310" "愛媛県・中予" "ehimekenchuuyo" "chuuyo")
	      ("39/7410" "高知県・中部" "kouchikenchuubu")
	      ("39/7420" "高知県・東部" "kouchikentoubu")
	      ("39/7430" "高知県・西部" "kouchikenseibu")
	      ("40/8210" "福岡県・福岡" "hukuokakenhukuoka" "hukuoka")
	      ("40/8220" "福岡県・北九州" "hukuokakenkitakyushu" "kitakyuushu")
	      ("40/8230" "福岡県・筑豊" "hukuokakenchikuhou" "chikuhou")
	      ("40/8240" "福岡県・筑後" "hukuokakenchikugo" "chikugo")
	      ("41/8510" "佐賀県・南部" "sagakennanbu")
	      ("41/8520" "佐賀県・北部" "sagakenhokubu")
	      ("42/700" "長崎県・壱岐対馬"
	       "nagasakikenikitsushima" "iki" "tsushima" "ikitsushima")
	      ("42/800" "長崎県・五島" "nagasakikengotou" "gotou")
	      ("42/8410" "長崎県・南部" "nagasakikennanbu")
	      ("42/8420" "長崎県・北部" "nagasakikenhokubu")
	      ("43/8610" "熊本県・熊本" "kumamotokenkumamoto" "kumamoto")
	      ("43/8620" "熊本県・阿蘇" "kumamotokenaso" "aso")
	      ("43/8630" "熊本県・天草芦北"
	       "kumamotokenamakusaashikita" "amakusa" "ashikita" "amakusaashikita")
	      ("43/8640" "熊本県・球磨" "kumamotokenkuma" "kuma")
	      ("44/8310" "大分県・中部" "ooitakenchuubu")
	      ("44/8320" "大分県・北部" "ooitakenhokubu")
	      ("44/8330" "大分県・西部" "ooitakenseibu")
	      ("44/8340" "大分県・南部" "ooitakennanbu")
	      ("45/8710" "宮崎県・南部平野部" "miyazakikennanbuheiyabu")
	      ("45/8720" "宮崎県・北部平野部" "miyazakikenhokubuheiyabu")
	      ("45/8730" "宮崎県・南部山沿い" "miyazakikennanbuyamazoi")
	      ("45/8740" "宮崎県・北部山沿い" "miyazakikenhokubuyamazoi")
	      ("46/8810" "鹿児島県・薩摩" "kagoshimakensatsuma" "satsuma")
	      ("46/8820" "鹿児島県・大隅" "kagoshimakenoosumi" "oosumi")
	      ("46/900" "鹿児島県・種子島・屋久島"
	       "kagoshimakentanegashimayakushima" "tanegashima" "yakushima" "tanegashimayakushima")
	      ("46/1000" "鹿児島県・奄美" "kagoshimakenamami" "amami")
	      ("47/9110" "沖縄県・本島中南部"
	       "okinawakenhontouchuunanbu" "hontouchuunanbu")
	      ("47/9120" "沖縄県・本島北部"
	       "okinawakenhontouhokubu" "hontouhokubu")
	      ("47/9130" "沖縄県・久米島" "okinawakenkumejima" "kumejima")
	      ("47/9200" "沖縄県・大東島" "okinawakendaitoujima" "daitoujima")
	      ("47/9300" "沖縄県・宮古島" "okinawakenmiyakojima" "miyakojima")
	      ("47/9400" "沖縄県・石垣島"
	       "okinawakenishigakijima" "ishigakijima")
	      ("47/9500" "沖縄県・与那国島"
	       "okinawakenyonagunijima" "yonagunijima")))
	   (table)
	   ;; ヘボン式と訓令式の対応表
	   (hepburn-table
	    (let (table)
	      (dolist (x '(("si" "shi")
			   ("zi" "ji")
			   ("zu" "du")
			   ("ti" "chi")
			   ("tu" "tsu")
			   ("hu" "fu")))
		(push x table)
		(push (reverse x)table))
	      (dolist (x '(("sy" . "sh")
			   ("zy" . "j")
			   ("ty" . "ch")))
		(dolist (y '("a" "u" "o"))
		  (push (list (concat (car x) y) (concat (cdr x) y)) table)
		  (push (list (concat (cdr x) y) (concat (car x) y)) table)))
	      table))
	   ;; 対応表に乗っている文字列を探す正規表現
	   (hepburn-regexp
	    (format "\\(?:\\`\\|[aiueo]\\)\\(n\\([^aiueoy]\\)\\|%s\\)"
		    (regexp-opt (mapcar (function car) hepburn-table))))
	   ;; 長音の有無による派生形の表
	   (prolonged-table
	    (let (table)
	      (dolist (x '("k" "ky"
			   "s" "sy" "sh"
			   "t" "ty" "ch"
			   "n" "ny"
			   "h" "hy"
			   "m" "my"
			   "y"
			   "r" "ry"
			   "w"
			   "g" "gy"
			   "z" "zy" "j"
			   "d" "dy"
			   "b" "by"
			   "p" "py"))
		(let ((long-vowels '("ou" "oo" "o-")))
		  (dolist (y long-vowels)
		    (push (cons (concat x y)
				(append
				 (mapcar
				  (lambda (z) (concat x z))
				  (delete y (copy-sequence long-vowels)))
				 (list (concat x "o"))))
			  table)))
		(push (list (concat x "uu") (concat x "u"))
		      table))
	      table))
	   ;; 派生形の表に乗っている文字列を探す正規表現
	   (prolonged-regexp (format "\\(?:\\`\\|[aiueo]\\)\\(%s\\)"
				     (regexp-opt (mapcar (function car)
							 prolonged-table)))))
      (labels ((hepburn-candidates
		(str)
		"ヘボン式と訓令式の差によって生じる派生形を得る"
		(if (string-match hepburn-regexp str)
		    (let ((prefix (substring str 0 (match-beginning 1)))
			  (candidates (if (match-beginning 2)
					  '("n" "nn")
					(assoc (match-string 1 str)
					       hepburn-table)))
			  (suffixes
			   (hepburn-candidates
			    (substring str (or (match-beginning 2)
					       (match-end 0)))))
			  (buf))
		      (dolist (x candidates)
			(dolist (y suffixes)
			  (push (concat prefix x y) buf)))
		      buf)
		  (list str)))
	       (prolonged-candidates
		(str)
		"長音の有無によって生じる派生形を得る"
		(let (buf)
		  (if (string-match prolonged-regexp str)
		      (let ((prefix (substring str 0 (match-beginning 1)))
			    (candidates (assoc (match-string 1 str)
					       prolonged-table))
			    (suffixes (prolonged-candidates
				       (substring str (match-end 0)))))
			(dolist (x candidates)
			  (dolist (y suffixes)
			    (push (concat prefix x y) buf))))
		    (setq buf (list str)))
		  (dolist (x buf)
		    (when (string-match "\\(\\`\\|[aiue]\\)oo" x)
		      (let ((prefix (substring x 0 (match-end 1)))
			    (suffix (substring x (match-end 0))))
			(dolist (y '("o" "oh" "o-"))
			  (push (concat prefix y suffix) buf)))))
		  buf))
	       (romaji-candidates
		(str)
		"全ての派生形を得る"
		(let (buf)
		  (dolist (x (hepburn-candidates str))
		    (dolist (y (prolonged-candidates x))
		      (push y buf)))
		  buf)))
	(dolist (area alist)
	  (let ((url (format format (car area)))
		(kanji (cadr area)))
	    (push (list kanji (nth 2 area) url) table)
	    (dolist (romaji (cddr area))
	      (dolist (x (romaji-candidates romaji))
		(push (list x kanji) table)))))
	(nreverse table))))
  "Completion table of areas and urls.")

(defcustom w3m-weather-default-area
  "京都府・南部"
  "Default region to check weather."
  :group 'w3m
  :type (cons 'radio
	      (delq nil
		    (mapcar (lambda (area)
			      (when (nth 2 area)
				(list 'const (car area))))
			    w3m-weather-completion-table))))

(defcustom w3m-weather-filter-functions
  '(w3m-weather-extract-contents
    w3m-weather-adjust-contents
    w3m-weather-expand-anchors
    w3m-weather-insert-title)
  "Filter functions to remove useless tags."
  :group 'w3m
  :type 'hook)

(defvar w3m-weather-input-history nil)

(defun w3m-weather-input-area ()
  (let* ((str
	  (completing-read (format "Input area (default %s): "
				   w3m-weather-default-area)
			   'w3m-weather-area-completion nil t nil
			   'w3m-weather-input-history))
	 (area
	  (cond
	   ((string= "" str) w3m-weather-default-area)
	   ((string-match "[^-a-zA-Z]" str) str)
	   (t (cadr (assoc str w3m-weather-completion-table))))))
    (setq w3m-weather-input-history
	  (cons area
		(delete area
			(delete str w3m-weather-input-history))))
    area))

(defun w3m-weather-area-completion (partial predicate flag)
  (if (eq flag 'lambda)
      (and (assoc partial w3m-weather-completion-table)
	   (or (not predicate)
	       (funcall predicate partial))
	   t)
    (let ((kanji "")
	  (romaji "")
	  (romaji-partial partial))
      (when (string-match "\\`\\(?:[^-a-zA-Z]+\\)" partial)
	(let ((suffix (substring partial (match-end 0))))
	  (setq kanji (substring partial 0 (match-end 0))
		romaji (try-completion
			""
			(mapcar
			 (lambda (x)
			   (list (cadr (assoc x w3m-weather-completion-table))))
			 (all-completions kanji w3m-weather-completion-table)))
		romaji-partial (concat romaji suffix))))
      (let ((collection)
	    (regexp
	     (and (string-match "・\\'" kanji)
		  (string-match "[aiueo]n\\'" romaji)
		  (concat "\\`" romaji "n[^aiueoy]"))))
	(dolist (x (all-completions romaji-partial w3m-weather-completion-table))
	  (unless (and regexp (string-match regexp x))
	    (setq x (assoc x w3m-weather-completion-table))
	    (unless (assoc (cadr x) collection)
	      (push (cons (cadr x) (car x)) collection))))
	(cond
	 ((not flag)
	  (let ((s (try-completion kanji collection predicate)))
	    (if (and (stringp s) (string< s partial))
		(when (setq s (try-completion romaji-partial
					      (mapcar (lambda (x) (list (cdr x)))
						      collection)
					      predicate))
		  (concat kanji (substring s (if romaji (length romaji) 0))))
	      s)))
	 ((eq flag t)
	  (all-completions kanji collection predicate)))))))

;;;###autoload
(defun w3m-weather (area)
  "Display weather report."
  (interactive
   (list (if current-prefix-arg
	     (w3m-weather-input-area)
	   w3m-weather-default-area)))
  (w3m-goto-url (format "about://weather/%s" area)))

;;;###autoload
(defun w3m-about-weather (url no-decode no-cache post-data referer handler)
  (if (string-match "\\`about://weather/" url)
      (lexical-let* ((url url)
		     (no-cache no-cache)
		     (area (substring url (match-end 0)))
		     (furl (nth 2 (assoc area w3m-weather-completion-table))))
	(w3m-process-do
	    (type (w3m-retrieve furl nil no-cache nil nil handler))
	  (when type
	    (w3m-decode-buffer furl)
	    (w3m-weather-run-filter-functions w3m-weather-filter-functions
					      area furl no-cache handler))))
    (w3m-message "Unknown URL: %s" url)
    nil))

(defun w3m-weather-run-filter-functions (functions area url no-cache handler)
  (if functions
      (lexical-let ((functions functions)
		    (area area)
		    (url url)
		    (no-cache no-cache))
	(w3m-process-do
	    (nil (funcall (pop functions) area url no-cache handler))
	  (w3m-weather-run-filter-functions functions area url
					    no-cache handler)))
    "text/html"))

(defun w3m-weather-extract-contents (&rest args)
  "Remove both header and footer in the weather forecast pages."
  (goto-char (point-min))
  (when (search-forward "<!---MAIN_CONTENTS_table--->" nil t)
    (delete-region (point-min) (match-beginning 0)))
  (goto-char (point-max))
  (when (search-backward "<!---Local_Link--->" nil t)
    (delete-region (match-beginning 0) (point-max))))

(defun w3m-weather-adjust-contents (&rest args)
  ;; Remove spacers.
  (goto-char (point-min))
  (while (search-forward "<tr><td>\
<img src=\"http://img.yahoo.co.jp/images/clear.gif\" width=1>\
</td></tr>" nil t)
    (delete-region (match-beginning 0) (match-end 0)))
  ;; Remove execessive tables.
  (goto-char (point-min))
  (while (re-search-forward "<table[^>]*>[ \t\r\f\n]*</table>" nil t)
    (delete-region (match-beginning 0) (match-end 0)))
  (goto-char (point-min))
  ;; Remove too narrow width parameters.
  (while (re-search-forward "<td[^>]*\\(width=1%\\)" nil t)
    (delete-region (match-beginning 1) (match-end 1)))
  ;; Display border lines.
  (goto-char (point-min))
  (while (re-search-forward "\
<table border=\\(0\\) cellpadding=[1-9][0-9]* cellspacing=[1-9][0-9]*" nil t)
    (goto-char (match-beginning 1))
    (delete-char 1)
    (insert "1"))
  (goto-char (point-min))
  (while (re-search-forward
	  "<td align=center width=25%>[ \t\r\f\n]*<table border=1" nil t)
    (delete-char -1)
    (insert "0")))

(defun w3m-weather-insert-title (area url &rest args)
  "Insert title."
  (goto-char (point-min))
  (insert "<head><title>Weather forecast of "
	  area
	  "</title></head>\n"
	  "<body><p align=left><a href=\""
	  url
	  "\">[Yahoo!]</a></p>\n")
  (goto-char (point-max))
  (insert "</body>"))

(defun w3m-weather-expand-anchors (area url &rest args)
  ;; FIXME: 天気予報ページに含まれている相対リンクを絶対リンクに書き換
  ;; えるための関数．これらの相対リンクを安全に取り扱うためには，base
  ;; URL を返せるように，about:// の構造を書き直す必要があると考えられ
  ;; るが，とりあえず後回し．
  (goto-char (point-min))
  (while (re-search-forward
	  (eval-when-compile
	    (concat "<a[ \t\r\f\n]+href=" w3m-html-string-regexp))
	  nil t)
    (replace-match (format
		    "<a href=\"%s\""
		    (w3m-expand-url (w3m-remove-redundant-spaces
				     (or (match-string-no-properties 2)
					 (match-string-no-properties 3)
					 (match-string-no-properties 1)))
				    url)))))

(provide 'w3m-weather)

;;; w3m-weather.el ends here.
