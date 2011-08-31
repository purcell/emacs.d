;;; sb-nytimes.el --- shimbun backend for The New York Times

;; Copyright (C) 2007, 2008, 2009 Katsumi Yamaoka

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
(require 'sb-rss)
(require 'sb-multi)

(luna-define-class shimbun-nytimes (shimbun-newspaper
				    shimbun-multi shimbun-rss) ())

(defvar shimbun-nytimes-url "http://www.nytimes.com/"
  "Name of the parent url.")

(defvar shimbun-nytimes-server-name "The New York Times")

(defvar shimbun-nytimes-group-table
  '(("homepage" "NYTIMES.COM HOMEPAGE"
     "http://www.nytimes.com/services/xml/rss/nyt/HomePage.xml")

    ("news.business" "BUSINESS"
     "http://www.nytimes.com/services/xml/rss/nyt/Business.xml")
    ("news.business.media&advertising" "Media & Advertising"
     "http://www.nytimes.com/services/xml/rss/nyt/MediaandAdvertising.xml")
    ("news.business.worldbusiness" "World Business"
     "http://www.nytimes.com/services/xml/rss/nyt/WorldBusiness.xml")
    ("news.business.smallbusiness" "Small Business"
     "http://www.nytimes.com/services/xml/rss/nyt/SmallBusiness.xml")
    ("news.business.yourmoney" "Your Money"
     "http://www.nytimes.com/services/xml/rss/nyt/YourMoney.xml")
    ("news.business.dealbook" "DealBook"
     "http://dealbook.blogs.nytimes.com/rss2.xml")

    ("news.education" "EDUCATION"
     "http://www.nytimes.com/services/xml/rss/nyt/Education.xml")

    ("news.health" "HEALTH"
     "http://www.nytimes.com/services/xml/rss/nyt/Health.xml")
    ("news.health.policy" "Health Policy"
     "http://www.nytimes.com/services/xml/rss/nyt/HealthCarePolicy.xml")
    ("news.health.psychology" "Mental Health & Behavior"
     "http://www.nytimes.com/services/xml/rss/nyt/Psychology.xml")

    ("news.world" "WORLD"
     "http://www.nytimes.com/services/xml/rss/nyt/International.xml")
    ("news.world.africa" "Africa News"
     "http://www.nytimes.com/services/xml/rss/nyt/Africa.xml")
    ("news.world.americas" "Americas News"
     "http://www.nytimes.com/services/xml/rss/nyt/Americas.xml")
    ("news.world.asia" "Asia News"
     "http://www.nytimes.com/services/xml/rss/nyt/AsiaPacific.xml")
    ("news.world.europe" "Europe News"
     "http://www.nytimes.com/services/xml/rss/nyt/Europe.xml")
    ("news.world.middleeast" "Middle East News"
     "http://www.nytimes.com/services/xml/rss/nyt/MiddleEast.xml")

    ("news.us" "U.S."
     "http://www.nytimes.com/services/xml/rss/nyt/National.xml")

    ("news.newyork" "NEW YORK / REGION"
     "http://www.nytimes.com/services/xml/rss/nyt/NYRegion.xml")
    ("news.newyork.thecity" "The City"
     "http://www.nytimes.com/services/xml/rss/nyt/TheCity.xml")
    ("news.newyork.metro" "Metro Campaigns"
     "http://www.nytimes.com/services/xml/rss/nyt/MetroCampaigns.xml")

    ("news.obituaries" "OBITUARIES"
     "http://www.nytimes.com/services/xml/rss/nyt/Obituaries.xml")

    ("news.science" "SCIENCE"
     "http://www.nytimes.com/services/xml/rss/nyt/Science.xml")
    ("news.science.earth" "Earth"
     "http://www.nytimes.com/services/xml/rss/nyt/Environment.xml")
    ("news.science.nutrition" "Nutrition"
     "http://www.nytimes.com/services/xml/rss/nyt/Nutrition.xml")
    ("news.science.space" "Space"
     "http://www.nytimes.com/services/xml/rss/nyt/Space.xml")

    ("news.sports" "SPORTS"
     "http://www.nytimes.com/services/xml/rss/nyt/Sports.xml")
    ("news.sports.basketball.college" "College Basketball"
     "http://www.nytimes.com/services/xml/rss/nyt/CollegeBasketball.xml")
    ("news.sports.football.college" "College Football"
     "http://www.nytimes.com/services/xml/rss/nyt/CollegeFootball.xml")
    ("news.sports.golf" "Golf"
     "http://www.nytimes.com/services/xml/rss/nyt/Golf.xml")
    ("news.sports.hockey" "Hockey"
     "http://www.nytimes.com/services/xml/rss/nyt/Hockey.xml")
    ("news.sports.other" "Other Sports"
     "http://www.nytimes.com/services/xml/rss/nyt/OtherSports.xml")
    ("news.sports.baseball.pro" "Pro Baseball"
     "http://www.nytimes.com/services/xml/rss/nyt/Baseball.xml")
    ("news.sports.basketball.pro" "Pro Basketball"
     "http://www.nytimes.com/services/xml/rss/nyt/ProBasketball.xml")
    ("news.sports.football.pro" "Pro Football"
     "http://www.nytimes.com/services/xml/rss/nyt/ProFootball.xml")
    ("news.sports.soccer" "Soccer"
     "http://www.nytimes.com/services/xml/rss/nyt/Soccer.xml")

    ("news.technology" "TECHNOLOGY"
     "http://www.nytimes.com/services/xml/rss/nyt/Technology.xml")
    ("news.technology.bits" "Bits"
     "http://bits.blogs.nytimes.com/rss2.xml")
    ("news.technology.circuits" "Circuits"
     "http://www.nytimes.com/services/xml/rss/nyt/Circuits.xml")
    ("news.technology.pogue" "Pogue's Posts"
     "http://pogue.blogs.nytimes.com/?feed=rss2")

    ("news.washington" "WASHINGTON"
     "http://www.nytimes.com/services/xml/rss/nyt/Washington.xml")

    ("features.arts" "ARTS"
     "http://www.nytimes.com/services/xml/rss/nyt/Arts.xml")
    ("features.arts.design" "Design"
     "http://www.nytimes.com/services/xml/rss/nyt/ArtandDesign.xml")
    ("features.arts.music" "Music"
     "http://www.nytimes.com/services/xml/rss/nyt/Music.xml")
    ("features.arts.television" "Television News"
     "http://www.nytimes.com/services/xml/rss/nyt/Television.xml")

    ("features.automobiles" "AUTOMOBILES"
     "http://www.nytimes.com/services/xml/rss/nyt/Automobiles.xml")

    ("features.books" "BOOKS"
     "http://www.nytimes.com/services/xml/rss/nyt/Books.xml")
    ("features.books.review" "Book Review"
     "http://www.nytimes.com/services/xml/rss/nyt/SundayBookReview.xml")

    ("features.dining&wine" "DINING & WINE"
     "http://www.nytimes.com/services/xml/rss/nyt/DiningandWine.xml")

    ("features.fashion" "FASHION & STYLE"
     "http://www.nytimes.com/services/xml/rss/nyt/FashionandStyle.xml")
    ("features.fashion.thursdaystyles" "Thursday Styles"
     "http://www.nytimes.com/services/xml/rss/nyt/ThursdayStyles.xml")
    ("features.fashion.weddings" "Weddings"
     "http://www.nytimes.com/services/xml/rss/nyt/Weddings.xml")

    ("features.home&garden" "HOME & GARDEN"
     "http://www.nytimes.com/services/xml/rss/nyt/HomeandGarden.xml")

    ("features.jobs" "JOBS"
     "http://www.nytimes.com/services/xml/rss/nyt/JobMarket.xml")

    ("features.magazine" "MAGAZINE"
     "http://www.nytimes.com/services/xml/rss/nyt/Magazine.xml")

    ("features.movie.news" "MOVIE NEWS"
     "http://www.nytimes.com/services/xml/rss/nyt/MovieNews.xml")

    ("features.movie.reviews" "MOVIE REVIEWS"
     "http://www.nytimes.com/services/xml/rss/nyt/Movies.xml")

    ("features.realestate" "REAL ESTATE"
     "http://www.nytimes.com/services/xml/rss/nyt/RealEstate.xml")

    ("features.theater" "THEATER"
     "http://www.nytimes.com/services/xml/rss/nyt/Theater.xml")

    ("features.travel" "TRAVEL"
     "http://www.nytimes.com/services/xml/rss/nyt/Travel.xml")
    ("features.travel.escapes" "Escapes"
     "http://www.nytimes.com/services/xml/rss/nyt/Escapes.xml")

    ("features.week_in_review" "WEEK IN REVIEW"
     "http://www.nytimes.com/services/xml/rss/nyt/WeekinReview.xml")

    ("additional.pop_top" "MOST E-MAILED ARTICLES"
     "http://www.nytimes.com/services/xml/rss/nyt/pop_top.xml")

    ;;("additional.multimedia" "MULTIMEDIA"
    ;; "http://www.nytimes.com/services/xml/rss/nyt/Multimedia.xml")

    ("opinion.editorial" "EDITORIALS / OP-ED"
     "http://www.nytimes.com/services/xml/rss/nyt/Opinion.xml")))

(defvar shimbun-nytimes-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAAHYAAAAQAgMAAAC+ZGPFAAAADFBMVEVLS0u8vLz///8ICAg
 XQ6oSAAABe0lEQVQY02OYkJm5atWqZavAwA1Er1i1yjETwl/AUP/5CZDuX/0LSK60qwGS81et+v8
 /CirNah8DpCer3wJx98YDifWrVor8KYJKp06dA6SX38paApLOB0uvCgvrgkq3XJsza8Wqpb+ylDV
 TgNIrtWbmL8xyT5u1Kitr6coABo9rcwwna036lHL8+v1M2/gJX43f96x8HmZYeOSWz+QPDCfuzNl
 b8qqoNtbyevKKv/F9ZaXro1Y89+vrNT153SmB4cS1OX2lWdN6YiOvJ6/0ze8rK1v/a8XztL65ZSd
 vNh5g+KEW01e2atn62JXXk1f1gqWNVlxJBUmfmmvAYLsUJL1wOZL03pXfloCl0wwY9gvP96vNqte
 Ojzx+ESjd72Y4Pz7Lxre31/Cr4f4DDJP/Tuibu4o5Mz3LS2pVqO/yrKYFS1f1f5s7t4yrzTmBYWX
 UqqZVq6TAobBSMxQS1kuzwNSsBQxAkgvIgEj//78KBYClEcBGGK/0qqVo0gCtEBjnqbJU8gAAAAB
 JRU5ErkJggg==")))

(defvar shimbun-nytimes-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-nytimes))
  (mapcar 'car shimbun-nytimes-group-table))

(luna-define-method shimbun-index-url ((shimbun shimbun-nytimes))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-nytimes-group-table)))

(defvar shimbun-nytimes-retry-fetching 1)

(defvar shimbun-nytimes-japanese-hankaku 'never)

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-nytimes)
					    header url)
  (goto-char (point-min))
  (when (re-search-forward
	 "<a[\t\n ]+\\([^>]+\\)>[\t\n ]*next[\t\n ]+page[^<]*</a>"
	 nil t)
    (let ((start (match-beginning 1))
	  (end (match-end 1)))
      (goto-char start)
      (when (and (re-search-forward
		  "class=\"next\"\\|title=\"next[\t\n ]+page\""
		  end t)
		 (progn
		   (goto-char start)
		   (re-search-forward "href=\"\\([^\"]+\\)\"" end t)))
	(shimbun-expand-url (match-string 1) url)))))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-nytimes)
						    header)
  (or (shimbun-nytimes-clear-contents shimbun header)
      (progn
	(erase-buffer)
	(insert "<html><body><i>This article may have been expired,\
 use the format different from the ordinary style that NYTimes uses,\
 or have not been successful to fetch.  Sorry.</i></body></html>\n")
	nil)))

(defun shimbun-nytimes-clear-contents (shimbun header)
  (shimbun-strip-cr)
  (let ((start "\
\\(?:\
\\(?:<p[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"post-author\"\
\\|\\(<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\
\"\\(?:entry\\|post\\)-content\"\\)\\)\
\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]*>\
\\|\
<NYT_\\(?:BYLINE\\|TEXT\\)\\(?:[\t\n ]*\\|[\t\n ]+[^>]+\\)>\
\\)[\t\n ]*")
	(end "[\t\n ]*\\(\\(<[^>]+>[\t\n ]*\\)*\
\\(?:\
<!-+[\t\n ]*end[\t\n ]+post-content[\t\n ]*-+>\
\\|\
<\\(?:/?NYT_UPDATE_BOTTOM\\|/NYT_TEXT\\)\\(?:[\t\n ]+[^>]+\\)?>\
\\)\\)")
	(case-fold-search t)
	pcont name)
    (goto-char (point-min))
    (when (or (and (re-search-forward start nil t)
		   (progn
		     (save-restriction
		       (setq pcont
			     ;; The marker version of (match-beginning 1).
			     (nth 2 (match-data)))
		       (narrow-to-region (point-min) (match-end 0))
		       (if (and (search-backward "</NYT_HEADLINE>" nil t)
				(re-search-forward "\
<div[\t\n ]+class=\"image\""
						   nil t)
				(progn
				  (setq start (match-beginning 0))
				  (shimbun-end-of-tag "div")))
			   (progn
			     (delete-region (match-end 0) (point-max))
			     (delete-region (point-min) start)
			     (goto-char (point-max)))
			 (delete-region (point-min) (point-max))))
		     (when (looking-at "</NYT_BYLINE>[\t\n ]*")
		       (delete-region (point-min) (match-end 0)))
		     (or (when (re-search-forward end nil t)
			   (delete-region
			    (if (and (match-beginning 2)
				     (progn
				       (goto-char (match-beginning 1))
				       (re-search-forward "\
\\(?:<[^>]+>\\)*\\(</blockquote>\\|</div>\\|</ul>\\)[\t\n ]*"
							  (match-end 2) t)))
				(match-end 1)
			      (match-beginning 0))
			    (point-max))
			   t)
			 (when (and pcont
				    (progn
				      (goto-char pcont)
				      (insert "<div>")
				      (goto-char pcont)
				      (shimbun-end-of-tag "div" t)))
			   (delete-region (match-end 3) (point-max))
			   (delete-region (point-min) (match-beginning 3))
			   t))))
	      (progn
		;; Extract blog listing.
		(goto-char (point-min))
		(when (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*id=\"blog_comments\""
					      nil t)
			   (shimbun-end-of-tag "div" t))
		  (delete-region (match-end 3) (point-max))
		  (delete-region (point-min) (match-beginning 3))
		  ;; Remove <ul>.
		  (goto-char (point-min))
		  (when (re-search-forward "\
<ul[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"commentlist\""
					   nil t)
		    (cond ((shimbun-end-of-tag "ul" t)
			   (delete-region (goto-char (match-end 3))
					  (match-end 0))
			   (insert "\n")
			   (delete-region (goto-char (match-beginning 0))
					  (match-beginning 3))
			   (insert "\n"))
			  ((shimbun-end-of-tag nil t)
			   (replace-match "\n"))))
		  ;; Remove useless links.
		  (goto-char (point-min))
		  (while (and (re-search-forward "\
<a[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*href=\"#"
						 nil t)
			      (shimbun-end-of-tag "a"))
		    (replace-match "\\2<br>"))
		  t)))
      ;; Insert a new line after every image.
      (goto-char (point-min))
      (while (re-search-forward "\\(<img[\t\n ]+[^>]+>\\)[\t\n ]*" nil t)
	(replace-match "\\1<br>"))
      ;; Remove the `Skip to next paragraph' buttons.
      (goto-char (point-min))
      (while (re-search-forward "\[\t\n ]*\
\\(?:<div[\t\n ]+[^>]+>[\t\n ]*\\)*\
<a[\t\n ]+href=\"#\\([^\"]+\\)\"[^>]*>[\t\n ]*\
Skip[\t\n ]+to[\t\n ]+next[\t\n ]+paragraph[\t\n ]*</a>[\t\n ]*"
				nil t)
	(setq start (match-beginning 0)
	      end (match-end 0)
	      name (match-string 1))
	(when (re-search-forward (concat "[\t\n ]*<a[\t\n ]+name=\""
					 (regexp-quote name)
					 "\"[^>]*>[\t\n ]*</a>[\t\n ]*")
				 nil t)
	  ;;(delete-region (match-beginning 0) (match-end 0))
	  ;; NYTimes is apt to forget to put this.
	  (replace-match "</ul>")
	  (delete-region (goto-char start) end)
	  (insert "\n")))
      ;; Remove Next/Previous buttons.
      (goto-char (point-min))
      (when (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*id=\"pageLinks\""
				    nil t)
		 (shimbun-end-of-tag "div" t))
	(replace-match "\n"))
      ;; Remove `Enlarge This Image', `Multimedia', and `Video'.
      (goto-char (point-min))
      (while (and (re-search-forward "<div[\t\n ]+\
\\(?:class=\"enlargeThis\\|id=\"inlineMultimedia\
\\|class=\"inlineVideo\\(?:[\t\n ]+[^\"]+\\)?\\)\""
				     nil t)
		  (shimbun-end-of-tag "div" t))
	(replace-match "\n"))
      ;; Remove javascripts.
      (goto-char (point-min))
      (while (and (re-search-forward "[\t\n ]*\
<a[\t\n ]+href=\"javascript:[^>]+>[\t\n ]*"
				     nil t)
		  (progn
		    (setq start (match-beginning 0)
			  end (match-end 0))
		    (re-search-forward "[\t\n ]*</a>[\t\n ]*" nil t)))
	(replace-match "\n")
	(delete-region (goto-char start) end)
	(insert "\n"))
      ;; Remove useless timesselect stuff.
      (goto-char (point-min))
      (while (re-search-forward "[\t\n ]*<img\\(?:[\t\n ]+[^\t\n >]+\\)*\
\[\t\n ]+src=\"[^\"]*/ts_icon\\.gif\"\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]*>\
\[\t\n ]*"
				nil t)
	(delete-region (match-beginning 0) (match-end 0)))
      ;; Replace wide apostrophe with the normal one.
      (goto-char (point-min))
      (while (re-search-forward "&#8217;\\|&#x2019;" nil t)
	(replace-match "&#39;"))
      ;; Add page delimiters.
      (goto-char (point-min))
      (while (re-search-forward "[\t\n ]*\\(?:<p>[\t\n ]*\\)+\
\\(<font[\t\n ]+[^>]+>[\t\n ]*(Page[\t\n ]+[0-9]+[\t\n ]+of[\t\n ]+[0-9]+)\
\[\t\n ]*</font>\\)\\(?:[\t\n ]*<p>\\)+[\t\n ]*"
				nil t)
	(replace-match "\n&#012;\\1\n<p>"))
      ;; Add last newline.
      (goto-char (point-max))
      (unless (bolp)
	(insert "\n"))
      t)))

(luna-define-method shimbun-rss-build-message-id :around ((shimbun
							   shimbun-nytimes)
							  url &optional date)
  ;; Don't strip string following "?" or "#" in url.  See sb-rss.el.
  (concat "<" (md5 url) "%" (shimbun-current-group shimbun)
	  "@" (shimbun-server shimbun) ".shimbun.namazu.org>"))

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-nytimes)
						 &optional range)
  (let ((name (cadr (assoc (shimbun-current-group-internal shimbun)
			   shimbun-nytimes-group-table)))
	(apostrophe (condition-case nil
			(make-char 'japanese-jisx0208 33 71)
		      (error nil)))
	(headers (luna-call-next-method))
	from)
    (dolist (header headers headers)
      ;; Show the group name in the From header.
      (when (and (setq from (shimbun-header-from header))
		 (string-match "\\`By [A-Z][A-Z]+" from))
	(setq from (substring from 3)))
      (shimbun-header-set-from header (concat from " <" name ">"))
      ;; Replace wide apostrophe with the normal one in the subject.
      (when apostrophe
	(shimbun-header-set-subject
	 header (shimbun-subst-char-in-string
		 apostrophe ?' (shimbun-header-subject header t)))))))

(provide 'sb-nytimes)

;;; sb-nytimes.el ends here
