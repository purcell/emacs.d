;;; sb-bbc.el --- shimbun backend for BBC UK

;; Copyright (C) 2003, 2004, 2005, 2006, 2007
;; Koichiro Ohba <koichiro@meadowy.org>

;; Author: Koichiro Ohba <koichiro@meadowy.org>
;; Keywords: news
;; Created: Jun 18, 2003

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

(luna-define-class shimbun-bbc (shimbun-rss) ())

(defvar shimbun-bbc-url
  "http://newsrss.bbc.co.uk/rss/newsonline_uk_edition")
(defvar shimbun-bbc-from-address  "newsonline@bbc.co.uk")
(defvar shimbun-bbc-content-start
  (concat "<!-- "
	  (regexp-opt '("E IBYL" "E IIMA" "S IBOX" "S IMA" "S BO"))
	  " -->"))
(defvar shimbun-bbc-content-end "<!-- E BO -->")

(defvar shimbun-bbc-path-alist
  '(("front_page" . "/front_page/rss.xml")
    ;; use the name "news" here to be backward compatible
    ;; ("world" . "/world/rss.xml")
    ("news" . "/world/rss.xml")
    ("uk" . "/uk/rss.xml")
    ("england" . "/england/rss.xml")
    ("northern_ireland" . "/northern_ireland/rss.xml")
    ("scotland" . "/scotland/rss.xml")
    ("wales" . "/wales/rss.xml")
    ("business" . "/business/rss.xml")
    ("politics" . "/uk_politics/rss.xml")
    ("health" . "/health/rss.xml")
    ("education" . "/education/rss.xml")
    ("science" . "/sci/tech/rss.xml")
    ("technology" . "/technology/rss.xml")
    ("entertainment" . "/entertainment/rss.xml")
    ("talking_point" . "/talking_point/rss.xml")
    ("magazine" . "/magazine/rss.xml")
    ("week_at-a-glance" . "/week_at-a-glance/rss.xml")
    ("programmes" . "programmes/rss.xml")
    ("latest_stories" . "/latest_published_stories/rss.xml")))

(defvar shimbun-bbc-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAACoAAAAOAgMAAAAkkGboAAAADFBMVEW7uLJ/e2z////09PP
 b5J9PAAAAjElEQVQI12OYGhqaGRoaBqQSGLJWrZq2atVKIDWBIev/qmnrf63M2v8KyBatmrYwfGW
 WaByQLeM1beGRlRn+h4BsqahpC5euzNUCqRcEqhFfmfgKxBaIm7aQ9SWELXVp2sK1molgNfJC0xZ
 +1IToFfWbtjDkZbZoEJB9/tW0Nf9WZm34NQHFDaEItwEAmSVN3A2XO9kAAAAASUVORK5CYII=")))

(defvar shimbun-bbc-groups (mapcar 'car shimbun-bbc-path-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-bbc))
  (concat shimbun-bbc-url
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-bbc-path-alist))))

(provide 'sb-bbc)

;;; sb-bbc.el ends here
