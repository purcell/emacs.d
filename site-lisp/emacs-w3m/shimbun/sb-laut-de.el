;;; sb-laut-de.el --- shimbun backend for <http://www.laut.de/>

;; Copyright (C) 2004, 2005, 2006 Andreas Seltenreich <seltenreich@gmx.de>

;; Author: Andreas Seltenreich <seltenreich@gmx.de>
;; Keywords: news
;; Created: May 23, 2004

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

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-laut-de (shimbun-rss) ())

(defvar shimbun-laut-de-groups
  '("news"
    "platten"
    "platten_alternative"
    "platten_dance"
    "platten_hiphop"
    "platten_jazz"
    "platten_metal"
    "platten_pop"
    "platten_rnb"
    "platten_rock"))

(defvar shimbun-laut-de-content-start
  (concat
   "<!-- headline -->\\|"
   "<span class=\"ueberschriftnormalgrau[^>]*>\\|"
   "<span class=\"inhaltsueberschrift\">"))

(defvar shimbun-laut-de-content-end
  (concat "<!-- /box weitere Links -->\\|"
	  "<!-- commercialflaeche -->\\|"
	  "<!-- link zu lautbar -->"))

(defvar shimbun-laut-de-from-address "redaktion@laut.de")

(luna-define-method shimbun-groups ((shimbun shimbun-laut-de))
  shimbun-laut-de-groups)

(luna-define-method shimbun-index-url ((shimbun shimbun-laut-de))
  (concat "http://www.laut.de/partner/allgemein/"
	  (shimbun-current-group-internal shimbun) ".rdf"))

(luna-define-method shimbun-clear-contents :after ((shimbun shimbun-laut-de)
						     header)
  (shimbun-remove-tags "<script" "</script>")
  (shimbun-remove-tags
   "<img src=\"/images/\\(?:voting\\|leer\\)[^\"]+gif" ">")
  (shimbun-remove-tags
   "<img[^>]+\\(?:width=\"1\"\\|height=\"1\"\\)[^>]*>")
  (shimbun-remove-tags
   "<a href=\"[^\"]+lautshop_preisvergleich_detail.php" "</a>"))

(provide 'sb-laut-de)

;;; sb-laut-de.el ends here
