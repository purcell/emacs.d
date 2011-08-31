;;; sb-sueddeutsche-de.el --- sueddeutsche.de shimbun backend

;; Copyright (C) 2008, 2009 David Engster

;; Author: David Engster <dengste@eml.cc>
;; Keywords: news

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

(luna-define-class shimbun-sueddeutsche-de (shimbun-rss) ())

(defvar shimbun-sueddeutsche-de-group-url
  '(("alles"
     "http://www.sueddeutsche.de/app/service/rss/alles/rss.xml")
    ("topthemen"
     "http://www.sueddeutsche.de/app/service/rss/topthemen/topthemen.xml")
    ("politik"
     "http://rss.sueddeutsche.de/rss/Politik")
    ("wirtschaft"
     "http://rss.sueddeutsche.de/rss/Wirtschaft")
    ("finanzen"
     "http://rss.sueddeutsche.de/rss/Geld")
    ("kultur"
     "http://rss.sueddeutsche.de/rss/Kultur")
    ("sport"
     "http://rss.sueddeutsche.de/rss/Sport")
    ("bayern"
     "http://rss.sueddeutsche.de/rss/Bayern")
    ("muenchen"
     "http://rss.sueddeutsche.de/rss/M%C3%BCnchen")
    ("panorama"
     "http://rss.sueddeutsche.de/rss/Panorama")
    ("leben"
     "http://rss.sueddeutsche.de/rss/Leben%20&%20Stil")
    ("gesundheit"
     "http://rss.sueddeutsche.de/rss/Gesundheit")
    ("computer"
     "http://rss.sueddeutsche.de/rss/Computer")
    ("immobilien"
     "http://rss.sueddeutsche.de/rss/Immobilien")
    ("wissen"
     "http://rss.sueddeutsche.de/rss/Wissen")
    ("jobs"
     "http://rss.sueddeutsche.de/rss/Job%20&%20Karriere")
    ("reise"
     "http://rss.sueddeutsche.de/rss/Reise")))

(defvar shimbun-sueddeutsche-de-groups
  (mapcar 'car shimbun-sueddeutsche-de-group-url))
(defvar shimbun-sueddeutsche-de-from-address "invalid@sueddeutsche.de")
(defvar shimbun-sueddeutsche-de-content-start
  "<!--.*?[Bb]egin.*?[cC]ontent.*?-->\\|class=\"artikelBox\"")
(defvar shimbun-sueddeutsche-de-content-end
  "<!--.*?[eE]nde.*?[cC]ontent.*?-->")

(defvar shimbun-sueddeutsche-de-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABAAAAAQAgMAAABinRfyAAAADFBMVEXLyspMSkr///9+fX1
 CK4DEAAAARUlEQVQI12NYBQQMCKL/jv13hk1rX01hkFqxSo5BRGtVEMPWohVVDAvDV3oxrGp9+4q
 ha+VTLQaR1earGNb/2W7PgGoAAO3JJfDNz7QzAAAAAElFTkSuQmCC")))

(luna-define-method shimbun-index-url ((shimbun shimbun-sueddeutsche-de))
  (let ((group (shimbun-current-group-internal shimbun)))
    (cadr (assoc group shimbun-sueddeutsche-de-group-url))))

(luna-define-method shimbun-rss-build-message-id ((shimbun
						   shimbun-sueddeutsche-de)
						  url date)
  (let ((group (shimbun-current-group-internal shimbun))
	id)
    (cond ((string-match
	    ".*sueddeutsche\\.de.*/\\(.+\\)/\\(.+\\)/?" url)
	   (concat "<" (match-string 1 url) "." (match-string 2 url) "." group
		   "@sueddeutsche.de>"))
	  (t
	   (error "Cannot find message-id base")))))

(luna-define-method shimbun-clear-contents :before ((shimbun
						     shimbun-sueddeutsche-de)
						    header)
  (shimbun-remove-tags "<!-- Stoerer //-->" "<!-- END Stoerer //-->")
  (shimbun-remove-tags "<span class=\"hidePrint\">" "</span>")
  (shimbun-remove-tags "<table.*?class=\"stoerBS\".*?>" "</table>")
  (shimbun-remove-tags "<\\(?:a\\|span\\) .*?bildstrecke.*?>"
		       "</\\(?:a\\|span\\)>")
  (shimbun-remove-tags "<td class=\"artikelDruckenRight\" align=\"right\">"
		       "class=\"artikelDachzeile\"")
  (shimbun-remove-tags "<div class=\"bannerOben\">" "<div class=\"bannerUnten\">"))

(luna-define-method shimbun-article-url ((shimbun shimbun-sueddeutsche-de)
					 header)
  ;; retrieve real URL and choose print-version
  (let ((url (shimbun-header-xref header)))
    (when (string-match "html?$" url)
      (setq url
	    (car (last
		  (w3m-process-with-wait-handler
		    (w3m-w3m-attributes url nil handler))))))
    (cond
     ((string-match "\\(.*jetzt.*sueddeutsche.*de.*\\)texte/anzeigen/\\(.+\\)" url)
      (setq url (concat (match-string 1 url) "drucken/text/" (match-string 2 url))))
     ((string-match "text/$" url)
      (setq url (concat url "print.html"))))
    url))

(provide 'sb-sueddeutsche-de)

;;; sb-sueddeutsche-de.el ends here
