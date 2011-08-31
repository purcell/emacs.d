;;; sb-ffii.el --- shimbun backend for <http://www.ffii.org/news/rss/>

;; Copyright (C) 2004, 2005
;; Felix E. Klee <felix.klee@inka.de>
;; Andreas Seltenreich <seltenreich@gmx.de>

;; Authors:
;; * Felix E. Klee <felix.klee@inka.de>
;; * Andreas Seltenreich <seltenreich@gmx.de> (author of sb-n24-de.el
;;   which was used as a base for this module)
;; Keywords: news
;; Created: Feb 09, 2005

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

(luna-define-class shimbun-ffii (shimbun-rss) ())

(defvar shimbun-ffii-group-alist
  '(("en.software-patents" . "SwpatcninoEn")
    ("en.software-patents.ffii" . "SwpatcninoEn-ffii")
    ("en.information-infrastructure" . "FfiinewsEn")
    ("en.project" . "FfiiprojNewsEn")
    ("de.software-patente" . "SwpatcninoDe")
    ("de.software-patente.ffii" . "SwpatcninoDe-ffii")
    ("de.informations-infrastruktur" . "FfiinewsDe")
    ("fr.brevets-logiciels" . "SwpatcninoFr")
    ("fr.brevets-logiciels.ffii" . "SwpatcninoFr-ffii")
    ("nl.softwarepatenten" . "SwpatcninoNl")
    ("nl.softwarepatenten.ffii" . "SwpatcninoNl-ffii")))

(defvar shimbun-ffii-server-name "FFII")
(defvar shimbun-ffii-from-address "info@ffii.org")

(luna-define-method shimbun-groups ((shimbun shimbun-ffii))
  (mapcar 'car shimbun-ffii-group-alist))

;; Returns a Message-ID created from the URL url and the date date.
(luna-define-method shimbun-rss-build-message-id ((shimbun shimbun-ffii)
						  url date)
  (let (page host datedesc)
    ; Sometimes URLs are broken (e.g. I've seen something like http:http://x/).
    ; The following expression tries to deal with cases like this.
    (unless (string-match "[^:]*:\\/\\/\\([^\/]+\\)\\([^@<>]*\\)" url)
      (error "Cannot parse URL for message-id base"))
    (setq host (match-string-no-properties 1 url)
	  page (match-string-no-properties 2 url))
    ; Sometimes dates are broken, therefore the following expression is very
    ; fault tolerant.
    (unless (string-match "\\([^@<>]*\\)" date)
      (error "Cannot parse date for message-id base"))
    (setq datedesc (concat (match-string-no-properties 1 date)
			   (match-string-no-properties 2 date)
			   (match-string-no-properties 3 date)
			   (match-string-no-properties 4 date)
			   (match-string-no-properties 5 date)))
    (format "<%s%%%s@%s>" datedesc page host)))

(luna-define-method shimbun-index-url ((shimbun shimbun-ffii))
  (concat "http://www.ffii.org/news/rss/"
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-ffii-group-alist))
	  ".rss"))

;; Provides a dummy date string if date information is lacking.
(luna-define-method shimbun-rss-process-date :around ((shimbun shimbun-ffii)
						      date)
  (if (stringp date)
      (luna-call-next-method)
    (let ((time (decode-time)))
      (shimbun-make-date-string (nth 5 time)
				(nth 1 time)
				(nth 3 time)))))

(provide 'sb-ffii)

;;; sb-ffii.el ends here
