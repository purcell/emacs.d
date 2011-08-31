;;; sb-zeit-de.el --- shimbun backend for <http://www.zeit.de>

;; Copyright (C) 2004, 2005, 2006, 2008, 2009
;; Andreas Seltenreich <seltenreich@gmx.de>

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

;;; Commentary:

;; Macro used to extract groups from the overview-page
;; (fset 'sb-zeit-de-macro [?\C-s ?d ?e ?/ ?\C-m ?\C-  ?\C-a ?\C-w ?\"
;; 			       ?\M-f ?\" ?\C-k ?\C-k ?\C-k return ?\C-k])

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-zeit-de (shimbun-rss) ())

(defvar shimbun-zeit-de-groups
  '("politik" "wirtschaft" "meinung" "gesellschaft" "kultur"
    "wissen" "digital" "studium" "karriere" "lebensart" "reisen"
    "auto" "sport" "blogs" "news"))

(defvar shimbun-zeit-de-x-face-alist
  '(("default" . "X-Face: +@u:6eD3Nq>u{P_Ev&\"A6eW=EA{5H[OqH;|oz7H>atafNFsUS-&7\
%\\qo;KFS%E`=t5Z)'q~lhfl6<7rQ=]")))

(defvar shimbun-zeit-de-content-start
  "title\">\\|<!--content starts here-->\\(?:<table[^>]+>\\)?")

(defvar shimbun-zeit-de-content-end
  (concat
   "</body>\\|</html>\\|navigation[^><]*>[^A]\\|"
   "<script language=\"JavaScript1\.2\" type=\"text/javascript\">\\|"
   "<div[^>]+\\(class\\|id\\)=\"comments\\|<li class=\"bookmarks\\\|"
   "class=\"com\"\\|class=\"toolad\""))

(defvar shimbun-zeit-de-from-address "DieZeit@zeit.de")

(luna-define-method shimbun-headers :before ((shimbun shimbun-zeit-de)
					     &rest range)
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-zeit-de))
  shimbun-zeit-de-groups)

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-zeit-de)
						 &optional range)
  (mapc
   (lambda (header)
     (let ((url (shimbun-header-xref header)))
       ;; remove the "?from=rss" parameter
       (when (string-match "\\(.*\\)\\?from=rss$" url)
         (setq url (match-string 1 url)))
       (cond ((string-match "\\`http://www\\.zeit\\.de" url)
	      (shimbun-header-set-xref header (concat url "?page=all")))
	     ((string-match "\\`/" url)
	      (shimbun-header-set-xref
	       header (concat "http://www.zeit.de" url))))))
   (luna-call-next-method)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-zeit-de)
						   header)
  (when (re-search-forward "<script.*window.location='\\(.+?\\)';" nil t)
    (let ((url (match-string 1)))
      (erase-buffer)
      (shimbun-retrieve-url (concat url "?page=all"))))
  (let* ((case-fold-search t)
	 (start (re-search-forward (shimbun-content-start shimbun) nil t))
	 (end (and start
		   (re-search-forward (shimbun-content-end shimbun) nil t)
		   (prog1
		       (match-beginning 0)
		     (goto-char start)))))
    (setq case-fold-search nil)
    (when (re-search-forward "(c)[^Z]*ZEIT[^0-9]*\
\\([0-3][0-9]\\)\\.\\([01][0-9]\\)\\.\\(20[0-9][0-9]\\)"
			     end t)
      (shimbun-header-set-date
       header
       (shimbun-make-date-string (string-to-number (match-string 3))
				 (string-to-number (match-string 2))
				 (string-to-number (match-string 1))
				 nil
				 "+02:00"))
      (goto-char (point-min)))))

(luna-define-method shimbun-index-url ((shimbun shimbun-zeit-de))
  (let ((group (shimbun-current-group shimbun)))
    (if (equal "news" group)
	"http://newsfeed.zeit.de/"
      (concat "http://newsfeed.zeit.de/" group "/index"))))

(luna-define-method shimbun-clear-contents :after ((shimbun shimbun-zeit-de)
						    header)

  ;;  remove advertisements and 1-pixel-images aka webbugs
  (shimbun-remove-tags "<!--START: LESERMEINUNG-->" "<!--ENDE: LESERMEINUNG-->")
  (shimbun-remove-tags "<div[^>]*class=\"?\\(?:ad\\|most_read\\)" "</div>")
  (shimbun-remove-tags "<a[^>]*doubleclick.net" "</a>")
  (shimbun-remove-tags "<IFRAME[^>]*doubleclick.net[^>]*>")
  (shimbun-remove-tags "<img[^>]*doubleclick.net[^>]*>")
  (shimbun-remove-tags "<img[^>]*\\(width\\|height\\)=\"1px\"[^>]*>")
  (shimbun-remove-tags "<tr><td[^>]*>Anzeige</td></tr>")
  (shimbun-remove-tags "<span class=\"anzeige\">.+?</span>")
  t)

(provide 'sb-zeit-de)

;;; sb-zeit-de.el ends here
