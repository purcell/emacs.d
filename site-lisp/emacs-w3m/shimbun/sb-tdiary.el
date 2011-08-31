;;; sb-tdiary.el --- shimbun backend for tDiary

;; Copyright (C) 2003, 2004, 2005, 2007 OHASHI Akira <bg66@koka-in.org>

;; Author: OHASHI Akira <bg66@koka-in.org>
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

(eval-when-compile
  (require 'cl))

(require 'shimbun)

(luna-define-class shimbun-tdiary (shimbun) ())

(defvar shimbun-tdiary-content-start "<h3")
(defvar shimbun-tdiary-content-end "</div>")

(defcustom shimbun-tdiary-group-alist nil
  "*An alist of TDIARY shimbun group definition.
Each element looks like (NAME URL).
NAME is a shimbun group name.
URL is the URL for TDIARY access point of the group."
  :group 'shimbun
  :type '(repeat (cons :format "%v"
		       (string :tag "Group name")
		       (string :tag "URL"))))

(luna-define-method shimbun-groups ((shimbun shimbun-tdiary))
  (mapcar 'car shimbun-tdiary-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-tdiary))
  (cadr (assoc (shimbun-current-group-internal shimbun)
	       shimbun-tdiary-group-alist)))

(defmacro shimbun-tdiary-get-headers (shimbun url headers &optional aux)
  `(let ((case-fold-search t))
     (goto-char (point-max))
     (while (re-search-backward "<h3[^>]*><a href=\"\./\\(.*\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\).*#\\(p[0-9]+\\)\\)\"> *\\(<span class=\"[ps]anchor\">.+</span>\\)?[^<]*</a>\\(.+]\\|\\) *\\(.+\\)</h3>" nil t)
       (let ((url (match-string 1))
	     (year (match-string 2))
	     (month (match-string 3))
	     (day (match-string 4))
	     (topic (match-string 5))
	     (subject (match-string 8))
	     date id)
	 (setq date (shimbun-make-date-string (string-to-number year)
					      (string-to-number month)
					      (string-to-number day)))
	 (setq id (format "<%s.%s%s%s.%s@tdiary.org>"
			  topic year month day
			  (eword-encode-string
			   (shimbun-current-group-internal ,shimbun))))
	 (push (shimbun-create-header
		0 subject (or ,aux (shimbun-from-address ,shimbun))
		date id "" 0 0 (concat (shimbun-index-url ,shimbun) url))
	       ,headers)))
     ,headers))

(defmacro shimbun-tdiary-make-date (count first)
  `(let* ((today (current-time))
	  (month (nth 4 (decode-time today)))
	  (year (nth 5 (decode-time today)))
	  (dow (nth 6 (decode-time today)))
	  (dst (nth 7 (decode-time today)))
	  (zone (nth 8 (decode-time today))))
     (dotimes (i (1- ,count))
       (decf month)
       (when (<= month 0)
	 (setq month 12)
	 (decf year)))
     (let ((date
	    (format-time-string
	     "%Y%m"
	     (encode-time 0 0 0 1 month year dow dst zone))))
       (if (string< date ,first)
	   nil
	 date))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-tdiary)
					 &optional range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	(count 0) (first "")
	headers month months author)
    (goto-char (point-min))
    (when (re-search-forward "<meta name=\"author\" content=\"\\(.*\\)\">" nil t)
      (setq author (match-string 1))
      (when (re-search-forward "<link rev=\"made\" href=\"mailto:\\(.*\\)\">" nil t)
	(setq author (format "%s <%s>" author (match-string 1)))))
    (goto-char (point-min))
    (when (re-search-forward "<link rel=\"first\" .* href=\"\./.*\\([0-9][0-9][0-9][0-9][0-9][0-9]\\)" nil t)
      (setq first (match-string 1)))
    (while (and (incf count)
		(if pages (<= count pages) t)
		(setq month (shimbun-tdiary-make-date count first))
		(push month months)))
    (setq months (nreverse months))
    (dolist (month months)
      (let ((url (concat (shimbun-index-url shimbun) "?date=" month)))
	(erase-buffer)
	(shimbun-retrieve-url url t)
	(goto-char (point-min))
	(while (search-forward "\r" nil t)
	  (delete-char -1))
	(shimbun-tdiary-get-headers shimbun url headers author)))
    headers))

(defvar shimbun-tdiary-footnote-regex
  (concat "<span class=\"footnote\">"
	  "<a +name=\"\\([^\"]*\\)\" +"
	  "href=\"\\([^#]*\\)#[^\"]+\" +"
	  "title=\"\\([^\"]*\\)\">"
	  "\\(\\*[0-9]+\\)</a></span>")
  "Regexp of footnote.")

(luna-define-method shimbun-make-contents ((shimbun shimbun-tdiary)
					   header)
  (let ((case-fold-search t)
	(id (shimbun-header-id header))
	(start (shimbun-content-start shimbun))
	footnotes)
    (when (string-match "\\(p[0-9]+\\)\." id)
      (setq id (substring id (match-beginning 1) (match-end 1)))
      (re-search-forward (concat "<a name=\"" id) nil t)
      (and (search-backward start nil t)
	   (goto-char (match-beginning 0))))
    (when (and (re-search-forward start nil t)
	       (setq start (match-beginning 0))
	       (re-search-forward (shimbun-content-end shimbun) nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start))
    (goto-char (point-min))
    (while (re-search-forward shimbun-tdiary-footnote-regex nil t)
      (setq footnotes (cons (format "<a href=\"%s#%s\">%s</a> %s<br>\n"
				    (match-string 2) (match-string 1)
				    (match-string 4) (match-string 3))
			    footnotes)))
    (when footnotes
      (goto-char (point-max))
      (insert "<br><br>\n")
      (mapc 'insert (nreverse footnotes)))
    (shimbun-header-insert-and-buffer-string shimbun header nil t)))

(provide 'sb-tdiary)

;;; sb-tdiary.el ends here
