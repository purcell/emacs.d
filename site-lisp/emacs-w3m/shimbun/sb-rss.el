;;; sb-rss.el --- shimbun backend for RSS (Rich Site Summary).

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
;; Koichiro Ohba <koichiro@meadowy.org>
;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
;; NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: Koichiro Ohba <koichiro@meadowy.org>
;;         NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Keywords: news
;; Created: Jun 14, 2003

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
  (require 'cl)
  (require 'static))

(require 'shimbun)
(eval-when-compile
  (ignore-errors
    (require 'xml)))
(eval '(require 'xml))

(eval-and-compile
  (luna-define-class shimbun-rss (shimbun) (ignored-subject)))

(luna-define-method initialize-instance :after ((shimbun shimbun-rss)
						&rest init-args)
  (shimbun-rss-initialize-ignored-subject shimbun))

(defun shimbun-rss-initialize-ignored-subject (shimbun)
  (luna-set-slot-value shimbun 'ignored-subject
		       (symbol-value
			(intern-soft (format "shimbun-%s-ignored-subject"
					     (shimbun-server shimbun)))))
  shimbun)

(luna-define-generic shimbun-rss-process-date (shimbun-rss date)
  "Process DATE string and return proper Date string to show it in MUA.")

(autoload 'timezone-parse-date "timezone")

(luna-define-method shimbun-rss-process-date ((shimbun shimbun-rss) date)
  ;; make Date string from ISO 8601 date format.  See
  ;; http://www.w3.org/TR/NOTE-datetime.
  ;;    Year:
  ;;       YYYY (eg 1997)
  ;;    Year and month:
  ;;       YYYY-MM (eg 1997-07)
  ;;    Complete date:
  ;;       YYYY-MM-DD (eg 1997-07-16)
  ;;    Complete date plus hours and minutes:
  ;;       YYYY-MM-DDThh:mmTZD (eg 1997-07-16T19:20+01:00)
  ;;    Complete date plus hours, minutes and seconds:
  ;;       YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)
  ;;    Complete date plus hours, minutes, seconds and a decimal fraction of a
  ;; second
  ;;       YYYY-MM-DDThh:mm:ss.sTZD (eg 1997-07-16T19:20:30.45+01:00)
  ;; where:
  ;;      YYYY = four-digit year
  ;;      MM   = two-digit month (01=January, etc.)
  ;;      DD   = two-digit day of month (01 through 31)
  ;;      hh   = two digits of hour (00 through 23) (am/pm NOT allowed)
  ;;      mm   = two digits of minute (00 through 59)
  ;;      ss   = two digits of second (00 through 59)
  ;;      s    = one or more digits representing a decimal fraction of a second
  ;;      TZD  = time zone designator (Z or +hh:mm or -hh:mm)
  ;;
  ;;      YYYY = four-digit year
  ;;      MM   = two-digit month (01=January, etc.)
  ;;      DD   = two-digit day of month (01 through 31)
  ;;      hh   = two digits of hour (00 through 23) (am/pm NOT allowed)

  ;; In addition to the above, it also supports the date format in the
  ;; RFC822 style which RSS 2.0 allows.
;;;<DEBUG>
;;  (shimbun-rss-process-date-1 date))
;;
;;(defun shimbun-rss-process-date-1 (date)
;;;</DEBUG>
  (let (vector year month day time zone)
    (cond ((null date))
	  ((string-match " [0-9]+ " date)
	   (setq vector (timezone-parse-date date)
		 year (string-to-number (aref vector 0)))
	   (when (>= year 1970)
	     (setq month (string-to-number (aref vector 1))
		   day   (string-to-number (aref vector 2))
		   time  (aref vector 3))
	     (when (setq zone (aref vector 4))
	       (unless (string-match "\\`[A-Z+-]" zone)
		 (setq zone nil)))))
	  ((string-match
	    "\\([0-9][0-9][0-9][0-9]\\)\\(-[0-9][0-9]\\)?\\(-[0-9][0-9]\\)?T?\\([0-9][0-9]:[0-9][0-9]\\(:[.0-9]+\\)?\\)?\\([-+][0-9][0-9]:?[0-9][0-9]\\|Z\\)?"
	    date)
	   (setq year  (string-to-number (match-string 1 date))
		 month (if (match-beginning 2)
			   (string-to-number (substring (match-string 2 date)
							1))
			 1)
		 day   (if (match-beginning 3)
			   (string-to-number (substring (match-string 3 date)
							1))
			 1)
		 time  (or (match-string-no-properties 4 date) "00:00")
		 zone  (match-string-no-properties 6 date))
	   (when zone
	     (cond ((null zone))
		   ((string-equal zone "Z")
		    (setq zone "+0000"))
		   ((string-match ":" zone)
		    (setq zone (concat (substring zone 0 (match-beginning 0))
				       (substring zone (match-end 0)))))))))
    (if month
	(shimbun-make-date-string year month day time zone)
      "")))

(luna-define-generic shimbun-rss-get-date (shimbun-rss url)
  "Process URL and return a Date string for an article of the URL.
When a RSS file does not contain any date information for each article,
but you can identify it from the URL, define this method in a backend.")

(luna-define-method shimbun-rss-get-date ((shimbun shimbun-rss) url)
  nil)

(luna-define-generic shimbun-rss-build-message-id (shimbun-rss
						   url &optional date)
  "Build unique message-id from URL and (optionally) DATE, and return it.")

(luna-define-method shimbun-rss-build-message-id ((shimbun shimbun-rss)
						  url &optional date)
  (when (string-match "[?#]" url)
    (setq url (substring url 0 (match-beginning 0))))
  (concat "<" (md5 url) "%" (shimbun-current-group shimbun)
	  "@" (shimbun-server shimbun) ".shimbun.namazu.org>"))

(luna-define-method shimbun-headers ((shimbun shimbun-rss) &optional range)
  (with-temp-buffer
    (let ((case-fold-search t))
      (shimbun-retrieve-url
       (shimbun-index-url shimbun) 'no-cache 'no-decode)
      ;; In some rss feeds, LFs might be used mixed with CRLFs.
      (shimbun-strip-cr)
      (insert
       (prog1
	   (decode-coding-string (buffer-string) (shimbun-rss-get-encoding))
	 (erase-buffer)
	 (set-buffer-multibyte t)))
      (shimbun-get-headers shimbun range))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-rss)
					 &optional range)
  (shimbun-rss-get-headers shimbun range t))

(defun shimbun-rss-get-headers (shimbun &optional range
					need-descriptions need-all-items)
  "Get headers from rss feed described by SHIMBUN.
RANGE is currently ignored.  If NEED-DESCRIPTIONS, include node
text as description.  By default, only existing and new items
from the feed are returned, i.e., those items which are newer
than the oldest one in the shimbun.  If NEED-ALL-ITEMS is
non-nil, all items from the feed are returned.  If the entries
from the feed have date information, the result is sorted by
ascending date."
  (let* ((xml (condition-case err
		  (xml-parse-region (point-min) (point-max))
		(error
		 (message "Error while parsing %s: %s"
			  (shimbun-index-url shimbun)
			  (error-message-string err))
		 nil)))
	 header headers oldheaders newheaders oldest)
    (dolist (tmp (shimbun-rss-get-headers-1 xml shimbun need-descriptions))
      (let* ((date (shimbun-header-date tmp))
	     (ftime
	      (when (and (stringp date)
			 (> (length date) 1))
		(w3m-float-time (date-to-time date)))))
	(push (list tmp ftime) headers)))
    (when headers
      (if (or need-all-items
	      ;; If there's a header without date information, we
	      ;; return everything, just to be safe.
	      (memq nil (mapcar 'cadr headers)))
	  (mapcar 'car headers)
	;; Otherwise, sort according to date.
	(setq headers
	      (sort headers (lambda (a b)
			      (> (cadr a) (cadr b)))))
	(while headers
	  (setq header (pop headers))
	  (if (shimbun-search-id shimbun (shimbun-header-id (car header)))
	      (push header oldheaders)
	    (push header newheaders)))
	(if (null oldheaders)
	    ;; All items are new
	    (mapcar 'car newheaders)
	  ;; Delete all items which are older than the ones we already
	  ;; have
	  (setq oldest (cadr (car oldheaders)))
	  (while (and newheaders
		      (> oldest (cadr (car newheaders))))
	    (setq newheaders (cdr newheaders)))
	  (append
	   (mapcar 'car newheaders)
	   (mapcar 'car oldheaders)))))))

(defun shimbun-rss-get-headers-1 (xml shimbun need-descriptions)
  "Retrieve all items found in XML for SHIMBUN and return headers.
If NEED-DESCRIPTIONS, include node text as description."
  (when xml
    (let  ((dc-ns (shimbun-rss-get-namespace-prefix
		   xml "http://purl.org/dc/elements/1.1/"))
	   (rss-ns (shimbun-rss-get-namespace-prefix
		    xml "http://purl.org/rss/1.0/"))
	   (ignored-subject (luna-slot-value shimbun 'ignored-subject))
	   author hankaku headers)
      (setq author
	    (catch 'found-author
	      (dolist (channel
		       (shimbun-rss-find-el (intern (concat rss-ns "channel"))
					    xml))
		(throw 'found-author
		       (or
			(shimbun-rss-node-text rss-ns 'author channel)
			(shimbun-rss-node-text dc-ns 'creator channel)
			(shimbun-rss-node-text dc-ns 'contributor channel)))))
	    hankaku (unless (memq (shimbun-japanese-hankaku shimbun)
				  '(body nil))
		      (generate-new-buffer " *temp*")))
      (unwind-protect
	  (dolist (item (shimbun-rss-find-el (intern (concat rss-ns "item"))
					     xml)
			headers)
	    (let ((url (and (listp item)
			    (eq (intern (concat rss-ns "item")) (car item))
			    (shimbun-rss-node-text rss-ns 'link (cddr item)))))
	      (when url
		(let* ((date (or (shimbun-rss-get-date shimbun url)
				 (shimbun-rss-node-text dc-ns 'date item)
				 (shimbun-rss-node-text rss-ns 'pubDate item)))
		       (id (shimbun-rss-build-message-id shimbun url date))
		       (subject (shimbun-rss-node-text rss-ns 'title item)))
		  (when id
		    (unless (and ignored-subject subject
				 (string-match ignored-subject subject))
		      (push
		       (shimbun-create-header
			0
			(if hankaku
			    (with-current-buffer hankaku
			      (insert (or subject ""))
			      (shimbun-japanese-hankaku-region (point-min)
							       (point-max))
			      (prog1 (buffer-string) (erase-buffer)))
			  subject)
			(or (shimbun-rss-node-text rss-ns 'author item)
			    (shimbun-rss-node-text dc-ns 'creator item)
			    (shimbun-rss-node-text dc-ns 'contributor item)
			    author
			    (shimbun-from-address shimbun))
			(shimbun-rss-process-date shimbun date)
			id "" 0 0 url
			(when need-descriptions
			  (let ((description (shimbun-rss-node-text
					      rss-ns 'description item)))
			    (when description
			      (list (cons 'description description))))))
		       headers)))))))
	(when (buffer-live-p hankaku)
	  (kill-buffer hankaku))))))

;;; Internal functions

;;; XML functions

(defvar shimbun-rss-compatible-encoding-alist
  '((iso-8859-1 . windows-1252)
    (iso-8859-8 . windows-1255)
    (iso-8859-9 . windows-1254))
  "Alist of encodings and those supersets.
The cdr of each element is used to decode data if it is available when
the car is what the data specify as the encoding.  Or, the car is used
for decoding when the cdr that the data specify is not available.")

(defun shimbun-rss-get-encoding ()
  "Return an encoding attribute specified in the current xml contents.
If `shimbun-rss-compatible-encoding-alist' specifies the compatible
encoding, it is used instead.  If the xml contents doesn't specify the
encoding, return `utf-8' which is the default encoding for xml if it
is available, otherwise return nil."
  (goto-char (point-min))
  (if (re-search-forward
       "<\\?[^>]*encoding=\\(\"\\([^\">]+\\)\"\\|'\\([^'>]+\\)'\\)"
       nil t)
      (let ((encoding (intern (downcase (or (match-string 2)
					    (match-string 3))))))
	(or
	 (shimbun-find-coding-system
	  (cdr (assq encoding shimbun-rss-compatible-encoding-alist)))
	 (shimbun-find-coding-system encoding)
	 (shimbun-find-coding-system
	  (car (rassq encoding shimbun-rss-compatible-encoding-alist)))))
    (shimbun-find-coding-system 'utf-8)))

(defun shimbun-rss-node-text (namespace local-name element)
  (let* ((node (assq (intern (concat namespace (symbol-name local-name)))
		     element))
	 (text (if (and node (listp node))
		   (shimbun-rss-node-just-text node)
		 node))
	 (cleaned-text (if text (shimbun-replace-in-string
				 text "^[ \000-\037\177]+\\|[ \000-\037\177]+$"
				 ""))))
    (if (string-equal "" cleaned-text)
	nil
      cleaned-text)))

(defun shimbun-rss-node-just-text (node)
  (if (and node (listp node))
      (mapconcat 'shimbun-rss-node-just-text (cddr node) " ")
    node))

(defun shimbun-rss-find-el (tag data &optional found-list)
  "Find the all matching elements in the data.
Careful with this on large documents!"
  (when (consp data)
    (dolist (bit data)
      (when (car-safe bit)
	(when (equal tag (car bit))
	  ;; Old xml.el may return a list of string.
	  (when (and (consp (caddr bit))
		     (stringp (caaddr bit)))
	    (setcar (cddr bit) (caaddr bit)))
	  (setq found-list (append found-list (list bit))))
	(if (and (consp (car-safe (caddr bit)))
		 (not (stringp (caddr bit))))
	    (setq found-list (append found-list
				     (shimbun-rss-find-el tag (caddr bit))))
	  (setq found-list (append found-list
				   (shimbun-rss-find-el tag (cddr bit))))))))
  found-list)

(defun shimbun-rss-get-namespace-prefix (el uri)
  "Given EL (containing a parsed element) and URI (containing a string
that gives the URI for which you want to retrieve the namespace
prefix), return the prefix.
See http://feeds.archive.org/validator/docs/howto/declare_namespaces.html
for more RSS namespaces."
  (let* ((prefix (car (rassoc uri (cadar el))))
	 (nslist (if prefix
		     (split-string (symbol-name prefix) ":")))
	 (ns (cond ((eq (length nslist) 1) ; no prefix given
		    "")
		   ((eq (length nslist) 2) ; extract prefix
		    (cadr nslist)))))
    (if (and ns (not (equal ns "")))
	(concat ns ":")
      ns)))

(provide 'sb-rss)

;; end of sb-rss.el
