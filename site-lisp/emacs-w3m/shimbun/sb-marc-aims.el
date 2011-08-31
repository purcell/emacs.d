;;; sb-marc-aims.el --- shimbun backend for marc.theaimsgroup.com.

;; Copyright (C) 2002, 2003, 2005 NOKUBI Takatsugu <knok@daionet.gr.jp>

;; Author: NOKUBI Takatsugu <knok@daionet.gr.jp>
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

(luna-define-class shimbun-marc-aims (shimbun) ())

(defvar shimbun-marc-aims-url "http://marc.theaimsgroup.com")

(defcustom shimbun-marc-aims-group-alist
  '(("fop-dev" "fop-dev" nil nil))
  "Table of mailing lists archives kept at http://marc.theaimsgroup.com/."
  :group 'shimbun
  :type '(repeat
	  (group :indent 0
		 (string :format "Group Name: %v\n" :size 0)
		 (string :format " List Name: %v\n" :size 0)
		 (radio :format "  Reply-To: %v"
			(const :format "None " nil)
			(string :format "Address: %v\n" :size 0))
		 (radio :format "    X-Face: %v"
			(const :format "None " nil)
			(string :format "%t: %v\n" :size 0)))))

(defvar shimbun-marc-aims-content-start "RAW</a>\\]</b>")
(defvar shimbun-marc-aims-content-end "<p>\\[<font ")

(luna-define-method shimbun-groups ((shimbun shimbun-marc-aims))
  (mapcar 'car shimbun-marc-aims-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-marc-aims))
  (concat shimbun-marc-aims-url
	  "/?l="
	  (nth 1 (assoc (shimbun-current-group-internal shimbun)
			shimbun-marc-aims-group-alist))))

(luna-define-method shimbun-reply-to ((shimbun shimbun-marc-aims))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-marc-aims-group-alist)))

(luna-define-method shimbun-x-face ((shimbun shimbun-marc-aims))
  (nth 3 (assoc (shimbun-current-group-internal shimbun)
		shimbun-marc-aims-group-alist)))

(luna-define-method shimbun-headers ((shimbun shimbun-marc-aims)
				     &optional range)
  (shimbun-marc-aims-headers shimbun range))

(defun shimbun-marc-aims-headers (shimbun &optional range)
  (cond
   ((eq range 'last) (setq range 1))
   ((eq range 'all) (setq range nil)))
  (let ((url (shimbun-index-url shimbun))
	(yearmonth)
	(headers)
	(case-fold-search t))
    (catch 'stop
      (with-temp-buffer
	(shimbun-retrieve-url url)
	(goto-char (point-max))
	(while (re-search-backward
		"b=\\([0-9][0-9][0-9][0-9][0-9][0-9]\\)&" nil t)
	  (push (match-string 1) yearmonth))
	(dolist (ym yearmonth)
	  (let ((surl (concat url "&r=1&b=" ym "&w=4")))
	    (while (when surl
		     (erase-buffer)
		     (shimbun-retrieve-url surl))
	      (and range
		   (< (setq range (1- range)) 0)
		   (throw 'stop nil))
	      (while (re-search-forward
		      "^ *[0-9]+\\. \\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\) +\\[\\(<font[^>]+>1</font>\\|<a href=\"\\?t=\\([0-9]+\\)&r=.&w=.&n=\\([0-9]+\\)\">[0-9]+</a>\\)\\] <a href=\"\\?l=[^&]+&m=\\([0-9]+\\)&w=.\">\\([^<]+\\)</a> +<a href=[^<]+</a> \\(.*\\)" nil t)
					; YYYY-MM-DD nil thread-id thread-wence message-id subject from
		(push (shimbun-make-header
		       0
		       (match-string 6)
		       (match-string 7)
		       (match-string 1)
		       (concat "<" (match-string 5) "@marc.theaimsgroup.com>")
		       nil
		       nil nil
		       (concat shimbun-marc-aims-url "/?m=" (match-string 5))
		       ) headers)
		(if
		    (not (eq (match-string 3) nil))
		    (let ((tid (match-string 3))
			  (tw (match-string 4))
			  (id (match-string 5)))
		      (with-temp-buffer
			(shimbun-retrieve-url (concat shimbun-marc-aims-url "/?t=" tid "&w=" tw))
			(while (re-search-forward
				"^ *[0-9]+\\. \\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\) +<a href=\"\\?l=[^&]+&m=\\([0-9]+\\)&w=.\">\\([^<]+\\)</a> +<a href=[^<]+</a> \\(.*\\)" nil t)
					; YYYY-MM-DD message-id subject from
			  (push (shimbun-make-header
				 0
				 (match-string 3)
				 (match-string 4)
				 (match-string 1)
				 (concat "<" (match-string 2) "@marc.theaimsgroup.com>")
				 (concat "<" id "@marc.theaimsgroup.com>")
				 nil nil
				 (concat shimbun-marc-aims-url "/?m=" (match-string 2))
				 ) headers))))))
	      (if (re-search-forward
		   "<a href=\"\\?l=[^&]+&r=\\([0-9]+\\)&b=[0-9]+&w=.\">Next" nil t)
		  (setq surl (concat url "&r=" (match-string 1) "&b=" ym "&w=4"))
		(setq surl nil)))))))
    headers))

(provide 'sb-marc-aims)

;;; sb-marc-aims.el ends here
