;;; sb-mailarc.el --- shimbun backend class for mailarc

;; Copyright (C) 2002, 2003  Free Software Foundation, Inc.

;; Author: ARISAWA Akihiro <ari@mbf.sphere.ne.jp>
;; Keywords: news

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'shimbun)
(luna-define-class shimbun-mailarc (shimbun))

(luna-define-method shimbun-get-headers ((shimbun shimbun-mailarc)
					 &optional range)
  (let (headers)
    (catch 'stop
      (goto-char (point-min))
      (while (re-search-forward
	      "<li>\\([0-9]+\\) <strong><a href=['\"]\\([^'\"]+\\)['\"]>\\([^<]*\\)</a>\n</strong>\n<em>\\([^<]*\\)</em>\n</li>\n"
	      nil t)
	(let ((id (format "<%s%%%s>" (match-string 1)
			  (shimbun-current-group-internal shimbun)))
	      (url (match-string 2))
	      (subject (match-string 3))
	      (from (match-string 4)))
	  (when (shimbun-search-id shimbun id)
	    (throw 'stop headers))
	  (push (shimbun-make-header 0
				     (shimbun-mime-encode-string subject)
				     (shimbun-mime-encode-string from)
				     "" id "" 0 0 url)
		headers))))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun-mailarc) header)
  (let (body-end header-start body-start)
    (goto-char (point-min))
    (when (and (setq header-start (search-forward "<hr>\n" nil t))
	       (setq body-start (search-forward "<hr>\n" nil t))
	       (search-forward "<hr>\n" nil t))
      (set-marker (setq body-end (make-marker)) (match-beginning 0))
      ;; parse headers
      (save-restriction
	(narrow-to-region header-start body-start)
	(goto-char (point-min))
	(while (re-search-forward
		"<li><strong>\\([^:]+\\):</strong>\n\\([^<]*\\)</li>\n"
		body-start t)
	  (let ((field (match-string 1))
		(value (shimbun-mime-encode-string (match-string 2))))
	    (cond ((string= field "Subject")
		   (shimbun-header-set-subject header value))
		  ((string= field "Date")
		   (shimbun-header-set-date header value))
		  ((string= field "From")
		   (shimbun-header-set-from header value))))))
      (delete-region (point-min) body-start)
      (delete-region (marker-position body-end) (point-max))
      (set-marker body-end nil)
      (goto-char (point-min))
      (insert "<html>\n<head>\n<base href=\""
	      (shimbun-header-xref header) "\">\n</head>\n</body>\n")
      (goto-char (point-max))
      (insert "\n</body>\n</html>"))
    (shimbun-make-mime-article shimbun header)
    (buffer-string)))

(provide 'sb-mailarc)
;;; sb-mailarc.el ends here
