;;; sb-cgi-board.el --- Shimbun backend for CGI_Board bulletin board systems

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: shimbun

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

;; This is a shimbun backend to browse CGI_Board bulletin board
;; systems, developed by KUROKI Gen <kuroki@math.tohoku.ac.jp>.

;;; Code:

(require 'shimbun)
(eval-when-compile
  (require 'cl)
  ;; `multiple-value-bind' requires the 2nd argument to be multiple-value,
  ;; not a list, in particular for XEmacs 21.5.  `values-list' does it,
  ;; but is a run-time cl function in XEmacs 21.4 and Emacs 21.
  (when (eq 'identity (symbol-function 'values-list))
    (define-compiler-macro values-list (arg)
      arg)))

(defcustom shimbun-cgi-board-group-alist
  '(("support" .
     "http://www.math.tohoku.ac.jp/~kuroki/support/BBS.cgi?b=cgi_board")
    ("kuroki.a" .
     "http://www.math.tohoku.ac.jp/~kuroki/keijiban/BBS.cgi?b=a")
    ("kuroki.b" .
     "http://www.math.tohoku.ac.jp/~kuroki/keijiban/BBS.cgi?b=b")
    ("kuroki.c" .
     "http://www.math.tohoku.ac.jp/~kuroki/keijiban/BBS.cgi?b=c")
    ("kuroki.e" .
     "http://www.math.tohoku.ac.jp/~kuroki/keijiban/BBS.cgi?b=e")
    ("nojiri" .
     "http://njb.virtualave.net/BBS.cgi?b=nmain")
    ("yamagata" .
     "http://ruitomo.com/~hiroo/bbs/BBS.cgi?b=kohobu"))
  "*An alist of CGI_Board bulletin board systems and their URLs."
  :group 'shimbun
  :type '(repeat
	  (cons :format "%v" :indent 4
		(string :tag "Name")
		(string :tag "      URL"))))

(luna-define-class shimbun-cgi-board (shimbun) ())

(luna-define-method shimbun-groups ((shimbun shimbun-cgi-board))
  (mapcar 'car shimbun-cgi-board-group-alist))

(defsubst shimbun-cgi-board-base-url (shimbun)
  (cdr (assoc (shimbun-current-group-internal shimbun)
	      shimbun-cgi-board-group-alist)))

(luna-define-method shimbun-index-url ((shimbun shimbun-cgi-board))
  (concat (shimbun-cgi-board-base-url shimbun) "&old"))

(luna-define-method shimbun-x-face ((shimbun shimbun-cgi-board))
  nil)

(luna-define-method shimbun-get-headers ((shimbun shimbun-cgi-board)
					 &optional range)
  (catch 'found
    (let ((base (shimbun-cgi-board-base-url shimbun))
	  (no-cache t)
	  (headers))
      (dolist (page (shimbun-cgi-board-get-pages range))
	(let (buffer header)
	  (unwind-protect
	      (with-temp-buffer
		(when (shimbun-fetch-url shimbun
					 (shimbun-expand-url page base)
					 no-cache)
		  (goto-char (point-min))
		  (while (re-search-forward
			  "\n<!--\\([^: \t\r\f\n]+\\):--><hr noshade>\n" nil t)
		    (let* ((fragment (match-string 1))
			   (id (shimbun-cgi-board-make-message-id base
								  fragment)))
		      (when (shimbun-search-id shimbun id)
			(throw 'found headers))
		      (unless buffer
			(with-current-buffer
			    (setq buffer (generate-new-buffer " *temp*"))
			  (shimbun-fetch-url shimbun
					     (concat base "&thread&_f=" page))))
		      (when (setq header
				  (with-current-buffer buffer
				    (shimbun-cgi-board-extract-header base
								      fragment)))
			(push header headers))))))
	    (when buffer
	      (kill-buffer buffer))))
	(setq no-cache nil))
      headers)))

(defconst shimbun-cgi-board-thread-regexp "\\( *\\)\\[\\([^]]+\\)\\] *\
<a name=\"\\([^\"]+\\)\" href=\"\\([^\"]+\\)\" target=\"article\">\\([^<]*\\)\
</a> *<small>(\\(.+\\))</small>")

(defun shimbun-cgi-board-extract-header (base fragment)
  (let (header)
    (goto-char (point-min))
    (while (and (not header) (search-forward fragment nil t))
      (forward-line 0)
      (if (and (looking-at shimbun-cgi-board-thread-regexp)
	       (equal fragment (match-string 3)))
	  (let ((level (length (match-string 1)))
		(url (shimbun-expand-url (match-string 4) base)))
	    (setq header
		  (shimbun-create-header
		   0
		   (let ((subject (match-string 5)))
		     (if (equal subject fragment) "" subject))
		   (match-string 2)
		   (shimbun-cgi-board-make-date-string (match-string 6))
		   (shimbun-cgi-board-make-message-id base (match-string 3))
		   nil nil nil url))
	    (when (> level 0)
	      ;; Search a parent article.
	      (while (and (not (shimbun-header-references header))
			  (zerop (forward-line 1))
			  (not (looking-at "^$")))
		(when (and (looking-at shimbun-cgi-board-thread-regexp)
			   (< (length (match-string 1)) level))
		  (shimbun-header-set-references
		   header
		   (shimbun-cgi-board-make-message-id base
						      (match-string 3)))))))
	(forward-line 1)))
    header))

(defun shimbun-cgi-board-make-date-string (string)
  (save-match-data
    (if (string-match
	 "\\`\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) \\([0-9:]+\\)\\'" string)
	(shimbun-make-date-string (string-to-number (match-string 1 string))
				  (string-to-number (match-string 2 string))
				  (string-to-number (match-string 3 string))
				  (match-string 4 string))
      (multiple-value-bind (sec min hour day month year dow dst zone)
	  (values-list (decode-time (shimbun-time-parse-string string)))
	(setq zone (/ zone 60))
	(shimbun-make-date-string year month day
				  (format "%02d:%02d" hour min)
				  (format "%s%02d%02d"
					  (if (>= zone 0) "+" "-")
					  (/ zone 60)
					  (% zone 60)))))))

(defun shimbun-cgi-board-get-pages (&optional range)
  "Return a list of splited index pages."
  (let ((pages)
	(count 0)
	(limit (shimbun-header-index-pages range)))
    (goto-char (point-min))
    (while (and (or (not limit) (<= (incf count) limit))
		(re-search-forward
		 "<a href=\"\\./\\([^.]+\\.html\\)\" target=\"article\">"
		 nil t))
      (push (match-string 1) pages))
    (nreverse pages)))

(defun shimbun-cgi-board-make-message-id (url &optional fragment)
  (save-match-data
    (format "<%s@%s>"
	    (or fragment
		(progn
		  (string-match "\\`[^#]*#" url)
		  (substring url (match-end 0))))
	    (progn
	      (string-match "\\`[^:/#?]+://\\([^/#?]+\\)/" url)
	      (match-string 1 url)))))

(luna-define-method shimbun-clear-contents ((shimbun shimbun-cgi-board) header)
  (let ((id (shimbun-header-id header)))
    (when (string-match "\\`<\\([^@]+\\)@" id)
      (goto-char (point-min))
      (let (start)
	(when (and (search-forward
		    (concat "\n<!--" (match-string 1 id) ":-->") nil t)
		   (setq start (match-end 0))
		   (re-search-forward "<!--[^-]*-->\n" nil t))
	  (delete-region (match-beginning 0) (point-max))
	  (delete-region (point-min) start)
	  (goto-char (point-min))
	  (when (looking-at "<hr[^>]*>")
	    (delete-region (match-beginning 0) (match-end 0)))
	  t)))))

(provide 'sb-cgi-board)

;;; sb-cgi-board.el ends here
