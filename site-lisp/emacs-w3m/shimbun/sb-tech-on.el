;;; sb-tech-on.el --- shimbun backend for Tech-On! -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2007, 2009 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
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

;;; Commentary:

;;; Code:

(eval-when-compile (require 'static))
(require 'sb-rss)
(require 'sb-multi)

(luna-define-class shimbun-tech-on (shimbun-multi shimbun-rss) ())

(defvar shimbun-tech-on-user-name 'none
  "*User name to log in on Tech-On! with.
If it is nil, you will be prompted for a user name when logging in on
Tech-On! with.  If it is a string, it will be used as a user name and
you will never be prompted.  If it is neither nil nor a string, you
will never log in.  See also `shimbun-tech-on-password'.")

(defvar shimbun-tech-on-password 'none
  "*Password to use to log in on Tech-On! with.
If it is nil, you will be prompted for a password when logging in on
Tech-On! with.  If it is a string, it will be used as a password and
you will never be prompted.  If it is neither nil nor a string, you
will never log in.  See also `shimbun-tech-on-user-name'.")

(defvar shimbun-tech-on-url "http://techon.nikkeibp.co.jp/")

(defvar shimbun-tech-on-group-table
  '(("latestnews" "Tech-On！" "/rss/index.rdf")
    ("mobile" "モバイル" "/mobile/index.rdf")
    ("bbint" "通信" "/bbint/index.rdf")
    ("d-ce" "デジタル家電" "/d-ce/index.rdf")
    ("AT" "Automotive Technology" "/AT/index.rdf")
    ("edaonline" "EDA Online" "/edaonline/index.rdf")
    ("device" "電子部品テクノロジ" "/device/index.rdf")
    ("lsi" "LSI情報局" "/lsi/index.rdf")
    ("silicon" "Silicon Online" "/silicon/index.rdf")
    ("observer" "産業動向オブザーバ" "/observer/index.rdf")
    ("fpd" "FPD International" "/fpd/index.rdf")
    ("mono" "ものづくりとIT" "/mono/index.rdf")
    ("embedded" "組み込み開発" "/embedded/index.rdf")
    ("mecha" "機械・メカトロニクス" "/mecha/index.rdf")
    ("MEMS" "MEMS International" "/MEMS/index.rdf")
    ("nano" "ナノテク･新素材" "/nano/index.rdf")
    ("carele" "カーエレクトロニクス" "/carele/index.rdf")
    ("board" "日経ボード情報" "/board/index.rdf")
    ("mcu" "マイコン" "/mcu/index.rdf")
    ("PLM" "PLM" "/PLM/index.rdf")
    ("memory" "メモリ" "/memory/index.rdf")
    ("measurement" "計測" "/measurement/index.rdf")
    ("column.mot" "技術経営戦略考" "/column/mot/index.rdf")))

(defvar shimbun-tech-on-server-name "Tech-On!")

(defvar shimbun-tech-on-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAACAAAAAgAgMAAAAOFJJnAAAADFBMVEUAAAB/gP+ttr7///8
 c6BRHAAAAnUlEQVQY02XNPQpCMQwA4NBs9jDvCJ5CXEVv4dJQLyKuHbyCl3i4Cl3EsSA8+l6NoU0
 HMVk+8gsEa2b2DP94rs7DYyCExZIlJCMw6NF7AaI5VZgOQMOtEhQYTOjDXuH7FrU7ZG9W8LlOkuE
 FrPGD0TFnQdlsmSfB240KyYo7F9dxtIrdRbAAln1SHJK2GmQ9ptwOxsTtRawteTrn6QtRz6k/Cwl
 XeQAAAABJRU5ErkJggg==")))

(defvar shimbun-tech-on-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-tech-on))
  (mapcar 'car shimbun-tech-on-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-tech-on))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-tech-on-group-table)))

(luna-define-method shimbun-from-address ((shimbun shimbun-tech-on))
  (concat shimbun-tech-on-server-name
	  " (" (shimbun-current-group-name shimbun) ")"))

(luna-define-method shimbun-index-url ((shimbun shimbun-tech-on))
  (shimbun-expand-url (nth 2 (assoc (shimbun-current-group-internal shimbun)
				    shimbun-tech-on-group-table))
		      shimbun-tech-on-url))

(luna-define-method shimbun-rss-build-message-id ((shimbun shimbun-tech-on)
						  url date)
  (let ((start 0)
	rest)
    (while (string-match "[0-9]+" url start)
      (push (match-string 0 url) rest)
      (setq start (match-end 0)))
    (if rest
	(concat "<" (mapconcat 'identity (nreverse rest) ".")
		"." (shimbun-current-group-internal shimbun)
		"%techon.nikkeibp.co.jp>")
      (error "Cannot find message-id base"))))

(defvar shimbun-tech-on-logged-in nil)

(defun shimbun-tech-on-login ()
  "Log in on Tech-On! with."
  (interactive)
  (when (or (interactive-p)
	    (not shimbun-tech-on-logged-in))
    (let ((user (cond ((stringp shimbun-tech-on-user-name)
		       shimbun-tech-on-user-name)
		      (shimbun-tech-on-user-name
		       nil)
		      (t
		       (condition-case nil
			   (let (inhibit-quit)
			     (read-string "[Tech-On!] User name: "))
			 (quit nil)))))
	  pass)
      (when (and user
		 (not (string-match "\\`[\t ]*\\'" user))
		 (setq pass (cond ((stringp shimbun-tech-on-password)
				   shimbun-tech-on-password)
				  (shimbun-tech-on-password
				   nil)
				  (t
				   (condition-case nil
				       (let (inhibit-quit)
					 (read-passwd "[Tech-On!] Password: "))
				     (quit nil)))))
		 (not (string-match "\\`[\t ]*\\'" pass)))
	(with-temp-buffer
	  (static-unless (featurep 'xemacs)
	    (set-buffer-multibyte t))
	  (shimbun-retrieve-url
	   (concat "https://techon.nikkeibp.co.jp/login/login.jsp"
		   "?MODE=LOGIN_EXEC"
		   "&USERID=" user
		   "&PASSWORD=" pass)
	   t)
	  (goto-char (point-min))
	  (setq shimbun-tech-on-logged-in
		(not (re-search-forward "\
\\(?:ユーザー名\\|パスワード\\).*に誤りがあります。\
\\|会員登録が行われていません。\
\\|ACTION=\"/login/login\\.jsp\\?MODE=LOGIN_EXEC\""
					nil t))))
	(if shimbun-tech-on-logged-in
	    (when (interactive-p)
	      (message "[Tech-On!] Logged in"))
	  (when (prog2
		    (message nil)
		    (y-or-n-p "[Tech-On!] Login failed; retry? ")
		  (message nil))
	    (setq shimbun-tech-on-user-name nil
		  shimbun-tech-on-password nil)
	    (shimbun-tech-on-login)))))))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-tech-on)
					    header url)
  (goto-char (point-min))
  (when (re-search-forward "[\t\n ]*\\(?:（[\t\n ]*\\)*<a[\t\n ]+\
\\(?:[^\t\n >]+[\t\n ]+\\)*href=\"\\([^\"]+\\)\"\
\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]*>[\t\n ]*次の?ページへ[^<]*</a>"
		   nil t)
    (shimbun-expand-url (match-string 1) url)))

(luna-define-method shimbun-multi-clear-contents :around ((shimbun
							   shimbun-tech-on)
							  header
							  has-previous-page
							  has-next-page)
  (when (luna-call-next-method)
    ;; Insert page delimiter.
    (when has-previous-page
      (goto-char (point-min))
      (insert "&#012;\n")
      ;; Remove tags that likely cause a newline preceding a page.
      (when (and (looking-at "[\t\n ]*<\\(h[0-9]+\\|p\\)[\t\n >]")
		 (shimbun-end-of-tag (match-string 1) t))
	(replace-match "\n\\3\n")))
    t))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-tech-on)
						    header)
  (let ((author (when (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"author\"" nil t)
			   (shimbun-end-of-tag "div" t))
		  (match-string 2))))
    (goto-char (point-min))
    (when (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*id=\"kiji\"" nil t)
	       (shimbun-end-of-tag "div" t))
      (delete-region (match-end 1) (point-max))
      (insert "\n")
      (delete-region (point-min) (match-beginning 1))
      ;; Remove repeated <p>s.
      (goto-char (point-min))
      (while (re-search-forward "<p>\\([\t\n ]*<p>\\)+" nil t)
	(delete-region (match-beginning 1) (match-end 0)))
      ;; Remove useless tags.
      (goto-char (point-min))
      (while (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"bpimage_click\"" nil t)
		  (shimbun-end-of-tag "div" t))
	(replace-match "\n"))
      (when author
	(goto-char (point-min))
	(insert "<p>" author "</p>\n"))
      t)))

(luna-define-method shimbun-footer :around ((shimbun shimbun-tech-on)
					    header &optional html)
  (concat "<div align=\"left\">\n--&nbsp;<br>\n\
この記事の著作権は日経BP社、またはその情報提供者に帰属します。\
原物は<a href=\""
	  (shimbun-article-base-url shimbun header)
	  "\"><u>ここ</u></a>で公開されています。\n</div>\n"))

(luna-define-method shimbun-article :before ((shimbun shimbun-tech-on)
					     &rest args)
  (shimbun-tech-on-login))

(luna-define-method shimbun-close :after ((shimbun shimbun-tech-on))
  (setq shimbun-tech-on-logged-in nil))

(provide 'sb-tech-on)

;;; sb-tech-on.el ends here
