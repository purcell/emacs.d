;;; sb-redhat.el --- shimbun backend for sources.redhat.com mailing lists

;; Copyright (C) 2002, 2003 OHASHI Akira <bg66@koka-in.org>

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
(require 'sb-mhonarc)

(luna-define-class shimbun-redhat (shimbun-mhonarc) ())

(defvar shimbun-redhat-url "http://sources.redhat.com/ml/")
(defvar shimbun-redhat-groups
  '("automake" "bug-automake" "automake-prs" "automake-cvs" "binutils"
    "binutils-cvs" "c++-embedded" "crossgcc" "cgen" "cgen-prs" "cgen-cvs"
    "cygwin" "cygwin-xfree" "cygwin-announce" "cygwin-xfree-announce"
    "cygwin-apps" "cygwin-patches" "cygwin-developers" "cygwin-cvs"
    "cygwin-apps-cvs" "docbook-tools-discuss" "docbook-tools-announce"
    "docbook-tools-cvs" "docbook" "dssslist" "sgml-tools" "docbook-apps"
    "ecos-announce" "ecos-devel" "ecos-discuss" "ecos-maintainers"
    "ecos-patches" "elix" "elix-announce" "gdb" "gdb-announce" "gdb-testers"
    "gdb-testresults" "gdb-patches" "gdb-cvs" "bug-gdb" "gdb-prs" "libc-alpha"
    "libc-hacker" "bug-glibc" "glibc-cvs" "glibc-linux" "bug-gnats"
    "gnats-devel" "gnats-announce" "gnats-cvs" "gsl-discuss" "gsl-announce"
    "gsl-cvs" "guile" "guile-emacs" "guile-prs" "guile-gtk" "bug-guile"
    "guile-cvs" "guile-emacs-cvs" "insight" "insight-announce" "insight-prs"
    "installshell" "inti" "kawa" "libffi-discuss" "libffi-announce"
    "libstdc++" "libstdc++-cvs" "libstdc++-prs" "mauve-discuss"
    "mauve-announce" "newlib" "pthreads-win32" "rhdb" "rhdb-announce"
    "rhug-rhats" "rpm2html-cvs" "rpm2html-prs" "rpm2html" "sid" "sid-announce"
    "sid-cvs" "sourcenav" "sourcenav-announce" "sourcenav-prs" "win32-x11"
    "xconq7" "xconq-announce" "xconq-cvs"))
(defvar shimbun-redhat-coding-system 'iso-8859-1)
(defvar shimbun-redhat-reverse-flag t)
(defvar shimbun-redhat-litemplate-regexp
  "<td align=\"left\"><b><a name=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+\.html\\)\">\\([^<]+\\)</a></b></td>\n<td align=\"right\">\\([^<]+\\)</td>")
(defvar shimbun-redhat-litemplate-regexp-old
  " </tt><b><a name=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+\.html\\)\">\\([^<]+\\)</a></b><tt> </tt>\\(.+\\)")

(luna-define-method shimbun-index-url ((shimbun shimbun-redhat))
  (concat (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun) "/"))
(luna-define-method shimbun-reply-to ((shimbun shimbun-redhat))
  (concat (shimbun-current-group-internal shimbun) "@redhat.com"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-redhat)
					 &optional range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers months)
    (goto-char (point-min))
    (catch 'stop
      (while (and (if pages (<= (incf count) pages) t)
		  (re-search-forward "   <li><a href=\"\\([0-9]+\\(-[0-9q][0-9]\\)?\\)/\">" nil t)
		  (push (match-string 1) months)))
      (setq months (nreverse months))
      (dolist (month months)
	(let ((url (concat (shimbun-index-url shimbun) month "/")))
	  (when (= (string-match "[0-9]+\\(-q[0-9]\\)?$" month) 0)
	    (shimbun-mhonarc-set-litemplate-regexp-internal
	     shimbun shimbun-redhat-litemplate-regexp-old))
	  (shimbun-retrieve-url url t)
	  (shimbun-mhonarc-get-headers shimbun url headers month))))
    headers))

(provide 'sb-redhat)

;;; sb-redhat.el ends here
