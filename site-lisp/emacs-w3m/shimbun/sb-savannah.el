;;; sb-savannah.el --- shimbun backend for gnu list archives on savannah

;; Copyright (C) 2002, 2003, 2005 Yoichi NAKAYAMA <yoichi@FreeBSD.org>

;; Author: Yoichi NAKAYAMA <yoichi@FreeBSD.org>
;;         Katsumi Yamaoka <yamaoka@jpl.org>
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

;; First of all, if you have NNTP access to news.gmane.org, you can
;; read (and post) all those savannah groups.  It is far useful rather
;; than using this module. :-p

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-savannah (shimbun-mhonarc) ())

(defvar shimbun-savannah-url "http://lists.gnu.org/archive/html/")

(defvar shimbun-savannah-group-path-alist
  '(("bug-gnu-emacs" . "bug-gnu-emacs")
    ("emacs-bidi" . "emacs-bidi")
    ("emacs-commit" . "emacs-commit")
    ("emacs-devel" . "emacs-devel")
    ("emacs-diffs" . "emacs-diffs")
    ("emacs-pretest-bug" . "emacs-pretest-bug")
    ("gnu-emacs-sources" . "gnu-emacs-sources")
    ("help-emacs-windows" . "help-emacs-windows")
    ("help-gnu-emacs" . "help-gnu-emacs")
    ("info-gnu-emacs" . "info-gnu-emacs")
    ("tramp-devel" . "tramp-devel")
    ("vms-gnu-emacs" . "vms-gnu-emacs")))

(defvar shimbun-savannah-groups
  (mapcar 'car shimbun-savannah-group-path-alist))

(defvar shimbun-savannah-reverse-flag t)

(defvar shimbun-savannah-litemplate-regexp
  "<li><a name=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+\\.html\\)\">\
\\([^<]+\\)</a>, <i>\\([^<]+\\)</i>")

(defvar shimbun-savannah-x-face-alist
  '(("default" . "X-Face: =R@<a%O\"k\\jy?{Bk~[*wi<LU(\\;&[*N1\"5X4/^\
@oCB)?b0%$gKcCNJ)o'4GZ$?X$=E}Bj[k\n @#KE2J*~^\\[r_IQJ.m6`>L:wwfLNRT(\
ej<20'LI/le]z)n!%Bb(KI(@c&\"<`Ah~3&6Yn%+>-K>`@13\n T?OXgWz^><'44jgi;\
3T1{Sb~c|]lJ3WIZXP-tu8S4@.C=,:q#nF5qV$xkaQmSC5LZbF=U(AS_51T|K\n W7G")))

(defun shimbun-savannah-index-url (entity)
  (concat (shimbun-url-internal entity)
	  (cdr (assoc (shimbun-current-group-internal entity)
		      shimbun-savannah-group-path-alist))
	  "/"))

(luna-define-method shimbun-index-url ((shimbun shimbun-savannah))
  (shimbun-savannah-index-url shimbun))

(defun shimbun-savannah-get-headers (entity range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	(parent (shimbun-savannah-index-url entity))
	headers months url)
    (goto-char (point-min))
    (catch 'stop
      (while (and (or (not pages)
		      (>= (decf pages) 0))
		  (re-search-forward
		   "<a href=\"\\(20[0-9][0-9]-[01][0-9]/\\)index\\.html\">"
		   nil t))
	(push (match-string 1) months))
      (dolist (month (nreverse months))
	(setq url (concat parent month "index.html"))
	(shimbun-retrieve-url url t)
	(shimbun-mhonarc-get-headers entity url headers month)))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-savannah)
					 &optional range)
  (shimbun-savannah-get-headers shimbun range))

(provide 'sb-savannah)

;;; sb-savannah.el ends here
