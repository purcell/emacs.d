;;; sb-debian.el --- shimbun backend for debian.org

;; Copyright (C) 2001, 2002, 2003, 2005, 2007 OHASHI Akira <bg66@koka-in.org>

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

(luna-define-class shimbun-debian (shimbun-mhonarc) ())

(defvar shimbun-debian-url "http://lists.debian.org/")
(defvar shimbun-debian-groups
  '(
    ;; Users
    "debian-announce" "debian-commercial" "debian-firewall" "debian-french"
    "debian-isp" "debian-italian" "debian-kde" "debian-laptop" "debian-news"
    "debian-news-german" "debian-news-portuguese" "debian-security-announce"
    "debian-testing" "debian-user" "debian-user-catalan" "debian-user-de"
    "debian-user-french" "debian-user-german" "debian-user-polish"
    "debian-user-portuguese" "debian-user-spanish" "debian-user-swedish"
    ;; Developers
    "debian-admintool" "debian-apache" "debian-autobuild" "debian-beowulf"
    "debian-boot" "debian-cd" "debian-ctte" "debian-debbugs" "debian-devel"
    "debian-devel-announce" "debian-devel-french" "debian-devel-games"
    "debian-devel-spanish" "debian-doc" "debian-dpkg" "debian-emacsen"
    "debian-events-eu" "debian-events-na" "debian-faq" "debian-gcc"
    "debian-glibc" "debian-gtk-gnome" "debian-hams" "debian-ipv6"
    "debian-java" "debian-jr" "debian-med" "debian-mentors" "debian-newmaint"
    "debian-newmaint-admin" "debian-newmaint-discuss" "debian-ocaml-maint"
    "debian-openoffice" "debian-perl" "debian-pilot" "debian-policy"
    "debian-pool" "debian-python" "debian-qa" "debian-qa-packages"
    "debian-qa-private" "debian-release" "debian-security" "debian-snapshots"
    "debian-ssh" "debian-tetex-maint" "debian-toolchain" "debian-vote"
    "debian-wnpp" "debian-www" "debian-x" "deity"
    ;; Internationalization and Translations
    "debian-chinese" "debian-chinese-big5" "debian-chinese-gb"
    "debian-esperanto" "debian-i18n" "debian-japanese" "debian-l10n-catalan"
    "debian-l10n-dutch" "debian-l10n-english" "debian-l10n-french"
    "debian-l10n-italian" "debian-l10n-portuguese" "debian-l10n-spanish"
    "debian-laespiral" "debian-russian" "debian-simplified-chinese"
    ;; Ports
    "debian-68k" "debian-alpha" "debian-arm" "debian-bsd" "debian-hppa"
    "debian-hurd" "debian-ia64" "debian-mips" "debian-parisc" "debian-powerpc"
    "debian-s390" "debian-sparc" "debian-superh" "debian-ultralinux"
    "debian-win32"
    ;; Miscellaneous Debian
    "debian-all-changes" "debian-alpha-changes" "debian-arm-changes"
    "debian-books" "debian-cd-vendors" "debian-changes" "debian-consultants"
    "debian-curiosa" "debian-devel-all-changes" "debian-devel-alpha-changes"
    "debian-devel-arm-changes" "debian-devel-changes"
    "debian-devel-hurd-i386-changes" "debian-devel-i386-changes"
    "debian-devel-m68k-changes" "debian-devel-powerpc-changes"
    "debian-devel-sparc-changes" "debian-hurd-i386-changes"
    "debian-i386-changes" "debian-legal" "debian-m68k-changes"
    "debian-mirrors" "debian-powerpc-changes" "debian-project"
    "debian-publicity" "debian-sgml" "debian-sparc-changes"
    ;; Linux Standard Base
    "lcs-eng" "lsb-confcall" "lsb-discuss" "lsb-impl" "lsb-spec" "lsb-test"
    ;; Software in the Public Interest
    "spi-announce" "spi-general"
    ;; Other
    "vgui-discuss"
    ))
(defvar shimbun-debian-coding-system 'iso-8859-1)
(defvar shimbun-debian-fetch-headers-max 1000)
(defvar shimbun-debian-reverse-flag nil)
(defvar shimbun-debian-litemplate-regexp
  "<strong><A NAME=\"\\([0-9]+\\)\" HREF=\"\\(msg[0-9]+.html\\)\">\\([^<]+\\)</A></strong> <em>\\([^<]+\\)</em>")
(defvar shimbun-debian-x-face-alist
  '(("default" . "X-Face: ]SX>@::/@(;bIJSLp?tu'vm&{Q=(T1L_wI)+bH6EY$^PkY|:Fa4VBhLG#EtcZ.#F==O~-vk
 !A2|wMxaLC|=iA#V$[r(C..3&<fJ-B|E2&SKUivW[C%BXG8AGcfZ5YN8W`r")))

(luna-define-method shimbun-index-url ((shimbun shimbun-debian))
  (concat (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun) "/"))

(luna-define-method shimbun-reply-to ((shimbun shimbun-debian))
  (concat (shimbun-current-group-internal shimbun) "@debian.org"))

(defmacro shimbun-debian-get-headers (shimbun url headers &optional aux)
  `(let ((case-fold-search t)
	 (regexp (or (shimbun-mhonarc-litemplate-regexp-internal ,shimbun)
		     shimbun-mhonarc-litemplate-regexp))
	 (year (and (string-match (concat (regexp-quote (shimbun-index-url
							 shimbun))
					  "\\([0-9]+\\)")
				  url)
		    (match-string 1 url)))
	 month day date-start date-end)
     (goto-char (point-max))
     (while (and (setq date-end (re-search-backward "^</ul>" nil t))
		 (setq date-start (re-search-backward "^<ul>" nil t)))
       (forward-line -1)
       (looking-at "^\\(\\w+\\) \\([0-9]+\\)")
       (setq month (match-string 1)
	     day (match-string 2))
       (goto-char date-end)
       (while (re-search-backward regexp date-start t)
	 (shimbun-mhonarc-extract-header-values ,shimbun ,url ,headers ,aux)
	 (shimbun-header-set-date (car ,headers)
				  (format "%s %s %s 00:00 +0000"
					  day month year))
	 (if (> (length ,headers) shimbun-debian-fetch-headers-max)
	     (throw 'stop ,headers))
	 (forward-line 0)))
     ,headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-debian)
					 &optional range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	;;(group (shimbun-current-group-internal shimbun))
	(count 0)
	headers paths)
    (goto-char (point-max))
    (while (and (if pages (<= (incf count) pages) t)
		(re-search-backward
		 (concat "<a href=\"\\([^\"]+\\)/threads.html\">") nil t))
      (push (match-string 1) paths))
    (catch 'stop
      (dolist (path (nreverse paths))
	(let* ((base-url (concat (shimbun-index-url shimbun)
				 path "/mail%s.html"))
	       url)
	  (shimbun-retrieve-url (format base-url "list") t)
	  (re-search-forward
	   "<td\\( align=\"center\"><b\\)?>Page [0-9]+ of \\([0-9]+\\)\\(</b><br>\\)?</td>")
	  (let* ((page-max (string-to-number (match-string 2)))
		 (i page-max))
	    (while (> i 1)
	      (setq url (format base-url i))
	      (shimbun-retrieve-url url t)
	      (shimbun-debian-get-headers shimbun url headers path)
	      (setq i (1- i)))
	    (setq url (format base-url "list"))
	    (shimbun-retrieve-url url)
	    (shimbun-debian-get-headers shimbun url headers path)))))
    headers))

(provide 'sb-debian)

;;; sb-debian.el ends here
