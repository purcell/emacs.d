;;; sb-wikimedia.el --- shimbun backend for Wikimedia Mailing list -*- coding: utf-8; -*-

;; Copyright (C) 2004, 2005, 2007 Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>

;; Author: Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>
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

;;; See also
;;; URL <http://mail.wikipedia.org/>

;;; Code:

(require 'shimbun)
(require 'sb-mailman)

(luna-define-class shimbun-wikimedia (shimbun-mailman) ())

(defvar shimbun-wikimedia-url "http://mail.wikipedia.org/pipermail/")

(defvar shimbun-wikimedia-group-path-alist
  ;; entry path mailaccount date-format
  '(;; Wikimedia Foundation
    ("wikimedia.foundation" "foundation-l" "foundation-l" "en") ;; Foundation ML
    ("wikimedia.wikitech" "wikitech-l" "wikitech-l" "en") ;; Wikitech ML
    ("wikimedia.mediawiki-cvs" "mediawiki-cvs" "mediawiki-cvs" "en")
    ;;  wikibugs-l@Wikipedia.org. redirect sourceforge.org proj ML
    ("wikimedia.vereinde" "vereinde-l" "vereinde-l" "de") ;; in-progress German Wikimedia organisation discussion
    ("wikimedia.commons" "commons-l" "commons-l" "en") ;; discussion list for the Wikimedia Commons <http://commons.wikimedia.org/>
    ("wikimedia.translators" "translators-l" "translators-l" "en") ;; a list for discussing and announcing translations across the Wikimedia projects

    ;; Project mailing lists
    ("project.wikibooks" "textbook-l" "textbook-l" "en") ;; Wikibooks
    ("project.wiktionary" "wiktionary-l" "wiktionary-l" "en") ;; Wiktionary
    ("project.wikipedia" "wikipedia-l" "wikipedia-l" "en") ;; Wikipedia global discuss
    ("project.wikiversityde" "wikiversityde-l" "wikiversityde-l" "en")
    ("project.wikispecies" "wikispecies-l" "wikispecies-l" "en")
    ("project.daily-article" "daily-article-l" "daily-article-l" "en") ;; Daily article?

    ;; Local Wikipedia mailing lists
    ;; Wikipedia
    ("wikipedia.ar" "wikiar-l" "wikiar-l" "en")
    ("wikipedia.da" "wikida-l" "wikida-l" "en")
    ("wikipedia.de" "wikide-l" "wikide-l" "de")
    ("wikipedia.en" "wikien-l" "wikien-l" "en")
    ("wikipedia.eo" "wikieo-l" "wikieo-l" "en")
    ("wikipedia.es" "wikies-l" "wikies-l" "es")
    ;; WikiFI-l non archived
    ("wikipedia.fr" "wikifr-l" "wikifr-l" "fr")
    ("wikipedia.hi" "wikihi-l" "wikihi-l" "en")
    ("wikipedia.ia" "wikiia-l" "wikiia-l" "ia")
    ("wikipedia.is" "wikiis-l" "wikiis-l" "is")
    ("wikipedia.it" "wikiit-l" "wikiit-l" "it")
    ("wikipedia.ja" "wikija-l" "wikija-l" "ja")
    ;;  WikiNL-l. (The current archive is only available to the list members.)
    ("wikipedia.no" "wikino-l" "wikino-l" "no")
    ("wikipedia.pl" "wikipl-l" "wikipl-l" "pl")
    ;; WikiSV-l Archives. (The current archive is only available to the list members.)
    ;; Wikita-l Archives. non archived

    ;; Wiktionary
    ("wiktionary.de" "wiktionaryde-l" "wiktionaryde-l" "de")
    ))

(defvar shimbun-wikimedia-groups (mapcar 'car shimbun-wikimedia-group-path-alist))

(defmacro shimbun-wikimedia-concat-url (shimbun url)
  `(concat (shimbun-url-internal ,shimbun)
	   (nth 1 (assoc (shimbun-current-group-internal ,shimbun)
			 shimbun-wikimedia-group-path-alist))
	   "/"
	   ,url))

(luna-define-method shimbun-index-url ((shimbun shimbun-wikimedia))
  (shimbun-expand-url
   (shimbun-wikimedia-concat-url shimbun "")))

(luna-define-method shimbun-reply-to ((shimbun shimbun-wikimedia))
  (concat   (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-wikimedia-group-path-alist))
	    "@wikipedia.org"))

(defun shimbun-wikimedia-l10n-make-contents (shimbun header)
  (subst-char-in-region (point-min) (point-max) ?\t ?\  t)
  (shimbun-decode-entities)
  (goto-char (point-min))
  (let* ((case-fold-search t)
	 (end (search-forward "<!--beginarticle-->"))
	 name address date)
    (goto-char (point-min))
    (search-forward "</HEAD>")
    (when (re-search-forward "<H1>\\([^\n]+\\)\\(\n +\\)?</H1>" end t nil)
      (shimbun-header-set-subject
       header
       (shimbun-mime-encode-string (match-string 1))))
    (when (re-search-forward "<B>\\([^\n]+\\)\\(\n +\\)?</B> *\n +\
<A HREF=\"[^\n]+\n +TITLE=\"[^\n]+\">\\([^\n]+\\)"
			     end t nil)
      (setq name (match-string 1)
	    address (match-string 3))
      ;; Yoshiki.Ohshima ＠ acm.org
      (when (string-match " \\(＠\\|at\\|w\\|a\\|en\\) " name)
	(setq name (concat (substring name 0 (match-beginning 0))
			   "@"
			   (substring name (match-end 0)))))
      (when (string-match " \\(＠\\|at\\|w\\|a\\|en\\) " address)
	(setq address (concat (substring address 0 (match-beginning 0))
			      "@"
			      (substring address (match-end 0)))))
      (shimbun-header-set-from
       header
       (shimbun-mime-encode-string (concat name " <" address ">")))
      (when (re-search-forward "<I>\\([^<]*\\)</I>" end t nil)
	(let (date-string func)
	  (setq func (shimbun-wikimedia-date-function-select shimbun))
	  (setq date-string (match-string-no-properties 1))
	  (setq date (funcall func date-string))
	  (unless (eq "" date)
	    (shimbun-header-set-date header date))))
      (delete-region (point-min) end)
      (delete-region (search-forward "<!--endarticle-->") (point-max))
      (shimbun-header-insert-and-buffer-string shimbun header nil t))))

(luna-define-method shimbun-make-contents ((shimbun shimbun-wikimedia) header)
  (shimbun-wikimedia-l10n-make-contents shimbun header))

(defun shimbun-wikimedia-date-function-select (shimbun)
  (let (lang func)
    (setq lang (nth 3 (assoc (shimbun-current-group-internal shimbun)  shimbun-wikimedia-group-path-alist)))
    (setq func (intern (concat "shimbun-wikimedia-" lang "-date-decode")))
    func))

;; internal date decode functions

;; latin format pattern
;; 1.en: Fri Nov 19 19:38:56 UTC 2004
;; 2a.pl: Czw, 1 Sty 2004, 12:17:45 UTC
;; 2b.ia: Dom 11 Gen 2004 15:50:50 UTC
(defun shimbun-wikimedia-en-date-decode (date-string)
  "Decode date function for English (default)"
  (shimbun-wikimedia-latin-date-decode date-string
				       '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))

(defun shimbun-wikimedia-de-date-decode (date-string)
  "Decode date function for de"
  (shimbun-wikimedia-latin-date-decode date-string
				       '("Jan" "Feb" "Mar" "Apr" "Mai" "Jun" "Jul" "Aug" "Sep" "Okt" "Nov" "Dez")))

(defun shimbun-wikimedia-es-date-decode (date-string)
  "Decode date function for es"
  (shimbun-wikimedia-latin-date-decode date-string
				       '("Jan" "Feb" "Mar" "Apr" "Maj" "Jun" "Jul" "Aug" "Sep" "Okt" "Nov" "Dec")))

(defun shimbun-wikimedia-fr-date-decode (date-string)
  "Decode date function for fr"
  (shimbun-wikimedia-latin-date-decode date-string
				       '("Jan" "Fev" "Mar" "Avr" "Mai" "Jui" "Juli" "Aou" "Sep" "Oct" "Nov" "Déc")))

(defun shimbun-wikimedia-ia-date-decode (date-string)
  "Decode date function for ia"
  (shimbun-wikimedia-en-date-decode date-string))

(defun shimbun-wikimedia-is-date-decode (date-string)
  "Decode date function for is"
  (shimbun-wikimedia-latin-date-decode date-string
				       '("Jan" "Feb" "Mar" "Apr" "Maí" "Jún" "Júl" "Ágú" "Sep" "Okt" "Nóv" "Des")))

(defun shimbun-wikimedia-it-date-decode (date-string)
  "Decode date function for it"
  (shimbun-wikimedia-latin-date-decode date-string
				       '("Gen" "Feb" "Mar" "Apr" "Mag" "Giu" "Lug" "Ago" "Set" "Ott" "Nov" "Dic")))

(defun shimbun-wikimedia-ja-date-decode (date-string)
  "Decode date function for Japnese localized"
  (if (string-match " *\\([0-9][0-9][0-9][0-9]\\)年 *\\([0-9][0-9]*\\)月 *\\([0-9][0-9]*\\)日 *(\\(月\\|火\\|水\\|木\\|金\\|土\\|日\\)) *\\([:0-9]+\\) *\\([A-Z]+\\) *" date-string)
      ;; <I>2003年 4月 11日 (金) 02:43:25 CEST</I> ;; squeak-ja
      (shimbun-make-date-string
       (string-to-number (match-string 1 date-string))
       (string-to-number (match-string 2 date-string))
       (string-to-number (match-string 3 date-string))
       (match-string 5 date-string)
       (match-string 6 date-string))
    ;; In the early days, the RFC822 style date format has been used.
    date-string))

(defun shimbun-wikimedia-no-date-decode (date-string)
  "Decode date function for no"
  (shimbun-wikimedia-latin-date-decode date-string
				       '("Jan" "Feb" "Mar" "Apr" "Mai" "Jun" "Jul" "Aug" "Sep" "Okt" "Nov" "Des")))

(defun shimbun-wikimedia-pl-date-decode (date-string)
  "Decode date function for pl"
  (shimbun-wikimedia-latin-date-decode date-string
				       '("Sty" "Lut" "Mar" "Kwi" "Maj" "Cze" "Lip" "Sie" "Wrz" "Paź" "Lis" "Gru")))

(defvar shimbun-wikimedia-date-decode-rule-alist
  '(;; Regex pattern(year month day time zone)
    ("\\([^ ,0-9]+\\) +\\([^ ,0-9]+\\) +\\([0-9]+\\) +\\([:0-9]+\\) +\\([A-Z]+\\) +\\([0-9]+\\)" .
     (6 2 3 4 5))
    ("\\([^ ,0-9]+\\),? +\\([0-9]+\\)\\.? +\\([^ ,0-9]+\\) +\\([0-9]+\\),? +\\([:0-9]+\\) +\\([A-Z]+\\)" .
     (4 3 2 5 6))
    ))

(defun shimbun-wikimedia-latin-date-decode (date-string month-list)
  "Decode date function for latin,require month-string-list 1-12"
  (let ((alist shimbun-wikimedia-date-decode-rule-alist)
	(date "")
	(match-flag nil)
	regex rule year month day time zone)
    (while (progn
	     (setq regex (car (car alist)))
	     (setq rule  (cdr (car alist)))
	     (setq match-flag (string-match regex date-string))
	     ;; if match-flag is non-nil,match regex for date-string
	     (when match-flag
	       (setq year  (match-string (nth 0 rule) date-string))
	       (setq month (match-string (nth 1 rule) date-string))
	       (setq day   (match-string (nth 2 rule) date-string))
	       (setq time  (match-string (nth 3 rule) date-string))
	       (setq zone  (match-string (nth 4 rule) date-string)))
	     (setq alist (cdr alist))
	     ;; repeat if alist is remain,and match-flag are nil
	     (and alist (not match-flag))))

    (when match-flag ;; matched
      (setq date (shimbun-make-date-string
		  (string-to-number year)
		  (length (member month (reverse month-list)))
		  (string-to-number day)
		  time
		  zone))
      (print date))
    date))

(provide 'sb-wikimedia)

;;; sb-wikimedia.el ends here
