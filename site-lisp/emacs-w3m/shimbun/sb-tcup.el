;;; sb-tcup.el --- shimbun backend for www.tcup.com -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2005 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
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

;; Original was http://homepage2.nifty.com/strlcat/nnshimbun-tcup.el

;;; Code:

(require 'shimbun)

(eval-and-compile
  (luna-define-class shimbun-tcup (shimbun) (content-hash))
  (luna-define-internal-accessors 'shimbun-tcup))

(defvar shimbun-tcup-group-alist
  '(("yutopia" "http://6116.teacup.com/yutopia/bbs2")
    ("meadow" "http://6629.teacup.com/yutopia/bbs2")
    ("skk" "http://6718.teacup.com/yutopia/bbs2"))
  "An alist of tcup bbs shimbun group definition.
Each element looks like:

 (NAME URL SUBJECT-REGEXP FROM-START-REGEXP DATE-START-REGEXP
           BODY-START-REGEXP BODY-END-REGEXP).

Each element have a following default value:

SUBJECT-REGEXP: `shimbun-tcup-subject-regexp'
FROM-START-REGEXP: `shimbun-tcup-from-start-regexp'
DATE-START-REGEXP: `shimbun-tcup-date-start-regexp'
BODY-START-REGEXP: `shimbun-tcup-body-start-regexp'
BODY-END-REGEXP: `shimbun-tcup-body-end-regexp'")

(defvar shimbun-tcup-subject-regexp
  (let ((s0 "[\t\n ]*")
	(s1 "[\t\n ]+"))
    (concat "<font" s1 "size=\"?4\"?[^>]*>" s0 "<b>" s0 "\\([^<]+\\)"
	    s0 "</b>" s0 "</font>"))
  "Default regexp for subject.
 This have a one parenthesized expression match for subject.")
(defvar shimbun-tcup-from-start-regexp "投稿者：[\t\n ]*"
  "Default regexp for from start string.")
(defvar shimbun-tcup-date-start-regexp "投稿日：[\t\n ]*"
  "Default regexp for date start string.")
(defvar shimbun-tcup-body-start-regexp
  "<blockquote>\\([\t\n ]*<[^>]+>\\)*[\t\n ]*"
  "Default regexp for body start string.")
(defvar shimbun-tcup-body-end-regexp
  "[\t\n ]*\\(<[^>]+>[\t\n ]*\\)*</blockquote>"
  "Default regexp for body end string.")

(defvar shimbun-tcup-content-hash-length 31)
(defvar shimbun-tcup-x-face-alist
  '(("yutopia" . "X-Face: ,Em61:vG$KP!G`Q]ZsO\\@&g`VXE-kicRnKs\"Wd'ZSF\
Q*O'i6OJ2(U$x6/gytz:<jCUn+&*e\n 8$BTg.~1,7OS%tjW#ty4Cp7x%6SD;aNfn(ugAN\
CC]q(-foA:@ULvLAJz_oeP1@a~C+Bxc3I\\+^W<%n\n y,z@:VoRoJXl'E`kX]3i1m;+I`")
    ("meadow" . "X-Face: xo];SyM=kg&iWSACakk9gGth>s`0KE!+n9}l[&W\
SG!QUj`15/+hzWfCvZ\\`R!i<c8{QI=hw\n Ez}CH&IOYewgffOCh5jTPWx/ehA\\\
:Qe[;P>8re^8`\\8omn]t;P~wC{X%Y$q/f!zC%IG1RVFj~Jf`c6\n t98[2O!+vg\
w!!gb8HQ,s0F*e6f*xs\"HR}{':>)Q_|+67gobo%?|n_SdjfzLI6kJ(T;q{+?p?")))

(luna-define-method initialize-instance :after ((shimbun shimbun-tcup)
						&rest init-args)
  (shimbun-tcup-set-content-hash-internal
   shimbun
   (make-vector shimbun-tcup-content-hash-length 0))
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-tcup))
  (mapcar 'car shimbun-tcup-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-tcup))
  (cadr (assoc (shimbun-current-group-internal shimbun)
	       shimbun-tcup-group-alist)))

(defun shimbun-tcup-get-group-key (group)
  "Returns (hostname . board-id)"
  (let ((url (cadr (assoc group
			  shimbun-tcup-group-alist))))
    (cond
     ((string-match "\\(^\\|://\\)\\([^:/]+\\)/\\([^/]+\\)/bbs"
		    url)		; "http://6718.teacup.com/yutopia/bbs"
      (cons (match-string 2 url)	; "6718.teacup.com"
	    (match-string 3 url)))	; "yutopia"
     ((string-match "\\(^\\|://\\)www[^/]+/\\([0-9]+\\)/\\([^/]+\\)\.html"
		    url)		; "http://www67.tcup.com/6718/yutopia.html"
      (cons (concat
	     (match-string 2 url)	; "6718"
	     ".teacup.com")
	    (match-string 3 url)))	; "yutopia"
     (t
      nil))))

(defun shimbun-tcup-stime-to-time (stime)
  (let (a b c)
    (setq a (length stime))
    (setq b (- (string-to-number (substring stime 0 (- a 4))) 9))
    (setq c (+ (string-to-number (substring stime (- a 4) a))
	       (* (% b 4096) 10000)
	       90000))
    (list (+ (* (/ b 4096) 625) (/ c 65536)) (% c 65536))))

(defun shimbun-tcup-make-time ()
  (let (yr mon day hr min sec dow tm)
    (looking-at
     "\\([ 0-9]+\\)月\\([ 0-9]+\\)日(\\(.\\))\\([ 0-9]+\\)時\\([ 0-9]+\\)分\\([ 0-9]+\\)秒")
    (setq mon (string-to-number (match-string 1))
	  day (string-to-number (match-string 2))
	  dow (match-string 3)
	  hr  (string-to-number (match-string 4))
	  min (string-to-number (match-string 5))
	  sec (string-to-number (match-string 6)))
    (setq dow (string-match dow "日月火水木金土"))
    (setq yr (nth 5 (decode-time (current-time))))
    (setq tm (encode-time sec min hr day mon yr))
    (while (not (eq dow (nth 6 (decode-time tm))))
      (setq yr (1- yr))
      (setq tm (encode-time sec min hr day mon yr)))
    tm))

(defun shimbun-tcup-make-id (stime group)
  (let ((keys (shimbun-tcup-get-group-key group)))
    (format "<%s.%s@%s>"		; "<1576232885.yutopia@6718.teacup.com>"
	    stime (cdr keys) (car keys))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-tcup)
				     &optional range)
;;;<DEBUG>
;;  (shimbun-tcup-get-headers shimbun range))
;;
;;(defun shimbun-tcup-get-headers (shimbun range)
;;;</DEBUG>
  (let* ((case-fold-search t)
	 (group (shimbun-current-group-internal shimbun))
	 (param (assoc group shimbun-tcup-group-alist))
	 (subject-regexp (or (nth 2 param) shimbun-tcup-subject-regexp))
	 (from-regexp (or (nth 3 param) shimbun-tcup-from-start-regexp))
	 (date-regexp (or (nth 4 param) shimbun-tcup-date-start-regexp))
	 (body-st-regexp (or (nth 5 param) shimbun-tcup-body-start-regexp))
	 (body-end-regexp (or (nth 6 param) shimbun-tcup-body-end-regexp))
	 (index-url (shimbun-index-url shimbun))
	 headers from subject date id url stime st body)
    (catch 'stop
      (while t
	(while (re-search-forward subject-regexp nil t)
	  (setq subject (match-string 1))
	  (re-search-forward from-regexp)
	  (setq from
		(cond
		 ((looking-at "<b><a href=\"mailto:\\([^\"]+\\)\">\\([^<]+\\)<")
		  (concat (match-string 2) " <" (match-string 1) ">"))
		 ((looking-at "\\(<[^>]+>[\t\n ]*\\)+\\([^<]+\\)[\t\n ]*<")
		  (match-string 2))
		 (t "(none)")))
	  (re-search-forward date-regexp nil t)
	  (cond ((looking-at "[^,]+, Time: \\([^ ]+\\) ")
		 (setq stime (shimbun-tcup-stime-to-time (match-string 1)))
		 ;; The clock has been advanced for nine hours in the
		 ;; level-1 bulletin boards.
		 (when (string-match "/bbs/?\\'\\|/bbs\\?" index-url)
		   (let ((ms (car stime))
			 (ls (cadr stime)))
		     (setq ls (- ls 32400))
		     (cond ((< ls 0)
			    (setq stime (list (1- ms) (+ ls 65536))))
			   ((>= ls 65536)
			    (setq stime (list (1+ ms) (- ls 65536))))
			   (t
			    (setq stime (list ms ls)))))))
		((looking-at "\\([^<]+\\)<")
		 (setq stime (shimbun-tcup-make-time)))
		(t
		 (setq stime (current-time))))
	  (let ((system-time-locale "C"))
	    (setq date (format-time-string "%d %b %Y %T %z" stime)))
	  (setq stime (format "%05d%05d" (car stime) (cadr stime)))
	  (setq id (shimbun-tcup-make-id stime group))
	  (when (shimbun-search-id shimbun id)
	    (throw 'stop nil))
	  (re-search-forward body-st-regexp)
	  (setq st (match-end 0))
	  (re-search-forward body-end-regexp)
	  (setq body (concat (buffer-substring st (match-beginning 0)) "\n"))
	  (forward-line 1)
	  (setq url
		(if (looking-at "<a[^>]+>[^<]+</a>")
		    (concat (match-string 0) "\n<p>\n")
		  ""))
	  (set (intern stime (shimbun-tcup-content-hash-internal shimbun))
	       (concat body "<p>\n" url))
	  (push (shimbun-make-header
		 0
		 (shimbun-mime-encode-string subject)
		 (shimbun-mime-encode-string from)
		 date id "" 0 0 stime)
		headers))
	(goto-char (point-min))
	(if (re-search-forward "<a[\t\n ]+href=\"\\([^\"]+\\)\"[^>]*>[\t\n ]*\
次のページ[\t\n ]*</a>"
			       nil t)
	    (progn
	      (shimbun-retrieve-url
	       (prog1
		   (shimbun-expand-url (match-string 1) index-url)
		 (erase-buffer))
	       t)
	      (goto-char (point-min)))
	  (throw 'stop nil))))
    headers))

(luna-define-method shimbun-article ((shimbun shimbun-tcup) header
				     &optional outbuf)
;;;<DEBUG>
;;  (shimbun-tcup-article shimbun header outbuf))
;;
;;(defun shimbun-tcup-article (shimbun header outbuf)
;;;</DEBUG>
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (insert
       (with-temp-buffer
	 (let ((sym (intern-soft (shimbun-header-xref header)
				 (shimbun-tcup-content-hash-internal
				  shimbun))))
	   (if (boundp sym)
	       (insert (or (symbol-value sym) "")))
	   (goto-char (point-min))
	   (shimbun-header-insert shimbun header)
	   (insert "Content-Type: " "text/html"
		   "; charset=ISO-2022-JP\n"
		   "MIME-Version: 1.0\n")
	   (insert "\n")
	   (encode-coding-string
	    (buffer-string)
	    (mime-charset-to-coding-system "ISO-2022-JP"))))))))

(provide 'sb-tcup)

;;; sb-tcup.el ends here
