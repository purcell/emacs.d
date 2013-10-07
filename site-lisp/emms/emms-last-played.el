;;; emms-last-played.el --- Support for last-played-time of a track

;; Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Lucas Bonnet <lucas@rincevent.net>
;; Keywords: emms, mp3, mpeg, multimedia

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Records when the track was last played.
;; Big portions of the time handling fuctions are copied from
;; gnus-util.el, and slightly adapted.

;;; Code:

(require 'emms)

(defvar emms-last-played-keep-count t
  "Specifies if EMMS should record the number of times you play a track.
Set it to t if you want such a feature, and to nil if you don't.")

(defvar emms-last-played-format-alist
  '(((emms-last-played-seconds-today) . "%k:%M")
    (604800 . "%a %k:%M")                   ;;that's one week
    ((emms-last-played-seconds-month) . "%a %d")
    ((emms-last-played-seconds-year) . "%b %d")
    (t . "%b %d '%y"))                      ;;this one is used when no
					    ;;other does match
  "Specifies date format depending on when a track was last played.
This is an alist of items (AGE . FORMAT).  AGE can be a number (of
seconds) or a Lisp expression evaluating to a number.  When the age of
the track is less than this number, then use `format-time-string'
with the corresponding FORMAT for displaying the date of the track.
If AGE is not a number or a Lisp expression evaluating to a
non-number, then the corresponding FORMAT is used as a default value.

Note that the list is processed from the beginning, so it should be
sorted by ascending AGE.  Also note that items following the first
non-number AGE will be ignored.

You can use the functions `emms-last-played-seconds-today',
`emms-last-played-seconds-month' and
`emms-last-played-seconds-year' in the AGE spec.  They return the
number of seconds passed since the start of today, of this month,
of this year, respectively.")


(defun emms-last-played-update-track (track)
  "Updates the last-played time of TRACK."
  (emms-track-set track 'last-played (current-time)))

(defun emms-last-played-increment-count (track)
  "Increments the play-count property of TRACK.
If non-existent, it is set to 1."
  (let ((play-count (emms-track-get track 'play-count)))
    (if play-count
        (emms-track-set track 'play-count (1+ play-count))
      (emms-track-set track 'play-count 1))))

(defun emms-last-played-update-current ()
  "Updates the current track."
  (emms-last-played-update-track (emms-playlist-current-selected-track))
  (if emms-last-played-keep-count
      (emms-last-played-increment-count (emms-playlist-current-selected-track))))

(defun emms-last-played-seconds-today ()
  "Return the number of seconds passed today."
  (let ((now (decode-time (current-time))))
    (+ (car now) (* (car (cdr now)) 60) (* (car (nthcdr 2 now)) 3600))))

(defun emms-last-played-seconds-month ()
  "Return the number of seconds passed this month."
  (let ((now (decode-time (current-time))))
    (+ (car now) (* (car (cdr now)) 60) (* (car (nthcdr 2 now)) 3600)
       (* (- (car (nthcdr 3 now)) 1) 3600 24))))

(defun emms-last-played-seconds-year ()
  "Return the number of seconds passed this year."
  (let ((now (decode-time (current-time)))
	(days (format-time-string "%j" (current-time))))
    (+ (car now) (* (car (cdr now)) 60) (* (car (nthcdr 2 now)) 3600)
       (* (- (string-to-number days) 1) 3600 24))))

(defun emms-last-played-format-date (messy-date)
  "Format the messy-date according to emms-last-played-format-alist.
Returns \"  ?  \" if there's bad input or if an other error occurs.
Input should look like this: \"Sun, 14 Oct 2001 13:34:39 +0200\"."
  (condition-case ()
      (let* ((messy-date (float-time messy-date))
             (now (float-time (current-time)))
	     ;;If we don't find something suitable we'll use this one
	     (my-format "%b %d '%y"))
	(let* ((difference (- now messy-date))
	       (templist emms-last-played-format-alist)
	       (top (eval (caar templist))))
	  (while (if (numberp top) (< top difference) (not top))
	    (progn
	      (setq templist (cdr templist))
	      (setq top (eval (caar templist)))))
	  (if (stringp (cdr (car templist)))
	      (setq my-format (cdr (car templist)))))
	(format-time-string (eval my-format) (seconds-to-time messy-date)))
    (error "Never.")))

(provide 'emms-last-played)
;;; emms-last-played.el ends here
