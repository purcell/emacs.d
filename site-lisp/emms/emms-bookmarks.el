;;; emms-bookmarks.el --- Bookmarks for Emms.

;; Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>
;; Keywords: emms, bookmark

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; You can use this to add ``temporal bookmarks'' (term by Lucas
;; Bonnet) to your media files.  The interesting functions here are
;; `emms-bookmarks-next', `emms-bookmarks-prev', `emms-bookmarks-add'
;; (which pauses the player while you describe the bookmark) and
;; `emms-bookmarks-clear'.  All of which do exactly what you think
;; they do.

;;; Code:


;; dependencies
(require 'emms)
(require 'emms-playing-time)

(defvar emms-bookmarks-prev-overshoot 5
  "Time in seconds for skipping a previous bookmark.")

(defun emms-bookmarks-reset (track)
  "Remove all the bookmarks from TRACK."
  (emms-track-set track 'bookmarks nil))

(defun emms-bookmarks-straight-insertion-sort (item l acc)
  "Insert ITEM into the already sorted L, ACC should be nil."
  (if (null l)
      (append acc (list item))
    (cond ((< (cdr item) (cdr (car l))) (append acc (list item (car l)) (cdr l)))
	  (t (emms-bookmarks-straight-insertion-sort item (cdr l) (append acc (list (car l))))))))

(defun emms-bookmarks-get (track)
  "Return the bookmark property from TRACK."
  (emms-track-get track 'bookmarks))

(defun emms-bookmarks-set (track desc time)
  "Set bookmark property for TRACK, text DESC at TIME seconds."
  (let ((old-bookmarks (emms-track-get track 'bookmarks))
	(new-bookmarks nil))
    (setq new-bookmarks (emms-bookmarks-straight-insertion-sort (cons desc time) old-bookmarks nil))
    (emms-track-set track 'bookmarks new-bookmarks)))

(defun emms-bookmarks-set-current (desc)
  "Set bookmark property for the current track with text DESC."
  (emms-bookmarks-set (emms-playlist-current-selected-track) desc emms-playing-time))

(defun emms-bookmarks-search (time track test)
  "Return a bookmark based on heuristics.

TIME should be a reference point in seconds.
TRACK should be an Emms track.
TEST should be a numerical comparator predicate."
  (let ((s (append (list (cons "time" time)) (copy-sequence (emms-bookmarks-get track)))))
    (sort s #'(lambda (a b) (funcall test (cdr a) (cdr b))))
    (while (not (= time (cdar s)))
      (setq s (cdr s)))
    (when (cdr s)
      (car (cdr s)))))

(defun emms-bookmarks-next-1 (time track)
  "Return the bookmark after TIME for TRACK, otherwise return nil."
  (emms-bookmarks-search time track #'<))

(defun emms-bookmarks-prev-1 (time track)
  "Return the bookmark before TIME for TRACK, otherwise return nil."
  (emms-bookmarks-search (- time emms-bookmarks-prev-overshoot) track #'>))

(defun emms-bookmarks-goto (search-f track failure-message)
  "Seek the player to a bookmark.

SEARCH-F should be a function which returns a bookmark.
TRACK should be an Emms track.
FAILURE-MESSAGE should be a string."
  ;; note that when emms is paused then `emms-player-playing-p' => t
  (when (not emms-player-playing-p)
    (emms-start))
  (let ((m (funcall search-f emms-playing-time track)))
    (if m
	(progn
	  (emms-player-seek-to (cdr m))
	  (message "%s" (car m)))
      (message "%s" failure-message))))


;; entry points

(defun emms-bookmarks-next ()
  "Seek to the next bookmark in the current track."
  (interactive)
  (emms-bookmarks-goto #'emms-bookmarks-next-1
		       (emms-playlist-current-selected-track)
		       "No next bookmark"))

(defun emms-bookmarks-prev ()
  "Seek to the previous bookmark in the current track."
  (interactive)
  (emms-bookmarks-goto #'emms-bookmarks-prev-1
		       (emms-playlist-current-selected-track)
		       "No previous bookmark"))

(defmacro emms-bookmarks-with-paused-player (&rest body)
  "Eval BODY with player paused."
  `(progn
     (when (not emms-player-paused-p) (emms-pause))
     ,@body
     (when emms-player-paused-p (emms-pause))))

;; can't use `interactive' to promt the user here because we want to
;; pause the player before the prompt appears.
(defun emms-bookmarks-add ()
  "Add a new bookmark to the current track.

This function pauses the player while prompting the user for a
description of the bookmark.  The function resumes the player
after the prompt."
  (interactive)
  (emms-bookmarks-with-paused-player
   (let ((desc (read-string "Description: ")))
     (if (emms-playlist-current-selected-track)
	 (emms-bookmarks-set-current desc)
       (error "No current track to bookmark")))))

(defun emms-bookmarks-clear ()
  "Remove all the bookmarks from the current track."
  (interactive)
  (let ((this (emms-playlist-current-selected-track)))
    (when this (emms-bookmarks-reset this))))

(provide 'emms-bookmarks)

;;; emms-bookmarks.el ends here
