;;; emms-score.el --- Scoring system for mp3player

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008,
;;   2009 Free Software Foundation, Inc.

;; Authors: Jean-Philippe Theberge <jphiltheberge@videotron.ca>,
;;          Yoni Rabkin <yonirabkin@member.fsf.org>
;;
;; Keywords: emms, mp3, mpeg, multimedia

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
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; NOTE: This is experimental stuff - comments welcome!  There
;; shouldn't worky anything in that file... scores aren't saved, they
;; even don't have any consequence on playing order and there's just
;; one mood in the moment. But it's a beginning and you can score down
;; or up tracks... :)
;;
;; * How to use scoring in emms
;;
;; When you load emms, you are set to a default mood
;; 'emms-default-mood' A mood is a one word string describing how
;; you feel (like "funny", "tired", "aggresive"...)  Each mood have is
;; own set of scoring rules.
;;
;; You can change your mood with M-x emms-score-change-mood.
;;
;; Every music file start with a default score of 0 the command
;; emms-score-up-playing and emms-score-down-playing modify the
;; score of the file you are curently listening by 1 In addition,
;; skipping a file (with emms-skip) automaticaly score the file
;; down.
;;
;; With scoring on (this mean the variable emms-use-scoring is t),
;; emms will compare the score of the file with your tolerance to
;; decide if it is played or not.
;;
;; The default tolerance level is 0 (or the variable
;; emms-score-min-score).  This mean files with a score of 0 or more will
;; be played and files with a score of -1 or less will be skipped.
;;
;; You can change the tolerance (by 1) with M-x
;; emms-score-lower-tolerance and M-x
;; emms-score-be-more-tolerant

;;; Code:

(require 'emms)

(defvar emms-scores-list nil)
(defvar emms-score-current-mood 'default)
(defvar emms-score-min-score 0)
(defvar emms-score-default-score 0)
(defvar emms-score-hash (make-hash-table :test 'equal))
(defvar emms-score-enabled-p nil
  "If non-nil, emms score is active.")

(defcustom emms-score-file (concat (file-name-as-directory emms-directory) "scores")
  "*Directory to store the score file."
  :type 'directory
  :group 'emms)


;;; User Interfaces

(defun emms-score (arg)
  "Turn on emms-score if prefix argument ARG is a positive integer,
off otherwise."
  (interactive "p")
  (if (and arg (> arg 0))
      (progn
	(setq emms-score-enabled-p t)
        (setq emms-player-next-function 'emms-score-next-noerror)
	(emms-score-load-hash)
        (unless noninteractive
          (add-hook 'kill-emacs-hook 'emms-score-save-hash)))
    (setq emms-score-enabled-p nil)
    (setq emms-player-next-function 'emms-next-noerror)
    (emms-score-save-hash)
    (remove-hook 'kill-emacs-hook 'emms-score-save-hash)))

;;;###autoload
(defun emms-score-enable ()
  "Turn on emms-score."
  (interactive)
  (emms-score 1)
  (message "emms score enabled"))

;;;###autoload
(defun emms-score-disable ()
  "Turn off emms-score."
  (interactive)
  (emms-score -1)
  (message "emms score disabled"))

;;;###autoload
(defun emms-score-toggle ()
  "Toggle emms-score."
  (interactive)
  (if emms-score-enabled-p
      (emms-score-disable)
    (emms-score-enable)))

(defun emms-score-change-mood (mood)
  "Change the current MOOD.
The score hash is automatically saved."
  (interactive "sMood: ")
  (emms-score-save-hash)
  (setq emms-score-current-mood (intern (downcase mood))))

(defun emms-score-up-playing ()
  (interactive)
  (if emms-player-playing-p
      (emms-score-change-score 1 (emms-score-current-selected-track-filename))
    (error "No track currently playing")))

(defun emms-score-down-playing ()
  (interactive)
  (if emms-player-playing-p
      (emms-score-change-score -1 (emms-score-current-selected-track-filename))
    (error "No track currently playing")))

(defun emms-score-up-file-on-line ()
  (interactive)
  (emms-score-change-score 1 (emms-score-track-at-filename)))

(defun emms-score-down-file-on-line ()
  (interactive)
  (emms-score-change-score -1 (emms-score-track-at-filename)))

(defun emms-score-less-tolerant ()
  "Only play mp3 with a higher score"
  (interactive)
  (setq emms-score-min-score (+ emms-score-min-score 1))
  (message "Will play songs with a score >= %d" emms-score-min-score))

(defun emms-score-more-tolerant ()
  "Allow playing of mp3 with a lower score."
  (interactive)
  (setq emms-score-min-score (- emms-score-min-score 1))
  (message "Will play songs with a score >= %d" emms-score-min-score))

(defun emms-score-set-playing (score)
  "Set score for current playing track."
  (interactive "nSet score for playing track: ")
  (let ((filename (emms-score-current-selected-track-filename)))
    (if emms-player-playing-p
	(emms-score-change-score
	 (- score (emms-score-get-score filename))
	 filename)
      (error "No track currently playing"))))

(defun emms-score-set-file-on-line (score)
  "Set score for track at point in emms-playlist buffer."
  (interactive "nSet score for track at point: ")
  (let ((filename (emms-score-track-at-filename)))
    (if emms-player-playing-p
	(emms-score-change-score
	 (- score (emms-score-get-score filename))
	 filename))))

(defun emms-score-set-tolerance (tolerance)
  "Allow playing tracks with a score >= tolerance."
  (interactive "nSet tolerance: ")
  (setq emms-score-min-score tolerance)
  (message "Will play songs with a score >= %d" emms-score-min-score))

(defun emms-score-show-playing ()
  "Show score for current playing track in minibuf."
  (interactive)
  (message "track/tolerance score: %d/%d"
	   (emms-score-get-score
	    (emms-score-current-selected-track-filename))
	   emms-score-min-score))

(defun emms-score-show-file-on-line ()
  "Show score for track at point in emms-playlist buffer."
  (interactive)
  (message "track/tolerance score: %d/%d"
	   (emms-score-get-score
            (emms-score-track-at-filename))
	   emms-score-min-score))


;;; Internal Functions

(defun emms-score-current-selected-track-filename ()
  "Return filename of current selected track."
  (emms-track-get (emms-playlist-current-selected-track) 'name))

(defun emms-score-track-at-filename ()
  "Return file of track at point in emms-playlist buffer."
  (emms-track-get (emms-playlist-track-at) 'name))

(defun emms-score-next-noerror ()
  "Run `emms-next-noerror' with score check.
See also `emms-next-noerror'."
  (interactive)
  (when emms-player-playing-p
    (error "A track is already being played"))
  (cond (emms-repeat-track
	 (emms-start))
	((condition-case nil
	     (progn
	       (emms-playlist-current-select-next)
	       t)
	   (error nil))
	 (if (emms-score-check-score
	      (emms-score-current-selected-track-filename))
	     (emms-start)
	   (emms-score-next-noerror)))
	(t
	 (message "No next track in playlist"))))

(defun emms-score-save-hash ()
  "Save score hash in `emms-score-file'."
  (interactive)
  (unless (file-directory-p (file-name-directory emms-score-file))
    (make-directory (file-name-directory emms-score-file)))
  (with-temp-file emms-score-file
    (let ((standard-output (current-buffer)))
      (insert "(")
      (maphash (lambda (key value)
		 (prin1 (cons key value)))
	       emms-score-hash)
      (insert ")"))))

(defun emms-score-load-hash ()
  "Load score hash from `emms-score-file'."
  (interactive)
  (if (file-exists-p emms-score-file)
      (let ((score-string (with-temp-buffer
			    (emms-insert-file-contents emms-score-file)
			    (buffer-string))))
	(if (> (length score-string) 0)
	    (mapc (lambda (elt)
		    (puthash (car elt) (cdr elt) emms-score-hash))
		  (read score-string)))
	;; when file not exists, make empty but valid score file
	(emms-score-save-hash))))

(defun emms-score-get-plist (filename)
  (gethash filename emms-score-hash))

(defun emms-score-change-score (score filename)
  (let ((sp (emms-score-get-plist filename) )
	(sc (emms-score-get-score filename)))
    (puthash filename
	     (plist-put sp emms-score-current-mood (+ sc score))
	     emms-score-hash)
    (message "New score is %s" (+ score sc))))

(defun emms-score-create-entry (filename)
  (puthash filename
	   `(,emms-score-current-mood ,emms-score-default-score)
	   emms-score-hash))

(defun emms-score-get-score (filename)
  "Return score of TRACK."
  (let ((plist (emms-score-get-plist filename)))
    (if (member emms-score-current-mood plist)
	(plist-get plist emms-score-current-mood)
      (emms-score-create-entry filename)
      (emms-score-get-score filename))))

(defun emms-score-check-score (filename)
  (>= (emms-score-get-score filename) emms-score-min-score))

(provide 'emms-score)

;;; emms-scores.el ends here
