;;; emms-cue.el --- Recognize cue sheet file

;; Copyright (C) 2009 Free Software Foundation, Inc.

;; Author: William Xu <william.xwl@gmail.com>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; By parsing cue file, we will be able to play next/previous track from a
;; single .ape or .flac file.

;;; Code:

(require 'emms-playing-time)
(require 'emms-info)

(defun emms-cue-next ()
  "Play next track from .cue file."
  (interactive)
  (let ((cue-track (emms-cue-next-track)))
    (if (cdr cue-track)
        (progn
          (emms-seek-to (cdr cue-track))
          (message "Will play: %s" (car cue-track)))
      (message "Nothing to seek or missing .cue file?"))))

(defun emms-cue-previous ()
  "Play previous track from .cue file."
  (interactive)
  (let ((cue-track (emms-cue-previous-track)))
    (if (cdr cue-track)
        (progn
          (emms-seek-to (cdr cue-track))
          (message "Will play: %s" (car cue-track)))
      (message "Nothing to seek or missing .cue file?"))))

(defun emms-cue-next-track (&optional previous-p)
  "Get title and offset of next track from .cue file.

When PREVIOUS-P is t, get previous track info instead."
  (let* ((track (emms-playlist-current-selected-track))
         (name (emms-track-get track 'name))
         (cue (concat (file-name-sans-extension name)".cue")))
    (when (file-exists-p cue)
      (with-temp-buffer
        (emms-insert-file-contents cue)
        (save-excursion
          (if previous-p
              (goto-char (point-max))
            (goto-char (point-min)))
          (let ((offset nil)
                (title "")
                ;; We should search one more track far when getting previous
                ;; track.
                (one-more-track previous-p))
            (while (and (not offset)
                        (funcall 
                         (if previous-p 'search-backward-regexp 'search-forward-regexp)
                         "INDEX 01 \\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)" nil t 1))
              (let* ((min (string-to-number (match-string-no-properties 1)))
                     (sec (string-to-number (match-string-no-properties 2)))
                     (msec (string-to-number (match-string-no-properties 3)))
                     (total-sec (+ (* min 60) sec (/ msec 100.0))))
                (when (funcall (if previous-p '> '<) emms-playing-time total-sec)
                  (if (not one-more-track)
                      (progn
                        (setq offset total-sec)
                        (when (search-backward-regexp "TITLE \"\\(.*\\)\"" nil t 1)
                          (setq title (match-string-no-properties 1))))
                    (setq one-more-track nil)))))
            (cons title offset)))))))

(defun emms-cue-previous-track ()
  "See `emms-cue-next-track'."
  (emms-cue-next-track t))

(defun emms-info-cueinfo (track)
  "Add track information to TRACK.
This is a useful element for `emms-info-functions'."
  (when (and (eq 'file (emms-track-type track))
             (string-match "\\.\\(ape\\|flac\\)\\'" (emms-track-name track)))
    (let ((cue (concat (file-name-sans-extension (emms-track-name track))
                       ".cue")))
      (when (file-exists-p cue)
        (with-temp-buffer
          (emms-insert-file-contents cue)
          (save-excursion
            (mapc (lambda (i)
                    (goto-char (point-min))
                    (when (let ((case-fold-search t))
                            (search-forward-regexp 
                             (concat (car i) " \\(.*\\)") nil t 1))
                      (emms-track-set track 
                                      (cdr i)
                                      (replace-regexp-in-string
                                       "\\`\"\\|\"\\'" "" (match-string 1)))))
                  '(("performer" . info-artist)
                    ("title" . info-album)
                    ("title" . info-title)
                    ("rem date" . info-year)))))))))


(provide 'emms-cue)
;;; emms-cue.el ends here
