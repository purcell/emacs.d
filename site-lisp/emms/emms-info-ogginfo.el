;;; emms-info-ogginfo.el --- Emms information from Ogg Vorbis files.

;; Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Jorgen Schaefer <forcer@forcix.cx>
;;         Yoni Rabkin <yonirabkin@member.fsf.org>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;

;;; Code:

(require 'emms-info)

(defgroup emms-info-ogginfo nil
  "An EMMS-info method for getting, using the external ogginfo
program"
  :group 'emms-info)

(defcustom emms-info-ogginfo-coding-system 'utf-8
  "*Coding system used in the output of ogginfo."
  :type 'coding-system
  :group 'emms-info-ogginfo)

(defcustom emms-info-ogginfo-program-name "ogginfo"
  "*The name/path of the ogginfo tag program."
  :type 'string
  :group 'emms-info-ogginfo)

(defun emms-info-ogginfo (track)
  "Add track information to TRACK.
This is a useful element for `emms-info-functions'."
  (when (and (eq 'file (emms-track-type track))
             (string-match "\\.[Oo][Gg][Gg]\\'" (emms-track-name track)))

    (with-temp-buffer
      (call-process emms-info-ogginfo-program-name
                    nil t nil (emms-track-name track))

      ;; play time, emms-info-ogg.el [U. Jensen]
      (goto-char (point-min))
      (when (re-search-forward
             "Playback length: \\([0-9]*\\)m:\\([0-9]*\\)" nil t)
        (let* ((minutes (string-to-number (match-string 1)))
               (seconds (string-to-number (match-string 2)))
               (ptime-total (+ (* minutes 60) seconds))
               (ptime-min minutes)
               (ptime-sec seconds))
          (emms-track-set track 'info-playing-time ptime-total)
          (emms-track-set track 'info-playing-time-min ptime-min)
          (emms-track-set track 'info-playing-time-sec ptime-sec)
          (emms-track-set track 'info-file (emms-track-name track))))

      ;; all the rest of the info available
      (goto-char (point-min))
      (when (re-search-forward "^.*\\.\\.\\.$" (point-max) t)
        (while (zerop (forward-line 1))
          (when (looking-at "^\t\\(.*?\\)=\\(.*\\)$") ; recognize the first '='
            (let ((a (match-string 1))
                  (b (match-string 2)))
              (when (and (< 0 (length a))
                         (< 0 (length b)))
                (emms-track-set track
                                (intern (downcase (concat "info-" (match-string 1))))
                                (match-string 2))))))))))

(provide 'emms-info-ogginfo)

;;; emms-info-ogginfo.el ends here
