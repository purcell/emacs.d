;;; emms-player-mplayer.el --- mplayer support for EMMS

;; Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Authors: William Xu <william.xwl@gmail.com>
;;          Jorgen Schaefer <forcer@forcix.cx>

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

;; This provides a player that uses mplayer. It supports pause and
;; seeking. For loading subtitles automatically, try adding
;; "sub-fuzziness=1" to your `~/.mplayer/config', see mplayer manual for
;; more.

;;; Code:

(require 'emms-compat)
(require 'emms-player-simple)

(define-emms-simple-player mplayer '(file url)
  (concat "\\`\\(http\\|mms\\)://\\|"
          (emms-player-simple-regexp
           "ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
           "mov" "avi" "divx" "ogm" "ogv" "asf" "mkv"
           "rm" "rmvb" "mp4" "flac" "vob" "m4a" "ape"))
  "mplayer" "-slave" "-quiet" "-really-quiet")

(define-emms-simple-player mplayer-playlist '(streamlist)
  "\\`http://"
  "mplayer" "-slave" "-quiet" "-really-quiet" "-playlist")

(emms-player-set emms-player-mplayer
		 'pause
		 'emms-player-mplayer-pause)

;;; Pause is also resume for mplayer
(emms-player-set emms-player-mplayer
                 'resume
                 nil)

(emms-player-set emms-player-mplayer
		 'seek
		 'emms-player-mplayer-seek)

(emms-player-set emms-player-mplayer
		 'seek-to
		 'emms-player-mplayer-seek-to)

(defun emms-player-mplayer-pause ()
  "Depends on mplayer's -slave mode."
  (process-send-string
   emms-player-simple-process-name "pause\n"))

(defun emms-player-mplayer-seek (sec)
  "Depends on mplayer's -slave mode."
  (process-send-string
   emms-player-simple-process-name
   (format "seek %d\n" sec)))

(defun emms-player-mplayer-seek-to (sec)
  "Depends on mplayer's -slave mode."
  (process-send-string
   emms-player-simple-process-name
   (format "seek %d 2\n" sec)))

(provide 'emms-player-mplayer)
;;; emms-player-mplayer.el ends here
