;;; emms-player-vlc.el --- vlc support for EMMS

;; Copyright (C) 2008, 2009 Free Software Foundation, Inc.

;; Authors: Yoni Rabkin <yonirabkin@member.fsf.org>

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

;;; Code:

(require 'emms-compat)
(require 'emms-player-simple)

;; I use this macro, and later override some of the stuff it defines
;; in order to accomodate VLC's particular idioms.
(define-emms-simple-player vlc '(file url)
  (concat "\\`\\(http\\|mms\\)://\\|"
          (emms-player-simple-regexp
           "ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
           "mov" "avi" "divx" "ogm" "ogv" "asf" "mkv"
           "rm" "rmvb" "mp4" "flac" "vob" "m4a" "ape"))
  "vlc" "--intf=rc")

(define-emms-simple-player vlc-playlist '(streamlist)
  "\\`http://"
  "vlc" "--intf=rc")

;; (kludge) By default, VLC does not quit after finishing to play a
;; track, so the player sentinel has no way of telling that the next
;; track should be played. Therefore I redefine this low-level
;; function and add a "quit" track which is invisible to Emms.
(defadvice emms-player-vlc-start (around quit-vlc-after-finish activate)
  (let ((process (apply 'start-process
                        emms-player-simple-process-name
                        nil
                        emms-player-vlc-command-name
                        ;; splice in params here
                        (append emms-player-vlc-parameters
                                (list (emms-track-name (ad-get-arg 0)))
                                '("vlc://quit")))))
    ;; add a sentinel for signaling termination
    (set-process-sentinel process 'emms-player-simple-sentinel))
  (emms-player-started emms-player-vlc))

(defun emms-player-vlc-pause ()
  "Depends on vlc's rc mode."
  (process-send-string
   emms-player-simple-process-name "pause\n"))

(defun emms-player-vlc-seek (sec)
  "Seek relative within a stream."
  (when (not (= 0 sec))
    (process-send-string
     emms-player-simple-process-name
     (if (< 0 sec) "fastforward\n" "rewind\n"))))

(defun emms-player-vlc-seek-to (sec)
  "Seek to time SEC within the stream."
  (process-send-string
   emms-player-simple-process-name
   (format "seek %d\n" sec)))

(emms-player-set emms-player-vlc 'pause 'emms-player-vlc-pause)
(emms-player-set emms-player-vlc 'resume nil) ; pause is also resume
(emms-player-set emms-player-vlc 'start 'emms-player-vlc-start)
(emms-player-set emms-player-vlc 'seek 'emms-player-vlc-seek)
(emms-player-set emms-player-vlc 'seek-to 'emms-player-vlc-seek-to)

(provide 'emms-player-vlc)

;;; emms-player-vlc.el ends here
