;;; emms-player-xine.el --- xine support for EMMS

;; Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Tassilo Horn <tassilo@member.fsf.org>

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

;; This provides a player that uses xine. It supports pause and
;; seeking.

;;; Code:

;; TODO: The video window cannot be disabled. I asked on
;; gmane.comp.video.xine.user (<87y7ohqcbq.fsf@baldur.tsdh.de>)...

;; TODO: Implement seek-to with "SetPositionX%\n" where X is in {0,10,..,90}

(require 'emms-player-simple)

(define-emms-simple-player xine '(file url)
  (concat "\\`\\(http\\|mms\\)://\\|"
          (emms-player-simple-regexp
           "ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
           "mov" "avi" "divx" "ogm" "ogv" "asf" "mkv"
           "rm" "rmvb" "mp4" "flac" "vob"))
  "xine" "--no-gui" "--no-logo" "--no-splash" "--no-reload" "--stdctl")

(emms-player-set emms-player-xine
                 'pause
                 'emms-player-xine-pause)

;;; Pause is also resume for xine
(emms-player-set emms-player-xine
                 'resume
                 nil)

(emms-player-set emms-player-xine
                 'seek
                 'emms-player-xine-seek)

(defun emms-player-xine-pause ()
  "Depends on xine's --stdctl mode."
  (process-send-string
   emms-player-simple-process-name "pause\n"))

(defun emms-player-xine-seek (secs)
  "Depends on xine's --stdctl mode."
  ;; xine-ui's stdctl supports only seeking forward/backward in 7/15/30 and 60
  ;; second steps, so we take the value that is nearest to SECS.
  (let ((s (emms-nearest-value secs '(-60 -30 -15 -7 7 15 30 60))))
    (when (/= s secs)
      (message (concat "EMMS: Xine only supports seeking for [+/-] 7/15/30/60 "
                       "seconds, so we seeked %d seconds") s))
    (process-send-string
     emms-player-simple-process-name
     (if (< s 0)
         (format "SeekRelative%d\n" s)
       (format "SeekRelative+%d\n" s)))))

(defun emms-nearest-value (val list)
  "Returns the value of LIST which is nearest to VAL.

LIST should be a list of integers."
  (let* ((nearest (car list))
         (dist (abs (- val nearest))))
    (dolist (lval (cdr list))
      (let ((ndist (abs (- val lval))))
        (when (< ndist dist)
          (setq nearest lval
                dist    ndist))))
    nearest))


(provide 'emms-player-xine)
;;; emms-player-xine.el ends here
