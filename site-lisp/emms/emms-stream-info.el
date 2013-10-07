;;; emms-stream-info.el --- Info from streaming audio

;; Copyright (C) 2004, 2005, 2006, 2007, 2008,
;;   2009 Free Software Foundation, Inc.

;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Set `*emms-stream-info-backend*' to either 'vlc or 'mplayer, which
;; are the two currently supported backends for retrieving stream
;; information.  You can then either call `emms-stream-info-message'
;; directly or hit "i" in the `emms-streams' buffer over stream you
;; want to investigate.
;;
;; Note that you do not have to be playing the stream in question in
;; order to find out what is playing on it since this library will
;; open its own connection to the streaming server.
;;
;; Please send bug reports and stations which do not work to the
;; maintainer (email at the top of this file).

;;; History:
;; 
;; This library was re-implemented from scratch around January of
;; 2009. If you are looking for the old code then grab source code
;; older than that.

;;; Code:

(defvar *emms-stream-info-backend* 'mplayer
  "Symbol designating the backend program to use.")

;; using unhygienic macros for good... or is it evil?
(defmacro emms-stream-info-defreg (symname regexp)
  "Set SYMNAME to be the match for REGEXP."
  `(save-excursion
     (goto-char (point-min))
     (re-search-forward ,regexp (point-max) t)
     (when (and (match-string-no-properties 1)
		(> (length (match-string-no-properties 1)) 0))
       (setq ,symname (match-string-no-properties 1)))))

(defun emms-stream-info-mplayer-backend (url)
  "Backend command for running mplayer on URL."
  (condition-case excep
      (call-process "mplayer" nil t nil
		    "-nocache" "-endpos" "0" "-vo" "null" "-ao" "null"
		    url)
    (file-error
     (error "Could not find the mplayer backend binary"))))

(defun emms-stream-info-vlc-backend (url)
  "Backend command for running VLC on URL."
  (condition-case excep
      (call-process "vlc" nil t nil
		    "-vvv" "--intf" "dummy" "--stop-time" "1" "--noaudio"
		    url "vlc:quit")
    (file-error
     (error "Could not find the VLC backend binary"))))

(defun emms-stream-info-call-backend (url)
  "Call backend and return a list of stream information for URL."
  (let ((name "N/A")
	(genre "N/A")
	(bitrate "N/A")
	(nowplaying "N/A"))
    (with-temp-buffer
      (message "querying stream...")
      (cond
       ((eq *emms-stream-info-backend* 'mplayer)
	(emms-stream-info-mplayer-backend url)
	(emms-stream-info-defreg name "^Name[ ]+:[ ]+\\(.*\\)$")
	(emms-stream-info-defreg genre "^Genre[ ]+:[ ]+\\(.*\\)$")
	(emms-stream-info-defreg bitrate "^Bitrate[ ]+:[ ]+\\(.*\\)$")
	(emms-stream-info-defreg nowplaying "ICY Info: StreamTitle='\\(.+?\\)'"))
       ((eq *emms-stream-info-backend* 'vlc)
	(emms-stream-info-vlc-backend url)
	(emms-stream-info-defreg name "'Title' = '\\(.*\\)'")
	(emms-stream-info-defreg genre "Genre: \\(.*\\)")
	(emms-stream-info-defreg bitrate "bitrate:\\([0-9].+\\)")
	(emms-stream-info-defreg nowplaying "'Now Playing' = '\\(.+?\\)'"))
       (t (error "Unknown backend"))))
    (message "querying stream...done")
    (list name genre bitrate nowplaying)))

;; point of entry
(defun emms-stream-info-message (url)
  "Display a message with information about the stream at URL."
  (interactive "Murl: ")
  (let* ((stream-info (emms-stream-info-call-backend url))
	 (name (nth 0 stream-info))
	 (genre (nth 1 stream-info))
	 (bitrate (nth 2 stream-info))
	 (nowplaying (nth 3 stream-info)))
    (message "now playing: %s on %s, genre: %s, bitrate: %s"
	     nowplaying name genre bitrate)))

(provide 'emms-stream-info)

;;; emms-stream-info.el ends here
