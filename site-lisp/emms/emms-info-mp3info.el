;;; emms-info-mp3info.el --- Info-method for EMMS using mp3info

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008,
;;   2009 Free Software Foundation, Inc.

;; Authors: Ulrik Jensen <terryp@daimi.au.dk>
;;          Jorgen Schäfer <forcer@forcix.cx>
;; Keywords:

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
;; along with EMMS; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This code has been adapted from code found in mp3player.el, written
;; by Jean-Philippe Theberge (jphiltheberge@videotron.ca), Mario
;; Domgoergen (kanaldrache@gmx.de) and Jorgen Schäfer
;; <forcer@forcix.cx>

;; To activate this method for getting info, use something like:

;; (require 'emms-info-mp3info)
;; (add-to-list 'emms-info-functions 'emms-info-mp3info)

;;; Code:

(require 'emms-info)

(defvar emms-info-mp3info-version "0.2 $Revision: 1.10 $"
  "EMMS info mp3info version string.")
;; $Id: emms-info-mp3info.el,v 1.10 2005/08/12 18:01:16 xwl Exp $

(defgroup emms-info-mp3info nil
  "An EMMS-info method for getting/setting ID3v1 tags, using the
external mp3info program"
  :group 'emms-info)

(defcustom emms-info-mp3info-coding-system 'utf-8
  "*Coding system used in the output of mp3info."
  :type 'coding-system
  :group 'emms-info-mp3info)

(defcustom emms-info-mp3info-program-name "mp3info"
  "*The name/path of the mp3info tag program."
  :type 'string
  :group 'emms-info-mp3info)

(defcustom emms-info-mp3find-arguments
  `("-p" ,(concat "info-artist=%a\\n"
                  "info-title=%t\\n"
                  "info-album=%l\\n"
                  "info-tracknumber=%n\\n"
                  "info-year=%y\\n"
                  "info-genre=%g\\n"
                  "info-note=%c\\n"
                  "info-playing-time=%S\\n"))
  "The argument to pass to `emms-info-mp3info-program-name'.
This should be a list of info-flag=value lines."
  :type '(repeat string)
  :group 'emms-info-mp3info)

(defun emms-info-mp3info (track)
  "Add track information to TRACK.
This is a useful element for `emms-info-functions'."
  (when (and (eq 'file (emms-track-type track))
             (string-match "\\.[Mm][Pp]3\\'" (emms-track-name track)))
    (with-temp-buffer
      (when (zerop
             (apply (if (fboundp 'emms-i18n-call-process-simple)
                        'emms-i18n-call-process-simple
                      'call-process)
                    emms-info-mp3info-program-name
                    nil t nil
                    (append emms-info-mp3find-arguments
                            (list (emms-track-name track)))))
        (goto-char (point-min))
        (while (looking-at "^\\([^=\n]+\\)=\\(.*\\)$")
          (let ((name (intern (match-string 1)))
                (value (match-string 2)))
            (when (> (length value)
                     0)
              (emms-track-set track
                              name
                              (if (eq name 'info-playing-time)
                                  (string-to-number value)
                                value))))
          (forward-line 1))))))

(provide 'emms-info-mp3info)
;;; emms-info-mp3info.el ends here
