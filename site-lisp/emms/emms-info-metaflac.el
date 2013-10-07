;;; emms-info-metaflac.el --- Info-method for EMMS using metaflac

;; Copyright (C) 2006, 2007, 2008, 2009  Free Software Foundation, Inc.

;; Author: Matthew Kennedy <mkennedy@gentoo.org>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301 USA

;;; Commentary:

;; This code has been adapted from code found in emms-info-mp3info.el
;; written by Ulrik Jensen <terryp@daimi.au.dk> which contains the
;; following attribution:

;; This code has been adapted from code found in mp3player.el, written
;; by Jean-Philippe Theberge (jphiltheberge@videotron.ca), Mario
;; Domgoergen (kanaldrache@gmx.de) and Jorgen Schäfer
;; <forcer@forcix.cx>

;; To activate this method for getting info, use something like:

;; (require 'emms-info-metaflac)
;; (add-to-list 'emms-info-methods-list 'emms-info-metaflac)

;;; Code:

(eval-when-compile (require 'cl))
(require 'emms-info)

(defvar emms-info-metaflac-version "0.1 $Revision: 1.10 $"
  "EMMS info metaflac version string.")

;; $Id: emms-info-mp3info.el,v 1.10 2005/08/12 18:01:16 xwl Exp $

(defgroup emms-info-metaflac nil
  "An EMMS-info method for getting/setting FLAC tags, using the
external metaflac program"
  :group 'emms-info)

(defcustom emms-info-metaflac-program-name "metaflac"
  "*The name/path of the metaflac program."
  :type 'string
  :group 'emms-info-metaflac)

(defcustom emms-info-metaflac-options
  '("--no-utf8-convert"
    "--show-tag=TITLE"
    "--show-tag=ARTIST"
    "--show-tag=ALBUM"
    "--show-tag=NOTE"
    "--show-tag=YEAR"
    "--show-tag=TRACKNUMBER"
    "--show-tag=DISCNUMBER"
    "--show-tag=GENRE")
  "The argument to pass to `emms-info-metaflac-program-name'."
  :type '(repeat string)
  :group 'emms-info-metaflac)

(defun emms-info-metaflac (track)
  "Get the FLAC tag of file TRACK, using `emms-info-metaflac-program'
and return an emms-info structure representing it."
  (when (and (eq 'file (emms-track-type track))
             (string-match "\\.\\(flac\\|FLAC\\)\\'" (emms-track-name track)))
    (with-temp-buffer
      (when (zerop
             (apply 'call-process
              emms-info-metaflac-program-name
              nil t nil
              "--show-total-samples"
              "--show-sample-rate"
              (append emms-info-metaflac-options
                      (list (emms-track-name track)))))
        (goto-char (point-min))
        (emms-track-set track 'info-playing-time
                        (/ (string-to-number (buffer-substring (point) (line-end-position)))
                           (progn
                             (forward-line 1)
                             (string-to-number (buffer-substring (point) (line-end-position))))))
        (forward-line 1)
        (while (looking-at "^\\([^=\n]+\\)=\\(.*\\)$")
          (let ((name (intern (concat "info-" (downcase (match-string 1)))))
                (value (match-string 2)))
            (when (> (length value)
                     0)
              (emms-track-set track
                              name
                              (if (eq name 'info-playing-time)
                                  (string-to-number value)
                                value))))
          (forward-line 1))))))

(provide 'emms-info-metaflac)

;;; emms-info-metaflac.el ends here
