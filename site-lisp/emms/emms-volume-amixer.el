;;; emms-volume-amixer.el --- a mode for changing volume using amixer

;; Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Martin Schoenmakers <aiviru@diamond-age.net>

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
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file defines a few simple functions to raise or lower the volume
;; using amixer. It can be used stand-alone, though it's meant for usage
;; with EMMS, particularly with emms-volume.el

;;; History:

;; May 30 2006: First cleanup and collation of amixer functions into a
;;              separate file for releasability.

;;; Todo:

;; There probably needs to be more configurability, which may in turn
;; mean adding some more functions.
;; Some of this could benefit from adding customize interfaces.

;;; Code:

(defcustom emms-volume-amixer-control "Master"
  "The control to change the volume with.
Controls includes \"Master\", \"PCM\", etc. For a full list of available
controls, run `amixer controls' in a shell."
  :type '(choice (const :tag "Master" "Master")
                 (const :tag "PCM" "PCM")
                 (string :tag "Something else: "))
  :group 'emms-volume)

;;;###autoload
(defun emms-volume-amixer-change (amount)
  "Change amixer master volume by AMOUNT."
  (message "Playback channels: %s"
           (with-temp-buffer
             (when (zerop
                    (call-process "amixer" nil (current-buffer) nil
                                  "sset" emms-volume-amixer-control
                                  (format "%d%%%s" (abs amount)
                                          (if (< amount 0) "-" "+"))))
               (if (re-search-backward "\\[\\([0-9]+%\\)\\]" nil t)
                   (match-string 1))))))

(provide 'emms-volume-amixer)

;;; emms-volume-amixer.el ends here
