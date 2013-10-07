;;; emms-setup.el --- Setup script for EMMS

;; Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>
;; Keywords: emms setup multimedia

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
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides the `emms-setup' feature. With `emms-setup' we
;; can setup Emms with different features enabled. The use of this
;; feature is documented in the Emms manual which is distributed with
;; Emms.
;;
;; The use this feature we can invoke (for example):
;;
;;         (require 'emms-setup)
;;         (emms-all)
;;
;; The first command loads the feature into Emacs and the second
;; chooses the `emms-all' level.

;;; Code:

(require 'emms)

(defgroup emms-setup nil
  "*The Emacs Multimedia System setup utility."
  :prefix "emms-setup"
  :group 'multimedia)

(defcustom emms-setup-default-player-list
  '(emms-player-mpg321
    emms-player-ogg123
    emms-player-mplayer-playlist
    emms-player-mplayer
    emms-player-vlc)
  "*Default list of players for emms-setup."
  :group 'emms-setup
  :type 'list)

;;;###autoload
(defun emms-minimalistic ()
  "An Emms setup script.
Invisible playlists and all the basics for playing media."
  (require 'emms-source-file)
  (require 'emms-source-playlist)
  (require 'emms-player-simple)
  (require 'emms-player-mplayer)
  (require 'emms-player-vlc))

;;;###autoload
(defun emms-standard ()
  "An Emms setup script.
Everything included in the `emms-minimalistic' setup, the Emms
interactive playlist mode, reading information from tagged
audio files, and a metadata cache."
  ;; include
  (emms-minimalistic)
  ;; define
  (eval-and-compile
    (require 'emms-playlist-mode)
    (require 'emms-info)
    (require 'emms-info-mp3info)
    (require 'emms-info-ogginfo)
    (require 'emms-cache))
  ;; setup
  (setq emms-playlist-default-major-mode 'emms-playlist-mode)
  (add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)
  (when (executable-find emms-info-mp3info-program-name)
    (add-to-list 'emms-info-functions 'emms-info-mp3info))
  (when (executable-find emms-info-ogginfo-program-name)
    (add-to-list 'emms-info-functions 'emms-info-ogginfo))
  (setq emms-track-description-function 'emms-info-track-description)
  (when (fboundp 'emms-cache)           ; work around compiler warning
    (emms-cache 1)))

;;;###autoload
(defun emms-all ()
  "An Emms setup script.
Everything included in the `emms-standard' setup and adds all the
stable features which come with the Emms distribution."
  ;; include
  (emms-standard)
  ;; define
  (eval-and-compile
    (require 'emms-mode-line)
    (require 'emms-mark)
    (require 'emms-tag-editor)
    (require 'emms-streams)
    (require 'emms-lyrics)
    (require 'emms-playing-time)
    (require 'emms-player-mpd)
    (require 'emms-player-xine)
    (require 'emms-playlist-sort)
    (require 'emms-browser)
    (require 'emms-mode-line-icon)
    (require 'emms-cue)
    (require 'emms-lastfm-client)
    (require 'emms-bookmarks)
    (require 'emms-last-played))
  ;; setup
  (emms-mode-line 1)
  (emms-mode-line-blank)
  (emms-lyrics 1)
  (emms-playing-time 1)
  (add-to-list 'emms-info-functions 'emms-info-cueinfo)
  (add-hook 'emms-player-started-hook 'emms-last-played-update-current))

;;;###autoload
(defun emms-devel ()
  "An Emms setup script.
Everything included in the `emms-all' setup and adds all the
features which come with the Emms distribution regardless of if
they are considered stable or not.  Use this if you like living
on the edge."
  ;; include
  (emms-all)
  ;; define
  (eval-and-compile
    (require 'emms-metaplaylist-mode)
    (require 'emms-stream-info)
    (require 'emms-score)
    (require 'emms-history)
    (require 'emms-i18n)
    (require 'emms-volume)
    (require 'emms-playlist-limit))
  ;; setup
  (emms-score 1)
  (emms-playlist-limit 1))

;;;###autoload
(defun emms-default-players ()
  "Set `emms-player-list' to `emms-setup-default-player-list'."
  (setq emms-player-list
	emms-setup-default-player-list))

(provide 'emms-setup)
;;; emms-setup.el ends here
