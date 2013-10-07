;;; emms-mode-line.el --- Mode-Line and titlebar infos for emms

;; Copyright (C) 2004, 2005, 2006, 2007, 2008,
;;   2009 Free Software Foundation, Inc.

;; Author: Mario Domgörgen <kanaldrache@gmx.de>
;; Keywords: multimedia

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
;;
;; To activate put simply the following line in your Emacs:
;;
;;   (require 'emms-mode-line)
;;   (emms-mode-line 1)

;;; Code:

(require 'emms)

(defgroup emms-mode-line nil
  "Showing information on mode-line and titlebar"
  :prefix "emms-mode-line-"
  :group 'emms)

(defcustom emms-mode-line-mode-line-function 'emms-mode-line-playlist-current
  "Function for showing infos in mode-line or nil if don't want to."
  :type '(choice (const :tag "Don't show info on mode-line" nil) function)
  :group 'emms-mode-line)

(defcustom emms-mode-line-titlebar-function nil
  "Function for showing infos in titlebar or nil if you don't want to."
  :type '(choice (const :tag "Don't show info on titlebar" nil) function)
  :group 'emms-mode-line)

(defcustom emms-mode-line-format " [ %s ] "
  "String used for displaying the current track in mode-line and titlebar."
  :type 'string
  :group 'emms)

(defun emms-mode-line-playlist-current ()
  "Format the currently playing song."
  (format emms-mode-line-format (emms-track-description
				 (emms-playlist-current-selected-track))))

(defvar emms-mode-line-active-p nil
  "If non-nil, emms mode line is active.")
(defvar emms-mode-line-string "")

(defvar emms-mode-line-initial-titlebar frame-title-format)

(defun emms-mode-line (arg)
  "Turn on `emms-mode-line' if ARG is positive, off otherwise."
  (interactive "p")
  (or global-mode-string (setq global-mode-string '("")))
  (if (and arg (> arg 0))
      (progn
        (setq emms-mode-line-active-p t)
  	(add-hook 'emms-track-updated-functions 'emms-mode-line-alter)
	(add-hook 'emms-player-finished-hook 'emms-mode-line-blank)
	(add-hook 'emms-player-stopped-hook 'emms-mode-line-blank)
	(add-hook 'emms-player-started-hook 'emms-mode-line-alter)
	(when (and emms-mode-line-mode-line-function
		   (not (member 'emms-mode-line-string global-mode-string)))
	  (setq global-mode-string
		(append global-mode-string
			'(emms-mode-line-string))))
	(when emms-player-playing-p (emms-mode-line-alter)))
    (setq emms-mode-line-active-p nil)
    (remove-hook 'emms-track-updated-functions 'emms-mode-line-alter)
    (remove-hook 'emms-player-finished-hook 'emms-mode-line-blank)
    (remove-hook 'emms-player-stopped-hook 'emms-mode-line-blank)
    (remove-hook 'emms-player-started-hook 'emms-mode-line-alter)
    (emms-mode-line-restore-titlebar)
    (emms-mode-line-restore-mode-line)))

;;;###autoload
(defun emms-mode-line-enable ()
  "Turn on `emms-mode-line'."
  (interactive)
  (emms-mode-line 1)
  (message "emms mode line enabled"))

;;;###autoload
(defun emms-mode-line-disable ()
  "Turn off `emms-mode-line'."
  (interactive)
  (emms-mode-line -1)
  (message "emms mode line disabled"))

;;;###autoload
(defun emms-mode-line-toggle ()
  "Toggle `emms-mode-line'."
  (interactive)
  (if emms-mode-line-active-p
      (emms-mode-line-disable)
    (emms-mode-line-enable)))

(defun emms-mode-line-alter (&optional track)
  "Alter mode-line/titlebar.

Optional TRACK is used to be compatible with
`emms-track-updated-functions'. It's simply ignored currently."
  (emms-mode-line-alter-mode-line)
  (emms-mode-line-alter-titlebar))

(defun emms-mode-line-alter-mode-line ()
  "Update the mode-line with song info."
  (when (and emms-mode-line-mode-line-function
	     emms-player-playing-p)
    (setq emms-mode-line-string
	  (funcall emms-mode-line-mode-line-function))
    (force-mode-line-update)))

(defun emms-mode-line-alter-titlebar ()
  "Update the titlebar with song info."
  (when emms-mode-line-titlebar-function
    (setq frame-title-format
	  (list "" emms-mode-line-initial-titlebar (funcall emms-mode-line-titlebar-function)))))


(defun emms-mode-line-blank ()
  "Blank mode-line and titlebar but not quit `emms-mode-line'."
  (setq emms-mode-line-string nil)
  (force-mode-line-update)
  (emms-mode-line-restore-titlebar))

(defun emms-mode-line-restore-mode-line ()
  "Restore the mode-line."
  (when emms-mode-line-mode-line-function
    (setq global-mode-string
	  (remove 'emms-mode-line-string global-mode-string))
    (force-mode-line-update)))

(defun emms-mode-line-restore-titlebar ()
  "Restore the mode-line."
  (when emms-mode-line-titlebar-function
    (setq frame-title-format
	  (list emms-mode-line-initial-titlebar))))

(provide 'emms-mode-line)
;;; emms-mode-line.el ends here
