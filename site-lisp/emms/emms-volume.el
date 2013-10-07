;;; emms-volume.el --- Volume functions and a minor mode to adjust volume easily

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
;; along with EMMS; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This file provides generally two things:
;; Generic volume setting functions and some appropriate bindings for EMMS
;; playlist buffers. These can also be bound to global keys,however, the
;; second part may be more useful for this. This part provides functions
;; meant to be bound to a global key (the author uses C-c e + and C-c e -),
;; which then temporarily activates a minor mode allowing you to change the
;; volume with just + and -. This mode deactivates a short (configurable)
;; amount of time after the last volume change. This allows for easier volume
;; adjustment without getting in the way.

;;; History:

;; May 2006: First stab at writing the minor mode.
;;
;; 30 May 2006: Cleanup and restructuring to fit with EMMS.

;;; Todo:

;; Some of this could benefit from adding customize interfaces.

;;; Code:


(require 'emms)
(require 'emms-playlist-mode)
(require 'emms-volume-amixer)

;; Customize group
(defgroup emms-volume nil
  "Volume setting for EMMS."
  :group 'emms)

;; General volume setting related code.
(defcustom emms-volume-change-function 'emms-volume-amixer-change
  "*The function to use to change the volume.
If you have your own functions for changing volume, set this."
  :type '(choice (const :tag "Amixer" emms-volume-amixer-change)
                 (const :tag "MPD" emms-volume-mpd-change)
                 (function :tag "Lisp function"))
  :group 'emms-volume)

(defcustom emms-volume-change-amount 2
  "The amount to use when raising or lowering the volume using the
emms-volume interface.

This should be a positive integer."
  :type 'integer
  :group 'emms-volume)

;;;###autoload
(defun emms-volume-raise ()
  "Raise the speaker volume."
  (interactive)
  (funcall emms-volume-change-function emms-volume-change-amount))

;;;###autoload
(defun emms-volume-lower ()
  "Lower the speaker volume."
  (interactive)
  (funcall emms-volume-change-function (- emms-volume-change-amount)))

(define-key emms-playlist-mode-map (kbd "+") 'emms-volume-raise)
(define-key emms-playlist-mode-map (kbd "-") 'emms-volume-lower)

;; Code specific to the minor mode.
(define-minor-mode emms-volume-minor-mode
  "Allows volume setting with + and - after an initial key combo."
  :global t
  :init-value nil
  :lighter " (+/-)"
  :keymap '(("+" . emms-volume-mode-plus)
            ("-" . emms-volume-mode-minus)))

(defvar emms-volume-mode-timeout 2
  "*The timeout in amount of seconds used by `emms-volume-minor-mode'.")

(defvar emms-volume-mode-timer nil
  "The timer `emms-volume-minor-mode' uses.")

;;;###autoload
(defun emms-volume-mode-plus ()
  "Raise volume and enable or extend the `emms-volume-minor-mode' timeout."
  (interactive)
  (emms-volume-raise)
  (emms-volume-mode-start-or-extend))

;;;###autoload
(defun emms-volume-mode-minus ()
  "Lower volume and enable or extend the `emms-volume-minor-mode' timeout."
  (interactive)
  (emms-volume-lower)
  (emms-volume-mode-start-or-extend))

(defun emms-volume-mode-disable-timer ()
  "Disable `emms-volume-minor-mode' timer."
  (cancel-timer emms-volume-mode-timer)
  (setq emms-volume-mode-timer nil))

(defun emms-volume-mode-set-timer ()
  "Set a new `emms-volume-minor-mode' timer."
  (when emms-volume-mode-timer
    (emms-volume-mode-disable-timer))
  (setq emms-volume-mode-timer (run-at-time emms-volume-mode-timeout
                                            nil
                                            'emms-volume-mode-timer-timeout)))

(defun emms-volume-mode-timer-timeout ()
  "Function to disable `emms-volume-minor-mode' at timeout."
  (setq emms-volume-mode-timer nil)
  (emms-volume-minor-mode -1))

(defun emms-volume-mode-start-or-extend ()
  "Start `emms-volume-minor-mode' or extend its running time."
  (when (null emms-volume-minor-mode)
    (emms-volume-minor-mode 1))
  (emms-volume-mode-set-timer))

(provide 'emms-volume)
;;; emms-volume.el ends here
