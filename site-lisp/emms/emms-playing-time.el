;;; emms-playing-time.el --- Display emms playing time on mode line

;; Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: William Xu <william.xwl@gmail.com>

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
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Display playing time on mode line, it looks like: 01:32/04:09.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;     (require 'emms-playing-time)
;;     (emms-playing-time 1)

;; Note: `(emms-playing-time -1)' will disable emms-playing-time module
;; completely, and is not recommended. (since some other emms modules
;; may rely on it, such as `emms-lastfm.el')

;; Instead, to toggle displaying playing time on mode line, one could
;; call `emms-playing-time-enable-display' and
;; `emms-playing-time-disable-display'."

;;; Code:

(eval-when-compile (require 'cl))
(require 'emms-info)
(require 'emms-player-simple)

;;; Customizations

(defgroup emms-playing-time nil
  "Playing-time module for EMMS."
  :group 'emms)

(defcustom emms-playing-time-display-short-p nil
  "Non-nil will only display elapsed time.
e.g., display 02:37 instead of 02:37/05:49."
  :type 'boolean
  :group 'emms-playing-time)

(defcustom emms-playing-time-display-format " %s "
  "Format used for displaying playing time."
  :type 'string
  :group 'emms-playing-time)

(defcustom emms-playing-time-style 'time
  "Style used for displaying playing time.
Valid styles are `time' (e.g., 01:30/4:20) and `bar' (e.g., [===>  ])."
  :type 'symbol
  :group 'emms-playing-time)


;;; Emms Playing Time

(defvar emms-playing-time-display-p nil
  "Whether to display playing time on mode line or not")

(defvar emms-playing-time 0
  "Time elapsed in current track.")

(defvar emms-playing-time-string "")

(defvar emms-playing-time-display-timer nil)

(defvar emms-playing-time-p nil
  "Whether emms-playing-time module is enabled or not")

(defun emms-playing-time-start ()
  "Get ready for display playing time."
  (setq emms-playing-time 0)
  (unless emms-playing-time-display-timer
    (setq emms-playing-time-display-timer
	  (run-at-time t 1 'emms-playing-time-display))))

(defun emms-playing-time-stop ()
  "Remove playing time on the mode line."
  (if (or (not emms-player-paused-p)
	  emms-player-stopped-p)
      (progn
	(setq emms-playing-time-string "")
	(force-mode-line-update)))
  (emms-cancel-timer emms-playing-time-display-timer)
  (setq emms-playing-time-display-timer nil))

(defun emms-playing-time-pause ()
  "Pause playing time."
  (if emms-player-paused-p
      (emms-playing-time-stop)
    (unless emms-playing-time-display-timer
      (setq emms-playing-time-display-timer
	    (run-at-time t 1 'emms-playing-time-display)))))

(defun emms-playing-time-seek (sec)
  "Seek forward or backward SEC playing time."
  (setq emms-playing-time (+ emms-playing-time sec))
  (when (< emms-playing-time 0)		; back to start point
    (setq emms-playing-time 0)))

(defun emms-playing-time-set (sec)
  "Set the playing time to SEC."
  (setq emms-playing-time sec)
  (when (< emms-playing-time 0)		; back to start point
    (setq emms-playing-time 0)))

(defun emms-playing-time (arg)
  "Turn on emms playing time if ARG is positive, off otherwise.

Note: `(emms-playing-time -1)' will disable emms-playing-time
module completely, and is not recommended. (since some other emms
modules may rely on it, such as `emms-lastfm.el')

Instead, to toggle displaying playing time on mode line, one
could call `emms-playing-time-enable-display' and
`emms-playing-time-disable-display'."
  (if (and arg (> arg 0))
      (progn
	(setq emms-playing-time-p t
              emms-playing-time-display-p t)
	(emms-playing-time-mode-line)
	(add-hook 'emms-player-started-hook       'emms-playing-time-start)
	(add-hook 'emms-player-stopped-hook       'emms-playing-time-stop)
	(add-hook 'emms-player-finished-hook      'emms-playing-time-stop)
	(add-hook 'emms-player-paused-hook        'emms-playing-time-pause)
	(add-hook 'emms-player-seeked-functions   'emms-playing-time-seek)
	(add-hook 'emms-player-time-set-functions 'emms-playing-time-set))
    (setq emms-playing-time-p nil
          emms-playing-time-display-p nil)
    (emms-playing-time-stop)
    (emms-playing-time-restore-mode-line)
    (remove-hook 'emms-player-started-hook       'emms-playing-time-start)
    (remove-hook 'emms-player-stopped-hook       'emms-playing-time-stop)
    (remove-hook 'emms-player-finished-hook      'emms-playing-time-stop)
    (remove-hook 'emms-player-paused-hook        'emms-playing-time-pause)
    (remove-hook 'emms-player-seeked-functions   'emms-playing-time-seek)
    (remove-hook 'emms-player-time-set-functions 'emms-playing-time-set)))

;;;###autoload
(defun emms-playing-time-enable-display ()
  "Display playing time on mode line."
  (interactive)
  (setq emms-playing-time-display-p t))

;;;###autoload
(defun emms-playing-time-disable-display ()
  "Remove playing time from mode line."
  (interactive)
  (setq emms-playing-time-display-p nil))

(defun emms-playing-time-display ()
  "Display playing time on the mode line."
  (setq emms-playing-time (round (1+ emms-playing-time)))
  (setq emms-playing-time-string "")
  (when emms-playing-time-display-p
    (let* ((min (/ emms-playing-time 60))
           (sec (% emms-playing-time 60))
           (total-playing-time
            (or (emms-track-get
                 (emms-playlist-current-selected-track)
                 'info-playing-time)
                0))
           (total-min-only (/ total-playing-time 60))
           (total-sec-only (% total-playing-time 60)))
      (case emms-playing-time-style
        ((bar)                          ; `bar' style
         (if (zerop total-playing-time)
             (setq emms-playing-time-string "[==>........]")
           (let ((progress "[")
                 ;; percent based on 10
                 (percent (/ (* emms-playing-time 10) total-playing-time)))
             (dotimes (i percent)
               (setq progress (concat progress "=")))
             (setq progress (concat progress ">"))
             (dotimes (i (- 10 percent))
               (setq progress (concat progress " ")))
             (setq progress (concat progress "]"))
             (setq emms-playing-time-string progress))))
        (t                              ; `time' style
         (setq emms-playing-time-string
               (emms-replace-regexp-in-string
                " " "0"
                (if (or emms-playing-time-display-short-p
                        ;; unable to get total playing-time
                        (eq total-playing-time 0))
                    (format "%2d:%2d" min sec)
                  (format "%2d:%2d/%2s:%2s"
                          min sec total-min-only total-sec-only))))))
      (setq emms-playing-time-string
            (format emms-playing-time-display-format
                    emms-playing-time-string))))
  (force-mode-line-update))

(defun emms-playing-time-mode-line ()
  "Add playing time to the mode line."
  (or global-mode-string (setq global-mode-string '("")))
  (unless (member 'emms-playing-time-string
		  global-mode-string)
    (setq global-mode-string
	  (append global-mode-string
		  '(emms-playing-time-string)))))

(defun emms-playing-time-restore-mode-line ()
  "Restore the mode line."
  (setq global-mode-string
	(remove 'emms-playing-time-string global-mode-string))
  (force-mode-line-update))

(provide 'emms-playing-time)

;;; emms-playing-time.el ends here
