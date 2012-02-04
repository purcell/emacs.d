;;; pomodoro.el --- A timer for the Pomodoro Technique 
;;;   (http://www.pomodorotechnique.com)

;; Copyright (C) 2010 Dave Kerschner (docgnome)

;; Author: Dave Kerschner <docgnome@docgno.me>
;; Created: Aug 25, 2010

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(defgroup pomodoro nil
  "Timer for the Pomodoro Technique in emacs"
  :prefix "pomodoro-"
  :group 'tools)

(defcustom pomodoro-work-time 20
  "Length of time in minutes for a work period"
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro-break-time 5
  "Length of time in minutes for a break period"
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro-long-break-time 15
  "Length of time in minutes for a long break period"
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro-nth-for-longer-break 4
  "Number of work cycles before a longer break"
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro-extra-time 2
  "Number of minutes to add onto a timer when avoiding a cycle change"
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro-break-start-message "Break time!"
  "Message show when a break period starts"
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-work-start-message "Back to work slave!"
  "Message to show when a work period starts"
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-long-break-start-message "Time for a longer break!"
  "Message to show when a long break starts"
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-work-cycle "w"
  "String to display in mode line for a work cycle"
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro-break-cycle "b"
  "String to display in mode line for a break cycle"
  :group 'pomodoro
  :type 'string)


(defvar pomodoro-timer nil)
(defvar pomodoros 0)
(defvar pomodoro-current-cycle pomodoro-work-cycle)
(defvar pomodor-mode-line-string "")

(defun pomodoro-epoch (c)
  (+ (* (car c) (expt 2 16)) (cadr c)))

(defun pomodoro-pad-time (a)
  (if (< (length a) 2)
      (concat "0" a)
    a))

(defun pomodoro-seconds-to-time (s)
  (let ((minutes (number-to-string (/ s 60)))
        (seconds (number-to-string (mod s 60))))
    (concat
     (pomodoro-pad-time minutes)
     ":"
     (pomodoro-pad-time seconds))))

(defun pomodoro-set-start-time (s)
  (setq pomodoro-start-time (+ (pomodoro-epoch (current-time)) (* s 60))))

(defun pomodoro-tick ()
  (let ((time (- pomodoro-start-time (pomodoro-epoch (current-time)))))
    (if (<= time 0)
        (if (string= pomodoro-current-cycle pomodoro-work-cycle)
            (let ((p (if (and (not (= pomodoros 0))
                              (= (mod pomodoros pomodoro-nth-for-longer-break) 0))
                         (cons pomodoro-long-break-time pomodoro-long-break-start-message)
                       (cons pomodoro-break-time pomodoro-break-start-message))))
              (if (yes-or-no-p (cdr p))
                  (progn
                    (setq pomodoro-current-cycle pomodoro-break-cycle)
                    (setq pomodoros (incf pomodoros))
                    (pomodoro-set-start-time (car p)))
                (pomodoro-set-start-time pomodoro-extra-time)))
          (if (yes-or-no-p pomodoro-work-start-message)
              (progn
								(setq pomodoro-current-cycle pomodoro-work-cycle)
                (pomodoro-set-start-time pomodoro-work-time))
            (pomodoro-set-start-time pomodoro-extra-time))))
    (setq pomodoro-mode-line-string (concat pomodoro-current-cycle (pomodoro-seconds-to-time time) " "))
    (force-mode-line-update)))

(defun pomodoro-start ()
  (interactive)
  (pomodoro-set-start-time pomodoro-work-time)
  (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-tick)))

(defun pomodoro-stop ()
  (interactive)
  (cancel-timer pomodoro-timer)
  (setq pomodoro-mode-line-string "")
  (setq pomodoro-current-cycle pomodoro-work-cycle)
  (force-mode-line-update))

(setq-default mode-line-format (cons '(pomodoro-mode-line-string pomodoro-mode-line-string) mode-line-format))

(provide 'pomodoro)
