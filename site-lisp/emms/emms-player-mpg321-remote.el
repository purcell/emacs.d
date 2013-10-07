;;; emms-player-mpg321-remote.el --- play files with mpg321 -R

;; Copyright (C) 2006, 2007, 2008, 2009  Free Software Foundation, Inc.

;; Author: Damien Elmes <emacs@repose.cx>
;; Keywords: emms, mp3, mpeg, multimedia

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

;;; Commentary:

;; This file provides an emms-player which uses mpg321's remote mode
;; to play files. This is a persistent process which isn't killed each
;; time a new file is played.

;; The remote process copes graciously with errors in music files, and
;; allows you to seek in files.

;; To enable this code, add the following to your emacs configuration:

;; (require 'emms-player-mpg321-remote)
;; (push 'emms-player-mpg321-remote emms-player-list)

;;; Code:

(require 'emms)
(require 'emms-player-simple)

;; --------------------------------------------------
;; Variables and configuration
;; --------------------------------------------------

(defgroup emms-player-mpg321-remote nil
  "*EMMS player using mpg321's remote mode."
  :group 'emms-player
  :prefix "emms-player-mpg321-remote")

(defcustom emms-player-mpg321-remote-command "mpg321"
  "*The command name of mpg321."
  :type  'string
  :group 'emms-player-mpg321-remote)

(defcustom emms-player-mpg321-remote-parameters nil
  "*Extra arguments to pass to mpg321 when using remote mode
For example: (list \"-o\" \"alsa\")"
  :type  '(repeat string)
  :group 'emms-player-mpg321-remote)

(defcustom emms-player-mpg321-remote
  (emms-player 'emms-player-mpg321-remote-start-playing
               'emms-player-mpg321-remote-stop-playing
               'emms-player-mpg321-remote-playable-p)
  "*A player for EMMS."
  :type '(cons symbol alist)
  :group 'emms-player-mpg321-remote)

(defvar emms-player-mpg321-remote-initial-args
  (list "--skip-printing-frames=10" "-R" "-")
  "Initial args to pass to the mpg321 process.")

(defvar emms-player-mpg321-remote-process-name "emms-player-mpg321-remote-proc"
  "The name of the mpg321 remote player process")

(defvar emms-player-mpg321-remote-ignore-stop 0
  "Number of stop messages to ignore, due to user action.")

(defmacro emms-player-mpg321-remote-add (cmd func)
  `(emms-player-set 'emms-player-mpg321-remote
                    ,cmd ,func))

(emms-player-mpg321-remote-add
 'regex (emms-player-simple-regexp "mp3" "mp2"))
(emms-player-mpg321-remote-add
 'pause 'emms-player-mpg321-remote-pause)
(emms-player-mpg321-remote-add
 'resume 'emms-player-mpg321-remote-pause)
(emms-player-mpg321-remote-add
 'seek 'emms-player-mpg321-remote-seek)

;; --------------------------------------------------
;; Process maintenence
;; --------------------------------------------------

(defun emms-player-mpg321-remote-start-process ()
  "Start a new remote process, and return the process."
  (let ((process (apply 'start-process
                        emms-player-mpg321-remote-process-name
                        nil
                        emms-player-mpg321-remote-command
                        (append emms-player-mpg321-remote-initial-args
                                emms-player-mpg321-remote-parameters))))
    (set-process-sentinel process 'emms-player-mpg321-remote-sentinel)
    (set-process-filter process 'emms-player-mpg321-remote-filter)
    process))

(defun emms-player-mpg321-remote-stop ()
  "Stop the currently playing process, if indeed there is one"
  (let ((process (emms-player-mpg321-remote-process)))
    (when process
      (kill-process process)
      (delete-process process))))

(defun emms-player-mpg321-remote-process ()
  "Return the remote process, if it exists."
  (get-process emms-player-mpg321-remote-process-name))

(defun emms-player-mpg321-remote-running-p ()
  "True if the remote process exists and is running."
  (let ((proc (emms-player-mpg321-remote-process)))
    (and proc
         (eq (process-status proc) 'run))))

(defun emms-player-mpg321-remote-sentinel (proc str)
  "Sentinel for determining the end of process"
  (when (or (eq (process-status proc) 'exit)
            (eq (process-status proc) 'signal))
    ;; reset
    (setq emms-player-mpg321-remote-ignore-stop 0)
    (message "Remote process died!")))

(defun emms-player-mpg321-remote-send (text)
  "Send TEXT to the mpg321 remote process, and add a newline."
  (let (proc)
    ;; we shouldn't be trying to send to a dead process
    (unless (emms-player-mpg321-remote-running-p)
      (emms-player-mpg321-remote-start-process))
    (setq proc  (emms-player-mpg321-remote-process))
    (process-send-string proc (concat text "\n"))))

;; --------------------------------------------------
;; Interfacing with emms
;; --------------------------------------------------

(defun emms-player-mpg321-remote-filter (proc str)
  (let* ((data-lines (split-string str "\n" t))
         data line cmd)
    (dolist (line data-lines)
      (setq data (split-string line))
      (setq cmd (car data))
      (cond
       ;; stop notice
       ((and (string= cmd "@P")
             (string= (cadr data) "0"))
        (emms-player-mpg321-remote-notify-emms))
       ;; frame notice
       ((string= cmd "@F")
        ;; even though a timer is constantly updating this variable,
        ;; updating it here will cause it to stay pretty much in sync.
        (run-hook-with-args 'emms-player-time-set-functions
                            (truncate (string-to-number (nth 3 data)))))))))

(defun emms-player-mpg321-remote-start-playing (track)
  "Start playing a song by telling the remote process to play it.
If the remote process is not running, launch it."
  (unless (emms-player-mpg321-remote-running-p)
    (emms-player-mpg321-remote-start-process))
  (emms-player-mpg321-remote-play-track track))

(defun emms-player-mpg321-remote-notify-emms (&optional user-action)
  "Tell emms that the current song has finished.
If USER-ACTION, set `emms-player-mpg321-remote-ignore-stop' so that we
ignore the next message from mpg321."
  (if user-action
      (let ((emms-player-ignore-stop t))
        ;; so we ignore the next stop message
        (setq emms-player-mpg321-remote-ignore-stop
              (1+ emms-player-mpg321-remote-ignore-stop))
        (emms-player-stopped))
    ;; not a user action
    (if (not (zerop emms-player-mpg321-remote-ignore-stop))
        (setq emms-player-mpg321-remote-ignore-stop
              (1- emms-player-mpg321-remote-ignore-stop))
      (emms-player-stopped))))

(defun emms-player-mpg321-remote-stop-playing ()
  "Stop the current song playing."
  (emms-player-mpg321-remote-notify-emms t)
  (emms-player-mpg321-remote-send "stop"))

(defun emms-player-mpg321-remote-play-track (track)
  "Send a play command to the remote, based on TRACK."
  (emms-player-mpg321-remote-send
   (concat "load " (emms-track-get track 'name)))
  (emms-player-started 'emms-player-mpg321-remote))

(defun emms-player-mpg321-remote-playable-p (track)
  ;; use the simple definition.
  (emms-player-mpg321-playable-p track))

(defun emms-player-mpg321-remote-pause ()
  "Pause the player."
  (emms-player-mpg321-remote-send "pause"))

(defun emms-player-mpg321-remote-resume ()
  "Resume the player."
  (emms-player-mpg321-remote-send "pause"))

(defun emms-player-mpg321-remote-seek (seconds)
  "Seek forward or backward in the file."
  ;; since mpg321 only supports seeking by frames, not seconds, we
  ;; make a very rough guess as to how much a second constitutes
  (let ((frame-string (number-to-string (* 35 seconds))))
    ;; if we're not going backwards, we need to add a '+'
    (unless (eq ?- (string-to-char frame-string))
      (setq frame-string (concat "+" frame-string)))
    (emms-player-mpg321-remote-send (concat "jump " frame-string))))

(provide 'emms-player-mpg321-remote)
;;; emms-player-mpg321-remote.el ends here
