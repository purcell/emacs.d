;;; emms-mark.el --- mark track like dired

;; Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@163.com>

;; This file is part of EMMS.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Provide mark operation to tracks

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'emms-mark)

;; To activate it for the current buffer only, do:
;;   (emms-mark-mode)

;; To make this the default EMMS mode, do:
;;   (setq emms-playlist-default-major-mode 'emms-mark-mode)

;;; Code:

(provide 'emms-mark)
(require 'emms)
(require 'emms-playlist-mode)
(eval-when-compile
  (require 'cl))

;;{{{  set new description-function
(defun emms-mark-track-description (track)
  "Return a description of the current track."
  (assert (not (eq (default-value 'emms-track-description-function)
                   'emms-mark-track-description))
          nil (concat "Do not set `emms-track-selection-function' to be"
                      " emms-mark-track-description."))
  (concat "  " (funcall (default-value 'emms-track-description-function)
                        track)))

(defun emms-mark-update-descriptions ()
  "Update the track descriptions in the current buffer."
  (emms-with-inhibit-read-only-t
   (save-excursion
     (goto-char (point-min))
     (emms-walk-tracks
       (emms-playlist-update-track)))))
;;}}}

;;{{{ functions to mark tracks
(defvar emms-mark-char ?*)
(defvar emms-mark-face-alist
  '((?* . font-lock-warning-face)
    (?\040 . emms-playlist-track-face)))

(defun emms-mark-track (&optional arg)
  "Mark the current track.
If ARG is positive, also mark the next ARG-1 tracks as well.
If ARG is negative, also mark the previous ARG-1 tracks."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((face (assoc-default emms-mark-char emms-mark-face-alist))
        buffer-read-only track)
    (save-excursion
      (beginning-of-line)
      (while (and (not (eobp))
                  (/= arg 0))
        (setq track (get-text-property (point) 'emms-track))
        (delete-char 1)
        (insert (emms-propertize (string emms-mark-char)
                                 'emms-track track))
        (backward-char 1)
        (if (> arg 0)
            ;; Propertizing forward...
            (put-text-property (point)
                               (progn (forward-line 1) (point))
                               'face face)
          ;; ... and backward
          (let ((start (save-excursion (end-of-line) (point))))
            (put-text-property (progn (beginning-of-line) (point))
                               start
                               'face face))
          (forward-line -1))
        (setq arg (if (> arg 0)
                      (1- arg)
                    (1+ arg)))))))

(defun emms-mark-unmark-track (&optional arg)
  "Unmark the current track.
If ARG is positive, also unmark the next ARG-1 tracks as well.
If ARG is negative, also unmark the previous ARG-1 tracks."
  (interactive "p")
  (let ((emms-mark-char ?\040))
    (emms-mark-track arg)))

(defun emms-mark-forward (arg)
  "Mark one or more tracks and move the point past the newly-marked tracks.
See `emms-mark-track' for further details."
  (interactive "p")
  (emms-mark-track arg)
  (forward-line arg))

(defun emms-mark-unmark-forward (arg)
  "Unmark one or more tracks and move the point past the tracks.
See `emms-mark-unmark-track' for further details."
  (interactive "p")
  (emms-mark-unmark-track arg)
  (forward-line arg))

(defun emms-mark-all ()
  "Mark all tracks in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (emms-mark-track (count-lines (point-min) (point-max)))))

(defun emms-mark-unmark-all ()
  "Unmark all tracks in the current buffer."
  (interactive)
  (emms-mark-do-with-marked-track 'emms-mark-unmark-track))

(defun emms-mark-regexp (regexp arg)
  "Mark all tracks matching REGEXP. A prefix argument means to
unmark them instead."
  (interactive
   (list
    (read-from-minibuffer (if current-prefix-arg
                              "Mark tracks matching: "
                            "Unmark tracks matching: "))
    current-prefix-arg))
  (let ((emms-mark-char (if arg ?\040 ?*)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (emms-mark-track 1)
        (forward-line 1)))))

(defun emms-mark-toggle ()
  "Toggle all marks in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (buffer-read-only)
      (while (not (eobp))
        (if (eq ?\040 (following-char))
            (emms-mark-track)
          (emms-mark-unmark-track))
        (forward-line 1)))))

(defsubst emms-mark-has-markedp ()
  "Return non-nil if the playlist has a marked line, nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "^[%c]" emms-mark-char) nil t)))

;;}}}

;;{{{ functions to operate marked tracks
(defun emms-mark-do-with-marked-track (func &optional move)
  "Call FUNC on every marked line in current playlist.
The function specified by FUNC takes no argument, so if the track
on the marked line is needed, use `emms-playlist-track-at' to get
it.

The function can also modify the playlist buffer, such as
deleting the current line.  If the function doesn't move forward,
be sure to set the second parameter MOVE to non-nil.  Otherwise
the function will never exit the loop."
  (let ((regexp (format "^[%c]" emms-mark-char))
        (newfunc func))
    (if move
        (setq newfunc (lambda () (funcall func) (forward-line 1))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (backward-char 1)               ; move to beginning of line
        (funcall newfunc)))))

(defun emms-mark-mapcar-marked-track (func &optional move)
  "This function does the same thing as
`emms-mark-do-with-marked-track', the only difference being that
this function collects the result of FUNC."
  (let ((regexp (format "^[%c]" emms-mark-char))
        result (newfunc func))
    (if move
        (setq newfunc (lambda () (let ((res (funcall func)))
                                   (forward-line 1) res))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (backward-char 1)               ; move to beginning of line
        (setq result (cons (funcall newfunc) result)))
      (nreverse result))))

(defun emms-mark-delete-marked-tracks ()
  "Delete all tracks that have been marked in the current buffer."
  (interactive)
  (emms-with-inhibit-read-only-t
   (emms-mark-do-with-marked-track
    (lambda nil (delete-region (point)
                               (progn (forward-line 1) (point)))))))

(defun emms-mark-kill-marked-tracks ()
  "Kill all tracks that have been marked in the current buffer."
  (interactive)
  (let (tracks buffer-read-only)
    (emms-mark-do-with-marked-track
     (lambda nil
       (setq tracks
             (concat tracks
                     (delete-and-extract-region (point)
                                                (progn (forward-line 1) (point)))))))
    (kill-new tracks)))

(defun emms-mark-copy-marked-tracks ()
  "Copy all tracks that have been marked in the current buffer."
  (interactive)
  (let (tracks)
    (emms-mark-do-with-marked-track
     (lambda nil
       (setq tracks
             (concat tracks
                     (buffer-substring (point)
                                       (progn (forward-line 1) (point)))))))
    (kill-new tracks)))
;;}}}

;;{{{ mode stuff
(defconst emms-mark-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "W" 'emms-mark-copy-marked-tracks)
    (define-key map "K" 'emms-mark-kill-marked-tracks)
    (define-key map "D" 'emms-mark-delete-marked-tracks)
    (define-key map "m" 'emms-mark-forward)
    (define-key map "u" 'emms-mark-unmark-forward)
    (define-key map "U" 'emms-mark-unmark-all)
    (define-key map "t" 'emms-mark-toggle)
    (define-key map "%m" 'emms-mark-regexp)
    map)
  "Keymap for `emms-mark-mode'.")

(defun emms-mark-mode ()
  "An EMMS major mode that allows tracks to be marked like dired.
\\{emms-mark-mode-map}"
  (interactive)
  (if (eq major-mode 'emms-mark-mode)
      ;; do nothing if we're already in emms-mark-mode
      nil

    ;; start emms-playlist-mode exactly once
    (setq emms-playlist-buffer-p t)
    (unless (eq major-mode 'emms-playlist-mode)
      (emms-playlist-mode))

    ;; use inherited keymap
    (set-keymap-parent emms-mark-mode-map (current-local-map))
    (use-local-map emms-mark-mode-map)
    (setq major-mode 'emms-mark-mode
          mode-name "Emms-Mark")

    ;; show a blank space at beginning of each line
    (set (make-local-variable 'emms-track-description-function)
         'emms-mark-track-description)
    (emms-mark-update-descriptions)))

(defun emms-mark-mode-disable ()
  "Disable `emms-mark-mode' and return to `emms-playlist-mode'."
  (interactive)
  (if (not (eq major-mode 'emms-mark-mode))
      ;; do nothing if we're not in emms-mark-mode
      nil

    ;; call emms-playlist-mode, saving important variables
    (let ((selected emms-playlist-selected-marker))
      (emms-playlist-mode)
      (setq emms-playlist-selected-marker selected)
      (emms-playlist-mode-overlay-selected))

    ;; update display
    (emms-mark-update-descriptions)))
;;}}}

;;; emms-mark.el ends here
