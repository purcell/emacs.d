;;; emms-history.el -- save all playlists when exiting emacs

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

;; Saves all playlists when you close emacs.  When you start it up again use
;; M-x emms-history-load to restore all saved playlists.

;; To use it put the following into your ~/.emacs:
;;
;;   (require 'emms-history)
;;
;; If all playlists should be restored on startup add this, too:
;;
;;   (emms-history-load)

;;; Code:

(require 'emms)
(eval-when-compile
  (require 'cl))

(defgroup emms-history nil
  "Saving and restoring all playlists when closing/restarting
Emacs."
  :prefix "emms-history-"
  :group 'emms)

(defcustom emms-history-file (concat (file-name-as-directory emms-directory) "history")
  "The file to save playlists in."
  :type   'string
  :group  'emms-history)

(defcustom emms-history-start-playing nil
  "If non-nil emms starts playing the current track after
`emms-history-load' was invoked."
  :type   'boolean
  :group  'emms-history)

(defcustom emms-history-file-coding-system 'utf-8
  "Coding system used for saving `emms-history-file'."
  :type 'coding-system
  :group 'emms-history)

(defun emms-history-save ()
  "Save all playlists that are open in this Emacs session."
  (interactive)
  (when (stringp emms-history-file)
    (let ((oldbuf emms-playlist-buffer)
          ;; print with no limit
          print-length print-level
          emms-playlist-buffer playlists)
      (save-excursion
        (dolist (buf (emms-playlist-buffer-list))
          (set-buffer buf)
          (when (> (buffer-size) 0) ; make sure there is track in the buffer
            (setq emms-playlist-buffer buf
                  playlists
                  (cons
                   (list (buffer-name)
                         (or
                          (and emms-playlist-selected-marker
                               (marker-position emms-playlist-selected-marker))
                          (point-min))
                         (save-restriction
                           (widen)
                           (nreverse
                            (emms-playlist-tracks-in-region (point-min)
                                                            (point-max)))))
                   playlists))))
        (with-temp-buffer
          (insert
           (concat ";;; emms history -*- mode: emacs-lisp; coding: "
                   (symbol-name emms-history-file-coding-system)
                   "; -*-\n"))
          (insert "(\n;; active playlist\n")
          (prin1 (buffer-name oldbuf) (current-buffer))
          (insert "\n;; playlists: ((BUFFER_NAME SELECT_POSITION TRACKS) ...)\n")
          (prin1 playlists (current-buffer))
          (insert "\n;; play method\n")
          (prin1 `((emms-repeat-track . ,emms-repeat-track)
                   (emms-repeat-playlist . ,emms-repeat-playlist))
                 (current-buffer))
          (insert "\n)")
          (write-file emms-history-file))))))

(unless noninteractive
  (add-hook 'kill-emacs-hook 'emms-history-save))

(defun emms-history-load ()
  "Restore all playlists in `emms-history-file'."
  (interactive)
  (when (and (stringp emms-history-file)
             (file-exists-p emms-history-file))
    (let (history buf)
      (with-temp-buffer
        (emms-insert-file-contents emms-history-file)
        (setq history (read (current-buffer)))
        (dolist (playlist (cadr history))
          (with-current-buffer (emms-playlist-new (car playlist))
            (setq emms-playlist-buffer (current-buffer))
            (if (string= (car playlist) (car history))
                (setq buf (current-buffer)))
            (mapc 'emms-playlist-insert-track
                  (nth 2 playlist))
            (ignore-errors
              (emms-playlist-select (cadr playlist)))))
        (setq emms-playlist-buffer buf)
        (dolist (method (nth 2 history))
          (set (car method) (cdr method)))
        (ignore-errors
          (when emms-history-start-playing
            (emms-start)))))))

(provide 'emms-history)
;;; emms-history.el ends here
