;;; emms-player-mpd.el --- MusicPD support for EMMS

;; Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Michael Olson <mwolson@gnu.org>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Benefits of MusicPD

;; MusicPD features crossfade, very little skipping, minor CPU usage,
;; many clients, many supported output formats, fast manipulation via
;; network processes, and good abstraction of client and server.

;;; MusicPD setup

;; If you want to set up a local MusicPD server, you'll need to have
;; mpd installed.  If you want to use a remote server instance, no
;; installation is needed.

;; The website is at http://musicpd.org/.  Debian packages are
;; available.  It is recommended to use mpd version 0.12.0 or higher.
;;
;; Copy the example configuration for mpd into ~/.mpdconf and edit it
;; to your needs.  Use your top level music directory for
;; music_directory.  If your playlists use absolute file names, be
;; certain that music_directory has the leading directory part.
;;
;; Before you try to play anything, but after setting up the above,
;; run `mkdir ~/.mpd && mpd --create-db' to create MusicPD's track
;; database.
;;
;; Check to see if mpd is running.  It must be running as a daemon for
;; you to be able to play anything.  Launch it by executing "mpd".  It
;; can be killed later with "mpd --kill" (or just "killall mpd" if
;; you're not using the latest development version).

;;; EMMS setup

;; Add the following to your config.
;;
;; (require 'emms-player-mpd)

;; Adjust `emms-player-mpd-server-name' and
;; `emms-player-mpd-server-port' to match the location and port of
;; your MusicPD server.
;;
;;   (setq emms-player-mpd-server-name "localhost")
;;   (setq emms-player-mpd-server-port "6600")

;; If your MusicPD setup requires a password, you will need to do the
;; following.
;;
;;   (setq emms-player-mpd-server-password "mypassword")

;; To get track info from MusicPD, do the following.
;;
;;   (add-to-list 'emms-info-functions 'emms-info-mpd)

;; To change the volume using MusicPD, do the following.
;;
;;   (setq emms-volume-change-function 'emms-volume-mpd-change)

;; Add 'emms-player-mpd to the top of `emms-player-list'.
;;
;;   (add-to-list 'emms-player-list 'emms-player-mpd)

;; If you use absolute file names in your m3u playlists (which is most
;; likely), make sure you set `emms-player-mpd-music-directory' to the
;; value of "music_directory" from your MusicPD config.  There are
;; additional options available as well, but the defaults should be
;; sufficient for most uses.

;; You can set `emms-player-mpd-sync-playlist' to nil if your master
;; EMMS playlist contains only stored playlists.

;; If at any time you wish to replace the current EMMS playlist buffer
;; with the contents of the MusicPD playlist, type
;; M-x emms-player-mpd-connect.
;;
;; This will also run the relevant seek functions, so that if you use
;; emms-playing-time, the displayed time will be accurate.

;;; Contributors

;; Adam Sjøgren implemented support for changing the volume.

(require 'emms-player-simple)
(require 'emms-source-playlist)  ; for emms-source-file-parse-playlist
(require 'tq)

(eval-when-compile
  (condition-case nil
      (progn
        (require 'url)           ; load if available
        (require 'emms-url))
    (error nil)))

(defgroup emms-player-mpd nil
  "EMMS player for MusicPD."
  :group 'emms-player
  :prefix "emms-player-mpd-")

(defcustom emms-player-mpd (emms-player 'emms-player-mpd-start
                                        'emms-player-mpd-stop
                                        'emms-player-mpd-playable-p)
 "*Parameters for the MusicPD player."
 :type '(cons symbol alist)
 :group 'emms-player-mpd)

(defcustom emms-player-mpd-music-directory nil
  "The value of 'music_directory' in your MusicPD configuration file.

You need this if your playlists use absolute file names, otherwise
leave it set to nil."
  ;; The :format part ensures that entering directories happens on the
  ;; next line, where there is more space to work with
  :type '(choice :format "%{%t%}:\n   %[Value Menu%] %v"
                 (const nil)
                 directory)
  :group 'emms-player-mpd)

(defun emms-player-mpd-get-supported-regexp ()
  "Returns a regexp of file extensions that MusicPD supports,
or nil if we cannot figure it out."
  (let ((out (shell-command-to-string "mpd --version")))
    ;; 0.17.x
    (if (string-match "Decoders plugins:$" out)
        (let* ((b (match-end 0))
               (e (string-match "Output plugins:$" out))
               (plugs (split-string (substring out b e) "\n" t))
               (plugs (mapcan (lambda (x)
                                (and (string-match " +\\[.*\\] +\\(.+\\)$" x)
                                     (split-string (match-string 1 x) nil t)))
                              plugs))
               (b (and (string-match "Protocols:$" out) (match-end 0)))
               (prots (and b (substring out (+ 2 b) -1)))
               (prots (split-string (or prots "") nil t)))
          (concat "\\(\\.\\(m3u\\|pls\\|"
                  (regexp-opt (delq nil plugs))
                  "\\)\\'\\)\\|\\(\\`"
                  (regexp-opt (delete "file://" prots)) "\\)"))
      (let ((found-start nil)
            (supported nil))
        (if (string-match "Supported decoders:\\([^0]+?\\)Supported outputs:" out)
            ;; 0.15.x
            (setq supported (replace-regexp-in-string "\\[.+?\\]" ""
                                                      (match-string 1 out)))
          ;; < 0.15
          (setq out (split-string out "\n"))
          (while (car out)
            (cond ((string= (car out) "Supported formats:")
                   (setq found-start t))
                  ((string= (car out) "")
                   (setq found-start nil))
                  (found-start
                   (setq supported (concat supported (car out)))))
            (setq out (cdr out))))
        ;; Create regexp
        (when (and (stringp supported)
                   (not (string= supported "")))
          (concat "\\`http://\\|\\.\\(m3u\\|pls\\|"
                  (regexp-opt (delq nil (split-string supported)))
                  "\\)\\'"))))))

(defcustom emms-player-mpd-supported-regexp
  ;; Use a sane default, just in case
  (or (emms-player-mpd-get-supported-regexp)
      (concat "\\`http://\\|"
              (emms-player-simple-regexp
               "m3u" "ogg" "flac" "mp3" "wav" "mod" "au" "aiff")))
  "Formats supported by MusicPD."
  :type 'regexp
  :set (function
        (lambda (sym value)
          (set sym value)
          (emms-player-set emms-player-mpd 'regex value)))
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-connect-function 'open-network-stream
  "Function used to initiate the connection to MusicPD.
It should take same arguments as `open-network-stream' does."
  :type 'function
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-server-name (or (getenv "MPD_HOST") "localhost")
  "The MusicPD server that we should connect to."
  :type 'string
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-server-port (or (getenv "MPD_PORT") "6600")
  "The port of the MusicPD server that we should connect to."
  :type '(choice number string)
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-server-password nil
  "The password for the MusicPD server that we should connect to."
  :type '(choice (const :tag "None" nil)
                 string)
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-check-interval 1
  "How often to check to see whether MusicPD has advanced to the
next song.  This may be an integer, a floating point number, or
nil.  If set to nil, this check will not be periodically
performed.

This variable is used only if `emms-player-mpd-sync-playlist' is
non-nil."
  :type '(choice (const :tag "Disable check" nil)
                 number)
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-verbose nil
  "Whether to provide notifications for server connection events
and errors."
  :type 'boolean
  :group 'emms-player-mpd)

(defcustom emms-player-mpd-sync-playlist t
  "Whether to synchronize the EMMS playlist with the MusicPD playlist.

If your EMMS playlist contains music files rather than playlists,
leave this set to non-nil.

If your EMMS playlist contains stored playlists, set this to nil."
  :type 'boolean
  :group 'emms-player-mpd)

(emms-player-set emms-player-mpd
                 'regex
                 emms-player-mpd-supported-regexp)

(emms-player-set emms-player-mpd
                 'pause
                 'emms-player-mpd-pause)

(emms-player-set emms-player-mpd
                 'resume
                 'emms-player-mpd-pause)

(emms-player-set emms-player-mpd
                 'seek
                 'emms-player-mpd-seek)

(emms-player-set emms-player-mpd
                 'seek-to
                 'emms-player-mpd-seek-to)

;;; Dealing with the MusicPD network process

(defvar emms-player-mpd-process nil)
(defvar emms-player-mpd-queue nil)

(defvar emms-player-mpd-playlist-id nil)
(make-variable-buffer-local 'emms-player-mpd-playlist-id)

(defvar emms-player-mpd-current-song nil)
(defvar emms-player-mpd-last-state nil)
(defvar emms-player-mpd-status-timer nil)

(defvar emms-player-mpd-status-regexp
  "^\\(OK\\( MPD \\)?\\|ACK \\[\\([0-9]+\\)@[0-9]+\\] \\(.+\\)\\)\n+\\'"
  "Regexp that matches the valid status strings that MusicPD can
return at the end of a request.")

(defun emms-player-mpd-sentinel (proc event)
  "The process sentinel for MusicPD."
  (let ((status (process-status proc)))
    (cond ((string-match "^deleted" event)
           (when emms-player-mpd-verbose
             (message "MusicPD process was deleted")))
          ((memq status '(exit signal closed))
           (emms-player-mpd-close-process t)
           (when emms-player-mpd-verbose
             (message "Closed MusicPD process")))
          ((memq status '(run open))
           (when emms-player-mpd-verbose
             (message "MusicPD process started successfully")))
          (t
           (when emms-player-mpd-verbose
             (message "Other MusicPD status change: %s, %s" status event))))))

;; Ignore a useless byte-compile warning
(eval-when-compile
  (put 'process-kill-without-query 'byte-compile nil))

(defun emms-player-mpd-ensure-process ()
  "Make sure that a MusicPD process is currently active."
  (unless (and emms-player-mpd-process
               (processp emms-player-mpd-process)
               (memq (process-status emms-player-mpd-process) '(run open)))
    (setq emms-player-mpd-process
          (funcall emms-player-mpd-connect-function "mpd"
                   nil
                   emms-player-mpd-server-name
                   emms-player-mpd-server-port))
    (set-process-sentinel emms-player-mpd-process
                          'emms-player-mpd-sentinel)
    (setq emms-player-mpd-queue
          (tq-create emms-player-mpd-process))
    (if (fboundp 'set-process-query-on-exit-flag)
        (set-process-query-on-exit-flag emms-player-mpd-process nil)
      (process-kill-without-query emms-player-mpd-process))
    ;; send password
    (when (stringp emms-player-mpd-server-password)
      (tq-enqueue emms-player-mpd-queue
                  (concat "password " emms-player-mpd-server-password "\n")
                  emms-player-mpd-status-regexp nil #'ignore t))))

(defun emms-player-mpd-close-process (&optional from-sentinel)
  "Terminate the current MusicPD client process.
FROM-SENTINEL indicates whether this was called by the process sentinel,
in which case certain checks should not be made."
  (when (or from-sentinel
            (and (processp emms-player-mpd-process)
                 (memq (process-status emms-player-mpd-process) '(run open))))
    (tq-close emms-player-mpd-queue)
    (setq emms-player-mpd-queue nil)
    (setq emms-player-mpd-process nil)))

(defun emms-player-mpd-send (question closure fn)
  "Send the given QUESTION to the MusicPD server.
When a reply comes, call FN with CLOSURE and the result."
  (emms-player-mpd-ensure-process)
  (unless (string= (substring question -1) "\n")
    (setq question (concat question "\n")))
  (tq-enqueue emms-player-mpd-queue question
              emms-player-mpd-status-regexp
              closure fn t))

;;; Helper functions

(defun emms-player-mpd-get-mpd-filename (file)
  "Turn FILE into something that MusicPD can understand.

This usually means removing a prefix."
  (if (or (not emms-player-mpd-music-directory)
          (not (eq (aref file 0) ?/))
          (string-match "\\`http://" file))
      file
    (file-relative-name file emms-player-mpd-music-directory)))

(defun emms-player-mpd-get-emms-filename (file)
  "Turn FILE into something that EMMS can understand.

This usually means adding a prefix."
  (if (or (not emms-player-mpd-music-directory)
          (eq (aref file 0) ?/)
          (string-match "\\`http://" file))
      file
    (expand-file-name file emms-player-mpd-music-directory)))

(defun emms-player-mpd-parse-response (response)
  "Convert the given MusicPD response into a list.

The car of the list is special:
If an error has occurred, it will contain a cons cell whose car is
an error number and whose cdr is the corresponding message.
Otherwise, it will be nil."
  (when (stringp response)
    (save-match-data
      (let* ((data (split-string response "\n"))
             (cruft (last data 3))
             (status (if (string= (cadr cruft) "")
                         (car cruft)
                       (cadr cruft))))
        (setcdr cruft nil)
        (when (and (stringp (car data))
                   (string-match "^OK\\( MPD \\)?" (car data)))
          (setq data (cdr data)))
        (if (and (stringp status)
                 (string-match "^ACK \\[\\([0-9]+\\)@[0-9]+\\] \\(.+\\)"
                               status))
            (cons (cons (match-string 1 status)
                        (match-string 2 status))
                  data)
          (cons nil data))))))

(defun emms-player-mpd-parse-line (line)
  "Turn the given LINE from MusicPD into a cons cell.

The format of the cell is (name . value)."
  (when (string-match "\\`\\([^:\n]+\\):\\s-*\\(.+\\)" line)
    (let ((name (match-string 1 line))
          (value (match-string 2 line)))
      (if (and name value)
          (progn
            (setq name (downcase name))
            (cons name value))
        nil))))

(defun emms-player-mpd-get-alist (info)
  "Turn the given parsed INFO from MusicPD into an alist."
  (when (and info
             (null (car info))          ; no error has occurred
             (cdr info))                ; data exists
    (let ((alist nil)
          cell old-cell)
      (dolist (line (cdr info))
        (when (setq cell (emms-player-mpd-parse-line line))
          (if (setq old-cell (assoc (car cell) alist))
              (setcdr old-cell (cdr cell))
            (setq alist (cons cell alist)))))
      alist)))

(defun emms-player-mpd-get-alists (info)
  "Turn the given parsed INFO from MusicPD into an list of alists.

The list will be in reverse order."
  (when (and info
             (null (car info))          ; no error has occurred
             (cdr info))                ; data exists
    (let ((alists nil)
          (alist nil)
          cell)
      (dolist (line (cdr info))
        (when (setq cell (emms-player-mpd-parse-line line))
          (if (assoc (car cell) alist)
              (setq alists (cons alist alists)
                    alist (list cell))
            (setq alist (cons cell alist)))))
      (when alist
        (setq alists (cons alist alists)))
      alists)))

(defun emms-player-mpd-get-tracks-1 (closure response)
  (let ((songs (emms-player-mpd-get-alists
                (emms-player-mpd-parse-response response)))
        (tracks nil))
    (when songs
      (dolist (song-info songs)
        (let ((file (cdr (assoc "file" song-info))))
          (when file
            (setq file (emms-player-mpd-get-emms-filename file))
            (let* ((type (if (string-match "\\`http://" file)
                            'url
                           'file))
                   (track (emms-track type file)))
              (emms-info-mpd track song-info)
              (run-hook-with-args 'emms-track-info-filters track)
              (setq tracks (cons track tracks)))))))
    (funcall (car closure) (cdr closure) tracks)))

(defun emms-player-mpd-get-tracks (closure callback)
  "Get the current playlist from MusicPD in the form of a list of
EMMS tracks.
Call CALLBACK with CLOSURE and result when the request is complete."
  (emms-player-mpd-send "playlistinfo" (cons callback closure)
                        #'emms-player-mpd-get-tracks-1))

(defun emms-player-mpd-get-status-1 (closure response)
  (funcall (car closure)
           (cdr closure)
           (emms-player-mpd-get-alist
            (emms-player-mpd-parse-response response))))

(defun emms-player-mpd-get-status (closure callback)
  "Get status information from MusicPD.
It will be returned in the form of an alist by calling CALLBACK
with CLOSURE as its first argument, and the status as the
second."
  (emms-player-mpd-send "status" (cons callback closure)
                        #'emms-player-mpd-get-status-1))

(defun emms-player-mpd-get-status-part (closure callback item &optional info)
  "Get ITEM from the current MusicPD status.
Call CALLBACK with CLOSURE and result when the request is complete.
If INFO is specified, use that instead of acquiring the necessary
info from MusicPD."
  (if info
      (funcall callback closure (cdr (assoc item info)))
    (emms-player-mpd-get-status
     (cons callback (cons closure item))
     (lambda (closure info)
       (let ((fn (car closure))
             (close (cadr closure))
             (item (cddr closure)))
         (funcall fn close (cdr (assoc item info))))))))

(defun emms-player-mpd-get-playlist-id (closure callback &optional info)
  "Get the current playlist ID from MusicPD.
Call CALLBACK with CLOSURE and result when the request is complete.
If INFO is specified, use that instead of acquiring the necessary
info from MusicPD."
  (when info
    (setq callback (lambda (closure id) id)))
  (emms-player-mpd-get-status-part closure callback "playlist" info))

(defun emms-player-mpd-get-volume (closure callback &optional info)
  "Get the current volume from MusicPD.
Call CALLBACK with CLOSURE and result when the request is complete.
If INFO is specified, use that instead of acquiring the necessary
info from MusicPD."
  (when info
    (setq callback (lambda (closure volume) volume)))
  (emms-player-mpd-get-status-part closure callback "volume" info))

(defun emms-player-mpd-get-current-song (closure callback &optional info)
  "Get the current song from MusicPD.
This is in the form of a number that indicates the position of
the song on the current playlist.

Call CALLBACK with CLOSURE and result when the request is complete.
If INFO is specified, use that instead of acquiring the necessary
info from MusicPD."
  (when info
    (setq callback (lambda (closure id) id)))
  (emms-player-mpd-get-status-part closure callback "song" info))

(defun emms-player-mpd-get-mpd-state (closure callback &optional info)
  "Get the current state of the MusicPD server.
This is either \"play\", \"stop\", or \"pause\".

Call CALLBACK with CLOSURE and result when the request is complete.
If INFO is specified, use that instead of acquiring the necessary
info from MusicPD."
  (when info
    (setq callback (lambda (closure id) id)))
  (emms-player-mpd-get-status-part closure callback "state" info))

(defun emms-player-mpd-get-playing-time (closure callback &optional info)
  "Get the number of seconds that the current song has been playing,
or nil if we cannot obtain this information.

Call CALLBACK with CLOSURE and result when the request is complete.
If INFO is specified, use that instead of acquiring the necessary
info from MusicPD."
  (if info
      (emms-player-mpd-get-status-part
       nil
       (lambda (closure time)
         (and time
              (string-match "\\`\\([0-9]+\\):" time)
              (string-to-number (match-string 1 time))))
       "time" info)
    (emms-player-mpd-get-status-part
     (cons callback closure)
     (lambda (closure time)
       (funcall (car closure)
                (cdr closure)
                (and time
                     (string-match "\\`\\([0-9]+\\):" time)
                     (string-to-number (match-string 1 time)))))
     "time" info)))

(defun emms-player-mpd-select-song (prev-song new-song)
  "Move to the given song position.

The amount to move is the number difference between PREV-SONG and
NEW-SONG.  NEW-SONG should be a string containing a number.
PREV-SONG may be either a string containing a number or nil,
which indicates that we should start from the beginning of the
buffer and move to NEW-SONG."
  (with-current-emms-playlist
    ;; move to current track
    (goto-char (if (and (stringp prev-song)
                        emms-playlist-selected-marker
                        (marker-position emms-playlist-selected-marker))
                   emms-playlist-selected-marker
                 (point-min)))
    ;; seek forward or backward
    (let ((diff (if (stringp prev-song)
                    (- (string-to-number new-song)
                       (string-to-number prev-song))
                  (string-to-number new-song))))
      (condition-case nil
          (progn
            ;; skip to first track if not on one
            (when (and (> diff 0)
                       (not (emms-playlist-track-at (point))))
              (emms-playlist-next))
            ;; move to new track
            (while (> diff 0)
              (emms-playlist-next)
              (setq diff (- diff 1)))
            (while (< diff 0)
              (emms-playlist-previous)
              (setq diff (+ diff 1)))
            ;; select track at point
            (unless (emms-playlist-selected-track-at-p)
              (emms-playlist-select (point))))
        (error (concat "Could not move to position " new-song))))))

(defun emms-player-mpd-sync-from-emms-1 (closure)
  (emms-player-mpd-get-playlist-id
   closure
   (lambda (closure id)
     (let ((buffer (car closure))
           (fn (cdr closure)))
       (when (functionp fn)
         (funcall fn buffer id))))))

(defun emms-player-mpd-sync-from-emms (&optional callback)
  "Synchronize the MusicPD playlist with the contents of the
current EMMS playlist.

If CALLBACK is provided, call it with the current EMMS playlist
buffer and MusicPD playlist ID when we are done, if there were no
errors."
  (emms-player-mpd-clear)
  (with-current-emms-playlist
    (let (tracks)
      (save-excursion
        (setq tracks (nreverse
                      (emms-playlist-tracks-in-region
                       (point-min) (point-max)))))
      (emms-player-mpd-add-several-tracks
       tracks
       (cons (current-buffer) callback)
       #'emms-player-mpd-sync-from-emms-1))))

(defun emms-player-mpd-sync-from-mpd-2 (closure info)
  (let ((buffer (car closure))
        (fn (cadr closure))
        (close (cddr closure))
        (id (emms-player-mpd-get-playlist-id nil #'ignore info))
        (song (emms-player-mpd-get-current-song nil #'ignore info)))
    (when (buffer-live-p buffer)
      (let ((emms-playlist-buffer buffer))
        (with-current-emms-playlist
          (setq emms-player-mpd-playlist-id id)
          (set-buffer-modified-p nil)
          (if song
              (emms-player-mpd-select-song nil song)
            (goto-char (point-min)))))
      (when (functionp fn)
        (funcall fn close info)))))

(defun emms-player-mpd-sync-from-mpd-1 (closure tracks)
  (let ((buffer (car closure)))
    (when (and tracks
               (buffer-live-p buffer))
      (let ((emms-playlist-buffer buffer))
        (with-current-emms-playlist
          (emms-playlist-clear)
          (mapc #'emms-playlist-insert-track tracks)))
      (emms-player-mpd-get-status closure
                                  #'emms-player-mpd-sync-from-mpd-2))))

(defun emms-player-mpd-sync-from-mpd (&optional closure callback)
  "Synchronize the EMMS playlist with the contents of the current
MusicPD playlist.  Namely, clear the EMMS playlist buffer and add
tracks to it that are present in the MusicPD playlist.

If the current buffer is an EMMS playlist buffer, make it the
main EMMS playlist buffer."
  (when (and emms-playlist-buffer-p
             (not (eq (current-buffer) emms-playlist-buffer)))
    (emms-playlist-set-playlist-buffer (current-buffer)))
  (with-current-emms-playlist
    (emms-player-mpd-get-tracks
     (cons emms-playlist-buffer (cons callback closure))
     #'emms-player-mpd-sync-from-mpd-1)))

(defun emms-player-mpd-detect-song-change-2 (state info)
  "Perform post-sync tasks after returning from a stop."
  (setq emms-player-mpd-current-song nil)
  (setq emms-player-playing-p 'emms-player-mpd)
  (when (string= state "pause")
    (setq emms-player-paused-p t))
  (emms-player-mpd-detect-song-change info))

(defun emms-player-mpd-detect-song-change-1 (closure info)
  (let ((song (emms-player-mpd-get-current-song nil #'ignore info))
        (state (emms-player-mpd-get-mpd-state nil #'ignore info))
        (time (emms-player-mpd-get-playing-time nil #'ignore info))
        (err-msg (cdr (assoc "error" info))))
    (if (stringp err-msg)
        (progn
          (message "MusicPD error: %s" err-msg)
          (emms-player-mpd-send
           "clearerror"
           nil #'ignore))
      (cond ((string= state "stop")
             (if song
                 ;; a track remains: the user probably stopped MusicPD
                 ;; manually, so we'll stop EMMS completely
                 (let ((emms-player-stopped-p t))
                   (setq emms-player-mpd-last-state "stop")
                   (emms-player-stopped))
               ;; no more tracks are left: we probably ran out of things
               ;; to play, so let EMMS do something further if it wants
               (unless (string= emms-player-mpd-last-state "stop")
                 (setq emms-player-mpd-last-state "stop")
                 (emms-player-stopped))))
            ((and emms-player-mpd-last-state
                  (string= emms-player-mpd-last-state "stop"))
             ;; resume from a stop that occurred outside of EMMS
             (setq emms-player-mpd-last-state nil)
             (emms-player-mpd-sync-from-mpd
              state
              #'emms-player-mpd-detect-song-change-2))
            ((string= state "pause")
             nil)
            ((string= state "play")
             (setq emms-player-mpd-last-state "play")
             (unless (or (null song)
                         (and (stringp emms-player-mpd-current-song)
                              (string= song emms-player-mpd-current-song)))
               (let ((emms-player-stopped-p t))
                 (emms-player-stopped))
               (emms-player-mpd-select-song emms-player-mpd-current-song song)
               (setq emms-player-mpd-current-song song)
               (emms-player-started 'emms-player-mpd)
               (when time
                 (run-hook-with-args 'emms-player-time-set-functions
                                     time))))))))

(defun emms-player-mpd-detect-song-change (&optional info)
  "Detect whether a song change has occurred.
This is usually called by a timer.

If INFO is specified, use that instead of acquiring the necessary
info from MusicPD."
  (if info
      (emms-player-mpd-detect-song-change-1 nil info)
    (emms-player-mpd-get-status nil #'emms-player-mpd-detect-song-change-1)))

(defun emms-player-mpd-quote-file (file)
  "Escape special characters in FILE and surround in double-quotes."
  (concat "\""
          (emms-replace-regexp-in-string
           "\"" "\\\\\""
           (emms-replace-regexp-in-string "\\\\" "\\\\\\\\" file))
          "\""))

;;;###autoload
(defun emms-player-mpd-clear ()
  "Clear the MusicPD playlist."
  (interactive)
  (when emms-player-mpd-status-timer
    (emms-cancel-timer emms-player-mpd-status-timer)
    (setq emms-player-mpd-status-timer nil))
  (setq emms-player-mpd-last-state nil)
  (emms-player-mpd-send "clear" nil #'ignore)
  (let ((emms-player-stopped-p t))
    (emms-player-stopped)))

;;; Adding to the MusicPD playlist

(defun emms-player-mpd-add-file (file closure callback)
  "Add FILE to the current MusicPD playlist.
Execute CALLBACK with CLOSURE as its first argument when done.

If an error occurs, display a relevant message."
  (setq file (emms-player-mpd-get-mpd-filename file))
  (emms-player-mpd-send
   (concat "add " (emms-player-mpd-quote-file file))
   (cons file (cons callback closure))
   (lambda (closure response)
     (let ((output (emms-player-mpd-parse-response response))
           (file (car closure))
           (callback (cadr closure))
           (close (cddr closure)))
       (if (car output)
           (message "MusicPD error: %s: %s" file (cdar output))
         (when (functionp callback)
           (funcall callback close)))))))

(defun emms-player-mpd-add-buffer-contents (buffer closure callback)
  "Load contents of BUFFER into MusicPD by adding each line.
Execute CALLBACK with CLOSURE as its first argument when done.

This handles both m3u and pls type playlists."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((format (emms-source-playlist-determine-format)))
      (when format
        (emms-player-mpd-add-several-files
         (emms-source-playlist-files format)
         closure callback)))))

(defun emms-player-mpd-add-playlist (playlist closure callback)
  "Load contents of PLAYLIST into MusicPD by adding each line.
Execute CALLBACK with CLOSURE as its first argument when done.

This handles both m3u and pls type playlists."
  ;; This is useful for playlists of playlists
  (with-temp-buffer
    (emms-insert-file-contents playlist)
    (emms-player-mpd-add-buffer-contents (current-buffer) closure callback)))

(defun emms-player-mpd-add-streamlist (url closure callback)
  "Download contents of URL and then add its feeds into MusicPD.
Execute CALLBACK with CLOSURE as its first argument when done."
  ;; This is useful with emms-streams.el
  (if (fboundp 'url-insert-file-contents)
      (progn
        (require 'emms-url)
        (with-temp-buffer
          (url-insert-file-contents (emms-url-quote-entire url))
          (emms-http-decode-buffer (current-buffer))
          (emms-player-mpd-add-buffer-contents (current-buffer)
                                               closure callback)))
    (error (message (concat "You need to install url.el so that"
                            " Emms can retrieve this stream")))))

(defun emms-player-mpd-add (track closure callback)
  "Add TRACK to the MusicPD playlist.
Execute CALLBACK with CLOSURE as its first argument when done."
  (let ((name (emms-track-get track 'name))
        (type (emms-track-get track 'type)))
    (cond ((eq type 'url)
           (emms-player-mpd-add-file name closure callback))
          ((eq type 'streamlist)
           (emms-player-mpd-add-streamlist name closure callback))
          ((or (eq type 'playlist)
               (string-match "\\.\\(m3u\\|pls\\)\\'" name))
           (emms-player-mpd-add-playlist name closure callback))
          ((and (eq type 'file)
                (string-match emms-player-mpd-supported-regexp name))
           (emms-player-mpd-add-file name closure callback)))))

(defun emms-player-mpd-add-several-tracks (tracks closure callback)
  "Add TRACKS to the MusicPD playlist.
Execute CALLBACK with CLOSURE as its first argument when done."
  (when (consp tracks)
    (while (cdr tracks)
      (emms-player-mpd-add (car tracks) nil #'ignore)
      (setq tracks (cdr tracks)))
    ;; only execute callback on last track
    (emms-player-mpd-add (car tracks) closure callback)))

(defun emms-player-mpd-add-several-files (files closure callback)
  "Add FILES to the MusicPD playlist.
Execute CALLBACK with CLOSURE as its first argument when done."
  (when (consp files)
    (while (cdr files)
      (emms-player-mpd-add-file (car files) nil #'ignore)
      (setq files (cdr files)))
    ;; only execute callback on last file
    (emms-player-mpd-add-file (car files) closure callback)))

;;; EMMS API

(defun emms-player-mpd-playable-p (track)
  "Return non-nil when we can play this track."
  (and (memq (emms-track-type track) '(file url playlist streamlist))
       (string-match (emms-player-get emms-player-mpd 'regex)
                     (emms-track-name track))
       (condition-case nil
           (progn (emms-player-mpd-ensure-process)
                  t)
         (error nil))))

(defun emms-player-mpd-play (&optional id)
  "Play whatever is in the current MusicPD playlist.
If ID is specified, play the song at that position in the MusicPD
playlist."
  (if id
      (progn
        (unless (stringp id)
          (setq id (number-to-string id)))
        (emms-player-mpd-send
         (concat "play " id)
         nil
         (lambda (closure response)
           (setq emms-player-mpd-current-song nil)
           (if emms-player-mpd-check-interval
               (setq emms-player-mpd-status-timer
                     (run-at-time t emms-player-mpd-check-interval
                                  'emms-player-mpd-detect-song-change))
             (emms-player-mpd-detect-song-change)))))
    ;; we only want to play one track, so don't start the timer
    (emms-player-mpd-send
     "play"
     nil
     (lambda (closure response)
       (emms-player-started 'emms-player-mpd)))))

(defun emms-player-mpd-start-and-sync-2 (buffer id)
  (when (buffer-live-p buffer)
    (let ((emms-playlist-buffer buffer))
      (with-current-emms-playlist
        (setq emms-player-mpd-playlist-id id)
        (set-buffer-modified-p nil)
        (let ((track-cnt 0))
          (save-excursion
            (goto-char
             (if (and emms-playlist-selected-marker
                      (marker-position emms-playlist-selected-marker))
                 emms-playlist-selected-marker
               (point-min)))
            (condition-case nil
                (while t
                  (emms-playlist-previous)
                  (setq track-cnt (1+ track-cnt)))
              (error nil)))
          (emms-player-mpd-play track-cnt))))))

(defun emms-player-mpd-start-and-sync-1 (closure id)
  (let ((buf-id (with-current-emms-playlist
                  emms-player-mpd-playlist-id)))
    (if (and (not (buffer-modified-p emms-playlist-buffer))
             (stringp buf-id)
             (string= buf-id id))
        (emms-player-mpd-start-and-sync-2 emms-playlist-buffer id)
      (emms-player-mpd-sync-from-emms
       #'emms-player-mpd-start-and-sync-2))))

(defun emms-player-mpd-start-and-sync ()
  "Ensure that MusicPD's playlist is up-to-date with EMMS's
playlist, and then play the current track.

This is called if `emms-player-mpd-sync-playlist' is non-nil."
  (when emms-player-mpd-status-timer
    (emms-cancel-timer emms-player-mpd-status-timer)
    (setq emms-player-mpd-status-timer nil))
  (emms-player-mpd-send
   "clearerror"
   nil
   (lambda (closure response)
     (emms-player-mpd-get-playlist-id
      nil
      #'emms-player-mpd-start-and-sync-1))))

(defun emms-player-mpd-connect-1 (closure info)
  (setq emms-player-mpd-current-song nil)
  (let* ((state (emms-player-mpd-get-mpd-state nil #'ignore info)))
    (unless (string= state "stop")
      (setq emms-player-playing-p 'emms-player-mpd))
    (when (string= state "pause")
      (setq emms-player-paused-p t))
    (unless (string= state "stop")
      (emms-player-mpd-detect-song-change info)
      (when emms-player-mpd-check-interval
        (setq emms-player-mpd-status-timer
              (run-at-time t emms-player-mpd-check-interval
                           'emms-player-mpd-detect-song-change))))))

;;;###autoload
(defun emms-player-mpd-connect ()
  "Connect to MusicPD and retrieve its current playlist.

Afterward, the status of MusicPD will be tracked.

This also has the effect of changing the current EMMS playlist to
be the same as the current MusicPD playlist.  Thus, this
function is useful to call if the contents of the EMMS playlist
buffer get out-of-sync for some reason."
  (interactive)
  (when emms-player-mpd-status-timer
    (emms-cancel-timer emms-player-mpd-status-timer)
    (setq emms-player-mpd-status-timer nil))
  (emms-player-mpd-sync-from-mpd
   nil #'emms-player-mpd-connect-1))

(defun emms-player-mpd-start (track)
  "Starts a process playing TRACK."
  (interactive)
  (if (and emms-player-mpd-sync-playlist
           (not (memq (emms-track-get track 'type) '(streamlist playlist))))
      (emms-player-mpd-start-and-sync)
    (emms-player-mpd-clear)
    ;; if we have loaded the item successfully, play it
    (emms-player-mpd-add track nil #'emms-player-mpd-play)))

(defun emms-player-mpd-disconnect (&optional no-stop)
  "Terminate the MusicPD client process and disconnect from MusicPD.

If NO-STOP is non-nil, do not indicate to EMMS that we are
stopped.  This argument is meant to be used when calling this
from other functions."
  (interactive)
  (emms-cancel-timer emms-player-mpd-status-timer)
  (setq emms-player-mpd-status-timer nil)
  (setq emms-player-mpd-current-song nil)
  (setq emms-player-mpd-last-state nil)
  (emms-player-mpd-close-process)
  (unless no-stop
    (let ((emms-player-stopped-p t))
      (emms-player-stopped))))

(defun emms-player-mpd-stop ()
  "Stop the currently playing song."
  (interactive)
  (condition-case nil
      (emms-player-mpd-send "stop" nil #'ignore)
    (error nil))
  (emms-player-mpd-disconnect t)
  (let ((emms-player-stopped-p t))
    (emms-player-stopped)))

(defun emms-player-mpd-pause ()
  "Pause the currently playing song."
  (interactive)
  (emms-player-mpd-send "pause" nil #'ignore))

(defun emms-player-mpd-seek (amount)
  "Seek backward or forward by AMOUNT seconds, depending on sign of AMOUNT."
  (interactive)
  (emms-player-mpd-get-status
   amount
   (lambda (amount info)
     (let ((song (emms-player-mpd-get-current-song nil #'ignore info))
           (secs (emms-player-mpd-get-playing-time nil #'ignore info)))
       (when (and song secs)
         (emms-player-mpd-send
          (concat "seek " song " " (number-to-string (round (+ secs amount))))
          nil #'ignore))))))

(defun emms-player-mpd-seek-to (pos)
  "Seek to POS seconds from the start of the current track."
  (interactive)
  (emms-player-mpd-get-current-song
   pos
   (lambda (pos song)
     (when (and song pos)
       (emms-player-mpd-send
        (concat "seek " song " " (number-to-string (round pos)))
        nil #'ignore)))))

(defun emms-player-mpd-next ()
  "Move forward by one track in MusicPD's internal playlist."
  (interactive)
  (emms-player-mpd-send "next" nil #'ignore))

(defun emms-player-mpd-previous ()
  "Move backward by one track in MusicPD's internal playlist."
  (interactive)
  (emms-player-mpd-send "previous" nil #'ignore))

;;; Volume

(defun emms-volume-mpd-change (amount)
  "Change volume up or down by AMOUNT, depending on whether it is
positive or negative."
  (interactive "MVolume change amount (+ increase, - decrease): ")
  (emms-player-mpd-get-volume
   amount
   (lambda (change volume)
     (let ((new-volume (+ (string-to-number volume) change)))
       (emms-player-mpd-send
        (concat "setvol \"" (number-to-string new-volume) "\"")
        nil #'ignore)))))

;;; Now playing

(defun emms-player-mpd-show-1 (closure response)
  (let* ((info (emms-player-mpd-get-alist
                (emms-player-mpd-parse-response response)))
         (insertp (car closure))
         (callback (cadr closure))
         (buffer (cddr closure))
         (name (cdr (assoc "name" info))) ; radio feeds sometimes set this
         (file (cdr (assoc "file" info)))
         (desc nil))
    ;; if we are playing lastfm radio, use its show function instead
    (if (and (boundp 'emms-lastfm-radio-stream-url)
             (stringp emms-lastfm-radio-stream-url)
             (string= emms-lastfm-radio-stream-url file))
        (with-current-buffer buffer
          (and (fboundp 'emms-lastfm-np)
               (emms-lastfm-np insertp callback)))
      ;; otherwise build and show the description
      (when info
        (when name
          (setq desc name))
        (when file
          (let ((track (emms-dictionary '*track*))
                track-desc)
            (if (string-match "\\`http://" file)
                (emms-track-set track 'type 'url)
              (emms-track-set track 'type 'file))
            (emms-track-set track 'name file)
            (emms-info-mpd track info)
            (run-hook-with-args 'emms-track-info-filters track)
            (setq track-desc (emms-track-description track))
            (when (and (stringp track-desc) (not (string= track-desc "")))
              (setq desc (if desc
                             (concat desc ": " track-desc)
                           track-desc))))))
      (if (not desc)
          (unless (functionp callback)
            (message "Nothing playing right now"))
        (setq desc (format emms-show-format desc))
        (cond ((functionp callback)
               (funcall callback buffer desc))
              (insertp
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (insert desc))))
              (t
               (message "%s" desc)))))))

;;;###autoload
(defun emms-player-mpd-show (&optional insertp callback)
  "Describe the current EMMS track in the minibuffer.

If INSERTP is non-nil, insert the description into the current
buffer instead.

If CALLBACK is a function, call it with the current buffer and
description as arguments instead of displaying the description or
inserting it.

This function uses `emms-show-format' to format the current track.
It differs from `emms-show' in that it asks MusicPD for the current track,
rather than EMMS."
  (interactive "P")
  (emms-player-mpd-send "currentsong"
                        (cons insertp (cons callback (current-buffer)))
                        #'emms-player-mpd-show-1))

;;; Track info

(defun emms-info-mpd-process (track info)
  (dolist (data info)
    (let ((name (car data))
          (value (cdr data)))
      (setq name (cond ((string= name "artist") 'info-artist)
                       ((string= name "composer") 'info-composer)
                       ((string= name "performer") 'info-performer)
                       ((string= name "title") 'info-title)
                       ((string= name "album") 'info-album)
                       ((string= name "track") 'info-tracknumber)
                       ((string= name "date") 'info-year)
                       ((string= name "genre") 'info-genre)
                       ((string= name "time")
                        (setq value (string-to-number value))
                        'info-playing-time)
                       (t nil)))
      (when name
        (emms-track-set track name value)))))

(defun emms-info-mpd-1 (track response)
  (let ((info (emms-player-mpd-get-alist
               (emms-player-mpd-parse-response response))))
    (when info
      (emms-info-mpd-process track info)
      (emms-track-updated track))))

(defun emms-info-mpd (track &optional info)
  "Add track information to TRACK.
If INFO is specified, use that instead of acquiring the necessary
info from MusicPD.

This is a useful addition to `emms-info-functions'."
  (if info
      (emms-info-mpd-process track info)
    (when (and (eq 'file (emms-track-type track))
               (not (string-match "\\`http://" (emms-track-name track))))
      (let ((file (emms-player-mpd-get-mpd-filename (emms-track-name track))))
        (when (or emms-player-mpd-music-directory
                  (and file
                       (string-match emms-player-mpd-supported-regexp file)))
          (condition-case nil
              (emms-player-mpd-send
               (concat "find filename "
                       (emms-player-mpd-quote-file file))
               track
               #'emms-info-mpd-1)
            (error nil)))))))

;;; Caching

(defun emms-cache-set-from-mpd-track (track-info)
  "Dump TRACK-INFO into the EMMS cache.

The track should be an alist as per `emms-player-mpd-get-alist'."
  (when emms-cache-set-function
    (let ((track (emms-dictionary '*track*))
          (name (cdr (assoc "file" track-info))))
      (when name
        (setq name (emms-player-mpd-get-emms-filename name))
        (emms-track-set track 'type 'file)
        (emms-track-set track 'name name)
        (emms-info-mpd-process track track-info)
        (funcall emms-cache-set-function 'file name track)))))

(defun emms-cache-set-from-mpd-directory (dir)
  "Dump all MusicPD data from DIR into the EMMS cache.

This is useful to do when you have recently acquired new music."
  (interactive
   (list (if emms-player-mpd-music-directory
             (emms-read-directory-name "Directory: "
                                       emms-player-mpd-music-directory)
           (read-string "Directory: "))))
  (unless (string= dir "")
    (setq dir (emms-player-mpd-get-mpd-filename dir)))
  (if emms-cache-set-function
      (progn
        (message "Dumping MusicPD data to cache...")
        (emms-player-mpd-send
         (concat "listallinfo " dir)
         nil
         (lambda (closure response)
           (message "Dumping MusicPD data to cache...processing")
           (let ((info (emms-player-mpd-get-alists
                        (emms-player-mpd-parse-response response))))
             (dolist (track-info info)
               (emms-cache-set-from-mpd-track track-info))
             (message "Dumping MusicPD data to cache...done")))))
    (error "Caching is not enabled")))

(defun emms-cache-set-from-mpd-all ()
  "Dump all MusicPD data into the EMMS cache.

This is useful to do once, just before using emms-browser.el, in
order to prime the cache."
  (interactive)
  (emms-cache-set-from-mpd-directory ""))

;;; Updating tracks

(defun emms-player-mpd-update-directory (dir)
  "Cause the tracks in DIR to be updated in the MusicPD database."
  (interactive
   (list (if emms-player-mpd-music-directory
             (emms-read-directory-name "Directory: "
                                       emms-player-mpd-music-directory)
           (read-string "Directory: "))))
  (unless (string= dir "")
    (setq dir (emms-player-mpd-get-mpd-filename dir)))
  (emms-player-mpd-send
   (concat "update " (emms-player-mpd-quote-file dir)) nil
   (lambda (closure response)
     (let ((id (cdr (assoc "updating_db"
                           (emms-player-mpd-get-alist
                            (emms-player-mpd-parse-response response))))))
       (if id
           (message "Updating DB with ID %s" id)
         (message "Could not update the DB"))))))

(defun emms-player-mpd-update-all ()
  "Cause all tracks in the MusicPD music directory to be updated in
the MusicPD database."
  (interactive)
  (emms-player-mpd-update-directory ""))

(defvar emms-player-mpd-waiting-for-update-timer nil
  "Timer object when waiting for MPD update to finish.")

(defun emms-player-mpd-update-all-reset-cache ()
  "Update all tracks in the MusicPD music directory.
When update finishes, clear the EMMS cache and call
`emms-cache-set-from-mpd-all' to dump the MusicPD data into the
cache."
  (interactive)
  (if emms-player-mpd-waiting-for-update-timer
      (message "Already waiting for an update to finish.")
    (emms-player-mpd-send
     "update" nil
     'emms-player-mpd-wait-for-update)))

(defun emms-player-mpd-wait-for-update (&optional closure response)
  "Wait for a currently running mpd update to finish.
Afterwards, clear the EMMS cache and call
`emms-cache-set-from-mpd-all'."
  (if response
      ;; This is the first call after the update command
      (let ((id (cdr (assoc "updating_db"
			    (emms-player-mpd-get-alist
			     (emms-player-mpd-parse-response response))))))
	(if id
	    (progn
	      (message "Updating DB with ID %s.  Waiting for the update to finish..." id)
	      (setq emms-player-mpd-waiting-for-update-timer
		    (run-at-time 1 nil 'emms-player-mpd-wait-for-update)))
	  (message "Could not update the DB")))
    ;; Otherwise, check if update is still in progress
    (emms-player-mpd-get-status-part
     nil
     (lambda (closure updating)
       (if updating
	   ;; MPD update still in progress, so wait another second
	   (run-at-time 1 nil 'emms-player-mpd-wait-for-update)
	 ;; MPD update finished
	 (setq  emms-player-mpd-waiting-for-update-timer nil)
	 (message "MPD update finished.")
	 (sit-for 1)
	 (clrhash emms-cache-db)
	 (emms-cache-set-from-mpd-all)))
     "updating_db")))


(provide 'emms-player-mpd)

;;; emms-player-mpd.el ends here
