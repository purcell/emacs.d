;;; emms.el --- The Emacs Multimedia System

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008,
;;   2009 Free Software Foundation, Inc.

;; Author: Jorgen Schäfer <forcer@forcix.cx>
;; Keywords: emms, mp3, mpeg, multimedia

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This is the very core of EMMS.  It provides ways to play a track
;; using `emms-start', to go through the playlist using the commands
;; `emms-next' and `emms-previous', to stop the playback using
;; `emms-stop', and to see what's currently playing using `emms-show'.

;; But in itself, this core is useless, because it doesn't know how to
;; play any tracks --- you need players for this.  In fact, it doesn't
;; even know how to find any tracks to consider playing --- for this,
;; you need sources.

;; A sample configuration is offered in emms-setup.el, so you might
;; just want to use that file.

;;; Code:

(defvar emms-version "3.0"
  "EMMS version string.")


;;; User Customization

(defgroup emms nil
  "*The Emacs Multimedia System."
  :prefix "emms-"
  :group 'multimedia
  :group 'applications)

(defgroup emms-player nil
  "*Track players for EMMS."
  :prefix "emms-player-"
  :group 'emms)

(defgroup emms-source nil
  "*Track sources for EMMS."
  :prefix "emms-source-"
  :group 'emms)

(defcustom emms-player-list nil
  "*List of players that EMMS can use.  You need to set this!"
  :group 'emms
  :type '(repeat (symbol :tag "Player")))

(defcustom emms-show-format "Currently playing: %s"
  "*The format to use for `emms-show'.
Any \"%s\" is replaced by what `emms-track-description-function' returns
for the currently playing track."
  :group 'emms
  :type 'string)

(defcustom emms-repeat-playlist nil
  "*Non-nil if the EMMS playlist should automatically repeat.
If nil, playback will stop when the last track finishes playing.
If non-nil, EMMS will wrap back to the first track when that happens."
  :group 'emms
  :type 'boolean)

(defcustom emms-random-playlist nil
  "*Non-nil means that tracks are played randomly. If nil, tracks
are played sequentially."
  :group 'emms
  :type 'boolean)

(defcustom emms-repeat-track nil
  "Non-nil, playback will repeat current track.  If nil, EMMS will play
track by track normally."
  :group 'emms
  :type 'boolean)

(defcustom emms-completing-read-function
  (if (and (boundp 'ido-mode)
           ido-mode)
      'ido-completing-read
    'completing-read)
  "Function to call when prompting user to choose between a list of options.
This should take the same arguments as `completing-read'.  Some
possible values are `completing-read' and `ido-completing-read'.
Note that you must set `ido-mode' if using
`ido-completing-read'."
  :group 'emms
  :type 'function)

(defcustom emms-track-description-function 'emms-track-simple-description
  "*Function for describing an EMMS track in a user-friendly way."
  :group 'emms
  :type 'function)

(defcustom emms-player-delay 0
  "The delay to pause after a player finished.
This is a floating-point number of seconds.  This is necessary
for some platforms where it takes a bit to free the audio device
after a player has finished.  If EMMS is skipping songs, increase
this number."
  :type 'number
  :group 'emms)

(defcustom emms-playlist-shuffle-function 'emms-playlist-simple-shuffle
  "*The function to use for shuffling the playlist."
  :type 'function
  :group 'emms)

(defcustom emms-playlist-sort-function 'emms-playlist-simple-sort
  "*The function to use for sorting the playlist."
  :type 'function
  :group 'emms)

(defcustom emms-playlist-uniq-function 'emms-playlist-simple-uniq
  "*The function to use for removing duplicate tracks in the playlist."
  :type 'function
  :group 'emms)

(defcustom emms-sort-lessp-function 'emms-sort-track-name-less-p
  "*Function for comparing two EMMS tracks.
The function should return non-nil if and only if the first track
sorts before the second (see `sort')."
  :group 'emms
  :type 'function)

(defcustom emms-playlist-buffer-name " *EMMS Playlist*"
  "*The default name of the EMMS playlist buffer."
  :type 'string
  :group 'emms)

(defcustom emms-playlist-default-major-mode default-major-mode
  "*The default major mode for EMMS playlist."
  :type 'function
  :group 'emms)

(defcustom emms-playlist-insert-track-function 'emms-playlist-simple-insert-track
  "*A function to insert a track into the playlist buffer."
  :group 'emms
  :type 'function)
(make-variable-buffer-local 'emms-playlist-insert-track-function)

(defcustom emms-playlist-update-track-function 'emms-playlist-simple-update-track
  "*A function to update the track at point.
This is called when the track information changed.  This also
shouldn't assume that the track has been inserted before."
  :group 'emms
  :type 'function)
(make-variable-buffer-local 'emms-playlist-insert-track-function)

(defcustom emms-playlist-delete-track-function 'emms-playlist-simple-delete-track
  "*A function to delete the track at point in the playlist buffer."
  :group 'emms
  :type 'function)
(make-variable-buffer-local 'emms-playlist-delete-track-function)

(defcustom emms-playlist-source-inserted-hook nil
  "*Hook run when a source got inserted into the playlist.
The buffer is narrowed to the new tracks."
  :type 'hook
  :group 'emms)

(defcustom emms-playlist-selection-changed-hook nil
  "*Hook run after another track is selected in the EMMS playlist."
  :group 'emms
  :type 'hook)

(defcustom emms-playlist-cleared-hook nil
  "*Hook run after the current EMMS playlist is cleared.
This happens both when the playlist is cleared and when a new
buffer is created for it."
  :group 'emms
  :type 'hook)

(defcustom emms-track-initialize-functions nil
  "*List of functions to call for each new EMMS track.
This can be used to initialize tracks with various info."
  :group 'emms
  :type 'hook)

(defcustom emms-track-info-filters nil
  "*List of functions to call when a track changes data, before updating
the display.
These functions are passed the track as an argument."
  :group 'emms
  :type 'hook)

(defcustom emms-track-updated-functions nil
  "*List of functions to call when a track changes data, after updating
the display.
These functions are passed the track as an argument."
  :group 'emms
  :type 'hook)

(defcustom emms-player-started-hook nil
  "*Hook run when an EMMS player starts playing."
  :group 'emms
  :type 'hook
  :options '(emms-show))

(defcustom emms-player-stopped-hook nil
  "*Hook run when an EMMS player is stopped by the user.
See `emms-player-finished-hook'."
  :group 'emms
  :type 'hook)

(defcustom emms-player-finished-hook nil
  "*Hook run when an EMMS player finishes playing a track.
Please pay attention to the differences between
`emms-player-finished-hook' and `emms-player-stopped-hook'.  The
former is called only when the player actually finishes playing a
track; the latter, only when the player is stopped
interactively."
  :group 'emms
  :type 'hook)

(defcustom emms-player-next-function 'emms-next-noerror
  "*A function run when EMMS thinks the next song should be played."
  :group 'emms
  :type 'function
  :options '(emms-next-noerror
             emms-random))

(defcustom emms-player-paused-hook nil
  "*Hook run when a player is paused or resumed.
Use `emms-player-paused-p' to find the current state."
  :group 'emms
  :type 'hook)

(defcustom emms-seek-seconds 10
  "The number of seconds to seek forward or backward when seeking."
  :group 'emms
  :type 'number)

(defcustom emms-player-seeked-functions nil
  "*Functions called when a player is seeking.
The functions are called with a single argument, the amount of
seconds the player did seek."
  :group 'emms
  :type 'hook)

(defcustom emms-player-time-set-functions nil
  "*Functions called when a player is setting the elapsed time of a track.
The functions are called with a single argument, the time elapsed
since the beginning of the current track."
  :group 'emms
  :type 'hook)

(defcustom emms-cache-get-function nil
  "A function to retrieve a track entry from the cache.
This is called with two arguments, the type and the name."
  :group 'emms
  :type 'function)

(defcustom emms-cache-set-function nil
  "A function to add/set a track entry from the cache.
This is called with three arguments: the type of the track, the
name of the track, and the track itself."
  :group 'emms
  :type 'function)

(defcustom emms-cache-modified-function nil
  "A function to be called when a track is modified.
The modified track is passed as the argument to this function."
  :group 'emms
  :type 'function)

(defcustom emms-directory "~/.emacs.d/emms"
  "*Directory variable from which all other emms file variables are derived."
  :group 'emms
  :type 'string)

(defvar emms-player-playing-p nil
  "The currently playing EMMS player, or nil.")

(defvar emms-player-paused-p nil
  "Whether the current player is paused or not.")

(defvar emms-source-old-buffer nil
  "The active buffer before a source was invoked.
This can be used if the source depends on the current buffer not
being the playlist buffer.")

(defvar emms-playlist-buffer nil
  "The current playlist buffer, if any.")


;;; Macros

;;; These need to be at the top of the file so that compilation works.

(defmacro with-current-emms-playlist (&rest body)
  "Run BODY with the current buffer being the current playlist buffer.
This also disables any read-onliness of the current buffer."
  `(progn
     (when (or (not emms-playlist-buffer)
               (not (buffer-live-p emms-playlist-buffer)))
       (emms-playlist-current-clear))
     (let ((emms-source-old-buffer (or emms-source-old-buffer
                                       (current-buffer))))
      (with-current-buffer emms-playlist-buffer
        (let ((inhibit-read-only t))
          ,@body)))))
(put 'with-current-emms-playlist 'lisp-indent-function 0)
(put 'with-current-emms-playlist 'edebug-form-spec '(body))

(defmacro emms-with-inhibit-read-only-t (&rest body)
  "Simple wrapper around `inhibit-read-only'."
  `(let ((inhibit-read-only t))
     ,@body))
(put 'emms-with-inhibit-read-only-t 'edebug-form-spec '(body))

(defmacro emms-with-widened-buffer (&rest body)
  `(save-restriction
     (widen)
     ,@body))
(put 'emms-with-widened-buffer 'edebug-form-spec '(body))

(defmacro emms-walk-tracks (&rest body)
  "Execute BODY for each track in the current buffer, starting at point.
Point will be placed at the beginning of the track before
executing BODY.

Point will not be restored afterward."
  (let ((donep (make-symbol "donep")))
    `(let ((,donep nil))
       ;; skip to first track if not on one
       (unless (emms-playlist-track-at (point))
         (condition-case nil
             (emms-playlist-next)
           (error
            (setq ,donep t))))
       ;; walk tracks
       (while (not ,donep)
         ,@body
         (condition-case nil
             (emms-playlist-next)
           (error
            (setq ,donep t)))))))
(put 'emms-walk-tracks 'lisp-indent-function 0)
(put 'emms-walk-tracks 'edebug-form-spec '(body))


;;; User Interface

(defun emms-start ()
  "Start playing the current track in the EMMS playlist."
  (interactive)
  (unless emms-player-playing-p
    (emms-player-start (emms-playlist-current-selected-track))))

(defun emms-stop ()
  "Stop any current EMMS playback."
  (interactive)
  (when emms-player-playing-p
    (emms-player-stop)))

(defun emms-next ()
  "Start playing the next track in the EMMS playlist.
This might behave funny if called from `emms-player-next-function',
so use `emms-next-noerror' in that case."
  (interactive)
  (when emms-player-playing-p
    (emms-stop))
  (emms-playlist-current-select-next)
  (emms-start))

(defun emms-next-noerror ()
  "Start playing the next track in the EMMS playlist.
Unlike `emms-next', this function doesn't signal an error when called
at the end of the playlist.
This function should only be called when no player is playing.
This is a good function to put in `emms-player-next-function'."
  (interactive)
  (when emms-player-playing-p
    (error "A track is already being played"))
  (cond (emms-repeat-track
	 (emms-start))
	((condition-case nil
             (progn
               (emms-playlist-current-select-next)
               t)
           (error nil))
	 (emms-start))
        (t
	 (message "No next track in playlist"))))

(defun emms-previous ()
  "Start playing the previous track in the EMMS playlist."
  (interactive)
  (when emms-player-playing-p
    (emms-stop))
  (emms-playlist-current-select-previous)
  (emms-start))

(defun emms-random ()
  "Jump to a random track."
  (interactive)
  (when emms-player-playing-p
    (emms-stop))
  (emms-playlist-current-select-random)
  (emms-start))

(defun emms-pause ()
  "Pause the current player.
If player hasn't started, then start it now."
  (interactive)
  (if emms-player-playing-p
      (emms-player-pause)
    (emms-start)))

(defun emms-seek (seconds)
  "Seek the current player SECONDS seconds.
This can be a floating point number for sub-second fractions.
It can also be negative to seek backwards."
  (interactive "nSeconds to seek: ")
  (emms-ensure-player-playing-p)
  (emms-player-seek seconds))

(defun emms-seek-to (seconds)
  "Seek the current player to SECONDS seconds.
This can be a floating point number for sub-second fractions.
It can also be negative to seek backwards."
  (interactive "nSeconds to seek to: ")
  (emms-ensure-player-playing-p)
  (emms-player-seek-to seconds))

(defun emms-seek-forward ()
  "Seek ten seconds forward."
  (interactive)
  (when emms-player-playing-p
    (emms-player-seek emms-seek-seconds)))

(defun emms-seek-backward ()
  "Seek ten seconds backward."
  (interactive)
  (when emms-player-playing-p
    (emms-player-seek (- emms-seek-seconds))))

(defun emms-show (&optional insertp)
  "Describe the current EMMS track in the minibuffer.
If INSERTP is non-nil, insert the description into the current buffer instead.
This function uses `emms-show-format' to format the current track."
  (interactive "P")
  (let ((string (if emms-player-playing-p
                    (format emms-show-format
                            (emms-track-description
                             (emms-playlist-current-selected-track)))
                  "Nothing playing right now")))
    (if insertp
        (insert string)
      (message "%s" string))))

(defun emms-shuffle ()
  "Shuffle the current playlist.
This uses `emms-playlist-shuffle-function'."
  (interactive)
  (with-current-emms-playlist
    (save-excursion
      (funcall emms-playlist-shuffle-function))))

(defun emms-sort ()
  "Sort the current playlist.
This uses `emms-playlist-sort-function'."
  (interactive)
  (with-current-emms-playlist
    (save-excursion
      (funcall emms-playlist-sort-function))))

(defun emms-uniq ()
  "Remove duplicates from the current playlist.
This uses `emms-playlist-uniq-function'."
  (interactive)
  (with-current-emms-playlist
    (save-excursion
      (funcall emms-playlist-uniq-function))))

(defun emms-toggle-random-playlist ()
  "Toggle whether emms plays the tracks randomly or sequentially.
See `emms-random-playlist'."
  (interactive)
  (setq emms-random-playlist (not emms-random-playlist))
  (if emms-random-playlist
      (progn (setq emms-player-next-function 'emms-random)
             (message "Will play the tracks randomly."))
    (setq emms-player-next-function 'emms-next-noerror)
    (message "Will play the tracks sequentially.")))

(defun emms-toggle-repeat-playlist ()
  "Toggle whether emms repeats the playlist after it is done.
See `emms-repeat-playlist'."
  (interactive)
  (setq emms-repeat-playlist (not emms-repeat-playlist))
  (if emms-repeat-playlist
      (message "Will repeat the playlist after it is done.")
    (message "Will stop after the playlist is over.")))

(defun emms-toggle-repeat-track ()
  "Toggle whether emms repeats the current track.
See `emms-repeat-track'."
  (interactive)
  (setq emms-repeat-track (not emms-repeat-track))
  (if emms-repeat-track
      (message "Will repeat the current track.")
    (message "Will advance to the next track after this one.")))

(defun emms-sort-track-name-less-p (a b)
  "Return non-nil if the track name of A sorts before B."
  (string< (emms-track-name a)
           (emms-track-name b)))

(defun emms-ensure-player-playing-p ()
  "Raise an error if no player is playing right now."
  (when (not emms-player-playing-p)
    (error "No EMMS player playing right now")))

(defun emms-completing-read (&rest args)
  "Read a string in the minibuffer, with completion.
Set `emms-completing-read' to determine which function to use.

See `completing-read' for a description of ARGS."
  (apply emms-completing-read-function args))


;;; Compatibility functions

(require 'emms-compat)


;;; Utility functions

(defun emms-insert-file-contents (filename &optional visit)
  "Insert the contents of file FILENAME after point.
Do character code conversion and end-of-line conversion, but none
of the other unnecessary things like format decoding or
`find-file-hook'.

If VISIT is non-nil, the buffer's visited filename
and last save file modtime are set, and it is marked unmodified.
If visiting and the file does not exist, visiting is completed
before the error is signaled."
  (let ((format-alist nil)
        (after-insert-file-functions nil)
        (inhibit-file-name-handlers
         (append '(jka-compr-handler image-file-handler epa-file-handler)
                 inhibit-file-name-handlers))
        (inhibit-file-name-operation 'insert-file-contents))
    (insert-file-contents filename visit)))


;;; Dictionaries

;; This is a simple helper data structure, used by both players
;; and tracks.

(defsubst emms-dictionary (name)
  "Create a new dictionary of type NAME."
  (list name))

(defsubst emms-dictionary-type (dict)
  "Return the type of the dictionary DICT."
  (car dict))

(defun emms-dictionary-get (dict name &optional default)
  "Return the value of NAME in DICT."
  (let ((item (assq name (cdr dict))))
    (if item
        (cdr item)
      default)))

(defun emms-dictionary-set (dict name value)
  "Set the value of NAME in DICT to VALUE."
  (let ((item (assq name (cdr dict))))
    (if item
        (setcdr item value)
      (setcdr dict (append (cdr dict)
                           (list (cons name value))))))
  dict)


;;; Tracks

;; This is a simple datatype to store track information.
;; Each track consists of a type (a symbol) and a name (a string).
;; In addition, each track has an associated dictionary of information.

(defun emms-track (type name)
  "Create an EMMS track with type TYPE and name NAME."
  (let ((track (when emms-cache-get-function
                 (funcall emms-cache-get-function type name))))
    (when (not track)
      (setq track (emms-dictionary '*track*))
      ;; Prevent the cache from being called for these two sets
      (let ((emms-cache-modified-function nil))
        (emms-track-set track 'type type)
        (emms-track-set track 'name name))
      (when emms-cache-set-function
        (funcall emms-cache-set-function type name track)))
    ;; run any hooks regardless of a cache hit, as the entry may be
    ;; old
    (run-hook-with-args 'emms-track-initialize-functions track)
    track))

(defun emms-track-p (obj)
  "True if OBJ is an emms track."
  (and (listp obj)
       (eq (car obj) '*track*)))

(defun emms-track-type (track)
  "Return the type of TRACK."
  (emms-track-get track 'type))

(defun emms-track-name (track)
  "Return the name of TRACK."
  (emms-track-get track 'name))

(defun emms-track-get (track name &optional default)
  "Return the value of NAME for TRACK.
If there is no value, return DEFAULT (or nil, if not given)."
  (emms-dictionary-get track name default))

(defun emms-track-set (track name value)
  "Set the value of NAME for TRACK to VALUE."
  (emms-dictionary-set track name value)
  (when emms-cache-modified-function
    (funcall emms-cache-modified-function track)))

(defun emms-track-description (track)
  "Return a description of TRACK.
This function uses the global value for
`emms-track-description-function', rather than anything the
current mode might have set.

Use `emms-track-force-description' instead if you need to insert
a description into a playlist buffer."
  (funcall (default-value 'emms-track-description-function) track))

(defun emms-track-updated (track)
  "Information in TRACK got updated."
  (run-hook-with-args 'emms-track-info-filters track)
  (emms-playlist-track-updated track)
  (run-hook-with-args 'emms-track-updated-functions track))

(defun emms-track-simple-description (track)
  "Simple function to give a user-readable description of a track.
If it's a file track, just return the file name.  Otherwise,
return the type and the name with a colon in between.
Hex-encoded characters in URLs are replaced by the decoded
character."
  (let ((type (emms-track-type track)))
    (cond ((eq 'file type)
           (emms-track-name track))
          ((eq 'url type)
           (emms-format-url-track-name (emms-track-name track)))
          (t (concat (symbol-name type)
                     ": " (emms-track-name track))))))

(defun emms-format-url-track-name (name)
  "Format URL track name for better readability."
  (url-unhex-string name))

(defun emms-track-force-description (track)
  "Always return text that describes TRACK.
This is used when inserting a description into a buffer.

The reason for this is that if no text was returned (i.e. the
user defined a track function that returned nil or the empty
string), a confusing error message would result."
  (let ((desc (funcall emms-track-description-function track)))
    (if (and (stringp desc) (not (string= desc "")))
        desc
      (emms-track-simple-description track))))


;;; The Playlist

;; Playlists are stored in buffers.  The current playlist buffer is
;; remembered in the `emms-playlist' variable.  The buffer consists of
;; any kind of data.  Strings of text with a `emms-track' property are
;; the tracks in the buffer.

(defvar emms-playlist-buffers nil
  "The list of EMMS playlist buffers.
You should use the `emms-playlist-buffer-list' function to
retrieve a current list of EMMS buffers.  Never use this variable
for that purpose.")

(defvar emms-playlist-selected-marker nil
  "The marker for the currently selected track.")
(make-variable-buffer-local 'emms-playlist-selected-marker)

(defvar emms-playlist-buffer-p nil
  "Non-nil if the current buffer is an EMMS playlist.")
(make-variable-buffer-local 'emms-playlist-buffer-p)

(defun emms-playlist-ensure-playlist-buffer ()
  "Throw an error if we're not in a playlist-buffer."
  (when (not emms-playlist-buffer-p)
    (error "Not an EMMS playlist buffer")))

(defun emms-playlist-set-playlist-buffer (&optional buffer)
  "Set the current playlist buffer."
  (interactive
   (list (let* ((buf-list (mapcar #'(lambda (buf)
                                      (list (buffer-name buf)))
                                  (emms-playlist-buffer-list)))
                (default (or (and emms-playlist-buffer-p
                                  ;; default to current buffer
                                  (buffer-name))
                             ;; pick shortest buffer name, since it is
                             ;; likely to be a shared prefix
                             (car (sort buf-list
                                        #'(lambda (lbuf rbuf)
                                            (< (length (car lbuf))
                                               (length (car rbuf)))))))))
           (emms-completing-read "Playlist buffer to make current: "
                                 buf-list nil t default))))
  (let ((buf (if buffer
                 (get-buffer buffer)
               (current-buffer))))
    (with-current-buffer buf
      (emms-playlist-ensure-playlist-buffer))
    (setq emms-playlist-buffer buf)
    (when (interactive-p)
      (message "Set current EMMS playlist buffer"))
    buf))

(defun emms-playlist-new (&optional name)
  "Create a new playlist buffer.
The buffer is named NAME, but made unique.  NAME defaults to
`emms-playlist-buffer-name'.  If called interactively, the new
buffer is also selected."
  (interactive)
  (let ((buf (generate-new-buffer (or name
                                      emms-playlist-buffer-name))))
    (with-current-buffer buf
      (when (not (eq major-mode emms-playlist-default-major-mode))
        (funcall emms-playlist-default-major-mode))
      (setq emms-playlist-buffer-p t))
    (add-to-list 'emms-playlist-buffers buf)
    (when (interactive-p)
      (switch-to-buffer buf))
    buf))

(defun emms-playlist-buffer-list ()
  "Return a list of EMMS playlist buffers.
The first element is guaranteed to be the current EMMS playlist
buffer, if it exists, otherwise the slot will be used for the
other EMMS buffers.  The list will be in newest-first order."
  ;; prune dead buffers
  (setq emms-playlist-buffers (emms-delete-if (lambda (buf)
                                                (not (buffer-live-p buf)))
                                              emms-playlist-buffers))
  ;; add new buffers
  (mapc (lambda (buf)
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (when (and emms-playlist-buffer-p
                         (not (memq buf emms-playlist-buffers)))
                (setq emms-playlist-buffers
                      (cons buf emms-playlist-buffers))))))
        (buffer-list))
  ;; force current playlist buffer to head position
  (when (and (buffer-live-p emms-playlist-buffer)
             (not (eq (car emms-playlist-buffers) emms-playlist-buffer)))
    (setq emms-playlist-buffers (cons emms-playlist-buffer
                                      (delete emms-playlist-buffer
                                              emms-playlist-buffers))))
  emms-playlist-buffers)

(defun emms-playlist-current-kill ()
  "Kill the current EMMS playlist buffer and switch to the next one."
  (interactive)
  (when (buffer-live-p emms-playlist-buffer)
    (let ((new (cadr (emms-playlist-buffer-list))))
      (if new
          (let ((old emms-playlist-buffer))
            (setq emms-playlist-buffer new
                  emms-playlist-buffers (cdr emms-playlist-buffers))
            (kill-buffer old)
            (switch-to-buffer emms-playlist-buffer))
        (with-current-buffer emms-playlist-buffer
          (bury-buffer))))))

(defun emms-playlist-current-clear ()
  "Clear the current playlist.
If no current playlist exists, a new one is generated."
  (interactive)
  (if (or (not emms-playlist-buffer)
          (not (buffer-live-p emms-playlist-buffer)))
      (setq emms-playlist-buffer (emms-playlist-new))
    (with-current-buffer emms-playlist-buffer
      (emms-playlist-clear))))

(defun emms-playlist-clear ()
  "Clear the current buffer.
If no playlist exists, a new one is generated."
  (interactive)
  (emms-playlist-ensure-playlist-buffer)
  (let ((inhibit-read-only t))
    (widen)
    (delete-region (point-min)
		   (point-max)))
  (run-hooks 'emms-playlist-cleared-hook))

;;; Point movement within the playlist buffer.
(defun emms-playlist-track-at (&optional pos)
  "Return the track at POS (point if not given), or nil if none."
  (emms-playlist-ensure-playlist-buffer)
  (emms-with-widened-buffer
   (get-text-property (or pos (point))
		      'emms-track)))

(defun emms-playlist-next ()
  "Move to the next track in the current buffer."
  (emms-playlist-ensure-playlist-buffer)
  (let ((next (next-single-property-change (point)
                                           'emms-track)))
    (when (not next)
      (error "No next track"))
    (when (not (emms-playlist-track-at next))
      (setq next (next-single-property-change next 'emms-track)))
    (when (or (not next)
              (= next (point-max)))
      (error "No next track"))
    (goto-char next)))

(defun emms-playlist-previous ()
  "Move to the previous track in the current buffer."
  (emms-playlist-ensure-playlist-buffer)
  (let ((prev (previous-single-property-change (point)
                                               'emms-track)))
    (when (not prev)
      (error "No previous track"))
    (when (not (get-text-property prev 'emms-track))
      (setq prev (or (previous-single-property-change prev 'emms-track)
                     (point-min))))
    (when (or (not prev)
              (not (get-text-property prev 'emms-track)))
      (error "No previous track"))
    (goto-char prev)))

(defun emms-playlist-first ()
  "Move to the first track in the current buffer."
  (emms-playlist-ensure-playlist-buffer)
  (let ((first (condition-case nil
                   (save-excursion
                     (goto-char (point-min))
                     (when (not (emms-playlist-track-at (point)))
                       (emms-playlist-next))
                     (point))
                 (error
                  nil))))
    (if first
        (goto-char first)
      (error "No first track"))))

(defun emms-playlist-last ()
  "Move to the last track in the current buffer."
  (emms-playlist-ensure-playlist-buffer)
  (let ((last (condition-case nil
                  (save-excursion
                    (goto-char (point-max))
                    (emms-playlist-previous)
                    (point))
                (error
                 nil))))
    (if last
        (goto-char last)
      (error "No last track"))))

(defun emms-playlist-delete-track ()
  "Delete the track at point."
  (emms-playlist-ensure-playlist-buffer)
  (funcall emms-playlist-delete-track-function))

;;; Track selection
(defun emms-playlist-selected-track ()
  "Return the currently selected track."
  (emms-playlist-ensure-playlist-buffer)
  (when emms-playlist-selected-marker
    (emms-playlist-track-at emms-playlist-selected-marker)))

(defun emms-playlist-current-selected-track ()
  "Return the currently selected track in the current playlist."
  (with-current-emms-playlist
    (emms-playlist-selected-track)))

(defun emms-playlist-selected-track-at-p (&optional point)
  "Return non-nil if POINT (defaulting to point) is on the selected track."
  (when emms-playlist-selected-marker
    (or (= emms-playlist-selected-marker
           (or point (point)))
        (let ((p (previous-single-property-change (or point (point))
                                                  'emms-track)))
          (when p
            (= emms-playlist-selected-marker
               p))))))

(defun emms-playlist-select (pos)
  "Select the track at POS."
  (emms-playlist-ensure-playlist-buffer)
  (when (not (emms-playlist-track-at pos))
    (error "No track at position %s" pos))
  (when (not emms-playlist-selected-marker)
    (setq emms-playlist-selected-marker (make-marker)))
  (set-marker-insertion-type emms-playlist-selected-marker t)
  (set-marker emms-playlist-selected-marker pos)
  (run-hooks 'emms-playlist-selection-changed-hook))

(defun emms-playlist-select-next ()
  "Select the next track in the current buffer."
  (emms-playlist-ensure-playlist-buffer)
  (save-excursion
    (goto-char (if (and emms-playlist-selected-marker
                        (marker-position emms-playlist-selected-marker))
                   emms-playlist-selected-marker
                 (point-min)))
    (condition-case nil
        (progn
          (if emms-repeat-playlist
              (condition-case nil
                  (emms-playlist-next)
                (error
                 (emms-playlist-first)))
            (emms-playlist-next))
          (emms-playlist-select (point)))
      (error
       (error "No next track in playlist")))))

(defun emms-playlist-current-select-next ()
  "Select the next track in the current playlist."
  (with-current-emms-playlist
    (emms-playlist-select-next)))

(defun emms-playlist-select-previous ()
  "Select the previous track in the current buffer."
  (emms-playlist-ensure-playlist-buffer)
  (save-excursion
    (goto-char (if (and emms-playlist-selected-marker
                        (marker-position emms-playlist-selected-marker))
                   emms-playlist-selected-marker
                 (point-max)))
    (condition-case nil
        (progn
          (if emms-repeat-playlist
              (condition-case nil
                  (emms-playlist-previous)
                (error
                 (emms-playlist-last)))
            (emms-playlist-previous))
          (emms-playlist-select (point)))
      (error
       (error "No previous track in playlist")))))

(defun emms-playlist-current-select-previous ()
  "Select the previous track in the current playlist."
  (with-current-emms-playlist
    (emms-playlist-select-previous)))

(defun emms-playlist-select-random ()
  "Select a random track in the current buffer."
  (emms-playlist-ensure-playlist-buffer)
  ;; FIXME: This is rather inefficient.
  (save-excursion
    (let ((track-indices nil))
      (goto-char (point-min))
      (emms-walk-tracks
        (setq track-indices (cons (point)
                                  track-indices)))
      (setq track-indices (vconcat track-indices))
      (emms-playlist-select (aref track-indices
                                  (random (length track-indices)))))))

(defun emms-playlist-current-select-random ()
  "Select a random track in the current playlist."
  (with-current-emms-playlist
    (emms-playlist-select-random)))

(defun emms-playlist-select-first ()
  "Select the first track in the current buffer."
  (emms-playlist-ensure-playlist-buffer)
  (save-excursion
    (emms-playlist-first)
    (emms-playlist-select (point))))

(defun emms-playlist-current-select-first ()
  "Select the first track in the current playlist."
  (with-current-emms-playlist
    (emms-playlist-select-first)))

(defun emms-playlist-select-last ()
  "Select the last track in the current buffer."
  (emms-playlist-ensure-playlist-buffer)
  (save-excursion
    (emms-playlist-last)
    (emms-playlist-select (point))))

(defun emms-playlist-current-select-last ()
  "Select the last track in the current playlist."
  (with-current-emms-playlist
    (emms-playlist-select-last)))

;;; Playlist manipulation
(defun emms-playlist-insert-track (track)
  "Insert TRACK at the current position into the playlist.
This uses `emms-playlist-insert-track-function'."
  (emms-playlist-ensure-playlist-buffer)
  (funcall emms-playlist-insert-track-function track))

(defun emms-playlist-update-track ()
  "Update TRACK at point.
This uses `emms-playlist-update-track-function'."
  (emms-playlist-ensure-playlist-buffer)
  (funcall emms-playlist-update-track-function))

(defun emms-playlist-insert-source (source &rest args)
  "Insert tracks from SOURCE, supplying ARGS as arguments."
  (emms-playlist-ensure-playlist-buffer)
  (save-restriction
    (narrow-to-region (point)
                      (point))
    (apply source args)
    (run-hooks 'emms-playlist-source-inserted-hook)))

(defun emms-playlist-current-insert-source (source &rest args)
  "Insert tracks from SOURCE in the current playlist.
This is supplying ARGS as arguments to the source."
  (with-current-emms-playlist
    (apply 'emms-playlist-insert-source source args)))

(defun emms-playlist-tracks-in-region (beg end)
  "Return all tracks between BEG and END."
  (emms-playlist-ensure-playlist-buffer)
  (let ((tracks nil))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (emms-walk-tracks
        (setq tracks (cons (emms-playlist-track-at (point))
                           tracks))))
    tracks))

(defun emms-playlist-track-updated (track)
  "Update TRACK in all playlist buffers."
  (mapc (lambda (buf)
          (with-current-buffer buf
            (when emms-playlist-buffer-p
              (save-excursion
                (let ((pos (text-property-any (point-min) (point-max)
                                              'emms-track track)))
                  (while pos
                    (goto-char pos)
                    (emms-playlist-update-track)
                    (setq pos (text-property-any
                               (next-single-property-change (point)
							    'emms-track)
                               (point-max)
                               'emms-track
                               track))))))))
        (buffer-list))
  t)

;;; Simple playlist buffer
(defun emms-playlist-simple-insert-track (track)
  "Insert the description of TRACK at point."
  (emms-playlist-ensure-playlist-buffer)
  (let ((inhibit-read-only t))
    (insert (emms-propertize (emms-track-force-description track)
                             'emms-track track)
            "\n")))

(defun emms-playlist-simple-update-track ()
  "Update the track at point.
Since we don't do anything special with the track anyway, just
ignore this."
  nil)

(defun emms-playlist-simple-delete-track ()
  "Delete the track at point."
  (emms-playlist-ensure-playlist-buffer)
  (when (not (emms-playlist-track-at (point)))
    (error "No track at point"))
  (let ((inhibit-read-only t)
        (region (emms-property-region (point) 'emms-track)))
    (delete-region (car region)
                   (cdr region))))

(defun emms-playlist-simple-shuffle ()
  "Shuffle the whole playlist buffer."
  (emms-playlist-ensure-playlist-buffer)
  (let ((inhibit-read-only t)
        (current nil))
    (widen)
    (when emms-player-playing-p
      (setq current (emms-playlist-selected-track))
      (goto-char emms-playlist-selected-marker)
      (emms-playlist-delete-track))
    (let* ((tracks (vconcat (emms-playlist-tracks-in-region (point-min)
                                                            (point-max))))
           (len (length tracks))
           (i 0))
      (delete-region (point-min)
                     (point-max))
      (run-hooks 'emms-playlist-cleared-hook)
      (emms-shuffle-vector tracks)
      (when current
        (emms-playlist-insert-track current))
      (while (< i len)
        (emms-playlist-insert-track (aref tracks i))
        (setq i (1+ i))))
    (emms-playlist-select-first)
    (goto-char (point-max))))

(defun emms-playlist-simple-sort ()
  "Sort the whole playlist buffer."
  (emms-playlist-ensure-playlist-buffer)
  (widen)
  (let ((inhibit-read-only t)
        (current (emms-playlist-selected-track))
        (tracks (emms-playlist-tracks-in-region (point-min)
                                                (point-max))))
    (delete-region (point-min)
                   (point-max))
    (run-hooks 'emms-playlist-cleared-hook)
    (mapc 'emms-playlist-insert-track
          (sort tracks emms-sort-lessp-function))
    (let ((pos (text-property-any (point-min)
                                  (point-max)
                                  'emms-track current)))
      (if pos
          (emms-playlist-select pos)
        (emms-playlist-first)))))

(defun emms-uniq-list (list stringify)
  "Compare stringfied element of list, and remove duplicate elements."
  ;; This uses a fast append list, keeping a pointer to the last cons
  ;; cell of the list (TAIL).  It might be worthwhile to provide an
  ;; abstraction for this eventually.
  (let* ((hash (make-hash-table :test 'equal))
         (result (cons nil nil))
         (tail result))
    (dolist (element list)
      (let ((str (funcall stringify element)))
        (when (not (gethash str hash))
          (setcdr tail (cons element nil))
          (setq tail (cdr tail)))
        (puthash str t hash)))
    (cdr result)))

(defun emms-playlist-simple-uniq ()
  "Remove duplicate tracks."
  ;; TODO: This seems unnecessarily destructive.
  (emms-playlist-ensure-playlist-buffer)
  (widen)
  (let ((inhibit-read-only t)
        (current (emms-playlist-selected-track))
        (tracks (emms-playlist-tracks-in-region (point-min)
                                                (point-max))))
    (delete-region (point-min) (point-max))
    (run-hooks 'emms-playlist-cleared-hook)
    (mapc 'emms-playlist-insert-track
          (nreverse
           (emms-uniq-list tracks 'emms-track-name)))
    (let ((pos (text-property-any (point-min)
                                  (point-max)
                                  'emms-track current)))
      (if pos
          (emms-playlist-select pos)
        (emms-playlist-first)))))

;;; Helper functions
(defun emms-property-region (pos prop)
  "Return a pair of the beginning and end of the property PROP at POS.
If POS does not contain PROP, try to find PROP just before POS."
  (let (begin end)
    (if (and (> pos (point-min))
             (get-text-property (1- pos) prop))
        (setq begin (previous-single-property-change (1- pos) prop))
      (if (get-text-property pos prop)
          (setq begin pos)
        (error "Cannot find the %s property at the given position" prop)))
    (if (get-text-property pos prop)
        (setq end (next-single-property-change pos prop))
      (if (and (> pos (point-min))
               (get-text-property (1- pos) prop))
          (setq end pos)
        (error "Cannot find the %s property at the given position" prop)))
    (cons (or begin (point-min))
          (or end (point-max)))))

(defun emms-shuffle-vector (vector)
  "Shuffle VECTOR."
  (let ((i (- (length vector) 1)))
    (while (>= i 0)
      (let* ((r (random (1+ i)))
             (old (aref vector r)))
        (aset vector r (aref vector i))
        (aset vector i old))
      (setq i (- i 1))))
  vector)


;;; Sources

;; A source is just a function which is called in a playlist buffer.
;; It should use `emms-playlist-insert-track' to insert the tracks it
;; knows about.
;;
;; The define-emms-source macro also defines functions
;; emms-play-SOURCE and emms-add-SOURCE.  The former will replace the
;; current playlist, while the latter will add to the end.

(defmacro define-emms-source (name arglist &rest body)
  "Define a new EMMS source called NAME.
This macro defines three functions: `emms-source-NAME',
`emms-play-NAME' and `emms-add-NAME'.  BODY should use
`emms-playlist-insert-track' to insert all tracks to be played,
which is exactly what `emms-source-NAME' will do.  The other two
functions will be simple wrappers around `emms-source-NAME'; any
`interactive' form that you specify in BODY will end up in these.
See emms-source-file.el for some examples."
  (let ((source-name (intern (format "emms-source-%s" name)))
        (source-play (intern (format "emms-play-%s" name)))
        (source-add (intern (format "emms-add-%s" name)))
        (source-insert (intern (format "emms-insert-%s" name)))
        (docstring "A source of tracks for EMMS.")
        (interactive nil)
        (call-args (delete '&rest
                           (delete '&optional
                                   arglist))))
    (when (stringp (car body))
      (setq docstring (car body)
            body (cdr body)))
    (when (eq 'interactive (caar body))
      (setq interactive (car body)
            body (cdr body)))
    `(progn
       (defun ,source-name ,arglist
         ,docstring
         ,@body)
       (defun ,source-play ,arglist
         ,docstring
         ,interactive
         (if current-prefix-arg
             (let ((current-prefix-arg nil))
               (emms-source-add ',source-name ,@call-args))
           (emms-source-play ',source-name ,@call-args)))
       (defun ,source-add ,arglist
         ,docstring
         ,interactive
         (if current-prefix-arg
             (let ((current-prefix-arg nil))
               (emms-source-play ',source-name ,@call-args))
           (emms-source-add ',source-name ,@call-args)))
       (defun ,source-insert ,arglist
         ,docstring
         ,interactive
         (emms-source-insert ',source-name ,@call-args)))))

(defun emms-source-play (source &rest args)
  "Play the tracks of SOURCE, after first clearing the EMMS playlist."
  (emms-stop)
  (emms-playlist-current-clear)
  (apply 'emms-playlist-current-insert-source source args)
  (emms-playlist-current-select-first)
  (emms-start))

(defun emms-source-add (source &rest args)
  "Add the tracks of SOURCE at the current position in the playlist."
  (with-current-emms-playlist
    (save-excursion
      (goto-char (point-max))
      (apply 'emms-playlist-current-insert-source source args))
    (when (or (not emms-playlist-selected-marker)
              (not (marker-position emms-playlist-selected-marker)))
      (emms-playlist-select-first))))

(defun emms-source-insert (source &rest args)
  "Insert the tracks from SOURCE in the current buffer."
  (if (not emms-playlist-buffer-p)
      (error "Not in an EMMS playlist buffer")
    (apply 'emms-playlist-insert-source source args)))

;;; User-defined playlists
;;; FIXME: Shuffle is bogus here! (because of narrowing)
(defmacro define-emms-combined-source (name shufflep sources)
  "Define a `emms-play-X' and `emms-add-X' function for SOURCES."
  `(define-emms-source ,name ()
     "An EMMS source for a tracklist."
     (interactive)
     (mapc (lambda (source)
             (apply (car source)
                    (cdr source)))
           ,sources)
     ,(when shufflep
        '(save-restriction
           (widen)
           (emms-shuffle)))))


;;; Players

;; A player is a data structure created by `emms-player'.
;; See the docstring of that function for more information.

(defvar emms-player-stopped-p nil
  "Non-nil if the last EMMS player was stopped by the user.")

(defun emms-player (start stop playablep)
  "Create a new EMMS player.
The start function will be START, and the stop function STOP.
PLAYABLEP should return non-nil for tracks that this player can
play.

When trying to play a track, EMMS walks through
`emms-player-list'.  For each player, it calls the PLAYABLEP
function.  The player corresponding to the first PLAYABLEP
function that returns non-nil is used to play the track.  To
actually play the track, EMMS calls the START function, passing
the chosen track as a parameter.

If the user tells EMMS to stop playing, the STOP function is
called.  Once the player has finished playing, it should call
`emms-player-stopped' to let EMMS know."
  (let ((p (emms-dictionary '*player*)))
    (emms-player-set p 'start start)
    (emms-player-set p 'stop stop)
    (emms-player-set p 'playablep playablep)
    p))

(defun emms-player-get (player name &optional inexistent)
  "Return the value of entry NAME in PLAYER."
  (let ((p (if (symbolp player)
               (symbol-value player)
             player)))
    (emms-dictionary-get p name inexistent)))

(defun emms-player-set (player name value)
  "Set the value of entry NAME in PLAYER to VALUE."
  (let ((p (if (symbolp player)
               (symbol-value player)
             player)))
    (emms-dictionary-set p name value)))

(defun emms-player-for (track)
  "Return an EMMS player capable of playing TRACK.
This will be the first player whose PLAYABLEP function returns
non-nil, or nil if no such player exists."
  (let ((lis emms-player-list))
    (while (and lis
                (not (funcall (emms-player-get (car lis) 'playablep)
                              track)))
      (setq lis (cdr lis)))
    (if lis
        (car lis)
      nil)))

(defun emms-player-start (track)
  "Start playing TRACK."
  (if emms-player-playing-p
      (error "A player is already playing")
    (let ((player (emms-player-for track)))
      (if (not player)
          (error "Don't know how to play track: %S" track)
        ;; Change default-directory so we don't accidentally block any
        ;; directories the current buffer was visiting.
        (let ((default-directory "/"))
          (funcall (emms-player-get player 'start)
                   track))))))

(defun emms-player-started (player)
  "Declare that the given EMMS PLAYER has started.
This should only be done by the current player itself."
  (setq emms-player-playing-p player
        emms-player-paused-p  nil)
  (run-hooks 'emms-player-started-hook))

(defun emms-player-stop ()
  "Stop the current EMMS player."
  (when emms-player-playing-p
    (let ((emms-player-stopped-p t))
      (funcall (emms-player-get emms-player-playing-p 'stop)))
    (setq emms-player-playing-p nil)))

(defun emms-player-stopped ()
  "Declare that the current EMMS player is finished.
This should only be done by the current player itself."
  (setq emms-player-playing-p nil)
  (if emms-player-stopped-p
      (run-hooks 'emms-player-stopped-hook)
    (sleep-for emms-player-delay)
    (run-hooks 'emms-player-finished-hook)
    (funcall emms-player-next-function)))

(defun emms-player-pause ()
  "Pause the current EMMS player."
  (cond
   ((not emms-player-playing-p)
    (error "Can't pause player, nothing is playing"))
   (emms-player-paused-p
    (let ((resume (emms-player-get emms-player-playing-p 'resume))
          (pause (emms-player-get emms-player-playing-p 'pause)))
      (cond
       (resume
        (funcall resume))
       (pause
        (funcall pause))
       (t
        (error "Player does not know how to pause"))))
    (setq emms-player-paused-p nil)
    (run-hooks 'emms-player-paused-hook))
   (t
    (let ((pause (emms-player-get emms-player-playing-p 'pause)))
      (if pause
          (funcall pause)
        (error "Player does not know how to pause")))
    (setq emms-player-paused-p t)
    (run-hooks 'emms-player-paused-hook))))

(defun emms-player-seek (seconds)
  "Seek the current player by SECONDS seconds.
This can be a floating point number for fractions of a second, or
negative to seek backwards."
  (if (not emms-player-playing-p)
      (error "Can't seek player, nothing playing right now")
    (let ((seek (emms-player-get emms-player-playing-p 'seek)))
      (if (not seek)
          (error "Player does not know how to seek")
        (funcall seek seconds)
        (run-hook-with-args 'emms-player-seeked-functions seconds)))))

(defun emms-player-seek-to (seconds)
  "Seek the current player to SECONDS seconds.
This can be a floating point number for fractions of a second, or
negative to seek backwards."
  (if (not emms-player-playing-p)
      (error "Can't seek-to player, nothing playing right now")
    (let ((seek (emms-player-get emms-player-playing-p 'seek-to)))
      (if (not seek)
          (error "Player does not know how to seek-to")
        (funcall seek seconds)
        (run-hook-with-args 'emms-player-time-set-functions seconds)))))

(provide 'emms)
;;; emms.el ends here
