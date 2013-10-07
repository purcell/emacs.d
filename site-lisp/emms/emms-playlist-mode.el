;;; emms-playlist-mode.el --- Playlist mode for Emms.

;; Copyright (C) 2005, 2006, 2007, 2008, 2009  Free Software Foundation, Inc.

;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>

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
;;;
;;; This is a method of displaying and manipulating the different Emms
;;; playlist buffers.
;;;
;;; Emms developer's motto: "When forcer says (require 'jump) we say
;;; (funcall #'jump height)"

;;; Code:

;;; --------------------------------------------------------
;;; Variables
;;; --------------------------------------------------------

(require 'emms)
(condition-case nil
    (require 'overlay)
  (error nil))
(require 'emms-source-playlist)

(defvar emms-playlist-mode-hook nil
  "Emms playlist mode hook.")

(defvar emms-playlist-mode-selected-overlay nil
  "Last selected track.  Use for updating the display.")

(defvar emms-playlist-mode-switched-buffer nil
  "Last buffer visited before calling `emms-playlist-mode-switch-buffer'.")

(defvar emms-playlist-mode-popup-enabled nil
  "True when the playlist was called as a popup window.")

(make-variable-buffer-local
 'emms-playlist-mode-selected-overlay)

(defgroup emms-playlist-mode nil
  "*The Emacs Multimedia System playlist mode."
  :prefix "emms-playlist-mode-"
  :group 'emms)

(defcustom emms-playlist-mode-open-playlists nil
  "*Determine whether to open playlists in a new EMMS buffer on RET.
This is useful if you have a master playlist buffer that is
composed of other playlists."
  :type 'boolean
  :group 'emms-playlist-mode)

(defcustom emms-playlist-mode-window-width 25
  "*Determine the width of the Emms popup window.
The value should a positive integer."
  :type 'integer
  :group 'emms-playlist-mode)

(defcustom emms-playlist-mode-center-when-go nil
  "*Determine whether to center on the currently selected track.
This is true for every invocation of `emms-playlist-mode-go'."
  :type 'boolean
  :group 'emms-playlist-mode)

;;; --------------------------------------------------------
;;; Faces
;;; --------------------------------------------------------

(defface emms-playlist-track-face
  '((((class color) (background dark))
     (:foreground "DarkSeaGreen"))
    (((class color) (background light))
     (:foreground "Blue"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "Blue")))
  "Face for the tracks in a playlist buffer."
  :group 'emms-playlist-mode)

(defface emms-playlist-selected-face
  '((((class color) (background dark))
     (:foreground "SteelBlue3"))
    (((class color) (background light))
     (:background "blue3" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "blue3")))
  "Face for highlighting the selected track."
  :group 'emms-playlist-mode)

;;; --------------------------------------------------------
;;; Keys
;;; --------------------------------------------------------

(defconst emms-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-x C-s") 'emms-playlist-save)
    (define-key map (kbd "C-y") 'emms-playlist-mode-yank)
    (define-key map (kbd "C-k") 'emms-playlist-mode-kill-track)
    (define-key map (kbd "C-w") 'emms-playlist-mode-kill)
    (define-key map (kbd "C-_") 'emms-playlist-mode-undo)
    (define-key map (kbd "C-/") 'emms-playlist-mode-undo)
    (define-key map (kbd "C-x u") 'emms-playlist-mode-undo)
    (define-key map (kbd "C-n") 'next-line)
    (define-key map (kbd "C-p") 'previous-line)
    (define-key map (kbd "C-j") 'emms-playlist-mode-insert-newline)
    (define-key map (kbd "M-y") 'emms-playlist-mode-yank-pop)
    (define-key map (kbd "M-<") 'emms-playlist-mode-first)
    (define-key map (kbd "M->") 'emms-playlist-mode-last)
    (define-key map (kbd "M-n") 'emms-playlist-mode-next)
    (define-key map (kbd "M-p") 'emms-playlist-mode-previous)
    (define-key map (kbd "a") 'emms-playlist-mode-add-contents)
    (define-key map (kbd "b") 'emms-playlist-set-playlist-buffer)
    (define-key map (kbd "D") 'emms-playlist-mode-kill-entire-track)
    (define-key map (kbd "n") 'emms-next)
    (define-key map (kbd "p") 'emms-previous)
    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd ">") 'emms-seek-forward)
    (define-key map (kbd "<") 'emms-seek-backward)
    (define-key map (kbd "P") 'emms-pause)
    (define-key map (kbd "s") 'emms-stop)
    (define-key map (kbd "f") 'emms-show)
    (define-key map (kbd "c") 'emms-playlist-mode-center-current)
    (define-key map (kbd "q") 'emms-playlist-mode-bury-buffer)
    (define-key map (kbd "k") 'emms-playlist-mode-current-kill)
    (define-key map (kbd "?") 'describe-mode)
    (define-key map (kbd "r") 'emms-random)
    (define-key map (kbd "C") 'emms-playlist-mode-clear)
    (define-key map (kbd "d") 'emms-playlist-mode-goto-dired-at-point)
    (define-key map (kbd "<mouse-2>") 'emms-playlist-mode-play-current-track)
    (define-key map (kbd "RET") 'emms-playlist-mode-play-smart)
    map)
  "Keymap for `emms-playlist-mode'.")

(defmacro emms-playlist-mode-move-wrapper (name fun)
  "Create a function NAME which is an `interactive' version of FUN.

NAME should be a symbol.
FUN should be a function."
  `(defun ,name ()
     ,(format "Interactive wrapper around `%s' for playlist-mode."
	      fun)
     (interactive)
     (,fun)))

(emms-playlist-mode-move-wrapper emms-playlist-mode-first
				 emms-playlist-first)

(emms-playlist-mode-move-wrapper emms-playlist-mode-select-next
				 emms-playlist-next)

(emms-playlist-mode-move-wrapper emms-playlist-mode-select-previous
				 emms-playlist-previous)

(defun emms-playlist-mode-bury-buffer ()
  "Wrapper around `bury-buffer' for popup windows."
  (interactive)
  (if emms-playlist-mode-popup-enabled
      (unwind-protect
	  (delete-window)
	(setq emms-playlist-mode-popup-enabled nil))
    (bury-buffer)))

(defun emms-playlist-mode-current-kill ()
  "If the current buffer is an EMMS playlist buffer, kill it.
Otherwise, kill the current EMMS playlist buffer."
  (interactive)
  (if (and emms-playlist-buffer-p
           (not (eq (current-buffer) emms-playlist-buffer)))
      (kill-buffer (current-buffer))
    (emms-playlist-current-kill)))

(defun emms-playlist-mode-clear ()
  "If the current buffer is an EMMS playlist buffer, clear it.
Otherwise, clear the current EMMS playlist buffer."
  (interactive)
  (if (and emms-playlist-buffer-p
           (not (eq (current-buffer) emms-playlist-buffer)))
      (let ((inhibit-read-only t))
        (widen)
        (delete-region (point-min) (point-max)))
    (emms-playlist-clear)))

(defun emms-playlist-mode-last ()
  "Move to directly after the last track in the current buffer."
  (interactive)
  (emms-playlist-ensure-playlist-buffer)
  (let ((last (condition-case nil
                  (save-excursion
                    (goto-char (point-max))
                    (point))
                (error
                 nil))))
    (if last
        (goto-char last)
      (error "No last track"))))

(defun emms-playlist-mode-center-current ()
  "Move point to the currently selected track."
  (interactive)
  (goto-char (if emms-playlist-mode-selected-overlay
                 (overlay-start emms-playlist-mode-selected-overlay)
               (point-min))))

(defun emms-playlist-mode-play-current-track ()
  "Start playing track at point."
  (interactive)
  (emms-playlist-set-playlist-buffer (current-buffer))
  (unless (emms-playlist-track-at (point))
    (emms-playlist-next))
  (emms-playlist-select (point))
  (when emms-player-playing-p
    (emms-stop))
  (emms-start))

(defun emms-playlist-mode-play-smart ()
  "Determine the best operation to take on the current track.

If on a playlist, and `emms-playlist-mode-open-playlists' is
non-nil, load the playlist at point into a new buffer.

Otherwise play the track immediately."
  (interactive)
  (save-excursion
    ;; move to the start of the line, in case the point is on the \n,
    ;; which isn't propertized
    (emms-move-beginning-of-line nil)
    (if (not emms-playlist-mode-open-playlists)
        (emms-playlist-mode-play-current-track)
      (unless (emms-playlist-track-at)
        (emms-playlist-next))
      (let* ((track (emms-playlist-track-at))
             (name (emms-track-get track 'name))
             (type (emms-track-get track 'type)))
        (if (or (eq type 'playlist)
                (and (eq type 'file)
                     (string-match "\\.\\(m3u\\|pls\\)\\'" name)))
            (emms-playlist-mode-load-playlist)
          (emms-playlist-mode-play-current-track))))))

(defun emms-playlist-mode-switch-buffer ()
  "Switch to the playlist buffer and then switch back if called again.

This function switches to the current Emms playlist buffer and
remembers the buffer switched from. When called again the
function switches back to the remembered buffer."
  (interactive)
  (if (eq (current-buffer)
	  emms-playlist-buffer)
      (switch-to-buffer emms-playlist-mode-switched-buffer)
    (setq emms-playlist-mode-switched-buffer (current-buffer))
    (switch-to-buffer emms-playlist-buffer)))

(defun emms-playlist-mode-insert-newline ()
  "Insert a newline at point."
  (interactive)
  (emms-with-inhibit-read-only-t
   (newline)))

(defun emms-playlist-mode-undo ()
  "Wrapper around `undo'."
  (interactive)
  (emms-with-inhibit-read-only-t
   (undo)))

(defun emms-playlist-mode-add-contents ()
  "Add files in the playlist at point to the current playlist buffer.

If we are in the current playlist, make a new playlist buffer and
set it as current."
  (interactive)
  (save-excursion
    (emms-move-beginning-of-line nil)
    (unless (emms-playlist-track-at)
      (emms-playlist-next))
    (let* ((track (emms-playlist-track-at))
           (name (emms-track-get track 'name))
           (type (emms-track-get track 'type))
           (playlist-p (or (eq type 'playlist)
                           (and (eq type 'file)
                                (save-match-data
                                  (string-match "\\.\\(m3u\\|pls\\)\\'"
                                                name))))))
      (emms-playlist-select (point))
      (unless (and (buffer-live-p emms-playlist-buffer)
                   (not (eq (current-buffer) emms-playlist-buffer)))
        (setq emms-playlist-buffer
              (emms-playlist-set-playlist-buffer (emms-playlist-new))))
      (with-current-emms-playlist
        (goto-char (point-max))
        (when playlist-p
          (insert (emms-track-force-description track) "\n"))
        (let ((beg (point)))
          (if playlist-p
              (emms-add-playlist name)
            (let ((func (intern (concat "emms-add-" (symbol-name type)))))
              (if (functionp func)
                  (funcall func name)
                ;; fallback
                (emms-add-file name))))
          (when playlist-p
            (goto-char (point-max))
            (while (progn
                     (forward-line -1)
                     (>= (point) beg))
              (insert "  ")))
          (goto-char (point-min))
          (message "Added %s" (symbol-name type)))))))

(defun emms-playlist-mode-goto-dired-at-point ()
  "Visit the track at point in a `dired' buffer."
  (interactive)
  (let ((track (emms-playlist-track-at)))
    (if track
	(let ((name (emms-track-get track 'name))
	      (type (emms-track-get track 'type)))
	  (if (eq type 'file)
	      (dired (file-name-directory name))
	    (error "Can't visit this track type in Dired")))
      (error "No track at point"))))

;;; --------------------------------------------------------
;;; Killing and yanking
;;; --------------------------------------------------------

(defun emms-playlist-mode-between-p (p a b)
  "Return t if P is a point between points A and B."
  (and (<= a p)
       (<= p b)))

;; d
(defun emms-playlist-mode-kill-entire-track ()
  "Kill track at point, including newline."
  (interactive)
  (let ((kill-whole-line t))
    (emms-playlist-mode-kill-track)))

;; C-k
;;
;; Currently this kills as regular GNU/Emacs would and not like a
;; typical music player would.
(defun emms-playlist-mode-kill-track ()
  "Kill track at point."
  (interactive)
  (emms-with-inhibit-read-only-t
   (let ((track (emms-playlist-track-at)))
     (if track
	 (let ((track-region (emms-property-region (point)
						   'emms-track)))
	   (when (and emms-player-playing-p
		      (emms-playlist-selected-track-at-p))
	     (emms-stop)
             (delete-overlay emms-playlist-mode-selected-overlay)
             (setq emms-playlist-mode-selected-overlay nil))
	   (kill-line))
       (kill-line)))))

;; C-w
(defun emms-playlist-mode-kill ()
  "Kill from mark to point."
  (interactive)
  (emms-with-inhibit-read-only-t
   ;; Are we killing the playing/selected track?
   (when (and (markerp emms-playlist-selected-marker)
              (emms-playlist-mode-between-p
               (marker-position emms-playlist-selected-marker)
               (region-beginning)
               (region-end)))
     (emms-stop)
     (delete-overlay emms-playlist-mode-selected-overlay)
     (setq emms-playlist-mode-selected-overlay nil))
   (kill-region (region-beginning)
                (region-end))))

(defun emms-playlist-mode-correct-previous-yank ()
  "Fix the previous yank if needed."
  (when (and (< (point-at-bol) (point))
	     (< (point) (point-at-eol)))
    (newline)))

;; C-y
(defun emms-playlist-mode-yank ()
  "Yank into the playlist buffer."
  (interactive)
  (emms-with-inhibit-read-only-t
   (goto-char (point-at-bol))
   (yank)
   (emms-playlist-mode-correct-previous-yank)))

;; M-y
(defun emms-playlist-mode-yank-pop ()
  "Cycle through the kill-ring."
  (interactive)
  (emms-with-inhibit-read-only-t
   (yank-pop nil)
   (emms-playlist-mode-correct-previous-yank)))

;;; --------------------------------------------------------
;;; Overlay
;;; --------------------------------------------------------

(defun emms-playlist-mode-overlay-selected ()
  "Place an overlay over the currently selected track."
  (when emms-playlist-selected-marker
    (save-excursion
      (goto-char emms-playlist-selected-marker)
      (let ((reg (emms-property-region (point) 'emms-track)))
        (if emms-playlist-mode-selected-overlay
            (move-overlay emms-playlist-mode-selected-overlay
                          (car reg)
                          (cdr reg))
          (setq emms-playlist-mode-selected-overlay
                (make-overlay (car reg)
                              (cdr reg)
                              nil t nil))
          (overlay-put emms-playlist-mode-selected-overlay
                       'face 'emms-playlist-selected-face)
          (overlay-put emms-playlist-mode-selected-overlay
                       'evaporate t))))))

;;; --------------------------------------------------------
;;; Saving/Restoring
;;; --------------------------------------------------------

(defun emms-playlist-mode-open-buffer (filename)
  "Opens a previously saved playlist buffer.

It creates a buffer called \"filename\", and restores the contents
of the saved playlist inside."
  (interactive "fFile: ")
  (let* ((s)
	 (buffer (get-buffer-create filename))
	 (name   (buffer-name buffer)))
    (with-current-buffer buffer
      (emms-insert-file-contents filename)
      (setq s (read (buffer-string))))
    (kill-buffer buffer)
    (with-current-buffer (emms-playlist-new name)
      (emms-with-inhibit-read-only-t
       (insert s)
       (goto-char (point-min))
       (emms-walk-tracks
         (emms-playlist-update-track)))
      (emms-playlist-first)
      (emms-playlist-select (point))
      (switch-to-buffer (current-buffer)))))

(defun emms-playlist-mode-load-playlist ()
  "Load the playlist into a new EMMS buffer.
This preserves the current EMMS buffer."
  (interactive)
  (let* ((track (emms-playlist-track-at))
         (name (emms-track-get track 'name))
         (type (emms-track-get track 'type)))
    (emms-playlist-select (point))
    (run-hooks 'emms-player-stopped-hook)
    (switch-to-buffer
     (emms-playlist-set-playlist-buffer (emms-playlist-new)))
    (emms-add-playlist name)))

;;; --------------------------------------------------------
;;; Local functions
;;; --------------------------------------------------------

(defun emms-playlist-mode-insert-track (track &optional no-newline)
  "Insert the description of TRACK at point.
When NO-NEWLINE is non-nil, do not insert a newline after the track."
  (emms-playlist-ensure-playlist-buffer)
  (emms-with-inhibit-read-only-t
   (insert (emms-propertize (emms-track-force-description track)
                            'emms-track track
                            'face 'emms-playlist-track-face))
   (when (emms-playlist-selected-track-at-p)
     (emms-playlist-mode-overlay-selected))
   (unless no-newline
     (insert "\n"))))

(defun emms-playlist-mode-update-track-function ()
  "Update the track display at point."
  (emms-playlist-ensure-playlist-buffer)
  (emms-with-inhibit-read-only-t
   (let ((track-region (emms-property-region (point)
					     'emms-track))
	 (track (get-text-property (point)
				   'emms-track))
	 (selectedp (emms-playlist-selected-track-at-p)))
     (save-excursion
       (delete-region (car track-region)
		      (cdr track-region))
       (when selectedp
	 (delete-overlay emms-playlist-mode-selected-overlay)
	 (setq emms-playlist-mode-selected-overlay nil))
       (emms-playlist-mode-insert-track track t))
     (when selectedp
       (emms-playlist-select (point))))))

;;; --------------------------------------------------------
;;; Entry
;;; --------------------------------------------------------

(defun emms-playlist-mode-go ()
  "Switch to the current emms-playlist buffer and use emms-playlist-mode."
  (interactive)
  (if (or (null emms-playlist-buffer)
	  (not (buffer-live-p emms-playlist-buffer)))
      (error "No current Emms buffer")
    (switch-to-buffer emms-playlist-buffer)
    (when (and (not (eq major-mode 'emms-playlist-mode))
	       emms-playlist-buffer-p)
      (emms-playlist-mode))
    (when emms-playlist-mode-center-when-go
      (emms-playlist-mode-center-current))))

(defun emms ()
  "Switch to the current emms-playlist buffer, use
emms-playlist-mode and query for a directory tree to add to the
playlist."
  (interactive)
  (if (or (null emms-playlist-buffer)
	  (not (buffer-live-p emms-playlist-buffer)))
      (call-interactively 'emms-add-file))
  (emms-playlist-mode-go))

(defun emms-playlist-mode-go-popup (&optional window-width)
  "Popup emms-playlist buffer as a side window.

Default value for WINDOW-WIDTH is `emms-playlist-mode-window-width'.
WINDOW-WIDTH should be a positive integer."
  (interactive)
  (setq emms-playlist-mode-window-width
        (round (or window-width emms-playlist-mode-window-width)))
  (split-window-horizontally (- emms-playlist-mode-window-width))
  (other-window 1)
  (emms-playlist-mode-go)
  (setq emms-playlist-mode-popup-enabled t))

(defun emms-playlist-mode-next (arg)
  "Navigate between playlists."
  (interactive "p")
  (let ((playlists (emms-playlist-buffer-list))
        bufs idx)
    (if playlists
        ;; if not in playlist mode, switch to emms-playlist-buffer
        (if (not (member (current-buffer) playlists))
            (switch-to-buffer (if (and emms-playlist-buffer
                                       (buffer-live-p emms-playlist-buffer))
                                  emms-playlist-buffer
                                (car playlists)))
          (setq bufs (member (current-buffer) playlists))
          (setq idx
                (+ (- (length playlists) (length bufs))
                   (if (> arg 0) 1 -1)))
          (switch-to-buffer (nth (mod idx (length playlists)) playlists)))
      (message "No playlist found!"))))
(defun emms-playlist-mode-previous (arg)
  (interactive "p")
  (emms-playlist-mode-next (- arg)))

(defun emms-playlist-mode-startup ()
  "Instigate emms-playlist-mode on the current buffer."
  ;; when there is neither a current emms track or a playing one...
  (when (not (or emms-playlist-selected-marker
		 emms-player-playing-p))
    ;; ...then stop the player.
    (emms-stop)
    ;; why select the first track?
    (when emms-playlist-buffer-p
      (emms-playlist-select-first)))
  ;; when there is a selected track.
  (when emms-playlist-selected-marker
    (emms-playlist-mode-overlay-selected))
  (emms-with-inhibit-read-only-t
   (add-text-properties (point-min)
                        (point-max)
                        '(face emms-playlist-track-face)))
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq buffer-undo-list nil))

;;;###autoload
(defun emms-playlist-mode ()
  "A major mode for Emms playlists.
\\{emms-playlist-mode-map}"
  (interactive)
  (let ((val emms-playlist-buffer-p))
    (kill-all-local-variables)
    (setq emms-playlist-buffer-p val))

  (use-local-map emms-playlist-mode-map)
  (setq major-mode 'emms-playlist-mode
	mode-name "EMMS")

  (setq emms-playlist-insert-track-function
	'emms-playlist-mode-insert-track)
  (setq emms-playlist-update-track-function
	'emms-playlist-mode-update-track-function)
  (add-hook 'emms-playlist-selection-changed-hook
	    'emms-playlist-mode-overlay-selected)

  (emms-playlist-mode-startup)

  (run-hooks 'emms-playlist-mode-hook))

(provide 'emms-playlist-mode)

;;; emms-playlist-mode.el ends here
