;;; emms-metaplaylist-mode.el --- A major mode for lists of Emms playlists

;; Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:
;;
;; `emms-metaplaylist-mode' creates an interactive list of all the
;; Emms playlist buffers. The currently active buffer is
;; highlighted. You can choose a buffer from the list with RET and get
;; taken there.

;;; Code:

(require 'emms)
(require 'emms-playlist-mode)

;;; --------------------------------------------------------
;;; Variables, customisation and faces
;;; --------------------------------------------------------

(defgroup emms-metaplaylist-mode nil
  "*The Emacs Multimedia System meta-playlist mode."
  :prefix "emms-metaplaylist-mode-"
  :group 'multimedia)

(defcustom emms-metaplaylist-mode-buffer-name "*Emms Playlists*"
  "*Name of the buffer in which Emms playlists will be listed."
  :type 'string
  :group 'emms-metaplaylist-mode)

(defcustom emms-metaplaylist-mode-hooks nil
  "*List of hooks to run on entry to emms-metaplaylist-mode."
  :type 'list
  :group 'emms-metaplaylist-mode)

(defface emms-metaplaylist-mode-face
  '((((class color) (background dark))
     (:foreground "AntiqueWhite3"))
    (((class color) (background light))
     (:foreground "red3"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "WhiteSmoke")))
  "Face for the buffer names in the playlists buffer."
  :group 'emms-metaplaylist-mode)

(defface emms-metaplaylist-mode-current-face
  '((((class color) (background dark))
     (:foreground "red2"))
    (((class color) (background light))
     (:background "red3" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "red3")))
  "Face for the current buffer name in the playlists buffer."
  :group 'emms-metaplaylist-mode)

;;; --------------------------------------------------------
;;; Keymap
;;; --------------------------------------------------------

(defconst emms-metaplaylist-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "RET") 'emms-metaplaylist-mode-goto-current)
    (define-key map (kbd "q") 'kill-this-buffer)
    (define-key map (kbd "?") 'describe-mode)
    (define-key map (kbd "SPC") 'emms-metaplaylist-set-active)
    (define-key map (kbd "c") 'emms-metaplaylist-new-buffer)
    map)
  "Keymap for `emms-metaplaylist-mode'.")

;;; --------------------------------------------------------
;;; Metaplaylist
;;; --------------------------------------------------------

(defun emms-metaplaylist-mode-goto-current ()
  "Switch to the buffer at point."
  (interactive)
  (switch-to-buffer
   (buffer-substring (point-at-bol)
		     (point-at-eol))))

;; Since there will never be a significantly large amount of playlist
;; buffers co-existing at once, we allow ourselves not to keep
;; state. We regenerate the playlists buffer anew on demand.
(defun emms-metaplaylist-mode-create ()
  "Create or recreate the meta-playlist buffer."
  (let ((name emms-metaplaylist-mode-buffer-name)
	(playlists (emms-playlist-buffer-list)))
    (if playlists
	(progn
	  (condition-case nil
	      (kill-buffer name)
	    (error nil))
	  (get-buffer-create name)
	  (with-current-buffer name
	    (emms-metaplaylist-mode)
	    (save-excursion
	      (mapc (lambda (buf)
		      (let ((inhibit-read-only t))
			(insert (buffer-name buf))
			(add-text-properties
			 (point-at-bol) (point-at-eol)
			 (list 'face
			       (if (eq buf emms-playlist-buffer)
				   'emms-metaplaylist-mode-current-face
				 'emms-metaplaylist-mode-face)))
			(newline)))
		    playlists))
	    (current-buffer)))	       ; return the buffer as lisp obj
      (error "No Emms playlist buffers"))))

;;; --------------------------------------------------------
;;; Playlist Management : creation, deletion (?)
;;; --------------------------------------------------------

(defun emms-metaplaylist-new-buffer (buffer-name)
  "Creates a new buffer called buffer-name, which will be ready
to host EMMS tracks."
  (interactive "sBuffer Name: ")
  (if(get-buffer buffer-name)
      (error "Buffer must not exist.")
    (let ((buf (get-buffer-create buffer-name)))
      (with-current-buffer buf
        (setq emms-playlist-buffer-p t)))
    (message "Buffer created")))

(defun emms-metaplaylist-set-active ()
  (interactive)
  (emms-playlist-set-playlist-buffer 
   (get-buffer (buffer-substring (point-at-bol) (point-at-eol))))
  (let ((ici (point)))
    (emms-metaplaylist-mode-go)
    (goto-char ici)))

;;; --------------------------------------------------------
;;; Mode entry
;;; --------------------------------------------------------

(defun emms-metaplaylist-mode-go ()
  "Single entry point to the metaplaylist interface."
  (interactive)
  (emms-metaplaylist-mode-create)
  (switch-to-buffer emms-metaplaylist-mode-buffer-name))

(defun emms-metaplaylist-mode ()
  "A major mode for Emms playlists."
;;  (interactive)
  (kill-all-local-variables)

  (use-local-map emms-metaplaylist-mode-map)
  (setq major-mode 'emms-metaplaylist-mode
	mode-name "Emms-MetaPlaylist")

  (setq buffer-read-only t)

  (run-hooks 'emms-metaplaylist-mode-hooks))

(provide 'emms-metaplaylist-mode)

;;; emms-metaplaylist-mode.el ends here
