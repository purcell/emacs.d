;;; emms-cache.el --- persistence for emms-track

;; Copyright (C) 2006, 2007, 2008, 2009  Free Software Foundation, Inc.

;; Author: Damien Elmes <emacs@repose.cx>
;; Keywords: emms, mp3, mpeg, multimedia

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The cache is a mapping of a full path name to information, and so
;; it is invalidated when you rename or move files about.  It also
;; does not differentiate between file or uri tracks.

;; Because cache lookups are much faster than disk access, this works
;; much better with a later-do-interval of something like 0.001.  Also
;; consider using synchronous mode, as it's quite fast now.

;; This code is activated by (emms-standard) and above.

;; To activate it by hand, use:

;; (emms-cache 1)

;;; Code:

(require 'emms)
(require 'emms-info)

(when (fboundp 'define-hash-table-test)
  (define-hash-table-test 'string-hash 'string= 'sxhash))

(defvar emms-cache-db (make-hash-table
                       :test (if (fboundp 'define-hash-table-test)
                                 'string-hash
                               'equal))
  "A mapping of paths to file info.
This is used to cache over emacs sessions.")

(defvar emms-cache-dirty nil
  "True if the cache has been updated since init.")

(defcustom emms-cache-file (concat (file-name-as-directory emms-directory) "cache")
  "A file used to store cached file information over sessions."
  :group 'emms
  :type 'file)

(defcustom emms-cache-file-coding-system 'utf-8
  "Coding system used for saving `emms-cache-file'."
  :group 'emms
  :type 'coding-system)

(defun emms-cache (arg)
  "Turn on Emms caching if ARG is positive, off otherwise."
  (interactive "p")
  (if (and arg (> arg 0))
      (progn
	(unless emms-cache-dirty
	  (emms-cache-restore))
        (unless noninteractive
          (add-hook 'kill-emacs-hook 'emms-cache-save))
        (setq emms-cache-get-function 'emms-cache-get)
        (setq emms-cache-set-function 'emms-cache-set)
        (setq emms-cache-modified-function 'emms-cache-dirty))
    (remove-hook 'kill-emacs-hook 'emms-cache-save)
    (setq emms-cache-get-function nil)
    (setq emms-cache-set-function nil)
    (setq emms-cache-modified-function nil)))

;;;###autoload
(defun emms-cache-enable ()
  "Enable caching of Emms track data."
  (interactive)
  (emms-cache 1)
  (message "Emms cache enabled"))

;;;###autoload
(defun emms-cache-disable ()
  "Disable caching of Emms track data."
  (interactive)
  (emms-cache -1)
  (message "Emms cache disabled"))

;;;###autoload
(defun emms-cache-toggle ()
  "Toggle caching of Emms track data."
  (interactive)
  (if emms-cache-get-function
      (emms-cache-disable)
    (emms-cache-enable)))

(defsubst emms-cache-dirty (&rest ignored)
  "Mark the cache as dirty."
  (setq emms-cache-dirty t))

(defun emms-cache-get (type path)
  "Return a cache element for PATH, or nil."
  (gethash path emms-cache-db))

;; Note we ignore TYPE, as it's stored in TRACK
(defun emms-cache-set (type path track)
  "Set PATH to TRACK in the cache."
  (puthash path track emms-cache-db)
  (emms-cache-dirty))

(defun emms-cache-del (path)
  "Remove a track from the cache, with key PATH."
  (remhash path emms-cache-db)
  (emms-cache-dirty))

(defun emms-cache-save ()
  "Save the track cache to a file."
  (interactive)
  (when emms-cache-dirty
    (message "Saving emms track cache...")
    (set-buffer (get-buffer-create " emms-cache "))
    (erase-buffer)
    (insert
     (concat ";;; .emms-cache -*- mode: emacs-lisp; coding: "
             (symbol-name emms-cache-file-coding-system)
             "; -*-\n"))
    (maphash (lambda (k v)
               (insert (format
                        "(puthash %S '%S emms-cache-db)\n" k v)))
             emms-cache-db)
    (when (fboundp 'set-buffer-file-coding-system)
      (set-buffer-file-coding-system emms-cache-file-coding-system))
    (unless (file-directory-p (file-name-directory emms-cache-file))
      (make-directory (file-name-directory emms-cache-file)))
    (write-region (point-min) (point-max) emms-cache-file)
    (kill-buffer (current-buffer))
    (message "Saving emms track cache...done")
    (setq emms-cache-dirty nil)))

(defun emms-cache-restore ()
  "Restore the track cache from a file."
  (interactive)
  (load emms-cache-file t nil t)
  (setq emms-cache-dirty nil))

(defun emms-cache-sync ()
  "Sync the cache with the data on disc.
Remove non-existent files, and update data for files which have
been modified."
  (interactive)
  (message "Syncing emms track cache...")
  (let (removed)
    (maphash (lambda (path track)
               (when (eq (emms-track-get track 'type) 'file)
                 ;; if no longer here, remove
                 (if (not (file-exists-p path))
                     (progn
                       (remhash path emms-cache-db)
                       (setq removed t))
                   (let ((file-mtime (emms-info-track-file-mtime track))
                         (info-mtime (emms-track-get track 'info-mtime)))
                     (when (or (not info-mtime)
                               (emms-time-less-p
                                info-mtime file-mtime))
                       (run-hook-with-args 'emms-info-functions track))))))
             emms-cache-db)
    (when removed
      (setq emms-cache-dirty t)))
  (message "Syncing emms track cache...done"))

(provide 'emms-cache)
;;; emms-cache.el ends here
