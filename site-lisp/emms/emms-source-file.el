;;; emms-source-file.el --- EMMS sources from the filesystem.

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008,
;;   2009 Free Software Foundation, Inc.

;; Author: Jorgen Schäfer <forcer@forcix.cx>
;; Keywords: emms, mp3, mpeg, multimedia

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
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains a track source for EMMS that is based on the
;; file system. You can retrieve single files or whole directories.
;; Also, this file offers the commands to play from these sources.

;;; Code:

;; Version control
(defvar emms-source-file-version "0.2 $Revision: 1.30 $"
  "emms-source-file.el version string")
;; $Id: emms-source-file.el,v 1.30 2005/08/11 06:16:15 yonirabkin Exp $

;;; User Customization

(require 'emms)
(eval-when-compile
  (condition-case nil
      (require 'locate)
    (error nil)))
(require 'dired)

(defgroup emms-source-file nil
  "*Sources for EMMS that use the file system."
  :prefix "emms-source-file-"
  :group 'emms-source)

(defcustom emms-source-file-default-directory nil
  "*The default directory to look for media files."
  :type 'string
  :group 'emms-source-file)

(defcustom emms-source-file-directory-tree-function
  'emms-source-file-directory-tree-internal
  "*A function to call that searches in a given directory all files
that match a given regex. DIR and REGEX are the only arguments passed
to this function.
You have two build-in options:
`emms-source-file-directory-tree-internal' will work always, but might
be slow.
`emms-source-file-directory-tree-find' will work only if you have GNU
find, but it's faster."
  :type 'function
  :options '(emms-source-file-directory-tree-internal
             emms-source-file-directory-tree-find)
  :group 'emms-source-file)

(defcustom emms-source-file-exclude-regexp
  (concat "\\`\\(#.*#\\|.*,v\\|.*~\\|\\.\\.?\\|\\.#.*\\|,.*\\)\\'\\|"
          "/\\(CVS\\|RCS\\|\\.arch-ids\\|{arch}\\|,.*\\|\\.svn\\|"
          "_darcs\\)\\(/\\|\\'\\)")
  "A regexp matching files to be ignored when adding directories.

You should set case-fold-search to nil before using this regexp
in code."
  :type 'regexp
  :group 'emms-source-file)

(defcustom emms-source-file-gnu-find "find"
  "*The program name for GNU find."
  :type 'string
  :group 'emms-source-file)

;; The `read-directory-name' function is not available in Emacs 21.
(defalias 'emms-read-directory-name
  (if (fboundp 'read-directory-name)
      #'read-directory-name
    #'read-file-name))

;;; Sources

;;;###autoload (autoload 'emms-play-file "emms-source-file" nil t)
;;;###autoload (autoload 'emms-add-file "emms-source-file" nil t)
(define-emms-source file (file)
  "An EMMS source for a single file - either FILE, or queried from the
user."
  (interactive (list (read-file-name "Play file: "
                                     emms-source-file-default-directory
                                     emms-source-file-default-directory
                                     t)))
  (if (file-directory-p file)
      (emms-source-directory file)
    (emms-playlist-insert-track
     (emms-track 'file (expand-file-name file)))))

;;;###autoload (autoload 'emms-play-directory "emms-source-file" nil t)
;;;###autoload (autoload 'emms-add-directory "emms-source-file" nil t)
(define-emms-source directory (dir)
  "An EMMS source for a whole directory tree - either DIR, or queried
from the user."
  (interactive (list
                (emms-read-directory-name "Play directory: "
                                          emms-source-file-default-directory
                                          emms-source-file-default-directory
                                          t)))
  (mapc (lambda (file)
          (unless (or (let ((case-fold-search nil))
                        (string-match emms-source-file-exclude-regexp file))
                      (file-directory-p file))
            (emms-playlist-insert-track
             (emms-track 'file (expand-file-name file)))))
        (directory-files dir t (emms-source-file-regex))))

;;;###autoload (autoload 'emms-play-directory-tree "emms-source-file" nil t)
;;;###autoload (autoload 'emms-add-directory-tree "emms-source-file" nil t)
(define-emms-source directory-tree (dir)
  "An EMMS source for multiple directory trees - either DIR, or the
value of `emms-source-file-default-directory'."
  (interactive (list
                (emms-read-directory-name "Play directory tree: "
                                          emms-source-file-default-directory
                                          emms-source-file-default-directory
                                          t)))
  (let ((files (emms-source-file-directory-tree (expand-file-name dir)
						(emms-source-file-regex)))
	(case-fold-search nil))
    (emms-playlist-ensure-playlist-buffer)
    (mapc (lambda (file)
	    (unless (string-match emms-source-file-exclude-regexp file)
	      (funcall emms-playlist-insert-track-function 
		       (emms-track 'file file))))
	  files)))

;;;###autoload (autoload 'emms-play-find "emms-source-file" nil t)
;;;###autoload (autoload 'emms-add-find "emms-source-file" nil t)
(define-emms-source find (dir regex)
  "An EMMS source that will find files in DIR or
`emms-source-file-default-directory' that match REGEX."
  (interactive (list
                (emms-read-directory-name "Find in directory: "
                                          emms-source-file-default-directory
                                          emms-source-file-default-directory
                                          t)
                (read-from-minibuffer "Find files matching: ")))
  (mapc (lambda (file)
          (unless (let ((case-fold-search nil))
                    (string-match emms-source-file-exclude-regexp file))
            (emms-playlist-insert-track
             (emms-track 'file file))))
        (emms-source-file-directory-tree (expand-file-name dir) regex)))

;;;###autoload (autoload 'emms-play-dired "emms-source-file" nil t)
;;;###autoload (autoload 'emms-add-dired "emms-source-file" nil t)
(define-emms-source dired ()
  "Return all marked files of a dired buffer"
  (interactive)
  (mapc (lambda (file)
          (if (file-directory-p file)
              (emms-source-directory-tree file)
            (emms-source-file file)))
        (with-current-buffer emms-source-old-buffer
          (dired-get-marked-files))))


;;; Helper functions

;;;###autoload
(defun emms-source-file-directory-tree (dir regex)
  "Return a list of all files under DIR that match REGEX.
This function uses `emms-source-file-directory-tree-function'."
  (message "Building playlist...")
  (let ((pl (sort (funcall emms-source-file-directory-tree-function
                           dir
                           regex)
                  'string<)))
    (message "Building playlist...done")
    pl))

(defun emms-source-file-directory-tree-internal (dir regex)
  "Return a list of all files under DIR that match REGEX.
This function uses only emacs functions, so it might be a bit slow."
  (let ((files '())
        (dirs (list dir)))
    (while dirs
      (cond
       ((file-directory-p (car dirs))
        (if (or (string-match "/\\.\\.?$" (car dirs))
                (let ((symlink (file-symlink-p (car dirs))))
                  (and symlink
                       (string-equal dir (substring symlink 0 (string-width dir))))))
            (setq dirs (cdr dirs))
          (setq dirs
                (condition-case nil
                    (append (cdr dirs)
                            (directory-files (car dirs)
                                             t nil t))
                  (error
                   (cdr dirs))))))
       ((string-match regex (car dirs))
        (setq files (cons (car dirs) files)
              dirs (cdr dirs)))
       (t
        (setq dirs (cdr dirs)))))
    files))

(defun emms-source-file-directory-tree-find (dir regex)
  "Return a list of all files under DIR that match REGEX.
This function uses the external find utility. The name for GNU find
may be supplied using `emms-source-file-gnu-find'."
  (with-temp-buffer
    (call-process emms-source-file-gnu-find
                  nil t nil
                  (expand-file-name dir)
                  "-type" "f"
                  "-iregex" (concat ".*\\(" regex "\\).*"))
    (delete ""
            (split-string (buffer-substring (point-min)
                                            (point-max))
                          "\n"))))

(defmacro emms-with-excluded-directories (directory-list &rest body)
  "Run BODY while excluding DIRECTORY-LIST."
  `(let ((emms-source-file-exclude-regexp
	  (concat (or ,emms-source-file-exclude-regexp "")
		  "\\|\\("
		  (or (regexp-opt ,directory-list) "")
		  "\\)")))
     ,@body))

;;;###autoload
(defun emms-source-file-regex ()
  "Return a regexp that matches everything any player (that supports
files) can play."
  (mapconcat (lambda (player)
               (or (emms-player-get player 'regex)
                   ""))
             emms-player-list
             "\\|"))

;; emms-locate should be part of a once to be emms-dired, with maybe
;; file rename after tag functions and so on, but till then i park it
;; here... :)

;;;###autoload
(defun emms-locate (regexp)
  "Search for REGEXP and display the results in a locate buffer"
  (interactive "sRegexp to search for: ")
  (require 'locate)
  (save-window-excursion
    (set-buffer (get-buffer-create "*EMMS Find*"))
    (locate-mode)
    (erase-buffer)
    (mapc (lambda (elt) (insert (cdr (assoc 'name elt)) "\n"))
          (emms-source-find emms-source-file-default-directory regexp))
    (locate-do-setup regexp))
  (and (not (string-equal (buffer-name) "*EMMS Find*"))
       (switch-to-buffer-other-window "*EMMS Find*"))
  (run-hooks 'dired-mode-hook)
  (dired-next-line 2))

;; Strictly speaking, this does not belong in this file (URLs are not
;; real files), but it's close enough :-)

;;;###autoload (autoload 'emms-play-url "emms-source-file" nil t)
;;;###autoload (autoload 'emms-add-url "emms-source-file" nil t)
(define-emms-source url (url)
  "An EMMS source for an URL - for example, for streaming."
  (interactive "sPlay URL: ")
  (emms-playlist-insert-track (emms-track 'url url)))

;;;###autoload (autoload 'emms-play-streamlist "emms-source-file" nil t)
;;;###autoload (autoload 'emms-add-streamlist "emms-source-file" nil t)
(define-emms-source streamlist (streamlist)
  "An EMMS source for streaming playlists (usually URLs ending in .pls)."
  (interactive "sPlay streamlist URL: ")
  (emms-playlist-insert-track (emms-track 'streamlist streamlist)))

;;;###autoload (autoload 'emms-play-lastfm "emms-lastfm" nil t)
;;;###autoload (autoload 'emms-add-lastfm "emms-lastfm" nil t)
(define-emms-source lastfm (lastfm-url)
  "An EMMS source for Last.fm URLs, which begin with lastfm://."
  (interactive "sPlay Last.fm URL: ")
  (emms-playlist-insert-track (emms-track 'lastfm lastfm-url)))


(provide 'emms-source-file)
;;; emms-source-file.el ends here
