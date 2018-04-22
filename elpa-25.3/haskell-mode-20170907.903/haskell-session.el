;;; haskell-session.el --- Haskell sessions -*- lexical-binding: t -*-

;; Copyright (C) 2011-2012  Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

;; This file is not part of GNU Emacs.

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Todo:

;;; Code:

(require 'cl-lib)
(require 'haskell-cabal)
(require 'haskell-customize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals

;; Used internally
(defvar-local haskell-session nil)

(defvar haskell-sessions (list)
  "All Haskell sessions in the Emacs session.")

(defun haskell-session-tags-filename (session)
  "Get the filename for the TAGS file."
  (concat (haskell-session-cabal-dir session) "/TAGS"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding/clearing the session

;;;###autoload
(defun haskell-session-maybe ()
  "Maybe get the Haskell session, return nil if there isn't one."
  (if (default-boundp 'haskell-session)
      haskell-session
    (setq haskell-session nil)))

(defun haskell-session-from-buffer ()
  "Get the session based on the buffer."
  (when (and (buffer-file-name)
             (consp haskell-sessions))
    (cl-reduce (lambda (acc a)
                 (let ((dir (haskell-session-get a 'cabal-dir)))
                   (if dir
                       (if (string-prefix-p dir
                                            (file-name-directory (buffer-file-name)))
                           (if acc
                               (if (and
                                    (> (length (haskell-session-get a 'cabal-dir))
                                       (length (haskell-session-get acc 'cabal-dir))))
                                   a
                                 acc)
                             a)
                         acc)
                     acc)))
               haskell-sessions
               :initial-value nil)))

(defun haskell-session-default-name ()
  "Generate a default project name for the new project prompt."
  (let ((file (haskell-cabal-find-file)))
    (or (when file
          (downcase (file-name-sans-extension
                     (file-name-nondirectory file))))
        "haskell")))

(defun haskell-session-assign (session)
  "Assing current buffer to SESSION.

This could be helpful for temporary or auxiliary buffers such as
presentation mode buffers (e.g. in case when session is killed
with all relevant buffers)."
  (setq-local haskell-session session))

(defun haskell-session-choose ()
  "Find a session by choosing from a list of the current sessions."
  (when haskell-sessions
    (let* ((session-name (funcall haskell-completing-read-function
                                  "Choose Haskell session: "
                                  (cl-remove-if (lambda (name)
                                                  (and haskell-session
                                                       (string= (haskell-session-name haskell-session)
                                                                name)))
                                                (mapcar 'haskell-session-name haskell-sessions))))
           (session (cl-find-if (lambda (session)
                                  (string= (haskell-session-name session)
                                           session-name))
                                haskell-sessions)))
      session)))

(defun haskell-session-clear ()
  "Clear the buffer of any Haskell session choice."
  (setq-local haskell-session nil))

(defun haskell-session-lookup (name)
  "Get the session by name."
  (cl-remove-if-not (lambda (s)
                      (string= name (haskell-session-name s)))
                    haskell-sessions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session modules

(defun haskell-session-strip-dir (session file)
  "Strip the load dir from the file path."
  (let ((cur-dir (haskell-session-current-dir session)))
    (if (> (length file) (length cur-dir))
        (if (string= (substring file 0 (length cur-dir))
                     cur-dir)
            (replace-regexp-in-string
             "^[/\\]" ""
             (substring file
                        (length cur-dir)))
          file)
      file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessing the session

(defun haskell-session-current-dir (s)
  "Get the session current directory."
  (let ((dir (haskell-session-get s 'current-dir)))
    (or dir
        (error "No current directory."))))

(defun haskell-session-name (s)
  "Get the session name."
  (haskell-session-get s 'name))

(defun haskell-session-target (s)
  "Get the session build target.
If `haskell-process-load-or-reload-prompt' is nil, accept `default'."
  (let* ((maybe-target (haskell-session-get s 'target))
         (target (if maybe-target maybe-target
                   (let ((new-target
                          (if haskell-process-load-or-reload-prompt
                              (read-string "build target (empty for default):")
                            "")))
                     (haskell-session-set-target s new-target)))))
    (if (not (string= target "")) target nil)))

(defun haskell-session-set-target (s target)
  "Set the session build target."
  (haskell-session-set s 'target target))

(defun haskell-session-set-interactive-buffer (s v)
  "Set the session interactive buffer."
  (haskell-session-set s 'interactive-buffer v))

(defun haskell-session-set-process (s v)
  "Set the session process."
  (haskell-session-set s 'process v))

;;;###autoload
(defun haskell-session-process (s)
  "Get the session process."
  (haskell-session-get s 'process))

(defun haskell-session-set-cabal-dir (s v)
  "Set the session cabal-dir."
  (let ((true-path (file-truename v)))
    (haskell-session-set s 'cabal-dir true-path)
    (haskell-session-set-cabal-checksum s true-path)))

(defun haskell-session-set-current-dir (s v)
  "Set the session current directory."
  (let ((true-path (file-truename v)))
    (haskell-session-set s 'current-dir true-path)))

(defun haskell-session-set-cabal-checksum (s cabal-dir)
  "Set the session checksum of .cabal files"
  (haskell-session-set s 'cabal-checksum
                       (haskell-cabal-compute-checksum cabal-dir)))

(defun haskell-session-cabal-dir (s)
  "Get the session cabal-dir."
  (or (haskell-session-get s 'cabal-dir)
      (let ((set-dir (haskell-cabal-get-dir (not haskell-process-load-or-reload-prompt))))
        (if set-dir
            (progn (haskell-session-set-cabal-dir s set-dir)
                   set-dir)
            (haskell-session-cabal-dir s)))))

(defun haskell-session-modify (session key update)
  "Update the value at KEY in SESSION with UPDATE."
  (haskell-session-set
   session
   key
   (funcall update
            (haskell-session-get session key))))

(defun haskell-session-get (session key)
  "Get the SESSION's KEY value.
Returns nil if KEY not set."
  (cdr (assq key session)))

(defun haskell-session-set (session key value)
  "Set the SESSION's KEY to VALUE.
Returns newly set VALUE."
  (let ((cell (assq key session)))
    (if cell
        (setcdr cell value) ; modify cell in-place
      (setcdr session (cons (cons key value) (cdr session))) ; new cell
      value)))

(provide 'haskell-session)

;;; haskell-session.el ends here
