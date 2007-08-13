;;; srecode-args.el --- Provide some simple template arguments

;; Copyright (C) 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-args.el,v 1.2 2007/02/24 03:10:00 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Srecode teplates can accept arguments.  These areguments represent
;; sets of dictionary words that need to be derived.  This file contains
;; a set of simple arguments for srecode templates.

(require 'simple)
(require 'srecode-insert)

;;; Code:

;;; :indent ARGUMENT HANDLING
;;
;; When a :indent argument is required, the default is to indent
;; for the current major mode.
;;;###autoload
(defun srecode-semantic-handle-:indent (dict)
  "Add macros into the dictionary DICT based on the current :user."
  (srecode-dictionary-set-value dict "INDENT" t)
  )

;;; :user ARGUMENT HANDLING
;;
;; When a :user argument is required, fill the dictionary with
;; information about the current Emacs user.
;;;###autoload
(defun srecode-semantic-handle-:user (dict)
  "Add macros into the dictionary DICT based on the current :user."
  (srecode-dictionary-set-value
   dict "AUTHOR" (user-full-name))
  (srecode-dictionary-set-value
   dict "LOGIN" (user-login-name))
  (srecode-dictionary-set-value
   dict "EMAIL" user-mail-address)
  (srecode-dictionary-set-value
   dict "EMACSINITFILE" user-init-file)
  (srecode-dictionary-set-value
   dict "UID" (user-uid))
  )

;;; :time ARGUMENT HANDLING
;;
;; When a :time argument is required, fill the dictionary with
;; information about the current Emacs time.
;;;###autoload
(defun srecode-semantic-handle-:time (dict)
  "Add macros into the dictionary DICT based on the current :time."
  (srecode-dictionary-set-value
   dict "YEAR" (format-time-string "%Y" (current-time)))
  (srecode-dictionary-set-value
   dict "MONTH" (format-time-string "%B" (current-time)))
  (srecode-dictionary-set-value
   dict "DAY" (format-time-string "%m" (current-time)))
  (srecode-dictionary-set-value
   dict "DATE" (format-time-string "%D" (current-time)))
  (srecode-dictionary-set-value
   dict "TIME" (format-time-string "%X" (current-time)))
  )

;;; :file ARGUMENT HANDLING
;;
;; When a :file argument is required, fill the dictionary with
;; information about the file Emacs is editing at the time of
;; insertion.
;;;###autoload
(defun srecode-semantic-handle-:file (dict)
  "Add macros into the dictionary DICT based on the current :file."
  (let* ((bfn (buffer-file-name))
	 (file (file-name-nondirectory bfn))
	 (dir (file-name-directory bfn)))
    (srecode-dictionary-set-value dict "FILENAME" file)
    (srecode-dictionary-set-value dict "FILE" (file-name-sans-extension file))
    (srecode-dictionary-set-value dict "EXTENSION" (file-name-extension file))
    (srecode-dictionary-set-value dict "DIRECTORY" dir)
    (srecode-dictionary-set-value dict "MODE" (symbol-name major-mode))
    (srecode-dictionary-set-value
     dict "SHORTMODE"
     (let* ((mode-name  (symbol-name major-mode))
	    (match (string-match "-mode" mode-name)))
       (if match
	   (substring mode-name 0 match)
	 mode-name)))
    (if (or (file-exists-p "CVS")
	    (file-exists-p "RCS"))
	(srecode-dictionary-show-section dict "RCS")
      )))

;;; :system ARGUMENT HANDLING
;;
;; When a :system argument is required, fill the dictionary with
;; information about the computer Emacs is running on.
;;;###autoload
(defun srecode-semantic-handle-:system (dict)
  "Add macros into the dictionary DICT based on the current :system."
    (srecode-dictionary-set-value dict "SYSTEMCONF" 
				  system-configuration)
    (srecode-dictionary-set-value dict "SYSTEMTYPE" 
				  system-type)
    (srecode-dictionary-set-value dict "SYSTEMNAME" 
				  (system-name))
    (srecode-dictionary-set-value dict "MAILHOST" 
				  (or mail-host-address
				      (system-name)))
  )

;;; :kill ARGUMENT HANDLING
;;
;; When a :kill argument is required, fill the dictionary with
;; information about the current kill ring.
;;;###autoload
(defun srecode-semantic-handle-:kill (dict)
  "Add macros into the dictionary DICT based on the kill ring."
  (srecode-dictionary-set-value dict "KILL" (car kill-ring))
  (srecode-dictionary-set-value dict "KILL2" (nth 1 kill-ring))
  (srecode-dictionary-set-value dict "KILL3" (nth 2 kill-ring))
  (srecode-dictionary-set-value dict "KILL4" (nth 3 kill-ring))
  )

(provide 'srecode-args)

;;; srecode-args.el ends here

