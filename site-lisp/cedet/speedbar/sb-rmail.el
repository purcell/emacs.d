;;; sb-rmail --- Speedbar support for rmail

;; Copyright (C) 1997, 1998, 1999, 2001, 2002, 2003 Free Software Foundation
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: file, tags, tools
;; X-RCS: $Id: sb-rmail.el,v 1.12 2003/02/21 18:04:30 zappo Exp $
;;
;; This file is part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;

;;; Commentary:
;;
;;   Speedbar provides a frame in which files, and locations in
;; files are displayed.  These functions provide rmail specific support,
;; showing links and addresses in the side-bar.
;;
;;   To enable in emacs 20.2 or earlier, add this to your .emacs file.
;;   (autoload 'rmail-speedbar-buttons "sb-rmail"
;;             "Rmail specific speedbar button generator.")
;;
;;   This file requires speedbar.

;;; Change log:
;; 0.1   - first revision copied from speedbspec.el V 0.1.1
;; 0.1.1 - removed require speedbspec.
;; 0.1.2 - Changed to handle new keymap feature.

;;; Code:
(defvar rmail-speedbar-match-folder-regexp "^[A-Z0-9]+\\(\\.[A-Z0-9]+\\)?$"
  "*This regex is used to match folder names to be displayed in speedbar.
Enabling this will permit speedbar to display your folders for easy
browsing, and moving of messages.")

(defvar rmail-speedbar-last-user nil
  "The last user to be displayed in the speedbar.")

(defvar rmail-speedbar-key-map nil
  "Keymap used when in rmail display mode.")

(defun rmail-install-speedbar-variables ()
  "Install those variables used by speedbar to enhance rmail."
  (if rmail-speedbar-key-map
      nil
    (setq rmail-speedbar-key-map (speedbar-make-specialized-keymap))

    (define-key rmail-speedbar-key-map "e" 'speedbar-edit-line)
    (define-key rmail-speedbar-key-map "r" 'speedbar-edit-line)
    (define-key rmail-speedbar-key-map "\C-m" 'speedbar-edit-line)
    (define-key rmail-speedbar-key-map "M"
      'rmail-speedbar-move-message-to-folder-on-line)))

(defvar rmail-speedbar-menu-items
  '(["Read Folder" speedbar-edit-line t]
    ["Move message to folder" rmail-speedbar-move-message-to-folder-on-line
     (save-excursion (beginning-of-line)
		     (looking-at "<M> "))])
  "Additional menu-items to add to speedbar frame.")

;; Make sure our special speedbar major mode is loaded
(if (featurep 'speedbar)
    (rmail-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'rmail-install-speedbar-variables))

;;;###autoload
(defun rmail-speedbar-buttons (buffer)
  "Create buttons for BUFFER containing rmail messages.
Click on the address under Reply to: to reply to this person.
Under Folders: Click a name to read it, or on the <M> to move the
current message into that RMAIL folder."
  (let ((from nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (if (not (re-search-forward "^Reply-To: " nil t))
	  (if (not (re-search-forward "^From:? " nil t))
	      (setq from t)))
      (if from
	  nil
	(setq from (buffer-substring (point) (save-excursion
					       (end-of-line)
					       (point))))))
    (goto-char (point-min))
    (if (and (looking-at "\\(//\\)?Reply to:")
	     (equal from rmail-speedbar-last-user))
	nil
      (setq rmail-speedbar-last-user from)
      (erase-buffer)
      (speedbar-insert-separator "Reply To")
      (if (stringp from)
	  (speedbar-insert-button from 'speedbar-directory-face 'highlight
				  'rmail-speedbar-button 'rmail-reply))
      (speedbar-insert-separator "Folders")
      (let* ((case-fold-search nil)
	     (df (directory-files (save-excursion (set-buffer buffer)
						  default-directory)
				  nil rmail-speedbar-match-folder-regexp)))
	(while df
	  (speedbar-insert-button "<M>" 'speedbar-button-face 'highlight
				  'rmail-speedbar-move-message (car df))
	  (speedbar-insert-button (car df) 'speedbar-file-face 'highlight
				  'rmail-speedbar-find-file nil t)
	  (setq df (cdr df)))))))

(defun rmail-speedbar-button (text token indent)
  "Execute an rmail command specified by TEXT.
The command used is TOKEN.  INDENT is not used."
  (speedbar-with-attached-buffer
   (funcall token t)))

(defun rmail-speedbar-find-file (text token indent)
  "Load in the rmail file TEXT.
TOKEN and INDENT are not used."
  (speedbar-with-attached-buffer
   (speedbar-message "Loading in RMAIL file %s..." text)
   (find-file text)))

(defun rmail-speedbar-move-message-to-folder-on-line ()
  "If the current line is a folder, move current message to it."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "<M> " (save-excursion (end-of-line) (point)) t)
	(progn
	  (forward-char -2)
	  (speedbar-do-function-pointer)))))

(defun rmail-speedbar-move-message (text token indent)
  "From button TEXT, copy current message to the rmail file specified by TOKEN.
TEXT and INDENT are not used."
  (speedbar-with-attached-buffer
   (message "Moving message to %s" token)
   (rmail-output-to-rmail-file token)))

(provide 'sb-rmail)
;;; sb-rmail.el ends here
