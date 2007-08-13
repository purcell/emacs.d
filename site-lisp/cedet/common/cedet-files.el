;;; cedet-files.el --- Common routines dealing with file names.

;; Copyright (C) 2007 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cedet-files.el,v 1.1 2007/05/20 15:54:09 zappo Exp $

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
;; Various useful routines for dealing with file names in the tools
;; which are a part of CEDET.

;;; Code:
(defvar cedet-dir-sep-char (if (boundp 'directory-sep-char)
			       (symbol-value 'directory-sep-char)
			     ?/)
  "Character used for directory separation.
Obsoleted in some versions of Emacs.  Needed in others.")


(defun cedet-directory-name-to-file-name (referencedir)
  "Convert the REFERENCEDIR (a full path name) into a filename.
Converts directory seperation characters into ! characters."
  (let ((file referencedir)
	dir-sep-string)
    ;; Expand to full file name
    (or (file-name-absolute-p file)
	(setq file (expand-file-name file)))
    ;; If FILE is a directory, then force it to end in /.
    (when (file-directory-p file)
      (setq file (file-name-as-directory file)))
    ;; Handle Windows Special cases
    (when (memq system-type '(windows-nt ms-dos))
      ;; Replace any invalid file-name characters (for the
      ;; case of backing up remote files).
      (setq file (expand-file-name (convert-standard-filename file)))
      (setq dir-sep-string (char-to-string cedet-dir-sep-char))
      ;; Normalize DOSish file names: convert all slashes to
      ;; directory-sep-char, downcase the drive letter, if any,
      ;; and replace the leading "x:" with "/drive_x".
      (if (eq (aref file 1) ?:)
	  (setq file (concat dir-sep-string
			     "drive_"
			     (char-to-string (downcase (aref file 0)))
			     (if (eq (aref file 2) cedet-dir-sep-char)
				 ""
			       dir-sep-string)
			     (substring file 2)))))
    ;; Make the name unique by substituting directory
    ;; separators.  It may not really be worth bothering about
    ;; doubling `!'s in the original name...
    (setq file (subst-char-in-string
		cedet-dir-sep-char ?!
		(replace-regexp-in-string "!" "!!" file)))
    file))


(provide 'cedet-files)

;;; cedet-files.el ends here
