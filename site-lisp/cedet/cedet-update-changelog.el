;;; cedet-update-changelog --- Utility for updating changelogs in CEDET.

;;; Copyright (C) 2005 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: cedet-update-changelog.el,v 1.3 2005/09/30 20:06:39 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; I rebuild the ChangeLog from CVS log files using rcs2log for each release.
;; This automates the process, and fixes up the bad email addresses that
;; are created by rcs2log.


;;; History:
;; 

(require 'cedet)
;;; Code:

(defvar cuc-my-machine-name (let ((sn (system-name)))
			      ;; Only the first part of the name...
			      (if (string-match "\\." sn)
				  (concat
				   (substring sn 0 (match-beginning 0))
				   "\\("
				   (substring sn (match-beginning 0))
				   "\\)?")
				sn))
  "The name of the machine running this code as output by rcs2diff.")

(defvar cuc-dirs
  (let ((pack cedet-packages)
	(dirs nil))
    (while pack
      (setq dirs
	    (cons (file-name-directory (locate-library
					(symbol-name (car (car pack)))))
		  dirs)
	    pack
	    (cdr pack)))
    dirs)
  "List of directories we need to change the ChangeLog in.")

(defun cuc-update-changelog (dir)
  "Update the changelog in DIR."
  (find-file (concat dir "ChangeLog"))
  (goto-char (point-min))
  (sit-for 0)
  (message "Calling rcs2log on %s..."
	   (file-name-nondirectory (directory-file-name dir)))
  (call-process "rcs2log" nil (current-buffer) nil
		"-c" "ChangeLog")
  (cuc-fixup-ChangeLog-names)
  (save-buffer))

(defun cuc-update-all-changelogs ()
  "Update all ChangeLogs for CEDET."
  (interactive)
  (let ((d cuc-dirs))
    (if (not (y-or-n-p
	      (format "Update ChangeLogs in %s? " (car d))))
	(error "Ok"))
    (while d
      (cuc-update-changelog (car d))
      (setq d (cdr d)))))

(defun cuc-make-search-name (name)
  "Make a search name based on NAME."
  (concat name " +<" name "@" cuc-my-machine-name ">"))

(defun cuc-fixup-ChangeLog-names ()
  "Update the names in the current ChangeLog.
Because the names come out of rcs2log as if on my machine, they
need to be transformed into the actual values."
  (interactive)
  (save-excursion
    ;; Eric's Name
    (goto-char (point-min))
    (while (re-search-forward (concat "<zappo@"
				      cuc-my-machine-name
				      ">")
			      nil t)
      (replace-match "<zappo@gnu.org>" t t))
    ;; David's Name.  (Why 2?)
    (goto-char (point-min))
    (while (or (re-search-forward
		(concat "\\("
			(cuc-make-search-name "ponced")
			"\\|"
			(cuc-make-search-name "david_ponce")
			"\\)")
		nil t))
      (replace-match "David Ponce  <david@dponce.com>" t t))
    ;; Richard's Name
    (goto-char (point-min))
    (while (re-search-forward (cuc-make-search-name "emacsman")
			      nil t)
      (replace-match "Richard Y. Kim <ryk@ap.com>" t t))
    ;; Klaus's Name
    (goto-char (point-min))
    (while (re-search-forward (cuc-make-search-name "berndl")
			      nil t)
      (replace-match "Klaus Berndl <klaus.berndl@sdm.de>" t t))
    ;; Suraj's Name
    (goto-char (point-min))
    (while (re-search-forward (cuc-make-search-name "surajacharya")
			      nil t)
      (replace-match "Suraj Acharya <sacharya@gmail.com>" t t))
    ))

(provide 'cedet-update-changelog)

;;; cedet-update-changelog.el ends here
