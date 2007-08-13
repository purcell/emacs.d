;;; cedet-update-version --- Update version numberes in all cedet packages.

;;; Copyright (C) 2005, 2006 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: cedet-update-version.el,v 1.4 2006/02/08 04:17:01 zappo Exp $

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
;; Maintaining all these version numbers is a real pain.
;; Try to make it a little bit easier on me.
;;
;; M-x cuv-update RET

;;; History:
;; 

;;; Code:
(if (not (featurep 'cedet))
    (error "You need to have cedet loaded to manage the update versions"))

(defun cuv-load-package-file (package)
  "Get the file name for PACKAGE."
  (setq package (symbol-name package))
  (find-library package))

(defun cuv-update-package-version (package)
  "Update the version number for the package PACKAGE."
  (cuv-load-package-file package)
  ;; Find the version tag.
  (let ((tag (car
	      (semantic-find-tags-by-name
	       (concat (symbol-name package) "-version")
	       (current-buffer)))))
    (goto-char (semantic-tag-start tag))
    (semantic-momentary-highlight-tag tag))
  ;; EDE will manage version numbers in files, and also
  ;; manage updating the Project files.
  (call-interactively 'ede-update-version)
  ;; Update Makefiles.
  (when (y-or-n-p "Update Makefiles from Projects? ")
    (ede-proj-regenerate)))

(defun cuv-update-all-cedet-packages ()
  "Update all package version numbers one by one."
  (cuv-load-package-file 'cedet)
  (let ((p cedet-packages)
	(cep (current-buffer))
	(tag (car
	      (semantic-find-tags-by-name "cedet-packages"
					  (current-buffer)))))
    (while p
      (cuv-update-package-version (car (car p)))
      (when (not (eq (car (car p)) 'cedet))
	;; Update the constant in cedet.el also!
	(let* ((eo (ede-toplevel))
	       (v (oref eo :version)))
	  (switch-to-buffer cep)
	  (goto-char (semantic-tag-start tag))
	  (re-search-forward (concat "(" (symbol-name (car (car p))) " +\"")
			     (semantic-tag-end tag))
	  (forward-char -1)
	  (when (y-or-n-p "Update this location also? ")
	    (kill-sexp 1)
	    (insert (format "%S" v))
	    (sit-for 2))
	  ))
      (setq p (cdr p)))))

(defun cuv-update ()
  "Interactively update all CEDET features before a release."
  (interactive)
  (cuv-update-all-cedet-packages)
  (message "Revision Number Updates Complete.")
  )
  
(provide 'cedet-update-version)

;;; cedet-update-version.el ends here
