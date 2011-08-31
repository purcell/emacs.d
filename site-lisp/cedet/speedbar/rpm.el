;;; rpm.el --- Manage Red Hat packages in emacs

;;; Copyright (C) 1998, 1999, 2000, 2001, 2002 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 1.0
;; Keywords: speedbar, rpm
;; X-RCS: $Id: rpm.el,v 1.11 2005/09/30 20:25:46 zappo Exp $

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Manage Red Hat system packages in emacs.  Uses speedbar to display
;; the package higherarchy, and defines an `rpm-mode' which is useful
;; for managing and viewing a specific package.
;;
;;  This tool depends on speedbar version 0.7 or higher.
;;

;;; History:
;;
;; 1.0  Initial revision

(require 'speedbar)
;;; Code:

(defvar rpm-system nil
  "This represents the current system.")

(defvar rpm-speedbar-key-map nil
  "Keymap used when working with RPMs in speedbar.")

(if rpm-speedbar-key-map
    nil
  (setq rpm-speedbar-key-map (speedbar-make-specialized-keymap))

  ;; General viewing pleasure...
  (define-key rpm-speedbar-key-map "\C-m" 'speedbar-edit-line)
  (define-key rpm-speedbar-key-map "+" 'speedbar-expand-line)
  (define-key rpm-speedbar-key-map "=" 'speedbar-expand-line)
  (define-key rpm-speedbar-key-map "-" 'speedbar-contract-line)

  )

(defvar rpm-speedbar-menu
  ()
  "Menu part in easymenu format that is used in speedbar while in rpm mode.")

(defvar rpm-font-lock-keywords
  '(
    ;; file names
    ("^\\(/[^ \n]+\\)$" 1 font-lock-reference-face)
    ;; Tags
    ("\\(Name\\|Version\\|Release\\|Install date\\|Group\\|Size\\|Packager\\|\
Summary\\|Description\\|Distribution\\|Vendor\\|Build Date\\|Build Host\\|\
Source RPM\\|URL\\) *:" 0 font-lock-variable-name-face)
    ("^Name +: \\([^ \t]+\\)" 1 font-lock-function-name-face)
    ;; Everything else is from the description.
    ;; This is a clever font lock hack since it wont double color items
    ("^\\([^\n]+\\)$" 1 font-lock-comment-face)
    )
  "Keywords used to highlight an RPM info buffer.")

(defun rpm-info (package)
  "View RPM PACKAGE information in the current buffer."
  (interactive "sPackage: ")
  (toggle-read-only -1)
  (call-process "rpm" nil t nil "-qil" package)
  (goto-char (point-min))
  (rpm-mode)
  (set-buffer-modified-p nil)
  (toggle-read-only 1))

(defun rpm-mode ()
  "Major mode for viewing package information."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rpm-mode
	mode-name "RPM")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((rpm-font-lock-keywords)
			     t t
			     ((?_ . "w") (?/ . "w"))))
  (run-hooks 'rpm-info-hook))

;;;###autoload
(defun rpm ()
  "Red Hat Package Management in Emacs."
  (interactive)
  ;; Make sure that speedbar is active
  (speedbar-frame-mode 1)
  ;; Make sure our special speedbar major mode is loaded
  (speedbar-add-expansion-list '("rpm" rpm-speedbar-menu rpm-speedbar-key-map
				 rpm-speedbar))
  ;; Now, throw us into RPM mode on speedbar.
  (speedbar-change-initial-expansion-list "rpm")
  )

(defun rpm-speedbar (directory zero)
  "Create buttons in speedbar that represents the current rpm system.
Takes DIRECTORY and ZERO, which are both ignored."
  (rpm-fetch-system)
  (let ((speedbar-tag-hierarchy-method '(speedbar-sort-tag-hierarchy)))
    (speedbar-insert-generic-list -1 rpm-system 'rpm-tag-expand 'rpm-tag-find)))

(defun rpm-tag-expand (text token indent)
  "Expand a tag sublist.  Imenu will return sub-lists of specialized tag types.
Etags does not support this feature.  TEXT will be the button
string.  TOKEN will be the list, and INDENT is the current indentation
level."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (let ((speedbar-tag-hierarchy-method '(speedbar-sort-tag-hierarchy)))
	       (speedbar-insert-generic-list indent
					     token
					     'rpm-tag-expand
					     'rpm-tag-find)))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do.")))
  (speedbar-center-buffer-smartly))

(defun rpm-tag-find (text token indent)
  "When clicking on a found tag, open that RPM file up.
TEXT is the name of the package.  TOKEN and INDENT are ignored."
  (let* ((buff (get-buffer-create text))
	 (bwin (get-buffer-window buff 0)))
    (if bwin
	(progn
	  (select-window bwin)
	  (raise-frame (window-frame bwin)))
      (if dframe-power-click
	  (let ((pop-up-frames t)) (select-window (display-buffer buff)))
	(dframe-select-attached-frame speedbar-frame)
	(switch-to-buffer buff)))
    (erase-buffer)
    (rpm-info text)))

(defun rpm-fetch-system ()
  "Fetch the system by executing rpm."
  (if rpm-system
      nil
    (save-excursion
      (set-buffer (get-buffer-create "*rpm output*"))
      ;; Get the database information here
      (if (= (point-min) (point-max))
	  (progn
	    (speedbar-message "Running rpm -qa")
	    (call-process "rpm" nil t nil "-qa" "--queryformat"
			  "%{name}-%{version}-%{release} %{group}\n")))
      ;; Convert it into a giant list
      (speedbar-message "Parsing output ... ")
      (goto-char (point-min))
      (while (re-search-forward "^\\([^ ]+\\) \\([^\n]+\\)$" nil t)
	(let* ((n (match-string 1))
	       (p (match-string 2))
	       (sl nil))
	  ;; Start the directory listing
	  (string-match "^\\([^/]+\\)\\(/\\|$\\)" p)
	  (setq sl (assoc (match-string 1 p) rpm-system))
	  (if (not sl) (setq rpm-system
			     (cons (setq sl (list (match-string 1 p)))
				   rpm-system)))
	  (setq p (substring p (match-end 0)))
	  ;; Loop to the end of the directory listing
	  (while (string-match "^\\([^/]+\\)\\(/\\|$\\)" p)
	    (let ((ssl (assoc (match-string 1 p) sl)))
	      (if (not ssl)
		  (setcdr sl (cons (setq ssl (list (match-string 1 p)))
				   (cdr sl))))
	      (setq sl ssl))
	    (setq p (substring p (match-end 0))))
	  ;; We are at the end.  Append our new element to the end
	  (while (cdr sl) (setq sl (cdr sl)))
	  (setcdr sl (cons (cons n 1) nil)))))))

(provide 'rpm)
;;; rpm.el ends here
