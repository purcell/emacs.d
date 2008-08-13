;;; sb-info --- Speedbar support for Info

;; Copyright (C) 1997, 1998, 2000, 2001, 2002, 2005 Free Software Foundation
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.3
;; Keywords: file, tags, tools
;; X-RCS: $Id: sb-info.el,v 1.20 2006/06/23 08:26:16 ponced Exp $
;;
;; This file is patch of GNU Emacs.
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
;; Please send bug reports, etc. to zappo@gnu.org
;;

;;; Commentary:
;;
;;   Speedbar provides a frame in which files, and locations in
;; files are displayed.  These functions provide Info specific support,
;; showing links and addresses in the side-bar.
;;
;;   To enable in Emacs 20.2 or earlier, add this to your .emacs file.
;;   (autoload 'Info-speedbar-buttons "sb-info"
;;             "Info specific speedbar button generator.")
;;
;;   This file requires speedbar and Info.

;;; Change log:
;; 0.1   - first revision copied from speedbspec.el V 0.1.1
;; 0.1.1 - No longer require speedbspec
;; 0.2   - Added a speedbar major mode for displaying Info nodes, and modeled
;;         the minor mode after it.  Completely replaced the old info display
;;         with the major mode, and mixed them to move nicely from major to
;;         minor mode effortlessly.
;; 0.2.1   Added section adding major display mode at load time.

(require 'speedbar)
(require 'info)

;;; Code:
(defvar Info-speedbar-key-map nil
  "Keymap used when in the info display mode.")

(defun Info-install-speedbar-variables ()
  "Install those variables used by speedbar to enhance Info."
  (if Info-speedbar-key-map
      nil
    (setq Info-speedbar-key-map (speedbar-make-specialized-keymap))

    ;; Basic tree features
    (define-key Info-speedbar-key-map "e" 'speedbar-edit-line)
    (define-key Info-speedbar-key-map "\C-m" 'speedbar-edit-line)
    (define-key Info-speedbar-key-map "+" 'speedbar-expand-line)
    (define-key Info-speedbar-key-map "=" 'speedbar-expand-line)
    (define-key Info-speedbar-key-map "-" 'speedbar-contract-line)
    (define-key Info-speedbar-key-map " " 'speedbar-toggle-line-expansion)
    )

  (speedbar-add-expansion-list '("Info" Info-speedbar-menu-items
				 Info-speedbar-key-map
				 Info-speedbar-hierarchy-buttons)))

(defvar Info-speedbar-menu-items
  '(["Browse Node" speedbar-edit-line t]
    ["Expand Node" speedbar-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Contract Node" speedbar-contract-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.-. "))]
    )
  "Additional menu-items to add to speedbar frame.")

;; Make sure our special speedbar major mode is loaded
(if (featurep 'speedbar)
    (Info-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'Info-install-speedbar-variables))

;;; Info hierarchy display method
;;;###autoload
(defun Info-speedbar-browser ()
  "Initialize speedbar to display an info node browser.
This will add a speedbar major display mode."
  (interactive)
  (require 'speedbar)
  ;; Make sure that speedbar is active
  (speedbar-frame-mode 1)
  ;; Now, throw us into Info mode on speedbar.
  (speedbar-change-initial-expansion-list "Info")
  )

(defvar Info-speedbar-image-button-alist
  '(("<+>" . ezimage-document-plus)
    ("<->" . ezimage-document-minus)
    ("[+]" . ezimage-page-plus)
    ("[-]" . ezimage-page-minus)
    ("[?]" . ezimage-page)
    ("[ ]" . ezimage-page)
    )
  "Image buttons used for Info mode.")

(defun Info-speedbar-hierarchy-buttons (directory depth &optional node)
  "Display an Info directory hierarchy in speedbar.
DIRECTORY is the current directory in the attached frame.
DEPTH is the current indentation depth.
NODE is an optional argument that is used to represent the
specific node to expand."
  (if (and (not node)
	   (save-excursion (goto-char (point-min))
			   (looking-at "Info Nodes:")))
      ;; Update our "current node" maybe?
      nil
    ;; We cannot use the generic list code, that depends on all leaves
    ;; being known at creation time.
    (if (not node)
	(speedbar-with-writable (insert "Info Nodes:\n")))
    (let ((completions nil)
	  (speedbar-expand-image-button-alist
	   Info-speedbar-image-button-alist))
      (dframe-select-attached-frame speedbar-frame)
      (save-window-excursion
	(setq completions
	      (Info-speedbar-fetch-file-nodes (or node '"(dir)top"))))
      (select-frame speedbar-frame)
      (if completions
	  (speedbar-with-writable
	   (while completions
	     (speedbar-make-tag-line (if (= depth 0)
					 'angle
				       'bracket)
				     ?+ 'Info-speedbar-expand-node
				     (cdr (car completions))
				     (car (car completions))
				     'Info-speedbar-goto-node
				     (cdr (car completions))
				     'info-xref depth)
	     (setq completions (cdr completions)))
	   t)
	nil))))
  
(defun Info-speedbar-goto-node (text node indent)
  "When user clicks on TEXT, goto an info NODE.
The INDENT level is ignored."
  (dframe-select-attached-frame speedbar-frame)
  (let* ((buff (or (get-buffer "*info*")
		   (progn (info) (get-buffer "*info*"))))
	 (bwin (get-buffer-window buff 0)))
    (if bwin
	(progn
	  (select-window bwin)
	  (raise-frame (window-frame bwin)))
      (if dframe-power-click
	  (let ((pop-up-frames t)) (select-window (display-buffer buff)))
	(dframe-select-attached-frame speedbar-frame)
	(switch-to-buffer buff)))
    (if (string-match "^(\\([^)]+\\))\\([^,:]+\\)$" node)
	(let ((file (match-string 1 node))
	      (node (match-string 2 node)))
	  (Info-find-node file node)
	  ;; If we do a find-node, and we were in info mode, restore
	  ;; the old default method.  Once we are in info mode, it makes
	  ;; sense to return to whatever method the user was using before.
	  (if (string= speedbar-initial-expansion-list-name "Info")
	      (speedbar-change-initial-expansion-list
	       speedbar-previously-used-expansion-list-name))))))

(defun Info-speedbar-expand-node (text token indent)
  "Expand the node the user clicked on.
TEXT is the text of the button we clicked on, a + or - item.
TOKEN is data related to this node (NAME . FILE).
INDENT is the current indentation depth."
  (let ((speedbar-expand-image-button-alist Info-speedbar-image-button-alist))
    (cond ((string-match "+" text)	;we have to expand this file
	   (speedbar-change-expand-button-char ?-)
	   (if (speedbar-with-writable
		 (save-excursion
		   (end-of-line) (forward-char 1)
		   (Info-speedbar-hierarchy-buttons nil (1+ indent) token)))
	       (speedbar-change-expand-button-char ?-)
	     (speedbar-change-expand-button-char ??)))
	  ((string-match "-" text)	;we have to contract this node
	   (speedbar-change-expand-button-char ?+)
	   (speedbar-delete-subblock indent))
	  (t (error "Ooops... not sure what to do")))
    (speedbar-center-buffer-smartly)))
  
(defun Info-speedbar-fetch-file-nodes (nodespec)
  "Fetch the subnodes from the info NODESPEC.
NODESPEC is a string of the form: (file)node.
Optional THISFILE represends the filename of"
  (save-excursion
    ;; Set up a buffer we can use to fake-out Info.
    (set-buffer (get-buffer-create "*info-browse-tmp*"))
    (if (not (equal major-mode 'Info-mode))
	(Info-mode))
    ;; Get the node into this buffer
    (if (string-match "^(\\([^)]+\\))\\([^,:]+\\)$" nodespec)
	(let ((file (match-string 1 nodespec))
	      (node (match-string 2 nodespec)))
	  (Info-find-node file node))
      (error "Node %s not found!" nodespec))
    ;; Scan the created buffer
    (goto-char (point-min))
    (let ((completions nil)
	  (thisfile (progn (string-match "^(\\([^)]+\\))" nodespec)
			   (match-string 1 nodespec))))
      ;; Always skip the first one...
      (re-search-forward "\n\\* \\([^:\t\n]*\\):" nil t)
      (while (re-search-forward "\n\\* \\([^:\t\n]*\\):" nil t)
	(let ((name (match-string 1)))
	  (if (looking-at " *\\(([^)]+)[^.\n]+\\)\\.")
	      (setq name (cons name (match-string 1)))
	    (if (looking-at "[ \t]*\\(([^)]+)\\)\\.")
		(setq name (cons name (concat (match-string 1) "Top")))
	      (if (looking-at " \\([^.]+\\).")
		  (setq name
			(cons name (concat "(" thisfile ")" (match-string 1))))
		(setq name (cons name (concat "(" thisfile ")" name))))))
	  (setq completions (cons name completions))))
      (nreverse completions))))

;;; Info mode node listing
;;;###autoload
(defun Info-speedbar-buttons (buffer)
  "Create a speedbar display to help navigation in an Info file.
BUFFER is the buffer speedbar is requesting buttons for."
  (if (save-excursion (goto-char (point-min))
		      (not (looking-at "Info Nodes:")))
      (erase-buffer))
  (Info-speedbar-hierarchy-buttons nil 0)
  )

(provide 'sb-info)

;;; Overriding preinstalled code.
;;;###autoload
(eval-after-load "info" '(require 'sb-info))
;;; sb-info.el ends here
