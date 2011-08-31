;;; sb-w3 --- Speedbar support for w3.

;; Copyright (C) 1997, 1998, 2001, 2002, 2005 Free Software Foundation
;;
;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.2.1
;; Keywords: tags, tools, w3
;; X-RCS: $Id: sb-w3.el,v 1.10 2005/09/30 20:26:13 zappo Exp $
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.org
;;

;;; Commentary:
;;
;;   Speedbar provides a frame in which files, and locations in
;; files are displayed.  These functions provide w3 specific support,
;; showing links and addresses in the side-bar.
;;
;;   To enable, add this to your .emacs file.
;;   (autoload 'w3-speedbar-buttons "sb-w3"
;;             "W3 specific speedbar button generator.")
;;
;;   Load w3-toc to get additional tags which represent the major
;;   headers in a given web page.
;;
;;   This file requires speedbar.

;;; Change log:
;; 0.1   - first revision copied from speedbspec.el V 0.1.1
;; 0.1.1 - Removed dependency on speedbspec
;; 0.2   - T.V.Raman's addition using newer w3, and w3 imenu.
;; 0.2.1 - Added new keymap support

;;; Code:
(eval-and-compile
  (condition-case nil
      (require 'w3-imenu)
    (error nil)))
(require 'cl)
(require 'speedbar)

(defvar w3-speedbar-last-buffer nil
  "The last buffer shown by w3-speedbar.")

(defvar w3-speedbar-key-map nil
  "Keymap used when in the w3 display mode.")

(if w3-speedbar-key-map
    nil
  (setq w3-speedbar-key-map (speedbar-make-specialized-keymap))

  ;; Basic features.
  (define-key w3-speedbar-key-map "e" 'speedbar-edit-line)
  (define-key w3-speedbar-key-map "r" 'speedbar-edit-line)
  (define-key w3-speedbar-key-map "\C-m" 'speedbar-edit-line)
  )

(defvar w3-speedbar-menu-items
  '(["Jump to link" speedbar-edit-line t]
    )
  "Extra menu items for w3 mode.")

;;;###autoload
(defun w3-speedbar-buttons (buffer)
  "Create speedbar buttons for the current web BUFFER displayed in w3 mode."
  (save-excursion
    (goto-char (point-min))
    (if (and (looking-at "History:") (equal w3-speedbar-last-buffer buffer))
	nil
      (setq w3-speedbar-last-buffer buffer)
      (erase-buffer)
      (let ((links (save-excursion (set-buffer buffer)
                                   (w3-only-links)))
            (toc (save-excursion
                   (set-buffer buffer) imenu--index-alist))
	    (part nil))
	(insert "History:\n")
	;; This taken out of w3 which was used to create the history list,
	;; and is here modified to create the speedbar buttons
	(cl-maphash
	 (function
	  (lambda (url desc)
	    (speedbar-insert-button (w3-speedbar-shorten-button url)
				    'speedbar-directory-face 'highlight
				    'w3-speedbar-link url)))
	 url-history-list)
	(insert "Links:\n")
	(while links
	  (setq part (car (cdr (member ':href (car links))))
		links (cdr links))
          (when part 
            (speedbar-insert-button (w3-speedbar-shorten-button part)
                                    'speedbar-file-face 'highlight
                                    'w3-speedbar-link
                                    part)))
	(if (not (featurep 'w3-imenu))
	    nil
	  (insert (format "Contents: %d\n" (length toc)))
	  (loop for e in toc do
		(when (car e)
		  (speedbar-insert-button  (car e)
					   'bold 'highlight
					   'w3-speedbar-goto-marker
					   (cdr e)
					   'prevline))))))))

(defun w3-speedbar-goto-marker (txt marker indent)
  "Speedbar callback function for jumping to a marker in a w3 buffer.
TXT is unused.  MARKER is the location.  INDENT is unused."
  (pop-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker)))
    
(defun w3-speedbar-shorten-button (button)
  "Takes text BUTTON and shortens it as much as possible."
  ;; I should make this more complex, but I'm not sure
  ;; how...
  (when button 
    (let ((fnnd (file-name-nondirectory button)))
      (if (< 0 (length fnnd))
	  fnnd
	(if (string-match "\\(ht\\|f\\)tp://" button)
	    (setq button (substring button (match-end 0))))
	(if (string-match "/$" button)
	    (setq button (substring button 0 (match-beginning 0))))
	button))))

(defun w3-speedbar-link (text token indent)
  "Follow link described by TEXT which has the URL TOKEN.
INDENT is not used."
  (speedbar-with-attached-buffer (w3-fetch token)))

(provide 'sb-w3)
;;; sb-w3.el ends here
