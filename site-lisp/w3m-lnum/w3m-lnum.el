;;; w3m-lnum.el --- Operations using link numbers

;; Copyright (C) 2004, 2005 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file provides a minor mode to enable operations using link
;; numbers.

;;; Usage:

;; Install this file to an appropriate directory, and add these
;; expressions to your ~/.emacs-w3m.

;;      (autoload 'w3m-link-numbering-mode "w3m-lnum" nil t)
;;	(add-hook 'w3m-mode-hook 'w3m-link-numbering-mode)

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'w3m)

(defface w3m-link-numbering-face
  '((((class color) (background light)) (:foreground "gray60"))
    (((class color) (background dark)) (:foreground "gray50")))
  "Face used to highlight link numbers."
  :group 'w3m-face)

(defcustom w3m-link-numbering-mode-hook nil
  "*Hook run after `w3m-link-numbering-mode' initialization."
  :group 'w3m
  :type 'hook)

(defvar w3m-link-numbering-mode-map
  (let ((keymap (make-sparse-keymap)))
    (substitute-key-definition 'w3m-view-this-url
			       'w3m-move-numbered-anchor
			       keymap w3m-mode-map)
    keymap)
  "Keymap used when `w3m-link-numbering-mode' is active.")

(defvar w3m-link-numbering-mode nil
  "Non-nil if w3m operations using link numbers are enabled.")
(make-variable-buffer-local 'w3m-link-numbering-mode)
(unless (assq 'w3m-link-numbering-mode minor-mode-map-alist)
  (push (cons 'w3m-link-numbering-mode w3m-link-numbering-mode-map)
	minor-mode-map-alist))

;;;###autoload
(defun w3m-link-numbering-mode (&optional arg)
  "Minor mode to enable operations using link numbers."
  (interactive "P")
  (add-hook 'w3m-display-functions 'w3m-link-numbering)
  (if (setq w3m-link-numbering-mode
	    (if arg
		(> (prefix-numeric-value arg) 0)
	      (not w3m-link-numbering-mode)))
      (progn
	(w3m-link-numbering)
	(run-hooks 'w3m-link-numbering-mode-hook))
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (overlay-get overlay 'w3m-link-numbering-overlay)
	(delete-overlay overlay)))))

(defun w3m-link-numbering (&rest args)
  "Make overlays that display link numbers."
  (when w3m-link-numbering-mode
    (save-excursion
      (goto-char (point-min))
      (let ((i 0)
	    overlay num)
	(catch 'already-numbered
	  (while (w3m-goto-next-anchor)
	    (when (w3m-anchor)
	      (when (get-char-property (point) 'w3m-link-numbering-overlay)
		(throw 'already-numbered nil))
	      (setq overlay (make-overlay (point) (1+ (point)))
		    num (format "[%d]" (incf i)))
	      (w3m-static-if (featurep 'xemacs)
		  (progn
		    (overlay-put overlay 'before-string num)
		    (set-glyph-face (extent-begin-glyph overlay)
				    'w3m-link-numbering-face))
		(when (featurep 'w3m-e20)
		  (overlay-put overlay
			       'face (get-text-property (point) 'face)))
		(put-text-property 0 (length num)
				   'face 'w3m-link-numbering-face
				   num)
		(overlay-put overlay 'before-string num)
		(overlay-put overlay 'evaporate t))
	      (overlay-put overlay 'w3m-link-numbering-overlay i))))))))

(defun w3m-move-numbered-anchor (&optional arg)
  "Move the point to the specified anchor.
When no prefix argument is specified, call `w3m-view-this-url' instead
of moving cursor."
  (interactive "P")
  (if (and arg
	   (> (setq arg (prefix-numeric-value arg)) 0))
      (catch 'found
	(dolist (overlay (overlays-in (point-min) (point-max)))
	  (when (eq arg (overlay-get overlay 'w3m-link-numbering-overlay))
	    (goto-char (overlay-start overlay))
	    (push (w3m-anchor-sequence) w3m-goto-anchor-hist)
	    (w3m-horizontal-on-screen)
	    (throw 'found (w3m-print-this-url))))
	(error "Cannot found your specified link: %d" arg))
    (w3m-view-this-url)))

(provide 'w3m-lnum)

;;; w3m-lnum.el ends here
