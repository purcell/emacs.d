;;; wl-highlight.el --- Hilight modules for Wanderlust.

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004
;;  Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;

;;; Code:
;;

(require 'invisible)
(if (and (featurep 'xemacs)
	 (featurep 'dragdrop))
    (require 'wl-dnd))
(require 'wl-vars)
(provide 'wl-highlight)			; circular dependency

(eval-when-compile
  (cond (wl-on-xemacs
	 (require 'wl-xmas))
	(wl-on-emacs21
	 (require 'wl-e21))
	(t
	 (require 'wl-mule)))
  (defun-maybe wl-dnd-set-drop-target (a b))
  (defun-maybe wl-dnd-set-drag-starter (a b)))

(put 'wl-defface 'lisp-indent-function 'defun)

(defgroup wl-faces nil
  "Wanderlust, Faces."
  :prefix "wl-highlight-"
  :group 'wl-highlight
  :group 'wl)

(defgroup wl-summary-faces nil
  "Wanderlust, Faces of summary buffer."
  :prefix "wl-highlight-"
  :group 'wl-highlight
  :group 'wl-summary)

(defgroup wl-folder-faces nil
  "Wanderlust, Faces of folder buffer."
  :prefix "wl-highlight-"
  :group 'wl-highlight
  :group 'wl-folder)

(defgroup wl-message-faces nil
  "Wanderlust, Faces of message buffer."
  :prefix "wl-highlight-"
  :group 'wl-highlight)

;; for message header and signature

(wl-defface wl-highlight-message-headers
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background dark))
     (:foreground "gray" :bold t))
    (((class color)
      (background light))
     (:foreground "gray50" :bold t)))
  "Face used for displaying header names."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-header-contents
  '(
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((class color)
      (background dark))
     (:foreground "LightSkyBlue" :bold t))
    (((class color)
      (background light))
     (:foreground "purple" :bold t)))
  "Face used for displaying header content."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-important-header-contents
  '(
    (((type tty)
      (background dark))
     (:foreground "yellow"))
    (((class color)
      (background dark))
     (:foreground "yellow" :bold t))
    (((class color)
      (background light))
     (:foreground "brown" :bold t)))
  "Face used for displaying contents of special headers."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-important-header-contents2
  '(
    (((type tty)
      (background dark))
     (:foreground "red"))
    (((class color)
      (background dark))
     (:foreground "orange" :bold t))
    (((class color)
      (background light))
     (:foreground "DarkSlateBlue" :bold t)))
  "Face used for displaying contents of special headers."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-citation-header
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background dark))
     (:foreground "SkyBlue"))
    (((class color)
      (background light))
     (:foreground "DarkGreen")))
  "Face used for displaying header of quoted texts."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-unimportant-header-contents
  '(
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((class color)
      (background dark))
     (:foreground "GreenYellow" :bold t))
    (((class color)
      (background light))
     (:foreground "DarkGreen" :bold t)))
  "Face used for displaying contents of unimportant headers."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-signature
  '((((class color)
      (background dark))
     (:foreground "khaki"))
    (((class color)
      (background light))
     (:foreground "DarkSlateBlue")))
  "Face used for displaying signature."
  :group 'wl-message-faces
  :group 'wl-faces)

;; for draft

(wl-defface wl-highlight-header-separator-face
  '(
    (((type tty)
      (background dark))
     (:foreground "black" :background "yellow"))
    (((class color))
     (:foreground "Black" :background "DarkKhaki")))
  "Face used for displaying header separator."
  :group 'wl-draft
  :group 'wl-faces)

;; important messages

(wl-defface wl-highlight-summary-flagged-face
  '((((type tty)
      (background dark))
     (:foreground "magenta"))
    (((class color)
      (background dark))
     (:foreground "orange"))
    (((class color)
      (background light))
     (:foreground "purple")))
  "Face used for displaying flagged messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-new-face
  '(
    (((type tty)
      (background dark))
     (:foreground "red"))
    (((class color)
      (background dark))
     (:foreground "tomato"))
    (((class color)
      (background light))
     (:foreground "tomato")))
  "Face used for displaying new messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-killed-face
  '((((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background dark))
     (:foreground "gray"))
    (((class color))
     (:foreground "LightSlateGray")))
  "Face used for displaying killed messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-displaying-face
  '((t
     (:underline t :bold t)))
  "Face used for displaying message."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-thread-indent-face
  '((t
     (:foreground "gray40")))
  "Face used for displaying indented thread."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; unimportant messages

(wl-defface wl-highlight-summary-unread-face
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background dark))
     (:foreground "LightSkyBlue"))
    (((class color)
      (background light))
     (:foreground "RoyalBlue")))
  "Face used for displaying unread messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-disposed-face
  '(
    (((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background dark))
     (:foreground "gray"))
    (((class color)
      (background light))
     (:foreground "DarkKhaki")))
  "Face used for displaying messages mark as disposed."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-deleted-face
  '(
    (((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background dark))
     (:foreground "SteelBlue"))
    (((class color)
      (background light))
     (:foreground "RoyalBlue4")))
  "Face used for displaying messages mark as deleted."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-prefetch-face
  '(
    (((type tty)
      (background dark))
     (:foreground "Green"))
    (((class color)
      (background dark))
     (:foreground "DeepSkyBlue"))
    (((class color)
      (background light))
     (:foreground "brown")))
  "Face used for displaying messages mark as deleted."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-resend-face
  '(
    (((type tty)
      (background dark))
     (:foreground "Yellow"))
    (((class color)
      (background dark))
     (:foreground "orange3"))
    (((class color)
      (background light))
     (:foreground "orange3")))
  "Face used for displaying messages mark as resend."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-refiled-face
  '(
    (((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background light))
     (:foreground "firebrick")))
  "Face used for displaying messages mark as refiled."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-copied-face
  '(
    (((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background light))
     (:foreground "blue")))
  "Face used for displaying messages mark as copied."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; answered
(wl-defface wl-highlight-summary-answered-face
  '((((type tty)
      (background dark))
     (:foreground "yellow"))
    (((class color)
      (background dark))
     (:foreground "khaki"))
    (((class color)
      (background light))
     (:foreground "khaki4")))
  "Face used for displaying answered messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; forwarded
(wl-defface wl-highlight-summary-forwarded-face
  '((((type tty)
      (background dark))
     (:foreground "yellow"))
    (((class color)
      (background dark))
     (:foreground "DarkOliveGreen2"))
    (((class color)
      (background light))
     (:foreground "DarkOliveGreen4")))
  "Face used for displaying forwarded messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-summary-persistent-mark-face
  '((((type tty))
     (:foreground "blue"))
    (((class color)
      (background dark))
     (:foreground "SeaGreen4"))
    (((class color)
      (background light))
     (:foreground "SeaGreen1")))
  "Dafault face used for displaying messages with persistent mark."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; obsolete.
(wl-defface wl-highlight-summary-temp-face
  '(
    (((type tty)
      (background dark))
     (:foreground "gold"))
    (((class color))
     (:foreground "HotPink1")))
  "Face used for displaying messages mark as temp."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-target-face
  '(
    (((type tty)
      (background dark))
     (:foreground "gold"))
    (((class color))
     (:foreground "HotPink1")))
  "Face used for displaying messages mark as target."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-low-read-face
  '(
    (((type tty)
      (background dark))
     (:foreground "yellow" :italic t))
    (((class color)
      (background dark))
     (:foreground "PaleGreen" :italic t))
    (((class color)
      (background light))
     (:foreground "Green3" :italic t)))
  "Face used for displaying low interest read messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-high-read-face
  '(
    (((type tty))
     (:bold t))
    (((class color)
      (background dark))
     (:foreground "PaleGreen" :bold t))
    (((class color)
      (background light))
     (:foreground "SeaGreen" :bold t)))
  "Face used for displaying high interest read messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-low-unread-face
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan" :italic t))
    (((class color)
      (background dark))
     (:foreground "LightSkyBlue" :italic t))
    (((class color)
      (background light))
     (:foreground "RoyalBlue" :italic t)))
  "Face used for displaying low interest unread messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-high-unread-face
  '(
    (((type tty))
     (:foreground "red" :bold t))
    (((class color)
      (background dark))
     (:foreground "tomato" :bold t))
    (((class color)
      (background light))
     (:foreground "tomato" :bold t)))
  "Face used for displaying high interest unread messages."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; ordinary messages

(wl-defface wl-highlight-summary-thread-top-face
  '(
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((class color)
      (background dark))
     (:foreground "GreenYellow"))
    (((class color)
      (background light))
     (:foreground "green4")))
  "Face used for displaying top thread message."
  :group 'wl-summary-faces
  :group 'wl-faces)

(wl-defface wl-highlight-summary-normal-face
  '(
    (((type tty)
      (background dark))
     (:foreground "yellow"))
    (((class color)
      (background dark))
     (:foreground "PaleGreen"))
    (((class color)
      (background light))
     (:foreground "SeaGreen")))
  "Face used for displaying normal message."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; folder

(wl-defface wl-highlight-folder-unknown-face
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background dark))
     (:foreground "pink"))
    (((class color)
      (background light))
     (:foreground "RoyalBlue")))
  "Face used for displaying unread folder."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-killed-face
  '(
    (((type tty)
      (background dark))
     (:foreground "gray"))
    (((class color))
     (:foreground "gray50")))
  "Face used for displaying killed folder."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-zero-face
  '(
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((class color)
      (background dark))
     (:foreground "SkyBlue"))
    (((class color)
      (background light))
     (:foreground "BlueViolet")))
  "Face used for displaying folder needs no sync."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-few-face
  '(
    (((type tty)
      (background dark))
     (:foreground "yellow"))
    (((class color)
      (background dark))
     (:foreground "orange"))
    (((class color)
      (background light))
     (:foreground "OrangeRed3")))
  "Face used for displaying folder contains few unsync messages."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-many-face
  '(
    (((type tty)
      (background dark))
     (:foreground "red"))
    (((class color)
      (background dark))
     (:foreground "HotPink1"))
    (((class color)
      (background light))
     (:foreground "tomato")))
  "Face used for displaying folder contains many unsync messages."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-unread-face
  '(
    (((type tty)
      (background dark))
     (:foreground "magenta"))
    (((class color)
      (background dark))
     (:foreground "gold"))
    (((class color)
      (background light))
     (:foreground "MediumVioletRed")))
  "Face used for displaying unread folder."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-opened-face
  '(
    (((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color)
      (background dark))
     (:foreground "PaleGreen"))
    (((class color)
      (background light))
     (:foreground "ForestGreen")))
  "Face used for displaying opened group folder."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-closed-face
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background dark))
     (:foreground "GreenYellow"))
    (((class color)
      (background light))
     (:foreground "DarkOliveGreen4")))
  "Face used for displaying closed group folder."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-folder-path-face
  '((t
     (:bold t :underline t)))
  "Face used for displaying path."
  :group 'wl-folder-faces
  :group 'wl-faces)

(wl-defface wl-highlight-demo-face
  '((((type tty))
     (:foreground "green"))
    (((class color)
      (background light))
     (:foreground "#006600" :background "#d9ffd9"))
    (((class color)
      (background dark))
     (:foreground "#d9ffd9" :background "#004400")))
  "Face used for displaying demo."
  :group 'wl-faces)

(wl-defface wl-highlight-logo-face
  '((((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background light))
     (:foreground "SteelBlue" :background "#d9ffd9"))
    (((class color)
      (background dark))
     (:foreground "SkyBlue" :background "#004400")))
  "Face used for displaying demo."
  :group 'wl-faces)

(wl-defface wl-highlight-action-argument-face
  '((((class color)
      (background dark))
     (:foreground "pink"))
    (((class color)
      (background light))
     (:foreground "red")))
  "Face used for displaying action argument."
  :group 'wl-summary-faces
  :group 'wl-faces)

;; cited face

(wl-defface wl-highlight-message-cited-text-1
  '(
    (((type tty)
      (background dark))
     (:foreground "magenta"))
    (((class color)
      (background dark))
     (:foreground "HotPink1"))
    (((class color)
      (background light))
     (:foreground "ForestGreen")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-2
  '(
    (((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color))
     (:foreground "violet")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-3
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color))
     (:foreground "orchid3")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-4
  '(
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((class color))
     (:foreground "purple1")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-5
  '(
    (((type tty)
      (background dark))
     (:foreground "yellow"))
    (((class color))
     (:foreground "MediumPurple1")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-6
  '(
    (((type tty)
      (background dark))
     (:foreground "red"))
    (((class color))
     (:foreground "PaleVioletRed")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-7
  '(
    (((type tty)
      (background dark))
     (:foreground "magenta"))
    (((class color))
     (:foreground "LightPink")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-8
  '(
    (((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color))
     (:foreground "salmon")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-9
  '(
    (((type tty)
      (background dark))
     (:foreground "cyan"))
    (((class color))
     (:foreground "SandyBrown")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(wl-defface wl-highlight-message-cited-text-10
  '(
    (((type tty)
      (background dark))
     (:foreground "green"))
    (((class color))
     (:foreground "wheat")))
  "Face used for displaying quoted text from other messages."
  :group 'wl-message-faces
  :group 'wl-faces)

(defface wl-message-header-narrowing-face
  '((((class color) (background light))
     (:foreground "black" :background "dark khaki"))
    (((class color) (background dark))
     (:foreground "white" :background "dark goldenrod"))
    (t (:bold t)))
  "Face used for header narrowing for the message."
  :group 'wl-message-faces
  :group 'wl-faces)

(defvar wl-highlight-folder-opened-regexp "^ *\\(\\[\\-\\]\\)")
(defvar wl-highlight-folder-closed-regexp "^ *\\(\\[\\+\\]\\)")
(defvar wl-highlight-folder-leaf-regexp "[ ]*\\([-%\\+]\\)\\(.*\\):.*$")

(defvar wl-highlight-citation-face-list
  '(wl-highlight-message-cited-text-1
    wl-highlight-message-cited-text-2
    wl-highlight-message-cited-text-3
    wl-highlight-message-cited-text-4
    wl-highlight-message-cited-text-5
    wl-highlight-message-cited-text-6
    wl-highlight-message-cited-text-7
    wl-highlight-message-cited-text-8
    wl-highlight-message-cited-text-9
    wl-highlight-message-cited-text-10))

(defun wl-delete-all-overlays ()
  "Delete all momentary overlays."
  (let ((overlays (overlays-in (point-min) (point-max)))
	overlay)
    (while (setq overlay (car overlays))
      (if (overlay-get overlay 'wl-momentary-overlay)
	  (delete-overlay overlay))
      (setq overlays (cdr overlays)))))

(defun wl-highlight-summary-displaying ()
  (interactive)
  (wl-delete-all-overlays)
  (let (bol eol ov)
    (save-excursion
      (end-of-line)
      (setq eol (point))
      (beginning-of-line)
      (setq bol (point))
      (setq ov (make-overlay bol eol))
      (overlay-put ov 'face 'wl-highlight-summary-displaying-face)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'wl-momentary-overlay t))))

(defun wl-highlight-folder-group-line (numbers)
  (end-of-line)
  (let ((eol (point))
	bol)
    (beginning-of-line)
    (setq bol (point))
    (let ((text-face (cond ((looking-at wl-highlight-folder-opened-regexp)
			    'wl-highlight-folder-opened-face)
			   ((looking-at wl-highlight-folder-closed-regexp)
			    'wl-highlight-folder-closed-face))))
      (if (and wl-highlight-folder-by-numbers
	       (re-search-forward "[0-9-]+/[0-9-]+/[0-9-]+" eol t))
	  (let* ((unsync (nth 0 numbers))
		 (unread (nth 1 numbers))
		 (face (cond ((and unsync (zerop unsync))
			      (if (and unread (> unread 0))
				  'wl-highlight-folder-unread-face
				'wl-highlight-folder-zero-face))
			     ((and unsync
				   (>= unsync wl-folder-many-unsync-threshold))
			      'wl-highlight-folder-many-face)
			     (t
			      'wl-highlight-folder-few-face))))
	    (if (numberp wl-highlight-folder-by-numbers)
		(progn
		  (put-text-property bol (match-beginning 0) 'face text-face)
		  (put-text-property (match-beginning 0) (match-end 0)
				     'face face))
	      ;; Remove previous face.
	      (put-text-property bol (match-end 0) 'face nil)
	      (put-text-property bol (match-end 0) 'face face)))
	(put-text-property bol eol 'face text-face)))))

(defsubst wl-highlight-get-face-by-name (format &rest args)
  (let ((face (intern (apply #'format format args))))
    (and (find-face face)
	 face)))

(defsubst wl-highlight-summary-line-face-spec (status temp-mark indent)
  "Return a cons cell of (face . argument)."
  (or (let (action)
	(and (setq action (assoc temp-mark wl-summary-mark-action-list))
	     (cons (nth 5 action) (nth 2 action))))
      (let ((flags (elmo-message-status-flags status)))
	(cond
	 ((and (string= temp-mark wl-summary-score-over-mark)
	       (or (memq 'new flags) (memq 'unread flags)))
	  '(wl-highlight-summary-high-unread-face))
	 ((and (string= temp-mark wl-summary-score-below-mark)
	       (or (memq 'new flags) (memq 'unread flags)))
	  '(wl-highlight-summary-low-unread-face))
	 ((let ((priorities wl-summary-persistent-mark-priority-list)
		(fl wl-summary-flag-alist)
		face result global-flags)
	    (while (and (null result) priorities)
	      (cond
	       ((eq (car priorities) 'killed)
		(when (elmo-message-status-killed-p status)
		  (setq result '(wl-highlight-summary-killed-face))))
	       ((eq (car priorities) 'flag)
		(when (setq global-flags
			    (elmo-get-global-flags flags 'ignore-preserved))
		  (while fl
		    (when (memq (car (car fl)) global-flags)
		      (setq result
			    (list (or (wl-highlight-get-face-by-name
				       "wl-highlight-summary-%s-flag-face"
				       (car (car fl)))
				      'wl-highlight-summary-flagged-face))
			    fl nil))
		    (setq fl (cdr fl)))
		  (unless result
		    (setq result (list 'wl-highlight-summary-flagged-face)))))
	       ((memq (car priorities) flags)
		(setq result
		      (list (or (wl-highlight-get-face-by-name
				 "wl-highlight-summary-%s-face"
				 (car priorities))
				'wl-summary-persistent-mark-face)))))
	      (setq priorities (cdr priorities)))
	    result))
	 ((string= temp-mark wl-summary-score-below-mark)
	  '(wl-highlight-summary-low-read-face))
	 ((string= temp-mark wl-summary-score-over-mark)
	  '(wl-highlight-summary-high-read-face))
	 (t (if indent
		'(wl-highlight-summary-normal-face)
	      '(wl-highlight-summary-thread-top-face)))))))

(autoload 'elmo-flag-folder-referrer "elmo-flag")
(defun wl-highlight-flag-folder-help-echo (folder number)
  (let ((referer (elmo-flag-folder-referrer folder number)))
    (concat "The message exists in "
	    (mapconcat
	     (lambda (pair)
		   (concat (car pair) "/"
			   (number-to-string
			    (cdr pair))))
	     referer ","))))

(defun wl-highlight-summary-line-help-echo (number beg end &optional string)
  (let ((type (elmo-folder-type-internal wl-summary-buffer-elmo-folder))
	message handler)
    (when (setq handler (cadr (assq type wl-highlight-summary-line-help-echo-alist)))
      (setq message
	    (funcall handler wl-summary-buffer-elmo-folder number))
      (if message
	  (put-text-property beg end 'help-echo
			     message
			     string)))))

(defun wl-highlight-summary-line-string (number line status temp-mark indent)
  (let ((fsymbol (car (wl-highlight-summary-line-face-spec
		       status
		       temp-mark
		       (> (length indent) 0)))))
    (put-text-property 0 (length line) 'face fsymbol line))
  (when wl-use-highlight-mouse-line
    (put-text-property 0 (length line) 'mouse-face 'highlight line))
  (when wl-highlight-summary-line-help-echo-alist
    (wl-highlight-summary-line-help-echo number 0 (length line) line)))

(defun wl-highlight-summary-current-line (&optional number status)
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
	  (case-fold-search nil)
	  (deactivate-mark nil)
	  (number (or number (wl-summary-message-number)))
	  bol eol spec)
      (when number
	(end-of-line)
	(setq eol (point))
	(beginning-of-line)
	(setq bol (point))
	(setq spec (wl-highlight-summary-line-face-spec
		    (or status (wl-summary-message-status number))
		    (wl-summary-temp-mark number)
		    (wl-thread-entity-get-parent-entity
		     (wl-thread-get-entity number))))
	(when (car spec)
	  (put-text-property bol eol 'face (car spec)))
	(when (cdr spec)
	  (put-text-property (next-single-property-change
			      (next-single-property-change
			       bol 'wl-summary-action-argument
			       nil eol)
			      'wl-summary-action-argument nil eol)
			     eol
			     'face
			     'wl-highlight-action-argument-face))
	(when wl-use-highlight-mouse-line
	  (put-text-property bol eol 'mouse-face 'highlight))
	(when wl-highlight-summary-line-help-echo-alist
	  (wl-highlight-summary-line-help-echo number bol eol))
	(when wl-use-dnd
	  (wl-dnd-set-drag-starter bol eol))))))

(defun wl-highlight-folder (start end)
  "Highlight folder between start and end.
Faces used:
  wl-highlight-folder-unknown-face      unread messages
  wl-highlight-folder-zero-face         folder needs no sync
  wl-highlight-folder-few-face          folder contains few unsync messages
  wl-highlight-folder-many-face         folder contains many unsync messages
  wl-highlight-folder-opened-face       opened group folder
  wl-highlight-folder-closed-face       closed group folder

Variables used:
  wl-highlight-folder-opened-regexp     matches opened group folder
  wl-highlight-folder-closed-regexp     matches closed group folder
"
  (interactive "r")
  (if (< end start)
      (let ((s start)) (setq start end end s)))
  (let* ((lines (count-lines start end))
	 (real-end end)
	 gc-message)
    (save-excursion
      (save-restriction
	(widen)
	(narrow-to-region start end)
	(save-restriction
	  (goto-char start)
	  (while (not (eobp))
	    (wl-highlight-folder-current-line)
	    (forward-line)))))))

(defun wl-highlight-folder-path (folder-path)
  "Highlight current folder path...overlay"
  (save-excursion
    (wl-delete-all-overlays)
    (let ((fp folder-path) ov)
      (goto-char (point-min))
      (while (and fp
		  (not (eobp)))
	(beginning-of-line)
	(or (looking-at "^[ ]*\\[[\\+-]\\]\\(.+\\):.*\n")
	    (looking-at "^[ ]*\\([^ \\[].+\\):.*\n"))
	(when (equal
	       (get-text-property (point) 'wl-folder-entity-id)
	       (car fp))
	  (setq fp (cdr fp))
	  (setq ov (make-overlay
		    (match-beginning 1)
		    (match-end 1)))
	  (setq wl-folder-buffer-cur-point (point))
	  (overlay-put ov 'face 'wl-highlight-folder-path-face)
	  (overlay-put ov 'evaporate t)
	  (overlay-put ov 'wl-momentary-overlay t))
	(forward-line)))))

(defun wl-highlight-action-argument-string (string)
  (put-text-property 0 (length string) 'face
		     'wl-highlight-action-argument-face
		     string))

(defun wl-highlight-summary-all ()
  "For evaluation"
  (interactive)
  (wl-highlight-summary (point-min)(point-max)))

(defun wl-highlight-summary (start end &optional lazy)
  "Highlight summary between start and end.
Faces used:
  wl-highlight-summary-unread-face      unread messages
  wl-highlight-summary-deleted-face     messages mark as deleted
  wl-highlight-summary-refiled-face     messages mark as refiled
  wl-highlight-summary-copied-face      messages mark as copied
  wl-highlight-summary-new-face         new messages
  wl-highlight-summary-*-flag-face      flagged messages"
  (if (< end start)
      (let ((s start)) (setq start end end s)))
  (let (lines too-big gc-message e p hend i percent)
    (save-excursion
      (unless wl-summary-lazy-highlight
	(setq lines (count-lines start end)
	      too-big (and wl-highlight-max-summary-lines
			   (> lines wl-highlight-max-summary-lines))))
      (goto-char start)
      (setq i 0)
      (while (and (not (eobp))
		  (< (point) end))
	(when (or (not lazy)
		  (null (get-text-property (point) 'face)))
	  (wl-highlight-summary-current-line))
	(forward-line))
      (unless wl-summary-lazy-highlight
	(message "Highlighting...done")))))

(defun wl-highlight-summary-window (&optional win beg)
  "Highlight summary window.
This function is defined for `window-scroll-functions'"
  (when wl-summary-highlight
    (with-current-buffer (window-buffer win)
      (when (eq major-mode 'wl-summary-mode)
	(let ((start (window-start win))
	      (end (condition-case nil
		       (window-end win t) ;; old emacsen doesn't support 2nd arg.
		     (error (window-end win)))))
	  (wl-highlight-summary start
				end
				'lazy))
	(set-buffer-modified-p nil)))))

(defun wl-highlight-headers (&optional for-draft)
  (let ((beg (point-min))
	(end (or (save-excursion (re-search-forward "^$" nil t)
				 (point))
		 (point-max))))
    (wl-highlight-message beg end nil)
    (unless for-draft
      (when wl-highlight-x-face-function
	(funcall wl-highlight-x-face-function)))
    (run-hooks 'wl-highlight-headers-hook)))

(defun wl-highlight-body-all ()
  (wl-highlight-message (point-min) (point-max) t t))

(defun wl-highlight-body ()
  (let ((beg (or (save-excursion (goto-char (point-min))
				 (re-search-forward "^$" nil t))
		 (point-min)))
	(end (point-max)))
    (wl-highlight-message beg end t)))

(defun wl-highlight-body-region (beg end)
  (wl-highlight-message beg end t t))

(defun wl-highlight-signature-search-simple (beg end)
  "Search signature area in the body message between BEG and END.
Returns start point of signature."
  (save-excursion
    (goto-char end)
    (if (re-search-backward "\n--+ *\n" beg t)
	(if (eq (following-char) ?\n)
	    (1+ (point))
	  (point))
      end)))

(defun wl-highlight-signature-search (beg end)
  "Search signature area in the body message between BEG and END.
Returns start point of signature."
  (save-excursion
    (goto-char end)
    (or
     ;; look for legal signature separator (check at first for fasten)
     (search-backward "\n-- \n" beg t)

     ;; look for dual separator
     (let ((pt (point))
	   separator)
       (prog1
	   (and (re-search-backward "^[^A-Za-z0-9> \t\n]+ *$" beg t)
		;; `10' is a magic number.
		(> (- (match-end 0) (match-beginning 0)) 10)
		(setq separator (buffer-substring (match-beginning 0)
						  (match-end 0)))
		;; We should not use `re-search-backward' for a long word
		;; since it is possible to crash XEmacs because of a bug.
		(if (search-backward (concat "\n" separator "\n") beg t)
		    (1+ (point))
		  (and (search-backward (concat separator "\n") beg t)
		       (bolp)
		       (point))))
	 (goto-char pt)))

     ;; look for user specified signature-separator
     (if (stringp wl-highlight-signature-separator)
	 (re-search-backward wl-highlight-signature-separator nil t);; case one string
       (let ((sep wl-highlight-signature-separator))		;; case list
	 (while (and sep
		     (not (re-search-backward (car sep) beg t)))
	   (setq sep (cdr sep)))
	 (point)))	;; if no separator found, returns end.
     )))

(defun wl-highlight-citation-prefix-index (prefix)
  "Return a face index for a given citation prefix"
  (apply '+ (mapcar (lambda (ch)
                      (cond
                        ((memq ch '(?> ?| ?: ?})) 1)
                        ((memq ch '(9 32)) 0)
                        (t ch)))
		    prefix)))

(defun wl-highlight-message (start end hack-sig &optional body-only)
  "Highlight message headers between start and end.
Faces used:
  wl-highlight-message-headers			  the part before the colon
  wl-highlight-message-header-contents		  the part after the colon
  wl-highlight-message-important-header-contents  contents of \"important\"
                                                  headers
  wl-highlight-message-important-header-contents2 contents of \"important\"
                                                  headers
  wl-highlight-message-unimportant-header-contents contents of unimportant
                                                   headers
  wl-highlight-message-cited-text-N	           quoted text from other
                                                   messages
  wl-highlight-message-citation-header             header of quoted texts
  wl-highlight-message-signature                   signature

Variables used:
  wl-highlight-message-header-alist             alist of header regexp with
                                                face for header contents
  wl-highlight-citation-prefix-regexp		matches lines of quoted text
  wl-highlight-force-citation-header-regexp	matches headers for quoted text
  wl-highlight-citation-header-regexp		matches headers for quoted text

If HACK-SIG is true,then we search backward from END for something that
looks like the beginning of a signature block, and don't consider that a
part of the message (this is because signatures are often incorrectly
interpreted as cited text.)"
  (if (< end start)
      (let ((s start)) (setq start end end s)))
  (let ((too-big (and wl-highlight-max-message-size
		      (> (- end start) wl-highlight-max-message-size)))
	(real-end end)
	current beg
	e p hend
	wl-draft-idle-highlight)
    (unless too-big
      (save-excursion
	(save-restriction
	  (widen)
	  ;; take off signature
	  (when hack-sig
	    (setq end (funcall wl-highlight-signature-search-function
			       (- end wl-max-signature-size) end))
	    (when (not (eq end real-end))
	      (put-text-property end (point-max)
				 'face 'wl-highlight-message-signature)))
	  (narrow-to-region start end)
	  ;; narrow down to just the headers...
	  (goto-char start)
	  (unless body-only
	    (save-restriction
	      ;; If this search fails then the narrowing performed above
	      ;; is sufficient
	      (if (re-search-forward
		   (format "^\\(%s\\)?$" (regexp-quote mail-header-separator))
		   nil t)
		  (narrow-to-region (point-min) (match-beginning 0)))
	      ;; highlight only when header is not too-big.
	      (if  (and wl-highlight-max-header-size
			(>= (point) wl-highlight-max-header-size))
		  (goto-char (point-max))
		(goto-char start)
		(while (not (eobp))
		  (if (looking-at "^[^ \t\n:]+[ \t]*:[ \t]*")
		      (progn
			(setq p (match-end 0))
			(put-text-property (match-beginning 0) p
					   'face 'wl-highlight-message-headers)
			(setq hend (save-excursion (std11-field-end end)))
			(put-text-property
			 p hend 'face
			 (catch 'match
			   (let ((regexp-alist wl-highlight-message-header-alist))
			     (while regexp-alist
			       (when (looking-at (caar regexp-alist))
				 (throw 'match (cdar regexp-alist)))
			       (setq regexp-alist (cdr regexp-alist))))
			   'wl-highlight-message-header-contents))
			(goto-char hend))
		    ;; ignore non-header field name lines
		    (forward-line))))))
	  (when (looking-at
		 (format "^%s$" (regexp-quote mail-header-separator)))
	    (put-text-property (match-beginning 0) (match-end 0)
			       'face 'wl-highlight-header-separator-face)
	    (forward-line))
	  (let (prefix end)
	    (while (null (progn
			     ;; Skip invisible region.
			   (when (invisible-p (point))
			     (goto-char (next-visible-point (point))))
			   (eobp)))
	      (cond
	       ((and wl-highlight-force-citation-header-regexp
		     (looking-at wl-highlight-force-citation-header-regexp))
		(setq current 'wl-highlight-message-citation-header)
		(setq end (match-end 0)))
	       ((and wl-highlight-citation-prefix-regexp
		     (looking-at wl-highlight-citation-prefix-regexp))
		(setq prefix (buffer-substring (point)
					       (match-end 0)))
		(unless wl-highlight-highlight-citation-too
		  (goto-char (match-end 0)))
		(setq current
                      (nth (% (wl-highlight-citation-prefix-index prefix)
                              (length wl-highlight-citation-face-list))
                           wl-highlight-citation-face-list)))
	       ((and wl-highlight-citation-header-regexp
		     (looking-at wl-highlight-citation-header-regexp))
		(setq current 'wl-highlight-message-citation-header)
		(setq end (match-end 0)))
	       (t (setq current nil)))
	      (when current
		(setq p (point))
		(forward-line) ; this is to put the \n in the face too
		(put-text-property p (or end (point)) 'face current)
		(setq end nil)
		(backward-char))
	      (forward-line)))
	  (run-hooks 'wl-highlight-message-hook))))))

;; highlight-mouse-line for folder mode

(defun wl-highlight-folder-mouse-line ()
  (interactive)
  (let* ((end (save-excursion (end-of-line) (point)))
	 (beg (progn
		(re-search-forward "[^ ]" end t)
		(1- (point))))
	 (inhibit-read-only t))
    (put-text-property beg end 'mouse-face 'highlight)))


(require 'product)
(product-provide (provide 'wl-highlight) (require 'wl-version))

;;; wl-highlight.el ends here
