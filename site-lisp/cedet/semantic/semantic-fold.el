;;; semantic-fold.el --- Folding support for Semantic Tags.

;;; Copyright (C) 2005 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-fold.el,v 1.6 2005/09/30 20:20:13 zappo Exp $

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
;; Using semantic-decoration-mode, apply folding icons onto tags.
;; The little +/- icon should be clickable which will then `fold'
;; that tag to one line.

(require 'semantic-decorate-mode)
(require 'ezimage)
(require 'fringe)

;;; Tag Folding Mode
;;
;;; Code:
(define-semantic-decoration-style semantic-tag-folding-decoration
  "Place folding icons in the Emacs fringe.
Clicking on the fringe icon would `fold' methods open or closed."
  :enabled nil)
  
(defun semantic-tag-folding-decoration-p-default (tag)
  "Return non-nil if TAG is a type, or a non-prototype function."
  (let ((c (semantic-tag-class tag)))
    (and
     semantic-folding-mode
     (or
      ;; All types can be folded
      (eq c 'type)
      ;; Functions which aren't prototypes get a line.
      (and (eq c 'function)
           (not (semantic-tag-get-attribute tag :prototype-flag)))
      )
     ;; Random truth
     t)
    ))

(defface semantic-tag-folding-fringe-face
  '((t
     :inherit fringe
     :foreground "yellow"))
  "Face for enabled breakpoint icon in fringe."
  :group 'semantic-face)

;; Don't create them twice.
(when (fboundp 'define-fringe-bitmap)
  (when (fringe-bitmap-p 'tag-fold-plus)
    (destroy-fringe-bitmap 'tag-fold-plus)
    (destroy-fringe-bitmap 'tag-fold-minus))
  ;; Create the fringe bitmaps.
  (define-fringe-bitmap 'tag-fold-plus
    "\x00\x18\x18\x18\xff\xff\x18\x18\x18\x00")
  (define-fringe-bitmap 'tag-fold-minus
    "\x00\x00\x00\x00\xff\xff\x00\x00\x00\x00")
  )

(defun semantic-tag-folding-decoration-highlight-default (tag)
  "Provide a folding +/- icon on TAG."
  (with-current-buffer (semantic-tag-buffer tag)
    (let ((o (semantic-decorate-tag
	      tag
	      (semantic-tag-start tag)
	      (save-excursion
		(goto-char (semantic-tag-start tag))
		(point))
	      nil))
	  (str (copy-sequence "+")))
      (put-text-property 0 1 'display
			 (if (semantic-tag-folded-p tag)
			     (list '(left-fringe
				     tag-fold-plus
				     semantic-tag-folding-fringe-face)
				   "+")
			   (list '(left-fringe
				   tag-fold-minus
				   semantic-tag-folding-fringe-face)
				 "-"))
			 str)
      (semantic-overlay-put o 'before-string str)
      (semantic-overlay-put o 'semantic-fold t)
      )))

;;; The minor mode
;;
;; While the decorator mode will put up the decorations, we need a
;; minor mode to manage the keymap for hte +/- in the fringe.
;;

;;;###autoload
(defun global-semantic-folding-mode (&optional arg)
  "Toggle global use of option `semantic-folding-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle."
  (interactive "P")
  (setq global-semantic-folding-mode
        (semantic-toggle-minor-mode-globally
         'semantic-folding-mode arg)))

;;;###autoload
(defcustom global-semantic-folding-mode nil
  "*If non-nil enable global use of variable `semantic-folding-mode'.
With this mode enabled, a new folding decoration mode is added.
Clicking on a + or - in the fringe will fold that tag."
  :group 'semantic
  :group 'semantic-modes
  :type 'boolean
  :require 'semantic-util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-folding-mode (if val 1 -1))))

(defcustom semantic-folding-mode-hook nil
  "*Hook run at the end of function `semantic-folding-mode'."
  :group 'semantic
  :type 'hook)
  
(defvar semantic-folding-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [left-fringe mouse-1] 'semantic-folding-click)
    km)
  "Keymap for folding minor mode.")

(defvar semantic-folding-mode nil
  "Non-nil if folding minor mode is enabled.
Use the command `semantic-folding-mode' to change this variable.")
(make-variable-buffer-local 'semantic-folding-mode)

(defun semantic-folding-mode-setup ()
  "Setup option `semantic-folding-mode'.
The minor mode can be turned on only if semantic feature is available
and the current buffer was set up for parsing.  In addition,
`semantic-folding-mode' is only available when fringe images are available
in Emacs 20.4."
  (if semantic-folding-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)
		    (fboundp 'define-fringe-bitmap)))
          (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-folding-mode nil)
            (error "Buffer %s cannot be folded by semantic"
                   (buffer-name)))
	;; Enable the decoration.
        )
    ;; Remove hooks
    ;; Disable the decoration.
    )
  semantic-folding-mode)

;;;###autoload
(defun semantic-folding-mode (&optional arg)
  "Minor mode for highlighting changes made in a buffer.
Changes are tracked by semantic so that the incremental parser can work
properly.
This mode will display +/- icons in the fringe.  Clicking on them
will fold the current tag.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
  (interactive
   (list (or current-prefix-arg
             (if semantic-folding-mode 0 1))))
  (setq semantic-folding-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not semantic-folding-mode)))
  (semantic-folding-mode-setup)
  (run-hooks 'semantic-folding-mode-hook)
  (if (interactive-p)
      (message "folding minor mode %sabled"
               (if semantic-folding-mode "en" "dis")))
  semantic-folding-mode)

(semantic-add-minor-mode 'semantic-folding-mode
                         "" ;; fringe will show we are in this mode.
                         semantic-folding-mode-map)

(defun semantic-folding-tag-on-line (pos)
  "Return the tag starting on line POS to be folded.
Do not return the current tag.  Only return a tag if there is
a folding icon on this line."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (let ((ol (semantic-overlays-in (point) (point-at-eol)))
	  (o nil))
      (while (and ol (not o))
	(when (semantic-overlay-get (car ol) 'semantic-fold)
	  (setq o (car ol)))
	(setq ol (cdr ol)))
      (when o
	(goto-char (overlay-start o))
	(semantic-current-tag))
    )))

(defun semantic-folding-click (e)
  "Respond to event E when you click on a folding icon."
  (interactive "e")
  (mouse-minibuffer-check e)
  ;;(message "%S" e)
  ;; This is an Emacs mouse event.
  (let* ((end (posn-point (event-end e)))
	 ;; Look for a tag, and if we are on the
	 ;; folding icon.  Return the tag to fold/unfold.
	 (tag (semantic-folding-tag-on-line end)))
    (if tag
	;; fold
	(progn
	  (semantic-set-tag-folded tag (not (semantic-tag-folded-p tag)))
	  (semantic-tag-folding-decoration-highlight tag)
	  )
      ;; Do the old work
      (mouse-set-point e)
      )))


(provide 'semantic-fold)

;;; semantic-fold.el ends here
