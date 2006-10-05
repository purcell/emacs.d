;;; eol-conversion.el --- set end-of-line conversion for current buffer

;; Copyright (C) 2000 Francis J. Wright

;; Maintainer: Francis J. Wright <F.J.Wright@maths.qmw.ac.uk>
;; Time-stamp: <14 December 2000>

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package is intended for use with GNU Emacs 20/21 and adds
;; commands, key bindings, and a submenu at the bottom of the Edit
;; menu, to control end-of-line conversion.

;; Based closely on code contributed by
;;   Larry Smith <lsmith@cio2000.eds.com>
;;   Mark Simpson <damned@world.std.com>
;; FJW's contribution was mainly the menu support.

;;; Installation:

;; Put this file somewhere where Emacs can find it (i.e. in one of the
;; directories in your `load-path' such as `site-lisp'), optionally
;; byte-compile it, and put this in your .emacs:
;;
;;   (require 'eol-conversion)

(require 'easymenu)

(defun set-buffer-eol-conversion (type)
  "Convert buffer to TYPE line endings.
TYPE should be one of the symbols: unix, dos, mac."
  (let ((old-bfcs buffer-file-coding-system))
    (set-buffer-file-coding-system
     (coding-system-change-eol-conversion
      (cond ((null buffer-file-coding-system) 'undecided)
	    ((eq buffer-file-coding-system 'no-conversion) 'raw-text)
	    (buffer-file-coding-system))
      type))
    (unless (eq old-bfcs buffer-file-coding-system)
      ;; Larry Smith: This may be necessary to remove ^Ms when going
      ;; from unix to dos.  FJW: When else?
      (if (eq type 'dos)
	  (decode-coding-region (point-min) (point-max)
				buffer-file-coding-system))
      (message "Coding system changed from %s to %s."
	       old-bfcs buffer-file-coding-system)
      )))

(defun set-buffer-eol-conversion-unix ()
  "Convert buffer to UNIX line endings"
  (interactive "*")
  (set-buffer-eol-conversion 'unix))

(defun set-buffer-eol-conversion-dos ()
  "Convert buffer to MS-DOS line endings"
  (interactive "*")
  (set-buffer-eol-conversion 'dos))

(defun set-buffer-eol-conversion-mac ()
  "Convert buffer to Macintosh line endings"
  (interactive "*")
  (set-buffer-eol-conversion 'mac))

(define-key global-map (vector '(control x) '(control m) ?:)
  'set-buffer-eol-conversion-unix)
(define-key global-map (vector '(control x) '(control m) ?\\)
  'set-buffer-eol-conversion-dos)
(define-key global-map (vector '(control x) '(control m) ?/)
  'set-buffer-eol-conversion-mac)

(easy-menu-add-item			; (map path item &optional before)
 nil '("edit") "--")

(easy-menu-add-item			; (map path item &optional before)
 nil '("edit")
 (easy-menu-create-menu			; (menu-name menu-items)
  "End of Line Conversion"
  '(["Unix" set-buffer-eol-conversion-unix
     :style radio
     :selected (eq (coding-system-eol-type buffer-file-coding-system) 0)
     :help "Convert buffer to UNIX line endings"
     :active t]
    ["Dos" set-buffer-eol-conversion-dos
     :style radio
     :selected (eq (coding-system-eol-type buffer-file-coding-system) 1)
     :help "Convert buffer to MS-DOS line endings"
     :active t]
    ["Mac" set-buffer-eol-conversion-mac
     :style radio
     :selected (eq (coding-system-eol-type buffer-file-coding-system) 2)
     :help "Convert buffer to Macintosh line endings"
     :active t]
    )))

(provide 'eol-conversion)

;;; eol-conversion.el ends here
