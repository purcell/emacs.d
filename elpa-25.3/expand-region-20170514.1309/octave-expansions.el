;;; octave-expansions.el --- octave-mode expansions for expand-region

;; Copyright (C) 2012 Mark Hepburn

;; Author: Mark Hepburn
;; Keywords: marking region

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Feel free to contribute any other expansions for Octave at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)
(declare-function octave-mark-block "octave-mod")

;;; Octave-mod received a major rewrite between versions 23 and 24 of
;;; Emacs, for example using the new smie package instead of
;;; hand-coding a lot of motion commands.  Unfortunately for our
;;; purposes here, in the process the behaviour of `octave-mark-block'
;;; changed slightly.  So, in order to behave identically across both
;;; versions we need to check which is which in a few places and
;;; adjust accordingly:
(defconst er/old-octave-mod-p (fboundp 'octave-up-block))

(defalias 'er/up-block
  (if er/old-octave-mod-p 'octave-up-block 'up-list))

(defun er/octave-mark-up-block ()
  "Mark the containing block, assuming the current block has
already been marked."
  (interactive)
  (when (use-region-p)
    (when (< (point) (mark))
      (exchange-point-and-mark))
    (er/up-block -1)                    ; -1 means backwards, ie to the front
    (octave-mark-block)))

(defun er/octave-mark-block ()
  "Not for general use; this is a work-around for the different
behaviour of `octave-mark-block' between emacs versions 23 and
24."
  (interactive)
  (forward-word)
  (octave-mark-block))

(defun er/add-octave-expansions ()
  "Adds octave/matlab-specific expansions for buffers in octave-mode"
  (let ((try-expand-list-additions (if er/old-octave-mod-p
                                       '(octave-mark-block
                                         er/octave-mark-up-block
                                         octave-mark-defun)
                                     '(octave-mark-block
                                       er/octave-mark-block
                                       er/octave-mark-up-block
                                       mark-defun))))
    (set (make-local-variable 'er/try-expand-list)
         (append er/try-expand-list try-expand-list-additions))))

(er/enable-mode-expansions 'octave-mode 'er/add-octave-expansions)

(provide 'octave-expansions)
;;; octave-expansions.el ends here
