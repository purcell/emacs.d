;;; sb-squeak-dev.el --- shimbun backend for Squeak-dev ML archive

;; Copyright (C) 2003, 2004, 2005 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news

;; This file is a part of shimbun.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'shimbun)
(require 'sb-mailman)
(require 'sendmail)

(luna-define-class shimbun-squeak-dev (shimbun-mailman) ())

(defvar shimbun-squeak-dev-url
  "http://lists.squeakfoundation.org/pipermail/squeak-dev/")

(defvar shimbun-squeak-dev-groups '("main"))

(luna-define-method shimbun-make-contents :after
  ((shimbun shimbun-squeak-dev) header)
  (save-excursion
    (let ((end (and (mail-position-on-field "From") (point)))
	  (begin (progn (beginning-of-line) (point)))
	  (marker (make-marker)))
      (when end
	(narrow-to-region begin end)
	(goto-char (point-min))
	(when (re-search-forward " at " nil t nil)
	  (set-marker marker (match-beginning 0))
	  (delete-region (match-beginning 0) (match-end 0))
	  (goto-char marker)
	  (insert "@"))
	(widen))))
  (buffer-string))

(provide 'sb-squeak-dev)

;;; sb-squeak-dev.el ends here
