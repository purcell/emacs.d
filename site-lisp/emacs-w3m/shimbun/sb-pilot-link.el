;;; sb-pilot-link.el --- shimbun backend for pilot-link

;; Copyright (C) 2002, 2003, 2005 NAKAJIMA Mikio <minakaji@namazu.org>

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

(luna-define-class shimbun-pilot-link (shimbun-mailman) ())

(defvar shimbun-pilot-link-url "http://www.pilot-link.org/pipermail")

(defconst shimbun-pilot-link-group-path-alist
  '(("announce" . "pilot-link-announce")
    ("devel" . "pilot-link-devel")
    ("general" . "pilot-link-general")
    ("unix-ng" . "pilot-unix-ng")))

(defvar shimbun-pilot-link-groups
  (mapcar 'car shimbun-pilot-link-group-path-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-pilot-link))
  (concat (shimbun-url-internal shimbun)
	  "/"
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-pilot-link-group-path-alist))
	  "/"))

;;(luna-define-method shimbun-reply-to ((shimbun shimbun-pilot-link))
;;  "")

(luna-define-method shimbun-make-contents :after
  ((shimbun shimbun-pilot-link) header)
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

(provide 'sb-pilot-link)
;;; sb-pilot-link.el ends here
