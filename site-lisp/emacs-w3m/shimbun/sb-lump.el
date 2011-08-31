;;; sb-lump.el --- shimbun backend class to check all groups at once

;; Copyright (C) 2001, 2002 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>
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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)
(defvar shimbun-lump-check-interval 300)

(eval-and-compile
  (luna-define-class shimbun-lump (shimbun)
		     (group-header-alist last-check check-interval))
  (luna-define-internal-accessors 'shimbun-lump))

(defsubst shimbun-lump-check-interval (shimbun)
  (or (shimbun-lump-check-interval-internal shimbun)
      shimbun-lump-check-interval))

(defun shimbun-lump-lapse-seconds (time)
  (let ((now (current-time)))
    (+ (* (- (car now) (car time)) 65536)
       (- (nth 1 now) (nth 1 time)))))

(defun shimbun-lump-check-p (shimbun)
  (or (null (shimbun-lump-last-check-internal shimbun))
      (and (shimbun-lump-last-check-internal shimbun)
	   (> (shimbun-lump-lapse-seconds
	       (shimbun-lump-last-check-internal shimbun))
	      (shimbun-lump-check-interval shimbun)))))

(defun shimbun-lump-checked (shimbun)
  (shimbun-lump-set-last-check-internal shimbun (current-time)))

(luna-define-generic shimbun-get-group-header-alist (shimbun &optional range)
  "Return an alist of group and header list.")

(luna-define-method shimbun-headers ((shimbun shimbun-lump) &optional range)
  (when (shimbun-lump-check-p shimbun)
    (shimbun-lump-set-group-header-alist-internal
     shimbun (shimbun-get-group-header-alist shimbun range))
    (shimbun-lump-checked shimbun))
  (cdr (assoc (shimbun-current-group-internal shimbun)
	      (shimbun-lump-group-header-alist-internal shimbun))))

(luna-define-method shimbun-close :after ((shimbun shimbun-lump))
  (shimbun-lump-set-group-header-alist-internal shimbun nil)
  (shimbun-lump-set-last-check-internal shimbun nil))

(provide 'sb-lump)

;;; sb-lump.el ends here
