;;; sb-hash.el --- shimbun backend for contents hashing -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2006, 2009 Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>

;; Author: Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>
;; Keywords: shimbun

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

;;; method memo
;; hash save/restore
;; shimbun-hash-set-item shimbun id item
;; shimbun-hash-get-item shimbun id
;;  # if item is existence, call update method
;; item update
;; shimbun-hash-update-items shimbun
;; shimbun-hash-update-items-impl (generic) shimbun
;;  # page fetched,require hash creation.
;; shimbun-hash-contents-url
;;  # normaly,using `shimbun-index-url'

;;; recommend
;; shimbun-get-headers
;;  # if runnable, shimbun-hash-update-items call(re-use loaded page)
;; shimbun-make-contents
;;  # hashed item from hash(if nil,call update) loaded to buffer.
;;  # work only mail body orthopedics.

;;; Code:

(require 'shimbun)

(eval-and-compile
  (luna-define-class content-hash () (hash))
  (luna-define-internal-accessors 'content-hash))

(defvar content-hash-content-hash-length 31)

(luna-define-method initialize-instance :after ((content content-hash)
						&rest init-args)
  (content-hash-set-hash-internal
   content
   (make-vector content-hash-content-hash-length 0))
  content)

(luna-define-generic content-hash-get-item (content id)
  "Return target ID related contents.")
(luna-define-method content-hash-get-item ((content content-hash) id)
  (let ((sym (intern-soft id (content-hash-hash-internal content))))
    (when sym
      (symbol-value sym))))

(luna-define-generic content-hash-set-item (content id item)
  "Save ID related contents ITEM to hash.")
(luna-define-method content-hash-set-item ((content content-hash) id item)
  (set (intern id (content-hash-hash-internal content)) item))

(luna-define-generic content-hash-contents-url (content shimbun)
  "Return contents url.")
(luna-define-method content-hash-contents-url ((content content-hash) shimbun)
  (shimbun-index-url shimbun))

(luna-define-generic content-hash-update-items (content shimbun)
  "Update hash items.
Call timing for `shimbun-get-headers' and `content-hash-shimbun-article'
(`shimbun-make-contents'). Need implements `content-hash-update-items-impl'.")
(luna-define-method content-hash-update-items ((content content-hash) shimbun)
  (with-temp-buffer
    (erase-buffer)
    (shimbun-retrieve-url (content-hash-contents-url content shimbun)
			  'reload)
    (content-hash-update-items-impl content shimbun)))

(luna-define-generic content-hash-update-items-impl (content shimbun)
  "Update hash items main routine (need implements).")

(defun content-hash-shimbun-article (content shimbun header &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (w3m-insert-string
       (or (with-temp-buffer
	     (erase-buffer)
	     (let ((buf-string nil)
		   id)
	       (setq id (shimbun-header-id header))
	       (setq buf-string (content-hash-get-item content id))
	       (unless buf-string
		 (content-hash-update-items content shimbun)
		 (setq buf-string (content-hash-get-item
				   content (shimbun-header-id header))))
	       (when buf-string
		 (insert buf-string)
		 (shimbun-message shimbun "shimbun: Make contents...")
		 (goto-char (point-min))
		 (prog1 (shimbun-make-contents shimbun header)
		   (shimbun-message shimbun
				    "shimbun: Make contents...done")))))
	   "")))))

(provide 'sb-hash)

;;; sb-hash.el ends here
