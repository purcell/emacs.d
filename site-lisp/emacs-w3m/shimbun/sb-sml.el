;;; sb-sml.el --- shimbun backend for Smalltalkers' Salon Mailing List archive

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

(luna-define-class shimbun-sml (shimbun-mailman) ())

(defvar shimbun-sml-url "http://www.akademia.co.jp/Smalltalk/SML/archives/archive/")

(defvar shimbun-sml-groups '("main"))

;;(luna-define-method shimbun-reply-to ((shimbun shimbun-sml))
;;  "sml@sra.co.jp")

(luna-define-method shimbun-make-contents
  ((shimbun shimbun-sml) header)
  (shimbun-sml-make-contents shimbun header))

(defun shimbun-sml-make-contents (shimbun header)
  (subst-char-in-region (point-min) (point-max) ?\t ?\  t)
  (shimbun-decode-entities)
  (goto-char (point-min))
  (let ((end (search-forward "<!--beginarticle-->")))
    (goto-char (point-min))
    (search-forward "</HEAD>")
    (when (re-search-forward "<H1>\\([^\n]+\\)\\(\n +\\)?</H1>" end t nil)
      (shimbun-header-set-subject
       header
       (shimbun-mime-encode-string (match-string 1))))
    (when (re-search-forward
	   "<B>\\([^\n]+\\)\\(\n +\\)?</B> *\n +\\([^\n]+\\)<BR>" end t nil)
      (shimbun-header-set-from
       header
       (shimbun-mime-encode-string
	(concat (match-string 1) " <" (match-string 3) ">")))
      (when (re-search-forward "<I>\\([^\n]+\\)</I>" end t nil)
	(shimbun-header-set-date header (match-string 1)))
      (delete-region (point-min) end)
      (delete-region (search-forward "<!--endarticle-->") (point-max))
      (shimbun-header-insert-and-buffer-string shimbun header nil t))))

(provide 'sb-sml)
;;; sb-sml.el ends here
