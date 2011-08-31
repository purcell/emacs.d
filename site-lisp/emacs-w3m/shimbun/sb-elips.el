;;; sb-elips.el --- shimbun backend for the Elips mailing list

;; Copyright(C) 2003, 2008 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
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

(luna-define-class shimbun-elips (shimbun-mailman-ja) ())

(defvar shimbun-elips-url "http://www.heimat.gr.jp/pipermail/elips/")
(defvar shimbun-elips-groups '("elips"))
(defvar shimbun-elips-x-face-alist
  '(("default" . "X-Face: 4(*_4GGM'.9>v7]}eY@L8:2Zn7:&ANIR4778Vg*'(f\
:=~'8D'EH!N`K{7@k+7Nmw6p_F}&F\n >e>Rmefuaz,B\\zqJ88R9M082;|cQF\\i$c6\
iXU''QtF,a2iFyp'd9$/Nk+b?gO{FxFmMp}vL7oNNy[i\n ]jV[(mQvWLWF/scE+r[")))

(provide 'sb-elips)

;;; sb-elips.el ends here
