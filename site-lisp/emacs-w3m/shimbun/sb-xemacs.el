;;; sb-xemacs.el --- shimbun backend for xemacs.org

;; Copyright (C) 2001, 2002 Akihiro Arisawa    <ari@mbf.sphere.ne.jp>
;; Copyright (C) 2001, 2002 Yuuichi Teranishi  <teranisi@gohome.org>

;; Author: Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
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
(require 'sb-glimpse)

(luna-define-class shimbun-xemacs (shimbun-glimpse) ())

(defvar shimbun-xemacs-url "http://list-archive.xemacs.org/")
(defvar shimbun-xemacs-groups '("xemacs-announce"
				"xemacs-beta-ja" "xemacs-beta"
				"xemacs-build-reports" "xemacs-cvs"
				"xemacs-design" "xemacs-mule" "xemacs-nt"
				"xemacs-patches" "xemacs-users-ja" "xemacs"))
(defvar shimbun-xemacs-coding-system 'euc-jp)
(defvar shimbun-xemacs-reverse-flag nil)
(defvar shimbun-xemacs-litemplate-regexp
  "<td><strong><a name=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+.html\\)\">\\([^<]+\\)</a></strong>\n<td><em>\\([^<]+\\)</em>")
(defvar shimbun-xemacs-x-face-alist
  '(("default" . "X-Face: %@A&y\\ef)A6pi|q43;M>uyhO)~N\
P*fpdo0XrUuutf0|nku\\O5JV(7EG%odc'n6}G@tYRl+B\n #[n,%B\
`.sHZ5>3MZvrm%,rWE7)c}ZXjH\\>=p@AL\\y\\gyu|.lJ8B`F++86")))

(provide 'sb-xemacs)

;;; sb-xemacs.el ends here
