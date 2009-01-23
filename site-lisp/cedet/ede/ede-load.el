;;; ede-load.el --- Autoload definitions for EDE

;;; Copyright (C) 2003, 2009 David Ponce

;; Author: David Ponce <david@dponce.com>
;; X-RCS: $Id: ede-load.el,v 1.3 2009/01/10 13:30:15 zappo Exp $

;; EDE is free software; you can redistribute it and/or modify
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
;; Initialize EDE for all supported conditions.

;;; Code:
;;
;; We need to load the base EDE tools for XEmacs which sorts
;; the files differently in the autoloads file.
;;
;; This isn't that big a deal since the act of reading in the autoloads
;; happens to have the effect of loading EDE anyway, so this saves us
;; the trouble.
(require 'ede)

;;; EDE autoloads
;;
(load "ede-loaddefs" nil t)

(provide 'ede-load)

;;; ede-load.el ends here
