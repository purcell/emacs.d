;;; cedet-load.el --- Load definitions for CEDET's common libraries

;;; Copyright (C) 2008 Eric M. Ludlam
;;; Copyright (C) 2003 David Ponce

;; Author: David Ponce <david@dponce.com>
;; X-RCS: $Id: cedet-load.el,v 1.3 2008/03/14 22:38:15 zappo Exp $

;; CEDET is free software; you can redistribute it and/or modify
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
;; Initialize CEDET's common libraries for all supported conditions.

;;; Code:
;;

;;; Common autoloads
;;
(load "cedet-loaddefs" nil t)
(require 'cedet-compat)

(provide 'cedet-load)

;;; cedet-load.el ends here
