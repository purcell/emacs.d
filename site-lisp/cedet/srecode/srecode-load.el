;;; srecode-load.el --- Autoload definitions for SRecode

;; Copyright (C) 2007, 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: srecode-load.el,v 1.3 2008/03/02 02:17:53 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Initialize SRecode for supported conditions.

;;; Code:

(require 'srecode)

;; This may be desirous, but don't enable by default.
;;(global-srecode-minor-mode 1)

(provide 'srecode-load)

;;; srecode-load.el ends here

