;;; cit-load.el --- 

;; Copyright (C) 2008 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cit-load.el,v 1.3 2008/03/11 02:35:54 zappo Exp $

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
;; Loading cit stuff.

;;; Code:

(defvar cit-src-dir
  (let ((dir (file-name-directory
	      (or load-file-name (buffer-file-name)))))
    (add-to-list 'load-path dir)
    dir)
  "Src dir to CIT testing suite.")

(setq inhibit-splash-screen t)

(require 'cedet-integ-test)

(provide 'cit-load)
;;; cit-load.el ends here
