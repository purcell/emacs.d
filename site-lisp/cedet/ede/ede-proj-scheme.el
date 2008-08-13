;;; ede-proj-scheme.el --- EDE Generic Project scheme (guile) support

;;;  Copyright (C) 1998, 1999, 2000  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make, scheme
;; RCS: $Id: ede-proj-scheme.el,v 1.8 2005/09/30 20:17:08 zappo Exp $

;; This software is free software; you can redistribute it and/or modify
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
;; Handle scheme (Guile) in and EDE Project file.
;; This is a specialized do nothing class.

(require 'ede-proj)
(require 'autoconf-edit)

;;; Code:
(defclass ede-proj-target-scheme (ede-proj-target)
  ((menu :initform nil)
   (keybindings :initform nil)
   (interpreter :initarg :interpreter
		:initform "guile"
		:type string
		:custom string
		:documentation "The preferred interpreter for this code.")
   )
  "This target consists of scheme files.")

(defmethod ede-proj-tweak-autoconf ((this ede-proj-target-scheme))
  "Tweak the configure file (current buffer) to accomodate THIS."
  (autoconf-insert-new-macro "AM_INIT_GUILE_MODULE"))

(provide 'ede-proj-scheme)

;;; ede-proj-scheme.el ends here
