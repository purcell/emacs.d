;;; ede-proj-archive.el --- EDE Generic Project archive support

;;;  Copyright (C) 1998, 1999, 2000, 2001  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-archive.el,v 1.9 2005/09/30 20:16:37 zappo Exp $

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
;; Handle object code archives in and EDE Project file.

(require 'ede-pmake)
(require 'ede-proj-obj)

;;; Code:
(defclass ede-proj-target-makefile-archive
  (ede-proj-target-makefile-objectcode)
  ()
  "This target generates an object code archive.")

(defvar ede-gcc-archive-compiler
  (clone ede-gcc-compiler
	 "ede-c-archive-compiler"
	 :name "ar"
	 :commands '("$(AR) $@ $^")
	 :autoconf '(("AC_CHECK_PROGS" . "RANLIB, ranlib"))
	 )
  "Create an archive of C code.")

(defmethod ede-proj-makefile-insert-source-variables :BEFORE
  ((this ede-proj-target-makefile-archive))
  "Insert bin_PROGRAMS variables needed by target THIS.
We aren't acutally inserting SOURCE details, but this is used by the
Makefile.am generator, so use it to add this important bin program."
  (ede-pmake-insert-variable-shared
      (concat "lib" (ede-name this) "_a_LIBRARIES")
    (insert (concat "lib" (ede-name this) ".a"))))

(defmethod ede-proj-makefile-insert-rules
  ((this ede-proj-target-makefile-archive))
  "Create the make rule needed to create an archive for THIS."
  (call-next-method)
  (insert "# Sorry, rule for making archive " (ede-name this)
	  "has not yet been implemented.\n\n")
  )


(provide 'ede-proj-archive)

;;; ede-proj-archive.el ends here
