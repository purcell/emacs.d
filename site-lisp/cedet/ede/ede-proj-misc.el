;;; ede-proj-nusc.el --- EDE Generic Project Emacs Lisp support

;;;  Copyright (C) 1998, 1999, 2000, 2001, 2008  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-misc.el,v 1.8 2008/12/28 22:11:30 zappo Exp $

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
;; Handle miscelaneous compilable projects in and EDE Project file.
;; This misc target lets the user link in custom makefiles to an EDE
;; project.

(require 'ede-pmake)
(require 'ede-proj-comp)

;;; Code:
(defclass ede-proj-target-makefile-miscelaneous (ede-proj-target-makefile)
  ((sourcetype :initform (ede-misc-source))
   (availablecompilers :initform (ede-misc-compile))
   (submakefile :initarg :submakefile
		:initform ""
		:type string
		:custom string
		:documentation
		"Miscellaneous sources which have a specialized makefile.
The sub-makefile is used to build this target.")
   )
   "Miscelaneous target type.
A user-written makefile is used to build this target.
All listed sources are included in the distribution.")

(defvar ede-misc-source
  (ede-sourcecode "ede-misc-source"
		  :name "Miscelaneous"
		  :sourcepattern ".*")
  "Miscelaneous fiels definition.")

(defvar ede-misc-compile
  (ede-compiler "ede-misc-compile"
		:name "Sub Makefile"
		:commands
		'(
		  )
		:autoconf nil
		:sourcetype '(ede-misc-source)
		)
  "Compile code via a sub-makefile.")

(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-makefile-miscelaneous))
  "Return the variable name for THIS's sources."
  (concat (ede-pmake-varname this) "_MISC"))

(defmethod ede-proj-makefile-dependency-files
  ((this ede-proj-target-makefile-miscelaneous))
  "Return a list of files which THIS target depends on."
  (with-slots (submakefile) this
    (cond ((string= submakefile "")
	   nil)
	  ((not submakefile)
	   nil)
	  (t (list submakefile)))))

(defmethod ede-proj-makefile-insert-rules ((this ede-proj-target-makefile-miscelaneous))
  "Create the make rule needed to create an archive for THIS."
  ;; DO NOT call the next method.  We will never have any compilers,
  ;; or any dependencies, or stuff like this.  This rull will lets us
  ;; deal with it in a nice way.
  (insert (ede-name this) ": ")
  (with-slots (submakefile) this
    (if (string= submakefile "")
	(insert "\n\t@\n\n")
      (insert submakefile "\n" "\t$(MAKE) -f " submakefile "\n\n"))))

(provide 'ede-proj-misc)

;;; ede-proj-misc.el ends here
