;;; ede-proj-prog.el --- EDE Generic Project program support

;;;  Copyright (C) 1998, 1999, 2000, 2001, 2005, 2008  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-prog.el,v 1.10 2008/12/28 22:14:04 zappo Exp $

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
;; Handle building programs from object files in and EDE Project file.

(require 'ede-pmake)
(require 'ede-proj-obj)

;;; Code:
(defclass ede-proj-target-makefile-program
  (ede-proj-target-makefile-objectcode)
  ((ldlibs :initarg :ldlibs
	   :initform nil
	   :type list
	   :custom (repeat (string :tag "Library"))
	   :documentation
	   "Libraries, such as \"m\" or \"Xt\" which this program depends on.
The linker flag \"-l\" is automatically prepended.  Do not include a \"lib\"
prefix, or a \".so\" suffix.

Note: Currently only used for Automake projects."
	   )
   (ldflags :initarg :ldflags
	    :initform nil
	    :type list
	    :custom (repeat (string :tag "Link Flag"))
	    :documentation
	    "Additional flags to add when linking this target.
Use ldlibs to add addition libraries.  Use this to specify specific
options to the linker.

Note: Not currently used.  This bug needs to be fixed.")
   )
   "This target is an executable program.")

(defmethod ede-proj-makefile-insert-automake-pre-variables
  ((this ede-proj-target-makefile-program))
  "Insert bin_PROGRAMS variables needed by target THIS."
  (ede-pmake-insert-variable-shared "bin_PROGRAMS"
    (insert (ede-name this)))
  (call-next-method))

(defmethod ede-proj-makefile-insert-automake-post-variables
  ((this ede-proj-target-makefile-program))
  "Insert bin_PROGRAMS variables needed by target THIS."
  (ede-pmake-insert-variable-shared
      (concat (ede-name this) "_LDADD")
    (mapcar (lambda (c) (insert " -l" c)) (oref this ldlibs)))
  ;; For other targets THIS depends on
  ;;
  ;; NOTE: FIX THIS
  ;; 
  ;;(ede-pmake-insert-variable-shared
  ;;    (concat (ede-name this) "_DEPENDENCIES")
  ;;  (mapcar (lambda (d) (insert d)) (oref this FOOOOOOOO)))
  (call-next-method))

(defmethod ede-proj-makefile-insert-rules ((this ede-proj-target-makefile-program))
  "Insert rules needed by THIS target."
  (let ((ede-proj-compiler-object-linkflags
	 (mapconcat 'identity (oref this ldflags) " ")))
    (with-slots (ldlibs) this
      (if ldlibs
	  (setq ede-proj-compiler-object-linkflags
		(concat ede-proj-compiler-object-linkflags
			" -l"
			(mapconcat 'identity ldlibs " -l")))))
    (call-next-method)))

(defmethod project-debug-target ((obj ede-proj-target-makefile-program))
  "Debug a program target OBJ."
  (let ((tb (get-buffer-create " *padt*"))
	(dd (if (not (string= (oref obj path) ""))
		(oref obj path)
	      default-directory))
	(cmd nil))
    (unwind-protect
	(progn
	  (set-buffer tb)
	  (setq default-directory dd)
	  (setq cmd (read-from-minibuffer
		     "Run (like this): "
		     (concat (symbol-name ede-debug-program-function)
			     " " (ede-target-name obj))))
	  (funcall ede-debug-program-function cmd))
      (kill-buffer tb))))


(provide 'ede-proj-prog)

;;; ede-proj-prog.el ends here
