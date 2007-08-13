;;; ede-proj-shared.el --- EDE Generic Project shared library support

;;;  Copyright (C) 1998, 1999, 2000  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-shared.el,v 1.9 2005/09/30 20:17:12 zappo Exp $

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
;; Handle shared object libraries in and EDE Project file.
;; Tries to deal with libtool and non-libtool situations.

(require 'ede-pmake)
(require 'ede-proj-prog)

;;; THIS NEEDS WORK.  SEE ede-proj-obj.

;;; Code:
(defclass ede-proj-target-makefile-shared-object
  (ede-proj-target-makefile-program)
  ((availablecompilers :initform (ede-gcc-shared-compiler
				  ede-gcc-libtool-shared-compiler))
   (ldflags :custom (repeat (string :tag "Libtool flag"))
	    :documentation
	    "Additional flags to add when linking this shared library.
Use ldlibs to add addition libraries.")
   )
  "This target generates a shared library.")

(defvar ede-gcc-shared-compiler
  (clone ede-gcc-compiler
	 "ede-c-shared-compiler"
	 :name "gcc -shared"
	 :variables '(("CC_SHARED" . "gcc")
		      ("C_SHARED_COMPILE" .
		       "$(CC_SHARED) -shared $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)"))
;	 :linkvariables '(("C_SHARED_LINK" .
;			   "$(CC_SHARED) -shared $(CFLAGS) $(LDFLAGS) -L. -o $@ $^")
;			  )
;	 :commands '("$(C_SHARED_LINK) %s")
	 :autoconf '("AM_PROG_LIBTOOL")
	 )
  "Compiler for C sourcecode.")

(defvar ede-gcc-libtool-shared-compiler
  (clone ede-gcc-shared-compiler
	 "ede-c-shared-compiler-libtool"
	 :name "libtool"
	 :variables '(("LIBTOOL" . "$(SHELL) libtool")
		      ("LTCOMPILE" . "$(LIBTOOL) --mode=compile $(CC) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)")
		      ("LTLINK" . "$(LIBTOOL) --mode=link $(CC) $(CFLAGS) $(LDFLAGS) -L. -o $@")
		      )
	 :commands '("$(LTLINK) $^"
		     )
	 :autoconf '("AM_PROG_LIBTOOL")
	 )
  "Compiler for C sourcecode.")

(when nil
  		 
	       
  (insert;; These C to O rules create dependencies
   "%.o: %.c\n"
   "\t@echo '$(COMPILE) -c $<'; \\\n"
   "\t$(COMPILE)"
   (if (oref this automatic-dependencies)
       " -Wp,-MD,.deps/$(*F).P"
     "")
   " -c $<\n\n")
  (if have-libtool
      (insert;; These C to shared o rules create pic code.
       "%.lo: %.c\n"
       "\t@echo '$(LTCOMPILE) -c $<'; \\\n"
       "\t$(LTCOMPILE) -Wp,-MD,.deps/$(*F).p -c $<\n"
       "\t@-sed -e 's/^\([^:]*\)\.o:/\1.lo \1.o:/' \\\n"
       "\t      < .deps/$(*F).p > .deps/$(*F).P\n"
       "\t@-rm -f .deps/$(*F).p\n\n"))
  )

(defmethod ede-proj-configure-add-missing
  ((this ede-proj-target-makefile-shared-object))
  "Query if any files needed by THIS provided by automake are missing.
Results in --add-missing being passed to automake."
  (not (and (ede-expand-filename (ede-toplevel) "ltconfig")
	    (ede-expand-filename (ede-toplevel) "ltmain.sh"))))

(defmethod ede-proj-makefile-insert-automake-pre-variables
  ((this ede-proj-target-makefile-shared-object))
  "Insert bin_PROGRAMS variables needed by target THIS.
We aren't acutally inserting SOURCE details, but this is used by the
Makefile.am generator, so use it to add this important bin program."
  (ede-pmake-insert-variable-shared "lib_LTLIBRARIES"
     (insert (concat "lib" (ede-name this) ".la"))))

(defmethod ede-proj-makefile-insert-automake-post-variables
  ((this ede-proj-target-makefile-shared-object))
  "Insert bin_PROGRAMS variables needed by target THIS.
We need to override -program which has an LDADD element."
  nil)

(defmethod ede-proj-makefile-target-name ((this ede-proj-target-makefile-shared-object))
  "Return the name of the main target for THIS target."
  ;; We need some platform gunk to make the .so change to .sl, or .a,
  ;; depending on the platform we are going to compile against.
  (concat "lib" (ede-name this) ".so"))

(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-makefile-shared-object))
  "Return the variable name for THIS's sources."
  (if (eq (oref (ede-target-parent this) makefile-type) 'Makefile.am)
      (concat "lib" (oref this name) "_la_SOURCES")
    (call-next-method)))
  

(provide 'ede-proj-shared)

;;; ede-proj-shared.el ends here
