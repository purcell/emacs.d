;;; ede-proj-obj.el --- EDE Generic Project Object code generation support

;;;  Copyright (C) 1998, 1999, 2000, 2005  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-obj.el,v 1.12 2005/09/30 20:17:03 zappo Exp $

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
;; Handles a supperclass of target types which create object code in
;; and EDE Project file.

(require 'ede-proj)
;; (require 'ede-pmake)
;; The above require is needed for ede-pmake-varname, but introduces
;; a circular dependency.  Leave it be.

(defvar ede-proj-objectcode-dodependencies nil
  "Flag specifies to do automatic dependencies.")

;;; Code:
(defclass ede-proj-target-makefile-objectcode (ede-proj-target-makefile)
  (;; Give this a new default
   (configuration-variables :initform ("debug" . (("CFLAGS" . "-g")
						  ("LDFLAGS" . "-g"))))
   (availablecompilers :initform (ede-gcc-compiler
				  ede-g++-compiler
				  ;; More C and C++ compilers, plus
				  ;; fortran or pascal can be added here
				  ))
   (availablelinkers :initform (ede-g++-linker
				;; Add more linker thingies here.
				ede-ld-linker
				))
   (sourcetype :initform (ede-source-c 
			  ede-source-c++
			  ;; ede-source-other
			  ;; This object should take everything that
			  ;; gets compiled into objects like fortran
			  ;; and pascal.
			  ))
   )
  "Abstract class for Makefile based object code generating targets.
Belonging to this group assumes you could make a .o from an element source
file.")

(defclass ede-object-compiler (ede-compiler)
  ((uselinker :initform t)
   (dependencyvar :initarg :dependencyvar
		  :type list
		  :custom (cons (string :tag "Variable")
				(string :tag "Value"))
		  :documentation
		  "A variable dedicated to dependency generation."))
  "Ede compiler class for source which must compiler, and link.")

(defvar ede-source-c
  (ede-sourcecode "ede-source-c"
		  :name "C"
		  :sourcepattern "\\.c$"
		  :auxsourcepattern "\\.h$"
		  :garbagepattern '("*.o" "*.obj" ".deps/*.P" ".lo"))
  "C source code definition.")

(defvar ede-gcc-compiler
  (ede-object-compiler
   "ede-c-compiler-gcc"
   :name "gcc"
   :dependencyvar '("C_DEPENDENCIES" . "-Wp,-MD,.deps/$(*F).P")
   :variables '(("CC" . "gcc")
		("C_COMPILE" .
		 "$(CC) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)"))
   :rules (list (ede-makefile-rule
		 "c-inference-rule"
		 :target "%.o"
		 :dependencies "%.c"
		 :rules '("@echo '$(C_COMPILE) -c $<'; \\"
			  "$(C_COMPILE) $(C_DEPENDENCIES) -o $@ -c $<"
			  )
		 ))
   :autoconf '("AC_PROG_CC" "AC_PROG_GCC_TRADITIONAL")
   :sourcetype '(ede-source-c)
   :objectextention ".o"
   :makedepends t
   :uselinker t)
  "Compiler for C sourcecode.")

(defvar ede-source-c++
  (ede-sourcecode "ede-source-c++"
		  :name "C++"
		  :sourcepattern "\\.cpp$"
		  :auxsourcepattern "\\.hpp$"
		  :garbagepattern '("*.o" "*.obj" ".deps/*.P" ".lo"))
  "C++ source code definition.")

(defvar ede-g++-compiler
  (ede-object-compiler
   "ede-c-compiler-g++"
   :name "g++"
   :dependencyvar '("CXX_DEPENDENCIES" . "-Wp,-MD,.deps/$(*F).P")
   :variables '(("CXX" "g++")
		("CXX_COMPILE" .
		 "$(CXX) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)")
		)
   :rules (list (ede-makefile-rule
		 "c++-inference-rule"
		 :target "%.o"
		 :dependencies "%.cpp"
		 :rules '("@echo '$(CXX_COMPILE) -c $<'; \\"
			  "$(CXX_COMPILE) $(CXX_DEPENDENCIES) -o $@ -c $<"
			  )
		 ))
   :autoconf '("AC_PROG_CXX")
   :sourcetype '(ede-source-c++)
   :objectextention ".o"
   :makedepends t
   :uselinker t)
  "Compiler for C sourcecode.")

(defvar ede-g++-linker
  (ede-linker
   "ede-g++-linker"
   :name "g++"
   ;; Only use this linker when c++ exists.
   :sourcetype '(ede-source-c++)
   :variables  '(("CXX_LINK" .
		  "$(CXX) $(CFLAGS) $(LDFLAGS) -L. -o $@")
		 )
   :commands '("$(CXX_LINK) $^")
   :autoconf '("AC_PROG_CXX")
   :objectextention "")
  "Linker needed for c++ programs.")

(defvar ede-ld-linker
  (ede-linker
   "ede-ld-linker"
   :name "ld"
   :variables  '(("LD" . "ld")
		 ("LD_LINK" .
		  "$(LD) $(LDFLAGS) -L. -o $@")
		 )
   :commands '("$(LD_LINK) $^")
   :objectextention "")
  "Linker needed for c++ programs.")

;;; The EDE object compiler
;;
(defmethod ede-proj-makefile-insert-variables ((this ede-object-compiler))
  "Insert variables needed by the compiler THIS."
  (call-next-method)
  (if (slot-boundp this 'dependencyvar)
      (with-slots (dependencyvar) this
	  (insert (car dependencyvar) "=")
	  (let ((cd (cdr dependencyvar)))
	    (if (listp cd)
		(mapc (lambda (c) (insert " " c)) cd)
	      (insert cd))
	    (insert "\n")))))

;;; EDE Object target type methods
;;
(defmethod ede-proj-makefile-sourcevar
  ((this ede-proj-target-makefile-objectcode))
  "Return the variable name for THIS's sources."
  (concat (ede-pmake-varname this) "_SOURCES"))

(defmethod ede-proj-makefile-dependency-files
  ((this ede-proj-target-makefile-objectcode))
  "Return a list of source files to convert to dependencies.
Argument THIS is the target to get sources from."
  (append (oref this source) (oref this auxsource)))

(defmethod ede-proj-makefile-insert-variables ((this ede-proj-target-makefile-objectcode)
					       &optional moresource)
  "Insert variables needed by target THIS.
Optional argument MORESOURCE is not used."
  (let ((ede-proj-objectcode-dodependencies
	 (oref (ede-target-parent this) automatic-dependencies)))
    (call-next-method)))

(defmethod ede-buffer-header-file((this ede-proj-target-makefile-objectcode)
				  buffer)
  "There are no default header files."
  (or (call-next-method)
      ;; Ok, nothing obvious. Try looking in ourselves.
      (let ((h (oref this auxsource)))
	;; Add more logic here when the problem is better understood.
	(car-safe h))))

(provide 'ede-proj-obj)

;;; ede-proj-obj.el ends here
