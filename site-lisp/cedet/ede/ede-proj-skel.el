;;; ede-proj-%NAME%.el --- EDE Generic Project ...

;;;  Copyright (C) 1999, 2000, 2001, 2008  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj-skel.el,v 1.13 2008/06/28 14:36:13 zappo Exp $

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
;; Handles ...
;; in an EDE Project file.

;; To use this skeleton file, replace all occurances of words in %PERCENT%
;; with the proper named you wish to use.
;;
;; If a function is commented out, then you probably don't need to
;; override it.  If it is not commented out, you probably need it, but
;; there is no requirement that you implement it.

(require 'ede-proj)

;;; Code:
;; This defines the class you will use.  In general, you probably
;; want to inherit from `ede-proj-target-makefile', which has built it
;; support for the creation of Makefile rules.  If your type will
;; never use a Makefile, inherit from `ede-proj-target'.
(defclass ede-proj-target-%NAME% (ede-proj-target-%PARENT%)
  (;; Use these two items to modify the target specificy menu.
   ;;(menu :initform nil)
   ;;(keybindings :initform nil)
   (sourcetype :initform (ede-source-%NAME%))   
   ;;(availablecompilers :initform (ede-%NAME%-compiler))
   ;; Add your specialized fields here
   )
  "Class for ....")

;; Register you class with the ede target manager.  This enables
;; the creation of targets of your type for users.
(ede-proj-register-target "%NAME%" ede-proj-target-%NAME%)

;;; COMPILER SUPPORT
;;
;; Support for specialized compilers for your object.

;; Create an instance of a source code type which knows how to
;; recognize your sourcecode.  The symbol created is used to cross
;; reference targets compilers, and source files.
(defvar ede-source-%NAME%
  (ede-sourcecode "ede-%NAME%-source"
		  :name "%LONGNAME%"
		  :sourcepattern "\\.%EXT%$"
		  :garbagepattern '("*.%EXT%"))
  "Emacs Lisp source code definition.")

;; It should be very rare to need to create a subclass of the `ede-compiler'
;; object.  Do this if you need to override any of the base methods following
;; this definition.
;;(defclass ede-compiler-%NAME% (ede-compiler)
;;  ()
;;  "Specialized compiler for %NAME%")

;; This variable is important to set if you are planning on supporting 
;; a compiled form of your sources via a Makefile or Makefile.am
;; If you do not create or use a compiler, remove the compiler init line
;; from the class you create.
;; You may create as many compiler elements as may be available for the
;; type of source code you are starting with.
;;
;; If you have your own compiler class, then use the correct class initializer
;; below.
(defvar ede-%NAME%-compiler
  (ede-compiler 
   "ede-%NAME%-compiler"
   :name "%NAME%"
   :variables '(("%NAME-COMPILER%" . "%compiler-and-flags%"))
   :commands
   (list "$(%NAME-COMPILER%) -o $@ $^"
	 )
   :autoconf '("AC_%COMPILER-MACRO-FOR-AUTONAME-OR-nil%"))
  "Compiler that can be used with `ede-proj-target-%NAME%'.")

;;(defmethod ede-proj-tweak-autoconf ((this ede-compiler-%NAME%))
;;  "Tweak the configure file (current buffer) to accomodate THIS."
;;  (%do-stuff%)
;;  (call-next-method)
;;  (%do-other-stuff%))
;;
;;(defmethod ede-proj-flush-autoconf ((this ede-compiler-%NAME%))
;;  "Flush the configure file (current buffer) to accomodate THIS."
;;  (%do-stuff%)
;;  (call-next-method)
;;  (%do-other-stuff%))
;;
;;(defmethod ede-proj-makefile-insert-variables ((this ede-compiler-%NAME%))
;;  "Insert variables needed by the compiler THIS."
;;  (call-next-method)
;;  (ede-compiler-only-once
;;     (%do-stuff%)))
;;
;;(defmethod ede-proj-makefile-insert-rules ((this ede-compiler-%NAME%))
;;  "Insert rules needed for THIS compiler object."
;;  (call-next-method)
;;  (ede-compiler-only-once
;;     (%do-stuff%)))
;;
;; This is the method you are most likely to need to override if
;; you need to create your own compiler class.
;;(defmethod ede-proj-makefile-insert-commands ((this ede-compiler-%NAME%))
;;  "Insert the commands needed to use compiler THIS."
;;  (%do-stuff%)
;;  (call-next-method)
;;  (%do-other-stuff%))

;;; EIEIO maintenance methods
;;

;; The chances of needing to implement this are near zero, but if
;; you need to perform some action when the user hits "apply", this
;; is the way to do it.
;;(defmethod eieio-done-customizing ((target ede-proj-target-%NAME%))
;;  "Called when a user finishes customizing this target."
;;  (call-next-method)
;;  (%do-my-stuff%))

;;; EDE target smarts methods
;;

;; NOTE: This method is not generally necessary because the work can be
;;       accomplished with the :sourcetype class property.
;; This function lets you define what types of files you want to claim.
;; Defining this provides a convenience for uses by not offering your
;; target type for files you don't care about.
;; (defmethod ede-want-file-p ((obj ede-proj-target-%NAME%) file)
;;  "Return t if OBJ wants to own FILE."
;;  (string-match "\\.%MYEXTENSION%$" file))

;; When a buffer is read in, EDE checks to see which target this
;; buffer belongs to.  The default method for all targets checks
;; the file name against the list of known source files.
;; If your target has additional source files stored elsewhere,
;; this is how you will check.
;;(defmethod ede-buffer-mine ((this ede-proj-target-%NAME%) buffer)
;;  "Return non-nil if object THIS lays claim to the file in BUFFER."
;;  (or (call-next-method)
;;      (%additional-checks-here%)
;;      ))

;; This function lets you take files being added to your target and
;; stick them into different slots.  This is useful if you have
;; compilable sources and auxiliary files related to compilation that
;; do not directly get compiled.  (Although in that case, you probably
;; want to extend `ede-proj-target-makefile-objectcode'
;;(defmethod project-add-file ((this ede-proj-target-%NAME%) file)
;;  "Add to target THIS the current buffer represented as FILE."
;;  (if (not (%do-something-special%))
;;	(call-next-method)
;;    (%do-my-special-stuff%)))

;; Reverse tactic as above.
;;(defmethod project-remove-file  ((target ede-proj-target-makefile-objectcode)
;;				  file)
;;  "For TARGET, remove FILE."
;;  (if (not (%do-something-special%))
;;	(call-next-method)
;;    (%do-my-special-stuff%)))

;; Provides a simple hook to do cleanup work if this target is deleted
;; from a project.
;;(defmethod project-delete-target ((this ede-proj-target-%NAME%))
;;  "Delete THIS target from its parent project."
;;  (%cleanup%)
;;  (call-next-method))

;;; EDE target do user stuff methods
;;

;; This method lets you control what commands are run when a user
;; wants to compile your target.  If you inherit from a makefile
;; target, then you can use "call-next-method" with a new
;; command if needed, or just comment this out.
;;(defmethod project-compile-target ((obj ede-proj-target-%NAME%)
;;				     &optional command)
;;  "Compile the current target OBJ.
;;Argument COMMAND is the command to use for compiling the target
;;if the user, or child class wishes to modify it."
;;  (or (call-next-method %"new command string"%)
;;      (project-compile-project (ede-current-project) %"some command"%)
;;	(%do-stuff%)))

;; This method lets you define how the target reacts to a debug
;; command.  Comment this out if you do not support a debugger.
;; If you don't support debugging, don't forget to also remove
;; any specialized keybindings and menu items in the class definition.
(defmethod project-debug-target ((obj ede-proj-target-%NAME%))
  "Run the current target OBJ in an debugger."
  (%do-stuff%))

;;; EDE project makefile code generation methods
;;

;; The name of the variable used in a Makefile for your main sources.
;; Attempt to use automake conventions so that your target is easy to
;; port when/if automake is supported by ede-proj.
(defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-%NAME%))
  "Return the variable name for THIS's sources."
  (concat (ede-pmake-varname this) "_%AUTOMAKE-VARIABLE-CONVENTION%"))

;; Your main target in the Makefile may depend on additional source
;; dependencies.  Use this to add more stuff.
;;(defmethod ede-proj-makefile-dependency-files ((this ede-proj-target-%NAME%))
;;  "Return a list of source files to convert to dependencies.
;;Argument THIS is the target to get sources from."
;;  (append (call-next-method) (%get-more-dependencies%)))

;; This is a clever way of packing more files into your main source
;; variable.  Only works if your "next" method is ede-proj-target.
;;(defmethod ede-proj-makefile-insert-source-variables
;;           ((this ede-proj-target-%NAME%) &optional moresource)
;;  "Insert variables needed by target THIS."
;;  (call-next-method this (oref this %moresourceslotname%))

;; This method lets you add more variables specific to your type of target.
;;(defmethod ede-proj-makefile-insert-variables ((this ede-proj-target-%NAME%)
;;                                                &optional moresource)
;;  "Insert variables needed by target THIS."
;;  (call-next-method)
;;  (insert "other variable thing"))

;; This returns a string for the dependencies as they 
(defmethod ede-proj-makefile-dependencies ((this ede-proj-target-%NAME%))
  "Return a string representing the dependencies for THIS."
  (or (call-next-method)
      (%do-some-stuff%)))

;; This is one of the most important methods which defines rules to
;; place into a makefile for building.  If you inherit from
;; `ede-proj-target-makefile', then this is the primary build
;; mechanism.  If you can create a compiler object instead, then
;; you probably don't have to do anything with this method.
;; If you have an emacs-centric build method, then this
;; is a secondary build method (for a distribution, for example.)
;; It can also contain auxiliary make commands in addition to
;; the main rules needed if not covered by the compiler object.
;;(defmethod ede-proj-makefile-insert-rules ((this ede-proj-target-%NAME%))
;;  "Insert rules needed by THIS target."
;;  (insert ... ;; Create aux rules here
;;        )
;;  (call-next-method) ;; catch user-rules case and build main rules
;;  (insert ... ;; Code to fill in the commands, or add more rules later.
;;	  ))

;; This function allows modifictions to configure.in.
;; Only useful when a user specifies automake mode in EDE project.
;; This may not be necessary, but is needed if special programs,
;; libraries, header files, or other configurable items are needed.
;;
;; See the autoconf editing routines, most of which are safe
;; to call multiple times.
;(defmethod ede-proj-tweak-autoconf ((this ede-proj-target-%NAME%))
;  "Tweak the configure file (current buffer) to accomodate THIS."
;  (autoconf-...)
;  )

;; This function allows the target to flush left overs.
;; ie, if a previous configuration included some macro, then
;;     you can remove it with this one.
;; A subsequent call to `ede-proj-tweak-autoconf' will restore
;; any changes if needed.
;(defmethod ede-proj-flush-autoconf ((this ede-proj-%NAME%))
;  "Flush the configure file (current buffer) to accomodate THIS."
;  nil)

;; This function is used to find a header file in which prototypes from
;; BUFFER go.  This is used by advanced features for which this type
;; of behavior is useful.  This feature is used mainly by tools
;; using the SEMANTIC BOVINATOR http://cedet.sourceforge.net/semantic.shtml
;; to perform advanced language specific actions.
;;(defmethod ede-buffer-header-file((this ede-proj-target-%NAME%) buffer)
;;  "Return the name of a file in which prototypes go."
;;  (oref this ...))

;; This function is used to return documentation files.  If this target
;; contains documentation files, then return those files.  If this target
;; does not provide documentation, delete this method.
;;(defmethod ede-documentation ((this ede-target-%NAME%))
;;  "Return a list of files that provides documentation.
;;Documentation is not for object THIS, but is provided by THIS for other
;;files in the project."
;;  nil)

;;; EDE speedbar browsing enhancements
;;
;; In general, none of these need to be defined unless your have slots
;; for auxiliary source files.

;; This lets you add buttons of things your target contains which may
;; not be shown be default.
;;
;; You will need to tweak the functions used when clicking on the
;; expand icon (maybe) and the item name (maybe). Leave those alone
;; if they are simple source files.
;;(defmethod eieio-speedbar-child-make-tag-lines ((this ede-proj-target-%NAME%))
;;  "Create buttons for items belonging to THIS."
;;  (call-next-method) ;; get the default buttons inserted.
;;  (with-slots (%SOME-SLOTS%) this
;;    (mapcar (lambda (car)
;;		(speedbar-make-tag-line 'bracket ?+
;;					'ede-tag-file
;;					(concat (oref this :path) car)
;;					car
;;					'ede-file-find
;;					(concat (oref this :path) car)
;;					'speedbar-file-face depth))
;;	      %A-SLOT%)))

(provide 'ede-proj-%NAME%)

;;; ede-proj-%NAME%.el ends here
