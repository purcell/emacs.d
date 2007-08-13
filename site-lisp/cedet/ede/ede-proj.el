;;; ede-proj.el --- EDE Generic Project file driver

;;;  Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2007  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj.el,v 1.47 2007/02/19 13:46:45 zappo Exp $

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
;; EDE defines a method for managing a project.  EDE-PROJ aims to be a
;; generic project file format based on the EIEIO object stream
;; methods.  Changes in the project structure will require Makefile
;; rebuild.  The targets provided in ede-proj can be augmented with
;; additional target types inherited directly from `ede-proj-target'.

(eval-and-compile '(require 'ede))
(require 'ede-proj-comp)

;;; Class Definitions:
(defclass ede-proj-target (ede-target)
  ((auxsource :initarg :auxsource
	      :initform nil
	      :type list
	      :custom (repeat (string :tag "File"))
	      :label "Auxiliary Source Files"
	      :group (default source)
	      :documentation "Auxilliary source files included in this target.
Each of these is considered equivalent to a source file, but it is not
distributed, and each should have a corresponding rule to build it.")
   (dirty :initform nil
	  :type boolean
	  :documentation "Non-nil when generated files needs updating.")
   (compiler :initarg :compiler
	     :initform nil
	     :type (or null symbol)
	     :custom (choice (const :tag "None" nil)
			     :slotofchoices availablecompilers)
	     :label "Compiler for building sources"
	     :group make
	     :documentation
	     "The compiler to be used to compile this object.
This should be a symbol, which contains the object defining the compiler.
This enables save/restore to do so by name, permitting the sharing
of these compiler resources, and global customization thereof.")
   (linker :initarg :linker
	     :initform nil
	     :type (or null symbol)
	     :custom (choice (const :tag "None" nil)
			     :slotofchoices availablelinkers)
	     :label "Linker for combining intermediate object files."
	     :group make
	     :documentation
	     "The linker to be used to link compled sources for this object.
This should be a symbol, which contains the object defining the linker.
This enables save/restore to do so by name, permitting the sharing
of these linker resources, and global customization thereof.")
   ;; Class allocated slots
   (phony :allocation :class
	  :initform nil
	  :type boolean
	  :documentation
	  "A phony target is one where the build target does not relate to a file.
Such targets are always built, but make knows how to deal with them..")
   (availablecompilers :allocation :class
		       :initform nil
		       :type (or null list)
		       :documentation
		       "A list of `ede-compiler' objects.
These are the compilers the user can choose from when setting the
`compiler' slot.")
   (availablelinkers :allocation :class
		     :initform nil
		     :type (or null list)
		     :documentation
		     "A list of `ede-linker' objects.
These are the linkers the user can choose from when setting the
`linker' slot.")
   )
  "Abstract class for ede-proj targets.")

(defclass ede-proj-target-makefile (ede-proj-target)
  ((makefile :initarg :makefile
	     :initform "Makefile"
	     :type string
	     :custom string
	     :label "Parent Makefile"
	     :group make
	     :documentation "File name of generated Makefile.")
   (partofall :initarg :partofall
	      :initform t
	      :type boolean
	      :custom boolean
	      :label "Part of `all:' target"
	      :group make
	      :documentation
	      "Non nil means the rule created is part of the all target.
Setting this to nil creates the rule to build this item, but does not
include it in the ALL`all:' rule.")
   (configuration-variables
    :initarg :configuration-variables
    :initform nil
    :type list
    :custom (repeat (cons (string :tag "Configuration")
			  (repeat
			   (cons (string :tag "Name")
				 (string :tag "Value")))))
    :label "Environment Variables for configurations"
    :group make
    :documentation "Makefile variables appended to use in different configurations.
These variables are used in the makefile when a configuration becomes active.
Target variables are always renamed such as foo_CFLAGS, then included into
commands where the variable would usually appear.")
   (rules :initarg :rules
	  :initform nil
	  :type list
	  :custom (repeat (object :objecttype ede-makefile-rule))
	  :label "Additional Rules"
	  :group (make)
	  :documentation
	  "Arbitrary rules and dependencies needed to make this target.
It is safe to leave this blank.")
   )
  "Abstract class for Makefile based targets.")

(autoload 'ede-proj-target-aux "ede-proj-aux"
  "Target class for a group of lisp files." nil nil)
(autoload 'ede-proj-target-elisp "ede-proj-elisp"
  "Target class for a group of lisp files." nil nil)
(autoload 'ede-proj-target-elisp-autoloads "ede-proj-elisp"
  "Target class for generating autoload files." nil nil)
(autoload 'ede-proj-target-scheme "ede-proj-scheme"
  "Target class for a group of lisp files." nil nil)
(autoload 'ede-proj-target-makefile-miscelaneous "ede-proj-misc"
  "Target class for a group of miscelaneous w/ a special makefile." nil nil)
(autoload 'ede-proj-target-makefile-program "ede-proj-prog"
  "Target class for building a program." nil nil)
(autoload 'ede-proj-target-makefile-archive "ede-proj-archive"
  "Target class for building an archive of object code." nil nil)
(autoload 'ede-proj-target-makefile-shared-object "ede-proj-shared"
  "Target class for building a shared object." nil nil)
(autoload 'ede-proj-target-makefile-info "ede-proj-info"
  "Target class for info files." nil nil)

(defvar ede-proj-target-alist
  '(("program" . ede-proj-target-makefile-program)
    ("archive" . ede-proj-target-makefile-archive)
    ("sharedobject" . ede-proj-target-makefile-shared-object)
    ("emacs lisp" . ede-proj-target-elisp)
    ("emacs lisp autoloads" . ede-proj-target-elisp-autoloads)
    ("info" . ede-proj-target-makefile-info)
    ("auxiliary" . ede-proj-target-aux)
    ("scheme" . ede-proj-target-scheme)
    ("miscelaneous" . ede-proj-target-makefile-miscelaneous)
    )
  "Alist of names to class types for available project target classes.")

(defun ede-proj-register-target (name class)
  "Register a new target class with NAME and class symbol CLASS.
This enables the creation of your target type."
  (let ((a (assoc name ede-proj-target-alist)))
    (if a
	(setcdr a class)
      (setq ede-proj-target-alist
	    (cons (cons name class) ede-proj-target-alist)))))

(defclass ede-proj-project (ede-project)
  ((makefile-type :initarg :makefile-type
		  :initform Makefile
		  :type symbol
		  :custom (choice (const Makefile)
				  ;(const Makefile.in)
				  (const Makefile.am)
				  ;(const cook)
				  )
		  :documentation "The type of Makefile to generate.
Can be one of 'Makefile, 'Makefile.in, or 'Makefile.am.
If this value is NOT 'Makefile, then that overrides the :makefile slot
in targets.")
   (variables :initarg :variables
	      :initform nil
	      :type list
	      :custom (repeat (cons (string :tag "Name")
				    (string :tag "Value")))
	      :documentation "Variables to set in this Makefile.")
   (configuration-variables
    :initarg :configuration-variables
    :initform ("debug" (("DEBUG" . "1")))
    :type list
    :custom (repeat (cons (string :tag "Configuration")
			  (repeat
			   (cons (string :tag "Name")
				 (string :tag "Value")))))
    :documentation "Makefile variables to use in different configurations.
These variables are used in the makefile when a configuration becomes active.")
   (inference-rules :initarg :inference-rules
		    :initform nil
		    :custom (repeat
			     (object :objecttype ede-makefile-rule))
		    :documentation "Inference rules to add to the makefile.")
   (include-file :initarg :include-file
		 :initform nil
		 :custom (repeat
			  (string :tag "Include File"))
		 :documentation "Additional files to include.
These files can contain additional rules, variables, and customizations.")
   (automatic-dependencies
    :initarg :automatic-dependencies
    :initform t
    :type boolean
    :custom boolean
    :group (default settings)
    :documentation
    "Non-nil to do implement automatic dependencies in the Makefile.")
   (menu :initform
	 (
	  [ "Regenerate Makefiles" ede-proj-regenerate t ]
	  [ "Upload Distribution" ede-upload-distribution t ]
	  )
	 )
   (metasubproject
    :initarg :metasubproject
    :initform nil
    :type boolean
    :custom boolean
    :group (default settings)
    :documentation
    "Non-nil if this is a metasubproject.
Usually, a subproject is determined by a parent project.  If multiple top level
projects are grouped into a large project not maintained by EDE, then you need
to set this to non-nil.  The only effect is that the `dist' rule will then avoid
making a tar file.")
   )
  "The EDE-PROJ project definition class.")

;;; Code:
(defun ede-proj-load (project)
  "Load a project file PROJECT."
  (save-excursion
    (let ((ret nil)
	  (subdirs (directory-files project nil "[^.].*" nil)))
      (set-buffer (get-buffer-create " *tmp proj read*"))
      (unwind-protect
	  (progn
	    (insert-file-contents (concat project "Project.ede")
				  nil nil nil t)
	    (goto-char (point-min))
	    (setq ret (read (current-buffer)))
	    (if (not (eq (car ret) 'ede-proj-project))
		(error "Corrupt project file"))
	    (setq ret (eval ret))
	    (oset ret file (concat project "Project.ede")))
	(kill-buffer " *tmp proj read*"))
      (while subdirs
	(let ((sd (concat project (car subdirs))))
	  (if (and (file-directory-p sd)
		   (ede-directory-project-p (concat sd "/")))
	      (oset ret subproj (cons (ede-proj-load (concat sd "/"))
				      (oref ret subproj))))
	  (setq subdirs (cdr subdirs))))
      ret)))

(defun ede-proj-save (&optional project)
  "Write out object PROJECT into its file."
  (save-excursion
    (if (not project) (setq project (ede-current-project)))
    (let ((b (set-buffer (get-buffer-create " *tmp proj write*")))
	  (cfn (oref project file)))
      (unwind-protect
	  (save-excursion
	    (erase-buffer)
	    (let ((standard-output (current-buffer)))
	      (oset project file (file-name-nondirectory cfn))
	      (object-write project ";; EDE project file."))
	    (write-file cfn nil)
	    )
	;; Restore the :file on exit.
	(oset project file cfn)
	(kill-buffer b)))))

(defmethod ede-commit-local-variables ((proj ede-proj-project))
  "Commit change to local variables in PROJ."
  (ede-proj-save proj))

(defmethod eieio-done-customizing ((proj ede-proj-project))
  "Call this when a user finishes customizing this object.
Argument PROJ is the project to save."
  (call-next-method)
  (ede-proj-save proj))

(defmethod eieio-done-customizing ((target ede-proj-target))
  "Call this when a user finishes customizing this object.
Argument TARGET is the project we are completing customization on."
  (call-next-method)
  (ede-proj-save (ede-current-project)))

(defmethod ede-commit-project ((proj ede-proj-project))
  "Commit any change to PROJ to its file."
  (ede-proj-save proj))

(defmethod ede-buffer-mine ((this ede-proj-project) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (let ((f (ede-convert-path this (buffer-file-name buffer))))
    (or (string= (file-name-nondirectory (oref this file)) f)
	(string= (ede-proj-dist-makefile this) f)
	(string-match "Makefile\\(\\.\\(in\\|am\\)\\)?" f)
	(string-match "config\\(ure\\.in\\|\\.stutus\\)?" f)
	)))

(defmethod ede-buffer-mine ((this ede-proj-target) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (or (call-next-method)
      (ede-target-buffer-in-sourcelist this buffer (oref this auxsource))))


;;; EDE command functions
;;
(defvar ede-proj-target-history nil
  "History when querying for a target type.")

(defmethod project-new-target ((this ede-proj-project))
  "Create a new target in THIS based on the current buffer."
  (let* ((name (read-string "Name: " ""))
	 (type (completing-read "Type: " ede-proj-target-alist
				nil t nil '(ede-proj-target-history . 1)))
	 (ot nil)
	 (src (if (and (buffer-file-name)
		       (y-or-n-p (format "Add %s to %s? " (buffer-name) name)))
		  (buffer-file-name))))
    (setq ot (funcall (cdr (assoc type ede-proj-target-alist)) name :name name
		      :path (ede-convert-path this default-directory)
		      :source (if src
				  (list (file-name-nondirectory src))
				nil)))
    ;; If we added it, set the local buffer's object.
    (if src (progn
	      (setq ede-object ot)
	      (ede-apply-object-keymap)))
    ;; Add it to the project object
    (oset this targets (cons ot (oref this targets)))
    ;; And save
    (ede-proj-save this)))

(defmethod project-new-target-custom ((this ede-proj-project))
  "Create a new target in THIS for custom."
  (let* ((name (read-string "Name: " ""))
	 (type (completing-read "Type: " ede-proj-target-alist
				nil t nil '(ede-proj-target-history . 1))))
    (funcall (cdr (assoc type ede-proj-target-alist)) name :name name
	     :path (ede-convert-path this default-directory)
	     :source nil)))

(defmethod project-delete-target ((this ede-proj-target))
  "Delete the current target THIS from it's parent project."
  (let ((p (ede-current-project))
	(ts (oref this source)))
    ;; Loop across all sources.  If it exists in a buffer,
    ;; clear it's object.
    (while ts
      (let* ((default-directory (oref this path))
	     (b (get-file-buffer (car ts))))
	(if b
	    (save-excursion
	      (set-buffer b)
	      (if (eq ede-object this)
		  (progn
		    (setq ede-object nil)
		    (ede-apply-object-keymap))))))
      (setq ts (cdr ts)))
    ;; Remove THIS from it's parent.
    ;; The two vectors should be pointer equivalent.
    (oset p targets (delq this (oref p targets)))
    (ede-proj-save (ede-current-project))))

(defmethod project-add-file ((this ede-proj-target) file)
  "Add to target THIS the current buffer represented as FILE."
  (let ((file (ede-convert-path this file))
	(src (ede-target-sourcecode this))
	(aux nil))
    (while (and src (not (ede-want-file-p (car src) file)))
      (setq src (cdr src)))
    (when src
      (setq src (car src))
      (cond ((ede-want-file-source-p this file)
	     (object-add-to-list this 'source file t))
	    ((ede-want-file-auxiliary-p this file)
	     (object-add-to-list this 'auxsource file t))
	    (t (error "`project-add-file(ede-target)' source mismatch error")))
      (ede-proj-save))))

(defmethod project-remove-file ((target ede-proj-target) file)
  "For TARGET, remove FILE.
FILE must be massaged by `ede-convert-path'."
  ;; Speedy delete should be safe.
  (object-remove-from-list target 'source (ede-convert-path target file))
  (object-remove-from-list target 'auxsource (ede-convert-path target file))
  (ede-proj-save))

(defmethod project-update-version ((this ede-proj-project))
  "The :version of project THIS has changed."
  (ede-proj-save))

(defmethod project-make-dist ((this ede-proj-project))
  "Build a distribution for the project based on THIS target."
  ;; I'm a lazy bum, so I'll make a makefile for doing this sort
  ;; of thing, and rely only on that small section of code.
  (let ((pm (ede-proj-dist-makefile this))
	(df (project-dist-files this)))
    (if (and (file-exists-p (car df))
	     (not (y-or-n-p "Dist file already exists.  Rebuild? ")))
	(error "Try `ede-update-version' before making a distribution"))
    (ede-proj-setup-buildenvironment this)
    (if (string= pm "Makefile.am") (setq pm "Makefile"))
    (compile (concat "make -f " pm " dist"))
    ))

(defmethod project-dist-files ((this ede-proj-project))
  "Return a list of files that constitutes a distribution of THIS project."
  (list
   ;; Note to self, keep this first for the above fn to check against.
   (concat (oref this name) "-" (oref this version) ".tar.gz")
   ))

(defmethod project-compile-project ((proj ede-proj-project) &optional command)
  "Compile the entire current project PROJ.
Argument COMMAND is the command to use when compiling."
  (let ((pm (ede-proj-dist-makefile proj))
	(default-directory (file-name-directory (oref proj file))))
    (ede-proj-setup-buildenvironment proj)
    (if (string= pm "Makefile.am") (setq pm "Makefile"))
    (compile (concat "make -f " pm " all"))))

;;; Target type specific compilations/debug
;;
(defmethod project-compile-target ((obj ede-proj-target) &optional command)
  "Compile the current target OBJ.
Argument COMMAND is the command to use for compiling the target."
  (project-compile-project (ede-current-project) command))

(defmethod project-compile-target ((obj ede-proj-target-makefile)
				   &optional command)
  "Compile the current target program OBJ.
Optional argument COMMAND is the s the alternate command to use."
  (ede-proj-setup-buildenvironment (ede-current-project))
  (compile (concat "make -f " (oref obj makefile) " "
		   (ede-proj-makefile-target-name obj))))

(defmethod project-debug-target ((obj ede-proj-target))
  "Run the current project target OBJ in a debugger."
  (error "Debug-target not supported by %s" (object-name obj)))

(defmethod ede-proj-makefile-target-name ((this ede-proj-target))
  "Return the name of the main target for THIS target."
  (ede-name this))

;;; Compiler and source code generators
;;
(defmethod ede-want-file-auxiliary-p ((this ede-target) file)
  "Return non-nil if THIS target wants FILE."
  ;; By default, all targets reference the source object, and let it decide.
  (let ((src (ede-target-sourcecode this)))
    (while (and src (not (ede-want-file-auxiliary-p (car src) file)))
      (setq src (cdr src)))
    src))

(defmethod ede-proj-compilers ((obj ede-proj-target))
  "List of compilers being used by OBJ.
If the `compiler' slot is empty, concoct one on a first match found
basis for any given type from the `availablecompilers' slot.
Otherwise, return the `compiler' slot.
Converts all symbols into the objects to be used."
  (when (slot-exists-p obj 'compiler)
    (let ((comp (oref obj compiler)))
      (if comp
	  ;; Now that we have a pre-set compilers to use, convert tye symbols
	  ;; into objects for ease of use
	  (if (listp comp)
	      (setq comp (mapcar 'symbol-value comp))
	    (setq comp (list (symbol-value comp))))
	(let ((avail (mapcar 'symbol-value (oref obj availablecompilers)))
	      (st (oref obj sourcetype))
	      (sources (oref obj source)))
	  ;; COMP is not specified, so generate a list from the available
	  ;; compilers list.
	  (while st
	    (if (ede-want-any-source-files-p (symbol-value (car st)) sources)
		(let ((c (ede-proj-find-compiler avail (car st))))
		  (if c (setq comp (cons c comp)))))
	    (setq st (cdr st)))))
      ;; Return the disovered compilers
      comp)))

(defmethod ede-proj-linkers ((obj ede-proj-target))
  "List of linkers being used by OBJ.
If the `linker' slot is empty, concoct one on a first match found
basis for any given type from the `availablelinkers' slot.
Otherwise, return the `linker' slot.
Converts all symbols into the objects to be used."
  (when (slot-exists-p obj 'linker)
    (let ((link (oref obj linker)))
      (if link
	  ;; Now that we have a pre-set linkers to use, convert type symbols
	  ;; into objects for ease of use
	  (setq link (mapcar 'symbol-value link))
	(let ((avail (mapcar 'symbol-value (oref obj availablelinkers)))
	      (st (oref obj sourcetype))
	      (sources (oref obj source)))
	  ;; LINKER is not specified, so generate a list from the available
	  ;; compilers list.
	  (while st
	    (if (ede-want-any-source-files-p (symbol-value (car st)) sources)
		(let ((c (ede-proj-find-linker avail (car st))))
		  (if c (setq link (cons c link)))))
	    (setq st (cdr st)))
	  (unless link
	    ;; No linker stands out!  Loop over our linkers and pull out
	    ;; the first that has no source type requirement.
	    (while (and avail (not (slot-boundp (car avail) 'sourcetype)))
	      (setq avail (cdr avail)))
	    (setq link (cdr avail)))))
      ;; Return the disovered linkers
      link)))
    

;;; Target type specific autogenerating gobbldegook.
;;
(eval-when-compile
  ;; This provides prevents recursive loading during a compile
  (provide 'ede-proj)
  (require 'ede-pmake "ede-pmake.el")
  (require 'ede-pconf "ede-pconf.el"))

(defun ede-proj-makefile-type (&optional proj)
  "Makefile type of the current project PROJ."
  (oref (or proj (ede-current-project)) makefile-type))

(defun ede-proj-automake-p (&optional proj)
  "Return non-nil if the current project PROJ is automake mode."
  (eq (ede-proj-makefile-type proj) 'Makefile.am))

(defun ede-proj-autoconf-p (&optional proj)
  "Return non-nil if the current project PROJ is automake mode."
  (eq (ede-proj-makefile-type proj) 'Makefile.in))

(defun ede-proj-make-p (&optional proj)
  "Return non-nil if the current project PROJ is automake mode."
  (eq (ede-proj-makefile-type proj) 'Makefile))

(defmethod ede-proj-dist-makefile ((this ede-proj-project))
  "Return the name of the Makefile with the DIST target in it for THIS."
  (cond ((eq (oref this makefile-type) 'Makefile.am)
	 (concat (file-name-directory (oref this file))
		 "Makefile.am"))
	((eq (oref this makefile-type) 'Makefile.in)
	 (concat (file-name-directory (oref this file))
		 "Makefile.in"))
	((object-assoc "Makefile" 'makefile (oref this targets))
	 (concat (file-name-directory (oref this file))
		 "Makefile"))
	(t
	 (let ((targets (oref this targets)))
	   (while (and targets
		       (not (obj-of-class-p
			     (car targets)
			     'ede-proj-target-makefile)))
	     (setq targets (cdr targets)))
	   (if targets (oref (car targets) makefile)
	     (concat (file-name-directory (oref this file))
		     "Makefile"))))))

(defun ede-proj-regenerate ()
  "Regenerate Makefiles for and edeproject project."
  (interactive)
  (ede-proj-setup-buildenvironment (ede-current-project) t))

(defmethod ede-proj-makefile-create-maybe ((this ede-proj-project) mfilename)
  "Create a Makefile for all Makefile targets in THIS if needed.
MFILENAME is the makefile to generate."
  ;; For now, pass through until dirty is implemented.
  (require 'ede-pmake)
  (if (or (not (file-exists-p mfilename))
	  (file-newer-than-file-p (oref this file) mfilename))
      (ede-proj-makefile-create this mfilename)))

(defmethod ede-proj-setup-buildenvironment ((this ede-proj-project)
					    &optional force)
  "Setup the build environment for project THIS.
Handles the Makefile, or a Makefile.am configure.in combination.
Optional argument FORCE will force items to be regenerated."
  (if (not force)
      (ede-proj-makefile-create-maybe this (ede-proj-dist-makefile this))
    (require 'ede-pmake)
    (ede-proj-makefile-create this (ede-proj-dist-makefile this)))
  (if (ede-proj-automake-p this)
      (progn
	(require 'ede-pconf)
	;; If the user wants to force this, do it some other way?
	(ede-proj-configure-synchronize this)
	;; Now run automake to fill in the blanks, autoconf, and other
	;; auto thingies so that we can just say "make" when done.
	
	))
  ;; Rebuild all subprojects
  (ede-map-subprojects
   this (lambda (sproj) (ede-proj-setup-buildenvironment sproj force)))
  )


;;; Lower level overloads
;;  
(defmethod project-rescan ((this ede-proj-project))
  "Rescan the EDE proj project THIS."
  (ede-with-projectfile this
    (goto-char (point-min))
    (let ((l (read (current-buffer)))
	  (fields (object-slots this))
	  (targets (oref this targets)))
      (setq l (cdr (cdr l))) ;; objtype and name skip
      (while fields ;  reset to defaults those that dont appear.
	(if (and (not (assoc (car fields) l))
		 (not (eq (car fields) 'file)))
	    (let ((eieio-skip-typecheck t))
	      ;; This is a hazardous thing, for some elements
	      ;; might not be bound.  Skip typechecking and duplicate
	      ;; unbound slots along the way.
	      (eieio-oset this (car fields)
			  (eieio-oref-default this (car fields)))))
	(setq fields (cdr fields)))
      (while l
	(let ((field (car l)) (val (car (cdr l))))
	  (cond ((eq field targets)
		 (let ((targets (oref this targets))
		       (newtarg nil))
		   (setq val (cdr val)) ;; skip the `list'
		   (while val
		     (let ((o (object-assoc (car (cdr (car val))) ; name
					    'name targets)))
		       (if o
			   (project-rescan o (car val))
			 (setq o (eval (car val))))
		       (setq newtarg (cons o newtarg)))
		     (setq val (cdr val)))
		   (oset this targets newtarg)))
		(t
		 (eieio-oset this field val))))
	(setq l (cdr (cdr l))))))) ;; field/value
	
(defmethod project-rescan ((this ede-proj-target) readstream)
  "Rescan target THIS from the read list READSTREAM."
  (setq readstream (cdr (cdr readstream))) ;; constructor/name
  (while readstream
    (let ((tag (car readstream))
	  (val (car (cdr readstream))))
      (eieio-oset this tag val))
    (setq readstream (cdr (cdr readstream)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("Project\\.ede" . emacs-lisp-mode))

(provide 'ede-proj)

;;; ede-proj.el ends here
