;;; ede.el --- Emacs Development Environment gloss

;;;  Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede.el,v 1.129 2009/02/28 02:40:56 zappo Exp $
(defconst ede-version "1.0pre6"
  "Current version of the Emacs EDE.")

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
;; EDE is the top level Lisp interface to a project management scheme
;; for Emacs.  Emacs does many things well, including editing,
;; building, and debugging.  Folks migrating from other IDEs don't
;; seem to thing this qualifies, however, because they still have to
;; write the makefiles, and specify parameters to programs.
;;
;; This EDE mode will attempt to link these diverse programs together
;; into a comprehensive single interface, instead of a bunch of
;; different ones.

;;; Install
;;
;;  This command enables project mode on all files.
;;
;;  (global-ede-mode t)

(require 'ede-source)

;;; Code:
(defun ede-version ()
  "Display the current running version of EDE."
  (interactive) (message "EDE %s" ede-version))

(require 'custom)

(defgroup ede nil
  "Emacs Development Environment gloss."
  :group 'tools
  :group 'convenience
  )

(defcustom ede-auto-add-method 'ask
  "*Determins if a new source file shoud be automatically added to a target.
Whenever a new file is encountered in a directory controlled by a
project file, all targets are queried to see if it should be added.
If the value is 'always, then the new file is added to the first
target encountered.  If the value is 'multi-ask, then if more than one
target wants the file, the user is asked.  If only one target wants
the file, then then it is automatically added to that target.  If the
value is 'ask, then the user is always asked, unless there is no
target willing to take the file.  'never means never perform the check."
  :group 'ede
  :type '(choice (const always)
		 (const multi-ask)
		 (const ask)
		 (const never)))

(defcustom ede-debug-program-function 'gdb
  "*Default Emacs command used to debug a target."
  :group 'ede
  :type 'sexp) ; make this be a list of options some day

(require 'eieio)
(require 'eieio-speedbar)

;;; Top level classes for projects and targets
;;
;;;###autoload
(defclass ede-project-autoload ()
  ((name :initarg :name
	 :documentation "Name of this project type")
   (file :initarg :file
	 :documentation "The lisp file belonging to this class.")
   (proj-file :initarg :proj-file
	      :documentation "Name of a project file of this type.")
   (proj-root :initarg :proj-root
	      :type function
	      :documentation "A function symbol to call for the project root.
This function takes no arguments, and returns the current directories
root, if available.  Leave blank to use the EDE directory walking
routine instead.")
   (initializers :initarg :initializers
		 :initform nil
		 :documentation
		 "Initializers passed to the project object.
These are used so there can be multiple types of projects
associated with a single object class, based on the initilizeres used.")
   (load-type :initarg :load-type
	      :documentation "Fn symbol used to load this project file.")
   (class-sym :initarg :class-sym
	      :documentation "Symbol representing the project class to use.")
   (new-p :initarg :new-p
	  :initform t
	  :documentation
	  "Non-nil if this is an option when a user creates a project.")
   )
  "Class representing minimal knowledge set to run preliminary EDE functions.
When more advanced functionality is needed from a project type, that projects
type is required and the load function used.")

(defvar ede-project-class-files
  (list
   (ede-project-autoload "edeproject-makefile"
			 :name "Make" :file 'ede-proj
			 :proj-file "Project.ede"
			 :load-type 'ede-proj-load
			 :class-sym 'ede-proj-project)
   (ede-project-autoload "edeproject-automake"
			 :name "Automake" :file 'ede-proj
			 :proj-file "Project.ede"
			 :initializers '(:makefile-type Makefile.am)
			 :load-type 'ede-proj-load
			 :class-sym 'ede-proj-project)
   (ede-project-autoload "automake"
			 :name "automake" :file 'project-am
			 :proj-file "Makefile.am"
			 :load-type 'project-am-load
			 :class-sym 'project-am-makefile
			 :new-p nil)
   )
  "List of vectos defining how to determine what type of projects exist.")

;;; Generic project information manager objects
;;
;;;###autoload
(defclass ede-target (eieio-speedbar-directory-button)
  ((buttonface :initform speedbar-file-face) ;override for superclass
   (name :initarg :name
	 :type string
	 :custom string
	 :label "Name"
	 :group (default name)
	 :documentation "Name of this target.")
   ;; @todo - I think this should be "dir", and not "path".
   (path :initarg :path
	 :type string
	 ;:custom string
	 ;:label "Path to target"
	 ;:group (default name)
	 :documentation "The path to the sources of this target.
Relative to the path of the project it belongs to.")
   (source :initarg :source
	   :initform nil
	   ;; I'd prefer a list of strings.
	   :type list
	   :custom (repeat (string :tag "File"))
	   :label "Source Files"
	   :group (default source)
	   :documentation "Source files in this target.")
   (versionsource :initarg :versionsource
		  :initform nil
		  :type list
		  :custom (repeat (string :tag "File"))
		  :label "Source Files with Version String"
		  :group (source)
		  :documentation
		  "Source files with a version string in them.
These files are checked for a version string whenever the EDE version
of the master project is changed.  When strings are found, the version
previously there is updated.")
   ;; Class level slots
   ;;
;   (takes-compile-command :allocation :class
;			  :initarg :takes-compile-command
;			  :type boolean
;			  :initform nil
;			  :documentation
;     "Non-nil if this target requires a user approved command.")
   (sourcetype :allocation :class
	       :type list ;; list of symbols
	       :documentation
	       "A list of `ede-sourcecode' objects this class will handle.
This is used to match target objects with the compilers they can use, and
which files this object is interested in."
	       :accessor ede-object-sourcecode)
   (keybindings :allocation :class
		:initform (("D" . ede-debug-target))
		:documentation 
"Keybindings specialized to this type of target."
		:accessor ede-object-keybindings)
   (menu :allocation :class
	 :initform ( [ "Debug target" ede-debug-target
		       (and ede-object
			    (obj-of-class-p ede-object ede-target)) ]
		     )
	 :documentation "Menu specialized to this type of target."
	 :accessor ede-object-menu)
   )
  "A top level target to build.")

(defclass ede-project-placeholder (eieio-speedbar-directory-button)
  ((name :initarg :name
	 :initform "Untitled"
	 :type string
	 :custom string
	 :label "Name"
	 :group (default name)
	 :documentation "The name used when generating distribution files.")
   (version :initarg :version
	    :initform "1.0"
	    :type string
	    :custom string
	    :label "Version"
	    :group (default name)
	    :documentation "The version number used when distributing files.")
   (directory :type string
	      :initarg :directory
	      :documentation "Directory this project is associated with.")
   (dirinode :documentation "The inode id for :directory.")
   (file :type string
	 :initarg :file
	 :documentation "File name where this project is stored.")
   (rootproject ; :initarg - no initarg, don't save this slot!
    :initform nil
    :type (or null ede-project-placeholder-child)
    :documentation "Pointer to our root project.")
   )
  "Placeholder object for projects not loaded into memory.
Projects placeholders will be stored in a user specific location
and querying them will cause the actual project to get loaded.")

;;;###autoload
(defclass ede-project (ede-project-placeholder)
  ((subproj :initform nil
	    :type list
	    :documentation "Sub projects controlled by this project.
For Automake based projects, each directory is treated as a project.")
   (targets :initarg :targets
	    :type list
	    :custom (repeat (object :objectcreatefcn ede-new-target-custom))
	    :label "Local Targets"
	    :group (targets)
	    :documentation "List of top level targets in this project.")
   (locate-obj :type (or null ede-locate-base-child)
	       :documentation
	       "A locate object to use as a backup to `ede-expand-filename'.")
   (tool-cache :initarg :tool-cache
	       :type list
	       :custom (repeat object)
	       :label "Tool: "
	       :group tools
	       :documentation "List of tool cache configurations in this project.
This allows any tool to create, manage, and persist project-specific settings.")
   (web-site-url :initarg :web-site-url
		 :initform ""
		 :type string
		 :custom string
		 :label "Web Site URL"
		 :group name
		 :documentation "URL to this projects web site.
This is a URL to be sent to a web site for documentation.")
   (web-site-directory :initarg :web-site-directory
		       :initform ""
		       :custom string
		       :label "Web Page Directory"
		       :group name
		       :documentation
		       "A directory where web pages can be found by Emacs.
For remote locations use a path compatible with ange-ftp or EFS.
You can also use TRAMP for use with rcp & scp.")
   (web-site-file :initarg :web-site-file
		  :initform ""
		  :custom string
		  :label "Web Page File"
		  :group name
		  :documentation
		  "A file which contains the home page for this project.
This file can be relative to slot `web-site-directory'.
This can be a local file, use ange-ftp, EFS, or TRAMP.")
   (ftp-site :initarg :ftp-site
	     :initform ""
	     :type string
	     :custom string
	     :label "FTP site"
	     :group name
	     :documentation
	     "FTP site where this project's distribution can be found.
This FTP site should be in Emacs form, as needed by `ange-ftp', but can
also be of a form used by TRAMP for use with scp, or rcp.")
   (ftp-upload-site :initarg :ftp-upload-site
		    :initform ""
		    :type string
		    :custom string
		    :label "FTP Upload site"
		    :group name
		    :documentation
		    "FTP Site to upload new distributions to.
This FTP site should be in Emacs form as needed by `ange-ftp'.
If this slot is nil, then use `ftp-site' instead.")
   (configurations :initarg :configurations
		   :initform ("debug" "release")
		   :type list
		   :custom (repeat string)
		   :label "Configuration Options"
		   :group (settings)
		   :documentation "List of available configuration types.
Individual target/project types can form associations between a configuration,
and target specific elements such as build variables.")
   (configuration-default :initarg :configuration-default
			  :initform "debug"
			  :custom string
			  :label "Current Configuration"
			  :group (settings)
			  :documentation "The default configuration.")
   (local-variables :initarg :local-variables
		    :initform nil
		    :custom (repeat (cons (sexp :tag "Variable")
					  (sexp :tag "Value")))
		    :label "Project Local Variables"
		    :group (settings)
		    :documentation "Project local variables")
   (keybindings :allocation :class
		:initform (("D" . ede-debug-target))
		:documentation "Keybindings specialized to this type of target."
		:accessor ede-object-keybindings)
   (menu :allocation :class
	 :initform
	 (
	  [ "Update Version" ede-update-version ede-object ]
	  [ "Version Control Status" ede-vc-project-directory ede-object ]
	  [ "Edit Project Homepage" ede-edit-web-page
	    (and ede-object (oref (ede-toplevel) web-site-file)) ]
	  [ "Browse Project URL" ede-web-browse-home
	    (and ede-object
		 (not (string= "" (oref (ede-toplevel) web-site-url)))) ]
	  "--"
	  [ "Rescan Project Files" ede-rescan-toplevel t ]
	  [ "Edit Projectfile" ede-edit-file-target
	    (and ede-object
		 (or (listp ede-object)
		     (not (obj-of-class-p ede-object ede-project)))) ]
	  )
	 :documentation "Menu specialized to this type of target."
	 :accessor ede-object-menu)
   )
  "Top level EDE project specification.
All specific project types must derive from this project."
  :method-invocation-order :depth-first)

;;; Management variables
;;
;;;###autoload
(defvar ede-projects nil
  "A list of all active projects currently loaded in Emacs.")

(defvar ede-object-root-project nil
  "The current buffer's current root project.
If a file is under a project, this specifies the project that is at
the root of a project tree.")
(make-variable-buffer-local 'ede-object-root-project)

(defvar ede-object-project nil
  "The current buffer's current project at that level.
If a file is under a project, this specifies the project that contains the
current target.")
(make-variable-buffer-local 'ede-object-project)

(defvar ede-object nil
  "The current buffer's target object.
This object's class determines how to compile and debug from a buffer.")
(make-variable-buffer-local 'ede-object)

(defvar ede-selected-object nil
  "The currently user-selected project or target.
If `ede-object' is nil, then commands will operate on this object.")

(defvar ede-constructing nil
  "Non nil when constructing a project hierarchy.")

(defvar ede-deep-rescan nil
  "Non nil means scan down a tree, otherwise rescans are top level only.
Do not set this to non-nil globally.  It is used internally.")

;;; The EDE persistent cache.
;;
(defcustom ede-project-placeholder-cache-file
  (expand-file-name "~/.projects.ede")
  "*File containing the list of projects EDE has viewed."
  :group 'ede
  :type 'file)

(defvar ede-project-cache-files nil
  "List of project files EDE has seen before.")

(defun ede-save-cache ()
  "Save a cache of EDE objects that Emacs has seen before."
  (interactive)
  (let ((p ede-projects)
	(c ede-project-cache-files))
    (condition-case nil
	(progn
	  (set-buffer (find-file-noselect ede-project-placeholder-cache-file t))
	  (erase-buffer)
	  (insert ";; EDE project cache file.
;; This contains a list of projects you have visited.\n(")
	  (while p
	    (when (and (car p) (ede-project-p p))
	      (let ((f (oref (car p) file)))
		(when (file-exists-p f)
		  (insert "\n  \"" f "\""))))
	    (setq p (cdr p)))
	  (while c
	    (insert "\n \"" (car c) "\"")
	    (setq c (cdr c)))
	  (insert "\n)\n")
	  (condition-case nil
	      (save-buffer 0)
	    (error
	     (message "File %s could not be saved."
		      ede-project-placeholder-cache-file)))
	  (kill-buffer (current-buffer))
	  )
      (error
       (message "File %s could not be read."
	       	ede-project-placeholder-cache-file))

      )))

(defun ede-load-cache ()
  "Load the cache of EDE projects."
  (save-excursion
    (let ((cachebuffer nil))
      (condition-case nil
	  (progn
	    (setq cachebuffer
		  (find-file-noselect ede-project-placeholder-cache-file t))
	    (set-buffer cachebuffer)
	    (goto-char (point-min))
	    (let ((c (read (current-buffer)))
		  (new nil)
		  (p ede-projects))
	      ;; Remove loaded projects from the cache.
	      (while p
		(setq c (delete (oref (car p) file) c))
		(setq p (cdr p)))
	      ;; Remove projects that aren't on the filesystem
	      ;; anymore.
	      (while c
		(when (file-exists-p (car c))
		  (setq new (cons (car c) new)))
		(setq c (cdr c)))
	      ;; Save it
	      (setq ede-project-cache-files (nreverse new))))
	(error nil))
      (when cachebuffer (kill-buffer cachebuffer))
      )))

;;; Get the cache usable.
(add-hook 'kill-emacs-hook 'ede-save-cache)
(when (not noninteractive)
  ;; No need to load the EDE cache if we aren't interactive.
  ;; This occurs during batch byte-compiling of other tools.
  (ede-load-cache))


;;; Important macros for doing commands.
;;
(defmacro ede-with-projectfile (obj &rest forms)
  "For the project in which OBJ resides, execute FORMS."
  (list 'save-window-excursion
	(list 'let* (list
		     (list 'pf
			   (list 'if (list 'obj-of-class-p
					   obj 'ede-target)
				 ;; @todo -I think I can change
				 ;; this to not need ede-load-project-file
				 ;; but I'm not sure how to test well.
				 (list 'ede-load-project-file
				       (list 'oref obj 'path))
				 obj))
		     '(dbka (get-file-buffer (oref pf file))))
	      '(if (not dbka) (find-file (oref pf file))
		 (switch-to-buffer dbka))
	      (cons 'progn forms)
	      '(if (not dbka) (kill-buffer (current-buffer))))))
(put 'ede-with-projectfile 'lisp-indent-function 1)


;;; Prompting
;;
(defun ede-singular-object (prompt)
  "Using PROMPT, choose a single object from the current buffer."
  (if (listp ede-object)
      (ede-choose-object prompt ede-object)
    ede-object))

(defun ede-choose-object (prompt list-o-o)
  "Using PROMPT, ask the user which OBJECT to use based on the name field.
Argument LIST-O-O is the list of objects to choose from."
  (let* ((al (object-assoc-list 'name list-o-o))
	 (ans (completing-read prompt al nil t)))
    (setq ans (assoc ans al))
    (cdr ans)))

;;; Menu and Keymap
;;
;;;###autoload
(defvar ede-minor-mode nil
  "Non-nil in EDE controlled buffers.")
(make-variable-buffer-local 'ede-minor-mode)

;; We don't want to waste space.  There is a menu after all.
(add-to-list 'minor-mode-alist '(ede-minor-mode ""))

(defvar ede-minor-keymap
  (let ((map (make-sparse-keymap))
	(pmap (make-sparse-keymap)))
    (define-key pmap "e" 'ede-edit-file-target)
    (define-key pmap "a" 'ede-add-file)
    (define-key pmap "d" 'ede-remove-file)
    (define-key pmap "t" 'ede-new-target)
    (define-key pmap "g" 'ede-rescan-toplevel)
    (define-key pmap "s" 'ede-speedbar)
    (define-key pmap "l" 'ede-load-project-file)
    (define-key pmap "f" 'ede-find-file)
    (define-key pmap "C" 'ede-compile-project)
    (define-key pmap "c" 'ede-compile-target)
    (define-key pmap "\C-c" 'ede-compile-selected)
    (define-key pmap "D" 'ede-debug-target)
    ;; bind our submap into map
    (define-key map "\C-c." pmap)
    map)
  "Keymap used in project minor mode.")

(if ede-minor-keymap
    (progn
      (easy-menu-define
       ede-minor-menu ede-minor-keymap "Project Minor Mode Menu"
       '("Project"
	 ( "Build" :filter ede-build-forms-menu )
	 ( "Project Options" :filter ede-project-forms-menu )
	 ( "Target Options" :filter ede-target-forms-menu )
	 [ "Create Project" ede-new (not ede-object) ]
	 [ "Load a project" ede t ]
;;	 [ "Select Active Target" 'undefined nil ]
;;	 [ "Remove Project" 'undefined nil ]
	 "---"
	 [ "Find File in Project..." ede-find-file t ]
	 ( "Customize" :filter ede-customize-forms-menu )
	 [ "View Project Tree" ede-speedbar t ]
	 ))
      ))

;; Allow re-insertion of a new keymap
(let ((a (assoc 'ede-minor-mode minor-mode-map-alist)))
  (if a
      (setcdr a ede-minor-keymap)
    (add-to-list 'minor-mode-map-alist
		 (cons 'ede-minor-mode ede-minor-keymap))
    ))

(defun ede-menu-obj-of-class-p (class)
  "Return non-nil if some member of `ede-object' is a child of CLASS."
  (if (listp ede-object)
      (ede-or (mapcar (lambda (o) (obj-of-class-p o class)) ede-object))
    (obj-of-class-p ede-object class)))

(defun ede-build-forms-menu (menu-def)
  "Create a sub menu for building different parts of an EDE system.
Argument MENU-DEF is the menu definition to use."
  (easy-menu-filter-return
   (easy-menu-create-menu
    "Build Forms"
    (let ((obj (ede-current-project))
	  (newmenu nil) ;'([ "Build Selected..." ede-compile-selected t ]))
	  targets
	  targitems
	  ede-obj
	  (tskip nil))
      (if (not obj)
	  nil
	(setq targets (when (slot-boundp obj 'targets)
			(oref obj targets))
	      ede-obj (if (listp ede-object) ede-object (list ede-object)))
	;; First, collect the build items from the project
	(setq newmenu (append newmenu (ede-menu-items-build obj t)))
	;; Second, Declare the current target menu items
	(if (and ede-obj (ede-menu-obj-of-class-p ede-target))
	    (while ede-obj
	      (setq newmenu (append newmenu
				    (ede-menu-items-build (car ede-obj) t))
		    tskip (car ede-obj)
		    ede-obj (cdr ede-obj))))
	;; Third, by name, enable builds for other local targets
	(while targets
	  (unless (eq tskip (car targets))
	    (setq targitems (ede-menu-items-build (car targets) nil))
	    (setq newmenu
		  (append newmenu
			  (if (= 1 (length targitems))
			      targitems
			    (cons (ede-name (car targets))
				  targitems))))
	    )
	  (setq targets (cdr targets)))
	;; Fourth, build sub projects.
	;; -- nerp
	;; Fifth, Add make distribution
	(append newmenu (list [ "Make distribution" ede-make-dist t ]))
	)))))

(defun ede-target-forms-menu (menu-def)
  "Create a target MENU-DEF based on the object belonging to this buffer."
  (easy-menu-filter-return
   (easy-menu-create-menu
    "Target Forms"
    (let ((obj (or ede-selected-object ede-object)))
      (append
       '([ "Add File" ede-add-file (ede-current-project) ]
	 [ "Remove File" ede-remove-file
	   (and ede-object
		(or (listp ede-object)
		    (not (obj-of-class-p ede-object ede-project)))) ]
	 "-")
       (if (not obj)
	   nil
	 (if (and (not (listp obj)) (oref obj menu))
	     (oref obj menu)
	   (when (listp obj)
	     ;; This is bad, but I'm not sure what else to do.
	     (oref (car obj) menu)))))))))
       
(defun ede-project-forms-menu (menu-def)
  "Create a target MENU-DEF based on the object belonging to this buffer."
  (easy-menu-filter-return
   (easy-menu-create-menu
    "Project Forms"
    (let* ((obj (ede-current-project))
	   (class (if obj (object-class obj)))
	   (menu nil))
      (condition-case err
	  (progn
	    (while (and class (slot-exists-p class 'menu))
	      ;;(message "Looking at class %S" class)
	      (setq menu (append menu (oref class menu))
		    class (class-parent class))
	      (if (listp class) (setq class (car class))))
	    (append
	     '( [ "Add Target" ede-new-target (ede-current-project) ]
		[ "Remove Target" ede-delete-target ede-object ]
		"-")
	     menu
	     ))
	(error (message "Err found: %S" err)
	       menu)
	)))))

(defun ede-customize-forms-menu (menu-def)
  "Create a menu of the project, and targets that can be customized.
Argument MENU-DEF is the definition of the current menu."
  (easy-menu-filter-return
   (easy-menu-create-menu
    "Customize Project"
    (let* ((obj (ede-current-project))
	   (targ (when (slot-boundp obj 'targets)
		   (oref obj targets))))
      (when obj
	;; Make custom menus for everything here.
	(append (list
		 (cons (concat "Project " (ede-name obj))
		       (eieio-customize-object-group obj)))
		(mapcar (lambda (o)
			  (cons (concat "Target " (ede-name o))
				(eieio-customize-object-group o)))
			targ)))))))


(defun ede-apply-object-keymap (&optional default)
  "Add target specific keybindings into the local map.
Optional argument DEFAULT indicates if this should be set to the default
version of the keymap."
  (let ((object (or ede-object ede-selected-object)))
    (condition-case nil
	(let ((keys (ede-object-keybindings object)))
	  (while keys
	    (local-set-key (concat "\C-c." (car (car keys)))
			   (cdr (car keys)))
	    (setq keys (cdr keys))))
      (error nil))))

;;; Menu building methods for building
;;
(defmethod ede-menu-items-build ((obj ede-project) &optional current)
  "Return a list of menu items for building project OBJ.
If optional argument CURRENT is non-nil, return sub-menu code."
  (if current
      (list [ "Build Current Project" ede-compile-project t ])
    (list (vector
	   (list
	    (concat "Build Project " (ede-name obj))
	    `(project-compile-project ,obj))))))

(defmethod ede-menu-items-build ((obj ede-target) &optional current)
  "Return a list of menu items for building target OBJ.
If optional argument CURRENT is non-nil, return sub-menu code."
  (if current
      (list [ "Build Current Target" ede-compile-target t ])
    (list (vector
	   (concat "Build Target " (ede-name obj))
	   `(project-compile-target ,obj)
	   t))))

;;; Mode Declarations
;;
(eval-and-compile
  (autoload 'ede-dired-minor-mode "ede-dired" "EDE commands for dired" t))

(defun ede-apply-target-options ()
  "Apply options to the current buffer for the active project/target."
  (if (ede-current-project)
      (ede-set-project-variables (ede-current-project)))
  (ede-apply-object-keymap)
  (ede-apply-preprocessor-map)
  )

(defun ede-turn-on-hook ()
  "Turn on EDE minor mode in the current buffer if needed.
To be used in hook functions."
  (if (or (and (stringp (buffer-file-name))
	       (stringp default-directory))
	  ;; Emacs 21 has no buffer file name for directory edits.
	  ;; so we need to add these hacks in.
	  (eq major-mode 'dired-mode)
	  (eq major-mode 'vc-dired-mode))
      (ede-minor-mode 1)))

(defun ede-minor-mode (&optional arg)
  "Project minor mode.
If this file is contained, or could be contained in an EDE
controlled project, then this mode should be active.

With argument ARG positive, turn on the mode.  Negative, turn off the
mode.  nil means to toggle the mode."
  (interactive "P")
  (if (or (eq major-mode 'dired-mode)
	  (eq major-mode 'vc-dired-mode))
      (ede-dired-minor-mode arg)
    (progn
      (setq ede-minor-mode
	    (not (or (and (null arg) ede-minor-mode)
		     (<= (prefix-numeric-value arg) 0))))
      (if (and ede-minor-mode (not ede-constructing)
	       (ede-directory-project-p default-directory))
	  (let* ((ROOT nil)
		 (proj (ede-directory-get-open-project default-directory
						       'ROOT)))
	    (when (not proj)
	      ;; @todo - this could be wasteful.
	      (setq proj (ede-load-project-file default-directory 'ROOT)))

	    (setq ede-object-project proj)
	    (setq ede-object-root-project
		  (or ROOT (ede-project-root proj)))
	    (setq ede-object (ede-buffer-object))
	    (if (and (not ede-object) ede-object-project)
		(ede-auto-add-to-target))
	    (ede-apply-target-options))
	;; If we fail to have a project here, turn it back off.
	(if (not (interactive-p))
	    (setq ede-minor-mode nil))))))

(defun ede-reset-all-buffers (onoff)
  "Reset all the buffers due to change in EDE.
ONOFF indicates enabling or disabling the mode."
  (let ((b (buffer-list)))
    (while b
      (when (buffer-file-name (car b))
	(ede-buffer-object (car b))
	)
      (setq b (cdr b)))))

;;;###autoload
(defun global-ede-mode (arg)
  "Turn on variable `ede-minor-mode' mode when ARG is positive.
If ARG is negative, disable.  Toggle otherwise."
  (interactive "P")
  (if (not arg)
      (if (member 'ede-turn-on-hook find-file-hook)
	  (global-ede-mode -1)
	(global-ede-mode 1))
    (if (or (eq arg t) (> arg 0))
	(progn
	  (add-hook 'semanticdb-project-predicate-functions 'ede-directory-project-p)
	  (add-hook 'semanticdb-project-root-functions 'ede-toplevel-project-or-nil)
	  (add-hook 'ecb-source-path-functions 'ede-ecb-project-paths)
	  (add-hook 'find-file-hooks 'ede-turn-on-hook)
	  (add-hook 'dired-mode-hook 'ede-turn-on-hook))
      (remove-hook 'semanticdb-project-predicate-functions 'ede-directory-project-p)
      (remove-hook 'semanticdb-project-root-functions 'ede-toplevel-project-or-nil)
      (remove-hook 'ecb-source-path-functions 'ede-ecb-project-paths)
      (remove-hook 'find-file-hooks 'ede-turn-on-hook)
      (remove-hook 'dired-mode-hook 'ede-turn-on-hook))
    (ede-reset-all-buffers arg)))

(defun ede-auto-add-to-target ()
  "Look for a target that wants to own the current file.
Follow the preference set with `ede-auto-add-method' and get the list
of objects with the `ede-want-file-p' method."
  (if ede-object (error "Ede-object already defined for %s" (buffer-name)))
  (if (eq ede-auto-add-method 'never)
      nil
    (let (wants desires)
      ;; Find all the objects.
      (setq wants (oref (ede-current-project) targets))
      (while wants
	(if (ede-want-file-p (car wants) (buffer-file-name))
	    (setq desires (cons (car wants) desires)))
	(setq wants (cdr wants)))
      (if desires
	  (cond ((or (eq ede-auto-add-method 'ask)
		     (and (eq ede-auto-add-method 'multi-ask)
			  (< 1 (length desires))))
		 (let* ((al (append
			     ;; some defaults
			     '(("none" . nil)
			       ("new target" . new))
			     ;; If we are in an unparented subdir,
			     ;; offer new a subproject
			     (if (ede-directory-project-p default-directory)
				 ()
			       '(("create subproject" . project)))
			     ;; Here are the existing objects we want.
			     (object-assoc-list 'name desires)))
			(case-fold-search t)
			(ans (completing-read
			      (format "Add %s to target: " (buffer-file-name))
			      al nil t)))
		   (setq ans (assoc ans al))
		   (cond ((eieio-object-p (cdr ans))
			  (ede-add-file (cdr ans)))
			 ((eq (cdr ans) 'new)
			  (ede-new-target))
			 (t nil))))
		((or (eq ede-auto-add-method 'always)
		     (and (eq ede-auto-add-method 'multi-ask)
			  (= 1 (length desires))))
		 (ede-add-file (car desires)))
		(t nil))))))


;;; Interactive method invocations
;;
(defun ede (file)
  "Start up EDE on something.
Argument FILE is the file or directory to load a project from."
  (interactive "fProject File: ")
  (if (not (file-exists-p file))
      (ede-new file)
    (ede-load-project-file (file-name-directory file))))

(defun ede-new (type &optional name)
  "Create a new project starting of project type TYPE.
Optional argument NAME is the name to give this project."
  (interactive
   (list (completing-read "Project Type: "
			  (object-assoc-list
			   'name
			   (let* ((l ede-project-class-files)
				  (cp (ede-current-project))
				  (cs (when cp (object-class cp)))
				  (r nil))
			     (while l
			       (if cs
				   (if (eq (oref (car l) :class-sym)
					   cs)
				       (setq r (cons (car l) r)))
				 (if (oref (car l) new-p)
				     (setq r (cons (car l) r))))
			       (setq l (cdr l)))
			     (when (not r)
			       (if cs
				   (error "No valid interactive sub project types for %s"
					  cs)
				 (error "EDE error: Can't fin project types to create")))
			     r)
			   )
			  nil t)))
  (let* ((obj (object-assoc type 'name ede-project-class-files))
	 (nobj (let ((f (oref obj file))
		     (pf (oref obj proj-file)))
		 ;; We are about to make something new, changing the
		 ;; state of existing directories.
		 (ede-project-directory-remove-hash default-directory)
		 ;; Make sure this class gets loaded!
		 (require f)
		 (make-instance (oref obj class-sym)
				:name (or name (read-string "Name: "))
				:directory default-directory
				:file (cond ((stringp pf)
					     (expand-file-name pf))
					    ((fboundp pf)
					     (funcall pf))
					    (t
					     (error
					      "Unknown file name specifier %S"
					      pf)))
				:targets nil)))
	 (inits (oref obj initializers)))
    ;; Force the name to match for new objects.
    (object-set-name-string nobj (oref nobj :name))
    ;; Handle init args.
    (while inits
      (eieio-oset nobj (car inits) (car (cdr inits)))
      (setq inits (cdr (cdr inits))))
    (let ((pp (ede-parent-project)))
      (when pp
	(ede-add-subproject pp nobj)
	(ede-commit-project pp)))
    (ede-commit-project nobj))
  ;; Have the menu appear
  (setq ede-minor-mode t)
  ;; Allert the user
  (message "Project created and saved.  You may now create targets."))

(defmethod ede-add-subproject ((proj-a ede-project) proj-b)
  "Add into PROJ-A, the subproject PROJ-B."
  (oset proj-a subproj (cons proj-b (oref proj-a subproj))))

(defmethod ede-subproject-relative-path ((proj ede-project) &optional parent-in)
  "Get a path name for PROJ which is relative to the parent project.
If PARENT is specified, then be relative to the PARENT project.
Specifying PARENT is useful for sub-sub projects relative to the root project."
  (let* ((parent (or parent-in (ede-parent-project proj)))
	 (dir (file-name-directory (oref proj file))))
    (if (and parent (not (eq parent proj)))
	(file-relative-name dir (file-name-directory (oref parent file)))
      "")))

(defmethod ede-subproject-p ((proj ede-project))
  "Return non-nil if PROJ is a sub project."
  (ede-parent-project proj))

(defun ede-invoke-method (sym &rest args)
  "Invoke method SYM on the current buffer's project object.
ARGS are additional arguments to pass to method sym."
  (if (not ede-object)
      (error "Cannot invoke %s for %s" (symbol-name sym)
	     (buffer-name)))
  ;; Always query a target.  There should never be multiple
  ;; projects in a single buffer.
  (apply sym (ede-singular-object "Target: ") args))

(defun ede-rescan-toplevel ()
  "Rescan all project files."
  (interactive)
  (let ((toppath (ede-toplevel-project default-directory))
	(ede-deep-rescan t))
    (project-rescan (ede-load-project-file toppath))
    (ede-reset-all-buffers 1)
    ))

(defun ede-new-target (&rest args)
  "Create a new target specific to this type of project file.
Different projects accept different arguments ARGS.
Typically you can specify NAME, target TYPE, and AUTOADD, where AUTOADD is
a string \"y\" or \"n\", which answers the y/n question done interactively."
  (interactive)
  (apply 'project-new-target (ede-current-project) args)
  (setq ede-object nil)
  (setq ede-object (ede-buffer-object (current-buffer)))
  (ede-apply-target-options))

(defun ede-new-target-custom ()
  "Create a new target specific to this type of project file."
  (interactive)
  (project-new-target-custom (ede-current-project)))

(defun ede-delete-target (target)
  "Delete TARGET from the current project."
  (interactive (list
		(let ((ede-object (ede-current-project)))
		  (ede-invoke-method 'project-interactive-select-target
				     "Target: "))))
  ;; Find all sources in buffers associated with the condemned buffer.
  (let ((condemned (ede-target-buffers target)))
    (project-delete-target target)
    ;; Loop over all project controlled buffers
    (save-excursion
      (while condemned
	(set-buffer (car condemned))
	(setq ede-object nil)
	(setq ede-object (ede-buffer-object (current-buffer)))
	(setq condemned (cdr condemned))))
    (ede-apply-target-options)))

(defun ede-add-file (target)
  "Add the current buffer to a TARGET in the current project."
  (interactive (list
		(let ((ede-object (ede-current-project)))
		  (ede-invoke-method 'project-interactive-select-target
				     "Target: "))))
  (when (stringp target)
    (let* ((proj (ede-current-project))
	   (ob (object-assoc-list 'name (oref proj targets))))
      (setq target (cdr (assoc target ob)))))

  (when (not target)
    (error "Could not find specified target %S" target))

  (project-add-file target (buffer-file-name))
  (setq ede-object nil)
  (setq ede-object (ede-buffer-object (current-buffer)))
  (when (not ede-object)
    (error "Can't add %s to target %s: Wrong file type"
	   (file-name-nondirectory (buffer-file-name))
	   (object-name target)))
  (ede-apply-target-options))

;;;###autoload
(defun ede-remove-file (&optional force)
  "Remove the current file from targets.
Optional argument FORCE forces the file to be removed without asking."
  (interactive "P")
  (if (not ede-object)
      (error "Cannot invoke remove-file for %s" (buffer-name)))
  (let ((eo (if (listp ede-object)
		(prog1
		    ede-object
		  (setq force nil))
	      (list ede-object))))
    (while eo
      (if (or force (y-or-n-p (format "Remove from %s? " (ede-name (car eo)))))
	  (project-remove-file (car eo) (buffer-file-name)))
      (setq eo (cdr eo)))
    (setq ede-object nil)
    (setq ede-object (ede-buffer-object (current-buffer)))
    (ede-apply-target-options)))

(defun ede-edit-file-target ()
  "Enter the project file to hand edit the current buffer's target."
  (interactive)
  (ede-invoke-method 'project-edit-file-target))

(defun ede-compile-project ()
  "Compile the current project."
  (interactive)
  ;; @TODO - This just wants the root.  There should be a better way.
  (let ((cp (ede-current-project)))
    (while (ede-parent-project cp)
      (setq cp (ede-parent-project cp)))
    (let ((ede-object cp))
      (ede-invoke-method 'project-compile-project))))

(defun ede-compile-selected (target)
  "Compile some TARGET from the current project."
  (interactive (list (project-interactive-select-target (ede-current-project)
							"Target to Build: ")))
  (project-compile-target target))

;;;###autoload
(defun ede-compile-target ()
  "Compile the current buffer's associated target."
  (interactive)
  (ede-invoke-method 'project-compile-target))

(defun ede-debug-target ()
  "Debug the current buffer's assocated target."
  (interactive)
  (ede-invoke-method 'project-debug-target))

(defun ede-make-dist ()
  "Create a distribution from the current project."
  (interactive)
  (let ((ede-object (ede-current-project)))
    (ede-invoke-method 'project-make-dist)))

(eval-when-compile (require 'eieio-custom))

(defvar eieio-ede-old-variables nil
  "The old variables for a project.")

(defalias 'customize-project 'ede-customize-project)
(defun ede-customize-project (&optional group)
  "Edit fields of the current project through EIEIO & Custom.
Optional GROUP specifies the subgroup of slots to customize."
  (interactive "P")
  (require 'eieio-custom)
  (let* ((ov (oref (ede-current-project) local-variables))
	 (cp (ede-current-project))
	 (group (if group (eieio-read-customization-group cp))))
    (eieio-customize-object cp group)
    (make-local-variable 'eieio-ede-old-variables)
    (setq eieio-ede-old-variables ov)))

(defalias 'customize-target 'ede-customize-current-target)
(defun ede-customize-current-target(&optional group)
  "Edit fields of the current target through EIEIO & Custom.
Optional argument OBJ is the target object to customize.
Optional argument GROUP is the slot group to display."
  (interactive "P")
  (require 'eieio-custom)
  (if (not (obj-of-class-p ede-object ede-target))
      (error "Current file is not part of a target."))
  (let ((group (if group (eieio-read-customization-group ede-object))))
    (ede-customize-target ede-object group)))

(defun ede-customize-target (obj group)
  "Edit fields of the current target through EIEIO & Custom.
Optional argument OBJ is the target object to customize.
Optional argument GROUP is the slot group to display."
  (require 'eieio-custom)
  (if (and obj (not (obj-of-class-p obj ede-target)))
      (error "No logical target to customize"))
  (eieio-customize-object obj (or group 'default)))

(defmethod eieio-done-customizing ((proj ede-project))
  "Call this when a user finishes customizing PROJ."
  (let ((ov eieio-ede-old-variables)
	(nv (oref proj local-variables)))
    (setq eieio-ede-old-variables nil)
    (while ov
      (if (not (assoc (car (car ov)) nv))
	  (save-excursion
	    (mapc (lambda (b)
		    (set-buffer b)
		    (kill-local-variable (car (car ov))))
		  (ede-project-buffers proj))))
      (setq ov (cdr ov)))
    (mapc (lambda (b) (ede-set-project-variables proj b))
	  (ede-project-buffers proj))))

(defmethod eieio-done-customizing ((target ede-target))
  "Call this when a user finishes customizing TARGET."
  nil)

(defmethod ede-commit-project ((proj ede-project))
  "Commit any change to PROJ to its file."
  nil
  )


;;; EDE project placeholder methods
;;
(defmethod ede-project-force-load ((this ede-project-placeholder))
  "Make sure the placeholder THIS is replaced with the real thing.
Return the new object created in its place."
  this
  )


;;; EDE project target baseline methods.
;;
;;  If you are developing a new project type, you need to implement
;;  all of these methods, unless, of course, they do not make sense
;;  for your particular project.
;;
;;  Your targets should inherit from `ede-target', and your project
;;  files should inherit from `ede-project'.  Create the appropriate
;;  methods based on those below.

(defmethod project-interactive-select-target ((this ede-project-placeholder) prompt)
  ; checkdoc-params: (prompt)
  "Make sure placeholder THIS is replaced with the real thing, and pass through."
  (project-interactive-select-target (ede-project-force-load this) prompt))

(defmethod project-interactive-select-target ((this ede-project) prompt)
  "Interactively query for a target that exists in project THIS.
Argument PROMPT is the prompt to use when querying the user for a target."
  (let ((ob (object-assoc-list 'name (oref this targets))))
    (cdr (assoc (completing-read prompt ob nil t) ob))))

(defmethod project-add-file ((this ede-project-placeholder) file)
  ; checkdoc-params: (file)
  "Make sure placeholder THIS is replaced with the real thing, and pass through."
  (project-add-file (ede-project-force-load this) file))

(defmethod project-add-file ((ot ede-target) file)
  "Add the current buffer into project project target OT.
Argument FILE is the file to add."
  (error "add-file not supported by %s" (object-name ot)))

(defmethod project-remove-file ((ot ede-target) fnnd)
  "Remove the current buffer from project target OT.
Argument FNND is an argument."
  (error "remove-file not supported by %s" (object-name ot)))

;;;###autoload
(defmethod project-edit-file-target ((ot ede-target))
  "Edit the target OT associated w/ this file."
  (find-file (oref (ede-current-project) file)))

(defmethod project-new-target ((proj ede-project) &rest args)
  "Create a new target.  It is up to the project PROJ to get the name."
  (error "new-target not supported by %s" (object-name proj)))

(defmethod project-new-target-custom ((proj ede-project))
  "Create a new target.  It is up to the project PROJ to get the name."
  (error "New-target-custom not supported by %s" (object-name proj)))

(defmethod project-delete-target ((ot ede-target))
  "Delete the current target OT from it's parent project."
  (error "add-file not supported by %s" (object-name ot)))

;;;###autoload
(defmethod project-compile-project ((obj ede-project) &optional command)
  "Compile the entire current project OBJ.
Argument COMMAND is the command to use when compiling."
  (error "compile-project not supported by %s" (object-name obj)))

;;;###autoload
(defmethod project-compile-target ((obj ede-target) &optional command)
  "Compile the current target OBJ.
Argument COMMAND is the command to use for compiling the target."
  (error "compile-target not supported by %s" (object-name obj)))

(defmethod project-debug-target ((obj ede-target))
  "Run the current project target OBJ in a debugger."
  (error "debug-target not supported by %s" (object-name obj)))

;;;###autoload
(defmethod project-make-dist ((this ede-project))
  "Build a distribution for the project based on THIS project."
  (error "Make-dist not supported by %s" (object-name this)))

(defmethod project-dist-files ((this ede-project))
  "Return a list of files that constitutes a distribution of THIS project."
  (error "Dist-files is not supported by %s" (object-name this)))

(defmethod project-rescan ((this ede-project))
  "Rescan the EDE proj project THIS."
  (error "Rescanning a project is not supported by %s" (object-name this)))

;;; Default methods for EDE classes
;;
;; These are methods which you might want to override, but there is
;; no need to in most situations because they are either a) simple, or
;; b) cosmetic.

;;;###autoload
(defmethod ede-name ((this ede-target))
  "Return the name of THIS targt."
  (oref this name))

(defmethod ede-target-name ((this ede-target))
  "Return the name of THIS target, suitable for make or debug style commands."
  (oref this name))

(defmethod ede-name ((this ede-project))
  "Return a short-name for THIS project file.
Do this by extracting the lowest directory name."
  (oref this name))

;;;###autoload
(defmethod ede-description ((this ede-project))
  "Return a description suitable for the minibuffer about THIS."
  (format "Project %s: %d subprojects, %d targets."
	  (ede-name this) (length (oref this subproj))
	  (length (oref this targets))))

(defmethod ede-description ((this ede-target))
  "Return a description suitable for the minibuffer about THIS."
  (format "Target %s: with %d source files."
	  (ede-name this) (length (oref this source))))

(defmethod ede-want-file-p ((this ede-target) file)
  "Return non-nil if THIS target wants FILE."
  ;; By default, all targets reference the source object, and let it decide.
  (let ((src (ede-target-sourcecode this)))
    (while (and src (not (ede-want-file-p (car src) file)))
      (setq src (cdr src)))
    src))

(defmethod ede-want-file-source-p ((this ede-target) file)
  "Return non-nil if THIS target wants FILE."
  ;; By default, all targets reference the source object, and let it decide.
  (let ((src (ede-target-sourcecode this)))
    (while (and src (not (ede-want-file-source-p (car src) file)))
      (setq src (cdr src)))
    src))

(defun ede-header-file ()
  "Return the header file for the current buffer.
Not all buffers need headers, so return nil if no applicable."
  (if ede-object
      (ede-buffer-header-file ede-object (current-buffer))
    nil))

(defmethod ede-buffer-header-file ((this ede-project) buffer)
  "Return nil, projects don't have header files."
  nil)

(defmethod ede-buffer-header-file ((this ede-target) buffer)
  "There are no default header files in EDE.
Do a quick check to see if there is a Header tag in this buffer."
  (save-excursion
    (set-buffer buffer)
    (if (re-search-forward "::Header:: \\([a-zA-Z0-9.]+\\)" nil t)
	(buffer-substring-no-properties (match-beginning 1)
					(match-end 1))
      (let ((src (ede-target-sourcecode this))
	    (found nil))
	(while (and src (not found))
	  (setq found (ede-buffer-header-file (car src) (buffer-file-name))
		src (cdr src)))
	found))))

;;;###autoload
(defun ede-documentation-files ()
  "Return the documentation files for the current buffer.
Not all buffers need documentations, so return nil if no applicable.
Some projects may have multiple documentation files, so return a list."
  (if ede-object
      (ede-buffer-documentation-files ede-object (current-buffer))
    nil))

(defmethod ede-buffer-documentation-files ((this ede-project) buffer)
  "Return all documentation in project THIS based on BUFFER."
  ;; Find the info node.
  (ede-documentation this))

(defmethod ede-buffer-documentation-files ((this ede-target) buffer)
  "Check for some documentation files for THIS.
Also do a quick check to see if there is a Documentation tag in this BUFFER."
  (save-excursion
    (set-buffer buffer)
    (if (re-search-forward "::Documentation:: \\([a-zA-Z0-9.]+\\)" nil t)
	(buffer-substring-no-properties (match-beginning 1)
					(match-end 1))
      ;; Check the master project
      (let ((cp (ede-toplevel)))
	(ede-buffer-documentation-files cp (current-buffer))))))

(defmethod ede-documentation ((this ede-project))
  "Return a list of files that provides documentation.
Documentation is not for object THIS, but is provided by THIS for other
files in the project."
  (let ((targ (oref this targets))
	(proj (oref this subproj))
	(found nil))
    (while targ
      (setq found (append (ede-documentation (car targ)) found)
	    targ (cdr targ)))
    (while proj
      (setq found (append (ede-documentation (car proj)) found)
	    proj (cdr proj)))
    found))

(defmethod ede-documentation ((this ede-target))
  "Return a list of files that provides documentation.
Documentation is not for object THIS, but is provided by THIS for other
files in the project."
  nil)

(defun ede-html-documentation-files ()
  "Return a list of HTML documentation files associated with this project."
  (ede-html-documentation (ede-toplevel))
  )

(defmethod ede-html-documentation ((this ede-project))
  "Return a list of HTML files provided by project THIS."
  
  )

(defun ede-ecb-project-paths ()
  "Return a list of all paths for all active EDE projects.
This functions is meant for use with ECB."
  (let ((p ede-projects)
	(d nil))
    (while p
      (setq d (cons (file-name-directory (oref (car p) file))
		    d)
	    p (cdr p)))
    d))

;;; EDE project-autoload methods
;;
(defmethod ede-dir-to-projectfile ((this ede-project-autoload) dir)
  "Return a full file name of project THIS found in DIR.
Return nil if the project file does not exist."
  (let* ((d (file-name-as-directory dir))
	 (root (ede-project-root-directory this d))
	 (pf (oref this proj-file))
	 (f (cond ((stringp pf)
		   (expand-file-name pf (or root d)))
		  ((and (symbolp pf) (fboundp pf))
		   (funcall pf (or root d)))))
	 )
    (when (and f (file-exists-p f))
      f)))

;;; EDE basic functions
;;
(defun ede-add-project-to-global-list (proj)
  "Add the project PROJ to the master list of projects.
On success, return the added project."
  (when (not proj)
    (error "No project created to add to master list"))
  (when (not (eieio-object-p proj))
    (error "Attempt to add Non-object to master project list"))
  (when (not (obj-of-class-p proj ede-project-placeholder))
    (error "Attempt to add a non-project to the ede projects list"))
  (add-to-list 'ede-projects proj)
  proj)

;;;###autoload
(defun ede-load-project-file (dir &optional rootreturn)
  "Project file independent way to read a project in from DIR.
Optional ROOTRETURN will return the root project for DIR."
  ;; Only load if something new is going on.  Flush the dirhash.
  (ede-project-directory-remove-hash dir)
  ;; Do the load
  ;;(message "EDE LOAD : %S" file)
  (let* ((file dir)
	 (path (expand-file-name (file-name-directory file)))
	 (pfc (ede-directory-project-p path))
	 (toppath nil)
	 (o nil))
    (cond
     ((not pfc)
      ;; @TODO - Do we really need to scan?  Is this a waste of time?
      ;; Scan upward for a the next project file style.
      (let ((p path))
	(while (and p (not (ede-directory-project-p p)))
	  (setq p (ede-up-directory p)))
	(if p (ede-load-project-file p)
	  nil)
	;; recomment as we go
	;nil
	))
     ;; Do nothing if we are buiding an EDE project already
     (ede-constructing
      nil)
     ;; Load in the project in question.
     (t
      (setq toppath (ede-toplevel-project path))
      ;; We found the top-most directory.  Check to see if we already
      ;; have an object defining it's project.
      (setq pfc (ede-directory-project-p toppath t))

      ;; See if it's been loaded before
      (setq o (object-assoc (ede-dir-to-projectfile pfc toppath) 'file
			    ede-projects))
      (if (not o)
	  ;; If not, get it now.
	  (let ((ede-constructing t))
	    (setq o (funcall (oref pfc load-type) toppath))
	    (when (not o)
	      (error "Project type error: :load-type failed to create a project"))
	    (ede-add-project-to-global-list o)))

      ;; Return the found root project.
      (when rootreturn (set rootreturn o))

      (let (tocheck found)
	;; Now find the project file belonging to FILE!
	(setq tocheck (list o))
	(setq file (ede-dir-to-projectfile pfc (expand-file-name path)))
	(while (and tocheck (not found))
	  (let ((newbits nil))
	    (when (car tocheck)
	      (if (string= file (oref (car tocheck) file))
		  (setq found (car tocheck)))
	      (setq newbits (oref (car tocheck) subproj)))
	    (setq tocheck
		  (append (cdr tocheck) newbits))))
	(if (not found)
	    (message "No project for %s, but passes project-p test" file)
	  ;; Now that the file has been reset inside the project object, do
	  ;; the cache maintenance.
	  (setq ede-project-cache-files
		(delete (oref found file) ede-project-cache-files)))
	found)))))

;;;###autoload
(defun ede-parent-project (&optional obj)
  "Return the project belonging to the parent directory.
nil if there is no previous directory.
Optional argument OBJ is an object to find the parent of."
  (let* ((proj (or obj ede-object-project)) ;; Current project.
	 (root (if obj (ede-project-root obj)
		 ede-object-root-project)))
    (if
	;; This case is a SHORTCUT if the project has defined
	;; a way to calculate the project root.
	(and root proj (eq root proj))
	nil ;; we are at the root.
      ;; Else, we may have a nil proj or root.
      (let* ((thisdir (if obj (oref obj directory)
			default-directory))
	     (updir (ede-up-directory thisdir))
	     (ans nil)
	     )
	;; If there was no root, perhaps we can derive it from
	;; updir now.
	(when (not root)
	  (setq root (ede-directory-get-toplevel-open-project updir)))

	;; This lets us find a subproject under root based on updir.
	(when root
	  (setq ans (ede-find-subproject-for-directory
		     root updir)))

	;; Try the all structure based search.
	(setq ans (ede-directory-get-open-project updir))

	;; Load up the project file as a last resort.
	;; Last resort since it uses file-truename, and other
	;; slow features.
	(when (and (not ans) (ede-directory-project-p updir))
	  (setq ans (ede-load-project-file
		     (file-name-as-directory updir))))
	ans))))

(defun ede-current-project (&optional dir)
  "Return the current project file.
If optional DIR is provided, get the project for DIR instead."
  (let ((ans nil))
    ;; If it matches the current directory, do we have a pre-existing project?
    (when (and (or (not dir) (string= dir default-directory))
	       ede-object-project)
      (setq ans ede-object-project)
      )
    ;; No current project.
    (when (not ans)
      (let* ((ldir (or dir default-directory)))
	(setq ans (ede-directory-get-open-project ldir))
	(or ans
	    ;; No open project, if this dir pass project-p, then load.
	    (when (ede-directory-project-p ldir)
	      (setq ans (ede-load-project-file ldir))))))
    ;; Return what we found.
    ans))

(defun ede-buffer-object (&optional buffer)
  "Return the target object for BUFFER.
This function clears cached values and recalculates."
  (save-excursion
    (if (not buffer) (setq buffer (current-buffer)))
    (set-buffer buffer)
    (setq ede-object nil)
    (let ((po (ede-current-project)))
      (if po (setq ede-object (ede-find-target po buffer))))
    (if (= (length ede-object) 1)
	(setq ede-object (car ede-object)))
    ede-object))

(defmethod ede-target-in-project-p ((proj ede-project) target)
  "Is PROJ the parent of TARGET?
If TARGET belongs to a subproject, return that project file."
  (if (and (slot-boundp proj 'targets)
	   (memq target (oref proj targets)))
      proj
    (let ((s (oref proj subproj))
	  (ans nil))
      (while (and s (not ans))
	(setq ans (ede-target-in-project-p (car s) target))
	(setq s (cdr s)))
      ans)))

;;;###autoload
(defun ede-target-parent (target)
  "Return the project which is the parent of TARGET.
It is recommended you track the project a different way as this function
could become slow in time."
  ;; @todo - use ede-object-project as a starting point.
  (let ((ans nil) (projs ede-projects))
    (while (and (not ans) projs)
      (setq ans (ede-target-in-project-p (car projs) target)
	    projs (cdr projs)))
    ans))

(defun ede-maybe-checkout (&optional buffer)
  "Check BUFFER out of VC if necessary."
  (save-excursion
    (if buffer (set-buffer buffer))
    (if (and buffer-read-only vc-mode
	     (y-or-n-p "Checkout Makefile.am from VC? "))
	(vc-toggle-read-only))))

(defmethod ede-find-target ((proj ede-project) buffer)
  "Fetch the target in PROJ belonging to BUFFER or nil."
  (save-excursion
    (set-buffer buffer)
    (or ede-object
	(if (ede-buffer-mine proj buffer)
	    proj
	  (let ((targets (oref proj targets))
		(f nil))
	    (while targets
	      (if (ede-buffer-mine (car targets) buffer)
		  (setq f (cons (car targets) f)))
	      (setq targets (cdr targets)))
	    f)))))

(defmethod ede-target-buffer-in-sourcelist ((this ede-target) buffer source)
  "Return non-nil if object THIS is in BUFFER to a SOURCE list.
Handles complex path issues."
  (member (ede-convert-path this (buffer-file-name buffer)) source))

(defmethod ede-buffer-mine ((this ede-project) buffer)
  "Return non-nil if object THIS lays claim to the file in BUFFER."
  nil)

(defmethod ede-buffer-mine ((this ede-target) buffer)
  "Return non-nil if object THIS lays claim to the file in BUFFER."
  (condition-case nil
      (ede-target-buffer-in-sourcelist this buffer (oref this source))
    ;; An error implies a bad match.
    (error nil)))


;;; Project mapping
;;
(defun ede-project-buffers (project)
  "Return a list of all active buffers controlled by PROJECT.
This includes buffers controlled by a specific target of PROJECT."
  (let ((bl (buffer-list))
	(pl nil))
    (while bl
      (save-excursion
	(set-buffer (car bl))
	(if (and ede-object (eq (ede-current-project) project))
	    (setq pl (cons (car bl) pl))))
      (setq bl (cdr bl)))
    pl))

(defun ede-target-buffers (target)
  "Return a list of buffers that are controlled by TARGET."
  (let ((bl (buffer-list))
	(pl nil))
    (while bl
      (save-excursion
	(set-buffer (car bl))
	(if (if (listp ede-object)
		(memq target ede-object)
	      (eq ede-object target))
	    (setq pl (cons (car bl) pl))))
      (setq bl (cdr bl)))
    pl))

(defun ede-buffers ()
  "Return a list of all buffers controled by an EDE object."
  (let ((bl (buffer-list))
	(pl nil))
    (while bl
      (save-excursion
	(set-buffer (car bl))
	(if ede-object
	    (setq pl (cons (car bl) pl))))
      (setq bl (cdr bl)))
    pl))

(defun ede-map-buffers (proc)
  "Execute PROC on all buffers controled by EDE."
  (mapcar proc (ede-buffers)))

(defmethod ede-map-project-buffers ((this ede-project) proc)
  "For THIS, execute PROC on all buffers belonging to THIS."
  (mapcar proc (ede-project-buffers this)))

(defmethod ede-map-target-buffers ((this ede-target) proc)
  "For THIS, execute PROC on all buffers belonging to THIS."
  (mapcar proc (ede-target-buffers this)))

;; other types of mapping
(defmethod ede-map-subprojects ((this ede-project) proc)
  "For object THIS, execute PROC on all direct subprojects.
This function does not apply PROC to sub-sub projects.
See also `ede-map-all-subprojects'."
  (mapcar proc (oref this subproj)))

(defmethod ede-map-all-subprojects ((this ede-project) allproc)
  "For object THIS, execute PROC on THIS and  all subprojects.
This function also applies PROC to sub-sub projects.
See also `ede-map-subprojects'."
  (apply 'append 
	 (list (funcall allproc this))
	 (ede-map-subprojects
	  this
	  (lambda (sp)
	    (ede-map-all-subprojects sp allproc))
	  )))

;; (ede-map-all-subprojects (ede-load-project-file "../semantic/") (lambda (sp) (oref sp file)))

(defmethod ede-map-targets ((this ede-project) proc)
  "For object THIS, execute PROC on all targets."
  (mapcar proc (oref this targets)))

(defmethod ede-map-any-target-p ((this ede-project) proc)
  "For project THIS, map PROC to all targets and return if any non-nil.
Return the first non-nil value returned by PROC."
  (ede-or (ede-map-targets this proc)))


;;; Some language specific methods.
;;
;; These items are needed by ede-cpp-root to add better support for
;; configuring items for Semantic.
(defun ede-apply-preprocessor-map ()
  "Apply preprocessor tables onto the current buffer."
  (when (and ede-object (boundp 'semantic-lex-spp-macro-symbol-obarray))
    (let ((map (ede-preprocessor-map ede-object)))
      (when map
	;; We can't do a require for the below symbol.
	(setq semantic-lex-spp-project-macro-symbol-obarray
	      (semantic-lex-make-spp-table map))
	))))

(defmethod ede-system-include-path ((this ede-project))
  "Get the system include path used by project THIS."
  nil)
  
(defmethod ede-preprocessor-map ((this ede-project))
  "Get the pre-processor map for project THIS."
  nil)

(defmethod ede-system-include-path ((this ede-target))
  "Get the system include path used by project THIS."
  nil)
  
(defmethod ede-preprocessor-map ((this ede-target))
  "Get the pre-processor map for project THIS."
  nil)


;;; Project-local variables
;;
(defun ede-make-project-local-variable (variable &optional project)
  "Make VARIABLE project-local to PROJECT."
  (if (not project) (setq project (ede-current-project)))
  (if (assoc variable (oref project local-variables))
      nil
    (oset project local-variables (cons (list variable)
					(oref project local-variables)))
    (mapcar (lambda (b) (save-excursion
			  (set-buffer  b)
			  (make-local-variable variable)))
	    (ede-project-buffers project))))

(defmethod ede-set-project-variables ((project ede-project) &optional buffer)
  "Set variables local to PROJECT in BUFFER."
  (if (not buffer) (setq buffer (current-buffer)))
  (save-excursion
   (set-buffer buffer)
   (mapcar (lambda (v)
	     (make-local-variable (car v))
	     ;; set it's value here?
	     (set (car v) (cdr v))
	     )
	   (oref project local-variables))))

(defun ede-set (variable value)
  "Set the project local VARIABLE to VALUE.
If VARIABLE is not project local, just use set."
  (let ((p (ede-current-project)) a)
    (if (and p (setq a (assoc variable (oref p local-variables))))
	(progn
	  (setcdr a value)
	  (mapc (lambda (b) (save-excursion
			      (set-buffer b)
			      (set variable value)))
		(ede-project-buffers p)))
      (set variable value))
    (ede-commit-local-variables p))
  value)

(defmethod ede-commit-local-variables ((proj ede-project))
  "Commit change to local variables in PROJ."
  nil)


;;; Accessors for more complex types where oref is inappropriate.
;;
(defmethod ede-target-sourcecode ((this ede-target))
  "Return the sourcecode objects which THIS permits."
  (let ((sc (oref this sourcetype))
	(rs nil))
    (while (and (listp sc) sc)
      (setq rs (cons (symbol-value (car sc)) rs)
	    sc (cdr sc)))
    rs))


;;; Lame stuff
;;
(defun ede-or (arg)
  "Do `or' like stuff to ARG because you can't apply `or'."
  (while (and arg (not (car arg)))
    (setq arg (cdr arg)))
  arg)


;;; Debugging.
;;
;;;###autoload
(defun ede-adebug-project ()
  "Run adebug against the current ede project.
Display the results as a debug list."
  (interactive)
  (require 'data-debug)
  (when (ede-current-project)
    (data-debug-new-buffer "*Analyzer ADEBUG*")
    (data-debug-insert-object-slots (ede-current-project) "")
    ))

;;;###autoload
(defun ede-adebug-project-parent ()
  "Run adebug against the current ede parent project.
Display the results as a debug list."
  (interactive)
  (require 'data-debug)
  (when (ede-parent-project)
    (data-debug-new-buffer "*Analyzer ADEBUG*")
    (data-debug-insert-object-slots (ede-parent-project) "")
    ))

;;;###autoload
(defun ede-adebug-project-root ()
  "Run adebug against the current ede parent project.
Display the results as a debug list."
  (interactive)
  (require 'data-debug)
  (when (ede-toplevel)
    (data-debug-new-buffer "*Analyzer ADEBUG*")
    (data-debug-insert-object-slots (ede-toplevel) "")
    ))

;;; Hooks & Autoloads
;;
;;  These let us watch various activities, and respond apropriatly.

(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec ede-with-projectfile
	      (form def-body))))

;; Prevent warnings w/out requiring ede-speedbar.
(eval-and-compile
  (autoload 'ede-speedbar "ede-speedbar" "Run speedbar in EDE project mode." t)
  (autoload 'ede-speedbar-file-setup "ede-speedbar" "EDE in Speedbar File mode hack." t)
)

(autoload 'ede-update-version "ede-util"
  "Update the version of the current project." t)

(autoload 'ede-vc-project-directory "ede-system" t
  "Run `vc-directory' on the the current project.")

(autoload 'ede-web-browse-home "ede-system" t
  "Web browse this project's home page.")

(autoload 'ede-edit-web-page "ede-system" t
  "Edit the web site for this project.")

(autoload 'ede-upload-distribution "ede-system" t
  "Upload the dist for this project to the upload site.")

(autoload 'ede-upload-html-documentation "ede-system" t
  "Upload auto-generated HTML to the web site.")

(provide 'ede)

(require 'ede-files)

;; If this does not occur after the provide, we can get a recursive
;; load.  Yuck!
(if (featurep 'speedbar)
    (ede-speedbar-file-setup)
  (add-hook 'speedbar-load-hook 'ede-speedbar-file-setup))

;;; ede.el ends here
