;;; ede-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (ede-adebug-project ede-target-parent ede-parent-project
;;;;;;  ede-load-project-file ede-documentation-files ede-description
;;;;;;  ede-name project-make-dist project-compile-target project-compile-project
;;;;;;  project-edit-file-target ede-compile-target ede-remove-file
;;;;;;  global-ede-mode) "ede" "ede.el" (18592 59461))
;;; Generated autoloads from ede.el

(eieio-defclass-autoload (quote ede-project-autoload) (quote nil) "ede" "Class representing minimal knowledge set to run preliminary EDE functions.\nWhen more advanced functionality is needed from a project type, that projects\ntype is required and the load function used.")

(eieio-defclass-autoload (quote ede-target) (quote (eieio-speedbar-directory-button)) "ede" "A top level target to build.")

(eieio-defclass-autoload (quote ede-project) (quote (ede-project-placeholder)) "ede" "Top level EDE project specification.\nAll specific project types must derive from this project.")

(defvar ede-projects nil "\
A list of all active projects currently loaded in Emacs.")

(defvar ede-minor-mode nil "\
Non-nil in EDE controlled buffers.")

(autoload (quote global-ede-mode) "ede" "\
Turn on variable `ede-minor-mode' mode when ARG is positive.
If ARG is negative, disable.  Toggle otherwise.

\(fn ARG)" t nil)

(autoload (quote ede-remove-file) "ede" "\
Remove the current file from targets.
Optional argument FORCE forces the file to be removed without asking.

\(fn &optional FORCE)" t nil)

(autoload (quote ede-compile-target) "ede" "\
Compile the current buffer's associated target.

\(fn)" t nil)

(autoload (quote project-edit-file-target) "ede" "\
Edit the target OT associated w/ this file.

\(fn (OT ede-target))" nil nil)

(autoload (quote project-compile-project) "ede" "\
Compile the entire current project OBJ.
Argument COMMAND is the command to use when compiling.

\(fn (OBJ ede-project) &optional COMMAND)" nil nil)

(autoload (quote project-compile-target) "ede" "\
Compile the current target OBJ.
Argument COMMAND is the command to use for compiling the target.

\(fn (OBJ ede-target) &optional COMMAND)" nil nil)

(autoload (quote project-make-dist) "ede" "\
Build a distribution for the project based on THIS project.

\(fn (THIS ede-project))" nil nil)

(autoload (quote ede-name) "ede" "\
Return the name of THIS targt.

\(fn (THIS ede-target))" nil nil)

(autoload (quote ede-description) "ede" "\
Return a description suitible for the minibuffer about THIS.

\(fn (THIS ede-project))" nil nil)

(autoload (quote ede-documentation-files) "ede" "\
Return the documentation files for the current buffer.
Not all buffers need documentations, so return nil if no applicable.
Some projects may have multiple documentation files, so return a list.

\(fn)" nil nil)

(autoload (quote ede-load-project-file) "ede" "\
Project file independent way to read in FILE.

\(fn FILE)" nil nil)

(autoload (quote ede-parent-project) "ede" "\
Return the project belonging to the parent directory.
nil if there is no previous directory.
Optional argument OBJ is an object to find the parent of.

\(fn &optional OBJ)" nil nil)

(autoload (quote ede-target-parent) "ede" "\
Return the project which is the parent of TARGET.
It is recommended you track the project a different way as this function
could become slow in time.

\(fn TARGET)" nil nil)

(autoload (quote ede-adebug-project) "ede" "\
Run adebug against the current ede project.
Display the results as a debug list.

\(fn)" t nil)

;;;***

;;;### (autoloads (ede-cpp-root-load ede-cpp-root-project-root ede-cpp-root-project-file-for-dir)
;;;;;;  "ede-cpp-root" "ede-cpp-root.el" (18539 36731))
;;; Generated autoloads from ede-cpp-root.el

(autoload (quote ede-cpp-root-project-file-for-dir) "ede-cpp-root" "\
Return a full file name to the project file stored in DIR.

\(fn &optional DIR)" nil nil)

(autoload (quote ede-cpp-root-project-root) "ede-cpp-root" "\
Get the root directory for DIR.

\(fn)" nil nil)

(autoload (quote ede-cpp-root-load) "ede-cpp-root" "\
Return a CPP root object if you created one.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project.

\(fn DIR &optional ROOTPROJ)" nil nil)

(add-to-list (quote ede-project-class-files) (ede-project-autoload "cpp-root" :name "CPP ROOT" :file (quote ede-cpp-root) :proj-file (quote ede-cpp-root-project-file-for-dir) :proj-root (quote ede-cpp-root-project-root) :load-type (quote ede-cpp-root-load) :class-sym (quote ede-cpp-root) :new-p nil) t)

(eieio-defclass-autoload (quote ede-cpp-root-project) (quote (eieio-instance-tracker ede-project)) "ede-cpp-root" "EDE cpp-root project class.\nEach directory needs a a project file to control it.")

;;;***

;;;### (autoloads (ede-pmake-varname) "ede-pmake" "ede-pmake.el"
;;;;;;  (18362 19217))
;;; Generated autoloads from ede-pmake.el

(autoload (quote ede-pmake-varname) "ede-pmake" "\
Convert OBJ into a variable name name, which converts .  to _.

\(fn OBJ)" nil nil)

;;;***

;;;### (autoloads nil "ede-proj" "ede-proj.el" (18521 49142))
;;; Generated autoloads from ede-proj.el

(add-to-list (quote auto-mode-alist) (quote ("Project\\.ede$" . emacs-lisp-mode)))

;;;***

;;;### (autoloads (ede-simple-load ede-simple-projectfile-for-dir)
;;;;;;  "ede-simple" "ede-simple.el" (18519 58532))
;;; Generated autoloads from ede-simple.el

(add-to-list (quote ede-project-class-files) (ede-project-autoload "simple-overlay" :name "Simple" :file (quote ede-simple) :proj-file (quote ede-simple-projectfile-for-dir) :load-type (quote ede-simple-load) :class-sym (quote ede-simple-project)) t)

(autoload (quote ede-simple-projectfile-for-dir) "ede-simple" "\
Return a full file name to the project file stored in the current directory.
The directory has three parts:
  <STORAGE ROOT>/<PROJ DIR AS FILE>/ProjSimple.ede

\(fn &optional DIR)" nil nil)

(autoload (quote ede-simple-load) "ede-simple" "\
Load a project of type `Simple' for the directory DIR.
Return nil if there isn't one.
ROOTPROJ is nil, since we will only create a single EDE project here.

\(fn DIR &optional ROOTPROJ)" nil nil)

(eieio-defclass-autoload (quote ede-simple-project) (quote (eieio-persistent ede-project)) "ede-simple" "EDE Simple project class.\nEach directory needs a a project file to control it.")

;;;***

;;;### (autoloads (ede-srecode-insert ede-srecode-setup) "ede-srecode"
;;;;;;  "ede-srecode.el" (18521 49742))
;;; Generated autoloads from ede-srecode.el

(autoload (quote ede-srecode-setup) "ede-srecode" "\
Update various paths to get SRecode to identify our macros.

\(fn)" nil nil)

(autoload (quote ede-srecode-insert) "ede-srecode" "\
Insert at the current point TEMPLATE.
TEMPLATE should specify a context by using a string format of:
  context:templatename
Add DICTIONARY-ENTRIES into the dictionary before insertion.
Note: Just like `srecode-insert', but templates found in 'ede app.

\(fn TEMPLATE &rest DICTIONARY-ENTRIES)" nil nil)

;;;***

;;;### (autoloads (ede-update-version) "ede-util" "ede-util.el" (17213
;;;;;;  40281))
;;; Generated autoloads from ede-util.el

(autoload (quote ede-update-version) "ede-util" "\
Update the current projects main version number.
Argument NEWVERSION is the version number to use in the current project.

\(fn NEWVERSION)" t nil)

;;;***

;;;### (autoloads nil nil ("autoconf-compat.el" "autoconf-edit.el"
;;;;;;  "ede-dired.el" "ede-load.el" "ede-pconf.el" "ede-proj-archive.el"
;;;;;;  "ede-proj-aux.el" "ede-proj-comp.el" "ede-proj-elisp.el"
;;;;;;  "ede-proj-info.el" "ede-proj-misc.el" "ede-proj-obj.el" "ede-proj-prog.el"
;;;;;;  "ede-proj-scheme.el" "ede-proj-shared.el" "ede-proj-skel.el"
;;;;;;  "ede-source.el" "ede-speedbar.el" "ede-system.el" "project-am.el")
;;;;;;  (18594 52853 908243))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ede-loaddefs.el ends here
