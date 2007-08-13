;;; ede-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (ede-target-parent ede-parent-project ede-load-project-file
;;;;;;  ede-documentation-files ede-description ede-name project-make-dist
;;;;;;  project-compile-target project-compile-project project-edit-file-target
;;;;;;  ede-compile-target ede-remove-file ede-project ede-target
;;;;;;  ede-project-autoload) "ede" "ede.el" (18078 49091))
;;; Generated autoloads from ede.el

(autoload (quote ede-project-autoload) "ede" "\
Class representing minimal knowledge set to run preliminary EDE functions.
When more advanced functionality is needed from a project type, that projects
type is required and the load function used.

\(fn)" nil nil)

(autoload (quote ede-target) "ede" "\
A top level target to build.

\(fn EIEIO-SPEEDBAR-DIRECTORY-BUTTON)" nil nil)

(autoload (quote ede-project) "ede" "\
Top level EDE project specification.
All specific project types must derive from this project.

\(fn EDE-PROJECT-PLACEHOLDER)" nil nil)

(defvar ede-projects nil "\
A list of all active projects currently loaded in Emacs.")

(defvar ede-minor-mode nil "\
Non-nil in EDE controlled buffers.")

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

;;;***

;;;### (autoloads (ede-pmake-varname) "ede-pmake" "ede-pmake.el"
;;;;;;  (17881 43568))
;;; Generated autoloads from ede-pmake.el

(autoload (quote ede-pmake-varname) "ede-pmake" "\
Convert OBJ into a variable name name, which converts .  to _.

\(fn OBJ)" nil nil)

;;;***

;;;### (autoloads nil "ede-proj" "ede-proj.el" (17881 43589))
;;; Generated autoloads from ede-proj.el

(add-to-list (quote auto-mode-alist) (quote ("Project\\.ede" . emacs-lisp-mode)))

;;;***

;;;### (autoloads (ede-simple-project ede-simple-load ede-simple-projectfile-for-dir)
;;;;;;  "ede-simple" "ede-simple.el" (18042 51835))
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

\(fn DIR)" nil nil)

(autoload (quote ede-simple-project) "ede-simple" "\
EDE Simple project class.
Each directory needs a a project file to control it.

\(fn EIEIO-PERSISTENT EDE-PROJECT)" nil nil)

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
;;;;;;  (18110 13412 718342))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ede-loaddefs.el ends here
