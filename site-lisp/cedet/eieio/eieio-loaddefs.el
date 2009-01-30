;;; eieio-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (call-tree) "call-tree" "call-tree.el" (18668 41327))
;;; Generated autoloads from call-tree.el

(autoload 'call-tree "call-tree" "\
Build a call tree to show all functions called by FUNC.

\(fn FUNC)" t nil)

;;;***

;;;### (autoloads (chart-test-it-all) "chart" "chart.el" (18753 19409))
;;; Generated autoloads from chart.el

(autoload 'chart-test-it-all "chart" "\
Test out various charting features.

\(fn)" t nil)

;;;***

;;;### (autoloads (data-debug-show data-debug-insert-object-button
;;;;;;  data-debug-insert-object-slots) "eieio-datadebug" "eieio-datadebug.el"
;;;;;;  (18810 50577))
;;; Generated autoloads from eieio-datadebug.el

(autoload 'data-debug-insert-object-slots "eieio-datadebug" "\
Insert all the slots of OBJECT.
PREFIX specifies what to insert at the start of each line.

\(fn OBJECT PREFIX)" nil nil)

(autoload 'data-debug-insert-object-button "eieio-datadebug" "\
Insert a button representing OBJECT.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between PREFIX and the object button.

\(fn OBJECT PREFIX PREBUTTONTEXT)" nil nil)

(autoload 'data-debug-show "eieio-datadebug" "\
Run ddebug against any EIEIO object OBJ

\(fn (OBJ eieio-default-superclass))" nil nil)

;;;***

;;;### (autoloads (eieio-describe-generic eieio-build-class-alist
;;;;;;  eieio-describe-constructor eieio-describe-class eieio-browse)
;;;;;;  "eieio-opt" "eieio-opt.el" (18810 50577))
;;; Generated autoloads from eieio-opt.el

(autoload 'eieio-browse "eieio-opt" "\
Create an object browser window to show all objects.
If optional ROOT-CLASS, then start with that, otherwise start with
variable `eieio-default-superclass'.

\(fn &optional ROOT-CLASS)" t nil)

(defalias 'describe-class 'eieio-describe-class)

(autoload 'eieio-describe-class "eieio-opt" "\
Describe a CLASS defined by a string or symbol.
If CLASS is actually an object, then also display current values of that obect.
Optional HEADERFCN should be called to insert a few bits of info first.

\(fn CLASS &optional HEADERFCN)" t nil)

(autoload 'eieio-describe-constructor "eieio-opt" "\
Describe the constructor function FCN.
Uses `eieio-describe-class' to describe the class being constructed.

\(fn FCN)" t nil)

(autoload 'eieio-build-class-alist "eieio-opt" "\
Return an alist of all currently active classes for completion purposes.
Optional argument CLASS is the class to start with.
If INSTANTIABLE-ONLY is non nil, only allow names of classes which
are not abstract, otherwise allow all classes.
Optional argument BUILDLIST is more list to attach and is used internally.

\(fn &optional CLASS INSTANTIABLE-ONLY BUILDLIST)" nil nil)

(defalias 'describe-method 'eieio-describe-generic)

(defalias 'describe-generic 'eieio-describe-generic)

(defalias 'eieio-describe-method 'eieio-describe-generic)

(autoload 'eieio-describe-generic "eieio-opt" "\
Describe the generic function GENERIC.
Also extracts information about all methods specific to this generic.

\(fn GENERIC)" t nil)

;;;***

;;;### (autoloads (eieio-perftest-onemethodcall eieio-perftest-methodcall)
;;;;;;  "eieio-perftest" "eieio-perftest.el" (18810 50577))
;;; Generated autoloads from eieio-perftest.el

(autoload 'eieio-perftest-methodcall "eieio-perftest" "\
Test and time performance of method invocation.

\(fn)" t nil)

(autoload 'eieio-perftest-onemethodcall "eieio-perftest" "\
Test and time performance of method invocation.

\(fn)" t nil)

;;;***

;;;### (autoloads (object-write-xml) "eieio-xml" "eieio-xml.el" (18753
;;;;;;  19409))
;;; Generated autoloads from eieio-xml.el

(autoload 'object-write-xml "eieio-xml" "\
Write object THIS out to the current stream as XML.
  If optional COMMENT is non-nil, include comments when outputting
this object.
@todo - support arbitrary schema output

\(fn (THIS eieio-default-superclass) &optional COMMENT)" nil nil)

;;;***

;;;### (autoloads (enable-visual-studio-bookmarks) "linemark" "linemark.el"
;;;;;;  (18810 50577))
;;; Generated autoloads from linemark.el

(autoload 'enable-visual-studio-bookmarks "linemark" "\
Bind the viss bookmark functions to F2 related keys.
\\<global-map>
\\[viss-bookmark-toggle]     - To=ggle a bookmark on this line.
\\[viss-bookmark-next-buffer]   - Move to the next bookmark.
\\[viss-bookmark-prev-buffer]   - Move to the previous bookmark.
\\[viss-bookmark-clear-all-buffer] - Clear all bookmarks.

\(fn)" t nil)

;;;***

;;;### (autoloads (lmcompile-do-highlight) "lmcompile" "lmcompile.el"
;;;;;;  (18810 50577))
;;; Generated autoloads from lmcompile.el

(autoload 'lmcompile-do-highlight "lmcompile" "\
Do compilation mode highlighting.
Works on grep, compile, or other type mode.

\(fn)" t nil)

;;;***

;;;### (autoloads (directory-tree-thing eieio-class-tree tree-test-it-all)
;;;;;;  "tree" "tree.el" (18668 41327))
;;; Generated autoloads from tree.el

(autoload 'tree-test-it-all "tree" "\
Try using various features of tree mode in a demo of it's display.

\(fn)" t nil)

(autoload 'eieio-class-tree "tree" "\
Displays a class tree using the TREE package in another buffer.
Optional argument ROOT-CLASS is the starting point.

\(fn &optional ROOT-CLASS)" t nil)

(autoload 'directory-tree-thing "tree" "\
Start at the current directory, and build a giant tree of files.
Argument PPATH is the path to the directory we are going to analyze.

\(fn PPATH)" t nil)

;;;***

;;;### (autoloads nil nil ("dbif-browse.el" "dbif-edit.el" "dbif.el"
;;;;;;  "dialog-mode.el" "dialog-tree.el" "dlg-class.el" "dlg-config.el"
;;;;;;  "e-config.el" "ecfg-menu.el" "eieio-base.el" "eieio-comp.el"
;;;;;;  "eieio-custom.el" "eieio-doc.el" "eieio-load.el" "eieio-speedbar.el"
;;;;;;  "eieio-test-methodinvoke.el" "eieio-tests.el" "eieio.el"
;;;;;;  "eieiocomp.el" "psql.el" "widget-d.el" "widget-i.el") (18821
;;;;;;  32657 187190))

;;;***

(provide 'eieio-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eieio-loaddefs.el ends here
