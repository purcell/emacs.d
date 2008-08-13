;;; eieio-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (call-tree) "call-tree" "call-tree.el" (17213 40289))
;;; Generated autoloads from call-tree.el

(autoload (quote call-tree) "call-tree" "\
Build a call tree to show all functions called by FUNC.

\(fn FUNC)" t nil)

;;;***

;;;### (autoloads (data-debug-show data-debug-insert-object-button
;;;;;;  data-debug-insert-object-fields) "eieio-datadebug" "eieio-datadebug.el"
;;;;;;  (18411 2614))
;;; Generated autoloads from eieio-datadebug.el

(autoload (quote data-debug-insert-object-fields) "eieio-datadebug" "\
Insert all the fields of OBJECT.
PREFIX specifies what to insert at the start of each line.

\(fn OBJECT PREFIX)" nil nil)

(autoload (quote data-debug-insert-object-button) "eieio-datadebug" "\
Insert a button representing OBJECT.
PREFIX is the text that preceeds the button.
PREBUTTONTEXT is some text between PREFIX and the object button.

\(fn OBJECT PREFIX PREBUTTONTEXT)" nil nil)

(autoload (quote data-debug-show) "eieio-datadebug" "\
Run ddebug against any EIEIO object OBJ

\(fn (OBJ eieio-default-superclass))" nil nil)

;;;***

;;;### (autoloads (eieio-describe-generic eieio-build-class-alist
;;;;;;  eieio-describe-class eieio-browse) "eieio-opt" "eieio-opt.el"
;;;;;;  (18480 10421))
;;; Generated autoloads from eieio-opt.el

(autoload (quote eieio-browse) "eieio-opt" "\
Create an object browser window to show all objects.
If optional ROOT-CLASS, then start with that, otherwise start with
variable `eieio-default-superclass'.

\(fn &optional ROOT-CLASS)" t nil)

(defalias (quote describe-class) (quote eieio-describe-class))

(autoload (quote eieio-describe-class) "eieio-opt" "\
Describe a CLASS defined by a string or symbol.
If CLASS is actually an object, then also display current values of that obect.

\(fn CLASS)" t nil)

(autoload (quote eieio-build-class-alist) "eieio-opt" "\
Return an alist of all currently active classes for completion purposes.
Optional argument CLASS is the class to start with.
If INSTANTIABLE-ONLY is non nil, only allow names of classes which
are not abstract, otherwise allow all classes.
Optional argument BUILDLIST is more list to attach and is used internally.

\(fn &optional CLASS INSTANTIABLE-ONLY BUILDLIST)" nil nil)

(defalias (quote describe-method) (quote eieio-describe-generic))

(defalias (quote describe-generic) (quote eieio-describe-generic))

(defalias (quote eieio-describe-method) (quote eieio-describe-generic))

(autoload (quote eieio-describe-generic) "eieio-opt" "\
Describe the generic function GENERIC.
Also extracts information about all methods specific to this generic.

\(fn GENERIC)" t nil)

;;;***

;;;### (autoloads (object-write-xml) "eieio-xml" "eieio-xml.el" (18521
;;;;;;  48244))
;;; Generated autoloads from eieio-xml.el

(autoload (quote object-write-xml) "eieio-xml" "\
Write object THIS out to the current stream as XML.
  If optional COMMENT is non-nil, include comments when outputting
this object.
@todo - support arbitrary schema output

\(fn (THIS eieio-default-superclass) &optional COMMENT)" nil nil)

;;;***

;;;### (autoloads (enable-visual-studio-bookmarks) "linemark" "linemark.el"
;;;;;;  (17880 50543))
;;; Generated autoloads from linemark.el

(autoload (quote enable-visual-studio-bookmarks) "linemark" "\
Bind the viss bookmark functions to F2 related keys.
\\<global-map>
\\[viss-bookmark-toggle]     - To=ggle a bookmark on this line.
\\[viss-bookmark-next-buffer]   - Move to the next bookmark.
\\[viss-bookmark-prev-buffer]   - Move to the previous bookmark.
\\[viss-bookmark-clear-all-buffer] - Clear all bookmarks.

\(fn)" t nil)

;;;***

;;;### (autoloads (lmcompile-do-highlight) "lmcompile" "lmcompile.el"
;;;;;;  (17213 40340))
;;; Generated autoloads from lmcompile.el

(autoload (quote lmcompile-do-highlight) "lmcompile" "\
Do compilation mode highlighting.
Works on grep, compile, or other type mode.

\(fn)" t nil)

;;;***

;;;### (autoloads (directory-tree-thing eieio-class-tree tree-test-it-all)
;;;;;;  "tree" "tree.el" (17213 40344))
;;; Generated autoloads from tree.el

(autoload (quote tree-test-it-all) "tree" "\
Try using various features of tree mode in a demo of it's display.

\(fn)" t nil)

(autoload (quote eieio-class-tree) "tree" "\
Displays a class tree using the TREE package in another buffer.
Optional argument ROOT-CLASS is the starting point.

\(fn &optional ROOT-CLASS)" t nil)

(autoload (quote directory-tree-thing) "tree" "\
Start at the current directory, and build a giant tree of files.
Argument PPATH is the path to the directory we are going to analyze.

\(fn PPATH)" t nil)

;;;***

;;;### (autoloads nil nil ("chart.el" "dbif-browse.el" "dbif-edit.el"
;;;;;;  "dbif.el" "dialog-mode.el" "dialog-tree.el" "dlg-class.el"
;;;;;;  "dlg-config.el" "e-config.el" "ecfg-menu.el" "eieio-base.el"
;;;;;;  "eieio-comp.el" "eieio-custom.el" "eieio-doc.el" "eieio-load.el"
;;;;;;  "eieio-speedbar.el" "eieio-test-methodinvoke.el" "eieio-tests.el"
;;;;;;  "eieio.el" "eieiocomp.el" "psql.el" "widget-d.el" "widget-i.el")
;;;;;;  (18594 52818 741001))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; eieio-loaddefs.el ends here
