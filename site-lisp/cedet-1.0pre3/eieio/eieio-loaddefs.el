;;; eieio-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (call-tree) "call-tree" "call-tree.el" (17091 25106))
;;; Generated autoloads from call-tree.el

(autoload (quote call-tree) "call-tree" "\
Build a call tree to show all functions called by FUNC." t nil)

;;;***

;;;### (autoloads (eieio-describe-generic eieio-describe-class eieio-browse)
;;;;;;  "eieio-opt" "eieio-opt.el" (17091 25106))
;;; Generated autoloads from eieio-opt.el

(autoload (quote eieio-browse) "eieio-opt" "\
Create an object browser window to show all objects.
If optional ROOT-CLASS, then start with that, otherwise start with
variable `eieio-default-superclass'." t nil)

(defalias (quote describe-class) (quote eieio-describe-class))

(autoload (quote eieio-describe-class) "eieio-opt" "\
Describe a CLASS defined by a string or symbol.
If CLASS is actually an object, then also display current values of that obect." t nil)

(defalias (quote describe-method) (quote eieio-describe-generic))

(defalias (quote describe-generic) (quote eieio-describe-generic))

(defalias (quote eieio-describe-method) (quote eieio-describe-generic))

(autoload (quote eieio-describe-generic) "eieio-opt" "\
Describe the generic function GENERIC.
Also extracts information about all methods specific to this generic." t nil)

;;;***

;;;### (autoloads (enable-visual-studio-bookmarks) "linemark" "linemark.el"
;;;;;;  (17091 25106))
;;; Generated autoloads from linemark.el

(autoload (quote enable-visual-studio-bookmarks) "linemark" "\
Bind the viss bookmark functions to F2 related keys.
\\<global-map>
\\[viss-bookmark-toggle]     - To=ggle a bookmark on this line.
\\[viss-bookmark-next-buffer]   - Move to the next bookmark.
\\[viss-bookmark-prev-buffer]   - Move to the previous bookmark.
\\[viss-bookmark-clear-all-buffer] - Clear all bookmarks." t nil)

;;;***

;;;### (autoloads (lmcompile-do-highlight) "lmcompile" "lmcompile.el"
;;;;;;  (17091 25106))
;;; Generated autoloads from lmcompile.el

(autoload (quote lmcompile-do-highlight) "lmcompile" "\
Do compilation mode highlighting.
Works on grep, compile, or other type mode." t nil)

;;;***

;;;### (autoloads (directory-tree-thing eieio-class-tree tree-test-it-all)
;;;;;;  "tree" "tree.el" (17091 25106))
;;; Generated autoloads from tree.el

(autoload (quote tree-test-it-all) "tree" "\
Try using various features of tree mode in a demo of it's display." t nil)

(autoload (quote eieio-class-tree) "tree" "\
Displays a class tree using the TREE package in another buffer.
Optional argument ROOT-CLASS is the starting point." t nil)

(autoload (quote directory-tree-thing) "tree" "\
Start at the current directory, and build a giant tree of files.
Argument PPATH is the path to the directory we are going to analyze." t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; eieio-loaddefs.el ends here
