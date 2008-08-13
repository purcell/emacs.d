;;; contrib-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (eassist-list-methods eassist-switch-h-cpp) "eassist"
;;;;;;  "eassist.el" (18368 33706))
;;; Generated autoloads from eassist.el

(defvar eassist-header-switches (quote (("h" "cpp" "cc" "c") ("hpp" "cpp" "cc") ("cpp" "h" "hpp") ("c" "h") ("C" "H") ("H" "C" "CPP" "CC") ("cc" "h" "hpp"))) "\
This variable defines possible switches for `eassist-switch-h-cpp' function.
Its format is list of (from . (to1 to2 to3...)) elements.  From and toN are
strings which are extentions of the files.")

(autoload (quote eassist-switch-h-cpp) "eassist" "\
Switch header and body file according to `eassist-header-switches' var.
The current buffer's file name extention is searched in
`eassist-header-switches' variable to find out extention for file's counterpart,
for example *.hpp <--> *.cpp.

\(fn)" t nil)

(autoload (quote eassist-list-methods) "eassist" "\
Show method/function list of current buffer in a newly created buffer.
This function is recommended to be bound to some convinient hotkey.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ede-gnustep" "ede-gnustep.el" (18586 20446))
;;; Generated autoloads from ede-gnustep.el

(add-to-list (quote ede-project-class-files) (ede-project-autoload "edegnustep" :name "GNUstep-Make" :file (quote ede-gnustep) :proj-file "ProjStep.ede" :load-type (quote ede-step-load) :class-sym (quote ede-step-project)) t)

(add-to-list (quote auto-mode-alist) (quote ("ProjStep\\.ede" . emacs-lisp-mode)))

;;;***

;;;### (autoloads (semantic-tag-folding-mode global-semantic-tag-folding-mode
;;;;;;  global-semantic-tag-folding-mode) "semantic-tag-folding"
;;;;;;  "semantic-tag-folding.el" (17213 40189))
;;; Generated autoloads from semantic-tag-folding.el

(defvar global-semantic-tag-folding-mode nil "\
*If non-nil enable global use of variable `semantic-tag-folding-mode'.
With this mode enabled, a new folding decoration mode is added.
Clicking on a + or - in the fringe will fold that tag.")

(custom-autoload (quote global-semantic-tag-folding-mode) "semantic-tag-folding" nil)

(autoload (quote global-semantic-tag-folding-mode) "semantic-tag-folding" "\
Toggle global use of option `semantic-tag-folding-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle.

\(fn &optional ARG)" t nil)

(autoload (quote semantic-tag-folding-mode) "semantic-tag-folding" "\
Minor mode mark semantic tags for folding.
This mode will display +/- icons in the fringe.  Clicking on them
will fold the current tag.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (wisent-csharp-default-setup) "wisent-csharp" "wisent-csharp.el"
;;;;;;  (17880 37474))
;;; Generated autoloads from wisent-csharp.el

(autoload (quote wisent-csharp-default-setup) "wisent-csharp" "\
Not documented

\(fn)" nil nil)

(add-hook (quote csharp-mode-hook) (function wisent-csharp-default-setup))

;;;***

;;;### (autoloads (wisent-javascript-setup-parser) "wisent-javascript"
;;;;;;  "wisent-javascript.el" (17213 40199))
;;; Generated autoloads from wisent-javascript.el

(autoload (quote wisent-javascript-setup-parser) "wisent-javascript" "\
Setup buffer for parse.

\(fn)" nil nil)

(add-hook (quote javascript-mode-hook) (quote wisent-javascript-setup-parser))

(add-hook (quote ecmascript-mode-hook) (quote wisent-javascript-setup-parser))

;;;***

;;;### (autoloads (wisent-php-default-setup) "wisent-php" "wisent-php.el"
;;;;;;  (18540 11866))
;;; Generated autoloads from wisent-php.el

(autoload (quote wisent-php-default-setup) "wisent-php" "\
Hook run to setup Semantic in `php-mode'.
Use the alternate LALR(1) parser.

\(fn)" nil nil)

(add-hook (quote php-mode-hook) (function wisent-php-default-setup))

;;;***

;;;### (autoloads nil nil ("cedet-contrib-load.el" "cedet-contrib.el"
;;;;;;  "semantic-matlab.el" "semanticdb-javascript.el" "wisent-csharp-wy.el"
;;;;;;  "wisent-javascript-jv-wy.el" "wisent-php-wy.el" "wisent-ruby-wy.el"
;;;;;;  "wisent-ruby.el") (18594 52859 994106))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; contrib-loaddefs.el ends here
