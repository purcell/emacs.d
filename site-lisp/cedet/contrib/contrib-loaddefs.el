;;; contrib-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (eassist-list-methods eassist-switch-h-cpp) "eassist"
;;;;;;  "eassist.el" (18107 15282))
;;; Generated autoloads from eassist.el

(defvar eassist-header-switches (quote (("h" "cpp" "cc" "c") ("hpp" "cpp") ("cpp" "h" "hpp") ("c" "h") ("C" "H") ("H" "C" "CPP" "CC") ("cc" "h" "hpp"))) "\
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

;;;### (autoloads (semantic-default-matlab-setup) "semantic-matlab"
;;;;;;  "semantic-matlab.el" (17213 40186))
;;; Generated autoloads from semantic-matlab.el

(autoload (quote semantic-default-matlab-setup) "semantic-matlab" "\
Set up a buffer for parsing of MATLAB files.

\(fn)" nil nil)

(add-hook (quote matlab-mode-hook) (quote semantic-default-matlab-setup))

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

;;;### (autoloads nil nil ("cedet-contrib-load.el" "cedet-contrib.el"
;;;;;;  "semanticdb-javascript.el") (18110 13448 763983))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; contrib-loaddefs.el ends here
