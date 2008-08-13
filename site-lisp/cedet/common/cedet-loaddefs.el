;;; cedet-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (cedet-update-autoloads) "cedet-autogen" "cedet-autogen.el"
;;;;;;  (18540 13339))
;;; Generated autoloads from cedet-autogen.el

(autoload (quote cedet-update-autoloads) "cedet-autogen" "\
Update autoloads in file LOADDEFS from sources.
Optional argument DIRECTORY, specifies the directory to scan for
autoloads.  It defaults to the current directory.
DIRECTORIES is a list of extra directory to scan.  Those directory
names are relative to DIRECTORY.  If DIRECTORIES is nil try to scan
sub directories of DIRECTORY where a `cedet-autogen-tagfile' file
exists.

\(fn LOADDEFS &optional DIRECTORY &rest DIRECTORIES)" t nil)

;;;***

;;;### (autoloads nil "cedet-edebug" "cedet-edebug.el" (18411 2781))
;;; Generated autoloads from cedet-edebug.el

(add-hook (quote edebug-setup-hook) (lambda nil (require (quote cedet-edebug)) (defalias (quote edebug-prin1-to-string) (quote cedet-edebug-prin1-to-string)) (define-key edebug-mode-map "A" (quote data-debug-edebug-expr))))

(add-hook (quote debugger-mode-hook) (lambda nil (require (quote cedet-edebug)) (define-key debugger-mode-map "A" (quote data-debug-edebug-expr))))

;;;***

;;;### (autoloads (data-debug-edebug-expr data-debug-new-buffer data-debug-mode
;;;;;;  data-debug-insert-thing data-debug-insert-stuff-list data-debug-insert-property-list)
;;;;;;  "data-debug" "data-debug.el" (18521 47213))
;;; Generated autoloads from data-debug.el

(autoload (quote data-debug-insert-property-list) "data-debug" "\
Insert the property list PROPLIST.
Each line starts with PREFIX.
The attributes belong to the tag PARENT.

\(fn PROPLIST PREFIX &optional PARENT)" nil nil)

(autoload (quote data-debug-insert-stuff-list) "data-debug" "\
Insert all the parts of STUFFLIST.
PREFIX specifies what to insert at the start of each line.

\(fn STUFFLIST PREFIX)" nil nil)

(autoload (quote data-debug-insert-thing) "data-debug" "\
Insert THING with PREFIX.
PREBUTTONTEXT is some text to insert between prefix and the thing
that is not included in the indentation calculation of any children.
If PARENT is non-nil, it is somehow related as a parent to thing.

\(fn THING PREFIX PREBUTTONTEXT &optional PARENT)" nil nil)

(autoload (quote data-debug-mode) "data-debug" "\
Major-mode for the Analyzer debugger.

\\{data-debug-map}

\(fn)" t nil)

(autoload (quote data-debug-new-buffer) "data-debug" "\
Create a new ddebug buffer with NAME.

\(fn NAME)" nil nil)

(autoload (quote data-debug-edebug-expr) "data-debug" "\
Dump out the contets of some expression EXPR in edebug with ddebug.

\(fn EXPR)" t nil)

;;;***

;;;### (autoloads (define-fame-channel) "fame" "fame.el" (17213 39681))
;;; Generated autoloads from fame.el

(autoload (quote define-fame-channel) "fame" "\
Define the new message channel CHANNEL.
CHANNEL must be a non-nil symbol.
The optional argument DEFAULT specifies the default value of message
levels for this channel.  By default it is the value of
`fame-default-level-values'.
DOCSTRING is an optional channel documentation.

This defines the option `CHANNEL-fame-levels' to customize the current
value of message levels.  And the functions `CHANNEL-send-debug',
`CHANNEL-send-info', `CHANNEL-send-warning', and `CHANNEL-send-error',
that respectively send debug, informational, warning, and error
messages to CHANNEL.

\(fn CHANNEL &optional DEFAULT DOCSTRING)" nil (quote macro))

;;;***

;;;### (autoloads (inversion-upgrade-package inversion-add-to-load-path
;;;;;;  inversion-find-version inversion-require-emacs inversion-require)
;;;;;;  "inversion" "inversion.el" (18113 61728))
;;; Generated autoloads from inversion.el

(autoload (quote inversion-require) "inversion" "\
Declare that you need PACKAGE with at least VERSION.
PACKAGE might be found in FILE.  (See `require'.)
Throws an error if VERSION is incompatible with what is installed.
Optional argument DIRECTORY is a location where new versions of
this tool can be located.  If there is a versioning problem and
DIRECTORY is provided, inversion will offer to download the file.
Optional argument RESERVED is saved for later use.

\(fn PACKAGE VERSION &optional FILE DIRECTORY &rest RESERVED)" nil nil)

(autoload (quote inversion-require-emacs) "inversion" "\
Declare that you need either EMACS-VER, or XEMACS-VER.
Only checks one based on which kind of Emacs is being run.

\(fn EMACS-VER XEMACS-VER)" nil nil)

(autoload (quote inversion-find-version) "inversion" "\
Search for the version and incompatible version of PACKAGE.
Does not load PACKAGE nor requires that it has been previously loaded.
Search in the directories in `load-path' for a PACKAGE.el library.
Visit the file found and search for the declarations of variables or
constants `PACKAGE-version' and `PACKAGE-incompatible-version'.  The
value of these variables must be a version string.

Return a pair (VERSION-STRING . INCOMPATIBLE-VERSION-STRING) where
INCOMPATIBLE-VERSION-STRING can be nil.
Return nil when VERSION-STRING was not found.

\(fn PACKAGE)" nil nil)

(autoload (quote inversion-add-to-load-path) "inversion" "\
Add the PACKAGE path to `load-path' if necessary.
MINIMUM is the minimum version requirement of PACKAGE.
Optional argument INSTALLDIR is the base directory where PACKAGE is
installed.  It defaults to `default-directory'/PACKAGE.
SUBDIRS are sub-directories to add to `load-path', following the main
INSTALLDIR path.

\(fn PACKAGE MINIMUM &optional INSTALLDIR &rest SUBDIRS)" nil nil)

(autoload (quote inversion-upgrade-package) "inversion" "\
Try to upgrade PACKAGE in DIRECTORY is available.

\(fn PACKAGE &optional DIRECTORY)" t nil)

;;;***

;;;### (autoloads (mode-local-read-function) "mode-local" "mode-local.el"
;;;;;;  (18478 55016))
;;; Generated autoloads from mode-local.el

(autoload (quote mode-local-read-function) "mode-local" "\
Interactively read in the name of a mode-local function.
PROMPT, INITIAL, HIST, and DEFAULT are the same as for `completing-read'.

\(fn PROMPT &optional INITIAL HIST DEFAULT)" nil nil)

;;;***

;;;### (autoloads (pprint-function pprint pprint-to-string) "pprint"
;;;;;;  "pprint.el" (17213 39693))
;;; Generated autoloads from pprint.el

(autoload (quote pprint-to-string) "pprint" "\
Return a string containing the pretty-printed representation of OBJECT.
OBJECT can be any Lisp object.  Quoting characters are used as needed
to make output that `read' can handle, whenever this is possible.  The
pretty printer try as much as possible to limit the length of lines to
given WIDTH.  WIDTH value defaults to `fill-column'.

\(fn OBJECT &optional WIDTH)" nil nil)

(autoload (quote pprint) "pprint" "\
Output the pretty-printed representation of OBJECT, any Lisp object.
Quoting characters are printed as needed to make output that `read'
can handle, whenever this is possible.  Output stream is STREAM, or
value of `standard-output' (which see).  The pretty printer try as
much as possible to limit the length of lines to given WIDTH.  WIDTH
value defaults to `fill-column'.

\(fn OBJECT &optional STREAM WIDTH)" nil nil)

(autoload (quote pprint-function) "pprint" "\
See a pretty-printed representation of FUNCTION-NAME.

\(fn FUNCTION-NAME)" t nil)

;;;***

;;;### (autoloads (pulse-line-hook-function pulse-toggle-integration-advice
;;;;;;  pulse-momentary-highlight-region pulse-momentary-highlight-one-line
;;;;;;  pulse-momentary-highlight-overlay pulse) "pulse" "pulse.el"
;;;;;;  (18374 54119))
;;; Generated autoloads from pulse.el

(autoload (quote pulse) "pulse" "\
Pulse the colors on our highlight face.
If optional FACE is provide, reset the face to FACE color,
instead of `pulse-highlight-start-face'.
Be sure to call `pulse-reset-face' after calling pulse.

\(fn &optional FACE)" nil nil)

(autoload (quote pulse-momentary-highlight-overlay) "pulse" "\
Pulse the overlay O, unhighlighting before next command.
Optional argument FACE specifies the fact to do the highlighting.

\(fn O &optional FACE)" nil nil)

(autoload (quote pulse-momentary-highlight-one-line) "pulse" "\
Highlight the line around POINT, unhighlighting before next command.
Optional argument FACE specifies the face to do the highlighting.

\(fn POINT &optional FACE)" nil nil)

(autoload (quote pulse-momentary-highlight-region) "pulse" "\
Highlight between START and END, unhighlighting before next command.
Optional argument FACE specifies the fact to do the highlighting.

\(fn START END &optional FACE)" nil nil)

(autoload (quote pulse-toggle-integration-advice) "pulse" "\
Toggle activation of advised functions that will now pulse.
With a negative ARG, disable pulse advice.
With a positive ARG, enable pulse advice.
Currently advised functions include:
  `goto-line'
  `exchange-point-and-mark'
  `find-tag'
  `tags-search'
  `tags-loop-continue'
  `pop-tag-mark'
  `imenu-default-goto-function'
Pulsing via `pulse-line-hook-function' has also been added to
the following hook:
  `next-error-hook'

\(fn ARG)" t nil)

(autoload (quote pulse-line-hook-function) "pulse" "\
Function used in hooks to pulse the current line.
Only pulses the line if `pulse-command-advice-flag' is non-nil.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("cedet-compat.el" "cedet-files.el" "cedet-load.el"
;;;;;;  "cedet.el" "ezimage.el" "sformat.el" "working.el") (18594
;;;;;;  52815 811073))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cedet-loaddefs.el ends here
