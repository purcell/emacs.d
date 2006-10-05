;;; cedet-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (cedet-update-autoloads) "cedet-autogen" "cedet-autogen.el"
;;;;;;  (17091 25106))
;;; Generated autoloads from cedet-autogen.el

(autoload (quote cedet-update-autoloads) "cedet-autogen" "\
Update autoloads in file LOADDEFS from sources.
Optional argument DIRECTORY, specifies the directory to scan for
autoloads.  It defaults to the current directory.
DIRECTORIES is a list of extra directory to scan.  Those directory
names are relative to DIRECTORY.  If DIRECTORIES is nil try to scan
sub directories of DIRECTORY where a `cedet-autogen-tagfile' file
exists." t nil)

;;;***

;;;### (autoloads (compare-strings) "cedet-compat" "cedet-compat.el"
;;;;;;  (17091 25106))
;;; Generated autoloads from cedet-compat.el

(autoload (quote compare-strings) "cedet-compat" "\
Compare the contents of two strings.
In string STR1, skip the first START1 characters and stop at END1.
In string STR2, skip the first START2 characters and stop at END2.
END1 and END2 default to the full lengths of the respective strings.

Case is significant in this comparison if IGNORE-CASE is nil.

The value is t if the strings (or specified portions) match.
If string STR1 is less, the value is a negative number N;
  - 1 - N is the number of characters that match at the beginning.
If string STR1 is greater, the value is a positive number N;
  N - 1 is the number of characters that match at the beginning." nil nil)

;;;***

;;;### (autoloads nil "cedet-edebug" "cedet-edebug.el" (17091 25106))
;;; Generated autoloads from cedet-edebug.el

(add-hook (quote edebug-setup-hook) (lambda nil (require (quote cedet-edebug)) (defalias (quote edebug-prin1-to-string) (quote cedet-edebug-prin1-to-string))))

;;;***

;;;### (autoloads (define-fame-channel) "fame" "fame.el" (17091 25106))
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
messages to CHANNEL." nil (quote macro))

;;;***

;;;### (autoloads (inversion-upgrade-package inversion-require) "inversion"
;;;;;;  "inversion.el" (17091 25106))
;;; Generated autoloads from inversion.el

(autoload (quote inversion-require) "inversion" "\
Declare that you need PACKAGE with at least VERSION.
PACKAGE might be found in FILE.  (See `require'.)
Throws an error if VERSION is incompatible with what is installed.
Optional argument DIRECTORY is a location where new versions of
this tool can be located.  If there is a versioning problem and
DIRECTORY is provided, inversion will offer to download the file.
Optional argument RESERVED is saved for later use." nil nil)

(autoload (quote inversion-upgrade-package) "inversion" "\
Try to upgrade PACKAGE in DIRECTORY is available." t nil)

;;;***

;;;### (autoloads (pprint-function pprint pprint-to-string) "pprint"
;;;;;;  "pprint.el" (17091 25106))
;;; Generated autoloads from pprint.el

(autoload (quote pprint-to-string) "pprint" "\
Return a string containing the pretty-printed representation of OBJECT.
OBJECT can be any Lisp object.  Quoting characters are used as needed
to make output that `read' can handle, whenever this is possible.  The
pretty printer try as much as possible to limit the length of lines to
given WIDTH.  WIDTH value defaults to `fill-column'." nil nil)

(autoload (quote pprint) "pprint" "\
Output the pretty-printed representation of OBJECT, any Lisp object.
Quoting characters are printed as needed to make output that `read'
can handle, whenever this is possible.  Output stream is STREAM, or
value of `standard-output' (which see).  The pretty printer try as
much as possible to limit the length of lines to given WIDTH.  WIDTH
value defaults to `fill-column'." nil nil)

(autoload (quote pprint-function) "pprint" "\
See a pretty-printed representation of FUNCTION-NAME." t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cedet-loaddefs.el ends here
