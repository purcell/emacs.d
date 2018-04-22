;;; flycheck-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "flycheck" "flycheck.el" (23009 21811 0 0))
;;; Generated autoloads from flycheck.el

(autoload 'flycheck-manual "flycheck" "\
Open the Flycheck manual.

\(fn)" t nil)

(autoload 'flycheck-mode "flycheck" "\
Minor mode for on-the-fly syntax checking.

When called interactively, toggle `flycheck-mode'.  With prefix
ARG, enable `flycheck-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `flycheck-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `flycheck-mode'.
Otherwise behave as if called interactively.

In `flycheck-mode' the buffer is automatically syntax-checked
using the first suitable syntax checker from `flycheck-checkers'.
Use `flycheck-select-checker' to select a checker for the current
buffer manually.

\\{flycheck-mode-map}

\(fn &optional ARG)" t nil)

(defvar global-flycheck-mode nil "\
Non-nil if Global Flycheck mode is enabled.
See the `global-flycheck-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-flycheck-mode'.")

(custom-autoload 'global-flycheck-mode "flycheck" nil)

(autoload 'global-flycheck-mode "flycheck" "\
Toggle Flycheck mode in all buffers.
With prefix ARG, enable Global Flycheck mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Flycheck mode is enabled in all buffers where
`flycheck-mode-on-safe' would do it.
See `flycheck-mode' for more information on Flycheck mode.

\(fn &optional ARG)" t nil)

(autoload 'flycheck-define-error-level "flycheck" "\
Define a new error LEVEL with PROPERTIES.

The following PROPERTIES constitute an error level:

`:severity SEVERITY'
     A number denoting the severity of this level.  The higher
     the number, the more severe is this level compared to other
     levels.  Defaults to 0.

     The severity is used by `flycheck-error-level-<' to
     determine the ordering of errors according to their levels.

`:compilation-level LEVEL'

     A number indicating the broad class of messages that errors
     at this level belong to: one of 0 (info), 1 (warning), or
     2 or nil (error).  Defaults to nil.

     This is used by `flycheck-checker-pattern-to-error-regexp'
     to map error levels into `compilation-mode''s hierarchy and
     to get proper highlighting of errors in `compilation-mode'.

`:overlay-category CATEGORY'
     A symbol denoting the overlay category to use for error
     highlight overlays for this level.  See Info
     node `(elisp)Overlay Properties' for more information about
     overlay categories.

     A category for an error level overlay should at least define
     the `face' property, for error highlighting.  Another useful
     property for error level categories is `priority', to
     influence the stacking of multiple error level overlays.

`:fringe-bitmap BITMAP'
     A fringe bitmap symbol denoting the bitmap to use for fringe
     indicators for this level.  See Info node `(elisp)Fringe
     Bitmaps' for more information about fringe bitmaps,
     including a list of built-in fringe bitmaps.

`:fringe-face FACE'
     A face symbol denoting the face to use for fringe indicators
     for this level.

`:error-list-face FACE'
     A face symbol denoting the face to use for messages of this
     level in the error list.  See `flycheck-list-errors'.

\(fn LEVEL &rest PROPERTIES)" nil nil)

(function-put 'flycheck-define-error-level 'lisp-indent-function '1)

(autoload 'flycheck-define-command-checker "flycheck" "\
Define SYMBOL as syntax checker to run a command.

Define SYMBOL as generic syntax checker via
`flycheck-define-generic-checker', which uses an external command
to check the buffer.  SYMBOL and DOCSTRING are the same as for
`flycheck-define-generic-checker'.

In addition to the properties understood by
`flycheck-define-generic-checker', the following PROPERTIES
constitute a command syntax checker.  Unless otherwise noted, all
properties are mandatory.  Note that the default `:error-filter'
of command checkers is `flycheck-sanitize-errors'.

`:command COMMAND'
     The command to run for syntax checking.

     COMMAND is a list of the form `(EXECUTABLE [ARG ...])'.

     EXECUTABLE is a string with the executable of this syntax
     checker.  It can be overridden with the variable
     `flycheck-SYMBOL-executable'.  Note that this variable is
     NOT implicitly defined by this function.  Use
     `flycheck-def-executable-var' to define this variable.

     Each ARG is an argument to the executable, either as string,
     or as special symbol or form for
     `flycheck-substitute-argument', which see.

`:error-patterns PATTERNS'
     A list of patterns to parse the output of the `:command'.

     Each ITEM in PATTERNS is a list `(LEVEL SEXP ...)', where
     LEVEL is a Flycheck error level (see
     `flycheck-define-error-level'), followed by one or more RX
     `SEXP's which parse an error of that level and extract line,
     column, file name and the message.

     See `rx' for general information about RX, and
     `flycheck-rx-to-string' for some special RX forms provided
     by Flycheck.

     All patterns are applied in the order of declaration to the
     whole output of the syntax checker.  Output already matched
     by a pattern will not be matched by subsequent patterns.  In
     other words, the first pattern wins.

     This property is optional.  If omitted, however, an
     `:error-parser' is mandatory.

`:error-parser FUNCTION'
     A function to parse errors with.

     The function shall accept three arguments OUTPUT CHECKER
     BUFFER.  OUTPUT is the syntax checker output as string,
     CHECKER the syntax checker that was used, and BUFFER a
     buffer object representing the checked buffer.  The function
     must return a list of `flycheck-error' objects parsed from
     OUTPUT.

     This property is optional.  If omitted, it defaults to
     `flycheck-parse-with-patterns'.  In this case,
     `:error-patterns' is mandatory.

`:standard-input t'
     Whether to send the buffer contents on standard input.

     If this property is given and has a non-nil value, send the
     contents of the buffer on standard input.

     Defaults to nil.

Note that you may not give `:start', `:interrupt', and
`:print-doc' for a command checker.  You can give a custom
`:verify' function, though, whose results will be appended to the
default `:verify' function of command checkers.

\(fn SYMBOL DOCSTRING &rest PROPERTIES)" nil nil)

(function-put 'flycheck-define-command-checker 'lisp-indent-function '1)

(function-put 'flycheck-define-command-checker 'doc-string-elt '2)

(autoload 'flycheck-def-config-file-var "flycheck" "\
Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable variable using `defcustom', to
provide a configuration file for the given syntax CHECKER.
CUSTOM-ARGS are forwarded to `defcustom'.

FILE-NAME is the initial value of the new variable.  If omitted,
the default value is nil.

Use this together with the `config-file' form in the `:command'
argument to `flycheck-define-checker'.

\(fn SYMBOL CHECKER &optional FILE-NAME &rest CUSTOM-ARGS)" nil t)

(function-put 'flycheck-def-config-file-var 'lisp-indent-function '3)

(autoload 'flycheck-def-option-var "flycheck" "\
Define SYMBOL as option variable with INIT-VALUE for CHECKER.

SYMBOL is declared as customizable variable using `defcustom', to
provide an option for the given syntax CHECKERS (a checker or a
list of checkers).  INIT-VALUE is the initial value of the
variable, and DOCSTRING is its docstring.  CUSTOM-ARGS are
forwarded to `defcustom'.

Use this together with the `option', `option-list' and
`option-flag' forms in the `:command' argument to
`flycheck-define-checker'.

\(fn SYMBOL INIT-VALUE CHECKERS DOCSTRING &rest CUSTOM-ARGS)" nil t)

(function-put 'flycheck-def-option-var 'lisp-indent-function '3)

(function-put 'flycheck-def-option-var 'doc-string-elt '4)

;;;***

;;;### (autoloads nil nil ("flycheck-buttercup.el" "flycheck-ert.el"
;;;;;;  "flycheck-pkg.el") (23009 21811 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; flycheck-autoloads.el ends here
