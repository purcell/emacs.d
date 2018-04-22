;;; ivy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ivy" "ivy.el" (23009 21814 0 0))
;;; Generated autoloads from ivy.el

(autoload 'ivy-resume "ivy" "\
Resume the last completion session.

\(fn)" t nil)

(autoload 'ivy-read "ivy" "\
Read a string in the minibuffer, with completion.

PROMPT is a format string, normally ending in a colon and a
space; %d anywhere in the string is replaced by the current
number of matching candidates.  For the literal % character,
escape it with %%. See also `ivy-count-format'.

COLLECTION is either a list of strings, a function, an alist, or
a hash table.

PREDICATE is applied to filter out the COLLECTION immediately.
This argument is for `completing-read' compat.

When REQUIRE-MATCH is non-nil, only members of COLLECTION can be
selected, i.e. custom text.

If INITIAL-INPUT is not nil, then insert that input in the
minibuffer initially.

HISTORY is a name of a variable to hold the completion session
history.

KEYMAP is composed with `ivy-minibuffer-map'.

If PRESELECT is not nil, then select the corresponding candidate
out of the ones that match the INITIAL-INPUT.

DEF is for compatibility with `completing-read'.

UPDATE-FN is called each time the current candidate(s) is changed.

When SORT is t, use `ivy-sort-functions-alist' for sorting.

ACTION is a lambda function to call after selecting a result.  It
takes a single string argument.

UNWIND is a lambda function to call before exiting.

RE-BUILDER is a lambda function to call to transform text into a
regex pattern.

MATCHER is to override matching.

DYNAMIC-COLLECTION is a boolean to specify if the list of
candidates is updated after each input by calling COLLECTION.

CALLER is a symbol to uniquely identify the caller to `ivy-read'.
It is used, along with COLLECTION, to determine which
customizations apply to the current completion session.

\(fn PROMPT COLLECTION &key PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY PRESELECT DEF KEYMAP UPDATE-FN SORT ACTION UNWIND RE-BUILDER MATCHER DYNAMIC-COLLECTION CALLER)" nil nil)

(autoload 'ivy-completing-read "ivy" "\
Read a string in the minibuffer, with completion.

This interface conforms to `completing-read' and can be used for
`completing-read-function'.

PROMPT is a string that normally ends in a colon and a space.
COLLECTION is either a list of strings, an alist, an obarray, or a hash table.
PREDICATE limits completion to a subset of COLLECTION.
REQUIRE-MATCH is a boolean value.  See `completing-read'.
INITIAL-INPUT is a string inserted into the minibuffer initially.
HISTORY is a list of previously selected inputs.
DEF is the default value.
INHERIT-INPUT-METHOD is currently ignored.

\(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY DEF INHERIT-INPUT-METHOD)" nil nil)

(defvar ivy-mode nil "\
Non-nil if Ivy mode is enabled.
See the `ivy-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-mode'.")

(custom-autoload 'ivy-mode "ivy" nil)

(autoload 'ivy-mode "ivy" "\
Toggle Ivy mode on or off.
Turn Ivy mode on if ARG is positive, off otherwise.
Turning on Ivy mode sets `completing-read-function' to
`ivy-completing-read'.

Global bindings:
\\{ivy-mode-map}

Minibuffer bindings:
\\{ivy-minibuffer-map}

\(fn &optional ARG)" t nil)

(autoload 'ivy-switch-buffer "ivy" "\
Switch to another buffer.

\(fn)" t nil)

(autoload 'ivy-switch-view "ivy" "\
Switch to one of the window views stored by `ivy-push-view'.

\(fn)" t nil)

(autoload 'ivy-switch-buffer-other-window "ivy" "\
Switch to another buffer in another window.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("colir.el" "ivy-overlay.el" "ivy-pkg.el")
;;;;;;  (23009 21814 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ivy-autoloads.el ends here
