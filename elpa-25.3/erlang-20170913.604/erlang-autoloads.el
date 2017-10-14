;;; erlang-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "erlang" "erlang.el" (23009 23263 0 0))
;;; Generated autoloads from erlang.el

(autoload 'erlang-mode "erlang" "\
Major mode for editing Erlang source files in Emacs.
It knows about syntax and comment, it can indent code, it is capable
of fontifying the source file, the TAGS commands are aware of Erlang
modules, and the Erlang man pages can be accessed.

Should this module, \"erlang.el\", be installed properly, Erlang mode
is activated whenever an Erlang source or header file is loaded into
Emacs.  To indicate this, the mode line should contain the word
\"Erlang\".

The main feature of Erlang mode is indentation, press TAB and the
current line will be indented correctly.

Comments starting with only one `%' are indented to the column stored
in the variable `comment-column'.  Comments starting with two `%':s
are indented with the same indentation as code.  Comments starting
with at least three `%':s are indented to the first column.

However, Erlang mode contains much more, this is a list of the most
useful commands:
     TAB     - Indent the line.
     C-c C-q - Indent current function.
     M-;     - Create a comment at the end of the line.
     M-q     - Fill a comment, i.e. wrap lines so that they (hopefully)
                 will look better.
     M-a     - Goto the beginning of an Erlang clause.
     M-C-a   - Ditto for function.
     M-e     - Goto the end of an Erlang clause.
     M-C-e   - Ditto for function.
     M-h     - Mark current Erlang clause.
     M-C-h   - Ditto for function.
     C-c C-z - Start, or switch to, an inferior Erlang shell.
     C-c C-k - Compile current file.
     C-x `   - Next error.
     ,       - Electric comma.
     ;       - Electric semicolon.

Erlang mode check the name of the file against the module name when
saving, whenever a mismatch occurs Erlang mode offers to modify the
source.

The variable `erlang-electric-commands' controls the electric
commands.  To deactivate all of them, set it to nil.

There exists a large number of commands and variables in the Erlang
module.  Please press `M-x apropos RET erlang RET' to see a complete
list.  Press `C-h f name-of-function RET' and `C-h v name-of-variable
RET'to see the full description of functions and variables,
respectively.

On entry to this mode the contents of the hook `erlang-mode-hook' is
executed.

Please see the beginning of the file `erlang.el' for more information
and examples of hooks.

Other commands:
\\{erlang-mode-map}

\(fn)" t nil)

(dolist (r '("\\.erl$" "\\.app\\.src$" "\\.escript" "\\.hrl$" "\\.xrl$" "\\.yrl" "/ebin/.+\\.app")) (add-to-list 'auto-mode-alist (cons r 'erlang-mode)))

(autoload 'erlang-find-tag "erlang" "\
Like `find-tag'.  Capable of retrieving Erlang modules.

Tags can be given on the forms `tag', `module:', `module:tag'.

\(fn MODTAGNAME &optional NEXT-P REGEXP-P)" t nil)

(autoload 'erlang-find-tag-other-window "erlang" "\
Like `find-tag-other-window' but aware of Erlang modules.

\(fn TAGNAME &optional NEXT-P REGEXP-P)" t nil)

(autoload 'erlang-shell "erlang" "\
Start a new Erlang shell.

The variable `erlang-shell-function' decides which method to use,
default is to start a new Erlang host.  It is possible that, in the
future, a new shell on an already running host will be started.

\(fn)" t nil)
 (autoload 'run-erlang "erlang" "Start a new Erlang shell." t)

(autoload 'erlang-compile "erlang" "\
Compile Erlang module in current buffer.

\(fn)" t nil)

(autoload 'inferior-erlang "erlang" "\
Run an inferior Erlang.
With prefix command, prompt for command to start Erlang with.

This is just like running Erlang in a normal shell, except that
an Emacs buffer is used for input and output.
\\<comint-mode-map>
The command line history can be accessed with  \\[comint-previous-input]  and  \\[comint-next-input].
The history is saved between sessions.

Entry to this mode calls the functions in the variables
`comint-mode-hook' and `erlang-shell-mode-hook' with no arguments.

The following commands imitate the usual Unix interrupt and
editing control characters:
\\{erlang-shell-mode-map}

\(fn &optional COMMAND)" t nil)

;;;***

;;;### (autoloads nil "erlang-edoc" "erlang-edoc.el" (23009 23263
;;;;;;  0 0))
;;; Generated autoloads from erlang-edoc.el

(autoload 'erlang-edoc-mode "erlang-edoc" "\
Toggle Erlang-Edoc mode on or off.
With a prefix argument ARG, enable Erlang-Edoc mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{erlang-edoc-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "erlang-start" "erlang-start.el" (23009 23263
;;;;;;  0 0))
;;; Generated autoloads from erlang-start.el

(let ((a '("\\.erl\\'" . erlang-mode)) (b '("\\.hrl\\'" . erlang-mode))) (or (assoc (car a) auto-mode-alist) (setq auto-mode-alist (cons a auto-mode-alist))) (or (assoc (car b) auto-mode-alist) (setq auto-mode-alist (cons b auto-mode-alist))))

(add-to-list 'interpreter-mode-alist (cons "escript" 'erlang-mode))

(let ((erl-ext '(".jam" ".vee" ".beam"))) (while erl-ext (add-to-list 'completion-ignored-extensions (car erl-ext)) (when (boundp 'dired-omit-extensions) (add-to-list 'dired-omit-extensions (car erl-ext))) (setq erl-ext (cdr erl-ext))))

;;;***

;;;### (autoloads nil "erldoc" "erldoc.el" (23009 23263 0 0))
;;; Generated autoloads from erldoc.el

(autoload 'erldoc-browse "erldoc" "\


\(fn MFA)" t nil)

(autoload 'erldoc-apropos "erldoc" "\


\(fn PATTERN)" t nil)

(autoload 'erldoc-eldoc-function "erldoc" "\
A function suitable for `eldoc-documentation-function'.

\(fn)" nil nil)

(autoload 'erldoc-browse-topic "erldoc" "\


\(fn TOPIC)" t nil)

;;;***

;;;### (autoloads nil nil ("erlang-eunit.el" "erlang-flymake.el"
;;;;;;  "erlang-pkg.el" "erlang-skels-old.el" "erlang-skels.el" "erlang-test.el")
;;;;;;  (23009 23263 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; erlang-autoloads.el ends here
