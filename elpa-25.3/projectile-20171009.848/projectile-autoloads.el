;;; projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "projectile" "projectile.el" (23009 21819 0
;;;;;;  0))
;;; Generated autoloads from projectile.el

(autoload 'projectile-version "projectile" "\
Get the Projectile version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

\(fn &optional SHOW-VERSION)" t nil)

(autoload 'projectile-invalidate-cache "projectile" "\
Remove the current project's files from `projectile-projects-cache'.

With a prefix argument ARG prompts for the name of the project whose cache
to invalidate.

\(fn ARG)" t nil)

(autoload 'projectile-purge-file-from-cache "projectile" "\
Purge FILE from the cache of the current project.

\(fn FILE)" t nil)

(autoload 'projectile-purge-dir-from-cache "projectile" "\
Purge DIR from the cache of the current project.

\(fn DIR)" t nil)

(autoload 'projectile-cache-current-file "projectile" "\
Add the currently visited file to the cache.

\(fn)" t nil)

(autoload 'projectile-discover-projects-in-directory "projectile" "\
Discover any projects in DIRECTORY and add them to the projectile cache.
This function is not recursive and only adds projects with roots
at the top level of DIRECTORY.

\(fn DIRECTORY)" t nil)

(autoload 'projectile-switch-to-buffer "projectile" "\
Switch to a project buffer.

\(fn)" t nil)

(autoload 'projectile-switch-to-buffer-other-window "projectile" "\
Switch to a project buffer and show it in another window.

\(fn)" t nil)

(autoload 'projectile-switch-to-buffer-other-frame "projectile" "\
Switch to a project buffer and show it in another window.

\(fn)" t nil)

(autoload 'projectile-display-buffer "projectile" "\
Display a project buffer in another window without selecting it.

\(fn)" t nil)

(autoload 'projectile-project-buffers-other-buffer "projectile" "\
Switch to the most recently selected buffer project buffer.
Only buffers not visible in windows are returned.

\(fn)" t nil)

(autoload 'projectile-multi-occur "projectile" "\
Do a `multi-occur' in the project's buffers.
With a prefix argument, show NLINES of context.

\(fn &optional NLINES)" t nil)

(autoload 'projectile-find-other-file "projectile" "\
Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

\(fn &optional FLEX-MATCHING)" t nil)

(autoload 'projectile-find-other-file-other-window "projectile" "\
Switch between files with the same name but different extensions in other window.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

\(fn &optional FLEX-MATCHING)" t nil)

(autoload 'projectile-find-other-file-other-frame "projectile" "\
Switch between files with the same name but different extensions in other window.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

\(fn &optional FLEX-MATCHING)" t nil)

(autoload 'projectile-find-file-dwim "projectile" "\
Jump to a project's files using completion based on context.

With a prefix ARG invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim' still switches to \"projectile/projectile.el\" immediately
 because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename like
 \"projectile/a\", a list of files with character 'a' in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting.

\(fn &optional ARG)" t nil)

(autoload 'projectile-find-file-dwim-other-window "projectile" "\
Jump to a project's files using completion based on context in other window.

With a prefix ARG invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-window' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim-other-window' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting.

\(fn &optional ARG)" t nil)

(autoload 'projectile-find-file-dwim-other-frame "projectile" "\
Jump to a project's files using completion based on context in other frame.

With a prefix ARG invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-frame' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim-other-frame' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting.

\(fn &optional ARG)" t nil)

(autoload 'projectile-find-file "projectile" "\
Jump to a project's file using completion.
With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(autoload 'projectile-find-file-other-window "projectile" "\
Jump to a project's file using completion and show it in another window.

With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(autoload 'projectile-find-file-other-frame "projectile" "\
Jump to a project's file using completion and show it in another frame.

With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(autoload 'projectile-find-dir "projectile" "\
Jump to a project's directory using completion.

With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(autoload 'projectile-find-dir-other-window "projectile" "\
Jump to a project's directory in other window using completion.

With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(autoload 'projectile-find-dir-other-frame "projectile" "\
Jump to a project's directory in other window using completion.

With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(autoload 'projectile-find-test-file "projectile" "\
Jump to a project's test file using completion.

With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(autoload 'projectile-project-info "projectile" "\
Display info for current project.

\(fn)" t nil)

(autoload 'projectile-find-implementation-or-test-other-window "projectile" "\
Open matching implementation or test file in other window.

\(fn)" t nil)

(autoload 'projectile-find-implementation-or-test-other-frame "projectile" "\
Open matching implementation or test file in other frame.

\(fn)" t nil)

(autoload 'projectile-toggle-between-implementation-and-test "projectile" "\
Toggle between an implementation file and its test file.

\(fn)" t nil)

(autoload 'projectile-grep "projectile" "\
Perform rgrep in the project.

With a prefix ARG asks for files (globbing-aware) which to grep in.
With prefix ARG of `-' (such as `M--'), default the files (without prompt),
to `projectile-grep-default-files'.

With REGEXP given, don't query the user for a regexp.

\(fn &optional REGEXP ARG)" t nil)

(autoload 'projectile-ag "projectile" "\
Run an ag search with SEARCH-TERM in the project.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

\(fn SEARCH-TERM &optional ARG)" t nil)

(autoload 'projectile-regenerate-tags "projectile" "\
Regenerate the project's [e|g]tags.

\(fn)" t nil)

(autoload 'projectile-find-tag "projectile" "\
Find tag in project.

\(fn)" t nil)

(autoload 'projectile-run-command-in-root "projectile" "\
Invoke `execute-extended-command' in the project's root.

\(fn)" t nil)

(autoload 'projectile-run-shell-command-in-root "projectile" "\
Invoke `shell-command' in the project's root.

\(fn)" t nil)

(autoload 'projectile-run-async-shell-command-in-root "projectile" "\
Invoke `async-shell-command' in the project's root.

\(fn)" t nil)

(autoload 'projectile-run-shell "projectile" "\
Invoke `shell' in the project's root.

\(fn)" t nil)

(autoload 'projectile-run-eshell "projectile" "\
Invoke `eshell' in the project's root.

\(fn)" t nil)

(autoload 'projectile-run-term "projectile" "\
Invoke `term' in the project's root.

\(fn PROGRAM)" t nil)

(autoload 'projectile-replace "projectile" "\
Replace literal string in project using non-regexp `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement.

\(fn &optional ARG)" t nil)

(autoload 'projectile-replace-regexp "projectile" "\
Replace a regexp in the project using `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement.

\(fn &optional ARG)" t nil)

(autoload 'projectile-kill-buffers "projectile" "\
Kill all project buffers.

\(fn)" t nil)

(autoload 'projectile-save-project-buffers "projectile" "\
Save all project buffers.

\(fn)" t nil)

(autoload 'projectile-dired "projectile" "\
Open `dired' at the root of the project.

\(fn)" t nil)

(autoload 'projectile-dired-other-window "projectile" "\
Open `dired'  at the root of the project in another window.

\(fn)" t nil)

(autoload 'projectile-dired-other-frame "projectile" "\
Open `dired' at the root of the project in another frame.

\(fn)" t nil)

(autoload 'projectile-vc "projectile" "\
Open `vc-dir' at the root of the project.

For git projects `magit-status-internal' is used if available.
For hg projects `monky-status' is used if available.

\(fn &optional PROJECT-ROOT)" t nil)

(autoload 'projectile-recentf "projectile" "\
Show a list of recently visited files in a project.

\(fn)" t nil)

(autoload 'projectile-compile-project "projectile" "\
Run project compilation command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

\(fn ARG &optional DIR)" t nil)

(autoload 'projectile-test-project "projectile" "\
Run project test command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

\(fn ARG)" t nil)

(autoload 'projectile-run-project "projectile" "\
Run project run command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

\(fn ARG)" t nil)

(autoload 'projectile-switch-project "projectile" "\
Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'

\(fn &optional ARG)" t nil)

(autoload 'projectile-switch-open-project "projectile" "\
Switch to a project we have currently opened.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'

\(fn &optional ARG)" t nil)

(autoload 'projectile-find-file-in-directory "projectile" "\
Jump to a file in a (maybe regular) DIRECTORY.

This command will first prompt for the directory the file is in.

\(fn &optional DIRECTORY)" t nil)

(autoload 'projectile-find-file-in-known-projects "projectile" "\
Jump to a file in any of the known projects.

\(fn)" t nil)

(autoload 'projectile-cleanup-known-projects "projectile" "\
Remove known projects that don't exist anymore.

\(fn)" t nil)

(autoload 'projectile-clear-known-projects "projectile" "\
Clear both `projectile-known-projects' and `projectile-known-projects-file'.

\(fn)" t nil)

(autoload 'projectile-remove-known-project "projectile" "\
Remove PROJECT from the list of known projects.

\(fn &optional PROJECT)" t nil)

(autoload 'projectile-remove-current-project-from-known-projects "projectile" "\
Remove the current project from the list of known projects.

\(fn)" t nil)

(autoload 'projectile-ibuffer "projectile" "\
Open an IBuffer window showing all buffers in the current project.

Let user choose another project when PREFIX is supplied.

\(fn PREFIX)" t nil)

(autoload 'projectile-commander "projectile" "\
Execute a Projectile command with a single letter.
The user is prompted for a single character indicating the action to invoke.
The `?' character describes then
available actions.

See `def-projectile-commander-method' for defining new methods.

\(fn)" t nil)

(autoload 'projectile-edit-dir-locals "projectile" "\
Edit or create a .dir-locals.el file of the project.

\(fn)" t nil)

(defvar projectile-mode-line '(:eval (format " Projectile[%s(%s)]" (projectile-project-name) (projectile-project-type))) "\
Mode line lighter for Projectile.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
details about mode line templates.

Customize this variable to change how Projectile displays its
status in the mode line.  The default value displays the project
name and type.  Set this variable to nil to disable the mode line
entirely.")

(custom-autoload 'projectile-mode-line "projectile" t)

(defvar projectile-mode nil "\
Non-nil if Projectile mode is enabled.
See the `projectile-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `projectile-mode'.")

(custom-autoload 'projectile-mode "projectile" nil)

(autoload 'projectile-mode "projectile" "\
Minor mode to assist project management and navigation.

When called interactively, toggle `projectile-mode'.  With prefix
ARG, enable `projectile-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `projectile-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `projectile-mode'.
Otherwise behave as if called interactively.

\\{projectile-mode-map}

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'projectile-global-mode 'projectile-mode)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; projectile-autoloads.el ends here
