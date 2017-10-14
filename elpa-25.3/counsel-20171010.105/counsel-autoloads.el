;;; counsel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "counsel" "counsel.el" (23009 21818 0 0))
;;; Generated autoloads from counsel.el

(autoload 'counsel-el "counsel" "\
Elisp completion at point.

\(fn)" t nil)

(autoload 'counsel-cl "counsel" "\
Common Lisp completion at point.

\(fn)" t nil)

(autoload 'counsel-clj "counsel" "\
Clojure completion at point.

\(fn)" t nil)

(autoload 'counsel-unicode-char "counsel" "\
Insert COUNT copies of a Unicode character at point.
COUNT defaults to 1.

\(fn &optional COUNT)" t nil)

(autoload 'counsel-describe-variable "counsel" "\
Forward to `describe-variable'.

Variables declared using `defcustom' are highlighted according to
`ivy-highlight-face'.

\(fn)" t nil)

(autoload 'counsel-describe-function "counsel" "\
Forward to `describe-function'.

Interactive functions (i.e., commands) are highlighted according
to `ivy-highlight-face'.

\(fn)" t nil)

(autoload 'counsel-set-variable "counsel" "\
Set a variable, with completion.

When the selected variable is a `defcustom' with the type boolean
or radio, offer completion of all possible values.

Otherwise, offer a variant of `eval-expression', with the initial
input corresponding to the chosen variable.

\(fn)" t nil)

(autoload 'counsel-info-lookup-symbol "counsel" "\
Forward to (`info-lookup-symbol' SYMBOL MODE) with ivy completion.

\(fn SYMBOL &optional MODE)" t nil)

(autoload 'counsel-file-register "counsel" "\
Search file in register.

You cannot use Emacs' normal register commands to create file
registers.  Instead you must use the `set-register' function like
so: `(set-register ?i \"/home/eric/.emacs.d/init.el\")'.  Now you
can use `C-x r j i' to open that file.

\(fn)" t nil)

(autoload 'counsel-bookmark "counsel" "\
Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist.

\(fn)" t nil)

(autoload 'counsel-M-x "counsel" "\
Ivy version of `execute-extended-command'.
Optional INITIAL-INPUT is the initial input in the minibuffer.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-load-library "counsel" "\
Load a selected the Emacs Lisp library.
The libraries are offered from `load-path'.

\(fn)" t nil)

(autoload 'counsel-find-library "counsel" "\
Visit a selected the Emacs Lisp library.
The libraries are offered from `load-path'.

\(fn)" t nil)

(autoload 'counsel-load-theme "counsel" "\
Forward to `load-theme'.
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'.

\(fn)" t nil)

(autoload 'counsel-descbinds "counsel" "\
Show a list of all defined keys and their definitions.
If non-nil, show only bindings that start with PREFIX.
BUFFER defaults to the current one.

\(fn &optional PREFIX BUFFER)" t nil)

(autoload 'counsel-git "counsel" "\
Find file in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-git-grep "counsel" "\
Grep for a string in the current git repository.
When CMD is a string, use it as a \"git grep\" command.
When CMD is non-nil, prompt for a specific \"git grep\" command.
INITIAL-INPUT can be given as the initial minibuffer input.

\(fn &optional CMD INITIAL-INPUT)" t nil)

(autoload 'counsel-git-stash "counsel" "\
Search through all available git stashes.

\(fn)" t nil)

(autoload 'counsel-git-change-worktree "counsel" "\
Find the file corresponding to the current buffer on a different worktree.

\(fn)" t nil)

(autoload 'counsel-git-checkout "counsel" "\
Call the \"git checkout\" command.

\(fn)" t nil)

(autoload 'counsel-git-log "counsel" "\
Call the \"git log --grep\" shell command.

\(fn)" t nil)

(autoload 'counsel-find-file "counsel" "\
Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-recentf "counsel" "\
Find a file on `recentf-list'.

\(fn)" t nil)

(autoload 'counsel-locate "counsel" "\
Call the \"locate\" shell command.
INITIAL-INPUT can be given as the initial minibuffer input.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-dpkg "counsel" "\
Call the \"dpkg\" shell command.

\(fn)" t nil)

(autoload 'counsel-rpm "counsel" "\
Call the \"rpm\" shell command.

\(fn)" t nil)

(autoload 'counsel-file-jump "counsel" "\
Jump to a file below the current directory.
List all files within the current directory or any of its subdirectories.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil)

(autoload 'counsel-dired-jump "counsel" "\
Jump to a directory (in dired) below the current directory.
List all subdirectories within the current directory.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil)

(autoload 'counsel-ag "counsel" "\
Grep for a string in the current directory using ag.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS string, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-AG-ARGS AG-PROMPT)" t nil)

(autoload 'counsel-pt "counsel" "\
Grep for a string in the current directory using pt.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-pt-base-command' instead of
`counsel-ag-base-command'.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-ack "counsel" "\
Grep for a string in the current directory using ack.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-ack-base-command' replacing
`counsel-ag-base-command'.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-rg "counsel" "\
Grep for a string in the current directory using rg.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-RG-ARGS string, if non-nil, is appended to `counsel-rg-base-command'.
RG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-RG-ARGS RG-PROMPT)" t nil)

(autoload 'counsel-grep "counsel" "\
Grep for a string in the current file.

\(fn)" t nil)

(autoload 'counsel-grep-or-swiper "counsel" "\
Call `swiper' for small buffers and `counsel-grep' for large ones.

\(fn)" t nil)

(autoload 'counsel-org-tag "counsel" "\
Add or remove tags in `org-mode'.

\(fn)" t nil)

(autoload 'counsel-org-tag-agenda "counsel" "\
Set tags for the current agenda item.

\(fn)" t nil)

(autoload 'counsel-org-goto "counsel" "\
Go to a different location in the current file.

\(fn)" t nil)

(autoload 'counsel-org-goto-all "counsel" "\
Go to a different location in any org file.

\(fn)" t nil)

(autoload 'counsel-org-file "counsel" "\
Browse all attachments for current Org file.

\(fn)" t nil)

(autoload 'counsel-tmm "counsel" "\
Text-mode emulation of looking and choosing from a menubar.

\(fn)" t nil)

(autoload 'counsel-yank-pop "counsel" "\
Ivy replacement for `yank-pop'.

\(fn)" t nil)

(autoload 'counsel-imenu "counsel" "\
Jump to a buffer position indexed by imenu.

\(fn)" t nil)

(autoload 'counsel-list-processes "counsel" "\
Offer completion for `process-list'.
The default action deletes the selected process.
An extra action allows to switch to the process buffer.

\(fn)" t nil)

(autoload 'counsel-expression-history "counsel" "\
Select an element of `read-expression-history'.
And insert it into the minibuffer.  Useful during `eval-expression'.

\(fn)" t nil)

(autoload 'counsel-shell-command-history "counsel" "\
Browse shell command history.

\(fn)" t nil)

(autoload 'counsel-esh-history "counsel" "\
Browse Eshell history.

\(fn)" t nil)

(autoload 'counsel-shell-history "counsel" "\
Browse shell history.

\(fn)" t nil)

(autoload 'counsel-rhythmbox "counsel" "\
Choose a song from the Rhythmbox library to play or enqueue.

\(fn)" t nil)

(autoload 'counsel-linux-app "counsel" "\
Launch a Linux desktop application, similar to Alt-<F2>.

\(fn)" t nil)

(autoload 'counsel-company "counsel" "\
Complete using `company-candidates'.

\(fn)" t nil)

(autoload 'counsel-colors-emacs "counsel" "\
Show a list of all supported colors for a particular frame.

You can insert or kill the name or the hexadecimal rgb value of the
selected candidate.

\(fn)" t nil)

(autoload 'counsel-colors-web "counsel" "\
Show a list of all W3C web colors for use in CSS.

You can insert or kill the name or the hexadecimal rgb value of the
selected candidate.

\(fn)" t nil)

(autoload 'counsel-org-agenda-headlines "counsel" "\
Choose from headers of `org-mode' files in the agenda.

\(fn)" t nil)

(autoload 'counsel-irony "counsel" "\
Inline C/C++ completion using Irony.

\(fn)" t nil)

(autoload 'counsel-apropos "counsel" "\
Show all matching symbols.
See `apropos' for further information about what is considered
a symbol and how to search for them.

\(fn)" t nil)

(autoload 'counsel-switch-to-shell-buffer "counsel" "\
Switch to a shell buffer, or create one.

\(fn)" t nil)

(defvar counsel-mode nil "\
Non-nil if Counsel mode is enabled.
See the `counsel-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `counsel-mode'.")

(custom-autoload 'counsel-mode "counsel" nil)

(autoload 'counsel-mode "counsel" "\
Toggle Counsel mode on or off.
Turn Counsel mode on if ARG is positive, off otherwise. Counsel
mode remaps built-in emacs functions that have counsel
replacements. 

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; counsel-autoloads.el ends here
