;;; helm-help.el --- Help messages for Helm. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2017 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'helm)

(defvar helm-org-headings--nofilename)
(declare-function helm-source-org-headings-for-files "helm-org.el")


(defgroup helm-help nil
  "Embedded help for `helm'."
  :group 'helm)

(defface helm-helper
  '((t :inherit helm-header))
  "Face for Helm help string in minibuffer."
  :group 'helm-help)

(defcustom helm-documentation-file "~/.emacs.d/helm-doc.org"
  "The file where to save Helm documentation."
  :group 'helm-help
  :type 'string)

(defvar helm-help--string-list '(helm-help-message
                                 helm-buffer-help-message
                                 helm-ff-help-message
                                 helm-read-file-name-help-message
                                 helm-generic-file-help-message
                                 helm-grep-help-message
                                 helm-pdfgrep-help-message
                                 helm-etags-help-message
                                 helm-ucs-help-message
                                 helm-bookmark-help-message
                                 helm-esh-help-message
                                 helm-buffers-ido-virtual-help-message
                                 helm-moccur-help-message
                                 helm-top-help-message
                                 helm-apt-help-message
                                 helm-el-package-help-message
                                 helm-M-x-help-message
                                 helm-imenu-help-message
                                 helm-colors-help-message
                                 helm-semantic-help-message
                                 helm-kmacro-help-message))

(defvar helm-documentation-buffer-name "*helm documentation*")

;;;###autoload
(defun helm-documentation ()
  "Preconfigured Helm for Helm documentation.
With a prefix arg refresh the documentation.

Find here the documentation of all documented sources."
  (interactive)
  (require 'helm-org)
  (with-current-buffer (get-buffer-create helm-documentation-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cl-loop for elm in helm-help--string-list
               for str = (helm-interpret-value elm)
               do (insert (substitute-command-keys str) "\n\n"))
      (org-mode))
    (setq buffer-read-only t)
    (view-mode))
  (let ((helm-org-headings--nofilename t))
    (helm :sources (helm-source-org-headings-for-files
                    (list (get-buffer helm-documentation-buffer-name)))
          :candidate-number-limit 99999
          :buffer "*helm doc*")))

;;; Local help messages.

;;; `helm-buffer-list' help
;;
;;
(defvar helm-buffer-help-message
  "* Helm Buffer

** Tips

*** Completion

**** Major-mode

You can enter a partial major-mode name (e.g. lisp, sh) to narrow down buffers.
To specify the major-mode, prefix it with \"*\" e.g. \"*lisp\".

If you want to match all buffers but the ones with a specific major-mode
\(negation), prefix the major-mode with \"!\" e.g. \"*!lisp\".

If you want to specify more than one major-mode, separate them with \",\",
e.g. \"*!lisp,!sh,!fun\" lists all buffers but the ones in lisp-mode, sh-mode
and fundamental-mode.

Then enter a space followed by a pattern to narrow down to buffers matching this
pattern.

**** Search inside buffers

If you enter a space and a pattern prefixed by \"@\", Helm searches for text
matching this pattern \*inside* the buffer (i.e. not in the name of the buffer).

If you enter a pattern prefixed with an escaped \"@\", Helm searches for a
buffer matching \"@pattern\" but does not search inside.

**** Search by directory name

If you prefix the pattern with \"/\", Helm matches over the directory names
of the buffers.

This feature can be used to narrow down the search to one directory while
subsequent strings entered after a space match over the buffer name only.

Note that negation is not supported for matching on buffer filename.

Starting from Helm v1.6.8, you can specify more than one directory.

**** Fuzzy matching

`helm-buffers-fuzzy-matching' turns on fuzzy matching on buffer names, but not
on directory names or major modes.  A pattern starting with \"^\" disables fuzzy
matching and matches by exact regexp.

**** Examples

With the following pattern

    \"*lisp ^helm @moc\"

Helm narrows down the list by selecting only the buffers that are in lisp mode,
start with \"helm\" and which content matches \"moc\".

Without the \"@\"

    \"*lisp ^helm moc\"

Helm looks for lisp mode buffers starting with \"helm\" and containing \"moc\"
in their name.

With this other pattern

    \"*!lisp !helm\"

Helm narrows down to buffers that are not in \"lisp\" mode and that do not match
\"helm\".

With this last pattern

    /helm/ w3

Helm narrows down to buffers that are in any \"helm\" subdirectory and
matching \"w3\".

*** Creating buffers

When creating a new buffer, use `\\[universal-argument]' to choose a mode from a
list.  This list is customizable, see `helm-buffers-favorite-modes'.

*** Killing buffers

You can kill buffers either one by one or all the marked buffers at once.

One kill-buffer command leaves Helm while the other is persistent.  Run the
persistent kill-buffer command either with the regular
`helm-execute-persistent-action' called with a prefix argument (`\\[universal-argument] \\<helm-map>\\[helm-execute-persistent-action]')
or with its specific command `helm-buffer-run-kill-persistent'.  See the
bindings below.

*** Switching to buffers

To switch to a buffer, press RET, to switch to a buffer in another window, select this buffer
and press \\<helm-buffer-map>\\[helm-buffer-switch-other-window], when called with a prefix arg
the buffer will be displayed vertically in other window.
If you mark more than one buffer, the marked buffers will be displayed in different windows.

*** Meaning of colors and prefixes for buffers

Remote buffers are prefixed with '@'.
Red        => Buffer's file was modified on disk by an external process.
Indianred2 => Buffer exists but its file has been deleted.
Orange     => Buffer is modified and not saved to disk.
Italic     => A non-file buffer.

** Commands
\\<helm-buffer-map>
\\[helm-buffer-run-zgrep]\t\tGrep Buffer(s) works as zgrep too (`\\[universal-argument]' to grep all buffers but non-file buffers).
\\[helm-buffers-run-multi-occur]\t\tMulti-Occur buffer or marked buffers (`\\[universal-argument]' to toggle force-searching current-buffer).
\\[helm-buffer-switch-other-window]\t\tSwitch to other window.
\\[helm-buffer-switch-other-frame]\t\tSwitch to other frame.
\\[helm-buffers-run-browse-project]\t\tBrowse project from buffer.
\\[helm-buffer-run-query-replace-regexp]\t\tQuery-replace-regexp in marked buffers.
\\[helm-buffer-run-query-replace]\t\tQuery-replace in marked buffers.
\\[helm-buffer-run-ediff]\t\tEdiff current buffer with candidate.  With two marked buffers, ediff those buffers.
\\[helm-buffer-run-ediff-merge]\t\tEdiff-merge current buffer with candidate.  With two marked buffers, ediff-merge those buffers.
\\[helm-buffer-diff-persistent]\t\tToggle Diff-buffer with saved file without leaving Helm.
\\[helm-buffer-revert-persistent]\t\tRevert buffer without leaving Helm.
\\[helm-buffer-save-persistent]\t\tSave buffer without leaving Helm.
\\[helm-buffer-run-kill-buffers]\t\tDelete marked buffers and leave Helm.
\\[helm-buffer-run-kill-persistent]\t\tDelete buffer without leaving Helm.
\\[helm-toggle-all-marks]\t\tToggle all marks.
\\[helm-mark-all]\t\tMark all.
\\[helm-toggle-buffers-details]\t\tToggle details.
\\[helm-buffers-toggle-show-hidden-buffers]\t\tShow hidden buffers.
\\[helm-buffers-mark-similar-buffers]\t\tMark all buffers of the same type (color) as current buffer.")

;;; Find files help (`helm-find-files')
;;
;;
(defvar helm-ff-help-message
  "* Helm Find Files

** Tips

*** Navigation summary

For a better experience you can enable auto completion by setting
`helm-ff-auto-update-initial-value' to non-nil in your init file.  It is not
enabled by default to not confuse new users.

**** Use `\\<helm-find-files-map>\\[helm-execute-persistent-action]' (persistent action) on a directory to go down one level.

On a symlinked directory a prefix argument expands to its true name.

**** Use `\\<helm-find-files-map>\\[helm-find-files-up-one-level]' on a directory to go up one level.

**** Use `\\<helm-find-files-map>\\[helm-find-files-down-last-level]' to walk back the resulting tree of all the `\\<helm-map>\\[helm-execute-persistent-action]' you did.

The tree is reinitialized each time you browse a new tree with
`\\<helm-map>\\[helm-execute-persistent-action]' or by entering some pattern in the prompt.

**** `RET' behavior

It behaves differently depending on `helm-selection' (current candidate in helm-buffer):

- candidate basename is \".\"   => Open it in dired.
- candidate is a directory    => Expand it.
- candidate is a file         => Open it.
- marked candidates (1+)      => Open them with default action.

Note that when copying, renaming, etc. from `helm-find-files' the
destination file is selected with `helm-read-file-name'.

To avoid confusion when using `read-file-name' or `read-directory-name', `RET'
follows its standard Emacs behaviour, i.e. it exits the minibuffer as soon as
you press `RET'.  If you want the same behavior as in `helm-find-files', bind
`helm-ff-RET' to the `helm-read-file-map':

    (define-key helm-read-file-map (kbd \"RET\") 'helm-ff-RET)

*** Find file at point

Helm uses `ffap' partially or completely to find file at point depending on the
value of `helm-ff-guess-ffap-filenames': if non-nil, support is complete
\(annoying), if nil, support is partial.

**** Find file at line number

When text at point is in the form of

    ~/elisp/helm/helm.el:1234

Helm finds this file at the indicated line number, here 1234.

**** Find URL at point

When a URL is found at point, Helm expands to that URL only.
Pressing `RET' opens that URL using `browse-url-browser-function'.

**** Find e-mail address at point

When an e-mail address is found at point, Helm expands to this e-mail address
prefixed with \"mailto:\".  Pressing `RET' opens a message buffer with that
e-mail address.

*** Quick pattern expansion

**** Enter `~/' at end of pattern to quickly reach home directory.

**** Enter `/' at end of pattern to quickly reach the file system root.

**** Enter `./' at end of pattern to quickly reach `default-directory'.

\(As per its value at the beginning of the session.)

If you already are in the `default-directory' this will move the cursor to the top.

**** Enter `../' at end of pattern will reach upper directory, moving cursor to the top.

This is different from using `\\<helm-find-files-map>\\[helm-find-files-up-one-level]' in that it moves
the cursor to the top instead of remaining on the previous subdir name.

**** Enter `..name/' at end of pattern to start a recursive search.

It searches directories matching \"name\" under the current directory, see the
\"Recursive completion on subdirectories\" section below for more details.

**** Any environment variable (e.g. `$HOME') at end of pattern gets expanded.

**** Any valid filename yanked after pattern gets expanded.

**** Special case: URL at point

The quick expansions do not take effect after end a URL, you must kill the
pattern first (`\\[helm-delete-minibuffer-contents]').

*** Helm-find-files supports fuzzy matching.

It starts from the third character of the pattern.

For instance \"fob\" or \"fbr\" will complete \"foobar\" but \"fb\" needs a
third character in order to complete it.

*** Use `\\[universal-argument] \\[helm-execute-persistent-action]' or `\\[helm-follow-action-forward]' to display an image.

*** `\\[helm-execute-persistent-action]' on a filename expands to that filename in the Helm buffer.

Second hit displays the buffer filename.
Third hit kills the buffer filename.
Note: `\\[universal-argument] \\[helm-execute-persistent-action]' displays the buffer directly.

*** Browse images directories with `helm-follow-mode' and navigate up/down.

You can also use `helm-follow-action-forward' and `helm-follow-action-backward' with
`\\[helm-follow-action-forward]' and `\\[helm-follow-action-backward]' respectively.

*** Toggle auto-completion with `\\[helm-ff-run-toggle-auto-update]'.

It is useful when trying to create a new file or directory and you don't want
Helm to complete what you are writing.

Note: On a terminal, the default binding `C-<backspace>' may not work.
In this case use `C-c <backspace>'.

*** You can create a new directory and a new file at the same time.

Simply write the path in the prompt and press `RET', e.g.
\"~/new/newnew/newnewnew/my_newfile.txt\".

*** To create a new directory, append a \"/\" to the new name and press `RET'.

*** To create a new file, enter a filename not ending with \"/\".

*** Recursive search from Helm-find-files.

**** You can use Helm-browse-project (see binding below).

- With no prefix argument:
If the current directory is under version control with either git or hg and
helm-ls-git and/or helm-ls-hg are installed, it lists all the files under
version control.  Otherwise it falls back to Helm-find-files.  See
https://github.com/emacs-helm/helm-ls-git and
https://github.com/emacs-helm/helm-ls-hg.

- With one prefix argument:
List all the files under this directory and other subdirectories
\(recursion) and this list of files will be cached.

- With two prefix arguments:
Same but the cache is refreshed.

**** You can start a recursive search with \"locate\" or \"find\".

See \"Note\" in the [[Recusive completion on subdirectories][section on subdirectories]].

Using \"locate\", you can enable the local database with a prefix argument. If the
local database doesn't already exists, you will be prompted for its creation.
If it exists and you want to refresh it, give it two prefix args.

When using locate the helm-buffer remains empty until you type something.
Regardless Helm uses the basename of the pattern entered in the helm-find-files
session by default.  Hitting `\\[next-history-element]' should just kick in the
locate search with this pattern.  If you want Helm to automatically do this, add
`helm-source-locate' to `helm-sources-using-default-as-input'.

**** Recursive completion on subdirectories

Starting from the directory you are currently browsing, it is possible to have
completion of all directories underneath.  Say you are at \"/home/you/foo/\" and
you want to go to \"/home/you/foo/bar/baz/somewhere/else\", simply type
\"/home/you/foo/..else\" and hit `\\[helm-execute-persistent-action]' or enter
the final \"/\".  Helm will then list all possible directories under \"foo\"
matching \"else\".

Entering two spaces before \"else\" instead of two dots also works.

Note: Completion on subdirectories uses \"locate\" as backend, you can configure
the command with `helm-locate-recursive-dirs-command'.  Because this completion
uses an index, the directory tree displayed may be out-of-date and not reflect
the latest change until you update the index (using \"updatedb\" for \"locate\").

If for some reason you cannot use an index, the \"find\" command from
\"findutils\" can be used instead.  It will be slower though.  You need to pass
the basedir as first argument of \"find\" and the subdir as the value for
'-(i)regex' or '-(i)name' with the two format specs that are mandatory in
`helm-locate-recursive-dirs-command'.

Examples:
- \"find %s -type d -name '*%s*'\"
- \"find %s -type d -regex .*%s.*$\"

*** Insert filename at point or complete filename at point.

On insertion (not on completion, i.e. there is nothing at point):

- `\\[helm-ff-run-complete-fn-at-point]': insert absolute file name.
- `\\[universal-argument] \\[helm-ff-run-complete-fn-at-point]': insert abbreviated file name.
- `\\[universal-argument] \\[universal-argument] \\[helm-ff-run-complete-fn-at-point]': insert relative file name.

On completion:

- Target starts with \"~/\": insert abbreviate file name.
- target starts with \"/\" or \"[a-z]:/\": insert full path.
- Otherwise: insert relative file name.

*** Use the wildcard to select multiple files.

Use of wilcard is supported to run an action over a set of files.

Example: You can copy all the files with \".el\" extension by using \"*.el\" and
then run copy action.

Similarly, \"**.el\" (note the two stars) will recursively select all \".el\"
files under the current directory.

Note that when recursively copying files, you may have files with same name
dispatched across different subdirectories, so when copying them in the same
directory they will get overwritten.  To avoid this Helm has a special action
called \"backup files\" that has the same behavior as the command line \"cp -f
--backup=numbered\": it allows you to copy many files with the same name from
different subdirectories into one directory.  Files with same name are renamed
as follows: \"foo.txt.~1~\".  Like with the --force option of cp, it is possible
to backup files in current directory.

This command is available only when `dired-async-mode' is active.

When using an action that involves an external backend (e.g. grep), using \"**\"
is not recommended (even thought it works fine) because it will be slower to
select all the files.  You are better off leaving the backend to do it, it will
be faster.  However, if you know you have not many files it is reasonable to use
this, also using not recursive wilcard (e.g. \"*.el\") is perfectly fine for
this.

The \"**\" feature is active by default in the option `helm-file-globstar'.  It
is different from the Bash \"shopt globstar\" feature in that to list files with
a named extension recursively you would write \"**.el\" whereas in Bash it would
be \"**/*.el\".  Directory selection with \"**/\" like Bash \"shopt globstar\"
option is not supported yet.

*** Query replace regexp on filenames.

Replace different parts of a file basename with something else.

When calling this action you will be prompted twice as with
`query-replace', first for the matching expression of the text to
replace and second for the replacement text.  Several facilities,
however, are provided to make the two prompts more powerfull.

**** Syntax of the first prompt

In addition to simple regexps, these shortcuts are available:

- Basename without extension => \"%.\"
- Only extension             => \".%\"
- Substring                  => \"%:<from>:<to>\"
- Whole basename             => \"%\"

**** Syntax of the second prompt

In addition to a simple string to use as replacement, here is what you can use:

- A placeholder refering to what you have selected in the first prompt: \"\\@\".

After this placeholder you can use a search-and-replace syntax à-la sed:

    \"\\@/<regexp>/<replacement>/

You can select a substring from the string represented by the placeholder:

    \"\\@:<from>:<to>\"

- A special character representing a number which is incremented: \"\\#\".

- Shortcuts for `upcase', `downcase' and `capitalize'
are available as`%u', `%d' and `%c' respectively.

**** Examples

***** Recursively rename all files with \".JPG\" extension to \".jpg\".

Use the `helm-file-globstar' feature described in [[Using wildcard to select multiple files][recursive globbing]]
by entering \"**.JPG\" at the end of the Helm-find-files pattern, then hit
\\<helm-map>\\[helm-ff-query-replace-on-filenames]: First \"JPG\", then \"jpg\"
and hit `RET'.

Alternatively you can enter \".%\" at the first prompt, then \"jpg\" and hit
`RET'.  Note that when using this instead of using \"JPG\" at the first prompt,
all extensions will be renamed to \"jpg\" even if the extension of one of the
files is, say, \"png\".  If you want to keep the original extension you can use
\"%d\" at the second prompt (downcase).

***** Batch-rename files from number 001 to 00x.

Use \"\\#\" inside the second prompt.

Example 1: To rename the files

    foo.jpg
    bar.jpg
    baz.jpg

to

    foo-001.jpg
    foo-002.jpg
    foo-003.jpg

use \"%.\" as matching regexp and \"foo-\\#\" as replacement string.

Example 2: To rename the files

    foo.jpg
    bar.jpg
    baz.jpg

to

    foo-001.jpg
    bar-002.jpg
    baz-003.jpg

use as matching regexp \"%.\" and as replacement string \"\\@-\\#\".

***** Replace a substring.

Use \"%:<from>:<to>\".

Example: To rename files

    foo.jpg
    bar.jpg
    baz.jpg

to

    fOo.jpg
    bAr.jpg
    bAz.jpg

use as matching regexp \"%:1:2\" and as replacement string \"%u\" (upcase).

Note that you \*cannot* use \"%.\" and \".%\" along with substring replacement.

***** Modify the string from the placeholder (\\@).

- By substring, i.e. only using the substring of the placeholder: \"\\@:<from>:<to>\".
The length of placeholder is used for <to> when unspecified.

Example 1: \"\\@:0:2\" replaces from the beginning to the second char of the placeholder.

Example 2: \\@:2: replaces from the second char of the placeholder to the end.

- By search-and-replace: \"\\@/<regexp>/<replacement>/\".

Incremental replacement is also handled in <replacement>.

Example 3: \"\\@/foo/bar/\" replaces \"foo\" by \"bar\" in the placeholder.

Example 4: \"\\@/foo/-\\#/\" replaces \"foo\" in the placeholder by 001, 002, etc.

***** Clash in replacements (avoid overwriting files).

When performing any of these replacement operations you may end up with same
names as replacement.  In such cases Helm numbers the file that would otherwise
overwritten.  For instance, should you remove the \"-m<n>\" part from the files
\"emacs-m1.txt\", \"emacs-m2.txt\" and \"emacs-m3.txt\" you would end up with
three files named \"emacs.txt\", the second renaming overwriting first file, and
the third renaming overwriting second file and so on.  Instead Helm will
automatically rename the second and third files as \"emacs(1).txt\" and
\"emacs(2).txt\" respectively.

***** Query-replace on filenames vs. serial-rename action.

Unlike the [[Serial renaming][serial rename]] actions, the files renamed with
the query-replace action stay in their initial directory and are not moved to
the current directory.  As such, using \"\\#\" to serial-rename files only makes
sense for files inside the same directory.  It even keeps renaming files
with an incremental number in the next directories.

*** Serial-rename.

You can use the serial-rename actions to rename, copy or symlink marked files to
a specific directory or in the current directory with all the files numbered
incrementally.

- Serial-rename by renaming:
Rename all marked files with incremental numbering to a specific directory.

- Serial-rename by copying:
Copy all marked files with incremental numbering to a specific directory.

- Serial-rename by symlinking:
Symlink all marked files with incremental numbering to a specific directory.

*** Edit marked files in a dired buffer.

You can open a dired buffer containing only marked files with `\\<helm-find-files-map>\\[helm-ff-run-marked-files-in-dired]'.
With a prefix argument you can open this same dired buffer in wdired mode for
editing.  Note that wildcards are supported as well, so you can use e.g.
\"*.txt\" to select all \".txt\" files in the current directory or \"**.txt\" to
select all files recursively from the current directory.  See [[Using wildcard to
select multiple files]] section above.

*** Defining default target directory for copying, renaming, etc.

You can customize `helm-dwim-target' to behave differently depending on the
windows open in the current frame.  Default is to provide completion on all
directories associated to each window.

*** Copying and renaming asynchronously.

If you have the async library installed (if you got Helm from MELPA you do), you
can use it for copying/renaming files by enabling `dired-async-mode'.

Note that even when async is enabled, running a copy/rename action with a prefix
argument will execute action synchronously. Moreover it will follow the first
file of the marked files in its destination directory.

When `dired-async-mode' is enabled, an additional action named \"Backup files\"
will be available. (Such command is not natively available in Emacs).
See [[Using wildcard to select multiple files]] for details.

*** Bookmark the `helm-find-files' session.

You can bookmark the `helm-find-files' session with `\\[helm-ff-bookmark-set]'.
You can later retrieve these bookmarks by calling `helm-filtered-bookmarks'
or, from the current `helm-find-files' session, by hitting `\\[helm-find-files-toggle-to-bookmark]'.

*** Grep files from `helm-find-files'.

You can grep individual files from `helm-find-files' by using
\`\\<helm-find-files-map>\\[helm-ff-run-grep]'.  This same command can also
recursively grep files from the current directory when called with a prefix
argument.  In this case you will be prompted for the file extensions to use
\(grep backend) or the types of files to use (ack-grep backend).  See the
`helm-grep-default-command' documentation to set this up.  For compressed files
or archives, use zgrep with \`\\<helm-find-files-map>\\[helm-ff-run-zgrep]'.

Otherwise you can use recursive commands like \`\\<helm-find-files-map>\\[helm-ff-run-grep-ag]' or `\\<helm-find-files-map>\\[helm-ff-run-git-grep]'
that are much faster than using `\\<helm-find-files-map>\\[helm-ff-run-grep]' with a prefix argument.
See `helm-grep-ag-command' and `helm-grep-git-grep-command' to set this up.

You can also use \"id-utils\"' GID with \`\\<helm-find-files-map>\\[helm-ff-run-gid]'
by creating an ID index file with the \"mkid\" shell command.

All those grep commands use the symbol at point as the default pattern.
Note that default is different from input (nothing is added to the prompt until
you hit `\\[next-history-element]').

**** Grepping on remote files.

On remote files grep is not well supported by TRAMP unless you suspend updates before
entering the pattern and re-enable it once your pattern is ready.
To toggle suspend-update, use `\\<helm-map>\\[helm-toggle-suspend-update]'.

*** Setting up aliases in Eshell allows you to set up powerful customized commands.

Adding Eshell aliases to your `eshell-aliases-file' or using the
`alias' command from Eshell allows you to create personalized
commands not available in `helm-find-files' actions and use them
from `\\<helm-find-files-map>\\[helm-ff-run-eshell-command-on-file]'.

Example: You want a command to uncompress some \"*.tar.gz\" files from `helm-find-files':

1) Create an Eshell alias named, say, \"untargz\" with the command
\"alias untargz tar zxvf $*\".

2) Now from `helm-find-files' select the \"*.tar.gz\" file (you can also
mark files if needed) and hit `\\<helm-find-files-map>\\[helm-ff-run-eshell-command-on-file]'.

Note: When using marked files with this, the meaning of the prefix argument is
quite subtle.  Say you have \"foo\", \"bar\" and \"baz\" marked; when you run
the alias command `example' on these files with no prefix argument it will run
`example' sequentially on each file:

$ example foo
$ example bar
$ example baz

With a prefix argument however it will apply `example' on all files at once:

$ example foo bar baz

Of course the alias command should support this.

*** Using TRAMP with `helm-find-files' to read remote directories.

`helm-find-files' works fine with TRAMP despite some limitations.

- By default filenames are not highlighted when working on remote directories,
this is controled by `helm-ff-tramp-not-fancy' variable.  If you change this,
expect Helm to be very slow unless your connection is super fast.

- Grepping files is not very well supported when used incrementally.
See [[Grepping on remote files]].

- Locate does not work on remote directories.

**** A TRAMP syntax crash course.

Please refer to TRAMP's documentation for more details.

- Connect to host 192.168.0.4 as user \"foo\":

/scp:192.168.0.4@foo:

- Connect to host 192.168.0.4 as user \"foo\" on port 2222:

/scp:192.168.0.4@foo#2222:

- Connect to host 192.168.0.4 as root using multihops syntax:

/ssh:192.168.0.4@foo|sudo:192.168.0.4:

Note: You can also use `tramp-default-proxies-alist' when connecting often to
the same hosts.

As a rule of thumb, prefer the scp method unless using multihops (which only
works with the ssh method), especially when copying large files.

You need to hit `C-j' once on top of a directory on the first connection
to complete the pattern in the minibuffer.

**** Completing host.

As soon as you enter the first \":\" after method e.g =/scp:\= you will
have some completion about previously used hosts or from your =~/.ssh/config\=
file, hitting `\\[helm-execute-persistent-action]' or `right' on a candidate will insert this host in minibuffer
without addind the ending \":\".
As soon the last \":\" is entered TRAMP will kick in and you should see the list
of candidates soon after.

When connection fails, be sure to delete your TRAMP connection with M-x
`helm-delete-tramp-connection' before retrying.

**** Editing local files as root.

Use the sudo method:

\"/sudo:host:\" or simply \"/sudo::\".

*** Attach files to a mail buffer (message-mode).

If you are in a `message-mode' or `mail-mode' buffer, that action will appear
in action menu, otherwise it is available at any time with \\<helm-find-files-map>\\[helm-ff-run-mail-attach-files].
It behaves as follows:

- If you are in a (mail or message) buffer, files are attached there.

- If you are not in a mail buffer but one or more mail buffers exist, you are
prompted to attach files to one of these mail buffers.

- If you are not in a mail buffer and no mail buffer exists,
a new mail buffer is created with the attached files in it.

*** Open files in separate windows.

When [[Marked candidates][marking]] multiple files or using [[Use the wildcard to select multiple files.][wildcard]], helm allow opening all
this files in separate windows using an horizontal layout or a
vertical layout if you used a prefix arg, when no more windows can be
displayed in frame, next files are opened in background without being
displayed.  When using \\<helm-find-files-map>\\[helm-ff-run-switch-other-window] the current
buffer is kept and files are displayed next to it.


** Commands
\\<helm-find-files-map>
\\[helm-ff-run-locate]\t\tRun `locate' (`\\[universal-argument]' to specify locate database, `M-n' to insert basename of candidate).
\\[helm-ff-run-browse-project]\t\tBrowse project (`\\[universal-argument]' to recurse, `\\[universal-argument] \\[universal-argument]' to recurse and refresh database).
\\[helm-ff-run-find-sh-command]\t\tRun `find' shell command from this directory.
\\[helm-ff-run-grep]\t\tRun Grep (`\\[universal-argument]' to recurse).
\\[helm-ff-run-pdfgrep]\t\tRun Pdfgrep on marked files.
\\[helm-ff-run-zgrep]\t\tRun zgrep (`\\[universal-argument]' to recurse).
\\[helm-ff-run-grep-ag]\t\tRun AG grep on current directory.
\\[helm-ff-run-git-grep]\t\tRun git-grep on current directory.
\\[helm-ff-run-gid]\t\tRun gid (id-utils).
\\[helm-ff-run-etags]\t\tRun Etags (`\\[universal-argument]' to use thing-at-point, `\\[universal-argument] \\[universal-argument]' to reload cache).
\\[helm-ff-run-rename-file]\t\tRename File (`\\[universal-argument]' to follow).
\\[helm-ff-run-query-replace-on-marked]\t\tQuery replace on marked files.
\\[helm-ff-run-copy-file]\t\tCopy File (`\\[universal-argument]' to follow).
\\[helm-ff-run-byte-compile-file]\t\tByte Compile File (`\\[universal-argument]' to load).
\\[helm-ff-run-load-file]\t\tLoad File.
\\[helm-ff-run-symlink-file]\t\tSymlink File.
\\[helm-ff-run-hardlink-file]\t\tHardlink file.
\\[helm-ff-run-delete-file]\t\tDelete File.
\\[helm-ff-run-kill-buffer-persistent]\t\tKill buffer candidate without leaving Helm.
\\[helm-ff-persistent-delete]\t\tDelete file without leaving Helm.
\\[helm-ff-run-switch-to-eshell]\t\tSwitch to Eshell.
\\[helm-ff-run-eshell-command-on-file]\t\tEshell command on file (`\\[universal-argument]' to apply on marked files, otherwise treat them sequentially).
\\[helm-ff-run-ediff-file]\t\tEdiff file.
\\[helm-ff-run-ediff-merge-file]\t\tEdiff merge file.
\\[helm-ff-run-complete-fn-at-point]\t\tComplete file name at point.
\\[helm-ff-run-switch-other-window]\t\tSwitch to other window.
\\[helm-ff-run-switch-other-frame]\t\tSwitch to other frame.
\\[helm-ff-run-open-file-externally]\t\tOpen file with external program (`\\[universal-argument]' to choose).
\\[helm-ff-run-open-file-with-default-tool]\t\tOpen file externally with default tool.
\\[helm-ff-rotate-left-persistent]\t\tRotate image left.
\\[helm-ff-rotate-right-persistent]\t\tRotate image right.
\\[helm-find-files-up-one-level]\t\tGo to parent directory.
\\[helm-ff-run-switch-to-history]\t\tSwitch to the vistied-directory history.
\\[helm-ff-file-name-history]\t\tSwitch to file name history.
\\[helm-ff-properties-persistent]\t\tShow file properties in a tooltip.
\\[helm-mark-all]\t\tMark all visible candidates.
\\[helm-ff-run-toggle-auto-update]\t\tToggle auto-expansion of directories.
\\[helm-unmark-all]\t\tUnmark all candidates, visible and invisible ones.
\\[helm-ff-run-gnus-attach-files]\t\tGnus' attach files to message buffer.
\\[helm-ff-run-print-file]\t\tPrint file, (`\\[universal-argument]' to refresh printer list).
\\[helm-enlarge-window]\t\tEnlarge Helm window.
\\[helm-narrow-window]\t\tNarrow Helm window.
\\[helm-ff-run-toggle-basename]\t\tToggle basename/fullpath.
\\[helm-ff-run-find-file-as-root]\t\tFind file as root.
\\[helm-ff-run-find-alternate-file]\t\tFind alternate file.
\\[helm-ff-run-insert-org-link]\t\tInsert org link.")

;;; Help for `helm-read-file-name'
;;
;;
(defun helm-read-file-name-help-message ()
  (let ((name (if helm-alive-p
                  (assoc-default 'name (helm-get-current-source))
                "generic")))
    (format 
     "* Helm `%s' read file name completion

This is `%s' read file name completion that have been \"helmized\"
because you have enabled [[Helm mode][helm-mode]]'.
Don't confuse this with `helm-find-files' which is a native helm command,
see [[Helm functions vs helmized emacs functions]].

** Tips

*** Navigation

**** Enter `~/' at end of pattern to quickly reach home directory.

**** Enter `/' at end of pattern to quickly reach the file system root.

**** Enter `./' at end of pattern to quickly reach `default-directory'.

\(As per its value at the beginning of the session.)

If you already are in the `default-directory' this will move the cursor to the top.

**** Enter `../' at end of pattern will reach upper directory, moving cursor on top

This is different from using `\\[helm-find-files-up-one-level]' in that it moves
the cursor to the top instead of remaining on the previous subdir name.

**** You can complete with partial basename.

It starts from the third character of the pattern.

For instance \"fob\" or \"fbr\" will complete \"foobar\" but \"fb\" needs a
third character in order to complete it.

*** Persistent actions

By default `helm-read-file-name' uses the persistent actions of `helm-find-files'.

**** Use `\\[universal-argument] \\<helm-map>\\[helm-execute-persistent-action]' to display an image.

**** `\\<helm-map>\\[helm-execute-persistent-action]' on a filename will expand to this filename in Helm-buffer.

Second hit displays the buffer filename.
Third hit kills the buffer filename.
Note: `\\[universal-argument] \\<helm-map>\\[helm-execute-persistent-action]' displays the buffer directly.

**** Browse images directories with `helm-follow-mode' and navigate up/down.

*** Delete characters backward

When you want to delete characters backward, e.g. to create a new file or directory,
auto-update may come in the way when it keeps updating to an existent directory.
In that case, type `C-<backspace>' and then `<backspace>'.
This should not be needed when copying/renaming files because autoupdate is disabled
by default in that case.

Note: On a terminal, the default binding `C-<backspace>' may not work.
In this case use `C-c <backspace>'.

*** Create new directories and files

**** You can create a new directory and a new file at the same time.

Simply write the path in prompt and press `RET', e.g.
\"~/new/newnew/newnewnew/my_newfile.txt\".

**** To create a new directory, append a \"/\" at to the new name and press `RET'.

**** To create a new file, enter a filename not ending with \"/\".

File and directory creation works only with some commands (e.g. `find-file')
and it will not work with others where it is not intended to return a file or
a directory \(e.g `list-directory').

*** Exiting minibuffer with empty string

You can exit minibuffer with empty string with \\<helm-read-file--map>\\[helm-cr-empty-string].
It is useful when some commands are prompting continuously until you enter an empty prompt.

** Commands
\\<helm-read-file-map>
\\[helm-find-files-up-one-level]\t\tGo to parent directory.
\\[helm-ff-run-toggle-auto-update]\t\tToggle auto-expansion of directories.
\\[helm-ff-run-toggle-basename]\t\tToggle basename.
\\[helm-ff-file-name-history]\t\tFile name history.
C/\\[helm-cr-empty-string]\t\tReturn empty string unless `must-match' is non-nil.
\\[helm-next-source]\t\tGo to next source.
\\[helm-previous-source]\t\tGo to previous source."
     name name)))

;;; Generic file help - Used by locate.
;;
;;
(defvar helm-generic-file-help-message
  "* Helm Generic files

** Tips

*** Locate

You can append to the search pattern any of the locate command line options,
e.g. -b, -e, -n <number>, etc.  See the locate(1) man page for more details.

Some other sources (at the moment \"recentf\" and \"file in current directory\")
support the -b flag for compatibility with locate when they are used with it.

When you enable fuzzy matching on locate with `helm-locate-fuzzy-match', the
search will be performed on basename only for efficiency (so don't add \"-b\" at
prompt).  As soon as you separate the patterns with spaces, fuzzy matching will
be disabled and search will be done on the full filename.  Note that in
multi-match, fuzzy is completely disabled, which means that each pattern is a
match regexp (i.e. \"helm\" will match \"helm\" but \"hlm\" will \*not* match
\"helm\").

*** Browse project

When the current directory is not under version control, don't forget to refresh
the cache when files have been added/removed in the directory.

*** Find command

Recursively search files using the \"find\" shell command.

Candidates are all filenames that match all given globbing patterns.  This
respects the options `helm-case-fold-search' and
`helm-findutils-search-full-path'.

You can pass arbitrary \"find\" options directly after a \"*\" separator.
For example, this would find all files matching \"book\" that are larger
than 1 megabyte:

    book * -size +1M

** Commands
\\<helm-generic-files-map>
\\[helm-ff-run-toggle-basename]\t\tToggle basename.
\\[helm-ff-run-grep]\t\tRun grep (`\\[universal-argument]' to recurse).
\\[helm-ff-run-zgrep]\t\tRun zgrep.
\\[helm-ff-run-gid]\t\tRun GID (id-utils).
\\[helm-ff-run-pdfgrep]\t\tRun PDFgrep on marked files.
\\[helm-ff-run-copy-file]\t\tCopy file(s)
\\[helm-ff-run-rename-file]\t\tRename file(s).
\\[helm-ff-run-symlink-file]\t\tSymlink file(s).
\\[helm-ff-run-hardlink-file]\t\tHardlink file(s).
\\[helm-ff-run-delete-file]\t\tDelete file(s).
\\[helm-ff-run-byte-compile-file]\t\tByte compile Elisp file(s) (`\\[universal-argument]' to load).
\\[helm-ff-run-load-file]\t\tLoad Elisp file(s).
\\[helm-ff-run-ediff-file]\t\tEdiff file.
\\[helm-ff-run-ediff-merge-file]\t\tEdiff-merge file.
\\[helm-ff-run-switch-other-window]\t\tSwitch to other window.
\\[helm-ff-properties-persistent]\t\tShow file properties.
\\[helm-ff-run-etags]\t\tRun etags (`\\[universal-argument]' to use tap, `\\[universal-argument] \\[universal-argument]' to reload the database).
\\[helm-yank-text-at-point]\t\tYank text at point.
\\[helm-ff-run-open-file-externally]\t\tOpen file with external program (`\\[universal-argument]' to choose).
\\[helm-ff-run-open-file-with-default-tool]\t\tOpen file externally with default tool.
\\[helm-ff-run-insert-org-link]\t\tInsert org link.")

;;; Grep help
;;
;;
(defvar helm-grep-help-message
  "* Helm Grep

** Tips

*** Use a prefix argument to grep recursively.

With Helm supporting git-grep and AG however, you are better off using one of
them for recursive searches.

*** You can use wild cards when selecting files (e.g. \"*.el\").

*** You can grep in many different directories by marking files or using wild cards.

*** You can save the result in a `helm-grep-mode' buffer.

See [[Commands][commands]] below.

Once in that buffer you can use \"emacs-wgrep\" (external package not bundled with Helm)
to edit your changes.

*** Helm-grep supports multi-matching.

\(Starting from version 1.9.4.)

Simply add a space between each pattern as for most Helm commands.

*** See full path of selected candidate

Add (helm-popup-tip-mode 1) in your init file or enable it interactively with
M-x helm-popup-tip-mode.

*** Open file in other window

The command \\<helm-grep-map>\\[helm-grep-run-other-window-action] allow you to open file
in other window horizontally or vertically if a prefix arg is supplied.
 
*** Performance over TRAMP

Grepping works but it is badly supported as TRAMP doesn't support multiple
processes running in a short delay (less than 5s) among other things.

Helm uses a special hook to suspend the process automatically while you are
typing.  Even if Helm handles this automatically by delaying each process by 5s,
you are adviced to this manually by hitting `\\<helm-map>\\[helm-toggle-suspend-update]' (suspend process) before
typing, and hit again `\\<helm-map>\\[helm-toggle-suspend-update]' when the regexp is ready to send to the remote
process.  For simple regexps, there should be no need for this.

Another solution is to not use TRAMP at all and mount your remote file system via
SSHFS.

* Helm GID

** Tips

Helm-GID reads the database created with the `mkid' command from id-utils.
The name of the database file can be customized with `helm-gid-db-file-name', it
is usually \"ID\".

Helm-GID use the symbol at point as default-input.  This command is also
accessible from `helm-find-files' which allow you to navigate to another
directory to consult its database.

Note: Helm-GID supports multi-matches but only the last pattern entered will be
highlighted since there is no ~--color~-like option in GID itself.

* Helm AG

** Tips

Helm-AG is different from grep or ack-grep in that it works on a directory and
not on a list of files.

You can ignore files and directories with a \".agignore\" file, local to a
directory or global when placed in the home directory. \(See the AG man page for
more details.)  That file follows the same syntax as `helm-grep-ignored-files'
and `helm-grep-ignored-directories'.

As always you can access Helm AG from `helm-find-files'.

Starting with version 0.30, AG accepts one or more TYPE arguments on its command
line.  Helm provides completion on these TYPE arguments when available with your
AG version.  Use a prefix argument when starting a Helm-AG session to enable this
completion.

Note: You can mark several types to match in the AG query.  The first AG
versions providing this feature allowed only one type, so in this case only the
last mark will be used.

* Helm git-grep

Helm-git-grep searches the current directory, i.e the default directory or the
directory in Helm-find-files.  If this current directory is a subdirectory of a
project and you want to also match parent directories (i.e the whole project),
use a prefix argument.

** Commands
\\<helm-grep-map>
\\[helm-goto-next-file]\t\tNext File.
\\[helm-goto-precedent-file]\t\tPrevious File.
\\[helm-yank-text-at-point]\t\tYank text at point in minibuffer.
\\[helm-grep-run-other-window-action]\t\tJump to other window.
\\[helm-grep-run-other-frame-action]\t\tJump to other frame.
\\[helm-grep-run-default-action]\t\tRun default action (same as `RET').
\\[helm-grep-run-save-buffer]\t\tSave to a `helm-grep-mode' enabled buffer.")

;;; PDF grep help
;;
;;
(defvar helm-pdfgrep-help-message
  "* Helm PDFgrep Map

** Commands
\\<helm-pdfgrep-map>
\\[helm-goto-next-file]\t\tNext file.
\\[helm-goto-precedent-file]\t\tPrevious file.
\\[helm-yank-text-at-point]\t\tYank text at point in minibuffer.")

;;; Etags help
;;
;;
(defvar helm-etags-help-message
  "* Helm Etags Map

** Commands
\\<helm-etags-map>
\\[helm-goto-next-file]\t\tNext file.
\\[helm-goto-precedent-file]\t\tPrevious file.
\\[helm-yank-text-at-point]\t\tYank text at point in minibuffer.")

;;; UCS help
;;
;;
(defvar helm-ucs-help-message
  "* Helm UCS

** Tips

Use commands below to insert unicode characters in current buffer without
leaving Helm.

** Commands
\\<helm-ucs-map>
\\[helm-ucs-persistent-insert]\t\tInsert character.
\\[helm-ucs-persistent-forward]\t\tForward character.
\\[helm-ucs-persistent-backward]\t\tBackward character.
\\[helm-ucs-persistent-delete]\t\tDelete character backward.")

;;; Bookmark help
;;
;;
(defvar helm-bookmark-help-message
  "* Helm bookmark name

** Commands
\\<helm-bookmark-map>
\\[helm-bookmark-run-jump-other-window]\t\tJump other window.
\\[helm-bookmark-run-delete]\t\tDelete bookmark.
\\[helm-bookmark-run-edit]\t\tEdit bookmark.
\\[helm-bookmark-toggle-filename]\t\tToggle bookmark location visibility.")

;;; Eshell command on file help
;;
;;
(defvar helm-esh-help-message
  "* Helm Eshell on file

** Tips

*** Pass extra arguments after filename.

Normally the command or alias will be called with file as argument.  For instance

    <command> candidate_file

But you can also pass an argument or more after \"candidate_file\" like this:

    <command> %s [extra_args]

\"candidate_file\" will be added at \"%s\" and the command will look at this:

    <command> candidate_file [extra_args]

*** Specify marked files as arguments.

Example:

    <command> file1 file2...

Call `helm-find-files-eshell-command-on-file' with one prefix argument.  Otherwise
you can pass one prefix argument from the command selection buffer.

Note: This does not work on remote files.

With two prefix-args the output is printed to the `current-buffer'.

With no prefix argument or a prefix argument value of '(16) (`\\[universal-argument] \\[universal-argument]')
the command is called once for each file like this:

    <command> file1
    <command> file2
    ...

** Commands
\\<helm-esh-on-file-map>")

;;; Ido virtual buffer help
;;
;;
(defvar helm-buffers-ido-virtual-help-message
  "* Helm Ido virtual buffers

** Commands
\\<helm-buffers-ido-virtual-map>
\\[helm-ff-run-switch-other-window]\t\tSwitch to other window.
\\[helm-ff-run-switch-other-frame]\t\tSwitch to other frame.
\\[helm-ff-run-grep]\t\tGrep file.
\\[helm-ff-run-zgrep]\t\tZgrep file.
\\[helm-ff-run-delete-file]\t\tDelete file.
\\[helm-ff-run-open-file-externally]\t\tOpen file externally.")

;;; Moccur help
;;
;;
(defvar helm-moccur-help-message
  "* Helm Moccur

** Tips

*** Matching

Multiple regexp matching is allowed, simply enter a space to separate the regexps.

Matching empty lines is supported with the regexp \"^$\", you then get the
results displayed as the buffer-name and the line number only.  You can
save and edit these results, i.e. add text to the empty line.

*** Automatically match symbol at point.

Helm can automatically match the symbol at point while keeping the minibuffer
empty, ready to be written to.  This behaviour is disabled by default.  To
enable this you need to add `helm-source-occur' and `helm-source-moccur' to
`helm-sources-using-default-as-input'.

*** Jump to the corresponding line in the searched buffer.

You can do this with `\\<helm-map>\\[helm-execute-persistent-action]' (persistent-action), to do it repeatedly
you can use `\\<helm-map>\\[helm-follow-action-forward]' and `\\<helm-map>\\[helm-follow-action-backward]' or enable `helm-follow-mode' with `\\<helm-map>\\[helm-follow-mode]'.

*** Switch to buffer in other window.

The command \\<helm-moccur-map>\\[helm-moccur-run-goto-line-ow] allow you to switch to buffer
in other window horizontally or vertically if a prefix arg is supplied.

*** Save the results.

Similarly to Helm-grep, you can save the results with `\\<helm-map>\\[helm-moccur-run-save-buffer]'.
Of course if you don't save the results, you can resume the Helm session with
`helm-resume'.

*** Refresh the resumed session.

When the buffer(s) where you ran helm-(m)occur get(s) modified, the Helm buffer
will flash red as a warning.  You can refresh the buffer by running `\\<helm-map>\\[helm-refresh]'.
This can be done automatically by customizing `helm-moccur-auto-update-on-resume'.

*** Refresh a saved buffer.

Type `g' to update the buffer.

*** Edit a saved buffer.

First, install wgrep (https://github.com/mhayashi1120/Emacs-wgrep) and then:

1) `C-c C-p' to edit the buffer(s).
2) `C-x C-s' to save your changes.

Tip: Use the excellent iedit (https://github.com/tsdh/iedit) to modify all
occurences at once in the buffer.

** Commands
\\<helm-moccur-map>
\\[helm-goto-next-file]\t\tNext buffer.
\\[helm-goto-precedent-file]\t\tPrevious buffer.
\\[helm-yank-text-at-point]\t\tYank text at point in minibuffer.
\\[helm-moccur-run-goto-line-ow]\t\tGo to line in other window.
\\[helm-moccur-run-goto-line-of]\t\tGo to line in new frame.")

;;; Helm Top
;;
;;
(defvar helm-top-help-message
  "* Helm Top

** Commands
\\<helm-top-map>
\\[helm-top-run-sort-by-com]\t\tSort by commands.
\\[helm-top-run-sort-by-cpu]\t\tSort by CPU usage.
\\[helm-top-run-sort-by-user]\t\tSort alphabetically by user.
\\[helm-top-run-sort-by-mem]\t\tSort by memory.")

;;; Helm APT
;;
;;
(defvar helm-apt-help-message
  "* Helm APT

** Commands
\\<helm-apt-map>
\\[helm-apt-show-all]\t\tShow all packages.
\\[helm-apt-show-only-installed]\t\tShow installed packages only.
\\[helm-apt-show-only-not-installed]\t\tShow non-installed packages only.
\\[helm-apt-show-only-deinstalled]\t\tShow uninstalled (not purged yet) packages only.")

;;; Helm Elisp package
;;
;;
(defvar helm-el-package-help-message
  "* Helm Elisp package

** Tips

*** Compile all your packages asynchronously.

If you use async (if you have installed Helm from MELPA you do), only \"helm\",
\"helm-core\", and \"magit\" are compiled asynchronously.  If you want all your
packages compiled asynchronously, add this to your init file:

     (setq async-bytecomp-allowed-packages '(all))

*** Upgrade Elisp packages

On initialization (when Emacs is fetching packages on remote), if Helm finds
packages to upgrade, it will start in the upgradable packages view showing the packages
available for upgrade.

On subsequent runs, you will have to refresh the list with `C-c \\[universal-argument]'.  If Helm
finds upgrades you can switch to upgrade view (see below) to see what packages
are available for upgrade or simply hit `C-c U' to upgrade them all.

To see upgradable packages hit `M-U'.

Then you can install all upgradable packages with the \"upgrade all\" action
\(`C-c \\[universal-argument]'), or upgrade only specific packages by marking them and running the
\"upgrade\" action (visible only when there are upgradable packages).  Of course
you can upgrade a single package by just running the \"upgrade\" action without
marking it (`C-c u' or `RET') .

\*Warning:* You are strongly advised to \*restart* Emacs after \*upgrading* packages.

*** Meaning of flags prefixing packages

\(Emacs ≥25)

- The flag \"S\" that prefixes package names means that the packages belong to `package-selected-packages'.

- The flag \"U\" that prefix package names mean that this package is no more needed.

** Commands
\\<helm-el-package-map>
\\[helm-el-package-show-all]\t\tShow all packages.
\\[helm-el-package-show-installed]\t\tShow installed packages only.
\\[helm-el-package-show-uninstalled]\t\tShow non-installed packages only.
\\[helm-el-package-show-upgrade]\t\tShow upgradable packages only.
\\[helm-el-package-show-built-in]\t\tShow built-in packages only.
\\[helm-el-run-package-install]\t\tInstall package(s).
\\[helm-el-run-package-reinstall]\t\tReinstall package(s).
\\[helm-el-run-package-uninstall]\t\tUninstall package(s).
\\[helm-el-run-package-upgrade]\t\tUpgrade package(s).
\\[helm-el-run-package-upgrade-all]\t\tUpgrade all packages.
\\[helm-el-run-visit-homepage]\t\tVisit package homepage.")

;;; Helm M-x
;;
;;
(defvar helm-M-x-help-message
  "* Helm M-x

** Tips

*** You can get help on any command with persistent action (\\[helm-execute-persistent-action]).

*** Prefix arguments

You must pass prefix arguments \*after* starting `helm-M-x'.  A mode-line
counter will display the number of given prefix arguments.

If you pass prefix arguments before running `helm-M-x', it will be displayed in the prompt.
The first `\\[universal-argument]' after `helm-M-x' clears those prefix arguments.")

;;; Helm imenu
;;
;;
(defvar helm-imenu-help-message
  "* Helm Imenu

** Commands
\\<helm-imenu-map>
\\[helm-imenu-next-section]\t\tGo to next section.
\\[helm-imenu-previous-section]\t\tGo to previous section.")

;;; Helm colors
;;
;;
(defvar helm-colors-help-message
  "* Helm colors

** Commands
\\<helm-color-map>
\\[helm-color-run-insert-name]\t\tInsert the entry name.
\\[helm-color-run-kill-name]\t\tKill the entry name.
\\[helm-color-run-insert-rgb]\t\tInsert entry in RGB format.
\\[helm-color-run-kill-rgb]\t\tKill entry in RGB format.")

;;; Helm Semantic
;;
;;
(defvar helm-semantic-help-message
  "* Helm Semantic

** Commands
\\<helm-semantic-map>")

;;; Helm kmacro
;;
;;
(defvar helm-kmacro-help-message
  "* Helm kmacro

** Tips

- Start recording a kmacro with `f3'.
- End the kmacro recording with `f4'.
- Run `helm-execute-kmacro' to list all your kmacros.

Use persistent action to run your kmacro as many time as needed.
You can browse the kmacros with `helm-next-line' and `helm-previous-line'.

Note: You can't record keys running Helm commands except `helm-M-x', under the
condition that you don't choose a command using Helm completion.

** Commands
\\<helm-kmacro-map>")

;;; Kill ring
;;
;;
(defvar helm-kill-ring-help-message
  "* Helm kill ring

** Tips

Every Helm session lets you save a candidate to the kill-ring / clipboard /
primary-selection with `\\<helm-map>\\[helm-kill-selection-and-quit]'.

To save space, Helm-kill-ring truncates the candidates longer than
`helm-kill-ring-max-offset'.
`\\<helm-kill-ring-map>\\[helm-kill-ring-kill-selection]' then saves the whole
text and not the truncated value.  The view of truncated candidates can be
toggled; see the command list below.

As opposed to `yank', numeric prefix arguments are ignored with
`helm-show-kill-ring': there is no need for them since selection happens within
Helm.  Moreover Helm has [[Shortcuts for executing Default Action on the nth
candidate][Shortcuts for executing Default Action on the nth candidate]].

It is recommended to globally bind `M-y' to `helm-show-kill-ring'.  Once in the
Helm-kill-ring session you can navigate to next/previous line with `M-y' and
`M-u' for convenience.  Of course `\\[helm-next-line]' and `\\[helm-previous-line]' are still available.

It is possible to delete candidates from the kill ring.

You can concatenate marked candidates and yank them in the current buffer, thus
creating a new entry in the kill ring.  See the commands below.  Candidates are
concatenated with a newline as separator.  Alternatively, use
`\\<helm-map>\\[helm-copy-to-buffer]' to not push a new entry in the kill ring.

When inserting candidates with the default action (`RET'), `point' is placed at
the end of the candidate and `mark' at the beginning.  You can revert this behavior
by using a prefix argument, i.e. `C-u RET', like the regular `yank' command does.

** Commands
\\<helm-kill-ring-map>
\\[helm-next-line]\t\tNext line.
\\[helm-previous-line]\t\tPrevious line.
\\[helm-kill-ring-delete]\t\tDelete entry.
\\[helm-kill-ring-run-append]\t\tYank concatenated marked candidates.
\\[helm-kill-ring-toggle-truncated]\t\tToggle truncated view of candidate.
\\[helm-kill-ring-kill-selection]\t\tKill non-truncated of selection.")

;;; Org headings
;;
;;
(defvar helm-org-headings-help-message
  "* Helm Org headings

** Tips

*** Refiling

You can refile one or more headings at a time.

To refile one heading, move the point to the entry you want to refile and run
\\[helm-org-in-buffer-headings].  Then select the heading you want to refile to
and press \\[C-c w] or select the refile action from the actions menu.

To refile multiple headings, run \\[helm-org-in-buffer-headings] and mark the
headings you want to refile.  Then select the heading you want to refile to
\(without marking it) and press \\[C-c w] or select the refile action from the
actions menu.

** Commands
\\<helm-org-headings-map>
\\[helm-org-run-open-heading-in-indirect-buffer]\t\tOpen heading in indirect buffer.
\\[helm-org-run-refile-heading-to]\t\tRefile current or marked headings to selection.
\\[helm-org-run-insert-link-to-heading-at-marker]\t\tInsert link at point to selection."
  )

;;; Completing-read
;;
(defun helm-comp-read-help-message ()
  (let ((com (assoc-default 'name (helm-get-current-source))))
    (format
     "* Helm completing-read completion for `%s'

Command `%s' is using a `completing-read' for completion on your input,
this completion have been \"helmized\" because you have enabled [[Helm mode][helm-mode]]'.

** Tips

*** Disabling or use something else than helm for completion of some commands

You can disable helm completion or use something else for specific commands of your choice,
for this customize variable `helm-completing-read-handlers-alist'.

*** Exiting minibuffer with empty string

You can exit minibuffer with empty string with \\<helm-comp-read-map>\\[helm-cr-empty-string].
It is useful when some commands are prompting continuously until you enter an empty prompt.

** Commands
\\<helm-comp-read-map>
\\[helm-cr-empty-string]\t\tExit minibuffer with empty string."
     com com)))


;;; Mode line strings
;;
;;
;;;###autoload
(defvar helm-comp-read-mode-line "\
\\<helm-comp-read-map>\
C/\\[helm-cr-empty-string]:Empty \
\\<helm-map>\
\\[helm-help]:Help \
\\[helm-select-action]:Act \
\\[helm-maybe-exit-minibuffer]/\
f1/f2/f-n:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend")

;;;###autoload
(defvar helm-read-file-name-mode-line-string "\
\\<helm-read-file-map>\
\\[helm-help]:Help \
C/\\[helm-cr-empty-string]:Empty \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-maybe-exit-minibuffer]/\
f1/f2/f-n:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend"
  "String displayed in mode-line in `helm-source-find-files'.")

;;;###autoload
(defvar helm-top-mode-line "\
\\<helm-top-map>\
\\[helm-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-maybe-exit-minibuffer]/\
f1/f2/f-n:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend")


(provide 'helm-help)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-help.el ends here
