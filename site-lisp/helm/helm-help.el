;;; helm-help.el --- Help messages for Helm. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

(defgroup helm-help nil
  "Embedded help for `helm'."
  :group 'helm)

(defface helm-helper
    '((t :inherit helm-header))
  "Face for helm help string in minibuffer."
  :group 'helm-help)


;;; Embeded documentation.
;;
;;
;;;###autoload
(defvar helm-mode-line-string "\
\\<helm-map>\
\\[helm-help]:Help \
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "Help string displayed in mode-line in `helm'.
It can be a string or a list of two args, in this case,
first arg is a string that will be used as name for candidates number,
second arg any string to display in mode line.
If nil, use default `mode-line-format'.")


;;; Global help message - Used by `helm-help'
;;
;;
(defvar helm-help-message
  (lambda ()
    (concat
     "\\<helm-map>"
     "`helm' is an Emacs incremental completion and selection narrowing framework.

Narrow the list by typing some pattern,
Multiple patterns are allowed by splitting by space.
Select with natural Emacs operations, choose with RET.

== Basic Operations ==
C-p, Up: Previous Line
C-n, Down : Next Line
M-v, PageUp : Previous Page
C-v, PageDown : Next Page
Enter : Execute first (default) action / Select
M-< : First Line
M-> : Last Line
M-PageUp, C-M-S-v, C-M-y : Previous Page (other-window)
M-PageDown, C-M-v : Next Page (other-window)

Tab, C-i : Show action list
Left : Previous Source
Right, C-o : Next Source
C-k : Delete pattern
C-z : Persistent Action (Execute action with helm session kept)

== Shortcuts For 2nd/3rd Action ==
\\[helm-select-2nd-action-or-end-of-line] : Execute 2nd Action (if the minibuffer cursor is at end of line)
\\[helm-select-3rd-action] : Execute 3rd Action

== Visible Marks ==
Visible marks store candidate. Some actions uses marked candidates.

\\[helm-toggle-visible-mark] : Toggle Visible Mark
\\[helm-prev-visible-mark] : Previous Mark
\\[helm-next-visible-mark] : Next Mark

== Miscellaneous Commands ==
\\[helm-toggle-resplit-window] : Toggle vertical/horizontal split helm window
\\[helm-quit-and-find-file] : Drop into `find-file'
\\[helm-delete-current-selection] : Delete Selected Item (visually)
\\[helm-kill-selection-and-quit] : Set Item Into the kill-ring And Quit
\\[helm-yank-selection] : Yank Selected Item Into Pattern
\\[helm-follow-mode] : Toggle Automatical Execution Of Persistent Action
\\[helm-force-update] : Recalculate And Redisplay Candidates

== Global Commands ==
\\<global-map>\\[helm-resume] revives last `helm' session.
It is very useful, so you should bind any key."))
  "Detailed help message string for `helm'.
It also accepts function or variable symbol.")

(defun helm-help-internal (bufname insert-content-fn)
  "Show long message during `helm' session in BUFNAME.
INSERT-CONTENT-FN is the function that insert
text to be displayed in BUFNAME."
  (let ((winconf (current-frame-configuration)))
    (unwind-protect
         (progn
           (setq helm-suspend-update-flag t)
           (switch-to-buffer (get-buffer-create bufname))
           (delete-other-windows)
           (erase-buffer)
           (funcall insert-content-fn)
           (setq cursor-type nil)
           (goto-char 1)
           (helm-help-event-loop))
      (setq helm-suspend-update-flag nil)
      (set-frame-configuration winconf))))

(defun helm-help-event-loop ()
  (let ((prompt (propertize
                 "[SPC,C-v,down:NextPage  b,M-v,up:PrevPage  C-s/r:Isearch Other:Exit]"
                 'face 'helm-helper))
        (scroll-error-top-bottom t))
    (condition-case _err
        (cl-loop for event = (read-key prompt) do
              (cl-case event
                ((?\C-v ? down) (scroll-up-command helm-scroll-amount))
                ((?\M-v ?b up)  (scroll-down-command helm-scroll-amount))
                ((?\C-s)        (isearch-forward))
                ((?\C-r)        (isearch-backward))
                (t (cl-return))))
      (beginning-of-buffer (message "Beginning of buffer"))
      (end-of-buffer       (message "End of Buffer")))))

;;;###autoload
(defun helm-help ()
  "Help of `helm'."
  (interactive)
  (save-selected-window
    (helm-help-internal
     " *Helm Help*"
     (lambda ()
       (insert (substitute-command-keys
                (helm-interpret-value (or (assoc-default
                                           'help-message
                                           (helm-get-current-source))
                                          helm-help-message))))))))

;;; `helm-buffer-list' help
;;
;;
(defvar helm-buffer-help-message
  "== Helm Buffer ==
\nTips:

Completion:

You can enter a partial name of major-mode (e.g lisp, sh) to narrow down buffers.
To specify the major-mode, prefix it with \"*\" e.g \"*lisp\".
If you want to match all buffers but the ones with a specific major-mode (negation),
prefix the major-mode with \"!\" e.g \"*!lisp\".
If you want to specify more than one major-mode, separate them with \",\",
e.g \"*!lisp,!sh,!fun\" will list all buffers but the ones in lisp-mode, sh-mode and
fundamental-mode.
Enter then a space and a pattern to narrow down to buffers matching this pattern.
If you enter a space and a pattern prefixed by \"@\" helm will search for text matching
this pattern INSIDE the buffer (i.e not in the name of buffer).
NOTE that if you enter your pattern prefixed with \"@\" but escaped, helm will search a buffer
matching \"@pattern\" but will not search inside.
If you prefix the beginning of pattern with \"/\" the match will occur on directory name
of buffer, it is interesting to narrow down to one directory for example, subsequent string
entered after a space will match on buffer-name only.
Note that negation is not supported for matching on buffer-file-name.

Note that if `helm-buffers-fuzzy-matching' is non--nil you will have
fuzzy matching on buffer names (not on buffer-file-name matching and major-mode though).


e.g

if I enter in pattern prompt:
\"*lisp ^helm @moc\"
helm will narrow down the list by selecting only buffers that are in lisp mode, start by helm
and match \"moc\" in their contents.

if I enter in pattern prompt:
\"*lisp ^helm moc\"
Notice there is no \"@\" this time
helm will look for lisp mode buffers starting by \"helm\" and have \"moc\" in their name.

if I enter in pattern prompt:
\"*!lisp !helm\"
helm will narrow down to buffers that are not in \"lisp\" mode and that do not match \"helm\"

if I enter in pattern prompt:
/helm/ w3
helm will narrow down to buffers that are in any \"helm\" subdirectory and matching w3.


Creating buffers

When creating a new buffer use \\[universal-argument] to choose a mode for your buffer in a list.
This list is customizable, see `helm-buffers-favorite-modes'.

Killing buffers

You have a command to kill buffer(s) and quit emacs and a command to kill buffers one by one
\(no marked\) without quitting helm.
You can run this persistent kill buffer command either with the regular
`helm-execute-persistent-action' called with a prefix arg (C-u C-z) or with its specific command
`helm-buffer-run-kill-persistent' see binding below.

Meaning of colors and prefixes for buffers:

Remote buffers are prefixed with '@'.
Red        => Buffer have its file modified on disk by an external process.
Indianred2 => Buffer exists but its file have been deleted.
Orange     => Buffer is modified and its file not saved to disk.
Italic     => A non--file buffer.

\nSpecific commands for `helm-buffers-list':
\\<helm-buffer-map>
\\[helm-buffer-run-zgrep]\t\t->Grep Buffer(s) works as zgrep too (C-u grep all buffers but non--file buffers).
\\[helm-buffers-run-multi-occur]\t\t->Multi Occur buffer or marked buffers. (C-u toggle force searching current-buffer).
\\[helm-buffer-switch-other-window]\t\t->Switch other window.
\\[helm-buffer-switch-other-frame]\t\t->Switch other frame.
\\[helm-buffer-run-query-replace-regexp]\t\t->Query replace regexp in marked buffers.
\\[helm-buffer-run-query-replace]\t\t->Query replace in marked buffers.
\\[helm-buffer-run-ediff]\t\t->Ediff current buffer with candidate.  If two marked buffers ediff those buffers.
\\[helm-buffer-run-ediff-merge]\t\t->Ediff merge current buffer with candidate.  If two marked buffers ediff merge those buffers.
\\[helm-buffer-diff-persistent]\t\t->Toggle Diff buffer with saved file without quitting.
\\[helm-buffer-revert-persistent]\t\t->Revert buffer without quitting.
\\[helm-buffer-save-persistent]\t\t->Save buffer without quitting.
\\[helm-buffer-run-kill-buffers]\t\t->Delete marked buffers and quit.
\\[helm-buffer-run-kill-persistent]\t\t->Delete buffer without quitting helm.
\\[helm-toggle-all-marks]\t\t->Toggle all marks.
\\[helm-mark-all]\t\t->Mark all.
\\[helm-toggle-buffers-details]\t\t->Toggle details.
\\[helm-buffers-toggle-show-hidden-buffers]\t\t->Show hidden buffers.
\\[helm-buffer-help]\t\t->Display this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-buffer-help ()
  "Help command for helm buffers."
  (interactive)
  (let ((helm-help-message helm-buffer-help-message))
    (helm-help)))

;;; Find files help (`helm-find-files')
;;
;;
(defvar helm-ff-help-message
  "== Helm Find Files ==
\nTips:
\n- Enter `~/' at end of pattern to quickly reach home directory.

- Enter `/' at end of pattern to quickly reach root of your file system.

- Enter `./' at end of pattern to quickly reach `default-directory' (initial start of session).
  If you are in `default-directory' move cursor on top.

- Enter `../' at end of pattern will reach upper directory, moving cursor on top.
  NOTE: This different to using `C-l' in that `C-l' don't move cursor on top but stay on previous
  subdir name.

- You can complete with partial basename (start on third char entered)

    e.g \"fob\" or \"fbr\" will complete \"foobar\"
    but \"fb\" will wait for a third char for completing.

- Use `C-u C-z' to watch an image.

- `C-z' on a filename will expand in helm-buffer to this filename.
  Second hit on `C-z' will display buffer filename.
  Third hit on `C-z' will kill buffer filename.
  NOTE: `C-u C-z' will display buffer directly.

- To browse images directories turn on `helm-follow-mode' and navigate with arrow keys.
  You can also use `helm-follow-action-forward' and `helm-follow-action-backward'
  (`C-<down' and `C-<left>').

- You can turn off/on (toggle) autoupdate completion at any moment with `C-DEL'.
  NOTE: On a terminal C-<backspace> may not work, use in this case C-c <backspace>.

- You can create a new directory and a new file at the same time, just write the path in prompt
  and press `<RET>'.
  e.g You can create \"~/new/newnew/newnewnew/my_newfile.txt\".

- To create a new directory, add a \"/\" at end of new name and press <RET>.

- To create a new file just write the filename not ending with \"/\".

- You can start a recursive search with Locate of Find (See commands below).
  With Locate you can use a local db with a prefix arg; If the localdb doesn't already
  exists, you will be prompted for its creation, if it exists and you want to refresh it,
  give two prefix args.

\nSpecific commands for `helm-find-files':
\\<helm-find-files-map>
\\[helm-ff-run-locate]\t\t->Run Locate (C-u to specify locate db, M-n insert basename of candidate)
\\[helm-ff-run-find-sh-command]\t\t->Run Find shell command from this directory.
\\[helm-ff-run-grep]\t\t->Run Grep (C-u Recursive).
\\[helm-ff-run-pdfgrep]\t\t->Run Pdfgrep on marked files.
\\[helm-ff-run-zgrep]\t\t->Run zgrep (C-u Recursive).
\\[helm-ff-run-etags]\t\t->Run Etags (C-u use thing-at-point `C-u C-u' reload cache)
\\[helm-ff-run-rename-file]\t\t->Rename File (C-u Follow).
\\[helm-ff-run-copy-file]\t\t->Copy File (C-u Follow).
\\[helm-ff-run-byte-compile-file]\t\t->Byte Compile File (C-u Load).
\\[helm-ff-run-load-file]\t\t->Load File.
\\[helm-ff-run-symlink-file]\t\t->Symlink File.
\\[helm-ff-run-hardlink-file]\t\t->Hardlink file.
\\[helm-ff-run-delete-file]\t\t->Delete File.
\\[helm-ff-run-kill-buffer-persistent]\t\t->Kill buffer candidate without quitting.
\\[helm-ff-persistent-delete]\t\t->Delete file without quitting.
\\[helm-ff-run-switch-to-eshell]\t\t->Switch to Eshell.
\\[helm-ff-run-eshell-command-on-file]\t\t->Eshell command on file (C-u Apply on marked files, otherwise treat them sequentially).
\\[helm-ff-run-ediff-file]\t\t->Ediff file.
\\[helm-ff-run-ediff-merge-file]\t\t->Ediff merge file.
\\[helm-ff-run-complete-fn-at-point]\t\t->Complete file name at point.
\\[helm-ff-run-switch-other-window]\t\t->Switch other window.
\\[helm-ff-run-switch-other-frame]\t\t->Switch other frame.
\\[helm-ff-run-open-file-externally]\t\t->Open file with external program (C-u to choose).
\\[helm-ff-run-open-file-with-default-tool]\t\t->Open file externally with default tool.
\\[helm-ff-rotate-left-persistent]\t\t->Rotate Image Left.
\\[helm-ff-rotate-right-persistent]\t\t->Rotate Image Right.
\\[helm-find-files-up-one-level]\t\t->Go down precedent directory.
\\[helm-ff-run-switch-to-history]\t\t->Switch to last visited directories history.
\\[helm-ff-file-name-history]\t\t->Switch to file name history.
\\[helm-ff-properties-persistent]\t\t->Show file properties in a tooltip.
\\[helm-mark-all]\t\t->Mark all visibles candidates.
\\[helm-ff-run-toggle-auto-update]\t->Toggle auto expansion of directories.
\\[helm-unmark-all]\t\t->Unmark all candidates, visibles and invisibles.
\\[helm-ff-run-gnus-attach-files]\t\t->Gnus attach files to message buffer.
\\[helm-ff-run-print-file]\t\t->Print file, (C-u to refresh printers list).
\\[helm-enlarge-window]\t\t->Enlarge helm window.
\\[helm-narrow-window]\t\t->Narrow helm window.
\\[helm-ff-run-toggle-basename]\t\t->Toggle basename/fullpath.
\\[helm-ff-run-find-file-as-root]\t\t->Find file as root.
\\[helm-ff-run-insert-org-link]\t\t->Insert org link.
\\[helm-ff-help]\t\t->Display this help info.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-ff-help ()
  "Help command for `helm-find-files'."
  (interactive)
  (let ((helm-help-message helm-ff-help-message))
    (helm-help)))

;;; Help for `helm-read-file-name'
;;
;;
(defvar helm-read-file-name-help-message
  "== Helm read file name ==\
\nTips:
\n- Enter `~/' at end of pattern to quickly reach home directory.

- Enter `/' at end of pattern to quickly reach root of your file system.

- Enter `./' at end of pattern to quickly reach `default-directory' (initial start of session).
  If you are in `default-directory' move cursor on top.

- Enter `../' at end of pattern will reach upper directory, moving cursor on top.
  NOTE: This different to using `C-l' in that `C-l' don't move cursor on top but stay on previous
  subdir name.

- You can complete with partial basename (start on third char entered)

    e.g \"fob\" or \"fbr\" will complete \"foobar\"
    but \"fb\" will wait for a third char for completing.

Persistent actions:

By default `helm-read-file-name' use the persistent actions of `helm-find-files'.

- Use `C-u C-z' to watch an image.

- `C-z' on a filename will expand in helm-buffer to this filename.
  Second hit on `C-z' will display buffer filename.
  Third hit on `C-z' will kill buffer filename.
  NOTE: `C-u C-z' will display buffer directly.

- To browse images directories turn on `helm-follow-mode' and navigate with arrow keys.

- When you want to delete backward characters to e.g creating a new file or directory,
  autoupdate may keep updating to an existent directory
  preventing you to do so, in this case just hit C-<backspace> and then <backspace>.
  This should not needed when copying/renaming files because autoupdate is disabled
  by default in this case.
  NOTE: On a terminal C-<backspace> may not work, use in this case C-c <backspace>.

- You can create a new directory and a new file at the same time, just write the path in prompt
  and press <RET>.
  e.g You can create \"~/new/newnew/newnewnew/my_newfile.txt\".

- To create a new directory, add a \"/\" at end of new name and press <RET>.

- To create a new file just write the filename not ending with \"/\".

\nSpecific commands for helm-read-file-name:
\\<helm-read-file-map>
\\[helm-find-files-up-one-level]\t\t->Go down precedent directory.
\\[helm-ff-run-toggle-auto-update]\t->Toggle auto expansion of directories.
\\[helm-ff-run-toggle-basename]\t\t->Toggle basename.
\\[helm-ff-file-name-history]\t\t->File name history.
\\[helm-cr-empty-string]\t->Maybe return empty string (unless `must-match').
\\[helm-next-source]\t->Goto next source.
\\[helm-previous-source]\t->Goto previous source.
\\[helm-read-file-name-help]\t\t->Display this help info.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-read-file-name-help ()
  (interactive)
  (let ((helm-help-message helm-read-file-name-help-message))
    (helm-help)))

;;; Generic file help - Used by locate.
;;
;;
(defvar helm-generic-file-help-message
  "== Helm Generic files Map ==\

\nLocate tips:
You can add after writing search pattern any of the locate command line options.
e.g -b, -e, -n <number>...etc.
See Man locate for more infos.

Note:
Some other sources (at the moment recentf and file in current directory sources)
support the -b flag for compatibility with locate when they are used with it.

\nSpecific commands for helm locate and others files sources:
\\<helm-generic-files-map>
\\[helm-ff-run-toggle-basename]\t\t->Toggle basename.
\\[helm-ff-run-grep]\t\t->Run grep (C-u recurse).
\\[helm-ff-run-pdfgrep]\t\t->Run Pdfgrep on marked files.
\\[helm-ff-run-delete-file]\t\t->Delete file.
\\[helm-ff-run-ediff-file]\t\t->Ediff file.
\\[helm-ff-run-ediff-merge-file]\t\t->Ediff merge file.
\\[helm-ff-run-switch-other-window]\t\t->Switch other window.
\\[helm-ff-properties-persistent]\t\t->Show file properties.
\\[helm-ff-run-etags]\t\t->Run etags (C-u use tap, C-u C-u reload DB).
\\[helm-yank-text-at-point]\t\t->Yank text at point.
\\[helm-ff-run-open-file-externally]\t\t->Open file with external program (C-u to choose).
\\[helm-ff-run-open-file-with-default-tool]\t\t->Open file externally with default tool.
\\[helm-ff-run-insert-org-link]\t\t->Insert org link.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-generic-file-help ()
  (interactive)
  (let ((helm-help-message helm-generic-file-help-message))
    (helm-help)))

;;; Grep help
;;
;;
(defvar helm-grep-help-message
  "== Helm Grep Map ==\
\nHelm Grep tips:
You can start grep with a prefix arg to recurse in subdirectories.
You can use wild card when selecting files (e.g *.el)
You can grep in many differents directories by marking files or wild cards.
You can save your results in a grep-mode buffer, see below.

\nSpecific commands for Helm Grep:
\\<helm-grep-map>
\\[helm-goto-next-file]\t->Next File.
\\[helm-goto-precedent-file]\t\t->Precedent File.
\\[helm-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[helm-grep-run-other-window-action]\t\t->Jump other window.
\\[helm-grep-run-other-frame-action]\t\t->Jump other frame.
\\[helm-grep-run-persistent-action]\t\t->Run persistent action (Same as `C-z').
\\[helm-grep-run-default-action]\t\t->Run default action (Same as RET).
\\[helm-grep-run-save-buffer]\t\t->Save to a `grep-mode' enabled buffer.
\\[helm-grep-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-grep-help ()
  (interactive)
  (let ((helm-help-message helm-grep-help-message))
    (helm-help)))

;;; Pdf grep help
;;
;;
(defvar helm-pdfgrep-help-message
  "== Helm PdfGrep Map ==\
\nSpecific commands for Pdf Grep:
\\<helm-pdfgrep-map>
\\[helm-goto-next-file]\t->Next File.
\\[helm-goto-precedent-file]\t\t->Precedent File.
\\[helm-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[helm-pdfgrep-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-pdfgrep-help ()
  (interactive)
  (let ((helm-help-message helm-pdfgrep-help-message))
    (helm-help)))

;;; Etags help
;;
;;
(defvar helm-etags-help-message
  "== Helm Etags Map ==\
\nSpecific commands for Etags:
\\<helm-etags-map>
\\[helm-goto-next-file]\t->Next File.
\\[helm-goto-precedent-file]\t\t->Precedent File.
\\[helm-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[helm-etags-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-etags-help ()
  "The help function for etags."
  (interactive)
  (let ((helm-help-message helm-etags-help-message))
    (helm-help)))

;;; Ucs help
;;
;;
(defvar helm-ucs-help-message
  "== Helm Ucs ==
\nSpecific commands for `helm-ucs':
\\<helm-ucs-map>
\\[helm-ucs-persistent-insert]\t->Insert char.
\\[helm-ucs-persistent-forward]\t->Forward char.
\\[helm-ucs-persistent-backward]\t->Backward char.
\\[helm-ucs-persistent-delete]\t->Delete char backward.
\\[helm-ucs-help]\t\t->Show this help.

\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-ucs-help ()
  "Help command for `helm-ucs'."
  (interactive)
  (let ((helm-help-message helm-ucs-help-message))
    (helm-help)))

;;; Bookmark help
;;
;;
(defvar helm-bookmark-help-message
  "== Helm bookmark name Map ==\
\nSpecific commands for bookmarks:
\\<helm-bookmark-map>
\\[helm-bookmark-run-jump-other-window]\t\t->Jump other window.
\\[helm-bookmark-run-delete]\t\t->Delete bookmark.
\\[helm-bookmark-run-edit]\t\t->Edit bookmark.
\\[helm-bmkext-run-sort-by-frequency]\t\t->Sort by frequency (only for bmkext).
\\[helm-bmkext-run-sort-by-last-visit]\t\t->Sort by last visited (only for bmkext).
\\[helm-bmkext-run-sort-alphabetically]\t\t->Sort alphabetically (only for bmkext).
\\[helm-bookmark-toggle-filename]\t\t->Toggle bookmark location visibility.
\\[helm-bookmark-help]\t\t->Run this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-bookmark-help ()
  "Help command for bookmarks."
  (interactive)
  (let ((helm-help-message helm-bookmark-help-message))
    (helm-help)))

;;; Eshell command on file help
;;
;;
(defvar helm-esh-help-message
  "== Helm eshell on file ==
\nTips:

- Passing extra args after filename:

Normally your command or alias will be called with file as argument.

e.g <command> 'candidate_file'

But you can also pass an argument or more after 'candidate_file' like this:

<command> %s [extra_args]\n

'candidate_file' will be inserted at '%s' and your command will look at this:

<command> 'candidate_file' [extra_args]

- Specify many files as args (marked files):

e.g <command> file1 file2 ...

Call `helm-find-files-eshell-command-on-file' with one prefix-arg
Otherwise you can pass one prefix-arg from the command selection buffer.

With two prefix-arg before starting or from the command selection buffer
the output is printed to your `current-buffer'.

With no prefix-arg or a prefix-arg value of '(16) (C-u C-u) the command
is called once for each file like this:

<command> file1 <command> file2 etc...

\nSpecific commands for `helm-find-files-eshell-command-on-file':
\\<helm-esh-on-file-map>
\\[helm-esh-help]\t\t->Display this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-esh-help ()
  "Help command for `helm-find-files-eshell-command-on-file'."
  (interactive)
  (let ((helm-help-message helm-esh-help-message))
    (helm-help)))

;;; Ido virtual buffer help
;;
;;
(defvar helm-buffers-ido-virtual-help-message
  "== Helm ido virtual buffers Map ==\
\nSpecific commands for ido virtuals buffers:
\\<helm-buffers-ido-virtual-map>
\\[helm-ff-run-switch-other-window]\t\t->Switch other window.
\\[helm-ff-run-switch-other-frame]\t\t->Switch other frame.
\\[helm-ff-run-grep]\t\t->Grep file.
\\[helm-ff-run-zgrep]\t\t->Zgrep file.
\\[helm-ff-run-delete-file]\t\t->Delete file.
\\[helm-ff-run-open-file-externally]\t\t->Open file externally.
\\[helm-buffers-ido-virtual-help]\t\t->Display this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-buffers-ido-virtual-help ()
  "Help command for ido virtual buffers."
  (interactive)
  (let ((helm-help-message helm-buffers-ido-virtual-help-message))
    (helm-help)))

;;; Moccur help
;;
;;
(defvar helm-moccur-help-message
  "== Helm Moccur Map ==\
\nHelm Moccur tips:

\nSpecific commands for Helm Moccur:
\\<helm-moccur-map>
\\[helm-goto-next-file]\t->Next Buffer.
\\[helm-goto-precedent-file]\t\t->Precedent Buffer.
\\[helm-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[helm-moccur-run-goto-line-ow]\t\t->Goto line in other window.
\\[helm-moccur-run-goto-line-of]\t\t->Goto line in new frame.
\\[helm-moccur-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-moccur-help ()
  (interactive)
  (let ((helm-help-message helm-moccur-help-message))
    (helm-help)))

;;; Helm Top
;;
;;
(defvar helm-top-help-message
  "== Helm Top Map ==\
\nHelm Top tips:

\nSpecific commands for Helm Top:
\\<helm-top-map>
\\[helm-top-run-sort-by-com]\t->Sort by commands.
\\[helm-top-run-sort-by-cpu]\t->Sort by cpu usage.
\\[helm-top-run-sort-by-user]\t->Sort alphabetically by user.
\\[helm-top-run-sort-by-mem]\t->Sort by memory.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-top-help ()
  (interactive)
  (let ((helm-help-message helm-top-help-message))
    (helm-help)))

;;; Helm Apt
;;
;;
(defvar helm-apt-help-message
  "== Helm Apt Map ==\
\nHelm Apt tips:

\nSpecific commands for Helm Apt:
\\<helm-apt-map>
\\[helm-apt-show-all]\t->Show all packages.
\\[helm-apt-show-only-installed]\t->Show installed packages only.
\\[helm-apt-show-only-not-installed]\t->Show not installed packages only.
\\[helm-apt-show-only-deinstalled]\t-Show deinstalled (not purged yet) packages only.>
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-apt-help ()
  (interactive)
  (let ((helm-help-message helm-apt-help-message))
    (helm-help)))

;;; Helm elisp package
;;
;;
(defvar helm-el-package-help-message
  "== Helm elisp package Map ==\
\nHelm elisp package tips:

\nSpecific commands for Helm elisp package:
\\<helm-el-package-map>
\\[helm-el-package-show-all]\t->Show all packages.
\\[helm-el-package-show-installed]\t->Show installed packages only.
\\[helm-el-package-show-uninstalled]\t->Show not installed packages only.
\\[helm-el-package-help]\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-el-package-help ()
  (interactive)
  (let ((helm-help-message helm-el-package-help-message))
    (helm-help)))

;;; Helm M-x
;;
;;
(defvar helm-M-x-help-message
  "== Helm M-x ==\
\nHelm M-x tips:

You can get help on any command with persistent action (C-z).

All the prefix args passed BEFORE running `helm-M-x' are ignored.
When you want to pass prefix args, pass them AFTER starting `helm-M-x',
you will have a prefix arg counter appearing in mode-line notifying you
the amount of prefix args entered.

\nSpecific commands for Helm M-x:
\\<helm-M-x-map>
\\[helm-M-x-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-M-x-help ()
  (interactive)
  (let ((helm-help-message helm-M-x-help-message))
    (helm-help)))

;;; helm-imenu
;;
;;
(defvar helm-imenu-help-message
  "== Helm imenu ==\
\nHelm imenu tips:

\nSpecific commands for Helm imenu:
\\<helm-imenu-map>
\\[helm-imenu-help]\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-imenu-help ()
  (interactive)
  (let ((helm-help-message helm-imenu-help-message))
    (helm-help)))

;;; helm-colors
;;
;;
(defvar helm-colors-help-message
  "== Helm colors ==\
\nSpecific commands for Helm colors:
\\<helm-color-map>
\\[helm-color-run-insert-name]\t\tInsert the entry'name.
\\[helm-color-run-kill-name]\t\tKill the entry's name.
\\[helm-color-run-insert-rgb]\t\tInsert entry in RGB format.
\\[helm-color-run-kill-rgb]\t\tKill entry in RGB format.
\\[helm-color-help]\t\tShow this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-color-help ()
  (interactive)
  (let ((helm-help-message helm-colors-help-message))
    (helm-help)))

;;; helm semantic
;;
;;
(defvar helm-semantic-help-message
  "== Helm semantic ==\
\nHelm semantic tips:

\nSpecific commands for Helm semantic:
\\<helm-semantic-map>
\\[helm-semantic-help]\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-semantic-help ()
  (interactive)
  (let ((helm-help-message helm-semantic-help-message))
    (helm-help)))


;;; Mode line strings
;;
;;
;;;###autoload
(defvar helm-buffer-mode-line-string
  '("Buffer(s)" "\
\\<helm-buffer-map>\
\\[helm-buffer-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
    "String displayed in mode-line in `helm-source-buffers-list'"))

;;;###autoload
(defvar helm-color-mode-line-string
  '("Colors" "\
\\<helm-color-map>\
\\[helm-color-help]:Help/\
\\[helm-color-run-insert-name]:Insert name/\
\\[helm-color-run-insert-rgb]:Insert RGB/\
with shift: Kill"))

;;;###autoload
(defvar helm-buffers-ido-virtual-mode-line-string
  '("Killed Buffer(s)" "\
\\<helm-buffers-ido-virtual-map>\
\\[helm-buffers-ido-virtual-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
    "String displayed in mode-line in `helm-source-buffers-list'"))

;;;###autoload
(defvar helm-ff-mode-line-string "\
\\<helm-find-files-map>\
\\[helm-ff-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-source-find-files'")

;;;###autoload
(defvar helm-read-file-name-mode-line-string "\
\\<helm-read-file-map>\
\\[helm-read-file-name-help]:Help \
\\[helm-cr-empty-string]:Empty \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-source-find-files'.")

;;;###autoload
(defvar helm-generic-file-mode-line-string "\
\\<helm-generic-files-map>\
\\[helm-generic-file-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend"
  "String displayed in mode-line in Locate.")

;;;###autoload
(defvar helm-grep-mode-line-string"\
\\<helm-grep-map>\
\\[helm-grep-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend"
  "String displayed in mode-line in `helm-do-grep'.")

;;;###autoload
(defvar helm-pdfgrep-mode-line-string "\
\\<helm-pdfgrep-map>\
\\[helm-pdfgrep-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend"
  "String displayed in mode-line in `helm-do-pdfgrep'.")

;;;###autoload
(defvar helm-etags-mode-line-string "\
\\<helm-etags-map>\
\\[helm-etags-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-etags-select'.")

;;;###autoload
(defvar helm-ucs-mode-line-string "\
\\<helm-ucs-map>\
\\[helm-ucs-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-ucs'.")

;;;###autoload
(defvar helm-bookmark-mode-line-string
  '("Bookmark(s)" "\
\\<helm-bookmark-map>\
\\[helm-bookmark-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct")
  "String displayed in mode-line in `helm-source-buffers-list'")

;;;###autoload
(defvar helm-occur-mode-line "\
\\<helm-map>\
\\[helm-help]:Help \
\\<helm-occur-map>\
\\[helm-occur-run-query-replace-regexp]:Query replace regexp \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend")

;;;###autoload
(defvar helm-moccur-mode-line "\
\\<helm-moccur-map>\
\\[helm-moccur-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend")

;;;###autoload
(defvar helm-comp-read-mode-line "\
\\<helm-comp-read-map>\
\\[helm-cr-empty-string]:Empty \
\\<helm-map>\
\\[helm-help]:Help \
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct")

;;;###autoload
(defvar helm-top-mode-line "\
\\<helm-top-map>\
\\[helm-top-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend")

;;;###autoload
(defvar helm-apt-mode-line "\
\\<helm-apt-map>\
\\[helm-apt-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend")

;;;###autoload
(defvar helm-el-package-mode-line "\
\\<helm-el-package-map>\
\\[helm-el-package-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend")

;;;###autoload
(defvar helm-M-x-mode-line "\
\\<helm-M-x-map>\
\\[helm-M-x-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend")

;;;###autoload
(defvar helm-imenu-mode-line "\
\\<helm-imenu-map>\
\\[helm-imenu-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend")

;;;###autoload
(defvar helm-semantic-mode-line "\
\\<helm-semantic-map>\
\\[helm-semantic-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend")


;;; Attribute Documentation
;;
;;
;;;###autoload
(defun helm-describe-helm-attribute (helm-attribute)
  "Display the full documentation of HELM-ATTRIBUTE.
HELM-ATTRIBUTE should be a symbol."
  (interactive (list (intern
                      (completing-read
                       "Describe helm attribute: "
                       (mapcar 'symbol-name helm-attributes)
                       nil t))))
  (with-output-to-temp-buffer "*Help*"
    (princ (get helm-attribute 'helm-attrdoc))))

(helm-document-attribute 'name "mandatory"
  "  The name of the source. It is also the heading which appears
  above the list of matches from the source. Must be unique.")

(helm-document-attribute 'header-name "optional"
  "  A function returning the display string of the header. Its
  argument is the name of the source. This attribute is useful to
  add an additional information with the source name.")

(helm-document-attribute 'candidates "mandatory if candidates-in-buffer attribute is not provided"
  "  Specifies how to retrieve candidates from the source. It can
  either be a variable name, a function called with no parameters
  or the actual list of candidates.

  The list must be a list whose members are strings, symbols
  or (DISPLAY . REAL) pairs.

  In case of (DISPLAY . REAL) pairs, the DISPLAY string is shown
  in the Helm buffer, but the REAL one is used as action
  argument when the candidate is selected. This allows a more
  readable presentation for candidates which would otherwise be,
  for example, too long or have a common part shared with other
  candidates which can be safely replaced with an abbreviated
  string for display purposes.

  Note that if the (DISPLAY . REAL) form is used then pattern
  matching is done on the displayed string, not on the real
  value.

  If the candidates have to be retrieved asynchronously (for
  example, by an external command which takes a while to run)
  then the function should start the external command
  asynchronously and return the associated process object.
  Helm will take care of managing the process (receiving the
  output from it, killing it if necessary, etc.). The process
  should return candidates matching the current pattern (see
  variable `helm-pattern'.)
  You should use instead `candidates-process' attribute for
  async processes, a warning will popup when using async process
  in a `candidates' attribute.

  Note that currently results from asynchronous sources appear
  last in the helm buffer regardless of their position in
  `helm-sources'.")

(helm-document-attribute 'candidates-process
    "Same as `candidates' attributes but for process function."
  "  You should use this attribute when using a function involving
  an async process instead of `candidates'.")

(helm-document-attribute 'action "mandatory if type attribute is not provided"
  "  It is a list of (DISPLAY . FUNCTION) pairs or FUNCTION.
  FUNCTION is called with one parameter: the selected candidate.

  An action other than the default can be chosen from this list
  of actions for the currently selected candidate (by default
  with TAB). The DISPLAY string is shown in the completions
  buffer and the FUNCTION is invoked when an action is
  selected. The first action of the list is the default.")

(helm-document-attribute 'coerce "optional"
  "  It's a function called with one argument: the selected
  candidate.

  This function is intended for type convertion. In normal case,
  the selected candidate (string) is passed to action
  function. If coerce function is specified, it is called just
  before action function.

  Example: converting string to symbol
    (coerce . intern)")

(helm-document-attribute 'type "optional if action attribute is provided"
  "  Indicates the type of the items the source returns.

  Merge attributes not specified in the source itself from
  `helm-type-attributes'.

  This attribute is implemented by plug-in.")

(helm-document-attribute 'init "optional"
  "  Function called with no parameters when helm is started. It
  is useful for collecting current state information which can be
  used to create the list of candidates later.

  For example, if a source needs to work with the current
  directory then it can store its value here, because later
  helm does its job in the minibuffer and in the
  `helm-buffer' and the current directory can be different
  there.")

(helm-document-attribute 'delayed-init "optional"
  "  Function called with no parameters before candidate function
  is called.  It is similar with `init' attribute, but its
  evaluation is deferred. It is useful to combine with ")

(helm-document-attribute 'match "optional"
  "  List of functions called with one parameter: a candidate. The
  function should return non-nil if the candidate matches the
  current pattern (see variable `helm-pattern').

  This attribute allows the source to override the default
  pattern matching based on `string-match'. It can be used, for
  example, to implement a source for file names and do the
  pattern matching on the basename of files, since it's more
  likely one is typing part of the basename when searching for a
  file, instead of some string anywhere else in its path.

  If the list contains more than one function then the list of
  matching candidates from the source is constructed by appending
  the results after invoking the first function on all the
  potential candidates, then the next function, and so on. The
  matching candidates supplied by the first function appear first
  in the list of results and then results from the other
  functions, respectively.

  This attribute has no effect for asynchronous sources (see
  attribute `candidates'), since they perform pattern matching
  themselves.")

(helm-document-attribute 'candidate-transformer "optional"
  "  It's a function or a list of functions called with one argument
  when the completion list from the source is built. The argument
  is the list of candidates retrieved from the source. The
  function should return a transformed list of candidates which
  will be used for the actual completion.  If it is a list of
  functions, it calls each function sequentially.

  This can be used to transform or remove items from the list of
  candidates.

  Note that `candidates' is run already, so the given transformer
  function should also be able to handle candidates with (DISPLAY
  . REAL) format.")

(helm-document-attribute 'filtered-candidate-transformer "optional"
  "  It has the same format as `candidate-transformer', except the
  function is called with two parameters: the candidate list and
  the source.

  This transformer is run on the candidate list which is already
  filtered by the current pattern. While `candidate-transformer'
  is run only once, it is run every time the input pattern is
  changed.

  It can be used to transform the candidate list dynamically, for
  example, based on the current pattern.

  In some cases it may also be more efficent to perform candidate
  transformation here, instead of with `candidate-transformer'
  even if this transformation is done every time the pattern is
  changed.  For example, if a candidate set is very large then
  `candidate-transformer' transforms every candidate while only
  some of them will actually be dislpayed due to the limit
  imposed by `helm-candidate-number-limit'.

  Note that `candidates' and `candidate-transformer' is run
  already, so the given transformer function should also be able
  to handle candidates with (DISPLAY . REAL) format.

  This option has no effect for asynchronous sources. (Not yet,
  at least.")

(helm-document-attribute 'action-transformer "optional"
  "  It's a function or a list of functions called with two
  arguments when the action list from the source is
  assembled. The first argument is the list of actions, the
  second is the current selection.  If it is a list of functions,
  it calls each function sequentially.

  The function should return a transformed action list.

  This can be used to customize the list of actions based on the
  currently selected candidate.")

(helm-document-attribute 'pattern-transformer "optional"
  "  It's a function or a list of functions called with one argument
  before computing matches. Its argument is `helm-pattern'.
  Functions should return transformed `helm-pattern'.

  It is useful to change interpretation of `helm-pattern'.")

(helm-document-attribute 'delayed "optional"
  "  Candidates from the source are shown only if the user stops
  typing and is idle for `helm-idle-delay' seconds.
  If a value is given to delayed attr, this value is used instead only
  if it is > to `helm-idle-delay'.")

(helm-document-attribute 'volatile "optional"
  "  Indicates the source assembles the candidate list dynamically,
  so it shouldn't be cached within a single Helm
  invocation. It is only applicable to synchronous sources,
  because asynchronous sources are not cached.")

(helm-document-attribute 'requires-pattern "optional"
  "  If present matches from the source are shown only if the
  pattern is not empty. Optionally, it can have an integer
  parameter specifying the required length of input which is
  useful in case of sources with lots of candidates.")

(helm-document-attribute 'persistent-action "optional"
  "  Can be a either a Function called with one parameter (the
  selected candidate) or a cons cell where first element is this
  same function and second element a symbol (e.g never-split)
  that inform `helm-execute-persistent-action'to not split his
  window to execute this persistent action.")

(helm-document-attribute 'candidates-in-buffer "optional"
  "  Shortcut attribute for making and narrowing candidates using
  buffers.  This newly-introduced attribute prevents us from
  forgetting to add volatile and match attributes.

  See docstring of `helm-candidates-in-buffer'.

  (candidates-in-buffer) is equivalent of three attributes:
    (candidates . helm-candidates-in-buffer)
    (volatile)
    (match identity)

  (candidates-in-buffer . candidates-function) is equivalent of:
    (candidates . candidates-function)
    (volatile)
    (match identity)

  This attribute is implemented by plug-in.")

(helm-document-attribute 'search "optional"
  "  List of functions like `re-search-forward' or `search-forward'.
  Buffer search function used by `helm-candidates-in-buffer'.
  By default, `helm-candidates-in-buffer' uses
  `re-search-forward'. This attribute is meant to be used with
  (candidates . helm-candidates-in-buffer) or
  (candidates-in-buffer) in short.")

(helm-document-attribute 'search-from-end "optional"
  "  Make `helm-candidates-in-buffer' search from the end of buffer.
  If this attribute is specified, `helm-candidates-in-buffer'
  uses `re-search-backward' instead.")

(helm-document-attribute 'get-line "optional"
  "  A function like `buffer-substring-no-properties' or `buffer-substring'.
  This function converts point of line-beginning and point of line-end,
  which represents a candidate computed by `helm-candidates-in-buffer'.
  By default, `helm-candidates-in-buffer' uses
  `buffer-substring-no-properties'.")

(helm-document-attribute 'display-to-real "optional"
  "  Function called with one parameter; the selected candidate.

  The function transforms the selected candidate, and the result
  is passed to the action function.  The display-to-real
  attribute provides another way to pass other string than one
  shown in Helm buffer.

  Traditionally, it is possible to make candidates,
  candidate-transformer or filtered-candidate-transformer
  function return a list with (DISPLAY . REAL) pairs. But if REAL
  can be generated from DISPLAY, display-to-real is more
  convenient and faster.")

(helm-document-attribute 'real-to-display "optional"
  "  Function called with one parameter; the selected candidate.

  The inverse of display-to-real attribute.

  The function transforms the selected candidate, which is passed
  to the action function, for display.  The real-to-display
  attribute provides the other way to pass other string than one
  shown in Helm buffer.

  Traditionally, it is possible to make candidates,
  candidate-transformer or filtered-candidate-transformer
  function return a list with (DISPLAY . REAL) pairs. But if
  DISPLAY can be generated from REAL, real-to-display is more
  convenient.

  Note that DISPLAY parts returned from candidates /
  candidate-transformer are IGNORED as the name `display-to-real'
  says.")

(helm-document-attribute 'cleanup "optional"
  "  Function called with no parameters when *helm* buffer is
  closed. It is useful for killing unneeded candidates buffer.

  Note that the function is executed BEFORE performing action.")

(helm-document-attribute 'candidate-number-limit "optional"
  "  Override `helm-candidate-number-limit' only for this source.")

(helm-document-attribute 'accept-empty "optional"
  "  Pass empty string \"\" to action function.")

(helm-document-attribute 'dummy "optional"
  "  Set `helm-pattern' to candidate. If this attribute is
  specified, The candidates attribute is ignored.

  This attribute is implemented by plug-in.")

(helm-document-attribute 'multiline "optional"
  "  Enable to selection multiline candidates.")

(helm-document-attribute 'update "optional"
  (substitute-command-keys
   "  Function called with no parameters when \
\\<helm-map>\\[helm-force-update] is pressed."))

(helm-document-attribute 'mode-line "optional"
  "  Source local `helm-mode-line-string' (included in
  `mode-line-format'). It accepts also variable/function name.")

(helm-document-attribute 'header-line "optional"
  "  Source local `header-line-format'.
  It accepts also variable/function name. ")

(helm-document-attribute
    'resume "optional"
  "  Function called with no parameters when `helm-resume' is
  started.")

(helm-document-attribute 'keymap "optional"
  "  Specific keymap for this source.
  It is useful to have a keymap per source when using more than
  one source.  Otherwise, a keymap can be set per command with
  `helm' argument KEYMAP.  NOTE: when a source have `helm-map' as
  keymap attr, the global value of `helm-map' will override the
  actual local one.")

(helm-document-attribute 'help-message "optional"
  "  Help message for this source.
  If not present, `helm-help-message' value will be used.")

(helm-document-attribute 'match-part "optional"
  "  Allow matching candidate in the line with `candidates-in-buffer'.
  In candidates-in-buffer sources, match is done with
  `re-search-forward' which allow matching only a regexp on the
  `helm-buffer'; when this search is done, match-part allow
  matching only a specific part of the current line e.g with a
  line like this:

  filename:candidate-containing-the-word-filename

  What you want is to ignore \"filename\" part and match only
  \"candidate-containing-the-word-filename\"

  So give a function matching only the part of candidate after \":\"

  If source contain match-part attribute, match is computed only
  on part of candidate returned by the call of function provided
  by this attribute. The function should have one arg, candidate,
  and return only a specific part of candidate.

  NOTE: This have effect only on sources using
  `candidates-in-buffer'.")

(helm-document-attribute 'match-strict "optional"
  "  When specifying a match function within a source and
  helm-match-plugin is enabled, the result of all matching
  functions will be concatened, which in some cases is not what
  is wanted. When using `match-strict' only this or these
  functions will be used. You can specify those functions as a
  list of functions or a single symbol function. For anonymous
  function don't add the dot, e.g:

  \(match-strict (lambda () (foo))).")

(helm-document-attribute 'nohighlight "optional"
  "  Disable highlight match in this source.")

(helm-document-attribute 'no-matchplugin "optional"
  "  Disable matchplugin for this source.")

(helm-document-attribute 'history "optional"
  "  Allow passing history variable to helm from source.
  It should be a quoted symbol evaluated from source, i.e:
  (history . ,'history-var)")

(helm-document-attribute 'follow "optional"
  "  Enable `helm-follow-mode' for this source only.
  You must give it a value of 1 or -1, though giving a -1 value
  is surely not what you want, e.g: (follow . 1)

  See `helm-follow-mode' for more infos")

(helm-document-attribute 'follow-delay "optional"
  "  `helm-follow-mode' will execute persistent-action after this delay.
Otherwise value of `helm-follow-input-idle-delay' is used if non--nil,
If none of these are found fallback to `helm-input-idle-delay'.")

(helm-document-attribute 'allow-dups "optional"
  "  Allow helm collecting duplicates candidates.")

(helm-document-attribute 'filter-one-by-one "optional"
  "  A transformer function that treat candidates one by one.
  It is called with one arg the candidate.
  It is faster than `filtered-candidate-transformer' or `candidates-transformer',
  but should be used only in sources that recompute constantly their candidates,
  e.g `helm-source-find-files'.
  Filtering happen early and candidates are treated
  one by one instead of re-looping on the whole list.
  If used with `filtered-candidate-transformer' or `candidates-transformer'
  these functions should treat the candidates transformed by the `filter-one-by-one'
  function in consequence.")

(helm-document-attribute 'nomark "optional"
  "  Don't allow marking candidates when this attribute is present.")

(provide 'helm-help)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-help.el ends here
