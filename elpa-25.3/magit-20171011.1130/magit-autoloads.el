;;; magit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "git-rebase" "git-rebase.el" (23009 21875 0
;;;;;;  0))
;;; Generated autoloads from git-rebase.el

(autoload 'git-rebase-mode "git-rebase" "\
Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details.

\(fn)" t nil)

(defconst git-rebase-filename-regexp "/git-rebase-todo\\'")

(add-to-list 'auto-mode-alist (cons git-rebase-filename-regexp 'git-rebase-mode))

;;;***

;;;### (autoloads nil "magit" "magit.el" (23009 21875 0 0))
;;; Generated autoloads from magit.el
 (autoload 'magit-merge-popup "magit" nil t)

(autoload 'magit-merge "magit" "\
Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

\(git merge --no-edit|--no-commit [ARGS] REV)

\(fn REV &optional ARGS NOCOMMIT)" t nil)

(autoload 'magit-merge-editmsg "magit" "\
Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.

\(git merge --edit --no-ff [ARGS] REV)

\(fn REV &optional ARGS)" t nil)

(autoload 'magit-merge-nocommit "magit" "\
Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.

\(git merge --no-commit --no-ff [ARGS] REV)

\(fn REV &optional ARGS)" t nil)

(autoload 'magit-merge-preview "magit" "\
Preview result of merging REV into the current branch.

\(fn REV)" t nil)

(autoload 'magit-merge-abort "magit" "\
Abort the current merge operation.

\(git merge --abort)

\(fn)" t nil)
 (autoload 'magit-reset-popup "magit" nil t)

(autoload 'magit-reset-index "magit" "\
Reset the index to COMMIT.
Keep the head and working tree as-is, so if COMMIT refers to the
head this effectively unstages all changes.

\(git reset COMMIT .)

\(fn COMMIT)" t nil)

(autoload 'magit-reset "magit" "\
Reset the head and index to COMMIT, but not the working tree.
With a prefix argument also reset the working tree.

\(git reset --mixed|--hard COMMIT)

\(fn COMMIT &optional HARD)" t nil)

(autoload 'magit-reset-head "magit" "\
Reset the head and index to COMMIT, but not the working tree.

\(git reset --mixed COMMIT)

\(fn COMMIT)" t nil)

(autoload 'magit-reset-soft "magit" "\
Reset the head to COMMIT, but not the index and working tree.

\(git reset --soft REVISION)

\(fn COMMIT)" t nil)

(autoload 'magit-reset-hard "magit" "\
Reset the head, index, and working tree to COMMIT.

\(git reset --hard REVISION)

\(fn COMMIT)" t nil)
 (autoload 'magit-tag-popup "magit" nil t)

(autoload 'magit-tag "magit" "\
Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.

\(git tag [--annotate] NAME REV)

\(fn NAME REV &optional ARGS)" t nil)

(autoload 'magit-tag-delete "magit" "\
Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.

\(git tag -d TAGS)

\(fn TAGS)" t nil)
 (autoload 'magit-dispatch-popup "magit" nil t)
 (autoload 'magit-run-popup "magit" nil t)

(autoload 'magit-git-command "magit" "\
Execute COMMAND asynchonously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

With a prefix argument COMMAND is run in the top-level directory
of the current working tree, otherwise in `default-directory'.

\(fn COMMAND)" t nil)

(autoload 'magit-git-command-topdir "magit" "\
Execute COMMAND asynchonously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

COMMAND is run in the top-level directory of the current
working tree.

\(fn COMMAND)" t nil)

(autoload 'magit-shell-command "magit" "\
Execute COMMAND asynchonously; display output.

Interactively, prompt for COMMAND in the minibuffer.  With a
prefix argument COMMAND is run in the top-level directory of
the current working tree, otherwise in `default-directory'.

\(fn COMMAND)" t nil)

(autoload 'magit-shell-command-topdir "magit" "\
Execute COMMAND asynchonously; display output.

Interactively, prompt for COMMAND in the minibuffer.  COMMAND
is run in the top-level directory of the current working tree.

\(fn COMMAND)" t nil)

(autoload 'magit-version "magit" "\
Return the version of Magit currently in use.
If optional argument PRINT-DEST is non-nil, output
stream (interactively, the echo area, or the current buffer with
a prefix argument), also print the used versions of Magit, Git,
and Emacs to it.

\(fn &optional PRINT-DEST)" t nil)

;;;***

;;;### (autoloads nil "magit-apply" "magit-apply.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-apply.el

(autoload 'magit-stage-file "magit-apply" "\
Stage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be staged.  Otherwise stage the file at point without
requiring confirmation.

\(fn FILE)" t nil)

(autoload 'magit-stage-modified "magit-apply" "\
Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files.
\('git add --update|--all .').

\(fn &optional ALL)" t nil)

(autoload 'magit-unstage-file "magit-apply" "\
Unstage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be unstaged.  Otherwise unstage the file at point
without requiring confirmation.

\(fn FILE)" t nil)

(autoload 'magit-unstage-all "magit-apply" "\
Remove all changes from the staging area.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "magit-autorevert" "magit-autorevert.el" (23009
;;;;;;  21875 0 0))
;;; Generated autoloads from magit-autorevert.el

(defvar magit-revert-buffers t)

(defvar magit-auto-revert-mode (and (with-no-warnings magit-revert-buffers) (not global-auto-revert-mode) (not noninteractive)) "\
Non-nil if Magit-Auto-Revert mode is enabled.
See the `magit-auto-revert-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-auto-revert-mode'.")

(custom-autoload 'magit-auto-revert-mode "magit-autorevert" nil)

(autoload 'magit-auto-revert-mode "magit-autorevert" "\
Toggle Auto-Revert mode in all buffers.
With prefix ARG, enable Magit-Auto-Revert mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Auto-Revert mode is enabled in all buffers where
`magit-turn-on-auto-revert-mode-if-desired' would do it.
See `auto-revert-mode' for more information on Auto-Revert mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "magit-bisect" "magit-bisect.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-bisect.el
 (autoload 'magit-bisect-popup "magit-bisect" nil t)

(autoload 'magit-bisect-start "magit-bisect" "\
Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a know
good and a bad commit.  To move the session forward use the
other actions from the bisect popup (\\<magit-status-mode-map>\\[magit-bisect-popup]).

\(fn BAD GOOD)" t nil)

(autoload 'magit-bisect-reset "magit-bisect" "\
After bisecting, cleanup bisection state and return to original `HEAD'.

\(fn)" t nil)

(autoload 'magit-bisect-good "magit-bisect" "\
While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question.

\(fn)" t nil)

(autoload 'magit-bisect-bad "magit-bisect" "\
While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question.

\(fn)" t nil)

(autoload 'magit-bisect-skip "magit-bisect" "\
While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one.

\(fn)" t nil)

(autoload 'magit-bisect-run "magit-bisect" "\
Bisect automatically by running commands after each step.

Unlike `git bisect run' this can be used before bisecting has
begun.  In that case it behaves like `git bisect start; git
bisect run'.

\(fn CMDLINE &optional BAD GOOD)" t nil)

;;;***

;;;### (autoloads nil "magit-blame" "magit-blame.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-blame.el
 (autoload 'magit-blame-popup "magit-blame" nil t)

(autoload 'magit-blame-reverse "magit-blame" "\
For each line show the subsequent revision that removes it.

\(fn REVISION FILE &optional ARGS)" t nil)

(autoload 'magit-blame "magit-blame" "\
For each line show the revision that last touched it.

Interactively blame the file being visited in the current buffer.
If the buffer visits a revision of that file, then blame up to
that revision, otherwise blame the file's full history, including
uncommitted changes.

If Magit-Blame mode is already turned on then blame recursively, by
visiting REVISION:FILE (using `magit-find-file'), where revision
is the revision before the revision that added the lines at
point.

ARGS is a list of additional arguments to pass to `git blame';
only arguments available from `magit-blame-popup' should be used.

\(fn REVISION FILE &optional ARGS)" t nil)

;;;***

;;;### (autoloads nil "magit-bookmark" "magit-bookmark.el" (23009
;;;;;;  21875 0 0))
;;; Generated autoloads from magit-bookmark.el

(autoload 'magit-bookmark--status-jump "magit-bookmark" "\
Handle a Magit status BOOKMARK.

\(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--status-make-record "magit-bookmark" "\
Create a Magit status bookmark.

\(fn)" nil nil)

(autoload 'magit-bookmark--refs-jump "magit-bookmark" "\
Handle a Magit refs BOOKMARK.

\(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--refs-make-record "magit-bookmark" "\
Create a Magit refs bookmark.

\(fn)" nil nil)

(autoload 'magit-bookmark--log-jump "magit-bookmark" "\
Handle a Magit log BOOKMARK.

\(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--log-make-record "magit-bookmark" "\
Create a Magit log bookmark.

\(fn)" nil nil)

(autoload 'magit-bookmark--reflog-jump "magit-bookmark" "\
Handle a Magit reflog BOOKMARK.

\(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--reflog-make-record "magit-bookmark" "\
Create a Magit reflog bookmark.

\(fn)" nil nil)

(autoload 'magit-bookmark--stashes-jump "magit-bookmark" "\
Handle a Magit stash list BOOKMARK.

\(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--stashes-make-record "magit-bookmark" "\
Create a Magit stash list bookmark.

\(fn)" nil nil)

(autoload 'magit-bookmark--cherry-jump "magit-bookmark" "\
Handle a Magit cherry BOOKMARK.

\(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--cherry-make-record "magit-bookmark" "\
Create a Magit cherry bookmark.

\(fn)" nil nil)

(autoload 'magit-bookmark--diff-jump "magit-bookmark" "\
Handle a Magit diff BOOKMARK.

\(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--diff-make-record "magit-bookmark" "\
Create a Magit diff bookmark.

\(fn)" nil nil)

(autoload 'magit-bookmark--revision-jump "magit-bookmark" "\
Handle a Magit revision BOOKMARK.

\(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--revision-make-record "magit-bookmark" "\
Create a Magit revision bookmark.

\(fn)" nil nil)

(autoload 'magit-bookmark--stash-jump "magit-bookmark" "\
Handle a Magit stash BOOKMARK.

\(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--stash-make-record "magit-bookmark" "\
Create a Magit stash bookmark.

\(fn)" nil nil)

(autoload 'magit-bookmark--submodules-jump "magit-bookmark" "\
Handle a Magit submodule list BOOKMARK.

\(fn BOOKMARK)" nil nil)

(autoload 'magit-bookmark--submodules-make-record "magit-bookmark" "\
Create a Magit submodule list bookmark.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "magit-branch" "magit-branch.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-branch.el
 (autoload 'magit-branch-popup "magit" nil t)

(autoload 'magit-checkout "magit-branch" "\
Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch, then that becomes the current
branch.  If it is something else, then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.

\(git checkout REVISION).

\(fn REVISION)" t nil)

(autoload 'magit-branch "magit-branch" "\
Create BRANCH at branch or revision START-POINT.

\(git branch [ARGS] BRANCH START-POINT).

\(fn BRANCH START-POINT &optional ARGS)" t nil)

(autoload 'magit-branch-and-checkout "magit-branch" "\
Create and checkout BRANCH at branch or revision START-POINT.

\(git checkout [ARGS] -b BRANCH START-POINT).

\(fn BRANCH START-POINT &optional ARGS)" t nil)

(autoload 'magit-branch-or-checkout "magit-branch" "\
Hybrid between `magit-checkout' and `magit-branch-and-checkout'.

Ask the user for an existing branch or revision.  If the user
input actually can be resolved as a branch or revision, then
check that out, just like `magit-checkout' would.

Otherwise create and checkout a new branch using the input as
its name.  Before doing so read the starting-point for the new
branch.  This is similar to what `magit-branch-and-checkout'
does.

\(fn ARG &optional START-POINT)" t nil)

(autoload 'magit-branch-orphan "magit-branch" "\
Create and checkout an orphan BRANCH with contents from revision START-POINT.

\(git checkout --orphan [ARGS] BRANCH START-POINT).

\(fn BRANCH START-POINT &optional ARGS)" t nil)

(autoload 'magit-branch-spinoff "magit-branch" "\
Create new branch from the unpushed commits.

Create and checkout a new branch starting at and tracking the
current branch.  That branch in turn is reset to the last commit
it shares with its upstream.  If the current branch has no
upstream or no unpushed commits, then the new branch is created
anyway and the previously current branch is not touched.

This is useful to create a feature branch after work has already
began on the old branch (likely but not necessarily \"master\").

If the current branch is a member of the value of option
`magit-branch-prefer-remote-upstream' (which see), then the
current branch will be used as the starting point as usual, but
the upstream of the starting-point may be used as the upstream
of the new branch, instead of the starting-point itself.

If optional FROM is non-nil, then the source branch is reset
to `FROM~', instead of to the last commit it shares with its
upstream.  Interactively, FROM is only ever non-nil, if the
region selects some commits, and among those commits, FROM is
the commit that is the fewest commits ahead of the source
branch.  (It not yet possible to spin off a single commit,
unless it is the only unpushed commit.  See #2920.)

The commit at the other end of the selection actually does not
matter, all commits between FROM and `HEAD' are moved to the new
branch.  If FROM is not reachable from `HEAD' or is reachable
from the source branch's upstream, then an error is raised.

\(fn BRANCH &optional FROM &rest ARGS)" t nil)

(autoload 'magit-branch-reset "magit-branch" "\
Reset a branch to the tip of another branch or any other commit.

When the branch being reset is the current branch, then do a
hard reset.  If there are any uncommitted changes, then the user
has to confirm the reset because those changes would be lost.

This is useful when you have started work on a feature branch but
realize it's all crap and want to start over.

When resetting to another branch and a prefix argument is used,
then also set the target branch as the upstream of the branch
that is being reset.

\(fn BRANCH TO &optional ARGS SET-UPSTREAM)" t nil)

(autoload 'magit-branch-delete "magit-branch" "\
Delete one or multiple branches.
If the region marks multiple branches, then offer to delete
those, otherwise prompt for a single branch to be deleted,
defaulting to the branch at point.

\(fn BRANCHES &optional FORCE)" t nil)

(autoload 'magit-branch-rename "magit-branch" "\
Rename branch OLD to NEW.
With prefix, forces the rename even if NEW already exists.

\(git branch -m|-M OLD NEW).

\(fn OLD NEW &optional FORCE)" t nil)

(autoload 'magit-branch-config-popup "magit-branch" "\
Popup console for setting branch variables.

\(fn BRANCH)" t nil)

(autoload 'magit-edit-branch*description "magit-branch" "\
Edit the description of the current branch.
With a prefix argument edit the description of another branch.

The description for the branch named NAME is stored in the Git
variable `branch.<name>.description'.

\(fn BRANCH)" t nil)

(autoload 'magit-set-branch*merge/remote "magit-branch" "\
Set or unset the upstream of the current branch.
With a prefix argument do so for another branch.

When the branch in question already has an upstream then simply
unsets it.  Invoke this command again to set another upstream.

Together the Git variables `branch.<name>.remote' and
`branch.<name>.merge' define the upstream branch of the local
branch named NAME.  The value of `branch.<name>.remote' is the
name of the upstream remote.  The value of `branch.<name>.merge'
is the full reference of the upstream branch, on the remote.

Non-interactively, when UPSTREAM is non-nil, then always set it
as the new upstream, regardless of whether another upstream was
already set.  When nil, then always unset.

\(fn BRANCH UPSTREAM)" t nil)

(autoload 'magit-cycle-branch*rebase "magit-branch" "\
Cycle the value of `branch.<name>.rebase' for the current branch.
With a prefix argument cycle the value for another branch.

The Git variables `branch.<name>.rebase' controls whether pulling
into the branch named NAME is done by rebasing that branch onto
the fetched branch or by merging that branch.

When `true' then pulling is done by rebasing.
When `false' then pulling is done by merging.

When that variable is undefined then the value of `pull.rebase'
is used instead.  It defaults to `false'.

\(fn BRANCH)" t nil)

(autoload 'magit-cycle-branch*pushRemote "magit-branch" "\
Cycle the value of `branch.<name>.pushRemote' for the current branch.
With a prefix argument cycle the value for another branch.

The Git variable `branch.<name>.pushRemote' specifies the remote
that the branch named NAME is usually pushed to.  The value has
to be the name of an existing remote.

If that variable is undefined, then the value of the Git variable
`remote.pushDefault' is used instead, provided that it is defined,
which by default it is not.

\(fn BRANCH)" t nil)

(autoload 'magit-cycle-pull\.rebase "magit-branch" "\
Cycle the repository-local value of `pull.rebase'.

The Git variable `pull.rebase' specifies whether pulling is done
by rebasing or by merging.  It can be overwritten using the Git
variable `branch.<name>.rebase'.

When `true' then pulling is done by rebasing.
When `false' (the default) then pulling is done by merging.

\(fn)" t nil)

(autoload 'magit-cycle-remote\.pushDefault "magit-branch" "\
Cycle the repository-local value of `remote.pushDefault'.

The Git variable `remote.pushDefault' specifies the remote that
local branches are usually pushed to.  It can be overwritten
using the Git variable `branch.<name>.pushRemote'.

\(fn)" t nil)

(autoload 'magit-cycle-branch*autoSetupMerge "magit-branch" "\
Cycle the repository-local value of `branch.autoSetupMerge'.

The Git variable `branch.autoSetupMerge' under what circumstances
creating a branch (named NAME) should result in the variables
`branch.<name>.merge' and `branch.<name>.remote' being set
according to the starting point used to create the branch.  If
the starting point isn't a branch, then these variables are never
set.

When `always' then the variables are set regardless of whether
the starting point is a local or a remote branch.

When `true' (the default) then the variable are set when the
starting point is a remote branch, but not when it is a local
branch.

When `false' then the variables are never set.

\(fn)" t nil)

(autoload 'magit-cycle-branch*autoSetupRebase "magit-branch" "\
Cycle the repository-local value of `branch.autoSetupRebase'.

The Git variable `branch.autoSetupRebase' specifies whether
creating a branch (named NAME) should result in the variable
`branch.<name>.rebase' being set to `true'.

When `always' then the variable is set regardless of whether the
starting point is a local or a remote branch.

When `local' then the variable are set when the starting point
is a local branch, but not when it is a remote branch.

When `remote' then the variable are set when the starting point
is a remote branch, but not when it is a local branch.

When `never' (the default) then the variable is never set.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "magit-commit" "magit-commit.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-commit.el

(autoload 'magit-commit "magit-commit" "\
Create a new commit on `HEAD'.
With a prefix argument, amend to the commit at `HEAD' instead.

\(git commit [--amend] ARGS)

\(fn &optional ARGS)" t nil)

(autoload 'magit-commit-amend "magit-commit" "\
Amend the last commit.

\(git commit --amend ARGS)

\(fn &optional ARGS)" t nil)

(autoload 'magit-commit-extend "magit-commit" "\
Amend the last commit, without editing the message.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-extend-override-date' can be used
to inverse the meaning of the prefix argument.  
\(git commit
--amend --no-edit)

\(fn &optional ARGS OVERRIDE-DATE)" t nil)

(autoload 'magit-commit-reword "magit-commit" "\
Reword the last commit, ignoring staged changes.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-reword-override-date' can be used
to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.

\(git commit --amend --only)

\(fn &optional ARGS OVERRIDE-DATE)" t nil)

(autoload 'magit-commit-fixup "magit-commit" "\
Create a fixup commit.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

\(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-squash "magit-commit" "\
Create a squash commit, without editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

\(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-augment "magit-commit" "\
Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

\(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-instant-fixup "magit-commit" "\
Create a fixup commit targeting COMMIT and instantly rebase.

\(fn &optional COMMIT ARGS)" t nil)

(autoload 'magit-commit-instant-squash "magit-commit" "\
Create a squash commit targeting COMMIT and instantly rebase.

\(fn &optional COMMIT ARGS)" t nil)

;;;***

;;;### (autoloads nil "magit-diff" "magit-diff.el" (23009 21875 0
;;;;;;  0))
;;; Generated autoloads from magit-diff.el

(autoload 'magit-diff-popup "magit-diff" "\
Popup console for diff commands.

\(fn ARG)" t nil)

(autoload 'magit-diff-buffer-file-popup "magit-diff" "\
Popup console for diff commands.

This is a variant of `magit-diff-popup' which shows the same popup
but which limits the diff to the file being visited in the current
buffer.

\(fn)" t nil)

(autoload 'magit-diff-dwim "magit-diff" "\
Show changes for the thing at point.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-diff "magit-diff" "\
Show differences between two commits.

REV-OR-RANGE should be a range or a single revision.  If it is a
revision, then show changes in the working tree relative to that
revision.  If it is a range, but one side is omitted, then show
changes relative to `HEAD'.

If the region is active, use the revisions on the first and last
line of the region as the two sides of the range.  With a prefix
argument, instead of diffing the revisions, choose a revision to
view changes along, starting at the common ancestor of both
revisions (i.e., use a \"...\" range).

\(fn REV-OR-RANGE &optional ARGS FILES)" t nil)

(autoload 'magit-diff-working-tree "magit-diff" "\
Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer.

\(fn &optional REV ARGS FILES)" t nil)

(autoload 'magit-diff-staged "magit-diff" "\
Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer.

\(fn &optional REV ARGS FILES)" t nil)

(autoload 'magit-diff-unstaged "magit-diff" "\
Show changes between the working tree and the index.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-diff-while-committing "magit-diff" "\
While committing, show the changes that are about to be committed.
While amending, invoking the command again toggles between
showing just the new changes or all the changes that will
be committed.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-diff-buffer-file "magit-diff" "\
Show diff for the blob or file visited in the current buffer.

\(fn)" t nil)

(autoload 'magit-diff-paths "magit-diff" "\
Show changes between any two files on disk.

\(fn A B)" t nil)

(autoload 'magit-show-commit "magit-diff" "\
Visit the revision at point in another buffer.
If there is no revision at point or with a prefix argument prompt
for a revision.

\(fn REV &optional ARGS FILES MODULE)" t nil)

;;;***

;;;### (autoloads nil "magit-ediff" "magit-ediff.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-ediff.el
 (autoload 'magit-ediff-popup "magit-ediff" nil t)

(autoload 'magit-ediff-resolve "magit-ediff" "\
Resolve outstanding conflicts in FILE using Ediff.
FILE has to be relative to the top directory of the repository.

In the rare event that you want to manually resolve all
conflicts, including those already resolved by Git, use
`ediff-merge-revisions-with-ancestor'.

\(fn FILE)" t nil)

(autoload 'magit-ediff-stage "magit-ediff" "\
Stage and unstage changes to FILE using Ediff.
FILE has to be relative to the top directory of the repository.

\(fn FILE)" t nil)

(autoload 'magit-ediff-compare "magit-ediff" "\
Compare REVA:FILEA with REVB:FILEB using Ediff.

FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil, then this stands for the
working tree state.

If the region is active, use the revisions on the first and last
line of the region.  With a prefix argument, instead of diffing
the revisions, choose a revision to view changes along, starting
at the common ancestor of both revisions (i.e., use a \"...\"
range).

\(fn REVA REVB FILEA FILEB)" t nil)

(autoload 'magit-ediff-dwim "magit-ediff" "\
Compare, stage, or resolve using Ediff.
This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using Ediff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `magit-ediff-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run.

\(fn)" t nil)

(autoload 'magit-ediff-show-staged "magit-ediff" "\
Show staged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

\(fn FILE)" t nil)

(autoload 'magit-ediff-show-unstaged "magit-ediff" "\
Show unstaged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

\(fn FILE)" t nil)

(autoload 'magit-ediff-show-working-tree "magit-ediff" "\
Show changes between `HEAD' and working tree using Ediff.
FILE must be relative to the top directory of the repository.

\(fn FILE)" t nil)

(autoload 'magit-ediff-show-commit "magit-ediff" "\
Show changes introduced by COMMIT using Ediff.

\(fn COMMIT)" t nil)

(autoload 'magit-ediff-show-stash "magit-ediff" "\
Show changes introduced by STASH using Ediff.
`magit-ediff-show-stash-with-index' controls whether a
three-buffer Ediff is used in order to distinguish changes in the
stash that were staged.

\(fn STASH)" t nil)

;;;***

;;;### (autoloads nil "magit-extras" "magit-extras.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-extras.el

(autoload 'magit-run-git-gui "magit-extras" "\
Run `git gui' for the current git repository.

\(fn)" t nil)

(autoload 'magit-run-git-gui-blame "magit-extras" "\
Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the `HEAD', with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on.

\(fn COMMIT FILENAME &optional LINENUM)" t nil)

(autoload 'magit-run-gitk "magit-extras" "\
Run `gitk' in the current repository.

\(fn)" t nil)

(autoload 'magit-run-gitk-branches "magit-extras" "\
Run `gitk --branches' in the current repository.

\(fn)" t nil)

(autoload 'magit-run-gitk-all "magit-extras" "\
Run `gitk --all' in the current repository.

\(fn)" t nil)

(autoload 'ido-enter-magit-status "magit-extras" "\
Drop into `magit-status' from file switching.

To make this command available use something like:

  (add-hook \\='ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd \"C-x g\") \\='ido-enter-magit-status)))

Starting with Emacs 25.1 the Ido keymaps are defined just once
instead of every time Ido is invoked, so now you can modify it
like pretty much every other keymap:

  (define-key ido-common-completion-map
    (kbd \"C-x g\") 'ido-enter-magit-status)

\(fn)" t nil)

(autoload 'magit-dired-jump "magit-extras" "\
Visit file at point using Dired.
With a prefix argument, visit in another window.  If there
is no file at point, then instead visit `default-directory'.

\(fn &optional OTHER-WINDOW)" t nil)

(autoload 'magit-do-async-shell-command "magit-extras" "\
Open FILE with `dired-do-async-shell-command'.
Interactively, open the file at point.

\(fn FILE)" t nil)

(autoload 'magit-previous-line "magit-extras" "\
Like `previous-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects an
area that is larger than the region.  This causes `previous-line'
when invoked while holding the shift key to move up one line and
thereby select two lines.  When invoked inside a hunk body this
command does not move point on the first invocation and thereby
it only selects a single line.  Which inconsistency you prefer
is a matter of preference.

\(fn &optional ARG TRY-VSCROLL)" t nil)

(function-put 'magit-previous-line 'interactive-only '"use `forward-line' with negative argument instead.")

(autoload 'magit-next-line "magit-extras" "\
Like `next-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects
an area that is larger than the region.  This causes `next-line'
when invoked while holding the shift key to move down one line
and thereby select two lines.  When invoked inside a hunk body
this command does not move point on the first invocation and
thereby it only selects a single line.  Which inconsistency you
prefer is a matter of preference.

\(fn &optional ARG TRY-VSCROLL)" t nil)

(function-put 'magit-next-line 'interactive-only 'forward-line)

(autoload 'magit-clean "magit-extras" "\
Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.

\(git clean -f -d [-x|-X])

\(fn &optional ARG)" t nil)

(autoload 'magit-gitignore "magit-extras" "\
Instruct Git to ignore FILE-OR-PATTERN.
With a prefix argument only ignore locally.

\(fn FILE-OR-PATTERN &optional LOCAL)" t nil)

(autoload 'magit-gitignore-locally "magit-extras" "\
Instruct Git to locally ignore FILE-OR-PATTERN.

\(fn FILE-OR-PATTERN)" t nil)

(autoload 'magit-add-change-log-entry "magit-extras" "\
Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Magit buffer instead of on
a position in a file-visiting buffer.

\(fn &optional WHOAMI FILE-NAME OTHER-WINDOW)" t nil)

(autoload 'magit-add-change-log-entry-other-window "magit-extras" "\
Find change log file in other window and add entry and item.
This differs from `add-change-log-entry-other-window' (which see)
in that it acts on the current hunk in a Magit buffer instead of
on a position in a file-visiting buffer.

\(fn &optional WHOAMI FILE-NAME)" t nil)

(autoload 'magit-abort-dwim "magit-extras" "\
Abort current operation.
Depending on the context, this will abort a merge, a rebase, a
patch application, a cherry-pick, a revert, or a bisect.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "magit-files" "magit-files.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-files.el

(autoload 'magit-find-file "magit-files" "\
View FILE from REV.
Switch to a buffer visiting blob REV:FILE,
creating one if none already exists.

\(fn REV FILE)" t nil)

(autoload 'magit-find-file-other-window "magit-files" "\
View FILE from REV, in another window.
Like `magit-find-file', but create a new window or reuse an
existing one.

\(fn REV FILE)" t nil)
 (autoload 'magit-file-popup "magit" nil t)

(defvar global-magit-file-mode nil "\
Non-nil if Global Magit-File mode is enabled.
See the `global-magit-file-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-magit-file-mode'.")

(custom-autoload 'global-magit-file-mode "magit-files" nil)

(autoload 'global-magit-file-mode "magit-files" "\
Toggle Magit-File mode in all buffers.
With prefix ARG, enable Global Magit-File mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-File mode is enabled in all buffers where
`magit-file-mode-turn-on' would do it.
See `magit-file-mode' for more information on Magit-File mode.

\(fn &optional ARG)" t nil)

(autoload 'magit-file-checkout "magit-files" "\
Checkout FILE from REV.

\(fn REV FILE)" t nil)

;;;***

;;;### (autoloads nil "magit-imenu" "magit-imenu.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-imenu.el

(autoload 'magit-imenu--log-prev-index-position-function "magit-imenu" "\
Move point to previous line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'.

\(fn)" nil nil)

(autoload 'magit-imenu--log-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line.

\(fn)" nil nil)

(autoload 'magit-imenu--diff-prev-index-position-function "magit-imenu" "\
Move point to previous file line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'.

\(fn)" nil nil)

(autoload 'magit-imenu--diff-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line.

\(fn)" nil nil)

(autoload 'magit-imenu--status-create-index-function "magit-imenu" "\
Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'.

\(fn)" nil nil)

(autoload 'magit-imenu--refs-create-index-function "magit-imenu" "\
Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'.

\(fn)" nil nil)

(autoload 'magit-imenu--cherry-create-index-function "magit-imenu" "\
Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'.

\(fn)" nil nil)

(autoload 'magit-imenu--submodule-prev-index-position-function "magit-imenu" "\
Move point to previous line in magit-submodule-list buffer.
This function is used as a value for
`imenu-prev-index-position-function'.

\(fn)" nil nil)

(autoload 'magit-imenu--submodule-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line.

\(fn)" nil nil)

(autoload 'magit-imenu--repolist-prev-index-position-function "magit-imenu" "\
Move point to previous line in magit-repolist buffer.
This function is used as a value for
`imenu-prev-index-position-function'.

\(fn)" nil nil)

(autoload 'magit-imenu--repolist-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line.

\(fn)" nil nil)

(autoload 'magit-imenu--process-prev-index-position-function "magit-imenu" "\
Move point to previous process in magit-process buffer.
This function is used as a value for
`imenu-prev-index-position-function'.

\(fn)" nil nil)

(autoload 'magit-imenu--process-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line.

\(fn)" nil nil)

(autoload 'magit-imenu--rebase-prev-index-position-function "magit-imenu" "\
Move point to previous commit in git-rebase buffer.
This function is used as a value for
`imenu-prev-index-position-function'.

\(fn)" nil nil)

(autoload 'magit-imenu--rebase-extract-index-name-function "magit-imenu" "\
Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "magit-log" "magit-log.el" (23009 21875 0 0))
;;; Generated autoloads from magit-log.el

(autoload 'magit-log-buffer-file-popup "magit-log" "\
Popup console for log commands.

This is a variant of `magit-log-popup' which shows the same popup
but which limits the log to the file being visited in the current
buffer.

\(fn)" t nil)

(autoload 'magit-log-current "magit-log" "\
Show log for the current branch.
When `HEAD' is detached or with a prefix argument show log for
one or more revs read from the minibuffer.

\(fn REVS &optional ARGS FILES)" t nil)

(autoload 'magit-log "magit-log" "\
Show log for one or more revs read from the minibuffer.
The user can input any revision or revisions separated by a
space, or even ranges, but only branches and tags, and a
representation of the commit at point, are available as
completion candidates.

\(fn REVS &optional ARGS FILES)" t nil)

(autoload 'magit-log-head "magit-log" "\
Show log for `HEAD'.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-branches "magit-log" "\
Show log for all local branches and `HEAD'.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-all-branches "magit-log" "\
Show log for all local and remote branches and `HEAD'.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-all "magit-log" "\
Show log for all references and `HEAD'.

\(fn &optional ARGS FILES)" t nil)

(autoload 'magit-log-buffer-file "magit-log" "\
Show log for the blob or file visited in the current buffer.
With a prefix argument or when `--follow' is part of
`magit-log-arguments', then follow renames.  When the region is
active, restrict the log to the lines that the region touches.

\(fn &optional FOLLOW BEG END)" t nil)

(autoload 'magit-reflog-current "magit-log" "\
Display the reflog of the current branch.

\(fn)" t nil)

(autoload 'magit-reflog "magit-log" "\
Display the reflog of a branch.

\(fn REF)" t nil)

(autoload 'magit-reflog-head "magit-log" "\
Display the `HEAD' reflog.

\(fn)" t nil)

(autoload 'magit-log-move-to-parent "magit-log" "\
Move to the Nth parent of the current commit.

\(fn &optional N)" t nil)

(autoload 'magit-cherry "magit-log" "\
Show commits in a branch that are not merged in the upstream branch.

\(fn HEAD UPSTREAM)" t nil)

;;;***

;;;### (autoloads nil "magit-notes" "magit-notes.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-notes.el
 (autoload 'magit-notes-popup "magit" nil t)

;;;***

;;;### (autoloads nil "magit-refs" "magit-refs.el" (23009 21875 0
;;;;;;  0))
;;; Generated autoloads from magit-refs.el

(autoload 'magit-show-refs-popup "magit-refs" "\
Popup console for `magit-show-refs'.

\(fn &optional ARG)" t nil)

(autoload 'magit-show-refs-head "magit-refs" "\
List and compare references in a dedicated buffer.
Refs are compared with `HEAD'.

\(fn &optional ARGS)" t nil)

(autoload 'magit-show-refs-current "magit-refs" "\
List and compare references in a dedicated buffer.
Refs are compared with the current branch or `HEAD' if
it is detached.

\(fn &optional ARGS)" t nil)

(autoload 'magit-show-refs "magit-refs" "\
List and compare references in a dedicated buffer.
Refs are compared with a branch read from the user.

\(fn &optional REF ARGS)" t nil)

;;;***

;;;### (autoloads nil "magit-remote" "magit-remote.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-remote.el

(autoload 'magit-clone "magit-remote" "\
Clone the REPOSITORY to DIRECTORY.
Then show the status buffer for the new repository.

\(fn REPOSITORY DIRECTORY)" t nil)
 (autoload 'magit-remote-popup "magit-remote" nil t)

(autoload 'magit-remote-add "magit-remote" "\
Add a remote named REMOTE and fetch it.

\(fn REMOTE URL &optional ARGS)" t nil)

(autoload 'magit-remote-rename "magit-remote" "\
Rename the remote named OLD to NEW.

\(fn OLD NEW)" t nil)

(autoload 'magit-remote-set-url "magit-remote" "\
Change the url of the remote named REMOTE to URL.

\(fn REMOTE URL)" t nil)

(autoload 'magit-remote-remove "magit-remote" "\
Delete the remote named REMOTE.

\(fn REMOTE)" t nil)

(autoload 'magit-remote-set-head "magit-remote" "\
Set the local representation of REMOTE's default branch.
Query REMOTE and set the symbolic-ref refs/remotes/<remote>/HEAD
accordingly.  With a prefix argument query for the branch to be
used, which allows you to select an incorrect value if you fancy
doing that.

\(fn REMOTE &optional BRANCH)" t nil)

(autoload 'magit-remote-unset-head "magit-remote" "\
Unset the local representation of REMOTE's default branch.
Delete the symbolic-ref \"refs/remotes/<remote>/HEAD\".

\(fn REMOTE)" t nil)
 (autoload 'magit-fetch-popup "magit-remote" nil t)

(autoload 'magit-fetch-from-pushremote "magit-remote" "\
Fetch from the push-remote of the current branch.

\(fn ARGS)" t nil)

(autoload 'magit-fetch-from-upstream "magit-remote" "\
Fetch from the upstream repository of the current branch.

\(fn ARGS)" t nil)

(autoload 'magit-fetch "magit-remote" "\
Fetch from another repository.

\(fn REMOTE ARGS)" t nil)

(autoload 'magit-fetch-branch "magit-remote" "\
Fetch a BRANCH from a REMOTE.

\(fn REMOTE BRANCH ARGS)" t nil)

(autoload 'magit-fetch-refspec "magit-remote" "\
Fetch a REFSPEC from a REMOTE.

\(fn REMOTE REFSPEC ARGS)" t nil)

(autoload 'magit-fetch-all "magit-remote" "\
Fetch from all remotes.

\(fn ARGS)" t nil)

(autoload 'magit-fetch-all-prune "magit-remote" "\
Fetch from all remotes, and prune.
Prune remote tracking branches for branches that have been
removed on the respective remote.

\(fn)" t nil)

(autoload 'magit-fetch-all-no-prune "magit-remote" "\
Fetch from all remotes.

\(fn)" t nil)
 (autoload 'magit-pull-popup "magit-remote" nil t)
 (autoload 'magit-pull-and-fetch-popup "magit-remote" nil t)

(autoload 'magit-pull-from-pushremote "magit-remote" "\
Pull from the push-remote of the current branch.

\(fn ARGS)" t nil)

(autoload 'magit-pull-from-upstream "magit-remote" "\
Pull from the upstream of the current branch.

\(fn ARGS)" t nil)

(autoload 'magit-pull "magit-remote" "\
Pull from a branch read in the minibuffer.

\(fn SOURCE ARGS)" t nil)
 (autoload 'magit-push-popup "magit-remote" nil t)

(autoload 'magit-push-current-to-pushremote "magit-remote" "\
Push the current branch to `branch.<name>.pushRemote'.
If that variable is unset, then push to `remote.pushDefault'.

When `magit-push-current-set-remote-if-missing' is non-nil and
the push-remote is not configured, then read the push-remote from
the user, set it, and then push to it.  With a prefix argument
the push-remote can be changed before pushed to it.

\(fn ARGS &optional PUSH-REMOTE)" t nil)

(autoload 'magit-push-current-to-upstream "magit-remote" "\
Push the current branch to its upstream branch.

When `magit-push-current-set-remote-if-missing' is non-nil and
the upstream is not configured, then read the upstream from the
user, set it, and then push to it.  With a prefix argument the
upstream can be changed before pushed to it.

\(fn ARGS &optional UPSTREAM)" t nil)

(autoload 'magit-push-current "magit-remote" "\
Push the current branch to a branch read in the minibuffer.

\(fn TARGET ARGS)" t nil)

(autoload 'magit-push "magit-remote" "\
Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer.

\(fn SOURCE TARGET ARGS)" t nil)

(autoload 'magit-push-refspecs "magit-remote" "\
Push one or multiple REFSPECS to a REMOTE.
Both the REMOTE and the REFSPECS are read in the minibuffer.  To
use multiple REFSPECS, separate them with commas.  Completion is
only available for the part before the colon, or when no colon
is used.

\(fn REMOTE REFSPECS ARGS)" t nil)

(autoload 'magit-push-matching "magit-remote" "\
Push all matching branches to another repository.
If multiple remotes exist, then read one from the user.
If just one exists, use that without requiring confirmation.

\(fn REMOTE &optional ARGS)" t nil)

(autoload 'magit-push-tags "magit-remote" "\
Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default.

\(fn REMOTE &optional ARGS)" t nil)

(autoload 'magit-push-tag "magit-remote" "\
Push a tag to another repository.

\(fn TAG REMOTE &optional ARGS)" t nil)

(autoload 'magit-push-implicitly "magit-remote" "\
Push somewhere without using an explicit refspec.

This command simply runs \"git push -v [ARGS]\".  ARGS are the
arguments specified in the popup buffer.  No explicit refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

To add this command to the push popup add this to your init file:

  (with-eval-after-load \\='magit-remote
    (magit-define-popup-action \\='magit-push-popup ?P
      \\='magit-push-implicitly--desc
      \\='magit-push-implicitly ?p t))

The function `magit-push-implicitly--desc' attempts to predict
what this command will do.  The value it returns is displayed in
the popup buffer.

\(fn ARGS)" t nil)

(autoload 'magit-push-to-remote "magit-remote" "\
Push to REMOTE without using an explicit refspec.
The REMOTE is read in the minibuffer.

This command simply runs \"git push -v [ARGS] REMOTE\".  ARGS
are the arguments specified in the popup buffer.  No refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

To add this command to the push popup add this to your init file:

  (with-eval-after-load \\='magit-remote
    (magit-define-popup-action \\='magit-push-popup ?r
      \\='magit-push-to-remote--desc
      \\='magit-push-to-remote ?p t))

\(fn REMOTE ARGS)" t nil)
 (autoload 'magit-patch-popup "magit-remote" nil t)

(autoload 'magit-format-patch "magit-remote" "\
Create patches for the commits in RANGE.
When a single commit is given for RANGE, create a patch for the
changes introduced by that commit (unlike 'git format-patch'
which creates patches for all commits that are reachable from
`HEAD' but not from the specified commit).

\(fn RANGE ARGS)" t nil)

(autoload 'magit-request-pull "magit-remote" "\
Request upstream to pull from you public repository.

URL is the url of your publically accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit.

\(fn URL START END)" t nil)

;;;***

;;;### (autoloads nil "magit-repos" "magit-repos.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-repos.el

(autoload 'magit-list-repositories "magit-repos" "\
Display a list of repositories.

Use the options `magit-repository-directories'
and `magit-repository-directories-depth' to
control which repositories are displayed.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "magit-sequence" "magit-sequence.el" (23009
;;;;;;  21875 0 0))
;;; Generated autoloads from magit-sequence.el

(autoload 'magit-sequencer-continue "magit-sequence" "\
Resume the current cherry-pick or revert sequence.

\(fn)" t nil)

(autoload 'magit-sequencer-skip "magit-sequence" "\
Skip the stopped at commit during a cherry-pick or revert sequence.

\(fn)" t nil)

(autoload 'magit-sequencer-abort "magit-sequence" "\
Abort the current cherry-pick or revert sequence.
This discards all changes made since the sequence started.

\(fn)" t nil)
 (autoload 'magit-cherry-pick-popup "magit-sequence" nil t)

(autoload 'magit-cherry-pick "magit-sequence" "\
Cherry-pick COMMIT.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then pick all of them,
without prompting.

\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-cherry-apply "magit-sequence" "\
Apply the changes in COMMIT but do not commit them.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then apply all of them,
without prompting.

\(fn COMMIT &optional ARGS)" t nil)
 (autoload 'magit-revert-popup "magit-sequence" nil t)

(autoload 'magit-revert "magit-sequence" "\
Revert COMMIT by creating a new commit.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-revert-no-commit "magit-sequence" "\
Revert COMMIT by applying it in reverse to the worktree.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

\(fn COMMIT &optional ARGS)" t nil)
 (autoload 'magit-am-popup "magit-sequence" nil t)

(autoload 'magit-am-apply-patches "magit-sequence" "\
Apply the patches FILES.

\(fn &optional FILES ARGS)" t nil)

(autoload 'magit-am-apply-maildir "magit-sequence" "\
Apply the patches from MAILDIR.

\(fn &optional MAILDIR ARGS)" t nil)

(autoload 'magit-am-continue "magit-sequence" "\
Resume the current patch applying sequence.

\(fn)" t nil)

(autoload 'magit-am-skip "magit-sequence" "\
Skip the stopped at patch during a patch applying sequence.

\(fn)" t nil)

(autoload 'magit-am-abort "magit-sequence" "\
Abort the current patch applying sequence.
This discards all changes made since the sequence started.

\(fn)" t nil)
 (autoload 'magit-rebase-popup "magit-sequence" nil t)

(autoload 'magit-rebase-onto-pushremote "magit-sequence" "\
Rebase the current branch onto `branch.<name>.pushRemote'.
If that variable is unset, then rebase onto `remote.pushDefault'.

\(fn ARGS)" t nil)

(autoload 'magit-rebase-onto-upstream "magit-sequence" "\
Rebase the current branch onto its upstream branch.

\(fn ARGS)" t nil)

(autoload 'magit-rebase "magit-sequence" "\
Rebase the current branch onto a branch read in the minibuffer.
All commits that are reachable from `HEAD' but not from the
selected branch TARGET are being rebased.

\(fn TARGET ARGS)" t nil)

(autoload 'magit-rebase-subset "magit-sequence" "\
Rebase a subset of the current branch's history onto a new base.
Rebase commits from START to `HEAD' onto NEWBASE.
START has to be selected from a list of recent commits.

\(fn NEWBASE START ARGS)" t nil)

(autoload 'magit-rebase-interactive "magit-sequence" "\
Start an interactive rebase sequence.

\(fn COMMIT ARGS)" t nil)

(autoload 'magit-rebase-autosquash "magit-sequence" "\
Combine squash and fixup commits with their intended targets.

\(fn ARGS)" t nil)

(autoload 'magit-rebase-edit-commit "magit-sequence" "\
Edit a single older commit using rebase.

\(fn COMMIT ARGS)" t nil)

(autoload 'magit-rebase-reword-commit "magit-sequence" "\
Reword a single older commit using rebase.

\(fn COMMIT ARGS)" t nil)

(autoload 'magit-rebase-continue "magit-sequence" "\
Restart the current rebasing operation.
In some cases this pops up a commit message buffer for you do
edit.  With a prefix argument the old message is reused as-is.

\(fn &optional NOEDIT)" t nil)

(autoload 'magit-rebase-skip "magit-sequence" "\
Skip the current commit and restart the current rebase operation.

\(fn)" t nil)

(autoload 'magit-rebase-edit "magit-sequence" "\
Edit the todo list of the current rebase operation.

\(fn)" t nil)

(autoload 'magit-rebase-abort "magit-sequence" "\
Abort the current rebase operation, restoring the original branch.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "magit-stash" "magit-stash.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-stash.el
 (autoload 'magit-stash-popup "magit-stash" nil t)

(autoload 'magit-stash "magit-stash" "\
Create a stash of the index and working tree.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-index "magit-stash" "\
Create a stash of the index only.
Unstaged and untracked changes are not stashed.  The stashed
changes are applied in reverse to both the index and the
worktree.  This command can fail when the worktree is not clean.
Applying the resulting stash has the inverse effect.

\(fn MESSAGE)" t nil)

(autoload 'magit-stash-worktree "magit-stash" "\
Create a stash of unstaged changes in the working tree.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-keep-index "magit-stash" "\
Create a stash of the index and working tree, keeping index intact.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-snapshot "magit-stash" "\
Create a snapshot of the index and working tree.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-snapshot-index "magit-stash" "\
Create a snapshot of the index only.
Unstaged and untracked changes are not stashed.

\(fn)" t nil)

(autoload 'magit-snapshot-worktree "magit-stash" "\
Create a snapshot of unstaged changes in the working tree.
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

\(fn &optional INCLUDE-UNTRACKED)" t nil)

(autoload 'magit-stash-apply "magit-stash" "\
Apply a stash to the working tree.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index.

\(fn STASH)" t nil)

(autoload 'magit-stash-drop "magit-stash" "\
Remove a stash from the stash list.
When the region is active offer to drop all contained stashes.

\(fn STASH)" t nil)

(autoload 'magit-stash-clear "magit-stash" "\
Remove all stashes saved in REF's reflog by deleting REF.

\(fn REF)" t nil)

(autoload 'magit-stash-branch "magit-stash" "\
Create and checkout a new BRANCH from STASH.

\(fn STASH BRANCH)" t nil)

(autoload 'magit-stash-format-patch "magit-stash" "\
Create a patch from STASH

\(fn STASH)" t nil)

(autoload 'magit-stash-list "magit-stash" "\
List all stashes in a buffer.

\(fn)" t nil)

(autoload 'magit-stash-show "magit-stash" "\
Show all diffs of a stash in a buffer.

\(fn STASH &optional ARGS FILES)" t nil)

;;;***

;;;### (autoloads nil "magit-status" "magit-status.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-status.el

(autoload 'magit-init "magit-status" "\
Initialize a Git repository, then show its status.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally.

\(fn DIRECTORY)" t nil)

(autoload 'magit-status "magit-status" "\
Show the status of the current Git repository in a buffer.
With a prefix argument prompt for a repository to be shown.
With two prefix arguments prompt for an arbitrary directory.
If that directory isn't the root of an existing repository,
then offer to initialize it as a new repository.

\(fn &optional DIRECTORY CACHE)" t nil)

(autoload 'magit-status-internal "magit-status" "\


\(fn DIRECTORY)" nil nil)

;;;***

;;;### (autoloads nil "magit-submodule" "magit-submodule.el" (23009
;;;;;;  21875 0 0))
;;; Generated autoloads from magit-submodule.el
 (autoload 'magit-submodule-popup "magit-submodule" nil t)

(autoload 'magit-submodule-add "magit-submodule" "\
Add the repository at URL as a submodule.

Optional PATH is the path to the submodule relative to the root
of the superproject.  If it is nil, then the path is determined
based on the URL.

Optional NAME is the name of the submodule.  If it is nil, then
PATH also becomes the name.

\(fn URL &optional PATH NAME)" t nil)

(autoload 'magit-submodule-read-name-for-path "magit-submodule" "\


\(fn PATH)" nil nil)

(autoload 'magit-submodule-setup "magit-submodule" "\
Clone and register missing submodules and checkout appropriate commits.

\(fn)" t nil)

(autoload 'magit-submodule-init "magit-submodule" "\
Register submodules listed in \".gitmodules\" into \".git/config\".

\(fn)" t nil)

(autoload 'magit-submodule-update "magit-submodule" "\
Clone missing submodules and checkout appropriate commits.
With a prefix argument also register submodules in \".git/config\".

\(fn &optional INIT)" t nil)

(autoload 'magit-submodule-sync "magit-submodule" "\
Update each submodule's remote URL according to \".gitmodules\".

\(fn)" t nil)

(autoload 'magit-submodule-fetch "magit-submodule" "\
Fetch all submodules.
With a prefix argument fetch all remotes.

\(fn &optional ALL)" t nil)

(autoload 'magit-submodule-deinit "magit-submodule" "\
Unregister the submodule at PATH.

\(fn PATH)" t nil)

(autoload 'magit-insert-modules "magit-submodule" "\
Insert submodule sections.
Hook `magit-module-sections-hook' controls which module sections
are inserted, and option `magit-insert-modules-nested' controls
whether they are wrapped in an additional section.

\(fn)" nil nil)

(autoload 'magit-insert-modules-overview "magit-submodule" "\
Insert sections for all modules.
For each section insert the path and the output of `git describe --tags',
or, failing that, the abbreviated HEAD commit hash.

\(fn)" nil nil)

(autoload 'magit-insert-modules-unpulled-from-upstream "magit-submodule" "\
Insert sections for modules that haven't been pulled from the upstream.
These sections can be expanded to show the respective commits.

\(fn)" nil nil)

(autoload 'magit-insert-modules-unpulled-from-pushremote "magit-submodule" "\
Insert sections for modules that haven't been pulled from the push-remote.
These sections can be expanded to show the respective commits.

\(fn)" nil nil)

(autoload 'magit-insert-modules-unpushed-to-upstream "magit-submodule" "\
Insert sections for modules that haven't been pushed to the upstream.
These sections can be expanded to show the respective commits.

\(fn)" nil nil)

(autoload 'magit-insert-modules-unpushed-to-pushremote "magit-submodule" "\
Insert sections for modules that haven't been pushed to the push-remote.
These sections can be expanded to show the respective commits.

\(fn)" nil nil)

(autoload 'magit-list-submodules "magit-submodule" "\
Display a list of the current repository's submodules.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "magit-subtree" "magit-subtree.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-subtree.el
 (autoload 'magit-subtree-popup "magit-subtree" nil t)

(autoload 'magit-subtree-add "magit-subtree" "\
Add REF from REPOSITORY as a new subtree at PREFIX.

\(fn PREFIX REPOSITORY REF ARGS)" t nil)

(autoload 'magit-subtree-add-commit "magit-subtree" "\
Add COMMIT as a new subtree at PREFIX.

\(fn PREFIX COMMIT ARGS)" t nil)

(autoload 'magit-subtree-merge "magit-subtree" "\
Merge COMMIT into the PREFIX subtree.

\(fn PREFIX COMMIT ARGS)" t nil)

(autoload 'magit-subtree-pull "magit-subtree" "\
Pull REF from REPOSITORY into the PREFIX subtree.

\(fn PREFIX REPOSITORY REF ARGS)" t nil)

(autoload 'magit-subtree-push "magit-subtree" "\
Extract the history of the subtree PREFIX and push it to REF on REPOSITORY.

\(fn PREFIX REPOSITORY REF ARGS)" t nil)

(autoload 'magit-subtree-split "magit-subtree" "\
Extract the history of the subtree PREFIX.

\(fn PREFIX COMMIT ARGS)" t nil)

;;;***

;;;### (autoloads nil "magit-utils" "magit-utils.el" (23009 21875
;;;;;;  0 0))
;;; Generated autoloads from magit-utils.el

(autoload 'magit-emacs-Q-command "magit-utils" "\
Show a shell command that runs an uncustomized Emacs with only Magit loaded.
See info node `(magit)Debugging Tools' for more information.

\(fn)" t nil)

(autoload 'Info-follow-nearest-node--magit-gitman "magit-utils" "\


\(fn FN &optional FORK)" nil nil)

(advice-add 'Info-follow-nearest-node :around 'Info-follow-nearest-node--magit-gitman)

(autoload 'org-man-export--magit-gitman "magit-utils" "\


\(fn FN LINK DESCRIPTION FORMAT)" nil nil)

(advice-add 'org-man-export :around 'org-man-export--magit-gitman)

;;;***

;;;### (autoloads nil "magit-wip" "magit-wip.el" (23009 21875 0 0))
;;; Generated autoloads from magit-wip.el

(defvar magit-wip-after-save-mode nil "\
Non-nil if Magit-Wip-After-Save mode is enabled.
See the `magit-wip-after-save-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-after-save-mode'.")

(custom-autoload 'magit-wip-after-save-mode "magit-wip" nil)

(autoload 'magit-wip-after-save-mode "magit-wip" "\
Toggle Magit-Wip-After-Save-Local mode in all buffers.
With prefix ARG, enable Magit-Wip-After-Save mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-Wip-After-Save-Local mode is enabled in all buffers where
`magit-wip-after-save-local-mode-turn-on' would do it.
See `magit-wip-after-save-local-mode' for more information on Magit-Wip-After-Save-Local mode.

\(fn &optional ARG)" t nil)

(defvar magit-wip-after-apply-mode nil "\
Non-nil if Magit-Wip-After-Apply mode is enabled.
See the `magit-wip-after-apply-mode' command
for a description of this minor mode.")

(custom-autoload 'magit-wip-after-apply-mode "magit-wip" nil)

(autoload 'magit-wip-after-apply-mode "magit-wip" "\
Commit to work-in-progress refs.

After applying a change using any \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected files to the current wip refs.  For each branch there
may be two wip refs; one contains snapshots of the files as found
in the worktree and the other contains snapshots of the entries
in the index.

\(fn &optional ARG)" t nil)

(defvar magit-wip-before-change-mode nil "\
Non-nil if Magit-Wip-Before-Change mode is enabled.
See the `magit-wip-before-change-mode' command
for a description of this minor mode.")

(custom-autoload 'magit-wip-before-change-mode "magit-wip" nil)

(autoload 'magit-wip-before-change-mode "magit-wip" "\
Commit to work-in-progress refs before certain destructive changes.

Before invoking a revert command or an \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected tracked files to the current wip refs.  For each branch
there may be two wip refs; one contains snapshots of the files
as found in the worktree and the other contains snapshots of the
entries in the index.

Only changes to files which could potentially be affected by the
command which is about to be called are committed.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "magit-worktree" "magit-worktree.el" (23009
;;;;;;  21875 0 0))
;;; Generated autoloads from magit-worktree.el

(autoload 'magit-worktree-checkout "magit-worktree" "\
Checkout BRANCH in a new worktree at PATH.

\(fn PATH BRANCH)" t nil)

(autoload 'magit-worktree-branch "magit-worktree" "\
Create a new BRANCH and check it out in a new worktree at PATH.

\(fn PATH BRANCH START-POINT &optional FORCE)" t nil)

;;;***

;;;### (autoloads nil nil ("magit-core.el" "magit-git.el" "magit-margin.el"
;;;;;;  "magit-mode.el" "magit-obsolete.el" "magit-pkg.el" "magit-process.el"
;;;;;;  "magit-section.el") (23009 21875 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; magit-autoloads.el ends here
