# wgrep.el

wgrep allows you to edit a grep buffer and apply those changes to
the file buffer.

## Install:

Put this file into load-path'ed directory, and byte compile it if
desired. And put the following expression into your ~/.emacs.

    (require 'wgrep)

## Usage:

You can edit the text in the *grep* buffer after typing `C-c C-p`.
After that the changed text is highlighted.
The following keybindings are defined:

* `C-c C-e`: Apply the changes to file buffers.
* `C-c C-u`: All changes are unmarked and ignored.
* `C-c C-d`: Mark as delete to current line (including newline).
* `C-c C-r`: Remove the changes in the region (these changes are not applied to the files. Of course, the remaining changes can still be applied to the files.)
* `C-c C-p`: Toggle read-only area.
* `C-c C-k`: Discard all changes and exit.
* `C-x C-q`: Exit wgrep mode.

To save all buffers that wgrep has changed, run

    M-x wgrep-save-all-buffers

To save buffer automatically when `wgrep-finish-edit'.

    (setq wgrep-auto-save-buffer t)

You can change the default key binding to switch to wgrep.

    (setq wgrep-enable-key "r")

To apply all changes wheather or not buffer is read-only.

    (setq wgrep-change-readonly-file t)

## History:

This program is a forked version. the original version can be downloaded from
http://www.bookshelf.jp/elc/grep-edit.el

Following added implementations and differences.

* Support GNU grep context option -A -B and -C
* Some bugfix. (wrong coloring text etc..)
* wdired.el like interface.
* Remove all advice.
* Bind to local variables. (grep-a-lot.el works well)
* After save buffer, colored face will be removed.
* Change face easy to see.
* Reinforce checking error.
* Support removing whole line include new-line.
