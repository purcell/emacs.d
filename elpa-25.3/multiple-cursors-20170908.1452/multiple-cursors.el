;;; multiple-cursors.el --- Multiple cursors for emacs.

;; Copyright (C) 2012-2016 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 1.4.0
;; Keywords: editing cursors

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

;;; Commentary:

;; Multiple cursors for Emacs. This is some pretty crazy functionality, so yes,
;; there are kinks. Don't be afraid tho, I've been using it since 2011 with
;; great success and much merriment.

;; ## Basic usage

;; Start out with:

;;     (require 'multiple-cursors)

;; Then you have to set up your keybindings - multiple-cursors doesn't presume to
;; know how you'd like them laid out. Here are some examples:

;; When you have an active region that spans multiple lines, the following will
;; add a cursor to each line:

;;     (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; When you want to add multiple cursors not based on continuous lines, but based on
;; keywords in the buffer, use:

;;     (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;     (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;     (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; First mark the word, then add more cursors.

;; To get out of multiple-cursors-mode, press `<return>` or `C-g`. The latter will
;; first disable multiple regions before disabling multiple cursors. If you want to
;; insert a newline in multiple-cursors-mode, use `C-j`.

;; ## Video

;; You can [watch an intro to multiple-cursors at Emacs Rocks](http://emacsrocks.com/e13.html).

;; ## Command overview

;; ### Mark one more occurrence

;;  - `mc/mark-next-like-this`: Adds a cursor and region at the next part of the buffer forwards that matches the current region.
;;  - `mc/mark-next-like-this-word`: Adds a cursor and region at the next part of the buffer forwards that matches the current region, if  no region is selected it selects the word at the point.
;;  - `mc/mark-next-like-this-symbol`: Adds a cursor and region at the next part of the buffer forwards that matches the current region, if  no region is selected it selects the symbol at the point.
;;  - `mc/mark-next-word-like-this`: Like `mc/mark-next-like-this` but only for whole words.
;;  - `mc/mark-next-symbol-like-this`: Like `mc/mark-next-like-this` but only for whole symbols.
;;  - `mc/mark-previous-like-this`: Adds a cursor and region at the next part of the buffer backwards that matches the current region.
;;  - `mc/mark-previous-word-like-this`: Like `mc/mark-previous-like-this` but only for whole words.
;;  - `mc/mark-previous-symbol-like-this`: Like `mc/mark-previous-like-this` but only for whole symbols.
;;  - `mc/mark-more-like-this-extended`: Use arrow keys to quickly mark/skip next/previous occurances.
;;  - `mc/add-cursor-on-click`: Bind to a mouse event to add cursors by clicking. See tips-section.

;; ### Mark many occurrences

;;  - `mc/mark-all-like-this`: Marks all parts of the buffer that matches the current region.
;;  - `mc/mark-all-words-like-this`: Like `mc/mark-all-like-this` but only for whole words.
;;  - `mc/mark-all-symbols-like-this`: Like `mc/mark-all-like-this` but only for whole symbols.
;;  - `mc/mark-all-in-region`: Prompts for a string to match in the region, adding cursors to all of them.
;;  - `mc/mark-all-like-this-in-defun`: Marks all parts of the current defun that matches the current region.
;;  - `mc/mark-all-words-like-this-in-defun`: Like `mc/mark-all-like-this-in-defun` but only for whole words.
;;  - `mc/mark-all-symbols-like-this-in-defun`: Like `mc/mark-all-like-this-in-defun` but only for whole symbols.
;;  - `mc/mark-all-like-this-dwim`: Tries to be smart about marking everything you want. Can be pressed multiple times.

;; ### Special

;;  - `set-rectangular-region-anchor`: Think of this one as `set-mark` except you're marking a rectangular region.
;;  - `mc/mark-sgml-tag-pair`: Mark the current opening and closing tag.
;;  - `mc/insert-numbers`: Insert increasing numbers for each cursor, top to bottom.
;;  - `mc/insert-letters`: Insert increasing letters for each cursor, top to bottom.
;;  - `mc/sort-regions`: Sort the marked regions alphabetically.
;;  - `mc/reverse-regions`: Reverse the order of the marked regions.

;; ## Tips and tricks

;; - To get out of multiple-cursors-mode, press `<return>` or `C-g`. The latter will
;;   first disable multiple regions before disabling multiple cursors. If you want to
;;   insert a newline in multiple-cursors-mode, use `C-j`.
;;
;; - Sometimes you end up with cursors outside of your view. You can
;;   scroll the screen to center on each cursor with `C-v` and `M-v`.
;;
;; - Try pressing `mc/mark-next-like-this` with no region selected. It will just add a cursor
;;   on the next line.
;;
;; - Try pressing `mc/mark-next-like-this-word` or
;;   `mc/mark-next-like-this-symbol` with no region selected. It will
;;   mark the symbol and add a cursor at the next occurance
;;
;; - Try pressing `mc/mark-all-like-this-dwim` on a tagname in html-mode.
;;
;; - Notice that the number of cursors active can be seen in the modeline.
;;
;; - If you get out of multiple-cursors-mode and yank - it will yank only
;;   from the kill-ring of main cursor. To yank from the kill-rings of
;;   every cursor use yank-rectangle, normally found at C-x r y.
;;
;; - You can use `mc/reverse-regions` with nothing selected and just one cursor.
;;   It will then flip the sexp at point and the one below it.
;;
;; - If you would like to keep the global bindings clean, and get custom keybindings
;;   when the region is active, you can try [region-bindings-mode](https://github.com/fgallina/region-bindings-mode).
;;
;; BTW, I highly recommend adding `mc/mark-next-like-this` to a key binding that's
;; right next to the key for `er/expand-region`.

;; ### Binding mouse events

;; To override a mouse event, you will likely have to also unbind the
;; `down-mouse` part of the event. Like this:
;;
;;     (global-unset-key (kbd "M-<down-mouse-1>"))
;;     (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
;;
;; Or you can do like me and find an unused, but less convenient, binding:
;;
;;     (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; ## Unknown commands

;; Multiple-cursors uses two lists of commands to know what to do: the run-once list
;; and the run-for-all list. It comes with a set of defaults, but it would be beyond silly
;; to try and include all the known Emacs commands.

;; So that's why multiple-cursors occasionally asks what to do about a command. It will
;; then remember your choice by saving it in `~/.emacs.d/.mc-lists.el`. You can change
;; the location with:

;;     (setq mc/list-file "/my/preferred/file")

;; ## Known limitations

;; * isearch-forward and isearch-backward aren't supported with multiple cursors.
;;   You should feel free to add a simplified version that can work with it.
;; * Commands run with `M-x` won't be repeated for all cursors.
;; * All key bindings that refer to lambdas are always run for all cursors. If you
;;   need to limit it, you will have to give it a name.
;; * Redo might screw with your cursors. Undo works very well.

;; ## Contribute

;; Yes, please do. There's a suite of tests, so remember to add tests for your
;; specific feature, or I might break it later.

;; You'll find the repo at:

;;     https://github.com/magnars/multiple-cursors.el

;; To fetch the test dependencies:

;;     $ cd /path/to/multiple-cursors
;;     $ git submodule update --init

;; Run the tests with:

;;     $ ./util/ecukes/ecukes --graphical

;; ## Contributors

;; * [Takafumi Arakaki](https://github.com/tkf) made .mc-lists.el diff friendly
;; * [Marco Baringer](https://github.com/segv) contributed looping to mc/cycle and adding cursors without region for mark-more.
;; * [Ivan Andrus](https://github.com/gvol) added showing number of cursors in mode-line
;; * [Fuco](https://github.com/Fuco1) added the first version of `mc/mark-all-like-this-dwim`

;; Thanks!

;;; Code:

(require 'mc-edit-lines)
(require 'mc-cycle-cursors)
(require 'mc-mark-more)
(require 'mc-mark-pop)
(require 'rectangular-region-mode)
(require 'mc-separate-operations)
(require 'mc-hide-unmatched-lines-mode)

(provide 'multiple-cursors)

;;; multiple-cursors.el ends here
