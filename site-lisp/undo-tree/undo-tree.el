
;;; undo-tree.el --- Treat undo history as a tree


;; Copyright (C) 2009-2010 Toby Cubitt

;; Author: Toby Cubitt <toby-undo-tree@dr-qubit.org>
;; Version: 0.1.6
;; Keywords: undo, redo, history, tree
;; URL: http://www.dr-qubit.org/emacs.php
;; Git Repository: http://www.dr-qubit.org/git/undo-tree.git

;; This file is NOT part of Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Emacs has a powerful undo system. Unlike the standard undo/redo system in
;; most software, it allows you to recover *any* past state of a buffer
;; (whereas the standard undo/redo system can lose past states as soon as you
;; redo). However, this power comes at a price: many people find Emacs' undo
;; system confusing and difficult to use, spawning a number of packages that
;; replace it with the less powerful but more intuitive undo/redo system.
;;
;; Both the loss of data with standard undo/redo, and the confusion of Emacs'
;; undo, stem from trying to treat undo history as a linear sequence of
;; changes. It's not. The `undo-tree-mode' provided by this package replaces
;; Emacs' undo system with a system that treats undo history as what it is: a
;; branching tree of changes. This simple idea allows the more intuitive
;; behaviour of the standard undo/redo system to be combined with the power of
;; never losing any history. An added side bonus is that undo history can in
;; some cases be stored more efficiently, allowing more changes to accumulate
;; before Emacs starts discarding history.
;;
;; The only downside to this more advanced yet simpler undo system is that it
;; was inspired by Vim. But, after all, most successful religions steal the
;; best ideas from their competitors!
;;
;;
;; Installation
;; ============
;;
;; This package has only been tested with Emacs versions 22, 23 and CVS. It
;; will not work without modifications in earlier versions of Emacs.
;;
;; To install `undo-tree-mode', make sure this file is saved in a directory in
;; your `load-path', and add the line:
;;
;;   (require 'undo-tree)
;;
;; to your .emacs file. Byte-compiling undo-tree.el is recommended (e.g. using
;; "M-x byte-compile-file" from within emacs).
;;
;; If you want to replace the standard Emacs' undo system with the
;; `undo-tree-mode' system in all buffers, you can enable it globally by
;; adding:
;;
;;   (global-undo-tree-mode)
;;
;; to your .emacs file.
;;
;;
;; Quick-Start
;; ===========
;;
;; If you're the kind of person who likes jump in the car and drive, without
;; bothering to first figure out whether the button on the left dips the
;; headlights or operates the ejector seat (after all, you'll soon figure it
;; out when you push it), then here's the minimum you need to know:
;;
;; `undo-tree-mode' and `global-undo-tree-mode'
;;   Enable undo-tree mode (either in the current buffer or globally).
;;
;; C-_  C-/  (`undo-tree-undo')
;;   Undo changes.
;;
;; M-_  C-?  (`undo-tree-redo')
;;   Redo changes.
;;
;; `undo-tree-switch-branch'
;;   Switch undo-tree branch.
;;   (What does this mean? Better press the button and see!)
;;
;; C-x u  (`undo-tree-visualize')
;;   Visualize the undo tree.
;;   (Better try pressing this button too!)
;;
;; C-x r u  (`undo-tree-save-state-to-register')
;;   Save current buffer state to register.
;;
;; C-x r U  (`undo-tree-restore-state-from-register')
;;   Restore buffer state from register.
;;
;;
;; In the undo-tree visualizer:
;;
;; <up>  p  C-p  (`undo-tree-visualize-undo')
;;   Undo changes.
;;
;; <down>  n  C-n  (`undo-tree-visualize-undo')
;;   Undo changes.
;;
;; <left>  b  C-b  (`undo-tree-visualize-switch-branch-left')
;;   Switch to previous undo-tree branch.
;;
;; <right>  f  C-f  (`undo-tree-visualize-switch-branch-right')
;;   Switch to next undo-tree branch.
;;
;; t  (`undo-tree-visualizer-toggle-timestamps')
;;   Toggle display of time-stamps.
;;
;; q  C-q  (`undo-tree-visualizer-quit')
;;   Quit undo-tree-visualizer.
;;
;; ,  <
;;   Scroll left.
;;
;; .  >
;;   Scroll right.
;;
;; <pgup>
;;   Scroll up.
;;
;; <pgdown>
;;   Scroll down.
;;
;;
;;
;; Undo Systems
;; ============
;;
;; To understand the different undo systems, it's easiest to consider an
;; example. Imagine you make a few edits in a buffer. As you edit, you
;; accumulate a history of changes, which we might visualize as a string of
;; past buffer states, growing downwards:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o  (first edit)
;;                                |
;;                                |
;;                                o  (second edit)
;;                                |
;;                                |
;;                                x  (current buffer state)
;;
;;
;; Now imagine that you undo the last two changes. We can visualize this as
;; rewinding the current state back two steps:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                x  (current buffer state)
;;                                |
;;                                |
;;                                o
;;                                |
;;                                |
;;                                o
;;
;;
;; However, this isn't a good representation of what Emacs' undo system
;; does. Instead, it treats the undos as *new* changes to the buffer, and adds
;; them to the history:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o  (first edit)
;;                                |
;;                                |
;;                                o  (second edit)
;;                                |
;;                                |
;;                                x  (buffer state before undo)
;;                                |
;;                                |
;;                                o  (first undo)
;;                                |
;;                                |
;;                                x  (second undo)
;;
;;
;; Actually, since the buffer returns to a previous state after an undo, a
;; better way to visualize it is to imagine the string of changes turning back
;; on itself:
;;
;;        (initial buffer state)  o
;;                                |
;;                                |
;;                  (first edit)  o  x  (second undo)
;;                                |  |
;;                                |  |
;;                 (second edit)  o  o  (first undo)
;;                                | /
;;                                |/
;;                                o  (buffer state before undo)
;;
;; Treating undos as new changes might seem a strange thing to do. But the
;; advantage becomes clear as soon as we imagine what happens when you edit
;; the buffer again. Since you've undone a couple of changes, new edits will
;; branch off from the buffer state that you've rewound to. Conceptually, it
;; looks like this:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o
;;                                |\
;;                                | \
;;                                o  x  (new edit)
;;                                |
;;                                |
;;                                o
;;
;; The standard undo/redo system only lets you go backwards and forwards
;; linearly. So as soon as you make that new edit, it discards the old
;; branch. Emacs' undo just keeps adding changes to the end of the string. So
;; the undo history in the two systems now looks like this:
;;
;;            Undo/Redo:                      Emacs' undo
;;
;;               o                                o
;;               |                                |
;;               |                                |
;;               o                                o  o
;;               .\                               |  |\
;;               . \                              |  | \
;;               .  x  (new edit)                 o  o  |
;;   (discarded  .                                | /   |
;;     branch)   .                                |/    |
;;               .                                o     |
;;                                                      |
;;                                                      |
;;                                                      x  (new edit)
;;
;; Now, what if you change your mind about those undos, and decide you did
;; like those other changes you'd made after all? With the standard undo/redo
;; system, you're stuck. There's no way to recover them, because that branch
;; was discarded when you made the new edit.
;;
;; However, in Emacs' undo system, those old buffer states are still there in
;; the undo history. You just have to rewind back through the new edit, and
;; back through the changes made by the undos, until you reach them. Of
;; course, since Emacs treats undos (even undos of undos!) as new changes,
;; you're really weaving backwards and forwards through the history, all the
;; time adding new changes to the end of the string as you go:
;;
;;                       o
;;                       |
;;                       |
;;                       o  o     o  (undo new edit)
;;                       |  |\    |\
;;                       |  | \   | \
;;                       o  o  |  |  o  (undo the undo)
;;                       | /   |  |  |
;;                       |/    |  |  |
;;      (trying to get   o     |  |  x  (undo the undo)
;;       to this state)        | /
;;                             |/
;;                             o
;;
;; So far, this is still reasonably intuitive to use. It doesn't behave so
;; differently to standard undo/redo, except that by going back far enough you
;; can access changes that would be lost in standard undo/redo.
;;
;; However, imagine that after undoing as just described, you decide you
;; actually want to rewind right back to the initial state. If you're lucky,
;; and haven't invoked any command since the last undo, you can just keep on
;; undoing until you get back to the start:
;;
;;      (trying to get   o              x  (got there!)
;;       to this state)  |              |
;;                       |              |
;;                       o  o     o     o  (keep undoing)
;;                       |  |\    |\    |
;;                       |  | \   | \   |
;;                       o  o  |  |  o  o  (keep undoing)
;;                       | /   |  |  | /
;;                       |/    |  |  |/
;;      (already undid   o     |  |  o  (got this far)
;;       to this state)        | /
;;                             |/
;;                             o
;;
;; But if you're unlucky, you happen to have moved the point (say) after
;; getting to the point labelled "got this far". In that case, you've "broken
;; the undo chain". If you try to undo now, Emacs thinks you're trying to undo
;; the undos! So to get back to the initial state you now have to rewind
;; through *all* the changes, including the undos you just did:
;;
;;      (trying to get   o                          x  (finally got there!)
;;       to this state)  |                          |
;;                       |                          |
;;                       o  o     o     o     o     o
;;                       |  |\    |\    |\    |\    |
;;                       |  | \   | \   | \   | \   |
;;                       o  o  |  |  o  o  o  |  o  o
;;                       | /   |  |  | /   |  |  | /
;;                       |/    |  |  |/    |  |  |/
;;      (already undid   o     |  |  o<.   |  |  o
;;       to this state)        | /     :   | /
;;                             |/      :   |/
;;                             o       :   o
;;                                     :
;;                             (got this far, but
;;                              broke undo chain)
;;
;; Confused?
;;
;; In practice you can just hold down the undo key until you reach the buffer
;; state that you want. But whatever you do, don't move around in the buffer
;; to check you've got back to where you want! Because you'll break the undo
;; chain, and then you'll have to traverse the entire string of undos again to
;; get back to the point at which you broke the chain. Undo in region and
;; commands such as `undo-only' help to make using Emacs' undo a little
;; easier, but nonetheless it remains confusing for many people.
;;
;;
;; So what does `undo-tree-mode' do? Remember the diagram we drew to represent
;; the history we've been discussing (make a few edits, undo a couple of them,
;; and edit again)? The diagram that conceptually represented our undo
;; history, before we started discussing specific undo systems? It looked like
;; this:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o
;;                                |\
;;                                | \
;;                                o  x  (current state)
;;                                |
;;                                |
;;                                o
;;
;; Well, that's *exactly* what the undo history looks like to
;; `undo-tree-mode'.  It doesn't discard the old branch (as standard undo/redo
;; does), nor does it treat undos as new changes to be added to the end of a
;; linear string of buffer states (as Emacs' undo does). It just keeps track
;; of the tree of branching changes that make up the entire undo history.
;;
;; If you undo from this point, you'll rewind back up the tree to the previous
;; state:
;;
;;                                o
;;                                |
;;                                |
;;                                x  (undo)
;;                                |\
;;                                | \
;;                                o  o
;;                                |
;;                                |
;;                                o
;;
;; If you were to undo again, you'd rewind back to the initial state. If on
;; the other hand you redo the change, you'll end up back at the bottom of the
;; most recent branch:
;;
;;                                o  (undo takes you here)
;;                                |
;;                                |
;;                                o  (start here)
;;                                |\
;;                                | \
;;                                o  x  (redo takes you here)
;;                                |
;;                                |
;;                                o
;;
;; So far, this is just like the standard undo/redo system. But what if you
;; want to return to a buffer state located on a previous branch of the
;; history? Since `undo-tree-mode' keeps the entire history, you simply need
;; to tell it to switch to a different branch, and then redo the changes you
;; want:
;;
;;                                o
;;                                |
;;                                |
;;                                o  (start here, but switch
;;                                |\  to the other branch)
;;                                | \
;;                        (redo)  o  o
;;                                |
;;                                |
;;                        (redo)  x
;;
;; Now you're on the other branch, and if you undo and redo changes you'll
;; stay on that branch, moving up and down through the buffer states located
;; on that branch. Until you decide to switch branches again, of course.
;;
;; Real undo trees might have multiple branches and sub-branches:
;;
;;                                o
;;                            ____|______
;;                           /           \
;;                          o             o
;;                      ____|__         __|
;;                     /    |  \       /   \
;;                    o     o   o     o     x
;;                    |               |
;;                   / \             / \
;;                  o   o           o   o
;;
;; Trying to imagine what Emacs' undo would do as you move about such a tree
;; will likely frazzle your brain circuits! But in `undo-tree-mode', you're
;; just moving around this undo history tree. Most of the time, you'll
;; probably only need to stay on the most recent branch, in which case it
;; behaves like standard undo/redo, so is just as simple to understand. But if
;; you ever need to recover a buffer state on a different branch, the
;; possibility of switching between branches and accessing the full undo
;; history is still there.
;;
;;
;;
;; The Undo-Tree Visualizer
;; ========================
;;
;; Actually, it gets better. You don't have to imagine all these tree
;; diagrams, because `undo-tree-mode' includes an undo-tree visualizer which
;; draws them for you! In fact, it draws even better diagrams: it highlights
;; the node representing the current buffer state, it highlights the current
;; branch, and you can toggle the display of time-stamps for each buffer
;; state. (There's one other tiny difference: the visualizer puts the most
;; recent branch on the left rather than the right.)
;;
;; In the visualizer, the usual keys for moving up and down a buffer instead
;; move up and down the undo history tree (e.g. the up and down arrow keys, or
;; "C-n" and "C-p"). The state of the "parent" buffer (the buffer whose undo
;; history you are visualizing) is updated as you move around the undo tree in
;; the visualizer. If you reach a branch point in the visualizer, the usual
;; keys for moving forward and backward in a buffer instead switch which
;; branch to follow (e.g. the left and right arrow keys, or "C-f" and "C-b").
;; Clicking with the mouse on any node in the visualizer will take you
;; directly to that node, resetting the state of the parent buffer to the
;; state represented by that node.
;;
;; It can be useful to see how long ago the parent buffer was in the state
;; represented by a particular node in the visualizer. Hitting "t" in the
;; visualizer toggles the display of time-stamps for all the nodes. (Note
;; that, because of the way `undo-tree-mode' works, these time-stamps may be
;; somewhat later than the true times, especially if it's been a long time
;; since you last undid any changes.)
;;
;; Finally, hitting "q" will quit the visualizer, leaving the parent buffer in
;; whatever state you were last on.
;;
;;
;;
;; Drawbacks
;; =========
;;
;; Emacs' undo system is deeply embedded in Emacs. In particular, garbage
;; collection treats the `buffer-undo-list' specially: references to markers
;; in `buffer-undo-list' don't count during the mark phase, and the sweep
;; phase removes undo entries for markers that have been garbage-collected.
;; This behaviour is implemented in C as part of the garbage collection code,
;; and it is difficult or impossible to emulate in Elisp.
;;
;; To avoid dead markers being resurrected in `undo-tree-mode', and to allow
;; them to be garbage-collected, `undo-tree-mode' doesn't record marker
;; adjustments. Markers are rarely explicitly created by users, so the impact
;; of this will primarily be through its effects on other features that make
;; use of markers. Since marker adjustments haven't always been restored by
;; undo, and even then it was buggy until recently (see Emacs bug#4803), it
;; seems likely that relatively little code relies heavily on correct marker
;; restoration.
;;
;; `undo-tree-mode' doesn't support "undo in region", i.e. selectively undoing
;; only the changes that affect the region. Support for this is planned for a
;; future version.



;;; Change Log:
;;
;; Version 0.1.6
;; * added `undo-tree-mode-lighter' customization option to allow the
;;   mode-line lighter to be changed
;; * bug-fix in `undo-tree-discard-node'
;; * added `undo-tree-save-state-to-register' and
;;   `undo-tree-restore-state-from-register' commands and keybindings for
;;   saving/restoring buffer states using registers
;;
;; Version 0.1.5
;; * modified `undo-tree-visualize' to mark the visualizer window as
;;   soft-dedicated and changed `undo-tree-visualizer-quit' to use
;;   `kill-buffer', so that visualizer window is deleted along with buffer if
;;   visualizer buffer was displayed in a new window, but not if it was
;;   displayed in an existing window.
;;
;; Version 0.1.4
;; * modified `undo-tree-undo' and `undo-tree-redo' to always replace
;;   redo/undo entries with new ones generated by `primitive-undo', as the new
;;   changesets will restore the point more reliably
;;
;; Version 0.1.3
;; * fixed `undo-tree-visualizer-quit' to remove `after-change-functions'
;;   hook there, rather than in `undo-tree-kill-visualizer'
;;
;; Version 0.1.2
;; * fixed keybindings
;; * renamed `undo-tree-visualizer-switch-previous-branch' and
;;   `undo-tree-visualizer-switch-next-branch' to
;;   `undo-tree-visualizer-switch-branch-left' and
;;   `undo-tree-visualizer-switch-branch-right'
;;
;; Version 0.1.1
;; * prevented `undo-tree-kill-visualizer' from killing visualizer when
;;   undoing/redoing from the visualizer, which completely broke the
;;   visualizer!
;; * changed one redo binding, so that at least one set of undo/redo bindings
;;   works in a terminal
;; * bound vertical scrolling commands in `undo-tree-visualizer-map', in case
;;   they aren't bound globally
;; * added missing :group argument to `defface's
;;
;; Version 0.1
;; * initial release



;;; Code:

(eval-when-compile (require 'cl))

;; `characterp' isn't defined in Emacs versions <= 22
(eval-and-compile
  (unless (fboundp 'characterp)
    (defmacro characterp (arg) `(char-valid-p ,arg))))


;;; =====================================================================
;;;              Global variables and customization options

(defvar buffer-undo-tree nil
  "Tree of undo entries in current buffer.")
(make-variable-buffer-local 'buffer-undo-tree)


(defgroup undo-tree nil
  "Tree undo/redo."
  :group 'undo)

(defcustom undo-tree-mode-lighter " Undo-Tree"
  "Lighter displayed in mode line
when `undo-tree-mode' is enabled."
  :group 'undo-tree
  :type 'string)

(defcustom undo-tree-visualizer-spacing 3
  "Horizontal spacing in undo-tree visualization.
Must be a postivie odd integer."
  :group 'undo-tree
  :type '(integer
          :match (lambda (w n) (and (integerp n) (> n 0) (= (mod n 2) 1)))))
(make-variable-buffer-local 'undo-tree-visualizer-spacing)

(defvar undo-tree-map nil
  "Keymap used in undo-tree-mode.")


(defface undo-tree-visualizer-default-face
  '((((class color)) :foreground "gray"))
  "*Face used to draw undo-tree in visualizer."
  :group 'undo-tree)

(defface undo-tree-visualizer-current-face
  '((((class color)) :foreground "red"))
  "*Face used to highlight current undo-tree node
in visualizer."
  :group 'undo-tree)

(defface undo-tree-visualizer-active-branch-face
  '((((class color) (background dark))
     (:foreground "white" :weight bold))
    (((class color) (background light))
     (:foreground "black" :weight bold)))
  "*Face used to highlight active undo-tree branch
in visualizer."
  :group 'undo-tree)

(defvar undo-tree-visualizer-map nil
  "Keymap used in undo-tree visualizer.")


(defvar undo-tree-visualizer-buffer nil
  "Parent buffer in visualizer.")
(make-variable-buffer-local 'undo-tree-visualizer-buffer)

(defvar undo-tree-visualizer-timestamps nil
  "Non-nil when visualizer is displaying time-stamps.")
(make-variable-buffer-local 'undo-tree-visualizer-timestamps)




;;; =================================================================
;;;                     Setup default keymaps

(unless undo-tree-map
  (setq undo-tree-map (make-sparse-keymap))
  ;; remap `undo' and `undo-only' to `undo-tree-undo'
  (define-key undo-tree-map [remap undo] 'undo-tree-undo)
  (define-key undo-tree-map [remap undo-only] 'undo-tree-undo)
  ;; bind standard undo bindings (since these match redo counterparts)
  (define-key undo-tree-map (kbd "C-/") 'undo-tree-undo)
  (define-key undo-tree-map "\C-_" 'undo-tree-undo)
  ;; redo doesn't exist normally, so define our own keybindings
  (define-key undo-tree-map (kbd "C-?") 'undo-tree-redo)
  (define-key undo-tree-map (kbd "M-_") 'undo-tree-redo)
  ;; just in case something has defined `redo'...
  (define-key undo-tree-map [remap redo] 'undo-tree-redo)
  ;; we use "C-x u" for the undo-tree visualizer
  (define-key undo-tree-map (kbd "\C-x u") 'undo-tree-visualize)
  ;; bind register commands
  (define-key undo-tree-map (kbd "C-x r u")
    'undo-tree-save-state-to-register)
  (define-key undo-tree-map (kbd "C-x r U")
    'undo-tree-restore-state-from-register))


(unless undo-tree-visualizer-map
  (setq undo-tree-visualizer-map (make-keymap))
  ;; vertical motion keys undo/redo
  (define-key undo-tree-visualizer-map [remap previous-line]
    'undo-tree-visualize-undo)
  (define-key undo-tree-visualizer-map [remap next-line]
    'undo-tree-visualize-redo)
  (define-key undo-tree-visualizer-map [up]
    'undo-tree-visualize-undo)
  (define-key undo-tree-visualizer-map "p"
    'undo-tree-visualize-undo)
  (define-key undo-tree-visualizer-map "\C-p"
    'undo-tree-visualize-undo)
  (define-key undo-tree-visualizer-map [down]
    'undo-tree-visualize-redo)
  (define-key undo-tree-visualizer-map "n"
    'undo-tree-visualize-redo)
  (define-key undo-tree-visualizer-map "\C-n"
    'undo-tree-visualize-redo)
  ;; horizontal motion keys switch branch
  (define-key undo-tree-visualizer-map [remap forward-char]
    'undo-tree-visualize-switch-branch-right)
  (define-key undo-tree-visualizer-map [remap backward-char]
    'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-map [right]
    'undo-tree-visualize-switch-branch-right)
  (define-key undo-tree-visualizer-map "f"
    'undo-tree-visualize-switch-branch-right)
  (define-key undo-tree-visualizer-map "\C-f"
    'undo-tree-visualize-switch-branch-right)
  (define-key undo-tree-visualizer-map [left]
    'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-map "b"
    'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-map "\C-b"
    'undo-tree-visualize-switch-branch-left)
  ;; mouse sets buffer state to node at click
  (define-key undo-tree-visualizer-map [mouse-1]
    'undo-tree-visualizer-set)
  ;; toggle timestamps
  (define-key undo-tree-visualizer-map "t"
    'undo-tree-visualizer-toggle-timestamps)
  ;; horizontal scrolling may be needed if tree is very wide
  (define-key undo-tree-visualizer-map ","
    'undo-tree-visualizer-scroll-left)
  (define-key undo-tree-visualizer-map "."
    'undo-tree-visualizer-scroll-right)
  (define-key undo-tree-visualizer-map "<"
    'undo-tree-visualizer-scroll-left)
  (define-key undo-tree-visualizer-map ">"
    'undo-tree-visualizer-scroll-right)
  ;; vertical scrolling may be needed if the tree is very tall
  (define-key undo-tree-visualizer-map [next] 'scroll-up)
  (define-key undo-tree-visualizer-map [prior] 'scroll-down)
  ;; quit visualizer
  (define-key undo-tree-visualizer-map "q"
    'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-map "\C-q"
    'undo-tree-visualizer-quit))




;;; =====================================================================
;;;                     Undo-tree data structure

(defstruct
  (undo-tree
   :named
   (:constructor nil)
   (:constructor make-undo-tree
                 (&aux
                  (root (make-undo-tree-node nil nil))
                  (current root)
                  (size 0)))
   (:copier nil))
  root current size)



(defstruct
  (undo-tree-node
   (:type vector)   ; create unnamed struct
   (:constructor nil)
   (:constructor make-undo-tree-node
                 (previous undo
                  &aux
                  (timestamp (current-time))
                  (branch 0)))
   (:constructor make-undo-tree-node-backwards
                 (next-node undo
                  &aux
                  (next (list next-node))
                  (timestamp (current-time))
                  (branch 0)))
   (:copier nil))
  previous next undo redo timestamp branch visualizer)


(defmacro undo-tree-node-p (n)
  (let ((len (length (make-undo-tree-node nil nil))))
    `(and (vectorp ,n) (= (length ,n) ,len))))



(defstruct
  (undo-tree-visualizer-data
   (:type vector)   ; create unnamed struct
   (:constructor nil)
   (:constructor make-undo-tree-visualizer-data)
   (:copier nil))
  lwidth cwidth rwidth marker)


(defmacro undo-tree-visualizer-data-p (v)
  (let ((len (length (make-undo-tree-visualizer-data))))
    `(and (vectorp ,v) (= (length ,v) ,len))))

(defmacro undo-tree-node-lwidth (node)
  `(when (vectorp (undo-tree-node-visualizer ,node))
     (undo-tree-visualizer-data-lwidth
      (undo-tree-node-visualizer ,node))))

(defmacro undo-tree-node-cwidth (node)
  `(when (vectorp (undo-tree-node-visualizer ,node))
     (undo-tree-visualizer-data-cwidth
      (undo-tree-node-visualizer ,node))))

(defmacro undo-tree-node-rwidth (node)
  `(when (vectorp (undo-tree-node-visualizer ,node))
     (undo-tree-visualizer-data-rwidth
      (undo-tree-node-visualizer ,node))))

(defmacro undo-tree-node-marker (node)
  `(when (vectorp (undo-tree-node-visualizer ,node))
     (undo-tree-visualizer-data-marker
      (undo-tree-node-visualizer ,node))))


(defsetf undo-tree-node-lwidth (node) (val)
  `(let ((v (undo-tree-node-visualizer ,node)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-visualizer ,node)
             (setq v (make-undo-tree-visualizer-data))))
     (setf (undo-tree-visualizer-data-lwidth v) ,val)))

(defsetf undo-tree-node-cwidth (node) (val)
  `(let ((v (undo-tree-node-visualizer ,node)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-visualizer ,node)
             (setq v (make-undo-tree-visualizer-data))))
     (setf (undo-tree-visualizer-data-cwidth v) ,val)))

(defsetf undo-tree-node-rwidth (node) (val)
  `(let ((v (undo-tree-node-visualizer ,node)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-visualizer ,node)
             (setq v (make-undo-tree-visualizer-data))))
     (setf (undo-tree-visualizer-data-rwidth v) ,val)))

(defsetf undo-tree-node-marker (node) (val)
  `(let ((v (undo-tree-node-visualizer ,node)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-visualizer ,node)
             (setq v (make-undo-tree-visualizer-data))))
     (setf (undo-tree-visualizer-data-marker v) ,val)))




;;; =====================================================================
;;;              Basic undo-tree data structure functions

(defun undo-tree-grow (undo)
  "Add an UNDO node to current branch of `buffer-undo-tree'."
  (let* ((current (undo-tree-current buffer-undo-tree))
         (new (make-undo-tree-node current undo)))
    (push new (undo-tree-node-next current))
    (setf (undo-tree-current buffer-undo-tree) new)))


(defun undo-tree-grow-backwards (node undo)
  "Add an UNDO node *above* undo-tree NODE, and return new node.
Note that this will overwrite NODE's \"previous\" link, so should
only be used on a detached NODE, never on nodes that are already
part of `buffer-undo-tree'."
  (let ((new (make-undo-tree-node-backwards node undo)))
    (setf (undo-tree-node-previous node) new)
    new))


(defun undo-tree-compute-widths (undo-tree)
  "Recursively compute widths for all UNDO-TREE's nodes."
  (let ((stack (list (undo-tree-root undo-tree)))
        res)
    (while stack
      ;; try to compute widths for node at top of stack
      (if (undo-tree-node-p
           (setq res (undo-tree-node-compute-widths (car stack))))
          ;; if computation fails, it returns a node whose widths still need
          ;; computing, which we push onto the stack
          (push res stack)
        ;; otherwise, store widths and remove it from stack
        (setf (undo-tree-node-lwidth (car stack)) (aref res 0)
              (undo-tree-node-cwidth (car stack)) (aref res 1)
              (undo-tree-node-rwidth (car stack)) (aref res 2))
        (pop stack)))))


(defun undo-tree-node-compute-widths (node)
  ;; Compute NODE's left-, centre-, and right-subtree widths. Returns widths
  ;; (in a vector) if successful. Otherwise, returns a node whose widths need
  ;; calculating before NODE's can be calculated.
  (let ((num-children (length (undo-tree-node-next node)))
        (lwidth 0) (cwidth 0) (rwidth 0)
        p w)
    (catch 'need-widths
      (cond
       ;; leaf nodes have 0 width
       ((= 0 num-children)
        (setf cwidth 1
              (undo-tree-node-lwidth node) 0
              (undo-tree-node-cwidth node) 1
              (undo-tree-node-rwidth node) 0))

       ;; odd number of children
       ((= (mod num-children 2) 1)
        (setq p (undo-tree-node-next node))
        ;; compute left-width
        (dotimes (i (/ num-children 2))
          (if (undo-tree-node-lwidth (car p))
              (incf lwidth (+ (undo-tree-node-lwidth (car p))
                              (undo-tree-node-cwidth (car p))
                              (undo-tree-node-rwidth (car p))))
            ;; if child's widths haven't been computed, return that child
            (throw 'need-widths (car p)))
          (setq p (cdr p)))
        (if (undo-tree-node-lwidth (car p))
            (incf lwidth (undo-tree-node-lwidth (car p)))
          (throw 'need-widths (car p)))
        ;; centre-width is inherited from middle child
        (setf cwidth (undo-tree-node-cwidth (car p)))
        ;; compute right-width
        (incf rwidth (undo-tree-node-rwidth (car p)))
        (setq p (cdr p))
        (dotimes (i (/ num-children 2))
          (if (undo-tree-node-lwidth (car p))
              (incf rwidth (+ (undo-tree-node-lwidth (car p))
                              (undo-tree-node-cwidth (car p))
                              (undo-tree-node-rwidth (car p))))
            (throw 'need-widths (car p)))
          (setq p (cdr p))))

       ;; even number of children
       (t
        (setq p (undo-tree-node-next node))
        ;; compute left-width
        (dotimes (i (/ num-children 2))
          (if (undo-tree-node-lwidth (car p))
              (incf lwidth (+ (undo-tree-node-lwidth (car p))
                              (undo-tree-node-cwidth (car p))
                              (undo-tree-node-rwidth (car p))))
            (throw 'need-widths (car p)))
          (setq p (cdr p)))
        ;; centre-width is 0 when number of children is even
        (setq cwidth 0)
        ;; compute right-width
        (dotimes (i (/ num-children 2))
          (if (undo-tree-node-lwidth (car p))
              (incf rwidth (+ (undo-tree-node-lwidth (car p))
                              (undo-tree-node-cwidth (car p))
                              (undo-tree-node-rwidth (car p))))
            (throw 'need-widths (car p)))
          (setq p (cdr p)))))

      ;; return left-, centre- and right-widths
      (vector lwidth cwidth rwidth))))


(defun undo-tree-clear-visualizer-data (undo-tree)
  ;; Clear visualizer data from UNDO-TREE.
  (let ((stack (list (undo-tree-root undo-tree)))
        node)
    (while stack
      (setq node (pop stack))
      (setf (undo-tree-node-visualizer node) nil)
      (dolist (n (undo-tree-node-next node))
        (push n stack)))))


(defun undo-tree-position (node list)
  "Find the first occurrence of NODE in LIST.
Return the index of the matching item, or nil of not found.
Comparison is done with 'eq."
  (let ((i 0))
    (catch 'found
      (while (progn
               (when (eq node (car list)) (throw 'found i))
               (incf i)
               (setq list (cdr list))))
      nil)))


(defmacro undo-tree-num-branches ()
  ;; Return number of branches at current undo tree node.
  '(length (undo-tree-node-next (undo-tree-current buffer-undo-tree))))


(defun undo-tree-oldest-leaf (node)
  ;; Return oldest leaf node below NODE.
  (while (undo-tree-node-next node)
    (setq node
          (car (sort (mapcar 'identity (undo-tree-node-next node))
                     (lambda (a b)
                       (time-less-p (undo-tree-node-timestamp a)
                                    (undo-tree-node-timestamp b)))))))
  node)


(defun undo-tree-discard-node (node)
  ;; Discard NODE from `buffer-undo-tree', and return next in line for
  ;; discarding.

  ;; don't discard current node
  (unless (eq node (undo-tree-current buffer-undo-tree))
    (decf (undo-tree-size buffer-undo-tree)
          (+ (undo-list-byte-size (undo-tree-node-undo node))
             (undo-list-byte-size (undo-tree-node-redo node))))

    ;; discarding root node...
    (if (eq node (undo-tree-root buffer-undo-tree))
        (cond
         ;; should always discard branches before root
         ((> (length (undo-tree-node-next node)) 1)
          (error "Trying to discard undo-tree root which still\
 has multiple branches"))
         ;; don't discard root if current node is only child
         ((eq (car (undo-tree-node-next node))
              (undo-tree-current buffer-undo-tree))
	  nil)
         (t
          ;; make child of root into new root
          (setf node (setf (undo-tree-root buffer-undo-tree)
                           (car (undo-tree-node-next node)))
                (undo-tree-node-undo node) nil
                (undo-tree-node-redo node) nil)
          ;; if new root has branches, or new root is current node, next node
          ;; to discard is oldest leaf, otherwise it's new root
          (if (or (> (length (undo-tree-node-next node)) 1)
                  (eq (car (undo-tree-node-next node))
                      (undo-tree-current buffer-undo-tree)))
              (undo-tree-oldest-leaf node)
            node)))

      ;; discarding leaf node...
      (let* ((parent (undo-tree-node-previous node))
             (current (nth (undo-tree-node-branch parent)
                           (undo-tree-node-next parent))))
        (setf (undo-tree-node-next parent)
              (delq node (undo-tree-node-next parent))
              (undo-tree-node-branch parent)
              (undo-tree-position current (undo-tree-node-next parent)))
        ;; if parent has branches, or parent is current node, next node to
        ;; discard is oldest lead, otherwise it's parent
        (if (or (eq parent (undo-tree-current buffer-undo-tree))
                (and (undo-tree-node-next parent)
                     (or (not (eq parent (undo-tree-root buffer-undo-tree)))
                         (> (length (undo-tree-node-next parent)) 1))))
            (undo-tree-oldest-leaf parent)
          parent)))))



(defun undo-tree-discard-history ()
  "Discard undo history until we're within memory usage limits
set by `undo-limit', `undo-strong-limit' and `undo-outer-limit'."

  (when (> (undo-tree-size buffer-undo-tree) undo-limit)
    ;; if there are no branches off root, first node to discard is root;
    ;; otherwise it's leaf node at botom of oldest branch
    (let ((node (if (> (length (undo-tree-node-next
                                (undo-tree-root buffer-undo-tree))) 1)
                    (undo-tree-oldest-leaf (undo-tree-root buffer-undo-tree))
                  (undo-tree-root buffer-undo-tree))))

      ;; discard nodes until memory use is within `undo-strong-limit'
      (while (and node
                  (> (undo-tree-size buffer-undo-tree) undo-strong-limit))
        (setq node (undo-tree-discard-node node)))

      ;; discard nodes until next node to discard would bring memory use
      ;; within `undo-limit'
      (while (and node
                  (> (- (undo-tree-size buffer-undo-tree)
                        (undo-list-byte-size (undo-tree-node-undo node))
                        (undo-list-byte-size (undo-tree-node-redo node)))
                     undo-limit))
        (setq node (undo-tree-discard-node node)))

      ;; if we're still over the `undo-outer-limit', discard entire history
      (when (> (undo-tree-size buffer-undo-tree) undo-outer-limit)
        ;; query first `undo-ask-before-discard' is set
        (if undo-ask-before-discard
            (when (yes-or-no-p
                   (format
                    "Buffer `%s' undo info is %d bytes long;  discard it? "
                    (buffer-name) (undo-tree-size buffer-undo-tree)))
              (setq buffer-undo-tree nil))
          ;; otherwise, discard and display warning
          (display-warning
           '(undo discard-info)
           (concat
            (format "Buffer `%s' undo info was %d bytes long.\n"
                    (buffer-name) (undo-tree-size buffer-undo-tree))
            "The undo info was discarded because it exceeded\
 `undo-outer-limit'.

This is normal if you executed a command that made a huge change
to the buffer. In that case, to prevent similar problems in the
future, set `undo-outer-limit' to a value that is large enough to
cover the maximum size of normal changes you expect a single
command to make, but not so large that it might exceed the
maximum memory allotted to Emacs.

If you did not execute any such command, the situation is
probably due to a bug and you should report it.

You can disable the popping up of this buffer by adding the entry
\(undo discard-info) to the user option `warning-suppress-types',
which is defined in the `warnings' library.\n")
           :warning)
          (setq buffer-undo-tree nil)))
      )))




;;; =====================================================================
;;;         Utility functions for handling `buffer-undo-list'

(defun undo-list-pop-changeset ()
  ;; Pop changeset from `buffer-undo-list'.
  ;; discard undo boundaries and marker adjustment entries at head of list
  (while (or (null (car buffer-undo-list))
	     (and (consp (car buffer-undo-list))
		  (markerp (caar buffer-undo-list))))
    (setq buffer-undo-list (cdr buffer-undo-list)))
  ;; pop elements up to next undo boundary
  (unless (eq (car buffer-undo-list) 'undo-tree-canary)
    (let* ((changeset (cons (pop buffer-undo-list) nil))
           (p changeset))
      (while (car buffer-undo-list)
        (setcdr p (cons (pop buffer-undo-list) nil))
	;; discard marker adjustment entries (see Commentary, above)
	(if (and (consp (cadr p)) (markerp (car (cadr p))))
	    (setcdr p nil)
	  (setq p (cdr p))))
      changeset)))


(defun undo-list-transfer-to-tree ()
  ;; Transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'.
  (if (null buffer-undo-list)
      (setq buffer-undo-list '(nil undo-tree-canary))
    (when (not (eq (cadr buffer-undo-list) 'undo-tree-canary))
      ;; create new node from first changeset in `buffer-undo-list', save old
      ;; `buffer-undo-tree' current node, and make new node the current node
      (let* ((node (make-undo-tree-node nil (undo-list-pop-changeset)))
             (splice (undo-tree-current buffer-undo-tree))
             (size (undo-list-byte-size (undo-tree-node-undo node))))
        (setf (undo-tree-current buffer-undo-tree) node)
        ;; grow tree fragment backwards using `buffer-undo-list' changesets
        (while (and buffer-undo-list
                    (not (eq (cadr buffer-undo-list) 'undo-tree-canary)))
          (setq node
                (undo-tree-grow-backwards node (undo-list-pop-changeset)))
          (incf size (undo-list-byte-size (undo-tree-node-undo node))))
        ;; if no undo history has been discarded from `buffer-undo-list' since
        ;; last transfer, splice new tree fragment onto end of old
        ;; `buffer-undo-tree' current node
        (if (eq (cadr buffer-undo-list) 'undo-tree-canary)
            (progn
              (setf (undo-tree-node-previous node) splice)
              (push node (undo-tree-node-next splice))
              (setf (undo-tree-node-branch splice) 0)
              (incf (undo-tree-size buffer-undo-tree) size))
          ;; if undo history has been discarded, replace entire
          ;; `buffer-undo-tree' with new tree fragment
          (setq node (undo-tree-grow-backwards node nil))
          (setf (undo-tree-root buffer-undo-tree) node)
          (setq buffer-undo-list '(nil undo-tree-canary))
          (setf (undo-tree-size buffer-undo-tree) size)))
      ;; discard undo history if necessary
      (undo-tree-discard-history))))


(defun undo-list-byte-size (undo-list)
  ;; Return size (in bytes) of UNDO-LIST
  (let ((size 0) (p undo-list))
    (while p
      (incf size 8)  ; cons cells use up 8 bytes
      (when (and (consp (car p)) (stringp (caar p)))
        (incf size (string-bytes (caar p))))
      (setq p (cdr p)))
    size))




;;; =====================================================================
;;;                        Undo-tree commands

(define-minor-mode undo-tree-mode
  "Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-map}"

  nil                       ; init value
  undo-tree-mode-lighter    ; lighter
  undo-tree-map             ; keymap
  ;; if disabling `undo-tree-mode', remove "canary" from `buffer-undo-list'
  (unless undo-tree-mode (setq buffer-undo-list nil)))


(defun turn-on-undo-tree-mode ()
  "Enable undo-tree-mode."
  (undo-tree-mode 1))


(define-globalized-minor-mode global-undo-tree-mode
  undo-tree-mode turn-on-undo-tree-mode)



(defun undo-tree-undo (&optional arg)
  "Undo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t) (error "No undo information in this buffer"))

  (let ((undo-in-progress t))
    ;; if `buffer-undo-tree' is empty, create initial undo-tree
    (when (null buffer-undo-tree)
      (setq buffer-undo-tree (make-undo-tree)))
    ;; transfer entries accumulated in `buffer-undo-list' to
    ;; `buffer-undo-tree'
    (undo-list-transfer-to-tree)

    (dotimes (i (or arg 1))
      ;; check if at top of undo tree
      (if (null (undo-tree-node-previous
                 (undo-tree-current buffer-undo-tree)))
          (error "No further undo information")
        ;; undo one record from undo tree
        (primitive-undo 1 (undo-copy-list
                           (undo-tree-node-undo
                            (undo-tree-current buffer-undo-tree))))
        ;; pop redo entries that `primitive-undo' has added to
        ;; `buffer-undo-list' and record them in current node's redo record,
	;; replacing existing entry if one already exists
        (when (undo-tree-node-redo (undo-tree-current buffer-undo-tree))
	  (decf (undo-tree-size buffer-undo-tree)
		(undo-list-byte-size
		 (undo-tree-node-redo (undo-tree-current buffer-undo-tree)))))
	(setf (undo-tree-node-redo (undo-tree-current buffer-undo-tree))
	      (undo-list-pop-changeset))
	(incf (undo-tree-size buffer-undo-tree)
	      (undo-list-byte-size
	       (undo-tree-node-redo (undo-tree-current buffer-undo-tree))))
        ;; rewind current node
        (setf (undo-tree-current buffer-undo-tree)
              (undo-tree-node-previous (undo-tree-current buffer-undo-tree)))
        ;; update timestamp
        (setf (undo-tree-node-timestamp (undo-tree-current buffer-undo-tree))
              (current-time))))

    ;; inform user if at branch point
    (when (> (undo-tree-num-branches) 1) (message "Undo branch point!"))))



(defun undo-tree-redo (&optional arg)
  "Redo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t) (error "No undo information in this buffer"))

  (let ((undo-in-progress t))
    ;; if `buffer-undo-tree' is empty, create initial undo-tree
    (when (null buffer-undo-tree)
      (setq buffer-undo-tree (make-undo-tree)))
    ;; transfer entries accumulated in `buffer-undo-list' to
    ;; `buffer-undo-tree'
    (undo-list-transfer-to-tree)

    (let ((current (undo-tree-current buffer-undo-tree)))
      (dotimes (i (or arg 1))
        ;; check if at bottom of undo tree
        (if (null (undo-tree-node-next current))
            (error "No further redo information")
          ;; advance current node
          (setq current
                (setf (undo-tree-current buffer-undo-tree)
                      (nth (undo-tree-node-branch current)
                           (undo-tree-node-next current))))
          ;; update timestamp
          (setf (undo-tree-node-timestamp current) (current-time))
          ;; redo one record from undo tree
          (primitive-undo 1 (undo-copy-list (undo-tree-node-redo current)))
	  ;; pop undo entries that `primitive-undo' has added to
	  ;; `buffer-undo-list' and record them in current node's undo record,
	  ;; replacing existing entry if one already exists
        (when (undo-tree-node-undo current)
	  (decf (undo-tree-size buffer-undo-tree)
		(undo-list-byte-size (undo-tree-node-undo current))))
	(setf (undo-tree-node-undo current) (undo-list-pop-changeset))
	(incf (undo-tree-size buffer-undo-tree)
	      (undo-list-byte-size (undo-tree-node-undo current))))))

    ;; inform user if at branch point
    (when (> (undo-tree-num-branches) 1) (message "Undo branch point!"))))



(defun undo-tree-switch-branch (branch)
  "Switch to a different BRANCH of the undo tree.
This will affect which branch to descend when *redoing* changes
using `undo-tree-redo'."
  (interactive (list (or (and prefix-arg (prefix-numeric-value prefix-arg))
                         (and (not (eq buffer-undo-list t))
                              (or buffer-undo-tree
                                  (progn
                                    (setq buffer-undo-tree (make-undo-tree))
                                    (undo-list-transfer-to-tree)
                                    t))
                              (> (undo-tree-num-branches) 1)
                              (read-number
                               (format "Branch (0-%d): "
                                       (1- (undo-tree-num-branches))))))))
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t) (error "No undo information in this buffer"))
  ;; sanity check branch number
  (when (<= (undo-tree-num-branches) 1) (error "Not at undo branch point"))
  (when (or (< branch 0) (> branch (1- (undo-tree-num-branches))))
    (error "Invalid branch number")

    ;; if `buffer-undo-tree' is empty, create initial undo-tree
    (when (null buffer-undo-tree)
      (setq buffer-undo-tree (make-undo-tree)))
    ;; transfer entries accumulated in `buffer-undo-list' to
    ;; `buffer-undo-tree'
    (undo-list-transfer-to-tree)
    ;; switch branch
    (setf (undo-tree-node-branch (undo-tree-current buffer-undo-tree))
          branch)))


(defun undo-tree-set (node)
  ;; Set buffer to state corresponding to NODE. Returns intersection point
  ;; between path back from current node and path back from selected NODE.
  (let ((path (make-hash-table :test 'eq))
        (n node))
    (puthash (undo-tree-root buffer-undo-tree) t path)
    ;; build list of nodes leading back from selected node to root, updating
    ;; branches as we go to point down to selected node
    (while (progn
             (puthash n t path)
             (when (undo-tree-node-previous n)
               (setf (undo-tree-node-branch (undo-tree-node-previous n))
                     (undo-tree-position
                      n (undo-tree-node-next (undo-tree-node-previous n))))
               (setq n (undo-tree-node-previous n)))))
    ;; work backwards from current node until we intersect path back from
    ;; selected node
    (setq n (undo-tree-current buffer-undo-tree))
    (while (not (gethash n path))
      (setq n (undo-tree-node-previous n)))
    ;; ascend tree until intersection node
    (while (not (eq (undo-tree-current buffer-undo-tree) n))
      (undo-tree-undo))
    ;; descend tree until selected node
    (while (not (eq (undo-tree-current buffer-undo-tree) node))
      (undo-tree-redo))
    n))  ; return intersection node



(defun undo-tree-save-state-to-register (register)
  "Store current undo-tree state to REGISTER.
The saved state can be restored using
`undo-tree-restore-state-from-register'.
Argument is a character, naming the register."
  (interactive "cUndo-tree state to register: ")
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t) (error "No undo information in this buffer"))
  ;; if `buffer-undo-tree' is empty, create initial undo-tree
  (when (null buffer-undo-tree)
    (setq buffer-undo-tree (make-undo-tree)))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
  (undo-list-transfer-to-tree)
  ;; save current node to REGISTER
  (set-register register (undo-tree-current buffer-undo-tree)))



(defun undo-tree-restore-state-from-register (register)
  "Restore undo-tree state from REGISTER.
The state must be saved using `undo-tree-save-state-to-register'.
Argument is a character, naming the register."
  (interactive "cRestore undo-tree state from register: ")
  ;; throw error if undo is disabled in buffer, or if register doesn't contain
  ;; an undo-tree node
  (let ((node (get-register register)))
    (cond
     ((eq buffer-undo-list t)
      (error "No undo information in this buffer"))
     ((not (undo-tree-node-p node))
      (error "Register doesn't contain undo-tree state")))
    ;; if `buffer-undo-tree' is empty, create initial undo-tree
    (when (null buffer-undo-tree)
      (setq buffer-undo-tree (make-undo-tree)))
    ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
    (undo-list-transfer-to-tree)
    ;; restore buffer state corresponding to saved node
    (undo-tree-set node)))




;;; =====================================================================
;;;                       Undo-tree visualizer

(defun undo-tree-visualize ()
  "Visualize the current buffer's undo tree."
  (interactive)
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t) (error "No undo information in this buffer"))
  ;; if `buffer-undo-tree' is empty, create initial undo-tree
  (when (null buffer-undo-tree)
    (setq buffer-undo-tree (make-undo-tree)))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
  (undo-list-transfer-to-tree)
  ;; add hook to kill visualizer buffer if original buffer is changed
  (add-hook 'before-change-functions 'undo-tree-kill-visualizer nil t)
  ;; prepare *undo-tree* buffer, then draw tree in it
  (let ((undo-tree buffer-undo-tree)
        (buff (current-buffer))
	(display-buffer-mark-dedicated 'soft))
    (switch-to-buffer-other-window " *undo-tree*")
    (undo-tree-visualizer-mode)
    (setq undo-tree-visualizer-buffer buff)
    (setq buffer-undo-tree undo-tree)
    (setq cursor-type nil)
    (setq buffer-read-only nil)
    (undo-tree-draw-tree undo-tree)
    (setq buffer-read-only t)))


(defun undo-tree-kill-visualizer (&rest dummy)
  ;; Kill visualizer. Added to `before-change-functions' hook of original
  ;; buffer when visualizer is invoked.
  (unless undo-in-progress
    (unwind-protect
	(with-current-buffer " *undo-tree*"
	  (undo-tree-visualizer-quit)))))



(defun undo-tree-draw-tree (undo-tree)
  ;; Draw UNDO-TREE in current buffer.
  (erase-buffer)
  (undo-tree-move-down 1)      ; top margin
  (undo-tree-clear-visualizer-data undo-tree)
  (undo-tree-compute-widths undo-tree)
  (undo-tree-move-forward
   (max (/ (window-width) 2)
        (+ (undo-tree-node-char-lwidth (undo-tree-root undo-tree))
           ;; add space for left part of left-most time-stamp
           (if undo-tree-visualizer-timestamps 4 0)
           2)))                ; left margin
  ;; draw undo-tree
  (let ((undo-tree-insert-face 'undo-tree-visualizer-default-face)
        (stack (list (undo-tree-root undo-tree)))
        (n (undo-tree-root undo-tree)))
    ;; link root node to its representation in visualizer
    (unless (markerp (undo-tree-node-marker n))
      (setf (undo-tree-node-marker n) (make-marker))
      (set-marker-insertion-type (undo-tree-node-marker n) nil))
    (move-marker (undo-tree-node-marker n) (point))
    ;; draw nodes from stack until stack is empty
    (while stack
      (setq n (pop stack))
      (goto-char (undo-tree-node-marker n))
      (setq n (undo-tree-draw-subtree n nil))
      (setq stack (append stack n))))
  ;; highlight active branch
  (goto-char (undo-tree-node-marker (undo-tree-root undo-tree)))
  (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face))
    (undo-tree-highlight-active-branch (undo-tree-root undo-tree)))
  ;; highlight current node
  (let ((undo-tree-insert-face 'undo-tree-visualizer-current-face))
    (undo-tree-draw-node (undo-tree-current undo-tree) 'current)))


(defun undo-tree-highlight-active-branch (node)
  ;; Draw highlighted active branch below NODE in current buffer.
  (let ((stack (list node)))
    ;; link node to its representation in visualizer
    (unless (markerp (undo-tree-node-marker node))
      (setf (undo-tree-node-marker node) (make-marker))
      (set-marker-insertion-type (undo-tree-node-marker node) nil))
    (move-marker (undo-tree-node-marker node) (point))
    ;; draw active branch
    (while stack
      (setq node (pop stack))
      (goto-char (undo-tree-node-marker node))
      (setq node (undo-tree-draw-subtree node 'active))
      (setq stack (append stack node)))))


(defun undo-tree-draw-node (node &optional current)
  ;; Highlight NODE as current node.
  (goto-char (undo-tree-node-marker node))
  (if undo-tree-visualizer-timestamps
      (progn
        (backward-char 4)
        (if current (undo-tree-insert ?*) (undo-tree-insert ? ))
        (undo-tree-insert
         (undo-tree-timestamp-to-string (undo-tree-node-timestamp node)))
        (backward-char 5)
        (move-marker (undo-tree-node-marker node) (point))
        (put-text-property (- (point) 3) (+ (point) 5)
                           'undo-tree-node node))
    (if current (undo-tree-insert ?x) (undo-tree-insert ?o))
    (backward-char 1)
    (put-text-property (point) (1+ (point)) 'undo-tree-node node)))


(defun undo-tree-draw-subtree (node &optional active-branch)
  ;; Draw subtree rooted at NODE. The subtree will start from point.
  ;; If ACTIVE-BRANCH is non-nil, just draw active branch below NODE.
  ;; If TIMESTAP is non-nil, draw time-stamps instead of "o" at nodes.
  (let ((num-children (length (undo-tree-node-next node)))
        node-list pos trunk-pos n)
    ;; draw node itself
    (undo-tree-draw-node node)

    (cond
     ;; if we're at a leaf node, we're done
     ((= num-children 0))

     ;; if node has only one child, draw it (not strictly necessary to deal
     ;; with this case separately, but as it's by far the most common case
     ;; this makes the code clearer and more efficient)
     ((= num-children 1)
      (undo-tree-move-down 1)
      (undo-tree-insert ?|)
      (backward-char 1)
      (undo-tree-move-down 1)
      (undo-tree-insert ?|)
      (backward-char 1)
      (undo-tree-move-down 1)
      (setq n (car (undo-tree-node-next node)))
      ;; link next node to its representation in visualizer
      (unless (markerp (undo-tree-node-marker n))
        (setf (undo-tree-node-marker n) (make-marker))
        (set-marker-insertion-type (undo-tree-node-marker n) nil))
      (move-marker (undo-tree-node-marker n) (point))
      ;; add next node to list of nodes to draw next
      (push n node-list))

     ;; if node had multiple children, draw branches
     (t
      (undo-tree-move-down 1)
      (undo-tree-insert ?|)
      (backward-char 1)
      (setq trunk-pos (point))
      ;; left subtrees
      (backward-char
       (- (undo-tree-node-char-lwidth node)
          (undo-tree-node-char-lwidth
           (car (undo-tree-node-next node)))))
      (setq pos (point))
      (setq n (cons nil (undo-tree-node-next node)))
      (dotimes (i (/ num-children 2))
        (setq n (cdr n))
        (when (or (null active-branch)
                  (eq (car n)
                      (nth (undo-tree-node-branch node)
                           (undo-tree-node-next node))))
          (undo-tree-move-forward 2)
          (undo-tree-insert ?_ (- trunk-pos pos 2))
          (goto-char pos)
          (undo-tree-move-forward 1)
          (undo-tree-move-down 1)
          (undo-tree-insert ?/)
          (backward-char 2)
          (undo-tree-move-down 1)
          ;; link node to its representation in visualizer
          (unless (markerp (undo-tree-node-marker (car n)))
            (setf (undo-tree-node-marker (car n)) (make-marker))
            (set-marker-insertion-type (undo-tree-node-marker (car n)) nil))
          (move-marker (undo-tree-node-marker (car n)) (point))
          ;; add node to list of nodes to draw next
          (push (car n) node-list))
        (goto-char pos)
        (undo-tree-move-forward
         (+ (undo-tree-node-char-rwidth (car n))
            (undo-tree-node-char-lwidth (cadr n))
            undo-tree-visualizer-spacing 1))
        (setq pos (point)))
      ;; middle subtree (only when number of children is odd)
      (when (= (mod num-children 2) 1)
        (setq n (cdr n))
        (when (or (null active-branch)
                  (eq (car n)
                      (nth (undo-tree-node-branch node)
                           (undo-tree-node-next node))))
          (undo-tree-move-down 1)
          (undo-tree-insert ?|)
          (backward-char 1)
          (undo-tree-move-down 1)
          ;; link node to its representation in visualizer
          (unless (markerp (undo-tree-node-marker (car n)))
            (setf (undo-tree-node-marker (car n)) (make-marker))
            (set-marker-insertion-type (undo-tree-node-marker (car n)) nil))
          (move-marker (undo-tree-node-marker (car n)) (point))
          ;; add node to list of nodes to draw next
          (push (car n) node-list))
        (goto-char pos)
        (undo-tree-move-forward
         (+ (undo-tree-node-char-rwidth (car n))
            (if (cadr n) (undo-tree-node-char-lwidth (cadr n)) 0)
            undo-tree-visualizer-spacing 1))
        (setq pos (point)))
      ;; right subtrees
      (incf trunk-pos)
      (dotimes (i (/ num-children 2))
        (setq n (cdr n))
        (when (or (null active-branch)
                  (eq (car n)
                      (nth (undo-tree-node-branch node)
                           (undo-tree-node-next node))))
          (goto-char trunk-pos)
          (undo-tree-insert ?_ (- pos trunk-pos 1))
          (goto-char pos)
          (backward-char 1)
          (undo-tree-move-down 1)
          (undo-tree-insert ?\\)
          (undo-tree-move-down 1)
          ;; link node to its representation in visualizer
          (unless (markerp (undo-tree-node-marker (car n)))
            (setf (undo-tree-node-marker (car n)) (make-marker))
            (set-marker-insertion-type (undo-tree-node-marker (car n)) nil))
          (move-marker (undo-tree-node-marker (car n)) (point))
          ;; add node to list of nodes to draw next
          (push (car n) node-list))
        (when (cdr n)
          (goto-char pos)
          (undo-tree-move-forward
           (+ (undo-tree-node-char-rwidth (car n))
              (if (cadr n) (undo-tree-node-char-lwidth (cadr n)) 0)
              undo-tree-visualizer-spacing 1))
          (setq pos (point))))
      ))
    ;; return list of nodes to draw next
    (nreverse node-list)))



(defun undo-tree-node-char-lwidth (node)
  ;; Return left-width of NODE measured in characters.
  (if (= (length (undo-tree-node-next node)) 0) 0
    (- (* (+ undo-tree-visualizer-spacing 1) (undo-tree-node-lwidth node))
       (if (= (undo-tree-node-cwidth node) 0)
           (1+ (/ undo-tree-visualizer-spacing 2)) 0))))


(defun undo-tree-node-char-rwidth (node)
  ;; Return right-width of NODE measured in characters.
  (if (= (length (undo-tree-node-next node)) 0) 0
    (- (* (+ undo-tree-visualizer-spacing 1) (undo-tree-node-rwidth node))
       (if (= (undo-tree-node-cwidth node) 0)
           (1+ (/ undo-tree-visualizer-spacing 2)) 0))))


(defun undo-tree-insert (str &optional arg)
  ;; Insert character or string STR ARG times, overwriting, and using
  ;; `undo-tree-insert-face'.
  (unless arg (setq arg 1))
  (when (characterp str)
    (setq str (make-string arg str))
    (setq arg 1))
  (dotimes (i arg) (insert str))
  (setq arg (* arg (length str)))
  (undo-tree-move-forward arg)
  (backward-delete-char arg)
  (when (boundp 'undo-tree-insert-face)
    (put-text-property (- (point) arg) (point) 'face undo-tree-insert-face)))


(defun undo-tree-move-down (&optional arg)
  ;; Move down, extending buffer if necessary.
  (let ((row (line-number-at-pos))
        (col (current-column))
        line)
    (unless arg (setq arg 1))
    (forward-line arg)
    (setq line (line-number-at-pos))
    ;; if buffer doesn't have enough lines, add some
    (when (/= line (+ row arg))
      (insert (make-string (- arg (- line row)) ?\n)))
    (undo-tree-move-forward col)))


(defun undo-tree-move-forward (&optional arg)
  ;; Move forward, extending buffer if necessary.
  (unless arg (setq arg 1))
  (let ((n (- (line-end-position) (point))))
    (if (> n arg)
        (forward-char arg)
      (end-of-line)
      (insert (make-string (- arg n) ? )))))


(defun undo-tree-timestamp-to-string (timestamp)
  ;; Convert TIMESTAMP to hh:mm:ss string.
  (let ((time (decode-time timestamp)))
    (format "%02d:%02d:%02d" (nth 2 time) (nth 1 time) (nth 0 time))))




;;; =====================================================================
;;;                    Visualizer mode commands

(defun undo-tree-visualizer-mode ()
  "Major mode used in undo-tree visualizer.

The undo-tree visualizer can only be invoked from a buffer in
which `undo-tree-mode' is enabled. The visualizer displays the
undo history tree graphically, and allows you to browse around
the undo history, undoing or redoing the corresponding changes in
the parent buffer.

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-map}"
  (kill-all-local-variables)
  (setq major-mode 'undo-tree-visualizer-mode)
  (setq mode-name "undo-tree-visualizer-mode")
  (use-local-map undo-tree-visualizer-map)
  (setq truncate-lines t)
  (setq buffer-read-only t))



(defun undo-tree-visualize-undo (&optional arg)
  "Undo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  (setq buffer-read-only nil)
  (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face))
    (undo-tree-draw-node (undo-tree-current buffer-undo-tree)))
  (switch-to-buffer-other-window undo-tree-visualizer-buffer)
  (unwind-protect
      (undo-tree-undo arg)
    (switch-to-buffer-other-window " *undo-tree*")
    (let ((undo-tree-insert-face 'undo-tree-visualizer-current-face))
      (undo-tree-draw-node (undo-tree-current buffer-undo-tree) 'current))
    (setq buffer-read-only t)))


(defun undo-tree-visualize-redo (&optional arg)
  "Redo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  (setq buffer-read-only nil)
  (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face))
    (undo-tree-draw-node (undo-tree-current buffer-undo-tree)))
  (switch-to-buffer-other-window undo-tree-visualizer-buffer)
  (unwind-protect
      (undo-tree-redo arg)
    (switch-to-buffer-other-window " *undo-tree*")
    (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
    (let ((undo-tree-insert-face 'undo-tree-visualizer-current-face))
      (undo-tree-draw-node (undo-tree-current buffer-undo-tree) 'current))
    (setq buffer-read-only t)))


(defun undo-tree-visualize-switch-branch-right (arg)
  "Switch to next branch of the undo tree.
This will affect which branch to descend when *redoing* changes
using `undo-tree-redo' or `undo-tree-visualizer-redo'."
  (interactive "p")
  ;; un-highlight old active branch below current node
  (setq buffer-read-only nil)
  (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
  (let ((undo-tree-insert-face 'undo-tree-visualizer-default-face))
    (undo-tree-highlight-active-branch (undo-tree-current buffer-undo-tree)))
  ;; increment branch
  (let ((branch (undo-tree-node-branch (undo-tree-current buffer-undo-tree))))
  (setf (undo-tree-node-branch (undo-tree-current buffer-undo-tree))
        (cond
         ((>= (+ branch arg) (undo-tree-num-branches))
          (1- (undo-tree-num-branches)))
         ((<= (+ branch arg) 0) 0)
         (t (+ branch arg))))
  ;; highlight new active branch below current node
  (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
  (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face))
    (undo-tree-highlight-active-branch (undo-tree-current buffer-undo-tree)))
  ;; re-highlight current node
  (let ((undo-tree-insert-face 'undo-tree-visualizer-current-face))
    (undo-tree-draw-node (undo-tree-current buffer-undo-tree) 'current))
  (setq buffer-read-only t)))


(defun undo-tree-visualize-switch-branch-left (arg)
  "Switch to previous branch of the undo tree.
This will affect which branch to descend when *redoing* changes
using `undo-tree-redo' or `undo-tree-visualizer-redo'."
  (interactive "p")
  (undo-tree-visualize-switch-branch-right (- arg)))


(defun undo-tree-visualizer-quit ()
  "Quit the undo-tree visualizer."
  (interactive)
  (undo-tree-clear-visualizer-data buffer-undo-tree)
  ;; remove kill visualizer hook from parent buffer
  (with-current-buffer undo-tree-visualizer-buffer
    (remove-hook 'before-change-functions 'undo-tree-kill-visualizer t))
  (kill-buffer))


(defun undo-tree-visualizer-set (pos)
  "Set buffer to state corresponding to undo tree node
at POS."
  (interactive "@e")
  (setq pos (event-start (nth 1 pos)))
  (let ((node (get-text-property pos 'undo-tree-node)))
    (when node
      ;; set parent buffer to state corresponding to node at POS
      (set-buffer undo-tree-visualizer-buffer)
      (undo-tree-set node)
      (set-buffer " *undo-tree*")
      (setq buffer-read-only nil)
      ;; re-draw undo tree
      (undo-tree-draw-tree buffer-undo-tree)
      (setq buffer-read-only t))))


(defun undo-tree-visualizer-toggle-timestamps ()
  "Toggle display of time-stamps."
  (interactive)
  (setq undo-tree-visualizer-spacing
        (if (setq undo-tree-visualizer-timestamps
                  (not undo-tree-visualizer-timestamps))
            ;; need sufficient space if TIMESTAMP is set
            (max 9 (default-value 'undo-tree-visualizer-spacing))
          (default-value 'undo-tree-visualizer-spacing)))
  ;; redraw tree
  (setq buffer-read-only nil)
  (undo-tree-draw-tree buffer-undo-tree)
  (setq buffer-read-only t))


(defun undo-tree-visualizer-scroll-left (&optional arg)
  (interactive "p")
  (scroll-right (or arg 1) t))


(defun undo-tree-visualizer-scroll-right (&optional arg)
  (interactive "p")
  (scroll-left (or arg 1) t))



(provide 'undo-tree)

;;; undo-tree.el ends here
