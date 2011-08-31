;;; ecb-tod.el --- ECB tip of the day

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2003

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: ecb-tod.el,v 1.23 2009/04/21 15:23:22 berndl Exp $

;;; Commentary:
;;
;; Contains code for tips of the day

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-util)

(silentcomp-defvar ecb-tod-cursor)

(defcustom ecb-tip-of-the-day t
  "*Show tip of the day at start time of ECB."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-tip-of-the-day-file "~/.ecb-tip-of-day.el"
  "*File where tip-of-the-day cursor is stored."
  :group 'ecb-general
  :type 'file)

(defconst ecb-tod-tip-list
  '("You can expand the ECB-methods-buffer with `ecb-expand-methods-nodes' [C-c . x]."
    "You can toggle between different layouts with `ecb-toggle-layout' [C-c . t]."
    "You can go back to the most recent layout with [C-u] `ecb-toggle-layout' [C-u C-c . t]."
    "You can toggle displaying the ECB-windows with `ecb-toggle-ecb-windows' [C-c . w]."
    "You can show and hide the ECB-windows on a major-mode-basis with `ecb-major-modes-show-or-hide'."
    "You can maximize a certain ECB-window either via its popup-menu or with [C-x 1] in that window."
    "You can use speedbar instead of the native tree-buffers with option `ecb-use-speedbar-instead-native-tree-buffer'."
    "You can speedup access for big directories with option `ecb-cache-directory-contents'."
    "You can display the online help also in HTML-format with option `ecb-show-help-format'."
    "You can interactively create your own layouts with the command `ecb-create-new-layout'."
    "You can start the eshell in the compile-window simply with `eshell' or [C-c . e]."
    "Use the incremental search in the methods-buffer for fast node-selecting; see `ecb-tree-incremental-search'."
    "You can cycle through all currently opened \"compile-buffers\" with `ecb-cycle-through-compilation-buffers'."
    "You can change the window-sizes by dragging the mouse and storing the new sizes with `ecb-store-window-sizes'."
    "You can get a quick overlook of all built-in layouts with `ecb-show-layout-help'."
    "Browse your sources as with a web-browser with `ecb-nav-goto-next' \[C-c . n], `ecb-nav-goto-previous' \[C-c . p]."
    "Customize the look\&feel of the tree-buffers with `ecb-tree-expand-symbol-before' and `ecb-tree-indent'."
    "Customize the contents of the methods-buffer with `ecb-tag-display-function', `ecb-type-tag-display', `ecb-show-tags'."
    "Customize the main mouse-buttons of the tree-buffers with `ecb-primary-secondary-mouse-buttons'."
    "Customize with `ecb-tree-do-not-leave-window-after-select' for which tree-buffers a selection doesn't leave the window."
    "Grep a directory \(recursive) by using the popup-menu \(the right mouse-button) in the directories buffer."
    "Customize the sorting of the sources with the option `ecb-sources-sort-method'."
    "Narrow the source-buffer to the selected tag in the methods-buffer with `ecb-tag-visit-post-actions'."
    "Enable autom. enlarging of the compile-window by select with the option `ecb-compile-window-temporally-enlarge'."
    "Customize with `ecb-compile-window-temporally-enlarge' the situations the compile-window is allowed to enlarge."
    "Customize the meaning of `other-window' [C-x o] with the option `ecb-other-window-behavior'."
    "Customize height and width of the ECB-windows with `ecb-windows-height' and `ecb-windows-width'."
    "Define with `ecb-compilation-buffer-names' and `ecb-compilation-major-modes' which buffers are \"compile-buffers\"."
    "Customize all faces used by ECB with the customize-groups `ecb-face-options' and `ecb-faces'."
    "Auto-activate eshell with the option `ecb-eshell-auto-activate'."
    "Get best use of big screen-displays with leftright-layouts like \"leftright1\" or \"leftright2\"."
    "Use the POWER-click in the methods-buffer to narrow the clicked node in the edit-window."
    "Use the POWER-click in the sources- and history-buffer to get only an overlook of the source-contents."
    "Exclude not important sources from being displayed in the sources-buffer with `ecb-source-file-regexps'."
    "Use left- and right-arrow for smart expanding/collapsing tree-buffer-nodes; see `ecb-tree-navigation-by-arrow'." ;;
    "Add personal key-bindings to the tree-buffers with `ecb-common-tree-buffer-after-create-hook'."
    "Add personal key-bindings to the directories-buffer with `ecb-directories-buffer-after-create-hook'."
    "Add personal key-bindings to the sources-buffer with `ecb-sources-buffer-after-create-hook'."
    "Add personal key-bindings to the methods-buffer with `ecb-methods-buffer-after-create-hook'."
    "Add personal key-bindings to the history-buffer with `ecb-history-buffer-after-create-hook'."
    "Pop up a menu with the right mouse-button and do senseful things in the tree-buffers."
    "Extend the builtin popup-menus to your needs - see `ecb-directories-menu-user-extension'."
    "Call `ecb-show-help' [C-c . o] with a prefix-argument [C-u] and choose the help-format."
    "You can change the prefix [C-c .] of all ECB-key-bindings quick and easy with `ecb-key-map'."
    "Send a problem-report to the ECB-mailing-list quick and easy with `ecb-submit-problem-report'."
    "Switch on/off auto. expanding of the ECB-methods-buffer with `ecb-auto-expand-directory-tree'."
    "You can quickly toggle auto. expanding of the ECB-methods-buffer with `ecb-toggle-auto-expand-tag-tree'."
    "Highlight current semantic-tag of the edit-buffer in the ECB-methods-buffer with `ecb-highlight-tag-with-point'."
    "Apply a filter to the sources-buffer either via `ecb-sources-filter' or via the popup-menu."
    "Apply a filter to the history-buffer either via `ecb-history-filter' or via the popup-menu."
    "Apply tag-filters (can be layered) to the methods-buffer either via `ecb-methods-filter' or via the popup-menu."
    "Use `scroll-all-mode' to scroll both edit-windows of ECB simultaneously - and no other windows are scrolled!"
    "You can toggle having a compile window with `ecb-toggle-compile-window' if `ecb-compile-window-height' is not nil."
    "Start ECB automatically after Emacs is started. Use option `ecb-auto-activate'"
    "Maximize a tree-buffer via modeline - ECB supports the standard-mechanism of (X)Emacs for deleting other windows."
    "Easy horizontal scrolling the tree-buffers with the mouse with [M-mouse-1] and [M-mouse-3]; see `ecb-tree-easy-hor-scroll'."
    "Expand and collapse very precisely the current node in a tree-buffer with commands in the popup-menu."
    "Let ECB display the version-control-state of your files in the tree-buffers. See `ecb-vc-enable-support'."
    "Work with remote paths (e.g. TRAMP-, ANGE-FTP-, or EFS-paths) as with local paths in `ecb-source-path'."
    "Exclude certain files from being displayed in the history-buffer. See `ecb-history-exclude-file-regexps'."
    "Get the most important options of ECB at a glance by viewing the customization group \"ecb-most-important\"."
    )
  "List of all available tips of the day.")




(defun ecb-show-tip-of-the-day ()
  "Show tip of the day if `ecb-tip-of-the-day' is not nil."
  (interactive)
  (when (or (interactive-p) ecb-tip-of-the-day)
    (ignore-errors (load-file ecb-tip-of-the-day-file))
    (let* ((cursor (if (boundp 'ecb-tod-cursor)
                       ecb-tod-cursor
                     0))
           (tip (or (ignore-errors (nth cursor ecb-tod-tip-list))
                    (nth 0 ecb-tod-tip-list))))
      ;; show tip
      (ecb-message-box tip "Tip of the day" "Close")
      ;; change cursor
      (ecb-tod-move-cursor cursor))))

(defun ecb-tod-move-cursor (cursor)
  (with-temp-file (expand-file-name ecb-tip-of-the-day-file)
    (erase-buffer)
    (insert (format "(defvar ecb-tod-cursor 0)\n(setq ecb-tod-cursor %d)"
                    (if (< (1+ cursor) (length ecb-tod-tip-list))
                        (1+ cursor)
                      0)))))

(silentcomp-provide 'ecb-tod)

;; ecb-tod.el ends here