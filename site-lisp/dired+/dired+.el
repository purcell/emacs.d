;;; dired+.el --- Extensions to Dired.
;;
;; Filename: dired+.el
;; Description: Extensions to Dired.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1999-2007, Drew Adams, all rights reserved.
;; Created: Fri Mar 19 15:58:58 1999
;; Version: 21.2
;; Last-Updated: Fri Jan 19 20:58:56 2007 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 1493
;; URL: http://www.emacswiki.org/cgi-bin/wiki/dired+.el
;; Keywords: unix, mouse, directories, diredp, dired
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `cl', `custom', `dired', `dired+', `dired-aux',
;;   `dired-x', `easymenu', `ediff-diff', `ediff-help', `ediff-init',
;;   `ediff-merg', `ediff-mult', `ediff-util', `ediff-wind',
;;   `fit-frame', `frame-cmds', `frame-fns', `info', `info+',
;;   `misc-fns', `mkhtml', `mkhtml-htmlize', `strings', `thingatpt',
;;   `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to Dired
;;
;;  This file extends functionalities provided by standard GNU Emacs
;;  files `dired.el', `dired-aux.el', and `dired-x.el'.
;;
;;  Key bindings changed.  Menus redefined.  `dired-mouse-3-menu'
;;  popup menu added.  New commands.
;;
;;  All new functions, variables, and faces defined here have the
;;  prefix `diredp-' (for Dired Plus) in their names.
;;
;;  Faces defined here:
;;
;;    `diredp-compressed-file-suffix', `diredp-date-time',
;;    `diredp-deletion', `diredp-deletion-file-name',
;;    `diredp-dir-heading', `diredp-dir-priv', `diredp-display-msg',
;;    `diredp-exec-priv', `diredp-executable-tag', `diredp-file-name',
;;    `diredp-file-suffix', `diredp-flag-mark',
;;    `diredp-flag-mark-line', `diredp-ignored-file-name',
;;    `diredp-link-priv', `diredp-no-priv', `diredp-other-priv',
;;    `diredp-rare-priv', `diredp-read-priv', `diredp-symlink',
;;    `diredp-write-priv'.
;;
;;  Commands defined here:
;;
;;    `diredp-byte-compile-this-file', `diredp-capitalize',
;;    `diredp-capitalize-this-file', `diredp-chgrp-this-file',
;;    `diredp-chmod-this-file', `diredp-chown-this-file',
;;    `diredp-compress-this-file', `diredp-copy-this-file',
;;    `diredp-delete-this-file', `diredp-downcase-this-file',
;;    `diredp-ediff', `diredp-find-a-file',
;;    `diredp-find-a-file-other-frame',
;;    `diredp-find-a-file-other-window',
;;    `diredp-find-file-other-frame',
;;    `diredp-find-file-reuse-dir-buffer',
;;    `diredp-flag-region-files-for-deletion',
;;    `diredp-hardlink-this-file', `diredp-load-this-file',
;;    `diredp-mark-region-files', `diredp-mark/unmark-extension',
;;    `diredp-mouse-3-menu', `diredp-mouse-backup-diff',
;;    `diredp-mouse-diff', `diredp-mouse-do-byte-compile',
;;    `diredp-mouse-do-chgrp', `diredp-mouse-do-chmod',
;;    `diredp-mouse-do-chown', `diredp-mouse-do-compress',
;;    `diredp-mouse-do-copy', `diredp-mouse-do-delete',
;;    `diredp-mouse-do-hardlink', `diredp-mouse-do-load',
;;    `diredp-mouse-do-print', `diredp-mouse-do-rename',
;;    `diredp-mouse-do-shell-command', `diredp-mouse-do-symlink',
;;    `diredp-mouse-downcase', `diredp-mouse-ediff',
;;    `diredp-mouse-find-file', `diredp-mouse-find-file-other-frame',
;;    `diredp-mouse-find-file-reuse-dir-buffer',
;;    `diredp-mouse-flag-file-deletion', `diredp-mouse-mark',
;;    `diredp-mouse-mark-region-files', `diredp-mouse-mark/unmark',
;;    `diredp-mouse-unmark', `diredp-mouse-upcase',
;;    `diredp-mouse-view-file', `diredp-omit-marked',
;;    `diredp-omit-unmarked', `diredp-print-this-file',
;;    `diredp-relsymlink-this-file', `diredp-rename-this-file',
;;    `diredp-shell-command-this-file', `diredp-symlink-this-file',
;;    `diredp-toggle-find-file-reuse-dir',
;;    `diredp-unmark-region-files', `diredp-upcase-this-file',
;;    `toggle-dired-find-file-reuse-dir'.
;;
;;  Non-interactive functions defined here:
;;
;;    `diredp-fewer-than-2-files-p', `diredp-find-a-file-read-args',
;;    `diredp-subst-find-alternate-for-find',
;;    `diredp-subst-find-for-find-alternate'.
;;
;;  Variables defined here:
;;
;;    `diredp-file-line-overlay', `diredp-font-lock-keywords-1',
;;    `diredp-menu-bar-immediate-menu', `diredp-menu-bar-mark-menu',
;;    `diredp-menu-bar-operate-menu', `diredp-menu-bar-regexp-menu',
;;    `diredp-menu-bar-subdir-menu'.
;;
;;
;;  ***** NOTE: The following functions defined in `dired.el' have
;;              been REDEFINED HERE:
;;
;;  `dired-do-delete' - Display message to warn that marked, not
;;                      flagged, files will be deleted.
;;
;;  `dired-do-flagged-delete' - Display message to warn that flagged,
;;                              not marked, files will be deleted.
;;
;;  `dired-insert-set-properties' - `mouse-face' on whole line.
;;  `dired-revert' - Resets `mode-line-process' to nil.
;;
;;
;;  ***** NOTE: The following functions defined in `dired-aux.el' have
;;              been REDEFINED HERE:
;;
;;  `dired-do-byte-compile', `dired-do-compress', `dired-do-load' -
;;     Redisplay only if at most one file is being treated.
;;
;;
;;  ***** NOTE: The following functions defined in `dired-x.el' have
;;              been REDEFINED HERE:
;;
;;  `dired-do-find-marked-files' - Doc string reflects the change (see
;;                                 below) to
;;                                 `dired-simultaneous-find-file'.
;;
;;  `dired-mark-sexp' - 1. Variable `s' -> `blks'.
;;                      2. Fixes to `uid' and `gid'.
;;  `dired-simultaneous-find-file' - Uses separate frames instead of
;;     windows if `pop-up-frames' is non-nil, or if prefix arg < 0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/09/03 dadams
;;     diredp-font-lock-keywords-1: Corrected file size and inode number.  Thx to Peter Barabas.
;; 2006/08/20 dadams
;;     Added: diredp-find-a-file*.
;; 2006/06/18 dadams
;;     diredp-font-lock-keywords-1: Highlight file name (also) of flagged files.
;;                                  Use dired-del-marker instead of literal D.
;;     Added: diredp-deletion-file-name.
;; 2006/03/31 dadams
;;     No longer use display-in-minibuffer.
;; 2006/01/07 dadams
;;     Added: link for sending bug report.
;; 2006/01/06 dadams
;;     Added defgroup Dired-Plus and used it. Added :link.
;; 2006/01/04 dadams
;;     Added defvar of directory-listing-before-filename-regexp, for Emacs 22 compatibility.
;; 2005/12/29 dadams
;;     Added: diredp-mouse-mark/unmark-mark-region-files.
;; 2005/12/26 dadams
;;     Updated groups.
;; 2005/12/05 dadams
;;     diredp-ignored-file-name: Made it slightly darker.
;; 2005/11/05 dadams
;;     Renamed all stuff defined here to have diredp- prefix.
;;     diredp-relsymlink-this-file: Protected with fboundp.
;;     Changed to soft require: dired-x.el.
;;     Removed comment to require this inside eval-after-load.
;; 2005/11/03 dadams
;;     Added: dired-display-msg.  Replace blue-foreground-face with it.
;;     Alias dired-do-toggle to dired-toggle-marks, if defined.
;; 2005/11/02 dadams
;;     Added: dired-get-file-for-visit, dired(-mouse)-find-alternate-file*, 
;;            toggle-dired-find-file-reuse-dir, dired+-subst-find-*.
;;     Use defface for all faces.  Renamed without "-face".  No longer require def-face-const.
;;     dired-simultaneous-find-file: Minor bug fix (typo).
;; 2005/07/10 dadams
;;     dired-unmark-all-files-no-query -> dired-unmark-all-marks
;;       (thanks to Sivaram Neelakantan for bug report).
;; 2005/05/25 dadams
;;     string-to-int -> string-to-number everywhere.
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2005/02/16 dadams
;;     Added dired-mark/unmark-extension. Replaced dired-mark-extension with it everywhere.
;; 2005/01/08 dadams
;;     Bind [S-mouse-1], instead of [S-down-mouse-1], to dired-mouse-mark-region-files.
;; 2004/11/20 dadams
;;     dired-mark-sexp: Search for literal month names only for versions before Emacs 20.
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;; 2004/11/14 dadams
;;     Bound dired-no-confirm to non-nil for dired-mouse-*.
;;     Updated for Emacs 21 and improved highlighting:
;;       Spaces OK in file and directory names. Highlight date/time and size.
;; 2004/10/17 dadams
;;     Require cl only for Emacs 20, and only when compile.
;; 2004/10/01 dadams
;;     Updated to work with Emacs 21 also.
;; 2004/04/02 dadams
;;     dired-font-lock-keywords-1: Prefer using dired-omit-extensions
;;     to completion-ignored-extensions, if available.
;; 2004/03/22 dadams
;;     Added dired-mouse-mark-region-files and dired-mouse-mark/unmark.
;; 2000/09/27 dadams
;;     1. dired-font-lock-keywords-1: fixed for spaces in dir names.
;;     2. Added: dired-buffers-for-dir.
;; 1999/09/06 dadams
;;     Added S-*-mouse-2 bindings (same as C-*-mouse-2).
;; 1999/08/26 dadams
;;     1. Added *-face vars and dired-font-lock-keywords-1.
;;     2. Added possibility to use dired-font-lock-keywords-1 via hook.
;; 1999/08/26 dadams
;;     Changed key binding of dired-mouse-find-file from down-mouse-2 to mouse-2.
;; 1999/08/25 dadams
;;     Changed (C-)(M-)mouse-2 bindings.
;; 1999/08/25 dadams
;;     1. Added cmds & menu bar and key bindings: (dired-)find-file-other-frame.
;;     2. Changed binding for dired-display-file.
;; 1999/03/26 dadams
;;     1. Get rid of Edit menu-bar menu.
;;     2. dired-mouse-3-menu: Changed popup titles and item names.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(and (< emacs-major-version 21)
     (eval-when-compile (require 'cl))) ;; pop (plus, for Emacs <20: when, unless)

(require 'dired) ;; dired-revert
(require 'dired-aux) ;; dired-bunch-files, dired-do-chxxx, dired-do-create-files,
                     ;; dired-map-over-marks-check, dired-mark-read-string,
                     ;; dired-read-shell-command, dired-run-shell-command, dired-shell-stuff-it
(require 'ediff-util) ;; ediff-read-file-name

(require 'dired-x nil t) ;; (no error if not found) dired-do-relsymlink
(require 'misc-fns nil t) ;; (no error if not found): undefine-killer-commands
(when (< emacs-major-version 21)
  (require 'mkhtml nil t)) ;; (no error if not found): mkhtml-dired-files

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dired+)
(require 'dired+)                       ; Ensure loaded before compile this.

;;;;;;;;;;;;;;;;;;;;;;;



;; `dired-do-toggle' was renamed to `dired-toggle-marks' after Emacs 20.
(when (fboundp 'dired-toggle-marks) (defalias 'dired-do-toggle 'dired-toggle-marks))

;;; This is duplicated in `diff.el' and `vc.el'.
(defcustom diff-switches "-c"
  "*A string or list of strings specifying switches to be passed to diff."
  :type '(choice string (repeat string))
  :group 'dired :group 'diff)


;; This is also defined in `menu-bar+.el'.
;; Note: COMMAND must be a command (commandp); it cannot be an expression.
(defmacro menu-item-any-version (item-string command &rest keywords)
  "Return valid menu-item spec, whether Emacs 20 or more recent.
ITEM-STRING and COMMAND are as for `menu-item'.
KEYWORDS are used only for versions more recent than Emacs 20."
  (if (or (< emacs-major-version 21) (null keywords))
      `(cons ,item-string ',command)
    `'(menu-item ,item-string ,command ,@keywords)))

;;; This is needed in Emacs versions before Emacs 22
(defvar directory-listing-before-filename-regexp dired-move-to-filename-regexp
  "")

;;;-----------------------------------------------------------------
;;; Key Bindings.


;;; Menu Bar.
;;; New order is (left -> right):
;;;
;;;     Dir  Regexp  Mark  Multiple  Single

;; Get rid of menu bar predefined in `dired.el'.
(define-key dired-mode-map [menu-bar] nil)
;; Get rid of Edit menu bar menu to save space.
(define-key dired-mode-map [menu-bar edit] 'undefined)


;; "Single" menu.
;;
;; REPLACES ORIGINAL "Immediate" menu in `dired.el'.
;;;###autoload
(defvar diredp-menu-bar-immediate-menu (make-sparse-keymap "Single"))
(define-key dired-mode-map [menu-bar immediate]
  (cons "Single" diredp-menu-bar-immediate-menu))
(define-key diredp-menu-bar-immediate-menu [chown]
  (menu-item-any-version "Change Owner..." diredp-chown-this-file
                         :visible (not (memq system-type '(ms-dos windows-nt)))
                         :help "Change the owner of file at cursor"))
(define-key diredp-menu-bar-immediate-menu [chgrp]
  (menu-item-any-version "Change Group..." diredp-chgrp-this-file
                         :visible (not (memq system-type '(ms-dos windows-nt)))
                         :help "Change the group of file at cursor"))
(define-key diredp-menu-bar-immediate-menu [chmod]
  (menu-item-any-version "Change Mode..." diredp-chmod-this-file
                         :help "Change mode (attributes) of file at cursor"))
(define-key diredp-menu-bar-immediate-menu [separator-ch] '("--"))
(define-key diredp-menu-bar-immediate-menu [load]
  (menu-item-any-version "Load" diredp-load-this-file
                         :help "Load this Emacs Lisp file"))
(define-key diredp-menu-bar-immediate-menu [compile]
  (menu-item-any-version "Byte Compile" diredp-byte-compile-this-file
                         :help "Byte-compile this Emacs Lisp file"))
(define-key diredp-menu-bar-immediate-menu [command]
  (menu-item-any-version "Shell Command..." diredp-shell-command-this-file
                         :help "Run a shell command on file at cursor"))
(define-key diredp-menu-bar-immediate-menu [compress]
  (menu-item-any-version "Compress/Decompress" diredp-compress-this-file
                         :help "Compress/uncompress file at cursor"))
(define-key diredp-menu-bar-immediate-menu [print]
  (menu-item-any-version "Print..." diredp-print-this-file
                         :help "Print file at cursor, supplying print command"))
(when (fboundp 'mkhtml-dired-files)
  (define-key diredp-menu-bar-immediate-menu [mkhtml-dired-files]
    (menu-item-any-version "Create HTML" mkhtml-dired-files
                           :help "Create an HTML file corresponding to file at cursor")))
(define-key diredp-menu-bar-immediate-menu [separator-misc] '("--"))
(define-key diredp-menu-bar-immediate-menu [hardlink]
  (menu-item-any-version "Hardlink to..." diredp-hardlink-this-file
                         :help "Make hard links for current or marked files"))
(if (not (fboundp 'diredp-relsymlink-this-file))
    (define-key diredp-menu-bar-immediate-menu [symlink]
      (menu-item-any-version "Symlink to..." diredp-symlink-this-file
                             :visible (fboundp 'make-symbolic-link)
                             :help "Make symbolic link for file at cursor"))
  (define-key diredp-menu-bar-immediate-menu [symlink]
    (menu-item-any-version
     "Symlink to (Absolute)..." diredp-symlink-this-file
     :help "Make absolute symbolic link for file at cursor"))
  (define-key diredp-menu-bar-immediate-menu [relsymlink]
    (menu-item-any-version
     "Symlink to (Relative)..." diredp-relsymlink-this-file ; In `dired-x.el'.
     :help "Make relative symbolic link for file at cursor")))
(define-key diredp-menu-bar-immediate-menu [separator-link] '("--"))
(define-key diredp-menu-bar-immediate-menu [delete]
  (menu-item-any-version "Delete" diredp-delete-this-file :help "Delete file at cursor"))
(define-key diredp-menu-bar-immediate-menu [capitalize]
  (menu-item-any-version "Capitalize" diredp-capitalize-this-file
                         :help "Capitalize (initial caps) name of file at cursor"))
(define-key diredp-menu-bar-immediate-menu [downcase]
  (menu-item-any-version "Downcase" diredp-downcase-this-file
                         ;; When running on plain MS-DOS, there's only one
                         ;; letter-case for file names.
                         :enable (or (not (fboundp 'msdos-long-file-names))
                                     (msdos-long-file-names))
                         :help "Rename file at cursor to a lower-case name"))
(define-key diredp-menu-bar-immediate-menu [upcase]
  (menu-item-any-version "Upcase" diredp-upcase-this-file
                         :enable (or (not (fboundp 'msdos-long-file-names))
                                     (msdos-long-file-names))
                         :help "Rename file at cursor to an upper-case name"))
(define-key diredp-menu-bar-immediate-menu [rename]
  (menu-item-any-version "Rename to..." diredp-rename-this-file
                         :help "Rename file at cursor"))
(define-key diredp-menu-bar-immediate-menu [copy]
  (menu-item-any-version "Copy to..." diredp-copy-this-file :help "Copy file at cursor"))
(define-key diredp-menu-bar-immediate-menu [separator-chg] '("--"))
(define-key diredp-menu-bar-immediate-menu [backup-diff]
  (menu-item-any-version "Diff with Backup" dired-backup-diff
                         :help "Diff file at cursor with its latest backup"))
(define-key diredp-menu-bar-immediate-menu [diff]
  (menu-item-any-version "Diff..." dired-diff
                         :help "Compare file at cursor with another file using `diff'"))
(define-key diredp-menu-bar-immediate-menu [ediff]
  (menu-item-any-version "Compare..." diredp-ediff
                         :help "Compare file at cursor with another file"))
(define-key diredp-menu-bar-immediate-menu [separator-diff] '("--"))
(define-key diredp-menu-bar-immediate-menu [view]
  (menu-item-any-version "View (Read Only)" dired-view-file
                         :help "Examine file at cursor in read-only mode"))
(define-key diredp-menu-bar-immediate-menu [display]
  (menu-item-any-version "Display in Other Window" dired-display-file
                         :help "Display file at cursor in a different window"))
(define-key diredp-menu-bar-immediate-menu [find-file-other-frame]
  (menu-item-any-version "Open in Other Frame" diredp-find-file-other-frame
                         :help "Edit file at cursor in a different frame"))
(define-key diredp-menu-bar-immediate-menu [find-file-other-window]
  (menu-item-any-version "Open in Other Window" dired-find-file-other-window
                         :help "Edit file at cursor in a different window"))
(define-key diredp-menu-bar-immediate-menu [find-file]
  (menu-item-any-version "Open" dired-find-file :help "Edit file at cursor"))


;; "Multiple" menu.
;;
;; REPLACES ORIGINAL "Operate" menu in `dired.el'.
;;;###autoload
(defvar diredp-menu-bar-operate-menu (make-sparse-keymap "Multiple"))
(define-key dired-mode-map [menu-bar operate]
  (cons "Multiple" diredp-menu-bar-operate-menu))
(define-key diredp-menu-bar-operate-menu [chown]
  (menu-item-any-version "Change Owner..." dired-do-chown
                         :visible (not (memq system-type '(ms-dos windows-nt)))
                         :help "Change the owner of marked files"))
(define-key diredp-menu-bar-operate-menu [chgrp]
  (menu-item-any-version "Change Group..." dired-do-chgrp
                         :visible (not (memq system-type '(ms-dos windows-nt)))
                         :help "Change the owner of marked files"))
(define-key diredp-menu-bar-operate-menu [chmod]
  (menu-item-any-version "Change Mode..." dired-do-chmod
                         :help "Change mode (attributes) of marked files"))
;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
(when (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
  (define-key diredp-menu-bar-operate-menu [touch]
    '(menu-item "Change Timestamp..." dired-do-touch
                :help "Change timestamp of marked files")))
(define-key diredp-menu-bar-operate-menu [separator-ch] '("--"))
(define-key diredp-menu-bar-operate-menu [load]
  (menu-item-any-version "Load" dired-do-load :help "Load marked Emacs Lisp files"))
(define-key diredp-menu-bar-operate-menu [compile]
  (menu-item-any-version "Byte Compile" dired-do-byte-compile
                         :help "Byte-compile marked Emacs Lisp files"))
(define-key diredp-menu-bar-operate-menu [command]
  (menu-item-any-version "Shell Command..." dired-do-shell-command
                         :help "Run a shell command on each of marked files"))
(define-key diredp-menu-bar-operate-menu [compress]
  (menu-item-any-version "Compress/Uncompress" dired-do-compress
                         :help "Compress/uncompress marked files"))
(define-key diredp-menu-bar-operate-menu [query-replace]
  (if (< emacs-major-version 21)
      (menu-item-any-version "Query Replace..." dired-do-query-replace)
    (menu-item-any-version "Query Replace..." dired-do-query-replace-regexp
                           :help "Replace regexp in marked files")))
(define-key diredp-menu-bar-operate-menu [search]
  (menu-item-any-version "Search Files..." dired-do-search
                         :help "Search marked files for regexp"))
(define-key diredp-menu-bar-operate-menu [print]
  (menu-item-any-version "Print..." dired-do-print
                         :help "Print marked files, supplying print command"))
(when (fboundp 'mkhtml-dired-files)
  (define-key diredp-menu-bar-operate-menu [mkhtml-dired-files]
    (menu-item-any-version "Create HTML" mkhtml-dired-files
                           :help "Create HTML files corresponding to marked files")))
(define-key diredp-menu-bar-operate-menu [separator-link] '("--"))
(define-key diredp-menu-bar-operate-menu [hardlink]
  (menu-item-any-version "Hardlink to..." dired-do-hardlink
                         :help "Make hard links for current or marked files"))
(if (not (fboundp 'dired-do-relsymlink))
    (define-key diredp-menu-bar-operate-menu [symlink]
      (menu-item-any-version "Symlink to..." dired-do-symlink
                             :visible (fboundp 'make-symbolic-link)
                             :help "Make symbolic links for current or marked files"))
  (define-key diredp-menu-bar-operate-menu [symlink]
    (menu-item-any-version
     "Symlink to (Absolute)..." dired-do-symlink
     :help "Make absolute symbolic links for current or marked files"))
  (define-key diredp-menu-bar-operate-menu [relsymlink] ; In `dired-x.el'.
    (menu-item-any-version
     "Symlink to (Relative)..." dired-do-relsymlink
     :help "Make relative symbolic links for current or marked files")))
(define-key diredp-menu-bar-operate-menu [separator-move] '("--"))
(define-key diredp-menu-bar-operate-menu [delete-flagged]
  (menu-item-any-version "Delete Flagged" dired-do-flagged-delete
                         :help "Delete all files flagged for deletion (D)"))
(define-key diredp-menu-bar-operate-menu [delete]
  (menu-item-any-version
   "Delete Marked (not Flagged)" dired-do-delete
   :help "Delete current file or all marked files (not flagged files)"))
(define-key diredp-menu-bar-operate-menu [capitalize]
  (menu-item-any-version "Capitalize" diredp-capitalize
                         :help "Capitalize (initial caps) the names of all marked files"))
(define-key diredp-menu-bar-operate-menu [downcase]
  (menu-item-any-version "Downcase" dired-downcase
                         :enable (or (not (fboundp 'msdos-long-file-names))
                                     (msdos-long-file-names))
                         :help "Rename marked files to lowercase names"))
(define-key diredp-menu-bar-operate-menu [upcase]
  (menu-item-any-version "Upcase" dired-upcase
                         :enable (or (not (fboundp 'msdos-long-file-names))
                                     (msdos-long-file-names))
                         :help "Rename marked files to uppercase names"))
(define-key diredp-menu-bar-operate-menu [rename]
  (menu-item-any-version "Rename to..." dired-do-rename
                         :help "Rename current file or move marked files"))
(define-key diredp-menu-bar-operate-menu [copy]
  (menu-item-any-version "Copy to..." dired-do-copy
                         :help "Copy current file or all marked files"))
(define-key diredp-menu-bar-operate-menu [separator-misc] '("--"))
(when (fboundp 'dired-copy-filename-as-kill)
  (define-key diredp-menu-bar-operate-menu [kill-ring]
    (menu-item-any-version
     "Copy File Names (to Paste)" dired-copy-filename-as-kill
     :help "Copy names of marked files onto kill ring, for pasting")))
(when (fboundp 'dired-do-find-marked-files)
  (define-key diredp-menu-bar-operate-menu [find-files]
    (menu-item-any-version "Open" dired-do-find-marked-files ; In `dired-x.el'.
                           :help "Open each marked file for editing")))


;; "Regexp" menu.
;;
;; REPLACES ORIGINAL "Regexp" menu in `dired.el'.
;;;###autoload
(defvar diredp-menu-bar-regexp-menu (make-sparse-keymap "Regexp"))
(define-key dired-mode-map [menu-bar regexp]
  (cons "Regexp" diredp-menu-bar-regexp-menu))
(define-key diredp-menu-bar-regexp-menu [hardlink]
  (menu-item-any-version "Hardlink to..." dired-do-hardlink-regexp
                         :help "Make hard links for files matching regexp"))
(if (not (fboundp 'dired-do-relsymlink-regexp))
    (define-key diredp-menu-bar-regexp-menu [symlink]
      (menu-item-any-version "Symlink to..." dired-do-symlink-regexp
                             :visible (fboundp 'make-symbolic-link)
                             :help "Make symbolic links for files matching regexp"))
  (define-key diredp-menu-bar-regexp-menu [symlink]
    (menu-item-any-version
     "Symlink to (Absolute)..." dired-do-symlink-regexp
     :visible (fboundp 'make-symbolic-link)
     :help "Make absolute symbolic links for files matching regexp"))
  (define-key diredp-menu-bar-regexp-menu [relsymlink] ; In `dired-x.el'.
    (menu-item-any-version
     "Symlink to (Relative)..." dired-do-relsymlink-regexp
     :visible (fboundp 'make-symbolic-link)
     :help "Make relative symbolic links for files matching regexp")))
(define-key diredp-menu-bar-regexp-menu [rename]
  (menu-item-any-version "Rename to..." dired-do-rename-regexp
                         :help "Rename marked files matching regexp"))
(define-key diredp-menu-bar-regexp-menu [copy]
  (menu-item-any-version "Copy to..." dired-do-copy-regexp
                         :help "Copy marked files matching regexp"))
(define-key diredp-menu-bar-regexp-menu [flag]
  (menu-item-any-version "Flag..." dired-flag-files-regexp
                         :help "Flag files matching regexp for deletion"))
(define-key diredp-menu-bar-regexp-menu [mark]
  (menu-item-any-version "Mark..." dired-mark-files-regexp
                         :help "Mark files matching regexp for future operations"))
(define-key diredp-menu-bar-regexp-menu [mark-cont]
  (menu-item-any-version "Mark Containing..." dired-mark-files-containing-regexp
                         :help "Mark files whose contents matches regexp"))


;; "Mark" menu.
;;
;; REPLACES ORIGINAL "Mark" menu in `dired.el'.
;;;###autoload
(defvar diredp-menu-bar-mark-menu (make-sparse-keymap "Mark"))
(define-key dired-mode-map [menu-bar mark] (cons "Mark" diredp-menu-bar-mark-menu))

(when (fboundp 'dired-flag-extension)
  (define-key diredp-menu-bar-mark-menu [flag-extension] ; In `dired-x.el'
    (menu-item-any-version
     "Flag Extension..." dired-flag-extension
     :help "Flag all files that have a certain extension, for deletion")))
(define-key diredp-menu-bar-mark-menu [garbage-files]
  (menu-item-any-version "Flag Garbage Files" dired-flag-garbage-files
                         :help "Flag unneeded files for deletion"))
(define-key diredp-menu-bar-mark-menu [backup-files]
  (menu-item-any-version "Flag Backup Files" dired-flag-backup-files
                         :help "Flag all backup files for deletion"))
(define-key diredp-menu-bar-mark-menu [auto-save-files]
  (menu-item-any-version "Flag Auto-save Files" dired-flag-auto-save-files
                         :help "Flag auto-save files for deletion"))
(define-key diredp-menu-bar-mark-menu [flag-region]
  (menu-item-any-version "Flag Region" diredp-flag-region-files-for-deletion
                         :enable mark-active
                         :help "Flag all files in the region (selection) for deletion"))
(when (< emacs-major-version 21)
  (put 'diredp-flag-region-files-for-deletion 'menu-enable 'mark-active))
(define-key diredp-menu-bar-mark-menu [deletion]
  (menu-item-any-version "Flag" dired-flag-file-deletion
                         :help "Flag current line's file for deletion"))
(define-key diredp-menu-bar-mark-menu [separator-flag] '("--"))
(define-key diredp-menu-bar-mark-menu [prev]
  (menu-item-any-version "Previous Marked" dired-prev-marked-file
                         :help "Move to previous marked file"))
(define-key diredp-menu-bar-mark-menu [next]
  (menu-item-any-version "Next Marked" dired-next-marked-file
                         :help "Move to next marked file"))
(define-key diredp-menu-bar-mark-menu [marks]
  (menu-item-any-version "Change Marks..." dired-change-marks
                         :help "Replace marker with another character"))
(define-key diredp-menu-bar-mark-menu [revert]
  (menu-item-any-version "Refresh (Show All)" revert-buffer
                         :help "Update contents of shown directories"))
(define-key diredp-menu-bar-mark-menu [omit-unmarked]
  (menu-item-any-version "Omit Unmarked" diredp-omit-unmarked
                         :help "Hide lines of unmarked files"))
(define-key diredp-menu-bar-mark-menu [omit-marked]
  (menu-item-any-version "Omit Marked" diredp-omit-marked
                         :help "Hide lines of marked files"))
(define-key diredp-menu-bar-mark-menu [toggle-marks]
;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
  (if (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
      '(menu-item "Toggle Marked/Unmarked" dired-toggle-marks
                  :help "Mark unmarked files, unmark marked ones")
    '("Toggle Marked/Unmarked" . dired-do-toggle)))
(define-key diredp-menu-bar-mark-menu [separator-mark] '("--"))
(when (fboundp 'dired-mark-sexp)
  (define-key diredp-menu-bar-mark-menu [mark-sexp] ; In `dired-x.el'.
    (menu-item-any-version "Mark If..." dired-mark-sexp
                           :help "Mark files for which specified condition is true")))
(define-key diredp-menu-bar-mark-menu [mark-extension]
  (menu-item-any-version "Mark Extension..." diredp-mark/unmark-extension
                         :help "Mark all files with specified extension"))
(define-key diredp-menu-bar-mark-menu [symlinks]
  (menu-item-any-version "Mark Symlinks" dired-mark-symlinks
                         :visible (fboundp 'make-symbolic-link)
                         :help "Mark all symbolic links"))
(define-key diredp-menu-bar-mark-menu [directories]
  (menu-item-any-version "Mark Directories" dired-mark-directories
                         :help "Mark all directories except `.' and `..'"))
(define-key diredp-menu-bar-mark-menu [directory]
  (menu-item-any-version "Mark Old Backups" dired-clean-directory
                         :help "Flag old numbered backups for deletion"))
(define-key diredp-menu-bar-mark-menu [executables]
  (menu-item-any-version "Mark Executables" dired-mark-executables
                         :help "Mark all executable files"))
(define-key diredp-menu-bar-mark-menu [mark-region]
  (menu-item-any-version "Mark Region" diredp-mark-region-files
                         :enable mark-active
                         :help "Mark all of the files in the region (selection)"))
(when (< emacs-major-version 21)
  (put 'diredp-mark-region-files 'menu-enable 'mark-active))
(define-key diredp-menu-bar-mark-menu [mark]
  (menu-item-any-version "Mark" dired-mark
                         :help "Mark current line's file for future operations"))
(define-key diredp-menu-bar-mark-menu [separator-unmark] '("--"))
(define-key diredp-menu-bar-mark-menu [unmark-all]
  (menu-item-any-version "Unmark All" dired-unmark-all-marks
                         :help "Remove all marks from all files"))
(define-key diredp-menu-bar-mark-menu [unmark-with]
  (menu-item-any-version "Unmark Marked-With..." dired-unmark-all-files
                         :help "Remove a specific mark (or all marks) from every file"))
(define-key diredp-menu-bar-mark-menu [unmark-region]
  (menu-item-any-version "Unmark Region" diredp-unmark-region-files
                         :enable mark-active
                         :help "Unmark all files in the region (selection)"))
(when (< emacs-major-version 21)
  (put 'diredp-unmark-region-files 'menu-enable 'mark-active))
(define-key diredp-menu-bar-mark-menu [unmark]
  (menu-item-any-version "Unmark" dired-unmark
                         :help "Unmark or unflag current line's file"))


;; "Dir" menu.
;;
;; REPLACES ORIGINAL "Subdir" menu in `dired.el'.
;;;###autoload
(defvar diredp-menu-bar-subdir-menu (make-sparse-keymap "Dir"))
(define-key dired-mode-map [menu-bar subdir]
  (cons "Dir" diredp-menu-bar-subdir-menu))
(define-key diredp-menu-bar-subdir-menu [hide-all]
  (menu-item-any-version "Hide/Show All" dired-hide-all
                         :help "Hide all subdirectories, leave only header lines"))
(define-key diredp-menu-bar-subdir-menu [hide-subdir]
  (menu-item-any-version "Hide/Show Subdir" dired-hide-subdir
                         :help "Hide or unhide current directory listing"))
(define-key diredp-menu-bar-subdir-menu [tree-down]
  (menu-item-any-version "Tree Down" dired-tree-down
                         :help "Go to first subdirectory header down the tree"))
(define-key diredp-menu-bar-subdir-menu [tree-up]
  (menu-item-any-version "Tree Up" dired-tree-up
                         :help "Go to first subdirectory header up the tree"))
(define-key diredp-menu-bar-subdir-menu [prev-subdir]
  (menu-item-any-version "Prev Subdir" dired-prev-subdir
                         :help "Go to previous subdirectory header line"))
(define-key diredp-menu-bar-subdir-menu [next-subdir]
  (menu-item-any-version "Next Subdir" dired-next-subdir
                         :help "Go to next subdirectory header line"))
(define-key diredp-menu-bar-subdir-menu [prev-dirline]
  (menu-item-any-version "Prev Dirline" dired-prev-dirline
                         :help "Move to previous directory-file line"))
(define-key diredp-menu-bar-subdir-menu [next-dirline]
  (menu-item-any-version "Next Dirline" dired-next-dirline
                         :help "Move to next directory-file line"))
(define-key diredp-menu-bar-subdir-menu [insert]
  (menu-item-any-version "Insert This Subdir" dired-maybe-insert-subdir
                         :help "Insert contents of subdirectory"))
(define-key diredp-menu-bar-subdir-menu [separator-subdir] '("--"))
(define-key diredp-menu-bar-subdir-menu [create-directory]
  '("Create Directory..." . dired-create-directory)) ; Moved from "Immediate".
(define-key diredp-menu-bar-subdir-menu [up]
  (menu-item-any-version "Up Directory" dired-up-directory
                         :help "Edit the parent directory"))
;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
(when (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
  (define-key diredp-menu-bar-subdir-menu [wdired-mode]
    '(menu-item "Edit File Names" wdired-change-to-wdired-mode)))
(define-key diredp-menu-bar-subdir-menu [dired]
  (menu-item-any-version "Dired (Filter via Wildcards)..." dired
                         :help "Explore a directory (you can provide wildcards)"))


;;; Mouse-3 menu binding.
(define-key dired-mode-map [mouse-3] 'diredp-mouse-3-menu)
;;;;;;;;(define-key dired-mode-map [mouse-3] 'ignore)


;;; Non-menu Dired bindings.

;; `diredp-mouse-mark-region-files' provides Windows-Explorer behavior
;; for selecting (marking) files.
(define-key dired-mode-map [S-down-mouse-1] 'ignore) ; (normally `mouse-set-font')
(define-key dired-mode-map [S-mouse-1] 'diredp-mouse-mark-region-files)
(define-key dired-mode-map [mouse-2] 'dired-mouse-find-file-other-window)
(define-key dired-mode-map [S-down-mouse-2] 'diredp-mouse-find-file)
(define-key dired-mode-map [S-mouse-2] 'ignore)
(define-key dired-mode-map [M-mouse-2] 'diredp-mouse-find-file-other-frame)
(define-key dired-mode-map "\C-\M-o" 'dired-display-file) ; Was C-o.
(define-key dired-mode-map "\C-o" 'diredp-find-file-other-frame)
(define-key dired-mode-map "U" 'dired-unmark-all-marks)
(define-key dired-mode-map "=" 'diredp-ediff)
(substitute-key-definition 'next-line 'dired-next-line
                           dired-mode-map (current-global-map))
(substitute-key-definition 'previous-line 'dired-previous-line
                           dired-mode-map (current-global-map))
;; Commands for operating on the current line's file.  When possible,
;; these are lower-case versions of the upper-case commands for operating on
;; the marked files.  (The other corresponding lower-case letters are already
;; defined and cannot be used here.)
(define-key dired-mode-map "b" 'diredp-byte-compile-this-file)
(define-key dired-mode-map "r" 'diredp-rename-this-file)
(define-key dired-mode-map "y" 'diredp-relsymlink-this-file)
(define-key dired-mode-map "z" 'diredp-compress-this-file)
(define-key dired-mode-map "\r" 'dired-find-file)
(when (fboundp 'mkhtml-dired-files)
  (define-key dired-mode-map [?\M-h] 'mkhtml-dired-files))
(define-key dired-mode-map [?\M-u] 'diredp-upcase-this-file)
(define-key dired-mode-map [?\M-l] 'diredp-downcase-this-file)
(define-key dired-mode-map [?\M-c] 'diredp-capitalize-this-file)
(define-key dired-mode-map [?\M-m] 'diredp-chmod-this-file)
(define-key dired-mode-map [?\M-p] 'diredp-print-this-file)
(substitute-key-definition 'kill-line 'diredp-delete-this-file
                           dired-mode-map (current-global-map))
;; This replaces the `dired-x.el' binding of `dired-mark-extension'.
(define-key dired-mode-map "*." 'diredp-mark/unmark-extension)


;;; Undefine some bindings that would try to modify a Dired buffer.
;;; Their key sequences will then appear to the user as available for
;;; local (Dired) definition.
(when (fboundp 'undefine-killer-commands)
  (undefine-killer-commands dired-mode-map (current-global-map)))



(defgroup Dired-Plus nil
  "Various enhancements to Dired."
  :prefix "diredp-" :group 'dired
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
dired+.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/dired+.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/DiredPlus")
  :link '(emacs-commentary-link :tag "Commentary" "dired+")
  )

;;;-----------------------------------------------------------------
;;; Face Definitions

;;; Miscellaneous faces.
(defface diredp-display-msg 
'((t (:foreground "Blue")))
  "*Face used for message display."
  :group 'Dired-Plus)
(defvar diredp-display-msg 'diredp-display-msg)

;;; Faces used to fontify buffer when using second level of fontifying.
(defface diredp-dir-heading
  '((t (:foreground "Blue" :background "Pink")))
  "*Face used for directory headings in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-dir-heading 'diredp-dir-heading)

(defface diredp-deletion
  '((t (:foreground "Yellow" :background "Red")))
  "*Face used for deletion flags (D) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-deletion 'diredp-deletion)

(defface diredp-deletion-file-name
  '((t (:foreground "Red")))
  "*Face used for names of deleted files in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-deletion-file-name 'diredp-deletion-file-name)

(defface diredp-flag-mark
  '((t (:foreground "Yellow" :background "Blueviolet")))
  "*Face used for flags and marks (except D) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-flag-mark 'diredp-flag-mark)

(defface diredp-flag-mark-line
  '((t (:background "Skyblue")))
  "*Face used for flagged and marked lines in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-flag-mark-line 'diredp-flag-mark-line)

(defface diredp-file-suffix
  '((t (:foreground "DarkMagenta")))
  "*Face used for file suffixes in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-file-suffix 'diredp-file-suffix)

(defface diredp-symlink
  '((t (:foreground "DarkOrange")))
  "*Face used for symbolic links in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-symlink 'diredp-symlink)

(defface diredp-date-time
  '((t (:foreground "DarkGoldenrod4")))
  "*Face used for date and time in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-date-time 'diredp-date-time)

(defface diredp-file-name
  '((t (:foreground "Blue")))
  "*Face used for file names (without suffixes) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-file-name 'diredp-file-name)

(defface diredp-ignored-file-name
  '((t (:foreground "#00006DE06DE0")))
  "*Face used for ignored file names  in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-ignored-file-name 'diredp-ignored-file-name)

(defface diredp-compressed-file-suffix
  '((t (:foreground "Yellow")))
  "*Face used for compressed file suffixes in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-compressed-file-suffix 'diredp-compressed-file-suffix)

;; For this to show up, you need `F' among the options in `dired-listing-switches'.
;; For example, I use "-alF" for `dired-listing-switches'.
(defface diredp-executable-tag
  '((t (:foreground "Red")))
  "*Face used for executable tag (*) on file names in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-executable-tag 'diredp-executable-tag)

(defface diredp-dir-priv
  '((t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for directory privilege indicator (d) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-dir-priv 'diredp-dir-priv)

(defface diredp-exec-priv
  '((t (:background "LightSteelBlue")))
  "*Face used for execute privilege indicator (x) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-exec-priv 'diredp-exec-priv)

(defface diredp-other-priv
  '((t (:background "PaleGoldenrod")))
  "*Face used for l,s,S,t,T privilege indicators in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-other-priv 'diredp-other-priv)

(defface diredp-write-priv
  '((t (:background "Orchid")))
  "*Face used for write privilege indicator (w) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-write-priv 'diredp-write-priv)

(defface diredp-read-priv
  '((t (:background "MediumAquamarine")))
  "*Face used for read privilege indicator (w) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-read-priv 'diredp-read-priv)

(defface diredp-no-priv
  '((t (:background "LightGray")))
  "*Face used for no privilege indicator (-) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-no-priv 'diredp-no-priv)

(defface diredp-rare-priv
  '((t (:foreground "Magenta" :background "SpringGreen")))
  "*Face used for rare privilege indicators (b,c,s,m,p,S) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-rare-priv 'diredp-rare-priv)

(defface diredp-link-priv
  '((t (:foreground "DarkOrange")))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-link-priv 'diredp-link-priv)


;;; Define second level of fontifying.
(defvar diredp-font-lock-keywords-1
  (list
   '("^  \\(.+:\\)$" 1 diredp-dir-heading) ; Directory headers
   '("[^ .]\\.\\([^. /]+\\)$" 1 diredp-file-suffix) ; Suffix
   '("\\([^ ]+\\) -> [^ ]+$" 1 diredp-symlink) ; Symbolic links
   ;; 1) Date/time and 2) filename w/o suffix:
   (list dired-move-to-filename-regexp  '(1 diredp-date-time t t) ; Date/time
         (list "\\(.+\\)$" nil nil (list 0 diredp-file-name 'keep t))) ; Filename
   ;; Files to ignore
   (list (concat "^  \\(.*\\("
                 (concat (mapconcat 'regexp-quote
                                    (or (and (boundp 'dired-omit-extensions)
                                             dired-omit-extensions)
                                        completion-ignored-extensions)
                                    "[*]?\\|")
                         "[*]?")        ; Allow for executable flag (*).
                 "\\|\\.\\(g?z\\|Z\\)[*]?\\)\\)$") ; Compressed.
         1 diredp-ignored-file-name t)
   '("[^ .]\\.\\(g?[zZ]\\)[*]?$" 1 diredp-compressed-file-suffix t) ; Compressed (*.z)
   '("\\([*]\\)$" 1 diredp-executable-tag t) ; Executable (*)
   '(" \\([0-9]+[kKMGTPEZY]?\\)" 1 diredp-file-suffix) ; File size and inode number
   ;; Directory names
   (list "^..\\([0-9]* \\)*d"
         (list dired-move-to-filename-regexp nil nil)
         (list "\\(.+\\)" nil nil '(0 diredp-dir-priv t t)))
   '("^..\\([0-9]* \\)*.........\\(x\\)" 2 diredp-exec-priv) ;o x
   '("^..\\([0-9]* \\)*.........\\([lsStT]\\)" 2 diredp-other-priv) ; o misc
   '("^..\\([0-9]* \\)*........\\(w\\)" 2 diredp-write-priv) ; o w
   '("^..\\([0-9]* \\)*.......\\(r\\)" 2 diredp-read-priv)   ; o r
   '("^..\\([0-9]* \\)*......\\(x\\)" 2 diredp-exec-priv)    ; g x
   '("^..\\([0-9]* \\)*....[^0-9].\\([lsStT]\\)" 2 diredp-other-priv) ; g misc
   '("^..\\([0-9]* \\)*.....\\(w\\)" 2 diredp-write-priv) ; g w
   '("^..\\([0-9]* \\)*....\\(r\\)" 2 diredp-read-priv)   ; g r
   '("^..\\([0-9]* \\)*...\\(x\\)" 2 diredp-exec-priv)    ; u x
   '("^..\\([0-9]* \\)*...\\([lsStT]\\)" 2 diredp-other-priv) ; u misc
   '("^..\\([0-9]* \\)*..\\(w\\)" 2 diredp-write-priv) ; u w
   '("^..\\([0-9]* \\)*.\\(r\\)" 2 diredp-read-priv)   ; u r
   '("^..\\([0-9]* \\)*.\\([-rwxlsStT]+\\)" 2 diredp-no-priv keep) ;-
   '("^..\\([0-9]* \\)*\\([bcsmpS]\\)[-rwxlsStT]" 2 diredp-rare-priv) ; (rare)
   '("^..\\([0-9]* \\)*\\(l\\)[-rwxlsStT]" 2 diredp-link-priv) ; l
   (list (concat "^\\([^ " (char-to-string dired-del-marker) "].*$\\)")
         1 diredp-flag-mark-line t) ; Flag/mark lines
   (list (concat "^\\([" (char-to-string dired-del-marker) "]\\)") ; Deletion flags (D)
         '(1 diredp-deletion t)
         '(".+" (dired-move-to-filename) nil (0 diredp-deletion-file-name t)))
   (list (concat "^\\([^ " (char-to-string dired-del-marker) "]\\)") ; Flags & marks (except D)
         1 diredp-flag-mark t)
   ) "Expressions to highlight in Dired mode.")


;;; Provide for the second level of fontifying.
(add-hook 'dired-mode-hook
          '(lambda () (if (and (boundp 'font-lock-maximum-decoration)
                               font-lock-maximum-decoration)
                          (set (make-local-variable 'font-lock-defaults)
                               '(diredp-font-lock-keywords-1 t)))))


;;;-----------------------------------------------------------------
;;; Function Definitions


;; Similar to `dired-mark-extension' in `dired-x.el'.
;; The difference is that this uses prefix arg to unmark, not to determine the mark character.
(defun diredp-mark/unmark-extension (extension &optional unmark-p)
  "Mark all files with a certain EXTENSION for use in later commands.
A `.' is not automatically prepended to the string entered.
Non-nil prefix argument UNMARK-P means unmark instead of mark."
  ;; EXTENSION may also be a list of extensions instead of a single one.
  ;; Optional MARKER-CHAR is marker to use.
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
				    "ing extension: "))
	 current-prefix-arg))
  (or (listp extension) (setq extension (list extension)))
  (dired-mark-files-regexp (concat ".";; don't match names with nothing but an extension
                                   "\\("
                                   (mapconcat 'regexp-quote extension "\\|")
                                   "\\)$")
   (and current-prefix-arg ?\040)))



;; REPLACES ORIGINAL in `dired.el'.
;; Allows for consp `dired-directory' too.
;;
(defun dired-buffers-for-dir (dir &optional file)
  "Return a list of buffers that dired DIR (top level or in-situ subdir).
If FILE is non-nil, include only those whose wildcard pattern (if any)
matches FILE.
The list is in reverse order of buffer creation, most recent last.
As a side effect, killed dired buffers for DIR are removed from
`dired-buffers'."
  (setq dir (file-name-as-directory dir))
  (let ((alist dired-buffers) result elt buf pattern)
    (while alist
      (setq elt (car alist)
	    buf (cdr elt))
      (if (buffer-name buf)
	  (if (dired-in-this-tree dir (car elt))
	      (with-current-buffer buf
		(and (assoc dir dired-subdir-alist)
		     (or (null file)
			 (let ((wildcards
                                ;; Allow for consp `dired-directory' too.
                                (file-name-nondirectory (if (consp dired-directory)
                                                            (car dired-directory)
                                                          dired-directory))))
			   (or (= 0 (length wildcards))
			       (string-match (dired-glob-regexp wildcards) file))))
		     (setq result (cons buf result)))))
	;; else buffer is killed - clean up:
	(setq dired-buffers (delq elt dired-buffers)))
      (setq alist (cdr alist)))
    result))

;;;###autoload
(defun diredp-find-file-other-frame ()
  "In dired, visit this file or directory in another frame."
  (interactive)
  (find-file-other-frame (file-name-sans-versions (dired-get-filename) t)))

;;;###autoload
(defun diredp-mouse-find-file-other-frame (event)
  "In dired, visit file or directory clicked on in another frame."
  (interactive "e")
  (let ((pop-up-frames t))
    (dired-mouse-find-file-other-window event)))


;; These functions let you use the file on the current line as the default.
;; They are useful only in Emacs 22 or later.
;;
;; However, if you use library `files+.el', you need not use these commands explicitly,
;; because that library redefines `find-file-read-args' to do the same thing, in Dired mode.
;; These are provided here in case you want to bind them directly - for example, in case your
;; code does not use `find-file-read-args'.  That is the case, for instance, for Icicles
;; (`icicles.cmd').
;;
;;;###autoload
(when (fboundp 'dired-get-file-for-visit) ; Defined in Emacs 22.
  (defun diredp-find-a-file (filename &optional wildcards)
    "`find-file', but use file on current line as default (`M-n')."
    (interactive (diredp-find-a-file-read-args "Find file: " nil))
    (find-file filename wildcards))
  
  (defun diredp-find-a-file-other-frame (filename &optional wildcards)
    "`find-file-other-frame', but use file under cursor as default (`M-n')."
    (interactive (diredp-find-a-file-read-args "Find file: " nil))
    (find-file-other-frame filename wildcards))
  
  (defun diredp-find-a-file-other-window (filename &optional wildcards)
    "`find-file-other-window', but use file under cursor as default (`M-n')."
    (interactive (diredp-find-a-file-read-args "Find file: " nil))
    (find-file-other-window filename wildcards))
  
  (defun diredp-find-a-file-read-args (prompt mustmatch)
    (list (let ((find-file-default (abbreviate-file-name (dired-get-file-for-visit))))
            (minibuffer-with-setup-hook
                (lambda () (setq minibuffer-default find-file-default))
              (read-file-name prompt nil default-directory mustmatch)))
          t)))

;; Define these for Emacs 20 and 21.
(unless (fboundp 'dired-get-file-for-visit) ; Defined in Emacs 22.
  (defun dired-get-file-for-visit ()
    "Get the current line's file name, with an error if file does not exist."
    (interactive)
    ;; We pass t for second arg so that we don't get error for `.' and `..'.
    (let ((raw (dired-get-filename nil t))
          file-name)
      (if (null raw)
          (error "No file on this line"))
      (setq file-name (file-name-sans-versions raw t))
      (if (file-exists-p file-name)
          file-name
        (if (file-symlink-p file-name)
            (error "File is a symlink to a nonexistent target")
          (error "File no longer exists; type `g' to update Dired buffer")))))

  (defun dired-find-alternate-file ()
    "In Dired, visit this file or directory instead of the dired buffer."
    (interactive)
    (set-buffer-modified-p nil)
    (find-alternate-file (dired-get-file-for-visit))))

(defun diredp-find-file-reuse-dir-buffer ()
  "Like `dired-find-file', but reuse buffer if target is a directory."
  (interactive)
  (set-buffer-modified-p nil)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (find-alternate-file file)
      (find-file file))))

;;;###autoload
(defun diredp-mouse-find-file-reuse-dir-buffer (event)
  "Like `diredp-mouse-find-file', but reuse buffer for a directory."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq file (dired-get-file-for-visit))))
    (select-window (posn-window (event-end event)))
    (if (file-directory-p file)
        (find-alternate-file (file-name-sans-versions file t))
      (find-file (file-name-sans-versions file t)))))

;;;###autoload
(defun toggle-dired-find-file-reuse-dir (force-p)
  "Toggle whether Dired `find-file' commands use alternate file.
Non-nil prefix arg FORCE-P => Use alternate file iff FORCE-P >= 0."
  (interactive "P")
  (if force-p                           ; Force.
      (if (natnump (prefix-numeric-value force-p))
          (diredp-subst-find-alternate-for-find)
        (diredp-subst-find-for-find-alternate))
    (if (where-is-internal 'dired-find-file dired-mode-map 'ascii)
        (diredp-subst-find-alternate-for-find)
      (diredp-subst-find-for-find-alternate))))

;;;###autoload
(defalias 'diredp-toggle-find-file-reuse-dir 'toggle-dired-find-file-reuse-dir)

(defun diredp-subst-find-alternate-for-find ()
  "Use find-alternate-file commands in place of find-file commands."
  (substitute-key-definition 'dired-find-file 'diredp-find-file-reuse-dir-buffer dired-mode-map)
  (substitute-key-definition 'diredp-mouse-find-file 'diredp-mouse-find-file-reuse-dir-buffer
                             dired-mode-map)
  (message "Accessing directories in Dired will REUSE the buffer"))

(defun diredp-subst-find-for-find-alternate ()
  "Don't use find-alternate-file commands in place of find-file commands."
  (substitute-key-definition 'diredp-find-file-reuse-dir-buffer 'dired-find-file dired-mode-map)
  (substitute-key-definition 'diredp-mouse-find-file-reuse-dir-buffer 'diredp-mouse-find-file
                             dired-mode-map)
  (message "Accessing directories in Dired will NOT reuse the buffer"))


;;;###autoload
(defun diredp-omit-marked ()
  "Omit lines of marked files.  Return the number of lines omitted."
  (interactive)
  (let ((old-modified-p (buffer-modified-p))
        count)
    (when (interactive-p) (message "Omitting marked lines..."))
    (setq count (dired-do-kill-lines nil "Omitted %d line%s."))
    (set-buffer-modified-p old-modified-p) ; So no `%*' appear in mode-line.
    count))

;; `dired-do-toggle' was renamed to `dired-toggle-marks' after Emacs 20.
;; That's why we aliased it to `dired-toggle-marks' at the top of the file.
;;
;;;###autoload
(defun diredp-omit-unmarked ()
  "Omit lines of unmarked files.  Return the number of lines omitted."
  (interactive)
  (let ((old-modified-p (buffer-modified-p))
        count)
    (dired-do-toggle)
    (message "Omitting unmarked lines...")
    (setq count (diredp-omit-marked))
    (dired-do-toggle)                   ; Marks all except `.', `..'
    (set-buffer-modified-p old-modified-p) ; So no `%*' appear in mode-line.
    count))

;;;###autoload
(defun diredp-ediff (file2)
  "Compare file at cursor with file FILE2 using `ediff'.
FILE2 defaults to the file at the cursor as well.  If you enter just a
directory name for FILE2, then the file at the cursor is compared with
a file of the same name in that directory.  FILE2 is the second file
given to `ediff'; the file at the cursor is the first."
  (interactive
   (progn
     (require 'ediff)
     (list (ediff-read-file-name        ; In `ediff.el'.
            (format "Compare %s with" (dired-get-filename t))
            (dired-current-directory) (dired-get-filename)))))
  (ediff-files (dired-get-filename) file2)) ; In `ediff.el'.


(defsubst diredp-fewer-than-2-files-p (arg)
  "Return non-nil iff fewer than two files are to be treated by dired.
More precisely, return non-nil iff ARG is nil and fewer than two
files are marked, or ARG is -1, 0 or 1."
  (if arg
      (and (integerp arg) (< (abs arg) 2)) ; Next or previous file (or none).
    (not (save-excursion                ; Fewer than two marked files.
           (goto-char (point-min))
           (re-search-forward (dired-marker-regexp) nil t 2)))))



;; REPLACES ORIGINAL version in `dired-aux.el':
;; Redisplay only if at most one file is being treated.
;;;###autoload
(defun dired-do-compress (&optional arg)
  "Compress or uncompress marked (or next prefix ARG) files."
  (interactive "P")
  (dired-map-over-marks-check (function dired-compress) arg 'compress
                              (diredp-fewer-than-2-files-p arg)))


;; REPLACES ORIGINAL version in `dired-aux.el':
;; Redisplay only if at most one file is being treated.
;;;###autoload
(defun dired-do-byte-compile (&optional arg)
  "Byte compile marked (or next prefix ARG) Emacs Lisp files."
  (interactive "P")
  (dired-map-over-marks-check (function dired-byte-compile) arg 'byte-compile
                              (diredp-fewer-than-2-files-p arg)))


;; REPLACES ORIGINAL version in `dired-aux.el':
;; Redisplay only if at most one file is being treated.
;;;###autoload
(defun dired-do-load (&optional arg)
  "Load the marked (or next prefix ARG) Emacs Lisp files."
  (interactive "P")
  (dired-map-over-marks-check (function dired-load) arg 'load
                              (diredp-fewer-than-2-files-p arg)))


;;; VISIT ALL MARKED FILES SIMULTANEOUSLY.

;;; Brief Description:
;;;
;;; `dired-do-find-marked-files' is bound to `F' by dired-x.el.
;;;
;;; * Use `dired-get-marked-files' to collect the marked files in the current
;;;   Dired Buffer into a list of filenames `FILE-LIST'.
;;;
;;; * Pass FILE-LIST to `dired-simultaneous-find-file' all with
;;;   `dired-do-find-marked-files''s prefix argument OPTION.
;;;
;;; * `dired-simultaneous-find-file' runs through FILE-LIST decrementing the
;;;   list each time.
;;;
;;; * If OPTION and `pop-up-frames' are both nil, then calculate the
;;; `size' of the window for each file by dividing the `window-height'
;;; by length of FILE-LIST.  Thus, `size' is cognizant of the
;;; window-configuration.
;;;
;;; * If `size' is too small abort, otherwise run `find-file' on each element
;;;   of FILE-LIST giving each a window of height `size'.

;; REPLACES ORIGINAL version in `dired-x.el':
;; Doc string updated to reflect change to `dired-simultaneous-find-file'.
;;;###autoload
(defun dired-do-find-marked-files (&optional arg)
  "Find marked files, displaying all of them simultaneously.
With a prefix ARG >= 0, just find files but do not select them.

If no prefix ARG, and variable `pop-up-frames' is non-nil, or
if prefix ARG < 0, then each file is displayed in a separate frame.

Otherwise (no prefix ARG and nil `pop-up-frames'), the current window
is split across all marked files, as evenly as possible.  Remaining
lines go to the bottom-most window.  The number of files that can be
displayed this way is restricted by the height of the current window
and `window-min-height'.

To keep the Dired buffer displayed, type \\[split-window-vertically] first.
To display just the marked files, type \\[delete-other-windows] first."
  (interactive "P")
  (setq arg (and arg (prefix-numeric-value arg)))
  (dired-simultaneous-find-file (dired-get-marked-files) arg))


;; REPLACES ORIGINAL version in `dired-x.el':
;; Use separate frames instead of windows if `pop-up-frames' is non-nil,
;; or if prefix arg is negative.
;;
(defun dired-simultaneous-find-file (file-list option)
  "Visit all files in list FILE-LIST and display them simultaneously.

With non-nil OPTION >= 0, the files are found but not selected.

If `pop-up-frames' is non-nil or OPTION < 0, use a separate frame
for each file.

Otherwise, the current window is split across all files in
FILE-LIST, as evenly as possible.  Remaining lines go to the
bottom-most window.  The number of files that can be displayed
this way is restricted by the height of the current window and
the variable `window-min-height'."

  ;; This is not interactive because it is usually too clumsy to
  ;; specify FILE-LIST interactively unless via dired.

  (let (size)
    (cond ((and option (natnump option))
           (while file-list (find-file-noselect (car file-list)) (pop file-list)))
          ((or pop-up-frames option)
           (while file-list (find-file-other-frame (car file-list)) (pop file-list)))
          (t
           (setq size (/ (window-height) (length file-list)))
           (when (> window-min-height size)
             (error "Too many files to visit simultaneously.  Try C-u prefix."))
           (find-file (car file-list))
           (pop file-list)
           (while file-list
             ;; Vertically split off a window of desired size.
             ;; Upper window will have SIZE lines.
             ;; Select lower (larger) window.  We split it again.
             (select-window (split-window nil size))
             (find-file (car file-list))
             (pop file-list))))))


;;;;;; REPLACES ORIGINAL versions in both `dired.el' and `dired-x.el':
;;;;;;
;;;;;; 1. This incorporates the `dired-x.el' change to the `dired.el'
;;;;;;    definition.  This version works with or without using dired-x.
;;;;;;    The `dired-x.el' version respects the var `dired-find-subdir'.
;;;;;;    When `dired-find-subdir' is non-nil, this version is the same
;;;;;;    as the `dired-x.el' version, except that a bug is corrected:
;;;;;;    Whenever the argument to `dired-find-buffer-nocreate' is a cons,
;;;;;;    the call to `dired-buffers-for-dir' gave a wrong type error.
;;;;;;    This has been avoided by not respecting `dired-find-subdir'
;;;;;;    whenever `dired-find-buffer-nocreate' is a cons.
;;;;;;    For the case when `dired-find-subdir' is nil, see #2, below.
;;;;;;
;;;;;; 2. Unless `dired-find-subdir' is bound and non-nil:
;;;;;;    If both DIRNAME and `dired-directory' are conses, then only
;;;;;;    compare their cars (directories), not their explicit file lists
;;;;;;    too.  If equal, then update `dired-directory's file list to that
;;;;;;    of DIRNAME.
;;;;;;
;;;;;;    This prevents `dired-internal-noselect' (which is currently
;;;;;;    `dired-find-buffer-nocreate's only caller) from creating a new
;;;;;;    buffer in this case whenever a different set of files is present
;;;;;;    in the cdr of DIRNAME and DIRNAME represents the same buffer as
;;;;;;    `dired-directory'.
;;;;;;
;;;;;;    If only one of DIRNAME and `dired-directory' is a cons, then
;;;;;;    this returns nil.
;;;;;;;###autoload
;;;;(defun dired-find-buffer-nocreate (dirname &optional mode)
;;;;  (let ((atomic-dirname-p (atom dirname)))
;;;;    (if (and (boundp 'dired-find-subdir) dired-find-subdir atomic-dirname-p)
;;;;        ;; This is the `dired-x.el' change:
;;;;        (let* ((cur-buf (current-buffer))
;;;;               (buffers (nreverse (dired-buffers-for-dir dirname)))
;;;;               (cur-buf-matches (and (memq cur-buf buffers)
;;;;                                     ;; Files list (wildcards) must match, too:
;;;;                                     (equal dired-directory dirname))))
;;;;          (setq buffers (delq cur-buf buffers)) ; Avoid using same buffer---
;;;;          (or (car (sort buffers (function dired-buffer-more-recently-used-p)))
;;;;              (and cur-buf-matches cur-buf))) ; ---unless no other possibility.
;;;;      ;; Comment from `dired.el':
;;;;      ;;  This differs from `dired-buffers-for-dir' in that it doesn't consider
;;;;      ;;  subdirs of `default-directory' and searches for the first match only.
;;;;      (let ((blist dired-buffers)       ; was (buffer-list)
;;;;            found)
;;;;        (or mode (setq mode 'dired-mode))
;;;;        (while blist
;;;;          (if (null (buffer-name (cdr (car blist))))
;;;;              (setq blist (cdr blist))
;;;;            (save-excursion
;;;;              (set-buffer (cdr (car blist)))
;;;;              (if (not (and (eq major-mode mode)
;;;;                            ;; DIRNAME and `dired-directory' have the same dir,
;;;;                            ;; and if either of them has an explicit file list,
;;;;                            ;; then both of them do.  In that case, update
;;;;                            ;; `dired-directory's file list from DIRNAME.
;;;;                            (if atomic-dirname-p
;;;;                                (and (atom dired-directory) ; Both are atoms.
;;;;                                     (string= (file-truename dirname)
;;;;                                              (file-truename dired-directory)))
;;;;                              (and (consp dired-directory) ; Both are conses.
;;;;                                   (string=
;;;;                                    (file-truename (car dirname))
;;;;                                    (file-truename (car dired-directory)))
;;;;                                   ;; Update `dired-directory's file list.
;;;;                                   (setq dired-directory dirname)))))
;;;;                  (setq blist (cdr blist))
;;;;                (setq found (cdr (car blist)))
;;;;                (setq blist nil)))))
;;;;        found))))



;; REPLACES ORIGINAL in `dired.el':
;; Resets `mode-line-process' to nil.
;;;###autoload
(when (< emacs-major-version 21)
  (or (fboundp 'old-dired-revert) (fset 'old-dired-revert (symbol-function 'dired-revert)))
  (defun dired-revert (&optional arg noconfirm)
    (setq mode-line-process nil)        ; Set by, e.g., `find-dired'.
    (old-dired-revert arg noconfirm)))



;; REPLACES ORIGINAL in `dired.el':
;; `mouse-face' on whole line, not just file name.
;;;###autoload
(defun dired-insert-set-properties (beg end)
  "Highlight entire dired line upon mouseover."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (condition-case nil
          (when (dired-move-to-filename)
            (add-text-properties
             (save-excursion (beginning-of-line) (point))
             (save-excursion (end-of-line) (point))
             '(mouse-face highlight help-echo "mouse-2: visit this file in other window")))
	(error nil))
      (forward-line 1))))



;; REPLACES ORIGINAL in `dired.el':
;; Display a message to warn that flagged, not marked, files will be deleted.
;;;###autoload
(defun dired-do-flagged-delete (&optional no-msg)
  "In dired, delete the files flagged for deletion.
NOTE: This deletes flagged, not marked, files.
If arg NO-MSG is non-nil, no messages are displayed."
  (interactive)
  (unless no-msg
    (ding)
    (message "NOTE: Deletion of files flagged `%c' (not those marked `%c')."
             dired-del-marker dired-marker-char))
  (let* ((dired-marker-char dired-del-marker)
	 (regexp (dired-marker-regexp))
	 case-fold-search)
    (if (save-excursion (goto-char (point-min))
                        (re-search-forward regexp nil t))
	(dired-internal-do-deletions
	 ;; This can't move point since last arg is nil.
	 (dired-map-over-marks (cons (dired-get-filename) (point)) nil)
	 nil)
      (unless no-msg (message "(No deletions requested.)")))))


;; REPLACES ORIGINAL in `dired.el':
;; Display a message to warn that marked, not flagged, files will be deleted.
;;;###autoload
(defun dired-do-delete (&optional arg)
  "Delete all marked (or next ARG) files.
NOTE: This deletes marked, not flagged, files."
  (interactive "P")
  ;; This is more consistent with the file-marking feature than
  ;; `dired-do-flagged-delete'.  But it can be confusing to the user,
  ;; especially since this is usually bound to `D', which is also the
  ;; `dired-del-marker'.  So offer this warning message:
  (unless arg
    (ding)
    (message "NOTE: Deletion of files marked `%c' (not those flagged `%c')."
             dired-marker-char dired-del-marker))
  (dired-internal-do-deletions
   ;; This may move point if ARG is an integer.
   (dired-map-over-marks (cons (dired-get-filename) (point)) arg)
   arg))

;;;###autoload
(defun diredp-capitalize (&optional arg)
  "Rename all marked (or next ARG) files by capitilizing them.
This gives the file name(s) a first character in upper case and the
rest lower case."
  (interactive "P")
  (dired-rename-non-directory (function capitalize) "Rename by capitalizing:" arg))


;;; Versions of `dired-do-*' commands for just this line's file.
(defsubst diredp-delete-this-file ()
  "In dired, delete the file on the cursor line, upon confirmation."
  (interactive) (dired-do-delete 1))
(defsubst diredp-capitalize-this-file ()
  "In dired, rename the file on the cursor line by capitilizing it.
This gives the file name a first character in upper case and the rest
lower case."
  (interactive) (diredp-capitalize 1))
(defsubst diredp-downcase-this-file ()
  "In dired, rename the file on the cursor line to lower case."
  (interactive) (dired-downcase 1))
(defsubst diredp-upcase-this-file ()
  "In dired, rename the file on the cursor line to upper case."
  (interactive) (dired-upcase 1))
(defsubst diredp-rename-this-file ()
  "In dired, rename the file on the cursor line."
  (interactive) (dired-do-rename 1))
(defsubst diredp-copy-this-file ()
  "In dired, copy the file on the cursor line."
  (interactive) (dired-do-copy 1))
(defsubst diredp-relsymlink-this-file ()
  "In dired, make a relative symbolic link to file on cursor line."
  (interactive) (and (fboundp 'dired-do-relsymlink) (dired-do-relsymlink 1)))
(defsubst diredp-symlink-this-file ()
  "In dired, make a symbolic link to the file on the cursor line."
  (interactive) (dired-do-symlink 1))
(defsubst diredp-hardlink-this-file ()
  "In dired, add a name (hard link) to the file on the cursor line."
  (interactive) (dired-do-hardlink 1))
(defsubst diredp-print-this-file ()
  "In dired, print the file on the cursor line."
  (interactive) (dired-do-print 1))
(defsubst diredp-compress-this-file ()
  "In dired, compress or uncompress the file on the cursor line."
  (interactive) (dired-do-compress 1))
(defsubst diredp-shell-command-this-file (command)
  "In dired, run a shell COMMAND on the file on the cursor line."
  (interactive
   (list (dired-read-shell-command (concat "! on " "%s: ") 1
                                   (list (dired-get-filename t)))))
  (dired-do-shell-command command 1))
(defsubst diredp-byte-compile-this-file ()
  "In dired, byte compile the (Lisp source) file on the cursor line."
  (interactive) (dired-do-byte-compile 1))
(defsubst diredp-load-this-file ()
  "In dired, load the file on the cursor line."
  (interactive) (dired-do-load 1))
(defsubst diredp-chmod-this-file ()
  "In dired, change the mode of the file on the cursor line."
  (interactive) (dired-do-chmod 1))
(defsubst diredp-chgrp-this-file ()
  "In dired, change the group of the file on the cursor line."
  (interactive) (dired-do-chgrp 1))
(defsubst diredp-chown-this-file ()
  "In dired, change the owner of the file on the cursor line."
  (interactive) (dired-do-chown 1))


;; REPLACES ORIGINAL in `dired-x.el':
;; 1. Variable (symbol) `s' -> `blks'.
;; 2. Fixes to remove leading space from `uid' and allow `.' in `gid'.
;; 3. Cleaned up doc string and code a bit.
;;;###autoload
(defun dired-mark-sexp (predicate &optional unmark-p)
  "Mark files for which PREDICATE returns non-nil.
With non-nil prefix arg UNMARK-P, unmark those files instead.

PREDICATE is a lisp sexp that can refer to the following variables:

    `mode'   [string]  file permission bits, e.g. \"-rw-r--r--\"
    `nlink'  [integer] number of links to file
    `size'   [integer] file size in bytes
    `uid'    [string]  owner
    `gid'    [string]  group (If the gid is not displayed by `ls',
                       this will still be set (to the same as uid))
    `time'   [string]  the time that `ls' displays, e.g. \"Feb 12 14:17\"
    `name'   [string]  the name of the file
    `sym'    [string]  if file is a symbolic link, the linked-to name,
                       else \"\"
    `inode'  [integer] the inode of the file (only for `ls -i' output)
    `blks'   [integer] the size of the file for `ls -s' output
                       (ususally in blocks or, with `-k', in Kbytes)
Examples:
  Mark zero-length files: `(equal 0 size)'
  Mark files last modified on Feb 2: `(string-match \"Feb  2\" time)'
  Mark uncompiled Emacs Lisp files (`.el' file without a `.elc' file):
     First, dired just the source files: `dired *.el'.
     Then, use \\[dired-mark-sexp] with this sexp:
          (not (file-exists-p (concat name \"c\")))"

  ;; Using `sym' = "", instead of nil, for non-linked files avoids the trap of
  ;; (string-match "foo" sym) into which a user would soon fall.
  ;; Use `equal' instead of `=' in the example, as it works on integers and strings.
  (interactive "xVars: inode,blks,mode,nlink,uid,gid,size,time,name,sym -> \nP")
  (message "%s" predicate)
  (let ((dired-marker-char (if unmark-p ?\040 dired-marker-char))
        (inode nil)
        (blks nil)
        mode nlink uid gid size time name sym)
    (dired-mark-if
     (save-excursion
       (and
        ;; Sets vars INODE BLKS MODE NLINK UID GID SIZE TIME NAME and SYM
        ;; according to current file line.  Returns `t' for success, nil if
        ;; there is no file line.  Upon success, these vars are set, to either
        ;; nil or the appropriate value, so they need not be initialized.
        ;; Moves point within the current line.
        (dired-move-to-filename)
        (let ((mode-len 10)             ; Length of mode string.
              ;; As in `dired.el', but with subexpressions \1=inode, \2=blks:
              (dired-re-inode-size "\\s *\\([0-9]*\\)\\s *\\([0-9]*\\) ?")
              pos)
          (beginning-of-line)
          (forward-char 2)
          (when (looking-at dired-re-inode-size)
            (goto-char (match-end 0))
            (setq inode (string-to-number (buffer-substring (match-beginning 1)
                                                            (match-end 1))))
            (setq blks (string-to-number (buffer-substring (match-beginning 2)
                                                           (match-end 2)))))
          (setq mode (buffer-substring (point) (+ mode-len (point))))
          (forward-char mode-len)
          (setq nlink (read (current-buffer)))
          (forward-char 1)              ; Fix: skip space.
          ;; Karsten Wenger <kw@cis.uni-muenchen.de> fixed uid.
          (setq uid (buffer-substring (+ (point) 1) (progn (forward-word 1) (point))))
          (re-search-forward
           (if (< emacs-major-version 20)
               "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)"
             dired-move-to-filename-regexp))
          (goto-char (match-beginning 1))
          (forward-char -1)
          (setq size (string-to-number (buffer-substring (save-excursion
                                                           (backward-word 1)
                                                           (setq pos (point)))
                                                         (point))))
          (goto-char pos)
          (backward-word 1)
          ;; if no gid is displayed, gid will be set to uid
          ;; but user will then not reference it anyway in PREDICATE.
          (setq gid (buffer-substring (save-excursion (forward-word 1) (point))
                                      (point)))
          (setq time (buffer-substring (match-beginning 1)
                                       (1- (dired-move-to-filename))))
          (setq name (buffer-substring (point)
                                       (or (dired-move-to-end-of-filename t)
                                           (point))))
          (setq sym  (if (looking-at " -> ")
                         (buffer-substring (progn (forward-char 4) (point))
                                           (progn (end-of-line) (point)))
                       "")))
        (eval predicate)))
     (format "'%s file" predicate))))

;;;###autoload
(defun diredp-mark-region-files (&optional unmark-p)
  "Mark all of the files in the current region (if it is active).
With non-nil prefix arg UNMARK-P, unmark them instead."
  (interactive "P")
  (let ((beg (min (point) (mark)))
        (end (max (point) (mark))))
    (setq beg (save-excursion (goto-char beg) (beginning-of-line) (point)))
    (setq end (save-excursion (goto-char end) (end-of-line) (point)))
    (let ((dired-marker-char (if unmark-p ?\040 dired-marker-char)))
      (dired-mark-if (and (<= (point) end) (>= (point) beg)) "in region"))))

;;;###autoload
(defun diredp-unmark-region-files (&optional mark-p)
  "Unmark all of the files in the current region (if it is active).
With non-nil prefix arg UNMARK-P, mark them instead."
  (interactive "P")
  (let ((beg (min (point) (mark)))
        (end (max (point) (mark))))
    (setq beg (save-excursion (goto-char beg) (beginning-of-line) (point)))
    (setq end (save-excursion (goto-char end) (end-of-line) (point)))
    (let ((dired-marker-char (if mark-p dired-marker-char ?\040)))
      (dired-mark-if (and (<= (point) end) (>= (point) beg)) "in region"))))

;;;###autoload
(defun diredp-flag-region-files-for-deletion ()
  "Flag all of the files in the current region (if it is active) for deletion."
  (interactive)
  (let ((beg (min (point) (mark)))
        (end (max (point) (mark))))
    (setq beg (save-excursion (goto-char beg) (beginning-of-line) (point)))
    (setq end (save-excursion (goto-char end) (end-of-line) (point)))
    (let ((dired-marker-char dired-del-marker))
      (dired-mark-if (and (<= (point) end) (>= (point) beg)) "in region"))))



;;; Mouse 3 menu.
;;;;;;;;;;;;;;;;;

;;;###autoload
(defvar diredp-file-line-overlay nil)

;;;###autoload
(defun diredp-mouse-3-menu (event)
  "Pop-up menu on Mouse-3 for a file or directory listed in dired buffer."
  (interactive "e")
  (let (selection)
    (if mark-active
        (setq selection
              (x-popup-menu
               event
               (list
                "Files in Region"
                (list
                 ""
                 '("Mark" . diredp-mark-region-files)
                 '("Unmark" . diredp-unmark-region-files)
                 '("Flag for Deletion" .
                   diredp-flag-region-files-for-deletion)))))
      (let* ((mouse-pos (event-start event))
             bol eol
             (file/dir-name
              (save-excursion
                (set-buffer (window-buffer (posn-window mouse-pos)))
                (save-excursion
                  (goto-char (posn-point mouse-pos))
                  (save-excursion
                    (setq bol (progn (beginning-of-line) (point)))
                    (setq eol (progn (end-of-line) (point))))
                  (if diredp-file-line-overlay ; Don't recreate if exists.
                      (move-overlay diredp-file-line-overlay bol eol
                                    (current-buffer))
                    (setq diredp-file-line-overlay (make-overlay bol eol))
                    (overlay-put diredp-file-line-overlay 'face 'region))
                  (and (not (eobp)) (dired-get-filename nil t))))))
        (sit-for 0)
        (setq selection
              (x-popup-menu
               (and file/dir-name event)
               (list
                "This File"
                (if file/dir-name
                    (list
                     file/dir-name

                     ;; Stuff from `Mark' menu.
                     (if (dired-file-marker file/dir-name)
                         '("Unmark" . diredp-mouse-unmark) ; It's now marked.
                       '("Mark" . diredp-mouse-mark)) ;  It's now unmarked.
                     '("Flag for Deletion" . diredp-mouse-flag-file-deletion)
                     '("--")            ; Separator.

                     ;; Stuff from `Single' / `Multiple' menus.
                     '("Open" . diredp-mouse-find-file)
                     '("Open in Other Window" .
                       dired-mouse-find-file-other-window)
                     '("Open in Other Frame" .
                       diredp-mouse-find-file-other-frame)
                     '("View (Read Only)" . diredp-mouse-view-file)
                     '("Compare..." . diredp-mouse-ediff)
                     '("Diff..." . diredp-mouse-diff)
                     '("Diff with Backup" . diredp-mouse-backup-diff)
                     '("Copy to..." . diredp-mouse-do-copy)
                     '("Rename to..." . diredp-mouse-do-rename)
                     '("Upcase" . diredp-mouse-upcase)
                     '("Downcase" . diredp-mouse-downcase)
                     '("Delete" . diredp-mouse-do-delete)
                     '("Shell Command..." . diredp-mouse-do-shell-command)
                     (and (fboundp 'dired-do-relsymlink)
                          '("Symlink to (Relative)..."
                                        . dired-do-relsymlink))
                     '("Symlink to..." . diredp-mouse-do-symlink)
                     '("Hardlink to..." . diredp-mouse-do-hardlink)
                     '("Print" . diredp-mouse-do-print)
                     '("Compress/Decompress" . diredp-mouse-do-compress)
                     '("Byte Compile" . diredp-mouse-do-byte-compile)
                     '("Load" . diredp-mouse-do-load)
                     '("Change Mode..." . diredp-mouse-do-chmod)
                     '("Change Group..." . diredp-mouse-do-chgrp)
                     '("Change Owner..." . diredp-mouse-do-chown)
                     )
                  '("" (""))))))        ; No menu: not on a file line.
        (when diredp-file-line-overlay
          (delete-overlay diredp-file-line-overlay))))
    (and selection (call-interactively selection))))

;;;###autoload
(defun diredp-mouse-find-file (event)
  "Replace dired in its window by this file or directory."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq file (dired-get-filename))))
    (select-window (posn-window (event-end event)))
    (find-file (file-name-sans-versions file t))))

;;;###autoload
(defun diredp-mouse-view-file (event)
  "Examine this file in view mode, returning to dired when done.
When file is a directory, show it in this buffer if it is inserted;
otherwise, display it in another buffer."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq file (dired-get-filename))))
    (select-window (posn-window (event-end event)))
    (if (file-directory-p file)
        (or (and (cdr dired-subdir-alist) (dired-goto-subdir file))
            (dired file))
      (view-file file))))               ; In `view.el'.

;;;###autoload
(defun diredp-mouse-ediff (event)
  "Compare this file (pointed by mouse) with file FILE2 using `ediff'.
FILE2 defaults to this file as well.  If you enter just a directory
name for FILE2, then this file is compared with a file of the same
name in that directory.  FILE2 is the second file given to `ediff';
this file is the first given to it."
  (interactive "e")
  (require 'ediff)
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (call-interactively 'diredp-ediff)))

;;;###autoload
(defun diredp-mouse-diff (event &optional switches)
  "Compare this file (pointed by mouse) with file FILE2 using `diff'.
FILE2 defaults to the file at the mark.  This file is the first file
given to `diff'.  With prefix arg, prompt for second arg SWITCHES,
which are options for `diff'."
  (interactive "e")
  (let ((default (if (mark t)
                     (save-excursion (goto-char (mark t))
                                     (dired-get-filename t t))))
        (mouse-pos (event-start event)))
    (require 'diff)
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (let ((file2 (read-file-name (format "Diff %s with: %s"
                                         (dired-get-filename t)
                                         (if default
                                             (concat "(default " default ") ")
                                           ""))
                                 (dired-current-directory) default t)))
      (setq switches
            (and current-prefix-arg
                 (read-string "Options for diff: "
                              (if (stringp diff-switches)
                                  diff-switches
                                (mapconcat 'identity diff-switches " ")))))
      (diff file2 (dired-get-filename t) switches))))

;;;###autoload
(defun diredp-mouse-backup-diff (event)
  "Diff this file with its backup file or vice versa.
Use the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'.
With prefix arg, prompt for SWITCHES which are the options for `diff'."
  (interactive "e")
  (let ((switches (and current-prefix-arg
                       (read-string "Options for diff: "
                                    (if (stringp diff-switches)
                                        diff-switches
                                      (mapconcat 'identity diff-switches
                                                 " ")))))
        (mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (diff-backup (dired-get-filename) switches)))

;;;###autoload
(defun diredp-mouse-mark (event)
  "In dired, mark this file.
If on a subdir headerline, mark all its files except `.' and `..'.

Use \\[dired-unmark-all-files] to remove all marks,
and \\[dired-unmark] on a subdir to remove the marks in this subdir."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (if (and (cdr dired-subdir-alist) (dired-get-subdir))
      (save-excursion (dired-mark-subdir-files))
    (let (buffer-read-only)
      (dired-repeat-over-lines 1 (function (lambda ()
                                             (delete-char 1)
                                             (insert dired-marker-char))))
      (dired-previous-line 1))))

;;;###autoload
(defun diredp-mouse-unmark (event)
  "In dired, unmark this file.
If looking at a subdir, unmark all its files except `.' and `..'."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (let ((dired-marker-char ?\040)) (dired-mark nil))
  (dired-previous-line 1))

;;; This can be bound to [C-down-mouse-1] to give behavior similar to Windows Explorer.
;;; However, Emacs generally uses [C-down-mouse-1] for `mouse-buffer-menu'.
;;;###autoload
(defun diredp-mouse-mark/unmark (event)
  "Mark/unmark file or directory at mouse EVENT."
  (interactive "e")
  (let* ((mouse-pos (event-start event))
         bol eol
         (file/dir-name
          (save-excursion
            (set-buffer (window-buffer (posn-window mouse-pos)))
            (save-excursion
              (goto-char (posn-point mouse-pos))
              (save-excursion
                (setq bol (progn (beginning-of-line) (point)))
                (setq eol (progn (end-of-line) (point))))
              (and (not (eobp)) (dired-get-filename nil t))))))
    ;; Return nil iff not on a file or directory name.
    (and file/dir-name (cond ((dired-file-marker file/dir-name)
                              (diredp-mouse-unmark event)
                              (message "Unmarked: %s" file/dir-name))
                             (t
                              (diredp-mouse-mark event)
                              (message "Marked: %s" file/dir-name))))))

;; This can be bound to [S-mouse-1] to give behavior similar to Windows Explorer.
;; If you do that, consider binding `diredp-mouse-mark/unmark' to `C-mouse-1'.
;; Alternatively, just bind `diredp-mouse-mark/unmark-mark-region-files' to [S-mouse-1].
;;;###autoload
(defun diredp-mouse-mark-region-files (event)
  "Mark files between point and the mouse."
  (interactive "e")
  (call-interactively 'mouse-save-then-kill)
  (diredp-mark-region-files))

;; This can be bound to [S-mouse-1] to give behavior similar to Windows Explorer.
;; If you don't bind `diredp-mouse-mark/unmark' to, for instance, `C-mouse-1', then
;; Consider binding this to [S-mouse-1].
;;;###autoload
(defun diredp-mouse-mark/unmark-mark-region-files (event)
  "Mark/unmark file or mark files in region.
If the file the cursor is on is marked, then mark all files between it
 and the line clicked (included).
Otherwise (cursor's file is unmarked):
 If the file clicked is marked, then unmark it.
 If it is unmarked, then mark it."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    ;; If same click same line as cursor, or cursor's line is marked,
    ;; Then toggle the clicked line's mark.
    ;; Else mark all files in region between point and clicked line (included).
    (if (or (eq (count-lines (point-min) (posn-point mouse-pos))
              (count-lines (point-min) (point)))
            (equal dired-marker-char (dired-file-marker (dired-get-filename nil t))))
        (diredp-mouse-mark/unmark event)
      (call-interactively 'mouse-save-then-kill)
      (diredp-mark-region-files))))

;;;###autoload
(defun diredp-mouse-flag-file-deletion (event)
  "In dired, flag this file for deletion.
If on a subdir headerline, mark all its files except `.' and `..'."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (let ((dired-marker-char dired-del-marker)) (dired-mark 1))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-copy (event)
  "In dired, copy this file.
This normally preserves the last-modified date when copying."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'copy (function dired-copy-file)
                         (if dired-copy-preserve-time "Copy [-p]" "Copy")
                         1 dired-keep-marker-copy))

;;;###autoload
(defun diredp-mouse-do-rename (event)
  "In dired, rename this file."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'move (function dired-rename-file)
                         "Move" 1 dired-keep-marker-rename "Rename"))

;;;###autoload
(defun diredp-mouse-upcase (event)
  "In dired, rename this file to upper case."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-rename-non-directory (function upcase) "Rename to uppercase:" nil))

;;;###autoload
(defun diredp-mouse-downcase (event)
  "In dired, rename this file to lower case."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-rename-non-directory (function downcase) "Rename to lowercase:" nil))

;;;###autoload
(defun diredp-mouse-do-delete (event)
  "In dired, delete this file, upon confirmation."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-internal-do-deletions (dired-map-over-marks (cons (dired-get-filename)
                                                           (point)) 1)
                               1)
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-shell-command (event)
  "Run a shell COMMAND on this file.
If there is output, it goes to a separate buffer.

No automatic redisplay of dired buffers is attempted, as there's no
telling what files the command may have changed.  Type
\\[dired-do-redisplay] to redisplay.

The shell command has the top level directory as working directory, so
output files usually are created there instead of in a subdir."
;;Functions dired-run-shell-command and dired-shell-stuff-it do the
;;actual work and can be redefined for customization.
  (interactive "e")
  (let ((mouse-pos (event-start event))
        (command   (dired-read-shell-command "! on %s: " nil
                                             (dired-get-marked-files t nil))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-bunch-files (- 10000 (length command))
                       (function (lambda (&rest files)
                                   (dired-run-shell-command
                                    (dired-shell-stuff-it command files t 1))))
                       nil
                       (dired-get-marked-files t 1))))

;;;###autoload
(defun diredp-mouse-do-symlink (event)
  "Make symbolic link to this file."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'symlink (function make-symbolic-link)
                         "Symlink" 1 dired-keep-marker-symlink))

;;;###autoload
(defun diredp-mouse-do-hardlink (event)
  "Make hard link (alias) to this file."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'hardlink (function add-name-to-file)
                         "Hardlink" 1 dired-keep-marker-hardlink))

;;;###autoload
(defun diredp-mouse-do-print (event)
  "Print this file.
Uses the shell command coming from variables `lpr-command' and
`lpr-switches' as default."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (let* ((file (dired-get-filename))
         (command (dired-mark-read-string
                   "Print %s with: "
                   (apply 'concat lpr-command " " lpr-switches)
                   'print 1 (list file))))
    (dired-run-shell-command (dired-shell-stuff-it command (list file) nil))))

;;;###autoload
(defun diredp-mouse-do-compress (event)
  "Compress or uncompress this file."
  (interactive "e")
  (let ((mouse-pos (event-start event))
        (dired-no-confirm t))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check (function dired-compress) 1 'compress t))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-byte-compile (event)
  "Byte compile this file."
  (interactive "e")
  (let ((mouse-pos (event-start event))
        (dired-no-confirm t))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check (function dired-byte-compile) 1 'byte-compile t))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-load (event)
  "Load this Emacs Lisp file."
  (interactive "e")
  (let ((mouse-pos (event-start event))
        (dired-no-confirm t))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check (function dired-load) 1 'load t))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-chmod (event)
  "Change the mode of this file.
This calls chmod, so symbolic modes like `g+w' are allowed."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-chxxx "Mode" "chmod" 'chmod 1)
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-chgrp (event)
  "Change the group of this file."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-chxxx "Group" "chgrp" 'chgrp 1)
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-chown (event)
  "Change the owner of this file."
  (interactive "e")
  (let ((mouse-pos (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-chxxx "Owner" dired-chown-program 'chown 1)
  (dired-previous-line 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired+.el ends here
