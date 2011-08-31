;;; ecb.el --- a code browser for Emacs

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2000

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

;; $Id: ecb.el,v 1.446 2009/05/10 16:53:19 berndl Exp $

;;; Commentary:
;;
;; ECB stands for "Emacs Code Browser".  While Emacs already has good
;; *editing* support for many modes, its *browsing* support is somewhat
;; lacking. That's where ECB comes in: it displays a number of informational
;; windows that allow for easy source code navigation and overview.
;;
;; The informational windows can contain:
;;
;; - A directory tree,
;; - a list of source files in the current directory,
;; - a list of functions/classes/methods/... in the current file, (ECB uses
;;   the Semantic Bovinator, or Imenu, or etags, for getting this list so all
;;   languages supported by any of these tools are automatically supported by
;;   ECB too)
;; - a history of recently visited files, 
;; - the Speedbar and
;; - output from compilation (the "*compilation*" window) and other modes like
;;   help, grep etc. or whatever a user defines to be displayed in this
;;   window.
;;
;; As an added bonus, ECB makes sure to keep these informational windows visible,
;; even when you use C-x 1 and similar commands.
;;
;; It goes without saying that you can configure the layout, ie which
;; informational windows should be displayed where. ECB comes with a number of
;; ready-made window layouts to choose from.
;;
;; Here is an ascii-screenshot of what ECB offers you:
;;
;;   ------------------------------------------------------------------
;;   |              |                                                 |
;;   | Directories  |                                                 |
;;   |              |                                                 |
;;   |--------------|                                                 |
;;   |              |                                                 |
;;   | Sources      |                                                 |
;;   |              |                                                 |
;;   |--------------|                   Edit-area                     |
;;   |              |    (can be splitted in several edit-windows)    |
;;   | Methods/Vars |                                                 |
;;   |              |                                                 |
;;   |--------------|                                                 |
;;   |              |                                                 |
;;   | History      |                                                 |
;;   |              |                                                 |
;;   ------------------------------------------------------------------
;;   |                                                                |
;;   |                 Compilation-window (optional)                  |
;;   |                                                                |
;;   ------------------------------------------------------------------
;;

;;; Installation
;;
;; To use the Emacs code browser add the ECB files to your load path and add the
;; following line to your .emacs file:
;;
;; If you want autoloading ECB after first start:
;;
;;    (require 'ecb-autoloads)
;;
;; or if you want loading the complete ECB:
;;
;;    (require 'ecb)
;;
;; Optional: You can byte-compile ECB with `ecb-byte-compile' after the
;;           ECB-package is loaded


;;; Requirements
;;
;; - Semantic, author-version between >= 1.4
;;   (http://cedet.sourceforge.net/semantic.shtml).
;; - Eieio, author-version >= 0.17
;;   (http://cedet.sourceforge.net/eieio.shtml).
;; - speedbar, author version >= 0.14beta1
;;   (http://cedet.sourceforge.net/speedbar.shtml)
;; - Optional: If Java code is edited the ECB works best when the JDEE package
;;   (http://sunsite.auc.dk/jde) is installed.


;;; Activation
;;
;; ECB is either activated and started by calling
;;    M-x ecb-activate
;; or
;;    via the menu "Tools --> Start Code Browser (ECB)"
;;
;; ECB can also be (de)activated/toggled by M-x ecb-minor-mode.
;;
;; After activating ECB you should call `ecb-show-help' to get a detailed
;; description of what ECB offers to you and how to use ECB.


;;; Availability
;;
;; The latest version of the ECB is available at http://ecb.sourceforge.net

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code:

(eval-when-compile
  (require 'silentcomp))


;; We need this libraries already here if we miss some requirements
(require 'ecb-upgrade)
(require 'ecb-util)

;; if we miss some of the requirements we report an error.

(defconst ecb-cedet-load-ok (featurep 'cedet))

(defconst ecb-compiled-in-semantic-version
  (eval-when-compile (ignore-errors semantic-version))
  "Semantic-version used for byte-compiling ECB. Either nil when no semantic
is loaded or the value of `semantic-version' at ECB-compilation time.")

(defconst ecb-compiled-in-cedet-version
  (eval-when-compile (ignore-errors cedet-version))
  "Cedet-version used for byte-compiling ECB. Either nil when no semantic
is loaded or the value of `cedet-version' at ECB-compilation time.")

(when (not ecb-cedet-load-ok)
  (if (ecb-noninteractive)
      (ecb-error "ECB is missing CEDET - check your CEDET-installation/setup!")
    (ecb-check-requirements)))


;; If we are here we can load ECB because at least we have installed and
;; loaded all required packages. The correct version will be checked
;; at start- or byte-compile-time


(message "ECB %s uses CEDET %s (contains semantic %s, eieio %s, speedbar %s)."
         ecb-version
         (or (and (boundp 'cedet-version)
                  cedet-version)
             "<unknown version>")
         (or (and (boundp 'semantic-version)
                  semantic-version)
             "<unknown version>")
         (or (and (boundp 'eieio-version)
                  eieio-version)
             "<unknown version>")
         (or (and (boundp 'speedbar-version)
                  speedbar-version)
             "<unknown version>"))

;; rest of ecb loads
(require 'tree-buffer)
(require 'ecb-file-browser)
(require 'ecb-method-browser)
(require 'ecb-jde)
(require 'ecb-layout)
(require 'ecb-create-layout)
(require 'ecb-mode-line)
(require 'ecb-help)
(require 'ecb-navigate)
(require 'ecb-eshell)
(require 'ecb-compilation)
(require 'ecb-cycle)
(require 'ecb-face)
(require 'ecb-tod)
(require 'ecb-speedbar)
(require 'ecb-autogen)
(require 'ecb-winman-support)
(require 'ecb-compatibility)

;; add-ons
(require 'ecb-analyse)
(require 'ecb-symboldef)

;; various loads
(require 'assoc)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))


;; XEmacs
(silentcomp-defun redraw-modeline)
(silentcomp-defvar modeline-map)
(silentcomp-defvar progress-feedback-use-echo-area)
;; Emacs
(silentcomp-defun force-mode-line-update)
(silentcomp-defun font-lock-add-keywords)

(silentcomp-defvar current-menubar)
(silentcomp-defun find-menu-item)
(silentcomp-defun add-submenu)
(silentcomp-defun delete-menu-item)
(silentcomp-defun Info-goto-node)

(silentcomp-defun ecb-speedbar-deactivate)
(silentcomp-defvar ecb-speedbar-buffer-name)


;;====================================================
;; Variables
;;====================================================
(defvar ecb-major-mode-selected-source nil
  "Major-mode of currently selected source.")

(defvar ecb-item-in-tree-buffer-selected nil
  "Only true if any item in any tree-buffer has been selected in recent
command.")

(defun ecb-initialize-all-internals (&optional no-caches)
  (ecb-tree-buffers-init)
  (setq ecb-major-mode-selected-source nil
        ecb-item-in-tree-buffer-selected nil)
  (ecb-file-browser-initialize no-caches)
  (ecb-method-browser-initialize no-caches))

;; Klaus Berndl <klaus.berndl@sdm.de>: FRAME-LOCAL
(defvar ecb-minor-mode nil
  "Do not set this variable directly. Use `ecb-activate' and
`ecb-deactivate' or `ecb-minor-mode'.!")

(defvar ecb-activated-window-configuration nil
  "Window configuration used after the ECB is activated.")

;;====================================================
;; Customization
;;====================================================

(defgroup ecb nil
  "Emacs code browser."
  :group 'tools
  :prefix "ecb-")

(defgroup ecb-general nil
  "General settings for the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-most-important nil
  "The most important settings of ECB you should know."
  :group 'ecb
  :prefix "ecb-")

(defcustom ecb-use-recursive-edit nil
  "*Tell ECB to use a recursive edit.
If set then it can easily be deactivated by \(keyboard-escape-quit)."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-auto-activate nil
  "*Automatically startup ECB when Emacs starts up.
This should only be true if you always want to run `ecb-activate'."
  :group 'ecb-general
  :group 'ecb-most-important
  :type 'boolean)

(defcustom ecb-activation-selects-ecb-frame-if-already-active 'ask
  "*Trying to activate an already activated ECB selects the ECB-frame.
If t then the ECB-frame is selected, if nil then it is not. If 'ask then ECB
asks if the ECB-frame should be selected if the current-frame is not the
`ecb-frame'."
  :group 'ecb-general
  :type '(radio (const :tag "Select the ECB-frame" :value t)
                (const :tag "Ask if the ECB-frame should be selected" :value ask)
                (const :tag "Do not select the ECB-frame" :value nil)))

(defcustom ecb-clear-caches-before-activate nil
  "*Clear all ECB internal caches before startup.
If t then ECB clears all its internal caches before starting up. Caches are
used for files- and subdirs \(see `ecb-cache-directory-contents' and
`ecb-cache-directory-contents-not') for semantic-tags and for the
history-filter.

This caches are completely empty at load-time of the ECB-library!

Default is nil, because is makes sense not to clear these caches at start-time
because ECB is often deacticated temporally especially in combination with
window-managers like escreen.el. In these situations the internal state of ECB
should be preserved for next activation."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-stealthy-tasks-delay 1
  "*Time Emacs must be idle before ECB runs its stealthy tasks.
Currently ECB performes the following stealthy tasks:

  Prescann directories for emptyness: Prescann directories and display them as
  empty or not-empty in the directories-buffer. See the documentation of the
  option `ecb-prescan-directories-for-emptyness' for a description.

  File is read only: Check if sourcefile-items of the directories- or
  sources-buffer are read-only or not. See documentation of the option
  `ecb-sources-perform-read-only-check'.

  Version-control-state: Checks the version-control-state of files in
  directories which are managed by a VC-backend. See the option
  `ecb-vc-enable-support'.

Here the interval is defined ECB has to be idle before starting with these
stealthy tasks. It can be a floating-point value in seconds. The value can
also be changed during running ECB."
  :group 'ecb-general
  :group 'ecb-most-important
  :type '(number :tag "Idle time before running stealthy tasks"
                 :value 1)
  :initialize 'custom-initialize-default
  :set (function (lambda (sym val)
                   (set sym val)
                   (ecb-activate-ecb-autocontrol-function
                    val 'ecb-stealthy-updates))))
                    


(defcustom ecb-minor-mode-text " ECB"
  "*String to display in the mode line when ECB minor mode is active.
\(When the string is not empty, make sure that it has a leading space.)

Because for ECB it is quite obvious if it is active or not when the
ECB-windows are visible this text is only display in the modeline if the
ECB-windows are hidden."
  :group 'ecb-general
  :type 'string)

(defcustom ecb-auto-compatibility-check t
  "*Check at ECB-startup if all ECB-options have correct values.
If not nil then all ECB-options are checked if their current value have the
correct type. It the type is incorrect the option is either auto. upgraded to
the new type or reset to the default-value of current ECB if no upgrade is
possible. This feature can also upgrade options which are renamed in current
ECB and try to transform the old-value to the new named option. After startup
all upgraded or reset options are displayed with their old \(before
upgrade/reset) and new values. See also the commands `ecb-upgrade-options' and
`ecb-display-upgraded-options'. If this option is off then the user can
perform the check and reset manually with `ecb-upgrade-options'."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-version-check t
  "*Checks at start-time if the requirements are fulfilled.
It checks if the required versio of CEDET is installed and loaded into Emacs.

It is strongly recommended to set this option to not nil!"
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-debug-mode nil
  "*If not nil ECB displays debug-information in the Messages-buffer.
This is done for some critical situations concerning semantic-tags and their
overlays \(or extends for XEmacs). Normally you should not need this switched
on! But if you get errors like \"destroyed extend\" for XEmacs or
\"wrong-argument-type\" concerning overlays for GNU Emacs then you should
switch on this option and submitting a bug-report to the ecb-mailing-list
\(`ecb-submit-problem-report') after getting the error again!"
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-run-ediff-in-ecb-frame t
  "*Run ediff-sessions in the same frame as ECB is running.
If not nil then ECB ensures that ediff runs in the same frame as ECB and ECB
restores exactly the \"before-ediff\"-window-layout after quiting ediff. If
nil then ediff decides in which frame it will run - depending on the current
window-layout \(e.g. if the ecb-windows are currently hidden) this can be the
ecb-frame but this can also be a newly created frame or any other frame."
  :group 'ecb-general
  :type 'boolean)


(defcustom ecb-activate-before-layout-draw-hook nil
  "*Hook run at the end of activating ECB by `ecb-activate'.
These hooks run after all the internal setup process but directly before\(!)
drawing the layout specified in `ecb-layout' \(means before dividing the frame
into several windows). A senseful using of this hook can be maximizing the
Emacs-frame for example, because this should be done before the layout is
drawn because ECB computes the size of the ECB-windows with the current frame
size! If you need a hook-option for the real end of the activating process
\(i.e. after the layout-drawing) look at `ecb-activate-hook'.

IMPORTANT: The difference between this hook and
`ecb-redraw-layout-before-hook' is that the latter one is evaluated always
before the layout is redrawn \(for example after calling `ecb-redraw-layout')
whereas the former one \(this hook) is only evaluated exactly once during the
activation-process of ECB. So during the activation process there is the
following sequence of hooks:
1. 'ecb-activate-before-layout-draw-hook' \(this one)
2. `ecb-redraw-layout-before-hook'
3. <Drawing the layout>
4. `ecb-redraw-layout-after-hook'
5. `ecb-activate-hook'"
  :group 'ecb-general
  :type 'hook)


(defcustom ecb-before-activate-hook nil
  "*Hook run at the beginning of activating ECB by `ecb-activate'.
These hooks run before any other tasks of the activating process are
performed. If any of these hooks returns nil then ECB will not be activated!

This can be used to check some conditions and then only start ECB if all
conditions are true. For example a function could be added which returns only
nil if Gnus is running. Then calling `ecb-activate' or `ecb-minor-mode' will
only start ECB if Gnus is not already running."
  :group 'ecb-general
  :type 'hook)


(defcustom ecb-activate-hook nil
  "*Hook run at the end of activating ECB by `ecb-activate'.
These hooks run at the real end of the activating process, means after the
layout has been drawn!. If you need hooks which are run direct before the
layout-drawing look at `ecb-activate-before-layout-draw-hook'."
  :group 'ecb-general
  :type 'hook)

(defcustom ecb-deactivate-hook nil
  "*Hook run at the end of deactivating ECB by `ecb-deactivate'.
These hooks run before the ecb-layout is cleared!"
  :group 'ecb-general
  :type 'hook)

(defcustom ecb-before-deactivate-hook nil
  "*Hook run at the beginning of deactivating ECB by `ecb-deactivate'.
These hooks run before any other tasks of the deactivating process are
performed. If any of these hooks returns nil then ECB will not be deactivated!
See also `ecb-before-activate-hook'."
  :group 'ecb-general
  :type 'hook)


;;====================================================
;; Internals
;;====================================================

(defun ecb-kill-buffer-hook ()
  "Function added to the `kill-buffer-hook' during ECB activation.
  It does several tasks:
- Depending on the value in `ecb-kill-buffer-clears-history' the corresponding
  entry in the history-buffer is removed.
- Clearing the method buffer if a file-buffer has been killed.
- The entry of the removed file-buffer is removed from `ecb-tag-tree-cache'."
  (let* ((curr-buf (current-buffer))
         (buffer-file (ecb-fix-filename (ecb-buffer-file-name curr-buf))))
    ;; this prevents automatically from killing one of the ecb-buffers because
    ;; these ones are never releated to file!
    (when buffer-file
      ;; 1. clearing the history if necessary
      (ecb-history-kill-buffer-clear curr-buf)

      ;; 2. clearing the method buffer if a file-buffer is killed
      (ecb-rebuild-methods-buffer-with-tagcache nil nil t)

      ;; 3. removing the file-buffer from `ecb-tag-tree-cache'. Must be done
      ;;    after 2. because otherwise a new element in the cache would be
      ;;    created again by `ecb-rebuild-methods-buffer-with-tagcache'.
      (ecb-clear-tag-tree-cache (buffer-name curr-buf)))
    (when (member curr-buf (ecb-get-current-visible-ecb-buffers))
      (ecb-error "Killing an special ECB-buffer is not possible!"))))


(defun ecb-window-sync (&optional only-basic-windows)
  "Synchronizes all special ECB-buffers with current buffer.
Depending on the contents of current buffer this command performs different
synchronizing tasks but only if ECB is active and point stays in an
edit-window.

- If current buffer is a file-buffer \(or an indirect-buffer with a
  file-buffer as base-buffer) then all special ECB-tree-buffers are
  synchronized with current buffer.

- If current buffer is a dired-buffer then the directory- and
  the sources-tree-buffer are synchronized if visible

In addition to this the hooks in `ecb-basic-buffer-sync-hook' run.

If optional argument only-basic-windows is not nil then only the basic
tree-buffer directories, sources, methods and history are synchronized.
Otherwise all registered special ecb-buffers."
  (interactive "P")
  (when (and ecb-minor-mode
             (not ecb-windows-hidden)
             (ecb-point-in-edit-window-number))
    (ecb-basic-buffer-sync t)
    ;; look in the sync-register and call all sync-function registered with a
    ;; buffer-name when buffer is contained in the list of buffers returned by
    ;; ecb-get-current-visible-ecb-buffers.
    (unless only-basic-windows
      (let ((visible-ecb-windows (ecb-get-current-visible-ecb-buffers)))
        (dolist (elem ecb-autocontrol/sync-fcn-register)
          (when (and (cdr elem)
                     (member (get-buffer (symbol-value (cdr elem)))
                             visible-ecb-windows))
            (funcall (car elem) t)))))
    ))


(defun ecb-customize ()
  "Open a customize-buffer for all customize-groups of ECB."
  (interactive)
  (ecb-select-edit-window)
  (customize-group "ecb"))

(defun ecb-customize-most-important ()
  "Open a customize-buffer for the most important options of ECB."
  (interactive)
  (ecb-select-edit-window)
  (customize-group "ecb-most-important"))


;;====================================================
;; ECB minor mode: Create buffers & menus & maps
;;====================================================

(defun ecb-menu-item (item)
  "Build an XEmacs compatible menu item from vector ITEM.
That is remove the unsupported :help stuff."
  (if ecb-running-xemacs
      (let ((n (length item))
            (i 0)
            slot l)
        (while (< i n)
          (setq slot (aref item i))
          (if (and (keywordp slot)
                   (eq slot :help))
              (setq i (1+ i))
            (setq l (cons slot l)))
          (setq i (1+ i)))
        (apply #'vector (nreverse l)))
    item))

(defvar ecb-menu-name "ECB")
(defvar ecb-menu-bar
  (list
   ecb-menu-name
   (ecb-menu-item
    [ "Select ECB frame"
      ecb-select-ecb-frame
      :active (and ecb-minor-mode
                   (not (equal (selected-frame) ecb-frame)))
      :help "Select the ECB-frame."
      ])
   (ecb-menu-item
    [ "Synchronize ECB windows"
      (ecb-window-sync)
      :active (and (equal (selected-frame) ecb-frame)
                   (ecb-point-in-edit-window-number))
      :help "Synchronize the ECB windows with the current edit-window."
      ])
   (ecb-menu-item
    [ "Update directories buffer"
      ecb-update-directories-buffer
      :active (equal (selected-frame) ecb-frame)
      :help "Updates the directories buffer with current disk-state"
      ])
   (ecb-menu-item
    [ "Add all buffers to history"
      ecb-add-all-buffers-to-history
      :active (and (equal (selected-frame) ecb-frame)
                   (ecb-window-live-p ecb-history-buffer-name))
      :help "Add all current file-buffers to history"
      ])
   "-"
   (ecb-menu-item
    [ "Rebuild methods buffer"
      ecb-rebuild-methods-buffer
      :active (equal (selected-frame) ecb-frame)
      :help "Rebuild the methods buffer completely"
      ])
   (ecb-menu-item
    [ "Expand methods buffer"
      ecb-expand-methods-nodes
      :active (equal (selected-frame) ecb-frame)
      :help "Expand all nodes of a certain indent-level"
      ])
   (ecb-menu-item
    [ "Toggle auto. expanding of the method buffer"
      ecb-toggle-auto-expand-tag-tree
      :active (equal (selected-frame) ecb-frame)
      :help "Toggle auto. expanding of the method buffer"
      ])
   "-"
   (ecb-menu-item
    [ "Change layout"
      ecb-change-layout
      :active (equal (selected-frame) ecb-frame)
      :help "Change the layout."
      ])
   (ecb-menu-item
    [ "Redraw layout"
      ecb-redraw-layout
      :active (equal (selected-frame) ecb-frame)
      :help "Redraw the current layout."
      ])
   (ecb-menu-item
    [ "Toggle layout"
      ecb-toggle-layout
      :active (and (equal (selected-frame) ecb-frame)
                   (> (length ecb-toggle-layout-sequence) 1))
      :help "Toggle between several layouts"
      ])
   (ecb-menu-item
    [ "Toggle visibility of ECB windows"
      ecb-toggle-ecb-windows
      :active (equal (selected-frame) ecb-frame)
      :help "Toggle the visibility of all ECB windows."
      ])
   (list
    "Layout administration"
    (ecb-menu-item
     [ "Store current window-sizes"
       ecb-store-window-sizes
       :active (equal (selected-frame) ecb-frame)
       :help "Store current sizes of the ecb-windows in current layout."
       ])
    (ecb-menu-item
     [ "Restore sizes of the ecb-windows"
       ecb-restore-window-sizes
       :active (equal (selected-frame) ecb-frame)
       :help "Restore the sizes of the ecb-windows in current layout."
       ])
    (ecb-menu-item
     [ "Restore default-sizes of the ecb-windows"
       ecb-restore-default-window-sizes
       :active (equal (selected-frame) ecb-frame)
       :help "Restore the default-sizes of the ecb-windows in current layout."
       ])
    "-"
    (ecb-menu-item
     [ "Create new layout"
       ecb-create-new-layout
       :active (equal (selected-frame) ecb-frame)
       :help "Create a new ECB-layout."
       ])
    (ecb-menu-item
     [ "Delete new layout"
       ecb-delete-new-layout
       :active (equal (selected-frame) ecb-frame)
       :help "Delete an user-created ECB-layout."
       ])
    "-"
    (ecb-menu-item
     [ "Show help for a layout"
       ecb-show-layout-help
       :active t
       :help "Show the documentation for a layout."
       ]))
   "-"
   (ecb-menu-item
    [ "Toggle compile window"
      ecb-toggle-compile-window
      :active (equal (selected-frame) ecb-frame)
      :help "Toggle visibility of compile window."
      ])
   (ecb-menu-item
    [ "Toggle enlarged compile window"
      ecb-toggle-compile-window-height
      :active (and (equal (selected-frame) ecb-frame)
                   ecb-compile-window
                   (ecb-compile-window-live-p))
      :help "Toggle enlarged compile window."
      ])
   "-"
   (list
    "Navigate"
    (ecb-menu-item
     ["Previous \(back)"
      ecb-nav-goto-previous
      :active t
      :help "Go to the previous navigation point"
      ])
    (ecb-menu-item
     ["Next \(forward)"
      ecb-nav-goto-next
      :active t
      :help "Go to the next navigation point"
      ]))
   (list
    "Goto window"
    (ecb-menu-item
     ["Last selected edit-window"
      ecb-goto-window-edit-last
      :active t
      :help "Go to the last selected edit-window"
      ])
    (ecb-menu-item
     ["Edit-window 1"
      ecb-goto-window-edit1
      :active t
      :help "Go to the first edit-window"
      ])
    (ecb-menu-item
     ["Edit-window 2"
      ecb-goto-window-edit2
      :active (ecb-edit-window-splitted)
      :help "Go to the second edit-window \(if splitted\)"
      ])
    (ecb-menu-item
     ["Directories"
      ecb-goto-window-directories
      :active (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-directories-buffer-name)
      :help "Go to the directories window"
      ])
    (ecb-menu-item
     ["Sources"
      ecb-goto-window-sources
      :active (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-sources-buffer-name)
      :help "Go to the sources window"
      ])
    (ecb-menu-item
     ["Methods and Variables"
      ecb-goto-window-methods
      :active (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-methods-buffer-name)
      :help "Go to the methods/variables window"
      ])
    (ecb-menu-item
     ["History"
      ecb-goto-window-history
      :active (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-history-buffer-name)
      :help "Go to the history window"
      ])
    (ecb-menu-item
     ["Analyse"
      ecb-goto-window-analyse
      :active (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-analyse-buffer-name)
      :help "Go to the analyse window"
      ])
    (ecb-menu-item
     ["Speedbar"
      ecb-goto-window-speedbar
      :active (and ecb-use-speedbar-instead-native-tree-buffer
                   (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-speedbar-buffer-name))
      :help "Go to the integrated speedbar window"
      ])
    (ecb-menu-item
     ["Compilation"
      ecb-goto-window-compilation
      :active (equal 'visible (ecb-compile-window-state))
      :help "Go to the history window"
      ])
    )
   (list
    "Display window maximized"
    (ecb-menu-item
     ["Directories"
      ecb-maximize-window-directories
      :active (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-directories-buffer-name)
      :help "Maximize the directories window - even if currently not visible"
      ])
    (ecb-menu-item
     ["Sources"
      ecb-maximize-window-sources
      :active (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-sources-buffer-name)
      :help "Maximize the sources window - even if currently not visible"
      ])
    (ecb-menu-item
     ["Methods and Variables"
      ecb-maximize-window-methods
      :active (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-methods-buffer-name)
      :help "Maximize the methods/variables window - even if currently not visible"
      ])
    (ecb-menu-item
     ["History"
      ecb-maximize-window-history
      :active (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-history-buffer-name)
      :help "Maximize the history window - even if currently not visible"
      ])
    (ecb-menu-item
     ["Analyse"
      ecb-maximize-window-analyse
      :active (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-analyse-buffer-name)
      :help "Maximize the analyse window - even if currently not visible"
      ])
    (ecb-menu-item
     ["Speedbar"
      ecb-maximize-window-speedbar
      :active (and ecb-use-speedbar-instead-native-tree-buffer
                   (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-speedbar-buffer-name))
      :help "Maximize the integrated speedbar window - even if not visible"
      ])
    )
   "-"
   (list
    "Preferences"
    (ecb-menu-item
     ["Most important..."
      (customize-group "ecb-most-important")
      :active t
      :help "Customize the most important options"
      ])
    (ecb-menu-item
     ["All..."
      (ecb-customize)
      :active t
      :help "Display all available option-groups..."
      ])
    "-"
    (ecb-menu-item
     ["General..."
      (customize-group "ecb-general")
      :active t
      :help "Customize general ECB options"
      ])
    (ecb-menu-item
     ["Directories..."
      (customize-group "ecb-directories")
      :active t
      :help "Customize ECB directories"
      ])
    (ecb-menu-item
     ["Sources..."
      (customize-group "ecb-sources")
      :active t
      :help "Customize ECB sources"
      ])
    (ecb-menu-item
     ["Methods..."
      (customize-group "ecb-methods")
      :active t
      :help "Customize ECB method display"
      ])
    (ecb-menu-item
     ["History..."
      (customize-group "ecb-history")
      :active t
      :help "Customize ECB history"
      ])
    (ecb-menu-item
     ["Analyse..."
      (customize-group "ecb-analyse")
      :active t
      :help "Customize ECB analyse ingeractor"
      ])
    (ecb-menu-item
     ["Version control..."
      (customize-group "ecb-version-control")
      :active t
      :help "Customize the version-control-support"
      ])
    (ecb-menu-item
     ["Layout..."
      (customize-group "ecb-layout")
      :active t
      :help "Customize ECB layout"
      ])
    (ecb-menu-item
     ["Tree-buffer style and handling..."
      (customize-group "ecb-tree-buffer")
      :active t
      :help "Customize the tree-buffers of ECB"
      ])
    (ecb-menu-item
     ["Face options..."
      (customize-group "ecb-face-options")
      :active t
      :help "Customize ECB faces"
      ])
    (ecb-menu-item
     ["Help options..."
      (customize-group "ecb-help")
      :active t
      :help "Customize options for the online help of ECB"
      ])
    (ecb-menu-item
     ["ECB/eshell options..."
      (customize-group "ecb-eshell")
      :active t
      :help "Customize options for the eshell integration of ECB"
      ])
    (ecb-menu-item
     ["Supporting non-semantic-sources..."
      (customize-group "ecb-non-semantic")
      :active t
      :help "Customize options for parsing non-semantic-sources"
      ])
    (ecb-menu-item
     ["Supporting window-managers..."
      (customize-group "ecb-winman-support")
      :active t
      :help "Customize options for the window-manager-support"
      ])
    )
   (list
    "Upgrade ECB"
    (ecb-menu-item
     [ "Upgrade ECB-options to current ECB-version"
       ecb-upgrade-options
       :active (equal (selected-frame) ecb-frame)
       :help "Try to upgrade ECB-options to current ECB-version if necessary."
       ])
    )
   (list
    "Help"
    (ecb-menu-item
     [ "Show Online Help"
       ecb-show-help
       :active t
       :help "Show the online help of ECB."
       ])
    (ecb-menu-item
     [ "ECB NEWS"
       (ecb-display-news-for-upgrade t)
       :active t
       :help "Displays the NEWS-file of ECB."
       ])
    (ecb-menu-item
     [ "List of most important options"
       (let ((ecb-show-help-format 'info))
         (ecb-show-help)
         (Info-goto-node "Most important options"))
       :active t
       :help "Displays a a list of options which you should know."
       ])
    (ecb-menu-item
     [ "List of all options"
       (let ((ecb-show-help-format 'info))
         (ecb-show-help)
         (Info-goto-node "Option Index"))
       :active t
       :help "Displays an index of all user-options in the online-help."
       ])
    (ecb-menu-item
     [ "List of all commands"
       (let ((ecb-show-help-format 'info))
         (ecb-show-help)
         (Info-goto-node "Command Index"))
       :active t
       :help "Displays an index of all commands in the online-help."
       ])
    (ecb-menu-item
     [ "FAQ"
       (let ((ecb-show-help-format 'info))
         (ecb-show-help)
         (Info-goto-node "FAQ"))
       :active t
       :help "Show the FAQ of ECB."
       ])
    (ecb-menu-item
     [ "Conflicts with other packages"
       (let ((ecb-show-help-format 'info))
         (ecb-show-help)
         (Info-goto-node "Conflicts and bugs"))
       :active t
       :help "What to do for conflicts with other packages."
       ])
    (ecb-menu-item
     [ "Submit problem report"
       ecb-submit-problem-report
       :active t
       :help "Submit a problem report to the ECB mailing list."
       ])
    (ecb-menu-item
     [ "ECB Debug mode"
       (setq ecb-debug-mode (not ecb-debug-mode))
       :active t
       :style toggle
       :selected ecb-debug-mode
       :help "Print debug-informations about parsing files in the message buffer."
       ])
    (ecb-menu-item
     [ "ECB Layout Debug mode"
       (setq ecb-layout-debug-mode (not ecb-layout-debug-mode))
       :active t
       :style toggle
       :selected ecb-layout-debug-mode
       :help "Print debug-informations about window-operations in the message buffer."
       ])
    "-"
    (ecb-menu-item
     ["Help preferences..."
      (customize-group "ecb-help")
      :active t
      :help "Customize options for the online help of ECB"
      ])
    "-"
    (concat "ECB " ecb-version)
    )
   "-"
   (ecb-menu-item
    [ "Deactivate ECB"
      ecb-deactivate
      :active t
      :help "Deactivate ECB."
      ])
   )
  "Menu for ECB minor mode.")

(defun ecb-add-to-minor-modes ()
  "Does all necessary to add ECB as a minor mode with current values of
`ecb-mode-map' and `ecb-minor-mode-text'."
  ;; ECB minor mode doesn't work w/ Desktop restore.
  ;; This line will disable this minor mode from being restored
  ;; by Desktop.
  (when (boundp 'desktop-minor-mode-handlers)
    (add-to-list 'desktop-minor-mode-handlers
		 (cons 'ecb-minor-mode 'ignore)))
  (add-minor-mode 'ecb-minor-mode
                  'ecb-minor-mode-text ecb-mode-map))

(defvar ecb-mode-map nil
  "Internal key-map for ECB minor mode.")

(defcustom ecb-key-map
  '("C-c ." . ((t "fh" ecb-history-filter)
               (t "fs" ecb-sources-filter)
               (t "fm" ecb-methods-filter)
               (t "fr" ecb-methods-filter-regexp)
               (t "ft" ecb-methods-filter-tagclass)
               (t "fc" ecb-methods-filter-current-type)
               (t "fp" ecb-methods-filter-protection)
               (t "fn" ecb-methods-filter-nofilter)
               (t "fl" ecb-methods-filter-delete-last)
               (t "ff" ecb-methods-filter-function)
               (t "p" ecb-nav-goto-previous)
               (t "n" ecb-nav-goto-next)
               (t "lc" ecb-change-layout)
               (t "lr" ecb-redraw-layout)
               (t "lw" ecb-toggle-ecb-windows)
               (t "lt" ecb-toggle-layout)
               (t "s" ecb-window-sync)
               (t "r" ecb-rebuild-methods-buffer)
               (t "a" ecb-toggle-auto-expand-tag-tree)
               (t "x" ecb-expand-methods-nodes)
               (t "h" ecb-show-help)
               (t "gl" ecb-goto-window-edit-last)
               (t "g1" ecb-goto-window-edit1)
               (t "g2" ecb-goto-window-edit2)
               (t "gc" ecb-goto-window-compilation)
               (t "gd" ecb-goto-window-directories)
               (t "gs" ecb-goto-window-sources)
               (t "gm" ecb-goto-window-methods)
               (t "gh" ecb-goto-window-history)
               (t "ga" ecb-goto-window-analyse)
               (t "gb" ecb-goto-window-speedbar)
               (t "md" ecb-maximize-window-directories)
               (t "ms" ecb-maximize-window-sources)
               (t "mm" ecb-maximize-window-methods)
               (t "mh" ecb-maximize-window-history)
               (t "ma" ecb-maximize-window-analyse)
               (t "mb" ecb-maximize-window-speedbar)
               (t "e" eshell)
               (t "o" ecb-toggle-scroll-other-window-scrolls-compile)
               (t "\\" ecb-toggle-compile-window)
               (t "/" ecb-toggle-compile-window-height)
               (t "," ecb-cycle-maximized-ecb-buffers)
               (t "." ecb-cycle-through-compilation-buffers)))

  "*Specifies all key-bindings for the ECB minor-mode key-map.
The value is a cons-cell where the car is a common-prefix key for all the
key-bindings. The cdr is a list of key-bindings each of them a list again. A
key-binding has the following form:

  '\(<common-prefix-flag> <keysequence> <function>) where

<common-prefix-flag>: If t then the common-prefixkey defined as car of the
                      value \(see above) is used.
<keysequence>: If the common prefixkey is used then the final key-binding is the
               concatenation of the common-prefixkey \(see above) and this
               keysequence.
<function>: The function to bind to the key. This can also be a
            lambda-expression .

It is highly recommended to use one of the standard keys C-c or C-x as first key
of your common-prefixkey!

You MUST change this option via customize to take effect!

All keysequences must be inserted as a string and must follow the syntax needed
by `read-kbd-macro' or `kbd'. This means you can insert the key in the same
manner \"C-h k\" displays keysequences. Here is the summary of the syntax:

Text is divided into \"words \" separated by whitespace. Except for the words
described below, the characters of each word go directly as characters of the
keysequence. The whitespace that separates words is ignored. Whitespace in the
macro must be written explicitly, as in \"C-c SPC\".

  * The special words RET, SPC, TAB, DEL, LFD, ESC, and NUL represent special
   control characters. The words must be written in uppercase.

  * A word in angle brackets, e.g., <return>, <down>, <left> or <f1>, represents
    a function key. \(Note that in the standard configuration, the function
    key <return> and the control key RET are synonymous.). You can use angle
    brackets on the words RET, SPC, etc., but they are not required there.

  * Keys can be written by their ASCII code, using a backslash followed by up
    to six octal digits. This is the only way to represent keys with codes
    above \377.

  * One or more prefixes M- \(meta), C- \(control), S- \(shift), A- \(alt),
    H- \(hyper), and s- \(super) may precede a character or key notation. For
    function keys, the prefixes may go inside or outside of the brackets:
    C-<down> = <C-down>. The prefixes may be written in any order: M-C-x =
    C-M-x.
    Prefixes are not allowed on multi-key words, e.g., C-abc, except that the
    Meta prefix is allowed on a sequence of digits and optional minus sign:
    M--123 = M-- M-1 M-2 M-3.

  * The `^' notation for control characters also works:  ^M = C-m."
  :group 'ecb-general
  :group 'ecb-most-important
  :type '(cons (choice :tag "Common prefix-key"
                       (const :tag "No common prefix-key" :value nil)
                       (string :tag "Prefix-key" :value "C-c ."))
               (repeat :tag "Key-bindings"
                       (list :tag "Key-definition"
                             (boolean :tag "o Use common prefix-key" :value t)
                             (string :tag "o Key")
                             (function :tag "o Function or lambda-expression"
                                       :value nil))))
  :set (function (lambda (symbol value)
                   (set symbol value)
                   ;; make a mode-map and save it
                   (setq ecb-mode-map
                         (let ((km (make-sparse-keymap))
                               (val-list (ecb-copy-list (cdr value)))
                               keq-string)
                           (dolist (elem val-list)
                             (setq key-string (concat (if (nth 0 elem) (car value))
                                                      " " (nth 1 elem)))
                             (define-key km (read-kbd-macro key-string) (nth 2 elem)))
                           (easy-menu-define ecb-minor-menu km
                             "ECB Minor Mode Menu" ecb-menu-bar)
                           km))
                   ;; add the minor-mode and and the minor-mode-map to the
                   ;; alists if not already contained. In this case just
                   ;; replace the values in the alists
                   (ecb-add-to-minor-modes))))

;;;###autoload
(defun ecb-activate ()
  "Activates ECB and creates the special buffers for the choosen layout.
For the layout see `ecb-layout-name'. This function raises always the
ECB-frame if called from another frame. This is the same as calling
`ecb-minor-mode' with a positive argument."
  (interactive)
  (ecb-minor-mode 1))

(defun ecb-activate-internal ()
  "Activates the ECB and creates all the buffers and draws the ECB-screen
with the actually chosen layout \(see `ecb-layout-name'). This function raises
always the ECB-frame if called from another frame."

  (if ecb-use-recursive-edit
      (if ecb-minor-mode
	  (progn
	    (message "ECB already activated. Drawing layout.")
	    (ecb-redraw-layout))
	(catch 'exit
	  (progn
	    (ecb-activate--impl)
	    (recursive-edit))
	  (ecb-deactivate-internal)))
    (ecb-activate--impl))
  ecb-minor-mode)


(defvar ecb-upgrade-check-done nil)

(defun ecb-clean-up-after-activation-failure (msg err)
  "Complete cleanup of all ECB-setups and report an error with message MSG."
  (let ((ecb-minor-mode t))
    (ecb-deactivate-internal t)
    (if ecb-running-xemacs
        (redraw-modeline t)
      (force-mode-line-update t))
    (error "ECB %s: %s (error-type: %S, error-data: %S)" ecb-version msg
           (car err) (cdr err))))

(defvar ecb-last-window-config-before-deactivation nil
  "Contains the last `ecb-current-window-configuration' directly before
ECB has been deactivated. Do not set this variable!")

(defvar ecb-temporary-changed-emacs-variables-alist nil
  "Internal alist which stores old values of emacs variables/options which
have to be changed during running ECB. Use only `ecb-modify-emacs-variable'
for modifying this alist.")

(defun ecb-modify-emacs-variable (var action &optional new-value)
  "Stores or restores the old value of the Emacs-variable symbol VAR.
VAR has to be a bound symbol for a variable. ACTION is either 'store or
'restore. The optional arg NEW-VALUE is only used when ACTION is 'store and is
that value VAR should be set to. After calling with ACTION is 'restore the
value of VAR is as before storing a NEW-VALUE for variable-symbol VAR."
  (case action
    (store
     (or (ecb-find-assoc var ecb-temporary-changed-emacs-variables-alist)
         (progn
           (setq ecb-temporary-changed-emacs-variables-alist
                 (ecb-add-assoc (cons var (symbol-value var))
                                ecb-temporary-changed-emacs-variables-alist))
           (set var new-value))))
    (restore
     (let ((elem (ecb-find-assoc var ecb-temporary-changed-emacs-variables-alist)))
       (when elem
         (set var (cdr elem))
         (setq ecb-temporary-changed-emacs-variables-alist
               (ecb-remove-assoc var ecb-temporary-changed-emacs-variables-alist)))))))

(defun ecb-check-semantic-load ()
  "Checks if cedet is correctly loaded if semantic 2.X is used and if the same
semantic-version has been used for byte-compiling ECB and loading into Emacs.
If ECB detects a problem it is reported and then an error is thrown."
  (when (boundp 'semantic-version)
    (let ((err-msg
           (cond ;; Different semantic-version used for byte-compiling ECB and
                 ;; loading into Emacs.
                 ((not (string= semantic-version ecb-compiled-in-semantic-version))
                  (concat "ECB has been byte-compiled with another semantic-version than currently\n"
                          "loaded into Emacs:\n"
                          (format "  + Semantic used for byte-compiling ECB: %s\n"
                                  ecb-compiled-in-semantic-version)
                          (format "  + Semantic currently loaded into Emacs: %s\n"
                                  semantic-version)
                          "Please ensure that ECB is byte-compiled with the same semantic-version as you\n"
                          "you load into your Emacs. Check if you have byte-compiled ECB with the cedet-\n"
                          "suite but loaded old semantic 1.X into Emacs or vice versa.\n\n"
                          "In general it is recommended to start ECB first-time not byte-compiled\n"
                          "and then call the command `ecb-byte-compile'. This ensures you byte-compile ECB\n"
                          "with the same library-versions \(semantic etc.) as you load into Emacs.\n"
                          "If you use the Makefile check the variables CEDET before compiling!\n"
                          ))
                 (t ""))))
      (unless (= 0 (length err-msg)) 
        (with-output-to-temp-buffer "*ECB semantic-load problems*"
          (princ "Currently ECB can not be activated cause of the following reason:\n\n")
          (princ err-msg)
          (princ "\n\nPlease fix the reported problem and restart Emacs\n"))
        (ecb-error "Please fix the reported problem and restart Emacs!")))))

(defun ecb-check-cedet-load ()
  "Checks if cedet is correctly loaded if semantic 2.X is used and if the same
semantic-version has been used for byte-compiling ECB and loading into Emacs.
If ECB detects a problem it is reported and then an error is thrown."
  (when (boundp 'cedet-version)
    (let ((err-msg
           (cond ;; cedet was not compiled into ECB
                 ((null ecb-compiled-in-cedet-version)
                  (concat (format "Currently CEDET %s is loaded but ECB has been byte-compiled without\n"
                                  cedet-version)
                          "any CEDET. Please either use ECB un-byte-compiled \(remove all *.elc\n"
                          "files from the ECB-directory) or byte-compile ECB correctly with CEDET!\n"
                          "In the later case it is recommended to start ECB first-time not byte-compiled\n"
                          "and then call the command `ecb-byte-compile'. This ensures you byte-compile ECB\n"
                          "with the same CEDET-library-version as you load into Emacs.\n"
                          "If you use the Makefile check the variable CEDET before compiling!\n"
                          ))
                 ;; Different cedet-version used for byte-compiling ECB and
                 ;; loading into Emacs.
                 ((not (string= cedet-version ecb-compiled-in-cedet-version))
                  (concat "ECB has been byte-compiled with another cedet-version than currently\n"
                          "loaded into Emacs:\n"
                          (format "  + CECET used for byte-compiling ECB: %s\n"
                                  ecb-compiled-in-cedet-version)
                          (format "  + CEDET currently loaded into Emacs: %s\n"
                                  cedet-version)
                          "Please ensure that ECB is byte-compiled with the same cedet-version as you\n"
                          "you load into your Emacs.\n\n"
                          "In general it is recommended to start ECB first-time not byte-compiled\n"
                          "and then call the command `ecb-byte-compile'. This ensures you byte-compile ECB\n"
                          "with the same CEDET-library-version as you load into Emacs.\n"
                          "If you use the Makefile check the variable CEDET before compiling!\n"))
                 (t ""))))
      (unless (= 0 (length err-msg)) 
        (with-output-to-temp-buffer "*ECB cedet-load problems*"
          (princ "Currently ECB can not be activated cause of the following reason:\n\n")
          (princ err-msg)
          (princ "\n\nPlease fix the reported problem and restart Emacs\n"))
        (ecb-error "Please fix the reported problem and restart Emacs!")))))


(defun ecb-activate--impl ()
  "See `ecb-activate'.  This is the implementation of ECB activation."
  (when (or (null ecb-frame) (not (frame-live-p ecb-frame)))
    (setq ecb-frame (selected-frame)))
  
  (if ecb-minor-mode
      (when (and (not (equal (selected-frame) ecb-frame))
                 (or (equal ecb-activation-selects-ecb-frame-if-already-active t)
                     (and (equal ecb-activation-selects-ecb-frame-if-already-active 'ask)
                          (y-or-n-p "ECB is already active in another frame. Select it? "))))
        (ecb-select-ecb-frame)
        (ecb-update-directories-buffer))

    (let ((stack-trace-on-error stack-trace-on-error))
      ;; we activate only if all before-hooks return non nil
      (when (run-hook-with-args-until-failure 'ecb-before-activate-hook)

        ;; temporary changing some emacs-vars
        (when (< max-specpdl-size 3000)
          (ecb-modify-emacs-variable 'max-specpdl-size 'store 3000))
        (when (< max-lisp-eval-depth 1000)
          (ecb-modify-emacs-variable 'max-lisp-eval-depth 'store 1000))
        (when (and ecb-running-xemacs
                   (boundp 'progress-feedback-use-echo-area))
          (ecb-modify-emacs-variable 'progress-feedback-use-echo-area 'store t))
      
        ;; checking if there are cedet or semantic-load problems
        (ecb-check-cedet-load)
        (ecb-check-semantic-load)
              
        ;; checking the requirements
        (ecb-check-requirements)

        (condition-case err-obj
            (progn
              
              ;; initialize the navigate-library
              (ecb-nav-initialize)

              ;; enable basic advices (we need the custom-save-all advice
              ;; already here! Maybe it would be better to remove this advice
              ;; from the basic-advices and add it to upgrade-advices.....)
              ;;(ecb-enable-advices 'ecb-layout-basic-adviced-functions)

              ;; we need the custom-all advice here!
              (ecb-enable-advices 'ecb-methods-browser-advices)
              
              ;; maybe we must upgrade some not anymore compatible or even renamed
              ;; options
              (when (and ecb-auto-compatibility-check
                         (not ecb-upgrade-check-done))
                (ecb-check-not-compatible-options)
                (ecb-upgrade-not-compatible-options)
                (ecb-upgrade-renamed-options)
                (setq ecb-upgrade-check-done t))

              ;; first initialize the whole layout-engine
              (ecb-initialize-layout)

              ;; initialize internals
              (ecb-initialize-all-internals (not ecb-clear-caches-before-activate))

              ;; enable permanent advices - these advices will never being
              ;; deactivated after first activation of ECB unless
              ;; `ecb-split-edit-window-after-start' is not 'before-activation
              ;; (see `ecb-deactivate-internal')
              (ecb-enable-advices 'ecb-permanent-adviced-layout-functions)

              ;; enable advices for not supported window-managers
              (ecb-enable-advices 'ecb-winman-not-supported-function-advices)

              ;; enable advices for the compatibility with other packages
              (ecb-enable-advices 'ecb-compatibility-advices)
            
              ;; set the ecb-frame
              (let ((old-ecb-frame ecb-frame))
                (if ecb-new-ecb-frame
                    (progn
                      (run-hooks 'ecb-activate-before-new-frame-created-hook)
                      (setq ecb-frame (make-frame))
                      (put 'ecb-frame 'ecb-new-frame-created t))
                  (setq ecb-frame (selected-frame))
                  (put 'ecb-frame 'ecb-new-frame-created nil))
                ;; If ECB is acivated in a frame unequal to that frame which was
                ;; the ecb-frame at last deactivation then we initialize the
                ;; `ecb-edit-area-creators'.
                (if (not (equal ecb-frame old-ecb-frame))
                    (ecb-edit-area-creators-init)))
              (raise-frame ecb-frame)
              (select-frame ecb-frame)

              (ecb-enable-own-temp-buffer-show-function t)
      
              ;; now we can activate ECB

              ;; first we run all tree-buffer-creators
              (ecb-tree-buffer-creators-run)
    
              ;; activate the eshell-integration - does not load eshell but
              ;; prepares ECB to run eshell right - if loaded and activated
              (ecb-eshell-activate-integration)

              ;; we need some hooks
              (add-hook (ecb--semantic-after-partial-cache-change-hook)
                        'ecb-update-after-partial-reparse t)
              (add-hook (ecb--semantic-after-toplevel-cache-change-hook)
                        'ecb-rebuild-methods-buffer-with-tagcache t)
;;               (add-hook (ecb--semantic--before-fetch-tags-hook)
;;                         'ecb-prevent-from-parsing-if-exceeding-threshold)              
              (ecb-activate-ecb-autocontrol-function ecb-highlight-tag-with-point-delay
                                                     'ecb-tag-sync)
              (ecb-activate-ecb-autocontrol-function ecb-basic-buffer-sync-delay
                                                     'ecb-basic-buffer-sync)
              (ecb-activate-ecb-autocontrol-function ecb-compilation-update-idle-time
                                                     'ecb-compilation-buffer-list-changed-p)
              (ecb-activate-ecb-autocontrol-function 'post
                                                     'ecb-layout-post-command-hook)
              (ecb-activate-ecb-autocontrol-function 'pre
                                                     'ecb-layout-pre-command-hook)
              (ecb-activate-ecb-autocontrol-function 0.5
                                                     'ecb-repair-only-ecb-window-layout)
              (ecb-activate-ecb-autocontrol-function 'post
                                                     'ecb-handle-major-mode-visibilty)
              (add-hook 'after-save-hook 'ecb-update-methods-after-saving)
              (add-hook 'kill-buffer-hook 'ecb-kill-buffer-hook)

              (add-hook 'find-file-hooks 'ecb-find-file-hook)

              ;; after adding all idle-timers and post- and pre-command-hooks we
              ;; activate the monitoring
              (ecb-activate-ecb-autocontrol-function 1 'ecb-monitor-autocontrol-functions)

              ;; We activate the stealthy update mechanism
              (ecb-stealthy-function-state-init)
              (ecb-activate-ecb-autocontrol-function ecb-stealthy-tasks-delay
                                                      'ecb-stealthy-updates)
            
              ;; running the compilation-buffer update first time
              (ecb-compilation-buffer-list-init)
                            
              ;; ediff-stuff; we operate here only with symbols to avoid bytecompiler
              ;; warnings
              (ecb-activate-ediff-compatibility)

              ;; enabling the VC-support
              (ecb-vc-enable-internals 1)
              
              ;; menus - dealing with the menu for XEmacs is really a pain...
              (when ecb-running-xemacs
                (let ((dummy-buf-name " *dummytogetglobalmap*"))
                  (save-excursion
                    (set-buffer (get-buffer-create dummy-buf-name))
                    (add-submenu nil ecb-minor-menu)
                    (kill-buffer dummy-buf-name)))
                (save-excursion
                  (dolist (buf (buffer-list))
                    (set-buffer buf)
                    (if (null (car (find-menu-item current-menubar
                                                   (list ecb-menu-name))))
                        (add-submenu nil ecb-minor-menu)))))

              (add-hook (if ecb-running-xemacs
                            'activate-menubar-hook
                          'menu-bar-update-hook)
                        'ecb-compilation-update-menu)
              )
          (error
           ;;          (backtrace)
           (ecb-clean-up-after-activation-failure
            "Errors during the basic setup of ECB." err-obj)))

        (condition-case err-obj
            ;; run personal hooks before drawing the layout
            (run-hooks 'ecb-activate-before-layout-draw-hook)
          (error
           (ecb-clean-up-after-activation-failure
            "Errors during the hooks of ecb-activate-before-layout-draw-hook."
            err-obj)))
         
        (setq ecb-minor-mode t)

        ;; now we draw the screen-layout of ECB.
        (condition-case err-obj
            ;; now we draw the layout chosen in `ecb-layout'. This function
            ;; activates at its end also the adviced functions if necessary!
            ;; Here the directories- and history-buffer will be updated.
            (let ((ecb-redraw-layout-quickly nil)
                  (use-last-win-conf (and ecb-last-window-config-before-deactivation
                                          (equal ecb-split-edit-window-after-start
                                                 'before-deactivation)
                                          (not (ecb-window-configuration-invalidp
                                                ecb-last-window-config-before-deactivation)))))
              (ecb-enable-temp-buffer-shrink-to-fit ecb-compile-window-height)
              (if use-last-win-conf                     
                  (setq ecb-edit-area-creators
                        (nth 4 ecb-last-window-config-before-deactivation)))

              (ecb-redraw-layout-full 'no-buffer-sync
                                      nil
                                      (if use-last-win-conf
                                          (nth 6 ecb-last-window-config-before-deactivation))
                                      (if use-last-win-conf
                                          (nth 5 ecb-last-window-config-before-deactivation)
                                        nil))

              ;; if there was no compile-window before deactivation then we have
              ;; to hide the compile-window after activation
              (if (and use-last-win-conf
                       (null (nth 2 ecb-last-window-config-before-deactivation)))
                  (ecb-toggle-compile-window -1))

              (when (member ecb-split-edit-window-after-start
                            '(vertical horizontal nil))
                (delete-other-windows)
                (case ecb-split-edit-window-after-start
                  (horizontal (split-window-horizontally))
                  (vertical (split-window-vertically))))
            
              ;; now we synchronize all ECB-windows
              (ecb-window-sync)
            
              ;; now update all the ECB-buffer-modelines
              (ecb-mode-line-format)
              )
          (error
           (ecb-clean-up-after-activation-failure
            "Errors during the layout setup of ECB." err-obj)))
        
        (condition-case err-obj
            (let ((edit-window (car (ecb-canonical-edit-windows-list))))
              (when (and ecb-display-default-dir-after-start
                         (null (ecb-buffer-file-name
                                (window-buffer edit-window))))
                (ecb-set-selected-directory
                 (ecb-fix-filename (save-excursion
                                     (set-buffer (window-buffer edit-window))
                                     default-directory)))))
          (error
           (ecb-clean-up-after-activation-failure
            "Errors during setting the default directory." err-obj)))

        (condition-case err-obj
            ;; we run any personal hooks
            (run-hooks 'ecb-activate-hook)
          (error
           (ecb-clean-up-after-activation-failure
            "Errors during the hooks of ecb-activate-hook." err-obj)))

        (condition-case err-obj
            ;; enable mouse-tracking for the ecb-tree-buffers; we do this after
            ;; running the personal hooks because if a user put´s activation of
            ;; follow-mouse.el (`turn-on-follow-mouse') in the
            ;; `ecb-activate-hook' then our own ECB mouse-tracking must be
            ;; activated later. If `turn-on-follow-mouse' would be activated
            ;; after our own follow-mouse stuff, it would overwrite our
            ;; mechanism and the show-node-name stuff would not work!
            (if (ecb-show-any-node-info-by-mouse-moving-p)
                (tree-buffer-activate-follow-mouse))
          (error
           (ecb-clean-up-after-activation-failure
            "Errors during the mouse-tracking activation." err-obj)))

        (setq ecb-minor-mode t)
        (message "The ECB is now activated.")

        (condition-case err-obj
            ;; now we display all `ecb-not-compatible-options' and
            ;; `ecb-renamed-options'
            (if (and ecb-auto-compatibility-check
                     (or (ecb-not-compatible-or-renamed-options-detected)
                         (not (ecb-options-version=ecb-version-p))))
                ;; we must run this with an idle-times because otherwise these
                ;; options are never displayed when Emacs is started with a
                ;; file-argument and ECB is automatically activated. I this
                ;; case the buffer of the file-argument would be displayed
                ;; after the option-display and would so hide this buffer.
                (ecb-run-with-idle-timer 0.25 nil 'ecb-display-upgraded-options)
              (ecb-display-news-for-upgrade))
          (error
           (ecb-clean-up-after-activation-failure
            "Error during the compatibility-check of ECB." err-obj)))

        ;; if we activate ECB first time then we display the node "First steps" of
        ;; the online-manual
        (ignore-errors
          (when (null ecb-source-path)
            (let ((ecb-show-help-format 'info))
              (ecb-show-help)
              (Info-goto-node "First steps"))))

        ;; display tip of the day if `ecb-tip-of-the-day' is not nil
        (ignore-errors
          (ecb-show-tip-of-the-day))

        (ecb-enable-advices 'ecb-layout-basic-adviced-functions)
        
        (condition-case err-obj
            ;;now take a snapshot of the current window configuration
            (setq ecb-activated-window-configuration
                  (ecb-current-window-configuration))
          (error
           (ecb-clean-up-after-activation-failure
            "Errors during the snapshot of the windows-configuration." err-obj)))
        ))))


(defun ecb-deactivate ()
  "Deactivates the ECB and kills all ECB buffers and windows."
  (interactive)
  (ecb-minor-mode 0))

(defun ecb-deactivate-internal (&optional run-no-hooks)
  "Deactivates the ECB and kills all ECB buffers and windows."
  (unless (not ecb-minor-mode)

    (when (or run-no-hooks
              (run-hook-with-args-until-failure 'ecb-before-deactivate-hook))

      (setq ecb-last-window-config-before-deactivation
            (ecb-current-window-configuration))
      
      ;; deactivating the adviced functions
      (dolist (adviced-set-elem ecb-adviced-function-sets)
        ;; Note: as permanent defined advices-sets are not disabled here!
        (ecb-disable-advices (car adviced-set-elem)))

      (ecb-enable-own-temp-buffer-show-function nil)      

      (ecb-enable-temp-buffer-shrink-to-fit nil)

      ;; deactivate and reset the speedbar stuff
      (ignore-errors (ecb-speedbar-deactivate))

      ;; deactivates the eshell-integration; this disables also the
      ;; eshell-advices! 
      (ecb-eshell-deactivate-integration)

      ;; For XEmacs
      (tree-buffer-activate-follow-mouse)
      (tree-buffer-deactivate-follow-mouse)

      ;; remove the hooks
      (remove-hook (ecb--semantic-after-partial-cache-change-hook)
                   'ecb-update-after-partial-reparse)
      (remove-hook (ecb--semantic-after-toplevel-cache-change-hook)
                   'ecb-rebuild-methods-buffer-with-tagcache)
;;       (remove-hook (ecb--semantic--before-fetch-tags-hook)
;;                 'ecb-prevent-from-parsing-if-exceeding-threshold)
      (ecb-stop-all-autocontrol/sync-functions)
      (remove-hook 'after-save-hook 'ecb-update-methods-after-saving)
      (remove-hook 'kill-buffer-hook 'ecb-kill-buffer-hook)

      (remove-hook 'find-file-hooks 'ecb-find-file-hook)

      ;; ediff-stuff
      (ecb-deactivate-ediff-compatibility)

      ;; disabling the VC-support
      (ecb-vc-enable-internals -1)

      ;; menus - dealing with the menu for XEmacs is really a pain...
      (ignore-errors
        (when ecb-running-xemacs
          (save-excursion
            (dolist (buf (buffer-list))
              (set-buffer buf)
              (if (car (find-menu-item current-menubar
                                       (list ecb-menu-name)))
                  (delete-menu-item (list ecb-menu-name)))))))
      
      (remove-hook (if ecb-running-xemacs
                       'activate-menubar-hook
                     'menu-bar-update-hook)
                   'ecb-compilation-update-menu)

      ;; run any personal hooks
      (unless run-no-hooks
        (run-hooks 'ecb-deactivate-hook))
    
      ;; clear the ecb-frame. Here we try to preserve the split-state after
      ;; deleting the ECB-screen-layout.
      (when (frame-live-p ecb-frame)
        (raise-frame ecb-frame)
        (select-frame ecb-frame)
        (condition-case oops
            (let* ((config (ecb-window-configuration-data))
                   (window-before-redraw (nth 0 config))
                   (pos-before-redraw (nth 1 config))
                   (edit-win-data-before-redraw (nth 2 config))
                   (edit-win-list-after-redraw nil))
              ;; first we make all windows of the ECB-frame not dedicated and
              ;; then we delete all ECB-windows
              (ecb-select-edit-window)
              (ecb-make-windows-not-dedicated ecb-frame)

              ;; deletion of all windows. (All other advices are already
              ;; disabled!) 
              (ecb-with-original-permanent-layout-functions
               (delete-other-windows))
              
              ;; some paranoia....
              (set-window-dedicated-p (selected-window) nil)

              ;; now we restore the edit-windows as before the deactivation
              ;; (All other advices are already disabled!)
              (if (= (length edit-win-data-before-redraw)
                     (ecb-edit-area-creators-number-of-edit-windows))
                  (ecb-with-original-permanent-layout-functions
                   (ecb-restore-edit-area))
                (ecb-edit-area-creators-init))
              
              (setq edit-win-list-after-redraw (ecb-canonical-edit-windows-list))

              ;; a safety-check if we have now at least as many windows as
              ;; edit-windows before deactivation. If yes we restore all
              ;; window-data as before deactivation.
              (when (= (length edit-win-list-after-redraw)
                       (length edit-win-data-before-redraw))
                (dotimes (i (length edit-win-data-before-redraw))
                  (let ((win (nth i edit-win-list-after-redraw))
                        (data (nth i edit-win-data-before-redraw)))
                    (set-window-buffer win (nth 0 data))
                    (set-window-start win (nth 1 data))
                    (set-window-point win (nth 2 data))
                    (if (> (length edit-win-list-after-redraw) 1)
                        (ecb-set-window-size win (nth 3 data)))
                    )))

              ;; at the end we always stay in that window as before the
              ;; deactivation.
              (when (integerp window-before-redraw)
                (ecb-select-edit-window window-before-redraw))       
              ;; if we were in an edit-window before deactivation let us go to
              ;; the old place
              (when pos-before-redraw
                (goto-char pos-before-redraw)))
          (error
           ;; in case of an error we make all windows not dedicated and delete
           ;; at least all other windows
           (ecb-warning "ecb-deactivate-internal (error-type: %S, error-data: %S)"
                        (car oops) (cdr oops))
           (ignore-errors (ecb-make-windows-not-dedicated ecb-frame))
           (ignore-errors (delete-other-windows))))
        
        (if (get 'ecb-frame 'ecb-new-frame-created)
            (ignore-errors (delete-frame ecb-frame t))))
        
      (ecb-initialize-layout)

      ;; we do NOT disable the permanent-advices of
      ;; `ecb-permanent-adviced-layout-functions' unless the user don't want
      ;; preserving the split-state after reactivating ECB.
      (when (not (equal ecb-split-edit-window-after-start 'before-activation))
        (ecb-disable-advices 'ecb-permanent-adviced-layout-functions t)
        (ecb-edit-area-creators-init))

      ;; we can safely do the kills because killing non existing buffers
      ;; doesn´t matter. We kill these buffers because some customize-options
      ;; takes only effect when deactivating/reactivating ECB, or to be more
      ;; precise when creating the tree-buffers again.
      (dolist (tb-elem ecb-tree-buffers)
        (tree-buffer-destroy (car tb-elem)))
      (ecb-tree-buffers-init)
      
      (setq ecb-activated-window-configuration nil)

      (setq ecb-minor-mode nil)

      ;; restoring the value of temporary modified vars
      (ecb-modify-emacs-variable 'max-specpdl-size 'restore)
      (ecb-modify-emacs-variable 'max-lisp-eval-depth 'restore)
      (when (and ecb-running-xemacs
                 (boundp 'progress-feedback-use-echo-area))
        (ecb-modify-emacs-variable 'progress-feedback-use-echo-area 'restore))))
      
  
  (if (null ecb-minor-mode)
      (message "The ECB is now deactivated."))
  ecb-minor-mode)

;;;###autoload
(defun ecb-minor-mode (&optional arg)
  "Toggle ECB minor mode.
With prefix argument ARG, turn on if positive, otherwise off. Return non-nil
if the minor mode is enabled.

\\{ecb-mode-map}"
  (interactive "P")
  (let ((new-state (if (null arg)
                       (not ecb-minor-mode)
                     (> (prefix-numeric-value arg) 0))))
    (if new-state
        (ecb-activate-internal)
      (ecb-deactivate-internal)))
  (if ecb-running-xemacs
      (redraw-modeline t)
    (force-mode-line-update t))
  ecb-minor-mode)


;; ECB byte-compilation

(defun ecb-compile-file-if-necessary (file &optional force)
  "Compile the ECB-file FILE if necessary. This is done if FORCE is not nil or
FILE.el is newer than FILE.elc or if FILE.elc doesn't exist."
  (let ((elc-file (concat (ecb-file-name-sans-extension file) ".elc")))
    (if (or force
	    (not (ecb-file-exists-p elc-file))
	    (file-newer-than-file-p file elc-file))
        (byte-compile-file file))))

;;;###autoload
(defun ecb-byte-compile (&optional force-all)
  "Byte-compiles the ECB package.
This is done for all lisp-files of ECB if FORCE-ALL is not nil or for each
lisp-file FILE.el which is either newer than FILE.elc or if FILE.elc doesn't
exist."
  (interactive "P")
  (ecb-check-requirements)
  (let ((load-path
	 (append (list (ecb-file-name-directory
			(or (locate-library "semantic")
			    (ecb-error "Semantic is not in the load-path!")))
                       (ecb-file-name-directory
			(or (locate-library "eieio")
			    (ecb-error "Eieio is not in the load-path!")))
                       (ecb-file-name-directory
			(or (locate-library "speedbar")
			    (ecb-error "Speedbar is not in the load-path!")))
		       (ecb-file-name-directory (locate-library "ecb")))
		 load-path))
	(files (ecb-directory-files (ecb-file-name-directory (locate-library "ecb"))
                                    t)))
    (save-excursion
      (dolist (file files)
	(if (save-match-data
              (and (string-match "\\(silentcomp\\|tree-buffer\\|ecb.*\\)\\.el$" file)
                   (not (string-match "ecb-autoloads" file))))
            (ecb-compile-file-if-necessary file force-all))))))

(defun ecb-auto-activate-hook()
  "If necessary, run `ecb-activate' when Emacs is started."
  (when ecb-auto-activate
    (ecb-activate)))

(add-hook 'emacs-startup-hook 'ecb-auto-activate-hook)

(silentcomp-defvar menu-bar-tools-menu)
(condition-case oops
    (progn
      (require 'easymenu)
      (easy-menu-add-item (if ecb-running-xemacs nil menu-bar-tools-menu)
                          (if ecb-running-xemacs '("tools") nil)
                          (ecb-menu-item
                           [ "Start Code Browser (ECB)"
                             ecb-activate
                             :active t
                             :help "Start the Emacs Code Browser."
                             ]))
      )
  (error
   (ecb-warning "Not critical error during adding menu-entry to Tools-menu (error-type: %S, error-data: %S)"
                (car oops) (cdr oops))))


;; some goodies for editing the ecb-elisp-code

;; parsing of our ecb-macros

(eval-after-load "semantic-el"
  (condition-case oops
      (when (fboundp 'semantic-elisp-setup-form-parser)
        ;; defecb-multicache
        (semantic-elisp-reuse-form-parser defvar defecb-multicache)
        ;; defecb-advice-set
        (semantic-elisp-reuse-form-parser defvar defecb-advice-set)        
        ;; defecb-stealthy and tree-buffer-defpopup-command
        (semantic-elisp-setup-form-parser
            (lambda (read-lobject start end)
              (semantic-tag-new-function
               (symbol-name (nth 1 read-lobject)) nil nil
               :user-visible-flag nil
               :documentation (semantic-elisp-do-doc (nth 2 read-lobject))))
          defecb-stealthy
          tree-buffer-defpopup-command)
        ;; defecb-tree-buffer-creator
        (semantic-elisp-setup-form-parser
            (lambda (read-lobject start end)
              (semantic-tag-new-function
               (symbol-name (nth 1 read-lobject)) nil nil
               :user-visible-flag nil
               :documentation (semantic-elisp-do-doc (nth 3 read-lobject))))
          defecb-tree-buffer-creator
          defecb-window-dedicator)
        ;; defecb-advice
        (semantic-elisp-setup-form-parser
            (lambda (read-lobject start end)
              (semantic-tag-new-function
               (symbol-name (nth 1 read-lobject)) nil
               (semantic-elisp-desymbolify
                (list '**ecb-advice: (nth 2 read-lobject) (nth 3 read-lobject)))
               :user-visible-flag nil
               :documentation (semantic-elisp-do-doc (nth 4 read-lobject))))
          defecb-advice)
        ;; defecb-tree-buffer-callback
        (semantic-elisp-setup-form-parser
            (lambda (read-lobject start end)
              (semantic-tag-new-function
               (symbol-name (nth 1 read-lobject)) nil
               (semantic-elisp-desymbolify
                (append '(node ecb-button edit-window-nr shift-mode meta-mode)
                        (nth 4 read-lobject)))
               :user-visible-flag nil
               :documentation (semantic-elisp-do-doc (nth 5 read-lobject))))
          defecb-tree-buffer-callback)
        ;; defecb-autocontrol/sync-function
        (semantic-elisp-setup-form-parser
            (lambda (read-lobject start end)
              (semantic-tag-new-function
               (symbol-name (nth 1 read-lobject)) nil
               (semantic-elisp-desymbolify
                (list '**autocontrol/sync_for_buffer: (nth 2 read-lobject)))
               :user-visible-flag nil
               :documentation (semantic-elisp-do-doc (nth 5 read-lobject))))
          defecb-autocontrol/sync-function)
        ;; ecb-layout-define
        (semantic-elisp-setup-form-parser
            (lambda (read-lobject start end)
              (semantic-tag-new-function
               (nth 1 read-lobject) nil
               (semantic-elisp-desymbolify (list (nth 2 read-lobject)))
               :user-visible-flag nil
               :documentation (semantic-elisp-do-doc (nth 3 read-lobject))))
          ecb-layout-define)
        ;; when-ecb-running-... macros
        (semantic-elisp-reuse-form-parser eval-and-compile
                                          when-ecb-running-xemacs
                                          when-ecb-running-emacs)
        )
    (error
     (ecb-warning "Not critical error during supporting parsing the ecb-macros: (error-type: %S, error-data: %S)"
                  (car oops) (cdr oops)))))

;; highlighting of some ecb-keywords
(condition-case oops
    (progn
      (defconst ecb-font-lock-keywords
        (eval-when-compile
          (let* (
                 ;; Function declarations and exec-with-macros
                 (variable-defs '(
                                  "defecb-multicache"
                                  "defecb-advice-set"
                                  ))
                 (function-defs '(
                                  "defecb-stealthy"
                                  "defecb-tree-buffer-creator"
                                  "defecb-window-dedicator"
                                  "defecb-advice"
                                  "defecb-autocontrol/sync-function"
                                  "defecb-tree-buffer-callback"
                                  ))
                 (plain-keywords '(
                                   "ecb-exec-in-window"
                                   "ecb-do-with-unfixed-ecb-buffers"
                                   "ecb-do-with-fixed-ecb-buffers"
                                   "ecb-with-original-adviced-function-set"
                                   "ecb-with-original-permanent-layout-functions"
                                   "ecb-with-dedicated-window"
                                   "ecb-with-original-basic-functions"
                                   "ecb-with-ecb-advice"
                                   "ecb-with-readonly-buffer"
                                   "ecb-do-if-buffer-visible-in-ecb-frame"
                                   "ecb-when-point-in-edit-window-ecb-windows-visible"
                                   "ecb-layout-define"
                                   "when-ecb-running-xemacs"
                                   "when-ecb-running-emacs"
                                   "ecb-exit-on-input"
                                   ))
                 (v-regexp (regexp-opt variable-defs t))
                 (f-regexp (regexp-opt function-defs t))
                 (k-regexp (regexp-opt plain-keywords t))
                 ;; Regexp depths
                 (v-depth (regexp-opt-depth v-regexp))
                 (f-depth (regexp-opt-depth f-regexp))
                 (k-depth (regexp-opt-depth k-regexp))
                 (full (concat
                        ;; Declarative things: the whole parenthesis expr has always
                        ;; number 1 ==> The paren-expression number for a keyword
                        ;; contained in (append variable-defs function-defs
                        ;; plain-keywords) is always 1
                        "(\\(" v-regexp "\\|" f-regexp "\\|" k-regexp "\\)"
                        ;; Whitespaces & name: The parenthesis expr for name has
                        ;; always the number
                        ;; (+ 1        -- the whole paren-expr for the declarative
                        ;;                things
                        ;;    v-depth  -- all paren-expressions of the variable-defs
                        ;;    f-depth  -- all paren-expressions of the function-defs
                        ;;    k-depth  -- all paren-expressions of the plain keywords
                        ;;    1        -- The \\(\\sw+\\)?: This is the name in case
                        ;;                of a variable- or function-def
                        ;;  )
                        ;; So variable, functions and keywords have the following
                        ;; numbers:
                        ;; - variable-match: Always 2 (The whole surrounding
                        ;;   paren-expr + the surrounding paren-expr defined with
                        ;;   regexp-opt for the variable-defs
                        ;; - function-match: 1 (for the whole surrounding
                        ;;   paren-expr) + v-depth (to jump over the paren-expr of
                        ;;   the variable-defs + 1 (the surrounding paren-expr
                        ;;   defined with regexp-opt for the function-defs
                        "\\>[ \t]*\\(\\sw+\\)?"
                        ))
                 )
            `((,full
               (1 font-lock-keyword-face)
               (,(+ 1 v-depth f-depth k-depth 1) ;; see explanation above
                (cond ((match-beginning 2) ;; see explanation above
                       font-lock-variable-name-face)
                      ((match-beginning ,(+ 1 v-depth 1)) ;; see explanation above
                       font-lock-function-name-face)
                      (t nil))
                nil t)))
            ))
        "Highlighted ecb keywords.")

      (when (fboundp 'font-lock-add-keywords)
        (font-lock-add-keywords 'emacs-lisp-mode
                                ecb-font-lock-keywords)
        ))
  (error
   (ecb-warning "Not critical error during supporting fontifying the ecb-macros: (error-type: %S, error-data: %S)"
                (car oops) (cdr oops))))
  

;; Klaus Berndl <klaus.berndl@sdm.de>: Cause of the magic autostart stuff of
;; the advice-package we must disable at load-time all these advices!!
;; Otherwise would just loading ecb (not activating!) activate each advice
;; AFTER the FIRST usage of our advices!!

(dolist (adviced-set-elem ecb-adviced-function-sets)
  (ecb-disable-advices (car adviced-set-elem) t))

;; init the method- and file-browser at load-time
(ecb-file-browser-initialize)
(ecb-method-browser-initialize)

(silentcomp-provide 'ecb)

;;; ecb.el ends here
