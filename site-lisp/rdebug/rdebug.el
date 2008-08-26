;;; rdebug.el --- Ruby debugger user interface, startup file.

;; Copyright (C) 2006, 2007, 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2007, 2008 Anders Lindgren

;; $Id: rdebug.el 409 2007-12-14 02:36:37Z rockyb $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;
;; Introduction:
;;
;; This is a full-blown debugger user interface to the Ruby rdebug
;; debugger shell.
;;
;; The main features are:
;;
;;  * Window layout with dedicated windows for:
;;      + Local and member variables
;;      + Stack trace
;;      + Display expressions
;;      + Breakpoints
;;      + Output
;;      + Debugger Shell
;;
;;  * Source-level debugging:
;;      + The current source file is shown and current line is marked.
;;      + Function keys bindings for effective stepping in the source code.
;;      + A "Debugger" menu for easy access to all features.
;;
;;  * A number of predefined window layouts and key bindings are
;;    supplied, including binding that emulate Eclipse and NetBeans.
;;    The user can easily provide their own window layout and
;;    settings.
;;
;; The default window layout looks like the following:
;;
;; +----------------------------------------------------------------------+
;; |                                Toolbar                               |
;; +-----------------------------------+----------------------------------+
;; | Debugger shell                    | Variables buffer                 |
;; +-----------------------------------+----------------------------------+
;; |                                   |                                  |
;; | Source buffer                     | Output buffer                    |
;; |                                   |                                  |
;; +-----------------------------------+----------------------------------+
;; | Stack buffer                      | Breakpoints buffer               |
;; +-----------------------------------+----------------------------------+
;;

;;
;; Installation:
;;
;; To use this package, place the following line in an appropriate
;; init file (for example ~/.emacs):
;;
;;    (require 'rdebug)
;;

;;
;; History and Future:
;;
;; The design of this debugger user interface was inspired by
;; `gdb-ui', a similar user interface to GDB.
;;
;; Hopefully, rdebug, gdb-ui, and other emacs user interfaces could
;; join forces to create a common user-level look and feel, and a
;; battery of underlying support functions.
;;

;;
;; This file contains only user-customizable variables and code to
;; load the other files when needed.
;;

;;; Code:

;; -------------------------------------------------------------------
;; Consistency checks.
;;

(if (< emacs-major-version 22)
    (error
     "This version of rdebug.el needs at least Emacs 22 or greater - you have version %d."
     emacs-major-version))


;; -------------------------------------------------------------------
;; Support functions.
;;

(defun rdebug-directory ()
  "The directory of this file, or nil."
  (let ((file-name (or load-file-name
                       (symbol-file 'rdebug-directory))))
    (if file-name
        (file-name-directory file-name)
      nil)))


(defun rdebug-compare-filenames (f1 f2)
  "Canonicalize and compare file names."
  ;; Canonicalize by:
  ;;  1) file-truename ensures that the file has got the correct case,
  ;;     and that "..":s in the path are eliminated.
  ;;  2) file-name-as-directory ensures "/foo" and "/foo/" becomes equal.

  ;; Note: for some reason, when the `comp-elisp' external program is
  ;; used, `nil' is part of `load-path'.
  (if f1
      (setq f1 (file-name-as-directory (file-truename f1))))
  (if f2
      (setq f2 (file-name-as-directory (file-truename f2))))
  (equal f1 f2))


;; Add the directory of `rdebug.el' to the load-path. This ensures
;; that all the user have do to use this package is to load this file.
(let ((dir (rdebug-directory)))
  (if dir
      (add-to-list 'load-path dir nil 'rdebug-compare-filenames)))


;; -------------------------------------------------------------------
;; Autoloads.
;;

(autoload 'rdebug "rdebug-core"
  "Run the rdebug Ruby debugger and start the Emacs user interface.

By default, the \"standard\" user window layout looks like the following:

+----------------------------------------------------------------------+
|                                Toolbar                               |
+-----------------------------------+----------------------------------+
| Debugger shell                    | Variables buffer                 |
+-----------------------------------+----------------------------------+
|                                   |                                  |
| Source buffer                     | Output buffer                    |
|                                   |                                  |
+-----------------------------------+----------------------------------+
| Stack buffer                      | Breakpoints buffer               |
+-----------------------------------+----------------------------------+

The variable `rdebug-many-windows-layout-function' can be
customized so that another layout is used. In addition to a
number of predefined layouts it's possible to define a function
to perform a custom layout.

If `rdebug-many-windows' is nil, only a traditional debugger
shell and source window is opened.

The directory containing the debugged script becomes the initial
working directory and source-file directory for your debugger.

The custom variable `gud-rdebug-command-name' sets the command
and options used to invoke rdebug." t)


(autoload 'rdebug-turn-on-debugger-support "rdebug-source"
  "Enable extra source buffer support for the `rdebug' Ruby debugger.

This includes a 'Debugger' menu and special key bindings when the
debugger is active."
  t)


(autoload 'rdebug-track-attach "rdebug-track"
  "Do things to make the current process buffer work like a
rdebug command buffer." t)

(autoload 'turn-on-rdebug-track-mode "rdebug-track"
  "Turn on rdebugtrack mode.

This function is designed to be added to hooks, for example:
  (add-hook 'comint-mode-hook 'turn-on-rdebugtrack-mode)"
  t)


(add-hook 'ruby-mode-hook 'rdebug-turn-on-debugger-support)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup rdebug nil
  "The Ruby debugger"
  :group 'processes
  :group 'tools)

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom gud-rdebug-command-name
  "rdebug --emacs 3"
  "File name for executing the Ruby debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'gud)

(defcustom rdebug-line-width 120
  "Length of line before truncation occurs.
This value limits output in secondary buffers."
  :type 'integer
  :group 'rdebug)

(defcustom rdebug-many-windows t
  "*If non-nil, use the full debugger user interface, see `rdebug'.

However only set to the multi-window display if the rdebug
command invocation has an annotate options (\"--annotate 3\")."
  :type 'boolean
  :group 'rdebug)

(defcustom rdebug-use-separate-io-buffer t
  "*If non-nil, output goes to a dedicated windows.

This only applies when `rdebug-many-windows' is non-nil."
  :type 'boolean
  :group 'rdebug)

(defcustom rdebug-populate-common-keys-function
  'rdebug-populate-common-keys-standard
  "The function to call to populate key bindings common to all rdebug windows.
This includes the secondary windows, the debugger shell, and all
Ruby source buffers when the debugger is active.

This variable can be bound to the following:

* nil -- Don't bind any keys.

* `rdebug-populate-common-keys-standard' -- Bind according to a widely used
  debugger convention:

\\{rdebug-example-map-standard}

* `rdebug-populate-common-keys-eclipse' -- Bind according to Eclipse.

\\{rdebug-example-map-eclipse}

* `rdebug-populate-common-keys-netbeans' -- Bind according to NetBeans.

\\{rdebug-example-map-netbeans}

* Any other value is expected to be a callable function that takes one
  argument, the keymap, and populates it with suitable keys."
  :type 'function
  :group 'rdebug)

(defcustom rdebug-restore-original-window-configuration :many
  "*Control if the original window layout is restored when the debugger exits.
The value can be t, nil, or :many.

A value of t means that the original layout is always restored,
nil means that it's never restored.

:many means that the original layout is restored only when
`rdebug-many-windows' is used."
  :type '(choice (const :tag "Always restore" t)
		 (const :tag "Never restore" nil)
		 (const :tag "Restore in many windows mode" :many))
  :group 'rdebug)

(defcustom rdebug-use-separate-io-buffer t
  "Non-nil means display output from the debugged program in a separate buffer."
  :type 'boolean
  :group 'gud)


(defcustom rdebug-window-layout-function
  'rdebug-window-layout-standard
  "*A function that performs the window layout of `rdebug'.

This is only used in `rdebug-many-windows' mode. This should be
bound to a function that performs the actual window layout. The
function should takes two arguments, the first is the source
buffer and the second the name of the script to debug.

Rdebug provides the following predefined layout functions:

* `rdebug-window-layout-standard'         -- See `rdebug'

* `rdebug-window-layout-no-shell'         -- Standard + Display, no Shell

* `rdebug-window-layout-conservative'     -- Source + Shell + Output

* `rdebug-window-layout-stack-of-windows' -- Extra windows to the right

* `rdebug-window-layout-rocky'            -- Rocky's own layout"
  :type
  '(choice
    (function :tag "Standard"         rdebug-window-layout-standard)
    (function :tag "Conservative"     rdebug-window-layout-conservative)
    (function :tag "Stack of windows" rdebug-window-layout-stack-of-windows)
    (function :tag "Rocky's own"      rdebug-window-layout-rocky)
    (function :tag "Rocky's II"       rdebug-window-layout-rocky2)
    (function :tag "Other"            function))
  :group 'rdebug)

(defcustom rdebug-source-location-ring-size 150
  "Size of rdebug position history ring."
  :type 'integer
  :group 'rdebug)


;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rdebug)

;;; rdebug.el ends here
