;;; session.el --- use variables, registers and buffer places across sessions

;; Copyright 1996-1999, 2001-2003 Free Software Foundation, Inc.
;;
;; Author: Christoph Wedler <wedler@users.sourceforge.net>
;; Version: (see `session-version' below)
;; Keywords: session, session management, desktop, data, tools
;; X-URL: http://emacs-session.sourceforge.net/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; When you start Emacs, package Session restores various variables (e.g.,
;; input histories) from your last session.  It also provides a menu
;; containing recently changed/visited files and restores the places (e.g.,
;; point) of such a file when you revisit it.

;; For details, check <http://emacs-session.sourceforge.net/> or, if you prefer
;; the manual style, the documentation of functions \\[session-save-session]
;; and `session-store-buffer-places'.

;; Bug fixes, bug reports, improvements, and suggestions for the newest version
;; are strongly appreciated.

;;; To-do:

;; One could imaging a combination of desktop.el and session.el.  IMHO it is
;; easier to include the remaining features of desktop.el (load some files at
;; startup) into session.el, but desktop.el is already part of Emacs...
;; Anyway, here are some ideas for the combined desktop/session:
;;
;;  * Using contexts for buffer positions (idea from bookmark and vc).
;;  * Define common code with bookmark to restore buffers from a
;;    file-representation (for files, dired, info buffers).
;;  * Saving window-configurations?

;;; Installation:

;; This file requires Emacs-20.2, XEmacs-20.2 or higher.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'session)
;;   (add-hook 'after-init-hook 'session-initialize)

;; If you want to use both desktop and session, use:
;;   (setq desktop-globals-to-save '(desktop-missing-file-warning))

;; To customize, use `M-x customize-group RET session RET' or the customize
;; entry in menu Options.

;;; Code:

(provide 'session)
(require 'custom)

;; General Emacs/XEmacs-compatibility compile-time macros
(eval-when-compile
  (require 'cl)
  (defmacro cond-emacs-xemacs (&rest args)
    (cond-emacs-xemacs-macfn
     args "`cond-emacs-xemacs' must return exactly one element"))
  (defun cond-emacs-xemacs-macfn (args &optional msg)
    (if (atom args) args
      (and (eq (car args) :@) (null msg) ; (:@ ...spliced...)
	   (setq args (cdr args)
		 msg "(:@ ....) must return exactly one element"))
      (let ((ignore (if (string-match "XEmacs" emacs-version) :EMACS :XEMACS))
	    (mode :BOTH) code)
	(while (consp args)
	  (if (memq (car args) '(:EMACS :XEMACS :BOTH)) (setq mode (pop args)))
	  (if (atom args)
	      (or args (error "Used selector %s without elements" mode))
	    (or (eq ignore mode)
		(push (cond-emacs-xemacs-macfn (car args)) code))
	    (pop args)))
	(cond (msg (if (or args (cdr code)) (error msg) (car code)))
	      ((or (null args) (eq ignore mode)) (nreverse code))
	      (t (nconc (nreverse code) args))))))
  ;; Emacs/XEmacs-compatibility `defun': remove interactive "_" for Emacs, use
  ;; existing functions when they are `fboundp', provide shortcuts if they are
  ;; known to be defined in a specific Emacs branch (for short .elc)
  (defmacro defunx (name arglist &rest definition)
    (let ((xemacsp (string-match "XEmacs" emacs-version)) reuses first)
      (while (memq (setq first (car definition))
		   '(:try :emacs-and-try :xemacs-and-try
			  :emacs-only :xemacs-only))
	(if (memq first (if xemacsp
			    '(:xemacs-and-try :xemacs-only)
			  '(:emacs-and-try :emacs-only)))
	    (setq reuses (cadr definition)
		  definition nil)
	  (unless (memq first '(:emacs-only :xemacs-only))
	    (push (cadr definition) reuses)))
	(setq definition (cddr definition)))
      (if (and reuses (symbolp reuses))
	  `(defalias ',name ',reuses)
	(let* ((docstring (if (stringp (car definition)) (pop definition)))
	       (spec (and (not xemacsp)
			  (eq (car-safe (car definition)) 'interactive)
			  (null (cddar definition))
			  (cadar definition))))
	  (if (and (stringp spec)
		   (not (string-equal spec ""))
		   (eq (aref spec 0) ?_))
	      (setq definition
		    (cons (if (string-equal spec "_")
			      '(interactive)
			    `(interactive ,(substring spec 1)))
			  (cdr definition))))
	  (if (null reuses)
	      `(defun ,name ,arglist ,docstring
		 ,@(cond-emacs-xemacs-macfn definition))
	    ;; no dynamic docstring in this case
	    `(eval-and-compile		; no warnings in Emacs
	       (defalias ',name
		 (cond ,@(mapcar (lambda (func) `((fboundp ',func) ',func))
				 (nreverse reuses))
		       (t ,(if definition
			       `(lambda ,arglist ,docstring
				  ,@(cond-emacs-xemacs-macfn definition))
			     'ignore)))))))))))

(eval-when-compile
  ;; Emacs would define these when compiling as 0-arg functions...
  ;;  (ignore-errors (defun split-path))
  ;;  (ignore-errors (defun int-to-char))
  (defvar put-buffer-names-in-file-menu)
  (defvar menu-bar-files-menu)
  (defvar yank-menu)
  (defvar minibuffer-local-ns-map))



;;;;##########################################################################
;;;;  User options, configuration variables
;;;;##########################################################################


(defconst session-version "2.2a"
  "Current version of package session.
Check <http://emacs-session.sourceforge.net/> for the newest.")


;;;===========================================================================
;;;  Customization and initialization
;;;===========================================================================

(defgroup session nil
  "Use variables, registers and buffer places across sessions."
  :group 'data
  :link '(emacs-commentary-link "session.el")
  :link '(url-link "http://emacs-session.sourceforge.net/")
  :prefix "session-")

(defgroup session-globals nil
  "Which variables and registers to save across sessions."
  :group 'session
  :prefix "session-")

(defgroup session-places nil
  "Which places are stored for which buffers."
  :group 'session
  :prefix "session-")

(defgroup session-miscellaneous nil
  "Miscellaneous configurations of package session."
  :group 'session
  :prefix "session-")

;; I could imagine that a future version of package custom could make this
;; `PACKAGE-initialize' stuff easier
(defcustom session-use-package nil
  "Pseudo variable.  Used to initialize session in custom buffer.
Put `(session-initialize)' into your ~/.emacs to initialize package
session in future sessions.  See variable `session-initialize'."
  :group 'session
  :type '(boolean :format "%{%t%}: %[(session-initialize)%], %v\n"
		  :on "in use" :off "not yet initialized"
		  :help-echo "Initialize package Session."
		  :action session-initialize))

(defcustom session-initialize t
  "Whether/what to initialize with `session-initialize'.
If t, do full initialization.  Otherwise, the value should be a list
with element.  To enable, include

 * `de-saveplace' to de-install package saveplace (is redundant),
 * `session' to load and save the session file,
 * `places' to store and use places for files/buffers,
 * `keys' to setup the default key and mouse bindings,
 * `menus' to setup the menus."
  :group 'session-miscellaneous
  :type '(choice (const :tag "All" t)
		 (set :value (de-saveplace session places keys menus)
		      (const :tag "De-install saveplace" de-saveplace)
		      (const :tag "Load/Save Session" session)
		      (const :tag "Store/Use Places" places)
		      (const :tag "Setup Key/Mouse Bindings" keys)
		      (const :tag "Setup Menus" menus))))


;;;===========================================================================
;;;  User Options and Configuration: Menu
;;;===========================================================================

(defcustom session-menu-max-size 30
  "*Max number of entries which may appear in the session menus."
  :group 'session-miscellaneous
  :type 'integer)

(defcustom session-file-menu-max-string
  (if (if (boundp 'put-buffer-names-in-file-menu)
	  put-buffer-names-in-file-menu	; XEmacs
	t)				; Emacs
      (cons 50 20)
    50)
  "*Max length of strings in submenus of the File menu.
Value has the form MAX or (MAX . NAME-THRESHOLD).  If the second form is
used and the length returned by `buffer-name' is longer than
NAME-THRESHOLD, the maximum length will be shortened accordingly.

Deprecated: a negative number -MAX stands for (MAX . 0)."
  :group 'session-miscellaneous
  :type '(choice (cons (integer :tag "Max. length" 50)
		       (integer :tag "Name threshold" 20))
		 (integer 50)))

(defcustom session-edit-menu-max-string 50
  "*Max length of strings in submenus of the Edit menu.
See also `session-compact-yank-gap-regexp'.

When running under Emacs, customize `yank-menu-length' instead."
  :group 'session-miscellaneous
  :type 'integer)

(defcustom session-compact-yank-gap-regexp "\\(\n\\|[ \t][ \t][ \t]\\)[ \t\n]*"
  "*Regexp used when trying to find a gap in a long compact string.
If non-nil, leading and trailing whitespaces are not shown, and we try
to find a gap consisting matched by this regexp if we have to split the
string according to `session-edit-menu-max-string'.

This variable has no effect when running under Emacs."
  :group 'session-miscellaneous
  :type 'string)

(defcustom session-menu-permanent-string " *"
  "*Marker for permanent files in menu \"File >> Open...recently changed\".
A file can set as permanent with prefix argument 3 for a command in
`session-kill-buffer-commands'.  It can be set as non-permanent with
prefix argument -1."
  :group 'session-miscellaneous
  :type 'string)

(defcustom session-set-file-name-exclude-regexp
  "/\\.overview\\|.session\\|News/"
  "*Regexp matching file names not to be stored in `file-name-history'.
This is used by `session-set-file-name-history'.  Value nil means, do
not exclude any file."
  :group 'session-miscellaneous
  :type '(choice (const nil) regexp))

(defvar session-menu-accelerator-support
  (and (featurep 'menu-accelerator-support)
       (fboundp 'submenu-generate-accelerator-spec)
       'submenu-generate-accelerator-spec)
  "*Function to generate menu accelerators, or nil if not supported.")

;; calling `abbrev-file-name' on remote files opens the connection!
(defvar session-abbrev-inhibit-function
  (cond ((fboundp 'file-remote-p) 'file-remote-p)
	;; maybe I should define my own `file-remote-p', doesn't exist in Emacs
	((fboundp 'efs-ftp-path) 'efs-ftp-path)
	((fboundp 'ange-ftp-ftp-name) 'ange-ftp-ftp-name)
	((fboundp 'ange-ftp-ftp-path) 'ange-ftp-ftp-path))
  "Function used to determine whether to abbreviate file name.
A file name is not abbreviated if this function returns non-nil when
called with the file name.")

(defvar session-directory-sep-char    ; directory-sep-char is not set
  (if (memq system-type '(ms-dos windows-nt)) ?\\ ?\/)
  "Directory separator character for session menus.")

(defvar session-save-file-coding-system
  (cond-emacs-xemacs
   :EMACS 'iso-latin-1-with-esc
   ;; used `emacs-mule' but this fails with X-Symbol characters...
   :XEMACS (and (featurep 'mule) 'escape-quoted))
  "Coding system to use when writing `session-save-file' if non-nil.")


;;;===========================================================================
;;;  User Options and Configuration: save global variables between sessions
;;;===========================================================================

(defcustom session-globals-max-size 50
  "*Maximal number of elements in the global variables.
Global variables are only saved if they are non-empty lists.  This value
can be shadowed by some element in `session-globals-include'.  If an
element appears more than once in the list, only the first appearance
will be stored."
  :group 'session-globals
  :type 'integer)

(defcustom session-globals-max-string 1024
  "*Maximal length of string elements in global variables."
  :group 'session-globals
  :type 'integer)

(defcustom session-registers-max-string 1024
  "*Maximal length of string elements in registers."
  :group 'session-globals
  :type 'integer)

(defcustom session-save-file (expand-file-name "~/.session")
  "File to save global variables and registers into.
It is saved with coding system `session-save-file-coding-system' at the
end of an Emacs session and loaded at the beginning.  Used for variables
which are typically changed by editing operations, e.g., history and
ring variables.  See \\[session-save-session] for details."
  :group 'session-globals
  :type 'file)

(defcustom session-save-file-modes 384
  "Mode bits of session save file, as an integer, or nil.
After writing `session-save-file', set mode bits of that file to this
value if it is non-nil."
  :group 'session-globals
  :type '(choice (const :tag "Don't change" nil) integer))
  
(defvar session-before-save-hook nil
  "Hook to be run before `session-save-file' is saved.
The functions are called after the global variables are written,
directly before the file is actually saved.")

(defvar session-after-load-save-file-hook
  (cond-emacs-xemacs
   :EMACS (and (default-boundp 'yank-menu)
	       (fboundp 'menu-bar-update-yank-menu)
	       '(session-refresh-yank-menu)))
  "Hook to be run after `session-save-file' has been loaded.
The functions are called when the file has been successfully loaded.")

(defcustom session-globals-regexp "-\\(ring\\|history\\)\\'"
  "Regexp matching global variables to be saved between sessions.
Variables in `session-globals-exclude' are not saved, but variables in
`session-globals-include' are always saved."
  :group 'session-globals
  :type 'regexp)

(defcustom session-globals-exclude
  '(load-history register-alist vc-comment-ring flyspell-auto-correct-ring)
  "Global variables not to be saved between sessions.
It affects `session-globals-regexp' but not `session-globals-include'."
  :group 'session-globals
  :type '(repeat variable))

(defcustom session-globals-include '((kill-ring 10)
				     (session-file-alist 100 t)
				     (file-name-history 200))
  "Global variables to be saved between sessions.
Each element has one of the following forms:
  NAME,
  (NAME MAX-SIZE), or
  (NAME MAX-SIZE ASSOC-P).
where NAME is the symbol name of the variable, whose value must be a
non-empty list and string elements in this list must be smaller than
`session-globals-max-string'.  MAX-SIZE (default is
`session-globals-max-size') is the maximal number of elements to be
saved for this symbol where only the first of equal elements are saved,
and ASSOC-P (default is nil) non-nil means that the variable is an alist
where the equality of elements is checked on the `car'.

If MAX-SIZE or ASSOC-P is non-nil, it can be useful to include a
variable in this list even if it matches `session-globals-regexp'.
`session-globals-exclude' has no effect on these variables.

Do not use this variable to customize your Emacs.  Package custom is the
appropriate choice for this!"
  :group 'session-globals
  :type '(repeat (group :value '(nil 50)
			variable
			(integer :tag "Max size")
			(option :value t (boolean :tag "Alist")))))


;;;===========================================================================
;;;  Configuration: registers and local variables
;;;===========================================================================

(defcustom session-registers '((?0 . ?9) ?- ?= ?\\ ?` region (?a . ?z))
  "*Registers to be saved in `session-save-file'.
Valid elements in this list are:
  CHAR or (FROM . TO) or `file' or `region' or t.
CHAR is a register to save, (FROM . TO) represents a list of registers
from FROM to TO.  `file' means, only save the following registers in
this list if they contain file or file-query references.  `region'
means, only save registers if they contain a region which has less then
`session-registers-max-string' characters.  t means, allow both content
types.  Processing of this list starts with type `file'.

Before saving the session files, markers in registers are turned into
file references, see `session-register-swap-out'."
  :group 'session-globals
  :type '(repeat (choice (const :tag "File registers:" file)
			 (const :tag "String registers:" region)
			 (const :tag "Any register type:" t)
			 (character :tag "Register")
			 (cons :tag "Registers"
			       (character :tag "From")
			       (character :tag "To")))))

(defcustom session-locals-include '(overwrite-mode)
  "Local variables to be stored for specific buffers.
See also `session-locals-predicate'.

Do not add variables to this list which are more appropriate for local
variables in files, i.e., variables which are related to the contents of
the file, e.g. `major-mode'!"
  :group 'session-places
  :type '(repeat variable))

(defcustom session-locals-predicate 'local-variable-p
  "Function which must return non-nil for a local variable to be stored.
This function is called on all variables in `session-locals-include'
with the variable as the first and the current buffer as the second
argument.  Good values are nil (do not store any variable),
`local-variable-p' for local variables, `local-variable-if-set-p' for
variables which become local when set, and t (store all variables in
`session-locals-include')."
  :group 'session-places
  :type '(choice (const :tag "none" nil)
		 (const :tag "All" t)
		 (function-item local-variable-p)
		 (function-item local-variable-if-set-p)
		 (function :tag "Other function")))

(defvar session-register-swap-out (if (fboundp 'register-swap-out)
				      'register-swap-out
				    'session-register-swap-out)
  "Function processing markers in registers when a buffer is killed.
If non-nil, this function is added to `kill-buffer-hook'.")


;;;===========================================================================
;;;  User Options and Configuration: buffer check--undo, mode+name
;;;===========================================================================

(defcustom session-jump-undo-threshold 240
  "*Number of character positions the undo position must be different.
Without prefix arg, `session-jump-to-last-change' jumps successively to
change positions which differ by at least `session-jump-undo-threshold'
characters compared to the current position and previously visited
change positions, see `session-jump-undo-remember'."
  :group 'session-places
  :type 'integer)

(defcustom session-jump-undo-remember 2
  "*Number of previously visited change positions checked additionally.
See `session-jump-undo-threshold' and `session-jump-to-last-change'."
  :group 'session-places
  :type 'integer)

;; Problem if homedir is a symlink (/bar/home -> /net/bar.home) & tmp-mounted
;;   (file-truename "~/foo") => "/tmp_mnt/net/bar.home/foo"
;;   (abbreviate-file-name "/tmp_mnt/net/bar.home/foo") => "/net/bar.home/foo"
;; I.e., there is a bug in `abbreviate-file-name' on both Emacs and XEmacs
;; (with 2nd arg t).  Workaround: use the following in your ~/.emacs:

;;(unless (string= (abbreviate-file-name (file-truename "~") t) "~") ; XEmacs
;;  (setq abbreviated-home-dir
;;	(let ((abbreviated-home-dir "$foo"))
;;	  (concat "\\`\\(?:"
;;		  (regexp-quote (abbreviate-file-name (expand-file-name "~")))
;;		  "\\|"
;;		  (regexp-quote (abbreviate-file-name (file-truename "~")))
;;		  "\\)\\(/\\|\\'\\)"))))

(defconst session-use-truenames-default
  (cond-emacs-xemacs
   :EMACS  (string= (abbreviate-file-name (file-truename "~")) "~")
   :XEMACS (and (string= (abbreviate-file-name (file-truename "~") t) "~")
		(if (eq system-type 'windows-nt)
		    'session-xemacs-buffer-local-mswindows-file-p
		  t))))

(defcustom session-use-truenames session-use-truenames-default
  "*Whether to use the canonical file names when saving/restoring places.
If a function, it is called with no argument and returns whether to use
the canonical names of files.  If non-nil, store and check file names
returned by `file-truename'."
  :group 'session-places
  :type '(choice (const :tag "No" nil)
		 (const :tag "Yes" t)
		 (function-item :tag "If not starting with \\\\"
				session-xemacs-buffer-local-mswindows-file-p)
		 (function :tag "Other function")))

(defcustom session-auto-store t
  "*Determines whether a buffer to be killed passes the mode/name check.
This boolean is used by `session-default-buffer-check-p', see
`session-buffer-check-function'.

A buffer passes the mode/name check, if it passes the mode check, see
below, and its file name is not matched by
`session-name-disable-regexp', or if fails the mode check and its file
name is matched by `session-name-enable-regexp'.

A buffer passes the mode check, if this variable is non-nil and its
major mode is not a member of `session-mode-disable-list', or if this
variable is nil and its major mode is a member of
`session-mode-enable-list'."
  :group 'session-places
  :type 'boolean)

(defcustom session-undo-check 1
  "*Determines how a buffer to be killed passes the undo check.
Its value is MIN or (MIN . LAST) where MIN is a number.  Used by
`session-default-buffer-check-p', see `session-buffer-check-function'.

To pass the undo check
 * the length of `buffer-undo-list', assumed to be -1 if no undo
   information is recorded, must be higher or equal to MIN,
 * the first form is used or LAST is nil: no further requirement
 * LAST is `and': additionally, `session-last-change' must be non-nil,
   i.e., the buffer has been changed previously,
 * LAST is `or': alternatively, `session-last-change' is non-nil."
  :group 'session-places
  :type '(choice (integer :tag "Min no of Changes")
		 (cons (integer :tag "Min no of Changes")
		       (choice :tag "Previous and New Changes"
			       (const :tag "Only consider New Changes" nil)
			       (const :tag "AND previously changed" and)
			       (const :tag "OR previously changed" or)))))

(defcustom session-kill-buffer-commands '(kill-this-buffer)
  "*Commands which kill a buffer.
If a prefix argument was provided to any of these commands, it will
influence the decision whether to store places for the buffer, see
`session-store-buffer-places'.  Using commands which use the minibuffer
for input, is useless."
  :group 'session-places
  :type '(repeat (function :tag "Command")))

(defcustom session-buffer-check-function 'session-default-buffer-check-p
  "Function which return non-nil if buffer places should be stored.
Used by `session-store-buffer-places'.  This function is called with the
buffer to check as argument.  You can also assume that the current
buffer is the buffer to check.

The default value `session-default-buffer-check-p' returns non-nil, if
the buffer
 * visits an existing readable file,
 * passes the mode/name check, see `session-auto-store', and
 * passes the undo check, see `session-undo-check', its default value 1
   means: the buffer must have been changed during the session."
  :group 'session-globals
  :type '(choice (function-item :tag "Default check"
				session-default-buffer-check-p)
		 (function :tag "Other function")))

(defcustom session-mode-disable-list
  '(vm-mode gnus-score-mode message-mode tar-mode)
  "*Major modes of buffers for which no places are stored.
See `session-buffer-check-function'."
  :group 'session-globals
  :type '(repeat (function :tag "Major mode")))

(defcustom session-mode-enable-list nil
  "*Major modes of buffers for which places are stored.
See `session-buffer-check-function'."
  :group 'session-globals
  :type '(repeat (function :tag "Major mode")))

(defcustom session-name-disable-regexp
  (concat "\\`" (regexp-quote
		 (if (fboundp 'temp-directory) (temp-directory) "/tmp")))
  "*File names of buffers for which no places are stored.
See `session-buffer-check-function'."
  :group 'session-places
  :type '(choice (const nil) regexp))

(defcustom session-name-enable-regexp nil
  "*File names of buffers for which places are stored.
See `session-buffer-check-function'."
  :group 'session-places
  :type '(choice (const nil) regexp))




;;;;##########################################################################
;;;;  Store buffer places and local variables, change register contents
;;;;##########################################################################


(defvar session-last-change nil
  "Position of last change in current buffer.
This variable is set by `session-find-file-hook' if the buffer was
changed in a previous session.  It can also be set by providing an
prefix argument to `session-jump-to-last-change'.")
(make-variable-buffer-local 'session-last-change)

(defvar session-file-alist nil
  "Alist for places and local variables for some files.
It has the form
  (NAME POINT MARK POINT-MIN POINT-MAX PERMANENT LAST-CHANGE
   (SYMBOL . VAR) ...)

NAME is the file name, POINT is the point position, MARK is the mark
position, POINT-MIN and POINT-MAX determine the narrow part if non-nil,
PERMANENT is the permanent marker (see `session-buffer-check-function'),
LAST-CHANGE is the position of the last change in the previous session
or was explicitly set with prefix argument 0 for command
\\[session-jump-to-last-change].  Optional pairs (SYMBOL . VAR) are
local variables with their values.")

(defvar session-jump-to-last-change-counter 0
  "Number of repeated invocations of `session-jump-to-last-change'.")

(defvar session-jump-to-last-change-recent nil
  "Current position and previously visited change positions.")


;;;===========================================================================
;;;  Position of last change
;;;===========================================================================

(defun session-undo-position (num pos1 pos2)
  "Return a previous undo-position or set it.
If argument NUM is nil, set `session-last-change' to the recomputed
position given by argument POS1 and return POS1 normalized.

Otherwise, return a previous undo-position or nil, if no such position
can be found.  If `session-jump-to-last-change-counter' is nil, the
position found is the stored last-change position.

If POS1 and POS2 are nil, NUM is the number of undo-boundaries to skip.
The position returned is the last change inside the corresponding undo
step.

Otherwise, NUM is the number of undo entries to skip.  The position
returned is the last change after these entries outside the range from
POS1 to POS2.  Increment `session-jump-to-last-change-counter' by the
number of entries skipped additionally."
  (let ((undo-list (and (consp buffer-undo-list) buffer-undo-list))
	back-list pos len elem)
    (while (and undo-list (null (car undo-list))) (pop undo-list))
    (while undo-list
      (setq elem (pop undo-list))
      (cond ((atom elem)		; marker position
	     (when (or elem pos1)
	       (if (integerp elem) (setq pos elem) (setq elem t))))
	    ((stringp (car elem))	; deletion: (TEXT . POSITION)
	     (setq pos (abs (cdr elem)) len (length (car elem)))
	     (push (list* pos (+ pos len) (- len)) back-list)
	     (when pos1
	       (if (>  pos1 pos) (incf pos1 len))
	       (if (>= pos2 pos) (incf pos2 len))))
	    ((integerp (car elem))	; insertion: (START . END)
	     (setq pos (car elem) len (- (cdr elem) pos))
	     (push (list* pos pos len) back-list)
	     (setq pos (cdr elem))	; compare/jump with/to end of insertion
	     (when pos1
	       (if (> pos1 pos)
		   (setq pos1 (if (> pos1 (cdr elem)) (- pos1 len) pos)))
	       (if (> pos2 pos)
		   (setq pos2 (if (> pos2 (cdr elem)) (- pos2 len) pos)))))
	    (t
	     (setq elem t)))
      (cond ((or (eq elem t) (null num))
	     (if pos1 (setq pos nil)))
	    ((and pos1 (> num 0))	; next distance
	     (decf num)
	     (setq pos nil))
	    ((and pos1 (<= pos1 pos) (<= pos pos2))
	     (incf session-jump-to-last-change-counter)
	     (setq pos nil))
	    ((or (zerop num) (and (null elem) (zerop (decf num))))
	     ;;(if pos1 (message "counter: %d, pos: %d, pos: %d, pos2: %d" session-jump-to-last-change-counter pos pos1 pos2))
	     (setq undo-list nil))))
    (cond ((null num)			; point as `session-last-change'
	   (setq session-last-change pos1
		 pos session-last-change))
	  ((or (null pos) (> num 0))
	   (setq session-jump-to-last-change-counter nil)
	   (setq pos session-last-change))
	  (t				; pos in undo-list
	   (if session-jump-to-last-change-counter
	       (incf session-jump-to-last-change-counter))
	   (setq back-list (cdr back-list))))
    (when pos
      (while back-list
	(setq elem (pop back-list))
	(cond ((> pos (cadr elem))
	       (incf pos (cddr elem)))
	      ((> pos (car elem))
	       (setq pos (car elem)))))
      pos)))

(defun session-jump-to-last-change (&optional arg)
  "Jump to the position of the last change.
Without prefix arg, jump successively to previous change positions which
differ by at least `session-jump-undo-threshold' characters by repeated
invocation of this command.  With prefix argument 0, jump to end of last
change.  With numeric prefix argument, jump to start of first change in
the abs(ARG)s undo block in the `buffer-undo-list'.

With non-numeric prefix argument (\\[universal-argument] only), set
point as oldest change position.  It might change slightly if you jump
to it due to intermediate insert/delete elements in the
`buffer-undo-list'."
  (interactive "P")
  (if (consp arg)
      (let ((pos (session-undo-position nil (point) (point)))
	    (undo-list (and (consp buffer-undo-list) buffer-undo-list)))
	(setq arg 1)
	(while (and undo-list (null (car undo-list))) (pop undo-list))
	(while undo-list (or (pop undo-list) (incf arg)))
	(message "Store %d as special last-change position (%s %d %s)"
		 pos
		 (substitute-command-keys "\\[universal-argument]")
		 arg
		 (substitute-command-keys "\\[session-jump-to-last-change]")))
    ;; set and restrict previously visited undo positions --------------------
    (push (point) session-jump-to-last-change-recent)
    (if (and (null arg) (eq last-command 'session-jump-to-last-change-seq))
	(let ((recent (nthcdr session-jump-undo-remember
			      session-jump-to-last-change-recent)))
	  (if recent (setcdr recent nil)))
      (setcdr session-jump-to-last-change-recent nil)
      (setq session-jump-to-last-change-counter 0))
    (let ((pos (if arg
		   (session-undo-position (abs (prefix-numeric-value arg))
					  nil nil)
		 (point))))
      (unless arg
	;; compute position, compare it with positions in
	;; `session-jump-to-last-change-recent'
	(let ((recent session-jump-to-last-change-recent) old pos1 pos2)
	  (while recent
	    (setq old (pop recent))
	    (setq pos1 (- pos session-jump-undo-threshold)
		  pos2 (+ pos session-jump-undo-threshold))
	    ;;(message "pos: %d, old: %d, len: %d, counter: %d" pos old (length recent) session-jump-to-last-change-counter)
	    (when (and (<= pos1 old) (<= old pos2))
	      (setq pos (session-undo-position
			 session-jump-to-last-change-counter pos1 pos2))
	      (setq recent (and pos
				session-jump-to-last-change-counter
				session-jump-to-last-change-recent))))))
      (cond ((null pos)
	     (message (if (or arg (atom buffer-undo-list))
			  "Do not know position of last change"
			"Do not know position of last distant change")))
	    ((< pos (point-min))
	     (goto-char (point-min))
	     (message "Change position outside visible region"))
	    ((> pos (point-max))
	     (goto-char (point-max))
	     (message "Change position outside visible region"))
	    (t
	     (goto-char pos)
	     (cond ((null session-jump-to-last-change-counter)
		    (message "Jumped to stored last-change position"))
		   ((null arg)
		    (setq this-command 'session-jump-to-last-change-seq))))))))


;;;===========================================================================
;;;  Yank menu (Emacs: refresh existing menu, XEmacs: do our own)
;;;===========================================================================

;; this function should be defined in menu-bar.el...
(defunx session-refresh-yank-menu ()
  :xemacs-only ignore
  "Refresh `yank-menu' according to `kill-ring'."
  (when (and (default-boundp 'yank-menu)
	     (fboundp 'menu-bar-update-yank-menu))
    (let ((killed (reverse (default-value 'kill-ring))))
      (while killed
	(menu-bar-update-yank-menu (pop killed) nil)))))

(defun session-yank (arg)
  "Reinsert the last stretch of killed text, like \\[yank].
Calls `yank' with argument ARG and with `interprogram-paste-function'
bound to nil."
  (interactive "*p")
  (let ((interprogram-paste-function nil)) ;#dynamic
    (yank arg)))

(defun session-popup-yank-menu (event)
  ;; checkdoc-params: (event)
  "Pop up a menu for inserting items in `kill-ring'."
  (interactive "e")
  (when kill-ring
    (setq this-command last-command)
    (popup-menu '("Select and Paste"
		  :filter session-yank-menu-filter))))

(defun session-yank-menu-filter (menu-items)
  ;; checkdoc-params: (menu-items)
  "Return a menu for inserting items in `kill-ring'."
  (let ((menu nil)
	(ring nil)
	(max session-menu-max-size)
	(len (length kill-ring))
	(half-str-len (/ (- session-edit-menu-max-string 4) 2))
	(i 0)
	(active (not buffer-read-only))
	elem
	(interprogram-paste-function nil)) ;#dynamic
    ;; Traversing (append kill-ring-yank-pointer kill-ring) instead indexing
    ;; (current-kill INDEX) would be probably more efficient, but would be a
    ;; very low-level hack
    (while (and (< i len) (> max 0))
      (setq elem (current-kill i t)
	    i (1+ i))
      (unless (or (assoc elem ring) (string-match "\\`[ \t\n]*\\'" elem))
	(push (cons elem i) ring)
	(setq max (1- max))))
    (while ring
      (setq elem (car ring)
	    ring (cdr ring))
      (push (session-yank-string (car elem) half-str-len
				 (list 'session-yank (cdr elem))
				 active)
	    menu))
    (session-menu-maybe-accelerator menu-items menu)))

(defun session-yank-string (string half-len-str callback active)
  "Return menu item STRING with callback CALLBACK.
If ACTIVE is non-nil, the item is active.  HALF-LEN-STR is the length of
the two parts of a abbreviated menu item name."
  (let ((beg (or (and session-compact-yank-gap-regexp
		      (string-match "\\`[ \t\n]+" string)
		      (match-end 0))
		 0))
	(end (or (and session-compact-yank-gap-regexp
		      (string-match "[ \t\n]+\\'" string))
		 (length string))))
    (vector (if (> (- end beg) session-edit-menu-max-string)
		(let ((gap (and session-compact-yank-gap-regexp
				(string-match session-compact-yank-gap-regexp
				 
				 string (- end half-len-str))
				(match-end 0))))
		  (if (and gap (< gap (- end 3)))
		      (setq half-len-str (- (+ half-len-str half-len-str gap)
					    end))
		    (setq gap (- end half-len-str)))
		  (concat (session-subst-char-in-string
			   ?\t ?\ (substring string beg (+ beg half-len-str)) t)
			  " ... "
			  (session-subst-char-in-string
			   ?\t ?\ (substring string gap end) t)))
	      (session-subst-char-in-string ?\t ?\
					    (substring string beg end) t))
	    callback
	    active)))

;; from EMACS-20.4/lisp/subr.el:
(defunx session-subst-char-in-string (fromchar tochar string &optional inplace)
  :try subst-char-in-string
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
  (let ((i (length string))
	(newstr (if inplace string (copy-sequence string))))
    (while (> i 0)
      (setq i (1- i))
      (if (eq (aref newstr i) fromchar)
	  (aset newstr i tochar)))
    newstr))


;;;===========================================================================
;;;  Menu filters (XEmacs only)
;;;===========================================================================

(defun session-file-opened-menu-filter (menu-items)
  ;; checkdoc-params: (menu-items)
  "This is the menu filter for \"File >> Open...recently visited\".
See `session-file-changed-menu-filter'."
  (session-file-changed-menu-filter menu-items file-name-history))

(defun session-file-changed-menu-filter (menu-items &optional files find-fn)
  ;; checkdoc-params: (menu-items)
  "This is the menu filter for \"File >> Open...recently changed\".
It dynamically creates a list of files to use as the contents of the
menu.  The files are taken from FILES or `session-file-alist'.  It
doesn't show the same name twice and shows `session-menu-max-size' names
at most.  FIND-FN or \\[find-file] is the function to use when selecting
a file in the menu."
  (let ((excl nil)
	(menu nil)
	(i session-menu-max-size)
	(max-string (max (cond ((natnump session-file-menu-max-string)
				session-file-menu-max-string)
			       ((integerp session-file-menu-max-string)
				(- 0 session-file-menu-max-string
				   (length (buffer-name))))
			       ((consp session-file-menu-max-string)
				(- (car session-file-menu-max-string)
				   (max (- (length (buffer-name))
					   (cdr session-file-menu-max-string))
					0)))
			       (t 50))
			 16))
	elem desc name)
    (or files (setq files session-file-alist))
    (or find-fn (setq find-fn 'find-file))
    (while (and files (> i 0))
      (setq elem (car files)
	    desc (and (consp elem) elem)
	    files (cdr files))
      (if (consp elem) (setq elem (car elem)))
      (setq elem (session-abbrev-file-name (directory-file-name elem)))
      (or (member elem excl)
	  (progn
	    (setq i (1- i))
	    (push elem excl)
	    (setq name elem)
	    (and (> (length elem) max-string)
		 (fboundp 'split-path)
		 (let* ((path-separator (char-to-string
					 session-directory-sep-char))
			(components (split-path elem)))
		   (or (cdr components)
		       (eq session-directory-sep-char ?\/) ; the right one
		       (setq path-separator "/"
			     components (split-path elem)))
		   (let* ((prefix (if (< (length (car components)) 2)
				      (concat (pop components) path-separator
					      (pop components))
				    (pop components)))
			  (len (+ (length prefix) 5))
			  postfix)
		     (setq components (nreverse components))
		     (while (and (cdr components)
				 (< (incf len (length (car components)))
				    max-string))
		       (push (pop components) postfix))
		     (if (or postfix (cdr components))
			 (setq name
			       (concat prefix path-separator
				       " ... " path-separator
				       (if postfix
					   (mapconcat 'identity postfix
						      path-separator)
					 (car components))))))))
	    (push (vector name (list find-fn elem)
			  :keys (and (sixth desc)
				     session-menu-permanent-string))
		  menu))))
    (session-menu-maybe-accelerator menu-items (nreverse menu))))

(defun session-menu-maybe-accelerator (menu-items menu)
  "Return menu consisting of items in MENU-ITEMS and MENU.
MENU-ITEMS have the usual format of elements in a menu, except that the
name always starts with a accelerator specification \"%_. \".  Also, a
:keys specification will be evaluated if :keys is the first keyword.

The items in MENU will be modified to add accelerator specifications if
`session-menu-accelerator-support' is non-nil."
  (nconc (mapcar 'session-change-menu-item menu-items)
	 (if session-menu-accelerator-support
	     (funcall session-menu-accelerator-support menu)
	   menu)))

(defun session-change-menu-item (item)
  "Change ITEM according to `session-menu-maybe-accelerator'."
  (if (vectorp item)
      (let ((keys (and (eq (aref item 2) :keys)
		       (not (stringp (aref item 3))))))
	(if (if session-menu-accelerator-support keys t)
	    (prog1 (setq item (copy-sequence item))
	      (if keys
		  (aset item 3 (eval (aref item 3))))
	      (or session-menu-accelerator-support
		  (aset item 0 (substring (aref item 0) 4))))
	  item))
    item))

(defun session-abbrev-file-name (name)
  "Return a version of NAME shortened using `directory-abbrev-alist'.
This function does not consider remote file names (see
`session-abbrev-inhibit-function') and substitutes \"~\" for the user's
home directory."
  (if (and session-abbrev-inhibit-function
	   (or (not (fboundp session-abbrev-inhibit-function))
	       (funcall session-abbrev-inhibit-function name)))
      name
    (cond-emacs-xemacs (abbreviate-file-name name :XEMACS t))))


;;;===========================================================================
;;;  Functions in hooks
;;;===========================================================================

(defun session-set-file-name-history ()
  "Add file-name of current buffer to `file-name-history'.
Don't add the file name if it does not visit an existing readable file,
if it matches `session-set-file-name-exclude-regexp', or if it is
already at the front of `file-name-history'.  This function is useful in
`find-file-hooks'."
  (and buffer-file-name
       (file-exists-p buffer-file-name) (file-readable-p buffer-file-name)
       (let ((name (session-abbrev-file-name buffer-file-name)))
	 (or (string= (car file-name-history) name)
	     (string= (car file-name-history) buffer-file-name)
	     (and session-set-file-name-exclude-regexp
		  (string-match session-set-file-name-exclude-regexp name))
	     (push name file-name-history)))))

(defun session-find-file-hook ()
  "Function in `find-file-hooks'.  See `session-file-alist'."
  (unless (eq this-command 'session-disable)
    (let* ((ass (assoc (session-buffer-file-name) session-file-alist))
	   (point (second ass))
	   (mark (third ass))
	   (min (fourth ass))
	   (max (fifth ass))
	   (alist (nthcdr 7 ass)))
      (condition-case nil
	  (while alist
	    (if (local-variable-if-set-p (caar alist) (current-buffer))
		(set (caar alist) (cdar alist)))
	    (setq alist (cdr alist)))
	(error nil))
      (setq session-last-change (seventh ass))
      (and mark
	   (<= (point-min) mark) (<= mark (point-max))
	   ;; I had `set-mark' but this function activates mark in Emacs, but
	   ;; not in XEmacs.  `push-mark' is also OK and doesn't activate in
	   ;; both Emacsen which is better if we use `pending-delete-mode'.
	   (push-mark mark t))
      (and min max
	   (<= (point-min) min) (<= max (point-max))
	   (narrow-to-region min max))
      (and point
	   (<= (point-min) point) (<= point (point-max))
	   (goto-char point)))))

(defun session-kill-buffer-hook ()
  "Function in `kill-buffer-hook'.
See `session-file-alist' and `session-registers'."
  (if buffer-file-name
      (condition-case nil
	  (session-store-buffer-places
	   (if (memq this-command session-kill-buffer-commands)
	       (prefix-numeric-value current-prefix-arg)
	     1))
	(error nil))))


;;;===========================================================================
;;;  Change register contents from marker to file
;;;===========================================================================

(defun session-register-swap-out ()
  "Turn markers in registers into file references when a buffer is killed."
  (and buffer-file-name
       (let ((tail register-alist))
	 (while tail
	   (and (markerp (cdr (car tail)))
		(eq (marker-buffer (cdr (car tail))) (current-buffer))
		(setcdr (car tail)
			(cons 'file buffer-file-name)))
	   (setq tail (cdr tail))))))

(if session-register-swap-out
    (add-hook 'kill-buffer-hook session-register-swap-out))



;;;;##########################################################################
;;;;  Save global variables, add functions to hooks
;;;;##########################################################################


(defvar session-successful-p nil
  "Whether the file `session-save-file' has been loaded successfully.")


;;;===========================================================================
;;;  The buffer file name
;;;===========================================================================

(defun session-xemacs-buffer-local-mswindows-file-p ()
  "Return t if the current buffer visits a local file on MS-Windows.
Also returns t if the current buffer does not visit a file.  Return nil
of the current buffer visits a file starting with \"\\\\\".  Workaround
for XEmacs bug in `file-truename' for file names starting with
\"\\\\\"."
  (or (< (length buffer-file-name) 2)
      (not (string= (substring buffer-file-name 0 2) "\\\\"))))

(defun session-buffer-file-name ()
  "Return the buffer file name according to `session-use-truenames'."
  (if (if (functionp session-use-truenames)
	  (funcall session-use-truenames)
	session-use-truenames)
      buffer-file-truename
    buffer-file-name))


;;;===========================================================================
;;;  Store places and local variables for buffer to be killed
;;;===========================================================================

(defun session-toggle-permanent-flag (arg &optional check)
  "Toggle the permanent flag of the current buffer.
With ARG, set permanent flag if and only if ARG is positive.  If the
permanent flag is set, the places are stored as well.  If CHECK is
non-nil, just return the status of the permanent flag: either nil if it
is unset or `session-menu-permanent-string' if it is set."
  (interactive "P")
  (if buffer-file-name
      (let ((permanent (if arg
			   (> (prefix-numeric-value arg) 0)
			 (not (nth 5 (assoc (session-buffer-file-name)
					    session-file-alist))))))
	(if check
	    (if permanent nil session-menu-permanent-string)
	  (session-store-buffer-places (if permanent 3 -1))
	  (message (if permanent
		       "Permanent flag is set and places are stored"
		     "Permanent flag has been unset"))))
    (if check nil (error "Buffer is not visiting a file"))))

(defun session-store-buffer-places (arg)
  "Store places and local variables in current buffer.
An entry for the current buffer and its places is added to the front of
`session-file-alist' if the buffer is visiting a file and if it is
mentioned in the list below.  ARG is the prefix argument to a command in
`session-kill-buffer-commands' or 1 for any other command.

ARG=-1: delete PERMANENT flag for buffer,
ARG=0: do nothing,
ARG=1: store buffer places, if the PERMANENT flag is set or the buffer
  passes the function in `session-buffer-check-function',
ARG=2: always store buffer places,
ARG=3: set PERMANENT flag and store buffer places.

See also `session-last-change' and `session-locals-include'.

Note that not storing buffer places does not mean deleting an old entry
for the same file.  It means that there is the danger of the entry
becoming too old to be saved across session.  By default, only the first
100 entries of `session-file-alist' are saved, see
`session-globals-include'."
  (let ((file-name (session-buffer-file-name)))
    (when file-name
      (let ((permanent (nthcdr 5 (assoc file-name session-file-alist))))
	(and (< arg 0) (car permanent)
	     (setcar permanent nil))	; reset permanent in existing entry
	(setq permanent (or (car permanent) (> arg 2)))
	(if (or (and permanent (> arg 0))
		(> arg 1)
		(and (= arg 1)
		     (funcall session-buffer-check-function (current-buffer))))
	    (let ((locals session-locals-include)
		  (store nil))
	      (while locals
		(if (if (functionp session-locals-include)
			(funcall session-locals-predicate
				 (car locals) (current-buffer))
		      session-locals-predicate)
		    (push (cons (car locals)
				(symbol-value (car locals)))
			  store))
		(setq locals (cdr locals)))
	      (setq store
		    (nconc (list file-name
				 (point) (mark t)
				 (point-min)
				 (and (<= (point-max) (buffer-size))
				      (point-max))
				 permanent
				 (session-undo-position 0 nil nil))
			   store))
	      (if (equal (caar session-file-alist) file-name)
		  (setcar session-file-alist store)
		(push store session-file-alist))))))))

(defun session-find-file-not-found-hook ()
  "Query the user to delete the permanent flag for a non-existent file.
Always return nil."
  (let ((file-name (session-buffer-file-name)))
    (when file-name
      (let ((permanent (nthcdr 5 (assoc file-name session-file-alist))))
	(and (car permanent)
	     (y-or-n-p "Delete permanent flag for non-existent file? ")
	     (setcar permanent nil))))))


;;;===========================================================================
;;;  Default standard check for buffers to be killed
;;;===========================================================================

(defun session-default-buffer-check-p (buffer)
  "Default function for `session-buffer-check-function'.
Argument BUFFER should be the current buffer."
  (and
   ;; undo check -------------------------------------------------------------
   (or (and (eq (cdr-safe session-undo-check) 'or)
	    session-last-change)
       (and (or (not (eq (cdr-safe session-undo-check) 'and))
		session-last-change)
	    (>= (if (listp buffer-undo-list) (length buffer-undo-list) -1)
		(if (consp session-undo-check)
		    (car session-undo-check)
		  session-undo-check))))
   ;; mode and name check ----------------------------------------------------
   (let ((file (buffer-file-name buffer)))
     (and (file-exists-p file) (file-readable-p file)
	  (if (if session-auto-store
		  (not (memq major-mode session-mode-disable-list))
		(memq major-mode session-mode-enable-list))
	      (not (and session-name-disable-regexp
			(string-match session-name-disable-regexp file)))
	    (and session-name-enable-regexp
		 (string-match session-name-enable-regexp file)))))))


;;;===========================================================================
;;;  Save session file
;;;===========================================================================

(defun session-save-session ()
  "Save session: file places, *-ring, *-history, registers.
Save some global variables and registers into file `session-save-file'
with coding system `session-save-file-coding-system'.  Run functions in
`session-before-save-hook' before writing the file.

See also `session-globals-regexp', `session-globals-include' and
`session-registers'.

This command is executed when using \\[save-buffers-kill-emacs] without
prefix argument 0.  See `kill-emacs-hook'."
  (interactive)
  (and session-save-file
       (not (and (eq this-command 'save-buffers-kill-emacs)
		 (equal current-prefix-arg 0)))
       (or session-successful-p
	   (not (file-exists-p session-save-file))
	   (y-or-n-p "Overwrite old session file (not loaded)? "))
       (save-excursion
	 ;; `kill-emacs' doesn't kill the buffers ----------------------------
	 (let ((buffers (nreverse (buffer-list))))
	   (while buffers
	     (set-buffer (car buffers))
	     (when buffer-file-name
	       (session-store-buffer-places 1)
	       (if session-register-swap-out
		   (funcall session-register-swap-out)))
	     (setq buffers (cdr buffers))))
	 ;; create header of session file ------------------------------------
	 (set-buffer (get-buffer-create " session "))
	 (erase-buffer)
	 (let ((s-excl session-globals-exclude)
	       (slist (append session-globals-include
			      (apropos-internal session-globals-regexp
						'boundp)))
	       ;;(print-readably t) ; no way!
	       symbol val vlist len ass-p
	       coding-system-for-write)
	   (if session-save-file-coding-system
	       (condition-case nil
		   (progn
		     (setq coding-system-for-write
			   (check-coding-system
			    session-save-file-coding-system))
		     (insert (format ";;; -*- coding: %S; -*-\n"
				     session-save-file-coding-system)))
		 (error nil)))
	   (insert ";;; Automatically generated on "
		   (current-time-string)
		   "\n;;; Invoked by "
		   (user-login-name)
		   "@"
		   (system-name)
		   " using "
		   emacs-version
		   "\n")
	   ;; save global variables ------------------------------------------
	   (while slist
	     (setq symbol (car slist)
		   slist  (cdr slist)
		   len session-globals-max-size
		   ass-p nil)
	     (if (consp symbol)
		 (setq ass-p (third symbol)
		       len (or (second symbol) session-globals-max-size)
		       symbol  (first symbol)))
	     (and (default-boundp symbol)
		  (setq val (default-value symbol))
		  (consp val)
		  (not (memq symbol s-excl))
		  (condition-case nil
		      (progn
			(push symbol s-excl)
			;; only takes first of same elements, cut length
			(setq vlist nil)
			(while val
			  (or (and (stringp (car val))
				   (> (length (car val))
				      session-globals-max-string))
			      (if ass-p
				  (assoc (caar val) vlist)
				(member (car val) vlist))
			      (progn
				(push (car val) vlist)
				(>= (setq len (1- len)) 0))
			      (setq val nil))
			  (setq val (cdr val)))
			;; print (the tricky part (read/load isn't clever)):
			;; check each elem
			(while vlist
			  (condition-case nil
			      (push (read (prin1-to-string (car vlist))) val)
			    (error nil))
			  (setq vlist (cdr vlist)))
			(insert (format "(setq-default %S '%S)\n" symbol val)))
		    (error nil))))
	   (session-save-registers)
	   (run-hooks 'session-before-save-hook)
	   (condition-case var
	       (progn
		 (if (file-exists-p session-save-file)
		     (delete-file session-save-file))
		 (make-directory (file-name-directory session-save-file) t)
		 (write-region (point-min) (point-max) session-save-file)
		 (if session-save-file-modes
		     (set-file-modes session-save-file
				     session-save-file-modes)))
	     (error			; efs would signal `ftp-error'
	      (or (y-or-n-p "Could not write session file.  Exit anyway? ")
		  (cond-emacs-xemacs
		   (:EMACS signal :XEMACS signal-error :BOTH
			   (car var) (cdr var))))))
	   (kill-buffer (current-buffer))))))

(defun session-save-registers ()
  "Save registers in `session-registers'."
  (let ((chars session-registers)
	(type 'file)
	register from to)
    (while chars
      (if (symbolp (car chars))
	  (setq type  (car chars)
		chars (cdr chars))
	(setq from (car chars)
	      chars (cdr chars))
	(if (consp from)
	    (setq to   (cdr from)
		  from (car from))
	  (setq to from))
	(while (<= from to)
	  (and (numberp from) (fboundp 'int-to-char)
	       (setq from (int-to-char from)))
	  (setq register (get-register from))
	  (cond ((null register))
		((and (memq type '(file t))
		      (consp register)
		      (memq (car register) '(file file-query)))
		 (insert (if (eq (car register) 'file)
			     (format "(set-register %S '(file . %S))\n"
				     from (cdr register))
			   (format "(set-register %S '(file-query %S %d))\n"
				   from (cadr register) (caddr register)))))
		((and (memq type '(region t))
		      (stringp register)
		      (< (length register) session-registers-max-string))
		 (insert (format "(set-register %S %S)\n" from register))))
	  (setq from (1+ from)))))))


;;;===========================================================================
;;;  Minibuffer history completion, see XEmacs' list-mode
;;;===========================================================================

(defvar session-history-help-string
  '(concat (if (device-on-window-system-p)
	       (substitute-command-keys "Click \\<list-mode-map>\\[list-mode-item-mouse-selected] on a history element to select it.\n") "")
	   (substitute-command-keys "In this buffer, type RET to select the element near point.\n\n"))
  "Form the evaluate to get a help string for history elements.")

(defun session-minibuffer-history-help ()
  "List history of current minibuffer type.
In Emacs, the *History* buffer talks about \"completions\" instead
\"history elements\".  In XEmacs before 21.4.9, selecting an entry might
not work if the minibuffer is non-empty."
  (interactive)
  (let ((history (symbol-value minibuffer-history-variable)))
    (message nil)
    (if history
	(with-output-to-temp-buffer "*History*"
	  (cond-emacs-xemacs
	   (display-completion-list
	    (sort history #'string-lessp)
	    :XEMACS
	    :help-string session-history-help-string
	    :completion-string "Elements in the history are:"))
	  (save-excursion
	    (set-buffer standard-output)
	    (setq completion-base-size 0)))
      (ding)
      (session-minibuffer-message " [Empty history]"))))

(defunx session-minibuffer-message (string)
  :emacs-only  minibuffer-message
  :xemacs-only temp-minibuffer-message)


;;;===========================================================================
;;;  Set hooks, load session file
;;;===========================================================================

;; easymenu.el is for top-level menus only...  both Emacs and XEmacs could
;; profit from a better menu interface...
(defunx session-add-submenu (menu)
  "Add the menu MENU to the beginning of the File menu in the menubar.
If the \"File\" menu does not exist, no submenu is added.  See
`easy-menu-define' for the format of MENU."
  (and menu
       :EMACS
       (>= emacs-major-version 21)
       (boundp 'menu-bar-files-menu)
       (let ((keymap (easy-menu-create-menu (car menu) (cdr menu))))
	 ;; `easy-menu-get-map' doesn't get the right one => use hard-coded
	 (define-key menu-bar-files-menu (vector (intern (car menu)))
	   (cons 'menu-item
		 (cons (car menu)
		       (if (not (symbolp keymap))
			   (list keymap)
			 (cons (symbol-function keymap)
			       (get keymap 'menu-prop)))))))
       :XEMACS
       (featurep 'menubar)
       (let ((current-menubar default-menubar) ;#dynamic
	     (first (cadar (find-menu-item default-menubar '("File")))))
	 (when first
	   ;; XEmacs-20.4 `add-submenu' does not have 4th arg IN-MENU
	   (add-submenu '("File") menu
			;; arg BEFORE cannot be retrieved by any
			;; menubar function -- great...
			(cond ((vectorp first) (aref first 0))
			      ((consp first) (car first))))))))

;;;###autoload
(defunx session-initialize (&rest dummies)
  ;; checkdoc-params: (dummies)
  "Initialize package session and read previous session file.
Setup hooks and load `session-save-file', see `session-initialize'.  At
best, this function is called at the end of the Emacs startup, i.e., add
this function to `after-init-hook'."
  (interactive)
  (setq session-use-package t)
  (when (or (eq session-initialize t)
	    (memq 'de-saveplace session-initialize))
    ;; Features of package saveplace, which has an auto-init, are covered by
    ;; this package.
    (when (functionp 'eval-after-load)
      (eval-after-load "saveplace"
	'(progn
	   (remove-hook 'find-file-hooks 'save-place-find-file-hook)
	   (remove-hook 'kill-emacs-hook 'save-place-kill-emacs-hook)
	   (remove-hook 'kill-buffer-hook 'save-place-to-alist)))))
  (when (or (eq session-initialize t)
	    (memq 'places session-initialize))
    ;; `session-find-file-hook' should be *very* late in `find-file-hooks',
    ;; esp. if some package, e.g. crypt or iso-cvt, change the buffer contents:
    (add-hook 'find-file-hooks 'session-find-file-hook t)
    (add-hook 'find-file-not-found-hooks 'session-find-file-not-found-hook t)
    (add-hook 'kill-buffer-hook 'session-kill-buffer-hook))
  (when (or (eq session-initialize t) (memq 'keys session-initialize))
    (condition-case nil
	(progn
	  (define-key ctl-x-map [(undo)] 'session-jump-to-last-change)
	  (define-key ctl-x-map [(control ?\/)] 'session-jump-to-last-change)
	  (define-key minibuffer-local-map [(meta ?\?)]
	    'session-minibuffer-history-help)
	  :XEMACS
	  ;; C-down-mouse-3 pops up mode menu under Emacs
	  (define-key global-map [(control button3)] 'session-popup-yank-menu)
	  :EMACS
	  ;; Emacs doesn't seem to have keymap inheritance...
	  (define-key minibuffer-local-completion-map [(meta ?\?)]
	    'session-minibuffer-history-help)
	  (define-key minibuffer-local-must-match-map [(meta ?\?)]
	    'session-minibuffer-history-help)
	  (define-key minibuffer-local-ns-map [(meta ?\?)]
	    'session-minibuffer-history-help))
      (error nil)))
  (when (or (eq session-initialize t)
	    (memq 'menus session-initialize))
    (add-hook 'find-file-hooks 'session-set-file-name-history)
    (session-add-submenu '("Open...recently visited"
			   :included file-name-history
			   :filter session-file-opened-menu-filter))
    (session-add-submenu '("Open...recently changed"
			   :included session-file-alist
			   :filter session-file-changed-menu-filter
			   ["%_* Toggle Permanent Flag of Current Buffer"
			    session-toggle-permanent-flag
			    ;; :keys must be at third position!
			    :keys (session-toggle-permanent-flag nil t)
			    :active buffer-file-name]
			   "---"))
    :XEMACS
    (and (featurep 'menubar)
	 (find-menu-item default-menubar '("Edit"))
	 (let ((current-menubar default-menubar))
	   ;; XEmacs-20.4 `add-submenu' does not have 4th arg IN-MENU
	   (add-submenu '("Edit")
			'("Select and Paste"
			  :included kill-ring
			  :filter session-yank-menu-filter)
			(cond ((find-menu-item default-menubar
					       '("Edit" "Delete"))
			       "Delete") ; why just BEFORE, not AFTER
			      ((find-menu-item default-menubar
					       '("Edit" "Paste"))
			       "Paste")
			      ((find-menu-item default-menubar
					       '("Edit" "Undo"))
			       "Undo"))))))
  (when (or (eq session-initialize t)
	    (memq 'session session-initialize))
    (add-hook 'kill-emacs-hook 'session-save-session)
    (or session-successful-p
	(setq session-successful-p
	      (and session-save-file
		   (condition-case nil
		       (progn
			 ;; load might fail with coding-system = emacs-mule
			 (load session-save-file t nil t)
			 (run-hooks 'session-after-load-save-file-hook)
			 t)
		     (error nil)))))))

;;; Local IspellPersDict: .ispell_session
;;; session.el ends here
