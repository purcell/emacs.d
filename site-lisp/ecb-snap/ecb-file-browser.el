;;; ecb-file-browser.el --- the file-browser of Emacs

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
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

;; $Id: ecb-file-browser.el,v 1.63 2006/05/12 16:03:11 berndl Exp $

;;; Commentary:

;; This file contains the code of the file-browser of ECB

(require 'ecb-util)
(require 'tree-buffer)
(require 'ecb-mode-line)
(require 'ecb-navigate)
(require 'ecb-face)
(require 'ecb-speedbar)
(require 'ecb-layout)
(require 'ecb-common-browser)

;; various loads
(require 'assoc)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(eval-when-compile
  (require 'silentcomp))

(silentcomp-defun ecb-speedbar-update-contents)
(silentcomp-defvar vc-cvs-stay-local)
(silentcomp-defvar dired-directory)

;;====================================================
;; Customization
;;====================================================


(defgroup ecb-directories nil
  "Settings for the directories-buffer in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-sources nil
  "Settings for the sources-buffers in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-history nil
  "Settings for the history-buffer in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-version-control nil
  "Settings for the version-control support in the ECB."
  :group 'ecb
  :prefix "ecb-")

(defcustom ecb-source-path nil
  "*Paths where to find code sources.
Each path can have an optional alias that is used as it's display name. If no
alias is set, the path is used as display name.

Lisp-type of this option: The value must be a list L whereas each element of L
is either
- a simple string which has to be the full path of a directory \(this string
  is displayed in the directory-browser of ECB) or
- a 2-element list whereas the first element is the full path of a directory
  \(string) and the second element is an arbitrary alias \(string) for this
  directory which is then displayed instead of the underlying directory."
  :group 'ecb-directories
  :group 'ecb-most-important
  :initialize 'custom-initialize-default
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'ecb-minor-mode)
                            ecb-minor-mode
			    (functionp 'ecb-update-directories-buffer))
		       (ecb-update-directories-buffer))))
  :type '(repeat (choice :tag "Display type"
                         :menu-tag "Display type"
			 (directory :tag "Path")
			 (list :tag "Path with alias"
			       (directory :tag "Path")
			       (string :tag "Alias")))))

(defcustom ecb-add-path-for-not-matching-files '(t . nil)
  "*Add path of a file to `ecb-source-path' if not already contained.
This is done during the auto. windows synchronization which happens if a file
is opened not via the file/directory-browser of ECB. In such a situation ECB
adds the path of the new file auto. to `ecb-source-path' at least temporally
for the current Emacs session. This option defines two things:
1. Should only the root-part \(which means for Unix-like systems always '/'
   and for windows-like systems the drive) of the new file be added as
   source-path to `ecb-source-path' or the whole directory-part? For
   remote-files \(e.g. tramp, ange-ftp- or efs-files) the root-part is the
   complete host-part + the root-dir at that host \(example:
   /berndl@ecb.sourceforge.net:/ would be the root-part of
   /berndl@ecb.sourceforge.net:/tmp/test.txt).
2. Should this path be added for future sessions too?

The value of this option is a cons-cell where the car is a boolean for 1. and
the cdr is a boolean for 2.

A value of not nil for the car \(1.) is reasonably if a user often opens files
not via the ECB-browser which are not located in any of the paths of
`ecb-source-path' because then only one path for each drive \(windows) or the
root-path \(unix) is added to the directory buffer of ECB."
  :group 'ecb-directories
  :type '(cons (boolean :tag "Add only root path")
               (boolean :tag "Ask for saving for future sessions")))


(defvar ecb-source-path-functions nil
  "List of functions to call for finding sources.
Each time the function `ecb-update-directories-buffer' is called, the
functions in this variable will be evaluated. Such a function must return
either nil or a list of strings where each string is a path.")


(defcustom ecb-display-default-dir-after-start t
  "*Automatically display current default-directory after activating ECB.
If a file-buffer is displayed in the edit-window then ECB synchronizes its
tree-buffers to this file-buffer - at least if the option `ecb-window-sync' it
not nil. So for this situation `ecb-display-default-dir-after-start' takes no
effect but this option is for the case if no file-buffer is displayed in the
edit-window after startup:

If true then ECB selects autom. the current default-directory after activation
even if no file-buffer is displayed in the edit-window. This is useful if ECB
is autom. activated after startup of Emacs and Emacs is started without a
file-argument. So the directory from which the startup has performed is auto.
selected in the ECB-directories buffer and the ECB-sources buffer displays the
contents of this directory."
  :group 'ecb-directories
  :type 'boolean)


(defcustom ecb-show-sources-in-directories-buffer '("left7" "left13"
                                                    "left14" "left15")
  "*Show source files in directories buffer.
The value is either 'always or 'never or a list of layout-names for which
layouts sources should be displayed in the directories window."
  :group 'ecb-directories
  :initialize 'custom-initialize-default
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (if (and ecb-minor-mode
			    (functionp 'ecb-set-selected-directory)
                            ecb-path-selected-directory)
		       (ecb-set-selected-directory ecb-path-selected-directory t))))
  :type '(radio (const :tag "Always" :value always)
                (const :tag "Never" :value never)
                (repeat :tag "With these layouts"
                        (string :tag "Layout name"))))


(defcustom ecb-directories-show-node-info '(if-too-long . path)
  "*When to display which node-info in the directories-buffer.
Define which node info should displayed after moving the mouse over a node
\(or after a shift click onto the node) in the directories-buffer.

You can define \"when\" a node-info should be displayed:
- always: Node info is displayed by moving with the mouse over a node.
- if-too-long: Node info is only displayed by moving with the mouse over a
  node does not fit into the window-width of the tree-buffer window.
  In the ECB directories buffer this means also if a node is shortend or if
  the node has an alias \(see `ecb-source-path').
- shift-click: Node info is only displayed after a shift click with the
  primary mouse button onto the node.
- never: Node info is never displayed.

You can define what info should be displayed:
- name: Only the full node-name is displayed.
- path: The full-path of the node is displayed.

Do NOT set this option directly via setq but use always customize!"
  :group 'ecb-directories
  :type '(cons (choice :tag "When"
                       (const :tag "Always" :value always)
                       (const :tag "If too long" :value if-too-long)
                       (const :tag "After shift click" :value shift-click)
                       (const :tag "Never" :value never))
               (choice :tag "What"
                       (const :tag "Node-name" :value name)
                       (const :tag "Full path" :value path))))

(defcustom ecb-directories-update-speedbar 'auto
  "*Update an integrated speedbar after selecting a directory.
If not nil then an integrated speedbar will be updated after selecting a
directory in the ECB-directories-buffer so the speedbar displays the contents
of that directory.

Of course this option makes only sense if the integrated speedbar is displayed
in addition to the ECB-directories-buffer.

This option can have the following values:
- t: Always update speedbar.
- nil: Never update speedbar.
- auto: Update when senseful \(see scenarios below)
- <function>: A user-defined function. The function is called after a
  directory is selected, gets the selected directory as argument and has to
  return nil if the the integrated speedbar should NOT be updated.

Two example-scenarios where different values for this option can be senseful:

If `ecb-show-sources-in-directories-buffer' is not nil or you have a layout
where an ECB-sources-buffer is visible then you probably want to use the
ECB-directories-buffer \(and/or the ECB-sources-buffer) for directory- and
file-browsing. If you have in addition an integrated speedbar running then you
probably want to use speedbar instead of the ECB-methods-buffer for
source-content-browsing. In this case you probably want the speedbar not be
updated because you do not need speedbar reflecting the current-directory
contents but only the contents of the currently selected source-file and the
integrated speedbar updates itself autom. for the latter one!

If `ecb-show-sources-in-directories-buffer' is nil and there is also no
ECB-sources-buffer visible in the current layout then you probably want to use
an integrated speedbar for browsing directory-contents \(i.e. the files) and
file-contents \(instead of the ECB-methods-buffer for example). In this case
you probably want the speedbar updated because you need speedbar reflecting
the current-directory contents so you can select files.

The value 'auto \(see above) takes exactly these two scenarios into account."
  :group 'ecb-directories
  :type '(radio (const :tag "Always" :value t)
                (const :tag "Never" :value nil)
                (const :tag "Automatic" :value auto)
                (function :tag "Custom function")))


(defun ecb-show-sources-in-directories-buffer-p ()
  "Return not nil if in current layout sources are shown in the
directories-buffer."
  (case ecb-show-sources-in-directories-buffer
    (never nil)
    (always t)
    (otherwise
     (and (listp ecb-show-sources-in-directories-buffer)
          (member ecb-layout-name
                  ecb-show-sources-in-directories-buffer)))))

(defcustom ecb-cache-directory-contents '(("^/\\([^:/]*@\\)?\\([^@:/]*\\):.*" . 0)
                                          (".*" . 50))
  "*Cache contents of certain directories.
This can be useful if `ecb-source-path' contains directories with many files
and subdirs, especially if these directories are mounted net-drives \(\"many\"
means here something > 1000, dependent of the speed of the net-connection and
the machine). Or if it contains remote-source-paths which means paths in the
sense of tramp, ange-ftp or efs. For these directories actualizing the
sources- and/or directories- buffer of ECB \(if displayed in current layout!)
can slow down dramatically so a caching increases speed a lot.

The value of this option is a list where the each element is a cons-cell and
looks like:
  \(<dir-regexp> . <filenumber threshold>)
<dir-regexp>: Regular expression a directory must match to be cached.
<filenumber threshold>: Number of directory contents must exceed this number.

A directory will only be cached if and only if the directory-name matches
one regexp of this option and its content-number exceeds the related
threshold AND the directory-name does not match and regexp of
`ecb-cache-directory-contents-not'!

The cache entry for a certain directory will be refreshed and actualized only
by using the POWER-click \(see `ecb-primary-secondary-mouse-buttons') in the
directories-buffer of ECB.

Default-value: ECB caches the contents of all remote directories regardless of
the size and all other directories if more than 50 entries are contained.

Examples:

An entry \(\"/usr/home/john_smith/bigdir*\" . 1000) means the contents of
every subdirectory of the home-directory of John Smith will be cached if the
directory contains more than 1000 entries and its name begins with \"bigdir\".

An entry \(\".*\" . 1000) caches every directory which has more than 1000
entries.

An entry \(\"^/\\\\\(\[^:/]*@\\\\)?\\\\\(\[^@:/]*\\\\):.*\" . 0) caches every
remote \(in the sense of tramp, ange-ftp or efs) directory regardless of the
number of entries.

Please note: If you want your home-dir being cached then you MUST NOT use
\"~\" because ECB tries always to match full path-names!"
  :group 'ecb-directories
  :group 'ecb-most-important
  :type `(repeat (cons (regexp :tag "Directory-regexp")
                       (integer :tag "Filenumber threshold" :value 1000))))


(defcustom ecb-cache-directory-contents-not nil
  "*Do not cache the contents of certain directories.
The value of this option is a list where the each element is a regular
expression a directory must match if it should not being cached.

If a directory-name matches at least one of the regexps of this option the
directory-contents will never being cached. See `ecb-cache-directory-contents'
to see when a directory will be cached.

This option can be useful when normally all directories with a certain amount
of content \(files and subdirs) should be cached but some special directories
not. This can be achieved by:
- setting `ecb-cache-directory-contents' to \(\".*\" . 500): Caches all
  directories with more then 500 entries
- setting `ecb-cache-directory-contents-not' to a value which matches these
  directories which should not being cached \(e.g. \(\"/usr/home/john_smith\")
  excludes the HOME-directory of John Smith from being cached).

Please note: If you want your home-dir exclude from being cached then you MUST
NOT use \"~\" because ECB tries always to match full path-names!"
  :group 'ecb-directories
  :type `(repeat (regexp :tag "Directory-regexp")))

(defcustom ecb-ping-program "ping"
  "Program to send network test packets to a host.
See also `ecb-ping-options'."
  :group 'ecb-directories
  :type  'string)

(defcustom ecb-ping-options
  (if (eq system-type 'windows-nt)
      (list "-n" "1")
    (list "-c" "1"))
  "List of options for the ping program.
These options can be used to limit how many ICMP packets are emitted. Ping is
used to test if a remote host of a remote path \(e.g. a tramp-, ange-ftp- or
efs-path) is accessible. See also `ecb-ping-program'."
  :group 'ecb-directories
  :type  '(repeat string))

(defcustom ecb-host-accessible-check-valid-time nil
  "Time in seconds a cached accessible-state of a remote host is valid.
This option is a list where each element specifies how long for a certain
remote host the cached ping-state \(i.e. if the host is accessible or not)
should be valid. During this time-intervall ECB pings such a remote host only
once, all other checks use the cached value of that real check. But it the
cached value is older than the value of this option ECB will ping again.

Per default ECB discards after 1 minute the cached ping-state of each remote
host. But if you are sure that a certain remote host is always accessible
\(i.e. means in consequence that you are always online when working with ECB
and remote-paths) then add an entry to this option with a high valid-interval.

Examples: An entry \(\".*sourceforge.*\" . 3600) ensures that all remote hosts
machting the string \"sourceforge\" will only once pinged during one hour. Or
\(\".*\" . 300) would ensure that every remote host would be pinged only once
during 5 minutes."
  :group 'ecb-directories
  :type '(repeat (cons (regexp :tag "Remote host regexp")
                       (integer :tag "Valid interval"))))

(defcustom ecb-prescan-directories-for-emptyness 'unless-remote
  "*Prescan directories for emptyness.
ECB does this so directories are displayed as empty in the directories-buffer
even without user-interaction \(i.e. in previous ECB-versions the emptyness of
a directory has been first checked when the user has clicked onto a
directory). ECB optimizes this check as best as possible but if a directory
contains a lot of subdirectories which contain in turn a lot of entries, then
expanding such a directory or selecting it would take of course more time as
without this check - at least at the first time \(all following selects of a
directory uses the cached information if its subdirectories are empty or not).
Therefore ECB performs this check stealthy \(see `ecb-stealthy-tasks-delay')
so normally there should no performance-decrease or additional waiting-time
for the user. There is one exception: For remote directories \(in the sense of
tramp, ange-ftp, or efs) this check can descrease performance even if
performed stealthy and interruptable. Therefore this option offers three
possible settings:

  t: Switch on this feature

  'unless-remote: Switch on this feature but not for remote directories. The
  term \"remote\" means here directories which are used via tramp, ange-ftp or
  efs. So mounted directories are counted not as remote directories here even
  if such a directory is maybe hosted on a remote machine. But normally only
  directories in a LAN are mounted so there should be no performance-problems
  with such mounted directories.

  nil: Switch off this feature completely.

The option `ecb-prescan-directories-exclude-regexps' offers are more fine
granularity to exclude certain directories from this prescan."
  :group 'ecb-directories
  :group 'ecb-most-important
  :type '(radio (const :tag "Switch on" :value t)
                (const :tag "Switch off for remote directories" :value unless-remote)
                (const :tag "Switch off completely" :value nil)))

(defcustom ecb-prescan-directories-exclude-regexps nil
  "*Which directories should be excluded from the empty-prescan.
If a directory matches any of the regexps of this option it will not be
prescanned for emptyness - This option takes only effect if
`ecb-prescan-directories-for-emptyness' is not nil."
  :group 'ecb-directories
  :type '(repeat (regexp :tag "Directory-regexp")))

(defsubst ecb-directory-should-prescanned-p (dir)
  "Return not nil if DIR should be prescanned for emptyness.
The check is performed according to the settings in the options
`ecb-prescan-directories-for-emptyness' and
`ecb-prescan-directories-exclude-regexps'."
  (and (or (equal t ecb-prescan-directories-for-emptyness)
           (and (equal 'unless-remote ecb-prescan-directories-for-emptyness)
                (not (ecb-remote-path dir))))
       (not (ecb-match-regexp-list dir ecb-prescan-directories-exclude-regexps))))

(defcustom ecb-grep-function (if (fboundp 'igrep) 'igrep 'grep)
  "*Function used for performing a grep.
The popup-menu of the tree-buffers \"Directories\", \"Sources\" and
\"History\" offer to grep the \"current\" directory:
- Directory-buffer: The grep is performed in the current popup-directory after
  clicking the right mouse-button onto a node.
- Sources-buffer: The grep is performed in the current selected directory.
- History-buffer: The grep is performed in the directory of the current
  popup-source after clicking the right mouse-button onto a node.

Conditions for such a function:
- The function is called interactively via `call-interactively'
- During the function-call the `default-directory' is temp. set to that
  directory mentioned above with \"... is performed in ...\", i.e. the
  function can use the value of `default-directory' to determine the directory
  to grep.
- The function must read all it's arguments itself.
- The function is completely responsible for performing the grep itself and
  displaying the results.

Normally one of the standard-grepping functions like `grep' or `igrep' \(or
some wrappers around it) should be used!"
  :group 'ecb-directories
  :group 'ecb-sources
  :type 'function)

(defcustom ecb-grep-find-function (if (fboundp 'igrep-find)
                                      'igrep-find 'grep-find)
  "*Function used for performing a recursive grep.
For more Details see option `ecb-grep-function' and replace \"grep\" with
\"recursive grep\" or \"grep-find\"."
  :group 'ecb-directories
  :group 'ecb-sources
  :type 'function)

(defcustom ecb-after-directory-change-hook nil
  "*Hook which run directly after the selected directory has changed.
This means not onyl after a click onto a directory in the directory-window of
ECB but it means this hook runs always when the current directory changes
regardless of the trigger of this change. So for example it runs also when you
just switches from one buffer to another via `switch-to-buffer' or
`switch-to-buffer-other-window' and the directory of these filebuffers is
different but only when auto-synchronizing of the ECB-windows is on (see
`ecb-window-sync'). It runs not when switching between buffers and the
associated files reside in the same directory.

Each function added to this hook will be called with two arguments: The
directory which was current _before_ the directory-change-trigger and the
directory which was now the current \(i.e. after the trigger).

Example: If you switch from a filebuffer \"~/.emacs\" to a filebuffer
\"/tmp/test.txt\" then the functions of this hook will be called with the
two arguments \"~\" and \"/tmp\"."
  :group 'ecb-directories
  :type 'hook)

(defcustom ecb-sources-perform-read-only-check 'unless-remote
  "*Check if source-items in the tree-buffers are read-only.
If a sourcefile is read-only then it will be displayed with that face set in
the option `ecb-source-read-only-face'.

Because this check can be take some time if files are used via a mounted
net-drive ECB performs this check stealthy \(see `ecb-stealthy-tasks-delay')
so normally there should no performance-decrease or additional waiting-time
for the user. But to get sure this option offers three choices: t,
'unless-remote and nil. See `ecb-prescan-directories-for-emptyness' for an
explanation for these three choices.

The option `ecb-read-only-check-exclude-regexps' offers are more fine
granularity to exclude the sources of certain directories from the read-only
state-check."
  :group 'ecb-sources
  :group 'ecb-directories
  :group 'ecb-most-important
  :type '(radio (const :tag "Switch on" :value t)
                (const :tag "Switch off for remote directories" :value unless-remote)
                (const :tag "Switch off completely" :value nil)))

(defcustom ecb-read-only-check-exclude-regexps nil
  "*Which directories should be excluded from the sources-read-only-check.
If a directory matches any of the regexps of this option their sources will
not be checked if they are writable - This option takes only effect if
`ecb-sources-perform-read-only-check' is not nil."
  :group 'ecb-sources
  :group 'ecb-directories
  :type '(repeat (regexp :tag "Directory-regexp")))

(defsubst ecb-sources-read-only-check-p (dir)
  "Return not nil if the sources of DIR should be checked for read-only-state.
The check is performed according to the settings in the options
`ecb-sources-perform-read-only-check' and
`ecb-read-only-check-exclude-regexps'."
  (and (or (equal t ecb-sources-perform-read-only-check)
           (and (equal 'unless-remote ecb-sources-perform-read-only-check)
                (not (ecb-remote-path dir))))
       (not (ecb-match-regexp-list dir ecb-read-only-check-exclude-regexps))))

(defcustom ecb-directories-buffer-name " *ECB Directories*"
  "*Name of the ECB directory buffer.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB Directories*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object of
the ECB-directory-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating and
then activating ECB again!"
  :group 'ecb-directories
  :type 'string)


(defcustom ecb-excluded-directories-regexps '("^\\(CVS\\|\\.[^xX]*\\)$")
  "*Directories that should not be included in the directories list.
The value of this variable should be a list of regular expressions."
  :group 'ecb-directories
  :type '(repeat (regexp :tag "Directory-regexp")))

(defsubst ecb-check-dir-exclude (dir)
  (ecb-match-regexp-list dir ecb-excluded-directories-regexps))

(defcustom ecb-auto-expand-directory-tree 'best
  "*Automatically expand the directory tree to the current source file.
There are three options:
- best: Expand the best-matching source-path
- first: Expand the first matching source-path
- nil: Do not automatically expand the directory tree."
  :group 'ecb-directories
  :type '(radio (const :tag "Best matching"
                       :value best)
                (const :tag "First matching"
                       :value first)
                (const :tag "No auto. expand"
                       :value nil)))


(defcustom ecb-sources-buffer-name " *ECB Sources*"
  "*Name of the ECB sources buffer.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB Sources*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object of
the ECB-sources-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating and
then activating ECB again!"
  :group 'ecb-sources
  :type 'string)

(defcustom ecb-sources-show-node-info '(if-too-long . name)
  "*When to display which node-info in the sources-buffer.
Define which node info should displayed after moving the mouse over a node
\(or after a shift click onto the node) in the sources-buffer.

You can define \"when\" a node-info should be displayed:
See `ecb-directories-show-node-info' for the possible choices.

You can define what info should be displayed:
- name: Only the full node-name is displayed.
- file-info: File infos for this file are displayed.
- file-info-full: Fill infos incl. full path for this file are displayed.

Do NOT set this option directly via setq but use always customize!"
  :group 'ecb-sources
  :type '(cons (choice :tag "When"
                       (const :tag "Always" :value always)
                       (const :tag "If too long" :value if-too-long)
                       (const :tag "After shift click" :value shift-click)
                       (const :tag "Never" :value never))
               (choice :tag "What"
                       (const :tag "Node-name" :value name)
                       (const :tag "File info" :value file-info)
                       (const :tag "File info \(full path)"
                              :value file-info-full))))

(defcustom ecb-sources-exclude-cvsignore nil
  "*Specify if files contained in a .cvsignore should be excluded.
Value is a list of regular expressions or nil. If you want to exclude files
listed in a .cvsignore-file from being displayed in the ecb-sources-buffer
then specify a regexp for such a directory.

If you want to exclude the contents of .cvsignore-files for every directory
then you should add one regexp \".*\" which matches every directory.

If you never want to exclude the contents of .cvsignore-files then set this
option to nil. This is the default."
  :group 'ecb-sources
  :group 'ecb-directories
  :type '(repeat (regexp :tag "Directory-regexp")))

(defcustom ecb-source-file-regexps
  '((".*" . (("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\)$\\)\\)")
             ("^\\.\\(emacs\\|gnus\\)$"))))
  "*Specifies which files are shown as source files.
This is done on directory-base, which means for each directory-regexp the
files to display can be specified. If more than one directory-regexp matches
the current selected directory then always the first one \(and its related
file-exclude/include-regexps) is used! If no directory-regexp matches then all
files are displayed for the currently selected directory.

Important note: It is recommended that the *LAST* element of this list should
contain an always matching directory-regexp \(\".*\")!

So the value of this option is a list of cons-cells where the car is a
directory regexp and the cdr is a 2 element list where the first element is a
list of exclude regexps and the second element is a list of include regexps. A
file is displayed in the sources-buffer of ECB iff: The file does not match
any of the exclude regexps OR the file matches at least one of the include
regexps.

But regardless of the value of this option a file F is never displayed in the
sources-buffer if the directory matches `ecb-sources-exclude-cvsignore'
and the directory contains a file .cvsignore which contains F as an entry!

There are three predefined and useful combinations of an exclude and include
regexp:
- All files
- All, but no backup, object, lib or ini-files \(except .emacs and .gnus). This
  means all files except those starting with \".\", \"#\" or ending with
  \"~\", \".elc\", \".obj\", \".o\", \".lib\", \".dll\", \".a\", \".so\".
  (but including .emacs and .gnus)
- Common source file types (.c, .java etc.)
In addition to these predefined values a custom exclude and include
combination can be defined. Here each list must at least contain one regexp -
this can be a least the empty regexp \"\"!

Tips for the directory- and file-regexps: \"$^\" matches no files/directories,
\".*\" matches all files/directories."
  :group 'ecb-sources
  :group 'ecb-most-important
  :type '(repeat (cons :tag "Directory file-spec"
                       (regexp :tag "Directory regexp")
                       (choice :tag "Files to display"
                               :menu-tag "Files to display"
                               (const :tag "All files"
                                      :value (("") ("")))
                               (const :tag "All, but no backups, objects, etc..."
                                      :value (("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\)$\\)\\)") ("^\\.\\(x?emacs\\|gnus\\)$")))
                               (const :tag "Common source file types"
                                      :value (("") ("\\(\\(M\\|m\\)akefile\\|.*\\.\\(java\\|el\\|c\\|cc\\|h\\|hh\\|txt\\|html\\|texi\\|info\\|bnf\\)\\)$")))
                               (list :tag "Custom"
                                     (repeat (regexp :tag "Exclude regexp"
                                                     :value ""))
                                     (repeat (regexp :tag "Include regexp"
                                                     :value "")))))))


(defcustom ecb-show-source-file-extension t
  "*Show the file extension of source files."
  :group 'ecb-sources
  :type 'boolean)

(defcustom ecb-sources-sort-method 'name
  "*Defines how the source files are sorted.
- 'name: Sorting by name.
- 'extension: Sorting first by extension and then by name.
- nil: No sorting, means source files are displayed in the sequence returned by
  `directory-files' \(called without sorting).
See also `ecb-sources-sort-ignore-case'."
  :group 'ecb-sources
  :type '(radio (const :tag "By name"
                       :value name)
                (const :tag "By extension"
                       :value extension)
                (const :tag "No sorting"
                       :value nil)))

(defcustom ecb-sources-sort-ignore-case t
  "*Ignore case for sorting the source-files of the Sources-buffer.
See also `ecb-sources-sort-method'."
  :group 'ecb-sources
  :type 'boolean)

(defcustom ecb-history-buffer-name " *ECB History*"
  "*Name of the ECB history buffer.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB History*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object of
the ECB-history-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating and
then activating ECB again!"
  :group 'ecb-history
  :type 'string)

(defcustom ecb-history-exclude-file-regexps '("TAGS$" "semantic\\.cache$")
  "*List of regexps which exclude source-files from being historized. Be aware
that each always full filenames \(ie. incl. full path) are matched against
these regexps! Therefore be carefore with regexps beginning with ^!"
  :group 'ecb-history
  :type '(repeat (regexp :tag "Source-regexp")))

(defsubst ecb-check-filename-for-history-exclude (filename)
  (ecb-match-regexp-list filename ecb-history-exclude-file-regexps))

(defcustom ecb-history-show-node-info '(always . path)
  "*When to display which node-info in the history-buffer.
Define which node info should displayed after moving the mouse over a node
\(or after a shift click onto the node) in the history-buffer.

You can define \"when\" a node-info should be displayed:
See `ecb-directories-show-node-info' for the possible choices.

You can define what info should be displayed:
See `ecb-directories-show-node-info' for the possible choices.

Do NOT set this option directly via setq but use always customize!"
  :group 'ecb-history
  :type '(cons (choice :tag "When"
                       (const :tag "Always" :value always)
                       (const :tag "If too long" :value if-too-long)
                       (const :tag "After shift click" :value shift-click)
                       (const :tag "Never" :value never))
               (choice :tag "What"
                       (const :tag "Node-name" :value name)
                       (const :tag "Full path" :value path))))

(defcustom ecb-history-sort-method 'name
  "*Defines how the entries in the history-buffer are sorted.
- 'name: Sorting by name.
- 'extension: Sorting first by extension and then by name.
- nil: No sorting, means the most recently used buffers are on the top of the
       history and the seldom used buffers at the bottom.
See also `ecb-history-sort-ignore-case'."
  :group 'ecb-history
  :type '(radio (const :tag "By name"
                       :value name)
                (const :tag "By extension"
                       :value extension)
                (const :tag "No sorting"
                       :value nil)))

(defcustom ecb-history-sort-ignore-case t
  "*Ignore case for sorting the history-entries.
See also `ecb-history-sort-method'."
  :group 'ecb-history
  :type 'boolean)

(defcustom ecb-kill-buffer-clears-history nil
  "*Define if `kill-buffer' should also clear the history.
There are three options:
- auto: Removes automatically the corresponding history-entry after the buffer
  has been killed.
- ask: Asks, if the history-entry should be removed after the kill.
- nil: `kill-buffer' does not affect the history \(this is the default)."
  :group 'ecb-history
  :type '(radio (const :tag "Remove history entry automatically"
                       :value auto)
                (const :tag "Ask if history entry should be removed"
                       :value ask)
                (const :tag "Do not clear the history"
                       :value nil)))


(defcustom ecb-history-item-name 'buffer-name
  "*The name to use for items in the history buffer."
  :group 'ecb-history
  :type '(radio (const :tag "Buffer name"
                       :value buffer-name)
                (const :tag "File name"
                       :value file-name)))

(defcustom ecb-directories-menu-user-extension
  '(("Version Control"
     (ecb-dir-popup-cvs-status "CVS Status" )
     (ecb-dir-popup-cvs-examine "CVS Examine")
     (ecb-dir-popup-cvs-update "CVS Update")))
  "*Static user extensions for the popup-menu of the directories buffer.
Value is a list of elements of the following type: Each element defines a new
menu-entry and is either:

a) Menu-command: A list containing two sub-elements, whereas the first is the
   function \(a function symbol) being called if the menu-entry is selected
   and the second is the name of the menu-entry.
b) Separator: A one-element-list and the element is the string \"---\": Then a
   non-selectable menu-separator is displayed.
c) Submenu: A list where the first element is the title of the submenu
   displayed in the main-menu and all other elements are either menu-commands
   \(see a) or separators \(see b) or another submenu \(see c). This allows
   deep nested menu-submenu-structures. Currently a level of 4 is allowed but
   in general there could be an infinite depth of nesting but it makes no
   sense - if possible at all - to define infinite nested defcustom-types. So
   there is a limit of 4 levels but tis is not a hard limit: Just increase the
   value of the `ecb-max-submenu-depth' *BEFORE* first loading ECB!

The function of a menu-command must follow the following guidelines: Such a
function must be defined with the macro `tree-buffer-defpopup-command! This
macro defines a new popup-command whereas the newly defined command gets one
argument NODE. See the docstring of `tree-buffer-defpopup-command' for further
details.

Example for the definition of such a popupmenu-command:

\(tree-buffer-defpopup-command ecb-my-special-dir-popup-function
  \"Prints the name of the directory of the node under point.\"
  \(let \(\(node-data=dir \(tree-node->data node)))
     \(message \"Dir under node: %s\" node-data=dir)))

Per default the static user-extensions are added at the beginning of the
built-in menu-entries of `ecb-directories-menu' but the whole menu can be
re-arranged with `ecb-directories-menu-sorter'.

These menu-extensions are static. A dynamic menu-extension can be achieved via
`ecb-directories-menu-user-extension-function'."
  :group 'ecb-directories
  :type (ecb-create-menu-user-ext-type 1 ecb-max-submenu-depth))

(defcustom ecb-directories-menu-user-extension-function 'ignore
  "*Dynamic user extensions for the popup-menu of the directories buffer.
A function which has to return a list in the same format like the option
`ecb-directories-menu-user-extension'. This function is called when the user
opens the popup-menu for the directories buffer.

If no dynamically evaluated menu-extensions should be added to the
directories-buffer the function has to return nil. Therefore the default-value
of this option is `ignore'.

Per default the dynamic user-extensions are added in front of the static
extensions of `ecb-directories-menu-user-extension' but the whole menu can be
re-arranged with `ecb-directories-menu-sorter'."
  :group 'ecb-directories
  :type 'function)

(defcustom ecb-sources-menu-user-extension
  '(("Version control"
     (ecb-file-popup-ediff-revision "Ediff against revision")
     ("---")
     (ecb-file-popup-vc-next-action "Check In/Out")
     (ecb-file-popup-vc-log "Revision history")
     (ecb-file-popup-vc-annotate "Annotate")
     (ecb-file-popup-vc-diff "Diff against last version")
     ("---")
     (ecb-file-popup-vc-refresh-file "Recompute state for file")
     (ecb-file-popup-vc-refresh-dir "Recompute state for whole dir")))
  "*Static user extensions for the popup-menu of the sources buffer.
For further explanations see `ecb-directories-menu-user-extension'.

The node-argument of a menu-function contains as data the filename of the
source for which the popup-menu has been opened.

Per default the static user-extensions are added at the beginning of the
built-in menu-entries of `ecb-sources-menu' but the whole menu can be
re-arranged with `ecb-sources-menu-sorter'."
  :group 'ecb-sources
  :type (ecb-create-menu-user-ext-type 1 ecb-max-submenu-depth))

(defcustom ecb-sources-menu-user-extension-function 'ignore
  "*Dynamic user extensions for the popup-menu of the sources buffer.
A function which has to return a list in the same format like the option
`ecb-sources-menu-user-extension'. This function is called when the user
opens the popup-menu for the sources buffer.

If no dynamically evaluated menu-extensions should be added to the
sources-buffer the function has to return nil. Therefore the default-value
of this option is `ignore'.

Per default the dynamic user-extensions are added in front of the static
extensions of `ecb-sources-menu-user-extension' but the whole menu can be
re-arranged with `ecb-sources-menu-sorter'."
  :group 'ecb-sources
  :type 'function)

(defcustom ecb-history-menu-user-extension
  '(("Version control"
     (ecb-file-popup-ediff-revision "Ediff against revision")
     ("---")
     (ecb-file-popup-vc-next-action "Check In/Out")
     (ecb-file-popup-vc-log "Revision history")
     (ecb-file-popup-vc-annotate "Annotate")
     (ecb-file-popup-vc-diff "Diff against last version")
     ("---")
     (ecb-file-popup-vc-refresh-file "Recompute state for file")
     (ecb-file-popup-vc-refresh-all-files "Recompute state for whole history")))
      "*Static user extensions for the popup-menu of the history buffer.
For further explanations see `ecb-directories-menu-user-extension'.

The node-argument of a menu-function contains as data the filename of the
source for which the popup-menu has been opened.

Per default the static user-extensions are added at the beginning of the
built-in menu-entries of `ecb-history-menu' but the whole menu can be
re-arranged with `ecb-history-menu-sorter'."
  :group 'ecb-history
  :type (ecb-create-menu-user-ext-type 1 ecb-max-submenu-depth))

(defcustom ecb-history-menu-user-extension-function 'ignore
  "*Dynamic user extensions for the popup-menu of the history buffer.
A function which has to return a list in the same format like the option
`ecb-history-menu-user-extension'. This function is called when the user
opens the popup-menu for the history buffer.

If no dynamically evaluated menu-extensions should be added to the
history-buffer the function has to return nil. Therefore the default-value
of this option is `ignore'.

Per default the dynamic user-extensions are added in front of the static
extensions of `ecb-history-menu-user-extension' but the whole menu can be
re-arranged with `ecb-history-menu-sorter'."
  :group 'ecb-history
  :type 'function)

(defcustom ecb-directories-menu-sorter nil
  "*Function which re-sorts the menu-entries of the directories buffer.
If a function then this function is called to re-arrange the menu-entries of
the combined menu-entries of the user-menu-extensions of
`ecb-directories-menu-user-extension' and the built-in-menu
`ecb-directories-menu'. If nil then no special sorting will be done and the
user-extensions are placed in front of the built-in-entries.

The function get one argument, a list of menu-entries. For the format of this
argument see `ecb-directories-menu-user-extension'. The function must return a
new list in the same format. Of course this function can not only re-arrange
the entries but also delete entries or add new entries."
  :group 'ecb-directories
  :type '(choice :tag "Menu-sorter" :menu-tag "Menu-sorter"
                 (const :tag "No special sorting" :value nil)
                 (function :tag "Sort-function" :value identity)))


(defcustom ecb-sources-menu-sorter nil
  "*Function which re-sorts the menu-entries of the directories buffer.
If a function then this function is called to sort the menu-entries of the
combined menu-entries of the user-menu-extensions of
`ecb-sources-menu-user-extension' and the built-in-menu
`ecb-sources-menu'. If nil then no special sorting will be done and the
user-extensions are placed in front of the built-in-entries.

For the guidelines for such a sorter-function see
`ecb-directories-menu-sorter'."
  :group 'ecb-sources
  :type '(choice :tag "Menu-sorter" :menu-tag "Menu-sorter"
                 (const :tag "No special sorting" :value nil)
                 (function :tag "Sort-function" :value identity)))


(defcustom ecb-history-menu-sorter nil
  "*Function which re-sorts the menu-entries of the directories buffer.
If a function then this function is called to sort the menu-entries of the
combined menu-entries of the user-menu-extensions of
`ecb-history-menu-user-extension' and the built-in-menu
`ecb-history-menu'. If nil then no special sorting will be done and the
user-extensions are placed in front of the built-in-entries.

For the guidelines for such a sorter-function see
`ecb-directories-menu-sorter'."
  :group 'ecb-history
  :type '(choice :tag "Menu-sorter" :menu-tag "Menu-sorter"
                 (const :tag "No special sorting" :value nil)
                 (function :tag "Sort-function" :value identity)))


(defcustom ecb-directories-buffer-after-create-hook nil
  "*Local hook running after the creation of the directories-buffer.
Every function of this hook is called once without arguments direct after
creating the directories-buffer of ECB and it's local key-map. So for example a
function could be added which performs calls of `local-set-key' to define new
key-bindings only for the directories-buffer of ECB.

The following keys must not be rebind in the directories-buffer:
<F2>, <F3> and <F4>"
  :group 'ecb-directories
  :type 'hook)


(defcustom ecb-sources-buffer-after-create-hook nil
  "*Local hook running after the creation of the sources-buffer.
Every function of this hook is called once without arguments direct after
creating the sources-buffer of ECB and it's local key-map. So for example a
function could be added which performs calls of `local-set-key' to define new
key-bindings only for the sources-buffer of ECB."
  :group 'ecb-sources
  :type 'hook)


(defcustom ecb-history-buffer-after-create-hook nil
  "*Local hook running after the creation of the history-buffer.
Every function of this hook is called once without arguments direct after
creating the history-buffer of ECB and it's local key-map. So for example a
function could be added which performs calls of `local-set-key' to define new
key-bindings only for the history-buffer of ECB."
  :group 'ecb-history
  :type 'hook)

(defcustom ecb-vc-enable-support 'unless-remote
  "*Enable support for version-control \(VC) systems.
If on then in the directories-buffer \(if the value of the option
`ecb-show-sources-in-directories-buffer' is on for current layout), the
sources-buffer and the history-buffer all file-items are displayed with an
appropriate icon in front of the item-name to indicate the VC-state of this
item. If off then no version-control-state checking is done.

Because this check can be take some time if files are managed by a not local
Version-control-server ECB performs this check stealthy \(see
`ecb-stealthy-tasks-delay') so normally there should no performance-decrease
or additional waiting-time for the user. But to get sure this option offers
three choices: t, 'unless-remote and nil. See the option
`ecb-prescan-directories-for-emptyness' for an explanation for these three
choices.

The option `ecb-vc-directory-exclude-regexps' offers are more fine granularity
to exclude the sources of certain directories from the VC-state-check.

See `ecb-vc-supported-backends' how to customize the VC-support itself."
  :group 'ecb-version-control
  :group 'ecb-sources
  :group 'ecb-most-important
  :type '(radio (const :tag "Switch on" :value t)
                (const :tag "Switch off for remote directories" :value unless-remote)
                (const :tag "Switch off completely" :value nil)))

(defcustom ecb-vc-directory-exclude-regexps nil
  "*Which directories should be excluded from VC-state-check.
If a directory matches any of the regexps of this option the VC-state of its
sources will not be checked - This option takes only effect if
`ecb-vc-enable-support' is not nil."
  :group 'ecb-version-control
  :group 'ecb-sources
  :type '(repeat (regexp :tag "Directory-regexp")))

(defcustom ecb-vc-xemacs-exclude-remote-cvs-repository
  (if ecb-running-xemacs t nil)
  "*Exclude directories with a remote cvs-repository from VC-check.
This option takes only effect for XEmacs and is needed cause of the outdated
VC-package of XEmacs which offers no heuristic state-checking and also no
option `vc-cvs-stay-local'. So this option takes only effect if
`vc-cvs-stay-local' is not avaiable. In this case ECB treats directories which
are managed by CVS but have a remote repository as if the directory would be
not managed by CVS \(so the files are not checked for their VC-state). This si
done to avoid blocking XEmacs when running full cvs-commands \(e.g. \"cvs
status\") over the net.

Note: When ECB can find the option `vc-cvs-stay-local' then this option will
automatically take no effect regardless which Emacs-version is used."
  :group 'ecb-version-control
  :group 'ecb-sources
  :type 'boolean)

(defsubst ecb-vc-directory-should-be-checked-p (dir)
  "Return not nil if the sources of DIR should be checked for VC-state.
The check is performed according to the settings in the options
`ecb-vc-enable-support' and `ecb-vc-directory-should-be-checked-p'."
  (and (or (equal t ecb-vc-enable-support)
           (and (equal 'unless-remote ecb-vc-enable-support)
                (not (ecb-remote-path dir))))
       (not (ecb-match-regexp-list dir ecb-vc-directory-exclude-regexps))))

(defcustom ecb-vc-state-mapping '((up-to-date       . up-to-date)
                                  (edited           . edited)
                                  (locally-modified . edited)
                                  (needs-patch      . needs-patch)
                                  (needs-checkout   . needs-patch)
                                  (needs-merge      . needs-merge)
                                  (unlocked-changes . unlocked-changes)
                                  (added            . added)
                                  (locally-added    . added)
                                  (ignored          . ignored)
                                  (unknown          . unknown))
  "*Mapping from VC-state-values of the backends to VC-state-values of ECB.
ECB understands the following state-values:

  'up-to-date        The working file is unmodified with respect to the
                     latest version on the current branch, and not locked.

  'edited            The working file has been locally edited by the user. If
                     locking is used for the file, this state means that
                     the current version is locked by the calling user.

  'needs-patch       The file has not been edited by the user, but there is
                     a more recent version on the current branch stored
                     in the master file.

  'needs-merge       The file has been edited by the user, and there is also
                     a more recent version on the current branch stored in
                     the master file. This state can only occur if locking
                     is not used for the file.

  'unlocked-changes  The current version of the working file is not locked,
                     but the working file has been changed with respect
                     to that version. This state can only occur for files
                     with locking\; it represents an erroneous condition that
                     should be resolved by the user.

  'added             The working file has already been added/registered to the
                     VC-system but not yet commited.

  'ignored           The version-control-system ignores this file \(e.g.
                     because included in a .cvsignore-file in case of CVS).

  'unknown           The state of the file can not be retrieved\; probably the
                     file is not under a version-control-system.

All state-values a check-vc-state-function of `ecb-vc-supported-backends' can
return must have a mapping to one of the ECB-state-values listed above. If for
a certain backend-VC-state no mapping can be found then per default 'edited is
assumed!

The default value of this option maps already the possible returned
state-values of `ecb-vc-state', `vc-state' and `vc-recompute-state' \(both GNU
Emacs) and `vc-cvs-status' \(Xemacs) to the ECB-VC-state-values."
  :group 'ecb-version-control
  :group 'ecb-sources
  :initialize 'custom-initialize-default  
  :set (function (lambda (sym val)
                   (set sym val)
                   (ecb-vc-cache-clear)))
  :type '(repeat (cons (choice :tag "Backend VC-state"
                               :menu-tag "Backend VC-state"
                               (const :tag "up-to-date" :value up-to-date)
                               (const :tag "edited" :value edited)
                               (const :tag "locally-modified" :value locally-modified)
                               (const :tag "needs-patch" :value needs-patch)
                               (const :tag "needs-checkout" :value needs-checkout)
                               (const :tag "needs-merge" :value needs-merge)
                               (const :tag "unlocked-changes" :value unlocked-changes)
                               (const :tag "added" :value added)
                               (const :tag "locally-added" :value locally-added)
                               (const :tag "ignored" :value ignored)
                               (const :tag "unknown" :value unknown)
                               (symbol :tag "Other..."))
                       (choice :tag "ECB VC-state"
                               :menu-tag "ECB VC-state"
                               (const :tag "up-to-date" :value up-to-date)
                               (const :tag "edited" :value edited)
                               (const :tag "needs-patch" :value needs-patch)
                               (const :tag "needs-merge" :value needs-merge)
                               (const :tag "unlocked-changes" :value unlocked-changes)
                               (const :tag "added" :value added)
                               (const :tag "ignored" :value ignored)
                               (const :tag "unknown" :value unknown)))))

(defcustom ecb-vc-supported-backends
  (delq nil (if ecb-running-xemacs
                `((ecb-vc-dir-managed-by-CVS . vc-cvs-status))
              `((ecb-vc-dir-managed-by-CVS . ecb-vc-state)
                (ecb-vc-dir-managed-by-RCS . ecb-vc-state)
                (ecb-vc-dir-managed-by-SCCS . ecb-vc-state)
                (ecb-vc-dir-managed-by-SVN . ecb-vc-state))))
  "*Define how to to identify the VC-backend and how to check the state.
The value of this option is a list containing cons-cells where the car is a
function which is called to identify the VC-backend for a DIRECTORY and the
cdr is a function which is called to check the VC-state of the FILEs contained
in DIRECTORY.

Identify-backend-function: It gets a full directory-name as argument - always
without ending slash \(rsp. backslash for native Windows-XEmacs) - and has to
return a unique symbol for the VC-backend which manages that directory \(e.g.
'CVS for the CVS-system or 'RCS for the RCS-system) or nil if the file is not
managed by a version-control-system.

Check-vc-state-function: It gets a full filename \(ie. incl. the complete
directory-part) and has to return a symbol which indicates the VC-state of
that file. The possible returned values of such a check-vc-state-function have
to be mapped with `ecb-vc-state-mapping' to the allowed ECB-VC-state values.

ECB runs for a certain DIRECTORY all identify-backend-functions in that order
they are listed in this option. For the first which returns a value unequal
nil the associated check-state-function is used to retrieve the VC-state of
all sourcefiles in that DIRECTORY.

There is no need for the identify-backend-function or the
check-vc-state-function to cache any state because ECB automatically caches
internally all necessary informations for directories and files for best
possible performance.

To prepend ECB from checking the VC-state for any file set
`ecb-vc-enable-support' to nil.

Default value for GNU Emacs: Support for CVS, RCS, SCCS and Subversion \(for
the later one the most recent version of the VC-package incl. the vc-svn
library is needed). To identify the VC-backend the functions
`ecb-vc-managed-by-CVS', `ecb-vc-managed-by-RCS' rsp. `ecb-vc-managed-by-SCCS'
rsp. `ecb-vc-managed-by-SVN' are used. For all four backends the function
`ecb-vc-state' of the VC-package is used.

Default value for XEmacs: XEmacs contains only a quite outdated VC-package,
especially there is no backend-independent check-vc-state-function available
\(like `vc-state' for GNU Emacs). Only for CVS a check-vc-state-function is
available: `vc-cvs-status'. Therefore ECB adds per default only support for
CVS and uses `ecb-vc-managed-by-CVS' rsp. `vc-cvs-status'. But read the
documentation of `ecb-vc-dir-managed-by-CVS'!

Example for GNU Emacs: If `vc-recompute-state' \(to get real state-values not
only heuristic ones) should be used to check the state for CVS-managed files
and `vc-state' for all other backends then an element
\(ecb-vc-dir-managed-by-CVS . vc-recompute-state) should be added at the
beginning of this option."
  :group 'ecb-version-control
  :group 'ecb-sources
  :initialize 'custom-initialize-default  
  :set (function (lambda (sym val)
                   (set sym val)
                   (ecb-vc-cache-clear)))
  :type '(repeat (cons (symbol :tag "Identify-backend-function")
                       (symbol :tag "Check-state-function"))))

;; Klaus Berndl <klaus.berndl@sdm.de>: For XEmacs a function like the
;; following could be used to get always fresh state-values:

;; (defun ecb-vc-recompute-state (file)
;;   ;; Return the cvs status of FILE
;;   ;; (Status field in output of "cvs status")
;;   (vc-fetch-master-properties file)
;;   (vc-file-getprop file 'vc-cvs-status))

;;====================================================
;; Internals
;;====================================================

;; constants for the node-types
(defconst ecb-directories-nodetype-directory 0)
(defconst ecb-directories-nodetype-sourcefile 1)
(defconst ecb-directories-nodetype-sourcepath 2)
(defconst ecb-sources-nodetype-sourcefile 0)
(defconst ecb-history-nodetype-sourcefile 0)


(defvar ecb-path-selected-directory nil
  "Path to currently selected directory.")

(defvar ecb-path-selected-source nil
  "Path to currently selected source.")

;; accessors for the FILES-AND-SUBDIRS-cache

(defun ecb-files-and-subdirs-cache-add (dir cached-value)
  "Add the files and subdirs of DIR to the cache."
  (ecb-multicache-put-value 'ecb-filename-cache dir 'FILES-AND-SUBDIRS
                             cached-value))

(defun ecb-files-and-subdirs-cache-get (dir)
  "Get the files and subdirs of DIR from the cache. Nil if not cached."
  (ecb-multicache-get-value 'ecb-filename-cache dir 'FILES-AND-SUBDIRS))

(defun ecb-files-and-subdirs-cache-remove (dir)
  "Remove DIR from the cache."
  (ecb-multicache-clear-value 'ecb-filename-cache dir 'FILES-AND-SUBDIRS))

(defun ecb-files-and-subdirs-cache-clear ()
  "Clear the whole FILES-AND-SUBDIRS-cache."
  (ecb-multicache-clear-subcache 'ecb-filename-cache 'FILES-AND-SUBDIRS))

(defun ecb-files-and-subdirs-cache-dump (&optional no-nil-value)
  "Dump the whole FILES-AND-SUBDIRS-cache in another window. If NO-NIL-VALUE
is not nil then these cache-entries are not dumped. This command is not
intended for end-users of ECB."
  (interactive "P")
  (ecb-multicache-print-subcache 'ecb-filename-cache
                                 'FILES-AND-SUBDIRS
                                 no-nil-value))
  
;; accessors for the EMPTY-DIR-P-cache

(defun ecb-directory-empty-cache-add (dir cached-value)
  "Add information if DIR is empty or not to the cache."
  (ecb-multicache-put-value 'ecb-filename-cache dir 'EMPTY-DIR-P
                             cached-value))

(defun ecb-directory-empty-cache-get (dir)
  "get information if DIR is empty or not from the cache."
  (ecb-multicache-get-value 'ecb-filename-cache dir 'EMPTY-DIR-P))

(defun ecb-directory-empty-cache-remove (dir)
  "Remove DIR from the EMPTY-DIR-P-cache."
  (ecb-multicache-clear-value 'ecb-filename-cache dir 'EMPTY-DIR-P))

(defun ecb-directory-empty-cache-remove-all (dir)
  "Remove DIR and all its suddirs from the EMPTY-DIR-P-cache."
  (ecb-directory-empty-cache-remove dir)
  ;; now we remove the subdirs
  (save-match-data
    (ecb-multicache-mapsubcache
     'ecb-filename-cache 'EMPTY-DIR-P
     (function (lambda (key old-value)
                 (if (string-match (concat "^"
                                           (regexp-quote dir)
                                           ".+")
                                   key)
                     ;; the directory-key matches DIR so its a cache
                     ;; subdirectory of DIR so we return nil ==> in fact we
                     ;; remove this subdir from the empty-dir-p-cache
                     nil
                   ;; the directory-key doesn't match DIR so we just return
                   ;; the old-value, which means in fact that nothing changes
                   old-value))))))

(defun ecb-directory-empty-cache-clear ()
  "Clear the whole EMPTY-DIR-P-cache."
  (ecb-multicache-clear-subcache 'ecb-filename-cache 'EMPTY-DIR-P))

(defun ecb-directory-empty-cache-dump (&optional no-nil-value)
  "Dump the whole EMPTY-DIR-P-cache. If NO-NIL-VALUE is not nil then these
cache-entries are not dumped. This command is not intended for end-users of
ECB."
  (interactive "P")
  (ecb-multicache-print-subcache 'ecb-filename-cache
                                 'EMPTY-DIR-P no-nil-value))


;; accessors for the SOURCES-cache

(defun ecb-sources-cache-remove (dir)
  "Remove the cache-entry for DIR from the cache."
  (ecb-multicache-clear-value 'ecb-filename-cache dir 'SOURCES))

(defun ecb-sources-cache-add-full (dir cache-elem-full)
  "Add the full sources-cache CACHE-ELEM-FULL for DIR to the cache. If there
is already a full cache-entry then replace it. CACHE-ELEM-FULL has to be a
list as returned by `ecb-sources-cache-get-full'."
  (ecb-multicache-apply-to-value
   'ecb-filename-cache dir 'SOURCES
   (function (lambda (old-cached-value)
               (if (consp old-cached-value)
                   (progn
                     (setcar old-cached-value cache-elem-full)
                     old-cached-value)
                 (cons cache-elem-full nil))))))

(defun ecb-sources-cache-add-filtered (dir cache-elem-filtered)
  "Add the filtered sources-cache CACHE-ELEM-FILTERED for DIR to the cache. If
there is already a filtered cache-entry then replace it. CACHE-ELEM-FILTERED
has to be a list as returned by `ecb-sources-cache-get-filtered'."
  (ecb-multicache-apply-to-value
   'ecb-filename-cache dir 'SOURCES
   (function (lambda (old-cached-value)
               (if (consp old-cached-value)
                   (progn
                     (setcdr old-cached-value cache-elem-filtered)
                     old-cached-value)
                 (cons nil cache-elem-filtered))))))

(defun ecb-sources-cache-get-full (dir)
  "Return the full value of a cached-directory DIR, means the 3-element-list
\(tree-buffer-root, tree-buffer-displayed-nodes, sources-buffer-string). If no
cache-entry for DIR is available then nil is returned."
  (car (ecb-multicache-get-value 'ecb-filename-cache dir 'SOURCES)))

(defun ecb-sources-cache-get-filtered (dir)
  "Return the filtered value of a cached-directory DIR, means the
4-element-list \(tree-buffer-root, tree-buffer-displayed-nodes,
sources-buffer-string, filter-regexp). If no cache-entry for DIR is available
then nil is returned."
  (cdr (ecb-multicache-get-value 'ecb-filename-cache dir 'SOURCES)))

(defun ecb-sources-cache-clear ()
  "Clear the whole SOURCES-cache."
  (ecb-multicache-clear-subcache 'ecb-filename-cache 'SOURCES))

(defun ecb-sources-cache-dump (&optional no-nil-value)
  "Dump the whole SOURCES-cache. If NO-NIL-VALUE is not nil then these
cache-entries are not dumped. This command is not intended for end-users of
ECB."
  (interactive "P")
  (ecb-multicache-print-subcache 'ecb-filename-cache 'SOURCES no-nil-value))

;; accessors for the VC-cache

(defun ecb-vc-cache-add-file (file state checked-buffer-names)
  (ecb-multicache-put-value 'ecb-filename-cache file 'VC
                            (list state
                                  (ecb-subseq (current-time) 0 2)
                                  checked-buffer-names))
  state)

(defun ecb-vc-cache-add-dir (dir backend)
  (ecb-multicache-put-value 'ecb-filename-cache dir 'VC backend)
  backend)

(defun ecb-vc-cache-get (file)
  (ecb-multicache-get-value 'ecb-filename-cache file 'VC))

(defun ecb-vc-cache-remove (file)
  "Remove FILE from the VC-cache."
  (ecb-multicache-clear-value 'ecb-filename-cache file 'VC))

(defun ecb-vc-cache-remove-files-of-dir (dir)
  "Remove all files contained in DIR from the VC-cache."
  (let* ((dir-sep-string (ecb-directory-sep-string dir))
         (regexp (concat "^"
                         (regexp-quote dir)
                         (regexp-quote dir-sep-string)
                         "[^"
                         dir-sep-string
                         "]+$")))
    (save-match-data
      (ecb-multicache-mapsubcache
       'ecb-filename-cache 'VC
       (function (lambda (key old-value)
                   (if (and old-value
                            (string-match regexp key))
                       ;; the filename-key has a VC-cache value and matches the
                       ;; regexp above so its a cached file of DIR so we return
                       ;; nil ==> in fact we remove this file from the VC-cache
                       nil
                     ;; the filename-key doesn't match the regexp above so we
                     ;; just return the old-value, which means in fact that
                     ;; nothing changes
                     old-value)))))))


(defun ecb-vc-cache-clear ()
  "Clear the whole VC-cache."
  (ecb-multicache-clear-subcache 'ecb-filename-cache 'VC))

(defun ecb-vc-cache-dump (&optional no-nil-value)
  "Dump the whole VC-cache. If NO-NIL-VALUE is not nil then these
cache-entries are not dumped. This command is not intended for end-users of
ECB."
  (interactive "P")
  (ecb-multicache-print-subcache 'ecb-filename-cache 'VC no-nil-value))

;; accessors for the REMOTE-PATH cache

(defun ecb-remote-path-cache-add (path remote-path)
  "Add the value of REMOTE-PATH for PATH to the REMOTE-PATH-cache."
  (ecb-multicache-put-value 'ecb-filename-cache path 'REMOTE-PATH
                            remote-path))

(defun ecb-remote-path-cache-get (path)
  "Return the cached value for PATH from the REMOTE-PATH-cache."
  (ecb-multicache-get-value 'ecb-filename-cache path 'REMOTE-PATH))

(defun ecb-remote-path-cache-dump (&optional no-nil-value)
  "Dump the whole REMOTE-PATH-cache. If NO-NIL-VALUE is not nil then these
cache-entries are not dumped. This command is not intended for end-users of
ECB."
  (interactive "P")
  (ecb-multicache-print-subcache 'ecb-filename-cache 'REMOTE-PATH no-nil-value))

;; accessors for the HOST-ACCESSIBLE cache

(defun ecb-host-accessible-cache-add (host accessible-p)
  "Add the value of ACCESSIBLE-P to the HOST-ACCESSIBLE-cache with key HOST."
  (ecb-multicache-put-value 'ecb-filename-cache host 'HOST-ACCESSIBLE
                            (cons (current-time) accessible-p)))

(defun ecb-host-accessible-cache-get (host valid-time)
  "Get the accessible-p value from the HOST-ACCESSIBLE-cache. If the cache
entry is older then VALID-TIME \(in seconds) then it is discarded."
  (let ((value (ecb-multicache-get-value 'ecb-filename-cache host
                                         'HOST-ACCESSIBLE)))
    (if (or (null value)
            (> (ecb-time-diff (current-time) (car value) t) valid-time))
        ;; either not yet cached or outdated
        nil
      ;; return the valid cache-value
      (cdr value))))

(defun ecb-host-accessible-cache-dump (&optional no-nil-value)
  "Dump the whole HOST-ACCESSIBLE-cache. If NO-NIL-VALUE is not nil then these
cache-entries are not dumped. This command is not intended for end-users of
ECB."
  (interactive "P")
  (ecb-multicache-print-subcache 'ecb-filename-cache 'HOST-ACCESSIBLE no-nil-value))


;; ---- end of filename-cache implementation -----------------------

(defun ecb-file-browser-initialize-caches ()
  "Initialize the caches of the file-browser of ECB."
  (ecb-reset-history-filter)
  (ecb-filename-cache-init))

(defun ecb-file-browser-initialize (&optional no-caches)
  "Initialize the file-browser of ECB. If optional arg NO-CACHES is not nil
then the caches used by the file-browser will not be initialized."
  (setq ecb-path-selected-directory nil
        ecb-path-selected-source nil)
  (unless no-caches
    (ecb-file-browser-initialize-caches)))
  
(defun ecb-goto-window-directories ()
  "Make the ECB-directories window the current window.
If `ecb-use-speedbar-instead-native-tree-buffer' is 'dir then goto to the
speedbar-window."
  (interactive)
  (or (ecb-goto-ecb-window ecb-directories-buffer-name)
      (and (equal ecb-use-speedbar-instead-native-tree-buffer 'dir)
           (ecb-goto-window-speedbar))))

(defun ecb-goto-window-sources ()
  "Make the ECB-sources window the current window.
If `ecb-use-speedbar-instead-native-tree-buffer' is 'source then goto to the
speedbar-window."
  (interactive)
  (or (ecb-goto-ecb-window ecb-sources-buffer-name)
      (and (equal ecb-use-speedbar-instead-native-tree-buffer 'source)
           (ecb-goto-window-speedbar))))

(defun ecb-goto-window-history ()
  "Make the ECB-history window the current window."
  (interactive)
  (ecb-goto-ecb-window ecb-history-buffer-name))

(defun ecb-maximize-window-directories ()
  "Maximize the ECB-directories-window.
I.e. delete all other ECB-windows, so only one ECB-window and the
edit-window\(s) are visible \(and maybe a compile-window). Works also if the
ECB-directories-window is not visible in current layout."
  (interactive)
  (if (equal ecb-use-speedbar-instead-native-tree-buffer 'dir)
      (ecb-maximize-window-speedbar)
    (ecb-maximize-ecb-buffer ecb-directories-buffer-name t)))

(defun ecb-maximize-window-sources ()
  "Maximize the ECB-sources-window.
I.e. delete all other ECB-windows, so only one ECB-window and the
edit-window\(s) are visible \(and maybe a compile-window). Works also if the
ECB-sources-window is not visible in current layout."
  (interactive)
  (if (equal ecb-use-speedbar-instead-native-tree-buffer 'source)
      (ecb-maximize-window-speedbar)
    (ecb-maximize-ecb-buffer ecb-sources-buffer-name t)))

(defun ecb-maximize-window-history ()
  "Maximize the ECB-history-window.
I.e. delete all other ECB-windows, so only one ECB-window and the
edit-window\(s) are visible \(and maybe a compile-window). Works also if the
ECB-history-window is not visible in current layout."
  (interactive)
  (ecb-maximize-ecb-buffer ecb-history-buffer-name t))

(defecb-window-dedicator ecb-set-directories-buffer ecb-directories-buffer-name
  "Display the Directories-buffer in current window and make window dedicated."
  (let ((set-directories-buffer
         (not (equal ecb-use-speedbar-instead-native-tree-buffer 'dir))))
    ;; first we act depending on the value of
    ;; ecb-use-speedbar-instead-native-tree-buffer
    (when (not set-directories-buffer)
      (condition-case error-data
          (ecb-set-speedbar-buffer)
        ;; setting the speedbar buffer has failed so we set
        ;; set-directories-buffer to t ==> standard-directories-buffer is set!
        (error (message "%s" error-data)
               (setq set-directories-buffer t))))
    ;; maybe we need to set the standard directories buffer:
    ;; - if ecb-use-speedbar-instead-native-tree-buffer is not 'dir or
    ;; - if setting the speedbar buffer has failed.
    (when set-directories-buffer
      (if (null ecb-use-speedbar-instead-native-tree-buffer)
          (ignore-errors (ecb-speedbar-deactivate)))
      (switch-to-buffer ecb-directories-buffer-name))))

(defecb-window-dedicator ecb-set-sources-buffer ecb-sources-buffer-name
  "Display the Sources-buffer in current window and make window dedicated."
  (let ((set-sources-buffer
         (not (equal ecb-use-speedbar-instead-native-tree-buffer 'source))))
    ;; first we act depending on the value of
    ;; ecb-use-speedbar-instead-native-tree-buffer
    (when (not set-sources-buffer)
      (condition-case error-data
          (ecb-set-speedbar-buffer)
        ;; setting the speedbar buffer has failed so we set
        ;; set-sources-buffer to t ==> standard-sources-buffer is set!
        (error (message "%s" error-data)
               (setq set-sources-buffer t))))
    ;; maybe we need to set the standard sources buffer:
    ;; - if ecb-use-speedbar-instead-native-tree-buffer is not 'source or
    ;; - if setting the speedbar buffer has failed.
    (when set-sources-buffer
      (if (null ecb-use-speedbar-instead-native-tree-buffer)
          (ignore-errors (ecb-speedbar-deactivate)))
      (switch-to-buffer ecb-sources-buffer-name))))


(defecb-window-dedicator ecb-set-history-buffer ecb-history-buffer-name
  "Display the History-buffer in current window and make window dedicated."
  (switch-to-buffer ecb-history-buffer-name))


(defun ecb-directories-sources-history-buffer-sync (&optional force)
  "Synchronizing the basic tree-buffers of ECB.

Under the following additional conditions some tasks are performed:

- Current buffer is a file-buffer and either FORCE is not nil or the buffer
  is different from the source-file currently displayed in the
  ECB-tree-buffers:

  Synchronizing all tree-buffers with the current buffer

- Current buffer is a dired-buffer:

  Synchronizing the directory- and sources-tree-buffer if visible"
  (let ((filename (buffer-file-name (current-buffer))))
    (cond ( ;; synchronizing for real filesource-buffers
           (and filename
                (ecb-buffer-or-file-readable-p)
                (or force
                    (not (ecb-string= filename ecb-path-selected-source))))
          
           ;; * KB: Problem: seems this little sleep is necessary because
           ;;   otherwise jumping to certain markers in new opened files (e.g.
           ;;   with next-error etc. ) doesn´t work correct. Can´t debug down
           ;;   this mysterious thing! Regardless of the size of the file to
           ;;   load, this 0.1 fraction of a sec is enough!
           ;; * KB: With current ECB implementation this sit-for seems not
           ;;   longer necessary, it works with every Emacs version correct.
           ;;   Therefore i comment out the sit-for until this error occurs
           ;;   again.               
           ;;           (sit-for 0.1)
               
           ;; if the file is not located in any of the paths in
           ;; `ecb-source-path' or in the paths returned from
           ;; `ecb-source-path-functions' we must at least add the new
           ;; source path temporally to our paths. But the user has also
           ;; the choice to save it for future sessions too.
           (if (null (ecb-matching-source-paths filename))
               (let* ((norm-filename (ecb-fix-filename filename))
                      (remote-path (ecb-remote-path norm-filename))
                      (source-path (if (car ecb-add-path-for-not-matching-files)
                                       ;; we always add the only the root
                                       ;; as source-path
                                       (if remote-path
                                           ;; for a remote-path we add the
                                           ;; host+ the root of the host
                                           (concat (car remote-path) "/")
                                         ;; filename is a local-path
                                         (if (= (aref norm-filename 0) ?/)
                                             ;; for Unix-style-path we add the
                                             ;; root-dir
                                             (substring norm-filename 0 1)
                                           ;; for win32-style-path we add
                                           ;; the drive; because
                                           ;; `ecb-fix-filename' also
                                           ;; converts cygwin-path-style
                                           ;; to win32-path-style here
                                           ;; also the drive is added.
                                           (substring norm-filename 0 2)))
                                     ;; add the full directory as source-path
                                     (ecb-file-name-directory norm-filename))))
                 (ecb-add-source-path source-path (ecb-fix-filename source-path)
                                      (not (cdr ecb-add-path-for-not-matching-files)))))

           ;; now we can be sure that a matching source-path exists
               
           ;; Klaus: The explicit update of the directories buffer is not
           ;; necessary because the sync with the current source is done by
           ;; `ecb-select-source-file'!
           ;;           (ecb-update-directories-buffer)
           (ecb-select-source-file filename force)
           (ecb-update-methods-buffer--internal 'scroll-to-begin)
           (setq ecb-major-mode-selected-source major-mode)

           ;; Klaus Berndl <klaus.berndl@sdm.de>: is now be done at the
           ;; end of `ecb-rebuild-methods-buffer-with-tagcache' which is
           ;; called by `ecb-update-methods-buffer--internal'!

           ;; selected source has changed, therefore we must initialize
           ;; ecb-selected-tag again.
           (ecb-tag-sync 'force)
           )
              
          ( ;; synchronizing for dired-mode
           (eq major-mode 'dired-mode)
           (ecb-set-selected-directory
            (or (and (stringp dired-directory)
                     (ecb-file-exists-p dired-directory)
                     dired-directory)
                (and (listp dired-directory)
                     (car dired-directory)))))
          (t nil))))

(defun ecb-expand-directory-tree (path node)
  "Expands the directory part so the node representing PATH is visible.
Start with the childrens of NODE. Return not nil when an expansion has been
done \(so normally the tree-buffer must be rebuild). Return nil if the
expansion-state of the tree can show without any further expansion the node
representing PATH."
  (catch 'exit
    (dolist (child (tree-node->children node))
      (let ((data (tree-node->data child)))
        (when (and (>= (length path) (length data))
                   (ecb-string= (substring path 0 (length data)) data)
                   (or (= (length path) (length data))
                       (eq (elt path (length data))
                           (ecb-directory-sep-char path))))
          (let ((was-expanded (or (not (tree-node->expandable child))
                                  (tree-node->expanded child))))
            (setf (tree-node->expanded child) t)
            (ecb-update-directory-node child)
            (throw 'exit
                   (or (when (> (length path) (length data))
                         (ecb-expand-directory-tree path child))
                       (not was-expanded)))))))))


(defun ecb-check-directory-for-caching (dir number-of-contents)
  "Return not nil if DIR matches not any regexp of the option
`ecb-cache-directory-contents-not' but matches at least one regexp in
`ecb-cache-directory-contents' and NUMBER-OF-CONTENTS is greater then the
related threshold."
  (and (not (catch 'exit
              (dolist (elem ecb-cache-directory-contents-not)
                (let ((case-fold-search t))
                  (save-match-data
                    (if (string-match (car elem) dir)
                        (throw 'exit (car elem))))
                  nil))))
       (catch 'exit
         (dolist (elem ecb-cache-directory-contents)
           (let ((case-fold-search t))
             (save-match-data
               (if (and (string-match (car elem) dir)
                        (> number-of-contents (cdr elem)))
                   (throw 'exit (car elem))))
             nil)))))


(defun ecb-check-directory-for-source-regexps (dir)
  "Return the related source-exclude-include-regexps of
`ecb-source-file-regexps' if DIR matches any directory-regexp in
`ecb-source-file-regexps'."
  (ecb-match-regexp-list dir ecb-source-file-regexps 'car 'cdr))


(defun ecb-files-from-cvsignore (dir)
  "Return an expanded list of filenames which are excluded by the .cvsignore
file in current directory."
  (let ((cvsignore-content (ecb-file-content-as-string
                            (ecb-expand-file-name ".cvsignore" dir)))
        (files nil))
    (when cvsignore-content
      (dolist (f (split-string cvsignore-content))
        (setq files (append (ecb-directory-files dir nil
                                                 (wildcard-to-regexp f) t)
                            files)))
      files)))


(defun ecb-check-directory-for-cvsignore-exclude (dir)
  "Return not nil if DIR matches a regexp in `ecb-sources-exclude-cvsignore'."
  (ecb-match-regexp-list dir ecb-sources-exclude-cvsignore))

(defun ecb-get-sources-sort-function (sort-method &optional ignore-case)
  "According to SORT-METHOD \(which can either be 'name, 'extension or nil)
and IGNORE-CASE return a function which can be used as argument for `sort'."
  (case sort-method
    (name
     (function (lambda (a b)
                 (ecb-string< a b ecb-sources-sort-ignore-case))))
    (extension
     (function
      (lambda(a b)
        (let ((ext-a (ecb-file-name-extension a t))
              (ext-b (ecb-file-name-extension b t)))
          (if (ecb-string= ext-a ext-b ecb-sources-sort-ignore-case)
              (ecb-string< a b ecb-sources-sort-ignore-case)
            (ecb-string< ext-a ext-b ecb-sources-sort-ignore-case))))))
    (otherwise
     (function (lambda (a b)
                 nil)))))


(defun ecb-get-files-and-subdirs (dir)
  "Return a cons cell where car is a list of all files to display in DIR and
cdr is a list of all subdirs to display in DIR. Both lists are sorted
according to `ecb-sources-sort-method'."
  (or (ecb-files-and-subdirs-cache-get dir)
      ;; dir is not cached
      (let ((files (ecb-directory-files dir nil nil t))
            (source-regexps (or (ecb-check-directory-for-source-regexps
                                 (ecb-fix-filename dir))
                                '(("") (""))))
            (cvsignore-files (if (ecb-check-directory-for-cvsignore-exclude dir)
                                 (ecb-files-from-cvsignore dir)))
            sorted-files source-files subdirs cached-value)
        ;; if necessary sort FILES
        (setq sorted-files
              (if ecb-sources-sort-method
                  (sort files (ecb-get-sources-sort-function
                               ecb-sources-sort-method))
                files))
        ;; divide real files and subdirs. For really large directories (~ >=
        ;; 2000 entries) this is the performance-bottleneck in the
        ;; file-browser of ECB.
        (dolist (file sorted-files)
          (if (ecb-file-directory-p (ecb-fix-filename dir file))
              (when (not (ecb-check-dir-exclude file))
;;                 (when (not (ecb-file-accessible-directory-p file))
;;                   (ecb-merge-face-into-text file
;;                                             ecb-directory-not-accessible-face))
                (setq subdirs (append subdirs (list file))))
            (when (and (not (member file cvsignore-files))
                       (or (ecb-match-regexp-list file (cadr source-regexps))
                           (not (ecb-match-regexp-list file (car source-regexps)))))
              (setq source-files (append source-files (list file))))))
        
        (setq cached-value (cons source-files subdirs))
        ;; check if this directory must be cached
        (if (ecb-check-directory-for-caching dir (length sorted-files))
            (ecb-files-and-subdirs-cache-add dir cached-value))
        ;; return the result
        cached-value)))


(defun ecb-update-sources-buffer (dir-before-update)
  "Updates the sources-buffer with all sources contained in
`ecb-path-selected-directory' - the contents are either newly computed or come
from the `ecb-sources-cache'. DIR-BEFORE-UPDATE is the directory which was
selected before this update."

  ;; Here we add a cache-mechanism which caches for each path the node-tree
  ;; and the whole buffer-string of the sources-buffer. A cache-elem would be
  ;; removed from the cache if a directory is POWER-clicked in the directories
  ;; buffer because this is the only way to synchronize the sources-buffer
  ;; with the disk-contents of the clicked directory. This works because the
  ;; tree of the sources-buffer contains only not expandable nodes (see the
  ;; comment in `ecb-rebuild-methods-buffer-with-tagcache'). If we would
  ;; make the nodes in the Sources-buffer "expandable" this caching would not
  ;; work!
  
  (ecb-exec-in-window ecb-sources-buffer-name
    ;; if we have a filtered cache we must display it - otherwise we use the
    ;; full cache if there is any
    (let ((cache-elem (or (ecb-sources-cache-get-filtered ecb-path-selected-directory)
                          (ecb-sources-cache-get-full ecb-path-selected-directory))))
      (if cache-elem
          (progn
            (tree-buffer-set-root (nth 0 cache-elem))
            (tree-buffer-update nil (cons (nth 2 cache-elem)
                                          (nth 1 cache-elem))))
        (let ((new-tree (tree-node-new-root))
              (old-children (tree-node->children (tree-buffer-get-root)))
              (new-cache-elem nil))
          ;; building up the new files-tree
          (ecb-tree-node-add-files
           new-tree
           ecb-path-selected-directory
           (car (ecb-get-files-and-subdirs ecb-path-selected-directory))
           ecb-sources-nodetype-sourcefile
           ecb-show-source-file-extension old-children t)

          ;; updating the buffer itself
          (tree-buffer-set-root new-tree)
          (tree-buffer-update)

          ;; check if the sources buffer for this directory must be
          ;; cached: If yes update the cache
          (when (ecb-check-directory-for-caching
                 ecb-path-selected-directory
                 (tree-buffer-number-of-displayed-nodes))
            (setq new-cache-elem (list (tree-buffer-get-root)
                                       (tree-buffer-displayed-nodes-copy)
                                       (ecb-buffer-substring (point-min)
                                                             (point-max))))
            (ecb-sources-cache-add-full ecb-path-selected-directory
                                        new-cache-elem))))
           
      (when (not (ecb-string= dir-before-update ecb-path-selected-directory))
        (ecb-scroll-window (point-min) (point-min))))))

(defun ecb-sources-filter-by-ext (ext-str)
  "Filter the sources by extension EXT-STR."
  (if (= (length ext-str) 0)
      (ecb-apply-filter-to-sources-buffer
       "^[^.]+$" ;; matches only filenames with no extension
       "No ext.")
    (ecb-apply-filter-to-sources-buffer
     (format "\\.%s\\'" ext-str)
     (format "*.%s" ext-str))))

(tree-buffer-defpopup-command ecb-popup-sources-filter-by-ext
  "Filter the sources by extension by popup."
  (ecb-sources-filter-by-ext
   (read-string "Insert the filter-extension without leading dot: "
                (and node
                     (ecb-file-name-extension (tree-node->data node))))))

(defun ecb-sources-filter-by-regexp (&optional regexp filter-display)
  "Filter history entries by REGEXP. If the first optional argument REGEXP is
nil then it asks for a regexp. If second argument FILTER-DISPLAY is not nil
then it is displayed in the modeline of the history-buffer for current
regexp-filter. Otherwise the regexp itself."
(let ((regexp-str (or regexp (read-string "Insert the filter-regexp: "))))
    (if (> (length regexp-str) 0)
        (ecb-apply-filter-to-sources-buffer regexp-str filter-display))))
  
(tree-buffer-defpopup-command ecb-popup-sources-filter-by-regexp
  "Filter the sources by regexp by popup."
  (ecb-sources-filter-by-regexp))
  
(tree-buffer-defpopup-command ecb-popup-sources-filter-none
  "Remove any filter from the sources by popup."
  (ecb-apply-filter-to-sources-buffer nil))
  

(defun ecb-sources-filter ()
  "Apply a filter to the sources-buffer to reduce the number of entries.
So you get a better overlooking. There are three choices:
- Filter by extension: Just insert the extension you want the Sources-buffer
  being filtered. Insert the extension without leading dot!
- Filter by regexp: Insert the filter as regular expression.
- No filter: This means to display an entry for every file in the current
  selected directory \(all except these filter already filtered out by
  `ecb-source-file-regexps' and `ecb-sources-exclude-cvsignore').
Such a filter is only applied to the current selected directory, i.e. each
directory has its own filtered sources-buffer."
  (interactive)
  (let ((choice (ecb-query-string "Filter sources by:"
                                  '("extension" "regexp" "nothing"))))
    (cond ((ecb-string= choice "extension")
           (ecb-sources-filter-by-ext
            (read-string "Insert the filter-extension without leading dot: ")))
          ((ecb-string= choice "regexp")
           (ecb-sources-filter-by-regexp))
          (t (ecb-apply-filter-to-sources-buffer nil)))))

(defun ecb-sources-filter-modeline-prefix (buffer-name sel-dir sel-source)
  "Compute a mode-line prefix for the Sources-buffer so the current filter
applied to the sources is displayed. This function is only for using by
the option `ecb-mode-line-prefixes'."
  (let ((filtered-cache-elem (ecb-sources-cache-get-filtered sel-dir)))
    (if (null filtered-cache-elem)
        nil ;; no prefix if no filter
      (format "[Filter: %s]" (cdr (nth 3 filtered-cache-elem))))))

(defun ecb-apply-filter-to-sources-buffer (filter-regexp &optional filter-display)
  "Apply the regular expression FILTER-REGEXP to the files of
`ecb-path-selected-directory' and display only the filtered files in the
Sources-buffer. If FILTER-REGEXP is nil then any applied filter is removed and
all files are displayed. Returns t if the filter has been applied otherwise
nil. Returns 'window-not-visible if the ECB-sources-buffer is not visible."
  (prog1
      (ecb-exec-in-window ecb-sources-buffer-name
        (if (or (null filter-regexp) (= (length filter-regexp) 0))
            ;; no filtering
            (progn
              ;; remove the filtered cache by setting it to nil
              (ecb-sources-cache-add-filtered ecb-path-selected-directory nil)
              ;; update the sources buffer - because the filtered cache is nil
              ;; the full sources are displayed.
              (ecb-update-sources-buffer ecb-path-selected-directory)
              (tree-buffer-highlight-node-data ecb-path-selected-source)
              nil)
          ;; apply the filter-regexp
          (let ((new-tree (tree-node-new-root))
                (old-children (tree-node->children (tree-buffer-get-root)))
                (all-files (car (ecb-get-files-and-subdirs ecb-path-selected-directory)))
                (filtered-files nil))
            (dolist (file all-files)
              (if (string-match filter-regexp file)
                  (setq filtered-files
                        (cons file filtered-files))))
            (if (null filtered-files)
                (progn
                  (ecb-apply-filter-to-sources-buffer nil)
                  (message "ECB has not applied this filter because it would filter out all files!")
                  nil)
              ;; building up the new files-tree
              (ecb-tree-node-add-files
               new-tree
               ecb-path-selected-directory
               (nreverse filtered-files)
               ecb-sources-nodetype-sourcefile
               ecb-show-source-file-extension old-children t)

              ;; updating the buffer itself
              (tree-buffer-set-root new-tree)
              (tree-buffer-update)
              (ecb-scroll-window (point-min) (point-min))
              (tree-buffer-highlight-node-data ecb-path-selected-source)

              ;; add the new filter to the cache, so the next call to
              ;; `ecb-update-sources-buffer' displays the filtered sources.
              (ecb-sources-cache-add-filtered ecb-path-selected-directory
                                              (list (tree-buffer-get-root)
                                                    (tree-buffer-displayed-nodes-copy)
                                                    (ecb-buffer-substring (point-min)
                                                                          (point-max))
                                                    (cons filter-regexp
                                                          (or filter-display
                                                              filter-regexp))))
              t))))
    ;; now we update the mode-lines so the current filter (can be no filter) is
    ;; displayed in the mode-line. See `ecb-sources-filter-modeline-prefix'.
    (ecb-mode-line-format)))

(defun ecb-matching-source-paths (path-to-match &optional sorted)
  "Return all source-paths of `ecb-source-path' which match PATH-TO-MATCH. If
SORTED is not nil then the paths are sorted by descending length, means the
longest path \(which is the best matching) is the first elem and the shortest
path the last elem. Otherwise the matching paths are returned in that sequence
they occur in `ecb-source-paths'."
  (let* ((p-t-m (ecb-fix-filename path-to-match))
         (normed-current-source-paths
          (mapcar (function (lambda (elem)
                              (ecb-fix-filename (if (listp elem) (car elem) elem))))
                  (append (ecb-get-source-paths-from-functions)
                          ecb-source-path)))
         (matching-paths
          (delq nil
                (mapcar (lambda (elem)
                          (save-match-data
                            (if (string-match (concat "^" (regexp-quote elem))
                                              p-t-m)
                                elem)))
                        normed-current-source-paths))))
    (if (not sorted)
        matching-paths
      (sort matching-paths
            (lambda (lhs rhs)
              (> (length lhs) (length rhs)))))))

(defun ecb-get-best-matching-source-path (path)
  "Return the best-matching source-path for PATH."
  (car (ecb-matching-source-paths path t)))

(defun ecb-set-selected-directory (path &optional force)
  "Set the contents of the ECB-directories and -sources buffer correct for the
value of PATH. If PATH is equal to the value of `ecb-path-selected-directory'
then nothing is done unless first optional argument FORCE is not nil."
  (let ((last-dir ecb-path-selected-directory))
    (setq ecb-path-selected-directory (ecb-fix-filename path))
    ;; if ecb-path-selected-directory has not changed then there is no need
    ;; to do anything here because neither the content of directory buffer
    ;; nor the content of the sources buffer can have been changed!
    (when (or force (not (ecb-string= last-dir ecb-path-selected-directory)))
      (when (or (not (ecb-show-sources-in-directories-buffer-p))
                ecb-auto-expand-directory-tree)
        (ecb-exec-in-window ecb-directories-buffer-name
          (let (start was-expanded)
            (when ecb-auto-expand-directory-tree
              ;; Expand tree to show selected directory
              (setq start
                    (if (equal ecb-auto-expand-directory-tree 'best)
                        ;; If none of the source-paths in the buffer
                        ;; `ecb-directories-buffer-name' matches then nil
                        ;; otherwise the node of the best matching
                        ;; source-path
                        (let ((best-source-path
                               (ecb-get-best-matching-source-path
                                ecb-path-selected-directory)))
                          (if best-source-path
                              (tree-buffer-search-displayed-node-list
                               (function
                                (lambda (node)
                                  (if (and (tree-buffer-node-data-equal-p
                                            (tree-node->data node)
                                            (ecb-fix-filename best-source-path))
                                           ;; only nodes of level 0 (ie. with
                                           ;; parent == root) can be source-paths
                                           (eq (tree-buffer-get-root)
                                               (tree-node->parent node)))
                                      node))))))
                      ;; we start at the root node
                      (tree-buffer-get-root)))
              (when (and (equal ecb-auto-expand-directory-tree 'best)
                         start)
                (setq was-expanded (or (not (tree-node->expandable start))
                                       (tree-node->expanded start)))
                ;; expand the best-match node itself
                (setf (tree-node->expanded start) t)
                ;; This function ensures a correct expandable-state of
                ;; start-node
                (ecb-update-directory-node start))
              ;; start recursive expanding of either the best-matching node or
              ;; the root-node itself.
              (if (or (ecb-expand-directory-tree ecb-path-selected-directory
                                                 (or start
                                                     (tree-buffer-get-root)))
                      (not was-expanded))
                  (tree-buffer-update)
                (tree-buffer-recenter start (selected-window))
                ;; sometimes we do not need a full tree-buffer-update, even
                ;; when FORCE is not nil. But we have to restart the
                ;; directories-buffer stealthy-state.
                (and force (ecb-stealth-tasks-after-directories-update))))
            ;;              (ecb-expand-directory-tree ecb-path-selected-directory
            ;;                                           (or start
            ;;                                               (tree-buffer-get-root)))
            ;;                (tree-buffer-update))
            (when (not (ecb-show-sources-in-directories-buffer-p))
              (tree-buffer-highlight-node-data ecb-path-selected-directory
                                               start)))))
      ;; now we update the sources buffer for `ecb-path-selected-directory'
      (ecb-update-sources-buffer last-dir)
      ;; now we run the hooks
      (run-hook-with-args 'ecb-after-directory-change-hook
                          last-dir ecb-path-selected-directory)
      ))
  
  ;; set the default-directory of each tree-buffer to current selected
  ;; directory so we can open files via find-file from each tree-buffer.
  ;; is this necessary if neither dir.- nor sources-buffer-contents have been
  ;; changed? I think not but anyway, doesn't matter, costs are very low.
  (save-excursion
    (dolist (buf (ecb-tree-buffers-buffer-list))
      (set-buffer buf)
      (setq default-directory
            (concat ecb-path-selected-directory
                    (and (not (= (aref ecb-path-selected-directory
                                       (1- (length ecb-path-selected-directory)))
                                 (ecb-directory-sep-char ecb-path-selected-directory)))
                         (ecb-directory-sep-string ecb-path-selected-directory))))))
  ;; set the modelines of all visible tree-buffers new
  (ecb-mode-line-format))


(defun ecb-get-source-name (filename)
  "Returns the source name of a file."
  (let ((f (ecb-file-name-nondirectory filename)))
    (if ecb-show-source-file-extension
        f
      (ecb-file-name-sans-extension f))))


(defun ecb-select-source-file (filename &optional force)
  "Updates the directories, sources and history buffers to match the filename
given. If FORCE is not nil then the update of the directories buffer is done
even if current directory is equal to `ecb-path-selected-directory'."
  (ecb-set-selected-directory (ecb-file-name-directory filename) force)
  (setq ecb-path-selected-source filename)
  
  ;; Update directory buffer
  (when (ecb-show-sources-in-directories-buffer-p)
    (ecb-exec-in-window ecb-directories-buffer-name
      (tree-buffer-highlight-node-data ecb-path-selected-source)))
    
  ;; Update source buffer
  (ecb-exec-in-window ecb-sources-buffer-name
    (tree-buffer-highlight-node-data ecb-path-selected-source))

  ;; Update history buffer always regardless of visibility of history window
  (ecb-add-item-to-history-buffer ecb-path-selected-source)
  (ecb-sort-history-buffer)
  ;; Update the history window only if it is visible
  (ecb-update-history-window ecb-path-selected-source))


(defvar ecb-history-filter nil
  "A cons-cell where car is the filter-function and the cdr is a string how
the current active filter should be displayed in the modeline of the
History-buffer. The filter-function gets the filename of an existing
file-buffer and has to return not nil if for this filename a history-entry
should be added.")

(defun ecb-reset-history-filter ()
  "Reset the `ecb-history-filter' so all file-buffers are displayed."
  (setq ecb-history-filter '(identity . nil)))

(defun ecb-history-filter-reset-p ()
  (null (cdr ecb-history-filter)))

(ecb-reset-history-filter)

(defun ecb-add-all-buffers-to-history ()
  "Add all current file-buffers to the history-buffer of ECB.
Dependend on the value of `ecb-history-sort-method' afterwards the history is
sorted either by name or by extension. If `ecb-history-sort-method' is nil the
most recently used buffers are on the top of the history and the seldom used
buffers at the bottom."
  (interactive)
  (ecb-reset-history-filter)
  (ecb-add-buffers-to-history))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: return t if filter applied else nil
(defun ecb-add-buffers-to-history ()
  "Add exactly these currently existing file-buffers to the history-buffer
which are not filtered out by current value of `ecb-history-filter'. Returns t
if the filter has been applied otherwise nil."
  ;; first we clear out the history buffer
  (save-excursion
    (set-buffer ecb-history-buffer-name)
    (tree-buffer-clear-tree))
  (mapc (function (lambda (buf)
                    (when (buffer-file-name buf)
                      (ecb-add-item-to-history-buffer
                       (buffer-file-name buf)))))
        (reverse (buffer-list)))
  (ecb-sort-history-buffer)
  (ecb-update-history-window (buffer-file-name ecb-last-source-buffer))
  (prog1
      (if (and (save-excursion
                 (set-buffer ecb-history-buffer-name)
                 (tree-buffer-empty-p))
               (not (ecb-history-filter-reset-p)))
          (progn
            (ecb-add-all-buffers-to-history)
            (message "ECB has not applied this filter because it would filter out all entries!")
            nil)
        t)
    ;; now the modeline has to display the current filter
    (ecb-mode-line-format)))

  
(defun ecb-history-filter-modeline-prefix (buffer-name sel-dir sel-source)
  "Compute a mode-line prefix for the History-buffer so the current filter
applied to the history-entries is displayed. This function is only for using
by the option `ecb-mode-line-prefixes'."
  (and (cdr ecb-history-filter)
       (format "[Filter: %s]" (cdr ecb-history-filter))))


(defun ecb-add-item-to-history-buffer (filename)
  "Add a new item for FILENAME to the history buffer if the current filter of
`ecb-history-filter' does not filter out this file."
  (save-excursion
    (set-buffer ecb-history-buffer-name)
    (tree-node-remove-child-by-data (tree-buffer-get-root) filename)
    (when (and (not (ecb-check-filename-for-history-exclude filename))
               (funcall (car ecb-history-filter) filename))
      (tree-node-add-children
       (tree-buffer-get-root)
       (tree-node-new
        (let ((file-1 (if (eq ecb-history-item-name 'buffer-name)
                          (let ((b (get-file-buffer filename)))
                            (if b
                                (buffer-name b)
                              (ecb-get-source-name filename)))
                        (ecb-get-source-name filename)))
              (dir (ecb-file-name-directory filename)))
          (if (and (ecb-vc-directory-should-be-checked-p dir)
                   (ecb-vc-managed-dir-p dir))
              (ecb-vc-generate-node-name file-1
                                         (nth 0 (ecb-vc-cache-get filename)))
            (ecb-generate-node-name file-1 -1 "leaf"
                                    ecb-sources-buffer-name)))
        ecb-history-nodetype-sourcefile
        filename t)
       'at-beginning))))


(defun ecb-sort-history-buffer ()
  "Sort the history buffer according to `ecb-history-sort-method'."
  (when ecb-history-sort-method
    (save-excursion
      (set-buffer ecb-history-buffer-name)
      (tree-node-sort-children
       (tree-buffer-get-root)
       (case ecb-history-sort-method
         (name
          (function (lambda (l r)
                      (let* ((l0 (tree-node->name l))
                             (r0 (tree-node->name r))
                             (vc-ascii-icon-length-l (get-text-property
                                                      0
                                                      'ecb-vc-ascii-icon-length
                                                      l0))
                             (vc-ascii-icon-length-r (get-text-property
                                                      0
                                                      'ecb-vc-ascii-icon-length
                                                      r0))
                             (prefix-length-l (or vc-ascii-icon-length-l
                                                  (or (get-text-property
                                                       0
                                                       'tree-buffer-image-length
                                                       l0)
                                                      0)))
                             (prefix-length-r (or vc-ascii-icon-length-r
                                                  (or (get-text-property
                                                       0
                                                       'tree-buffer-image-length
                                                       r0)
                                                      0)))
                             (l1 (substring l0 prefix-length-l))
                             (r1 (substring r0 prefix-length-r)))
                        (ecb-string< l1 r1 ecb-history-sort-ignore-case)))))
         (extension
          (function
           (lambda (l r)
             (let* ((l0 (tree-node->name l))
                    (r0 (tree-node->name r))
                    (vc-ascii-icon-length-l (get-text-property
                                             0
                                             'ecb-vc-ascii-icon-length
                                             l0))
                    (vc-ascii-icon-length-r (get-text-property
                                             0
                                             'ecb-vc-ascii-icon-length
                                             r0))
                    (prefix-length-l (or vc-ascii-icon-length-l
                                         (or (get-text-property
                                              0
                                              'tree-buffer-image-length
                                              l0)
                                             0)))
                    (prefix-length-r (or vc-ascii-icon-length-r
                                         (or (get-text-property
                                              0
                                              'tree-buffer-image-length
                                              r0)
                                             0)))
                    (l1 (substring l0 prefix-length-l))
                    (r1 (substring r0 prefix-length-r))
                    (ext-l (ecb-file-name-extension l1 t))
                    (ext-r (ecb-file-name-extension r1 t)))
               (if (ecb-string= ext-l ext-r ecb-history-sort-ignore-case)
                   (ecb-string< l1 r1 ecb-history-sort-ignore-case)
                 (ecb-string< ext-l ext-r ecb-history-sort-ignore-case))))))
         (otherwise
          (function (lambda (l r)
                      nil))))))))


(defun ecb-update-history-window (&optional filename)
  "Updates the history window and highlights the item for FILENAME if given."
  (ecb-exec-in-window ecb-history-buffer-name
    (tree-buffer-update)
    (tree-buffer-highlight-node-data filename)))

(defun ecb-set-selected-source (filename other-edit-window
					 no-edit-buffer-selection hide)
  "Updates all the ECB buffers and loads the file. The file is also
displayed unless NO-EDIT-BUFFER-SELECTION is set to non nil. In such case the
file is only loaded invisible in the background, all semantic-parsing and
ECB-Buffer-updating is done but the content of the main-edit window is not
changed. For the allowed values of OTHER-EDIT-WINDOW see
`ecb-combine-ecb-button/edit-win-nr'. If HIDE is not nil then ECB hides the
ecb-windows after displaying the file in an edit-window."
  (ecb-select-source-file filename)
  (if no-edit-buffer-selection
      ;; load the selected source in an invisible buffer, do all the
      ;; updating and parsing stuff with this buffer in the background and
      ;; display the methods in the METHOD-buffer. We can not go back to
      ;; the edit-window because then the METHODS buffer would be
      ;; immediately updated with the methods of the edit-window.
      (save-excursion
        (set-buffer (find-file-noselect filename))
        (ecb-update-methods-buffer--internal 'scroll-to-begin))
    ;; open the selected source in the edit-window and do all the update and
    ;; parsing stuff with this buffer
    (ecb-find-file-and-display ecb-path-selected-source
			       other-edit-window)
    (ecb-update-methods-buffer--internal 'scroll-to-begin)
    (setq ecb-major-mode-selected-source major-mode)
    (ecb-tag-sync 'force)
    (if hide
        (ecb-hide-ecb-windows))))
             

(defun ecb-clear-history ()
  "Clears the ECB history-buffer."
  (interactive)
  (unless (or (not ecb-minor-mode)
              (not (equal (selected-frame) ecb-frame)))
    (ecb-exec-in-window ecb-history-buffer-name
      (tree-buffer-clear-tree)
      (tree-buffer-update)
      (tree-buffer-highlight-node-data ecb-path-selected-source))))



(defun ecb-update-directory-node (node)
  "Updates the directory node NODE and add all subnodes if any."
  (let ((old-children (tree-node->children node))
        (path (tree-node->data node)))
    (setf (tree-node->children node) nil)
    (if (ecb-file-accessible-directory-p path)
        (let ((files-and-dirs (ecb-get-files-and-subdirs path)))
          (ecb-tree-node-add-files node path (cdr files-and-dirs)
                                   ecb-directories-nodetype-directory
                                   t old-children)
          (if (ecb-show-sources-in-directories-buffer-p)
              (ecb-tree-node-add-files node path (car files-and-dirs)
                                       ecb-directories-nodetype-sourcefile
                                       ecb-show-source-file-extension
                                       old-children t))
          (setf (tree-node->expandable node)
                (or (tree-node->children node)))
          ;; if node is not expandable we set its expanded state to nil
          (setf (tree-node->expanded node)
                (if (not (tree-node->expandable node))
                    nil
                  (tree-node->expanded node)))))))


(defun ecb-get-source-paths-from-functions ()
  "Return a list of paths found by querying `ecb-source-path-functions'."
  (let ((func ecb-source-path-functions)
	(paths nil)
	(rpaths nil))
    (while func
      (setq paths (append paths (funcall (car ecb-source-path-functions)))
	    func (cdr func)))
    (while paths
      (setq rpaths (cons (ecb-fix-filename (car paths)) rpaths)
	    paths (cdr paths)))
    rpaths))


(defun ecb-update-directories-buffer ()
  "Updates the ECB directories buffer."
  (interactive)
  (unless (or (not ecb-minor-mode)
              (not (equal (selected-frame) ecb-frame)))
    (ecb-exec-in-window ecb-directories-buffer-name
      (let* ((node (tree-buffer-get-root))
             (old-children (tree-node->children node))
             (paths (append (ecb-get-source-paths-from-functions)
                            ecb-source-path)))
        (setf (tree-node->children node) nil)
        (dolist (dir paths)
          (let* ((path (if (listp dir) (car dir) dir))
                 (remote-path (ecb-remote-path path))
                 (norm-dir nil)
                 (name nil)
                 (not-accessible nil))
            (if (or (not remote-path)
                    (ecb-host-accessible-p (nth 1 remote-path)))
                (progn
                  (setq norm-dir (ecb-fix-filename path nil t))
                  (setq name (if (listp dir) (cadr dir) norm-dir))
                  (if (ecb-file-accessible-directory-p norm-dir)
                      (tree-node-add-children
                       node
                       (ecb-new-child old-children name
                                      ecb-directories-nodetype-sourcepath
                                      norm-dir
                                      nil
                                      (if ecb-truncate-long-names
                                          'beginning)))
                    (setq not-accessible t)))
              (setq not-accessible t))
            (when not-accessible
              (if (listp dir)
                  (ecb-warning "Source-path %s with alias %s is not accessible - ignored!"
                               (car dir) (cadr dir))
                (ecb-warning "Source-path %s is not accessible - ignored!" dir)))))
        (tree-buffer-update)))
    ))

;; remote-path stuff 

(defsubst ecb-host-accessible-valid-time (host)
  "Get the valid-cache-time of a remote HOST concering its ping-state. If host
doesn't match any regexp of `ecb-host-accessible-check-valid-time' then return
60 seconds."
  (or (ecb-match-regexp-list host ecb-host-accessible-check-valid-time
                             'car 'cdr)
      60))

;; (ecb-host-accessible-valid-time "ecb.sourceforge.net")

(defun ecb-host-accessible-p (host)
  "Return not nil if HOST is accessible."
  (let ((value (ecb-host-accessible-cache-get
                host (ecb-host-accessible-valid-time host))))
    (case value
      (NOT-ACCESSIBLE nil)
      ((nil) ;; not cached or outdated
       (let* ((options (append ecb-ping-options (list host)))
              (result (equal 0 (apply 'call-process
                                      ecb-ping-program
                                      nil nil nil
                                      options))))
         (ecb-host-accessible-cache-add host (or result 'NOT-ACCESSIBLE))
         result))
      (otherwise value))))


;; (ecb-host-accessible-p "ecb.sourceforge.net")

(silentcomp-defun ange-ftp-ftp-name)
(silentcomp-defun efs-ftp-path)
(silentcomp-defun tramp-tramp-file-p)
(silentcomp-defun tramp-file-name-path)
(silentcomp-defun tramp-file-name-localname)
(silentcomp-defun tramp-file-name-host)
(silentcomp-defun tramp-dissect-file-name)
(defun ecb-remote-path (path)
  "Test if PATH is a remote path and dissect it into components if yes.
Returns a list (FULL-HOST-USER-PART HOST REAL-PATH), or nil if PATH is not a
remote path. FULL-HOST-USER-PART is that component from beginning of PATH to
the :-separator which separates user- and host-parts from the real path, i.e.
it always ends with a colon! HOST is the remote HOST and REAL-PATH is that
component after that :-separator. Supports tramp, ange-ftp and efs."
  (let ((value (ecb-remote-path-cache-get path)))
    (case value
      (NOT-REMOTE nil)
      ((nil)
       (let* ((dissection (or (and (featurep 'tramp) ;; tramp-support
                                   (tramp-tramp-file-p path)
                                   (tramp-dissect-file-name path))
                              (and (featurep 'ange-ftp) ;; ange-ftp-support
                                   (ange-ftp-ftp-name path))
                              (and (featurep 'efs) ;; efs support
                                   (efs-ftp-path path))))
              (host/real-path
               (if dissection
                   (or (and (featurep 'tramp) ;; tramp-support
                            (cons (tramp-file-name-host dissection)
                                  (if (fboundp 'tramp-file-name-localname)
                                      (tramp-file-name-localname dissection)
                                    (tramp-file-name-path dissection))))
                       (and (featurep 'ange-ftp) ;; ange-ftp-support
                            (cons (nth 0 dissection)
                                  (nth 2 dissection)))
                       (and (featurep 'efs) ;; efs support
                            (cons (nth 0 dissection)
                                  (nth 2 dissection))))
                 (cons nil path)))
              (full-host-user-part
               (substring path 0 (- (length path)
                                    (length (cdr host/real-path)))))
              (result nil))
         (setq result
               (and dissection
                    (list full-host-user-part
                          (car host/real-path)
                          (cdr host/real-path))))
         (ecb-remote-path-cache-add path (or result 'NOT-REMOTE))
         result))
      (otherwise value))))

;; (ecb-remote-path "/berndl@ecb.sourceforge.net:~")
;; (ecb-remote-path "~")


;; empty dirs

(defun ecb-check-emptyness-of-dir (dir)
  "Checks if DIR is an empty directory. If empty return not nil otherwise nil."
  (let ((cache-value (ecb-directory-empty-cache-get dir))
        (show-sources (ecb-show-sources-in-directories-buffer-p)))
    (if (and cache-value
             (equal (cdr cache-value) show-sources))
        (car cache-value)
      (ecb-directory-empty-cache-remove dir)
      (let ((entries (and (ecb-file-accessible-directory-p dir)
                          (ecb-directory-files dir nil nil t)))
            (just-files-means-empty (not show-sources))
            (full-file-name nil)
            (empty-p nil))
        (setq empty-p
              (catch 'found
                (dolist (e entries)
                  (when (not (member e '("." ".." "CVS")))
                    (setq full-file-name (ecb-fix-filename dir e))
                    (if (ecb-file-directory-p full-file-name)
                        (throw 'found 'nil)
                      (if (not just-files-means-empty)
                          (throw 'found 'nil)))))
                t))
        ;; now we add this value to the cache
        (ecb-directory-empty-cache-add (ecb-fix-filename dir)
                                       (cons empty-p show-sources))
        empty-p))))


(defecb-stealthy ecb-stealthy-empty-dir-check
  "Check for each current visible nodes in the directories buffer if the
underlying directory is empty or not and update the node if the current node
state and display is different from the empty-state of the associated
directory. This function is only for use by `ecb-stealthy-updates'!"
  (when (equal 'window-not-visible
               (ecb-exec-in-window ecb-directories-buffer-name
                 (if (equal state 'restart)
                     (setq state 1))
                 ;; Here the state is an integer because a stealthy functions runs only
                 ;; when state != 'done
                 (let ((lines-of-buffer (count-lines (point-min) (point-max)))
                       (curr-node nil)
                       (dir-empty-p nil))
                   (ecb-exit-on-input 'empty-dir-check-stealthy
                     (save-excursion
                       (while (<= state lines-of-buffer)
                         (ecb-throw-on-input 'lines-of-buffer-loop)
                         (goto-line state)
                         (setq curr-node (tree-buffer-get-node-at-point))
                         (when (and (ecb-directory-should-prescanned-p
                                     (tree-node->data curr-node))
                                    (ecb-file-exists-p
                                     (tree-node->data curr-node)))
                           (setq dir-empty-p
                                 (ecb-check-emptyness-of-dir (tree-node->data curr-node)))
                           ;; we update the node only if we have an empty dir and the node is
                           ;; still expandable
                           (when (or (and dir-empty-p
                                          (tree-node->expandable curr-node))
                                     (and (not dir-empty-p)
                                          (not (tree-node->expandable curr-node))))
                             (tree-buffer-update-node nil
                                                      'use-old-value
                                                      'use-old-value
                                                      'use-old-value
                                                      'use-old-value
                                                      (not dir-empty-p)
                                                      t)))
                         (setq state (1+ state)))))
                   (if (> state lines-of-buffer)
                       (setq state 'done)))))
    (setq state 'done)))

;; read-only-files?

(defun ecb-stealthy-read-only-check--internal (state)
  "Check for all sourcefile-nodes either in the directories- or the
sources-buffer if the associated file is writable or not. This function does
the real job and is is only for use by a stealthy function defined with
`defecb-stealthy'! STATE is the initial state-value the stealthy-function has
when called. Return the new state-value."
  (if (or (not ecb-sources-perform-read-only-check)
          (not (or (string= (buffer-name (current-buffer))
                            ecb-sources-buffer-name)
                   (and (string= (buffer-name (current-buffer))
                                 ecb-directories-buffer-name)
                        (ecb-show-sources-in-directories-buffer-p)))))
      'done
    ;; Now we are either in the sources-buffer or in the directories-buffer
    ;; when sources are displayed in the directories-buffer
    (if (equal state 'restart)
        (setq state 1))
    ;; Here the state is an integer because a stealthy functions runs only
    ;; when state != 'done
    (let ((lines-of-buffer (count-lines (point-min) (point-max)))
          (curr-node nil)
          (new-name nil)
          (read-only-p nil)
          (node-type-to-check (if (string= (buffer-name (current-buffer))
                                           ecb-sources-buffer-name)
                                  ecb-sources-nodetype-sourcefile
                                ecb-directories-nodetype-sourcefile)))
      (ecb-exit-on-input 'read-only-stealthy
        (save-excursion
          (while (<= state lines-of-buffer)
            (ecb-throw-on-input 'lines-of-buffer-loop)
            (goto-line state)
            (setq curr-node (tree-buffer-get-node-at-point))
            (when (and (= (tree-node->type curr-node) node-type-to-check)
                       (ecb-sources-read-only-check-p
                        (ecb-file-name-directory (tree-node->data curr-node))))
              (setq new-name (tree-node->name curr-node))
              (setq read-only-p
                    (not (ecb-file-writable-p (tree-node->data curr-node))))
              (if read-only-p
                  (ecb-merge-face-into-text new-name
                                            ecb-source-read-only-face))
              ;; we update the node only if we have an empty dir and the node is
              ;; still expandable
              (when read-only-p
                (tree-buffer-update-node
                 nil
                 new-name
                 'use-old-value
                 'use-old-value
                 'use-old-value
                 'use-old-value
                 t)))
            (setq state (1+ state)))))
      (if (> state lines-of-buffer)
          (setq state 'done)))
    state))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: The best would be using the
;; new macro `while-no-input' with Emacs 22 but currently it seems not to work
;; properly:
;;
;; (defun klaus-test-input ()
;;   (interactive)
;;   (let ((result nil)
;;         (i 0))
;;     (setq result (while-no-input
;;                   (while t
;;                     (setq i (1+ i)))))
;;     (message "Klausi: %s" result)))
;;
;; What is wrong??
;; Seems that it works on *nix but not an windows - have already filed a
;; bug-report to the Emacs-team...
; when it works also on windows then we should build in this macro in the
; stealthy stuff - the best would be to use it within `ecb-exit-on-input'.

(defecb-stealthy ecb-stealthy-ro-check-in-directories-buf
  "Check for all sourcefile-nodes in the directories-buffer if the associated
file is writable or not."
  (if (ecb-show-sources-in-directories-buffer-p)
      (when (equal 'window-not-visible
                   (ecb-exec-in-window ecb-directories-buffer-name
                     (setq state
                           (ecb-stealthy-read-only-check--internal state))))
        (setq state 'done))
    (setq state 'done)))


;; Test, if our new interrupt mechanism works...
;; (defecb-stealthy ecb-stealthy-klaus-test
;;   "test"
;;   (setq state
;;         (if (equal (ecb-exit-on-input 'testing
;;                      (let ((inhibit-quit nil)
;;                            (message-log-max nil))
;;                        (while t
;;                          (message "Looping ...")
;;                          (ecb-throw-on-input 'user-input)
;;                          )))
;;                    'user-input)
;;             'done
;;           'Klaus)))


(defecb-stealthy ecb-stealthy-ro-check-in-sources-buf
  "Check for all sourcefile-nodes in the sources-buffer if the associated file
is writable or not."
  (when (equal 'window-not-visible
               (ecb-exec-in-window ecb-sources-buffer-name
                 (setq state
                       (ecb-stealthy-read-only-check--internal state))))
    (setq state 'done)))

;; version control support

;; We use a cache which stores for
;; + a directory: either the function used to check the VC-state of its files
;;   (if the directory is managed by a VC-backend) or the symbol 'NO-VC (if
;;   the dir is not managed by a VC-backend).
;; + a file: the most recent VC-state plus the check-timestamp for this state.
;;
;; So we have to call only once the identify-backend-function (see above) for
;; a directory. The check-state-function must only be called if the file has
;; been modified since the stored check-state-timestamp. With this caching
;; even using real-VC-checks (as vc-recompute-state) which never uses
;; heuristics is possible without loosing to much informations (only if a file
;; is modified by another user can not be detected with this cache - but for
;; this we have the power-click which always throws away any cache-state)

(defconst ecb-vc-state-icon-alist '((up-to-date . ("vc-up-to-date" "(u)"))
                                    (edited . ("vc-edited" "(e)"))
                                    (added . ("vc-added" "(a)"))
                                    (needs-patch . ("vc-needs-patch" "(p)"))
                                    (needs-merge . ("vc-needs-merge" "(m)"))
                                    (ignored . ("vc-ignored" "(x)"))
                                    (unknown . ("vc-unknown" "(?)"))
                                    (nil . ("vc-unknown" "(?)")))
  "Associate an image-name and a textual icon to the allowed VC-states - see
`ecb-vc-supported-backends'. Each element is a cons-cell where the car is the
symbol of a supported VC-state and the cdr a 2-element list where the first
element is the name of the needed image-icon and the second element the
ascii-string which should be dislayed if Emacs doesn't support image-display.")

(defsubst ecb-vc-get-image-name-for-vc-state (state)
  "Return the associated image-name for the vc-state STATE."
  (or (nth 0 (cdr (assq state ecb-vc-state-icon-alist)))
      "vc-unknown"))
  
(defsubst ecb-vc-get-ascii-icon-for-vc-state (state)
  "Return the associated texual icon for the vc-state STATE."
  (or (nth 1 (cdr (assq state ecb-vc-state-icon-alist)))
      "(?)"))


(defconst ecb-vc-incr-searchpattern-node-prefix
  '("\\(\\(([uempx?])\\)? \\)?" . 2)
  "Prefix-pattern which ignores all not interesting vc-icon-stuff of a
node-name at incr. search. This ignores the \"(<vc-state-char>)\" whereas
<vc-state-char> is one of u, e, m, p or ?.
Format: cons with car is the pattern and cdr is the number of subexpr in this
pattern.")


(defun ecb-vc-check-state (file tree-buffer-name vc-state-fcn)
  "Check if the VC-state for FILE must be rechecked, i.e. if it is out of
date. If it is still valid and also already checked for TREE-BUFFER-NAME then
return the symbol 'unchanged \(if still valid but only not checked for
TREE-BUFFER-NAME then return the state and store the fact that it has been
check now also for this buffer). Otherwise check the new state for FILE with
VC-STATE-FCN, store it in the cache only for TREE-BUFFER-NAME and return the
new state."
  (let* ((cached-state (ecb-vc-cache-get file))
         (last-state (nth 0 cached-state))
         (last-check-time (nth 1 cached-state))
         (checked-tree-buffer-names (nth 2 cached-state))
         (no-need-for-state-check-p
          (and last-check-time
               (or (null last-state) ;; FILE has been checked but is not in VC
                   (not (ecb-time-less-p last-check-time
                                         (ecb-subseq (nth 5 (ecb-file-attributes file))
                                                     0 2))))))
         (result nil))
    (if no-need-for-state-check-p
        ;; FILE was not modified since our last vc-state-check
        (if (member tree-buffer-name checked-tree-buffer-names)
            ;; TREE-BUFFER-NAMES is in the list of buffer-names for which the
            ;; state of FILE has already been cached ==> there is no need to
            ;; update the cache we can just return 'unchanged to signalize
            ;; that nothing has to be updated
            (setq result 'unchanged)
          ;; now we add TREE-BUFFER-NAME to that list - this new list will be
          ;; added to the cache below. As result we will return the last-state
          ;; because the state itself is still valid - the only thing we now
          ;; have to store in the cache is that the last-state is now valid
          ;; for TREE-BUFFER-NAME too!
          (setq result last-state)
          (setq checked-tree-buffer-names
                (cons tree-buffer-name checked-tree-buffer-names)))

      ;; FILE was modified since our last vc-state-check, so we have to check
      ;; the state again
      
      ;; set the list of the buffer-names for which the check will be performed
      ;; and then cached to TREE-BUFFER-NAME ==> Only for this buffer-name the
      ;; cache is valid.
      (setq checked-tree-buffer-names (list tree-buffer-name))
      ;; get the new vc-state
      (setq result (and vc-state-fcn
                        (fboundp vc-state-fcn)
                        ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>:
                        ;; vc-cvs-state seems to change the window-config
                        ;; (opens a new small window) if it fails...so maybe
                        ;; we have to save the window-config before this call
                        ;; and restore it when the call fails - later... ;-)

                        ;; we must ignore errors here because it could be that
                        ;; a user has a certain VC-system not installed onto
                        ;; his machine but opens directories which have a
                        ;; CVS-subdir for example - then for such a directory
                        ;; ECB would eventually call this backend - but this
                        ;; would fail because the needed program is not
                        ;; installed - so we ignore this an handle this as
                        ;; unknown-state. 
                        (ignore-errors (funcall vc-state-fcn file))))
      ;; now we map the backend-state to one of the ECB-VC-state-values
      (setq result (or (cdr (assoc result ecb-vc-state-mapping)) 'unknown)))
    (if (not (equal result 'unchanged))
        ;; add the new state to the cache because either the list of checked
        ;; buffers and/or the state has been modified.
        (ecb-vc-cache-add-file file
                               result
                               checked-tree-buffer-names))
    ;; return the result - either 'unchanged or the new VC-state
    result))

(defun ecb-vc-update-sources-cache (dir)
  "Update the SOURCES cache for DIR with the current-content of the
sources-buffer if DIR has currently either a filtered or full cache entry in
the SOURCES-cache."
  (let* ((full-sources-cache (ecb-sources-cache-get-full dir))
         (filtered-sources-cache (and full-sources-cache
                                      (ecb-sources-cache-get-filtered dir))))
    (if filtered-sources-cache
        ;; we have currently a filtered sources-buffer so we must update the
        ;; filtered sources-cache.
        (ecb-sources-cache-add-filtered dir
                                        (list (tree-buffer-get-root)
                                              (tree-buffer-displayed-nodes-copy)
                                              (ecb-buffer-substring (point-min)
                                                                    (point-max))
                                              ;; add the old-filter-spec
                                              ;; because it must be the same
                                              (nth 3 filtered-sources-cache)))
      (if full-sources-cache
          ;; we have currently a cached sources-buffer without an applied
          ;; filter so we must update the full sources-cache.
          (ecb-sources-cache-add-full dir
                                      (list (tree-buffer-get-root)
                                            (tree-buffer-displayed-nodes-copy)
                                            (ecb-buffer-substring (point-min)
                                                                  (point-max))))))))

(defun ecb-vc-cvs-root-remote-p (root)
  "Return not nil if ROOT is a remote CVS-repository."
  (if (string-match "^:local:" root)
      nil
    (and (string-match "^\\(:ext:\\|:server:\\)?\\([^@]+@\\)?\\([^:]+\\):"
                       root)
         (match-string 3 root))))

;; some tests:
;; The following must all return cvs.sourceforge.net!
;; (ecb-vc-cvs-root-remote-p ":ext:berndl@cvs.sourceforge.net:/usr/local/root")
;; (ecb-vc-cvs-root-remote-p "berndl@cvs.sourceforge.net:/usr/local/root")
;; (ecb-vc-cvs-root-remote-p ":ext:cvs.sourceforge.net:/usr/local/root")
;; (ecb-vc-cvs-root-remote-p "cvs.sourceforge.net:/usr/local/root")
;; (ecb-vc-cvs-root-remote-p ":ext:berndl@cvs.sourceforge.net:C:/local/root")
;; (ecb-vc-cvs-root-remote-p "berndl@cvs.sourceforge.net:C:/local/root")
;; (ecb-vc-cvs-root-remote-p ":ext:cvs.sourceforge.net:C:/local/root")
;; (ecb-vc-cvs-root-remote-p "cvs.sourceforge.net:C:/local/root")
;; The following has to return nil!
;; (ecb-vc-cvs-root-remote-p "/local/root")
;; (ecb-vc-cvs-root-remote-p ":local:C:/local/root")
;; The following is allowed to return "C" because CVS forbids to use
;; windows-path as root without keyword :local:!
;; (ecb-vc-cvs-root-remote-p "C:/local/root")

(defun ecb-vc-dir-managed-by-CVS (directory)
  "Return 'CVS if DIRECTORY is managed by CVS. nil if not.

This function tries to be as smart as possible: First it checks if DIRECTORY
is managed by CVS by checking if there is a subdir CVS. If no then nil is
returned. If yes then for GNU Emacs it takes into account the value of
`vc-cvs-stay-local': If t then just return 'CVS. Otherwise ECB checks the root
repository if it is a remote repository. If not just 'CVS is returned. If a
remote repository it checks if the value of `vc-cvs-stay-local' is a string
and matches the host of that repository. If yes then just 'CVS is returned. If
not then ECB checks if that host is currently accessible by performing a ping.
If accessible 'CVS is returned otherwise nil. This has the advantage that ECB
will not be blocked by trying to get the state from a remote repository while
the host is not accessible \(e.g. because the user works offline).

Special remark for XEmacs: XEmacs has a quite outdated VC-package which has no
option `vc-cvs-stay-local' so the user can not work with remote
CVS-repositories if working offline for example. So if there is no option
`vc-cvs-stay-local' then ECB performs always the repository check mentioned
above and it depends on the value of `ecb-vc-xemacs-exclude-remote-repository'
if ECB treats such a directory as managed by CVS or not!"
  (and (ecb-file-exists-p (concat directory "/CVS/"))
       (or (ignore-errors (progn
                            (require 'vc)
                            (require 'vc-cvs)))
           t)
       (if (or (not (boundp 'vc-cvs-stay-local)) ;; XEmacs doesn't have this
               (not (eq vc-cvs-stay-local t)))
           ;; XEmacs has a quite outdated VC-package which has no option
           ;; `vc-cvs-stay-local' so the user can not work with remote
           ;; directories if working offline for example. so we use a
           ;; workaround by checking the root of the CVS-repsoitory (we can
           ;; get it from the file /CVS/Root) if it is a remote root and if
           ;; yes we ping the host of that root. If accessible ...
           (let* ((Root-content (ecb-file-content-as-string (concat directory
                                                                    "/CVS/Root")))
                  (host (and Root-content
                             (ecb-vc-cvs-root-remote-p Root-content))))
             (if (and host
                      ecb-vc-xemacs-exclude-remote-cvs-repository
                      (not (boundp 'vc-cvs-stay-local)))
                 nil
               (when (or (null host) ;; local repository
                         ;; vc-cvs-stay-local says VC should stay local for this
                         ;; host
                         (and (boundp 'vc-cvs-stay-local)
                              (stringp vc-cvs-stay-local)
                              (string-match vc-cvs-stay-local host))
                         ;; the host is at least accessible
                         (ecb-host-accessible-p host))
                 'CVS)))
         ;; VC always will stay local so we are satisfied ;-)
         'CVS)))


(defun ecb-vc-dir-managed-by-RCS (directory)
  "Return 'RCS if DIRECTORY is managed by RCS. nil if not."
  (and (ecb-file-exists-p (concat directory "/RCS/"))
       'RCS))

(defun ecb-vc-dir-managed-by-SVN (directory)
  "Return 'SVN if DIRECTORY is managed by SVN. nil if not."
  (and (ecb-file-exists-p (concat directory "/.svn/"))
       (locate-library "vc-svn")
       'SVN))

(defun ecb-vc-dir-managed-by-SCCS (directory)
  "Return 'SCCS if DIRECTORY is managed by SCCS. nil if not."
  (or (and (ecb-file-exists-p (concat directory "/SCCS/")) 'SCCS)
      ;; Remote SCCS project
      (let ((proj-dir (getenv "PROJECTDIR")))
        (if proj-dir
            (and (ecb-file-exists-p (concat proj-dir "/SCCS")) 'SCCS)
          nil))))

;; clearcase support

;; Problem: is this OK to assume, that the clearcase-specific
;; stuff is already loaded? may be we need more checks here.
(silentcomp-defun clearcase-file-is-in-view-p)
(defun ecb-vc-dir-managed-by-CC (directory)
  "Return 'CC if DIRECTORY is managed by ClearCase. nil if not."
  (and (fboundp 'clearcase-file-is-in-view-p)
       (if (clearcase-file-is-in-view-p directory)
           'CC)))

(silentcomp-defun clearcase-compute-next-action)
(defun ecb-vc-check-CC-state (file)
  "Checks the VC-state of FILE when under Control of Clearcase.
Returns the following different state-values: 'unknown, 'up-to-date, 'edited
and 'unlocked-changes."
  (let ((action (and (fboundp 'clearcase-compute-next-action)
                     (clearcase-compute-next-action file))))
    (cond
     ((eq action 'mkelem)
      'unknown)
     ((eq action 'checkout)
      'up-to-date)
     ((eq action 'uncheckout)
      'edited)
     ((eq action 'illegal-checkin)
      ;; ??? Is OK ???
      'unlocked-changes)
     ((eq action 'checkin)
      'edited))))

(defadvice clearcase-sync-from-disk (after ecb)
  "Ensures that the ECB-cache is reset and the entry for the currently
checked-in/out or added file is cleared. Does nothing if the function
`ecb-vc-dir-managed-by-CC' is not contained in `ecb-vc-supported-backends'!"
  (when (assoc 'ecb-vc-dir-managed-by-CC ecb-vc-supported-backends)
    (ecb-vc-cache-remove (ecb-fix-filename (ad-get-arg 0)))
    (ecb-vc-reset-vc-stealthy-checks)))

(defun ecb-vc-state (file)
  "Same as `vc-state' but it clears the internal caches of the VC-package for
FILE before calling `vc-state'. Finally calls `vc-state' and returns that value."
  (and (fboundp 'vc-file-clearprops) (vc-file-clearprops file))
  (vc-state file))

(defun ecb-vc-get-state-fcn-for-dir (directory)
  "Get that function which should be used for checking the VC-state for files
contained in DIRECTORY. Get it either from the VC-cache or call the car of
each element of `ecb-vc-supported-backends' and return the cdr of the first
elem where the car returns not nil. If Directory is not managed by a
version-control system then return nil. Store the result in the VC-cache for
speeding up things next time. Ange-ftp- or efs-directories will never be
checked for VC-states!"
  (let* ((norm-dir (ecb-fix-filename directory))
         (cache-val (ecb-vc-cache-get norm-dir)))
    (case cache-val
      (NO-VC nil)
      ((nil) ;; not cached or outdated
       (let ((vc-backend-fcn
              (catch 'found
                (dolist (elem ecb-vc-supported-backends)
                  (when (and (fboundp (car elem))
                             (funcall (car elem) norm-dir))
                    (throw 'found (cdr elem))))
                nil)))
         ;; Add it to the vc-cache: Either NO-VC if nil otherwise the
         ;; check-state-function
         (ecb-vc-cache-add-dir norm-dir (or vc-backend-fcn 'NO-VC))
         vc-backend-fcn))
      (otherwise cache-val))))

(defalias 'ecb-vc-managed-dir-p 'ecb-vc-get-state-fcn-for-dir)

(defun ecb-vc-generate-node-name (name state)
  "Generate a node-name with an appropriate icon in the front of NAME
depending on STATE. If Emacs supports image-display then an image-icon wll be
used otherwise an ascii-icon. The text-property 'ecb-vc-ascii-icon-length is
added to the full length of the returned node-name. It contains as value the
length of the ascii-icon \(incl. one trailing space) which is added in front
of NAME."
  (let* ((ascii-icon (ecb-vc-get-ascii-icon-for-vc-state state))
         (node-name (concat ascii-icon " "
                            (save-match-data
                              (if (string-match "^(.) \\(.+\\)$" name)
                                  (match-string 1 name)
                                name)))))
    (put-text-property 0 (length node-name)
                       'ecb-vc-ascii-icon-length (1+ (length ascii-icon))
                       node-name)
    (ecb-generate-node-name node-name (1+ (length ascii-icon))
                            (ecb-vc-get-image-name-for-vc-state state)
                            ;; even in the history- or the directories-buffers
                            ;; we use the icons of the sources-buffer because
                            ;; they are the same!
                            ecb-sources-buffer-name)))

;; (insert (ecb-vc-generate-node-name "test-name" 'needs-merge))

(defun ecb-stealthy-vc-check--dir/history (state)
  "Check for all sourcefile-nodes either in the directories- or the
history-buffer the VC-state. This function does the real job and is is only
for use by a stealthy function defined with `defecb-stealthy'! STATE is the
initial state-value the stealthy-function has when called. Return the new
state-value."
  (if (not ecb-vc-enable-support)
      'done
    (if (not (or (string= (buffer-name (current-buffer))
                          ecb-history-buffer-name)
                 (and (string= (buffer-name (current-buffer))
                               ecb-directories-buffer-name)
                      (ecb-show-sources-in-directories-buffer-p))))
        'done
      ;; Now we are either in the history-buffer or in the directories-buffer
      ;; when sources are displayed in the directories-buffer
      (if (equal state 'restart)
          (setq state 1))
      ;; Here the state is an integer because a stealthy functions runs only
      ;; when state != 'done
      (let ((lines-of-buffer (count-lines (point-min) (point-max)))
            (curr-node nil)
            (curr-dir nil)
            (new-name nil)
            (vc-state-fcn nil)
            (new-state nil)
            (node-type-to-check (if (string= (buffer-name (current-buffer))
                                             ecb-history-buffer-name)
                                    ecb-history-nodetype-sourcefile
                                  ecb-directories-nodetype-sourcefile)))
        (ecb-exit-on-input 'vc-check-dir-hist-stealthy
          (save-excursion
            (while (<= state lines-of-buffer)
              (ecb-throw-on-input 'lines-of-buffer-loop)
              (goto-line state)
              (setq curr-node (tree-buffer-get-node-at-point))
              (setq curr-dir (ecb-file-name-directory (tree-node->data curr-node)))
              (when (and (= (tree-node->type curr-node) node-type-to-check)
                         (ecb-vc-directory-should-be-checked-p curr-dir)
                         ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Move this
                         ;; existing-check to outside this when-clause and then
                         ;; do the same as in `ecb-stealthy-vc-check--sources'!
                         (ecb-file-exists-p (tree-node->data curr-node)))
                (setq vc-state-fcn (ecb-vc-get-state-fcn-for-dir curr-dir))
                (when vc-state-fcn ;; file is under VC-control
                  (setq new-name (tree-node->name curr-node))
                  (setq new-state
                        (ecb-vc-check-state (tree-node->data curr-node)
                                            (buffer-name (current-buffer))
                                            vc-state-fcn))
                  ;; we update the node only if the state has changed 
                  (when (not (equal 'unchanged new-state))
                    (setq new-name (ecb-vc-generate-node-name new-name new-state))
                    (tree-buffer-update-node
                     nil new-name
                     'use-old-value 'use-old-value 'use-old-value 'use-old-value t))))
              (setq state (1+ state)))))
        (if (> state lines-of-buffer)
            (setq state 'done)))
      state)))

(defun ecb-stealthy-vc-check--sources (state)
  "Check for all sourcefile-nodes in sources-buffer the VC-state. This
function does the real job and is is only for use by a stealthy function
defined with `defecb-stealthy'! STATE is the initial state-value the
stealthy-function has when called. Return the new state-value."
  (if (not (ecb-vc-directory-should-be-checked-p ecb-path-selected-directory))
      'done
    (let ((vc-state-fcn (ecb-vc-get-state-fcn-for-dir ecb-path-selected-directory)))
      (if (null vc-state-fcn)
          ;; the sources-files are not under VC-control
          'done
        ;; Now we are either in the sources-, or history-buffer or in the
        ;; directories-buffer when sources are displayed in the
        ;; directories-buffer
        (if (equal state 'restart)
            (setq state 1))
        ;; Here the state is an integer because a stealthy functions runs only
        ;; when state != 'done
        (let ((lines-of-buffer (count-lines (point-min) (point-max)))
              (curr-node nil)
              (new-name nil)
              (new-state nil)
              (update-performed-for-dir nil))
          (ecb-exit-on-input 'vc-check-sources-stealthy
            (save-excursion
              (while (<= state lines-of-buffer)
                (ecb-throw-on-input 'lines-of-buffer-loop)
                (goto-line state)
                (setq curr-node (tree-buffer-get-node-at-point))
                (if (ecb-file-exists-p (tree-node->data curr-node))
                    (progn
                      (setq new-name (tree-node->name curr-node))
                      (setq new-state
                            (ecb-vc-check-state (tree-node->data curr-node)
                                                (buffer-name (current-buffer))
                                                vc-state-fcn))
                      ;; we update the node only if the state has changed 
                      (when (not (equal 'unchanged new-state))
                        (setq new-name (ecb-vc-generate-node-name new-name new-state))
                        (or update-performed-for-dir
                            (setq update-performed-for-dir ecb-path-selected-directory))
                        (tree-buffer-update-node
                         curr-node new-name
                         'use-old-value 'use-old-value 'use-old-value
                         'use-old-value t))
                      (setq state (1+ state)))
                
                  ;; file does no longer exist
                
                  ;; reduce the number of lines/files
                  (setq lines-of-buffer (1- lines-of-buffer))
                  ;; remove the node from the tree and the tree-display
                  (tree-buffer-remove-node curr-node t)
                  ;; remove the current dir from the files-and-sub-dir-cache
                  (ecb-files-and-subdirs-cache-remove ecb-path-selected-directory)
                  ;; if there is currently only one line in the tree-buffer the
                  ;; directory will be probably empty now (we knowingly ignore
                  ;; here the possibility that some files can exists in the dir
                  ;; but they are not displayed in the tree-buffer cause of some
                  ;; filters for example), so it's a heuristic approach
                  (if (= lines-of-buffer 1)
                      (ecb-directory-empty-cache-remove ecb-path-selected-directory))
                  ;; we must trigger that the sources-cache will be updated below
                  (setq update-performed-for-dir ecb-path-selected-directory)
                  ))))
          ;; if we have performed at least one update then we must update the
          ;; SOURCES-cache.
          (when update-performed-for-dir
            (ecb-vc-update-sources-cache update-performed-for-dir))
          (if (> state lines-of-buffer)
              (setq state 'done)))
        state))))

(defecb-stealthy ecb-stealthy-vc-check-in-history-buf
  "Check for all entries in the history-buffer their VC-state and
display an appropriate icon in front of the item."
  (when (equal 'window-not-visible
               (ecb-exec-in-window ecb-history-buffer-name
                 (setq state
                       (ecb-stealthy-vc-check--dir/history state))))
    (setq state 'done)))


(defecb-stealthy ecb-stealthy-vc-check-in-sources-buf
  "Check for all sourcefile-nodes in the sources-buffer their VC-state and
display an appropriate icon in front of the file."
  (when (equal 'window-not-visible
               (ecb-exec-in-window ecb-sources-buffer-name
                 (setq state
                       (ecb-stealthy-vc-check--sources state))))
    (setq state 'done)))

(defecb-stealthy ecb-stealthy-vc-check-in-directories-buf
  "Check for all sourcefile-nodes in the directories-buffer their VC-state and
display an appropriate icon in front of the file."
  (if (ecb-show-sources-in-directories-buffer-p)
      (when (equal 'window-not-visible
                   (ecb-exec-in-window ecb-directories-buffer-name
                     (setq state
                           (ecb-stealthy-vc-check--dir/history state))))
        (setq state 'done))
    (setq state 'done)))

(defun ecb-vc-reset-vc-stealthy-checks ()
  "Resets all stealthy VC-checks."
  ;; we can call savely all these initialization because if one of the
  ;; following tree-windows is not visible nothing will be done (and the
  ;; directories-check will only run when sources are displayed in the
  ;; directories buffer!). If visible the vc-check will be performed for all
  ;; current visible file-nodes again in all visible tree-buffers of the
  ;; file-browser but because we have only removed the cache-entry for
  ;; exactly one file, the check will be very fast for all file-nodes
  ;; besides this file!

  ;; I think the read-only check must be performed too - because for
  ;; backends like Clearcase a changed VC-state can also result in a changed
  ;; read-only-state!
  (ecb-stealthy-function-state-init 'ecb-stealthy-ro-check-in-directories-buf)
  (ecb-stealthy-function-state-init 'ecb-stealthy-ro-check-in-sources-buf)
  (ecb-stealthy-function-state-init 'ecb-stealthy-vc-check-in-sources-buf)
  (ecb-stealthy-function-state-init 'ecb-stealthy-vc-check-in-directories-buf)
  (ecb-stealthy-function-state-init 'ecb-stealthy-vc-check-in-history-buf)
  ;; This function is also used in write-file-hooks so we have to return nil
  ;; because otherwise a file will never be written - see documentation of
  ;; `write-file-hooks'!
  nil)
  
;; we have to add a smart piece of code to `vc-checkin-hook' which is able to
;; clear the cache entry for exactly that file checked-in with vc-checkin!
;; Problems to solve:
;; - unfortunatelly this hook is not called with the checked-in filename as
;;   argument but it is a normal hook runned with `run-hooks' :-( But we can
;;   not reset the stuff in the advice itself because this doesn't ensure that
;;   the after-advice-stuff is called *after* the checkin - seems that the
;;   after-advice runs already during the user inserts/edits the
;;   checkin-comment. But the vc-checkin-hook is really called after the
;;   checkin! ==> We use a combination of an after-advice and vc-checkin-hook!
;; - If a user uses PCL-CVS for CVS-operations this advice of vc-checkin will
;;   not run because pcl-cvs doesn't delegate the checkin-task to
;;   `vc-checkin'. Therefore also the `vc-checkin-hook' is not runned via
;;   pcl-cvs.
;; - What about other backends not supported by VC, e.g. clearcase.el? Well,
;;   with a good documentation what a user has to do.... ;-)


(defvar ecb-vc-advices '((vc-checkin . after)
                         (clearcase-sync-from-disk . after))
  "All advices needed for the builtin VC-support of ECB. Same format as
`ecb-basic-adviced-functions'.")

(defvar ecb-checkedin-file nil
  "Stored the filename of the most recent checked-in file. Is only set by the
after-advice of `vc-checkin' and `ecb-vc-checkin-hook' \(resets it to nil).
Evaluated only by `ecb-vc-checkin-hook'.

This is the communication-channel between `vc-checkin' and
`ecb-vc-checkin-hook' so this hook-function gets the filename of the
checked-in file.")

(defadvice vc-checkin (after ecb)
  "Simply stores the filename of the checked-in file in `ecb-checkedin-file'
so it is available in the `vc-checkin-hook'."
  (setq ecb-checkedin-file (ecb-fix-filename (ad-get-arg 0))))

(defun ecb-vc-checkin-hook ()
  "Ensures that the ECB-cache is reset and the entry for the most recent
checkedin file is cleared. Uses `ecb-checkedin-file' as last checked-in file."
  (when ecb-checkedin-file
    (ecb-vc-cache-remove ecb-checkedin-file)
    (ecb-vc-reset-vc-stealthy-checks)
    (setq ecb-checkedin-file nil)))

(defun ecb-vc-after-revert-hook ()
  "Ensures that the ECB-cache is reset and the entry for the currently
reverted file-buffer is cleared."
  (let ((file (ignore-errors (ecb-fix-filename buffer-file-name))))
    (when (and file (ecb-file-exists-p file))
      (ecb-vc-cache-remove file)
      (ecb-vc-reset-vc-stealthy-checks))))

(defun ecb-vc-enable-internals (arg)
  "Enable or disable \(if ARG < 0) all settings needed by the VC-support."
  (if (< arg 0)
      (progn
        (remove-hook 'after-revert-hook 'ecb-vc-after-revert-hook)
        (remove-hook 'write-file-hooks 'ecb-vc-reset-vc-stealthy-checks)
        (remove-hook 'vc-checkin-hook 'ecb-vc-checkin-hook)
        (ecb-disable-advices ecb-vc-advices))
    (add-hook 'after-revert-hook 'ecb-vc-after-revert-hook)
    (add-hook 'write-file-hooks 'ecb-vc-reset-vc-stealthy-checks)
    (add-hook 'vc-checkin-hook 'ecb-vc-checkin-hook)
    (ecb-enable-advices ecb-vc-advices)))

;; -- end of vc-support ---------------

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Maybe we should run directly
;; `ecb-stealthy-updates' from within this after-update-hooks?!

(defun ecb-stealth-tasks-after-directories-update ()
  "After update hook for the directories-buffer. Runs directly after
performing a `tree-buffer-update' for this buffer."
  (ecb-stealthy-function-state-init 'ecb-stealthy-empty-dir-check)
  (ecb-stealthy-function-state-init 'ecb-stealthy-ro-check-in-directories-buf)
  (ecb-stealthy-function-state-init 'ecb-stealthy-vc-check-in-directories-buf)
  )

(defun ecb-stealth-tasks-after-sources-update ()
  "After update hook for the sources-buffer. Runs directly after
performing a `tree-buffer-update' for this buffer."
  (ecb-stealthy-function-state-init 'ecb-stealthy-ro-check-in-sources-buf)
  (ecb-stealthy-function-state-init 'ecb-stealthy-vc-check-in-sources-buf)
  )

(defun ecb-stealth-tasks-after-history-update ()
  "After update hook for the history-buffer. Runs directly after
performing a `tree-buffer-update' for this buffer."
  (ecb-stealthy-function-state-init 'ecb-stealthy-vc-check-in-history-buf)
  )

;; -- adding files ---------------------

(defun ecb-tree-node-add-files
  (node path files type include-extension old-children &optional not-expandable)
  "For every file in FILES add a child-node to NODE."
  (let* ((no-vc-state-display
          ;; no vc-state-display when the type of FILES means subdirs in
          ;; the directories-buffer
          (and (equal (buffer-name) ecb-directories-buffer-name)
               (= type ecb-directories-nodetype-directory)))
         (dir-managed-by-vc (if (or no-vc-state-display
                                    (not (ecb-vc-directory-should-be-checked-p path)))
                                nil
                              (ecb-vc-managed-dir-p path))))
    (dolist (file files)
      (let* ((filename (ecb-fix-filename path file))
             (file-1 (if include-extension
                         file
                       (ecb-file-name-sans-extension file))))
        (tree-node-add-children
         node
         (ecb-new-child
          old-children
          (if no-vc-state-display
              file-1
            (if dir-managed-by-vc
                (ecb-vc-generate-node-name file-1
                                           (nth 0 (ecb-vc-cache-get filename)))
              (ecb-generate-node-name file-1 -1 "leaf"
                                      ecb-sources-buffer-name)))
          type filename
          (or not-expandable
              (= type ecb-directories-nodetype-sourcefile)
              ;; The empty-dir-check is performed stealthy
              nil ;;(ecb-check-emptyness-of-dir filename)
              )
          (if ecb-truncate-long-names 'end)))))))

(defun ecb-new-child (old-children name type data
    &optional not-expandable shrink-name)
  "Return a node with type = TYPE, data = DATA and name = NAME. Tries to find
a node with matching TYPE and DATA in OLD-CHILDREN. If found no new node is
created but only the fields of this node will be updated. Otherwise a new node
is created."
  (catch 'exit
    (dolist (child old-children)
      (when (and (equal (tree-node->data child) data)
                 (= (tree-node->type child) type))
        (setf (tree-node->name child) name)
        (if not-expandable
            (setf (tree-node->expandable child) nil))
        (throw 'exit child)))
    (tree-node-new name type data not-expandable nil shrink-name)))

(defun ecb-add-source-path (&optional dir alias no-prompt-for-future-session)
  "Add a directory to the `ecb-source-path'."
  (interactive)
  ;; we must manually cut a filename because we must not add filenames to
  ;; `ecb-source-path'!
  (let* ((use-dialog-box nil)
         (my-dir (ecb-fix-filename
                  (or dir
                      (ecb-file-name-directory (read-file-name "Add source path: ")))
                  nil t))
         (my-alias (or alias
                       (read-string (format "Alias for \"%s\" (empty = no alias): "
                                            my-dir)))))
    (setq ecb-source-path (append ecb-source-path
                                  (list (if (> (length my-alias) 0)
                                            (list my-dir my-alias) my-dir))))
    (ecb-update-directories-buffer)
    (if (and (not no-prompt-for-future-session)
             (y-or-n-p "Add the new source-path also for future-sessions? "))
        (ecb-customize-save-variable 'ecb-source-path ecb-source-path)
      (customize-set-variable 'ecb-source-path ecb-source-path))))

(tree-buffer-defpopup-command ecb-add-source-path-node
  "Runs `ecb-add-source-path' from popup."
  (call-interactively 'ecb-add-source-path))


(tree-buffer-defpopup-command ecb-node-to-source-path
  "Add this node to the source-path."
  (ecb-add-source-path (tree-node->data node)))


(defun ecb-delete-s (child children sources)
  (when children
    (if (eq child (car children))
	(cdr sources)
      (cons (car sources) (ecb-delete-s child (cdr children) (cdr sources))))))


(tree-buffer-defpopup-command ecb-delete-source-path
  "Delete this source-path via popup."
  (let ((path (tree-node->data node)))
    (when (ecb-confirm (concat "Really delete source-path " path "?"))
      (setq ecb-source-path (ecb-delete-s
                             node (tree-node->children (tree-node->parent node))
                             ecb-source-path))
      (ecb-update-directories-buffer)
      (if (y-or-n-p "Delete source-path also for future-sessions? ")
          (ecb-customize-save-variable 'ecb-source-path ecb-source-path)
        (customize-set-variable 'ecb-source-path ecb-source-path)))))


(defun ecb-remove-dir-from-caches (dir)
  "Remove DIR from the caches SUBDIR, EMPTY-DIR and SOURCES."
  (ecb-files-and-subdirs-cache-remove dir)
  (ecb-directory-empty-cache-remove dir)
  (ecb-sources-cache-remove dir))

(defun ecb-directory-update-speedbar (dir)
  "Update the integrated speedbar if necessary."
  (and (ecb-speedbar-active-p)
       ;; depending on the value of `ecb-directory-update-speedbar' we have to
       ;; check if it is senseful to update the speedbar.
       (or (equal ecb-directories-update-speedbar t)
           (and (equal ecb-directories-update-speedbar 'auto)
                (not (or (get-buffer-window ecb-sources-buffer-name ecb-frame)
                         (member ecb-layout-name
                                 ecb-show-sources-in-directories-buffer))))
           (and (not (equal ecb-directories-update-speedbar 'auto))
                (functionp ecb-directories-update-speedbar)
                (funcall ecb-directories-update-speedbar dir)))
       (ecb-speedbar-update-contents)))

(defun ecb-directory-clicked (node ecb-button edit-window-nr shift-mode meta-mode)
  "Handle clicking onto NODE in the directories-buffer. ECB-BUTTON can be 1, 2
or 3. If 3 then EDIT-WINDOW-NR contains the number of the edit-window the NODE
should be displayed. For 1 and 2 the value of EDIT-WINDOW-NR is ignored."
  (if (= 3 (tree-node->type node))
      (funcall (tree-node->data node))
    (ecb-update-directory-node node)
    (if shift-mode
        (ecb-mouse-over-directory-node node nil nil 'force))
    (if (or (= ecb-directories-nodetype-directory
               (tree-node->type node))
            (= ecb-directories-nodetype-sourcepath
               (tree-node->type node)))
        (progn
          (if (= 2 ecb-button)
              (when (tree-node->expandable node)
                (tree-node-toggle-expanded node)
                (ecb-exec-in-window ecb-directories-buffer-name
                  ;; Update the tree-buffer with optimized display of NODE
                  (tree-buffer-update node)))
            
            ;; Removing the element from the sources-cache, the
            ;; files-and-subdirs-cache and the empty-dirs-cache (incl. all
            ;; subdirs)
            (when shift-mode
              (ecb-remove-dir-from-caches (tree-node->data node))
              (ecb-directory-empty-cache-remove-all (tree-node->data node))
              ;; a powerclick should remove all vc-caches of contained files
              (ecb-vc-cache-remove-files-of-dir (tree-node->data node))
              )

            ;; if we are in a maximized directories-window and if no sources
            ;; are shown in the directories-buffer but a sources-buffer is
            ;; contained in current layout then we have to redraw the full
            ;; layout first so the contents of the clicked directory can be
            ;; displayed in the sources-buffer.
            (when (and (ecb-buffer-is-maximized-p ecb-directories-buffer-name)
                       (not (ecb-show-sources-in-directories-buffer-p))
                       (ecb-buffer-is-ecb-buffer-of-current-layout-p
                        ecb-sources-buffer-name))
              (if (ecb-member-of-symbol/value-list
                   ecb-directories-buffer-name
                   ecb-maximize-next-after-maximized-select)
                  (progn
                    (ecb-maximize-ecb-buffer ecb-sources-buffer-name)
                    (ecb-window-select ecb-sources-buffer-name))
                (ecb-undo-maximize-ecb-buffer t)))
            
            (ecb-set-selected-directory (tree-node->data node) shift-mode)
            ;; if we have running an integrated speedbar we must update the
            ;; speedbar 
            (ecb-directory-update-speedbar (tree-node->data node))))
          
;;           (ecb-exec-in-directories-window
;;            ;; Update the tree-buffer with optimized display of NODE
;;            (tree-buffer-update node)))
      (ecb-set-selected-source (tree-node->data node)
                               (ecb-combine-ecb-button/edit-win-nr ecb-button edit-window-nr)
			       shift-mode meta-mode))))

(defun ecb-source-file-clicked (node ecb-button edit-window-nr shift-mode meta-mode)
  ;; if we are in a maximized sources-window and if a methods-buffer is
  ;; contained in current layout then we have to redraw the full layout first
  ;; so the contents of the clicked source-file can be displayed in the
  ;; methods-buffer.

  (when (and (ecb-buffer-is-maximized-p (buffer-name))
             (ecb-buffer-is-ecb-buffer-of-current-layout-p
              ecb-methods-buffer-name))
    (if (ecb-member-of-symbol/value-list
         (buffer-name)
         ecb-maximize-next-after-maximized-select)
        (progn
          (ecb-maximize-ecb-buffer ecb-methods-buffer-name)
          (ecb-window-select ecb-methods-buffer-name))
      (ecb-undo-maximize-ecb-buffer t)))
  (ecb-set-selected-source (tree-node->data node)
                           (ecb-combine-ecb-button/edit-win-nr ecb-button edit-window-nr)
			   shift-mode meta-mode))

(defun ecb-source-clicked (node ecb-button edit-window-nr shift-mode meta-mode)
  "Handle clicking onto NODE in the sources-buffer. ECB-BUTTON can be 1, 2 or
3. If 3 then EDIT-WINDOW-NR contains the number of the edit-window the NODE
should be displayed. For 1 and 2 the value of EDIT-WINDOW-NR is ignored."
  (if shift-mode
      (ecb-mouse-over-source-node node nil nil 'force))
  (ecb-source-file-clicked node ecb-button edit-window-nr shift-mode meta-mode))

(defun ecb-history-clicked (node ecb-button edit-window-nr shift-mode meta-mode)
  "Handle clicking onto NODE in the history-buffer. ECB-BUTTON can be 1, 2 or
3. If 3 then EDIT-WINDOW-NR contains the number of the edit-window the NODE
should be displayed. For 1 and 2 the value of EDIT-WINDOW-NR is ignored."
  (if shift-mode
      (ecb-mouse-over-history-node node nil nil 'force))
  (ecb-source-file-clicked node ecb-button edit-window-nr shift-mode meta-mode))

(defun ecb-expand-directory-nodes (level)
  "Set the expand level of the nodes in the ECB-directories-buffer.
For argument LEVEL see `ecb-expand-methods-nodes'.

Be aware that for deep structured paths and a lot of source-paths this command
can last a long time - depending of machine- and disk-performance."
  (interactive "nLevel: ")
  (ecb-exec-in-window ecb-directories-buffer-name
    (dolist (node (tree-node->children (tree-buffer-get-root)))
      (tree-buffer-expand-node node level))
    (tree-buffer-update))
  (ecb-current-buffer-sync 'force))


(defun ecb-get-file-info-text (file)
  "Return a file-info string for a file in the ECB sources buffer"
  (let ((attrs (ecb-file-attributes file)))
    (format "%s %8s %4d %10d %s %s"
	    (nth 8 attrs)
	    (user-login-name (nth 2 attrs))
	    (nth 3 attrs)
	    (nth 7 attrs)
	    (format-time-string "%Y/%m/%d %H:%M" (nth 5 attrs))
            (if (equal (cdr ecb-sources-show-node-info) 'file-info-full)
                file
              (ecb-file-name-nondirectory file)))
    ))


(defun ecb-mouse-over-directory-node (node &optional window no-message click-force)
  "Displays help text if mouse moves over a node in the directory buffer or if
CLICK-FORCE is not nil and always with regards to the settings in
`ecb-directories-show-node-info'. NODE is the node for which help text should
be displayed, WINDOW is the related window, NO-MESSAGE defines if the
help-text should be printed here."
  (if (= (tree-node->type node) ecb-directories-nodetype-sourcefile)
      (ecb-mouse-over-source-node node window no-message click-force)
    (if (not (= (tree-node->type node) 3))
        (let ((str (when (or click-force
                             (ecb-show-minibuffer-info node window
                                                       (car ecb-directories-show-node-info))
                             (and (not (equal (car ecb-directories-show-node-info)
                                              'never))
                                  (not (ecb-string= (tree-node->data node)
                                                    (tree-node->name node)))
                                  (eq (tree-node->parent node)
                                      (tree-buffer-get-root))))
                     (if (equal (cdr ecb-directories-show-node-info) 'name)
                         (tree-node->name node)
                       (tree-node->data node)))))
          (prog1 str
            (unless no-message
              (ecb-nolog-message str)))))))


(defun ecb-mouse-over-source-node (node &optional window no-message click-force)
  "Displays help text if mouse moves over a node in the sources buffer or if
CLICK-FORCE is not nil and always with regards to the settings in
`ecb-sources-show-node-info'. NODE is the node for which help
text should be displayed, WINDOW is the related window, NO-MESSAGE defines if
the help-text should be printed here."
  (let ((str (ignore-errors ;; For buffers that hasn't been saved yet
               (when (or click-force
                         (ecb-show-minibuffer-info node window
                                                   (car ecb-sources-show-node-info)))
                 (if (equal (cdr ecb-sources-show-node-info) 'name)
                     (tree-node->name node)
                   (ecb-get-file-info-text (tree-node->data node)))))))
    (prog1 str
      (unless no-message
        (ecb-nolog-message str)))))


(defun ecb-mouse-over-history-node (node &optional window no-message click-force)
  "Displays help text if mouse moves over a node in the history buffer or if
CLICK-FORCE is not nil and always with regards to the settings in
`ecb-history-show-node-info'. NODE is the node for which help
text should be displayed, WINDOW is the related window, NO-MESSAGE defines if
the help-text should be printed here."
  (let ((str (ignore-errors ;; For buffers that hasn't been saved yet
               (when (or click-force
                         (ecb-show-minibuffer-info node window
                                                   (car ecb-history-show-node-info)))
                 (if (equal (cdr ecb-history-show-node-info) 'name)
                     (tree-node->name node)
                   (tree-node->data node))))))
    (prog1 str
      (unless no-message
        (ecb-nolog-message str)))))

;; popups

;; needs methods
(tree-buffer-defpopup-command ecb-create-source
  "Creates a new sourcefile in current directory."
  (let* ((use-dialog-box nil)
         (dir (ecb-fix-filename
               (funcall (if (ecb-file-directory-p (tree-node->data node))
                            'identity
                          'ecb-file-name-directory)
                        (tree-node->data node))))
         (filename (ecb-file-name-nondirectory
                    (read-file-name "Source name: " (concat dir "/")))))
    (ecb-select-edit-window)
    (if (string-match "\\.java$" filename)
        (ecb-jde-gen-class-buffer dir filename)
      (find-file (concat dir "/" filename)))
    (when (= (point-min) (point-max))
      (set-buffer-modified-p t)
      (let ((ecb-auto-update-methods-after-save nil))
        (save-buffer))
      (ecb-rebuild-methods-buffer-with-tagcache nil nil t))
    (ecb-remove-dir-from-caches dir)
    (ecb-set-selected-directory dir t)
    (ecb-current-buffer-sync)))


(defun ecb-grep-directory-internal (node find)
  (ecb-select-edit-window)
  (let* ((node-data (tree-node->data node))
         (default-directory (concat (ecb-fix-filename
                                     (if (ecb-file-directory-p node-data)
                                         node-data
                                       (ecb-file-name-directory node-data)))
                                    (ecb-directory-sep-string node-data))))
    (call-interactively (if find
                            (or (and (fboundp ecb-grep-find-function)
                                     ecb-grep-find-function)
                                'grep-find)
                          (or (and (fboundp ecb-grep-function)
                                   ecb-grep-function)
                              'grep)))))


(tree-buffer-defpopup-command ecb-grep-find-directory
  "Runs grep-find for current directory."
  (ecb-grep-directory-internal node t))


(tree-buffer-defpopup-command ecb-grep-directory
  "Runs grep for current directory."
  (ecb-grep-directory-internal node nil))


(defun ecb-create-directory (parent-node)
  (make-directory (concat (tree-node->data parent-node) "/"
                          (read-from-minibuffer "Directory name: ")))
  (ecb-update-directory-node parent-node)
  (tree-buffer-update))


(tree-buffer-defpopup-command ecb-delete-directory
  "Delete current directory."
  (let ((dir (tree-node->data node)))
    (when (ecb-confirm (concat "Really delete directory" dir "? "))
      (delete-directory (tree-node->data node))
      (ecb-update-directory-node (tree-node->parent node))
      (tree-buffer-update))))


(defun ecb-dired-directory-internal (node &optional other)
  (ecb-select-edit-window)
  (let ((dir (ecb-fix-filename
              (funcall (if (ecb-file-directory-p (tree-node->data node))
                           'identity
                         'ecb-file-name-directory)
                       (tree-node->data node)))))
    (ecb-with-adviced-functions
     (funcall (if other
                  'dired-other-window
                'dired)
              dir))))


(tree-buffer-defpopup-command ecb-dired-directory
  "Run dired for this directory."
  (ecb-dired-directory-internal node))


(tree-buffer-defpopup-command ecb-dired-directory-other-window
  "Run dired for this directory in the other window."
  (ecb-dired-directory-internal node 'other))


(defun ecb-dir-run-cvs-op (node op op-arg-list)
  (let ((dir (tree-node->data node)))
    (funcall op dir op-arg-list)))


(tree-buffer-defpopup-command ecb-dir-popup-cvs-status
  "Check status of directory \(and below) in pcl-cvs mode."
  (ecb-dir-run-cvs-op node 'cvs-status '("-v")))


(tree-buffer-defpopup-command ecb-dir-popup-cvs-examine
  "Examine directory \(and below) in pcl-cvs mode."
  (ecb-dir-run-cvs-op node 'cvs-examine '("-d" "-P")))


(tree-buffer-defpopup-command ecb-dir-popup-cvs-update
  "Update directory \(and below) in pcl-cvs mode."
  (ecb-dir-run-cvs-op node 'cvs-update '("-d" "-P")))


(defvar ecb-common-directories-menu nil)


(setq ecb-common-directories-menu
      '(("Grep"
         (ecb-grep-directory "Grep Directory")
         (ecb-grep-find-directory "Grep Directory recursive"))
        ;;("---")
        ("Dired"
         (ecb-dired-directory "Open in Dired")
         (ecb-dired-directory-other-window "Open in Dired other window"))
        ("---")
	(ecb-create-source "Create Sourcefile")
	(ecb-create-directory "Create Child Directory")
	(ecb-delete-directory "Delete Directory")
        ("---")
	(ecb-add-source-path-node "Add Source Path")))


(defvar ecb-directories-menu nil
  "Built-in menu for the directories-buffer for directories which are not a
source-path of `ecb-source-path'.")
(setq ecb-directories-menu
      (append
       ecb-common-directories-menu
       '((ecb-node-to-source-path "Make This a Source Path")
         ("---")
         (ecb-maximize-ecb-window-menu-wrapper "Maximize window"))))


(defvar ecb-directories-menu-title-creator
  (function (lambda (node)
              (let ((node-type (tree-node->type node))
                    (node-data (tree-node->name node)))
                    (cond ((= node-type ecb-directories-nodetype-directory)
                           (format "%s  (Directory)" node-data))
                          ((= node-type ecb-directories-nodetype-sourcefile)
                           (format "%s  (File)" node-data))
                          ((= node-type ecb-directories-nodetype-sourcepath)
                           (format "%s  (Source-path)" node-data))))))
  "The menu-title for the directories menu. Has to be either a string or a
function which is called with current node and has to return a string.")

(tree-buffer-defpopup-command ecb-open-source-in-editwin1
  "Open current source-file the 1. edit-window."
  ;; We can use `ecb-source-clicked' for history-buffer too because shift-mode
  ;; is nil.
  (ecb-source-clicked node 3 1 nil nil))
(tree-buffer-defpopup-command ecb-open-source-in-editwin2
  "Open current source-file the 2. edit-window."
  (ecb-source-clicked node 3 2 nil nil))
(tree-buffer-defpopup-command ecb-open-source-in-editwin3
  "Open current source-file the 3. edit-window."
  (ecb-source-clicked node 3 3 nil nil))
(tree-buffer-defpopup-command ecb-open-source-in-editwin4
  "Open current source-file the 4. edit-window."
  (ecb-source-clicked node 3 4 nil nil))
(tree-buffer-defpopup-command ecb-open-source-in-editwin5
  "Open current source-file the 5. edit-window."
  (ecb-source-clicked node 3 5 nil nil))
(tree-buffer-defpopup-command ecb-open-source-in-editwin6
  "Open current source-file the 6. edit-window."
  (ecb-source-clicked node 3 6 nil nil))
(tree-buffer-defpopup-command ecb-open-source-in-editwin7
  "Open current source-file the 7. edit-window."
  (ecb-source-clicked node 3 7 nil nil))
(tree-buffer-defpopup-command ecb-open-source-in-editwin8
  "Open current source-file the 8. edit-window."
  (ecb-source-clicked node 3 8 nil nil))

(defun ecb-dir/source/hist-menu-editwin-entries ()
  "Generate popup-menu-entries for each edit-window if there are at least 2
edit-windows. Otherwise return nil."
  (let ((edit-win-list (ecb-canonical-edit-windows-list))
        (result nil))
    (when (> (length edit-win-list) 1)
      (dotimes (i (min 8 (length edit-win-list)))
        (setq result
              (append result
                      (list (list (intern (format "ecb-open-source-in-editwin%d" (1+ i)))
                                  (format "edit-window %d" (1+ i)))))))
      (append (list (list "---")) ;; we want a separator
              (list (append (list "Open source-file in ...")
                            result))))))


(defun ecb-directories-menu-creator (tree-buffer-name node)
  "Creates the popup-menus for the directories-buffer."
  (setq ecb-layout-prevent-handle-ecb-window-selection t)
  (let ((dyn-user-extension
         (and (functionp ecb-directories-menu-user-extension-function)
              (funcall ecb-directories-menu-user-extension-function)))
        (dyn-builtin-extension (ecb-dir/source/hist-menu-editwin-entries)))
    (list (cons ecb-directories-nodetype-directory
                (funcall (or ecb-directories-menu-sorter
                             'identity)
                         (append dyn-user-extension
                                 ecb-directories-menu-user-extension
                                 ecb-directories-menu)))
          (cons ecb-directories-nodetype-sourcefile
                (funcall (or ecb-sources-menu-sorter
                             'identity)
                         (append dyn-user-extension
                                 ecb-sources-menu-user-extension
                                 ecb-sources-menu
                                 dyn-builtin-extension)))
          (cons ecb-directories-nodetype-sourcepath
                (funcall (or ecb-directories-menu-sorter
                             'identity)
                         (append dyn-user-extension
                                 ecb-directories-menu-user-extension
                                 ecb-source-path-menu))))))

;; source-path tokens

(defvar ecb-source-path-menu nil
  "Built-in menu for the directories-buffer for directories which are elements of
`ecb-source-path'.")
(setq ecb-source-path-menu
      (append
       ecb-common-directories-menu
       '((ecb-delete-source-path "Delete Source Path")
         ("---")
         (ecb-maximize-ecb-window-menu-wrapper "Maximize window"))))


(tree-buffer-defpopup-command ecb-delete-source
  "Deletes current sourcefile."
  (let* ((file (tree-node->data node))
         (dir (ecb-fix-filename (ecb-file-name-directory file))))
    (when (ecb-confirm (concat "Really delete " (ecb-file-name-nondirectory file) "? "))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (ecb-delete-file file)
      (ecb-remove-dir-from-caches dir)
      (ecb-set-selected-directory dir t))))


(tree-buffer-defpopup-command ecb-file-popup-ediff-revision
  "Diff file against repository with ediff."
  (let ((file (tree-node->data node)))
    (ediff-revision file)))


(tree-buffer-defpopup-command ecb-file-popup-vc-next-action
  "Checkin/out file."
  (let ((file (tree-node->data node)))
    (find-file file)
    (vc-next-action nil)))


(tree-buffer-defpopup-command ecb-file-popup-vc-log
  "Print revision history of file."
  (let ((file (tree-node->data node)))
    (find-file file)
    (vc-print-log)))


(tree-buffer-defpopup-command ecb-file-popup-vc-annotate
  "Annotate file"
  (let ((file (tree-node->data node)))
    (find-file file)
    (vc-annotate nil)))

(tree-buffer-defpopup-command ecb-file-popup-vc-diff
  "Diff file against last version in repository."
  (let ((file (tree-node->data node)))
    (find-file file)
    (vc-diff nil)))

(tree-buffer-defpopup-command ecb-file-popup-vc-refresh-file
  "Recompute the VC-state for this file."
  (let ((file (tree-node->data node)))
    (ecb-vc-cache-remove file)
    (ecb-vc-reset-vc-stealthy-checks)))

(tree-buffer-defpopup-command ecb-file-popup-vc-refresh-dir
  "Recompute the VC-state-values for the whole directory."
  (let ((dir (ecb-fix-filename (ecb-file-name-directory (tree-node->data node)))))
    (ecb-vc-cache-remove-files-of-dir dir)
    (ecb-vc-reset-vc-stealthy-checks)))

(defvar ecb-sources-menu nil
  "Built-in menu for the sources-buffer.")

(setq ecb-sources-menu
      '(("Grep"
         (ecb-grep-directory "Grep Directory")
         (ecb-grep-find-directory "Grep Directory recursive"))
        ("Dired"
         (ecb-dired-directory "Open Dir in Dired")
         (ecb-dired-directory-other-window "Open Dir in Dired other window"))
        ("Filter"
         (ecb-popup-sources-filter-by-ext "Filter by extension")
         (ecb-popup-sources-filter-by-regexp "Filter by a regexp")
         (ecb-popup-sources-filter-none "No filter"))
        ("---")        
	(ecb-create-source "Create Sourcefile")
        (ecb-delete-source "Delete Sourcefile")
        ("---")
        (ecb-maximize-ecb-window-menu-wrapper "Maximize window")))


(defvar ecb-sources-menu-title-creator
  (function (lambda (node)
              (ecb-file-name-nondirectory (tree-node->data node))))
  "The menu-title for the sources menu. See
`ecb-directories-menu-title-creator'.")

(defun ecb-sources-menu-creator (tree-buffer-name node)
  "Creates the popup-menus for the sources-buffer."
  (setq ecb-layout-prevent-handle-ecb-window-selection t)
  (let ((dyn-user-extension
         (and (functionp ecb-sources-menu-user-extension-function)
              (funcall ecb-sources-menu-user-extension-function)))
        (dyn-builtin-extension (ecb-dir/source/hist-menu-editwin-entries)))
    (list (cons ecb-sources-nodetype-sourcefile
                (funcall (or ecb-sources-menu-sorter
                             'identity)
                         (append dyn-user-extension
                                 ecb-sources-menu-user-extension
                                 ecb-sources-menu
                                 dyn-builtin-extension))))))

;; history popups

(tree-buffer-defpopup-command ecb-history-kill-buffer
  "Kills the buffer for current entry."
  (let ((data (tree-node->data node)))
    (when (get-file-buffer data)
      (kill-buffer (get-file-buffer data)))))

(defun ecb-history-filter-by-ext (ext-str)
  "Filter history entries by extension. Returns t if the filter has been
applied otherwise nil."
  (if (= (length ext-str) 0)
      (setq ecb-history-filter
            (cons `(lambda (filename)
                     (save-match-data
                       (string-match "^[^.]+$" filename)))
                  "No ext."))
    (setq ecb-history-filter
          (cons `(lambda (filename)
                   (save-match-data
                     (string-match ,(format "\\.%s\\'" ext-str)
                                   filename)))
                (format "*.%s" ext-str))))
  (ecb-add-buffers-to-history))

(tree-buffer-defpopup-command ecb-popup-history-filter-by-ext
  "Filter history entries by extension by popup."
  (let ((ext-str (read-string "Insert the filter-extension without leading dot: "
                              (and node
                                   (ecb-file-name-extension (tree-node->data node))))))
    (ecb-history-filter-by-ext ext-str)))

(defun ecb-history-filter-by-regexp (&optional regexp filter-display)
  "Filter history entries by REGEXP. If the first optional argument REGEXP is
nil then it asks for a regexp. If second argument FILTER-DISPLAY is not nil
then it is displayed in the modeline of the history-buffer for current
regexp-filter. Otherwise the regexp itself. Returns t if the filter has been
applied otherwise nil."
  (let ((regexp-str (or regexp (read-string "Insert the filter-regexp: "))))
    (if (> (length regexp-str) 0)
        (setq ecb-history-filter
              (cons `(lambda (filename)
                       (save-match-data
                         (string-match ,regexp-str filename)))
                    (or filter-display regexp-str)))))
  (ecb-add-buffers-to-history))

(tree-buffer-defpopup-command ecb-popup-history-filter-by-regexp
  "Filter history entries by regexp by popup."
  (ecb-history-filter-by-regexp))
  
(tree-buffer-defpopup-command ecb-popup-history-filter-all-existing
  "No history filter, i.e. add all existing file-buffers to the history."
  (ecb-add-all-buffers-to-history))
  
(tree-buffer-defpopup-command ecb-file-popup-vc-refresh-all-files
  "Recompute the VC-state for the whole history."
  (when (equal (buffer-name) ecb-history-buffer-name)
    (let ((files (mapcar (function (lambda (node)
                                     (tree-node->data node)))
                         (tree-node->children (tree-buffer-get-root)))))
      (dolist (file files)
        (ecb-vc-cache-remove file))
      (ecb-vc-reset-vc-stealthy-checks))))

(defun ecb-history-filter ()
  "Apply a filter to the history-buffer to reduce the number of entries.
So you get a better overlooking. There are three choices:
- Filter by extension: Just insert the extension you want the History-buffer
  being filtered. Insert the extension without leading dot!
- Filter by regexp: Insert the filter as regular expression.
- No filter: This means to display an entry for all currently living
  file-buffers."
  (interactive)
  (let ((choice (ecb-query-string "Filter history by:"
                                  '("extension" "regexp" "no filter"))))
    (cond ((ecb-string= choice "extension")
           (ecb-history-filter-by-ext
            (read-string "Insert the filter-extension without leading dot: ")))
          ((ecb-string= choice "regexp")
           (ecb-history-filter-by-regexp))
          (t (ecb-add-all-buffers-to-history)))))

(defvar ecb-history-menu nil
  "Built-in menu for the history-buffer.")


(setq ecb-history-menu
      '(("Grep"
         (ecb-grep-directory "Grep Directory")
         (ecb-grep-find-directory "Grep Directory recursive"))
        ;;("---")
        ("Dired"
         (ecb-dired-directory "Open Dir in Dired")
         (ecb-dired-directory-other-window "Open Dir in Dired other window"))
	("Filter"
         (ecb-popup-history-filter-by-ext "Filter by extension")
         (ecb-popup-history-filter-by-regexp "Filter by regexp")
         (ecb-popup-history-filter-all-existing "No filter"))
        ("---")
        (ecb-popup-history-filter-all-existing "Exactly all living file-buffers")
        (ecb-history-kill-buffer "Kill Buffer")
        (ecb-delete-source "Delete Sourcefile")
        ("---")
        (ecb-maximize-ecb-window-menu-wrapper "Maximize window")))


(defvar ecb-history-menu-title-creator
  (function (lambda (node)
              (tree-node->name node)))
  "The menu-title for the history menu. See
`ecb-directories-menu-title-creator'.")

(defun ecb-history-menu-creator (tree-buffer-name node)
  "Creates the popup-menus for the history-buffer."
  (setq ecb-layout-prevent-handle-ecb-window-selection t)
  (let ((dyn-user-extension
         (and (functionp ecb-history-menu-user-extension-function)
              (funcall ecb-history-menu-user-extension-function)))
        (dyn-builtin-extension (ecb-dir/source/hist-menu-editwin-entries)))
    (list (cons ecb-history-nodetype-sourcefile
                (funcall (or ecb-history-menu-sorter 'identity)
                         (append dyn-user-extension
                                 ecb-history-menu-user-extension
                                 ecb-history-menu
                                 dyn-builtin-extension))))))

;; create the tree-buffers

(defecb-tree-buffer-creator ecb-create-directories-tree-buffer
    ecb-directories-buffer-name
  "Create the tree-buffer for directories"
  (tree-buffer-create
   ecb-directories-buffer-name
   :frame ecb-frame
   :mouse-action-trigger ecb-tree-mouse-action-trigger
   :is-click-valid-fn 'ecb-interpret-mouse-click
   :node-selected-fn 'ecb-tree-buffer-node-select-callback
   :node-expanded-fn 'ecb-tree-buffer-node-expand-callback
   :node-collapsed-fn 'ecb-tree-buffer-node-collapsed-callback
   :node-mouse-over-fn 'ecb-mouse-over-directory-node
   :mouse-highlight-fn t ;; highlight each node when moving mouse over it
   :node-data-equal-fn 'equal
   :maybe-empty-node-types (list ecb-directories-nodetype-directory)
   ;; Now no longer tree-buffer decides if a node is displayed as leave but
   ;; now the file-browser does it in the function `ecb-tree-node-add-files' -
   ;; Reason: We have now to deal with the VC-support
   :leaf-node-types nil ;;(list ecb-directories-nodetype-sourcefile)
   :menu-creator 'ecb-directories-menu-creator
   :menu-titles (list (cons ecb-directories-nodetype-directory
                            ecb-directories-menu-title-creator)
                      (cons ecb-directories-nodetype-sourcefile
                            ecb-directories-menu-title-creator)
                      (cons ecb-directories-nodetype-sourcepath
                            ecb-directories-menu-title-creator))
   :modeline-menu-creator 'ecb-common-tree-buffer-modeline-menu-creator
   :trunc-lines (ecb-member-of-symbol/value-list ecb-directories-buffer-name
                                                 ecb-tree-truncate-lines)
   :read-only t
   :tree-indent ecb-tree-indent
   :incr-search-p ecb-tree-incremental-search
   :incr-search-additional-pattern ecb-vc-incr-searchpattern-node-prefix
   :arrow-navigation ecb-tree-navigation-by-arrow
   :hor-scroll-step ecb-tree-easy-hor-scroll
   :default-images-dir (car ecb-tree-image-icons-directories)
   :additional-images-dir (ecb-member-of-symbol/value-list ecb-directories-buffer-name
                                                           (cdr ecb-tree-image-icons-directories)
                                                           'car 'cdr)
   :image-file-prefix "ecb-"
   :tree-style ecb-tree-buffer-style
   :ascii-guide-face ecb-tree-guide-line-face
   :type-facer (list (cons ecb-directories-nodetype-sourcefile
                           ecb-source-in-directories-buffer-face))
   :expand-symbol-before-p ecb-tree-expand-symbol-before
   :highlight-node-face ecb-directory-face
   :general-face ecb-directories-general-face
   ;; we add an after-create-hook to the tree-buffer
   :after-create-hook (append
                       (list (function (lambda ()
                                         (local-set-key [f2] 'ecb-customize)
                                         (local-set-key [f3] 'ecb-show-help)
                                         (local-set-key [f4] 'ecb-add-source-path)
                                         (ecb-common-after-tree-buffer-create-actions))))
                       ecb-common-tree-buffer-after-create-hook
                       ecb-directories-buffer-after-create-hook)
   :after-update-hook 'ecb-stealth-tasks-after-directories-update
   ))

(defecb-tree-buffer-creator ecb-create-sources-tree-buffer ecb-sources-buffer-name
  "Create the tree-buffer for sources"
  (tree-buffer-create
   ecb-sources-buffer-name
   :frame ecb-frame
   :mouse-action-trigger ecb-tree-mouse-action-trigger
   :is-click-valid-fn 'ecb-interpret-mouse-click
   :node-selected-fn 'ecb-tree-buffer-node-select-callback
   :node-expanded-fn 'ecb-tree-buffer-node-expand-callback
   :node-collapsed-fn 'ecb-tree-buffer-node-collapsed-callback
   :node-mouse-over-fn 'ecb-mouse-over-source-node
   :mouse-highlight-fn t ;; highlight each node when moving mouse over it
   :node-data-equal-fn 'equal
   :maybe-empty-node-types nil
   ;; If we want to display the VC-state in the sources-icon then we should
   ;; set this argument to nil because then we must compute the needed icon in
   ;; the file-browser and not in the tree-buffer-library (analogue to the
   ;; methods-icons computet in the methods-browser).
   :leaf-node-types nil ;; (list ecb-sources-nodetype-sourcefile)
   :menu-creator 'ecb-sources-menu-creator
   :menu-titles (list (cons ecb-sources-nodetype-sourcefile
                            ecb-sources-menu-title-creator))
   :modeline-menu-creator 'ecb-common-tree-buffer-modeline-menu-creator
   :trunc-lines (ecb-member-of-symbol/value-list ecb-sources-buffer-name
                                                 ecb-tree-truncate-lines)
   :read-only t
   :tree-indent ecb-tree-indent
   :incr-search-p ecb-tree-incremental-search
   :incr-search-additional-pattern ecb-vc-incr-searchpattern-node-prefix
   ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: make an option for this
   :reduce-tree-for-incr-search-fn
   (lambda (search full-search-regexp)
     (if (ecb-apply-filter-to-sources-buffer (and search
                                                  (> (length search) 0)
                                                  full-search-regexp)
                                             search)
         search
       ""))
   :arrow-navigation ecb-tree-navigation-by-arrow
   :hor-scroll-step ecb-tree-easy-hor-scroll
   :default-images-dir (car ecb-tree-image-icons-directories)
   :additional-images-dir (ecb-member-of-symbol/value-list ecb-sources-buffer-name
                                                           (cdr ecb-tree-image-icons-directories)
                                                           'car 'cdr)
   :image-file-prefix "ecb-"
   :tree-style ecb-tree-buffer-style
   :ascii-guide-face ecb-tree-guide-line-face
   :type-facer nil
   :expand-symbol-before-p ecb-tree-expand-symbol-before
   :highlight-node-face ecb-source-face
   :general-face ecb-sources-general-face
   :after-create-hook (append
                       (list (function (lambda ()
                                         (ecb-common-after-tree-buffer-create-actions))))
                       ecb-common-tree-buffer-after-create-hook
                       ecb-sources-buffer-after-create-hook)
   :after-update-hook 'ecb-stealth-tasks-after-sources-update))

(defecb-tree-buffer-creator ecb-create-history-tree-buffer ecb-history-buffer-name
  "Create the tree-buffer for history"
  (tree-buffer-create
   ecb-history-buffer-name
   :frame ecb-frame
   :mouse-action-trigger ecb-tree-mouse-action-trigger
   :is-click-valid-fn 'ecb-interpret-mouse-click
   :node-selected-fn 'ecb-tree-buffer-node-select-callback
   :node-expanded-fn 'ecb-tree-buffer-node-expand-callback
   :node-collapsed-fn 'ecb-tree-buffer-node-collapsed-callback
   :node-mouse-over-fn 'ecb-mouse-over-history-node
   :mouse-highlight-fn t ;; highlight each node when moving mouse over it
   :node-data-equal-fn 'equal
   :maybe-empty-node-types nil
   :leaf-node-types nil
   :menu-creator 'ecb-history-menu-creator
   :menu-titles (list (cons ecb-history-nodetype-sourcefile
                            ecb-history-menu-title-creator))
   :modeline-menu-creator 'ecb-common-tree-buffer-modeline-menu-creator
   :trunc-lines (ecb-member-of-symbol/value-list ecb-history-buffer-name
                                                 ecb-tree-truncate-lines)
   :read-only t
   :tree-indent ecb-tree-indent
   :incr-search-p ecb-tree-incremental-search
   :incr-search-additional-pattern ecb-vc-incr-searchpattern-node-prefix
   :arrow-navigation ecb-tree-navigation-by-arrow
   :hor-scroll-step ecb-tree-easy-hor-scroll
   :default-images-dir (car ecb-tree-image-icons-directories)
   :additional-images-dir (ecb-member-of-symbol/value-list ecb-history-buffer-name
                                                           (cdr ecb-tree-image-icons-directories)
                                                           'car 'cdr)
   :image-file-prefix "ecb-"
   :tree-style ecb-tree-buffer-style
   :ascii-guide-face ecb-tree-guide-line-face
   :type-facer nil
   :expand-symbol-before-p ecb-tree-expand-symbol-before
   :highlight-node-face ecb-history-face
   :general-face ecb-history-general-face
   :after-create-hook (append
                       (list (function (lambda ()
                                         (ecb-common-after-tree-buffer-create-actions))))
                       ecb-common-tree-buffer-after-create-hook
                       ecb-history-buffer-after-create-hook)
   :after-update-hook 'ecb-stealth-tasks-after-history-update))


(silentcomp-provide 'ecb-file-browser)

;;; ecb-file-browser.el ends here
