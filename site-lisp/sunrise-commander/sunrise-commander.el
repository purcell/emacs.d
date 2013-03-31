;;; sunrise-commander.el --- two-pane file manager for Emacs based on Dired and inspired by MC  -*- lexical-binding: t -*-

;; Copyright (C) 2007-2012 José Alfredo Romero Latouche.

;; Author: José Alfredo Romero L. <escherdragon@gmail.com>
;;	Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero L. <escherdragon@gmail.com>
;; Created: 24 Sep 2007
;; Version: 6
;; RCS Version: $Rev: 446 $
;; Keywords: files, dired, midnight commander, norton, orthodox
;; URL: http://www.emacswiki.org/emacs/sunrise-commander.el
;; Compatibility: GNU Emacs 22+

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more de-
;; tails.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The Sunrise Commmander is an double-pane file manager for Emacs. It's built
;; atop of Dired and takes advantage of all its power, but also provides many
;; handy features of its own:

;; * Sunrise is implemented as a derived major mode confined inside the pane
;; buffers, so its buffers and Dired ones can live together without easymenu or
;; viper to avoid key binding collisions.

;; * It automatically closes unused buffers and tries to never keep open more
;; than the one or two used to display the panes, though this behavior may be
;; disabled if desired.

;; * Each pane has its own history stack: press M-y / M-u for moving backwards /
;; forwards in the history of directories.

;; * Press M-t to swap (transpose) the panes.

;; * Press C-= for "smart" file comparison using `ediff'. It compares together
;; the first two files marked on each pane or, if no files have been marked, it
;; assumes that the second pane contains a file with the same name as the
;; selected one and tries to compare these two. You can also mark whole lists of
;; files to be compared and then just press C-= for comparing the next pair.

;; * Press = for fast "smart" file comparison -- like above, but using regular
;; diff.

;; * Press C-M-= for directory comparison (by date / size / contents of files).

;; * Press C-c C-s to change the layout of the panes (horizontal/vertical/top)

;; * Press C-c / to interactively refine the contents of the current pane using
;; fuzzy (a.k.a. flex) matching, then:
;;    - press Delete or Backspace to revert the buffer to its previous state
;;    - press Return, C-n or C-p to exit and accept the current narrowed state
;;    - press Esc or C-g to abort the operation and revert the buffer
;;    - use ! to prefix characters that should NOT appear after a given position
;; Once narrowed and accepted, you can restore the original contents of the pane
;; by pressing g (revert-buffer).

;; * Sticky search: press C-c s to launch an interactive search that will remain
;; active from directory to directory, until you hit a regular file or press C-g

;; * Press C-x C-q to put the current pane in Editable Dired mode (allows to
;; edit the pane as if it were a regular file -- press C-c C-c to commit your
;; changes to the filesystem, or C-c C-k to abort).

;; * Press y to recursively calculate the total size (in bytes) of all files and
;; directories currently selected/marked in the active pane.

;; * Sunrise VIRTUAL mode integrates dired-virtual mode to Sunrise, allowing to
;; capture find and locate results in regular files and to use them later as if
;; they were directories with all Dired and Sunrise operations at your
;; fingertips.
;; The results of the following operations are displayed in VIRTUAL mode:
;;    - find-name-dired (press C-c C-n),
;;    - find-grep-dired (press C-c C-g),
;;    - find-dired      (press C-c C-f),
;;    - locate          (press C-c C-l),
;;    - list all recently visited files (press C-c C-r -- requires recentf),
;;    - list all directories in active pane's history ring (press C-c C-d).

;; * Supports AVFS (http://avf.sourceforge.net/) for transparent navigation
;; inside compressed archives (*.zip, *.tgz, *.tar.bz2, *.deb, etc. etc.)
;; You need to have AVFS with coda or fuse installed and running on your system
;; for this to work, though.

;; * Opening terminals directly from Sunrise:
;;    - Press C-c C-t to inconditionally open a new terminal into the currently
;;      selected directory in the active pane.
;;    - Use C-c t to switch to the last opened terminal, or (when already inside
;;      a terminal) to cycle through all open terminals.
;;    - Press C-c T to switch to the last opened terminal and change directory
;;      to the one in the current directory.
;;    - Press C-c M-t to be prompted for a program name, and then open a new
;;      terminal using that program into the currently selected directory
;;      (eshell is a valid value; if no program can be found with the given name
;;      then the value of `sr-terminal-program' is used instead).

;; * Terminal integration and Command line expansion: integrates tightly with
;; `eshell' and `term-mode' to allow interaction between terminal emulators in
;; line mode (C-c C-j) and the panes: the most important navigation commands
;; (up, down, mark, unmark, go to parent dir) can be executed on the active pane
;; directly from the terminal by pressing the usual keys with Meta: <M-up>,
;; <M-down>, etc. Additionally, the following substitutions are automagically
;; performed in `eshell' and `term-line-mode':
;;     %f - expands to the currently selected file in the left pane
;;     %F - expands to the currently selected file in the right pane
;;     %m - expands to the list of paths of all marked files in the left pane
;;     %M - expands to the list of paths of all marked files in the right pane
;;     %n - expands to the list of names of all marked files in the left pane
;;     %N - expands to the list of names of all marked files in the right pane
;;     %d - expands to the current directory in the left pane
;;     %D - expands to the current directory in the right pane
;;     %a - expands to the list of paths of all marked files in the active pane
;;     %A - expands to the current directory in the active pane
;;     %p - expands to the list of paths of all marked files in the passive pane
;;     %P - expands to the current directory in the passive pane

;; * Cloning of complete directory trees: press K to clone the selected files
;; and directories into the passive pane. Cloning is a more general operation
;; than copying, in which all directories are recursively created with the same
;; names and structures at the destination, while what happens to the files
;; within them depends on the option you choose:
;;    - "(D)irectories only" ignores all files, copies only directories,
;;    - "(C)opies" performs a regular recursive copy of all files and dirs,
;;    - "(H)ardlinks" makes every new file a (hard) link to the original one
;;    - "(S)ymlinks" creates absolute symbolic links for all files in the tree,
;;    - "(R)elative symlinks” creates relative symbolic links.

;; * Passive navigation: the usual navigation keys (n, p, Return, U, ;) combined
;; with Meta allow to move across the passive pane without actually having to
;; switch to it.

;; * Synchronized navigation: press C-c C-z to enable / disable synchronized
;; navigation. In this mode, the passive navigation keys (M-n, M-p, M-Return,
;; etc.) operate on both panes simultaneously. I've found this quite useful for
;; comparing hierarchically small to medium-sized directory trees (for large to
;; very large directory trees one needs something on the lines of diff -r
;; though).

;; * And much more -- press ? while in Sunrise mode for basic help, or h for a
;; complete list of all keybindings available (use C-e and C-y to scroll).

;; There is no help window like in MC, but if you really miss it, just get and
;; install the sunrise-x-buttons extension.

;; A lot of this code was once adapted from Kevin Burton's mc.el, but it has
;; evolved considerably since then. Another part (the code for file copying and
;; renaming) derives originally from the Dired extensions written by Kurt
;; Nørmark for LAML (http://www.cs.aau.dk/~normark/scheme/distribution/laml/).

;; It was written on GNU Emacs 24 on Linux and tested on GNU Emacs 22, 23 and 24
;; for Linux and on EmacsW32 (version 23) for Windows. I have also received
;; feedback from users reporting it works OK on the Mac. It does not work either
;; on GNU Emacs 21 or XEmacs -- please drop me a line if you would like to help
;; porting it. All contributions and/or bug reports will be very welcome.

;; For more details on the file manager, several available extensions and many
;; cool tips & tricks visit http://www.emacswiki.org/emacs/Sunrise_Commander

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) Add a (require 'sunrise-commander) to your .emacs file.

;; 3) Choose some unused extension for files to be opened in Sunrise VIRTUAL
;; mode and add it to `auto-mode-alist', e.g. if you want to name your virtual
;; directories like *.svrm just add to your .emacs file a line like the
;; following:
;;
;;     (add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))

;; 4) Evaluate the new lines, or reload your .emacs file, or restart Emacs.

;; 5) Type M-x sunrise to invoke the Sunrise Commander (or much better: bind the
;; function to your favorite key combination). The command `sunrise-cd' invokes
;; Sunrise and automatically selects the current file wherever it is in the
;; filesystem. Type h at any moment for information on available key bindings.

;; 6) Type M-x customize-group <RET> sunrise <RET> to customize options, fonts
;; and colors (activate AVFS support here, too).

;; 7) Enjoy :)

;;; Code:

(require 'advice)
(require 'desktop)
(require 'dired)
(require 'dired-aux)
(require 'dired-x)
(require 'enriched)
(require 'esh-mode)
(require 'find-dired)
(require 'font-lock)
(require 'hl-line)
(require 'sort)
(require 'term)
(eval-when-compile (require 'cl)
                   (require 'recentf)
                   (require 'tramp))

(eval-and-compile
  (unless (fboundp 'cl-labels)
    (defalias 'cl-labels 'labels))
  (unless (fboundp 'cl-letf)
    (defalias 'cl-letf 'letf)))

(defgroup sunrise nil
  "The Sunrise Commander File Manager."
  :group 'files)

(defcustom sr-show-file-attributes t
  "Whether to initially display file attributes in Sunrise panes.
You can always toggle file attributes display pressing
\\<sr-mode-map>\\[sr-toggle-attributes]."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-autoload-extensions t
  "Whether to load extensions immediately after their declaration, or when the
SC core is loaded (e.g. when using autoload cookies)."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-show-hidden-files nil
  "Whether to initially display hidden files in Sunrise panes.
You can always toggle hidden files display pressing
\\<sr-mode-map>\\[dired-omit-mode].
You can also customize what files are considered hidden by setting
`dired-omit-files' and `dired-omit-extensions' in your .emacs file."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-terminal-kill-buffer-on-exit t
  "Whether to kill terminal buffers after their shell process ends."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-terminal-program "eshell"
  "The program to use for terminal emulation.
If this value is set to \"eshell\", the Emacs shell (`eshell')
will be used."
  :group 'sunrise
  :type 'string)

(defcustom sr-listing-switches "-al"
  "Listing switches passed to `ls' when building Sunrise buffers.
\(Cf. `dired-listing-switches'.)
  Most portable value: -al
  Recommended value on GNU systems: \
--time-style=locale --group-directories-first -alDhgG"
  :group 'sunrise
  :type 'string)

(defcustom sr-virtual-listing-switches "-ald"
  "Listing switches for building buffers in `sr-virtual-mode'.
Should not contain the -D option. See also `sr-listing-switches'."
  :group 'sunrise
  :type 'string)

(defcustom sr-avfs-root nil
  "Root of the AVFS virtual filesystem used for navigating compressed archives.
Setting this value activates AVFS support."
  :group 'sunrise
  :type '(choice
          (const :tag "AVFS support disabled" nil)
          (directory :tag "AVFS root directory")))

(defcustom sr-avfs-handlers-alist '(("\\.[jwesh]ar$" . "#uzip/")
                                    ("\\.wsar$"      . "#uzip/")
                                    ("\\.xpi$"       . "#uzip/")
                                    ("\\.apk$"       . "#uzip/")
                                    ("\\.iso$"       . "#iso9660/")
                                    ("\\.patch$"     . "#/")
                                    ("\\.txz$"       . "#/")
                                    ("."             . "#/"))
  "List of AVFS handlers to manage specific file extensions."
  :group 'sunrise
  :type 'alist)

(defcustom sr-md5-shell-command "md5sum %f | cut -d' ' -f1 2>/dev/null"
  "Shell command to use for calculating MD5 sums for files.
Used when comparing directories using the ``(c)ontents'' option.
Use %f as a placeholder for the name of the file."
  :group 'sunrise
  :type 'string)

(defcustom sr-window-split-style 'horizontal
  "The current window split configuration.
May be `horizontal', `vertical' or `top'."
  :group 'sunrise
  :type '(choice
          (const horizontal)
          (const vertical)
          (const top)))

(defcustom sr-windows-locked t
  "When non-nil, vertical size of the panes will remain constant."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-windows-default-ratio 66
  "Percentage of the total height of the frame to use by default for the Sunrise
Commander panes."
  :group 'sunrise
  :type 'integer
  :set (defun sr-set-windows-default-ratio (symbol value)
         "Setter function for the `sr-windows-default-ratio' custom option."
         (if (and (integerp value) (>= value 0) (<= value 100))
             (set-default symbol value)
           (error "Invalid value: %s" value))))

(defcustom sr-history-length 20
  "Number of entries to keep in each pane's history rings."
  :group 'sunrise
  :type 'integer)

(defcustom sr-kill-unused-buffers t
  "Whether buffers should be killed automatically by Sunrise when not displayed
in any of the panes."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-confirm-kill-viewer t
  "Whether to ask for confirmation before killing a buffer opened in quick-view
mode."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-attributes-display-mask nil
  "Contols hiding/transforming columns with `sr-toggle-attributes'.
If set, its value must be a list of symbols, one for each
attributes column. If the symbol is nil, then the corresponding
column will be hidden, and if it's not nil then the column will
be left untouched. The symbol may also be the name of a function
that takes one string argument and evaluates to a different
string -- in this case this function will be used to transform
the contents of the corresponding column and its result will be
displayed instead."
  :group 'sunrise
  :type '(repeat symbol))

(defcustom sr-fast-backup-extension ".bak"
  "Determines the extension to append to the names of new files
created with the `sr-fast-backup-files' function (@!). This can
be either a simple string or an s-expression to be evaluated at
run-time."
  :group 'sunrise
  :type '(choice
          (string :tag "Literal text")
          (sexp :tag "Symbolic expression")))

(defcustom sr-traditional-other-window nil
  "Sunrise modifies the behavior of the `other-window' command,
so that focus is always given to the currently selected pane when
switching from external windows. If you'd prefer the original
Emacs behavior instead, then set this flag to t."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-fuzzy-negation-character ?!
  "Character to use for negating patterns when fuzzy-narrowing a pane."
  :group 'sunrise
  :type '(choice
          (const :tag "Fuzzy matching negation disabled" nil)
          (character :tag "Fuzzy matching negation character" ?!)))

(defcustom sr-init-hook nil
  "List of functions to be called before the Sunrise panes are displayed."
  :group 'sunrise
  :type 'hook
  :options '(auto-insert))

(defcustom sr-start-hook nil
  "List of functions to be called after the Sunrise panes are displayed."
  :group 'sunrise
  :type 'hook
  :options '(auto-insert))

(defcustom sr-refresh-hook nil
  "List of functions to be called every time a pane is refreshed."
  :group 'sunrise
  :type 'hook
  :options '(auto-insert))

(defcustom sr-quit-hook nil
  "List of functions to be called after the Sunrise panes are hidden."
  :group 'sunrise
  :type 'hook
  :options '(auto-insert))

(defvar sr-restore-buffer nil
  "Buffer to restore when Sunrise quits.")

(defvar sr-prior-window-configuration nil
  "Window configuration before Sunrise was started.")

(defvar sr-running nil
  "True when Sunrise commander mode is running.")

(defvar sr-synchronized nil
  "True when synchronized navigation is on")

(defvar sr-current-window-overlay nil
  "Holds the current overlay which marks the current Dired buffer.")

(defvar sr-clex-hotchar-overlay nil
  "Overlay used to highlight the hot character (%) during CLEX operations.")

(defvar sr-left-directory "~/"
  "Dired directory for the left window. See variable `dired-directory'.")

(defvar sr-left-buffer nil
  "Dired buffer for the left window.")

(defvar sr-left-window nil
  "The left window of Dired.")

(defvar sr-right-directory "~/"
  "Dired directory for the right window. See variable `dired-directory'.")

(defvar sr-right-buffer nil
  "Dired buffer for the right window.")

(defvar sr-right-window nil
  "The right window of Dired.")

(defvar sr-current-frame nil
  "The frame Sunrise is active on (if any).")

(defvar sr-this-directory "~/"
  "Dired directory in the active pane.
This isn't necessarily the same as `dired-directory'.")

(defvar sr-other-directory "~/"
  "Dired directory in the passive pane.")

(defvar sr-selected-window 'left
  "The window to select when Sunrise starts up.")

(defvar sr-selected-window-width nil
  "The width the selected window should have on startup.")

(defvar sr-history-registry '((left) (right))
  "Registry of visited directories for both panes.")

(defvar sr-history-stack '((left 0 . 0) (right 0 . 0))
  "History stack counters.
The first counter on each side tracks (by value) the absolute
depth of the stack and (by sign) the direction it is currently
being traversed. The second counter points at the position of the
element that is immediately beneath the top of the stack.")

(defvar sr-ti-openterms nil
  "Stack of currently open terminal buffers.")

(defvar sr-ediff-on nil
  "Flag that indicates whether an `ediff' is being currently done.")

(defvar sr-clex-on nil
  "Flag that indicates that a CLEX operation is taking place.")

(defvar sr-virtual-buffer nil
  "Local flag that indicates the current buffer was originally in
  VIRTUAL mode.")

(defvar sr-dired-directory ""
  "Directory inside which `sr-mode' is currently active.")

(defvar sr-start-message
  "Been coding all night? Enjoy the Sunrise! (or press q to quit)"
  "Message to display when Sunrise is started.")

(defvar sr-panes-height nil
  "Current height of the pane windows.
Initial value is 2/3 the viewport height.")

(defvar sr-current-path-faces nil
  "List of faces to display the path in the current pane (first wins)")
(make-variable-buffer-local 'sr-current-path-faces)

(defvar sr-inhibit-highlight nil
  "Special variable used to temporarily inhibit highlighting in panes.")

(defvar sr-find-items nil
  "Special variable used by `sr-find' to control the scope of find operations.")

(defvar sr-desktop-save-handlers nil
  "List of extension-defined handlers to save Sunrise buffers with desktop.")

(defvar sr-desktop-restore-handlers nil
  "List of extension-defined handlers to restore Sunrise buffers from desktop.")

(defvar sr-backup-buffer nil
  "Variable holding a buffer-local value of the backup buffer.")
(make-variable-buffer-local 'sr-backup-buffer)

(defvar sr-goto-dir-function nil
  "Function to use to navigate to a given directory, or nil to do
the default.  The function receives one argument DIR, which is
the directory to go to.")

(defconst sr-side-lookup (list '(left . right) '(right . left))
  "Trivial alist used by the Sunrise Commander to lookup its own passive side.")

(defface sr-active-path-face
  '((((type tty) (class color) (min-colors 8))
     :background "green" :foreground "yellow" :bold t)
    (((type tty) (class mono)) :inverse-video t)
    (t :background "#ace6ac" :foreground "yellow" :bold t :height 120))
  "Face of the directory path in the active pane."
  :group 'sunrise)

(defface sr-passive-path-face
  '((((type tty) (class color) (min-colors 8) (background dark))
     :background "black" :foreground "cyan")
    (((type tty) (class color) (min-colors 8) (background light))
     :background "white" :foreground "cyan")
    (t :background "white" :foreground "lightgray" :bold t :height 120))
  "Face of the directory path in the passive pane."
  :group 'sunrise)

(defface sr-editing-path-face
  '((t :background "red" :foreground "yellow" :bold t :height 120))
  "Face of the directory path in the active pane while in editable pane mode."
  :group 'sunrise)

(defface sr-highlight-path-face
  '((t :background "yellow" :foreground "#ace6ac" :bold t :height 120))
  "Face of the directory path on mouse hover."
  :group 'sunrise)

(defface sr-clex-hotchar-face
  '((t :foreground "red" :bold t))
  "Face of the hot character (%) in CLEX mode.
Indicates that a CLEX substitution may be about to happen."
  :group 'sunrise)

;;; ============================================================================
;;; This is the core of Sunrise: the main idea is to apply `sr-mode' only inside
;;; Sunrise buffers while keeping all of `dired-mode' untouched.

;;; preserve this variable when switching from `dired-mode' to another mode
(put 'dired-subdir-alist 'permanent-local t)

;;;###autoload
(define-derived-mode sr-mode dired-mode "Sunrise Commander"
  "Two-pane file manager for Emacs based on Dired and inspired by MC.
The following keybindings are available:

        /, j .......... go to directory
        p, n .......... move cursor up/down
        M-p, M-n ...... move cursor up/down in passive pane
        ^, J .......... go to parent directory
        M-^, M-J ...... go to parent directory in passive pane
        Tab ........... switch to other pane
        C-Tab.......... switch to viewer window
        C-c Tab ....... switch to viewer window (console compatible)
        RET, f ........ visit selected file/directory
        M-RET, M-f .... visit selected file/directory in passive pane
        C-c RET ....... visit selected in passive pane (console compatible)
        b ............. visit selected file/directory in default browser
        F ............. visit all marked files, each in its own window
        C-u F ......... visit all marked files in the background
        o,v ........... quick visit selected file (scroll with C-M-v, C-M-S-v)
        C-u o, C-u v .. kill quick-visited buffer (restores normal scrolling)
        X ............. execute selected file
        C-u X.......... execute selected file with arguments

        + ............. create new directory
        M-+ ........... create new empty file(s)
        C ............. copy marked (or current) files and directories
        R ............. rename marked (or current) files and directories
        D ............. delete marked (or current) files and directories
        S ............. soft-link selected file/directory to passive pane
        Y ............. do relative soft-link of selected file in passive pane
        H ............. hard-link selected file to passive pane
        K ............. clone selected files and directories into passive pane
        M-C ........... copy (using traditional dired-do-copy)
        M-R ........... rename (using traditional dired-do-rename)
        M-D ........... delete (using traditional dired-do-delete)
        M-S............ soft-link (using traditional dired-do-symlink)
        M-Y............ do relative soft-link (traditional dired-do-relsymlink)
        M-H............ hard-link selected file/directory (dired-do-hardlink)
        A ............. search marked files for regular expression
        Q ............. perform query-replace-regexp on marked files
        C-c s ......... start a \"sticky\" interactive search in the current pane

        M-a ........... move to beginning of current directory
        M-e ........... move to end of current directory
        M-y ........... go to previous directory in history
        M-u ........... go to next directory in history
        C-M-y ......... go to previous directory in history on passive pane
        C-M-u ......... go to next directory in history on passive pane

        g, C-c C-c .... refresh pane
        s ............. sort entries (by name, number, size, time or extension)
        r ............. reverse the order of entries in the active pane (sticky)
        C-o ........... show/hide hidden files (requires dired-omit-mode)
        C-Backspace ... hide/show file attributes in pane
        C-c Backspace . hide/show file attributes in pane (console compatible)
        y ............. show file type / size of selected files and directories.
        M-l ........... truncate/continue long lines in pane
        C-c v ......... put current panel in VIRTUAL mode
        C-c C-v ....... create new pure VIRTUAL buffer
        C-c C-w ....... browse directory tree using w3m

        M-t ........... transpose panes
        M-o ........... synchronize panes
        C-c C-s ....... change panes layout (vertical/horizontal/top-only)
        [ ............. enlarges the right pane by 5 columns
        ] ............. enlarges the left pane by 5 columns
        } ............. enlarges the panes vertically by 1 row
        C-} ........... enlarges the panes vertically as much as it can
        C-c } ......... enlarges the panes vertically as much as it can
        { ............. shrinks the panes vertically by 1 row
        C-{ ........... shrinks the panes vertically as much as it can
        C-c { ......... shrinks the panes vertically as much as it can
        \\ ............. restores the size of all windows back to «normal»
        C-c C-z ....... enable/disable synchronized navigation

        C-= ........... smart compare files (ediff)
        C-c = ......... smart compare files (console compatible)
        = ............. fast smart compare files (plain diff)
        C-M-= ......... compare panes
        C-x = ......... compare panes (console compatible)

        C-c C-f ....... execute Find-dired in Sunrise VIRTUAL mode
        C-c C-n ....... execute find-Name-dired in Sunrise VIRTUAL mode
        C-c C-g ....... execute find-Grep-dired in Sunrise VIRTUAL mode
        C-u C-c C-g ... execute find-Grep-dired with additional grep options
        C-c C-l ....... execute Locate in Sunrise VIRTUAL mode
        C-c C-r ....... browse list of Recently visited files (requires recentf)
        C-c C-c ....... [after find, locate or recent] dismiss virtual buffer
        C-c / ......... narrow the contents of current pane using fuzzy matching
        C-c b ......... partial Branch view of selected items in current pane
        C-c p ......... Prune paths matching regular expression from current pane
        ; ............. follow file (go to same directory as selected file)
        M-; ........... follow file in passive pane
        C-M-o ......... follow a projection of current directory in passive pane

        C-> ........... save named checkpoint (a.k.a. \"bookmark panes\")
        C-c > ......... save named checkpoint (console compatible)
        C-.    ........ restore named checkpoint
        C-c .  ........ restore named checkpoint

        C-x C-q ....... put pane in Editable Dired mode (commit with C-c C-c)
        @! ............ fast backup files (not dirs!), each to [filename].bak

        C-c t ......... open new terminal or switch to already open one
        C-c T ......... open terminal AND/OR change directory to current
        C-c C-t ....... open always a new terminal in current directory
        C-c M-t ....... open a new terminal using an alternative shell program
        q, C-x k ...... quit Sunrise Commander, restore previous window setup
        M-q ........... quit Sunrise Commander, don't restore previous windows

Additionally, the following traditional commander-style keybindings are provided
\(these may be disabled by customizing the `sr-use-commander-keys' option):

        F2 ............ go to directory
        F3 ............ quick visit selected file
        F4 ............ visit selected file
        F5 ............ copy marked (or current) files and directories
        F6 ............ rename marked (or current) files and directories
        F7 ............ create new directory
        F8 ............ delete marked (or current) files and directories
        F10 ........... quit Sunrise Commander
        C-F3 .......... sort contents of current pane by name
        C-F4 .......... sort contents of current pane by extension
        C-F5 .......... sort contents of current pane by time
        C-F6 .......... sort contents of current pane by size
        C-F7 .......... sort contents of current pane numerically
        S-F7 .......... soft-link selected file/directory to passive pane
        Insert ........ mark file
        C-PgUp ........ go to parent directory

Any other dired keybinding (not overridden by any of the above) can be used in
Sunrise, like G for changing group, M for changing mode and so on.

Some more bindings are available in terminals opened using any of the Sunrise
functions (i.e. one of: C-c t, C-c T, C-c C-t, C-c M-t):

        C-c Tab ....... switch focus to the active pane
        C-c t ......... cycle through all currently open terminals
        C-c T ......... cd to the directory in the active pane
        C-c C-t ....... open new terminal, cd to directory in the active pane
        C-c ; ......... follow the current directory in the active pane
        C-c { ......... shrink the panes vertically as much as possible
        C-c } ......... enlarge the panes vertically as much as possible
        C-c \\ ......... restore the size of all windows back to «normal»
        C-c C-j ....... put terminal in line mode
        C-c C-k ....... put terminal back in char mode

The following bindings are available only in line mode (eshell is considered to
be *always* in line mode):

        M-<up>, M-P ... move cursor up in the active pane
        M-<down>, M-N . move cursor down in the active pane
        M-Return ...... visit selected file/directory in the active pane
        M-J ........... go to parent directory in the active pane
        M-G ........... refresh active pane
        M-Tab ......... switch to passive pane (without leaving the terminal)
        M-M ........... mark selected file/directory in the active pane
        M-Backspace ... unmark previous file/directory in the active pane
        M-U ........... remove all marks from the active pane
        C-Tab ......... switch focus to the active pane

In a terminal in line mode the following substitutions are also performed
automatically:

       %f - expands to the currently selected file in the left pane
       %F - expands to the currently selected file in the right pane
       %m - expands to the list of paths of all marked files in the left pane
       %M - expands to the list of paths of all marked files in the right pane
       %n - expands to the list of names of all marked files in the left pane
       %N - expands to the list of names of all marked files in the right pane
       %d - expands to the current directory in the left pane
       %D - expands to the current directory in the right pane
       %a - expands to the list of paths of all marked files in the active pane
       %A - expands to the current directory in the active pane
       %p - expands to the list of paths of all marked files in the passive pane
       %P - expands to the current directory in the passive pane
       %% - inserts a single % sign.
"
  :group 'sunrise
  (unless (string-match "\\(Sunrise\\)" (buffer-name))
    (rename-buffer (concat (buffer-name) " (Sunrise)") t))
  (set-keymap-parent sr-mode-map dired-mode-map)
  (sr-highlight)
  (dired-omit-mode dired-omit-mode)

  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows (sr-truncate-v t))

  (set (make-local-variable 'buffer-read-only) t)
  (set (make-local-variable 'dired-header-face) 'sr-passive-path-face)
  (set (make-local-variable 'dired-recursive-deletes) 'top)
  (set (make-local-variable 'truncate-lines) nil)
  (set (make-local-variable 'desktop-save-buffer) 'sr-desktop-save-buffer)
  (set (make-local-variable 'revert-buffer-function) 'sr-revert-buffer)
  (set (make-local-variable 'buffer-quit-function) 'sr-quit)
  (set (make-local-variable 'sr-show-file-attributes) sr-show-file-attributes)
  (set (make-local-variable 'hl-line-sticky-flag) nil)
  (hl-line-mode 1)
)

;;;###autoload
(define-derived-mode sr-virtual-mode dired-virtual-mode "Sunrise VIRTUAL"
  "Sunrise Commander Virtual Mode. Useful for reusing find and locate results."
  :group 'sunrise
  (set-keymap-parent sr-virtual-mode-map sr-mode-map)
  (sr-highlight)
  (enriched-mode -1)

  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows (sr-truncate-v t))

  (set (make-local-variable 'buffer-read-only) t)
  (set (make-local-variable 'dired-header-face) 'sr-passive-path-face)
  (set (make-local-variable 'truncate-lines) nil)
  (set (make-local-variable 'desktop-save-buffer) 'sr-desktop-save-buffer)
  (set (make-local-variable 'revert-buffer-function) 'sr-revert-buffer)
  (set (make-local-variable 'buffer-quit-function) 'sr-quit)
  (set (make-local-variable 'sr-show-file-attributes) sr-show-file-attributes)
  (set (make-local-variable 'hl-line-sticky-flag) nil)
  (hl-line-mode 1)

  (define-key sr-virtual-mode-map "\C-c\C-c" 'sr-virtual-dismiss))

(defmacro sr-within (dir form)
  "Evaluate FORM in Sunrise context."
  `(unwind-protect
       (progn
         (setq sr-dired-directory
               (file-name-as-directory (abbreviate-file-name ,dir)))
         (ad-activate 'dired-find-buffer-nocreate)
         ,form)
     (ad-deactivate 'dired-find-buffer-nocreate)
     (setq sr-dired-directory "")))

(defmacro sr-save-aspect (&rest body)
  "Restore omit mode, hidden attributes and point after a directory transition."
  `(let ((inhibit-read-only t)
         (omit (or dired-omit-mode -1))
         (attrs (eval 'sr-show-file-attributes))
         (path-faces sr-current-path-faces))
     ,@body
     (dired-omit-mode omit)
     (if path-faces
         (setq sr-current-path-faces path-faces))
     (if (string= "NUMBER" (get sr-selected-window 'sorting-order))
         (sr-sort-by-operation 'sr-numerical-sort-op))
     (if (get sr-selected-window 'sorting-reverse)
         (sr-reverse-pane))
     (setq sr-show-file-attributes attrs)
     (sr-display-attributes (point-min) (point-max) sr-show-file-attributes)
     (sr-restore-point-if-same-buffer)))

(defmacro sr-alternate-buffer (form)
  "Execute FORM in a new buffer, after killing the previous one."
  `(let ((dispose nil))
     (unless (or (not (or dired-directory (eq major-mode 'sr-tree-mode)))
                 (eq sr-left-buffer sr-right-buffer))
       (setq dispose (current-buffer)))
     ,form
     (setq sr-this-directory default-directory)
     (sr-keep-buffer)
     (sr-highlight)
     (when (and sr-kill-unused-buffers (buffer-live-p dispose))
       (with-current-buffer dispose
         (bury-buffer)
         (set-buffer-modified-p nil)
         (unless (kill-buffer dispose)
           (kill-local-variable 'sr-current-path-faces))))))

(defmacro sr-in-other (form)
  "Execute FORM in the context of the passive pane.
Helper macro for passive & synchronized navigation."
  `(let ((home sr-selected-window))
     (let ((sr-inhibit-highlight t))
       (if sr-synchronized ,form)
       (sr-change-window)
       (condition-case description
           ,form
         (error (message (cadr description)))))
     (if (not sr-running)
         (sr-select-window home)
       (run-hooks 'sr-refresh-hook)
       (sr-change-window))))

(defmacro sr-silently (&rest body)
  "Inhibit calls to `message' in BODY."
  `(cl-letf (((symbol-function 'message) (lambda (_msg &rest _args) (ignore))))
     ,@body))

(eval-and-compile
  (defun sr-symbol (side type)
    "Synthesize Sunrise symbols (`sr-left-buffer', `sr-right-window', etc.)."
    (intern (concat "sr-" (symbol-name side) "-" (symbol-name type)))))

(defun sr-dired-mode ()
  "Set Sunrise mode in every Dired buffer opened in Sunrise (called in a hook)."
  (if (and sr-running
           (eq (selected-frame) sr-current-frame)
           (sr-equal-dirs dired-directory default-directory)
           (not (eq major-mode 'sr-mode)))
      (let ((dired-listing-switches dired-listing-switches)
            (sorting-options (or (get sr-selected-window 'sorting-options) "")))
        (unless (and (featurep 'tramp)
                     (string-match tramp-file-name-regexp default-directory))
          (setq dired-listing-switches
                (concat sr-listing-switches sorting-options)))
        (sr-mode)
        (dired-unadvertise dired-directory))))
(add-hook 'dired-before-readin-hook 'sr-dired-mode)

(defun sr-bookmark-jump ()
  "Handle panes opened from bookmarks in Sunrise."
  (when (and sr-running
             (memq (selected-window) (list sr-left-window sr-right-window)))
    (let ((last-buf (symbol-value (sr-symbol sr-selected-window 'buffer))))
      (setq dired-omit-mode (with-current-buffer last-buf dired-omit-mode))
      (setq sr-this-directory default-directory)
      (if (sr-equal-dirs sr-this-directory sr-other-directory)
          (sr-synchronize-panes t)
        (revert-buffer))
      (sr-keep-buffer)
      (unless (memq last-buf (list (current-buffer) (sr-other 'buffer)))
        (kill-buffer last-buf)))))
(add-hook 'bookmark-after-jump-hook 'sr-bookmark-jump)

(defun sr-virtualize-pane ()
  "Put the current normal view in VIRTUAL mode."
  (interactive)
  (when (eq major-mode 'sr-mode)
    (let ((focus (dired-get-filename 'verbatim t)))
      (sr-save-aspect
       (when (eq sr-left-buffer sr-right-buffer)
         (dired default-directory)
         (sr-keep-buffer))
       (sr-virtual-mode))
      (if focus (sr-focus-filename focus)))))

(defun sr-virtual-dismiss ()
  "Restore normal pane view in Sunrise VIRTUAL mode."
  (interactive)
  (when (eq major-mode 'sr-virtual-mode)
    (let ((focus (dired-get-filename 'verbatim t)))
      (sr-process-kill)
      (sr-save-aspect
       (sr-alternate-buffer (sr-goto-dir sr-this-directory))
       (if focus (sr-focus-filename focus))
       (revert-buffer)))))

(defun sr-select-window (side)
  "Select/highlight the given Sunrise window (right or left)."
  (select-window (symbol-value (sr-symbol side 'window)))
  (setq sr-selected-window side)
  (setq sr-this-directory default-directory)
  (sr-highlight))

(defun sr-viewer-window ()
  "Return an active window that can be used as the viewer."
  (if (or (memq major-mode '(sr-mode sr-virtual-mode sr-tree-mode))
          (memq (current-buffer) (list sr-left-buffer sr-right-buffer)))
      (let ((current-window (selected-window)) (target-window))
        (dotimes (_times 2)
          (setq current-window (next-window current-window))
          (unless (memq current-window (list sr-left-window sr-right-window))
            (setq target-window current-window)))
        target-window)
    (selected-window)))

(defun sr-select-viewer-window (&optional force-setup)
  "Select a window that is not a Sunrise pane.
If no suitable active window can be found and FORCE-SETUP is set,
calls the function `sr-setup-windows' and tries once again."
  (interactive "p")
  (let ((viewer (sr-viewer-window)))
    (if (memq major-mode '(sr-mode sr-virtual-mode sr-tree-mode))
        (hl-line-mode 1))
    (if viewer
        (select-window viewer)
      (when force-setup
        (sr-setup-windows)
        (select-window (sr-viewer-window))))))

(defun sr-backup-buffer ()
  "Create a backup copy of the current buffer.
Used as a cache during revert operations."
  (if (buffer-live-p sr-backup-buffer) (sr-kill-backup-buffer))
  (let ((buf (current-buffer)))
    (setq sr-backup-buffer (generate-new-buffer "*Sunrise Backup*"))
    (with-current-buffer sr-backup-buffer
      (insert-buffer-substring buf))
    (run-hooks 'sr-refresh-hook)))

(defun sr-kill-backup-buffer ()
  "Kill the backup buffer associated to the current one, if there is any."
  (when (buffer-live-p sr-backup-buffer)
    (kill-buffer sr-backup-buffer)
    (setq sr-backup-buffer nil)))
(add-hook 'kill-buffer-hook       'sr-kill-backup-buffer)
(add-hook 'change-major-mode-hook 'sr-kill-backup-buffer)

(add-to-list 'enriched-translations '(invisible (t "x-invisible")))
(defun sr-enrich-buffer ()
  "Activate `enriched-mode' before saving a Sunrise buffer to a file.
This is done so all its dired-filename attributes are kept in the file."
  (if (memq major-mode '(sr-mode sr-virtual-mode))
      (enriched-mode 1)))
(add-hook 'before-save-hook 'sr-enrich-buffer)

(defun sr-extend-with (extension &optional filename)
  "Try to enhance Sunrise with EXTENSION (argument must be a symbol).
An extension can be loaded from optional FILENAME. If found, the extension is
immediately loaded, but only if `sr-autoload-extensions' is not nil."
  (when sr-autoload-extensions
    (require extension filename t)))

(defadvice dired-find-buffer-nocreate
  (before sr-advice-findbuffer (dirname &optional mode))
  "A hack to avoid some Dired mode quirks in the Sunrise Commander."
  (if (sr-equal-dirs sr-dired-directory dirname)
      (setq mode 'sr-mode)))
;; ^--- activated by sr-within macro

(defadvice dired-dwim-target-directory
  (around sr-advice-dwim-target ())
  "Tweak the target directory guessing mechanism when Sunrise Commander is on."
  (if (and sr-running (eq (selected-frame) sr-current-frame))
      (setq ad-return-value sr-other-directory)
    ad-do-it))
(ad-activate 'dired-dwim-target-directory)

(defadvice other-window
  (around sr-advice-other-window (count &optional all-frames))
  "Select the correct Sunrise Commander pane when switching from other windows."
  (if (or (not sr-running) sr-ediff-on)
      ad-do-it
    (let ((from (selected-window)))
      ad-do-it
      (unless (or sr-traditional-other-window
                  (memq from (list sr-left-window sr-right-window)))
        ;; switching from outside
        (sr-select-window sr-selected-window))
      (with-no-warnings
        (when (eq (selected-window) (sr-other 'window))
          ;; switching from the other pane
          (sr-change-window))))))
(ad-activate 'other-window)

(defadvice use-hard-newlines
  (around sr-advice-use-hard-newlines (&optional arg insert))
  "Stop asking if I want hard lines the in Sunrise Commander, just guess."
  (if (memq major-mode '(sr-mode sr-virtual-mode))
      (let ((inhibit-read-only t))
        (setq insert 'guess)
        ad-do-it)
    ad-do-it))
(ad-activate 'use-hard-newlines)

(defadvice dired-insert-set-properties
  (after sr-advice-dired-insert-set-properties (beg end))
  "Manage hidden attributes in files added externally (e.g. from find-dired) to
the Sunrise Commander."
  (when (memq major-mode '(sr-mode sr-virtual-mode))
    (with-no-warnings
      (sr-display-attributes beg end sr-show-file-attributes))))
(ad-activate 'dired-insert-set-properties)

;;; ============================================================================
;;; Sunrise Commander keybindings:

(define-key sr-mode-map "\C-m"        'sr-advertised-find-file)
(define-key sr-mode-map "f"           'sr-advertised-find-file)
(define-key sr-mode-map "X"           'sr-advertised-execute-file)
(define-key sr-mode-map "o"           'sr-quick-view)
(define-key sr-mode-map "v"           'sr-quick-view)
(define-key sr-mode-map "/"           'sr-goto-dir)
(define-key sr-mode-map "j"           'sr-goto-dir)
(define-key sr-mode-map "^"           'sr-dired-prev-subdir)
(define-key sr-mode-map "J"           'sr-dired-prev-subdir)
(define-key sr-mode-map ";"           'sr-follow-file)
(define-key sr-mode-map "\M-t"        'sr-transpose-panes)
(define-key sr-mode-map "\M-o"        'sr-synchronize-panes)
(define-key sr-mode-map "\C-\M-o"     'sr-project-path)
(define-key sr-mode-map "\M-y"        'sr-history-prev)
(define-key sr-mode-map "\M-u"        'sr-history-next)
(define-key sr-mode-map "\C-c>"       'sr-checkpoint-save)
(define-key sr-mode-map "\C-c."       'sr-checkpoint-restore)
(define-key sr-mode-map "\C-c\C-z"    'sr-sync)
(define-key sr-mode-map "\C-c\C-c"    'revert-buffer)

(define-key sr-mode-map "\t"          'sr-change-window)
(define-key sr-mode-map "\C-c\t"      'sr-select-viewer-window)
(define-key sr-mode-map "\M-a"        'sr-beginning-of-buffer)
(define-key sr-mode-map "\M-e"        'sr-end-of-buffer)
(define-key sr-mode-map "\C-c\C-s"    'sr-split-toggle)
(define-key sr-mode-map "]"           'sr-enlarge-left-pane)
(define-key sr-mode-map "["           'sr-enlarge-right-pane)
(define-key sr-mode-map "}"           'sr-enlarge-panes)
(define-key sr-mode-map "{"           'sr-shrink-panes)
(define-key sr-mode-map "\\"          'sr-lock-panes)
(define-key sr-mode-map "\C-c}"       'sr-max-lock-panes)
(define-key sr-mode-map "\C-c{"       'sr-min-lock-panes)
(define-key sr-mode-map "\C-o"        'dired-omit-mode)
(define-key sr-mode-map "b"           'sr-browse-file)
(define-key sr-mode-map "\C-c\C-w"    'sr-browse-pane)
(define-key sr-mode-map "\C-c\d"      'sr-toggle-attributes)
(define-key sr-mode-map "\M-l"        'sr-toggle-truncate-lines)
(define-key sr-mode-map "s"           'sr-interactive-sort)
(define-key sr-mode-map "r"           'sr-reverse-pane)
(define-key sr-mode-map "\C-e"        'sr-scroll-up)
(define-key sr-mode-map "\C-y"        'sr-scroll-down)
(define-key sr-mode-map " "           'sr-scroll-quick-view)
(define-key sr-mode-map "\M- "        'sr-scroll-quick-view-down)
(define-key sr-mode-map [?\S- ]       'sr-scroll-quick-view-down)

(define-key sr-mode-map "C"           'sr-do-copy)
(define-key sr-mode-map "K"           'sr-do-clone)
(define-key sr-mode-map "R"           'sr-do-rename)
(define-key sr-mode-map "D"           'sr-do-delete)
(define-key sr-mode-map "x"           'sr-do-flagged-delete)
(define-key sr-mode-map "S"           'sr-do-symlink)
(define-key sr-mode-map "Y"           'sr-do-relsymlink)
(define-key sr-mode-map "H"           'sr-do-hardlink)
(define-key sr-mode-map "\M-C"        'dired-do-copy)
(define-key sr-mode-map "\M-R"        'dired-do-rename)
(define-key sr-mode-map "\M-D"        'dired-do-delete)
(define-key sr-mode-map "\M-S"        'dired-do-symlink)
(define-key sr-mode-map "\M-Y"        'dired-do-relsymlink)
(define-key sr-mode-map "\M-H"        'dired-do-hardlink)
(define-key sr-mode-map "\C-x\C-q"    'sr-editable-pane)
(define-key sr-mode-map "@"           'sr-fast-backup-files)
(define-key sr-mode-map "\M-+"        'sr-create-files)

(define-key sr-mode-map "="           'sr-diff)
(define-key sr-mode-map "\C-c="       'sr-ediff)
(define-key sr-mode-map "\C-x="       'sr-compare-panes)

(define-key sr-mode-map "\C-c\C-f"    'sr-find)
(define-key sr-mode-map "\C-c\C-n"    'sr-find-name)
(define-key sr-mode-map "\C-c\C-g"    'sr-find-grep)
(define-key sr-mode-map "\C-cb"       'sr-flatten-branch)
(define-key sr-mode-map "\C-cp"       'sr-prune-paths)
(define-key sr-mode-map "\C-c\C-l"    'sr-locate)
(define-key sr-mode-map "\C-c/"       'sr-fuzzy-narrow)
(define-key sr-mode-map "\C-c\C-r"    'sr-recent-files)
(define-key sr-mode-map "\C-c\C-d"    'sr-recent-directories)
(define-key sr-mode-map "\C-cv"       'sr-virtualize-pane)
(define-key sr-mode-map "\C-c\C-v"    'sr-pure-virtual)
(define-key sr-mode-map "Q"           'sr-do-query-replace-regexp)
(define-key sr-mode-map "F"           'sr-do-find-marked-files)
(define-key sr-mode-map "A"           'sr-do-search)
(define-key sr-mode-map "\C-cs"       'sr-sticky-isearch-forward)
(define-key sr-mode-map "\C-cr"       'sr-sticky-isearch-backward)
(define-key sr-mode-map "\C-x\C-f"    'sr-find-file)
(define-key sr-mode-map "y"           'sr-show-files-info)

(define-key sr-mode-map "\M-n"        'sr-next-line-other)
(define-key sr-mode-map [M-down]      'sr-next-line-other)
(define-key sr-mode-map [A-down]      'sr-next-line-other)
(define-key sr-mode-map "\M-p"        'sr-prev-line-other)
(define-key sr-mode-map [M-up]        'sr-prev-line-other)
(define-key sr-mode-map [A-up]        'sr-prev-line-other)
(define-key sr-mode-map "\M-j"        'sr-goto-dir-other)
(define-key sr-mode-map "\M-\C-m"     'sr-advertised-find-file-other)
(define-key sr-mode-map "\M-f"        'sr-advertised-find-file-other)
(define-key sr-mode-map "\C-c\C-m"    'sr-advertised-find-file-other)
(define-key sr-mode-map "\M-^"        'sr-prev-subdir-other)
(define-key sr-mode-map "\M-J"        'sr-prev-subdir-other)
(define-key sr-mode-map "\M-m"        'sr-mark-other)
(define-key sr-mode-map "\M-M"        'sr-unmark-backward-other)
(define-key sr-mode-map "\M-U"        'sr-unmark-all-marks-other)
(define-key sr-mode-map "\M-;"        'sr-follow-file-other)
(define-key sr-mode-map "\C-\M-y"     'sr-history-prev-other)
(define-key sr-mode-map "\C-\M-u"     'sr-history-next-other)

(define-key sr-mode-map "\C-ct"       'sr-term)
(define-key sr-mode-map "\C-cT"       'sr-term-cd)
(define-key sr-mode-map "\C-c\C-t"    'sr-term-cd-newterm)
(define-key sr-mode-map "\C-c\M-t"    'sr-term-cd-program)
(define-key sr-mode-map "\C-c;"       'sr-follow-viewer)
(define-key sr-mode-map "q"           'sr-quit)
(define-key sr-mode-map "\C-xk"       'sr-kill-pane-buffer)
(define-key sr-mode-map "\M-q"        'sunrise-cd)
(define-key sr-mode-map "h"           'sr-describe-mode)
(define-key sr-mode-map "?"           'sr-summary)
(define-key sr-mode-map "k"           'dired-do-kill-lines)
(define-key sr-mode-map [remap undo]  'sr-undo)
(define-key sr-mode-map [remap undo-only] 'sr-undo)
(define-key sr-mode-map [backspace]   'dired-unmark-backward)

(define-key sr-mode-map [mouse-1]     'sr-mouse-advertised-find-file)
(define-key sr-mode-map [mouse-2]     'sr-mouse-change-window)

(define-key sr-mode-map [(control >)]         'sr-checkpoint-save)
(define-key sr-mode-map [(control .)]         'sr-checkpoint-restore)
(define-key sr-mode-map [(control tab)]       'sr-select-viewer-window)
(define-key sr-mode-map [(control backspace)] 'sr-toggle-attributes)
(define-key sr-mode-map [(control ?\=)]       'sr-ediff)
(define-key sr-mode-map [(control meta ?\=)]  'sr-compare-panes)
(define-key sr-mode-map [(control })]         'sr-max-lock-panes)
(define-key sr-mode-map [(control {)]         'sr-min-lock-panes)

(define-key sr-mode-map (kbd "<down-mouse-1>")  'ignore)

(defvar sr-commander-keys
  '(([(f2)]            . sr-goto-dir)
    ([(f3)]            . sr-quick-view)
    ([(f4)]            . sr-advertised-find-file)
    ([(f5)]            . sr-do-copy)
    ([(f6)]            . sr-do-rename)
    ([(f7)]            . dired-create-directory)
    ([(f8)]            . sr-do-delete)
    ([(f10)]           . sr-quit)
    ([(control f3)]    . sr-sort-by-name)
    ([(control f4)]    . sr-sort-by-extension)
    ([(control f5)]    . sr-sort-by-time)
    ([(control f6)]    . sr-sort-by-size)
    ([(control f7)]    . sr-sort-by-number)
    ([(shift f7)]      . sr-do-symlink)
    ([(insert)]        . sr-mark-toggle)
    ([(control prior)] . sr-dired-prev-subdir))
  "Traditional commander-style keybindings for the Sunrise Commander.")

(defcustom sr-use-commander-keys t
  "Whether to use traditional commander-style function keys (F5 = copy, etc)"
  :group 'sunrise
  :type 'boolean
  :set (defun sr-set-commander-keys (symbol value)
         "Setter function for the `sr-use-commander-keys' custom option."
         (if value
             (mapc (lambda (x)
                     (define-key sr-mode-map (car x) (cdr x))) sr-commander-keys)
           (mapc (lambda (x)
                   (define-key sr-mode-map (car x) nil)) sr-commander-keys))
         (set-default symbol value)))

;;; ============================================================================
;;; Initialization and finalization functions:

;;;###autoload
(defun sunrise (&optional left-directory right-directory filename)
  "Toggle the Sunrise Commander file manager.
If LEFT-DIRECTORY is given, the left window will display that
directory (same for RIGHT-DIRECTORY). Specifying nil for any of
these values uses the default, ie. $HOME."
  (interactive)
  (message "Starting Sunrise Commander...")

  (if (not sr-running)
      (let ((welcome sr-start-message))
        (if left-directory
            (setq sr-left-directory left-directory))
        (if right-directory
            (setq sr-right-directory right-directory))

        (sr-switch-to-nonpane-buffer)
        (setq sr-restore-buffer (current-buffer)
              sr-current-frame (window-frame (selected-window))
              sr-prior-window-configuration (current-window-configuration)
              sr-running t)
        (sr-setup-windows)
        (if filename
            (condition-case description
                (sr-focus-filename (file-name-nondirectory filename))
              (error (setq welcome (cadr description)))))
        (setq sr-this-directory default-directory)
        (message "%s" welcome)
        (sr-highlight) ;;<-- W32Emacs needs this
        (hl-line-mode 1))
    (let ((my-frame (window-frame (selected-window))))
      (sr-quit)
      (message "All life leaps out to greet the light...")
      (unless (eq my-frame (window-frame (selected-window)))
        (select-frame my-frame)
        (sunrise left-directory right-directory filename)))))
 
;;;###autoload
(defun sr-dired (&optional target switches)
  "Visit the given target (file or directory) in `sr-mode'."
  (interactive
   (list
    (read-file-name "Visit (file or directory): " nil nil nil)))
  (let* ((target (expand-file-name (or target default-directory)))
         (file (if (file-directory-p target) nil target))
         (directory (if file (file-name-directory target) target))
         (dired-omit-mode (if sr-show-hidden-files -1 1))
         (sr-listing-switches (or switches sr-listing-switches)))
    (unless (file-readable-p directory) 
      (error "%s is not readable!" (sr-directory-name-proper directory)))
    (unless (and sr-running (eq (selected-frame) sr-current-frame)) (sunrise))
    (sr-select-window sr-selected-window)
    (if file
        (sr-follow-file file)
      (sr-goto-dir directory))
    (hl-line-mode 1)
    (sr-display-attributes (point-min) (point-max) sr-show-file-attributes)
    (sr-this 'buffer)))

(defun sr-choose-cd-target ()
  "Select a suitable target directory for cd operations."
  (if (and sr-running (eq (selected-frame) sr-current-frame))
      sr-this-directory
    default-directory))

;;;###autoload
(defun sunrise-cd ()
  "Toggle the Sunrise Commander FM keeping the current file in focus.
If Sunrise is off, enable it and focus the file displayed in the current buffer.
If Sunrise is on, disable it and switch to the buffer currently displayed in the
viewer window."
  (interactive)
  (if (not (and sr-running
                (eq (window-frame sr-left-window) (selected-frame))))
      (sr-dired (or (buffer-file-name) (sr-choose-cd-target)))
    (sr-quit t)
    (message "Hast thou a charm to stay the morning-star in his steep course?")))

(defun sr-this (&optional type)
  "Return object of type TYPE corresponding to the active side of the manager.
If TYPE is not specified (nil), returns a symbol (`left' or `right').
If TYPE is `buffer' or `window', returns the corresponding buffer
or window."
  (if type
      (symbol-value (sr-symbol sr-selected-window type))
    sr-selected-window))

(defun sr-other (&optional type)
  "Return object of type TYPE corresponding to the passive side of the manager.
If TYPE is not specified (nil), returns a symbol (`left' or `right').
If TYPE is `buffer' or `window', returns the corresponding
buffer or window."
  (let ((side (cdr (assq sr-selected-window sr-side-lookup))))
    (if type
        (symbol-value (sr-symbol side type))
      side)))

;;; ============================================================================
;;; Window management functions:

(defmacro sr-setup-pane (side)
  "Helper macro for the function `sr-setup-windows'."
  `(let ((sr-selected-window ',side))
     (setq ,(sr-symbol side 'window) (selected-window))
     (if (buffer-live-p ,(sr-symbol side 'buffer))
         (progn
           (switch-to-buffer ,(sr-symbol side 'buffer))
           (setq ,(sr-symbol side 'directory) default-directory))
       (sr-dired ,(sr-symbol side 'directory)))))

(defun sr-setup-visible-panes ()
  "Set up sunrise on all visible panes."
  (sr-setup-pane left)
  (unless (eq sr-window-split-style 'top)
    (other-window 1)
    (sr-setup-pane right)))

(defun sr-setup-windows()
  "Set up the Sunrise window configuration (two windows in `sr-mode')."
  (run-hooks 'sr-init-hook)
  ;;get rid of all windows except one (not any of the panes!)
  (sr-select-viewer-window)
  (delete-other-windows)
  (if (buffer-live-p other-window-scroll-buffer)
      (switch-to-buffer other-window-scroll-buffer)
    (sr-switch-to-nonpane-buffer))

  ;;now create the viewer window
  (unless (and sr-panes-height (< sr-panes-height (frame-height)))
    (setq sr-panes-height (sr-get-panes-size)))
  (if (and (<= sr-panes-height (* 2 window-min-height))
           (eq sr-window-split-style 'vertical))
      (setq sr-panes-height (* 2 window-min-height)))
  (split-window (selected-window) sr-panes-height)

  (case sr-window-split-style
    (horizontal (split-window-horizontally))
    (vertical   (split-window-vertically))
    (top        (ignore))
    (t (error "Unrecognised `sr-window-split-style' value: %s"
              sr-window-split-style)))

  (sr-setup-visible-panes)

  ;;select the correct window
  (sr-select-window sr-selected-window)
  (sr-restore-panes-width)
  (run-hooks 'sr-start-hook))

(defun sr-switch-to-nonpane-buffer ()
  "Try to switch to a buffer that is *not* a Sunrise pane."
  (let ((start (current-buffer)))
    (while (and
              start
              (or (memq major-mode '(sr-mode sr-virtual-mode sr-tree-mode))
                  (memq (current-buffer) (list sr-left-buffer sr-right-buffer))))
        (bury-buffer)
        (if (eq start (current-buffer)) (setq start nil)))))

(defun sr-restore-prior-configuration ()
  "Restore the configuration stored in `sr-prior-window-configuration' if any."
  (set-window-configuration sr-prior-window-configuration)
  (if (buffer-live-p sr-restore-buffer)
      (set-buffer sr-restore-buffer)))

(defun sr-lock-window (_frame)
  "Resize the left Sunrise pane to have the \"right\" size."
  (when sr-running
    (if (not (window-live-p sr-left-window))
        (setq sr-running nil)
      (let ((sr-windows-locked sr-windows-locked))
        (when (> window-min-height (- (frame-height)
                                      (window-height sr-left-window)))
          (setq sr-windows-locked nil))
        (and sr-windows-locked
             (not sr-ediff-on)
             (not (eq sr-window-split-style 'vertical))
             (window-live-p sr-left-window)
             (save-selected-window
               (select-window sr-left-window)
               (let ((my-delta (- sr-panes-height (window-height))))
                 (enlarge-window my-delta))
               (scroll-right)
               (when (window-live-p sr-right-window)
                 (select-window sr-right-window)
                 (scroll-right))))))))

;; This keeps the size of the Sunrise panes constant:
(add-hook 'window-size-change-functions 'sr-lock-window)

(defun sr-highlight(&optional face)
  "Set up the path line in the current buffer.
With optional FACE, register this face as the current face to display the active
path line."
  (when (and (memq major-mode '(sr-mode sr-virtual-mode sr-tree-mode))
             (not sr-inhibit-highlight))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (sr-hide-avfs-root)
        (sr-highlight-broken-links)
        (sr-graphical-highlight face)
        (sr-force-passive-highlight)
        (run-hooks 'sr-refresh-hook)))))

(defun sr-unhighlight (face)
  "Remove FACE from the list of faces of the active path line."
  (when face
    (setq sr-current-path-faces (delq face sr-current-path-faces))
    (overlay-put sr-current-window-overlay 'face
                 (or (car sr-current-path-faces) 'sr-active-path-face))))

(defun sr-hide-avfs-root ()
  "Hide the AVFS virtual filesystem root (if any) on the path line."
  (if sr-avfs-root
      (let ((start nil) (end nil)
            (next (search-forward sr-avfs-root (point-at-eol) t)))
        (if next (setq start (- next (length sr-avfs-root))))
        (while next
          (setq end (point)
                next (search-forward sr-avfs-root (point-at-eol) t)))
        (when end
          (add-text-properties start end '(invisible t))))))

(defun sr-highlight-broken-links ()
  "Mark broken symlinks with an exclamation mark."
  (let ((dired-marker-char ?!))
    (while (search-forward-regexp dired-re-sym nil t)
      (unless (or (not (eq 32 (char-after (line-beginning-position))))
                  (file-exists-p (dired-get-filename)))
        (dired-mark 1)))))

(defsubst sr-invalid-overlayp ()
  "Test for invalidity of the current buffer's graphical path line overlay.
Returns t if the overlay is no longer valid and should be replaced."
  (or (not (overlayp sr-current-window-overlay))
      (eq (overlay-start sr-current-window-overlay)
          (overlay-end sr-current-window-overlay))))

(defun sr-graphical-highlight (&optional face)
  "Set up the graphical path line in the current buffer.
\(Fancy fonts and clickable path.)"
  (let ((begin) (end) (inhibit-read-only t))

    (when (sr-invalid-overlayp)
      ;;determine begining and end
      (save-excursion
        (goto-char (point-min))
        (search-forward-regexp "\\S " nil t)
        (setq begin (1- (point)))
        (end-of-line)
        (setq end (1- (point))))

      ;;build overlay
      (when sr-current-window-overlay
        (delete-overlay sr-current-window-overlay))
      (set (make-local-variable 'sr-current-window-overlay)
           (make-overlay begin end))

      ;;path line hover effect:
      (add-text-properties
       begin
       end
       '(mouse-face sr-highlight-path-face
                    help-echo "click to move up")
       nil))
    (when face
      (setq sr-current-path-faces (cons face sr-current-path-faces)))
    (overlay-put sr-current-window-overlay 'face
                 (or (car sr-current-path-faces) 'sr-active-path-face))
    (overlay-put sr-current-window-overlay 'window (selected-window))))

(defun sr-force-passive-highlight (&optional revert)
  "Set up the graphical path line in the passive pane.
With optional argument REVERT, executes `revert-buffer' on the passive buffer."
    (unless (or (not (buffer-live-p (sr-other 'buffer)))
                (eq sr-left-buffer sr-right-buffer))
      (with-current-buffer (sr-other 'buffer)
        (when sr-current-window-overlay
          (delete-overlay sr-current-window-overlay))
        (when (and revert
                   (memq major-mode '(sr-mode sr-virtual-mode sr-tree-mode)))
          (revert-buffer)))))

(defun sr-quit (&optional norestore)
  "Quit Sunrise and restore Emacs to the previous state."
  (interactive)
  (if sr-running
      (progn
        (setq sr-running nil)
        (sr-save-directories)
        (sr-save-panes-width)
        (if norestore
            (progn
              (sr-select-viewer-window)
              (delete-other-windows))
          (sr-restore-prior-configuration))
        (sr-bury-panes)
        (setq buffer-read-only nil)
        (run-hooks 'sr-quit-hook)
        (setq sr-current-frame nil))
    (bury-buffer)))

(add-hook 'delete-frame-functions
          (lambda (frame)
            (if (and sr-running (eq frame sr-current-frame)) (sr-quit))))

(defun sr-save-directories ()
  "Save current directories in the panes to use them at the next startup."
  (when (window-live-p sr-left-window)
    (set-buffer (window-buffer sr-left-window))
    (when (memq major-mode '(sr-mode sr-tree-mode))
      (setq sr-left-directory default-directory)
      (setq sr-left-buffer (current-buffer))))

  (when (window-live-p sr-right-window)
    (set-buffer (window-buffer sr-right-window))
    (when (memq major-mode '(sr-mode sr-tree-mode))
      (setq sr-right-directory default-directory)
      (setq sr-right-buffer (current-buffer)))))

(defun sr-bury-panes ()
  "Send both pane buffers to the end of the `buffer-list'."
  (mapc (lambda (x)
          (bury-buffer (symbol-value (sr-symbol x 'buffer))))
        '(left right)))

(defun sr-save-panes-width ()
  "Save the width of the panes to use them at the next startup."
  (unless sr-selected-window-width
    (if (and (window-live-p sr-left-window)
             (window-live-p sr-right-window))
        (setq sr-selected-window-width
              (window-width
               (symbol-value (sr-symbol sr-selected-window 'window))))
      (setq sr-selected-window-width t))))

(defun sr-restore-panes-width ()
  "Restore the last registered pane width."
  (when (and (eq sr-window-split-style 'horizontal)
             (numberp sr-selected-window-width))
    (enlarge-window-horizontally
     (min (- sr-selected-window-width (window-width))
          (- (frame-width) (window-width) window-min-width)))))

(defun sr-resize-panes (&optional reverse)
  "Enlarge (or shrink, if REVERSE is t) the left pane by 5 columns."
  (when (and (window-live-p sr-left-window)
             (window-live-p sr-right-window))
    (let ((direction (or (and reverse -1) 1)))
      (save-selected-window
        (select-window sr-left-window)
        (enlarge-window-horizontally (* 5 direction))))
    (setq sr-selected-window-width nil)))

(defun sr-enlarge-left-pane ()
  "Enlarge the left pane by 5 columns."
  (interactive)
  (when (< (1+ window-min-width) (window-width sr-right-window))
      (sr-resize-panes)
      (sr-save-panes-width)))

(defun sr-enlarge-right-pane ()
  "Enlarge the right pane by 5 columns."
  (interactive)
  (when (< (1+ window-min-width) (window-width sr-left-window))
      (sr-resize-panes t)
      (sr-save-panes-width)))

(defun sr-get-panes-size (&optional size)
  "Tell what the maximal, minimal and normal pane sizes should be."
  (let ((frame (frame-height)))
    (case size
      (max (max (- frame window-min-height 1) 5))
      (min (min (1+ window-min-height) 5))
      (t  (/ (* sr-windows-default-ratio (frame-height)) 100)))))

(defun sr-enlarge-panes ()
  "Enlarge both panes vertically."
  (interactive)
  (let ((sr-windows-locked nil)
        (max (sr-get-panes-size 'max))
        (ratio 1)
        delta)
    (save-selected-window
      (when (eq sr-window-split-style 'vertical)
        (select-window sr-right-window)
        (setq ratio 2)
        (setq delta (- max (window-height)))
        (if (> (/ max ratio) (window-height))
            (shrink-window (if (< 2 delta) -2 -1))))
      (select-window sr-left-window)
      (if (> (/ max ratio) (window-height))
          (shrink-window -1))
      (setq sr-panes-height (* (window-height) ratio)))))

(defun sr-shrink-panes ()
  "Shink both panes vertically."
  (interactive)
  (let ((sr-windows-locked nil)
        (min (sr-get-panes-size 'min))
        (ratio 1)
        delta)
    (save-selected-window
      (when (eq sr-window-split-style 'vertical)
        (select-window sr-right-window)
        (setq ratio 2)
        (setq delta (- (window-height) min))
        (if (< min (window-height))
            (shrink-window (if (< 2 delta) 2 1))))
      (select-window sr-left-window)
      (if (< min (window-height))
          (shrink-window 1))
      (setq sr-panes-height (* (window-height) ratio)))))

(defun sr-lock-panes (&optional height)
  "Resize and lock the panes at some vertical position.
The optional argument determines the height to lock the panes at.
Valid values are `min' and `max'; given any other value, locks
the panes at normal position."
  (interactive)
  (if sr-running
    (if (not (and (window-live-p sr-left-window)
                  (or (window-live-p sr-right-window)
                      (eq sr-window-split-style 'top))))
        (sr-setup-windows)
      (setq sr-panes-height (sr-get-panes-size height))
      (let ((locked sr-windows-locked))
        (setq sr-windows-locked t)
        (if height
            (shrink-window 1)
          (setq sr-selected-window-width t)
          (balance-windows))
        (unless locked
          (sit-for 0.1)
          (setq sr-windows-locked nil))))
    (sunrise)))

(defun sr-max-lock-panes ()
  (interactive)
  (sr-save-panes-width)
  (sr-lock-panes 'max))

(defun sr-min-lock-panes ()
  (interactive)
  (sr-save-panes-width)
  (sr-lock-panes 'min))

;;; ============================================================================
;;; File system navigation functions:

(defun sr-advertised-find-file (&optional filename)
  "Handle accesses to file system objects through the user interface.
Includes cases when the user presses return, f or clicks on the path line."
  (interactive)
  (unless filename
    (if (eq 1 (line-number-at-pos)) ;; <- Click or Enter on path line.
        (let* ((path (buffer-substring (point) (point-at-eol)))
               (levels (1- (length (split-string path "/")))))
          (if (< 0 levels)
              (sr-dired-prev-subdir levels)
            (sr-beginning-of-buffer)))
      (setq filename (dired-get-filename nil t)
            filename (and filename (expand-file-name filename)))))
  (if filename
      (if (file-exists-p filename)
          (sr-find-file filename)
        (error "Sunrise: nonexistent target"))))

(defun sr-advertised-execute-file (&optional prefix)
  "Execute the currently selected file in a new subprocess."
  (interactive "P")
  (let ((path (dired-get-filename nil t)) (label) (args))
    (if path
        (setq label  (file-name-nondirectory path))
      (error "Sunrise: no executable file on this line"))
    (unless (and (not (file-directory-p path)) (file-executable-p path))
      (error "Sunrise: \"%s\" is not an executable file" label))
    (when prefix
      (setq args (read-string (format "arguments for \"%s\": " label))
            label (format "%s %s" label args)))
    (message "Sunrise: executing \"%s\" in new process" label)
    (if args
        (apply #'start-process (append (list "Sunrise Subprocess" nil path)
                                       (split-string args)))
      (start-process "Sunrise Subprocess" nil path))))

(defun sr-find-file (filename &optional wildcards)
  "Determine the proper way of handling an object in the file system.
FILENAME can be either a regular file, a regular directory, a
Sunrise VIRTUAL directory, or a virtual directory served by
AVFS."
  (interactive (find-file-read-args "Find file or directory: " nil))
  (cond ((file-directory-p filename) (sr-find-regular-directory filename))
        ((and (sr-avfs-directory-p filename) (sr-avfs-dir filename))
         (sr-find-regular-directory (sr-avfs-dir filename)))
        ((sr-virtual-directory-p filename) (sr-find-virtual-directory filename))
        (t (sr-find-regular-file filename wildcards))))

(defun sr-virtual-directory-p (filename)
  "Tell whether FILENAME is the path to a Sunrise VIRTUAL directory."
  (eq 'sr-virtual-mode (assoc-default filename auto-mode-alist 'string-match)))

(defun sr-avfs-directory-p (filename)
  "Tell whether FILENAME can be seen as the root of an AVFS virtual directory."
  (let ((mode (assoc-default filename auto-mode-alist 'string-match)))
    (and sr-avfs-root
         (or (eq 'archive-mode mode)
             (eq 'tar-mode mode)
             (and (listp mode) (eq 'jka-compr (cadr mode)))
             (not (equal "." (sr-assoc-key filename
                                           sr-avfs-handlers-alist
                                           'string-match)))))))

(defun sr-find-regular-directory (directory)
  "Visit the given regular directory in the active pane."
  (setq directory (file-name-as-directory directory))
  (let ((parent (expand-file-name "../")))
    (if (and (not (sr-equal-dirs parent default-directory))
             (sr-equal-dirs directory parent))
        (sr-dired-prev-subdir)
      (sr-goto-dir directory))))

(defun sr-find-virtual-directory (sr-virtual-dir)
  "Visit the given Sunrise VIRTUAL directory in the active pane."
  (sr-save-aspect
   (sr-alternate-buffer (find-file sr-virtual-dir)))
  (sr-history-push sr-virtual-dir)
  (set-visited-file-name nil t)
  (sr-keep-buffer)
  (sr-backup-buffer))

(defun sr-find-regular-file (filename &optional wildcards)
  "Deactivate Sunrise and visit FILENAME as a regular file with WILDCARDS.
\(See `find-file' for more details on wildcard expansion.)"
  (condition-case description
      (let ((buff (find-file-noselect filename nil nil wildcards)))
        (sr-save-panes-width)
        (sr-quit)
        (set-window-configuration sr-prior-window-configuration)
        (switch-to-buffer buff))
    (error (message "%s" (cadr description)))))

(defun sr-avfs-dir (filename)
  "Return the virtual path for accessing FILENAME through AVFS.
Returns nil if AVFS cannot manage this kind of file."
  (let* ((handler (assoc-default filename sr-avfs-handlers-alist 'string-match))
         (vdir (concat filename handler)))
    (unless (sr-overlapping-paths-p sr-avfs-root vdir)
      (setq vdir (concat sr-avfs-root vdir)))
    (if (file-attributes vdir) vdir nil)))

(defun sr-goto-dir (dir)
  "Change the current directory in the active pane to the given one."
  (interactive "DChange directory (file or pattern): ")
  (if sr-goto-dir-function
      (funcall sr-goto-dir-function dir)
    (unless (and (eq major-mode 'sr-mode) (sr-equal-dirs dir default-directory))
      (if (and sr-avfs-root
               (null (posix-string-match "#" dir)))
          (setq dir (replace-regexp-in-string
                     (expand-file-name sr-avfs-root) "" dir)))
      (sr-save-aspect
       (sr-within dir (sr-alternate-buffer (dired dir))))
      (sr-history-push default-directory)
      (sr-beginning-of-buffer))))

(defun sr-dired-prev-subdir (&optional count)
  "Go to the parent directory, or COUNT subdirectories upwards."
  (interactive "P")
  (unless (sr-equal-dirs default-directory "/")
    (let* ((count (or count 1))
           (to (replace-regexp-in-string "x" "../" (make-string count ?x)))
           (from (expand-file-name (substring to 1)))
           (from (sr-directory-name-proper from))
           (from (replace-regexp-in-string "\\(?:#.*/?$\\|/$\\)" "" from))
           (to (replace-regexp-in-string "\\.\\./$" "" (expand-file-name to))))
      (sr-goto-dir to)
      (unless (sr-equal-dirs from to)
        (sr-focus-filename from)))))

(defun sr-follow-file (&optional target-path)
  "Go to the same directory where the selected file is.
Very useful inside Sunrise VIRTUAL buffers."
  (interactive)
  (if (null target-path)
      (setq target-path (dired-get-filename nil t)))

  (let ((target-dir (file-name-directory target-path))
        (target-symlink (file-symlink-p target-path))
        (target-file))

    ;; if the target is a symlink and there's nothing more interesting to do
    ;; then follow the symlink:
    (when (and target-symlink
               (string= target-dir (dired-current-directory))
               (not (eq major-mode 'sr-virtual-mode)))
      (unless (file-exists-p target-symlink)
        (error "Sunrise: file is a symlink to a nonexistent target"))
      (setq target-path target-symlink)
      (setq target-dir (file-name-directory target-symlink)))

    (setq target-file (file-name-nondirectory target-path))

    (when target-dir ;; <-- nil in symlinks to other files in same directory:
      (setq target-dir (sr-chop ?/ target-dir))
      (sr-goto-dir target-dir))
    (sr-focus-filename target-file)))

(defun sr-follow-viewer ()
  "Go to the directory of the file displayed in the viewer window."
  (interactive)
  (when sr-running
    (let* ((viewer (sr-viewer-window))
           (viewer-buffer (if viewer (window-buffer viewer)))
           (target-dir) (target-file))
      (when viewer-buffer
        (with-current-buffer viewer-buffer
          (setq target-dir default-directory
                target-file (sr-directory-name-proper (buffer-file-name)))))
      (sr-select-window sr-selected-window)
      (if target-dir (sr-goto-dir target-dir))
      (if target-file (sr-focus-filename target-file)))))

(defun sr-project-path ()
  "Find projections of the active directory over the passive one.

Locates interactively all descendants of the directory in the passive pane that
have a path similar to the directory in the active pane.

For instance, if the active pane is displaying directory /a/b/c and the passive
one is displaying /x/y, this command will check for the existence of any of the
following: /x/y/a/b/c, /x/y/b/c, /x/y/c and /x/y. Each (existing) directory
located according to this schema will be known hereafter as a 'projection of the
directory /a/b/c over /x/y'.

If many projections of the active directory over the passive one exist, one can
rotate among all of them by invoking `sr-project-path' repeatedly : they will be
visited in order, from longest path to shortest."

  (interactive)
  (let* ((sr-synchronized nil)
         (path (sr-chop ?/ (expand-file-name (dired-current-directory))))
         (pos (if (< 0 (length path)) 1)) (candidate) (next-key))
    (while pos
      (setq candidate (concat sr-other-directory (substring path pos))
            pos (string-match "/" path (1+ pos))
            pos (if pos (1+ pos)))
      (when (and (file-directory-p candidate)
                 (not (sr-equal-dirs sr-this-directory candidate)))
        (sr-goto-dir-other candidate)
        (setq next-key (read-key-sequence "(press C-M-o again for more)"))
        (if (eq (lookup-key sr-mode-map next-key) 'sr-project-path)
            (sr-history-prev-other)
          (setq unread-command-events (listify-key-sequence next-key)
                pos nil))))
    (unless next-key
      (message "Sunrise: sorry, no suitable projections found"))))

(defun sr-history-push (element)
  "Push a new path into the history stack of the current pane."
  (unless (or (null element)
              (and (featurep 'tramp)
                   (string-match tramp-file-name-regexp element)))
    (let* ((pane (assoc sr-selected-window sr-history-registry))
           (hist (cdr pane))
           (len (length hist)))
      (if (>= len sr-history-length)
          (nbutlast hist (- len sr-history-length)))
      (setq element (abbreviate-file-name (sr-chop ?/ element))
            hist (delete element hist))
      (push element hist)
      (setcdr pane hist))
    (sr-history-stack-reset)))

(defun sr-history-next ()
  "Navigate forward in the history of the active pane."
  (interactive)
  (let ((side (assoc sr-selected-window sr-history-stack)))
    (unless (zerop (cadr side))
      (sr-history-move -1))
    (when (zerop (cadr side))
      (sr-history-stack-reset))))

(defun sr-history-prev ()
  "Navigate backwards in the history of the active pane."
  (interactive)
  (let ((history (cdr (assoc sr-selected-window sr-history-registry)))
        (stack (cdr (assoc sr-selected-window sr-history-stack))))
    (when (< (abs (cdr stack)) (1- (length history)))
      (sr-history-move 1))))

(defun sr-history-move (step)
  "Traverse the history of the active pane in a stack-like fashion.
This function re-arranges the history list of the current pane so as to make it
simulate a stack of directories, from which one can 'pop' the current directory
and 'push' it back, keeping the most recently visited entries always near the
top of the stack."
  (let* ((side (assoc sr-selected-window sr-history-stack))
         (depth (cadr side)) (goal) (target-dir))
    (when (> 0 (* step depth))
      (sr-history-stack-reset))
    (setq goal  (1+ (cddr side))
          depth (* step (+ (abs depth) step))
          target-dir (sr-history-pick goal))
    (when target-dir
      (sr-goto-dir target-dir)
      (setcdr side (cons depth goal)))))

(defun sr-history-stack-reset ()
  "Reset the current history stack counter."
  (let ((side (assoc sr-selected-window sr-history-stack)))
    (setcdr side '(0 . 0))))

(defun sr-history-pick (position)
  "Return directory at POSITION in current history.
If the entry was removed or made inaccessible since our last visit, remove it
from the history list and check among the previous ones until an accessible
directory is found, or the list runs out of entries."
  (let* ((history (cdr (assoc sr-selected-window sr-history-registry)))
         (target (nth position history)))
    (while (and target (not (file-accessible-directory-p target)))
      (delete target history)
      (setq target (nth position history)))
    target))

(defun sr-require-checkpoints-extension (&optional noerror)
  "Bootstrap code for checkpoint support.
Just tries to require the appropriate checkpoints extension
depending on the version of bookmark.el being used."
  (require 'bookmark nil t)
  (let* ((feature
          (cond ((fboundp 'bookmark-make-record) 'sunrise-x-checkpoints)
                (t 'sunrise-x-old-checkpoints)))
         (name (symbol-name feature)))
    (or
     (not (featurep 'sunrise-commander))
     (require feature nil t)
     noerror
     (error "Feature `%s' not found!\
For checkpoints to work, download http://joseito.republika.pl/%s.el.gz\
and add it to your `load-path'" name name))))

(defmacro sr-checkpoint-command (function-name)
  `(defun ,function-name (&optional arg)
     (interactive)
     (sr-require-checkpoints-extension)
     (if (commandp #',function-name)
         (call-interactively #',function-name)
       (funcall #',function-name arg))))
(sr-checkpoint-command sr-checkpoint-save)
(sr-checkpoint-command sr-checkpoint-restore)
(sr-checkpoint-command sr-checkpoint-handler)
;;;###autoload (autoload 'sr-checkpoint-handler "sunrise-commander" "" t)

(defun sr-do-find-marked-files (&optional noselect)
  "Sunrise replacement for `dired-do-find-marked-files'."
  (interactive "P")
  (let* ((files (delq nil (mapcar (lambda (x)
                                    (and (file-regular-p x) x))
                                  (dired-get-marked-files)))))
    (unless files
      (error "Sunrise: no regular files to open"))
    (unless noselect (sr-quit))
    (dired-simultaneous-find-file files noselect)))

;;; ============================================================================
;;; Graphical interface interaction functions:

(defun sr-change-window()
  "Change to the other Sunrise pane."
  (interactive)
  (if (and (window-live-p sr-left-window) (window-live-p sr-right-window))
      (let ((here sr-this-directory))
        (setq sr-this-directory sr-other-directory)
        (setq sr-other-directory here)
        (sr-select-window (sr-other)))))

(defun sr-mouse-change-window (e)
  "Change to the Sunrise pane clicked in by the mouse."
  (interactive "e")
  (mouse-set-point e)
  (if (eq (selected-window) (sr-other 'window))
      (sr-change-window)))

(defun sr-beginning-of-buffer()
  "Go to the first directory/file in Dired."
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward directory-listing-before-filename-regexp nil t)
    (dotimes (_times 2)
      (when (looking-at "\.\.?/?$")
        (dired-next-line 1)))))

(defun sr-end-of-buffer()
  "Go to the last directory/file in Dired."
  (interactive)
  (goto-char (point-max))
  (re-search-backward directory-listing-before-filename-regexp)
  (dired-next-line 0))

(defun sr-focus-filename (filename)
  "Try to select FILENAME in the current buffer."
  (if (and dired-omit-mode
           (string-match (dired-omit-regexp) filename))
      (dired-omit-mode -1))
  (let ((sr-inhibit-highlight t)
        (expr (sr-chop ?/ filename)))
    (cond ((file-symlink-p filename)
           (setq expr (concat (regexp-quote expr) " ->")))
          ((file-directory-p filename)
           (setq expr (concat (regexp-quote expr) "\\(?:/\\|$\\)")))
          ((file-regular-p filename)
           (setq expr (concat (regexp-quote expr) "$"))))
    (setq expr (concat "[0-9] +" expr))
    (beginning-of-line)
    (unless (re-search-forward expr nil t)
      (re-search-backward expr nil t)))
  (beginning-of-line)
  (re-search-forward directory-listing-before-filename-regexp nil t))

(defun sr-split-toggle()
  "Change Sunrise window layout from horizontal to vertical to top and so on."
  (interactive)
  (case sr-window-split-style
    (horizontal (sr-split-setup 'vertical))
    (vertical (sr-split-setup 'top))
    (top (progn
           (sr-split-setup 'horizontal)
           (sr-in-other (revert-buffer))))
    (t (sr-split-setup 'horizontal))))

(defun sr-split-setup(split-type)
  (setq sr-window-split-style split-type)
  (when sr-running
    (when (eq sr-window-split-style 'top)
      (sr-select-window 'left)
      (delete-window sr-right-window)
      (setq sr-panes-height (window-height)))
    (sr-setup-windows))
  (message "Sunrise: split style changed to \"%s\"" (symbol-name split-type)))

(defun sr-transpose-panes ()
  "Change the order of the panes."
  (interactive)
  (unless (eq sr-left-buffer sr-right-buffer)
    (mapc (lambda (x)
            (let ((left (sr-symbol 'left x)) (right (sr-symbol 'right x)) (tmp))
              (setq tmp (symbol-value left))
              (set left (symbol-value right))
              (set right tmp)))
          '(directory buffer window))
    (let ((tmp sr-this-directory))
      (setq sr-this-directory sr-other-directory
            sr-other-directory tmp))
    (select-window sr-right-window)
    (sr-setup-visible-panes)
    (sr-select-window sr-selected-window)))

(defun sr-synchronize-panes (&optional reverse)
  "Change the directory in the other pane to that in the current one.
If the optional parameter REVERSE is non-nil, performs the
opposite operation, ie. changes the directory in the current pane
to that in the other one."
  (interactive "P")
  (let ((target (current-buffer)) (sr-inhibit-highlight t))
    (sr-change-window)
    (if reverse
        (setq target (current-buffer))
      (sr-alternate-buffer (switch-to-buffer target))
      (sr-history-push default-directory))
    (sr-change-window)
    (when reverse
      (sr-alternate-buffer (switch-to-buffer target))
      (sr-history-push default-directory)
      (revert-buffer)))
  (sr-highlight))

(defun sr-browse-pane ()
  "Browse the directory in the active pane."
  (interactive)
  (if (not (featurep 'browse-url))
      (error "Sunrise: feature `browse-url' not available!")
    (let ((url (concat "file://" (expand-file-name default-directory))))
      (message "Browsing directory %s " default-directory)
      (if (featurep 'w3m)
          (eval '(w3m-goto-url url))
        (browse-url url)))))

(defun sr-browse-file (&optional file)
  "Display the selected file in the default web browser."
  (interactive)
  (unless (featurep 'browse-url)
    (error "ERROR: Feature browse-url not available!"))
  (setq file (or file (dired-get-filename)))
  (save-selected-window
    (sr-select-viewer-window)
    (let ((buff (current-buffer)))
      (browse-url (concat "file://" file))
      (unless (eq buff (current-buffer))
        (sr-scrollable-viewer (current-buffer)))))
  (message "Browsing \"%s\" in web browser" file))

(defun sr-revert-buffer (&optional _ignore-auto _no-confirm)
  "Revert the current pane using the contents of the backup buffer (if any).
If the buffer is non-virtual the backup buffer is killed."
  (interactive)
  (if (buffer-live-p sr-backup-buffer)
      (let ((marks (dired-remember-marks (point-min) (point-max)))
            (focus (dired-get-filename 'verbatim t))
            (inhibit-read-only t))
        (erase-buffer)
        (insert-buffer-substring sr-backup-buffer)
        (sr-beginning-of-buffer)
        (dired-mark-remembered marks)
        (if focus (sr-focus-filename focus))
        (dired-change-marks ?\t ?*)
        (if (eq 'sr-mode major-mode) (sr-kill-backup-buffer)))
    (unless (or (eq major-mode 'sr-virtual-mode)
                (local-variable-p 'sr-virtual-buffer))
      (dired-revert)
      (if (string= "NUMBER" (get sr-selected-window 'sorting-order))
          (sr-sort-by-number t)
        (if (get sr-selected-window 'sorting-reverse)
            (sr-reverse-pane)))))
  (sr-display-attributes (point-min) (point-max) sr-show-file-attributes)
  (sr-highlight))

(defun sr-kill-pane-buffer ()
  "Kill the buffer currently displayed in the active pane, or quit Sunrise.
Custom variable `sr-kill-unused-buffers' controls whether unused buffers are
killed automatically by Sunrise when the user navigates away from the directory
they contain. When this flag is set, all requests to kill the current buffer are
managed by just calling `sr-quit'."
  (interactive)
  (if sr-kill-unused-buffers
      (sr-quit)
    (kill-buffer (current-buffer))
    (let ((_x (pop (cdr (assoc sr-selected-window sr-history-registry)))))
      (sr-history-stack-reset))))

(defun sr-quick-view (&optional arg)
  "Quickly view the currently selected item.
On regular files, opens the file in quick-view mode (see `sr-quick-view-file'
for more details), on directories, visits the selected directory in the passive
pane, and on symlinks follows the file the link points to in the passive pane.
With optional argument kills the last quickly viewed file without opening a new
buffer."
  (interactive "P")
  (if arg
      (sr-quick-view-kill)
    (let ((name (dired-get-filename nil t)))
      (cond ((file-directory-p name) (sr-quick-view-directory name))
            ((file-symlink-p name) (sr-quick-view-symlink name))
            (t (sr-quick-view-file))))))

(defun sr-quick-view-kill ()
  "Kill the last buffer opened using quick view (if any)."
  (let ((buf other-window-scroll-buffer))
    (when (and (buffer-live-p buf)
               (or (not sr-confirm-kill-viewer)
                   (y-or-n-p (format "Kill buffer %s? " (buffer-name buf)))))
      (setq other-window-scroll-buffer nil)
      (save-window-excursion (kill-buffer buf)))))

(defun sr-quick-view-directory (name)
  "Open the directory NAME in the passive pane."
  (let ((name (expand-file-name name)))
    (sr-in-other (sr-advertised-find-file name))))

(defun sr-quick-view-symlink (name)
  "Follow the target of the symlink NAME in the passive pane."
  (let ((name (expand-file-name (file-symlink-p name))))
    (if (file-exists-p name)
        (sr-in-other (sr-follow-file name))
      (error "Sunrise: file is a symlink to a nonexistent target"))))

(defun sr-quick-view-file ()
  "Open the selected file on the viewer window without selecting it.
Kills any other buffer opened previously the same way."
  (let ((split-width-threshold (* 10 (window-width)))
        (filename (expand-file-name (dired-get-filename nil t))))
    (save-selected-window
      (condition-case description
          (progn
            (sr-select-viewer-window)
            (find-file filename)
            (if (and (not (eq (current-buffer) other-window-scroll-buffer))
                          (buffer-live-p other-window-scroll-buffer))
                (kill-buffer other-window-scroll-buffer))
            (sr-scrollable-viewer (current-buffer)))
        (error (message "%s" (cadr description)))))))

;; These clean up after a quick view:
(add-hook 'sr-quit-hook (defun sr-sr-quit-function ()
                          (setq other-window-scroll-buffer nil)))
(add-hook 'kill-buffer-hook
          (defun sr-kill-viewer-function ()
            (if (eq (current-buffer) other-window-scroll-buffer)
                (setq other-window-scroll-buffer  nil))))

(defun sr-mask-attributes (beg end)
  "Manage the hiding of attributes in region from BEG to END.
Selective hiding of specific attributes can be controlled by customizing the
`sr-attributes-display-mask' variable."
  (let ((cursor beg) props)
    (cl-labels ((sr-make-display-props
            (display-function-or-flag)
            (cond ((functionp display-function-or-flag)
                   `(display
                     ,(apply display-function-or-flag
                             (list (buffer-substring cursor (1- (point)))))))
                  ((null display-function-or-flag) '(invisible t))
                  (t nil))))
      (if sr-attributes-display-mask
          (block block 
            (mapc (lambda (do-display)
                    (search-forward-regexp "\\w")
                    (search-forward-regexp "\\s-")
                    (forward-char -1)
                    (setq props (sr-make-display-props do-display))
                    (when props 
                      (add-text-properties cursor (point) props))
                    (setq cursor (point))
                    (if (>= (point) end) (return-from block)))
                  sr-attributes-display-mask))
        (unless (>= cursor end)
          (add-text-properties cursor (1- end) '(invisible t)))))))

(defun sr-display-attributes (beg end visiblep)
  "Manage the display of file attributes in the region from BEG to END.
if VISIBLEP is nil then shows file attributes in region, otherwise hides them."
  (let ((inhibit-read-only t) (next))
    (save-excursion
      (goto-char beg)
      (forward-line -1)
      (while (and (null next) (< (point) end))
        (forward-line 1)
        (setq next (dired-move-to-filename)))
      (while (and next (< next end))
        (beginning-of-line)
        (forward-char 1)
        (if (not visiblep)
            (sr-mask-attributes (point) next)
          (remove-text-properties (point) next '(invisible t))
          (remove-text-properties (point) next '(display)))
        (forward-line 1)
        (setq next (dired-move-to-filename))))))

(defun sr-toggle-attributes ()
  "Hide/Show the attributes of all files in the active pane."
  (interactive)
  (setq sr-show-file-attributes (not sr-show-file-attributes))
  (sr-display-attributes (point-min) (point-max) sr-show-file-attributes))

(defun sr-toggle-truncate-lines ()
  "Enable/Disable truncation of long lines in the active pane."
  (interactive)
  (if (sr-truncate-p)
      (progn
        (setq truncate-partial-width-windows (sr-truncate-v nil))
        (message "Sunrise: wrapping long lines"))
    (progn
      (setq truncate-partial-width-windows (sr-truncate-v t))
      (message "Sunrise: truncating long lines")))
  (sr-silently (dired-do-redisplay)))

(defun sr-truncate-p ()
  "Return non-nil if `truncate-partial-width-windows' affects the current pane.
Used by `sr-toggle-truncate-lines'."
  (if (numberp truncate-partial-width-windows)
      (< 0 truncate-partial-width-windows)
    truncate-partial-width-windows))

(defun sr-truncate-v (active)
  "Return the appropriate value for `truncate-partial-width-widows'.
Depends on the Emacs version being used. Used by
`sr-toggle-truncate-lines'."
  (or (and (version<= "23" emacs-version)
           (or (and active 3000) 0))
      active))

(defun sr-sort-order (label option)
  "Change the sorting order of the active pane.
Appends additional options to `dired-listing-switches' and
reverts the buffer."
  (if (eq major-mode 'sr-virtual-mode)
      (sr-sort-virtual option)
    (progn
      (put sr-selected-window 'sorting-order label)
      (put sr-selected-window 'sorting-options option)
      (let ((dired-listing-switches dired-listing-switches))
        (unless (string-match "^/ftp:" default-directory)
          (setq dired-listing-switches sr-listing-switches))
        (dired-sort-other (concat dired-listing-switches option) t))
      (revert-buffer)))
  (message "Sunrise: sorting entries by %s" label))

(defmacro sr-defun-sort-by (postfix options)
  "Helper macro for defining `sr-sort-by-xxx' functions."
  `(defun ,(intern (format "sr-sort-by-%s" postfix)) ()
     ,(format "Sorts the contents of the current Sunrise pane by %s." postfix)
     (interactive)
     (sr-sort-order ,(upcase postfix) ,options)))
(sr-defun-sort-by "name" "")
(sr-defun-sort-by "extension" "X")
(sr-defun-sort-by "time" "t")
(sr-defun-sort-by "size" "S")

(defun sr-sort-by-number (&optional inhibit-label)
  "Sort the contents of the current Sunrise pane numerically.
Displays entries containing unpadded numbers in a more logical
order than when sorted alphabetically by name."
  (interactive)
  (sr-sort-by-operation 'sr-numerical-sort-op (unless inhibit-label "NUMBER"))
  (if (get sr-selected-window 'sorting-reverse) (sr-reverse-pane)))

(defun sr-interactive-sort (order)
  "Prompt for a new sorting order for the active pane and apply it."
  (interactive "cSort by (n)ame, n(u)mber, (s)ize, (t)ime or e(x)tension? ")
  (if (>= order 97)
      (setq order (- order 32)))
  (case order
    (?U (sr-sort-by-number))
    (?T (sr-sort-by-time))
    (?S (sr-sort-by-size))
    (?X (sr-sort-by-extension))
    (t  (sr-sort-by-name))))

(defun sr-reverse-pane (&optional interactively)
  "Reverse the contents of the active pane."
  (interactive "p")
  (let ((line (line-number-at-pos))
        (reverse (get sr-selected-window 'sorting-reverse)))
    (sr-sort-by-operation 'identity)
    (when interactively
      (put sr-selected-window 'sorting-reverse (not reverse))
      (goto-char (point-min)) (forward-line (1- line))
      (re-search-forward directory-listing-before-filename-regexp nil t))))

(defun sr-sort-virtual (option)
  "Manage sorting of buffers in Sunrise VIRTUAL mode."
  (let ((opt (string-to-char option)) (inhibit-read-only t) (beg) (end))
    (case opt
      (?X (sr-end-of-buffer)
          (setq end (point-at-eol))
          (sr-beginning-of-buffer)
          (setq beg (point-at-bol))
          (sort-regexp-fields nil "^.*$" "[/.][^/.]+$" beg end))
      (?t (sr-sort-by-operation
           (lambda (x) (sr-attribute-sort-op 5 t x)) "TIME"))
      (?S (sr-sort-by-operation
           (lambda (x) (sr-attribute-sort-op 7 t x)) "SIZE"))
      (t (sr-sort-by-operation
          (lambda (x) (sr-attribute-sort-op -1 nil x)) "NAME")))))

(defun sr-sort-by-operation (operation &optional label)
  "General function for reordering the contents of a Sunrise pane.
OPERATION is a function that receives a list produced by
`sr-build-sort-lists', reorders it in some way, transforming it
into a list that can be passed to `sort-reorder', so the records
in the current buffer are reordered accordingly. The LABEL is a
string that will be used to set the sorting order of the current
pane and then displayed in the minibuffer; if it's not provided
or its value is nil then the ordering enforced by this function
is transient and can be undone by reverting the pane, or by
moving it to a different directory. See `sr-numerical-sort-op'
and `sr-attribute-sort-op' for examples of OPERATIONs."
  (interactive)
  (let ((messages (> (- (point-max) (point-min)) 50000))
        (focus (dired-get-filename 'verbatim t))
        (inhibit-read-only t))
    (if messages (message "Finding sort keys..."))
    (let* ((sort-lists (sr-build-sort-lists))
           (old (reverse sort-lists))
           (beg) (end))
      (if messages (message "Sorting records..."))
      (setq sort-lists (apply operation (list sort-lists)))
      (if messages (message "Reordering buffer..."))
      (save-excursion
        (save-restriction
          (sr-end-of-buffer)
          (setq end (point-at-eol))
          (sr-beginning-of-buffer)
          (setq beg (point-at-bol))
          (narrow-to-region beg end)
          (sort-reorder-buffer sort-lists old)))
      (if messages (message "Reordering buffer... Done")))
    (sr-highlight)
    (if focus (sr-focus-filename focus))
    (when label
      (put sr-selected-window 'sorting-order label)
      (message "Sunrise: sorting entries by %s" label)))
  nil)

(defun sr-numerical-sort-op (sort-lists)
  "Strategy used to numerically sort contents of a Sunrise pane.
Used by `sr-sort-by-operation'. See `sr-sort-by-number' for more
on this kind of sorting."
  (mapcar
   'cddr
   (sort
    (sort
     (mapcar
      (lambda (x)
        (let ((key (buffer-substring-no-properties (car x) (cddr x))))
          (append
           (list key
                 (string-to-number (replace-regexp-in-string "^[^0-9]*" "" key))
                 (cdr x))
           (cdr x))))
      sort-lists)
     (lambda (a b) (string< (car a) (car b))))
    (lambda (a b) (< (cadr a) (cadr b))))))

(defun sr-attribute-sort-op (nth-attr as-number sort-lists)
  "Strategy used to sort contents of a Sunrise pane according to file attributes.
Used by `sr-sort-by-operation'. See `file-attributes' for a list
of supported attributes and their positions. Directories are
forced to remain always on top. NTH-ATTR is the position of the
attribute to use for sorting, or -1 for the name of the file.
AS-NUMBER determines whether comparisons will be numeric or
alphabetical. SORT-LISTS is a list of positions obtained from
`sr-build-sort-lists'."
  (let ((attributes (sr-files-attributes))
        (zero (if as-number 0 "")))
    (mapcar
     'cddr
     (sort
      (sort
       (mapcar
        (lambda (x)
          (let* ((key (buffer-substring-no-properties (car x) (cddr x)))
                 (key (sr-chop ?/ (replace-regexp-in-string " -> .*$" "" key)))
                 (attrs (assoc-default key attributes))
                 (index))
            (when attrs
              (setq attrs (apply 'cons attrs)
                    index (or (nth (1+ nth-attr) attrs) zero))
              (append (list (cadr attrs) index (cdr x)) (cdr x)))))
        sort-lists)
       (lambda (a b) (sr-compare nth-attr (cadr b) (cadr a))))
      (lambda (a b)
        (if (and (car a) (car b))
            (sr-compare nth-attr (cadr b) (cadr a))
          (and (car a) (not (stringp (car a))))))))))

(defun sr-build-sort-lists ()
  "Analyse contents of the current Sunrise pane for `sr-sort-by-operation'.
Builds a list of dotted lists of the form (a b . c) -- where 'a'
is the position at the start of the file name in an entry, while
'b' and 'c' are the start and end positions of the whole entry.
These lists are used by `sr-sort-by-operation' to sort the
contents of the pane in arbitrary ways."
  (delq nil
        (mapcar
         (lambda (x) (and (atom (car x)) x))
         (save-excursion
           (sr-beginning-of-buffer)
           (beginning-of-line)
           (sort-build-lists 'forward-line 'end-of-line 'dired-move-to-filename
                             nil)))))

(defun sr-compare (mode a b)
  "General comparison function, used to sort files in VIRTUAL buffers.
MODE must be a number; if it is less than 0, the direction of the
comparison is inverted: (sr-compare -1 a b) === (sr-compare 1
b a). Compares numbers using `<', strings case-insensitively
using `string<' and lists recursively until the first two
elements that are non-equal are found."
  (if (< mode 0) (let (tmp) (setq tmp a a b b tmp mode (abs mode))))
  (cond ((or (null a) (null b)) nil)
        ((and (listp a) (listp b)) (if (= (car a) (car b))
                                       (sr-compare mode (cdr a) (cdr b))
                                     (sr-compare mode (car a) (car b))))
        ((and (stringp a) (stringp b)) (string< (downcase a) (downcase b)))
        ((and (numberp a) (numberp b)) (< a b))
        (t nil)))

(defun sr-scroll-up ()
  "Scroll the current pane or (if active) the viewer pane 1 line up."
  (interactive)
  (if (buffer-live-p other-window-scroll-buffer)
      (save-selected-window
        (sr-select-viewer-window)
        (scroll-up 1))
    (scroll-up 1)))

(defun sr-scroll-down ()
  "Scroll the current pane or (if active) the viewer pane 1 line down."
  (interactive)
  (if (buffer-live-p other-window-scroll-buffer)
      (save-selected-window
        (sr-select-viewer-window)
        (scroll-down 1))
    (scroll-down 1)))

(defun sr-scroll-quick-view ()
  "Scroll down the viewer window during a quick view."
  (interactive)
  (if other-window-scroll-buffer (scroll-other-window)))

(defun sr-scroll-quick-view-down ()
  "Scroll down the viewer window during a quick view."
  (interactive)
  (if other-window-scroll-buffer (scroll-other-window-down nil)))

(defun sr-undo ()
  "Restore selection as it was before the last file operation."
  (interactive)
  (dired-undo)
  (sr-highlight))

;;; ============================================================================
;;; Passive & synchronized navigation functions:

(defun sr-sync ()
  "Toggle the Sunrise synchronized navigation feature."
  (interactive)
  (setq sr-synchronized (not sr-synchronized))
  (mapc 'sr-mark-sync (list sr-left-buffer sr-right-buffer))
  (message "Sunrise: sync navigation is now %s" (if sr-synchronized "ON" "OFF"))
  (run-hooks 'sr-refresh-hook)
  (sr-in-other (run-hooks 'sr-refresh-hook)))

(defun sr-mark-sync (&optional buffer)
  "Change `mode-name' depending on whether synchronized navigation is enabled."
  (save-window-excursion
    (if buffer
        (switch-to-buffer buffer))
    (setq mode-name (concat "Sunrise "
                            (if sr-synchronized "SYNC-NAV" "Commander")))))

;; This advertises synchronized navigation in all new buffers:
(add-hook 'sr-mode-hook 'sr-mark-sync)

(defun sr-next-line-other ()
  "Move the cursor down in the passive pane."
  (interactive)
  (sr-in-other (dired-next-line 1)))

(defun sr-prev-line-other ()
  "Move the cursor up in the passive pane."
  (interactive)
  (sr-in-other (dired-next-line -1)))

(defun sr-goto-dir-other (dir)
  "Change the current directory in the passive pane to the given one."
  (interactive (list (read-directory-name
                      "Change directory in PASSIVE pane (file or pattern): "
                      sr-other-directory)))
  (sr-in-other (sr-goto-dir dir)))

(defun sr-advertised-find-file-other ()
  "Open the file/directory selected in the passive pane."
  (interactive)
  (if sr-synchronized
      (let ((target (sr-directory-name-proper (dired-get-filename))))
        (sr-change-window)
        (if (file-directory-p target)
            (sr-goto-dir (expand-file-name target))
          (if (y-or-n-p "Unable to synchronize. Disable sync navigation? ")
              (sr-sync)))
        (sr-change-window)
        (sr-advertised-find-file))
    (sr-in-other (sr-advertised-find-file))))

(defun sr-mouse-advertised-find-file (e)
  "Open the file/directory pointed to by the mouse."
  (interactive "e")
  (sr-mouse-change-window e)
  (sr-advertised-find-file))

(defun sr-prev-subdir-other (&optional count)
  "Go to the previous subdirectory in the passive pane."
  (interactive "P")
  (let ((count (or count 1)))
    (sr-in-other (sr-dired-prev-subdir count))))

(defun sr-follow-file-other ()
  "Go to the directory of the selected file, but in the passive pane."
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (sr-in-other (sr-follow-file filename))))

(defun sr-history-prev-other ()
  "Change to previous directory (if any) in the passive pane's history list."
  (interactive)
  (sr-in-other (sr-history-prev)))

(defun sr-history-next-other ()
  "Change to the next directory (if any) in the passive pane's history list."
  (interactive)
  (sr-in-other (sr-history-next)))

(defun sr-mark-other (arg)
  "Mark the current (or next ARG) files in the passive pane."
  (interactive "P")
  (setq arg (or arg 1))
  (sr-in-other (dired-mark arg)))

(defun sr-unmark-backward-other (arg)
  (interactive "p")
  (sr-in-other (dired-unmark-backward arg)))

(defun sr-unmark-all-marks-other ()
  "Remove all marks from the passive pane."
  (interactive)
  (sr-in-other (dired-unmark-all-marks)))

;;; ============================================================================
;;; Progress feedback functions:

(defun sr-progress-prompt (op-name)
  "Build the default progress feedback message."
  (concat "Sunrise: " op-name "... "))

(defun sr-make-progress-reporter (op-name totalsize)
  "Make a new Sunrise progress reporter.
Prepends two integers (accumulator and scale) to a standard
progress reporter (built using `make-progress-reporter' from
subr.el): accumulator keeps the current state of the reporter,
and scale is used when the absolute value of 100% is bigger than
`most-positive-fixnum'."
  (let ((accumulator 0) (scale 1) (maxval totalsize))
    (when (> totalsize most-positive-fixnum)
      (setq scale (/ totalsize most-positive-fixnum))
      (setq maxval most-positive-fixnum))
    (list accumulator scale
          (make-progress-reporter
           (sr-progress-prompt op-name) 0 maxval 0 1 0.5))))

(defun sr-progress-reporter-update (reporter size)
  "Update REPORTER (a Sunrise progress reporter) by adding SIZE to its state."
  (let ((scale (cadr reporter)))
    (setcar reporter (+ (truncate (/ size scale)) (car reporter)))
    (progress-reporter-update (car (cddr reporter)) (car reporter))))

(defun sr-progress-reporter-done (reporter)
  "Print REPORTER's feedback message followed by \"done\" in echo area."
  (progress-reporter-done (car (cddr reporter))))

;;; ============================================================================
;;; File manipulation functions:

(defun sr-create-files (&optional qty)
  "Interactively create empty file(s) with the given name or template.
Optional prefix argument specifies the number of files to create.
*NEVER* overwrites existing files. A template may contain one
%-sequence like those used by `format', but the only supported
specifiers are: d (decimal), x (hex) or o (octal)."
  (interactive "p")
  (let* ((qty (or (and (integerp qty) (< 0 qty) qty) 1))
         (prompt (if (>= 1 qty) "Create file: "
                   (format "Create %d files using template: " qty)))
         (filename (read-file-name prompt)) (name))
    (with-temp-buffer
      (if (>= 1 qty)
          (unless (file-exists-p filename) (write-file filename))
        (unless (string-match "%[0-9]*[dox]" filename)
          (setq filename (concat filename ".%d")))
        (setq filename (replace-regexp-in-string "%\\([^%]\\)" "%%\\1" filename)
              filename (replace-regexp-in-string
                        "%%\\([0-9]*[dox]\\)" "%\\1" filename))
        (dotimes (n qty)
          (setq name (format filename (1+ n)))
          (unless (file-exists-p name) (write-file name)))))
    (sr-revert-buffer)))

(defun sr-editable-pane ()
  "Put the current pane in File Names Editing mode (`wdired-mode')."
  (interactive)
  (sr-graphical-highlight 'sr-editing-path-face)
  (let* ((was-virtual (eq major-mode 'sr-virtual-mode))
         (major-mode 'dired-mode))
    (wdired-change-to-wdired-mode)
    (if was-virtual
        (set (make-local-variable 'sr-virtual-buffer) t)))
  (run-hooks 'sr-refresh-hook))

(defun sr-readonly-pane (as-virtual)
  "Put the current pane back in Sunrise mode."
  (when as-virtual
    (sr-virtual-mode)
    (sr-force-passive-highlight t))
  (dired-build-subdir-alist)
  (sr-revert-buffer))

(defmacro sr-protect-terminate-wdired (&rest body)
  "Compile the `cl-letf' forms used in `sr-terminate-wdired'.
This macro allows interpreted code to work without requiring
cl-macs at runtime."
  `(cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (ignore)))
          ((symbol-function 'revert-buffer)
           (lambda (&optional ignore-auto noconfirm preserve-modes))
           (ignore)))
     ,@body))

(defun sr-terminate-wdired (fun)
  "Restore the current pane's original mode after editing with WDired."
  (ad-add-advice
   fun
   (ad-make-advice
    (intern (concat "sr-advice-" (symbol-name fun))) nil t
    `(advice
      lambda ()
      (if (not sr-running)
          ad-do-it
        (let ((was-virtual (local-variable-p 'sr-virtual-buffer))
              (saved-point (point)))
          (sr-save-aspect
           (setq major-mode 'wdired-mode)
           (sr-protect-terminate-wdired ad-do-it)
           (sr-readonly-pane was-virtual)
           (goto-char saved-point))
          (sr-unhighlight 'sr-editing-path-face)))))
   'around 'last)
  (ad-activate fun nil))
(sr-terminate-wdired 'wdired-finish-edit)
(sr-terminate-wdired 'wdired-abort-changes)

(defun sr-do-copy ()
  "Copy selected files and directories recursively to the passive pane."
  (interactive)
  (let* ((items (dired-get-marked-files nil))
         (vtarget (sr-virtual-target))
         (target (or vtarget sr-other-directory))
         (progress))
    (if (and (not vtarget) (sr-equal-dirs default-directory sr-other-directory))
        (dired-do-copy)
      (when (sr-ask "Copy" target items #'y-or-n-p)
        (if vtarget
            (progn
              (sr-copy-virtual)
              (message "Done: %d items(s) copied" (length items)))
          (progn
            (setq progress (sr-make-progress-reporter
                            "copying" (sr-files-size items)))
            (sr-clone items target #'copy-file progress ?C)
            (sr-progress-reporter-done progress)))
        (sr-silently (dired-unmark-all-marks))))))

(defun sr-do-symlink ()
  "Symlink selected files or directories from one pane to the other."
  (interactive)
  (if (sr-equal-dirs default-directory sr-other-directory)
      (dired-do-symlink)
    (sr-link #'make-symbolic-link "Symlink" dired-keep-marker-symlink)))

(defun sr-do-relsymlink ()
  "Symlink selected files or directories from one pane to the other relatively.
See `dired-make-relative-symlink'."
  (interactive)
  (if (sr-equal-dirs default-directory sr-other-directory)
      (dired-do-relsymlink)
    (sr-link #'dired-make-relative-symlink
             "RelSymLink"
             dired-keep-marker-relsymlink)))

(defun sr-do-hardlink ()
  "Same as `dired-do-hardlink', but refuse to hardlink files to VIRTUAL buffers."
  (interactive)
  (if (sr-virtual-target)
      (error "Cannot hardlink files to a VIRTUAL buffer, try (C)opying instead")
    (dired-do-hardlink)))

(defun sr-do-rename ()
  "Move selected files and directories recursively from one pane to the other."
  (interactive)
  (when (sr-virtual-target)
    (error "Cannot move files to a VIRTUAL buffer, try (C)opying instead"))
  (if (sr-equal-dirs default-directory sr-other-directory)
      (dired-do-rename)
    (let ((marked (dired-get-marked-files)))
      (when (sr-ask "Move" sr-other-directory marked #'y-or-n-p)
        (let ((names (mapcar #'file-name-nondirectory marked))
              (progress (sr-make-progress-reporter "renaming" (length marked)))
              (inhibit-read-only t))
          (sr-in-other
           (progn
             (sr-move-files marked default-directory progress)
             (revert-buffer)
             (when (eq major-mode 'sr-mode)
               (dired-mark-remembered
                (mapcar (lambda (x) (cons (expand-file-name x) ?R)) names))
               (sr-focus-filename (car names)))))
          (sr-progress-reporter-done progress))
        (sr-silently (revert-buffer))))))

(defun sr-do-delete ()
  "Remove selected files from the file system."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (mode (sr-ask "Delete" nil files #'sr-y-n-or-a-p))
         (deletion-mode (cond ((eq mode 'ALWAYS) 'always)
                              (mode 'top)
                              (t (error "(No deletions performed)")))))
    (mapc (lambda (x)
            (message "Deleting %s" x)
            (dired-delete-file x deletion-mode)) files)
    (if (eq major-mode 'sr-virtual-mode)
        (dired-do-kill-lines)
      (revert-buffer))))

(defun sr-do-flagged-delete ()
  "Remove flagged files from the file system."
  (interactive)
  (let* ((dired-marker-char dired-del-marker)
         (regexp (dired-marker-regexp)) )
    (if (save-excursion (goto-char (point-min))
                        (re-search-forward regexp nil t))
        (sr-do-delete)
      (message "(No deletions requested)"))))

(defun sr-do-clone (&optional mode)
  "Clone all selected items recursively into the passive pane."
  (interactive "cClone as: (D)irectories only, (C)opies, (H)ardlinks,\
 (S)ymlinks or (R)elative symlinks? ")

  (if (sr-virtual-target)
      (error "Cannot clone into a VIRTUAL buffer, try (C)opying instead"))
  (if (sr-equal-dirs default-directory sr-other-directory)
      (error "Cannot clone inside one single directory, please select a\
 different one in the passive pane"))

  (let ((target sr-other-directory) clone-op items progress)
    (if (and mode (>= mode 97)) (setq mode (- mode 32)))
    (setq clone-op
          (case mode
            (?D nil)
            (?C #'copy-file)
            (?H #'add-name-to-file)
            (?S #'make-symbolic-link)
            (?R #'dired-make-relative-symlink)
            (t (error "Invalid cloning mode: %c" mode))))
    (setq items (dired-get-marked-files nil))
    (setq progress (sr-make-progress-reporter
                    "cloning" (sr-files-size items)))
    (sr-clone items target clone-op progress ?K)
    (dired-unmark-all-marks)
    (message "Done: %d items(s) dispatched" (length items))))

(defun sr-fast-backup-files ()
  "Make backup copies of all marked files inside the same directory.
The extension to append to each filename can be controlled by
setting the value of the `sr-fast-backup-extension' custom
variable. Directories are not copied."
  (interactive)
  (let ((extension (if (listp sr-fast-backup-extension)
                       (eval sr-fast-backup-extension)
                     sr-fast-backup-extension)))
    (dired-do-copy-regexp "$" extension))
  (revert-buffer))

(defun sr-clone (items target clone-op progress mark-char)
  "Clone all given items (files and dirs) recursively into the passive pane."
  (let ((names (mapcar #'file-name-nondirectory items))
        (inhibit-read-only t))
    (with-current-buffer (sr-other 'buffer)
      (sr-clone-files items target clone-op progress))
    (when (window-live-p (sr-other 'window))
      (sr-in-other
       (progn
         (revert-buffer)
         (when (memq major-mode '(sr-mode sr-virtual-mode))
           (dired-mark-remembered
            (mapcar (lambda (x) (cons (expand-file-name x) mark-char)) names))
           (sr-focus-filename (car names))))))))

(defun sr-clone-files (file-paths target-dir clone-op progress &optional do-overwrite)
  "Clone all files in FILE-PATHS to TARGET-DIR using CLONE-OP to clone the files.
FILE-PATHS should be a list of full paths."
  (setq target-dir (replace-regexp-in-string "/?$" "/" target-dir))
  (mapc
   (function
    (lambda (f)
      (sr-progress-reporter-update progress (nth 7 (file-attributes f)))
      (let* ((name (file-name-nondirectory f))
             (target-file (concat target-dir name))
             (symlink-to (file-symlink-p (sr-chop ?/ f)))
             (clone-args (list f target-file t)))
        (cond
         (symlink-to
          (progn
            (if (file-exists-p symlink-to)
                (setq symlink-to (expand-file-name symlink-to)))
            (make-symbolic-link symlink-to target-file do-overwrite)))

         ((file-directory-p f)
          (let ((initial-path (file-name-directory f)))
            (unless (file-symlink-p initial-path)
              (sr-clone-directory
               initial-path name target-dir clone-op progress do-overwrite))))

         (clone-op
          ;; (message "[[Cloning: %s => %s]]" f target-file)
          (if (eq clone-op 'copy-file)
              (setq clone-args
                    (append clone-args (list dired-copy-preserve-time))))
          (if (file-exists-p target-file)
              (if (or (eq do-overwrite 'ALWAYS)
                      (setq do-overwrite (sr-ask-overwrite target-file)))
                  (apply clone-op clone-args))
            (apply clone-op clone-args)))))))
   file-paths))

(defun sr-clone-directory (in-dir d to-dir clone-op progress do-overwrite)
  "Clone directory IN-DIR/D and all its files recursively to TO-DIR.
IN-DIR/D => TO-DIR/D using CLONE-OP to clone the files."
  (setq d (replace-regexp-in-string "/?$" "/" d))
  (if (string= "" d)
      (setq to-dir (concat to-dir (sr-directory-name-proper in-dir))))
  (let* ((files-in-d (sr-list-of-contents (concat in-dir d)))
         (file-paths-in-d
          (mapcar (lambda (f) (concat in-dir d f)) files-in-d)))
    (unless (file-exists-p (concat to-dir d))
      (make-directory (concat to-dir d)))
    (sr-clone-files file-paths-in-d (concat to-dir d) clone-op progress do-overwrite)))

(defsubst sr-move-op (file target-dir progress do-overwrite)
  "Helper function used by `sr-move-files' to rename files and directories."
  (condition-case nil
      (dired-rename-file file target-dir do-overwrite)
    (error
     (sr-clone-directory file "" target-dir 'copy-file progress do-overwrite)
     (dired-delete-file file 'always))))

(defun sr-move-files (file-path-list target-dir progress &optional do-overwrite)
  "Move all files in FILE-PATH-LIST (list of full paths) to TARGET-DIR."
  (mapc
   (function
    (lambda (f)
      (if (file-directory-p f)
          (progn
            (setq f (replace-regexp-in-string "/?$" "/" f))
            (sr-progress-reporter-update progress 1)
            (sr-move-op f target-dir progress do-overwrite))
        (let* ((name (file-name-nondirectory f))
               (target-file (concat target-dir name)))
          ;; (message "Renaming: %s => %s" f target-file)
          (sr-progress-reporter-update progress 1)
          (if (file-exists-p target-file)
              (if (or (eq do-overwrite 'ALWAYS)
                      (setq do-overwrite (sr-ask-overwrite target-file)))
                  (dired-rename-file f target-file t))
            (dired-rename-file f target-file t)) ))))
   file-path-list))

(defun sr-link (creator action marker)
  "Helper function for implementing `sr-do-symlink' and `sr-do-relsymlink'."
  (if (sr-virtual-target)
      (error "Cannot link files to a VIRTUAL buffer, try (C)opying instead.")
    (dired-create-files creator action (dired-get-marked-files nil)
                        (lambda (from)
                          (setq from (sr-chop ?/ from))
                          (if (file-directory-p from)
                              (setq from (sr-directory-name-proper from))
                            (setq from (file-name-nondirectory from)))
                          (expand-file-name from sr-other-directory))
                        marker)))

(defun sr-virtual-target ()
  "If the passive pane is in VIRTUAL mode, return its name as a string.
Otherwise returns nil."
  (save-window-excursion
    (switch-to-buffer (sr-other 'buffer))
    (if (eq major-mode 'sr-virtual-mode)
        (or (buffer-file-name) "Sunrise VIRTUAL buffer")
      nil)))

(defun sr-copy-virtual ()
  "Manage copying of files or directories to buffers in VIRTUAL mode."
  (let ((fileset (dired-get-marked-files nil))
        (inhibit-read-only t) (beg))
    (sr-change-window)
    (goto-char (point-max))
    (setq beg (point))
    (mapc (lambda (file)
            (insert-char 32 2)
            (setq file (dired-make-relative file default-directory)
                  file (sr-chop ?/ file))
            (insert-directory file sr-virtual-listing-switches))
          fileset)
    (sr-display-attributes beg (point-at-eol) sr-show-file-attributes)
    (unwind-protect
        (delete-region (point) (line-end-position))
      (progn
        (sr-change-window)
        (dired-unmark-all-marks)))))

(defun sr-ask (prompt target files function)
  "Use FUNCTION to ask whether to do PROMPT on FILES with TARGET as destination."
  (if (and files (listp files))
      (let* ((len (length files))
             (msg (if (< 1 len)
                      (format "* [%d items]" len)
                    (file-name-nondirectory (car files)))))
        (if target
            (setq msg (format "%s to %s" msg target)))
        (funcall function (format "%s %s? " prompt msg)))))

(defun sr-ask-overwrite (file-name)
  "Ask whether to overwrite the given FILE-NAME."
  (sr-y-n-or-a-p (format "File %s exists. OK to overwrite? " file-name)))

(defun sr-y-n-or-a-p (prompt)
  "Ask the user with PROMPT for an answer y/n/a ('a' stands for 'always').
Returns t if the answer is y/Y, nil if the answer is n/N or the
symbol `ALWAYS' if the answer is a/A."
  (setq prompt (concat prompt "([y]es, [n]o or [a]lways)"))
  (let ((resp -1))
    (while (not (memq resp '(?y ?Y ?n ?N ?a ?A)))
      (setq resp (read-event prompt))
      (setq prompt "Please answer [y]es, [n]o or [a]lways "))
    (if (>= resp 97)
        (setq resp (- resp 32)))
    (case resp
      (?Y t)
      (?A 'ALWAYS)
      (t nil))))

(defun sr-overlapping-paths-p (dir1 dir2)
  "Return non-nil if directory DIR2 is located inside directory DIR1."
  (when (and dir1 dir2)
    (setq dir1 (expand-file-name (file-name-as-directory dir1))
          dir2 (expand-file-name dir2))
    (if (>= (length dir2) (length dir1))
        (equal (substring dir2 0 (length dir1)) dir1)
      nil)))

(defun sr-list-of-contents (dir)
  "Return the list of all files in DIR as a list of strings."
  (sr-filter (function (lambda (x) (not (string-match "\\.\\.?/?$" x))))
             (directory-files dir)))

(defun sr-list-of-directories (dir)
 "Return the list of directories in DIR as a list of strings.
The list does not include the current directory and the parent directory."
 (let ((result (sr-filter (function (lambda (x)
                                      (file-directory-p (concat dir "/" x))))
                          (sr-list-of-contents dir))))
   (mapcar (lambda (x) (concat x "/")) result)))

(defun sr-list-of-files (dir)
  "Return the list of regular files in DIR as a list of strings.
Broken links are *not* considered regular files."
  (sr-filter
   (function (lambda (x) (file-regular-p (concat dir "/" x))))
   (sr-list-of-contents dir)))

(defun sr-filter (p x)
  "Return the elements of the list X that satisfy the predicate P."
  (let ((res-list nil))
    (while x
      (if (apply p (list (car x)))
          (setq res-list (cons (car x) res-list)))
      (setq x (cdr x)))
    (reverse res-list)))

(defun sr-directory-name-proper (file-path)
  "Return the proper name of the directory FILE-PATH, without initial path."
  (if file-path
      (let (
            (file-path-1 (substring file-path 0 (- (length file-path) 1)))
            (lastchar (substring file-path (- (length file-path) 1)))
            )
        (concat (file-name-nondirectory file-path-1) lastchar))))

;;; ============================================================================
;;; Directory and file comparison functions:

(defun sr-compare-panes ()
  "Compare the contents of Sunrise panes."
  (interactive)
  (let* ((file-alist1 (sr-files-attributes))
         (other (sr-other 'buffer))
         (file-alist2 (with-current-buffer other (sr-files-attributes)))
         (progress
          (sr-make-progress-reporter
           "comparing" (+ (length file-alist1) (length file-alist2))))
         (predicate `(prog1 ,(sr-ask-compare-panes-predicate)
                            (sr-progress-reporter-update ',progress 1)))
         (file-list1 (mapcar 'cadr (dired-file-set-difference
                                    file-alist1 file-alist2 predicate)))
         (file-list2 (mapcar 'cadr (dired-file-set-difference
                                    file-alist2 file-alist1 predicate))))
    (sr-md5 nil)
    (dired-mark-if (member (dired-get-filename nil t) file-list1) nil)
    (with-current-buffer other
      (dired-mark-if (member (dired-get-filename nil t) file-list2) nil))
    (message "Marked in pane1: %s files, in pane2: %s files"
             (length file-list1)
             (length file-list2))
    (sit-for 0.2)))

(defun sr-ask-compare-panes-predicate ()
  "Prompt for the criterion to use for comparing the contents of the panes."
  (let ((prompt "Compare by (d)ate, (s)ize, date_(a)nd_size, (n)ame \
or (c)ontents? ")
        (response -1))
    (while (not (memq response '(?d ?D ?s ?S ?a ?A ?n ?N ?c ?C)))
      (setq response (read-event prompt))
      (setq prompt "Please select: Compare by (d)ate, (s)ize, date_(a)nd_size,\
 (n)ame or (c)ontents? "))
    (if (>= response 97)
        (setq response (- response 32)))
    (case response
      (?D `(not (= mtime1 mtime2)))
      (?S `(not (= size1 size2)))
      (?N nil)
      (?C `(not (string= (sr-md5 file1 t) (sr-md5 file2 t))))
      (t `(or (not (= mtime1 mtime2)) (not (= size1 size2)))))))

(defun sr-files-attributes ()
  "Return a list of all file names and attributes in the current pane.
The list has the same form as the one returned by
`dired-files-attributes', but contains all the files currently
displayed in VIRTUAL panes."
  (delq
   nil
   (mapcar
    (lambda (file-name)
      (unless (member file-name '("." ".."))
        (let ((full-file-name (expand-file-name file-name default-directory)))
          (list file-name full-file-name (file-attributes full-file-name)))))
    (sr-pane-files))))

(defun sr-pane-files ()
  "Return the list of files in the current pane.
For VIRTUAL panes, returns the list of all files being currently
displayed."
  (delq
   nil
   (if (eq major-mode 'sr-virtual-mode)
       (sr-buffer-files (current-buffer))
     (directory-files default-directory))))

(defvar sr-md5 '(nil) "Memoization cache for the sr-md5 function.")
(defun sr-md5 (file-alist &optional memoize)
  "Build and execute a shell command to calculate the MD5 checksum of a file.
Second element of FILE-ALIST is the absolute path of the file. If
MEMOIZE is non-nil, save the result into the `sr-md5' alist so it
can be reused the next time this function is called with the same
path. This cache can be cleared later calling `sr-md5' with nil
as its first argument."
  (if (null file-alist)
      (setq sr-md5 '(nil))
    (let* ((filename (cadr file-alist))
           (md5-digest (cdr (assoc filename sr-md5)))
           (md5-command))
      (unless md5-digest
        (setq md5-command
              (replace-regexp-in-string
               "%f" (format "\"%s\"" filename) sr-md5-shell-command))
        (setq md5-digest (shell-command-to-string md5-command))
        (if memoize
            (push (cons filename md5-digest) sr-md5)))
      md5-digest)))

(defun sr-diff ()
  "Run `diff' on the top two marked files in both panes."
  (interactive)
  (eval (sr-diff-form 'diff))
  (sr-scrollable-viewer (get-buffer "*Diff*")))

(defun sr-ediff ()
  "Run `ediff' on the two top marked files in both panes."
  (interactive)
  (eval (sr-diff-form 'ediff)))

(add-hook 'ediff-before-setup-windows-hook
          (defun sr-ediff-before-setup-windows-function ()
            (setq sr-ediff-on t)))

(add-hook 'ediff-quit-hook
          (defun sr-ediff-quit-function ()
            (setq sr-ediff-on nil)
            (when sr-running
              (if (buffer-live-p sr-restore-buffer)
                  (switch-to-buffer sr-restore-buffer))
              (delete-other-windows)
              (sr-setup-windows))))

(defun sr-diff-form (fun)
  "Return the appropriate form to evaluate for comparing files using FUN."
  (let ((this (sr-pop-mark)) (other nil))
    (unless this
      (setq this (car (dired-get-marked-files t))))
    (if (sr-equal-dirs default-directory sr-other-directory)
        (setq other (sr-pop-mark))
      (progn
        (sr-change-window)
        (setq other (sr-pop-mark))
        (sr-change-window)
        (setq other (or other
                        (if (file-exists-p (concat sr-other-directory this))
                            this
                          (file-name-nondirectory this))))))
    (setq this (concat default-directory this)
          other (concat sr-other-directory other))
    (list fun this other)))

(defun sr-pop-mark ()
  "Pop the first mark in the current Dired buffer."
  (let ((result nil))
    (condition-case description
      (save-excursion
        (goto-char (point-min))
        (dired-next-marked-file 1)
        (setq result (dired-get-filename t t))
        (dired-unmark 1))
      (error (message (cadr description))))
    result))

;;; ============================================================================
;;; File search & analysis functions:

(defun sr-process-kill ()
  "Kill the process running in the current buffer (if any)."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (and proc (eq (process-status proc) 'run)
         (condition-case nil
             (delete-process proc)
           (error nil)))))

(defvar sr-process-map (let ((map (make-sparse-keymap)))
                         (set-keymap-parent map sr-virtual-mode-map)
                         (define-key map "\C-c\C-k" 'sr-process-kill)
                         map)
  "Local map used in Sunrise panes during find and locate operations.")

(defun sr-find-decorate-buffer (find-items)
  "Provide details on `sr-find' execution in the current buffer.
If the current find operation is done only in selected files and directories,
modify the info line of the buffer to reflect this. Additionally, display an
appropriate message in the minibuffer."
  (rename-uniquely)
  (when find-items
    (let ((items-len (length find-items))
          (max-items-len (window-width))
          (inhibit-read-only t))
      (goto-char (point-min))
      (forward-line 1)
      (when (re-search-forward "find \." nil t)
        (if (> items-len max-items-len)
            (setq find-items
                  (concat (substring find-items 0 max-items-len) " ...")))
        (replace-match (format "find %s" find-items)))))
  (sr-beginning-of-buffer)
  (sr-highlight)
  (hl-line-mode 1)
  (message (propertize "Sunrise find (C-c C-k to kill)"
                       'face 'minibuffer-prompt)))

(defun sr-find-apply (fun pattern)
  "Helper function for functions `sr-find', `sr-find-name' and `sr-find-grep'."
  (let* ((suffix (if (eq 'w32 window-system) " {} ;" " \\{\\} \\;"))
         (find-ls-option
          (cons
           (concat "-exec ls -d " sr-virtual-listing-switches suffix)
           "ls -ld"))
         (sr-find-items (sr-quote-marked)) (dir))
    (when sr-find-items
      (if (not (y-or-n-p "Find in marked items only? "))
          (setq sr-find-items nil)
        (setq dir (directory-file-name (expand-file-name default-directory)))
        (add-to-list 'file-name-handler-alist (cons dir 'sr-multifind-handler))))
    (sr-save-aspect
     (sr-alternate-buffer (apply fun (list default-directory pattern)))
     (sr-virtual-mode)
     (use-local-map sr-process-map)
     (sr-keep-buffer))
    (run-with-idle-timer 0.01 nil 'sr-find-decorate-buffer sr-find-items)))

(defun sr-find (pattern)
  "Run `find-dired' passing the current directory as first parameter."
  (interactive "sRun find (with args): ")
  (sr-find-apply 'find-dired pattern))

(defun sr-find-name (pattern)
  "Run `find-name-dired' passing the current directory as first parameter."
  (interactive "sFind name pattern: ")
  (sr-find-apply 'find-name-dired pattern))

(defun sr-find-grep (pattern)
  "Run `find-grep-dired' passing the current directory as first
parameter. Called with prefix asks for additional grep options."
  (interactive "sFind files containing pattern: ")
  (let ((find-grep-options
         (if current-prefix-arg
             (concat find-grep-options
                     " "
                     (read-string "Additional Grep Options: "))
         find-grep-options)))
    (sr-find-apply 'find-grep-dired pattern)))

(defadvice find-dired-sentinel
  (after sr-advice-find-dired-sentinel (proc state))
  "If the current find operation was launched inside the Sunrise
Commander, create a new backup buffer on operation completion or
abort."
  (with-current-buffer (process-buffer proc)
    (when (eq 'sr-virtual-mode major-mode)
      (sr-backup-buffer))))
(ad-activate 'find-dired-sentinel)

(defadvice find-dired-filter
  (around sr-advice-find-dired-filter (proc string))
  "Disable the \"non-foolproof\" padding mechanism in `find-dired-filter' that
breaks Dired when using ls options that omit some columns (like g or G). Defined
by the Sunrise Commander."
  (if (and (eq 'sr-virtual-mode major-mode)
           (or (string-match "g" sr-virtual-listing-switches)
               (string-match "G" sr-virtual-listing-switches)))
      (let ((find-ls-option nil)) ad-do-it)
    ad-do-it))
(ad-activate 'find-dired-filter)

(defun sr-multifind-handler (operation &rest args)
  "Magic file name handler for manipulating the command executed by `find-dired'
when the user requests to perform the find operation on all currently marked
items (as opposed to the current default directory). Removes itself from the
`inhibit-file-name-handlers' every time it's executed."
  (let ((inhibit-file-name-handlers
         (cons 'sr-multifind-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (when (eq operation 'shell-command)
      (setq file-name-handler-alist
            (rassq-delete-all 'sr-multifind-handler file-name-handler-alist))
      (when sr-find-items
        (setcar args (replace-regexp-in-string
                      "find \." (format "find %s" sr-find-items) (car args)))))
    (apply operation args)))

(defun sr-flatten-branch (&optional mode)
  "Display a flat view of the items contained in the current directory and all
its subdirectories, sub-subdirectories and so on (recursively) in the active
pane."
  (interactive "cFlatten branch showing: (E)verything, (D)irectories,\
 (N)on-directories or (F)iles only?")
  (if (and mode (>= mode 97)) (setq mode (- mode 32)))
  (case mode
    (?E (sr-find-name "*"))
    (?D (sr-find "-type d"))
    (?N (sr-find "-not -type d"))
    (?F (sr-find "-type f"))))

(defun sr-prune-paths (regexp)
  "Kill all lines (only the lines) in the current pane matching REGEXP."
  (interactive "sPrune paths matching: ")
  (save-excursion
    (sr-beginning-of-buffer)
    (while (if (string-match regexp (dired-get-filename t))
               (dired-kill-line)
             (dired-next-line 1)))))

(defun sr-locate-filter (locate-buffer search-string)
  "Return a filter function for the background `locate' process."
  `(lambda (process output)
     (let ((inhibit-read-only t)
           (search-regexp ,(regexp-quote search-string))
           (beg (point-max)))
       (set-buffer ,locate-buffer)
       (save-excursion
         (mapc (lambda (x)
                 (when (and (string-match search-regexp x) (file-exists-p x))
                   (goto-char (point-max))
                   (insert-char 32 2)
                   (insert-directory x sr-virtual-listing-switches nil nil)))
               (split-string output "[\r\n]" t))
         (sr-display-attributes beg (point-at-eol) sr-show-file-attributes)))))

(defun sr-locate-sentinel (locate-buffer)
  "Return a sentinel function for the background locate process.
Used to notify about the termination status of the process."
  `(lambda (process status)
     (let ((inhibit-read-only t))
       (set-buffer ,locate-buffer)
       (goto-char (point-max))
       (insert "\n " locate-command " " status)
       (forward-char -1)
       (insert " at " (substring (current-time-string) 0 19))
       (forward-char 1))
     (sr-beginning-of-buffer)
     (sr-highlight)
     (hl-line-mode 1)))

(defun sr-locate-prompt ()
  "Display the message that appears when a locate process is launched."
  (message (propertize "Sunrise locate (C-c C-k to kill)"
                       'face 'minibuffer-prompt)))

(defvar locate-command)
(autoload 'locate-prompt-for-search-string "locate")
(defun sr-locate (search-string &optional _filter _arg)
  "Run locate asynchronously and display the results in Sunrise virtual mode."
  (interactive
   (list (locate-prompt-for-search-string) nil current-prefix-arg))
  (let ((locate-buffer (create-file-buffer "*Sunrise Locate*"))
        (process-connection-type nil)
        (locate-process nil))
    (sr-save-aspect
     (sr-alternate-buffer (switch-to-buffer locate-buffer))
     (cd "/")
     (insert "  " default-directory ":")(newline)
     (insert " Results of: " locate-command " " search-string)(newline)
     (sr-virtual-mode)
     (set-process-filter
      (setq locate-process
            (start-process "Async Locate" nil locate-command search-string))
      (sr-locate-filter locate-buffer search-string))
     (set-process-sentinel locate-process (sr-locate-sentinel locate-buffer))
     (set-process-buffer locate-process locate-buffer)
     (use-local-map sr-process-map)
     (run-with-idle-timer 0.01 nil 'sr-locate-prompt))))

(defun sr-fuzzy-narrow ()
  "Interactively narrow contents of the current pane using fuzzy matching:
  * press Delete or Backspace to revert the buffer to its previous state
  * press Return, C-n or C-p to exit and accept the current narrowed state
  * press Esc or C-g to abort the operation and revert the buffer
  * use ! to prefix characters that should NOT appear beyond a given position.
  Once narrowed and accepted, you can restore the original contents of the pane
  by pressing g (`revert-buffer')."
  (interactive)
  (when sr-running
    (sr-beginning-of-buffer)
    (dired-change-marks ?* ?\t)
    (let ((stack nil) (filter "") (regex "") (next-char nil) (inhibit-quit t))
      (cl-labels ((read-next (f) (read-char (concat "Fuzzy narrow: " f))))
        (setq next-char (read-next filter))
        (sr-backup-buffer)
        (while next-char
          (case next-char
            ((?\e ?\C-g) (setq next-char nil) (sr-revert-buffer))
            (?\C-n (setq next-char nil) (sr-beginning-of-buffer))
            (?\C-p (setq next-char nil) (sr-end-of-buffer))
            ((?\n ?\r) (setq next-char nil))
            ((?\b ?\d)
             (revert-buffer)
             (setq stack (cdr stack) filter (caar stack) regex (cdar stack))
             (unless stack (setq next-char nil)))
            (t
             (setq filter (concat filter (char-to-string next-char)))
             (if (not (eq next-char sr-fuzzy-negation-character))
                 (setq next-char (char-to-string next-char)
                       regex (if (string= "" regex) ".*" regex)
                       regex (concat regex (regexp-quote next-char) ".*"))
               (setq next-char (char-to-string (read-next filter))
                     filter (concat filter next-char)
                     regex (replace-regexp-in-string "\\.\\*\\'" "" regex)
                     regex (concat regex "[^"(regexp-quote next-char)"]*")
                     regex (replace-regexp-in-string "\\]\\*\\[\\^" "" regex)))
             (setq stack (cons (cons filter regex) stack))))
          (when next-char
            (dired-mark-files-regexp (concat "^" regex "$"))
            (dired-toggle-marks)
            (dired-do-kill-lines)
            (setq next-char (read-next filter)))))
      (dired-change-marks ?\t ?*))))

(defun sr-recent-files ()
  "Display the history of recent files in Sunrise virtual mode."
  (interactive)
  (if (not (featurep 'recentf))
      (error "ERROR: Feature recentf not available!"))

  (sr-save-aspect
   (let ((dired-actual-switches dired-listing-switches))
     (sr-switch-to-clean-buffer "*Recent Files*")
     (insert "Recently Visited Files: \n")
     (dolist (file recentf-list)
       (condition-case nil
           (insert-directory file sr-virtual-listing-switches nil nil)
         (error (ignore))))
     (sr-virtual-mode)
     (sr-keep-buffer))))

(defun sr-recent-directories ()
  "Display the history of directories recently visited in the current pane."
  (interactive)
  (sr-save-aspect
   (let ((hist (cdr (assoc sr-selected-window sr-history-registry)))
         (dired-actual-switches dired-listing-switches)
         (pane-name (capitalize (symbol-name sr-selected-window)))
         (switches (concat sr-virtual-listing-switches " -d")))
     (sr-switch-to-clean-buffer (format "*%s Pane History*" pane-name))
     (insert (concat "Recent Directories in " pane-name " Pane: \n"))
     (dolist (dir hist)
       (condition-case nil
           (when dir
             (setq dir (sr-chop ?/ (expand-file-name dir)))
             (insert-directory dir switches nil nil))
         (error (ignore))))
     (sr-virtual-mode))))

(defun sr-switch-to-clean-buffer (name)
  (sr-alternate-buffer (switch-to-buffer name))
  (erase-buffer))

(defun sr-pure-virtual (&optional passive)
  "Create a new empty buffer in Sunrise VIRTUAL mode.
If the optional argument PASSIVE is non-nil, creates the virtual
buffer in the passive pane."
  (interactive "P")
  (if passive
      (progn
        (sr-synchronize-panes)
        (sr-in-other (sr-pure-virtual nil)))
    (sr-save-aspect
     (let* ((dir (directory-file-name (dired-current-directory)))
            (buff (generate-new-buffer-name (buffer-name (current-buffer)))))
       (sr-alternate-buffer (switch-to-buffer buff))
       (goto-char (point-min))
       (insert "  " dir ":")(newline)
       (insert " Pure VIRTUAL buffer: ")(newline)
       (sr-virtual-mode)
       (sr-keep-buffer)))))

(defun sr-dired-do-apply (dired-fun)
  "Helper function for implementing `sr-do-query-replace-regexp' and Co."
  (let ((buff (current-buffer)) (orig sr-restore-buffer))
    (condition-case nil
        (progn
          (sr-quit)
          (switch-to-buffer buff)
          (call-interactively dired-fun)
          (replace-buffer-in-windows buff)
          (sr-bury-panes))
      (quit
       (when orig (switch-to-buffer orig))
       (sunrise)))))

(defun sr-do-query-replace-regexp ()
  "Force Sunrise to quit before executing `dired-do-query-replace-regexp'."
  (interactive)
  (sr-dired-do-apply 'dired-do-query-replace-regexp))

(defun sr-do-search ()
  "Force Sunrise to quit before executing `dired-do-search'."
  (interactive)
  (sr-dired-do-apply 'dired-do-search))

(defun sr-sticky-isearch-prompt ()
  "Display the message that appears when a sticky search is launched."
  (message (propertize "Sunrise sticky I-search (C-g to exit): "
                       'face 'minibuffer-prompt)))

(defvar sr-sticky-isearch-commands
  '(nil
    ("\C-o" . dired-omit-mode)
    ("\M-a" . sr-beginning-of-buffer)
    ("\M-e" . sr-end-of-buffer)
    ("\C-v" . scroll-up-command)
    ("\M-v" . (lambda () (interactive) (scroll-up-command '-)))
    ("\C-g" . (lambda () (interactive) (save-excursion (isearch-abort))))
  ) "Keybindings installed in `isearch-mode' during a sticky search.")

(defun sr-sticky-isearch-remap-commands (&optional restore)
  "Remap `isearch-mode-map' commands using `sr-sticky-isearch-commands'.
Replace the bindings in our table with the previous ones from `isearch-mode-map'
so we can restore them when the current sticky search operation finishes."
  (when (eq restore (car sr-sticky-isearch-commands))
    (setcar sr-sticky-isearch-commands (not restore))
    (mapc (lambda (entry)
            (let* ((binding (car entry))
                   (old-command (lookup-key isearch-mode-map binding))
                   (new-command (cdr entry)))
              (define-key isearch-mode-map binding new-command)
              (setcdr entry old-command)))
          (cdr sr-sticky-isearch-commands))))

(defun sr-sticky-isearch (&optional backward)
  "Concatenate Isearch operations to allow fast file system navigation.
Search continues until C-g is pressed (to abort) or Return is
pressed on a regular file (to end the operation and visit that
file)."
  (set (make-local-variable 'search-nonincremental-instead) nil)
  (add-hook 'isearch-mode-end-hook 'sr-sticky-post-isearch)
  (sr-sticky-isearch-remap-commands)
  (if backward
      (isearch-backward nil t)
    (isearch-forward nil t))
  (run-hooks 'sr-refresh-hook)
  (run-with-idle-timer 0.01 nil 'sr-sticky-isearch-prompt))

(defun sr-sticky-isearch-forward ()
  "Start a sticky forward search in the current pane."
  (interactive)
  (sr-sticky-isearch))

(defun sr-sticky-isearch-backward ()
  "Start a sticky backward search in the current pane."
  (interactive)
  (sr-sticky-isearch t))

(defun sr-sticky-post-isearch ()
  "`isearch-mode-end-hook' function for Sunrise sticky Isearch operations."
  (and
   (dired-get-filename nil t)
   (let* ((filename (expand-file-name (dired-get-filename nil t)))
          (is-dir (or (file-directory-p filename)
                      (sr-avfs-dir filename)
                      (sr-virtual-directory-p filename))))
     (cond ((or isearch-mode-end-hook-quit (not is-dir))
            (progn
              (remove-hook 'isearch-mode-end-hook 'sr-sticky-post-isearch)
              (kill-local-variable 'search-nonincremental-instead)
              (sr-sticky-isearch-remap-commands t)
              (isearch-done)
              (if isearch-mode-end-hook-quit
                  (run-hooks 'sr-refresh-hook)
                (sr-find-file filename))))
           (t
            (progn
              (sr-find-file filename)
              (set (make-local-variable 'search-nonincremental-instead) nil)
              (isearch-forward nil t)
              (run-with-idle-timer 0.01 nil 'sr-sticky-isearch-prompt)))))))

(defun sr-show-files-info (&optional deref-symlinks)
  "Enhanced version of `dired-show-file-type' from dired‐aux.
If at most one item is marked, print the filetype of the current
item according to the \"file\" command, including its size in bytes.
If more than one item is marked, print the total size in
bytes (calculated recursively) of all marked items."
  (interactive "P")
  (message "Calculating total size of selection... (C-g to abort)")
  (let* ((selection (dired-get-marked-files t))
         (size (sr-size-format (sr-files-size selection)))
         (items (length selection)) (label) (regex))
    (if (>= 1 items)
        (progn
          (setq selection (car selection)
                label (file-name-nondirectory selection)
                regex (concat "^.*" label "[:;]")
                label (concat label ":"))
          (dired-show-file-type selection deref-symlinks)
          (message
           "%s (%s bytes)"
           (replace-regexp-in-string regex label (current-message)) size))
      (message "%s bytes in %d selected items" size items))
    (sit-for 0.5)))

(eval-when-compile
  (defsubst sr-size-attr (file)
    "Helper function for `sr-files-size'."
    (float (or (nth 7 (file-attributes file)) 0))))

(defun sr-files-size (files)
  "Recursively calculate the total size of all FILES.
FILES should be a list of paths."
  (let ((result 0))
    (mapc
     (lambda (x) (setq result (+ x result)))
     (mapcar (lambda (f) (cond ((string-match "\\.\\./?$" f) 0)
                               ((string-match "\\./?$" f) (sr-size-attr f))
                               ((file-symlink-p f) (sr-size-attr f))
                               ((file-directory-p f) (sr-directory-size f))
                               (t (float (sr-size-attr f)))))
             files))
    result))

(defun sr-directory-size (directory)
  "Recursively calculate the total size of the given DIRECTORY."
  (sr-files-size (directory-files directory t nil t)))

(defun sr-size-format (size)
  "Return integer representation of SIZE (a float) as a string.
Uses comma as the thousands separator."
  (let* ((num (replace-regexp-in-string "\\..*$" "" (number-to-string size)))
         (digits (reverse (split-string num "" t)))
         result)
    (dotimes (n (length digits))
      (when (and (< 0 n) (zerop (% n 3)))
        (setq result (concat "," result)))
      (setq result (concat (pop digits) result)))
    result))

;;; ============================================================================
;;; TI (Terminal Integration) and CLEX (Command Line EXpansion) functions:

;;;###autoload
(defun sr-term (&optional cd newterm program)
  "Run terminal in a new buffer or switch to an existing one.
If the optional argument CD is non-nil, directory is changed to
the current one in the active pane. A non-nil NEWTERM argument
forces the creation of a new terminal. If PROGRAM is provided
and exists in `exec-path', then it will be used instead of the
default `sr-terminal-program'."
  (interactive)
  (let ((aterm (car sr-ti-openterms)))
    (if (and (null program)
             (or (eq major-mode 'eshell-mode)
                 (and (buffer-live-p aterm)
                      (with-current-buffer aterm
                        (eq major-mode 'eshell-mode)))))
        (setq program "eshell")
      (setq program (or program sr-terminal-program))))
  (if (memq major-mode '(sr-mode sr-virtual-mode sr-tree-mode))
      (hl-line-mode 1))
  (if (string= program "eshell")
      (sr-term-eshell cd newterm)
    (sr-term-extern cd newterm program)))

;;;###autoload
(defun sr-term-cd ()
  "Run terminal in a new buffer or switch to an existing one.
cd's to the current directory of the active pane."
  (interactive)
  (sr-term t))

;;;###autoload
(defun sr-term-cd-newterm ()
  "Open a NEW terminal (don't switch to an existing one).
cd's to the current directory of the active pane."
  (interactive)
  (sr-term t t))

;;;###autoload
(defun sr-term-cd-program (&optional program)
  "Open a NEW terminal using PROGRAM as the shell."
  (interactive "sShell program to use: ")
  (sr-term t t program))

(defmacro sr-term-excursion (cd newterm form &optional is-external)
  "Take care of the common mechanics of launching or switching to a terminal.
Helper macro."
  `(let* ((start-buffer (current-buffer))
          (new-term (or (null sr-ti-openterms) ,newterm))
          (next-buffer (or (cadr (memq start-buffer sr-ti-openterms))
                           (car sr-ti-openterms)))
          (new-name) (is-line-mode))
     (sr-select-viewer-window t)
     (if (not new-term)
         ;;don't switch anywhere else if we're in a term and we want only to cd:
         (unless (and ,cd (memq (current-buffer) sr-ti-openterms))
           (switch-to-buffer next-buffer))
       (when next-buffer
         (with-current-buffer next-buffer
           (setq is-line-mode (and (boundp 'sr-term-line-minor-mode)
                                   (symbol-value 'sr-term-line-minor-mode)))))
       ,form
       (if ,is-external (sr-term-char-mode))
       (if is-line-mode (sr-term-line-mode))
       (when (memq (current-buffer) sr-ti-openterms)
         (rename-uniquely)
         (setq new-name (buffer-name))
         ,form)
       (when new-name
         (message "Sunrise: previous terminal renamed to %s" new-name))
       (push (current-buffer) sr-ti-openterms))))

(defun sr-term-line-mode ()
  "Switch the current terminal to line mode.
Apply additional Sunrise keybindings for terminal integration."
  (interactive)
  (term-line-mode)
  (sr-term-line-minor-mode 1))

(defun sr-term-char-mode ()
  "Switch the current terminal to character mode.
Bind C-j and C-k to Sunrise terminal integration commands."
  (interactive)
  (term-char-mode)
  (sr-term-line-minor-mode 0)
  (sr-term-char-minor-mode 1))

(defun sr-term-extern (&optional cd newterm program)
  "Implementation of `sr-term' for external terminal programs.
See `sr-term' for a description of the arguments."
  (let* ((program (if program (executable-find program)))
         (program (or program sr-terminal-program))
         (dir (expand-file-name (sr-choose-cd-target)))
        (aterm (car sr-ti-openterms))
        (cd (or cd (null sr-ti-openterms)))
        (line-mode (if (buffer-live-p aterm)
                       (with-current-buffer aterm (term-in-line-mode)))))
    (sr-term-excursion cd newterm (term program) t)
    (sr-term-char-mode)
    (when (or line-mode (term-in-line-mode))
      (sr-term-line-mode))
    (when cd
      (term-send-raw-string
       (concat "cd " (shell-quote-wildcard-pattern dir) "")))))

(defun sr-term-eshell (&optional cd newterm)
  "Implementation of `sr-term' when using `eshell'."
  (let ((dir (expand-file-name (sr-choose-cd-target)))
        (cd (or cd (null sr-ti-openterms))))
    (sr-term-excursion cd newterm (eshell))
    (when cd
      (insert (concat "cd " (shell-quote-wildcard-pattern dir)))
      (eshell-send-input))
    (sr-term-line-mode)))

(defmacro sr-ti (form)
  "Evaluate FORM in the context of the selected pane.
Helper macro for implementing terminal integration in Sunrise."
  `(if sr-running
       (progn
         (sr-select-window sr-selected-window)
         (hl-line-unhighlight)
         (unwind-protect
             ,form
           (when sr-running
             (sr-select-viewer-window))))))

(defun sr-ti-previous-line ()
  "Move one line backward on active pane from the terminal window."
  (interactive)
  (sr-ti (forward-line -1)))

(defun sr-ti-next-line ()
  "Move one line forward on active pane from the terminal window."
  (interactive)
  (sr-ti (forward-line 1)))

(defun sr-ti-select ()
  "Run `dired-advertised-find-file' on active pane from the terminal window."
  (interactive)
  (sr-ti (sr-advertised-find-file)))

(defun sr-ti-mark ()
  "Run `dired-mark' on active pane from the terminal window."
  (interactive)
  (sr-ti (dired-mark 1)))

(defun sr-ti-unmark ()
  "Run `dired-unmark-backward' on active pane from the terminal window."
  (interactive)
  (sr-ti (dired-unmark-backward 1)))

(defun sr-ti-prev-subdir (&optional count)
  "Run `dired-prev-subdir' on active pane from the terminal window."
  (interactive "P")
  (let ((count (or count 1)))
    (sr-ti (sr-dired-prev-subdir count))))

(defun sr-ti-unmark-all-marks ()
  "Remove all marks on active pane from the terminal window."
  (interactive)
  (sr-ti (dired-unmark-all-marks)))

(defun sr-ti-change-window ()
  "Switch focus to the currently active pane."
  (interactive)
  (sr-select-window sr-selected-window))

(defun sr-ti-change-pane ()
  "Change selection of active pane to passive one."
  (interactive)
  (sr-ti (sr-change-window)))

(add-hook
 'kill-buffer-hook
 (defun sr-ti-cleanup-openterms ()
   "Remove the current buffer from the list of open terminals."
   (setq sr-ti-openterms (delete (current-buffer) sr-ti-openterms))))

(defun sr-ti-revert-buffer ()
  "Refresh the currently active pane."
  (interactive)
  (let ((dir default-directory))
    (if (not (sr-equal-dirs dir sr-this-directory))
        (sr-ti (sr-goto-dir dir))
      (sr-ti (sr-revert-buffer)))))

(defun sr-ti-lock-panes ()
  "Resize and lock the panes at standard position from the command line."
  (interactive)
  (sr-ti (sr-lock-panes)))

(defun sr-ti-min-lock-panes ()
  "Minimize the panes from the command line."
  (interactive)
  (sr-ti (sr-min-lock-panes)))

(defun sr-ti-max-lock-panes ()
  "Maximize the panes from the command line."
  (interactive)
  (sr-ti (sr-max-lock-panes)))

(defmacro sr-clex (pane form)
  "Evaluate FORM in the context of PANE.
Helper macro for implementing command line expansion in Sunrise."
  `(save-window-excursion
     (setq pane (if (atom pane) pane (eval pane)))
     (select-window (symbol-value (sr-symbol ,pane 'window)))
     ,form))

(defun sr-clex-marked (pane)
  "Return a string containing the list of marked files in PANE."
  (sr-clex
   pane
   (mapconcat 'shell-quote-wildcard-pattern (dired-get-marked-files) " ")))

(defun sr-clex-file (pane)
  "Return the file currently selected in PANE."
  (sr-clex
   pane
   (concat (shell-quote-wildcard-pattern (dired-get-filename)) " ")))

(defun sr-clex-marked-nodir (pane)
  "Return a list of basenames of all the files currently marked in PANE."
  (sr-clex
   pane
   (mapconcat 'shell-quote-wildcard-pattern
              (dired-get-marked-files 'no-dir) " ")))

(defun sr-clex-dir (pane)
  "Return the current directory of the given pane."
  (sr-clex
   pane
   (concat (shell-quote-wildcard-pattern default-directory) " ")))

(defun sr-clex-start ()
  "Start a new CLEX operation.
Puts `sr-clex-commit' into local `after-change-functions'."
  (interactive)
  (if sr-clex-on
      (progn
        (setq sr-clex-on nil)
        (delete-overlay sr-clex-hotchar-overlay))
    (progn
      (insert-char ?% 1)
      (if sr-running
          (progn
            (add-hook 'after-change-functions 'sr-clex-commit nil t)
            (setq sr-clex-on t)
            (setq sr-clex-hotchar-overlay (make-overlay (point) (1- (point))))
            (overlay-put sr-clex-hotchar-overlay 'face 'sr-clex-hotchar-face)
            (message
             "Sunrise: CLEX is now ON for keys: m f n d a p M F N D A P %%"))))))

(defun sr-clex-commit (&optional _beg _end _range)
  "Commit the current CLEX operation (if any).
This function is added to the local `after-change-functions' list
by `sr-clex-start'."
  (interactive)
  (if sr-clex-on
      (progn
        (setq sr-clex-on nil)
        (delete-overlay sr-clex-hotchar-overlay)
        (let* ((xchar (char-before))
               (expansion (case xchar
                            (?m (sr-clex-marked       'left))
                            (?f (sr-clex-file         'left))
                            (?n (sr-clex-marked-nodir 'left))
                            (?d (sr-clex-dir          'left))
                            (?M (sr-clex-marked       'right))
                            (?F (sr-clex-file         'right))
                            (?N (sr-clex-marked-nodir 'right))
                            (?D (sr-clex-dir          'right))
                            (?a (sr-clex-marked       '(sr-this)))
                            (?A (sr-clex-dir          '(sr-this)))
                            (?p (sr-clex-marked       '(sr-other)))
                            (?P (sr-clex-dir          '(sr-other)))
                            (t nil))))
          (if expansion
              (progn
                (delete-char -2)
                (insert expansion)))))))

(define-minor-mode sr-term-char-minor-mode
  "Sunrise Commander terminal add-on for character (raw) mode."
  nil nil
  '(("\C-c\C-j" . sr-term-line-mode)
    ("\C-c\C-k" . sr-term-char-mode)
    ("\C-c\t"   . sr-ti-change-window)
    ("\C-ct"    . sr-term)
    ("\C-cT"    . sr-term-cd)
    ("\C-c\C-t" . sr-term-cd-newterm)
    ("\C-c\M-t" . sr-term-cd-program)
    ("\C-c;"    . sr-follow-viewer)
    ("\C-c\\"   . sr-ti-lock-panes)
    ("\C-c{"    . sr-ti-min-lock-panes)
    ("\C-c}"    . sr-ti-max-lock-panes)))

(define-minor-mode sr-term-line-minor-mode
  "Sunrise Commander terminal add-on for line (cooked) mode."
  nil nil
  '(([M-up]        . sr-ti-previous-line)
    ([A-up]        . sr-ti-previous-line)
    ("\M-P"        . sr-ti-previous-line)
    ([M-down]      . sr-ti-next-line)
    ([A-down]      . sr-ti-next-line)
    ("\M-N"        . sr-ti-next-line)
    ("\M-\C-m"     . sr-ti-select)
    ("\C-\M-j"     . sr-ti-select)
    ([M-return]    . sr-ti-select)
    ([S-M-return]  . sr-ti-select)
    ("\M-M"        . sr-ti-mark)
    ([M-backspace] . sr-ti-unmark)
    ("\M-\d"       . sr-ti-unmark)
    ("\M-J"        . sr-ti-prev-subdir)
    ("\M-U"        . sr-ti-unmark-all-marks)
    ([C-tab]       . sr-ti-change-window)
    ([M-tab]       . sr-ti-change-pane)
    ("\C-c\t"      . sr-ti-change-window)
    ("\C-ct"       . sr-term)
    ("\C-cT"       . sr-term-cd)
    ("\C-c\C-t"    . sr-term-cd-newterm)
    ("\C-c\M-t"    . sr-term-cd-program)
    ("\C-c;"       . sr-follow-viewer)
    ("\M-\S-g"     . sr-ti-revert-buffer)
    ("%"           . sr-clex-start)
    ("\t"          . term-dynamic-complete)
    ("\C-c\\"      . sr-ti-lock-panes)
    ("\C-c{"       . sr-ti-min-lock-panes)
    ("\C-c}"       . sr-ti-max-lock-panes))
  :group 'sunrise)

(defadvice term-sentinel (around sr-advice-term-sentinel (proc msg) activate)
  "Take care of killing Sunrise Commander terminal buffers on exit."
  (if (and (or sr-term-char-minor-mode sr-term-line-minor-mode)
           sr-terminal-kill-buffer-on-exit
           (memq (process-status proc) '(signal exit)))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (bury-buffer buffer)
        (kill-buffer buffer))
    ad-do-it))

;;; ============================================================================
;;; Desktop support:

(defun sr-pure-virtual-p (&optional buffer)
  "Return t if BUFFER (or the current buffer if nil) is purely virtual.
Purely virtual means it is not attached to any directory or any
file in the file system."
  (with-current-buffer (if (bufferp buffer) buffer (current-buffer))
    (not (or (eq 'sr-mode major-mode)
             (and (eq 'sr-virtual-mode major-mode)
                  buffer-file-truename
                  (file-exists-p buffer-file-truename))))))

(defun sr-desktop-save-buffer (desktop-dir)
  "Return the additional data for saving a Sunrise buffer to a desktop file."
  (unless (sr-pure-virtual-p)
    (let* ((side (if (eq (current-buffer) sr-left-buffer) 'left 'right))
           (sorting-order (get side 'sorting-order))
           (sorting-reverse (get side 'sorting-reverse)))
      (apply
       'append
       (delq nil
             (list
              (if (eq major-mode 'sr-virtual-mode)
                  (list 'dirs buffer-file-truename)
                (cons 'dirs (dired-desktop-buffer-misc-data desktop-dir)))
              (cons side t)
              (if sorting-order (cons 'sorting-order sorting-order))
              (if sorting-reverse (cons 'sorting-reverse sorting-reverse))
              (if (eq major-mode 'sr-virtual-mode) (cons 'virtual t))))
       (mapcar (lambda (fun)
                 (funcall fun desktop-dir))
               sr-desktop-save-handlers)))))

(defun sr-desktop-restore-buffer (desktop-buffer-file-name
                                  desktop-buffer-name
                                  desktop-buffer-misc)
  "Restore a Sunrise (normal or VIRTUAL) buffer from its desktop file data."
  (let* ((sr-running t)
         (misc-data (cdr (assoc 'dirs desktop-buffer-misc)))
         (is-virtual (assoc 'virtual desktop-buffer-misc))
         (buffer
          (if (not is-virtual)
              (with-current-buffer
                  (dired-restore-desktop-buffer desktop-buffer-file-name
                                                desktop-buffer-name
                                                misc-data)
                (sr-mode)
                (current-buffer))
            (desktop-restore-file-buffer (car misc-data)
                                         desktop-buffer-name
                                         misc-data))))
    (with-current-buffer buffer
      (when is-virtual (set-visited-file-name nil t))
      (mapc (lambda (side)
              (when (cdr (assq side desktop-buffer-misc))
                (set (sr-symbol side 'buffer) buffer)
                (set (sr-symbol side 'directory) default-directory)
                (sr-desktop-sort buffer side desktop-buffer-misc)))
            '(left right))
      (mapc (lambda (fun)
              (funcall fun
                       desktop-buffer-file-name
                       desktop-buffer-name
                       desktop-buffer-misc))
            sr-desktop-restore-handlers))
    buffer))

(defun sr-desktop-sort (buffer side desktop-buffer-misc)
  "Restore the sorting order in BUFFER to be displayed in SIDE.
Use the data in DESKTOP-BUFFER-MISC to obtain all pertinent
details."
  (with-current-buffer buffer
    (let ((sr-selected-window side)
          (sorting-order (cdr (assoc 'sorting-order desktop-buffer-misc)))
          (sorting-reverse (cdr (assoc 'sorting-reverse desktop-buffer-misc))))
      (when sorting-order
        (condition-case nil
            (funcall (intern (format "sr-sort-by-%s" (downcase sorting-order))))
          (error (ignore))))
      (when sorting-reverse (sr-reverse-pane)))))

(defun sr-reset-state ()
  "Reset some environment variables that control the Sunrise behavior.
Used for desktop support."
  (setq sr-left-directory "~/" sr-right-directory "~/"
        sr-this-directory "~/" sr-other-directory "~/")
  (if sr-running (sr-quit))
  nil)

;; These register the previous functions in the desktop framework:
(add-to-list 'desktop-buffer-mode-handlers
             '(sr-mode . sr-desktop-restore-buffer))
(add-to-list 'desktop-buffer-mode-handlers
             '(sr-virtual-mode . sr-desktop-restore-buffer))

;; This initializes (and sometimes starts) Sunrise after desktop restoration:
(add-hook 'desktop-after-read-hook
          (defun sr-desktop-after-read-function ()
            (unless (assoc 'sr-running desktop-globals-to-clear)
              (add-to-list 'desktop-globals-to-clear
                           '(sr-running . (sr-reset-state))))
            (if (memq major-mode '(sr-mode sr-virtual-mode sr-tree-mode))
                (sunrise))))

;;; ============================================================================
;;; Miscellaneous functions:

(defun sr-buffer-files (buffer-or-name)
  "Return the list of all file names currently displayed in the given buffer."
  (with-current-buffer buffer-or-name
    (save-excursion
      (let ((result nil))
        (sr-beginning-of-buffer)
        (while (not (eobp))
          (setq result (cons (dired-get-filename t t) result))
          (forward-line 1))
        (reverse result)))))

(defun sr-keep-buffer (&optional side)
  "Keep the currently displayed buffer in SIDE (left or right) window.
Keeps it there even if it does not belong to the panel's history
ring. If SIDE is nil, use the value of `sr-selected-window'
instead. Useful for maintaining the contents of the pane during
layout switching."
  (let* ((side (or side sr-selected-window))
         (window (symbol-value (sr-symbol side 'window))))
    (set (sr-symbol side 'buffer) (window-buffer window))))

(defun sr-scrollable-viewer (buffer)
  "Set the `other-window-scroll-buffer' variable to BUFFER.
Doing so allows to scroll the given buffer directly from the active pane."
  (setq other-window-scroll-buffer buffer)
  (if buffer
      (message "QUICK VIEW: Press C-e/C-y to scroll, Space/M-Space to page, and C-u v (or C-u o) to dismiss")))

(defun sr-describe-mode ()
  "Call `describe-mode' and make the resulting buffer C-M-v scrollable."
  (interactive)
  (describe-mode)
  (sr-scrollable-viewer (get-buffer "*Help*"))
  (sr-select-window sr-selected-window))

(defun sr-equal-dirs (dir1 dir2)
  "Return non-nil if the two paths DIR1 and DIR2 represent the same directory."
  (string= (expand-file-name (concat (directory-file-name dir1) "/"))
           (expand-file-name (concat (directory-file-name dir2) "/"))))

(defun sr-summary ()
  "Summarize basic Sunrise commands and show recent Dired errors."
  (interactive)
  (dired-why)
  (message "C-opy, R-ename, K-lone, D-elete, v-iew, e-X-ecute, Ff-ollow, \
Jj-ump, q-uit, m-ark, u-nmark, h-elp"))

(defun sr-restore-point-if-same-buffer ()
  "Synchronize point position if the same buffer is displayed in both panes."
  (let ((this-win)(other-win)(point))
    (when (and (eq sr-left-buffer sr-right-buffer)
               (window-live-p (setq other-win (sr-other 'window))))
      (setq this-win (selected-window))
      (setq point (point))
      (select-window other-win)
      (goto-char point)
      (select-window this-win))))

(defun sr-mark-toggle ()
  "Toggle the mark on the current file or directory."
  (interactive)
  (when (dired-get-filename t t)
    (if (eq ?  (char-after (line-beginning-position)))
        (dired-mark 1)
      (dired-unmark 1))))

(defun sr-assoc-key (name alist test)
  "Return the key in ALIST matched by NAME according to TEST."
  (let (head (tail alist) found)
    (while (and tail (not found))
      (setq head (caar tail)
            found (and (apply test (list head name)) head)
            tail (cdr tail)))
    found))

(defun sr-quote-marked ()
  "Return current pane's selected entries quoted and space-separated as a string."
  (let (marked)
    (condition-case err
        (setq marked (dired-get-marked-files t nil nil t))
      (error (unless (string= "No file on this line" (cadr err))
               (signal (car err) (cdr err)))))
    (unless (< (length marked) 2)
      (if (eq t (car marked)) (setq marked (cdr marked)))
      (format "\"%s\"" (mapconcat 'identity marked "\" \"")))))

(defun sr-fix-listing-switches()
  "Work around a bug in Dired that makes `dired-move-to-filename' misbehave
when any of the options -p or -F is used with ls."
  (mapc (lambda (sym)
          (let ((val (replace-regexp-in-string "\\(?:^\\| \\)-[pF]*\\(?: \\|$\\)" " " (symbol-value sym))))
            (while (string-match "\\(?:^\\| \\)-[^- ]*[pF]" val)
              (setq val (replace-regexp-in-string "\\(\\(?:^\\| \\)-[^- ]*\\)[pF]\\([^ ]*\\)" "\\1\\2" val)))
            (set sym val)))
        '(sr-listing-switches sr-virtual-listing-switches))
  (remove-hook 'sr-init-hook 'sr-fix-listing-switches))
(add-hook 'sr-init-hook 'sr-fix-listing-switches)

(defun sr-chop (char path)
  "Remove all trailing instances of character CHAR from the string PATH."
  (while (and (< 1 (length path))
              (eq (string-to-char (substring path -1)) char))
    (setq path (substring path 0 -1)))
  path)

;;; ============================================================================
;;; Advice

(defun sr-ad-enable (regexp &optional function)
  "Put all or FUNCTION-specific advice matching REGEXP into effect.
If provided, only update FUNCTION itself, otherwise all functions
with advice matching REGEXP."
  (if function
      (progn (ad-enable-advice function 'any regexp)
             (ad-activate function))
    (ad-enable-regexp regexp)
    (ad-activate-regexp regexp)))

(defun sr-ad-disable (regexp &optional function)
  "Stop all FUNCTION-specific advice matching REGEXP from taking effect.
If provided, only update FUNCTION itself, otherwise all functions
with advice matching REGEXP."
  (if function
      (progn (ad-disable-advice function 'any regexp)
             (ad-update function))
    (ad-disable-regexp regexp)
    (ad-update-regexp regexp)))

(defun sunrise-commander-unload-function ()
  (sr-ad-disable "^sr-advice-"))

;;; ============================================================================
;;; Font-Lock colors & styles:

(defmacro sr-rainbow (symbol spec regexp)
  `(progn
     (defface ,symbol '((t ,spec)) "Sunrise rainbow face" :group 'sunrise)
     ,@(mapcar (lambda (m)
                 `(font-lock-add-keywords ',m '((,regexp 1 ',symbol))))
               '(sr-mode sr-virtual-mode))))

(sr-rainbow sr-html-face              (:foreground "DarkOliveGreen")        "\\(^[^!].[^d].*\\.x?html?$\\)")
(sr-rainbow sr-xml-face               (:foreground "DarkGreen")             "\\(^[^!].[^d].*\\.\\(xml\\|xsd\\|xslt?\\|wsdl\\)$\\)")
(sr-rainbow sr-log-face               (:foreground "brown")                 "\\(^[^!].[^d].*\\.log$\\)")
(sr-rainbow sr-compressed-face        (:foreground "magenta")               "\\(^[^!].[^d].*\\.\\(zip\\|bz2\\|t?[gx]z\\|[zZ]\\|[jwers]?ar\\|xpi\\|apk\\|xz\\)$\\)")
(sr-rainbow sr-packaged-face          (:foreground "DarkMagenta")           "\\(^[^!].[^d].*\\.\\(deb\\|rpm\\)$\\)")
(sr-rainbow sr-encrypted-face         (:foreground "DarkOrange1")           "\\(^[^!].[^d].*\\.\\(gpg\\|pgp\\)$\\)")

(sr-rainbow sr-directory-face         (:inherit dired-directory :bold t)    "\\(^[^!].d.*\\)")
(sr-rainbow sr-symlink-face           (:inherit dired-symlink :italic t)    "\\(^[^!].l.*[^/]$\\)")
(sr-rainbow sr-symlink-directory-face (:inherit dired-directory :italic t)  "\\(^[^!].l.*/$\\)")
(sr-rainbow sr-alt-marked-dir-face    (:foreground "DeepPink" :bold t)      "\\(^[^ *!D].d.*$\\)")
(sr-rainbow sr-alt-marked-file-face   (:foreground "DeepPink")              "\\(^[^ *!D].[^d].*$\\)")
(sr-rainbow sr-marked-dir-face        (:inherit dired-marked)               "\\(^[*!D].d.*$\\)")
(sr-rainbow sr-marked-file-face       (:inherit dired-marked :bold nil)     "\\(^[*!D].[^d].*$\\)")
(sr-rainbow sr-broken-link-face       (:inherit dired-warning :italic t)    "\\(^[!].l.*$\\)")

(provide 'sunrise-commander)

;;; sunrise-commander.el ends here
