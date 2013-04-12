(define-key global-map "\C-cx" 'sunrise)
(define-key global-map "\C-cX" 'sunrise-cd)
; reset windows layout
(define-key global-map "\C-cd" 'sr-popviewer-setup-windows)

;; this tip pretty annoying when I just want to create some sub-directories
; @see http://www.emacswiki.org/emacs/Sunrise_Commander_Tips#ReplaceDiredWithSunrise
;; (setq find-directory-functions (cons 'sr-dired find-directory-functions))


(eval-after-load 'sunrise-commander
  '(progn
     (require 'sunrise-x-buttons)
     (require 'sunrise-x-popviewer)

     ; Highlighting files with inappropriate permissions.
     (sr-rainbow sr-gorw-dir-face
                 (:background "misty rose"
                              :foreground "blue1"
                              :bold t)
                 "^..\\(d....\\(...\\)?w..*$\\)")
     (sr-rainbow sr-gorw-face
                 (:background "misty rose")
                 "^..\\(-....\\(...\\)?w..*$\\)")

     ;; start using the passive pane as a transient viewer is
     (setq sr-popviewer-select-viewer-action
           (lambda nil (let ((sr-running nil)) (other-window 1))))
     ))

;;;### (autoloads (sr-term-cd-program sr-term-cd-newterm sr-term-cd
;;;;;;  sr-term sunrise-cd sr-dired sunrise sr-virtual-mode sr-mode)
;;;;;;  "site-lisp/sunrise-commander/sunrise-commander" "site-lisp/sunrise-commander/sunrise-commander.el"
;;;;;;  (20821 36671))
;;; Generated autoloads from site-lisp/sunrise-commander/sunrise-commander.el

(autoload 'sr-mode "site-lisp/sunrise-commander/sunrise-commander" "\
Two-pane file manager for Emacs based on Dired and inspired by MC.
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

\(fn)" t nil)

(autoload 'sr-virtual-mode "site-lisp/sunrise-commander/sunrise-commander" "\
Sunrise Commander Virtual Mode. Useful for reusing find and locate results.

\(fn)" t nil)

(autoload 'sunrise "site-lisp/sunrise-commander/sunrise-commander" "\
Toggle the Sunrise Commander file manager.
If LEFT-DIRECTORY is given, the left window will display that
directory (same for RIGHT-DIRECTORY). Specifying nil for any of
these values uses the default, ie. $HOME.

\(fn &optional LEFT-DIRECTORY RIGHT-DIRECTORY FILENAME)" t nil)

(autoload 'sr-dired "site-lisp/sunrise-commander/sunrise-commander" "\
Visit the given target (file or directory) in `sr-mode'.

\(fn &optional TARGET SWITCHES)" t nil)

(autoload 'sunrise-cd "site-lisp/sunrise-commander/sunrise-commander" "\
Toggle the Sunrise Commander FM keeping the current file in focus.
If Sunrise is off, enable it and focus the file displayed in the current buffer.
If Sunrise is on, disable it and switch to the buffer currently displayed in the
viewer window.

\(fn)" t nil)
 (autoload 'sr-checkpoint-handler "sunrise-commander" "" t)

(autoload 'sr-term "site-lisp/sunrise-commander/sunrise-commander" "\
Run terminal in a new buffer or switch to an existing one.
If the optional argument CD is non-nil, directory is changed to
the current one in the active pane. A non-nil NEWTERM argument
forces the creation of a new terminal. If PROGRAM is provided
and exists in `exec-path', then it will be used instead of the
default `sr-terminal-program'.

\(fn &optional CD NEWTERM PROGRAM)" t nil)

(autoload 'sr-term-cd "site-lisp/sunrise-commander/sunrise-commander" "\
Run terminal in a new buffer or switch to an existing one.
cd's to the current directory of the active pane.

\(fn)" t nil)

(autoload 'sr-term-cd-newterm "site-lisp/sunrise-commander/sunrise-commander" "\
Open a NEW terminal (don't switch to an existing one).
cd's to the current directory of the active pane.

\(fn)" t nil)

(autoload 'sr-term-cd-program "site-lisp/sunrise-commander/sunrise-commander" "\
Open a NEW terminal using PROGRAM as the shell.

\(fn &optional PROGRAM)" t nil)

;;;***

(provide 'init-sunrise-commander)