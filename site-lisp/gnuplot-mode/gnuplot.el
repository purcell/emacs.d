;;;; gnuplot.el -- drive gnuplot from within emacs

;; Copyright (C) 1998 Phil Type and Bruce Ravel, 1999-2002 Bruce Ravel

;; Author:     Bruce Ravel <ravel@phys.washington.edu> and Phil Type
;; Maintainer: Bruce Ravel <ravel@phys.washington.edu>
;; Created:    June 28 1998
;; Updated:    December 13, 2002
;; Version:    0.6.0
;; Keywords:   gnuplot, plotting

;; This file is not part of GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This lisp script is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;
;; Permission is granted to distribute copies of this lisp script
;; provided the copyright notice and this permission are preserved in
;; all copies.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; send bug reports to the author (ravel@phys.washington.edu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; This is a major mode for composing gnuplot scripts and displaying
;; their results using gnuplot.  It is optimized for use with gnuplot
;; 3.7 or one of the later patchlevels of "version 3.6".  It should
;; also work very handily with version 3.5.  This mode offers several
;; tools to help you compose your scripts, including syntax
;; colorization using either font-lock or hilit19, a syntax table
;; appropriate to gnuplot, key bindings, pull-down menus, indentation,
;; keyword completions and variable customization using the Custom
;; package.  Once the script is composed, there are several function
;; for sending some or all of the script to gnuplot.  The interaction
;; with the gnuplot process is within a comint buffer.
;;
;;    C-c C-l       send current line to gnuplot
;;    C-c C-v       send current line to gnuplot and move forward 1 line
;;    C-c C-r       send current region to gnuplot
;;    C-c C-b       send entire buffer to gnuplot
;;    C-c C-f       send a file to gnuplot
;;    C-c C-i       insert filename at point
;;    C-c C-n       negate set option on current line
;;    C-c C-c       comment region
;;    C-c C-o       set arguments for command at point
;;   S-mouse-2      set arguments for command under mouse cursor
;;    C-c C-h       read the gnuplot info file
;;    C-c C-e       show-gnuplot-buffer
;;    C-c C-k       kill gnuplot process
;;    C-c C-u       submit a bug report about gnuplot-mode
;; M-tab or M-ret   complete keyword before point
;;      ret         newline and indent
;;      tab         indent current line
;;
;; Gnuplot-mode adds two key bindings to the comint buffer:
;;     M-C-p        plot the current script buffer line-by-line
;;     M-C-f        save the current script buffer and load that file
;;
;; These two functions are useful for starting up gnuplot-mode.
;;
;; M-x gnuplot-mode
;;         start gnuplot-mode in the current buffer
;;
;; M-x gnuplot-make-buffer
;;         open a new buffer (which is not visiting a file) and start
;;         gnuplot-mode in that buffer
;;
;; ---------------------------------------------------------------------
;;
;; Other lisp files used by gnuplot.el
;;
;; info-look.el (comes with GNU Emacs 20):
;;   This provides the interface to the gnuplot-info file and provides
;;   on-line help and keyword completion functionality.  The version
;;   of info-look.el that comes with version 20.2 of Emacs contains a
;;   bug that will impede its interaction with the gnuplot info file.
;;   You should use the version from the gnuplot-mode homepage
;;   instead.  info-look is not distributed with XEmacs and so should
;;   be installed along with gnuplot-mode when using XEmacs.
;;
;; gnuplot-gui.el (written by Bruce):
;;   Defines the GUI interface for setting setting arguments to
;;   gnuplot options.  This uses the widget package extensively.
;;
;; ---------------------------------------------------------------------
;;
;; This mode was inspired by the original gnu-plot-mode by Gershon
;; Elber, which is distributed with gnuplot itself and which dates
;; back to the early 90's.  Although this mode encompasses the
;; functionality of the original, the two share no code and the
;; current implementation takes advantage of many features of modern
;; versions of emacs and adheres (or so I intend) to the major mode
;; conventions described in the emacs-lisp reference for version 19
;; and later.
;;
;; ---------------------------------------------------------------------
;;
;;                         Installation
;;                         ============
;;
;; A recent version of this file can be found at
;;   http://feff.phys.washington.edu/~ravel/software/gnuplot-mode/
;;
;; To autoload gnuplot-mode on any file with gp extension, put this in
;; your .emacs file
;;   (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
;;   (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
;;
;; Something like
;;   (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode))
;;			           auto-mode-alist))
;; is useful for having files ending in .gp start up in gnuplot-mode.
;;
;; Something like
;;   (global-set-key [(f9)] 'gnuplot-make-buffer)
;; may be useful.  This binds f9 to the function that opens a scratch
;; buffer (i.e. one that is not visiting a file) in gnuplot-mode.
;; This is handy for your quick 'n' dirty plotting chores.
;;
;; To use the `gnuplot-info-lookup-symbol' function, the file
;; gnuplot.info MUST be installed somewhere that info can find it.
;; This means you must either:
;;   1.  Copy gnuplot.info to the normal info directory or
;;   2.  Make sure info can find gnuplot.info by putting this in your
;;       .emacs file:
;;         (setenv "INFOPATH"
;;	      (concat (getenv "INFOPATH") ":"
;;                    (expand-file-name "/path/to/file")))
;;       where "/path/to/file" is the location of gnuplot.info
;;
;; This had been tested extensively with Emacs 19.34 and 20.2 and
;; XEmacs 20.3 and in a limited manner with Emacs 19.30 and XEmacs
;; 19.14.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History:
;;
;;  0.1   Jun 25 1998 Finished with initial release.
;;  0.2   Sep  4 1998 Added filename insertion, indentation, and
;;        colorization/completion in comint buffer. <BR>
;;  0.2a  Sep 11 1998 made `indent-line-function' buffer-local (whoops!)
;;        and fixed some stuff in the installation script <BR>
;;  0.3   Sep 12 1998 include insertions menu <BR>
;;  0.3a  Sep 14 1998 fixed bug finding info file if missing, fixed bug
;;        starting font-lock, fixed bug re overwriting files in
;;        installation script <BR>
;;  0.3b  Sep 15 1998 Added (require 'info) to `(eval-and-compile'
;;        clause, Added (kill-all-local-variables) to `gnuplot-mode',
;;        altered order of:-
;;            (provide 'gnuplot)
;;            (run-hooks 'gnuplot-load-hook)
;;        at the end of the file in case something in the load hook
;;        requires gnuplot (oh not that old one again...), added
;;        `gnuplot-comint-setup-hook', corrected `gnuplot-mark-active'
;;        which caused an error to be raised by (mark) when the mark
;;        was inactive <DB>  Some changes to font-lock rules <LB>&<BR>
;;  0.4   Nov 14 1998 <BR> Use info-look for info interface.  No
;;        change to gnuplot-mode user interface, but cleaner code.
;;        With info-look, the help funcion works regardless of the
;;        version number of gnuplot.  Also, `gnuplot-keywords' (used
;;        for help, keyword-completion, and hilit19 highlighting) is
;;        now generated automatically.
;;  0.4a  Nov 18 1998 <BR> info-look leaves a couple of really useless
;;        buffers lying around so I cleaned them up.  Also fixed
;;        font-lock rules so that things in quotes get highlighted
;;        correctly and the surrounding text is unhighlighted.  Fixed
;;        up font-lock rules for plot and splot.  Added
;;        `gnuplot-send-line-and-forward' as suggested by <MD>.
;;  0.4b  Nov 21 1998 <BR> added toolbar for xemacs -- see file
;;        gnuplot-toolbar.el.  fixed error message in plot line
;;        function when line is empty.  option added to display the
;;        comint buffer showing the gnuplot process in a separate
;;        frame
;;  0.4c  Minor stuff: Nov 30 1998 <BR> fixed highlighting in comint
;;        buffer.  fixed frame behavior.  added "[:]" to range
;;        insertions.  added :link to defgroup.  Dec 1 1998 <BR> fixed
;;        some mismatched defcustoms.  added a few lines to suppress
;;        some compile-time warnings.  Dec 3 1998 <BR> Fixed behavior
;;        of filename insertion function.  Added more :links to
;;        defgroup.
;;  0.4d  Dec 6 1998 <BR> Added function gnuplot-setup-info-look and
;;        variable gnuplot-info-hook to handle various versions of the
;;        gnuplot info file.
;;  0.4e  Dec 12 1998 <BR> Split up gnuplot-insertions-menu for ease of
;;        custimization, put menubar initialization in a function.
;;  0.4f  Dec 14 1998 <BR> defcustom the insertions submenus, add
;;        gnuplot-insertion-menu-flag, intelligent Makefile knows
;;        which info-look to compile
;;  0.5   Dec 27 1998 <BR> wrote initial draft of gnuplot-gui.el,
;;        included it in insertions menu and in `gnuplot-insert'.
;;        Negate option function, bound to C-c C-n. Dec 29 1998 <BR>
;;        C-c C-h with no response goes to Commands menu.  Transparent
;;        toolbar icons.  Replace kw-compl with a simple completion
;;        function.  Put gnuplot-toolbar code in gnuplot.el.
;;  0.5a  Jan 23 1999 <BR> send file uses the load command.  add
;;        gnuplot-plot-from-comint and
;;        gnuplot-save-and-plot-from-comint and keybindings in the
;;        comint buffer.  do (process-kill-without-query
;;        gnuplot-process nil).  `gnuplot-negate-option' checks if set
;;        option has a negated form.
;;  0.5b  `gnuplot-kill-gnuplot-buffer' made more robust.  fixed a bug
;;        in `gnuplot-plot-from-comint'.  fixed description of
;;        gnuplot-faces group.
;;  0.5c  update copyright information, update gpelcard
;;  0.5d  Mar 20 1999 <BR> adopt installation materials from <LH>.  Add
;;        some support for hidden3d.  Use constants in types alists in
;;        gui.  Various other minor improvements to the types alists.
;;  0.5e  Apr 6 1999 <BR> at the suggestion of <SE> I did away with the
;;        gnuplot-which-highlight variable and revamped how
;;        colorization gets turned on.  This is a bit tricky since I
;;        want it to work with font-lock under emacs and xemacs and
;;        with hilit19.  Apr 11 1999 <BR> insert space at end of
;;        unique completion.  add a few GUI types, rewrite some stuff
;;        in the GUI interface.  primitive support for plot, splot,
;;        and fit.  Fixed completion in file widget.
;;  0.5f  May 15 1999 <BR> Add pgnuplot.c and Win9x install instructions
;;        to the distribution.  Fixed a defface bug.  Added
;;        `gnuplot-keywords-when' allowing deferral of parsing the
;;        info file.
;;  0.5g  May 27 1999 <BR> Fixed font-locking of strings and
;;        comments.  Figure out gnuplot-version number from startup
;;        message and set `gnuplot-echo-command-line-flag'
;;        accordingly.  Added `gnuplot-program-version' variable.
;;        Check that font-lock is actually a feature, as suggested by
;;        <KL>
;;  0.5h  Aug 15 1999 <BR> Added `gnuplot-determine-gnuplot-version' so
;;        that the gnuplot version number and `comint-process-echos'
;;        actually get set correctly.  Actually, the first time
;;        something is plotted, the echoing might not work, but the
;;        second time it will.
;;  0.5i  Sep  2 1999 <BR> Once again changed how
;;        `comint-process-echos' gets set.  Maybe I got it right this
;;        time?  Also fixed certain situations where the info file
;;        did notget properly loaded (insertion with info toggle on
;;        and info button in GUI).
;;  0.5j  Sep  9 1999 <BR> Do a more robust check for the gnuplot
;;        process before killing the gnuplot buffer, as suggested by
;;        <SE>.
;;  0.5k  Sep 22 1999 <BR> make `gnuplot-send-line-and-forward' skip
;;        over blank and comment lines as suggested by <SE>.  Jan 10
;;        2000 Bound C-c C-j to `gnuplot-forward-script-line'.
;;  0.5l  Nov 16 2000 <BR> support for pm3d in gnuplot-gui and in plot
;;        options insertions menu.  mentioned pm3d in gpelcard. gui
;;        support for x11 pm3d and vgagl terms.
;;        `gnuplot-negate-option' works with new syntax.
;;  0.5m  Nov 17 2000 <BR> add colorization and gui support for new
;;        commands in 3.8.  gui support for emf term. gui support for
;;        new "set style" syntax.  much better scheme for determining
;;        gnuplot version number + make better use of it.
;;  0.5n  Jan 4 2001 <BR> corrected a serious problem interacting with
;;        speedbar
;;  0.5o  skipped
;;  0.5p  Mar 14 2001 <BR> fixed problem with toolbar creation and
;;        speedbar clicking
;;  0.5q  May 30 2001 <BR> added font-lock bindings for words associated
;;        with plotting
;;  0.5r  Oct 17 2001 <BR> Incorporate two suggestions by <RF>, bind
;;        C-c C-c to comment-region and C-c C-o to the GUI, also make
;;        C-c C-l respect continuation lines
;;        April 12, 2002 <BR> added feature to trim length of gnuplot
;;        process buffer
;;  0.5s  Jun 7 2002 <BR> Yet again changed how `comint-process-echos'
;;        gets set.  It really needs to be nil on NTEmacs 21.1 or
;;        comint gets stuck in an infinate loop.
;;  0.5t  Sep 16 2002 <BR> Fixed a problem with C-c C-v jumping
;;        forward 2 lines at a time
;;  0.6.0 Dec 13 2002 <BR> Changed numbering scheme to accommodate
;;        gnuplot packaging requirements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Acknowledgements:
;;    David Batty       <DB> (numerous corrections)
;;    Laurent Bonnaud   <LB> (suggestions regarding font-lock rules)
;;    Markus Dickebohm  <MD> (suggested `gnuplot-send-line-and-forward')
;;    Stephen Eglan     <SE> (suggested the use of info-look,
;;                            contributed a bug fix regarding shutting
;;                            down the gnuplot process, improvement to
;;                            `gnuplot-send-line-and-forward')
;;    Robert Fenk       <RF> (suggested respecting continuation lines)
;;    Michael Karbach   <MK> (suggested trimming the gnuplot process buffer)
;;    Alex Chan Libchen <AL> (suggested font-lock for plotting words)
;;    Kuang-Yu Liu      <KL> (pointed out buggy dependence on font-lock)
;;    Hrvoje Niksic     <HN> (help with defcustom arguments for insertions)
;;    Andreas Rechtsteiner <AR> (pointed out problem with C-c C-v)
;;    Michael Sanders   <MS> (help with the info-look interface)
;;    Jinwei Shen       <JS> (suggested functionality in comint buffer)
;;    Michael M. Tung   <MT> (prompted me to add pm3d support)
;;    Holger Wenzel     <HW> (suggested using `gnuplot-keywords-when')
;;    Wolfgang Zocher   <WZ> (pointed out problem with gnuplot-mode + speedbar)
;;  and especially to Lars Hecking <LH> for including gnuplot-mode
;;  with the gnuplot 3.7-beta distribution and for providing me with
;;  installation materials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; To Do:
;;
;; 1. Since `gnuplot-display-process' can be nil, it would be
;;    handy to have a function to put on
;;    `gnuplot-after-plot-buffer-hook' to check and see if the script
;;    executed properly.  Alas I am not sure how gnuplot signals its
;;    errors.
;; 2. improve plot, splot, fit in GUI
;; 3. interface to setting bind command using `read-key-sequence'.
;;    this is a pain because the nomenclature is really different in
;;    gnuplot than in `read-key-sequence'
;;
;;; Bugs:
;;
;; -- indentation is not quite right (but close)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'comint)
(require 'easymenu)


;;; --- variable definitions + eval-and-compile clauses

;; handle defcustom
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (if (fboundp 'defgroup)
        nil
      (defmacro defgroup (&rest args)
        nil))
    (if (fboundp 'defface)
        nil
      (defmacro defface (var values doc &rest args)
        (` (progn
             (defvar (, var) (quote (, var)))
             ;; To make colors for your faces you need to set your .Xdefaults
             ;; or set them up ahead of time in your .emacs file.
             (make-face (, var))
             ))))
    (if (fboundp 'defcustom)
        nil
      (defmacro defcustom (var value doc &rest args)
        (` (defvar (, var) (, value) (, doc)))))))

;; (eval-and-compile
;;   (condition-case ()
;;       (require 'kw-compl)
;;     (error nil)))
(eval-and-compile  ;; <DB>
  (require 'info))
(eval-and-compile
  (condition-case ()
      (require 'info-look)
    (error nil)))
;; this just gets rid of an annoying compile time error message
;; (eval-when-compile
;;   (defun gnuplot-dummy ())
;;   (defalias 'hilit-set-mode-patterns 'gnuplot-dummy))


(defconst gnuplot-xemacs-p (string-match "XEmacs" (emacs-version)))
(defconst gnuplot-ntemacs-p (string-match "msvc" (emacs-version)))
(defvar   gnuplot-three-eight-p "")

(defconst gnuplot-maintainer "Bruce Ravel")
(defconst gnuplot-maintainer-email "ravel@phys.washington.edu")
(defconst gnuplot-maintainer-url
  "http://feff.phys.washington.edu/~ravel/software/gnuplot-mode/")
(defconst gnuplot-version "0.6.0")

(defgroup gnuplot nil
  "Gnuplot-mode for Emacs."
  :prefix "gnuplot-"
  :group 'processes
  :group 'applications
  :group 'local
  :link '(emacs-library-link :tag "Lisp File" "gnuplot.el")
  :link '(url-link :tag "Homepage"
		   "http://feff.phys.washington.edu/~ravel/software/gnuplot-mode/")
  :link '(custom-manual "(gnuplot)Top")
  :link '(emacs-commentary-link :tag "Commentary" "gnuplot.el") )
(defgroup gnuplot-insertions nil
  "Insert commands into gnuplot-scripts from a pull-down menu."
  :prefix "gnuplot-insertions-"
  :group 'gnuplot)
(defgroup gnuplot-hooks nil
  "Hook variables used by `gnuplot-mode'."
  :prefix "gnuplot-"
  :group 'gnuplot)

(defcustom gnuplot-mode-hook nil
  "*Hook run when `gnuplot-mode' is entered."
  :group 'gnuplot-hooks
  :type 'hook)
(defcustom gnuplot-load-hook nil
  "*Hook run when gnuplot.el is first loaded."
  :group 'gnuplot-hooks
  :type 'hook)
(defcustom gnuplot-after-plot-hook nil
  "*Hook run after gnuplot plots something.
This is the last thing done by the functions for plotting a line, a
region, a buffer, or a file."
  :group 'gnuplot-hooks
  :type 'hook)
(defcustom gnuplot-info-hook nil
  "*Hook run before setting up the info-look interface.
This hook is necessary to handle inconsistencies in versions of and
sources of the gnuplot info file.  If Gnuplot-mode can find the info
file generated from the 3.6beta patchlevel 347 (or later) release of
Gnuplot, then this hook probably is not necessary.  Some versions of
the info file may have a General Index session, which can be used by
info-look.  In that case the following (or something similar with the
value of `info-lookup-symbol-alist' altered appropriately) should be
placed in the .emacs file.

Emacs version 20.2 ships with a different version of info-look that
does 20.3.  If you use any version of Emacs 19, you must use the
version from 20.2.  Any version of XEmacs 20 or 21 should use the
version from 20.3 but can use either.  XEmacs 19 should use the
version 20.2.

For the newer version of info-look, do this:

  (add-hook \'gnuplot-info-hook
	    \'(lambda ()
	       (let ((elem (assoc \'gnuplot-mode info-lookup-alist)))
	         (delete elem info-lookup-alist)
		 (info-lookup-maybe-add-help
		  :mode 'gnuplot-mode :topic 'symbol
		  :regexp \"[a-zA-Z][_a-zA-Z0-9]*\"
		  :doc-spec '((\"(gnuplot)General Index\" nil
			       \"[_a-zA-Z0-9]+\"))))))

For the older version of info-look, do this:

  (add-hook \'gnuplot-info-hook
	    \'(lambda ()
	       (let ((elem (assoc \'gnuplot-mode info-lookup-alist)))
	         (delete elem info-lookup-alist)
	         (setq info-lookup-alist
		       (append info-lookup-alist
			       \'((gnuplot-mode
			          \"[a-zA-Z][_a-zA-Z0-9]*\" nil
			          ((\"(gnuplot)General Index\" nil
				    \"[_a-zA-Z0-9]+\" )))))))))"
  :group 'gnuplot-hooks
  :type 'hook)
;; comint hook suggested by <DB>
(defcustom gnuplot-comint-setup-hook nil
  "*Hook run after setting up the gnuplot buffer in comint mode.
So the configuration can be customised by the user."
  :group 'gnuplot-hooks
  :type 'hook)
(defvar gnuplot-recently-sent nil
  "This is a record of the most recent kind of text sent to gnuplot.
It takes as its value nil, 'line, 'region, 'buffer, or 'file.  It is
useful for functions included in `gnuplot-after-plot-hook'.")
(make-variable-buffer-local 'gnuplot-recently-sent)

(defcustom gnuplot-program "gnuplot"
  "*The name of the gnuplot executable."
  :group 'gnuplot
  :type 'string)
(defvar gnuplot-program-version nil
  "Version number of gnuplot.
This is found using `gnuplot-determine-gnuplot-version")
(defcustom gnuplot-process-name "gnuplot"
  "Name given to the gnuplot buffer and process."
  :group 'gnuplot
  :type 'string)
(defvar gnuplot-buffer nil
  "*The name of the buffer displaying the gnuplot process.")
(defvar gnuplot-process nil
  "Variable holding the process handle.")
(defvar gnuplot-process-frame nil
  "The frame for displaying the gnuplot process.
This is used when `gnuplot-display-process' is equal to 'frame.")
(defvar gnuplot-comint-recent-buffer nil
  "The most recently plotted gnuplot script buffer.
This is used by the function that plot from the comint buffer.  It is
reset every time something is plotted from a script buffer.")

(defcustom gnuplot-gnuplot-buffer "plot.gp"
  "*The name of the gnuplot scratch buffer opened by 'gnuplot-make-buffer'."
  :group 'gnuplot
  :type 'string)

(defcustom gnuplot-display-process 'window
  "This controls how the gnuplot process buffer is displayed.
The values are
   'frame    display gnuplot process in a separate frame
   'window   display gnuplot process in this frame but in another window
   nil       `gnuplot-process' is in the current frame but not displayed"
  :group 'gnuplot
  :type '(radio (const :tag "Separate frame"  frame)
		(const :tag "Separate window" window)
		(const :tag "Not displayed"   nil)))
(defcustom gnuplot-info-display 'window
  "*Determines how `gnuplot-info-lookup-symbol' displays the info file.
The values are
   'frame    display info file in a separate frame
   'window   display info file in another window
   nil       display info file in the current window"
  :group 'gnuplot
  :type '(radio (const :tag "Separate frame"  frame)
		(const :tag "Separate window" window)
		(const :tag "This window"     nil)))

(defcustom gnuplot-echo-command-line-flag (not gnuplot-ntemacs-p)
  "*This sets the fall-back value of `comint-process-echos'.
If `gnuplot-mode' cannot figure out what version number of gnuplot
this is, then the value of this variable will be used for
`comint-process-echos'.  It seems that gnuplot 3.5 wants this to be
nil and 3.7 wants it to be t.  If lines that you send to gnuplot from
the `gnuplot-mode' buffer are not appearing at the gnuplot prompt in
the process buffer, try toggling it.  Also see the document string for
`comint-process-echos'.  If you change this, kill the gnuplot process
and start it again."
  :group 'gnuplot
  :type 'boolean)
(defcustom gnuplot-insertions-show-help-flag nil
  "*Non-nil means to display certain help messages automatically.
These messages are shown after menu insertion of gnuplot commands."
  :group 'gnuplot-insertions
  :type 'boolean)

(defcustom gnuplot-delay 0.01
  "*Amount of time to delay before sending a new line to gnuplot.
This is needed so that the the line is not written in the gnuplot
buffer in advance of its prompt.  Increase this number if the
prompts and lines are displayed out of order."
  :group 'gnuplot
  :type 'number)
(defcustom gnuplot-buffer-max-size 1000
  "*The maximum size in lines of the gnuplot process buffer.
Each time text is written in the gnuplot process buffer, lines are
trimmed from the beginning of the buffer so that the buffer is this
many lines long.  The lines are deleted after the most recent lines
were interpretted by gnuplot.  Setting to 0 turns off this feature
(i.e. no lines get trimmed)."
  :group 'gnuplot
  :type 'integer)
(defcustom gnuplot-quote-character "\'"
  "*Quotation character used for inserting quoted strings.
Gnuplot can use single or double quotes.  If you prefer to have the
filename insertion function never insert quotes for you, set this
to the empty string."
  :group 'gnuplot
  :type '(radio (const :tag "double quote"  "\"")
		(const :tag "single quote"  "\'")
		(const :tag "none"          ""  )))
;; (defcustom gnuplot-gnuplot-version nil
;;   "*Force gnuplot-mode to behave for this version of gnuplot."
;;   :group 'gnuplot
;;   :type '(radio (const :tag "unspecified"   nil)
;; 		(const :tag "3.8 or newer" "3.8")
;; 		(const :tag "3.7 or older" "3.7")))

(defvar gnuplot-info-frame nil)
(defvar gnuplot-info-nodes '())

(defvar gnuplot-first-call t)

;; with info-look, there is no need to carry this list around -- it
;; can be generated on the fly appropriate to the currently installed
;; version of gnuplot.info
(defvar gnuplot-keywords nil
  "A list of keywords used in GNUPLOT.
These are set by `gnuplot-set-keywords-list' from the values in
`info-lookup-cache'.")
(defvar gnuplot-keywords-pending t	;; <HW>
  "A boolean which gets toggled when the info file is probed.")
(defcustom gnuplot-keywords-when 'deferred ;; 'immediately
  "This variable controls when the info file is parsed.
The choices are immediately upon starting gnuplot-mode or the first
time that data is needed.  If you use hilit19, then the info file is
parsed immediately regardless of the value of this variable.  But
you're not using that musty old thing, are you..."
  :group 'gnuplot
  :type
  '(radio (const :tag "Parse info file when gnuplot-mode starts"    immediately)
	  (const :tag "Parse info file the first time it is needed" deferred)))

(defgroup gnuplot-faces nil
  "Text faces used by gnuplot-mode."
  :prefix "gnuplot-"
  :group 'gnuplot)

(cond ((and (featurep 'custom) (fboundp 'custom-declare-variable))
       (defface gnuplot-prompt-face '((((class color))
				       (:foreground "firebrick"))
				      (t
				       (:bold t :underline t)))
	 "Face used for the prompt in the gnuplot process buffer."
	 :group 'gnuplot-faces))
      (t
       (make-face 'gnuplot-prompt-face)
       (set-face-foreground 'gnuplot-prompt-face "firebrick")))


;;; --- key bindings and menus

(defvar gnuplot-mode-map nil)
(if gnuplot-mode-map
    ()
  (setq gnuplot-mode-map (make-sparse-keymap))
  (define-key gnuplot-mode-map "\C-c\C-b" 'gnuplot-send-buffer-to-gnuplot)
  (define-key gnuplot-mode-map "\C-c\C-c" 'comment-region) ; <RF>
  (define-key gnuplot-mode-map "\C-c\C-o" 'gnuplot-gui-set-options-and-insert)
  (define-key gnuplot-mode-map "\C-c\C-d" 'gnuplot-show-version)
  (define-key gnuplot-mode-map "\C-c\C-e" 'gnuplot-show-gnuplot-buffer)
  (define-key gnuplot-mode-map "\C-c\C-f" 'gnuplot-send-file-to-gnuplot)
  (define-key gnuplot-mode-map "\C-c\C-h" 'gnuplot-info-lookup-symbol)
  (define-key gnuplot-mode-map "\C-c\C-i" 'gnuplot-insert-filename)
  (define-key gnuplot-mode-map "\C-c\C-j" 'gnuplot-forward-script-line)
  (define-key gnuplot-mode-map "\C-c\C-k" 'gnuplot-kill-gnuplot-buffer)
  (define-key gnuplot-mode-map "\C-c\C-l" 'gnuplot-send-line-to-gnuplot)
  (define-key gnuplot-mode-map "\C-c\C-n" 'gnuplot-negate-option)
  (define-key gnuplot-mode-map "\C-c\C-p" 'gnuplot-show-gnuplot-version)
  (define-key gnuplot-mode-map "\C-c\C-r" 'gnuplot-send-region-to-gnuplot)
  ;;(define-key gnuplot-mode-map "\C-c\C-t" 'gnuplot-gui-swap-simple-complete)
  (define-key gnuplot-mode-map "\C-c\C-u" 'gnuplot-bug-report)
  (define-key gnuplot-mode-map "\C-c\C-v" 'gnuplot-send-line-and-forward)
  (define-key gnuplot-mode-map "\C-c\C-z" 'gnuplot-customize)
  (define-key gnuplot-mode-map "\M-\r"    'gnuplot-complete-keyword)
  (define-key gnuplot-mode-map "\M-\t"    'gnuplot-complete-keyword)
  (define-key gnuplot-mode-map "\C-i"     'indent-for-tab-command)
  (define-key gnuplot-mode-map "\C-m"     'newline-and-indent)
  ;;(define-key gnuplot-mode-map "\C-m"     'reindent-then-newline-and-indent)
  ;;(if (featurep 'kw-compl)
  ;;    (define-key gnuplot-mode-map "\M-\r" 'kw-compl-abbrev)))
  (cond (gnuplot-xemacs-p
	 (define-key gnuplot-mode-map '(shift button2)
	   'gnuplot-gui-mouse-set))
	(t
	 (define-key gnuplot-mode-map [S-mouse-2]
	   'gnuplot-gui-mouse-set))) )

(defvar gnuplot-mode-menu nil)
(defvar gnuplot-menu nil
  "Menu for `gnuplot-mode'.")
(setq gnuplot-menu
      '("Gnuplot"
	["Send line to gnuplot"             gnuplot-send-line-to-gnuplot   t]
	["Send line & move forward"         gnuplot-send-line-and-forward (not (eobp))]
	["Send region to gnuplot"           gnuplot-send-region-to-gnuplot
	 (gnuplot-mark-active)]
	["Send buffer to gnuplot"           gnuplot-send-buffer-to-gnuplot t]
	["Send file to gnuplot"             gnuplot-send-file-to-gnuplot t]
	"---"
	["Insert filename at point"         gnuplot-insert-filename t]
	["Negate set option"                gnuplot-negate-option t]
	;;["Set key binding"             gnuplot-set-binding gnuplot-three-eight-p]
	["Keyword help"                     gnuplot-info-lookup-symbol
	 (or gnuplot-keywords gnuplot-keywords-pending)]
	["Show gnuplot process buffer"      gnuplot-show-gnuplot-buffer t]
	["Set arguments at point"           gnuplot-gui-set-options-and-insert
	 (fboundp 'gnuplot-gui-set-options-and-insert)]
	["Swap plot/splot/fit lists in GUI" gnuplot-gui-swap-simple-complete
	 (fboundp 'gnuplot-gui-swap-simple-complete)]
	"---"
	["Customize gnuplot"                gnuplot-customize t]
	["Submit bug report"                gnuplot-bug-report t]
	["Show gnuplot-mode version"        gnuplot-show-version t]
	["Show gnuplot version"             gnuplot-show-gnuplot-version t]
	"---"
	["Kill gnuplot"                     gnuplot-kill-gnuplot-buffer t]
	))


;;; --- insertions variables and menus

;;(load-library "gnuplot-insertions")
(defvar gnuplot-mode-insertions-menu nil)
(defvar gnuplot-insertions-menu nil
  "Menu for insertions in `gnuplot-mode'.

The insertions menu is composed of several sub-menus.  The variables
describing the sub-menus are:
  `gnuplot-insertions-adornments'
  `gnuplot-insertions-plot-options'
  `gnuplot-insertions-terminal'
  `gnuplot-insertions-x-axis'
  `gnuplot-insertions-y-axis'
  `gnuplot-insertions-z-axis'
  `gnuplot-insertions-x2-axis'
  `gnuplot-insertions-y2-axis'
  `gnuplot-insertions-parametric-plots'
  `gnuplot-insertions-polar-plots'
  `gnuplot-insertions-surface-plots'
These variables can be customized by the user.  For example, there are
many terminal types which are not in the terminal submenu but which
may be compiled into a user's copy of gnuplot.

Each of these variables is a list whose first element is a string and
all the rest are vectors as described in the document string for
`easy-menu-define'.  The callback used throughout these menus is
`gnuplot-insert' which inserts the appropriate set expression and,
optionally, looks up that item in the gnuplot info file.

The easiest way to customize the submenus is to use the custom
package.  Just type \\[gnuplot-customize] and follow your nose.

You can also add new items to any of these sub-menus by adding to the
`gnuplot-load-hook' in your .emacs file.  Here is an example of adding
the \"regis\" terminal type to the terminal sub-menu:

 (add-hook
  'gnuplot-load-hook
  '(lambda ()
      (setq gnuplot-insertions-terminal
	    (append gnuplot-insertions-terminal
		    (list
		     [\"regis\"
                      (gnuplot-insert \"set terminal regis\")
                       t])))))")

(defvar gnuplot-insertions-top ()
  "Top part of insertions menu.
See the document string for `gnuplot-insertions-menu'")

(defcustom gnuplot-insertions-menu-flag t
  "*Non-nil means to place the insertion menu in the menubar.
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type 'boolean)

(defcustom gnuplot-insertions-adornments ; this is icky...
  (if gnuplot-three-eight-p
      '("adornments"
	["arrow"       (gnuplot-insert "set arrow ")	      t]
	["bar"	       (gnuplot-insert "set bar")	      t]
	["border"      (gnuplot-insert "set border")	      t]
	["boxwidth"    (gnuplot-insert "set boxwidth ")	      t]
	["format"      (gnuplot-insert "set format ")	      t]
	["grid"	       (gnuplot-insert "set grid")	      t]
	["key"	       (gnuplot-insert "set key ")	      t]
	["label"       (gnuplot-insert "set label ")	      t]
	["pointsize"   (gnuplot-insert "set pointsize ")      t]
	["samples"     (gnuplot-insert "set samples ")	      t]
	["size"        (gnuplot-insert "set size ")	      t]
	["style"       (gnuplot-insert "set style ")          t]
	["tics"        (gnuplot-insert "set tics ")	      t]
	["timefmt"     (gnuplot-insert "set timefmt ")        t]
	["timestamp"   (gnuplot-insert "set timestamp ")      t]
	["title"       (gnuplot-insert "set title ")	      t]
	["zeroaxis"    (gnuplot-insert "set zeroaxis")        t] )
    '("adornments"
      ["data style"     (gnuplot-insert "set data style ")     t]
      ["function style" (gnuplot-insert "set function style ") t]
      ["arrow"          (gnuplot-insert "set arrow ")	       t]
      ["bar"	        (gnuplot-insert "set bar")	       t]
      ["border"         (gnuplot-insert "set border")	       t]
      ["boxwidth"       (gnuplot-insert "set boxwidth ")       t]
      ["format"         (gnuplot-insert "set format ")	       t]
      ["grid"	        (gnuplot-insert "set grid")	       t]
      ["key"	        (gnuplot-insert "set key ")	       t]
      ["label"          (gnuplot-insert "set label ")	       t]
      ["pointsize"      (gnuplot-insert "set pointsize ")      t]
      ["samples"        (gnuplot-insert "set samples ")	       t]
      ["size"           (gnuplot-insert "set size ")	       t]
      ["tics"           (gnuplot-insert "set tics ")	       t]
      ["timefmt"        (gnuplot-insert "set timefmt ")        t]
      ["timestamp"      (gnuplot-insert "set timestamp ")      t]
      ["title"          (gnuplot-insert "set title ")	       t]
      ["zeroaxis"       (gnuplot-insert "set zeroaxis")        t] ))
  "Adornments submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
	:group 'gnuplot-insertions
	:type '(list (string :tag "Title")
		     (repeat :inline t
			     (vector (string   :tag "Name")
				     (function :tag "Callback")
				     (boolean  :tag "Enabled" t)))))



(defcustom gnuplot-insertions-plot-options
  '("plot options"
    ["autoscale"  (gnuplot-insert "set autoscale ")	     t]
    ["clip"	  (gnuplot-insert "set clip ")		     t]
    ["encoding"   (gnuplot-insert "set encoding ")	     t]
    ["locale"     (gnuplot-insert "set locale ")	     t]
    ["logscale"	  (gnuplot-insert "set logscale ")	     t]
    ["multiplot"  (gnuplot-insert "set multiplot")	     t]
    ["missing"	  (gnuplot-insert "set missing \"\"")	     t]
    ["palette"    (gnuplot-insert "set palette ")            t]	; <MT>
    ["pm3d"       (gnuplot-insert "set pm3d ")               t]
    ["offsets"	  (gnuplot-insert "set offsets ")	     t]
    ["output"     (gnuplot-insert "set output ")	     t]
    ["zero"	  (gnuplot-insert "set zero ")		     t] )
  "Plot options submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
	       (repeat :inline t
		       (vector (string   :tag "Name")
			       (function :tag "Callback")
			       (boolean  :tag "Enabled" t)))))


(defcustom gnuplot-insertions-terminal
  '("terminal"
    ["eepic"      (gnuplot-insert "set terminal eepic")      t]
    ["fig"        (gnuplot-insert "set terminal fig")        t]
    ["gpic"       (gnuplot-insert "set terminal gpic")       t]
    ["latex"      (gnuplot-insert "set terminal latex")      t]
    ["linux"      (gnuplot-insert "set terminal linux")      t]
    ["pbm"        (gnuplot-insert "set terminal pbm")        t]
    ["png"        (gnuplot-insert "set terminal png")        t]
    ["postscript" (gnuplot-insert "set terminal postscript") t]
    ["pslatex"    (gnuplot-insert "set terminal pslatex")    t]
    ["table"      (gnuplot-insert "set terminal table")      t]
    ["tek40xx"    (gnuplot-insert "set terminal tek40xx")    t]
    ["tkcanvas"   (gnuplot-insert "set terminal tkcanvas")   t]
    ["tpic"       (gnuplot-insert "set terminal tpic")       t]
    ["vgagl"      (gnuplot-insert "set terminal vgagl")      t]	; for pm3d patch
    ["vttek"      (gnuplot-insert "set terminal vttek")      t]
    ["x11"        (gnuplot-insert "set terminal x11")        t] )
  "Terminal submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
	       (repeat :inline t
		       (vector (string   :tag "Name")
			       (function :tag "Callback")
			       (boolean  :tag "Enabled" t)))))


(defcustom gnuplot-insertions-x-axis
  '("x-axis"
    ["xdata"	  (gnuplot-insert "set xdata ")		     t]
    ["xlabel"	  (gnuplot-insert "set xlabel ")	     t]
    ["xrange"	  (gnuplot-insert "set xrange [:]")	     t]
    ["xtics"	  (gnuplot-insert "set xtics ")		     t]
    ["mxtics"	  (gnuplot-insert "set mxtics ")	     t]
    ["xzeroaxis"  (gnuplot-insert "set xzeroaxis ")	     t]
    ["xdtics"	  (gnuplot-insert "set xdtics ")	     t]
    ["xmtics"	  (gnuplot-insert "set xmtics ")	     t])
  "X-axis submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
	       (repeat :inline t
		       (vector (string   :tag "Name")
			       (function :tag "Callback")
			       (boolean  :tag "Enabled" t)))))


(defcustom gnuplot-insertions-x2-axis
  '("x2-axis"
    ["x2data"	  (gnuplot-insert "set xdata ")		     t]
    ["x2label"	  (gnuplot-insert "set xlabel ")	     t]
    ["x2range"	  (gnuplot-insert "set xrange [:]")	     t]
    ["x2tics"	  (gnuplot-insert "set xtics ")		     t]
    ["mx2tics"	  (gnuplot-insert "set mxtics ")	     t]
    ["x2zeroaxis" (gnuplot-insert "set xzeroaxis ")	     t]
    ["x2dtics"	  (gnuplot-insert "set xdtics ")	     t]
    ["x2mtics"	  (gnuplot-insert "set xmtics ")	     t])
  "X2-axis submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
	       (repeat :inline t
		       (vector (string   :tag "Name")
			       (function :tag "Callback")
			       (boolean  :tag "Enabled" t)))))


(defcustom gnuplot-insertions-y-axis
  '("y-axis"
    ["ydata"	  (gnuplot-insert "set ydata ")		     t]
    ["ylabel"	  (gnuplot-insert "set ylabel ")	     t]
    ["ymtics"	  (gnuplot-insert "set ymtics ")	     t]
    ["yrange"	  (gnuplot-insert "set yrange [:]")	     t]
    ["ytics"	  (gnuplot-insert "set ytics ")		     t]
    ["yzeroaxis"  (gnuplot-insert "set yzeroaxis ")	     t]
    ["ydtics"	  (gnuplot-insert "set ydtics ")	     t]
    ["mytics"	  (gnuplot-insert "set mytics ")	     t])
  "Y-axis submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
	       (repeat :inline t
		       (vector (string   :tag "Name")
			       (function :tag "Callback")
			       (boolean  :tag "Enabled" t)))))

(defcustom gnuplot-insertions-y2-axis
  '("y2-axis"
    ["y2data"	  (gnuplot-insert "set ydata ")		     t]
    ["y2label"	  (gnuplot-insert "set ylabel ")	     t]
    ["y2range"	  (gnuplot-insert "set yrange [:]")	     t]
    ["y2tics"	  (gnuplot-insert "set ytics ")		     t]
    ["my2tics"	  (gnuplot-insert "set mytics ")	     t]
    ["y2zeroaxis"  (gnuplot-insert "set yzeroaxis ")	     t]
    ["y2mtics"	  (gnuplot-insert "set ymtics ")	     t]
    ["y2dtics"	  (gnuplot-insert "set ydtics ")	     t])
  "Y2-axis submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
	       (repeat :inline t
		       (vector (string   :tag "Name")
			       (function :tag "Callback")
			       (boolean  :tag "Enabled" t)))))



(defcustom gnuplot-insertions-z-axis
  '("z-axis"
    ["zdata"	  (gnuplot-insert "set zdata ")		     t]
    ["zlabel"	  (gnuplot-insert "set zlabel ")	     t]
    ["zrange"	  (gnuplot-insert "set zrange [:]")	     t]
    ["ztics"	  (gnuplot-insert "set ztics ")		     t]
    ["mztics"	  (gnuplot-insert "set mztics ")             t]
    ["zdtics"	  (gnuplot-insert "set zdtics ")	     t]
    ["zmtics"	  (gnuplot-insert "set zmtics ")	     t] )
  "Z-axis submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
	       (repeat :inline t
		       (vector (string   :tag "Name")
			       (function :tag "Callback")
			       (boolean  :tag "Enabled" t)))))


(defcustom gnuplot-insertions-parametric-plots
  '("parametric plots"
    ["parametric" (gnuplot-insert "set parametric")	     t]
    ["isosamples" (gnuplot-insert "set isosamples ")	     t]
    ["dummy"	  (gnuplot-insert "set dummy ")		     t]
    ["trange"	  (gnuplot-insert "set trange [:]")	     t]
    ["urange"	  (gnuplot-insert "set urange [:]")	     t]
    ["vrange"	  (gnuplot-insert "set vrange [:]")	     t] )
  "Parametric plots submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
	       (repeat :inline t
		       (vector (string   :tag "Name")
			       (function :tag "Callback")
			       (boolean  :tag "Enabled" t)))))


(defcustom gnuplot-insertions-polar-plots
  '("polar plots"
    ["polar"	  (gnuplot-insert "set polar")		     t]
    ["angles"	  (gnuplot-insert "set angles ")	     t]
    ["rrange"	  (gnuplot-insert "set rrange [:]")	     t] )
  "Polar plots submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
	       (repeat :inline t
		       (vector (string   :tag "Name")
			       (function :tag "Callback")
			       (boolean  :tag "Enabled" t)))))


(defcustom gnuplot-insertions-surface-plots
  '("surface plots"
    ["clabel"	  (gnuplot-insert "set clabel ")	     t]
    ["cntrparam"  (gnuplot-insert "set cntrparam ")	     t]
    ["contour"	  (gnuplot-insert "set contour")	     t]
    ["dgrid3d"	  (gnuplot-insert "set dgrid3d ")	     t]
    ["hidden3d"	  (gnuplot-insert "set hidden3d ")	     t]
    ["mapping"	  (gnuplot-insert "set mapping ")	     t]
    ["surface"	  (gnuplot-insert "set surface ")	     t]
    ["view"	  (gnuplot-insert "set view ")		     t] )
  "Surface plots submenu in the insertions menu.
See the document string for `gnuplot-insertions-menu'
Changing this will not effect a change in any currently existing
`gnuplot-mode' buffer.  You will see the change the next time you
create a `gnuplot-mode' buffer."
  :group 'gnuplot-insertions
  :type '(list (string :tag "Title")
	       (repeat :inline t
		       (vector (string   :tag "Name")
			       (function :tag "Callback")
			       (boolean  :tag "Enabled" t)))))



(defvar gnuplot-insertions-bottom ()
  "Bottom part of the insertions menu.
This part contains the toggle buttons for displaying info or
opening an argument-setting popup.")
(setq gnuplot-insertions-bottom
      '("---"
	["Display of info with insertion" gnuplot-toggle-info-display
	 :style toggle :selected gnuplot-insertions-show-help-flag]
	["Display GUI popup with insertion" gnuplot-gui-toggle-popup
	 :active (fboundp 'gnuplot-gui-toggle-popup)
	 :style toggle :selected (and (fboundp 'gnuplot-gui-toggle-popup)
				      gnuplot-gui-popup-flag)] ))


;; Regarding a comment by <DB>:
;;
;; This is from the header in easymenu.el distributed with XEmacs:
;;
;; ;; - Function: easy-menu-add MENU [ MAP ]
;; ;;     Add MENU to the current menubar in MAP.
;; ;;
;; ;; - Function: easy-menu-remove MENU
;; ;;     Remove MENU from the current menubar.
;; ;;
;; ;; Emacs 19 never uses `easy-menu-add' or `easy-menu-remove', menus
;; ;; automatically appear and disappear when the keymaps specified by
;; ;; the MAPS argument to `easy-menu-define' are activated.
;; ;;
;; ;; XEmacs will bind the map to button3 in each MAPS, but you must
;; ;; explicitly call `easy-menu-add' and `easy-menu-remove' to add and
;; ;; remove menus from the menu bar.
;;
;; in Emacs, easy-menu-add is defined like this:
;;      (defun easy-menu-add (menu &optional map))

(defun gnuplot-setup-menubar ()
  "Initial setup of gnuplot and insertions menus."
  (if gnuplot-insertions-menu-flag	; set up insertions menu
      (progn
	(if gnuplot-xemacs-p
	    (setq gnuplot-insertions-top
		  '("insert set expression" "--:doubleLine"))
	  (setq gnuplot-insertions-top
		'("insert set expression" "---")))
	(setq gnuplot-insertions-menu
	      (append (list "Insertions")
		      gnuplot-insertions-top
		      (list gnuplot-insertions-adornments)
		      (list gnuplot-insertions-plot-options)
		      (list gnuplot-insertions-terminal)
		      (list gnuplot-insertions-x-axis)
		      (list gnuplot-insertions-y-axis)
		      (list gnuplot-insertions-z-axis)
		      (list gnuplot-insertions-x2-axis)
		      (list gnuplot-insertions-y2-axis)
		      (list gnuplot-insertions-parametric-plots)
		      (list gnuplot-insertions-polar-plots)
		      (list gnuplot-insertions-surface-plots)
		      gnuplot-insertions-bottom))
	(easy-menu-define gnuplot-mode-insertions-menu gnuplot-mode-map
			  "Insertions menu used in Gnuplot-mode"
			  gnuplot-insertions-menu)
	(easy-menu-add gnuplot-mode-insertions-menu gnuplot-mode-map)))
  (easy-menu-define			; set up gnuplot menu
   gnuplot-mode-menu gnuplot-mode-map "Menu used in gnuplot-mode"
   gnuplot-menu)
  (easy-menu-add gnuplot-mode-menu gnuplot-mode-map) )

;; There is no `mark-active' variable in XEmacs.  Hassle!  This is not
;; only replicates mark-active, but it only returns true if the region
;; is of non-zero width.
;; Error checking suggested by <DB>
(defun gnuplot-mark-active ()
  "Return non-nil if the mark is active and it is not equal to point."
  (condition-case nil
      (and (mark) (/= (mark) (point)))
    (error nil)))


;;; --- XEmacs toolbar

(defgroup gnuplot-toolbar nil
  "Toolbar used by XEmacs."
  :prefix "gnuplot-toolbar-"
  :group 'gnuplot)

(defcustom gnuplot-toolbar-display-flag gnuplot-xemacs-p
  "*Non-nil means to display display a toolbar in XEmacs."
  :group 'gnuplot-toolbar
  :type 'boolean)

(defcustom gnuplot-toolbar-use-toolbar (if (featurep 'toolbar) 'left-toolbar nil)
  "*If nil, do not use a toolbar.
If it is non-nil, it must be a toolbar.  The five legal values are
`default-toolbar', `top-toolbar', `bottom-toolbar', `right-toolbar',
and `left-toolbar', although choosing `default-toolbar' or
`top-toolbar' may be a bad idea since either will make the GNUPLOT
toolbar replace the standard toolbar.  Changing this will not change
the toolbar in a currently existing buffer, but it will take effect
the next time you use `gnuplot-mode' and emacs.

This is only used if a toolbar can be displayed, thus this is used in
XEmacs and ignored in FSF Emacs."
  :type '(choice (const default-toolbar)
		 (const top-toolbar)
		 (const bottom-toolbar)
		 (const left-toolbar)
		 (const right-toolbar)
		 (const :tag "No toolbar" nil))
  :group 'gnuplot-toolbar)

(defvar gnuplot-toolbar-location "")

(defun gnuplot-toolbar-setup-toolbar (toolbar)
  "Setup function for the `gnuplot-mode' toolbar.
TOOLBAR contains the toolbar specification.
This is basically swiped from VM."
  (let ((width 46) (height 46)
	(buffer (current-buffer))
	(frame (selected-frame))
	(tag-set '(win)))
    (cond
     ((eq (symbol-value gnuplot-toolbar-use-toolbar) right-toolbar)
      (setq gnuplot-toolbar-location       "right")
      (set-specifier right-toolbar         toolbar buffer)
      (set-specifier right-toolbar-width   width frame  tag-set))
     ((eq (symbol-value gnuplot-toolbar-use-toolbar) left-toolbar)
      (setq gnuplot-toolbar-location       "left")
      (set-specifier left-toolbar          toolbar buffer)
      (set-specifier left-toolbar-width    width frame  tag-set))
     ((eq (symbol-value gnuplot-toolbar-use-toolbar) bottom-toolbar)
      (setq gnuplot-toolbar-location       "bottom")
      (set-specifier bottom-toolbar        toolbar buffer)
      (set-specifier bottom-toolbar-height height frame tag-set))
     ((eq (symbol-value gnuplot-toolbar-use-toolbar) top-toolbar)
      (setq gnuplot-toolbar-location       "top")
      (set-specifier top-toolbar           toolbar buffer)
      (set-specifier top-toolbar-height    height frame tag-set))) ))

(defvar gnuplot-line-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *line[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        5           1\",
/* colors */
\". c #000000\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaa..a..aaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaa..a..a....aaa...aaaaaaaaaaaa\",
\"aaaaaaaaaaaa..a..a..a..a..a..aaaaaaaaaaa\",
\"aaaaaaaaaaaa..a..a.aa..a.....aaaaaaaaaaa\",
\"aaaaaaaaaaa..a..a..a..a..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaa..a..a..a..a..a..aaaaaaaaaaaa\",
\"aaaaaaaaaaa..a..a..a..aa...aaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};")
  "XPM format image used for the \"plot line\" button"))

(defvar gnuplot-region-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *region[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        5           1\",
/* colors */
\". c #000000\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaa..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa.a..a...aaa....a..aa...aa....aaaaa\",
\"aaaaaa...a..a..a..a..a..a..a..a..a..aaaa\",
\"aaaaaa..aa.....a.aa..a....aa..a.aa..aaaa\",
\"aaaaa..a...aaaa..aa.a..a..aa....a..aaaaa\",
\"aaaaa..a...a..a..a..a..a..a..a..a..aaaaa\",
\"aaaa..aaaa...aaa....a..aa...aa..a..aaaaa\",
\"aaaaaaaaaaaaaaaaa..aaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaa...aaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};")
  "XPM format image used for the \"plot region\" button"))

(defvar gnuplot-buffer-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *buffer[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        5           1\",
/* colors */
\". c #000000\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa..aaaaaaaaaa......aaaaaaaaaaaaaa\",
\"aaaaaaaa..aaaaaaaaa..a..aaaaaaaaaaaaaaaa\",
\"aaaaaaa....aa..a.........a...aa.a.aaaaaa\",
\"aaaaaaa..a..a..a..a..a..a..a..a...aaaaaa\",
\"aaaaaaa.aa....aa..a..a..a.....a..aaaaaaa\",
\"aaaaaa...a.a..a..a..a..a..aaaa..aaaaaaaa\",
\"aaaaaa.....a..a..a..a..a..a..a..aaaaaaaa\",
\"aaaaaa....aaa..a.a..a..aa...aa..aaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};")
  "XPM format image used for the \"plot buffer\" button"))

(defvar gnuplot-doc-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *book_index[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        6            1\",
/* colors */
\". c #000000\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff0000\",
\"d c #ffffff\",
\"e c #708090\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa.........bbeaaaebb..........aaaaaa\",
\"aaaaaa.ddddddddaaebebeaaddddddddd.aaaaaa\",
\"aaaa...dab.bddeebadbdaeebedeeeeed...aaaa\",
\"aaaa.c.dbaddddebeedbdeebeedebebed.c.aaaa\",
\"aaaa.c.d.de.edeebeabdbbeeddebbbed.c.aaaa\",
\"aaaa.c.dbad.ddebeadbdeeebeddeeeed.c.aaaa\",
\"aaaa.c.dab..ddeeeedbdebeeedebebed.c.aaaa\",
\"aaaa.c.dddddddeebeabdebebedeebedd.c.aaaa\",
\"aaaa.c.debebedebeedbdbebeedbeeeeb.c.aaaa\",
\"aaaa.c.debeeedeeeaabdaaddddebedbb.c.aaaa\",
\"aaaa.c.deebeddbebedbdbaa.adeeedeb.c.aaaa\",
\"aaaa.c.ddeebedeeebaba.dd.dddeeedd.c.aaaa\",
\"aaaa.c.debeebdbeeedbd....ddeebeed.c.aaaa\",
\"aaaa.c.deebeedeebadbd.dd.ddeeeedd.c.aaaa\",
\"aaaa.c.dbbebddeeeeabd.aa.adebebbd.c.aaaa\",
\"aaaa.c.deeeeedeebeabaedddddeeeedd.c.aaaa\",
\"aaaa.c.dbebbbdebeadbdaeeeedebeeed.c.aaaa\",
\"aaaa.c.deeebddeeebdbdeebeedeebeed.c.aaaa\",
\"aaaa.c.debeeedebeeabdebebedebeebd.c.aaaa\",
\"aaaa.c.deebbedeeeedbdeeeeddeeeeed.c.aaaa\",
\"aaaa.c.dddddddddaadbdaddddddddddd.c.aaaa\",
\"aaaa.c..........beabaeb...........c.aaaa\",
\"aaaa.c.bbbbbbbbbb.bbbbbbbbbbbbbbb.c.aaaa\",
\"aaaa.c.bbbbbbbbbb..e.bbbbbbbbbbbb.c.aaaa\",
\"aaaa.c.bbbbbbbbbb.b.bbbbbbbbbbbbb.c.aaaa\",
\"aaaa.c............e.e.............c.aaaa\",
\"aaaa.cccccccccccc.a.a.ccccccccccccc.aaaa\",
\"aaaa................................aaaa\",
\"aaaaaaaaaaaaaaaaaa...aaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};")
  "XPM format image used for the \"document\" button"))

(defvar gnuplot-help-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *help_btn[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        3            1\",
/* colors */
\"a c #bebebe s backgroundToolBarColor\",
\"b c #000000\",
\"c c #ff0000\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbbbbbbbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaabbbccccccccbbbaaaaaaaaaaaaa\",
\"aaaaaaaaaaabbccccccccccccccbbaaaaaaaaaaa\",
\"aaaaaaaaaabccccccccccccccccccbaaaaaaaaaa\",
\"aaaaaaaaabccccccccccccccccccccbaaaaaaaaa\",
\"aaaaaaaabcccccccbbbbbbbbcccccccbaaaaaaaa\",
\"aaaaaaaabccccbbbaaaaaaaabbbccccbaaaaaaaa\",
\"aaaaaaabccccbaaaaaaaaaaaaaabccccbaaaaaaa\",
\"aaaaaaabcccbaaaaaaaaaaaaaaaabcccbaaaaaaa\",
\"aaaaaaabcccbaaaaaaaaaaaaaaaabcccbaaaaaaa\",
\"aaaaaaabcccbaaaaaaaaaaaaaaaabcccbaaaaaaa\",
\"aaaaaaabbbbbaaaaaaaaaaaaaaabccccbaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaabbbccccbaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbbbbbbbcccccccbaaaaaaaa\",
\"aaaaaaaaaaaaaaaabcccccccccccccbaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccccccccbaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccccccbbaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccbbbbbbaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbbbbbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbbbbbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbbbbbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};")
  "XPM format image used for the \"help\" button"))

(defvar gnuplot-toolbar
  '([gnuplot-line-xpm
     gnuplot-line-fn     t     "Plot the line under point"]
    [gnuplot-region-xpm
     gnuplot-region-fn   t     "Plot the selected region"]
    [gnuplot-buffer-xpm
     gnuplot-buffer-fn   t     "Plot the entire buffer"]
    [:style 3d :size 8]
    [gnuplot-help-xpm
     gnuplot-help-fn     t     "Look at the gnuplot process buffer"]
    [gnuplot-doc-xpm
     gnuplot-doc-fn      t     "Look at the gnuplot document"])
  "The gnuplot toolbar.")

(fset 'gnuplot-line-fn   'gnuplot-send-line-and-forward)
(fset 'gnuplot-region-fn 'gnuplot-send-region-to-gnuplot)
(fset 'gnuplot-buffer-fn 'gnuplot-send-buffer-to-gnuplot)
(fset 'gnuplot-help-fn   'gnuplot-show-gnuplot-buffer)
(fset 'gnuplot-doc-fn    'gnuplot-info-lookup-symbol)

(defvar gnuplot-all-buttons-defined
  (and (listp gnuplot-line-xpm)   (listp gnuplot-region-xpm)
       (listp gnuplot-buffer-xpm) (listp gnuplot-doc-xpm)
       (listp gnuplot-help-xpm)))


(defun gnuplot-make-toolbar-function ()
  (if (and gnuplot-xemacs-p gnuplot-all-buttons-defined)
      (progn
	;;(remove-specifier gnuplot-toolbar-use-toolbar (current-buffer))
	(gnuplot-toolbar-setup-toolbar gnuplot-toolbar)
	(add-spec-to-specifier (symbol-value gnuplot-toolbar-use-toolbar)
			       gnuplot-toolbar
			       (current-buffer) ))))

;;(defalias 'gnuplot-make-toolbar 'gnuplot-make-toolbar-function)



;;; --- syntax colorization, syntax table

(defvar gnuplot-mode-syntax-table nil
  "Syntax table in use in `gnuplot-mode' buffers.
This is the same as the standard syntax table except that ' is a
string quote character, ` and _ are word characters, and math
operators are punctuation characters.")
(if gnuplot-mode-syntax-table
    ()
  (setq gnuplot-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?* "."  gnuplot-mode-syntax-table)
  (modify-syntax-entry ?+ "."  gnuplot-mode-syntax-table)
  (modify-syntax-entry ?- "."  gnuplot-mode-syntax-table)
  (modify-syntax-entry ?/ "."  gnuplot-mode-syntax-table)
  (modify-syntax-entry ?% "."  gnuplot-mode-syntax-table)
  ;;(modify-syntax-entry ?& "."  gnuplot-mode-syntax-table) ; rarely used
  ;;(modify-syntax-entry ?^ "."  gnuplot-mode-syntax-table) ; operators
  ;;(modify-syntax-entry ?| "."  gnuplot-mode-syntax-table) ; in gnuplot,
  ;;(modify-syntax-entry ?& "."  gnuplot-mode-syntax-table) ; (by me,
  ;;(modify-syntax-entry ?? "."  gnuplot-mode-syntax-table) ;  anyway...)
  ;;(modify-syntax-entry ?~ "."  gnuplot-mode-syntax-table) ;
  (modify-syntax-entry ?' "\"" gnuplot-mode-syntax-table)
  (modify-syntax-entry ?` "w"  gnuplot-mode-syntax-table)
  (modify-syntax-entry ?_ "w"  gnuplot-mode-syntax-table))


(defvar gnuplot-font-lock-keywords nil)
(defvar gnuplot-font-lock-keywords-1 nil)
(defvar gnuplot-font-lock-keywords-2 nil)

;; used make-regexp to generate the regular expression strings
;; this is all pattern based
;; (insert (format "%s"
;;  		(regexp-quote
;;  		 (make-regexp
;; 		  '("abs" "acos" "acosh" "arg" "asin" "asinh" "atan"
;; 		    "atan2" "atanh" "besj0" "besj1" "besy0" "besy1"
;; 		    "ceil" "cos" "cosh" "erf" "erfc" "exp" "floor"
;; 		    "gamma" "ibeta" "inverf" "igamma" "imag" "invnorm"
;; 		    "int" "lgamma" "log" "log10" "norm" "rand" "real"
;; 		    "sgn" "sin" "sinh" "sqrt" "tan" "tanh" "column"
;; 		    "tm_hour" "tm_mday" "tm_min" "tm_mon" "tm_sec"
;; 		    "tm_wday" "tm_yday" "tm_year" "valid")))))

;; Set up colorization for gnuplot.
;; This handles font-lock for emacs and xemacs.
;; hilit19 is handled in `gnuplot-mode'.
;; These regular expressions treat the gnuplot vocabulary as complete
;; words.  Although gnuplot will recognise unique abbreviations, these
;; regular expressions will not."
(if (featurep 'font-lock)		; <KL>
    (setq gnuplot-font-lock-keywords
	  (list
					; comments
	   '("#.*$" . font-lock-comment-face)
					; quoted things
	   ;'("['\"]\\([^'\"\n]*\\)['\"]"
	   ;  1 font-lock-string-face)
	   '("'[^'\n]*'?" . font-lock-string-face)
					; stuff in brackets, sugg. by <LB>
	   '("\\[\\([^]]+\\)\\]"
	     1 font-lock-reference-face)
					; variable/function definitions
	   '("\\(\\<[a-z]+[a-z_0-9()]*\\)[ \t]*="
	     1 font-lock-variable-name-face)
					; built-in function names
	   (cons (concat
		  "\\<\\("
		  "a\\(bs\\|cosh\?\\|rg\\|sinh\?\\|"
		  "tan\\(\\|\[2h\]\\)\\)\\|"
		  "bes\\(j\[01\]\\|y\[01\]\\)\\|"
		  "c\\(eil\\|o\\(lumn\\|sh\?\\)\\)\\|"
		  "e\\(rfc\?\\|xp\\)\\|floor\\|gamma\\|"
		  "i\\(beta\\|gamma\\|mag\\|"
		  "n\\(t\\|v\\(erf\\|norm\\)\\)\\)\\|"
		  "l\\(gamma\\|og\\(\\|10\\)\\)\\|"
		  "norm\\|r\\(and\\|eal\\)\\|"
		  "s\\(gn\\|inh\?\\|qrt\\)\\|"
		  "t\\(anh\?\\|m_\\(hour\\|m\\(day\\|in\\|on\\)\\|"
		  "sec\\|wday\\|y\\(day\\|ear\\)\\)\\)\\|"
		  "valid"
		  "\\)\\>")
		 font-lock-function-name-face)
					; reserved words associated with
					; plotting <AL>
	   '("\\<\\(axes\\|every\\|index\\|l\\(\[stw\]\\|ine\\(style\\|type\\|width\\)\\)\\|notitle\\|p\\(\[st\]\\|oint\\(size\\|type\\)\\)\\|smooth\\|t\\(hru\\|itle\\)\\|using\\|with\\)\\>" . font-lock-type-face)
	   '("\\<\\(box\\(e\\(rrorbars\\|s\\)\\|xyerrorbars\\)\\|candlesticks\\|dots\\|errorbars\\|f\\(inancebars\\|steps\\)\\|histeps\\|impulses\\|lines\\(\\|points\\)\\|points\\|steps\\|vector\\|x\\(errorbars\\|yerrorbars\\)\\|yerrorbars\\)\\>" . font-lock-function-name-face)
					; (s)plot -- also thing (s)plotted
	   '("\\<s?plot\\>" . font-lock-keyword-face)
	   '("\\<s?plot\\s-+\\([^'\" ]+\\)[) \n,\\\\]"
	     1 font-lock-variable-name-face)
					; other common commands
					; miscellaneous commands
	   (cons (concat "\\<\\("
			 "bind\\|"
			 "c\\(d\\|lear\\)\\|exit\\|fit\\|h\\(elp\\|istory\\)\\|load\\|"
			 "p\\(ause\\|rint\\|wd\\)\\|quit\\|replot\\|"
			 "s\\(ave\\|et\\|how\\)\\|unset"
			 "\\)\\>\\|!.*$")
		 font-lock-reference-face))
	  gnuplot-font-lock-keywords-1 gnuplot-font-lock-keywords
	  gnuplot-font-lock-keywords-2 gnuplot-font-lock-keywords) )

(if (and gnuplot-xemacs-p (featurep 'font-lock))
    (put 'gnuplot-mode 'font-lock-defaults
	 '((gnuplot-font-lock-keywords
	    gnuplot-font-lock-keywords-1
	    gnuplot-font-lock-keywords-2)
	   t t ((?_ . "w")) )))

;; these two lines get rid of an annoying compile time error
;; message.  that function gets non-trivially defalias-ed in
;; gnuplot-toolbar.el
;; (defun gnuplot-make-toolbar-dummy ())
;; (defalias 'gnuplot-make-toolbar 'gnuplot-make-toolbar-dummy)


;;; --- functions for sending commands to gnuplot

(defun gnuplot-split-string (string)
  "Break STRING at each carriage return, returning a list of lines."
  (let ((list ()) (line "") (index 0))
    (while (< index (length string))
      (if (char-equal (elt string index) ?\n)
	  (setq list (append list (list line))
		line "")
	(setq line (concat line (char-to-string (elt string index)))))
      (setq index (1+ index)) )
    list))

;; -- the calls to `sleep-for' are to allow enough time for gnuplot
;;    to write to the buffer before the next line is inserted
;; -- note that the input string is split into lines and each line is
;;    sent to gnuplot individually.  this is a bit slow, but it puts
;;    each line on the comint history.
(defun gnuplot-send-string-to-gnuplot (string text)
  "Sends STRING to the gnuplot program.
If no gnuplot process exists, a new one is created.  TEXT indicates
the type of text being sent to gnuplot and is typically one of
nil, 'line, 'region, 'buffer, or 'file.  TEXT may be useful for
functions in `gnuplot-after-plot-hook'.  `gnuplot-after-plot-hook' is
called by this function after all of STRING is sent to gnuplot."
  (gnuplot-make-gnuplot-buffer)	; make sure a gnuplot buffer exists
  (or gnuplot-program-version
      (progn
	(message "Determining gnuplot version number (sitting for 2 seconds)")
	(gnuplot-fetch-version-number)
	(sit-for 2)))
  (setq gnuplot-comint-recent-buffer (current-buffer))
  (if (equal gnuplot-display-process 'frame)
      (or (and gnuplot-process-frame
	       (frame-live-p gnuplot-process-frame))
	  (let ((frame (selected-frame)))
	    (setq gnuplot-process-frame (make-frame))
	    (select-frame gnuplot-process-frame)
	    (switch-to-buffer gnuplot-buffer)
	    (delete-other-windows)
	    (select-frame frame))) )
  (let ((buffer  (current-buffer))
	(gbuffer (get-buffer gnuplot-buffer))
	(list    (gnuplot-split-string string)))
    (set-buffer gbuffer)
    (goto-char (point-max))
    ;; bruce asks: what is this next line for?
    (set-marker (process-mark gnuplot-process) (point-marker))
    (sleep-for (* 20 gnuplot-delay))
    (while list
      (insert (car list))
      (comint-send-input)
      (sleep-for gnuplot-delay)
      (setq list (cdr list))
      (goto-char (point-max)))
    (set-buffer buffer)
    (cond ((equal gnuplot-display-process 'window)
	   (select-window (display-buffer gbuffer))
	   (goto-char (point-max))
	   (or (pos-visible-in-window-p (point) (selected-window))
	       (recenter 5))
	   (other-window 1))
	  ((equal gnuplot-display-process 'frame)
	   ;;(raise-frame gnuplot-process-frame)
	   (select-frame gnuplot-process-frame)
	   (display-buffer gbuffer)
	   (goto-char (point-max))
	   (or (pos-visible-in-window-p (point) (selected-window))
	       (recenter 5))))
    ;;(process-send-string gnuplot-program string)
    (setq gnuplot-recently-sent text)
    (run-hooks 'gnuplot-after-plot-hook)))

(defun gnuplot-send-region-to-gnuplot (&optional begin end text)
  "Sends a selected region to the gnuplot program.
If BEGIN and END are not specified, point and mark are used.  TEXT
indicates the type of text being sent to gnuplot.  This will be
'region unless explicitly set by a function calling this one.  Other
typical values are of nil, 'line, 'buffer, or 'file.  TEXT may be
useful for function in `gnuplot-after-plot-hook'."
  (interactive "r")
  (let (string (txt (or text 'region)))
    (cond ((equal major-mode 'gnuplot-mode)
	   (setq string (buffer-substring-no-properties begin end))
 	   (if (string= (substring string -1) "\n") ()
 	     (setq string (concat string "\n")))
	   (gnuplot-send-string-to-gnuplot string txt))
	  (t
	   (message (concat "You can only send regions from "
			    "gnuplot-mode buffers to gnuplot."))))))

(defun gnuplot-send-line-to-gnuplot ()
  "Sends the current line to the gnuplot program.
Respects continuation lines.
This sets `gnuplot-recently-sent' to 'line."
  (interactive)
  (cond ((equal major-mode 'gnuplot-mode)
	 (let ((start (save-excursion (beginning-of-line)   (point-marker)))
	       end
	       ;(end   (save-excursion (beginning-of-line 2) (point-marker)))
	       )
           (save-excursion
	     (goto-char start)
	     (end-of-line)
	     (backward-char 1)
	     (while (looking-at "\\\\")	; go to end of last continuation line
	       (end-of-line 2)
	       (backward-char 1))
	     (beginning-of-line 2)
	     (setq end (point-marker)))
	   (if (not (string-match "\\`\\s-*\\'"
				  (buffer-substring-no-properties start end)))
	       (gnuplot-send-region-to-gnuplot start end 'line))
	   end))
	(t
	 (message "You can only send lines in gnuplot-mode buffers to gnuplot.")
	 nil)))

;; I chose a very easy to type but slightly non-mnemonic key-binding
;; for this (C-c C-v).  It seems like the kind of thing one would want
;; to do repeatedly without incurring RSI. 8^)
(defun gnuplot-send-line-and-forward (&optional num)
  "Call `gnuplot-send-line-to-gnuplot' and move forward 1 line.
You can use a numeric prefix to send more than one line.  Blank lines and
lines with only comments are skipped when moving forward."
  (interactive "p")
  (let (end)
    (while (> num 0)
      (setq end (gnuplot-send-line-to-gnuplot))
      (goto-char end)
      (backward-char 1)			; <AR>
      (gnuplot-forward-script-line 1)
      (setq num (1- num)))))


(defun gnuplot-forward-script-line (&optional num) ; <SE>
  "Move forward my NUM script lines.
Blank lines and commented lines are not included in the NUM count."
  (interactive "p")
  (while (> num 0)
    (and (not (eobp)) (forward-line 1))
    (while (and (not (eobp))
		(or (looking-at "^\\s-*$")
		    (looking-at "^\\s-*#")))
      (forward-line 1))
    (setq num (1- num))) )

(defun gnuplot-send-buffer-to-gnuplot ()
  "Sends the entire buffer to the gnuplot program.
This sets `gnuplot-recently-sent' to 'buffer."
  (interactive)
  (if (equal major-mode 'gnuplot-mode)
      (gnuplot-send-region-to-gnuplot (point-min) (point-max) 'buffer)
    (message "You can only send gnuplot-mode buffers to gnuplot.")))

(defun gnuplot-send-file-to-gnuplot ()
  "Sends a selected file to the gnuplot program using the \"load\" command.
This sets `gnuplot-recently-sent' to 'file."
  (interactive)
  (let ((string (read-file-name "Name of file to send to gnuplot > " nil nil t)))
    (setq string (concat "load '" (expand-file-name string) "'\n"))
    (message "%S" string)
    (gnuplot-make-gnuplot-buffer)	; make sure a gnuplot buffer exists
    (gnuplot-send-string-to-gnuplot string 'file)))

;; suggested by <JS>
(defun gnuplot-plot-from-comint ()
  "Send the contents of a script to gnuplot from the process buffer.
This inserts the contents of the most recently used gnuplot script
into the process buffer and sends those lines to gnuplot.  It does
this by copying the script line by line."
  (interactive)
  (if (equal major-mode 'comint-mode)
      (let (string list (buffer (current-buffer)))
	(set-buffer gnuplot-comint-recent-buffer)
	(setq string (buffer-substring-no-properties (point-min) (point-max))
	      string (concat string "\n")
	      list   (gnuplot-split-string string))
	(set-buffer buffer)
	(while list
	  (insert (car list))
	  (comint-send-input)
	  (sleep-for gnuplot-delay)
	  (setq list (cdr list)))
	(comint-send-input))
    (message
     "`gnuplot-plot-from-comint' only works in the gnuplot process buffer")))

(defun gnuplot-save-and-plot-from-comint ()
  "Send a current script to gnuplot from the process buffer.
This sends the most recently used gnuplot script to gnuplot using the
\"load\" command.  This function first saves the script buffer to a
file, prompting for a filename if one is not associated with the script
buffer.  Then it sends a load command to gnuplot using the name of the
file visited by the script buffer."
  (interactive)
  (if (equal major-mode 'comint-mode)
      (let (fname (buffer (current-buffer)))
	(set-buffer gnuplot-comint-recent-buffer)
	(save-buffer)
	(setq fname (buffer-file-name))
	(set-buffer buffer)
	(goto-char (point-max))
	(insert (format "load '%s'" fname))
	(comint-send-input))
    (message (concat "`gnuplot-save-and-plot-from-comint' only works "
		     "in the gnuplot process buffer"))))


(defun gnuplot-trim-gnuplot-buffer ()
  "Trim lines form the beginning of the *gnuplot* buffer.
This keeps that buffer from growing excessively in size.  Normally,
this function is attached to `gnuplot-after-plot-hook'"
  (if (> gnuplot-buffer-max-size 0)
      (save-excursion
	(set-buffer gnuplot-buffer)
	(let ((nlines (count-lines (point-min) (point-max)))
	      (kill-whole-line t))
	  (while (> nlines gnuplot-buffer-max-size)
	    (goto-char (point-min))
	    (kill-line)
	    (setq nlines (1- nlines)))
	  (goto-char (point-max)) ))))
(add-hook 'gnuplot-after-plot-hook 'gnuplot-trim-gnuplot-buffer nil nil)


;;; --- functions controlling the gnuplot process

;; use of comint-setup-hook suggested by <DB>
(defun gnuplot-comint-start-function ()
  "Function run when comint/gnuplot started.
This sets font-lock and keyword completion in the comint/gnuplot
buffer.  Further customization is possible via
`gnuplot-comint-setup-hook'."
  ;;(if (not (fboundp 'hilit-set-mode-patterns))
  (if (featurep 'font-lock)
      (progn
	(make-variable-buffer-local 'font-lock-defaults)
	(setq font-lock-defaults '(gnuplot-font-lock-keywords t t))
	(if gnuplot-xemacs-p (turn-on-font-lock))))
  ;;(if (featurep 'kw-compl)
  ;;    (progn
  ;;	(setq kw-compl-list gnuplot-keywords
  ;;	      kw-compl-upper-case nil)
  ;;	(define-key comint-mode-map "\M-\r" 'kw-compl-abbrev)))
  (define-key comint-mode-map "\M-\C-p" 'gnuplot-plot-from-comint)
  (define-key comint-mode-map "\M-\C-f" 'gnuplot-save-and-plot-from-comint)
  (define-key comint-mode-map "\C-d"    'gnuplot-delchar-or-maybe-eof)
  (define-key comint-mode-map "\M-\r"   'gnuplot-complete-keyword)
  (define-key comint-mode-map "\M-\t"   'gnuplot-complete-keyword)
  (run-hooks 'gnuplot-comint-setup-hook))

(defun gnuplot-make-gnuplot-buffer ()
  "Switch to the gnuplot program buffer or create one if none exists."
  (or (and gnuplot-process (get-process gnuplot-process)
	   gnuplot-buffer (get-buffer gnuplot-buffer))
      (progn
	(message "Starting gnuplot plotting program...")
	(setq gnuplot-buffer (make-comint gnuplot-process-name gnuplot-program)
	      gnuplot-process (get-process gnuplot-process-name))
	(process-kill-without-query gnuplot-process nil)
	(save-excursion
	  (set-buffer gnuplot-buffer)
	  (make-local-hook 'kill-buffer-hook)
	  (add-hook 'kill-buffer-hook 'gnuplot-close-down nil t)
	  (gnuplot-comint-start-function)
          (make-local-variable 'comint-output-filter-functions)
          (setq comint-output-filter-functions
                (append comint-output-filter-functions
                        '(comint-postoutput-scroll-to-bottom
                          gnuplot-protect-prompt-fn)))
	  (message "Starting gnuplot plotting program...Done")))))


(defun gnuplot-fetch-version-number ()
  ;;(interactive)
  (message "gnuplot-mode %s -- determining gnuplot version ......"
	   gnuplot-version)
  (let* ((command (concat "echo \"show version\" | " gnuplot-program))
	 (process (start-process-shell-command "gnuplot-version"
					       "*gnuplot-version*"
					       command)))
    (set-process-sentinel process 'gnuplot-determine-version-number)))

(defun gnuplot-determine-version-number (process event)
  (save-excursion
    (let (version)
      (if (string-match "SPEEDBAR" (format "%S" (current-buffer))) ;; <WZ>
	  (if (fboundp 'speedbar-switch-buffer-attached-frame)
	      (speedbar-switch-buffer-attached-frame "*gnuplot-version*")
	    (progn
	      (speedbar-select-attached-frame)
	      (switch-to-buffer "*gnuplot-version*")))
	(switch-to-buffer "*gnuplot-version*"))
      (goto-char (point-min))
      (re-search-forward "[Vv]ersion\\s-+" (point-max) t)
      (if (looking-at "[0-9]\\.[0-9]+")
	  (setq version (match-string 0))
	(setq version "3.7"))
      (kill-buffer (get-buffer "*gnuplot-version*"))
      ;;(and (interactive-p) (message "You are using gnuplot version %s" version))
      (setq gnuplot-program-version version
	    gnuplot-three-eight-p (>= (string-to-number gnuplot-program-version) 3.8))
      (gnuplot-setup-menu-and-toolbar)
      )))

(defun gnuplot-setup-menu-and-toolbar ()
  ;; set up the menubar (possibly dependent on version number)
  (gnuplot-setup-menubar)
  ;; set up the toolbar (possibly dependent on version number)
  (if (and gnuplot-xemacs-p gnuplot-toolbar-display-flag)
      (condition-case ()		; deal with the toolbar
	  (and (require 'toolbar)
	       (require 'xpm)
	       (gnuplot-make-toolbar-function))
	(error nil)))
  (message "gnuplot-mode %s (gnuplot %s) -- report bugs with %S"
	   gnuplot-version gnuplot-program-version
	   (substitute-command-keys "\\[gnuplot-bug-report]"))
  )



;; (defun gnuplot-determine-gnuplot-version ()
;;   "Figure out which version of gnuplot we are running."
;;   (interactive)
;;   (cond (gnuplot-gnuplot-version
;; 	 (setq comint-process-echoes nil          ;; t
;; 	       gnuplot-program-version gnuplot-gnuplot-version))
;; 	(t
;; 	 (let ((counter 0))
;; 	   (save-excursion
;; 	     (set-buffer gnuplot-buffer)
;; 	     (goto-char (point-min))
;; 	     ;; it may take a while for emacs to display the gnuplot start-up
;; 	     ;; message.  since we need this to determine the version number
;; 	     ;; and hence the value of `comint-process-echoes', we must wait
;; 	     ;; for this to happen.
;; 	     (while (and (equal (point-max) (point-min)) (< 10 counter))
;; 	       (1+ counter)
;; 	       (sleep-for 0.1))
;; 	     (if (re-search-forward "[Vv]ersion" (point-max) t)
;; 		 (progn
;; 		   (cond ((or (looking-at "\\s-*3.8") (looking-at "\\s-*4"))
;; 			  (setq comint-process-echoes nil          ;; t
;; 				gnuplot-program-version "3.8"))
;; 			 ((looking-at "\\s-*3.7")
;; 			  (setq comint-process-echoes nil          ;; t
;; 				gnuplot-program-version "3.7"))
;; 			 (t
;; 			  (setq comint-process-echoes nil
;; 				gnuplot-program-version "3.5") )))
;; 	       (setq comint-process-echoes gnuplot-echo-command-line-flag)))))))

(defun gnuplot-protect-prompt-fn (string)
  "Prevent the Gnuplot prompt from being deleted or overwritten.
STRING is the text as originally inserted in the comint buffer."
  (save-excursion
    (let ((b (progn
               (goto-char (point-max))
               (beginning-of-line)
               (point)))
          e)
      (if (re-search-forward "^gnuplot> " (point-max) t)
          (progn
            (setq e (point))
            (put-text-property b e 'rear-nonsticky '(read-only intangible face))
            (put-text-property b e 'intangible t)
            (put-text-property b e 'face 'gnuplot-prompt-face)
            ;;(put-text-property b e 'read-only t)
	    )) )))

(defun gnuplot-close-down ()
  "Tidy up when deleting the gnuplot buffer."
  (if (eq (process-status gnuplot-process) 'run);; <SE>
      (kill-process gnuplot-process))
  (setq gnuplot-process nil
        gnuplot-buffer nil))

(defun gnuplot-delchar-or-maybe-eof (arg)
  "Delete ARG characters forward, or (if at eob) send an EOF to subprocess.
This is very similar to `comint-delchar-or-maybe-eof'."
  (interactive "p")
  (if (eobp)
      (gnuplot-kill-gnuplot-buffer)
    (delete-char arg)))

(defun gnuplot-kill-gnuplot-buffer ()
  "Kill the gnuplot process and its display buffers."
  (interactive)
  (if (and gnuplot-process
	   (eq (process-status gnuplot-process) 'run))  ;; <SE>
      (kill-process gnuplot-process))
  (if (and gnuplot-buffer (get-buffer gnuplot-buffer))
      (progn
	(if (one-window-p) ()
	  (delete-window (get-buffer-window gnuplot-buffer)))
	(kill-buffer gnuplot-buffer)))
  (setq gnuplot-process nil
        gnuplot-buffer nil))


(defun gnuplot-show-gnuplot-buffer ()
  "Switch to the buffer containing the gnuplot process.
When `gnuplot-display-process' is nil this will switch to
the gnuplot process buffer.  When that variable is non-nil, the
gnuplot process buffer will be displayed in a window."
  (interactive)
  (if (and gnuplot-buffer (get-buffer gnuplot-buffer))
      (cond ((equal gnuplot-display-process 'window)
	     (switch-to-buffer-other-window gnuplot-buffer))
	    ((equal gnuplot-display-process 'frame)
	     (or (and gnuplot-process-frame
		      (frame-live-p gnuplot-process-frame))
		 (setq gnuplot-process-frame (make-frame)))
	     (raise-frame gnuplot-process-frame)
	     (select-frame gnuplot-process-frame)
	     (switch-to-buffer gnuplot-buffer))
	    (t
	     (switch-to-buffer gnuplot-buffer)))
    (message "There is not an active Gnuplot process.")))


;;; --- miscellaneous functions: insert file name, indentation, negation

(defun gnuplot-insert-filename ()
  "Insert a filename at point, prompting for name in the minibuffer.
This inserts a filename relative to the buffer's default directory.
Uses completion and the value of `gnuplot-quote-character'.
Bound to \\[gnuplot-insert-filename]"
  (interactive)
  (insert gnuplot-quote-character
	  (file-relative-name (read-file-name "Filename > " "")
			      default-directory)
	  gnuplot-quote-character) )

;; is this more complicated than it need be ...?
;; this doesn't quite do plot lists correctly:
;;   plot sin(x),\
;;        cos(x)         # ok
;;        set auto       # not ok, should be under "p" (but does it matter?)

(defun gnuplot-indent-line ()
  "Set indentation in gnuplot buffer.
For most lines, set indentation to previous level of indentation.
Attempt to add additional indentation for continued plot and splot
lines."
  (interactive)
  (let ((indent 0))
    (save-excursion
      (save-excursion
	(end-of-line 0)
	(if (bobp) ()
	  (re-search-backward "^[ \t]*." (point-min) "to_limit")
	  (back-to-indentation)
	  (setq indent (current-column))
	  (if (looking-at "s?pl\\(o?\\|\\(ot\\)?\\)[ \t]+.?")
	      (let ((plus (1- (length (match-string 0)))))
		(end-of-line)
		(backward-char 1)
		(if (looking-at (regexp-quote "\\"))
		    (setq indent  (+ plus indent)))))))
      (if (= (current-indentation) indent)
	  ()
	(beginning-of-line)
	(delete-horizontal-space)
	(insert (make-string indent ? ))))
    (if (looking-at "[ \t]+$")
	(end-of-line))))

;; FWIW, here are all the options which can be negated:
;; (insert (format "%s"
;; 		(regexp-quote
;; 		 (make-regexp
;; 		  '("arrow" "autoscale" "border" "clabel" "clip"
;; 		    "contour" "dgrid3d" "grid" "hidden3d" "key" "label"
;; 		    "linestyle" "logscale" "multiplot" "mxtics"
;; 		    "mytics" "mztics" "mx2tics" "my2tics"
;; 		    "offsets" "polar" "surface" "timestamp" "title"
;; 		    "xdtics" "ydtics" "zdtics" "x2dtics" "y2dtics"
;; 		    "xmtics" "ymtics" "zmtics" "x2mtics" "y2mtics"
;; 		    "xtics" "ytics" "ztics" "x2tics" "y2tics"
;; 		    "xzeroaxis" "yzeroaxis" "zzeroaxis" "x2zeroaxis"
;; 		    "y2zeroaxis")))))

(defun gnuplot-negate-option ()
  "Append \"no\" to or remove \"no\" from the set option on the current line.
This checks if the set option is one which has a negated form."
  (interactive)
  (let ((begin (save-excursion (beginning-of-line) (point-marker)))
	(end   (save-excursion (end-of-line)       (point-marker)))
	(regex "a\\(rrow\\|utoscale\\)\\|border\\|c\\(l\\(abel\\|ip\\)\\|ontour\\)\\|dgrid3d\\|grid\\|hi\\(dden3d\\|storysize\\)\\|key\\|l\\(abel\\|inestyle\\|ogscale\\)\\|m\\(ouse\\|ultiplot\\|x\\(2tics\\|tics\\)\\|y\\(2tics\\|tics\\)\\|ztics\\)\\|offsets\\|polar\\|surface\\|ti\\(mestamp\\|tle\\)\\|x\\(2\\(dtics\\|mtics\\|tics\\|zeroaxis\\)\\|dtics\\|mtics\\|tics\\|zeroaxis\\)\\|y\\(2\\(dtics\\|mtics\\|tics\\|zeroaxis\\)\\|dtics\\|mtics\\|tics\\|zeroaxis\\)\\|z\\(dtics\\|mtics\\|tics\\|zeroaxis\\)"))
    (save-excursion
      (if (search-backward ";" begin t)
	  (progn (forward-char  1) (setq begin (point-marker))))
      (if (search-forward  ";" end   t)
	  (progn (forward-char -1) (setq end   (point-marker))))
      (goto-char begin)
      (skip-syntax-forward "-" end)
      (if (looking-at "\\(un\\)?set\\s-+")
	  (cond ((and gnuplot-program-version
		      (> (string-to-number gnuplot-program-version) 3.7))
		 (cond ((looking-at "unset")
			(delete-char 2))
		       ((looking-at (concat "set\\s-+\\(" regex "\\)"))
			(insert "un"))
		       (t
			(message "There is not a negatable set option on this line"))))
		(t
		 (goto-char (match-end 0))
		 (if (> (point) end) (goto-char end))
		 (cond ((looking-at "no")
			(delete-char 2))
		       ((looking-at regex)
			(insert "no"))
		       (t
			(message "There is not a negatable set option on this line")))))
	(message "There is not a set option on this line")) )))

;; (defun gnuplot-set-binding ()
;;   "Interactively select a key sequence for binding to a plot function.
;; This is only useful in gnuplot 3.8 and for plot terminals which support
;; key bindings (i.e. those covered by pm3d)."
;;   (interactive)
;;   (let ((keyseq (read-key-sequence "Choose a key sequence now"))
;; 	(command (read-string "Bind to this command > ")))
;;     (setq keyseq (format "%S" keyseq))
;;     (string-match "keypress-event\\s-+" keyseq)
;;     (setq keyseq (substring keyseq (match-end 0) -2))
;;     ;; need to convert from emacs nomenclature to gnuplot.  what a pain.
;;     (let* ((alist '(("backspace" . "Backspace") ("tab" . "Tab") ("linefeed" . "Linefeed")
;; 		    ("clear" . "Clear") ("return" . "Return") ("pause" . "Pause")
;; 		    ("scroll-lock" . "Scroll_Lock") ("SysReq" . "sys-req")
;; 		    ("escape" . "Escape") ("delete" . "Delete") ("home" . "Home")
;; 		    ("left" . "Left") ("right" . "Right") ("up" . "Up") ("down" . "Down")
;; 		    ("prior" . "PageUp") ("next" . "PageDown") ("end" . "End")
;; 		    ("begin". "Begin")))
;; 	   (match (assoc keyseq alist)))
;;       (if match (setq keyseq (cdr match)))
;;
;;     (insert (format "bind \"%s\" \"%s\"" keyseq command)))))


(defun gnuplot-customize ()
  "Customize `gnuplot-mode'."
  (interactive)
  (if (fboundp 'customize-group)
      (customize-group "gnuplot")
    (message "The Custom library is not installed.")))



;;; --- help from the info file, keyword list + completion, insert function


;; set up stuff for info-look (as suggested by <SE>)
;; modified with suggestion from <MS>
(defun gnuplot-setup-info-look ()
  "Setup info-look in the gnuplot buffer.
Also set the variable `gnuplot-keywords' and do something sensible if
info-look was not available.
See the comments in `gnuplot-info-hook'."
  (interactive)
  (setq gnuplot-keywords-pending nil)
  (if (featurep 'info-look)
      (progn
	(cond ((boundp 'info-lookup-symbol-alist) ; older version
	       (setq info-lookup-symbol-alist
		     (append
		      info-lookup-symbol-alist
		      '((gnuplot-mode
			 "[a-zA-Z][_a-zA-Z0-9]*" nil
			 (("(gnuplot)Top"           nil "[_a-zA-Z0-9]+")
			  ("(gnuplot)Commands"      nil "[_a-zA-Z0-9]+")
			  ("(gnuplot)Functions"     nil "[_a-zA-Z0-9]+")
			  ("(gnuplot)plot"          nil "[_a-zA-Z0-9]+")
			  ("(gnuplot)set-show"      nil "[_a-zA-Z0-9]+")
			  ("(gnuplot)data-file"     nil "[_a-zA-Z0-9]+")
			  ("(gnuplot)smooth"        nil "[_a-zA-Z0-9]+")
			  ("(gnuplot)style"         nil "[_a-zA-Z0-9]+")
			  ("(gnuplot)terminal"      nil "[_a-zA-Z0-9]+")
			  ;;("(gnuplot)General Index" nil "[_a-zA-Z0-9]+")
			  ) "[_a-zA-Z0-9]+" ))) ))
	      (t			; newer version
	       (info-lookup-maybe-add-help
		:mode 'gnuplot-mode :topic 'symbol
		:regexp "[a-zA-Z][_a-zA-Z0-9]*"
		:doc-spec '(("(gnuplot)Top"           nil "[_a-zA-Z0-9]+")
			    ("(gnuplot)Commands"      nil "[_a-zA-Z0-9]+")
			    ("(gnuplot)Functions"     nil "[_a-zA-Z0-9]+")
			    ("(gnuplot)plot"          nil "[_a-zA-Z0-9]+")
			    ("(gnuplot)set-show"      nil "[_a-zA-Z0-9]+")
			    ("(gnuplot)data-file"     nil "[_a-zA-Z0-9]+")
			    ("(gnuplot)smooth"        nil "[_a-zA-Z0-9]+")
			    ("(gnuplot)style"         nil "[_a-zA-Z0-9]+")
			    ("(gnuplot)terminal"      nil "[_a-zA-Z0-9]+")
			    ) )))
	;; this hook is my best way of working with info-look and
	;; allowing multiple versions of the gnuplot-info file.
	;; yes, this is a hassle.
	(run-hooks 'gnuplot-info-hook)
	(let ((there (bufferp (get-buffer "*info*"))))
	  (info-lookup-setup-mode 'symbol 'gnuplot-mode)
	  (or there (and (get-buffer "*info*") (kill-buffer "*info*")))
	  ;; why are these buffers here?  I think that the general
	  ;; user will not want them lying around
	  (and (get-buffer "info dir")    (kill-buffer "info dir"))
	  (and (get-buffer "info dir<2>") (kill-buffer "info dir<2>")))
	(setq gnuplot-keywords (gnuplot-set-keywords-list))
	)

    ;; or do something sensible if info-look is not installed
    (defun info-lookup-interactive-arguments (symbol)
      (message
       "Help is not available.  The gnuplot info file could not be found.")
      (list nil nil))) )


(defun gnuplot-set-keywords-list ()
  "Set `gnuplot-keywords' from `info-lookup-cache'.
Return a list of keywords."
  (let* ((list (cdr (assoc 'symbol info-lookup-cache)))
	 (list (cdr (cdr (assoc 'gnuplot-mode list))))
	 (list (car list))
	 (store ()) item)
    (while list
      (setq item (car (car list))
	    item (format "%s" item) ; keep this line for the sake of
	    store (append (list item) store) ; info-look.el w/o my patch
	    list  (cdr list)))
    (delete "nil" store)
    store ))


(defun gnuplot-complete-keyword ()
  "Perform completion on keyword preceding point.
This is a pretty simple minded completion function.  It is loosely
adapted from `lisp-complete-symbol'."
  (interactive)
  (if gnuplot-keywords-pending		; <HW>
      (gnuplot-setup-info-look))
  (let* ((end (point))
	 (beg (unwind-protect (save-excursion (backward-sexp 1) (point))))
	 (patt (buffer-substring beg end))
	 (pattern (if (string-match "\\([^ \t]*\\)\\s-+$" patt)
		      (match-string 1 patt) patt))
	 (alist (mapcar 'list gnuplot-keywords))
	 (completion (try-completion pattern alist)))
    (cond ((eq completion t))
	  ((null completion)
	   (message "No gnuplot keywords complete \"%s\"" pattern))
	  (t
	   (when (not (string= pattern completion))
	     (delete-region beg end)
	     (insert completion))
	   (let* ((list (all-completions pattern alist))
		  (mess (format "%S could be one of %S" pattern list))
		  (orig (current-buffer))
		  (buff (get-buffer-create " *gnuplot-completions*")))
	     (if (= (length list) 1) (insert " "))
	     (if (< (length mess) (frame-width))
		 (if (> (length list) 1) (message mess))
	       (switch-to-buffer-other-window buff)
	       (insert mess)
	       (fill-region (point-min) (point-max))
	       (goto-char (point-min))
	       (enlarge-window
		(+ 2 (- (count-lines (point-min) (point-max))
			(window-height))))
	       (sit-for (max (length list) 15))
	       (switch-to-buffer orig)
	       (kill-buffer buff)
	       (delete-other-windows) ))) )))

(defun gnuplot-info-lookup-symbol (symbol &optional mode)
  "Wrapper for `info-lookup-symbol'.
Takes SYMBOL and MODE as arguments exactly as `info-lookup-symbol'.
After doing the info lookup, this displays the info file in a window
frame as specified by the value of `gnuplot-info-display'.  If
`gnuplot-info-display' is 'window, then the window will be shrunk to
the size of the info entry if it is smaller than half the height of
the frame."
  (interactive
   (cond (gnuplot-keywords
	  (info-lookup-interactive-arguments 'symbol))
	 (gnuplot-keywords-pending	; <HW>
	  (gnuplot-setup-info-look)
	  (info-lookup-interactive-arguments 'symbol))
	 (t
	  (list nil (message
       "Help is not available.  The gnuplot info file could not be found.")))))
  (if (and (featurep 'info-look) gnuplot-keywords)
      (let ((buff (current-buffer))
	    (info-lookup-other-window-flag
	     (if gnuplot-info-display t nil)))
	(if symbol () (setq symbol "Commands"))
	(info-lookup-symbol symbol mode)
	(cond ((equal gnuplot-info-display 'window)
	       (let ((sw (selected-window))
		     (window-min-height 2))
		 (other-window 1)
		 (enlarge-window
		  (min (- (count-lines (point-min) (point-max)) (window-height))
		       (- (/ (frame-height) 2) (window-height))))
		 (select-window sw)))
	      ((equal gnuplot-info-display 'frame)
	       (switch-to-buffer buff)
	       (delete-other-windows)
	       (or (and gnuplot-info-frame
			(frame-live-p gnuplot-info-frame))
		   (setq gnuplot-info-frame (make-frame)))
	       (select-frame gnuplot-info-frame)
	       (raise-frame gnuplot-info-frame)
	       (if gnuplot-xemacs-p (setq toolbar-info-frame gnuplot-info-frame))
	       (switch-to-buffer "*info*") )) )))


(defun gnuplot-insert (string)
  "Insert STRING at point and display help for for STRING.
Help is not shown if `gnuplot-insertions-show-help-flag' is nil.  The
help shown is for STRING unless STRING begins with the word \"set\" or
\"show\", in which case help is shown for the thing being set or
shown."
  (interactive)
  (cond ((and (not gnuplot-three-eight-p)
	      (string-match "\\(emf\\|p\\(alette\\|m3d\\)\\|vgagl\\)" string))
	 (message "%S is an option introduced in gnuplot 3.8 (You are using %s)"
		  string gnuplot-program-version) )
	(t
	 (insert string)
	 (let ((topic string) term)
	   (if (string-match
		"\\(set\\|show\\)[ \t]+\\([^ \t]+\\)\\(\\s-+\\([^ \t]+\\)\\)?"
		string)
	       (progn
		 (setq topic (downcase (match-string 2 string))
		       term            (match-string 4 string))
		 (if (string= topic "terminal") (setq topic (downcase term)))))
	   (cond ((and (fboundp 'gnuplot-gui-set-options-and-insert)
		       gnuplot-gui-popup-flag)
		  (gnuplot-gui-set-options-and-insert))
		 (gnuplot-insertions-show-help-flag
		  (if gnuplot-keywords-pending		; <HW>
		      (gnuplot-setup-info-look))
		  (gnuplot-info-lookup-symbol topic)) ) )) ) )

(defun gnuplot-toggle-info-display ()
  (interactive)
  (setq gnuplot-insertions-show-help-flag (not gnuplot-insertions-show-help-flag))
  (message (if gnuplot-insertions-show-help-flag
	       "Help will be displayed after insertions."
	     "Help no longer displayed after insertions.")))


;;; --- bug reports
;; grep '(defcustom' gnuplot.el gnuplot-gui.el | awk '{print $2}'
(defun gnuplot-bug-report ()
  "Submit a bug report about `gnuplot-mode' by email.
Please do not send any bug reports about gnuplot itself to the
maintainer of `gnuplot-mode'."
  (interactive)
  (let ((line (make-string 62 ?-)))
    (require 'reporter)
    (and (y-or-n-p
	  "Do you really want to submit an email report about gnuplot? ")
	 (y-or-n-p
	  (concat "Variable values will be written to the message.  "
		  "Don't erase them.  OK? "))
	 (reporter-submit-bug-report
	  (format "%s <%s>" gnuplot-maintainer gnuplot-maintainer-email)
	  (format "gnuplot-mode (version %s)" gnuplot-version)
	  (append      ; variables to display values of in mail
	   '(gnuplot-mode-hook
	     gnuplot-load-hook
	     gnuplot-after-plot-hook
	     gnuplot-info-hook
	     gnuplot-comint-setup-hook
	     gnuplot-program
	     gnuplot-program-version
	     gnuplot-process-name
	     gnuplot-gnuplot-buffer
	     gnuplot-display-process
	     gnuplot-info-display
	     gnuplot-echo-command-line-flag
	     gnuplot-insertions-show-help-flag
	     gnuplot-delay
	     gnuplot-quote-character
	     gnuplot-keywords-when
	     ;;gnuplot-insertions-menu-flag
	     ;;gnuplot-insertions-adornments
	     ;;gnuplot-insertions-plot-options
	     ;;gnuplot-insertions-terminal
	     ;;gnuplot-insertions-x-axis
	     ;;gnuplot-insertions-x2-axis
	     ;;gnuplot-insertions-y-axis
	     ;;gnuplot-insertions-y2-axis
	     ;;gnuplot-insertions-z-axis
	     ;;gnuplot-insertions-parametric-plots
	     ;;gnuplot-insertions-polar-plots
	     ;;gnuplot-insertions-surface-plots
	     gnuplot-toolbar-display-flag
	     gnuplot-toolbar-use-toolbar
	     gnuplot-gui-popup-flag
	     gnuplot-gui-frame-plist
	     gnuplot-gui-frame-parameters
	     gnuplot-gui-fontname-list
	     gnuplot-gui-plot-splot-fit-style
	     ;; plus a few more...
	     gnuplot-comint-recent-buffer
	     gnuplot-version
	     Info-directory-list
	     exec-path
	     features ))
	  nil				; pre-hooks
	  nil				; post-hooks
	  (concat line                  ; salutation
	   "\nInsert your description of the gnuplot-mode bug here.\n"
	   "Please be as specific as possible.\n\n"
	   "There are several known shortcomings of gnuplot-mode.\n"
	   "Many of these have to do with the complicated and inconsistent\n"
	   "syntax of gnuplot itself.  See the document string for the\n"
	   "function `gnuplot-mode' (use `"
	   (substitute-command-keys "\\[describe-function]")
	   "') for details.\n\n"
	   "Note that this bug report form should be used for problems\n"
	   "with gnuplot-mode only.  Problems with gnuplot itself should\n"
	   "be addressed directly to the developers of gnuplot.\n"
	   "The maintainer of gnuplot-mode will not field questions about\n"
	   "gnuplot itself.  Thank you.\n"
	   line)
	  ))))



;;; --- autoloaded functions: gnuplot-mode and gnuplot-make-buffer

;;;###autoload
(defun gnuplot-mode ()
  "Major mode for editing and executing GNUPLOT scripts.
This was written with version 3.7 of gnuplot in mind but it should
work fine with version 3.5 and the various 3.6 beta versions.

Report bugs in `gnuplot-mode' using \\[gnuplot-bug-report].

			    ------O------

The help functions, keyword completion, and several other features
depend upon having the info file properly installed.  The info file
can be made in the document directory of the gnuplot distribution or
is available at the `gnuplot-mode' web page:
    http://feff.phys.washington.edu/~ravel/software/gnuplot-mode/

If the help function does not work properly, you may have an older
version of the gnuplot info file.  Try the suggestion in the document
string for the variable `gnuplot-info-hook'.  See the `gnuplot-mode'
web page for more details.

			    ------O------

There are several known shortcomings of `gnuplot-mode', version 0.5g
and up.  Many of the shortcomings involve the graphical interface
(refered to as the GUI) to setting arguments to plot options.  Here is
a list:

 1.  Currently there is no way for `gnuplot-mode' to know if information
     sent to gnuplot was correctly plotted.
 2.  Indentation is sometimes a bit flaky.
 3.  \"plot\", \"splot\", and \"fit\" are handled in the GUI, but are
     a bit flaky.  Their arguments may not be read correctly from
     existing text, and continuation lines (common for plot and splot)
     are not supported.
 4.  The GUI does not know how to read from continuation lines.
 5.  Comma separated position arguments to plot options are
     unsupported in the GUI.  Colon separated datafile modifiers (used
     for plot, splot, and fit) are not supported either.  Arguments
     not yet supported by the GUI generate messages printed in grey
     text.
 6.  The GUI handling of \"hidden3d\" is flaky and \"cntrparam\" is
     unsupported.

			    ------O------

 Key bindings:
 \\{gnuplot-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gnuplot-mode-map)
  (setq major-mode 'gnuplot-mode
	mode-name "Gnuplot")
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 32)
  (set (make-local-variable 'comment-start-skip) "#[ \t]*")
  (set (make-local-variable 'indent-line-function) 'gnuplot-indent-line)
  (set-syntax-table gnuplot-mode-syntax-table)
  (if (or (fboundp 'hilit-set-mode-patterns)
	  (equal gnuplot-keywords-when 'immediately)) ; <HW>
      (gnuplot-setup-info-look)) ;; <SE>
  (if (fboundp 'hilit-set-mode-patterns) ; deal with hilit19 (ho hum!)
      (let ((keywords (concat "\\b\\(" (mapconcat 'identity
						  gnuplot-keywords "\\|")
			      "\\)\\b")))
	(hilit-set-mode-patterns
	 'gnuplot-mode
	 `(("#.*$" nil comment)
	   ("\\([a-zA-Z0-9_-]+\\)\\(([^)]*)\\)?\\s *=" nil define)
	   ,(list keywords 'nil 'keyword)
	   (hilit-string-find ?\\ string)
	   ))) )
  ;;(if (featurep 'kw-compl)		; old-style keyword completion
  ;;    (setq kw-compl-list gnuplot-keywords
  ;;	    kw-compl-upper-case nil)) ; gnuplot keywords must be lower case
  (if gnuplot-xemacs-p			; deal with font-lock
      (if (fboundp 'turn-on-font-lock) (turn-on-font-lock))
    (progn
      (make-variable-buffer-local 'font-lock-defaults)
      (setq font-lock-defaults '(gnuplot-font-lock-keywords t t))))
;;   (if (and gnuplot-xemacs-p gnuplot-toolbar-display-flag)
;;       (condition-case ()		; deal with the toolbar
;; 	  (and (require 'toolbar)
;; 	       (require 'xpm)
;; 	       (gnuplot-make-toolbar-function))
;; 	(error nil)))
  (if (fboundp 'widget-create)		; gunplot-gui
      (condition-case ()
  	  (require 'gnuplot-gui)
  	(error nil)))
  (setq gnuplot-first-call nil		; a few more details ...
	gnuplot-comint-recent-buffer (current-buffer)
        comint-process-echoes        gnuplot-echo-command-line-flag)
  (run-hooks 'gnuplot-mode-hook)
  ;; the first time we need to figure out which gnuplot we are running
  (if gnuplot-program-version
      (gnuplot-setup-menu-and-toolbar)
    (gnuplot-fetch-version-number)))

;;;###autoload
(defun gnuplot-make-buffer ()
  "Open a new buffer in `gnuplot-mode'.
When invoked, it switches to a new, empty buffer visiting no file
and then starts `gnuplot-mode'.

It is convenient to bind this function to a global key sequence.  For
example, to make the F10 key open a gnuplot script buffer, put the
following in your .emacs file:
     (autoload 'gnuplot-make-buffer \"gnuplot\"
               \"open a buffer in gnuplot mode\" t)
     (global-set-key [(f10)] 'gnuplot-make-buffer)"
  (interactive)
  (switch-to-buffer gnuplot-gnuplot-buffer)
  (gnuplot-mode))

(defun gnuplot-show-version ()
  "Show version number in echo area"
  (interactive)
  (message "gnuplot-mode %s -- URL: %s" gnuplot-version gnuplot-maintainer-url))

(defun gnuplot-show-gnuplot-version ()
  "Show gnuplot program and version number in echo area"
  (interactive)
  (message "You are calling gnuplot %s as %s" gnuplot-program-version gnuplot-program))


;;; That's it! ----------------------------------------------------------------


;;; --- final chores: provide 'gnuplot and run load-hook
;; provide before run-hooks suggested by <DB>
(provide 'gnuplot)
(run-hooks 'gnuplot-load-hook)

;;;============================================================================
;;;
;;; gnuplot.el ends here
