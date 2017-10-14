;;; gnuplot.el --- drive gnuplot from within emacs

;; Copyright (C) 1998, 2011 Phil Type and Bruce Ravel, 1999-2012 Bruce Ravel

;; Author:     Bruce Ravel <bruceravel1@gmail.com> and Phil Type
;; Maintainer: Bruce Ravel <bruceravel1@gmail.com>
;; Created:    June 28 1998
;; Updated:    November 1 2012
;; Version:    0.7.0
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
;; send bug reports to the author (bruceravel1@gmail.com)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:

;;
;; This is a major mode for composing gnuplot scripts and displaying
;; their results using gnuplot.  It supports features of recent
;; Gnuplot versions (4.4 and up), but should also work fine with older
;; versions.
;;
;; This version of gnuplot-mode has been tested mostly on GNU Emacs 23
;; and 24, but should also work with older GNU Emacs versions back to
;; Emacs 21, and XEmacs 21.
;; 
;; This mode offers several tools to help you compose your scripts,
;; including font-lock syntax colorization, a syntax table appropriate
;; to gnuplot, key bindings, pull-down menus, indentation, keyword
;; completions and variable customization using the Custom package.
;; Once the script is composed, there are several function for sending
;; some or all of the script to gnuplot.  The interaction with the
;; gnuplot process is within a comint buffer.  Plots can optionally be
;; displayed within Emacs.
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
;;    C-c C-d       read the gnuplot info file
;;    C-c C-e       show-gnuplot-buffer
;;    C-c C-k       kill gnuplot process
;;    C-c C-u       submit a bug report about gnuplot-mode
;;    C-c C-z       customize gnuplot-mode
;; M-tab or M-ret   complete keyword before point
;;      ret         newline and indent
;;      tab         indent current line
;;    C-c M-i       toggle inline plot display in comint buffer
;;
;; With the exception of the commands for sending commands to Gnuplot,
;; most of the above commands also work in the Gnuplot comint buffer,
;; in addition to the following:
;;     M-C-p        plot the most recent script buffer line-by-line
;;     M-C-f        save the current script buffer and load that file
;;    C-c C-e       pop back to most recent script buffer
;;
;; These two functions are useful for starting up gnuplot-mode:
;;
;; M-x gnuplot-mode
;;         start gnuplot-mode in the current buffer
;;
;; M-x gnuplot-make-buffer
;;         open a new buffer (which is not visiting a file) and start
;;         gnuplot-mode in that buffer
;;
;; Gnuplot-mode now includes context-sensitive support for keyword
;; completion and, optionally, eldoc-mode help text.  See the
;; commentary in gnuplot-context.el for more information.  If you
;; don't find it useful, it can be turned off by customizing
;; `gnuplot-context-sensitive-mode'.
;;
;;
;; ---------------------------------------------------------------------
;;
;; Other lisp files used by gnuplot.el
;;
;; gnuplot-gui.el (written by Bruce):
;;   Defines the GUI interface for setting setting arguments to
;;   gnuplot options.  This uses the widget package extensively.
;;
;; gnuplot-context.el (written by Jonathan, j.j.oddie@gmail.com)
;;   Context-sensitive completion, help lookup and eldoc
;;   strings for gnuplot buffers. Should be byte-compiled before
;;   using. 
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
;;   http://github.com/bruceravel/gnuplot-mode/
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
;; This mode has been tested extensively with GNU Emacs 23 and 24, and
;; in a limited manner with GNU Emacs 22 and XEmacs 21.
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
;;  0.6.1 Sep 13 2011 <BR> Moved to github, updated contact info
;;  0.7.0 Oct 20 2012 <jjo> Contextual completion & help, inline plots,
;;        some other stuff
 
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
;;    Jon Oddie        <jjo> (indentation, inline images, context mode)
;;
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'comint)
(require 'easymenu)
(eval-when-compile (require 'cl))

(declare-function 'eldoc-add-command "eldoc")


;;; --- variable definitions + eval-and-compile clauses

;; We no longer hack around ancient versions of Customize with macros
(require 'custom)

(eval-and-compile  ;; <DB>
  (require 'info))

(eval-and-compile
  (condition-case ()
      (require 'info-look)
    (error nil)))

;; Workaround missing `completion-at-point' in (X)Emacs < 22
(if (not (fboundp 'completion-at-point))
    (defun gnuplot-xemacs-completion-at-point ()
      "Perform completion on keyword preceding point.

This binds `comint-dynamic-complete-functions' to
`gnuplot-comint-complete' and uses `comint-dynamic-complete' to do the
real work."
      (interactive)
      (let ((comint-dynamic-complete-functions
             '(gnuplot-comint-complete)))
        (comint-dynamic-complete))))

;; Work around missing `window-full-height-p'
(if (fboundp 'window-full-height-p)
    (defalias 'gnuplot-window-full-height-p 'window-full-height-p)
  ;; The below is taken from window.el in GNU Emacs
  (defun gnuplot-window-full-height-p (&optional window)
      (unless window
	(setq window (selected-window)))
      (= (window-height window)
	 (window-height (frame-root-window (window-frame window))))))

;; Workaround obsolete `process-kill-without-query'
(if (fboundp 'set-process-query-on-exit-flag)
    (defalias 'gnuplot-set-process-query-on-exit-flag 'set-process-query-on-exit-flag)
  (defalias 'gnuplot-set-process-query-on-exit-flag 'process-kill-without-query))

;; Workaround for missing syntax-ppss in XEmacs
(if (fboundp 'syntax-ppss)
    (defalias 'gnuplot-syntax-ppss 'syntax-ppss)
  (defun gnuplot-syntax-ppss (&optional pos)
      (save-excursion
	(unless pos (setq pos (point)))
	(let ((begin
	       (save-excursion
		 (goto-char pos)
		 (gnuplot-point-at-beginning-of-continuation))))
	  (parse-partial-sexp begin pos)))))


;;;;
(defconst gnuplot-xemacs-p (string-match "XEmacs" (emacs-version)))
(defconst gnuplot-ntemacs-p (string-match "msvc" (emacs-version)))
(defvar   gnuplot-three-eight-p "")

(defconst gnuplot-maintainer "Bruce Ravel")
(defconst gnuplot-maintainer-email "bruceravel1@gmail.com>")
(defconst gnuplot-maintainer-url
  "http://github.com/bruceravel/gnuplot-mode/")
(defconst gnuplot-version "0.7-beta")

(defgroup gnuplot nil
  "Gnuplot-mode for Emacs."
  :prefix "gnuplot-"
  :group 'processes
  :group 'applications
  :group 'local
  :link '(emacs-library-link :tag "Lisp File" "gnuplot.el")
  :link '(url-link :tag "Homepage"
		   "http://github.com/bruceravel/gnuplot-mode/")
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

(defcustom gnuplot-comint-mode-hook nil
  "*Hook run after setting up the gnuplot buffer in gnuplot-comint-mode.
By default this runs the hook named `gnuplot-comint-setup-hook',
for backward compatibility."
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
This is using `gnuplot-fetch-version-number'.")
(defvar gnuplot-program-major-version nil
  "Major version number of gnuplot.
This is found using `gnuplot-fetch-version-number'.")
(defvar gnuplot-program-minor-version nil
  "Minor version number of gnuplot.
This is found using `gnuplot-fetch-version-number'.")

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
\(i.e. no lines get trimmed)."
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
(defcustom gnuplot-basic-offset 4
  "Number of columns to indent lines inside a do- or if-else-block.

This applies only to new-style do- and if-statements using
braces. Commands continued over a linebreak using a backslash are
always indented to line up with the second word on the line
beginning the continued command."
  :group 'gnuplot
  :type 'integer)

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
(defvar gnuplot-keywords-alist nil) ;; For all-completions
(defvar gnuplot-keywords-pending t	;; <HW>
  "A boolean which gets toggled when the info file is probed.")
(defcustom gnuplot-keywords-when 'deferred ;; 'immediately
  "This variable controls when the info file is parsed.
The choices are immediately upon starting gnuplot-mode or the first
time that data is needed."
  :group 'gnuplot
  :type
  '(radio (const :tag "Parse info file when gnuplot-mode starts"    immediately)
	  (const :tag "Parse info file the first time it is needed" deferred)))

(defun gnuplot-set-context-sensitive-completion (_variable value)
  "Customize :set function for `gnuplot-use-context-sensitive-completion'."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'gnuplot-mode 'gnuplot-comint-mode)
        (gnuplot-context-sensitive-mode
         (if value 1 0))))))

(defcustom gnuplot-use-context-sensitive-completion t
  "Non-nil if `gnuplot-context-sensitive-mode' should be enabled by default. 

In context-sensitive mode, gnuplot-mode parses the current
command line to provide smarter completion and documentation
suggestions."
  :group 'gnuplot
  :type 'boolean
  :initialize 'custom-set-default
  :set 'gnuplot-set-context-sensitive-completion
  :link '(emacs-commentary-link "gnuplot-context"))

(defcustom gnuplot-eldoc-mode nil
  "Non-nil if ElDoc mode should be enabled by default in Gnuplot buffers.
ElDoc support requires `gnuplot-context-sensitive-mode' to be
on."
  :group 'gnuplot
  :type 'boolean)
  
(defcustom gnuplot-tab-completion nil
  "Non-nil if TAB should perform completion in gnuplot-mode buffers.

Setting this to `t' sets the `tab-always-indent' variable to the
symbol `complete' in gnuplot-mode buffers."
  :group 'gnuplot
  :type 'boolean)

(defun gnuplot-set-display-mode (variable value &rest args)
  "Customize :set function for `gnuplot-inline-image-mode'."
  (if (and (eq variable 'gnuplot-inline-image-mode)
           value
           (not (gnuplot-display-images-p)))
      (progn
        (message "Displaying images is not supported.")
        (set variable nil))
    (set variable value))
  (gnuplot-setup-comint-for-image-mode))

(defcustom gnuplot-inline-image-mode nil
  "Whether to display Gnuplot output in Emacs.

Possible values are nil, `inline' and `dedicated'.

When this is `nil', Gnuplot output is handled outside of Emacs in
the normal way.  Otherwise, Emacs attempts to capture Gnuplot's
output and display it in a buffer.  Output is inserted inline in
the Gnuplot interaction buffer it this is `inline', in a
separate dedicated buffer if it is `dedicated'.

Use Customize to set this variable, or the commands
`gnuplot-external-display-mode', `gnuplot-inline-display-mode',
and `gnuplot-dedicated-display-mode'."
  :group 'gnuplot
  :type '(radio
          (const :tag "No" nil)
          (const :tag "In Comint buffer" inline)
          (const :tag "In dedicated buffer" dedicated))
  :initialize 'custom-initialize-default
  :set 'gnuplot-set-display-mode)

(defcustom gnuplot-image-format "png"
  "Image format to use for displaying images within Emacs.

This will be sent directly to Gnuplot as a command of the form
\"set terminal <FORMAT>\".  Common values are \"png\" and
\"svg\".

This only has an effect when `gnuplot-inline-image-mode' is
non-nil."
  :group 'gnuplot
  :type 'string
  :initialize 'custom-initialize-default
  :set 'gnuplot-set-display-mode)

(defgroup gnuplot-faces nil
  "Text faces used by gnuplot-mode."
  :prefix "gnuplot-"
  :group 'gnuplot)

(defface gnuplot-prompt-face '((((class color))
                                (:foreground "firebrick"))
                               (t
                                (:bold t :underline t)))
  "Face used for the prompt in the gnuplot process buffer."
  :group 'gnuplot-faces)


;;; --- key bindings and menus

(defvar gnuplot-mode-map
  (let ((map (make-sparse-keymap))
        (completion-function
           (if (fboundp 'completion-at-point)
               'completion-at-point
             'gnuplot-xemacs-completion-at-point)))
    (define-key map "\C-c\C-b"    'gnuplot-send-buffer-to-gnuplot)
    (define-key map "\C-c\C-c"    'comment-region) ; <RF>
    (define-key map "\C-c\C-o"    'gnuplot-gui-set-options-and-insert)
    (define-key map "\C-c\C-w"    'gnuplot-show-version)
    (define-key map "\C-c\C-e"    'gnuplot-show-gnuplot-buffer)
    (define-key map "\C-c\C-f"    'gnuplot-send-file-to-gnuplot)
    (define-key map "\C-c\C-d"    'gnuplot-info-lookup-symbol)
    (define-key map "\C-c\C-i"    'gnuplot-insert-filename)
    (define-key map "\C-c\C-j"    'gnuplot-forward-script-line)
    (define-key map "\C-c\C-k"    'gnuplot-kill-gnuplot-buffer)
    (define-key map "\C-c\C-l"    'gnuplot-send-line-to-gnuplot)
    (define-key map "\C-c\C-n"    'gnuplot-negate-option)
    (define-key map "\C-c\C-p"    'gnuplot-show-gnuplot-version)
    (define-key map "\C-c\C-r"    'gnuplot-send-region-to-gnuplot)
    (define-key map (kbd "C-M-x") 'gnuplot-send-line-to-gnuplot)
    (define-key map "\C-c\C-u"    'gnuplot-bug-report)
    (define-key map "\C-c\C-v"    'gnuplot-send-line-and-forward)
    (define-key map "\C-c\C-z"    'gnuplot-customize)
    (define-key map "\C-i"        'indent-for-tab-command)
    (define-key map "\C-m"        'newline-and-indent)
    (define-key map "\C-c\M-i"    'gnuplot-inline-image-mode)
    (define-key map (kbd "}")     'gnuplot-electric-insert)
    (define-key map "\M-\r" completion-function)
    (define-key map "\M-\t" completion-function)

    (if gnuplot-xemacs-p
        (define-key map '(shift button2) 'gnuplot-gui-mouse-set)
      (define-key map [S-mouse-2] 'gnuplot-gui-mouse-set))

    map))

(defvar gnuplot-mode-menu nil)

(defvar gnuplot-display-options-menu
  (flet ((make-image-setter (type)
           `[,(concat (upcase type) " images")
              (lambda () (interactive) (gnuplot-set-image-format ,type))
              :style toggle
              :selected (eq gnuplot-image-format ,type)]))
    `("Display plot output"
      ["Externally" gnuplot-external-display-mode
       :style toggle
       :selected (null gnuplot-inline-image-mode)]
      ["In Comint buffer" gnuplot-inline-display-mode
       :active (gnuplot-display-images-p)
       :style toggle
       :selected (eq gnuplot-inline-image-mode 'comint)]
      ["In dedicated buffer" gnuplot-dedicated-display-mode
       :style toggle
       :selected (eq gnuplot-inline-image-mode 'dedicated)]
      "---"
      ,@(mapcar #'make-image-setter (list "png" "jpeg" "svg"))
      ["Other image type..." gnuplot-set-image-format])))

(defvar gnuplot-menu
  `("Gnuplot"
    ["Send line to gnuplot"             gnuplot-send-line-to-gnuplot   t]
    ["Send line & move forward"         gnuplot-send-line-and-forward (not (eobp))]
    ["Send region to gnuplot"           gnuplot-send-region-to-gnuplot
     (gnuplot-mark-active)]
    ["Send buffer to gnuplot"           gnuplot-send-buffer-to-gnuplot t]
    ["Send file to gnuplot"             gnuplot-send-file-to-gnuplot t]
    "---"
    ,gnuplot-display-options-menu
    ["Contextual completion and help"   gnuplot-context-sensitive-mode
     :style toggle
     :selected gnuplot-context-sensitive-mode]
    ["Echo area help (eldoc-mode)" eldoc-mode
     :active gnuplot-context-sensitive-mode
     :style toggle
     :selected eldoc-mode]
    "---"
    ["Insert filename at point"         gnuplot-insert-filename t]
    ["Negate set option"                gnuplot-negate-option t]
    ;;["Set key binding"                gnuplot-set-binding gnuplot-three-eight-p]
    ["Keyword help"                     gnuplot-info-lookup-symbol
     (or gnuplot-keywords gnuplot-keywords-pending)]
    ["Quick help for thing at point"    gnuplot-help-function
     gnuplot-context-sensitive-mode]
    ["Info documentation on thing at point"
     gnuplot-info-at-point
     gnuplot-context-sensitive-mode]
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
    )
  "Menu for `gnuplot-mode'.")



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

(defvar gnuplot-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?& "." table ) ; rarely used
    (modify-syntax-entry ?^ "." table ) ; operators
    (modify-syntax-entry ?| "." table ) ; in gnuplot,
    (modify-syntax-entry ?& "." table ) ; (by me,
    (modify-syntax-entry ?? "." table ) ;  anyway...)
    (modify-syntax-entry ?~ "." table ) ;

    (modify-syntax-entry ?_ "w" table )

    ;; In GNU Emacs >=24 we can use `syntax-propertize-function' to
    ;; accurately scan for strings and comments (see
    ;; `gnuplot-syntax-propertize').  If there's no
    ;; `syntax-propertize', fall back to using the built-in parser and
    ;; making ", ', and # string or comment delimiters as normal.
    (if (not (boundp 'syntax-propertize-function))
	(progn
	  (modify-syntax-entry ?\' "\"" table)
	  (modify-syntax-entry ?# "<" table)
	  (modify-syntax-entry ?\n ">" table)
	  (modify-syntax-entry ?\\ "\\" table))

      ;; When syntax-propertize is available, ", ', and # should be
      ;; punctuation so that the built-in parser doesn't interfere
      ;; with the syntax-propertize search.
      (modify-syntax-entry ?\" "." table)
      (modify-syntax-entry ?\' "." table)
      (modify-syntax-entry ?` "." table)
      (modify-syntax-entry ?\\ "." table))

    table)

  "Syntax table in use in `gnuplot-mode' buffers.
This is the same as the standard syntax table except that ` and _
are word characters, and math operators are punctuation
characters.")

;; Macro to generate efficient regexps for keyword matching
;;
;; These regular expressions treat the gnuplot vocabulary as complete
;; words.  Although gnuplot will recognise unique abbreviations, these
;; regular expressions will not.
(defmacro gnuplot-make-regexp (list)
  `(regexp-opt ,list 'words))

;; Lists of gnuplot keywords for syntax coloring etc.
(defvar gnuplot-keywords-builtin-functions
  '("abs" "acosh" "acos" "arg" "asinh" "asin" "atan" "atanh" "atan2" "besj1"
    "besj0" "besy1" "besy0" "ceil" "column" "cosh" "cos" "erfc" "erf" "exp"
    "floor" "gamma" "ibeta" "igamma" "imag" "int" "inverf" "invnorm" "lgamma"
    "log" "log10" "norm" "rand" "real" "sgn" "sinh" "sin" "sqrt" "tanh" "tan"
    "tm_hour" "tm_mday" "tm_min" "tm_mon" "tm_sec" "tm_wday" "tm_yday" "tm_year"
    "valid" "EllipticPi" "EllipticE" "EllipticK" "words" "word" "value"
    "timecolumn" "substr" "strstrt" "strptime" "strlen" "stringcolumn"
    "strftime" "sprintf" "lambertw" "gprintf" "exists" "defined" "columnhead")

  "List of GNUPLOT built-in functions, as strings.

These are highlighted using `font-lock-function-name-face'.")

(defvar gnuplot-keywords-plotting
  '("axes" "every" "index" "lw" "lt" "ls" "linestyle" "linetype" "linewidth"
    "notitle" "pt" "ps" "pointsize" "pointtype" "smooth" "thru" "title" "using"
    "with" "noautoscale" "volatile" "matrix" "nonuniform" "binary" "fillstyle"
    "linecolor" "pointinterval" "nosurface" "nocontours" "nohidden3d")
  "List of GNUPLOT keywords associated with plotting, as strings.

These are highlighted using `font-lock-type-face'.
This list does not include plotting styles -- for that, see 
`gnuplot-keywords-plotting-styles'")

(defvar gnuplot-keywords-plotting-styles
  '("boxerrorbars" "boxes" "boxxyerrorbars" "candlesticks" "dots" "errorbars"
    "financebars" "fsteps" "histeps" "impulses" "lines" "linespoints" "points"
    "steps" "vector" "xerrorbars" "xyerrorbars" "yerrorbars" "vectors"
    "filledcurves" "labels" "rgbalpha" "rgbimage" "image" "circles" "pm3d"
    "histograms" "xyerrorlines" "xerrorlines" "errorlines" "yerrorlines")

  "List of GNUPLOT plotting styles, as strings.

These are highlighted using `font-lock-function-name-face'.")

(defvar gnuplot-keywords-misc
  '("bind" "cd" "clear" "exit" "fit" "help" "history" "load" "pause" "print"
    "pwd" "quit" "replot" "save" "set" "show" "unset" "if" "else" "do" "update"
    "undefine" "test" "system" "raise" "lower" "eval" "shell" "reset" "reread"
    "refresh" "call")
  "List of GNUPLOT miscellaneous commands, as strings.

These are highlighted using `font-lock-constant-face'.")

(defvar gnuplot-keywords-negatable-options
  '("arrow" "autoscale" "border" "clabel" "clip" "contour" "dgrid3d" "grid"
    "hidden3d" "historysize" "key" "label" "linestyle" "logscale" "mouse"
    "multiplot" "mx2tics" "mxtics" "my2tics" "mytics" "mztics" "offsets" "polar"
    "surface" "timestamp" "title" "x2dtics" "x2mtics" "x2tics" "x2zeroaxis"
    "xdtics" "xmtics" "xtics" "xzeroaxis" "y2dtics" "y2mtics" "y2tics"
    "y2zeroaxis" "ydtics" "ymtics" "ytics" "yzeroaxis" "zdtics" "zmtics" "ztics"
    "zzeroaxis")

  "List of gnuplot options which can be negated using `gnuplot-negate-option'")

(defvar gnuplot-negatable-options-regexp
  (gnuplot-make-regexp gnuplot-keywords-negatable-options))

;; Set up colorization for gnuplot.
;; This handles font-lock for emacs and xemacs.
(defvar gnuplot-font-lock-keywords nil)
(defvar gnuplot-font-lock-syntactic-keywords nil)
(defvar gnuplot-font-lock-defaults nil)

(when (featurep 'font-lock)		; <KL>
  (setq gnuplot-font-lock-keywords
	(list
	 ;; stuff in brackets, sugg. by <LB>
	 '("\\[\\([^]]+\\)\\]" 1 font-lock-constant-face)

	 ;; variable/function definitions
	 '("\\(\\(\\sw\\|\\s_\\)+\\s-*\\((\\s-*\\(\\sw\\|\\s_\\)*\\s-*\\(,\\s-*\\sw*\\)*\\s-*)\\)?\\s-*=\\)[^=]"
	   1 font-lock-variable-name-face)

	 ;; built-in function names
	 (cons (gnuplot-make-regexp gnuplot-keywords-builtin-functions)
	       font-lock-function-name-face)

	 ;; reserved words associated with plotting <AL>
	 (cons (gnuplot-make-regexp gnuplot-keywords-plotting)
	       font-lock-type-face)
	 (cons (gnuplot-make-regexp gnuplot-keywords-plotting-styles)
	       font-lock-function-name-face)

	 ;; (s)plot -- also thing (s)plotted
	 '("\\<s?plot\\>" . font-lock-keyword-face)
	 ;; '("\\<s?plot\\s-+\\([^'\" ]+\\)[) \n,\\\\]"
	 ;;   1 font-lock-variable-name-face)

	 ;; other common commands
	 (cons (gnuplot-make-regexp gnuplot-keywords-misc)
	       font-lock-constant-face)
	 (cons "!.*$" font-lock-constant-face))) ; what is this for? jjo
      
  (setq gnuplot-font-lock-defaults 
	'(gnuplot-font-lock-keywords
	  nil				; Use syntactic fontification
	  t				; Use case folding
	  nil				; No extra syntax
	  ;; calls `gnuplot-beginning-of-continuation'
	  ;; to find a safe place to begin syntactic highlighting
	  beginning-of-defun))
  
  ;; Set up font-lock for Xemacs
  ;; For GNU Emacs, this is done in `gnuplot-mode'
  (if gnuplot-xemacs-p
      (put 'gnuplot-mode 'font-lock-defaults
	   gnuplot-font-lock-defaults)))

;; Some corner cases in Gnuplot's comment and string syntax are
;; difficult to handle accurately using Emacs's built-in syntax tables
;; and parser:
;;
;; - strings can continue over several lines, but only by using a
;;   backslash to escape the newline
;; 
;; - double-quoted strings can contain escaped quotes, \", and escaped
;;   backslashes, \\; but in single-quoted strings the quote is
;;   escaped by doubling it, '', and backslash is only special at
;;   end-of-line
;; 
;; - either type of string can end at newline without needing a
;; - closing delimiter
;; 
;; - comments continue over continuation lines
;;
;; The following syntax-propertize rules should accurately mark string
;; and comment boundaries using the "generic string fence" and
;; "generic comment fence" syntax properties.  When syntax-propertize
;; is unavailable (on Emacs versions <24), we fall back to using the
;; normal syntax-table parser, which is accurate enough for most
;; normal cases. (See the definition of `gnuplot-mode-syntax-table'.)
(defalias 'gnuplot-syntax-propertize
    (when (fboundp 'syntax-propertize-rules)
      (syntax-propertize-rules
       ;; Double quoted strings
       ((rx
         (group "\"")
         (* (or (seq "\\" anything)
                (not (any "\"" "\n"))))
         (group (or "\"" "\n" buffer-end)))
        (1 "|") (2 "|"))

       ;; Single quoted strings
       ((rx
         (group "'")
         (* (or (seq "\\" "\n")
                "''"
                (not (any "'" "\n"))))
         (group (or "'" "\n" buffer-end)))
        (1 "|") (2 "|"))

       ;; Comments
       ((rx
         (group "#")
         (* (or (seq "\\" "\n")
                any))
         (or (group "\n") buffer-end))
        (1 "!") (2 "!")))))

(defun gnuplot-syntax-propertize-extend-region (start end)
  "Expand the region to syntax-propertize for strings and comments.

Ensures that the region being searched begins and ends outside of
any lines continued with a backslash.

This function is added to
`syntax-propertize-extend-region-functions' in gnuplot-mode
buffers."
  (let ((continuation-start
         (min start
              (gnuplot-point-at-beginning-of-continuation start)))
        (continuation-end
         (max end
              (gnuplot-point-at-end-of-continuation end))))
    (if (and (= continuation-start start)
             (= continuation-end end))
        nil
      (cons continuation-start continuation-end))))

;; Parsing utilities to tell if we are inside a string or comment
(defun gnuplot-in-string (&optional where)
  "Returns non-nil if the text at WHERE is within a string.

If WHERE is omitted, defaults to text at point.
This is a simple wrapper for `syntax-ppss'."
  (save-excursion
    (let ((parse-state (gnuplot-syntax-ppss where)))
      (nth 3 parse-state))))

(defun gnuplot-in-comment (&optional where)
  "Returns non-nil if the text at WHERE is within a comment.

If WHERE is omitted, defaults to text at point.
This is a simple wrapper for `syntax-ppss'."
  (save-excursion
    (let ((parse-state (gnuplot-syntax-ppss where)))
      (nth 4 parse-state))))

(defun gnuplot-in-string-or-comment (&optional where)
  "Returns non-nil if the text at WHERE is within a string or comment.

If WHERE is omitted, defaults to text at point.
This is a simple wrapper for `syntax-ppss'."

  (save-excursion
    (let ((parse-state (gnuplot-syntax-ppss where)))
      (or (nth 3 parse-state)
	  (nth 4 parse-state)))))

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
  (gnuplot-fetch-version-number)
  (setq gnuplot-comint-recent-buffer (current-buffer))

  ;; Create a gnuplot frame if needed
  (if (equal gnuplot-display-process 'frame)
      (or (and gnuplot-process-frame
	       (frame-live-p gnuplot-process-frame))
	  (let ((frame (selected-frame)))
	    (setq gnuplot-process-frame (make-frame))
	    (select-frame gnuplot-process-frame)
	    (switch-to-buffer gnuplot-buffer)
	    (delete-other-windows)
	    (select-frame frame))))

  (let ((list (gnuplot-split-string string)))
    (with-current-buffer (get-buffer gnuplot-buffer)
      (goto-char (point-max))
      ;; bruce asks: what is this next line for?
      (set-marker (process-mark gnuplot-process) (point-marker))
      (sleep-for (* 20 gnuplot-delay))
      (while list
	(insert (car list))
	(comint-send-input)
	(sleep-for gnuplot-delay)
	(setq list (cdr list))
	(goto-char (point-max))))

    (cond ((equal gnuplot-display-process 'window)
	   (gnuplot-display-and-recenter-gnuplot-buffer))
	  ((equal gnuplot-display-process 'frame)
	   ;;(raise-frame gnuplot-process-frame)
	   (with-selected-frame gnuplot-process-frame
	     (gnuplot-display-and-recenter-gnuplot-buffer))))

    (setq gnuplot-recently-sent text)
    (run-hooks 'gnuplot-after-plot-hook)))

(defun gnuplot-display-and-recenter-gnuplot-buffer ()
  "Make sure the gnuplot comint buffer is displayed, and
move point to the end if necessary"
  (save-selected-window
    (select-window (display-buffer (get-buffer gnuplot-buffer)))
    (goto-char (point-max))
    (unless (pos-visible-in-window-p (point) (selected-window)) (recenter 5))))

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
	 (let (start end)
	   (save-excursion 
	     ;; go to start of continued command, or beginning of line
	     ;; if this is not a continuation of a previous line <JJO>
	     (gnuplot-beginning-of-continuation)
	     (setq start (point))
	     (end-of-line)
	     (while (save-excursion
		      (backward-char)
		      (looking-at "\\\\"))	; go to end of last continuation line
	       (end-of-line 2))
	     (beginning-of-line 2)
	     (setq end (point)))
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

(defun gnuplot-send-line-and-newline ()
  "Call `gnuplot-send-line-to-gnuplot' and insert a new line."
  (interactive)
  (end-of-line)
  (gnuplot-send-line-to-gnuplot)
  (insert "\n"))

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
    (setq num (1- num))))

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
  (if (not (buffer-live-p gnuplot-comint-recent-buffer))
      (message "Script buffer has been deleted.")
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
      (comint-send-input))))

(defun gnuplot-save-and-plot-from-comint ()
  "Send a current script to gnuplot from the process buffer.
This sends the most recently used gnuplot script to gnuplot using the
\"load\" command.  This function first saves the script buffer to a
file, prompting for a filename if one is not associated with the script
buffer.  Then it sends a load command to gnuplot using the name of the
file visited by the script buffer."
  (interactive)
  (if (not (buffer-live-p gnuplot-comint-recent-buffer))
      (message "Script buffer has been deleted.")
    (let (fname)
      (with-current-buffer gnuplot-comint-recent-buffer
	(save-buffer)
	(setq fname (buffer-file-name)))
      (goto-char (point-max))
      (insert (format "load '%s'" fname))
      (comint-send-input))))

(defun gnuplot-pop-to-recent-buffer ()
  "Switch to the most recently-plotted gnuplot script buffer."
  (interactive)
  (when (buffer-live-p gnuplot-comint-recent-buffer)
    (pop-to-buffer gnuplot-comint-recent-buffer)))

(defun gnuplot-trim-gnuplot-buffer ()
  "Trim lines from the beginning of the *gnuplot* buffer.
This keeps that buffer from growing excessively in size.  Normally,
this function is attached to `gnuplot-after-plot-hook'"
  (if (> gnuplot-buffer-max-size 0)
      (with-current-buffer gnuplot-buffer
	(let ((nlines (count-lines (point-min) (point-max)))
	      (kill-whole-line t))
	  (while (> nlines gnuplot-buffer-max-size)
	    (goto-char (point-min))
	    (kill-line)
	    (setq nlines (1- nlines)))
	  (goto-char (point-max)) ))))
(add-hook 'gnuplot-after-plot-hook 'gnuplot-trim-gnuplot-buffer nil nil)


;;; --- functions controlling the gnuplot process

;; Menu for the comint-mode buffer
(defvar gnuplot-comint-menu
  `("Gnuplot"
    ["Plot most recent gnuplot buffer"		gnuplot-plot-from-comint
     (buffer-live-p gnuplot-comint-recent-buffer)]
    ["Save and plot most recent gnuplot buffer"	gnuplot-save-and-plot-from-comint
     (buffer-live-p gnuplot-comint-recent-buffer)]
    "---"
    ,gnuplot-display-options-menu
    ["Contextual completion and help"           gnuplot-context-sensitive-mode
     :style toggle
     :selected gnuplot-context-sensitive-mode]
    ["Echo area help (eldoc-mode)" eldoc-mode
     :active gnuplot-context-sensitive-mode
     :style toggle
     :selected eldoc-mode]
    "---"
    ["Insert filename at point"			gnuplot-insert-filename t]
    ["Negate set option"			gnuplot-negate-option t]
    ["Keyword help"				gnuplot-info-lookup-symbol
     (or gnuplot-keywords gnuplot-keywords-pending)]
    ["Quick help for thing at point"            gnuplot-help-function
     gnuplot-context-sensitive-mode]
    ["Info documentation on thing at point"
     gnuplot-info-at-point
     gnuplot-context-sensitive-mode]
    ["Switch to recent gnuplot script buffer"	gnuplot-pop-to-recent-buffer
     (buffer-live-p gnuplot-comint-recent-buffer)]
    "---"
    ["Customize gnuplot"			gnuplot-customize t]
    ["Submit bug report"			gnuplot-bug-report t]
    ["Show gnuplot-mode version"		gnuplot-show-version t]
    ["Show gnuplot version"			gnuplot-show-gnuplot-version t]
    "---"
    ["Kill gnuplot"				gnuplot-kill-gnuplot-buffer t]
    ))

;; Major mode `gnuplot-comint-mode' for the interaction buffer
(define-derived-mode gnuplot-comint-mode comint-mode "Gnuplot interaction"
  "Major mode for interacting with a gnuplot process in a buffer.

This sets font-lock and keyword completion in the comint/gnuplot
buffer."

  (set-syntax-table gnuplot-mode-syntax-table)

  (if gnuplot-xemacs-p			; deal with font-lock
      (if (fboundp 'turn-on-font-lock) (turn-on-font-lock))
    (progn
      (setq font-lock-defaults gnuplot-font-lock-defaults)
      (set (make-local-variable 'parse-sexp-lookup-properties) t)
      (set (make-local-variable 'syntax-propertize-function)
           #'gnuplot-syntax-propertize)))

  ;; XEmacs needs the call to make-local-hook
  (when (and (featurep 'xemacs)
	     (fboundp 'make-local-hook))
    (make-local-hook 'kill-buffer-hook))
  (add-hook 'kill-buffer-hook 'gnuplot-close-down nil t)

  (add-hook 'comint-output-filter-functions
	    'comint-postoutput-scroll-to-bottom
	    nil t)
  (add-hook 'comint-output-filter-functions
	    'gnuplot-protect-prompt-fn
	    nil t)

  ;; Set up completion, using completion-at-point in recent Emacs,
  ;; comint-dynamic-complete in older Emacs
  (if (and (>= emacs-major-version 24)
           (>= emacs-minor-version 1))
      (add-hook 'completion-at-point-functions #'gnuplot-completion-at-point nil t)
    (add-hook 'comint-dynamic-complete-functions 'gnuplot-comint-complete nil t))

  ;; Set up menu (see below)
  (easy-menu-define
   gnuplot-comint-mode-menu gnuplot-comint-mode-map "Menu used in gnuplot-comint-mode"
   gnuplot-comint-menu)
  (easy-menu-add gnuplot-comint-mode-menu gnuplot-comint-mode-map))

;; Key bindings for gnuplot-comint-mode
(define-key gnuplot-comint-mode-map "\M-\C-p"	'gnuplot-plot-from-comint)
(define-key gnuplot-comint-mode-map "\M-\C-f"	'gnuplot-save-and-plot-from-comint)
(define-key gnuplot-comint-mode-map "\C-d"	'gnuplot-delchar-or-maybe-eof)
(let ((completion-function
       (if (and (>= emacs-major-version 24)
                (>= emacs-minor-version 1))
           'completion-at-point
         'comint-dynamic-complete)))
  (define-key gnuplot-comint-mode-map "\M-\r"	completion-function)
  (define-key gnuplot-comint-mode-map "\M-\t"	completion-function))
(define-key gnuplot-comint-mode-map "\C-c\C-d"  'gnuplot-info-lookup-symbol)
(define-key gnuplot-comint-mode-map "\C-c\C-w"	'gnuplot-show-version)
(define-key gnuplot-comint-mode-map "\C-c\C-i"	'gnuplot-insert-filename)
(define-key gnuplot-comint-mode-map "\C-c\C-n"	'gnuplot-negate-option)
(define-key gnuplot-comint-mode-map "\C-c\C-p"	'gnuplot-show-gnuplot-version)
(define-key gnuplot-comint-mode-map "\C-c\C-u"	'gnuplot-bug-report)
(define-key gnuplot-comint-mode-map "\C-c\C-z"	'gnuplot-customize)
(define-key gnuplot-comint-mode-map "\C-c\C-e"	'gnuplot-pop-to-recent-buffer)
(define-key gnuplot-comint-mode-map "\C-c\M-i"  'gnuplot-inline-image-mode)

;; Menu for gnuplot-comint-mode
(defvar gnuplot-comint-mode-menu nil
  "Menu for `gnuplot-comint-mode'.")

;; Switch to the gnuplot program buffer
(defun gnuplot-make-gnuplot-buffer ()
  "Switch to the gnuplot program buffer or create one if none exists."
  (unless (and gnuplot-process (eq (process-status gnuplot-process) 'run)
               gnuplot-buffer (buffer-live-p gnuplot-buffer))
    (message "Starting gnuplot plotting program...")
    (setq gnuplot-buffer (make-comint gnuplot-process-name gnuplot-program)
          gnuplot-process (get-buffer-process gnuplot-buffer))
    (gnuplot-set-process-query-on-exit-flag gnuplot-process nil)
    (with-current-buffer gnuplot-buffer
      (gnuplot-comint-mode)
      (when gnuplot-inline-image-mode
        (sleep-for gnuplot-delay)
        (gnuplot-setup-comint-for-image-mode)))
    (message "Starting gnuplot plotting program...Done")))


(defun gnuplot-fetch-version-number ()
  "Determine the installed version of the gnuplot program.

If `gnuplot-program-version' is already set, does
nothing. Otherwise, runs `gnuplot-program' and searches the text
printed at startup for a string like \"Version N.N\".

Sets the variables `gnuplot-program-version',
`gnuplot-program-major-version', `gnuplot-program-minor-version',
and `gnuplot-three-eight-p'.

If the version number cannot be determined by this method, it
defaults to 3.7."
  (unless gnuplot-program-version
    (message "gnuplot-mode %s -- determining gnuplot version ......"
	     gnuplot-version)
    (with-temp-buffer
      (insert "show version")
      (call-process-region (point-min) (point-max)
			   gnuplot-program t (current-buffer))
      (goto-char (point-min))
      (if (and (re-search-forward "[Vv]ersion\\s-+" (point-max) t)
	       (looking-at "\\([0-9]\\)\\.\\([0-9]+\\)"))
	  (progn
	    (setq gnuplot-program-version (match-string 0)
		  gnuplot-program-major-version (string-to-number
						 (match-string 1))
		  gnuplot-program-minor-version (string-to-number
						 (match-string 2))
		  gnuplot-three-eight-p
		  (>= (string-to-number gnuplot-program-version) 3.8)))

	;; Guess v3.7 if something went wrong
	(message "Warning: could not determine gnuplot version, guessing 3.7")
	(setq gnuplot-program-version "3.7"
	      gnuplot-program-major-version 3
	      gnuplot-program-minor-version 7
	      gnuplot-three-eight-p nil)))
    
    ;; Setup stuff that depends on version number
    (gnuplot-setup-menu-and-toolbar)))

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
	   (substitute-command-keys "\\[gnuplot-bug-report]")))

(defvar gnuplot-prompt-regexp
  (regexp-opt '("gnuplot> " "multiplot> "))
  "Regexp for recognizing the GNUPLOT prompt")

(defun gnuplot-protect-prompt-fn (string)
  "Prevent the Gnuplot prompt from being deleted or overwritten.
STRING is the text as originally inserted in the comint buffer."
  (save-excursion
    (let ((b (progn
               (goto-char (point-max))
               (beginning-of-line)
               (point)))
          e)
      (if (re-search-forward gnuplot-prompt-regexp (point-max) t)
          (progn
            (setq e (point))
            (put-text-property b e 'rear-nonsticky '(read-only intangible face))
            (put-text-property b e 'intangible t)
            (put-text-property b e 'face 'gnuplot-prompt-face)
            ;;(put-text-property b e 'read-only t)
	    )) )))

(defun gnuplot-close-down ()
  "Tidy up when deleting the gnuplot buffer."
  (if (and gnuplot-process
	   (eq (process-status gnuplot-process) 'run)) ; <SE>
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
  (unless (and gnuplot-buffer (get-buffer gnuplot-buffer))
    (gnuplot-make-gnuplot-buffer))
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
	 (switch-to-buffer gnuplot-buffer))))


;;; Support for displaying plotted images within Emacs

(defvar gnuplot-inline-image-filename nil
  "Name of the current Gnuplot output file.")

(defvar gnuplot-image-buffer-name "*gnuplot output*")

(defun gnuplot-display-images-p ()
  ;; Inline images require GNU Emacs.
  (and (not (featurep 'xemacs))
       (fboundp 'display-images-p)
       (display-images-p)))

(defun gnuplot-external-display-mode ()
  (interactive)
  (gnuplot-set-display-mode 'gnuplot-inline-image-mode nil))

(defun gnuplot-inline-display-mode ()
  (interactive)
  (gnuplot-set-display-mode 'gnuplot-inline-image-mode 'inline))

(defun gnuplot-dedicated-display-mode ()
  (interactive)
  (gnuplot-set-display-mode 'gnuplot-inline-image-mode 'dedicated))

(defun gnuplot-set-image-format (format)
  (interactive "sGnuplot image format: ")
  (gnuplot-set-display-mode 'gnuplot-image-format format)
  (unless gnuplot-inline-image-mode
    (message "Setting will take effect when plots are displayed in Emacs")))

(defun gnuplot-setup-comint-for-image-mode ()
  (when (and gnuplot-buffer (buffer-live-p gnuplot-buffer)
             (get-buffer-process gnuplot-buffer))
    (with-current-buffer gnuplot-buffer
      (if gnuplot-inline-image-mode
          (progn
            (gnuplot-send-hiding-output
             (format "set terminal %s\n" gnuplot-image-format))
            (gnuplot-inline-image-set-output)
            (add-hook 'comint-output-filter-functions
                      'gnuplot-insert-inline-image-output nil t))
        (gnuplot-send-hiding-output "set terminal pop\n")
        (remove-hook 'comint-output-filter-functions
                     'gnuplot-insert-inline-image-output t)))))

(defun gnuplot-inline-image-set-output ()
  "Set Gnuplot's output file to `gnuplot-inline-image-filename'."
  (let ((tmp (make-temp-file "gnuplot")))
    (setq gnuplot-inline-image-filename tmp)
    (gnuplot-send-hiding-output (format "set output '%s'\n" tmp))))

(defvar gnuplot-inhibit-filter nil)

(defun gnuplot-insert-inline-image-output (string)
  "Insert Gnuplot graphical output in the gnuplot-comint buffer.

Called via `comint-preoutput-filter-functions' hook when
`gnuplot-inline-image-mode' is enabled. Checks the status of the
file `gnuplot-inline-image-filename'; if it exists and has
nonzero size, inserts it as an inline image, stores a new
temporary filename in `gnuplot-inline-image-filename', and
updates Gnuplot with the appropriate 'set output' command."
  (unless gnuplot-inhibit-filter        ; Prevent recursively entering this filter
    (let ((gnuplot-inhibit-filter t))   ; (causing an infinite loop)
      (save-excursion
        (goto-char (point-max))
        (beginning-of-line)
        (when (looking-at gnuplot-prompt-regexp)
          (let* ((filename gnuplot-inline-image-filename)
                 (size (nth 7 (file-attributes filename))))
            (when (and size (> size 0))
              (gnuplot-send-hiding-output "set output\n") ; Flush output file
              (sit-for 0.1)             ; Hack: wait for Gnuplot IO to finish
              (ecase gnuplot-inline-image-mode
                (nil nil)
                (inline
                 (ignore-errors
                   (let ((image (create-image filename)))
                     (beginning-of-line)
                     (insert-image image)
                     (insert "\n")
                     (gnuplot-inline-image-set-output))))
                (dedicated
                 (with-current-buffer
                     (get-buffer-create gnuplot-image-buffer-name)
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (insert-file-contents filename)
                     (ignore-errors (normal-mode))
                     (display-buffer (current-buffer))
                     (gnuplot-inline-image-set-output))))))))))))

;;; Send commands to GNUPLOT silently & without generating an extra prompt
(defvar gnuplot-hidden-output-buffer " *gnuplot output*")
  
(defun gnuplot-send-hiding-output (string)
  "Send STRING to the running Gnuplot process invisibly."
  (with-current-buffer gnuplot-buffer
    (add-hook 'comint-preoutput-filter-functions
	      'gnuplot-discard-output nil t))
  (with-current-buffer (get-buffer-create gnuplot-hidden-output-buffer)
    (erase-buffer))
  (comint-send-string (get-buffer-process gnuplot-buffer) string))

(defun gnuplot-discard-output (string)
  ;; Temporary preoutput filter for hiding Gnuplot output & prompt.
  ;; Accumulates output in a buffer until it finds the next prompt,
  ;; then removes itself from comint-preoutput-filter-functions.
  (with-current-buffer
      (get-buffer-create gnuplot-hidden-output-buffer)
    (insert string)
    (when (looking-back gnuplot-prompt-regexp)
      (with-current-buffer gnuplot-buffer
	(remove-hook 'comint-preoutput-filter-functions
		     'gnuplot-discard-output t))))
  "")



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


;; Adjust indentation for the line containing point
(defun gnuplot-indent-line ()
  "Set indentation in gnuplot buffer.
For most lines, set indentation to previous level of indentation.
Add additional indentation for continuation lines."
  (interactive)
  (let (indent)
    (if (gnuplot-in-string (point-at-bol))
	;; Continued strings begin at left margin
	(setq indent 0)
      (save-excursion 
	(if (gnuplot-continuation-line-p)
	    ;; This is a continuation line. Indent to the same level as
	    ;; the second word on the line beginning this command (i.e.,
	    ;; the first non-whitespace character after whitespace)
	    (progn
	      (gnuplot-beginning-of-continuation)
	      (back-to-indentation) 
	      (re-search-forward "\\S-+\\s-+" (point-at-eol) 'end-at-limit)
	      (setq indent (- (point) (point-at-bol))))

          ;; Not a continuation line; indent according to block
          ;; nesting depth
          (save-excursion
            (condition-case nil
                (progn
                  (beginning-of-line)
                  (skip-syntax-forward "-" (point-at-eol))
                  (if (looking-at "\\s)") (forward-char))
                  (backward-up-list)
                  (gnuplot-beginning-of-continuation)
                  (setq indent (+ gnuplot-basic-offset (current-indentation))))
              (error
               (setq indent 0)))))))

    ;; Set indentation
    (save-excursion 
      (indent-line-to indent))

    ;; Move point after indentation when at beginning of line
    (let ((point-at-indent (+ (point-at-bol) indent)))
      (when (< (point) point-at-indent) (goto-char point-at-indent)))))

;; Adjust indentation on inserting a close brace
;; The blink-paren fix is stolen from cc-mode
(defun gnuplot-electric-insert (arg)
  (interactive "*p")
  (let ((old-blink-paren blink-paren-function)
        (blink-paren-function nil))
    (self-insert-command arg)
    (gnuplot-indent-line)
    (when old-blink-paren (funcall old-blink-paren))))

;;
;; Functions for finding the start and end of continuation blocks
;;

;; Check if line containing point is a continuation
(defun gnuplot-continuation-line-p ()
  "Return t if the line containing point is a continuation of the previous line."
  (save-excursion
    (condition-case ()
	(progn
	  (end-of-line 0)
	  (backward-char)
	  (looking-at "\\\\"))
      (error nil))))

;; Move point to start of continuation block
(defun gnuplot-beginning-of-continuation ()
  "Move point to the beginning of the continuation lines containing point.

If not in a continuation line, move point to beginning of line."
  (beginning-of-line)
  (while (gnuplot-continuation-line-p)
    (beginning-of-line 0)))

;; Move point to end of continuation block
(defun gnuplot-end-of-continuation ()
  "Move point to the end of the continuation lines containing point.

If there are no continuation lines, move point to end-of-line."
  (end-of-line)
  (unless (bobp)
    (catch 'eob
      (while (save-excursion (backward-char)
			     (looking-at "\\\\"))
	(end-of-line 2)
	(if (eobp) (throw 'eob nil))))))

;; Save-excursion wrappers for the above to return point at beginning
;; or end of continuation
(defun gnuplot-point-at-beginning-of-continuation (&optional pos)
  "Return value of point at beginning of the continued block containing point.

If there are no continuation lines, returns point-at-bol."
  (save-excursion
    (when pos (goto-char pos))
    (gnuplot-beginning-of-continuation)
    (point)))

(defun gnuplot-point-at-end-of-continuation (&optional pos)
  "Return value of point at the end of the continued block containing point.

If there are no continuation lines, returns point-at-eol."
  (save-excursion
    (when pos (goto-char pos))
    (gnuplot-end-of-continuation)
    (point)))

;; We also treat a block of continuation lines as a 'defun' for
;; movement purposes
(defun gnuplot-beginning-of-defun (&optional arg)
  (if (not arg) (setq arg 1))
  (if (> arg 0) 			
      (catch 'bob		; go to beginning of ARGth prev. defun
	(dotimes (n arg)
	  (when (= (point)
		   (gnuplot-point-at-beginning-of-continuation))
	    (forward-line -1)
	    (if (bobp) (throw 'bob t))
	    (while (looking-at "^\\s-*$")
	      (forward-line -1)
	      (if (bobp) (throw 'bob t))))
	  (gnuplot-beginning-of-continuation))
	t)

    (catch 'eob		  ; find beginning of (-ARG)th following defun
      (dotimes (n (- arg))
	(gnuplot-end-of-continuation)
	(forward-line)
	(if (eobp) (throw 'eob t))
	(while (looking-at "^\\s-*$")
	  (forward-line)
	  (if (eobp) (throw 'eob t)))))))

;; Movement to start or end of command, including multiple commands
;; separated by semicolons
(defun gnuplot-beginning-of-command ()
  "Move point to beginning of command containing point."
  (let ((limit (gnuplot-point-at-beginning-of-continuation)))
    (while
	(and
	 (search-backward ";" limit 'lim)
	 (gnuplot-in-string-or-comment)))
    (skip-chars-forward ";")
    (skip-syntax-forward "-")))

(defun gnuplot-end-of-command ()
  "Move point to end of command containing point."
  (let ((limit (gnuplot-point-at-end-of-continuation)))
    (while
	(and
	 (search-forward ";" limit 'lim)
	 (gnuplot-in-string-or-comment)))
    (skip-chars-backward ";")
    (skip-syntax-backward "-")))

(defun gnuplot-point-at-beginning-of-command ()
  "Return position at the beginning of command containing point."
  (save-excursion (gnuplot-beginning-of-command) (point)))

(defun gnuplot-point-at-end-of-command ()
  "Return position at the end of command containing point."
  (save-excursion (gnuplot-end-of-command) (point)))

(defun gnuplot-negate-option ()
  "Append \"no\" to or remove \"no\" from the set option on the current line.
This checks if the set option is one which has a negated form.

Negatable options are defined in `gnuplot-keywords-negatable-options'."
  (interactive)
  (gnuplot-fetch-version-number)
  (let ((begin (gnuplot-point-at-beginning-of-command))
	(end   (gnuplot-point-at-end-of-command))
	(regex gnuplot-negatable-options-regexp))
    (save-excursion
      (goto-char begin)
      (skip-syntax-forward "-" end)
      (if (looking-at "\\(un\\)?set\\s-+")
	  (cond ((> (string-to-number gnuplot-program-version) 3.7)
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
	(gnuplot-fetch-version-number)

	;; In the absence of evidence to the contrary, I'm guessing
	;; the info file layout changed with gnuplot version 4 <jjo>
	 (let ((doc-spec
	       (if (>= (string-to-number gnuplot-program-version) 4.0)
		   ;; New info-file layout - works with gnuplot 4.4
		   '(("(gnuplot)Command_Index"   nil "[_a-zA-Z0-9]+")
		     ("(gnuplot)Options_Index"   nil "[_a-zA-Z0-9]+")
		     ("(gnuplot)Function_Index"  nil "[_a-zA-Z0-9]+")
		     ("(gnuplot)Terminal_Index"  nil "[_a-zA-Z0-9]+"))

		 ;; Old info-file layout
		 '(("(gnuplot)Top"           nil "[_a-zA-Z0-9]+")
		   ("(gnuplot)Commands"      nil "[_a-zA-Z0-9]+")
		   ("(gnuplot)Functions"     nil "[_a-zA-Z0-9]+")
		   ("(gnuplot)plot"          nil "[_a-zA-Z0-9]+")
		   ("(gnuplot)set-show"      nil "[_a-zA-Z0-9]+")
		   ("(gnuplot)data-file"     nil "[_a-zA-Z0-9]+")
		   ("(gnuplot)smooth"        nil "[_a-zA-Z0-9]+")
		   ("(gnuplot)style"         nil "[_a-zA-Z0-9]+")
		   ("(gnuplot)terminal"      nil "[_a-zA-Z0-9]+")))))
	  (cond ((boundp 'info-lookup-symbol-alist) ; older info-lookup version
		 (setq info-lookup-symbol-alist
		       (append
			info-lookup-symbol-alist
			`((gnuplot-mode
			   "[a-zA-Z][_a-zA-Z0-9]*" nil
			   ,doc-spec "[_a-zA-Z0-9]+" )))))
		(t			; newer version
		 (info-lookup-add-help
		  :mode 'gnuplot-mode :topic 'symbol
		  :regexp "[a-zA-Z][_a-zA-Z0-9]*"
		  :doc-spec doc-spec)
		 ;; allow help lookup from the comint buffer as well <jjo>
		 (info-lookup-add-help
		  :mode 'gnuplot-comint-mode :topic 'symbol
		  :regexp "[a-zA-Z][_a-zA-Z0-9]*"
		  :doc-spec doc-spec))))

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
	(setq gnuplot-keywords-alist	; needed for all-completions
	      (mapcar 'list gnuplot-keywords)))

    ;; or do something sensible if info-look is not installed
    (defun info-lookup-interactive-arguments (symbol)
      (message
       "Help is not available.  info-look.el is not installed.")
      (list nil nil))))


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


;;;; Completion at point and Eldoc.

;; There are two alternative completion-at-point mechanisms: the old
;; one using info-look and the new one (enabled by default) which
;; parses the command line to provide smarter completions.

;; `gnuplot-completion-at-point-function' defines which one is
;; used. `gnuplot-context-sensitive-mode' toggles between the two.

(defvar gnuplot-completion-at-point-function #'gnuplot-completion-at-point-info-look
  "Function to call to perform completion in Gnuplot buffers.")

(defun gnuplot-completion-at-point ()
  (funcall gnuplot-completion-at-point-function))

(defvar gnuplot-eldoc-hash nil
  "ElDoc strings for gnuplot-mode.

These have to be compiled from the Gnuplot source tree using
`doc2texi.el'.")

;; Enable and disable context-sensitive completion
(define-minor-mode gnuplot-context-sensitive-mode
  "Use context-sensitive completion and help in gnuplot-mode.

When context-sensitive mode is enabled, gnuplot-mode tries to
provide more useful completions and help suggestions for built-in
keywords and functions by parsing each command as you type.  It
attempts to take into account Gnuplot's many abbreviated
keywords.  For example, with point at the end of a line reading
\"plot 'datafile' w \", typing \\[completion-at-point] will pop
up a list of plotting styles.

Key bindings:

\\[completion-at-point] will complete the keyword at point based
on its context in the command. To make keyword completion work on
pressing TAB, set `tab-always-indent' to `complete', or customize
`gnuplot-tab-completion' to make this automatic in gnuplot-mode
buffers.

\\[gnuplot-info-at-point] will try to find the most relevant
Gnuplot info node for the construction at point, prompting for a
node name if nothing is found.

\\[gnuplot-help-function] will pop up a brief summary of the
syntax at point in the minibuffer. To have one-line syntax
summaries appear in the echo area as you type, toggle
`eldoc-mode' or customize `gnuplot-eldoc-mode'.

To choose whether to use this mode by default in Gnuplot buffers,
customize the variable
`gnuplot-use-context-sensitive-completion'.

Note: help strings for eldoc-mode and \\[gnuplot-help-function]
need to be provided in an Emacs-readable form by the Gnuplot
distribution. See gnuplot-context.el for details."
  :keymap
  `((,(kbd "C-c C-/") . gnuplot-help-function)
    (,(kbd "C-c C-d") . gnuplot-info-at-point))
  (unless (derived-mode-p 'gnuplot-mode 'gnuplot-comint-mode)
    (message "Gnuplot context-sensitive mode works only in Gnuplot-mode buffers")
    (setq gnuplot-context-sensitive-mode nil))
  (if gnuplot-context-sensitive-mode
      ;; Turn on
      (progn
        (load-library "gnuplot-context")
        (load-library "eldoc")
        (setq gnuplot-completion-at-point-function #'gnuplot-context-completion-at-point)

        ;; Setup Eldoc
        (set (make-local-variable 'eldoc-documentation-function)
             'gnuplot-eldoc-function)
        (eldoc-add-command 'completion-at-point)     ; Check for eldoc after completion
        (when (fboundp 'comint-dynamic-complete)
          (eldoc-add-command 'comint-dynamic-complete))

        ;; Try to load Eldoc strings
        (when gnuplot-eldoc-mode
          (unless gnuplot-eldoc-hash
            (condition-case nil
                (load-library "gnuplot-eldoc")
              (error
               (message "gnuplot-eldoc.el not found. Install it from the Gnuplot distribution.")
               (setq gnuplot-eldoc-hash nil
                     gnuplot-eldoc-mode nil))))

          (if gnuplot-eldoc-hash
              (eldoc-mode 1)
            (eldoc-mode 0)))

        ;; Set up tab-to-complete
        (when gnuplot-tab-completion
          (set (make-local-variable 'tab-always-indent) 'complete))

	(message "Gnuplot context-sensitive help & completion enabled."))

    ;; Turn off
    (setq gnuplot-completion-at-point-function #'gnuplot-completion-at-point-info-look)
    (setq eldoc-documentation-function nil)
    (eldoc-mode 0)
    (message "Gnuplot context-sensitive help & completion disabled.")))

;; Older completion method using info-look
(defun gnuplot-completion-at-point-info-look ()
  "Return completions of keyword preceding point.

Uses the cache of keywords generated by info-lookup. See
`gnuplot-setup-info-look'. If not nil, the return value is in the form
\(BEGIN END COMPLETIONS) where BEGIN and END are buffer 
positions and COMPLETIONS is a list."
 
  (if gnuplot-keywords-pending		; <HW>
      (gnuplot-setup-info-look))
  (let* ((end (point))
	 (beg (unwind-protect (save-excursion (backward-sexp 1) (point))))
	 (patt (buffer-substring beg end))
	 (pattern (if (string-match "\\([^ \t]*\\)\\s-+$" patt)
		      (match-string 1 patt) patt))
	 (completions (all-completions pattern gnuplot-keywords-alist)))
    (if completions
	(list beg end completions)
      (message "No gnuplot keywords complete '%s'" pattern)
      nil))) 

(defun gnuplot-comint-complete ()
  "Complete the keyword preceding point in the gnuplot comint buffer.

This is only used in Emacs versions before 24.1."
  (let ((completions (gnuplot-completion-at-point)))
    (if completions
	(let* ((beg (nth 0 completions))
	       (end (nth 1 completions))
	       (candidates (nth 2 completions))
	       (completion-base-position (list beg end)))
	  (comint-dynamic-simple-complete
	   (buffer-substring-no-properties beg end)
	   candidates))
      nil)))


(defun gnuplot-info-lookup-symbol (symbol &optional mode)
  "Wrapper for `info-lookup-symbol'.
Takes SYMBOL and MODE as arguments exactly as
`info-lookup-symbol'.  After doing the info lookup, calls
`gnuplot--adjust-info-display' to display the info buffer
according to the value of `gnuplot-info-display'."
  (interactive
   (cond (gnuplot-keywords
	  (info-lookup-interactive-arguments 'symbol))
	 (gnuplot-keywords-pending	; <HW>
	  (gnuplot-setup-info-look)
	  (info-lookup-interactive-arguments 'symbol))
	 (t
	  (list nil (message
       "Help is not available.  The gnuplot info file could not be found.")))))

  (when (and (featurep 'info-look) gnuplot-keywords)
    (unless symbol (setq symbol "Commands"))
    (save-window-excursion
      (info-lookup-symbol symbol mode))
    (gnuplot--adjust-info-display)))

(defun gnuplot--adjust-info-display ()
  "Displays the *info* buffer in a window or frame as specified
by the value of `gnuplot-info-display'.  If
`gnuplot-info-display' is 'window, then the window will be shrunk
to the size of the info entry if it is smaller than half the
height of the frame.

The *info* buffer should already exist when this function is
called."
  (case gnuplot-info-display
    (window
     (switch-to-buffer-other-window "*info*")
     ;; Adjust window height only if the frame is split 
     ;; horizontally, so as not to mess up the minibuffer <jjo>
     ;; we can't use shrink-window-if-larger-than-buffer here
     ;; because it doesn't work with Info mode's narrowing
     (with-selected-window (get-buffer-window "*info*")
       (unless (gnuplot-window-full-height-p)
         (enlarge-window
          (min (- (count-lines (point-min) (point-max)) (window-height) -1)
               (- (/ (frame-height) 2) (window-height)))))))

    (frame
     (unless (and gnuplot-info-frame
                  (frame-live-p gnuplot-info-frame))
       (setq gnuplot-info-frame (make-frame)))
     (select-frame gnuplot-info-frame)
     (raise-frame gnuplot-info-frame)
     (if gnuplot-xemacs-p (setq toolbar-info-frame gnuplot-info-frame))
     (switch-to-buffer "*info*"))

    (t
     (switch-to-buffer "*info*"))))

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
             gnuplot-inline-image-mode
             gnuplot-tab-completion
             gnuplot-eldoc-mode
             gnuplot-context-sensitive-mode
             gnuplot-basic-offset
             gnuplot-buffer-max-size
             gnuplot-comint-mode-hook
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
This was written with version 4.6 of gnuplot in mind, but should
work with newer and older versions.

Report bugs in `gnuplot-mode' using \\[gnuplot-bug-report].

			    ------O------

Gnuplot-mode includes two different systems for keyword
completion and documentation lookup: a newer one,
`gnuplot-context-sensitive-mode' (enabled by default), and a
older one which extracts keywords from gnuplot's Info file.  Both
systems allow looking up documentation in the Info file.  The
older system also depends having the info file properly installed
to make a list of keywords.

The info file should be installed by default with the Gnuplot
distribution, or is available at the `gnuplot-mode' web page:
http://github.com/bruceravel/gnuplot-mode/

With the new context-sensitive mode active, gnuplot-mode can also
provide `eldoc-mode' syntax hints as you type.  This requires a
separate file of strings, `gnuplot-eldoc.el', which is also
provided by recent Gnuplot distributions.

			    ------O------

There are several known shortcomings of `gnuplot-mode', version 0.5g
and up.  Many of the shortcomings involve the graphical interface
\(refered to as the GUI) to setting arguments to plot options.  Here is
a list:

 1.  Currently there is no way for `gnuplot-mode' to know if information
     sent to gnuplot was correctly plotted.
 2.  \"plot\", \"splot\", and \"fit\" are handled in the GUI, but are
     a bit flaky.  Their arguments may not be read correctly from
     existing text, and continuation lines (common for plot and splot)
     are not supported.
 3.  The GUI does not know how to read from continuation lines.
 4.  Comma separated position arguments to plot options are
     unsupported in the GUI.  Colon separated datafile modifiers (used
     for plot, splot, and fit) are not supported either.  Arguments
     not yet supported by the GUI generate messages printed in grey
     text.
 5.  The GUI handling of \"hidden3d\" is flaky and \"cntrparam\" is
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

  (set (make-local-variable 'beginning-of-defun-function) 'gnuplot-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'gnuplot-end-of-continuation)

  (unless (featurep 'xemacs)
    (add-hook 'completion-at-point-functions 'gnuplot-completion-at-point nil t))

  (set-syntax-table gnuplot-mode-syntax-table)

  (when (eq gnuplot-keywords-when 'immediately) ; <HW>
    (gnuplot-setup-info-look)) ;; <SE>

  (if gnuplot-xemacs-p			; deal with font-lock
      (when (fboundp 'turn-on-font-lock)
	(turn-on-font-lock))
    (progn
      ;; Add syntax-propertizing functions to search for strings and comments
      (set (make-local-variable 'syntax-propertize-function)
           #'gnuplot-syntax-propertize)
      (add-hook 'syntax-propertize-extend-region-functions
                #'gnuplot-syntax-propertize-extend-region nil t)

      ;; Set up font-lock 
      (setq font-lock-defaults gnuplot-font-lock-defaults)
      (set (make-local-variable 'font-lock-multiline) t)
      (set (make-local-variable 'parse-sexp-lookup-properties) t)))

  (if (fboundp 'widget-create)		; gnuplot-gui
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

;;;###autoload
(defun run-gnuplot ()
  "Run an inferior Gnuplot process."
  (interactive)
  (gnuplot-make-gnuplot-buffer)
  (pop-to-buffer gnuplot-buffer))

(defun gnuplot-show-version ()
  "Show version number in echo area"
  (interactive)
  (message "gnuplot-mode %s -- URL: %s" gnuplot-version gnuplot-maintainer-url))

(defun gnuplot-show-gnuplot-version ()
  "Show gnuplot program and version number in echo area"
  (interactive)
  (gnuplot-fetch-version-number)
  (message "You are calling gnuplot %s as %s" gnuplot-program-version gnuplot-program))


;;; That's it! ----------------------------------------------------------------


;;; --- final chores: provide 'gnuplot and run load-hook
;; provide before run-hooks suggested by <DB>
(provide 'gnuplot)
(run-hooks 'gnuplot-load-hook)

;;;============================================================================
;;;

;;; gnuplot.el ends here
