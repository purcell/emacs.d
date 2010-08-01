;;; quack.el --- enhanced support for editing and running Scheme code

(defconst quack-copyright    "Copyright (C) 2002-2009 Neil Van Dyke")
(defconst quack-copyright-2  "Portions Copyright (C) Free Software Foundation")
;; Emacs-style font-lock specs adapted from GNU Emacs 21.2 scheme.el.
;; Scheme Mode menu adapted from GNU Emacs 21.2 cmuscheme.el.

(defconst quack-version      "0.37")
(defconst quack-author-name  "Neil Van Dyke")
(defconst quack-author-email "neil@neilvandyke.org")
(defconst quack-web-page     "http://www.neilvandyke.org/quack/")

(defconst quack-legal-notice
  "This is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.  This is
distributed in the hope that it will be useful, but without any warranty;
without even the implied warranty of merchantability or fitness for a
particular purpose.  See the GNU General Public License for more details.  See
http://www.gnu.org/licenses/ for details.  For other licenses and consulting,
please contact Neil Van Dyke.")

(defconst quack-cvsid "$Id: quack.el,v 1.463 2009/06/29 12:54:35 neilpair Exp $")

;;; Commentary:

;; INTRODUCTION:
;;
;;     Quack enhances Emacs support for Scheme programming.
;;
;;     Install Quack rather than following non-Quack-based tutorials on how to
;;     set up Emacs for Scheme.
;;
;;     The name "Quack" was a play on "DrScheme".
;;
;;     Quack is dedicated to Yosh, naturally.

;; COMPATIBILITY:
;;
;;     GNU Emacs 22 -- Yes.  Quack is developed under GNU Emacs 22 on a
;;     GNU/Linux system, which is the preferred platform for Quacksmokers.
;;     Quack should work under GNU Emacs 22 on any Un*x-like OS.  Reportedly,
;;     Quack also works with GNU Emacs 22 on Apple Mac OS X and Microsoft
;;     Windows (NT, 2000, XP), but the author has no means of testing on those
;;     platforms.
;;
;;     GNU Emacs 21 -- Probably, but no longer tested.
;;
;;     GNU Emacs 20 -- Probably mostly.  When last tested. Some of the menus do
;;     not work properly, due to a bug in easymenu.el (which the FSF will not
;;     fix, since they no longer support Emacs 20).  Nested block comments are
;;     not fontified correctly.  Pretty-lambda does not work.  Quack runs less
;;     efficiently in 20 than 21, due to the lack of standard hash tables.
;;
;;     XEmacs 21 -- Probably mostly, but no longer tested.  Block comment
;;     fontification is not yet supported under XEmacs 21, due to what appears
;;     to be a bug in 21.4 font-lock.  Pretty-lambda does not work.  XEmacs
;;     Quacksmokers who always want the latest and greatest Quack should
;;     consider GNU Emacs 21 -- Quack treats XEmacs like a high-maintenance
;;     redheaded stepchild.

;; INSTALLATION:
;;
;;     To install, put this file (`quack.el') somewhere in your Emacs load
;;     path, and add the following line to your `.emacs' file:
;;
;;         (require 'quack)
;;
;;     If you don't know what your Emacs load path is, try invoking the command
;;     "C-h v load-path RET" or consulting the Emacs manual.
;;
;;     Note to advanced Emacsers: Byte-compiled `quack.elc' files generally are
;;     *not* portable between Emacs implementations, nor between different
;;     versions of the same implementation.
;;
;;     You will also need the GNU `wget' program, which Quack uses for
;;     downloading SRFI indexes.  This popular program is included in most
;;     GNU/Linux distributions and is available for most other platforms.
;;
;;     Note to PLT Scheme users: If you do not already have the PLT manuals
;;     installed, they can be downloaded from 
;;     `http://download.plt-scheme.org/doc/' and installed in your PLT `doc'
;;     collection.  If Quack is not finding installed PLT manuals, then be sure
;;     that the `quack-pltcollect-dirs' variable contains the appropriate
;;     collection directory (if it does not, then either set the `PLTHOME'
;;     and/or `PLTCOLLECTS' environment variables appropriately, or set
;;     `quack-pltcollect-dirs').

;; KEY BINDINGS:
;;
;;     The key bindings that Quack adds to `scheme-mode' include:
;;
;;         C-c C-q m   View a manual in your Web browser.
;;         C-c C-q k   View the manual documentation for a keyword
;;                     (currently only works for PLT manuals).
;;         C-c C-q s   View an SRFI.
;;         C-c C-q r   Run an inferior Scheme process.
;;         C-c C-q f   Find a file using context of point for default.
;;         C-c C-q l   Toggle `lambda' syntax of `define'-like form.
;;         C-c C-q t   Tidy the formatting of the buffer.
;;
;;     One additional command that does not currently have a standard binding
;;     is `quack-dired-pltcollect', which prompts for a PLT collection name and
;;     creates a Dired buffer on the collection's directory.  (A future version
;;     of Quack may integrate this functionality into a more generalized
;;     documentation navigation interface.)

;; RELEASE ANNOUNCEMENTS EMAIL:
;;
;;     To receive email notification when a new Quack version is released, ask
;;     neil@neilvandyke.org to add you to the moderated `scheme-announce' list.

;; HISTORY:
;;
;;     Version 0.37 (2009-06-29)
;;         * Disabled highlighting of "Compilation started at" lines.
;;
;;     Version 0.36 (2009-05-27)
;;         * Made `#:' ``colon keywords'' fontify in PLT-ish mode.
;;         * Added PLT `r6rs' and `typed-scheme' languages to `quack-programs'.
;;
;;     Version 0.35 (2009-02-24)
;;         * Added `interpreter-mode-alist' support, so Scheme scripts with "#!"
;;           start in `scheme-mode'.
;;         * Added PLT `parameterize-break'.
;;         * Improved `compile' mode for PLT 4.x tracebacks when there is only
;;           file, line, and column, but no additional information.
;;
;;     Version 0.34 (2009-02-19)
;;         * Added fontify and indent support for PLT `define/kw', `lambda/kw',
;;          `parameterize*'.
;;         * Fontify Unix "#!" cookie in PLT-ish font-lock.
;;         * Changed reference to `quack-announce' email list to
;;           `scheme-announce'.
;;         * Added PLT `default-load-handler' to
;;          `quack-compilation-error-regexp-alist-additions'
;;         * Changed some face ":height" attributes.
;;
;;     Version 0.33 (2008-07-31)
;;         * Added handlers for some PLT 4.0.1 "setup-plt" messages.
;;
;;     Version 0.32 (2008-06-19)
;;         * Added to `quack-programs'.
;;         * Updated compatibility comments.
;;         * Added indent rule for `for/fold'.
;;
;;     Version 0.31 (2008-05-03)
;;         * Added `defvar' for `quack-pltish-font-lock-keywords', so that the
;;           GNU Emacs 22.1 compiler doesn't complain about assignment to a free
;;           variable.
;;         * Changed banner regexp for MzScheme for v3.99.x.
;;         * Set `dynamic-wind' `scheme-indent-function to 0, when the default
;;           is 3.  It was just taking up too much space.  DrScheme's
;;           indentation seems to be equivalent -1, so there is precedent for
;;           something different.  We generally respect Emacs indentation
;;           convention.
;;         * Added fontifying and indent for PLT `define-for-syntax',
;;           `define-values-for-syntax', `quasisyntax', `quasisyntax/loc',
;;           `syntax', `syntax/loc', `define-parameters'.
;;         * Advise `scheme-interactively-start-process' for GNU Emacs 22.
;;         * Removed TODO comment that mentioned using `(current-eventspace
;;           (make-eventspace))' under `mred', as Robby Findler has indicated
;;           that is not good advice.
;;
;;     Version 0.30 (2007-06-27)
;;         * Emacs 22 compatibility change: `string-to-number' instead of
;;           `string-to-int'.  Thanks to Charles Comstock.
;;
;;     Version 0.29 (2006-11-12)
;;         * Fixed `quack-bar-syntax-string', which caused vertical bar
;;           characters to be treated as whitespace.  Thanks to Eric Hanchrow
;;           for reporting.
;;
;;     Version 0.28 (2005-05-14)
;;         * Added `quack-smart-open-paren-p'.
;;         * Changed `scheme-indent-function' for `parameterize' from `defun'
;;           to `1'.
;;         * In `quack-pltish-keywords-to-fontify': added `quasiquote',
;;           `unquote', and `unquote-splicing'.
;;         * Added ".mzschemerc" to `auto-mode-alist'.
;;         * Added a little extra threesemi fontification for Funcelit and
;;           similar Texinfo markup formats.
;;
;;     Version 0.27 (2004-12-19)
;;         * For Gambit-C, added REPL banner fontifying, `quack-manuals' entry,
;;           and "gsi ~~/syntax-case.scm -" `quack-programs' entry.
;;         * Changed "[PLT]" prefix on PLT manuals to "PLT", to make it easier
;;           to type.
;;         * Minor changes to reflect "MIT Scheme" becoming "MIT/GNU Scheme".
;;
;;     Version 0.26 (2004-07-14)
;;         * Added fontifying of a bunch of "define-"* syntax from Chicken.
;;
;;     Version 0.25 (2004-07-09)
;;         * Added `define-record-type' to `quack-pltish-keywords-to-fontify'.
;;         * Added "csi -hygienic" to `quack-programs'.
;;         * In `quack-manuals', replaced PLT-specific `r5rs' and `t-y-scheme'
;;           with generic ones.
;;         * Updated URL in `quack-manuals' for 3rd ed. of `tspl'.
;;         * `quack-view-manual' completions no longer include symbols.
;;         * `quack-view-manual' completion default is now "R5RS".
;;
;;     Version 0.24 (2004-05-09)
;;         * Made `quack-pltish-keywords-to-fontify' and
;;           `quack-emacs-keywords-to-fontify' custom changes update
;;           immediately.  Bug reported by Taylor Campbell.
;;         * Removed some non-syntax names from  
;;           `quack-pltish-keywords-to-fontify'.
;;         * Documentation changes.
;;
;;     Version 0.23 (2003-11-11)
;;         * `quack-local-keywords-for-remote-manuals-p' can now have the value
;;           of the symbol `always', to work around a defect in some versions
;;           of Microsoft Windows.  Thanks to Bill Clementson.
;;         * `quack-w3m-browse-url-other-window' no longer splits a `*w3m*'
;;           buffer.
;;         * Added indent and `quack-pltish-keywords-to-fontify' rules for
;;           `c-lambda' and `c-declare'.
;;
;;     Version 0.22 (2003-07-03)
;;         * `quack-newline-behavior' controls the RET key behavior in Scheme
;;           buffers.
;;         * In `quack-manuals', added Chez Scheme, and updated Chicken.
;;         * Added error message navigation to `compile' for PLT `setup-plt'.
;;         * Partial fix for Quack global menu disappearing from the main menu
;;           bar in XEmacs.  Thought it used to work, but it doesn't in XEmacs
;;           21.4.12.
;;
;;     Version 0.21 (2003-05-28)
;;         * `quack-find-file' is faster in many cases due to fix to
;;           `quack-backward-sexp'.
;;         * Added auto-mode-alist for `.ccl', `.stk', and `.stklos' files.
;;         * Indent rule additions/changes for `chicken-setup' and `unit/sig'.
;;
;;     Version 0.20 (2003-05-04)
;;         * Added indent and fontify for SRFI-8 "receive".
;;         * Added indent and fontify for additional PLT syntax.
;;         * Added `quack-fontify-threesemi-p'.
;;         * `quack-tidy-buffer' sets `fill-prefix' to nil when running.
;;         * Added messages to `run-scheme', if only to get rid of annoying 
;;           "Mark set" message.
;;         * Added "mzscheme -M errortrace" to `quack-programs'.
;;         * `quack-dired-pltcollect' prompt defaults to `mzlib'.
;;         * "Update SRFI Index" menu item has moved to top of menu, mainly to
;;           avoid usability issue in a particular Emacs menu implementation.
;;         * Several code quality improvements sent by Stefan Monnier will be
;;           in the next release.
;;
;;     Version 0.19 (2003-03-04)
;;         * Commands such as `scheme-load-file' now start a Scheme process if
;;           none is found.
;;         * Bugfix for using `match-string-no-properties' when we meant
;;           `quack-match-string-no-properties'.  (Thanks to Noel Welsh.)
;;
;;     Version 0.18 (2003-05-02)
;;         * Removed uses of `(regexp-opt LIST t)', since XEmacs21 does not
;;           create match data.  (Thanks to Garrett Mitchener for debugging.)
;;         * Added to `quack-programs' and `quack-manuals'.
;;         * Added pretty-case-lambda.
;;         * Changed PLT documentation URL function.
;;
;;     Version 0.17 (2003-01-03)
;;         * Pretty-lambda is supported well under GNU Emacs 21, when using PLT
;;           Style fontification.  Enable via the Options menu.  (Based on
;;           approach by Stefan Monnier; suggested by Ray Racine.)
;;         * Various faces now have separate defaults for `light' and `dark'
;;           backgrounds, so may now look better on dark backgrounds.
;;           (Suggested by Eli Barzilay.)
;;         * `quack-find-file' now respects `insert-default-directory' when
;;           there is no default file.  (Thanks to Eli Barzilay.)
;;         * Most of the special w3m support has been moved to a separate
;;           package, `w3mnav' (`http://www.neilvandyke.org/w3mnav/').
;;           `quack-w3m-browse-url-other-window' has been added.
;;
;;     Version 0.16 (2002-12-16)
;;         * `quack-insert-closing' now calls `blink-paren-function'.  (Thanks
;;           to Guillaume Marceau and Steve Elkins for reporting this.)
;;         * Now uses PLT 202 manuals.  Added "PLT Framework" manual.
;;         * Added `quack-pltish-module-defn-face'.
;;         * Added some PLTish font-lock keywords.
;;
;;     Version 0.15 (2002-11-21)
;;         * "Keywords" are now fontified in PLT Style fontification mode.
;;         * Definition names are now blue by default in PLT Style.
;;         * Symbol literals with vertical bars are now fontified in PLT Style.
;;         * New `quack-manuals-webjump-sites' function for people who prefer
;;           to use the `webjump' package for invoking manuals.
;;         * New `quack-quiet-warnings-p' option.
;;         * New `quack-pltish-class-defn-face' face.
;;
;;     Version 0.14 (2002-10-18)
;;         * Fix for `quack-view-manual' interactive prompting (thanks to Marko
;;           Slyz for reporting this).
;;         * `quack-emacsw3m-go-next' and `quack-emacsw3m-go-prev' now work
;;           with GTK reference documentation (not that this has anything to do
;;           with Scheme).
;;         * Added SLIB to `quack-manuals'.
;;         * Added comment about installing PLT manuals (thanks to Marko).
;;         * We now call the canonical version of Emacs "GNU Emacs," instead of
;;           "FSF Emacs".
;;
;;     Version 0.13 (2002-09-21)
;;         * Bugfix: No longer drop SRFI index entries on the floor.
;;
;;     Version 0.12 (2002-09-20)
;;         * New "View SRFI" menu.  Select "Update SRFI Index" if the submenus
;;           "Draft", "Final", and "Withdrawn" are disabled.
;;         * Most options are now settable via "Options" menu.
;;         * PLT collections are no longer scanned when building "View Manuals"
;;           menu.
;;         * "View Keyword Docs..." back on Scheme Mode menu in addition to
;;           Quack menu.
;;         * Various `defcustom' variables have been made to dynamically update
;;           relevant program state when changed.
;;         * Under GNU Emacs 20, dynamic menus still do not work -- they now
;;           display, but do not perform the selected action.  Will do more
;;           debugging after this release.
;;         * '[' and ']' keys work in emacs-w3m of MIT Scheme manuals.
;;
;;     Version 0.11 (2002-09-17)
;;         * Menus now work under XEmacs.  Also now partly broken for Emacs 20.
;;         * New global "Quack" menu.  Disable with `quack-global-menu-p'.
;;         * New "View Manual" submenu under GNU Emacs 21 and XEmacs (GNU Emacs
;;           20 is stuck with the old "View Manual..." menu item).
;;         * Fix for `quack-pltcollects-alist' to include PLT `doc' collection,
;;           which was preventing local manuals from being used.
;;         * `quack-manuals' now includes `t-y-scheme'.
;;         * `quack-view-in-different-browser' command that spawns alternative
;;           Web browser from the special emacs-w3m support, bound to `B'.  For
;;           when you normally view manuals in an Emacs window, but
;;           occasionally want to view a particular page in normal Web browser.
;;         * More `scheme-indent-function' properties set.
;;         * `quack-about' command.
;;         * Fix to `quack-keyword-at-point'.
;;
;;     Version 0.10 (2002-09-11)
;;         * `quack-view-srfi' now prompts with completion, including titles
;;           for all SRFIs.  The SRFI titles are fetched from the official SRFI
;;           Web site using the GNU Wget program, and cached locally.
;;         * `quack-view-srfi' also now defaults to the SRFI number at or near
;;           the point.
;;         * `quack-dir' variable specifies a directory where Quack should
;;           store its persistent data files (e.g., cached SRFI indexes), and
;;           defaults to "~/.quack/".
;;         * New `quack-tidy-buffer' command.  [C-c C-q t] is now bound to
;;           this; [C-c C-q l] ("l" as in "lambda) is now the official binding
;;           for `quack-toggle-lambda'.
;;         * `quack-find-file' now recognizes PLT `dynamic-require' form.
;;         * Fix to make `quack-looking-at-backward' preserve match data.
;;         * Fix for benign bug in `quack-parent-sexp-search'.
;;
;;     Version 0.9 (2002-09-04)
;;         * Quack now works under XEmacs 21, except no menus are currently
;;           defined (that will come in a later version) and block comments
;;           aren't fontified.
;;         * `quack-toggle-lambda' command toggles a `define' form between
;;           explicit and implicit `lambda' syntax.
;;         * `quack-dired-pltcollect' feature prompts for a PLT collection name
;;           and creates a Dired on the collection.
;;         * `)' and `]' keys are bound to insert a closing character that
;;           agrees with the opening character of the sexp.
;;         * Nested `#|' comment blocks are now fontified mostly correctly
;;           under GNU Emacs 21.
;;         * Fix to `quack-parent-sexp-search'.
;;         * Fix for PLT manual keywords lookup under Emacs 20.
;;         * `quack-manuals' URLs for assorted implementation manuals now point
;;           to canonical Web copies.
;;         * No longer warns about PLT manual keywords file found without HTML.
;;         * `find-file' key bindings are automatically remapped to
;;           `quack-find-file' in Scheme buffers.
;;         * Both PLT-style and Emacs-style fontification now work with the
;;           `noweb-mode' package.  Tested under GNU Emacs 21 with
;;           Debian `nowebm' package version 2.10c-1.
;;         * Added to `quack-emacsish-keywords-to-fontify'.
;;         * Disabled fontification of named `let'.
;;         * Renamed "collect" in PLT identifiers to "pltcollect".
;;         * `auto-mode-alist' set more aggressively.
;;
;;     Version 0.8 (2002-08-25)
;;         * PLT package file viewing mode.  This is mainly used to easily
;;           inspect a ".plt" package before installing it via DrScheme or
;;           "setup-plt".
;;         * No longer warns about `font-lock-keywords' when `noweb-mode'
;;           package is installed.
;;
;;     Version 0.7 (2002-08-22)
;;         * Now works on GNU Emacs 20 (though people are still encouraged to
;;           upgrade to GNU Emacs 21 if they are able).
;;         * `quack-manuals' now includes MIT Scheme and Chicken manuals
;;           (currently where Debian GNU/Linux puts them).
;;         * `quack-view-srfi' command.
;;         * Named-`let' name is fontified like a PLTish definition name.
;;         * `define-record' and `define-opt' fontified.
;;         * Scheme Mode is forced in `auto-mode-alist' for ".sch" files.
;;         * Fix to `quack-backward-sexp'.
;;         * `quack-warning' messages get your attention.
;;         * `quack-pltrequire-at-point-data-1' search depth limited.
;;
;;     Version 0.6 (2002-08-20)
;;         * `quack-find-file' now supports multi-line PLT `require' forms.
;;         * When `emacs-w3m' is used, the keys "[", "]", and "t" are bound to
;;           navigate through PLT manuals like in Info mode.
;;         * Names highlighted in PLT-style fontification of `defmacro',
;;           `defmacro-public', `defsyntax'.
;;         * Advised `run-scheme' no longer prompts when there is already a
;;           running Scheme.
;;         * "csi" (Chicken interpreter) added to `quack-programs' default.
;;         * Forces `auto-mode-alist' for ".scm" files to `scheme-mode'
;;           (two can play at that game, `bee-mode'!).
;;         * To-do comments moved from the top of the file to throughout code.
;;
;;     Version 0.5 (2002-08-15)
;;         * New `quack-find-file' permits quick navigation to files indicated
;;           by a PLT Scheme `require' form under the point.  Currently only
;;           works when the "(require" string is on the same line as point.
;;         * Improved PLT-style fontification.  Most noticeable difference is
;;           that names in many definition forms are boldfaced.  See
;;           `quack-pltish-fontify-definition-names-p' option.
;;         * `quack-collects-alist' added.
;;         * "~/plt/" has been removed from `quack-collect-dirs' default.
;;         * Unnecessary syntax table settings have been removed.
;;         * Reduced memory usage in some cases, via explicit GC calls.
;;
;;     Version 0.4 (2002-08-07)
;;         * Functionality adapted from author's `giguile.el' package:
;;             - Enhanced `run-scheme' behavior.  `quack-run-mzscheme',
;;               `quack-run-mred', and `quack-remove-run-scheme-menu-item-p'
;;               are obsolete.
;;             - Enhanced `switch-to-scheme' behavior.
;;             - Options menu.
;;             - Indent rules for a few Guile-isms.
;;         * Inferior Scheme Mode now uses the preferred fontification method.
;;         * Now uses the PLT-bundled version of R5RS manual, which permits
;;           keyword searching.
;;         * `quack-banner-face' for the MzScheme/MrEd banner in REPL buffer.
;;         * This code includes a start on toolbars and XEmacs21 portability,
;;           but neither feature is yet functional.
;;
;;     Version 0.3 (2002-08-01)
;;         * PLT-style fontification added, except for quoted lists.  Emacs-
;;           style fontification still available; see `quack-fontify-style'.
;;         * `emacs-w3m' package support for lightweight viewing of PLT manuals
;;           in Emacs window.  If you install the `emacs-w3m' package, then you
;;           can change the new `quack-browse-url-browser-function' option to
;;           use it.
;;         * Quack menu items added to Scheme Mode menu.  "Run Scheme" item
;;           is removed by default; see `quack-remove-run-scheme-menu-item-p'.
;;         * MrEd REPL supported with `quack-run-mred'.
;;         * Better default for `quack-collect-dirs'.
;;         * More `scheme-indent-function' settings.
;;         * Bugfix for `quack-prompt-for-kwmatch-choice'.
;;         * Bugfix for font-lock keywords getting set too early.
;;         * Now byte-compiles without warnings/errors.
;;
;;     Version 0.2 (2002-07-28)
;;         * Manual keywords lookup.
;;         * Other minor changes.
;;
;;     Version 0.1 (2002-07-18)
;;         * Initial release.

;; ADMONISHMENT TO IMPRESSIONABLE YOUNG SCHEME STUDENTS:
;;
;;     Quack should by no means be construed as a model of good programming,
;;     much less of good software engineering.  Emacs is by nature a complex
;;     system of interacting kludges.  To get Emacs to do useful new things is
;;     to artfully weave one's extensions into a rich tapestry of sticky duct
;;     tape.  Also, Quack usually only got hacked on when I was stuck in a busy
;;     lobby for an hour with a laptop and unable to do real work.

;;; Code:

;; Dependencies:

(require 'advice)
(require 'cmuscheme)
(require 'compile)
(require 'custom)
(require 'easymenu)
(require 'font-lock)
(require 'scheme)
(require 'thingatpt)

(unless (fboundp 'customize-save-variable)
  (autoload 'customize-save-variable "cus-edit"))

;; Custom Variables:

(defgroup quack nil
  "Enhanced support for editing and running Scheme code."
  :group  'scheme
  :prefix "quack-"
  :link   '(url-link "http://www.neilvandyke.org/quack/"))

(defcustom quack-dir "~/.quack"
  "*Directory where Quack stores various persistent data in file format."
  :type  'string
  :group 'quack)

(defcustom quack-scheme-mode-keymap-prefix "\C-c\C-q"
  "*Keymap prefix string for `quack-scheme-mode-keymap'.

One of the nice things about having C-q in the prefix is that it is unlikely to
be already be in use, due to the historical reality of software flow control
\(and the fact that it is hard to type).  If your C-q doesn't seem to be going
through, then you have several options: disable flow control (if it is safe to
do so), change the value of this variable, or see the Emacs documentation for
`enable-flow-control-on'."
  :type  'string
  :group 'quack)

(defcustom quack-remap-find-file-bindings-p t
  "Whether to remap `find-file' key bindings to `quack-find-file'.
The local map in Scheme Mode and Inferior Scheme Mode buffers is used."
  :type  'boolean
  :group 'quack)

(defcustom quack-global-menu-p t
  "*Whether to have a \"Quack\" menu always on the menu bar."
  :type  'boolean  :group 'quack)

(defcustom quack-tabs-are-evil-p t
  "*Whether Quack should avoid use of Tab characters in indentation."
  :type  'boolean
  :group 'quack)

(defcustom quack-browse-url-browser-function nil
  "*Optional override for `browse-url-browser-function'.

If non-nil, overrides that variable for URLs viewed by `quack-browse-url'."
  :type '(choice (const    :tag "Do Not Override" nil)
                 (function :tag "Function")
                 (alist    :tag "Regexp/Function Association List"
                           :key-type regexp :value-type function))
  :group 'quack)

(defcustom quack-manuals                ; TODO: Options menu.

  ;; TODO: If we make this so users are likely to want to override parts of it,
  ;;       then introduce `quack-manuals-defaults' variable with this in it,
  ;;       and let users edit `quack-manuals-overrides' which are keyed on the
  ;;       ID symbol.

  ;; TODO: Have a way for finding docs on the local filesystem, and/or
  ;;       permitting a user to easily specify location.

  ;; TODO: Provide a way of specifying alternative access means so that, for
  ;;       example, we can look for R5RS first in locally-installed PLT
  ;;       collection, then in one of various non-PLT directories it might be
  ;;       mirrored, then remote PLT copy using local PLT keywords file, then
  ;;       the canonical HTML copy on the Web...  Maybe even permit Info
  ;;       format.  Let's just reinvent the Web, while we're at it.

  '(

    (r5rs "R5RS"
          "http://www.schemers.org/Documents/Standards/R5RS/HTML/"
          nil)

    (bigloo
     "Bigloo"
     "http://www-sop.inria.fr/mimosa/fp/Bigloo/doc/bigloo.html"
     ;;"file:///usr/share/doc/bigloo/manuals/bigloo.html"
     nil)

    (chez
     "Chez Scheme User's Guide"
     "http://www.scheme.com/csug/index.html"
     nil)

    (chicken
     "Chicken User's Manual"
     "http://www.call-with-current-continuation.org/manual/manual.html"
     ;;"file:///usr/share/doc/chicken/manual.html"
     nil)

    (gambit
     "Gambit-C home page"
     "http://www.iro.umontreal.ca/~gambit/")

    (gauche
     "Gauche Reference Manual"
     "http://www.shiro.dreamhost.com/scheme/gauche/man/gauche-refe.html"
     nil)

    (mitgnu-ref
     "MIT/GNU Scheme Reference"
     "http://www.gnu.org/software/mit-scheme/documentation/scheme.html"
     ;;"http://www.swiss.ai.mit.edu/projects/scheme/documentation/scheme.html"

     ;;"file:///usr/share/doc/mit-scheme/html/scheme.html"
     nil)

    (mitgnu-user
     "MIT/GNU Scheme User's Manual"
     "http://www.gnu.org/software/mit-scheme/documentation/user.html"
     ;;"http://www.swiss.ai.mit.edu/projects/scheme/documentation/user.html"
     ;;"file:///usr/share/doc/mit-scheme/html/user.html"
     nil)

    (mitgnu-sos
     "MIT/GNU Scheme SOS Reference Manual"
     "http://www.gnu.org/software/mit-scheme/documentation/sos.html"
     ;;"http://www.swiss.ai.mit.edu/projects/scheme/documentation/sos.html"
     ;;"file:///usr/share/doc/mit-scheme/html/sos.html"
     nil)

    (plt-mzscheme  "PLT MzScheme: Language Manual"                plt t)
    (plt-mzlib     "PLT MzLib: Libraries Manual"                  plt t)
    (plt-mred      "PLT MrEd: Graphical Toolbox Manual"           plt t)
    (plt-framework "PLT Framework: GUI Application Framework"     plt t)
    (plt-drscheme  "PLT DrScheme: Programming Environment Manual" plt nil)
    (plt-insidemz  "PLT Inside PLT MzScheme"                      plt nil)
    (plt-tools     "PLT Tools: DrScheme Extension Manual"         plt nil)
    (plt-mzc       "PLT mzc: MzScheme Compiler Manual"            plt t)
    (plt-r5rs      "PLT R5RS"                                     plt t)

    (scsh
     "Scsh Reference Manual"
     "http://www.scsh.net/docu/html/man-Z-H-1.html"
     ;;"file:///usr/share/doc/scsh-doc/scsh-manual/man-Z-H-1.html"
     nil)

    (sisc
     "SISC for Seasoned Schemers"
     "http://sisc.sourceforge.net/manual/html/"
     nil)

    (htdp       "How to Design Programs"
                "http://www.htdp.org/"
                nil)
    (htus       "How to Use Scheme"
                "http://www.htus.org/"
                nil)
    (t-y-scheme "Teach Yourself Scheme in Fixnum Days"
                "http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme.html"
                nil)
    (tspl       "Scheme Programming Language (Dybvig)"
                "http://www.scheme.com/tspl/"
                nil)
    (sicp       "Structure and Interpretation of Computer Programs"
                "http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html"
                nil)
    (slib       "SLIB"
                "http://swissnet.ai.mit.edu/~jaffer/SLIB.html"
                nil)
    (faq        "Scheme Frequently Asked Questions"
                "http://www.schemers.org/Documents/FAQ/"
                nil))
  "*List of specifications of manuals that can be viewed.

Each manual specification is a list of four elements:

    (SYMBOL TITLE LOCATION USE-KEYWORDS-P)

where SYMBOL is a short symbol that identifies the manual, TITLE is a string,
LOCATION is either a string with the URL of the manual or the symbol `plt',
and USE-KEYWORDS-P is `t' or `nil'.

If LOCATION is `plt', then Quack treats it as a PLT bundled manual, looking for
the HTML and keyword files in `quack-pltcollect-dirs', and optionally providing
keyword lookup if USE-KEYWORDS-P is `t'.  Remote canonical copies of the
manuals will be used if local copies cannot be found.

If LOCATION is a URL, then USE-KEYWORDS-P must be `nil'."
  :type  '(repeat (list (symbol :tag "Identifying Symbol")
                        (string :tag "Title String")
                        (choice :tag "Location"
                                (string :tag "URL")
                                (const  :tag "PLT Bundled Manual" plt))
                        (boolean :tag "Use Keywords?")))
  :group 'quack)

(defcustom quack-local-keywords-for-remote-manuals-p t
  "*If non-nil, Quack will use canonical remote Web URLs when there is a local
keyword file for a PLT manual but no local HTML files.  (This feature was
prompted by the Debian 200.2-3 package for MzScheme, which includes keyword
files but not HTML files.)  If the symbol `always', then Quack will always use
remote Web manuals for keywords lookup, even if local HTML files exist, as a
workaround for how some versions of Emacs interact with some versions of
Microsoft Windows \(inexplicably discarding the fragment identifier from `file'
scheme URI\)."
  :type       '(choice (const :tag "Permit"    t)
                       (const :tag "Forbid"     nil)
                       (const :tag "Always" always))
  :group      'quack
  :set        'quack-custom-set
  :initialize 'custom-initialize-default)

(defcustom quack-srfi-master-base-url "http://srfi.schemers.org/"
  ;; Note: Intentionally not letting user change this through the options menu.
  "*The base URL for the master SRFI Web pages.
The SRFI index files should be immediately beneath this."
  :type   'string
  :group  'quack)

(defcustom quack-pltcollect-dirs
  (let ((good '()))
    (mapcar (function (lambda (dir)
                        (and dir
                             (not (assoc dir good))
                             (file-directory-p dir)
                             (setq good (nconc good (list dir))))))
            `(,@(let ((v (getenv "PLTCOLLECTS")))
                  (and v (split-string v ":")))
                ,(let ((v (getenv "PLTHOME")))
                   (and v (expand-file-name "collects" v)))
                ,@(mapcar 'expand-file-name
                          '("/usr/lib/plt/collects"
                            "/usr/local/lib/plt/collects"))))
    good)
  "*PLT collection directories.
Listed in order of priority."
  :type       '(repeat directory)
  :group      'quack
  :set        'quack-custom-set
  :initialize 'custom-initialize-default)

(defcustom quack-fontify-style 'plt
  "*Which font-lock fontification style to use.

If symbol `plt', an approximation of PLT DrScheme 200 Check Syntax
fontification will be used.  If symbol `emacs', then fontification in the style
of GNU Emacs' Scheme Mode with extensions will be used.  If nil, then Quack
will not override the default Scheme Mode fontification."
  :type       '(choice (const :tag "PLT Style"                plt)
                       (const :tag "Extended GNU Emacs Style" emacs)
                       (const :tag "Emacs Default"            nil))
  :group      'quack
  :set        'quack-custom-set
  :initialize 'custom-initialize-default)

(defcustom quack-pltish-fontify-definition-names-p t
  "*If non-nil, fontify names in definition forms for PLT-style fontification.

This only has effect when `quack-fontify-style' is `plt'."
  :type       'boolean
  :group      'quack
  :set        'quack-custom-set
  :initialize 'custom-initialize-default)

(defcustom quack-pltish-fontify-keywords-p t
  ;; TODO: !!! Rename this from "keywords" to "syntax-keywords", here, and in
  ;; for face names.
  "*If non-nil, fontify keywords in PLT-style fontification.

This only has effect when `quack-fontify-style' is `plt'."
  :type       'boolean
  :group      'quack
  :set        'quack-custom-set
  :initialize 'custom-initialize-default)

(defcustom quack-pltish-keywords-to-fontify
  ;; TODO: These are currently R5RS and some SRFI special syntax plus a bunch
  ;; of PLT, especially PLT 200 class.ss, and some "define-"* variants from
  ;; various dialects.  The dumbness of this kind of highlighting without
  ;; regard to context is not really satisfactory.
  '(

    "and" "begin" "begin0" "c-declare" "c-lambda" "case" "case-lambda" "class"
    "class*" "class*/names" "class100" "class100*" "compound-unit/sig" "cond"
    "cond-expand" "define" "define-class" "define-const-structure"
    "define-constant" "define-embedded" "define-entry-point" "define-external"
    "define-for-syntax" "define-foreign-record" "define-foreign-type"
    "define-foreign-variable" "define-generic" "define-generic-procedure"
    "define-inline" "define-location" "define-macro" "define-method"
    "define-module" "define-opt" "define-public" "define-reader-ctor"
    "define-record" "define-record-printer" "define-record-type"
    "define-signature" "define-struct" "define-structure" "define-syntax"
    "define-syntax-set" "define-values" "define-values-for-syntax"
    "define-values/invoke-unit/sig" "define/contract" "define/override"
    "define/private" "define/public" "define/kw"
    "delay" "do" "else" "exit-handler" "field"
    "if" "import" "inherit" "inherit-field" "init" "init-field" "init-rest"
    "instantiate" "interface" "lambda" "lambda/kw" "let" "let*" "let*-values"
    "let+"
    "let-syntax" "let-values" "let/ec" "letrec" "letrec-values" "letrec-syntax"
    "match-lambda" "match-lambda*" "match-let" "match-let*" "match-letrec"
    "match-define" "mixin" "module" "opt-lambda" "or" "override" "override*"
    "namespace-variable-bind/invoke-unit/sig" "parameterize" "parameterize*"
    "parameterize-break" "private"
    "private*" "protect" "provide" "provide-signature-elements"
    "provide/contract" "public" "public*" "quasiquote" 
    "quasisyntax" "quasisyntax/loc" "quote" "receive"
    "rename" "require" "require-for-syntax" "send" "send*" "set!" "set!-values"
    "signature->symbols" "super-instantiate" "syntax" "syntax/loc"
    "syntax-case" "syntax-case*" "syntax-error" "syntax-rules" "unit/sig"
    "unless" "unquote" "unquote-splicing" "when" "with-handlers" "with-method"
    "with-syntax"
    "define-type-alias"
    "define-struct:"
    "define:"
    "let:"
    "letrec:"
    "let*:"
    "lambda:"
    "plambda:"
    "case-lambda:"
    "pcase-lambda:"
    "require/typed"
    "require/opaque-type"
    "require-typed-struct"
    "inst"
    "ann"

    )
  "*Scheme keywords to fontify when `quack-fontify-style' is `plt'."
  :type       '(repeat string)
  :group      'quack
  :set        'quack-custom-set
  :initialize 'custom-initialize-default)

(defcustom quack-emacsish-keywords-to-fontify
  '("and" "begin" "begin0" "call-with-current-continuation"
    "call-with-input-file" "call-with-output-file" "call/cc" "case"
    "case-lambda" "class" "cond" "delay" "do" "else" "exit-handler" "field"
    "for-each" "if" "import" "inherit" "init-field" "interface" "lambda" "let"
    "let*" "let*-values" "let-values" "let-syntax" "let/ec" "letrec"
    "letrec-syntax" "map" "mixin" "opt-lambda" "or" "override" "protect"
    "provide" "public" "rename" "require" "require-for-syntax" "syntax"
    "syntax-case" "syntax-error" "syntax-rules" "unit/sig" "unless" "when"
    "with-syntax")
  "*Scheme keywords to fontify when `quack-fontify-style' is `emacs'."
  :type       '(repeat string)
  :group      'quack
  :set        'quack-custom-set
  :initialize 'custom-initialize-default)

(defcustom quack-fontify-threesemi-p t
  "*Whether three-semicolon comments should be fontified differently."
  :type       'boolean
  :group      'quack
  :set        'quack-custom-set
  :initialize 'custom-initialize-default)

(defcustom quack-pretty-lambda-p nil
  "*Whether Quack should display \"lambda\" as the lambda character.

`quack-fontify-style' must be `plt'.  Only supported under GNU Emacs version
21\; not under XEmacs or older GNU Emacs.

Note: Pretty lambda requires that suitable iso8859-7 fonts be available.  Under
Debian/GNU Linux, for example, these can be downloaded and installed with the
shell command \"apt-get install 'xfonts-greek-*'\".  If iso8859-7 fonts are
unavailable for your system, please notify the Quack author."
  :type       'boolean
  :group      'quack
  :set        'quack-custom-set
  :initialize 'custom-initialize-default)

(defcustom quack-programs
  '("bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -"
    "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -il r6rs"
    "mzscheme -il typed-scheme"
    "mzscheme -M errortrace" 
    "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh"
    "sisc" "stklos" "sxi")
  "List of Scheme interpreter programs that can be used with `run-scheme'.

These names will be accessible via completion when `run-scheme' prompts for
which program to run."
  :group      'quack
  :type       '(repeat string)
  :set        'quack-custom-set
  :initialize 'custom-initialize-default)

(defcustom quack-default-program "mzscheme"
  "Default Scheme interpreter program to use with `run-scheme'."
  :group 'quack
  :type  'string)

(defcustom quack-run-scheme-always-prompts-p t
  "`run-scheme' should always prompt for which program to run.

If nil, `run-scheme' will always use `quack-default-program' when invoked
interactively without a prefix argument; this is closest to the behavior of the
`cmuscheme' package."
  :group 'quack
  :type  'boolean)

(defcustom quack-run-scheme-prompt-defaults-to-last-p t
  "If non-nil, `run-scheme' prompt should default to the last program run."
  :group 'quack
  :type  'boolean)

(defcustom quack-remember-new-programs-p t
  "Programs are added to `quack-programs' automatically."
  :group 'gigule
  :type  'boolean)

(defcustom quack-switch-to-scheme-method 'other-window
  "Method to use for choosing a window and frame for the process buffer.

One of three symbols:
`other-window' will split display in a different window in the current frame,
splitting the current window if necessary.
`own-frame' will display the process buffer in its own frame.
`cmuscheme' will use the normal behavior of the `cmuscheme' package."
  :group  'quack
  :type   '(choice (const :tag "Other Window"       other-window)
                   (const :tag "Own Frame"          own-frame)
                   (const :tag "Cmuscheme Behavior" cmuscheme)))

(defcustom quack-warp-pointer-to-frame-p t
  "Warp mouse pointer to frame with Scheme process buffer.

When `quack-switch-to-scheme-method' is `own-frame', `switch-to-scheme' will
warp the mouse pointer to the frame displaying the Scheme process buffer."
  :group 'quack
  :type  'boolean)

(defcustom quack-newline-behavior 'newline-indent
  "*Behavior of the RET key in Scheme-Mode buffers.  The value is one of three
symbols: `newline' inserts a normal newline, `newline-indent' \(the default\)
inserts a newline and leaves the point properly indented on the new line, and
`indent-newline-indent' indents the current line before inserting a newline and
indenting the new one."
  :type '(choice (const 'newline)
                 (const 'newline-indent)
                 (const 'indent-newline-indent))
  :group 'quack)

(defcustom quack-smart-open-paren-p nil
  "The `[' can be used to insert `(' characters.
Actually, this just makes the `(' and '[' keys both insert `(', unless given a
prefix argument.  This makes typing parens easier on typical keyboards for
which `(' requires a shift modifier but `[' does not.  A later version of Quack
might add actual \"smart\" support for automatic PLT-esque insertion of `['
instead of `(' in some syntactic contexts."
  :group 'quack
  :type  'boolean)

(defcustom quack-options-persist-p t
  "Option menu settings and programs persist using the `custom' facility.

Note that the value of this option itself cannot be set persistently via the
option menu -- you must use the `customize' interface or set it manually in an
Emacs startup file.  This is by design, to avoid the risk of users accidentally
disabling their ability to set persistent options via the option menu."
  :group 'quack
  :type  'boolean)

(defcustom quack-quiet-warnings-p t     ; TODO: Options menu.
  "Warning messages are quiet and subtle."
  :group 'quack
  :type  'boolean)

(defconst quack-pltish-comment-face 'quack-pltish-comment-face)
(defface  quack-pltish-comment-face
  '((((class color) (background light)) (:foreground "cyan4"))
    (((class color) (background dark))  (:foreground "cyan1"))
    (t                                  (:slant italic)))
  "Face used for comments when `quack-fontify-style' is `plt'."
  :group 'quack)

(defconst quack-pltish-selfeval-face 'quack-pltish-selfeval-face)
(defface  quack-pltish-selfeval-face
  '((((class color) (background light)) (:foreground "green4"))
    (((class color) (background dark))  (:foreground "green2"))
    (t                                  ()))
  "Face used for self-evaluating forms when `quack-fontify-style' is `plt'."
  :group 'quack)

(defconst quack-pltish-paren-face 'quack-pltish-paren-face)
(defface  quack-pltish-paren-face
  '((((class color) (background light)) (:foreground "red3"))
    (((class color) (background dark))  (:foreground "red1"))
    (((class grayscale))                (:foreground "gray"))
    (t                                  ()))
  "Face used for parentheses when `quack-fontify-style' is `plt'."
  :group 'quack)

(defconst quack-pltish-colon-keyword-face 'quack-pltish-colon-keyword-face)
(defface  quack-pltish-colon-keyword-face
  '((t (:bold t :foreground "gray50")))
  "Face used for `#:' keywords when `quack-fontify-style' is `plt'.
Note that this isn't based on anything in PLT."
  :group 'quack)

(defconst quack-pltish-paren-face 'quack-pltish-paren-face)
(defface  quack-pltish-paren-face
  '((((class color) (background light)) (:foreground "red3"))
    (((class color) (background dark))  (:foreground "red1"))
    (((class grayscale))                (:foreground "gray"))
    (t                                  ()))
  "Face used for parentheses when `quack-fontify-style' is `plt'."
  :group 'quack)

(defconst quack-banner-face 'quack-banner-face)
(defface  quack-banner-face
  '((t (:family "Helvetica")))
  "Face used in the inferior process buffer for the MzScheme banner.

Currently only takes effect when `quack-fontify-style' is `plt'."
  :group 'quack)

(defconst quack-pltish-defn-face 'quack-pltish-defn-face)
(defface  quack-pltish-defn-face
  '((((class color) (background light)) (:bold t :foreground "blue3"))
    (((class color) (background dark))  (:bold t :foreground "blue1"))
    (t                                  (:bold t :underline t)))
  "Face used for names in toplevel definitions.

For PLT-style when `quack-pltish-fontify-definition-names-p' is non-nil."
  :group 'quack)

(defconst quack-pltish-class-defn-face 'quack-pltish-class-defn-face)
(defface  quack-pltish-class-defn-face
  '((((class color) (background light))
     (:foreground "purple3" :inherit quack-pltish-defn-face))
    (((class color) (background dark))
     (:foreground "purple1" :inherit quack-pltish-defn-face))
    (t (:inherit quack-pltish-defn-face)))
  "Face used for class names in toplevel definitions.

For PLT-style when `quack-pltish-fontify-definition-names-p' is non-nil."
  :group 'quack)

(defconst quack-pltish-module-defn-face 'quack-pltish-module-defn-face)
(defface  quack-pltish-module-defn-face
  '((((class color) (background light))
     (:foreground "purple3" :inherit quack-pltish-defn-face))
    (((class color) (background dark))
     (:foreground "purple1" :inherit quack-pltish-defn-face))
    (t (:inherit quack-pltish-defn-face)))
  "Face used for module names in toplevel definitions.

For PLT-style when `quack-pltish-fontify-definition-names-p' is non-nil."
  :group 'quack)

(defconst quack-pltish-keyword-face 'quack-pltish-keyword-face)
(defface  quack-pltish-keyword-face
  '((t (:bold t)))
  "Face used for keywords in PLT Style fontification.

For PLT-style when `quack-pltish-fontify-keywords-p' is non-nil."
  :group 'quack)

(defconst quack-threesemi-semi-face 'quack-threesemi-semi-face)
(defface  quack-threesemi-semi-face
  '((((class color) (background light))
     (:foreground "#a0ffff":background "#c0ffff"))
    (((class color) (background dark))
     (:foreground "cyan2" :background "cyan4"))
    (t (:slant italic)))
  "Face used for `;;;' semicolons when `quack-fontify-threesemi-p' is non-nil."
  :group 'quack)

(defconst quack-threesemi-text-face 'quack-threesemi-text-face)
(defface  quack-threesemi-text-face
  '((((class color) (background light))
     (:foreground "cyan4" :background "#c0ffff"))
    (((class color) (background dark))
     (:foreground "white" :background "cyan4"))
    (t (:slant italic)))
  "Face used for `;;;' text when `quack-fontify-threesemi-p' is non-nil."
  :group 'quack)

(defconst quack-threesemi-h1-face 'quack-threesemi-h1-face)
(defface  quack-threesemi-h1-face
  '((t (:bold t :family "Helvetica" :height 1.4 :size "20pt")))
  "Face used for H1 headings in `;;;' text."
  :group 'quack)

(defconst quack-threesemi-h2-face 'quack-threesemi-h2-face)
(defface  quack-threesemi-h2-face
  '((t (:bold t :family "Helvetica" :height 1.2 :size "16pt")))
  "Face used for H2 headings in `;;;' text."
  :group 'quack)

(defconst quack-threesemi-h3-face 'quack-threesemi-h3-face)
(defface  quack-threesemi-h3-face
  '((t (:bold t :family "Helvetica")))
  "Face used for H3 headings in `;;;' text."
  :group 'quack)

(defconst quack-pltfile-prologue-face 'quack-pltfile-prologue-face)
(defface  quack-pltfile-prologue-face
  '((((class color))     (:foreground "black" :background "gray66"))
    (((class grayscale)) (:foreground "black" :background "gray66"))
    (t                   ()))
  "Face used for the prologue in a decoded PLT package buffer."
  :group 'quack)

(defconst quack-pltfile-dir-face 'quack-pltfile-dir-face)
(defface  quack-pltfile-dir-face
  '((((class color))     (:bold t :foreground "white" :background "gray33"
                                :family "Helvetica" :height 1.2 :size "20pt"))
    (((class grayscale)) (:bold t :foreground "white" :background "gray33"
                                :family "Helvetica" :height 1.2 :size "20pt"))
    (t                   (:bold t :inverse-video t)))
  "Face used for directory headers in a decoded PLT package buffer."
  :group 'quack)

(defconst quack-pltfile-file-face 'quack-pltfile-file-face)
(defface  quack-pltfile-file-face
  '((((class color))     (:bold t :foreground "black" :background "gray66"
                                :family "Helvetica" :height 1.2 :size "20pt"))
    (((class grayscale)) (:bold t :foreground "black" :background "gray66"
                                :family "Helvetica" :height 1.2 :size "20pt"))
    (t                   (:bold t :inverse-video t)))
  "Face used for file headers in a decoded PLT package buffer."
  :group 'quack)

(defconst quack-about-title-face 'quack-about-title-face)
(defface  quack-about-title-face
  '((((class color) (background light))
     (:bold t :family "Helvetica" :foreground "#008000"
            :height 2.0 :size "24pt"))
    (((class color) (background dark))
     (:bold t :family "Helvetica" :foreground "#00f000"
            :height 2.0 :size "24pt"))
    (t               (:bold t :family "Helvetica"
                            :height 2.0 :size "24pt")))
  "Face used for Quack name in About Quack."
  :group 'quack)

(defconst quack-about-face 'quack-about-face)
(defface  quack-about-face
  '((t (:family "Helvetica")))
  "Face used for the body text in About Quack."
  :group 'quack)

(defconst quack-smallprint-face 'quack-smallprint-face)
(defface  quack-smallprint-face
  '((t (:family "Courier" :height 0.8 :size "8pt")))
  "Face used for the \"small print\" in About Quack."
  :group 'quack)

;; Compatibility/Portability Misc. Kludges:

;; Note: Some compatibility gotchas found while porting Quack that aren't
;; addressed by macros and functions:
;;
;;   * `defface' in Emacs 21 supports ":weight bold", but this is silently
;;     ignored under older Emacsen, so ":bold t" must be used instead.
;;
;;   * Third argument of `detect-coding-region' is different in Emacs 21 and
;;     XEmacs 21, so only use the first two args.
;;
;;   * Under XEmacs 21, characters are `equal' but not `eq' to their integer
;;     ASCII values
;;
;;   * GNU Emacs 21 faces have `:height' property that is either absolute
;;     decipoints or relative scaling factor.  XEmacs 21 faces instead have
;;     `:size' property, which appears to be absolute point or mm size.
;;
;;   * XEmacs 21 text properties appear to be front-sticky, and there did not
;;     seem to be any documentation references to stickiness.
;;
;;   * XEmacs 21 `local-variable-p' has second argument mandatory.
;;
;;   * XEmacs 21 does not display submenu labels at all unless the submenu has
;;     content.  For inactive submenus, an empty string suffices for content.
;;
;;   * XEmacs 21 doesn't support composite characters (which we use for very
;;     nice pretty lambda under GNU Emacs).

(eval-and-compile
  (defvar quack-xemacs-p (eval '(and (boundp 'running-xemacs) running-xemacs)))
  (defvar quack-gnuemacs-p (not quack-xemacs-p)))

(defmacro quack-when-xemacs (&rest args)
  (if quack-xemacs-p (cons 'progn args) 'nil))

(defmacro quack-when-gnuemacs (&rest args)
  (if quack-gnuemacs-p (cons 'progn args) 'nil))

(defmacro quack-define-key-after (keymap key definition &optional after)
  (if quack-gnuemacs-p
      `(define-key-after ,keymap ,key ,definition ,after)
    `(define-key ,keymap ,key (prog1 ,definition ,after))))

(defmacro quack-delete-horizontal-space (&rest args)
  (if (and quack-gnuemacs-p (>= emacs-major-version 21))
      `(delete-horizontal-space ,@args)
    `(delete-horizontal-space)))

(defmacro quack-match-string-no-properties (&rest args)
  `(,(if quack-xemacs-p 'match-string 'match-string-no-properties) ,@args))

(defmacro quack-menufilter-return (name form)
  (if (= emacs-major-version 20)
      ;; Note: This isn't working in Emacs 20.  Menu displays now but actions
      ;;       are not executed.  No answer to test case posted to comp.emacs
      ;;       and then to gnu.emacs.help.  In response to my subsequent bug
      ;;       report against Emacs, RMS says that, if this is indeed a bug,
      ;;       then nothing will be done, since 20 is no longer supported.  I'm
      ;;       going to let this quietly not work unless someone emails me that
      ;;       they're actually using Emacs 20.
      `(easy-menu-filter-return (easy-menu-create-menu ,name ,form))
    form))

(defmacro quack-propertize (obj &rest props)
  (if (and quack-gnuemacs-p (>= emacs-major-version 21))
      `(propertize ,obj ,@props)
    (let ((obj-var 'quack-propertize-G-obj))
      `(let ((,obj-var ,obj))
         (add-text-properties 0 (length ,obj-var) (list ,@props) ,obj-var)
         ,obj-var))))

(eval-when-compile
  (when quack-xemacs-p
    (defvar inhibit-eol-conversion)
    (defvar minibuffer-allow-text-properties)))

;; Compatibility/Portability Hash Table:

(eval-and-compile
  (defmacro quack-make-hash-table (&rest args)
    `(,(if (>= emacs-major-version 21)
           'make-hash-table
         'quack-fake-make-hash-table)
      ,@args)))

(defmacro quack-puthash (key value table)
  (list (if (>= emacs-major-version 21) 'puthash 'quack-fake-puthash)
        key value table))

(defmacro quack-gethash (key table &optional dflt)
  (list (if (>= emacs-major-version 21) 'gethash 'quack-fake-gethash)
        key table dflt))

(defun quack-fake-make-hash-table (&rest args)
  ;; TODO: Parse the keyword args and make this do 'assoc or 'assq, as
  ;;       appropriate.  Currently, this package only needs 'assoc.
  (vector 'assoc '()))

(defun quack-fake-puthash (key value table)
  (let ((pair (funcall (aref table 0) key (aref table 1))))
    (if pair
        (setcdr pair value)
      (aset table 1 (cons (cons key value) (aref table 1))))))

(defun quack-fake-gethash (key table &optional dflt)
  (let ((pair (funcall (aref table 0) key (aref table 1))))
    (if pair (cdr pair) dflt)))

;; Compatibility/Portability Overlays/Extents:

;; TODO: Maybe get rid of overlays (and the XEmacs extent kludge), and just use
;;       text properties instead.

(defmacro quack-make-face-ovlext (beg end face)
  (if quack-xemacs-p
      `(set-extent-property (make-extent ,beg ,end) 'face ,face)
    `(overlay-put (make-overlay ,beg ,end) 'face ,face)))

(defmacro quack-make-hiding-ovlext (beg end)
  (if quack-xemacs-p
      `(set-extent-property (make-extent ,beg ,end) 'invisible t)
    `(overlay-put (make-overlay ,beg ,end) 'category 'quack-hiding-ovlcat)))

;; Messages, Errors, Warnings:

(defmacro quack-activity (what &rest body)
  (let ((var-what (make-symbol "quack-activity-G-what")))
    `(let ((,var-what ,what))
       (message (concat ,var-what "..."))
       (prog1 (progn ,@body)
         (message (concat ,var-what "...done"))))))

(defun quack-internal-error (&optional format &rest args)
  (if format
      (apply 'error (concat "Quack Internal Error: " format) args)
    (error "Quack Internal Error.")))

(defun quack-warning (format &rest args)
  (apply 'message (concat "Quack Warning: " format) args)
  (unless quack-quiet-warnings-p
    (beep)
    (sleep-for 1)))

;; Regular Expressions:

(defun quack-re-alt (&rest regexps)
  (concat "\\(" (mapconcat 'identity regexps "\\|") "\\)"))

(defun quack-re-optional (&rest regexps)
  (concat "\\("
          (apply 'concat regexps)
          "\\)?"))

;; Misc.:

;; (defun quack-abbreviate-file-name (file-name)
;;   (let ((directory-abbrev-alist '()))
;;     (abbreviate-file-name file-name)))

(defun quack-delete-file-if-can (file)
  (condition-case nil (delete-file file) (error nil)))

(defun quack-expand-file-name (name-or-names &optional directory)
  ;; Note: This only works for systems with Unix-like filenames.
  (expand-file-name (if (listp name-or-names)
                        (mapconcat 'identity name-or-names "/")
                      name-or-names)
                    directory))

(defun quack-kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun quack-line-at-point ()
  (save-excursion
    (buffer-substring-no-properties
     (progn (beginning-of-line) (point))
     (progn (end-of-line)       (point)))))

(defun quack-looking-at-backward (re &optional limit)
  (save-excursion
    (save-restriction
      (let ((start-pt (point)))
        (narrow-to-region (point-min) (point))
        (and (re-search-backward re limit t)
             (= (match-end 0) start-pt)
             (match-beginning 0))))))

(defun quack-looking-at-close-paren-backward ()
  (save-match-data
    (quack-looking-at-backward "[])][ \t\r\n\f]*")))

(defun quack-looking-at-open-paren-backward ()
  (save-match-data
    (quack-looking-at-backward "[[(][ \t\r\n\f]*")))

(defun quack-make-directory (dir)
  (setq dir (file-name-as-directory dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun quack-make-directory-for-file (file)
  (let ((dir (file-name-directory file)))
    (when dir (quack-make-directory dir))))

(defun quack-propertize-bold (str)
  (quack-propertize str 'face 'bold))

(defun quack-propertize-face (str face)
  (quack-propertize str 'face face))

(defun quack-propertize-italic (str)
  (quack-propertize str 'face 'italic))

(defun quack-sort-string-list-copy (lst)
  (sort (copy-sequence lst) 'string<))

(defun quack-uncomment-region (beg end)
  ;; TODO: Make a quack-toggle-commentout-region.
  (interactive "r")
  (comment-region beg end '(4)))

(defun quack-without-side-whitespace (str)
  ;; Copied from `padr-str-trim-ws' by author.
  ;;
  ;; TODO: Don't make an intermediate string.  Use regexp match start position.
  (save-match-data
    (if (string-match "^[ \t\n\r]+" str)
        (setq str (substring str (match-end 0))))
    (if (string-match "[ \t\n\r]+$" str)
        (setq str (substring str 0 (match-beginning 0))))
    str))

;; Kludgey Sexp Buffer Operations:

(defconst quack-backward-sexp-re
  (concat "\\`"
          (quack-re-alt "[^\";\\\\]"
                        "\\\\\\."
                        (concat "\""
                                (quack-re-alt "[^\"\\\\]"
                                              "\\\\\\.")
                                "*\""))
          "*\\([\"\\\\]\\)?"))

(defun quack-backward-sexp ()
  ;; Returns non-nil iff point was in a string literal or comment.
  (interactive)
  (when (bobp)
    (error "beginning of buffer"))
  (save-match-data
    (let* ((orig (point))
           (bol  (progn (beginning-of-line) (point))))
      (if (string-match quack-backward-sexp-re
                        (buffer-substring-no-properties bol orig))
          (if (match-beginning 3)
              ;; We're in what appears to be a comment or unterminated string
              ;; literal (though might not be, due to multi-line string
              ;; literals and block comments), so move point to the beginning.
              (progn (goto-char (+ bol (match-beginning 3)))
                     t)
            ;; We don't appear to be in a comment or string literal, so just
            ;; let `backward-sexp' do its thing.
            (goto-char orig)
            (backward-sexp)
            nil)))))

(defun quack-parent-sexp-search (name-regexp &optional max-depth max-breadth)
  (save-match-data
    (save-excursion
      (let ((max-depth   (or max-depth   100))
            (max-breadth (or max-breadth 100))
            (orig-point  (point))
            (found       'looking)
            (depth       0)
            (child-start nil))
        (while (and (eq found 'looking) (< depth max-depth))
          (condition-case nil
              (let ((breadth 0))
                ;; Loop until we hit max breadth or error.
                (while (< breadth max-breadth)
                  (when (and (quack-backward-sexp) (not child-start))
                    (setq child-start (point)))
                  (setq breadth (1+ breadth)))
                ;; We hit our max breadth without erroring, so set the found
                ;; flag to indicate failure and then fall out of our loop.
                (setq found nil))
            (error                      ; scan-error
             ;; We probably hit the beginning of the enclosing sexp, and point
             ;; should be on the first sexp, which will most often be the form
             ;; name, so first check that there really is an open paren to our
             ;; left, and then check if it matches our regexp.
             (let ((paren-start (quack-looking-at-open-paren-backward)))
               (if paren-start
                   ;; There is a paren, so check the name of the form.
                   (if (and (looking-at name-regexp)
                            (quack-not-symbol-char-at-point-p (match-end 0)))
                       ;; Found it, so set the result to a list (lexeme, lexeme
                       ;; end point, last nested child sexp start point, parent
                       ;; paren start point) and then fall out of our loop.
                       ;; Note that we return the original point if no child
                       ;; point was found, on the assumption that point was at
                       ;; the beginning of the child sexp (unless it was within
                       ;; the found form name, in which case child sexp start
                       ;; is nil).
                       (setq found (list (quack-match-string-no-properties 0)
                                         (match-end 0)
                                         (or child-start
                                             (if (> orig-point (match-end 0))
                                                 orig-point))
                                         paren-start))
                     ;; This form name didn't match, so try to move up in the
                     ;; paren syntax (which will usually mean moving left one
                     ;; character).
                     (condition-case nil
                         (progn (up-list -1)
                                (setq child-start (point))
                                (setq depth (1+ depth)))
                       (error           ; scan-error
                        ;; We can't go up here, so set found flag to indicate
                        ;; failure and then fall out of the loop.
                        (setq found nil))))
                 ;; There wasn't a paren, which means we hit a scan error for
                 ;; some reason other than being at the beginning of the sexp,
                 ;; so consider the search a failure
                 (setq found nil))))))
        (if (eq found 'looking)
            nil
          found)))))

;; TODO: We really need a global definition of what are Scheme symbol
;;       constituent characters (or a whole-symbol regexp)!

(defun quack-not-symbol-char-at-point-p (pt)
  ;; This is used to check for a symbol boundary point.
  (save-match-data
    (or (= pt (point-max))
        (if (string-match "[^-a-zA-Z0-9!+<=>$%&*./:@^_~]"
                          (buffer-substring-no-properties pt (1+ pt)))
            t))))

;; String Constant Hashtable:

(eval-and-compile
  (if (< emacs-major-version 21)

      (defun quack-strconst (str) str)

    (defvar quack-strconst-hashtable
      (if (>= emacs-major-version 21)
          (quack-make-hash-table :test 'equal :size 1000)))

    (defun quack-strconst (str)
      (unless (stringp str)
        (error "Non-string object passed to quack-strconst: %s" str))
      (or (quack-gethash str quack-strconst-hashtable nil)
          (quack-puthash str str quack-strconst-hashtable)
          str))))

;; Web URLs:

(defun quack-quote-url-substring (str &optional quote-slash-p always-new-p)
  (save-match-data
    (let ((regexp (if quote-slash-p "[^-_.A-Za-z0-9]" "[^-_.A-Za-z0-9/]"))
          (subs   '())
          (len    (length str))
          (start  0))
      (while (and (> len start)
                  (string-match regexp str start))
        (let ((beg (match-beginning 0))
              (end (match-end       0)))
          (when (> beg start)
            (setq subs (cons (substring str start beg) subs)))
          (setq subs (cons (format "%%%X" (aref str beg)) subs))
          (setq start end)))
      (if subs
          (apply 'concat (reverse (if (> len start)
                                      (cons (substring str start len) subs)
                                    subs)))
        (if always-new-p (copy-sequence str) str)))))

(defun quack-file-url (dir file)
  ;; TODO: This is Unix-centric and a little fragile.  Rewrite eventually.
  (concat "file:"
          (quack-quote-url-substring dir)
          "/"
          (or (quack-quote-url-substring file) "")))

(defun quack-build-url (base path)
  (let ((base-slash-p (= (aref base (1- (length base))) ?\/)))
    (if path
        (mapconcat 'identity
                   (cons (if base-slash-p
                             (substring base 0 -1)
                           base)
                         path)
                   "/")
      (if base-slash-p
          base
        (concat base "/")))))

;; Web Browsing:

(defun quack-browse-url (url)
  (require 'browse-url)
  (message "Quack viewing URL: %s" url)
  (let ((browse-url-browser-function (or quack-browse-url-browser-function
                                         browse-url-browser-function)))
    (browse-url url)))

(defun quack-browse-quack-web-page ()
  (interactive)
  (quack-browse-url quack-web-page))

(defun quack-w3m-browse-url-other-window (url &optional new-window)
  (interactive (eval '(browse-url-interactive-arg "URL: ")))
  (unless (string= (buffer-name) "*w3m*")
    (switch-to-buffer-other-window (current-buffer)))
  ;; TODO: If `*w3m*' buffer is visible in current frame or other frame,
  ;;       switch to that, for Emacsen that don't do that by default.
  (eval '(w3m-browse-url url nil)))

;; Web Getting:

(defconst quack-web-get-log-buffer-name "*quack-web-get*")

(defun quack-web-get-to-file (url out-file)
  ;; TODO: Support other getting tools, such as "lynx -source", "links
  ;;       -source", "w3m -dump_source", and the Emacs w3 package.  Most of
  ;;       these send the Web content to stdout, so, unlike for wget, it will
  ;;       be easier to insert directly to a buffer and send stderr to a temp
  ;;       file.  We should have *-to-file-* and *-insert-via-* functions for
  ;;       each external downloader program anyway.
  (quack-make-directory-for-file out-file)
  (quack-web-get-to-file-via-wget url out-file))

;;(defun quack-web-get-to-temp-file (url)
;;  (let ((temp-file (quack-make-temp-file "web-get")))
;;    (quack-web-get-to-file url temp-file)
;;    temp-file))

(defun quack-web-get-to-file-via-wget (url out-file)
  ;; TODO: Make this initially download to a temp file; replace any
  ;;       pre-existing out-file after successful download.  Do this for any
  ;;       external downloader programs that write to the specified output file
  ;;       before the download is complete.
  (let ((window    (selected-window))
        (saved-buf (current-buffer))
        (log-buf   (get-buffer-create quack-web-get-log-buffer-name)))
    (unwind-protect
        (progn
          ;; Prepare the log buffer.
          (set-buffer log-buf)
          (widen)
          (buffer-disable-undo)
          (goto-char (point-min))
          (delete-region (point-min) (point-max))
          (set-window-buffer window log-buf)
          ;; Do the wget.
          (quack-activity
           (format "Getting %S via wget" url)
           (let ((status (call-process "wget" nil t t
                                       "-O" out-file "-t" "1" "--" url)))
             (unless (= status 0)
               (quack-delete-file-if-can out-file)
               (error "Could not get %S via wget." url))
             (kill-buffer log-buf)
             out-file)))
      ;; unwind-protect cleanup
      (set-window-buffer window saved-buf)
      (set-buffer saved-buf))))

;; HTML Kludges:

(defun quack-strip-limited-html-tags (str)
  (save-match-data
    (let ((case-fold-search t)
          (str-len          (length str))
          (frags            '())
          (start            0))
      (while (string-match "</?[a-z]+[ \r\n]*>" str start)
        (when (> (match-beginning 0) start)
          (setq frags (cons (substring str start (match-beginning 0)) frags)))
        (setq start (match-end 0)))
      (if frags
          (progn (when (< start str-len)
                   (setq frags (cons (substring str start) frags)))
                 (apply 'concat (reverse frags)))
        str))))

;; Temp Files:

(defun quack-temp-dir ()
  (file-name-as-directory (expand-file-name "tmp" quack-dir)))

;; TODO: Make sure this gets executed in load phase even if byte-compiled.

(random t)

(defun quack-make-temp-file (purpose-str)
  ;; Note: There is an obvious race condition here.  But we're trying to do
  ;;       this in portable Elisp, and if user's `quack-dir' is writable by
  ;;       someone other than user, then user has bigger problems.
  (save-excursion
    (let* ((buf (generate-new-buffer "*quack-make-temp-file*"))
           (dir (quack-temp-dir))
           file)
      (set-buffer buf)
      (quack-make-directory dir)
      (while (progn (setq file (expand-file-name (format "%d-%s-%d"
                                                         (emacs-pid)
                                                         purpose-str
                                                         (random 10000))
                                                 dir))
                    (file-exists-p file)))
      (set-visited-file-name file)
      (save-buffer 0)
      (kill-buffer buf)
      file)))

;; About:

(defun quack-about ()
  (interactive)
  (let* ((buf-name "*About Quack*")
         (buf      (get-buffer buf-name)))
    (when buf (kill-buffer buf))
    (setq buf (get-buffer-create buf-name))
    (switch-to-buffer buf)
    (setq buffer-read-only nil)
    (widen)
    (fundamental-mode)
    (when font-lock-mode
      ;;(quack-warning "Font-lock mode mysteriously on in fundamental-mode.")
      (font-lock-mode -1))
    (buffer-disable-undo)
    ;;(delete-region (point-min) (point-max))
    (erase-buffer)
    (insert
     "\n"
     (quack-propertize-face (copy-sequence "Quack") 'quack-about-title-face)
     "   Version "
     (quack-propertize-bold (copy-sequence quack-version))
     "\n"
     (quack-propertize-italic
      (copy-sequence "Enhanced Emacs support for Scheme programming"))
     "\n\n"
     "You can email bug reports and feature requests to the author,\n"
     quack-author-name
     " <"
     quack-author-email
     ">.  Mention that\n"
     "you are using "
     (quack-propertize-bold
      (copy-sequence
       (cond (quack-gnuemacs-p "GNU Emacs")
             (quack-xemacs-p   "XEmacs")
             (t                "*an unrecognized Emacs kind*"))))
     " "
     (quack-propertize-bold
      (format "%d.%d" emacs-major-version emacs-minor-version))
     " on "
     (quack-propertize-bold (copy-sequence system-configuration))
     ".\n\n"
     "To be notified via email when new Quack versions are released,\n"
     "ask Neil to add you to the moderated "
     (quack-propertize-bold "scheme-announce")
     " list.\n\n"
     "Visit the Web page:  "
     quack-web-page
     "\n")
    (insert "\n\n"
            (quack-propertize-face (copy-sequence quack-copyright)
                                   'quack-smallprint-face)
            "\n"
            (quack-propertize-face (copy-sequence quack-copyright-2)
                                   'quack-smallprint-face)
            "\n\n"
            (quack-propertize-face (concat quack-legal-notice "\n")
                                   'quack-smallprint-face))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (local-set-key "q" 'quack-kill-current-buffer)
    (local-set-key "w" 'quack-browse-quack-web-page)
    (message
     "Press `q' to quit *About Quack*, `w' to visit the Quack Web page.")))

;; PLT Collections:

(defvar quack-pltcollects-alist-cache nil)

(defun quack-invalidate-pltcollects-caches ()
  (setq quack-pltcollects-alist-cache nil)
  (quack-invalidate-manuals-caches))

(defun quack-pltcollects-alist ()
  (or quack-pltcollects-alist-cache
      (quack-activity
       "Scanning PLT collection directories"
       (let ((result '()))
         (mapcar (function
                  (lambda (dir)
                    (mapcar (function
                             (lambda (subdir)
                               (unless (member subdir '("." ".." "CVS" "RCS"))
                                 (let ((subdir-path (expand-file-name subdir
                                                                      dir)))
                                   (when (file-directory-p subdir-path)
                                     (setq result
                                           (cons (cons subdir subdir-path)
                                                 result)))))))
                            (condition-case nil
                                (directory-files dir)
                              (file-error nil)))))
                 quack-pltcollect-dirs)
         (setq quack-pltcollects-alist-cache (reverse result))))))

(defun quack-dir-for-pltcollect (name)
  (cdr (assoc name (quack-pltcollects-alist))))

(defun quack-dired-pltcollect ()
  (interactive)
  (let* ((alist   (quack-pltcollects-alist))
         (default (if (assoc "mzlib" alist) "mzlib" nil))
         (dir (cdr (assoc
                    (completing-read
                     (if default 
                         (format "Dired for PLT collection (default %S): "
                                 default)
                       "Dired for PLT collection: ")
                     alist nil t nil nil default)
                    alist))))
    (and dir (dired dir))))

;; Find File:

(defun quack-shorter-file-relative-name (filename &optional directory)
  (let ((absolute (expand-file-name   filename directory))
        (relative (file-relative-name filename directory)))
    (if (< (length relative) (length absolute))
        relative
      absolute)))

;; TODO: Also write `quack-find-file-other-window' and
;;       `quack-find-file-other-frame' and steal appropriate key bindings.

(defun quack-find-file ()
  ;; TODO: Hangup/delay problems in mega-huge files.
  ;;
  ;; TODO: Handle `(load <filename>)'
  (interactive)
  (let* ((default (quack-find-file-default))
         (entry   (let ((insert-default-directory (if default
                                                      nil
                                                    insert-default-directory)))
                    (read-file-name
                     (if default
                         (format "Quack find file (default %S): "
                                 (quack-shorter-file-relative-name
                                  default
                                  default-directory))
                       "Quack find file: ")
                     default-directory
                     default))))
    (find-file (if (string= entry "")
                   (or default "")
                 entry))))

(defun quack-find-file-default ()
  (or (quack-pltrequire-at-point-filename)
      ;; TODO: Add support for syntax from Guile, SLIB, Chicken, etc.
      ))

;; TODO: Guile `:use-module' support.  Forget about 1.4, and do 1.6.
;;
;; (defun quack-guilecolonusemodule-at-point-data ()
;;   (save-match-data
;;     (when (thing-at-point-looking-at
;;            ":use-module[ \t]+\\(([^][()\"#'`,]+)\\)")
;;       (condition-case nil
;;           (car (read-from-string (buffer-substring-no-properties
;;                                   (match-beginning 1) (match-end 1))))
;;         (error nil)))))
;;
;; ;; (define-module (ice-9 expect) :use-module (ice-9 regex))

;; TODO: Guile 1.6 `use-modules' and `use-syntax' support.
;;
;; (use-modules (ice-9 regex))
;;
;; (use-modules ((ice-9 popen)
;;               :select  ((open-pipe . pipe-open) close-pipe)
;;               :renamer (symbol-prefix-proc 'unixy:)))
;;
;; (use-modules { SPEC }+ )
;;
;; SPEC ::= MODULE-NAME | (MODULE-NAME [:select SELECTION] [:renamer RENAMER])
;;
;; (use-syntax MODULE-NAME)

;; TODO: Support SLIB-style `require' forms:
;;
;; (require 'foo)

;; TODO: Bigloo `import' and maybe `extern' support.
;;
;; ;; /usr/share/doc/bigloo-examples/examples/Foreign/
;; (module example
;;   (import (bis foreign2 "foreign2.scm"))
;;   ...)
;;
;; ;; /usr/share/doc/bigloo-examples/examples/Fork/
;; (module sys-example
;;   (extern (include "sys/types.h")
;;           (include "wait.h")
;;           (include "unistd.h")
;;           ...))

;; TODO: PLT module language syntax: (module info (lib "infotab.ss" "setup")

(defconst quack-pltrequire-at-point-data-re
  (quack-re-alt "dynamic-require"
                (concat "require"
                        (quack-re-alt "-for-syntax"
                                      ""))))

(defconst quack-pltrequire-at-point-data-1-re
  (concat quack-pltrequire-at-point-data-re
          "\\>"))

(defconst quack-pltrequire-at-point-data-2-re
  (concat "[^\r\n]*[[(]"
          quack-pltrequire-at-point-data-re
          "[ \t]+\\([^\r\n]+\\)"))

(defun quack-pltrequire-at-point-data-1 ()
  (save-match-data
    (let ((qpss (quack-parent-sexp-search quack-pltrequire-at-point-data-1-re
                                          4)))
      (when qpss
        (let ((child-start (nth 2 qpss)))
          (when child-start
            (save-excursion
              (goto-char child-start)
              (condition-case nil
                  ;; Note: It is normally OK to use the Elisp reader here.
                  (read (current-buffer))
                (error nil)))))))))

(defun quack-pltrequire-at-point-data-2 ()
  (save-match-data
    (when (thing-at-point-looking-at quack-pltrequire-at-point-data-2-re)
      (let* ((read-start (match-beginning 2))
             (parts-pt   (- (point) read-start))
             (parts      (buffer-substring-no-properties read-start
                                                         (match-end 2)))
             (parts-len  (length parts))
             (start      0)
             (result     '()))
        (condition-case nil
            (while (< start parts-len)
              ;; Note: It is normally OK to use the Elisp reader here.
              (let ((r (read-from-string parts start)))
                (when (or (not result) (> parts-pt start))
                  (setq result (car r)))
                (setq start (cdr r))))
          (error nil))
        result))))

(defun quack-pltrequire-at-point-filename (&optional silent)
  (let* ((d (or (quack-pltrequire-at-point-data-1)
                (quack-pltrequire-at-point-data-2)))
         (m (cond
             ((not     d) nil)
             ((stringp d) d)
             ((listp   d)
              (let ((f (car d)))
                (when (symbolp f)
                  (cond ((memq f '(file lib))                 d)
                        ((memq f '(all-except rename))        (nth 1 d))
                        ((memq f '(prefix prefix-all-except)) (nth 2 d)))))))))
    (cond
     ((stringp m) m)
     ((listp   m)
      (let ((f (car m)))
        (when (symbolp f)
          (cond ((eq f 'file) (nth 1 f))
                ((eq f 'lib)
                 (let* ((file        (nth 1 m))
                        (collect     (or (nth 2 m) "mzlib"))
                        (collect-dir (quack-dir-for-pltcollect collect))
                        (subs        (nthcdr 3 m)))
                   (when file
                     (if collect-dir
                         (quack-expand-file-name (nconc subs (list file))
                                                 collect-dir)
                       (unless silent
                         (quack-warning "Cannot find collection %S" collect))
                       nil)))))))))))

;; Indenting Newline:

(defun quack-newline (&optional arg)
  (interactive "*P")
  (if (eq quack-newline-behavior 'newline)
      (newline arg)
    (if (eq quack-newline-behavior 'indent-newline-indent)
        (lisp-indent-line)
      (unless (eq quack-newline-behavior 'newline-indent)
        (error "invalid quack-newline-behavior value: %s"
               quack-newline-behavior)))
    (let ((n (prefix-numeric-value arg)))
      (when (> n 0)
        (while (> n 0)
          (setq n (1- n))
          (quack-delete-horizontal-space t)
          (newline))
        (lisp-indent-line)))))

;; Agreeing-Paren Insert:

;; TODO: Make paren-matching within comments limit seaching to within comments,
;;       not skip back and try to match code.  One workaround is to prefix
;;       parents/brackets in comments with backslash.

(defun quack-insert-closing (prefix default-close other-open other-close)
  (insert default-close)
  (unless prefix
    (let ((open-pt (condition-case nil
                       (scan-sexps (point) -1)
                     (error (beep) nil))))
      (when open-pt
        (let ((open-char (aref (buffer-substring-no-properties
                                open-pt (1+ open-pt))
                               0)))
          (when (= open-char other-open)
            (delete-backward-char 1)
            (insert other-close))))))
  (when blink-paren-function (funcall blink-paren-function)))

(defun quack-insert-closing-paren (&optional prefix)
  (interactive "P")
  (quack-insert-closing prefix ?\) ?\[ ?\]))

(defun quack-insert-closing-bracket (&optional prefix)
  (interactive "P")
  (quack-insert-closing prefix ?\] ?\( ?\)))

;; Opening-Paren Insert:

(defun quack-insert-opening (prefix char)
  (insert (if (or prefix (not quack-smart-open-paren-p)) char ?\())
  (when blink-paren-function (funcall blink-paren-function)))

(defun quack-insert-opening-paren (&optional prefix)
  (interactive "P")
  (quack-insert-opening prefix ?\())

(defun quack-insert-opening-bracket (&optional prefix)
  (interactive "P")
  (quack-insert-opening prefix ?\[))

;; Definition Lambda Syntax Toggling:

(defconst quack-toggle-lambda-re-1
  (concat "define\\*?"
          (quack-re-alt "-for-syntax"
                        "-public"
                        "/override"
                        "/private"
                        "/public"
                        "")))

(defconst quack-toggle-lambda-re-2
  (let ((ws-opt      "[ \t\r\n\f]*")
        (symbol      "[^][() \t\r\n\f]+")
        (open-paren  "[[(]")
        (close-paren "[])]"))
    (concat ws-opt
            (quack-re-alt               ; #=1
             (concat "\\("              ; #<2 `NAME (lambda ('
                     "\\("              ; #<3 name
                     symbol
                     "\\)"              ; #>3
                     ws-opt
                     open-paren
                     ws-opt
                     "lambda"
                     ws-opt
                     open-paren
                     ws-opt
                     "\\)")
             (concat "\\("              ; #<4 `(NAME'
                     open-paren
                     ws-opt
                     "\\("              ; #<5 name
                     symbol
                     "\\)"              ; #>5
                     ws-opt
                     "\\)"))
            "\\("                       ; #<6 optional close paren
            close-paren
            "\\)?"                      ; #>6
            )))

(defun quack-toggle-lambda ()
  (interactive)
  (save-match-data
    (let ((found (quack-parent-sexp-search quack-toggle-lambda-re-1))
          last-paren-marker
          leave-point-marker)
      (unless found
        (error "Sorry, this does not appear to be a definition form."))
      (unwind-protect
          (let ((lexeme-end (nth 1 found))
                (define-beg (nth 3 found)))

            ;; Make the markers.
            (setq last-paren-marker  (make-marker))
            (setq leave-point-marker (point-marker))

            ;; Move to right after the define form keyword, and match the
            ;; pattern of the two possible syntaxes.  Error if no match.
            (goto-char lexeme-end)
            (unless (looking-at quack-toggle-lambda-re-2)
              (error "Sorry, we can't grok this definition syntax."))

            ;; Pattern matched, so find the closing paren of the define form.
            (let ((pt (condition-case nil
                          (scan-sexps define-beg 1)
                        (error          ; scan-error
                         nil))))
              (if pt
                  (set-marker last-paren-marker (1- pt))
                (quack-warning
                 "This definition form sexp is unclosed.  Consider undo.")))

            ;; Now act based on which syntax we saw.
            (cond

             ((match-beginning 2)
              ;; We saw the syntax `NAME (lambda ('.
              (let ((name (quack-match-string-no-properties 3)))
                (when (marker-position last-paren-marker)
                  (goto-char last-paren-marker)
                  (let ((victim-beg (quack-looking-at-close-paren-backward)))
                    (unless victim-beg
                      (error "This definition form should end with `))'."))
                    (delete-region victim-beg (point))))
                (goto-char lexeme-end)
                (delete-region lexeme-end (match-end 2))
                (insert " (" name (if (match-beginning 6) "" " "))))

             ((match-beginning 4)
              ;; We saw the syntax `(NAME'.
              (let ((name (quack-match-string-no-properties 5)))
                (when (marker-position last-paren-marker)
                  (goto-char last-paren-marker)
                  (insert ")"))
                (goto-char lexeme-end)
                (delete-region lexeme-end (match-end 4))
                (insert " " name "\n")
                (set-marker leave-point-marker (point))
                (insert "(lambda (")
                (set-marker-insertion-type leave-point-marker t)))

             (t (quack-internal-error)))

            ;; Reindent, which also takes care of font-lock updating of deleted
            ;; and inserted text.
            (indent-region define-beg
                           (or (marker-position last-paren-marker)
                               (max (marker-position leave-point-marker)
                                    (point)))
                           nil))

        ;; unwind-protect cleanup
        (goto-char (marker-position leave-point-marker))
        (set-marker leave-point-marker nil)))))

;; Buffer Tidying:

;; TODO: Maybe have an option to automatically tidy the buffer on save.  Make
;;       default off.  This can be slow for larger buffers on older computers,
;;       especially if font-lock is activated.  It can also annoy people who
;;       have a CM system full of improperly formatted files, or who like
;;       things like formfeed characters in their files.

(defun quack-delete-all-in-buffer (regexp &optional subexp)
  (unless subexp (setq subexp 0))
  ;; Note: This moves the point and changes the match data.
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (goto-char (match-end subexp))
    (delete-region (match-beginning subexp) (point))))

(defun quack-tidy-buffer ()

  ;; TODO: Make sure this works with odd eol conventions and the various
  ;;       codeset representations in various versions of Emacs.

  ;; TODO: Maybe detect DrScheme ASCII-art "big letters" and protect them from
  ;;       reindenting.

  "Tidy the formatting of the current Scheme buffer.

This reindents, converts tabs to spaces, removes trailing whitespace on lines,
removes formfeed characters, removes extraneous blank lines, and makes sure
the buffer ends with a newline.

This can conceivably corrupt multi-line string literals, but not in any way
they wouldn't be corrupted by Usenet, various mailers, typesetting for print,
etc.

This may also result in large diffs when the tidied file is commited back to a
version control or configuration management system.  Consider making a VC or CM
delta that consists only of changes made by `quack-tidy-buffer'."
  (interactive)
  (if (= (point-min) (point-max))
      (message "Buffer is empty; no tidying necessary.")
    (let ((marker      (point-marker))
          (fill-prefix nil))
      (unwind-protect
          (save-excursion
            (save-match-data
              (quack-activity
               "Tidying buffer"

               ;; Make sure last character is a newline.
               (unless (string= "\n" (buffer-substring-no-properties
                                      (1- (point-max))
                                      (point-max)))
                 (goto-char (point-max))
                 (insert "\n"))

               ;; Remove form-feed characters.
               (quack-delete-all-in-buffer "\f")

               ;; Reindent buffer (without inserting any new tabs).
               ;; Note: This is the time-consuming pass.
               (let ((saved-indent-tabs-mode indent-tabs-mode))
                 (unwind-protect
                     (progn (setq indent-tabs-mode nil)
                            (indent-region (point-min) (point-max) nil))
                   ;; unwind-protect cleanup
                   (setq indent-tabs-mode saved-indent-tabs-mode)))

               ;; Expand any remaining tabs.
               (untabify (point-min) (point-max))

               ;; Remove trailing whitespace on each line.
               (quack-delete-all-in-buffer "\\([ \t\r]+\\)\n" 1)

               ;; Remove blank lines from top.
               (goto-char (point-min))
               (when (looking-at "[ \t\r\n]+")
                 (delete-region (match-beginning 0) (match-end 0)))

               ;; Remove excess adjacent blank lines.
               (quack-delete-all-in-buffer "\n\n\\(\n+\\)" 1)

               ;; Remove blank lines from bottom.
               (goto-char (point-max))
               (when (quack-looking-at-backward
                      "\n\\(\n\\)"
                      (max (point-min) (- (point-max) 3)))
                 (delete-region (match-beginning 1) (match-end 1))))))

        ;; unwind-protect cleanup
        (goto-char (marker-position marker))
        (set-marker marker nil)))))

;; SRFIs:

;; TODO: Archive local copies of SRFIs?  Have to update them when modified, but
;;       without unnecessarily downloading from the master site.  This is
;;       doable with wget mirroring, but not with things like "lynx -source".

(defconst quack-srfi-subindex-kinds '(draft final withdrawn)
  "List of symbols representing the three possible states of an SRFI (`draft',
`final', and `withdrawn'), in order of increasing precedence (e.g., final
follows draft,since a final version supercedes a draft version).")

(defvar quack-srfi-completes-cache 'invalid)
(defvar quack-srfi-menu-cache      'invalid)

(defun quack-srfi-completes ()
  (when (eq quack-srfi-completes-cache 'invalid)
    (quack-process-srfi-subindex-files))
  quack-srfi-completes-cache)

(defun quack-srfi-menu (&optional noninteractive)
  (when (eq quack-srfi-menu-cache 'invalid)
    (quack-process-srfi-subindex-files noninteractive))
  quack-srfi-menu-cache)

(defun quack-srfi-master-url (path)
  (quack-build-url quack-srfi-master-base-url path))

(defun quack-srfi-subindex-master-url (kind)
  (quack-srfi-master-url (list (quack-srfi-subindex-basename kind))))

(defun quack-srfi-dir ()
  (file-name-as-directory (expand-file-name "srfi" quack-dir)))

(defun quack-srfi-subindex-file (kind)
  (expand-file-name (quack-srfi-subindex-basename kind) (quack-srfi-dir)))

(defun quack-srfi-subindex-basename (kind)
  (format "%S-srfis.html" kind))

(defun quack-invalidate-srfi-index-caches ()
  (setq quack-srfi-completes-cache 'invalid)
  (setq quack-srfi-menu-cache      'invalid))

(defun quack-update-srfi-index ()
  (interactive)
  (quack-activity
   "Updating SRFI index"
   (quack-download-srfi-subindex-files)))

(defun quack-download-srfi-subindex-files ()
  (quack-invalidate-srfi-index-caches)
  (mapcar (function
           (lambda (kind)
             (quack-activity
              (format "Downloading %s SRFI subindex" kind)
              (quack-web-get-to-file (quack-srfi-subindex-master-url kind)
                                     (quack-srfi-subindex-file       kind)))))
          quack-srfi-subindex-kinds))

(defun quack-download-srfi-subindex-files-if-missing ()
  (let ((missing '()))
    (mapcar (function
             (lambda (kind)
               (unless (file-exists-p (quack-srfi-subindex-file kind))
                 (setq missing (nconc missing (list kind))))))
            quack-srfi-subindex-kinds)
    (when (and missing
               (y-or-n-p "Some cached SRFI subindexes are missing. Update? "))
      (quack-update-srfi-index))))

(defun quack-process-srfi-subindex-files (&optional noninteractive)
  (let ((index      '())
        (completes  '())
        (menu       (mapcar (function (lambda (kind) (cons kind nil)))
                            quack-srfi-subindex-kinds)))

    ;; Invalidate dependent caches.
    (quack-invalidate-srfi-index-caches)

    ;; Give user a chance to download any missing cache files all at once,
    ;; instead of prompting individually later.
    (unless noninteractive
      (quack-download-srfi-subindex-files-if-missing))

    ;; Parse the index files, letting entries for successive states supercede.
    (mapcar (function
             (lambda (kind)
               (mapcar (function
                        (lambda (new)
                          (let (old)
                            (if (setq old (assq (car new) index))
                                (setcdr old (cdr new))
                              (setq index (cons new index))))))
                       (quack-parse-srfi-subindex-file kind noninteractive))))
            quack-srfi-subindex-kinds)

    ;; Sort the parse form in reverse order, since the cache-building functions
    ;; will reverse this.
    (setq index (sort index (function (lambda (a b) (>= (car a) (car b))))))

    ;; Build the completions and menu caches.
    (let ((fmt (concat "%"
                       (if index
                           (number-to-string
                            (length (number-to-string (car (car index)))))
                         "")
                       "d  %s")))
      (mapcar (function
               (lambda (n)
                 (let ((num      (nth 0 n))
                       (kind     (nth 1 n))
                       (title    (nth 2 n)))
                   (unless kind (quack-internal-error))
                   (setq completes
                         (cons (cons (if (eq kind 'final)
                                         (format "%d  %s" num title)
                                       (format "%d  [%s] %s" num kind title))
                                     num)
                               completes))
                   (let ((pair (or (assq kind menu)
                                   (quack-internal-error))))
                     (setcdr pair (cons `[,(format fmt num title)
                                          (quack-view-srfi ,num)]
                                        (cdr pair)))))))
              index))

    ;; Finish the menu.
    (mapcar (function (lambda (n)
                        (setcar n (cdr (assoc (car n)
                                              '((draft     . "Draft")
                                                (final     . "Final")
                                                (withdrawn . "Withdrawn")))))
                        ;; Add dummy content so that XEmacs 21 will display
                        ;; the submenu label.
                        (unless (cdr n)
                          (setcdr n (cons "(None)" nil)))))
            menu)
    (setq menu `(["Update SRFI Index" quack-update-srfi-index]
                 "---"
                 ,@menu
                 ["Other SRFI..." quack-view-srfi]))

    ;; Store the results.
    (setq quack-srfi-menu-cache      menu)
    (setq quack-srfi-completes-cache completes)))

(defun quack-parse-srfi-subindex-file (kind &optional noninteractive)
  (save-excursion
    (let ((file (quack-srfi-subindex-file kind)))
      (unless (file-exists-p file)
        (error "No SRFI index file %S" file))
      (let* ((buf                (get-file-buffer file))
             (already-visiting-p buf))
        (unless buf
          (setq buf (find-file-noselect file t t)))
        (unwind-protect
            (progn (set-buffer buf)
                   (quack-parse-srfi-subindex-buffer kind))
          ;; unwind-protect-cleanup
          (unless already-visiting-p
            (kill-buffer buf)))))))

(defconst quack-parse-srfi-index-buffer-re-1
  (concat
   "<LI><A HREF=\"?srfi-[0-9]+/?\"?>SRFI[ \t]+"
   "\\([0-9]+\\)"                       ; #=1 srfi number
   "</A>:?[ \t]*"
   "\\("                                ; #<2 srfi title
                                        ; #=3
   (quack-re-alt "[^\r\n<>]" "</?[a-z]+>")
   "+"
   "\\)"))

(defun quack-parse-srfi-subindex-buffer (kind)
  (save-excursion
    (let ((case-fold-search t)
          (alist            '()))
      (goto-char (point-min))
      (while (re-search-forward quack-parse-srfi-index-buffer-re-1 nil t)
        (let ((number (string-to-number (quack-match-string-no-properties 1)))
              (title  (quack-without-side-whitespace
                       (quack-strip-limited-html-tags
                        (quack-match-string-no-properties 2)))))
          (setq alist (cons

                       ;;(cons number
                       ;;      (if (and kind (not (eq kind 'final)))
                       ;;          (format "[%s] %s" kind title)
                       ;;        title))
                       (list number kind title)

                       alist))))
      (setq alist (reverse alist)))))

(defun quack-srfi-num-url (num)
  (quack-srfi-master-url (list (format "srfi-%d"      num)
                               (format "srfi-%d.html" num))))

(defconst quack-srfi-num-at-point-re-1
  "srfi[-: \t]*\\([0-9]+\\)")

(defconst quack-srfi-num-at-point-re-2
  ;; Note: We can't have "[^\r\n]*" as a prefix, since it's too slow.
  (concat quack-srfi-num-at-point-re-1 "[^\r\n]*"))

(defun quack-srfi-num-at-point ()
  ;; TODO: Make this get the nearest SRFI number in all cases.
  (save-match-data
    (let ((case-fold-search t))
      (cond ((thing-at-point-looking-at quack-srfi-num-at-point-re-1)
             (string-to-number (quack-match-string-no-properties 1)))
            ((thing-at-point-looking-at "[0-9]+")
             (string-to-number (quack-match-string-no-properties 0)))
            ((thing-at-point-looking-at quack-srfi-num-at-point-re-2)
             (string-to-number (quack-match-string-no-properties 1)))
            ((let ((str (quack-line-at-point)))
               (when (string-match quack-srfi-num-at-point-re-1 str)
                 (string-to-number
                  (quack-match-string-no-properties 1 str)))))))))

(defun quack-view-srfi (num)
  (interactive (list (quack-srfi-num-prompt "View SRFI number")))
  (when num
    (unless (and (integerp num) (>= num 0))
      (error "Not a valid SRFI number: %S" num))
    (quack-browse-url (quack-srfi-num-url num))))

(defun quack-srfi-num-prompt (prompt)
  (let* ((completes (quack-srfi-completes))
         (default   (quack-srfi-num-at-point))
         (input     (quack-without-side-whitespace
                     (completing-read
                      (if default
                          (format "%s (default %d): " prompt default)
                        (concat prompt ": "))
                      completes)))
         v)
    (cond ((or (not input) (string= "" input)) default)
          ((setq v (assoc input completes))      (cdr v))
          ((and (setq v (condition-case nil
                            (string-to-number input)
                          (error nil)))
                (integerp v)
                (>= v 0))
           v)
          (t (error "Invalid SRFI number: %s" input)))))

;; Doc Keyword Value Object:

(defmacro quack-kw-get-syntax   (o) `(aref ,o 0))
(defmacro quack-kw-get-file     (o) `(aref ,o 1))
(defmacro quack-kw-get-fragment (o) `(aref ,o 2))

(defmacro quack-kw-set-syntax   (o v) `(aset ,o 0 ,v))
(defmacro quack-kw-set-file     (o v) `(aset ,o 1 ,v))
(defmacro quack-kw-set-fragment (o v) `(aset ,o 2 ,v))

;; Documentation Object:

;; TODO: Rework these document representations once we know the different kinds
;;       of documents with which we'll be dealing.

(defmacro quack-doc-get-type         (o) `(aref ,o 0))
(defmacro quack-doc-get-sym          (o) `(aref ,o 1))
(defmacro quack-doc-get-title        (o) `(aref ,o 2))
(defmacro quack-doc-get-loc          (o) `(aref ,o 3))
(defmacro quack-doc-get-kw-p         (o) `(aref ,o 4))
(defmacro quack-doc-get-start-url    (o) `(aref ,o 5))
(defmacro quack-doc-get-kw-base-url  (o) `(aref ,o 6))
(defmacro quack-doc-get-kw-file      (o) `(aref ,o 7))
(defmacro quack-doc-get-kw-hashtable (o) `(aref ,o 8))

(defmacro quack-doc-set-type         (o v) `(aset ,o 0 ,v))
(defmacro quack-doc-set-sym          (o v) `(aset ,o 1 ,v))
(defmacro quack-doc-set-title        (o v) `(aset ,o 2 ,v))
(defmacro quack-doc-set-loc          (o v) `(aset ,o 3 ,v))
(defmacro quack-doc-set-kw-p         (o v) `(aset ,o 4 ,v))
(defmacro quack-doc-set-start-url    (o v) `(aset ,o 5 ,v))
(defmacro quack-doc-set-kw-base-url  (o v) `(aset ,o 6 ,v))
(defmacro quack-doc-set-kw-file      (o v) `(aset ,o 7 ,v))
(defmacro quack-doc-set-kw-hashtable (o v) `(aset ,o 8 ,v))

(defun quack-manual-to-doc (manual)
  ;; Accepts a user's manual preference object of the list form:
  ;;
  ;;     (SYM TITLE LOC KW-P)
  ;;
  ;; and creates a manual doc object of the vector form:
  ;;
  ;;     [manual SYM TITLE LOC KW-P START-URL KW-BASE-URL KW-FILE KW-P
  ;;      KEYWORDS]
  ;;
  ;; KEYWORDS is not populated here -- keywords importing for a manual happens
  ;; the first time keyword searching is done for the manual."
  (let ((sym       (nth 0 manual))
        (title     (nth 1 manual))
        (loc       (nth 2 manual))
        (kw-p      (nth 3 manual))
        (start-url nil)
        (kw-file   nil)
        (kw-base   nil))
    (cond
     ;; If the location is a string, then handle manual as simple URL.
     ((stringp loc)
      (setq start-url loc)
      (when kw-p
        (quack-warning "Quack can only use keywords for PLT manuals.")
        (setq kw-p nil)))
     ;; If the location is a symbol, handle manual as special.
     ((symbolp loc)
      (cond
       ;; If the location is symbol `plt', handle manual as PLT bundled.
       ((eq loc 'plt)
        (let* ((plt-name (let ((s (symbol-name sym)))
                           (if (string-match "\\`plt-\\(.+\\)\\'" s)
                               (match-string 1 s)
                             s)))
               (web-base   (concat
                           "http://download.plt-scheme.org/doc/"
                           plt-name
                           "/"))
              (index-name "index.htm")
              (col-dirs   quack-pltcollect-dirs))
          ;; Search from the collection directories for keywords and index
          ;; files.  Note that we currently look for keywords files even if
          ;; `kw-p' is false since we want to allow the user to dynamically
          ;; enable and disable keywords searching for a particular manual
          ;; without us having to change `quack-docs'.
          (while (and col-dirs (not (and kw-file kw-base start-url)))
            (let ((dir (expand-file-name plt-name
                                         (expand-file-name "doc"
                                                           (car col-dirs)))))
              (setq col-dirs (cdr col-dirs))
              (when (file-directory-p dir)
                (let* ((k-f (expand-file-name "keywords" dir))
                       (i-f (expand-file-name index-name dir))
                       (i-r (file-readable-p i-f)))
                  (if (file-readable-p k-f)
                      ;; Keywords file.
                      (if i-r
                          ;; Keywords file and index file.  So, unless we
                          ;; already found a keywords base URL, set everything
                          ;; based on this directory.  Note that we override
                          ;; any existing start URL because we prefer to use
                          ;; the same manual version for both keywords and
                          ;; non-keywords access.
                          (unless kw-base
                            (setq kw-file   k-f)
                            (setq kw-base   (quack-file-url dir nil))
                            (setq start-url (quack-file-url dir index-name)))
                        ;; Keywords file, but no index file.  So, unless we
                        ;; already have a keywords file, set it to this one.
                        (unless kw-file
                          (setq kw-file k-f)))
                    ;; No keywords file.  So, if there is an index file, and we
                    ;; don't already have one, then use this one.
                    (when (and i-r (not start-url))
                      (setq start-url (quack-file-url dir index-name))))))))
          ;; If we didn't find a start URL, use the Web one.
          (unless start-url
            (setq start-url (concat web-base index-name)))
          ;; Do we have a keywords file?
          (if kw-file
              ;; We have a keywords file, so set the keywords base to the Web
              ;; if needed and desired.  Note that we never use the keywords
              ;; file from one directory with the HTML files from a different
              ;; directory, on the assumption that a local copy of HTML missing
              ;; a keywords file is suspect, and that the Web version is
              ;; therefore preferable.
              (when (or (eq quack-local-keywords-for-remote-manuals-p 'always)
                        (and (not kw-base)
                             quack-local-keywords-for-remote-manuals-p))
                (setq kw-base web-base))
            ;; We don't have a keywords file, so warn if the user wanted
            ;; keywords for this manual.
            (when kw-p
              (quack-warning "Could not find keywords file for manual %S."
                             plt-name)))))
       ;; The location is an unrecognized symbol, so just barf.
       (t (quack-internal-error))))
     ;; The location is something other than a string or symbol, so just barf.
     (t (quack-internal-error)))
    ;; We've populated all the variables for the location type, so return the
    ;; representation.
    (vector 'manual sym title loc kw-p start-url kw-base kw-file nil)))

(defun quack-doc-keyword-lookup (doc keyword)
  (let ((ht (or (quack-doc-get-kw-hashtable doc)
                (progn (quack-doc-import-keywords doc)
                       (quack-doc-get-kw-hashtable doc)))))
    (if ht
        (quack-gethash keyword ht nil)
      (quack-warning "No keywords for document \"%S\"."
                     (quack-doc-get-sym doc))
      nil)))

(defun quack-doc-import-keywords (doc)
  (if (eq (quack-doc-get-loc doc) 'plt)
      (quack-doc-import-plt-manual-keywords doc)
    (quack-internal-error)))

(defun quack-doc-import-plt-manual-keywords (doc)
  ;; Reads in the predetermined keywords file for PLT manual `doc' object,
  ;; populating the `kw-hashtable' field of the `doc' object.  The format of
  ;; each entry in the PLT keywords file is a list of 5 strings:
  ;;
  ;;     (KEYWORD SYNTAX FILE FRAGMENT SECTION)
  ;;
  ;; The hashtable is keyed on the KEYWORD string, for which the value is
  ;; usually a vector:
  ;;
  ;;     [SYNTAX FILE-CONST FRAGMENT]
  ;;
  ;; where FILE-CONST is the FILE string registered with the `quack-strconst'
  ;; to save memory on redundant strings.
  ;;
  ;; When more there is more than one entry for a given keyword, then the value
  ;; of the hashtable entry for that keyword is a list of vectors, in the order
  ;; in which they were derived from the original keywords file.
  ;;
  ;; These duplicate values may be duplicated or conflicting, as in:
  ;;
  ;;     (["(regexp-match pattern input-port [start-k end-k output-port])"
  ;;       "mzscheme-Z-H-10.html" "%_kw_definitionregexp-match"]
  ;;      ["(regexp-match pattern string [start-k end-k output-port])"
  ;;       "mzscheme-Z-H-10.html" "%_kw_definitionregexp-match"])
  ;;
  ;; No attempt is made here to weed out any duplicate/conflicting entries --
  ;; that behavior left up to the code that accesses the hashtable.  For the
  ;; example above, a command to display the syntax for the keyword would need
  ;; to display both values.  However, a command to view the documentation for
  ;; the keyword would need only to display one Web page without querying the
  ;; user, since both entries above point to the same page and fragment.
  (quack-activity
   (format "Importing keywords for manual %S" (quack-doc-get-sym doc))
   (let (sexp)
     (garbage-collect)
     (condition-case err
         (setq sexp (quack-read-sexp-file
                     (or (quack-doc-get-kw-file doc)
                         (quack-warning "Manual %S has no keywords file."
                                        (quack-doc-get-sym doc)))))
       (error (quack-warning "Problem importing keywords for manual %S: %s"
                             (quack-doc-get-sym doc) err)))
     (when sexp
       (garbage-collect)
       (let ((ht (quack-make-hash-table :test             'equal
                                        :size             (length sexp)
                                        :rehash-threshold 1.0)))
         ;; Note: We make the hashtable equal to the length of the read list of
         ;; keyword forms so that it will be at least large enough for all the
         ;; keywords without being excessively overlarge, and without having to
         ;; do resizes or a counting pass or intermediate representation.  The
         ;; hashtable will be a little larger than necessary when there are
         ;; multiple keyword forms for the same keyword.  In a test with
         ;; MzScheme 200.2, the hashtable used/size for "mzscheme" manual was
         ;; 489/502; for "mzlib", 245/257.
         (quack-doc-set-kw-hashtable doc ht)
         (mapcar (function
                  (lambda (raw-entry)
                    (let* ((kw  (nth 0 raw-entry))
                           (new (vector (nth 1 raw-entry)
                                        (quack-strconst (nth 2 raw-entry))
                                        (nth 3 raw-entry)))
                           (old (quack-gethash kw ht nil)))
                      (quack-puthash
                       kw
                       (cond ((not     old) new)
                             ((vectorp old) (list  old new))
                             ((listp   old) (nconc old (list new))))
                       ht))))
                 sexp))))))

(defun quack-read-sexp-file (filename)
  (save-excursion
    (let* ((buf (generate-new-buffer "*quack-read-sexp-file*")))
      (set-buffer buf)
      (unwind-protect
          (progn (insert-file-contents-literally filename)
                 (goto-char (point-min))
                 (read buf))
        ;; unwind-protect cleanup
        (kill-buffer buf)))))

;; Documentation Database:

(defvar quack-docs 'invalid)

(defun quack-docs ()
  (when (eq quack-docs 'invalid)
    (quack-docs-build))
  quack-docs)

(defun quack-docs-build ()
  (quack-activity
   "Building Quack docs database"
   (quack-invalidate-manuals-caches)
   (setq quack-docs (mapcar 'quack-manual-to-doc quack-manuals))))

(defun quack-docs-manual-lookup (sym)
  (let ((docs  (quack-docs))
        (found nil))
    (while (and docs (not found))
      (let ((doc (car docs)))
        (setq docs (cdr docs))
        (when (eq (quack-doc-get-sym doc) sym)
          (setq found doc))))
    found))

(defun quack-docs-manual-keyword-lookup (keyword)
  (let ((results '()))
    (mapcar (function
             (lambda (doc)
               (cond
                ((not (quack-doc-get-kw-p doc)) nil)
                ((not (quack-doc-get-kw-base-url doc))
                 (quack-warning "Manual %S has no HTML."
                                (quack-doc-get-sym doc)))
                (t (let ((match (quack-doc-keyword-lookup doc keyword)))
                     (cond
                      ((not match) nil)
                      ((vectorp match)
                       (setq results (cons (cons doc match) results)))
                      ((listp match)
                       (mapcar (function
                                (lambda (m)
                                  (setq results (cons (cons doc m) results))))
                               match))
                      (t (quack-internal-error))))))))
            (quack-docs))
    (reverse results)))

;; Keyword Lookup Match Object:

(defmacro quack-kwmatch-get-doc (o) `(car ,o))
(defmacro quack-kwmatch-get-kw  (o) `(cdr ,o))

(defun quack-kwmatch-url (kwmatch)
  (let ((doc (car kwmatch))
        (kw  (cdr kwmatch)))
    (concat (quack-doc-get-kw-base-url doc)
            (quack-quote-url-substring (quack-kw-get-file kw))
            "#"
            (quack-quote-url-substring (quack-kw-get-fragment kw) t))))

;; Manual Viewing:

(defun quack-view-manual (&optional sym)
  "View a manual."
  (interactive
   (list
    (let* ((completes (or (quack-manuals-completes)
                          (error
                           "Sorry, variable \"quack-manuals\" is empty.")))
           (default   "R5RS")
           (input     (let ((completion-ignore-case t))
                        (completing-read
                         (format "Quack Manual (default %S): " default)
                         completes nil t nil nil default))))
      (cdr (or (assoc input completes)
               (error "No manual %S." input))))))
  (quack-activity
   (format "Viewing manual \"%S\"" sym)
   (quack-browse-url (or (quack-doc-get-start-url
                          (or (quack-docs-manual-lookup sym)
                              (error "Manual \"%S\" not found." sym)))
                         (error "Don't know a URL for manual \"%S\"." sym)))))

(defvar quack-manuals-menu-cache      'invalid)
(defvar quack-manuals-completes-cache 'invalid)

(defun quack-invalidate-manuals-caches ()
  (setq quack-docs                    'invalid)
  (setq quack-manuals-completes-cache 'invalid)
  (setq quack-manuals-menu-cache      'invalid))

;;(quack-invalidate-manuals-caches)

;; This version maps completion strings to URLs.
;; (defun quack-manuals-completes ()
;;   (when (eq quack-manuals-completes-cache 'invalid)
;;     (let ((completes '()))
;;       (mapcar (function
;;                (lambda (doc)
;;                  (let ((sym (quack-doc-get-sym doc))
;;                        (url (quack-doc-get-start-url doc)))
;;                    (setq completes
;;                          (cons (cons (quack-doc-get-title doc) url)
;;                                (cons (cons (symbol-name sym) url)
;;                                      completes))))))
;;               (quack-docs))
;;       (setq quack-manuals-completes-cache (reverse completes))))
;;   quack-manuals-completes-cache)

(defun quack-manuals-completes ()
  (when (eq quack-manuals-completes-cache 'invalid)
    (let ((completes '()))
      (mapcar (function
               (lambda (doc)
                 (let ((sym (quack-doc-get-sym doc))
                       ;;(url (quack-doc-get-start-url doc))
                       )
                   (setq completes
                         (cons (cons (quack-doc-get-title doc) sym)
                               ;;(cons (cons (symbol-name sym) sym)
                                     completes
                                     ;;)
                                     )))))
              (quack-docs))
      (setq quack-manuals-completes-cache (reverse completes))))
  quack-manuals-completes-cache)

(defun quack-manuals-menu ()
  (when (eq quack-manuals-menu-cache 'invalid)
    (setq quack-manuals-menu-cache
          (mapcar (function
                   (lambda (manual)
                     (let ((sym   (nth 0 manual))
                           (title (nth 1 manual)))
                       `[,title (quack-view-manual (quote ,sym))])))
                  quack-manuals)))
  quack-manuals-menu-cache)

(defun quack-manuals-webjump-sites ()
  "Returns `webjump' entries for manuals in `quack-manuals'.

Can be used in your `~/.emacs' file something like this:

    (require 'quack)
    (require 'webjump)
    (require 'webjump-plus)
    (setq webjump-sites
          (append my-own-manually-maintained-webjump-sites
                  (quack-manuals-webjump-sites)
                  webjump-plus-sites
                  webjump-sample-sites))"
  ;; TODO: Note what they should do if they are adding to plt collectsion dirs
  ;;       via custom settings but quack-manuals-webjump-sites is getting
  ;;       called before then.
  (let ((result                 '())
        (quack-quiet-warnings-p t))
    (mapcar (function
             (lambda (doc)
               (let ((url (quack-doc-get-start-url doc)))
                 (when url
                   (setq result (cons (cons (quack-doc-get-title doc) url)
                                      result))))))
            (quack-docs))
    result))

;; Keyword Docs Viewing:

;; TODO: Add doc lookup in PLT "doc.txt" files.  A little tricky.  Maybe make
;;       sure doc.txt is a long-term format first.

(defun quack-view-keyword-docs (keyword)
  ;; TODO: Don't prompt if all choices would result in the same URL.
  (interactive (list (quack-prompt-for-keyword "View docs for keyword")))
  (when (and keyword (stringp keyword) (not (string= keyword "")))
    (let ((matches (quack-docs-manual-keyword-lookup keyword)))
      (if (not matches)
          (message "Sorry, no documentation found for keyword %S." keyword)
        (quack-browse-url
         (quack-kwmatch-url
          (if (cdr matches)
              (quack-prompt-for-kwmatch-choice "Which" matches)
            (car matches))))))))

(defun quack-keyword-at-point ()
  ;; TODO: Make sure this reads all Scheme symbols -- it may currently only
  ;;       read valid Elisp symbols.
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    ;; In some cases (point at beginning of empty buffer?), `bounds' will be
    ;; the bounds of an empty string, so check this.
    (when bounds
      (let ((beg (car bounds))
            (end (cdr bounds)))
        (when (/= beg end)
          (buffer-substring-no-properties beg end))))))

(defun quack-prompt-for-keyword (prompt)
  (let* ((default (quack-keyword-at-point))
         (history (list default)))
    (read-string (if default
                     (format "%s (default %S): " prompt default)
                   (concat prompt ": "))
                 nil
                 ;; Note: Gratuitous reference to `history' eliminates warning
                 ;;       from XEmacs 21 byte-compiler.
                 (if (and default history) 'history nil)
                 default)))

(defun quack-prompt-for-kwmatch-choice (prompt kwmatch-list)
  (let ((completes '()))
    ;; Build the completion alist, ensure each key is unique.
    (mapcar
     (function
      (lambda (kwmatch)
        (let* ((kw         (quack-kwmatch-get-kw kwmatch))
               (orig-name  (or (quack-kw-get-syntax kw)
                               (progn (quack-warning "No keyword syntax: %s"
                                                     kw)
                                      "???")))
               (name       orig-name)
               (name-tries 1))
          ;; Ensure the name is unique within the completion list thus far.
          (while (assoc name completes)
            (setq name-tries (1+ name-tries))
            (setq name (format "%s #%d" orig-name name-tries)))
          ;; Prepend to the completion list (we'll reverse the list later).
          (setq completes (cons (cons name kwmatch) completes)))))
     kwmatch-list)
    (setq completes (reverse completes))
    ;; Prompt user and return selection.
    (let* ((default (car (car completes)))
           (read    (let ((completion-ignore-case t))
                      (completing-read
                       (format "%s (default %S): " prompt default)
                       completes nil t nil nil default))))
      (cdr (assoc read completes)))))

;; Inferior Process:

(defvar quack-run-scheme-prompt-history '())

(defun quack-remember-program-maybe (program)
  (when (and quack-remember-new-programs-p
             (not (member program quack-programs)))
    (quack-option-set 'quack-programs (cons program quack-programs) t)
    (message "Remembering program %S." program)))

(defun quack-run-scheme-prompt ()
  (let* ((last    (car quack-run-scheme-prompt-history))
         (default (or (and quack-run-scheme-prompt-defaults-to-last-p
                           last)
                      quack-default-program
                      scheme-program-name
                      last
                      "mzscheme"))
         (program (let ((minibuffer-allow-text-properties nil))
                    (completing-read
                     (concat "Run Scheme"
                             (if default
                                 (format " (default %S)" default)
                               "")
                             ": ")
                     (quack-run-scheme-prompt-completion-collection)
                     nil nil nil
                     'quack-run-scheme-prompt-history
                     default))))
    (quack-remember-program-maybe program)
    program))

(defun quack-run-scheme-prompt-completion-collection ()
  (let ((program-list quack-programs))
    (mapcar (function (lambda (program)
                        (and program
                             (not (member program program-list))
                             (setq program-list (cons program program-list)))))
            (list quack-default-program
                  scheme-program-name))
    (mapcar (function (lambda (program) (cons program nil)))
            program-list)))

(defadvice run-scheme (around quack-ad-run first nil activate)
  "Adds prompting for which Scheme interpreter program to run."
  ;; We don't want to prompt if there's already a Scheme running, but it's
  ;; possible for process to die between the comint check in `interactive' form
  ;; of this advice and the comint check in the `run-scheme' function.  We
  ;; should override `run-scheme' altogether, but for now let's only call the
  ;; original in the case that we do not detect a running Scheme.
  (interactive (list (cond ((comint-check-proc "*scheme*") nil)
                           ((or current-prefix-arg
                                quack-run-scheme-always-prompts-p)
                            (quack-run-scheme-prompt))
                           (t quack-default-program))))
  (if cmd
      ;; We will assume there is no running Scheme, so...  Since `run-scheme'
      ;; calls `pop-to-buffer' rather than `switch-to-scheme', our options for
      ;; Scheme process window management, such as putting the process buffer
      ;; window in its own frame, do not take effect when the process buffer is
      ;; displayed by `run-scheme'.  So, unless we are using the `cmuscheme'
      ;; window management behavior, we attempt to undo whatever window changes
      ;; and buffer changes `run-scheme' makes, then just call
      ;; `switch-to-scheme'.  (This code will be revisited once we decide how
      ;; to handle multiple Schemes, if not before then.)
      (let ((buf (current-buffer))
            (wg  (current-window-configuration)))
        ad-do-it
        (unless (or (not quack-switch-to-scheme-method)
                    (eq quack-switch-to-scheme-method 'cmuscheme))
          (set-window-configuration wg)
          (set-buffer buf)
          (switch-to-scheme t))
        (message "Started Scheme: %s" scheme-program-name))
    ;; There is a running Scheme, so don't call the `run-scheme' function at
    ;; all -- just call `switch-to-scheme' or duplicate the `cmuscheme'
    ;; package's `pop-to-buffer' behavior.
    (if (or (not quack-switch-to-scheme-method)
            (eq quack-switch-to-scheme-method 'cmuscheme))
        (pop-to-buffer "*scheme*")
      (switch-to-scheme t))
    (message "Switched to running Scheme: %s" scheme-program-name)))

(defadvice scheme-interactively-start-process (around
                                               quack-ad-sisp 
                                               first
                                               (&optional cmd)
                                               activate)
  ;; (save-window-excursion
  (call-interactively 'run-scheme)
  ;; )
  )

(defadvice scheme-proc (around quack-ad-scheme-proc first nil activate)
  (condition-case nil
      ad-do-it
    (error (message "Oops, we must start a Scheme process!")
           (call-interactively 'run-scheme)
           (setq ad-return-value (scheme-proc)))))

;; Switch-to-Scheme:

(defun quack-force-frame-switch-to-window (win)
  (let ((frame (window-frame win)))
    (unless (eq frame (selected-frame))
      (and window-system
           quack-warp-pointer-to-frame-p
           (set-mouse-position frame 0 0))
      (select-frame frame))
    (select-window win)))

(defadvice switch-to-scheme (before quack-ad-switch last nil activate)
  "Adds support for the `quack-switch-to-scheme-method' option."
  ;; This can be done as before-advice since the `pop-to-buffer' that
  ;; `switch-to-scheme' is using appears to always be a no-op when the target
  ;; buffer is already the current buffer.
  (require 'cmuscheme)
  ;; The `eval' below is to avoid problems with the byte-compiler and advising.
  ;; It doesn't seem to like: (and (boundp 'SYM) SYM)
  (let ((repl-buf (eval '(and (boundp 'scheme-buffer)
                              scheme-buffer
                              (get-buffer scheme-buffer)))))
    (cond ((not repl-buf)
           (error (concat "No process current buffer."
                          " Set `scheme-buffer' or execute `run-scheme'")))

          ((or (not quack-switch-to-scheme-method)
               (eq quack-switch-to-scheme-method 'cmuscheme))
           nil)

          ((eq (current-buffer) repl-buf) nil)

          ((eq quack-switch-to-scheme-method 'other-window)
           (switch-to-buffer-other-window repl-buf))

          ;; The following code may be revived if anyone reports problems with
          ;; the use of `special-display-popup-frame'.
          ;;
          ;; ((eq quack-switch-to-scheme-method 'own-frame)
          ;;  (let ((pop-up-frames                t)
          ;;        (same-window-buffer-names     nil)
          ;;        (same-window-regexps          nil)
          ;;        (special-display-buffer-names nil)
          ;;        (special-display-regexps      nil))
          ;;    (switch-to-buffer (pop-to-buffer repl-buf))))

          ((eq quack-switch-to-scheme-method 'own-frame)
           (quack-force-frame-switch-to-window
            (special-display-popup-frame repl-buf)))

          (t (error "Invalid quack-switch-to-scheme-method: %S"
                    quack-switch-to-scheme-method)))))

;; Customize:

(defun quack-customize ()
  "Customize the Quack package."
  (interactive)
  (customize-group 'quack))

;; Auto Modes:

(defun quack-add-auto-mode-alist (alist)
  (setq auto-mode-alist
        (append alist
                (let ((retained '()))
                  (mapcar (function (lambda (pair)
                                      (unless (assoc (car pair) alist)
                                        (setq retained (cons pair retained)))))
                          auto-mode-alist)
                  (reverse retained)))))

(quack-add-auto-mode-alist '(("\\.ccl\\'"    . scheme-mode)
                             ("\\.sch\\'"    . scheme-mode)
                             ("\\.scm\\'"    . scheme-mode)
                             ("\\.ss\\'"     . scheme-mode)
                             ("\\.stk\\'"    . scheme-mode)
                             ("\\.stklos\\'" . scheme-mode)
                             ;;
                             ("/\\.mzschemerc\\'" . scheme-mode)
                             ;; Non-Scheme:
                             ("\\.plt\\'"    . quack-pltfile-mode)))

;; Syntax Table:

(defmacro quack-str-syntax (str)
  `(,(if (and quack-gnuemacs-p (>= emacs-major-version 21))
         'string-to-syntax
       'quack-kludged-string-to-syntax)
    ,str))

(defun quack-kludged-string-to-syntax (str)
  (let* ((str-len (length str))
         (code    (aref str 0))
         (matches (if (> str-len 1) (aref str 1)))
         (result  (cond ((= code 32) 0)
                        ((= code ?_) 3)
                        (t (quack-internal-error))))
         (i       2))
    (while (< i str-len)
      (let ((c (aref str i)))
        (setq i (1+ i))
        (setq result (logior result
                             (lsh 1 (cond ((= c ?1) 16)
                                          ((= c ?2) 17)
                                          ((= c ?3) 18)
                                          ((= c ?4) 19)
                                          ((= c ?p) 20)
                                          ((= c ?b) 21)
                                          ((= c ?n) 21)
                                          (t (quack-internal-error))))))))
    (cons result (if (= matches 32) nil matches))))

;; Note: We are assuming that it is better to endeavor to fontify all "#|"
;;       block comments as nestable rather than as unnestable, regardless of
;;       whether or not a user's target Scheme dialect supports nested.

(defconst quack-pound-syntax-string (if quack-gnuemacs-p "_ p14bn" "_ p14b"))
;; (defconst quack-bar-syntax-string   (if quack-gnuemacs-p "  23bn"  "  23b"))
(defconst quack-bar-syntax-string   (if quack-gnuemacs-p "_ 23bn"  "_ 23b"))

(defconst quack-pound-syntax (quack-str-syntax quack-pound-syntax-string))
(defconst quack-bar-syntax   (quack-str-syntax quack-bar-syntax-string))

(modify-syntax-entry ?# quack-pound-syntax-string scheme-mode-syntax-table)
(modify-syntax-entry ?| quack-bar-syntax-string   scheme-mode-syntax-table)

;; Note: Unclear why, but `scheme.el' in GNU Emacs 21.2 is doing
;;       `(set-syntax-table scheme-mode-syntax-table)' in whatever buffer is
;;       active at the time the Elisp package is loaded.

;; Indent Properties:

(put 'begin0             'scheme-indent-function 1)
(put 'c-declare          'scheme-indent-function 0)
(put 'c-lambda           'scheme-indent-function 2)
(put 'case-lambda        'scheme-indent-function 0)
(put 'catch              'scheme-indent-function 1)
(put 'chicken-setup      'scheme-indent-function 1)
(put 'class              'scheme-indent-function 'defun)
(put 'class*             'scheme-indent-function 'defun)
(put 'compound-unit/sig  'scheme-indent-function 0)
(put 'dynamic-wind       'scheme-indent-function 0)
(put 'for/fold           'scheme-indent-function 2)
(put 'instantiate        'scheme-indent-function 2)
(put 'interface          'scheme-indent-function 1)
(put 'lambda/kw          'scheme-indent-function 1)
(put 'let*-values        'scheme-indent-function 1)
(put 'let+               'scheme-indent-function 1)
(put 'let-values         'scheme-indent-function 1)
(put 'let/ec             'scheme-indent-function 1)
(put 'mixin              'scheme-indent-function 2)
(put 'module             'scheme-indent-function 'defun)
(put 'opt-lambda         'scheme-indent-function 1)
(put 'parameterize       'scheme-indent-function 1)
(put 'parameterize-break 'scheme-indent-function 1)
(put 'parameterize*      'scheme-indent-function 1)
(put 'quasisyntax/loc    'scheme-indent-function 1)
(put 'receive            'scheme-indent-function 2)
(put 'send*              'scheme-indent-function 1)
(put 'sigaction          'scheme-indent-function 1)
(put 'syntax-case        'scheme-indent-function 2)
(put 'syntax/loc         'scheme-indent-function 1)
(put 'unit               'scheme-indent-function 'defun)
(put 'unit/sig           'scheme-indent-function 2)
(put 'unless             'scheme-indent-function 1)
(put 'when               'scheme-indent-function 1)
(put 'while              'scheme-indent-function 1)
(put 'with-handlers      'scheme-indent-function 1)
(put 'with-method        'scheme-indent-function 1)
(put 'with-syntax        'scheme-indent-function 1)

;; Keymaps:

(defvar quack-scheme-mode-keymap nil)

(setq quack-scheme-mode-keymap (make-sparse-keymap))

;; TODO: Maybe have an option to also map the Ctrl variants of each of these
;;       keys to their respective bindings.  As Eli pointed out, `C-c C-q C-x'
;;       is arguably easier to type than `C-c C-q x'.  Actually, though, I
;;       don't like the `C-c C-q' prefix at all -- it signifies everything that
;;       is wrong with traditional modifier-happy Emacs keybindings.  Maybe we
;;       should encourage users to set the prefix to some other key, like an
;;       unmodified function key.

(define-key quack-scheme-mode-keymap "f" 'quack-find-file)
(define-key quack-scheme-mode-keymap "k" 'quack-view-keyword-docs)
(define-key quack-scheme-mode-keymap "m" 'quack-view-manual)
(define-key quack-scheme-mode-keymap "r" 'run-scheme)
(define-key quack-scheme-mode-keymap "s" 'quack-view-srfi)
(define-key quack-scheme-mode-keymap "l" 'quack-toggle-lambda)
(define-key quack-scheme-mode-keymap "t" 'quack-tidy-buffer)

;; Menus:

(defmacro quack-bool-menuitem (title var &rest rest)
  (unless (stringp title) (quack-internal-error))
  (unless (symbolp var)   (quack-internal-error))
  `[,title (quack-option-toggle (quote ,var)) :style toggle :selected ,var
           ,@rest])

(defmacro quack-radio-menuitems (var alist)
  (unless (symbolp var)   (quack-internal-error))
  (unless (listp   alist) (quack-internal-error))
  `(quote ,(mapcar
            (function (lambda (pair)
                        (let ((title (car pair))
                              (value (cdr pair)))
                          (unless (stringp title) (quack-internal-error))
                          (unless (symbolp value) (quack-internal-error))
                          `[,title
                            (quack-option-set (quote ,var) (quote ,value))
                            :style    radio
                            :selected (eq ,var (quote ,value))])))
            alist)))

(defconst quack-browser-radio-alist
  '((nil                                . "(Browse-URL Default)")
    (browse-url-galeon                  . "Galeon")
    (browse-url-mozilla                 . "Mozilla")
    (browse-url-kde                     . "KDE Konqueror")
    (browse-url-netscape                . "Netscape Navigator")
    (browse-url-w3                      . "Emacs W3")
    (w3m-browse-url                     . "W3M")
    (quack-w3m-browse-url-other-window  . "W3M (in other window)")
    (browse-url-lynx-xterm              . "Lynx in Xterm")
    (browse-url-lynx-emacs              . "Lynx in Emacs")
    (browse-url-default-windows-browser . "MS Windows Default")))

(defconst quack-global-menuspec
  `("Quack"
    ["About Quack..." quack-about]
    ("Options"
     ("Startup Options"
      "These settings take full effect"
      "once Emacs is restarted."
      "---"
      ,(quack-bool-menuitem "Put Quack on Global Menu Bar" quack-global-menu-p)
      ,(quack-bool-menuitem "Remap Find-File Bindings"
                            quack-remap-find-file-bindings-p)
      "---"
      ["Quack Directory..." (customize-option 'quack-dir)]
      ["Quack Scheme Mode Keymap Prefix..."
       (customize-option 'quack-scheme-mode-keymap-prefix)])
     "---"
     ("Default Program" :filter quack-defaultprogram-menufilter)
     ,(quack-bool-menuitem "Always Prompt for Program"
                           quack-run-scheme-always-prompts-p)
     ,(quack-bool-menuitem "Program Prompt Defaults to Last"
                           quack-run-scheme-prompt-defaults-to-last-p)
     ,(quack-bool-menuitem "Remember New Programs"
                           quack-remember-new-programs-p)
     "---"
     ("Newline Behavior"
      ,@(quack-radio-menuitems
         quack-newline-behavior
         (("Newline"               . newline)
          ("Newline-Indent"        . newline-indent)
          ("Indent-Newline-Indent" . indent-newline-indent))))
     ,(quack-bool-menuitem "Smart Open-Paren"
                           quack-smart-open-paren-p)
     ("Switch-to-Scheme Method"
      ,@(quack-radio-menuitems quack-switch-to-scheme-method
                               (("Other Window"       . other-window)
                                ("Own Frame"          . own-frame)
                                ("Cmuscheme Behavior" . cmuscheme)))
      "---"
      ,(quack-bool-menuitem
        "Warp Pointer to Frame"
        quack-warp-pointer-to-frame-p
        :active (eq quack-switch-to-scheme-method 'own-frame)))
     ("Fontification"
      ,@(quack-radio-menuitems quack-fontify-style
                               (("PLT Style"                . plt)
                                ("Extended GNU Emacs Style" . emacs)
                                ("Emacs Default"            . nil)))
      "---"
      ,(quack-bool-menuitem "Pretty Lambda \(in PLT Style\)"
                            quack-pretty-lambda-p
                            :active (and quack-pretty-lambda-supported-p
                                         (memq quack-fontify-style '(plt))))
      ,(quack-bool-menuitem "Fontify Definition Names \(in PLT Style\)"
                            quack-pltish-fontify-definition-names-p
                            :active (eq quack-fontify-style 'plt))
      ,(quack-bool-menuitem "Fontify Syntax Keywords \(in PLT Style\)"
                            quack-pltish-fontify-keywords-p
                            :active (eq quack-fontify-style 'plt))
      ;; TODO: Add menuitem here for "Fontify #: Keywords \(in PLT Style\)"
      ,(quack-bool-menuitem "Fontify 3-Semicolon Comments \(in PLT Style\)"
                            quack-fontify-threesemi-p
                            :active (memq quack-fontify-style '(plt)))
      )
     ("Web Browser"
      ,@(mapcar (function
                 (lambda (n)
                   (let ((func  (car n))
                         (title (cdr n)))
                     `[,title
                       (quack-option-set 'quack-browse-url-browser-function
                                         (quote ,func))
                       :style radio
                       :selected ,(if (not func)
                                      '(not quack-browse-url-browser-function)
                                    `(eq quack-browse-url-browser-function
                                         (quote ,func)))])))
                quack-browser-radio-alist)
      ["(Other)..."
       (customize-option 'quack-browse-url-browser-function)
       :style    radio
       :selected (not (assq quack-browse-url-browser-function
                            quack-browser-radio-alist))])
     ,(quack-bool-menuitem "Tab Characters are Evil" quack-tabs-are-evil-p)
     ("Local Keywords for Remote Manuals"
      ,@(quack-radio-menuitems
         quack-local-keywords-for-remote-manuals-p
         (("Permit" . t)
          ("Forbid" . nil)
          ("Always" . always))))
     ["PLT Collection Directories..."
      (customize-option 'quack-pltcollect-dirs)]
     "---"
     ["Customize..." quack-customize])
    "---"
    ["Run Scheme"              run-scheme]
    ["Switch to Scheme Buffer" switch-to-scheme]
    "---"
    ("View Manual" :filter quack-view-manual-menufilter)
    ("View SRFI"   :filter quack-view-srfi-menufilter)
    ["View Keyword Docs..."       quack-view-keyword-docs]
    ["Dired on PLT Collection..." quack-dired-pltcollect]))

(defun quack-install-global-menu ()
  (when quack-global-menu-p
    (quack-when-gnuemacs
     (unless (assq 'Quack menu-bar-final-items)
       (setq menu-bar-final-items (cons 'Quack menu-bar-final-items)))
     (easy-menu-define quack-global-menu global-map ""
       quack-global-menuspec))
    (quack-when-xemacs
     ;; Die! Die! Die!
     ;;(mapcar (function (lambda (n)
     ;;(delete-menu-item '("Quack") n)
     ;;(add-submenu nil quack-global-menuspec "Help" n)))
     ;;(list 
     ;;;;current-menubar 
     ;;default-menubar
     ;;))
     (delete-menu-item '("Quack") current-menubar)
     (add-submenu nil quack-global-menuspec "Help" current-menubar)
     (set-menubar-dirty-flag))))

;; TODO: We should make sure the user's custom settings have been loaded
;; before we do this.
(quack-install-global-menu)

;; And die some more!
;;(quack-when-xemacs (add-hook 'after-init-hook 'quack-install-global-menu))

(defconst quack-scheme-mode-menuspec
  `("Scheme"
    ("Quack Global" ,@(cdr quack-global-menuspec))
    "---"
    ["Toggle Lambda Syntax"          quack-toggle-lambda]
    ["Tidy Buffer Formatting"        quack-tidy-buffer]
    ["Comment-Out Region"            comment-region]
    ["Un-Comment-Out Region"         quack-uncomment-region]
    "---"
    ["Evaluate Last S-expression"    scheme-send-last-sexp]
    ["Evaluate Region"               scheme-send-region]
    ["Evaluate Region & Go"          scheme-send-region-and-go]
    ["Evaluate Last Definition"      scheme-send-definition]
    ["Evaluate Last Definition & Go" scheme-send-definition-and-go]
    ["Compile Definition"            scheme-compile-definition]
    ["Compile Definition & Go"       scheme-compile-definition-and-go]
    ["Load Scheme File"              scheme-load-file]
    ["Compile Scheme File"           scheme-compile-file]
    "---"
    ["View Keyword Docs..."          quack-view-keyword-docs]
    ["Quack Find File"               quack-find-file]))

(defvar quack-scheme-mode-menu)
(quack-when-gnuemacs
 (let ((map (make-sparse-keymap)))
   (setq quack-scheme-mode-menu nil)
   (easy-menu-define quack-scheme-mode-menu map ""
     quack-scheme-mode-menuspec)
   (define-key scheme-mode-map [menu-bar scheme]
     (cons "Scheme"
           (or (lookup-key map [menu-bar Scheme])
               (lookup-key map [menu-bar scheme]))))))

(defun quack-view-manual-menufilter (arg)
  (quack-menufilter-return "quack-view-manual-menufilter-menu"
                           (quack-manuals-menu)))

(defun quack-view-srfi-menufilter (arg)
  (quack-menufilter-return
   "quack-view-srfi-menufilter-menu"
   (condition-case nil
       (quack-srfi-menu t)
     ;; TODO: Move the generation of this fallback menu down to
     ;;       quack-srfi-menu.
     (error '(["Update SRFI Index" quack-update-srfi-index]
              "---"
              ("Draft"     :active nil "")
              ("Final"     :active nil "")
              ("Withdrawn" :active nil "")
              ["Other SRFI..."     quack-view-srfi])))))

(defun quack-defaultprogram-menufilter (arg)
  (quack-menufilter-return
   "quack-defaultprogram-menufilter-menu"
   `(,@(quack-optionmenu-items-setdefaultprogram)
       "---"
       ["Other Program..." quack-set-other-default-program]
       "---"
       ("Forget Program"
        ,@(mapcar
           (function
            (lambda (program)
              `[,(format "Forget  %s" program)
                (quack-forget-program ,program)]))
           quack-programs)))))

(defun quack-optionmenu-items-setdefaultprogram ()
  (let* ((programs      (quack-sort-string-list-copy quack-programs))
         (add-default-p (and quack-default-program
                             (not (member quack-default-program programs)))))
    (and add-default-p
         (setq programs (cons quack-default-program programs)))
    (mapcar
     (function
      (lambda (program)
        (let* ((selected-p (and quack-default-program
                                (equal program quack-default-program))))
          `[,(format "%s%s"
                     program
                     (if (and add-default-p
                              (equal program quack-default-program))
                         " (temporary)"
                       ""))
            (quack-option-set 'quack-default-program ,program)
            :style radio :selected ,selected-p])))
     programs)))

(mapcar (function (lambda (sym) (put sym 'menu-enable 'mark-active)))
        '(comment-region
          indent-region
          quack-uncomment-region
          scheme-send-region
          scheme-send-region-and-go))

;; Option Menu Callbacks:

(defun quack-set-other-default-program ()
  (interactive)
  (let* ((minibuffer-allow-text-properties nil)
         (program (quack-without-side-whitespace
                   (read-string "Other Default Program: "))))
    (if (string= program "")
        (message "Default program unchanged.")
      (quack-remember-program-maybe program)
      (quack-option-set 'quack-default-program
                        program))))

(defun quack-forget-program (program)
  (setq quack-programs (delete program quack-programs))
  (quack-option-set 'quack-programs quack-programs t)
  (message "Forgot program %S." program))

(defun quack-custom-set (sym value)
  ;; Clean up the value based on the variable symbol.
  (cond ((eq sym 'quack-programs)
         (setq value (quack-sort-string-list-copy value))))

  ;; Set default binding.  Set local binding just for the halibut, although if
  ;; there are local bindings, then other things will likely break.  \(We used
  ;; to have a check here, but removed it while porting to XEmacs.\)
  (set         sym value)
  (set-default sym value)

  ;; TODO: Probably don't do this during Emacs initialization time, to avoid
  ;;       unnecessary behavior like:
  ;;
  ;;           Loading ~/emacs/my-custom.el (source)...
  ;;           Updating Scheme Mode buffers...done
  ;;           Updating Scheme Mode buffers...done
  ;;           Updating Scheme Mode buffers...done
  ;;           Updating Scheme Mode buffers...done
  ;;           Updating Scheme Mode buffers...done
  ;;           Loading ~/emacs/my-custom.el (source)...done
  
  ;; Update dependent program state.
  (cond ((memq sym '(quack-emacsish-keywords-to-fontify
                     quack-fontify-style
                     quack-fontify-threesemi-p
                     quack-pltish-fontify-definition-names-p
                     quack-pltish-fontify-keywords-p
                     quack-pltish-keywords-to-fontify
                     quack-pretty-lambda-p))
         (quack-update-scheme-mode-buffers))

        ((eq sym 'quack-local-keywords-for-remote-manuals-p)
         (quack-invalidate-manuals-caches))

        ((eq sym 'quack-pltcollect-dirs)
         (quack-invalidate-pltcollects-caches))))

(defun quack-option-set (sym value &optional silently)
  (if quack-options-persist-p
      (customize-save-variable sym value)
    (quack-custom-set sym value))
  (or silently
      (message "Set %s%s to: %S"
               sym
               (if quack-options-persist-p "" " (non-persistently)")
               value)))

(defun quack-option-toggle (sym &optional silently)
  (quack-option-set sym (not (symbol-value sym)) t)
  (or silently
      (message "Set %s%s %s."
               sym
               (if quack-options-persist-p "" " (non-persistently)")
               (if (symbol-value sym) "ON" "OFF"))))

(defun quack-update-scheme-mode-buffers ()
  (save-excursion
    (quack-activity
     "Updating Scheme Mode buffers"
     (mapcar (function
              (lambda (buf)
                (set-buffer buf)
                (when (eq major-mode 'scheme-mode)
                  (quack-activity (format "Updating buffer %S" (buffer-name))
                                  (scheme-mode)))))
             (buffer-list)))))

;; Pretty Lambda:

(defconst quack-lambda-char (make-char 'greek-iso8859-7 107))

(defconst quack-pretty-lambda-supported-p
  (and quack-gnuemacs-p (>= emacs-major-version 21)))

;; Font Lock:

(defconst quack-emacsish1-font-lock-keywords
  `((,(concat "[[(]"
              "\\("                     ; #<1
              "define\\*?"
                                        ; #=2 #=3
              (quack-re-alt (quack-re-alt ""
                                          "-generic"
                                          "-generic-procedure"
                                          "-method"
                                          "-public"
                                          "/kw"
                                          "/override"
                                          "/private"
                                          "/public")
                                        ; #=4
                            (quack-re-alt "-macro"
                                          "-syntax")
                            "-class"
                            "-module"
                            "-signature"
                            "-struct")
              "\\)"                     ; #>1
              "\\>"
              "[ \t]*[[(]?"
                                        ; #=5
              "\\(\\sw+\\)?")
     (1 font-lock-keyword-face)
     (5 (cond ((match-beginning 3) font-lock-function-name-face)
              ((match-beginning 4) font-lock-variable-name-face)
              (t                   font-lock-type-face))
        nil t))

    ;; PLT module definitions.
    ("[[(]\\(module\\)\\>[ \t]+\\(\\sw+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil t))))

(defconst quack-emacsish2-font-lock-keywords
  (append quack-emacsish1-font-lock-keywords
          `(
            ;; Misc. keywords.
            (,(concat
               "[[(]\\("
               (regexp-opt quack-emacsish-keywords-to-fontify)
               "\\)\\>")
             . 1)
            ;; Class specifiers in SOS, Stklos, Goops.
            ("\\<<\\sw+>\\>" . font-lock-type-face)
            ;; Colon keywords.
            ("\\<:\\sw+\\>" . font-lock-builtin-face))))

(defvar quack-pltish-font-lock-keywords nil)

(defun quack-pltish-num-re (radix digit base16-p)
  ;; These regexps started as a transliteration of the R5RS BNF to regular
  ;; expressions, adapted for PLTisms, and with a few optimizations.
  ;;
  ;; PLTisms are that 'e' is not permitted as an exponent marker in base-16
  ;; literals, and that "decimal-point" forms are permitted in any radix.
  ;;
  ;; There's obvious opportunity for further optimization, especially if we
  ;; relax the accepted syntax a little.  These regexps have not been tested
  ;; much, but, since this is only Emacs syntax fontification, false-positives
  ;; and false-negatives will be obvious yet benign.
  (let* ((uint    (concat digit "+#*"))
         (sign    "[-+]?")
         (suffix  (quack-re-optional (if base16-p "[sSfFdDlL]" "[eEsSfFdDlL]")
                                     sign
                                     "[0-9]+"))
         (decimal (quack-re-alt
                   (concat uint suffix)
                   (concat "\\." digit "+#*" suffix)
                   (concat digit
                           "+"
                           (quack-re-alt (concat "\\." digit "*")
                                         "#+\\.")
                           "#*")))
         (ureal   (quack-re-alt uint
                                (concat uint "/" uint)
                                decimal))
         (real    (concat sign ureal))
         (complex (quack-re-alt
                   (concat real
                           (quack-re-alt (concat "@" real)
                                         (quack-re-optional
                                          "[-+]"
                                          (quack-re-optional ureal)
                                          "i")
                                         ""))
                   (concat "[-+]" (quack-re-optional ureal) "i")))
         (exact   (quack-re-optional "#[eEiI]"))
         (prefix  (quack-re-alt (concat radix exact)
                                (concat exact radix))))
    (concat "\\<" prefix complex "\\>")))

(defconst quack-pltish-fls-base
  `(
    ("\\`\\(MrEd\\|Welcome to MzScheme\\) v[^\n]+" . quack-banner-face)
    ("\\`Gambit Version 4\\.0[^\n]*" . quack-banner-face)
    ("\\`Welcome to scsh [0-9][^\n]+\nType ,\\? for help[^\n]+"
     . quack-banner-face)
    ("\\`MIT/GNU Scheme running under [^\n]+" . quack-banner-face)
    ;;("\\`; This is the CHICKEN interpreter - Version [^\n]+\n; (c)[^\n]+"
    ;; . quack-banner-face)
    ;;("\\`Scheme Microcode Version[^\n]+\nMIT Scheme[^\n]+\n\\([^\n]+\n\\)+" .
    ;;quack-banner-face)
    ;; Unix cookie line.
    ("\\`#![^\r\n]*" . quack-pltish-comment-face)
    ;; Colon keywords:
    ("\\<#:\\sw+\\>" . quack-pltish-colon-keyword-face)
    ;; Self-evals:
    ("'\\sw+\\>"                . quack-pltish-selfeval-face)
    ("'|\\(\\sw\\| \\)+|"       . quack-pltish-selfeval-face)
    ;; Note: The first alternative in the following rule will misleadingly
    ;; fontify some invalid syntax, such as "#\(x".
    ("\\<#\\\\\\([][-`~!@#$%&*()_+=^{}\;:'\"<>,.?/|\\\\]\\|\\sw+\\>\\)"
     . quack-pltish-selfeval-face)
    ("[][()]"                   . quack-pltish-paren-face)
    ("\\<#\\(t\\|f\\)\\>"       . quack-pltish-selfeval-face)
    ("\\<+\\(inf.0\\|nan\\)\\>" . quack-pltish-selfeval-face)
    ("\\<-inf.0\\>"             . quack-pltish-selfeval-face)
    ,@(mapcar (function (lambda (args)
                          (cons (apply 'quack-pltish-num-re args)
                                'quack-pltish-selfeval-face)))
              '(("#b"        "[01]"        nil)
                ("#o"        "[0-7]"       nil)
                ("\\(#d\\)?" "[0-9]"       nil)
                ("#x"        "[0-9a-fA-F]" t)))))

(defconst quack-pltish-fls-defnames
  ;; TODO: Optimize these once they're fairly complete and correct.

  ;; TODO: Would be nice to fontify binding names everywhere they are
  ;;       introduced, such as in `let' and `lambda' forms.  That may require
  ;;       real parsing to do reasonably well -- the kludges get too bad and
  ;;       slow, and font-lock gets in the way more than it helps.

  `(
                                        ;,@quack-pltish-font-lock-keywords

    ;; Lots of definition forms that start with "define".
    (,(concat "[[(]"
              "define\\*?"
              ;; TODO: make this into regexp-opt
              (quack-re-alt ""
                            ":"
                            "-class"
                            "-class"
                            "-const-structure"
                            "-constant"
                            "-embedded"
                            "-entry-point"
                            "-external"
                            "-for-syntax"
                            "-foreign-record"
                            "-foreign-type"
                            "-foreign-variable"
                            "-generic"
                            "-generic-procedure"
                            "-inline"
                            "-location"
                            "-macro"
                            "-method"
                            "-opt"
                            "-parameters"
                            "-public"
                            "-reader-ctor"
                            "-record"
                            "-record-printer"
                            "-record-type"
                            "-signature"
                            "-struct"
                            "-structure"
                            "-syntax"
                            "-values"
                            "-values-for-syntax"
                            "/contract"
                            "/override"
                            "/private"
                            "/public")
              "\\>"
              "[ \t]*[[(]?"
              "\\(\\sw+\\)")
     (2 (let ((name (quack-match-string-no-properties 2)))
          (if (= (aref name (1- (length name))) ?%)
              quack-pltish-class-defn-face
            quack-pltish-defn-face))
        nil t))

    ;; `defmacro' and related SCM forms.
    (,(concat "[[(]def"
              (quack-re-alt (concat "macro"
                                    (quack-re-alt "" "-public"))
                            "syntax")
              "\\>[ \t]+\\(\\sw+\\)")
     3 quack-pltish-defn-face nil t)

    ;; `defmac' from SIOD.
    ("[[(]defmac[ \t]+[[(][ \t]*\\(\\sw+\\)"
     1 quack-pltish-defn-face nil t)

    ;; `defvar' and `defun' from SIOD.
    (,(concat "[[(]def"
              (quack-re-alt "un"
                            "var")
              "[ \t]+\\(\\sw+\\)")
     2 quack-pltish-defn-face nil t)

    ;; Guile and Chicken `define-module'.
    ("[[(]define-module\\>[ \t]+[[(][ \t]*\\(\\sw+\\([ \t]+\\sw+\\)*\\)"
     1 quack-pltish-module-defn-face nil t)

    ;; PLT `define-values', `define-syntaxes', and `define-syntax-set'.
    (,(concat "[[(]define-"
              (quack-re-alt "values" "syntax-set" "syntaxes")
              "\\>[ \t]+[[(][ \t]*\\(\\sw+\\([ \t]+\\sw+\\)*\\)")
     2 quack-pltish-defn-face nil t)

    ;; PLT `module'.
    ("[[(]module\\>[ \t]+\\(\\sw+\\)"
     1 quack-pltish-module-defn-face nil t)

    ;; Named `let'.  (Note: This is disabled because it's too incongruous.)
    ;;("[[(]let\\>[ \t]+\\(\\sw+\\)"
    ;; 1 quack-pltish-defn-face nil t)
    ))

;; TODO: Adding PLT-style (quasi)quoted list fontifying is obviously not doable
;;       with just regexps.  Probably requires either cloning
;;       `font-lock-default-fontify-region' just to get it to call our
;;       replacement syntactic pass fontification function, *or*
;;       before-advising `font-lock-fontify-keywords-region' to perform our
;;       syntactic pass when in scheme-mode, and around-advising
;;       `font-lock-fontify-syntactically-region' to not do anything for
;;       scheme-mode (or maybe setting `font-lock-keywords-only' to non-nil,
;;       unless that breaks something else).  Or just ditch font-lock.  See
;;       `font-lock-fontify-region-function' variable in font-lock specs.

;; (defconst quack-pltish-fls-keywords
;;   `((,(concat
;;        "[[(]\\("
;;        (regexp-opt quack-pltish-keywords-to-fontify)
;;        "\\)\\>")
;;      (1 quack-pltish-keyword-face))))

(defun quack-install-fontification ()

  (when (eq quack-fontify-style 'plt)
    (set (make-local-variable 'font-lock-comment-face)
         'quack-pltish-comment-face)
    (set (make-local-variable 'font-lock-string-face)
         'quack-pltish-selfeval-face))

  (let* ((sk  `(("\\(#\\)\\(|\\)"
                 (1 ,quack-pound-syntax)
                 (2 ,quack-bar-syntax))
                ("\\(|\\)\\(#\\)"
                 (1 ,quack-bar-syntax)
                 (2 ,quack-pound-syntax))))
         (pl  (if (and quack-pretty-lambda-supported-p quack-pretty-lambda-p)
                  '(("[[(]\\(case-\\|match-\\|opt-\\)?\\(lambda\\)\\>"
                     2
                     (progn (compose-region (match-beginning 2)
                                            (match-end       2)
                                            quack-lambda-char)
                            nil)))
                '()))
         (threesemi
          (if quack-fontify-threesemi-p
              `(
                (,(concat "^\\(\;\;\;\\)"
                          ;; TODO: Make this enforce space or newline after the
                          ;; three semicolons.
                          "\\("
                          "[ \t]*"
                          "\\("
                          "[^\r\n]*"
                          "\\)"
                          "\r?\n?\\)")
                 (1 quack-threesemi-semi-face prepend)
                 (2 quack-threesemi-text-face prepend)
                 ;;(4 quack-threesemi-h1-face   prepend)
                 ;;(5 quack-threesemi-h2-face   prepend)
                 )

                ;; Funcelit:
                ("^\;\;\; @\\(Package\\|section\\|unnumberedsec\\)[ \t]+\\([^\r\n]*\\)"
                 (2 quack-threesemi-h1-face prepend))
                ("^\;\;\; @subsection[ \t]+\\([^\r\n]*\\)"
                 (1 quack-threesemi-h2-face prepend))

                ;; semiscribble:
                ("^\;\;\; package +\"\\([^\r\n\"]*\\)\" *"
                 (1 quack-threesemi-h1-face prepend))
                ("^\;\;\; @section\\(?:\\[[^]]*\\]\\)?{\\([^\r\n]*\\)}"
                 (1 quack-threesemi-h1-face prepend))
                ("^\;\;\; @subsection\\(?:\\[[^]]*\\]\\)?{\\([^\r\n]*\\)}"
                 (1 quack-threesemi-h2-face prepend))
                
                
                )
            '()))
         (fld `(,(cond
                  ((eq quack-fontify-style 'plt)
                   (set (make-local-variable
                         'quack-pltish-font-lock-keywords)
                        `(,@quack-pltish-fls-base
                          ,@(if quack-pltish-fontify-definition-names-p
                                quack-pltish-fls-defnames
                              '())
                          ,@pl
                          ,@(if quack-pltish-fontify-keywords-p
                                ;; quack-pltish-fls-keywords
                                `((,(concat
                                     "[[(]\\("
                                     (regexp-opt
                                      quack-pltish-keywords-to-fontify)
                                     "\\)\\>")
                                   (1 quack-pltish-keyword-face)))
                              '())
                          ,@threesemi
                          ))
                   'quack-pltish-font-lock-keywords)
                  ((eq quack-fontify-style 'emacs)
                   ;; TODO: Do pretty-lambda here too.  But first get rid of
                   ;;       this font-lock style "degrees of general gaudiness"
                   ;;       and switch to separate options for each property of
                   ;;       fontification.
                   '(quack-emacsish1-font-lock-keywords
                     quack-emacsish1-font-lock-keywords
                     quack-emacsish2-font-lock-keywords))
                  (t (quack-internal-error)))
                nil
                t
                ((?! . "w") (?$ . "w") (?% . "w") (?& . "w") (?* . "w")
                 (?+ . "w") (?- . "w") (?. . "w") (?/ . "w") (?: . "w")
                 (?< . "w") (?= . "w") (?> . "w") (?? . "w") (?@ . "w")
                 (?^ . "w") (?_ . "w") (?~ . "w")
                 ,@(if (eq quack-fontify-style 'plt)
                       '((?# . "w"))
                     '()))
                ;; TODO: Using `beginning-of-defun' here could be very slow,
                ;;       say, when you have a large buffer that is wrapped in a
                ;;       `module' form.  Look into whether this is a problem.
                beginning-of-defun
                ,@(if t                 ; quack-gnuemacs-p
                      `((font-lock-mark-block-function . mark-defun)
                        (font-lock-syntactic-keywords  . ,sk))
                    '()))))

    ;; TODO: Figure out why `font-lock-syntactic-keywords' just doesn't work in
    ;;       XEmacs 21, even though the syntax text properties seem to get set.
    ;;       We have already beaten it like an egg-sucking dog.

    ;;(if quack-xemacs-p
    ;;(put 'scheme-mode 'font-lock-defaults fld)
    (set (make-local-variable 'font-lock-defaults) fld)
    ;;)

    ;;(when quack-xemacs-p
    ;;  (set (make-local-variable 'font-lock-syntactic-keywords)
    ;;       syntactic-keywords))
    ))

;; Scheme Mode Startup Hook:

(defun quack-locally-steal-key-bindings (old-func new-func)
  (mapcar (function (lambda (key)
                      (unless (and (vectorp key)
                                   (eq (aref key 0) 'menu-bar))
                        (local-set-key key new-func))))
          (where-is-internal old-func)))

(defun quack-shared-mode-hookfunc-stuff ()

  ;; Install the Quack keymap and menu items.
  (local-set-key quack-scheme-mode-keymap-prefix quack-scheme-mode-keymap)
  (quack-when-xemacs
   (when (featurep 'menubar)
     ;;(set-buffer-menubar current-menubar)
     ;; TODO: For XEmacs, we could have two versions of this menu -- the popup
     ;;       one would have the Global submenu, but the menubar one would have
     ;;       the Global submenu only if quack-global-menu-p were nil.
     (add-submenu nil quack-scheme-mode-menuspec)
     (set-menubar-dirty-flag)
     (setq mode-popup-menu quack-scheme-mode-menuspec)))

  ;; Bind the paren-matching keys.
  (local-set-key ")" 'quack-insert-closing-paren)
  (local-set-key "]" 'quack-insert-closing-bracket)

  (local-set-key "(" 'quack-insert-opening-paren)
  (local-set-key "[" 'quack-insert-opening-bracket)

  ;; Steal any find-file bindings.
  (when quack-remap-find-file-bindings-p
    (quack-locally-steal-key-bindings 'find-file     'quack-find-file)
    (quack-locally-steal-key-bindings 'ido-find-file 'quack-find-file))

  ;; Fight against tabs.
  (when quack-tabs-are-evil-p
    (setq indent-tabs-mode nil))

  ;; Remove character compositions, to get rid of any pretty-lambda.  (Note:
  ;; This is bad, if it turns out compositions are used for other purposes in
  ;; buffers that are edited with Scheme Mode.)
  (when quack-pretty-lambda-supported-p
    (eval '(decompose-region (point-min) (point-max))))

  ;; Install fontification
  (when quack-fontify-style
    (when (and (boundp 'font-lock-keywords)
               (symbol-value 'font-lock-keywords)
               (not (featurep 'noweb-mode)))
      ;; This warning is not given if the `noweb-mode' package is installed.
      (quack-warning "`font-lock-keywords' already set when hook ran."))
    (quack-install-fontification))

  ;; Die! Die! Die!
  (quack-when-xemacs
   (quack-install-global-menu)))

(defun quack-inferior-scheme-mode-hookfunc ()
  (quack-shared-mode-hookfunc-stuff))

(defun quack-scheme-mode-hookfunc ()
  (quack-shared-mode-hookfunc-stuff)

  ;; Bind Return/Enter key.
  (local-set-key "\r" 'quack-newline)

  ;; Install toolbar.
  ;;(unless quack-xemacs-p
  ;;(when (display-graphic-p)
  ;;(quack-install-tool-bar)))
  )

(add-hook 'scheme-mode-hook          'quack-scheme-mode-hookfunc)
(add-hook 'inferior-scheme-mode-hook 'quack-inferior-scheme-mode-hookfunc)

;; Compilation Mode:

;; TODO: Add compilation-directory-matcher support for "setup-plt:  in".

(defvar quack-saved-compilation-error-regexp-alist nil)

(defconst quack-compilation-error-regexp-alist-additions
  (let ((no-line (if quack-xemacs-p
                     (let ((m (make-marker))) (set-marker m 0) m)
                   'quack-compile-no-line-number)))
  `(

    ;; PLT MzScheme 4.1.4 "=== context ===" traceback when there is only file,
    ;; line, and column info, but potentially no following ":" and additional
    ;; info like procedure name.
    ("^\\([^:\n\" ]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)

    ;; PLT MzScheme 205 "setup-plt"
    ;;   load-handler: expected a `module' declaration for `bar-unit' in
    ;;   "/u/collects/bar/bar-unit.ss", but found something else
    (,(concat "load-handler: expected a `module' declaration for `[^']+' in "
              "\"\\([^:\n\"]+\\)\", but found something else")
     1 ,no-line)

    ;; PLT MzScheme 205 "setup-plt".
    ;;   setup-plt: Error during Compiling .zos for Foo Bar (/u/collects/fb)
    ("setup-plt: Error during Compiling .zos for [^\n]+ \(\\([^\n\)]+\\)\)"
     1 ,no-line)

    ;; PLT MzScheme 4.0.1 "setup-plt".
    ("setup-plt: +\\(?:WARNING: +\\)\\([^:\n]+\\)::"
     1 ,no-line)

    ;; PLT MzScheme 4.0.1 "setup-plt".
    ("setup-plt: +\\(?:WARNING: +\\)\\([^:\n ][^:\n]*\\):\\([0-9]+\\):\\([0-9]+\\)"
     1 2 3)

    ;; PLT MzScheme 4.0.1 "setup-plt":
    ("load-handler: expected a `module' declaration for `[^'\n]+' in #<path:\\([^>\n]+\\)>[^\n]+"
     1 ,no-line)

    ;; PLT Scheme 4.1.2 "default-load-handler" error without useful filename:
    ("default-load-handler: cannot open input-file: "
     nil ,no-line)

    )))

(defun quack-compile-no-line-number (filename column)
  (list (point-marker) filename 1 (and column (string-to-number column))))

(defun quack-install-compilation-mode-stuff ()
  (unless quack-saved-compilation-error-regexp-alist
    (setq quack-saved-compilation-error-regexp-alist 
          compilation-error-regexp-alist))
  (setq compilation-error-regexp-alist
        (append quack-compilation-error-regexp-alist-additions
                quack-saved-compilation-error-regexp-alist)))

(quack-install-compilation-mode-stuff)

;; Interpreter-mode-alist:

(defvar quack-saved-interpreter-mode-alist nil)

(defvar quack-interpreter-mode-alist-additions
  (mapcar (function (lambda (x)
                      (cons x 'scheme-mode)))
          '("bigloo"
            "csi"
            "gosh"
            "gsi"
            "guile"
            "kawa"
            "mit-scheme"
            "mred"
            "mred3m"
            "mredcgc"
            "mzscheme"
            "mzscheme3m"
            "mzschemecgc"
            "r5rs"
            "r6rs"
            "rs"
            "rs"
            "scheme"
            "scheme48"
            "scsh"
            "sisc"
            "stklos"
            "sxi")))

(defun quack-install-interpreter-mode-alist ()
  (unless quack-saved-interpreter-mode-alist
    (setq quack-saved-interpreter-mode-alist
          interpreter-mode-alist))
  (setq interpreter-mode-alist
        (append quack-interpreter-mode-alist-additions
                quack-saved-interpreter-mode-alist)))

(quack-install-interpreter-mode-alist)

;; PLT Package Mode:

;; TODO: Do some simple checking and summarize what directories and files are
;;       getting modified by this package.

;; TODO: Maybe don't worry about preserving the decompressed text verbatim in
;;       the buffer -- set markers and generate headings, and be able to
;;       construct valid package.

;; TODO: Command to install package from original file using "setup-plt".

;; TODO: Fontify Scheme code file contents.

(defvar quack-pltfile-mode-hook nil)

(defvar quack-hiding-ovlcat)
(put 'quack-hiding-ovlcat 'face       'default)
(put 'quack-hiding-ovlcat 'intangible t)
(put 'quack-hiding-ovlcat 'invisible  t)

(defvar quack-pltfile-mode-map (make-sparse-keymap))
(define-key quack-pltfile-mode-map "q" 'quack-pltfile-quit)
(define-key quack-pltfile-mode-map "r" 'quack-pltfile-raw)
(define-key quack-pltfile-mode-map " " 'scroll-up)

;; TODO: Make a menu map for pltfile-mode.

(defun quack-pltfile-mode ()
  (interactive)
  "Major mode for viewing PLT Scheme `.plt' package files.

\\{quack-pltfile-mode-map}

Provided by Quack: http://www.neilvandyke.org/quack/"
  (kill-all-local-variables)
  (put 'quack-pltfile-mode 'mode-class 'special)
  (setq major-mode 'quack-pltfile-mode)
  (setq mode-name "PLT Package")
  (use-local-map quack-pltfile-mode-map)
  ;; Note: Currently, the `font-lock' feature is always defined, since we
  ;; require it.
  (when (featurep 'font-lock)
    (setq font-lock-defaults nil))
  (buffer-disable-undo)
  (let ((saved-bmp (buffer-modified-p)))
    (quack-activity "Decoding PLT package" (quack-pltfile-decode-buffer))
    (setq buffer-read-only t)
    (set-buffer-modified-p saved-bmp))
  (quack-when-xemacs
   (make-variable-buffer-local 'write-contents-hooks))
  (add-hook 'write-contents-hooks 'quack-prevent-pltfile-write)
  (run-hooks 'quack-pltfile-mode-hook)
  (message "Decoded PLT package.  %s"
           (substitute-command-keys
            (concat "`\\[quack-pltfile-quit]' to quit"
                    ", `\\[quack-pltfile-raw]' for raw format."))))

(defun quack-prevent-pltfile-write ()
  (unless (yes-or-no-p
           "Write a decoded PLT package buffer?!  Are you *sure*?!")
    (error "Aborted write of decoded PLT package buffer.")))

(defun quack-pltfile-raw ()
  (interactive)
  (let ((auto-mode-alist '()))
    (setq buffer-read-only nil)
    (widen)
    (delete-region (point-min) (point-max))
    (fundamental-mode)
    (revert-buffer t t)))

(defun quack-pltfile-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun quack-skip-whitespace-to-nonblank-line-beginning ()
  (save-match-data
    (while (looking-at "[ \t\r\f]*\n")
      (goto-char (match-end 0)))))

(defun quack-pltfile-decode-buffer ()

  ;; MIME Base-64 decode.  (Note: an error is signaled if this fails.)
  (base64-decode-region (point-min) (point-max))

  ;; Gzip decompress.
  (let ((coding-system-for-write (if quack-xemacs-p 'binary 'raw-text-unix))
        (coding-system-for-read  (if quack-xemacs-p 'binary 'raw-text-unix))
        (inhibit-eol-conversion  t)
        status)
    (unless (= (setq status (call-process-region (point-min) (point-max)
                                                 "gzip" t t nil "-d")) 0)
      (error "Could not decompress PLT package: gzip process status %s"
             status)))

  ;; Move past the "PLT" cookie, and the two sexp forms.
  (goto-char (point-min))
  (unless (looking-at "PLT")
    (error "This does not appear to be a PLT package file."))
  (goto-char (match-end 0))
  (forward-list 2)
  (quack-skip-whitespace-to-nonblank-line-beginning)
  (quack-make-face-ovlext (point-min) (point) 'quack-pltfile-prologue-face)

  ;; Process the buffer contents.
  (let ((standard-input (current-buffer)))

    (while (not (eobp))
      (let ((step-beg (point)))
        ;; TODO: This read will fail if we just had whitespace at the end of
        ;;       the file, which it shouldn't, but maybe we should check, just
        ;;       in case.
        (let ((sym (read)))
          (unless (symbolp sym)
            (error "Expected a symbol, but saw: %S" sym))
          (cond

           ((eq sym 'dir)
            (forward-list)
            (quack-skip-whitespace-to-nonblank-line-beginning)
            (quack-make-face-ovlext step-beg
                                    (point)
                                    'quack-pltfile-dir-face))

           ((memq sym '(file file-replace))
            (forward-list)
            (let ((size (read)))
              (unless (and (integerp size) (>= size 0))
                (error "Expected a file size, but saw: %S" size))
              (unless (looking-at "[ \t\r\n\f]*\\*")
                (error "Expected a `*' after file size."))
              (goto-char (match-end 0))

              ;; Fontify the file header.
              (quack-make-face-ovlext step-beg
                                      (1- (point))
                                      'quack-pltfile-file-face)

              ;; Hide the file contents asterisk.
              (quack-make-hiding-ovlext (1- (point)) (point))

              ;; Set the coding region for the content.
              (let* ((content-beg (point))
                     (content-end (+ content-beg size))
                     (cs          (detect-coding-region content-beg
                                                        content-end)))
                (goto-char content-end)
                (when (listp cs)
                  (setq cs (car cs)))
                (unless (eq cs 'undecided)
                  (cond ((eq cs 'undecided-dos)  (setq cs 'raw-text-dos))
                        ((eq cs 'undecided-mac)  (setq cs 'raw-text-mac))
                        ((eq cs 'undecided-unix) (setq cs 'raw-text-unix)))
                  (decode-coding-region content-beg content-end cs))
                ;; TODO: XEmacs 21 `decode-coding-region' seems to lose the
                ;;       point position.  This is disconcerting, since the
                ;;       point semantics under coding system changes do not
                ;;       currently seem to be well-specified, so resetting the
                ;;       point here *might* not always be the right thing to
                ;;       do.  Verify.
                (quack-when-xemacs
                 (goto-char content-end)))))

           (t (error "Expected `dir', `file', or `file-replace', but saw: %S"
                     sym)))))))

  ;; Return point to top of buffer.
  (goto-char (point-min)))

;; The rest of this file except for the `provide' form is TODO comments.

;; TODO: Add tool bar support later.
;;
;; (defvar quack-toolbarimage-width  24)
;; (defvar quack-toolbarimage-height 24)
;;
;; (defun quack-create-image (&rest args)
;;   (if (and quack-gnuemacs-p (>= emacs-major-version 21))
;;       (apply 'create-image args)
;;     nil))
;;
;; (defun quack-make-toolbarimage (&rest lines)
;;   ;; TODO: We really should make an efficient function to print N spaces
;;   ;;       or to return a string of N spaces.  Or at least keep 1-2
;;   ;;       strings for the left and right padding here, which will
;;   ;;       usually be the same for the duration of this function.
;;   (quack-create-image
;;    (let* ((lines-count (length lines))
;;           (blank-line  (make-string quack-toolbarimage-width 32)))
;;      (and (> lines-count quack-toolbarimage-height) (quack-internal-error))
;;      (with-output-to-string
;;        (princ "/* XPM */\nstatic char *magick[] = {\n")
;;        ;;(princ "/* columns rows colors chars-per-pixel */\n")
;;        (princ (format "\"%d %d 5 1\",\n"
;;                       quack-toolbarimage-width quack-toolbarimage-height))
;;        (princ "\". c #f0f0f0\",\n")
;;        (princ "\"@ c #0f0f0f\",\n")
;;        (princ "\"g c #00b000\",\n")
;;        (princ "\"r c #d00000\",\n")
;;        (princ "\"  c None\",\n")
;;        ;;(princ "/* pixels */\n")
;;        (let ((line-num 0))
;;          (mapcar (function
;;                   (lambda (line)
;;                     (princ "\"")
;;                     (if line
;;                         (let* ((c (length line))
;;                                (l (/ (- quack-toolbarimage-width c) 2)))
;;                           (and (> c quack-toolbarimage-width)
;;                                (quack-internal-error))
;;                           (princ (make-string l 32))
;;                           (princ line)
;;                           (princ (make-string (- quack-toolbarimage-width
;;                                                  c l)
;;                                               32)))
;;                       (princ blank-line))
;;                     (if (< (setq line-num (1+ line-num))
;;                            quack-toolbarimage-height)
;;                         (princ "\",\n")
;;                       (princ "\"\n"))))
;;                  (let ((rows-before (/ (- quack-toolbarimage-width
;;                                           lines-count)
;;                                        2)))
;;                    `(,@(make-list rows-before nil)
;;                        ,@lines
;;                        ,@(make-list (- quack-toolbarimage-height
;;                                        lines-count rows-before)
;;                                     nil)))))
;;        (princ "};\n")))
;;    'xpm t))
;;
;; (defvar quack-tbi-evalbuf
;;   (quack-make-toolbarimage
;;    "@@@@@@@@@@          "
;;    "@........@@         "
;;    "@........@.@   ggg  "
;;    "@........@..@  ggg  "
;;    "@........@@@@@ ggg  "
;;    "@............@ ggg  "
;;    "@..@@........@ ggg  "
;;    "@...@@.......@ ggg  "
;;    "@....@@......@ ggg  "
;;    "@.....@@.....@ ggg  "
;;    "@....@@@@....@ ggg  "
;;    "@...@@..@@...@ ggg  "
;;    "@..@@....@@..@ ggg  "
;;    "@............@ ggg  "
;;    "@@@@@@@@@@@@@@ ggg  "
;;    "               ggg  "
;;    "             ggggggg"
;;    "              ggggg "
;;    "               ggg  "
;;    "                g   "))
;;
;; (defvar quack-tbi-adoc
;;   (quack-make-toolbarimage
;;    "@@@@@@@@@@    "
;;    "@........@@   "
;;    "@........@.@  "
;;    "@........@..@ "
;;    "@........@@@@@"
;;    "@...@@@......@"
;;    "@..@@@@@@....@"
;;    "@..@....@@...@"
;;    "@...@@@.@@...@"
;;    "@..@@@@@@@...@"
;;    "@..@@...@@...@"
;;    "@..@@..@@@...@"
;;    "@...@@@@.@@..@"
;;    "@............@"
;;    "@@@@@@@@@@@@@@"))
;;
;; (defvar quack-tbi-manual
;;   (quack-make-toolbarimage
;;    "@@@@@@@@@@    "
;;    "@........@@   "
;;    "@........@.@  "
;;    "@........@..@ "
;;    "@........@@@@@"
;;    "@............@"
;;    "@..@@.@.@@...@"
;;    "@..@@@@@@@@..@"
;;    "@..@@.@@.@@..@"
;;    "@..@@.@@.@@..@"
;;    "@..@@.@@.@@..@"
;;    "@..@@.@@.@@..@"
;;    "@..@@.@@.@@..@"
;;    "@............@"
;;    "@@@@@@@@@@@@@@"))
;;
;; (defvar quack-tbi-manuallookup
;;   (quack-make-toolbarimage
;;    "@@@@@@@@@@          "
;;    "@........@@         "
;;    "@........@.@        "
;;    "@........@..@       "
;;    "@........@@@@@      "
;;    "@............@      "
;;    "@..@@.@@@@@@@@@@    "
;;    "@...@@@........@@   "
;;    "@....@@........@.@  "
;;    "@.....@........@..@ "
;;    "@....@@........@@@@@"
;;    "@...@@@............@"
;;    "@..@@.@..@@.@.@@...@"
;;    "@.....@..@@@@@@@@..@"
;;    "@@@@@@@..@@.@@.@@..@"
;;    "      @..@@.@@.@@..@"
;;    "      @..@@.@@.@@..@"
;;    "      @..@@.@@.@@..@"
;;    "      @..@@.@@.@@..@"
;;    "      @............@"
;;    "      @@@@@@@@@@@@@@"))
;;
;; (defvar quack-tbi-stop
;;   (quack-make-toolbarimage
;;    "    @@@@@    "
;;    "  @@rrrrr@@  "
;;    " @rrrrrrrrr@ "
;;    " @rrrrrrrrr@ "
;;    "@rr@@rrr@@rr@"
;;    "@rrr@@r@@rrr@"
;;    "@rrrr@@@rrrr@"
;;    "@rrr@@r@@rrr@"
;;    "@rr@@rrr@@rr@"
;;    " @rrrrrrrrr@ "
;;    " @rrrrrrrrr@ "
;;    "  @@rrrrr@@  "
;;    "    @@@@@    "))
;;
;; (defun quack-install-tool-bar ()
;;   (require 'tool-bar)
;;   (let ((map (make-sparse-keymap)))
;;
;;     (quack-define-key-after map [quack-load-file]
;;                             `(menu-item "quack-evalbuffer" scheme-load-file
;;                                         :image ,quack-tbi-evalbuf
;;                                         :help  "Load File"))
;;
;;     (quack-define-key-after map [quack-alpha]
;;                             `(menu-item "quack-alpha" quack-alpha
;;                                         :image ,quack-tbi-adoc
;;                                         :help  "alpha"))
;;
;;     (quack-define-key-after map [quack-manual]
;;                             `(menu-item "quack-manual" quack-manual
;;                                         :image ,quack-tbi-manual
;;                                         :help  "View Manual"))
;;
;;     (quack-define-key-after map [quack-view-keyword-docs]
;;                             `(menu-item "quack-view-keyword-docs"
;;                                         quack-view-keyword-docs
;;                                         :image ,quack-tbi-manuallookup
;;                                         :help  "View Keyword Docs"))
;;
;;     (quack-define-key-after map [quack-stop]
;;                             `(menu-item "quack-stop" quack-stop
;;                                         :image ,quack-tbi-stop
;;                                         :help  "Stop"))
;;
;;     (set (make-local-variable 'tool-bar-map) map)))

;; TODO: Extend `scheme-imenu-generic-expression' for PLT-specific definition
;;       forms and for definitions within modules.

;; TODO: Clickable URLs
;;
;; (defvar quack-url-keymap)
;; 
;; (setq quack-url-keymap (make-sparse-keymap))
;; (define-key quack-url-keymap "\r" 'quack-browse-overlaid-url)
;; (define-key quack-url-keymap "q" 'quack-browse-overlaid-url)
;; 
;; (defun quack-make-url-overlay (beg end &optional url)
;;   (let ((ovl (make-overlay beg end nil t)))
;;     (overlay-put ovl 'face      'underline)
;;     (overlay-put ovl 'local-map 'quack-url-keymap)
;;     (overlay-put ovl 'help-echo "Press RET to browse this URL.")
;;     (overlay-put ovl 'quack-url
;;                      (or url (buffer-substring-no-properties beg end)))
;;     ovl))
;; 
;; (defun quack-insert-url (url)
;;   (let* ((beg (point)))
;;     (insert url)
;;     (quack-make-url-overlay beg (point))))
;; 
;; (defun quack-overlaid-url-at-point (&optional pt)
;;   (let ((overlays (overlays-at (or pt (point))))
;;         (url      nil))
;;     (while overlays
;;       (setq overlays (if (setq url (overlay-get (car overlays) 'quack-url))
;;                          (cdr overlays)
;;                        '())))
;;     url))
;; 
;; (defun quack-browse-overlaid-url (pt)
;;   ;; Dehydration.
;;   (interactive "d")
;;   (quack-browse-url (quack-overlaid-url-at-point pt)))

;; TODO: Possible Future Inferior Process I/O Stuff.  Make encoding with
;;       inferior process disambiguate REPL values, port output, error info,
;;       etc.  Start of code commented out below.  This may require rewriting
;;       chunks of `cmuscheme' and `comint'.
;;
;;       Try to use ELI protocol first.  http://www.cliki.net/ELI
;;
;; (defface quack-output-face
;;   '((((class color)) (:foreground "purple4" :background "lavender"))
;;     (t               (:inverse-video t)))
;;   "Face used for..."
;;   :group 'quack)
;;
;; (defface quack-value-face
;;   '((((class color)) (:foreground "blue4" :background "light sky blue"))
;;     (t               (:inverse-video t)))
;;   "Face used for..."
;;   :group 'quack)
;;
;; Escape Codes:
;;     REPL State:
;;         R  repl read begin
;;         r  repl read end
;;         E  repl eval begin
;;         e  repl eval end
;;         P  repl print begin
;;         p  repl print end
;;     Stream Change:
;;         O  output stream
;;         E  error stream
;;     Error Info?
;;
;; (defconst quack-mzscheme-init-string
;;   (let ((print-length nil)
;;         (print-level  nil))
;;     (prin1-to-string
;;      '(let ((o (current-output-port))
;;             (i (current-input-port))
;;             (e (current-eval)))
;;         ;; TODO: Define custom escaping output and error ports here.
;;         (current-prompt-read
;;          (lambda ()
;;            (display "\eR" o)
;;            (begin0 (read-syntax "quack-repl" i)
;;                    (display "\er" o))))
;;         (current-eval
;;          (lambda (n)
;;            (display "\eE" o)
;;            (begin0 (e n)
;;                    (display "\ee" o))))
;;         (current-print
;;          (lambda (n)
;;            (display "\eP" o)
;;            (begin0 (print n o)
;;                    (display "\ep" o))))))))
;;
;; In `quack' function, after call to `run-scheme':
;;
;; (add-hook 'comint-preoutput-filter-functions
;;           'quack-comint-preoutput-filter-func)
;; (comint-send-string (scheme-proc) quack-mzscheme-init-string)
;; (comint-send-string (scheme-proc) "\n")

;; TODO: If we do that, then add pretty-printing of REPL results.

;; TODO: Maybe provide utilities for converting to/from PLT-style
;;       square-bracket paren conventions.

;; TODO: Populate abbrevs table from keywords extracted from manuals, and from
;;       definitions in current buffer.  Or maybe query running MzScheme
;;       process for bound symbols.

;; TODO: Maybe use `compile-zos' to do error-checking for PLT (look up person
;;       to credit with idea of using that to get more warnings).  Need to know
;;       more about a particular Scheme implementation than just the command
;;       line to start its REPL, though.

;; TODO: Perhaps put some initialization code that depends on user's custom
;;       settings into after-init-hook.  See if this works in XEmacs.

;; TODO: Set `interpreter-mode-alist' based on interpreter list.

;; TODO: "I think it would be good if the quack menu showed up only when emacs
;;        was in Scheme mode."

;; TODO: Support this:
;;
;;       * Added 'addon-dir for `find-system-path':
;;       Unix: "~/.plt-scheme"
;;       Windows: "PLT Scheme" in the user's Application Data folder.
;;       Mac OS X: "~/Library/PLT Scheme"
;;       Mac OS Classic: "PLT Scheme" in the preferences folder.
;;
;;       The version string for "~/.plt-scheme/<version>/collects/" might be:
;;       mzscheme -mqe '(begin (display (version)) (exit))'
;;       Double-check PLT source first.

;; TODO: Add autoindenting to inferior Scheme buffer when pressing RET on an
;;       incomplete sexp -- iff we can do this reliably enough.

;; TODO: When tidying and point is within a series of multiple blank lines that
;;       are reduced to a single blank line, leave point at the beginning of
;;       the single blank line.

;; TODO: Riastradh says: Do you suppose you could add a feature to Quack that
;;       indents lists beginning with symbols of the form WITH-...  &
;;       CALL-WITH-... as if their SCHEME-INDENT-FUNCTION property were DEFUN?

;; TODO: Matt Dickerson asks " Also, the command history appears to be based on
;;       newlines -- I work with blocks of code in the REPL and would like C-p
;;       to give me the last block, not the last line of the previous block."

;; TODO: Maybe get appropriate PLT collection path from the default for
;;       whatever "mzscheme" executable is picked up.
;;
;; mzscheme -emq '(begin (write (current-library-collection-paths)) (exit 0))'
;; ("/home/neil/collects" "/home/neil/.plt-scheme/208/collects"
;;  "/usr/lib/plt/collects")

;; TODO: Bind M-[ to quack-insert-parentheses

;; TODO: Peter Barabas reports that `quack-global-menu-p' set to nil doesn't
;; disable the menu.

;; TODO: Way to get default collects directories.  From Matthew Flatt,
;; 2006-04-22:
;; 
;; env PLTCOLLECTS="" mzscheme -mvqe '(printf "~s\n" (map path->string 
;; (current-library-collection-paths)))'

;; TODO: Have key binding to insert "lambda" (for use with pretty-lambda).
;; Suggested by Olwe Bottorff on 2006-04-20.

;; TODO: Jerry van Dijk writes: "I would like to try out quack, but I do not
;; like its menu constantly on the main menu bar (as I use emacs for a lot of
;; things). Unfortunately sofar quack has bravely defied all my attempts to
;; remove it. From desecting the customize option to adding (define-key
;; global-map [menu-bar quack] nil)"

;; TODO: We could do this:
;;
;; mzscheme -m -e "(begin (display #\') (write (map path->string (current-library-collection-paths))) (newline) (exit))"
;; '("/home/neil/collects"
;;   "/home/neil/.plt-scheme/360/collects"
;;   "/usr/lib/plt/collects")

;; emacs22  -batch -no-site-file -f batch-byte-compile quack.el ; rm quack.elc
;; emacs21  -batch -no-site-file -f batch-byte-compile quack.el ; rm quack.elc
;; emacs20  -batch -no-site-file -f batch-byte-compile quack.el ; rm quack.elc
;; xemacs21 -batch -no-site-file -f batch-byte-compile quack.el ; rm quack.elc

;; End:

(provide 'quack)

;; quack.el ends here
