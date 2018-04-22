;;; haskell-doc.el --- show function types in echo area  -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2004, 2005, 2006, 2007, 2009, 2016  Free Software Foundation, Inc.
;; Copyright © 1997  Hans-Wolfgang Loidl
;;             2016  Arthur Fayzrakhmanov

;; Author: Hans-Wolfgang Loidl <hwloidl@dcs.glasgow.ac.uk>
;; Temporary Maintainer and Hacker: Graeme E Moss <gem@cs.york.ac.uk>
;; Keywords: extensions, minor mode, language mode, Haskell
;; Created: 1997-06-17
;; URL: http://cvs.haskell.org/cgi-bin/cvsweb.cgi/fptools/CONTRIB/haskell-modes/emacs/haskell-doc.el?rev=HEAD

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This program shows the type of the Haskell function under the cursor in the
;; minibuffer.  It acts as a kind of "Emacs background process", by regularly
;; checking the word under the cursor and matching it against a list of
;; prelude, library, local and global functions.

;; This program was inspired by the `eldoc.el' package by Noah Friedman.

;; Installation:

;; Depending on the major mode you use for your Haskell programs add
;; one of the following to your .emacs:
;;
;;   (add-hook 'haskell-mode-hook 'haskell-doc-mode)

;; Customisation:

;; You can control what exactly is shown by setting the following variables to
;; either t or nil:
;;  `haskell-doc-show-global-types' (default: nil)
;;  `haskell-doc-show-reserved'     (default: t)
;;  `haskell-doc-show-prelude'      (default: t)
;;  `haskell-doc-show-strategy'     (default: t)
;;  `haskell-doc-show-user-defined' (default: t)

;; If you want to define your own strings for some identifiers define an
;; alist of (ID . STRING) and set `haskell-doc-show-user-defined' to t.
;; E.g:
;;
;;   (setq haskell-doc-show-user-defined t)
;;   (setq haskell-doc-user-defined-ids
;;      (list
;;         '("main" . "just another pathetic main function")
;;         '("foo" . "a very dummy name")
;;         '("bar" . "another dummy name")))

;;  The following two variables are useful to make the type fit on one line:
;;  If `haskell-doc-chop-off-context' is non-nil the context part of the type
;;  of a local fct will be eliminated (default: t).
;;  If `haskell-doc-chop-off-fctname' is non-nil the function name is not
;;  shown together with the type (default: nil).

;; Internals:

;; `haskell-doc-mode' is implemented as a minor-mode.  So, you can combine it
;; with any other mode.  To enable it just type
;;   M-x haskell-doc-mode

;; These are the names of the functions that can be called directly by the
;; user (with keybindings in `haskell-mode'):
;;  `haskell-doc-mode' ... toggle haskell-doc-mode; with prefix turn it on
;;                        unconditionally if the prefix is greater 0 otherwise
;;                        turn it off
;;                        Key: CTRL-c CTRL-o (CTRL-u CTRL-c CTRL-o)
;;  `haskell-doc-ask-mouse-for-type' ... show the type of the id under the mouse
;;                                      Key: C-S-M-mouse-3
;;  `haskell-doc-show-reserved'     ... toggle echoing of reserved id's types
;;  `haskell-doc-show-prelude'      ... toggle echoing of prelude id's types
;;  `haskell-doc-show-strategy'     ... toggle echoing of strategy id's types
;;  `haskell-doc-show-user-defined' ... toggle echoing of user def id's types
;;  `haskell-doc-check-active' ... check whether haskell-doc is active;
;;                                 Key: CTRL-c ESC-/

;; ToDo:

;;   - Fix byte-compile problems in `haskell-doc-prelude-types' for getArgs etc
;;   - Write a parser for .hi files.  Read library interfaces via this parser.
;;   - Indicate kind of object with colours
;;   - Handle multi-line types
;;   - Encode i-am-fct info in the alist of ids and types.

;; Bugs:

;;   - Some prelude fcts aren't displayed properly.  This might be due to a
;;     name clash of Haskell and Elisp functions (e.g. length) which
;;     confuses Emacs when reading `haskell-doc-prelude-types'

;;; Changelog:

;;  $Log: haskell-doc.el,v $
;;  Revision 1.31 2015/07/23 10:34:20  ankhers
;;  (turn-on-haskell-doc-mode): marked obsolete
;;  (turn-on-haskell-doc): marked obsolete
;;  other packages have been moving away from (turn-on-haskell-*)
;;
;;  Revision 1.30  2009/02/02 21:00:33  monnier
;;  (haskell-doc-imported-list): Don't add current buffer
;;  to the imported file list if it is not (yet?) visiting a file.
;;
;;  Revision 1.29  2007-12-12 04:04:19  monnier
;;  (haskell-doc-in-code-p): New function.
;;  (haskell-doc-show-type): Use it.
;;
;;  Revision 1.28  2007/08/30 03:10:08  monnier
;;  Comment/docs fixes.
;;
;;  Revision 1.27  2007/07/30 17:36:50  monnier
;;  (displayed-month): Remove declaration since it's not used here.
;;
;;  Revision 1.26  2007/02/10 06:28:55  monnier
;;  (haskell-doc-get-current-word): Remove.
;;  Change all refs to it, to use haskell-ident-at-point instead.
;;
;;  Revision 1.25  2007/02/09 21:53:42  monnier
;;  (haskell-doc-get-current-word): Correctly distinguish
;;  variable identifiers and infix identifiers.
;;  (haskell-doc-rescan-files): Avoid switch-to-buffer.
;;  (haskell-doc-imported-list): Operate on current buffer.
;;  (haskell-doc-make-global-fct-index): Adjust call.
;;
;;  Revision 1.24  2006/11/20 20:18:24  monnier
;;  (haskell-doc-mode-print-current-symbol-info): Fix thinko.
;;
;;  Revision 1.23  2006/10/20 03:12:31  monnier
;;  Drop post-command-idle-hook in favor of run-with-idle-timer.
;;  (haskell-doc-timer, haskell-doc-buffers): New vars.
;;  (haskell-doc-mode): Use them.
;;  (haskell-doc-check-active): Update the check.
;;  (haskell-doc-mode-print-current-symbol-info): Remove the interactive spec.
;;  Don't sit-for unless it's really needed.
;;
;;  Revision 1.22  2006/09/20 18:42:35  monnier
;;  Doc fix.
;;
;;  Revision 1.21  2005/11/21 21:48:52  monnier
;;  * haskell-doc.el (haskell-doc-extract-types): Get labelled data working.
;;  (haskell-doc-prelude-types): Update via auto-generation.
;;
;;  * haskell-doc.el (haskell-doc-extract-types): Get it partly working.
;;  (haskell-doc-fetch-lib-urls): Don't use a literal if we apply
;;  `nreverse' on it later on.
;;  (haskell-doc-prelude-types): Update some parts by auto-generation.
;;  (haskell-doc-grab, haskell-doc-string-nub-ws): Simplify.
;;
;;  * haskell-doc.el (haskell-doc-maintainer, haskell-doc-varlist)
;;  (haskell-doc-submit-bug-report, haskell-doc-ftp-site)
;;  (haskell-doc-visit-home): Remove.
;;  (haskell-doc-reserved-ids, haskell-doc-fetch-lib-urls)
;;  (haskell-doc-extract-and-insert-types): New funs.
;;  (haskell-doc-reserved-ids): Fix type of `map'.
;;
;;  Revision 1.20  2005/11/21 21:27:57  monnier
;;  (haskell-doc-extract-types): Get labelled data working.
;;  (haskell-doc-prelude-types): Update via auto-generation.
;;
;;  Revision 1.19  2005/11/21 20:44:13  monnier
;;  (haskell-doc-extract-types): Get it partly working.
;;  (haskell-doc-fetch-lib-urls): Don't use a literal if we apply
;;  `nreverse' on it later on.
;;  (haskell-doc-prelude-types): Update some parts by auto-generation.
;;  (haskell-doc-grab, haskell-doc-string-nub-ws): Simplify.
;;
;;  Revision 1.18  2005/11/21 18:02:15  monnier
;;  (haskell-doc-maintainer, haskell-doc-varlist)
;;  (haskell-doc-submit-bug-report, haskell-doc-ftp-site)
;;  (haskell-doc-visit-home): Remove.
;;  (haskell-doc-reserved-ids, haskell-doc-fetch-lib-urls)
;;  (haskell-doc-extract-and-insert-types): New funs.
;;  (haskell-doc-reserved-ids): Fix type of `map'.
;;
;;  Revision 1.17  2005/11/20 23:55:09  monnier
;;  Add coding cookie.
;;
;;  Revision 1.16  2005/11/07 01:28:16  monnier
;;  (haskell-doc-xemacs-p, haskell-doc-emacs-p)
;;  (haskell-doc-message): Remove.
;;  (haskell-doc-is-id-char-at): Remove.
;;  (haskell-doc-get-current-word): Rewrite.
;;
;;  Revision 1.15  2005/11/04 17:11:12  monnier
;;  Add arch-tag.
;;
;;  Revision 1.14  2005/08/24 11:36:32  monnier
;;  (haskell-doc-message): Paren typo.
;;
;;  Revision 1.13  2005/08/23 19:23:27  monnier
;;  (haskell-doc-show-type): Assume that the availability
;;  of display-message won't change at runtime.
;;
;;  Revision 1.12  2005/07/18 21:04:14  monnier
;;  (haskell-doc-message): Remove.
;;  (haskell-doc-show-type): inline it.  Do nothing for if there's no doc to show.
;;
;;  Revision 1.11  2004/12/10 17:33:18  monnier
;;  (haskell-doc-minor-mode-string): Make it dynamic.
;;  (haskell-doc-install-keymap): Remove conflicting C-c C-o binding.
;;  (haskell-doc-mode): Make a nil arg turn the mode ON.
;;  (turn-on-haskell-doc-mode): Make it an alias for haskell-doc-mode.
;;  (haskell-doc-mode): Don't touch haskell-doc-minor-mode-string.
;;  (haskell-doc-show-global-types): Don't touch
;;  haskell-doc-minor-mode-string.  Call haskell-doc-make-global-fct-index.
;;  (haskell-doc-check-active): Fix message.
;;  (define-key-after): Don't define.
;;  (haskell-doc-install-keymap): Check existence of define-key-after.
;;
;;  Revision 1.10  2004/11/25 23:03:23  monnier
;;  (haskell-doc-sym-doc): Make even the last char bold.
;;
;;  Revision 1.9  2004/11/24 22:14:36  monnier
;;  (haskell-doc-install-keymap): Don't blindly assume there's a Hugs menu.
;;
;;  Revision 1.8  2004/11/22 10:45:35  simonmar
;;  Fix type of getLine
;;
;;  Revision 1.7  2004/10/14 22:27:47  monnier
;;  (turn-off-haskell-doc-mode, haskell-doc-current-info): Don't autoload.
;;
;;  Revision 1.6  2004/10/13 22:45:22  monnier
;;  (haskell-doc): New group.
;;  (haskell-doc-show-reserved, haskell-doc-show-prelude)
;;  (haskell-doc-show-strategy, haskell-doc-show-user-defined)
;;  (haskell-doc-chop-off-context, haskell-doc-chop-off-fctname):
;;  Make them custom vars.
;;  (haskell-doc-keymap): Declare and fill it right there.
;;  (haskell-doc-mode): Simplify.
;;  (haskell-doc-toggle-var): Make it into what it was supposed to be.
;;  (haskell-doc-mode-print-current-symbol-info): Simplify.
;;  (haskell-doc-current-info): New autoloaded function.
;;  (haskell-doc-sym-doc): New fun extracted from haskell-doc-show-type.
;;  (haskell-doc-show-type): Use it.
;;  (haskell-doc-wrapped-type-p): Remove unused var `lim'.
;;  (haskell-doc-forward-sexp-safe, haskell-doc-current-symbol): Remove.  Unused.
;;  (haskell-doc-visit-home): Don't require ange-ftp, it's autoloaded.
;;  (haskell-doc-install-keymap): Simplify.
;;
;;  Revision 1.5  2003/01/09 11:56:26  simonmar
;;  Patches from Ville Skyttä <scop@xemacs.org>, the XEmacs maintainer of
;;  the haskell-mode:
;;
;;   - Make the auto-mode-alist modifications autoload-only.
;;
;;  Revision 1.4  2002/10/14 09:55:03  simonmar
;;  Patch to update the Prelude/libraries function names and to remove
;;  support for older versions of Haskell.
;;
;;  Submitted by: Anders Lau Olsen <alauo@mip.sdu.dk>
;;
;;  Revision 1.3  2002/04/30 09:34:37  rrt
;;  Remove supporting Haskell 1.4 and 1.2 from the ToDo list. It's Far Too Late.
;;
;;  Add (require 'imenu). Thanks to N. Y. Kwok.
;;
;;  Revision 1.2  2002/04/23 14:45:10  simonmar
;;  Tweaks to the doc strings and support for customization, from
;;  Ville Skyttä <scop@xemacs.org>.
;;
;;  Revision 1.1  2001/07/19 16:17:36  rrt
;;  Add the current version of the Moss/Thorn/Marlow Emacs mode, along with its
;;  web pages and sample files. This is now the preferred mode, and the
;;  haskell.org pages are being changed to reflect that. Also includes the new
;;  GHCi mode from Chris Webb.
;;
;;  Revision 1.6  1998/12/10 16:27:25  hwloidl
;;  Minor changes ("Doc" as modeline string, mouse-3 moved to C-S-M-mouse-3)
;;
;;  Revision 1.5  1998/09/24 14:25:46  gem
;;  Fixed minor compatibility bugs with Haskell mode of Moss&Thorn.
;;  Disabled M-/ binding.
;;
;;  Revision 1.4  1997/11/12 23:51:19  hwloidl
;;  Fixed start-up problem under emacs-19.34.
;;  Added support for wrapped (multi-line) types and 2 vars to control the
;;  behaviour with long fct types
;;
;;  Revision 1.3  1997/11/03 00:48:03  hwloidl
;;  Major revision for first release.
;;  Added alists for showing prelude fcts, haskell syntax, and strategies
;;  Added mouse interface to show type under mouse
;;  Fixed bug which causes demon to fall over
;;  Works now with hugs-mode and haskell-mode under emacs 19.34,20 and xemacs 19.15
;;

;;; Code:

(require 'haskell-mode)
(require 'haskell-process)
(require 'haskell)
(require 'haskell-utils)
(require 'inf-haskell)
(require 'imenu)
(require 'eldoc)

;;;###autoload
(defgroup haskell-doc nil
  "Show Haskell function types in echo area."
  :group 'haskell
  :prefix "haskell-doc-")


(defvar-local haskell-doc-mode nil
  "*If non-nil, show the type of the function near point or a related comment.

If the identifier near point is a Haskell keyword and the variable
`haskell-doc-show-reserved' is non-nil show a one line summary
of the syntax.

If the identifier near point is a Prelude or one of the standard library
functions and `haskell-doc-show-prelude' is non-nil show its type.

If the identifier near point is local \(i.e. defined in this module\) check
the `imenu' list of functions for the type.  This obviously requires that
your language mode uses `imenu'.

If the identifier near point is global \(i.e. defined in an imported module\)
and the variable `haskell-doc-show-global-types' is non-nil show the type of its
function.

If the identifier near point is a standard strategy or a function, type related
related to strategies and `haskell-doc-show-strategy' is non-nil show the type
of the function.  Strategies are special to the parallel execution of Haskell.
If you're not interested in that just turn it off.

If the identifier near point is a user defined function that occurs as key
in the alist `haskell-doc-user-defined-ids' and the variable
`haskell-doc-show-user-defined' is non-nil show the type of the function.

This variable is buffer-local.")

(defvar haskell-doc-mode-hook nil
  "Hook invoked when entering `haskell-doc-mode'.")

(defvar-local haskell-doc-index nil
  "Variable holding an alist matching file names to fct-type alists.
The function `haskell-doc-make-global-fct-index' rebuilds this variables
\(similar to an `imenu' rescan\).
This variable is buffer-local.")

(defcustom haskell-doc-show-global-types nil
  "If non-nil, search for the types of global functions by loading the files.
This variable is buffer-local."
  :group 'haskell-doc
  :type 'boolean)
(make-variable-buffer-local 'haskell-doc-show-global-types)

(defcustom haskell-doc-show-reserved t
  "If non-nil, show a documentation string for reserved ids.
This variable is buffer-local."
  :group 'haskell-doc
  :type 'boolean)
(make-variable-buffer-local 'haskell-doc-show-reserved)

(defcustom haskell-doc-show-prelude t
  "If non-nil, show a documentation string for prelude functions.
This variable is buffer-local."
  :group 'haskell-doc
  :type 'boolean)
(make-variable-buffer-local 'haskell-doc-show-prelude)

(defcustom haskell-doc-show-strategy t
  "If non-nil, show a documentation string for strategies.
This variable is buffer-local."
  :group 'haskell-doc
  :type 'boolean)
(make-variable-buffer-local 'haskell-doc-show-strategy)

(defcustom haskell-doc-show-user-defined t
  "If non-nil, show a documentation string for user defined ids.
This variable is buffer-local."
  :group 'haskell-doc
  :type 'boolean)
(make-variable-buffer-local 'haskell-doc-show-user-defined)

(defcustom haskell-doc-chop-off-context t
  "If non-nil eliminate the context part in a Haskell type."
  :group 'haskell-doc
  :type 'boolean)

(defcustom haskell-doc-chop-off-fctname nil
  "If non-nil omit the function name and show only the type."
  :group 'haskell-doc
  :type 'boolean)

(defcustom haskell-doc-use-inf-haskell nil
  "If non-nil use inf-haskell.el to get type and kind information."
  :group 'haskell-doc
  :type 'boolean)

(defvar haskell-doc-search-distance 40  ; distance in characters
  "*How far to search when looking for the type declaration of fct under cursor.")


(defvar haskell-doc-idle-delay 0.50
  "*Number of seconds of idle time to wait before printing.
If user input arrives before this interval of time has elapsed after the
last input, no documentation will be printed.

If this variable is set to 0, no idle time is required.")

(defvar haskell-doc-argument-case 'identity ; 'upcase
  "Case in which to display argument names of functions, as a symbol.
This has two preferred values: `upcase' or `downcase'.
Actually, any name of a function which takes a string as an argument and
returns another string is acceptable.")

(defvar haskell-doc-mode-message-commands nil
  "*Obarray of command names where it is appropriate to print in the echo area.

This is not done for all commands since some print their own
messages in the echo area, and these functions would instantly overwrite
them.  But `self-insert-command' as well as most motion commands are good
candidates.

It is probably best to manipulate this data structure with the commands
`haskell-doc-add-command' and `haskell-doc-remove-command'.")

;;(cond ((null haskell-doc-mode-message-commands)
;;       ;; If you increase the number of buckets, keep it a prime number.
;;       (setq haskell-doc-mode-message-commands (make-vector 31 0))
;;       (let ((list '("self-insert-command"
;;                     "next-"         "previous-"
;;                     "forward-"      "backward-"
;;                     "beginning-of-" "end-of-"
;;                     "goto-"
;;                     "recenter"
;;                     "scroll-"))
;;             (syms nil))
;;         (while list
;;           (setq syms (all-completions (car list) obarray 'fboundp))
;;           (setq list (cdr list))
;;           (while syms
;;             (set (intern (car syms) haskell-doc-mode-message-commands) t)
;;             (setq syms (cdr syms)))))))

;; Bookkeeping; the car contains the last symbol read from the buffer.
;; The cdr contains the string last displayed in the echo area, so it can
;; be printed again if necessary without reconsing.
(defvar haskell-doc-last-data '(nil . nil))

(defvar haskell-doc-minor-mode-string
  '(haskell-doc-show-global-types " DOC" " Doc")
  "*String to display in mode line when Haskell-Doc Mode is enabled.")


(defvar haskell-doc-reserved-ids
  '(("case" . "case exp of { alts [;] }")
    ("class" . "class [context =>] simpleclass [where { cbody [;] }]")
    ("data" . "data [context =>] simpletype = constrs [deriving]")
    ("default" . "default (type1 , ... , typen)")
    ("deriving" . "deriving (dclass | (dclass1, ... , dclassn))") ; used with data or newtype
    ("do" . "do { stmts [;] }  stmts -> exp [; stmts] | pat <- exp ; stmts | let decllist ; stmts")
    ("else" . "if exp then exp else exp")
    ("if" . "if exp then exp else exp")
    ("import" . "import [qualified] modid [as modid] [impspec]")
    ("in" . "let decllist in exp")
    ("infix" . "infix [digit] ops")
    ("infixl" . "infixl [digit] ops")
    ("infixr" . "infixr [digit] ops")
    ("instance" . "instance [context =>] qtycls inst [where { valdefs [;] }]")
    ("let" . "let { decl; ...; decl [;] } in exp")
    ("module" . "module modid [exports] where body")
    ("newtype" . "newtype [context =>] simpletype = con atype [deriving]")
    ("of" . "case exp of { alts [;] }")
    ("then" . "if exp then exp else exp")
    ("type" . "type simpletype = type")
    ("where" . "exp where { decl; ...; decl [;] }") ; check that ; see also class, instance, module
    ("as" . "import [qualified] modid [as modid] [impspec]")
    ("qualified" . "import [qualified] modid [as modid] [impspec]")
    ("hiding" . "hiding ( import1 , ... , importn [ , ] )")
    ("family" . "(type family type [kind] [= type_fam_equations]) | (data family type [kind])"))
  "An alist of reserved identifiers.
Each element is of the form (ID . DOC) where both ID and DOC are strings.
DOC should be a concise single-line string describing the construct in which
the keyword is used.")


(defun haskell-doc-extract-types (url)
  (with-temp-buffer
    (insert-file-contents url)
    (goto-char (point-min))
    (while (search-forward "&nbsp;" nil t) (replace-match " " t t))

    ;; First, focus on the actual code, removing the surrounding HTML text.
    (goto-char (point-min))
    (let ((last (point-min))
          (modules nil))
      (while (re-search-forward "^module +\\([[:alnum:]]+\\)" nil t)
        (let ((module (match-string 1)))
          (if (member module modules)
              ;; The library nodes of the HTML doc contain modules twice:
              ;; once at the top, with only type declarations, and once at
              ;; the bottom with an actual sample implementation which may
              ;; include declaration of non-exported values.
              ;; We're now at this second occurrence is the implementation
              ;; which should thus be ignored.
              nil
            (push module modules)
            (delete-region last (point))
            (search-forward "</tt>")
            ;; Some of the blocks of code are split.
            (while (looking-at "\\(<[^<>]+>[ \t\n]*\\)*<tt>")
              (goto-char (match-end 0))
              (search-forward "</tt>"))
            (setq last (point)))))
      (delete-region last (point-max))

      ;; Then process the HTML encoding to get back to pure ASCII.
      (goto-char (point-min))
      (while (search-forward "<br>" nil t) (replace-match "\n" t t))
      ;; (goto-char (point-min))
      ;; (while (re-search-forward "<[^<>]+>" nil t) (replace-match "" t t))
      (goto-char (point-min))
      (while (search-forward "&gt;" nil t) (replace-match ">" t t))
      (goto-char (point-min))
      (while (search-forward "&lt;" nil t) (replace-match "<" t t))
      (goto-char (point-min))
      (while (search-forward "&amp;" nil t) (replace-match "&" t t))
      (goto-char (point-min))
      (if (re-search-forward "&[a-z]+;" nil t)
          (error "Unexpected charref %s" (match-string 0)))
      ;; Remove TABS.
      (goto-char (point-min))
      (while (search-forward "\t" nil t) (replace-match "        " t t))

      ;; Finally, extract the actual data.
      (goto-char (point-min))
      (let* ((elems nil)
             (space-re "[ \t\n]*\\(?:--.*\n[ \t\n]*\\)*")
             (comma-re (concat " *," space-re))
             ;; A list of identifiers.  We have to be careful to weed out
             ;; entries like "ratPrec = 7 :: Int".  Also ignore entries
             ;; which start with a < since they're actually in the HTML text
             ;; part.  And the list may be spread over several lines, cut
             ;; after a comma.
             (idlist-re
              (concat "\\([^< \t\n][^ \t\n]*"
                      "\\(?:" comma-re "[^ \t\n]+\\)*\\)"))
             ;; A type.  A few types are spread over 2 lines,
             ;; cut after the "=>", so we have to handle these as well.
             (type-re "\\(.*[^\n>]\\(?:>[ \t\n]+.*[^\n>]\\)*\\) *$")
             ;; A decl of a list of values, possibly indented.
             (val-decl-re
              (concat "^\\( +\\)?" idlist-re "[ \t\n]*::[ \t\n]*" type-re))
             (re (concat
                  ;; 3 possibilities: a class decl, a data decl, or val decl.
                  ;; First, let's match a class decl.
                  "^class \\(?:.*=>\\)? *\\(.*[^ \t\n]\\)[ \t\n]*where"

                  ;; Or a value decl:
                  "\\|" val-decl-re

                  "\\|" ;; Or a data decl.  We only handle single-arm
                  ;; datatypes with labels.
                  "^data +\\([[:alnum:]][[:alnum:] ]*[[:alnum:]]\\)"
                  " *=.*{\\([^}]+\\)}"
                  ))
             (re-class (concat "^[^ \t\n]\\|" re))
             curclass)
        (while (re-search-forward (if curclass re-class re) nil t)
          (cond
           ;; A class decl.
           ((match-end 1) (setq curclass (match-string 1)))
           ;; A value decl.
           ((match-end 4)
            (let ((type (match-string 4))
                  (vars (match-string 3))
                  (indented (match-end 2)))
              (if (string-match "[ \t\n][ \t\n]+" type)
                  (setq type (replace-match " " t t type)))
              (if (string-match " *\\(--.*\\)?\\'" type)
                  (setq type (substring type 0 (match-beginning 0))))
              (if indented
                  (if curclass
                      (if (string-match "\\`\\(.*[^ \t\n]\\) *=> *" type)
                          (let ((classes (match-string 1 type)))
                            (setq type (substring type (match-end 0)))
                            (if (string-match "\\`(.*)\\'" classes)
                                (setq classes (substring classes 1 -1)))
                            (setq type (concat "(" curclass ", " classes
                                               ") => " type)))
                        (setq type (concat curclass " => " type)))
                    ;; It's actually not an error: just a type annotation on
                    ;; some local variable.
                    ;; (error "Indentation outside a class in %s: %s"
                    ;;        module vars)
                    nil)
                (setq curclass nil))
              (dolist (var (split-string vars comma-re t))
                (if (string-match "(.*)" var) (setq var (substring var 1 -1)))
                (push (cons var type) elems))))
           ;; A datatype decl.
           ((match-end 5)
            (setq curclass nil)
            (let ((name (match-string 5)))
              (save-excursion
                (save-restriction
                  (narrow-to-region (match-beginning 6) (match-end 6))
                  (goto-char (point-min))
                  (while (re-search-forward val-decl-re nil t)
                    (let ((vars (match-string 2))
                          (type (match-string 3)))
                      (if (string-match "[ \t\n][ \t\n]+" type)
                          (setq type (replace-match " " t t type)))
                      (if (string-match " *\\(--.*\\)?\\'" type)
                          (setq type (substring type 0 (match-beginning 0))))
                      (if (string-match ",\\'" type)
                          (setq type (substring type 0 -1)))
                      (setq type (concat name " -> " type))
                      (dolist (var (split-string vars comma-re t))
                        (if (string-match "(.*)" var)
                            (setq var (substring var 1 -1)))
                        (push (cons var type) elems))))))))

           ;; The end of a class declaration.
           (t (setq curclass nil) (beginning-of-line))))
        (cons (car (last modules)) elems)))))

(defun haskell-doc-fetch-lib-urls (base-url)
  (with-temp-buffer
    (insert-file-contents base-url)
    (goto-char (point-min))
    (search-forward "Part II: Libraries")
    (delete-region (point-min) (point))
    (search-forward "</table>")
    (delete-region (point) (point-max))
    (goto-char (point-min))
    (let ((libs (list "standard-prelude.html")))
      (while (re-search-forward "<a href=\"\\([^\"]+\\)\">" nil t)
        (push (match-string 1) libs))
      (mapcar (lambda (s) (expand-file-name s (file-name-directory base-url)))
              (nreverse libs)))))

(defun haskell-doc-extract-and-insert-types (url)
  "Fetch the types from the online doc and insert them at point.
URL is the URL of the online doc."
  (interactive (if current-prefix-arg
                   (read-file-name "URL: ")
                 (list "http://www.haskell.org/onlinereport/")))
  (let ((urls (haskell-doc-fetch-lib-urls url)))
    (dolist (url urls)
      (let ((data (haskell-doc-extract-types url)))
        (insert ";; " (pop data)) (indent-according-to-mode) (newline)
        (dolist (elem (sort data (lambda (x y) (string-lessp (car x) (car y)))))
          (prin1 elem (current-buffer))
          (indent-according-to-mode) (newline))))))

(defvar haskell-doc-prelude-types
  ;; This list was auto generated by `haskell-doc-extract-and-insert-types'.
  '(
    ;; Prelude
    ("!!" . "[a] -> Int -> a")
    ("$" . "(a -> b) -> a -> b")
    ("$!" . "(a -> b) -> a -> b")
    ("&&" . "Bool -> Bool -> Bool")
    ("*" . "Num a => a -> a -> a")
    ("**" . "Floating a => a -> a -> a")
    ("+" . "Num a => a -> a -> a")
    ("++" . "[a] -> [a] -> [a]")
    ("-" . "Num a => a -> a -> a")
    ("." . "(b -> c) -> (a -> b) -> a -> c")
    ("/" . "Fractional a => a -> a -> a")
    ("/=" . "Eq a => a -> a -> Bool")
    ("<" . "Ord a => a -> a -> Bool")
    ("<=" . "Ord a => a -> a -> Bool")
    ("=<<" . "Monad m => (a -> m b) -> m a -> m b")
    ("==" . "Eq a => a -> a -> Bool")
    (">" . "Ord a => a -> a -> Bool")
    (">=" . "Ord a => a -> a -> Bool")
    (">>" . "Monad m => m a -> m b -> m b")
    (">>=" . "Monad m => m a -> (a -> m b) -> m b")
    ("^" . "(Num a, Integral b) => a -> b -> a")
    ("^^" . "(Fractional a, Integral b) => a -> b -> a")
    ("abs" . "Num a => a -> a")
    ("acos" . "Floating a => a -> a")
    ("acosh" . "Floating a => a -> a")
    ("all" . "(a -> Bool) -> [a] -> Bool")
    ("and" . "[Bool] -> Bool")
    ("any" . "(a -> Bool) -> [a] -> Bool")
    ("appendFile" . "FilePath -> String -> IO ()")
    ("asTypeOf" . "a -> a -> a")
    ("asin" . "Floating a => a -> a")
    ("asinh" . "Floating a => a -> a")
    ("atan" . "Floating a => a -> a")
    ("atan2" . "RealFloat a => a -> a -> a")
    ("atanh" . "Floating a => a -> a")
    ("break" . "(a -> Bool) -> [a] -> ([a],[a])")
    ("catch" . "IO a -> (IOError -> IO a) -> IO a")
    ("ceiling" . "(RealFrac a, Integral b) => a -> b")
    ("compare" . "Ord a => a -> a -> Ordering")
    ("concat" . "[[a]] -> [a]")
    ("concatMap" . "(a -> [b]) -> [a] -> [b]")
    ("const" . "a -> b -> a")
    ("cos" . "Floating a => a -> a")
    ("cosh" . "Floating a => a -> a")
    ("curry" . "((a, b) -> c) -> a -> b -> c")
    ("cycle" . "[a] -> [a]")
    ("decodeFloat" . "RealFloat a => a -> (Integer,Int)")
    ("div" . "Integral a => a -> a -> a")
    ("divMod" . "Integral a => a -> a -> (a,a)")
    ("drop" . "Int -> [a] -> [a]")
    ("dropWhile" . "(a -> Bool) -> [a] -> [a]")
    ("either" . "(a -> c) -> (b -> c) -> Either a b -> c")
    ("elem" . "(Eq a) => a -> [a] -> Bool")
    ("encodeFloat" . "RealFloat a => Integer -> Int -> a")
    ("enumFrom" . "Enum a => a -> [a]")
    ("enumFromThen" . "Enum a => a -> a -> [a]")
    ("enumFromThenTo" . "Enum a => a -> a -> a -> [a]")
    ("enumFromTo" . "Enum a => a -> a -> [a]")
    ("error" . "String -> a")
    ("even" . "(Integral a) => a -> Bool")
    ("exp" . "Floating a => a -> a")
    ("exponent" . "RealFloat a => a -> Int")
    ("fail" . "Monad m => String -> m a")
    ("filter" . "(a -> Bool) -> [a] -> [a]")
    ("flip" . "(a -> b -> c) -> b -> a -> c")
    ("floatDigits" . "RealFloat a => a -> Int")
    ("floatRadix" . "RealFloat a => a -> Integer")
    ("floatRange" . "RealFloat a => a -> (Int,Int)")
    ("floor" . "(RealFrac a, Integral b) => a -> b")
    ("fmap" . "Functor f => (a -> b) -> f a -> f b")
    ("foldl" . "(a -> b -> a) -> a -> [b] -> a")
    ("foldl1" . "(a -> a -> a) -> [a] -> a")
    ("foldr" . "(a -> b -> b) -> b -> [a] -> b")
    ("foldr1" . "(a -> a -> a) -> [a] -> a")
    ("fromEnum" . "Enum a => a -> Int")
    ("fromInteger" . "Num a => Integer -> a")
    ("fromIntegral" . "(Integral a, Num b) => a -> b")
    ("fromRational" . "Fractional a => Rational -> a")
    ("fst" . "(a,b) -> a")
    ("gcd" . "(Integral a) => a -> a -> a")
    ("getChar" . "IO Char")
    ("getContents" . "IO String")
    ("getLine" . "IO String")
    ("head" . "[a] -> a")
    ("id" . "a -> a")
    ("init" . "[a] -> [a]")
    ("interact" . "(String -> String) -> IO ()")
    ("ioError" . "IOError -> IO a")
    ("isDenormalized" . "RealFloat a => a -> Bool")
    ("isIEEE" . "RealFloat a => a -> Bool")
    ("isInfinite" . "RealFloat a => a -> Bool")
    ("isNaN" . "RealFloat a => a -> Bool")
    ("isNegativeZero" . "RealFloat a => a -> Bool")
    ("iterate" . "(a -> a) -> a -> [a]")
    ("last" . "[a] -> a")
    ("lcm" . "(Integral a) => a -> a -> a")
    ("length" . "[a] -> Int")
    ("lex" . "ReadS String")
    ("lines" . "String -> [String]")
    ("log" . "Floating a => a -> a")
    ("logBase" . "Floating a => a -> a -> a")
    ("lookup" . "(Eq a) => a -> [(a,b)] -> Maybe b")
    ("map" . "(a -> b) -> [a] -> [b]")
    ("mapM" . "Monad m => (a -> m b) -> [a] -> m [b]")
    ("mapM_" . "Monad m => (a -> m b) -> [a] -> m ()")
    ("max" . "Ord a => a -> a -> a")
    ("maxBound" . "Bounded a => a")
    ("maximum" . "(Ord a) => [a] -> a")
    ("maybe" . "b -> (a -> b) -> Maybe a -> b")
    ("min" . "Ord a => a -> a -> a")
    ("minBound" . "Bounded a => a")
    ("minimum" . "(Ord a) => [a] -> a")
    ("mod" . "Integral a => a -> a -> a")
    ("negate" . "Num a => a -> a")
    ("not" . "Bool -> Bool")
    ("notElem" . "(Eq a) => a -> [a] -> Bool")
    ("null" . "[a] -> Bool")
    ("numericEnumFrom" . "(Fractional a) => a -> [a]")
    ("numericEnumFromThen" . "(Fractional a) => a -> a -> [a]")
    ("numericEnumFromThenTo" . "(Fractional a, Ord a) => a -> a -> a -> [a]")
    ("numericEnumFromTo" . "(Fractional a, Ord a) => a -> a -> [a]")
    ("odd" . "(Integral a) => a -> Bool")
    ("or" . "[Bool] -> Bool")
    ("otherwise" . "Bool")
    ("pi" . "Floating a => a")
    ("pred" . "Enum a => a -> a")
    ("print" . "Show a => a -> IO ()")
    ("product" . "(Num a) => [a] -> a")
    ("properFraction" . "(RealFrac a, Integral b) => a -> (b,a)")
    ("putChar" . "Char -> IO ()")
    ("putStr" . "String -> IO ()")
    ("putStrLn" . "String -> IO ()")
    ("quot" . "Integral a => a -> a -> a")
    ("quotRem" . "Integral a => a -> a -> (a,a)")
    ("read" . "(Read a) => String -> a")
    ("readFile" . "FilePath -> IO String")
    ("readIO" . "Read a => String -> IO a")
    ("readList" . "Read a => ReadS [a]")
    ("readLn" . "Read a => IO a")
    ("readParen" . "Bool -> ReadS a -> ReadS a")
    ("reads" . "(Read a) => ReadS a")
    ("readsPrec" . "Read a => Int -> ReadS a")
    ("realToFrac" . "(Real a, Fractional b) => a -> b")
    ("recip" . "Fractional a => a -> a")
    ("rem" . "Integral a => a -> a -> a")
    ("repeat" . "a -> [a]")
    ("replicate" . "Int -> a -> [a]")
    ("return" . "Monad m => a -> m a")
    ("reverse" . "[a] -> [a]")
    ("round" . "(RealFrac a, Integral b) => a -> b")
    ("scaleFloat" . "RealFloat a => Int -> a -> a")
    ("scanl" . "(a -> b -> a) -> a -> [b] -> [a]")
    ("scanl1" . "(a -> a -> a) -> [a] -> [a]")
    ("scanr" . "(a -> b -> b) -> b -> [a] -> [b]")
    ("scanr1" . "(a -> a -> a) -> [a] -> [a]")
    ("seq" . "a -> b -> b")
    ("sequence" . "Monad m => [m a] -> m [a]")
    ("sequence_" . "Monad m => [m a] -> m ()")
    ("show" . "Show a => a -> String")
    ("showChar" . "Char -> ShowS")
    ("showList" . "Show a => [a] -> ShowS")
    ("showParen" . "Bool -> ShowS -> ShowS")
    ("showString" . "String -> ShowS")
    ("shows" . "(Show a) => a -> ShowS")
    ("showsPrec" . "Show a => Int -> a -> ShowS")
    ("significand" . "RealFloat a => a -> a")
    ("signum" . "Num a => a -> a")
    ("sin" . "Floating a => a -> a")
    ("sinh" . "Floating a => a -> a")
    ("snd" . "(a,b) -> b")
    ("span" . "(a -> Bool) -> [a] -> ([a],[a])")
    ("splitAt" . "Int -> [a] -> ([a],[a])")
    ("sqrt" . "Floating a => a -> a")
    ("subtract" . "(Num a) => a -> a -> a")
    ("succ" . "Enum a => a -> a")
    ("sum" . "(Num a) => [a] -> a")
    ("tail" . "[a] -> [a]")
    ("take" . "Int -> [a] -> [a]")
    ("takeWhile" . "(a -> Bool) -> [a] -> [a]")
    ("tan" . "Floating a => a -> a")
    ("tanh" . "Floating a => a -> a")
    ("toEnum" . "Enum a => Int -> a")
    ("toInteger" . "Integral a => a -> Integer")
    ("toRational" . "Real a => a -> Rational")
    ("truncate" . "(RealFrac a, Integral b) => a -> b")
    ("uncurry" . "(a -> b -> c) -> ((a, b) -> c)")
    ("undefined" . "a")
    ("unlines" . "[String] -> String")
    ("until" . "(a -> Bool) -> (a -> a) -> a -> a")
    ("unwords" . "[String] -> String")
    ("unzip" . "[(a,b)] -> ([a],[b])")
    ("unzip3" . "[(a,b,c)] -> ([a],[b],[c])")
    ("userError" . "String -> IOError")
    ("words" . "String -> [String]")
    ("writeFile" . "FilePath -> String -> IO ()")
    ("zip" . "[a] -> [b] -> [(a,b)]")
    ("zip3" . "[a] -> [b] -> [c] -> [(a,b,c)]")
    ("zipWith" . "(a->b->c) -> [a]->[b]->[c]")
    ("zipWith3" . "(a->b->c->d) -> [a]->[b]->[c]->[d]")
    ("||" . "Bool -> Bool -> Bool")
    ;; Ratio
    ("%" . "(Integral a) => a -> a -> Ratio a")
    ("approxRational" . "(RealFrac a) => a -> a -> Rational")
    ("denominator" . "(Integral a) => Ratio a -> a")
    ("numerator" . "(Integral a) => Ratio a -> a")
    ;; Complex
    ("cis" . "(RealFloat a) => a -> Complex a")
    ("conjugate" . "(RealFloat a) => Complex a -> Complex a")
    ("imagPart" . "(RealFloat a) => Complex a -> a")
    ("magnitude" . "(RealFloat a) => Complex a -> a")
    ("mkPolar" . "(RealFloat a) => a -> a -> Complex a")
    ("phase" . "(RealFloat a) => Complex a -> a")
    ("polar" . "(RealFloat a) => Complex a -> (a,a)")
    ("realPart" . "(RealFloat a) => Complex a -> a")
    ;; Numeric
    ("floatToDigits" . "(RealFloat a) => Integer -> a -> ([Int], Int)")
    ("fromRat" . "(RealFloat a) => Rational -> a")
    ("lexDigits" . "ReadS String")
    ("readDec" . "(Integral a) => ReadS a")
    ("readFloat" . "(RealFrac a) => ReadS a")
    ("readHex" . "(Integral a) => ReadS a")
    ("readInt" . "(Integral a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a")
    ("readOct" . "(Integral a) => ReadS a")
    ("readSigned" . "(Real a) => ReadS a -> ReadS a")
    ("showEFloat" . "(RealFloat a) => Maybe Int -> a -> ShowS")
    ("showFFloat" . "(RealFloat a) => Maybe Int -> a -> ShowS")
    ("showFloat" . "(RealFloat a) => a -> ShowS")
    ("showGFloat" . "(RealFloat a) => Maybe Int -> a -> ShowS")
    ("showHex" . "Integral a => a -> ShowS")
    ("showInt" . "Integral a => a -> ShowS")
    ("showIntAtBase" . "Integral a => a -> (Int -> Char) -> a -> ShowS")
    ("showOct" . "Integral a => a -> ShowS")
    ("showSigned" . "(Real a) => (a -> ShowS) -> Int -> a -> ShowS")
    ;; Ix
    ("inRange" . "Ix a => (a,a) -> a -> Bool")
    ("index" . "Ix a => (a,a) -> a -> Int")
    ("range" . "Ix a => (a,a) -> [a]")
    ("rangeSize" . "Ix a => (a,a) -> Int")
    ;; Array
    ("!" . "(Ix a) => Array a b -> a -> b")
    ("//" . "(Ix a) => Array a b -> [(a,b)] -> Array a b")
    ("accum" . "(Ix a) => (b -> c -> b) -> Array a b -> [(a,c)]")
    ("accumArray" . "(Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)]")
    ("array" . "(Ix a) => (a,a) -> [(a,b)] -> Array a b")
    ("assocs" . "(Ix a) => Array a b -> [(a,b)]")
    ("bounds" . "(Ix a) => Array a b -> (a,a)")
    ("elems" . "(Ix a) => Array a b -> [b]")
    ("indices" . "(Ix a) => Array a b -> [a]")
    ("ixmap" . "(Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c")
    ("listArray" . "(Ix a) => (a,a) -> [b] -> Array a b")
    ;; List
    ("\\\\" . "Eq a => [a] -> [a] -> [a]")
    ("delete" . "Eq a => a -> [a] -> [a]")
    ("deleteBy" . "(a -> a -> Bool) -> a -> [a] -> [a]")
    ("deleteFirstsBy" . "(a -> a -> Bool) -> [a] -> [a] -> [a]")
    ("elemIndex" . "Eq a => a -> [a] -> Maybe Int")
    ("elemIndices" . "Eq a => a -> [a] -> [Int]")
    ("find" . "(a -> Bool) -> [a] -> Maybe a")
    ("findIndex" . "(a -> Bool) -> [a] -> Maybe Int")
    ("findIndices" . "(a -> Bool) -> [a] -> [Int]")
    ("genericDrop" . "Integral a => a -> [b] -> [b]")
    ("genericIndex" . "Integral a => [b] -> a -> b")
    ("genericLength" . "Integral a => [b] -> a")
    ("genericReplicate" . "Integral a => a -> b -> [b]")
    ("genericSplitAt" . "Integral a => a -> [b] -> ([b],[b])")
    ("genericTake" . "Integral a => a -> [b] -> [b]")
    ("group" . "Eq a => [a] -> [[a]]")
    ("groupBy" . "(a -> a -> Bool) -> [a] -> [[a]]")
    ("inits" . "[a] -> [[a]]")
    ("insert" . "Ord a => a -> [a] -> [a]")
    ("insertBy" . "(a -> a -> Ordering) -> a -> [a] -> [a]")
    ("intersect" . "Eq a => [a] -> [a] -> [a]")
    ("intersectBy" . "(a -> a -> Bool) -> [a] -> [a] -> [a]")
    ("intersperse" . "a -> [a] -> [a]")
    ("isPrefixOf" . "Eq a => [a] -> [a] -> Bool")
    ("isSuffixOf" . "Eq a => [a] -> [a] -> Bool")
    ("mapAccumL" . "(a -> b -> (a, c)) -> a -> [b] -> (a, [c])")
    ("mapAccumR" . "(a -> b -> (a, c)) -> a -> [b] -> (a, [c])")
    ("maximumBy" . "(a -> a -> Ordering) -> [a] -> a")
    ("minimumBy" . "(a -> a -> Ordering) -> [a] -> a")
    ("nub" . "Eq a => [a] -> [a]")
    ("nubBy" . "(a -> a -> Bool) -> [a] -> [a]")
    ("partition" . "(a -> Bool) -> [a] -> ([a],[a])")
    ("sort" . "Ord a => [a] -> [a]")
    ("sortBy" . "(a -> a -> Ordering) -> [a] -> [a]")
    ("tails" . "[a] -> [[a]]")
    ("transpose" . "[[a]] -> [[a]]")
    ("unfoldr" . "(b -> Maybe (a,b)) -> b -> [a]")
    ("union" . "Eq a => [a] -> [a] -> [a]")
    ("unionBy" . "(a -> a -> Bool) -> [a] -> [a] -> [a]")
    ("unzip4" . "[(a,b,c,d)] -> ([a],[b],[c],[d])")
    ("unzip5" . "[(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])")
    ("unzip6" . "[(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])")
    ("unzip7" . "[(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])")
    ("zip4" . "[a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]")
    ("zip5" . "[a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]")
    ("zip6" . "[a] -> [b] -> [c] -> [d] -> [e] -> [f]")
    ("zip7" . "[a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]")
    ("zipWith4" . "(a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]")
    ("zipWith5" . "(a->b->c->d->e->f) ->")
    ("zipWith6" . "(a->b->c->d->e->f->g) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]")
    ("zipWith7" . "(a->b->c->d->e->f->g->h) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]")
    ;; Maybe
    ("catMaybes" . "[Maybe a] -> [a]")
    ("fromJust" . "Maybe a -> a")
    ("fromMaybe" . "a -> Maybe a -> a")
    ("isJust" . "Maybe a -> Bool")
    ("isNothing" . "Maybe a -> Bool")
    ("listToMaybe" . "[a] -> Maybe a")
    ("mapMaybe" . "(a -> Maybe b) -> [a] -> [b]")
    ("maybeToList" . "Maybe a -> [a]")
    ;; Char
    ("chr" . "Int -> Char")
    ("digitToInt" . "Char -> Int")
    ("intToDigit" . "Int -> Char")
    ("isAlpha" . "Char -> Bool")
    ("isAlphaNum" . "Char -> Bool")
    ("isAscii" . "Char -> Bool")
    ("isControl" . "Char -> Bool")
    ("isDigit" . "Char -> Bool")
    ("isHexDigit" . "Char -> Bool")
    ("isLatin1" . "Char -> Bool")
    ("isLower" . "Char -> Bool")
    ("isOctDigit" . "Char -> Bool")
    ("isPrint" . "Char -> Bool")
    ("isSpace" . "Char -> Bool")
    ("isUpper" . "Char -> Bool")
    ("lexLitChar" . "ReadS String")
    ("ord" . "Char -> Int")
    ("readLitChar" . "ReadS Char")
    ("showLitChar" . "Char -> ShowS")
    ("toLower" . "Char -> Char")
    ("toUpper" . "Char -> Char")
    ;; Monad
    ("ap" . "Monad m => m (a -> b) -> m a -> m b")
    ("filterM" . "Monad m => (a -> m Bool) -> [a] -> m [a]")
    ("foldM" . "Monad m => (a -> b -> m a) -> a -> [b] -> m a")
    ("guard" . "MonadPlus m => Bool -> m ()")
    ("join" . "Monad m => m (m a) -> m a")
    ("liftM" . "Monad m => (a -> b) -> (m a -> m b)")
    ("liftM2" . "Monad m => (a -> b -> c) -> (m a -> m b -> m c)")
    ("liftM3" . "Monad m => (a -> b -> c -> d) -> (m a -> m b -> m c -> m d)")
    ("liftM4" . "Monad m => (a -> b -> c -> d -> e) -> (m a -> m b -> m c -> m d -> m e)")
    ("liftM5" . "Monad m => (a -> b -> c -> d -> e -> f) -> (m a -> m b -> m c -> m d -> m e -> m f)")
    ("mapAndUnzipM" . "Monad m => (a -> m (b,c)) -> [a] -> m ([b], [c])")
    ("mplus" . "MonadPlus m => m a -> m a -> m a")
    ("msum" . "MonadPlus m => [m a] -> m a")
    ("mzero" . "MonadPlus m => m a")
    ("unless" . "Monad m => Bool -> m () -> m ()")
    ("when" . "Monad m => Bool -> m () -> m ()")
    ("zipWithM" . "Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]")
    ("zipWithM_" . "Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()")
    ;; IO
    ("bracket" . "IO a -> (a -> IO b) -> (a -> IO c) -> IO c")
    ("bracket_" . "IO a -> (a -> IO b) -> IO c -> IO c")
    ("hClose" . "Handle -> IO ()")
    ("hFileSize" . "Handle -> IO Integer")
    ("hFlush" . "Handle -> IO ()")
    ("hGetBuffering" . "Handle -> IO BufferMode")
    ("hGetChar" . "Handle -> IO Char")
    ("hGetContents" . "Handle -> IO String")
    ("hGetLine" . "Handle -> IO String")
    ("hGetPosn" . "Handle -> IO HandlePosn")
    ("hIsClosed" . "Handle -> IO Bool")
    ("hIsEOF" . "Handle -> IO Bool")
    ("hIsOpen" . "Handle -> IO Bool")
    ("hIsReadable" . "Handle -> IO Bool")
    ("hIsSeekable" . "Handle -> IO Bool")
    ("hIsWritable" . "Handle -> IO Bool")
    ("hLookAhead" . "Handle -> IO Char")
    ("hPrint" . "Show a => Handle -> a -> IO ()")
    ("hPutChar" . "Handle -> Char -> IO ()")
    ("hPutStr" . "Handle -> String -> IO ()")
    ("hPutStrLn" . "Handle -> String -> IO ()")
    ("hReady" . "Handle -> IO Bool")
    ("hSeek" . "Handle -> SeekMode -> Integer -> IO ()")
    ("hSetBuffering" . "Handle -> BufferMode -> IO ()")
    ("hSetPosn" . "HandlePosn -> IO ()")
    ("hWaitForInput" . "Handle -> Int -> IO Bool")
    ("ioeGetErrorString" . "IOError -> String")
    ("ioeGetFileName" . "IOError -> Maybe FilePath")
    ("ioeGetHandle" . "IOError -> Maybe Handle")
    ("isAlreadyExistsError" . "IOError -> Bool")
    ("isAlreadyInUseError" . "IOError -> Bool")
    ("isDoesNotExistError" . "IOError -> Bool")
    ("isEOF" . "IO Bool")
    ("isEOFError" . "IOError -> Bool")
    ("isFullError" . "IOError -> Bool")
    ("isIllegalOperation" . "IOError -> Bool")
    ("isPermissionError" . "IOError -> Bool")
    ("isUserError" . "IOError -> Bool")
    ("openFile" . "FilePath -> IOMode -> IO Handle")
    ("stderr" . "Handle")
    ("stdin" . "Handle")
    ("stdout" . "Handle")
    ("try" . "IO a -> IO (Either IOError a)")
    ;; Directory
    ("createDirectory" . "FilePath -> IO ()")
    ("doesDirectoryExist" . "FilePath -> IO Bool")
    ("doesFileExist" . "FilePath -> IO Bool")
    ("executable" . "Permissions -> Bool")
    ("getCurrentDirectory" . "IO FilePath")
    ("getDirectoryContents" . "FilePath -> IO [FilePath]")
    ("getModificationTime" . "FilePath -> IO ClockTime")
    ("getPermissions" . "FilePath -> IO Permissions")
    ("readable" . "Permissions -> Bool")
    ("removeDirectory" . "FilePath -> IO ()")
    ("removeFile" . "FilePath -> IO ()")
    ("renameDirectory" . "FilePath -> FilePath -> IO ()")
    ("renameFile" . "FilePath -> FilePath -> IO ()")
    ("searchable" . "Permissions -> Bool")
    ("setCurrentDirectory" . "FilePath -> IO ()")
    ("setPermissions" . "FilePath -> Permissions -> IO ()")
    ("writable" . "Permissions -> Bool")
    ;; System
    ("exitFailure" . "IO a")
    ("exitWith" . "ExitCode -> IO a")
    ("getArgs" . "IO [String]")
    ("getEnv" . "String -> IO String")
    ("getProgName" . "IO String")
    ("system" . "String -> IO ExitCode")
    ;; Time
    ("addToClockTime" . "TimeDiff -> ClockTime -> ClockTime")
    ("calendarTimeToString" . "CalendarTime -> String")
    ("ctDay" . "CalendarTime -> Int")
    ("ctHour" . "CalendarTime -> Int")
    ("ctIsDST" . "CalendarTime -> Bool")
    ("ctMin" . "CalendarTime -> Int")
    ("ctMonth" . "CalendarTime -> Month")
    ("ctPicosec" . "CalendarTime -> Integer")
    ("ctSec" . "CalendarTime -> Int")
    ("ctTZ" . "CalendarTime -> Int")
    ("ctTZName" . "CalendarTime -> String")
    ("ctWDay" . "CalendarTime -> Day")
    ("ctYDay" . "CalendarTime -> Int")
    ("ctYear" . "CalendarTime -> Int")
    ("diffClockTimes" . "ClockTime -> ClockTime -> TimeDiff")
    ("formatCalendarTime" . "TimeLocale -> String -> CalendarTime -> String")
    ("getClockTime" . "IO ClockTime")
    ("tdDay" . "TimeDiff -> Int")
    ("tdHour" . "TimeDiff -> Int")
    ("tdMin" . "TimeDiff -> Int")
    ("tdMonth" . "TimeDiff -> Int")
    ("tdPicosec" . "TimeDiff -> Integer")
    ("tdSec" . "TimeDiff -> Int")
    ("tdYear" . "TimeDiff -> Int")
    ("toCalendarTime" . "ClockTime -> IO CalendarTime")
    ("toClockTime" . "CalendarTime -> ClockTime")
    ("toUTCTime" . "ClockTime -> CalendarTime")
    ;; Locale
    ("amPm" . "TimeLocale -> (String, String)")
    ("dateFmt" . "TimeLocale -> String")
    ("dateTimeFmt" . "TimeLocale -> String")
    ("defaultTimeLocale" . "TimeLocale")
    ("months" . "TimeLocale -> [(String, String)]")
    ("time12Fmt" . "TimeLocale -> String")
    ("timeFmt" . "TimeLocale -> String")
    ("wDays" . "TimeLocale -> [(String, String)]")
    ;; CPUTime
    ("cpuTimePrecision" . "Integer")
    ("getCPUTime" . "IO Integer")
    ;; Random
    ("genRange" . "RandomGen g => g -> (Int, Int)")
    ("getStdGen" . "IO StdGen")
    ("getStdRandom" . "(StdGen -> (a, StdGen)) -> IO a")
    ("mkStdGen" . "Int -> StdGen")
    ("newStdGen" . "IO StdGen")
    ("next" . "RandomGen g => g -> (Int, g)")
    ("random" . "(Random a, RandomGen g) => g -> (a, g)")
    ("randomIO" . "Random a => IO a")
    ("randomR" . "(Random a, RandomGen g) => (a, a) -> g -> (a, g)")
    ("randomRIO" . "Random a => (a,a) -> IO a")
    ("randomRs" . "(Random a, RandomGen g) => (a, a) -> g -> [a]")
    ("randoms" . "(Random a, RandomGen g) => g -> [a]")
    ("setStdGen" . "StdGen -> IO ()")
    ("split" . "RandomGen g => g -> (g, g)")
    )
  "Alist of prelude functions and their types.")


(defvar haskell-doc-strategy-ids
  (list
   '("par"  . "Done -> Done -> Done ; [infixr 0]")
   '("seq"  . "Done -> Done -> Done ; [infixr 1]")

   '("using"      . "a -> Strategy a -> a ; [infixl 0]")
   '("demanding"  . "a -> Done -> a ; [infixl 0]")
   '("sparking"   . "a -> Done -> a ; [infixl 0]")

   '(">||" . "Done -> Done -> Done ; [infixr 2]")
   '(">|" .  "Done -> Done -> Done ; [infixr 3]")
   '("$||" . "(a -> b) -> Strategy a -> a -> b ; [infixl 6]")
   '("$|"  . "(a -> b) -> Strategy a -> a -> b ; [infixl 6]")
   '(".|"  . "(b -> c) -> Strategy b -> (a -> b) -> (a -> c) ; [infixl 9]")
   '(".||" . "(b -> c) -> Strategy b -> (a -> b) -> (a -> c) ; [infixl 9]")
   '("-|"  . "(a -> b) -> Strategy b -> (b -> c) -> (a -> c) ; [infixl 9]")
   '("-||" . "(a -> b) -> Strategy b -> (b -> c) -> (a -> c) ; [infixl 9]")

   '("Done" . "type Done = ()")
   '("Strategy" . "type Strategy a = a -> Done")

   '("r0"    . "Strategy a")
   '("rwhnf" . "Eval a => Strategy a")
   '("rnf" . "Strategy a")
   '("NFData" . "class Eval a => NFData a where rnf :: Strategy a")
   '("NFDataIntegral" ."class (NFData a, Integral a) => NFDataIntegral a")
   '("NFDataOrd" . "class (NFData a, Ord a) => NFDataOrd a")

   '("markStrat" . "Int -> Strategy a -> Strategy a")

   '("seqPair" . "Strategy a -> Strategy b -> Strategy (a,b)")
   '("parPair" . "Strategy a -> Strategy b -> Strategy (a,b)")
   '("seqTriple" . "Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)")
   '("parTriple" . "Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)")

   '("parList"  . "Strategy a -> Strategy [a]")
   '("parListN"  . "(Integral b) => b -> Strategy a -> Strategy [a]")
   '("parListNth"  . "Int -> Strategy a -> Strategy [a]")
   '("parListChunk"  . "Int -> Strategy a -> Strategy [a]")
   '("parMap"  . "Strategy b -> (a -> b) -> [a] -> [b]")
   '("parFlatMap"  . "Strategy [b] -> (a -> [b]) -> [a] -> [b]")
   '("parZipWith"  . "Strategy c -> (a -> b -> c) -> [a] -> [b] -> [c]")
   '("seqList"  . "Strategy a -> Strategy [a]")
   '("seqListN"  . "(Integral a) => a -> Strategy b -> Strategy [b]")
   '("seqListNth"  . "Int -> Strategy b -> Strategy [b]")

   '("parBuffer"  . "Int -> Strategy a -> [a] -> [a]")

   '("seqArr"  . "(Ix b) => Strategy a -> Strategy (Array b a)")
   '("parArr"  . "(Ix b) => Strategy a -> Strategy (Array b a)")

   '("fstPairFstList"  . "(NFData a) => Strategy [(a,b)]")
   '("force"  . "(NFData a) => a -> a ")
   '("sforce"  . "(NFData a) => a -> b -> b")
   )
  "Alist of strategy functions and their types as defined in Strategies.lhs.")

(defvar haskell-doc-user-defined-ids nil
  "Alist of functions and strings defined by the user.")


(defsubst haskell-doc-is-of (fn types)
  "Check whether FN is one of the functions in the alist TYPES and return the type."
  (assoc fn types) )


;; Put this minor mode on the global minor-mode-alist.
(or (assq 'haskell-doc-mode (default-value 'minor-mode-alist))
    (setq-default minor-mode-alist
                  (append (default-value 'minor-mode-alist)
                          '((haskell-doc-mode haskell-doc-minor-mode-string)))))


(defvar haskell-doc-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [visit]
      '("Visit FTP home site" . haskell-doc-visit-home))
    (define-key map [submit]
      '("Submit bug report" . haskell-doc-submit-bug-report))
    (define-key map [dummy] '("---" . nil))
    (define-key map [make-index]
      '("Make global fct index" . haskell-doc-make-global-fct-index))
    (define-key map [global-types-on]
      '("Toggle display of global types" . haskell-doc-show-global-types))
    (define-key map [strategy-on]
      '("Toggle display of strategy ids" . haskell-doc-show-strategy))
    (define-key map [user-defined-on]
      '("Toggle display of user defined ids" . haskell-doc-show-user-defined))
    (define-key map [prelude-on]
      '("Toggle display of prelude functions" . haskell-doc-show-prelude))
    (define-key map [reserved-ids-on]
      '("Toggle display of reserved ids" . haskell-doc-show-reserved))
    (define-key map [haskell-doc-on]
      '("Toggle haskell-doc mode" . haskell-doc-mode))
    map))

(defun haskell-doc-install-keymap ()
  "Install a menu for `haskell-doc-mode' as a submenu of \"Hugs\"."
  (interactive)
  ;; Add the menu to the hugs menu as last entry.
  (let ((hugsmap (lookup-key (current-local-map) [menu-bar Hugs])))
    (if (not (or (featurep 'xemacs) ; XEmacs has problems here
                 (not (keymapp hugsmap))
                 (lookup-key hugsmap [haskell-doc])))
        (if (functionp 'define-key-after)
            (define-key-after hugsmap [haskell-doc]
              (cons "Haskell-doc" haskell-doc-keymap)
              [Haskell-doc mode]))))
  ;; Add shortcuts for these commands.
  (local-set-key "\C-c\e/" 'haskell-doc-check-active)
  ;; Conflicts with the binding of haskell-insert-otherwise.
  ;; (local-set-key "\C-c\C-o" 'haskell-doc-mode)
  (local-set-key [(control shift meta mouse-3)]
                 'haskell-doc-ask-mouse-for-type))


(defvar haskell-doc-timer nil)
(defvar haskell-doc-buffers nil)

;;;###autoload
(defun haskell-doc-mode (&optional arg)
  "Enter `haskell-doc-mode' for showing fct types in the echo area.
See variable docstring."
  (interactive (list (or current-prefix-arg 'toggle)))

  (setq haskell-doc-mode
        (cond
         ((eq arg 'toggle) (not haskell-doc-mode))
         (arg (> (prefix-numeric-value arg) 0))
         (t)))

  ;; First, unconditionally turn the mode OFF.

  (setq haskell-doc-buffers (delq (current-buffer) haskell-doc-buffers))
  ;; Refresh the buffers list.
  (dolist (buf haskell-doc-buffers)
    (unless (and (buffer-live-p buf)
                 (with-current-buffer buf haskell-doc-mode))
      (setq haskell-doc-buffers (delq buf haskell-doc-buffers))))
  ;; Turn off the idle timer (or idle post-command-hook).
  (when (and haskell-doc-timer (null haskell-doc-buffers))
    (cancel-timer haskell-doc-timer)
    (setq haskell-doc-timer nil))
  (remove-hook 'post-command-hook
               'haskell-doc-mode-print-current-symbol-info 'local)

  (when haskell-doc-mode
    ;; Turning the mode ON.
    (push (current-buffer) haskell-doc-buffers)

    (if (fboundp 'run-with-idle-timer)
        (unless haskell-doc-timer
          (setq haskell-doc-timer
                (run-with-idle-timer
                 haskell-doc-idle-delay t
                 'haskell-doc-mode-print-current-symbol-info)))
      (add-hook 'post-command-hook
                'haskell-doc-mode-print-current-symbol-info nil 'local))
    (and haskell-doc-show-global-types
         (haskell-doc-make-global-fct-index)) ; build type index for global fcts

    (haskell-doc-install-keymap)

    (run-hooks 'haskell-doc-mode-hook))

  (and (called-interactively-p 'any)
       (message "haskell-doc-mode is %s"
                (if haskell-doc-mode "enabled" "disabled")))
  haskell-doc-mode)

(defmacro haskell-doc-toggle-var (id prefix)
  ;; toggle variable or set it based on prefix value
  `(setq ,id
         (if ,prefix
             (>= (prefix-numeric-value ,prefix) 0)
           (not ,id))) )

(defun haskell-doc-show-global-types (&optional prefix)
  "Turn on global types information in `haskell-doc-mode'."
  (interactive "P")
  (haskell-doc-toggle-var haskell-doc-show-global-types prefix)
  (if haskell-doc-show-global-types
      (haskell-doc-make-global-fct-index)))

(defun haskell-doc-show-reserved (&optional prefix)
  "Toggle the automatic display of a doc string for reserved ids."
  (interactive "P")
  (haskell-doc-toggle-var haskell-doc-show-reserved prefix))

(defun haskell-doc-show-prelude (&optional prefix)
  "Toggle the automatic display of a doc string for reserved ids."
  (interactive "P")
  (haskell-doc-toggle-var haskell-doc-show-prelude prefix))

(defun haskell-doc-show-strategy (&optional prefix)
  "Toggle the automatic display of a doc string for strategy ids."
  (interactive "P")
  (haskell-doc-toggle-var haskell-doc-show-strategy prefix))

(defun haskell-doc-show-user-defined (&optional prefix)
  "Toggle the automatic display of a doc string for user defined ids."
  (interactive "P")
  (haskell-doc-toggle-var haskell-doc-show-user-defined prefix))


;;;###autoload
(defalias 'turn-on-haskell-doc-mode 'haskell-doc-mode)
(make-obsolete 'turn-on-haskell-doc-mode
               'haskell-doc-mode
               "2015-07-23")

;;;###autoload
(defalias 'turn-on-haskell-doc 'haskell-doc-mode)
(make-obsolete 'turn-on-haskell-doc
               'haskell-doc-mode
               "2015-07-23")

(defalias 'turn-off-haskell-doc-mode 'turn-off-haskell-doc)

(defun turn-off-haskell-doc ()
  "Unequivocally turn off `haskell-doc-mode' (which see)."
  (haskell-doc-mode 0))

(defun haskell-doc-check-active ()
  "Check whether the print function is hooked in.
Should be the same as the value of `haskell-doc-mode' but alas currently it
is not."
  (interactive)
  (message "%s"
           (if (or (and haskell-doc-mode haskell-doc-timer)
                   (memq 'haskell-doc-mode-print-current-symbol-info
                         post-command-hook))
               "haskell-doc is ACTIVE"
             (substitute-command-keys
              "haskell-doc is not ACTIVE \(Use \\[haskell-doc-mode] to turn it on\)"))))


;; This is the function hooked into the elisp command engine
(defun haskell-doc-mode-print-current-symbol-info ()
  "Print the type of the symbol under the cursor.

This function is run by an idle timer to print the type
 automatically if `haskell-doc-mode' is turned on."
  (and haskell-doc-mode
       (haskell-doc-in-code-p)
       (not haskell-mode-interactive-prompt-state)
       (not (eobp))
       (not executing-kbd-macro)
       ;; Having this mode operate in the minibuffer makes it impossible to
       ;; see what you're doing.
       (not (eq (selected-window) (minibuffer-window)))
       ;; not in string or comment
       ;; take a nap, if run straight from post-command-hook.
       (if (fboundp 'run-with-idle-timer) t
         (sit-for haskell-doc-idle-delay))
       ;; good morning! read the word under the cursor for breakfast
       (haskell-doc-show-type)))
;; ;; ToDo: find surrounding fct
;; (cond ((eq current-symbol current-fnsym)
;;        (haskell-doc-show-type current-fnsym))
;;       (t
;;        (or nil ; (haskell-doc-print-var-docstring current-symbol)
;;            (haskell-doc-show-type current-fnsym)))))))

;;;###autoload
(defun haskell-doc-current-info ()
  "Return the info about symbol at point.
Meant for `eldoc-documentation-function'."
  ;; There are a number of possible documentation functions.
  ;; Some of them are asynchronous.
  (when (haskell-doc-in-code-p)
    (let ((msg (or
                (haskell-doc-current-info--interaction)
                (haskell-doc-sym-doc (haskell-ident-at-point)))))
      (unless (symbolp msg) msg))))

(defun haskell-doc-ask-mouse-for-type (event)
  "Read the identifier under the mouse and echo its type.
This uses the same underlying function `haskell-doc-show-type' as the hooked
function.  Only the user interface is different."
  (interactive "e")
  (save-excursion
    (select-window (posn-window (event-end event)))
    (goto-char (posn-point (event-end event)))
    (haskell-doc-show-type)))

(defun haskell-doc-in-code-p ()
  "A predicate indicating suitable case to show docs."
  (not (or (and (eq haskell-literate 'bird)
                ;; Copied from haskell-indent-bolp.
                (<= (current-column) 2)
                (eq (char-after (line-beginning-position)) ?\>))
           (nth 8 (syntax-ppss)))))

;;;###autoload
(defun haskell-doc-show-type (&optional sym)
  "Show the type of the function near point or given symbol SYM.
For the function under point, show the type in the echo area.
This information is extracted from the `haskell-doc-prelude-types' alist
of prelude functions and their types, or from the local functions in the
current buffer."
  (interactive)
  (unless sym (setq sym (haskell-ident-at-point)))
  ;; if printed before do not print it again
  (unless (string= sym (car haskell-doc-last-data))
    (let ((doc (or (haskell-doc-current-info--interaction t)
                  (haskell-doc-sym-doc sym))))
      (when (and doc (haskell-doc-in-code-p))
        ;; In Emacs 19.29 and later, and XEmacs 19.13 and later, all
        ;; messages are recorded in a log.  Do not put haskell-doc messages
        ;; in that log since they are legion.
        (let ((message-log-max nil))
          (message "%s" doc))))))

(defvar haskell-doc-current-info--interaction-last nil
  "Async message stack.
If non-nil, a previous eldoc message from an async call, that
hasn't been displayed yet.")

(defun haskell-doc-current-info--interaction (&optional sync)
  "Asynchronous call to `haskell-process-get-type'.
Suitable for use in the eldoc function `haskell-doc-current-info'.

If SYNC is non-nil, the call will be synchronous instead, and
instead of calling `eldoc-print-current-symbol-info', the result
will be returned directly."
  ;; Return nil if nothing is available, or 'async if something might
  ;; be available, but asynchronously later. This will call
  ;; `eldoc-print-current-symbol-info' later.
  (when (haskell-doc-in-code-p)
    ;; do nothing when inside string or comment
    (let (sym prev-message)
      (cond
       ((setq prev-message haskell-doc-current-info--interaction-last)
        (setq haskell-doc-current-info--interaction-last nil)
        (cdr prev-message))
       ((setq sym
              (if (use-region-p)
                  (buffer-substring-no-properties
                   (region-beginning) (region-end))
                (haskell-ident-at-point)))
        (if sync
            (haskell-process-get-type sym #'identity t)
          (haskell-process-get-type
           sym (lambda (response)
                 (setq haskell-doc-current-info--interaction-last
                       (cons 'async response))
                 (eldoc-print-current-symbol-info)))))))))

(defun haskell-process-get-type (expr-string &optional callback sync)
  "Asynchronously get the type of a given string.

EXPR-STRING should be an expression passed to :type in ghci.

CALLBACK will be called with a formatted type string.

If SYNC is non-nil, make the call synchronously instead."
  (unless callback (setq callback (lambda (response) (message "%s" response))))
  (let ((process (and (haskell-session-maybe)
                    (haskell-session-process (haskell-session-maybe))))
        ;; Avoid passing bad strings to ghci
        (expr-okay
         (and (not (string-match-p "\\`[[:space:]]*\\'" expr-string))
            (not (string-match-p "\n" expr-string))))
        (ghci-command (concat ":type " expr-string))
        (process-response
         (lambda (response)
           ;; Responses with empty first line are likely errors
           (if (string-match-p (rx string-start line-end) response)
               (setq response nil)
             ;; Remove a newline at the end
             (setq response (replace-regexp-in-string "\n\\'" "" response))
             ;; Propertize for eldoc
             (save-match-data
               (when (string-match " :: " response)
                 ;; Highlight type
                 (let ((name (substring response 0 (match-end 0)))
                       (type (propertize
                              (substring response (match-end 0))
                              'face 'eldoc-highlight-function-argument)))
                   (setq response (concat name type)))))
             (when haskell-doc-prettify-types
               (dolist (re '(("::" . "∷") ("=>" . "⇒") ("->" . "→")))
                 (setq response
                       (replace-regexp-in-string (car re) (cdr re) response))))
             response))))
    (when (and process expr-okay)
      (if sync
          (let ((response (haskell-process-queue-sync-request process ghci-command)))
            (funcall callback (funcall process-response response)))
        (haskell-process-queue-command
         process
         (make-haskell-command
          :go (lambda (_) (haskell-process-send-string process ghci-command))
          :complete
          (lambda (_ response)
            (funcall callback (funcall process-response response)))))
        'async))))

(defun haskell-doc-sym-doc (sym)
  "Show the type of given symbol SYM.
For the function under point, show the type in the echo area.
This information is extracted from the `haskell-doc-prelude-types' alist
of prelude functions and their types, or from the local functions in the
current buffer.
If `haskell-doc-use-inf-haskell' is non-nil, this function will consult
the inferior Haskell process for type/kind information, rather than using
the haskell-doc database."
  (if haskell-doc-use-inf-haskell
      (unless (or (null sym) (string= "" sym))
        (let* ((message-log-max nil)
               (result (ignore-errors
                         (unwind-protect
                             (inferior-haskell-type sym)
                           (message "")))))
          (if (and result (string-match " :: " result))
              result
            (setq result (unwind-protect
                             (inferior-haskell-kind sym)
                           (message "")))
            (and result (string-match " :: " result) result))))
    (let ((i-am-prelude nil)
          (i-am-fct nil)
          (type nil)
          (is-reserved (haskell-doc-is-of sym haskell-doc-reserved-ids))
          (is-prelude  (haskell-doc-is-of sym haskell-doc-prelude-types))
          (is-strategy (haskell-doc-is-of sym haskell-doc-strategy-ids))
          (is-user-defined (haskell-doc-is-of sym haskell-doc-user-defined-ids)))
      (cond
       ;; if reserved id (i.e. Haskell keyword
       ((and haskell-doc-show-reserved
             is-reserved)
        (setq type (cdr is-reserved))
        (setcdr haskell-doc-last-data type))
       ;; if built-in function get type from docstring
       ((and (not (null haskell-doc-show-prelude))
             is-prelude)
        (setq type (cdr is-prelude)) ; (cdr (assoc sym haskell-doc-prelude-types)))
        (if (= 2 (length type))      ; horrible hack to remove bad formatting
            (setq type (car (cdr type))))
        (setq i-am-prelude t)
        (setq i-am-fct t)
        (setcdr haskell-doc-last-data type))
       ((and haskell-doc-show-strategy
             is-strategy)
        (setq i-am-fct t)
        (setq type (cdr is-strategy))
        (setcdr haskell-doc-last-data type))
       ((and haskell-doc-show-user-defined
             is-user-defined)
        ;; (setq i-am-fct t)
        (setq type (cdr is-user-defined))
        (setcdr haskell-doc-last-data type))
       (t
        (let ( (x (haskell-doc-get-and-format-fct-type sym)) )
          (if (null x)
              (setcdr haskell-doc-last-data nil) ; if not found reset last data
            (setq type (car x))
            (setq i-am-fct (string= "Variables" (cdr x)))
            (if (and haskell-doc-show-global-types (null type))
                (setq type (haskell-doc-get-global-fct-type sym)))
            (setcdr haskell-doc-last-data type)))) )
      ;; ToDo: encode i-am-fct info into alist of types
      (and type
           ;; drop `::' if it's not a fct
           (let ( (str (cond ((and i-am-fct (not haskell-doc-chop-off-fctname))
                              (format "%s :: %s" sym type))
                             (t
                              (format "%s" type)))) )
             (if i-am-prelude
                 (add-text-properties 0 (length str) '(face bold) str))
             str)))))


;; ToDo: define your own notion of `near' to find surrounding fct
;;(defun haskell-doc-fnsym-in-current-sexp ()
;;  (let* ((p (point))
;;         (sym (progn
;;              (forward-word -1)
;;                (while (and (forward-word -1) ; (haskell-doc-forward-sexp-safe -1)
;;                            (> (point) (point-min))))
;;                (cond ((or (= (point) (point-min))
;;                           (memq (or (char-after (point)) 0)
;;                                 '(?\( ?\"))
;;                           ;; If we hit a quotation mark before a paren, we
;;                           ;; are inside a specific string, not a list of
;;                           ;; symbols.
;;                           (eq (or (char-after (1- (point))) 0) ?\"))
;;                       nil)
;;                      (t (condition-case nil
;;                             (read (current-buffer))
;;                           (error nil)))))))
;;    (goto-char p)
;;    (if sym
;;      (format "%s" sym)
;;      sym)))

;;    (and (symbolp sym)
;;         sym)))


;; ToDo: handle open brackets to decide if it's a wrapped type

(defun haskell-doc-grab-line (fct-and-pos)
  "Get the type of an \(FCT POSITION\) pair from the current buffer."
  ;; (if (null fct-and-pos)
  ;;     "" ; fn is not a local fct
  (let ( (str ""))
    (goto-char (cdr fct-and-pos))
    (beginning-of-line)
    ;; search for start of type (phsp give better bound?)
    (if (null (search-forward "::" (+ (point) haskell-doc-search-distance) t))
        ""
      (setq str (haskell-doc-grab))        ; leaves point at end of line
      (while (haskell-doc-wrapped-type-p)  ; while in a multi-line type expr
        (forward-line 1)
        (beginning-of-line)
        (skip-chars-forward " \t")
        (setq str (concat str (haskell-doc-grab))))
      (haskell-doc-string-nub-ws           ; squeeze string
       (if haskell-doc-chop-off-context    ; no context
           (haskell-doc-chop-off-context str)
         str)))))
;; (concat (car fct-and-pos) "::" (haskell-doc-string-nub-ws str))))

(defun haskell-doc-wrapped-type-p ()
  "Check whether the type under the cursor is wrapped over several lines.
The cursor must be at the end of a line, which contains the type.
Currently, only the following is checked:
If this line ends with a `->' or the next starts with an `->' it is a
multi-line type \(same for `=>'\).
`--' comments are ignored.
ToDo: Check for matching parenthesis!."
  (save-excursion
    (let ( (here (point))
           (lim (progn (beginning-of-line) (point)))
           ;; (foo "")
           (res nil)
           )
      (goto-char here)
      (search-backward "--" lim t) ; skip over `--' comment
      (skip-chars-backward " \t")
      (if (bolp)                   ; skip empty lines
          (progn
            (forward-line 1)
            (end-of-line)
            (setq res (haskell-doc-wrapped-type-p)))
        (forward-char -1)
        ;; (setq foo (concat foo (char-to-string (preceding-char)) (char-to-string (following-char))))
        (if (or (and (or (char-equal (preceding-char) ?-) (char-equal (preceding-char) ?=))
                     (char-equal (following-char) ?>)) ; (or -!> =!>
                (char-equal (following-char) ?,))      ;     !,)
            (setq res t)
          (forward-line)
          (let ((here (point)))
            (goto-char here)
            (skip-chars-forward " \t")
            (if (looking-at "--")  ; it is a comment line
                (progn
                  (forward-line 1)
                  (end-of-line)
                  (setq res (haskell-doc-wrapped-type-p)))
              (forward-char 1)
              ;; (setq foo (concat foo (char-to-string (preceding-char)) (char-to-string (following-char))))
              ;; (message "|%s|" foo)
              (if (and (or (char-equal (preceding-char) ?-) (char-equal (preceding-char) ?=))
                       (char-equal (following-char) ?>)) ; -!> or =!>
                  (setq res t))))))
      res)))

(defun haskell-doc-grab ()
  "Return the text from point to the end of the line, chopping off comments.
Leaves point at end of line."
  (let ((str (buffer-substring-no-properties
              (point) (progn (end-of-line) (point)))))
    (if (string-match "--" str)
        (substring str 0 (match-beginning 0))
      str)))

(defun haskell-doc-string-nub-ws (str)
  "Replace all sequences of whitespace in STR by just one space.
ToDo: Also eliminate leading and trailing whitespace."
  (let ((i -1))
    (while (setq i (string-match " [ \t\n]+\\|[\t\n]+" str (1+ i)))
      (setq str (replace-match " " t t str)))
    str))

(defun haskell-doc-chop-off-context (str)
  "Eliminate the context in a type represented by the string STR."
  (let ((i (string-match "=>" str)) )
    (if (null i)
        str
      (substring str (+ i 2)))))

(defun haskell-doc-get-imenu-info (obj kind)
  "Return a string describing OBJ of KIND \(Variables, Types, Data\)."
  (cond
   ((eq major-mode 'haskell-mode)
    (let* ((imenu-info-alist (cdr (assoc kind imenu--index-alist)))
           ;; (names (mapcar 'car imenu-info-alist))
           (x (assoc obj imenu-info-alist)))
      (when x (haskell-doc-grab-line x))))

   (t ;; (error "Cannot get local functions in %s mode, sorry" major-mode)))
    nil)))

;; ToDo:
;;  - modular way of defining a mapping of module name to file
;;  - use a path to search for file (not just current directory)


(defun haskell-doc-imported-list ()
  "Return a list of the imported modules in current buffer."
  (interactive "fName of outer `include' file: ") ;  (buffer-file-name))
  ;; Don't add current buffer to the imported file list if it is not (yet?)
  ;; visiting a file since it leads to errors further down.
  (let ((imported-file-list (and buffer-file-name (list buffer-file-name))))
    (widen)
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*import\\s-+\\([^ \t\n]+\\)" nil t)
      (let ((basename (match-string 1)))
        (dolist (ext '(".hs" ".lhs"))
          (let ((file (concat basename ext)))
            (if (file-exists-p file)
                (push file imported-file-list))))))
    (nreverse imported-file-list)
    ;;(message imported-file-list)
    ))

;; ToDo: generalise this to "Types" etc (not just "Variables")

(defun haskell-doc-rescan-files (filelist)
  "Do an `imenu' rescan on every file in FILELIST and return the fct-list.
This function switches to and potentially loads many buffers."
  (save-current-buffer
    (mapcar (lambda (f)
              (set-buffer (find-file-noselect f))
              (imenu--make-index-alist t)
              (cons f
                    (mapcar (lambda (x)
                              `(,(car x) . ,(haskell-doc-grab-line x)))
                            (cdr (assoc "Variables" imenu--index-alist)))))
            filelist)))

(defun haskell-doc-make-global-fct-index ()
  "Scan imported files for types of global fcts and update `haskell-doc-index'."
  (interactive)
  (setq haskell-doc-index
        (haskell-doc-rescan-files (haskell-doc-imported-list))))

;; ToDo: use a separate munge-type function to format type concisely

(defun haskell-doc-get-global-fct-type (&optional sym)
  "Get type for function symbol SYM by examining `haskell-doc-index'."
  (interactive) ;  "fName of outer `include' file: \nsFct:")
  (save-excursion
    ;; (switch-to-buffer "*scratch*")
    ;; (goto-char (point-max))
    ;; ;; Produces a list of fct-type alists
    ;; (if (null sym)
    ;;     (setq sym (progn (forward-word -1) (read (current-buffer)))))
    (or sym
        (current-word))
    (let* ( (fn sym) ; (format "%s" sym))
            (fal haskell-doc-index)
            (res "") )
      (while (not (null fal))
        (let* ( (l (car fal))
                (f (car l))
                (x (assoc fn (cdr l))) )
          (if (not (null x))
              (let* ( (ty (cdr x)) ; the type as string
                      (idx (string-match "::" ty))
                      (str (if (null idx)
                               ty
                             (substring ty (+ idx 2)))) )
                (setq res (format "[%s] %s" f str))))
          (setq fal (cdr fal))))
      res))) ; (message res)) )

(defun haskell-doc-get-and-format-fct-type (fn)
  "Get the type and kind of FN by checking local and global functions."
  (save-excursion
    (save-match-data
      (let ((docstring "")
            (doc nil)
            )
        ;; is it a local function?
        (setq docstring (haskell-doc-get-imenu-info fn "Variables"))
        (if (not (null docstring))
            ;; (string-match (format "^%s\\s-+::\\s-+\\(.*\\)$" fn) docstring))
            (setq doc `(,docstring . "Variables"))) ; `(,(match-string 1 docstring) . "Variables") ))
        ;; is it a type declaration?
        (setq docstring (haskell-doc-get-imenu-info fn "Types"))
        (if (not (null docstring))
            ;; (string-match (format "^\\s-*type\\s-+%s.*$" fn) docstring))
            (setq doc `(,docstring . "Types"))) ; `(,(match-string 0 docstring) . "Types")) )
        (if (not (null docstring))
            ;; (string-match (format "^\\s-*data.*%s.*$" fn) docstring))
            (setq doc `(,docstring . "Data"))) ; (setq doc `(,(match-string 0 docstring) . "Data")) )
        ;; return the result
        doc ))))

(defun inferior-haskell-kind (sym)
  "Find the kind of SYM with `:kind' ghci feature."
  (inferior-haskell-get-result (format ":kind %s" sym)))

(defun inferior-haskell-type (sym)
  "Find the type of SYM with `:type' ghci feature."
  (inferior-haskell-get-result (format ":type (%s)" sym)))

(provide 'haskell-doc)

;;; haskell-doc.el ends here
