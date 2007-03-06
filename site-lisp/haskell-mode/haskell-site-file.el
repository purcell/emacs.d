;;; haskell-site-file.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path
              (or (file-name-directory load-file-name) (car load-path)))



;;;### (autoloads (haskell-cabal-mode) "haskell-cabal" "haskell-cabal.el"
;;;;;;  (17875 26421))
;;; Generated autoloads from haskell-cabal.el

(add-to-list (quote auto-mode-alist) (quote ("\\.cabal\\'" . haskell-cabal-mode)))

(autoload (quote haskell-cabal-mode) "haskell-cabal" "\
Major mode for Cabal package description files.

\(fn)" t nil)

;;;***

;;;### (autoloads (haskell-decl-scan-mode) "haskell-decl-scan" "haskell-decl-scan.el"
;;;;;;  (17282 16501))
;;; Generated autoloads from haskell-decl-scan.el

(autoload (quote haskell-decl-scan-mode) "haskell-decl-scan" "\
Minor mode for declaration scanning for Haskell mode.
Top-level declarations are scanned and listed in the menu item \"Declarations\".
Selecting an item from this menu will take point to the start of the
declaration.

\\[haskell-ds-forward-decl] and \\[haskell-ds-backward-decl] move forward and backward to the start of a declaration.

Under XEmacs, the following keys are also defined:

\\[fume-list-functions] lists the declarations of the current buffer,
\\[fume-prompt-function-goto] prompts for a declaration to move to, and
\\[fume-mouse-function-goto] moves to the declaration whose name is at point.

This may link with `haskell-doc' (only for Emacs currently).

For non-literate and LaTeX-style literate scripts, we assume the
common convention that top-level declarations start at the first
column.  For Bird-style literate scripts, we assume the common
convention that top-level declarations start at the third column,
ie. after \"> \".

Anything in `font-lock-comment-face' is not considered for a
declaration.  Therefore, using Haskell font locking with comments
coloured in `font-lock-comment-face' improves declaration scanning.

To turn on declaration scanning for all Haskell buffers, add this to
.emacs:

  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

To turn declaration scanning on for the current buffer, call
`turn-on-haskell-decl-scan'.

Literate Haskell scripts are supported: If the value of
`haskell-literate' (automatically set by the Haskell mode of
Moss&Thorn) is 'bird, a Bird-style literate script is assumed.  If it
is nil or 'latex, a non-literate or LaTeX-style literate script is
assumed, respectively.

Invokes `haskell-decl-scan-hook' if not nil.

Use `haskell-decl-scan-version' to find out what version this is.

\(fn &optional ARG)" nil nil)

;;;***

;;;### (autoloads (haskell-doc-show-type haskell-doc-mode) "haskell-doc"
;;;;;;  "haskell-doc.el" (17869 26151))
;;; Generated autoloads from haskell-doc.el

(autoload (quote haskell-doc-mode) "haskell-doc" "\
Enter `haskell-doc-mode' for showing fct types in the echo area.
See variable docstring.

\(fn &optional ARG)" t nil)

(defalias (quote turn-on-haskell-doc-mode) (quote haskell-doc-mode))

(autoload (quote haskell-doc-show-type) "haskell-doc" "\
Show the type of the function near point.
For the function under point, show the type in the echo area.
This information is extracted from the `haskell-doc-prelude-types' alist
of prelude functions and their types, or from the local functions in the
current buffer.

\(fn &optional SYM)" t nil)

;;;***

;;;### (autoloads (haskell-indent-mode) "haskell-indent" "haskell-indent.el"
;;;;;;  (17875 21187))
;;; Generated autoloads from haskell-indent.el

(autoload (quote haskell-indent-mode) "haskell-indent" "\
``intelligent'' Haskell indentation mode that deals with
the layout rule of Haskell.  \\[haskell-indent-cycle] starts the cycle
which proposes new possibilities as long as the TAB key is pressed.
Any other key or mouse click terminates the cycle and is interpreted
except for RET which merely exits the cycle.
Other special keys are:
    \\[haskell-indent-insert-equal]
      inserts an =
    \\[haskell-indent-insert-guard]
      inserts an |
    \\[haskell-indent-insert-otherwise]
      inserts an | otherwise =
these functions also align the guards and rhs of the current definition
    \\[haskell-indent-insert-where]
      inserts a where keyword
    \\[haskell-indent-align-guards-and-rhs]
      aligns the guards and rhs of the region
    \\[haskell-indent-put-region-in-literate]
      makes the region a piece of literate code in a literate script

Note: \\[indent-region] which applies \\[haskell-indent-cycle] for each line
of the region also works but it stops and asks for any line having more
than one possible indentation.
Use TAB to cycle until the right indentation is found and then RET to go the
next line to indent.

Invokes `haskell-indent-hook' if not nil.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (literate-haskell-mode haskell-mode) "haskell-mode"
;;;;;;  "haskell-mode.el" (17869 25437))
;;; Generated autoloads from haskell-mode.el

(autoload (quote haskell-mode) "haskell-mode" "\
Major mode for editing Haskell programs.  Last adapted for Haskell 1.4.
Blank lines separate paragraphs, comments start with `-- '.

\\<haskell-mode-map>\\[indent-for-comment] will place a comment at an appropriate place on the current line.
\\[comment-region] comments (or with prefix arg, uncomments) each line in the region.

Literate scripts are supported via `literate-haskell-mode'.  The
variable `haskell-literate' indicates the style of the script in the
current buffer.  See the documentation on this variable for more
details.

Modules can hook in via `haskell-mode-hook'.  The following modules
are supported with an `autoload' command:

   `haskell-decl-scan', Graeme E Moss
     Scans top-level declarations, and places them in a menu.

   `haskell-doc', Hans-Wolfgang Loidl
     Echoes types of functions or syntax of keywords when the cursor is idle.

   `haskell-indent', Guy Lapalme
     Intelligent semi-automatic indentation.

   `haskell-simple-indent', Graeme E Moss and Heribert Schuetz
     Simple indentation.

Module X is activated using the command `turn-on-X'.  For example,
`haskell-font-lock' is activated using `turn-on-haskell-font-lock'.
For more information on a module, see the help for its `turn-on-X'
function.  Some modules can be deactivated using `turn-off-X'.  (Note
that `haskell-doc' is irregular in using `turn-(on/off)-haskell-doc-mode'.)

Use `haskell-version' to find out what version this is.

Invokes `haskell-mode-hook' if not nil.

\(fn)" t nil)

(autoload (quote literate-haskell-mode) "haskell-mode" "\
As `haskell-mode' but for literate scripts.

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . literate-haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;;***

;;;### (autoloads (inferior-haskell-load-file switch-to-haskell)
;;;;;;  "inf-haskell" "inf-haskell.el" (17875 21120))
;;; Generated autoloads from inf-haskell.el

(defalias (quote run-haskell) (quote switch-to-haskell))

(autoload (quote switch-to-haskell) "inf-haskell" "\
Show the inferior-haskell buffer.  Start the process if needed.

\(fn &optional ARG)" t nil)

(autoload (quote inferior-haskell-load-file) "inf-haskell" "\
Pass the current buffer's file to the inferior haskell process.

\(fn &optional RELOAD)" t nil)

;;;***

;;;### (autoloads nil nil ("haskell-font-lock.el" "haskell-ghci.el"
;;;;;;  "haskell-hugs.el" "haskell-simple-indent.el") (17875 26544
;;;;;;  864790))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; haskell-site-file.el ends here
