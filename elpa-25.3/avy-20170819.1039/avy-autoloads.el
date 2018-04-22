;;; avy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "avy" "avy.el" (23009 21841 0 0))
;;; Generated autoloads from avy.el

(autoload 'avy-goto-char "avy" "\
Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it).

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-char-in-line "avy" "\
Jump to the currently visible CHAR in the current line.

\(fn CHAR)" t nil)

(autoload 'avy-goto-char-2 "avy" "\
Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

\(fn CHAR1 CHAR2 &optional ARG BEG END)" t nil)

(autoload 'avy-goto-char-2-above "avy" "\
Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR1 CHAR2 &optional ARG)" t nil)

(autoload 'avy-goto-char-2-below "avy" "\
Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR1 CHAR2 &optional ARG)" t nil)

(autoload 'avy-isearch "avy" "\
Jump to one of the current isearch candidates.

\(fn)" t nil)

(autoload 'avy-goto-word-0 "avy" "\
Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

\(fn ARG &optional BEG END)" t nil)

(autoload 'avy-goto-word-1 "avy" "\
Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start.

\(fn CHAR &optional ARG BEG END SYMBOL)" t nil)

(autoload 'avy-goto-word-1-above "avy" "\
Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-word-1-below "avy" "\
Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-symbol-1 "avy" "\
Jump to the currently visible CHAR at a symbol start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-symbol-1-above "avy" "\
Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-symbol-1-below "avy" "\
Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-subword-0 "avy" "\
Jump to a word or subword start.

The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true.

\(fn &optional ARG PREDICATE)" t nil)

(autoload 'avy-goto-subword-1 "avy" "\
Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored.

\(fn CHAR &optional ARG)" t nil)

(autoload 'avy-goto-word-or-subword-1 "avy" "\
Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'.

\(fn)" t nil)

(autoload 'avy-goto-line "avy" "\
Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG.

\(fn &optional ARG)" t nil)

(autoload 'avy-goto-line-above "avy" "\
Goto visible line above the cursor.

\(fn)" t nil)

(autoload 'avy-goto-line-below "avy" "\
Goto visible line below the cursor.

\(fn)" t nil)

(autoload 'avy-copy-line "avy" "\
Copy a selected line above the current line.
ARG lines can be used.

\(fn ARG)" t nil)

(autoload 'avy-move-line "avy" "\
Move a selected line above the current line.
ARG lines can be used.

\(fn ARG)" t nil)

(autoload 'avy-copy-region "avy" "\
Select two lines and copy the text between them to point.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

\(fn ARG)" t nil)

(autoload 'avy-move-region "avy" "\
Select two lines and move the text between them above the current line.

\(fn)" t nil)

(autoload 'avy-kill-region "avy" "\
Select two lines and kill the region between them.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

\(fn ARG)" t nil)

(autoload 'avy-kill-ring-save-region "avy" "\
Select two lines and save the region between them to the kill ring.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

\(fn ARG)" t nil)

(autoload 'avy-kill-whole-line "avy" "\
Select line and kill the whole selected line.

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\\[universal-argument] 3 \\[avy-kil-whole-line] kill three lines
starting from the selected line.  \\[universal-argument] -3

\\[avy-kill-whole-line] kill three lines backward including the
selected line.

\(fn ARG)" t nil)

(autoload 'avy-kill-ring-save-whole-line "avy" "\
Select line and save the whole selected line as if killed, but don’t kill it.

This command is similar to `avy-kill-whole-line', except that it
saves the line(s) as if killed, but does not kill it(them).

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\(fn ARG)" t nil)

(autoload 'avy-setup-default "avy" "\
Setup the default shortcuts.

\(fn)" nil nil)

(autoload 'avy-goto-char-timer "avy" "\
Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it).

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; avy-autoloads.el ends here
