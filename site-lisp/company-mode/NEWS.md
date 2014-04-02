# History of user-visible changes

## Next

* New user option `company-abort-manual-when-too-short`.

## 2014-03-25 (0.7.3)

* New user option `company-etags-ignore-case`.

## 2014-03-19 (0.7.2)

* Support for Emacs 22 officially dropped.
* `company-clang` supports `indent-tabs-mode` and multibyte chars before point.

## 2014-03-18 (0.7.1)

* Group of back-ends can now contain keyword `:with`, which makes all back-ends
  after it to be skipped for prefix calculation.
* New function `company-version`.
* New bundled back-end `company-yasnippet`.
* Completion candidates returned from grouped back-ends are tagged to remember
  which back-end each came from.
* New user option `company-tooltip-align-annotations`, off by default.
* New bundled back-end `company-bbdb`.

## 2014-02-18 (0.7)

* New back-end command, `match`, for non-prefix completion.
* New user option `company-continue-commands`. The default value aborts
  completion on buffer saving commands.
* New back-end command, `annotation`, for text displayed inline in the popup
  that's not a part of completion candidate.
* `company-capf`, `company-clang` and `company-eclim` use `annotation`.
* `company-preview*` faces inherit from `company-tooltip-selection` and
  `company-tooltip-common-selection` on light themes.
* New user option `company-transformers`.
* First transformer, `company-sort-by-occurrence`.
* New user options controlling `company-dabbrev` and `company-dabbrev-code`.

## 2014-01-25 (0.6.14)

* The tooltip front-end is rendered with scrollbar, controlled by the user
  option `company-tooltip-offset-display`.
* The tooltip front-end is rendered with margins, controlled by the user option
  `company-tooltip-margin`.

## 2014-01-14 (0.6.13)

* Experimental support for non-prefix completion.
* Starting with Emacs version 24.4, `company-capf` is included in
  `company-backends` and replaces `company-elisp`.
* `company-capf` supports completion tables that return non-default boundaries.
* `company-elisp` is enabled in `inferior-emacs-lisp-mode`.

## 2013-09-28 (0.6.12)

* Default value of `company-begin-commands` changed to `(self-insert-command)`.
* Futher improvement in `org-indent-mode` compatibility.

## 2013-08-18 (0.6.11)

* `company-template-c-like-templatify` removes all text after closing paren, for
  use in backends that display additional info there.
* `company-cmake` is now bundled.
* Better `linum` compatibility in Emacs <= 24.2.
* `company-global-modes`: New option.

## 2013-05-26 (0.6.10)

* Plays nicer with `org-indent-mode`.
* Works in horizontally scrolled windows.

## 2013-05-10 (0.6.9)

* `company-capf` respects `:exit-function` completion property.
* `company-backends`: `prefix` command can return `t` in the cdr.
* `company-clang-begin-after-member-access`: New option.
* Mouse click outside the tooltip aborts completion.
* `company-clang` uses standard input to pass the contents of current buffer to
  Clang 2.9+, otherwise saves the buffer and passes the path to the file.
* `company-clang-auto-save` option has been removed.
* Better interaction with `outline-minor-mode`.
* `company-dabbrev-code` supports all `prog-mode` derivatives.

## 2013-04-16 (0.6.8)

* `company-auto-complete` is disabled by default.
* `company-auto-complete-chars` default value includes fewer syntax classes.
* In expanded function calls, arguments skipped by the user default to "argN".
* `company-eclim` and `company-clang` do not strip argument types from fields.
* `company-clang` expands function calls for all three modes now.
* `company-clang` supports `c++-mode` by default.

## 2013-04-05 (0.6.7)

* Two `company-elisp` tweaks.

## 2013-04-01 (0.6.6)

* `company-elisp` doesn't offer completions when typing the name and the
  arguments of a new function or macro definition, allowing to fall back to
  other back-ends like `company-dabbrev-code`.

## 2013-03-30 (0.6.5)

* Fixed keybindings when running in a terminal.
* `company-elisp-show-locals-first`: new customizable variable.
* `company-elisp` shows more accurate and comprehensive candidates list.

## 2013-03-26 (0.6.4)

* `company-eclim` shows valid completions after an opening paren.
* Expanded template does not get removed until the point leaves it.  After your
  input the last argument in a method call expanded by `company-eclim`, you can
  press `<tab>` once more, to jump after the closing paren.  No other bundled
  back-ends are affected.

## 2013-03-25 (0.6.3)

* New tooltip face colors used on themes with light background.
* Pseudo-tooltip stays up-to-date when text is inserted after the point.
* Fixed `company-require-match` mechanics.

## 2013-03-24 (0.6.2)

* `global-company-mode` is now autoloaded.

## 2013-03-23 (0.6.1)

* Documented `init` and `post-completion` back-end commands.
* `company-eclim` and `company-clang` only expand the template on explicit user
  action (such as `company-complete-{selection,number,mouse}`).
* `company-template` has some breaking changes.  When point is at one of the
  fields, it's displayed at the beginning, not right after it; `<tab>` jumps to
  the next field, `forward-word` and `subword-forward` remappings are removed;
  when you jump to the next field, if the current one hasn't been edited, the
  overlay gets removed but the text remains.
* `company-eclim` shows method overloads and expands templates for calls.
* `company-clang-objc-templatify` does not insert spaces after colons anymore.
* `company-clang` is now only initialized in supported buffers.
  So, no error messages if you don't have Clang until you open a C file.
* `company-clang` recognizes Clang included in recent Xcode.
* New commands `company-select-previous-or-abort` and
  `company-select-next-or-abort`, bound to `<up>` and `<down>`.

## 2013-03-19 (0.6)

* Across-the-board bugfixing.
* `company-pysmell` is not used by default anymore.
* Loading of `nxml`, `semantic`, `pymacs` and `ropemacs` is now deferred.
* Candidates from grouped back-ends are merged more conservatively: only
  back-ends that return the same prefix at point are used.
* `company-clang` now shows meta information, too.
* Some performance improvements.
* Fixed two old tooltip annoyances.
* Instead of `overrriding-terminal-local-map`, we're now using
  `emulation-mode-map-alists` (experimental).  This largely means that when the
  completion keymap is active, other minor modes' keymaps are still used, so,
  for example, it's not as easy to accidentally circumvent `paredit-mode`
  when it's enabled.
* `company-elisp` has seen some improvements.
* Added `company-capf`: completion adapter using
  `completion-at-point-functions`.  (Stefan Monnier)
* Clang completions now include macros and are case-sensitive.
* Switching between tag files now works correctly with `company-etags`.

## 2010-02-24 (0.5)

* `company-ropemacs` now provides location and docs.  (Fernando H. Silva)
* Added `company-with-candidate-inserted` macro.
* Added `company-clang` back-end.
* Added new mechanism for non-consecutive insertion.
  (So far only used by clang for ObjC.)
* The semantic back-end now shows meta information for local symbols.
* Added compatibility for CEDET in Emacs 23.2 and from CVS.  (Oleg Andreev)

## 2009-05-07 (0.4.3)

* Added `company-other-backend`.
* Idle completion no longer interrupts multi-key command input.
* Added `company-ropemacs` and `company-pysmell` back-ends.

## 2009-04-25 (0.4.2)

* In C modes . and -> now count towards `company-minimum-prefix-length`.
* Reverted default front-end back to `company-preview-if-just-one-frontend`.
* The pseudo tooltip will no longer be clipped at the right window edge.
* Added `company-tooltip-minimum`.
* Windows compatibility fixes.

## 2009-04-19 (0.4.1)

* Added `global-company-mode`.
* Performance enhancements.
* Added `company-eclim` back-end.
* Added safer workaround for Emacs `posn-col-row` bug.

## 2009-04-18 (0.4)

* Automatic completion is now aborted if the prefix gets too short.
* Added option `company-dabbrev-time-limit`.
* `company-backends` now supports merging back-ends.
* Added back-end `company-dabbrev-code` for generic code.
* Fixed `company-begin-with`.

## 2009-04-15 (0.3.1)

* Added 'stop prefix to prevent dabbrev from completing inside of symbols.
* Fixed issues with tabbar-mode and line-spacing.
* Performance enhancements.

## 2009-04-12 (0.3)

* Added `company-begin-commands` option.
* Added abbrev, tempo and Xcode back-ends.
* Back-ends are now interactive.  You can start them with M-x backend-name.
* Added `company-begin-with` for starting company from elisp-code.
* Added hooks.
* Added `company-require-match` and `company-auto-complete` options.

## 2009-04-05 (0.2.1)

* Improved Emacs Lisp back-end behavior for local variables.
* Added `company-elisp-detect-function-context` option.
* The mouse can now be used for selection.

## 2009-03-22 (0.2)

* Added `company-show-location`.
* Added etags back-end.
* Added work-around for end-of-buffer bug.
* Added `company-filter-candidates`.
* More local Lisp variables are now included in the candidates.

## 2009-03-21 (0.1.5)

* Fixed elisp documentation buffer always showing the same doc.
* Added `company-echo-strip-common-frontend`.
* Added `company-show-numbers` option and M-0 ... M-9 default bindings.
* Don't hide the echo message if it isn't shown.

## 2009-03-20 (0.1)

* Initial release.
