;;;; anything.el --- open anything / QuickSilver-like candidate-selection framework
;; $Id: anything.el,v 1.201 2009/08/08 13:25:30 rubikitch Exp rubikitch $

;; Copyright (C) 2007        Tamas Patrovics
;;               2008, 2009  rubikitch <rubikitch@ruby-lang.org>

;; Author: Tamas Patrovics
;; Maintainer: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: files, frames, help, matching, outlines, processes, tools, convenience, anything
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything.el
;; Site: http://www.emacswiki.org/cgi-bin/emacs/Anything

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;
;; Start with M-x anything, narrow the list by typing some pattern,
;; select with up/down/pgup/pgdown/C-p/C-n/C-v/M-v, choose with enter,
;; left/right moves between sources. With TAB actions can be selected
;; if the selected candidate has more than one possible action.
;;
;; Note that anything.el provides only the framework and some example
;; configurations for demonstration purposes. See anything-config.el
;; for practical, polished, easy to use configurations which can be
;; used to assemble a custom personalized configuration. And many
;; other configurations are in the EmacsWiki.
;; 
;; http://www.emacswiki.org/cgi-bin/wiki/download/anything-config.el
;; http://www.emacswiki.org/cgi-bin/emacs/AnythingSources
;;
;; Maintainer's configuration is in the EmacsWiki. It would tell you
;; many tips to write smart sources!
;;
;; http://www.emacswiki.org/cgi-bin/emacs/RubikitchAnythingConfiguration

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything'
;;    Select anything. In Lisp program, some optional arguments can be used.
;;  `anything-resume'
;;    Resurrect previously invoked `anything'.
;;  `anything-at-point'
;;    Same as `anything' except when C-u is pressed, the initial input is the symbol at point.
;;  `anything-select-action'
;;    Select an action for the currently selected candidate.
;;  `anything-previous-line'
;;    Move selection to the previous line.
;;  `anything-next-line'
;;    Move selection to the next line.
;;  `anything-previous-page'
;;    Move selection back with a pageful.
;;  `anything-next-page'
;;    Move selection forward with a pageful.
;;  `anything-previous-source'
;;    Move selection to the previous source.
;;  `anything-next-source'
;;    Move selection to the next source.
;;  `anything-exit-minibuffer'
;;    Select the current candidate by exiting the minibuffer.
;;  `anything-delete-current-selection'
;;    Delete the currently selected item.
;;  `anything-delete-minibuffer-content'
;;    Same as `delete-minibuffer-contents' but this is a command.
;;  `anything-select-2nd-action'
;;    Select the 2nd action for the currently selected candidate.
;;  `anything-select-3rd-action'
;;    Select the 3rd action for the currently selected candidate.
;;  `anything-select-4th-action'
;;    Select the 4th action for the currently selected candidate.
;;  `anything-select-2nd-action-or-end-of-line'
;;    Select the 2nd action for the currently selected candidate if the point is at the end of minibuffer.
;;  `anything-execute-persistent-action'
;;    If a candidate is selected then perform the associated action without quitting anything.
;;  `anything-scroll-other-window'
;;    Scroll other window (not *Anything* window) upward.
;;  `anything-scroll-other-window-down'
;;    Scroll other window (not *Anything* window) downward.
;;  `anything-quit-and-find-file'
;;    Drop into `find-file' from `anything' like `iswitchb-find-file'.
;;  `anything-yank-selection'
;;    Set minibuffer contents to current selection.
;;  `anything-kill-selection-and-quit'
;;    Store current selection to kill ring.
;;  `anything-follow-mode'
;;    If this mode is on, persistent action is executed everytime the cursor is moved.
;;  `anything-isearch'
;;    Start incremental search within results. (UNMAINTAINED)
;;  `anything-isearch-printing-char'
;;    Add printing char to the pattern.
;;  `anything-isearch-again'
;;    Search again for the current pattern
;;  `anything-isearch-delete'
;;    Undo last event.
;;  `anything-isearch-default-action'
;;    Execute the default action for the selected candidate.
;;  `anything-isearch-select-action'
;;    Choose an action for the selected candidate.
;;  `anything-isearch-cancel'
;;    Cancel Anything isearch.
;;  `anything-iswitchb-setup'
;;    Integrate anything completion into iswitchb (UNMAINTAINED).
;;  `anything-iswitchb-cancel-anything'
;;    Cancel anything completion and return to standard iswitchb.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;; You can extend `anything' by writing plug-ins. As soon as
;; `anything' is invoked, `anything-sources' is compiled into basic
;; attributes, then compiled one is used during invocation.
;;
;; The oldest built-in plug-in is `type' attribute: appends
;; appropriate element of `anything-type-attributes'. Second built-in
;; plug-in is `candidates-in-buffer': selecting a line from candidates
;; buffer.
;;
;; To write a plug-in:
;; 1. Define a compiler: anything-compile-source--*
;; 2. Add compier function to `anything-compile-source-functions'.
;; 3. (optional) Write helper functions.
;;
;; Anything plug-ins are found in the EmacsWiki.
;;
;; http://www.emacswiki.org/cgi-bin/emacs/AnythingPlugins

;; Tested on Emacs 22.
;;
;;
;; Thanks to Vagn Johansen for ideas.
;; Thanks to Stefan Kamphausen for fixes and XEmacs support.
;; Thanks to Tassilo Horn for fixes.
;; Thanks to Drew Adams for various fixes (frame, isearch, customization, etc.)
;; Thanks to IMAKADO for candidates-in-buffer idea.
;; Thanks to Tomohiro MATSUYAMA for multiline patch.
;;

;;; (@* "Index")

;;  If you have library `linkd.el', load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections  Linkd mode will
;;  highlight this Index.  You can get `linkd.el' here:
;;  http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el
;;


;;; (@* "INCOMPATIBLE CHANGES")

;; v1.114
;;
;;   `anything-attr' returns nil when the source attribute is defined
;;   but the value of attribute is nil, eg. (volatile) cell. Use
;;   `anything-attr-defined' when testing whether the attribute is
;;   defined.

;;; (@* "Tips")

;;
;; Now symbols are acceptable as candidates. So you do not have to use
;; `symbol-name' function. The source is much simpler. For example,
;; `apropos-internal' returns a list of symbols.
;; 
;;   (anything
;;    '(((name . "Commands")
;;       (candidates . (lambda () (apropos-internal anything-pattern 'commandp)))
;;       (volatile)
;;       (action . describe-function))))

;;
;; To mark a candidate, press C-SPC as normal Emacs marking. To go to
;; marked candidate, press M-[ or M-].

;;
;; `anything-map' is now Emacs-standard key bindings by default. If
;; you are using `iswitchb', execute `anything-iswitchb-setup'. Then
;; some key bindings are adjusted to `iswitchb'. Note that
;; anything-iswitchb is not maintained.

;;
;; There are many `anything' applications, using `anything' for
;; selecting candidate. In this case, if there is one candidate or no
;; candidate, popping up *anything* buffer is irritating. If one
;; candidate, you want to select it at once. If no candidate, you want
;; to quit `anything'. Set `anything-execute-action-at-once-if-one'
;; and `anything-quit-if-no-candidate' to non-nil to remedy it. Note
;; that setting these variables GLOBALLY is bad idea because of
;; delayed sources. These are meant to be let-binded.
;; See anything-etags.el for example.
;;
;; [EVAL IT] (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/anything-etags.el")
;;
;; ex.
;; (let ((anything-execute-action-at-once-if-one t)
;;       (anything-quit-if-no-candidate (lambda () (message "No candidate"))))
;;    (anything temporary-sources input))

;;
;; `set-frame-configuration' arises flickering. If you hate
;; flickering, eval:
;; (setq anything-save-configuration-functions
;;    '(set-window-configuration . current-window-configuration))
;; at the cost of restoring frame configuration (only window configuration).

;;
;; `anything-delete-current-selection' deletes the current line.
;; It is useful when deleting a candidate in persistent action.
;; eg. `kill-buffer'.
;;
;; [EVAL IT] (describe-function 'anything-delete-current-selection)

;;
;; `anything-attr' gets the attribute. `anything-attrset' sets the
;; attribute. `anything-attr-defined' tests whether the attribute is
;; defined. They handles source-local variables.
;;
;; [EVAL IT] (describe-function 'anything-attr)
;; [EVAL IT] (describe-function 'anything-attrset)
;; [EVAL IT] (describe-function 'anything-attr-defined)

;;
;; `anything-sources' accepts many attributes to make your life easier.
;; Now `anything-sources' accepts a list of symbols.
;;
;; [EVAL IT] (describe-variable 'anything-sources)

;;
;; `anything' has optional arguments. Now you do not have to let-bind
;; `anything-sources'.
;;
;; [EVAL IT] (describe-function 'anything)

;;
;; `anything-resume' resumes last `anything' session. Now you do not
;; have to retype pattern.
;;
;; [EVAL IT] (describe-function 'anything-resume)

;;
;; `anything-execute-persistent-action' executes action without
;; quitting `anything'. When popping up a buffer in other window by
;; persistent action, you can scroll with `anything-scroll-other-window' and
;; `anything-scroll-other-window-down'. See also `anything-sources' docstring.
;;
;; [EVAL IT] (describe-function 'anything-execute-persistent-action)
;; [EVAL IT] (describe-variable 'anything-sources)

;;
;; `anything-select-2nd-action', `anything-select-3rd-action' and
;; `anything-select-4th-action' select other than default action
;; without pressing Tab.

;;
;; Using `anything-candidate-buffer' and the candidates-in-buffer
;; attribute is much faster than traditional "candidates and match"
;; way. And `anything-current-buffer-is-modified' avoids to
;; recalculate candidates for unmodified buffer. See docstring of
;; them.
;;
;; [EVAL IT] (describe-function 'anything-candidate-buffer)
;; [EVAL IT] (describe-function 'anything-candidates-in-buffer)
;; [EVAL IT] (describe-function 'anything-current-buffer-is-modified)

;;
;; `anything-current-buffer' and `anything-buffer-file-name' stores
;; `(current-buffer)' and `buffer-file-name' in the buffer `anything'
;; is invoked. Use them freely.
;;
;; [EVAL IT] (describe-variable 'anything-current-buffer)
;; [EVAL IT] (describe-variable 'anything-buffer-file-name)

;;
;; `anything-completing-read' and `anything-read-file-name' are
;; experimental implementation. If you are curious, type M-x
;; anything-read-string-mode. It is a minor mode and toggles on/off.

;;
;; Use `anything-test-candidates' to test your handmade anything
;; sources. It simulates contents of *anything* buffer with pseudo
;; `anything-sources' and `anything-pattern', without side-effect. So
;; you can unit-test your anything sources! Let's TDD!
;;
;; [EVAL IT] (describe-function 'anything-test-candidates)
;;
;; There are many unit-testing framework in Emacs Lisp. See the EmacsWiki.
;; http://www.emacswiki.org/cgi-bin/emacs/UnitTesting
;;
;; There is an unit-test by Emacs Lisp Expectations at the tail of this file.
;; http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el
;; http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el


;; (@* "TODO")
;;
;;   - process status indication
;;
;;   - async sources doesn't honor digit-shortcut-count
;;
;;   - anything-candidate-number-limit can't be nil everywhere

;; (@* "HISTORY")
;; $Log: anything.el,v $
;; Revision 1.201  2009/08/08 13:25:30  rubikitch
;; `anything-toggle-visible-mark': move next line after unmarking
;;
;; Revision 1.200  2009/08/08 13:23:46  rubikitch
;; `anything-toggle-visible-mark': Applied ThierryVolpiatto's patch. thx.
;;
;; Revision 1.199  2009/07/19 13:22:29  rubikitch
;; `anything-follow-execute-persistent-action-maybe': execute persistent action after `anything-input-idle-delay'
;;
;; Revision 1.198  2009/07/06 15:22:48  rubikitch
;; header modified (no code change)
;;
;; Revision 1.197  2009/06/29 15:10:13  rubikitch
;; OOPS! remove debug code
;;
;; Revision 1.196  2009/06/29 13:29:25  rubikitch
;; anything-follow-mode: automatical execution of persistent-action (C-c C-f)
;;
;; Revision 1.195  2009/06/19 14:42:57  rubikitch
;; silence byte compiler
;;
;; Revision 1.194  2009/06/14 15:12:34  rubikitch
;; typo
;;
;; Revision 1.193  2009/06/08 19:37:12  rubikitch
;; typo!
;;
;; Revision 1.192  2009/06/08 19:36:39  rubikitch
;; New keybind: C-e, C-j, C-k
;;
;; Revision 1.191  2009/06/08 19:30:27  rubikitch
;; New command: `anything-select-2nd-action-or-end-of-line'
;;
;; Revision 1.190  2009/06/07 17:09:50  rubikitch
;; add M-<next>, C-M-S-v, M-<prior> to `anything-map'.
;;
;; Revision 1.189  2009/06/01 21:36:31  rubikitch
;; New function: `anything-other-buffer'
;;
;; Revision 1.188  2009/05/29 18:33:07  rubikitch
;; avoid error when executing (anything-mark-current-line) in async process.
;;
;; Revision 1.187  2009/05/29 06:49:05  rubikitch
;; small refactoring
;;
;; Revision 1.186  2009/05/29 06:46:34  rubikitch
;; Prevent `anything-isearch-map' from overwriting `global-map'. With
;; `copy-keymap', the prefix command "M-s" in `global-map' ends up
;; getting clobbered by `anything-isearch-again', preventing `occur'
;; (among other things) from running. This change replaces overwriting a
;; copied map with writing to a sparse map whose parent is `global-map'.
;;
;; patched by DanielHackney. thanks!
;;
;; Revision 1.185  2009/05/25 19:07:42  rubikitch
;; `anything': set `case-fold-search' to t
;; Because users can assign commands to capital letter keys.
;;
;; Revision 1.184  2009/05/25 19:05:04  rubikitch
;; Added auto-document
;;
;; Revision 1.183  2009/05/15 01:50:46  rubikitch
;; typo
;;
;; Revision 1.182  2009/05/08 18:28:18  rubikitch
;; Bug fix: `anything-attr' is usable in `header-name' function.
;;
;; Revision 1.181  2009/05/04 19:05:03  rubikitch
;; * `anything-yank-selection' and `anything-kill-selection-and-quit' handles display string now.
;; * `anything-get-selection': Added optional arguments.
;;
;; Revision 1.180  2009/05/03 19:03:34  rubikitch
;; Add `anything-input' to `minibuffer-history' even if `anything' is quit.
;;
;; Revision 1.179  2009/04/20 16:35:44  rubikitch
;; New keybindings in anything-map:
;;   C-c C-d: `anything-delete-current-selection'
;;   C-c C-y: `anything-yank-selection'
;;   C-c C-k: `anything-kill-selection-and-quit'
;;
;; Revision 1.178  2009/04/20 16:18:58  rubikitch
;; New variable: `anything-display-function'
;;
;; Revision 1.177  2009/04/20 02:17:16  rubikitch
;; New commands: `anything-yank-selection', `anything-kill-selection-and-quit'
;;
;; Revision 1.176  2009/04/08 14:48:15  rubikitch
;; bug fix in `anything-candidate-buffer'
;;
;; Revision 1.175  2009/03/22 19:10:37  rubikitch
;; New Variable: `anything-scroll-amount' (thx. ThierryVolpiatto)
;;
;; Revision 1.174  2009/03/12 19:12:24  rubikitch
;; New API: `define-anything-type-attribute'
;;
;; Revision 1.173  2009/03/11 08:10:32  rubikitch
;; Update doc
;;
;; Revision 1.172  2009/03/10 17:11:58  rubikitch
;; `candidate-transformer', `filtered-candidate-transformer',
;; `action-transformer' attributes: accept a list of functions
;;
;; Revision 1.171  2009/03/09 18:49:44  rubikitch
;; New command: `anything-quit-and-find-file'
;;
;; Revision 1.170  2009/03/09 18:46:11  rubikitch
;; New API: `anything-run-after-quit'
;;
;; Revision 1.169  2009/03/09 10:02:49  rubikitch
;; Set candidate-number-limit attribute for actions.
;;
;; Revision 1.168  2009/03/07 21:01:10  rubikitch
;; Bug workaround
;;
;; Revision 1.167  2009/03/06 04:13:42  rubikitch
;; Fix doc
;;
;; Revision 1.166  2009/03/03 10:35:57  rubikitch
;; Set default `anything-input-idle-delay' to 0.1
;;
;; Revision 1.165  2009/03/03 07:14:42  rubikitch
;; Make sure to run `anything-update-hook' after processing delayed sources.
;;
;; Revision 1.164  2009/03/02 01:51:40  rubikitch
;; better error handling.
;;
;; Revision 1.163  2009/03/01 05:15:00  rubikitch
;; anything-iswitchb and anything-isearch are marked as unmaintained.
;; (document change only)
;;
;; Revision 1.162  2009/02/28 01:24:13  rubikitch
;; Symbols are now acceptable as candidate.
;;
;; Revision 1.161  2009/02/27 07:18:46  rubikitch
;; Fix bug of `anything-scroll-other-window' and `anything-scroll-other-window-down'.
;;
;; Revision 1.160  2009/02/27 01:05:06  rubikitch
;; * Make sure to restore point after running `anything-update-hook'.
;; * Make `anything-compute-matches' easy to find error.
;;
;; Revision 1.159  2009/02/26 23:45:48  rubikitch
;; * Check whether candidate is a string, otherwise ignore.
;;
;; Revision 1.158  2009/02/24 06:39:20  rubikitch
;; suppress compile warnings.
;;
;; Revision 1.157  2009/02/23 22:51:43  rubikitch
;; New function: `anything-document-attribute'
;;
;; Revision 1.156  2009/02/23 21:36:09  rubikitch
;; New Variable: `anything-display-source-at-screen-top'
;;
;; Revision 1.155  2009/02/23 21:30:52  rubikitch
;; New command: `anything-at-point'
;;
;; Revision 1.154  2009/02/23 08:57:54  rubikitch
;; Visible Mark
;;
;; Revision 1.153  2009/02/23 08:38:57  rubikitch
;; update doc
;;
;; Revision 1.152  2009/02/23 08:32:17  rubikitch
;; More key bindings.
;;
;; Revision 1.151  2009/02/23 08:21:24  rubikitch
;; `anything-map' is now Emacs-standard key bindings by default.
;; After evaluating `anything-iswitchb-setup'. some key bindings are adjusted to iswitchb.
;;
;; Revision 1.150  2009/02/20 22:58:18  rubikitch
;; Cancel timer in `anything-cleanup'.
;;
;; Revision 1.149  2009/02/20 12:23:44  rubikitch
;; `anything-header' face now inherits header-line (not a copy).
;;
;; Revision 1.148  2009/02/16 23:40:22  rubikitch
;; `real-to-display' attribute bug fix.
;;
;; Revision 1.147  2009/02/02 20:51:41  rubikitch
;; New `anything-sources' attribute: real-to-display
;;
;; Revision 1.146  2009/02/01 20:01:00  rubikitch
;; Update Tips
;;
;; Revision 1.145  2009/02/01 19:45:53  rubikitch
;; New variable: `anything-quit-if-no-candidate'
;;
;; Revision 1.144  2009/02/01 19:31:47  rubikitch
;; fixed a typo
;;
;; Revision 1.143  2009/02/01 19:23:32  rubikitch
;; New variable: `anything-execute-action-at-once-if-one'
;;
;; Revision 1.142  2009/02/01 19:12:34  rubikitch
;; `anything-persistent-action-display-buffer': bug fix
;;
;; Revision 1.141  2009/02/01 18:25:25  rubikitch
;; * fix docstring
;; * New variable: `anything-selection-face'
;;
;; Revision 1.140  2009/01/16 16:36:25  rubikitch
;; New variable: `anything-persistent-action-use-special-display'.
;;
;; Revision 1.139  2009/01/05 20:15:53  rubikitch
;; Fixed a bug of anything action buffer.
;; The action source should not be cached.
;;
;; Revision 1.138  2008/12/21 16:56:05  rubikitch
;; Fixed an error when action attribute is a function symbol and press TAB,
;;
;; Revision 1.137  2008/12/20 19:38:47  rubikitch
;; `anything-check-minibuffer-input-1': proper quit handling
;; `anything-process-delayed-sources': ditto
;;
;; Revision 1.136  2008/10/27 17:41:27  rubikitch
;; `anything-process-delayed-sources', `anything-check-minibuffer-input-1': quittable
;;
;; Revision 1.135  2008/10/27 17:04:25  rubikitch
;; arranged source, added more linkd tags (no code change)
;;
;; Revision 1.134  2008/10/27 15:02:25  rubikitch
;; New variable: `anything-save-configuration-functions'
;; Delete variable: `anything-save-configuration-type'
;;
;; Revision 1.133  2008/10/27 11:16:13  rubikitch
;; New variable: `anything-save-configuration-type'
;;
;; Revision 1.132  2008/10/26 22:34:59  rubikitch
;; `anything-delete-current-selection' with multiline
;;
;; Revision 1.131  2008/10/26 21:44:43  rubikitch
;; New command: `anything-delete-current-selection'
;;
;; Revision 1.130  2008/10/22 10:41:09  rubikitch
;; `anything-insert-match': do not override 'anything-realvalue property
;;
;; Revision 1.129  2008/10/21 17:01:37  rubikitch
;; `anything-resume' per buffer.
;; `anything-last-sources': obsolete
;;
;; Revision 1.128  2008/10/20 06:27:54  rubikitch
;; `anything-quick-update': new user option
;;
;; Revision 1.127  2008/10/20 05:47:49  rubikitch
;; refactoring
;;
;; Revision 1.126  2008/10/20 03:47:58  rubikitch
;; `anything-update': reversed order of delayed sources
;;
;; Revision 1.125  2008/10/19 00:29:54  rubikitch
;; kill buffer-local candidate buffers when creating global candidate buffers.
;;
;; Revision 1.124  2008/10/18 13:04:20  rubikitch
;; Remove tick entry from `anything-tick-hash' when killing a buffer.
;;
;; Revision 1.123  2008/10/18 10:23:36  rubikitch
;; multiline patch by Tomohiro MATSUYAMA.
;;
;; Revision 1.122  2008/10/13 03:10:07  rubikitch
;; `anything': do `anything-mark-current-line' when resuming
;;
;; Revision 1.121  2008/10/13 03:08:08  rubikitch
;; always set `anything-current-position'
;;
;; Revision 1.120  2008/10/07 14:12:02  rubikitch
;; `anything-execute-persistent-action': optional arg
;;
;; Revision 1.119  2008/10/06 06:43:29  rubikitch
;; `anything-candidate-buffer': return nil when the buffer is dead
;;
;; Revision 1.118  2008/09/30 22:21:28  rubikitch
;; New `anything-sources' attribute: accept-empty
;; dummy: include accept-empty
;;
;; Revision 1.117  2008/09/30 21:59:10  rubikitch
;; New function: `anything-buffer-is-modified'
;;
;; Revision 1.116  2008/09/22 11:27:29  rubikitch
;; *** empty log message ***
;;
;; Revision 1.115  2008/09/20 20:21:11  rubikitch
;; added linkd index. (no code change)
;;
;; Revision 1.114  2008/09/20 20:09:57  rubikitch
;; INCOMPATIBLE CHANGES: `anything-attr'
;; New functions: `anything-attrset', `anything-attr-defined'
;;
;; Revision 1.113  2008/09/14 15:15:32  rubikitch
;; bugfix: volatile and match attribute / process and match attribute
;;
;; Revision 1.112  2008/09/12 01:57:17  rubikitch
;; When resuming anything, reinitialize overlays.
;;
;; Revision 1.111  2008/09/10 22:53:11  rubikitch
;; anything: bug fix of `anything-buffer'
;; New macro: `anything-test-update'
;;
;; Revision 1.110  2008/09/10 22:17:11  rubikitch
;; New `anything-sources' attribute: header-name
;;
;; Revision 1.109  2008/09/10 21:12:26  rubikitch
;; New hook: `anything-after-action-hook'
;;
;; Revision 1.108  2008/09/06 06:07:56  rubikitch
;; Extended `anything-set-sources' optional arguments.
;;
;; Revision 1.107  2008/09/05 03:14:35  rubikitch
;; reimplement `anything-current-buffer-is-modified' in the right way
;;
;; Revision 1.106  2008/09/05 00:11:05  rubikitch
;; Moved `anything-read-string-mode' and read functions to anything-complete.el.
;;
;; Revision 1.105  2008/09/04 12:45:06  rubikitch
;; New hook: `anything-after-persistent-action-hook'
;;
;; Revision 1.104  2008/09/04 12:27:05  rubikitch
;; `anything': prefixed optional arguments
;;
;; Revision 1.103  2008/09/04 09:16:28  rubikitch
;; fixed a bug of `anything-read-file-name'.
;;
;; Revision 1.102  2008/09/03 11:25:19  rubikitch
;; Extended `anything' optional arguments: buffer
;;
;; Revision 1.101  2008/09/03 11:15:13  rubikitch
;; `anything': return nil when keybord-quitted
;;
;; Revision 1.100  2008/09/01 23:11:02  rubikitch
;; bug fix of search-from-end
;;
;; Revision 1.99  2008/09/01 13:45:55  rubikitch
;; bug fix of search-from-end
;;
;; Revision 1.98  2008/09/01 11:23:38  rubikitch
;; New `anything-sources' attribute: search-from-end
;;
;; Revision 1.97  2008/09/01 00:44:34  rubikitch
;; Make sure to display the other window when persistent action.
;;
;; Revision 1.96  2008/08/31 20:55:20  rubikitch
;; define `buffer-modified-tick' for older emacs.
;;
;; Revision 1.95  2008/08/30 04:55:51  rubikitch
;; fixed a bug of `anything-completing-read'
;;
;; Revision 1.94  2008/08/28 20:18:03  rubikitch
;; added some tests
;;
;; Revision 1.93  2008/08/25 20:18:46  rubikitch
;; `anything': set `anything-input' and `anything-pattern' before `anything-update'
;;
;; Revision 1.92  2008/08/24 22:38:46  rubikitch
;; *** empty log message ***
;;
;; Revision 1.91  2008/08/24 21:34:35  rubikitch
;; rewrite `with-anything-restore-variables'
;;
;; Revision 1.90  2008/08/24 20:33:02  rubikitch
;; prevent the unit test from byte-compiled.
;; macro bug fix.
;;
;; Revision 1.89  2008/08/24 08:35:27  rubikitch
;; *** empty log message ***
;;
;; Revision 1.88  2008/08/24 08:22:19  rubikitch
;; Rename `anything-candidates-buffer' -> `anything-candidate-buffer'
;;
;; Revision 1.87  2008/08/23 22:27:04  rubikitch
;; New hook: `anything-cleanup-hook'
;;
;; Revision 1.86  2008/08/23 22:05:42  rubikitch
;; `anything-original-source-filter' is removed.
;; Now use `anything-restored-variables' and `with-anything-restore-variables'.
;;
;; Revision 1.85  2008/08/23 21:23:21  rubikitch
;; inhibit-read-only = t in anything-buffer
;;
;; Revision 1.84  2008/08/23 21:18:33  rubikitch
;; *** empty log message ***
;;
;; Revision 1.83  2008/08/23 20:44:20  rubikitch
;; `anything-execute-persistent-action': display-to-real bug fix
;;
;; Revision 1.82  2008/08/23 20:19:12  rubikitch
;; New `anything-sources' attribute: get-line
;;
;; Revision 1.81  2008/08/23 19:32:14  rubikitch
;; `anything-attr': Return t in (attribute-name) case.
;;
;; Revision 1.80  2008/08/22 21:25:05  rubikitch
;; anything-candidates-in-buffer-1:
;; Open a line at the BOB to make use of `search-forward' for faster exact/prefix match.
;; Of course, restore the buffer contents after search.
;;
;; Revision 1.79  2008/08/22 17:11:00  rubikitch
;; New hook: `anything-before-initialize-hook', `anything-after-initialize-hook'
;;
;; Revision 1.78  2008/08/21 18:37:03  rubikitch
;; Implemented dummy sources as plug-in.
;;
;; Revision 1.77  2008/08/21 17:40:40  rubikitch
;; New function: `anything-set-sources'
;;
;; Revision 1.76  2008/08/21 12:25:02  rubikitch
;; New variable: `anything-version'
;;
;; Revision 1.75  2008/08/21 12:13:46  rubikitch
;; New variable: `anything-in-persistent-action'
;;
;; Revision 1.74  2008/08/21 10:34:22  rubikitch
;; New function `anything-mklist'
;;
;; Revision 1.73  2008/08/21 09:41:38  rubikitch
;; accept multiple init/cleanup functions so that plug-ins can add new function.
;;
;; Revision 1.72  2008/08/20 22:51:53  rubikitch
;; New `anything-sources' attribute: candidate-number-limit
;;
;; Revision 1.71  2008/08/20 21:45:42  rubikitch
;; added many tests.
;;
;; Revision 1.70  2008/08/20 18:51:45  rubikitch
;; `anything-preselect' bug fix.
;; refactoring.
;;
;; Revision 1.69  2008/08/20 17:57:51  rubikitch
;; Extended `anything' optional arguments: preselect
;;
;; Revision 1.68  2008/08/20 16:39:07  rubikitch
;; Nested `anything' invocation support, ie. `anything' can be invoked by anything action.
;;
;; (anything '(((name . "nested anything invocation test")
;;              (candidates "anything-c-source-buffers" "anything-c-source-man-pages")
;;              (display-to-real . intern)
;;              (action . anything))))
;;
;; Revision 1.67  2008/08/20 00:08:28  rubikitch
;; `anything-candidates-in-buffer-1': add code when pattern == ""
;;
;; Revision 1.66  2008/08/19 23:31:52  rubikitch
;; Removed `anything-show-exact-match-first' because it should be provided as a plug-in.
;;
;; Revision 1.65  2008/08/19 23:18:47  rubikitch
;; *** empty log message ***
;;
;; Revision 1.64  2008/08/19 23:15:43  rubikitch
;; `anything-compute-matches': short-cut when match == '(identity)
;;
;; Revision 1.63  2008/08/19 23:06:42  rubikitch
;; Use hash table to speed uniquify candidates.
;;
;; Revision 1.62  2008/08/19 22:40:57  rubikitch
;; `anything-test-candidates': additional optonal argument
;;
;; Revision 1.61  2008/08/19 18:13:39  rubikitch
;; search attribute: multiple search functions
;;
;; Revision 1.60  2008/08/19 15:07:39  rubikitch
;; New function: `anything-attr'
;;
;; Revision 1.59  2008/08/19 15:01:59  rubikitch
;; arranged code
;; added unit tests
;; update doc
;;
;; Revision 1.58  2008/08/19 13:40:52  rubikitch
;; `anything-get-current-source': This function can be used in
;;  init/candidates/action/candidate-transformer/filtered-candidate-transformer
;;  display-to-real/cleanup function.
;;
;; Revision 1.57  2008/08/19 03:43:57  rubikitch
;; `anything-process-delayed-sources': delay = anything-idle-delay - anything-input-idle-delay
;;
;; Revision 1.56  2008/08/18 06:37:51  rubikitch
;; Make `anything-input-idle-delay' ineffective when the action list is shown.
;;
;; Revision 1.55  2008/08/18 06:35:00  rubikitch
;; New variable: `anything-show-exact-match-first'
;;
;; Revision 1.54  2008/08/17 23:22:24  rubikitch
;; *** empty log message ***
;;
;; Revision 1.53  2008/08/17 23:15:38  rubikitch
;; bind `anything-source-name' when executing action to enable to use `anything-candidate-buffer' in action.
;;
;; Revision 1.52  2008/08/17 15:21:27  rubikitch
;; `anything-test-candidates': accept a symbol for source
;; New variable: `anything-input-idle-delay'
;;
;; Revision 1.51  2008/08/17 12:45:30  rubikitch
;; (buffer-disable-undo) in anything-buffer
;;
;; Revision 1.50  2008/08/16 22:21:37  rubikitch
;; `anything-saved-sources': removed
;; `anything-action-buffer': action selection buffer
;; `anything-select-action': toggle actions <=> candidates
;;
;; Revision 1.49  2008/08/16 19:46:11  rubikitch
;; New function: `anything-action-list-is-shown'
;;
;; Revision 1.48  2008/08/16 17:03:02  rubikitch
;; bugfix: cleanup
;;
;; Revision 1.47  2008/08/16 16:35:24  rubikitch
;; silence byte compiler
;;
;; Revision 1.46  2008/08/16 14:51:27  rubikitch
;; *** empty log message ***
;;
;; Revision 1.45  2008/08/16 11:27:59  rubikitch
;; refactoring
;;  `anything-aif': Anaphoric if.
;;  `anything-compile-source-functions': make `anything-get-sources' customizable.
;;
;; Revision 1.44  2008/08/16 09:38:15  rubikitch
;; *** empty log message ***
;;
;; Revision 1.43  2008/08/15 11:44:28  rubikitch
;; `anything-read-string-mode': minor mode for `anything' version of read functions. (experimental)
;;
;; Revision 1.42  2008/08/15 11:03:20  rubikitch
;; update docs
;;
;; Revision 1.41  2008/08/14 20:51:28  rubikitch
;; New `anything-sources' attribute: cleanup
;;
;; Revision 1.40  2008/08/14 10:34:04  rubikitch
;; `anything': SOURCES: accept symbols
;;
;; Revision 1.39  2008/08/10 22:46:01  rubikitch
;; `anything-move-selection': avoid infinite loop
;;
;; Revision 1.38  2008/08/09 21:38:25  rubikitch
;; `anything-read-file-name': experimental implementation.
;;
;; Revision 1.37  2008/08/09 17:54:25  rubikitch
;; action test
;;
;; Revision 1.36  2008/08/09 17:13:00  rubikitch
;; fixed test
;;
;; Revision 1.35  2008/08/09 10:43:08  rubikitch
;; New `anything-sources' attribute: display-to-real
;;
;; Revision 1.34  2008/08/07 13:15:44  rubikitch
;; New `anything-sources' attribute: search
;;
;; Revision 1.33  2008/08/05 23:14:20  rubikitch
;; `anything-candidate-buffer': bugfix
;;
;; Revision 1.32  2008/08/05 21:42:15  rubikitch
;; *** empty log message ***
;;
;; Revision 1.31  2008/08/05 21:06:23  rubikitch
;; `anything-candidate-buffer': candidates buffer registration
;;
;; Revision 1.30  2008/08/05 19:46:36  rubikitch
;; New `anything-sources' attribute: candidates-in-buffer
;;
;; Revision 1.29  2008/08/05 17:58:31  rubikitch
;; *** empty log message ***
;;
;; Revision 1.28  2008/08/05 17:46:04  rubikitch
;; memoized `anything-get-sources'
;;
;; Revision 1.27  2008/08/05 17:29:40  rubikitch
;; update doc
;;
;; Revision 1.26  2008/08/05 08:35:45  rubikitch
;; `anything-completing-read': accept obarray
;;
;; Revision 1.25  2008/08/05 07:26:17  rubikitch
;; `anything-completing-read': guard from non-string return value
;;
;; Revision 1.24  2008/08/04 12:05:41  rubikitch
;; Wrote Tips and some docstrings.
;; `anything-candidate-buffer': buffer-local by default
;;
;; Revision 1.23  2008/08/04 05:29:46  rubikitch
;; `anything-buffer-file-name': `buffer-file-name' when `anything' is invoked.
;;
;; Revision 1.22  2008/08/04 00:10:13  rubikitch
;; `anything-candidate-buffer': new API
;;
;; Revision 1.21  2008/08/03 22:05:08  rubikitch
;; `anything-candidate-buffer': Return a buffer containing candidates of current source.
;;
;; Revision 1.20  2008/08/03 20:47:56  rubikitch
;; `anything-current-buffer-is-modified': modify checker
;;
;; Revision 1.19  2008/08/03 19:06:18  rubikitch
;; `anything-candidates-in-buffer': use `with-current-buffer' instead.
;;
;; Revision 1.18  2008/08/03 05:55:01  rubikitch
;; `anything-candidates-in-buffer': extract candidates in a buffer for speed.
;;
;; Revision 1.17  2008/08/02 21:31:29  rubikitch
;; Extended `anything' optional arguments.
;; `anything-completing-read': experimental implementation.
;;
;; Revision 1.16  2008/08/02 20:32:54  rubikitch
;; Extended `anything' optional arguments.
;;
;; Revision 1.15  2008/08/02 16:53:40  rubikitch
;; Fixed a small bug of `anything-test-candidates'.
;;
;; Revision 1.14  2008/08/02 16:48:29  rubikitch
;; Refactored to testable code.
;; Added many candidate tests with `anything-test-candidates'.
;;
;; Revision 1.13  2008/08/02 15:08:14  rubikitch
;; *** empty log message ***
;;
;; Revision 1.12  2008/08/02 14:29:31  rubikitch
;; `anything-sources' accepts symbols. (patched by Sugawara)
;;
;; Revision 1.11  2008/08/02 10:20:36  rubikitch
;; `anything-resume' is usable with other (let-binded) `anything-sources'.
;;
;; Revision 1.10  2008/08/01 19:44:01  rubikitch
;; `anything-resume': resurrect previously invoked `anything'.
;;
;; Revision 1.9  2008/07/30 15:44:49  rubikitch
;; *** empty log message ***
;;
;; Revision 1.8  2008/07/30 15:38:51  rubikitch
;; *** empty log message ***
;;
;; Revision 1.7  2008/07/30 15:21:48  rubikitch
;; `anything-scroll-other-window', `anything-scroll-other-window-down':
;; Scroll other window (for persistent action).
;;
;; Revision 1.6  2008/07/30 15:12:36  rubikitch
;; *** empty log message ***
;;
;; Revision 1.5  2008/07/30 15:06:32  rubikitch
;; `anything-select-2nd-action', `anything-select-3rd-action', `anything-select-4th-action':
;; Select other than default action without pressing Tab.
;;
;; Revision 1.4  2008/07/30 14:58:27  rubikitch
;; `anything-current-buffer': Store current buffer when `anything' is invoked.
;; `anything-current-position': Restore position when keyboard-quitted.
;;
;; Revision 1.3  2008/07/30 14:38:04  rubikitch
;; Implemented persistent action.
;;
;; Revision 1.2  2008/07/30 13:37:16  rubikitch
;; Update doc.
;;
;; Revision 1.1  2008/07/30 13:22:06  rubikitch
;; New maintainer.
;;

(defvar anything-version "$Id: anything.el,v 1.201 2009/08/08 13:25:30 rubikitch Exp rubikitch $")
(require 'cl)

;; (@* "User Configuration")

;; This is only an example. Customize it to your own taste!
(defvar anything-sources `(((name . "Buffers")
                            (candidates
                             . (lambda ()
                                 (remove-if (lambda (name)
                                              (or (equal name anything-buffer)
                                                  (eq ?\  (aref name 0))))
                                            (mapcar 'buffer-name (buffer-list)))))
			    (type . buffer))

                           ((name . "File Name History")
                            (candidates . file-name-history)
                            (match (lambda (candidate)
                                     ;; list basename matches first
                                     (string-match 
                                      anything-pattern 
                                      (file-name-nondirectory candidate)))

                                   (lambda (candidate)                                     
                                     ;; and then directory part matches
                                     (let ((dir (file-name-directory candidate)))
                                       (if dir
                                           (string-match anything-pattern dir)))))
                            (type . file))

                           ((name . "Files from Current Directory")
                            (init . (lambda ()
                                      (setq anything-default-directory
                                            default-directory)))
                            (candidates . (lambda ()
                                            (directory-files
                                             anything-default-directory)))
                            (type . file))

                           ((name . "Manual Pages")
                            (candidates . ,(progn
                                             ;; XEmacs doesn't have a woman :)
                                             (declare (special woman-file-name
                                                               woman-topic-all-completions))
                                             (condition-case nil
                                                 (progn
                                                   (require 'woman)
                                                   (woman-file-name "")
                                                   (sort (mapcar 'car
                                                                 woman-topic-all-completions)
                                                         'string-lessp))
                                               (error nil))))
                            (action . (("Open Manual Page" . woman)))
                            (requires-pattern . 2))

                           ((name . "Complex Command History")
                            (candidates . (lambda ()
                                            (mapcar 'prin1-to-string
                                                    command-history)))
                            (action . (("Repeat Complex Command" . 
                                        (lambda (c)
                                          (eval (read c))))))
                            (delayed)))
  "The source of candidates for anything.
It accepts symbols:
 (setq anything-sources (list anything-c-foo anything-c-bar))
can be written as
 (setq anything-sources '(anything-c-foo anything-c-bar))
The latter is recommended because if you change anything-c-* variable,
you do not have to update `anything-sources'.

If you want to change `anything-sources' during `anything' invocation,
use `anything-set-sources', never use `setq'.

Attributes:

- name (mandatory)

  The name of the source. It is also the heading which appears
  above the list of matches from the source. Must be unique.

- header-name (optional)

  A function returning the display string of the header. Its
  argument is the name of the source. This attribute is useful to
  add an additional information with the source name.

- candidates (mandatory if candidates-in-buffer attribute is not provided)

  Specifies how to retrieve candidates from the source. It can
  either be a variable name, a function called with no parameters
  or the actual list of candidates.

  The list must be a list whose members are strings, symbols
  or (DISPLAY . REAL) pairs.

  In case of (DISPLAY . REAL) pairs, the DISPLAY string is shown
  in the Anything buffer, but the REAL one is used as action
  argument when the candidate is selected. This allows a more
  readable presentation for candidates which would otherwise be,
  for example, too long or have a common part shared with other
  candidates which can be safely replaced with an abbreviated
  string for display purposes.

  Note that if the (DISPLAY . REAL) form is used then pattern
  matching is done on the displayed string, not on the real
  value.

  If the candidates have to be retrieved asynchronously (for
  example, by an external command which takes a while to run)
  then the function should start the external command
  asynchronously and return the associated process object.
  Anything will take care of managing the process (receiving the
  output from it, killing it if necessary, etc.). The process
  should return candidates matching the current pattern (see
  variable `anything-pattern'.)

  Note that currently results from asynchronous sources appear
  last in the anything buffer regardless of their position in
  `anything-sources'.

- action (mandatory if type attribute is not provided)

  It is a list of (DISPLAY . FUNCTION) pairs or FUNCTION.
  FUNCTION is called with one parameter: the selected candidate.

  An action other than the default can be chosen from this list
  of actions for the currently selected candidate (by default
  with TAB). The DISPLAY string is shown in the completions
  buffer and the FUNCTION is invoked when an action is
  selected. The first action of the list is the default.

- type (optional if action attribute is provided)

  Indicates the type of the items the source returns. 

  Merge attributes not specified in the source itself from
  `anything-type-attributes'.

  This attribute is implemented by plug-in.

- init (optional)

  Function called with no parameters when anything is started. It
  is useful for collecting current state information which can be
  used to create the list of candidates later.

  For example, if a source needs to work with the current
  directory then it can store its value here, because later
  anything does its job in the minibuffer and in the
  `anything-buffer' and the current directory can be different
  there.

- match (optional)

  List of functions called with one parameter: a candidate. The
  function should return non-nil if the candidate matches the
  current pattern (see variable `anything-pattern').

  This attribute allows the source to override the default
  pattern matching based on `string-match'. It can be used, for
  example, to implement a source for file names and do the
  pattern matching on the basename of files, since it's more
  likely one is typing part of the basename when searching for a
  file, instead of some string anywhere else in its path.

  If the list contains more than one function then the list of
  matching candidates from the source is constructed by appending
  the results after invoking the first function on all the
  potential candidates, then the next function, and so on. The
  matching candidates supplied by the first function appear first
  in the list of results and then results from the other
  functions, respectively.

  This attribute has no effect for asynchronous sources (see
  attribute `candidates'), since they perform pattern matching
  themselves.

- candidate-transformer (optional)

  It's a function or a list of functions called with one argument
  when the completion list from the source is built. The argument
  is the list of candidates retrieved from the source. The
  function should return a transformed list of candidates which
  will be used for the actual completion.  If it is a list of
  functions, it calls each function sequentially.

  This can be used to transform or remove items from the list of
  candidates.

  Note that `candidates' is run already, so the given transformer
  function should also be able to handle candidates with (DISPLAY
  . REAL) format.

- filtered-candidate-transformer (optional)

  It has the same format as `candidate-transformer', except the
  function is called with two parameters: the candidate list and
  the source.

  This transformer is run on the candidate list which is already
  filtered by the current pattern. While `candidate-transformer'
  is run only once, it is run every time the input pattern is
  changed.

  It can be used to transform the candidate list dynamically, for
  example, based on the current pattern.

  In some cases it may also be more efficent to perform candidate
  transformation here, instead of with `candidate-transformer'
  even if this transformation is done every time the pattern is
  changed.  For example, if a candidate set is very large then
  `candidate-transformer' transforms every candidate while only
  some of them will actually be dislpayed due to the limit
  imposed by `anything-candidate-number-limit'.

  Note that `candidates' and `candidate-transformer' is run
  already, so the given transformer function should also be able
  to handle candidates with (DISPLAY . REAL) format.

  This option has no effect for asynchronous sources. (Not yet,
  at least.

- action-transformer (optional)

  It's a function or a list of functions called with two
  arguments when the action list from the source is
  assembled. The first argument is the list of actions, the
  second is the current selection.  If it is a list of functions,
  it calls each function sequentially.

  The function should return a transformed action list.

  This can be used to customize the list of actions based on the
  currently selected candidate.

- delayed (optional)

  Candidates from the source are shown only if the user stops
  typing and is idle for `anything-idle-delay' seconds.

- volatile (optional)

  Indicates the source assembles the candidate list dynamically,
  so it shouldn't be cached within a single Anything
  invocation. It is only applicable to synchronous sources,
  because asynchronous sources are not cached.

- requires-pattern (optional)

  If present matches from the source are shown only if the
  pattern is not empty. Optionally, it can have an integer
  parameter specifying the required length of input which is
  useful in case of sources with lots of candidates.

- persistent-action (optional)

  Function called with one parameter; the selected candidate.

  An action performed by `anything-execute-persistent-action'.
  If none, use the default action.

- candidates-in-buffer (optional)

  Shortcut attribute for making and narrowing candidates using
  buffers.  This newly-introduced attribute prevents us from
  forgetting to add volatile and match attributes.

  See docstring of `anything-candidates-in-buffer'.

  (candidates-in-buffer) is equivalent of three attributes:
    (candidates . anything-candidates-in-buffer)
    (volatile)
    (match identity)

  (candidates-in-buffer . candidates-function) is equivalent of:
    (candidates . candidates-function)
    (volatile)
    (match identity)

  This attribute is implemented by plug-in.

- search (optional)

  List of functions like `re-search-forward' or `search-forward'.
  Buffer search function used by `anything-candidates-in-buffer'.
  By default, `anything-candidates-in-buffer' uses `re-search-forward'.
  This attribute is meant to be used with
  (candidates . anything-candidates-in-buffer) or
  (candidates-in-buffer) in short.

- search-from-end (optional)

  Make `anything-candidates-in-buffer' search from the end of buffer.
  If this attribute is specified, `anything-candidates-in-buffer' uses
  `re-search-backward' instead.

- get-line (optional)

  A function like `buffer-substring-no-properties' or `buffer-substring'.
  This function converts point of line-beginning and point of line-end,
  which represents a candidate computed by `anything-candidates-in-buffer'.
  By default, `anything-candidates-in-buffer' uses
  `buffer-substring-no-properties'.

- display-to-real (optional)

  Function called with one parameter; the selected candidate.

  The function transforms the selected candidate, and the result
  is passed to the action function.  The display-to-real
  attribute provides another way to pass other string than one
  shown in Anything buffer.

  Traditionally, it is possible to make candidates,
  candidate-transformer or filtered-candidate-transformer
  function return a list with (DISPLAY . REAL) pairs. But if REAL
  can be generated from DISPLAY, display-to-real is more
  convenient and faster.

- real-to-display (optional)

  Function called with one parameter; the selected candidate.

  The inverse of display-to-real attribute.

  The function transforms the selected candidate, which is passed
  to the action function, for display.  The real-to-display
  attribute provides the other way to pass other string than one
  shown in Anything buffer.

  Traditionally, it is possible to make candidates,
  candidate-transformer or filtered-candidate-transformer
  function return a list with (DISPLAY . REAL) pairs. But if
  DISPLAY can be generated from REAL, real-to-display is more
  convenient and faster.

- cleanup (optional)

  Function called with no parameters when *anything* buffer is closed. It
  is useful for killing unneeded candidates buffer.

  Note that the function is executed BEFORE performing action.

- candidate-number-limit (optional)

  Override `anything-candidate-number-limit' only for this source.

- accept-empty (optional)

  Pass empty string \"\" to action function.

- dummy (optional)

  Set `anything-pattern' to candidate. If this attribute is
  specified, The candidates attribute is ignored.

  This attribute is implemented by plug-in.

- multiline (optional)

  Enable to selection multiline candidates.
")


;; This value is only provided as an example. Customize it to your own
;; taste!
(defvar anything-type-attributes
  '((file (action . (("Find File" . find-file)
                     ("Delete File" . (lambda (file)
                                        (if (y-or-n-p (format "Really delete file %s? "
                                                              file))
                                            (delete-file file)))))))
    (buffer (action . (("Switch to Buffer" . switch-to-buffer)
                       ("Pop to Buffer"    . pop-to-buffer)
                       ("Display Buffer"   . display-buffer)
                       ("Kill Buffer"      . kill-buffer)))))
  "It's a list of (TYPE ATTRIBUTES ...). ATTRIBUTES are the same
  as attributes for `anything-sources'. TYPE connects the value
  to the appropriate sources in `anything-sources'.

  This allows specifying common attributes for several
  sources. For example, sources which provide files can specify
  common attributes with a `file' type.")


(defvar anything-enable-digit-shortcuts nil
  "*If t then the first nine matches can be selected using
  Ctrl+<number>.")

(defvar anything-display-source-at-screen-top t
  "*If t, `anything-next-source' and `anything-previous-source'
  display candidates at the top of screen.")

(defvar anything-candidate-number-limit 50
  "*Do not show more candidates than this limit from individual
  sources. It is usually pointless to show hundreds of matches
  when the pattern is empty, because it is much simpler to type a
  few characters to narrow down the list of potential candidates.

  Set it to nil if you don't want this limit.")


(defvar anything-idle-delay 0.5
  "*The user has to be idle for this many seconds, before
  candidates from delayed sources are collected. This is useful
  for sources involving heavy operations (like launching external
  programs), so that candidates from the source are not retrieved
  unnecessarily if the user keeps typing.

  It also can be used to declutter the results anything displays,
  so that results from certain sources are not shown with every
  character typed, only if the user hesitates a bit.")


(defvar anything-input-idle-delay 0.1
  "The user has to be idle for this many seconds, before ALL candidates are collected.
Unlink `anything-input-idle', it is also effective for non-delayed sources.
If nil, candidates are collected immediately. ")


(defvar anything-samewindow nil
  "If t then Anything doesn't pop up a new window, it uses the
current window to show the candidates.")


(defvar anything-source-filter nil
  "A list of source names to be displayed. Other sources won't
appear in the search results. If nil then there is no filtering.
See also `anything-set-source-filter'.")


(defvar anything-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "<down>") 'anything-next-line)
    (define-key map (kbd "<up>") 'anything-previous-line)
    (define-key map (kbd "C-n")     'anything-next-line)
    (define-key map (kbd "C-p")     'anything-previous-line)
    (define-key map (kbd "<prior>") 'anything-previous-page)
    (define-key map (kbd "<next>") 'anything-next-page)
    (define-key map (kbd "M-v")     'anything-previous-page)
    (define-key map (kbd "C-v")     'anything-next-page)
    (define-key map (kbd "<right>") 'anything-next-source)
    (define-key map (kbd "<left>") 'anything-previous-source)
    (define-key map (kbd "<RET>") 'anything-exit-minibuffer)
    (define-key map (kbd "C-1") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-2") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-3") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-4") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-5") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-6") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-7") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-8") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-9") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-i") 'anything-select-action)
    (define-key map (kbd "C-z") 'anything-execute-persistent-action)
    (define-key map (kbd "C-e") 'anything-select-2nd-action-or-end-of-line)
    (define-key map (kbd "C-j") 'anything-select-3rd-action)
    (define-key map (kbd "C-o") 'anything-next-source)
    (define-key map (kbd "C-M-v") 'anything-scroll-other-window)
    (define-key map (kbd "M-<next>") 'anything-scroll-other-window)
    (define-key map (kbd "C-M-y") 'anything-scroll-other-window-down)
    (define-key map (kbd "C-M-S-v") 'anything-scroll-other-window-down)
    (define-key map (kbd "M-<prior>") 'anything-scroll-other-window-down)
    (define-key map (kbd "C-SPC") 'anything-toggle-visible-mark)
    (define-key map (kbd "M-[") 'anything-prev-visible-mark)
    (define-key map (kbd "M-]") 'anything-next-visible-mark)
    (define-key map (kbd "C-k") 'anything-delete-minibuffer-content)

    (define-key map (kbd "C-s") 'anything-isearch)
    (define-key map (kbd "C-r") 'undefined)
    (define-key map (kbd "C-x C-f") 'anything-quit-and-find-file)

    (define-key map (kbd "C-c C-d") 'anything-delete-current-selection)
    (define-key map (kbd "C-c C-y") 'anything-yank-selection)
    (define-key map (kbd "C-c C-k") 'anything-kill-selection-and-quit)
    (define-key map (kbd "C-c C-f") 'anything-follow-mode)

    ;; the defalias is needed because commands are bound by name when
    ;; using iswitchb, so only commands having the prefix anything-
    ;; get rebound
    (defalias 'anything-previous-history-element 'previous-history-element)
    (defalias 'anything-next-history-element 'next-history-element)
    (define-key map (kbd "M-p") 'anything-previous-history-element)
    (define-key map (kbd "M-n") 'anything-next-history-element)
    map)
  "Keymap for anything.

If you execute `anything-iswitchb-setup', some keys are modified.
See `anything-iswitchb-setup-keys'.")

(defvar anything-isearch-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-global-map))
    (define-key map (kbd "<return>") 'anything-isearch-default-action)
    (define-key map (kbd "<RET>") 'anything-isearch-default-action)
    (define-key map (kbd "C-i") 'anything-isearch-select-action)
    (define-key map (kbd "C-g") 'anything-isearch-cancel)
    (define-key map (kbd "M-s") 'anything-isearch-again)
    (define-key map (kbd "<backspace>") 'anything-isearch-delete)
    ;; add printing chars
    (loop for i from 32 below 256 do
          (define-key map (vector i) 'anything-isearch-printing-char))
    map)
  "Keymap for anything incremental search.")


(defgroup anything nil
  "Open anything." :prefix "anything-" :group 'convenience)

(defface anything-header 
  '((t (:inherit header-line))) 
  "Face for header lines in the anything buffer." :group 'anything)

(defvar anything-header-face 'anything-header
  "Face for header lines in the anything buffer.")

(defface anything-isearch-match '((t (:background "Yellow")))
  "Face for isearch in the anything buffer." :group 'anything)

(defvar anything-isearch-match-face 'anything-isearch-match
  "Face for matches during incremental search.")

(defvar anything-selection-face 'highlight
  "Face for currently selected item.")

(defvar anything-iswitchb-idle-delay 1
  "Show anything completions if the user is idle that many
  seconds after typing.")

(defvar anything-iswitchb-dont-touch-iswithcb-keys nil
  "If t then those commands are not bound from `anything-map'
  under iswitchb which would override standard iswithcb keys.

This allows an even more seamless integration with iswitchb for
those who prefer using iswitchb bindings even if the anything
completions buffer is popped up.

Note that you can bind alternative keys for the same command in
`anything-map', so that you can use different keys for anything
under iswitchb. For example, I bind the character \ to
`anything-exit-minibuffer' which key is just above Enter on my
keyboard. This way I can switch buffers with Enter and choose
anything completions with \.")

;;----------------------------------------------------------------------

(defvar anything-buffer "*anything*"
  "Buffer showing completions.")

(defvar anything-action-buffer "*anything action*"
  "Buffer showing actions.")

(defvar anything-selection-overlay nil
  "Overlay used to highlight the currently selected item.")

(defvar anything-isearch-overlay nil
  "Overlay used to highlight the current match during isearch.")

(defvar anything-digit-overlays nil
  "Overlays for digit shortcuts. See `anything-enable-digit-shortcuts'.")

(defvar anything-candidate-cache nil
  "Holds the available candidate withing a single anything invocation.")

(defvar anything-pattern
  "The input pattern used to update the anything buffer.")

(defvar anything-input
  "The input typed in the candidates panel.")

(defvar anything-async-processes nil
  "List of information about asynchronous processes managed by anything.")

(defvar anything-digit-shortcut-count 0
  "Number of digit shortcuts shown in the anything buffer.")

(defvar anything-before-initialize-hook nil
  "Run before anything initialization.
This hook is run before init functions in `anything-sources'.")

(defvar anything-after-initialize-hook nil
  "Run after anything initialization.
Global variables are initialized and the anything buffer is created.
But the anything buffer has no contents. ")

(defvar anything-update-hook nil
  "Run after the anything buffer was updated according the new
  input pattern.")

(defvar anything-cleanup-hook nil
  "Run after anything minibuffer is closed, IOW this hook is executed BEFORE performing action. ")

(defvar anything-after-action-hook nil
  "Run after executing action.")

(defvar anything-after-persistent-action-hook nil
  "Run after executing persistent action.")

(defvar anything-restored-variables
  '( anything-candidate-number-limit
     anything-source-filter)
  "Variables which are restored after `anything' invocation.")
;; `anything-saved-sources' is removed

(defvar anything-saved-selection nil
  "Saved value of the currently selected object when the action
  list is shown.")

;; `anything-original-source-filter' is removed

(defvar anything-candidate-separator
  "--------------------"
  "Candidates separator of `multiline' source.")

(defvar anything-current-buffer nil
  "Current buffer when `anything' is invoked.")

(defvar anything-buffer-file-name nil
  "`buffer-file-name' when `anything' is invoked.")

(defvar anything-current-position nil
  "Cons of (point) and (window-start) when `anything' is invoked.
It is needed because restoring position when `anything' is keyboard-quitted.")

(defvar anything-saved-action nil
  "Saved value of the currently selected action by key.")

(defvar anything-last-sources nil
  "OBSOLETE!! Sources of previously invoked `anything'.")

(defvar anything-saved-current-source nil
  "Saved value of the original (anything-get-current-source) when the action
  list is shown.")

(defvar anything-compiled-sources nil
  "Compiled version of `anything-sources'. ")

(defvar anything-in-persistent-action nil
  "Flag whether in persistent-action or not.")

(defvar anything-quick-update nil
  "If non-nil, suppress displaying sources which are out of screen at first.
They are treated as delayed sources at this input.
This flag makes `anything' a bit faster with many sources.")

(defvar anything-last-sources-local nil
  "Buffer local value of `anything-sources'.")
(defvar anything-last-buffer nil
  "`anything-buffer' of previously `anything' session.")

(defvar anything-save-configuration-functions
  '(set-frame-configuration . current-frame-configuration)
  "If you hate flickering, set this variable to
 '(set-window-configuration . current-window-configuration)
")

(defvar anything-persistent-action-use-special-display nil
  "If non-nil, use `special-display-function' in persistent action.")

(defvar anything-execute-action-at-once-if-one nil
  "If non-nil and there is one candidate, execute the first action without selection.
It is useful for `anything' applications.")

(defvar anything-quit-if-no-candidate nil
  "if non-nil and there is no candidate, do not display *anything* buffer and quit.
This variable accepts a function, which is executed if no candidate.

It is useful for `anything' applications.")

(defvar anything-scroll-amount nil
  "Scroll amount used by `anything-scroll-other-window' and `anything-scroll-other-window-down'.
If you prefer scrolling line by line, set this value to 1.")

(defvar anything-display-function 'anything-default-display-buffer
  "Function to display *anything* buffer.
It is `anything-default-display-buffer' by default, which affects `anything-samewindow'.")

(put 'anything 'timid-completion 'disabled)

;; (@* "Internal Variables")
(defvar anything-test-candidate-list nil)
(defvar anything-test-mode nil)
(defvar anything-source-name nil)
(defvar anything-candidate-buffer-alist nil)
(defvar anything-check-minibuffer-input-timer nil)
(defvar anything-match-hash (make-hash-table :test 'equal))
(defvar anything-cib-hash (make-hash-table :test 'equal))
(defvar anything-tick-hash (make-hash-table :test 'equal))
(defvar anything-issued-errors nil)

;; (@* "Programming Tools")
(defmacro anything-aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))  
(put 'anything-aif 'lisp-indent-function 2)

(defun anything-mklist (obj)
  "If OBJ is a list, return itself, otherwise make a list with one element."
  (if (listp obj) obj (list obj)))

;; (@* "Anything API")
(defun anything-buffer-get ()
  "If *anything action* buffer is shown, return `anything-action-buffer', otherwise `anything-buffer'."
  (if (anything-action-window)
      anything-action-buffer
    anything-buffer))

(defun anything-window ()
  "Window of `anything-buffer'."
  (get-buffer-window (anything-buffer-get) 'visible))

(defun anything-action-window ()
  "Window of `anything-action-buffer'."
  (get-buffer-window anything-action-buffer 'visible))

(defmacro with-anything-window (&rest body)
  `(let ((--tmpfunc-- (lambda () ,@body)))
     (if anything-test-mode
         (with-current-buffer (anything-buffer-get)
           (funcall --tmpfunc--))
       (with-selected-window (anything-window)
         (funcall --tmpfunc--)))))
(put 'with-anything-window 'lisp-indent-function 0)

(defmacro with-anything-restore-variables(&rest body)
  "Restore variables specified by `anything-restored-variables' after executing BODY ."
  `(let ((--orig-vars (mapcar (lambda (v) (cons v (symbol-value v))) anything-restored-variables)))
     (unwind-protect (progn ,@body)
       (loop for (var . value) in --orig-vars
             do (set var value)))))
(put 'with-anything-restore-variables 'lisp-indent-function 0)

(defun* anything-attr (attribute-name &optional (src (anything-get-current-source)))
  "Get the value of ATTRIBUTE-NAME of SRC (source).
if SRC is omitted, use current source.
It is useful to write your sources."
  (anything-aif (assq attribute-name src)
      (cdr it)))

(defun* anything-attr-defined (attribute-name &optional (src (anything-get-current-source)))
  "Return non-nil if ATTRIBUTE-NAME of SRC (source)  is defined.
if SRC is omitted, use current source.
It is useful to write your sources."
  (and (assq attribute-name src) t))

(defun* anything-attrset (attribute-name value &optional (src (anything-get-current-source)))
  "Set the value of ATTRIBUTE-NAME of SRC (source) to VALUE.
if SRC is omitted, use current source.
It is useful to write your sources."
  (anything-aif (assq attribute-name src)
      (setcdr it value)
    (setcdr src (cons (cons attribute-name value) (cdr src))))
  value)

;; anything-set-source-filter
;;
;;   This function sets a filter for anything sources and it may be
;;   called while anything is running. It can be used to toggle
;;   displaying of sources dinamically. For example, additional keys
;;   can be bound into `anything-map' to display only the file-related
;;   results if there are too many matches from other sources and
;;   you're after files only:
;;
;;   Shift+F shows only file results from some sources:
;;
;;     (define-key anything-map "F" 'anything-my-show-files-only)
;;     
;;     (defun anything-my-show-files-only ()
;;       (interactive)
;;       (anything-set-source-filter '("File Name History"
;;                                     "Files from Current Directory")))
;;
;;   Shift+A shows all results:
;;
;;     (define-key anything-map "A" 'anything-my-show-all)
;;     
;;     (defun anything-my-show-all ()
;;       (interactive)
;;       (anything-set-source-filter nil))
;;  
;;  
;;   Note that you have to prefix the functions with anything- prefix,
;;   otherwise they won't be bound when Anything is used under
;;   Iswitchb. The -my- part is added to avoid collisions with
;;   existing Anything function names.
;;  
(defun anything-set-source-filter (sources)
  "Sets the value of `anything-source-filter' and updates the list of results."
  (setq anything-source-filter sources)
  (anything-update))

(defun anything-set-sources (sources &optional no-init no-update)
  "Set `anything-sources' during `anything' invocation.
If NO-INIT is non-nil, skip executing init functions of SOURCES.
If NO-UPDATE is non-nil, skip executing `anything-update'."
  (setq anything-compiled-sources nil
        anything-sources sources)
  (unless no-init (anything-funcall-foreach 'init))
  (unless no-update (anything-update)))

(defvar anything-compile-source-functions
  '(anything-compile-source--type anything-compile-source--dummy anything-compile-source--candidates-in-buffer)
  "Functions to compile elements of `anything-sources' (plug-in).")

(defun anything-get-sources ()
  "Return compiled `anything-sources', which is memoized.

Attributes:

- type
  `anything-type-attributes' are merged in.
- candidates-buffer
  candidates, volatile and match attrubute are created.
"
  (cond
   ;; action
   ((anything-action-window)
    anything-sources)
   ;; memoized
   (anything-compiled-sources)
   ;; first time
   (t
    (setq anything-compiled-sources
          (anything-compile-sources anything-sources anything-compile-source-functions)))))

(defun* anything-get-selection (&optional (buffer nil buffer-s) (force-display-part))
  "Return the currently selected item or nil.
if BUFFER is nil or unspecified, use anything-buffer as default value.
If FORCE-DISPLAY-PART is non-nil, return the display string."
  (setq buffer (if (and buffer buffer-s) buffer anything-buffer))
  (unless (zerop (buffer-size (get-buffer buffer)))
    (with-current-buffer buffer
      (let ((selection
             (or (and (not force-display-part)
                      (get-text-property (overlay-start
                                          anything-selection-overlay)
                                         'anything-realvalue))
                 (buffer-substring-no-properties
                  (overlay-start anything-selection-overlay)
                  (1- (overlay-end anything-selection-overlay))))))
        (unless (equal selection "")
          selection)))))

(defun anything-get-action ()
  "Return the associated action for the selected candidate."
  (unless (zerop (buffer-size (get-buffer (anything-buffer-get))))
    (let* ((source (anything-get-current-source))
           (actions (assoc-default 'action source)))

      (anything-aif (assoc-default 'action-transformer source)
          ;; (funcall it actions (anything-get-selection))
          (anything-composed-funcall-with-source source it actions (anything-get-selection))
        actions))))

(defun anything-get-current-source ()
  "Return the source for the current selection / in init/candidates/action/candidate-transformer/filtered-candidate-transformer function."
  (declare (special source))
  ;; The name `anything-get-current-source' should be used in init function etc.
  (if (and (boundp 'anything-source-name) (stringp anything-source-name))
      source
    (with-current-buffer (anything-buffer-get)
      ;; This goto-char shouldn't be necessary, but point is moved to
      ;; point-min somewhere else which shouldn't happen.
      (goto-char (overlay-start anything-selection-overlay))
      (let* ((header-pos (anything-get-previous-header-pos))
             (source-name
              (save-excursion
                (assert header-pos)
                (goto-char header-pos)
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)))))
        (some (lambda (source)
                (if (equal (assoc-default 'name source)
                           source-name)
                    source))
              (anything-get-sources))))))

(defun anything-buffer-is-modified (buffer)
  "Return non-nil when BUFFER is modified since `anything' was invoked."
  (let* ((b (get-buffer buffer))
         (key (concat (buffer-name b)
                     "/"
                     (anything-attr 'name)))
         (source-tick (or (gethash key anything-tick-hash) 0))
         (buffer-tick (buffer-chars-modified-tick b)))
    (prog1 (/= source-tick buffer-tick)
      (puthash key buffer-tick anything-tick-hash))))
(defun anything-current-buffer-is-modified ()
  "Return non-nil when `anything-current-buffer' is modified since `anything' was invoked."
  (anything-buffer-is-modified anything-current-buffer))

(defvar anything-quit nil)
(defun anything-run-after-quit (function &rest args)
  "Perform an action after quitting `anything'.
The action is to call FUNCTION with arguments ARGS."
  (setq anything-quit t)
  (apply 'run-with-idle-timer 0 nil function args)
  (anything-exit-minibuffer))

(defun define-anything-type-attribute (type definition &optional doc)
  "Register type attribute of TYPE as DEFINITION with DOC.
DOC is displayed in `anything-type-attributes' docstring.

Use this function is better than setting `anything-type-attributes' directly."
  (anything-add-type-attribute type definition)
  (and doc (anything-document-type-attribute type doc))
  nil)

(defvar anything-additional-attributes nil)
(defun anything-document-attribute (attribute short-doc &optional long-doc)
  "Register ATTRIBUTE documentation introduced by plug-in.
SHORT-DOC is displayed beside attribute name.
LONG-DOC is displayed below attribute name and short documentation."
  (if long-doc
      (setq short-doc (concat "(" short-doc ")"))
    (setq long-doc short-doc
          short-doc ""))
  (add-to-list 'anything-additional-attributes attribute t)
  (put attribute 'anything-attrdoc
       (concat "- " (symbol-name attribute) " " short-doc "\n\n" long-doc "\n")))
(put 'anything-document-attribute 'lisp-indent-function 2)

;; (@* "Core: tools")
(defun anything-current-frame/window-configuration ()
  (funcall (cdr anything-save-configuration-functions)))

(defun anything-set-frame/window-configuration (conf)
  (funcall (car anything-save-configuration-functions) conf))

(defun anything-funcall-with-source (source func &rest args)
  (let ((anything-source-name (assoc-default 'name source)))
    (apply func args)))

(defun anything-funcall-foreach (sym)
  "Call the sym function(s) for each source if any."
  (dolist (source (anything-get-sources))
    (when (symbolp source)
      (setq source (symbol-value source)))
    (anything-aif (assoc-default sym source)
        (dolist (func (if (functionp it) (list it) it))
          (anything-funcall-with-source source func)))))

(defun anything-normalize-sources (sources)
  (cond ((and sources (symbolp sources)) (list sources))
        (sources)
        (t anything-sources)))  

(defun anything-approximate-candidate-number ()
  "Approximate Number of candidates.
It is used to check if candidate number is 0 or 1."
  (with-current-buffer anything-buffer
    (1- (line-number-at-pos (1- (point-max))))))

(defmacro with-anything-quittable (&rest body)
  `(let (inhibit-quit)
     (condition-case v
         (progn ,@body)
       (quit (setq anything-quit t)
             (exit-minibuffer)
             (keyboard-quit)))))
(put 'with-anything-quittable 'lisp-indent-function 0)

(defun anything-compose (arg-lst func-lst)
  "Call each function in FUNC-LST with the arguments specified in ARG-LST.
The result of each function will be the new `car' of ARG-LST.

This function allows easy sequencing of transformer functions."
  (dolist (func func-lst)
    (setcar arg-lst (apply func arg-lst)))
  (car arg-lst))

(defun anything-composed-funcall-with-source (source funcs &rest args)
  (if (functionp funcs)
      (apply 'anything-funcall-with-source source funcs args)
    (apply 'anything-funcall-with-source
           source (lambda (&rest args) (anything-compose args funcs)) args)))

;; (@* "Core: entry point")
(defun anything (&optional any-sources any-input any-prompt any-resume any-preselect any-buffer)
  "Select anything. In Lisp program, some optional arguments can be used.

Note that all the optional arguments are prefixed because of
dynamic scope problem, IOW argument variables may eat
already-bound variables. Yuck!

- ANY-SOURCES

  Temporary value of `anything-sources'. ANY-SOURCES accepts a
  symbol, interpreted as a variable of an anything source.

- ANY-INPUT

  Temporary value of `anything-pattern', ie. initial input of minibuffer.

- ANY-PROMPT

  Prompt other than \"pattern: \".

- ANY-RESUME

  Resurrect previously instance of `anything'. Skip the initialization.

- ANY-PRESELECT

  Initially selected candidate. Specified by exact candidate or a regexp.
  Note that it is not working with delayed sources.

- ANY-BUFFER

  `anything-buffer' instead of *anything*.
"
  ;; TODO more document
  (interactive)
  (condition-case v
      (with-anything-restore-variables
        (let ((frameconfig (anything-current-frame/window-configuration))
              ;; It is needed because `anything-source-name' is non-nil
              ;; when `anything' is invoked by action. Awful global scope.
              anything-source-name anything-in-persistent-action
              anything-quit anything-follow-mode
              (case-fold-search t)
              (anything-buffer (or any-buffer anything-buffer))
              (anything-sources (anything-normalize-sources any-sources)))
         
          (add-hook 'post-command-hook 'anything-check-minibuffer-input)
          (add-hook 'minibuffer-setup-hook 'anything-print-error-messages)
          (setq anything-current-position (cons (point) (window-start)))
          (if any-resume
              (anything-initialize-overlays (anything-buffer-get))
            (anything-initialize))
          (setq anything-last-buffer anything-buffer)
          (when any-input (setq anything-input any-input anything-pattern any-input))
          (anything-display-buffer anything-buffer)
          (unwind-protect
              (progn
                (if any-resume (anything-mark-current-line) (anything-update))
                
                (select-frame-set-input-focus (window-frame (minibuffer-window)))
                (anything-preselect any-preselect)
                (let ((ncandidate (anything-approximate-candidate-number))
                      (minibuffer-local-map anything-map))
                  (cond ((and anything-execute-action-at-once-if-one
                              (= ncandidate 1))
                         (ignore))
                        ((and anything-quit-if-no-candidate (= ncandidate 0))
                         (setq anything-quit t)
                         (and (functionp anything-quit-if-no-candidate)
                              (funcall anything-quit-if-no-candidate)))
                        (t
                         (read-string (or any-prompt "pattern: ")
                                      (if any-resume anything-pattern any-input))))))
            (anything-cleanup)
            (remove-hook 'minibuffer-setup-hook 'anything-print-error-messages)
            (remove-hook 'post-command-hook 'anything-check-minibuffer-input)
            (anything-set-frame/window-configuration frameconfig))
          (unless anything-quit
            (unwind-protect
                (anything-execute-selection-action)
              (anything-aif (get-buffer anything-action-buffer)
                  (kill-buffer it))
              (run-hooks 'anything-after-action-hook)))))
    (quit
     (setq minibuffer-history (cons anything-input minibuffer-history))
     (goto-char (car anything-current-position))
     (set-window-start (selected-window) (cdr anything-current-position))
     nil)))

(defun* anything-resume (&optional (any-buffer anything-last-buffer))
  "Resurrect previously invoked `anything'."
  (interactive)
  (when current-prefix-arg
    (setq any-buffer
          (completing-read
           "Resume anything buffer: "
           (delq nil
                 (mapcar (lambda (b)
                           (when (buffer-local-value 'anything-last-sources-local b)
                             (list (buffer-name b)))) (buffer-list)))
           nil t nil nil anything-buffer)))
  (setq anything-compiled-sources nil)
  (anything
   (or (buffer-local-value 'anything-last-sources-local (get-buffer any-buffer))
       anything-last-sources anything-sources)
   nil nil t nil any-buffer))

(defun anything-at-point (&optional any-sources any-input any-prompt any-resume any-preselect any-buffer)
  "Same as `anything' except when C-u is pressed, the initial input is the symbol at point."
  (interactive)
  (anything any-sources
            (if current-prefix-arg
                (concat "\\b" (thing-at-point 'symbol) "\\b"
                        (if (featurep 'anything-match-plugin) " " ""))
              any-input)
            any-prompt any-resume any-preselect any-buffer))

(defun anything-other-buffer (any-sources any-buffer)
  "Simplified interface of `anything' with other `anything-buffer'"
  (anything any-sources nil nil nil nil any-buffer))

;; (@* "Core: Display *anything* buffer")
(defun anything-display-buffer (buf)
  "Display *anything* buffer."
  (funcall anything-display-function buf))

(defun anything-default-display-buffer (buf)
  (funcall (if anything-samewindow 'switch-to-buffer 'pop-to-buffer) buf))

;; (@* "Core: initialize")
(defun anything-initialize ()
  "Initialize anything settings and set up the anything buffer."
  (run-hooks 'anything-before-initialize-hook)
  (setq anything-current-buffer (current-buffer))
  (setq anything-buffer-file-name buffer-file-name)
  (setq anything-issued-errors nil)
  (setq anything-compiled-sources nil)
  (setq anything-saved-current-source nil)
  ;; Call the init function for sources where appropriate
  (anything-funcall-foreach 'init)

  (setq anything-pattern "")
  (setq anything-input "")
  (setq anything-candidate-cache nil)
  (setq anything-last-sources anything-sources)

  (anything-create-anything-buffer)
  (run-hooks 'anything-after-initialize-hook))

(defun anything-create-anything-buffer (&optional test-mode)
  "Create newly created `anything-buffer'.
If TEST-MODE is non-nil, clear `anything-candidate-cache'."
  (when test-mode
    (setq anything-candidate-cache nil))
  (with-current-buffer (get-buffer-create anything-buffer)
    (buffer-disable-undo)
    (erase-buffer)
    (set (make-local-variable  'inhibit-read-only) t)
    (set (make-local-variable 'anything-last-sources-local) anything-sources)
    (setq cursor-type nil)
    (setq mode-name "Anything"))
  (anything-initialize-overlays anything-buffer)
  (get-buffer anything-buffer))

(defun anything-initialize-overlays (buffer)
  (if anything-selection-overlay
      ;; make sure the overlay belongs to the anything buffer if
      ;; it's newly created
      (move-overlay anything-selection-overlay (point-min) (point-min)
                    (get-buffer buffer))

    (setq anything-selection-overlay 
          (make-overlay (point-min) (point-min) (get-buffer buffer)))
    (overlay-put anything-selection-overlay 'face anything-selection-face))

  (if anything-enable-digit-shortcuts
      (unless anything-digit-overlays
        (dotimes (i 9)
          (push (make-overlay (point-min) (point-min)
                              (get-buffer buffer))
                anything-digit-overlays)
          (overlay-put (car anything-digit-overlays)
                       'before-string (concat (int-to-string (1+ i)) " - ")))
        (setq anything-digit-overlays (nreverse anything-digit-overlays)))

    (when anything-digit-overlays
      (dolist (overlay anything-digit-overlays)
        (delete-overlay overlay))
      (setq anything-digit-overlays nil))))

;; (@* "Core: clean up")
(defun anything-cleanup ()
  "Clean up the mess."
  (with-current-buffer anything-buffer
    (setq cursor-type t))
  (bury-buffer anything-buffer)
  (anything-funcall-foreach 'cleanup)
  (if anything-check-minibuffer-input-timer
      (cancel-timer anything-check-minibuffer-input-timer))
  (anything-kill-async-processes)
  (run-hooks 'anything-cleanup-hook))

;; (@* "Core: input handling")
(defun anything-check-minibuffer-input ()
  "Extract input string from the minibuffer and check if it needs
to be handled."
  (if (or (not anything-input-idle-delay) (anything-action-window))
      (anything-check-minibuffer-input-1)
    (if anything-check-minibuffer-input-timer
        (cancel-timer anything-check-minibuffer-input-timer))
    (setq anything-check-minibuffer-input-timer
          (run-with-idle-timer anything-input-idle-delay nil
                               'anything-check-minibuffer-input-1))))

(defun anything-check-minibuffer-input-1 ()
  (with-anything-quittable
    (with-selected-window (minibuffer-window)
      (anything-check-new-input (minibuffer-contents)))))

(defun anything-check-new-input (input)
  "Check input string and update the anything buffer if
necessary."
  (unless (equal input anything-pattern)
    (setq anything-pattern input)
    (unless (anything-action-window)
      (setq anything-input anything-pattern))
    (anything-update)))

;; (@* "Core: source compiler")
(defvar anything-compile-source-functions-default anything-compile-source-functions
  "Plug-ins this file provides.")
(defun anything-compile-sources (sources funcs)
  "Compile sources (`anything-sources') with funcs (`anything-compile-source-functions').
Anything plug-ins are realized by this function."
  (mapcar
   (lambda (source)
     (loop with source = (if (listp source) source (symbol-value source))
           for f in funcs
           do (setq source (funcall f source))
           finally (return source)))
   sources))  

;; (@* "Core: plug-in attribute documentation hack")

;; `anything-document-attribute' is public API.
(defadvice documentation-property (after anything-document-attribute activate)
  "Hack to display plug-in attributes' documentation as `anything-sources' docstring."
  (when (eq symbol 'anything-sources)
    (setq ad-return-value
          (concat ad-return-value "++++ Additional attributes by plug-ins ++++\n"
                  (mapconcat (lambda (sym) (get sym 'anything-attrdoc))
                             anything-additional-attributes
                             "\n")))))
;; (describe-variable 'anything-sources)
;; (documentation-property 'anything-sources 'variable-documentation)
;; (progn (ad-disable-advice 'documentation-property 'after 'anything-document-attribute) (ad-update 'documentation-property)) 

;; (@* "Core: all candidates")
(defun anything-get-candidates (source)
  "Retrieve and return the list of candidates from
SOURCE."
  (let* ((candidate-source (assoc-default 'candidates source))
         (candidates
          (if (functionp candidate-source)
                (anything-funcall-with-source source candidate-source)
            (if (listp candidate-source)
                candidate-source
              (if (and (symbolp candidate-source)
                       (boundp candidate-source))
                  (symbol-value candidate-source)
                (error (concat "Candidates must either be a function, "
                               " a variable or a list: %s")
                       candidate-source))))))
    (if (processp candidates)
        candidates
      (anything-transform-candidates candidates source))))
         

(defun anything-transform-candidates (candidates source)
  "Transform CANDIDATES according to candidate transformers."
  (anything-aif (assoc-default 'candidate-transformer source)
      (anything-composed-funcall-with-source source it candidates)
    candidates))


(defun anything-get-cached-candidates (source)
  "Return the cached value of candidates for SOURCE.
Cache the candidates if there is not yet a cached value."
  (let* ((name (assoc-default 'name source))
         (candidate-cache (assoc name anything-candidate-cache))
         candidates)

    (if candidate-cache
        (setq candidates (cdr candidate-cache))

      (setq candidates (anything-get-candidates source))

      (if (processp candidates)
          (progn
            (push (cons candidates
                        (append source 
                                (list (cons 'item-count 0)
                                      (cons 'incomplete-line ""))))
                  anything-async-processes)
            (set-process-filter candidates 'anything-output-filter)
            (setq candidates nil))

        (unless (assoc 'volatile source)
          (setq candidate-cache (cons name candidates))
          (push candidate-cache anything-candidate-cache))))

    candidates))

;; (@* "Core: narrowing candidates")
(defun anything-candidate-number-limit (source)
  "`anything-candidate-number-limit' variable may be overridden by SOURCE."
  (or (assoc-default 'candidate-number-limit source)
      anything-candidate-number-limit
      99999999))

(defun anything-compute-matches (source)
  "Compute matches from SOURCE according to its settings."
  (let ((doit (lambda ()
                (let ((functions (assoc-default 'match source))
                      (limit (anything-candidate-number-limit source))
                      matches)
                  (cond ((or (equal anything-pattern "") (equal functions '(identity)))
                         (setq matches (anything-get-cached-candidates source))
                         (if (> (length matches) limit)
                             (setq matches 
                                   (subseq matches 0 limit))))
                        (t
                         (condition-case nil
                             (let ((item-count 0)
                                   (cands (anything-get-cached-candidates source))
                                   exit)

                               (unless functions
                                 (setq functions
                                       (list (lambda (candidate)
                                               (string-match anything-pattern candidate)))))

                               (clrhash anything-match-hash)
                               (dolist (function functions)
                                 (let (newmatches c cc)
                                   (dolist (candidate cands)
                                     (when (and (not (gethash candidate anything-match-hash))
                                                (setq c (if (listp candidate)
                                                                     (car candidate)
                                                                   candidate))
                                                (setq cc (cond ((stringp c) c)
                                                               ((symbolp c) (symbol-name c))))
                                                (funcall function cc))
                                       (puthash candidate t anything-match-hash)
                                       (push candidate newmatches)

                                       (when limit
                                         (incf item-count)
                                         (when (= item-count limit)
                                           (setq exit t)
                                           (return)))))

                                   (setq matches (append matches (reverse newmatches)))

                                   (if exit
                                       (return)))))

                           (invalid-regexp (setq matches nil)))))

                  (anything-aif (assoc-default 'filtered-candidate-transformer source)
                      (setq matches
                            (anything-composed-funcall-with-source source it matches source)))
                  matches))))
    (if debug-on-error
        (funcall doit)
      (condition-case v
          (funcall doit)
        (error (anything-log-error
                "anything-compute-matches: error when processing source: %s"
                (assoc-default 'name source))
               nil)))))

;; (anything '(((name . "error")(candidates . (lambda () (hage))) (action . identity))))

(defun anything-process-source (source)
  "Display matches from SOURCE according to its settings."
  (let ((matches (anything-compute-matches source)))
    (when matches
      (when anything-test-mode
          (setq anything-test-candidate-list
                `(,@anything-test-candidate-list
                  (,(assoc-default 'name source)
                   ,matches))))
      (let ((multiline (assoc 'multiline source))
            (real-to-display (assoc-default 'real-to-display source))
            (start (point))
            separate)
        (anything-insert-header-from-source source)
        (dolist (match matches)
          (when (and anything-enable-digit-shortcuts
                     (not (eq anything-digit-shortcut-count 9)))
            (move-overlay (nth anything-digit-shortcut-count
                               anything-digit-overlays)
                          (line-beginning-position)
                          (line-beginning-position))
            (incf anything-digit-shortcut-count))

          (if (and multiline separate)
              (anything-insert-candidate-separator)
            (setq separate t))
          (anything-insert-match match 'insert real-to-display))
        
        (if multiline
            (put-text-property start (point) 'anything-multiline t))))))

(defun anything-process-delayed-sources (delayed-sources)
  "Process delayed sources if the user is idle for
`anything-idle-delay' seconds."
  (with-anything-quittable
    (if (sit-for (if anything-input-idle-delay
                     (max 0 (- anything-idle-delay anything-input-idle-delay))
                   anything-idle-delay))
        (with-current-buffer anything-buffer        
          (save-excursion
            (goto-char (point-max))
            (dolist (source delayed-sources)
              (anything-process-source source))

            (when (and (not (equal (buffer-size) 0))
                       ;; no selection yet
                       (= (overlay-start anything-selection-overlay)
                          (overlay-end anything-selection-overlay)))
              (goto-char (point-min))
              (anything-next-line)))
          (save-excursion
            (goto-char (point-min))
            (run-hooks 'anything-update-hook))
          (anything-maybe-fit-frame)))))

;; (@* "Core: *anything* buffer contents")
(defun anything-update ()
  "Update the list of matches in the anything buffer according to
the current pattern."
  (setq anything-digit-shortcut-count 0)
  (anything-kill-async-processes)
  (with-current-buffer (anything-buffer-get)
    (erase-buffer)

    (if anything-enable-digit-shortcuts
        (dolist (overlay anything-digit-overlays)
          (delete-overlay overlay)))

    (let (delayed-sources)
      (dolist (source (anything-get-sources))
        (when (and (or (not anything-source-filter)
                       (member (assoc-default 'name source) anything-source-filter))
                   (>= (length anything-pattern)
                       (anything-aif (assoc 'requires-pattern source)
                           (or (cdr it) 1)
                         0)))
          (if (or (assoc 'delayed source)
                  (and anything-quick-update
                       (< (window-height (get-buffer-window (current-buffer)))
                          (line-number-at-pos (point-max)))))
              (push source delayed-sources)
            (anything-process-source source))))

      (goto-char (point-min))
      (save-excursion (run-hooks 'anything-update-hook))
      (anything-next-line)
      (setq delayed-sources (nreverse delayed-sources))
      (if anything-test-mode
          (dolist (source delayed-sources)
            (anything-process-source source))
        (anything-maybe-fit-frame)
        (run-with-idle-timer (if (featurep 'xemacs)
                                 0.1
                               0)
                             nil
                             'anything-process-delayed-sources
                             delayed-sources)))))

(defun anything-insert-match (match insert-function &optional real-to-display)
  "Insert MATCH into the anything buffer. If MATCH is a list then
insert the string inteneded to appear on the display and store
the real value in a text property."
  (let ((start (line-beginning-position (point)))
        (string (if (listp match) (car match) match))
        (realvalue (if (listp match) (cdr match) match)))
    (and (functionp real-to-display)
         (setq string (funcall real-to-display realvalue)))
    (when (symbolp string) (setq string (symbol-name string)))
    (when (stringp string)                    ; real-to-display may return nil
      (funcall insert-function string)
      ;; Some sources with candidates-in-buffer have already added
      ;; 'anything-realvalue property when creating candidate buffer.
      (unless (get-text-property start 'anything-realvalue)
        (put-text-property start (line-end-position)
                           'anything-realvalue realvalue))
      (funcall insert-function "\n"))))

(defun anything-insert-header-from-source (source)
  (let ((name (assoc-default 'name source)))
    (anything-insert-header name
                            (anything-aif (assoc-default 'header-name source)
                                (anything-funcall-with-source source it name)))))

(defun anything-insert-header (name &optional display-string)
  "Insert header of source NAME into the anything buffer."
  (unless (bobp)
    (let ((start (point)))
      (insert "\n")
      (put-text-property start (point) 'anything-header-separator t)))

  (let ((start (point)))
    (insert name)
    (put-text-property (line-beginning-position)
                       (line-end-position) 'anything-header t)
    (when display-string
      (overlay-put (make-overlay (line-beginning-position) (line-end-position))
                   'display display-string))
    (insert "\n")
    (put-text-property start (point) 'face anything-header-face)))


(defun anything-insert-candidate-separator ()
  "Insert separator of candidates into the anything buffer."
  (insert anything-candidate-separator)
  (put-text-property (line-beginning-position)
                     (line-end-position) 'anything-candidate-separator t)
  (insert "\n"))




;; (@* "Core: async process")
(defun anything-output-filter (process string)
  "Process output from PROCESS."
  (let* ((process-assoc (assoc process anything-async-processes))
         (process-info (cdr process-assoc))
         (insertion-marker (assoc-default 'insertion-marker process-info))
         (incomplete-line-info (assoc 'incomplete-line process-info))
         (item-count-info (assoc 'item-count process-info))
         (real-to-display (assoc-default 'real-to-display process-info)))

    (with-current-buffer anything-buffer
      (save-excursion
        (if insertion-marker
            (goto-char insertion-marker)
        
          (goto-char (point-max))
          (anything-insert-header-from-source process-info)
          (setcdr process-assoc
                  (append process-info `((insertion-marker . ,(point-marker))))))

        (let ((lines (split-string string "\n"))
              candidates)
          (while lines
            (if (not (cdr lines))
                ;; store last incomplete line until new output arrives
                (setcdr incomplete-line-info (car lines))

              (if (cdr incomplete-line-info)
                  (progn
                    (push (concat (cdr incomplete-line-info) (car lines))
                          candidates)
                    (setcdr incomplete-line-info nil))

              (push (car lines) candidates)))
                  
            (pop lines))

          (setq candidates (reverse candidates))
          (dolist (candidate (anything-transform-candidates candidates process-info))
            (anything-insert-match candidate 'insert-before-markers real-to-display)
            (incf (cdr item-count-info))
            (when (>= (cdr item-count-info) anything-candidate-number-limit)
              (anything-kill-async-process process)
              (return)))))

      (anything-maybe-fit-frame)

      (run-hooks 'anything-update-hook)

      (if (bobp)
          (anything-next-line)

        (save-selected-window
          (select-window (get-buffer-window anything-buffer 'visible))
          (anything-mark-current-line))))))


(defun anything-kill-async-processes ()
  "Kill all known asynchronous processes according to
`anything-async-processes'."
    "Kill locate process."
    (dolist (process-info anything-async-processes)
      (anything-kill-async-process (car process-info)))
    (setq anything-async-processes nil))


(defun anything-kill-async-process (process)
  "Kill PROCESS and detach the associated functions."
  (set-process-filter process nil)
  (delete-process process))
  

;; (@* "Core: action")
(defun anything-execute-selection-action (&optional selection action clear-saved-action display-to-real)
  "If a candidate was selected then perform the associated
action."
  (setq selection (or selection (anything-get-selection)))
  (setq action (or action
                   anything-saved-action
                   (if (get-buffer anything-action-buffer)
                       (anything-get-selection anything-action-buffer)
                     (anything-get-action))))
  (let ((source (or anything-saved-current-source (anything-get-current-source))))
    (if (and (not selection) (assoc 'accept-empty source))
        (setq selection ""))
    (setq display-to-real
          (or display-to-real (assoc-default 'display-to-real source)
              #'identity))
    (if (and (listp action)
             (not (functionp action)))  ; lambda
        ;;select the default action
        (setq action (cdar action)))
    (unless clear-saved-action (setq anything-saved-action nil))
    (if (and selection action)
        (anything-funcall-with-source
         source  action
         (anything-funcall-with-source source display-to-real selection)))))

(defun anything-select-action ()
  "Select an action for the currently selected candidate.
If action buffer is selected, back to the anything buffer."
  (interactive)
  (cond ((get-buffer-window anything-action-buffer 'visible)
         (set-window-buffer (get-buffer-window anything-action-buffer) anything-buffer)
         (kill-buffer anything-action-buffer))
        (t
         (setq anything-saved-selection (anything-get-selection))
         (unless anything-saved-selection
           (error "Nothing is selected."))
         (setq anything-saved-current-source (anything-get-current-source))
         (let ((actions (anything-get-action)))
           (if (functionp actions)
               (message "Sole action: %s" actions)
             (with-current-buffer (get-buffer-create anything-action-buffer)
               (erase-buffer)
               (buffer-disable-undo)
               (set-window-buffer (get-buffer-window anything-buffer) anything-action-buffer)
               (set (make-local-variable 'anything-sources)
                    `(((name . "Actions")
                       (volatile)
                       (candidates . ,actions)
                       ;; Override `anything-candidate-number-limit'
                       (candidate-number-limit . 9999))))
               (set (make-local-variable 'anything-source-filter) nil)
               (set (make-local-variable 'anything-selection-overlay) nil)
               (set (make-local-variable 'anything-digit-overlays) nil)
               (anything-initialize-overlays anything-action-buffer))
             (with-selected-window (minibuffer-window)
               (delete-minibuffer-contents))
             (setq anything-pattern 'dummy) ; so that it differs from the
                                        ; previous one
           
             (anything-check-minibuffer-input))))))

;; (@* "Core: selection")
(defun anything-previous-line ()
  "Move selection to the previous line."
  (interactive)
  (anything-move-selection 'line 'previous))

(defun anything-next-line ()
  "Move selection to the next line."
  (interactive)
  (anything-move-selection 'line 'next))

(defun anything-previous-page ()
  "Move selection back with a pageful."
  (interactive)
  (anything-move-selection 'page 'previous))

(defun anything-next-page ()
  "Move selection forward with a pageful."
  (interactive)
  (anything-move-selection 'page 'next))


(defun anything-previous-source ()
  "Move selection to the previous source."
  (interactive)
  (anything-move-selection 'source 'previous))


(defun anything-next-source ()
  "Move selection to the next source."
  (interactive)
  (anything-move-selection 'source 'next))


(defun anything-move-selection (unit direction)
  "Move the selection marker to a new position determined by
UNIT and DIRECTION."
  (unless (or (zerop (buffer-size (get-buffer (anything-buffer-get))))
              (not (anything-window)))
    (with-anything-window
      (case unit
        (line (case direction
                (next (if (not (anything-pos-multiline-p))
                          (forward-line 1)
                        (let ((header-pos (anything-get-next-header-pos))
                              (candidate-pos (anything-get-next-candidate-separator-pos)))
                          (if (and candidate-pos
                                   (or (null header-pos)
                                       (< candidate-pos header-pos)))
                              (goto-char candidate-pos)
                            (if header-pos
                                (goto-char header-pos)))
                          (if candidate-pos
                              (forward-line 1)))))
                
                (previous (progn
                            (forward-line -1)
                            (when (anything-pos-multiline-p)
                              (if (or (anything-pos-header-line-p)
                                      (anything-pos-candidate-separator-p))
                                  (forward-line -1)
                                (forward-line 1))
                              (let ((header-pos (anything-get-previous-header-pos))
                                    (candidate-pos (anything-get-previous-candidate-separator-pos)))
                                (when header-pos
                                  (if (or (null candidate-pos) (< candidate-pos header-pos))
                                      (goto-char header-pos)
                                    (goto-char candidate-pos))
                                  (forward-line 1))))))
                
                (t (error "Invalid direction."))))

        (page (case direction
                (next (condition-case nil
                          (scroll-up)
                        (end-of-buffer (goto-char (point-max)))))
                (previous (condition-case nil
                              (scroll-down)
                            (beginning-of-buffer (goto-char (point-min)))))
                (t (error "Invalid direction."))))

        (source (case direction
                   (next (goto-char (or (anything-get-next-header-pos)
                                        (point-min))))
                   (previous (progn
                               (forward-line -1)
                               (if (bobp)
                                   (goto-char (point-max))
                                 (if (anything-pos-header-line-p)
                                     (forward-line -1)
                                   (forward-line 1)))
                               (goto-char (anything-get-previous-header-pos))
                               (forward-line 1)))
                   (t (error "Invalid direction."))))

        (t (error "Invalid unit.")))

      (while (and (not (bobp))
                  (or (anything-pos-header-line-p)
                      (anything-pos-candidate-separator-p)))
        (forward-line (if (and (eq direction 'previous)
                               (not (eq (line-beginning-position)
                                        (point-min))))
                          -1
                        1)))
      (if (bobp)
          (forward-line 1))
      (if (eobp)
          (forward-line -1))

      (when (and anything-display-source-at-screen-top (eq unit 'source))
        (set-window-start (selected-window)
                          (save-excursion (forward-line -1) (point))))
      (when (anything-get-previous-header-pos)
        (anything-mark-current-line)))))

(defun anything-mark-current-line ()
  "Move selection overlay to current line."
  (move-overlay anything-selection-overlay
                (line-beginning-position)
                (if (anything-pos-multiline-p)
                    (let ((header-pos (anything-get-next-header-pos))
                          (candidate-pos (anything-get-next-candidate-separator-pos)))
                      (or (and (null header-pos) candidate-pos candidate-pos)
                          (and header-pos candidate-pos (< candidate-pos header-pos) candidate-pos)
                          header-pos
                          (point-max)))
                  (1+ (line-end-position))))
  (anything-follow-execute-persistent-action-maybe))

(defun anything-select-with-digit-shortcut ()
  (interactive)
  (if anything-enable-digit-shortcuts
      (save-selected-window
        (select-window (anything-window))          
        (let* ((index (- (event-basic-type (elt (this-command-keys-vector) 0)) ?1))
               (overlay (nth index anything-digit-overlays)))
          (when (overlay-buffer overlay)
            (goto-char (overlay-start overlay))
            (anything-mark-current-line)
            (anything-exit-minibuffer))))))

(defun anything-exit-minibuffer ()
  "Select the current candidate by exiting the minibuffer."
  (interactive)
  (declare (special anything-iswitchb-candidate-selected))
  (setq anything-iswitchb-candidate-selected (anything-get-selection))
  (exit-minibuffer))


(defun anything-get-next-header-pos ()
  "Return the position of the next header from point."
  (next-single-property-change (point) 'anything-header))


(defun anything-get-previous-header-pos ()
  "Return the position of the previous header from point"
  (previous-single-property-change (point) 'anything-header))


(defun anything-pos-multiline-p ()
  "Return non-nil if the current position is in the multiline source region."
  (get-text-property (point) 'anything-multiline))


(defun anything-get-next-candidate-separator-pos ()
  "Return the position of the next candidate separator from point."
  (next-single-property-change (point) 'anything-candidate-separator))


(defun anything-get-previous-candidate-separator-pos ()
  "Return the position of the previous candidate separator from point."
  (previous-single-property-change (point) 'anything-candidate-separator))


(defun anything-pos-header-line-p ()
  "Return t if the current line is a header line."
  (or (get-text-property (line-beginning-position) 'anything-header)
      (get-text-property (line-beginning-position) 'anything-header-separator)))

(defun anything-pos-candidate-separator-p ()
  "Return t if the current line is a candidate separator."
  (get-text-property (line-beginning-position) 'anything-candidate-separator))

;; (@* "Core: error handling")
(defun anything-log-error (&rest args)
  "Accumulate error messages into `anything-issued-errors'."
  (let ((msg (apply 'format args)))
    (unless (member msg anything-issued-errors)
      (add-to-list 'anything-issued-errors msg))))

(defun anything-print-error-messages ()
  "Print error messages in `anything-issued-errors'."
  (message "%s" (mapconcat 'identity (reverse anything-issued-errors) "\n")))


;; (@* "Core: misc")
(defun anything-kill-buffer-hook ()
  "Remove tick entry from `anything-tick-hash' when killing a buffer."
  (loop for key being the hash-keys in anything-tick-hash
        if (string-match (format "^%s/" (regexp-quote (buffer-name))) key)
        do (remhash key anything-tick-hash)))
(add-hook 'kill-buffer-hook 'anything-kill-buffer-hook)

(defun anything-maybe-fit-frame ()
  "Fit anything frame to its buffer, and put it at top right of display.
 To inhibit fitting, set `fit-frame-inhibit-fitting-flag' to t.
 You can set user options `fit-frame-max-width-percent' and
 `fit-frame-max-height-percent' to control max frame size."
  (declare (warn (unresolved 0)))
  (when (and (require 'fit-frame nil t)
             (boundp 'fit-frame-inhibit-fitting-flag)
             (not fit-frame-inhibit-fitting-flag)
             (anything-window))
    (ignore-errors
      (with-anything-window
        (fit-frame nil nil nil t)
        (modify-frame-parameters
         (selected-frame)
         `((left . ,(- (x-display-pixel-width) (+ (frame-pixel-width) 7)))
           (top . 0))))))) ; The (top . 0) shouldn't be necessary (Emacs bug).

(defun anything-preselect (candidate-or-regexp)
  (when candidate-or-regexp
    (with-anything-window
      (goto-char (point-min))
      ;; go to first candidate of first source
      (forward-line 1)
      (let ((start (point)))
        (unless (or (re-search-forward (concat "^" (regexp-quote candidate-or-regexp) "$") nil t)
                (progn (goto-char start)
                       (re-search-forward candidate-or-regexp nil t)))
          (goto-char start))
        (anything-mark-current-line)))))

(defun anything-delete-current-selection ()
  "Delete the currently selected item."
  (interactive)
  (with-anything-window
    (cond ((anything-pos-multiline-p)
           (anything-aif (anything-get-next-candidate-separator-pos)
               (delete-region (point-at-bol)
                              (1+ (progn (goto-char it) (point-at-eol))))
             ;; last candidate
             (goto-char (anything-get-previous-candidate-separator-pos))
             (delete-region (point-at-bol) (point-max)))
           (when (eobp)
             (goto-char (or (anything-get-previous-candidate-separator-pos)
                            (point-min)))
             (forward-line 1)))
          (t
           (delete-region (point-at-bol) (1+ (point-at-eol)))
           (when (eobp) (forward-line -1))))
    (anything-mark-current-line)))

(defun anything-delete-minibuffer-content ()
  "Same as `delete-minibuffer-contents' but this is a command."
  (interactive)
  (delete-minibuffer-contents))

;; (@* "Built-in plug-in: type")
(defun anything-compile-source--type (source)
  (anything-aif (assoc-default 'type source)
      (append source (assoc-default it anything-type-attributes) nil)
    source))

;; `define-anything-type-attribute' is public API.

(defun anything-add-type-attribute (type definition)
  (anything-aif (assq type anything-type-attributes)
      (setq anything-type-attributes (delete it anything-type-attributes)))
  (push (cons type definition) anything-type-attributes))

(defvar anything-types nil)
(defun anything-document-type-attribute (type doc)
  (add-to-list 'anything-types type t)
  (put type 'anything-typeattrdoc
       (concat "- " (symbol-name type) "\n\n" doc "\n")))

(defadvice documentation-property (after anything-document-type-attribute activate)
  "Hack to display type attributes' documentation as `anything-type-attributes' docstring."
  (when (eq symbol 'anything-type-attributes)
    (setq ad-return-value
          (concat ad-return-value "\n\n++++ Types currently defined ++++\n"
                  (mapconcat (lambda (sym) (get sym 'anything-typeattrdoc))
                             anything-types "\n")))))

;; (@* "Built-in plug-in: dummy")
(defun anything-dummy-candidate (candidate source)
  ;; `source' is defined in filtered-candidate-transformer
  (list anything-pattern))  

(defun anything-compile-source--dummy (source)
  (if (assoc 'dummy source)
      (append '((candidates "dummy")
                (accept-empty)
                (match identity)
                (filtered-candidate-transformer . anything-dummy-candidate)
                (volatile))
              source)
    source))

;; (@* "Built-in plug-in: candidates-in-buffer")
(defun anything-candidates-in-buffer ()
  "Get candidates from the candidates buffer according to `anything-pattern'.

BUFFER is `anything-candidate-buffer' by default.  Each
candidate must be placed in one line.  This function is meant to
be used in candidates-in-buffer or candidates attribute of an
anything source.  Especially fast for many (1000+) candidates.

eg.
 '((name . \"many files\")
   (init . (lambda () (with-current-buffer (anything-candidate-buffer 'local)
                        (insert-many-filenames))))
   (search re-search-forward)  ; optional
   (candidates-in-buffer)
   (type . file))

+===============================================================+
| The new way of making and narrowing candidates: Using buffers |
+===============================================================+

By default, `anything' makes candidates by evaluating the
candidates function, then narrows them by `string-match' for each
candidate.

But this way is very slow for many candidates. The new way is
storing all candidates in a buffer and narrowing them by
`re-search-forward'. Search function is customizable by search
attribute. The important point is that buffer processing is MUCH
FASTER than string list processing and is the Emacs way.

The init function writes all candidates to a newly-created
candidate buffer.  The candidates buffer is created or specified
by `anything-candidate-buffer'.  Candidates are stored in a line.

The candidates function narrows all candidates, IOW creates a
subset of candidates dynamically. It is the task of
`anything-candidates-in-buffer'.  As long as
`anything-candidate-buffer' is used,`(candidates-in-buffer)' is
sufficient in most cases.

Note that `(candidates-in-buffer)' is shortcut of three attributes:
  (candidates . anything-candidates-in-buffer)
  (volatile)
  (match identity)
And `(candidates-in-buffer . func)' is shortcut of three attributes:
  (candidates . func)
  (volatile)
  (match identity)
The expansion is performed in `anything-get-sources'.

The candidates-in-buffer attribute implies the volatile attribute.
The volatile attribute is needed because `anything-candidates-in-buffer'
creates candidates dynamically and need to be called everytime
`anything-pattern' changes.

Because `anything-candidates-in-buffer' plays the role of `match' attribute
function, specifying `(match identity)' makes the source slightly faster.

To customize `anything-candidates-in-buffer' behavior, use search,
get-line and search-from-end attributes. See also `anything-sources' docstring.
"
  (declare (special source))
  (anything-candidates-in-buffer-1 (anything-candidate-buffer)
                                   anything-pattern
                                   (or (assoc-default 'get-line source)
                                       #'buffer-substring-no-properties)
                                   ;; use external variable `source'.
                                   (or (assoc-default 'search source)
                                       (if (assoc 'search-from-end source)
                                           '(re-search-backward)
                                         '(re-search-forward)))
                                   (anything-candidate-number-limit source)
                                   (assoc 'search-from-end source)))

(defun* anything-candidates-in-buffer-1 (buffer &optional (pattern anything-pattern) (get-line-fn 'buffer-substring-no-properties) (search-fns '(re-search-forward)) (limit anything-candidate-number-limit) search-from-end)
  ;; buffer == nil when candidates buffer does not exist.
  (when buffer
    (with-current-buffer buffer
      (let ((start-point (if search-from-end (point-max) (point-min)))
            (next-line-fn (if search-from-end
                              (lambda (x) (goto-char (max (1- (point-at-bol)) 1)))
                            #'forward-line))
            (endp (if search-from-end #'bobp #'eobp)))
        (goto-char (1- start-point))
        (if (string= pattern "")
            (delq nil (loop until (funcall endp)
                                    for i from 1 to limit
                                    collecting (funcall get-line-fn (point-at-bol) (point-at-eol))
                                    do (funcall next-line-fn 1)))
                    
          (let ((i 1)
                (next-line-fn (if search-from-end
                                  (lambda (x) (goto-char (max (point-at-bol) 1)))
                                #'forward-line))
                buffer-read-only
                matches exit newmatches)
            (progn
              (goto-char (point-min))
              (insert "\n")
              (goto-char (point-max))
              (insert "\n")
              (setq start-point (if search-from-end (point-max) (point-min)))
              (clrhash anything-cib-hash)
              (unwind-protect
                  (dolist (searcher search-fns)
                    (goto-char start-point)
                    (setq newmatches nil)
                    (loop while (funcall searcher pattern nil t)
                          if (or (funcall endp) (< limit i))
                          do (setq exit t) (return)
                          else do
                          (let ((cand (funcall get-line-fn (point-at-bol) (point-at-eol))))
                            (unless (gethash cand anything-cib-hash)
                              (puthash cand t anything-cib-hash)
                              (incf i)
                              (push cand newmatches)))
                          (funcall next-line-fn 1))
                    (setq matches (append matches (nreverse newmatches)))
                    (if exit (return)))
                (goto-char (point-min))
                (delete-char 1)
                (goto-char (1- (point-max)))
                (delete-char 1)
                           
                (set-buffer-modified-p nil)))
            (delq nil matches)))))))


(defun anything-candidate-buffer (&optional create-or-buffer)
  "Register and return a buffer containing candidates of current source.
`anything-candidate-buffer' searches buffer-local candidates buffer first,
then global candidates buffer.

Acceptable values of CREATE-OR-BUFFER:

- nil (omit)
  Only return the candidates buffer.
- a buffer
  Register a buffer as a candidates buffer.
- 'global
  Create a new global candidates buffer,
  named \" *anything candidates:SOURCE*\".
- other non-nil value
  Create a new global candidates buffer,
  named \" *anything candidates:SOURCE*ANYTHING-CURRENT-BUFFER\".
"
  (let* ((gbufname (format " *anything candidates:%s*" anything-source-name))
         (lbufname (concat gbufname (buffer-name anything-current-buffer)))
         buf)
    (when create-or-buffer
      (if (bufferp create-or-buffer)
          (setq anything-candidate-buffer-alist
                (cons (cons anything-source-name create-or-buffer)
                      (delete (assoc anything-source-name anything-candidate-buffer-alist)
                              anything-candidate-buffer-alist)))
        (add-to-list 'anything-candidate-buffer-alist
                     (cons anything-source-name create-or-buffer))
        (when (eq create-or-buffer 'global)
          (loop for b in (buffer-list)
                if (string-match (format "^%s" (regexp-quote gbufname)) (buffer-name b))
                do (kill-buffer b)))
        (with-current-buffer
            (get-buffer-create (if (eq create-or-buffer 'global) gbufname lbufname))
          (buffer-disable-undo)
          (erase-buffer)
          (font-lock-mode -1))))
    (or (get-buffer lbufname)
        (get-buffer gbufname)
        (anything-aif (assoc-default anything-source-name anything-candidate-buffer-alist)
            (and (buffer-live-p it) it)))))

(defun anything-compile-source--candidates-in-buffer (source)
  (anything-aif (assoc 'candidates-in-buffer source)
      (append source `((candidates . ,(or (cdr it) 'anything-candidates-in-buffer))
                       (volatile) (match identity)))
    source))

;; (@* "Utility: select another action by key")
(defun anything-select-nth-action (n)
  "Select the nth action for the currently selected candidate."
  (setq anything-saved-selection (anything-get-selection))
  (unless anything-saved-selection
    (error "Nothing is selected."))
  (setq anything-saved-action (cdr (elt (anything-get-action) n)))
  (anything-exit-minibuffer))

(defun anything-select-2nd-action ()
  "Select the 2nd action for the currently selected candidate."
  (interactive)
  (anything-select-nth-action 1))

(defun anything-select-3rd-action ()
  "Select the 3rd action for the currently selected candidate."
  (interactive)
  (anything-select-nth-action 2))

(defun anything-select-4th-action ()
  "Select the 4th action for the currently selected candidate."
  (interactive)
  (anything-select-nth-action 3))

(defun anything-select-2nd-action-or-end-of-line ()
  "Select the 2nd action for the currently selected candidate if the point is at the end of minibuffer.
Otherwise goto the end of minibuffer."
  (interactive)
  (if (eolp)
      (anything-select-nth-action 1)
    (end-of-line)))

;; (@* "Utility: Persistent Action")
(defmacro with-anything-display-same-window (&rest body)
  "Make `pop-to-buffer' and `display-buffer' display in the same window."
  `(let ((display-buffer-function 'anything-persistent-action-display-buffer))
     ,@body))
(put 'with-anything-display-same-window 'lisp-indent-function 0)

(defun* anything-execute-persistent-action (&optional (attr 'persistent-action))
  "If a candidate is selected then perform the associated action without quitting anything."
  (interactive)
  (save-selected-window
    (select-window (get-buffer-window (anything-buffer-get)))
    (select-window (setq minibuffer-scroll-window
                         (if (one-window-p t) (split-window)
                           (next-window (selected-window) 1))))
    (let ((anything-in-persistent-action t))
      (with-anything-display-same-window
        (anything-execute-selection-action
         nil
         (or (assoc-default attr (anything-get-current-source))
             (anything-get-action))
         t)
        (run-hooks 'anything-after-persistent-action-hook)))))

(defun anything-persistent-action-display-buffer (buf &optional not-this-window)
  "Make `pop-to-buffer' and `display-buffer' display in the same window in persistent action.
If `anything-persistent-action-use-special-display' is non-nil and
BUF is to be displayed by `special-display-function', use it.
Otherwise ignores `special-display-buffer-names' and `special-display-regexps'."
  (let* ((name (buffer-name buf))
         display-buffer-function pop-up-windows
         (same-window-regexps
          (unless (and anything-persistent-action-use-special-display
                       (or (member name
                                   (mapcar (lambda (x) (or (car-safe x) x)) special-display-buffer-names))
                           (remove-if-not
                            (lambda (x) (string-match (or (car-safe x) x) name))
                            special-display-regexps)))
            '("."))))
    (display-buffer buf not-this-window)))

;; scroll-other-window(-down)? for persistent-action
(defun anything-scroll-other-window-base (command)
  (save-selected-window
    (select-window
     (some-window
      (lambda (w) (not (string= anything-buffer (buffer-name (window-buffer w)))))
      'no-minibuffer 'current-frame))
    (call-interactively command)))

(defun anything-scroll-other-window ()
  "Scroll other window (not *Anything* window) upward."
  (interactive)
  (anything-scroll-other-window-base (lambda ()
                                       (interactive)
                                       (scroll-up anything-scroll-amount))))
(defun anything-scroll-other-window-down ()
  "Scroll other window (not *Anything* window) downward."
  (interactive)
  (anything-scroll-other-window-base (lambda ()
                                       (interactive)
                                       (scroll-down anything-scroll-amount))))

;; (@* "Utility: Visible Mark")
(defface anything-visible-mark
  '((((min-colors 88) (background dark))
     (:background "green1" :foreground "black"))
    (((background dark)) (:background "green" :foreground "black"))
    (((min-colors 88)) (:background "green1"))
    (t (:background "green")))
  "Face for visible mark."
  :group 'anything)
(defvar anything-visible-mark-face 'anything-visible-mark)
(defvar anything-visible-mark-overlays nil)

(defun anything-clear-visible-mark ()
  (mapc 'delete-overlay anything-visible-mark-overlays)
  (setq anything-visible-mark-overlays nil))
(add-hook 'anything-after-initialize-hook 'anything-clear-visible-mark)

;; (defun anything-toggle-visible-mark ()
;;   (interactive)
;;   (with-anything-window
;;     (anything-aif (loop for o in anything-visible-mark-overlays
;;                         when (equal (line-beginning-position) (overlay-start o))
;;                         do (return o))
;;         ;; delete
;;         (progn (delete-overlay it)
;;                (delq it anything-visible-mark-overlays))
;;       (let ((o (make-overlay (line-beginning-position) (1+ (line-end-position)))))
;;         (overlay-put o 'face anything-visible-mark-face)
;;         (overlay-put o 'source (assoc-default 'name (anything-get-current-source)))
;;         (overlay-put o 'string (buffer-substring (overlay-start o) (overlay-end o)))
;;         (add-to-list 'anything-visible-mark-overlays o)))))

(defvar anything-c-marked-candidate-list nil)
(defun anything-toggle-visible-mark ()
  (interactive)
  (with-anything-window
    (anything-aif (loop for o in anything-visible-mark-overlays
                        when (equal (line-beginning-position) (overlay-start o))
                        do   (return o))
        ;; delete
        (progn
          (setq anything-c-marked-candidate-list
                (remove
                 (buffer-substring-no-properties (point-at-bol) (point-at-eol)) anything-c-marked-candidate-list))
          (delete-overlay it)
          (delq it anything-visible-mark-overlays))
      (let ((o (make-overlay (line-beginning-position) (1+ (line-end-position)))))
        (overlay-put o 'face anything-visible-mark-face)
        (overlay-put o 'source (assoc-default 'name (anything-get-current-source)))
        (overlay-put o 'string (buffer-substring (overlay-start o) (overlay-end o)))
        (add-to-list 'anything-visible-mark-overlays o)
        (push (buffer-substring-no-properties (point-at-bol) (point-at-eol)) anything-c-marked-candidate-list)))
    (anything-next-line)))

(add-hook 'anything-after-initialize-hook (lambda ()
                                   (setq anything-c-marked-candidate-list nil)))

(add-hook 'anything-after-action-hook (lambda ()
                                   (setq anything-c-marked-candidate-list nil)))

(defun anything-revive-visible-mark ()
  (interactive)
  (with-current-buffer anything-buffer
    (loop for o in anything-visible-mark-overlays do
          (goto-char (point-min))
          (when (search-forward (overlay-get o 'string) nil t)
            (forward-line -1)
            (when (save-excursion
                    (goto-char (anything-get-previous-header-pos))
                    (equal (overlay-get o 'source)
                           (buffer-substring (line-beginning-position) (line-end-position))))
              (move-overlay o (line-beginning-position) (1+ (line-end-position))))))))
(add-hook 'anything-update-hook 'anything-revive-visible-mark)

(defun anything-next-visible-mark (&optional prev)
  (interactive)
  (with-anything-window
    (setq anything-visible-mark-overlays
          (sort* anything-visible-mark-overlays
                 '< :key 'overlay-start))
    (let ((i (position-if (lambda (o) (< (point) (overlay-start o)))
                          anything-visible-mark-overlays)))
      (when prev
          (if (not i) (setq i (length anything-visible-mark-overlays)))
          (if (equal (point) (overlay-start (nth (1- i) anything-visible-mark-overlays)))
              (setq i (1- i))))
      (when i
        (goto-char (overlay-start (nth (if prev (1- i) i) anything-visible-mark-overlays)))
        (anything-mark-current-line)))))

(defun anything-prev-visible-mark ()
  (interactive)
  (anything-next-visible-mark t))

;; (@* "Utility: `find-file' integration")
(defun anything-quit-and-find-file ()
  "Drop into `find-file' from `anything' like `iswitchb-find-file'.
This command is a simple example of `anything-run-after-quit'."
  (interactive)
  (anything-run-after-quit 'call-interactively 'find-file))

;; (@* "Utility: Selection Paste")
(defun anything-yank-selection ()
  "Set minibuffer contents to current selection."
  (interactive)
  (delete-minibuffer-contents)
  (insert (anything-get-selection nil t)))

(defun anything-kill-selection-and-quit ()
  "Store current selection to kill ring.
You can paste it by typing C-y."
  (interactive)
  (anything-run-after-quit
   (lambda (sel)
     (kill-new sel)
     (message "Killed: %s" sel))
   (anything-get-selection nil t)))


;; (@* "Utility: Automatical execution of persistent-action")
(define-minor-mode anything-follow-mode
  "If this mode is on, persistent action is executed everytime the cursor is moved."
  nil " AFollow" :global t)

(defun anything-follow-execute-persistent-action-maybe ()
  "Execute persistent action after `anything-input-idle-delay' secs when `anything-follow-mode' is enabled."
  (and anything-follow-mode
       (sit-for anything-input-idle-delay)
       (anything-window)
       (anything-get-selection)
       (save-excursion
         (anything-execute-persistent-action))))

;; (@* "Utility: Incremental search within results (unmaintained)")

(defvar anything-isearch-original-global-map nil
  "Original global map before Anything isearch is started.")

(defvar anything-isearch-original-message-timeout nil
  "Original message timeout before Anything isearch is started.")

(defvar anything-isearch-pattern nil
  "The current isearch pattern.")

(defvar anything-isearch-message-suffix ""
  "Message suffix indicating the current state of the search.")

(defvar anything-isearch-original-point nil
  "Original position of point before isearch is started.")

(defvar anything-isearch-original-window nil
  "Original selected window before isearch is started.")

(defvar anything-isearch-original-cursor-in-non-selected-windows nil
  "Original value of cursor-in-non-selected-windows before isearch is started.")

(defvar anything-isearch-original-post-command-hook nil
  "Original value of post-command-hook before isearch is started.")

(defvar anything-isearch-match-positions nil
  "Stack of positions of matches or non-matches.

It's a list of plists with two properties: `event', the last user
 event, `start', the start position of the current match, and
 `pos', the position of point after that event.

The value of `event' can be the following symbols: `char' if a
character was typed, `error' if a non-matching character was
typed, `search' if a forward search had to be done after a
character, and `search-again' if a search was done for the next
occurrence of the current pattern.")

(defvar anything-isearch-match-start nil
  "Start position of the current match.")


(defun anything-isearch ()
  "Start incremental search within results. (UNMAINTAINED)"
  (interactive)
  (if (zerop (buffer-size (get-buffer (anything-buffer-get))))
      (message "There are no results.")

    (setq anything-isearch-original-message-timeout minibuffer-message-timeout)
    (setq minibuffer-message-timeout nil)

    (setq anything-isearch-original-global-map global-map)

    (condition-case nil
        (progn
          (setq anything-isearch-original-window (selected-window))
          (select-window (anything-window))
          (setq cursor-type t)

          (setq anything-isearch-original-post-command-hook
                (default-value 'post-command-hook))
          (setq-default post-command-hook nil)
          (add-hook 'post-command-hook 'anything-isearch-post-command)

          (use-global-map anything-isearch-map)
          (setq overriding-terminal-local-map anything-isearch-map)

          (setq anything-isearch-pattern "")

          (setq anything-isearch-original-cursor-in-non-selected-windows
                cursor-in-non-selected-windows)
          (setq cursor-in-non-selected-windows nil) 

          (setq anything-isearch-original-point (point-marker))
          (goto-char (point-min))
          (forward-line)
          (anything-mark-current-line)

          (setq anything-isearch-match-positions nil)
          (setq anything-isearch-match-start (point-marker))

          (if anything-isearch-overlay
              ;; make sure the overlay belongs to the anything buffer
              (move-overlay anything-isearch-overlay (point-min) (point-min)
                            (get-buffer (anything-buffer-get)))

            (setq anything-isearch-overlay (make-overlay (point-min) (point-min)))
            (overlay-put anything-isearch-overlay 'face anything-isearch-match-face))

          (setq anything-isearch-message-suffix
                (substitute-command-keys "cancel with \\[anything-isearch-cancel]")))

      (error (anything-isearch-cleanup)))))


(defun anything-isearch-post-command ()
  "Print the current pattern after every command."
  (anything-isearch-message)
  (when (anything-window)
    (with-anything-window
      (move-overlay anything-isearch-overlay anything-isearch-match-start (point)
                    (get-buffer (anything-buffer-get))))))


(defun anything-isearch-printing-char ()
  "Add printing char to the pattern."
  (interactive)
  (let ((char (char-to-string last-command-char)))
    (setq anything-isearch-pattern (concat anything-isearch-pattern char))

    (with-anything-window
      (if (looking-at char)
          (progn
            (push (list 'event 'char
                        'start anything-isearch-match-start
                        'pos (point-marker))
                  anything-isearch-match-positions)
            (forward-char))

        (let ((start (point)))
          (while (and (re-search-forward anything-isearch-pattern nil t)
                      (anything-pos-header-line-p)))
          (if (or (anything-pos-header-line-p)
                  (eq start (point)))
              (progn
                (goto-char start)
                (push (list 'event 'error
                            'start anything-isearch-match-start
                            'pos (point-marker))
                      anything-isearch-match-positions))

            (push (list 'event 'search
                        'start anything-isearch-match-start
                        'pos (copy-marker start))
                  anything-isearch-match-positions)
            (setq anything-isearch-match-start (copy-marker (match-beginning 0))))))
  
      (anything-mark-current-line))))


(defun anything-isearch-again ()
  "Search again for the current pattern"
  (interactive)
  (if (equal anything-isearch-pattern "")
      (setq anything-isearch-message-suffix "no pattern yet")

    (with-anything-window
      (let ((start (point)))
        (while (and (re-search-forward anything-isearch-pattern nil t)
                    (anything-pos-header-line-p)))
        (if (or (anything-pos-header-line-p)
                (eq start (point)))
            (progn
              (goto-char start)
              (unless (eq 'error (plist-get (car anything-isearch-match-positions)
                                            'event))
                (setq anything-isearch-message-suffix "no more matches")))

          (push (list 'event 'search-again
                      'start anything-isearch-match-start
                      'pos (copy-marker start))
                anything-isearch-match-positions)
          (setq anything-isearch-match-start (copy-marker (match-beginning 0)))

          (anything-mark-current-line))))))


(defun anything-isearch-delete ()
  "Undo last event."
  (interactive)
  (unless (equal anything-isearch-pattern "")
    (let ((last (pop anything-isearch-match-positions)))
      (unless (eq 'search-again (plist-get last 'event))
        (setq anything-isearch-pattern
              (substring anything-isearch-pattern 0 -1)))

      (with-anything-window      
        (goto-char (plist-get last 'pos))
        (setq anything-isearch-match-start (plist-get last 'start))
        (anything-mark-current-line)))))


(defun anything-isearch-default-action ()
  "Execute the default action for the selected candidate."
  (interactive)
  (anything-isearch-cleanup)
  (with-current-buffer (anything-buffer-get) (anything-exit-minibuffer)))


(defun anything-isearch-select-action ()
  "Choose an action for the selected candidate."
  (interactive)
  (anything-isearch-cleanup)
  (with-anything-window
    (anything-select-action)))


(defun anything-isearch-cancel ()
  "Cancel Anything isearch."
  (interactive)
  (anything-isearch-cleanup)
  (when (anything-window)
    (with-anything-window
      (goto-char anything-isearch-original-point)
      (anything-mark-current-line))))


(defun anything-isearch-cleanup ()
  "Clean up the mess."
  (setq minibuffer-message-timeout anything-isearch-original-message-timeout)
  (with-current-buffer (anything-buffer-get)
    (setq overriding-terminal-local-map nil)
    (setq cursor-type nil)
    (setq cursor-in-non-selected-windows
          anything-isearch-original-cursor-in-non-selected-windows))
  (when anything-isearch-original-window
    (select-window anything-isearch-original-window))

  (use-global-map anything-isearch-original-global-map)
  (setq-default post-command-hook anything-isearch-original-post-command-hook)
  (when (overlayp anything-isearch-overlay) 
    (delete-overlay anything-isearch-overlay)))


(defun anything-isearch-message ()
  "Print prompt."
  (if (and (equal anything-isearch-message-suffix "")
           (eq (plist-get (car anything-isearch-match-positions) 'event)
               'error))
      (setq anything-isearch-message-suffix "failing"))

  (unless (equal anything-isearch-message-suffix "")
    (setq anything-isearch-message-suffix 
          (concat " [" anything-isearch-message-suffix "]")))

  (message (concat "Search within results: "
                   anything-isearch-pattern
                   anything-isearch-message-suffix))

  (setq anything-isearch-message-suffix ""))


;; (@* "Utility: Iswitchb integration (unmaintained)")

(defvar anything-iswitchb-candidate-selected nil
  "Indicates whether an anything candidate is selected from iswitchb.")

(defvar anything-iswitchb-frame-configuration nil
  "Saved frame configuration, before anything buffer was displayed.")

(defvar anything-iswitchb-saved-keys nil
  "The original in iswitchb before binding anything keys.")


(defun anything-iswitchb-setup ()
  "Integrate anything completion into iswitchb (UNMAINTAINED).

If the user is idle for `anything-iswitchb-idle-delay' seconds
after typing something into iswitchb then anything candidates are
shown for the current iswitchb input.

ESC cancels anything completion and returns to normal iswitchb.

Some key bindings in `anything-map' are modified.
See also `anything-iswitchb-setup-keys'."
  (interactive)

  (require 'iswitchb)

  ;; disable timid completion during iswitchb
  (put 'iswitchb-buffer 'timid-completion 'disabled)
  (add-hook 'minibuffer-setup-hook  'anything-iswitchb-minibuffer-setup)

  (defadvice iswitchb-visit-buffer
    (around anything-iswitchb-visit-buffer activate)
    (if anything-iswitchb-candidate-selected
        (anything-execute-selection-action)
      ad-do-it))

  (defadvice iswitchb-possible-new-buffer
    (around anything-iswitchb-possible-new-buffer activate)
    (if anything-iswitchb-candidate-selected
        (anything-execute-selection-action)
      ad-do-it))
  (anything-iswitchb-setup-keys)
  (message "Iswitchb integration is activated."))

(defun anything-iswitchb-setup-keys ()
  "Modify `anything-map' for anything-iswitchb users.

C-p is used instead of M-p, because anything uses ESC
 (currently hardcoded) for `anything-iswitchb-cancel-anything' and
Emacs handles ESC and Meta as synonyms, so ESC overrides
other commands with Meta prefix.

Note that iswitchb uses M-p and M-n by default for history
navigation, so you should bind C-p and C-n in
`iswitchb-mode-map' if you use the history keys and don't want
to use different keys for iswitchb while anything is not yet
kicked in. These keys are not bound automatically by anything
in `iswitchb-mode-map' because they (C-n at least) already have
a standard iswitchb binding which you might be accustomed to.

Binding M-s is used instead of C-s, because C-s has a binding in
iswitchb.  You can rebind it AFTER `anything-iswitchb-setup'.

Unbind C-r to prevent problems during anything-isearch."
  (define-key anything-map (kbd "C-s") nil)
  (define-key anything-map (kbd "M-p") nil)
  (define-key anything-map (kbd "M-n") nil)
  (define-key anything-map (kbd "M-v") nil)
  (define-key anything-map (kbd "C-v") nil)
  (define-key anything-map (kbd "C-p") 'anything-previous-history-element)
  (define-key anything-map (kbd "C-n") 'anything-next-history-element)
  (define-key anything-map (kbd "M-s") nil)
  (define-key anything-map (kbd "M-s") 'anything-isearch)
  (define-key anything-map (kbd "C-r") nil))

(defun anything-iswitchb-minibuffer-setup ()
  (when (eq this-command 'iswitchb-buffer)
    (add-hook 'minibuffer-exit-hook  'anything-iswitchb-minibuffer-exit)

    (setq anything-iswitchb-frame-configuration nil)
    (setq anything-iswitchb-candidate-selected nil)
    (add-hook 'anything-update-hook 'anything-iswitchb-handle-update)

    (anything-initialize)
    
    (add-hook 'post-command-hook 'anything-iswitchb-check-input)))


(defun anything-iswitchb-minibuffer-exit ()
  (remove-hook 'minibuffer-exit-hook  'anything-iswitchb-minibuffer-exit)
  (remove-hook 'post-command-hook 'anything-iswitchb-check-input)
  (remove-hook 'anything-update-hook 'anything-iswitchb-handle-update)

  (anything-cleanup)

  (when anything-iswitchb-frame-configuration
    (anything-set-frame/window-configuration anything-iswitchb-frame-configuration)
    (setq anything-iswitchb-frame-configuration nil)))


(defun anything-iswitchb-check-input ()
  "Extract iswitchb input and check if it needs to be handled."
  (declare (special iswitchb-text))
  (if (or anything-iswitchb-frame-configuration
          (sit-for anything-iswitchb-idle-delay))
      (anything-check-new-input iswitchb-text)))


(defun anything-iswitchb-handle-update ()
  "Pop up the anything buffer if it's not empty and it's not
shown yet and bind anything commands in iswitchb."
  (unless (or (equal (buffer-size (get-buffer anything-buffer)) 0)
              anything-iswitchb-frame-configuration)
    (setq anything-iswitchb-frame-configuration (anything-current-frame/window-configuration))

    (save-selected-window 
      (if (not anything-samewindow)
          (pop-to-buffer anything-buffer)

        (select-window (get-lru-window))
        (switch-to-buffer anything-buffer)))

    (with-current-buffer (window-buffer (active-minibuffer-window))
      (let* ((anything-prefix "anything-")
             (prefix-length (length anything-prefix))
             (commands 
              (delete-dups
               (remove-if 'null
                          (mapcar 
                           (lambda (binding)
                             (let ((command (cdr binding)))
                               (when (and (symbolp command)
                                          (eq (compare-strings 
                                               anything-prefix 
                                               0 prefix-length
                                               (symbol-name command)
                                               0 prefix-length)
                                              t))
                                 command)))
                           (cdr anything-map)))))
             (bindings (mapcar (lambda (command)
                                 (cons command 
                                       (where-is-internal command anything-map)))
                               commands)))

        (push (list 'anything-iswitchb-cancel-anything (kbd "<ESC>"))
              bindings)

        (setq anything-iswitchb-saved-keys nil)

      (let* ((iswitchb-prefix "iswitchb-")
             (prefix-length (length iswitchb-prefix)))
        (dolist (binding bindings)
          (dolist (key (cdr binding))
            (let ((old-command (lookup-key (current-local-map) key)))
              (unless (and anything-iswitchb-dont-touch-iswithcb-keys
                           (symbolp old-command)
                           (eq (compare-strings iswitchb-prefix 
                                                0 prefix-length
                                                (symbol-name old-command)
                                                0 prefix-length)
                               t))
                (push (cons key old-command)
                      anything-iswitchb-saved-keys)
                (define-key (current-local-map) key (car binding)))))))))))


(defun anything-iswitchb-cancel-anything ()
  "Cancel anything completion and return to standard iswitchb."
  (interactive)
  (save-excursion
    (dolist (binding anything-iswitchb-saved-keys)
      (define-key (current-local-map) (car binding) (cdr binding)))
    (anything-iswitchb-minibuffer-exit)))

;; (@* "Compatibility")

;; Copied assoc-default from XEmacs version 21.5.12
(unless (fboundp 'assoc-default)
  (defun assoc-default (key alist &optional test default)
    "Find object KEY in a pseudo-alist ALIST.
ALIST is a list of conses or objects.  Each element (or the element's car,
if it is a cons) is compared with KEY by evaluating (TEST (car elt) KEY).
If that is non-nil, the element matches;
then `assoc-default' returns the element's cdr, if it is a cons,
or DEFAULT if the element is not a cons.

If no element matches, the value is nil.
If TEST is omitted or nil, `equal' is used."
    (let (found (tail alist) value)
      (while (and tail (not found))
        (let ((elt (car tail)))
          (when (funcall (or test 'equal) (if (consp elt) (car elt) elt) key)
            (setq found t value (if (consp elt) (cdr elt) default))))
        (setq tail (cdr tail)))
      value)))

;; Function not available in XEmacs, 
(unless (fboundp 'minibuffer-contents)
  (defun minibuffer-contents ()
    "Return the user input in a minbuffer as a string.
The current buffer must be a minibuffer."
    (field-string (point-max)))

  (defun delete-minibuffer-contents  ()
    "Delete all user input in a minibuffer.
The current buffer must be a minibuffer."
    (delete-field (point-max))))

;; Function not available in older Emacs (<= 22.1).
(unless (fboundp 'buffer-chars-modified-tick)
  (defun buffer-chars-modified-tick (&optional buffer)
    "Return BUFFER's character-change tick counter.
Each buffer has a character-change tick counter, which is set to the
value of the buffer's tick counter (see `buffer-modified-tick'), each
time text in that buffer is inserted or deleted.  By comparing the
values returned by two individual calls of `buffer-chars-modified-tick',
you can tell whether a character change occurred in that buffer in
between these calls.  No argument or nil as argument means use current
buffer as BUFFER."
    (with-current-buffer (or buffer (current-buffer))
      (if (listp buffer-undo-list)
          (length buffer-undo-list)
        (buffer-modified-tick)))))


;; (@* "Unit Tests")

(defun* anything-test-candidates (sources &optional (input "") (compile-source-functions anything-compile-source-functions-default))
  "Test helper function for anything.
Given pseudo `anything-sources' and `anything-pattern', returns list like
  ((\"source name1\" (\"candidate1\" \"candidate2\"))
   (\"source name2\" (\"candidate3\" \"candidate4\")))
"
  (let ((anything-test-mode t)
        anything-enable-digit-shortcuts
        anything-candidate-cache
        (anything-sources (anything-normalize-sources sources))
        (anything-compile-source-functions compile-source-functions)
        anything-before-initialize-hook
        anything-after-initialize-hook
        anything-update-hook
        anything-test-candidate-list)
    (get-buffer-create anything-buffer)

    (anything-initialize)
    (setq anything-input input anything-pattern input)
    (anything-update)
    ;; test-mode spec: select 1st candidate!
    (with-current-buffer anything-buffer
      (forward-line 1)
      (anything-mark-current-line))
    (prog1
        anything-test-candidate-list
      (anything-cleanup))))

(defmacro anything-test-update (sources pattern)
  "Test helper macro for anything. It is meant for testing *anything* buffer contents."
  `(progn (stub anything-get-sources => ,sources)
          (stub run-hooks => nil)
          (stub anything-maybe-fit-frame => nil)
          (stub run-with-idle-timer => nil)
          (let (anything-test-mode (anything-pattern ,pattern))
            (anything-update))))

;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "anything-current-buffer")
      (expect "__a_buffer"
        (with-current-buffer (get-buffer-create "__a_buffer")
          (anything-test-candidates '(((name . "FOO"))) "")
          (prog1
              (buffer-name anything-current-buffer)
            (kill-buffer "__a_buffer")
            )))
      (desc "anything-buffer-file-name")
      (expect (regexp "/__a_file__")
        (with-current-buffer (get-buffer-create "__a_file__")
          (setq buffer-file-name "/__a_file__")
          (anything-test-candidates '(((name . "FOO"))) "")
          (prog1
              anything-buffer-file-name
            ;;(kill-buffer "__a_file__")
            )))
      (desc "anything-compile-sources")
      (expect '(((name . "foo")))
        (anything-compile-sources '(((name . "foo"))) nil)
        )
      (expect '(((name . "foo") (type . test) (action . identity)))
        (let ((anything-type-attributes '((test (action . identity)))))
          (anything-compile-sources '(((name . "foo") (type . test)))
                                    '(anything-compile-source--type))))
      (desc "anything-sources accepts symbols")
      (expect '(((name . "foo")))
        (let* ((foo '((name . "foo"))))
          (anything-compile-sources '(foo) nil)))
      (desc "anything-get-sources action")
      (expect '(((name . "Actions") (candidates . actions)))
        (stub anything-action-window => t)
        (let (anything-compiled-sources
              (anything-sources '(((name . "Actions") (candidates . actions)))))
          (anything-get-sources)))
      (desc "get-buffer-create candidates-buffer")
      (expect '(((name . "many") (init . many-init)
                 (candidates-in-buffer . anything-candidates-in-buffer)
                 (candidates . anything-candidates-in-buffer)
                 (volatile) (match identity)))
        (anything-compile-sources
         '(((name . "many") (init . many-init)
            (candidates-in-buffer . anything-candidates-in-buffer)))
         '(anything-compile-source--candidates-in-buffer)))
      (expect '(((name . "many") (init . many-init)
                 (candidates-in-buffer)
                 (candidates . anything-candidates-in-buffer)
                 (volatile) (match identity)))
        (anything-compile-sources
         '(((name . "many") (init . many-init)
            (candidates-in-buffer)))
         '(anything-compile-source--candidates-in-buffer)))
      (expect '(((name . "many") (init . many-init)
                 (candidates-in-buffer)
                 (type . test)
                 (action . identity)
                 (candidates . anything-candidates-in-buffer)
                 (volatile) (match identity)))
        (let ((anything-type-attributes '((test (action . identity)))))
          (anything-compile-sources
           '(((name . "many") (init . many-init)
              (candidates-in-buffer)
              (type . test)))
           '(anything-compile-source--type
             anything-compile-source--candidates-in-buffer))))

      (desc "anything-get-candidates")
      (expect '("foo" "bar")
        (anything-get-candidates '((name . "foo") (candidates "foo" "bar"))))
      (expect '("FOO" "BAR")
        (anything-get-candidates '((name . "foo") (candidates "foo" "bar")
                                   (candidate-transformer
                                    . (lambda (cands) (mapcar 'upcase cands))))))
      (expect '("foo" "bar")
        (anything-get-candidates '((name . "foo")
                                   (candidates . (lambda () '("foo" "bar"))))))
      (desc "anything-compute-matches")
      (expect '("foo" "bar")
        (let ((anything-pattern ""))
          (anything-compute-matches '((name . "FOO") (candidates "foo" "bar") (volatile)))))
      (expect '("foo")
        (let ((anything-pattern "oo"))
          (anything-compute-matches '((name . "FOO") (candidates "foo" "bar") (volatile)))))
      (expect '("bar")
        (let ((anything-pattern "^b"))
          (anything-compute-matches '((name . "FOO") (candidates "foo" "bar") (volatile)))))
      (expect '("a" "b")
        (let ((anything-pattern "")
              (anything-candidate-number-limit 2))
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      (expect '("a" "b")
        (let ((anything-pattern ".")
              (anything-candidate-number-limit 2))
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      (expect '("a" "b" "c")
        (let ((anything-pattern "")
              anything-candidate-number-limit)
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      (expect '("a" "b" "c")
        (let ((anything-pattern "[abc]")
              anything-candidate-number-limit)
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      ;; using anything-test-candidate-list
      (desc "anything-test-candidates")
      (expect '(("FOO" ("foo" "bar")))
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar")))))
      (expect '(("FOO" ("bar")))
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar"))) "ar"))
      (expect '(("T1" ("hoge" "aiue"))
                ("T2" ("test" "boke")))
        (anything-test-candidates '(((name . "T1") (candidates "hoge" "aiue"))
                                    ((name . "T2") (candidates "test" "boke")))))
      (expect '(("T1" ("hoge"))
                ("T2" ("boke")))
        (anything-test-candidates '(((name . "T1") (candidates "hoge" "aiue"))
                                    ((name . "T2") (candidates "test" "boke"))) "o"))
      (desc "requires-pattern attribute")
      (expect nil
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (requires-pattern . 1)))))
      (expect '(("FOO" ("bar")))
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (requires-pattern . 1))) "b"))
      (desc "delayed attribute(for test)")
      (expect '(("T2" ("boke"))
                ("T1" ("hoge")))
        (anything-test-candidates
         '(((name . "T1") (candidates "hoge" "aiue") (delayed))
           ((name . "T2") (candidates "test" "boke")))
         "o"))
      (desc "match attribute(prefix search)")
      (expect '(("FOO" ("bar")))
        (anything-test-candidates
         '(((name . "FOO") (candidates "foo" "bar")
            (match (lambda (c) (string-match (concat "^" anything-pattern) c)))))
         "ba"))
      (expect nil
        (anything-test-candidates
         '(((name . "FOO") (candidates "foo" "bar")
            (match (lambda (c) (string-match (concat "^" anything-pattern) c)))))
         "ar"))
      (desc "init attribute")
      (expect '(("FOO" ("bar")))
        (let (v)
          (anything-test-candidates
           '(((name . "FOO") (init . (lambda () (setq v '("foo" "bar"))))
              (candidates . v)))
           "ar")))
      (desc "candidate-transformer attribute")
      (expect '(("FOO" ("BAR")))
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (candidate-transformer
                                      . (lambda (cands) (mapcar 'upcase cands)))))
                                  "ar"))
      (desc "filtered-candidate-transformer attribute")
      ;; needs more tests
      (expect '(("FOO" ("BAR")))
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (filtered-candidate-transformer
                                      . (lambda (cands src) (mapcar 'upcase cands)))))
                                  "ar"))
      (desc "anything-candidates-in-buffer-1")
      (expect nil
        (anything-candidates-in-buffer-1 nil))
      (expect '("foo+" "bar+" "baz+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (let ((anything-candidate-number-limit 5))
            (anything-candidates-in-buffer-1 (current-buffer) ""))))
      (expect '("foo+" "bar+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (let ((anything-candidate-number-limit 2))
            (anything-candidates-in-buffer-1 (current-buffer) ""))))
      (expect '("foo+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (anything-candidates-in-buffer-1 (current-buffer) "oo\\+")))
      (expect '("foo+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (anything-candidates-in-buffer-1 
           (current-buffer) "oo+"
           #'buffer-substring-no-properties '(search-forward))))
      (expect '(("foo+" "FOO+"))
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (anything-candidates-in-buffer-1
           (current-buffer) "oo\\+"
           (lambda (s e)
             (let ((l (buffer-substring-no-properties s e)))
               (list l (upcase l)))))))
      (desc "anything-candidates-in-buffer")
      (expect '(("TEST" ("foo+" "bar+" "baz+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))))
      (expect '(("TEST" ("foo+" "bar+" "baz+")))
        (let (anything-candidate-number-limit)
          (anything-test-candidates
           '(((name . "TEST")
              (init
               . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                              (insert "foo+\nbar+\nbaz+\n"))))
              (candidates . anything-candidates-in-buffer)
              (match identity)
              (volatile))))))
      (expect '(("TEST" ("foo+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo\\+"))
      (desc "search attribute")
      (expect '(("TEST" ("foo+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\nooo\n"))))
            (search search-forward)
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      (expect '(("TEST" ("foo+" "ooo")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\nooo\n"))))
            (search search-forward re-search-forward)
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      (expect '(("TEST" ("foo+" "ooo")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\nooo\n"))))
            (search re-search-forward search-forward)
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      (expect '(("TEST" ("ooo" "foo+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "bar+\nbaz+\nooo\nfoo+\n"))))
            (search re-search-forward search-forward)
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      ;; faster exact match
      (expect '(("TEST" ("bar+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "bar+\nbaz+\nooo\nfoo+\n"))))
            (search (lambda (pattern &rest _)
                      (and (search-forward (concat "\n" pattern "\n") nil t)
                           (forward-line -1))))
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "bar+"))
      ;; faster prefix match
      (expect '(("TEST" ("bar+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "bar+\nbaz+\nooo\nfoo+\n"))))
            (search (lambda (pattern &rest _)
                      (search-forward (concat "\n" pattern) nil t)))
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "ba"))
      (desc "anything-current-buffer-is-modified")
      (expect '(("FOO" ("modified")))
        (let ((sources '(((name . "FOO")
                          (candidates
                           . (lambda ()
                               (if (anything-current-buffer-is-modified)
                                   '("modified")
                                 '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources))))
      (expect '(("FOO" ("unmodified")))
        (let ((sources '(((name . "FOO")
                          (candidates
                           . (lambda ()
                               (if (anything-current-buffer-is-modified)
                                   '("modified")
                                 '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources)
            (anything-test-candidates sources))))
      (expect '(("FOO" ("modified")))
        (let ((sources '(((name . "FOO")
                          (candidates
                           . (lambda ()
                               (if (anything-current-buffer-is-modified)
                                   '("modified")
                                 '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources)
            (insert "2")
            (anything-test-candidates sources))))
      (expect '(("BAR" ("modified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources1)
            (anything-test-candidates sources2))))
      (expect '(("FOO" ("unmodified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources1)
            (anything-test-candidates sources2)
            (anything-test-candidates sources1))))
      (expect '(("BAR" ("unmodified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources1)
            (anything-test-candidates sources2)
            (anything-test-candidates sources2))))
      (expect '(("BAR" ("modified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources1)
            (anything-test-candidates sources2)
            (with-temp-buffer
              (anything-test-candidates sources2)))))
      (desc "anything-source-name")
      (expect "FOO"
        (let (v)
          (anything-test-candidates '(((name . "FOO")
                                       (init
                                        . (lambda () (setq v anything-source-name)))
                                       (candidates "ok"))))
          v))
      (expect "FOO"
        (let (v)
          (anything-test-candidates '(((name . "FOO")
                                       (candidates
                                        . (lambda ()
                                            (setq v anything-source-name)
                                            '("ok"))))))
          v))
      (expect "FOO"
        (let (v)
          (anything-test-candidates '(((name . "FOO")
                                       (candidates "ok")
                                       (candidate-transformer
                                        . (lambda (c)
                                            (setq v anything-source-name)
                                            c)))))
          v))
      (expect "FOO"
        (let (v)
          (anything-test-candidates '(((name . "FOO")
                                       (candidates "ok")
                                       (filtered-candidate-transformer
                                        . (lambda (c s)
                                            (setq v anything-source-name)
                                            c)))))
          v))
      (expect "FOO"
        (let (v)
          (anything-test-candidates '(((name . "FOO")
                                       (candidates "ok")
                                       (display-to-real
                                        . (lambda (c)
                                            (setq v anything-source-name)
                                            c))
                                       (action . identity))))
          (anything-execute-selection-action)
          v))
      (desc "anything-candidate-buffer create")
      (expect " *anything candidates:FOO*"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (buf (anything-candidate-buffer 'global)))
          (prog1 (buffer-name buf)
            (kill-buffer buf))))
      (expect " *anything candidates:FOO*aTestBuffer"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (anything-current-buffer (get-buffer-create "aTestBuffer"))
               (buf (anything-candidate-buffer 'local)))
          (prog1 (buffer-name buf)
            (kill-buffer anything-current-buffer)
            (kill-buffer buf))))
      (expect 0
        (let (anything-candidate-buffer-alist
              (anything-source-name "FOO") buf)
          (with-current-buffer  (anything-candidate-buffer 'global)
            (insert "1"))
          (setq buf  (anything-candidate-buffer 'global))
          (prog1 (buffer-size buf)
            (kill-buffer buf))))
      (desc "anything-candidate-buffer get-buffer")
      (expect " *anything candidates:FOO*"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (buf (anything-candidate-buffer 'global)))
          (prog1 (buffer-name (anything-candidate-buffer))
            (kill-buffer buf))))
      (expect " *anything candidates:FOO*aTestBuffer"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (anything-current-buffer (get-buffer-create "aTestBuffer"))
               (buf (anything-candidate-buffer 'local)))
          (prog1 (buffer-name (anything-candidate-buffer))
            (kill-buffer anything-current-buffer)
            (kill-buffer buf))))
      (expect " *anything candidates:FOO*"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (buf-local (anything-candidate-buffer 'local))
               (buf-global (anything-candidate-buffer 'global)))
          (prog1 (buffer-name (anything-candidate-buffer))
            (kill-buffer buf-local)
            (kill-buffer buf-global))))
      (expect " *anything candidates:FOO*aTestBuffer"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (anything-current-buffer (get-buffer-create "aTestBuffer"))
               (buf-global (anything-candidate-buffer 'global))
               (buf-local (anything-candidate-buffer 'local)))
          (prog1 (buffer-name (anything-candidate-buffer))
            (kill-buffer buf-local)
            (kill-buffer buf-global))))
      (expect nil
        (let* (anything-candidate-buffer-alist
               (anything-source-name "NOP__"))
          (anything-candidate-buffer)))
      (desc "anything-candidate-buffer register-buffer")
      (expect " *anything test candidates*"
        (let (anything-candidate-buffer-alist
              (buf (get-buffer-create " *anything test candidates*")))
          (with-current-buffer buf
            (insert "1\n2\n")
            (prog1 (buffer-name (anything-candidate-buffer buf))
              (kill-buffer (current-buffer))))))
      (expect " *anything test candidates*"
        (let (anything-candidate-buffer-alist
              (buf (get-buffer-create " *anything test candidates*")))
          (with-current-buffer buf
            (insert "1\n2\n")
            (anything-candidate-buffer buf)
            (prog1 (buffer-name (anything-candidate-buffer))
              (kill-buffer (current-buffer))))))
      (expect "1\n2\n"
        (let (anything-candidate-buffer-alist
              (buf (get-buffer-create " *anything test candidates*")))
          (with-current-buffer buf
            (insert "1\n2\n")
            (anything-candidate-buffer buf)
            (prog1 (buffer-string)
              (kill-buffer (current-buffer))))))
      (expect "buf1"
        (let (anything-candidate-buffer-alist
              (anything-source-name "foo")
              (buf1 (get-buffer-create "buf1"))
              (buf2 (get-buffer-create "buf2")))
          (anything-candidate-buffer buf1)
          (anything-candidate-buffer buf2)
          (prog1 (buffer-name (anything-candidate-buffer buf1))
            (kill-buffer buf1)
            (kill-buffer buf2))))
      (desc "action attribute")
      (expect "foo"
        (anything-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (action ("identity" . identity)))))
        (anything-execute-selection-action))
      (expect "foo"
        (anything-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (action ("identity" . (lambda (c) (identity c)))))))
        (anything-execute-selection-action))
      (desc "anything-execute-selection-action")
      (expect "FOO"
        (anything-execute-selection-action
         "foo" '(("upcase" . upcase))  nil #'identity))
      (expect "FOO"
        (anything-execute-selection-action
         "foo" '(("upcase" . (lambda (c) (upcase c)))) nil #'identity))
      (desc "display-to-real attribute")
      (expect "FOO"
        (anything-execute-selection-action
         "foo"
         '(("identity" . identity))
         nil
         #'upcase
         ))
      (expect "FOO"
        (anything-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (display-to-real . upcase)
            (action ("identity" . identity)))))
        (anything-execute-selection-action))
      (desc "cleanup test")
      (expect 'cleaned
        (let (v)
          (anything-test-candidates
           '(((name . "TEST")
              (cleanup . (lambda () (setq v 'cleaned))))))
          v))
      (desc "anything-get-current-source")
      ;; in init/candidates/action/candidate-transformer/filtered-candidate-transformer
      ;; display-to-real/cleanup function
      (expect "FOO"
        (assoc-default
         'name
         (anything-funcall-with-source '((name . "FOO")) 'anything-get-current-source)))
      ;; init
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (init . (lambda () (setq v (anything-get-current-source)))))))
          (assoc-default 'name v)))
      ;; candidates
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates . (lambda () (setq v (anything-get-current-source)) '("a"))))))
          (assoc-default 'name v)))
      ;; action
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (action
               . (lambda (c) (setq v (anything-get-current-source)) c)))))
          (anything-execute-selection-action)
          (assoc-default 'name v)))
      ;; candidate-transformer
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (candidate-transformer
               . (lambda (c) (setq v (anything-get-current-source)) c)))))
          (assoc-default 'name v)))
      ;; filtered-candidate-transformer
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (filtered-candidate-transformer
               . (lambda (c s) (setq v (anything-get-current-source)) c)))))
          (assoc-default 'name v)))
      ;; action-transformer
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (action-transformer
               . (lambda (a c) (setq v (anything-get-current-source)) a))
              (action . identity))))
          (anything-execute-selection-action)
          (assoc-default 'name v)))
      ;; display-to-real
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (init . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                                   (insert "a\n"))))
              (candidates-in-buffer)
              (display-to-real
               . (lambda (c) (setq v (anything-get-current-source)) c))
              (action . identity))))
          (anything-execute-selection-action)
          (assoc-default 'name v)))
      ;; cleanup
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (cleanup
               . (lambda () (setq v (anything-get-current-source)))))))
          (assoc-default 'name v)))
      ;; candidates are displayed
      (expect "TEST"
        (anything-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (action ("identity" . identity)))))
        (assoc-default 'name (anything-get-current-source)))
      (desc "anything-attr")
      (expect "FOO"
        (anything-funcall-with-source
         '((name . "FOO"))
         (lambda ()
           (anything-attr 'name))))
      (expect 'fuga
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (hoge . fuga)
              (init . (lambda () (setq v (anything-attr 'hoge))))
              (candidates "a"))))
          v))
      (expect nil
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (init . (lambda () (setq v (anything-attr 'hoge))))
              (candidates "a"))))
          v))
      (expect nil
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (hoge)                    ;INCOMPATIBLE!
              (init . (lambda () (setq v (anything-attr 'hoge))))
              (candidates "a"))))
          v))
      (desc "anything-attr-defined")
      (expect (non-nil)
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (hoge)
              (init . (lambda () (setq v (anything-attr-defined 'hoge))))
              (candidates "a"))))
          v))      
      (expect nil
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (init . (lambda () (setq v (anything-attr-defined 'hoge))))
              (candidates "a"))))
          v))      
      (desc "anything-attrset")
      (expect '((name . "FOO") (hoge . 77))
        (let ((src '((name . "FOO") (hoge))))
          (anything-attrset 'hoge 77 src)
          src))
      (expect 77
        (anything-attrset 'hoge 77 '((name . "FOO") (hoge))))

      (expect '((name . "FOO") (hoge . 77))
        (let ((src '((name . "FOO") (hoge . 1))))
          (anything-attrset 'hoge 77 src)
          src))

      (expect '((name . "FOO") (hoge . 77) (x))
        (let ((src '((name . "FOO") (x))))
          (anything-attrset 'hoge 77 src)
          src))
      (expect 77
        (anything-attrset 'hoge 77 '((name . "FOO"))))
      (desc "anything-preselect")
      ;; entire candidate
      (expect "foo"
        (with-current-buffer (anything-create-anything-buffer t)
          (let ((anything-pattern "")
                (anything-test-mode t))
            (anything-process-source '((name . "test")
                                       (candidates "hoge" "foo" "bar")))
            (anything-preselect "foo")
            (anything-get-selection))))
      ;; regexp
      (expect "foo"
        (with-current-buffer (anything-create-anything-buffer t)
          (let ((anything-pattern "")
                (anything-test-mode t))
            (anything-process-source '((name . "test")
                                       (candidates "hoge" "foo" "bar")))
            (anything-preselect "fo+")
            (anything-get-selection))))
      ;; no match -> first entry
      (expect "hoge"
        (with-current-buffer (anything-create-anything-buffer t)
          (let ((anything-pattern "")
                (anything-test-mode t))
            (anything-process-source '((name . "test")
                                       (candidates "hoge" "foo" "bar")))
            (anything-preselect "not found")
            (anything-get-selection))))
      (desc "anything-check-new-input")
      (expect "newpattern"
        (stub anything-update)
        (stub anything-action-window)
        (let ((anything-pattern "pattern"))
          (anything-check-new-input "newpattern")
          anything-pattern))
      ;; anything-input == nil when action window is available
      (expect nil
        (stub anything-update)
        (stub anything-action-window => t)
        (let ((anything-pattern "pattern")
              anything-input)
          (anything-check-new-input "newpattern")
          anything-input))
      ;; anything-input == anything-pattern unless action window is available
      (expect "newpattern"
        (stub anything-update)
        (stub anything-action-window => nil)
        (let ((anything-pattern "pattern")
              anything-input)
          (anything-check-new-input "newpattern")
          anything-input))
      (expect (mock (anything-update))
        (stub anything-action-window)
        (let (anything-pattern)
          (anything-check-new-input "foo")))
      (desc "anything-update")
      (expect (mock (anything-process-source '((name . "1"))))
        (anything-test-update '(((name . "1"))) ""))
      ;; (find-function 'anything-update)
      ;; TODO el-mock.el should express 2nd call of function.
      ;;     (expect (mock (anything-process-source '((name . "2"))))
      ;;       (stub anything-get-sources => '(((name . "1")) ((name . "2"))))
      ;;       (stub run-hooks)
      ;;       (stub anything-maybe-fit-frame)
      ;;       (stub run-with-idle-timer)
      ;;       (anything-update))
      (expect (mock (run-with-idle-timer * nil 'anything-process-delayed-sources
                                         '(((name . "2") (delayed)))))
        (stub anything-get-sources => '(((name . "1"))
                                        ((name . "2") (delayed))))
        (stub run-hooks)
        (stub anything-maybe-fit-frame)
        (let ((anything-pattern "") anything-test-mode)
          (anything-update)))

      (expect (mock (run-with-idle-timer * nil 'anything-process-delayed-sources nil))
        (stub anything-get-sources => '(((name . "1"))
                                        ((name . "2"))))
        (stub run-hooks)
        (stub anything-maybe-fit-frame)
        (let ((anything-pattern "") anything-test-mode)
          (anything-update)))


      (desc "requires-pattern attribute")
      (expect (not-called anything-process-source)
        (anything-test-update '(((name . "1") (requires-pattern))) ""))
      (expect (not-called anything-process-source)
        (anything-test-update '(((name . "1") (requires-pattern . 3))) "xx"))

      (desc "delay")
      (expect (mock (sit-for 0.25))
        (stub with-current-buffer)
        (let ((anything-idle-delay 1.0)
              (anything-input-idle-delay 0.75))
          (anything-process-delayed-sources t)))
      (expect (mock (sit-for 0.0))
        (stub with-current-buffer)
        (let ((anything-idle-delay 0.2)
              (anything-input-idle-delay 0.5))
          (anything-process-delayed-sources t)))    
      (expect (mock (sit-for 0.5))
        (stub with-current-buffer)
        (let ((anything-idle-delay 0.5)
              (anything-input-idle-delay nil))
          (anything-process-delayed-sources t)))
      (desc "anything-normalize-sources")
      (expect '(anything-c-source-test)
        (anything-normalize-sources 'anything-c-source-test))
      (expect '(anything-c-source-test)
        (anything-normalize-sources '(anything-c-source-test)))
      (expect '(anything-c-source-test)
        (let ((anything-sources '(anything-c-source-test)))
          (anything-normalize-sources nil)))
      (desc "anything-get-action")
      (expect '(("identity" . identity))
        (stub buffer-size => 1)
        (stub anything-get-current-source => '((name . "test")
                                               (action ("identity" . identity))))
        (anything-get-action))
      (expect '((("identity" . identity)) "action-transformer is called")
        (stub buffer-size => 1)
        (stub anything-get-current-source
              => '((name . "test")
                   (action ("identity" . identity))
                   (action-transformer
                    . (lambda (actions selection)
                        (list actions selection)))))
        (stub anything-get-selection => "action-transformer is called")
        (anything-get-action))
      (desc "anything-select-nth-action")
      (expect "selection"
        (stub anything-get-selection => "selection")
        (stub anything-exit-minibuffer)
        (let (anything-saved-selection)
          (anything-select-nth-action 1)
          anything-saved-selection))
      (expect 'cadr
        (stub anything-get-action => '(("0" . car) ("1" . cdr) ("2" . cadr)))
        (stub anything-exit-minibuffer)
        (stub anything-get-selection => "selection")
        (let (anything-saved-action)
          (anything-select-nth-action 2)
          anything-saved-action))
      (desc "anything-funcall-foreach")
      (expect (mock (upcase "foo"))
        (stub anything-get-sources => '(((init . (lambda () (upcase "foo"))))))
        (anything-funcall-foreach 'init))
      (expect (mock (downcase "bar"))
        (stub anything-get-sources => '(((init . (lambda () (upcase "foo"))))
                                        ((init . (lambda () (downcase "bar"))))))
        (anything-funcall-foreach 'init))
      (expect (not-called anything-funcall-with-source)
        (stub anything-get-sources => '(((init . (lambda () (upcase "foo"))))))
        (anything-funcall-foreach 'not-found))
      ;; TODO anything-select-with-digit-shortcut test
      (desc "anything-get-cached-candidates")
      (expect '("cached" "version")
        (let ((anything-candidate-cache '(("test" "cached" "version"))))
          (anything-get-cached-candidates '((name . "test")
                                            (candidates "new")))))
      (expect '("new")
        (let ((anything-candidate-cache '(("other" "cached" "version"))))
          (anything-get-cached-candidates '((name . "test")
                                            (candidates "new")))))
      (expect '(("test" "new")
                ("other" "cached" "version"))
        (let ((anything-candidate-cache '(("other" "cached" "version"))))
          (anything-get-cached-candidates '((name . "test")
                                            (candidates "new")))
          anything-candidate-cache))
      (expect '(("other" "cached" "version"))
        (let ((anything-candidate-cache '(("other" "cached" "version"))))
          (anything-get-cached-candidates '((name . "test")
                                            (candidates "new")
                                            (volatile)))
          anything-candidate-cache))
      ;; TODO when candidates == process
      ;; TODO anything-output-filter
      (desc "candidate-number-limit attribute")
      (expect '("a" "b")
        (let ((anything-pattern "")
              (anything-candidate-number-limit 20))
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c")
                                      (candidate-number-limit . 2) (volatile)))))
      (expect '("a" "b")
        (let ((anything-pattern "[abc]")
              (anything-candidate-number-limit 20))
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c")
                                      (candidate-number-limit . 2) (volatile)))))
      (expect '(("TEST" ("a" "b" "c")))
        (let ((anything-candidate-number-limit 2))
          (anything-test-candidates
           '(((name . "TEST")
              (init
               . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                              (insert "a\nb\nc\nd\n"))))
              (candidates . anything-candidates-in-buffer)
              (match identity)
              (candidate-number-limit . 3)
              (volatile))))))
      (expect '(("TEST" ("a" "b" "c")))
        (let ((anything-candidate-number-limit 2))
          (anything-test-candidates
           '(((name . "TEST")
              (init
               . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                              (insert "a\nb\nc\nd\n"))))
              (candidates . anything-candidates-in-buffer)
              (match identity)
              (candidate-number-limit . 3)
              (volatile)))
           ".")))
      (desc "multiple init")
      (expect '(1 . 2)
        (let (a b)
          (anything-test-candidates
           '(((name . "test")
              (init (lambda () (setq a 1))
                    (lambda () (setq b 2))))))
          (cons a b)))
      (expect 1
        (let (a)
          (anything-test-candidates
           '(((name . "test")
              (init (lambda () (setq a 1))))))
          a))
      (desc "multiple cleanup")
      (expect '(1 . 2)
        (let (a b)
          (anything-test-candidates
           '(((name . "test")
              (cleanup (lambda () (setq a 1))
                       (lambda () (setq b 2))))))
          (cons a b)))
      (desc "anything-mklist")
      (expect '(1)
        (anything-mklist 1))
      (expect '(2)
        (anything-mklist '(2)))
      (desc "anything-before-initialize-hook")
      (expect 'called
        (let ((anything-before-initialize-hook '((lambda () (setq v 'called))))
              v)
          (anything-initialize)
          v))
      (desc "anything-after-initialize-hook")
      (expect '(b a)
        (let ((anything-before-initialize-hook
               '((lambda () (setq v '(a)))))
              (anything-after-initialize-hook
               '((lambda () (setq v (cons 'b v)))))
              v)
          (anything-initialize)
          v))
      (expect 0
        (let ((anything-after-initialize-hook
               '((lambda () (setq v (buffer-size (get-buffer anything-buffer))))))
              v)
          (anything-initialize)
          v))
      (desc "get-line attribute")
      (expect '(("TEST" ("FOO+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (get-line . (lambda (s e) (upcase (buffer-substring-no-properties s e))))))
         "oo\\+"))
      (desc "with-anything-restore-variables")
      (expect '(7 8)
        (let ((a 7) (b 8)
              (anything-restored-variables '(a b)))
          (with-anything-restore-variables
            (setq a 0 b 0))
          (list a b)))
      (desc "anything-cleanup-hook")
      (expect 'called
        (let ((anything-cleanup-hook
               '((lambda () (setq v 'called))))
              v)
          (anything-cleanup)
          v))
      (desc "with-anything-display-same-window")
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (split-window)
          
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-anything-display-same-window
              (display-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (split-window)
          
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-anything-display-same-window
              (pop-to-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (split-window)
          
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-anything-display-same-window
              (switch-to-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-anything-display-same-window
              (display-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-anything-display-same-window
              (pop-to-buffer buf)
              (eq win (get-buffer-window buf))))))
      (desc "search-from-end attribute")
      (expect '(("TEST" ("baz+" "bar+" "foo+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)))))
      (expect '(("TEST" ("baz+" "bar+" "foo+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)))
         "\\+"))
      (expect '(("TEST" ("baz+" "bar+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)
            (candidate-number-limit . 2)))))
      (expect '(("TEST" ("baz+" "bar+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)
            (candidate-number-limit . 2)))
         "\\+"))

      (desc "header-name attribute")
      (expect "original is transformed"
        (anything-test-update '(((name . "original")
                                 (candidates "1")
                                 (header-name
                                  . (lambda (name)
                                      (format "%s is transformed" name)))))
                              "")
        (with-current-buffer (anything-buffer-get)
          (buffer-string)
          (overlay-get (car (overlays-at (1+(point-min)))) 'display)))
      (desc "volatile and match attribute")
      ;; candidates function is called once per `anything-process-delayed-sources'
      (expect 1
        (let ((v 0))
          (anything-test-candidates '(((name . "test")
                                       (candidates . (lambda () (incf v) '("ok")))
                                       (volatile)
                                       (match identity identity identity)))
                                    "o")
          v))
      (desc "accept-empty attribute")
      (expect nil
        (anything-test-candidates
         '(((name . "test") (candidates "") (action . identity))))
        (anything-execute-selection-action))
      (expect ""
        (anything-test-candidates
         '(((name . "test") (candidates "") (action . identity) (accept-empty))))
        (anything-execute-selection-action))
      (desc "anything-tick-hash")
      (expect nil
        (with-current-buffer (get-buffer-create " *00create+*")
          (puthash " *00create+*/xxx" 1 anything-tick-hash)
          (kill-buffer (current-buffer)))
        (gethash " *00create+*/xxx" anything-tick-hash))
      (desc "anything-execute-action-at-once-if-once")
      (expect "HOGE"
        (let ((anything-execute-action-at-once-if-one t))
          (anything '(((name . "one test1")
                       (candidates "hoge")
                       (action . upcase))))))
      (expect "ANY"
        (let ((anything-execute-action-at-once-if-one t))
          (anything '(((name . "one test2")
                       (candidates "hoge" "any")
                       (action . upcase)))
                    "an")))
      ;; candidates > 1
      (expect (mock (read-string "word: " nil))
        (let ((anything-execute-action-at-once-if-one t))
          (anything '(((name . "one test3")
                       (candidates "hoge" "foo" "bar")
                       (action . identity)))
                    nil "word: ")))
      (desc "anything-quit-if-no-candidate")
      (expect nil
        (let ((anything-quit-if-no-candidate t))
          (anything '(((name . "zero test1") (candidates) (action . upcase))))))
      (expect 'called
        (let (v (anything-quit-if-no-candidate (lambda () (setq v 'called))))
          (anything '(((name . "zero test2") (candidates) (action . upcase))))
          v))
      (desc "real-to-display attribute")
      (expect '(("test" ("ddd")))
        (anything-test-candidates '(((name . "test")
                                     (candidates "ddd")
                                     (real-to-display . upcase)
                                     (action . identity)))))
      (expect "test\nDDD\n"
        (anything-test-update '(((name . "test")
                                 (candidates "ddd")
                                 (real-to-display . upcase)
                                 (action . identity)))
                              "")
        (with-current-buffer (anything-buffer-get) (buffer-string)))
      (desc "real-to-display and candidates-in-buffer")
      (expect '(("test" ("a" "b")))
        (anything-test-candidates
         '(((name . "test")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (erase-buffer)
                            (insert "a\nb\n"))))
            (candidates-in-buffer)
            (real-to-display . upcase)
            (action . identity)))))
      (expect "test\nA\nB\n"
        (stub read-string)
        (anything
         '(((name . "test")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (erase-buffer)
                            (insert "a\nb\n"))))
            (candidates-in-buffer)
            (real-to-display . upcase)
            (action . identity))))
        (with-current-buffer (anything-buffer-get) (buffer-string)))
      (desc "Symbols are acceptable as candidate.")
      (expect '(("test" (sym "str")))
        (anything-test-candidates
         '(((name . "test")
            (candidates sym "str")))))
      (expect '(("test" ((sym . realsym) ("str" . "realstr"))))
        (anything-test-candidates
         '(((name . "test")
            (candidates (sym . realsym) ("str" . "realstr"))))))
      (expect '(("test" (sym)))
        (anything-test-candidates
         '(((name . "test")
            (candidates sym "str")))
         "sym"))
      (expect '(("test" ("str")))
        (anything-test-candidates
         '(((name . "test")
            (candidates sym "str")))
         "str"))
      (expect '(("test" ((sym . realsym))))
        (anything-test-candidates
         '(((name . "test")
            (candidates (sym . realsym) ("str" . "realstr"))))
         "sym"))
      (expect '(("test" (("str" . "realstr"))))
        (anything-test-candidates
         '(((name . "test")
            (candidates (sym . realsym) ("str" . "realstr"))))
         "str"))
      (desc "multiple transformers")
      (expect '(("test" ("<FOO>")))
        (anything-test-candidates
         '(((name . "test")
            (candidates "foo")
            (candidate-transformer
             . (lambda (cands)
                 (anything-compose (list cands)
                                   (list (lambda (c) (mapcar 'upcase c))
                                         (lambda (c) (list (concat "<" (car c) ">")))))))))))
      (expect '("<FOO>")
        (anything-composed-funcall-with-source
         '((name . "test"))
         (list (lambda (c) (mapcar 'upcase c))
               (lambda (c) (list (concat "<" (car c) ">"))))
         '("foo"))
        )
      (expect '(("test" ("<FOO>")))
        (anything-test-candidates
         '(((name . "test")
            (candidates "foo")
            (candidate-transformer
             (lambda (c) (mapcar 'upcase c))
             (lambda (c) (list (concat "<" (car c) ">"))))))))
      (expect '(("test" ("<BAR>")))
        (anything-test-candidates
         '(((name . "test")
            (candidates "bar")
            (filtered-candidate-transformer
             (lambda (c s) (mapcar 'upcase c))
             (lambda (c s) (list (concat "<" (car c) ">"))))))))
      (expect '(("find-file" . find-file)
                ("view-file" . view-file))
        (stub zerop => nil)
        (stub anything-get-current-source
              => '((name . "test")
                   (action)
                   (action-transformer
                    . (lambda (a s)
                        (anything-compose
                         (list a s)
                         (list (lambda (a s) (push '("view-file" . view-file) a))
                               (lambda (a s) (push '("find-file" . find-file) a))))))))
        (anything-get-action))
      (expect '(("find-file" . find-file)
                ("view-file" . view-file))
        (stub zerop => nil)
        (stub anything-get-current-source
              => '((name . "test")
                   (action)
                   (action-transformer
                    (lambda (a s) (push '("view-file" . view-file) a))
                    (lambda (a s) (push '("find-file" . find-file) a)))))
        (anything-get-action))
      (desc "define-anything-type-attribute")
      (expect '((file (action . find-file)))
        (let (anything-type-attributes)
          (define-anything-type-attribute 'file '((action . find-file)))
          anything-type-attributes))
      (expect '((file (action . find-file)))
        (let ((anything-type-attributes '((file (action . view-file)))))
          (define-anything-type-attribute 'file '((action . find-file)))
          anything-type-attributes))
      (expect '((file (action . find-file))
                (buffer (action . switch-to-buffer)))
        (let (anything-type-attributes)
          (define-anything-type-attribute 'buffer '((action . switch-to-buffer)))
          (define-anything-type-attribute 'file '((action . find-file)))
          anything-type-attributes))
      )))


(provide 'anything)
;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything.el")
;;; anything.el ends here
