;;; evil-vars.el --- Settings and variables

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.0-dev

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;; Hooks

(defvar evil-after-load-hook nil
  "Functions to be run when loading of evil is finished.
This hook can be used the execute some initialization routines
when evil is completely loaded.")

;;; Initialization

(defvar evil-pending-custom-initialize nil
  "A list of pending initializations for custom variables.
Each element is a triple (FUNC VAR VALUE). When evil is
completely loaded then the functions (funcall FUNC VAR VALUE) is
called for each element. FUNC should be a function suitable for
the :initialize property of `defcustom'.")

(defun evil-custom-initialize-pending-reset (var value)
  "Add a pending customization with `custom-initialize-reset'."
  (push (list 'custom-initialize-reset var value)
        evil-pending-custom-initialize))

(defun evil-run-pending-custom-initialize ()
  "Executes the pending initializations.
See `evil-pending-custom-initialize'."
  (dolist (init evil-pending-custom-initialize)
    (apply (car init) (cdr init)))
  (remove-hook 'evil-after-load-hook 'evil-run-pending-custom-initialize))
(add-hook 'evil-after-load-hook 'evil-run-pending-custom-initialize)

;;; Setters

(defun evil-set-toggle-key (key)
  "Set `evil-toggle-key' to KEY.
KEY must be readable by `read-kbd-macro'."
  (let ((old-key (read-kbd-macro
                  (if (boundp 'evil-toggle-key)
                      evil-toggle-key
                    "C-z")))
        (key (read-kbd-macro key)))
    (with-no-warnings
      (dolist (pair '((evil-motion-state-map evil-emacs-state)
                      (evil-insert-state-map evil-emacs-state)
                      (evil-emacs-state-map evil-exit-emacs-state)))
        (when (boundp (car pair))
          (let ((map (symbol-value (car pair)))
                (fun (cadr pair)))
            (when (keymapp map)
              (define-key map key fun)
              (define-key map old-key nil))))))))

(defun evil-set-custom-state-maps (var pending-var key make newlist)
  "Changes the list of special keymaps.
VAR         is the variable containing the list of keymaps.
PENDING-VAR is the variable containing the list of the currently pending
            keymaps.
KEY         the special symbol to be stored in the keymaps.
MAKE        the creation function of the special keymaps.
NEWLIST     the list of new special keymaps."
  (set-default pending-var newlist)
  (when (default-boundp var)
    (dolist (map (default-value var))
      (when (and (boundp (car map))
                 (keymapp (default-value (car map))))
        (define-key (default-value (car map)) (vector key) nil))))
  (set-default var newlist)
  (evil-update-pending-maps))

(defun evil-update-pending-maps (&optional file)
  "Tries to set pending special keymaps.
This function should be called from an `after-load-functions'
hook."
  (let ((maps '((evil-make-overriding-map . evil-pending-overriding-maps)
                (evil-make-intercept-map . evil-pending-intercept-maps))))
    (while maps
      (let* ((map (pop maps))
             (make (car map))
             (pending-var (cdr map))
             (pending (symbol-value pending-var))
             newlist)
        (while pending
          (let* ((map (pop pending))
                 (kmap (and (boundp (car map))
                            (keymapp (symbol-value (car map)))
                            (symbol-value (car map))))
                 (state (cdr map)))
            (if kmap
                (funcall make kmap state)
              (push map newlist))))
        (set-default pending-var newlist)))))

(defun evil-set-visual-newline-commands (var value)
  "Set the value of `evil-visual-newline-commands'.
Setting this variable changes the properties of the appropriate
commands."
  (with-no-warnings
    (when (default-boundp var)
      (dolist (cmd (default-value var))
        (evil-set-command-property cmd :exclude-newline nil)))
    (set-default var value)
    (dolist (cmd (default-value var))
      (evil-set-command-property cmd :exclude-newline t))))

(defun evil-set-custom-motions (var values)
  "Sets the list of motion commands."
  (with-no-warnings
    (when (default-boundp var)
      (dolist (motion (default-value var))
        (evil-add-command-properties motion :keep-visual nil :repeat nil)))
    (set-default var values)
    (mapc #'evil-declare-motion (default-value var))))

;;; Customization group

(defgroup evil nil
  "Extensible vi layer."
  :group 'emulations
  :prefix 'evil-)

(defcustom evil-auto-indent t
  "Whether to auto-indent when entering Insert state."
  :type  'boolean
  :group 'evil)
(make-variable-buffer-local 'evil-auto-indent)

(defcustom evil-shift-width 4
  "The offset used by \\<evil-normal-state-map>\\[evil-shift-right] \
and \\[evil-shift-left]."
  :type 'integer
  :group 'evil)
(make-variable-buffer-local 'evil-shift-width)

(defcustom evil-shift-round t
  "Whether \\<evil-normal-state-map>\\[evil-shift-right] \
and \\[evil-shift-left] round to the nearest multiple \
of `evil-shift-width'."
  :type 'boolean
  :group 'evil)
(make-variable-buffer-local 'evil-shift-round)

(defcustom evil-default-cursor
  (list (or (frame-parameter nil 'cursor-color) "black") t)
  "The default cursor.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above."
  :type '(set symbol (cons symbol symbol) string function)
  :group 'evil)

(defcustom evil-repeat-move-cursor t
  "Whether \"\\<evil-normal-state-map>\\[evil-repeat]\" \
moves the cursor."
  :type 'boolean
  :group 'evil)

(defcustom evil-cross-lines nil
  "Whether motions may cross newlines."
  :type 'boolean
  :group 'evil)

(defcustom evil-backspace-join-lines t
  "Whether backward delete in insert state may join lines."
  :type 'boolean
  :group 'evil)

(defcustom evil-move-cursor-back t
  "Whether the cursor is moved backwards when exiting Insert state."
  :type 'boolean
  :group 'evil)

(defcustom evil-repeat-find-to-skip-next t
  "Whether a repeat of t or T should skip an adjacent character."
  :type 'boolean
  :group 'evil)

(defcustom evil-kbd-macro-suppress-motion-error nil
  "Whether left/right motions signal errors during keyboard-macro definition.
If this variable is set to non-nil, then the function
`evil-forward-char' and `evil-backward-char' do not signal
`end-of-line' or `beginning-of-line' errors when a keyboard macro
is being defined and/or it is being executed. This may be desired
because such an error would cause the macro definition/execution
being terminated."
  :type '(radio (const :tag "No" :value nil)
                (const :tag "Record" :value record)
                (const :tag "Replay" :value replay)
                (const :tag "Both" :value t))
  :group 'evil)

(defcustom evil-track-eol t
  "If non-nil line moves after a call to `evil-end-of-line' stay at eol.
This is analogous to `track-eol' but deals with the end-of-line
interpretation of evil."
  :type 'boolean
  :group 'evil)

(defcustom evil-mode-line-format 'before
  "The position of the mode line tag.
Either a symbol or a cons-cell. If it is a symbol it should be
one of 'before, 'after or 'nil. 'before mean the the tag is
placed before the mode-list, 'after means it is placed after the
mode-list, and 'nil means no mode line tag. If it is a cons cell
it should have the form (WHERE . WHICH) where WHERE is either
'before or 'after and WHICH is a symbol in
`mode-line-format'. The tag is then placed right before or after
that symbol."
  :type '(radio :value 'before
                (const before)
                (const after)
                (cons :tag "Next to symbol"
                      (choice :value after
                              (const before)
                              (const after))
                      symbol))
  :group 'evil)

(defcustom evil-mouse-word 'evil-move-word
  "The (movement) function to be used for double click selection.
The double-click starts visual state in a special word selection
mode. This function is used to determine the words to be
selected. Possible values are 'evil-move-word or
'evil-move-WORD."
  :type 'symbol
  :group 'evil)

(defcustom evil-bigword "^ \t\r\n"
  "The characters to be considered as a big word.
This should be a regexp set without the enclosing []."
  :type 'string
  :group 'evil)
(make-variable-buffer-local 'evil-bigword)

(defcustom evil-want-fine-undo nil
  "Whether actions like \"cw\" are undone in several steps."
  :type 'boolean
  :group 'evil)

(defcustom evil-regexp-search t
  "Whether to use regular expressions for searching."
  :type  'boolean
  :group 'evil)

(defcustom evil-search-wrap t
  "Whether search wraps around."
  :type  'boolean
  :group 'evil)

(defcustom evil-flash-delay 2
  "Time in seconds to flash search matches."
  :type  'number
  :group 'evil)

(defcustom evil-fold-level 0
  "Default fold level."
  :type  'integer
  :group 'evil)

(defcustom evil-esc-delay 0.01
  "Time in seconds to wait for another key after ESC."
  :type 'number
  :group 'evil)

(defvar evil-esc-mode nil
  "Non-nil if `evil-esc-mode' is enabled.")

(defvar evil-esc-map nil
  "Original ESC prefix map in `input-decode-map'.
Used by `evil-esc-mode'.")

(defvar evil-inhibit-esc nil
  "If non-nil, the \\e event will never be translated to 'escape.")

(defcustom evil-intercept-esc 'always
  "Whether evil should intercept the ESC key.
In terminal, a plain ESC key and a meta-key-sequence both
generate the same event. In order to distinguish both evil
modifies `input-decode-map'. This is necessary in terminal but
not in X mode. However, the terminal ESC is equivalent to C-[, so
if you want to use C-[ instead of ESC in X, then Evil must
intercept the ESC event in X, too. This variable determines when
Evil should intercept the event."
  :type '(radio (const :tag "Never" :value nil)
                (const :tag "In terminal only" :value t)
                (const :tag "Always" :value always))
  :group 'evil)

(defcustom evil-show-paren-range 0
  "The minimal distance between point and a parenthesis
which causes the parenthesis to be highlighted."
  :type 'integer
  :group 'evil)

(defcustom evil-ex-hl-update-delay 0.02
  "Time in seconds of idle before updating search highlighting.
Setting this to a period shorter than that of keyboard's repeat
rate allows highlights to update while scrolling."
  :type 'number
  :group 'evil)

(defcustom evil-highlight-closing-paren-at-point-states
  '(not emacs insert replace)
  "The states in which the closing parenthesis at point should be highlighted.
All states listed here highlight the closing parenthesis at
point (which is Vim default behavior), all others highlight the
parenthesis before point (which is Emacs default behavior). If
this list contains the symbol 'not then its meaning is inverted,
i.e., all states listed here highlight the closing parenthesis
before point."
  :type '(repeat symbol)
  :group 'evil)

(defcustom evil-want-C-i-jump t
  "Whether \"C-i\" jumps forward like in Vim."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-C-u-scroll nil
  "Whether \"C-u\" scrolls like in Vim."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-C-w-delete t
  "Whether \"C-w\" deletes a word in Insert state."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-C-w-in-emacs-state nil
  "Whether \"C-w\" prefixes windows commands in Emacs state."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-change-word-to-end t
  "Whether \"cw\" behaves like \"ce\"."
  :type 'boolean
  :group 'evil)

(defcustom evil-echo-state t
  "Whether to signal the current state in the echo area."
  :type 'boolean
  :group 'evil)

(defcustom evil-complete-all-buffers t
  "Whether completion looks for matches in all buffers."
  :type 'boolean
  :group 'evil)

(defcustom evil-complete-next-func
  #'(lambda (arg)
      (require 'dabbrev)
      (let ((dabbrev-search-these-buffers-only
             (unless evil-complete-all-buffers
               (list (current-buffer))))
            dabbrev-case-distinction)
        (condition-case nil
            (if (eq last-command this-command)
                (dabbrev-expand nil)
              (dabbrev-expand (- (abs (or arg 1)))))
          (error (dabbrev-expand nil)))))
  "Completion function used by \
\\<evil-insert-state-map>\\[evil-complete-next]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-previous-func
  #'(lambda (arg)
      (require 'dabbrev)
      (let ((dabbrev-search-these-buffers-only
             (unless evil-complete-all-buffers
               (list (current-buffer))))
            dabbrev-case-distinction)
        (dabbrev-expand arg)))
  "Completion function used by \
\\<evil-insert-state-map>\\[evil-complete-previous]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-next-minibuffer-func 'minibuffer-complete
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-next]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-previous-minibuffer-func 'minibuffer-complete
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-previous]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-next-line-func
  #'(lambda (arg)
      (let ((hippie-expand-try-functions-list
             '(try-expand-line
               try-expand-line-all-buffers)))
        (hippie-expand arg)))
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-next-line]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-previous-line-func
  evil-complete-next-line-func
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-previous-line]."
  :type 'function
  :group 'evil)

(defcustom evil-lookup-func #'woman
  "Lookup function used by \
\"\\<evil-motion-state-map>\\[evil-lookup]\"."
  :type 'function
  :group 'evil)

(defcustom evil-toggle-key "C-z"
  "The key used to change to and from Emacs state.
Must be readable by `read-kbd-macro'. For example: \"C-z\"."
  :type 'string
  :group 'evil
  :set #'(lambda (sym value)
           (evil-set-toggle-key value)
           (set-default sym value)))

(defcustom evil-default-state 'normal
  "The default state.
This is the state a mode comes up in when it is not listed
in `evil-emacs-state-modes', `evil-insert-state-modes' or
`evil-motion-state-modes'. The value may be one of `normal',
`insert', `visual', `replace', `operator', `motion' and
`emacs'."
  :type  'symbol
  :group 'evil)

(defcustom evil-buffer-regexps
  '(("^ \\*load\\*" . nil))
  "Regular expression determining the initial state for a buffer.
Entries have the form (REGEXP . STATE), where REGEXP is a regular
expression matching the buffer's name and STATE is one of `normal',
`insert', `visual', `replace', `operator', `motion', `emacs' and nil.
If STATE is nil, Evil is disabled in the buffer."
  :type '(alist :key-type string :value-type symbol)
  :group 'evil)

(defcustom evil-emacs-state-modes
  '(archive-mode
    bbdb-mode
    bookmark-bmenu-mode
    bookmark-edit-annotation-mode
    browse-kill-ring-mode
    bzr-annotate-mode
    calc-mode
    cfw:calendar-mode
    completion-list-mode
    Custom-mode
    debugger-mode
    delicious-search-mode
    desktop-menu-blist-mode
    desktop-menu-mode
    doc-view-mode
    dvc-bookmarks-mode
    dvc-diff-mode
    dvc-info-buffer-mode
    dvc-log-buffer-mode
    dvc-revlist-mode
    dvc-revlog-mode
    dvc-status-mode
    dvc-tips-mode
    ediff-mode
    ediff-meta-mode
    efs-mode
    Electric-buffer-menu-mode
    emms-browser-mode
    emms-mark-mode
    emms-metaplaylist-mode
    emms-playlist-mode
    etags-select-mode
    fj-mode
    gc-issues-mode
    gdb-breakpoints-mode
    gdb-disassembly-mode
    gdb-frames-mode
    gdb-locals-mode
    gdb-memory-mode
    gdb-registers-mode
    gdb-threads-mode
    gist-list-mode
    gnus-article-mode
    gnus-browse-mode
    gnus-group-mode
    gnus-server-mode
    gnus-summary-mode
    google-maps-static-mode
    ibuffer-mode
    jde-javadoc-checker-report-mode
    magit-commit-mode
    magit-diff-mode
    magit-key-mode
    magit-log-mode
    magit-mode
    magit-reflog-mode
    magit-show-branches-mode
    magit-branch-manager-mode ;; New name for magit-show-branches-mode
    magit-stash-mode
    magit-status-mode
    magit-wazzup-mode
    mh-folder-mode
    monky-mode
    mu4e-main-mode
    mu4e-headers-mode
    mu4e-view-mode
    notmuch-hello-mode
    notmuch-search-mode
    notmuch-show-mode
    occur-mode
    org-agenda-mode
    package-menu-mode
    proced-mode
    rcirc-mode
    rebase-mode
    recentf-dialog-mode
    reftex-select-bib-mode
    reftex-select-label-mode
    reftex-toc-mode
    sldb-mode
    slime-inspector-mode
    slime-thread-control-mode
    slime-xref-mode
    sr-buttons-mode
    sr-mode
    sr-tree-mode
    sr-virtual-mode
    tar-mode
    tetris-mode
    tla-annotate-mode
    tla-archive-list-mode
    tla-bconfig-mode
    tla-bookmarks-mode
    tla-branch-list-mode
    tla-browse-mode
    tla-category-list-mode
    tla-changelog-mode
    tla-follow-symlinks-mode
    tla-inventory-file-mode
    tla-inventory-mode
    tla-lint-mode
    tla-logs-mode
    tla-revision-list-mode
    tla-revlog-mode
    tla-tree-lint-mode
    tla-version-list-mode
    twittering-mode
    urlview-mode
    vc-annotate-mode
    vc-dir-mode
    vc-git-log-view-mode
    vc-svn-log-view-mode
    vm-mode
    vm-summary-mode
    w3m-mode
    wab-compilation-mode
    xgit-annotate-mode
    xgit-changelog-mode
    xgit-diff-mode
    xgit-revlog-mode
    xhg-annotate-mode
    xhg-log-mode
    xhg-mode
    xhg-mq-mode
    xhg-mq-sub-mode
    xhg-status-extra-mode)
  "Modes that should come up in Emacs state."
  :type  '(repeat symbol)
  :group 'evil)

(defcustom evil-insert-state-modes
  '(comint-mode
    erc-mode
    eshell-mode
    geiser-repl-mode
    gud-mode
    inferior-apl-mode
    inferior-caml-mode
    inferior-emacs-lisp-mode
    inferior-j-mode
    inferior-python-mode
    inferior-scheme-mode
    inferior-sml-mode
    internal-ange-ftp-mode
    prolog-inferior-mode
    reb-mode
    shell-mode
    slime-repl-mode
    term-mode
    wdired-mode)
  "Modes that should come up in Insert state."
  :type  '(repeat symbol)
  :group 'evil)

(defcustom evil-motion-state-modes
  '(apropos-mode
    Buffer-menu-mode
    calendar-mode
    color-theme-mode
    command-history-mode
    compilation-mode
    dictionary-mode
    ert-results-mode
    help-mode
    Info-mode
    Man-mode
    speedbar-mode
    undo-tree-visualizer-mode
    view-mode
    woman-mode)
  "Modes that should come up in Motion state."
  :type  '(repeat symbol)
  :group 'evil)

(defvar evil-pending-overriding-maps nil
  "An alist of pending overriding maps.")

(defvar evil-pending-intercept-maps nil
  "An alist of pending intercept maps.")

(defcustom evil-overriding-maps
  '((Buffer-menu-mode-map . nil)
    (color-theme-mode-map . nil)
    (comint-mode-map . nil)
    (compilation-mode-map . nil)
    (dictionary-mode-map . nil)
    (ert-results-mode-map . motion)
    (Info-mode-map . motion)
    (speedbar-key-map . nil)
    (speedbar-file-key-map . nil)
    (speedbar-buffers-key-map . nil))
  "Keymaps that should override Evil maps.
Entries have the form (MAP-VAR . STATE), where MAP-VAR is
a keymap variable and STATE is the state whose bindings
should be overridden. If STATE is nil, all states are
overridden."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'evil
  :set #'(lambda (var values)
           (evil-set-custom-state-maps 'evil-overriding-maps
                                       'evil-pending-overriding-maps
                                       'override-state
                                       'evil-make-overriding-map
                                       values))
  :initialize 'evil-custom-initialize-pending-reset)

(add-hook 'after-load-functions #'evil-update-pending-maps)

(defcustom evil-intercept-maps
  '((edebug-mode-map . nil))
  "Keymaps that should intercept Evil maps.
Entries have the form (MAP-VAR . STATE), where MAP-VAR is
a keymap variable and STATE is the state whose bindings
should be intercepted. If STATE is nil, all states are
intercepted."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'evil
  :set #'(lambda (var values)
           (evil-set-custom-state-maps 'evil-intercept-maps
                                       'evil-pending-intercept-maps
                                       'intercept-state
                                       'evil-make-intercept-map
                                       values))
  :initialize 'evil-custom-initialize-pending-reset)

(defcustom evil-motions
  '(back-to-indentation
    backward-char
    backward-list
    backward-paragraph
    backward-sentence
    backward-sexp
    backward-up-list
    backward-word
    beginning-of-buffer
    beginning-of-defun
    beginning-of-line
    beginning-of-visual-line
    c-beginning-of-defun
    c-end-of-defun
    diff-file-next
    diff-file-prev
    diff-hunk-next
    diff-hunk-prev
    down-list
    end-of-buffer
    end-of-defun
    end-of-line
    end-of-visual-line
    exchange-point-and-mark
    forward-char
    forward-list
    forward-paragraph
    forward-sentence
    forward-sexp
    forward-word
    goto-last-change
    ibuffer-backward-line
    ibuffer-forward-line
    isearch-abort
    isearch-cancel
    isearch-complete
    isearch-del-char
    isearch-delete-char
    isearch-edit-string
    isearch-exit
    isearch-highlight-regexp
    isearch-occur
    isearch-other-control-char
    isearch-other-meta-char
    isearch-printing-char
    isearch-query-replace
    isearch-query-replace-regexp
    isearch-quote-char
    isearch-repeat-backward
    isearch-repeat-forward
    isearch-ring-advance
    isearch-ring-retreat
    isearch-toggle-case-fold
    isearch-toggle-input-method
    isearch-toggle-regexp
    isearch-toggle-specified-input-method
    isearch-toggle-word
    isearch-yank-char
    isearch-yank-kill
    isearch-yank-line
    isearch-yank-word-or-char
    keyboard-quit
    left-char
    left-word
    mouse-drag-region
    mouse-save-then-kill
    mouse-set-point
    mouse-set-region
    mwheel-scroll
    move-beginning-of-line
    move-end-of-line
    next-error
    next-line
    paredit-backward
    paredit-backward-down
    paredit-backward-up
    paredit-forward
    paredit-forward-down
    paredit-forward-up
    pop-global-mark
    pop-tag-mark
    pop-to-mark-command
    previous-error
    previous-line
    right-char
    right-word
    scroll-down
    scroll-up
    sgml-skip-tag-backward
    sgml-skip-tag-forward
    up-list)
  "Non-Evil commands to initialize to motions."
  :type  '(repeat symbol)
  :group 'evil
  :set 'evil-set-custom-motions
  :initialize 'evil-custom-initialize-pending-reset)

(defcustom evil-visual-newline-commands
  '(LaTeX-section
    TeX-font)
  "Commands excluding the trailing newline of a Visual Line selection.
These commands work better without this newline."
  :type  '(repeat symbol)
  :group 'evil
  :set 'evil-set-visual-newline-commands
  :initialize 'evil-custom-initialize-pending-reset)

(defcustom evil-want-visual-char-semi-exclusive nil
  "Visual character selection to beginning/end of line is exclusive.
If non nil then an inclusive visual character selection which
ends at the beginning or end of a line is turned into an
exclusive selection. Thus if the selected (inclusive) range ends
at the beginning of a line it is changed to not include the first
character of that line, and if the selected range ends at the end
of a line it is changed to not include the newline character of
that line."
  :type 'boolean
  :group 'evil)

(defgroup evil-cjk nil
  "CJK support"
  :prefix "evil-cjk-"
  :group 'evil)

(defcustom evil-cjk-emacs-word-boundary nil
  "Determine word boundary exactly the same way as Emacs does."
  :type 'boolean
  :group 'evil-cjk)

(defcustom evil-cjk-word-separating-categories
  '(;; Kanji
    (?C . ?H) (?C . ?K) (?C . ?k) (?C . ?A) (?C . ?G)
    ;; Hiragana
    (?H . ?C) (?H . ?K) (?H . ?k) (?H . ?A) (?H . ?G)
    ;; Katakana
    (?K . ?C) (?K . ?H) (?K . ?k) (?K . ?A) (?K . ?G)
    ;; half-width Katakana
    (?k . ?C) (?k . ?H) (?k . ?K) ; (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?C) (?A . ?H) (?A . ?K) ; (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?C) (?G . ?H) (?G . ?K) ; (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `evil-cjk-word-boundary-p'. See the documentation of
`word-separating-categories'. Use `describe-categories' to see
the list of categories."
  :type '((character . character))
  :group 'evil-cjk)

(defcustom evil-cjk-word-combining-categories
  '(;; default value in word-combining-categories
    (nil . ?^) (?^ . nil)
    ;; Roman
    (?r . ?k) (?r . ?A) (?r . ?G)
    ;; half-width Katakana
    (?k . ?r) (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?r) (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?r) (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `evil-cjk-word-boundary-p'. See the documentation of
`word-combining-categories'. Use `describe-categories' to see the
list of categories."
  :type '((character . character))
  :group 'evil-cjk)

(defcustom evil-ex-complete-emacs-commands 'in-turn
  "TAB-completion for Emacs commands in ex command line.
This variable determines when Emacs commands are considered for
completion, always, never, or only if no Evil ex command is
available for completion."
  :group 'evil
  :type '(radio (const :tag "Only if no ex-command." :value in-turn)
                (const :tag "Never" :value nil)
                (const :tag "Always" :value t)))

(defface evil-ex-commands '(( nil
                              :underline t
                              :slant italic))
  "Face for the evil command in completion in ex mode."
  :group 'evil)

(defface evil-ex-info '(( ((supports :slant))
                          :slant italic
                          :foreground "red"))
  "Face for the info message in ex mode."
  :group 'evil)

(defcustom evil-ex-visual-char-range nil
  "Type of default ex range in visual char state.
If non-nil the default range when starting an ex command from
character visual state is `<,`> otherwise it is '<,'>. In the
first case the ex command will be passed a region covering only
the visual selection. In the second case the passed region will
be extended to contain full lines."
  :group 'evil
  :type 'boolean)

;; Searching
(defcustom evil-magic t
  "Meaning which characters in a pattern are magic.
The meaning of those values is the same as in Vim. Note that it
only has influence if the evil search module is chosen in
`evil-search-module'."
  :group 'evil
  :type '(radio (const :tag "Very magic." :value very-magic)
                (const :tag "Magic" :value t)
                (const :tag "Nomagic" :value nil)
                (const :tag "Very nomagic" :value very-nomagic)))

(defcustom evil-ex-search-vim-style-regexp nil
  "If non-nil Vim-style backslash codes are supported in search patterns.
See `evil-transform-vim-style-regexp' for the supported backslash
codes.  Note that this only affects the search command if
`evil-search-module' is set to 'evil. The isearch module always
uses plain Emacs regular expressions."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-interactive-search-highlight 'all-windows
  "Determine in which windows the interactive highlighting should be shown."
  :type '(radio (const :tag "All windows." all-windows)
                (const :tag "Selected window." selected-window)
                (const :tag "Disable highlighting." nil))
  :group 'evil)

(defcustom evil-ex-search-case 'smart
  "The case behaviour of the search command.
Smart case means that the pattern is case sensitive if and only
if it contains an upper case letter, otherwise it is case
insensitive."
  :type '(radio (const :tag "Case sensitive." sensitive)
                (const :tag "Case insensitive." insensitive)
                (const :tag "Smart case." smart))
  :group 'evil)

(defcustom evil-ex-substitute-case nil
  "The case behaviour of the search command.
Smart case means that the pattern is case sensitive if and only
if it contains an upper case letter, otherwise it is case
insensitive. If nil then the setting of `evil-ex-search-case' is
used."
  :type '(radio (const :tag "Same as interactive search." nil)
                (const :tag "Case sensitive." sensitive)
                (const :tag "Case insensitive." insensitive)
                (const :tag "Smart case." smart))
  :group 'evil)

(defcustom evil-ex-search-interactive t
  "If t search is interactive."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-search-highlight-all t
  "If t and interactive search is enabled, all matches are
highlighted."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-substitute-highlight-all t
  "If t all matches for the substitute pattern are highlighted."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-substitute-interactive-replace t
  "If t and substitute patterns are highlighted,
the replacement is shown interactively."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-substitute-global nil
  "If non-nil substitute patterns a global by default.
Usually (if this variable is nil) a substitution works only on
the first match of a pattern in a line unless the 'g' flag is
given, in which case the substitution happens on all matches in a
line. If this option is non-nil, this behaviour is reversed: the
substitution works on all matches unless the 'g' pattern is
specified, then is works only on the first match."
  :type  'boolean
  :group 'evil)

(defface evil-ex-search '((t :inherit isearch))
  "Face for interactive search."
  :group 'evil)

(defface evil-ex-lazy-highlight '((t :inherit lazy-highlight))
  "Face for highlighting all matches in interactive search."
  :group 'evil)

(defface evil-ex-substitute-matches '((t :inherit lazy-highlight))
  "Face for interactive substitute matches."
  :group 'evil)

(defface evil-ex-substitute-replacement '((((supports :underline))
                                           :underline t
                                           :foreground "red"))
  "Face for interactive replacement text."
  :group 'evil)

;;; Variables

(defmacro evil-define-local-var (symbol &optional initvalue docstring)
  "Define SYMBOL as permanent buffer local variable, and return SYMBOL.
The parameters are the same as for `defvar', but the variable
SYMBOL is made permanent buffer local."
  (declare (indent defun)
           (debug (symbolp &optional form stringp)))
  `(progn
     (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     (put ',symbol 'permanent-local t)))

(evil-define-local-var evil-state nil
  "The current Evil state.
To change the state, use `evil-change-state'
or call the state function (e.g., `evil-normal-state').")

;; these may be used inside `evil-define-state'
(evil-define-local-var evil-next-state nil
  "The Evil state being switched to.")

(evil-define-local-var evil-previous-state-alist nil
  "For Each evil state the Evil state being switched from.")

(evil-define-local-var evil-previous-state nil
  "The Evil state being switched from.")

(defvar evil-execute-in-emacs-state-buffer nil
  "The buffer of the latest `evil-execute-in-emacs-state'.
When this command is being executed the current buffer is stored
in this variable. This is necessary in case the Emacs-command to
be called changes the current buffer.")

(evil-define-local-var evil-mode-line-tag nil
  "Mode-Line indicator for the current state.")
(put 'evil-mode-line-tag 'risky-local-variable t)

(defvar evil-global-keymaps-alist nil
  "Association list of keymap variables.
Entries have the form (MODE . KEYMAP), where KEYMAP
is the variable containing the keymap for MODE.")

(defvar evil-local-keymaps-alist nil
  "Association list of keymap variables that must be
reinitialized in each buffer. Entries have the form
\(MODE . KEYMAP), where KEYMAP is the variable containing
the keymap for MODE.")

(defvar evil-state-properties nil
  "Specifications made by `evil-define-state'.
Entries have the form (STATE . PLIST), where PLIST is a property
list specifying various aspects of the state. To access a property,
use `evil-state-property'.")

(evil-define-local-var evil-mode-map-alist nil
  "Association list of keymaps to use for Evil modes.
Elements have the form (MODE . KEYMAP), with the first keymaps
having higher priority.")

(defvar evil-command-properties nil
  "Specifications made by `evil-define-command'.")

(defvar evil-transient-vars '(cua-mode transient-mark-mode select-active-regions)
  "List of variables pertaining to Transient Mark mode.")

(defvar evil-transient-vals nil
  "Association list of old values for Transient Mark mode variables.
Entries have the form (VARIABLE VALUE LOCAL), where LOCAL is
whether the variable was previously buffer-local.")

(evil-define-local-var evil-no-display nil
  "If non-nil, various Evil displays are inhibited.
Use the macro `evil-without-display' to set this variable.")

(defvar evil-type-properties nil
  "Specifications made by `evil-define-type'.
Entries have the form (TYPE . PLIST), where PLIST is a property
list specifying functions for handling the type: expanding it,
describing it, etc.")

(defvar evil-interactive-alist nil
  "Association list of Evil-specific interactive codes.")

(evil-define-local-var evil-motion-marker nil
  "Marker for storing the starting position of a motion.")

(evil-define-local-var evil-this-type nil
  "Current motion type.")

(evil-define-local-var evil-this-register nil
  "Current register.")

(evil-define-local-var evil-this-macro nil
  "Current macro register.")

(evil-define-local-var evil-this-operator nil
  "Current operator.")

(evil-define-local-var evil-this-motion nil
  "Current motion.")

(evil-define-local-var evil-this-motion-count nil
  "Current motion count.")

(defvar evil-inhibit-operator nil
  "Inhibit current operator.
If an operator calls a motion and the motion sets this variable
to t, the operator code is not executed.")

(defvar evil-inhibit-operator-value nil
  "This variable is used to transfer the value
of `evil-inhibit-operator' from one local scope to another.")

;; used by `evil-define-operator'
(defvar evil-operator-range-beginning nil
  "Beginning of `evil-operator-range'.")

(defvar evil-operator-range-end nil
  "End of `evil-operator-range'.")

(defvar evil-operator-range-type nil
  "Type of `evil-operator-range'.")

(defvar evil-operator-range-motion nil
  "Motion of `evil-operator-range'.")

(defvar evil-restriction-stack nil
  "List of previous restrictions.
Using `evil-with-restriction' stores the previous values of
`point-min' and `point-max' as a pair in this list.")

(evil-define-local-var evil-markers-alist
  '((?\( . evil-backward-sentence)
    (?\) . evil-forward-sentence)
    (?{ . evil-backward-paragraph)
    (?} . evil-forward-paragraph)
    (?' . evil-jump-backward)
    (?` . evil-jump-backward)
    (?< . evil-visual-beginning)
    (?> . evil-visual-goto-end)
    (?. . (lambda ()
            (let (last-command)
              (goto-last-change nil)))))
  "Association list for markers.
Entries have the form (CHAR . DATA), where CHAR is the marker's
name and DATA is either a marker object as returned by `make-marker',
a variable, a movement function, or a cons cell (STRING NUMBER),
where STRING is a file path and NUMBER is a buffer position.
The global value of this variable holds markers available from
every buffer, while the buffer-local value holds markers available
only in the current buffer.")

(evil-define-local-var evil-jump-list nil
  "Jump list.")

(defconst evil-suppress-map (make-keymap)
  "Full keymap disabling default bindings to `self-insert-command'.")
(suppress-keymap evil-suppress-map t)

(defvar evil-read-key-map (make-sparse-keymap)
  "Keymap active during `evil-read-key'.
This keymap can be used to bind some commands during the
execution of `evil-read-key' which is usually used to read a
character argument for some commands, e.g. `evil-replace'.")

;; TODO: customize size of ring
(defvar evil-repeat-ring (make-ring 10)
  "A ring of repeat-informations to repeat the last command.")

(defvar evil-repeat-types
  '((t . evil-repeat-keystrokes)
    (change . evil-repeat-changes)
    (motion . evil-repeat-motion)
    (insert-at-point . evil-repeat-insert-at-point)
    (ignore . nil))
  "An alist of defined repeat-types.")

(defvar evil-recording-repeat nil
  "Whether we are recording a repeat.")

(defvar evil-recording-current-command nil
  "Whether we are recording the current command for repeat.")

(defvar evil-repeat-changes nil
  "Accumulated buffer changes for changed-based commands.")

(defvar evil-repeat-info nil
  "Information accumulated during current repeat.")

(defvar evil-repeat-buffer nil
  "The buffer in which the repeat started.
If the buffer is changed, the repeat is cancelled.")

(defvar evil-repeat-pos nil
  "The position of point at the beginning of an change-tracking
  editing command.")

(defvar evil-repeat-keys nil
  "The keys that invoked the current command.")

(defvar evil-last-repeat nil
  "Information about the latest repeat command.
This is a list of three elements (POINT COUNT UNDO-POINTER),
where POINT is the position of point before the latest repeat,
COUNT the count-argument of the latest repeat command and
UNDO-POINTER the head of the undo-list before the last command
has been repeated.")

(defvar evil-repeat-count nil
  "The explicit count when repeating a command.")

(evil-define-local-var evil-insert-count nil
  "The explicit count passed to an command starting Insert state.")

(evil-define-local-var evil-insert-vcount nil
  "The information about the number of following lines the
insertion should be repeated. This is list (LINE COLUMN COUNT)
where LINE is the line-number where the original insertion
started and COLUMN is either a number of function determining the
column where the repeated insertions should take place. COUNT is
number of repeats (including the original insertion).")

(defvar evil-insert-skip-empty-lines nil
  "Non-nil of the current insertion should not take place on
  lines at which the insertion point is behind the end of the
  line.")

(evil-define-local-var evil-insert-lines nil
  "Non-nil if the current insertion command is a line-insertion
command o or O.")

(evil-define-local-var evil-insert-repeat-info nil
  "Repeat information accumulated during an insertion.")

(evil-define-local-var evil-replace-alist nil
  "Association list of characters overwritten in Replace state.
The format is (POS . CHAR).")

(evil-define-local-var evil-echo-area-message nil
  "Previous value of `current-message'.")

(defvar evil-write-echo-area nil
  "If set to t inside `evil-save-echo-area', then the echo area
is not restored.")

(defvar evil-last-find nil
  "A pair (FUNCTION . CHAR) describing the lastest character
  search command.")

(defvar evil-last-paste nil
  "Information about the latest paste.
This should be a list (CMD COUNT POINT BEG END FIRSTVISUAL) where
CMD is the last paste-command (`evil-paste-before',
`evil-paste-after' or `evil-visual-paste'), COUNT is the repeat
count of the paste, POINT is the position of point before the
paste, BEG end END are the region of the inserted
text. FIRSTVISUAL is t if and only if the previous command was
the first visual paste (i.e. before any paste-pop).")

(evil-define-local-var evil-last-undo-entry nil
  "Information about the latest undo entry in the buffer.
This should be a pair (OBJ . CONS) where OBJ is the entry as an
object, and CONS is a copy of the entry.")

(evil-define-local-var evil-current-insertion nil
  "Information about the latest insertion in insert state.
This should be a pair (BEG . END) that describes the
buffer-region of the newly inserted text.")

(defvar evil-last-insertion nil
  "The last piece of inserted text.")

(defvar evil-last-small-deletion nil
  "The last piece of deleted text.
The text should be less than a line.")

(defvar evil-paste-count nil
  "The count argument of the current paste command.")

(defvar evil-temporary-undo nil
  "When undo is disabled in current buffer.
Certain commands depending on undo use this variable
instead of `buffer-undo-list'.")

(evil-define-local-var evil-undo-list-pointer nil
  "Everything up to this mark is united in the undo-list.")

(defvar evil-in-single-undo nil
  "Set to non-nil if the current undo steps are connected.")

(defvar evil-flash-timer nil
  "Timer for flashing search results.")

(defvar evil-search-prompt nil
  "String to use for search prompt.")

(defvar evil-inner-text-objects-map (make-sparse-keymap)
  "Keymap for inner text objects.")

(defvar evil-outer-text-objects-map (make-sparse-keymap)
  "Keymap for outer text objects.")

(defvar evil-window-map (make-sparse-keymap)
  "Keymap for window-related commands.")

(evil-define-local-var evil-input-method nil
  "Input method used in Insert state and Emacs state.")

;;; Visual state

(evil-define-local-var evil-visual-beginning nil
  "The beginning of the Visual selection, a marker.")

(evil-define-local-var evil-visual-end nil
  "The end of the Visual selection, a marker.")

(evil-define-local-var evil-visual-point nil
  "The position of point in Visual state, a marker.")

(evil-define-local-var evil-visual-mark nil
  "The position of mark in Visual state, a marker.")

(evil-define-local-var evil-visual-previous-mark nil
  "The position of mark before Visual state, a marker.")

(evil-define-local-var evil-visual-selection nil
  "The kind of Visual selection.
This is a selection as defined by `evil-define-visual-selection'.")

;; we could infer the direction by comparing `evil-visual-mark'
;; and `evil-visual-point', but destructive operations may
;; displace the markers
(evil-define-local-var evil-visual-direction 0
  "Whether point follows mark in Visual state.
Negative if point precedes mark, otherwise positive.
See also the function `evil-visual-direction'.")

(evil-define-local-var evil-visual-properties nil
  "Property list of miscellaneous Visual properties.")

(evil-define-local-var evil-visual-region-expanded nil
  "Whether the region matches the Visual selection.
That is, whether the positions of point and mark have been
expanded to coincide with the selection's boundaries.
This makes the selection available to functions acting
on Emacs' region.")

(evil-define-local-var evil-visual-overlay nil
  "Overlay for highlighting the Visual selection.
Not used for blockwise selections, in which case
see `evil-visual-block-overlays'.")

(evil-define-local-var evil-visual-block-overlays nil
  "Overlays for Visual Block selection, one for each line.
They are reused to minimize flicker.")

(defvar evil-visual-alist nil
  "Association list of Visual selection functions.
Elements have the form (NAME . FUNCTION).")

(evil-define-local-var evil-visual-x-select-timer nil
  "Timer for updating the X selection in visual state.")

(defvar evil-visual-x-select-timeout 0.1
  "Time in seconds for the update of the X selection.")

;;; Ex

(defvar evil-ex-map (make-sparse-keymap)
  "Keymap for Ex.
Key sequences bound in this map are immediately executed.")

(defvar evil-ex-completion-map (make-sparse-keymap)
  "Completion keymap for Ex.")

(defvar evil-ex-shell-argument-initialized nil
  "This variable is set to t if shell command completion has been initialized.
See `evil-ex-init-shell-argument-completion'.")

(defvar evil-ex-commands nil
  "Association list of command bindings and functions.")

(defvar evil-ex-history nil
  "History of Ex commands.")

(defvar evil-ex-current-buffer nil
  "The buffer from which Ex was started.")

(defvar evil-ex-expression nil
  "The evaluation tree.")

(defvar evil-ex-tree nil
  "The syntax tree.")

(defvar evil-ex-command nil
  "The current Ex command.")

(defvar evil-ex-previous-command nil
  "The previously executed Ex command.")

(defvar evil-ex-point nil
  "The position of `point' when the ex command has been called.")

(defvar evil-ex-range nil
  "The current range of the Ex command.")

(defvar evil-ex-bang nil
  "The \"!\" argument of the current Ex command.")

(defvar evil-ex-argument nil
  "The current argument of the Ex command.")

(defvar evil-ex-argument-handler nil
  "The argument handler for the current Ex command.")

(defvar evil-ex-argument-types nil
  "Association list of argument handlers.")

(defvar evil-previous-shell-command nil
  "The last shell command.")

;; Searching
(defvar evil-ex-search-history nil
  "The history for the search command.")

(defvar evil-ex-search-direction nil
  "The direction of the current search, either 'forward or 'backward.")

(defvar evil-ex-search-count nil
  "The count if the current search.")

(defvar evil-ex-search-start-point nil
  "The point where the search started.")

(defvar evil-ex-search-overlay nil
  "The overlay for the current search result.")

(defvar evil-ex-search-pattern nil
  "The last search pattern.")

(defvar evil-ex-search-offset nil
  "The last search offset.")

(defvar evil-ex-search-match-beg nil
  "The beginning position of the last match.")

(defvar evil-ex-search-match-end nil
  "The end position of the last match.")

(defvar evil-ex-substitute-pattern nil
  "The last substitute pattern.")

(defvar evil-ex-substitute-replacement nil
  "The last substitute replacement.")

(defvar evil-ex-substitute-flags nil
  "The last substitute flags.")

(defvar evil-ex-substitute-current-replacement nil
  "The actual replacement.")

(defvar evil-ex-last-was-search nil
  "Non-nil if the previous was a search.
Otherwise the previous command is assumed as substitute.")

;; The lazy-highlighting framework.
(evil-define-local-var evil-ex-active-highlights-alist nil
  "An alist of currently active highlights.")

(evil-define-local-var evil-ex-hl-update-timer nil
  "Time used for updating highlights.")

(defvar evil-ex-search-keymap (make-sparse-keymap)
  "Keymap used in ex-search-mode.")
(set-keymap-parent evil-ex-search-keymap minibuffer-local-map)

(defconst evil-version "1.0-dev"
  "The current version of Evil")

(defun evil-version ()
  (interactive)
  (message "Evil version %s" evil-version))

(provide 'evil-vars)

;;; evil-vars.el ends here
