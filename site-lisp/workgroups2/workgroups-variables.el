;;; workgroups-variables --- Workgroups vars and consts
;;; Commentary:
;;; Code:

(defconst wg-version "1.0.3"
  "Current version of workgroups.")

;;; customization

(defgroup workgroups nil
  "Workgroups for Emacs -- Emacs session manager"
  :group 'convenience
  :version wg-version)

(defcustom workgroups-mode nil
  "Non-nil if Workgroups mode is enabled."
  :set 'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :group 'workgroups
  :type 'boolean)

(defcustom wg-first-wg-name "First workgroup"
  "Title of the first workgroup created."
  :type 'string
  :group 'workgroups)

(defcustom wg-load-last-workgroup nil
  "Load last active, not first, workgroup from all your workgroups."
  :group 'workgroups
  :type 'boolean)


;; keybinding customization

(defcustom wg-prefix-key (kbd "C-c z")
  "Workgroups' prefix key.
Setting this variable requires that `workgroups-mode' be turned
off and then on again to take effect."
  :type 'string
  :group 'workgroups)


;; hooks

(defcustom workgroups-mode-hook nil
  "Hook run when `workgroups-mode' is turned on."
  :type 'hook
  :group 'workgroups)

(defcustom workgroups-mode-exit-hook nil
  "Hook run when `workgroups-mode' is turned off."
  :type 'hook
  :group 'workgroups)

(defcustom wg-switch-to-workgroup-hook nil
  "Hook run by `wg-switch-to-workgroup'."
  :type 'hook
  :group 'workgroups)

(defcustom wg-buffer-list-finalization-hook nil
  "Functions in this hook can modify `wg-temp-buffer-list'
arbitrarily, provided its final value is still a list of the
names of live buffer.  Any final adjustments the user wishes to
make to the filtered buffer list before ido/iswitchb get ahold of
it should be made here."
  :type 'hook
  :group 'workgroups)

(defcustom wg-pre-window-configuration-change-hook nil
  "Hook run before any function that triggers
`window-configuration-change-hook'."
  :type 'hook
  :group 'workgroups)


;; save and load customization
(defcustom wg-use-default-session-file (not (daemonp))
  "Generally, non-nil means take care of saving and loading automatically,
and nil means leave it up to the user.

FIXME: docstring this"
  :type 'boolean
  :group 'workgroups)

(defcustom wg-default-session-file
  "~/.emacs_workgroups"
  "Default filename to be used to save workgroups."
  :type 'file
  :group 'workgroups)

(defcustom wg-open-this-wg nil
  "Try to open this workgroup on start. If nil - nothing happens."
  :type 'string
  :group 'workgroups)

(defcustom wg-switch-to-first-workgroup-on-find-session-file t
  "Non-nil means switch to the first workgroup in a session file
when it's found with `wg-find-session-file'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-emacs-exit-save-behavior 'save
  "Determines save behavior on Emacs exit.
Possible values:

`ask'           Ask the user whether to save if there are unsaved changes

`save'          Call `wg-save-session' when there are unsaved changes

Anything else   Exit Emacs without saving changes"
  :type 'symbol
  :group 'workgroups)

(defcustom wg-workgroups-mode-exit-save-behavior 'save
  "Determines save behavior on `workgroups-mode' exit.
Possible values:

`ask'           Ask the user whether to saveif there are unsaved changes

`save'          Call `wg-save-session' when there are unsaved changes

Anything else   Exit `workgroups-mode' without saving changes"
  :type 'symbol
  :group 'workgroups)


;; minibuffer customization

(defcustom wg-confirm-on-get-workgroup-create nil
  "Non-nil means request confirmation before creating a new
workgroup when `wg-get-workgroup-create' is called with a string
that doesn't name an existing workgroup."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-no-confirm-on-destructive-operation nil
  "Non-nil means don't request confirmation before various
destructive operations, like `wg-reset'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-minibuffer-message-timeout 0.75
  "Bound to `minibuffer-message-timeout' when messaging while the
minibuffer is active."
  :type 'float
  :group 'workgroups)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FIXME:
;;
;; Only set `wg-workgroup-base-wconfig' on `wg-write-session-file' or
;; `delete-frame' and only with the most recently changed working-wconfig.
;; Then, since it's not overwritten on every call to
;; `wg-workgroup-working-wconfig', its restoration can be retried after manually
;; recreating buffers that couldn't be restored.  So it takes over the
;; 'incorrect restoration' portion of the base wconfig's duty.  All that leaves
;; to base wconfigs is that they're a saved wconfig the user felt was important.
;; So why not allow more of of them?  A workgroup could stash an unlimited
;; number of wconfigs.
;;
;; TODO:
;;
;;   * Write new commands for restoring stashed wconfigs
;;
;;   * Add this message on improper restoration of `base-wconfig':
;;
;;       "Unable to restore 'buf1', 'buf2'... Hit C-whatever to retry after
;;        manually recreating these buffers."
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; workgroup restoration customization

;; TODO: possibly add `buffer-file-coding-system', `text-scale-mode-amount'
(defcustom wg-buffer-local-variables-alist
  `((major-mode nil wg-deserialize-buffer-major-mode)
    (mark-ring wg-serialize-buffer-mark-ring wg-deserialize-buffer-mark-ring)
    (left-fringe-width nil nil)
    (right-fringe-width nil nil)
    (fringes-outside-margins nil nil)
    (left-margin-width nil nil)
    (right-margin-width nil nil)
    (vertical-scroll-bar nil nil))
  "Alist mapping buffer-local variable symbols to serdes functions.

The `car' of each entry should be a buffer-local variable symbol.

The `cadr' of the entry should be either nil or a function of no
arguments.  If nil, the variable's value is used as-is, and
should have a readable printed representation.  If a function,
`funcall'ing it should yield a serialization of the value of the
variable.

The `caddr' of the entry should be either nil or a function of
one argument.  If nil, the serialized value from above is
assigned to the variable as-is.  It a function, `funcall'ing it
on the serialized value from above should do whatever is
necessary to properly restore the original value of the variable.
For example, in the case of `major-mode' it should funcall the
value (a major-mode function symbol) rather than just assigning
it to `major-mode'."
  :type 'alist
  :group 'workgroups)

(defcustom wg-special-buffer-serdes-functions
  '(wg-serialize-comint-buffer
    wg-serialize-speedbar-buffer)
  "List of functions providing special buffer serialization/deserialization.

Use `wg-support' macro and this variable will be filled
automatically.

An entry should be either a function symbol or a lambda, and should
accept a single Emacs buffer object as an argument.

When a buffer is to be serialized, it is passed to each of these
functions in turn until one returns non-nil, or the list ends.  A
return value of nil indicates that the function can't handle
buffers of that type.  A non-nil return value indicates that it
can.  The first non-nil return value becomes the buffer's special
serialization data.  The return value should be a cons, with a
deserialization function (a function symbol or a lambda) as the car,
and any other serialization data as the cdr.

When it comes time to deserialize the buffer, the deserialization
function (the car of the cons mentioned above) is passed the
wg-buf object, from which it should restore the buffer.  The
special serialization data itself can be accessed
with (cdr (wg-buf-special-data <wg-buf>)).  The deserialization
function must return the restored Emacs buffer object.

See the definitions of the functions in this list for examples of
how to write your own."
  :type 'alist
  :group 'workgroups)

(defcustom wg-default-buffer "*scratch*"
  "Buffer made visible a window when the window's actual buffer
can't be restored.  Also used when a blank workgroup is created."
  :type 'string
  :group 'workgroups)

(defcustom wg-nowg-string "No workgroups"
  "Display this string if there are no workgroups and
`wg-display-nowg' is t."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-nowg nil
  "Display something if there are no workgroups."
  :type 'boolean
  :group 'workgroups)

;; What to restore:

(defcustom wg-restore-remote-buffers t
  "Restore buffers that get \"t\" with `file-remote-p'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-associated-buffers nil
  "Non-nil means restore ALL buffers associated (opened in) with
the workgroup on workgroup restore.  \"nil\" means to restore
only needed buffers to show them to you."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-frame-position t
  "Non-nil means restore frame position on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-scroll-bars t
  "Non-nil means restore scroll-bar settings on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-fringes t
  "Non-nil means restore fringe settings on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-margins t
  "Non-nil means restore margin settings on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-point t
  "Non-nil means restore `point' on workgroup restore.
This is included mainly so point restoration can be suspended
during `wg-morph' -- you probably want this non-nil."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-point-max t
  "Controls point restoration when point is at `point-max'.
If `point' is at `point-max' when a wconfig is created, put
`point' back at `point-max' when the wconfig is restored, even if
`point-max' has increased in the meantime.  This is useful in,
say, irc buffers where `point-max' is constantly increasing."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-mark t
  "Non-nil means restore mark data on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-window-dedicated-p t
  "Non-nil means restore `window-dedicated-p' on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remember-frame-for-each-wg nil
  "When switching workgroups - restore frame parameters for each workgroup.

When nil - save/restore frame parameters to/from the first workgroup."
  :type 'boolean
  :group 'workgroups)


;; wconfig undo/redo customization

(defcustom wg-wconfig-undo-list-max 20
  "Number of past window configs to retain for undo."
  :type 'integer
  :group 'workgroups)


;; wconfig kill-ring customization

(defcustom wg-wconfig-kill-ring-max 20
  "Maximum length of the `wg-wconfig-kill-ring'."
  :type 'integer
  :group 'workgroups)


;; buffer-list filtration customization

(defcustom wg-buffer-list-filtration-on t
  "Non-nil means Workgroups' buffer-list filtration feature is on.
Nil means ido and iswitchb behave normally.  See
`wg-buffer-list-filter-definitions' for more info."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-buffer-list-filter-definitions
  '((all "all" wg-buffer-list-filter-all)
    (associated "associated" wg-buffer-list-filter-associated)
    (unassociated "unassociated" wg-buffer-list-filter-unassociated)
    (fallback "fallback" nil))
  "List of buffer list filter definitions.
Each entry should be a list containing an identifier symbol, a
prompt string, and a function form that's funcall'd to produce
the filtered buffer-list.

The prompt string is displayed as part of the minibuffer prompt
when its filter is active.

The function form should be either a function-symbol or a lambda, and
should take two arguments: a workgroup and a list of live Emacs
buffers.  The function should return a new list of live buffers,
typically by filtering its second argument in some way.

Default buffer-list-filters include:

`all'           All buffer names

`associated'    Only the names of those live buffers that have
                been associated with the current workgroup

`unassociated'  Only the names of those live buffers that are
                unassociated with the current workgroup

`fallback'      A special case used to fallback to the
                original (non-ido/iswitchb) Emacs command.
                `fallback' isn't actually a buffer-list-filter
                itself, but can be used in
                `wg-buffer-list-filter-order-alist' just the
                same.

A few example custom buffer-list filtration functions are
included, like `wg-buffer-list-filter-home-dir',
`wg-buffer-list-filter-irc' and `wg-buffer-list-filter-elisp'.
See their definitions for more info on how they're defined, and
the utilities they're built on.

Here's an example of how to add an `elisp' buffer-list-filter
definition to `wg-buffer-list-filter-definitions' using the
example function `wg-buffer-list-filter-elisp':

(add-to-list
 'wg-buffer-list-filter-definitions
 '(elisp \"elisp\" wg-buffer-list-filter-elisp))

After this form has been evaluated, `elisp' can be used wherever
other buffer-list-filter identifiers are used, like in
`wg-buffer-list-filter-order-alist'.

Becomes workgroup-local when set with `wg-set-workgroup-parameter'.
Becomes session-local when set with `wg-set-session-parameter'."
  :type 'list
  :group 'workgroups)

(defcustom wg-buffer-list-filter-order-alist
  '((default associated unassociated all fallback))
  "Alist defining the order in which filtered buffer-lists are presented.

The car of each entry should be the symbol of the original Emacs
command (not the ido or iswitchb remappings) -- i.e. one of
`switch-to-buffer', `switch-to-buffer-other-window',
`switch-to-buffer-other-frame', `kill-buffer', `next-buffer',
`previous-buffer', `display-buffer', `insert-buffer',
`read-buffer', or the special symbol `default', which defines the
buffer-list-filter order for all commands not present in this
alist.

The cdr of each entry should be a list of buffer-list-filter
identifiers defining the order in which filtered buffer-lists are
presented for the command.  See
`wg-buffer-list-filter-definitions'.

Becomes workgroup-local when set with `wg-set-workgroup-parameter'.
Becomes session-local when set with `wg-set-session-parameter'."
  :type 'alist
  :group 'workgroups)

(defcustom wg-center-rotate-buffer-list-display nil
  "Non-nil means rotate the buffer list display so that the
current buffer is in the center of the list.  This can make it
easier to see the where `wg-previous-buffer' will take you, but
it doesn't look right if the buffer list display is long enough
to wrap in the miniwindow."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-buffer-auto-association-on t
  "Non-nil means buffer auto-association is on.
nil means it's off.  See `wg-buffer-auto-association'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-buffer-auto-association 'weak
  "Specifies the behavior for auto-associating buffers with workgroups.

When a buffer is made visible in a window it can be automatically
associated with the current workgroup in the window's frame.
This setting determines whether and how that happens.

Allowable values:

`weak' - weakly associate the buffer with the workgroup

`strong' - strongly associate the buffer with the workgroup

A function (a function-symbol or a lambda) - `funcall' the function to
determine whether and how to associate the buffer with the
workgroup.  The function should accept two arguments -- the
buffer and the workgroup -- and should return one of the
allowable values for this variable.

`nil' or any other value - don't associate the buffer with the
workgroup.

Becomes workgroup-local when set with `wg-set-workgroup-parameter'.
Becomes session-local when set with `wg-set-session-parameter'."
  :type 'sexp
  :group 'workgroups)

(defcustom wg-dissociate-buffer-on-kill-buffer t
  "Non-nil means dissociate from the current workgroup buffers
killed with `kill-buffer'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-switch-to-buffer nil
  "Non-nil means remap `switch-to-buffer' to `wg-switch-to-buffer'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-switch-to-buffer-other-window nil
  "Non-nil means remap `switch-to-buffer-other-window' to
`wg-switch-to-buffer-other-window'.  Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-switch-to-buffer-other-frame nil
  "Non-nil means remap `switch-to-buffer-other-frame' to
`wg-switch-to-buffer-other-frame'.  Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-kill-buffer nil
  "Non-nil means remap `kill-buffer' to `wg-kill-buffer'.
Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-display-buffer nil
  "Non-nil means remap `display-buffer' to `wg-display-buffer'.
Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-insert-buffer nil
  "Non-nil means remap `insert-buffer' to `wg-insert-buffer'.
Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-next-buffer nil
  "Non-nil means remap `next-buffer' to `wg-next-buffer'.
Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-previous-buffer nil
  "Non-nil means remap `previous-buffer' to `wg-previous-buffer'.
Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-bury-buffer 'bury
  "Non-nil means remap `bury-buffer'.
`banish' means remap `bury-buffer' to `wg-banish-buffer'.
`bury' or other non-nil means remap `bury-buffer' to
`wg-bury-buffer'.  Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-ido-entry-buffer-replacement-regexp "^ .*Minibuf.*$"
  "Regexp matching the name of a buffer to replace `ido-entry-buffer'.
The regexp should match the name of a live buffer that will never
be a completion candidate under normal circumstances.  You
probably don't want to change this.  See
`wg-get-sneaky-ido-entry-buffer-replacement'."
  :type 'regexp
  :group 'workgroups)


;; mode-line customization

(defcustom wg-mode-line-display-on t
  "Toggles Workgroups' mode-line display."
  :type 'boolean
  :group 'workgroups
  :set (lambda (sym val)
         (custom-set-default sym val)
         (force-mode-line-update)))

(defcustom wg-mode-line-use-faces nil
  "Non-nil means use faces in the mode-line display.
It can be tricky to choose faces that are visible in both active
and inactive mode-lines, so this feature defaults to off."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-mode-line-disable (featurep 'powerline)
  "Do not do any modeline modifications."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-mode-line-only-name t
  "Display only workgroup name in modeline without any flags."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-mode-line-decor-left-brace "("
  "String displayed at the left of the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-right-brace ")"
  "String displayed at the right of the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-divider ":"
  "String displayed between elements of the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-strongly-associated
  #("@" 0 1 (help-echo "This buffer is strongly associated with the \
current workgroup"))
  "Indicates that a buffer is strongly associated with the current workgroup."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-weakly-associated
  #("~" 0 1 (help-echo "This buffer is weakly associated with the \
current workgroup"))
  "Indicates that a buffer is weakly associated with the current workgroup."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-unassociated
  #("-" 0 1 (help-echo "This buffer is unassociated with the \
current workgroup"))
  "Indicates that a buffer is unassociated with the current workgroup."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-window-dedicated
  #("#" 0 1 (help-echo "This window is dedicated to its buffer."))
  "Indicates that the window is dedicated to its buffer."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-window-undedicated
  #("-" 0 1 (help-echo "This window is not dedicated to its buffer."))
  "Indicates that the window is not dedicated to its buffer."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-session-modified
  #("*" 0 1 (help-echo "The session is modified"))
  "Indicates that the session is modified."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-session-unmodified
  #("-" 0 1 (help-echo "The session is unmodified"))
  "Indicates that the session is unmodified."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-workgroup-modified
  #("*" 0 1 (help-echo "The current workgroup is modified"))
  "Indicates that the current workgroup is modified."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-workgroup-unmodified
  #("-" 0 1 (help-echo "The current workgroup is unmodified"))
  "Indicates that the current workgroup is unmodified."
  :type 'string
  :group 'workgroups)


;; display customization

(defcustom wg-use-faces t
  "Non-nil means use faces in various displays."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-time-format "%H:%M:%S %A, %B %d %Y"
  "Format string for time display.  Passed to `format-time-string'."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-battery t
  "Non-nil means include `battery', when available, in the time display."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-list-display-decor-left-brace "( "
  "String displayed to the left of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-right-brace " )"
  "String displayed to the right of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-divider " | "
  "String displayed between elements of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-current-left "-<{ "
  "String displayed to the left of the current element of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-current-right " }>-"
  "String displayed to the right of the current element of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-previous-left "< "
  "String displayed to the left of the previous element of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-previous-right " >"
  "String displayed to the right of the previous element of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-associate-buffers t
  "Non-nil means when emacs chooses a buffer to display in a
workgroup, prefer buffers whose most recent appearance was in
that workgroup."
  :type 'boolean
  :group 'workgroups)

;;; vars

(defvar workgroups-mode-map nil
  "Workgroups Mode's keymap")

(defvar wg-current-session nil
  "Current session object.")

(defvar wg-workgroups-mode-minor-mode-map-entry nil
  "Workgroups' minor-mode-map entry.")

(defvar wg-wconfig-kill-ring nil
  "Ring of killed or kill-ring-saved wconfigs.")

(defvar wg-last-message nil
  "Holds the last message Workgroups sent to the echo area.")

(defvar wg-face-abbrevs nil
  "Assoc list mapping face abbreviations to face names.")

(defvar wg-buffer-uid nil
  "Symbol for the current buffer's wg-buf's uid.
Every Workgroups buffer object (wg-buf) has a uid.  When
Workgroups creates or encounters an Emacs buffer object
corresponding to a wg-buf, it tags it with the wg-buf's uid to
unambiguously pair the two.")
(make-variable-buffer-local 'wg-buffer-uid)


;; file and modified flag vars

(defvar wg-flag-modified t
  "Dynamically bound to nil around destructive operations to
temporarily disable flagging `modified'.")


;; undo vars

(defvar wg-window-configuration-changed nil
  "Flag set by `window-configuration-change-hook'.")

(defvar wg-already-updated-working-wconfig nil
  "Flag set by `wg-update-working-wconfig-hook'.")

(defvar wg-undoify-window-configuration-change t
  "Flag unset when changes to the window config shouldn't cause
workgroups' undo info to be updated.")

(defvar wg-just-exited-minibuffer nil
  "Flag set by `minibuffer-exit-hook' to exempt from
undoification those window-configuration changes caused by
exiting the minibuffer.  This is ugly, but necessary.  It may
seem like we could just null out
`wg-undoify-window-configuration-change' in
`minibuffer-exit-hook', but that also prevents undoification of
window configuration changes triggered by commands called with
`execute-extended-command' -- i.e. it's just too coarse.")


;; buffer-list-filter vars

(defvar wg-current-workgroup nil
  "Bound to the current workgroup in `wg-with-buffer-list-filters'.")

;; (defvar wg-current-buffer-command nil
;;   "Bound to the current buffer command in `wg-with-buffer-list-filters'.")

(defvar wg-current-buffer-list-filter-id nil
  "Bound to the current buffer-list-filter symbol in `wg-with-buffer-list-filters'.")

(defvar wg-previous-minibuffer-contents nil
  "Holds the previous minibuffer contents for re-insertion when
the buffer-list-filter is cycled.")

(defvar wg-ido-method-translations
  `((switch-to-buffer              . selected-window)
    (switch-to-buffer-other-window . other-window)
    (switch-to-buffer-other-frame  . other-frame)
    (kill-buffer                   . kill)
    (insert-buffer                 . insert)
    (display-buffer                . display))
  "Alist mapping buffer commands to ido buffer methods.")

(defvar wg-iswitchb-method-translations
  `((switch-to-buffer              . samewindow)
    (switch-to-buffer-other-window . otherwindow)
    (switch-to-buffer-other-frame  . otherframe)
    (kill-buffer                   . kill)
    (insert-buffer                 . insert)
    (display-buffer                . display))
  "Alist mapping buffer commands to iswitchb buffer methods.")

(defvar wg-buffer-internal-default-buffer nil
  "Bound to `wg-buffer-internal's optional DEFAULT argument for
use by buffer list filtration hooks.")

(defvar wg-temp-buffer-list nil
  "Dynamically bound to the filtered buffer list in
`wg-finalize-buffer-list'.  Functions in
`wg-buffer-list-finalization-hook' should modify this variable.")


;; wconfig restoration

(defvar wg-window-min-width 2
  "Bound to `window-min-width' when restoring wtrees. ")

(defvar wg-window-min-height 1
  "Bound to `window-min-height' when restoring wtrees.")

(defvar wg-window-min-pad 2
  "Added to `wg-window-min-foo' to produce the actual minimum window size.")

(defvar wg-actual-min-width (+ wg-window-min-width wg-window-min-pad)
  "Actual minimum window width when creating windows.")

(defvar wg-actual-min-height (+ wg-window-min-height wg-window-min-pad)
  "Actual minimum window height when creating windows.")

(defvar wg-min-edges `(0 0 ,wg-actual-min-width ,wg-actual-min-height)
  "Smallest allowable edge list of windows created by Workgroups.")

(defvar wg-null-edges '(0 0 0 0)
  "Null edge list.")

(defvar wg-window-tree-selected-window nil
  "Used during wconfig restoration to hold the selected window.")

(defvar wg-update-current-workgroup-working-wconfig-on-select-frame t
  "Non-nil means update `selected-frame's current workgroup's
working wconfig before `select-frame' selects a new frame.
let-bind this to nil around forms in which you don't want this to
happen.")


(defvar wg-buffer-workgroup nil
  "Buffer-local variable associating each buffer with the
  workgroup in which it most recently appeared.")
(make-variable-buffer-local 'wg-buffer-workgroup)

(defvar wg-deactivation-list nil
  "A stack of workgroups that are currently being switched away from.
Used to avoid associating the old workgroup's buffers with the
new workgroup during a switch.")

(defvar wg-incorrectly-restored-bufs nil
  "FIXME: docstring this.")
;; TODO: check it on switching WG

(defvar wg-record-incorrectly-restored-bufs nil
  "FIXME: docstring this.")

;;; faces

(defmacro wg-defface (face key spec doc &rest args)
  "`defface' wrapper adding a lookup key used by `wg-fontify'."
  (declare (indent 2))
  `(progn
     (cl-pushnew (cons ,key ',face) wg-face-abbrevs :test #'equal)
     (defface ,face ,spec ,doc ,@args)))

(wg-defface wg-current-workgroup-face :cur
  '((t :inherit font-lock-constant-face :bold nil))
  "Face used for current elements in list displays."
  :group 'workgroups)

(wg-defface wg-previous-workgroup-face :prev
  '((t :inherit font-lock-keyword-face :bold nil))
  "Face used for the name of the previous workgroup in the list display."
  :group 'workgroups)

(wg-defface wg-other-workgroup-face :other
  '((t :inherit font-lock-string-face :bold nil))
  "Face used for the names of other workgroups in the list display."
  :group 'workgroups)

(wg-defface wg-command-face :cmd
  '((t :inherit font-lock-function-name-face :bold nil))
  "Face used for command/operation strings."
  :group 'workgroups)

(wg-defface wg-divider-face :div
  '((t :inherit font-lock-builtin-face :bold nil))
  "Face used for dividers."
  :group 'workgroups)

(wg-defface wg-brace-face :brace
  '((t :inherit font-lock-builtin-face :bold nil))
  "Face used for left and right braces."
  :group 'workgroups)

(wg-defface wg-message-face :msg
  '((t :inherit font-lock-string-face :bold nil))
  "Face used for messages."
  :group 'workgroups)

(wg-defface wg-mode-line-face :mode
  '((t :inherit font-lock-doc-face :bold nil))
  "Face used for workgroup position and name in the mode-line display."
  :group 'workgroups)

(wg-defface wg-filename-face :file
  '((t :inherit font-lock-keyword-face :bold nil))
  "Face used for filenames."
  :group 'workgroups)

(provide 'workgroups-variables)
;;; workgroups-variables.el ends here
