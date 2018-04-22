;;; magit-popup.el --- Define prefix-infix-suffix command combos  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2017  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; This library was inspired by and replaces library `magit-key-mode',
;; which was written by Phil Jackson <phil@shellarchive.co.uk> and is
;; distributed under the GNU General Public License version 3 or later.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Package-Requires: ((emacs "24.4") (async "20170823") (dash "20170810"))
;; Keywords: bindings
;; Homepage: https://github.com/magit/magit

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This library implements a generic interface for toggling switches
;; and setting options and then invoking an Emacs command which does
;; something with these arguments.  The prototypical use is for the
;; command to call an external process, passing on the arguments as
;; command line arguments.  But this is only one of many possible
;; uses (though the one this library is optimized for).

;; With the Emacs concept of "prefix arguments" in mind this could be
;; described as "infix arguments with feedback in a buffer".

;; Commands that set the prefix argument for the subsequent command do
;; not limit what that next command could be.  But entering a command
;; console popup does limit the selection to the commands defined for
;; that popup, and so we use the term "infix" instead of "prefix".

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'dash)
(require 'format-spec)

(and (require 'async-bytecomp nil t)
     (cl-intersection '(all magit)
                      (bound-and-true-p async-bytecomp-allowed-packages))
     (fboundp 'async-bytecomp-package-mode)
     (async-bytecomp-package-mode 1))

(declare-function info 'info)
(declare-function Man-find-section 'man)
(declare-function Man-next-section 'man)

;; For the `:variable' event type.
(declare-function magit-git-string 'magit-git)
(declare-function magit-refresh 'magit-mode)
(declare-function magit-get 'magit-git)
(declare-function magit-set 'magit-git)

;; For branch actions.
(declare-function magit-branch-set-face 'magit-git)

;;; Settings
;;;; Custom Groups

(defgroup magit-popup nil
  "Infix arguments with a popup as feedback."
  :link '(info-link "(magit-popup)")
  :group 'bindings)

(defgroup magit-popup-faces nil
  "Faces used by Magit-Popup."
  :group 'magit-popup)

;;;; Custom Options

(defcustom magit-popup-display-buffer-action '((display-buffer-below-selected))
  "The action used to display a popup buffer.

Popup buffers are displayed using `display-buffer' with the value
of this option as ACTION argument.  You can also set this to nil
and instead add an entry to `display-buffer-alist'."
  :package-version '(magit-popup . "2.4.0")
  :group 'magit-popup
  :type 'sexp)

(defcustom magit-popup-manpage-package
  (if (memq system-type '(windows-nt ms-dos)) 'woman 'man)
  "The package used to display manpages.
One of `man' or `woman'."
  :group 'magit-popup
  :type '(choice (const man) (const woman)))

(defcustom magit-popup-show-help-echo t
  "Show usage information in the echo area."
  :group 'magit-popup
  :type 'boolean)

(defcustom magit-popup-show-common-commands nil
  "Whether to initially show section with commands common to all popups.
This section can also be toggled temporarily using \
\\<magit-popup-mode-map>\\[magit-popup-toggle-show-common-commands]."
  :package-version '(magit-popup . "2.9.0")
  :group 'magit-popup
  :type 'boolean)

(defcustom magit-popup-use-prefix-argument 'default
  "Control how prefix arguments affect infix argument popups.

This option controls the effect that the use of a prefix argument
before entering a popup has.

`default'  With a prefix argument directly invoke the popup's
           default action (an Emacs command), instead of bringing
           up the popup.

`popup'    With a prefix argument bring up the popup, otherwise
           directly invoke the popup's default action.

`nil'      Ignore prefix arguments."
  :group 'magit-popup
  :type '(choice
          (const :tag "Call default action instead of showing popup" default)
          (const :tag "Show popup instead of calling default action" popup)
          (const :tag "Ignore prefix argument" nil)))

;;;; Custom Faces

(defface magit-popup-heading
  '((t :inherit font-lock-keyword-face))
  "Face for key mode header lines."
  :group 'magit-popup-faces)

(defface magit-popup-key
  '((t :inherit font-lock-builtin-face))
  "Face for key mode buttons."
  :group 'magit-popup-faces)

(defface magit-popup-argument
  '((t :inherit font-lock-warning-face))
  "Face used to display enabled arguments in popups."
  :group 'magit-popup-faces)

(defface magit-popup-disabled-argument
  '((t :inherit shadow))
  "Face used to display disabled arguments in popups."
  :group 'magit-popup-faces)

(defface magit-popup-option-value
  '((t :inherit font-lock-string-face))
  "Face used to display option values in popups."
  :group 'magit-popup-faces)

;;;; Keymap

(defvar magit-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'magit-invoke-popup-action)
    (define-key map (kbd "- <t>")               'magit-invoke-popup-switch)
    (define-key map (kbd "= <t>")               'magit-invoke-popup-option)
    (define-key map (kbd "C-g")     'magit-popup-quit)
    (define-key map (kbd "?")       'magit-popup-help)
    (define-key map (kbd "C-h k")   'magit-popup-help)
    (define-key map (kbd "C-h i")   'magit-popup-info)
    (define-key map (kbd "C-t")     'magit-popup-toggle-show-common-commands)
    (define-key map (kbd "C-c C-c") 'magit-popup-set-default-arguments)
    (define-key map (kbd "C-x C-s") 'magit-popup-save-default-arguments)
    (cond ((featurep 'jkl)
           (define-key map (kbd "C-p") 'universal-argument)
           (define-key map [return]    'push-button)
           (define-key map (kbd "C-i") 'backward-button)
           (define-key map (kbd "C-k") 'forward-button))
          (t
           (define-key map (kbd "C-m") 'push-button)
           (define-key map (kbd "DEL") 'backward-button)
           (define-key map (kbd "C-p") 'backward-button)
           (define-key map (kbd "C-i") 'forward-button)
           (define-key map (kbd "C-n") 'forward-button)))
    map)
  "Keymap for `magit-popup-mode'.

\\<magit-popup-mode-map>\
This keymap contains bindings common to all popups.  A section
listing these commands can be shown or hidden using \
\\[magit-popup-toggle-show-common-commands].

The prefix used to toggle any switch can be changed by binding
another key to `magit-invoke-popup-switch'.  Likewise binding
another key to `magit-invoke-popup-option' changes the prefixed
used to set any option.  The two prefixes have to be different.
If you change these bindings, you should also change the `prefix'
property of the button types `magit-popup-switch-button' and
`magit-popup-option-button'.

If you change any other binding, then you might have to also edit
`magit-popup-common-commands' for things to align correctly in
the section listing these commands.

Never bind an alphabetic character in this keymap or you might
make it impossible to invoke certain actions.")

(defvar magit-popup-common-commands
  '(("Set defaults"          magit-popup-set-default-arguments)
    ("View popup manual"     magit-popup-info)
    ("Toggle this section"   magit-popup-toggle-show-common-commands)
    ("Save defaults"         magit-popup-save-default-arguments)
    ("    Popup help prefix" magit-popup-help)
    ("Abort"                 magit-popup-quit)))

;;;; Buttons

(define-button-type 'magit-popup-button
  'face nil
  'action (lambda (button)
            (funcall (button-get button 'function)
                     (button-get button 'event))))

(define-button-type 'magit-popup-switch-button
  'supertype 'magit-popup-button
  'function  'magit-invoke-popup-switch
  'property  :switches
  'heading   "Switches\n"
  'formatter 'magit-popup-format-argument-button
  'format    " %k %d (%a)"
  'prefix    ?-
  'maxcols   1)

(define-button-type 'magit-popup-option-button
  'supertype 'magit-popup-button
  'function  'magit-invoke-popup-option
  'property  :options
  'heading   "Options\n"
  'formatter 'magit-popup-format-argument-button
  'format    " %k %d (%a%v)"
  'prefix    ?=
  'maxcols   1)

(define-button-type 'magit-popup-variable-button
  'supertype 'magit-popup-button
  'function  'magit-invoke-popup-action
  'property  :variables
  'heading   "Variables\n"
  'formatter 'magit-popup-format-variable-button
  'format    " %k %d"
  'prefix    nil
  'maxcols   1)

(define-button-type 'magit-popup-action-button
  'supertype 'magit-popup-button
  'function  'magit-invoke-popup-action
  'property  :actions
  'heading   "Actions\n"
  'formatter 'magit-popup-format-action-button
  'format    " %k %d"
  'prefix    nil
  'maxcols   :max-action-columns)

(define-button-type 'magit-popup-command-button
  'supertype 'magit-popup-action-button
  'formatter 'magit-popup-format-command-button
  'action    (lambda (button)
               (let ((command (button-get button 'function)))
                 (unless (eq command 'push-button)
                   (call-interactively command)))))

(define-button-type 'magit-popup-internal-command-button
  'supertype 'magit-popup-command-button
  'heading   "Common Commands\n"
  'maxcols   3)

;;; Events

(defvar-local magit-this-popup nil
  "The popup which is currently active.
This is intended for internal use only.
Don't confuse this with `magit-current-popup'.")

(defvar-local magit-this-popup-events nil
  "The events known to the active popup.
This is intended for internal use only.
Don't confuse this with `magit-current-popup-args'.")

(defvar-local magit-previous-popup nil)

(defun magit-popup-get (prop)
  "While a popup is active, get the value of PROP."
  (if (memq prop '(:switches :options :variables :actions))
      (plist-get magit-this-popup-events prop)
    (plist-get (symbol-value magit-this-popup) prop)))

(defun magit-popup-put (prop val)
  "While a popup is active, set the value of PROP to VAL."
  (if (memq prop '(:switches :options :variables :actions))
      (setq magit-this-popup-events
            (plist-put magit-this-popup-events prop val))
    (error "Property %s isn't supported" prop)))

(defvar magit-current-popup nil
  "The popup from which this editing command was invoked.

Use this inside the `interactive' form of a popup aware command
to determine whether it was invoked from a popup and if so from
which popup.  If the current command was invoked without the use
of a popup, then this is nil.")

(defvar magit-current-popup-action nil
  "The popup action now being executed.")

(defvar magit-current-popup-args nil
  "The value of the popup arguments for this editing command.

If the current command was invoked from a popup, then this is
a list of strings of all the set switches and options.  This
includes arguments which are set by default not only those
explicitly set during this invocation.

When the value is nil, then that can be because no argument is
set, or because the current command wasn't invoked from a popup;
consult `magit-current-popup' to tell the difference.

Generally it is better to use `NAME-arguments', which is created
by `magit-define-popup', instead of this variable or the function
by the same name, because `NAME-argument' uses the default value
for the arguments when the editing command is invoked directly
instead of from a popup.  When the command is bound in several
popups that might not be feasible though.")

(defun magit-current-popup-args (&rest filter)
  "Return the value of the popup arguments for this editing command.

The value is the same as that of the variable by the same name
\(which see), except that FILTER is applied.  FILTER is a list
of regexps; only arguments that match one of them are returned.
The first element of FILTER may also be `:not' in which case
only arguments that don't match any of the regexps are returned,
or `:only' which doesn't change the behaviour."
  (let ((-compare-fn (lambda (a b) (magit-popup-arg-match b a))))
    (-filter (if (eq (car filter) :not)
                 (lambda (arg) (not (-contains-p (cdr filter) arg)))
               (when (eq (car filter) :only)
                 (pop filter))
               (lambda (arg) (-contains-p filter arg)))
             magit-current-popup-args)))

(defun magit-popup-arg-match (pattern string)
  (if (or (string-match-p "=$" pattern)
          (string-match-p "^-[A-Z]$" pattern))
      (string-match (format "^%s\\(.*\\)$" pattern) string)
    (string-equal string pattern)))

(cl-defstruct magit-popup-event key dsc arg fun use val)

(defun magit-popup-event-keydsc (ev)
  (let ((key (magit-popup-event-key ev)))
    (key-description (if (vectorp key) key (vector key)))))

(defun magit-popup-lookup (event type)
  (--first (equal (magit-popup-event-key it) event)
           (-filter 'magit-popup-event-p (magit-popup-get type))))

(defun magit-popup-get-args ()
  (--mapcat (when (and (magit-popup-event-p it)
                       (magit-popup-event-use it))
              (list (format "%s%s"
                            (magit-popup-event-arg it)
                            (or (magit-popup-event-val it) ""))))
            (append (magit-popup-get :switches)
                    (magit-popup-get :options))))

(defmacro magit-popup-convert-events (def form)
  (declare (indent 1) (debug (form form)))
  `(--map (if (or (null it) (stringp it) (functionp it)) it ,form) ,def))

(defun magit-popup-convert-switches (val def)
  (magit-popup-convert-events def
    (let ((a (nth 2 it)))
      (make-magit-popup-event
       :key (car it) :dsc (cadr it) :arg a
       :use (and (member a val) t)))))

(defun magit-popup-convert-options (val def)
  (magit-popup-convert-events def
    (let* ((a (nth 2 it))
           (r (format "^%s\\(.*\\)" a))
           (v (--first (string-match r it) val)))
      (make-magit-popup-event
       :key (car it)  :dsc (cadr it) :arg a
       :use (and v t) :val (and v (match-string 1 v))
       :fun (or (nth 3 it) 'read-from-minibuffer)))))

(defun magit-popup-convert-variables (_val def)
  (magit-popup-convert-events def
    (make-magit-popup-event
     :key (car it) :dsc (cadr it) :fun (nth 2 it) :arg (nth 3 it))))

(defun magit-popup-convert-actions (_val def)
  (magit-popup-convert-events def
    (make-magit-popup-event
     :key (car it) :dsc (cadr it) :fun (nth 2 it))))

;;; Define

(defmacro magit-define-popup (name doc &rest args)
  "Define a popup command named NAME.

NAME should begin with the package prefix and by convention end
with `-popup'.  That name is used for the actual command as well
as for a variable used internally.  DOC is used as the doc-string
of that command.

Also define an option and a function named `SHORTNAME-arguments',
where SHORTNAME is NAME with the trailing `-popup' removed.  The
name of this option and this function can be overwritten using
the optional argument OPTION, but that is rarely advisable. As a
special case if OPTION is specified but nil, do not define this
option and this function at all.

The option `SHORTNAME-arguments' holds the default value for the
popup arguments.  It can be customized from within the popup or
using the Custom interface.

The function `SHORTNAME-arguments' is a wrapper around the
variable `magit-current-popup-args', both of which are intended
to be used inside the `interactive' form of commands commonly
invoked from the popup `NAME'.  When such a command is invoked
from that popup, then the function `SHORTNAME-arguments' returns
the value of the variable `magit-current-popup-args'; however
when the command is invoked directly, then it returns the default
value of the variable `SHORTNAME-arguments'.

Optional argument GROUP specifies the Custom group into which the
option is placed.  If omitted, then the option is placed into some
group the same way it is done when directly using `defcustom' and
omitting the group, except when NAME begins with \"magit-\", in
which case the group `magit-git-arguments' is used.

Optional argument MODE is deprecated, instead use the keyword
arguments `:setup-function' and/or `:refresh-function'.  If MODE
is non-nil, then it specifies the mode used by the popup buffer,
instead of the default, which is `magit-popup-mode'.

The remaining arguments should have the form

    [KEYWORD VALUE]...

The following keywords are meaningful (and by convention are
usually specified in that order):

`:actions'
  The actions which can be invoked from the popup.  VALUE is a
  list whose members have the form (KEY DESC COMMAND), see
  `magit-define-popup-action' for details.

  Actions are regular Emacs commands, which usually have an
  `interactive' form setup to consume the values of the popup
  `:switches' and `:options' when invoked from the corresponding
  popup, else when invoked as the default action or directly
  without using the popup, the default value of the variable
  `SHORTNAME-arguments'.  This is usually done by calling the
  function `SHORTNAME-arguments'.

  Members of VALUE may also be strings, assuming the first member
  is also a string.  Instead of just one action section with the
  heading \"Actions\", multiple sections are then inserted into
  the popup buffer, using these strings as headings.

  Members of VALUE may also be nil.  This should only be used
  together with `:max-action-columns' and allows having gaps in
  the action grid, which can help arranging actions sensibly.

`:default-action'
  The default action of the popup which is used directly instead
  of displaying the popup buffer, when the popup is invoked with
  a prefix argument.  Also see `magit-popup-use-prefix-argument'
  and `:use-prefix', which can be used to inverse the meaning of
  the prefix argument.

`:use-prefix'
  Controls when to display the popup buffer and when to invoke
  the default action (if any) directly.  This overrides the
  global default set using `magit-popup-use-prefix-argument'.
  The value, if specified, should be one of `default' or `popup',
  or a function that is called with no arguments and returns one
  of these symbols.

`:max-action-columns'
  The maximum number of actions to display on a single line, a
  number or a function that returns a number and takes the name
  of the section currently being inserted as argument.  If there
  isn't enough room to display as many columns as specified here,
  then fewer are used.

`:switches'
  The popup arguments which can be toggled on and off.  VALUE
  is a list whose members have the form (KEY DESC SWITCH), see
  `magit-define-popup-switch' for details.

`:options'
  The popup arguments which take a value, as in \"--opt=OPTVAL\".
  VALUE is a list whose members have the form (KEY DESC OPTION
  READER), see `magit-define-popup-option' for details.

`:default-arguments'
  The default arguments, a list of switches (which are then
  enabled by default) and options with there default values, as
  in \"--OPT=OPTVAL\".

`:variables'

  Git variables which can be set from the popup.  VALUE is a list
  whose members have the form (KEY DESC COMMAND FORMATTER), see
  `magit-define-popup-variable' for details.

`:sequence-predicate'
  When this function returns non-nil, then the popup uses
  `:sequence-actions' instead of `:actions', and does not show
  the `:switches' and `:options'.

`:sequence-actions'
  The actions which can be invoked from the popup, when
  `:sequence-predicate' returns non-nil.

`:setup-function'
  When this function is specified, then it is used instead of
  `magit-popup-default-setup'.

`:refresh-function'
  When this function is specified, then it is used instead of
  calling `magit-popup-insert-section' three times with symbols
  `magit-popup-switch-button', `magit-popup-option-button', and
  finally `magit-popup-action-button' as argument.

`:man-page'
  The name of the manpage to be displayed when the user requests
  help for a switch or argument.

\(fn NAME DOC [GROUP [MODE [OPTION]]] :KEYWORD VALUE...)"
  (declare (indent defun) (doc-string 2))
  (let* ((str  (symbol-name name))
         (grp  (if (keywordp (car args))
                   (and (string-prefix-p "magit-" str) ''magit-git-arguments)
                 (pop args)))
         (mode (and (not (keywordp (car args))) (pop args)))
         (opt  (if (keywordp (car args))
                   (intern (concat (if (string-suffix-p "-popup" str)
                                       (substring str 0 -6)
                                     str)
                                   "-arguments"))
                 (eval (pop args)))))
    `(progn
       (defun ,name (&optional arg) ,doc
         (interactive "P")
         (magit-invoke-popup ',name ,mode arg))
       (defvar ,name
         (list :variable ',opt ,@args))
       (magit-define-popup-keys-deferred ',name)
       ,@(when opt
           `((defcustom ,opt (plist-get ,name :default-arguments)
               ""
               ,@(and grp (list :group grp))
               :type '(repeat (string :tag "Argument")))
             (defun ,opt ()
               (if (eq magit-current-popup ',name)
                   magit-current-popup-args
                 ,opt))
             (put ',opt 'definition-name ',name))))))

(defun magit-define-popup-switch (popup key desc switch
                                        &optional enable at prepend)
  "In POPUP, define KEY as SWITCH.

POPUP is a popup command defined using `magit-define-popup'.
SWITCH is a string representing an argument that takes no value.
KEY is a character representing the second event in the sequence
of keystrokes used to toggle the argument.  (The first event, the
prefix, is shared among all switches, defaults to -, and can be
changed in `magit-popup-mode-keymap').

DESC is a string describing the purpose of the argument, it is
displayed in the popup.

If optional ENABLE is non-nil, then the switch is on by default.

SWITCH is inserted after all other switches already defined for
POPUP, unless optional PREPEND is non-nil, in which case it is
placed first.  If optional AT is non-nil, then it should be the
KEY of another switch already defined for POPUP, the argument
is then placed before or after AT, depending on PREPEND."
  (declare (indent defun))
  (magit-define-popup-key popup :switches key
    (list desc switch enable) at prepend))

(defun magit-define-popup-option (popup key desc option
                                        &optional reader value at prepend)
  "In POPUP, define KEY as OPTION.

POPUP is a popup command defined using `magit-define-popup'.
OPTION is a string representing an argument that takes a value.
KEY is a character representing the second event in the sequence
of keystrokes used to set the argument's value.  (The first
event, the prefix, is shared among all options, defaults to =,
and can be changed in `magit-popup-mode-keymap').

DESC is a string describing the purpose of the argument, it is
displayed in the popup.

If optional VALUE is non-nil then the option is on by default,
and VALUE is its default value.

READER is used to read a value from the user when the option is
invoked and does not currently have a value.  It should take one
argument and use it as the prompt.  If this is nil, then
`read-from-minibuffer' is used.

OPTION is inserted after all other options already defined for
POPUP, unless optional PREPEND is non-nil, in which case it is
placed first.  If optional AT is non-nil, then it should be the
KEY of another option already defined for POPUP, the argument
is then placed before or after AT, depending on PREPEND."
  (declare (indent defun))
  (magit-define-popup-key popup :options key
    (list desc option reader value) at prepend))

(defun magit-define-popup-variable (popup key desc command formatter
                                          &optional at prepend)
  "In POPUP, define KEY as COMMAND.

POPUP is a popup command defined using `magit-define-popup'.
COMMAND is a command which calls `magit-popup-set-variable'.
FORMATTER is a function which calls `magit-popup-format-variable'.
These two functions have to be called with the same arguments.

KEY is a character representing the event used interactively call
the COMMAND.

DESC is the variable or a representation thereof.  It's not
actually used for anything.

COMMAND is inserted after all other commands already defined for
POPUP, unless optional PREPEND is non-nil, in which case it is
placed first.  If optional AT is non-nil, then it should be the
KEY of another command already defined for POPUP, the command
is then placed before or after AT, depending on PREPEND."
  (declare (indent defun))
  (magit-define-popup-key popup :variables key
    (list desc command formatter) at prepend))

(defun magit-define-popup-action (popup key desc command
                                        &optional at prepend)
  "In POPUP, define KEY as COMMAND.

POPUP is a popup command defined using `magit-define-popup'.
COMMAND can be any command but should usually consume the popup
arguments in its `interactive' form.
KEY is a character representing the event used invoke the action,
i.e. to interactively call the COMMAND.

DESC is a string describing the purpose of the action, it is
displayed in the popup.

COMMAND is inserted after all other commands already defined for
POPUP, unless optional PREPEND is non-nil, in which case it is
placed first.  If optional AT is non-nil, then it should be the
KEY of another command already defined for POPUP, the command
is then placed before or after AT, depending on PREPEND."
  (declare (indent defun))
  (magit-define-popup-key popup :actions key
    (list desc command) at prepend))

(defun magit-define-popup-sequence-action
    (popup key desc command &optional at prepend)
  "Like `magit-define-popup-action' but for `:sequence-action'."
  (declare (indent defun))
  (magit-define-popup-key popup :sequence-actions key
    (list desc command) at prepend))

(defconst magit-popup-type-plural-alist
  '((:switch . :switches)
    (:option . :options)
    (:variable . :variables)
    (:action . :actions)
    (:sequence-action . :sequence-actions)))

(defun magit-popup-pluralize-type (type)
  (or (cdr (assq type magit-popup-type-plural-alist))
      type))

(defun magit-define-popup-key
    (popup type key def &optional at prepend)
  "In POPUP, define KEY as an action, switch, or option.
It's better to use one of the specialized functions
  `magit-define-popup-action',
  `magit-define-popup-sequence-action',
  `magit-define-popup-switch',
  `magit-define-popup-option', or
  `magit-define-popup-variable'."
  (declare (indent defun))
  (setq type (magit-popup-pluralize-type type))
  (if (memq type '(:switches :options :variables :actions :sequence-actions))
      (if (boundp popup)
          (let* ((plist (symbol-value popup))
                 (value (plist-get plist type))
                 (elt   (assoc key value)))
            (if elt
                (setcdr elt def)
              (setq elt (cons key def)))
            (if at
                (when (setq at (cl-member at value :key 'car-safe :test 'equal))
                  (setq value (cl-delete key value :key 'car-safe :test 'equal))
                  (if prepend
                      (progn (push (car at) (cdr at))
                             (setcar at elt))
                    (push elt (cdr at))))
              (setq value (cl-delete key value :key 'car-safe :test 'equal)))
            (unless (assoc key value)
              (setq value (if prepend
                              (cons elt value)
                            (append value (list elt)))))
            (set popup (plist-put plist type value)))
        (push (list type key def at prepend)
              (get popup 'magit-popup-deferred)))
    (error "Unknown popup event type: %s" type)))

(defun magit-define-popup-keys-deferred (popup)
  (dolist (args (get popup 'magit-popup-deferred))
    (condition-case err
        (apply #'magit-define-popup-key popup args)
      ((debug error)
       (display-warning 'magit (error-message-string err) :error))))
  (put popup 'magit-popup-deferred nil))

(defun magit-change-popup-key (popup type from to)
  "In POPUP, bind TO to what FROM was bound to.
TYPE is one of `:action', `:sequence-action', `:switch', or
`:option'.  Bind TO and unbind FROM, both are characters."
  (--if-let (assoc from (plist-get (symbol-value popup)
                                   (magit-popup-pluralize-type type)))
      (setcar it to)
    (message "magit-change-popup-key: FROM key %c is unbound" from)))

(defun magit-remove-popup-key (popup type key)
  "In POPUP, remove KEY's binding of TYPE.
POPUP is a popup command defined using `magit-define-popup'.
TYPE is one of `:action', `:sequence-action', `:switch', or
`:option'.  KEY is the character which is to be unbound."
  (setq type (magit-popup-pluralize-type type))
  (let* ((plist (symbol-value popup))
         (alist (plist-get plist type))
         (value (assoc key alist)))
    (set popup (plist-put plist type (delete value alist)))))

;;; Invoke

(defvar-local magit-popup-previous-winconf nil)

(defun magit-invoke-popup (popup mode arg)
  (let* ((def     (symbol-value popup))
         (val     (symbol-value (plist-get def :variable)))
         (default (plist-get def :default-action))
         (local   (plist-get def :use-prefix))
         (local   (if (functionp local)
                      (funcall local)
                    local))
         (use-prefix (or local magit-popup-use-prefix-argument)))
    (cond
     ((or (and (eq use-prefix 'default) arg)
          (and (eq use-prefix 'popup) (not arg)))
      (if default
          (let ((magit-current-popup (list popup 'default))
                (magit-current-popup-args
                 (let ((magit-this-popup popup)
                       (magit-this-popup-events nil))
                   (magit-popup-default-setup val def)
                   (magit-popup-get-args))))
            (when (and arg (listp arg))
              (setq current-prefix-arg (and (not (= (car arg) 4))
                                            (list (/ (car arg) 4)))))
            (call-interactively default))
        (message "%s has no default action; showing popup instead." popup)
        (magit-popup-mode-setup popup mode)))
     ((memq use-prefix '(default popup nil))
      (magit-popup-mode-setup popup mode)
      (when magit-popup-show-help-echo
        (message
         (format
          "[%s] show common commands, [%s] describe events, [%s] show manual"
          (propertize "C-t"   'face 'magit-popup-key)
          (propertize "?"     'face 'magit-popup-key)
          (propertize "C-h i" 'face 'magit-popup-key)))))
     (local
      (error "Invalid :use-prefix popup property value: %s" use-prefix))
     (t
      (error "Invalid magit-popup-use-prefix-argument value: %s" use-prefix)))))

(defun magit-invoke-popup-switch (event)
  (interactive (list last-command-event))
  (--if-let (magit-popup-lookup event :switches)
      (progn
        (setf (magit-popup-event-use it)
              (not (magit-popup-event-use it)))
        (magit-refresh-popup-buffer))
    (user-error "%c isn't bound to any switch" event)))

(defun magit-invoke-popup-option (event)
  (interactive (list last-command-event))
  (--if-let (magit-popup-lookup event :options)
      (progn
        (if (magit-popup-event-use it)
            (setf (magit-popup-event-use it) nil)
          (let* ((arg (magit-popup-event-arg it))
                 (val (funcall
                       (magit-popup-event-fun it)
                       (concat arg (unless (string-match-p "=$" arg) ": "))
                       (magit-popup-event-val it))))
            (setf (magit-popup-event-use it) t)
            (setf (magit-popup-event-val it) val)))
        (magit-refresh-popup-buffer))
    (user-error "%c isn't bound to any option" event)))

(defun magit-invoke-popup-action (event)
  (interactive (list last-command-event))
  (let ((action   (magit-popup-lookup event :actions))
        (variable (magit-popup-lookup event :variables)))
    (when (and variable (not (magit-popup-event-arg variable)))
      (setq action variable)
      (setq variable nil))
    (cond ((or action variable)
           (let* ((magit-current-popup magit-this-popup)
                  (magit-current-popup-args (magit-popup-get-args))
                  (command (magit-popup-event-fun (or action variable)))
                  (magit-current-popup-action command))
             (when action
               (magit-popup-quit))
             (call-interactively command)
             (when (eq this-command 'magit-invoke-popup-action)
               (setq this-command command))
             (unless action
               (magit-refresh-popup-buffer))))
          ((eq event ?q)
           (magit-popup-quit)
           (when magit-previous-popup
             (magit-popup-mode-setup magit-previous-popup nil)))
          (t
           (user-error "%c isn't bound to any action" event)))))

(defun magit-popup-set-variable
    (variable choices &optional default other)
  (magit-set (--if-let (magit-git-string "config" "--local" variable)
                 (cadr (member it choices))
               (car choices))
             variable)
  (magit-refresh)
  (message "%s %s" variable
           (magit-popup-format-variable-1 variable choices default other)))

(defun magit-popup-quit ()
  "Quit the current popup command without invoking an action."
  (interactive)
  (let ((winconf magit-popup-previous-winconf))
    (if (derived-mode-p 'magit-popup-mode)
        (kill-buffer)
      (magit-popup-help-mode -1)
      (kill-local-variable 'magit-popup-previous-winconf))
    (when winconf
      (set-window-configuration winconf))))

(defun magit-popup-read-number (prompt &optional default)
  "Like `read-number' but DEFAULT may be a numeric string."
  (read-number prompt (if (stringp default)
                          (string-to-number default)
                        default)))

;;; Save

(defun magit-popup-set-default-arguments (arg)
  "Set default value for the arguments for the current popup.
Then close the popup without invoking an action; unless a prefix
argument is used in which case the popup remains open.

For a popup named `NAME-popup' that usually means setting the
value of the custom option `NAME-arguments'."
  (interactive "P")
  (-if-let (var (magit-popup-get :variable))
      (progn (customize-set-variable var (magit-popup-get-args))
             (unless arg (magit-popup-quit)))
    (user-error "Nothing to set")))

(defun magit-popup-save-default-arguments (arg)
  "Save default value for the arguments for the current popup.
Then close the popup without invoking an action; unless a prefix
argument is used in which case the popup remains open.

For a popup named `NAME-popup' that usually means saving the
value of the custom option `NAME-arguments'."
  (interactive "P")
  (-if-let (var (magit-popup-get :variable))
      (progn (customize-save-variable var (magit-popup-get-args))
             (unless arg (magit-popup-quit)))
    (user-error "Nothing to save")))

;;; Help

(defun magit-popup-toggle-show-common-commands ()
  "Show or hide an additional section with common commands.
The commands listed in this section are common to all popups
and are defined in `magit-popup-mode-map' (which see)."
  (interactive)
  (setq magit-popup-show-common-commands
        (not magit-popup-show-common-commands))
  (magit-refresh-popup-buffer)
  (fit-window-to-buffer))

(defun magit-popup-help ()
  "Show help for the argument or action at point."
  (interactive)
  (let* ((man (magit-popup-get :man-page))
         (key (read-key-sequence
               (concat "Describe key" (and man " (? for manpage)") ": ")))
         (int (aref key (1- (length key))))
         (def (or (lookup-key (current-local-map)  key t)
                  (lookup-key (current-global-map) key))))
    (pcase def
      (`magit-invoke-popup-switch
       (magit-popup-manpage man (magit-popup-lookup int :switches)))
      (`magit-invoke-popup-option
       (magit-popup-manpage man (magit-popup-lookup int :options)))
      (`magit-popup-help
       (magit-popup-manpage man nil))
      ((or `self-insert-command
           `magit-invoke-popup-action)
       (setq def (or (magit-popup-lookup int :actions)
                     (magit-popup-lookup int :variables)))
       (if def
           (magit-popup-describe-function (magit-popup-event-fun def))
         (ding)
         (message nil)))
      (`nil (ding)
            (message nil))
      (_    (magit-popup-describe-function def)))))

(defun magit-popup-manpage (topic arg)
  (unless topic
    (user-error "No man page associated with %s"
                (magit-popup-get :man-page)))
  (when arg
    (setq arg (magit-popup-event-arg arg)))
  (let ((winconf (current-window-configuration)) buffer)
    (pcase magit-popup-manpage-package
      (`woman (delete-other-windows)
              (split-window-below)
              (with-no-warnings ; display-buffer-function is obsolete
                (let ((display-buffer-alist nil)
                      (display-buffer-function nil)
                      (display-buffer-overriding-action nil))
                  (woman topic)))
              (setq buffer (current-buffer)))
      (`man   (cl-letf (((symbol-function #'fboundp) (lambda (_) nil)))
                (setq buffer (man topic)))
              (delete-other-windows)
              (split-window-below)
              (set-window-buffer (selected-window) buffer)))
    (with-current-buffer buffer
      (setq magit-popup-previous-winconf winconf)
      (magit-popup-help-mode)
      (fit-window-to-buffer (next-window))
      (if (and arg
               (Man-find-section "OPTIONS")
               (re-search-forward (format "^[\t\s]+\\(-., \\)*?%s[=\n]" arg)
                                  (save-excursion
                                    (Man-next-section 1)
                                    (point))
                                  t))
          (goto-char (1+ (match-beginning 0)))
        (goto-char (point-min))))))

(defun magit-popup-describe-function (function)
  (let ((winconf (current-window-configuration)))
    (delete-other-windows)
    (split-window-below)
    (other-window 1)
    (with-no-warnings ; display-buffer-function is obsolete
      (let ((display-buffer-alist '(("" display-buffer-use-some-window)))
            (display-buffer-function nil)
            (display-buffer-overriding-action nil)
            (help-window-select nil))
        (describe-function function)))
    (fit-window-to-buffer)
    (other-window 1)
    (setq magit-popup-previous-winconf winconf)
    (magit-popup-help-mode)))

(defun magit-popup-info ()
  "Show the popup manual."
  (interactive)
  (let ((winconf (current-window-configuration)))
    (delete-other-windows)
    (split-window-below)
    (info "(magit-popup.info)Usage")
    (magit-popup-help-mode)
    (setq magit-popup-previous-winconf winconf))
  (magit-popup-help-mode)
  (fit-window-to-buffer (next-window)))

(define-minor-mode magit-popup-help-mode
  "Auxiliary minor mode used to restore previous window configuration.
When some sort of help buffer is created from within a popup,
then this minor mode is turned on in that buffer, so that when
the user quits it, the previous window configuration is also
restored."
  :keymap '(([remap Man-quit]    . magit-popup-quit)
            ([remap Info-exit]   . magit-popup-quit)
            ([remap quit-window] . magit-popup-quit)))

;;; Modes

(define-derived-mode magit-popup-mode fundamental-mode "MagitPopup"
  "Major mode for infix argument popups."
  :mode 'magit-popup
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local scroll-margin 0)
  (setq-local magit-popup-show-common-commands magit-popup-show-common-commands)
  (hack-dir-local-variables-non-file-buffer))

(put 'magit-popup-mode 'mode-class 'special)

(defun magit-popup-default-setup (val def)
  (if (--when-let (magit-popup-get :sequence-predicate)
        (funcall it))
      (magit-popup-put :actions (magit-popup-convert-actions
                                 val (magit-popup-get :sequence-actions)))
    (magit-popup-put :variables (magit-popup-convert-variables
                                 val (plist-get def :variables)))
    (magit-popup-put :switches  (magit-popup-convert-switches
                                 val (plist-get def :switches)))
    (magit-popup-put :options   (magit-popup-convert-options
                                 val (plist-get def :options)))
    (magit-popup-put :actions   (magit-popup-convert-actions
                                 val (plist-get def :actions)))))

(defun magit-popup-mode-setup (popup mode)
  (setq magit-previous-popup magit-current-popup)
  (let ((val (symbol-value (plist-get (symbol-value popup) :variable)))
        (def (symbol-value popup)))
    (magit-popup-mode-display-buffer (get-buffer-create
                                      (format "*%s*" popup))
                                     (or mode 'magit-popup-mode))
    (setq magit-this-popup popup)
    (if (bound-and-true-p magit-popup-setup-hook) ; obsolete
        (run-hook-with-args 'magit-popup-setup-hook val def)
      (funcall (or (magit-popup-get :setup-function)
                   'magit-popup-default-setup)
               val def)))
  (magit-refresh-popup-buffer)
  (fit-window-to-buffer nil nil (line-number-at-pos (point-max))))

(defun magit-popup-mode-display-buffer (buffer mode)
  (let ((winconf (current-window-configuration)))
    (select-window (display-buffer buffer magit-popup-display-buffer-action))
    (funcall mode)
    (setq magit-popup-previous-winconf winconf)))

(defvar magit-refresh-popup-buffer-hook nil
  "Hook run by `magit-refresh-popup-buffer'.

The hook is run right after inserting the representation of the
popup events but before optionally inserting the representation
of events shared by all popups and before point is adjusted.")

(defun magit-refresh-popup-buffer ()
  (let* ((inhibit-read-only t)
         (button (button-at (point)))
         (prefix (and button (button-get button 'prefix)))
         (event  (and button (button-get button 'event))))
    (erase-buffer)
    (save-excursion
      (--if-let (magit-popup-get :refresh-function)
          (funcall it)
        (magit-popup-insert-section 'magit-popup-switch-button)
        (magit-popup-insert-section 'magit-popup-option-button)
        (magit-popup-insert-section 'magit-popup-variable-button)
        (magit-popup-insert-section 'magit-popup-action-button))
      (run-hooks 'magit-refresh-popup-buffer-hook)
      (when magit-popup-show-common-commands
        (magit-popup-insert-command-section
         'magit-popup-internal-command-button
         magit-popup-common-commands)))
    (set-buffer-modified-p nil)
    (when event
      (while (and (ignore-errors (forward-button 1))
                  (let ((b (button-at (point))))
                    (or (not (equal (button-get b 'prefix) prefix))
                        (not (equal (button-get b 'event)  event)))))))))

;;; Draw

(defvar magit-popup-min-padding 3
  "Minimal amount of whitespace between columns in popup buffers.")

(defun magit-popup-insert-section (type &optional spec heading)
  (if (not spec)
      (progn (setq spec (magit-popup-get (button-type-get type 'property)))
             (when spec
               (if (or (stringp (car spec))
                       (functionp (car spec)))
                   (--each (--partition-by-header
                            (or (stringp it) (functionp it))
                            spec)
                     (magit-popup-insert-section type (cdr it) (car it)))
                 (magit-popup-insert-section type spec))))
    (let* ((formatter (button-type-get type 'formatter))
           (items (mapcar (lambda (ev)
                            (and ev (or (funcall formatter type ev) '(""))))
                          (or spec (magit-popup-get
                                    (button-type-get type 'property)))))
           (maxcols (button-type-get type 'maxcols))
           (pred (magit-popup-get :sequence-predicate)))
      (if (and pred (funcall pred))
          (setq maxcols nil)
        (cl-typecase maxcols
          (keyword (setq maxcols (magit-popup-get maxcols)))
          (symbol  (setq maxcols (symbol-value maxcols)))))
      (when (functionp maxcols)
        (setq maxcols (funcall maxcols heading)))
      (when items
        (if (functionp heading)
            (when (setq heading (funcall heading))
              (insert heading ?\n))
          (unless heading
            (setq heading (button-type-get type 'heading)))
          (insert (propertize heading 'face 'magit-popup-heading))
          (unless (string-match "\n$" heading)
            (insert "\n")))
        (when heading
          (let ((colwidth
                 (+ (apply 'max (mapcar (lambda (e) (length (car e))) items))
                    magit-popup-min-padding)))
            (dolist (item items)
              (unless (bolp)
                (let ((padding (- colwidth (% (current-column) colwidth))))
                  (if (and (< (+ (current-column) padding colwidth)
                              (window-width))
                           (< (ceiling (/ (current-column) (* colwidth 1.0)))
                              (or maxcols 1000)))
                      (insert (make-string padding ?\s))
                    (insert "\n"))))
              (unless (equal item '(""))
                (if item
                    (apply 'insert-button item)
                  (insert ?\s)))))
          (insert (if (= (char-before) ?\n) "\n" "\n\n")))))))

(defun magit-popup-format-argument-button (type ev)
  (list (format-spec
         (button-type-get type 'format)
         `((?k . ,(propertize (concat
                               (--when-let (button-type-get type 'prefix)
                                 (char-to-string it))
                               (magit-popup-event-keydsc ev))
                              'face 'magit-popup-key))
           (?d . ,(magit-popup-event-dsc ev))
           (?a . ,(propertize (magit-popup-event-arg ev)
                              'face (if (magit-popup-event-use ev)
                                        'magit-popup-argument
                                      'magit-popup-disabled-argument)))
           (?v . ,(let ((val (magit-popup-event-val ev)))
                    (if (and (magit-popup-event-use ev)
                             (not (equal val "")))
                        (propertize (format "\"%s\"" val)
                                    'face 'magit-popup-option-value)
                      "")))))
        'type type 'event (magit-popup-event-key ev)))

(defun magit-popup-format-variable-button (type ev)
  (if (not (magit-popup-event-arg ev))
      (magit-popup-format-action-button 'magit-popup-action-button ev)
    (list (format-spec
           (button-type-get type 'format)
           `((?k . ,(propertize (magit-popup-event-keydsc ev)
                                'face 'magit-popup-key))
             (?d . ,(funcall (magit-popup-event-arg ev)))))
          'type type 'event (magit-popup-event-key ev))))

(defun magit-popup-format-variable
    (variable choices &optional default other width)
  (concat variable
          (if width (make-string (- width (length variable)) ?\s) " ")
          (magit-popup-format-variable-1 variable choices default other)))

(defun magit-popup-format-variable-1
    (variable choices &optional default other)
  "Print popup entry for git VARIABLE with possible CHOICES.
DEFAULT is git's default choice for VARIABLE.  OTHER is a git
variable whose value may be used as a default."
  (let ((local  (magit-git-string "config" "--local"  variable))
        (global (magit-git-string "config" "--global" variable)))
    (when other
      (setq other (--when-let (magit-get other)
                    (concat other ":" it))))
    (concat
     (propertize "[" 'face 'magit-popup-disabled-argument)
     (mapconcat
      (lambda (choice)
        (propertize choice 'face (if (equal choice local)
                                     'magit-popup-option-value
                                   'magit-popup-disabled-argument)))
      choices
      (propertize "|" 'face 'magit-popup-disabled-argument))
     (when (or global other default)
       (concat
        (propertize "|" 'face 'magit-popup-disabled-argument)
        (cond (global
               (propertize (concat "global:" global)
                           'face (cond (local
                                        'magit-popup-disabled-argument)
                                       ((member global choices)
                                        'magit-popup-option-value)
                                       (t
                                        'font-lock-warning-face))))
              (other
               (propertize other
                           'face (if local
                                     'magit-popup-disabled-argument
                                   'magit-popup-option-value)))
              (default
               (propertize (concat "default:" default)
                           'face (if local
                                     'magit-popup-disabled-argument
                                   'magit-popup-option-value))))))
     (propertize "]" 'face 'magit-popup-disabled-argument))))

(defun magit-popup-format-action-button (type ev)
  (let* ((dsc (magit-popup-event-dsc ev))
         (fun (and (functionp dsc) dsc)))
    (when fun
      (setq dsc
            (-when-let (branch (funcall fun))
              (if (next-single-property-change 0 'face (concat "0" branch))
                  branch
                (magit-branch-set-face branch)))))
    (when dsc
      (list (format-spec
             (button-type-get type 'format)
             `((?k . ,(propertize (magit-popup-event-keydsc ev)
                                  'face 'magit-popup-key))
               (?d . ,dsc)
               (?D . ,(if (and (not fun)
                               (eq (magit-popup-event-fun ev)
                                   (magit-popup-get :default-action)))
                          (propertize dsc 'face 'bold)
                        dsc))))
            'type type 'event (magit-popup-event-key ev)))))

(defun magit-popup-insert-command-section (type spec)
  (magit-popup-insert-section
   type (mapcar (lambda (elt)
                  (list (car (where-is-internal (cadr elt)
                                                (current-local-map)))
                        (car elt)))
                spec)))

(defun magit-popup-format-command-button (type elt)
  (nconc (magit-popup-format-action-button
          type (make-magit-popup-event :key (car  elt)
                                       :dsc (cadr elt)))
         (list 'function (lookup-key (current-local-map) (car elt)))))

;;; Utilities

(defun magit-popup-import-file-args (args files)
  (if files
      (cons (concat "-- " (mapconcat #'identity files ",")) args)
    args))

(defun magit-popup-export-file-args (args)
  (let ((files (--first (string-prefix-p "-- " it) args)))
    (when files
      (setq args  (remove files args))
      (setq files (split-string (substring files 3) ",")))
    (list args files)))

(defconst magit-popup-font-lock-keywords
  (eval-when-compile
    `((,(concat "(\\(magit-define-popup\\)\\_>"
                "[ \t'\(]*"
                "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
       (1 'font-lock-keyword-face)
       (2 'font-lock-function-name-face nil t)))))

(font-lock-add-keywords 'emacs-lisp-mode magit-popup-font-lock-keywords)

(provide 'magit-popup)
;;; magit-popup.el ends here
