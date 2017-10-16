;;; helm.el --- Emacs incremental and narrowing framework -*- lexical-binding: t -*-

;; Copyright (C) 2007         Tamas Patrovics
;;               2008 ~ 2011  rubikitch <rubikitch@ruby-lang.org>
;;               2011 ~ 2017  Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; This is a fork of anything.el wrote by Tamas Patrovics.

;; Authors of anything.el: Tamas Patrovics
;;                         rubikitch <rubikitch@ruby-lang.org>
;;                         Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; URL: http://github.com/emacs-helm/helm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'async)
(require 'advice) ; Shutup byte compiler about ad-deactivate.
(require 'helm-lib)
(require 'helm-multi-match)
(require 'helm-source)


;;; Multi keys
;;
;;
;;;###autoload
(defun helm-define-multi-key (keymap key functions &optional delay)
  "In KEYMAP, define key sequence KEY for function list FUNCTIONS.
Each function runs sequentially for each KEY press.
If DELAY is specified, switch back to initial function of FUNCTIONS list
after DELAY seconds.
The functions in FUNCTIONS list take no args.
e.g
    (defun foo ()
      (interactive)
      (message \"Run foo\"))
    (defun bar ()
      (interactive)
      (message \"Run bar\"))
    (defun baz ()
      (interactive)
      (message \"Run baz\"))

\(helm-define-multi-key global-map (kbd \"<f5> q\") '(foo bar baz) 2)

Each time \"<f5> q\" is pressed, the next function is executed. Waiting
more than 2 seconds between key presses switches back to executing the first
function on the next hit."
  (define-key keymap key (helm-make-multi-command functions delay)))

;;;###autoload
(defmacro helm-multi-key-defun (name docstring funs &optional delay)
  "Define NAME as a multi-key command running FUNS.
After DELAY seconds, the FUNS list is reinitialized.
See `helm-define-multi-key'."
  (declare (indent 2))
  (setq docstring (if docstring (concat docstring "\n\n")
                    "This is a helm-ish multi-key command."))
  `(defalias (quote ,name) (helm-make-multi-command ,funs ,delay) ,docstring))

(defun helm-make-multi-command (functions &optional delay)
  "Return an anonymous multi-key command running FUNCTIONS.
Run each function in the FUNCTIONS list in turn when called within DELAY seconds."
  (declare (indent 1))
  (let ((funs functions)
        (iter (cl-gensym "helm-iter-key"))
        (timeout delay))
    (eval (list 'defvar iter nil))
    (lambda ()
      (interactive)
      (helm-run-multi-key-command funs iter timeout))))

(defun helm-run-multi-key-command (functions iterator delay)
  (let ((fn (lambda ()
                (cl-loop for count from 1 to (length functions)
                      collect count)))
        next)
    (unless (and (symbol-value iterator)
                 ;; Reset iterator when another key is pressed.
                 (eq this-command real-last-command))
      (set iterator (helm-iter-list (funcall fn))))
    (setq next (helm-iter-next (symbol-value iterator)))
    (unless next
      (set iterator (helm-iter-list (funcall fn)))
      (setq next (helm-iter-next (symbol-value iterator))))
    (and next (symbol-value iterator)
         (call-interactively (nth (1- next) functions)))
    (when delay (run-with-idle-timer
                 delay nil (lambda ()
                             (set iterator nil))))))

(helm-multi-key-defun helm-toggle-resplit-and-swap-windows
    "Multi key command to re-split and swap helm window.
First call runs `helm-toggle-resplit-window',
and second call within 0.5s runs `helm-swap-windows'."
  '(helm-toggle-resplit-window helm-swap-windows) 1)
(put 'helm-toggle-resplit-and-swap-windows 'helm-only t)

;;;###autoload
(defun helm-define-key-with-subkeys (map key subkey command
                                         &optional other-subkeys prompt exit-fn)
  "Defines in MAP a KEY and SUBKEY to COMMAND.

This allows typing KEY to call COMMAND the first time and
type only SUBKEY on subsequent calls.

Arg MAP is the keymap to use, SUBKEY is the initial short key-binding to
call COMMAND.

Arg OTHER-SUBKEYS is an alist specifying other short key-bindings
to use once started e.g:

    \(helm-define-key-with-subkeys global-map
       \(kbd \"C-x v n\") ?n 'git-gutter:next-hunk '((?p . git-gutter:previous-hunk))\)


In this example, `C-x v n' will run `git-gutter:next-hunk'
subsequent \"n\"'s run this command again
and subsequent \"p\"'s run `git-gutter:previous-hunk'.

If specified PROMPT can be displayed in minibuffer to
describe SUBKEY and OTHER-SUBKEYS.
Arg EXIT-FN specifies a function to run on exit.

For any other keys pressed, run their assigned command as defined
in MAP and then exit the loop running EXIT-FN, if specified.

NOTE: SUBKEY and OTHER-SUBKEYS bindings support only char syntax and
vectors, so don't use strings to define them."
  (declare (indent 1))
  (define-key map key
    (lambda ()
      (interactive)
      (unwind-protect
          (progn
            (call-interactively command)
            (while (let ((input (read-key prompt)) other kb com)
                     (setq last-command-event input)
                     (cond
                      ((eq input subkey)
                       (call-interactively command)
                       t)
                      ((setq other (assoc input other-subkeys))
                       (call-interactively (cdr other))
                       t)
                      (t
                       (setq kb (vector last-command-event))
                       (setq com (lookup-key map kb))
                       (if (commandp com)
                           (call-interactively com)
                         (setq unread-command-events
                               (nconc (mapcar 'identity kb)
                                      unread-command-events)))
                       nil)))))
        (and exit-fn (funcall exit-fn))))))

;;; Keymap
;;
;;
(defvar helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "<down>")     'helm-next-line)
    (define-key map (kbd "<up>")       'helm-previous-line)
    (define-key map (kbd "C-n")        'helm-next-line)
    (define-key map (kbd "C-p")        'helm-previous-line)
    (define-key map (kbd "<C-down>")   'helm-follow-action-forward)
    (define-key map (kbd "<C-up>")     'helm-follow-action-backward)
    (define-key map (kbd "<prior>")    'helm-previous-page)
    (define-key map (kbd "<next>")     'helm-next-page)
    (define-key map (kbd "M-v")        'helm-previous-page)
    (define-key map (kbd "C-v")        'helm-next-page)
    (define-key map (kbd "M-<")        'helm-beginning-of-buffer)
    (define-key map (kbd "M->")        'helm-end-of-buffer)
    (define-key map (kbd "C-g")        'helm-keyboard-quit)
    (define-key map (kbd "<right>")    'helm-next-source)
    (define-key map (kbd "<left>")     'helm-previous-source)
    (define-key map (kbd "<RET>")      'helm-maybe-exit-minibuffer)
    (define-key map (kbd "C-i")        'helm-select-action)
    (define-key map (kbd "C-z")        'helm-execute-persistent-action)
    (define-key map (kbd "C-j")        'helm-execute-persistent-action)
    (define-key map (kbd "C-o")        'helm-next-source)
    (define-key map (kbd "M-o")        'helm-previous-source)
    (define-key map (kbd "C-l")        'helm-recenter-top-bottom-other-window)
    (define-key map (kbd "M-C-l")      'helm-reposition-window-other-window)
    (define-key map (kbd "C-M-v")      'helm-scroll-other-window)
    (define-key map (kbd "M-<next>")   'helm-scroll-other-window)
    (define-key map (kbd "C-M-y")      'helm-scroll-other-window-down)
    (define-key map (kbd "C-M-S-v")    'helm-scroll-other-window-down)
    (define-key map (kbd "M-<prior>")  'helm-scroll-other-window-down)
    (define-key map (kbd "<C-M-down>") 'helm-scroll-other-window)
    (define-key map (kbd "<C-M-up>")   'helm-scroll-other-window-down)
    (define-key map (kbd "C-@")        'helm-toggle-visible-mark)
    (define-key map (kbd "C-SPC")      'helm-toggle-visible-mark)
    (define-key map (kbd "M-SPC")      'helm-toggle-visible-mark)
    (define-key map (kbd "M-[")        nil)
    (define-key map (kbd "M-(")        'helm-prev-visible-mark)
    (define-key map (kbd "M-)")        'helm-next-visible-mark)
    (define-key map (kbd "C-k")        'helm-delete-minibuffer-contents)
    (define-key map (kbd "C-x C-f")    'helm-quit-and-find-file)
    (define-key map (kbd "M-m")        'helm-toggle-all-marks)
    (define-key map (kbd "M-a")        'helm-mark-all)
    (define-key map (kbd "M-U")        'helm-unmark-all)
    (define-key map (kbd "C-M-a")      'helm-show-all-in-this-source-only)
    (define-key map (kbd "C-M-e")      'helm-display-all-sources)
    (define-key map (kbd "C-s")        'undefined)
    (define-key map (kbd "M-s")        'undefined)
    (define-key map (kbd "C-}")        'helm-narrow-window)
    (define-key map (kbd "C-{")        'helm-enlarge-window)
    (define-key map (kbd "C-c -")      'helm-swap-windows)
    (define-key map (kbd "C-c C-y")    'helm-yank-selection)
    (define-key map (kbd "C-c C-k")    'helm-kill-selection-and-quit)
    (define-key map (kbd "C-c C-i")    'helm-copy-to-buffer)
    (define-key map (kbd "C-c C-f")    'helm-follow-mode)
    (define-key map (kbd "C-c C-u")    'helm-refresh)
    (define-key map (kbd "C-c >")      'helm-toggle-truncate-line)
    (define-key map (kbd "M-p")        'previous-history-element)
    (define-key map (kbd "M-n")        'next-history-element)
    (define-key map (kbd "C-!")        'helm-toggle-suspend-update)
    (define-key map (kbd "C-x b")      'helm-resume-previous-session-after-quit)
    (define-key map (kbd "C-x C-b")    'helm-resume-list-buffers-after-quit)
    (helm-define-key-with-subkeys map (kbd "C-c n") ?n 'helm-run-cycle-resume)
    ;; Disable `file-cache-minibuffer-complete'.
    (define-key map (kbd "<C-tab>")    'undefined)
    ;; Multi keys
    (define-key map (kbd "C-t")        'helm-toggle-resplit-and-swap-windows)
    ;; Debugging command
    (define-key map (kbd "C-h C-d")    'undefined)
    (define-key map (kbd "C-h C-d")    'helm-enable-or-switch-to-debug)
    (define-key map (kbd "C-h c")      'helm-customize-group)
    ;; Allow to eval keymap without errors.
    (define-key map [f1] nil)
    (define-key map (kbd "C-h C-h")    'undefined)
    (define-key map (kbd "C-h h")      'undefined)
    (helm-define-key-with-subkeys map
      (kbd "C-w") ?\C-w 'helm-yank-text-at-point
      '((?\C-_ . helm-undo-yank-text-at-point)))
    ;; Use `describe-mode' key in `global-map'.
    (cl-dolist (k (where-is-internal 'describe-mode global-map))
      (define-key map k 'helm-help))
    (define-key map (kbd "C-c ?")    'helm-help)
    ;; Bind all actions from 1 to 12 to their corresponding nth index+1.
    (cl-loop for n from 0 to 12 do
             (define-key map (kbd (format "<f%s>" (1+ n)))
               `(lambda ()
                  (interactive)
                  (helm-select-nth-action ,n))))
    map)
  "Keymap for helm.")

(defun helm-customize-group ()
  "Jump to customization group of current source.

Default to `helm' when group is not defined in source."
  (interactive)
  (helm-run-after-exit 'customize-group (helm-attr 'group)))
(put 'helm-customize-group 'helm-only t)

(defun helm--action-at-nth-set-fn-1 (value &optional negative)
  (cl-loop for n from 1 to 9
        for key = (format value n)
        for sym = (make-symbol (format "helm-execute-selection-action-at-nth-+%d" n))
        for fn = `(lambda ()
                     (interactive)
                     (helm-execute-selection-action-at-nth ,(if negative (- n) n)))
        do (progn
             (defalias sym fn)
             (define-key helm-map (kbd key) sym))))

(defun helm--action-at-nth-set-fn- (var val)
  (set var val)
  (helm--action-at-nth-set-fn-1 val 'negative))

(defun helm--action-at-nth-set-fn+ (var val)
  (set var val)
  (helm--action-at-nth-set-fn-1 val))

(defcustom helm-action-at-nth-negative-prefix-key "C-x %d"
  "The prefix key to execute default action on nth <-n> candidate.

This is a format spec where %d will be replaced by the candidate
number.

NOTE: `setq' have no effect until you restart emacs, use customize for
immediate effect."
  :group 'helm
  :type 'string
  :set #'helm--action-at-nth-set-fn-)

(defcustom helm-action-at-nth-positive-prefix-key "C-c %d"
  "The prefix key to execute default action on nth <+n> candidate.

This is a format spec where %d will be replaced by the candidate
number.

NOTE: `setq' have no effect until you restart emacs, use customize for
immediate effect."
  :group 'helm
  :type 'string
  :set #'helm--action-at-nth-set-fn+)


(defgroup helm nil
  "Open helm."
  :prefix "helm-" :group 'convenience)

(defcustom helm-completion-window-scroll-margin 5
  " `scroll-margin' to use for helm completion window.
Set to 0 to disable.
NOTE: This has no effect when `helm-display-source-at-screen-top'
id is non-`nil'."
  :group 'helm
  :type  'integer)

(defcustom helm-display-source-at-screen-top t
  "Display candidates at the top of screen.
This happens with `helm-next-source' and `helm-previous-source'.
NOTE: When non-`nil' (default), disable `helm-completion-window-scroll-margin'."
  :group 'helm
  :type 'boolean)

(defcustom helm-candidate-number-limit 100
  "Global limit for number of candidates displayed.
When the pattern is empty, the number of candidates shown will be
as set here instead of the entire list, which may be hundreds or
thousands. Since narrowing and filtering rapidly reduces
available candidates, having a small list will keep the interface
responsive.

Set this value to nil for no limit."
  :group 'helm
  :type '(choice (const :tag "Disabled" nil) integer))

(defcustom helm-input-idle-delay 0.01
  "Idle time before updating, specified in seconds."
  :group 'helm
  :type 'float)

(defcustom helm-exit-idle-delay 0
  "Idle time before exiting minibuffer while helm is updating.
Has no affect when helm-buffer is up to date \(i.e exit without
delay in this condition\)."
  :group 'helm
  :type 'float)

(defcustom helm-full-frame nil
  "Use current window for showing candidates.
If t, then Helm does not pop-up new window."
  :group 'helm
  :type 'boolean)

(defvaralias 'helm-samewindow 'helm-full-frame)
(make-obsolete-variable 'helm-samewindow 'helm-full-frame "1.4.8.1")

(defcustom helm-candidate-separator
  "--------------------"
  "Candidates separator of `multiline' source."
  :group 'helm
  :type 'string)

(defcustom helm-save-configuration-functions
  '(set-window-configuration . current-window-configuration)
  "Functions used to restore or save configurations for frames and windows.
Specified as a pair of functions, where car is the restore function and cdr
is the save function.

To save and restore frame configuration, set this variable to
'\(set-frame-configuration . current-frame-configuration\)

NOTE: This may not work properly with own-frame minibuffer
settings. Older versions saves/restores frame configuration, but
the default has changed now to avoid flickering."
  :group 'helm
  :type 'sexp)

(defcustom helm-display-function 'helm-default-display-buffer
  "Function to display *helm* buffer.
By default, it is `helm-default-display-buffer', which affects
`helm-full-frame'."
  :group 'helm
  :type 'symbol)

(defcustom helm-case-fold-search 'smart
  "Adds 'smart' option to `case-fold-search'.
Smart option ignores case for searches as long as there are no
upper case characters in the pattern.

Use nil or t to turn off smart behavior and use
`case-fold-search' behavior.

Default is smart.

NOTE: Case fold search has no effect when searching asynchronous
sources, which rely on customized features implemented directly
into their execution process. See helm-grep.el for an example."
  :group 'helm
  :type '(choice (const :tag "Ignore case" t)
                 (const :tag "Respect case" nil)
                 (other :tag "Smart" 'smart)))

(defcustom helm-file-name-case-fold-search
  (if (memq system-type
            '(cygwin windows-nt ms-dos darwin))
      t
    helm-case-fold-search)
  "Local setting of `helm-case-fold-search' for reading filenames.

See `helm-case-fold-search' for more info."
  :group 'helm
  :type 'symbol)

(defcustom helm-reuse-last-window-split-state nil
  "Use the same state of window split, vertical or horizontal.
`helm-toggle-resplit-window' for the next helm session will use
the same window scheme as the previous session unless
`helm-split-window-default-side' is 'same or 'other."
  :group 'helm
  :type 'boolean)

(defcustom helm-split-window-preferred-function 'helm-split-window-default-fn
  "Default function used for splitting window."
  :group 'helm
  :type 'function)

(defcustom helm-split-window-default-side 'below
  "The default side to display `helm-buffer'.
Must be one acceptable arg for `split-window' SIDE,
that is `below', `above', `left' or `right'.

Other acceptable values are `same' which always display
`helm-buffer' in current window and `other' that display
`helm-buffer' below if only one window or in
`other-window-for-scrolling' when available.

A nil value has same effect as `below'.
If `helm-full-frame' is non-`nil', it take precedence over this setting.

See also `helm-split-window-in-side-p' and `helm-always-two-windows' that
take precedence over this.

NOTE: this have no effect if `helm-split-window-preferred-function' is not
`helm-split-window-default-fn' unless this new function can handle this."
  :group 'helm
  :type 'symbol)

(defcustom helm-display-buffer-default-height nil
  "Initial height of `helm-buffer', specified as an integer or a function.

The function should take one arg and the responsibility for
re-sizing the window; function's return value is ignored.
Note that this have no effect when the split is vertical.
See `display-buffer' for more info."
  :group 'helm
  :type '(choice integer function))

(defcustom helm-display-buffer-default-width nil
  "Initial width of `helm-buffer', specified as an integer or a function.

The function should take one arg and the responsibility for
re-sizing the window; function's return value is ignored.
Note that this have no effect when the split is horizontal.
See `display-buffer' for more info."
  :group 'helm
  :type '(choice integer function))

(defcustom helm-split-window-in-side-p nil
  "Forces split inside selected window when non-`nil'.
See also `helm-split-window-default-side'.

NOTE: this has no effect if
`helm-split-window-preferred-function' is not
`helm-split-window-default-fn' unless this new function can
handle this."
  :group 'helm
  :type 'boolean)

(defcustom helm-always-two-windows nil
  "When non-`nil' helm uses two windows in this frame.
To display `helm-buffer' in one window and `helm-current-buffer'
in the other.

Note: this has no effect when `helm-split-window-in-side-p' is non-`nil',
or when `helm-split-window-default-side' is set to 'same.

When `helm-autoresize-mode' is enabled, setting this to nil
will have no effect.

Also when non-`nil' it overrides the effect of `helm-split-window-default-side'
set to `other'."
  :group 'helm
  :type 'boolean)

(defcustom helm-sources-using-default-as-input '(helm-source-imenu
                                                 helm-source-imenu-all
                                                 helm-source-info-elisp
                                                 helm-source-etags-select
                                                 helm-source-man-pages
                                                 helm-source-occur
                                                 helm-source-moccur
                                                 helm-source-grep-ag
                                                 helm-source-grep-git
                                                 helm-source-grep)
  "List of helm sources that need to use `helm--maybe-use-default-as-input'.
When a source is a member of this list, default `thing-at-point'
will be used as input."
  :group 'helm
  :type '(repeat (choice symbol)))

(defcustom helm-delete-minibuffer-contents-from-point t
  "When non-`nil', `helm-delete-minibuffer-contents' delete region from `point'.
Otherwise delete `minibuffer-contents'.
See documentation for  `helm-delete-minibuffer-contents'."
  :group 'helm
  :type 'boolean)

(defcustom helm-follow-mode-persistent nil
  "When non-`nil', save last state of `helm-follow-mode' for the next emacs sessions.

Each time you turn on or off `helm-follow-mode', the current source name will be stored
or removed from `helm-source-names-using-follow'.

Note that this may be disabled in some places where it is unsafe to use
because persistent action is changing according to context."
  :group 'helm
  :type 'boolean)

(defcustom helm-source-names-using-follow nil
  "A list of source names to have follow enabled.
This list of source names will be used only
when `helm-follow-mode-persistent' is non-nil.

You don't have to customize this yourself unless you really want and
know what you are doing, instead just set
`helm-follow-mode-persistent' to non-nil and as soon you turn on or
off `helm-follow-mode' (C-c C-f) in a source, helm will save or remove
source name in this variable."
  :group 'helm
  :type '(repeat (choice string)))

(defcustom helm-prevent-escaping-from-minibuffer t
  "Prevent escaping from minibuffer with `other-window' during the helm session."
  :group 'helm
  :type 'boolean)

(defcustom helm-allow-mouse nil
  "Allow mouse usage during the helm session when non-nil.

Note that this also allow moving out of minibuffer when clicking
outside of `helm-buffer', up to you to get back to helm by clicking
back in `helm-buffer' of minibuffer."
  :group 'helm
  :type 'boolean)

(defcustom helm-move-to-line-cycle-in-source nil
  "Cycle to the beginning or end of the list after reaching the bottom or top.
This applies when using `helm-next/previous-line'."
  :group 'helm
  :type 'boolean)

(defcustom helm-fuzzy-match-fn 'helm-fuzzy-match
  "The function for fuzzy matching in `helm-source-sync' based sources."
  :group 'helm
  :type 'function)

(defcustom helm-fuzzy-search-fn 'helm-fuzzy-search
  "The function for fuzzy matching in `helm-source-in-buffer' based sources."
  :group 'helm
  :type 'function)

(defcustom helm-fuzzy-sort-fn 'helm-fuzzy-matching-default-sort-fn
  "The sort transformer function used in fuzzy matching.
When nil, sorting is not done."
  :group 'helm
  :type 'function)

(defcustom helm-fuzzy-matching-highlight-fn 'helm-fuzzy-default-highlight-match
  "The function to highlight fuzzy matches.
When nil, no highlighting is done."
  :group 'helm
  :type 'function)

(defcustom helm-autoresize-max-height 40
  "Specifies maximum height and defaults to percent of helm window's frame height.

See `fit-window-to-buffer' for more infos."
  :group 'helm
  :type 'integer)

(defcustom helm-autoresize-min-height 10
  "Specifies minimum height and defaults to percent of helm window's frame height.

If nil, `window-min-height' is used.
See `fit-window-to-buffer' for details."
  :group 'helm
  :type 'integer)

(defcustom helm-input-method-verbose-flag nil
  "The default value for `input-method-verbose-flag' used in helm minibuffer.
It is nil by default, which does not turn off input method. Helm
updates and exits without interruption -- necessary for complex methods.

If set to any other value as per `input-method-verbose-flag',
then use `C-\\' to disable the `current-input-method' to exit or update helm"
  :group 'helm
  :type '(radio :tag "A flag to control extra guidance for input methods in helm."
                (const :tag "Never provide guidance" nil)
                (const :tag "Always provide guidance" t)
                (const :tag "Provide guidance only for complex methods" complex-only)))

(defcustom helm-display-header-line t
  "Display header-line when non nil."
  :group 'helm
  :type 'boolean)

(defcustom helm-inherit-input-method t
  "Inherit `current-input-method' from `current-buffer' when non-`nil'.
The default is to enable this by default and then toggle
`toggle-input-method'."
  :group 'helm
  :type 'boolean)

(defcustom helm-echo-input-in-header-line nil
  "Send current input in header-line when non-nil."
  :group 'helm
  :type 'boolean)

(defcustom helm-header-line-space-before-prompt 'left-fringe
  "Specify the space before prompt in header-line.

This will be used when `helm-echo-input-in-header-line' is non-nil.

Value can be one of the symbols 'left-fringe or 'left-margin or an
integer specifying the number of spaces before prompt.
Note that on input longer that `window-width' the continuation string
will be shown on left side of window without taking care of this."
  :group 'helm
  :type '(choice
          (symbol
           (const :tag "Fringe" 'left-fringe)
           (const :tag "Margin" 'left-margin))
          integer))

(defcustom helm-tramp-connection-min-time-diff 5
  "Value of `tramp-connection-min-time-diff' for helm remote processes.
If set to zero helm remote processes are not delayed.
Setting this to a value less than 5 or disabling it with a zero value
is risky, however on emacs versions starting at 24.5 it seems
it is now possible to disable it.
Anyway at any time in helm you can suspend your processes while typing
by hitting \\<helm-map> `\\[helm-toggle-suspend-update]'.
Only async sources than use a sentinel calling
`helm-process-deferred-sentinel-hook' are affected by this."
  :type 'integer
  :group 'helm)

(defcustom helm-debug-root-directory nil
  "When non-`nil', saves helm log messages to a file in this directory.
When `nil' log messages are saved to a buffer instead.
Log message are saved only when `helm-debug' is non-nil, so setting this
doesn't enable debugging by itself.

See `helm-log-save-maybe' for more info."
  :type 'string
  :group 'helm)

(defcustom helm-show-action-window-other-window nil
  "Show action buffer beside `helm-buffer' when non-nil.

If nil don't split and replace helm-buffer by the action buffer
in same window.
If left display the action buffer at the left of helm-buffer.
If right or any other value, split at right.

Note that this may not fit well with some helm window configurations,
so it have only effect when `helm-always-two-windows' is non-nil."
  :group 'helm
  :type '(choice
          (const :tag "Split at left" left)
          (const :tag "Don't split" nil)
          (other :tag "Split at right" right)))

(defcustom helm-cycle-resume-delay 1.0
  "Delay used before resuming in `helm-run-cycle-resume'."
  :type 'float
  :group 'helm)


;;; Faces
;;
;;
(defgroup helm-faces nil
  "Customize the appearance of helm."
  :prefix "helm-"
  :group 'faces
  :group 'helm)

(defface helm-source-header
  '((((background dark))
     :background "#22083397778B"
     :foreground "white"
     :weight bold :height 1.3 :family "Sans Serif")
    (((background light))
     :background "#abd7f0"
     :foreground "black"
     :weight bold :height 1.3 :family "Sans Serif"))
  "Face for source header in the helm buffer."
  :group 'helm-faces)

(defface helm-visible-mark
  '((((min-colors 88) (background dark))
     (:background "green1" :foreground "black"))
    (((background dark))
     (:background "green" :foreground "black"))
    (((background light)) :background "#d1f5ea")
    (((min-colors 88))
     (:background "green1"))
    (t (:background "green")))
  "Face for visible mark."
  :group 'helm-faces)

(defface helm-header
  '((t (:inherit header-line)))
  "Face for header lines in the helm buffer."
  :group 'helm-faces)

(defface helm-candidate-number
  '((((background dark)) :background "Yellow" :foreground "black")
    (((background light)) :background "#faffb5" :foreground "black"))
  "Face for candidate number in mode-line."
  :group 'helm-faces)

(defface helm-candidate-number-suspended
  '((t (:inherit helm-candidate-number :inverse-video t)))
  "Face for candidate number in mode-line when helm is suspended."
  :group 'helm-faces)

(defface helm-selection
  '((((background dark)) :background "ForestGreen"
     :distant-foreground "black")
    (((background light)) :background "#b5ffd1"
     :distant-foreground "black"))
  "Face for currently selected item in the helm buffer."
  :group 'helm-faces)

(defface helm-separator
  '((((background dark)) :foreground "red")
    (((background light)) :foreground "#ffbfb5"))
  "Face for multiline source separator."
  :group 'helm-faces)

(defface helm-action
  '((t (:underline t)))
  "Face for action lines in the helm action buffer."
  :group 'helm-faces)

(defface helm-prefarg
  '((((background dark)) :foreground "green")
    (((background light)) :foreground "red"))
  "Face for showing prefix arg in mode-line."
  :group 'helm-faces)

(defface helm-match
  '((((background light)) :foreground "#b00000")
    (((background dark))  :foreground "gold1"))
  "Face used to highlight matches."
  :group 'helm-faces)

(defface helm-header-line-left-margin
  '((t (:foreground "black" :background "yellow")))
  "Face used to highlight helm-header sign in left-margin."
  :group 'helm-faces)


;;; Variables.
;;
;;
(defvar helm-selection-overlay nil
  "Overlay used to highlight the currently selected item.")

(defvar helm-async-processes nil
  "List of information about asynchronous processes managed by helm.")

(defvar helm-before-initialize-hook nil
  "Runs before helm initialization.
This hook runs before init functions in `helm-sources', which is
before creation of `helm-buffer'. Set local variables for
`helm-buffer' that need a value from `current-buffer' with
`helm-set-local-variable'.")

(defvar helm-after-initialize-hook nil
  "Runs after helm initialization.
This hook runs after `helm-buffer' is created but not from
`helm-buffer'. The hook needs to specify in which buffer to run.")

(defvaralias 'helm-update-hook 'helm-after-update-hook)
(make-obsolete-variable 'helm-update-hook 'helm-after-update-hook "1.9.9")

(defvar helm-after-update-hook nil
  "Runs after updating the helm buffer with the new input pattern.")

(defvar helm-cleanup-hook nil
  "Runs after exiting the minibuffer and before performing an
action.

This hook runs even if helm exits the minibuffer abnormally (e.g.
via `helm-keyboard-quit').")

(defvar helm-select-action-hook nil
  "Runs when opening the action buffer.")

(defvar helm-before-action-hook nil
  "Runs before executing action.
Unlike `helm-cleanup-hook', this hook runs before helm closes the
minibuffer and also before performing an action.")

(defvar helm-after-action-hook nil
  "Runs after executing action.")

(defvar helm-exit-minibuffer-hook nil
  "Runs just before exiting the minibuffer.

This hook runs when helm exits the minibuffer normally (e.g. via
candidate selection), but does NOT run if helm exits the
minibuffer abnormally (e.g. via `helm-keyboard-quit').")

(defvar helm-after-persistent-action-hook nil
  "Runs after executing persistent action.")

(defvar helm-move-selection-before-hook nil
  "Runs before moving selection in `helm-buffer'.")

(defvar helm-move-selection-after-hook nil
  "Runs after moving selection in `helm-buffer'.")

(defvar helm-after-preselection-hook nil
  "Runs after pre-selection in `helm-buffer'.")

(defvar helm-window-configuration-hook nil
  "Runs when switching to and from the action buffer.")

(defvar helm-execute-action-at-once-if-one nil
  "When non--nil executes the default action and then exits if only one candidate.
If symbol 'current-source is given as value exit if only one candidate
in current source.
This variable accepts a function with no args that should returns a boolean
value or 'current-source.")

(defvar helm-quit-if-no-candidate nil
  "When non-`nil', quits if there are no candidates.
This variable accepts a function.")

(defvar helm-debug-variables nil
  "A list of helm variables that `helm-debug-output' displays.
If `nil', `helm-debug-output' includes only variables with
`helm-' prefixes.")

(defvar helm-debug-buffer "*Debug Helm Log*")

(defvar helm-debug nil
  "If non-`nil', write log message to `helm-debug-buffer'.
Default is `nil', which disables writing log messages because the
size of `helm-debug-buffer' grows quickly.")

(defvar helm-mode-line-string "\
\\<helm-map>\
\\[helm-help]:Help \
\\[helm-select-action]:Act \
\\[helm-maybe-exit-minibuffer]/\
f1/f2/f-n:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend"
  "Help string displayed by helm in the mode-line.
It is either a string or a list of two string arguments where the
first string is the name and the second string is displayed in
the mode-line. When `nil', uses default `mode-line-format'.")

(defvar helm-minibuffer-set-up-hook nil
  "Hook that runs at minibuffer initialization.
A hook useful for modifying minibuffer settings in helm.

An example that hides the minibuffer when using
`helm-echo-input-in-header-line':

      (add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)

Note that we check `helm-echo-input-in-header-line' value
from `helm-buffer' which allow detecting possible local
value of this var.")

(defvar helm-help-message
  "* Helm Generic Help
** Basics

Helm allow you narrowing the list of candidates as the pattern is typed and
updates the list in a live feedback.

Helm accepts multiple patterns (entered with a space between patterns).
Helm support also fuzzy matching in some places when specified.

Helm uses familiar Emacs navigation keys to move up and down the list,
however some keybindings are maybe confusing for new users, here are some:

`\\[helm-maybe-exit-minibuffer]' selects the candidate from the list and execute default action
on it, exiting helm session.

`\\[helm-execute-persistent-action]' execute the default action but without exiting helm session,
it may be not available in some places.

`\\[helm-select-action]' show you a list of actions available on current candidate or all marked candidates,
this maybe surprising for new helm users that expect `\\[helm-select-action]' for completions and have not
realized they are already completing something as soon as helm is started!
See [[https://github.com/emacs-helm/helm/wiki#helm-completion-vs-emacs-completion][Helm wiki]]

NOTE: In addition to this fixed actions list, you will notice that depending
of the type of candidate selected you may have additional actions
appearing and disapearing when you select another type of candidate, they are called
filtered actions.

** Helm mode

`helm-mode' allows you enabling helm completion in native emacs functions,
so when you turn on `helm-mode' commands like e.g `switch-to-buffer' will use
helm completion instead of the usual emacs completion buffer.

*** What is helmized and not when `helm-mode' is enabled ?

Helm is providing completion on all functions in emacs using `completing-read'
and derived and `completion-in-region', it uses generic functions for this.

For the functions using `completing-read' and derived e.g `read-file-name' helm
have a user variable that allows controlling which function to use for a specific
emacs command, it is `helm-completing-read-handlers-alist', it allows also
disabling helm completion for a specific command when the specified
function is nil.
See its documentation for more infos.

*** Helm functions vs helmized emacs functions

Sometimes you have helm functions that do the same completion as other
emacs vanilla helmized functions, e.g `switch-to-buffer' and
`helm-buffers-list', you have to understand that the native helm
functions like `helm-buffers-list' can receive new features, allow
marking candidates, have several actions and much more whereas the
emacs vanilla helmized functions have only a helm completion, one
action and no more what emacs provide for this function, it is the
intended behavior.

So generally you have better time using the native helm command generally
much more featured than the emacs function helmized than `helm-mode'.

** Helm Help

\\[helm-documentation] Shows all helm documentations concatenated in one org file.

When you are in an helm session, just hit \\<helm-map>\\[helm-help] to have the documentation
for the current source followed for conveniences by the global helm documentation.

While in the help buffer, you have most of the regular keybindings
available in emacs buffers, the most important are shown in
minibuffer; However due to the implementation that do not use regular
emacs keymap (you are in a loop when reading help buffer) they are
hardcoded and not modifiable, here they are:

| Key       | Alternative keys | Command             |
|-----------+------------------+---------------------|
| C-v       | Space next       | Scroll up           |
| M-v       | b prior          | Scroll down         |
| C-s       |                  | Isearch forward     |
| C-r       |                  | Isearch backward    |
| C-a       |                  | Beginning of line   |
| C-e       |                  | End of line         |
| C-f       | right            | Forward char        |
| C-b       | left             | Backward char       |
| C-n       | down             | Next line           |
| C-p       | up               | Previous line       |
| M-a       |                  | Backward sentence   |
| M-e       |                  | Forward sentence    |
| M-f       |                  | Forward word        |
| M-b       |                  | Backward word       |
| M->       |                  | End of buffer       |
| M-<       |                  | Beginning of buffer |
| C-<SPACE> |                  | Toggle mark         |
| TAB       |                  | Org cycle           |
| M-<TAB>   |                  | Toggle visibility   |
| M-w       |                  | Copy region         |
| q         |                  | Quit                |

** Customize helm

Helm have a lot of user variables to configure it as you want,
you can use from any helm session \\<helm-map>\\[helm-customize-group] to jump to the current source group.
Helm have also a special group for faces you can access via M-x customize-group => helm-faces.

NOTE: Some sources may not have their group set and default to `helm' group.

** Helm's Basic Operations and Default Key Bindings

| Key     | Alternative Keys | Command                                                              |
|---------+------------------+----------------------------------------------------------------------|
| C-p     | Up               | Previous Line                                                        |
| C-n     | Down             | Next Line                                                            |
| M-v     | PageUp           | Previous Page                                                        |
| C-v     | PageDown         | Next Page                                                            |
| Enter   |                  | Execute first (default) action / Select                              |
| M-<     |                  | First Line                                                           |
| M->     |                  | Last Line                                                            |
| C-M-S-v | M-PageUp, C-M-y  | Previous Page (other-window)                                         |
| C-M-v   | M-PageDown       | Next Page (other-window)                                             |
| Tab     | C-i              | Show action list                                                     |
| Left    |                  | Previous Source                                                      |
| Right   | C-o              | Next Source                                                          |
| C-k     |                  | Delete pattern (with prefix arg delete from point to end or all [1]) |
| C-j     | C-z              | Persistent Action (Execute and keep helm session)                    |

\[1] Delete from point to end or all depending of
`helm-delete-minibuffer-contents-from-point' value.

** Shortcuts For nth Action

f1-12: Execute nth Action where n is 1 to 12.

** Shortcuts for executing Default Action on the nth candidate

C-x <n> => executes default action on number <n> candidate before currently selected candidate.

C-c <n> => executes default action on number <n> candidate after current selected candidate.

n is limited only to 1 through 9. For larger jumps use other
navigation keys. Also note that Helm candidates list by default
do not display line numbers. Line numbers can be enabled with the
\[[https://github.com/coldnew/linum-relative][linum-relative]] package and `helm-linum-relative-mode'.

** Using the mouse in helm

A basic usage of mouse is provided when user set `helm-allow-mouse' to non-nil.

- mouse-1 allows selecting candidate.
- mouse-2 execute default action on selected candidate.
- mouse-3 pops up menu action.

NOTE: When mouse usage is enabled in helm, it allow also clicking around and quit
the minibuffer focus, it will be up to you to click back to helm buffer or minibuffer
to retrieve control of your helm session.

** Marked candidates

You can mark candidates to execute an action on them instead
of the current selected candidate only (See binding below).
Most Helm actions operate on marked candidates unless marking candidates
is prevented explicitely for a specific source.

To mark/unmark a candidate use \\[helm-toggle-visible-mark] (See bindings below).
To mark all visible unmarked candidates at once in current source use \\[helm-mark-all].
To mark/unmark all candidates at once use \\[helm-toggle-all-marks].

NOTE: These two functions allow marking candidates in all sources with a prefix argument,
but even if you mark all candidates of all sources, only those of current source will be used
when executing your action unless this action specify to use candidates of all sources, which
is not the case in most sources for evident reasons
\(i.e Each action handle only a specific type of candidate).
IOW Unless you use specific sources that have actions handling candidates of all other sources
you don't need the prefix arg when using \\[helm-mark-all] or \\[helm-toggle-all-marks].

** Follow candidates

You can execute automatically an action specified in the source as
persistent-action while moving up and down in helm-window or while
updating the list of candidates by turning on `helm-follow-mode' while
in helm with \\<helm-map>\\[helm-follow-mode].  The follow behavior
will be saved and used in next emacs sessions when
`helm-follow-mode-persistent' is non-nil.

If you just want to follow candidates occasionally without enabling
`helm-follow-mode' you can use instead \\<helm-map>\\[helm-follow-action-forward] or \\[helm-follow-action-backward].
Note that when `helm-follow-mode' is enabled these commands are just
going to next/previous line without executing persistent action.

** Frequently Used Commands

\\[helm-toggle-resplit-and-swap-windows]\t\tToggle vertical/horizontal split on first hit and swap helm window on second hit.
\\[helm-quit-and-find-file]\t\tDrop into `helm-find-files'.
\\[helm-kill-selection-and-quit]\t\tKill display value of candidate and quit (with prefix arg, kill the real value).
\\[helm-yank-selection]\t\tYank current selection into pattern.
\\[helm-copy-to-buffer]\t\tCopy selected candidate at point in current-buffer.
\\[helm-follow-mode]\t\tToggle automatic execution of persistent action.
\\[helm-follow-action-forward]\tRun persistent action and then select next line.
\\[helm-follow-action-backward]\t\tRun persistent action and then select previous line.
\\[helm-refresh]\t\tRecalculate and redisplay candidates.
\\[helm-toggle-suspend-update]\t\tSuspend/reenable updates to candidates list.

** Moving in `helm-buffer'

You can move in `helm-buffer' with usual commands used in emacs
\(\\<helm-map>\\[helm-next-line], \\<helm-map>\\[helm-previous-line] etc... see above basic commands).
When `helm-buffer' contains more than one source change source with \\<helm-map>\\[helm-next-source].

NOTE: When at end of source \\<helm-map>\\[helm-next-line] will NOT go to next source if
variable `helm-move-to-line-cycle-in-source' is non--nil, so you will have to use \\<helm-map>\\[helm-next-source].

** Resume previous session from current helm session

You can use `C-c n' which is bound to `helm-run-cycle-resume' to cycle in resumables sources.
`C-c n' is a special key bound with `helm-define-key-with-subkeys' which allow you
to hit `C-c n' at first and then continue cycling with only `n'.
Tip: You can bound the same key in `global-map' to `helm-cycle-resume'
     with `helm-define-key-with-subkeys' to allow you cycling transparently
     from outside and inside helm session.
     You can also bind the cycling commands to single key pressed (e.g S-f1) this time
     with a simple `define-key' (note that S-f1 is not available in terminals).

NOTE: `helm-define-key-with-subkeys' is available only once helm is loaded.

You can also use  \\<helm-map>\\[helm-resume-previous-session-after-quit] to resume
the previous session before this one, or \\<helm-map>\\[helm-resume-list-buffers-after-quit]
to have completion on all resumables buffers.

** Global Commands

*** Resume helm session from outside helm

\\<global-map>\\[helm-resume] revives the last `helm' session.
Very useful for resuming previous Helm. Binding a key to this
command will greatly improve `helm' interactivity especially
after an accidental exit.
You can call  \\<global-map>\\[helm-resume] with a prefix arg to have completion on previous
sources used and resumables.
You can also cycle in these source with `helm-cycle-resume' (see above).

** Debugging helm

helm have a special variable called `helm-debug', setting it to non-nil
will allow helm logging in a special outline-mode buffer.
Helm is resetting the variable to nil at end of each session.

A convenient command is bound to \\<helm-map>\\[helm-enable-or-switch-to-debug]
and allow turning debugging to this session only.
To avoid accumulating log while you are typing your pattern, you can use
\\<helm-map>\\[helm-toggle-suspend-update] to turn off updating, then when you
are ready turn it on again to start updating.

Once you exit your helm session you can access the debug buffer with `helm-debug-open-last-log'.
It is possible to save logs to dated files when `helm-debug-root-directory'
is set to a valid directory.

NOTE: Be aware that helm log buffers grow really fast, so use `helm-debug' only when needed.

** Writing your own helm sources

It is easy writing simple sources for your own usage.
Basically in a call to `helm' function, the sources are added as a
single source which can be a symbol or a list of sources in the :sources slot.
Sources can be builded with different eieio classes depending
what you want to do, for simplifying this several `helm-build-*' macros are provided.
We will not go further here, see [[https://github.com/emacs-helm/helm/wiki/Developing][Helm wiki]] for more infos.
Below simple examples to start with.

#+begin_src elisp

    ;; Candidates are stored in a list.
    (helm :sources (helm-build-sync-source \"test\"
                     ;; A function can be used as well
                     ;; to provide candidates.
                     :candidates '(\"foo\" \"bar\" \"baz\"))
          :buffer \"*helm test*\")

    ;; Candidates are stored in a buffer.
    ;; Generally faster but doesn't allow a dynamic updating
    ;; of the candidates list i.e the list is fixed on start.
    (helm :sources (helm-build-in-buffer-source \"test\"
                     :data '(\"foo\" \"bar\" \"baz\"))
          :buffer \"*helm test*\")

#+end_src

For more complex sources, See [[https://github.com/emacs-helm/helm/wiki/Developing][Helm wiki]]
and the many examples you will find in helm source code.

** Helm Map
\\{helm-map}"
  "Message string containing detailed help for `helm'.
It also accepts function or variable symbol.")

(defvar helm-autoresize-mode) ;; Undefined in `helm-default-display-buffer'.

(defvar helm-async-outer-limit-hook nil
  "A hook that run in async sources when process output comes out of `candidate-number-limit'.
Should be set locally to `helm-buffer' with `helm-set-local-variable'.")

(defvar helm-quit-hook nil
  "A hook that run when quitting helm.")

;;; Internal Variables
;;
;;
(defvar helm-source-filter nil
  "A list of source names to be displayed.
Other sources won't appear in the search results.
If nil, no filtering is done.
Don't set this directly, use `helm-set-source-filter' during helm session
to modify it.")
(defvar helm-current-prefix-arg nil
  "Record `current-prefix-arg' when exiting minibuffer.")
(defvar helm-saved-action nil
  "Saved value of the currently selected action by key.")
(defvar helm-saved-current-source nil
  "Value of the current source when the action list is shown.")
(defvar helm-compiled-sources nil
  "Compiled version of `helm-sources'.")
(defvar helm-in-persistent-action nil
  "Flag whether in persistent-action or not.")
(defvar helm-last-buffer nil
  "`helm-buffer' of previously `helm' session.")
(defvar helm-saved-selection nil
  "Value of the currently selected object when the action list is shown.")
(defvar helm-sources nil
  "[INTERNAL] Value of current sources in use, a list.")
(defvar helm-buffer-file-name nil
  "Variable `buffer-file-name' when `helm' is invoked.")
(defvar helm-candidate-cache (make-hash-table :test 'equal)
  "Holds the available candidate within a single helm invocation.")
(defvar helm--candidate-buffer-alist nil)
(defvar helm-input ""
  "The input typed in the candidates panel.")
(defvar helm-input-local nil
  "Internal, store locally `helm-pattern' value for later use in `helm-resume'.")
(defvar helm--source-name nil)
(defvar helm-current-source nil)
(defvar helm-tick-hash (make-hash-table :test 'equal))
(defvar helm-issued-errors nil)
(defvar helm--last-log-file nil
  "The name of the log file of the last helm session.")
(defvar helm--local-variables nil)
(defvar helm-split-window-state nil)
(defvar helm--window-side-state nil)
(defvar helm-selection-point nil)
(defvar helm-alive-p nil)
(defvar helm-visible-mark-overlays nil)
(defvar helm-update-blacklist-regexps '("^" "^ *" "$" "!" " " "\\b"
                                        "\\<" "\\>" "\\_<" "\\_>" ".*"
                                        "??" "?*" "*?" "?"))
(defvar helm--force-updating-p nil
  "[INTERNAL] Don't use this in your programs.")
(defvar helm-exit-status 0
  "Flag to inform if helm did exit or quit.
0 means helm did exit when executing an action.
1 means helm did quit with \\[keyboard-quit]
Knowing this exit-status could help restore a window config when helm aborts
in some special circumstances.
See `helm-exit-minibuffer' and `helm-keyboard-quit'.")
(defvar helm-minibuffer-confirm-state nil)
(defvar helm-quit nil)
(defvar helm-buffers nil
  "Helm buffers listed in order of most recently used.")
(defvar helm-current-position nil
  "Cons of \(point . window-start\)  when `helm' is invoked.
`helm-current-buffer' uses this to restore position after
`helm-keyboard-quit'")
(defvar helm-last-frame-or-window-configuration nil
  "Used to store window or frame configuration at helm start.")
(defvar helm-onewindow-p nil)
(defvar helm-types nil)
(defvar helm--mode-line-string-real nil) ; The string to display in mode-line.
(defvar helm-persistent-action-display-window nil)
(defvar helm-marked-candidates nil
  "Marked candadates.  List of \(source . real\) pair.")
(defvar helm--mode-line-display-prefarg nil)
(defvar helm--temp-follow-flag nil
  "[INTERNAL] A simple flag to notify persistent action we are following.")
(defvar helm--reading-passwd-or-string nil)
(defvar helm--in-update nil)
(defvar helm--in-fuzzy nil)
(defvar helm--maybe-use-default-as-input nil
  "Flag to notify the use of use-default-as-input.
Use only in let-bindings.
Use :default arg of `helm' as input to update display.
Note that if also :input is specified as `helm' arg, it will take
precedence on :default.")
(defvar helm--temp-hooks nil
  "Store temporary hooks added by `with-helm-temp-hook'.")
(defvar helm-truncate-lines nil
  "[Internal] Don't set this globally, it is used as a local var.")
(defvar helm--prompt nil)
(defvar helm--file-completion-sources
  '("Find Files" "Read File Name")
  "Sources that use the *find-files mechanism can be added here.
Sources generated by `helm-mode' don't need to be added here
because they are automatically added.

You should not modify this yourself unless you know what you are doing.")
(defvar helm--completing-file-name nil
  "Non nil when `helm-read-file-name' is running.
Used for `helm-file-completion-source-p'.")
;; Same as `ffap-url-regexp' but keep it here to ensure `ffap-url-regexp' is not nil.
(defvar helm--url-regexp "\\`\\(news\\(post\\)?:\\|mailto:\\|file:\\|\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://\\)")
(defvar helm--ignore-errors nil
  "Flag to prevent helm popping up errors in candidates functions.
Should be set in candidates functions if needed, will be restored
at end of session.")
(defvar helm--action-prompt "Select action: ")
(defvar helm--cycle-resume-iterator nil)

;; Utility: logging
(defun helm-log (format-string &rest args)
  "Log message `helm-debug' is non-`nil'.
Messages are written to the `helm-debug-buffer' buffer.

Argument FORMAT-STRING is a string to use with `format'.
Use optional arguments ARGS like in `format'."
  (when helm-debug
    (with-current-buffer (get-buffer-create helm-debug-buffer)
      (outline-mode)
      (buffer-disable-undo)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (let ((tm (current-time)))
                  (format (concat (if (string-match "Start session" format-string)
                                      "* " "** ")
                                  "%s.%06d (%s)\n %s\n")
                          (format-time-string "%H:%M:%S" tm)
                          (nth 2 tm)
                          (helm-log-get-current-function)
                          (apply #'format (cons format-string args)))))))))

(defun helm-log-run-hook (hook)
  "Run HOOK like `run-hooks' but write these actions to helm log buffer."
  (helm-log "Executing %s with value = %S" hook (symbol-value hook))
  (helm-log "Executing %s with global value = %S" hook (default-value hook))
  (run-hooks hook)
  (helm-log "executed %s" hook))

(defun helm-log-get-current-function ()
  "Get name of function that is calling `helm-log'.
The original idea is from `tramp-debug-message'."
  (cl-loop with exclude-func-re = "^helm-\\(?:interpret\\|log\\|.*funcall\\)"
           for btn from 1 to 40
           for btf = (cl-second (backtrace-frame btn))
           for fn  = (if (symbolp btf) (symbol-name btf) "")
           if (and (string-match "^helm" fn)
                   (not (string-match exclude-func-re fn)))
           return fn))

(defun helm-log-error (&rest args)
  "Accumulate error messages into `helm-issued-errors'.
ARGS are args given to `format'.
e.g (helm-log-error \"Error %s: %s\" (car err) (cdr err))."
  (apply 'helm-log (concat "ERROR: " (car args)) (cdr args))
  (let ((msg (apply 'format args)))
    (unless (member msg helm-issued-errors)
      (cl-pushnew msg helm-issued-errors :test 'equal))))

(defun helm-log-save-maybe ()
  "Save log buffer if `helm-debug-root-directory' is set to a valid directory.
Messages are logged to a file named with todays date and time in this directory."
  (when (and (stringp helm-debug-root-directory)
             (file-directory-p helm-debug-root-directory)
             helm-debug)
    (let ((logdir (expand-file-name (concat "helm-debug-"
                                            (format-time-string "%Y%m%d"))
                                    helm-debug-root-directory)))
      (make-directory logdir t)
      (with-current-buffer (get-buffer-create helm-debug-buffer)
        (write-region (point-min) (point-max)
                      (setq helm--last-log-file
                            (expand-file-name
                             (format-time-string "%Y%m%d-%H%M%S")
                             logdir))
                      nil 'silent)
        (kill-buffer))))
  (setq helm-debug nil))

;;;###autoload
(defun helm-debug-open-last-log ()
  "Open helm log file or buffer of last helm session."
  (interactive)
  (if helm--last-log-file
      (progn
        (find-file helm--last-log-file)
        (outline-mode) (view-mode 1) (visual-line-mode 1))
    (switch-to-buffer helm-debug-buffer)
    (view-mode 1) (visual-line-mode 1)))

(defun helm-print-error-messages ()
  "Print error messages in `helm-issued-errors'."
  (and helm-issued-errors
       (message "Helm issued errors: %s"
                (mapconcat 'identity (reverse helm-issued-errors) "\n"))))


;; Test tools
(defmacro with-helm-time-after-update (&rest body)
  (helm-with-gensyms (start-time time-elapsed)
    `(let ((,start-time (float-time)) ,time-elapsed)
       (add-hook 'helm-after-update-hook
                 (lambda ()
                   (setq ,time-elapsed (- (float-time) ,start-time))
                   (keyboard-quit)))
       (unwind-protect ,@body
         (remove-hook 'helm-after-update-hook
                      (lambda ()
                        (setq  ,time-elapsed (- (float-time) ,start-time))
                        (keyboard-quit))))
       ,time-elapsed)))


;; Helm API
(defmacro with-helm-default-directory (directory &rest body)
  (declare (indent 2) (debug t))
  `(let ((default-directory (or (and ,directory
                                     (file-name-as-directory ,directory))
                                default-directory)))
     ,@body))

(defun helm-default-directory ()
  "Return the local value of `default-directory' in `helm-buffer'."
  (buffer-local-value 'default-directory (get-buffer helm-buffer)))

(defmacro with-helm-temp-hook (hook &rest body)
  "Execute temporarily BODY as a function for HOOK."
  (declare (indent 1) (debug t))
  (helm-with-gensyms (helm--hook)
    `(progn
       (defun ,helm--hook ()
         (unwind-protect
             (progn ,@body)
           (remove-hook ,hook (quote ,helm--hook))
           (fmakunbound (quote ,helm--hook))))
       (push (cons ',helm--hook ,hook) helm--temp-hooks)
       (add-hook ,hook (quote ,helm--hook)))))

(defmacro with-helm-after-update-hook (&rest body)
  "Execute BODY at end of `helm-update'."
  (declare (indent 0) (debug t))
  `(with-helm-temp-hook 'helm-after-update-hook ,@body))

(defmacro with-helm-alive-p (&rest body)
  "Return error when BODY run outside helm context."
  (declare (indent 0) (debug t))
  `(progn
     (if helm-alive-p
         (progn ,@body)
       (error "Running helm command outside of context"))))


;;; helm-attributes
;;
(defun helm-attr (attribute-name &optional source compute)
  "Get the value of ATTRIBUTE-NAME of SRC.

If SRC is omitted, use current source.

If COMPUTE is non-`nil' compute value of ATTRIBUTE-NAME with
`helm-interpret-value'.  COMPUTE can have also 'ignorefn as value, in
this case `helm-interpret-value' will return a function as value
unchanged, but will eval a symbol which is bound.

You can use `setf' to modify value of ATTRIBUTE-NAME unless COMPUTE is
specified, if attribute ATTRIBUTE-NAME is not found in SOURCE `setf'
will create new attribute ATTRIBUTE-NAME with specified value.
You can also use `helm-attrset' to modify ATTRIBUTE-NAME."
  (declare (gv-setter
            (lambda (val)
              `(let* ((src (or ,source (helm-get-current-source)))
                      (attr (assq ,attribute-name src)))
                 (cl-assert (null ,compute) nil
                            "`setf' can't set the computed value of attribute `%s'"
                            ,attribute-name)
                 (if attr
                     (setcdr attr ,val)
                   (and (setcdr src (cons (cons ,attribute-name ,val)
                                          (cdr src)))
                        ,val))))))
  (let ((src (or source (helm-get-current-source))))
    (helm-aif (assq attribute-name src)
        (if compute
            (helm-interpret-value (cdr it) src compute)
          (cdr it)))))

(cl-defun helm-attrset (attribute-name value
                                       &optional
                                       (src (helm-get-current-source)))
  "Set the value of ATTRIBUTE-NAME of source SRC to VALUE.

If ATTRIBUTE-NAME doesn't exists in source it is created with value VALUE..
If SRC is omitted, use current source.
If operation succeed, return value, otherwise nil.

Note that `setf' on `helm-attr' can be used instead of this function."
  (setf (helm-attr attribute-name src) value))

(defun helm-add-action-to-source (name fn source &optional index)
  "Add new action NAME linked to function FN to SOURCE.
Function FN should be a valid function that takes one arg i.e candidate,
argument NAME is a string that will appear in action menu
and SOURCE should be an existing helm source already loaded.
If INDEX is specified, action is added to the action list at INDEX,
otherwise added at end.
This allows users to add specific actions to an existing source
without modifying source code."
  (let ((actions    (helm-attr 'action source 'ignorefn))
        (new-action (list (cons name fn))))
    (when (functionp actions)
      (setq actions (list (cons "Default action" actions))))
    (helm-attrset 'action
                  (if index
                      (helm-append-at-nth actions new-action index)
                    (append actions new-action))
                  source)))

(defun helm-delete-action-from-source (action-or-name source)
  "Delete ACTION-OR-NAME from SOURCE.
ACTION-OR-NAME can either be the name of action or the symbol function
associated to name."
  (let* ((actions    (helm-attr 'action source 'ignorefn))
         (del-action (if (symbolp action-or-name)
                         (rassoc action-or-name actions)
                       (assoc action-or-name actions))))
    (helm-attrset 'action (delete del-action actions) source)))

(cl-defun helm-add-action-to-source-if (name fn source predicate
                                             &optional (index 4) test-only)
  "Add new action NAME linked to function FN to SOURCE.
Action NAME will be available when the current candidate matches PREDICATE.
This function adds an entry in the `action-transformer' attribute
of SOURCE (or creates one if not found).
Function PREDICATE must take one candidate as arg.
Function FN should be a valid function that takes one arg i.e. candidate,
argument NAME is a string that will appear in action menu
and SOURCE should be an existing helm source already loaded.
If INDEX is specified, action is added in action list at INDEX.
Value of INDEX should be always >=1, default to 4.
This allow user to add a specific `action-transformer'
to an existing source without modifying source code.
E.g
Add the action \"Byte compile file async\" linked to
function 'async-byte-compile-file to source `helm-source-find-files'
only when predicate helm-ff-candidates-lisp-p return non-`nil':

\(helm-add-action-to-source-if \"Byte compile file async\"
                              'async-byte-compile-file
                              helm-source-find-files
                              'helm-ff-candidates-lisp-p\)."
  (let* ((actions     (helm-attr 'action source 'ignorefn))
         (action-transformers (helm-attr 'action-transformer source))
         (new-action  (list (cons name fn)))
         (transformer (lambda (actions candidate)
                        (cond ((funcall predicate candidate)
                               (helm-append-at-nth
                                actions new-action index))
                              (t actions)))))
    (when (functionp actions)
      (helm-attrset 'action (list (cons "Default action" actions)) source))
    (when (or (symbolp action-transformers) (functionp action-transformers))
      (setq action-transformers (list action-transformers)))
    (if test-only                       ; debug
        (delq nil (append (list transformer) action-transformers))
      (helm-attrset 'action-transformer
                    (helm-fast-remove-dups
                     (delq nil (append (list transformer) action-transformers))
                     :test 'equal)
                    source))))


;;; Source filter
;;
(defun helm-set-source-filter (sources)
  "Set the value of `helm-source-filter' to SOURCES and update.

This function sets a filter for helm sources and it may be
called while helm is running. It can be used to toggle
displaying of sources dynamically. For example, additional keys
can be bound into `helm-map' to display only the file-related
results if there are too many matches from other sources and
you're after files only:

Shift+F shows only file results from some sources:

\(define-key helm-map \"F\" 'helm-my-show-files-only)

\(defun helm-my-show-files-only ()
  (interactive)
  (helm-set-source-filter '(\"File Name History\"
                                  \"Files from Current Directory\")))

Shift+A shows all results:

\(define-key helm-map \"A\" 'helm-my-show-all)

\(defun helm-my-show-all ()
  (interactive)
  (helm-set-source-filter nil))

The -my- part is added to avoid collisions with
existing Helm function names."
  (with-helm-buffer
    (let ((cur-disp-sel (helm-get-selection nil t)))
      (set (make-local-variable 'helm-source-filter)
           (helm--normalize-filter-sources sources))
      (helm-log "helm-source-filter = %S" helm-source-filter)
      ;; Use force-update to run init/update functions.
      (helm-force-update (and (stringp cur-disp-sel)
                              (regexp-quote cur-disp-sel))))))

(defun helm--normalize-filter-sources (sources)
  (cl-loop for s in sources collect
           (cl-typecase s
             (symbol (assoc-default 'name (symbol-value s)))
             (list   (assoc-default 'name s))
             (string s))))

(defun helm-set-sources (sources &optional no-init no-update)
  "Set SOURCES during `helm' invocation.
If NO-INIT is non-`nil', skip executing init functions of SOURCES.
If NO-UPDATE is non-`nil', skip executing `helm-update'."
  (with-current-buffer helm-buffer
    (setq helm-compiled-sources nil
          helm-sources sources)
    (helm-log "helm-compiled-sources = %S" helm-compiled-sources)
    (helm-log "helm-sources = %S" helm-sources))
  (unless no-init (helm-funcall-foreach 'init))
  (unless no-update (helm-update)))

(defun helm-get-sources ()
  "Return compiled `helm-sources', which is memoized."
  (cond
    ;; action
    ((helm-action-window) helm-sources)
    ;; memoized
    (helm-compiled-sources)
    ;; first time
    (t
     (prog1
         (setq helm-compiled-sources
               (mapcar (lambda (source)
                         (if (listp source) source (symbol-value source)))
                       helm-sources))
       (helm-log "helm-compiled-sources = %S" helm-compiled-sources)))))

(defun helm-get-selection (&optional buffer force-display-part source)
  "Return the currently selected item or nil.
if BUFFER is nil or unspecified, use helm-buffer as default value.
If FORCE-DISPLAY-PART is non-`nil', return the display string.
If FORCE-DISPLAY-PART value is 'withprop the display string is returned
with its properties."
  (setq buffer (or buffer helm-buffer))
  (unless (helm-empty-buffer-p buffer)
    (with-current-buffer buffer
      (let* ((disp-fn (if (eq force-display-part 'withprop)
                          'buffer-substring
                        'buffer-substring-no-properties))
             (selection
              (or (and (not force-display-part)
                       (get-text-property (overlay-start
                                           helm-selection-overlay)
                                          'helm-realvalue))
                  ;; It is needed to return properties of DISP in some case,
                  ;; e.g for `helm-confirm-and-exit-minibuffer',
                  ;; so use `buffer-substring' here when 'withprop is specified.
                  (let ((disp (funcall
                               disp-fn
                               (overlay-start helm-selection-overlay)
                               (1- (overlay-end helm-selection-overlay))))
                        (src (or source (helm-get-current-source))))
                    (helm-aif (and (not force-display-part)
                                   (assoc-default 'display-to-real src))
                        (helm-funcall-with-source source it disp)
                      disp)))))
        (unless (equal selection "")
          (helm-log "selection = %S" selection)
          selection)))))

(defun helm-get-actions-from-current-source (&optional source)
  "Return the associated action for the selected candidate.
It is a function symbol \(sole action\) or list
of \(action-display . function\)."
  (unless (helm-empty-buffer-p (helm-buffer-get))
    (let ((src (helm-get-current-source)))
      (helm-aif (helm-attr 'action-transformer)
          (helm-funcall-with-source
           (or source src) it
           (helm-attr 'action nil 'ignorefn)
           ;; Check if the first given transformer
           ;; returns the same set of actions for each
           ;; candidate in marked candidates.
           ;; If so use the car of marked to determine
           ;; the set of actions, otherwise use the selection.
           (if (cl-loop with marked = (helm-marked-candidates)
                        with act = (car (helm-mklist it))
                        with acts = (funcall act nil (car marked))
                        for c in marked
                        always (equal (funcall act nil c) acts))
               (car (helm-marked-candidates))
               (helm-get-selection nil nil src)))
        (helm-attr 'action nil 'ignorefn)))))

(defun helm-get-current-source ()
  "Return the source for the current selection.
Return nil when `helm-buffer' is empty."
  (or helm-current-source
      (with-helm-buffer
        (or (get-text-property (point) 'helm-cur-source)
            (progn
              ;; This is needed to not loose selection.
              (goto-char (overlay-start helm-selection-overlay))
              (let ((header-pos (or (helm-get-previous-header-pos)
                                    (helm-get-next-header-pos))))
                ;; Return nil when no--candidates.
                (when header-pos
                  (cl-loop with source-name = (save-excursion
                                                (goto-char header-pos)
                                                (helm-current-line-contents))
                           for source in (helm-get-sources) thereis
                           (and (equal (assoc-default 'name source) source-name)
                                source)))))))))

(defun helm-buffer-is-modified (buffer)
  "Return non-`nil' when BUFFER is modified since `helm' was invoked."
  (let* ((buf         (get-buffer buffer))
         (key         (concat (buffer-name buf) "/" (helm-attr 'name)))
         (source-tick (or (gethash key helm-tick-hash) 0))
         (buffer-tick (buffer-chars-modified-tick buf))
         (modifiedp   (/= source-tick buffer-tick)))
    (puthash key buffer-tick helm-tick-hash)
    (helm-log "buffer = %S" buffer)
    (helm-log "modifiedp = %S" modifiedp)
    modifiedp))

(defun helm-current-buffer-is-modified ()
  "Check if `helm-current-buffer' is modified since `helm' was invoked."
  (helm-buffer-is-modified helm-current-buffer))

(defun helm-run-after-exit (function &rest args)
  "Execute FUNCTION with ARGS after exiting `helm'.
The action is to call FUNCTION with arguments ARGS.
Unlike `helm-exit-and-execute-action', this can be used
to call non--actions functions with any ARGS or no ARGS at all.

Use this on commands invoked from key-bindings, but not
on action functions invoked as action from the action menu,
i.e. functions called with RET."
  (helm-kill-async-processes)
  (helm-log "function = %S" function)
  (helm-log "args = %S" args)
  (helm-exit-and-execute-action
   (lambda (_candidate)
     (apply function args))))

(defun helm-exit-and-execute-action (action)
  "Exit current helm session and execute ACTION.
Argument ACTION is a function called with one arg (candidate)
and part of the actions of current source.

Use this on commands invoked from key-bindings, but not
on action functions invoked as action from the action menu,
i.e functions called with RET."
  (setq helm-saved-action action)
  (setq helm-saved-selection (or (helm-get-selection) ""))
  (helm-exit-minibuffer))

(defalias 'helm-run-after-quit 'helm-run-after-exit)
(make-obsolete 'helm-run-after-quit 'helm-run-after-exit "1.7.7")
(defalias 'helm-quit-and-execute-action 'helm-exit-and-execute-action)
(make-obsolete 'helm-quit-and-execute-action 'helm-exit-and-execute-action "1.7.7")

(defun helm-interpret-value (value &optional source compute)
  "Interpret VALUE as variable, function or literal and return it.
If VALUE is a function, call it with no arguments and return the value
unless COMPUTE value is 'ignorefn.
If SOURCE compute VALUE for this source.
If VALUE is a variable, return the value.
If VALUE is a symbol, but it is not a function or a variable, cause an error.
Otherwise, return VALUE itself."
  (cond ((and source (functionp value) (not (eq compute 'ignorefn)))
         (helm-funcall-with-source source value))
        ((and (functionp value) (not (eq compute 'ignorefn)))
         (funcall value))
        ((and (symbolp value) (boundp value))
         (symbol-value value))
        ((and (symbolp value) (not (functionp value)))
         (error
          "helm-interpret-value: Symbol must be a function or a variable"))
        (t
         value)))

(defun helm-set-local-variable (&rest args)
  "Bind each pair in ARGS locally to `helm-buffer'.

Use this to set local vars before calling helm.

When used from an init or update function
(i.e when `helm-force-update' is running) the variables are set
using `make-local-variable' within the `helm-buffer'.

Usage: helm-set-local-variable ([VAR VALUE]...)
Just like `setq' except that the vars are not set sequentially.
IOW Don't use VALUE of previous VAR to set the VALUE of next VAR.

\(fn VAR VALUE ...)"
  (if helm--force-updating-p
      (with-helm-buffer
        (cl-loop for i on args by #'cddr
                 do (set (make-local-variable (car i)) (cadr i))))
      (setq helm--local-variables
            (append (cl-loop for i on args by #'cddr
                             collect (cons (car i) (cadr i)))
                    helm--local-variables))))


;; API helper
(cl-defun helm-empty-buffer-p (&optional (buffer helm-buffer))
  "Check if BUFFER have candidates.
Default value for BUFFER is `helm-buffer'."
  (zerop (buffer-size (and buffer (get-buffer buffer)))))

(defun helm-empty-source-p ()
  "Check if current source contains candidates.
This could happen when for example the last element of a source
was deleted and the candidates list not updated."
  (when (helm-window)
    (with-helm-window
      (or (helm-empty-buffer-p)
          (and (helm-end-of-source-p)
               (eq (point-at-bol) (point-at-eol))
               (or
                (save-excursion
                  (forward-line -1)
                  (helm-pos-header-line-p))
                (bobp)))))))


;; Tools
;;
(defun helm-funcall-with-source (source functions &rest args)
  "From SOURCE apply FUNCTIONS on ARGS.
FUNCTIONS is either a symbol or a list of functions, each function being
applied on ARGS and called on the result of the precedent function.
Return the result of last function call."
  (let ((helm--source-name (assoc-default 'name source))
        (helm-current-source source)
        (funs (if (functionp functions) (list functions) functions)))
    (helm-log "helm--source-name = %S" helm--source-name)
    (helm-log "functions = %S" functions)
    (helm-log "args = %S" args)
    (cl-loop with result
             for fn in funs
             do (setq result (apply fn args))
             when (and args (cdr funs))
             ;; In filter functions, ARGS is a list of one or two elements where
             ;; the first element is the list of candidates and the second
             ;; a list containing the source.
             ;; When more than one fn, set the candidates list to what returns
             ;; this fn to compute the modified candidates with the next fn
             ;; and so on.
             do (setcar args result)
             finally return result)))

(defun helm-funcall-foreach (sym &optional sources)
  "Call the associated function(s) to SYM for each source if any."
  (let ((sources (or sources (helm-get-sources))))
    (cl-dolist (source sources)
      (helm-aif (assoc-default sym source)
          (helm-funcall-with-source source it)))))

(defun helm-normalize-sources (sources)
  "If SOURCES is only one source, make a list of one element."
  (cond ((or (and sources (symbolp sources))
             (and (listp sources) (assq 'name sources)))
         (list sources))
        (sources)
        (t helm-sources)))

(defun helm-get-candidate-number (&optional in-current-source)
  "Return candidates number in `helm-buffer'.
If IN-CURRENT-SOURCE is provided return number of candidates of current source
only."
  (with-helm-buffer
    (if (or (helm-empty-buffer-p)
            (helm-empty-source-p))
        0
        (save-excursion
          (helm-aif (and in-current-source (helm-get-previous-header-pos))
              (goto-char it)
            (goto-char (point-min)))
          (forward-line 1)
          (if (helm-pos-multiline-p)
              (cl-loop with count-multi = 1
                       while (and (not (if in-current-source
                                           (save-excursion
                                             (forward-line 2)
                                             (or (helm-pos-header-line-p) (eobp)))
                                           (eobp)))
                                  (search-forward helm-candidate-separator nil t))
                       do (cl-incf count-multi)
                       finally return count-multi)
              (cl-loop with ln = 0
                       while (not (if in-current-source
                                      (or (helm-pos-header-line-p) (eobp))
                                      (eobp)))
                       ;; Don't count empty lines maybe added by popup (#1370).
                       unless (or (eq (point-at-bol) (point-at-eol))
                                  (helm-pos-header-line-p))
                       do (cl-incf ln)
                       do (forward-line 1) finally return ln))))))

(defmacro with-helm-quittable (&rest body)
  "If an error occurs in execution of BODY, safely quit helm."
  (declare (indent 0) (debug t))
  `(condition-case _v
       (let (inhibit-quit)
         ,@body)
     (quit (setq quit-flag t)
           (setq helm-quit t)
           (exit-minibuffer)
           (keyboard-quit)
           ;; See comment about this in `with-local-quit'.
           (eval '(ignore nil)))))

;; Entry point
;; `:allow-nest' is not in this list because it is treated before.
(defconst helm-argument-keys
  '(:sources :input :prompt :resume
    :preselect :buffer :keymap :default :history))

;;;###autoload
(defun helm (&rest plist)
  "Main function to execute helm sources.

PLIST is a list like

\(:key1 val1 :key2 val2 ...\)

 or

\(&optional sources input prompt resume preselect
            buffer keymap default history allow-nest\).

** Keywords

Keywords supported:

- :sources
- :input
- :prompt
- :resume
- :preselect
- :buffer
- :keymap
- :default
- :history
- :allow-nest

Extra LOCAL-VARS keywords are supported, see the \"** Other
keywords\" section below.

Basic keywords are the following:

*** :sources

One of the following:

- List of sources
- Symbol whose value is a list of sources
- Alist representing a Helm source.
  - In this case the source has no name and is referenced in
    `helm-sources' as a whole alist.

*** :input

Initial input of minibuffer (temporary value of `helm-pattern')

*** :prompt

Minibuffer prompt. Default value is `helm--prompt'.

*** :resume

If t, allow resumption of the previous session of this Helm
command, skipping initialization.

If 'noresume, this instance of `helm' cannot be resumed.

*** :preselect

Initially selected candidate (string or regexp).

*** :buffer

Buffer name for this Helm session. `helm-buffer' will take this value.

*** :keymap

\[Obsolete]

Keymap used at the start of this Helm session.

It is overridden by keymaps specified in sources, and is kept
only for backward compatibility.

Keymaps should be specified in sources using the :keymap slot
instead. See `helm-source'.

This keymap is not restored by `helm-resume'.

*** :default

Default value inserted into the minibuffer \ with
\\<minibuffer-local-map>\\[next-history-element].

It can be a string or a list of strings, in this case
\\<minibuffer-local-map>\\[next-history-element] cycles through
the list items, starting with the first.

If nil, `thing-at-point' is used.

If `helm--maybe-use-default-as-input' is non-`nil', display is
updated using this value, unless :input is specified, in which
case that value is used instead.

*** :history

Minibuffer input, by default, is pushed to `minibuffer-history'.

When an argument HISTORY is provided, input is pushed to
HISTORY. HISTORY should be a valid symbol.

*** :allow-nest

Allow running this Helm command in a running Helm session.

** Other keywords

Other keywords are interpreted as local variables of this Helm
session. The `helm-' prefix can be omitted. For example,

\(helm :sources 'helm-source-buffers-list
       :buffer \"*helm buffers*\"
       :candidate-number-limit 10\)

starts a Helm session with the variable
`helm-candidate-number-limit' set to 10.

** Backward compatibility

For backward compatibility, positional parameters are
supported:

\(helm sources input prompt resume preselect
       buffer keymap default history allow-nest\)

However, the use of non-keyword args is deprecated.

\(fn &key SOURCES INPUT PROMPT RESUME PRESELECT BUFFER KEYMAP DEFAULT HISTORY ALLOW-NEST OTHER-LOCAL-VARS)"
  (let ((fn (cond ((or (and helm-alive-p (plist-get plist :allow-nest))
                       (and helm-alive-p (memq 'allow-nest plist)))
                   #'helm--nest)
                  ((keywordp (car plist))
                   #'helm)
                  (t #'helm-internal))))
    (if (and helm-alive-p (eq fn #'helm))
        (if (helm--alive-p)
            ;; A helm session is normally running.
            (error "Error: Trying to run helm within a running helm session")
          ;; A helm session is already running and user jump somewhere else
          ;; without deactivating it.
          (with-helm-buffer
            (prog1
                (message "Aborting an helm session running in background")
              ;; `helm-alive-p' will be reset in unwind-protect forms.
              (helm-keyboard-quit))))
      (if (keywordp (car plist))
          ;; Parse `plist' and move not regular `helm-argument-keys'
          ;; to `helm--local-variables', then calling helm on itself
          ;; with normal arguments (the non--arguments-keys removed)
          ;; will end up in [1].
          (progn
            (setq helm--local-variables
                  (append helm--local-variables
                          ;; Vars passed by keyword on helm call
                          ;; take precedence on same vars
                          ;; that may have been passed before helm call.
                          (helm-parse-keys plist)))
            (apply fn (mapcar (lambda (key) (plist-get plist key))
                              helm-argument-keys)))
        (apply fn plist))))) ; [1] fn == helm-internal.

(defun helm--alive-p ()
  "[Internal] Check if `helm' is alive.
An `helm' session is considered alive if `helm-alive-p' value is
non-`nil', the `helm-buffer' is visible, and cursor is in the
minibuffer."
  (and helm-alive-p
       (get-buffer-window (helm-buffer-get) 'visible)
       (minibuffer-window-active-p (minibuffer-window))
       (minibufferp (current-buffer))))

(defun helm-parse-keys (keys)
  "Parse the KEYS arguments of `helm'.
Return only those keys not in `helm-argument-keys', prefix them
with \"helm\", and then convert them to an alist. This allows
adding arguments that are not part of `helm-argument-keys', but
are valid helm variables nevertheless. For
example, :candidate-number-limit is bound to
`helm-candidate-number-limit' in the source.

  (helm-parse-keys '(:sources ((name . \"test\")
                               (candidates . (a b c)))
                     :buffer \"toto\"
                     :candidate-number-limit 4))
  ==> ((helm-candidate-number-limit . 4))."

  (cl-loop for (key value) on keys by #'cddr
           for symname = (substring (symbol-name key) 1)
           for sym = (intern (if (string-match "^helm-" symname)
                                 symname
                               (concat "helm-" symname)))
           unless (memq key helm-argument-keys)
           collect (cons sym value)))

;;; Entry point helper
(defun helm-internal (&optional
                        any-sources any-input
                        any-prompt any-resume
                        any-preselect any-buffer
                        any-keymap any-default any-history)
  "The internal helm function called by `helm'.
For ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT ANY-BUFFER and
ANY-KEYMAP ANY-DEFAULT ANY-HISTORY See `helm'."
  ;; Activate the advice for `tramp-read-passwd' and cua.
  ;; Advices will be available only in >=emacs-24.4, but
  ;; allow compiling without errors on lower emacs.
  (when (fboundp 'advice-add)
    (advice-add 'tramp-read-passwd :around #'helm--advice-tramp-read-passwd)
    (advice-add 'ange-ftp-get-passwd :around #'helm--advice-ange-ftp-get-passwd)
    (advice-add 'cua-delete-region :around #'cua-delete-region--advice)
    (advice-add 'copy-region-as-kill :around #'copy-region-as-kill--advice))
  (helm-log (concat "[Start session] " (make-string 41 ?+)))
  (helm-log "any-prompt = %S" any-prompt)
  (helm-log "any-preselect = %S" any-preselect)
  (helm-log "any-buffer = %S" any-buffer)
  (helm-log "any-keymap = %S" any-keymap)
  (helm-log "any-default = %S" any-default)
  (helm-log "any-history = %S" any-history)
  (setq helm--prompt (or any-prompt "pattern: "))
  (let ((non-essential t)
        ;; Prevent mouse jumping to the upper-right
        ;; hand corner of the frame (#1538).
        mouse-autoselect-window
        focus-follows-mouse
        mode-line-in-non-selected-windows
        (input-method-verbose-flag helm-input-method-verbose-flag)
        (old--cua cua-mode)
        (helm--maybe-use-default-as-input
         (and (null any-input)
              (or helm--maybe-use-default-as-input ; it is let-bounded so use it.
                  (cl-loop for s in (helm-normalize-sources any-sources)
                           thereis (memq s helm-sources-using-default-as-input))))))
    ;; cua-mode override local helm bindings.
    ;; disable this stupid thing if enabled.
    (and cua-mode (cua-mode -1))
    (unwind-protect
         (condition-case-unless-debug _v
             (let ( ;; `helm--source-name' is non-`nil'
                   ;; when `helm' is invoked by action, reset it.
                   helm--source-name
                   helm-current-source
                   helm-in-persistent-action
                   helm-quit
                   (helm-buffer (or any-buffer helm-buffer)))
               (helm-initialize
                any-resume any-input any-default any-sources)
               (helm-display-buffer helm-buffer)
               (select-window (helm-window))
               ;; We are now in helm-buffer.
               (unless helm-allow-mouse
                 (helm--remap-mouse-mode 1)) ; Disable mouse bindings.
               (add-hook 'post-command-hook 'helm--maybe-update-keymap)
               ;; Add also to update hook otherwise keymap is not updated
               ;; until a key is hitted (Issue #1670).
               (add-hook 'helm-after-update-hook 'helm--maybe-update-keymap)
               (add-hook 'post-command-hook 'helm--update-header-line)
               (helm-log "show prompt")
               (unwind-protect
                    (helm-read-pattern-maybe
                     any-prompt any-input any-preselect
                     any-resume any-keymap any-default any-history)
                 (helm-cleanup))
               (prog1
                   (unless helm-quit (helm-execute-selection-action))
                 (helm-log (concat "[End session] " (make-string 41 ?-)))))
           (quit
            (helm-restore-position-on-quit)
            (helm-log-run-hook 'helm-quit-hook)
            (helm-log (concat "[End session (quit)] " (make-string 34 ?-)))
            nil))
      (when (fboundp 'advice-remove)
        (advice-remove 'tramp-read-passwd #'helm--advice-tramp-read-passwd)
        (advice-remove 'ange-ftp-get-passwd #'helm--advice-ange-ftp-get-passwd)
        (advice-remove 'cua-delete-region #'cua-delete-region--advice)
        (advice-remove 'copy-region-as-kill #'copy-region-as-kill--advice))
      (helm-log "helm-alive-p = %S" (setq helm-alive-p nil))
      (helm--remap-mouse-mode -1)       ; Reenable mouse bindings.
      (setq helm-alive-p nil)
      ;; Reset helm-pattern so that lambda's using it
      ;; before running helm will not start with its old value.
      (setq helm-pattern "")
      (setq helm-sources nil)
      (setq helm--ignore-errors nil)
      (and old--cua (cua-mode 1))
      (helm-log-save-maybe))))


;;; Helm resume
;;
;;
(defun helm-resume (arg)
  "Resume a previous `helm' session.
Call with a prefix arg to choose among existing helm
buffers (sessions). When calling from lisp, specify a buffer-name
as a string with ARG."
  (interactive "P")
  (let (any-buffer
        cur-dir
        (helm-full-frame (default-value 'helm-full-frame)))
    (if arg
        (if (and (stringp arg) (bufferp (get-buffer arg)))
            (setq any-buffer arg)
          (setq any-buffer (helm-resume-select-buffer)))
      (setq any-buffer helm-last-buffer))
    (cl-assert any-buffer nil
               "helm-resume: No helm buffers found to resume")
    ;; Reset `cursor-type' to nil as it have been set to t
    ;; when quitting previous session.
    (with-current-buffer any-buffer (setq cursor-type nil))
    (setq helm-full-frame (buffer-local-value
                           'helm-full-frame (get-buffer any-buffer)))
    (setq helm-compiled-sources nil)
    (setq cur-dir (buffer-local-value
                   'default-directory (get-buffer any-buffer)))
    (setq helm-saved-selection nil
          helm-saved-action nil)
    (unless (buffer-live-p helm-current-buffer)
      ;; `helm-current-buffer' may have been killed.
      (setq helm-current-buffer (current-buffer)))
    ;; Restart with same `default-directory' value this session
    ;; was initially started with.
    (with-helm-default-directory cur-dir
        (helm
         :sources (buffer-local-value
                   'helm-sources (get-buffer any-buffer))
         :input (buffer-local-value 'helm-input-local (get-buffer any-buffer))
         :prompt (buffer-local-value 'helm--prompt (get-buffer any-buffer))
         :resume t
         :buffer any-buffer))))

(defun helm-resume-previous-session-after-quit (arg)
  "Resume previous helm session within a running helm."
  (interactive "p")
  (with-helm-alive-p
    (if (> (length helm-buffers) arg)
        (helm-run-after-exit (lambda () (helm-resume (nth arg helm-buffers))))
      (message "No previous helm sessions available for resuming!"))))
(put 'helm-resume-previous-session-after-quit 'helm-only t)

(defun helm-resume-list-buffers-after-quit ()
  "List resumable helm buffers within running helm."
  (interactive)
  (with-helm-alive-p
    (if (> (length helm-buffers) 0)
        (helm-run-after-exit (lambda () (helm-resume t)))
      (message "No previous helm sessions available for resuming!"))))
(put 'helm-resume-list-buffers-after-quit 'helm-only t)

(defun helm-resume-p (any-resume)
  "Whether current helm session is resumed or not."
  (eq any-resume t))

(defun helm-resume-select-buffer ()
  "Select an `helm-buffer' in `helm-buffers' list to resume a helm session.
Return nil if no `helm-buffer' found."
  (when helm-buffers
    (or (helm :sources (helm-build-sync-source "Resume helm buffer"
                          :candidates helm-buffers)
              :resume 'noresume
              :buffer "*helm resume*")
        (keyboard-quit))))

;;;###autoload
(defun helm-cycle-resume ()
  "Cycle in `helm-buffers' list and resume when waiting more than 1.2s."
  (interactive)
  (cl-assert (and helm-buffers helm-last-buffer)
             nil "No helm buffers to resume")
  ;; Setup a new iterator only on first hit on
  ;; `helm-run-cycle-resume', subsequents hits should reuse same
  ;; iterator.
  (unless (and (eq last-command 'helm-cycle-resume)
               helm--cycle-resume-iterator)
    (setq helm--cycle-resume-iterator
          (helm-iter-sub-next-circular
           helm-buffers helm-last-buffer :test 'equal)))
  (helm--resume-or-iter))

(defun helm--resume-or-iter (&optional from-helm)
  (message "Resuming helm buffer `%s'" helm-last-buffer)
  (if (sit-for helm-cycle-resume-delay)
      ;; Delay expire, run helm-resume.
      (if from-helm
          (helm-run-after-exit (lambda () (helm-resume helm-last-buffer)))
        (helm-resume helm-last-buffer))
    ;; key pressed before delay, cycle.
    (unless from-helm ; cycling to next item already done.
      (message "Resuming helm buffer `%s'"
               (setq helm-last-buffer
                     (helm-iter-next helm--cycle-resume-iterator))))))

(defun helm-run-cycle-resume ()
  "Same as `helm-cycle-resume' but intended to be called only from helm."
  (interactive)
  (when (cdr helm-buffers) ; only one session registered.
    ;; Setup a new iterator only on first hit on
    ;; `helm-run-cycle-resume', subsequents hits should reuse same
    ;; iterator.
    (unless (and (eq last-command 'helm-run-cycle-resume)
                 helm--cycle-resume-iterator)
      (setq helm--cycle-resume-iterator
            (helm-iter-sub-next-circular
             helm-buffers helm-last-buffer :test 'equal)))
    ;; start at next buffer as we already are at `helm-last-buffer'.
    (setq helm-last-buffer
          (helm-iter-next helm--cycle-resume-iterator))
    (helm--resume-or-iter 'from-helm)))
(put 'helm-run-cycle-resume 'helm-only t)


;;;###autoload
(defun helm-other-buffer (any-sources any-buffer)
  "Simplified `helm' interface with other `helm-buffer'.
Call `helm' only with ANY-SOURCES and ANY-BUFFER as args."
  (helm :sources any-sources :buffer any-buffer))

(defun helm--nest (&rest same-as-helm)
  "[internal]Allows calling `helm' within a running helm session.

Arguments SAME-AS-HELM are the same as `helm'.

Don't use this directly, use instead `helm' with the keyword
:allow-nest.

\(fn &key SOURCES INPUT PROMPT RESUME PRESELECT BUFFER KEYMAP DEFAULT HISTORY OTHER-LOCAL-VARS)"
  (with-helm-window
    (let ((orig-helm-current-buffer helm-current-buffer)
          (orig-helm-buffer helm-buffer)
          (orig-helm--prompt helm--prompt)
          (orig-helm--in-fuzzy helm--in-fuzzy)
          (orig-helm-last-frame-or-window-configuration
           helm-last-frame-or-window-configuration)
          (orig-one-window-p helm-onewindow-p))
      (unwind-protect
          (let (helm-current-position
                helm-current-buffer
                helm-pattern
                (helm-buffer (or (cl-getf same-as-helm :buffer)
                                 (nth 5 same-as-helm)
                                 "*Helm*"))
                helm-sources
                helm-compiled-sources
                (helm-full-frame t)
                (enable-recursive-minibuffers t))
            (apply #'helm same-as-helm))
        (with-current-buffer orig-helm-buffer
          (setq helm-alive-p t) ; Nested session set this to nil on exit.
          (setq helm-buffer orig-helm-buffer)
          (setq helm--prompt orig-helm--prompt)
          (setq helm--in-fuzzy orig-helm--in-fuzzy)
          (helm-initialize-overlays helm-buffer)
          (unless (helm-empty-buffer-p) (helm-mark-current-line t))
          (setq helm-last-frame-or-window-configuration
                orig-helm-last-frame-or-window-configuration)
          (setq cursor-type nil)
          (setq helm-current-buffer orig-helm-current-buffer)
          (setq helm-onewindow-p orig-one-window-p)
          ;; Be sure advices, hooks, and local modes keep running.
          (if (fboundp 'advice-add)
              (progn
                (advice-add 'tramp-read-passwd
                            :around #'helm--advice-tramp-read-passwd)
                (advice-add 'ange-ftp-get-passwd
                            :around #'helm--advice-ange-ftp-get-passwd))
            (ad-activate 'tramp-read-passwd)
            (ad-activate 'ange-ftp-get-passwd))
          (unless helm-allow-mouse
            (helm--remap-mouse-mode 1))
          (unless (cl-loop for h in post-command-hook
                           thereis (memq h '(helm--maybe-update-keymap
                                             helm--update-header-line)))
            (add-hook 'post-command-hook 'helm--maybe-update-keymap)
            (add-hook 'post-command-hook 'helm--update-header-line))
          (helm-display-mode-line (helm-get-current-source)))))))


;;; Accessors
;;
(defun helm-current-position (save-or-restore)
  "Save or restore current position in `helm-current-buffer'.
Argument SAVE-OR-RESTORE is either save or restore."
  (cl-case save-or-restore
    (save
     (helm-log "Save position at %S" (cons (point) (window-start)))
     (setq helm-current-position (cons (point) (window-start))))
    (restore
     ;; Maybe `helm-current-buffer' have been deleted
     ;; during helm session so check if it is here
     ;; otherwise position in underlying buffer will be lost.
     (when (get-buffer-window helm-current-buffer 'visible)
       (helm-log "Restore position at  %S in buffer %s"
                 helm-current-position
                 (buffer-name (current-buffer)))
       (goto-char (car helm-current-position))
       ;; Fix this position with the NOFORCE arg of `set-window-start'
       ;; otherwise, if there is some other buffer than `helm-current-buffer'
       ;; one, position will be lost.
       (set-window-start (selected-window) (cdr helm-current-position) t)))))


(defun helm-frame-or-window-configuration (save-or-restore)
  "Save or restore last frame or window configuration.
Argument SAVE-OR-RESTORE is either save or restore of window or
frame configuration as per `helm-save-configuration-functions'."
  (helm-log "helm-save-configuration-functions = %S"
            helm-save-configuration-functions)
  (let ((window-persistent-parameters (append '((no-other-window . t))
                                              window-persistent-parameters)))
    (cl-case save-or-restore
      (save    (setq helm-last-frame-or-window-configuration
                     (funcall (cdr helm-save-configuration-functions))))
      (restore (funcall (car helm-save-configuration-functions)
                        helm-last-frame-or-window-configuration)
               ;; Restore frame focus.
               ;; This is needed for minibuffer own-frame config
               ;; when recursive minibuffers are in use.
               ;; e.g M-: + helm-minibuffer-history.
               (cl-letf ((frame (if (minibufferp helm-current-buffer)
                                    (selected-frame)
                                  (last-nonminibuffer-frame)))
                         ;; This is a workaround, because the i3 window
                         ;; manager developers are refusing to fix their
                         ;; broken timestamp and event handling.
                         ;;
                         ;; We basically just disable the part of
                         ;; select-frame-set-input-focus that would call
                         ;; XSetInputFocus in Xlib (x-focus-frame), that
                         ;; resets a timestamp in the xserver which the i3
                         ;; developers fail to notice.
                         ;;
                         ;; Since they don't know about the new timestamp,
                         ;; their keyboard handling can break after a helm
                         ;; user quits emacs, as reported in #1641.
                         ;;
                         ;; Fortunately for us, we really don't need this
                         ;; XSetInputFocus call, since we already have focus
                         ;; for Emacs, the user is just using helm!  We call
                         ;; select-frame-set-input-focus for the other
                         ;; side-effects, not for x-focus-frame.
                         ((symbol-function 'x-focus-frame) #'ignore))
                 (select-frame-set-input-focus frame))))))

(defun helm-split-window-default-fn (window)
  (let (split-width-threshold)
    (if (and (fboundp 'window-in-direction)
             ;; Don't try to split when starting in a minibuffer
             ;; e.g M-: and try to use helm-show-kill-ring.
             (not (minibufferp helm-current-buffer)))
        (if (or (one-window-p t)
                helm-split-window-in-side-p)
            (split-window
             (selected-window) nil (if (eq helm-split-window-default-side 'other)
                                       'below helm-split-window-default-side))
          ;; If more than one window reuse one of them.
          (cl-case helm-split-window-default-side
            (left  (or (helm-window-in-direction 'left)
                       (helm-window-in-direction 'above)
                       (selected-window)))
            (above (or (helm-window-in-direction 'above)
                       (helm-window-in-direction 'left)
                       (selected-window)))
            (right (or (helm-window-in-direction 'right)
                       (helm-window-in-direction 'below)
                       (selected-window)))
            (below (or (helm-window-in-direction 'below)
                       (helm-window-in-direction 'right)
                       (selected-window)))
            (same  (selected-window))
            (other (other-window-for-scrolling))
            (t     (or (window-next-sibling) (selected-window)))))
      (split-window-sensibly window))))

(defun helm-window-in-direction (direction)
  "Same as `window-in-direction' but check if window is dedicated."
  (helm-aif (window-in-direction direction)
      (and (not (window-dedicated-p it)) it)))


;;; Display helm buffer
;;
;;
(defun helm-display-buffer (buffer)
  "Display BUFFER.
The function to display `helm-buffer'."
  (let (pop-up-frames
        (split-window-preferred-function
         helm-split-window-preferred-function)
        (helm-split-window-default-side
         (if (and (not helm-full-frame)
                  helm-reuse-last-window-split-state)
             (cond ((eq helm-split-window-default-side 'same) 'same)
                   ((eq helm-split-window-default-side 'other) 'other)
                   (helm--window-side-state)
                   (t helm-split-window-default-side))
           helm-split-window-default-side)))
    (prog1
        (funcall (with-current-buffer buffer helm-display-function) buffer)
      (setq helm-onewindow-p (one-window-p t))
      ;; Don't allow other-window and friends switching out of minibuffer.
      (when helm-prevent-escaping-from-minibuffer
        (helm-prevent-switching-other-window)))))

(cl-defun helm-prevent-switching-other-window (&key (enabled t))
  "Allow setting `no-other-window' parameter for all windows.
Arg ENABLE is the value of `no-other-window' window property."
  (walk-windows
   (lambda (w)
     (unless (window-dedicated-p w)
       (set-window-parameter w 'no-other-window enabled)))
   0))

(defun helm-default-display-buffer (buffer)
  "Default function to display `helm-buffer' BUFFER.
It uses `switch-to-buffer' or `display-buffer' depending on the
value of `helm-full-frame' or `helm-split-window-default-side'."
  (if (or (buffer-local-value 'helm-full-frame (get-buffer buffer))
          (and (eq helm-split-window-default-side 'same)
               (one-window-p t)))
      (progn (and (not (minibufferp helm-current-buffer))
                  (delete-other-windows))
             (switch-to-buffer buffer))
    (when (and (or helm-always-two-windows helm-autoresize-mode
                   (and (not helm-split-window-in-side-p)
                        (eq (save-selected-window
                              (funcall helm-split-window-preferred-function
                                       (selected-window)))
                            (get-buffer-window helm-current-buffer))))
               (not (eq helm-split-window-default-side 'same))
               (not (minibufferp helm-current-buffer))
               (not helm-split-window-in-side-p))
      (delete-other-windows))
    (display-buffer
     buffer `(nil . ((window-height . ,helm-display-buffer-default-height)
                     (window-width  . ,helm-display-buffer-default-width))))
    (helm-log-run-hook 'helm-window-configuration-hook)))


;;; Initialize
;;
(defun helm-initialize (any-resume any-input any-default any-sources)
  "Start initialization of `helm' session.
For ANY-RESUME ANY-INPUT ANY-DEFAULT and ANY-SOURCES See `helm'."
  (helm-log "start initialization: any-resume=%S any-input=%S"
            any-resume any-input)
  (helm-frame-or-window-configuration 'save)
  (setq helm-sources (helm-normalize-sources any-sources))
  (setq helm--in-fuzzy
        (cl-loop for s in helm-sources
                 for matchfns = (helm-match-functions
                                 (if (symbolp s) (symbol-value s) s))
                 for searchfns = (helm-search-functions
                                  (if (symbolp s) (symbol-value s) s))
                 when (or (memq 'helm-fuzzy-match matchfns)
                          (memq 'helm-fuzzy-search searchfns))
                 return t))
  (helm-log "sources = %S" helm-sources)
  (helm-current-position 'save)
  (if (helm-resume-p any-resume)
      (helm-initialize-overlays (helm-buffer-get))
    (helm-initial-setup any-default))
  (setq helm-alive-p t)
  (unless (eq any-resume 'noresume)
    (helm--recent-push helm-buffer 'helm-buffers)
    (setq helm-last-buffer helm-buffer))
  (when any-input
    (setq helm-input any-input
          helm-pattern any-input)
    (helm--fuzzy-match-maybe-set-pattern))
  ;; If a `resume' attribute is present `helm-funcall-foreach'
  ;; will run its function.
  (when (helm-resume-p any-resume)
    (helm-funcall-foreach 'resume))
  (helm-log "end initialization"))

(defun helm-initialize-overlays (buffer)
  "Initialize helm overlays in BUFFER."
  (helm-log "overlay setup")
  (if helm-selection-overlay
      ;; make sure the overlay belongs to the helm buffer if
      ;; it's newly created
      (move-overlay helm-selection-overlay (point-min) (point-min)
                    (get-buffer buffer))

    (setq helm-selection-overlay
          (make-overlay (point-min) (point-min) (get-buffer buffer)))
    (overlay-put helm-selection-overlay 'face 'helm-selection)
    (overlay-put helm-selection-overlay 'priority 1)))

(defun helm-restore-position-on-quit ()
  "Restore position in `helm-current-buffer' when quitting."
  (helm-current-position 'restore))

(defun helm--recent-push (elm sym)
  "Move ELM of SYM value on top and set SYM to this new value."
  (pcase (symbol-value sym)
    ((and (pred (member elm)) l)
     (set sym (delete elm l))))
  (push elm (symbol-value sym)))

(defun helm--current-buffer ()
  "[internal] Return `current-buffer' BEFORE `helm-buffer' is initialized.
Note that it returns the minibuffer in use after helm has started
and is intended for `helm-initial-setup'. To get the buffer where
helm was started, use `helm-current-buffer' instead."
  (if (minibuffer-window-active-p (minibuffer-window))
      ;; If minibuffer is active be sure to use it's buffer
      ;; as `helm-current-buffer', this allow to use helm
      ;; from an already active minibuffer (M-: etc...)
      (window-buffer (active-minibuffer-window))
    ;; Fix Issue #456
    ;; Use this instead of `current-buffer' to ensure
    ;; helm session started in helm-mode from a completing-read
    ;; Use really the buffer where we started and not the one
    ;; where the completing-read is wrapped. i.e
    ;; (with-current-buffer SOME-OTHER-BUFFER (completing-read [...])
    (window-buffer (with-selected-window (minibuffer-window)
                     (minibuffer-selected-window)))))

(defun helm--run-init-hooks (hook)
  "Run after and before init hooks local to source.
See :after-init-hook and :before-init-hook in `helm-source'."
  (cl-loop with sname = (cl-ecase hook
                          (before-init-hook "h-before-init-hook")
                          (after-init-hook "h-after-init-hook"))
           with h = (cl-gensym sname)
           for s in (helm-get-sources)
           for hv = (assoc-default hook s)
           if (and hv (not (symbolp hv)))
           do (set h hv)
           and do (helm-log-run-hook h)
           else do (helm-log-run-hook hv)))

(defun helm-initial-setup (any-default)
  "Initialize helm settings and set up the helm buffer."
  ;; Run global hook.
  (helm-log-run-hook 'helm-before-initialize-hook)
  ;; Run local source hook.
  (helm--run-init-hooks 'before-init-hook)
  ;; For initialization of helm locals vars that need
  ;; a value from current buffer, it is here.
  (helm-set-local-variable 'current-input-method current-input-method)
  (setq helm-current-prefix-arg nil
        helm-saved-action nil
        helm-saved-selection nil
        helm-suspend-update-flag nil
        ;; Ensure this is called BEFORE selecting helm-window.
        helm-current-buffer (helm--current-buffer)
        helm-buffer-file-name buffer-file-name
        helm-issued-errors nil
        helm-compiled-sources nil
        helm-saved-current-source nil)
  (unless (and (or helm-split-window-state
                   helm--window-side-state)
               helm-reuse-last-window-split-state)
    (setq helm-split-window-state
          (if (or (null split-width-threshold)
                  (and (integerp split-width-threshold)
                       (>= split-width-threshold (+ (frame-width) 4))))
              'vertical 'horizontal))
    (setq helm--window-side-state
          (or helm-split-window-default-side 'below)))
  ;; Call the init function for sources where appropriate
  (helm-funcall-foreach 'init)
  (setq helm-pattern (or (and helm--maybe-use-default-as-input
                              (or (if (listp any-default)
                                      (car any-default) any-default)
                                  (with-helm-current-buffer
                                    (thing-at-point 'symbol))))
                         ""))
  (setq helm-input "")
  (clrhash helm-candidate-cache)
  (helm-create-helm-buffer)
  (helm-clear-visible-mark)
  ;; Run global hook.
  (helm-log-run-hook 'helm-after-initialize-hook)
  ;; Run local source hook.
  (helm--run-init-hooks 'after-init-hook))

(define-derived-mode helm-major-mode
    fundamental-mode "Hmm"
    "[Internal] Provide major-mode name in helm buffers.
Unuseful when used outside helm, don't use it.")
(put 'helm-major-mode 'mode-class 'special)
(put 'helm-major-mode 'helm-only t)

(defun helm-create-helm-buffer ()
  "Create and setup `helm-buffer'."
  (let ((root-dir default-directory)
        (inhibit-read-only t))
    (with-current-buffer (get-buffer-create helm-buffer)
      (helm-log "Enabling major-mode %S" major-mode)
      (helm-log "kill local variables: %S" (buffer-local-variables))
      (kill-all-local-variables)
      (helm-major-mode)
      (set (make-local-variable 'buffer-read-only) nil)
      (buffer-disable-undo)
      (erase-buffer)
      (set (make-local-variable 'helm-map) helm-map)
      (set (make-local-variable 'helm-source-filter) nil)
      (make-local-variable 'helm-sources)
      (set (make-local-variable 'helm-display-function) helm-display-function)
      (set (make-local-variable 'helm-selection-point) nil)
      (set (make-local-variable 'scroll-margin)
           (if helm-display-source-at-screen-top
               0 helm-completion-window-scroll-margin))
      (set (make-local-variable 'default-directory) root-dir)
      (set (make-local-variable 'helm-marked-candidates) nil)
      (set (make-local-variable 'helm--prompt) helm--prompt)
      (helm-initialize-persistent-action)
      (helm-log "helm-display-function = %S" helm-display-function)
      (helm-log "helm--local-variables = %S" helm--local-variables)
      (cl-loop for (var . val) in helm--local-variables
               do (set (make-local-variable var) val)
               finally (setq helm--local-variables nil))
      (setq truncate-lines helm-truncate-lines) ; already local.
      (setq cursor-type nil))
    (helm-initialize-overlays helm-buffer)
    (get-buffer helm-buffer)))

(define-minor-mode helm--minor-mode
  "[INTERNAL] Enable keymap in helm minibuffer.
Since this mode has no effect when run outside of helm context,
please don't use it outside helm.

\\{helm-map}"
  :group 'helm
  :keymap (and helm-alive-p helm-map)
  (unless helm-alive-p (setq helm--minor-mode nil)))
(put 'helm--minor-mode 'helm-only t)

(defun helm--reset-default-pattern ()
  (setq helm-pattern "")
  (setq helm--maybe-use-default-as-input nil))

(defun helm-read-pattern-maybe (any-prompt any-input
                                           any-preselect any-resume any-keymap
                                           any-default any-history)
  "Read pattern with prompt ANY-PROMPT and initial input ANY-INPUT.
For ANY-PRESELECT ANY-RESUME ANY-KEYMAP ANY-DEFAULT ANY-HISTORY, See `helm'."
  (if (and (helm-resume-p any-resume)
           ;; When no source, helm-buffer is empty
           ;; or contain non--candidate lines (e.g grep exit status)
           (helm-get-current-source))
      (helm-mark-current-line t)
    (helm-update any-preselect))
  (with-helm-buffer
    (let* ((src        (helm-get-current-source))
           (src-keymap (assoc-default 'keymap src))
           (hist       (or (and any-history (symbolp any-history) any-history)
                           ;; Needed for resuming.
                           (assoc-default 'history src)))
           (timer nil)
           blink-matching-paren
           (resize-mini-windows (and (null helm-echo-input-in-header-line)
                                     resize-mini-windows))
           (first-src (car helm-sources))
           (first-src-val (if (symbolp first-src)
                              (symbol-value first-src)
                            first-src))
           (source-process-p (or (assq 'candidates-process src)
                                 (assq 'candidates-process first-src-val))))
      (helm-log "helm-get-candidate-number => %S"
                (helm-get-candidate-number))
      (helm-log "helm-execute-action-at-once-if-one = %S"
                helm-execute-action-at-once-if-one)
      (helm-log "helm-quit-if-no-candidate = %S" helm-quit-if-no-candidate)
      (when (and src (helm-resume-p any-resume))
        (helm-display-mode-line src))
      ;; Reset `helm-pattern' and update
      ;; display if no result found with precedent value of `helm-pattern'
      ;; unless `helm-quit-if-no-candidate' is non-`nil', in this case
      ;; Don't force update with an empty pattern.
      ;; Reset also `helm--maybe-use-default-as-input' as this checking
      ;; happen only on startup.
      (when helm--maybe-use-default-as-input
        ;; Store value of `default' temporarily here waiting next update
        ;; to allow actions like helm-moccur-action matching pattern
        ;; at the place it jump to.
        (setq helm-input helm-pattern)
        (if source-process-p
            ;; Reset pattern to next update.
            (with-helm-after-update-hook
              (helm--reset-default-pattern))
          ;; Reset pattern right now.
          (helm--reset-default-pattern))
        ;; Ensure force-update when no candidates
        ;; when we start with an empty pattern.
        (and (helm-empty-buffer-p)
             (null helm-quit-if-no-candidate)
             (helm-force-update)))
      ;; Handle `helm-execute-action-at-once-if-one' and
      ;; `helm-quit-if-no-candidate' now.
      (cond ((and (if (functionp helm-execute-action-at-once-if-one)
                      (funcall helm-execute-action-at-once-if-one)
                    helm-execute-action-at-once-if-one)
                  (= (helm-get-candidate-number
                      (eq helm-execute-action-at-once-if-one 'current-source)) 1))
             (ignore))              ; Don't enter the minibuffer loop.
            ((and helm-quit-if-no-candidate
                  (= (helm-get-candidate-number) 0))
             (setq helm-quit t)
             (and (functionp helm-quit-if-no-candidate)
                  (funcall helm-quit-if-no-candidate)))
            (t              ; Enter now minibuffer and wait for input.
             (let ((tap (or any-default
                            (with-helm-current-buffer
                              (thing-at-point 'symbol)))))
               (unwind-protect
                   (minibuffer-with-setup-hook
                       (lambda ()
                         ;; Start minor-mode with global value of helm-map.
                         (helm--minor-mode 1)
                         ;; Now override the global value of `helm-map' with
                         ;; the local one which is in this order:
                         ;; - The keymap of current source.
                         ;; - The value passed in ANY-KEYMAP
                         ;; - Or fallback to the global value of helm-map.
                         (helm--maybe-update-keymap
                          (or src-keymap any-keymap helm-map))
                         (helm-log-run-hook 'helm-minibuffer-set-up-hook)
                         (setq timer
                               (run-with-idle-timer
                                (max (with-helm-buffer helm-input-idle-delay)
                                     0.001)
                                'repeat
                                (lambda ()
                                  ;; Stop updating in persistent action
                                  ;; or when `helm-suspend-update-flag'
                                  ;; is non-`nil'.
                                  (unless (or helm-in-persistent-action
                                              helm-suspend-update-flag)
                                    (save-selected-window
                                      (helm-check-minibuffer-input)
                                      (helm-print-error-messages))))))
                         ;; minibuffer has already been filled here.
                         (helm--update-header-line))
                     (read-from-minibuffer (or any-prompt "pattern: ")
                                           any-input helm-map
                                           nil hist tap
                                           helm-inherit-input-method))
                 (when timer (cancel-timer timer) (setq timer nil)))))))))

(defun helm-toggle-suspend-update ()
  "Enable or disable update of display in helm.
This can be useful for example for quietly writing a complex regexp
without helm constantly updating."
  (interactive)
  (with-helm-alive-p
    (when (setq helm-suspend-update-flag (not helm-suspend-update-flag))
      (helm-kill-async-processes)
      (setq helm-pattern ""))
    (message (if helm-suspend-update-flag
                 "Helm update suspended!"
               "Helm update re-enabled!"))
    (helm-aif (helm-get-current-source)
        (with-helm-buffer (helm-display-mode-line it t)))))
(put 'helm-toggle-suspend-update 'helm-only t)

(defun helm--advice-tramp-read-passwd (old--fn &rest args)
  ;; Suspend update when prompting for a tramp password.
  (setq helm-suspend-update-flag t)
  (setq overriding-terminal-local-map nil)
  (setq helm--reading-passwd-or-string t)
  (unwind-protect
       ;; No need to suspend timer in emacs-24.4
       ;; it is fixed upstream.
       (apply old--fn args)
    (setq helm--reading-passwd-or-string nil)
    (setq helm-suspend-update-flag nil)))

(defun helm--advice-ange-ftp-get-passwd (old--fn &rest args)
  ;; Suspend update when prompting for a ange password.
  (setq helm-suspend-update-flag t)
  (setq overriding-terminal-local-map nil)
  (setq helm--reading-passwd-or-string t)
  (unwind-protect
       (apply old--fn args)
    (setq helm--reading-passwd-or-string nil)
    (setq helm-suspend-update-flag nil)))

;; CUA workaround
(defun cua-delete-region--advice (old--fn &rest args)
  (ignore-errors
    (apply old--fn args)))

(defun copy-region-as-kill--advice (old--fn &rest args)
  (if cua-mode
      (ignore-errors (apply old--fn args))
      (apply old--fn args)))

(defun helm--maybe-update-keymap (&optional map)
  "Handle different keymaps in multiples sources.

Overrides `helm-map' with the local map of current source. If no
map is found in current source, does nothing (keeps previous
map)."
  (with-helm-buffer
    (helm-aif (or map (assoc-default 'keymap (helm-get-current-source)))
        ;; We used a timer in the past to leave
        ;; enough time to helm to setup its keymap
        ;; when changing source from a recursive minibuffer.
        ;; e.g C-x C-f M-y C-g
        ;; => *find-files have now the bindings of *kill-ring.
        ;; It is no more true now we are using `minor-mode-overriding-map-alist'
        ;; and `helm--minor-mode' thus it fix issue #1076 for emacs-24.3
        ;; where concurrent timers are not supported.
        ;; i.e update keymap+check input.
        (with-current-buffer (window-buffer (minibuffer-window))
          (setq minor-mode-overriding-map-alist `((helm--minor-mode . ,it)))))))

;;; Prevent loosing focus when using mouse.
;;
(defvar helm--remap-mouse-mode-map
  (let ((map (make-sparse-keymap)))
    (cl-loop for k in '([mouse-1] [mouse-2] [mouse-3]
                        [down-mouse-1] [down-mouse-2] [down-mouse-3]
                        [drag-mouse-1] [drag-mouse-2] [drag-mouse-3]
                        [double-mouse-1] [double-mouse-2] [double-mouse-3]
                        [triple-mouse-1] [triple-mouse-2] [triple-mouse-3])
             do (define-key map k 'ignore))
    map))

(define-minor-mode helm--remap-mouse-mode
    "[INTERNAL] Prevent escaping helm minibuffer with mouse clicks.
Do nothing when used outside of helm context.

WARNING: Do not use this mode yourself, it is internal to helm."
  :group 'helm
  :global t
  :keymap helm--remap-mouse-mode-map
  (unless helm-alive-p
    (setq helm--remap-mouse-mode-map nil)))
(put 'helm--remap-mouse-mode 'helm-only t)

;; Clean up

(defun helm-cleanup ()
  "Clean up the mess when helm exit or quit."
  (helm-log "start cleanup")
  (with-current-buffer helm-buffer
    (setq cursor-type t)
    ;; bury-buffer from this window.
    (bury-buffer) ;[1]
    (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
    (remove-hook 'post-command-hook 'helm--update-header-line)
    ;; Be sure we call this from helm-buffer.
    (helm-funcall-foreach 'cleanup))
  (helm-kill-async-processes)
  ;; Remove the temporary hooks added
  ;; by `with-helm-temp-hook' that
  ;; may not have been consumed.
  (when helm--temp-hooks
    (cl-loop for (fn . hook) in helm--temp-hooks
             do (set hook (delete fn (symbol-value hook)))))
  ;; When running helm from a dedicated frame
  ;; with no minibuffer, helm will run in the main frame
  ;; which have a minibuffer, so be sure to disable
  ;; the `no-other-window' prop there.
  (helm-prevent-switching-other-window :enabled nil)
  (helm-log-run-hook 'helm-cleanup-hook)
  (helm-frame-or-window-configuration 'restore)
  ;; [1] now bury-buffer from underlying windows otherwise,
  ;; if this window is killed the underlying buffer will
  ;; be a helm buffer.
  (replace-buffer-in-windows helm-buffer)
  (setq helm-alive-p nil)
  ;; This is needed in some cases where last input
  ;; is yielded infinitely in minibuffer after helm session.
  (helm-clean-up-minibuffer))

(defun helm-clean-up-minibuffer ()
  "Remove contents of minibuffer."
  (let ((miniwin (minibuffer-window)))
    ;; Clean only current minibuffer used by helm.
    ;; i.e The precedent one is active.
    (unless (minibuffer-window-active-p miniwin)
      (with-current-buffer (window-buffer miniwin)
        (delete-minibuffer-contents)))))


;;; Input handling
;;
;;
(defun helm-check-minibuffer-input ()
  "Check minibuffer content."
  (with-helm-quittable
    (with-selected-window (or (active-minibuffer-window)
                              (minibuffer-window))
      (helm-check-new-input (minibuffer-contents)))))

(defun helm-check-new-input (input)
  "Check INPUT string and update the helm buffer if necessary."
  (unless (equal input helm-pattern)
    (setq helm-pattern input)
    (unless (helm-action-window)
      (setq helm-input helm-pattern))
    (helm-log "helm-pattern = %S" helm-pattern)
    (helm-log "helm-input = %S" helm-input)
    (setq helm--in-update t)
    (helm-update)))

(defun helm--reset-update-flag ()
  (run-with-idle-timer
   helm-exit-idle-delay nil
   (lambda () (setq helm--in-update nil))))

;; (add-hook 'helm-after-update-hook #'helm--reset-update-flag)


;; All candidates

(defun helm-get-candidates (source)
  "Retrieve and return the list of candidates from SOURCE."
  (let* (inhibit-quit
         (candidate-fn (assoc-default 'candidates source))
         (candidate-proc (assoc-default 'candidates-process source))
         cfn-error
         (notify-error
          (lambda (&optional e)
            (error
             "In `%s' source: `%s' %s %s"
             (assoc-default 'name source)
             (or candidate-fn candidate-proc)
             (if e "\n" "must be a list, a symbol bound to a list, or a function returning a list")
             (if e (prin1-to-string e) ""))))
         (candidates (condition-case-unless-debug err
                         ;; Process candidates-(process) function
                         ;; It may return a process or a list of candidates.
                         (if candidate-proc
                             ;; Calling `helm-interpret-value' with no
                             ;; SOURCE arg force the use of `funcall'
                             ;; and not `helm-funcall-with-source'.
                             (helm-interpret-value candidate-proc)
                           (helm-interpret-value candidate-fn source))
                       (error (helm-log "Error: %S" (setq cfn-error err)) nil))))
    (when (and (processp candidates) (not candidate-proc))
      (warn "Candidates function `%s' should be called in a `candidates-process' attribute"
            candidate-fn))
    (cond ((processp candidates)
           ;; Candidates will be filtered later in process filter.
           candidates)
          ;; An error occured in candidates function.
          (cfn-error (unless helm--ignore-errors
                       (funcall notify-error cfn-error)))
          ;; Candidates function returns no candidates.
          ((or (null candidates)
               ;; Can happen when the output of a process
               ;; is empty, and the candidates function call
               ;; something like (split-string (buffer-string) "\n")
               ;; which result in a list of one empty string (Issue #938).
               ;; e.g (completing-read "test: " '(""))
               (equal candidates '("")))
           nil)
          ((listp candidates)
           ;; Transform candidates with `candidate-transformer' functions or
           ;; `real-to-display' functions if those are found,
           ;; otherwise return candidates unmodified.
           ;; `filtered-candidate-transformer' is NOT called here.
           (helm-transform-candidates candidates source))
          (t (funcall notify-error)))))

(defmacro helm-while-no-input (&rest body)
  "Same as `while-no-input' but without the `input-pending-p' test."
  (declare (debug t) (indent 0))
  (let ((catch-sym (make-symbol "input")))
    `(with-local-quit
       (catch ',catch-sym
         (let ((throw-on-input ',catch-sym))
           ,@body)))))

(defun helm-get-cached-candidates (source)
  "Return the cached value of candidates for SOURCE.
Cache the candidates if there is no cached value yet."
  (let* ((name (assoc-default 'name source))
         (candidate-cache (gethash name helm-candidate-cache)))
    (helm-aif candidate-cache
        (prog1 it (helm-log "Use cached candidates"))
      (helm-log "No cached candidates, calculate candidates")
      (let ((candidates (helm-get-candidates source)))
        (cond ((processp candidates)
               (push (cons candidates
                           (append source
                                   (list (cons 'item-count 0)
                                         (cons 'incomplete-line ""))))
                     helm-async-processes)
               (set-process-filter candidates 'helm-output-filter)
               (setq candidates nil))
              ((not (assq 'volatile source))
               (puthash name candidates helm-candidate-cache)))
        candidates))))


;;; Candidate transformers

(defun helm-process-candidate-transformer (candidates source)
  "Execute `candidate-transformer' function(s) on CANDIDATES in SOURCE."
  (helm-aif (assoc-default 'candidate-transformer source)
      (helm-funcall-with-source source it candidates)
    candidates))

(defun helm-process-filtered-candidate-transformer (candidates source)
  "Execute `filtered-candidate-transformer' function(s) on CANDIDATES in SOURCE."
  (helm-aif (assoc-default 'filtered-candidate-transformer source)
      (helm-funcall-with-source source it candidates source)
    candidates))

(defmacro helm--maybe-process-filter-one-by-one-candidate (candidate source)
  "Execute `filter-one-by-one' function(s) on real value of CANDIDATE in SOURCE."
  `(helm-aif (assoc-default 'filter-one-by-one ,source)
       (let ((real (if (consp ,candidate)
                       (cdr ,candidate)
                       ,candidate)))
         (if (and (listp it)
                  (not (functionp it))) ;; Don't treat lambda's as list.
             (cl-loop for f in it
                      do (setq ,candidate (funcall f real))
                      finally return ,candidate)
             (setq ,candidate (funcall it real))))
     ,candidate))

(defun helm--initialize-one-by-one-candidates (candidates source)
  "Process the CANDIDATES with the `filter-one-by-one' function in SOURCE.
Return CANDIDATES unchanged when pattern is not empty."
  (helm-aif (and (string= helm-pattern "")
                 (assoc-default 'filter-one-by-one source))
      (cl-loop for cand in candidates collect
               (helm--maybe-process-filter-one-by-one-candidate cand source))
    candidates))

(defun helm-process-filtered-candidate-transformer-maybe
    (candidates source process-p)
  "Execute `filtered-candidate-transformer' function(s) on CANDIDATES in SOURCE.
When PROCESS-P is non-`nil' execute `filtered-candidate-transformer'
functions if some, otherwise return CANDIDATES."
  (if process-p
      ;; When no filter return CANDIDATES unmodified.
      (helm-process-filtered-candidate-transformer candidates source)
    candidates))

(defun helm-process-real-to-display (candidates source)
  "Execute real-to-display function on all CANDIDATES of SOURCE."
  (helm-aif (assoc-default 'real-to-display source)
      (setq candidates (helm-funcall-with-source
                        source 'mapcar
                        (lambda (cand)
                          (if (consp cand)
                              ;; override DISPLAY from candidate-transformer
                              (cons (funcall it (cdr cand)) (cdr cand))
                            (cons (funcall it cand) cand)))
                        candidates))
    candidates))

(defun helm-transform-candidates (candidates source &optional process-p)
  "Transform CANDIDATES from SOURCE according to candidate transformers.

When PROCESS-P is non-`nil' executes the
`filtered-candidate-transformer' functions, otherwise processes
`candidate-transformer' functions only,
`filtered-candidate-transformer' functions being processed later,
after the candidates have been narrowed by
`helm-candidate-number-limit', see `helm-compute-matches'.  When
`real-to-display' attribute is present, execute its functions on all
maybe filtered CANDIDATES."
  (helm-process-real-to-display
   (helm-process-filtered-candidate-transformer-maybe
    (helm-process-candidate-transformer
     candidates source)
    source process-p)
   source))


;; Narrowing candidates
(defun helm-candidate-number-limit (source)
  "Apply candidate-number-limit attribute value.
This overrides `helm-candidate-number-limit' variable.

e.g:
If \(candidate-number-limit\) is in SOURCE, show all candidates in SOURCE.
If \(candidate-number-limit . 123\) is in SOURCE limit candidate to 123."
  (helm-aif (assq 'candidate-number-limit source)
      ;; When assoc value is nil use by default 99999999 otherwise use
      ;; the assoc value, when it is a symbol interpret its value (#1831).
      (or (helm-aand (cdr it) (helm-interpret-value it)) 99999999)
    (or helm-candidate-number-limit 99999999)))

(defun helm-candidate-get-display (candidate)
  "Get searched display part from CANDIDATE.
CANDIDATE is either a string, a symbol, or a \(DISPLAY . REAL\)
cons cell."
  (cond ((car-safe candidate))
        ((symbolp candidate)
         (symbol-name candidate))
        ((numberp candidate)
         (number-to-string candidate))
        (t candidate)))

(defun helm-process-pattern-transformer (pattern source)
  "Execute pattern-transformer attribute function(s) on PATTERN in SOURCE."
  (helm-aif (assoc-default 'pattern-transformer source)
      (helm-funcall-with-source source it pattern)
    pattern))

(defun helm-default-match-function (candidate)
  "Check if `helm-pattern' match CANDIDATE.
Default function to match candidates according to `helm-pattern'."
  (string-match helm-pattern candidate))


;;; Fuzzy matching
;;
;;
(defvar helm--fuzzy-regexp-cache (make-hash-table :test 'eq))
(defun helm--fuzzy-match-maybe-set-pattern ()
  ;; Computing helm-pattern with helm--mapconcat-pattern
  ;; is costly, so cache it once time for all and reuse it
  ;; until pattern change.
  (when helm--in-fuzzy
    (let ((fun (if (string-match "\\`\\^" helm-pattern)
                   #'identity
                   #'helm--mapconcat-pattern)))
      (clrhash helm--fuzzy-regexp-cache)
      ;; FIXME: Splitted part are not handled here,
      ;; I must compute them in `helm-search-match-part'
      ;; when negation and in-buffer are used.
      (if (string-match "\\`!" helm-pattern)
          (puthash 'helm-pattern
                   (if (> (length helm-pattern) 1)
                       (list (funcall fun (substring helm-pattern 1 2))
                             (funcall fun (substring helm-pattern 1)))
                       '("" ""))
                   helm--fuzzy-regexp-cache)
          (puthash 'helm-pattern
                   (if (> (length helm-pattern) 0)
                       (list (funcall fun (substring helm-pattern 0 1))
                             (funcall fun helm-pattern))
                       '("" ""))
                   helm--fuzzy-regexp-cache)))))

(defun helm-fuzzy-match (candidate)
  "Check if `helm-pattern' fuzzy matches CANDIDATE.
This function is used with sources built with `helm-source-sync'."
  (unless (string-match " " helm-pattern)
    ;; When pattern have one or more spaces, let
    ;; multi-match doing the job with no fuzzy matching.[1]
    (let ((regexp (cadr (gethash 'helm-pattern helm--fuzzy-regexp-cache))))
      (if (string-match "\\`!" helm-pattern)
          (not (string-match regexp candidate))
        (string-match regexp candidate)))))

(defun helm-fuzzy-search (pattern)
  "Same as `helm-fuzzy-match' but for sources built with
`helm-source-in-buffer'."
  (unless (string-match " " helm-pattern)
    ;; Same as in `helm-fuzzy-match' ref[1].
    (let* ((regexps (gethash 'helm-pattern helm--fuzzy-regexp-cache))
           (partial-regexp (car regexps))
           (regexp (cadr regexps)))
      (if (string-match "\\`!" pattern)
          ;; Don't try to search here, just return
          ;; the position of line and go ahead,
          ;; letting `helm-search-match-part' checking if
          ;; pattern match against this line.
          (prog1 (list (point-at-bol) (point-at-eol))
            (forward-line 1))
        ;; We could use here directly `re-search-forward'
        ;; on the regexp produced by `helm--mapconcat-pattern',
        ;; but it is very slow because emacs have to do an incredible
        ;; amount of loops to match e.g "[^f]*f[^o]*o..." in the whole buffer,
        ;; more the regexp is long more the amount of loops grow.
        ;; (Probably leading to a max-lisp-eval-depth error if both
        ;; regexp and buffer are too big)
        ;; So just search the first bit of pattern e.g "[^f]*f", and
        ;; then search the corresponding line with the whole regexp,
        ;; which increase dramatically the speed of the search.
        (cl-loop while (re-search-forward partial-regexp nil t)
                 for bol = (point-at-bol)
                 for eol = (point-at-eol)
                 if (progn (goto-char bol)
                           (re-search-forward regexp eol t))
                 do (goto-char eol) and return t
                 else do (goto-char eol)
                 finally return nil)))))

(defun helm-score-candidate-for-pattern (candidate pattern)
  "Assign score to CANDIDATE according to PATTERN.
Score is calculated for contiguous matches found with PATTERN.
Score is 100 (maximum) if PATTERN is fully matched in CANDIDATE.
One point bonus is added to score when PATTERN prefix matches
CANDIDATE. Contiguous matches get a coefficient of 2."
  (let* ((cand (if (stringp candidate)
                   candidate (helm-stringify candidate)))
         (pat-lookup (helm--collect-pairs-in-string pattern))
         (str-lookup (helm--collect-pairs-in-string cand))
         (bonus (cond ((equal (car pat-lookup) (car str-lookup))
                       1)
                      ((and (null pat-lookup) ; length = 1
                            (string= pattern (substring cand 0 1)))
                       150)
                      (t 0)))
         (bonus1 (and (string-match (concat "\\<" (regexp-quote pattern) "\\>")
                                    cand)
                      100)))
    (+ bonus (or bonus1
                 ;; Give a coefficient of 2 for contiguous matches.
                 ;; That's mean that "wiaaaki" will not take precedence
                 ;; on "aaawiki" when matching on "wiki" even if "wiaaaki"
                 ;; starts by "wi".
                 (* (length (cl-nintersection
                             pat-lookup str-lookup :test 'equal))
                    2)))))

(defun helm-fuzzy-matching-default-sort-fn-1 (candidates &optional use-real basename preserve-tie-order)
  "The transformer for sorting candidates in fuzzy matching.
It sorts on the display part by default.

Sorts CANDIDATES by their scores as calculated by
`helm-score-candidate-for-pattern'.  Set USE-REAL to non-`nil' to
sort on the real part.  If BASENAME is non-nil assume we are
completing filenames and sort on basename of candidates.  If
PRESERVE-TIE-ORDER is nil, ties in scores are sorted by length of
the candidates."
  (if (string= helm-pattern "")
      candidates
    (let ((table-scr (make-hash-table :test 'equal)))
      (sort candidates
            (lambda (s1 s2)
              ;; Score and measure the length on real or display part of candidate
              ;; according to `use-real'.
              (let* ((real-or-disp-fn (if use-real #'cdr #'car))
                     (cand1 (cond ((and basename (consp s1))
                                   (helm-basename (funcall real-or-disp-fn s1)))
                                  ((consp s1) (funcall real-or-disp-fn s1))
                                  (basename (helm-basename s1))
                                  (t s1)))
                     (cand2 (cond ((and basename (consp s2))
                                   (helm-basename (funcall real-or-disp-fn s2)))
                                  ((consp s2) (funcall real-or-disp-fn s2))
                                  (basename (helm-basename s2))
                                  (t s2)))
                     (data1 (or (gethash cand1 table-scr)
                                (puthash cand1
                                         (list (helm-score-candidate-for-pattern
                                                cand1 helm-pattern)
                                               (length (helm-stringify cand1)))
                                         table-scr)))
                     (data2 (or (gethash cand2 table-scr)
                                (puthash cand2
                                         (list (helm-score-candidate-for-pattern
                                                cand2 helm-pattern)
                                               (length (helm-stringify cand2)))
                                         table-scr)))
                     (len1 (cadr data1))
                     (len2 (cadr data2))
                     (scr1 (car data1))
                     (scr2 (car data2)))
                (cond ((= scr1 scr2)
                       (unless preserve-tie-order
                         (< len1 len2)))
                      ((> scr1 scr2)))))))))

(defun helm-fuzzy-matching-default-sort-fn (candidates _source)
  "Default `filtered-candidate-transformer' to sort candidates in fuzzy matching."
  (helm-fuzzy-matching-default-sort-fn-1 candidates))

(defun helm-fuzzy-matching-sort-fn-preserve-ties-order (candidates _source)
  "`filtered-candidate-transformer' to sort candidates in fuzzy matching, preserving order of ties.
The default function, `helm-fuzzy-matching-default-sort-fn',
sorts ties by length, shortest first.  This function may be more
useful when the order of the candidates is meaningful, e.g. with
`recentf-list'."
  (helm-fuzzy-matching-default-sort-fn-1 candidates nil t))

(defun helm--maybe-get-migemo-pattern (pattern)
  (or (and helm-migemo-mode
           (assoc-default pattern helm-mm--previous-migemo-info))
      pattern))

(defun helm-fuzzy-default-highlight-match (candidate)
  "The default function to highlight matches in fuzzy matching.
Highlight elements in CANDIDATE matching `helm-pattern' according
to the matching method in use."
  (if (string= helm-pattern "")
      ;; Empty pattern, do nothing.
      candidate
    ;; Else start highlighting.
    (let* ((pair    (and (consp candidate) candidate))
           (display (helm-stringify (if pair (car pair) candidate)))
           (real    (cdr pair))
           (regex   (helm--maybe-get-migemo-pattern helm-pattern))
           (mp      (pcase (get-text-property 0 'match-part display)
                      ((pred (string= display)) nil)
                      (str str)))
           (count   0)
           beg-str end-str)
      ;; Extract all parts of display keeping original properties.
      (when (and mp (string-match (regexp-quote mp) display))
        (setq beg-str (substring display 0 (match-beginning 0))
              end-str (substring display (match-end 0) (length display))
              mp (substring display (match-beginning 0) (match-end 0))))
      (with-temp-buffer
        ;; Insert the whole display part and remove non--match-part
        ;; to keep their original face properties.
        (insert (propertize (or mp display) 'read-only nil)) ; Fix (#1176)
        (goto-char (point-min))
        (condition-case nil
            (progn
              ;; Try first matching against whole pattern.
              (while (re-search-forward regex nil t)
                (cl-incf count)
                (helm-add-face-text-properties
                 (match-beginning 0) (match-end 0) 'helm-match))
              ;; If no matches start matching against multiples or fuzzy matches.
              (when (zerop count)
                (cl-loop with multi-match = (string-match-p " " helm-pattern)
                         with patterns = (if multi-match
                                             (mapcar #'helm--maybe-get-migemo-pattern
                                                     (helm-mm-split-pattern helm-pattern))
                                             (split-string helm-pattern "" t))
                         for p in patterns
                         ;; Multi matches (regexps patterns).
                         if multi-match do
                         (progn
                           (while (re-search-forward p nil t)
                             (helm-add-face-text-properties
                              (match-beginning 0) (match-end 0)
                              'helm-match))
                           (goto-char (point-min)))
                         ;; Fuzzy matches (literal patterns).
                         else do
                         (when (search-forward p nil t)
                           (helm-add-face-text-properties
                            (match-beginning 0) (match-end 0)
                            'helm-match)))))
          (invalid-regexp nil))
        ;; Now replace the original match-part with the part
        ;; with face properties added.
        (setq display (if mp (concat beg-str (buffer-string) end-str) (buffer-string))))
      (if real (cons display real) display))))

(defun helm-fuzzy-highlight-matches (candidates _source)
  "The filtered-candidate-transformer function to highlight fuzzy matches.
See `helm-fuzzy-default-highlight-match'."
  (cl-loop for c in candidates
           collect (funcall helm-fuzzy-matching-highlight-fn c)))


;;; Matching candidates
;;
;;
(defun helm-match-functions (source)
  (let ((matchfns (or (assoc-default 'match source)
                      (assoc-default 'match-strict source)
                      #'helm-default-match-function)))
    (if (and (listp matchfns) (not (functionp matchfns)))
        matchfns (list matchfns))))

(defun helm-search-functions (source)
  (let ((searchfns (assoc-default 'search source)))
    (if (and (listp searchfns) (not (functionp searchfns)))
        searchfns (list searchfns))))

(defun helm-take-first-elements (seq n)
  "Return the first N elements of SEQ if SEQ is longer than N.
It is used for narrowing list of candidates to the
`helm-candidate-number-limit'."
  (if (> (length seq) n) (cl-subseq seq 0 n) seq))

(defun helm-match-from-candidates (cands matchfns match-part-fn limit source)
  (condition-case-unless-debug err
      (cl-loop with hash = (make-hash-table :test 'equal)
               with allow-dups = (assq 'allow-dups source)
               with case-fold-search = (helm-set-case-fold-search)
               with count = 0
               for iter from 1
               for fn in matchfns
               when (< count limit) nconc
               (cl-loop for c in cands
                        for dup = (gethash c hash)
                        while (< count limit)
                        for target = (helm-candidate-get-display c)
                        for prop-part = (get-text-property 0 'match-part target)
                        for part = (and match-part-fn
                                        (or prop-part
                                            (funcall match-part-fn target)))
                        ;; When allowing dups check if DUP
                        ;; have been already found in previous loop
                        ;; by comparing its value with ITER.
                        when (and (or (and allow-dups dup (= dup iter))
                                      (null dup))
                                  (condition-case nil
                                      (funcall fn (or part target))
                                    (invalid-regexp nil)))
                        do
                        (progn
                          ;; Give as value the iteration number of
                          ;; inner loop to be able to check if
                          ;; the duplicate have not been found in previous loop.
                          (puthash c iter hash)
                          (helm--maybe-process-filter-one-by-one-candidate c source)
                          (cl-incf count))
                        ;; Filter out nil candidates maybe returned by
                        ;; `helm--maybe-process-filter-one-by-one-candidate'.
                        and when c collect
                        (if (and part (not prop-part))
                            (if (consp c)
                                (cons (propertize target 'match-part part) (cdr c))
                              (propertize c 'match-part part))
                          c)))
    (error (unless (eq (car err) 'invalid-regexp) ; Always ignore regexps errors.
             (helm-log-error "helm-match-from-candidates in source `%s': %s %s"
                             (assoc-default 'name source) (car err) (cdr err)))
           nil)))

(defun helm-compute-matches (source)
  "Start computing candidates in SOURCE."
  (save-current-buffer
    (let ((matchfns (helm-match-functions source))
          (matchpartfn (assoc-default 'match-part source))
          (helm--source-name (assoc-default 'name source))
          (helm-current-source source)
          (limit (helm-candidate-number-limit source))
          (helm-pattern (helm-process-pattern-transformer
                         helm-pattern source)))
      (helm--fuzzy-match-maybe-set-pattern)
      ;; If source have a `filtered-candidate-transformer' attr
      ;; Filter candidates with this func, otherwise just compute
      ;; candidates.
      ;; NOTE that this next block of code is returning nil on async sources,
      ;; the candidates being processed directly in `helm-output-filter'
      ;; process-filter. 
      (helm-process-filtered-candidate-transformer
       ;; Using in-buffer method or helm-pattern is empty
       ;; in this case compute all candidates.
       (if (or (equal helm-pattern "")
               (helm--candidates-in-buffer-p matchfns))
           ;; Compute all candidates up to LIMIT.
           ;; one-by-one are computed here only for sources that
           ;; display a list of  candidates even with an empty
           ;; pattern.
           (helm--initialize-one-by-one-candidates
            (helm-take-first-elements
             (helm-get-cached-candidates source) limit)
            source)
           ;; Compute candidates according to pattern with their match
           ;; fns.
           ;; one-by-one filtered candidates are computed during the
           ;; execution of next loop in `helm-match-from-candidates'.
           (helm-match-from-candidates
            (helm-get-cached-candidates source) matchfns matchpartfn limit source))
       source))))

(defun helm--candidates-in-buffer-p (matchfns)
  (equal matchfns '(identity)))

(defun helm-render-source (source matches)
  "Display MATCHES from SOURCE according to its settings."
  (helm-log "Source name = %S" (assoc-default 'name source))
  (when matches
    (helm-insert-header-from-source source)
    (cl-loop with separate = nil
             with start = (point)
             with singleline = (null (assq 'multiline source))
             for m in matches
             for count from 1
             if singleline
             do (helm-insert-match m 'insert count source)
             else
             do (progn
                  (if separate
                      (helm-insert-candidate-separator)
                    (setq separate t))
                  (helm-insert-match m 'insert count source))
             finally (and (null singleline)
                          (put-text-property start (point)
                                             'helm-multiline t)))))

(defmacro helm--maybe-use-while-no-input (&rest body)
  "Wrap BODY in `helm-while-no-input' unless initializing a remote connection."
  `(progn
     (if (and (file-remote-p helm-pattern)
              (not (file-remote-p helm-pattern nil t)))
         ;; Tramp will ask for passwd, don't use `helm-while-no-input'.
         ,@body
       (helm-log "Using here `helm-while-no-input'")
       (helm-while-no-input ,@body))))

(defun helm--collect-matches (src-list)
  "Returns a list of matches for each source in SRC-LIST.

The resulting value is a list of lists, e.g ((a b c) (c d) (e f)) or
\(nil nil nil) for three sources when no matches found, however this
function can be interrupted by new input and in this case returns a
plain `nil' i.e not (nil), in this case `helm-update' is not rendering
the source, keeping previous candidates in display."
  (let ((matches (helm--maybe-use-while-no-input
                  (cl-loop for src in src-list
                           collect (helm-compute-matches src)))))
    (unless (eq matches t) matches)))


;;; Case fold search
;;
;;
(cl-defun helm-set-case-fold-search (&optional (pattern helm-pattern))
  "Used to set the value of `case-fold-search' in helm.
Return t or nil depending on the value of `helm-case-fold-search'
and `helm-pattern'."
  (let ((helm-case-fold-search
         (helm-aif (assq 'case-fold-search (helm-get-current-source))
             (cdr it)
           helm-case-fold-search))
        ;; Only parse basename for filenames
        ;; to avoid setting case sensitivity
        ;; when expanded directories contains upcase
        ;; characters.
        (bn-or-pattern (if (string-match "[~/]*" pattern)
                           (helm-basename pattern)
                         pattern)))
    (helm-set-case-fold-search-1 bn-or-pattern)))

(defun helm-set-case-fold-search-1 (pattern)
  (cl-case helm-case-fold-search
    (smart (let ((case-fold-search nil))
             (if (string-match "[[:upper:]]" pattern) nil t)))
    (t helm-case-fold-search)))


;;; Helm update
;;
(defun helm-update (&optional preselect source candidates)
  "Update candidates list in `helm-buffer' based on `helm-pattern'.
Argument PRESELECT is a string or regexp used to move selection
to a particular place after finishing update.
When SOURCE is provided update mode-line for this source, otherwise
the current source will be used.
Argument CANDIDATES when provided is used to redisplay these candidates
without recomputing them, it should be a list of lists."
  (helm-log "Start updating")
  (helm-kill-async-processes)
  ;; When persistent action have been called
  ;; we have two windows even with `helm-full-frame'.
  ;; So go back to one window when updating if `helm-full-frame'
  ;; is non-`nil'.
  (with-helm-window
    (when (and helm-onewindow-p
               (not (helm-action-window)))
      (delete-other-windows)))
  (with-current-buffer (helm-buffer-get)
    (set (make-local-variable 'helm-input-local) helm-pattern)
    (unwind-protect
         (let (sources matches)
           ;; Collect sources ready to be updated.
           (setq sources
                 (cl-loop for src in (helm-get-sources)
                          when (helm-update-source-p src)
                          collect src))
           ;; When no sources to update erase buffer
           ;; to avoid duplication of header and candidates
           ;; when next chunk of update will arrive,
           ;; otherwise the buffer is erased AFTER [1] the results
           ;; are computed.
           (unless sources (erase-buffer))
           ;; Compute matches without rendering the sources.
           ;; This prevent the helm-buffer flickering when constantly
           ;; updating.
           (helm-log "Matches: %S"
                     (setq matches (or candidates (helm--collect-matches sources))))
           ;; If computing matches finished and is not interrupted
           ;; erase the helm-buffer and render results (Fix #1157).
           (when matches ;; nil only when interrupted by helm-while-no-input.
             (erase-buffer)             ; [1]
             (cl-loop for src in sources
                      for mtc in matches
                      do (helm-render-source src mtc))
             ;; Move to first line only when there is matches
             ;; to avoid cursor moving upside down (issue #1703).
             (helm--update-move-first-line)
             (helm--reset-update-flag)))
      ;; When there is only one async source, update mode-line and run
      ;; `helm-after-update-hook' in `helm-output-filter--post-process',
      ;; when there is more than one source, update mode-line and run
      ;; `helm-after-update-hook' now even if an async source is
      ;; present and running in BG.
      (let ((src (or source (helm-get-current-source))))
        (unless (assq 'candidates-process src)
          (and src (helm-display-mode-line src 'force))
          (helm-log-run-hook 'helm-after-update-hook)))
      (when preselect
        (helm-log "Update preselect candidate %s" preselect)
        (helm-preselect preselect source))
      (setq helm--force-updating-p nil))
    (helm-log "end update")))

(defun helm-update-source-p (source)
  "Whether SOURCE need updating or not."
  (let ((len (string-width
              (if (assq 'multimatch source)
                  ;; Don't count spaces entered when using
                  ;; multi-match.
                  (replace-regexp-in-string " " "" helm-pattern)
                helm-pattern))))
    (and (or (not helm-source-filter)
             (member (assoc-default 'name source) helm-source-filter))
         (>= len
             (helm-aif (assq 'requires-pattern source) (or (cdr it) 1) 0))
         ;; Entering repeatedly these strings (*, ?) takes 100% CPU
         ;; and hang emacs on MacOs preventing deleting backward those
         ;; characters (issue #1802).
         (not (string-match-p "\\`[*]+\\'" helm-pattern))
         ;; These incomplete regexps hang helm forever
         ;; so defer update. Maybe replace spaces quoted when using
         ;; multi-match.
         (not (member (replace-regexp-in-string "\\s\\ " " " helm-pattern)
                      helm-update-blacklist-regexps)))))

(defun helm--update-move-first-line ()
  "Goto first line of `helm-buffer'."
  (goto-char (point-min))
  (helm-move-selection-common :where 'line
                              :direction 'next
                              :follow t))

(cl-defun helm-force-update (&optional preselect (recenter t))
  "Force recalculation and update of candidates.

Unlike `helm-update', this function re-evaluates `init' and
`update' attributes when present; also `helm-candidate-cache' is
not reinitialized, meaning candidates are not recomputed unless
pattern has changed.

Selection is preserved to current candidate if it still exists after
update or moved to PRESELECT, if specified.
The helm-window is recentered at the end when RECENTER is `t'
which is the default, RECENTER can be also a number in this case it is
passed as argument to `recenter'."
  (with-helm-window
    (let* ((source    (helm-get-current-source))
           (selection (helm-aif (helm-get-selection nil t source)
                          (regexp-quote it))))
      (setq helm--force-updating-p t)
      (mapc 'helm-force-update--reinit (helm-get-sources))
      (helm-update (or preselect selection) source)
      (and recenter (recenter (and (numberp recenter) recenter))))))

(defun helm-refresh ()
  "Force recalculation and update of candidates."
  (interactive)
  (with-helm-alive-p
    (helm-force-update)))
(put 'helm-refresh 'helm-only t)

(defun helm-force-update--reinit (source)
  "Reinit SOURCE by calling its update and init functions."
  ;; When using a specific buffer as cache, don't kill it.
  (helm-aif (and (null (bufferp (assoc-default
                                 (helm-attr 'name source)
                                 helm--candidate-buffer-alist)))
                 (helm-funcall-with-source
                  source 'helm-candidate-buffer))
      (kill-buffer it))
  (cl-dolist (attr '(update init))
    (helm-aif (assoc-default attr source)
        (helm-funcall-with-source source it)))
  (helm-remove-candidate-cache source))

(defun helm-redisplay-buffer ()
  "Redisplay candidates in `helm-buffer'.

Candidates are not recomputed, only redisplayed after modifying the
whole list of candidates in each source with functions found in
`redisplay' attribute of current source.  Note that candidates are
redisplayed with their display part with all properties included only.
This function is used in async sources to transform the whole list of
candidates from the sentinel functions (i.e when all candidates have
been computed) because other filters like `candidate-transformer' are
modifying only each chunk of candidates from process-filter as they
come in and not the whole list.  Use this for e.g sorting the whole
list of async candidates once computed.
Note: To ensure redisplay is done in async sources after helm
reached `candidate-number-limit' you will have also to redisplay your
candidates from `helm-async-outer-limit-hook'."
  (with-helm-buffer
    (let ((get-cands (lambda (source)
                       (let ((fns (assoc-default 'redisplay source))
                             candidates
                             helm-move-to-line-cycle-in-source
                             helm-allow-mouse)
                         (helm-goto-source source)
                         (helm-next-line)
                         (helm-awhile (condition-case-unless-debug nil
                                          (and (not (helm-pos-header-line-p))
                                               (helm-get-selection
                                                nil 'withprop source))
                                        (error nil))
                           (push it candidates)
                           (when (save-excursion
                                   (forward-line 1) (helm-end-of-source-p t))
                             (cl-return nil))
                           (helm-next-line))
                         (helm-funcall-with-source
                          source fns (nreverse candidates)))))
          (get-sources (lambda ()
                         (let (sources helm-move-to-line-cycle-in-source)
                           (helm-awhile (helm-get-current-source)
                             (push it sources)
                             (when (save-excursion
                                     (helm-move--end-of-source)
                                     (forward-line 1) (eobp))
                             (cl-return nil))
                             (helm-next-source))
                           (nreverse sources)))))
      (goto-char (point-min))
      (helm-update nil (helm-get-current-source)
                   (cl-loop with sources = (funcall get-sources)
                            for s in (helm-get-sources)
                            for name =  (assoc-default 'name s) collect
                            (when (cl-loop for src in sources thereis
                                           (string= name
                                                    (assoc-default 'name src)))
                                      (funcall get-cands s)))))))

(defun helm-remove-candidate-cache (source)
  "Remove SOURCE from `helm-candidate-cache'."
  (remhash (assoc-default 'name source) helm-candidate-cache))

(defun helm-insert-match (match insert-function &optional num source)
  "Insert MATCH into `helm-buffer' with INSERT-FUNCTION.
If MATCH is a cons cell then insert the car as display with
the cdr stored as real value in a `helm-realvalue' text property.
Args NUM and SOURCE are also stored as text property when specified as
respectively `helm-cand-num' and `helm-cur-source'."
  (let ((start     (point-at-bol (point)))
        (dispvalue (helm-candidate-get-display match))
        (realvalue (cdr-safe match))
        (map       (when helm-allow-mouse (make-sparse-keymap)))
        (inhibit-read-only t)
        end)
    (when (and (stringp dispvalue)
               (not (zerop (length dispvalue))))
      (funcall insert-function dispvalue)
      (setq end (point-at-eol))
      (put-text-property start end 'read-only nil)
      ;; Some sources with candidates-in-buffer have already added
      ;; 'helm-realvalue property when creating candidate buffer.
      (unless (get-text-property start 'helm-realvalue)
        (and realvalue
             (put-text-property start end
                                'helm-realvalue realvalue)))
      (when (and map
                 ;; Don't overwrite mouse properties when
                 ;; redisplaying.
                 (not (get-text-property start 'keymap)))
        (define-key map [mouse-1] 'helm-mouse-select-candidate)
        (define-key map [mouse-2] 'ignore)
        (define-key map [mouse-3] 'helm-select-action)
        (add-text-properties
         start end
         `(mouse-face highlight
           keymap ,map
           help-echo ,(helm-aif (get-text-property start 'help-echo)
                         (concat it "\nmouse-1: select candidate\nmouse-3: menu actions")
                       "mouse-1: select candidate\nmouse-3: menu actions"))))
      (when num
        (put-text-property start end 'helm-cand-num num))
      (when source
        (put-text-property start end 'helm-cur-source source))
      (funcall insert-function "\n"))))

(defun helm--mouse-reset-selection-help-echo ()
  (let* ((inhibit-read-only t)
         (start (overlay-start helm-selection-overlay))
         (end   (overlay-end helm-selection-overlay))
         (help-echo (get-text-property start 'help-echo)))
    (when (and help-echo
               (string-match "mouse-2: execute action" help-echo))
      (put-text-property
       start end
       'help-echo (replace-match "mouse-1: select candidate"
                                 t t help-echo)))))

(defun helm--bind-mouse-for-selection (pos)
  (let ((inhibit-read-only t)
        (map (get-text-property pos 'keymap)))
    (when map
      (define-key map [mouse-2] 'helm-maybe-exit-minibuffer)
      (put-text-property
       helm-selection-point
       (overlay-end helm-selection-overlay)
       'help-echo (helm-aif (get-text-property pos 'help-echo)
                      (if (string-match "mouse-1: select candidate" it)
                          (replace-match "mouse-2: execute action" t t it)
                          "mouse-2: execute action\nmouse-3: menu actions")
                    "mouse-2: execute action\nmouse-3: menu actions")))))

(defun helm-mouse-select-candidate (event)
  (interactive "e")
  (let* ((window (posn-window (event-end event)))
         (pos    (posn-point (event-end event))))
    (unwind-protect
         (with-current-buffer (window-buffer window)
           (if (and (helm-action-window)
                    (eql window (get-buffer-window helm-buffer)))
               (user-error "selection in helm-window not available while selecting action")
               (helm--mouse-reset-selection-help-echo)
               (goto-char pos)
               (when (helm-pos-multiline-p)
                 (goto-char (or (helm-get-previous-candidate-separator-pos)
                                (helm-get-previous-header-pos)))
                 (forward-line 1)))
           (helm-mark-current-line)
           (helm-follow-execute-persistent-action-maybe))
      (select-window (minibuffer-window))
      (set-buffer (window-buffer window)))))
(put 'helm-mouse-select-candidate 'helm-only t)

(defun helm-insert-header-from-source (source)
  "Insert SOURCE name in `helm-buffer' header.
Maybe insert, by overlay, additional info after the source name
if SOURCE has header-name attribute."
  (let ((name (assoc-default 'name source)))
    (helm-insert-header
     name
     (helm-aif (assoc-default 'header-name source)
         (helm-funcall-with-source source it name)))))

(defun helm-insert-header (name &optional display-string)
  "Insert header of source NAME into the helm buffer.
If DISPLAY-STRING is non-`nil' and a string value then display
this additional info after the source name by overlay."
  (unless (bobp)
    (let ((start (point)))
      (insert "\n")
      (put-text-property start (point) 'helm-header-separator t)))
  (let ((start (point)))
    (insert name)
    (put-text-property (point-at-bol)
                       (point-at-eol) 'helm-header t)
    (when display-string
      (overlay-put (make-overlay (point-at-bol) (point-at-eol))
                   'display display-string))
    (insert "\n")
    (put-text-property start (point) 'face 'helm-source-header)))

(defun helm-insert-candidate-separator ()
  "Insert separator of candidates into the helm buffer."
  (insert (propertize helm-candidate-separator 'face 'helm-separator))
  (put-text-property (point-at-bol)
                     (point-at-eol) 'helm-candidate-separator t)
  (insert "\n"))


;;; Async process
;;
(defun helm-output-filter (process output-string)
  "The `process-filter' function for helm async sources."
  (with-helm-quittable
    (helm-output-filter-1 (assoc process helm-async-processes) output-string)))

(defun helm-output-filter-1 (process-assoc output-string)
  (helm-log "output-string = %S" output-string)
  (with-current-buffer helm-buffer
    (let ((source (cdr process-assoc)))
      (save-excursion
        (helm-aif (assoc-default 'insertion-marker source)
            (goto-char it)
          (goto-char (point-max))
          (helm-insert-header-from-source source)
          (setcdr process-assoc
                  (append source `((insertion-marker . ,(point-marker))))))
        (helm-output-filter--process-source
         (car process-assoc) output-string source
         (helm-candidate-number-limit source))))
    (helm-output-filter--post-process)))

(defun helm-output-filter--process-source (process output-string source limit)
  (cl-dolist (candidate (helm-transform-candidates
                         (helm-output-filter--collect-candidates
                          (split-string output-string "\n")
                          (assq 'incomplete-line source))
                         source t))
    (setq candidate
          (helm--maybe-process-filter-one-by-one-candidate candidate source))
    (if (assq 'multiline source)
        (let ((start (point)))
          (helm-insert-candidate-separator)
          (helm-insert-match candidate 'insert-before-markers
                             (1+ (cdr (assq 'item-count source)))
                             source)
          (put-text-property start (point) 'helm-multiline t))
        (helm-insert-match candidate 'insert-before-markers
                           (1+ (cdr (assq 'item-count source)))
                           source))
    (cl-incf (cdr (assq 'item-count source)))
    (when (>= (assoc-default 'item-count source) limit)
      (helm-kill-async-process process)
      (helm-log-run-hook 'helm-async-outer-limit-hook)
      (cl-return))))

(defun helm-output-filter--collect-candidates (lines incomplete-line-info)
  "Collect LINES maybe completing the truncated first and last lines."
  ;; The output of process may come in chunks of any size, so the last
  ;; line of LINES could be truncated, this truncated line is stored
  ;; in INCOMPLETE-LINE-INFO to be concatenated with the first
  ;; incomplete line of the next arriving chunk. INCOMPLETE-LINE-INFO
  ;; is an attribute of source; it is created with an empty string
  ;; when the source is computed => (incomplete-line . "")
  (helm-log "incomplete-line-info = %S" (cdr incomplete-line-info))
  (butlast
   (cl-loop for line in lines
            ;; On start `incomplete-line-info' value is empty string.
            for newline = (helm-aif (cdr incomplete-line-info)
                              (prog1
                                  (concat it line)
                                (setcdr incomplete-line-info nil))
                            line)
            collect newline
            ;; Store last incomplete line (last chunk truncated) until
            ;; new output arrives. Previously storing 'line' in
            ;; incomplete-line-info assumed output was truncated in
            ;; only two chunks. But output could be large and
            ;; truncated in more than two chunks. Therefore store
            ;; 'newline' to contain the previous chunks (Issue #1187).
            finally do (setcdr incomplete-line-info newline))))

(defun helm-output-filter--post-process ()
  (helm-aif (get-buffer-window helm-buffer 'visible)
      (with-selected-window it
        (helm-skip-noncandidate-line 'next)
        (helm-mark-current-line nil 'nomouse)
        ;; FIXME Don't hardcode follow delay.
        (helm-follow-execute-persistent-action-maybe 0.5)
        (helm-display-mode-line (helm-get-current-source))
        (helm-log-run-hook 'helm-after-update-hook)
        (helm--reset-update-flag))))

(defun helm-process-deferred-sentinel-hook (process event file)
  "Defer remote processes in sentinels.
Meant to be called at the beginning of a sentinel process
function."
  (when (and (not (zerop helm-tramp-connection-min-time-diff))
             (string= event "finished\n")
             (or (file-remote-p file)
                 ;; `helm-suspend-update-flag'
                 ;; is non-`nil' here only during a
                 ;; running process, this will never be called
                 ;; when user set it explicitly with `C-!'.
                 helm-suspend-update-flag))
    (setq helm-suspend-update-flag t)
    ;; Kill the process but don't delete entry in
    ;; `helm-async-processes'.
    (helm-kill-async-process process)
    ;; When tramp opens the same connection twice in less than 5
    ;; seconds, it throws 'suppress, which calls the real-handler on
    ;; the main "Emacs". To avoid this [1] helm waits for 5 seconds
    ;; before updates yet allows user input during this delay. [1] In
    ;; recent Emacs versions, this has been fixed so tramp returns nil
    ;; in such conditions. Note: `tramp-connection-min-time-diff' cannot
    ;; have values less than 5 seconds otherwise the process dies.
    (run-at-time helm-tramp-connection-min-time-diff
                 nil (lambda ()
                       (when helm-alive-p ; Don't run timer fn after quit.
                         (setq helm-suspend-update-flag nil)
                         (helm-check-minibuffer-input))))))

(defun helm-kill-async-processes ()
  "Kill all asynchronous processes registered in `helm-async-processes'."
  (while helm-async-processes
    (helm-kill-async-process (caar helm-async-processes))
    (setq helm-async-processes (cdr helm-async-processes))))

(defun helm-kill-async-process (process)
  "Stop output from `helm-output-filter' and kill associated PROCESS."
  (set-process-filter process nil)
  (delete-process process))


;;; Actions
;;
(defun helm-execute-selection-action ()
  "Execute current action."
  (helm-log-run-hook 'helm-before-action-hook)
  ;; Position can be change when `helm-current-buffer'
  ;; is split, so jump to this position before executing action.
  (helm-current-position 'restore)
  (prog1 (helm-execute-selection-action-1)
    (helm-log-run-hook 'helm-after-action-hook)))

(defun helm-execute-selection-action-1 (&optional
                                        selection action
                                        preserve-saved-action)
  "Execute ACTION on current SELECTION.
If PRESERVE-SAVED-ACTION is non-`nil', then save the action."
  (helm-log "executing action")
  (setq action (helm-get-default-action
                (or action
                    helm-saved-action
                    (if (get-buffer helm-action-buffer)
                        (helm-get-selection helm-action-buffer)
                      (helm-get-actions-from-current-source)))))
  (helm-aif (and (not helm-in-persistent-action)
                 (get-buffer helm-action-buffer))
      (kill-buffer it))
  (let ((source (or helm-saved-current-source
                    (helm-get-current-source)))
        non-essential)
    (setq selection (helm-coerce-selection
                     (or selection
                         helm-saved-selection
                         (helm-get-selection nil nil source)
                         (and (assq 'accept-empty source) ""))
                     source))
    (unless preserve-saved-action (setq helm-saved-action nil))
    (when (and selection action) (funcall action selection))))

(defun helm-coerce-selection (selection source)
  "Apply coerce attribute function to SELECTION in SOURCE.
Coerce source with coerce function."
  (helm-aif (assoc-default 'coerce source)
      (helm-funcall-with-source source it selection)
    selection))

(defun helm-get-default-action (action)
  "Get the first ACTION value of action list in source."
  (if (and (listp action) (not (functionp action)))
      (cdar action)
    action))

(defun helm-select-action ()
  "Select an action for the currently selected candidate.
If action buffer is selected, back to the helm buffer."
  (interactive)
  (with-helm-alive-p
    (let ((src (helm-get-current-source)))
      (helm-log-run-hook 'helm-select-action-hook)
      (setq helm-saved-selection (helm-get-selection nil nil src))
      (with-selected-frame (with-helm-window (selected-frame))
        (prog1
            (helm-acond ((get-buffer-window helm-action-buffer 'visible)
                         (set-window-buffer it helm-buffer)
                         (helm--set-action-prompt 'restore)
                         (when (and helm-show-action-window-other-window
                                    helm-always-two-windows
                                    (eq helm-split-window-state 'vertical))
                           (delete-window it))
                         (kill-buffer helm-action-buffer)
                         (setq helm-saved-selection nil)
                         (helm-set-pattern helm-input 'noupdate))
                        (helm-saved-selection
                         (setq helm-saved-current-source src)
                         (let ((actions (helm-get-actions-from-current-source src))
                               helm-onewindow-p)
                           (if (functionp actions)
                               (message "Sole action: %s"
                                        (if (or (consp actions)
                                                (byte-code-function-p actions))
                                            "Anonymous" actions))
                               (helm-show-action-buffer actions)
                               ;; Be sure the minibuffer is entirely deleted (#907).
                               (helm--delete-minibuffer-contents-from "")
                               (helm--set-action-prompt)
                               (helm-check-minibuffer-input))))
                        (t (message "No Actions available")))
          (helm-display-mode-line (helm-get-current-source))
          (run-hooks 'helm-window-configuration-hook))))))
(put 'helm-select-action 'helm-only t)

(defun helm--set-action-prompt (&optional restore)
  (with-selected-window (minibuffer-window)
    (let ((inhibit-read-only t)
          (props '(face minibuffer-prompt
                   field t
                   read-only t
                   rear-nonsticky t
                   front-sticky t))
          (prt (if restore helm--prompt helm--action-prompt)))
      (erase-buffer)
      (insert (apply #'propertize prt props)))))

(defun helm-show-action-buffer (actions)
  (with-current-buffer (get-buffer-create helm-action-buffer)
    (erase-buffer)
    (buffer-disable-undo)
    (setq cursor-type nil)
    (set-window-buffer (if (and helm-show-action-window-other-window
                                helm-always-two-windows
                                (eq helm-split-window-state 'vertical))
                           (split-window (get-buffer-window helm-buffer)
                                         nil helm-show-action-window-other-window)
                           (get-buffer-window helm-buffer))
                       helm-action-buffer)
    (set (make-local-variable 'helm-sources)
         (list
          (helm-build-sync-source "Actions"
            :volatile t
            :nomark t
            :persistent-action #'ignore
            :persistent-help "DoNothing"
            :keymap 'helm-map
            :candidates actions
            :mode-line '("Action(s)" "\\<helm-map>\\[helm-select-action]:BackToCands RET/f1/f2/fn:NthAct")
            :candidate-transformer
             (lambda (candidates)
               (cl-loop for (i . j) in candidates
                        for count from 1
                        collect
                        (cons (concat (cond ((> count 12)
                                             "      ")
                                            ((< count 10)
                                             (format "[f%s]  " count))
                                            (t (format "[f%s] " count)))
                                      (propertize i 'face 'helm-action))
                              j)))
            :candidate-number-limit nil)))
    (set (make-local-variable 'helm-source-filter) nil)
    (set (make-local-variable 'helm-selection-overlay) nil)
    (helm-initialize-overlays helm-action-buffer)))


;; Selection of candidates

(defun helm-display-source-at-screen-top-maybe (unit)
  "Display source at the top of screen when UNIT value is 'source.
Returns nil for any other value of UNIT."
  (when (and helm-display-source-at-screen-top (eq unit 'source))
    (set-window-start (selected-window)
                      (save-excursion (forward-line -1) (point)))))

(defun helm-skip-noncandidate-line (direction)
  "Skip source header or candidates separator when going in DIRECTION.
DIRECTION is either 'next or 'previous.
Same as `helm-skip-header-and-separator-line' but ensure
point is moved to the right place when at bop or eob."
  (helm-skip-header-and-separator-line direction)
  (and (bobp) (forward-line 1))     ; Skip first header.
  (and (eobp) (forward-line -1)))   ; Avoid last empty line.

(defun helm-skip-header-and-separator-line (direction)
  "Skip source header or candidate separator when going to next/previous line.
DIRECTION is either 'next or 'previous."
  (let ((fn (cl-ecase direction
              (next 'eobp)
              (previous 'bobp))))
    (while (and (not (funcall fn))
                (or (helm-pos-header-line-p)
                    (helm-pos-candidate-separator-p)))
      (forward-line (if (and (eq direction 'previous)
                             (not (eq (point-at-bol) (point-min))))
                        -1 1)))))

(defun helm-display-mode-line (source &optional force)
  "Set up mode line and header line for `helm-buffer'.

SOURCE is a Helm source object.

Optional argument FORCE forces redisplay of the Helm buffer's
mode and header lines."
  (set (make-local-variable 'helm-mode-line-string)
       (helm-interpret-value (or (and (listp source) ; Check if source is empty.
                                      (assoc-default 'mode-line source))
                                 (default-value 'helm-mode-line-string))
                             source))
  (let ((follow (and (or (helm-follow-mode-p source)
                         (and helm-follow-mode-persistent
                              (member (assoc-default 'name source)
                                      helm-source-names-using-follow)))
                     " (HF)"))
        (marked (and helm-marked-candidates
                     (cl-loop with cur-name = (assoc-default 'name source)
                              for c in helm-marked-candidates
                              for name = (assoc-default 'name (car c))
                              when (string= name cur-name)
                              collect c))))
    ;; Setup mode-line.
    (if helm-mode-line-string
        (setq mode-line-format
              `(:propertize
                (" " mode-line-buffer-identification " "
                     (:eval (format "L%-3d" (helm-candidate-number-at-point)))
                     ,follow
                     " "
                     (:eval ,(and marked
                                  (propertize
                                   (format "M%d" (length marked))
                                   'face 'helm-visible-mark)))
                     (:eval (when ,helm--mode-line-display-prefarg
                              (let ((arg (prefix-numeric-value
                                          (or prefix-arg current-prefix-arg))))
                                (unless (= arg 1)
                                  (propertize (format " [prefarg:%s]" arg)
                                              'face 'helm-prefarg)))))
                     " "
                     (:eval (with-helm-buffer
                              (helm-show-candidate-number
                               (car-safe helm-mode-line-string))))
                     " " helm--mode-line-string-real " "
                     (:eval (make-string (window-width) ? )))
                keymap (keymap (mode-line keymap
                                          (mouse-1 . ignore)
                                          (down-mouse-1 . ignore)
                                          (drag-mouse-1 . ignore)
                                          (mouse-2 . ignore)
                                          (down-mouse-2 . ignore)
                                          (drag-mouse-2 . ignore)
                                          (mouse-3 . ignore)
                                          (down-mouse-3 . ignore)
                                          (drag-mouse-3 . ignore))))
              helm--mode-line-string-real
              (substitute-command-keys (if (listp helm-mode-line-string)
                                           (cadr helm-mode-line-string)
                                           helm-mode-line-string)))
        (setq mode-line-format (default-value 'mode-line-format)))
    ;; Setup header-line.
    (cond (helm-echo-input-in-header-line
           (setq force t)
           (helm--set-header-line))
          (helm-display-header-line
           (let ((hlstr (helm-interpret-value
                         (and (listp source)
                              (assoc-default 'header-line source))
                         source))
                 (endstr (make-string (window-width) ? )))
             (setq header-line-format
                   (propertize (concat " " hlstr endstr)
                               'face 'helm-header))))))
  (when force (force-mode-line-update)))

(defun helm--set-header-line (&optional update)
  (with-selected-window (minibuffer-window)
    (let* ((beg  (save-excursion (vertical-motion 0 (helm-window)) (point)))
           (end  (save-excursion (end-of-visual-line) (point)))
           ;; The visual line where the cursor is.
           (cont (buffer-substring beg end))
           (pref (propertize
                  " "
                  'display (if (string-match-p (regexp-opt `(,helm--prompt
                                                             ,helm--action-prompt))
                                               cont)
                               `(space :width ,helm-header-line-space-before-prompt)
                               (propertize
                                "->"
                                'face 'helm-header-line-left-margin))))
           (pos  (- (point) beg)))
      ;; Increment pos each time we find a "%" up to current-pos (#1648).
      (cl-loop for c across (buffer-substring-no-properties beg (point))
            when (eql c ?%) do (cl-incf pos))
      ;; Increment pos when cursor is on a "%" to make it visible in header-line
      ;; i.e "%%|" and not "%|%" (#1649).
      (when (eql (char-after) ?%) (setq pos (1+ pos)))
      (setq cont (replace-regexp-in-string "%" "%%" cont))
      (with-helm-buffer
        (setq header-line-format (concat pref cont " "))
        (put-text-property
         ;; Increment pos to handle the space before prompt (i.e `pref').
         (+ 1 pos) (+ 2 pos)
         'face
         ;; Don't just use 'cursor, this can hide the current character.
         (list :inverse-video t
               :foreground (face-background 'cursor)
               :background (face-background 'default))
         header-line-format)
        (when update (force-mode-line-update))))))

(defun helm--update-header-line ()
  ;; This should be used in `post-command-hook',
  ;; nowhere else.
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (helm--set-header-line t)))

(defun helm-hide-minibuffer-maybe ()
  "Hide minibuffer contents in a Helm session.
This function should normally go to `helm-minibuffer-set-up-hook'.
It has no effect if `helm-echo-input-in-header-line' is nil."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq cursor-type nil))))

(defun helm-show-candidate-number (&optional name)
  "Used to display candidate number in mode-line.
You can specify NAME of candidates e.g \"Buffers\" otherwise
it is \"Candidate\(s\)\" by default."
  (when helm-alive-p
    (unless (helm-empty-source-p)
      ;; Build a fixed width string when candidate-number < 1000
      (let* ((cand-name (or name "Candidate(s)"))
             (width (length (format "[999 %s]" cand-name))))
        (propertize
         (format (concat "%-" (number-to-string width) "s")
                 (format "[%s %s]"
                         (helm-get-candidate-number 'in-current-source)
                         cand-name))
         'face (if helm-suspend-update-flag
                   'helm-candidate-number-suspended
                   'helm-candidate-number))))))

(cl-defun helm-move-selection-common (&key where direction (follow t))
  "Move the selection marker to a new position.
Position is determined by WHERE and DIRECTION.
Key arg WHERE can be one of:
 - line
 - page
 - edge
 - source
Key arg DIRECTION can be one of:
 - previous
 - next
 - A source or a source name when used with :WHERE 'source."
  (let ((move-func (cl-case where
                     (line (cl-ecase direction
                             (previous 'helm-move--previous-line-fn)
                             (next 'helm-move--next-line-fn)))
                     (page (cl-ecase direction
                             (previous 'helm-move--previous-page-fn)
                             (next 'helm-move--next-page-fn)))
                     (edge (cl-ecase direction
                             (previous 'helm-move--beginning-of-buffer-fn)
                             (next 'helm-move--end-of-buffer-fn)))
                     (source (cl-case direction
                               (previous 'helm-move--previous-source-fn)
                               (next 'helm-move--next-source-fn)
                               (t (lambda () ; A source is passed as DIRECTION arg.
                                    (helm-move--goto-source-fn direction))))))))
    (unless (or (helm-empty-buffer-p (helm-buffer-get))
                (not (helm-window)))
      (with-helm-window
        (when helm-allow-mouse
          (helm--mouse-reset-selection-help-echo))
        (helm-log-run-hook 'helm-move-selection-before-hook)
        (funcall move-func)
        (and (memq direction '(next previous))
             (helm-skip-noncandidate-line direction))
        (when (helm-pos-multiline-p)
          (helm-move--beginning-of-multiline-candidate))
        (helm-display-source-at-screen-top-maybe where)
        (helm-mark-current-line)
        (when follow
          (helm-follow-execute-persistent-action-maybe))
        (helm-display-mode-line (helm-get-current-source))
        (helm-log-run-hook 'helm-move-selection-after-hook)))))

(defun helm-move--beginning-of-multiline-candidate ()
  (let ((header-pos (helm-get-previous-header-pos))
        (separator-pos (helm-get-previous-candidate-separator-pos)))
    (when header-pos
      (goto-char (if (or (null separator-pos)
                         (< separator-pos header-pos))
                     header-pos
                     separator-pos))
      (forward-line 1))))

(defun helm-move--previous-multi-line-fn ()
  (forward-line -1)
  (unless (helm-pos-header-line-p)
    (helm-skip-header-and-separator-line 'previous)
    (helm-move--beginning-of-multiline-candidate)))

(defun helm-move--previous-line-fn ()
  (if (not (helm-pos-multiline-p))
      (forward-line -1)
    (helm-move--previous-multi-line-fn))
  (when (and helm-move-to-line-cycle-in-source
             (helm-pos-header-line-p))
    (forward-line 1)
    (helm-move--end-of-source)
    ;; We are at end of helm-buffer
    ;; check if last candidate is a multiline candidate
    ;; and jump to it
    (when (and (eobp)
               (save-excursion (forward-line -1) (helm-pos-multiline-p)))
      (helm-move--previous-multi-line-fn))))

(defun helm-move--next-multi-line-fn ()
  (let ((header-pos (helm-get-next-header-pos))
        (separator-pos (helm-get-next-candidate-separator-pos)))
    (cond ((and separator-pos
                (or (null header-pos) (< separator-pos header-pos)))
           (goto-char separator-pos))
          (header-pos
           (goto-char header-pos)))))

(defun helm-move--next-line-fn ()
  (if (not (helm-pos-multiline-p))
      (forward-line 1)
    (helm-move--next-multi-line-fn))
  (when (and helm-move-to-line-cycle-in-source
             (or (save-excursion (and (helm-pos-multiline-p)
                                      (goto-char (overlay-end
                                                  helm-selection-overlay))
                                      (helm-end-of-source-p t)))
                 (helm-end-of-source-p t)))
    (helm-move--beginning-of-source)))

(defun helm-move--previous-page-fn ()
  (condition-case nil
      (scroll-down)
    (beginning-of-buffer (goto-char (point-min)))))

(defun helm-move--next-page-fn ()
  (condition-case nil
      (scroll-up)
    (end-of-buffer (goto-char (point-max)))))

(defun helm-move--beginning-of-buffer-fn ()
  (goto-char (point-min)))

(defun helm-move--end-of-buffer-fn ()
  (goto-char (point-max)))

(defun helm-move--end-of-source ()
  (helm-aif (helm-get-next-header-pos)
      (progn (goto-char it) (forward-line -2))
    (goto-char (point-max))))

(defun helm-move--beginning-of-source ()
  (helm-aif (helm-get-previous-header-pos)
      (progn (goto-char it)
             (forward-line 1))
    (goto-char (point-min))))

(defun helm-move--previous-source-fn ()
  (forward-line -1)
  (if (bobp)
      (goto-char (point-max))
    (helm-skip-header-and-separator-line 'previous))
  (goto-char (helm-get-previous-header-pos))
  (forward-line 1))

(defun helm-move--next-source-fn ()
  (goto-char (or (and (not (save-excursion
                             (forward-line 1) (eobp)))
                      ;; Empty source at eob are just
                      ;; not displayed unless they are dummy.
                      ;; Issue #1117.
                      (helm-get-next-header-pos))
                 (point-min))))

(defun helm-move--goto-source-fn (source-or-name)
  (goto-char (point-min))
  (let ((name (if (stringp source-or-name)
                  source-or-name
                  (assoc-default 'name source-or-name))))
    (if (or (null name) (string= name ""))
        (forward-line 1)
        (condition-case err
            (while (not (string= name (helm-current-line-contents)))
              (goto-char (helm-get-next-header-pos)))
          (error (helm-log "%S" err))))))

(defun helm-candidate-number-at-point ()
  (if helm-alive-p
      (with-helm-buffer
        (or (get-text-property (point) 'helm-cand-num) 1))
      (or (get-text-property (point) 'helm-cand-num) 1)))

(defun helm--next-or-previous-line (direction &optional arg)
  ;; Be sure to not use this in non--interactives calls.
  (let ((helm-move-to-line-cycle-in-source
         (and helm-move-to-line-cycle-in-source arg)))
    (if (and arg (> arg 1))
        (cl-loop with pos = (helm-candidate-number-at-point)
                 with cand-num = (helm-get-candidate-number t)
                 with iter = (min arg (if (eq direction 'next)
                                          (- cand-num pos)
                                          (min arg (1- pos))))
                 for count from 1
                 while (<= count iter)
                 do
                 (helm-move-selection-common :where 'line :direction direction))
        (helm-move-selection-common :where 'line :direction direction))))

(defun helm-previous-line (&optional arg)
  "Move selection to the ARG previous line(s).
Same behavior as `helm-next-line' when called with a numeric prefix arg."
  (interactive "p")
  (with-helm-alive-p
    (helm--next-or-previous-line 'previous arg)))
(put 'helm-previous-line 'helm-only t)

(defun helm-next-line (&optional arg)
  "Move selection to the next ARG line(s).
When numeric prefix arg is > than the number of candidates, then
move to the last candidate of current source (i.e. don't move to
next source)."
  (interactive "p")
  (with-helm-alive-p
    (helm--next-or-previous-line 'next arg)))
(put 'helm-next-line 'helm-only t)

(defun helm-previous-page ()
  "Move selection back with a pageful."
  (interactive)
  (with-helm-alive-p
    (helm-move-selection-common :where 'page :direction 'previous)))
(put 'helm-previous-page 'helm-only t)

(defun helm-next-page ()
  "Move selection forward with a pageful."
  (interactive)
  (with-helm-alive-p
    (helm-move-selection-common :where 'page :direction 'next)))
(put 'helm-next-page 'helm-only t)

(defun helm-beginning-of-buffer ()
  "Move selection at the top."
  (interactive)
  (with-helm-alive-p
    (helm-move-selection-common :where 'edge :direction 'previous)))
(put 'helm-beginning-of-buffer 'helm-only t)

(defun helm-end-of-buffer ()
  "Move selection at the bottom."
  (interactive)
  (with-helm-alive-p
    (helm-move-selection-common :where 'edge :direction 'next)))
(put 'helm-end-of-buffer 'helm-only t)

(defun helm-previous-source ()
  "Move selection to the previous source."
  (interactive)
  (with-helm-alive-p
    (helm-move-selection-common :where 'source :direction 'previous)))
(put 'helm-previous-source 'helm-only t)

(defun helm-next-source ()
  "Move selection to the next source."
  (interactive)
  (with-helm-alive-p
    (helm-move-selection-common :where 'source :direction 'next)))
(put 'helm-next-source 'helm-only t)

(defun helm-goto-source (&optional source-or-name)
  "Move the selection to the source named SOURCE-OR-NAME.

If SOURCE-OR-NAME is empty string or nil go to the first candidate of
first source."
  (helm-move-selection-common :where 'source :direction source-or-name))

(defun helm--follow-action (arg)
  (let ((helm--temp-follow-flag t) ; Needed in HFF.
        (in-follow-mode (helm-follow-mode-p)))
    ;; When follow-mode is already enabled, just go to next or
    ;; previous line.
    (when (or (eq last-command 'helm-follow-action-forward)
              (eq last-command 'helm-follow-action-backward)
              (eq last-command 'helm-execute-persistent-action)
              in-follow-mode)
      (if (> arg 0)
          (helm-move-selection-common :where 'line
                                      :direction 'next
                                      :follow nil)
          (helm-move-selection-common :where 'line
                                      :direction 'previous
                                      :follow nil)))
    (unless in-follow-mode
      (helm-execute-persistent-action))))

(defun helm-follow-action-forward ()
  "Go to next line and execute persistent action."
  (interactive)
  (with-helm-alive-p (helm--follow-action 1)))
(put 'helm-follow-action-forward 'helm-only t)

(defun helm-follow-action-backward ()
  "Go to previous line and execute persistent action."
  (interactive)
  (with-helm-alive-p (helm--follow-action -1)))
(put 'helm-follow-action-backward 'helm-only t)

(defun helm-mark-current-line (&optional resumep nomouse)
  "Move `helm-selection-overlay' to current line.
Note that this is unrelated to visible marks used for marking
candidates."
  (with-helm-window
    (when resumep
      (goto-char helm-selection-point))
    (move-overlay
     helm-selection-overlay (point-at-bol)
     (if (helm-pos-multiline-p)
         (let ((header-pos (helm-get-next-header-pos))
               (separator-pos (helm-get-next-candidate-separator-pos)))
           (or (and (null header-pos) separator-pos)
               (and header-pos separator-pos
                    (< separator-pos header-pos)
                    separator-pos)
               header-pos
               (point-max)))
       (1+ (point-at-eol))))
    (setq helm-selection-point (overlay-start helm-selection-overlay))
    (when (and helm-allow-mouse (null nomouse))
      (helm--bind-mouse-for-selection helm-selection-point))))

(defun helm-confirm-and-exit-minibuffer ()
  "Maybe ask for confirmation when exiting helm.
It is similar to `minibuffer-complete-and-exit' adapted to helm.
If `minibuffer-completion-confirm' value is 'confirm,
send minibuffer confirm message and exit on next hit.
If `minibuffer-completion-confirm' value is t,
don't exit and send message 'no match'."
  (interactive)
  (with-helm-alive-p
    (if (and (helm--updating-p)
             (null helm--reading-passwd-or-string))
        (progn (message "[Display not ready]")
               (sit-for 0.5) (message nil))
      (let* ((src (helm-get-current-source))
             (empty-buffer-p (with-current-buffer helm-buffer
                               (eq (point-min) (point-max))))
             (sel (helm-get-selection nil nil src))
             (unknown (and (not empty-buffer-p)
                           (string= (get-text-property
                                     0 'display
                                     (helm-get-selection nil 'withprop src))
                                    "[?]"))))
        (cond ((and (or empty-buffer-p unknown)
                    (eq minibuffer-completion-confirm 'confirm))
               (setq helm-minibuffer-confirm-state
                     'confirm)
               (setq minibuffer-completion-confirm nil)
               (minibuffer-message " [confirm]"))
              ((and (or empty-buffer-p
                        (unless (if minibuffer-completing-file-name
                                    (and minibuffer-completion-predicate
                                         (funcall minibuffer-completion-predicate sel))
                                  (and (stringp sel)
                                       ;; SEL may be a cons cell when helm-comp-read
                                       ;; is called directly with a collection composed
                                       ;; of (display . real) and real is a cons cell.
                                       (try-completion sel minibuffer-completion-table
                                                       minibuffer-completion-predicate)))
                          unknown))
                    (eq minibuffer-completion-confirm t))
               (minibuffer-message " [No match]"))
              (t
               (setq helm-minibuffer-confirm-state nil)
               (helm-exit-minibuffer)))))))
(put 'helm-confirm-and-exit-minibuffer 'helm-only t)

(add-hook 'helm-after-update-hook 'helm-confirm-and-exit-hook)

(defun helm-confirm-and-exit-hook ()
  "Restore `minibuffer-completion-confirm' when helm update."
  (unless (or (eq minibuffer-completion-confirm t)
              (not helm-minibuffer-confirm-state))
    (setq minibuffer-completion-confirm
          helm-minibuffer-confirm-state)))

(defun helm-read-string (prompt &optional initial-input history
                                  default-value inherit-input-method)
  "Same as `read-string' but for reading string from a helm session."
  (let ((helm--reading-passwd-or-string t))
    (read-string
     prompt initial-input history default-value inherit-input-method)))

(defun helm--updating-p ()
  ;; helm timer is between two cycles.
  ;; IOW `helm-check-minibuffer-input' haven't yet compared input
  ;; and `helm-pattern'.
  (or (not (equal (minibuffer-contents) helm-pattern))
      ;; `helm-check-minibuffer-input' have launched `helm-update'.
      helm--in-update))

(defun helm-maybe-exit-minibuffer ()
  (interactive)
  (with-helm-alive-p
    (if (and (helm--updating-p)
             (null helm--reading-passwd-or-string))
        (progn
          (message "[Display not ready]")
          (sit-for 0.5) (message nil)
          (helm-update))
        (helm-exit-minibuffer))))
(put 'helm-maybe-exit-minibuffer 'helm-only t)

(defun helm-exit-minibuffer ()
  "Select the current candidate by exiting the minibuffer."
  (unless helm-current-prefix-arg
    (setq helm-current-prefix-arg current-prefix-arg))
  (setq helm-exit-status 0)
  (helm-log-run-hook 'helm-exit-minibuffer-hook)
  (exit-minibuffer))

(defun helm-keyboard-quit ()
  "Quit minibuffer in helm.
If action buffer is displayed, kill it."
  (interactive)
  (with-helm-alive-p
    (when (get-buffer-window helm-action-buffer 'visible)
      (kill-buffer helm-action-buffer))
    (setq helm-exit-status 1)
    (abort-recursive-edit)))
(put 'helm-keyboard-quit 'helm-only t)

(defun helm-get-next-header-pos ()
  "Return the position of the next header from point."
  (next-single-property-change (point) 'helm-header))

(defun helm-get-previous-header-pos ()
  "Return the position of the previous header from point."
  (previous-single-property-change (point) 'helm-header))

(defun helm-pos-multiline-p ()
  "Return non-`nil' if the current position is in the multiline source region."
  (get-text-property (point) 'helm-multiline))

(defun helm-get-next-candidate-separator-pos ()
  "Return the position of the next candidate separator from point."
  (let ((hp (helm-get-next-header-pos)))
    (helm-aif (next-single-property-change (point) 'helm-candidate-separator)
        (or
         ;; Be sure we don't catch
         ;; the separator of next source.
         (and hp (< it hp) it)
         ;; The separator found is in next source
         ;; we are at last cand, so use the header pos.
         (and hp (< hp it) hp)
         ;; A single source, just try next separator.
         it))))

(defun helm-get-previous-candidate-separator-pos ()
  "Return the position of the previous candidate separator from point."
  (previous-single-property-change (point) 'helm-candidate-separator))

(defun helm-pos-header-line-p ()
  "Return t if the current line is a header line."
  (or (get-text-property (point-at-bol) 'helm-header)
      (get-text-property (point-at-bol) 'helm-header-separator)))

(defun helm-pos-candidate-separator-p ()
  "Return t if the current line is a candidate separator."
  (get-text-property (point-at-bol) 'helm-candidate-separator))


;;; Debugging
;;
;;
(defun helm-debug-output ()
  "Show all helm-related variables at this time."
  (interactive)
  (with-helm-alive-p
    (helm-help-internal " *Helm Debug*" 'helm-debug-output-function)))
(put 'helm-debug-output 'helm-only t)

(defun helm-debug-output-function (&optional vars)
  (message "Calculating all helm-related values...")
  (insert "If you debug some variables or forms, set `helm-debug-variables'
to a list of forms.\n\n")
  (cl-dolist (v (or vars
                    helm-debug-variables
                    (apropos-internal "^helm-" 'boundp)))
    (insert "** "
            (pp-to-string v) "\n"
            (pp-to-string (with-current-buffer helm-buffer (eval v))) "\n"))
  (message "Calculating all helm-related values...Done"))

(defun helm-enable-or-switch-to-debug ()
  "First hit enable helm debugging, second hit switch to debug buffer."
  (interactive)
  (with-helm-alive-p
    (if helm-debug
        (helm-run-after-exit
         #'helm-debug-open-last-log)
        (setq helm-debug t)
        (with-helm-buffer (setq truncate-lines nil))
        (message "Debugging enabled"))))
(put 'helm-enable-or-switch-to-debug 'helm-only t)


;; Misc
(defun helm-kill-buffer-hook ()
  "Remove tick entry from `helm-tick-hash' and remove buffer from
`helm-buffers' when killing a buffer."
  (cl-loop for key being the hash-keys in helm-tick-hash
        if (string-match (format "^%s/" (regexp-quote (buffer-name))) key)
        do (remhash key helm-tick-hash))
  (setq helm-buffers (remove (buffer-name) helm-buffers)))
(add-hook 'kill-buffer-hook 'helm-kill-buffer-hook)

(defun helm-preselect (candidate-or-regexp &optional source)
  "Move selection to CANDIDATE-OR-REGEXP on Helm start.

CANDIDATE-OR-REGEXP can be a:

- String
- Cons cell of two strings
- Nullary function, which moves to a candidate

When CANDIDATE-OR-REGEXP is a cons cell, tries moving to first
element of the cons cell, then the second, and so on. This allows
selection of duplicate candidates after the first.

Optional argument SOURCE is a Helm source object."
  (with-helm-window
    (when candidate-or-regexp
      (if source
          (helm-goto-source source)
          (goto-char (point-min))
          (forward-line 1))
      (if (functionp candidate-or-regexp)
          (funcall candidate-or-regexp)
          (let ((start (point)) mp)
            (helm-awhile (if (consp candidate-or-regexp)
                             (and (re-search-forward (car candidate-or-regexp) nil t)
                                  (re-search-forward (cdr candidate-or-regexp) nil t))
                             (re-search-forward candidate-or-regexp nil t))
              ;; If search fall on an header line continue loop
              ;; until it match or fail (Issue #1509).
              (unless (helm-pos-header-line-p) (cl-return (setq mp it))))
            (goto-char (or mp start)))))
    (forward-line 0) ; Avoid scrolling right on long lines.
    (when (helm-pos-multiline-p)
      (helm-move--beginning-of-multiline-candidate))
    (when (helm-pos-header-line-p) (forward-line 1))
    (when helm-allow-mouse
      (helm--mouse-reset-selection-help-echo))
    (helm-mark-current-line)
    (helm-display-mode-line (or source (helm-get-current-source)))
    (helm-log-run-hook 'helm-after-preselection-hook)))

(defun helm-delete-current-selection ()
  "Delete the currently selected item."
  (with-helm-window
    (cond ((helm-pos-multiline-p)
           (helm-aif (helm-get-next-candidate-separator-pos)
               (delete-region (point-at-bol)
                              (1+ (progn (goto-char it) (point-at-eol))))
             ;; last candidate
             (goto-char (helm-get-previous-candidate-separator-pos))
             (delete-region (point-at-bol) (point-max)))
           (when (helm-end-of-source-p)
             (goto-char (or (helm-get-previous-candidate-separator-pos)
                            (point-min)))
             (forward-line 1)))
          (t
           (delete-region (point-at-bol) (1+ (point-at-eol)))
           (when (helm-end-of-source-p t)
             (let ((headp (save-excursion
                            (forward-line -1)
                            (not (helm-pos-header-line-p)))))
               (and headp (forward-line -1))))))
    (unless (helm-end-of-source-p t)
      (helm-mark-current-line))))

(defun helm-end-of-source-1 (n at-point)
  (save-excursion
    (if (and (helm-pos-multiline-p) (null at-point))
        (null (helm-get-next-candidate-separator-pos))
        (forward-line (if at-point 0 n))
        (or (eq (point-at-bol) (point-at-eol))
            (helm-pos-header-line-p)
            (if (< n 0) (bobp) (eobp))))))

(defun helm-end-of-source-p (&optional at-point)
  "Return non-`nil' if we are at eob or end of source."
  (helm-end-of-source-1 1 at-point))

(defun helm-beginning-of-source-p (&optional at-point)
  "Return non-`nil' if we are at bob or beginning of source."
  (helm-end-of-source-1 -1 at-point))

(defun helm--edit-current-selection-internal (func)
  (with-helm-window
    (forward-line 0)
    (let ((realvalue (get-text-property (point) 'helm-realvalue))
          (multiline (get-text-property (point) 'helm-multiline)))
      (funcall func)
      (forward-line 0)
      (and realvalue
           (put-text-property (point) (point-at-eol)
                              'helm-realvalue realvalue))
      (and multiline
           (put-text-property (point)
                              (or (helm-get-next-candidate-separator-pos)
                                  (point-max))
                              'helm-multiline multiline))
      (helm-mark-current-line))))

(defmacro helm-edit-current-selection (&rest forms)
  "Evaluate FORMS at current selection in the helm buffer.
Used generally to modify current selection."
  (declare (indent 0) (debug t))
  `(helm--edit-current-selection-internal
    (lambda () ,@forms)))

(defun helm--delete-minibuffer-contents-from (from-str)
  ;; Giving an empty string value to FROM-STR delete all.
  (let ((input (minibuffer-contents)))
    (helm-reset-yank-point)
    (if (> (length input) 0)
        ;; minibuffer is not empty, delete contents from end
        ;; of FROM-STR and update.
        (helm-set-pattern from-str)
        ;; minibuffer is already empty, force update.
        (helm-force-update))))

(defun helm-delete-minibuffer-contents (&optional arg)
  "Delete minibuffer contents.
When `helm-delete-minibuffer-contents-from-point' is non-`nil',
delete minibuffer contents from point instead of deleting all.
Giving a prefix arg reverses this behavior.
When at the end of minibuffer, deletes all."
  (interactive "P")
  (let ((str (if helm-delete-minibuffer-contents-from-point
                 (if (or arg (eobp))
                     "" (helm-minibuffer-completion-contents))
               (if (and arg (not (eobp)))
                   (helm-minibuffer-completion-contents) ""))))
    (helm--delete-minibuffer-contents-from str)))


;;; helm-source-in-buffer.
;;
(defun helm-candidates-in-buffer (&optional source)
  "The top level function used to store candidates with `helm-source-in-buffer'.

Candidates are stored in a buffer generated internally
by `helm-candidate-buffer' function.
Each candidate must be placed in one line.

The buffer is created and fed in the init attribute function of helm.

e.g:

     (helm-build-in-buffer-source \"test\"
       :init (lambda ()
               (helm-init-candidates-in-buffer
                   'global '(foo foa fob bar baz))))

A shortcut can be used to simplify:

     (helm-build-in-buffer-source \"test\"
       :data '(foo foa fob bar baz))

By default, `helm' makes candidates by evaluating the
candidates function, then narrows them by `string-match' for each
candidate.

But this is slow for large number of candidates. The new way is
to store all candidates in a buffer and then narrow
with `re-search-forward'. Search function is customizable by search
attribute. The important point is that buffer processing is MUCH
FASTER than string list processing and is the Emacs way.

The init function writes all candidates to a newly-created
candidate buffer.  The candidates buffer is created or specified
by `helm-candidate-buffer'.  Candidates are stored in a line.

The candidates function narrows all candidates, IOW creates a
subset of candidates dynamically.

Class `helm-source-in-buffer' is implemented with three attributes:

    (candidates . helm-candidates-in-buffer)
    (volatile)
    (match identity)

The volatile attribute is needed because `helm-candidates-in-buffer'
creates candidates dynamically and need to be called every
time `helm-pattern' changes.

Because `helm-candidates-in-buffer' plays the role of `match' attribute
function, specifying `(match identity)' makes the source slightly faster.

However if source contains `match-part' attribute, match is computed only
on part of candidate returned by the call of function provided by this attribute.
The function should have one arg, candidate, and return only
a specific part of candidate.

To customize `helm-candidates-in-buffer' behavior,
use `search', `get-line' and `match-part' attributes."
  (let ((src (or source (helm-get-current-source))))
    (helm-candidates-in-buffer-1
     (helm-candidate-buffer)
     helm-pattern
     (or (assoc-default 'get-line src)
         #'buffer-substring-no-properties)
     (or (assoc-default 'search src)
         '(helm-candidates-in-buffer-search-default-fn))
     (helm-candidate-number-limit src)
     (helm-attr 'match-part)
     src)))

(defun helm-candidates-in-buffer-search-default-fn (pattern)
  "Search PATTERN with `re-search-forward' with bound and noerror args."
  (condition-case _err
      (re-search-forward pattern nil t)
    (invalid-regexp nil)))

(defun helm-candidates-in-buffer-1 (buffer pattern get-line-fn
                                    search-fns limit
                                    match-part-fn source)
  "Return the list of candidates inserted in BUFFER matching PATTERN."
  ;; buffer == nil when candidates buffer does not exist.
  (when buffer
    (with-current-buffer buffer
      (let ((inhibit-point-motion-hooks t)
            (start-point (1- (point-min))))
        (goto-char start-point)
        (if (string= pattern "")
            (helm-initial-candidates-from-candidate-buffer
             get-line-fn limit)
          (helm-search-from-candidate-buffer
           pattern get-line-fn search-fns limit
           start-point match-part-fn source))))))


(defun helm-search-from-candidate-buffer (pattern get-line-fn search-fns
                                          limit start-point match-part-fn source)
  (let ((inhibit-read-only t))
    (helm--search-from-candidate-buffer-1
     (lambda ()
       (cl-loop with hash = (make-hash-table :test 'equal)
                with allow-dups = (assq 'allow-dups source)
                with case-fold-search = (helm-set-case-fold-search)
                with count = 0
                for iter from 1
                for searcher in search-fns
                do (progn
                     (goto-char start-point)
                     ;; The character at start-point is a newline,
                     ;; if pattern match it that's mean we are
                     ;; searching for newline in buffer, in this
                     ;; case skip this false line.
                     ;; See comment >>>[1] in
                     ;; `helm--search-from-candidate-buffer-1'.
                     (and (condition-case nil
                              (looking-at pattern)
                            (invalid-regexp nil))
                          (forward-line 1)))
                nconc
                (cl-loop with pos-lst
                         while (and (setq pos-lst (funcall searcher pattern))
                                    (not (eobp))
                                    (< count limit))
                         for cand = (apply get-line-fn
                                           (if (and pos-lst (listp pos-lst))
                                               pos-lst
                                               (list (point-at-bol) (point-at-eol))))
                         when (and match-part-fn
                                   (not (get-text-property 0 'match-part cand)))
                         do (setq cand
                                  (propertize cand 'match-part (funcall match-part-fn cand)))
                         for dup = (gethash cand hash)
                         when (and (or (and allow-dups dup (= dup iter))
                                       (null dup))
                                   (or
                                    ;; Always collect when cand is matched
                                    ;; by searcher funcs and match-part attr
                                    ;; is not present.
                                    (and (not match-part-fn)
                                         (not (consp pos-lst)))
                                    ;; If match-part attr is present, or if SEARCHER fn
                                    ;; returns a cons cell, collect PATTERN only if it
                                    ;; match the part of CAND specified by
                                    ;; the match-part func.
                                    (helm-search-match-part cand pattern)))
                         do (progn
                              (puthash cand iter hash)
                              (helm--maybe-process-filter-one-by-one-candidate cand source)
                              (cl-incf count))
                         and collect cand))))))

(defun helm-search-match-part (candidate pattern)
  "Match PATTERN only on match-part property value of CANDIDATE.

Because `helm-search-match-part' maybe called even if unspecified
in source (negation or fuzzy), the part to match fallback to the whole
candidate even if match-part haven't been computed by match-part-fn
and stored in the match-part property."
  (let ((part (or (get-text-property 0 'match-part candidate)
                  candidate))
        (fuzzy-regexp (cadr (gethash 'helm-pattern helm--fuzzy-regexp-cache)))
        (matchfn (if helm-migemo-mode
                     'helm-mm-migemo-string-match 'string-match)))
    (if (string-match " " pattern)
        (cl-loop for i in (helm-mm-split-pattern pattern) always
                 (if (string-match "\\`!" i)
                     (not (funcall matchfn (substring i 1) part))
                     (funcall matchfn i part)))
        (if (string-match "\\`!" pattern)
            (if helm--in-fuzzy
                ;; Fuzzy regexp have already been
                ;; computed with substring 1.
                (not (string-match fuzzy-regexp part))
                (not (funcall matchfn (substring pattern 1) part)))
            (funcall matchfn (if helm--in-fuzzy fuzzy-regexp pattern) part)))))

(defun helm-initial-candidates-from-candidate-buffer (get-line-fn limit)
  (delq nil (cl-loop for i from 1 to limit
                     until (eobp)
                     collect (funcall get-line-fn
                                      (point-at-bol) (point-at-eol))
                     do (forward-line 1))))

(defun helm--search-from-candidate-buffer-1 (search-fn)
  ;; We are adding a newline at bob and at eol
  ;; and removing these newlines afterward.
  ;; This is a bad hack that should be removed.
  ;; To avoid matching the empty line at first line
  ;; when searching with e.g occur and "^$" just
  ;; forward-line before searching (See >>>[1] above).
  (goto-char (point-min))
  (insert "\n")
  (goto-char (point-max))
  (insert "\n")
  (unwind-protect
       (funcall search-fn)
    (goto-char (point-min))
    (delete-char 1)
    (goto-char (1- (point-max)))
    (delete-char 1)
    (set-buffer-modified-p nil)))

(defun helm-candidate-buffer (&optional buffer-spec)
  "Register and return a buffer storing candidates of current source.

This is used to initialize a buffer for storing candidates for a
candidates-in-buffer source, candidates will be searched in this
buffer and displayed in `helm-buffer'.
This should be used only in init functions, don't relay on this in
other places unless you know what you are doing.

This function is still in public API only for backward compatibility,
you should use instead `helm-init-candidates-in-buffer' for
initializing your sources.

Internally, this function is called without argument and returns the
buffer corresponding to current source i.e `helm--source-name' which
is available in only some places.

Acceptable values of BUFFER-SPEC:

- global (a symbol)
  Create a new global candidates buffer,
  named \" *helm candidates:SOURCE*\".
  This is used by `helm-init-candidates-in-buffer' and it is
  the most common usage of BUFFER-SPEC.
  The buffer will be killed and recreated at each new helm-session.

- local (a symbol)
  Create a new local candidates buffer,
  named \" *helm candidates:SOURCE*HELM-CURRENT-BUFFER\".
  You may want to use this when you want to have a different buffer
  each time source is used from a different `helm-current-buffer'.
  The buffer is erased and refilled at each new session but not killed.
  You probably don't want to use this value for BUFFER-SPEC.

- nil (omit)
  Only return the candidates buffer of current source if found.
  
- A buffer
  Register a buffer as a candidates buffer.
  The buffer needs to exists, it is not created.
  This allow you to use the buffer as a cache, it is faster because
  the buffer is already drawn, but be careful when using this as you
  may mangle your buffer depending what you write in your init(s)
  function, IOW don't modify the contents of the buffer in init(s)
  function but in a transformer.
  The buffer is not erased nor deleted.
  Generally it is safer to use a copy of buffer inserted
  in a global or local buffer.
  
If for some reasons a global buffer and a local buffer exist and are
belonging to the same source, the local buffer takes precedence on the
global one and is used instead.

When forcing update only the global and local buffers are killed
before running again the init function."
  (let ((global-bname (format " *helm candidates:%s*"
                              helm--source-name))
        (local-bname (format " *helm candidates:%s*%s"
                             helm--source-name
                             (buffer-name helm-current-buffer))))
    (when buffer-spec
      ;; Register buffer in `helm--candidate-buffer-alist'.
      ;; This is used only to retrieve buffer associated to current source
      ;; when using named buffer as value of BUFFER-SPEC.
      (setq helm--candidate-buffer-alist
            (cons (cons helm--source-name buffer-spec)
                  (delete (assoc helm--source-name
                                 helm--candidate-buffer-alist)
                          helm--candidate-buffer-alist)))
      ;; When using global or local as value of CREATE-OR-BUFFER
      ;; create the buffer global-bname or local-bname, otherwise
      ;; reuse the named buffer.
      (unless (bufferp buffer-spec)
        ;; Global buffers are killed and recreated.
        (and (eq buffer-spec 'global)
             (buffer-live-p (get-buffer global-bname))
             (kill-buffer global-bname))
        ;; Create global or local buffer.
        ;; Local buffer, once created are reused and a new one
        ;; is created when `helm-current-buffer' change across sessions.
        (with-current-buffer (get-buffer-create
                              (cl-ecase buffer-spec
                                (global global-bname)
                                (local  local-bname)))
          ;; We need a buffer not read-only to perhaps insert later
          ;; text coming from read-only buffers (issue #1176).
          (set (make-local-variable 'buffer-read-only) nil)
          ;; Undo is automatically disabled in buffer names starting
          ;; with a space, so no need to disable it.
          (erase-buffer)
          (font-lock-mode -1))))
    ;; Finally return the candidates buffer.
    (helm-acond ((get-buffer local-bname))
                ((get-buffer global-bname))
                ((assoc-default helm--source-name helm--candidate-buffer-alist)
                 (and (or (stringp it) (bufferp it))
                      (buffer-live-p (get-buffer it))
                      it)))))

(defun helm-init-candidates-in-buffer (buffer-spec data)
  "Register BUFFER-SPEC with DATA for a helm candidates-in-buffer session.

Arg BUFFER-SPEC can be a buffer-name (stringp), a buffer-spec object
\(bufferp), or a symbol, either 'local or 'global which is passed to
`helm-candidate-buffer'.
The most common usage of BUFFER-SPEC is 'global.

Arg DATA can be either a list or a plain string.
Returns the resulting buffer.

Use this in your init function to register a buffer for a
`helm-source-in-buffer' session and feed it with DATA.
You probably don't want to bother with this and use the :data slot
when initializing a source with `helm-source-in-buffer' class."
  (declare (indent 1))
  (let ((caching (and (or (stringp buffer-spec)
                          (bufferp buffer-spec))
                      (buffer-live-p (get-buffer buffer-spec))))
        (buf (helm-candidate-buffer
              (if (or (stringp buffer-spec)
                      (bufferp buffer-spec))
                  (get-buffer-create buffer-spec)
                buffer-spec)))) ; a symbol 'global or 'local.
    (unless caching
      (with-current-buffer buf
        (erase-buffer)
        (cond ((listp data)
               (insert (mapconcat (lambda (i)
                                    (cond ((symbolp i) (symbol-name i))
                                          ((numberp i) (number-to-string i))
                                          (t i)))
                                  data "\n")))
              ((stringp data) (insert data))))
      buf)))


;;; Resplit helm window
;;
;;
(defun helm-toggle-resplit-window ()
  "Toggle resplit helm window, vertically or horizontally."
  (interactive)
  (with-helm-alive-p
    (if (= (length (window-list nil 1)) 2)
        (progn
          (when helm-prevent-escaping-from-minibuffer
            (helm-prevent-switching-other-window :enabled nil))
          (unwind-protect
               (with-helm-window
                 (cond ((or helm-full-frame (one-window-p t))
                        (user-error "Attempt to resplit a single window"))
                       ((helm-action-window)
                        (user-error "Can't resplit while selecting actions"))
                       (t
                        (let ((before-height (window-height)))
                          (delete-window)
                          (set-window-buffer
                           (select-window
                            (if (= (window-height) before-height) ; initial split was horizontal.
                                ;; Split window vertically with `helm-buffer' placed
                                ;; on the good side according to actual value of
                                ;; `helm-split-window-default-side'.
                                (prog1
                                    (cond ((or (eq helm-split-window-default-side 'above)
                                               (eq helm-split-window-default-side 'left))
                                           (split-window
                                            (selected-window) nil 'above))
                                          (t (split-window-vertically)))
                                  (setq helm-split-window-state 'vertical))
                                ;; Split window vertically, same comment as above.
                                (setq helm-split-window-state 'horizontal)
                                (cond ((or (eq helm-split-window-default-side 'left)
                                           (eq helm-split-window-default-side 'above))
                                       (split-window (selected-window) nil 'left))
                                      (t (split-window-horizontally)))))
                           helm-buffer))))
                 (setq helm--window-side-state (helm--get-window-side-state)))
            (when helm-prevent-escaping-from-minibuffer
              (helm-prevent-switching-other-window :enabled t))))
        (error "current window configuration not suitable for splitting"))))
(put 'helm-toggle-resplit-window 'helm-only t)

;; Utility: Resize helm window.
(defun helm-enlarge-window-1 (n)
  "Enlarge or narrow helm window.
If N is positive enlarge, if negative narrow."
  (unless helm-full-frame
    (let ((horizontal-p (eq helm-split-window-state 'horizontal)))
      (with-helm-window
        (enlarge-window n horizontal-p)))))

(defun helm-narrow-window ()
  "Narrow helm window."
  (interactive)
  (with-helm-alive-p
    (helm-enlarge-window-1 -1)))
(put 'helm-narrow-window 'helm-only t)

(defun helm-enlarge-window ()
  "Enlarge helm window."
  (interactive)
  (with-helm-alive-p
    (helm-enlarge-window-1 1)))
(put 'helm-enlarge-window 'helm-only t)

(defun helm-swap-windows ()
  "Swap window holding `helm-buffer' with other window."
  (interactive)
  (with-helm-alive-p
    (if (= (length (window-list nil 1)) 2)
        (cond ((and helm-full-frame (one-window-p t))
               (user-error "Can't swap windows in a single window"))
              ((helm-action-window)
               (user-error "Can't resplit while selecting actions"))
              (t
               (let* ((w1          (helm-window))
                      (split-state (eq helm-split-window-state 'horizontal))
                      (w1size      (window-total-size w1 split-state))
                      (b1          (window-buffer w1)) ; helm-buffer
                      (s1          (window-start w1))
                      (cur-frame   (window-frame w1))
                      (w2          (with-selected-window (helm-window)
                                     ;; Don't try to display helm-buffer
                                     ;; in a dedicated window.
                                     (get-window-with-predicate
                                      (lambda (w) (not (window-dedicated-p w)))
                                      1 cur-frame)))
                      (w2size      (window-total-size w2 split-state))
                      (b2          (window-buffer w2)) ; probably helm-current-buffer
                      (s2          (window-start w2))
                      resize)
                 (with-selected-frame (window-frame w1)
                   (helm-replace-buffer-in-window w1 b1 b2)
                   (helm-replace-buffer-in-window w2 b2 b1)
                   (setq resize
                         (cond ( ;; helm-window is smaller than other window.
                                (< w1size w2size)
                                (- (- (max w2size w1size)
                                      (min w2size w1size))))
                               ( ;; helm-window is larger than other window.
                                (> w1size w2size)
                                (- (max w2size w1size)
                                   (min w2size w1size)))
                               ( ;; windows have probably same size.
                                t nil)))
                   ;; Maybe resize the window holding helm-buffer.
                   (and resize (window-resize w2 resize split-state))
                   (set-window-start w1 s2 t)
                   (set-window-start w2 s1 t))
                 (setq helm--window-side-state (helm--get-window-side-state)))))
        (error "current window configuration not suitable for splitting"))))
(put 'helm-swap-windows 'helm-only t)

(defun helm--get-window-side-state ()
  "Return the position of `helm-window' from `helm-current-buffer'.
Possible values are 'left 'right 'below or 'above."
  (let ((side-list '(left right below above)))
    (cl-loop for side in side-list
          thereis (and (equal (helm-window)
                              (window-in-direction
                               side (get-buffer-window helm-current-buffer t)
                               t))
                       side))))

(defun helm-replace-buffer-in-window (window buffer1 buffer2)
  "Replace BUFFER1 by BUFFER2 in WINDOW registering BUFFER1."
  (when (get-buffer-window buffer1)
    (unrecord-window-buffer window buffer1)
    (set-window-buffer window buffer2)))

;; Utility: select another action by key
(defun helm-select-nth-action (n)
  "Select the N nth action for the currently selected candidate."
  (let ((src (helm-get-current-source)))
    (setq helm-saved-selection (helm-get-selection nil nil src))
    (unless helm-saved-selection
      (error "Nothing is selected"))
    (setq helm-saved-action
          (helm-get-nth-action
           n
           (if (get-buffer-window helm-action-buffer 'visible)
               (assoc-default 'candidates src)
               (helm-get-actions-from-current-source src))))
    (helm-maybe-exit-minibuffer)))

(defun helm-get-nth-action (n action)
  (cond ((and (zerop n) (functionp action))
         action)
        ((listp action)
         (or (cdr (elt action n))
             (error "No such action")))
        ((and (functionp action) (< 0 n))
         (error "Sole action"))
        (t
         (error "Error in `helm-select-nth-action'"))))

(defun helm-execute-selection-action-at-nth (linum)
  "Execute default action on candidate at LINUM lines from selection."
  (let ((prefarg current-prefix-arg))
    (if (>= linum 0)
        (helm-next-line linum)
        (helm-previous-line (lognot (1- linum))))
    (setq current-prefix-arg prefarg)
    (helm-exit-minibuffer)))

;;; Persistent Action
;;
(defun helm-initialize-persistent-action ()
  (set (make-local-variable 'helm-persistent-action-display-window) nil))

(cl-defun helm-execute-persistent-action
    (&optional (attr 'persistent-action) split-onewindow)
  "Perform the associated action ATTR without quitting helm.
ATTR default is 'persistent-action', but it can be anything else.
In this case you have to add this new attribute to your source.

When `helm-full-frame' or SPLIT-ONEWINDOW are non-`nil', and
`helm-buffer' is displayed in only one window, the helm window is
split to display `helm-select-persistent-action-window' in other
window to maintain visibility."
  (interactive)
  (with-helm-alive-p
    (helm-log "executing persistent-action")
    (let* ((source (helm-get-current-source))
           (selection (and source (helm-get-selection nil nil source)))
           (attr-val (assoc-default attr source))
           ;; If attr value is a cons, use its car as persistent function
           ;; and its car to decide if helm window should be splitted.
           (fn       (if (and (consp attr-val)
                              ;; maybe a lambda.
                              (not (functionp attr-val)))
                         (car attr-val) attr-val))
           (no-split (and (consp attr-val)
                          (not (functionp attr-val))
                          (cdr attr-val)))
           (cursor-in-echo-area t)
           mode-line-in-non-selected-windows)
      (when (eq fn 'ignore)
        (cl-return-from helm-execute-persistent-action nil))
      (when source
        (with-helm-window
          (save-selected-window
            (if no-split
                (helm-select-persistent-action-window)
                (helm-select-persistent-action-window
                 (or split-onewindow helm-onewindow-p)))
            (helm-log "current-buffer = %S" (current-buffer))
            (let ((helm-in-persistent-action t)
                  (same-window-regexps '("."))
                  display-buffer-function pop-up-windows pop-up-frames
                  special-display-regexps special-display-buffer-names)
              (helm-execute-selection-action-1
               selection (or fn (helm-get-actions-from-current-source source)) t)
              (unless (helm-action-window)
                (helm-log-run-hook 'helm-after-persistent-action-hook)))
            ;; A typical case is when a persistent action delete
            ;; the buffer already displayed in
            ;; `helm-persistent-action-display-window' and `helm-full-frame'
            ;; is enabled, we end up with the `helm-buffer'
            ;; displayed in two windows.
            (when (and helm-onewindow-p
                       (> (length (window-list)) 1)
                       (equal (buffer-name
                               (window-buffer
                                helm-persistent-action-display-window))
                              (helm-buffer-get)))
              (delete-other-windows))))))))
(put 'helm-execute-persistent-action 'helm-only t)

(defun helm-persistent-action-display-window (&optional split-onewindow)
  "Return the window that will be used for persistent action.
If SPLIT-ONEWINDOW is non-`nil' window is split in persistent action."
  (with-helm-window
    (setq helm-persistent-action-display-window
          (cond ((and (window-live-p helm-persistent-action-display-window)
                      (not (member helm-persistent-action-display-window
                                   (get-buffer-window-list helm-buffer))))
                 helm-persistent-action-display-window)
                (split-onewindow (split-window))
                ((get-buffer-window helm-current-buffer))
                (t (next-window (selected-window) 1))))))

(defun helm-select-persistent-action-window (&optional split-onewindow)
  "Select the window that will be used for persistent action.
See `helm-persistent-action-display-window' for how to use SPLIT-ONEWINDOW."
  (select-window (get-buffer-window (helm-buffer-get)))
  (prog1
      (select-window
       (setq minibuffer-scroll-window
             (helm-persistent-action-display-window split-onewindow)))
    (helm-log "Selected window is %S" minibuffer-scroll-window)))

;;; Scrolling - recentering
;;
;;
(defun helm-other-window-base (command &optional arg)
  (let ((minibuffer-scroll-window
         (helm-persistent-action-display-window)))
    (funcall command (or arg helm-scroll-amount))))

(defun helm-scroll-other-window (&optional arg)
  "Scroll other window upward ARG many lines.
When arg is not provided scroll `helm-scroll-amount' lines.
See `scroll-other-window'."
  (interactive "P")
  (with-helm-alive-p (helm-other-window-base 'scroll-other-window arg)))
(put 'helm-scroll-other-window 'helm-only t)

(defun helm-scroll-other-window-down (&optional arg)
  "Scroll other window downward ARG many lines.
When arg is not provided scroll `helm-scroll-amount' lines.
See `scroll-other-window-down'."
  (interactive "P")
  (with-helm-alive-p (helm-other-window-base 'scroll-other-window-down arg)))
(put 'helm-scroll-other-window-down 'helm-only t)

(defun helm-recenter-top-bottom-other-window (&optional arg)
  "Run `recenter-top-bottom' in other window.
Meaning of prefix ARG is the same as in `recenter-top-bottom'."
  (interactive "P")
  (with-helm-alive-p
    (with-helm-window
      (with-selected-window (helm-persistent-action-display-window)
        (recenter-top-bottom arg)))))
(put 'helm-recenter-top-bottom-other-window 'helm-only t)

(defun helm-reposition-window-other-window (&optional arg)
  "Run `reposition-window' in other window.
Meaning of prefix ARG is the same as in `reposition-window'."
  (interactive "P")
  (with-helm-alive-p
    (with-helm-window
      (with-selected-window (helm-persistent-action-display-window)
        (reposition-window arg)))))
(put 'helm-reposition-window-other-window 'helm-only t)


;; Utility: Visible Mark

(defun helm-clear-visible-mark ()
  (with-current-buffer (helm-buffer-get)
    (mapc 'delete-overlay helm-visible-mark-overlays)
    (set (make-local-variable 'helm-visible-mark-overlays) nil)))

(defun helm-this-visible-mark ()
  (cl-loop for o in (overlays-at (point))
           when (overlay-get o 'visible-mark)
           return o))

(defun helm-delete-visible-mark (overlay)
  (let ((src (helm-get-current-source)))
    (setq helm-marked-candidates
          (remove
           (cons src (helm-get-selection nil nil src))
           helm-marked-candidates))
    (delete-overlay overlay)
    (setq helm-visible-mark-overlays
          (delq overlay helm-visible-mark-overlays))))

(defun helm-make-visible-mark (&optional src selection)
  (let* ((source (or src  (helm-get-current-source)))
         (sel    (or selection (helm-get-selection nil nil source)))
         (selection-end (if (helm-pos-multiline-p)
                            (or (helm-get-next-candidate-separator-pos)  ; Stays within source
                                (helm-get-next-header-pos)
                                (point-max))
                          ;; Not multiline
                          (1+ (point-at-eol))))
         (o (make-overlay (point-at-bol) selection-end)))
    (overlay-put o 'priority 0)
    (overlay-put o 'face   'helm-visible-mark)
    (overlay-put o 'source (assoc-default 'name source))
    (overlay-put o 'string (buffer-substring (overlay-start o) (overlay-end o)))
    (overlay-put o 'real sel)
    (overlay-put o 'visible-mark t)
    (cl-pushnew o helm-visible-mark-overlays)
    (push (cons source sel) helm-marked-candidates)))

(defun helm-toggle-visible-mark ()
  "Toggle helm visible mark at point."
  (interactive)
  (with-helm-alive-p
    (with-helm-window
      (let ((nomark (assq 'nomark (helm-get-current-source))))
        (if nomark
            (message "Marking not allowed in this source")
            (helm-aif (helm-this-visible-mark)
                (helm-delete-visible-mark it)
              (helm-make-visible-mark))
            (if (helm-end-of-source-p)
                (helm-display-mode-line (helm-get-current-source))
                (helm-next-line)))))))
(put 'helm-toggle-visible-mark 'helm-only t)

(defun helm-file-completion-source-p (&optional source)
  "Return non-`nil' if current source is a file completion source."
  (or helm--completing-file-name ; helm-read-file-name
      (let ((cur-source (cdr (assq 'name
                                    (or source (helm-get-current-source))))))
        (cl-loop for i in helm--file-completion-sources
                 thereis (string= cur-source i)))))

(defun helm-mark-all (&optional all)
  "Mark all visible unmarked candidates in current source.

With a prefix arg mark all visible unmarked candidates in all sources."
  (interactive "P")
  (with-helm-alive-p
    (with-helm-window ; Using `with-helm-buffer' for some unknow reasons infloop.
      (if (null all)
          (helm-mark-all-1 t)
          (let ((pos (point)))
            (goto-char (point-min))
            (helm-awhile (helm-get-next-header-pos)
              (goto-char it)
              (forward-line 1)
              (helm-mark-current-line)
              (helm-mark-all-1))
            ;; `save-excursion' seems confused if used in addition of
            ;; the one used in `helm-mark-all-1', so save POS and back
            ;; to it when loop is finished.
            (goto-char pos)
            (helm-mark-current-line)
            (helm-display-mode-line (helm-get-current-source) t))))))
(put 'helm-mark-all 'helm-only t)

(defun helm-mark-all-1 (&optional ensure-beg-of-source)
  "Mark all visible unmarked candidates in current source.
Need to be wrapped in `with-helm-window'.
Arg ENSURE-BEG-OF-SOURCE ensure we are at beginning of source when
starting to mark candidates, if handled elsewhere before starting it
is not needed."
  (let* ((src        (helm-get-current-source))
         (follow     (if (helm-follow-mode-p src) 1 -1))
         (nomark     (assq 'nomark src))
         (src-name   (assoc-default 'name src))
         (filecomp-p (or (helm-file-completion-source-p src)
                         (string= src-name "Files from Current Directory")))
         (remote-p (and filecomp-p (file-remote-p helm-pattern))))
    ;; Note that `cl-letf' prevents edebug working properly.
    (cl-letf (((symbol-function 'message) #'ignore))
      (helm-follow-mode -1)
      (unwind-protect
           (if nomark
               (message "Marking not allowed in this source")
               (save-excursion
                 (when ensure-beg-of-source
                   (goto-char (helm-get-previous-header-pos))
                   (forward-line 1))
                 (let* ((next-head (helm-get-next-header-pos))
                        (end       (and next-head
                                        (save-excursion
                                          (goto-char next-head)
                                          (forward-line -1)
                                          (point))))
                        (maxpoint  (or end (point-max))))
                   (while (< (point) maxpoint)
                     (helm-mark-current-line)
                     (let* ((prefix (get-text-property (point-at-bol) 'display))
                            (cand   (helm-get-selection nil nil src))
                            (bn     (and filecomp-p (helm-basename cand))))
                       ;; Don't mark possibles directories ending with . or ..
                       ;; autosave files/links and non--existent files.
                       (unless
                           (or (helm-this-visible-mark)
                               (string= prefix "[?]") ; doesn't match
                               (and filecomp-p
                                    (or (string-match-p ; autosave or dot files
                                         "^[.]?#.*#?$\\|[^#]*[.]\\{1,2\\}$" bn)
                                        ;; We need to test here when not using
                                        ;; a transformer that put a prefix tag
                                        ;; before candidate.
                                        ;; (i.e no [?] prefix on tramp).
                                        (and remote-p (not (file-exists-p cand))))))
                         (helm-make-visible-mark src cand)))
                     (when (helm-pos-multiline-p)
                       (goto-char
                        (or (helm-get-next-candidate-separator-pos)
                            (point-max))))
                     (forward-line 1))))
               (helm-mark-current-line))
        (helm-follow-mode follow)))))

(defun helm-unmark-all ()
  "Unmark all candidates in all sources of current helm session."
  (interactive)
  (with-helm-alive-p
    (with-helm-window
      (save-excursion
        (helm-clear-visible-mark))
      (setq helm-marked-candidates nil)
      (helm-mark-current-line)
      (helm-display-mode-line (helm-get-current-source)))))
(put 'helm-unmark-all 'helm-only t)

(defun helm-toggle-all-marks (&optional all)
  "Toggle all marks.

Mark all visible candidates of current source or unmark all candidates
visible or invisible in all sources of current helm session.

With a prefix argument mark all candidates in all sources."
  (interactive "P")
  (with-helm-alive-p
    (let ((marked (helm-marked-candidates)))
      (if (and (>= (length marked) 1)
               (with-helm-window helm-visible-mark-overlays))
          (helm-unmark-all)
          (helm-mark-all all)))))
(put 'helm-toggle-all-marks 'helm-only t)

(defun helm--compute-marked (real source &optional wildcard)
  (let* ((coerced (helm-coerce-selection real source))
         (wilds   (and wildcard
                       (condition-case nil
                           (helm-file-expand-wildcards
                            coerced t)
                         (error nil)))))
    ;; Avoid returning a not expanded wilcard fname.
    ;; e.g assuming "/tmp" doesn't contain "*.el"
    ;; return nil when coerced is "/tmp/*.el".
    (unless (or wilds (null wildcard)
                (string-match-p helm--url-regexp coerced)
                (file-exists-p coerced)
                (and (stringp coerced)
                     (null (string-match-p "[[*?]" coerced))))
      (setq coerced nil))
    (or wilds (and coerced (list coerced)))))

(cl-defun helm-marked-candidates (&key with-wildcard all-sources)
  "Return marked candidates of current source, if any.

Otherwise return one element list consisting of the current
selection. When key WITH-WILDCARD is specified, expand it.
When ALL-SOURCES key value is non-nil returns marked candidates of all
sources."
  (with-current-buffer helm-buffer
    (let ((candidates
           (cl-loop with current-src = (helm-get-current-source)
                    for (source . real) in (reverse helm-marked-candidates)
                    for use-wc = (and with-wildcard (string-match-p "\\*" real))
                    when (or all-sources
                             (equal (assq 'name source)
                                    (assq 'name current-src)))
                    append (helm--compute-marked real source use-wc)
                    into cands
                    finally return (or cands
                                       (append
                                        (helm--compute-marked
                                         (helm-get-selection nil nil current-src)
                                         current-src
                                         with-wildcard)
                                        cands)))))
      (helm-log "Marked candidates = %S" candidates)
      candidates)))

(defun helm--remove-marked-and-update-mode-line (elm)
  (with-helm-buffer
    (setq helm-marked-candidates
          (delete (rassoc elm helm-marked-candidates)
                  helm-marked-candidates))
    (helm-display-mode-line (helm-get-current-source))))

(defun helm-current-source-name= (name)
  (save-excursion
    (goto-char (helm-get-previous-header-pos))
    (equal name (helm-current-line-contents))))

(defun helm-revive-visible-mark ()
  "Restore marked candidates when helm updates display."
  (with-current-buffer helm-buffer
    (save-excursion
      (cl-dolist (o helm-visible-mark-overlays)
        (let ((o-src-str (overlay-get o 'source))
              (o-str (overlay-get o 'string))
              beg end)
          ;; Move point to end of source header line.
          (goto-char (point-min))
          (search-forward o-src-str nil t)
          (while (and (search-forward o-str nil t)
                      (cl-loop for ov in (overlays-at (point-at-bol 0))
                               never (overlay-get ov 'visible-mark))
                      (helm-current-source-name= o-src-str))
            (setq beg (match-beginning 0)
                  end (match-end 0))
            ;; Calculate real value of candidate.
            ;; It can be nil if candidate have only a display value.
            (let ((real (get-text-property (point-at-bol 0) 'helm-realvalue)))
              (if real
                  ;; Check if real value of current candidate is the same
                  ;; than the one stored in overlay.
                  ;; This is needed when some cands have same display names.
                  ;; Using equal allow testing any type of value for real cand.
                  ;; Issue (#706).
                  (and (equal (overlay-get o 'real) real)
                       (move-overlay o beg end))
                  (and (equal o-str (buffer-substring beg end))
                       (move-overlay o beg end))))))))))
(add-hook 'helm-after-update-hook 'helm-revive-visible-mark)

(defun helm-next-point-in-list (curpos points &optional prev)
  (cond
    ;; rule out special cases.
    ((null points) curpos)
    ((and prev (<= curpos (car points)))
     (nth (1- (length points)) points))
    ((< (car (last points)) curpos)
     (if prev (car (last points)) (nth 0 points)))
    ((and (not prev) (>= curpos (car (last points))))
     (nth 0 points))
    (t
     (nth (if prev
              (cl-loop for pt in points
                    for i from 0
                    if (<= curpos pt) return (1- i))
            (cl-loop for pt in points
                  for i from 0
                  if (< curpos pt) return i))
          points))))

(defun helm-next-visible-mark (&optional prev)
  "Move next helm visible mark.
If PREV is non-`nil' move to precedent."
  (interactive)
  (with-helm-alive-p
    (with-helm-window
      (ignore-errors
        (goto-char (helm-next-point-in-list
                    (point)
                    (sort (mapcar 'overlay-start helm-visible-mark-overlays) '<)
                    prev)))
      (helm-mark-current-line))))
(put 'helm-next-visible-mark 'helm-only t)

(defun helm-prev-visible-mark ()
  "Move previous helm visible mark."
  (interactive)
  (with-helm-alive-p
    (helm-next-visible-mark t)))
(put 'helm-prev-visible-mark 'helm-only t)

;;; Utility: Selection Paste
;;
(defun helm-yank-selection (arg)
  "Set minibuffer contents to current display selection.
With a prefix arg set to real value of current selection."
  (interactive "P")
  (with-helm-alive-p
    (let ((str (format "%s" (helm-get-selection nil (not arg)))))
      (kill-new str)
      (helm-set-pattern str))))
(put 'helm-yank-selection 'helm-only t)

(defun helm-kill-selection-and-quit (arg)
  "Store display value of current selection to kill ring.
With a prefix arg use real value of current selection.
Display value is shown in `helm-buffer' and real value
is used to perform actions."
  (interactive "P")
  (with-helm-alive-p
    (helm-run-after-exit
     (lambda (sel)
       (kill-new sel)
       ;; Return nil to force `helm-mode--keyboard-quit'
       ;; in `helm-comp-read' otherwise the value "Saved to kill-ring: foo"
       ;; is used as exit value for `helm-comp-read'.
       (prog1 nil (message "Saved to kill-ring: %s" sel) (sit-for 1)))
     (format "%s" (helm-get-selection nil (not arg))))))
(put 'helm-kill-selection-and-quit 'helm-only t)

(defun helm-copy-to-buffer ()
  "Copy selection or marked candidates to `helm-current-buffer'.
Note that the real values of candidates are copied and not the
display values."
  (interactive)
  (with-helm-alive-p
    (helm-run-after-exit
     (lambda (cands)
       (with-helm-current-buffer
         (insert (mapconcat (lambda (c)
                              (format "%s" c))
                            cands "\n"))))
     (helm-marked-candidates))))
(put 'helm-copy-to-buffer 'helm-only t)


;;; Follow-mode: Automatic execution of persistent-action
;;
;;
(defvar helm-follow-input-idle-delay nil
  "`helm-follow-mode' will execute its persistent action after this delay.
Note that if the `follow-delay' attr is present in source,
it will take precedence over this.")

(defun helm-follow-mode (&optional arg)
  "Execute persistent action every time the cursor is moved.

This mode is source local, i.e It apply on current source only.
\\<helm-map>
This mode can be enabled or disabled interactively at anytime during
a helm session with \\[helm-follow-mode].

When enabling interactively `helm-follow-mode' in a source, you can keep it enabled
for next emacs sessions by setting `helm-follow-mode-persistent' to a non-nil value.

When `helm-follow-mode' is called with a prefix arg and `helm-follow-mode-persistent'
is non-nil `helm-follow-mode' will be persistent only for this emacs session,
but not for next emacs sessions, i.e the current source will not be saved
to `helm-source-names-using-follow'.
A prefix arg with `helm-follow-mode' already enabled will have no effect.

Note that you can use instead of this mode the commands `helm-follow-action-forward'
and `helm-follow-action-backward' at anytime in all helm sessions.

They are bound by default to \\[helm-follow-action-forward] and \\[helm-follow-action-backward]."
  (interactive (list (helm-aif (and current-prefix-arg
                                    (prefix-numeric-value current-prefix-arg))
                         (unless (helm-follow-mode-p) it))))
  (with-helm-alive-p
    (with-current-buffer helm-buffer
      (let* ((src      (helm-get-current-source))
             (name     (assoc-default 'name src))
             (sym      (cl-loop for s in helm-sources
                                for sname = (and (symbolp s)
                                                 (assoc-default
                                                  'name (symbol-value s)))
                                thereis (and sname (string= sname name) s)))
             (fol-attr (assq 'follow src))
             (enabled  (or (helm-follow-mode-p src)
                           (and helm-follow-mode-persistent
                                (member (assoc-default 'name src)
                                        helm-source-names-using-follow)))))
        (if src
            (progn
              (if (eq (cdr fol-attr) 'never)
                  (message "helm-follow-mode not allowed in this source")
                  ;; Make follow attr persistent for this emacs session.
                  (helm-follow-mode-set-source
                   (if (or enabled (and (numberp arg) (< arg 0))) -1 1)
                   src)
                  ;; When arg is nil assume the call is interactive.
                  ;; However if user call helm-follow-mode with a prefix arg,
                  ;; the call will be considered non--interactive and
                  ;; src-name will NOT be saved to helm-source-names-using-follow.
                  ;; When called from lisp (non--interactive) src-name
                  ;; will never be saved.
                  (when (and helm-follow-mode-persistent (null arg))
                    (if (null enabled)
                        (unless (member name helm-source-names-using-follow)
                          (push name helm-source-names-using-follow)
                          (customize-save-variable 'helm-source-names-using-follow
                                                   helm-source-names-using-follow))
                        (when (member name helm-source-names-using-follow)
                          (setq helm-source-names-using-follow
                                (delete name helm-source-names-using-follow))
                          (customize-save-variable 'helm-source-names-using-follow
                                                   helm-source-names-using-follow))))
                  (message "helm-follow-mode is %s"
                           (if (helm-follow-mode-p src)
                               "enabled" "disabled"))
                  (helm-display-mode-line src t))
              (unless helm-follow-mode-persistent
                (and sym (set sym (remove (assq 'follow src) src)))))
            (message "Not enough candidates for helm-follow-mode"))))))
(put 'helm-follow-mode 'helm-only t)

(defun helm-follow-execute-persistent-action-maybe (&optional delay)
  "Execute persistent action in mode `helm-follow-mode'.

This happen after: DELAY or the 'follow-attr value of current source
or `helm-follow-input-idle-delay' or `helm-input-idle-delay' secs."
  (let* ((src (helm-get-current-source))
         (at (or delay
                 (assoc-default 'follow-delay src)
                 helm-follow-input-idle-delay
                 (or (and helm-input-idle-delay
                          (max helm-input-idle-delay 0.01))
                     0.01))))
    (when (and (not (get-buffer-window helm-action-buffer 'visible))
               (not (helm-pos-header-line-p))
               (or (helm-follow-mode-p src)
                   (and helm-follow-mode-persistent
                        (member (assoc-default 'name src)
                                helm-source-names-using-follow)))
               (null (eq (assoc-default 'follow src) 'never))
               (helm-window)
               (helm-get-selection nil nil src))
      (helm-follow-mode-set-source 1 src)
      (run-with-idle-timer at nil (lambda ()
                                    (when helm-alive-p
                                      (helm-execute-persistent-action)))))))

(defun helm-follow-mode-p (&optional source)
  (with-helm-buffer
    (eq (helm-attr 'follow (or source (helm-get-current-source))) 1)))

(defun helm-follow-mode-set-source (value &optional source)
  (with-helm-buffer
    (helm-attrset 'follow value (or source (helm-get-current-source)))))

;;; Auto-resize mode
;;
(defun helm--autoresize-hook (&optional max-height min-height)
  (with-helm-window
    (fit-window-to-buffer nil
                          (/ (* (frame-height)
                                (or max-height helm-autoresize-max-height))
                             100)
                          (/ (* (frame-height)
                                (or min-height helm-autoresize-min-height))
                             100))))

(define-minor-mode helm-autoresize-mode
    "Auto resize helm window when enabled.
Helm window is re-sized according to `helm-autoresize-max-height'
and `helm-autoresize-min-height'. Note that when this mode is
enabled, helm behaves as if `helm-always-two-windows' is
enabled.

See `fit-window-to-buffer' for more infos."
  :group 'helm
  :global t
  (if helm-autoresize-mode
      (progn (add-hook 'helm-after-update-hook 'helm--autoresize-hook)
             (add-hook 'helm-window-configuration-hook 'helm--autoresize-hook))
      (remove-hook 'helm-after-update-hook 'helm--autoresize-hook)
      (remove-hook 'helm-window-configuration-hook 'helm--autoresize-hook)))

(defun helm-help ()
  "Generate helm's help according to `help-message' attribute.

If source is not available yet or doesn't have any `help-message'
attribute, a generic message explaining this is added instead.  The
global `helm-help-message' is always added after this local help."
  (interactive)
  (with-helm-alive-p
    (save-selected-window
      (helm-help-internal
       "*Helm Help*"
       (lambda ()
         (helm-aif (assoc-default 'help-message (helm-get-current-source))
             (insert (substitute-command-keys
                      (helm-interpret-value it)))
           (insert "* No specific help for this source at this time.\n
It may appear after first results popup in helm buffer."))
         (insert "\n\n"
                 (substitute-command-keys
                  (helm-interpret-value helm-help-message))))))))
(put 'helm-help 'helm-only t)

(defun helm-toggle-truncate-line ()
  "Toggle `truncate-lines' value in `helm-buffer'"
  (interactive)
  (with-helm-alive-p
    (with-helm-buffer
      (setq truncate-lines (not truncate-lines))
      (when (helm-get-previous-header-pos)
        (helm-update (regexp-quote (helm-get-selection nil t)))))))
(put 'helm-toggle-truncate-line 'helm-only t)

(provide 'helm)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm.el ends here
