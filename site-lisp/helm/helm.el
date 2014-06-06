;;; helm.el --- Emacs incremental and narrowing framework -*- lexical-binding: t -*-

;; Copyright (C) 2007         Tamas Patrovics
;;               2008 ~ 2011  rubikitch <rubikitch@ruby-lang.org>
;;               2011 ~ 2014  Thierry Volpiatto <thierry.volpiatto@gmail.com>

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


;;; Multi keys
;;
;;
;;;###autoload
(defun helm-define-multi-key (keymap key functions &optional delay)
  "In KEYMAP, define key sequence KEY for function list FUNCTIONS.
Each function run sequentialy each time the key KEY is pressed.
If DELAY is specified switch back to initial function of FUNCTIONS list
after DELAY seconds.
The functions in FUNCTIONS list are functions with no args.
e.g
  \(defun foo ()
    (message \"Run foo\"))
  \(defun bar ()
    (message \"Run bar\"))
  \(defun baz ()
    (message \"Run baz\"))

\(helm-define-multi-key global-map \"<f5> q\" '(foo bar baz) 2)

Each time \"<f5> q\" is pressed the next function is executed, if you wait
More than 2 seconds, next hit will run again the first function and so on."
  (define-key keymap key (helm-make-multi-command functions delay)))

;;;###autoload
(defmacro helm-multi-key-defun (name docstring funs &optional delay)
  "Define NAME as a multi-key command running FUNS.
After DELAY seconds the FUNS list is reinitialised.
See `helm-define-multi-key'."
  (declare (indent 2))
  (setq docstring (if docstring (concat docstring "\n\n")
                    "This is a helmish multi-key command."))
  `(defalias (quote ,name) (helm-make-multi-command ,funs ,delay) ,docstring))

(defun helm-make-multi-command (functions &optional delay)
  "Return an anonymous multi-key command running FUNCTIONS.
Run each function of FUNCTIONS list in turn when called within DELAY seconds."
  (declare (indent 1))
  (let ((funs functions)
        (iter (cl-gensym "helm-iter-key"))
        (timeout delay))
    (eval (list 'defvar iter nil))
    #'(lambda () (interactive) (helm-run-multi-key-command funs iter timeout))))

(defun helm-run-multi-key-command (functions iterator delay)
  (let ((fn #'(lambda ()
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
    (and next (symbol-value iterator) (call-interactively (nth (1- next) functions)))
    (when delay (run-with-idle-timer delay nil `(lambda ()
                                                  (setq ,iterator nil))))))

(defun helm-iter-list (seq)
  "Return an iterator object from SEQ."
  (let ((lis seq))
    (lambda ()
      (let ((elm (car lis)))
        (setq lis (cdr lis))
        elm))))

(defun helm-iter-next (iterator)
  "Return next elm of ITERATOR."
  (funcall iterator))

(helm-multi-key-defun helm-toggle-resplit-and-swap-windows
    "Multi key command to resplit and swap helm window.
First call run `helm-toggle-resplit-window',
second call within 0.5s run `helm-swap-windows'."
  '(helm-toggle-resplit-window helm-swap-windows) 1)

;;;###autoload
(defmacro helm-define-key-with-subkeys (map key subkey command
                                        &optional other-subkeys menu exit-fn)
  "Allow defining a KEY without having to type its prefix again on next calls.
Arg MAP is the keymap to use, SUBKEY is the initial long keybinding to
call COMMAND.
Arg OTHER-SUBKEYS is an unquoted alist specifying other short keybindings
to use once started.
e.g:

\(helm-define-key-with-subkeys global-map
      \(kbd \"C-x v n\") ?n 'git-gutter:next-hunk ((?p 'git-gutter:previous-hunk))\)


In this example, `C-x v n' will run `git-gutter:next-hunk' subsequent hit on \"n\"
will run this command again and subsequent hit on \"p\" will run `git-gutter:previous-hunk'.

Arg MENU is a string to display in minibuffer to describe SUBKEY and OTHER-SUBKEYS.
Arg EXIT-FN specify a function to run on exit.

Any other keys pressed run their assigned command defined in MAP and exit the loop."

  (let ((other-keys (and other-subkeys
                         (cl-loop for (x . y) in other-subkeys
                               collect (list x (list 'call-interactively y) t)))))
    `(define-key ,map ,key
       #'(lambda ()
           (interactive)
           (unwind-protect
                (progn
                  (call-interactively ,command)
                  (while (let ((input (read-key ,menu)) kb com)
                           (cl-case input
                             (,subkey (call-interactively ,command) t)
                             ,@other-keys
                             (t (setq kb  (this-command-keys-vector))
                                (setq com (lookup-key ,map kb))
                                (if (commandp com)
                                    (call-interactively com)
                                  (setq unread-command-events
                                        (nconc (mapcar 'identity
                                                       (this-single-command-raw-keys))
                                               unread-command-events)))
                                nil)))))
             (and ,exit-fn (funcall ,exit-fn)))))))


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
    (define-key map (kbd "<RET>")      'helm-exit-minibuffer)
    (define-key map (kbd "C-i")        'helm-select-action)
    (define-key map (kbd "C-z")        'helm-execute-persistent-action)
    (define-key map (kbd "C-e")        'helm-select-2nd-action-or-end-of-line)
    (define-key map (kbd "C-j")        'helm-select-3rd-action)
    (define-key map (kbd "C-o")        'helm-next-source)
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
    (define-key map (kbd "M-u")        'helm-unmark-all)
    (define-key map (kbd "C-w")        'helm-yank-text-at-point)
    (define-key map (kbd "C-M-a")      'helm-show-all-in-this-source-only)
    (define-key map (kbd "C-M-e")      'helm-display-all-sources)
    (define-key map (kbd "C-r")        'undefined)
    (define-key map (kbd "C-s")        'undefined)
    (define-key map (kbd "M-s")        'undefined)
    (define-key map (kbd "C-}")        'helm-narrow-window)
    (define-key map (kbd "C-{")        'helm-enlarge-window)
    (define-key map (kbd "C-c -")      'helm-swap-windows)
    (define-key map (kbd "C-c C-d")    'helm-delete-current-selection)
    (define-key map (kbd "C-c C-y")    'helm-yank-selection)
    (define-key map (kbd "C-c C-k")    'helm-kill-selection-and-quit)
    (define-key map (kbd "C-c C-f")    'helm-follow-mode)
    (define-key map (kbd "C-c C-u")    'helm-force-update)
    (define-key map (kbd "M-p")        'previous-history-element)
    (define-key map (kbd "M-n")        'next-history-element)
    (define-key map (kbd "C-!")        'helm-toggle-suspend-update)
    (define-key map (kbd "C-x b")      'helm-resume-previous-session-after-quit)
    (define-key map (kbd "C-x C-b")    'helm-resume-list-buffers-after-quit)
    ;; Disable usage of the mouse while in helm.
    (define-key map (kbd "<down-mouse-1>")   'ignore)
    (define-key map (kbd "<drag-mouse-1>")   'ignore)
    (define-key map (kbd "<mouse-1>")        'ignore)
    (define-key map (kbd "<double-mouse-1>") 'ignore)
    (define-key map (kbd "<triple-mouse-1>") 'ignore)
    (define-key map (kbd "<down-mouse-2>")   'ignore)
    (define-key map (kbd "<drag-mouse-2>")   'ignore)
    (define-key map (kbd "<mouse-2>")        'ignore)
    (define-key map (kbd "<double-mouse-2>") 'ignore)
    (define-key map (kbd "<triple-mouse-2>") 'ignore)
    (define-key map (kbd "<down-mouse-3>")   'ignore)
    (define-key map (kbd "<drag-mouse-3>")   'ignore)
    (define-key map (kbd "<mouse-3>")        'ignore)
    (define-key map (kbd "<double-mouse-3>") 'ignore)
    (define-key map (kbd "<triple-mouse-3>") 'ignore)
    ;; Disable `file-cache-minibuffer-complete'.
    (define-key map (kbd "<C-tab>")    'undefined)
    ;; Multi keys
    (define-key map (kbd "C-t")        'helm-toggle-resplit-and-swap-windows)
    ;; Debugging command
    (define-key map (kbd "C-h C-d")    'undefined)
    (define-key map (kbd "C-h C-d")    'helm-debug-output)
    ;; Use `describe-mode' key in `global-map'.
    (define-key map [f1] nil) ; Allow to eval keymap without errors.
    (define-key map (kbd "C-h C-h")    'undefined)
    (define-key map (kbd "C-h h")      'undefined)
    (cl-dolist (k (where-is-internal 'describe-mode global-map))
      (define-key map k 'helm-help))
    map)
  "Keymap for helm.")


(defgroup helm nil
  "Open helm."
  :prefix "helm-" :group 'convenience)

(defcustom helm-completion-window-scroll-margin 5
  " `scroll-margin' to use for helm completion window.
Which see.  Set to 0 to disable.
NOTE: This have no effect when `helm-display-source-at-screen-top'
id non--nil."
  :group 'helm
  :type  'integer)

(defcustom helm-display-source-at-screen-top t
  "Display candidates at the top of screen.
This happen when using `helm-next-source' and `helm-previous-source'.
NOTE: When non--nil (default) disable `helm-completion-window-scroll-margin'."
  :group 'helm
  :type 'boolean)

(defcustom helm-candidate-number-limit 100
  "Limit candidate number globally.
Do not show more candidates than this limit from individual sources.
It is usually pointless to show hundreds of matches
when the pattern is empty, because it is much simpler to type a
few characters to narrow down the list of potential candidates.

Set it to nil if you don't want this limit."
  :group 'helm
  :type '(choice (const :tag "Disabled" nil) integer))

(defcustom helm-idle-delay 0.01
  "Be idle for this many seconds, before updating in delayed sources.
This is useful for sources involving heavy operations
\(like launching external programs\), so that candidates
from the source are not retrieved unnecessarily if the user keeps typing.

It also can be used to declutter the results helm displays,
so that results from certain sources are not shown with every
character typed, only if the user hesitates a bit.
Be sure to know what you are doing when modifying this."
  :group 'helm
  :type 'float)

(defcustom helm-input-idle-delay 0.01
  "Be idle for this many seconds, before updating.

Unlike `helm-idle-delay', it is also effective for non-delayed sources.
If nil, candidates are collected immediately.

Note:  If this value is too low compared to `helm-idle-delay',
you may have duplicated sources when using multiples sources.
Safe value is always >= `helm-idle-delay'.
Default settings are equal value for both.
Be sure to know what you are doing when modifying this."
  :group 'helm
  :type 'float)

(defcustom helm-full-frame nil
  "Use current window to show the candidates.
If t then Helm doesn't pop up a new window."
  :group 'helm
  :type 'boolean)

(defvaralias 'helm-samewindow 'helm-full-frame)
(make-obsolete-variable 'helm-samewindow 'helm-full-frame "1.4.8.1")

(defcustom helm-quick-update nil
  "If non-nil, suppress displaying sources which are out of screen at first.
They are treated as delayed sources at this input.
This flag makes `helm' a bit faster with many sources."
  :group 'helm
  :type 'boolean)

(defcustom helm-candidate-separator
  "--------------------"
  "Candidates separator of `multiline' source."
  :group 'helm
  :type 'string)

(defcustom helm-save-configuration-functions
  '(set-window-configuration . current-window-configuration)
  "The functions used to restore/save window or frame configurations.
It is a pair where the car is the function to restore window or frame config,
and the cdr is the function to save the window or frame config.

If you want to save and restore frame configuration, set this variable to
 '\(set-frame-configuration . current-frame-configuration\)
NOTE: This may not work properly with own-frame minibuffer settings.
Older version saves/restores frame configuration, but the default is changed now
because flickering can occur in some environment."
  :group 'helm
  :type 'sexp)

(defcustom helm-persistent-action-use-special-display nil
  "If non-nil, use `special-display-function' in persistent action."
  :group 'helm
  :type 'boolean)

(defcustom helm-scroll-amount nil
  "Scroll amount when scrolling other window in a helm session.
It is used by `helm-scroll-other-window'
and `helm-scroll-other-window-down'.

If you prefer scrolling line by line, set this value to 1."
  :group 'helm
  :type 'integer)

(defcustom helm-display-function 'helm-default-display-buffer
  "Function to display *helm* buffer.
It is `helm-default-display-buffer' by default,
which affects `helm-full-frame'."
  :group 'helm
  :type 'symbol)

(defcustom helm-case-fold-search 'smart
  "Add 'smart' option to `case-fold-search'.
When smart is enabled, ignore case in the search strings
if pattern contains no uppercase characters.
Otherwise, with a nil or t value, the behavior is same as
`case-fold-search'.
Default value is smart, other possible values are nil and t.
NOTE: This have no effect in asynchronous sources, you will
have to implement a similar feature directly in the process.
See in helm-grep.el how it is implemented."
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
  "Reuse the last state of window split, vertical or horizontal.
That is when you use `helm-toggle-resplit-window' the next helm session
will reuse the same window scheme than the one of last session unless
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
that is 'below, 'above, 'left or 'right.

Other acceptable values are 'same which always display `helm-buffer'
in current window and 'other that display `helm-buffer' below if only one
window or in `other-window-for-scrolling' if available.

A nil value as same effect as 'below.
If `helm-full-frame' is non--nil, it take precedence on this.

See also `helm-split-window-in-side-p' and `helm-always-two-windows' that
take precedence on this.

NOTE: this have no effect if `helm-split-window-preferred-function' is not
`helm-split-window-default-fn' unless this new function handle this."
  :group 'helm
  :type 'symbol)

(defcustom helm-split-window-in-side-p nil
  "Force splitting inside selected window when non--nil.
See also `helm-split-window-default-side'.

NOTE: this have no effect if `helm-split-window-preferred-function' is not
`helm-split-window-default-fn' unless this new function handle this."
  :group 'helm
  :type 'boolean)

(defcustom helm-always-two-windows nil
  "When non--nil helm will use two windows in this frame.
That is one window to display `helm-buffer' and one to display
`helm-current-buffer'.
Note: this have no effect when `helm-split-window-in-side-p' is non--nil,
or when `helm-split-window-default-side' is set to 'same."
  :group 'helm
  :type 'boolean)

(defcustom helm-sources-using-default-as-input '(helm-source-imenu
                                                 helm-source-semantic
                                                 helm-source-info-elisp
                                                 helm-source-etags-select)
  "List of helm sources that need to use `helm-maybe-use-default-as-input'.
When a source is member of this list, default `thing-at-point'
will be used as input."
  :group 'helm
  :type '(repeat (choice symbol)))

(defcustom helm-delete-minibuffer-contents-from-point nil
  "When non--nil, `helm-delete-minibuffer-contents' delete region from `point'.
Otherwise (default) delete `minibuffer-contents'."
  :group 'helm
  :type 'boolean)

(defcustom helm-follow-mode-persistent nil
  "Retrieve last state of `helm-follow-mode' in next helm session when non--nil.
This will not make it persistent through emacs sessions though,
you will have to set explicitely the `follow' attribute in the source where
you want this mode enabled definitely."
  :group 'helm
  :type 'boolean)

(defcustom helm-prevent-escaping-from-minibuffer t
  "Prevent escaping from minibuffer during helm session."
  :group 'helm
  :type 'boolean)

(defcustom helm-truncate-lines nil
  "Truncate long lines when non--nil.
See `truncate-lines'."
  :group 'helm
  :type 'boolean)

(defcustom helm-move-to-line-cycle-in-source nil
  "Move to end or beginning of source when reaching top or bottom of source.
This happen when using `helm-next/previous-line'."
  :group 'helm
  :type 'boolean)


;;; Faces
;;
;;
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
  :group 'helm)

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
  :group 'helm)

(defface helm-header
    '((t (:inherit header-line)))
  "Face for header lines in the helm buffer."
  :group 'helm)

(defface helm-candidate-number
    '((((background dark)) :background "Yellow" :foreground "black")
      (((background light)) :background "#faffb5" :foreground "black"))
  "Face for candidate number in mode-line." :group 'helm)

(defface helm-selection
    '((((background dark)) :background "ForestGreen" :underline t)
      (((background light)) :background "#b5ffd1" :underline t))
  "Face for currently selected item in the helm buffer."
  :group 'helm)

(defface helm-separator
    '((((background dark)) :foreground "red")
      (((background light)) :foreground "#ffbfb5"))
  "Face for multiline source separator."
  :group 'helm)

(defface helm-action
    '((t (:underline t)))
  "Face for action lines in the helm action buffer."
  :group 'helm)

(defface helm-prefarg
    '((((background dark)) :foreground "green")
      (((background light)) :foreground "red"))
  "Face for showing prefix arg in mode-line."
  :group 'helm)


;;; Variables.
;;
;;
(defvar helm-type-attributes nil
  "It's a list of \(TYPE ATTRIBUTES ...\).
ATTRIBUTES are the same as attributes for `helm-sources'.
TYPE connects the value to the appropriate sources.
Don't set this directly, use instead `define-helm-type-attribute'.

This allows specifying common attributes for several sources.
For example, sources which provide files can specify
common attributes with a `file' type.")

(defvar helm-source-filter nil
  "A list of source names to be displayed.
Other sources won't appear in the search results.
If nil then there is no filtering.
See also `helm-set-source-filter'.")

(defvar helm-action-buffer "*helm action*"
  "Buffer showing actions.")

(defvar helm-selection-overlay nil
  "Overlay used to highlight the currently selected item.")

(defvar helm-async-processes nil
  "List of information about asynchronous processes managed by helm.")

(defvar helm-before-initialize-hook nil
  "Run before helm initialization.
This hook is run before init functions in `helm-sources'.")

(defvar helm-after-initialize-hook nil
  "Run after helm initialization.
Global variables are initialized and the helm buffer is created.
But the helm buffer has no contents.")

(defvar helm-update-hook nil
  "Run after the helm buffer was updated according the new input pattern.
This hook is run at the beginning of buffer.
The first candidate is selected after running this hook.
See also `helm-after-update-hook'.")

(defvar helm-after-update-hook nil
  "Run after the helm buffer was updated according the new input pattern.
This is very similar to `helm-update-hook' but selection is not moved.
It is useful to select a particular object instead of the first one.")

(defvar helm-cleanup-hook nil
  "Run after helm minibuffer is closed.
IOW this hook is executed BEFORE performing action.")

(defvar helm-select-action-hook nil
  "Run when opening the action buffer.")

(defvar helm-before-action-hook nil
  "Run before executing action.
Contrarily to `helm-cleanup-hook',
this hook run before helm minibuffer is closed
and before performing action.")

(defvar helm-after-action-hook nil
  "Run after executing action.")

(defvar helm-exit-minibuffer-hook nil
  "Run just before exiting minibuffer.")

(defvar helm-after-persistent-action-hook nil
  "Run after executing persistent action.")

(defvar helm-move-selection-before-hook nil
  "Run before moving selection in `helm-buffer'.")

(defvar helm-move-selection-after-hook nil
  "Run after moving selection in `helm-buffer'.")

(defvar helm-restored-variables
  '(helm-candidate-number-limit
    helm-source-filter
    helm-source-in-each-line-flag
    helm-map
    helm-sources)
  "Variables which are restored after `helm' invocation.")

(defvar helm-execute-action-at-once-if-one nil
  "Execute default action and exit when only one candidate is remaining.")

(defvar helm-quit-if-no-candidate nil
  "Quit when there is no candidates when non--nil.
This variable accepts a function, which is executed if no candidate.")

(defvar helm-maybe-use-default-as-input nil
  "Use :default arg of `helm' as input to update display.
Note that if also :input is specified as `helm' arg, it will take
precedence on :default.")

(defvar helm-source-in-each-line-flag nil
  "Non-nil means add helm-source text-property in each candidate.
experimental feature.")

(defvar helm-debug-variables nil
  "A list of helm variables to show in `helm-debug-output'.
Otherwise all variables started with `helm-' are shown.")

(defvar helm-debug-buffer "*Debug Helm Log*")

(defvar helm-debug nil
  "If non-nil, write log message into `helm-debug-buffer' buffer.
It is disabled by default because `helm-debug-buffer' grows quickly.")

(defvar helm-compile-source-functions
  '(helm-compile-source--type
    helm-compile-source--dummy
    helm-compile-source--candidates-in-buffer)
  "Functions to compile elements of `helm-sources' (plug-in).")


;;; Internal Variables
;;
;;
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
  "[INTERNAL] Value of current sources in used, a list.")
(defvar helm-delayed-init-executed nil)
(defvar helm-buffer "*helm*"
  "Buffer showing completions.")
(defvar helm-current-buffer nil
  "Current buffer when `helm' is invoked.")
(defvar helm-buffer-file-name nil
  "Variable `buffer-file-name' when `helm' is invoked.")
(defvar helm-default-directory nil
  "The value of `default-directory' when `helm' is initialized.")
(defvar helm-candidate-cache (make-hash-table :test 'equal)
  "Holds the available candidate within a single helm invocation.")
(defvar helm-pattern ""
  "The input pattern used to update the helm buffer.")
(defvar helm-input ""
  "The input typed in the candidates panel.")
(defvar helm-input-local nil
  "Internal, store locally `helm-pattern' value for later use in `helm-resume'.")
(defvar helm-source-name nil)
(defvar helm-current-source nil)
(defvar helm-candidate-buffer-alist nil)
(defvar helm-match-hash (make-hash-table :test 'equal))
(defvar helm-cib-hash (make-hash-table :test 'equal))
(defvar helm-tick-hash (make-hash-table :test 'equal))
(defvar helm-issued-errors nil)
(defvar helm-debug-root-directory nil
  "When non--nil, save helm log to `helm-last-log-file'.
Be aware that if you set that, you will end up with a huge directory
of log files, so use that only for debugging purpose.
See `helm-log-save-maybe' for more info.")
(defvar helm-last-log-file nil
  "The name of the last helm session log file.")
(defvar helm-follow-mode nil)
(defvar helm-let-variables nil)
(defvar helm-split-window-state nil)
(defvar helm--window-side-state nil)
(defvar helm-selection-point nil)
(defvar helm-alive-p nil)
(defvar helm-visible-mark-overlays nil)
(defvar helm-update-blacklist-regexps '("^" "^ *" "$" "!" " " "\\b"
                                        "\\<" "\\>" "\\_<" "\\_>" ".*"))
(defvar helm-suspend-update-flag nil)
(defvar helm-force-updating-p nil)
(defvar helm-exit-status 0
  "Flag to inform whether helm have exited or quitted.
Exit with 0 mean helm have exited executing an action.
Exit with 1 mean helm have quitted with \\[keyboard-quit]
It is useful for example to restore a window config if helm abort
in special cases.
See `helm-exit-minibuffer' and `helm-keyboard-quit'.")
(defvar helm-minibuffer-confirm-state nil)
(defvar helm-quit nil)
(defvar helm-attributes nil "List of all `helm' attributes.")
(defvar helm-buffers nil
  "All of `helm-buffer' in most recently used order.")
(defvar helm-current-position nil
  "Cons of \(point . window-start\)  when `helm' is invoked.
It is needed to restore position in `helm-current-buffer'
when `helm' is keyboard-quitted.")
(defvar helm-last-frame-or-window-configuration nil
  "Used to store window or frame configuration when helm start.")
(defvar helm-onewindow-p nil)
(defvar helm-types nil)
(defvar helm-mode-line-string-real nil) ; The string to display in mode-line.
(defvar helm-persistent-action-display-window nil)
(defvar helm-marked-candidates nil
  "Marked candadates.  List of \(source . real\) pair.")
(defvar helm-in-file-completion-p nil)
(defvar helm--mode-line-display-prefarg nil)
(defvar helm--temp-follow-flag nil
  "[INTERNAL] A simple flag to notify persistent action we are following.")


;; Utility: logging
(defun helm-log (format-string &rest args)
  "Log message `helm-debug' is non-nil.
Messages are written to the `helm-debug-buffer' buffer.

Argument FORMAT-STRING is a string to use with `format'.
Use optional arguments ARGS like in `format'."
  (when helm-debug
    (with-current-buffer (get-buffer-create helm-debug-buffer)
      (outline-mode)
      (buffer-disable-undo)
      (set (make-local-variable 'inhibit-read-only) t)
      (goto-char (point-max))
      (insert (let ((tm (current-time)))
                (format (concat (if (string-match "Start session" format-string)
                                    "* " "** ")
                                "%s.%06d (%s)\n %s\n")
                        (format-time-string "%H:%M:%S" tm)
                        (nth 2 tm)
                        (helm-log-get-current-function)
                        (apply #'format (cons format-string args))))))))

(defmacro helm-log-eval (&rest exprs)
  "Eval EXPRS and write results to helm log buffer."
  (cl-dolist (expr exprs)
    `(condition-case err
         ;; Don't eval expression EXPR
         ;; when debugging is not turned on.
         (when helm-debug
           (helm-log "%S = %S" ,expr (eval ,expr t)))
       (error (helm-log "%S = ERROR: %S" ,expr err)))))

(defun helm-log-run-hook (hook)
  "Run HOOK like `run-hooks' but write these actions to helm log buffer."
  (helm-log "executing %s" hook)
  (helm-log-eval (symbol-value hook))
  (helm-log-eval (default-value hook))
  (run-hooks hook)
  (helm-log "executed %s" hook))

(defun helm-log-get-current-function ()
  "Get function name calling `helm-log'.
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
      (add-to-list 'helm-issued-errors msg))))

(defun helm-log-save-maybe ()
  "May be save log buffer to `helm-last-log-file'.
If `helm-debug-root-directory' is non--nil and a valid directory,
a directory named 'helm-debug-<date of today>'
will be created there and the log recorded in a file named
at the date and time of today in this directory."
  (when (and (stringp helm-debug-root-directory)
             (file-directory-p helm-debug-root-directory)
             helm-debug)
    (let ((logdir (expand-file-name (concat "helm-debug-"
                                            (format-time-string "%Y%m%d"))
                                    helm-debug-root-directory)))
      (make-directory logdir t)
      (with-current-buffer (get-buffer-create helm-debug-buffer)
        (write-region (point-min) (point-max)
                      (setq helm-last-log-file
                            (expand-file-name
                             (format-time-string "%Y%m%d-%H%M%S")
                             logdir))
                      nil 'silent)
        (kill-buffer)))))

;;;###autoload
(defun helm-debug-open-last-log ()
  "Open helm log file of last helm session.
If `helm-last-log-file' is nil, switch to `helm-debug-buffer' ."
  (interactive)
  (if helm-last-log-file
      (view-file helm-last-log-file)
    (switch-to-buffer helm-debug-buffer)
    (view-mode 1) (visual-line-mode 1)))

(defun helm-print-error-messages ()
  "Print error messages in `helm-issued-errors'."
  (and helm-issued-errors
       (message "Helm issued errors: %s"
                (mapconcat 'identity (reverse helm-issued-errors) "\n"))))

;; These advices are needed to fix cursor position in minibuffer
;; after insertion (otherwise cursor stay at beginning of insertion)
;; Activate deactivate them by hook because they may not work outside
;; of helm (Issue #338).
(defadvice next-history-element (around delay)
  (interactive "p")
  (or (zerop n)
      (run-with-timer
       0.01 nil `(lambda ()
                   (goto-history-element (- minibuffer-history-position ,n))))))

(defadvice previous-history-element (around delay)
  (interactive "p")
  (or (zerop n)
      (run-with-timer
       0.01 nil `(lambda ()
                   (goto-history-element (+ minibuffer-history-position ,n))))))

(add-hook 'helm-before-initialize-hook (lambda ()
                                         (ad-activate 'next-history-element)
                                         (ad-activate 'previous-history-element)))
(add-hook 'helm-cleanup-hook (lambda ()
                               (ad-deactivate 'next-history-element)
                               (ad-deactivate 'previous-history-element)))


;; Programming Tools
(defmacro helm-aif (test-form then-form &rest else-forms)
  "Like `if' but set the result of TEST-FORM in a temprary variable called `it'.
THEN-FORM and ELSE-FORMS are then excuted just like in `if'."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defun helm-mklist (obj)
  "If OBJ is a list \(but not lambda\), return itself.
Otherwise make a list with one element."
  (if (and (listp obj) (not (functionp obj)))
      obj
    (list obj)))

(defun helm-this-command ()
  "Return the actual command in action.
Like `this-command' but return the real command,
not `exit-minibuffer' or unwanted functions."
  (cl-loop with bl = '(helm-exit-minibuffer
                       exit-minibuffer)
        for count from 1 to 50
        for btf = (backtrace-frame count)
        for fn = (cl-second btf)
        if (and (commandp fn) (not (memq fn bl))) return fn
        else
        if (and (eq fn 'call-interactively)
                (> (length btf) 2))
        return (cadr (cdr btf))))


;; Helm API

(defun helm-buffer-get ()
  "Return `helm-action-buffer' if shown otherwise `helm-buffer'."
  (if (helm-action-window)
      helm-action-buffer
    helm-buffer))

(defun helm-window ()
  "Window of `helm-buffer'."
  (get-buffer-window (helm-buffer-get) 'visible))

(defun helm-action-window ()
  "Window of `helm-action-buffer'."
  (get-buffer-window helm-action-buffer 'visible))

(defmacro with-helm-window (&rest body)
  "Be sure BODY is excuted in the helm window."
  (declare (indent 0) (debug t))
  `(with-selected-window (helm-window)
     ,@body))

(defmacro with-helm-current-buffer (&rest body)
  "Eval BODY inside `helm-current-buffer'."
  (declare (indent 0) (debug t))
  `(with-current-buffer (or (and (buffer-live-p helm-current-buffer)
                                 helm-current-buffer)
                            (setq helm-current-buffer
                                  (current-buffer)))
     ,@body))

(defmacro with-helm-buffer (&rest body)
  "Eval BODY inside `helm-buffer'."
  (declare (indent 0) (debug t))
  `(with-current-buffer (helm-buffer-get)
     ,@body))

(defmacro with-helm-restore-variables(&rest body)
  "Restore `helm-restored-variables' after executing BODY."
  (declare (indent 0) (debug t))
  `(let ((orig-vars (mapcar (lambda (v)
                              (cons v (symbol-value v)))
                            helm-restored-variables)))
     (unwind-protect (progn ,@body)
       (cl-loop for (var . value) in orig-vars
             do (set var value))
       (helm-log "restore variables"))))

(defmacro with-helm-default-directory (directory &rest body)
  (declare (indent 2) (debug t))
  `(let ((default-directory (or (and ,directory
                                     (file-name-as-directory ,directory))
                                default-directory)))
     ,@body))

(defun helm-default-directory ()
  "Return the value of `helm-default-directory'."
  (buffer-local-value 'helm-default-directory (get-buffer helm-buffer)))

(defmacro with-helm-temp-hook (hook &rest body)
  "Execute temporarily BODY as a function for HOOK."
  (declare (indent 1) (debug t))
  (let ((fun (cl-gensym "helm-hook")))
    `(progn
       (defun ,fun ()
         (unwind-protect
              (progn ,@body)
           (remove-hook ,hook (quote ,fun))))
       (add-hook ,hook (quote ,fun)))))

(defmacro with-helm-after-update-hook (&rest body)
  "Execute BODY at end of `helm-update'."
  (declare (indent 0) (debug t))
  `(with-helm-temp-hook 'helm-after-update-hook ,@body))

(defmacro with-helm-alive-p (&rest body)
  "Return error when BODY run ouside helm context."
  (declare (indent 0) (debug t))
  `(progn
     (if helm-alive-p
         (progn ,@body)
       (error "Running helm command outside of context"))))

(cl-defun helm-attr (attribute-name
                     &optional (src (helm-get-current-source)) compute)
  "Get the value of ATTRIBUTE-NAME of SRC.
If SRC is omitted, use current source.
If COMPUTE is non--nil compute value of ATTRIBUTE-NAME
with `helm-interpret-value'."
  (helm-aif (or (assq attribute-name src)
                (helm-get-attribute-from-source-type attribute-name src))
      (if compute (helm-interpret-value (cdr it)) (cdr it))))

(cl-defun helm-attr-defined (attribute-name
                             &optional (src (helm-get-current-source)))
  "Return non-nil if ATTRIBUTE-NAME of SRC is defined.
if SRC is omitted, use current source."
  (and (helm-attr attribute-name src) t))

(cl-defun helm-attrset (attribute-name value
                        &optional
                          (src (helm-get-current-source))
                          alter-type)
  "Set the value of ATTRIBUTE-NAME of source SRC to VALUE.
If ATTRIBUTE-NAME doesn't exists in source it is created with value VALUE.
If ALTER-TYPE is non--nil alter the value of ATTRIBUTE-NAME in `helm-attributes'
if it exists. 
If SRC is omitted, use current source.
If operation succeed, return value, otherwise nil."
  (let ((from-type (helm-get-attribute-from-source-type attribute-name src))
        done)
    (helm-aif (or (assq attribute-name src)
                  (and alter-type from-type))
        (prog1 (setcdr it value) (setq done t))
      (unless from-type
        (setcdr src (cons (cons attribute-name value) (cdr src)))
        (setq done t)))
    (and done value)))

(defun helm-get-attribute-from-source-type (attribute source)
  "Get ATTRIBUTE from type attribute of SOURCE."
  (when (assq 'type source)
    (assq attribute
          (assq (cdr (assq 'type source))
                helm-type-attributes))))

(defun helm-get-attribute-from-type (attribute type)
  "Get ATTRIBUTE from TYPE.
arg TYPE is an existing type defined in `helm-type-attributes'."
  (assq attribute (assq type helm-type-attributes)))

(defun helm-get-actions-from-type (source)
  "Get actions list from type attribute of SOURCE."
  (when (assq 'type source)
    (helm-get-attribute-from-source-type 'action source)))

(defun helm-inherit-attribute-from-source (attribute source)
  "Get the ATTRIBUTE of SOURCE."
  (helm-aif (assq attribute source)
      it
    (helm-get-attribute-from-source-type attribute source)))

(defun helm-append-at-nth (seq elm index)
  "Append ELM at INDEX in SEQ."
  (let ((len (length seq)))
    (when (> index len) (setq index len))
    (cl-loop for i in seq
          for count from 1 collect i
          when (= count index)
          if (listp elm) append elm
          else collect elm)))

(defun helm-add-action-to-source (name fn source &optional index)
  "Add new action NAME linked to function FN to SOURCE.
Function FN should be a valid function that take one arg i.e candidate,
argument NAME is a string that will appear in action menu
and SOURCE should be an existing helm source already loaded.
If INDEX is specified, action is added in action list at INDEX,
otherwise it is added at end.
This allow user to add a specific action to an existing source
without modifying source code."
  (let ((actions    (helm-attr 'action source))
        (new-action (list (cons name fn))))
    (when (symbolp actions)
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
  (let* ((actions    (helm-attr 'action source))
         (del-action (if (symbolp action-or-name)
                         (rassoc action-or-name actions)
                       (assoc action-or-name actions))))
    (helm-attrset 'action (delete del-action actions) source)))

(cl-defun helm-add-action-to-source-if (name fn source predicate
                                        &optional (index 4) test-only)
  "Add new action NAME linked to function FN to SOURCE.
Action is added only if current candidate match PREDICATE.
This function add an entry in the `action-transformer' attribute
of SOURCE (or create one if not found).
Function PREDICATE should take one arg candidate.
Function FN should be a valid function that take one arg i.e candidate,
argument NAME is a string that will appear in action menu
and SOURCE should be an existing helm source already loaded.
If INDEX is specified, action is added in action list at INDEX.
Value of INDEX should be always >=1, default to 4.
This allow user to add a specific `action-tranformer'
to an existing source without modifying source code.
E.g
Add the action \"Byte compile file async\" linked to
function 'async-byte-compile-file to source `helm-source-find-files'
only when predicate helm-ff-candidates-lisp-p return non--nil:

\(helm-add-action-to-source-if \"Byte compile file async\"
                              'async-byte-compile-file
                              helm-source-find-files
                              'helm-ff-candidates-lisp-p\)."
  (let* ((actions     (helm-attr 'action source))
         (action-transformers (helm-attr 'action-transformer source))
         (new-action  (list (cons name fn)))
         (transformer `(lambda (actions candidate)
                         (cond ((funcall (quote ,predicate) candidate)
                                (helm-append-at-nth
                                 actions (quote ,new-action) ,index))
                               (t actions)))))
    (when (symbolp actions)
      (helm-attrset 'action (list (cons "Default action" actions)) source))
    (when (symbolp action-transformers)
      (setq action-transformers (list action-transformers)))
    (if test-only                       ; debug
        (delq nil (append (list transformer) action-transformers))
      (helm-attrset 'action-transformer
                    (delq nil (append (list transformer) action-transformers))
                    source))))

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
  (unless (and (listp sources)
               (cl-loop for name in sources always (stringp name)))
    (error "Invalid data in `helm-set-source-filter': %S" sources))
  (let ((cur-disp-sel (with-current-buffer helm-buffer
                        (helm-get-selection nil t))))
    (setq helm-source-filter sources)
    (helm-log-eval helm-source-filter)
    ;; Use force-update to run init/update functions.
    (helm-force-update (regexp-quote cur-disp-sel))))

(defun helm-set-sources (sources &optional no-init no-update)
  "Set SOURCES during `helm' invocation.
If NO-INIT is non-nil, skip executing init functions of SOURCES.
If NO-UPDATE is non-nil, skip executing `helm-update'."
  (with-current-buffer helm-buffer
    (setq helm-compiled-sources nil
          helm-sources sources)
    (helm-log-eval helm-compiled-sources helm-sources))
  (unless no-init (helm-funcall-foreach 'init))
  (unless no-update (helm-update)))

(defun helm-get-sources ()
  "Return compiled `helm-sources', which is memoized.

Attributes:

- type
  `helm-type-attributes' are merged in.
- candidates-buffer
  candidates, volatile and match attribute are created."
  (cond
    ;; action
    ((helm-action-window)
     helm-sources)
    ;; memoized
    (helm-compiled-sources)
    ;; first time
    (t
     (prog1
         (setq helm-compiled-sources
               (helm-compile-sources
                helm-sources helm-compile-source-functions))
       (helm-log-eval helm-compiled-sources)))))

(cl-defun helm-get-selection (&optional (buffer nil buffer-s)
                                force-display-part)
  "Return the currently selected item or nil.
if BUFFER is nil or unspecified, use helm-buffer as default value.
If FORCE-DISPLAY-PART is non-nil, return the display string.
If FORCE-DISPLAY-PART value is 'withprop the display string is returned
with its properties."
  (setq buffer (if (and buffer buffer-s) buffer helm-buffer))
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
                        (source (helm-get-current-source)))
                    (helm-aif (and (not force-display-part)
                                   (assoc-default 'display-to-real source))
                        (helm-funcall-with-source source it disp)
                      disp)))))
        (unless (equal selection "")
          (helm-log-eval selection)
          selection)))))

(defun helm-get-action ()
  "Return the associated action for the selected candidate.
It is a function symbol \(sole action\) or list
of \(action-display . function\)."
  (unless (helm-empty-buffer-p (helm-buffer-get))
    (helm-aif (helm-attr 'action-transformer)
        (helm-composed-funcall-with-source
         (helm-get-current-source) it
         (helm-attr 'action) (helm-get-selection))
      (helm-attr 'action))))

(defun helm-get-current-source ()
  "Return the source for the current selection.
Allow also checking if helm-buffer contain candidates."
  (or helm-current-source
      (with-current-buffer (helm-buffer-get)
        (or
         ;; This happen only when `helm-source-in-each-line-flag'
         ;; is non--nil and there is candidates in buffer.
         (get-text-property (point) 'helm-source)
         ;; Return nil when no--candidates.
         (cl-block exit
           ;; This goto-char shouldn't be necessary, but point is moved to
           ;; point-min somewhere else which shouldn't happen.
           (goto-char (overlay-start helm-selection-overlay))
           (let* ((header-pos (or (helm-get-previous-header-pos)
                                  (helm-get-next-header-pos)))
                  (source-name
                   (save-excursion
                     (unless header-pos
                       (cl-return-from exit nil))
                     (goto-char header-pos)
                     (helm-current-line-contents))))
             (cl-loop for source in (helm-get-sources) thereis
                   (and (equal (assoc-default 'name source) source-name)
                        source))))))))

(defun helm-buffer-is-modified (buffer)
  "Return non-nil when BUFFER is modified since `helm' was invoked."
  (let* ((b (get-buffer buffer))
         (key (concat (buffer-name b) "/" (helm-attr 'name)))
         (source-tick (or (gethash key helm-tick-hash) 0))
         (buffer-tick (buffer-chars-modified-tick b))
         (modifiedp (/= source-tick buffer-tick)))
    (puthash key buffer-tick helm-tick-hash)
    (helm-log-eval buffer modifiedp)
    modifiedp))

(defun helm-current-buffer-is-modified ()
  "Check if `helm-current-buffer' is modified since `helm' was invoked."
  (helm-buffer-is-modified helm-current-buffer))

(defun helm-run-after-quit (function &rest args)
  "Perform an action after quitting `helm'.
The action is to call FUNCTION with arguments ARGS."
  (setq helm-quit t)
  (helm-kill-async-processes)
  (helm-log-eval function args)
  (apply 'run-with-timer 0.1 nil function args)
  (helm-exit-minibuffer))


(defun define-helm-type-attribute (type definition &optional doc)
  "Register type attribute of TYPE as DEFINITION with DOC.
DOC is displayed in `helm-type-attributes' docstring.

Use this function is better than setting `helm-type-attributes' directly."
  (cl-loop for i in definition do
        ;; without `ignore-errors', error at emacs22
        (ignore-errors (setf i (delete nil i))))
  (helm-add-type-attribute type definition)
  (and doc (helm-document-type-attribute type doc))
  nil)

(defun helm-document-attribute (attribute short-doc &optional long-doc)
  "Register ATTRIBUTE documentation introduced by plug-in.
SHORT-DOC is displayed beside attribute name.
LONG-DOC is displayed below attribute name and short documentation."
  (if long-doc
      (setq short-doc (concat "(" short-doc ")"))
    (setq long-doc short-doc
          short-doc ""))
  (add-to-list 'helm-attributes attribute t)
  (put attribute 'helm-attrdoc
       (concat "- " (symbol-name attribute)
               " " short-doc "\n\n" long-doc "\n")))

(put 'helm-document-attribute 'lisp-indent-function 2)

(defun helm-interpret-value (value &optional source)
  "Interpret VALUE as variable, function or literal and return it.
If VALUE is a function, call it with no arguments and return the value.
If SOURCE compute VALUE for this source.
If VALUE is a variable, return the value.
If VALUE is a symbol, but it is not a function or a variable, cause an error.
Otherwise, return VALUE itself."
  (cond ((and source (functionp value))
         (helm-funcall-with-source source value))
        ((functionp value)
         (funcall value))
        ((and (symbolp value) (boundp value))
         (symbol-value value))
        ((symbolp value)
         (error
          "helm-interpret-value: Symbol must be a function or a variable"))
        (t
         value)))


;; Core: API helper
(cl-defun helm-empty-buffer-p (&optional (buffer helm-buffer))
  "Check if BUFFER have candidates.
Default value for BUFFER is `helm-buffer'."
  (zerop (buffer-size (and buffer (get-buffer buffer)))))

(defun helm-empty-source-p ()
  "Check if current source contains candidates.
This happen only in certains cases when e.g the last element
of a source is deleted without updating the source."
  (with-helm-window
    (or (helm-empty-buffer-p)
        (and (helm-end-of-source-p)
             (eq (point-at-bol) (point-at-eol))
             (or
              (save-excursion
                (forward-line -1)
                (helm-pos-header-line-p))
              (bobp))))))

(defun helm-let-internal (binding bodyfunc)
  "Set BINDING to helm buffer-local variables and Evaluate BODYFUNC.
BINDING is a list of (VARNAME . VALUE) pair.
The BINDING list should be created with `helm-parse-keys' when `helm'
is called.
Each KEYS VARNAME of elements of BINDING will be bound locally
to VALUE by `helm-create-helm-buffer'."
  (setq helm-let-variables binding)
  (unwind-protect
       (funcall bodyfunc)
    (setq helm-let-variables nil)))


;; Core: tools
(defun helm-current-line-contents ()
  "Current line string without properties."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun helm-funcall-with-source (source functions &rest args)
  "Call from SOURCE FUNCTIONS list or single function FUNCTIONS with ARGS.
FUNCTIONS can be a symbol or a list of functions.
Return the result of last function call."
  (let ((helm-source-name (assoc-default 'name source))
        (helm-current-source source)
        (funs (if (functionp functions) (list functions) functions)))
    (helm-log-eval helm-source-name functions args)
    (cl-loop with result for fn in funs
          do (setq result (apply fn args)) finally return result)))

(defun helm-funcall-foreach (sym &optional sources)
  "Call the function SYM for each source if any."
  (let ((sources (or sources (helm-get-sources))))
    (cl-dolist (source sources)
      (helm-aif (assoc-default sym source)
          (helm-funcall-with-source source it)))))

(defun helm-normalize-sources (sources)
  "If SOURCES is only one source, make a list of one element."
  (cond ((or (and sources
                  (symbolp sources))
             (and (listp sources) (assq 'name sources)))
         (list sources))
        (sources)
        (t helm-sources)))

(defun helm-get-candidate-number (&optional in-current-source)
  "Return candidates number in `helm-buffer'.
If IN-CURRENT-SOURCE is provided return number of candidates
in the source where point is."
  (with-current-buffer helm-buffer
    (if (or (helm-empty-buffer-p)
            (helm-empty-source-p))
        0
      (save-excursion
        (if in-current-source
            (goto-char (helm-get-previous-header-pos))
          (goto-char (point-min)))
        (forward-line 1)
        (if (helm-pos-multiline-p)
            (save-excursion
              (cl-loop with count-multi = 1
                    while (and (not (if in-current-source
                                        (save-excursion
                                          (forward-line 2)
                                          (or (helm-pos-header-line-p) (eobp)))
                                      (eobp)))
                               (search-forward helm-candidate-separator nil t))
                    do (cl-incf count-multi)
                    finally return count-multi))
          (save-excursion
            (cl-loop with ln = 0
                  while (not (if in-current-source
                                 (or (helm-pos-header-line-p) (eobp))
                               (eobp)))
                  unless (helm-pos-header-line-p)
                  do (cl-incf ln)
                  do (forward-line 1) finally return ln)))))))

(defmacro with-helm-quittable (&rest body)
  "If an error occur in execution of BODY, quit helm safely."
  (declare (indent 0) (debug t))
  `(let (inhibit-quit)
     (condition-case _v
         (progn ,@body)
       (quit (setq helm-quit t)
             (exit-minibuffer)
             (keyboard-quit)))))

(defun helm-compose (arg-lst func-lst)
  "Apply arguments specified in ARG-LST with each function of FUNC-LST.
The result of each function will be the new `car' of ARG-LST.
Each function in FUNC-LST must accept (length ARG-LST) arguments
\(See examples below) .
This function allows easy sequencing of transformer functions.
Where generally, ARG-LST is '(candidates-list source) and FUNC-LST a
list of transformer functions that take one or two arguments depending
we are using 'filtered-candidate-transformer' or 'candidate-transformer'.
e.g
filtered-candidate-transformer:
\(helm-compose '((1 2 3 4 5 6 7)
                '((name . \"A helm source\") (candidates . (a b c))))
              '((lambda (candidates _source)
                  (cl-loop for i in candidates
                        when (cl-oddp i) collect i))
                (lambda (candidates _source)
                  (cl-loop for i in candidates collect (1+ i)))))
=>(2 4 6 8)

candidate-transformer:
\(helm-compose '((1 2 3 4 5 6 7))
                '((lambda (candidates)
                  (cl-loop for i in candidates
                        when (cl-oddp i) collect i))
                (lambda (candidates)
                  (cl-loop for i in candidates collect (1+ i)))))
=> (2 4 6 8)."
  (cl-dolist (func func-lst)
    (setcar arg-lst (apply func arg-lst)))
  (car arg-lst))

(defun helm-composed-funcall-with-source (source funcs &rest args)
  "With SOURCE apply `helm-funcall-with-source' with each FUNCS and ARGS.
This is used in transformers to modify candidates list."
  (if (functionp funcs)
      (apply 'helm-funcall-with-source source funcs args)
    (apply 'helm-funcall-with-source source
           (lambda (&rest oargs) (helm-compose oargs funcs))
           args)))


;; Core: entry point
(defconst helm-argument-keys
  '(:sources :input :prompt :resume
    :preselect :buffer :keymap :default :history))

(defun helm (&rest plist)
  "Main function to execute helm sources.

Keywords supported:
:sources :input :prompt :resume :preselect
:buffer :keymap :default :history :allow-nest

Extra keywords are supported and can be added, see below.

PLIST is a list like \(:key1 val1 :key2 val2 ...\) or
\(&optional sources input prompt resume
            preselect buffer keymap default history\).

Basic keywords are the following:

\:sources

A list of sources used for this session.  It also accepts a
symbol, interpreted as a variable of a helm source
i.e (a symbol can be passed instead of a list of sources).
It also accepts an alist representing a helm source, which is
detected by \(assq 'name ANY-SOURCES\).
NOTE: In this case the source is embedded in the helm command and
have no symbol name, so it is not reachable from outside.
It will be referenced in `helm-sources' as a whole alist.

\:input

Temporary value of `helm-pattern', ie. initial input of minibuffer.

\:prompt

Prompt other than \"pattern: \".

\:resume

If t, Resurrect previously instance of `helm'.  Skip the initialization.
If 'noresume, this instance of `helm' cannot be resumed.

\:preselect

Initially selected candidate.  Specified by exact candidate or a regexp.

\:buffer

`helm-buffer' instead of *helm*.

\:keymap

`helm-map' for current `helm' session.

\:default

A default argument that will be inserted in minibuffer \
with \\<minibuffer-local-map>\\[next-history-element].
When nil or not present `thing-at-point' will be used instead.
If `helm-maybe-use-default-as-input' is non--nil display will be
updated using :default arg as input unless :input is specified,
which in this case will take precedence on :default
This is a string or a list, in this case the car of the list will
be used as initial default input, but you will be able to cycle in this
list with \\<minibuffer-local-map>\\[next-history-element].

\:history

By default all minibuffer input is pushed to `minibuffer-history',
if an argument HISTORY is provided, input will be pushed to HISTORY.
History element should be a symbol.

\:allow-nest

Allow running this helm command within a running helm session.

Of course, conventional arguments are supported, the two are same.

\(helm :sources sources :input input :prompt prompt :resume resume
       :preselect preselect :buffer buffer :keymap keymap :default default
       :history history\)

and

\(helm sources input prompt resume preselect buffer keymap default history\)

are the same.

However the use of non keyword args is deprecated and should not be used.

Other keywords are interpreted as local variables of this helm session.
The `helm-' prefix can be omitted.  For example,

\(helm :sources 'helm-source-buffers-list
       :buffer \"*buffers*\" :candidate-number-limit 10\)

means starting helm session with `helm-source-buffers'
source in *buffers* buffer and set variable `helm-candidate-number-limit'
to 10 as session local variable."
  (let ((fn (cond ((or (and helm-alive-p (plist-get plist :allow-nest))
                       (and helm-alive-p (memq 'allow-nest plist)))
                   #'helm-nest)
                  ((keywordp (car plist))
                   #'helm)
                  (t #'helm-internal))))
    (if (and helm-alive-p (eq fn #'helm))
        (if (helm-alive-p)
            ;; A helm session is normally running.
            (error "Error: Trying to run helm within a running helm session")
          ;; A helm session is already running and user jump somewhere else
          ;; without desactivating it: weird.
          (with-helm-buffer
            (prog1
                (message "Aborting an helm session running in background")
              ;; helm-alive-p will be reset in unwind-protect forms.
              (helm-keyboard-quit)))) 
      (if (keywordp (car plist))
          ;; Recursion: [1] Call `helm' on itself with plist args converted
          ;; to simple args will end up to [2] and call `helm-internal' with
          ;; simple args.
          ;; (`helm-let-internal' is not exited until recursion finish)
          (helm-let-internal
           (helm-parse-keys plist)
           (lambda () ; [1]
             (apply fn (mapcar #'(lambda (key) (plist-get plist key))
                               helm-argument-keys))))
        (apply fn plist))))) ; [2]

(defun helm-alive-p ()
  "Check if `helm' is alive.
An `helm' session is considered alive if `helm-alive-p' is non--nil,
the `helm-buffer' is visible, and cursor is in minibuffer."
  (and helm-alive-p
       (get-buffer-window helm-buffer 'visible)
       (minibuffer-window-active-p (minibuffer-window))
       (minibufferp (current-buffer))))

(defun helm-parse-keys (keys)
  "Parse the KEYS arguments of `helm'.
Return only the keys that are not in `helm-argument-keys'.
It is used to set local variables via `helm-let-internal'.
This allow to add arguments that are not part of `helm-argument-keys',
but are valid helm attributes.
i.e :candidate-number-limit will be bound to `helm-candidate-number-limit'
in source."
  ;; (helm-parse-keys '(:sources ((name . "test")
  ;;                                  (candidates . (a b c)))
  ;;                        :buffer "toto"
  ;;                        :candidate-number-limit 4))
  ;; ==> ((helm-candidate-number-limit . 4))
  (cl-loop for (key value) on keys by #'cddr
        for symname = (substring (symbol-name key) 1)
        for sym = (intern (if (string-match "^helm-" symname)
                              symname
                            (concat "helm-" symname)))
        unless (memq key helm-argument-keys)
        collect (cons sym value)))

;;; Core: entry point helper
(defun helm-internal (&optional
                        any-sources any-input
                        any-prompt any-resume
                        any-preselect any-buffer
                        any-keymap any-default any-history)
  "The internal helm function called by `helm'.
For ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT ANY-BUFFER and
ANY-KEYMAP ANY-DEFAULT ANY-HISTORY See `helm'."
  ;; Activate the advice for `tramp-read-passwd'.
  (if (fboundp 'advice-add)
      (advice-add 'tramp-read-passwd :around #'helm--advice-tramp-read-passwd)
    (ad-activate 'tramp-read-passwd))
  (catch 'exit ; `exit-minibuffer' use this tag on exit.
    (helm-log (concat "[Start session] " (make-string 41 ?+)))
    (helm-log-eval any-prompt any-preselect
                   any-buffer any-keymap any-default)
    (let ((old-overriding-local-map overriding-terminal-local-map)
          ;; #163 no cursor in minibuffer in <=Emacs-24.2.
          ;; Apart this bug in <=24.2, this is needed for
          ;; messages in minibuffer on top of helm prompt. 
          (cursor-in-echo-area t)
          (non-essential t)
          (old--cua cua-mode)
          (helm-maybe-use-default-as-input
           (or helm-maybe-use-default-as-input ; it is let-bounded so use it.
               (cl-loop for s in (helm-normalize-sources any-sources)
                     thereis (memq s helm-sources-using-default-as-input)))))
      ;; cua-mode overhide local helm bindings.
      ;; disable this stupid thing if enabled.
      (and cua-mode (cua-mode -1))
      (unwind-protect
           (condition-case _v
               (let (;; `helm-source-name' is non-nil
                     ;; when `helm' is invoked by action, reset it.
                     helm-source-name
                     helm-current-source
                     helm-in-persistent-action
                     helm-quit
                     (helm-buffer (or any-buffer helm-buffer)))
                 (with-helm-restore-variables
                   (helm-initialize any-resume any-input
                                    any-default any-sources)
                   (helm-display-buffer helm-buffer)
                   (add-hook 'post-command-hook 'helm--maybe-update-keymap)
                   (helm-log "show prompt")
                   (unwind-protect
                        (helm-read-pattern-maybe
                         any-prompt any-input any-preselect
                         any-resume any-keymap any-default
                         (when (and any-history (symbolp any-history))
                           any-history))
                     (helm-cleanup)))
                 (prog1 (unless helm-quit
                          (with-helm-temp-hook 'helm-cleanup-hook
                            (setq overriding-terminal-local-map old-overriding-local-map))
                          (helm-execute-selection-action))
                   (helm-log (concat "[End session] " (make-string 41 ?-)))))
             (quit
              (helm-restore-position-on-quit)
              (helm-log (concat "[End session (quit)] " (make-string 34 ?-)))
              nil))
        (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
        (if (fboundp 'advice-add)
            (advice-remove 'tramp-read-passwd #'helm--advice-tramp-read-passwd)
          (ad-deactivate 'tramp-read-passwd))
        (helm-log-eval (setq helm-alive-p nil))
        (setq overriding-terminal-local-map old-overriding-local-map)
        (setq helm-alive-p nil)
        (setq helm-in-file-completion-p nil)
        (and old--cua (cua-mode 1))
        (helm-log-save-maybe)))))


;;; Helm resume
;;
;;
;;;###autoload
(defun helm-resume (arg)
  "Resurrect previously invoked `helm'.
Called with a prefix arg, allow choosing among all existing
helm buffers.  i.e choose among various helm sessions.
Called from lisp, you can specify a buffer-name as a string with ARG."
  (interactive "P")
  (let (any-buffer helm-full-frame cur-dir)
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
                   'helm-default-directory (get-buffer any-buffer)))
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
         :resume t
         :buffer any-buffer))))

;;;###autoload
(defun helm-resume-previous-session-after-quit (arg)
  "Resume previous helm session within running helm."
  (interactive "p")
  (if (and helm-alive-p (> (length helm-buffers) arg))
      (helm-run-after-quit `(lambda () (helm-resume (nth ,arg helm-buffers))))
    (message "No previous helm sessions to resume yet!")))

;;;###autoload
(defun helm-resume-list-buffers-after-quit ()
  "List resumable helm buffers within running helm."
  (interactive)
  (if (and helm-alive-p (> (length helm-buffers) 0))
      (helm-run-after-quit #'(lambda () (helm-resume t)))
    (message "No previous helm sessions to resume yet!")))

(defun helm-resume-p (any-resume)
  "Whether current helm session is resumed or not."
  (eq any-resume t))

(defun helm-resume-select-buffer ()
  "Select an `helm-buffer' in `helm-buffers' list to resume a helm session.
Return nil if no `helm-buffer' found."
  (when helm-buffers
    (or (helm :sources '(((name . "Resume helm buffer")
                          (candidates . helm-buffers)
                          (action . identity)))
              :resume 'noresume
              :buffer "*helm resume*")
        (keyboard-quit))))


;;;###autoload
(defun helm-other-buffer (any-sources any-buffer)
  "Simplified interface of `helm' with other `helm-buffer'.
Call `helm' with only ANY-SOURCES and ANY-BUFFER as args."
  (helm :sources any-sources :buffer any-buffer))

(defun helm-nest (&rest same-as-helm)
  "Allow calling `helm' whithin a running helm session."
  (with-helm-window
    (let ((orig-helm-current-buffer helm-current-buffer)
          (orig-helm-buffer helm-buffer)
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
          (helm-initialize-overlays helm-buffer)
          (unless (helm-empty-buffer-p) (helm-mark-current-line t))
          (setq helm-last-frame-or-window-configuration
                orig-helm-last-frame-or-window-configuration)
          (setq cursor-type t)
          (setq helm-current-buffer orig-helm-current-buffer)
          (setq helm-onewindow-p orig-one-window-p))))))


;;; Core: Accessors
;;
(defun helm-current-position (save-or-restore)
  "Restore or save current position in `helm-current-buffer'.
Argument SAVE-OR-RESTORE is one of save or restore."
  (cl-case save-or-restore
    (save
     (helm-log "Save position at %S" (cons (point) (window-start)))
     (setq helm-current-position (cons (point) (window-start))))
    (restore
     (helm-log "Restore position at  %S in buffer %s"
               helm-current-position
               (buffer-name (current-buffer)))
     (goto-char (car helm-current-position))
     ;; Fix this position with the NOFORCE arg of `set-window-start'
     ;; otherwise, if there is some other buffer than `helm-current-buffer'
     ;; one, position will be lost.
     (set-window-start (selected-window) (cdr helm-current-position) t))))


(defun helm-frame-or-window-configuration (save-or-restore)
  "Save or restore last frame or window configuration.
Possible value of SAVE-OR-RESTORE are 'save and 'restore.
window or frame configuration is saved/restored according to values of
`helm-save-configuration-functions'."
  (helm-log-eval helm-save-configuration-functions)
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
               (let ((frame (if (minibufferp helm-current-buffer)
                                (selected-frame)
                              (last-nonminibuffer-frame))))
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
The function used to display `helm-buffer'."
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
  "Allow setting `no-other-window' window parameter in all windows.
Arg ENABLE will be the value of the `no-other-window' window property."
  (walk-windows
   #'(lambda (w)
       (unless (window-dedicated-p w)
         (set-window-parameter w 'no-other-window enabled))) 0))

(defun helm-default-display-buffer (buffer)
  "Default function to display `helm-buffer' BUFFER.
It uses `switch-to-buffer' or `pop-to-buffer' depending of value of
`helm-full-frame' and/or `helm-split-window-default-side'."
  (if (or (buffer-local-value 'helm-full-frame (get-buffer buffer))
          (and (eq helm-split-window-default-side 'same)
               (one-window-p t)))
      (progn (delete-other-windows) (switch-to-buffer buffer))
    (when (and helm-always-two-windows
               (not (eq helm-split-window-default-side 'same))
               (not (minibufferp helm-current-buffer))
               (not helm-split-window-in-side-p))
      (delete-other-windows))
    (pop-to-buffer buffer)))


;;; Core: initialize
;;
(defun helm-initialize (any-resume any-input any-default any-sources)
  "Start initialization of `helm' session.
For ANY-RESUME ANY-INPUT ANY-DEFAULT and ANY-SOURCES See `helm'."
  (helm-log "start initialization: any-resume=%S any-input=%S"
            any-resume any-input)
  (helm-frame-or-window-configuration 'save)
  (setq helm-sources (helm-normalize-sources any-sources))
  (helm-log "sources = %S" helm-sources)
  (helm-current-position 'save)
  (if (helm-resume-p any-resume)
      (helm-initialize-overlays (helm-buffer-get))
    (helm-initial-setup any-default))
  (setq helm-alive-p t)
  (unless (eq any-resume 'noresume)
    (helm-recent-push helm-buffer 'helm-buffers)
    (setq helm-last-buffer helm-buffer))
  (when any-input (setq helm-input any-input helm-pattern any-input))
  (and (helm-resume-p any-resume) (helm-funcall-foreach 'resume))
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
    (overlay-put helm-selection-overlay 'face 'helm-selection)))

(defun helm-restore-position-on-quit ()
  "Restore position in `helm-current-buffer' when quitting."
  (helm-current-position 'restore))

(defun helm-recent-push (elt list-var)
  "Add ELT to the value of LIST-VAR as most recently used value."
  (let ((m (member elt (symbol-value list-var))))
    (and m (set list-var (delq (car m) (symbol-value list-var))))
    (push elt (symbol-value list-var))))

(defun helm--current-buffer ()
  "[internal] Return `current-buffer' BEFORE `helm-buffer' is initialized.
Note that this will return the minibuffer in use after helm have started,
so to get the buffer where helm started while in a helm session,
use `helm-current-buffer'.
It is intended to use this only in `helm-initial-setup'."
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

(defun helm-initial-setup (any-default)
  "Initialize helm settings and set up the helm buffer."
  (helm-log-run-hook 'helm-before-initialize-hook)
  (setq helm-current-prefix-arg nil)
  (setq helm-suspend-update-flag nil)
  (setq helm-delayed-init-executed nil)
  (setq helm-current-buffer (helm--current-buffer))
  (setq helm-buffer-file-name buffer-file-name)
  (setq helm-issued-errors nil)
  (setq helm-compiled-sources nil)
  (setq helm-saved-current-source nil)
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
  (helm-funcall-foreach
   'init (and helm-source-filter
              (cl-remove-if-not #'(lambda (s)
                                    (member (assoc-default 'name s)
                                            helm-source-filter))
                                (helm-get-sources))))
  (setq helm-pattern (or (and helm-maybe-use-default-as-input
                              (or (if (listp any-default)
                                      (car any-default) any-default)
                                  (with-helm-current-buffer
                                    (thing-at-point 'symbol))))
                         ""))
  (setq helm-input "")
  (clrhash helm-candidate-cache)
  (helm-create-helm-buffer)
  (helm-clear-visible-mark)
  (helm-log-run-hook 'helm-after-initialize-hook))

(defun helm-create-helm-buffer ()
  "Create and setup `helm-buffer'."
  (let ((root-dir default-directory))
    (with-current-buffer (get-buffer-create helm-buffer)
      (helm-log "kill local variables: %S" (buffer-local-variables))
      (kill-all-local-variables)
      (set (make-local-variable 'inhibit-read-only) t)
      (buffer-disable-undo)
      (erase-buffer)
      (set (make-local-variable 'helm-map) helm-map)
      (make-local-variable 'helm-sources)
      (set (make-local-variable 'helm-follow-mode) nil)
      (set (make-local-variable 'helm-display-function) helm-display-function)
      (set (make-local-variable 'helm-selection-point) nil)
      (set (make-local-variable 'scroll-margin)
           (if helm-display-source-at-screen-top
               0 helm-completion-window-scroll-margin))
      (set (make-local-variable 'helm-default-directory) root-dir)
      (set (make-local-variable 'default-directory) root-dir)
      (set (make-local-variable 'helm-marked-candidates) nil)
      (helm-initialize-persistent-action)
      (helm-log-eval helm-display-function helm-let-variables)
      (cl-loop for (var . val) in helm-let-variables
            do (set (make-local-variable var) val))
      (setq truncate-lines helm-truncate-lines) ; already local.
      (setq cursor-type nil)
      (setq mode-name "Helm"))
    (helm-initialize-overlays helm-buffer)
    (get-buffer helm-buffer)))

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
  (with-current-buffer (helm-buffer-get)
    (let* ((src        (helm-get-current-source))
           (src-keymap (assoc-default 'keymap src))
           (hist       (or any-history
                           ;; Needed for resuming. 
                           (assoc-default 'history src)))
           (timer nil)
           blink-matching-paren
           (first-src (car helm-sources))
           (source-delayed-p (or (assq 'delayed src)
                                 (assq 'delayed (if (symbolp first-src)
                                                    (symbol-value first-src)
                                                  first-src)))))
      ;; Startup with the first keymap found either in current source
      ;; or helm arg, otherwise use global value of `helm-map'.
      ;; This map will be used as a `minibuffer-local-map'.
      ;; Maybe it will be overriden when changing source
      ;; by `helm--maybe-update-keymap'.
      ;; Note that helm-map have been made buffer-local
      ;; in `helm-create-helm-buffer'.
      (setq helm-map (or src-keymap any-keymap helm-map))
      (helm-log-eval (helm-get-candidate-number)
                     helm-execute-action-at-once-if-one
                     helm-quit-if-no-candidate)
      ;; If source is delayed `helm-execute-action-at-once-if-one'
      ;; and `helm-quit-if-no-candidate' are handled after update finish.
      (when source-delayed-p
        ;; Note that we quickly add the hook now when `helm-update'
        ;; is already started, but because source is delayed the hook
        ;; should have the time to be passed !!!
        ;; the hook will remove itself once done.
        (with-helm-after-update-hook (helm-exit-or-quit-maybe)))
      ;; Reset `helm-pattern' for non--delayed sources and update
      ;; display if no result found with precedent value of `helm-pattern'
      ;; unless `helm-quit-if-no-candidate' is non--nil, in this case
      ;; Don't force update with an empty pattern.
      ;; Reset also `helm-maybe-use-default-as-input' as this checking
      ;; happen only on startup.
      (when (and helm-maybe-use-default-as-input (not source-delayed-p))
        (setq helm-pattern "")
        (setq helm-maybe-use-default-as-input nil)
        (and (helm-empty-buffer-p)
             (null helm-quit-if-no-candidate)
             (helm-force-update)))
      ;; Handle `helm-execute-action-at-once-if-one' and
      ;; `helm-quit-if-no-candidate' now only for not--delayed sources.
      (cond ((and helm-execute-action-at-once-if-one
                  (not source-delayed-p)
                  (= (helm-get-candidate-number) 1))
             (ignore)) ; Don't enter the minibuffer loop.
            ((and helm-quit-if-no-candidate
                  (not source-delayed-p)
                  (= (helm-get-candidate-number) 0))
             (setq helm-quit t)
             (and (functionp helm-quit-if-no-candidate)
                  (funcall helm-quit-if-no-candidate)))
            (t ; Enter now minibuffer and wait for input.
             (let ((tap (or any-default
                            (with-helm-current-buffer
                              (thing-at-point 'symbol)))))
               (unwind-protect
                    (minibuffer-with-setup-hook
                        #'(lambda ()
                            (setq timer (run-with-idle-timer
                                         (max helm-input-idle-delay 0.001) 'repeat
                                         #'(lambda ()
                                             ;; Stop updating when in persistent action
                                             ;; or when `helm-suspend-update-flag' is
                                             ;; non--nil.
                                             (unless (or helm-in-persistent-action
                                                         helm-suspend-update-flag)
                                               (save-selected-window
                                                 (helm-check-minibuffer-input)
                                                 (helm-print-error-messages)))))))
                      (read-from-minibuffer (or any-prompt "pattern: ")
                                            any-input helm-map
                                            nil hist tap t))
                 (when timer (cancel-timer timer) (setq timer nil)))))))))

(defun helm-exit-or-quit-maybe ()
  "Exit and run default action if only one candidate, quit if no candidates.
This function is handling `helm-execute-action-at-once-if-one' and
`helm-quit-if-no-candidate' in delayed sources."
  (with-helm-window
    (cond ((and helm-execute-action-at-once-if-one
                (= (helm-get-candidate-number) 1))
           (helm-exit-minibuffer))
          ((and helm-quit-if-no-candidate
                (= (helm-get-candidate-number) 0))
           (setq helm-quit t)
           (and (functionp helm-quit-if-no-candidate)
                (funcall helm-quit-if-no-candidate))
           (keyboard-quit)))))

;;;###autoload
(defun helm-toggle-suspend-update ()
  "Enable or disable update of display in helm.
This can be useful for e.g writing quietly a complex regexp."
  (interactive)
  (when (setq helm-suspend-update-flag (not helm-suspend-update-flag))
    (helm-kill-async-processes)
    (setq helm-pattern ""))
  (message (if helm-suspend-update-flag
               "Helm update suspended!"
             "Helm update reenabled!")))

(defadvice tramp-read-passwd (around disable-helm-update)
  ;; Suspend update when prompting for a tramp password.
  (setq helm-suspend-update-flag t)
  (setq overriding-terminal-local-map nil)
  (let (stimers)
    (unwind-protect
         (progn
           (setq stimers (with-timeout-suspend))
           ad-do-it)
      (with-timeout-unsuspend stimers)
      (setq helm-suspend-update-flag nil))))

(defun helm--advice-tramp-read-passwd (old--fn &rest args)
  ;; Suspend update when prompting for a tramp password.
  (setq helm-suspend-update-flag t)
  (setq overriding-terminal-local-map nil)
  (unwind-protect
       ;; No need to suspend timer in emacs-24.4
       (apply old--fn args)
    (setq helm-suspend-update-flag nil)))

(defun helm--maybe-update-keymap ()
  "Handle differents keymaps in multiples sources.

It will override `helm-map' with the local map of current source.
If no map is found in current source do nothing (keep previous map)."
  (with-helm-buffer
    (helm-aif (assoc-default 'keymap (helm-get-current-source))
        ;; Fix #466; we use here set-transient-map
        ;; to not overhide other minor-mode-map's.
        (if (fboundp 'set-transient-map)
            (set-transient-map it)
            (set-temporary-overlay-map it)))))


;; Core: clean up

(defun helm-cleanup ()
  "Clean up the mess when helm exit or quit."
  (helm-log "start cleanup")
  (with-current-buffer helm-buffer
    ;; bury-buffer from this window.
    (bury-buffer) ;[1]
    ;; Be sure we call this from helm-buffer.
    (helm-funcall-foreach 'cleanup))
  (helm-kill-async-processes)
  (helm-log-run-hook 'helm-cleanup-hook)
  (helm-frame-or-window-configuration 'restore)
  ;; [1] now bury-buffer from underlying windows otherwise,
  ;; if this window is killed the underlying buffer will
  ;; be a helm buffer.
  (replace-buffer-in-windows helm-buffer)
  (setq helm-alive-p nil)
  (setq helm-in-file-completion-p nil)
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


;;; Core: input handling
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
  ;; First time minibuffer is entered
  ;; we check value of `helm-pattern' that have been set
  ;; in `helm-initial-setup' when `helm-maybe-use-default-as-input'
  ;; is non--nil.  After this initial check, reset
  ;; `helm-maybe-use-default-as-input' and ignore this.
  ;; This happen only when source is `delayed'.
  (when helm-maybe-use-default-as-input ; nil when non--delayed.
    (setq input helm-pattern)
    (with-helm-after-update-hook (setq helm-pattern ""))
    (setq helm-maybe-use-default-as-input nil))
  ;; In delayed sources `helm-pattern' have not been resat yet.
  (unless (equal input helm-pattern)
    (setq helm-pattern input)
    (unless (helm-action-window)
      (setq helm-input helm-pattern))
    (helm-log-eval helm-pattern helm-input)
    (helm-update)))


;;; Core: source compiler
;;
;;
(defun helm-compile-sources (sources funcs)
  "Compile SOURCES with FUNCS.
See `helm-compile-source-functions'.
Helm plug-ins are realized by this function."
  (mapcar
   (lambda (source)
     (cl-loop with src = (if (listp source) source (symbol-value source))
           for f in funcs
           do (setq src (funcall f src))
           finally (cl-return src)))
   sources))


;; Core: all candidates
(defun helm-process-delayed-init (source)
  "Initialize delayed SOURCE."
  (let ((name (assoc-default 'name source)))
    (unless (member name helm-delayed-init-executed)
      (helm-aif (assoc-default 'delayed-init source)
          (with-current-buffer helm-current-buffer
            (helm-funcall-with-source source it)
            (cl-dolist (_f (if (functionp it) (list it) it))
              (add-to-list 'helm-delayed-init-executed name)))))))

(defun helm-get-candidates (source)
  "Retrieve and return the list of candidates from SOURCE."
  (helm-process-delayed-init source)
  (let* (inhibit-quit
         (candidate-fn (assoc-default 'candidates source))
         (candidate-proc (assoc-default 'candidates-process source))
         (type-error (lambda ()
                       (error
                        "`%s' must either be a function, a variable or a list"
                        (or candidate-fn candidate-proc))))
         (candidates (condition-case err
                         ;; Process candidates-(process) function
                         ;; It may return a process or a list of candidates.
                         (if candidate-proc
                             (helm-interpret-value candidate-proc source)
                           (helm-interpret-value candidate-fn source))
                       (error (helm-log "Error: %S" err) nil))))
    (when (and (processp candidates) (not candidate-proc))
      (warn "Candidates function `%s' should be called in a `candidates-process' attribute"
            candidate-fn))
    (cond ((processp candidates)
           ;; Candidates will be filtered later in process filter.
           candidates)
          ((null candidates) candidates)
          ((listp candidates)
           ;; Transform candidates with `candidate-transformer' functions if
           ;; some, otherwise return candidates.
           (helm-transform-candidates candidates source))
          (t (funcall type-error)))))

(defmacro helm-while-no-input (&rest body)
  "Same as `while-no-input' but without testing with `input-pending-p'."
  (declare (debug t) (indent 0))
  (let ((catch-sym (make-symbol "input"))
        inhibit-quit)
    `(with-local-quit
       (catch ',catch-sym
	 (let ((throw-on-input ',catch-sym))
           ,@body)))))

(defun helm-get-cached-candidates (source)
  "Return the cached value of candidates for SOURCE.
Cache the candidates if there is not yet a cached value."
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
              ((not (assoc 'volatile source))
               (puthash name candidates helm-candidate-cache)))
        candidates))))


;;; Core: candidate transformers
(defun helm-transform-mapcar (function args)
  "`mapcar' for candidate-transformer.

ARGS is (cand1 cand2 ...) or ((disp1 . real1) (disp2 . real2) ...)

\(helm-transform-mapcar 'upcase '(\"foo\" \"bar\"))
=> (\"FOO\" \"BAR\")
\(helm-transform-mapcar 'upcase '((\"1st\" . \"foo\") (\"2nd\" . \"bar\")))
=> ((\"1st\" . \"FOO\") (\"2nd\" . \"BAR\"))
"
  (cl-loop for arg in args
        if (consp arg)
        collect (cons (car arg) (funcall function (cdr arg)))
        else
        collect (funcall function arg)))

(defun helm-process-candidate-transformer (candidates source)
  "Execute `candidate-transformer' function(s) on CANDIDATES in SOURCE."
  (helm-aif (assoc-default 'candidate-transformer source)
      (helm-composed-funcall-with-source source it candidates)
    candidates))

(defun helm-process-filtered-candidate-transformer (candidates source)
  "Execute `filtered-candidate-transformer' function(s) on CANDIDATES in SOURCE."
  (helm-aif (assoc-default 'filtered-candidate-transformer source)
      (helm-composed-funcall-with-source source it candidates source)
    candidates))

(defmacro helm--maybe-process-filter-one-by-one-candidate (candidate source)
  "Execute `filter-one-by-one' function(s) on CANDIDATE in SOURCE."
  `(helm-aif (assoc-default 'filter-one-by-one ,source)
       (if (listp it)
           (cl-loop for f in it
                 do (setq ,candidate (funcall f ,candidate)))
         (setq ,candidate (funcall it ,candidate)))))

(defun helm--initialize-one-by-one-candidates (candidates source)
  "Process the CANDIDATES with the `filter-one-by-one' function in SOURCE.
Return CANDIDATES when pattern is empty."
  (helm-aif (and (string= helm-pattern "")
                 (assoc-default 'filter-one-by-one source))
      (cl-loop for cand in candidates
            do (helm--maybe-process-filter-one-by-one-candidate cand source)
            collect cand)
    candidates))

(defun helm-process-filtered-candidate-transformer-maybe
    (candidates source process-p)
  "Execute `filtered-candidate-transformer' function(s) on CANDIDATES in SOURCE.
When PROCESS-P is non-nil execute `filtered-candidate-transformer'
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
                        (lambda (cand_)
                          (if (consp cand_)
                              ;; override DISPLAY from candidate-transformer
                              (cons (funcall it (cdr cand_)) (cdr cand_))
                            (cons (funcall it cand_) cand_)))
                        candidates))
    candidates))

(defun helm-transform-candidates (candidates source &optional process-p)
  "Transform CANDIDATES of SOURCE according to candidate transformers.
When PROCESS-P is non-nil execute the `filtered-candidate-transformer' functions
otherwise only the `candidate-transformer' functions are processed.
When attribute `real-to-display' is present, execute its function on all maybe
filtered CANDIDATES."
  (helm-process-real-to-display
   (helm-process-filtered-candidate-transformer-maybe
    (helm-process-candidate-transformer
     (helm--initialize-one-by-one-candidates candidates source) source)
    source process-p)
   source))


;; Core: narrowing candidates
(defun helm-candidate-number-limit (source)
  "Apply candidate-number-limit attribute value.
This overhide variable `helm-candidate-number-limit'.

e.g:
If \(candidate-number-limit\) is in SOURCE, show all candidates in SOURCE.
If \(candidate-number-limit . 123\) is in SOURCE limit candidate to 123."
  (helm-aif (assq 'candidate-number-limit source)
      (or (cdr it) 99999999)
    (or helm-candidate-number-limit 99999999)))

(defun helm-candidate-get-display (candidate)
  "Get searched display part from CANDIDATE.
CANDIDATE is a string, a symbol, or \(DISPLAY . REAL\) cons cell."
  (format "%s" (or (car-safe candidate) candidate)))

(defun helm-process-pattern-transformer (pattern source)
  "Execute pattern-transformer attribute PATTERN function in SOURCE."
  (helm-aif (assoc-default 'pattern-transformer source)
      (helm-composed-funcall-with-source source it pattern)
    pattern))

(defun helm-default-match-function (candidate)
  "Check if `helm-pattern' match CANDIDATE.
Default function to match candidates according to `helm-pattern'."
  (string-match helm-pattern candidate))

(defun helm-match-functions (source)
  (let ((matchfns (or (assoc-default 'match source)
                      (assoc-default 'match-strict source)
                      #'helm-default-match-function)))
    (if (listp matchfns) matchfns (list matchfns))))

(defmacro helm--accumulate-candidates (candidate newmatches
                                       hash item-count limit source)
  "Add CAND into NEWMATCHES and use HASH to uniq NEWMATCHES.
Argument ITEM-COUNT count the matches.
if ITEM-COUNT reaches LIMIT, exit from inner loop."
  `(unless (gethash ,candidate ,hash)
     (unless (assq 'allow-dups ,source)
       (puthash ,candidate t ,hash))
     (helm--maybe-process-filter-one-by-one-candidate ,candidate source)
     (push ,candidate ,newmatches)
     (cl-incf ,item-count)
     (when (= ,item-count ,limit) (cl-return))))

(defun helm-take-first-elements (seq n)
  "Return the N first element of SEQ if SEQ is longer than N.
It is used to narrow down list of candidates to `helm-candidate-number-limit'."
  (if (> (length seq) n) (cl-subseq seq 0 n) seq))

(cl-defun helm-set-case-fold-search (&optional (pattern helm-pattern))
  "Used to set the value of `case-fold-search' in helm.
Return t or nil depending of value of `helm-case-fold-search'
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
                           ;; `helm-basename' is not available yet.
                           (file-name-nondirectory
                            (directory-file-name pattern))
                         pattern)))
    (helm-set-case-fold-search-1 bn-or-pattern)))

(defun helm-set-case-fold-search-1 (pattern)
  (cl-case helm-case-fold-search
    (smart (let ((case-fold-search nil))
             (if (string-match "[A-Z]" pattern) nil t)))
    (t helm-case-fold-search)))

(defun helm-match-from-candidates (cands matchfns limit source)
  (let (matches)
    (condition-case err
        (let ((item-count 0)
              (case-fold-search (helm-set-case-fold-search)))
          (clrhash helm-match-hash)
          (cl-dolist (match matchfns)
            (let (newmatches)
              (cl-dolist (candidate cands)
                (unless (gethash candidate helm-match-hash)
                  (when (funcall match
                                 (helm-candidate-get-display candidate))
                    (helm--accumulate-candidates
                     candidate newmatches
                     helm-match-hash item-count limit source))))
              ;; filter-one-by-one may return nil candidates, so delq them if some.
              (setq matches (nconc matches (nreverse (delq nil newmatches)))))))
      (error (unless (eq (car err) 'invalid-regexp) ; Always ignore regexps errors.
               (helm-log-error "helm-match-from-candidates in source `%s': %s %s"
                               (assoc-default 'name source) (car err) (cdr err)))
             (setq matches nil)))
    matches))

(defun helm-compute-matches (source)
  "Start computing candidates in SOURCE."
  (save-current-buffer
    (let ((matchfns (helm-match-functions source))
          (helm-source-name (assoc-default 'name source))
          (helm-current-source source)
          (limit (helm-candidate-number-limit source))
          (helm-pattern (helm-process-pattern-transformer
                         helm-pattern source)))
      ;; If source have a `filtered-candidate-transformer' attr
      ;; Filter candidates with this func, otherwise just compute
      ;; candidates.
      (helm-process-filtered-candidate-transformer
       (if (or (equal helm-pattern "")
               (equal matchfns '(identity)))
           ;; Compute all candidates up to LIMIT.
           (helm-take-first-elements
            (helm-get-cached-candidates source) limit)
         ;; Compute candidates according to pattern with their match fns.
         (helm-match-from-candidates
          (helm-get-cached-candidates source) matchfns limit source))
       source))))

(defun helm-render-source (source matches)
  "Display MATCHES from SOURCE according to its settings."
  (helm-log-eval (assoc-default 'name source))
  (when matches
    (helm-insert-header-from-source source)
    (if (not (assq 'multiline source))
        (mapc #'(lambda (m)
                  (helm-insert-match m 'insert source))
              matches)
      (let ((start (point)) separate)
        (cl-dolist (match matches)
          (if separate
              (helm-insert-candidate-separator)
            (setq separate t))
          (helm-insert-match match 'insert source))
        (put-text-property start (point) 'helm-multiline t)))))

(defmacro helm--maybe-use-while-no-input (&rest body)
  "Wrap BODY in `helm-while-no-input' unless initializing a remote connection."
  `(progn
     (if (and (file-remote-p helm-pattern)
              (not (file-remote-p helm-pattern nil t)))
         ;; Tramp will ask for passwd, don't use `helm-while-no-input'.
         ,@body
       (helm-log "Using here `helm-while-no-input'")
       (helm-while-no-input ,@body))))

(defun helm--compute-sources (src-list)
  (cl-loop with matches = (helm--maybe-use-while-no-input
                           (cl-loop for src in src-list
                                 collect (helm-compute-matches src)))
        when (eq matches t) do (setq matches nil)
        for src in src-list
        for mtc in matches
        do (helm-render-source src mtc)))

(cl-defun helm-process-delayed-sources (delayed-sources &optional preselect source)
  "Process helm DELAYED-SOURCES.
Move selection to string or regexp PRESELECT if non--nil.
This function is called in `helm-process-delayed-sources-timer'
when emacs is idle for `helm-idle-delay'."
  (with-helm-quittable
    (helm-log-eval (mapcar (lambda (s)
                             (assoc-default 'name s))
                           delayed-sources))
    (with-current-buffer (helm-buffer-get)
      (save-excursion
        (goto-char (point-max))
        (helm--compute-sources delayed-sources)
        (when (and (not (helm-empty-buffer-p))
                   ;; No selection yet.
                   (= (overlay-start helm-selection-overlay)
                      (overlay-end helm-selection-overlay)))
          (helm-update-move-first-line 'without-hook)))
      (save-excursion
        (goto-char (point-min))
        (helm-log-run-hook 'helm-update-hook))
      (setq helm-force-updating-p nil)
      (unless (assoc 'candidates-process source)
        (helm-display-mode-line (helm-get-current-source))
        (helm-log-run-hook 'helm-after-update-hook))
      (when preselect (helm-preselect preselect source)))))


;;; Core: helm-update
;;
(defun helm-update (&optional preselect source)
  "Update candidates list in `helm-buffer' according to `helm-pattern'.
Argument PRESELECT is a string or regexp used to move selection to a particular
place once updating is done.  It should be used on single source because search
is done on whole `helm-buffer' and not on current source."
  (helm-log "Start updating")
  (helm-kill-async-processes)
  ;; When persistent action have been called
  ;; we have two windows even with `helm-full-frame'.
  ;; So go back to one window when updating if `helm-full-frame'
  ;; is non--nil.
  (with-helm-window
    (when helm-onewindow-p (delete-other-windows)))
  (with-current-buffer (helm-buffer-get)
    (set (make-local-variable 'helm-input-local) helm-pattern)
    (let (normal-sources
          delayed-sources)
      (unwind-protect
           (progn
             ;; Iterate over all the sources
             (cl-loop for source in (cl-remove-if-not
                                     'helm-update-source-p (helm-get-sources))
                   if (helm-delayed-source-p source)
                   ;; Delayed sources just get collected for later
                   ;; processing
                   collect source into ds
                   else
                   ;; Collect the normal sources
                   collect source into ns
                   ;; Export the variables from cl-loop
                   finally (setq delayed-sources ds
                                 normal-sources ns))
             (erase-buffer)
             ;; Render all the sources into the helm buffer after
             ;; calculating all candidates.
             ;; Candidates must be computed AFTER erasing buffer
             ;; even if it cause flickering; Doing so avoid
             ;; unexpected results when executing actions.
             (helm--compute-sources normal-sources))
        (helm-log-eval
         (mapcar (lambda (s) (assoc-default 'name s)) delayed-sources))
        (cond ((and preselect delayed-sources normal-sources)
               ;; Preselection run here when there is
               ;; normal AND delayed sources.
               (helm-log "Update preselect candidate %s" preselect)
               (helm-preselect preselect source))
              (delayed-sources ; Preselection and hooks will run later.
               (helm-update-move-first-line 'without-hook))
              (t              ; No delayed sources, run the hooks now.
               (helm-update-move-first-line)
               (unless (assoc 'candidates-process source)
                 (helm-display-mode-line (helm-get-current-source))
                 (helm-log-run-hook 'helm-after-update-hook))
               (when preselect
                 (helm-log "Update preselect candidate %s" preselect)
                 (helm-preselect preselect source))
               (setq helm-force-updating-p nil)))
        (when delayed-sources
          ;; Allow giving a value to `delayed' attr from inside source.
          ;; Retain the biggest value (the slower) found in DELAYED-SOURCES.
          (let ((helm-idle-delay (cl-loop with delay = helm-idle-delay
                                       for s in delayed-sources
                                       for d = (assoc-default 'delayed s)
                                       when d do (setq delay (max delay d))
                                       finally return delay)))
            (run-with-idle-timer
             ;; Be sure helm-idle-delay is >
             ;; to helm-input-idle-delay
             ;; otherwise use value of helm-input-idle-delay
             ;; or 0.01 if == to 0.
             (max helm-idle-delay helm-input-idle-delay 0.001) nil
             'helm-process-delayed-sources delayed-sources preselect source)))
        (helm-log "end update")))))

;; Update keymap after updating.
;; Putting this in a hook allow users to disable it.
(add-hook 'helm-after-update-hook 'helm--maybe-update-keymap)

(defun helm-update-source-p (source)
  "Whether SOURCE need updating or not."
  (let ((len (string-width
              (if (or (not (assoc 'no-matchplugin source))
                      helm-match-plugin-mode)
                  ;; Don't count spaces entered when using
                  ;; match-plugin.
                  (replace-regexp-in-string " " "" helm-pattern)
                helm-pattern))))
    (and (or (not helm-source-filter)
             (member (assoc-default 'name source) helm-source-filter))
         (>= len
             (helm-aif (assoc 'requires-pattern source) (or (cdr it) 1) 0))
         ;; These incomplete regexps hang helm forever
         ;; so defer update. Maybe replace spaces quoted when using
         ;; match-plugin-mode.
         (not (member (replace-regexp-in-string "\\s\\ " " " helm-pattern)
                      helm-update-blacklist-regexps)))))

(defun helm-delayed-source-p (source)
  "Wheter SOURCE is a delayed source or not."
  (or (assoc 'delayed source)
      (and helm-quick-update
           (< (window-height (get-buffer-window (current-buffer)))
              (line-number-at-pos (point-max))))))

(defun helm-update-move-first-line (&optional without-hook)
  "Goto first line of `helm-buffer'."
  (goto-char (point-min))
  (unless without-hook
    (save-excursion (helm-log-run-hook 'helm-update-hook)))
  (helm-next-line))

;;;###autoload
(defun helm-force-update (&optional preselect)
  "Force recalculation and update of candidates.
The difference with `helm-update' is this function is reevaling
the `init' and `update' attributes functions when present
before updating candidates according to pattern i.e calling `helm-update'.
Selection is preserved to current candidate or moved to PRESELECT
if specified."
  (interactive)
  (let ((source    (helm-get-current-source))
        (selection (helm-get-selection nil t))
        ;; `helm-goto-source' need to have all sources displayed
        ;; So disable `helm-quick-update'.
        helm-quick-update)
    (setq helm-force-updating-p t)
    (when source
      (mapc 'helm-force-update--reinit
            (helm-get-sources)))
    (helm-update (or preselect selection) source)
    (with-helm-window (recenter))))

(defun helm-force-update--reinit (source)
  "Reinit SOURCE by calling his update and/or init functions."
  (helm-aif (helm-funcall-with-source
             source 'helm-candidate-buffer)
      (kill-buffer it))
  (cl-dolist (attr '(update init))
    (helm-aif (assoc-default attr source)
        (helm-funcall-with-source source it)))
  (helm-remove-candidate-cache source))

(defun helm-remove-candidate-cache (source)
  "Remove SOURCE from `helm-candidate-cache'."
  (remhash (assoc-default 'name source) helm-candidate-cache))

(defun helm-insert-match (match insert-function source)
  "Insert MATCH into `helm-buffer' with INSERT-FUNCTION for SOURCE.
If MATCH is a list then insert the string intended to appear on the display
and store the real value in a text property."
  (let ((start     (point-at-bol (point)))
        (dispvalue (or (car-safe match) match))
        (realvalue (cdr-safe match)))
    (setq dispvalue
          (cond ((symbolp dispvalue) (symbol-name dispvalue))
                ((numberp dispvalue) (number-to-string dispvalue))
                ((string= "" dispvalue))
                (t dispvalue)))
    (when (stringp dispvalue)
      (funcall insert-function dispvalue)
      ;; Some sources with candidates-in-buffer have already added
      ;; 'helm-realvalue property when creating candidate buffer.
      (unless (get-text-property start 'helm-realvalue)
        (and realvalue
             (put-text-property start (point-at-eol)
                                'helm-realvalue realvalue)))
      (when helm-source-in-each-line-flag
        (put-text-property start (point-at-eol) 'helm-source source))
      (funcall insert-function "\n"))))

(defun helm-insert-header-from-source (source)
  "Insert SOURCE name in `helm-buffer' header.
Maybe insert by overlay additional info after source name if SOURCE have
header-name attribute."
  (let ((name (assoc-default 'name source)))
    (helm-insert-header
     name
     (helm-aif (assoc-default 'header-name source)
         (helm-funcall-with-source source it name)))))

(defun helm-insert-header (name &optional display-string)
  "Insert header of source NAME into the helm buffer.
If DISPLAY-STRING is non--nil and a string, display this additional info
after the source name by overlay."
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


;;; Core: async process
;;
(defun helm-output-filter (process output-string)
  "The `process-filter' function for helm async sources."
  (helm-output-filter-1 (assoc process helm-async-processes) output-string))

(defun helm-output-filter-1 (process-assoc output-string)
  (helm-log-eval output-string)
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
                          (assoc 'incomplete-line source)
                          source)
                         source t))
    (when candidate ; filter-one-by-one may return nil candidates.
      (if (assq 'multiline source)
          (let ((start (point)))
            (helm-insert-candidate-separator)
            (helm-insert-match candidate 'insert-before-markers source)
            (put-text-property start (point) 'helm-multiline t))
        (helm-insert-match candidate 'insert-before-markers source))
      (cl-incf (cdr (assoc 'item-count source)))
      (when (>= (assoc-default 'item-count source) limit)
        (helm-kill-async-process process)
        (cl-return)))))

(defun helm-output-filter--collect-candidates (lines incomplete-line-info source)
  "Collect LINES maybe completing the truncated first and last lines."
  ;; The output of process may come in chunks of any size,
  ;; so the last line of LINES come truncated, this truncated line is
  ;; stored in INCOMPLETE-LINE-INFO and will be concated with the first
  ;; incomplete line of next chunk arriving.
  ;; INCOMPLETE-LINE-INFO is an attribute of source which is created
  ;; with an empty string when the source is computed => (incomplete-line . "")
  (helm-log-eval (cdr incomplete-line-info))
  (butlast
   (cl-loop for line in lines
         ;; On start `incomplete-line-info' value is empty string.
         for newline = (helm-aif (cdr incomplete-line-info)
                           (prog1
                               (concat it line)
                             (setcdr incomplete-line-info nil))
                         line)
         do (helm--maybe-process-filter-one-by-one-candidate newline source)
         collect newline
         ;; Store last incomplete line (last chunk truncated)
         ;; until new output arrives.
         finally do (setcdr incomplete-line-info line))))

(defun helm-output-filter--post-process ()
  (let ((src (helm-get-current-source)))
    (helm-log-run-hook 'helm-update-hook)
    (helm-aif (get-buffer-window helm-buffer 'visible)
        (with-selected-window it
          (helm-skip-noncandidate-line 'next)
          (helm-mark-current-line)
          (helm-display-mode-line src)
          (helm-log-run-hook 'helm-after-update-hook)))))

(defun helm-process-deferred-sentinel-hook (process event file)
  "Defer remote processes in sentinels.
Meant to be called at beginning of a sentinel process function."
  (when (and (string= event "finished\n")
             (or (file-remote-p file)
                 ;; `helm-suspend-update-flag'
                 ;; is non--nil here only during a
                 ;; running process, this will never be called
                 ;; when user set it explicitely with `C-!'.
                 helm-suspend-update-flag))
    (setq helm-suspend-update-flag t)
    ;; Kill the process but don't delete entry in
    ;; `helm-async-processes'.
    (helm-kill-async-process process)
    ;; When tramp tries to open the same connection twice in a
    ;; short time frame (less than 5s) it throw 'suppress which
    ;; call the real-handler on the main "Emacs", so we wait
    ;; 5s before updating to avoid this [1], but allowing user to
    ;; enter input during this delay.
    ;; [1] On last Emacs versions, this is fixed and tramp return
    ;; nil in this situation.
    ;; Note: It is difficult to have a value < to 5 for
    ;; `tramp-connection-min-time-diff', because the process die
    ;; when calling too quickly same process.
    (run-at-time (or (and (boundp 'tramp-connection-min-time-diff)
                          tramp-connection-min-time-diff)
                     5)
                 nil #'(lambda ()
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


;;; Core: action
;;
(defun helm-execute-selection-action ()
  "Execute current action and kill the action buffer if present."
  (helm-log-run-hook 'helm-before-action-hook)
  ;; Position can be different when `helm-current-buffer'
  ;; is splitted, so jump to this position before executing action.
  (helm-current-position 'restore)
  (unwind-protect
       (helm-execute-selection-action-1)
    (helm-aif (get-buffer helm-action-buffer)
        (kill-buffer it))
    (helm-log-run-hook 'helm-after-action-hook)))

(defun helm-execute-selection-action-1 (&optional
                                          selection action
                                          preserve-saved-action)
  "Execute ACTION on current SELECTION.
If PRESERVE-SAVED-ACTION is non--nil save action."
  (helm-log "executing action")
  (setq action (helm-get-default-action
                (or action
                    helm-saved-action
                    (if (get-buffer helm-action-buffer)
                        (helm-get-selection helm-action-buffer)
                      (helm-get-action)))))
  (let ((source (or helm-saved-current-source
                    (helm-get-current-source)))
        non-essential)
    (setq selection (or selection
                        (helm-get-selection)
                        (and (assoc 'accept-empty source) "")))
    (unless preserve-saved-action (setq helm-saved-action nil))
    (when (and selection action)
      (helm-funcall-with-source
       source action
       (helm-coerce-selection selection source)))))

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

;;;###autoload
(defun helm-select-action ()
  "Select an action for the currently selected candidate.
If action buffer is selected, back to the helm buffer."
  (interactive)
  (helm-log-run-hook 'helm-select-action-hook)
  (setq helm-saved-selection (helm-get-selection))
  (with-selected-frame (with-helm-window (selected-frame))
    (cond ((get-buffer-window helm-action-buffer 'visible)
           (set-window-buffer (get-buffer-window helm-action-buffer)
                              helm-buffer)
           (kill-buffer helm-action-buffer)
           (helm-set-pattern helm-input 'noupdate))
          (helm-saved-selection
           (setq helm-saved-current-source (helm-get-current-source))
           (let ((actions (helm-get-action)))
             (if (functionp actions)
                 (message "Sole action: %s" actions)
               (helm-show-action-buffer actions)
               (helm-delete-minibuffer-contents)
               ;; Make `helm-pattern' differs from the previous value.
               (setq helm-pattern 'dummy)
               (helm-check-minibuffer-input))))
          (t (message "No Actions available")))))

(defun helm-show-action-buffer (actions)
  (with-current-buffer (get-buffer-create helm-action-buffer)
    (erase-buffer)
    (buffer-disable-undo)
    (set-window-buffer (get-buffer-window helm-buffer) helm-action-buffer)
    (set (make-local-variable 'helm-sources)
         `(((name . "Actions")
            (volatile)
            (nomark)
            (candidates . ,actions)
            (candidate-transformer
             . (lambda (candidates)
                 (cl-loop for (i . j) in candidates
                       collect
                       (cons (propertize i 'face 'helm-action) j))))
            (candidate-number-limit))))
    (set (make-local-variable 'helm-source-filter) nil)
    (set (make-local-variable 'helm-selection-overlay) nil)
    (helm-initialize-overlays helm-action-buffer)))


;; Core: selection

(defun helm-display-source-at-screen-top-maybe (unit)
  "Display source at top of screen when UNIT value is 'source.
With any other value of UNIT return nil."
  (when (and helm-display-source-at-screen-top (eq unit 'source))
    (set-window-start (selected-window)
                      (save-excursion (forward-line -1) (point)))))

(defun helm-skip-noncandidate-line (direction)
  "Skip source header or candidates separator when going in DIRECTION.
Possible value of DIRECTION are 'next or 'previous.
Same as `helm-skip-header-and-separator-line' but ensure
point is moved to the right place when at bop or eob."
  (helm-skip-header-and-separator-line direction)
  (and (bobp) (forward-line 1))     ; skip first header.
  (and (eobp) (forward-line -1)))   ; avoid last empty line.


(defun helm-skip-header-and-separator-line (direction)
  "Skip source header or candidate separator when going to next/previous line.
Possible value of DIRECTION are 'next or 'previous."
  (while (and (not (bobp))
              (or (helm-pos-header-line-p)
                  (helm-pos-candidate-separator-p)))
    (forward-line (if (and (eq direction 'previous)
                           (not (eq (point-at-bol) (point-min))))
                      -1 1))))

(defun helm-display-mode-line (source &optional force)
  "Setup mode-line and header-line for `helm-buffer'."
  (set (make-local-variable 'helm-mode-line-string)
       (helm-interpret-value (or (and (listp source) ; Check if source is empty.
                                      (assoc-default 'mode-line source))
                                 (default-value 'helm-mode-line-string))
                             source))
  (let ((follow (and (eq (cdr (assq 'follow source)) 1) "(HF) ")))
    ;; Setup mode-line.
    (if helm-mode-line-string
        (setq mode-line-format
              `(" " mode-line-buffer-identification " "
                    (line-number-mode "L%l") " " ,follow
                    (:eval (when ,helm--mode-line-display-prefarg
                             (let ((arg (prefix-numeric-value (or prefix-arg
                                                                  current-prefix-arg))))
                               (unless (= arg 1)
                                 (propertize (format "[prefarg:%s] " arg)
                                             'face 'helm-prefarg)))))
                    (:eval (helm-show-candidate-number
                            (car-safe helm-mode-line-string)))
                    " " helm-mode-line-string-real " -%-")
              helm-mode-line-string-real
              (substitute-command-keys (if (listp helm-mode-line-string)
                                           (cadr helm-mode-line-string)
                                         helm-mode-line-string)))
      (setq mode-line-format (default-value 'mode-line-format)))
    ;; Setup header-line.
    (let* ((hlstr (helm-interpret-value
                   (and (listp source)
                        (assoc-default 'header-line source))
                   source))
           (hlend (make-string (max 0 (- (window-width) (length hlstr))) ? )))
      (setq header-line-format
            (propertize (concat " " hlstr hlend) 'face 'helm-header))))
  (when force (force-mode-line-update)))

(defun helm-show-candidate-number (&optional name)
  "Used to display candidate number in mode-line.
You can specify NAME of candidates e.g \"Buffers\" otherwise
it is \"Candidate\(s\)\" by default."
  (when helm-alive-p
    (unless (helm-empty-source-p)
      (propertize
       (format "[%s %s]"
               (helm-get-candidate-number 'in-current-source)
               (or name "Candidate(s)"))
       'face 'helm-candidate-number))))

(cl-defun helm-move-selection-common (&key where direction)
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
        (helm-log-run-hook 'helm-move-selection-before-hook)
        (funcall move-func)
        (helm-skip-noncandidate-line direction)
        (helm-display-source-at-screen-top-maybe where)
        (when (helm-get-previous-header-pos)
          (helm-mark-current-line))
        (helm-display-mode-line (helm-get-current-source))
        (helm-log-run-hook 'helm-move-selection-after-hook)))))

(defun helm-move--previous-multi-line-fn ()
  (forward-line -1)
  (unless (helm-pos-header-line-p)
    (helm-skip-header-and-separator-line 'previous)
    (let ((header-pos (helm-get-previous-header-pos))
          (separator-pos (helm-get-previous-candidate-separator-pos)))
      (when header-pos
        (goto-char (if (or (null separator-pos)
                           (< separator-pos header-pos))
                       header-pos
                     separator-pos))
        (forward-line 1)))))

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
  (goto-char (or (helm-get-next-header-pos) (point-max)))
  (when (helm-pos-header-line-p) (forward-line -2)))

(defun helm-move--beginning-of-source ()
  (goto-char (helm-get-previous-header-pos))
  (forward-line 1))

(defun helm-move--previous-source-fn ()
  (forward-line -1)
  (if (bobp)
      (goto-char (point-max))
    (helm-skip-header-and-separator-line 'previous))
  (goto-char (helm-get-previous-header-pos))
  (forward-line 1))

(defun helm-move--next-source-fn ()
  (goto-char (or (helm-get-next-header-pos) (point-min))))

(defun helm-move--goto-source-fn (source-or-name)
  (goto-char (point-min))
  (let ((name (if (stringp source-or-name) source-or-name
                (assoc-default 'name source-or-name))))
    (condition-case err
        (while (not (string= name (helm-current-line-contents)))
          (goto-char (helm-get-next-header-pos)))
      (error (helm-log "%S" err)))))

;;;###autoload
(defun helm-previous-line (&optional arg)
  "Move selection to the previous line."
  (interactive "p")
  ;; Be sure to not use this in non--interactives calls.
  (let ((helm-move-to-line-cycle-in-source
         (and helm-move-to-line-cycle-in-source arg)))
    (helm-move-selection-common :where 'line :direction 'previous)))

;;;###autoload
(defun helm-next-line (&optional arg)
  "Move selection to the next line."
  (interactive "p")
  ;; Be sure to not use this in non--interactives calls.
  (let ((helm-move-to-line-cycle-in-source
         (and helm-move-to-line-cycle-in-source arg)))
    (helm-move-selection-common :where 'line :direction 'next)))

;;;###autoload
(defun helm-previous-page ()
  "Move selection back with a pageful."
  (interactive)
  (helm-move-selection-common :where 'page :direction 'previous))

;;;###autoload
(defun helm-next-page ()
  "Move selection forward with a pageful."
  (interactive)
  (helm-move-selection-common :where 'page :direction 'next))

;;;###autoload
(defun helm-beginning-of-buffer ()
  "Move selection at the top."
  (interactive)
  (helm-move-selection-common :where 'edge :direction 'previous))

;;;###autoload
(defun helm-end-of-buffer ()
  "Move selection at the bottom."
  (interactive)
  (helm-move-selection-common :where 'edge :direction 'next))

;;;###autoload
(defun helm-previous-source ()
  "Move selection to the previous source."
  (interactive)
  (helm-move-selection-common :where 'source :direction 'previous))

;;;###autoload
(defun helm-next-source ()
  "Move selection to the next source."
  (interactive)
  (helm-move-selection-common :where 'source :direction 'next))

(defun helm-goto-source (source-or-name)
  "Move the selection to the source SOURCE-OR-NAME."
  (helm-move-selection-common :where 'source :direction source-or-name))

(defun helm--follow-action (arg)
  (let ((helm--temp-follow-flag t))
    (when (or (eq last-command 'helm-follow-action-forward)
              (eq last-command 'helm-follow-action-backward))
      (if (> arg 0)
          (helm-next-line)
        (helm-previous-line)))
    (helm-execute-persistent-action)))

(defun helm-follow-action-forward ()
  "Go to next line and execute persistent action."
  (interactive)
  (helm--follow-action 1))

(defun helm-follow-action-backward ()
  "Go to previous line and execute persistent action."
  (interactive)
  (helm--follow-action -1))

(defun helm-mark-current-line (&optional resumep)
  "Move `helm-selection-overlay' to current line.
Note that this is not related with visibles marks, which are used
to mark candidates."
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
    (setq helm-selection-point (overlay-start helm-selection-overlay)))
  (helm-follow-execute-persistent-action-maybe))

;;;###autoload
(defun helm-confirm-and-exit-minibuffer ()
  "Maybe ask for confirmation when exiting helm.
It is similar to `minibuffer-complete-and-exit' adapted to helm.
If `minibuffer-completion-confirm' value is 'confirm,
send in minibuffer confirm message and exit on next hit.
If `minibuffer-completion-confirm' value is t,
don't exit and send message 'no match'."
  (interactive)
  (let* ((empty-buffer-p (with-current-buffer helm-buffer
                           (eq (point-min) (point-max))))
         (unknown (and (not empty-buffer-p)
                       (string= (get-text-property
                                 0 'display (helm-get-selection nil 'withprop))
                                "[?]"))))
    (cond ((and (or empty-buffer-p unknown)
                (eq minibuffer-completion-confirm 'confirm))
           (setq helm-minibuffer-confirm-state
                 'confirm)
           (setq minibuffer-completion-confirm nil)
           (minibuffer-message " [confirm]"))
          ((and (or empty-buffer-p unknown)
                (eq minibuffer-completion-confirm t))
           (minibuffer-message " [No match]"))
          (t
           (setq helm-minibuffer-confirm-state nil)
           (helm-exit-minibuffer)))))
(add-hook 'helm-after-update-hook 'helm-confirm-and-exit-hook)

(defun helm-confirm-and-exit-hook ()
  "Restore `minibuffer-completion-confirm' when helm update."
  (unless (or (eq minibuffer-completion-confirm t)
              (not helm-minibuffer-confirm-state))
    (setq minibuffer-completion-confirm
          helm-minibuffer-confirm-state)))

;;;###autoload
(defun helm-exit-minibuffer ()
  "Select the current candidate by exiting the minibuffer."
  (interactive)
  (unless helm-current-prefix-arg
    (setq helm-current-prefix-arg current-prefix-arg))
  (setq helm-exit-status 0)
  (helm-log-run-hook 'helm-exit-minibuffer-hook)
  (exit-minibuffer))

;;;###autoload
(defun helm-keyboard-quit ()
  "Quit minibuffer in helm.
If action buffer is displayed, kill it."
  (interactive)
  (when (get-buffer-window helm-action-buffer 'visible)
    (kill-buffer helm-action-buffer))
  (setq helm-exit-status 1)
  (abort-recursive-edit))

(defun helm-get-next-header-pos ()
  "Return the position of the next header from point."
  (next-single-property-change (point) 'helm-header))

(defun helm-get-previous-header-pos ()
  "Return the position of the previous header from point."
  (previous-single-property-change (point) 'helm-header))

(defun helm-pos-multiline-p ()
  "Return non-nil if the current position is in the multiline source region."
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
;;;###autoload
(defun helm-debug-output ()
  "Show all helm-related variables at this time."
  (interactive)
  (helm-help-internal " *Helm Debug*" 'helm-debug-output-function))

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


;; Core: misc
(defun helm-kill-buffer-hook ()
  "Remove tick entry from `helm-tick-hash' when killing a buffer."
  (cl-loop for key being the hash-keys in helm-tick-hash
        if (string-match (format "^%s/" (regexp-quote (buffer-name))) key)
        do (remhash key helm-tick-hash)))
(add-hook 'kill-buffer-hook 'helm-kill-buffer-hook)

(defun helm-preselect (candidate-or-regexp &optional source)
  "Move `helm-selection-overlay' to CANDIDATE-OR-REGEXP on startup."
  (with-helm-window
    (when candidate-or-regexp
      (if helm-force-updating-p
          (and source (helm-goto-source source))
        (goto-char (point-min))
        (forward-line 1))
      (let ((start (point)))
        (or (re-search-forward candidate-or-regexp nil t)
            (goto-char start))))
    (forward-line 0) ; Avoid scrolling right on long lines.
    (helm-mark-current-line)))

;;;###autoload
(defun helm-delete-current-selection ()
  "Delete the currently selected item."
  (interactive)
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

(defun helm-end-of-source-p (&optional at-point)
  "Return non--nil if we are at eob or end of source."
  (save-excursion
    (if (and (helm-pos-multiline-p) (null at-point))
        (null (helm-get-next-candidate-separator-pos))
      (forward-line (if at-point 0 1))
      (or (eq (point-at-bol) (point-at-eol))
          (helm-pos-header-line-p)
          (eobp)))))

(defun helm-edit-current-selection-internal (func)
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
           (put-text-property (point) (point-at-eol)
                              'helm-multiline multiline))
      (helm-mark-current-line))))

(defmacro helm-edit-current-selection (&rest forms)
  "Evaluate FORMS at current selection in the helm buffer.
Used generally to modify current selection."
  (declare (indent 0) (debug t))
  `(helm-edit-current-selection-internal
    (lambda () ,@forms)))

(defun helm-set-pattern (pattern &optional noupdate)
  "Set minibuffer contents to PATTERN.
if optional NOUPDATE is non-nil, helm buffer is not changed."
  (with-selected-window (or (active-minibuffer-window) (minibuffer-window))
    (delete-minibuffer-contents)
    (insert pattern))
  (when noupdate
    (setq helm-pattern pattern)))

(defun helm-minibuffer-completion-contents ()
  "Return the user input in a minibuffer before point as a string.
That is what completion commands operate on."
  (buffer-substring (field-beginning) (point)))

;;;###autoload
(defun helm-delete-minibuffer-contents (&optional arg)
  "Delete minibuffer contents.
When called with a prefix arg or when
`helm-delete-minibuffer-contents-from-point' is non--nil,
delete minibuffer contents from point instead of deleting all."
  (interactive "P")
  (require 'helm-utils)
  (let* ((input (minibuffer-contents))
         (str (if (or arg helm-delete-minibuffer-contents-from-point)
                  (helm-minibuffer-completion-contents) "")))
    (helm-reset-yank-point)
    (if (> (length input) 0)
        ;; minibuffer is not empty, delete contents and update.
        (helm-set-pattern str)
      ;; minibuffer is already empty, force update.
      (helm-force-update))))


;;; Plugins
;;
;; Built-in plug-in: type
(defun helm-compile-source--type (source)
  (helm-aif (assoc-default 'type source)
      (append source (assoc-default it helm-type-attributes) nil)
    source))

;; `define-helm-type-attribute' is public API.

(defun helm-add-type-attribute (type definition)
  (helm-aif (assq type helm-type-attributes)
      (setq helm-type-attributes (delete it helm-type-attributes)))
  (push (cons type definition) helm-type-attributes))

(defun helm-document-type-attribute (type doc)
  (add-to-list 'helm-types type t)
  (put type 'helm-typeattrdoc
       (concat "- " (symbol-name type) "\n\n" doc "\n")))

;; Built-in plug-in: dummy
(defun helm-dummy-candidate (_candidate _source)
  "Use `helm-pattern' as CANDIDATE in SOURCE."
  ;; `source' is defined in filtered-candidate-transformer
  (list helm-pattern))

(defun helm-compile-source--dummy (source)
  (if (assoc 'dummy source)
      (progn
        (unless (helm-attr-defined
                 'filtered-candidate-transformer source)
          (helm-attrset 'filtered-candidate-transformer
                        'helm-dummy-candidate source))
        (append source
                '((candidates "dummy")
                  (accept-empty)
                  (match identity)
                  (volatile))))
    source))

;; Built-in plug-in: candidates-in-buffer
(defun helm-candidates-in-buffer (source)
  "Get candidates from the candidates buffer according to `helm-pattern'.

BUFFER is `helm-candidate-buffer' by default.  Each
candidate must be placed in one line.  This function is meant to
be used in candidates-in-buffer or candidates attribute of an
helm source.  Especially fast for many (1000+) candidates.

eg.
 '((name . \"many files\")
   (init . (lambda () (with-current-buffer (helm-candidate-buffer 'local)
                        (insert-many-filenames))))
   (search re-search-forward)  ; optional
   (candidates-in-buffer)
   (type . file))

+===============================================================+
| The new way of making and narrowing candidates: Using buffers |
+===============================================================+

By default, `helm' makes candidates by evaluating the
candidates function, then narrows them by `string-match' for each
candidate.

But this way is very slow for many candidates. The new way is
storing all candidates in a buffer and narrowing them by
`re-search-forward'. Search function is customizable by search
attribute. The important point is that buffer processing is MUCH
FASTER than string list processing and is the Emacs way.

The init function writes all candidates to a newly-created
candidate buffer.  The candidates buffer is created or specified
by `helm-candidate-buffer'.  Candidates are stored in a line.

The candidates function narrows all candidates, IOW creates a
subset of candidates dynamically. It is the task of
`helm-candidates-in-buffer'.  As long as
`helm-candidate-buffer' is used,`(candidates-in-buffer)' is
sufficient in most cases.

Note that `(candidates-in-buffer)' is shortcut of three attributes:
  (candidates . helm-candidates-in-buffer)
  (volatile)
  (match identity)
And `(candidates-in-buffer . func)' is shortcut of three attributes:
  (candidates . func)
  (volatile)
  (match identity)
The expansion is performed in `helm-get-sources'.

The candidates-in-buffer attribute implies the volatile attribute.
The volatile attribute is needed because `helm-candidates-in-buffer'
creates candidates dynamically and need to be called everytime
`helm-pattern' changes.

Because `helm-candidates-in-buffer' plays the role of `match' attribute
function, specifying `(match identity)' makes the source slightly faster.

However if source contain match-part attribute, match is computed only
on part of candidate returned by the call of function provided by this attribute.
The function should have one arg, candidate, and return only
a specific part of candidate.

To customize `helm-candidates-in-buffer' behavior, use `search',
`get-line', `match-part' and `search-from-end' attributes."

  (helm-candidates-in-buffer-1
   (helm-candidate-buffer)
   helm-pattern
   (or (assoc-default 'get-line source)
       #'buffer-substring-no-properties)
   ;; use external variable `source'.
   (or (assoc-default 'search source)
       (if (assoc 'search-from-end source)
           '(helm-candidates-in-buffer-search-from-end)
         '(helm-candidates-in-buffer-search-from-start)))
   (helm-candidate-number-limit source)
   (assoc 'search-from-end source)
   (helm-attr 'match-part)
   source))

(defun helm-candidates-in-buffer-search-from-start (pattern)
  "Search PATTERN with `re-search-forward' with bound and noerror args."
  (re-search-forward pattern nil t))

(defun helm-candidates-in-buffer-search-from-end (pattern)
  "Search PATTERN with `re-search-backward' with bound and noerror args."
  (re-search-backward pattern nil t))

(defun helm-candidates-in-buffer-1 (buffer pattern get-line-fn
                                    search-fns limit search-from-end
                                    match-part-fn source)
  "Return the list of candidates inserted in BUFFER matching PATTERN."
  ;; buffer == nil when candidates buffer does not exist.
  (when buffer
    (with-current-buffer buffer
      (let ((start-point (if search-from-end
                             (1+ (point-max)) (1- (point-min))))
            (endp (if search-from-end #'bobp #'eobp))
            (inhibit-point-motion-hooks t))
        (goto-char start-point)
        (if (string= pattern "")
            (helm-initial-candidates-from-candidate-buffer
             endp get-line-fn limit search-from-end)
          (helm-search-from-candidate-buffer
           pattern get-line-fn search-fns limit search-from-end
           start-point match-part-fn source))))))

(defun helm-point-is-moved (proc)
  "If point is moved after executing PROC, return t, otherwise nil."
  (/= (point) (save-excursion (funcall proc) (point))))

(defun helm-search-from-candidate-buffer (pattern get-line-fn search-fns
                                          limit search-from-end
                                          start-point match-part-fn source)
  (let (buffer-read-only
        matches 
        newmatches
        (case-fold-search (helm-set-case-fold-search)))
    (helm-search-from-candidate-buffer-internal
     (lambda ()
       (clrhash helm-cib-hash)
       (cl-dolist (searcher search-fns)
         (goto-char start-point)
         (setq newmatches nil)
         (cl-loop with item-count = 0
               while (funcall searcher pattern)
               for cand = (funcall get-line-fn (point-at-bol) (point-at-eol))
               when (and (not (gethash cand helm-cib-hash))
                         (or
                          ;; Always collect when cand is matched by searcher funcs
                          ;; and match-part attr is not present.
                          (not match-part-fn)
                          ;; If match-part attr is present, collect only if PATTERN
                          ;; match the part of CAND specified by the match-part func.
                          (helm-search-match-part cand pattern match-part-fn)))
               do (helm--accumulate-candidates
                   cand newmatches helm-cib-hash item-count limit source)
               unless (helm-point-is-moved
                       (lambda ()
                         (if search-from-end
                             (goto-char (1- (point-at-bol)))
                           (forward-line 1))))
               return nil)
         (setq matches (append matches (nreverse newmatches))))
       (delq nil matches)))))

(defun helm-search-match-part (candidate pattern match-part-fn)
  "Match PATTERN only on part of CANDIDATE returned by MATCH-PART-FN."
  (let ((part (funcall match-part-fn candidate)))
    (if (string-match " " pattern)
        (cl-loop for i in (split-string pattern " " t)
              always (string-match i part))
      (string-match pattern part))))

(defun helm-initial-candidates-from-candidate-buffer (endp
                                                      get-line-fn
                                                      limit search-from-end)
  (delq nil (cl-loop with next-line-fn =
                  (if search-from-end
                      (lambda (_x) (goto-char (max (1- (point-at-bol)) 1)))
                    #'forward-line)
                  until (funcall endp)
                  for i from 1 to limit
                  collect (funcall get-line-fn (point-at-bol) (point-at-eol))
                  do (funcall next-line-fn 1))))

(defun helm-search-from-candidate-buffer-internal (search-fn)
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

(defun helm-candidate-buffer (&optional create-or-buffer)
  "Register and return a buffer containing candidates of current source.
`helm-candidate-buffer' searches buffer-local candidates buffer first,
then global candidates buffer.

Acceptable values of CREATE-OR-BUFFER:

- nil (omit)
  Only return the candidates buffer.
- a buffer
  Register a buffer as a candidates buffer.
- 'global
  Create a new global candidates buffer,
  named \" *helm candidates:SOURCE*\".
- other non-nil value
  Create a new local candidates buffer,
  named \" *helm candidates:SOURCE*HELM-CURRENT-BUFFER\"."
  (let* ((global-bname (format " *helm candidates:%s*"
                               helm-source-name))
         (local-bname (format " *helm candidates:%s*%s"
                              helm-source-name
                              (buffer-name helm-current-buffer)))
         (register-func #'(lambda ()
                            (setq helm-candidate-buffer-alist
                                  (cons (cons helm-source-name create-or-buffer)
                                        (delete (assoc helm-source-name
                                                       helm-candidate-buffer-alist)
                                                helm-candidate-buffer-alist)))))
         (kill-buffers-func #'(lambda ()
                                (cl-loop for b in (buffer-list)
                                      if (string-match (format "^%s" (regexp-quote global-bname))
                                                       (buffer-name b))
                                      do (kill-buffer b))))
         (create-func #'(lambda ()
                          (with-current-buffer
                              (get-buffer-create (if (eq create-or-buffer 'global)
                                                     global-bname
                                                   local-bname))
                            (buffer-disable-undo)
                            (erase-buffer)
                            (font-lock-mode -1))))
         (return-func #'(lambda ()
                          (or (get-buffer local-bname)
                              (get-buffer global-bname)
                              (helm-aif (assoc-default helm-source-name
                                                       helm-candidate-buffer-alist)
                                  (and (buffer-live-p it) it))))))
    (when create-or-buffer
      (funcall register-func)
      (unless (bufferp create-or-buffer)
        (and (eq create-or-buffer 'global) (funcall kill-buffers-func))
        (funcall create-func)))
    (funcall return-func)))

(defun helm-init-candidates-in-buffer (buffer data)
  "Register BUFFER with DATA for a helm candidates-in-buffer session.
Arg BUFFER can be a string, a buffer object (bufferp), or a symbol,
either 'local or 'global which is passed to `helm-candidate-buffer'.
Arg DATA can be either a list or a plain string."
  (declare (indent 1))
  (let ((buf (helm-candidate-buffer
              (if (or (stringp buffer)
                      (bufferp buffer))
                  (get-buffer-create buffer)
                buffer)))) ; a symbol.
    (with-current-buffer buf
      (erase-buffer)
      (if (listp data)
          (cl-loop for i in data do (insert (concat i "\n")))
        (and (stringp data) (insert data)))))
  buffer)

(defun helm-compile-source--candidates-in-buffer (source)
  (helm-aif (assoc 'candidates-in-buffer source)
      (append source
              `((candidates . ,(or (cdr it)
                                   (lambda ()
                                     (helm-candidates-in-buffer source))))
                (volatile) (match identity)))
    source))


;;; Resplit helm window
;;
;;;###autoload
(defun helm-toggle-resplit-window ()
  "Toggle resplit helm window, vertically or horizontally."
  (interactive)
  (when helm-prevent-escaping-from-minibuffer
    (helm-prevent-switching-other-window :enabled nil))
  (unwind-protect
       (with-helm-window
         (if (or helm-full-frame (one-window-p t))
             (message "Error: Attempt to resplit a single window")
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
              helm-buffer)))
         (setq helm--window-side-state (helm--get-window-side-state)))
    (when helm-prevent-escaping-from-minibuffer
      (helm-prevent-switching-other-window :enabled nil))))

;; Utility: Resize helm window.
(defun helm-enlarge-window-1 (n)
  "Enlarge or narrow helm window.
If N is positive enlarge, if negative narrow."
  (unless helm-full-frame
    (let ((horizontal-p (eq helm-split-window-state 'horizontal)))
      (with-helm-window
        (enlarge-window n horizontal-p)))))

;;;###autoload
(defun helm-narrow-window ()
  "Narrow helm window."
  (interactive)
  (helm-enlarge-window-1 -1))

;;;###autoload
(defun helm-enlarge-window ()
  "Enlarge helm window."
  (interactive)
  (helm-enlarge-window-1 1))

;;;###autoload
(defun helm-swap-windows ()
  "Swap window holding `helm-buffer' with other window."
  (interactive)
  (if (and helm-full-frame (one-window-p t))
      (error "Error: Can't swap windows in a single window")
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
  (setq helm-saved-selection (helm-get-selection))
  (unless helm-saved-selection
    (error "Nothing is selected"))
  (setq helm-saved-action (helm-get-nth-action n (helm-get-action)))
  (helm-exit-minibuffer))

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

;;;###autoload
(defun helm-select-2nd-action ()
  "Select the 2nd action for the currently selected candidate."
  (interactive)
  (helm-select-nth-action 1))

;;;###autoload
(defun helm-select-3rd-action ()
  "Select the 3rd action for the currently selected candidate."
  (interactive)
  (helm-select-nth-action 2))

;;;###autoload
(defun helm-select-4th-action ()
  "Select the 4th action for the currently selected candidate."
  (interactive)
  (helm-select-nth-action 3))

;;;###autoload
(defun helm-select-2nd-action-or-end-of-line ()
  "Select the 2nd action for the currently selected candidate.
This happen when point is at the end of minibuffer.
Otherwise goto the end of minibuffer."
  (interactive)
  (if (eolp)
      (helm-select-nth-action 1)
    (end-of-line)))

;; Utility: Persistent Action
(defmacro with-helm-display-same-window (&rest body)
  "Execute BODY in the window used for persistent action.
Make `pop-to-buffer' and `display-buffer' display in the same window."
  (declare (indent 0) (debug t))
  `(let ((display-buffer-function 'helm-persistent-action-display-buffer))
     ,@body))

(defun helm-initialize-persistent-action ()
  (set (make-local-variable 'helm-persistent-action-display-window) nil))

;;;###autoload
(cl-defun helm-execute-persistent-action
    (&optional (attr 'persistent-action) split-onewindow)
  "Perform the associated action ATTR without quitting helm.
ATTR default is 'persistent-action', but it can be anything else.
In this case you have to add this new attribute to your source.

When `helm-full-frame' or SPLIT-ONEWINDOW are non--nil,
and `helm-buffer' is displayed in only one window,
the helm window is splitted to display
`helm-select-persistent-action-window' in other window 
and keep its visibility."
  (interactive)
  (helm-log "executing persistent-action")
  (let* ((attr-val (assoc-default attr (helm-get-current-source)))
         ;; If attr value is a cons, use its car as persistent function
         ;; and its car to decide if helm window should be splitted.
         (fn       (if (and (consp attr-val)
                            ;; maybe a lambda.
                            (not (functionp attr-val)))
                       (car attr-val) attr-val))
         (no-split (and (consp attr-val)
                        (not (functionp attr-val))
                        (cdr attr-val))))
    (with-helm-window
      (save-selected-window
        (if no-split
            (helm-select-persistent-action-window)
          (helm-select-persistent-action-window
           (or split-onewindow helm-onewindow-p)))
        (helm-log-eval (current-buffer))
        (let ((helm-in-persistent-action t))
          (with-helm-display-same-window
            (helm-execute-selection-action-1 nil (or fn (helm-get-action)) t)
            (helm-log-run-hook 'helm-after-persistent-action-hook))
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
            (delete-other-windows)))))))

(defun helm-persistent-action-display-window (&optional split-onewindow)
  "Return the window that will be used for persistent action.
If SPLIT-ONEWINDOW is non--nil window will be splitted in persistent action."
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
  (select-window
   (setq minibuffer-scroll-window
         (helm-persistent-action-display-window split-onewindow))))

(defun helm-persistent-action-display-buffer (buf &optional  action)
  "Make `pop-to-buffer' and `display-buffer' display in the same window.
If `helm-persistent-action-use-special-display' is non-nil and
BUF is to be displayed by `special-display-function', use it.
Otherwise ignores `special-display-buffer-names' and `special-display-regexps'.
Argument ACTION if present will be used as second argument of `display-buffer'."
  (let* ((name (buffer-name buf))
         display-buffer-function pop-up-windows pop-up-frames
         ;; Disable `special-display-regexps' and `special-display-buffer-names'
         ;; unless `helm-persistent-action-use-special-display' is non--nil.
         (special-display-buffer-names
          (and helm-persistent-action-use-special-display
               special-display-buffer-names))
         (special-display-regexps
          (and helm-persistent-action-use-special-display
               special-display-regexps))
         (same-window-regexps
          (unless (and helm-persistent-action-use-special-display
                       (or (member name
                                   (mapcar (lambda (x) (or (car-safe x) x))
                                           special-display-buffer-names))
                           (cl-loop for x in special-display-regexps
                                 thereis (string-match (or (car-safe x) x)
                                                       name))))
            '("."))))
    ;; Don't loose minibuffer when displaying persistent window in
    ;; another frame.
    ;; This happen when the displayed persistent buffer-name is one of
    ;; `special-display-buffer-names' or match `special-display-regexps'
    ;; and `helm-persistent-action-use-special-display' is enabled.
    (with-selected-window (if (or special-display-regexps
                                  special-display-buffer-names)
                              (minibuffer-window)
                            (selected-window))
      ;; Be sure window of BUF is not dedicated.
      (set-window-dedicated-p (get-buffer-window buf) nil)
      (display-buffer buf action))))

;; scroll-other-window(-down)? for persistent-action
(defun helm-other-window-base (command &optional scroll-amount)
  (setq scroll-amount (unless (eq scroll-amount 'noscroll)
                        helm-scroll-amount))
  (with-selected-window (helm-persistent-action-display-window)
    (funcall command scroll-amount)))

;;;###autoload
(defun helm-scroll-other-window ()
  "Scroll other window (not *Helm* window) upward."
  (interactive)
  (helm-other-window-base 'scroll-up))

;;;###autoload
(defun helm-scroll-other-window-down ()
  "Scroll other window (not *Helm* window) downward."
  (interactive)
  (helm-other-window-base 'scroll-down))

;;;###autoload
(defun helm-recenter-top-bottom-other-window ()
  "`recenter-top-bottom' in other window (not *Helm* window)."
  (interactive)
  (helm-other-window-base 'recenter-top-bottom 'noscroll))

;;;###autoload
(defun helm-reposition-window-other-window ()
  "`helm-reposition-window' in other window (not *Helm* window)."
  (interactive)
  (helm-other-window-base 'reposition-window 'noscroll))



;; Utility: Visible Mark

(defun helm-clear-visible-mark ()
  (with-current-buffer (helm-buffer-get)
    (mapc 'delete-overlay helm-visible-mark-overlays)
    (set (make-local-variable 'helm-visible-mark-overlays) nil)))

(defun helm-this-visible-mark ()
  (cl-loop for o in helm-visible-mark-overlays
        when (equal (point-at-bol) (overlay-start o))
        return o))

(defun helm-delete-visible-mark (overlay)
  (setq helm-marked-candidates
        (remove
         (cons (helm-get-current-source) (helm-get-selection))
         helm-marked-candidates))
  (delete-overlay overlay)
  (setq helm-visible-mark-overlays
        (delq overlay helm-visible-mark-overlays)))

(defun helm-make-visible-mark ()
  (let ((o (make-overlay (point-at-bol)
                          (if (helm-pos-multiline-p)
                              (or (helm-get-next-candidate-separator-pos)
                                  (point-max))
                            (1+ (point-at-eol))))))
    (overlay-put o 'face   'helm-visible-mark)
    (overlay-put o 'source (assoc-default 'name (helm-get-current-source)))
    (overlay-put o 'string (buffer-substring (overlay-start o) (overlay-end o)))
    (overlay-put o 'real   (helm-get-selection))
    (add-to-list 'helm-visible-mark-overlays o))
  (push (cons (helm-get-current-source) (helm-get-selection))
        helm-marked-candidates))

;;;###autoload
(defun helm-toggle-visible-mark ()
  "Toggle helm visible mark at point."
  (interactive)
  (with-helm-window
    (let ((nomark (assq 'nomark (helm-get-current-source))))
      (if nomark
          (message "Marking not allowed in this source")
        (helm-aif (helm-this-visible-mark)
            (helm-delete-visible-mark it)
          (helm-make-visible-mark))
        (unless (helm-end-of-source-p)
          (helm-next-line))))))

;;;###autoload
(defun helm-mark-all ()
  "Mark all visible unmarked candidates in current source."
  (interactive)
  (require 'helm-files)
  (with-helm-window
    (let ((nomark (assq 'nomark (helm-get-current-source))))
      (if nomark
          (message "Marking not allowed in this source")
        (save-excursion
          (goto-char (helm-get-previous-header-pos))
          (helm-next-line)
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
                     (cand   (helm-get-selection))
                     (bn     (and (helm-file-completion-source-p)
                                  (helm-basename cand)))
                     (src    (assoc-default 'name (helm-get-current-source))))
                (when (and (not (helm-this-visible-mark))
                           (not (or (string= prefix "[?]")
                                    (string= prefix "[@]"))))
                  ;; Don't mark possibles directories ending with . or ..
                  ;; autosave files/links and non--existent file.
                  (unless
                      (and (or (helm-file-completion-source-p)
                               (equal src "Files from Current Directory"))
                           (or (string-match "^[.]?#.*#?$\\|[^#]*[.]\\{1,2\\}$" bn)
                               ;; We need to test here when not using a transformer
                               ;; that tag prefix (i.e on tramp)
                               (not (file-exists-p cand))))
                    (helm-make-visible-mark))))
              (if (helm-pos-multiline-p)
                  (progn
                    (goto-char (or (helm-get-next-candidate-separator-pos) (point-max)))
                    (forward-line 1))
                (forward-line 1))
              (end-of-line))))
        (helm-mark-current-line)
        (message "%s candidates marked" (length helm-marked-candidates))))))

;;;###autoload
(defun helm-unmark-all ()
  "Unmark all candidates in all sources of current helm session."
  (interactive)
  (with-helm-window
    (let ((len (length helm-marked-candidates)))
      (save-excursion
        (helm-clear-visible-mark))
      (setq helm-marked-candidates nil)
      (helm-mark-current-line)
      (message "%s candidates unmarked" len))))

;;;###autoload
(defun helm-toggle-all-marks ()
  "Toggle all marks.
Mark all visible candidates of current source or unmark all candidates
visible or invisible in all sources of current helm session"
  (interactive)
  (let ((marked (helm-marked-candidates)))
    (if (and (>= (length marked) 1)
             (with-helm-window helm-visible-mark-overlays))
        (helm-unmark-all)
      (helm-mark-all))))

;;;###autoload
(defun helm-display-all-visible-marks ()
  "Show all `helm' visible marks strings.
Only useful for debugging."
  (interactive)
  (with-helm-window
    (let ((overlays (reverse helm-visible-mark-overlays)))
      (helm-run-after-quit
       (lambda ()
         (with-output-to-temp-buffer "*helm visible marks*"
           (cl-dolist (o overlays) (princ (overlay-get o 'string)))))))))

(cl-defun helm-marked-candidates (&key with-wildcard)
  "Return marked candidates of current source if any.
Otherwise one element list of current selection.
When key WITH-WILDCARD is specified try to expand a wilcard if some."
  (with-current-buffer helm-buffer
    (cl-loop with current-src = (helm-get-current-source)
          for (source . real) in
          (or (reverse helm-marked-candidates)
              (list (cons current-src (helm-get-selection))))
          when (equal current-src source)
          ;; When real is a normal filename without wildcard
          ;; file-expand-wildcards returns a list of one file.
          ;; When real is a non--existent file it return nil.
          append (let* ((elm (helm-coerce-selection real source))
                        (c   (and with-wildcard
                                  (condition-case nil
                                      (file-expand-wildcards elm t)
                                    (error nil)))))
                   (or c (list elm)))
          into cands
          finally do (prog1 (cl-return cands) (helm-log-eval cands)))))

(defun helm-current-source-name= (name)
  (save-excursion
    (goto-char (helm-get-previous-header-pos))
    (equal name (helm-current-line-contents))))

(defun helm-revive-visible-mark ()
  "Restore marked candidates when helm update display."
  (with-current-buffer helm-buffer
    (cl-dolist (o helm-visible-mark-overlays)
      (goto-char (point-min))
      (while (and (search-forward (overlay-get o 'string) nil t)
                  (helm-current-source-name= (overlay-get o 'source)))
        ;; Calculate real value of candidate.
        ;; It can be nil if candidate have only a display value.
        (let ((real (get-text-property (point-at-bol 0) 'helm-realvalue)))
          (if real
              ;; Check if real value of current candidate is the same
              ;; that the one stored in overlay.
              (and (string= (overlay-get o 'real) real)
                   (move-overlay o (point-at-bol 0) (1+ (point-at-eol 0))))
            (move-overlay o (point-at-bol 0) (1+ (point-at-eol 0)))))))))
(add-hook 'helm-update-hook 'helm-revive-visible-mark)

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

;;;###autoload
(defun helm-next-visible-mark (&optional prev)
  "Move next helm visible mark.
If PREV is non-nil move to precedent."
  (interactive)
  (with-helm-window
    (ignore-errors
      (goto-char (helm-next-point-in-list
                  (point)
                  (sort (mapcar 'overlay-start helm-visible-mark-overlays) '<)
                  prev)))
    (helm-mark-current-line)))

;;;###autoload
(defun helm-prev-visible-mark ()
  "Move previous helm visible mark."
  (interactive)
  (helm-next-visible-mark t))

;; Utility: Selection Paste
;;;###autoload
(defun helm-yank-selection (arg)
  "Set minibuffer contents to current display selection.
With a prefix arg set to real value of current selection."
  (interactive "P")
  (let ((str (helm-get-selection nil (not arg))))
    (kill-new str)
    (helm-set-pattern str)))

;;;###autoload
(defun helm-kill-selection-and-quit (arg)
  "Store current selection to kill ring.
With a prefix arg set to real value of current selection."
  (interactive "P")
  (helm-run-after-quit
   (lambda (sel)
     (kill-new sel)
     (message "Killed: %s" sel))
   (helm-get-selection nil (not arg))))


;;; Follow-mode: Automatical execution of persistent-action
;;
;;
;;;###autoload
(defun helm-follow-mode (&optional arg)
  "Execute persistent action everytime the cursor is moved when enabled.
The mode is enabled for the current source only, you will have to turn it
on again when you go to next source if you want it there also.
This mode can be enabled or disabled interactively at anytime during
helm session or enabled specifically by source by adding the `follow'
attribute to this source.
Even when the attribute `follow' exists in source, it is still possible
to disable/enable this mode interactively.
Note that when you disable it interactively and `follow' attribute exists,
`helm-follow-mode' will be disabled on next helm session even if `follow'
attribute is specified in source. To avoid this set your `follow' attribute
in source in `helm-before-initialize-hook'.

e.g:

\(add-hook 'helm-before-initialize-hook
          #'(lambda () (helm-attrset 'follow 1 helm-source-buffers-list)))

This will enable `helm-follow-mode' automatically in `helm-source-buffers-list'."
  (interactive "p")
  (with-current-buffer helm-buffer
    (let* ((src      (helm-get-current-source))
           (name     (assoc-default 'name src))
           (sym      (cl-loop for s in helm-sources
                           for sname = (and (symbolp s)
                                            (assoc-default
                                             'name (symbol-value s)))
                           thereis (and sname (string= sname name) s)))
           (fol-attr (assq 'follow src))
           (enabled  (or (< arg 0)      ; Assume follow is enabled.
                         (eq (cdr fol-attr) 1))))
      (if (eq (cdr fol-attr) 'never)
          (message "helm-follow-mode not allowed in this source")
        (helm-attrset 'follow (if enabled -1 1) src)
        (setq helm-follow-mode (eq (cdr (assq 'follow src)) 1))
        (message "helm-follow-mode is %s"
                 (if helm-follow-mode
                     "enabled" "disabled"))
        (helm-display-mode-line src))
      ;; Make follow attr persistent for this session.
      (when (and helm-follow-mode-persistent sym)
        (set (car `(,sym)) src)))))

(defvar helm-follow-input-idle-delay nil
  "`helm-follow-mode' will execute its persistent action after this delay.
Note that if the `follow-delay' attr is present in source,
it will take precedence on this.")
(defun helm-follow-execute-persistent-action-maybe ()
  "Execute persistent action in mode `helm-follow-mode'.
This happen after `helm-input-idle-delay' secs."
  (let ((src (helm-get-current-source)))
    (and (not (get-buffer-window helm-action-buffer 'visible))
         (eq (assoc-default 'follow src) 1)
         (sit-for (or (assoc-default 'follow-delay src)
                      helm-follow-input-idle-delay
                      (and helm-input-idle-delay
                           (max helm-input-idle-delay 0.01))))
         (helm-window)
         (helm-get-selection)
         (save-excursion
           (helm-execute-persistent-action)))))


(provide 'helm)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm.el ends here
