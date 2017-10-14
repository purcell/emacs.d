;;; whole-line-or-region-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "whole-line-or-region" "whole-line-or-region.el"
;;;;;;  (23009 21845 0 0))
;;; Generated autoloads from whole-line-or-region.el

(defvar whole-line-or-region-extensions-alist '((copy-region-as-kill whole-line-or-region-copy-region-as-kill nil) (kill-region whole-line-or-region-kill-region nil) (kill-ring-save whole-line-or-region-kill-ring-save nil) (yank whole-line-or-region-yank nil)) "\
List of functions for whole-line-or-region to swap.

When whole-line-or-region is activated, all original functions
will be bound to their whole-line counterparts in
`whole-line-or-region-local-mode-map', with the bindings taken from
global keymap, or the optionally specified keymap.

The default is to map the following:

  o `copy-region-as-kill'  ->  `whole-line-or-region-copy-region-as-kill'
  o `kill-region'          ->  `whole-line-or-region-kill-region'
  o `kill-ring-save'       ->  `whole-line-or-region-kill-ring-save'
  o `yank'                 ->  `whole-line-or-region-yank'

In addition, the following functions are provided by the package for
your convenience:

  o `whole-line-or-region-delete'
  o `whole-line-or-region-comment-dwim'
  o `whole-line-or-region-comment-dwim-2'

See the individual functions for more information on what they do and
suggested mappings.

If you set this through other means than customize be sure to run
`whole-line-or-region-bind-keys' afterwards")

(custom-autoload 'whole-line-or-region-extensions-alist "whole-line-or-region" nil)

(autoload 'whole-line-or-region-local-mode "whole-line-or-region" "\
Toggle use of whole-line-or-region minor mode.

This minor mode allows functions to operate on the current line if
they would normally operate on a region and region is currently
undefined.

Optional ARG turns mode on iff ARG is a positive integer.

\(fn &optional ARG)" t nil)

(defvar whole-line-or-region-global-mode nil "\
Non-nil if Whole-Line-Or-Region-Global mode is enabled.
See the `whole-line-or-region-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `whole-line-or-region-global-mode'.")

(custom-autoload 'whole-line-or-region-global-mode "whole-line-or-region" nil)

(autoload 'whole-line-or-region-global-mode "whole-line-or-region" "\
Toggle Whole-Line-Or-Region-Local mode in all buffers.
With prefix ARG, enable Whole-Line-Or-Region-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Whole-Line-Or-Region-Local mode is enabled in all buffers where
`whole-line-or-region--turn-on' would do it.
See `whole-line-or-region-local-mode' for more information on Whole-Line-Or-Region-Local mode.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'whole-line-or-region-mode 'whole-line-or-region-global-mode)

(autoload 'whole-line-or-region-copy-region-as-kill "whole-line-or-region" "\
Copy region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-kill-region "whole-line-or-region" "\
Kill (cut) region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-kill-ring-save "whole-line-or-region" "\
Copy region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-yank "whole-line-or-region" "\
Yank (paste) previously killed text.

If the text to be yanked was killed with a whole-line-or-region
function *as* a whole-line, then paste it as a whole line (i.e. do not
break up the current line, and do not force the user to move point).

RAW-PREFIX is used to determine which string to yank, just as `yank'
would normally use it.

Optionally, pass in string to be \"yanked\" via STRING-IN.

\(fn RAW-PREFIX &optional STRING-IN)" t nil)

(autoload 'whole-line-or-region-delete "whole-line-or-region" "\
Delete region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-comment-dwim "whole-line-or-region" "\
Call `comment-dwim' on current region or current line.

See `comment-dwim' for details of RAW-PREFIX usage.

\(fn RAW-PREFIX)" t nil)

(autoload 'whole-line-or-region-comment-dwim-2 "whole-line-or-region" "\
Call `comment-dwim' on region or PREFIX whole lines.

\(fn PREFIX)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; whole-line-or-region-autoloads.el ends here
