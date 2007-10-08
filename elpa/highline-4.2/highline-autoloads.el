;;; highline-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (highline-view-off highline-view-on highline-view-mode
;;;;;;  highline-off highline-on highline-local-mode highline-mode-off
;;;;;;  highline-mode-on highline-mode highline-customize) "highline"
;;;;;;  "highline.el" (18185 63592))
;;; Generated autoloads from highline.el

(autoload (quote highline-customize) "highline" "\
Customize highline group.

\(fn)" t nil)

(autoload (quote highline-mode) "highline" "\
Toggle global minor mode to highlight line about point (HL on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system.

\(fn &optional ARG)" t nil)

(autoload (quote highline-mode-on) "highline" "\
Turn on global minor mode to highlight line about point (HL on modeline).

\(fn)" t nil)

(autoload (quote highline-mode-off) "highline" "\
Turn off global minor mode to highlight line about point (HL on modeline).

\(fn)" t nil)

(autoload (quote highline-local-mode) "highline" "\
Toggle local minor mode to highlight the line about point (hl on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system.

\(fn &optional ARG)" t nil)

(autoload (quote highline-on) "highline" "\
Turn on local highlighting of the current line in buffer (hl on modeline).

\(fn)" t nil)

(autoload (quote highline-off) "highline" "\
Turn off local highlighting of the current line in buffer (hl on modeline).

\(fn)" t nil)

(autoload (quote highline-view-mode) "highline" "\
Toggle indirect mode to highlight current line in buffer (Ihl on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system.

Indirect highline (`highline-view-on', `highline-view-off' and
`highline-view-mode') is useful when you wish to have various \"visions\" of
the same buffer.

Indirect highline uses an indirect buffer to get the \"vision\" of the buffer.
So, if you kill an indirect buffer, the base buffer is not affected; if you
kill the base buffer, all indirect buffer related with the base buffer is
automagicaly killed.  Also, any text insertion/deletion in any indirect or base
buffer is updated in all related buffers.

See also `highline-selected-window'.

\(fn &optional ARG)" t nil)

(autoload (quote highline-view-on) "highline" "\
Turn on indirect highlightining current line in buffer (Ihl on modeline).

Indirect highline (`highline-view-on', `highline-view-off' and
`highline-view-mode') is useful when you wish to have various \"visions\" of
the same buffer.

Indirect highline uses an indirect buffer to get the \"vision\" of the buffer.
So, if you kill an indirect buffer, the base buffer is not affected; if you
kill the base buffer, all indirect buffer related with the base buffer is
automagicaly killed.  Also, any text insertion/deletion in any indirect or base
buffer is updated in all related buffers.

See also `highline-selected-window'.

\(fn)" t nil)

(autoload (quote highline-view-off) "highline" "\
Turn off indirect highlightining current line in buffer (Ihl on modeline).

Indirect highline (`highline-view-on', `highline-view-off' and
`highline-view-mode') is useful when you wish to have various \"visions\" of
the same buffer.

Indirect highline uses an indirect buffer to get the \"vision\" of the buffer.
So, if you kill an indirect buffer, the base buffer is not affected; if you
kill the base buffer, all indirect buffer related with the base buffer is
automagicaly killed.  Also, any text insertion/deletion in any indirect or base
buffer is updated in all related buffers.

See also `highline-selected-window'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("highline-pkg.el") (18185 63592 345084))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; highline-autoloads.el ends here
