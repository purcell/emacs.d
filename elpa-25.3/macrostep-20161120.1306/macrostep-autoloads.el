;;; macrostep-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "macrostep" "macrostep.el" (23009 25353 0 0))
;;; Generated autoloads from macrostep.el

(autoload 'macrostep-mode "macrostep" "\
Minor mode for inline expansion of macros in Emacs Lisp source buffers.

\\<macrostep-keymap>Progressively expand macro forms with \\[macrostep-expand], collapse them with \\[macrostep-collapse],
and move back and forth with \\[macrostep-next-macro] and \\[macrostep-prev-macro].
Use \\[macrostep-collapse-all] or collapse all visible expansions to
quit and return to normal editing.

\\{macrostep-keymap}

\(fn &optional ARG)" t nil)

(autoload 'macrostep-expand "macrostep" "\
Expand the macro form following point by one step.

Enters `macrostep-mode' if it is not already active, making the
buffer temporarily read-only. If macrostep-mode is active and the
form following point is not a macro form, search forward in the
buffer and expand the next macro form found, if any.

With a prefix argument, the expansion is displayed in a separate
buffer instead of inline in the current buffer.  Setting
`macrostep-expand-in-separate-buffer' to non-nil swaps these two
behaviors.

\(fn &optional TOGGLE-SEPARATE-BUFFER)" t nil)

;;;***

;;;### (autoloads nil "macrostep-c" "macrostep-c.el" (23009 25353
;;;;;;  0 0))
;;; Generated autoloads from macrostep-c.el

(autoload 'macrostep-c-mode-hook "macrostep-c" "\


\(fn)" nil nil)

(add-hook 'c-mode-hook #'macrostep-c-mode-hook)

;;;***

;;;### (autoloads nil nil ("macrostep-pkg.el") (23009 25353 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; macrostep-autoloads.el ends here
