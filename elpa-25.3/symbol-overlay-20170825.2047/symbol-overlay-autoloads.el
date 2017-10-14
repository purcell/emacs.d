;;; symbol-overlay-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "symbol-overlay" "symbol-overlay.el" (23009
;;;;;;  21836 0 0))
;;; Generated autoloads from symbol-overlay.el

(autoload 'symbol-overlay-mode "symbol-overlay" "\
Minor mode for auto-highlighting symbol at point.

\(fn &optional ARG)" t nil)

(autoload 'symbol-overlay-put "symbol-overlay" "\
Toggle all overlays of symbol at point.

\(fn)" t nil)

(autoload 'symbol-overlay-count "symbol-overlay" "\
Show count of symbol at point.

\(fn)" t nil)

(autoload 'symbol-overlay-remove-all "symbol-overlay" "\
Remove all highlighted symbols in the buffer.

\(fn)" t nil)

(autoload 'symbol-overlay-save-symbol "symbol-overlay" "\
Copy symbol at point.

\(fn)" t nil)

(autoload 'symbol-overlay-toggle-in-scope "symbol-overlay" "\
Toggle overlays to be showed in buffer or only in scope.

\(fn)" t nil)

(autoload 'symbol-overlay-echo-mark "symbol-overlay" "\
Jump back to the mark.

\(fn)" t nil)

(autoload 'symbol-overlay-jump-next "symbol-overlay" "\
Jump to the next location of symbol at point.

\(fn)" t nil)

(autoload 'symbol-overlay-jump-prev "symbol-overlay" "\
Jump to the previous location of symbol at point.

\(fn)" t nil)

(autoload 'symbol-overlay-jump-to-definition "symbol-overlay" "\
Jump to the definition of symbol at point.
The definition syntax should be defined in a function stored in
`symbol-overlay-definition-function' that returns the definition's regexp
with the input symbol.

\(fn)" t nil)

(autoload 'symbol-overlay-switch-forward "symbol-overlay" "\
Switch forward to another symbol.

\(fn)" t nil)

(autoload 'symbol-overlay-switch-backward "symbol-overlay" "\
Switch backward to another symbol.

\(fn)" t nil)

(autoload 'symbol-overlay-isearch-literally "symbol-overlay" "\
Isearch symbol at point literally, without `regexp-quote' the symbol.

\(fn)" t nil)

(autoload 'symbol-overlay-query-replace "symbol-overlay" "\
Query replace symbol at point.

\(fn)" t nil)

(autoload 'symbol-overlay-rename "symbol-overlay" "\
Rename symbol at point on all its occurrences.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; symbol-overlay-autoloads.el ends here
