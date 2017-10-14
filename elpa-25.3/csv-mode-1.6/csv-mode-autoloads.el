;;; csv-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "csv-mode" "csv-mode.el" (23009 23262 0 0))
;;; Generated autoloads from csv-mode.el

(autoload 'csv-mode "csv-mode" "\
Major mode for editing files of comma-separated value type.

CSV mode is derived from `text-mode', and runs `text-mode-hook' before
running `csv-mode-hook'.  It turns `auto-fill-mode' off by default.
CSV mode can be customized by user options in the CSV customization
group.  The separators are specified by the value of `csv-separators'.

CSV mode commands ignore blank lines and comment lines beginning with
the value of `csv-comment-start', which delimit \"paragraphs\".
\"Sexp\" is re-interpreted to mean \"field\", so that `forward-sexp'
\(\\[forward-sexp]), `kill-sexp' (\\[kill-sexp]), etc. all apply to fields.
Standard comment commands apply, such as `comment-dwim' (\\[comment-dwim]).

If `font-lock-mode' is enabled then separators, quoted values and
comment lines are highlighted using respectively `csv-separator-face',
`font-lock-string-face' and `font-lock-comment-face'.

The user interface (UI) for CSV mode commands is similar to that of
the standard commands `sort-fields' and `sort-numeric-fields', except
that if there is no prefix argument then the UI prompts for the field
index or indices.  In `transient-mark-mode' only: if the region is not
set then the UI attempts to set it to include all consecutive CSV
records around point, and prompts for confirmation; if there is no
prefix argument then the UI prompts for it, offering as a default the
index of the field containing point if the region was not set
explicitly.  The region set automatically is delimited by blank lines
and comment lines, and the number of header lines at the beginning of
the region given by the value of `csv-header-lines' are skipped.

Sort order is controlled by `csv-descending'.

CSV mode provides the following specific keyboard key bindings:

\\{csv-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; csv-mode-autoloads.el ends here
