;;; company-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "company" "company.el" (23009 21820 0 0))
;;; Generated autoloads from company.el

(autoload 'company-mode "company" "\
\"complete anything\"; is an in-buffer completion framework.
Completion starts automatically, depending on the values
`company-idle-delay' and `company-minimum-prefix-length'.

Completion can be controlled with the commands:
`company-complete-common', `company-complete-selection', `company-complete',
`company-select-next', `company-select-previous'.  If these commands are
called before `company-idle-delay', completion will also start.

Completions can be searched with `company-search-candidates' or
`company-filter-candidates'.  These can be used while completion is
inactive, as well.

The completion data is retrieved using `company-backends' and displayed
using `company-frontends'.  If you want to start a specific backend, call
it interactively or use `company-begin-backend'.

By default, the completions list is sorted alphabetically, unless the
backend chooses otherwise, or `company-transformers' changes it later.

regular keymap (`company-mode-map'):

\\{company-mode-map}
keymap during active completions (`company-active-map'):

\\{company-active-map}

\(fn &optional ARG)" t nil)

(defvar global-company-mode nil "\
Non-nil if Global Company mode is enabled.
See the `global-company-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-company-mode'.")

(custom-autoload 'global-company-mode "company" nil)

(autoload 'global-company-mode "company" "\
Toggle Company mode in all buffers.
With prefix ARG, enable Global Company mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Company mode is enabled in all buffers where
`company-mode-on' would do it.
See `company-mode' for more information on Company mode.

\(fn &optional ARG)" t nil)

(autoload 'company-manual-begin "company" "\


\(fn)" t nil)

(autoload 'company-complete "company" "\
Insert the common part of all candidates or the current selection.
The first time this is called, the common part is inserted, the second
time, or when the selection has been changed, the selected candidate is
inserted.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "company-abbrev" "company-abbrev.el" (23009
;;;;;;  21821 0 0))
;;; Generated autoloads from company-abbrev.el

(autoload 'company-abbrev "company-abbrev" "\
`company-mode' completion backend for abbrev.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-bbdb" "company-bbdb.el" (23009 21820
;;;;;;  0 0))
;;; Generated autoloads from company-bbdb.el

(autoload 'company-bbdb "company-bbdb" "\
`company-mode' completion backend for BBDB.

\(fn COMMAND &optional ARG &rest IGNORE)" t nil)

;;;***

;;;### (autoloads nil "company-css" "company-css.el" (23009 21820
;;;;;;  0 0))
;;; Generated autoloads from company-css.el

(autoload 'company-css "company-css" "\
`company-mode' completion backend for `css-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-dabbrev" "company-dabbrev.el" (23009
;;;;;;  21820 0 0))
;;; Generated autoloads from company-dabbrev.el

(autoload 'company-dabbrev "company-dabbrev" "\
dabbrev-like `company-mode' completion backend.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-dabbrev-code" "company-dabbrev-code.el"
;;;;;;  (23009 21820 0 0))
;;; Generated autoloads from company-dabbrev-code.el

(autoload 'company-dabbrev-code "company-dabbrev-code" "\
dabbrev-like `company-mode' backend for code.
The backend looks for all symbols in the current buffer that aren't in
comments or strings.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-elisp" "company-elisp.el" (23009 21821
;;;;;;  0 0))
;;; Generated autoloads from company-elisp.el

(autoload 'company-elisp "company-elisp" "\
`company-mode' completion backend for Emacs Lisp.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-etags" "company-etags.el" (23009 21820
;;;;;;  0 0))
;;; Generated autoloads from company-etags.el

(autoload 'company-etags "company-etags" "\
`company-mode' completion backend for etags.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-files" "company-files.el" (23009 21820
;;;;;;  0 0))
;;; Generated autoloads from company-files.el

(autoload 'company-files "company-files" "\
`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-gtags" "company-gtags.el" (23009 21820
;;;;;;  0 0))
;;; Generated autoloads from company-gtags.el

(autoload 'company-gtags "company-gtags" "\
`company-mode' completion backend for GNU Global.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-ispell" "company-ispell.el" (23009
;;;;;;  21821 0 0))
;;; Generated autoloads from company-ispell.el

(autoload 'company-ispell "company-ispell" "\
`company-mode' completion backend using Ispell.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-keywords" "company-keywords.el" (23009
;;;;;;  21820 0 0))
;;; Generated autoloads from company-keywords.el

(autoload 'company-keywords "company-keywords" "\
`company-mode' backend for programming language keywords.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-nxml" "company-nxml.el" (23009 21820
;;;;;;  0 0))
;;; Generated autoloads from company-nxml.el

(autoload 'company-nxml "company-nxml" "\
`company-mode' completion backend for `nxml-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-oddmuse" "company-oddmuse.el" (23009
;;;;;;  21820 0 0))
;;; Generated autoloads from company-oddmuse.el

(autoload 'company-oddmuse "company-oddmuse" "\
`company-mode' completion backend for `oddmuse-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-semantic" "company-semantic.el" (23009
;;;;;;  21820 0 0))
;;; Generated autoloads from company-semantic.el

(autoload 'company-semantic "company-semantic" "\
`company-mode' completion backend using CEDET Semantic.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-tempo" "company-tempo.el" (23009 21820
;;;;;;  0 0))
;;; Generated autoloads from company-tempo.el

(autoload 'company-tempo "company-tempo" "\
`company-mode' completion backend for tempo.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-tng" "company-tng.el" (23009 21820
;;;;;;  0 0))
;;; Generated autoloads from company-tng.el

(autoload 'company-tng-frontend "company-tng" "\
When the user changes the selection at least once, this
frontend will display the candidate in the buffer as if it's
already there and any key outside of `company-active-map' will
confirm the selection and finish the completion.

\(fn COMMAND)" nil nil)

(autoload 'company-tng-configure-default "company-tng" "\
Applies the default configuration to enable company-tng.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "company-xcode" "company-xcode.el" (23009 21820
;;;;;;  0 0))
;;; Generated autoloads from company-xcode.el

(autoload 'company-xcode "company-xcode" "\
`company-mode' completion backend for Xcode projects.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "company-yasnippet" "company-yasnippet.el"
;;;;;;  (23009 21820 0 0))
;;; Generated autoloads from company-yasnippet.el

(autoload 'company-yasnippet "company-yasnippet" "\
`company-mode' backend for `yasnippet'.

This backend should be used with care, because as long as there are
snippets defined for the current major mode, this backend will always
shadow backends that come after it.  Recommended usages:

* In a buffer-local value of `company-backends', grouped with a backend or
  several that provide actual text completions.

  (add-hook 'js-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-dabbrev-code company-yasnippet)))))

* After keyword `:with', grouped with other backends.

  (push '(company-semantic :with company-yasnippet) company-backends)

* Not in `company-backends', just bound to a key.

  (global-set-key (kbd \"C-c y\") 'company-yasnippet)

\(fn COMMAND &optional ARG &rest IGNORE)" t nil)

;;;***

;;;### (autoloads nil nil ("company-capf.el" "company-clang.el" "company-cmake.el"
;;;;;;  "company-eclim.el" "company-pkg.el" "company-template.el")
;;;;;;  (23009 21820 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; company-autoloads.el ends here
