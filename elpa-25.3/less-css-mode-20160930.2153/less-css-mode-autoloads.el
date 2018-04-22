;;; less-css-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "less-css-mode" "less-css-mode.el" (23009 23614
;;;;;;  0 0))
;;; Generated autoloads from less-css-mode.el

(defvar less-css-lessc-command "lessc" "\
Command used to compile LESS files.
Should be lessc or the complete path to your lessc executable,
  e.g.: \"~/.gem/ruby/1.8/bin/lessc\"")

(custom-autoload 'less-css-lessc-command "less-css-mode" t)

(defvar less-css-compile-at-save nil "\
If non-nil, the LESS buffers will be compiled to CSS after each save.")

(custom-autoload 'less-css-compile-at-save "less-css-mode" t)

(defvar less-css-lessc-options '("--no-color") "\
Command line options for less executable.

Use \"-x\" to minify output.")

(custom-autoload 'less-css-lessc-options "less-css-mode" t)

(defvar less-css-output-directory nil "\
Directory in which to save CSS, or nil to use the LESS file's directory.

This path is expanded relative to the directory of the LESS file
using `expand-file-name', so both relative and absolute paths
will work as expected.")

(custom-autoload 'less-css-output-directory "less-css-mode" t)

(defvar less-css-output-file-name nil "\
File name in which to save CSS, or nil to use <name>.css for <name>.less.

This can be also be set to a full path, or a relative path.  If
the path is relative, it will be relative to the value of
`less-css-output-dir', if set, or the current directory by
default.")

(custom-autoload 'less-css-output-file-name "less-css-mode" t)

(defvar less-css-input-file-name nil "\
File name which will be compiled to CSS.

When the current buffer is saved `less-css-input-file-name' file
will be compiled to css instead of the current file.

Set this in order to trigger compilation of a \"master\" .less
file which includes the current file.  The best way to set this
variable in most cases is likely to be via directory local
variables.

This can be also be set to a full path, or a relative path. If
the path is relative, it will be relative to the the current directory by
default.")

(custom-autoload 'less-css-input-file-name "less-css-mode" t)

(autoload 'less-css-compile "less-css-mode" "\
Compiles the current buffer to css using `less-css-lessc-command'.

\(fn)" t nil)

(autoload 'less-css-mode "less-css-mode" "\
Major mode for editing LESS files, http://lesscss.org/
Special commands:
\\{less-css-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; less-css-mode-autoloads.el ends here
