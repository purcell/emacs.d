;;; less-css-mode.el --- Major mode for editing LESS CSS files (lesscss.org)
;;
;; Copyright (C) 2011-2014 Steve Purcell
;;
;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/less-css-mode
;; Package-Version: 20160930.2153
;; Keywords: less css mode
;; Version: DEV
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;;; Commentary:
;;
;; This mode provides syntax highlighting for LESS CSS files, plus
;; optional support for compilation of .less files to .css files at
;; the time they are saved: use `less-css-compile-at-save' to enable
;; this.
;;
;; Command line utility "lessc" is required if setting
;; `less-css-compile-at-save' to t.  To install "lessc" using the
;; Node.js package manager, run "npm install less"
;;
;; Also make sure the "lessc" executable is in Emacs' PATH, example:
;; (setq exec-path (cons (expand-file-name "~/.gem/ruby/1.8/bin") exec-path))
;; or customize `less-css-lessc-command' to point to your "lessc" executable.
;;
;; We target lessc >= 1.4.0, and thus use the `--no-color' flag by
;; default.  You may want to adjust `less-css-lessc-options' for
;; compatibility with older versions.
;;
;; `less-css-mode' is derived from `css-mode', and indentation of
;; nested blocks may not work correctly with versions of `css-mode'
;; other than that bundled with recent Emacs.
;;
;; You can specify per-file values for `less-css-compile-at-save',
;; `less-css-output-file-name' or `less-css-output-directory' using a
;; variables header at the top of your .less file, e.g.:
;;
;; // -*- less-css-compile-at-save: t; less-css-output-directory: "../css" -*-
;;
;; Alternatively, you can use directory local variables to set the
;; default value of `less-css-output-directory' for your project.
;;
;; In the case of files which are included in other .less files, you
;; may want to trigger the compilation of a "master" .less file on
;; save: you can accomplish this with `less-css-input-file-name',
;; which is probably best set using directory local variables.
;;
;; If you don't need CSS output but would like to be warned of any
;; syntax errors in your .less source, consider using `flymake-less':
;; https://github.com/purcell/flymake-less
;;
;;; Credits
;;
;; The original code for this mode was, in large part, written using
;; Anton Johansson's scss-mode as a template -- thanks Anton!
;; https://github.com/antonj
;;
;;; Code:

(require 'derived)
(require 'compile)

;; There are at least three css-mode.el implementations, but we need
;; the right one in order to work as expected, not the versions by
;; Landström or Garshol

(require 'css-mode)
(unless (or (boundp 'css-navigation-syntax-table)
            (functionp 'css-smie-rules))
  (error "Wrong css-mode.el: please use the version by Stefan Monnier, bundled with Emacs >= 23"))

(defgroup less-css nil
  "Less-css mode"
  :prefix "less-css-"
  :group 'css)

;;;###autoload
(defcustom less-css-lessc-command "lessc"
  "Command used to compile LESS files.
Should be lessc or the complete path to your lessc executable,
  e.g.: \"~/.gem/ruby/1.8/bin/lessc\""
  :type 'file
  :group 'less-css
  :safe 'stringp)

;;;###autoload
(defcustom less-css-compile-at-save nil
  "If non-nil, the LESS buffers will be compiled to CSS after each save."
  :type 'boolean
  :group 'less-css
  :safe 'booleanp)

;;;###autoload
(defcustom less-css-lessc-options '("--no-color")
  "Command line options for less executable.

Use \"-x\" to minify output."
  :type '(repeat string)
  :group 'less-css
  :safe t)

;;;###autoload
(defcustom less-css-output-directory nil
  "Directory in which to save CSS, or nil to use the LESS file's directory.

This path is expanded relative to the directory of the LESS file
using `expand-file-name', so both relative and absolute paths
will work as expected."
  :type 'directory
  :group 'less-css
  :safe 'stringp)

;;;###autoload
(defcustom less-css-output-file-name nil
  "File name in which to save CSS, or nil to use <name>.css for <name>.less.

This can be also be set to a full path, or a relative path.  If
the path is relative, it will be relative to the value of
`less-css-output-dir', if set, or the current directory by
default."
  :type 'file
  :group 'less-css
  :safe 'stringp)
(make-variable-buffer-local 'less-css-output-file-name)

;;;###autoload
(defcustom less-css-input-file-name nil
  "File name which will be compiled to CSS.

When the current buffer is saved `less-css-input-file-name' file
will be compiled to css instead of the current file.

Set this in order to trigger compilation of a \"master\" .less
file which includes the current file.  The best way to set this
variable in most cases is likely to be via directory local
variables.

This can be also be set to a full path, or a relative path. If
the path is relative, it will be relative to the the current directory by
default."
  :type 'file
  :group 'less-css
  :safe 'stringp)
(make-variable-buffer-local 'less-css-input-file-name)

(defconst less-css-default-error-regex
  "^\\(?:\e\\[31m\\)?\\([^\e\n]*\\|FileError:.*\n\\)\\(?:\e\\[39m\e\\[31m\\)? in \\(?:\e\\[39m\\)?\\([^ \r\n\t\e]+\\)\\(?:\e\\[90m\\)?\\(?::\\| on line \\)\\([0-9]+\\)\\(?::\\|, column \\)\\([0-9]+\\):?\\(?:\e\\[39m\\)?")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation to CSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'compilation-error-regexp-alist-alist
             (list 'less-css less-css-default-error-regex 2 3 4 nil 1))
(add-to-list 'compilation-error-regexp-alist 'less-css)


(defun less-css-compile-maybe ()
  "Run `less-css-compile' if `less-css-compile-at-save' is non-nil."
  (if less-css-compile-at-save
      (less-css-compile)))

(defun less-css--output-path ()
  "Calculate the path for the compiled CSS file created by `less-css-compile'."
  (expand-file-name (or less-css-output-file-name
                        (concat (file-name-nondirectory (file-name-sans-extension buffer-file-name)) ".css"))
                    (or less-css-output-directory default-directory)))

(defun less-css--maybe-shell-quote-command (command)
  "Selectively shell-quote COMMAND appropriately for `system-type'."
  (funcall (if (eq system-type 'windows-nt)
               'identity
             'shell-quote-argument) command))

;;;###autoload
(defun less-css-compile ()
  "Compiles the current buffer to css using `less-css-lessc-command'."
  (interactive)
  (message "Compiling less to css")
  (let ((compilation-buffer-name-function (lambda (mode-name) "*less-css-compilation*")))
    (save-window-excursion
      (with-current-buffer
          (compile
           (mapconcat 'identity
                      (append (list (less-css--maybe-shell-quote-command less-css-lessc-command))
                              (mapcar 'shell-quote-argument less-css-lessc-options)
                              (list (shell-quote-argument
                                     (or less-css-input-file-name buffer-file-name))
                                    (shell-quote-argument (less-css--output-path))))
                      " "))
        (add-hook 'compilation-finish-functions
                  (lambda (buf msg)
                    (unless (string-match-p "^finished" msg)
                      (display-buffer buf)))
                  nil
                  t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: interpolation ("@{val}"), escaped values (~"..."), JS eval (~`...`), custom faces
(defconst less-css-font-lock-keywords
  '(;; Variables
    ("@[a-z_-][a-z-_0-9]*" . font-lock-constant-face)
    ("&" . font-lock-preprocessor-face)
    ;; Mixins
    ("\\(?:[ \t{;]\\|^\\)\\(\\.[a-z_-][a-z-_0-9]*\\)[ \t]*;" . (1 font-lock-keyword-face)))
  )

;;;###autoload
(define-derived-mode less-css-mode css-mode "LESS"
  "Major mode for editing LESS files, http://lesscss.org/
Special commands:
\\{less-css-mode-map}"
  (font-lock-add-keywords nil less-css-font-lock-keywords)
  ;; cpp-style comments
  (modify-syntax-entry ?/ ". 124b" less-css-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" less-css-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" less-css-mode-syntax-table)
  ;; Special chars that sometimes come at the beginning of words.
  (modify-syntax-entry ?. "'" less-css-mode-syntax-table)

  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'indent-line-function) 'less-css-indent-line)
  (when (functionp 'css-smie-rules)
    (smie-setup css-smie-grammar #'css-smie-rules
                :forward-token #'css-smie--forward-token
                :backward-token #'css-smie--backward-token))

  (add-hook 'after-save-hook 'less-css-compile-maybe nil t))

(define-key less-css-mode-map "\C-c\C-c" 'less-css-compile)

(defun less-css-indent-line ()
  "Indent current line according to LESS CSS indentation rules."
  (let ((css-navigation-syntax-table less-css-mode-syntax-table))
    (if (fboundp 'css-indent-line)
        (css-indent-line)
      (smie-indent-line))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))


(provide 'less-css-mode)
;;; less-css-mode.el ends here
