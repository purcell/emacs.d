;;; coffee-mode.el --- Major mode for CoffeeScript code -*- lexical-binding: t; -*-

;; Copyright (C) 2010 Chris Wanstrath

;; Version: 0.6.3
;; Package-Version: 20170324.240
;; Keywords: CoffeeScript major mode
;; Author: Chris Wanstrath <chris@ozmm.org>
;; URL: http://github.com/defunkt/coffee-mode
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Provides syntax highlighting, indentation support, imenu support,
;; compiling to JavaScript, REPL, a menu bar, and a few cute commands.

;;; Code:

(require 'comint)
(require 'easymenu)
(require 'font-lock)
(require 'rx)

(require 'cl-lib)

(declare-function tramp-file-name-localname "tramp")
(declare-function tramp-dissect-file-name "tramp")

;;
;; Customizable Variables
;;

(defconst coffee-mode-version "0.6.3"
  "The version of `coffee-mode'.")

(defgroup coffee nil
  "A CoffeeScript major mode."
  :group 'languages)

(defcustom coffee-tab-width tab-width
  "The tab width to use when indenting."
  :type 'integer
  :safe 'integerp)

(defcustom coffee-command "coffee"
  "The CoffeeScript command used for evaluating code."
  :type 'string)

(defcustom coffee-js-directory ""
  "The directory for compiled JavaScript files output. This can
be an absolute path starting with a `/`, or it can be path
relative to the directory containing the coffeescript sources to
be compiled."
  :type 'string)

(defcustom js2coffee-command "js2coffee"
  "The js2coffee command used for evaluating code."
  :type 'string)

(defcustom coffee-args-repl '("-i")
  "The arguments to pass to `coffee-command' to start a REPL."
  :type '(repeat string))

(defcustom coffee-args-compile '("-c" "--no-header")
  "The arguments to pass to `coffee-command' to compile a file."
  :type '(repeat string))

(defcustom coffee-compiled-buffer-name "*coffee-compiled*"
  "The name of the scratch buffer used for compiled CoffeeScript."
  :type 'string)

(defcustom coffee-repl-buffer "*CoffeeREPL*"
  "The name of the CoffeeREPL buffer."
  :type 'string)

(defcustom coffee-compile-jump-to-error t
  "Whether to jump to the first error if compilation fails.
Since the coffee compiler does not always include a line number in
its error messages, this is not always possible."
  :type 'boolean)

(defcustom coffee-watch-buffer-name "*coffee-watch*"
  "The name of the scratch buffer used when using the --watch flag
with CoffeeScript."
  :type 'string)

(defcustom coffee-mode-hook nil
  "Hook called by `coffee-mode'.  Examples:

      ;; Compile '.coffee' files on every save
      (and (file-exists-p (buffer-file-name))
           (file-exists-p (coffee-compiled-file-name))
           (coffee-cos-mode t)))"
  :type 'hook)

(defcustom coffee-indent-tabs-mode nil
  "Indentation can insert tabs if this is t."
  :type 'boolean)

(defcustom coffee-show-mode 'js-mode
  "Major mode to used to show the compiled Javascript."
  :type 'function)

(defcustom coffee-after-compile-hook nil
  "Hook called after compile to Javascript"
  :type 'hook)

(defcustom coffee-indent-like-python-mode nil
  "Indent like python-mode."
  :type 'boolean)

(defcustom coffee-switch-to-compile-buffer nil
  "Switch to compilation buffer `coffee-compiled-buffer-name' after compiling
a buffer or region."
  :type 'boolean)

(defvar coffee-mode-map
  (let ((map (make-sparse-keymap)))
    ;; key bindings
    (define-key map (kbd "A-r") 'coffee-compile-buffer)
    (define-key map (kbd "C-c C-k") 'coffee-compile-buffer)
    (define-key map (kbd "A-R") 'coffee-compile-region)
    (define-key map (kbd "A-M-r") 'coffee-repl)
    (define-key map (kbd "C-c C-z") 'coffee-repl)
    (define-key map [remap comment-dwim] 'coffee-comment-dwim)
    (define-key map [remap newline-and-indent] 'coffee-newline-and-indent)
    (define-key map "\C-m" 'coffee-newline-and-indent)
    (define-key map "\C-c\C-o\C-s" 'coffee-cos-mode)
    (define-key map "\177" 'coffee-dedent-line-backspace)
    (define-key map (kbd "C-c C-<") 'coffee-indent-shift-left)
    (define-key map (kbd "C-c C->") 'coffee-indent-shift-right)
    (define-key map (kbd "C-c C-l") 'coffee-send-line)
    (define-key map (kbd "C-c C-r") 'coffee-send-region)
    (define-key map (kbd "C-c C-b") 'coffee-send-buffer)
    (define-key map (kbd "<backtab>") 'coffee-indent-shift-left)
    (define-key map (kbd "C-M-a") 'coffee-beginning-of-defun)
    (define-key map (kbd "C-M-e") 'coffee-end-of-block)
    (define-key map (kbd "C-M-h") 'coffee-mark-defun)
    map)
  "Keymap for CoffeeScript major mode.")

(defvar coffee--process nil)

;;
;; Commands
;;

(defun coffee-comint-filter (string)
  (ansi-color-apply
   (replace-regexp-in-string
    "\uFF00" "\n"
    (replace-regexp-in-string "\x1b\\[.[GJK]" "" string))))

(defun coffee-repl ()
  "Launch a CoffeeScript REPL using `coffee-command' as an inferior mode."
  (interactive)

  (unless (comint-check-proc coffee-repl-buffer)
    (set-buffer
     (apply 'make-comint "CoffeeREPL"
            "env"
            nil
            "NODE_NO_READLINE=1"
            coffee-command
            coffee-args-repl))
    ;; Workaround for ansi colors
    (add-hook 'comint-preoutput-filter-functions 'coffee-comint-filter nil t))

  (pop-to-buffer coffee-repl-buffer))

(defun coffee-compiled-file-name (&optional filename)
  ;; Returns the name of the JavaScript file compiled from a CoffeeScript file.
  ;; If FILENAME is omitted, the current buffer's file name is used.
  (let ((input (expand-file-name (or filename (buffer-file-name)))))
    (unless (string= coffee-js-directory "")
      (setq input
            (expand-file-name
             (concat (unless (file-name-absolute-p coffee-js-directory)
                       (file-name-directory input))
                     (file-name-as-directory coffee-js-directory)
                     (file-name-nondirectory input)))))
    (concat (file-name-sans-extension input) ".js")))

(defun coffee-revert-buffer-compiled-file (file-name)
  "Revert a buffer of compiled file when the buffer exist and is not modified."
  (let ((buffer (find-buffer-visiting file-name)))
    (when (and buffer (not (buffer-modified-p buffer)))
      (with-current-buffer buffer
        (revert-buffer nil t)))))

(defun coffee-parse-error-output (compiler-errstr)
  (let* ((msg (car (split-string compiler-errstr "[\n\r]+")))
         line column)
    (message msg)
    (when (or (string-match "on line \\([0-9]+\\)" msg)
              (string-match ":\\([0-9]+\\):\\([0-9]+\\): error:" msg))
      (setq line (string-to-number (match-string 1 msg)))
      (when (match-string 2 msg)
        (setq column (string-to-number (match-string 2 msg))))

      (when coffee-compile-jump-to-error
        (goto-char (point-min))
        (forward-line (1- line))
        (when column
          (move-to-column (1- column)))))))

(defun coffee-compile-file ()
  "Compiles and saves the current file to disk in a file of the same
base name, with extension `.js'.  Subsequent runs will overwrite the
file.

If there are compilation errors, point is moved to the first
See `coffee-compile-jump-to-error'."
  (interactive)
  (let* ((input (buffer-file-name))
         (basename (file-name-sans-extension input))
         (output (when (string-match-p "\\.js\\'" basename) ;; for Rails '.js.coffee' file
                   basename))
         (compile-args (coffee-command-compile input output))
         (compiler-output (with-temp-buffer
                            (unless (zerop (apply #'process-file coffee-command nil t nil compile-args))
                              (error "Failed: %s %s" coffee-command compile-args))
                            (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string= compiler-output "")
        (let ((file-name (coffee-compiled-file-name (buffer-file-name))))
          (message "Compiled and saved %s" (or output (concat basename ".js")))
          (coffee-revert-buffer-compiled-file file-name))
      (coffee-parse-error-output compiler-output))))

(defun coffee-compile-buffer ()
  "Compiles the current buffer and displays the JavaScript in a buffer
called `coffee-compiled-buffer-name'."
  (interactive)
  (coffee-compile-region (point-min) (point-max)))

(defsubst coffee-generate-sourcemap-p ()
  (cl-find-if (lambda (opt) (member opt '("-m" "--map"))) coffee-args-compile))

(defun coffee--coffeescript-version ()
  (with-temp-buffer
    (unless (zerop (process-file coffee-command nil t nil "--version"))
      (error "Failed: 'coffee --version'"))
    (goto-char (point-min))
    (let ((line (buffer-substring-no-properties (point) (line-end-position))))
      (when (string-match "[0-9.]+\\'" line)
        (match-string-no-properties 0 line)))))

(defun coffee--map-file-name (coffee-file)
  (let* ((version (coffee--coffeescript-version))
         (extension (if (version<= "1.8" version) ".js.map" ".map")))
    ;; foo.js: foo.js.map(>= 1.8), foo.map(< 1.8)
    (concat (file-name-sans-extension coffee-file) extension)))

(defmacro coffee-save-window-if (bool &rest body)
  `(if ,bool (save-selected-window ,@body) ,@body))
(put 'coffee-save-window-if 'lisp-indent-function 1)

(defun coffee-compile-sentinel (buffer file line column)
  (lambda (proc _event)
    (when (eq (process-status proc) 'exit)
      (setq coffee--process nil)
      (coffee-save-window-if (not coffee-switch-to-compile-buffer)
        (pop-to-buffer (get-buffer coffee-compiled-buffer-name))
        (ansi-color-apply-on-region (point-min) (point-max))
        (goto-char (point-min))
        (if (not (= (process-exit-status proc) 0))
            (let ((compile-output (buffer-string)))
              (with-current-buffer buffer
                (coffee-parse-error-output compile-output)))
          (let ((props (list :sourcemap (coffee--map-file-name file)
                             :line line :column column :source file)))
            (setq buffer-read-only t)
            (when (fboundp coffee-show-mode)
              (funcall coffee-show-mode))
            (run-hook-with-args 'coffee-after-compile-hook props)))))))

(defun coffee-start-compile-process (curbuf line column)
  (lambda (start end)
    (let ((proc (apply 'start-file-process "coffee-mode"
                       (get-buffer-create coffee-compiled-buffer-name)
                       coffee-command (append coffee-args-compile '("-s" "-p"))))
          (curfile (buffer-file-name curbuf)))
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel
       proc (coffee-compile-sentinel curbuf curfile line column))
      (with-current-buffer curbuf
        (process-send-region proc start end))
      (process-send-string proc "\n")
      (process-send-eof proc)
      (setq coffee--process proc))))

(defun coffee-start-generate-sourcemap-process (start end)
  ;; so that sourcemap generation reads from the current buffer
  (save-buffer)
  (let* ((file (buffer-file-name))
         (sourcemap-buf (get-buffer-create "*coffee-sourcemap*"))
         (proc (start-file-process "coffee-sourcemap" sourcemap-buf
                                   coffee-command "-m" file))
         (curbuf (current-buffer))
         (line (line-number-at-pos))
         (column (current-column)))
    (setq coffee--process proc)
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel
     proc
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (setq coffee--process nil)
         (if (not (= (process-exit-status proc) 0))
             (let ((sourcemap-output
                    (with-current-buffer sourcemap-buf (buffer-string))))
               (with-current-buffer curbuf
                 (coffee-parse-error-output sourcemap-output)))
           (kill-buffer sourcemap-buf)
           (funcall (coffee-start-compile-process curbuf line column) start end)))))))

(defun coffee-cleanup-compile-buffer ()
  (let ((buffer (get-buffer coffee-compiled-buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer)))))

(defun coffee-compile-region (start end)
  "Compiles a region and displays the JavaScript in a buffer called
`coffee-compiled-buffer-name'."
  (interactive "r")
  (coffee-cleanup-compile-buffer)
  (if (coffee-generate-sourcemap-p)
      (coffee-start-generate-sourcemap-process start end)
    (funcall (coffee-start-compile-process
              (current-buffer) (line-number-at-pos) (current-column))
             start end)))

(defun coffee-get-repl-proc ()
  (unless (comint-check-proc coffee-repl-buffer)
    (coffee-repl)
    ;; see issue #332
    (sleep-for 0 100))
  (get-buffer-process coffee-repl-buffer))

(defun coffee-send-line ()
  "Send the current line to the inferior Coffee process."
  (interactive)
  (coffee-send-region (line-beginning-position) (line-end-position)))

(defun coffee-send-region (start end)
  "Send the current region to the inferior Coffee process."
  (interactive "r")
  (deactivate-mark t)
  (let* ((string (buffer-substring-no-properties start end))
         (proc (coffee-get-repl-proc))
         (multiline-escaped-string
          (replace-regexp-in-string "\n" "\uFF00" string)))
    (comint-simple-send proc multiline-escaped-string)))

(defun coffee-send-buffer ()
  "Send the current buffer to the inferior Coffee process."
  (interactive)
  (coffee-send-region (point-min) (point-max)))

(defun coffee-js2coffee-replace-region (start end)
  "Convert JavaScript in the region into CoffeeScript."
  (interactive "r")

  (let ((buffer (get-buffer coffee-compiled-buffer-name)))
    (when buffer
      (kill-buffer buffer)))

  (call-process-region start end
                       js2coffee-command t
                       (current-buffer)))

(defun coffee-version ()
  "Show the `coffee-mode' version in the echo area."
  (interactive)
  (message (concat "coffee-mode version " coffee-mode-version)))

(defun coffee-watch (dir-or-file)
  "Run `coffee-run-cmd' with the --watch flag on a directory or file."
  (interactive "fDirectory or File: ")
  (let ((coffee-compiled-buffer-name coffee-watch-buffer-name)
        (args (mapconcat 'identity (append coffee-args-compile (list "--watch" (expand-file-name dir-or-file))) " ")))
    (coffee-run-cmd args)))

;;
;; Menubar
;;

(easy-menu-define coffee-mode-menu coffee-mode-map
  "Menu for CoffeeScript mode"
  '("CoffeeScript"
    ["Compile File" coffee-compile-file]
    ["Compile Buffer" coffee-compile-buffer]
    ["Compile Region" coffee-compile-region]
    ["REPL" coffee-repl]
    "---"
    ["Version" coffee-version]
    ))

;;
;; Define Language Syntax
;;

;; Instance variables (implicit this)
(defvar coffee-this-regexp "\\(?:@[_[:word:]]+\\|\\<this\\)\\>")

;; Prototype::access
(defvar coffee-prototype-regexp "[_[:word:].$]+?::")

;; Assignment
(defvar coffee-assign-regexp "\\(@?[_[:word:].$]+?\\)\\s-*:")

;; Local Assignment
(defvar coffee-local-assign-regexp "\\s-*\\([_[:word:].$]+\\)\\s-*\\??=\\(?:[^>=]\\|$\\)")

;; Lambda
(defvar coffee-lambda-regexp "\\(?:([^)]*)\\)?\\s-*\\(->\\|=>\\)")

;; Namespaces
(defvar coffee-namespace-regexp "\\b\\(?:class\\s-+\\(\\S-+\\)\\)\\b")

;; Booleans
(defvar coffee-boolean-regexp
  (rx (or bol (not (any ".")))
      (group symbol-start
             (or "true" "false" "yes" "no" "on" "off" "null" "undefined")
             symbol-end)))

;; Regular expressions
(eval-and-compile
  (defvar coffee-regexp-regexp "\\s/\\(\\(?:\\\\/\\|[^/\n\r]\\)*\\)\\s/"))

;; JavaScript Keywords
(defvar coffee-js-keywords
  '("if" "else" "new" "return" "try" "catch"
    "finally" "throw" "break" "continue" "for" "in" "while"
    "delete" "instanceof" "package" "typeof" "switch" "super" "extends"
    "class" "until" "loop" "yield"))

;; Reserved keywords either by JS or CS.
(defvar coffee-js-reserved
  '("case" "default" "do" "function" "var" "void" "with"
    "const" "let" "debugger" "enum" "export" "import" "native"
    "__extends" "__hasProp"))

;; CoffeeScript keywords.
(defvar coffee-cs-keywords
  '("then" "unless" "and" "or" "is" "own"
    "isnt" "not" "of" "by" "when"))

;; Iced CoffeeScript keywords
(defvar iced-coffee-cs-keywords
  '("await" "defer"))

;; Regular expression combining the above three lists.
(defvar coffee-keywords-regexp
  ;; keywords can be member names.
  (concat "\\(?:^\\|[^.]\\)"
          (regexp-opt (append coffee-js-reserved
                              coffee-js-keywords
                              coffee-cs-keywords
                              iced-coffee-cs-keywords) 'symbols)))

;; Create the list for font-lock. Each class of keyword is given a
;; particular face.
(defvar coffee-font-lock-keywords
  ;; *Note*: order below matters. `coffee-keywords-regexp' goes last
  ;; because otherwise the keyword "state" in the function
  ;; "state_entry" would be highlighted.
  `((,coffee-regexp-regexp . font-lock-constant-face)
    (,coffee-this-regexp . font-lock-variable-name-face)
    (,coffee-prototype-regexp . font-lock-type-face)
    (,coffee-keywords-regexp 1 font-lock-keyword-face)
    (,coffee-boolean-regexp 1 font-lock-constant-face)
    (,coffee-assign-regexp . font-lock-type-face)
    (,coffee-local-assign-regexp 1 font-lock-variable-name-face)
    (,coffee-lambda-regexp 1 font-lock-function-name-face)
    (,(lambda (limit)
        (let ((res nil)
              start)
          (while (and (not res) (search-forward "#{" limit t)
                      (not (coffee-in-comment-p)))
            (let ((restart-pos (match-end 0)))
              (setq start (match-beginning 0))
              (let (finish)
                (while (and (not finish) (search-forward "}" limit t))
                  (let ((end-pos (point)))
                    (save-excursion
                      (when (and (ignore-errors (backward-list 1))
                                 (= start (1- (point))))
                        (setq res end-pos finish t)))))
                (unless finish
                  (goto-char restart-pos)))))
          (when (and res start)
            (set-match-data (list start res)))
          res))
     (0 font-lock-variable-name-face t))))

;;
;; Helper Functions
;;

(defun coffee-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For details, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
    (comment-dwim arg)
    (deactivate-mark t)))

(defsubst coffee-command-compile-options (output)
  (if output
      (append coffee-args-compile (list "-j" output))
    coffee-args-compile))

(defun coffee-command-compile (input output)
  "Run `coffee-command' to compile FILE-NAME to file with default
.js output file, or optionally to OUTPUT-FILE-NAME."
  (let* ((expanded (expand-file-name input))
         (filename (if (file-remote-p expanded)
                       (tramp-file-name-localname (tramp-dissect-file-name expanded))
                     (file-truename expanded)))
         (output-file (coffee-compiled-file-name filename))
         (output-dir (file-name-directory output-file)))
    (unless (file-directory-p output-dir)
      (make-directory output-dir t))
    (append (coffee-command-compile-options output)
            (list "-o" output-dir filename))))

(defun coffee-run-cmd (args)
  "Run `coffee-command' with the given arguments, and display the
output in a compilation buffer."
  (interactive "sArguments: ")
  (let ((compilation-buffer-name-function
         (lambda (_this-mode)
           (generate-new-buffer-name coffee-compiled-buffer-name))))
    (compile (concat coffee-command " " args))))

(defun coffee-toggle-fatness ()
  "Toggle fatness of a coffee function arrow."
  (interactive)
  (save-excursion
    (when (re-search-backward "[-=]>" nil t)
      (cond ((looking-at "=") (replace-match "-"))
            ((looking-at "-") (replace-match "="))))))

;;
;; imenu support
;;

(defconst coffee-imenu-index-regexp
  (concat "^\\(\\s-*\\)" ; $1
          "\\(?:"
          coffee-assign-regexp ; $2
          "\\s-*"
          coffee-lambda-regexp
          "\\|"
          coffee-namespace-regexp ; $4
          "\\|"
          "\\(@?[_[:word:]:.$]+\\)\\s-*=\\(?:[^>]\\|$\\)" ; $5 match prototype access too
          "\\(?:" "\\s-*" "\\(" coffee-lambda-regexp "\\)" "\\)?" ; $6
          "\\)"))

(defun coffee-imenu-create-index ()
  "Create an imenu index of all methods in the buffer."
  (interactive)

  ;; This function is called within a `save-excursion' so we're safe.
  (goto-char (point-min))

  (let ((index-alist '())
        (ns-indent 0)
        ns-name)
    ;; Go through every assignment that includes -> or => on the same
    ;; line or starts with `class'.
    (while (re-search-forward coffee-imenu-index-regexp nil t)
      (let ((current-indent (- (match-end 1) (match-beginning 1)))
            (property-name (match-string-no-properties 2))
            (class-name (match-string-no-properties 4))
            (variable-name (match-string-no-properties 5))
            (func-assign (match-string-no-properties 6)))

        ;; If this is the start of a new namespace, save the namespace's
        ;; indentation level and name.
        (if class-name
            (setq ns-name (concat class-name "::")
                  ns-indent current-indent)
          (when (and variable-name (<= current-indent ns-indent))
            (setq ns-name (concat variable-name ".")
                  ns-indent current-indent)))

        (if func-assign
            (push (cons variable-name (match-beginning 5)) index-alist)
          (when (and ns-name property-name)
            (let ((index-pos (match-beginning 2)))
              (if (<= current-indent ns-indent)
                  ;; Clear the namespace if we're no longer indented deeper
                  (setq ns-name nil ns-indent nil)
                ;; Register as index-name if we are within the context of a namespace
                (push (cons (concat ns-name property-name) index-pos) index-alist)))))))
    index-alist))

;;
;; Indentation
;;

(defsubst coffee-insert-spaces (count)
  (if coffee-indent-tabs-mode
      (insert-char (string-to-char "\t")  (floor count coffee-tab-width))
    (insert-char ?  count)))

;;; The theory is explained in the README.

(defsubst coffee--in-string-or-comment-p ()
  (nth 8 (syntax-ppss)))

(defun coffee--block-type ()
  (save-excursion
    (back-to-indentation)
    (unless (coffee--in-string-or-comment-p)
      (cond ((looking-at-p "else\\(\\s-+if\\)?\\_>") 'if-else)
            ((looking-at-p "\\(?:catch\\|finally\\)\\_>") 'try-catch)))))

(defun coffee--closed-if-else-p (curindent if-indent)
  (let (else-if-p else-p)
    (when (looking-at "else\\(?:\\s-+\\(if\\)\\)?\\_>")
      (if (string= (match-string 1) "if")
          (setq else-if-p t)
        (setq else-p t)))
    (or (and (not (or else-p else-if-p)) (<= curindent if-indent))
        (and else-p (= curindent if-indent)))))

(defun coffee--closed-try-catch-p (curindent if-indent)
  (and (not (looking-at-p "\\(?:finally\\|catch\\)\\_>"))
       (<= curindent if-indent)))

(defun coffee--closed-block-p (type if-indent limit)
  (let ((limit-line (line-number-at-pos limit))
        (closed-pred (cl-case type
                       (if-else 'coffee--closed-if-else-p)
                       (try-catch 'coffee--closed-try-catch-p)))
        finish)
    (save-excursion
      (while (and (not finish) (< (point) limit))
        (forward-line 1)
        (when (< (line-number-at-pos) limit-line)
          (let ((curindent (current-indentation)))
            (unless (coffee--in-string-or-comment-p)
              (back-to-indentation)
              (when (funcall closed-pred curindent if-indent)
                (setq finish t))))))
      finish)))

(defun coffee--find-if-else-indents (limit cmpfn)
  (let (indents)
    (while (re-search-forward "^\\s-*if\\_>" limit t)
      (let ((indent (current-indentation)))
        (unless (coffee--closed-block-p 'if-else indent limit)
          (push indent indents))))
    (sort indents cmpfn)))

(defun coffee--find-try-catch-indents (limit cmpfn)
  (let (indents)
    (while (re-search-forward "^\\s-*try\\_>" limit t)
      (let ((indent (current-indentation)))
        (unless (coffee--closed-block-p 'try-catch indent limit)
          (push indent indents))))
    (sort indents cmpfn)))

(defun coffee--find-indents (type limit cmpfn)
  (save-excursion
    (coffee-beginning-of-defun 1)
    (cl-case type
      (if-else (coffee--find-if-else-indents limit cmpfn))
      (try-catch (coffee--find-try-catch-indents limit cmpfn)))))

(defsubst coffee--decide-indent (curindent if-indents cmpfn)
  (cl-loop for if-indent in if-indents
           when (funcall cmpfn if-indent curindent)
           return if-indent
           finally
           return (car if-indents)))

(defun coffee--indent-insert-spaces (indent-size)
  (unless (= (current-indentation) indent-size)
    (save-excursion
      (goto-char (line-beginning-position))
      (delete-horizontal-space)
      (coffee-insert-spaces indent-size)))
  (when (< (current-column) (current-indentation))
    (back-to-indentation)))

(defun coffee--indent-line-like-python-mode (prev-indent repeated)
  (let ((next-indent (- (current-indentation) coffee-tab-width))
        (indent-p (coffee-line-wants-indent)))
    (if repeated
        (if (< next-indent 0)
            (+ prev-indent (if indent-p coffee-tab-width 0))
          next-indent)
      (+ prev-indent (if indent-p coffee-tab-width 0)))))

(defun coffee-indent-line ()
  "Indent current line as CoffeeScript."
  (interactive)
  (let* ((curindent (current-indentation))
         (limit (+ (line-beginning-position) curindent))
         (type (coffee--block-type))
         indent-size
         begin-indents)
    (if (and type (setq begin-indents (coffee--find-indents type limit '<)))
        (setq indent-size (coffee--decide-indent curindent begin-indents '>))
      (if coffee-indent-like-python-mode
          (setq indent-size
                (coffee--indent-line-like-python-mode
                 (coffee-previous-indent) (eq last-command this-command)))
        (let ((prev-indent (coffee-previous-indent))
              (next-indent-size (+ curindent coffee-tab-width)))
          (if (> (- next-indent-size prev-indent) coffee-tab-width)
              (setq indent-size 0)
            (setq indent-size (+ curindent coffee-tab-width))))))
    (coffee--indent-insert-spaces indent-size)))

(defun coffee-previous-indent ()
  "Return the indentation level of the previous non-blank line."
  (save-excursion
    (forward-line -1)
    (while (and (looking-at-p "^[ \t]*$") (not (bobp)))
      (forward-line -1))
    (current-indentation)))

(defun coffee-newline-and-indent ()
  "Insert a newline and indent it to the same level as the previous line."
  (interactive)

  ;; Remember the current line indentation level,
  ;; insert a newline, and indent the newline to the same
  ;; level as the previous line.
  (let ((prev-indent (current-indentation)))
    (when (< (current-column) (current-indentation))
      (move-to-column (current-indentation)))
    (delete-horizontal-space t)
    (newline)

    (if (coffee-line-wants-indent)
        ;; We need to insert an additional tab because the last line was special.
        (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
      ;; otherwise keep at the same indentation level
      (coffee-insert-spaces prev-indent))

    ;; Last line was a comment so this one should probably be,
    ;; too. Makes it easy to write multi-line comments (like the one I'm
    ;; writing right now).
    (unless (and auto-fill-function comment-auto-fill-only-comments)
      (when (coffee-previous-line-is-single-line-comment)
        (insert "# ")))))

(defun coffee-dedent-line-backspace (arg)
  "Unindent to increment of `coffee-tab-width' with ARG==1 when
called from first non-blank char of line.

Delete ARG spaces if ARG!=1."
  (interactive "*p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (if (and (= 1 arg)
             (= (point) (save-excursion
                          (back-to-indentation)
                          (point)))
             (not (bolp)))
        (let* ((extra-space-count (% (current-column) coffee-tab-width))
               (deleted-chars (if (zerop extra-space-count)
                                  coffee-tab-width
                                extra-space-count)))
          (backward-delete-char-untabify deleted-chars))
      (backward-delete-char-untabify arg))))

;; Indenters help determine whether the current line should be
;; indented further based on the content of the previous line. If a
;; line starts with `class', for instance, you're probably going to
;; want to indent the next line.

(defvar coffee-indenters-bol '("class" "for" "if" "else" "unless" "while" "until"
                               "try" "catch" "finally" "switch" "when")
  "Keywords or syntax whose presence at the start of a line means the
next line should probably be indented.")

(defun coffee-indenters-bol-regexp ()
  "Builds a regexp out of `coffee-indenters-bol' words."
  (regexp-opt coffee-indenters-bol 'words))

(defvar coffee-indenters-eol '(?> ?{ ?\[ ?:)
  "Single characters at the end of a line that mean the next line
should probably be indented.")

(defun coffee-line-wants-indent ()
  "Return t if the current line should be indented relative to the
previous line."
  (save-excursion
    (back-to-indentation)
    (skip-chars-backward "\r\n\t ")
    (let ((char-of-eol (char-before (line-end-position))))
      (or (and char-of-eol (memq char-of-eol coffee-indenters-eol))
          (progn
            (back-to-indentation)
            (and (looking-at-p (coffee-indenters-bol-regexp))
                 (not (re-search-forward "\\_<then\\_>" (line-end-position) t))))))))

(defun coffee-previous-line-is-single-line-comment ()
  "Return t if the previous line is a CoffeeScript single line comment."
  (save-excursion
    (forward-line -1)
    (back-to-indentation)
    (and (looking-at-p "#")
         (not (looking-at-p "###\\(?:\\s-+.*\\)?$"))
         (progn
           (goto-char (line-end-position))
           (nth 4 (syntax-ppss))))))

(defun coffee-indent-shift-amount (start end dir)
  "Compute distance to the closest increment of `coffee-tab-width'."
  (let ((min most-positive-fixnum))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((current (current-indentation)))
          (when (< current min)
            (setq min current)))
        (forward-line))
      (let ((rem (% min coffee-tab-width)))
        (if (zerop rem)
            coffee-tab-width
          (cond ((eq dir 'left) rem)
                ((eq dir 'right) (- coffee-tab-width rem))
                (t 0)))))))

(defun coffee-indent-shift-left (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
If COUNT is not given, indents to the closest increment of
`coffee-tab-width'. If region isn't active, the current line is
shifted. The shifted region includes the lines in which START and
END lie. An error is signaled if any lines in the region are
indented less than COUNT columns."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((amount (if count (* coffee-tab-width (prefix-numeric-value count))
                  (coffee-indent-shift-amount start end 'left))))
    (when (> amount 0)
      (let (deactivate-mark)
        (save-excursion
          (goto-char start)
          ;; Check that all lines can be shifted enough
          (while (< (point) end)
            (if (and (< (current-indentation) amount)
                     (not (looking-at-p "[ \t]*$")))
                (error "Can't shift all lines enough"))
            (forward-line))
          (indent-rigidly start end (- amount)))))))

(add-to-list 'debug-ignored-errors "^Can't shift all lines enough")

(defun coffee-indent-shift-right (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the right.
if COUNT is not given, indents to the closest increment of
`coffee-tab-width'. If region isn't active, the current line is
shifted. The shifted region includes the lines in which START and
END lie."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let (deactivate-mark
        (amount (if count (* coffee-tab-width (prefix-numeric-value count))
                  (coffee-indent-shift-amount start end 'right))))
    (indent-rigidly start end amount)))

(defun coffee-indent-region (start end)
  (interactive "r")
  (save-excursion
    (goto-char start)
    (forward-line 1)
    (while (and (not (eobp)) (< (point) end))
      (let ((prev-indent (coffee-previous-indent))
            (curindent (current-indentation))
            indent-size)
        (if (coffee-line-wants-indent)
            (let ((expected (+ prev-indent coffee-tab-width)))
              (when (/= curindent expected)
                (setq indent-size expected)))
          (when (> curindent prev-indent)
            (setq indent-size prev-indent)))
        (when indent-size
          (save-excursion
            (goto-char (line-beginning-position))
            (delete-horizontal-space)
            (coffee-insert-spaces indent-size))))
      (forward-line 1))))

;;
;; Fill
;;

(defun coffee-fill-forward-paragraph-function (&optional count)
  "`fill-forward-paragraph-function' which correctly handles block
comments such as the following:

  class Klass
    method: ->
      ###
      This is a method doc comment that spans multiple lines.
      If `fill-paragraph' is applied to this paragraph, the comment
      should preserve its format, with the delimiters on separate lines.
      ###
      ..."
  (let ((ret (forward-paragraph count)))
    (when (and (= count -1)
               (looking-at-p "[[:space:]]*###[[:space:]]*$"))
      (forward-line))
    ret))

;;
;; Define navigation functions
;;

(defconst coffee-defun-regexp
  (concat "^\\s-*\\(?:"
          coffee-assign-regexp
          "\\s-*"
          coffee-lambda-regexp
          "\\|"
          coffee-namespace-regexp
          "\\|"
          "@?[_[:word:]:.$]+\\s-*=\\(?:[^>]\\|$\\)"
          "\\s-*"
          coffee-lambda-regexp
          "\\)"))

(defun coffee-in-comment-p ()
  (unless (eobp)
    (save-excursion
      (back-to-indentation)
      (when (eq (char-after) ?#)
        (forward-char 1))
      (nth 4 (syntax-ppss)))))

(defsubst coffee-current-line-empty-p ()
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (string-match-p "^\\s-*$" line)))

(defun coffee-current-line-is-defun ()
  (save-excursion
    (goto-char (line-end-position))
    (re-search-backward coffee-defun-regexp (line-beginning-position) t)))

(defun coffee-current-line-is-assignment ()
  (save-excursion
    (goto-char (line-end-position))
    (re-search-backward "^[_[:word:].$]+\\s-*=\\(?:[^>]\\|$\\)"
                        (line-beginning-position) t)))

(defun coffee-curline-defun-type (parent-indent start-is-defun)
  (save-excursion
    (goto-char (line-end-position))
    (if (not (re-search-backward coffee-defun-regexp (line-beginning-position) t))
        (when (and (zerop parent-indent) (coffee-current-line-is-assignment))
          'other)
      (if (not start-is-defun)
          'other
        (if (< parent-indent (current-indentation))
            'child
          'other)))))

(defun coffee-same-block-p (block-indent start-is-defun)
  (let ((type (coffee-curline-defun-type block-indent start-is-defun)))
    (cond ((eq type 'child) t)
          ((eq type 'other) nil)
          (t (>= (current-indentation) block-indent)))))

(defsubst coffee-skip-line-p ()
  (or (coffee-in-comment-p) (coffee-current-line-empty-p)))

(defun coffee-skip-forward-lines (arg)
  (let ((pred (if (> arg 0)
                  (lambda () (not (eobp)))
                (lambda () (not (bobp))))))
   (while (and (funcall pred) (coffee-skip-line-p))
     (forward-line arg))))

(defun coffee-beginning-of-defun (&optional count)
  (interactive "p")
  (unless count
    (setq count 1))
  (let ((next-indent nil))
    (when (coffee-skip-line-p)
      (save-excursion
        (coffee-skip-forward-lines +1)
        (setq next-indent (current-indentation))))
    (coffee-skip-forward-lines -1)
    (let ((start-indent (or next-indent (current-indentation))))
      (when (and (not (eq this-command 'coffee-mark-defun)) (looking-back "^\\s-*" (line-beginning-position)))
        (forward-line -1))
      (let ((finish nil))
        (goto-char (line-end-position))
        (while (and (not finish) (re-search-backward coffee-defun-regexp nil 'move))
          (let ((cur-indent (current-indentation)))
            (when (<= cur-indent start-indent)
              (setq start-indent cur-indent)
              (cl-decf count)))
          (when (<= count 0)
            (back-to-indentation)
            (setq finish t)))))))

(defun coffee-end-of-block (&optional count)
  "Move point to the end of the block."
  (interactive "p")
  (unless count
    (setq count 1))
  (dotimes (_i count)
    (let* ((curline-is-defun (coffee-current-line-is-defun))
           start-indent)
      (coffee-skip-forward-lines 1)
      (setq start-indent (current-indentation))
      (when (and (zerop start-indent) (not curline-is-defun))
        (when (re-search-forward coffee-defun-regexp nil 'move)
          (back-to-indentation)
          (setq curline-is-defun t)))
      (let ((finish nil))
        (while (not finish)
          (forward-line 1)
          (coffee-skip-forward-lines 1)
          (when (or (not (coffee-same-block-p start-indent curline-is-defun))
                    (eobp))
            (setq finish t)))
        (forward-line -1)
        (coffee-skip-forward-lines -1)
        (forward-line 1)))))

(defun coffee-mark-defun ()
  (interactive)
  (let ((be-actived transient-mark-mode))
    (push-mark (point))
    (let ((cur-indent (current-indentation)))
      (coffee-beginning-of-defun)
      (push-mark (point))
      (coffee-end-of-block)
      (push-mark (point) nil be-actived)
      (let ((next-indent nil))
        (when (coffee-skip-line-p)
          (save-excursion
            (coffee-skip-forward-lines +1)
            (setq next-indent (current-indentation))))
        (when (and next-indent (< next-indent cur-indent))
          (coffee-skip-forward-lines -1))
        (coffee-beginning-of-defun)))))

;;
;; hs-minor-mode
;;

;; support for hs-minor-mode
(add-to-list 'hs-special-modes-alist
             '(coffee-mode "\\s-*\\(?:class\\|.+[-=]>$\\)" nil "#"
                           coffee-end-of-block nil))

;;
;; Based on triple quote of python.el
;;
(eval-and-compile
  (defconst coffee-block-strings-delimiter
    (rx (and
         ;; Match even number of backslashes.
         (or (not (any ?\\ ?\' ?\" ?/))
             point
             ;; Quotes might be preceded by a escaped quote.
             (and (or (not (any ?\\)) point)
                  ?\\
                  (* ?\\ ?\\)
                  (any ?\' ?\" ?/)))
         (* ?\\ ?\\)
         ;; Match single or triple quotes of any kind.
         (group (or "'''" "\"\"\"" "///"))))))

(defsubst coffee-syntax-count-quotes (quote-char start-point limit)
  (let ((i 0))
    (while (and (< i 3)
                (< (+ start-point i) limit)
                (eq (char-after (+ start-point i)) quote-char))
      (cl-incf i))
    i))

(defun coffee-syntax-block-strings-stringify ()
  (let* ((ppss (prog2
                   (backward-char 3)
                   (syntax-ppss)
                 (forward-char 3)))
         (string-start (and (not (nth 4 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) 3))
         (quote-ending-pos (point))
         (num-closing-quotes
          (and string-start
               (coffee-syntax-count-quotes
                (char-before) string-start quote-starting-pos))))
    (cond ((and string-start (= num-closing-quotes 0))
           ;; This set of quotes doesn't match the string starting
           ;; kind. Do nothing.
           nil)
          ((not string-start)
           ;; This set of quotes delimit the start of a string.
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|")))
          ((= num-closing-quotes 3)
           ;; This set of quotes delimit the end of a string.
           (put-text-property (1- quote-ending-pos) quote-ending-pos
                              'syntax-table (string-to-syntax "|"))))))

(defun coffee-syntax-propertize-block-comment ()
  (let ((curpoint (point))
        (inhibit-changing-match-data t))
    (let* ((valid-comment-start nil)
           (valid-comment-end (looking-at-p "#\\{0,2\\}\\s-*$"))
           (ppss (prog2
                     (backward-char 3)
                     (syntax-ppss)
                   (setq valid-comment-start (looking-back "^\\s-*" (line-beginning-position)))
                   (forward-char 3)))
           (in-comment (nth 4 ppss))
           (in-string (nth 3 ppss)))
      (when (or (and (not in-comment) (not in-string) valid-comment-start)
                (and in-comment valid-comment-end))
        (put-text-property (- curpoint 3) curpoint
                           'syntax-table (string-to-syntax "!"))))))

(defsubst coffee--in-string-p ()
  (nth 3 (syntax-ppss)))

(defun coffee-syntax-string-interpolation ()
  (let ((start (match-beginning 0))
        (end (point)))
    (if (not (coffee--in-string-p))
        (put-text-property start (1+ start)
                           'syntax-table (string-to-syntax "< b"))
      (goto-char start)
      (let (finish res)
        (while (and (not finish) (search-forward "}" nil t))
          (let ((end-pos (match-end 0)))
            (save-excursion
              (when (and (ignore-errors (backward-list 1))
                         (= start (1- (point))))
                (setq res end-pos finish t)))))
        (goto-char end)
        (when res
          (while (re-search-forward "[\"'#]" res t)
            (put-text-property (match-beginning 0) (match-end 0)
                               'syntax-table (string-to-syntax "_")))
          (goto-char (1- res)))))))

(defun coffee-syntax-propertize-function (start end)
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    (coffee-block-strings-delimiter
     (0 (ignore (coffee-syntax-block-strings-stringify))))
    ("/"
     (0 (ignore
         (let ((curpoint (point))
               (start (match-beginning 0))
               (end (match-end 0)))
           (goto-char start)
           (let ((ppss (syntax-ppss)))
             (cond ((nth 8 ppss)
                    (put-text-property start end
                                       'syntax-table (string-to-syntax "_"))
                    (goto-char curpoint))
                   ((looking-at coffee-regexp-regexp)
                    (put-text-property (match-beginning 1) (match-end 1)
                                       'syntax-table (string-to-syntax "_"))
                    (goto-char (match-end 0)))
                   (t (goto-char curpoint))))))))
    ("#{" (0 (ignore (coffee-syntax-string-interpolation))))
    ("###"
     (0 (ignore (coffee-syntax-propertize-block-comment)))))
   (point) end))

(defun coffee-get-comment-info ()
  (let* ((syntax (syntax-ppss))
         (commentp (nth 4 syntax))
         (comment-start-kinda (nth 8 syntax)))
    (when commentp
      (save-excursion
        (if (and
             (> comment-start-kinda 2) (< comment-start-kinda (point-max))
             (string=
              "###" (buffer-substring
                     (- comment-start-kinda 2) (1+ comment-start-kinda))))
            'multiple-line
          'single-line)))))

(defun coffee-comment-line-break-fn (&optional _)
  (let ((comment-type (coffee-get-comment-info))
        (coffee-indent-like-python-mode t))
    (comment-indent-new-line)
    (cond ((eq comment-type 'multiple-line)
           (save-excursion
             (beginning-of-line)
             (when (looking-at "[[:space:]]*\\(#\\)")
               (replace-match "" nil nil nil 1))))
          ((eq comment-type 'single-line)
           (coffee-indent-line)))))

(defun coffee-auto-fill-fn ()
  (let ((comment-type (coffee-get-comment-info))
        (fill-result (do-auto-fill))
        (coffee-indent-like-python-mode t))
    (when (and fill-result (eq comment-type 'single-line))
      (save-excursion
        (beginning-of-line)
        (when (looking-at "[[:space:]]*#")
          (replace-match "#")))
      (coffee-indent-line))))

;;
;; Define Major Mode
;;

(defvar coffee-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; perl style comment: "# ..."
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)

    ;; Treat slashes as paired delimiters; useful for finding regexps.
    (modify-syntax-entry ?/ "/" table)

    ;; single quote strings
    (modify-syntax-entry ?' "\"" table)
    table))

;;;###autoload
(define-derived-mode coffee-mode prog-mode "Coffee"
  "Major mode for editing CoffeeScript."

  ;; code for syntax highlighting
  (setq font-lock-defaults '((coffee-font-lock-keywords)))

  ;; fix comment filling function
  (setq-local comment-line-break-function #'coffee-comment-line-break-fn)
  (setq-local normal-auto-fill-function #'coffee-auto-fill-fn)

  (setq-local comment-start "#")

  ;; indentation
  (make-local-variable 'coffee-tab-width)
  (make-local-variable 'coffee-indent-tabs-mode)
  (setq-local indent-line-function 'coffee-indent-line)
  (setq-local indent-region-function 'coffee-indent-region)
  (setq-local tab-width coffee-tab-width)

  (setq-local syntax-propertize-function 'coffee-syntax-propertize-function)

  ;; fill
  (setq-local fill-forward-paragraph-function 'coffee-fill-forward-paragraph-function)

  (setq-local beginning-of-defun-function 'coffee-beginning-of-defun)
  (setq-local end-of-defun-function 'coffee-end-of-block)

  ;; imenu
  (setq-local imenu-create-index-function 'coffee-imenu-create-index)

  ;; Don't let electric-indent-mode break coffee-mode.
  (setq-local electric-indent-functions (list (lambda (_arg) 'no-indent)))

  ;; no tabs
  (setq indent-tabs-mode coffee-indent-tabs-mode))

;;
;; Compile-on-Save minor mode
;;

(defcustom coffee-cos-mode-line " CoS"
  "Lighter of `coffee-cos-mode'"
  :type 'string)

(define-minor-mode coffee-cos-mode
  "Toggle compile-on-save for coffee-mode.

Add `'(lambda () (coffee-cos-mode t))' to `coffee-mode-hook' to turn
it on by default."
  :lighter coffee-cos-mode-line
  (if coffee-cos-mode
      (add-hook 'after-save-hook 'coffee-compile-file nil t)
    (remove-hook 'after-save-hook 'coffee-compile-file t)))

;;
;; Live compile minor mode
;;

(defun coffee--live-compile (&rest _unused)
  (when (or (not coffee--process)
            (not (eq (process-status coffee--process) 'run)))
    (coffee-compile-buffer)))

(defcustom coffee-live-compile-mode-line " LiveCS"
  "Lighter of `coffee-live-compile-mode'"
  :type 'string)

(define-minor-mode coffee-live-compile-mode
  "Compile current buffer in real time"
  :lighter coffee-live-comp-mode-line
  (if coffee-live-compile-mode
      (add-hook 'after-change-functions 'coffee--live-compile nil t)
    (remove-hook 'after-change-functions 'coffee--live-compile t)))

(provide 'coffee-mode)

;;
;; On Load
;;

;; Run coffee-mode for files ending in .coffee.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.iced\\'" . coffee-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("Cakefile\\'" . coffee-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cson\\'" . coffee-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))

;;; coffee-mode.el ends here
