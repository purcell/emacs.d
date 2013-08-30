;;; full-ack.el --- a front-end for ack
;;; -*- lexical-binding: t -*-
;;
;; Copyright (C) 2009-2012 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 1.0
;; Keywords: tools, matching
;; URL: http://nschum.de/src/emacs/full-ack/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x, GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; ack is a tool like grep, aimed at programmers with large trees of
;; heterogeneous source code.
;; It is available at <http://betterthangrep.com/>.
;;
;; Add the following to your .emacs:
;;
;; (add-to-list 'load-path "/path/to/full-ack")
;; (autoload 'ack-same "full-ack" nil t)
;; (autoload 'ack "full-ack" nil t)
;; (autoload 'ack-find-same-file "full-ack" nil t)
;; (autoload 'ack-find-file "full-ack" nil t)
;;
;; Run `ack' to search for all files and `ack-same' to search for files of the
;; same type as the current buffer.
;;
;; `next-error' and `previous-error' can be used to jump to the matches.
;;
;; `ack-find-file' and `ack-find-same-file' use ack to list the files in the
;; current project.  It's a convenient, though slow, way of finding files.
;;
;;; Change Log:
;;
;; 2013-03-26 (1.0)
;;    Added `ack-next-file` and `ack-previous-file`.
;;
;; 2011-12-16 (0.2.3)
;;    Added `ack-again' (bound to "g" in search buffers).
;;    Added default value for search.
;;
;; 2010-11-17 (0.2.2)
;;    Made changes for ack 1.92.
;;    Made `ack-guess-project-root' Windows friendly.
;;
;; 2009-04-13 (0.2.1)
;;    Added `ack-next-match' and `ack-previous-match'.
;;    Fixed mouse clicking and let it move next-error position.
;;
;; 2009-04-06 (0.2)
;;    Added 'unless-guessed value for `ack-prompt-for-directory'.
;;    Added `ack-list-files', `ack-find-file' and `ack-find-same-file'.
;;    Fixed regexp toggling.
;;
;; 2009-04-05 (0.1)
;;    Initial release.
;;
;;; Code:

(eval-when-compile (require 'cl))
(require 'compile)

(add-to-list 'debug-ignored-errors
             "^Moved \\(back before fir\\|past la\\)st match$")
(add-to-list 'debug-ignored-errors "^File .* not found$")

(defgroup full-ack nil
  "A front-end for ack."
  :group 'tools
  :group 'matching)

(defcustom ack-executable (executable-find "ack")
  "*The location of the ack executable."
  :group 'full-ack
  :type 'file)

(defcustom ack-arguments nil
  "*The arguments to use when running ack."
  :group 'full-ack
  :type '(repeat (string)))

(defcustom ack-mode-type-alist nil
  "*Matches major modes to searched file types.
This overrides values in `ack-mode-default-type-alist'.  The car in each
list element is a major mode, the rest are strings representing values of
the --type argument used by `ack-same'."
  :group 'full-ack
  :type '(repeat (cons (symbol :tag "Major mode")
                       (repeat (string :tag "ack type")))))

(defcustom ack-mode-extension-alist nil
  "*Matches major modes to searched file extensions.
This overrides values in `ack-mode-default-extension-alist'.  The car in
each list element is a major mode, the rest is a list of file extensions
that that should be searched in addition to the type defined in
`ack-mode-type-alist' by `ack-same'."
  :group 'full-ack
  :type '(repeat (cons (symbol :tag "Major mode")
                       (repeat :tag "File extensions"
                               (string :tag "extension")))))

(defcustom ack-ignore-case 'smart
  "*Determines whether `ack' ignores the search case.
Special value 'smart enables ack option \"smart-case\"."
  :group 'full-ackk
  :type '(choice (const :tag "Case sensitive" nil)
                 (const :tag "Smart" 'smart)
                 (const :tag "Ignore case" t)))

(defcustom ack-search-regexp t
  "*Determines whether `ack' should default to regular expression search.
Giving a prefix arg to `ack' toggles this option."
  :group 'full-ack
  :type '(choice (const :tag "Literal" nil)
                 (const :tag "Regular expression" t)))

(defcustom ack-display-buffer t
  "*Determines whether `ack' should display the result buffer.
Special value 'after means display the buffer only after a successful search."
  :group 'full-ack
  :type '(choice (const :tag "Don't display" nil)
                 (const :tag "Display immediately" t)
                 (const :tag "Display when done" 'after)))

(defcustom ack-context 2
  "*The number of context lines for `ack'"
  :group 'full-ack
  :type 'integer)

(defcustom ack-heading t
  "*Determines whether `ack' results should be grouped by file."
  :group 'full-ack
  :type '(choice (const :tag "No heading" nil)
                 (const :tag "Heading" t)))

(defcustom ack-use-environment t
  "*Determines whether `ack' should use access .ackrc and ACK_OPTIONS."
  :group 'full-ack
  :type '(choice (const :tag "Ignore environment" nil)
                 (const :tag "Use environment" t)))

(defcustom ack-root-directory-functions '(ack-guess-project-root)
  "*A list of functions used to find the ack base directory.
These functions are called until one returns a directory.  If successful,
`ack' is run from that directory instead of `default-directory'.  The
directory is verified by the user depending on `ack-promtp-for-directory'."
  :group 'full-ack
  :type '(repeat function))

(defcustom ack-project-root-file-patterns
  '(".project\\'" ".xcodeproj\\'" ".sln\\'" "\\`Project.ede\\'"
    "\\`.git\\'" "\\`.bzr\\'" "\\`_darcs\\'" "\\`.hg\\'")
  "A list of project file patterns for `ack-guess-project-root'.
Each element is a regular expression.  If a file matching either element is
found in a directory, that directory is assumed to be the project root by
`ack-guess-project-root'."
  :group 'full-ack
  :type '(repeat (string :tag "Regular expression")))

(defcustom ack-prompt-for-directory nil
  "*Determines whether `ack' asks the user for the root directory.
If this is 'unless-guessed, the value determined by
`ack-root-directory-functions' is used without confirmation.  If it is
nil, the directory is never confirmed."
  :group 'full-ack
  :type '(choice (const :tag "Don't prompt" nil)
                 (const :tag "Don't Prompt when guessed " unless-guessed)
                 (const :tag "Prompt" t)))

;;; faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface ack-separator
  '((default (:foreground "gray50")))
  "*Face for the group separator \"--\" in `ack' output."
  :group 'full-ack)

(defface ack-file
  '((((background dark)) (:foreground "green1"))
    (((background light)) (:foreground "green4")))
  "*Face for file names in `ack' output."
  :group 'full-ack)

(defface ack-line
  '((((background dark)) (:foreground "LightGoldenrod"))
    (((background dark)) (:foreground "DarkGoldenrod")))
  "*Face for line numbers in `ack' output."
  :group 'full-ack)

(defface ack-match
  '((default (:foreground "black"))
    (((background dark)) (:background "yellow"))
    (((background light)) (:background "yellow")))
  "*Face for matched text in `ack' output."
  :group 'full-ack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ack-mode-default-type-alist
  ;; Some of these names are guessed.  More should be constantly added.
  '((actionscript-mode "actionscript")
    (LaTeX-mode "tex")
    (TeX-mode "tex")
    (asm-mode "asm")
    (batch-file-mode "batch")
    (c++-mode "cpp")
    (c-mode "cc")
    (cfmx-mode "cfmx")
    (cperl-mode "perl")
    (csharp-mode "csharp")
    (css-mode "css")
    (emacs-lisp-mode "elisp")
    (erlang-mode "erlang")
    (espresso-mode "js")
    (f90-mode "fortran")
    (fortran-mode "fortran")
    (haskell-mode "haskell")
    (hexl-mode "binary")
    (html-mode "html")
    (java-mode "java")
    (javascript-mode "js")
    (jde-mode "java")
    (js2-mode "js")
    (jsp-mode "jsp")
    (latex-mode "tex")
    (lisp-mode "lisp")
    (lua-mode "lua")
    (makefile-mode "make")
    (mason-mode "mason")
    (nxml-mode "xml")
    (objc-mode "objc" "objcpp")
    (ocaml-mode "ocaml")
    (parrot-mode "parrot")
    (perl-mode "perl")
    (php-mode "php")
    (plone-mode "plone")
    (python-mode "python")
    (ruby-mode "ruby")
    (scheme-mode "scheme")
    (shell-script-mode "shell")
    (smalltalk-mode "smalltalk")
    (sql-mode "sql")
    (tcl-mode "tcl")
    (tex-mode "tex")
    (text-mode "text")
    (tt-mode "tt")
    (vb-mode "vb")
    (vim-mode "vim")
    (xml-mode "xml")
    (yaml-mode "yaml"))
  "Default values for `ack-mode-type-alist', which see.")

(defconst ack-mode-default-extension-alist
  '((d-mode "d"))
  "Default values for `ack-mode-extension-alist', which see.")

(defun ack-create-type (extensions)
  (list "--type-set"
        (concat "full-ack-custom-type=" (mapconcat 'identity extensions ","))
        "--type" "full-ack-custom-type"))

(defun ack-type-for-major-mode (mode)
  "Return the --type and --type-set arguments for major mode MODE."
  (let ((types (cdr (or (assoc mode ack-mode-type-alist)
                        (assoc mode ack-mode-default-type-alist))))
        (ext (cdr (or (assoc mode ack-mode-extension-alist)
                      (assoc mode ack-mode-default-extension-alist))))
        result)
    (dolist (type types)
      (push type result)
      (push "--type" result))
    (if ext
        (if types
            `("--type-add" ,(concat (car types)
                                    "=" (mapconcat 'identity ext ","))
              . ,result)
          (ack-create-type ext))
      result)))

;;; root ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ack-guess-project-root ()
  "A function to guess the project root directory.
This can be used in `ack-root-directory-functions'."
  (catch 'root
    (let ((dir (expand-file-name (if buffer-file-name
                                     (file-name-directory buffer-file-name)
                                   default-directory)))
          (prev-dir nil)
          (pattern (mapconcat 'identity ack-project-root-file-patterns "\\|")))
      (while (not (equal dir prev-dir))
        (when (directory-files dir nil pattern t)
          (throw 'root dir))
        (setq prev-dir dir
              dir (file-name-directory (directory-file-name dir)))))))

;;; process ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ack-buffer-name "*ack*")
(defvar ack-process nil)

(defvar ack-buffer--rerun-args nil)

(defun ack-count-matches ()
  "Count the matches printed by `ack' in the current buffer."
  (let ((c 0)
        (beg (point-min)))
    (setq beg (next-single-char-property-change beg 'ack-match))
    (while (< beg (point-max))
      (when (get-text-property beg 'ack-match)
        (incf c))
      (setq beg (next-single-char-property-change beg 'ack-match)))
    c))

(defun ack-sentinel (proc result)
  (when (eq (process-status proc) 'exit)
    (with-current-buffer (process-buffer proc)
      (let ((c (ack-count-matches)))
        (if (> c 0)
            (when (eq ack-display-buffer 'after)
              (display-buffer (current-buffer)))
          (kill-buffer (current-buffer)))
        (message "Ack finished with %d match%s" c (if (eq c 1) "" "es"))))))

(defun ack-filter (proc output)
  (let ((buffer (process-buffer proc))
        (inhibit-read-only t)
        beg)
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-excursion
            (goto-char (setq beg (point-max)))
            (insert output)
            ;; Error properties are done by font-lock.
            (font-lock-fontify-region beg (point-max))))
      (ack-abort))))

(defun ack-abort ()
  "Abort the running `ack' process."
  (interactive)
  (when (processp ack-process)
    (delete-process ack-process)))

(defun ack-option (name enabled)
  (format "--%s%s" (if enabled "" "no") name))

(defun ack-arguments-from-options (regexp)
  (let ((arguments (list "--color"
                         (ack-option "smart-case" (eq ack-ignore-case 'smart))
                         (ack-option "heading" ack-heading)
                         (ack-option "env" ack-use-environment))))
    (unless ack-ignore-case
      (push "-i" arguments))
    (unless regexp
      (push "--literal" arguments))
    (when (and ack-context (/= ack-context 0))
      (push (format "--context=%d" ack-context) arguments))
    arguments))

(defun ack-run (directory regexp &rest arguments)
  "Run ack in DIRECTORY with ARGUMENTS."
  (ack-abort)
  (setq directory
        (if directory
            (file-name-as-directory (expand-file-name directory))
          default-directory))
  (setq arguments (append ack-arguments
                          (nconc (ack-arguments-from-options regexp)
                                 arguments)))
  (let ((buffer (get-buffer-create ack-buffer-name))
        (inhibit-read-only t)
        (default-directory directory)
        (rerun-args (cons directory (cons regexp arguments))))
    (setq next-error-last-buffer buffer
          ack-buffer--rerun-args rerun-args)
    (with-current-buffer buffer
      (erase-buffer)
      (ack-mode)
      (setq buffer-read-only t
            default-directory directory)
      (set (make-local-variable 'ack-buffer--rerun-args) rerun-args)
      (font-lock-fontify-buffer)
      (when (eq ack-display-buffer t)
        (display-buffer (current-buffer))))
    (setq ack-process
          (apply 'start-process "ack" buffer ack-executable arguments))
    (set-process-sentinel ack-process 'ack-sentinel)
    (set-process-query-on-exit-flag ack-process nil)
    (set-process-filter ack-process 'ack-filter)))

(defun ack-version-string ()
  "Return the ack version string."
  (with-temp-buffer
    (call-process ack-executable nil t nil "--version")
    (goto-char (point-min))
    (re-search-forward " +")
    (buffer-substring (point) (point-at-eol))))

(defun ack-list-files (directory &rest arguments)
  (with-temp-buffer
    (let ((default-directory directory))
      (when (eq 0 (apply 'call-process ack-executable nil t nil "-f" "--print0"
                         arguments))
        (goto-char (point-min))
        (let ((beg (point-min))
              files)
          (while (re-search-forward "\0" nil t)
            (push (buffer-substring beg (match-beginning 0)) files)
            (setq beg (match-end 0)))
          files)))))

;;; commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ack-directory-history nil
  "Directories recently searched with `ack'.")
(defvar ack-literal-history nil
  "Strings recently searched for with `ack'.")
(defvar ack-regexp-history nil
  "Regular expressions recently searched for with `ack'.")

(defun ack--read (regexp)
  (let ((default (ack--default-for-read))
        (type (if regexp "pattern" "literal"))
        (history-var (if regexp 'ack-regexp-history 'ack-literal-history)))
    (read-string (if default
                     (format "ack %s search (default %s): " type default)
                   (format "ack %s search: " type))
                 (ack--initial-contents-for-read)
                 history-var
                 default)))

(defun ack--initial-contents-for-read ()
  (when (ack--use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun ack--default-for-read ()
  (unless (ack--use-region-p)
    (thing-at-point 'symbol)))

(defun ack--use-region-p ()
  (or (and (fboundp 'use-region-p) (use-region-p))
      (and transient-mark-mode mark-active
           (> (region-end) (region-beginning)))))

(defun ack-read-dir ()
  (let ((dir (run-hook-with-args-until-success 'ack-root-directory-functions)))
    (if ack-prompt-for-directory
        (if (and dir (eq ack-prompt-for-directory 'unless-guessed))
            dir
          (read-directory-name "Directory: " dir dir t))
      (or dir
          (and buffer-file-name (file-name-directory buffer-file-name))
          default-directory))))

(defun ack-xor (a b)
  (if a (not b) b))

(defun ack-interactive ()
  "Return the (interactive) arguments for `ack' and `ack-same'"
  (let ((regexp (ack-xor current-prefix-arg ack-search-regexp)))
    (list (ack--read regexp)
          regexp
          (ack-read-dir))))

(defun ack-type ()
  (or (ack-type-for-major-mode major-mode)
      (when buffer-file-name
        (ack-create-type (list (file-name-extension buffer-file-name))))))

;;;###autoload
(defun ack-same (pattern &optional regexp directory)
  "Run ack with --type matching the current `major-mode'.
The types of files searched are determined by `ack-mode-type-alist' and
`ack-mode-extension-alist'.  If no type is configured the buffer's file
extension is used for the search.
PATTERN is interpreted as a regular expression, iff REGEXP is non-nil.  If
called interactively, the value of REGEXP is determined by `ack-search-regexp'.
A prefix arg toggles that value.
DIRECTORY is the root directory.  If called interactively, it is determined by
`ack-project-root-file-patterns'.  The user is only prompted, if
`ack-prompt-for-directory' is set."
  (interactive (ack-interactive))
  (let ((type (ack-type)))
    (if type
        (apply 'ack-run directory regexp (append type (list pattern)))
      (ack pattern regexp directory))))

;;;###autoload
(defun ack (pattern &optional regexp directory)
  "Run ack.
PATTERN is interpreted as a regular expression, iff REGEXP is non-nil.  If
called interactively, the value of REGEXP is determined by `ack-search-regexp'.
A prefix arg toggles that value.
DIRECTORY is the root directory.  If called interactively, it is determined by
`ack-project-root-file-patterns'.  The user is only prompted, if
`ack-prompt-for-directory' is set."
  (interactive (ack-interactive))
  (ack-run directory regexp pattern))

(defun ack-read-file (prompt choices)
  (if ido-mode
      (ido-completing-read prompt choices nil t)
    (require 'iswitchb)
    (with-no-warnings
      (let ((iswitchb-make-buflist-hook
             `(lambda () (setq iswitchb-temp-buflist ',choices))))
        (iswitchb-read-buffer prompt nil t)))))

;;;###autoload
(defun ack-find-same-file (&optional directory)
  "Prompt to find a file found by ack in DIRECTORY."
  (interactive (list (ack-read-dir)))
  (find-file (expand-file-name
              (ack-read-file "Find file: "
                             (apply 'ack-list-files directory (ack-type)))
              directory)))

;;;###autoload
(defun ack-find-file (&optional directory)
  "Prompt to find a file found by ack in DIRECTORY."
  (interactive (list (ack-read-dir)))
  (find-file (expand-file-name (ack-read-file "Find file: "
                                              (ack-list-files directory))
                               directory)))

;;; run again ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ack-again ()
  "Run the last ack search in the same directory."
  (interactive)
  (if ack-buffer--rerun-args
      (let ((ack-buffer-name (ack--again-buffer-name)))
        (apply 'ack-run ack-buffer--rerun-args))
    (call-interactively 'ack)))

(defun ack--again-buffer-name ()
  (if (local-variable-p 'ack-buffer--rerun-args)
      (buffer-name)
    ack-buffer-name))

;;; text utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ack-visible-distance (beg end)
  "Determine the number of visible characters between BEG and END."
  (let ((offset 0)
        next)
    ;; Subtract invisible text
    (when (get-text-property beg 'invisible)
      (setq beg (next-single-property-change beg 'invisible)))
    (while (and beg (< beg end))
      (if (setq next (next-single-property-change beg 'invisible))
          (setq offset (+ offset (- (min next end) beg))
                beg (next-single-property-change next 'invisible))
        (setq beg nil)))
    offset))

(defun ack-previous-property-value (property pos)
  "Find the value of PROPERTY at or somewhere before POS."
  (or (get-text-property pos property)
      (when (setq pos (previous-single-property-change pos property))
        (get-text-property (1- pos) property))))

(defun ack-property-beg (pos property)
  "Move to the first char of consecutive sequence with PROPERTY set."
  (when (get-text-property pos property)
    (if (or (eq pos (point-min))
            (not (get-text-property (1- pos) property)))
        pos
      (previous-single-property-change pos property))))

(defun ack-property-end (pos property)
  "Move to the last char of consecutive sequence with PROPERTY set."
  (when (get-text-property pos property)
    (if (or (eq pos (point-max))
            (not (get-text-property (1+ pos) property)))
        pos
      (next-single-property-change pos property))))

;;; next-error ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ack-error-pos nil)
(make-variable-buffer-local 'ack-error-pos)

(defun ack-next-marker (pos arg marker marker-name)
  (setq arg (* 2 arg))
  (unless (get-text-property pos marker)
    (setq arg (1- arg)))
  (assert (> arg 0))
  (dotimes (i arg)
    (setq pos (next-single-property-change pos marker))
    (unless pos
      (error (format "Moved past last %s" marker-name))))
  (goto-char pos)
  pos)

(defun ack-previous-marker (pos arg marker marker-name)
  (assert (> arg 0))
  (dotimes (i (* 2 arg))
    (setq pos (previous-single-property-change pos marker))
    (unless pos
      (error (format "Moved back before first %s" marker-name))))
  (goto-char pos)
  pos)

(defun ack-next-match (pos arg)
  "Move to the next match in the *ack* buffer."
  (interactive "d\np")
  (ack-next-marker pos arg 'ack-match "match"))

(defun ack-previous-match (pos arg)
  "Move to the previous match in the *ack* buffer."
  (interactive "d\np")
  (ack-previous-marker pos arg 'ack-match "match"))

(defun ack-next-file (pos arg)
  "Move to the next file in the *ack* buffer."
  (interactive "d\np")
  ;; Workaround for problem at the begining of the buffer.
  (when (bobp) (incf arg))
  (ack-next-marker pos arg 'ack-file "file"))

(defun ack-previous-file (pos arg)
  "Move to the previous file in the *ack* buffer."
  (interactive "d\np")
  (ack-previous-marker pos arg 'ack-file "file"))

(defun ack-next-error-function (arg reset)
  (when (or reset (null ack-error-pos))
    (setq ack-error-pos (point-min)))
  (ack-find-match (if (<= arg 0)
                      (ack-previous-match ack-error-pos (- arg))
                    (ack-next-match ack-error-pos arg))))

(defun ack-create-marker (pos end &optional force)
  (let ((file (ack-previous-property-value 'ack-file pos))
        (line (ack-previous-property-value 'ack-line pos))
        (offset (ack-visible-distance
                 (or (previous-single-property-change pos 'ack-line) 0)
                 pos))
        buffer)
    (if force
        (or (and file
                 line
                 (file-exists-p file)
                 (setq buffer (find-file-noselect file)))
            (error "File <%s> not found" file))
      (and file
           line
           (setq buffer (find-buffer-visiting file))))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (ack--move-to-line (string-to-number line))
          (copy-marker (+ (point) offset -1)))))))

(defun ack--move-to-line (line)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun ack-find-match (pos)
  "Jump to the match at POS."
  (interactive (list (let ((posn (event-start last-input-event)))
                       (set-buffer (window-buffer (posn-window posn)))
                       (posn-point posn))))
  (when (setq pos (ack-property-beg pos 'ack-match))
    (let ((marker (get-text-property pos 'ack-marker))
          (msg (copy-marker pos))
          (msg-end (ack-property-end pos 'ack-match))
          (compilation-context-lines ack-context)
          (inhibit-read-only t)
          (end (make-marker)))
      (setq ack-error-pos pos)

      (let ((bol (save-excursion (goto-char pos) (point-at-bol))))
        (if overlay-arrow-position
            (move-marker overlay-arrow-position bol)
          (setq overlay-arrow-position (copy-marker bol))))

      (unless (and marker (marker-buffer marker))
        (setq marker (ack-create-marker msg msg-end t))
        (add-text-properties msg msg-end (list 'ack-marker marker)))
      (set-marker end (+ marker (ack-visible-distance msg msg-end))
                  (marker-buffer marker))
      (compilation-goto-locus msg marker end)
      (set-marker msg nil)
      (set-marker end nil))))

;;; ack-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ack-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [mouse-2] 'ack-find-match)
    (define-key keymap "\C-m" 'ack-find-match)
    (define-key keymap "n" 'ack-next-match)
    (define-key keymap "p" 'ack-previous-match)
    (define-key keymap "\M-n" 'ack-next-file)
    (define-key keymap "\M-p" 'ack-previous-file)
    (define-key keymap "g" 'ack-again)
    (define-key keymap "r" 'ack-again)
    keymap))

(defconst ack-font-lock-regexp-color-fg-begin "\\(\33\\[1;..?m\\)")
(defconst ack-font-lock-regexp-color-bg-begin "\\(\33\\[30;..m\\)")
(defconst ack-font-lock-regexp-color-end "\\(\33\\[0m\\)")

(defconst ack-font-lock-regexp-line
  (concat "\\(" ack-font-lock-regexp-color-fg-begin "?\\)"
          "\\([0-9]+\\)"
          "\\(" ack-font-lock-regexp-color-end "?\\)"
          "[:-]")
  "Matches the line output from ack (with or without color).
Color is used starting ack 1.94.")

(defvar ack-font-lock-keywords
  `(("^--" . 'ack-separator)
    ;; file and line
    (,(concat "^" ack-font-lock-regexp-color-fg-begin
              "\\(.*?\\)" ack-font-lock-regexp-color-end
              "[:-]" ack-font-lock-regexp-line)
     (1 '(face nil invisible t))
     (2 `(face ack-file ack-file ,(match-string-no-properties 2)))
     (3 '(face nil invisible t))
     (4 '(face nil invisible t))
     (6 `(face ack-line ack-line ,(match-string-no-properties 6)))
     (7 '(face nil invisible t) nil optional))
    ;; lines
    (,(concat "^" ack-font-lock-regexp-line)
     (1 '(face nil invisible t))
     (3 `(face ack-line ack-line ,(match-string-no-properties 3)))
     (5 '(face nil invisible t) nil optional))
    ;; file
    (,(concat "^" ack-font-lock-regexp-color-fg-begin
              "\\(.*?\\)" ack-font-lock-regexp-color-end "$")
     (1 '(face nil invisible t))
     (2 `(face ack-file ack-file ,(match-string-no-properties 2)))
     (3 '(face nil invisible t)))
    ;; matches
    (,(concat ack-font-lock-regexp-color-bg-begin
              "\\(.*?\\)"
              ack-font-lock-regexp-color-end)
     (1 '(face nil invisible t))
     (0 `(face ack-match
               ack-marker ,(ack-create-marker (match-beginning 2) (match-end 2))
               ack-match t
               mouse-face highlight
               follow-link t))
     (3 '(face nil invisible t)))
    ;; noise
    ("\\(\33\\[\\(0m\\|K\\)\\)"
     (0 '(face nil invisible t)))))

(define-derived-mode ack-mode nil "ack"
  "Major mode for ack output."
  font-lock-defaults
  (setq font-lock-defaults
        (list ack-font-lock-keywords t))
  (set (make-local-variable 'font-lock-extra-managed-props)
       '(mouse-face follow-link ack-line ack-file ack-marker ack-match))
  (make-local-variable 'overlay-arrow-position)
  (set (make-local-variable 'overlay-arrow-string) "")

  (font-lock-fontify-buffer)
  (use-local-map ack-mode-map)

  (setq next-error-function 'ack-next-error-function
        ack-error-pos nil))

(provide 'full-ack)
;;; full-ack.el ends here
