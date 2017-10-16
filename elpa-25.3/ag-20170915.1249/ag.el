;;; ag.el --- A front-end for ag ('the silver searcher'), the C ack replacement.

;; Copyright (C) 2013-2014 Wilfred Hughes <me@wilfred.me.uk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 11 January 2013
;; Version: 0.48
;; Package-Version: 20170915.1249
;; Package-Requires: ((dash "2.8.0") (s "1.9.0") (cl-lib "0.5"))
;;; Commentary:

;; Please see README.md for documentation, or read it online at
;; https://github.com/Wilfred/ag.el/#agel

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'cl-lib) ;; cl-letf, cl-defun
(require 'dired) ;; dired-sort-inhibit
(require 'dash)
(require 's)
(require 'find-dired) ;; find-dired-filter

(defgroup ag nil
  "A front-end for ag - The Silver Searcher."
  :group 'tools
  :group 'matching)

(defcustom ag-executable
  "ag"
  "Name of the ag executable to use."
  :type 'string
  :group 'ag)

(defcustom ag-arguments
  (list "--smart-case" "--stats")
  "Additional arguments passed to ag.

Ag.el internally uses --column, --line-number and --color
options (with specific colors) to match groups, so options
specified here should not conflict.

--line-number is required on Windows, as otherwise ag will not
print line numbers when the input is a stream."
  :type '(repeat (string))
  :group 'ag)

(defcustom ag-dired-arguments
  (list "--nocolor" "-S")
  "Additional arguments passed to ag-dired."
  :type '(repeat (string))
  :group 'ag)

(defcustom ag-context-lines nil
  "Number of context lines to include before and after a matching line."
  :type 'integer
  :group 'ag)

(defcustom ag-group-matches t
  "Group matches in the same file together.

If nil, the file name is repeated at the beginning of every match line."
  :type 'boolean
  :group 'ag)

(defcustom ag-highlight-search nil
  "Non-nil means we highlight the current search term in results.

This requires the ag command to support --color-match, which is only in v0.14+"
  :type 'boolean
  :group 'ag)

(defcustom ag-reuse-buffers nil
  "Non-nil means we reuse the existing search results buffer or
dired results buffer, rather than creating one buffer per unique
search."
  :type 'boolean
  :group 'ag)

(defcustom ag-reuse-window nil
  "Non-nil means we open search results in the same window,
hiding the results buffer."
  :type 'boolean
  :group 'ag)

(defcustom ag-project-root-function nil
  "A function to determine the project root for `ag-project'.

If set to a function, call this function with the name of the
file or directory for which to determine the project root
directory.

If set to nil, fall back to finding VCS root directories."
  :type '(choice (const :tag "Default (VCS root)" nil)
                 (function :tag "Function"))
  :group 'ag)

(defcustom ag-ignore-list nil
  "A list of patterns for files/directories to ignore when searching."
  :type '(repeat (string))
  :group 'ag)

(require 'compile)

;; Although ag results aren't exactly errors, we treat them as errors
;; so `next-error' and `previous-error' work. However, we ensure our
;; face inherits from `compilation-info-face' so the results are
;; styled appropriately.
(defface ag-hit-face '((t :inherit compilation-info))
  "Face name to use for ag matches."
  :group 'ag)

(defface ag-match-face '((t :inherit match))
  "Face name to use for ag matches."
  :group 'ag)

(defvar ag-search-finished-hook nil
  "Hook run when ag completes a search in a buffer.")

(defun ag/run-finished-hook (buffer how-finished)
  "Run the ag hook to signal that the search has completed."
  (with-current-buffer buffer
    (run-hooks 'ag-search-finished-hook)))

(defmacro ag/with-patch-function (fun-name fun-args fun-body &rest body)
  "Temporarily override the definition of FUN-NAME whilst BODY is executed.

Assumes FUNCTION is already defined (see http://emacs.stackexchange.com/a/3452/304)."
  `(cl-letf (((symbol-function ,fun-name)
              (lambda ,fun-args ,fun-body)))
     ,@body))

(defun ag/next-error-function (n &optional reset)
  "Open the search result at point in the current window or a
different window, according to `ag-reuse-window'."
  (if ag-reuse-window
      ;; prevent changing the window
      (ag/with-patch-function
       'pop-to-buffer (buffer &rest args) (switch-to-buffer buffer)
       (compilation-next-error-function n reset))

    ;; just navigate to the results as normal
    (compilation-next-error-function n reset)))

;; Note that we want to use as tight a regexp as we can to try and
;; handle weird file names (with colons in them) as well as possible.
;; E.g. we use [1-9][0-9]* rather than [0-9]+ so as to accept ":034:"
;; in file names.
(defvar ag/file-column-pattern-nogroup
  "^\\(.+?\\):\\([1-9][0-9]*\\):\\([1-9][0-9]*\\):"
  "A regexp pattern that groups output into filename, line number and column number.")

(defvar ag/file-column-pattern-group
  "^\\([[:digit:]]+\\):\\([[:digit:]]+\\):"
  "A regexp pattern to match line number and column number with grouped output.")

(defun ag/compilation-match-grouped-filename ()
  "Match filename backwards when a line/column match is found in grouped output mode."
  (save-match-data
    (save-excursion
      (when (re-search-backward "^File: \\(.*\\)$" (point-min) t)
        (list (match-string 1))))))

(define-compilation-mode ag-mode "Ag"
  "Ag results compilation mode"
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(compilation-ag-nogroup compilation-ag-group))
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (list (cons 'compilation-ag-nogroup  (list ag/file-column-pattern-nogroup 1 2 3))
             (cons 'compilation-ag-group  (list ag/file-column-pattern-group
                                                'ag/compilation-match-grouped-filename 1 2))))
  (set (make-local-variable 'compilation-error-face) 'ag-hit-face)
  (set (make-local-variable 'next-error-function) #'ag/next-error-function)
  (set (make-local-variable 'compilation-finish-functions)
       #'ag/run-finished-hook)
  (add-hook 'compilation-filter-hook 'ag-filter nil t))

(define-key ag-mode-map (kbd "p") #'compilation-previous-error)
(define-key ag-mode-map (kbd "n") #'compilation-next-error)
(define-key ag-mode-map (kbd "k") '(lambda () (interactive)
                                     (let (kill-buffer-query-functions) (kill-buffer))))

(defun ag/buffer-name (search-string directory regexp)
  "Return a buffer name formatted according to ag.el conventions."
  (cond
   (ag-reuse-buffers "*ag search*")
   (regexp (format "*ag search regexp:%s dir:%s*" search-string directory))
   (:else (format "*ag search text:%s dir:%s*" search-string directory))))

(defun ag/format-ignore (ignores)
  "Prepend '--ignore' to every item in IGNORES."
  (apply #'append
         (mapcar (lambda (item) (list "--ignore" item)) ignores)))

(cl-defun ag/search (string directory
                            &key (regexp nil) (file-regex nil) (file-type nil))
  "Run ag searching for the STRING given in DIRECTORY.
If REGEXP is non-nil, treat STRING as a regular expression."
  (let ((default-directory (file-name-as-directory directory))
        (arguments ag-arguments)
        (shell-command-switch "-c"))
    ;; Add double dashes at the end of command line if not specified in
    ;; ag-arguments.
    (unless (equal (car (last arguments)) "--")
      (setq arguments (append arguments '("--"))))
    (setq arguments
          (append '("--line-number" "--column" "--color" "--color-match" "30;43"
                    "--color-path" "1;32")
                  arguments))
    (if ag-group-matches
        (setq arguments (cons "--group" arguments))
      (setq arguments (cons "--nogroup" arguments)))
    (unless regexp
      (setq arguments (cons "--literal" arguments)))
    (when (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
      ;; Use --vimgrep to work around issue #97 on Windows.
      (setq arguments (cons "--vimgrep" arguments)))
    (when (char-or-string-p file-regex)
      (setq arguments (append `("--file-search-regex" ,file-regex) arguments)))
    (when file-type
      (setq arguments (cons (format "--%s" file-type) arguments)))
    (if (integerp current-prefix-arg)
        (setq arguments (cons (format "--context=%d" (abs current-prefix-arg)) arguments))
      (when ag-context-lines
        (setq arguments (cons (format "--context=%d" ag-context-lines) arguments))))
    (when ag-ignore-list
      (setq arguments (append (ag/format-ignore ag-ignore-list) arguments)))
    (unless (file-exists-p default-directory)
      (error "No such directory %s" default-directory))
    (let ((command-string
           (mapconcat #'shell-quote-argument
                      (append (list ag-executable) arguments (list string "."))
                      " ")))
      ;; If we're called with a prefix, let the user modify the command before
      ;; running it. Typically this means they want to pass additional arguments.
      ;; The numeric value is used for context lines: positive is just context
      ;; number (no modification), negative allows further modification.
      (when (and current-prefix-arg (not (and (integerp current-prefix-arg) (> current-prefix-arg 0))))
        ;; Make a space in the command-string for the user to enter more arguments.
        (setq command-string (ag/replace-first command-string " -- " "  -- "))
        ;; Prompt for the command.
        (let ((adjusted-point (- (length command-string) (length string) 5)))
          (setq command-string
                (read-from-minibuffer "ag command: "
                                      (cons command-string adjusted-point)))))
      ;; Call ag.
      (compilation-start
       command-string
       #'ag-mode
       `(lambda (mode-name) ,(ag/buffer-name string directory regexp))))))

(defun ag/dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

(defun ag/buffer-extension-regex ()
  "If the current buffer has an extension, return
a PCRE pattern that matches files with that extension.
Returns an empty string otherwise."
  (let ((file-name (buffer-file-name)))
    (if (stringp file-name)
        (format "\\.%s$" (ag/escape-pcre (file-name-extension file-name)))
      "")))

(defun ag/longest-string (&rest strings)
  "Given a list of strings and nils, return the longest string."
  (let ((longest-string nil))
    (dolist (string (-non-nil strings))
      (when (< (length longest-string)
               (length string))
        (setq longest-string string)))
    longest-string))

(defun ag/replace-first (string before after)
  "Replace the first occurrence of BEFORE in STRING with AFTER."
  (replace-regexp-in-string
   (concat "\\(" (regexp-quote before) "\\)" ".*\\'")
   after string
   nil nil 1))

(autoload 'vc-git-root "vc-git")

(require 'vc-svn)
;; Emacs 23.4 doesn't provide vc-svn-root.
(unless (functionp 'vc-svn-root)
  (defun vc-svn-root (file)
    (vc-find-root file vc-svn-admin-directory)))

(autoload 'vc-hg-root "vc-hg")

(autoload 'vc-bzr-root "vc-bzr")

(defun ag/project-root (file-path)
  "Guess the project root of the given FILE-PATH.

Use `ag-project-root-function' if set, or fall back to VCS
roots."
  (if ag-project-root-function
      (funcall ag-project-root-function file-path)
    (or (ag/longest-string
       (vc-git-root file-path)
       (vc-svn-root file-path)
       (vc-hg-root file-path)
       (vc-bzr-root file-path))
      file-path)))

(defun ag/dired-align-size-column ()
  (beginning-of-line)
  (when (looking-at "^  ")
    (forward-char 2)
    (search-forward " " nil t 4)
    (let* ((size-start (point))
           (size-end (search-forward " " nil t))
           (width (and size-end (- size-end size-start))))
      (when (and size-end
                 (< width 12)
                 (> width 1))
        (goto-char size-start)
        (insert (make-string (- 12 width) ? ))))))

(defun ag/dired-filter (proc string)
  "Filter the output of ag to make it suitable for `dired-mode'."
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (if (buffer-name buf)
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (widen)
              (let ((beg (point-max)))
                (goto-char beg)
                (insert string)
                (goto-char beg)
                (or (looking-at "^")
                    (progn
                      (ag/dired-align-size-column)
                      (forward-line 1)))
                (while (looking-at "^")
                  (insert "  ")
                  (ag/dired-align-size-column)
                  (forward-line 1))
                (goto-char beg)
                (beginning-of-line)

                ;; Remove occurrences of default-directory.
                (while (search-forward (concat " " default-directory) nil t)
                  (replace-match " " nil t))

                (goto-char (point-max))
                (if (search-backward "\n" (process-mark proc) t)
                    (progn
                      (dired-insert-set-properties (process-mark proc)
                                                   (1+ (point)))
                      (move-marker (process-mark proc) (1+ (point)))))))))
      (delete-process proc))))

(defun ag/dired-sentinel (proc state)
  "Update the status/modeline after the process finishes."
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (if (buffer-name buf)
        (with-current-buffer buf
          (let ((buffer-read-only nil))
            (save-excursion
              (goto-char (point-max))
              (insert "\n  ag " state)
              (forward-char -1)     ;Back up before \n at end of STATE.
              (insert " at " (substring (current-time-string) 0 19))
              (forward-char 1)
              (setq mode-line-process
                    (concat ":" (symbol-name (process-status proc))))
              ;; Since the buffer and mode line will show that the
              ;; process is dead, we can delete it now.  Otherwise it
              ;; will stay around until M-x list-processes.
              (delete-process proc)
              (force-mode-line-update)))
          (run-hooks 'dired-after-readin-hook)
          (message "%s finished." (current-buffer))))))

(defun ag/kill-process ()
  "Kill the `ag' process running in the current buffer."
  (interactive)
  (let ((ag (get-buffer-process (current-buffer))))
    (and ag (eq (process-status ag) 'run)
         (eq (process-filter ag) (function find-dired-filter))
         (condition-case nil
             (delete-process ag)
           (error nil)))))

(defun ag/escape-pcre (regexp)
  "Escape the PCRE-special characters in REGEXP so that it is
matched literally."
  (let ((alphanum "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (apply #'concat
           (mapcar
            (lambda (c)
              (cond
               ((not (string-match-p (regexp-quote c) alphanum))
                (concat "\\" c))
               (t c)))
            (mapcar #'char-to-string (string-to-list regexp))))))

;;;###autoload
(defun ag (string directory)
  "Search using ag in a given DIRECTORY for a given literal search STRING,
with STRING defaulting to the symbol under point.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (ag/read-from-minibuffer "Search string")
                     (read-directory-name "Directory: ")))
  (ag/search string directory))

;;;###autoload
(defun ag-files (string file-type directory)
  "Search using ag in a given DIRECTORY for a given literal search STRING,
limited to files that match FILE-TYPE. STRING defaults to the
symbol under point.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (ag/read-from-minibuffer "Search string")
                     (ag/read-file-type)
                     (read-directory-name "Directory: ")))
  (apply #'ag/search string directory file-type))

;;;###autoload
(defun ag-regexp (string directory)
  "Search using ag in a given directory for a given regexp.
The regexp should be in PCRE syntax, not Emacs regexp syntax.

If called with a prefix, prompts for flags to pass to ag."
  (interactive "sSearch regexp: \nDDirectory: ")
  (ag/search string directory :regexp t))

;;;###autoload
(defun ag-project (string)
  "Guess the root of the current project and search it with ag
for the given literal search STRING.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (ag/read-from-minibuffer "Search string")))
  (ag/search string (ag/project-root default-directory)))

;;;###autoload
(defun ag-project-files (string file-type)
  "Search using ag for a given literal search STRING,
limited to files that match FILE-TYPE. STRING defaults to the
symbol under point.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (ag/read-from-minibuffer "Search string")
                     (ag/read-file-type)))
  (apply 'ag/search string (ag/project-root default-directory) file-type))

(defun ag/read-from-minibuffer (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, offer that as a default."
  (let* ((suggested (ag/dwim-at-point))
         (final-prompt
          (if suggested
              (format "%s (default %s): " prompt suggested)
            (format "%s: " prompt)))
         ;; Ask the user for input, but add `suggested' to the history
         ;; so they can use M-n if they want to modify it.
         (user-input (read-from-minibuffer
                      final-prompt
                      nil nil nil nil suggested)))
    ;; Return the input provided by the user, or use `suggested' if
    ;; the input was empty.
    (if (> (length user-input) 0)
        user-input
      suggested)))

;;;###autoload
(defun ag-project-regexp (regexp)
  "Guess the root of the current project and search it with ag
for the given regexp. The regexp should be in PCRE syntax, not
Emacs regexp syntax.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (ag/read-from-minibuffer "Search regexp")))
  (ag/search regexp (ag/project-root default-directory) :regexp t))

(autoload 'symbol-at-point "thingatpt")

;;;###autoload
(defalias 'ag-project-at-point 'ag-project)
(make-obsolete 'ag-project-at-point 'ag-project "0.19")

;;;###autoload
(defalias 'ag-regexp-project-at-point 'ag-project-regexp)
(make-obsolete 'ag-regexp-project-at-point 'ag-project-regexp "0.46")

;;;###autoload
(defun ag-dired (dir string)
  "Recursively find files in DIR matching literal search STRING.

The PATTERN is matched against the full path to the file, not
only against the file name.

The results are presented as a `dired-mode' buffer with
`default-directory' being DIR.

See also `ag-dired-regexp'."
  (interactive "DDirectory: \nsFile pattern: ")
  (ag-dired-regexp dir (ag/escape-pcre string)))

;;;###autoload
(defun ag-dired-regexp (dir regexp)
  "Recursively find files in DIR matching REGEXP.
REGEXP should be in PCRE syntax, not Emacs regexp syntax.

The REGEXP is matched against the full path to the file, not
only against the file name.

Results are presented as a `dired-mode' buffer with
`default-directory' being DIR.

See also `find-dired'."
  (interactive "DDirectory: \nsFile regexp: ")
  (let* ((dired-buffers dired-buffers) ;; do not mess with regular dired buffers
         (orig-dir dir)
         (dir (file-name-as-directory (expand-file-name dir)))
         (buffer-name (if ag-reuse-buffers
                          "*ag dired*"
                        (format "*ag dired pattern:%s dir:%s*" regexp dir)))
         (cmd (concat ag-executable " " (string-join ag-dired-arguments " ") " -g '" regexp "' "
                      (shell-quote-argument dir)
                      " | grep -v '^$' | sed s/\\'/\\\\\\\\\\'/ | xargs -I '{}' "
                      insert-directory-program " "
                      dired-listing-switches " '{}' &")))
    (with-current-buffer (get-buffer-create buffer-name)
      (switch-to-buffer (current-buffer))
      (widen)
      (kill-all-local-variables)
      (if (fboundp 'read-only-mode)
          (read-only-mode -1)
        (setq buffer-read-only nil))
      (let ((inhibit-read-only t)) (erase-buffer))
      (setq default-directory dir)
      (run-hooks 'dired-before-readin-hook)
      (shell-command cmd (current-buffer))
      (insert "  " dir ":\n")
      (insert "  " cmd "\n")
      (dired-mode dir)
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map (current-local-map))
        (define-key map "\C-c\C-k" 'ag/kill-process)
        (use-local-map map))
      (set (make-local-variable 'dired-sort-inhibit) t)
      (set (make-local-variable 'revert-buffer-function)
           `(lambda (ignore-auto noconfirm)
              (ag-dired-regexp ,orig-dir ,regexp)))
      (if (fboundp 'dired-simple-subdir-alist)
          (dired-simple-subdir-alist)
        (set (make-local-variable 'dired-subdir-alist)
             (list (cons default-directory (point-min-marker)))))
      (let ((proc (get-buffer-process (current-buffer))))
        (set-process-filter proc #'ag/dired-filter)
        (set-process-sentinel proc #'ag/dired-sentinel)
        ;; Initialize the process marker; it is used by the filter.
        (move-marker (process-mark proc) 1 (current-buffer)))
      (setq mode-line-process '(":%s")))))

;;;###autoload
(defun ag-project-dired (pattern)
  "Recursively find files in current project matching PATTERN.

See also `ag-dired'."
  (interactive "sFile pattern: ")
  (ag-dired-regexp (ag/project-root default-directory) (ag/escape-pcre pattern)))

;;;###autoload
(defun ag-project-dired-regexp (regexp)
  "Recursively find files in current project matching REGEXP.

See also `ag-dired-regexp'."
  (interactive "sFile regexp: ")
  (ag-dired-regexp (ag/project-root default-directory) regexp))

;;;###autoload
(defun ag-kill-buffers ()
  "Kill all `ag-mode' buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value 'major-mode buffer) 'ag-mode)
      (kill-buffer buffer))))

;;;###autoload
(defun ag-kill-other-buffers ()
  "Kill all `ag-mode' buffers other than the current buffer."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (and
             (eq (buffer-local-value 'major-mode buffer) 'ag-mode)
             (not (eq buffer current-buffer)))
        (kill-buffer buffer)))))

;; Based on grep-filter.
(defun ag-filter ()
  "Handle escape sequences inserted by the ag process.
This function is called from `compilation-filter-hook'."
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        (when ag-highlight-search
          ;; Highlight ag matches and delete marking sequences.
          (while (re-search-forward "\033\\[30;43m\\(.*?\\)\033\\[0m\033\\[K" end 1)
            (replace-match (propertize (match-string 1)
                                       'face nil 'font-lock-face 'ag-match-face)
                           t t)))
        ;; Add marker at start of line for files. This is used by the match
        ;; in `compilation-error-regexp-alist' to extract the file name.
        (when ag-group-matches
          (goto-char beg)
          (while (re-search-forward "\033\\[1;32m\\(.*\\)\033\\[0m\033\\[K" end 1)
            (replace-match
             (concat "File: " (propertize (match-string 1) 'face nil 'font-lock-face
                                          'compilation-info))
             t t)))
        ;; Delete all remaining escape sequences
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[mK]" end 1)
          (replace-match "" t t))))))

(defun ag/get-supported-types ()
  "Query the ag executable for which file types it recognises."
  (let* ((ag-output (shell-command-to-string (format "%s --list-file-types" ag-executable)))
         (lines (-map #'s-trim (s-lines ag-output)))
         (types (--keep (when (s-starts-with? "--" it) (s-chop-prefix "--" it )) lines))
         (extensions (--map (s-split "  " it) (--filter (s-starts-with? "." it) lines))))
    (-zip types extensions)))

(defun ag/read-file-type ()
  "Prompt the user for a known file type, or let them specify a PCRE regex."
  (let* ((all-types-with-extensions (ag/get-supported-types))
         (all-types (mapcar 'car all-types-with-extensions))
         (file-type
          (completing-read "Select file type: "
                           (append '("custom (provide a PCRE regex)") all-types)))
         (file-type-extensions
          (cdr (assoc file-type all-types-with-extensions))))
    (if file-type-extensions
        (list :file-type file-type)
      (list :file-regex
            (read-from-minibuffer "Filenames which match PCRE: "
                                  (ag/buffer-extension-regex))))))

(provide 'ag)
;;; ag.el ends here
