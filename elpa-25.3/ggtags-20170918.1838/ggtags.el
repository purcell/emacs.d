;;; ggtags.el --- emacs frontend to GNU Global source code tagging system  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2017  Free Software Foundation, Inc.

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.8.13
;; Package-Version: 20170918.1838
;; Keywords: tools, convenience
;; Created: 2013-01-29
;; URL: https://github.com/leoliu/ggtags
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A package to integrate GNU Global source code tagging system
;; (http://www.gnu.org/software/global) with Emacs.
;;
;; Usage:
;;
;; `ggtags' is similar to the standard `etags' package. These keys
;; `M-.', `M-,' and `C-M-.' should work as expected in `ggtags-mode'.
;; See the README in https://github.com/leoliu/ggtags for more
;; details.
;;
;; All commands are available from the `Ggtags' menu in `ggtags-mode'.

;;; NEWS 0.8.12 (2016-10-02):

;; - Work with Emacs 25
;; - `ggtags-navigation-mode' is more discreet in displaying lighter
;;   when `ggtags-enable-navigation-keys' is set to nil
;; - `ggtags-make-project' tries harder to find TAG files respecting
;;   `GTAGSDBPATH'
;; - Fix error "Selecting deleted buffer"
;;   https://github.com/leoliu/ggtags/issues/89
;;
;; See full NEWS on https://github.com/leoliu/ggtags#news

;;; Code:

(eval-when-compile
  (require 'url-parse))

(require 'cl-lib)
(require 'ewoc)
(require 'compile)
(require 'etags)
(require 'tabulated-list)               ;preloaded since 24.3

(eval-when-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      (list 'set (list 'make-local-variable (list 'quote var)) val)))

  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      (declare (debug defvar) (doc-string 3))
      (list 'progn (list 'defvar var val docstring)
            (list 'make-variable-buffer-local (list 'quote var)))))

  (or (fboundp 'add-function) (defmacro add-function (&rest _))) ;24.4
  (or (fboundp 'remove-function) (defmacro remove-function (&rest _)))

  (defmacro ignore-errors-unless-debug (&rest body)
    "Ignore all errors while executing BODY unless debug is on."
    (declare (debug t) (indent 0))
    `(condition-case-unless-debug nil (progn ,@body) (error nil)))

  (defmacro with-display-buffer-no-window (&rest body)
    (declare (debug t) (indent 0))
    ;; See http://debbugs.gnu.org/13594
    `(let ((display-buffer-overriding-action
            (if (and ggtags-auto-jump-to-match
                     ;; Appeared in emacs 24.4.
                     (fboundp 'display-buffer-no-window))
                (list #'display-buffer-no-window)
              display-buffer-overriding-action)))
       ,@body)))

(eval-and-compile
  (or (fboundp 'user-error)             ;24.3
      (defalias 'user-error 'error))
  (or (fboundp 'read-only-mode)         ;24.3
      (defalias 'read-only-mode 'toggle-read-only))
  (or (fboundp 'register-read-with-preview) ;24.4
      (defalias 'register-read-with-preview 'read-char))
  (or (boundp 'xref--marker-ring)       ;25.1
      (defvaralias 'xref--marker-ring 'find-tag-marker-ring))
  (or (fboundp 'xref-push-marker-stack) ;25.1
      (defun xref-push-marker-stack (&optional m)
        (ring-insert xref--marker-ring (or m (point-marker)))))
  (or (fboundp 'xref-pop-marker-stack)
      (defalias 'xref-pop-marker-stack 'pop-tag-mark)))

(defgroup ggtags nil
  "GNU Global source code tagging system."
  :group 'tools)

(defface ggtags-highlight '((t (:underline t)))
  "Face used to highlight a valid tag at point."
  :group 'ggtags)

(defface ggtags-global-line '((t (:inherit secondary-selection)))
  "Face used to highlight matched line in Global buffer."
  :group 'ggtags)

(defcustom ggtags-executable-directory nil
  "If non-nil the directory to search global executables."
  :type '(choice (const :tag "Unset" nil) directory)
  :risky t
  :group 'ggtags)

(defcustom ggtags-oversize-limit (* 10 1024 1024)
  "The over size limit for the  GTAGS file.
When the size of the GTAGS file is below this limit, ggtags
always maintains up-to-date tags for the whole source tree by
running `global -u'. For projects with GTAGS larger than this
limit, only files edited in Ggtags mode are updated (via `global
--single-update')."
  :safe 'numberp
  :type '(choice (const :tag "None" nil)
                 (const :tag "Always" t)
                 number)
  :group 'ggtags)

(defcustom ggtags-include-pattern
  '("^\\s-*#\\s-*\\(?:include\\|import\\)\\s-*[\"<]\\(?:[./]*\\)?\\(.*?\\)[\">]" . 1)
  "Pattern used to detect #include files.
Value can be (REGEXP . SUB) or a function with no arguments.
REGEXP should match from the beginning of line."
  :type '(choice (const :tag "Disable" nil)
                 (cons regexp integer)
                 function)
  :safe 'stringp
  :group 'ggtags)

;; See also: http://article.gmane.org/gmane.comp.gnu.global.bugs/1751
(defcustom ggtags-use-project-gtagsconf t
  "Non-nil to use GTAGSCONF file found at project root.
File .globalrc and gtags.conf are checked in order.

Note: GNU Global v6.2.13 has the feature of using gtags.conf at
project root. Setting this variable to nil doesn't disable this
feature."
  :safe 'booleanp
  :type 'boolean
  :group 'ggtags)

(defcustom ggtags-project-duration 600
  "Seconds to keep information of a project in memory."
  :type 'number
  :group 'ggtags)

(defcustom ggtags-process-environment nil
  "Similar to `process-environment' with higher precedence.
Elements are run through `substitute-env-vars' before use.
GTAGSROOT will always be expanded to current project root
directory. This is intended for project-wise ggtags-specific
process environment settings. Note on remote hosts (e.g. tramp)
directory local variables is not enabled by default per
`enable-remote-dir-locals' (which see)."
  :safe 'ggtags-list-of-string-p
  :type '(repeat string)
  :group 'ggtags)

(defcustom ggtags-auto-jump-to-match 'history
  "Strategy on how to jump to match: nil, first or history.

    nil: never automatically jump to any match;
  first: jump to the first match;
history: jump to the match stored in search history."
  :type '(choice (const :tag "First match" first)
                 (const :tag "Search History" history)
                 (const :tag "Never" nil))
  :group 'ggtags)

(defcustom ggtags-global-window-height 8 ; ggtags-global-mode
  "Number of lines for the *ggtags-global* popup window.
If nil, use Emacs default."
  :type '(choice (const :tag "Default" nil) integer)
  :group 'ggtags)

(defcustom ggtags-global-abbreviate-filename 40
  "Non-nil to display file names abbreviated e.g. \"/u/b/env\".
If an integer abbreviate only names longer than that number."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Always" t)
                 integer)
  :group 'ggtags)

(defcustom ggtags-split-window-function split-window-preferred-function
  "A function to control how ggtags pops up the auxiliary window."
  :type 'function
  :group 'ggtags)

(defcustom ggtags-use-idutils (and (executable-find "mkid") t)
  "Non-nil to also generate the idutils DB."
  :type 'boolean
  :group 'ggtags)

(defcustom ggtags-use-sqlite3 nil
  "Use sqlite3 for storage instead of Berkeley DB.
This feature requires GNU Global 6.3.3+ and is ignored if `gtags'
isn't built with sqlite3 support."
  :type 'boolean
  :safe 'booleanp
  :group 'ggtags)

(defcustom ggtags-sort-by-nearness nil
  "Sort tags by nearness to current directory.
GNU Global 6.5+ required."
  :type 'boolean
  :safe #'booleanp
  :group 'ggtags)

(defcustom ggtags-update-on-save t
  "Non-nil to update tags for current buffer on saving."
  ;; It is reported that `global --single-update' can be slow in sshfs
  ;; directories. See https://github.com/leoliu/ggtags/issues/85.
  :safe #'booleanp
  :type 'boolean
  :group 'ggtags)

(defcustom ggtags-global-output-format 'grep
  "Global output format: path, ctags, ctags-x, grep or cscope."
  :type '(choice (const path)
                 (const ctags)
                 (const ctags-x)
                 (const grep)
                 (const cscope))
  :group 'ggtags)

(defcustom ggtags-global-use-color t
  "Non-nil to use color in output if supported by Global.
Note: processing colored output takes noticeable time
particularly when the output is large."
  :type 'boolean
  :safe 'booleanp
  :group 'ggtags)

(defcustom ggtags-global-ignore-case nil
  "Non-nil if Global should ignore case in the search pattern."
  :safe 'booleanp
  :type 'boolean
  :group 'ggtags)

(defcustom ggtags-global-treat-text nil
  "Non-nil if Global should include matches from text files.
This affects `ggtags-find-file' and `ggtags-grep'."
  :safe 'booleanp
  :type 'boolean
  :group 'ggtags)

;; See also https://github.com/leoliu/ggtags/issues/52
(defcustom ggtags-global-search-libpath-for-reference t
  "If non-nil global will search GTAGSLIBPATH for references.
Search is only continued in GTAGSLIBPATH if it finds no matches
in current project."
  :safe 'booleanp
  :type 'boolean
  :group 'ggtags)

(defcustom ggtags-global-large-output 1000
  "Number of lines in the Global buffer to indicate large output."
  :type 'number
  :group 'ggtags)

(defcustom ggtags-global-history-length history-length
  "Maximum number of items to keep in `ggtags-global-search-history'."
  :type 'integer
  :group 'ggtags)

(defcustom ggtags-enable-navigation-keys t
  "If non-nil key bindings in `ggtags-navigation-map' are enabled."
  :safe 'booleanp
  :type 'boolean
  :group 'ggtags)

(defcustom ggtags-find-tag-hook nil
  "Hook run immediately after finding a tag."
  :options '(recenter reposition-window)
  :type 'hook
  :group 'ggtags)

(defcustom ggtags-get-definition-function #'ggtags-get-definition-default
  "Function called by `ggtags-show-definition' to get definition.
It is passed a list of definition candidates of the form:

 (TEXT NAME FILE LINE)

where TEXT is usually the source line of the definition.

The return value is passed to `ggtags-print-definition-function'."
  :type 'function
  :group 'ggtags)

(defcustom ggtags-print-definition-function
  (lambda (s) (ggtags-echo "%s" (or s "[definition not found]")))
  "Function used by `ggtags-show-definition' to print definition."
  :type 'function
  :group 'ggtags)

(defcustom ggtags-mode-sticky t
  "If non-nil enable Ggtags Mode in files visited."
  :safe 'booleanp
  :type 'boolean
  :group 'ggtags)

(defcustom ggtags-mode-prefix-key "\C-c"
  "Key binding used for `ggtags-mode-prefix-map'.
Users should change the value using `customize-variable' to
properly update `ggtags-mode-map'."
  :set (lambda (sym value)
         (when (bound-and-true-p ggtags-mode-map)
           (let ((old (and (boundp sym) (symbol-value sym))))
             (and old (define-key ggtags-mode-map old nil)))
           (and value
                (bound-and-true-p ggtags-mode-prefix-map)
                (define-key ggtags-mode-map value ggtags-mode-prefix-map)))
         (set-default sym value))
  :type 'key-sequence
  :group 'ggtags)

(defcustom ggtags-completing-read-function nil
  "Ggtags specific `completing-read-function' (which see).
Nil means using the value of `completing-read-function'."
  :type '(choice (const :tag "Use completing-read-function" nil)
                 function)
  :group 'ggtags)

(define-obsolete-variable-alias 'ggtags-highlight-tag-delay 'ggtags-highlight-tag
  "0.8.11")

(defcustom ggtags-highlight-tag 0.25
  "If non-nil time in seconds before highlighting tag at point.
Set to nil to disable tag highlighting."
  :set (lambda (sym value)
         (when (fboundp 'ggtags-setup-highlight-tag-at-point)
           (ggtags-setup-highlight-tag-at-point value))
         (set-default sym value))
  :type '(choice (const :tag "Disable" nil) number)
  :group 'ggtags)

(defcustom ggtags-bounds-of-tag-function (lambda ()
                                           (bounds-of-thing-at-point 'symbol))
  "Function to get the start and end positions of the tag at point."
  :type 'function
  :group 'ggtags)

;; Used by ggtags-global-mode
(defvar ggtags-global-error "match"
  "Stem of message to print when no matches are found.")

(defconst ggtags-bug-url "https://github.com/leoliu/ggtags/issues")

(defvar ggtags-global-last-buffer nil)

(defvar ggtags-global-continuation nil)

(defvar ggtags-current-tag-name nil)

(defvar ggtags-highlight-tag-overlay nil)

(defvar ggtags-highlight-tag-timer nil)

(defmacro ggtags-with-temp-message (message &rest body)
  (declare (debug t) (indent 1))
  (let ((init-time (make-symbol "-init-time-"))
        (tmp-msg (make-symbol "-tmp-msg-")))
    `(let ((,init-time (float-time))
           (,tmp-msg ,message))
       (with-temp-message ,tmp-msg
         (prog1 (progn ,@body)
           (message "%sdone (%.2fs)" ,(or tmp-msg "")
                    (- (float-time) ,init-time)))))))

(defmacro ggtags-delay-finish-functions (&rest body)
  "Delay running `compilation-finish-functions' until after BODY."
  (declare (indent 0) (debug t))
  (let ((saved (make-symbol "-saved-"))
        (exit-args (make-symbol "-exit-args-")))
    `(let ((,saved compilation-finish-functions)
           ,exit-args)
       (setq-local compilation-finish-functions nil)
       (add-hook 'compilation-finish-functions
                 (lambda (&rest args) (setq ,exit-args args))
                 nil t)
       (unwind-protect (progn ,@body)
         (setq-local compilation-finish-functions ,saved)
         (and ,exit-args (apply #'run-hook-with-args
                                'compilation-finish-functions ,exit-args))))))

(defmacro ggtags-ensure-global-buffer (&rest body)
  (declare (debug t) (indent 0))
  `(progn
     (or (and (buffer-live-p ggtags-global-last-buffer)
              (with-current-buffer ggtags-global-last-buffer
                (derived-mode-p 'ggtags-global-mode)))
         (error "No global buffer found"))
     (with-current-buffer ggtags-global-last-buffer ,@body)))

(defun ggtags-list-of-string-p (xs)
  "Return non-nil if XS is a list of strings."
  (cl-every #'stringp xs))

(defun ggtags-ensure-localname (file)
  (and file (or (file-remote-p file 'localname) file)))

(defun ggtags-echo (format-string &rest args)
  "Print formatted text to echo area."
  (let (message-log-max) (apply #'message format-string args)))

(defun ggtags-forward-to-line (line)
  "Move to line number LINE in current buffer."
  (cl-check-type line (integer 1))
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun ggtags-kill-window ()
  "Quit selected window and kill its buffer."
  (interactive)
  (quit-window t))

(defun ggtags-program-path (name)
  (if ggtags-executable-directory
      (expand-file-name name ggtags-executable-directory)
    name))

(defun ggtags-process-succeed-p (program &rest args)
  "Return non-nil if successfully running PROGRAM with ARGS."
  (let ((program (ggtags-program-path program)))
    (condition-case err
        (zerop (apply #'process-file program nil nil nil args))
      (error (message "`%s' failed: %s" program (error-message-string err))
             nil))))

(defun ggtags-process-string (program &rest args)
  (with-temp-buffer
    (let ((exit (apply #'process-file
                       (ggtags-program-path program) nil t nil args))
          (output (progn
                    (goto-char (point-max))
                    (skip-chars-backward " \t\n\r")
                    (buffer-substring (point-min) (point)))))
      (or (zerop exit)
          (error "`%s' non-zero exit: %s" program output))
      output)))

(defun ggtags-tag-at-point ()
  (pcase (funcall ggtags-bounds-of-tag-function)
    (`(,beg . ,end) (buffer-substring beg end))))

;;; Store for project info and settings

(defvar ggtags-projects (make-hash-table :size 7 :test #'equal))

(cl-defstruct (ggtags-project (:constructor ggtags-project--make)
                              (:copier nil)
                              (:type vector)
                              :named)
  root tag-size has-refs has-path-style has-color dirty-p mtime timestamp)

(defun ggtags-make-project (root)
  (cl-check-type root string)
  (let* ((default-directory (file-name-as-directory root))
         ;; NOTE: use of GTAGSDBPATH is not recommended. -- GLOBAL(1)
         ;; ROOT and DB can be different directories due to GTAGSDBPATH.
         (dbdir (concat (file-remote-p root)
                        (ggtags-process-string "global" "-p"))))
    (pcase (nthcdr 5 (file-attributes (expand-file-name "GTAGS" dbdir)))
      (`(,mtime ,_ ,tag-size . ,_)
       (let* ((rtags-size (nth 7 (file-attributes (expand-file-name "GRTAGS" dbdir))))
              (has-refs
               (when rtags-size
                 (and (or (> rtags-size (* 32 1024))
                          (with-demoted-errors "ggtags-make-project: %S"
                            (not (equal "" (ggtags-process-string "global" "-crs")))))
                      'has-refs)))
              ;; http://thread.gmane.org/gmane.comp.gnu.global.bugs/1518
              (has-path-style
               (and (ggtags-process-succeed-p "global" "--path-style" "shorter" "--help")
                    'has-path-style))
              ;; http://thread.gmane.org/gmane.comp.gnu.global.bugs/1542
              (has-color (and (ggtags-process-succeed-p "global" "--color" "--help")
                              'has-color)))
         (puthash default-directory
                  (ggtags-project--make :root default-directory
                                        :tag-size tag-size
                                        :has-refs has-refs
                                        :has-path-style has-path-style
                                        :has-color has-color
                                        :mtime (float-time mtime)
                                        :timestamp (float-time))
                  ggtags-projects))))))

(defun ggtags-project-expired-p (project)
  (or (< (ggtags-project-timestamp project) 0)
      (> (- (float-time)
            (ggtags-project-timestamp project))
         ggtags-project-duration)))

(defun ggtags-project-update-mtime-maybe (&optional project)
  "Update PROJECT's modtime and if current file is newer.
Value is new modtime if updated."
  (let ((project (or project (ggtags-find-project))))
    (when (and (ggtags-project-p project)
               (consp (visited-file-modtime))
               (> (float-time (visited-file-modtime))
                  (ggtags-project-mtime project)))
      (setf (ggtags-project-dirty-p project) t)
      (setf (ggtags-project-mtime project)
            (float-time (visited-file-modtime))))))

(defun ggtags-project-oversize-p (&optional project)
  (pcase ggtags-oversize-limit
    (`nil nil)
    (`t t)
    (size (let ((project (or project (ggtags-find-project))))
            (and project (> (ggtags-project-tag-size project) size))))))

(defvar-local ggtags-last-default-directory nil)
(defvar-local ggtags-project-root 'unset
  "Internal variable for project root directory.")

;;;###autoload
(defun ggtags-find-project ()
  ;; See https://github.com/leoliu/ggtags/issues/42
  ;;
  ;; It is unsafe to cache `ggtags-project-root' in non-file buffers
  ;; whose `default-directory' can often change.
  (unless (equal ggtags-last-default-directory default-directory)
    (kill-local-variable 'ggtags-project-root))
  (let ((project (gethash ggtags-project-root ggtags-projects)))
    (if (ggtags-project-p project)
        (if (ggtags-project-expired-p project)
            (progn
              (remhash ggtags-project-root ggtags-projects)
              (ggtags-find-project))
          project)
      (setq ggtags-last-default-directory default-directory)
      (setq ggtags-project-root
            (or (ignore-errors
                  (file-name-as-directory
                   (concat (file-remote-p default-directory)
                           ;; Resolves symbolic links
                           (ggtags-process-string "global" "-pr"))))
                ;; 'global -pr' resolves symlinks before checking the
                ;; GTAGS file which could cause issues such as
                ;; https://github.com/leoliu/ggtags/issues/22, so
                ;; let's help it out.
                ;;
                ;; Note: `locate-dominating-file' doesn't accept
                ;; function for NAME before 24.3.
                (let ((dir (locate-dominating-file default-directory "GTAGS")))
                  ;; `file-truename' may strip the trailing '/' on
                  ;; remote hosts, see http://debbugs.gnu.org/16851
                  (and dir (file-regular-p (expand-file-name "GTAGS" dir))
                       (file-name-as-directory (file-truename dir))))))
      (when ggtags-project-root
        (if (gethash ggtags-project-root ggtags-projects)
            (ggtags-find-project)
          (ggtags-make-project ggtags-project-root))))))

(defun ggtags-current-project-root ()
  (and (ggtags-find-project)
       (ggtags-project-root (ggtags-find-project))))

(defun ggtags-check-project ()
  (or (ggtags-find-project) (error "File GTAGS not found")))

(defun ggtags-ensure-project ()
  (or (ggtags-find-project)
      (progn (call-interactively #'ggtags-create-tags)
             ;; Need checking because `ggtags-create-tags' can create
             ;; tags in any directory.
             (ggtags-check-project))))

(defvar delete-trailing-lines)          ;new in 24.3

(defun ggtags-save-project-settings (&optional noconfirm)
  "Save Gnu Global's specific environment variables."
  (interactive "P")
  (ggtags-check-project)
  (let* ((inhibit-read-only t)          ; for `add-dir-local-variable'
         (default-directory (ggtags-current-project-root))
         ;; Not using `ggtags-with-current-project' to preserve
         ;; environment variables that may be present in
         ;; `ggtags-process-environment'.
         (process-environment
          (append ggtags-process-environment
                  process-environment
                  (and (not (ggtags-project-has-refs (ggtags-find-project)))
                       (list "GTAGSLABEL=ctags"))))
         (envlist (delete-dups
                   (cl-loop for x in process-environment
                            when (string-match
                                  "^\\(GTAGS[^=\n]*\\|MAKEOBJDIRPREFIX\\)=" x)
                            ;; May have duplicates thus `delete-dups'.
                            collect (concat (match-string 1 x)
                                            "="
                                            (getenv (match-string 1 x))))))
         (help-form (format "y: save\nn: don't save\n=: diff\n?: help\n")))
    (add-dir-local-variable nil 'ggtags-process-environment envlist)
    ;; Remove trailing newlines by `add-dir-local-variable'.
    (let ((delete-trailing-lines t)) (delete-trailing-whitespace))
    (or noconfirm
        (while (pcase (read-char-choice
                       (format "Save `%s'? (y/n/=/?) " buffer-file-name)
                       '(?y ?n ?= ??))
                 ;; ` required for 24.1 and 24.2
                 (`?n (user-error "Aborted"))
                 (`?y nil)
                 (`?= (diff-buffer-with-file) 'loop)
                 (`?? (help-form-show) 'loop))))
    (save-buffer)
    (kill-buffer)))

(defun ggtags-toggle-project-read-only ()
  (interactive)
  (ggtags-check-project)
  (let ((inhibit-read-only t)           ; for `add-dir-local-variable'
        (val (not buffer-read-only))
        (default-directory (ggtags-current-project-root)))
    (add-dir-local-variable nil 'buffer-read-only val)
    (save-buffer)
    (kill-buffer)
    (when buffer-file-name
      (read-only-mode (if val +1 -1)))
    (when (called-interactively-p 'interactive)
      (message "Project read-only-mode is %s" (if val "on" "off")))
    val))

(defun ggtags-visit-project-root (&optional project)
  "Visit the root directory of (current) PROJECT in dired.
When called with a prefix \\[universal-argument], choose from past projects."
  (interactive (list (and current-prefix-arg
                          (completing-read "Project: " ggtags-projects))))
  (dired (cl-typecase project
           (string project)
           (ggtags-project (ggtags-project-root project))
           (t (ggtags-ensure-project) (ggtags-current-project-root)))))

(defmacro ggtags-with-current-project (&rest body)
  "Eval BODY in current project's `process-environment'."
  (declare (debug t) (indent 0))
  (let ((gtagsroot (make-symbol "-gtagsroot-"))
        (root (make-symbol "-ggtags-project-root-")))
    `(let* ((,root ggtags-project-root)
            (,gtagsroot (when (ggtags-find-project)
                          (ggtags-ensure-localname
                           (directory-file-name (ggtags-current-project-root)))))
            (process-environment
             (append (let ((process-environment (copy-sequence process-environment)))
                       (and ,gtagsroot (setenv "GTAGSROOT" ,gtagsroot))
                       (mapcar #'substitute-env-vars ggtags-process-environment))
                     process-environment
                     (and ,gtagsroot (list (concat "GTAGSROOT=" ,gtagsroot)))
                     (and (ggtags-find-project)
                          (not (ggtags-project-has-refs (ggtags-find-project)))
                          (list "GTAGSLABEL=ctags")))))
       (unwind-protect (save-current-buffer ,@body)
         (setq ggtags-project-root ,root)))))

(defun ggtags-get-libpath ()
  (let ((path (ggtags-with-current-project (getenv "GTAGSLIBPATH"))))
    (and path (mapcar (apply-partially #'concat (file-remote-p default-directory))
                      (split-string path (regexp-quote path-separator) t)))))

(defun ggtags-project-relative-file (file)
  "Get file name relative to current project root."
  (ggtags-check-project)
  (if (file-name-absolute-p file)
      (file-relative-name file (if (string-prefix-p (ggtags-current-project-root)
                                                    file)
                                   (ggtags-current-project-root)
                                 (locate-dominating-file file "GTAGS")))
    file))

(defun ggtags-project-file-p (file)
  "Return non-nil if FILE is part of current project."
  (when (ggtags-find-project)
    (with-temp-buffer
      (ggtags-with-current-project
        ;; NOTE: `process-file' requires all files in ARGS be relative
        ;; to `default-directory'; see its doc string for details.
        (let ((default-directory (ggtags-current-project-root)))
          (process-file (ggtags-program-path "global") nil t nil
                        "-vP" (concat "^" (ggtags-project-relative-file file) "$"))))
      (goto-char (point-min))
      (not (re-search-forward "^file not found" nil t)))))

(defun ggtags-invalidate-buffer-project-root (root)
  (mapc (lambda (buf)
          (with-current-buffer buf
            (and buffer-file-truename
                 (string-prefix-p root buffer-file-truename)
                 (kill-local-variable 'ggtags-project-root))))
        (buffer-list)))

(defun ggtags-create-tags (root)
  "Create tag files (e.g. GTAGS) in directory ROOT.
If file .globalrc or gtags.conf exists in ROOT, it will be used
as configuration file per `ggtags-use-project-gtagsconf'.

If file gtags.files exists in ROOT, it should be a list of source
files to index, which can be used to speed gtags up in large
source trees. See Info node `(global)gtags' for details."
  (interactive "DRoot directory: ")
  (let ((process-environment (copy-sequence process-environment)))
    (when (zerop (length root)) (error "No root directory provided"))
    (setenv "GTAGSROOT" (ggtags-ensure-localname
                         (expand-file-name
                          (directory-file-name (file-name-as-directory root)))))
    (ggtags-with-current-project
      (let ((conf (and ggtags-use-project-gtagsconf
                       (cl-loop for name in '(".globalrc" "gtags.conf")
                                for full = (expand-file-name name root)
                                thereis (and (file-exists-p full) full)))))
        (unless (or conf (getenv "GTAGSLABEL")
                    (not (yes-or-no-p "Use `ctags' backend? ")))
          (setenv "GTAGSLABEL" "ctags"))
        (ggtags-with-temp-message "`gtags' in progress..."
          (let ((default-directory (file-name-as-directory root))
                (args (cl-remove-if
                       #'null
                       (list (and ggtags-use-idutils "--idutils")
                             (and ggtags-use-sqlite3
                                  (ggtags-process-succeed-p "gtags" "--sqlite3" "--help")
                                  "--sqlite3")
                             (and conf "--gtagsconf")
                             (and conf (ggtags-ensure-localname conf))))))
            (condition-case err
                (apply #'ggtags-process-string "gtags" args)
              (error (if (and ggtags-use-idutils
                              (stringp (cadr err))
                              (string-match-p "mkid not found" (cadr err)))
                         ;; Retry without mkid
                         (apply #'ggtags-process-string
                                "gtags" (cl-remove "--idutils" args))
                       (signal (car err) (cdr err)))))))))
    (ggtags-invalidate-buffer-project-root (file-truename root))
    (message "GTAGS generated in `%s'" root)
    root))

(defun ggtags-explain-tags ()
  "Explain how each file is indexed in current project."
  (interactive (ignore (ggtags-check-project)
                       (or (ggtags-process-succeed-p "gtags" "--explain" "--help")
                           (user-error "Global 6.4+ required"))))
  (ggtags-check-project)
  (ggtags-with-current-project
    (let ((default-directory (ggtags-current-project-root)))
      (compilation-start (concat (ggtags-program-path "gtags") " --explain")))))

(defun ggtags-update-tags (&optional force)
  "Update GNU Global tag database.
Do nothing if GTAGS exceeds the oversize limit unless FORCE.

When called interactively on large (per `ggtags-oversize-limit')
projects, the update process runs in the background without
blocking emacs."
  (interactive (progn
                 (ggtags-check-project)
                 ;; Mark project info expired.
                 (setf (ggtags-project-timestamp (ggtags-find-project)) -1)
                 (list 'interactive)))
  (cond ((and (eq force 'interactive) (ggtags-project-oversize-p))
         (ggtags-with-current-project
           (with-display-buffer-no-window
             (with-current-buffer (compilation-start "global -u")
               ;; A hack to fool compilation mode to display `global
               ;; -u finished' on finish.
               (setq mode-name "global -u")
               (add-hook 'compilation-finish-functions
                         #'ggtags-update-tags-finish nil t)))))
        ((or force (and (ggtags-find-project)
                        (not (ggtags-project-oversize-p))
                        (ggtags-project-dirty-p (ggtags-find-project))))
         (ggtags-with-current-project
           (ggtags-with-temp-message "`global -u' in progress..."
             (ggtags-process-string "global" "-u")
             (ggtags-update-tags-finish))))))

(defun ggtags-update-tags-finish (&optional buf how)
  (if (and how buf (string-prefix-p "exited abnormally" how))
      (display-buffer buf)
    (setf (ggtags-project-dirty-p (ggtags-find-project)) nil)
    (setf (ggtags-project-mtime (ggtags-find-project)) (float-time))))

(defun ggtags-update-tags-single (file &optional nowait)
  ;; NOTE: NOWAIT is ignored if file is remote file; see
  ;; `tramp-sh-handle-process-file'.
  (cl-check-type file string)
  (let ((nowait (unless (file-remote-p file) nowait)))
    (ggtags-with-current-project
      ;; See comment in `ggtags-project-file-p'.
      (let ((default-directory (ggtags-current-project-root)))
        (process-file (ggtags-program-path "global") nil (and nowait 0) nil
                      "--single-update" (ggtags-project-relative-file file))))))

(defun ggtags-delete-tags ()
  "Delete file GTAGS, GRTAGS, GPATH, ID etc. generated by gtags."
  (interactive (ignore (ggtags-check-project)))
  (when (ggtags-current-project-root)
    (let* ((re (concat "\\`" (regexp-opt '("GPATH" "GRTAGS" "GTAGS" "ID")) "\\'"))
           (files (cl-remove-if-not
                   (lambda (file)
                     ;; Don't trust `directory-files'.
                     (let ((case-fold-search nil))
                       (string-match-p re (file-name-nondirectory file))))
                   (directory-files (ggtags-current-project-root) t re)))
           (buffer "*GTags File List*"))
      (or files (user-error "No tag files found"))
      (with-output-to-temp-buffer buffer
        (princ (mapconcat #'identity files "\n")))
      (let ((win (get-buffer-window buffer)))
        (unwind-protect
            (progn
              (fit-window-to-buffer win)
              (when (yes-or-no-p "Remove GNU Global tag files? ")
                (with-demoted-errors (mapc #'delete-file files))
                (remhash (ggtags-current-project-root) ggtags-projects)
                (and (overlayp ggtags-highlight-tag-overlay)
                     (delete-overlay ggtags-highlight-tag-overlay))))
          (when (window-live-p win)
            (quit-window t win)))))))

(defvar-local ggtags-completion-cache nil)

;; See global/libutil/char.c
;; (defconst ggtags-regexp-metachars "[][$()*+.?\\{}|^]")
(defvar ggtags-completion-flag "")      ;internal use

(defvar ggtags-completion-table
  (completion-table-dynamic
   (lambda (prefix)
     (let ((cache-key (concat prefix "$" ggtags-completion-flag)))
       (unless (equal cache-key (car ggtags-completion-cache))
         (setq ggtags-completion-cache
               (cons cache-key
                     (ignore-errors-unless-debug
                       ;; May throw global: only name char is allowed
                       ;; with -c option.
                       (ggtags-with-current-project
                         (split-string
                          (apply #'ggtags-process-string
                                 "global"
                                 (append (and completion-ignore-case '("--ignore-case"))
                                         ;; Note -c alone returns only definitions
                                         (list (concat "-c" ggtags-completion-flag) prefix)))
                          "\n" t)))))))
     (cdr ggtags-completion-cache))))

(defun ggtags-completion-at-point ()
  "A function for `completion-at-point-functions'."
  (pcase (funcall ggtags-bounds-of-tag-function)
    (`(,beg . ,end)
     (and (< beg end) (list beg end ggtags-completion-table)))))

(defun ggtags-read-tag (&optional type confirm prompt require-match default)
  (ggtags-ensure-project)
  (let ((default (or default (ggtags-tag-at-point)))
        (prompt (or prompt (capitalize (symbol-name (or type 'tag)))))
        (ggtags-completion-flag (pcase type
                                  (`(or nil definition) "T")
                                  (`symbol "s")
                                  (`reference "r")
                                  (`id "I")
                                  (`path "P")
                                  ((pred stringp) type)
                                  (_ ggtags-completion-flag))))
    (setq ggtags-current-tag-name
          (cond (confirm
                 (ggtags-update-tags)
                 (let ((completing-read-function
                        (or ggtags-completing-read-function
                            completing-read-function)))
                   (completing-read
                    (format (if default "%s (default %s): " "%s: ") prompt default)
                    ggtags-completion-table nil require-match nil nil default)))
                (default (substring-no-properties default))
                (t (ggtags-read-tag type t prompt require-match default))))))

(defun ggtags-sort-by-nearness-p ()
  (and ggtags-sort-by-nearness
       (ggtags-process-succeed-p "global" "--nearness" "--help")))

(defun ggtags-global-build-command (cmd &rest args)
  ;; CMD can be definition, reference, symbol, grep, idutils
  (let ((xs (append (list (shell-quote-argument (ggtags-program-path "global"))
                          "-v"
                          (format "--result=%s" ggtags-global-output-format)
                          (and ggtags-global-ignore-case "--ignore-case")
                          (and ggtags-global-use-color
                               (ggtags-find-project)
                               (ggtags-project-has-color (ggtags-find-project))
                               "--color=always")
                          (and (ggtags-sort-by-nearness-p) "--nearness")
                          (and (ggtags-find-project)
                               (ggtags-project-has-path-style (ggtags-find-project))
                               "--path-style=shorter")
                          (and ggtags-global-treat-text "--other")
                          (pcase cmd
                            ((pred stringp) cmd)
                            (`definition nil) ;-d not supported by Global 5.7.1
                            (`reference "--reference")
                            (`symbol "--symbol")
                            (`path "--path")
                            (`grep "--grep")
                            (`idutils "--idutils")))
                    args)))
    (mapconcat #'identity (delq nil xs) " ")))

;; Can be three values: nil, t and a marker; t means start marker has
;; been saved in the tag ring.
(defvar ggtags-global-start-marker nil)
(defvar ggtags-global-start-file nil)
(defvar ggtags-tag-ring-index nil)
(defvar ggtags-global-search-history nil)

(defvar ggtags-auto-jump-to-match-target nil)

(defvar-local ggtags-global-exit-info nil) ; (EXIT-STATUS COUNT DB)

(defun ggtags-global-save-start-marker ()
  (when (markerp ggtags-global-start-marker)
    (setq ggtags-tag-ring-index nil)
    (xref-push-marker-stack ggtags-global-start-marker)
    (setq ggtags-global-start-marker t)))

(defun ggtags-global-start (command &optional directory)
  (let* ((default-directory (or directory (ggtags-current-project-root)))
         (split-window-preferred-function ggtags-split-window-function)
         (env ggtags-process-environment))
    (unless (and (markerp ggtags-global-start-marker)
                 (marker-position ggtags-global-start-marker))
      (setq ggtags-global-start-marker (point-marker)))
    ;; Record the file name for `ggtags-navigation-start-file'.
    (setq ggtags-global-start-file buffer-file-name)
    (setq ggtags-auto-jump-to-match-target
          (nth 4 (assoc (ggtags-global-search-id command default-directory)
                        ggtags-global-search-history)))
    (ggtags-navigation-mode +1)
    (ggtags-update-tags)
    (ggtags-with-current-project
      (with-current-buffer (with-display-buffer-no-window
                             (compilation-start command 'ggtags-global-mode))
        (setq-local ggtags-process-environment env)
        (setq ggtags-global-last-buffer (current-buffer))))))

(defun ggtags-find-tag-continue ()
  (interactive)
  (ggtags-ensure-global-buffer
    (ggtags-navigation-mode +1)
    (let ((split-window-preferred-function ggtags-split-window-function))
      (ignore-errors (compilation-next-error 1))
      (compile-goto-error))))

(defun ggtags-find-tag (cmd &rest args)
  (ggtags-check-project)
  (ggtags-global-start (apply #'ggtags-global-build-command cmd args)
                       (and (ggtags-sort-by-nearness-p) default-directory)))

(defun ggtags-include-file ()
  "Calculate the include file based on `ggtags-include-pattern'."
  (pcase ggtags-include-pattern
    (`nil nil)
    ((pred functionp)
     (funcall ggtags-include-pattern))
    (`(,re . ,sub)
     (save-excursion
       (beginning-of-line)
       (and (looking-at re) (match-string sub))))
    (_ (warn "Invalid value for `ggtags-include-pattern': %s"
             ggtags-include-pattern)
       nil)))

;;;###autoload
(defun ggtags-find-tag-dwim (name &optional what)
  "Find NAME by context.
If point is at a definition tag, find references, and vice versa.
If point is at a line that matches `ggtags-include-pattern', find
the include file instead.

When called interactively with a prefix arg, always find
definition tags."
  (interactive
   (let ((include (and (not current-prefix-arg) (ggtags-include-file))))
     (ggtags-ensure-project)
     (if include (list include 'include)
       (list (ggtags-read-tag 'definition current-prefix-arg)
             (and current-prefix-arg 'definition)))))
  (ggtags-check-project)    ; For `ggtags-current-project-root' below.
  (cond
   ((eq what 'include)
    (ggtags-find-file name))
   ((or (eq what 'definition)
        (not buffer-file-name)
        (not (ggtags-project-has-refs (ggtags-find-project)))
        (not (ggtags-project-file-p buffer-file-name)))
    (ggtags-find-definition name))
   (t (ggtags-find-tag
       (format "--from-here=%d:%s"
               (line-number-at-pos)
               (shell-quote-argument
                ;; Note `ggtags-find-tag' may bind `default-directory'
                ;; to project root.
                (funcall (if (ggtags-sort-by-nearness-p)
                             #'file-relative-name #'ggtags-project-relative-file)
                         buffer-file-name)))
       "--" (shell-quote-argument name)))))

(defun ggtags-find-tag-mouse (event)
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (save-excursion
      (goto-char (posn-point (event-start event)))
      (call-interactively #'ggtags-find-tag-dwim))))

;; Another option for `M-.'.
(defun ggtags-find-definition (name)
  (interactive (list (ggtags-read-tag 'definition current-prefix-arg)))
  (ggtags-find-tag 'definition "--" (shell-quote-argument name)))

(defun ggtags-setup-libpath-search (type name)
  (pcase (and ggtags-global-search-libpath-for-reference
              (ggtags-get-libpath))
    ((and libs (guard libs))
     (cl-labels ((cont (buf how)
                   (pcase ggtags-global-exit-info
                     (`(0 0 ,_)
                      (with-temp-buffer
                        (setq default-directory
                              (file-name-as-directory (pop libs)))
                        (and libs (setq ggtags-global-continuation #'cont))
                        (if (ggtags-find-project)
                            (ggtags-find-tag type (shell-quote-argument name))
                          (cont buf how))))
                     (_ (ggtags-global-handle-exit buf how)))))
       (setq ggtags-global-continuation #'cont)))))

(defun ggtags-find-reference (name)
  (interactive (list (ggtags-read-tag 'reference current-prefix-arg)))
  (ggtags-setup-libpath-search 'reference name)
  (ggtags-find-tag 'reference "--" (shell-quote-argument name)))

(defun ggtags-find-other-symbol (name)
  "Find tag NAME that is a reference without a definition."
  (interactive (list (ggtags-read-tag 'symbol current-prefix-arg)))
  (ggtags-setup-libpath-search 'symbol name)
  (ggtags-find-tag 'symbol "--" (shell-quote-argument name)))

(defun ggtags-quote-pattern (pattern)
  (prin1-to-string (substring-no-properties pattern)))

(defun ggtags-idutils-query (pattern)
  (interactive (list (ggtags-read-tag 'id t)))
  (ggtags-find-tag 'idutils "--" (ggtags-quote-pattern pattern)))

(defun ggtags-grep (pattern &optional invert-match)
  "Grep for lines matching PATTERN.
Invert the match when called with a prefix arg \\[universal-argument]."
  (interactive (list (ggtags-read-tag 'definition 'confirm
                                      (if current-prefix-arg
                                          "Inverted grep pattern" "Grep pattern"))
                     current-prefix-arg))
  (ggtags-find-tag 'grep (and invert-match "--invert-match")
                   "--" (ggtags-quote-pattern pattern)))

(defun ggtags-find-file (pattern &optional invert-match)
  (interactive (list (ggtags-read-tag 'path 'confirm (if current-prefix-arg
                                                         "Inverted path pattern"
                                                       "Path pattern")
                                      nil (thing-at-point 'filename))
                     current-prefix-arg))
  (let ((ggtags-global-output-format 'path))
    (ggtags-find-tag 'path (and invert-match "--invert-match")
                     "--" (ggtags-quote-pattern pattern))))

;; Note: Coloured output requested in http://goo.gl/Y9IcX and appeared
;; in global v6.2.12.
(defun ggtags-find-tag-regexp (regexp directory)
  "List tags matching REGEXP in DIRECTORY (default to project root).
When called interactively with a prefix, ask for the directory."
  (interactive
   (progn
     (ggtags-check-project)
     (list (ggtags-read-tag "" t "POSIX regexp")
           (if current-prefix-arg
               (read-directory-name "Directory: " nil nil t)
             (ggtags-current-project-root)))))
  (ggtags-check-project)
  (ggtags-global-start
   (ggtags-global-build-command nil nil "-l" "--" (ggtags-quote-pattern regexp))
   (file-name-as-directory directory)))

(defvar ggtags-navigation-mode)

(defun ggtags-foreach-file (fn)
  "Invoke FN with each file found.
FN is invoked while *ggtags-global* buffer is current."
  (ggtags-ensure-global-buffer
    (save-excursion
      (goto-char (point-min))
      (while (with-demoted-errors "compilation-next-error: %S"
               (compilation-next-error 1 'file)
               t)
        (funcall fn (caar
                     (compilation--loc->file-struct
                      (compilation--message->loc
                       (get-text-property (point) 'compilation-message)))))))))

(defun ggtags-query-replace (from to &optional delimited)
  "Query replace FROM with TO on files in the Global buffer.
If not in navigation mode, do a grep on FROM first.

Note: the regular expression FROM must be supported by both
Global and Emacs."
  (interactive
   ;; Note: in 24.4 query-replace-read-args returns a list of 4 elements.
   (let ((args (query-replace-read-args "Query replace (regexp)" t t)))
     (list (nth 0 args) (nth 1 args) (nth 2 args))))
  (unless ggtags-navigation-mode
    (let ((ggtags-auto-jump-to-match nil))
      (ggtags-grep from)))
  (let ((file-form
         '(let ((files))
            (ggtags-ensure-global-buffer
              (ggtags-with-temp-message "Waiting for Grep to finish..."
                (while (get-buffer-process (current-buffer))
                  (sit-for 0.2)))
              (ggtags-foreach-file
               (lambda (file) (push (expand-file-name file) files))))
            (ggtags-navigation-mode -1)
            (nreverse files))))
    (tags-query-replace from to delimited file-form)))

(defun ggtags-global-normalise-command (cmd)
  (if (string-match
       (concat (regexp-quote (ggtags-global-build-command nil)) "\\s-*")
       cmd)
      (substring-no-properties cmd (match-end 0))
    cmd))

(defun ggtags-global-search-id (cmd directory)
  (sha1 (concat directory (make-string 1 0)
                (ggtags-global-normalise-command cmd))))

(defun ggtags-global-current-search ()
  ;; CMD DIR ENV LINE TEXT
  (ggtags-ensure-global-buffer
    (list (ggtags-global-normalise-command (car compilation-arguments))
          default-directory
          ggtags-process-environment
          (line-number-at-pos)
          (buffer-substring-no-properties
           (line-beginning-position) (line-end-position)))))

(defun ggtags-global-rerun-search (data)
  (pcase data
    (`(,cmd ,dir ,env ,line ,_text)
     (with-current-buffer (let ((ggtags-auto-jump-to-match nil)
                                ;; Switch current project to DIR.
                                (default-directory dir)
                                (ggtags-project-root dir)
                                (ggtags-process-environment env))
                            (ggtags-global-start
                             (ggtags-global-build-command cmd) dir))
       (add-hook 'compilation-finish-functions
                 (lambda (buf _msg)
                   (with-current-buffer buf
                     (ggtags-forward-to-line line)
                     (compile-goto-error)))
                 nil t)))))

(defvar-local ggtags-global-search-ewoc nil)
(defvar ggtags-view-search-history-last nil)

(defvar ggtags-view-search-history-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "p" 'ggtags-view-search-history-prev)
    (define-key m "\M-p" 'ggtags-view-search-history-prev)
    (define-key m "n" 'ggtags-view-search-history-next)
    (define-key m "\M-n" 'ggtags-view-search-history-next)
    (define-key m "\C-k" 'ggtags-view-search-history-kill)
    (define-key m [remap yank] (lambda (&optional arg) (interactive "P") (yank arg)))
    (define-key m "\C-c\C-c" 'ggtags-view-search-history-update)
    (define-key m "r" 'ggtags-save-to-register)
    (define-key m "\r" 'ggtags-view-search-history-action)
    (define-key m "q" 'ggtags-kill-window)
    m))

(defun ggtags-view-search-history-remember ()
  (setq ggtags-view-search-history-last
        (pcase (ewoc-locate ggtags-global-search-ewoc)
          (`nil nil)
          (node (ewoc-data node)))))

(defun ggtags-view-search-history-next (&optional arg)
  (interactive "p")
  (let ((arg (or arg 1)))
    (prog1 (funcall (if (cl-minusp arg) #'ewoc-goto-prev #'ewoc-goto-next)
                    ggtags-global-search-ewoc (abs arg))
      (ggtags-view-search-history-remember))))

(defun ggtags-view-search-history-prev (&optional arg)
  (interactive "p")
  (ggtags-view-search-history-next (- (or arg 1))))

(defun ggtags-view-search-history-kill (&optional append)
  (interactive "P")
  (let* ((node (or (ewoc-locate ggtags-global-search-ewoc)
                   (user-error "No node at point")))
         (next (ewoc-next ggtags-global-search-ewoc node))
         (text (filter-buffer-substring (ewoc-location node)
                                        (if next (ewoc-location next)
                                          (point-max)))))
    (put-text-property
     0 (length text) 'yank-handler
     (list (lambda (arg)
             (if (not ggtags-global-search-ewoc)
                 (insert (car arg))
               (let* ((inhibit-read-only t)
                      (node (unless (looking-at-p "[ \t\n]*\\'")
                              (ewoc-locate ggtags-global-search-ewoc))))
                 (if node
                     (ewoc-enter-before ggtags-global-search-ewoc
                                        node (cadr arg))
                   (ewoc-enter-last ggtags-global-search-ewoc (cadr arg)))
                 (setq ggtags-view-search-history-last (cadr arg)))))
           (list text (ewoc-data node)))
     text)
    (if append (kill-append text nil)
      (kill-new text))
    (let ((inhibit-read-only t))
      (ewoc-delete ggtags-global-search-ewoc node))))

(defun ggtags-view-search-history-update (&optional noconfirm)
  "Update `ggtags-global-search-history' to current buffer."
  (interactive "P")
  (when (and (buffer-modified-p)
             (or noconfirm
                 (yes-or-no-p "Modify `ggtags-global-search-history'?")))
    (setq ggtags-global-search-history
          (ewoc-collect ggtags-global-search-ewoc #'identity))
    (set-buffer-modified-p nil)))

(defun ggtags-view-search-history-action ()
  (interactive)
  (let ((data (ewoc-data (or (ewoc-locate ggtags-global-search-ewoc)
                             (user-error "No search at point")))))
    (ggtags-view-search-history-remember)
    (quit-window t)
    (ggtags-global-rerun-search (cdr data))))

(defvar bookmark-make-record-function)

(define-derived-mode ggtags-view-search-history-mode special-mode "SearchHist"
  "Major mode for viewing search history."
  :group 'ggtags
  (setq-local ggtags-enable-navigation-keys nil)
  (setq-local bookmark-make-record-function #'ggtags-make-bookmark-record)
  (setq truncate-lines t)
  (add-hook 'kill-buffer-hook #'ggtags-view-search-history-update nil t))

(defun ggtags-view-search-history-restore-last ()
  (when ggtags-view-search-history-last
    (cl-loop for n = (ewoc-nth ggtags-global-search-ewoc 0)
             then (ewoc-next ggtags-global-search-ewoc n)
             while n when (eq (ewoc-data n)
                              ggtags-view-search-history-last)
             do (progn (goto-char (ewoc-location n)) (cl-return t)))))

(defun ggtags-view-search-history ()
  "Pop to a buffer to view or re-run past searches.

\\{ggtags-view-search-history-mode-map}"
  (interactive)
  (or ggtags-global-search-history (user-error "No search history"))
  (let ((split-window-preferred-function ggtags-split-window-function)
        (inhibit-read-only t))
    (pop-to-buffer "*Ggtags Search History*")
    (erase-buffer)
    (ggtags-view-search-history-mode)
    (cl-labels ((prop (s)
                  (propertize s 'face 'minibuffer-prompt))
                (prop-tag (cmd)
                  (with-temp-buffer
                    (insert cmd)
                    (forward-sexp -1)
                    (if (eobp)
                        cmd
                      (put-text-property (point) (point-max)
                                         'face font-lock-constant-face)
                      (buffer-string))))
                (pp (data)
                  (pcase data
                    (`(,_id ,cmd ,dir ,_env ,line ,text)
                     (insert (prop " cmd: ") (prop-tag cmd) "\n"
                             (prop " dir: ") dir "\n"
                             (prop "line: ") (number-to-string line) "\n"
                             (prop "text: ") text "\n"
                             (propertize (make-string 32 ?-) 'face 'shadow))))))
      (setq ggtags-global-search-ewoc
            (ewoc-create #'pp "Global search history keys:  n:next  p:prev  r:register  RET:choose\n")))
    (dolist (data ggtags-global-search-history)
      (ewoc-enter-last ggtags-global-search-ewoc data))
    (ggtags-view-search-history-restore-last)
    (set-buffer-modified-p nil)
    (fit-window-to-buffer nil (floor (frame-height) 2))))

(defun ggtags-save-to-register (r)
  "Save current search session to register R.
Use \\[jump-to-register] to restore the search session."
  (interactive (list (register-read-with-preview "Save search to register: ")))
  (cl-labels ((prn (data)
                (pcase data
                  (`(,command ,root ,_env ,line ,_)
                   (princ (format "a ggtags search session `%s' in directory `%s' at line %d."
                                  command root line))))))
    (set-register r (registerv-make
                     (if ggtags-global-search-ewoc
                         (cdr (ewoc-data (ewoc-locate ggtags-global-search-ewoc)))
                       (ggtags-global-current-search))
                     :jump-func #'ggtags-global-rerun-search
                     :print-func #'prn))))

(defun ggtags-make-bookmark-record ()
  `(,(and ggtags-current-tag-name (format "*ggtags %s*" ggtags-current-tag-name))
    (ggtags-search . ,(if ggtags-global-search-ewoc
                          (cdr (ewoc-data (ewoc-locate ggtags-global-search-ewoc)))
                        (ggtags-global-current-search)))
    (handler . ggtags-bookmark-jump)))

(declare-function bookmark-prop-get "bookmark")

(defun ggtags-bookmark-jump (bmk)
  (ggtags-global-rerun-search (bookmark-prop-get bmk 'ggtags-search)))

(defun ggtags-browse-file-as-hypertext (file line)
  "Browse FILE in hypertext (HTML) form."
  (interactive (if (or current-prefix-arg (not buffer-file-name))
                   (list (read-file-name "Browse file: " nil nil t)
                         (read-number "Line: " 1))
                 (list buffer-file-name (line-number-at-pos))))
  (cl-check-type line (integer 1))
  (or (and file (file-exists-p file)) (error "File `%s' doesn't exist" file))
  (ggtags-check-project)
  (or (file-exists-p (expand-file-name "HTML" (ggtags-current-project-root)))
      (if (yes-or-no-p "No hypertext form exists; run htags? ")
          (let ((default-directory (ggtags-current-project-root)))
            (ggtags-with-current-project (ggtags-process-string "htags")))
        (user-error "Aborted")))
  (let ((url (ggtags-process-string "gozilla" "-p" (format "+%d" line)
                                    (file-relative-name file))))
    (or (equal (file-name-extension
                (url-filename (url-generic-parse-url url))) "html")
        (user-error "No hypertext form for `%s'" file))
    (when (called-interactively-p 'interactive)
      (message "Browsing %s" url))
    (browse-url url)))

(defun ggtags-next-mark (&optional arg)
  "Move to the next (newer) mark in the tag marker ring."
  (interactive)
  (and (ring-empty-p xref--marker-ring) (user-error "Tag ring empty"))
  (setq ggtags-tag-ring-index
        ;; Note `ring-minus1' gets newer item.
        (funcall (if arg #'ring-plus1 #'ring-minus1)
                 (or ggtags-tag-ring-index
                     (progn (xref-push-marker-stack)
                            0))
                 (ring-length xref--marker-ring)))
  (let ((m (ring-ref xref--marker-ring ggtags-tag-ring-index))
        (i (- (ring-length xref--marker-ring) ggtags-tag-ring-index)))
    (ggtags-echo "%d%s marker%s" i (pcase (mod i 10)
                                     ;; ` required for 24.1 and 24.2
                                     (`1 "st")
                                     (`2 "nd")
                                     (`3 "rd")
                                     (_ "th"))
                 (if (marker-buffer m) "" " (dead)"))
    (if (not (marker-buffer m))
        (ding)
      (switch-to-buffer (marker-buffer m))
      (goto-char m))))

(defun ggtags-prev-mark ()
  "Move to the previous (older) mark in the tag marker ring."
  (interactive)
  (ggtags-next-mark 'previous))

(defvar ggtags-view-tag-history-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-n" 'next-error-no-select)
    (define-key m "\M-p" 'previous-error-no-select)
    (define-key m "q"    'ggtags-kill-window)
    m))

(define-derived-mode ggtags-view-tag-history-mode tabulated-list-mode "TagHist"
  :abbrev-table nil :group 'ggtags)

(defun ggtags-view-tag-history ()
  "Pop to a buffer listing visited locations from newest to oldest.
The buffer is a next error buffer and works with standard
commands `next-error' and `previous-error'.

\\{ggtags-view-tag-history-mode-map}"
  (interactive)
  (and (ring-empty-p xref--marker-ring)
       (user-error "Tag ring empty"))
  (let ((split-window-preferred-function ggtags-split-window-function)
        (inhibit-read-only t))
    (pop-to-buffer "*Tag Ring*")
    (erase-buffer)
    (ggtags-view-tag-history-mode)
    (setq next-error-function #'ggtags-view-tag-history-next-error
          next-error-last-buffer (current-buffer))
    (setq tabulated-list-entries
          ;; Use a function so that revert can work properly.
          (lambda ()
            (let ((counter (ring-length xref--marker-ring))
                  (elements (or (ring-elements xref--marker-ring)
                                (user-error "Tag ring empty")))
                  (action (lambda (_button) (next-error 0)))
                  (get-line (lambda (m)
                              (with-current-buffer (marker-buffer m)
                                (save-excursion
                                  (goto-char m)
                                  (buffer-substring (line-beginning-position)
                                                    (line-end-position)))))))
              (setq tabulated-list-format
                    `[("ID" ,(max (1+ (floor (log counter 10))) 2)
                       car-less-than-car)
                      ("Buffer" ,(max (cl-loop for m in elements
                                               for b = (marker-buffer m)
                                               maximize
                                               (length (and b (buffer-name b))))
                                      6)
                       t :right-align t)
                      ("Position" ,(max (cl-loop for m in elements
                                                 for p = (or (marker-position m) 1)
                                                 maximize (1+ (floor (log p 10))))
                                        8)
                       (lambda (x y)
                         (< (string-to-number (aref (cadr x) 2))
                            (string-to-number (aref (cadr y) 2))))
                       :right-align t)
                      ("Contents" 100 t)])
              (tabulated-list-init-header)
              (mapcar (lambda (x)
                        (prog1
                            (list counter
                                  (if (marker-buffer x)
                                      (vector (number-to-string counter)
                                              `(,(buffer-name (marker-buffer x))
                                                face link
                                                follow-link t
                                                marker ,x
                                                action ,action)
                                              (number-to-string (marker-position x))
                                              (funcall get-line x))
                                    (vector (number-to-string counter)
                                            "(dead)" "?" "?")))
                          (cl-decf counter)))
                      elements))))
    (setq tabulated-list-sort-key '("ID" . t))
    (tabulated-list-print)
    (fit-window-to-buffer nil (floor (frame-height) 2))))

(defun ggtags-view-tag-history-next-error (&optional arg reset)
  (if (not reset)
      (forward-button arg)
    (goto-char (point-min))
    (forward-button (if (button-at (point)) 0 1)))
  (when (get-buffer-window)
    (set-window-point (get-buffer-window) (point)))
  (pcase (button-get (button-at (point)) 'marker)
    ((and (pred markerp) m)
     (if (eq (get-buffer-window) (selected-window))
         (pop-to-buffer (marker-buffer m))
       (switch-to-buffer (marker-buffer m)))
     (goto-char (marker-position m)))
    (_ (error "Dead marker"))))

(defun ggtags-global-exit-message-1 ()
  "Get the total of matches and db file used."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward
         "^\\w+ \\(not found\\)\\|^\\([0-9]+\\) \\w+ located" nil t)
        (cons (or (and (match-string 1) 0)
                  (string-to-number (match-string 2)))
              (when (re-search-forward
                     "using \\(?:\\(idutils\\)\\|'[^']*/\\(\\w+\\)'\\)"
                     (line-end-position)
                     t)
                (or (and (match-string 1) "ID")
                    (match-string 2))))
      (cons 0 nil))))

(defun ggtags-global-exit-message-function (_process-status exit-status msg)
  "A function for `compilation-exit-message-function'."
  (pcase (ggtags-global-exit-message-1)
    (`(,count . ,db)
     (setq ggtags-global-exit-info (list exit-status count db))
     ;; Clear the start marker in case of zero matches.
     (and (zerop count)
          (markerp ggtags-global-start-marker)
          (not ggtags-global-continuation)
          (setq ggtags-global-start-marker nil))
     (cons (if (> exit-status 0)
               msg
             (format "found %d %s" count
                     (funcall (if (= count 1) #'car #'cadr)
                              (pcase db
                                ;; ` required for 24.1 and 24.2
                                (`"GTAGS"  '("definition" "definitions"))
                                (`"GSYMS"  '("symbol"     "symbols"))
                                (`"GRTAGS" '("reference"  "references"))
                                (`"GPATH"  '("file"       "files"))
                                (`"ID"     '("identifier" "identifiers"))
                                (_         '("match"      "matches"))))))
           exit-status))))

(defun ggtags-global-column (start)
  ;; START is the beginning position of source text.
  (let ((mbeg (text-property-any start (line-end-position) 'global-color t)))
    (and mbeg (- mbeg start))))

;;; NOTE: Must not match the 'Global started at Mon Jun 3 10:24:13'
;;; line or `compilation-auto-jump' will jump there and fail. See
;;; comments before the 'gnu' entry in
;;; `compilation-error-regexp-alist-alist'.
(defvar ggtags-global-error-regexp-alist-alist
  (append
   `((path "^\\(?:[^\"'\n]*/\\)?[^ )\t\n]+$" 0)
     ;; ACTIVE_ESCAPE   src/dialog.cc   172
     (ctags "^\\([^ \t\n]+\\)[ \t]+\\(.*?\\)[ \t]+\\([0-9]+\\)$"
            2 3 nil nil 2 (1 font-lock-function-name-face))
     ;; ACTIVE_ESCAPE     172 src/dialog.cc    #undef ACTIVE_ESCAPE
     (ctags-x "^\\([^ \t\n]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(\\(?:[^/\n]*/\\)?[^ \t\n]+\\)"
              3 2 (,(lambda () (ggtags-global-column (1+ (match-end 0)))))
              nil 3 (1 font-lock-function-name-face))
     ;; src/dialog.cc:172:#undef ACTIVE_ESCAPE
     (grep "^\\(.+?\\):\\([0-9]+\\):\\(?:$\\|[^0-9\n]\\|[0-9][^0-9\n]\\|[0-9][0-9].\\)"
           1 2 (,(lambda () (ggtags-global-column (1+ (match-end 2))))) nil 1)
     ;; src/dialog.cc ACTIVE_ESCAPE 172 #undef ACTIVE_ESCAPE
     (cscope "^\\(.+?\\)[ \t]+\\([^ \t\n]+\\)[ \t]+\\([0-9]+\\).*\\(?:[^0-9\n]\\|[^0-9\n][0-9]\\|[^:\n][0-9][0-9]\\)$"
             1 3 nil nil 1 (2 font-lock-function-name-face)))
   compilation-error-regexp-alist-alist))

(defun ggtags-abbreviate-file (start end)
  (let ((inhibit-read-only t)
        (amount (if (numberp ggtags-global-abbreviate-filename)
                    (- (- end start) ggtags-global-abbreviate-filename)
                  999))
        (advance-word (lambda ()
                        "Return the length of the text made invisible."
                        (let ((wend (min end (progn (forward-word 1) (point))))
                              (wbeg (max start (progn (backward-word 1) (point)))))
                          (goto-char wend)
                          (if (<= (- wend wbeg) 1)
                              0
                            (put-text-property (1+ wbeg) wend 'invisible t)
                            (1- (- wend wbeg)))))))
    (goto-char start)
    (while (and (> amount 0) (> end (point)))
      (cl-decf amount (funcall advance-word)))))

(defun ggtags-abbreviate-files (start end)
  (goto-char start)
  (let* ((error-re (cdr (assq (car compilation-error-regexp-alist)
                              ggtags-global-error-regexp-alist-alist)))
         (sub (cadr error-re)))
    (when (and ggtags-global-abbreviate-filename error-re)
      (while (re-search-forward (car error-re) end t)
        (when (and (or (not (numberp ggtags-global-abbreviate-filename))
                       (> (length (match-string sub))
                          ggtags-global-abbreviate-filename))
                   ;; Ignore bogus file lines such as:
                   ;;     Global found 2 matches at Thu Jan 31 13:45:19
                   (get-text-property (match-beginning sub) 'compilation-message))
          (ggtags-abbreviate-file (match-beginning sub) (match-end sub)))))))

(defvar-local ggtags-global-output-lines 0)

(defun ggtags-global--display-buffer (&optional buffer desired-point)
  (pcase (let ((buffer (or buffer (current-buffer)))
               (split-window-preferred-function ggtags-split-window-function))
           (and (not (get-buffer-window buffer))
                (display-buffer buffer '(nil (allow-no-window . t)))))
    ((and (pred windowp) w)
     (with-selected-window w
       (compilation-set-window-height w)
       (and desired-point (goto-char desired-point))))))

(defun ggtags-global-filter ()
  "Called from `compilation-filter-hook' (which see)."
  (let ((ansi-color-apply-face-function
         (lambda (beg end face)
           (when face
             (ansi-color-apply-overlay-face beg end face)
             (put-text-property beg end 'global-color t)))))
    (ansi-color-apply-on-region compilation-filter-start (point)))
  ;; Get rid of line "Using config file '/PATH/TO/.globalrc'." or
  ;; "Using default configuration."
  (when (re-search-backward
         "^ *Using \\(?:config file '.*\\|default configuration.\\)\n"
         compilation-filter-start t)
    (replace-match ""))
  (cl-incf ggtags-global-output-lines
           (count-lines compilation-filter-start (point)))
  ;; If the number of output lines is small
  ;; `ggtags-global-handle-exit' takes care of displaying the buffer.
  (when (and (> ggtags-global-output-lines 30) ggtags-navigation-mode)
    (ggtags-global--display-buffer nil (or compilation-current-error (point-min))))
  (when (and (eq ggtags-auto-jump-to-match 'history)
             (numberp ggtags-auto-jump-to-match-target)
             (not compilation-current-error)
             ;; `ggtags-global-output-lines' is imprecise but use it
             ;; as first approximation.
             (> (+ 10 ggtags-global-output-lines) ggtags-auto-jump-to-match-target)
             (> (line-number-at-pos (point-max))
                ggtags-auto-jump-to-match-target))
    (ggtags-forward-to-line ggtags-auto-jump-to-match-target)
    (setq-local ggtags-auto-jump-to-match-target nil)
    (ggtags-delay-finish-functions
      (with-display-buffer-no-window
        (condition-case nil
            (let ((compilation-auto-jump-to-first-error t))
              (compilation-auto-jump (current-buffer) (point)))
          (error (message "\
ggtags: history match invalid, jump to first match instead")
                 (first-error)))))
    ;; `compilation-filter' restores point and as a result commands
    ;; dependent on point such as `ggtags-navigation-next-file' and
    ;; `ggtags-navigation-previous-file' fail to work.
    (run-with-idle-timer
     0 nil
     (lambda (buf pt)
       (and (buffer-live-p buf)
            (with-current-buffer buf (goto-char pt))))
     (current-buffer) (point)))
  (make-local-variable 'ggtags-global-large-output)
  (when (> ggtags-global-output-lines ggtags-global-large-output)
    (cl-incf ggtags-global-large-output 500)
    (ggtags-echo "Output %d lines (Type `C-c C-k' to cancel)"
                 ggtags-global-output-lines)))

(defun ggtags-global-handle-exit (buf how)
  "A function for `compilation-finish-functions' (which see)."
  (cond
   (ggtags-global-continuation
    (let ((cont (prog1 ggtags-global-continuation
                  (setq ggtags-global-continuation nil))))
      (funcall cont buf how)))
   ((string-prefix-p "exited abnormally" how)
    ;; If exit abnormally display the buffer for inspection.
    (ggtags-global--display-buffer)
    (when (save-excursion
            (goto-char (point-max))
            (re-search-backward
             (eval-when-compile
               (format "^global: %s not found.$"
                       (regexp-opt '("GTAGS" "GRTAGS" "GSYMS" "GPATH"))))
             nil t))
      (ggtags-echo "WARNING: Global tag files missing in `%s'"
                   ggtags-project-root)
      (remhash ggtags-project-root ggtags-projects)))
   (ggtags-auto-jump-to-match
    (if (pcase (compilation-next-single-property-change
                (point-min) 'compilation-message)
          ((and pt (guard pt))
           (compilation-next-single-property-change
            (save-excursion (goto-char pt) (end-of-line) (point))
            'compilation-message)))
        ;; There are multiple matches so pop up the buffer.
        (and ggtags-navigation-mode (ggtags-global--display-buffer))
      ;; Manually run the `compilation-auto-jump' timer. Hackish but
      ;; everything else seems unreliable. See:
      ;;
      ;; - http://debbugs.gnu.org/13829
      ;; - http://debbugs.gnu.org/23987
      ;; - https://github.com/leoliu/ggtags/issues/89
      ;;
      (pcase (cl-find 'compilation-auto-jump timer-list :key #'timer--function)
        (`nil )
        (timer (timer-event-handler timer)))
      (ggtags-navigation-mode -1)
      (ggtags-navigation-mode-cleanup buf t)))))

(defvar ggtags-global-mode-font-lock-keywords
  '(("^Global \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
     (1 'compilation-error)
     (2 'compilation-error nil t))
    ("^Global found \\([0-9]+\\)" (1 compilation-info-face))))

(defvar compilation-always-kill)        ;new in 24.3

(define-compilation-mode ggtags-global-mode "Global"
  "A mode for showing outputs from gnu global."
  ;; Note: Place `ggtags-global-output-format' as first element for
  ;; `ggtags-abbreviate-files'.
  (setq-local compilation-error-regexp-alist (list ggtags-global-output-format))
  (when (markerp ggtags-global-start-marker)
    (setq ggtags-project-root
          (buffer-local-value 'ggtags-project-root
                              (marker-buffer ggtags-global-start-marker))))
  (pcase ggtags-auto-jump-to-match
    (`history (make-local-variable 'ggtags-auto-jump-to-match-target)
              (setq-local compilation-auto-jump-to-first-error
                          (not ggtags-auto-jump-to-match-target)))
    (`nil (setq-local compilation-auto-jump-to-first-error nil))
    (_ (setq-local compilation-auto-jump-to-first-error t)))
  (setq-local compilation-scroll-output nil)
  ;; See `compilation-move-to-column' for details.
  (setq-local compilation-first-column 0)
  (setq-local compilation-error-screen-columns nil)
  (setq-local compilation-disable-input t)
  (setq-local compilation-always-kill t)
  (setq-local compilation-error-face 'compilation-info)
  (setq-local compilation-exit-message-function
              'ggtags-global-exit-message-function)
  ;; See: https://github.com/leoliu/ggtags/issues/26
  (setq-local find-file-suppress-same-file-warnings t)
  (setq-local truncate-lines t)
  (jit-lock-register #'ggtags-abbreviate-files)
  (add-hook 'compilation-filter-hook 'ggtags-global-filter nil 'local)
  (add-hook 'compilation-finish-functions 'ggtags-global-handle-exit nil t)
  (setq-local bookmark-make-record-function #'ggtags-make-bookmark-record)
  (setq-local ggtags-enable-navigation-keys nil)
  (add-hook 'kill-buffer-hook (lambda () (ggtags-navigation-mode -1)) nil t))

;; NOTE: Need this to avoid putting menu items in
;; `emulation-mode-map-alists', which creates double entries. See
;; http://i.imgur.com/VJJTzVc.png
(defvar ggtags-navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-n" 'next-error)
    (define-key map "\M-p" 'previous-error)
    (define-key map "\M-}" 'ggtags-navigation-next-file)
    (define-key map "\M-{" 'ggtags-navigation-previous-file)
    (define-key map "\M-=" 'ggtags-navigation-start-file)
    (define-key map "\M->" 'ggtags-navigation-last-error)
    (define-key map "\M-<" 'first-error)
    ;; Note: shadows `isearch-forward-regexp' but it can still be
    ;; invoked with `C-u C-s'.
    (define-key map "\C-\M-s" 'ggtags-navigation-isearch-forward)
    ;; Add an alternative binding because C-M-s is reported not
    ;; working on some systems.
    (define-key map "\M-ss" 'ggtags-navigation-isearch-forward)
    (define-key map "\C-c\C-k"
      (lambda () (interactive)
        (ggtags-ensure-global-buffer (kill-compilation))))
    (define-key map "\M-o" 'ggtags-navigation-visible-mode)
    (define-key map [return] 'ggtags-navigation-mode-done)
    (define-key map "\r" 'ggtags-navigation-mode-done)
    (define-key map [remap pop-tag-mark] 'ggtags-navigation-mode-abort) ;Emacs 24
    (define-key map [remap xref-pop-marker-stack] 'ggtags-navigation-mode-abort)
    map))

(defvar ggtags-mode-map-alist
  `((ggtags-enable-navigation-keys . ,ggtags-navigation-map)))

(defvar ggtags-navigation-mode-map
  (let ((map (make-sparse-keymap))
        (menu (make-sparse-keymap "GG-Navigation")))
    ;; Menu items: (info "(elisp)Extended Menu Items")
    (define-key map [menu-bar ggtags-navigation] (cons "GG-Navigation" menu))
    ;; Ordered backwards
    (define-key menu [visible-mode]
      '(menu-item "Visible mode" ggtags-navigation-visible-mode
                  :button (:toggle . (ignore-errors
                                       (ggtags-ensure-global-buffer
                                         visible-mode)))))
    (define-key menu [done]
      '(menu-item "Finish navigation" ggtags-navigation-mode-done))
    (define-key menu [abort]
      '(menu-item "Abort" ggtags-navigation-mode-abort))
    (define-key menu [last-match]
      '(menu-item "Last match" ggtags-navigation-last-error))
    (define-key menu [first-match] '(menu-item "First match" first-error))
    (define-key menu [previous-file]
      '(menu-item "Previous file" ggtags-navigation-previous-file))
    (define-key menu [next-file]
      '(menu-item "Next file" ggtags-navigation-next-file))
    (define-key menu [isearch-forward]
      '(menu-item "Find match with isearch" ggtags-navigation-isearch-forward))
    (define-key menu [previous]
      '(menu-item "Previous match" previous-error))
    (define-key menu [next]
      '(menu-item "Next match" next-error))
    map))

(defun ggtags-move-to-tag (&optional name)
  "Move to NAME tag in current line."
  (let ((tag (or name ggtags-current-tag-name)))
    ;; Do nothing if on the tag already i.e. by `ggtags-global-column'.
    (unless (or (not tag) (looking-at (concat (regexp-quote tag) "\\_>")))
      (let ((orig (point))
            (regexps (mapcar (lambda (fmtstr)
                               (format fmtstr (regexp-quote tag)))
                             '("\\_<%s\\_>" "%s\\_>" "%s"))))
        (beginning-of-line)
        (if (cl-loop for re in regexps
                     ;; Note: tag might not agree with current
                     ;; major-mode's symbol, so try harder. For
                     ;; example, in `php-mode' $cacheBackend is a
                     ;; symbol, but cacheBackend is a tag.
                     thereis (re-search-forward re (line-end-position) t))
            (goto-char (match-beginning 0))
          (goto-char orig))))))

(defun ggtags-navigation-mode-cleanup (&optional buf kill)
  (let ((buf (or buf ggtags-global-last-buffer)))
    (and (buffer-live-p buf)
         (with-current-buffer buf
           (when (get-buffer-process (current-buffer))
             (kill-compilation))
           (when (and (derived-mode-p 'ggtags-global-mode)
                      (get-buffer-window))
             (quit-windows-on (current-buffer)))
           (and kill (kill-buffer buf))))))

(defun ggtags-navigation-mode-done ()
  (interactive)
  (ggtags-navigation-mode -1)
  (setq tags-loop-scan t
        tags-loop-operate '(ggtags-find-tag-continue))
  (ggtags-navigation-mode-cleanup))

(defun ggtags-navigation-mode-abort ()
  "Abort navigation and return to where the search was started."
  (interactive)
  (ggtags-navigation-mode -1)
  (ggtags-navigation-mode-cleanup nil t)
  ;; Run after (ggtags-navigation-mode -1) or
  ;; ggtags-global-start-marker might not have been saved.
  (when (and ggtags-global-start-marker
             (not (markerp ggtags-global-start-marker)))
    (setq ggtags-global-start-marker nil)
    (xref-pop-marker-stack)))

(defun ggtags-navigation-next-file (n)
  (interactive "p")
  (ggtags-ensure-global-buffer
    (compilation-next-file n)
    (compile-goto-error)))

(defun ggtags-navigation-previous-file (n)
  (interactive "p")
  (ggtags-navigation-next-file (- n)))

(defun ggtags-navigation-start-file ()
  "Move to the file where navigation session starts."
  (interactive)
  (let ((start-file (or ggtags-global-start-file
                        (user-error "Cannot decide start file"))))
    (ggtags-ensure-global-buffer
      (pcase (cl-block nil
               (ggtags-foreach-file
                (lambda (file)
                  (when (file-equal-p file start-file)
                    (cl-return (point))))))
        (`nil (user-error "No matches for `%s'" start-file))
        (n (goto-char n) (compile-goto-error))))))

(defun ggtags-navigation-last-error ()
  (interactive)
  (ggtags-ensure-global-buffer
    (goto-char (point-max))
    (compilation-previous-error 1)
    (compile-goto-error)))

(defun ggtags-navigation-isearch-forward (&optional regexp-p)
  (interactive "P")
  (ggtags-ensure-global-buffer
    (let ((saved (if visible-mode 1 -1)))
      (visible-mode 1)
      (with-selected-window (get-buffer-window (current-buffer))
        (isearch-forward regexp-p)
        (beginning-of-line)
        (visible-mode saved)
        (compile-goto-error)))))

(defun ggtags-navigation-visible-mode (&optional arg)
  (interactive (list (or current-prefix-arg 'toggle)))
  (ggtags-ensure-global-buffer
    (visible-mode arg)))

(defvar ggtags-global-line-overlay nil)

(defun ggtags-global-next-error-function ()
  (when (eq next-error-last-buffer ggtags-global-last-buffer)
    (ggtags-move-to-tag)
    (ggtags-global-save-start-marker)
    (and (ggtags-project-update-mtime-maybe)
         (message "File `%s' is newer than GTAGS"
                  (file-name-nondirectory buffer-file-name)))
    (and ggtags-mode-sticky (ggtags-mode 1))
    (ignore-errors
      (ggtags-ensure-global-buffer
        (unless (overlayp ggtags-global-line-overlay)
          (setq ggtags-global-line-overlay (make-overlay (point) (point)))
          (overlay-put ggtags-global-line-overlay 'face 'ggtags-global-line))
        (move-overlay ggtags-global-line-overlay
                      (line-beginning-position) (line-end-position)
                      (current-buffer))
        ;; Update search history
        (let ((id (ggtags-global-search-id (car compilation-arguments)
                                           default-directory)))
          (setq ggtags-global-search-history
                (cl-remove id ggtags-global-search-history :test #'equal :key #'car))
          (add-to-history 'ggtags-global-search-history
                          (cons id (ggtags-global-current-search))
                          ggtags-global-history-length))))
    (run-hooks 'ggtags-find-tag-hook)))

(put 'ggtags-navigation-mode-lighter 'risky-local-variable t)

(defvar ggtags-navigation-mode-lighter
  '(" GG["
    (:eval
     (if (not (buffer-live-p ggtags-global-last-buffer))
         '(:propertize "??" face error help-echo "No Global buffer")
       (with-current-buffer ggtags-global-last-buffer
         (pcase (or ggtags-global-exit-info '(0 0 ""))
           (`(,exit ,count ,db)
            `((:propertize ,(pcase db
                              (`"GTAGS"  "D")
                              (`"GRTAGS" "R")
                              (`"GSYMS"  "S")
                              (`"GPATH"  "F")
                              (`"ID"     "I"))
                           face success)
              (:propertize
               ,(pcase (get-text-property (line-beginning-position)
                                          'compilation-message)
                  (`nil "?")
                  ;; Assume the first match appears at line 5
                  (_ (number-to-string (- (line-number-at-pos) 4))))
               face success)
              "/"
              (:propertize ,(number-to-string count) face success)
              ,(unless (zerop exit)
                 `(":" (:propertize ,(number-to-string exit) face error)))))))))
    "]")
  "Ligher for `ggtags-navigation-mode'; set to nil to disable it.")

(define-minor-mode ggtags-navigation-mode nil
  ;; If `ggtags-enable-navigation-keys' is set to nil only display the
  ;; lighter in `ggtags-mode' buffers.
  ;; See https://github.com/leoliu/ggtags/issues/124
  :lighter (:eval (and (or ggtags-enable-navigation-keys
                           ggtags-mode)
                       ggtags-navigation-mode-lighter))
  :global t
  (if ggtags-navigation-mode
      (progn
        ;; Higher priority for `ggtags-navigation-mode' to avoid being
        ;; hijacked by modes such as `view-mode'.
        (add-to-list 'emulation-mode-map-alists 'ggtags-mode-map-alist)
        (add-hook 'next-error-hook 'ggtags-global-next-error-function)
        (add-hook 'minibuffer-setup-hook 'ggtags-minibuffer-setup-function))
    (setq emulation-mode-map-alists
          (delq 'ggtags-mode-map-alist emulation-mode-map-alists))
    (remove-hook 'next-error-hook 'ggtags-global-next-error-function)
    (remove-hook 'minibuffer-setup-hook 'ggtags-minibuffer-setup-function)))

(defun ggtags-minibuffer-setup-function ()
  ;; Disable ggtags-navigation-mode in minibuffer.
  (setq-local ggtags-enable-navigation-keys nil))

(defun ggtags-kill-file-buffers (&optional interactive)
  "Kill all buffers visiting files in current project."
  (interactive "p")
  (ggtags-check-project)
  (let ((directories (cons (ggtags-current-project-root) (ggtags-get-libpath)))
        (count 0))
    (dolist (buf (buffer-list))
      (let ((file (and (buffer-live-p buf)
                       (not (eq buf (current-buffer)))
                       (buffer-file-name buf))))
        (when (and file (cl-some (lambda (dir)
                                   ;; Don't use `file-in-directory-p'
                                   ;; to allow symbolic links.
                                   (string-prefix-p dir file))
                                 directories))
          (and (kill-buffer buf) (cl-incf count)))))
    (and interactive
         (message "%d %s killed" count (if (= count 1) "buffer" "buffers")))))

(defun ggtags-after-save-function ()
  (when (ggtags-find-project)
    (ggtags-project-update-mtime-maybe)
    (and buffer-file-name ggtags-update-on-save
         (ggtags-update-tags-single buffer-file-name 'nowait))))

(defun ggtags-global-output (buffer cmds callback &optional cutoff)
  "Asynchronously pipe the output of running CMDS to BUFFER.
When finished invoke CALLBACK in BUFFER with process exit status."
  (or buffer (error "Output buffer required"))
  (when (get-buffer-process (get-buffer buffer))
    ;; Notice running multiple processes in the same buffer so that we
    ;; can fix the caller. See for example `ggtags-eldoc-function'.
    (message "Warning: detected %S already running in %S; interrupting..."
             (get-buffer-process buffer) buffer)
    (interrupt-process (get-buffer-process buffer)))
  (let* ((program (car cmds))
         (args (cdr cmds))
         (cutoff (and cutoff (+ cutoff (if (get-buffer buffer)
                                           (with-current-buffer buffer
                                             (line-number-at-pos (point-max)))
                                         0))))
         (proc (apply #'start-file-process program buffer program args))
         (filter (lambda (proc string)
                   (and (buffer-live-p (process-buffer proc))
                        (with-current-buffer (process-buffer proc)
                          (goto-char (process-mark proc))
                          (insert string)
                          (when (and (> (line-number-at-pos (point-max)) cutoff)
                                     (process-live-p proc))
                            (interrupt-process (current-buffer)))))))
         (sentinel (lambda (proc _msg)
                     (when (memq (process-status proc) '(exit signal))
                       (with-current-buffer (process-buffer proc)
                         (set-process-buffer proc nil)
                         (funcall callback (process-exit-status proc)))))))
    (set-process-query-on-exit-flag proc nil)
    (and cutoff (set-process-filter proc filter))
    (set-process-sentinel proc sentinel)
    proc))

(cl-defun ggtags-fontify-code (code &optional (mode major-mode))
  (cl-check-type mode function)
  (cl-typecase code
    ((not string) code)
    (string (cl-labels ((prepare-buffer ()
                          (with-current-buffer
                              (get-buffer-create " *Code-Fontify*")
                            (let ((inhibit-read-only t))
                              (erase-buffer))
                            (funcall mode)
                            (setq font-lock-mode t)
                            (funcall font-lock-function font-lock-mode)
                            (setq jit-lock-mode nil)
                            (current-buffer))))
              (with-current-buffer (prepare-buffer)
                (let ((inhibit-read-only t))
                  (insert code)
                  (font-lock-default-fontify-region (point-min) (point-max) nil))
                (buffer-string))))))

(defun ggtags-get-definition-default (defs)
  (and (caar defs)
       (concat (ggtags-fontify-code (caar defs))
               (and (cdr defs) " [guess]"))))

(defun ggtags-show-definition (name)
  (interactive (list (ggtags-read-tag 'definition current-prefix-arg)))
  (ggtags-check-project)
  (let* ((re (cadr (assq 'grep ggtags-global-error-regexp-alist-alist)))
         (current (current-buffer))
         (buffer (get-buffer-create " *ggtags-definition*"))
         (args (list "--result=grep" "--path-style=absolute" name))
         ;; Need these bindings so that let-binding
         ;; `ggtags-print-definition-function' can work see
         ;; `ggtags-eldoc-function'.
         (get-fn ggtags-get-definition-function)
         (print-fn ggtags-print-definition-function)
         (show (lambda (_status)
                 (goto-char (point-min))
                 (let ((defs (cl-loop while (re-search-forward re nil t)
                                      collect (list (buffer-substring (1+ (match-end 2))
                                                                      (line-end-position))
                                                    name
                                                    (match-string 1)
                                                    (string-to-number (match-string 2))))))
                   (kill-buffer buffer)
                   (with-current-buffer current
                     (funcall print-fn (funcall get-fn defs)))))))
    (ggtags-with-current-project
      (ggtags-global-output
       buffer
       (cons (ggtags-program-path "global")
             (if (ggtags-sort-by-nearness-p) (cons "--nearness" args) args))
       show 100))))

(defvar ggtags-mode-prefix-map
  (let ((m (make-sparse-keymap)))
    ;; Globally bound to `M-g p'.
    ;; (define-key m "\M-'" 'previous-error)
    (define-key m (kbd "M-DEL") 'ggtags-delete-tags)
    (define-key m "\M-p" 'ggtags-prev-mark)
    (define-key m "\M-n" 'ggtags-next-mark)
    (define-key m "\M-f" 'ggtags-find-file)
    (define-key m "\M-o" 'ggtags-find-other-symbol)
    (define-key m "\M-g" 'ggtags-grep)
    (define-key m "\M-i" 'ggtags-idutils-query)
    (define-key m "\M-b" 'ggtags-browse-file-as-hypertext)
    (define-key m "\M-k" 'ggtags-kill-file-buffers)
    (define-key m "\M-h" 'ggtags-view-tag-history)
    (define-key m "\M-j" 'ggtags-visit-project-root)
    (define-key m "\M-/" 'ggtags-view-search-history)
    (define-key m (kbd "M-SPC") 'ggtags-save-to-register)
    (define-key m (kbd "M-%") 'ggtags-query-replace)
    (define-key m "\M-?" 'ggtags-show-definition)
    m))

(defvar ggtags-mode-map
  (let ((map (make-sparse-keymap))
        (menu (make-sparse-keymap "Ggtags")))
    (define-key map "\M-." 'ggtags-find-tag-dwim)
    (define-key map (kbd "M-]") 'ggtags-find-reference)
    (define-key map (kbd "C-M-.") 'ggtags-find-tag-regexp)
    (define-key map ggtags-mode-prefix-key ggtags-mode-prefix-map)
    ;; Menu items
    (define-key map [menu-bar ggtags] (cons "Ggtags" menu))
    ;; Ordered backwards
    (define-key menu [report-bugs]
      `(menu-item "Report bugs"
                  (lambda () (interactive)
                    (browse-url ggtags-bug-url)
                    (message "Please visit %s" ggtags-bug-url))
                  :help ,(format "Visit %s" ggtags-bug-url)))
    (define-key menu [custom-ggtags]
      '(menu-item "Customize Ggtags"
                  (lambda () (interactive) (customize-group 'ggtags))))
    (define-key menu [eldoc-mode]
      '(menu-item "Toggle eldoc mode" eldoc-mode :button (:toggle . eldoc-mode)))
    (define-key menu [save-project]
      '(menu-item "Save project settings" ggtags-save-project-settings))
    (define-key menu [toggle-read-only]
      '(menu-item "Toggle project read-only" ggtags-toggle-project-read-only
                  :button (:toggle . buffer-read-only)))
    (define-key menu [visit-project-root]
      '(menu-item "Visit project root" ggtags-visit-project-root))
    (define-key menu [sep2] menu-bar-separator)
    (define-key menu [browse-hypertext]
      '(menu-item "Browse as hypertext" ggtags-browse-file-as-hypertext
                  :enable (ggtags-find-project)))
    (define-key menu [delete-tags]
      '(menu-item "Delete tags" ggtags-delete-tags
                  :enable (ggtags-find-project)
                  :help "Delete file GTAGS, GRTAGS, GPATH, ID etc."))
    (define-key menu [kill-buffers]
      '(menu-item "Kill project file buffers" ggtags-kill-file-buffers
                  :enable (ggtags-find-project)))
    (define-key menu [view-tag]
      '(menu-item "View tag history" ggtags-view-tag-history))
    (define-key menu [pop-mark]
      '(menu-item "Pop mark" xref-pop-marker-stack
                  :help "Pop to previous mark and destroy it"))
    (define-key menu [next-mark]
      '(menu-item "Next mark" ggtags-next-mark))
    (define-key menu [prev-mark]
      '(menu-item "Previous mark" ggtags-prev-mark))
    (define-key menu [sep1] menu-bar-separator)
    (define-key menu [previous-error]
      '(menu-item "Previous match" previous-error))
    (define-key menu [next-error]
      '(menu-item "Next match" next-error))
    (define-key menu [rerun-search]
      '(menu-item "View past searches" ggtags-view-search-history))
    (define-key menu [save-to-register]
      '(menu-item "Save search to register" ggtags-save-to-register))
    (define-key menu [find-file]
      '(menu-item "Find files" ggtags-find-file))
    (define-key menu [query-replace]
      '(menu-item "Query replace" ggtags-query-replace))
    (define-key menu [idutils]
      '(menu-item "Query idutils DB" ggtags-idutils-query))
    (define-key menu [grep]
      '(menu-item "Grep" ggtags-grep))
    (define-key menu [find-symbol]
      '(menu-item "Find other symbol" ggtags-find-other-symbol
                  :help "Find references without definition"))
    (define-key menu [find-tag-regexp]
      '(menu-item "Find tag matching regexp" ggtags-find-tag-regexp))
    (define-key menu [show-definition]
      '(menu-item "Show definition" ggtags-show-definition))
    (define-key menu [find-reference]
      '(menu-item "Find reference" ggtags-find-reference))
    ;; TODO: bind `find-tag-continue' to `M-*' after dropping support
    ;; for emacs < 25.
    (define-key menu [find-tag-continue]
      '(menu-item "Continue find tag" tags-loop-continue))
    (define-key menu [find-tag]
      '(menu-item "Find tag" ggtags-find-tag-dwim))
    (define-key menu [update-tags]
      '(menu-item "Update tag files" ggtags-update-tags
                  :visible (ggtags-find-project)))
    (define-key menu [run-gtags]
      '(menu-item "Run gtags" ggtags-create-tags
                  :visible (not (ggtags-find-project))))
    map))

(defvar ggtags-mode-line-project-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'ggtags-visit-project-root)
    map))

(put 'ggtags-mode-line-project-name 'risky-local-variable t)
(defvar ggtags-mode-line-project-name
  '("[" (:eval (let ((name (if (stringp ggtags-project-root)
                               (file-name-nondirectory
                                (directory-file-name ggtags-project-root))
                             "?")))
                 (propertize
                  name 'face compilation-info-face
                  'help-echo (if (stringp ggtags-project-root)
                                 (concat "mouse-1 to visit " ggtags-project-root)
                               "mouse-1 to set project")
                  'mouse-face 'mode-line-highlight
                  'keymap ggtags-mode-line-project-keymap)))
    "]")
  "Mode line construct for displaying current project name.
The value is the name of the project root directory. Setting it
to nil disables displaying this information.")

;;;###autoload
(define-minor-mode ggtags-mode nil
  :lighter (:eval (if ggtags-navigation-mode "" " GG"))
  (ggtags-setup-highlight-tag-at-point ggtags-highlight-tag)
  (if ggtags-mode
      (progn
        (add-hook 'after-save-hook 'ggtags-after-save-function nil t)
        ;; Append to serve as a fallback method.
        (add-hook 'completion-at-point-functions
                  #'ggtags-completion-at-point t t)
        ;; Work around http://debbugs.gnu.org/19324
        (or eldoc-documentation-function
            (setq-local eldoc-documentation-function #'ignore))
        (add-function :after-until (local 'eldoc-documentation-function)
                      #'ggtags-eldoc-function '((name . ggtags-eldoc-function)
                                                (depth . -100)))
        (unless (memq 'ggtags-mode-line-project-name
                      mode-line-buffer-identification)
          (setq mode-line-buffer-identification
                (append mode-line-buffer-identification
                        '(ggtags-mode-line-project-name)))))
    (remove-hook 'after-save-hook 'ggtags-after-save-function t)
    (remove-hook 'completion-at-point-functions #'ggtags-completion-at-point t)
    (remove-function (local 'eldoc-documentation-function) 'ggtags-eldoc-function)
    (setq mode-line-buffer-identification
          (delq 'ggtags-mode-line-project-name mode-line-buffer-identification))
    (ggtags-cancel-highlight-tag-at-point 'keep-timer)))

(defvar ggtags-highlight-tag-map
  (let ((map (make-sparse-keymap)))
    ;; Bind down- events so that the global keymap won't ``shine
    ;; through''. See `mode-line-buffer-identification-keymap' for
    ;; similar workaround.
    (define-key map [S-mouse-1] 'ggtags-find-tag-dwim)
    (define-key map [S-down-mouse-1] 'ignore)
    (define-key map [S-mouse-3] 'ggtags-find-reference)
    (define-key map [S-down-mouse-3] 'ignore)
    map)
  "Keymap used for valid tag at point.")

(put 'ggtags-active-tag 'face 'ggtags-highlight)
(put 'ggtags-active-tag 'keymap ggtags-highlight-tag-map)
;; (put 'ggtags-active-tag 'mouse-face 'match)
(put 'ggtags-active-tag 'help-echo
     "S-mouse-1 for definitions\nS-mouse-3 for references")

(defun ggtags-setup-highlight-tag-at-point (flag)
  (cond ((null flag) (ggtags-cancel-highlight-tag-at-point))
        ((not (timerp ggtags-highlight-tag-timer))
         (setq ggtags-highlight-tag-timer
               (run-with-idle-timer flag t #'ggtags-highlight-tag-at-point)))
        (t (timer-set-idle-time ggtags-highlight-tag-timer flag t))))

(defun ggtags-cancel-highlight-tag-at-point (&optional keep-timer)
  (when (and (not keep-timer)
             (timerp ggtags-highlight-tag-timer))
    (cancel-timer ggtags-highlight-tag-timer)
    (setq ggtags-highlight-tag-timer nil))
  (when ggtags-highlight-tag-overlay
    (delete-overlay ggtags-highlight-tag-overlay)
    (setq ggtags-highlight-tag-overlay nil)))

(defun ggtags-highlight-tag-at-point ()
  (when (and ggtags-mode ggtags-project-root (ggtags-find-project))
    (unless (overlayp ggtags-highlight-tag-overlay)
      (setq ggtags-highlight-tag-overlay (make-overlay (point) (point) nil t))
      (overlay-put ggtags-highlight-tag-overlay 'modification-hooks
                   (list (lambda (o after &rest _args)
                           (and (not after) (delete-overlay o))))))
    (let ((bounds (funcall ggtags-bounds-of-tag-function))
          (o ggtags-highlight-tag-overlay))
      (cond
       ((and bounds
             (eq (overlay-buffer o) (current-buffer))
             (= (overlay-start o) (car bounds))
             (= (overlay-end o) (cdr bounds)))
        ;; Overlay matches current tag so do nothing.
        nil)
       ((and bounds (let ((completion-ignore-case nil))
                      (test-completion
                       (buffer-substring (car bounds) (cdr bounds))
                       ggtags-completion-table)))
        (move-overlay o (car bounds) (cdr bounds) (current-buffer))
        (overlay-put o 'category 'ggtags-active-tag))
       (t (move-overlay o
                        (or (car bounds) (point))
                        (or (cdr bounds) (point))
                        (current-buffer))
          (overlay-put o 'category nil))))))

;;; eldoc

(defvar-local ggtags-eldoc-cache nil)

(declare-function eldoc-message "eldoc")
(defun ggtags-eldoc-function ()
  "A function suitable for `eldoc-documentation-function' (which see)."
  (pcase (ggtags-tag-at-point)
    (`nil nil)
    (tag (if (equal tag (car ggtags-eldoc-cache))
             (cadr ggtags-eldoc-cache)
           (and ggtags-project-root (ggtags-find-project)
                (let* ((ggtags-print-definition-function
                        (lambda (s)
                          (setq ggtags-eldoc-cache (list tag s))
                          (eldoc-message s))))
                  ;; Prevent multiple runs of ggtags-show-definition
                  ;; for the same tag.
                  (setq ggtags-eldoc-cache (list tag))
                  (condition-case err
                      (ggtags-show-definition tag)
                    (file-error
                     (remove-function (local 'eldoc-documentation-function)
                                      'ggtags-eldoc-function)
                     (message "\
Function `ggtags-eldoc-function' disabled for eldoc in current buffer: %S" err)))
                  nil))))))

;;; imenu

(defun ggtags-goto-imenu-index (name line &rest _args)
  (ggtags-forward-to-line line)
  (ggtags-move-to-tag name))

;;;###autoload
(defun ggtags-build-imenu-index ()
  "A function suitable for `imenu-create-index-function'."
  (let ((file (and buffer-file-name (file-relative-name buffer-file-name))))
    (and file (with-temp-buffer
                (when (with-demoted-errors "ggtags-build-imenu-index: %S"
                        (zerop (ggtags-with-current-project
                                 (process-file (ggtags-program-path "global")
                                               nil t nil "-x" "-f" file))))
                  (goto-char (point-min))
                  (cl-loop while (re-search-forward
                                  "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)" nil t)
                           collect (list (match-string 1)
                                         (string-to-number (match-string 2))
                                         'ggtags-goto-imenu-index)))))))

;;; hippie-expand

;;;###autoload
(defun ggtags-try-complete-tag (old)
  "A function suitable for `hippie-expand-try-functions-list'."
  (eval-and-compile (require 'hippie-exp))
  (unless old
    (he-init-string (or (car (funcall ggtags-bounds-of-tag-function)) (point))
                    (point))
    (setq he-expand-list
          (and (not (equal he-search-string ""))
               (ggtags-find-project)
               (sort (all-completions he-search-string
                                      ggtags-completion-table)
                     #'string-lessp))))
  (if (null he-expand-list)
      (progn
        (if old (he-reset-string))
        nil)
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))

(defun ggtags-reload (&optional force)
  (interactive "P")
  (unload-feature 'ggtags force)
  (require 'ggtags))

(provide 'ggtags)
;;; ggtags.el ends here
